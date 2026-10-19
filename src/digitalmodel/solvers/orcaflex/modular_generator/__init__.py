# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""OrcaFlex Modular Model Generator from Project Spec."""

import logging
import shutil
from contextlib import contextmanager
from dataclasses import dataclass, field
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Any, Dict

import yaml

from ..yaml_utils import _represent_none

from .input_manifest import write_input_manifest
from .schema import ProjectInputSpec
from .builders.base import BaseBuilder
from .builders.context import BuilderContext
from .builders.registry import BuilderRegistry

# Import builders to trigger @BuilderRegistry.register decorators
from .builders import (  # noqa: F401
    GeneralBuilder,
    VarDataBuilder,
    EnvironmentBuilder,
    VesselTypeBuilder,
    LineTypeBuilder,
    VesselBuilder,
    SupportsBuilder,
    MorisonBuilder,
    ShapesBuilder,
    BuoysBuilder,
    LinesBuilder,
    WinchBuilder,
    GroupsBuilder,
)

from .post_validator import (
    PostGenerationValidator,
    ValidationWarning,
    check_collection_collisions,
)
from .routers.mooring_router import MooringRouter
from .routers.vessel_router import VesselRouter
from .schema.generic import GenericModel

logger = logging.getLogger(__name__)


class ModelValidationError(ValueError):
    """Generated inputs failed validation; all findings remain inspectable."""

    def __init__(self, findings: list[ValidationWarning]):
        self.findings = findings
        errors = [w for w in findings if w.level == "error"]
        details = "; ".join(f"{w.category}: {w.message}" for w in errors)
        super().__init__(f"Post-generation validation failed ({len(errors)} errors): {details}")


@contextmanager
def _staged_output(output_dir: Path):
    """Publish model files only after generation and validation succeed.

    A validation or builder failure leaves even an existing output untouched.
    Publication copies files after validation; it is not an atomic transaction
    against I/O failures during the copy itself.
    """
    with TemporaryDirectory(prefix="modular-model-") as temporary:
        staging = Path(temporary)
        yield staging
        shutil.copytree(staging, output_dir, dirs_exist_ok=True)


class _NoAliasDumper(yaml.Dumper):
    """YAML Dumper that never emits anchors/aliases.

    OrcFxAPI's YAML parser does not support YAML anchors (``&id001``)
    or aliases (``*id001``).  When multiple buoys share identical vertex
    data, default PyYAML output uses aliases which OrcFxAPI rejects.
    """

    def ignore_aliases(self, data):
        return True

    def represent_mapping(self, tag, mapping, flow_style=None):
        """Keep free-end stiffness rows in the native save's compact form."""
        node = super().represent_mapping(tag, mapping, flow_style)
        stiffness_key = (
            "ConnectionxBendingStiffness, ConnectionyBendingStiffness, "
            "ConnectionTwistingStiffness, ConnectionDamping"
        )
        for key, value in node.value:
            if key.value in (stiffness_key,
                             "ConnectionxBendingStiffness, ConnectionyBendingStiffness") and any(row == [] for row in mapping[key.value]):
                for row in value.value:
                    row.flow_style = True
        return node


# Pin defaults locally; prior imports shall not select PyYAML's null spelling.
_NoAliasDumper.add_representer(type(None), _represent_none)


# Backward-compatible constant: ordered list of include file names
INCLUDE_ORDER = BuilderRegistry.get_include_order()


@dataclass
class GenerationResult:
    """Result of a generate_with_overrides() call."""

    master_file: Path
    include_files: list[Path]
    variables_resolved: dict[str, Any] = field(default_factory=dict)
    warnings: list[str] = field(default_factory=list)
    manifest_file: Path | None = None


class ModularModelGenerator:
    """Generates complete OrcaFlex modular model structure from spec file."""

    def __init__(self, spec_file: Path):
        """Load and validate the specification used to generate model inputs."""
        self.spec_file = Path(spec_file)
        self.spec = self._load_and_validate_spec()

    @classmethod
    def from_spec(cls, spec: ProjectInputSpec) -> "ModularModelGenerator":
        """Create generator from an in-memory ProjectInputSpec.

        Args:
            spec: Validated ProjectInputSpec object.

        Returns:
            ModularModelGenerator ready to generate output.
        """
        instance = cls.__new__(cls)
        instance.spec_file = None
        instance.spec = spec
        return instance

    def _run_routers(self) -> None:
        """Run domain routers to convert engineering specs to generic model.

        Routers transform domain-specific specs (mooring, vessel, etc.)
        into GenericModel-compatible dicts, then merge them into
        spec.generic for processing by GenericModelBuilder.
        """
        router_outputs: list[dict[str, Any]] = []

        # Route mooring system
        if hasattr(self.spec, 'mooring') and self.spec.mooring is not None:
            mooring_result = MooringRouter().route(self.spec.mooring)
            router_outputs.append(mooring_result)

        # Route vessel from equipment (if standalone vessel model)
        # VesselRouter is available for from_hull_catalog() calls
        # but automatic routing requires explicit vessel spec

        if not router_outputs:
            return

        # Merge router outputs into spec.generic
        merged: dict[str, Any] = {}
        for output in router_outputs:
            for key, value in output.items():
                if key in merged and isinstance(merged[key], list):
                    merged[key].extend(value)
                else:
                    merged[key] = value

        if self.spec.generic is None:
            # Create new generic model from router output
            self.spec = self.spec.model_copy(
                update={"generic": GenericModel(**merged)}
            )
        else:
            # Merge into existing generic model
            existing = self.spec.generic.model_dump()
            for key, value in merged.items():
                if key in existing and isinstance(existing[key], list):
                    existing[key].extend(value)
                elif key not in existing or not existing[key]:
                    existing[key] = value
            self.spec = self.spec.model_copy(
                update={"generic": GenericModel(**existing)}
            )

    def _load_and_validate_spec(self) -> ProjectInputSpec:
        with open(self.spec_file) as f:
            data = yaml.safe_load(f)
        return ProjectInputSpec(**data)

    def generate(self, output_dir: Path, *, allow_validation_errors: bool = False) -> None:
        """Validate then publish inputs; errors raise ModelValidationError.

        Set allow_validation_errors=True only to write diagnostic inputs. Error
        findings remain logged at ERROR level. Collection collisions still raise.
        """
        with _staged_output(Path(output_dir)) as staging:
            self._generate(staging, allow_validation_errors=allow_validation_errors)

    def _generate(self, output_dir: Path, *, allow_validation_errors: bool) -> None:
        """Build into temporary storage; callers publish only after validation."""
        output_dir = Path(output_dir)
        includes_dir = output_dir / 'includes'
        inputs_dir = output_dir / 'inputs'

        includes_dir.mkdir(parents=True, exist_ok=True)
        inputs_dir.mkdir(parents=True, exist_ok=True)

        self._run_routers()

        # Typed context for cross-builder entity sharing
        context = BuilderContext()
        generated_files = []

        # Generate includes in registry-defined dependency order
        for file_name, builder_class in BuilderRegistry.get_ordered_builders():
            builder = builder_class(self.spec, context)

            if not builder.should_generate():
                continue

            data = builder.build()

            # Update context with generated entities for cross-references
            context.update_from_dict(builder.get_generated_entities())

            # Write include file
            file_path = includes_dir / file_name
            with open(file_path, 'w', encoding='utf-8') as f:
                yaml.dump(data, f, Dumper=_NoAliasDumper, default_flow_style=False, allow_unicode=True, sort_keys=False)
            generated_files.append(file_name)

        # Generate parameters.yml
        self._write_parameters(inputs_dir / 'parameters.yml')

        # Generate master.yml (only include files that were actually generated)
        master_path = output_dir / 'master.yml'
        self._write_master(master_path, generated_files)

        # A list-style collection REPLACES the collection in OrcaFlex, so two
        # includefiles emitting the same key silently delete the earlier one's
        # objects. This fails closed rather than warning: the defect produces a
        # model that loads cleanly and is missing objects. See workspace-hub#3838.
        check_collection_collisions(master_path, raise_on_collision=True)

        # Post-generation cross-builder validation
        self._run_post_validation(includes_dir, allow_validation_errors=allow_validation_errors)
        self._write_input_manifest(output_dir, generated_files)

    @staticmethod
    def _write_input_manifest(output_dir: Path, generated_files: list[str]) -> Path:
        """Cover every generated input, including parameters outside the include graph."""
        return write_input_manifest(output_dir, [
            "master.yml", "inputs/parameters.yml",
            *(f"includes/{name}" for name in generated_files),
        ])

    def _run_post_validation(
        self, includes_dir: Path, *, allow_validation_errors: bool = False,
    ) -> list[ValidationWarning]:
        """Run post-generation validation and log any findings.

        Validates cross-builder consistency of the generated YAML files.
        Warnings do not block generation. Every error blocks publication unless
        the caller explicitly requests diagnostic output. Native unique-name
        rules and known invalid wave selectors remain enforced by the validator:
        https://www.orcina.com/webhelp/OrcaFlex/Content/html/Datafields.htm
        https://www.orcina.com/webhelp/OrcaFlex/Content/html/Themodel.htm

        Args:
            includes_dir: Directory containing generated include files.

        Returns:
            List of validation warnings found.
        """
        validator = PostGenerationValidator()
        findings = validator.validate_directory(includes_dir)
        for finding in findings:
            if finding.level == "error":
                logger.error(
                    "Post-validation %s: %s", finding.category, finding.message
                )
            else:
                logger.info(
                    "Post-validation %s: %s", finding.category, finding.message
                )
        if not allow_validation_errors and any(w.level == "error" for w in findings):
            raise ModelValidationError(findings)
        return findings

    def _write_parameters(self, path: Path) -> None:
        env = self.spec.environment
        params: dict[str, Any] = {
            'water_depth': env.water.depth,
            'water_density': env.water.density,
        }
        if env.current is not None:
            params['current_speed'] = env.current.speed
            params['current_direction'] = env.current.direction
        if env.wind is not None:
            params['wind_speed'] = env.wind.speed
        if env.waves is not None:
            params['hs'] = env.waves.height
            params['tp'] = env.waves.period
            params['wave_direction'] = env.waves.direction
        params['stage_durations'] = self.spec.simulation.stages
        params['time_step'] = self.spec.simulation.time_step
        with open(path, 'w', encoding='utf-8') as f:
            yaml.dump(params, f, default_flow_style=False)

    def _write_master(self, path: Path, generated_files: list[str] | None = None) -> None:
        include_order = generated_files or BuilderRegistry.get_include_order()
        source_name = self.spec_file.name if self.spec_file else "in-memory spec"
        lines = [
            '%YAML 1.1',
            '# Type: Model',
            f'# Generated from: {source_name}',
            '',
            '---',
        ]
        for file_name in include_order:
            lines.append(f'- includefile: includes/{file_name}')

        with open(path, 'w', encoding='utf-8') as f:
            f.write('\n'.join(lines))

    def generate_with_overrides(
        self,
        output_dir: Path,
        sections: list | None = None,
        variables: dict[str, Any] | None = None,
        template_base_dir: Path | None = None,
        *,
        allow_validation_errors: bool = False,
    ) -> GenerationResult:
        """Validate overridden inputs before publishing, preserving result paths.

        Error findings raise ModelValidationError by default. Explicit
        allow_validation_errors=True writes diagnostic inputs and retains the
        findings in GenerationResult.warnings and ERROR logs.
        """
        output_dir = Path(output_dir)
        with _staged_output(output_dir) as staging:
            result = self._generate_with_overrides(
                staging, sections, variables, template_base_dir,
                allow_validation_errors=allow_validation_errors,
            )
            result.master_file = output_dir / result.master_file.relative_to(staging)
            result.include_files = [output_dir / p.relative_to(staging) for p in result.include_files]
            if result.manifest_file is not None:
                result.manifest_file = output_dir / result.manifest_file.relative_to(staging)
        return result

    def _generate_with_overrides(
        self,
        output_dir: Path,
        sections: list | None,
        variables: dict[str, Any] | None,
        template_base_dir: Path | None,
        *,
        allow_validation_errors: bool,
    ) -> GenerationResult:
        """Generate model with optional section overrides.

        Args:
            output_dir: Root output directory.
            sections: Optional list of InstallationSection overrides.
            variables: Optional variables dict for template resolution.
            template_base_dir: Base directory for resolving template paths.

        Returns:
            GenerationResult with paths and metadata.
        """
        from .sections import VariableResolver

        output_dir = Path(output_dir)
        includes_dir = output_dir / 'includes'
        inputs_dir = output_dir / 'inputs'

        includes_dir.mkdir(parents=True, exist_ok=True)
        inputs_dir.mkdir(parents=True, exist_ok=True)

        self._run_routers()

        # Index sections by builder_file for fast lookup
        section_map: dict[str, Any] = {}
        if sections:
            for sec in sections:
                section_map[sec.builder_file] = sec

        context = BuilderContext()
        generated_files: list[str] = []
        include_paths: list[Path] = []
        resolved_vars: dict[str, Any] = {}
        warnings: list[str] = []

        for file_name, builder_class in BuilderRegistry.get_ordered_builders():
            section = section_map.get(file_name)

            # If section exists and is disabled, skip entirely
            if section and not section.enabled:
                continue

            file_path = includes_dir / file_name

            # If section has a custom template, resolve and write it
            if section and section.template:
                base_dir = template_base_dir or Path(".")
                template_path = base_dir / section.template
                template_content = template_path.read_text()

                # Merge section variables with top-level variables
                merged_vars = dict(variables or {})
                merged_vars.update(section.variables)

                resolved_content = VariableResolver.resolve(
                    template_content, merged_vars
                )
                file_path.write_text(resolved_content, encoding='utf-8')
                resolved_vars.update(merged_vars)
            else:
                # Standard builder path
                builder = builder_class(self.spec, context)

                if not builder.should_generate():
                    continue

                data = builder.build()
                context.update_from_dict(builder.get_generated_entities())

                with open(file_path, 'w', encoding='utf-8') as f:
                    yaml.dump(
                        data, f,
                        Dumper=_NoAliasDumper,
                        default_flow_style=False,
                        allow_unicode=True,
                        sort_keys=False,
                    )

            generated_files.append(file_name)
            include_paths.append(file_path)

        # Write parameters and master
        self._write_parameters(inputs_dir / 'parameters.yml')
        master_path = output_dir / 'master.yml'
        self._write_master(master_path, generated_files)

        # Fails closed — see the note in generate(). Campaign overrides replace
        # whole include files, which is exactly how a second emitter of the same
        # collection key gets introduced. workspace-hub#3838.
        check_collection_collisions(master_path, raise_on_collision=True)

        # Post-generation cross-builder validation
        findings = self._run_post_validation(includes_dir, allow_validation_errors=allow_validation_errors)
        for finding in findings:
            warnings.append(
                f"[{finding.level}] {finding.category}: {finding.message}"
            )

        manifest_path = self._write_input_manifest(output_dir, generated_files)
        return GenerationResult(
            master_file=master_path,
            manifest_file=manifest_path,
            include_files=include_paths,
            variables_resolved=resolved_vars,
            warnings=warnings,
        )
