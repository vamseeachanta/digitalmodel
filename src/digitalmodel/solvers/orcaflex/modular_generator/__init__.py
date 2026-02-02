"""OrcaFlex Modular Model Generator from Project Spec."""

from pathlib import Path
from typing import Dict, Any

import yaml

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

# Backward-compatible constant: ordered list of include file names
INCLUDE_ORDER = BuilderRegistry.get_include_order()


class ModularModelGenerator:
    """Generates complete OrcaFlex modular model structure from spec file."""

    def __init__(self, spec_file: Path):
        self.spec_file = Path(spec_file)
        self.spec = self._load_and_validate_spec()

    def _load_and_validate_spec(self) -> ProjectInputSpec:
        with open(self.spec_file) as f:
            data = yaml.safe_load(f)
        return ProjectInputSpec(**data)

    def generate(self, output_dir: Path) -> None:
        output_dir = Path(output_dir)
        includes_dir = output_dir / 'includes'
        inputs_dir = output_dir / 'inputs'

        includes_dir.mkdir(parents=True, exist_ok=True)
        inputs_dir.mkdir(parents=True, exist_ok=True)

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
            with open(file_path, 'w') as f:
                yaml.dump(data, f, default_flow_style=False, allow_unicode=True, sort_keys=False)
            generated_files.append(file_name)

        # Generate parameters.yml
        self._write_parameters(inputs_dir / 'parameters.yml')

        # Generate master.yml (only include files that were actually generated)
        self._write_master(output_dir / 'master.yml', generated_files)

    def _write_parameters(self, path: Path) -> None:
        params = {
            'water_depth': self.spec.environment.water.depth,
            'water_density': self.spec.environment.water.density,
            'current_speed': self.spec.environment.current.speed,
            'current_direction': self.spec.environment.current.direction,
            'wind_speed': self.spec.environment.wind.speed,
            'hs': self.spec.environment.waves.height,
            'tp': self.spec.environment.waves.period,
            'wave_direction': self.spec.environment.waves.direction,
            'stage_durations': self.spec.simulation.stages,
            'time_step': self.spec.simulation.time_step,
        }
        with open(path, 'w') as f:
            yaml.dump(params, f, default_flow_style=False)

    def _write_master(self, path: Path, generated_files: list[str] | None = None) -> None:
        include_order = generated_files or BuilderRegistry.get_include_order()
        lines = [
            '%YAML 1.1',
            '# Type: Model',
            f'# Generated from: {self.spec_file.name}',
            '',
        ]
        for file_name in include_order:
            lines.append(f'- includefile: includes/{file_name}')

        with open(path, 'w') as f:
            f.write('\n'.join(lines))
