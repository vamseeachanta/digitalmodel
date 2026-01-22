"""OrcaFlex Modular Model Generator from Project Spec."""

from pathlib import Path
from typing import Dict, Any, List
import yaml

from .schema import ProjectInputSpec
from .builders.base import BaseBuilder
from .builders.general_builder import GeneralBuilder
from .builders.environment_builder import EnvironmentBuilder
from .builders.vardata_builder import VarDataBuilder
from .builders.linetype_builder import LineTypeBuilder
from .builders.supports_builder import SupportsBuilder
from .builders.morison_builder import MorisonBuilder
from .builders.shapes_builder import ShapesBuilder
from .builders.buoys_builder import BuoysBuilder
from .builders.lines_builder import LinesBuilder
from .builders.groups_builder import GroupsBuilder

# Critical: Explicit include order per OrcaFlex requirements
INCLUDE_ORDER = [
    '01_general.yml',
    '02_var_data.yml',
    '03_environment.yml',
    '05_line_types.yml',
    '13_supports.yml',
    '14_morison.yml',
    '09_shapes.yml',
    '08_buoys.yml',
    '07_lines.yml',
    '10_groups.yml',
]

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

        # Context for cross-builder entity sharing
        context: Dict[str, Any] = {}

        # Builder registry mapping file names to builder classes
        builders = {
            '01_general.yml': GeneralBuilder,
            '02_var_data.yml': VarDataBuilder,
            '03_environment.yml': EnvironmentBuilder,
            '05_line_types.yml': LineTypeBuilder,
            '13_supports.yml': SupportsBuilder,
            '14_morison.yml': MorisonBuilder,
            '09_shapes.yml': ShapesBuilder,
            '08_buoys.yml': BuoysBuilder,
            '07_lines.yml': LinesBuilder,
            '10_groups.yml': GroupsBuilder,
        }

        # Generate includes in explicit dependency order
        for file_name in INCLUDE_ORDER:
            builder_class = builders[file_name]
            builder = builder_class(self.spec, context)
            data = builder.build()

            # Update context with generated entities for cross-references
            context.update(builder.get_generated_entities())

            # Write include file
            file_path = includes_dir / file_name
            with open(file_path, 'w') as f:
                yaml.dump(data, f, default_flow_style=False, allow_unicode=True, sort_keys=False)

        # Generate parameters.yml
        self._write_parameters(inputs_dir / 'parameters.yml')

        # Generate master.yml
        self._write_master(output_dir / 'master.yml')

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

    def _write_master(self, path: Path) -> None:
        lines = [
            '%YAML 1.1',
            '# Type: Model',
            f'# Generated from: {self.spec_file.name}',
            '',
        ]
        for file_name in INCLUDE_ORDER:
            lines.append(f'- includefile: includes/{file_name}')

        with open(path, 'w') as f:
            f.write('\n'.join(lines))
