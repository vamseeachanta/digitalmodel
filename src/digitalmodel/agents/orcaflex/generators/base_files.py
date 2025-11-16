"""
ABOUTME: BaseFileGenerator creates modular OrcaFlex base files from Jinja2 templates
ABOUTME: Generates general settings, vessel config, lines, buoys, and master include files
"""

from pathlib import Path
from datetime import datetime
from jinja2 import Environment, FileSystemLoader, select_autoescape
from typing import List, Dict, Any


class BaseFileGenerator:
    """Generate modular base files for OrcaFlex projects"""

    def __init__(self, project_name: str, vessel_type: str, water_depth: float,
                 num_mooring_lines: int = 6, verbose: bool = False):
        """
        Initialize base file generator

        Args:
            project_name: Project identifier
            vessel_type: Vessel type (e.g., 'crowley650_atb')
            water_depth: Water depth in meters
            num_mooring_lines: Number of mooring lines (default: 6)
            verbose: Enable verbose output
        """
        self.project_name = project_name
        self.vessel_type = vessel_type
        self.water_depth = water_depth
        self.num_mooring_lines = num_mooring_lines
        self.verbose = verbose

        # Setup Jinja2 environment
        template_dir = Path(__file__).parent.parent / 'templates' / 'base_files'
        self.env = Environment(
            loader=FileSystemLoader(str(template_dir)),
            autoescape=select_autoescape(),
            trim_blocks=True,
            lstrip_blocks=True
        )

        # Vessel-specific configurations
        self.vessel_configs = {
            'crowley650_atb': {
                'length': 195.0,
                'beam': 32.3,
                'draft': 6.1,
                'displacement': 30000.0,
                'rao_file': 'crowley650_atb_RAO.txt',
                'amd_file': 'crowley650_atb_AMD.txt'
            }
        }

    def _get_template_params(self) -> Dict[str, Any]:
        """Get common template parameters"""
        vessel_config = self.vessel_configs.get(self.vessel_type, {})

        return {
            'project_name': self.project_name,
            'vessel_type': self.vessel_type,
            'water_depth': self.water_depth,
            'num_mooring_lines': self.num_mooring_lines,
            'generation_date': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
            'vessel_length': vessel_config.get('length', 195.0),
            'vessel_beam': vessel_config.get('beam', 32.3),
            'vessel_draft': vessel_config.get('draft', 6.1),
            'vessel_displacement': vessel_config.get('displacement', 30000.0),
            'rao_file': vessel_config.get('rao_file', 'vessel_RAO.txt'),
            'amd_file': vessel_config.get('amd_file', 'vessel_AMD.txt'),
            # General settings
            'stage_duration_buildup': -10.0,
            'stage_duration_simulation': 100.0,
            'log_sample_interval': 0.1,
            'time_step': 0.1,
            'wave_type': 'JONSWAP',
            'wave_direction': 0,
            # Line settings
            'line_length': self.water_depth + 100.0,  # Catenary length
            'line_diameter': 0.084,  # 84mm chain
            'line_mass_per_length': 49.0,  # kg/m for R3 studless chain
            # Buoy settings
            'buoy_diameter': 3.0,
            'buoy_height': 2.0,
            'buoy_mass': 5000.0
        }

    def _render_template(self, template_name: str, output_path: Path, **extra_params) -> Path:
        """Render a template and write to file"""
        params = self._get_template_params()
        params.update(extra_params)

        template = self.env.get_template(template_name)
        output = template.render(**params)

        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(output, encoding='utf-8')

        if self.verbose:
            print(f"[OK] Generated: {output_path.name}")

        return output_path

    def generate_general(self, output_dir: Path) -> Path:
        """Generate 01_general.yml"""
        return self._render_template(
            '01_general.yml.j2',
            output_dir / '01_general.yml'
        )

    def generate_var_data(self, output_dir: Path) -> Path:
        """Generate 02_var_data.yml"""
        return self._render_template(
            '02_var_data.yml.j2',
            output_dir / '02_var_data.yml'
        )

    def generate_environment_ref(self, output_dir: Path) -> Path:
        """Generate 03_environment.yml (reference to env files)"""
        return self._render_template(
            '03_environment.yml.j2',
            output_dir / '03_environment.yml'
        )

    def generate_vessel(self, output_dir: Path) -> Path:
        """Generate 04_vessel_{vessel_type}.yml"""
        filename = f"04_vessel_{self.vessel_type}.yml"
        return self._render_template(
            '04_vessel.yml.j2',
            output_dir / filename
        )

    def generate_lines(self, output_dir: Path) -> Path:
        """Generate 05_lines.yml"""
        return self._render_template(
            '05_lines.yml.j2',
            output_dir / '05_lines.yml'
        )

    def generate_buoys(self, output_dir: Path) -> Path:
        """Generate 06_buoys.yml"""
        return self._render_template(
            '06_buoys.yml.j2',
            output_dir / '06_buoys.yml'
        )

    def generate_groups(self, output_dir: Path) -> Path:
        """Generate 08_groups.yml"""
        return self._render_template(
            '08_groups.yml.j2',
            output_dir / '08_groups.yml'
        )

    def generate_calculated_positions(self, output_dir: Path) -> Path:
        """Generate _90_calculated_positions.yml"""
        return self._render_template(
            '_90_calculated_positions.yml.j2',
            output_dir / '_90_calculated_positions.yml'
        )

    def generate_master_include(self, output_dir: Path) -> Path:
        """Generate master include file that references all base files"""
        vessel_file = f"04_vessel_{self.vessel_type}.yml"

        return self._render_template(
            '_master_base.yml.j2',
            output_dir / f"{self.project_name}_base.yml",
            vessel_file=vessel_file
        )

    def generate_all(self, output_dir: Path) -> List[Path]:
        """
        Generate all base files

        Args:
            output_dir: Output directory path

        Returns:
            List of generated file paths
        """
        output_path = Path(output_dir)

        if self.verbose:
            print(f"\nGenerating base files to: {output_path}")

        files = [
            self.generate_general(output_path),
            self.generate_var_data(output_path),
            self.generate_environment_ref(output_path),
            self.generate_vessel(output_path),
            self.generate_lines(output_path),
            self.generate_buoys(output_path),
            self.generate_groups(output_path),
            self.generate_calculated_positions(output_path),
            self.generate_master_include(output_path)
        ]

        return files
