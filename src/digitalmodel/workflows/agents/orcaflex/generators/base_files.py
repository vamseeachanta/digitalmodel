"""
ABOUTME: BaseFileGenerator creates modular OrcaFlex base files from Jinja2 templates
ABOUTME: Generates general settings, vessel config, lines, buoys, and master include files
"""

from pathlib import Path
from datetime import datetime
from jinja2 import Environment, FileSystemLoader, select_autoescape
from typing import List, Dict, Any
import math


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

        # Add math functions to Jinja2 environment
        self.env.globals['cos'] = math.cos
        self.env.globals['sin'] = math.sin

        # Vessel-specific configurations
        self.vessel_configs = {
            'crowley650_atb': {
                'length': 195.0,
                'beam': 32.3,
                'draft': 6.1,
                'displacement': 30000.0,
                'cog_x': 0.0,
                'cog_y': 0.0,
                'cog_z': -3.0,
                'ixx': 2.5e8,
                'iyy': 6.0e9,
                'izz': 6.0e9,
                'superstructure_area': 500.0,
                'lateral_wind_area': 2000.0,
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

            # Vessel parameters
            'vessel_length': vessel_config.get('length', 195.0),
            'vessel_beam': vessel_config.get('beam', 32.3),
            'vessel_draft': vessel_config.get('draft', 6.1),
            'vessel_displacement': vessel_config.get('displacement', 30000.0),
            'cog_x': vessel_config.get('cog_x', 0.0),
            'cog_y': vessel_config.get('cog_y', 0.0),
            'cog_z': vessel_config.get('cog_z', -3.0),
            'ixx': vessel_config.get('ixx', 2.5e8),
            'iyy': vessel_config.get('iyy', 6.0e9),
            'izz': vessel_config.get('izz', 6.0e9),
            'superstructure_area': vessel_config.get('superstructure_area', 500.0),
            'lateral_wind_area': vessel_config.get('lateral_wind_area', 2000.0),
            'rao_file': vessel_config.get('rao_file', 'vessel_RAO.txt'),
            'amd_file': vessel_config.get('amd_file', 'vessel_AMD.txt'),

            # General settings
            'stage_duration_buildup': 10,
            'stage_duration_simulation': 100,
            'statics_min_damping': 5,
            'use_calculated_mean_position': 'No',
            'dynamics_solution_method': 'Implicit time domain',
            'line_contact': 'No',

            # Line settings
            'line_length': self.water_depth + 100.0,
            'line_diameter': 0.084,
            'line_mass_per_length': 49.0,
            'line_ea': 1.0e9,
            'vessel_fairlead_x': 0.0,
            'vessel_fairlead_y': 0.0,
            'vessel_fairlead_z': -vessel_config.get('draft', 6.1),

            # Buoy settings
            'buoy_mass': 5000.0,
            'buoy_draft': 1.0,
            'buoy_diameter': 3.0,
            'buoy_waterplane_area': 3.14159 * (3.0/2)**2
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

    def generate_environment(self, output_dir: Path) -> Path:
        """Generate 02_environment.yml"""
        return self._render_template(
            '02_environment.yml.j2',
            output_dir / '02_environment.yml'
        )

    def generate_vessel_type(self, output_dir: Path) -> tuple:
        """Generate 04_vessel wrapper and data files"""
        wrapper = self._render_template(
            '04_vessel_wrapper.yml.j2',
            output_dir / '04_vessel.yml'
        )
        data = self._render_template(
            '_04_vessel_data.yml.j2',
            output_dir / '_04_vessel_data.yml'
        )
        return (wrapper, data)

    def generate_vessel_instance(self, output_dir: Path) -> tuple:
        """Generate 05_vessel_inst wrapper and data files"""
        wrapper = self._render_template(
            '05_vessel_inst_wrapper.yml.j2',
            output_dir / '05_vessel_inst.yml'
        )
        data = self._render_template(
            '_05_vessel_inst_data.yml.j2',
            output_dir / '_05_vessel_inst_data.yml'
        )
        return (wrapper, data)

    def generate_line_types(self, output_dir: Path) -> Path:
        """Generate 06_line_types.yml"""
        return self._render_template(
            '06_line_types.yml.j2',
            output_dir / '06_line_types.yml'
        )

    def generate_lines(self, output_dir: Path) -> tuple:
        """Generate 07_lines wrapper and data files"""
        wrapper = self._render_template(
            '07_lines_wrapper.yml.j2',
            output_dir / '07_lines.yml'
        )
        data = self._render_template(
            '_07_lines_data.yml.j2',
            output_dir / '_07_lines_data.yml'
        )
        return (wrapper, data)

    def generate_buoys(self, output_dir: Path) -> tuple:
        """Generate 08_buoys wrapper and data files"""
        wrapper = self._render_template(
            '08_buoys_wrapper.yml.j2',
            output_dir / '08_buoys.yml'
        )
        data = self._render_template(
            '_08_buoys_data.yml.j2',
            output_dir / '_08_buoys_data.yml'
        )
        return (wrapper, data)

    def generate_groups(self, output_dir: Path) -> Path:
        """Generate 09_groups.yml"""
        return self._render_template(
            '09_groups.yml.j2',
            output_dir / '09_groups.yml'
        )

    def generate_master_include(self, output_dir: Path) -> Path:
        """Generate master include file that references all base files"""
        return self._render_template(
            '_master_base.yml.j2',
            output_dir / f"{self.project_name}_base.yml"
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
            self.generate_environment(output_path),
        ]

        # Vessel type (wrapper + data)
        vessel_type_files = self.generate_vessel_type(output_path)
        files.extend(vessel_type_files)

        # Vessel instance (wrapper + data)
        vessel_inst_files = self.generate_vessel_instance(output_path)
        files.extend(vessel_inst_files)

        files.append(self.generate_line_types(output_path))

        # Lines (wrapper + data)
        lines_files = self.generate_lines(output_path)
        files.extend(lines_files)

        # Buoys (wrapper + data)
        buoys_files = self.generate_buoys(output_path)
        files.extend(buoys_files)

        files.extend([
            self.generate_groups(output_path),
            self.generate_master_include(output_path)
        ])

        return files
