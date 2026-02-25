"""
ABOUTME: EnvFileGenerator creates environmental condition files for OrcaFlex analysis
ABOUTME: Generates wave, wind, current files and composite environmental files for all headings/return periods
"""

from pathlib import Path
from datetime import datetime
from jinja2 import Environment, FileSystemLoader, select_autoescape
from typing import List, Dict, Any


class EnvFileGenerator:
    """Generate environmental files for OrcaFlex projects"""

    def __init__(self, conditions_profile: str = 'baltic_sea', verbose: bool = False):
        """
        Initialize environmental file generator

        Args:
            conditions_profile: Environmental conditions profile name
            verbose: Enable verbose output
        """
        self.conditions_profile = conditions_profile
        self.verbose = verbose

        # Setup Jinja2 environment
        template_dir = Path(__file__).parent.parent / 'templates' / 'env_files'
        self.env = Environment(
            loader=FileSystemLoader(str(template_dir)),
            autoescape=select_autoescape(),
            trim_blocks=True,
            lstrip_blocks=True
        )

        # Environmental conditions database
        self.conditions = {
            'baltic_sea': {
                '1yr': {'Hs': 2.5, 'Tz': 5.3, 'wind_speed': 15.0, 'current_speed': 0.5},
                '10yr': {'Hs': 3.5, 'Tz': 6.6, 'wind_speed': 20.0, 'current_speed': 0.7},
                '100yr': {'Hs': 4.5, 'Tz': 7.9, 'wind_speed': 25.0, 'current_speed': 1.0}
            }
        }

    def _get_env_params(self, return_period: str, heading: int) -> Dict[str, Any]:
        """Get environmental parameters for return period and heading"""
        conditions = self.conditions[self.conditions_profile][return_period]

        return {
            'return_period': return_period,
            'heading': heading,
            'Hs': conditions['Hs'],
            'Tz': conditions['Tz'],
            'gamma': 3.3,  # JONSWAP peak enhancement factor
            'wind_speed': conditions['wind_speed'],
            'wind_direction': heading,
            'current_speed': conditions['current_speed'],
            'current_direction': heading,
            'generation_date': datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        }

    def _render_template(self, template_name: str, output_path: Path, **params) -> Path:
        """Render a template and write to file"""
        template = self.env.get_template(template_name)
        output = template.render(**params)

        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(output, encoding='utf-8')

        if self.verbose:
            print(f"[OK] Generated: {output_path.name}")

        return output_path

    def generate_wave_file(self, output_dir: Path, return_period: str, heading: int) -> Path:
        """Generate standalone wave file"""
        params = self._get_env_params(return_period, heading)
        filename = f"wave_{return_period}_{heading:03d}deg.yml"

        return self._render_template(
            'wave.yml.j2',
            output_dir / filename,
            **params
        )

    def generate_wind_file(self, output_dir: Path, return_period: str, heading: int) -> Path:
        """Generate standalone wind file"""
        params = self._get_env_params(return_period, heading)
        filename = f"wind_{return_period}_{heading:03d}deg.yml"

        return self._render_template(
            'wind.yml.j2',
            output_dir / filename,
            **params
        )

    def generate_current_file(self, output_dir: Path, return_period: str, heading: int) -> Path:
        """Generate standalone current file"""
        params = self._get_env_params(return_period, heading)
        filename = f"current_{return_period}_{heading:03d}deg.yml"

        return self._render_template(
            'current.yml.j2',
            output_dir / filename,
            **params
        )

    def generate_composite_env_file(self, output_dir: Path, return_period: str, heading: int) -> Path:
        """Generate composite environmental file that includes wave, wind, current"""
        params = self._get_env_params(return_period, heading)
        params.update({
            'wave_file': f"wave_{return_period}_{heading:03d}deg.yml",
            'wind_file': f"wind_{return_period}_{heading:03d}deg.yml",
            'current_file': f"current_{return_period}_{heading:03d}deg.yml"
        })

        filename = f"env_{return_period}_{heading:03d}deg.yml"

        return self._render_template(
            'composite_env.yml.j2',
            output_dir / filename,
            **params
        )

    def generate_all(self, output_dir: Path, return_periods: List[str], headings: List[int]) -> List[Path]:
        """
        Generate all environmental files

        Args:
            output_dir: Output directory path
            return_periods: List of return periods (e.g., ['1yr', '10yr', '100yr'])
            headings: List of headings in degrees (e.g., [0, 30, 60, ...])

        Returns:
            List of generated file paths
        """
        output_path = Path(output_dir)
        files = []

        if self.verbose:
            print(f"\nGenerating environmental files to: {output_path}")
            print(f"Return periods: {return_periods}")
            print(f"Headings: {len(headings)} directions")

        for return_period in return_periods:
            for heading in headings:
                # Generate standalone files
                files.append(self.generate_wave_file(output_path, return_period, heading))
                files.append(self.generate_wind_file(output_path, return_period, heading))
                files.append(self.generate_current_file(output_path, return_period, heading))

                # Generate composite file
                files.append(self.generate_composite_env_file(output_path, return_period, heading))

        return files
