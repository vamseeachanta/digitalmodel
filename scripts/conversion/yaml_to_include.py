#!/usr/bin/env python3
"""
OrcaFlex YAML to Include-Based Format Converter

Converts flat OrcaFlex YAML files to include-based format with:
- Master file with includes and input parameters
- Separate include files for each section
- Extracted input parameters for parametric analysis
"""

import os
import re
import yaml
import argparse
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime


# Section mappings for include files
SECTION_MAPPING = {
    'General': '01_general.yml',
    'VariableData': '02_var_data.yml',
    'Environment': '03_environment.yml',
    'VesselTypes': '04_vessel_types.yml',
    'LineTypes': '05_line_types.yml',
    'Vessels': '06_vessels.yml',
    'Lines': '07_lines.yml',
    '6DBuoys': '08_buoys.yml',
    'Buoys': '08_buoys.yml',
    'BuoyTypes': '08a_buoy_types.yml',
    'Shapes': '09_shapes.yml',
    'Constraints': '10_constraints.yml',
    'Links': '11_links.yml',
    'Winches': '12_winches.yml',
    'Supports': '13_supports.yml',
    'SupportTypes': '13_supports.yml',      # Support type definitions
    'MorisonElementTypes': '14_morison.yml', # Morison element definitions
    '3DBuoys': '08_buoys.yml',               # Combine with 6DBuoys
    'Groups': '10_groups.yml',               # Model grouping
    'TurbineTypes': '14_turbine_types.yml',
    'Turbines': '15_turbines.yml',
    'DragChainTypes': '16_drag_chain_types.yml',
    'DragChains': '17_drag_chains.yml',
    'FlexJoints': '18_flex_joints.yml',
    'SolidFrictionCoefficients': '19_friction.yml',
}

# Parameters to extract for input block
INPUT_PARAMETERS = {
    'Environment': {
        'SeabedOriginDepth': 'water_depth',
        'WaveHeight': 'hs',
        'WavePeriod': 'tp',
        'RefCurrentSpeed': 'current_speed',
        'RefCurrentDirection': 'current_direction',
        'WindSpeed': 'wind_speed',
        'Density': 'water_density',
    },
    'General': {
        'StageDuration': 'stage_durations',
        'ImplicitConstantTimeStep': 'time_step',
    },
}


class OrcaFlexDumper(yaml.SafeDumper):
    """Custom YAML dumper for OrcaFlex format."""
    pass


def represent_none(dumper, data):
    """Represent None as ~ (OrcaFlex format)."""
    return dumper.represent_scalar('tag:yaml.org,2002:null', '~')


def represent_bool(dumper, data):
    """Represent bool as Yes/No (OrcaFlex format)."""
    return dumper.represent_scalar('tag:yaml.org,2002:bool', 'Yes' if data else 'No')


OrcaFlexDumper.add_representer(type(None), represent_none)
OrcaFlexDumper.add_representer(bool, represent_bool)


class OrcaFlexYAMLConverter:
    """Converts flat OrcaFlex YAML to include-based format."""

    def __init__(self, source_path: str, output_dir: Optional[str] = None):
        self.source_path = Path(source_path)
        self.output_dir = Path(output_dir) if output_dir else self.source_path.parent / self.source_path.stem
        self.data: Dict[str, Any] = {}
        self.header_lines: List[str] = []
        self.input_params: Dict[str, Any] = {}

    def parse_yaml_with_header(self) -> None:
        """Parse YAML file preserving header comments."""
        with open(self.source_path, 'r', encoding='utf-8-sig') as f:
            content = f.read()

        # Split into header and YAML content
        lines = content.split('\n')
        yaml_start = 0

        for i, line in enumerate(lines):
            if line.strip() == '---':
                yaml_start = i + 1
                self.header_lines = lines[:i+1]
                break
            elif line.startswith('%YAML') or line.startswith('#'):
                continue
            else:
                # First non-comment, non-directive line
                yaml_start = i
                self.header_lines = lines[:i]
                break

        yaml_content = '\n'.join(lines[yaml_start:])
        self.data = yaml.safe_load(yaml_content) or {}

    def extract_input_parameters(self) -> Dict[str, Any]:
        """Extract key parameters for input block."""
        inputs = {}

        for section, params in INPUT_PARAMETERS.items():
            if section in self.data:
                section_data = self.data[section]
                for yaml_key, input_key in params.items():
                    if yaml_key in section_data:
                        value = section_data[yaml_key]
                        if value is not None and value != '~':
                            inputs[input_key] = value

        # Extract wave parameters from WaveTrains if present
        if 'Environment' in self.data:
            env = self.data['Environment']
            if 'WaveTrains' in env and env['WaveTrains']:
                wave = env['WaveTrains'][0]
                if 'WaveHeight' in wave:
                    inputs['hs'] = wave['WaveHeight']
                if 'WavePeriod' in wave:
                    inputs['tp'] = wave['WavePeriod']
                if 'WaveDirection' in wave:
                    inputs['wave_direction'] = wave['WaveDirection']

        self.input_params = inputs
        return inputs

    def split_into_sections(self) -> Dict[str, Dict]:
        """Split data into section dictionaries."""
        sections = {}

        for key, value in self.data.items():
            if key in SECTION_MAPPING:
                sections[key] = {key: value}
            else:
                # Unknown sections go to general
                if 'General' not in sections:
                    sections['General'] = {'General': {}}
                sections['General'][key] = value

        return sections

    def generate_include_file(self, section_name: str, section_data: Dict,
                               filename: str) -> str:
        """Generate content for an include file."""
        header = f"""# {section_name} Configuration
# Generated from: {self.source_path.name}
# Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
"""

        # Use safe_dump with custom options
        yaml_content = yaml.dump(
            section_data,
            Dumper=OrcaFlexDumper,
            default_flow_style=False,
            allow_unicode=True,
            sort_keys=False,
            width=1000,
        )

        return header + yaml_content

    def generate_master_file(self, include_files: List[str]) -> str:
        """Generate the master file with includes and inputs."""
        header = '\n'.join(self.header_lines) if self.header_lines else """%YAML 1.1
# Type: Model
# Program: OrcaFlex 11.5
---"""

        # Build input parameters as comments (not YAML block to keep file valid)
        inputs_comments = ""
        if self.input_params:
            inputs_comments = "# Input Parameters (for parametric analysis)\n"
            inputs_comments += "# See inputs/parameters.yml for extracted values:\n"
            for key, value in self.input_params.items():
                if isinstance(value, list):
                    inputs_comments += f"#   {key}: {value}\n"
                else:
                    inputs_comments += f"#   {key}: {value}\n"
            inputs_comments += "#\n"

        # Build include directives
        includes = []
        for inc_file in include_files:
            includes.append(f"- includefile: includes/{inc_file}")

        includes_block = '\n'.join(includes)

        content = f"""{header}
{inputs_comments}# Include files
{includes_block}
"""
        return content

    def convert(self) -> Path:
        """Execute the conversion."""
        # Create output directory structure
        self.output_dir.mkdir(parents=True, exist_ok=True)
        includes_dir = self.output_dir / 'includes'
        includes_dir.mkdir(exist_ok=True)
        inputs_dir = self.output_dir / 'inputs'
        inputs_dir.mkdir(exist_ok=True)

        # Parse source file
        self.parse_yaml_with_header()

        # Extract input parameters
        self.extract_input_parameters()

        # Split into sections
        sections = self.split_into_sections()

        # Generate include files
        include_files = []
        for section_name, section_data in sections.items():
            if section_name in SECTION_MAPPING:
                filename = SECTION_MAPPING[section_name]
                content = self.generate_include_file(section_name, section_data, filename)

                include_path = includes_dir / filename
                with open(include_path, 'w', encoding='utf-8') as f:
                    f.write(content)

                include_files.append(filename)

        # Sort include files by number prefix
        include_files.sort()

        # Generate master file
        master_content = self.generate_master_file(include_files)
        master_path = self.output_dir / 'master.yml'
        with open(master_path, 'w', encoding='utf-8') as f:
            f.write(master_content)

        # Generate parameters file
        if self.input_params:
            params_yaml = yaml.dump(self.input_params, Dumper=OrcaFlexDumper, default_flow_style=False, sort_keys=False)
            params_content = f"""# Input Parameters for Parametric Analysis
# Source: {self.source_path.name}
# Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

{params_yaml}
"""
            params_path = inputs_dir / 'parameters.yml'
            with open(params_path, 'w', encoding='utf-8') as f:
                f.write(params_content)

        return self.output_dir


def batch_convert(source_dir: str, output_base: str, pattern: str = "*.yml") -> List[Path]:
    """Batch convert multiple YAML files."""
    source_path = Path(source_dir)
    output_base_path = Path(output_base)

    converted = []
    for yml_file in source_path.rglob(pattern):
        # Skip already converted files (in includes directories)
        if 'includes' in str(yml_file) or 'master.yml' in str(yml_file):
            continue

        # Create output path preserving relative structure
        rel_path = yml_file.relative_to(source_path)
        output_dir = output_base_path / rel_path.parent / yml_file.stem

        try:
            converter = OrcaFlexYAMLConverter(str(yml_file), str(output_dir))
            result = converter.convert()
            converted.append(result)
            print(f"Converted: {yml_file.name} -> {result}")
        except Exception as e:
            print(f"Error converting {yml_file}: {e}")

    return converted


def main():
    parser = argparse.ArgumentParser(
        description='Convert OrcaFlex YAML files to include-based format'
    )
    parser.add_argument('source', help='Source YAML file or directory')
    parser.add_argument('-o', '--output', help='Output directory (default: source_name/)')
    parser.add_argument('-b', '--batch', action='store_true',
                        help='Batch convert all YAML files in directory')
    parser.add_argument('-p', '--pattern', default='*.yml',
                        help='File pattern for batch mode (default: *.yml)')

    args = parser.parse_args()

    if args.batch:
        output_base = args.output or args.source
        converted = batch_convert(args.source, output_base, args.pattern)
        print(f"\nConverted {len(converted)} files")
    else:
        converter = OrcaFlexYAMLConverter(args.source, args.output)
        result = converter.convert()
        print(f"Converted to: {result}")


if __name__ == '__main__':
    main()
