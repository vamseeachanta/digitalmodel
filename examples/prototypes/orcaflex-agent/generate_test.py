#!/usr/bin/env python3
"""
OrcaFlex Agent Prototype
Proof of concept for template-based file generation
"""

from jinja2 import Environment, FileSystemLoader
from datetime import datetime
from pathlib import Path

def generate_base_file(template_name, output_path, **params):
    """Generate OrcaFlex base file from template"""

    # Setup Jinja2 environment
    template_dir = Path(__file__).parent / 'templates' / 'base_files'
    env = Environment(loader=FileSystemLoader(str(template_dir)))

    # Load template
    template = env.get_template(template_name)

    # Add generation date
    params['generation_date'] = datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    # Render template
    output = template.render(**params)

    # Write to file
    output_file = Path(output_path)
    output_file.parent.mkdir(parents=True, exist_ok=True)
    output_file.write_text(output)

    print(f"[OK] Generated: {output_file}")
    return output_file

def main():
    """Run proof of concept generation"""

    print("=" * 60)
    print("OrcaFlex Agent - Prototype Test")
    print("=" * 60)

    # Define project parameters
    project_params = {
        'project_name': 'CALM_Buoy_Baltic_POC',
        'stage_duration_buildup': -10.0,
        'stage_duration_simulation': 100.0,
        'log_sample_interval': 0.1,
        'time_step': 0.1,
        'wave_type': 'JONSWAP',
        'wave_direction': 0,
        'water_depth': 39.0,
        'seabed_z': -39.0
    }

    # Generate 01_general.yml
    output_dir = Path(__file__).parent / 'output'
    generate_base_file(
        '01_general.yml.j2',
        output_dir / '01_general.yml',
        **project_params
    )

    # Generate 02_var_data.yml
    generate_base_file(
        '02_var_data.yml.j2',
        output_dir / '02_var_data.yml',
        **project_params
    )

    print("\n" + "=" * 60)
    print("[SUCCESS] Prototype generation complete!")
    print(f"Output directory: {output_dir}")
    print("=" * 60)

if __name__ == '__main__':
    main()
