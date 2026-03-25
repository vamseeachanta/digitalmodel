"""
Generate command - Create OrcaFlex files from templates
"""

import click
from pathlib import Path
from ..generators.base_files import BaseFileGenerator
from ..generators.env_files import EnvFileGenerator

@click.group(name='generate')
def generate():
    """Generate OrcaFlex project files"""
    pass

@generate.command(name='base-files')
@click.option('--project', required=True, help='Project name/identifier')
@click.option('--vessel', default='crowley650_atb', help='Vessel type')
@click.option('--water-depth', type=float, default=39.0, help='Water depth in meters')
@click.option('--output', type=click.Path(), required=True, help='Output directory')
@click.option('--mooring-lines', type=int, default=6, help='Number of mooring lines')
@click.pass_context
def base_files(ctx, project, vessel, water_depth, output, mooring_lines):
    """
    Generate modular base files for OrcaFlex project

    Creates the complete base file structure:
    - 01_general.yml (analysis settings)
    - 02_var_data.yml (variables)
    - 03_environment.yml (env reference)
    - 04_vessel_*.yml (vessel configuration)
    - 05_lines.yml (line definitions)
    - 06_buoys.yml (buoy configuration)
    - Master include file
    """
    verbose = ctx.obj.get('VERBOSE', False)

    click.echo(f"\n{'='*60}")
    click.echo(f"Generating Base Files")
    click.echo(f"{'='*60}")
    click.echo(f"Project: {project}")
    click.echo(f"Vessel: {vessel}")
    click.echo(f"Water Depth: {water_depth}m")
    click.echo(f"Mooring Lines: {mooring_lines}")
    click.echo(f"Output: {output}\n")

    # Create generator
    generator = BaseFileGenerator(
        project_name=project,
        vessel_type=vessel,
        water_depth=water_depth,
        num_mooring_lines=mooring_lines,
        verbose=verbose
    )

    # Generate files
    output_path = Path(output)
    files = generator.generate_all(output_path)

    click.echo(f"\n{'='*60}")
    click.echo(f"[SUCCESS] Generated {len(files)} base files")
    click.echo(f"{'='*60}")

    for f in files:
        click.echo(f"  [OK] {f.name}")

    click.echo(f"\nOutput directory: {output_path}\n")

@generate.command(name='env-files')
@click.option('--return-periods', default='1,10,100', help='Comma-separated return periods (years)')
@click.option('--headings', default='all', help='"all" or comma-separated headings (0,30,60,...)')
@click.option('--conditions', default='baltic_sea', help='Environmental conditions profile')
@click.option('--output', type=click.Path(), required=True, help='Output directory')
@click.pass_context
def env_files(ctx, return_periods, headings, conditions, output):
    """
    Generate environmental files for all conditions

    Creates standalone wave, wind, current files plus composite env files
    for all combinations of return periods and headings.
    """
    verbose = ctx.obj.get('VERBOSE', False)

    # Parse return periods
    periods = [f"{int(p)}yr" for p in return_periods.split(',')]

    # Parse headings
    if headings.lower() == 'all':
        heading_list = list(range(0, 360, 15))
    else:
        heading_list = [int(h) for h in headings.split(',')]

    click.echo(f"\n{'='*60}")
    click.echo(f"Generating Environmental Files")
    click.echo(f"{'='*60}")
    click.echo(f"Return Periods: {', '.join(periods)}")
    click.echo(f"Headings: {len(heading_list)} directions")
    click.echo(f"Conditions: {conditions}")
    click.echo(f"Output: {output}\n")

    # Create generator
    generator = EnvFileGenerator(
        conditions_profile=conditions,
        verbose=verbose
    )

    # Generate files
    output_path = Path(output)
    files = generator.generate_all(output_path, periods, heading_list)

    click.echo(f"\n{'='*60}")
    click.echo(f"[SUCCESS] Generated {len(files)} environmental files")
    click.echo(f"{'='*60}")
    click.echo(f"\nOutput directory: {output_path}\n")
