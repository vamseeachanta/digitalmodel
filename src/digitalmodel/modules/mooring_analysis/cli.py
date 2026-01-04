#!/usr/bin/env python3
"""
ABOUTME: Command-line interface for mooring analysis providing catenary calculations,
design verification, and OrcaFlex model generation.
"""

import click
import json
import sys
from pathlib import Path
from typing import Optional

from . import (
    __version__,
    MooringLineProperties,
    LineType,
    CatenaryAnalyzer,
    MooringDesigner,
    MooringSystem,
    MooringLine,
    MooringType,
    AnchorProperties,
    VesselParticulars,
    EnvironmentalConditions,
    DesignLoadCase,
    ConditionType,
    OrcaFlexModelGenerator,
    MOORING_MATERIALS,
    get_material,
)


@click.group()
@click.version_option(version=__version__, prog_name="mooring-analysis")
def cli():
    """Mooring Analysis Tools - CLI for catenary analysis, design verification, and model generation"""
    pass


@cli.command('catenary')
@click.option('--water-depth', type=float, required=True, help='Water depth (m)')
@click.option('--line-length', type=float, required=True, help='Line length (m)')
@click.option('--line-weight', type=float, required=True, help='Weight in water (kg/m)')
@click.option('--line-diameter', type=float, default=84.0, help='Line diameter (mm)')
@click.option('--line-ea', type=float, default=850000.0, help='Axial stiffness EA (kN)')
@click.option('--horizontal-tension', type=float, help='Horizontal tension H (kN)')
@click.option('--fairlead-tension', type=float, help='Target fairlead tension (kN)')
@click.option('--output', '-o', type=click.Path(), help='Output file (JSON)')
def catenary_analysis(
    water_depth: float,
    line_length: float,
    line_weight: float,
    line_diameter: float,
    line_ea: float,
    horizontal_tension: Optional[float],
    fairlead_tension: Optional[float],
    output: Optional[str]
):
    """Calculate catenary geometry and tensions"""

    # Create line properties
    line = MooringLineProperties(
        line_type=LineType.CHAIN,
        length=line_length,
        diameter=line_diameter,
        mbl=10000.0,  # Placeholder
        weight_water=line_weight,
        ea=line_ea
    )

    # Create analyzer
    analyzer = CatenaryAnalyzer(water_depth)

    # Solve catenary
    try:
        if fairlead_tension:
            result = analyzer.solve_for_horizontal_tension(
                line,
                target_fairlead_tension=fairlead_tension,
                vertical_height=water_depth
            )
            solve_method = "fairlead_tension"
        elif horizontal_tension:
            result = analyzer.solve_catenary(
                line,
                horizontal_tension=horizontal_tension,
                vertical_height=water_depth
            )
            solve_method = "horizontal_tension"
        else:
            click.echo("Error: Must specify either --horizontal-tension or --fairlead-tension", err=True)
            sys.exit(1)

        # Calculate stiffness
        stiffness = analyzer.calculate_stiffness(line, result)

        # Output results
        results = {
            'solve_method': solve_method,
            'input': {
                'water_depth': water_depth,
                'line_length': line_length,
                'line_weight': line_weight,
                'line_ea': line_ea,
            },
            'catenary': {
                'horizontal_tension': round(result.horizontal_tension, 2),
                'fairlead_tension': round(result.fairlead_tension, 2),
                'touchdown_tension': round(result.touchdown_tension, 2),
                'arc_length': round(result.arc_length, 2),
                'horizontal_distance': round(result.horizontal_distance, 2),
                'fairlead_angle': round(result.fairlead_angle, 2),
                'grounded_length': round(result.grounded_length, 2),
            },
            'stiffness': {
                'horizontal': round(stiffness.horizontal_stiffness, 2),
                'vertical': round(stiffness.vertical_stiffness, 2),
                'geometric': round(stiffness.geometric_stiffness, 2),
                'elastic': round(stiffness.elastic_stiffness, 2),
            }
        }

        # Print summary
        click.echo("\n=== Catenary Analysis Results ===\n")
        click.echo(f"Horizontal Tension:    {result.horizontal_tension:.2f} kN")
        click.echo(f"Fairlead Tension:      {result.fairlead_tension:.2f} kN")
        click.echo(f"Fairlead Angle:        {result.fairlead_angle:.2f}°")
        click.echo(f"Arc Length:            {result.arc_length:.2f} m")
        click.echo(f"Horizontal Distance:   {result.horizontal_distance:.2f} m")
        click.echo(f"Grounded Length:       {result.grounded_length:.2f} m")
        click.echo(f"\nHorizontal Stiffness:  {stiffness.horizontal_stiffness:.2f} kN/m")

        # Save to file
        if output:
            with open(output, 'w') as f:
                json.dump(results, f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('design')
@click.option('--system-type', type=click.Choice(['calm', 'salm', 'spread']), required=True, help='Mooring system type')
@click.option('--water-depth', type=float, required=True, help='Water depth (m)')
@click.option('--n-lines', type=int, default=6, help='Number of mooring lines')
@click.option('--anchor-radius', type=float, default=400.0, help='Anchor circle radius (m)')
@click.option('--fairlead-radius', type=float, default=20.0, help='Fairlead circle radius (m)')
@click.option('--material', type=str, default='chain_r3_84', help='Line material from library')
@click.option('--line-length', type=float, default=500.0, help='Total line length (m)')
@click.option('--pretension', type=float, default=500.0, help='Pretension (kN)')
@click.option('--wave-hs', type=float, default=8.0, help='Significant wave height (m)')
@click.option('--current-speed', type=float, default=1.0, help='Current speed (m/s)')
@click.option('--wind-speed', type=float, default=20.0, help='Wind speed (m/s)')
@click.option('--check-damaged', is_flag=True, help='Include damaged line analysis')
@click.option('--output', '-o', type=click.Path(), help='Output file (JSON)')
def design_verification(
    system_type: str,
    water_depth: float,
    n_lines: int,
    anchor_radius: float,
    fairlead_radius: float,
    material: str,
    line_length: float,
    pretension: float,
    wave_hs: float,
    current_speed: float,
    wind_speed: float,
    check_damaged: bool,
    output: Optional[str]
):
    """Design verification with safety factor checks"""

    try:
        # Get material from library
        base_material = get_material(material)
        base_material.length = line_length

        # Create vessel
        vessel = VesselParticulars(
            vessel_type="tanker",
            length=280.0,
            beam=46.0,
            draft=17.5,
            displacement=150000.0,
            windage_area=6000.0
        )

        # Create mooring lines
        lines = []
        angle_spacing = 360.0 / n_lines

        for i in range(n_lines):
            angle = i * angle_spacing
            angle_rad = angle * 3.14159 / 180.0

            # Anchor location
            anchor_x = anchor_radius * np.cos(angle_rad)
            anchor_y = anchor_radius * np.sin(angle_rad)

            # Fairlead location
            fairlead_x = fairlead_radius * np.cos(angle_rad)
            fairlead_y = fairlead_radius * np.sin(angle_rad)

            anchor = AnchorProperties(
                anchor_type="suction_pile",
                holding_capacity=5000.0,
                location=(anchor_x, anchor_y, -water_depth)
            )

            line = MooringLine(
                line_id=f"ML{i+1}",
                segments=[base_material],
                anchor=anchor,
                fairlead_location=(fairlead_x, fairlead_y, -10.0),
                pretension=pretension
            )
            lines.append(line)

        # Create mooring system
        system = MooringSystem(
            system_type=MooringType[system_type.upper()],
            water_depth=water_depth,
            lines=lines,
            vessel=vessel
        )

        # Create environment
        env = EnvironmentalConditions(
            wave_hs=wave_hs,
            wave_tp=12.0,
            wave_direction=0.0,
            current_speed=current_speed,
            current_direction=0.0,
            wind_speed=wind_speed,
            wind_direction=0.0,
            return_period=100
        )

        # Create designer
        designer = MooringDesigner(system)

        # Calculate environmental loads
        env_loads = designer.calculate_environmental_loads(env)

        # Create load cases
        load_cases = [
            DesignLoadCase(
                name="intact_100yr",
                condition=ConditionType.INTACT,
                environment=env
            )
        ]

        if check_damaged:
            # Add damaged case for first line
            load_cases.append(
                DesignLoadCase(
                    name="damaged_ML1_100yr",
                    condition=ConditionType.DAMAGED,
                    environment=env,
                    damaged_line_id="ML1"
                )
            )

        # Analyze all load cases
        results = designer.analyze_all_conditions(load_cases)

        # Generate summary
        summary = designer.generate_design_summary(results)

        # Output results
        output_data = {
            'system': {
                'type': system_type,
                'water_depth': water_depth,
                'n_lines': n_lines,
                'material': material,
            },
            'environmental_loads': {
                'wave_drift': round(env_loads.wave_drift_force, 2),
                'current': round(env_loads.current_force, 2),
                'wind': round(env_loads.wind_force, 2),
                'total': round(env_loads.total_force, 2),
            },
            'summary': summary,
            'results': [
                {
                    'line_id': r.line_id,
                    'load_case': r.load_case,
                    'max_tension': round(r.max_tension, 2),
                    'safety_factor': round(r.safety_factor, 2),
                    'utilization': round(r.utilization, 3),
                    'status': 'PASS' if r.passes else 'FAIL'
                }
                for r in results
            ]
        }

        # Print summary
        click.echo("\n=== Mooring Design Verification ===\n")
        click.echo(f"System Type:           {system_type.upper()}")
        click.echo(f"Number of Lines:       {n_lines}")
        click.echo(f"Overall Status:        {summary['overall_status']}")
        click.echo(f"Min Safety Factor:     {summary['min_safety_factor']:.2f}")
        click.echo(f"Max Utilization:       {summary['max_utilization']:.3f}")
        click.echo(f"\nCritical Line:")
        click.echo(f"  Line ID:             {summary['critical_line']['line_id']}")
        click.echo(f"  Load Case:           {summary['critical_line']['load_case']}")
        click.echo(f"  Safety Factor:       {summary['critical_line']['safety_factor']:.2f}")

        # Save to file
        if output:
            with open(output, 'w') as f:
                json.dump(output_data, f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        import traceback
        traceback.print_exc()
        sys.exit(1)


@cli.command('generate-model')
@click.option('--config', type=click.Path(exists=True), help='YAML configuration file')
@click.option('--output', '-o', type=click.Path(), required=True, help='Output YAML file for OrcaFlex')
def generate_orcaflex_model(config: Optional[str], output: str):
    """Generate OrcaFlex model from configuration"""
    click.echo("OrcaFlex model generation from config file not yet implemented.")
    click.echo("Use Python API directly for custom model generation.")


@cli.command('list-materials')
def list_materials():
    """List available mooring line materials"""

    click.echo("\n=== Available Mooring Line Materials ===\n")
    click.echo(f"{'Material ID':<20} {'Type':<12} {'Diameter':<10} {'MBL':<10} {'Weight':<12} {'EA'}")
    click.echo(f"{'':<20} {'':<12} {'(mm)':<10} {'(kN)':<10} {'(kg/m)':<12} {'(kN)'}")
    click.echo("-" * 90)

    for mat_id, mat in MOORING_MATERIALS.items():
        click.echo(
            f"{mat_id:<20} {mat.line_type.value:<12} {mat.diameter:<10.1f} "
            f"{mat.mbl:<10.0f} {mat.weight_water:<12.1f} {mat.ea:<.0f}"
        )


def main():
    """Main entry point for CLI"""
    import numpy as np
    globals()['np'] = np
    cli()


if __name__ == '__main__':
    main()
