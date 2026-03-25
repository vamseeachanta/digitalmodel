#!/usr/bin/env python3
"""
ABOUTME: Command-line interface for VIV analysis providing natural frequency
calculation, vortex shedding analysis, and VIV susceptibility screening.
"""

import click
import json
import sys

from . import (
    __version__,
    TubularMember,
    MaterialProperties,
    FluidProperties,
    BoundaryCondition,
    FrequencyCalculator,
    VortexSheddingAnalyzer,
    VIVScreening,
    VIVFatigueCalculator,
    get_material,
    STROUHAL_NUMBERS,
)


@click.group()
@click.version_option(version=__version__, prog_name="viv-analysis")
def cli():
    """VIV Analysis Tools - Natural frequency, vortex shedding, and screening"""
    pass


@cli.command('natural-freq')
@click.option('--length', type=float, required=True, help='Member length (m)')
@click.option('--diameter', type=float, required=True, help='Outer diameter (m)')
@click.option('--thickness', type=float, required=True, help='Wall thickness (m)')
@click.option('--material', type=str, default='steel', help='Material (steel, titanium)')
@click.option('--boundary', type=str, default='pinned-pinned', help='Boundary condition')
@click.option('--n-modes', type=int, default=5, help='Number of modes')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def natural_frequency(length, diameter, thickness, material, boundary, n_modes, output):
    """Calculate natural frequencies"""

    try:
        # Create member
        mat = get_material(material)
        bc = BoundaryCondition(boundary)

        member = TubularMember(
            name="Member1",
            length=length,
            outer_diameter=diameter,
            wall_thickness=thickness,
            material=mat,
            boundary_condition=bc
        )

        # Calculate frequencies
        calc = FrequencyCalculator()
        results = calc.calculate_multiple_modes(member, n_modes=n_modes)

        # Output
        output_data = {
            'member': {
                'length': length,
                'diameter': diameter,
                'thickness': thickness,
                'material': material,
                'boundary': boundary
            },
            'frequencies': [
                {
                    'mode': r.mode_number,
                    'frequency_hz': round(r.frequency, 4),
                    'period_s': round(r.period, 4)
                }
                for r in results
            ]
        }

        click.echo("\n=== Natural Frequencies ===\n")
        for r in results:
            click.echo(f"Mode {r.mode_number}: {r.frequency:.4f} Hz (T = {r.period:.3f} s)")

        if output:
            with open(output, 'w') as f:
                json.dump(output_data, f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('screening')
@click.option('--length', type=float, required=True, help='Member length (m)')
@click.option('--diameter', type=float, required=True, help='Outer diameter (m)')
@click.option('--thickness', type=float, required=True, help='Wall thickness (m)')
@click.option('--current', type=float, required=True, help='Current velocity (m/s)')
@click.option('--material', type=str, default='steel', help='Material')
@click.option('--boundary', type=str, default='pinned-pinned', help='Boundary condition')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def screening_cmd(length, diameter, thickness, current, material, boundary, output):
    """VIV susceptibility screening"""

    try:
        # Create member
        mat = get_material(material)
        bc = BoundaryCondition(boundary)

        member = TubularMember(
            name="Member1",
            length=length,
            outer_diameter=diameter,
            wall_thickness=thickness,
            material=mat,
            boundary_condition=bc
        )

        # Screen
        screening = VIVScreening()
        result = screening.screen_member(member, current, mode=1)

        # Output
        output_data = {
            'natural_frequency_hz': round(result.natural_frequency, 4),
            'shedding_frequency_hz': round(result.shedding_frequency, 4),
            'reduced_velocity': round(result.reduced_velocity, 2),
            'is_susceptible': result.is_susceptible,
            'lock_in_status': result.lock_in_status,
            'safety_factor': round(result.safety_factor, 2),
            'recommendation': result.recommendation
        }

        click.echo("\n=== VIV Screening Results ===\n")
        click.echo(f"Natural Frequency:    {result.natural_frequency:.4f} Hz")
        click.echo(f"Shedding Frequency:   {result.shedding_frequency:.4f} Hz")
        click.echo(f"Reduced Velocity:     {result.reduced_velocity:.2f}")
        click.echo(f"Lock-in Status:       {result.lock_in_status.upper()}")
        click.echo(f"Safety Factor:        {result.safety_factor:.2f}")
        click.echo(f"\nRecommendation: {result.recommendation}")

        if output:
            with open(output, 'w') as f:
                json.dump(output_data, f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


def main():
    """Main entry point"""
    cli()


if __name__ == '__main__':
    main()
