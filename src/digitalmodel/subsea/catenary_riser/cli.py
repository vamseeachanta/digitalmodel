#!/usr/bin/env python3
"""
ABOUTME: Command-line interface for catenary riser analysis providing simple catenary,
lazy wave analysis, and effective weight calculations.
"""

import click
import json
import sys

from . import (
    __version__,
    RiserConfiguration,
    BuoyancyModule,
    LazyWaveConfiguration,
    SimpleCatenaryAnalyzer,
    EffectiveWeightCalculator,
    LazyWaveAnalyzer,
    get_material,
    get_fluid,
)


@click.group()
@click.version_option(version=__version__, prog_name="catenary-riser")
def cli():
    """Catenary Riser Analysis - Simple and lazy wave catenary risers"""
    pass


@cli.command('simple')
@click.option('--diameter', type=float, required=True, help='Outer diameter (m)')
@click.option('--thickness', type=float, required=True, help='Wall thickness (m)')
@click.option('--length', type=float, required=True, help='Total riser length (m)')
@click.option('--water-depth', type=float, required=True, help='Water depth (m)')
@click.option('--offset', type=float, required=True, help='Horizontal offset (m)')
@click.option('--material', type=str, default='x65', help='Material (x65, x70)')
@click.option('--internal-fluid', type=str, default='oil', help='Internal fluid (oil, water, mud, air)')
@click.option('--top-tension', type=float, help='Top tension (N) - alternative to offset')
@click.option('--coating-thickness', type=float, help='Coating thickness (m)')
@click.option('--coating-density', type=float, help='Coating density (kg/m³)')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def simple_catenary(diameter, thickness, length, water_depth, offset, material,
                    internal_fluid, top_tension, coating_thickness, coating_density, output):
    """Analyze simple catenary riser configuration"""

    try:
        # Create riser configuration
        mat = get_material(material)
        fluid = get_fluid(internal_fluid)

        riser = RiserConfiguration(
            name="Riser1",
            outer_diameter=diameter,
            wall_thickness=thickness,
            length=length,
            material=mat,
            internal_fluid=fluid,
            water_depth=water_depth,
            horizontal_offset=offset,
            coating_thickness=coating_thickness,
            coating_density=coating_density,
        )

        # Analyze
        analyzer = SimpleCatenaryAnalyzer()
        if top_tension:
            result = analyzer.analyze_riser(riser, top_tension=top_tension)
        else:
            result = analyzer.analyze_riser(riser, water_depth=water_depth,
                                          horizontal_offset=offset)

        # Output
        output_data = {
            'configuration': {
                'diameter': diameter,
                'thickness': thickness,
                'length': length,
                'water_depth': water_depth,
                'material': material,
                'internal_fluid': internal_fluid,
            },
            'results': {
                'horizontal_tension_N': round(result.horizontal_tension, 2),
                'top_tension_N': round(result.top_tension, 2),
                'touchdown_tension_N': round(result.touchdown_tension, 2),
                'top_angle_deg': round(result.top_angle, 2),
                'arc_length_m': round(result.arc_length, 2),
                'grounded_length_m': round(result.grounded_length, 2),
                'effective_weight_N_per_m': round(result.effective_weight, 2),
                'catenary_parameter_m': round(result.catenary_parameter, 2),
            }
        }

        click.echo("\n=== Simple Catenary Riser Analysis ===\n")
        click.echo(f"Effective Weight:     {result.effective_weight:.2f} N/m")
        click.echo(f"Horizontal Tension:   {result.horizontal_tension/1000:.2f} kN")
        click.echo(f"Top Tension:          {result.top_tension/1000:.2f} kN")
        click.echo(f"Top Angle:            {result.top_angle:.2f}° from vertical")
        click.echo(f"Suspended Length:     {result.arc_length:.2f} m")
        click.echo(f"Grounded Length:      {result.grounded_length:.2f} m")

        if output:
            with open(output, 'w') as f:
                json.dump(output_data, f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('weight')
@click.option('--diameter', type=float, required=True, help='Outer diameter (m)')
@click.option('--thickness', type=float, required=True, help='Wall thickness (m)')
@click.option('--material', type=str, default='x65', help='Material (x65, x70)')
@click.option('--internal-fluid', type=str, default='oil', help='Internal fluid')
@click.option('--coating-thickness', type=float, help='Coating thickness (m)')
@click.option('--coating-density', type=float, default=700, help='Coating density (kg/m³)')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def effective_weight(diameter, thickness, material, internal_fluid,
                    coating_thickness, coating_density, output):
    """Calculate effective weight breakdown"""

    try:
        # Create riser
        mat = get_material(material)
        fluid = get_fluid(internal_fluid)

        riser = RiserConfiguration(
            name="Riser1",
            outer_diameter=diameter,
            wall_thickness=thickness,
            length=100.0,  # Not used for weight calc
            material=mat,
            internal_fluid=fluid,
            coating_thickness=coating_thickness,
            coating_density=coating_density,
        )

        # Calculate
        calc = EffectiveWeightCalculator()
        result = calc.calculate(riser)

        # Output
        click.echo("\n=== Effective Weight Breakdown ===\n")
        click.echo(f"Steel Weight:         {result.steel_weight:.2f} N/m")
        click.echo(f"Coating Weight:       {result.coating_weight:.2f} N/m")
        click.echo(f"Contents Weight:      {result.contents_weight:.2f} N/m")
        click.echo(f"Buoyancy:             {result.buoyancy:.2f} N/m")
        click.echo(f"---")
        click.echo(f"Total Dry Weight:     {result.total_dry_weight:.2f} N/m")
        click.echo(f"Effective Weight:     {result.effective_weight:.2f} N/m")

        if result.effective_weight < 0:
            click.echo("\n⚠️  WARNING: Negative effective weight - riser will float!")
        elif result.effective_weight < 50:
            click.echo("\n⚠️  WARNING: Very low effective weight - may need additional ballast")

        if output:
            with open(output, 'w') as f:
                json.dump(result.to_dict(), f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('lazy-wave')
@click.option('--diameter', type=float, required=True, help='Outer diameter (m)')
@click.option('--thickness', type=float, required=True, help='Wall thickness (m)')
@click.option('--length', type=float, required=True, help='Total riser length (m)')
@click.option('--water-depth', type=float, required=True, help='Water depth (m)')
@click.option('--offset', type=float, required=True, help='Horizontal offset (m)')
@click.option('--material', type=str, default='x65', help='Material')
@click.option('--internal-fluid', type=str, default='oil', help='Internal fluid')
@click.option('--buoy-length', type=float, default=200.0, help='Buoyancy module length (m)')
@click.option('--buoy-diameter', type=float, default=1.5, help='Buoyancy module diameter (m)')
@click.option('--buoy-start', type=float, default=300.0, help='Buoyancy start from bottom (m)')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def lazy_wave_analysis(diameter, thickness, length, water_depth, offset, material,
                      internal_fluid, buoy_length, buoy_diameter, buoy_start, output):
    """Analyze lazy wave riser with buoyancy modules"""

    try:
        # Create riser
        mat = get_material(material)
        fluid = get_fluid(internal_fluid)

        riser = RiserConfiguration(
            name="LazyWave1",
            outer_diameter=diameter,
            wall_thickness=thickness,
            length=length,
            material=mat,
            internal_fluid=fluid,
            water_depth=water_depth,
            horizontal_offset=offset,
        )

        # Create buoyancy module
        buoy_module = BuoyancyModule(
            name="BuoyModule1",
            length=buoy_length,
            outer_diameter=buoy_diameter,
            buoyancy_material_density=500,  # Syntactic foam
            start_length=buoy_start,
        )

        # Create configuration
        config = LazyWaveConfiguration(
            riser=riser,
            buoyancy_modules=[buoy_module],
        )

        # Analyze
        analyzer = LazyWaveAnalyzer()
        result = analyzer.analyze(config)

        # Output
        click.echo("\n=== Lazy Wave Riser Analysis ===\n")
        click.echo(f"Sag Bend Depth:       {result.sag_bend_depth:.2f} m")
        click.echo(f"Hog Bend Depth:       {result.hog_bend_depth:.2f} m")
        click.echo(f"Arch Height:          {result.arch_height:.2f} m")
        click.echo(f"\nTop Tension:          {result.top_tension/1000:.2f} kN")
        click.echo(f"Tension at Sag:       {result.tension_at_sag_bend/1000:.2f} kN (min)")
        click.echo(f"Tension at Hog:       {result.tension_at_hog_bend/1000:.2f} kN")
        click.echo(f"\nTop Angle:            {result.top_angle:.2f}°")
        click.echo(f"Buoyancy Utilization: {result.buoyancy_utilization*100:.1f}%")

        if output:
            output_data = {
                'sag_bend_depth_m': result.sag_bend_depth,
                'hog_bend_depth_m': result.hog_bend_depth,
                'arch_height_m': result.arch_height,
                'top_tension_N': result.top_tension,
                'tension_at_sag_N': result.tension_at_sag_bend,
                'tension_at_hog_N': result.tension_at_hog_bend,
            }
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
