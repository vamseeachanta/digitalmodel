#!/usr/bin/env python3
"""
ABOUTME: Command-line interface for hydrodynamics analysis providing wave spectrum
generation, coefficient management, and OCIMF loading calculations.
"""

import click
import json
import sys
import numpy as np
import pandas as pd
from pathlib import Path

from . import __version__
from .models import (
    WaveParameters,
    WaveSpectrumType,
    VesselProperties,
    EnvironmentalConditions,
    get_vessel_type,
)
from .wave_spectra import WaveSpectra
from .coefficient_database import CoefficientDatabase
from .ocimf_loading import OCIMFLoading


@click.group()
@click.version_option(version=__version__, prog_name="hydrodynamics")
def cli():
    """Hydrodynamics Analysis Tools - Wave spectra, coefficients, and environmental loading"""
    pass


@cli.command('spectrum')
@click.option('--type', '-t', 'spectrum_type',
              type=click.Choice(['jonswap', 'pm', 'bretschneider', 'issc']),
              default='jonswap',
              help='Spectrum type')
@click.option('--hs', type=float, required=True, help='Significant wave height (m)')
@click.option('--tp', type=float, required=True, help='Peak period (s)')
@click.option('--gamma', type=float, default=3.3, help='JONSWAP gamma parameter')
@click.option('--freq-min', type=float, default=0.02, help='Minimum frequency (rad/s)')
@click.option('--freq-max', type=float, default=2.0, help='Maximum frequency (rad/s)')
@click.option('--n-points', type=int, default=100, help='Number of frequency points')
@click.option('--output', '-o', type=click.Path(), help='Output CSV file')
def spectrum_cmd(spectrum_type, hs, tp, gamma, freq_min, freq_max, n_points, output):
    """Generate wave spectrum"""

    try:
        # Map spectrum type
        type_map = {
            'jonswap': WaveSpectrumType.JONSWAP,
            'pm': WaveSpectrumType.PIERSON_MOSKOWITZ,
            'bretschneider': WaveSpectrumType.BRETSCHNEIDER,
            'issc': WaveSpectrumType.ISSC,
        }

        # Create wave parameters
        params = WaveParameters(
            spectrum_type=type_map[spectrum_type],
            significant_height=hs,
            peak_period=tp,
            gamma=gamma,
            freq_min=freq_min,
            freq_max=freq_max,
            n_frequencies=n_points
        )

        # Generate spectrum
        generator = WaveSpectra()
        frequencies, S = generator.generate_spectrum(params)

        # Calculate statistics
        stats = generator.spectrum_statistics(frequencies, S)

        # Output results
        click.echo("\n=== Wave Spectrum Analysis ===\n")
        click.echo(f"Spectrum Type:  {spectrum_type.upper()}")
        click.echo(f"Hs:             {stats['Hs_m']:.2f} m")
        click.echo(f"Tp:             {stats['Tp_s']:.2f} s")
        click.echo(f"Tz:             {stats['Tz_s']:.2f} s")
        click.echo(f"Peak Frequency: {stats['omega_p_rad_s']:.4f} rad/s")
        click.echo(f"Spectral Width: {stats['spectral_width']:.4f}")
        click.echo(f"\nMoments:")
        click.echo(f"  m0 = {stats['m0']:.4e}")
        click.echo(f"  m2 = {stats['m2']:.4e}")
        click.echo(f"  m4 = {stats['m4']:.4e}")

        if output:
            # Save to CSV
            df = pd.DataFrame({
                'frequency_rad_s': frequencies,
                'frequency_hz': frequencies / (2 * np.pi),
                'period_s': 2 * np.pi / frequencies,
                'spectral_density': S,
            })
            df.to_csv(output, index=False)
            click.echo(f"\nSpectrum saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('ocimf-wind')
@click.option('--vessel-type', type=click.Choice(['fpso', 'semisubmersible', 'tanker']),
              help='Standard vessel type')
@click.option('--length', type=float, help='Vessel length (m)')
@click.option('--beam', type=float, help='Vessel beam (m)')
@click.option('--draft', type=float, help='Vessel draft (m)')
@click.option('--displacement', type=float, help='Displacement (tonnes)')
@click.option('--wind-speed', type=float, required=True, help='Wind speed (m/s)')
@click.option('--wind-direction', type=float, default=0.0, help='Wind direction from bow (deg)')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def ocimf_wind_cmd(vessel_type, length, beam, draft, displacement, wind_speed,
                   wind_direction, output):
    """Calculate OCIMF wind loads on vessel"""

    try:
        # Get vessel properties
        if vessel_type:
            vessel = get_vessel_type(vessel_type)
        elif all([length, beam, draft, displacement]):
            vessel = VesselProperties(
                name="Custom Vessel",
                length_overall=length,
                beam=beam,
                draft=draft,
                displacement=displacement
            )
        else:
            click.echo("Error: Provide either --vessel-type or all vessel dimensions", err=True)
            sys.exit(1)

        # Calculate wind loads
        ocimf = OCIMFLoading()
        loads = ocimf.wind_load(vessel, wind_speed, wind_direction)

        # Output results
        click.echo("\n=== OCIMF Wind Loading ===\n")
        click.echo(f"Vessel:         {vessel.name}")
        click.echo(f"Dimensions:     L={vessel.length_overall:.1f}m, B={vessel.beam:.1f}m, T={vessel.draft:.1f}m")
        click.echo(f"Wind Speed:     {wind_speed:.1f} m/s")
        click.echo(f"Wind Direction: {wind_direction:.1f}°")
        click.echo(f"\nWind Loads:")
        click.echo(f"  Surge (Fx):   {loads['Fx_surge_N']/1e3:.1f} kN")
        click.echo(f"  Sway (Fy):    {loads['Fy_sway_N']/1e3:.1f} kN")
        click.echo(f"  Yaw (Mz):     {loads['Mz_yaw_Nm']/1e6:.1f} MN·m")

        if output:
            with open(output, 'w') as f:
                json.dump({
                    'vessel': vessel.to_dict(),
                    'loads': loads,
                }, f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('ocimf-current')
@click.option('--vessel-type', type=click.Choice(['fpso', 'semisubmersible', 'tanker']),
              help='Standard vessel type')
@click.option('--length', type=float, help='Vessel length (m)')
@click.option('--beam', type=float, help='Vessel beam (m)')
@click.option('--draft', type=float, help='Vessel draft (m)')
@click.option('--displacement', type=float, help='Displacement (tonnes)')
@click.option('--current-speed', type=float, required=True, help='Current speed (m/s)')
@click.option('--current-direction', type=float, default=0.0, help='Current direction from bow (deg)')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def ocimf_current_cmd(vessel_type, length, beam, draft, displacement, current_speed,
                      current_direction, output):
    """Calculate OCIMF current loads on vessel"""

    try:
        # Get vessel properties
        if vessel_type:
            vessel = get_vessel_type(vessel_type)
        elif all([length, beam, draft, displacement]):
            vessel = VesselProperties(
                name="Custom Vessel",
                length_overall=length,
                beam=beam,
                draft=draft,
                displacement=displacement
            )
        else:
            click.echo("Error: Provide either --vessel-type or all vessel dimensions", err=True)
            sys.exit(1)

        # Calculate current loads
        ocimf = OCIMFLoading()
        loads = ocimf.current_load(vessel, current_speed, current_direction)

        # Output results
        click.echo("\n=== OCIMF Current Loading ===\n")
        click.echo(f"Vessel:            {vessel.name}")
        click.echo(f"Dimensions:        L={vessel.length_overall:.1f}m, B={vessel.beam:.1f}m, T={vessel.draft:.1f}m")
        click.echo(f"Current Speed:     {current_speed:.2f} m/s")
        click.echo(f"Current Direction: {current_direction:.1f}°")
        click.echo(f"\nCurrent Loads:")
        click.echo(f"  Surge (Fx):      {loads['Fx_surge_N']/1e3:.1f} kN")
        click.echo(f"  Sway (Fy):       {loads['Fy_sway_N']/1e3:.1f} kN")
        click.echo(f"  Yaw (Mz):        {loads['Mz_yaw_Nm']/1e6:.1f} MN·m")

        if output:
            with open(output, 'w') as f:
                json.dump({
                    'vessel': vessel.to_dict(),
                    'loads': loads,
                }, f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command('combined-env')
@click.option('--vessel-type', type=click.Choice(['fpso', 'semisubmersible', 'tanker']),
              required=True, help='Standard vessel type')
@click.option('--wind-speed', type=float, default=0.0, help='Wind speed (m/s)')
@click.option('--wind-direction', type=float, default=0.0, help='Wind direction from bow (deg)')
@click.option('--current-speed', type=float, default=0.0, help='Current speed (m/s)')
@click.option('--current-direction', type=float, default=0.0, help='Current direction from bow (deg)')
@click.option('--output', '-o', type=click.Path(), help='Output JSON file')
def combined_env_cmd(vessel_type, wind_speed, wind_direction, current_speed,
                     current_direction, output):
    """Calculate combined wind and current loads"""

    try:
        # Get vessel
        vessel = get_vessel_type(vessel_type)

        # Create environmental conditions
        env = EnvironmentalConditions(
            wind_speed=wind_speed,
            wind_direction=wind_direction,
            current_speed=current_speed,
            current_direction=current_direction
        )

        # Calculate combined loads
        ocimf = OCIMFLoading()
        result = ocimf.combined_environmental_load(vessel, env)

        # Output results
        click.echo("\n=== Combined Environmental Loading ===\n")
        click.echo(f"Vessel: {vessel.name}")
        click.echo(f"\nEnvironment:")
        click.echo(f"  Wind:    {wind_speed:.1f} m/s @ {wind_direction:.1f}°")
        click.echo(f"  Current: {current_speed:.2f} m/s @ {current_direction:.1f}°")

        wind_loads = result['wind_loads']
        current_loads = result['current_loads']
        combined = result['combined_loads']

        click.echo(f"\nWind Loads:")
        click.echo(f"  Fx = {wind_loads['Fx_surge_N']/1e3:.1f} kN")
        click.echo(f"  Fy = {wind_loads['Fy_sway_N']/1e3:.1f} kN")
        click.echo(f"  Mz = {wind_loads['Mz_yaw_Nm']/1e6:.2f} MN·m")

        click.echo(f"\nCurrent Loads:")
        click.echo(f"  Fx = {current_loads['Fx_surge_N']/1e3:.1f} kN")
        click.echo(f"  Fy = {current_loads['Fy_sway_N']/1e3:.1f} kN")
        click.echo(f"  Mz = {current_loads['Mz_yaw_Nm']/1e6:.2f} MN·m")

        click.echo(f"\nCombined Loads:")
        click.echo(f"  Fx = {combined['Fx_surge_N']/1e3:.1f} kN")
        click.echo(f"  Fy = {combined['Fy_sway_N']/1e3:.1f} kN")
        click.echo(f"  Mz = {combined['Mz_yaw_Nm']/1e6:.2f} MN·m")
        click.echo(f"  Resultant = {combined['F_resultant_N']/1e3:.1f} kN @ {combined['resultant_direction_deg']:.1f}°")

        if output:
            with open(output, 'w') as f:
                json.dump({
                    'vessel': vessel.to_dict(),
                    'environment': env.to_dict(),
                    'results': result,
                }, f, indent=2)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


def main():
    """Main entry point"""
    cli()


if __name__ == '__main__':
    main()
