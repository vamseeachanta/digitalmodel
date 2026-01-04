#!/usr/bin/env python3
"""
Structural Analysis CLI

Command-line interface for structural analysis including stress calculations,
buckling checks, and capacity verification per DNV, API, and Eurocode 3.
"""

import click
import sys
import json
from pathlib import Path
from typing import Optional

from .models import (
    StressState, MaterialProperties, PlateGeometry,
    STEEL_S275, STEEL_S355, STEEL_S420
)
from .stress_calculator import StressCalculator
from .buckling import PlateBucklingAnalyzer, ColumnBucklingAnalyzer
from .capacity import MemberCapacityChecker


# Material lookup
MATERIALS = {
    'S275': STEEL_S275,
    'S355': STEEL_S355,
    'S420': STEEL_S420,
}


@click.group()
@click.version_option(version="1.0.0", prog_name="structural-analysis")
def cli():
    """Structural Analysis Tools - CLI for stress, buckling, and capacity checks"""
    pass


@cli.command('stress')
@click.option('--sigma-x', type=float, default=0.0, help='Normal stress in x (MPa)')
@click.option('--sigma-y', type=float, default=0.0, help='Normal stress in y (MPa)')
@click.option('--sigma-z', type=float, default=0.0, help='Normal stress in z (MPa)')
@click.option('--tau-xy', type=float, default=0.0, help='Shear stress xy (MPa)')
@click.option('--tau-xz', type=float, default=0.0, help='Shear stress xz (MPa)')
@click.option('--tau-yz', type=float, default=0.0, help='Shear stress yz (MPa)')
@click.option('--material', type=click.Choice(['S275', 'S355', 'S420']), default='S355', help='Material grade')
@click.option('--output', '-o', type=click.Path(), help='Output file (JSON)')
def calculate_stress(sigma_x, sigma_y, sigma_z, tau_xy, tau_xz, tau_yz, material, output):
    """Calculate Von Mises stress and principal stresses"""

    stress = StressState(sigma_x, sigma_y, sigma_z, tau_xy, tau_xz, tau_yz)
    mat = MATERIALS[material]

    von_mises = stress.von_mises()
    principals = stress.principal_stresses()
    max_shear = stress.max_shear()

    # Results
    results = {
        'stress_state': {
            'sigma_x': sigma_x,
            'sigma_y': sigma_y,
            'sigma_z': sigma_z,
            'tau_xy': tau_xy,
            'tau_xz': tau_xz,
            'tau_yz': tau_yz
        },
        'von_mises_stress': round(von_mises, 2),
        'principal_stresses': {
            'sigma_1': round(principals[0], 2),
            'sigma_2': round(principals[1], 2),
            'sigma_3': round(principals[2], 2)
        },
        'max_shear_stress': round(max_shear, 2),
        'material': material,
        'yield_strength': mat.yield_strength,
        'safety_factor': round(mat.yield_strength / von_mises, 2) if von_mises > 0 else float('inf'),
        'status': 'PASS' if von_mises <= mat.yield_strength else 'FAIL'
    }

    # Output
    click.echo("\n" + "=" * 70)
    click.echo("Stress Analysis Results")
    click.echo("=" * 70)
    click.echo(f"Von Mises Stress:     {von_mises:.2f} MPa")
    click.echo(f"Principal Stresses:   σ1={principals[0]:.2f}, σ2={principals[1]:.2f}, σ3={principals[2]:.2f} MPa")
    click.echo(f"Max Shear Stress:     {max_shear:.2f} MPa")
    click.echo(f"Material:             {material} (fy = {mat.yield_strength} MPa)")
    click.echo(f"Safety Factor:        {results['safety_factor']:.2f}")
    click.echo(f"Status:               {results['status']}")
    click.echo("=" * 70 + "\n")

    if output:
        with open(output, 'w') as f:
            json.dump(results, f, indent=2)
        click.echo(f"Results saved to: {output}\n")


@cli.command('buckling-plate')
@click.option('--length', type=float, required=True, help='Plate length a (mm)')
@click.option('--width', type=float, required=True, help='Plate width b (mm)')
@click.option('--thickness', type=float, required=True, help='Plate thickness t (mm)')
@click.option('--sigma-x', type=float, required=True, help='Compressive stress (MPa)')
@click.option('--sigma-y', type=float, default=0.0, help='Transverse stress (MPa)')
@click.option('--tau', type=float, default=0.0, help='Shear stress (MPa)')
@click.option('--material', type=click.Choice(['S275', 'S355', 'S420']), default='S355')
@click.option('--gamma-m', type=float, default=1.15, help='Material safety factor')
@click.option('--output', '-o', type=click.Path(), help='Output file (JSON)')
def check_plate_buckling(length, width, thickness, sigma_x, sigma_y, tau, material, gamma_m, output):
    """Check plate buckling per DNV-RP-C201"""

    mat = MATERIALS[material]
    analyzer = PlateBucklingAnalyzer(mat)
    plate = PlateGeometry(length, width, thickness)

    result = analyzer.check_plate_buckling(plate, sigma_x, sigma_y, tau, gamma_m)

    # Results dict
    results = {
        'plate_geometry': {
            'length': length,
            'width': width,
            'thickness': thickness
        },
        'applied_loading': {
            'sigma_x': sigma_x,
            'sigma_y': sigma_y,
            'tau': tau
        },
        'critical_stress': round(result.critical_stress, 2),
        'utilization': round(result.utilization, 3),
        'safety_factor': round(result.safety_factor, 2),
        'material': material,
        'gamma_m': gamma_m,
        'status': 'PASS' if result.passes else 'FAIL'
    }

    # Output
    click.echo("\n" + "=" * 70)
    click.echo("Plate Buckling Check (DNV-RP-C201)")
    click.echo("=" * 70)
    click.echo(f"Plate: {length}mm x {width}mm x {thickness}mm")
    click.echo(f"Applied Stress:       σx={sigma_x:.1f} MPa, τ={tau:.1f} MPa")
    click.echo(f"Critical Stress:      {result.critical_stress:.2f} MPa")
    click.echo(f"Utilization:          {result.utilization:.3f} ({result.utilization*100:.1f}%)")
    click.echo(f"Safety Factor:        {result.safety_factor:.2f}")
    click.echo(f"Status:               {results['status']}")
    click.echo("=" * 70 + "\n")

    if output:
        with open(output, 'w') as f:
            json.dump(results, f, indent=2)
        click.echo(f"Results saved to: {output}\n")


@cli.command('buckling-column')
@click.option('--axial-force', type=float, required=True, help='Axial force (N)')
@click.option('--area', type=float, required=True, help='Cross-sectional area (mm²)')
@click.option('--I-min', type=float, required=True, help='Minimum moment of inertia (mm⁴)')
@click.option('--L-eff', type=float, required=True, help='Effective length (mm)')
@click.option('--curve', type=click.Choice(['a0', 'a', 'b', 'c', 'd']), default='b', help='EC3 buckling curve')
@click.option('--material', type=click.Choice(['S275', 'S355', 'S420']), default='S355')
@click.option('--gamma-m', type=float, default=1.0, help='Material safety factor')
@click.option('--output', '-o', type=click.Path(), help='Output file (JSON)')
def check_column_buckling(axial_force, area, i_min, l_eff, curve, material, gamma_m, output):
    """Check column buckling per Eurocode 3"""

    mat = MATERIALS[material]
    analyzer = ColumnBucklingAnalyzer(mat)

    result = analyzer.check_column_buckling(
        axial_force, area, i_min, l_eff, curve, gamma_m
    )

    # Results dict
    results = {
        'column_properties': {
            'area': area,
            'I_min': i_min,
            'L_eff': l_eff,
            'buckling_curve': curve
        },
        'axial_force': axial_force,
        'critical_stress': round(result.critical_stress, 2),
        'applied_stress': round(result.applied_stress, 2),
        'utilization': round(result.utilization, 3),
        'safety_factor': round(result.safety_factor, 2),
        'material': material,
        'status': 'PASS' if result.passes else 'FAIL'
    }

    # Output
    click.echo("\n" + "=" * 70)
    click.echo("Column Buckling Check (Eurocode 3)")
    click.echo("=" * 70)
    click.echo(f"Axial Force:          {axial_force/1e6:.2f} MN")
    click.echo(f"Area:                 {area:.0f} mm²")
    click.echo(f"Applied Stress:       {result.applied_stress:.2f} MPa")
    click.echo(f"Critical Stress:      {result.critical_stress:.2f} MPa")
    click.echo(f"Utilization:          {result.utilization:.3f} ({result.utilization*100:.1f}%)")
    click.echo(f"Safety Factor:        {result.safety_factor:.2f}")
    click.echo(f"Buckling Curve:       {curve}")
    click.echo(f"Status:               {results['status']}")
    click.echo("=" * 70 + "\n")

    if output:
        with open(output, 'w') as f:
            json.dump(results, f, indent=2)
        click.echo(f"Results saved to: {output}\n")


@cli.command('capacity')
@click.option('--tension', is_flag=True, help='Check tension capacity')
@click.option('--axial-force', type=float, required=True, help='Axial force (N)')
@click.option('--area-gross', type=float, required=True, help='Gross area (mm²)')
@click.option('--area-net', type=float, help='Net area at connections (mm²)')
@click.option('--material', type=click.Choice(['S275', 'S355', 'S420']), default='S355')
@click.option('--output', '-o', type=click.Path(), help='Output file (JSON)')
def check_capacity(tension, axial_force, area_gross, area_net, material, output):
    """Check member capacity"""

    mat = MATERIALS[material]
    checker = MemberCapacityChecker(mat)

    if tension:
        if not area_net:
            area_net = area_gross  # Assume no holes if not specified

        result = checker.check_tension_member(axial_force, area_gross, area_net)

        results = {
            'check_type': 'tension',
            'axial_force': axial_force,
            'area_gross': area_gross,
            'area_net': area_net,
            'capacity': round(result.capacity, 2),
            'utilization': round(result.utilization, 3),
            'governing_mode': result.governing_mode,
            'material': material,
            'status': 'PASS' if result.passes else 'FAIL'
        }

        # Output
        click.echo("\n" + "=" * 70)
        click.echo("Tension Capacity Check")
        click.echo("=" * 70)
        click.echo(f"Tension Force:        {axial_force/1e6:.2f} MN")
        click.echo(f"Design Capacity:      {result.capacity/1e6:.2f} MN")
        click.echo(f"Utilization:          {result.utilization:.3f} ({result.utilization*100:.1f}%)")
        click.echo(f"Governing Mode:       {result.governing_mode}")
        click.echo(f"Status:               {results['status']}")
        click.echo("=" * 70 + "\n")

    else:
        click.echo("Error: Specify --tension for tension capacity check")
        click.echo("Combined loading checks require additional parameters (use Python API)")
        sys.exit(1)

    if output:
        with open(output, 'w') as f:
            json.dump(results, f, indent=2)
        click.echo(f"Results saved to: {output}\n")


@cli.command('list-materials')
def list_materials():
    """List available material grades"""
    click.echo("\nAvailable Material Grades:")
    click.echo("=" * 70)
    for name, mat in MATERIALS.items():
        click.echo(f"\n{name}:")
        click.echo(f"  Yield Strength:      {mat.yield_strength} MPa")
        click.echo(f"  Ultimate Strength:   {mat.ultimate_strength} MPa")
        click.echo(f"  Young's Modulus:     {mat.youngs_modulus} MPa")
        click.echo(f"  Poisson's Ratio:     {mat.poissons_ratio}")
    click.echo("\n" + "=" * 70 + "\n")


def main():
    """Main CLI entry point"""
    cli()


if __name__ == '__main__':
    main()
