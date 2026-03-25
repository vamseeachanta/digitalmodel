#!/usr/bin/env python3
"""
Riser Pipe Database Example
=============================

Demonstrates usage of the RiserClient for pipe specification and property calculation.

This example shows:
1. Getting pipe specifications from the database
2. Calculating properties with coatings
3. Generating OrcaFlex YAML
4. Finding pipes by pressure rating
"""

from digitalmodel.data_systems.data_procurement.riser import RiserClient


def main():
    """Run riser client example."""
    print("=" * 80)
    print("Riser Pipe Database Example")
    print("=" * 80)

    # Initialize client
    client = RiserClient()

    # Example 1: Get 10" SCH 80 X52 pipe
    print("\n1. Get 10\" SCH 80 X52 Pipe Specification")
    print("-" * 80)

    pipe = client.get_pipe_specification(diameter=10, schedule='SCH 80', grade='X52')

    print(f"Pipe: {pipe['nominal_diameter']}\" {pipe['schedule']} {pipe['grade']}")
    print(f"  OD: {pipe['outer_diameter_mm']:.1f} mm ({pipe['outer_diameter_in']:.3f} in)")
    print(f"  Wall: {pipe['wall_thickness_mm']:.1f} mm ({pipe['wall_thickness_in']:.3f} in)")
    print(f"  ID: {pipe['inner_diameter_mm']:.1f} mm ({pipe['inner_diameter_in']:.3f} in)")
    print(f"  Mass: {pipe['mass_per_meter_kg']:.2f} kg/m (bare pipe)")
    print(f"  Yield Strength: {pipe['yield_strength_mpa']:.0f} MPa")
    print(f"  Burst Pressure: {pipe['burst_pressure_mpa']:.1f} MPa")

    # Example 2: Calculate properties with coatings
    print("\n2. Calculate Properties with Coatings and Contents")
    print("-" * 80)

    props = client.calculate_properties(
        pipe=pipe,
        coatings=[
            {'type': '3LPE', 'thickness': 3.2, 'density': 940},  # Corrosion coating
            {'type': 'polypropylene', 'thickness': 50, 'density': 500}  # Insulation
        ],
        contents_density=850  # Oil
    )

    print(f"Mass Properties:")
    print(f"  Dry (with coatings): {props['mass_per_meter_dry_kg']:.2f} kg/m")
    print(f"  Wet (with contents): {props['mass_per_meter_wet_kg']:.2f} kg/m")
    print(f"  Submerged (in seawater): {props['mass_per_meter_submerged_kg']:.2f} kg/m")

    print(f"\nOuter Diameter:")
    print(f"  Bare pipe: {props['outer_diameter_mm']:.1f} mm")
    print(f"  With coatings: {props['outer_diameter_with_coatings_mm']:.1f} mm")

    print(f"\nStiffness Properties:")
    print(f"  EA (Axial): {props['axial_stiffness_ea_n']/1e9:.3f} GN")
    print(f"  EI (Bending): {props['bending_stiffness_ei_nm2']/1e6:.3f} MN·m²")
    print(f"  GJ (Torsional): {props['torsional_stiffness_gj_nm2']/1e6:.3f} MN·m²/rad")

    # Example 3: Generate OrcaFlex YAML
    print("\n3. Generate OrcaFlex LineType YAML")
    print("-" * 80)

    yaml_output = client.to_orcaflex_line_type(props, name="Production_Riser_SCR")

    print("OrcaFlex YAML generated:")
    print(yaml_output)

    # Example 4: Find pipes by pressure rating
    print("\n4. Find Pipes by Pressure Rating")
    print("-" * 80)

    # Find pipes for 10 MPa internal pressure with safety factor 1.5
    pressure_pipes = client.pipe_client.find_by_pressure_rating(
        internal_pressure=10.0,  # MPa
        diameter=10,
        safety_factor=1.5
    )

    print(f"Pipes suitable for 10 MPa internal pressure (SF=1.5):")
    for p in pressure_pipes[:5]:  # Show first 5
        print(f"  {p['schedule']:10s} {p['grade']:5s} - "
              f"Wall: {p['wall_thickness_mm']:5.1f} mm, "
              f"Burst: {p['burst_pressure_mpa']:5.1f} MPa")

    # Example 5: Compare different schedules
    print("\n5. Compare Different Schedules for 10\" X52 Pipe")
    print("-" * 80)

    schedules = ['SCH 40', 'SCH 60', 'SCH 80', 'SCH 100', 'STD', 'XS']
    print(f"{'Schedule':<10} {'Wall (mm)':<12} {'Mass (kg/m)':<12} {'EA (GN)':<12} {'EI (MN·m²)':<12}")
    print("-" * 80)

    for schedule in schedules:
        pipes = client.pipe_client.find_by_diameter(10, schedule=schedule, grade='X52')
        if pipes:
            p = pipes[0]
            ea_gn = p['axial_stiffness_ea_n'] / 1e9
            ei_mnm2 = p['bending_stiffness_ei_nm2'] / 1e6
            print(f"{schedule:<10} {p['wall_thickness_mm']:<12.1f} "
                  f"{p['mass_per_meter_kg']:<12.2f} {ea_gn:<12.3f} {ei_mnm2:<12.3f}")

    print("\n" + "=" * 80)
    print("Example completed successfully!")
    print("=" * 80)


if __name__ == '__main__':
    main()
