# ABOUTME: Complete OrcaFlex workflow integrating metocean, vessel, and mooring data
# ABOUTME: End-to-end example from data procurement to OrcaFlex model generation

"""
Complete OrcaFlex Workflow Example
===================================

Demonstrates complete OrcaFlex model generation workflow:
1. Metocean data procurement (waves, wind, current from ERA5/NOAA)
2. Vessel data procurement (generic VLCC with RAOs and hydrostatics)
3. Mooring system design (8-point catenary mooring)
4. Combined OrcaFlex YAML output (in-memory)

This example shows the ZERO STORAGE architecture:
- All data streamed from APIs or generated on-demand
- OrcaFlex YAML generated in-memory
- No intermediate files saved
- Direct consumption by OrcaFlex API

Critical: This is the complete workflow that the entire data procurement
framework was designed to support!
"""

import sys
from pathlib import Path
from datetime import datetime, timedelta
import numpy as np

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.data_procurement import MetoceanClient, VesselClient, MooringClient


def example_fpso_mooring_design():
    """
    Example 1: Complete FPSO mooring design with metocean data.

    Demonstrates:
    - Metocean data for design conditions
    - VLCC vessel with RAOs
    - 8-point catenary mooring design
    - Complete OrcaFlex model (in-memory)
    """
    print("\n" + "=" * 70)
    print("Example 1: FPSO Mooring System Design")
    print("=" * 70 + "\n")

    # Project parameters
    project = {
        'name': 'FPSO Mooring System',
        'location': {
            'latitude': 25.0,
            'longitude': -90.0,
            'water_depth': 1500  # m
        },
        'vessel': {
            'type': 'VLCC',
            'draft': 20.0  # m
        },
        'mooring': {
            'num_lines': 8,
            'design_load_per_line': 2000,  # kN
            'soil_type': 'clay'
        }
    }

    print(f"Project: {project['name']}")
    print(f"Location: {project['location']['latitude']}°N, {project['location']['longitude']}°W")
    print(f"Water depth: {project['location']['water_depth']} m")
    print(f"Vessel: {project['vessel']['type']}")
    print(f"Mooring: {project['mooring']['num_lines']}-point, {project['mooring']['design_load_per_line']} kN/line\n")

    # Step 1: Get metocean design conditions
    print("Step 1: Retrieving metocean design conditions...")

    metocean_client = MetoceanClient.from_config(
        Path(__file__).parent.parent / 'specs/modules/data-procurement/metocean-data/configs/example_config.yml'
    )

    # Query 20 years of wave data for extreme value analysis
    start_date = datetime(2000, 1, 1)
    end_date = datetime(2020, 12, 31)

    print(f"  Querying ERA5: {start_date.date()} to {end_date.date()}")

    # Stream wave data and calculate extremes
    wave_heights = []
    wave_periods = []
    wave_directions = []

    print("  Processing wave data (streaming, zero storage)...")

    count = 0
    for record in metocean_client.query_metocean(
        start_date=start_date,
        end_date=end_date,
        location=project['location'],
        parameters=['significant_wave_height', 'peak_period', 'wave_direction']
    ):
        wave_heights.append(record.get('significant_wave_height', 0))
        wave_periods.append(record.get('peak_period', 0))
        wave_directions.append(record.get('wave_direction', 0))

        count += 1
        if count >= 1000:  # Limit for example
            break

    # Calculate design conditions (100-year return period - simplified)
    design_hs = np.percentile(wave_heights, 99.9) if wave_heights else 8.0
    design_tp = np.percentile(wave_periods, 99.9) if wave_periods else 12.0
    predominant_direction = np.median(wave_directions) if wave_directions else 0.0

    print(f"  Design conditions:")
    print(f"    Hs (100-yr): {design_hs:.1f} m")
    print(f"    Tp (100-yr): {design_tp:.1f} s")
    print(f"    Predominant direction: {predominant_direction:.0f}°")
    print(f"  (Based on {count} records, processed in-memory)")

    # Step 2: Get vessel data with RAOs
    print("\nStep 2: Retrieving vessel data and RAOs...")

    vessel_client = VesselClient.from_config(
        Path(__file__).parent.parent / 'specs/modules/data-procurement/vessel-systems/configs/example_config.yml'
    )

    # Use generic VLCC
    vessel_type = project['vessel']['type']
    draft = project['vessel']['draft']

    print(f"  Vessel type: {vessel_type}, Draft: {draft} m")

    # Get vessel particulars
    from digitalmodel.data_procurement.vessel.api_clients import GenericRAOClient
    rao_client = GenericRAOClient()
    vessel_particulars = rao_client.get_vessel_particulars(vessel_type)

    vessel = {
        'name': f'Generic {vessel_type}',
        'length': vessel_particulars['loa'],
        'beam': vessel_particulars['beam'],
        'depth': vessel_particulars['depth'],
        'draft': draft,
        'dwt': vessel_particulars['dwt']
    }

    print(f"  Vessel dimensions: {vessel['length']}m × {vessel['beam']}m × {vessel['draft']}m")

    # Get RAOs
    print("  Retrieving RAOs...")
    raos = vessel_client.get_vessel_raos(vessel_type=vessel_type, draft=draft)

    print(f"    RAO data: 6 DOF, {len(raos['wave_directions'])} directions, {len(raos['frequencies'])} frequencies")

    # Calculate hydrostatics
    print("  Calculating hydrostatic properties...")
    hydro = vessel_client.calculate_hydrostatics(vessel, draft=draft)

    print(f"    Displacement: {hydro['displacement']:,.0f} tonnes")
    print(f"    GM: {hydro['gm_metacentric_height']:.2f} m")

    # Calculate natural periods
    periods = vessel_client.calculate_natural_periods(vessel, hydro)

    print(f"    Natural periods: Roll={periods['roll']:.1f}s, Pitch={periods['pitch']:.1f}s, Heave={periods['heave']:.1f}s")

    # Step 3: Design mooring system
    print("\nStep 3: Designing mooring system...")

    mooring_client = MooringClient()

    # Design single mooring line
    print(f"  Design load per line: {project['mooring']['design_load_per_line']} kN")

    mooring_line = mooring_client.design_mooring_line(
        design_load=project['mooring']['design_load_per_line'],
        water_depth=project['location']['water_depth'],
        mooring_type='catenary',
        soil_type=project['mooring']['soil_type']
    )

    print(f"  Mooring type: {mooring_line['mooring_type']}")
    print(f"  Total line length: {mooring_line['total_length']:.1f} m")

    print("  Segments:")
    for i, segment in enumerate(mooring_line['segments']):
        component = segment['component']

        if segment['type'] == 'chain':
            print(f"    {i+1}. {segment['name']}: {segment['length']:.0f}m")
            print(f"       Chain {component['grade']} {component['diameter']}mm, MBL={component['minimum_breaking_load']}kN")

        elif segment['type'] == 'wire_rope':
            print(f"    {i+1}. {segment['name']}: {segment['length']:.0f}m")
            print(f"       Wire rope {component['construction']} {component['diameter']}mm, MBL={component['minimum_breaking_load']}kN")

    anchor = mooring_line['anchor']
    print(f"  Anchor: {anchor['type']}, {anchor['dry_weight']}kg, capacity={anchor['design_capacity_kn']}kN")

    # Validation
    validation = mooring_line['validation']
    print(f"  Validation: {'✓ Valid' if validation['valid'] else '✗ Issues found'} (API RP 2SK)")

    if validation['warnings']:
        print(f"  Warnings:")
        for warning in validation['warnings']:
            print(f"    - {warning}")

    # Step 4: Generate complete OrcaFlex model
    print("\nStep 4: Generating OrcaFlex model (in-memory)...")

    # Vessel YAML
    print("  Generating vessel definition...")
    vessel_yaml = vessel_client.to_orcaflex_vessel(vessel, raos, hydro, draft=draft)

    # Mooring YAML (for all 8 lines)
    print(f"  Generating {project['mooring']['num_lines']} mooring lines...")

    all_mooring_yaml = []
    for line_num in range(1, project['mooring']['num_lines'] + 1):
        # Calculate line heading (evenly distributed)
        heading = (line_num - 1) * 360 / project['mooring']['num_lines']

        mooring_yaml = mooring_client.to_orcaflex_mooring(
            mooring_line,
            name=f"MooringLine{line_num}"
        )
        all_mooring_yaml.append(mooring_yaml)

        print(f"    Line {line_num}: Heading {heading:.0f}°")

    # Combine all YAML
    complete_model = "# Complete OrcaFlex Model\n"
    complete_model += "# Generated by digitalmodel data procurement framework\n"
    complete_model += f"# Project: {project['name']}\n"
    complete_model += f"# Generated: {datetime.now()}\n\n"

    complete_model += "# ===== VESSEL =====\n\n"
    complete_model += vessel_yaml

    complete_model += "\n\n# ===== MOORING LINES =====\n\n"
    for i, mooring_yaml in enumerate(all_mooring_yaml):
        complete_model += f"# --- Mooring Line {i+1} ---\n\n"
        complete_model += mooring_yaml
        complete_model += "\n"

    # Show statistics
    lines_count = len(complete_model.split('\n'))
    print(f"\n  OrcaFlex model generated:")
    print(f"    Total lines: {lines_count}")
    print(f"    Components: 1 vessel + {project['mooring']['num_lines']} mooring lines")
    print(f"    Size: {len(complete_model) / 1024:.1f} KB (in-memory)")

    # Show sample (first 50 lines)
    print(f"\n  Sample YAML (first 50 lines):")
    print("  " + "-" * 66)
    for line in complete_model.split('\n')[:50]:
        print(f"  {line}")
    print("  " + "-" * 66)
    print("  ... (truncated)")

    print("\n  ✓ Complete OrcaFlex model ready for direct consumption (zero storage)")

    return {
        'project': project,
        'metocean': {
            'design_hs': design_hs,
            'design_tp': design_tp,
            'direction': predominant_direction
        },
        'vessel': vessel,
        'mooring': mooring_line,
        'orcaflex_yaml': complete_model
    }


def example_floating_wind_turbine():
    """
    Example 2: Floating wind turbine mooring design.

    Demonstrates:
    - Floating wind specific requirements
    - Semi-taut mooring with polyester rope
    - Moderate water depth design
    """
    print("\n" + "=" * 70)
    print("Example 2: Floating Wind Turbine Mooring")
    print("=" * 70 + "\n")

    # Project parameters
    project = {
        'name': '15MW Floating Wind Turbine',
        'location': {
            'latitude': 57.0,  # North Sea
            'longitude': 2.0,
            'water_depth': 120  # m
        },
        'mooring': {
            'num_lines': 3,  # Typical for floating wind
            'design_load_per_line': 2500,  # kN
            'soil_type': 'sand'
        }
    }

    print(f"Project: {project['name']}")
    print(f"Location: North Sea ({project['location']['latitude']}°N, {project['location']['longitude']}°E)")
    print(f"Water depth: {project['location']['water_depth']} m")
    print(f"Mooring: {project['mooring']['num_lines']}-point, {project['mooring']['design_load_per_line']} kN/line\n")

    # Design mooring
    mooring_client = MooringClient()

    print("Designing mooring system...")

    mooring_line = mooring_client.design_mooring_line(
        design_load=project['mooring']['design_load_per_line'],
        water_depth=project['location']['water_depth'],
        mooring_type='semi_taut',  # Preferred for floating wind
        soil_type=project['mooring']['soil_type']
    )

    print(f"  Mooring type: {mooring_line['mooring_type']}")
    print(f"  Total line length: {mooring_line['total_length']:.1f} m")

    print("  Segments:")
    for i, segment in enumerate(mooring_line['segments']):
        component = segment['component']

        if segment['type'] == 'chain':
            print(f"    {i+1}. {segment['name']}: {segment['length']:.0f}m")
            print(f"       Chain {component['grade']} {component['diameter']}mm")

        elif segment['type'] == 'synthetic_rope':
            print(f"    {i+1}. {segment['name']}: {segment['length']:.0f}m")
            print(f"       {component['fiber_type'].capitalize()} rope {component['diameter']}mm")
            print(f"       Elongation@50%: {component['elongation_at_50_mbl']}%")

    anchor = mooring_line['anchor']
    print(f"  Anchor: {anchor['type']}, {anchor['dry_weight']}kg")

    # Generate OrcaFlex YAML
    print("\nGenerating OrcaFlex mooring YAML...")
    orcaflex_yaml = mooring_client.to_orcaflex_mooring(mooring_line, name="FloatingWind_Mooring")

    lines_count = len(orcaflex_yaml.split('\n'))
    print(f"  Generated {lines_count} lines of YAML (in-memory)")

    print("\n  ✓ Floating wind mooring design complete")


def example_tlp_deepwater():
    """
    Example 3: TLP (Tension-Leg Platform) deepwater mooring.

    Demonstrates:
    - Ultra-deepwater design (2000m)
    - HMPE taut-leg mooring (floats)
    - High-capacity suction pile anchors
    """
    print("\n" + "=" * 70)
    print("Example 3: TLP Deepwater Mooring")
    print("=" * 70 + "\n")

    # Project parameters
    project = {
        'name': 'TLP Tension-Leg Platform',
        'location': {
            'latitude': 28.0,  # Gulf of Mexico
            'longitude': -88.0,
            'water_depth': 2000  # m (deepwater)
        },
        'mooring': {
            'num_lines': 4,  # TLP typically 4 tendons
            'design_load_per_line': 12000,  # kN (high pretension)
            'soil_type': 'sand'
        }
    }

    print(f"Project: {project['name']}")
    print(f"Location: Gulf of Mexico ({project['location']['latitude']}°N, {project['location']['longitude']}°W)")
    print(f"Water depth: {project['location']['water_depth']} m (ultra-deepwater)")
    print(f"Tendons: {project['mooring']['num_lines']}, {project['mooring']['design_load_per_line']} kN/line\n")

    # Design taut-leg mooring
    mooring_client = MooringClient()

    print("Designing taut-leg mooring system...")

    mooring_line = mooring_client.design_mooring_line(
        design_load=project['mooring']['design_load_per_line'],
        water_depth=project['location']['water_depth'],
        mooring_type='taut_leg',  # Required for TLP
        soil_type=project['mooring']['soil_type']
    )

    print(f"  Mooring type: {mooring_line['mooring_type']}")
    print(f"  Total line length: {mooring_line['total_length']:.1f} m")

    print("  Segments:")
    for i, segment in enumerate(mooring_line['segments']):
        component = segment['component']

        if segment['type'] == 'synthetic_rope':
            print(f"    {i+1}. {segment['name']}: {segment['length']:.0f}m")
            print(f"       {component['fiber_type'].upper()} rope {component['diameter']}mm")
            print(f"       MBL: {component['minimum_breaking_load']} kN")
            print(f"       Floats: {component['floats']} (reduces submerged weight)")
            print(f"       Elongation@50%: {component['elongation_at_50_mbl']}%")

        elif segment['type'] == 'chain':
            print(f"    {i+1}. {segment['name']}: {segment['length']:.0f}m")
            print(f"       Chain {component['grade']} {component['diameter']}mm")

    anchor = mooring_line['anchor']
    print(f"  Anchor: {anchor['type']}, {anchor['dry_weight']}kg")
    print(f"  Holding capacity: {anchor['design_capacity_kn']} kN")

    print("\n  Key advantages of HMPE for ultra-deepwater:")
    print("    ✓ Floats (specific gravity 0.97 < 1.0)")
    print("    ✓ Very lightweight (reduces top tension)")
    print("    ✓ High strength-to-weight ratio")
    print("    ✓ Low elongation (3-4% vs 12% for polyester)")

    print("\n  ✓ TLP deepwater mooring design complete")


def main():
    """
    Run all integration examples.

    Critical: Demonstrates COMPLETE WORKFLOW with ZERO STORAGE!
    """
    print("=" * 70)
    print("Complete OrcaFlex Workflow - Integration Examples")
    print("=" * 70)

    print("\nThis example demonstrates the complete data procurement framework:")
    print("  1. Metocean data (ERA5 API → wave/wind/current)")
    print("  2. Vessel data (Generic RAO database → vessel + RAOs + hydrostatics)")
    print("  3. Mooring design (Repository databases → chains + ropes + anchors)")
    print("  4. OrcaFlex model (In-memory YAML → direct consumption)")
    print("\nKey principle: ZERO STORAGE - all data streamed/generated in-memory!\n")

    try:
        # Run examples
        result = example_fpso_mooring_design()
        example_floating_wind_turbine()
        example_tlp_deepwater()

        print("\n" + "=" * 70)
        print("All integration examples completed successfully!")
        print("=" * 70)

        print("\nWhat was demonstrated:")
        print("  ✓ Metocean data streaming (zero storage)")
        print("  ✓ Vessel RAOs and hydrostatics (generated on-demand)")
        print("  ✓ Mooring component selection (repository-based)")
        print("  ✓ Complete OrcaFlex models (in-memory YAML)")
        print("  ✓ Three different applications (FPSO, floating wind, TLP)")

        print("\nData flow summary:")
        print("  API/Repository → In-memory processing → OrcaFlex YAML")
        print("  NO intermediate files saved!")

        print("\nNext steps:")
        print("  1. Pass OrcaFlex YAML directly to OrcaFlex API")
        print("  2. Run simulations with real metocean conditions")
        print("  3. Customize component databases with project data")
        print("  4. Extend to other analysis software (AQWA, MOSES)")

    except Exception as e:
        print(f"\nError running examples: {e}")

        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
