# ABOUTME: Example demonstrating vessel data procurement with RAO streaming
# ABOUTME: Vessel-based queries with in-memory processing and direct OrcaFlex consumption

"""
Vessel Streaming Example
=========================

Demonstrates vessel data procurement with:
- Vessel database queries (MarineTraffic, ShipXplorer)
- Generic RAO database with interpolation
- Hydrostatic properties calculation
- Direct OrcaFlex vessel YAML output

Critical: NO data saving (RAO files can be large)!

This example shows how to:
1. Query vessel data by IMO/MMSI/name
2. Get RAOs for generic vessel types
3. Calculate hydrostatic properties
4. Generate OrcaFlex vessel YAML (in-memory)
"""

import sys
from pathlib import Path
import numpy as np

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.data_procurement.vessel import VesselClient


def example_vessel_database_query():
    """
    Example 1: Query vessel database by IMO/MMSI/name.

    Demonstrates:
    - Automatic provider selection (MarineTraffic or ShipXplorer)
    - Fallback mechanism
    - Vessel data retrieval
    """
    print("\n=== Example 1: Vessel Database Query ===\n")

    config_path = Path(__file__).parent.parent / \
                  'specs/modules/data-procurement/vessel-systems/configs/example_config.yml'

    client = VesselClient.from_config(str(config_path))

    # Query by IMO (auto-detected)
    print("Querying vessel by IMO: 9321483")
    try:
        vessel = client.get_vessel("9321483")

        print(f"\nVessel Data:")
        print(f"  Name: {vessel['name']}")
        print(f"  IMO: {vessel['imo']}")
        print(f"  Type: {vessel['type']}")
        print(f"  Dimensions: {vessel['length']:.1f}m × {vessel['beam']:.1f}m × {vessel['draft']:.1f}m")
        print(f"  DWT: {vessel['dwt']:,} tonnes")
        print(f"  Built: {vessel['built_year']}")
        print(f"  Flag: {vessel['flag']}")

    except Exception as e:
        print(f"Note: {e}")
        print("  (MarineTraffic/ShipXplorer API key may be required)")

    # Query by name
    print("\nSearching vessels by name: 'Front Altair'")
    try:
        vessel = client.get_vessel("Front Altair", identifier_type='name')
        print(f"  Found: {vessel['name']} (IMO: {vessel['imo']})")
    except Exception as e:
        print(f"  Note: {e}")


def example_generic_rao_retrieval():
    """
    Example 2: Get generic RAOs for vessel type.

    Demonstrates:
    - Generic RAO database queries
    - RAO data structure
    - 6 DOF motion RAOs
    """
    print("\n=== Example 2: Generic RAO Retrieval ===\n")

    config_path = Path(__file__).parent.parent / \
                  'specs/modules/data-procurement/vessel-systems/configs/example_config.yml'

    client = VesselClient.from_config(str(config_path))

    # Get RAOs for VLCC at 20m draft
    print("Retrieving RAOs for VLCC at 20m draft")

    raos = client.get_vessel_raos(
        vessel_type="VLCC",
        draft=20.0,
        wave_directions=[0, 45, 90, 135, 180],
        frequencies=np.arange(0.2, 1.5, 0.1)
    )

    print(f"\nRAO Data:")
    print(f"  Vessel Type: {raos['vessel_type']}")
    print(f"  Draft: {raos['draft']} m")
    print(f"  Wave Directions: {list(raos['wave_directions'])} degrees")
    print(f"  Frequencies: {len(raos['frequencies'])} points ({raos['frequencies'][0]:.2f} - {raos['frequencies'][-1]:.2f} rad/s)")
    print(f"  Periods: {raos['periods'][0]:.1f} - {raos['periods'][-1]:.1f} s")
    print(f"  DOF: {list(raos['raos'].keys())}")

    # Show sample RAO values
    print(f"\nSample Heave RAOs (head seas, 0°):")
    for i in range(min(5, len(raos['frequencies']))):
        freq = raos['frequencies'][i]
        period = raos['periods'][i]
        amplitude = raos['raos']['heave'][0.0]['amplitude'][i]
        phase = raos['raos']['heave'][0.0]['phase'][i]

        print(f"  T={period:.1f}s (f={freq:.2f} rad/s): Amplitude={amplitude:.3f} m/m, Phase={phase:.1f}°")

    print("\nNote: RAOs processed in-memory (zero storage)")


def example_hydrostatic_calculation():
    """
    Example 3: Calculate hydrostatic properties.

    Demonstrates:
    - Automatic hydrostatic calculation
    - GM, center of gravity, radii of gyration
    - Natural period estimation
    """
    print("\n=== Example 3: Hydrostatic Properties ===\n")

    config_path = Path(__file__).parent.parent / \
                  'specs/modules/data-procurement/vessel-systems/configs/example_config.yml'

    client = VesselClient.from_config(str(config_path))

    # Get generic VLCC particulars
    from digitalmodel.data_procurement.vessel.api_clients import GenericRAOClient

    rao_client = GenericRAOClient()
    vessel_particulars = rao_client.get_vessel_particulars("VLCC")

    # Add required fields for hydrostatics
    vessel = {
        'name': 'Generic VLCC',
        'length': vessel_particulars['loa'],
        'beam': vessel_particulars['beam'],
        'depth': vessel_particulars['depth'],
        'draft': 20.0,
        'dwt': vessel_particulars['dwt']
    }

    print(f"Calculating hydrostatics for {vessel['name']}")
    print(f"  LOA: {vessel['length']}m, Beam: {vessel['beam']}m, Draft: {vessel['draft']}m")

    # Calculate hydrostatics
    hydro = client.calculate_hydrostatics(vessel, draft=20.0)

    print(f"\nHydrostatic Properties:")
    print(f"  Displacement: {hydro['displacement']:,.0f} tonnes")
    print(f"  Block Coefficient (Cb): {hydro['block_coefficient']:.3f}")
    print(f"  Waterplane Coefficient (Cwp): {hydro['waterplane_coefficient']:.3f}")
    print(f"  GM (Metacentric Height): {hydro['gm_metacentric_height']:.2f} m")
    print(f"  KB (Center of Buoyancy): {hydro['kb_center_buoyancy']:.2f} m")
    print(f"  KG (Center of Gravity): {hydro['kg_center_gravity']:.2f} m")
    print(f"  Radii of Gyration:")
    print(f"    Roll (Kxx): {hydro['radii_of_gyration']['roll']:.2f} m")
    print(f"    Pitch (Kyy): {hydro['radii_of_gyration']['pitch']:.2f} m")
    print(f"    Yaw (Kzz): {hydro['radii_of_gyration']['yaw']:.2f} m")

    # Calculate natural periods
    periods = client.calculate_natural_periods(vessel, hydro)

    print(f"\nNatural Periods:")
    print(f"  Roll: {periods['roll']:.1f} s")
    print(f"  Pitch: {periods['pitch']:.1f} s")
    print(f"  Heave: {periods['heave']:.1f} s")


def example_orcaflex_vessel_yaml():
    """
    Example 4: Generate OrcaFlex vessel YAML (in-memory).

    Demonstrates:
    - Complete vessel definition for OrcaFlex
    - RAO integration
    - Direct consumption (no file saving)
    """
    print("\n=== Example 4: OrcaFlex Vessel YAML ===\n")

    config_path = Path(__file__).parent.parent / \
                  'specs/modules/data-procurement/vessel-systems/configs/example_config.yml'

    client = VesselClient.from_config(str(config_path))

    # Get vessel particulars
    from digitalmodel.data_procurement.vessel.api_clients import GenericRAOClient

    rao_client = GenericRAOClient()
    vessel_particulars = rao_client.get_vessel_particulars("VLCC")

    vessel = {
        'name': 'Generic VLCC',
        'length': vessel_particulars['loa'],
        'beam': vessel_particulars['beam'],
        'depth': vessel_particulars['depth'],
        'draft': 20.0,
        'dwt': vessel_particulars['dwt']
    }

    # Get RAOs
    raos = client.get_vessel_raos(vessel_type="VLCC", draft=20.0)

    print("Generating OrcaFlex vessel YAML (in-memory)")

    # Generate OrcaFlex YAML
    orcaflex_yaml = client.to_orcaflex_vessel(vessel, raos, draft=20.0)

    # Show sample (first 50 lines)
    lines = orcaflex_yaml.split('\n')
    print(f"\nOrcaFlex Vessel YAML ({len(lines)} lines total):\n")
    print('\n'.join(lines[:30]))
    print("\n... (truncated)")

    print(f"\nNote: {len(lines)} lines of OrcaFlex YAML generated in-memory")
    print("  Ready for direct consumption by OrcaFlex API (no file I/O)")


def example_complete_workflow():
    """
    Example 5: Complete vessel workflow (database → RAOs → OrcaFlex).

    Demonstrates:
    - End-to-end vessel data pipeline
    - Zero storage architecture
    - Direct consumption
    """
    print("\n=== Example 5: Complete Vessel Workflow ===\n")

    config_path = Path(__file__).parent.parent / \
                  'specs/modules/data-procurement/vessel-systems/configs/example_config.yml'

    client = VesselClient.from_config(str(config_path))

    print("Complete Workflow: IMO → Vessel Data → RAOs → OrcaFlex YAML\n")

    # Step 1: Get vessel from database (or use generic)
    print("Step 1: Get vessel data")
    vessel_type = "VLCC"  # Fallback to generic if API query fails

    try:
        vessel = client.get_vessel("9321483")
        vessel_type = client._map_vessel_type(vessel.get('type'))
        print(f"  Retrieved: {vessel['name']} ({vessel_type})")
    except Exception as e:
        print(f"  Using generic {vessel_type} (API query failed: {e})")

        # Use generic vessel
        from digitalmodel.data_procurement.vessel.api_clients import GenericRAOClient
        rao_client = GenericRAOClient()
        vessel_particulars = rao_client.get_vessel_particulars(vessel_type)

        vessel = {
            'name': f'Generic {vessel_type}',
            'length': vessel_particulars['loa'],
            'beam': vessel_particulars['beam'],
            'depth': vessel_particulars['depth'],
            'draft': 20.0,
            'dwt': vessel_particulars['dwt']
        }

    # Step 2: Get RAOs
    print(f"\nStep 2: Retrieve {vessel_type} RAOs")
    raos = client.get_vessel_raos(vessel_type=vessel_type, draft=20.0)
    print(f"  RAOs: 6 DOF, {len(raos['wave_directions'])} directions, {len(raos['frequencies'])} frequencies")

    # Step 3: Calculate hydrostatics
    print("\nStep 3: Calculate hydrostatic properties")
    hydro = client.calculate_hydrostatics(vessel, draft=20.0)
    print(f"  Displacement: {hydro['displacement']:,.0f} tonnes, GM: {hydro['gm_metacentric_height']:.2f} m")

    # Step 4: Generate OrcaFlex YAML
    print("\nStep 4: Generate OrcaFlex vessel YAML (in-memory)")
    orcaflex_yaml = client.to_orcaflex_vessel(vessel, raos, hydro, draft=20.0)
    lines = orcaflex_yaml.split('\n')
    print(f"  Generated: {len(lines)} lines of YAML")

    # Step 5: Ready for OrcaFlex
    print("\nStep 5: Ready for direct consumption")
    print("  → Pass YAML directly to OrcaFlex API")
    print("  → No intermediate files saved")
    print("  → Zero storage footprint")

    print("\n✓ Complete workflow executed with ZERO file storage!")


def main():
    """
    Run all vessel streaming examples.

    Critical: All examples demonstrate ZERO STORAGE architecture!
    """
    print("=" * 70)
    print("Vessel Data Procurement - Zero Storage Examples")
    print("=" * 70)

    print("\nKey Principles:")
    print("  1. Vessel-based queries (IMO/MMSI/name → data)")
    print("  2. In-memory RAO processing")
    print("  3. Direct consumption (OrcaFlex vessel YAML)")
    print("  4. NO data file storage (RAO files can be large)")
    print("  5. FREE APIs (MarineTraffic, ShipXplorer, Generic DB)\n")

    try:
        # Run examples
        example_vessel_database_query()
        example_generic_rao_retrieval()
        example_hydrostatic_calculation()
        example_orcaflex_vessel_yaml()
        example_complete_workflow()

        print("\n" + "=" * 70)
        print("All vessel examples completed successfully!")
        print("=" * 70)

        print("\nNext Steps:")
        print("  1. Configure API keys in example_config.yml (optional)")
        print("  2. Integrate with OrcaFlex models")
        print("  3. Customize RAO database with vessel-specific data")
        print("  4. Proceed to Phase 3: Mooring Systems")

    except Exception as e:
        print(f"\nError running examples: {e}")

        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
