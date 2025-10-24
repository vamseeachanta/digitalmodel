# ABOUTME: Example demonstrating mooring component procurement and system design
# ABOUTME: Component selection, mooring line design, and direct OrcaFlex consumption

"""
Mooring Systems Example
========================

Demonstrates mooring component data procurement with:
- Component database queries (chains, ropes, anchors, connectors)
- Mooring line design (catenary, taut-leg, semi-taut)
- Standards validation (API RP 2SK)
- Direct OrcaFlex mooring YAML output

Critical: Repository-based data (no storage)!

This example shows how to:
1. Query component databases by design criteria
2. Compare different grades/types
3. Design complete mooring lines
4. Validate against standards
5. Generate OrcaFlex mooring YAML (in-memory)
"""

import sys
from pathlib import Path
import numpy as np

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.data_procurement.mooring import MooringClient


def example_chain_database():
    """
    Example 1: Query chain database.

    Demonstrates:
    - Chain selection by design load
    - Grade comparison
    - Property retrieval
    """
    print("\n=== Example 1: Chain Database ===\n")

    client = MooringClient()

    # Find chains for 3500 kN design load
    print("Finding chains for 3500 kN design load:")

    chains = client.chain_client.find_by_design_load(3500, grade='R4S')

    print(f"\nFound {len(chains)} suitable R4S chains:")
    for chain in chains[:3]:  # Show first 3
        print(f"  {chain['diameter']}mm: MBL={chain['minimum_breaking_load']}kN, "
              f"Mass={chain['mass_per_meter']}kg/m")

    # Compare grades
    print("\nComparing grades for 3500 kN:")
    comparison = client.chain_client.compare_grades(3500)

    for grade, chain in comparison.items():
        print(f"  {grade}: {chain['diameter']}mm, "
              f"MBL={chain['minimum_breaking_load']}kN, "
              f"Mass={chain['mass_per_meter']}kg/m")

    # Get specific chain
    print("\nGetting R4S 127mm chain:")
    chain = client.get_chain(grade='R4S', diameter=127)

    print(f"  Proof load: {chain['proof_load']} kN")
    print(f"  MBL: {chain['minimum_breaking_load']} kN")
    print(f"  Mass: {chain['mass_per_meter']} kg/m")
    print(f"  Submerged: {chain['submerged_mass_per_meter']} kg/m")
    print(f"  EA: {chain['axial_stiffness_ea']} kN")


def example_synthetic_rope():
    """
    Example 2: Query synthetic rope database.

    Demonstrates:
    - Fiber type selection
    - Nonlinear stiffness
    - Polyester vs HMPE comparison
    """
    print("\n=== Example 2: Synthetic Rope Database ===\n")

    client = MooringClient()

    # Find polyester ropes for 8000 kN design load
    print("Finding polyester ropes for 8000 kN design load:")

    ropes = client.synthetic_rope_client.find_by_design_load(
        8000,
        fiber_type='polyester'
    )

    print(f"\nFound {len(ropes)} suitable polyester ropes:")
    for rope in ropes[:3]:
        print(f"  {rope['diameter']}mm: MBL={rope['minimum_breaking_load']}kN, "
              f"Mass={rope['mass_per_meter']}kg/m, "
              f"Elongation@50%={rope['elongation_at_50_mbl']}%")

    # Compare fiber types
    print("\nComparing fiber types for 8000 kN:")
    comparison = client.synthetic_rope_client.compare_fiber_types(8000)

    for fiber, rope in comparison.items():
        print(f"  {fiber.capitalize()}: {rope['diameter']}mm, "
              f"MBL={rope['minimum_breaking_load']}kN, "
              f"Floats={rope['floats']}, "
              f"Service life={rope['service_life']}yr")

    # Get polyester rope and calculate nonlinear stiffness
    print("\nPolyester rope 220mm - Nonlinear stiffness:")
    rope = client.get_synthetic_rope(fiber_type='polyester', diameter=220)

    stiffness = client.synthetic_rope_client.calculate_nonlinear_stiffness(rope)

    print(f"  Tension points: {stiffness['tension_percent_mbl'][:5]}% MBL")
    print(f"  Extension: {stiffness['extension_percent'][:5]}%")


def example_anchor_selection():
    """
    Example 3: Anchor selection and holding capacity.

    Demonstrates:
    - Anchor selection by holding capacity
    - Soil-specific capacity calculation
    - Anchor type comparison
    """
    print("\n=== Example 3: Anchor Selection ===\n")

    client = MooringClient()

    # Find anchors for 1500 kN holding capacity in clay
    print("Finding anchors for 1500 kN holding capacity (clay soil):")

    anchors = client.anchor_client.find_by_holding_capacity(
        1500,
        soil='clay',
        safety_factor=1.5
    )

    print(f"\nFound {len(anchors)} suitable anchors:")
    for anchor in anchors[:3]:
        print(f"  {anchor['type']}, {anchor['dry_weight']}kg: "
              f"Clay capacity={anchor['clay_capacity_kn']}kN")

    # Compare anchor types
    print("\nComparing anchor types for 1500 kN:")
    comparison = client.anchor_client.compare_anchor_types(1500, soil='clay')

    for anchor_type, anchor in comparison.items():
        print(f"  {anchor_type}: {anchor['dry_weight']}kg, "
              f"Capacity={anchor['clay_capacity_kn']}kN")

    # Get specific anchor and calculate holding capacity
    print("\nStevpris Mk6 20,000kg anchor - Holding capacity:")
    anchor = client.get_anchor(anchor_type='stevpris_mk6', weight=20000)

    capacity = client.anchor_client.calculate_holding_capacity(
        anchor,
        soil_type='clay',
        su=50  # Undrained shear strength 50 kPa
    )

    print(f"  Submerged weight: {capacity['submerged_weight_kn']} kN")
    print(f"  Capacity factor: {capacity['capacity_factor']}")
    print(f"  Holding capacity: {capacity['holding_capacity_kn']} kN")
    print(f"  Soil: {capacity['soil_type']}, Su={capacity['su_kpa']} kPa")


def example_catenary_mooring_design():
    """
    Example 4: Design catenary mooring line.

    Demonstrates:
    - Automatic component selection
    - Catenary mooring design
    - Standards validation
    - OrcaFlex export
    """
    print("\n=== Example 4: Catenary Mooring Design ===\n")

    client = MooringClient()

    # Design catenary mooring for FPSO
    print("Designing catenary mooring line:")
    print("  Design load: 2000 kN")
    print("  Water depth: 1000 m")
    print("  Soil: Clay")

    line = client.design_mooring_line(
        design_load=2000,
        water_depth=1000,
        mooring_type='catenary',
        soil_type='clay'
    )

    print(f"\nMooring Line Design:")
    print(f"  Type: {line['mooring_type']}")
    print(f"  Total length: {line['total_length']:.1f} m")
    print(f"  Safety factor: {line['safety_factor']}")

    print(f"\nSegments:")
    for i, segment in enumerate(line['segments']):
        component = segment['component']

        if segment['type'] == 'chain':
            print(f"  {i+1}. {segment['name']}: {segment['length']:.1f}m")
            print(f"     Chain {component['grade']} {component['diameter']}mm, "
                  f"MBL={component['minimum_breaking_load']}kN")

        elif segment['type'] == 'wire_rope':
            print(f"  {i+1}. {segment['name']}: {segment['length']:.1f}m")
            print(f"     Wire rope {component['construction']} {component['diameter']}mm, "
                  f"MBL={component['minimum_breaking_load']}kN")

    print(f"\nAnchor:")
    anchor = line['anchor']
    print(f"  {anchor['type']}, {anchor['dry_weight']}kg")
    print(f"  Holding capacity: {anchor['design_capacity_kn']} kN")

    # Validation
    print(f"\nValidation:")
    validation = line['validation']
    print(f"  Valid: {validation['valid']}")
    print(f"  Standard: {validation['standard']}")

    if validation['issues']:
        print(f"  Issues:")
        for issue in validation['issues']:
            print(f"    - {issue}")

    if validation['warnings']:
        print(f"  Warnings:")
        for warning in validation['warnings']:
            print(f"    - {warning}")


def example_taut_leg_mooring_design():
    """
    Example 5: Design taut-leg mooring line.

    Demonstrates:
    - Taut-leg mooring with synthetic rope
    - Deepwater design
    - HMPE rope selection
    """
    print("\n=== Example 5: Taut-Leg Mooring Design ===\n")

    client = MooringClient()

    # Design taut-leg mooring for TLP
    print("Designing taut-leg mooring line:")
    print("  Design load: 2500 kN")
    print("  Water depth: 2000 m (deepwater)")
    print("  Soil: Sand")

    line = client.design_mooring_line(
        design_load=2500,
        water_depth=2000,
        mooring_type='taut_leg',
        soil_type='sand'
    )

    print(f"\nMooring Line Design:")
    print(f"  Type: {line['mooring_type']}")
    print(f"  Total length: {line['total_length']:.1f} m")

    print(f"\nSegments:")
    for i, segment in enumerate(line['segments']):
        component = segment['component']

        if segment['type'] == 'synthetic_rope':
            print(f"  {i+1}. {segment['name']}: {segment['length']:.1f}m")
            print(f"     {component['fiber_type'].capitalize()} rope {component['diameter']}mm, "
                  f"MBL={component['minimum_breaking_load']}kN")
            print(f"     Elongation@50%: {component['elongation_at_50_mbl']}%")
            print(f"     Floats: {component['floats']}")

        elif segment['type'] == 'chain':
            print(f"  {i+1}. {segment['name']}: {segment['length']:.1f}m")
            print(f"     Chain {component['grade']} {component['diameter']}mm")

    print(f"\nAnchor:")
    anchor = line['anchor']
    print(f"  {anchor['type']}, {anchor['dry_weight']}kg")
    print(f"  Holding capacity: {anchor['design_capacity_kn']} kN")


def example_orcaflex_export():
    """
    Example 6: Export to OrcaFlex mooring YAML.

    Demonstrates:
    - Complete OrcaFlex mooring definition
    - Multi-segment line types
    - Direct consumption (no file saving)
    """
    print("\n=== Example 6: OrcaFlex Export ===\n")

    client = MooringClient()

    # Design catenary mooring
    line = client.design_mooring_line(
        design_load=2000,
        water_depth=1000,
        mooring_type='catenary'
    )

    print("Generating OrcaFlex mooring YAML (in-memory):")

    # Generate OrcaFlex YAML
    orcaflex_yaml = client.to_orcaflex_mooring(line, name="FPSO_Mooring_Line1")

    # Show sample (first 50 lines)
    lines = orcaflex_yaml.split('\n')
    print(f"\nOrcaFlex Mooring YAML ({len(lines)} lines total):\n")
    print('\n'.join(lines[:40]))
    print("\n... (truncated)")

    print(f"\nNote: {len(lines)} lines of OrcaFlex YAML generated in-memory")
    print("  Ready for direct consumption by OrcaFlex API (no file I/O)")


def main():
    """
    Run all mooring examples.

    Critical: All examples demonstrate repository-based data!
    """
    print("=" * 70)
    print("Mooring Systems Data Procurement Examples")
    print("=" * 70)

    print("\nKey Principles:")
    print("  1. Repository-based component databases")
    print("  2. Component selection by design criteria")
    print("  3. Automatic mooring line design")
    print("  4. Standards validation (API RP 2SK)")
    print("  5. Direct consumption (OrcaFlex mooring YAML)")
    print("  6. Zero storage (component data in repository)\n")

    try:
        # Run examples
        example_chain_database()
        example_synthetic_rope()
        example_anchor_selection()
        example_catenary_mooring_design()
        example_taut_leg_mooring_design()
        example_orcaflex_export()

        print("\n" + "=" * 70)
        print("All mooring examples completed successfully!")
        print("=" * 70)

        print("\nNext Steps:")
        print("  1. Customize component databases with project-specific data")
        print("  2. Integrate with OrcaFlex models")
        print("  3. Add manufacturer-specific components")
        print("  4. Use in mooring system design")

    except Exception as e:
        print(f"\nError running examples: {e}")

        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
