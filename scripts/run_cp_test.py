#!/usr/bin/env python3
"""
ABOUTME: Execute cathodic protection tests from YAML configuration files
ABOUTME: Loads test configs and generates HTML reports with DNV RP-F103 calculations
"""

import sys
import yaml
from pathlib import Path

# Add src to path
src_path = Path(__file__).parent.parent / "src"
sys.path.insert(0, str(src_path))

from digitalmodel.common.cathodic_protection import CathodicProtection
from digitalmodel.reporting.cp_html_report import CPHTMLReportGenerator


def load_test_config(yaml_path: Path) -> dict:
    """
    Load test configuration from YAML file.

    Args:
        yaml_path: Path to YAML configuration file

    Returns:
        Configuration dictionary with 'inputs' key structure
    """
    with open(yaml_path, 'r') as f:
        config = yaml.safe_load(f)

    # Return in the format expected by CathodicProtection class
    # The DNV method expects: {"inputs": {...}}
    if 'inputs' in config:
        result = {'inputs': config['inputs']}
    else:
        # If config is already in the right format
        result = config

    # Ensure calculation_type is set for router method
    # This script is specifically for DNV RP-F103:2010 calculations
    if 'inputs' not in result:
        result['inputs'] = {}
    if 'calculation_type' not in result['inputs']:
        result['inputs']['calculation_type'] = 'DNV_RP_F103_2010'

    return result


def main():
    """Execute CP test from YAML configuration."""

    if len(sys.argv) < 2:
        print("Usage: python run_cp_test.py <config.yaml>")
        print("\nExample:")
        print("  python scripts/run_cp_test.py config/input/cp_poor_coating.yaml")
        sys.exit(1)

    config_path = Path(sys.argv[1])

    if not config_path.exists():
        print(f"Error: Configuration file not found: {config_path}")
        sys.exit(1)

    # Load full YAML for metadata
    with open(config_path, 'r') as f:
        full_config = yaml.safe_load(f)

    test_name = full_config.get('test_metadata', {}).get('test_name', 'unknown')
    test_purpose = full_config.get('test_metadata', {}).get('test_purpose', 'CP Analysis')

    print("=" * 80)
    print(f"Cathodic Protection Test Execution")
    print(f"Test: {test_name}")
    print(f"Purpose: {test_purpose}")
    print("=" * 80)
    print()

    # Load configuration for calculations
    config = load_test_config(config_path)

    # Initialize CP calculator
    cp = CathodicProtection()

    # Run DNV RP-F103:2010 calculation using router method
    # Note: router() mutates config in-place and returns the modified config
    print("Running DNV RP-F103:2010 calculation...")
    config = cp.router(config)
    print("✓ Calculation complete")
    print()

    # Display key results
    if 'results' in config:
        res = config['results']

        print("Key Results:")
        print("-" * 80)

        # Current demand
        if 'current_demand_A' in res:
            cd = res['current_demand_A']
            print(f"Current Demand:")
            print(f"  Initial: {cd.get('initial_current_demand_A', 0):.2f} A")
            print(f"  Mean:    {cd.get('mean_current_demand_A', 0):.2f} A")
            print(f"  Final:   {cd.get('final_current_demand_A', 0):.2f} A")
            print()

        # Anode requirements
        if 'anode_requirements' in res:
            ar = res['anode_requirements']
            print(f"Anode Requirements:")
            print(f"  Count:      {ar.get('anode_count', 0):.0f} anodes")
            print(f"  Total Mass: {ar.get('total_anode_mass_kg', 0):,.0f} kg")
            print()

        # Anode spacing
        if 'anode_spacing_m' in res:
            spacing = res['anode_spacing_m'].get('spacing_m', 0)
            print(f"Anode Spacing: {spacing:.2f} m")
            print()

    # Generate HTML report
    print("Generating HTML report...")
    report_gen = CPHTMLReportGenerator()

    # Create report title from test metadata
    report_title = f"CP Analysis: {test_name}"

    report_path = report_gen.generate_attenuation_report(
        results=config,
        title=report_title
    )

    print(f"✓ HTML report generated: {report_path}")
    print()

    # Show validation results if available
    if 'validation_criteria' in full_config:
        print("Validation Criteria:")
        print("-" * 80)
        for criterion in full_config['validation_criteria']:
            metric = criterion.get('metric', 'unknown')
            operator = criterion.get('operator', '==')
            value = criterion.get('value', 0)
            print(f"  {metric} {operator} {value}")
        print()

    print("=" * 80)
    print("Test execution complete!")
    print("=" * 80)


if __name__ == "__main__":
    main()
