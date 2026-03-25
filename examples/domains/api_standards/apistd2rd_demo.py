"""
Demonstration of the modernized API STD 2RD modules.

This script shows how to use the migrated API STD 2RD analysis modules
for pipe stress analysis.

Usage:
    PYTHONPATH=src python examples/apistd2rd_demo.py
"""

import numpy as np
import pandas as pd
from pathlib import Path
import tempfile
import yaml

from digitalmodel.structural.structural_analysis.models import APISTD2RDAnalyzer
from digitalmodel.infrastructure.calculations.pipe_properties import calculate_geometric_properties
from digitalmodel.infrastructure.calculations.stress_calculations import APISTD2RDCalculations
from digitalmodel.data_systems.data_manager.configuration import ConfigurationManager


def create_sample_configuration():
    """Create a sample configuration for demonstration."""
    config = {
        'default': {
            'logLevel': 'INFO',
            'Analysis': {
                'variableWTBurst': True,
                'nominalWTAPISTD2RDMethod1': True,
                'variableWTCollapse': False
            }
        },
        'geometry': {
            'NominalOD': 20.0,  # inches
            'DesignWT': 1.0,    # inches
            'NominalWT': [0.75, 1.0, 1.25, 1.5],  # For variable WT analysis
            'CorrosionAllowance': 0.125
        },
        'material': {
            'SMYS': 52000,      # psi
            'SMUS': 66000,      # psi
            'E': 30e6,          # psi
            'Poissionsratio': 0.3,
            'alphafab': 1.0,
            'k': 1.0
        },
        'designFactors': {
            'internalPressure': {
                'design': 0.72,
                'incidentalPressure': 0.80,
                'hydroStaticTest': 1.25
            },
            'externalPressure': {
                'design': 0.72
            }
        },
        'nominalWTAPISTD2RDMethod1': {
            'data': [
                {
                    'CorrosionAllowance': 0.125,
                    'ExternalPressure': 0,
                    'InternalPressure': 1000,
                    'LimitState': 'design'
                },
                {
                    'CorrosionAllowance': 0.125,
                    'ExternalPressure': 0,
                    'InternalPressure': 1200,
                    'LimitState': 'incidentalPressure'
                }
            ]
        },
        'plotSettings': {
            'variableWTBurst': {
                'plotFileName': 'burst_analysis_{0}_{1}_{2}.png',
                'pltTitle': 'Burst Analysis - {0} {2} = {1} in, CA = {2} in'
            }
        }
    }
    return config


def demo_pipe_properties():
    """Demonstrate pipe properties calculations."""
    print("\n" + "="*60)
    print("PIPE PROPERTIES CALCULATION DEMO")
    print("="*60)

    # Example 1: Calculate from OD and ID
    od = 20.0
    id_val = 18.0
    print(f"Example 1: OD = {od} in, ID = {id_val} in")

    props = calculate_geometric_properties(outer_diameter=od, inner_diameter=id_val)
    print(f"  Wall Thickness: {props['wall_thickness']:.3f} in")
    print(f"  Cross-sectional Area: {props['A']:.2f} in²")
    print(f"  Internal Area: {props['Ai']:.2f} in²")
    print(f"  Moment of Inertia: {props['I']:.2f} in⁴")

    # Example 2: Calculate from OD and WT
    od = 20.0
    wt = 1.25
    print(f"\nExample 2: OD = {od} in, WT = {wt} in")

    props = calculate_geometric_properties(outer_diameter=od, wall_thickness=wt)
    print(f"  Inner Diameter: {props['inner_diameter']:.3f} in")
    print(f"  Cross-sectional Area: {props['A']:.2f} in²")


def demo_stress_calculations():
    """Demonstrate stress calculations."""
    print("\n" + "="*60)
    print("STRESS CALCULATIONS DEMO")
    print("="*60)

    calculator = APISTD2RDCalculations()

    # Pipe parameters
    od = 20.0
    id_val = 18.0
    wt = 1.0
    smys = 52000  # psi
    smus = 66000  # psi
    elastic_modulus = 30e6  # psi
    poisson_ratio = 0.3
    corrosion_allowance = 0.125

    print(f"Pipe: OD = {od} in, ID = {id_val} in, WT = {wt} in")
    print(f"Material: SMYS = {smys} psi, SMUS = {smus} psi")

    # Burst pressure calculation
    burst_pressure = calculator.calculate_burst_pressure(
        od, id_val, smys, smus, corrosion_allowance
    )
    print(f"\nBurst Pressure (with CA): {burst_pressure:.0f} psi")

    # Design pressures
    design_factors = {
        'internalPressure': {
            'design': 0.72,
            'incidentalPressure': 0.80,
            'hydroStaticTest': 1.25
        }
    }

    design_pressures = calculator.calculate_design_pressures(burst_pressure, design_factors)
    print(f"Design Pressure: {design_pressures.get('design', 0):.0f} psi")
    print(f"Test Pressure: {design_pressures.get('test', 0):.0f} psi")

    # Collapse pressure calculation
    collapse_data = calculator.calculate_collapse_pressure(
        od, id_val, wt, smys, elastic_modulus, poisson_ratio
    )
    print(f"\nCollapse Pressures:")
    print(f"  Yield Collapse: {collapse_data['yield_collapse']:.0f} psi")
    print(f"  Elastic Collapse: {collapse_data['elastic_collapse']:.0f} psi")
    print(f"  Combined Collapse: {collapse_data['combined_collapse']:.0f} psi")


def demo_configuration_management():
    """Demonstrate configuration management."""
    print("\n" + "="*60)
    print("CONFIGURATION MANAGEMENT DEMO")
    print("="*60)

    # Create sample configuration
    config = create_sample_configuration()

    # Save to temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
        yaml.safe_dump(config, f, default_flow_style=False, indent=2)
        config_file = Path(f.name)

    print(f"Created sample configuration file: {config_file}")

    try:
        # Load configuration
        manager = ConfigurationManager()
        loaded_config = manager.load_configuration(config_file)

        print(f"Loaded configuration with {len(loaded_config)} main sections")

        # Update geometry properties
        manager.update_geometry_properties()
        geometry = loaded_config['geometry']
        print(f"Calculated ID from OD and WT: {geometry.get('NominalID', 'N/A')} in")

        # Validate configuration
        warnings = manager.validate_configuration()
        if warnings:
            print(f"Configuration warnings: {len(warnings)}")
            for warning in warnings:
                print(f"  - {warning}")
        else:
            print("Configuration validation passed ✓")

        # Prepare analysis input data
        input_data = manager.get_analysis_input_data(0)
        print(f"\nPrepared input data for analysis:")
        print(f"  OD: {input_data['OD']} in")
        print(f"  ID: {input_data['ID']} in")
        print(f"  Internal Pressure: {input_data['internalPressure']} psi")

    finally:
        config_file.unlink()  # Clean up


def demo_full_analysis():
    """Demonstrate a complete API STD 2RD analysis."""
    print("\n" + "="*60)
    print("FULL API STD 2RD ANALYSIS DEMO")
    print("="*60)

    # Create sample configuration
    config = create_sample_configuration()

    # Save to temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
        yaml.safe_dump(config, f, default_flow_style=False, indent=2)
        config_file = Path(f.name)

    try:
        # Initialize analyzer
        analyzer = APISTD2RDAnalyzer()
        analyzer.load_configuration(config_file)

        print("Configuration loaded successfully")

        # Run variable wall thickness burst analysis
        print("\nRunning variable wall thickness burst analysis...")
        burst_df, _ = analyzer.run_variable_wt_burst_analysis()

        print(f"Analysis completed for {len(burst_df)} wall thicknesses:")
        print(burst_df.to_string(index=False, float_format='%.1f'))

        # Run Method 1 analysis
        print("\nRunning Method 1 analysis...")
        utilization_dfs = analyzer.run_nominal_wt_method1_analysis()

        print(f"Method 1 analysis completed for {len(utilization_dfs)} cases")

        # Get analysis summary
        summary = analyzer.get_analysis_summary()
        print(f"\nAnalysis Summary:")
        print(f"  Results available: {summary['results_available']}")

        if 'variable_wt_burst_summary' in summary:
            burst_summary = summary['variable_wt_burst_summary']
            print(f"  Wall thickness range: {burst_summary['wall_thickness_range'][0]:.2f} - {burst_summary['wall_thickness_range'][1]:.2f} in")
            print(f"  Burst pressure range: {burst_summary['burst_pressure_range'][0]:.0f} - {burst_summary['burst_pressure_range'][1]:.0f} psi")

    finally:
        config_file.unlink()  # Clean up


def main():
    """Run all demonstrations."""
    print("API STD 2RD MODERNIZED MODULES DEMONSTRATION")
    print("=" * 70)
    print("This demonstration shows the capabilities of the migrated")
    print("API STD 2RD modules with modern Python best practices.")

    try:
        demo_pipe_properties()
        demo_stress_calculations()
        demo_configuration_management()
        demo_full_analysis()

        print("\n" + "="*70)
        print("✅ ALL DEMONSTRATIONS COMPLETED SUCCESSFULLY!")
        print("="*70)
        print("\nKey improvements in the modernized modules:")
        print("  ✓ Secure YAML loading (yaml.safe_load)")
        print("  ✓ Cross-platform path handling (pathlib)")
        print("  ✓ Comprehensive type hints and docstrings")
        print("  ✓ Modern error handling and validation")
        print("  ✓ Absolute imports from digitalmodel package")
        print("  ✓ Object-oriented design with proper encapsulation")
        print("  ✓ Comprehensive input validation")
        print("  ✓ Consistent logging throughout")

    except Exception as e:
        print(f"\n❌ Demonstration failed with error: {e}")
        raise


if __name__ == "__main__":
    main()