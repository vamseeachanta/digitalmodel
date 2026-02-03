"""
Plate Capacity Analysis Examples

This module provides example usage of the plate capacity analysis modules,
demonstrating various use cases and analysis scenarios.

Author: Generated for migrated plate capacity modules
Date: 2025-01-15
"""

import sys
from pathlib import Path
import logging
import json

# Add src directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.structural.analysis.plate_capacity import (
    PlateProperties, AppliedLoads, BucklingConstants, BucklingResults,
    PlateBucklingAnalyzer, BoundaryCondition, run_example_analysis
)
from digitalmodel.structural.analysis.multi_plate_analyzer import (
    MultiPlateAnalyzer, PlateConfiguration, create_legacy_multi_plate_analyzer
)
from digitalmodel.infrastructure.calculations.plate_buckling import (
    ElasticBucklingCalculator, SlendernessCalculator, UltimateStrengthCalculator,
    UsageFactorCalculator, PlateEdgeCondition, calculate_plate_buckling_212
)

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


def example_1_basic_plate_analysis():
    """Example 1: Basic single plate buckling analysis."""

    print("=" * 60)
    print("EXAMPLE 1: Basic Single Plate Buckling Analysis")
    print("=" * 60)

    # Define a steel plate with typical marine structure properties
    plate_props = PlateProperties(
        length=3.0,          # 3 meters
        breadth=0.8,         # 0.8 meters
        thickness=0.012,     # 12 mm
        youngs_modulus=210e9,  # 210 GPa (steel)
        poisson_ratio=0.3,
        yield_strength=355e6,  # 355 MPa (high strength steel)
        water_depth=50.0,    # 50 meters
        length_unit="m",
        stress_unit="Pa"
    )

    # Define applied loads (moderate loading)
    applied_loads = AppliedLoads(
        longitudinal_stress=50e6,   # 50 MPa compression
        transverse_stress=30e6,     # 30 MPa compression
        shear_stress=20e6           # 20 MPa shear
    )

    # Create analyzer and run analysis
    analyzer = PlateBucklingAnalyzer(
        plate_props=plate_props,
        applied_loads=applied_loads,
        boundary_condition=BoundaryCondition.SIMPLY_SUPPORTED
    )

    results = analyzer.perform_analysis()

    # Display results
    print(f"Plate dimensions: {plate_props.length}m × {plate_props.breadth}m × {plate_props.thickness*1000:.1f}mm")
    print(f"Material: Steel (fy = {plate_props.yield_strength/1e6:.0f} MPa)")
    print(f"Von Mises stress: {results.von_mises_stress/1e6:.1f} MPa")
    print(f"Safety status: {'SAFE' if results.is_safe else 'UNSAFE'}")

    if not results.is_safe:
        print(f"Failure mode: {results.failure_mode}")

    print(f"\nUsage Factors:")
    print(f"  Longitudinal: {results.usage_factor_longitudinal:.3f}")
    print(f"  Transverse:   {results.usage_factor_transverse:.3f}")
    print(f"  Shear:        {results.usage_factor_shear:.3f}")
    print(f"  Biaxial:      {results.usage_factor_biaxial:.3f}")

    print(f"\nSlenderness Ratios:")
    print(f"  λx: {results.lambda_x:.3f}")
    print(f"  λy: {results.lambda_y:.3f}")
    print(f"  λτ: {results.lambda_tau:.3f}")

    return results


def example_2_parametric_study():
    """Example 2: Parametric study of plate thickness effect."""

    print("\n" + "=" * 60)
    print("EXAMPLE 2: Parametric Study - Effect of Plate Thickness")
    print("=" * 60)

    # Base plate properties
    base_props = PlateProperties(
        length=2.5,
        breadth=0.7,
        thickness=0.010,  # Will be varied
        youngs_modulus=210e9,
        poisson_ratio=0.3,
        yield_strength=235e6,  # Mild steel
        water_depth=40.0
    )

    # Fixed applied loads
    applied_loads = AppliedLoads(
        longitudinal_stress=40e6,
        transverse_stress=60e6,
        shear_stress=25e6
    )

    # Test different thicknesses
    thicknesses = [0.008, 0.010, 0.012, 0.015, 0.020]  # 8mm to 20mm

    print(f"{'Thickness (mm)':<15} {'Max Usage':<12} {'Status':<8} {'Critical Mode'}")
    print("-" * 55)

    for thickness in thicknesses:
        # Update thickness
        plate_props = PlateProperties(
            length=base_props.length,
            breadth=base_props.breadth,
            thickness=thickness,
            youngs_modulus=base_props.youngs_modulus,
            poisson_ratio=base_props.poisson_ratio,
            yield_strength=base_props.yield_strength,
            water_depth=base_props.water_depth
        )

        # Analyze
        analyzer = PlateBucklingAnalyzer(plate_props, applied_loads)
        results = analyzer.perform_analysis()

        max_usage = max([
            results.usage_factor_longitudinal,
            results.usage_factor_transverse,
            results.usage_factor_shear,
            results.usage_factor_biaxial
        ])

        status = "SAFE" if results.is_safe else "UNSAFE"
        critical_mode = results.failure_mode if results.failure_mode else "None"

        print(f"{thickness*1000:<15.1f} {max_usage:<12.3f} {status:<8} {critical_mode}")


def example_3_boundary_condition_comparison():
    """Example 3: Compare different boundary conditions."""

    print("\n" + "=" * 60)
    print("EXAMPLE 3: Boundary Condition Comparison")
    print("=" * 60)

    # Standard plate
    plate_props = PlateProperties(
        length=2.0,
        breadth=0.6,
        thickness=0.010,
        youngs_modulus=210e9,
        poisson_ratio=0.3,
        yield_strength=275e6
    )

    # Heavy loading
    applied_loads = AppliedLoads(
        longitudinal_stress=80e6,
        transverse_stress=50e6,
        shear_stress=30e6
    )

    boundary_conditions = [
        BoundaryCondition.SIMPLY_SUPPORTED,
        BoundaryCondition.SIDES_CLAMPED
    ]

    print(f"{'Boundary Condition':<20} {'Max Usage':<12} {'Status':<8} {'Critical Buckling (MPa)'}")
    print("-" * 65)

    for bc in boundary_conditions:
        analyzer = PlateBucklingAnalyzer(plate_props, applied_loads, boundary_condition=bc)
        results = analyzer.perform_analysis()

        max_usage = max([
            results.usage_factor_longitudinal,
            results.usage_factor_transverse,
            results.usage_factor_shear,
            results.usage_factor_biaxial
        ])

        status = "SAFE" if results.is_safe else "UNSAFE"

        critical_stress = min([
            results.critical_stress_longitudinal,
            results.critical_stress_transverse,
            results.critical_stress_shear
        ])

        bc_name = bc.value.replace('_', ' ').title()
        print(f"{bc_name:<20} {max_usage:<12.3f} {status:<8} {critical_stress/1e6:<12.1f}")


def example_4_multi_plate_analysis():
    """Example 4: Multi-plate analysis using legacy data."""

    print("\n" + "=" * 60)
    print("EXAMPLE 4: Multi-Plate Analysis (Legacy Data)")
    print("=" * 60)

    # Create analyzer with legacy plate data
    analyzer = create_legacy_multi_plate_analyzer()

    print(f"Analyzing {analyzer.get_plate_count()} plates from legacy data...")

    # Run analysis
    results = analyzer.analyze_all_plates()

    # Display summary
    print(f"\nMulti-Plate Analysis Summary:")
    print(f"Overall Status: {results.overall_status}")
    print(f"Total Plates: {results.summary_statistics['total_plate_count']}")
    print(f"Critical Plates: {results.summary_statistics['critical_plate_count']}")

    print(f"\nStatistics:")
    print(f"  Max Usage Factor: {results.summary_statistics['max_usage_factor']:.3f}")
    print(f"  Avg Usage Factor: {results.summary_statistics['avg_usage_factor']:.3f}")
    print(f"  Min Usage Factor: {results.summary_statistics['min_usage_factor']:.3f}")

    # Show plate ranking
    print(f"\nPlate Ranking (by Usage Factor):")
    ranking = analyzer.get_plate_ranking("usage_factor")
    for i, (plate_id, usage_factor) in enumerate(ranking, 1):
        status = "UNSAFE" if plate_id in results.critical_plates else "SAFE"
        print(f"  {i}. {plate_id}: {usage_factor:.3f} ({status})")

    # Export results
    output_dir = Path(__file__).parent / "output"
    output_dir.mkdir(exist_ok=True)

    csv_file = output_dir / "multi_plate_results.csv"
    report_file = output_dir / "multi_plate_summary.txt"

    analyzer.export_results_to_csv(csv_file)
    analyzer.export_summary_report(report_file)

    print(f"\nResults exported to:")
    print(f"  CSV: {csv_file}")
    print(f"  Summary: {report_file}")

    return analyzer, results


def example_5_calculation_components():
    """Example 5: Using individual calculation components."""

    print("\n" + "=" * 60)
    print("EXAMPLE 5: Individual Calculation Components")
    print("=" * 60)

    # Material and geometric properties
    E = 210e9    # Pa
    v = 0.3
    fy = 355e6   # Pa
    t = 0.015    # m
    b = 0.8      # m
    l = 2.5      # m

    print(f"Plate: {l}m × {b}m × {t*1000}mm, Steel fy={fy/1e6:.0f} MPa")

    # 1. Elastic buckling calculations
    print(f"\n1. Elastic Buckling Calculations:")
    calc = ElasticBucklingCalculator()

    elastic_stresses = calc.calculate_all_elastic_buckling_stresses(
        E, v, t, b, l, PlateEdgeCondition.SIMPLY_SUPPORTED)

    for direction, stress in elastic_stresses.items():
        print(f"  σcr_{direction}: {stress/1e6:.1f} MPa")

    # 2. Slenderness ratios
    print(f"\n2. Slenderness Ratios:")
    slenderness_calc = SlendernessCalculator()

    for direction, stress in elastic_stresses.items():
        char_resistance = fy if direction != 'shear' else fy/1.732
        slenderness = slenderness_calc.calculate_slenderness_ratio(char_resistance, stress)
        print(f"  λ_{direction}: {slenderness:.3f}")

    # 3. Ultimate strength calculations
    print(f"\n3. Ultimate Strength Calculations:")
    ultimate_calc = UltimateStrengthCalculator()

    # DNV resistances
    sigma_xrd = ultimate_calc.calculate_dnv_longitudinal_resistance(fy, E, t, b)
    sigma_yrd = ultimate_calc.calculate_dnv_transverse_resistance(fy, E, t, b, l)
    tau_rd = ultimate_calc.calculate_dnv_shear_resistance(fy, E, t, b, l)

    print(f"  σx,Rd: {sigma_xrd/1e6:.1f} MPa")
    print(f"  σy,Rd: {sigma_yrd/1e6:.1f} MPa")
    print(f"  τRd:   {tau_rd/1e6:.1f} MPa")

    # 4. Usage factors for example loading
    print(f"\n4. Usage Factors (for example loading):")
    applied_stresses = {
        'longitudinal': 60e6,  # 60 MPa
        'transverse': 40e6,    # 40 MPa
        'shear': 25e6          # 25 MPa
    }

    design_resistances = {
        'longitudinal': sigma_xrd,
        'transverse': sigma_yrd,
        'shear': tau_rd
    }

    usage_calc = UsageFactorCalculator()
    usage_factors = usage_calc.calculate_all_usage_factors(applied_stresses, design_resistances)

    for direction, factor in usage_factors.items():
        print(f"  η_{direction}: {factor:.3f}")


def example_6_legacy_compatibility():
    """Example 6: Legacy calculation compatibility test."""

    print("\n" + "=" * 60)
    print("EXAMPLE 6: Legacy Calculation Compatibility")
    print("=" * 60)

    # Original PlateBuckling_212 data
    legacy_data = {
        'YoungsModulus': 30000000,      # psi
        'PoissionsRatio': 0.3,
        'constantvalueTable1': 0.425,
        'PlateThickness': 0.552,        # in
        'PlateBreadth': 27.6,           # in
        'PlateLength': 104.04,          # in
        'TangentModulus': 25000000,     # psi
        'YieldPoint': 33000             # psi
    }

    print("Testing legacy PlateBuckling_212 calculations...")

    # Run legacy calculation
    legacy_results = calculate_plate_buckling_212(legacy_data)

    print(f"\nLegacy Calculation Results:")
    for key, value in legacy_results.items():
        if 'stress' in key.lower():
            print(f"  {key}: {value:.0f} psi")
        else:
            print(f"  {key}: {value:.4f}")

    # Compare with new system
    print(f"\nUsing new system for comparison...")

    # Convert to SI units for new system
    psi_to_pa = 6894.76
    in_to_m = 0.0254

    plate_props = PlateProperties(
        length=legacy_data['PlateLength'] * in_to_m,
        breadth=legacy_data['PlateBreadth'] * in_to_m,
        thickness=legacy_data['PlateThickness'] * in_to_m,
        youngs_modulus=legacy_data['YoungsModulus'] * psi_to_pa,
        poisson_ratio=legacy_data['PoissionsRatio'],
        yield_strength=legacy_data['YieldPoint'] * psi_to_pa,
        length_unit="m",
        stress_unit="Pa"
    )

    # Calculate using new elastic buckling calculator
    calc = ElasticBucklingCalculator()
    elastic_stresses = calc.calculate_all_elastic_buckling_stresses(
        plate_props.youngs_modulus,
        plate_props.poisson_ratio,
        plate_props.thickness,
        plate_props.breadth,
        plate_props.length
    )

    print(f"\nNew System Results (converted to psi):")
    print(f"  Longitudinal buckling stress: {elastic_stresses['longitudinal']/psi_to_pa:.0f} psi")
    print(f"  Transverse buckling stress:   {elastic_stresses['transverse']/psi_to_pa:.0f} psi")
    print(f"  Shear buckling stress:        {elastic_stresses['shear']/psi_to_pa:.0f} psi")


def create_example_input_files():
    """Create example input files for plate capacity analysis."""

    print("\n" + "=" * 60)
    print("Creating Example Input Files")
    print("=" * 60)

    output_dir = Path(__file__).parent / "input_templates"
    output_dir.mkdir(exist_ok=True)

    # 1. Single plate JSON template
    single_plate_template = {
        "plate_analysis": {
            "description": "Single plate buckling analysis template",
            "properties": {
                "length": 3.0,
                "breadth": 0.8,
                "thickness": 0.012,
                "youngs_modulus": 210e9,
                "poisson_ratio": 0.3,
                "yield_strength": 355e6,
                "water_depth": 50.0,
                "length_unit": "m",
                "stress_unit": "Pa"
            },
            "loads": {
                "longitudinal_stress": 50e6,
                "transverse_stress": 30e6,
                "shear_stress": 20e6
            },
            "constants": {
                "material_factor": 1.15
            },
            "boundary_condition": "simply_supported"
        }
    }

    # 2. Multi-plate JSON template
    multi_plate_template = {
        "multi_plate_analysis": {
            "description": "Multi-plate analysis template",
            "plates": [
                {
                    "plate_id": "Deck_Panel_1",
                    "description": "Main deck panel",
                    "properties": {
                        "length": 3.0,
                        "breadth": 0.8,
                        "thickness": 0.012,
                        "youngs_modulus": 210e9,
                        "poisson_ratio": 0.3,
                        "yield_strength": 355e6,
                        "water_depth": 0.0,
                        "length_unit": "m",
                        "stress_unit": "Pa"
                    },
                    "loads": {
                        "longitudinal_stress": 60e6,
                        "transverse_stress": 40e6,
                        "shear_stress": 25e6
                    },
                    "constants": {
                        "material_factor": 1.15
                    }
                },
                {
                    "plate_id": "Bottom_Panel_1",
                    "description": "Bottom panel under pressure",
                    "properties": {
                        "length": 2.5,
                        "breadth": 0.7,
                        "thickness": 0.015,
                        "youngs_modulus": 210e9,
                        "poisson_ratio": 0.3,
                        "yield_strength": 275e6,
                        "water_depth": 50.0,
                        "length_unit": "m",
                        "stress_unit": "Pa"
                    },
                    "loads": {
                        "longitudinal_stress": 45e6,
                        "transverse_stress": 80e6,
                        "shear_stress": 15e6
                    },
                    "constants": {
                        "material_factor": 1.15
                    }
                }
            ]
        }
    }

    # Save JSON templates
    single_file = output_dir / "single_plate_template.json"
    multi_file = output_dir / "multi_plate_template.json"

    with open(single_file, 'w') as f:
        json.dump(single_plate_template, f, indent=2)

    with open(multi_file, 'w') as f:
        json.dump(multi_plate_template, f, indent=2)

    # 3. CSV template for batch processing
    csv_template_content = """plate_id,description,length,breadth,thickness,youngs_modulus,yield_strength,longitudinal_stress,transverse_stress,shear_stress
Panel_A,Deck panel section A,3.0,0.8,0.012,210e9,355e6,50e6,30e6,20e6
Panel_B,Side panel section B,2.5,0.7,0.010,210e9,275e6,40e6,60e6,25e6
Panel_C,Bottom panel section C,2.8,0.9,0.015,210e9,355e6,70e6,45e6,30e6"""

    csv_file = output_dir / "batch_analysis_template.csv"
    with open(csv_file, 'w') as f:
        f.write(csv_template_content)

    print(f"Created input templates:")
    print(f"  Single plate: {single_file}")
    print(f"  Multi-plate:  {multi_file}")
    print(f"  CSV batch:    {csv_file}")

    return output_dir


def main():
    """Run all examples."""

    print("PLATE CAPACITY ANALYSIS EXAMPLES")
    print("=" * 60)
    print("Demonstrating various analysis capabilities")

    try:
        # Run examples
        example_1_basic_plate_analysis()
        example_2_parametric_study()
        example_3_boundary_condition_comparison()
        example_4_multi_plate_analysis()
        example_5_calculation_components()
        example_6_legacy_compatibility()

        # Create input templates
        create_example_input_files()

        print("\n" + "=" * 60)
        print("ALL EXAMPLES COMPLETED SUCCESSFULLY")
        print("=" * 60)

    except Exception as e:
        logger.error(f"Example execution failed: {e}")
        raise


if __name__ == "__main__":
    main()