#!/usr/bin/env python3
"""
Fatigue Analysis Examples
========================

This module demonstrates the comprehensive fatigue analysis capabilities of the
Digital Model fatigue module, including migration from legacy code patterns.

Examples include:
- Basic S-N curve usage and comparisons
- Multi-slope S-N curve analysis (migrated from legacy)
- Time-domain fatigue analysis with rainflow counting
- Frequency-domain spectral fatigue analysis
- High stress range life calculations (legacy LinearSlopeCal functionality)
- Shear data analysis (legacy Shear7dataCal functionality)
- Engineering validation and reporting

Author: Digital Model Team
Version: 2.0.0
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
import logging

# Import the fatigue analysis module
from digitalmodel.fatigue import (
    # Main analysis engine
    FatigueAnalysisEngine,
    FatigueAnalysisConfig,
    MultislopeSNCurve,

    # S-N curves
    get_dnv_curve,
    get_api_curve,
    StandardSNCurves,
    MaterialProperties,

    # Quick analysis functions
    quick_time_domain_analysis,
    quick_frequency_domain_analysis,

    # Individual components
    LinearDamageAccumulation,
    rainflow_count
)

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


def example_1_basic_sn_curves():
    """
    Example 1: Basic S-N curve usage and comparisons

    Demonstrates standard curve libraries and plotting capabilities.
    """
    print("="*60)
    print("EXAMPLE 1: Basic S-N Curve Usage")
    print("="*60)

    # Create standard curves from different codes
    dnv_d = get_dnv_curve('D')
    dnv_f = get_dnv_curve('F')
    api_x = get_api_curve('X')

    print(f"DNV-D Curve: {dnv_d.name}")
    print(f"Fatigue limit: {dnv_d.fatigue_limit:.2f} MPa")
    print()

    # Calculate life for specific stress ranges
    stress_ranges = [100, 80, 60, 40, 20]

    print("Stress Range (MPa) | DNV-D Cycles | DNV-F Cycles | API-X Cycles")
    print("-" * 60)

    for stress in stress_ranges:
        dnv_d_cycles = dnv_d.get_allowable_cycles(stress)
        dnv_f_cycles = dnv_f.get_allowable_cycles(stress)
        api_x_cycles = api_x.get_allowable_cycles(stress)

        print(f"{stress:15.0f} | {dnv_d_cycles:10.2e} | {dnv_f_cycles:10.2e} | {api_x_cycles:10.2e}")

    print()

    # List available curves
    available_curves = StandardSNCurves.list_curves()
    print("Available S-N Curve Standards:")
    for standard, curves in available_curves.items():
        print(f"  {standard}: {curves}")

    print("\nExample 1 completed!\n")


def example_2_multislope_curves():
    """
    Example 2: Multi-slope S-N curve analysis

    Demonstrates the multi-slope functionality migrated from legacy code.
    """
    print("="*60)
    print("EXAMPLE 2: Multi-slope S-N Curves (Legacy Migration)")
    print("="*60)

    # Create a multi-slope curve (similar to legacy FatigueBasiccurve.py)
    multislope_curve = MultislopeSNCurve(
        name="Custom Multi-slope",
        slopes=[3.0, 3.5, 5.0],
        constants=[1e12, 5e11, 1e10],
        transition_cycles=[1e6, 5e6],
        fatigue_limit=30.0
    )

    print(f"Multi-slope curve: {multislope_curve.name}")
    print(f"Slopes: {multislope_curve.slopes}")
    print(f"Constants: {multislope_curve.constants}")
    print(f"Transition cycles: {multislope_curve.transition_cycles}")
    print(f"Fatigue limit: {multislope_curve.fatigue_limit} MPa")
    print()

    # Compare with single-slope curve
    single_slope = get_dnv_curve('D')

    stress_ranges = np.logspace(1, 2.5, 10)  # 10 to 300 MPa

    print("Stress (MPa) | Multi-slope Cycles | Single-slope Cycles | Ratio")
    print("-" * 65)

    for stress in stress_ranges:
        multi_cycles = multislope_curve.get_allowable_cycles(stress)
        single_cycles = single_slope.get_allowable_cycles(stress)
        ratio = multi_cycles / single_cycles if single_cycles > 0 else 1.0

        print(f"{stress:10.1f} | {multi_cycles:15.2e} | {single_cycles:16.2e} | {ratio:6.2f}")

    print("\nExample 2 completed!\n")


def example_3_time_domain_analysis():
    """
    Example 3: Complete time-domain fatigue analysis

    Demonstrates rainflow counting, damage calculation, and validation.
    """
    print("="*60)
    print("EXAMPLE 3: Time-Domain Fatigue Analysis")
    print("="*60)

    # Generate realistic stress time series
    np.random.seed(42)
    time = np.linspace(0, 3600, 36000)  # 1 hour at 10 Hz

    # Multi-component stress signal
    stress = (50 * np.sin(0.1 * time) +           # Low frequency wave
              30 * np.sin(0.5 * time) +           # Medium frequency
              20 * np.sin(2.0 * time) +           # High frequency
              15 * np.random.randn(len(time)))     # Random noise

    print(f"Generated stress time series:")
    print(f"  Duration: {time[-1]:.0f} seconds")
    print(f"  Sample rate: {len(time)/(time[-1]-time[0]):.1f} Hz")
    print(f"  Stress range: {np.min(stress):.1f} to {np.max(stress):.1f} MPa")
    print(f"  RMS stress: {np.sqrt(np.mean(stress**2)):.1f} MPa")
    print()

    # Quick analysis
    result = quick_time_domain_analysis(stress, sn_standard='DNV', sn_curve_class='D')

    print("QUICK ANALYSIS RESULTS:")
    print(f"  S-N Curve: {result['sn_curve_info']['name']}")
    print(f"  Total Damage: {result['damage_results']['total_damage']:.6f}")
    print(f"  Safety Factor: {result['damage_results']['safety_factor']:.2f}")
    print(f"  Life Fraction Used: {result['damage_results']['life_fraction_used']:.1%}")
    print(f"  Total Cycles: {result['rainflow_results']['statistics']['total_cycles']:.0f}")
    print(f"  Unique Stress Ranges: {result['rainflow_results']['statistics']['unique_ranges']}")
    print()

    # Detailed analysis with custom configuration
    config = FatigueAnalysisConfig(
        sn_standard='DNV',
        sn_curve_class='D',
        damage_method='modified_miners',
        mean_stress_correction='goodman',
        rainflow_gate_value=0.02,
        safety_factor_target=3.0,
        validation_checks=True
    )

    engine = FatigueAnalysisEngine(config)
    detailed_result = engine.analyze_time_series(stress, time)

    print("DETAILED ANALYSIS RESULTS:")
    print(f"  Damage Method: {detailed_result['damage_results']['method']}")
    print(f"  Total Damage: {detailed_result['damage_results']['total_damage']:.6f}")
    print(f"  Safety Factor: {detailed_result['damage_results']['safety_factor']:.2f}")

    # Show validation results
    validation = detailed_result['validation']
    if validation.warnings:
        print("\n  VALIDATION WARNINGS:")
        for warning in validation.warnings:
            print(f"    - {warning}")

    if validation.recommendations:
        print("\n  RECOMMENDATIONS:")
        for rec in validation.recommendations:
            print(f"    - {rec}")

    print("\nExample 3 completed!\n")


def example_4_frequency_domain_analysis():
    """
    Example 4: Frequency-domain spectral fatigue analysis

    Demonstrates PSD-based fatigue calculation using various methods.
    """
    print("="*60)
    print("EXAMPLE 4: Frequency-Domain Analysis")
    print("="*60)

    # Create synthetic PSD
    frequency = np.linspace(0.01, 10, 1000)

    # Multi-peak PSD representing typical offshore structure response
    psd = (20 * np.exp(-((frequency - 0.1)/0.05)**2) +     # Low frequency peak
           10 * np.exp(-((frequency - 0.8)/0.2)**2) +      # Medium frequency peak
           5 * np.exp(-((frequency - 3.0)/0.5)**2) +       # High frequency peak
           1.0)                                            # Background noise

    duration = 3600.0  # 1 hour analysis

    print(f"PSD characteristics:")
    print(f"  Frequency range: {frequency[0]:.3f} to {frequency[-1]:.1f} Hz")
    print(f"  Peak frequency: {frequency[np.argmax(psd)]:.2f} Hz")
    print(f"  RMS stress: {np.sqrt(np.trapz(psd, frequency)):.2f} MPa")
    print(f"  Analysis duration: {duration:.0f} seconds")
    print()

    # Quick frequency domain analysis
    freq_result = quick_frequency_domain_analysis(
        psd, frequency, duration,
        sn_standard='DNV', sn_curve_class='D',
        frequency_method='dirlik'
    )

    print("FREQUENCY DOMAIN RESULTS:")
    print(f"  Method: {freq_result['frequency_results']['method']}")
    print(f"  Damage Rate: {freq_result['frequency_results']['damage_rate']:.2e} /sec")
    print(f"  Total Damage: {freq_result['damage_results']['total_damage']:.6f}")
    print(f"  Safety Factor: {freq_result['damage_results']['safety_factor']:.2f}")
    print(f"  Equivalent Cycles/sec: {freq_result['damage_results']['equivalent_cycles_per_sec']:.2f}")
    print()

    # Compare different frequency domain methods
    methods = ['dirlik', 'tovo_benasciutti', 'narrow_band']

    print("METHOD COMPARISON:")
    print("Method            | Damage Rate   | Total Damage  | Safety Factor")
    print("-" * 65)

    for method in methods:
        try:
            result = quick_frequency_domain_analysis(
                psd, frequency, duration,
                frequency_method=method
            )
            damage_rate = result['frequency_results']['damage_rate']
            total_damage = result['damage_results']['total_damage']
            safety_factor = result['damage_results']['safety_factor']

            print(f"{method:16s} | {damage_rate:11.2e} | {total_damage:11.6f} | {safety_factor:11.2f}")
        except Exception as e:
            print(f"{method:16s} | Error: {str(e)}")

    print("\nExample 4 completed!\n")


def example_5_high_stress_range_analysis():
    """
    Example 5: High stress range life calculations

    Recreates functionality from legacy LinearSlopeCal.py module.
    """
    print("="*60)
    print("EXAMPLE 5: High Stress Range Analysis (Legacy LinearSlopeCal)")
    print("="*60)

    # Configure analysis engine
    config = FatigueAnalysisConfig(sn_standard='DNV', sn_curve_class='D')
    engine = FatigueAnalysisEngine(config)

    # Stress ranges from legacy code
    stress_ranges = [900, 500, 350, 300, 250, 200]

    print("High stress range fatigue life calculations:")
    print("Stress Range (MPa) | Allowable Cycles | Design Cycles* | Years at 1Hz")
    print("-" * 70)

    results = engine.calculate_high_stress_range_life(stress_ranges)

    for stress in stress_ranges:
        key = f"{stress}_MPa"
        if key in results:
            data = results[key]
            allowable_cycles = data['allowable_cycles']
            design_cycles = data['design_cycles']
            years_at_1hz = allowable_cycles / (365.25 * 24 * 3600)  # Convert to years

            print(f"{stress:15.0f} | {allowable_cycles:14.2e} | {design_cycles:12.2e} | {years_at_1hz:10.2f}")

    print("* Design cycles include safety factor of 2.0")
    print()

    # Show curve characteristics
    curve = engine.sn_curve
    print(f"S-N Curve: {curve.name}")
    print(f"Fatigue limit: {curve.fatigue_limit:.2f} MPa")
    print(f"All stress ranges above fatigue limit: {all(s > curve.fatigue_limit for s in stress_ranges)}")

    print("\nExample 5 completed!\n")


def example_6_shear_data_analysis():
    """
    Example 6: Shear data analysis

    Recreates functionality from legacy Shear7dataCal.py module.
    """
    print("="*60)
    print("EXAMPLE 6: Shear Data Analysis (Legacy Shear7dataCal)")
    print("="*60)

    # Configure analysis engine
    config = FatigueAnalysisConfig(sn_standard='DNV', sn_curve_class='D')
    engine = FatigueAnalysisEngine(config)

    # Perform shear analysis (legacy patterns)
    high_stress = 1000.0
    low_stress_range = (1.0, 16.0)

    shear_results = engine.analyze_shear_data(high_stress, low_stress_range)

    print("SHEAR DATA ANALYSIS RESULTS:")
    print()

    # High stress analysis
    high_analysis = shear_results['high_stress_analysis']
    print(f"High stress analysis (legacy high stress range calculation):")
    print(f"  Stress range: {high_analysis['stress_range']:.0f} MPa")
    print(f"  Allowable cycles: {high_analysis['allowable_cycles']:.2e}")
    print(f"  Finite cycles: {high_analysis['is_finite']}")
    print()

    # Low stress analysis
    low_analysis = shear_results['low_stress_analysis']
    print("Low stress analysis (legacy low stress range sweep):")
    print("Stress (MPa) | Allowable Cycles | Finite")
    print("-" * 40)

    for data in low_analysis[:10]:  # Show first 10 points
        stress = data['stress']
        cycles = data['cycles']
        finite = data['is_finite']

        if finite:
            print(f"{stress:10.1f} | {cycles:14.2e} | {finite}")
        else:
            print(f"{stress:10.1f} | {'Infinite':>14s} | {finite}")

    print("...")
    print()

    # Curve characteristics
    curve_info = shear_results['curve_characteristics']
    print(f"S-N Curve characteristics:")
    print(f"  Name: {curve_info['curve_name']}")
    print(f"  Fatigue limit: {curve_info['fatigue_limit']:.2f} MPa")

    print("\nExample 6 completed!\n")


def example_7_comprehensive_reporting():
    """
    Example 7: Comprehensive analysis and reporting

    Demonstrates complete workflow with validation and reporting.
    """
    print("="*60)
    print("EXAMPLE 7: Comprehensive Analysis and Reporting")
    print("="*60)

    # Generate complex stress history
    np.random.seed(123)
    time = np.linspace(0, 7200, 72000)  # 2 hours at 10 Hz

    # Combine multiple load cases
    operational_load = 40 * np.sin(0.05 * time)
    storm_load = 80 * np.exp(-((time - 3600)/1800)**2) * np.sin(0.2 * time)
    fatigue_load = 25 * np.sin(0.8 * time) + 15 * np.sin(3.0 * time)
    random_load = 20 * np.random.randn(len(time))

    total_stress = operational_load + storm_load + fatigue_load + random_load

    # Configure comprehensive analysis
    config = FatigueAnalysisConfig(
        sn_standard='DNV',
        sn_curve_class='D',
        thickness=32.0,  # Include thickness effects
        damage_method='modified_miners',
        mean_stress_correction='goodman',
        safety_factor_target=2.5,
        validation_checks=True,
        detailed_output=True
    )

    # Perform analysis
    engine = FatigueAnalysisEngine(config)
    result = engine.analyze_time_series(total_stress, time)

    # Generate and display report
    report = engine.generate_report(result)
    print(report)

    # Additional statistics
    stats = result['analysis_statistics']
    print("\nADDITIONAL ANALYSIS STATISTICS:")
    print("-" * 40)

    if 'stress_statistics' in stats:
        stress_stats = stats['stress_statistics']
        print(f"Stress Statistics:")
        print(f"  Skewness: {stress_stats['skewness']:.3f}")
        print(f"  Kurtosis: {stress_stats['kurtosis']:.3f}")

    if 'cycle_statistics' in stats:
        cycle_stats = stats['cycle_statistics']
        print(f"Cycle Statistics:")
        print(f"  Most damaging range: {cycle_stats['most_damaging_range']:.1f} MPa")
        print(f"  Mean cycle range: {cycle_stats['mean_range']:.1f} MPa")

    if 'damage_distribution' in stats:
        damage_stats = stats['damage_distribution']
        print(f"Damage Distribution:")
        print(f"  Max single contribution: {damage_stats['max_contribution']:.2e}")
        print(f"  Top 10% contribution: {damage_stats['top_10_percent_contribution']:.2e}")

    print("\nExample 7 completed!\n")


def run_all_examples():
    """Run all fatigue analysis examples"""
    print("FATIGUE ANALYSIS EXAMPLES")
    print("Digital Model - Legacy Code Migration")
    print("=" * 80)
    print()

    try:
        example_1_basic_sn_curves()
        example_2_multislope_curves()
        example_3_time_domain_analysis()
        example_4_frequency_domain_analysis()
        example_5_high_stress_range_analysis()
        example_6_shear_data_analysis()
        example_7_comprehensive_reporting()

        print("=" * 80)
        print("ALL EXAMPLES COMPLETED SUCCESSFULLY!")
        print("=" * 80)

    except Exception as e:
        print(f"ERROR in examples: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    # Run all examples
    run_all_examples()