#!/usr/bin/env python3
"""
Basic Fatigue Module Validation Test
===================================

A simple test to validate that the migrated fatigue module works correctly
without requiring external dependencies beyond numpy and pandas.

Author: Digital Model Team
"""

import sys
import os
import traceback
import numpy as np
import pandas as pd

# Add the src path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

def test_basic_sn_curve():
    """Test basic S-N curve functionality"""
    print("Testing S-N curve functionality...")

    try:
        from digitalmodel.fatigue.sn_curves import get_dnv_curve, PowerLawSNCurve

        # Test DNV curve
        curve = get_dnv_curve('D')
        print(f"  ✓ Created DNV-D curve: {curve.name}")
        print(f"    Fatigue limit: {curve.fatigue_limit:.2f} MPa")

        # Test calculations
        stress = 100.0
        cycles = curve.get_allowable_cycles(stress)
        print(f"    Stress {stress} MPa → {cycles:.2e} cycles")

        # Test power law curve directly
        power_curve = PowerLawSNCurve(
            name="Test Curve",
            A=1e12,
            m=3.0,
            fatigue_limit=50.0
        )

        cycles2 = power_curve.get_allowable_cycles(stress)
        print(f"  ✓ Power law curve: {stress} MPa → {cycles2:.2e} cycles")

        return True

    except Exception as e:
        print(f"  ✗ S-N curve test failed: {e}")
        traceback.print_exc()
        return False


def test_multislope_curve():
    """Test multi-slope S-N curve functionality"""
    print("Testing multi-slope S-N curve...")

    try:
        from digitalmodel.fatigue.analysis import MultislopeSNCurve

        # Create multi-slope curve
        curve = MultislopeSNCurve(
            name="Test Multi-slope",
            slopes=[3.0, 4.0],
            constants=[1e12, 1e11],
            transition_cycles=[1e6],
            fatigue_limit=30.0
        )

        print(f"  ✓ Created multi-slope curve: {curve.name}")

        # Test high stress (first slope)
        high_stress = 150.0
        high_cycles = curve.get_allowable_cycles(high_stress)
        print(f"    High stress: {high_stress} MPa → {high_cycles:.2e} cycles")

        # Test low stress (second slope)
        low_stress = 60.0
        low_cycles = curve.get_allowable_cycles(low_stress)
        print(f"    Low stress: {low_stress} MPa → {low_cycles:.2e} cycles")

        # Test below fatigue limit
        very_low_stress = 20.0
        very_low_cycles = curve.get_allowable_cycles(very_low_stress)
        print(f"    Below limit: {very_low_stress} MPa → {'Infinite' if np.isinf(very_low_cycles) else f'{very_low_cycles:.2e}'} cycles")

        return True

    except Exception as e:
        print(f"  ✗ Multi-slope curve test failed: {e}")
        traceback.print_exc()
        return False


def test_damage_accumulation():
    """Test damage accumulation functionality"""
    print("Testing damage accumulation...")

    try:
        from digitalmodel.fatigue.damage_accumulation import LinearDamageAccumulation
        from digitalmodel.fatigue.sn_curves import get_dnv_curve

        # Create test data
        cycles = pd.DataFrame({
            'range': [100, 80, 60, 40],
            'mean': [0, 0, 0, 0],
            'count': [1000, 2000, 5000, 10000]
        })

        curve = get_dnv_curve('D')
        calculator = LinearDamageAccumulation()

        result = calculator.calculate_damage(cycles, curve)

        print(f"  ✓ Damage calculation completed")
        print(f"    Total damage: {result['total_damage']:.6f}")
        print(f"    Safety factor: {result['safety_factor']:.2f}")
        print(f"    Cycles processed: {result['cycles_processed']}")

        # Basic validations
        assert result['total_damage'] > 0
        assert result['safety_factor'] > 0
        assert result['cycles_processed'] == 18000

        return True

    except Exception as e:
        print(f"  ✗ Damage accumulation test failed: {e}")
        traceback.print_exc()
        return False


def test_analysis_engine():
    """Test the main analysis engine"""
    print("Testing analysis engine...")

    try:
        from digitalmodel.fatigue.analysis import FatigueAnalysisEngine, FatigueAnalysisConfig

        # Create configuration
        config = FatigueAnalysisConfig(
            sn_standard='DNV',
            sn_curve_class='D',
            validation_checks=True
        )

        engine = FatigueAnalysisEngine(config)
        print(f"  ✓ Created analysis engine with {engine.sn_curve.name}")

        # Test high stress range analysis (legacy LinearSlopeCal functionality)
        stress_ranges = [500, 300, 200]
        results = engine.calculate_high_stress_range_life(stress_ranges)

        print(f"  ✓ High stress range analysis:")
        for stress in stress_ranges:
            key = f"{stress}_MPa"
            if key in results:
                data = results[key]
                print(f"    {stress} MPa → {data['allowable_cycles']:.2e} cycles")

        # Test shear analysis (legacy Shear7dataCal functionality)
        shear_results = engine.analyze_shear_data(1000.0, (5.0, 15.0))
        print(f"  ✓ Shear analysis completed")
        print(f"    High stress (1000 MPa): {shear_results['high_stress_analysis']['allowable_cycles']:.2e} cycles")
        print(f"    Low stress points: {len(shear_results['low_stress_analysis'])}")

        return True

    except Exception as e:
        print(f"  ✗ Analysis engine test failed: {e}")
        traceback.print_exc()
        return False


def test_time_domain_simple():
    """Test simple time domain analysis without rainflow (if available)"""
    print("Testing simple time domain analysis...")

    try:
        # Generate simple stress history
        np.random.seed(42)
        stress = 50 * np.sin(np.linspace(0, 10, 1000)) + 20 * np.random.randn(1000)

        # Try to use damage accumulation directly
        from digitalmodel.fatigue.damage_accumulation import LinearDamageAccumulation
        from digitalmodel.fatigue.sn_curves import get_dnv_curve

        # Create simple cycle representation (mock rainflow)
        stress_ranges = np.abs(np.diff(stress))
        ranges_df = pd.DataFrame({
            'range': stress_ranges[stress_ranges > 10],  # Filter small ranges
            'count': 1,
            'mean': 0
        })

        # Group similar ranges
        ranges_df = ranges_df.groupby(pd.cut(ranges_df['range'], bins=20)).agg({
            'range': 'mean',
            'count': 'sum',
            'mean': 'mean'
        }).dropna()

        curve = get_dnv_curve('D')
        calculator = LinearDamageAccumulation()
        result = calculator.calculate_damage(ranges_df, curve)

        print(f"  ✓ Simple time domain analysis completed")
        print(f"    Input: {len(stress)} stress points")
        print(f"    Extracted: {len(ranges_df)} cycle bins")
        print(f"    Total damage: {result['total_damage']:.6f}")

        return True

    except Exception as e:
        print(f"  ✗ Time domain test failed: {e}")
        traceback.print_exc()
        return False


def run_all_tests():
    """Run all basic validation tests"""
    print("="*60)
    print("FATIGUE MODULE MIGRATION VALIDATION")
    print("="*60)
    print()

    tests = [
        test_basic_sn_curve,
        test_multislope_curve,
        test_damage_accumulation,
        test_analysis_engine,
        test_time_domain_simple
    ]

    results = []
    for test in tests:
        try:
            result = test()
            results.append(result)
            print()
        except Exception as e:
            print(f"Test {test.__name__} crashed: {e}")
            results.append(False)
            print()

    # Summary
    print("="*60)
    print("VALIDATION SUMMARY")
    print("="*60)

    passed = sum(results)
    total = len(results)

    for i, (test, result) in enumerate(zip(tests, results)):
        status = "PASS" if result else "FAIL"
        print(f"{i+1}. {test.__name__}: {status}")

    print()
    print(f"OVERALL RESULT: {passed}/{total} tests passed")

    if passed == total:
        print("✓ ALL TESTS PASSED - Migration successful!")
        return True
    else:
        print("✗ Some tests failed - Need investigation")
        return False


if __name__ == "__main__":
    success = run_all_tests()
    sys.exit(0 if success else 1)