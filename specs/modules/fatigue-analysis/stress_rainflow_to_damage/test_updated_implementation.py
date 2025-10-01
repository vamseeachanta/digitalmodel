#!/usr/bin/env python3
"""
Test the UPDATED two-segment S-N curve implementation
Verifies that run_damage_analysis.py now properly implements:
1. Two-segment curve (no fatigue endurance limit by default)
2. Optional fatigue endurance limit via config
"""

import sys
sys.path.insert(0, '.')

from run_damage_analysis import DamageAnalyzer
import numpy as np
import matplotlib.pyplot as plt

def test_implementation():
    """Test the updated calculate_cycles_to_failure method"""

    print("="*80)
    print("TESTING UPDATED TWO-SEGMENT IMPLEMENTATION")
    print("="*80)

    # Initialize analyzer with test config
    analyzer = DamageAnalyzer('input/damage_analysis_config_test.yml')

    # Test stress ranges
    test_stresses = [30, 40, 45, 47, 48, 50, 60, 80, 100, 150]

    print(f"\nConfiguration:")
    print(f"  ignore_below_fatigue_limit = {analyzer.config['damage_calculation']['options']['ignore_below_fatigue_limit']}")
    print(f"  two_segment enabled = {analyzer.config['sn_curve']['parameters']['two_segment']['enabled']}")
    print(f"  fatigue_limit_stress = {float(analyzer.config['sn_curve']['parameters']['fatigue_limit_stress'])} MPa")
    print(f"  transition_cycles = {float(analyzer.config['sn_curve']['parameters']['fatigue_limit_cycles']):.0e}")

    print(f"\n{'Stress (MPa)':<15} {'Cycles to Failure':<20} {'Segment Used':<15} {'Status'}")
    print("-"*70)

    for stress in test_stresses:
        N = analyzer.calculate_cycles_to_failure(stress)

        # Determine which segment
        params = analyzer.config['sn_curve']['parameters']
        log_N1 = float(params['log_a']) - float(params['m']) * np.log10(stress)
        N1 = 10 ** log_N1
        transition = float(params['fatigue_limit_cycles'])

        if N == float('inf'):
            segment = "N/A"
            status = "Infinite Life"
        elif N1 >= transition:
            segment = "Segment 2"
            status = "Low Stress"
        else:
            segment = "Segment 1"
            status = "High Stress"

        if N == float('inf'):
            print(f"{stress:<15.1f} {'Infinite':<20} {segment:<15} {status}")
        else:
            print(f"{stress:<15.1f} {N:<20.2e} {segment:<15} {status}")

    # Verification tests
    print(f"\n{'='*80}")
    print("VERIFICATION TESTS")
    print("="*80)

    # Test 1: Two-segment transition
    stress_at_transition = 47.03  # Approximately where N = 10^7
    N_at_trans = analyzer.calculate_cycles_to_failure(stress_at_transition)
    print(f"\nTest 1: Transition Point (Stress = {stress_at_transition} MPa)")
    print(f"  Cycles to failure: {N_at_trans:.2e}")
    print(f"  Expected: ~10^7 cycles")
    if 9e6 < N_at_trans < 11e6:
        print(f"  [PASS] PASS - Using Segment 2 at transition")
    else:
        print(f"  [FAIL] FAIL - Unexpected result")

    # Test 2: Low stress uses Segment 2
    low_stress = 45.0  # Below transition stress
    N_low = analyzer.calculate_cycles_to_failure(low_stress)
    params = analyzer.config['sn_curve']['parameters']
    log_N1_low = float(params['log_a']) - float(params['m']) * np.log10(low_stress)
    N1_low = 10 ** log_N1_low

    print(f"\nTest 2: Low Stress (Stress = {low_stress} MPa)")
    print(f"  Segment 1 would give: {N1_low:.2e} cycles")
    print(f"  Actual result: {N_low:.2e} cycles" if N_low != float('inf') else f"  Actual result: Infinite")
    if N1_low >= float(params['fatigue_limit_cycles']):
        print(f"  Expected: Should use Segment 2")
        if N_low != float('inf'):
            two_seg = params['two_segment']
            log_N2_expected = float(two_seg['log_c']) - float(two_seg['r']) * np.log10(low_stress)
            N2_expected = 10 ** log_N2_expected
            if abs(N_low - N2_expected) / N2_expected < 0.01:
                print(f"  [PASS] PASS - Using Segment 2")
            else:
                print(f"  [FAIL] FAIL - Not using Segment 2 correctly")
        else:
            print(f"  [FAIL] FAIL - Returning infinite life (should use Segment 2)")
    else:
        print(f"  Expected: Should use Segment 1")

    # Test 3: High stress uses Segment 1
    high_stress = 80.0
    N_high = analyzer.calculate_cycles_to_failure(high_stress)
    log_N1_high = float(params['log_a']) - float(params['m']) * np.log10(high_stress)
    N1_high = 10 ** log_N1_high

    print(f"\nTest 3: High Stress (Stress = {high_stress} MPa)")
    print(f"  Expected (Segment 1): {N1_high:.2e} cycles")
    print(f"  Actual result: {N_high:.2e} cycles")
    if abs(N_high - N1_high) / N1_high < 0.01:
        print(f"  [PASS] PASS - Using Segment 1")
    else:
        print(f"  [FAIL] FAIL - Not using Segment 1 correctly")

    # Test 4: No fatigue endurance limit (all stresses contribute)
    print(f"\nTest 4: No Fatigue Endurance Limit")
    print(f"  Config: ignore_below_fatigue_limit = {analyzer.config['damage_calculation']['options']['ignore_below_fatigue_limit']}")
    very_low_stress = 40.0
    N_very_low = analyzer.calculate_cycles_to_failure(very_low_stress)
    if N_very_low != float('inf'):
        print(f"  Stress = {very_low_stress} MPa gives N = {N_very_low:.2e}")
        print(f"  [PASS] PASS - All stresses contribute to damage (no endurance limit)")
    else:
        print(f"  Stress = {very_low_stress} MPa gives N = Infinite")
        print(f"  [FAIL] FAIL - Should not have infinite life when ignore_below_fatigue_limit = false")

    print(f"\n{'='*80}")
    print("IMPLEMENTATION STATUS")
    print("="*80)

    # Count which tests passed
    if (9e6 < analyzer.calculate_cycles_to_failure(47.03) < 11e6 and
        analyzer.calculate_cycles_to_failure(80.0) < 3e6 and
        analyzer.calculate_cycles_to_failure(40.0) != float('inf')):
        print("\n[PASS] SUCCESS: Two-segment S-N curve is correctly implemented!")
        print("  - Segment 1 used for high stress (N < 10^7)")
        print("  - Segment 2 used for low stress (N >= 10^7)")
        print("  - No fatigue endurance limit (all stress contributes)")
        return True
    else:
        print("\n[FAIL] FAILURE: Implementation has issues")
        return False

if __name__ == "__main__":
    success = test_implementation()
    sys.exit(0 if success else 1)
