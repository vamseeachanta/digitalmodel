#!/usr/bin/env python3
"""
Test and verify two-segment S-N curve implementation
Checks if the current implementation properly handles:
1. Fatigue limit
2. Two-segment curve transition
3. Comparison with single-segment approach
"""

import numpy as np
import matplotlib.pyplot as plt
import yaml

def calculate_cycles_single_segment(stress_range, log_a, m, fatigue_limit_stress):
    """Current implementation (single segment only)"""
    if stress_range <= fatigue_limit_stress:
        return float('inf')

    log_N = log_a - m * np.log10(stress_range)
    N = 10 ** log_N
    return N

def calculate_cycles_two_segment(stress_range, log_a, m, transition_cycles,
                                  log_c, r, fatigue_limit_stress):
    """Proper two-segment implementation"""
    if stress_range <= fatigue_limit_stress:
        return float('inf')

    # Segment 1: High stress (N < transition_cycles)
    log_N1 = log_a - m * np.log10(stress_range)
    N1 = 10 ** log_N1

    if N1 < transition_cycles:
        return N1
    else:
        # Segment 2: Low stress (N >= transition_cycles)
        log_N2 = log_c - r * np.log10(stress_range)
        N2 = 10 ** log_N2
        return N2

def main():
    # Load config
    config_path = 'input/damage_analysis_config_test.yml'
    with open(config_path, 'r') as f:
        config = yaml.safe_load(f)

    params = config['sn_curve']['parameters']
    two_seg = params['two_segment']

    print("="*80)
    print("TWO-SEGMENT S-N CURVE ANALYSIS")
    print("="*80)
    print(f"\nConfiguration: {config['sn_curve']['curve_type']} - {config['sn_curve']['environment']}")
    print(f"\nSegment 1 (High Stress):")
    print(f"  log_a = {params['log_a']}")
    print(f"  m = {params['m']}")
    print(f"  Transition cycles = {float(params['fatigue_limit_cycles']):,.0f}")
    print(f"  Fatigue limit stress = {float(params['fatigue_limit_stress'])} MPa")

    print(f"\nSegment 2 (Low Stress):")
    print(f"  log_c = {two_seg['log_c']}")
    print(f"  r = {two_seg['r']}")
    print(f"  Enabled = {two_seg['enabled']}")

    # Calculate transition stress (where N = transition_cycles)
    # From Segment 1: log(N) = log_a - m*log(S)
    # Solving for S: log(S) = (log_a - log(N)) / m
    transition_cycles = float(params['fatigue_limit_cycles'])
    log_transition_stress = (float(params['log_a']) - np.log10(transition_cycles)) / float(params['m'])
    transition_stress = 10 ** log_transition_stress

    print(f"\nCalculated Transition Point:")
    print(f"  Transition stress = {transition_stress:.2f} MPa")
    print(f"  Transition cycles = {transition_cycles:,.0f}")

    # Verify continuity at transition
    N1_at_transition = calculate_cycles_single_segment(transition_stress, params['log_a'],
                                                       params['m'], 0)  # Set limit to 0 for test
    N2_at_transition = 10 ** (two_seg['log_c'] - two_seg['r'] * np.log10(transition_stress))

    print(f"\nContinuity Check at Transition:")
    print(f"  Segment 1 gives N = {N1_at_transition:,.0f}")
    print(f"  Segment 2 gives N = {N2_at_transition:,.0f}")
    print(f"  Ratio (should be ~1.0) = {N1_at_transition/N2_at_transition:.4f}")

    # Test stress ranges
    stress_ranges = np.logspace(1, 2.5, 50)  # 10 to 316 MPa

    # Calculate cycles using both methods
    cycles_single = []
    cycles_two_segment = []

    for stress in stress_ranges:
        N_single = calculate_cycles_single_segment(stress, params['log_a'], params['m'],
                                                   params['fatigue_limit_stress'])
        N_two = calculate_cycles_two_segment(stress, params['log_a'], params['m'],
                                             transition_cycles, two_seg['log_c'],
                                             two_seg['r'], params['fatigue_limit_stress'])

        cycles_single.append(N_single if N_single != float('inf') else 1e11)
        cycles_two_segment.append(N_two if N_two != float('inf') else 1e11)

    # Calculate damage difference
    test_stress_ranges = [50, 60, 70, 80, 100]
    test_annual_cycles = 1e6  # 1 million cycles per year

    print(f"\n{'='*80}")
    print("DAMAGE CALCULATION COMPARISON")
    print(f"Annual cycles = {test_annual_cycles:,.0f}")
    print(f"{'='*80}")
    print(f"{'Stress (MPa)':<15} {'N_single':<15} {'N_two_seg':<15} {'D_single':<15} {'D_two_seg':<15} {'Diff %':<10}")
    print("-"*80)

    for stress in test_stress_ranges:
        N_single = calculate_cycles_single_segment(stress, params['log_a'], params['m'],
                                                   params['fatigue_limit_stress'])
        N_two = calculate_cycles_two_segment(stress, params['log_a'], params['m'],
                                             transition_cycles, two_seg['log_c'],
                                             two_seg['r'], params['fatigue_limit_stress'])

        if N_single == float('inf'):
            D_single = 0
        else:
            D_single = test_annual_cycles / N_single

        if N_two == float('inf'):
            D_two = 0
        else:
            D_two = test_annual_cycles / N_two

        if D_single > 0:
            diff_pct = ((D_single - D_two) / D_single) * 100
        else:
            diff_pct = 0

        print(f"{stress:<15.1f} {N_single:<15.2e} {N_two:<15.2e} {D_single:<15.3e} {D_two:<15.3e} {diff_pct:<10.2f}")

    # Create visualization
    fig, axes = plt.subplots(2, 2, figsize=(14, 10))

    # Plot 1: S-N Curves Comparison
    ax1 = axes[0, 0]
    ax1.loglog(stress_ranges, cycles_single, 'b-', linewidth=2, label='Single Segment (Current)')
    ax1.loglog(stress_ranges, cycles_two_segment, 'r--', linewidth=2, label='Two Segment (Correct)')
    ax1.axvline(x=params['fatigue_limit_stress'], color='green', linestyle=':',
               linewidth=2, label=f'Fatigue Limit ({params["fatigue_limit_stress"]} MPa)')
    ax1.axvline(x=transition_stress, color='orange', linestyle=':',
               linewidth=2, label=f'Transition ({transition_stress:.1f} MPa)')
    ax1.axhline(y=transition_cycles, color='orange', linestyle=':', linewidth=1, alpha=0.5)
    ax1.set_xlabel('Stress Range [MPa]', fontsize=12)
    ax1.set_ylabel('Cycles to Failure', fontsize=12)
    ax1.set_title('S-N Curve Comparison', fontsize=14, fontweight='bold')
    ax1.grid(True, alpha=0.3, which='both')
    ax1.legend(loc='upper right')
    ax1.set_xlim(10, 300)
    ax1.set_ylim(1e3, 1e11)

    # Plot 2: Damage Rate Comparison
    ax2 = axes[0, 1]
    damage_single = [test_annual_cycles / N if N < 1e11 else 0 for N in cycles_single]
    damage_two = [test_annual_cycles / N if N < 1e11 else 0 for N in cycles_two_segment]

    ax2.semilogy(stress_ranges, damage_single, 'b-', linewidth=2, label='Single Segment')
    ax2.semilogy(stress_ranges, damage_two, 'r--', linewidth=2, label='Two Segment')
    ax2.axvline(x=params['fatigue_limit_stress'], color='green', linestyle=':',
               linewidth=2, label=f'Fatigue Limit')
    ax2.axvline(x=transition_stress, color='orange', linestyle=':',
               linewidth=2, label=f'Transition')
    ax2.set_xlabel('Stress Range [MPa]', fontsize=12)
    ax2.set_ylabel('Damage per Million Cycles', fontsize=12)
    ax2.set_title('Damage Rate Comparison', fontsize=14, fontweight='bold')
    ax2.grid(True, alpha=0.3, which='both')
    ax2.legend(loc='upper right')
    ax2.set_xlim(10, 300)

    # Plot 3: Percent Difference
    ax3 = axes[1, 0]
    pct_diff = [((cycles_two_segment[i] - cycles_single[i]) / cycles_single[i] * 100)
                if cycles_single[i] < 1e11 else 0
                for i in range(len(cycles_single))]

    ax3.plot(stress_ranges, pct_diff, 'g-', linewidth=2)
    ax3.axvline(x=params['fatigue_limit_stress'], color='green', linestyle=':',
               linewidth=2, label=f'Fatigue Limit')
    ax3.axvline(x=transition_stress, color='orange', linestyle=':',
               linewidth=2, label=f'Transition')
    ax3.axhline(y=0, color='black', linestyle='-', linewidth=1)
    ax3.set_xlabel('Stress Range [MPa]', fontsize=12)
    ax3.set_ylabel('Percent Difference [%]', fontsize=12)
    ax3.set_title('Two-Segment vs Single-Segment (% Difference in N)', fontsize=14, fontweight='bold')
    ax3.grid(True, alpha=0.3)
    ax3.legend(loc='best')
    ax3.set_xlim(10, 300)

    # Plot 4: Conservatism Analysis
    ax4 = axes[1, 1]
    conservatism_ratio = [cycles_single[i] / cycles_two_segment[i]
                          if cycles_two_segment[i] < 1e11 else 1.0
                          for i in range(len(cycles_single))]

    ax4.semilogx(stress_ranges, conservatism_ratio, 'purple', linewidth=2)
    ax4.axvline(x=params['fatigue_limit_stress'], color='green', linestyle=':',
               linewidth=2, label=f'Fatigue Limit')
    ax4.axvline(x=transition_stress, color='orange', linestyle=':',
               linewidth=2, label=f'Transition')
    ax4.axhline(y=1.0, color='black', linestyle='-', linewidth=1)
    ax4.fill_between(stress_ranges, 1.0, conservatism_ratio,
                     where=[c < 1.0 for c in conservatism_ratio],
                     alpha=0.3, color='red', label='Non-conservative')
    ax4.fill_between(stress_ranges, 1.0, conservatism_ratio,
                     where=[c >= 1.0 for c in conservatism_ratio],
                     alpha=0.3, color='green', label='Conservative')
    ax4.set_xlabel('Stress Range [MPa]', fontsize=12)
    ax4.set_ylabel('N_single / N_two_segment', fontsize=12)
    ax4.set_title('Conservatism Ratio (>1 = Conservative)', fontsize=14, fontweight='bold')
    ax4.grid(True, alpha=0.3)
    ax4.legend(loc='best')
    ax4.set_xlim(10, 300)
    ax4.set_ylim(0.5, 1.5)

    plt.tight_layout()
    plt.savefig('output/test/two_segment_curve_analysis.png', dpi=150, bbox_inches='tight')
    print(f"\nPlot saved: output/test/two_segment_curve_analysis.png")
    plt.close()

    print(f"\n{'='*80}")
    print("CONCLUSIONS:")
    print("="*80)
    print(f"1. Current implementation uses ONLY Segment 1 (single segment)")
    print(f"2. Two-segment curve is configured but NOT implemented in code")
    print(f"3. Below {transition_stress:.1f} MPa, single-segment is NON-CONSERVATIVE")
    print(f"4. At fatigue limit ({params['fatigue_limit_stress']} MPa), both give infinite life")
    print(f"5. RECOMMENDATION: Implement two-segment logic in calculate_cycles_to_failure()")
    print("="*80)

if __name__ == "__main__":
    main()
