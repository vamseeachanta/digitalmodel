#!/usr/bin/env python3
"""
Verify S-N Curve Correction
Compare old (incorrect) vs new (corrected) ABS E curve parameters
"""

import numpy as np
import matplotlib.pyplot as plt

def calculate_N_old(stress):
    """Old incorrect parameters"""
    log_N = 12.164 - 3.0 * np.log10(stress)
    return 10 ** log_N

def calculate_N_new(stress):
    """Corrected ABS E parameters"""
    log_N = 12.0170 - 3.0 * np.log10(stress)
    N = 10 ** log_N
    
    # Two-segment curve
    if N > 1e7:
        # Use second segment for N > 10^7
        log_N = 15.378 - 5.0 * np.log10(stress)
        N = 10 ** log_N
    
    return N

# Test points
test_stresses = [10, 20, 30, 40, 47, 50, 52.64, 60, 80, 100, 150, 200]

print("S-N Curve Verification - ABS E")
print("="*80)
print(f"{'Stress':<10} {'N (Old)':<15} {'N (Corrected)':<15} {'Difference':<15} {'Status'}")
print("-"*80)

for stress in test_stresses:
    N_old = calculate_N_old(stress)
    N_new = calculate_N_new(stress)
    
    # Check fatigue limits
    if stress <= 52.64 and N_old > 1e7:
        N_old = float('inf')
    if stress <= 47.0 and N_new > 1e7:
        N_new_display = float('inf')
    else:
        N_new_display = N_new
    
    diff_pct = ((N_old - N_new) / N_new * 100) if N_new != float('inf') and N_old != float('inf') else 0
    
    # Status
    if stress == 47.0:
        status = "Fatigue Limit (NEW)"
    elif stress == 52.64:
        status = "Fatigue Limit (OLD)"
    elif 47.0 < stress < 52.64:
        status = "CRITICAL ZONE"
    else:
        status = ""
    
    # Format output
    if N_old == float('inf'):
        N_old_str = "INFINITE"
    else:
        N_old_str = f"{N_old:.3e}"
    
    if N_new_display == float('inf'):
        N_new_str = "INFINITE"
    else:
        N_new_str = f"{N_new:.3e}"
    
    if diff_pct != 0:
        diff_str = f"{diff_pct:+.1f}%"
    else:
        diff_str = "-"
    
    print(f"{stress:<10.1f} {N_old_str:<15} {N_new_str:<15} {diff_str:<15} {status}")

print("="*80)

# Key findings
print("\nKEY FINDINGS:")
print("-" * 40)
print("1. OLD fatigue limit: 52.64 MPa")
print("2. CORRECTED fatigue limit: 47.0 MPa")
print("3. Critical zone: 47-52.64 MPa")
print("   - OLD: Infinite life (unsafe)")
print("   - NEW: Finite life (correct)")
print("\n4. Example at 50 MPa:")
old_50 = calculate_N_old(50)
new_50 = calculate_N_new(50)
print(f"   - OLD: {old_50:.2e} cycles → INFINITE (wrong)")
print(f"   - NEW: {new_50:.2e} cycles (correct)")

# Create comparison plot
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

# Generate stress range for plotting
stress_range = np.logspace(0.5, 2.5, 100)
N_old_array = [calculate_N_old(s) for s in stress_range]
N_new_array = [calculate_N_new(s) for s in stress_range]

# Cap infinite values for plotting
N_old_plot = [min(n, 1e12) for n in N_old_array]
N_new_plot = [min(n, 1e12) for n in N_new_array]

# Left plot: S-N curves comparison
ax1.loglog(stress_range, N_old_plot, 'r--', linewidth=2, label='Old (Incorrect)')
ax1.loglog(stress_range, N_new_plot, 'b-', linewidth=2, label='Corrected ABS E')
ax1.axvline(x=52.64, color='red', linestyle=':', alpha=0.5, label='Old Limit (52.64 MPa)')
ax1.axvline(x=47.0, color='blue', linestyle=':', alpha=0.5, label='Correct Limit (47.0 MPa)')
ax1.axhspan(1e7, 1e12, xmin=0, xmax=1, alpha=0.1, color='green', label='Infinite Life Region')
ax1.axhline(y=1e7, color='gray', linestyle='-', alpha=0.3)

ax1.set_xlabel('Stress Range [MPa]')
ax1.set_ylabel('Cycles to Failure')
ax1.set_title('S-N Curve Comparison')
ax1.grid(True, alpha=0.3, which='both')
ax1.legend(loc='upper right')
ax1.set_xlim([1, 300])
ax1.set_ylim([1e4, 1e12])

# Right plot: Error in critical zone
critical_zone = np.linspace(40, 60, 50)
error_pct = []

for s in critical_zone:
    N_old_val = calculate_N_old(s)
    N_new_val = calculate_N_new(s)
    
    if s <= 52.64:
        N_old_val = 1e15  # Represent infinity as large number
    
    error = (N_old_val - N_new_val) / N_new_val * 100
    error_pct.append(min(error, 1000))  # Cap at 1000% for visualization

ax2.fill_between([47, 52.64], [0, 0], [1000, 1000], alpha=0.3, color='red', label='Critical Error Zone')
ax2.plot(critical_zone, error_pct, 'k-', linewidth=2)
ax2.axvline(x=47.0, color='blue', linestyle='--', label='Correct Fatigue Limit')
ax2.axvline(x=52.64, color='red', linestyle='--', label='Old (Wrong) Limit')
ax2.set_xlabel('Stress Range [MPa]')
ax2.set_ylabel('Error in Predicted Life [%]')
ax2.set_title('Error Impact in Critical Zone (47-53 MPa)')
ax2.grid(True, alpha=0.3)
ax2.legend()
ax2.set_xlim([40, 60])
ax2.set_yscale('log')
ax2.set_ylim([1, 1000])

plt.suptitle('ABS E S-N Curve Correction Analysis', fontsize=14, fontweight='bold')
plt.tight_layout()
plt.savefig('sn_curve_correction_comparison.png', dpi=150)
plt.show()

print("\nPlot saved as 'sn_curve_correction_comparison.png'")
print("\nCONCLUSION: The correction is CRITICAL for safety.")
print("Stresses in the 47-52.64 MPa range were incorrectly assessed as safe.")