#!/usr/bin/env python3
"""
pyLife & py-fatigue Proof-of-Concept — Fatigue Analysis Evaluation
Issue: vamseeachanta/workspace-hub#1458

Demonstrates:
  1. S-N curve definition (DNV-RP-C203 Category F)
  2. Miner's rule cumulative damage from a stress histogram
  3. Woehler curve fitting from synthetic fatigue test data
  4. Comparison of pyLife vs py-fatigue capabilities
"""

import numpy as np
import pandas as pd

# ═══════════════════════════════════════════════════════════════════
# 1. pyLife: S-N Curve (Woehler Curve) Definition
# ═══════════════════════════════════════════════════════════════════

from pylife.materiallaws.woehlercurve import WoehlerCurve

# DNV-RP-C203 (2021) Category F curve parameters:
#   log(a) = 11.855, m = 3.0 for N <= 10^7
#   log(a) = 15.091, m = 5.0 for N > 10^7
#   Detail category: 52.63 MPa at 10^7 cycles (FAT 52)
dnv_f_params = pd.Series({
    'k_1': 3.0,      # slope in high-stress region (N < ND)
    'SD': 52.63,      # endurance limit stress range (MPa) at knee point
    'ND': 1e7,        # knee point cycles
    'k_2': 5.0,       # slope below endurance limit (Haibach extension)
})

wc = WoehlerCurve(dnv_f_params)

print("=" * 70)
print("1. pyLife — DNV-RP-C203 Category F S-N Curve")
print("=" * 70)
print(f"   Slope k_1 = {wc.k_1}, k_2 = {wc.k_2}")
print(f"   Endurance limit: {wc.SD} MPa at {wc.ND:.0e} cycles")

# Calculate allowable cycles at various stress ranges
test_stresses = np.array([150, 120, 100, 80, 60, 52.63, 40, 30, 20])
allowable_cycles = wc.cycles(test_stresses)

print(f"\n   {'Stress (MPa)':>14s}  {'Allowable N':>14s}")
print(f"   {'-'*14}  {'-'*14}")
for s, n in zip(test_stresses, allowable_cycles):
    print(f"   {s:14.2f}  {n:14.0f}")

# ═══════════════════════════════════════════════════════════════════
# 2. pyLife: Miner's Rule Cumulative Damage Calculation
# ═══════════════════════════════════════════════════════════════════

print("\n" + "=" * 70)
print("2. pyLife — Miner's Rule Damage Accumulation")
print("=" * 70)

# Typical offshore stress histogram (simplified)
stress_histogram = pd.DataFrame({
    'stress_range_MPa': [120, 100, 80, 60, 50, 40, 30, 20],
    'applied_cycles':   [1e3, 5e3, 2e4, 8e4, 2e5, 5e5, 2e6, 1e7],
})

# Calculate damage per bin using WoehlerCurve
stress_ranges = stress_histogram['stress_range_MPa'].values
applied = stress_histogram['applied_cycles'].values
allowable = wc.cycles(stress_ranges)
damage_per_bin = applied / allowable
total_damage = damage_per_bin.sum()

print(f"\n   {'Stress':>8s}  {'Applied n':>12s}  {'Allowable N':>14s}  {'Damage n/N':>12s}")
print(f"   {'-'*8}  {'-'*12}  {'-'*14}  {'-'*12}")
for s, n, N, d in zip(stress_ranges, applied, allowable, damage_per_bin):
    print(f"   {s:8.0f}  {n:12.0f}  {N:14.0f}  {d:12.6f}")
print(f"   {'-'*8}  {'-'*12}  {'-'*14}  {'-'*12}")
print(f"   {'TOTAL':>8s}  {applied.sum():12.0f}  {'':14s}  {total_damage:12.6f}")
print(f"\n   Miner's damage sum D = {total_damage:.4f}")
print(f"   Failure criterion D >= 1.0: {'FAIL' if total_damage >= 1.0 else 'SAFE'}")
print(f"   Design life factor: {1/total_damage:.1f}x (can repeat loading {1/total_damage:.0f} times)")

# ═══════════════════════════════════════════════════════════════════
# 3. pyLife: Woehler Curve Fitting from Test Data
# ═══════════════════════════════════════════════════════════════════

print("\n" + "=" * 70)
print("3. pyLife — Woehler Curve Fitting (Elementary Method)")
print("=" * 70)

from pylife.materialdata.woehler.elementary import Elementary

# Synthetic fatigue test data (stress range vs cycles to failure)
np.random.seed(42)
test_stress = np.array([150, 150, 120, 120, 100, 100, 80, 80, 60, 60, 50, 50])
# Generate cycles using known S-N relationship + scatter
log_a_true = 11.855  # DNV Cat F
m_true = 3.0
log_N_true = log_a_true - m_true * np.log10(test_stress)
scatter = np.random.normal(0, 0.15, len(test_stress))
log_N_observed = log_N_true + scatter
N_observed = 10**log_N_observed

# Mark runouts (did not fail) for low stress levels
fracture = np.ones(len(test_stress), dtype=int)
fracture[-2:] = 0  # last two are runouts at 50 MPa

fatigue_data = pd.DataFrame({
    'load': test_stress,
    'cycles': N_observed,
    'fracture': fracture,
})

print("\n   Test data:")
print(f"   {'Stress (MPa)':>14s}  {'Cycles':>14s}  {'Fracture':>8s}")
print(f"   {'-'*14}  {'-'*14}  {'-'*8}")
for _, row in fatigue_data.iterrows():
    print(f"   {row['load']:14.0f}  {row['cycles']:14.0f}  {'Yes' if row['fracture'] else 'Runout':>8s}")

try:
    elem = Elementary(fatigue_data)
    result = elem.analyze()
    print(f"\n   Fitted parameters:")
    print(f"   k_1 (slope):         {result.get('k_1', 'N/A')}")
    print(f"   SD (endurance MPa):  {result.get('SD', 'N/A')}")
    print(f"   ND (knee cycles):    {result.get('ND', 'N/A')}")
    print(f"   TN (scatter):        {result.get('TN', 'N/A')}")
except Exception as e:
    print(f"\n   Elementary fitting error: {type(e).__name__}: {e}")
    print("   (Woehler fitting requires specific data format — see pylife docs)")

    # Manual fit via log-log regression as fallback
    mask = fracture == 1
    log_s = np.log10(test_stress[mask])
    log_n = np.log10(N_observed[mask])
    coeffs = np.polyfit(log_s, log_n, 1)
    m_fit = -coeffs[0]
    log_a_fit = coeffs[1]
    print(f"\n   Manual log-log regression (fracture data only):")
    print(f"   Slope m = {m_fit:.2f} (true: {m_true})")
    print(f"   log(a) = {log_a_fit:.3f} (true: {log_a_true})")

# ═══════════════════════════════════════════════════════════════════
# 4. py-fatigue: Equivalent S-N Curve & Damage
# ═══════════════════════════════════════════════════════════════════

print("\n" + "=" * 70)
print("4. py-fatigue — S-N Curve & Palmgren-Miner Damage")
print("=" * 70)

from py_fatigue.material.sn_curve import SNCurve

# DNV-RP-C203 Category F: log10(a)=11.855, m=3
# py-fatigue SNCurve: intercept is log10(a), slope is m
# Two-slope curve needs two segments
sn_f_seg1 = SNCurve(
    slope=3.0,
    intercept=11.855,   # log10(a) for N < 10^7
    endurance=1e7,
    curve="F (seg1)",
    norm="DNV-RP-C203",
    unit_string="MPa",
)
sn_f_seg2 = SNCurve(
    slope=5.0,
    intercept=15.091,   # log10(a) for N > 10^7
    curve="F (seg2)",
    norm="DNV-RP-C203",
    unit_string="MPa",
)

print(f"   Curve: F ({sn_f_seg1.norm})")
print(f"   Segment 1: slope={float(sn_f_seg1.slope)}, log10(a)={float(sn_f_seg1.intercept):.3f}")
print(f"   Segment 2: slope={float(sn_f_seg2.slope)}, log10(a)={float(sn_f_seg2.intercept):.3f}")
print(f"   Endurance: {sn_f_seg1.endurance:.0e} cycles")

# Get cycles — use seg1 for high stress, seg2 for low stress
pf_cycles_1 = sn_f_seg1.get_cycles(test_stresses)
pf_cycles_2 = sn_f_seg2.get_cycles(test_stresses)
# Pick the correct segment based on endurance limit
pf_cycles = np.where(pf_cycles_1 <= 1e7, pf_cycles_1, pf_cycles_2)
print(f"\n   {'Stress (MPa)':>14s}  {'pyLife N':>14s}  {'py-fatigue N':>14s}")
print(f"   {'-'*14}  {'-'*14}  {'-'*14}")
for s, n_pl, n_pf in zip(test_stresses, allowable_cycles, pf_cycles):
    print(f"   {s:14.2f}  {n_pl:14.0f}  {n_pf:14.0f}")

# Palmgren-Miner damage with py-fatigue
from py_fatigue.damage.stress_life import calc_pm
print(f"\n   Palmgren-Miner via py-fatigue:")
print(f"   (Uses CycleCount accessor — requires rainflow-counted data)")

# ═══════════════════════════════════════════════════════════════════
# 5. Comparison Summary
# ═══════════════════════════════════════════════════════════════════

print("\n" + "=" * 70)
print("5. Comparison: pyLife vs py-fatigue")
print("=" * 70)

comparison = """
   ┌─────────────────────────┬──────────────────────┬──────────────────────┐
   │ Feature                 │ pyLife (Bosch)        │ py-fatigue (OWI-Lab) │
   ├─────────────────────────┼──────────────────────┼──────────────────────┤
   │ License                 │ Apache-2.0           │ GPL-3.0              │
   │ Stars                   │ ~165                 │ ~30                  │
   │ Maturity                │ Mature (v2.2.1)      │ Growing (v2.1.0)     │
   │ S-N Curves              │ Yes (WoehlerCurve)   │ Yes (SNCurve)        │
   │ Miner's Rule            │ Yes (3 variants)     │ Yes (PalmgrenMiner)  │
   │ Woehler Fitting         │ Yes (4+ methods)     │ No                   │
   │ Rainflow Counting       │ Yes (C extension)    │ Yes (numba-accel.)   │
   │ Crack Growth (Paris)    │ No                   │ Yes                  │
   │ Mean Stress Correction  │ Yes (FKM)            │ Yes (Goodman etc.)   │
   │ FEM Mesh Integration    │ Yes (hotspot, grad.)  │ No                   │
   │ pandas Integration      │ Deep (accessors)     │ Moderate (accessors) │
   │ Notch Approx. (Neuber) │ Yes                  │ No                   │
   │ FKM Nonlinear           │ Yes                  │ No                   │
   │ Failure Probability     │ Yes                  │ No                   │
   │ Offshore Wind Focus     │ No (automotive)      │ Yes                  │
   │ Cycle Count DEMs/DES    │ No                   │ Yes                  │
   └─────────────────────────┴──────────────────────┴──────────────────────┘

   Recommendation for digitalmodel:
   - USE BOTH: pyLife for S-N curves, Woehler fitting, Miner's rule, FEM post-proc
   - USE py-fatigue for: crack growth (Paris' law), offshore-specific DEMs,
     and when GPL-3.0 is acceptable
   - pyLife is better suited as the PRIMARY fatigue engine (Apache-2.0, broader API)
   - py-fatigue complements with crack growth and offshore wind heritage
"""
print(comparison)
