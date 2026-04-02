#!/usr/bin/env python3
"""
================================================================================
  ACE Engineer — 221 S-N Curves from 17 International Standards
================================================================================

  Demo: Comprehensive S-N Curve Library for Fatigue Design
  Run:  PYTHONPATH=src python examples/demos/demo_sn_library.py

  This script showcases the digitalmodel fatigue S-N curve library:
    1. Total curve count and standards covered
    2. Search curves by standard and environment
    3. Compare curves across standards for same detail class
    4. Calculate fatigue life for a stress range histogram
    5. Summary table of all curves

================================================================================
"""

import math
import numpy as np

from digitalmodel.fatigue.sn_library import (
    get_catalog,
    search_curves,
    curve_count,
    list_standards,
    summary_table,
    get_library_curve,
)


def separator(title=""):
    """Print a visual separator."""
    if title:
        print(f"\n{'=' * 80}")
        print(f"  {title}")
        print(f"{'=' * 80}")
    else:
        print(f"{'─' * 80}")


def main():
    print()
    print("  ╔══════════════════════════════════════════════════════════════════════╗")
    print("  ║         ACE ENGINEER  —  S-N Curve Library Demo                     ║")
    print("  ║         221 Fatigue Curves from 17 International Standards           ║")
    print("  ╚══════════════════════════════════════════════════════════════════════╝")
    print()

    # ──────────────────────────────────────────────────────────────────────
    # 1. Library Overview
    # ──────────────────────────────────────────────────────────────────────
    separator("1. LIBRARY OVERVIEW")

    total = curve_count()
    standards = list_standards()
    catalog = get_catalog()

    print(f"\n  Total S-N Curves:   {total}")
    print(f"  Standards Covered:  {len(standards)}")
    print(f"\n  {'No.':<5} {'Standard':<30} {'Edition':<10} {'Curves':<8}")
    print(f"  {'─'*5} {'─'*30} {'─'*10} {'─'*8}")

    for i, std in enumerate(standards, 1):
        curves_in_std = catalog.filter(standard=std)
        # Get edition from first curve
        edition = curves_in_std[0].standard_edition if curves_in_std else ""
        print(f"  {i:<5} {std:<30} {edition:<10} {len(curves_in_std):<8}")

    print(f"\n  {'':>47} Total: {total}")

    # ──────────────────────────────────────────────────────────────────────
    # 2. Search by Standard & Environment
    # ──────────────────────────────────────────────────────────────────────
    separator("2. SEARCH: DNV-RP-C203 — Seawater with Cathodic Protection")

    dnv_cp = search_curves(standard="DNV-RP-C203", environment="seawater_cp")

    print(f"\n  Found {len(dnv_cp)} curves for DNV-RP-C203 in seawater with CP:")
    print()
    print(f"  {'Class':<8} {'m1':<6} {'log(a1)':<10} {'m2':<6} {'Endurance':<12} {'Note'}")
    print(f"  {'─'*8} {'─'*6} {'─'*10} {'─'*6} {'─'*12} {'─'*40}")

    for c in dnv_cp:
        m2_str = f"{c.m2:.1f}" if c.m2 is not None else "—"
        el_str = f"{c.endurance_limit:.1f} MPa" if c.endurance_limit is not None else "None"
        note = c.note[:45] + "..." if len(c.note) > 48 else c.note
        print(f"  {c.curve_class:<8} {c.m1:<6.1f} {c.log_a1:<10.3f} {m2_str:<6} {el_str:<12} {note}")

    # ──────────────────────────────────────────────────────────────────────
    # 3. Cross-Standard Comparison — Detail Class "D"
    # ──────────────────────────────────────────────────────────────────────
    separator("3. CROSS-STANDARD COMPARISON: Detail Class 'D' (In Air)")

    d_curves = search_curves(curve_class="D", environment="air")

    print(f"\n  Found {len(d_curves)} 'Class D' curves across standards:\n")
    print(f"  {'Standard':<25} {'m1':<6} {'log(a1)':<10} {'CAFL (MPa)':<12} {'N @ 100 MPa':<14}")
    print(f"  {'─'*25} {'─'*6} {'─'*10} {'─'*12} {'─'*14}")

    for c in d_curves:
        # Calculate cycles at 100 MPa stress range
        n_100 = c.cycles(100.0)
        cafl_str = f"{c.endurance_limit:.1f}" if c.endurance_limit is not None else "—"
        n_str = f"{n_100:.2e}" if n_100 < float('inf') else "∞"
        print(f"  {c.standard:<25} {c.m1:<6.1f} {c.log_a1:<10.3f} {cafl_str:<12} {n_str:<14}")

    # ──────────────────────────────────────────────────────────────────────
    # 4. Fatigue Life Calculation
    # ──────────────────────────────────────────────────────────────────────
    separator("4. FATIGUE LIFE CALCULATION — DNV-RP-C203 Class D")

    # Realistic stress range histogram (offshore structure weld detail)
    stress_ranges = np.array([30, 50, 70, 90, 110, 130, 150, 170, 190])  # MPa
    cycle_counts = np.array([
        5.0e6, 2.0e6, 1.0e6, 5.0e5, 2.0e5, 8.0e4, 3.0e4, 1.0e4, 2.0e3
    ])

    print(f"\n  Stress Range Histogram (offshore jacket weld detail):")
    print(f"  {'S (MPa)':<12} {'Cycles (n)':<15} {'N (allowable)':<18} {'Damage (n/N)'}")
    print(f"  {'─'*12} {'─'*15} {'─'*18} {'─'*15}")

    # Get the DNV D-curve in air
    d_air = get_library_curve("DNV-RP-C203:D:air")
    d_sw = get_library_curve("DNV-RP-C203:D:seawater_cp")

    total_damage_air = 0.0
    total_damage_sw = 0.0
    total_cycles = 0

    for s, n in zip(stress_ranges, cycle_counts):
        N_allow = d_air.cycles(float(s))
        damage = n / N_allow if N_allow < float('inf') else 0.0
        total_damage_air += damage
        total_cycles += int(n)
        print(f"  {s:<12} {n:<15.2e} {N_allow:<18.2e} {damage:<15.4e}")

    # Also compute for seawater
    for s, n in zip(stress_ranges, cycle_counts):
        N_allow_sw = d_sw.cycles(float(s))
        damage_sw = n / N_allow_sw if N_allow_sw < float('inf') else 0.0
        total_damage_sw += damage_sw

    separator()

    total_cycles_str = f"{total_cycles:,.0f}"
    fatigue_life_air = 1.0 / total_damage_air if total_damage_air > 0 else float('inf')
    fatigue_life_sw = 1.0 / total_damage_sw if total_damage_sw > 0 else float('inf')

    print(f"\n  Total stress cycles:              {total_cycles_str}")
    print(f"\n  --- In Air (DNV-RP-C203 D) ---")
    print(f"  Cumulative Miner's damage:        {total_damage_air:.6f}")
    print(f"  Fatigue life (design repeats):     {fatigue_life_air:.1f}")
    print(f"  If 1 repeat = 1 year:             {fatigue_life_air:.0f} years")
    print(f"\n  --- Seawater w/ CP (DNV-RP-C203 D) ---")
    print(f"  Cumulative Miner's damage:        {total_damage_sw:.6f}")
    print(f"  Fatigue life (design repeats):     {fatigue_life_sw:.1f}")
    print(f"  If 1 repeat = 1 year:             {fatigue_life_sw:.0f} years")
    print(f"\n  Seawater fatigue life reduction:   {(1 - fatigue_life_sw/fatigue_life_air)*100:.1f}%")

    # ──────────────────────────────────────────────────────────────────────
    # 5. Summary Table (first 30 curves)
    # ──────────────────────────────────────────────────────────────────────
    separator("5. LIBRARY SUMMARY (first 30 of 221 curves)")

    table = summary_table()

    print(f"\n  {'Curve ID':<35} {'Standard':<20} {'Class':<10} {'Env':<16} {'m1':<5} {'log(a1)':<9} {'CAFL'}")
    print(f"  {'─'*35} {'─'*20} {'─'*10} {'─'*16} {'─'*5} {'─'*9} {'─'*8}")

    for row in table[:30]:
        el_str = f"{row['endurance_limit']:.0f}" if row['endurance_limit'] is not None else "—"
        print(
            f"  {row['curve_id']:<35} {row['standard']:<20} "
            f"{row['curve_class']:<10} {row['environment']:<16} "
            f"{row['m1']:<5.1f} {row['log_a1']:<9.3f} {el_str}"
        )

    print(f"\n  ... and {len(table) - 30} more curves in the library.")

    # ──────────────────────────────────────────────────────────────────────
    # Environments Summary
    # ──────────────────────────────────────────────────────────────────────
    separator("ENVIRONMENT COVERAGE")

    air_count = len(search_curves(environment="air"))
    sw_cp_count = len(search_curves(environment="seawater_cp"))
    fc_count = len(search_curves(environment="free_corrosion"))

    print(f"\n  In Air:                {air_count:>4} curves")
    print(f"  Seawater with CP:      {sw_cp_count:>4} curves")
    print(f"  Free Corrosion:        {fc_count:>4} curves")
    print(f"  {'─'*32}")
    print(f"  Total:                 {air_count + sw_cp_count + fc_count:>4} curves")

    # ──────────────────────────────────────────────────────────────────────
    # Footer
    # ──────────────────────────────────────────────────────────────────────
    print()
    print("  ╔══════════════════════════════════════════════════════════════════════╗")
    print("  ║  ACE Engineer — Programmatic access to 221 S-N curves              ║")
    print("  ║  from 17 international standards, ready for fatigue assessment.     ║")
    print("  ║                                                                    ║")
    print("  ║  Contact: vamsee.achanta@aceengineer.com | aceengineer.com          ║")
    print("  ╚══════════════════════════════════════════════════════════════════════╝")
    print()


if __name__ == "__main__":
    main()
