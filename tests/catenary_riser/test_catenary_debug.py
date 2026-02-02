"""
Debug script to test catenary solver and diagnose numerical issues.
"""

import pytest
import sys

pytest.importorskip(
    "src.marine_engineering.mooring_analysis.catenary_solver",
    reason="module src.marine_engineering.mooring_analysis.catenary_solver not available"
)

sys.path.insert(0, 'D:/workspace-hub/digitalmodel')

import numpy as np
from src.marine_engineering.mooring_analysis.catenary_solver import (
    CatenarySolver, CatenaryInput
)

def test_excel_reference():
    """Test against Excel 'Poly Mooring' reference case."""

    # Excel Reference Case
    params = CatenaryInput(
        length=1000,           # m
        horizontal_span=800,   # m
        vertical_span=100,     # m
        weight_per_length=1962,  # N/m
        ea_stiffness=64e9      # N
    )

    # Expected from Excel
    expected_H = 785_000  # N
    expected_V = 196_200  # N
    expected_T = 809_000  # N

    solver = CatenarySolver(tolerance=1e-8, max_iterations=100)

    print("\n" + "="*80)
    print("CATENARY SOLVER DEBUG TEST")
    print("="*80)
    print("\nInput Parameters:")
    print(f"  Length:           {params.length} m")
    print(f"  Horizontal span:  {params.horizontal_span} m")
    print(f"  Vertical span:    {params.vertical_span} m")
    print(f"  Weight/length:    {params.weight_per_length} N/m")
    print(f"  EA stiffness:     {params.ea_stiffness:.2e} N")

    print("\nExpected Results (Excel):")
    print(f"  H tension:        {expected_H:,.0f} N")
    print(f"  V tension:        {expected_V:,.0f} N")
    print(f"  Total tension:    {expected_T:,.0f} N")

    try:
        results = solver.solve(params)

        print("\nSolver Results:")
        print(f"  Converged:        {results.converged}")
        print(f"  Iterations:       {results.iterations}")
        print(f"  H tension:        {results.horizontal_tension:,.0f} N")
        print(f"  V tension:        {results.vertical_tension_fairlead:,.0f} N")
        print(f"  Total tension:    {results.total_tension_fairlead:,.0f} N")
        print(f"  Elongation:       {results.elongation:.2f} m")
        print(f"  Catenary param:   {results.catenary_parameter:.2f} m")

        print("\nErrors vs Excel:")
        H_error = abs(results.horizontal_tension - expected_H) / expected_H * 100
        V_error = abs(results.vertical_tension_fairlead - expected_V) / expected_V * 100
        T_error = abs(results.total_tension_fairlead - expected_T) / expected_T * 100

        print(f"  H tension error:  {H_error:.2f}%")
        print(f"  V tension error:  {V_error:.2f}%")
        print(f"  Total T error:    {T_error:.2f}%")

        print("\nTest Status:")
        if H_error < 1.0:
            print("  ✓ PASS - H tension within ±1%")
        else:
            print(f"  ✗ FAIL - H tension error {H_error:.2f}% exceeds 1% tolerance")

        if V_error < 1.0:
            print("  ✓ PASS - V tension within ±1%")
        else:
            print(f"  ✗ FAIL - V tension error {V_error:.2f}% exceeds 1% tolerance")

        # Debug: Check arc length calculation
        a = results.horizontal_tension / params.weight_per_length
        x_span = params.horizontal_span

        print("\nDiagnostics:")
        print(f"  a (catenary param): {a:.2f} m")
        print(f"  x/a ratio:          {x_span/a:.4f}")

        # Check for sinh overflow
        try:
            sinh_val = np.sinh(x_span / a)
            arc_length = a * sinh_val
            print(f"  sinh(x/a):          {sinh_val:.4f}")
            print(f"  Arc length:         {arc_length:.2f} m")
            print(f"  Elongation:         {results.elongation:.2f} m")
            print(f"  Total (arc+elong):  {arc_length + results.elongation:.2f} m")
            print(f"  Target length:      {params.length} m")
            print(f"  Length error:       {(arc_length + results.elongation - params.length):.6f} m")
        except:
            print(f"  ✗ OVERFLOW in sinh({x_span/a:.2f})")

    except Exception as e:
        print(f"\n✗ SOLVER FAILED: {e}")
        import traceback
        traceback.print_exc()

    print("="*80)

if __name__ == "__main__":
    test_excel_reference()
