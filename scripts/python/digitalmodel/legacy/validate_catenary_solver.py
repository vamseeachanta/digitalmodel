"""
Validation script for catenary solver implementation.

Validates against Excel "Poly Mooring" reference case and
demonstrates basic usage.
"""

import sys
import os

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

import numpy as np
from src.marine_engineering.mooring_analysis.catenary_solver import (
    CatenaryInput,
    CatenarySolver
)


def validate_excel_reference_case():
    """
    Validate against Excel "Poly Mooring" reference case.

    Excel Reference Values:
    - Length: 1000 m
    - Horizontal span: 800 m
    - Vertical span: 100 m
    - Weight per length: 1962 N/m (76mm chain in water)
    - EA stiffness: 64e9 N

    Expected Results (±1% accuracy):
    - Horizontal tension: ~785,000 N
    - Vertical tension: ~196,200 N
    - Total tension: ~809,000 N
    - Elongation: ~12.3 m
    """
    print("=" * 70)
    print("CATENARY SOLVER VALIDATION - Excel Reference Case")
    print("=" * 70)

    # Define input parameters from Excel reference
    params = CatenaryInput(
        length=1000,           # m
        horizontal_span=800,   # m
        vertical_span=100,     # m
        weight_per_length=1962,  # N/m (76mm chain in water)
        ea_stiffness=64e9      # N (studlink chain)
    )

    print("\nInput Parameters:")
    print(f"  Length:              {params.length:>10.1f} m")
    print(f"  Horizontal span:     {params.horizontal_span:>10.1f} m")
    print(f"  Vertical span:       {params.vertical_span:>10.1f} m")
    print(f"  Weight per length:   {params.weight_per_length:>10.1f} N/m")
    print(f"  EA stiffness:        {params.ea_stiffness:>10.2e} N")

    # Solve catenary
    solver = CatenarySolver(tolerance=1e-6)
    results = solver.solve(params)

    print("\nSolver Results:")
    print(f"  Converged:           {results.converged}")
    print(f"  Iterations:          {results.iterations}")

    print("\nTension Results:")
    print(f"  Horizontal tension (H):        {results.horizontal_tension:>12,.0f} N")
    print(f"  Vertical tension (V):          {results.vertical_tension_fairlead:>12,.0f} N")
    print(f"  Total tension (fairlead):      {results.total_tension_fairlead:>12,.0f} N")
    print(f"  Total tension (anchor):        {results.total_tension_anchor:>12,.0f} N")

    print("\nLine Properties:")
    print(f"  Catenary parameter (a):        {results.catenary_parameter:>12.2f} m")
    print(f"  Elastic elongation:            {results.elongation:>12.2f} m")
    print(f"  Touchdown distance:            {results.touchdown_distance if results.touchdown_distance else 'N/A'}")

    # Validate against Excel reference
    excel_H = 785_000
    excel_V = 196_200
    excel_T = 809_000

    print("\n" + "=" * 70)
    print("VALIDATION AGAINST EXCEL REFERENCE (±1% tolerance)")
    print("=" * 70)

    H_error = abs(results.horizontal_tension - excel_H) / excel_H * 100
    V_error = abs(results.vertical_tension_fairlead - excel_V) / excel_V * 100
    T_error = abs(results.total_tension_fairlead - excel_T) / excel_T * 100

    print(f"\nHorizontal Tension:")
    print(f"  Python:    {results.horizontal_tension:>12,.0f} N")
    print(f"  Excel:     {excel_H:>12,.0f} N")
    print(f"  Error:     {H_error:>12.3f}%  {'✓ PASS' if H_error < 1.0 else '✗ FAIL'}")

    print(f"\nVertical Tension:")
    print(f"  Python:    {results.vertical_tension_fairlead:>12,.0f} N")
    print(f"  Excel:     {excel_V:>12,.0f} N")
    print(f"  Error:     {V_error:>12.3f}%  {'✓ PASS' if V_error < 1.0 else '✗ FAIL'}")

    print(f"\nTotal Tension:")
    print(f"  Python:    {results.total_tension_fairlead:>12,.0f} N")
    print(f"  Excel:     {excel_T:>12,.0f} N")
    print(f"  Error:     {T_error:>12.3f}%  {'✓ PASS' if T_error < 1.0 else '✗ FAIL'}")

    # Overall validation
    all_pass = H_error < 1.0 and V_error < 1.0 and T_error < 1.0

    print("\n" + "=" * 70)
    if all_pass:
        print("✓ VALIDATION SUCCESSFUL - All results within ±1% of Excel")
    else:
        print("✗ VALIDATION FAILED - Results exceed ±1% tolerance")
    print("=" * 70)

    return all_pass


def demonstrate_usage():
    """Demonstrate basic catenary solver usage."""
    print("\n\n" + "=" * 70)
    print("CATENARY SOLVER USAGE DEMONSTRATION")
    print("=" * 70)

    # Example: Moderate mooring line
    params = CatenaryInput(
        length=1200,
        horizontal_span=1000,
        vertical_span=120,
        weight_per_length=2000,
        ea_stiffness=80e9,
        water_depth=150
    )

    print("\nExample Mooring Line:")
    print(f"  Length:           {params.length} m")
    print(f"  Horizontal span:  {params.horizontal_span} m")
    print(f"  Vertical span:    {params.vertical_span} m")
    print(f"  Weight/length:    {params.weight_per_length} N/m")
    print(f"  Water depth:      {params.water_depth} m")

    solver = CatenarySolver()
    results = solver.solve(params)

    print("\nSolution:")
    print(f"  H tension:        {results.horizontal_tension:,.0f} N")
    print(f"  V tension:        {results.vertical_tension_fairlead:,.0f} N")
    print(f"  Total tension:    {results.total_tension_fairlead:,.0f} N")
    print(f"  Elongation:       {results.elongation:.2f} m")
    print(f"  Touchdown:        {results.touchdown_distance:.2f} m" if results.touchdown_distance else "  No touchdown")

    # Plot line shape (text-based)
    print("\nLine Shape (simplified):")
    n_plot = 20
    indices = np.linspace(0, len(results.shape_x) - 1, n_plot, dtype=int)

    print(f"  {'X (m)':>10} {'Y (m)':>10} {'T (N)':>12}")
    print("  " + "-" * 34)
    for i in indices:
        x = results.shape_x[i]
        y = results.shape_y[i]
        t = results.tension_distribution[i]
        print(f"  {x:>10.1f} {y:>10.2f} {t:>12,.0f}")


def test_physical_properties():
    """Test fundamental physical properties."""
    print("\n\n" + "=" * 70)
    print("PHYSICAL PROPERTY TESTS")
    print("=" * 70)

    params = CatenaryInput(
        length=900,
        horizontal_span=750,
        vertical_span=95,
        weight_per_length=1800,
        ea_stiffness=70e9
    )

    solver = CatenarySolver()
    results = solver.solve(params)

    tests_passed = []

    # Test 1: Pythagorean theorem
    calc_total = np.sqrt(
        results.horizontal_tension**2 + results.vertical_tension_fairlead**2
    )
    pythagorean_ok = abs(calc_total - results.total_tension_fairlead) / calc_total < 1e-6
    tests_passed.append(pythagorean_ok)
    print(f"\n1. Pythagorean Theorem (T² = H² + V²):  {'✓ PASS' if pythagorean_ok else '✗ FAIL'}")

    # Test 2: Catenary parameter
    calc_a = results.horizontal_tension / params.weight_per_length
    catenary_ok = abs(calc_a - results.catenary_parameter) / calc_a < 1e-6
    tests_passed.append(catenary_ok)
    print(f"2. Catenary Parameter (a = H/w):        {'✓ PASS' if catenary_ok else '✗ FAIL'}")

    # Test 3: Elongation
    calc_elong = results.horizontal_tension * params.length / params.ea_stiffness
    elongation_ok = abs(calc_elong - results.elongation) / calc_elong < 1e-6
    tests_passed.append(elongation_ok)
    print(f"3. Elastic Elongation (e = H*L/EA):     {'✓ PASS' if elongation_ok else '✗ FAIL'}")

    # Test 4: Arc length
    a = results.catenary_parameter
    arc_length = a * np.sinh(params.horizontal_span / a)
    total_length = arc_length + results.elongation
    length_ok = abs(total_length - params.length) / params.length < 1e-3
    tests_passed.append(length_ok)
    print(f"4. Total Length (s + e = L):            {'✓ PASS' if length_ok else '✗ FAIL'}")

    # Test 5: Tension monotonicity
    monotonic_ok = np.all(np.diff(results.tension_distribution) >= 0)
    tests_passed.append(monotonic_ok)
    print(f"5. Tension Monotonicity:                {'✓ PASS' if monotonic_ok else '✗ FAIL'}")

    all_tests_pass = all(tests_passed)
    print("\n" + "=" * 70)
    if all_tests_pass:
        print(f"✓ ALL {len(tests_passed)} PHYSICAL PROPERTY TESTS PASSED")
    else:
        print(f"✗ {sum(tests_passed)}/{len(tests_passed)} TESTS PASSED")
    print("=" * 70)

    return all_tests_pass


def main():
    """Run all validation tests."""
    print("\n")
    print("+" + "=" * 68 + "+")
    print("|" + " " * 68 + "|")
    print("|" + "    CATENARY SOLVER VALIDATION SUITE".center(68) + "|")
    print("|" + "    Marine Engineering Module".center(68) + "|")
    print("|" + " " * 68 + "|")
    print("+" + "=" * 68 + "+")

    try:
        # Run validation tests
        excel_pass = validate_excel_reference_case()
        demonstrate_usage()
        physics_pass = test_physical_properties()

        # Final summary
        print("\n\n" + "+" + "=" * 68 + "+")
        print("|" + " " * 68 + "|")
        print("|" + "    VALIDATION SUMMARY".center(68) + "|")
        print("|" + " " * 68 + "|")
        print("+" + "=" * 68 + "+")

        print("\nTest Results:")
        print(f"  Excel Reference Validation:  {'PASS' if excel_pass else 'FAIL'}")
        print(f"  Physical Property Tests:     {'PASS' if physics_pass else 'FAIL'}")

        if excel_pass and physics_pass:
            print("\n>> ALL VALIDATIONS SUCCESSFUL")
            print("  Implementation meets specification requirements")
            print("  Results within +-1% of Excel reference")
            return 0
        else:
            print("\n>> VALIDATION FAILED")
            return 1

    except Exception as e:
        print(f"\n✗ ERROR: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
