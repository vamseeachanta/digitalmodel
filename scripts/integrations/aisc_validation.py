"""AISC shapes database validation against sectionproperties FEM results.

Loads shapes from the YAML catalog, computes section properties via
sectionproperties, and compares against AISC manual reference values.

Reference: vamseeachanta/workspace-hub#1497
"""

import sys
from pathlib import Path

# Ensure the integrations package is importable
sys.path.insert(0, str(Path(__file__).resolve().parent))

import matplotlib
matplotlib.use("Agg")

from aisc_section_lookup import list_shapes, validate_shape

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

# Shapes to validate (mix of series sizes)
VALIDATION_SHAPES = [
    "W14x90",
    "W14x48",
    "W14x22",
    "W12x65",
    "W10x49",
    "W8x31",
    "W6x15",
    "W24x176",
    "W36x150",
    "W21x44",
]

# Tolerance for geometric properties (A, Ix, Iy)
GEO_TOLERANCE_PCT = 3.0

# Properties to check against tolerance
GEO_PROPERTIES = ("A", "Ix", "Iy")


def main():
    print("=" * 78)
    print("  AISC Shapes Database — Validation Report")
    print("  sectionproperties FEM vs. AISC Manual Reference Values")
    print("=" * 78)

    all_results = []
    for shape in VALIDATION_SHAPES:
        result = validate_shape(shape, verbose=True)
        all_results.append(result)

    # -----------------------------------------------------------------------
    # Summary table
    # -----------------------------------------------------------------------
    print("\n")
    print("=" * 78)
    print("  SUMMARY TABLE")
    print("=" * 78)

    header = f"  {'Shape':<12s}"
    for prop in ("A", "Ix", "Iy", "Sx", "Zx", "J", "Cw"):
        header += f"  {prop:>7s}"
    print(header)
    print(f"  {'-' * 72}")

    for r in all_results:
        row = f"  {r['designation']:<12s}"
        for prop in ("A", "Ix", "Iy", "Sx", "Zx", "J", "Cw"):
            pct = r["pct_diff"][prop]
            row += f"  {pct:>6.2f}%"
        print(row)

    # -----------------------------------------------------------------------
    # Assertions: geometric properties within tolerance
    # -----------------------------------------------------------------------
    print(f"\n{'=' * 78}")
    print(f"  ASSERTION CHECK: A, Ix, Iy within {GEO_TOLERANCE_PCT}%")
    print(f"{'=' * 78}")

    failures = []
    for r in all_results:
        for prop in GEO_PROPERTIES:
            pct = r["pct_diff"][prop]
            if pct > GEO_TOLERANCE_PCT:
                failures.append((r["designation"], prop, pct))

    if failures:
        print("\n  FAILURES:")
        for shape, prop, pct in failures:
            print(f"    {shape} {prop}: {pct:.2f}% > {GEO_TOLERANCE_PCT}%")
        print(f"\n  {len(failures)} assertion(s) FAILED.")
        sys.exit(1)
    else:
        print(f"\n  All {len(all_results)} shapes pass: A, Ix, Iy within {GEO_TOLERANCE_PCT}%.")
        print("  Validation PASSED.")

    return all_results


if __name__ == "__main__":
    main()
