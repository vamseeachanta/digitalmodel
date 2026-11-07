# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run the synthetic axial-spring example in the MoorPy adapter guide.

Hand calculation: span=sqrt(6.6²+8.8²)=11 m, L=10 m, EA=1000 N.
Hooke's law gives T=EA*(span/L-1)=100 N; direction=(0.6,0.8,0),
so the line applies (60,80,0) N at A and (-60,-80,0) N at B.
These inputs are synthetic, not empirical line properties.
"""

from math import hypot

from digitalmodel.subsea.mooring_analysis.moorpy_adapter import solve_single_line


def main() -> None:
    """Solve with real MoorPy and check the independently derived end forces."""
    result = solve_single_line(
        endpoint_a=(0.0, 0.0, 0.0),
        endpoint_b=(6.6, 8.8, 0.0),
        unstretched_length=10.0,
        ea=1000.0,
        submerged_weight_per_length=0.0,
    )
    # A 1e-8 N numerical allowance is negligible beside the 100 N test load.
    expected = ((60.0, 80.0, 0.0), (-60.0, -80.0, 0.0))
    for actual, target in zip((result.force_a, result.force_b), expected):
        if hypot(*(a - b for a, b in zip(actual, target))) > 1e-8:
            raise RuntimeError("Endpoint load differs from the hand calculation")
    print("Force at A [N]: " + ", ".join(f"{f:.1f}" for f in result.force_a))
    print("Force at B [N]: " + ", ".join(f"{f:.1f}" for f in result.force_b))


if __name__ == "__main__":
    main()
