#!/usr/bin/env python3
# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Solve a synthetic pinned-end catenary from endpoints and submerged weight.

Run from the repository root with PYTHONPATH=src. See pinned_catenary.md
for the independent derivation and assumptions. No external solver is used.
"""

from math import cosh, inf, isfinite, log, sinh, sqrt

from digitalmodel.marine_ops.marine_analysis.catenary.solver import (
    CatenaryInput,
    CatenaryResults,
    CatenarySolver,
)


def solve_pinned_catenary(
    start: tuple[float, float],
    end: tuple[float, float],
    length: float,
    submerged_weight: float,
) -> CatenaryResults:
    """Solve a fully suspended, uniform, inextensible line in an x-z plane.

    Coordinates and length are in metres; z is positive upward. Submerged
    weight is a positive downward force per unit length in N/m, not kg/m.
    The end must lie to the right of the start, and length must exceed the
    endpoint distance. No seabed, bending, extension or external loads enter.
    Returned shape coordinates use the same datum as the input endpoints.
    """
    if not all(isfinite(value) for value in (*start, *end, length, submerged_weight)):
        raise ValueError("Endpoints, length and submerged weight must be finite")
    params = CatenaryInput(
        length=length,
        horizontal_span=end[0] - start[0],
        vertical_span=end[1] - start[1],
        weight_per_length=submerged_weight,
        # The exact inextensible limit eliminates the elastic correction.
        ea_stiffness=inf,
    )
    result = CatenarySolver().solve(params)
    if not result.converged:
        raise RuntimeError("Pinned catenary solution did not converge")
    result.shape_x += start[0]
    result.shape_y += start[1]
    return result


def main() -> None:
    """Derive the closed-form case, then solve using only geometry and weight."""
    # Synthetic choices: a=80 m and H=200 N imply w=H/a=2.5 N/m.
    # u_start=ln(2), u_end=ln(4). Hand arithmetic is in pinned_catenary.md.
    a, horizontal_tension = 80.0, 200.0
    submerged_weight = horizontal_tension / a
    u_start, u_end = log(2.0), log(4.0)
    start = (a * u_start, a * (cosh(u_start) - 1.0))
    end = (a * u_end, a * (cosh(u_end) - 1.0))
    length = a * (sinh(u_end) - sinh(u_start))
    result = solve_pinned_catenary(start, end, length, submerged_weight)
    # This case rises at the start, so its signed vertical tension is positive.
    # Recover it from the computed resultant, independently of V_end-w*L.
    start_vertical = sqrt(result.total_tension_anchor**2 - result.horizontal_tension**2)
    print(f"Start (x, z): ({start[0]:.1f}, {start[1]:.1f}) m")
    print(f"End (x, z): ({end[0]:.1f}, {end[1]:.1f}) m")
    print(f"Line length: {length:.1f} m; submerged weight: {submerged_weight:.1f} N/m")
    print(f"Catenary parameter: {result.catenary_parameter:.1f} m")
    print(f"Horizontal tension: {result.horizontal_tension:.1f} N")
    print(f"Signed vertical tensions, start/end: {start_vertical:.1f}, "
          f"{result.vertical_tension_fairlead:.1f} N")
    print(f"Resultant tensions, start/end: {result.total_tension_anchor:.1f}, "
          f"{result.total_tension_fairlead:.1f} N")
    print(f"Supported weight: {submerged_weight * length:.1f} N; "
          f"vertical tension difference: {result.vertical_tension_fairlead - start_vertical:.1f} N")


if __name__ == "__main__":
    main()
