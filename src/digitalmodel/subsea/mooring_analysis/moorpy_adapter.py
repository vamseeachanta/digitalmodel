# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Optional MoorPy single-line equilibrium in SI units, with prescribed ends.

API basis: MoorPy 1.3.0, Catenary.catenary and nonlinear.nonlinear in the
published distribution: https://pypi.org/project/MoorPy/1.3.0/.
The former uses metres, newtons, and positive-up vertical forces; the latter
supports an exactly weightless straight line with a supplied tension-strain
curve. MoorPy is not imported until a valid solve is requested.
"""

from dataclasses import dataclass
from math import hypot, isfinite
from typing import Sequence


@dataclass(frozen=True)
class SingleLineResult:
    """Forces applied by the line to endpoints A and B, in global (x, y, z) N.

    Global x and y are horizontal; z is positive upward. These are loads on
    the attachments, so support reactions on the line have opposite signs.
    For a massless taut line force_a points toward B and force_b toward A.
    For a weighted line their sum is (0, 0, -weight_per_length * length).
    """

    force_a: tuple[float, float, float]
    force_b: tuple[float, float, float]


def _endpoint(value: Sequence[float], name: str) -> tuple[float, ...]:
    """Validate a Cartesian endpoint without importing the optional solver."""
    try:
        point = tuple(float(component) for component in value)
    except (TypeError, ValueError, OverflowError) as error:
        raise ValueError(f"{name} must contain three finite coordinates") from error
    if len(point) != 3 or not all(isfinite(component) for component in point):
        raise ValueError(f"{name} must contain three finite coordinates")
    return point


def _scalar(value: float, name: str, *, allow_zero: bool = False) -> float:
    """Validate a positive property, or a nonnegative submerged weight."""
    try:
        number = float(value)
    except (TypeError, ValueError, OverflowError) as error:
        raise ValueError(f"{name} must be finite") from error
    if not isfinite(number) or number < 0 or (number == 0 and not allow_zero):
        requirement = "nonnegative" if allow_zero else "positive"
        raise ValueError(f"{name} must be finite and {requirement}")
    return number


def solve_single_line(
    endpoint_a: Sequence[float],
    endpoint_b: Sequence[float],
    unstretched_length: float,
    ea: float,
    submerged_weight_per_length: float,
) -> SingleLineResult:
    """Solve one uniform elastic line using the optional MoorPy library.

    Args:
        endpoint_a: Three finite global Cartesian coordinates in m.
        endpoint_b: Three finite global Cartesian coordinates in m, distinct
            from A. Either endpoint may be higher.
        unstretched_length: Positive unstretched line length in m.
        ea: Positive axial rigidity EA in N, using a linear elastic law.
        submerged_weight_per_length: Nonnegative downward weight in N/m per
            unstretched length. Zero selects a tension-only straight line.

    Returns:
        Endpoint loads in N following the axis and sign convention in
        SingleLineResult. Coordinates may be translated or rotated about z.

    Raises:
        ValueError: Inputs violate the stated geometry or property domain.
        ImportError: MoorPy or one of its dependencies is unavailable.
        RuntimeError: MoorPy fails to solve, returns nonfinite loads, or uses
            its approximate fallback instead of a catenary solution.

    Notes:
        Tested against MoorPy 1.3.0. There is no seabed, surface, current,
        dynamics, multibody equilibrium, or material-table lookup. Weighted
        lines use MoorPy's default displacement tolerance and vertical-line
        approximation; this wrapper returns forces only, not stiffness.
    """
    a = _endpoint(endpoint_a, "endpoint_a")
    b = _endpoint(endpoint_b, "endpoint_b")
    length = _scalar(unstretched_length, "unstretched_length")
    stiffness = _scalar(ea, "ea")
    weight = _scalar(
        submerged_weight_per_length, "submerged_weight_per_length", allow_zero=True,
    )
    dx, dy, dz = (end - start for start, end in zip(a, b))
    horizontal = hypot(dx, dy)
    span = hypot(horizontal, dz)
    if not isfinite(span) or not isfinite(span / length):
        raise ValueError("endpoint separation and strain must be finite")
    if span == 0:
        raise ValueError("endpoint_a and endpoint_b must be distinct")

    try:
        if weight == 0:
            from moorpy.nonlinear import nonlinear
        else:
            from moorpy.Catenary import catenary
    except ImportError as error:
        raise ImportError(
            "MoorPy is required for solve_single_line; "
            "install it with: pip install moorpy==1.3.0"
        ) from error

    try:
        if weight == 0:
            # A linear tension-strain curve: T=EA*strain. The upper node
            # span/L exceeds the actual strain span/L-1, avoiding clipping
            # at any prescribed extension. Zero strain has zero tension.
            upper_strain = span / length
            planar = nonlinear(
                horizontal, abs(dz), length,
                [0.0, upper_strain], [0.0, stiffness * upper_strain], weight,
            )
        else:
            # In MoorPy 1.3.0 negative CB is the depth below A. Infinite
            # clearance places the mathematical seabed out of reach for
            # any sag, independent of coordinates and elastic extension.
            planar = catenary(horizontal, dz, length, stiffness, weight, CB=-float("inf"))
    except Exception as error:
        # Avoid echoing solver input or diagnostics into the public API.
        raise RuntimeError("MoorPy single-line equilibrium failed") from error

    ah, av, bh, bv, info = planar
    # MoorPy 1.3.0 nonlinear does not restore the sign for descending
    # endpoints. Solve with a nonnegative rise and reflect the loads back.
    if weight == 0 and dz < 0:
        av, bv = -av, -bv
    # ProfileType -1 is MoorPy 1.3.0's approximate taut fallback, not an
    # accepted converged catenary; do not silently return it as equilibrium.
    if info.get("error") or info.get("ProfileType") == -1:
        raise RuntimeError("MoorPy did not converge to single-line equilibrium")
    ux, uy = (dx / horizontal, dy / horizontal) if horizontal else (0.0, 0.0)
    force_a = (float(ah * ux), float(ah * uy), float(av))
    force_b = (float(bh * ux), float(bh * uy), float(bv))
    if not all(isfinite(force) for force in (*force_a, *force_b)):
        raise RuntimeError("MoorPy returned nonfinite endpoint forces")
    return SingleLineResult(force_a=force_a, force_b=force_b)
