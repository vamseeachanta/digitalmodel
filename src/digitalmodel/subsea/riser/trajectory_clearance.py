# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Minimum static clearance of piecewise-linear centerlines in Cartesian SI.

The construction follows elementary constrained Euclidean minimisation:
an interior minimum has a connector perpendicular to both segment directions;
a boundary minimum is an endpoint projected onto the other segment. Enumerate
those candidates over every segment pair. Cross products avoid subtracting
nearly equal squared dot products for nearly parallel directions.

Zero and one below are the exact endpoints of a segment's affine parameter,
not engineering constants. No angular or length cutoff is applied.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import dist, fsum, hypot, isfinite
from numbers import Real
from typing import Iterable, Sequence

Point = tuple[float, float, float]


@dataclass(frozen=True)
class ClearanceResult:
    """Distances in metres, centerline witnesses and zero-based segment indices.

    Parameters lie in [0, 1] on their reported segments. A repeated-point
    segment uses parameter zero. A negative surface gap denotes overlap of
    the radius offsets; it is not a contact force or a design verdict.
    """

    centerline_distance: float
    surface_gap: float
    witness_a: Point
    witness_b: Point
    segment_index_a: int
    segment_index_b: int
    parameter_a: float
    parameter_b: float


def _real(value: object, label: str) -> float:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{label} must contain finite real numbers")
    try:
        result = float(value)
    except (OverflowError, ValueError) as exc:
        raise ValueError(f"{label} exceeds floating-point range") from exc
    if not isfinite(result):
        raise ValueError(f"{label} must contain finite real numbers")
    return result


def _centerline(values: Iterable[Sequence[float]], label: str) -> tuple[Point, ...]:
    try:
        rows = tuple(tuple(row) for row in values)
    except TypeError as exc:
        raise ValueError(f"{label} must be an array of three-coordinate points") from exc
    if len(rows) < 2 or any(len(row) != 3 for row in rows):
        raise ValueError(f"{label} must have shape (N, 3), with N at least 2")
    return tuple((_real(x, label), _real(y, label), _real(z, label)) for x, y, z in rows)


def _finite(value: float) -> float:
    if not isfinite(value):
        raise ValueError("geometry exceeds floating-point range")
    return value


def _sub(a: Point, b: Point) -> Point:
    return (_finite(a[0] - b[0]), _finite(a[1] - b[1]), _finite(a[2] - b[2]))


def _dot(a: Point, b: Point) -> float:
    return _finite(fsum(x * y for x, y in zip(a, b)))


def _cross(a: Point, b: Point) -> Point:
    return (
        _finite(a[1] * b[2] - a[2] * b[1]),
        _finite(a[2] * b[0] - a[0] * b[2]),
        _finite(a[0] * b[1] - a[1] * b[0]),
    )


def _direction(start: Point, end: Point) -> tuple[Point, float]:
    delta = _sub(end, start)
    length = _finite(hypot(*delta))
    if not length:
        return delta, length
    return (delta[0] / length, delta[1] / length, delta[2] / length), length


def _at(start: Point, end: Point, parameter: float) -> Point:
    # Convex interpolation keeps endpoint witnesses exact and in segment bounds.
    x, y, z = ((1 - parameter) * a + parameter * b for a, b in zip(start, end))
    return x, y, z


def _projection(point: Point, start: Point, end: Point) -> float:
    direction, length = _direction(start, end)
    if not length:
        return 0.0
    along = _dot(_sub(point, start), direction)
    return min(length, max(0.0, along)) / length


def _segment_minimum(
    a0: Point, a1: Point, b0: Point, b1: Point,
) -> tuple[float, Point, Point, float, float]:
    # All four edges of the parameter square, including corners and point cases.
    parameters = [
        (0.0, _projection(a0, b0, b1)),
        (1.0, _projection(a1, b0, b1)),
        (_projection(b0, a0, a1), 0.0),
        (_projection(b1, a0, a1), 1.0),
    ]
    u, length_a = _direction(a0, a1)
    v, length_b = _direction(b0, b1)
    normal = _cross(u, v)
    sine = hypot(*normal)
    if sine and length_a and length_b:
        normal_unit = (normal[0] / sine, normal[1] / sine, normal[2] / sine)
        offset = _sub(b0, a0)
        # Solve offset = along_a*u - along_b*v + perpendicular connector.
        along_a = _dot(_cross(offset, v), normal_unit) / sine
        along_b = _dot(_cross(offset, u), normal_unit) / sine
        if 0 <= along_a <= length_a and 0 <= along_b <= length_b:
            parameters.append((along_a / length_a, along_b / length_b))

    candidates = []
    for parameter_a, parameter_b in parameters:
        point_a = _at(a0, a1, parameter_a)
        point_b = _at(b0, b1, parameter_b)
        candidates.append((
            _finite(dist(point_a, point_b)), point_a, point_b,
            parameter_a if length_a else 0.0,
            parameter_b if length_b else 0.0,
        ))
    return min(candidates, key=lambda candidate: candidate[0])


def minimum_surface_gap(
    centerline_a: Iterable[Sequence[float]],
    centerline_b: Iterable[Sequence[float]],
    radius_a: float,
    radius_b: float,
) -> ClearanceResult:
    """Return minimum centerline distance minus two constant explicit radii.

    Args:
        centerline_a: At least two Cartesian points of shape (N, 3), in metres.
        centerline_b: At least two Cartesian points of shape (M, 3), in metres.
        radius_a: Finite nonnegative radius in metres, constant along A.
        radius_b: Finite nonnegative radius in metres, constant along B.

    Returns:
        Minimum distances, centerline witnesses, segment indices and parameters.
        Ties retain the first candidate in segment traversal order; witnesses
        need not be unique. Input arrays are neither modified nor retained.

    Raises:
        ValueError: Malformed, non-real or nonfinite coordinates/radii, negative
            radii, or geometry outside representable floating-point range.

    Notes:
        Repeated points define zero-length point segments. The cost is O(N*M)
        time and O(N+M) input storage. Computation uses binary64 arithmetic;
        use a local Cartesian origin to reduce coordinate cancellation.
        This is static geometry of radius offsets with rounded end caps. It
        infers no hydrodynamic factor, motion envelope or standard spacing.
    """
    a = _centerline(centerline_a, "centerline_a")
    b = _centerline(centerline_b, "centerline_b")
    ra = _real(radius_a, "radius_a")
    rb = _real(radius_b, "radius_b")
    if ra < 0 or rb < 0:
        raise ValueError("radius_a and radius_b must be nonnegative")
    radii = _finite(ra + rb)
    best = None
    try:
        for i, (a0, a1) in enumerate(zip(a, a[1:])):
            for j, (b0, b1) in enumerate(zip(b, b[1:])):
                distance, point_a, point_b, ta, tb = _segment_minimum(a0, a1, b0, b1)
                if best is None or distance < best.centerline_distance:
                    best = ClearanceResult(
                        distance, _finite(distance - radii), point_a, point_b,
                        i, j, ta, tb,
                    )
    except OverflowError as exc:
        raise ValueError("geometry exceeds floating-point range") from exc
    if best is None:  # Both validated centerlines contain a segment.
        raise RuntimeError("no segment pairs evaluated")
    return best
