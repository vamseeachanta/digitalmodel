"""Harmonic radial geometry of an imperfect pipe, in SI units.

The method extracted from a legacy engineering workbook is
``r(theta) = D/2 * (1 + S*cos(2*theta))``, with ``S = (q/D)/2`` and
``q = maximum diameter - minimum diameter``. Coordinates are ``r*cos(theta)``
and ``r*sin(theta)``; the reference circle has radius ``D/2``. The returned
radial difference is the difference of the two Cartesian vector lengths.
Angles are in degrees at the boundary. Lengths and areas are metres and m^2.

``pi * max(x) * max(y)`` is an axis-product area proxy from the sampled
points. It is NOT the enclosed area of the harmonic curve, which is
``pi*(D/2)^2*(1 + S^2/2)``. Coordinate maxima can depend on the sample grid.
The optional geometric interpretation of enlarged axes is not established;
the axis offset is therefore a required argument to the summary function.

References as stated in the source: API 5L code, 44th edition,
Section 9.11.3.1, Table 10; DNV-OS-F101, clause not stated in source.
These references concern out-of-roundness limits, not a strength assessment.
The radial method is attributed to a page-48 geometry reference in the source;
that reference has not been independently qualified here.

Input gaps: policy fractions, absolute caps, their applicability, the DNV
edition and clause, offset meaning, and sampling resolution require caller
evidence. No standards-derived coefficients or acceptance defaults are supplied.
The diameter/thickness output is a geometric ratio only. Wall stresses,
collapse, manufacturing acceptance, FE solution and plotting remain outside
this method. The geometric domain is D>0 and 0<=q<2D; it is not a code limit.
Unrepresentable derived quantities raise ValueError. Positive subnormal radial
amplitudes or half-ovality ratios are rejected to avoid lossy underflow. Analytic
radial deviations retain perturbations below Cartesian coordinate precision;
the coordinates alone may not resolve such perturbations.
"""

from __future__ import annotations

import math
import sys
from dataclasses import dataclass, fields
from typing import Iterable

from digitalmodel.cad.units import resolve_units
from digitalmodel.drilling_riser.section import cross_sectional_area
from digitalmodel.structural.analysis.wall_thickness import PipeGeometry
from digitalmodel.structural.ovality.workflow import _out_of_roundness


def _finite(name: str, value: float, *, positive: bool = False) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    try:
        finite = math.isfinite(value)
    except OverflowError as exc:
        raise ValueError(f"{name} must be a finite number") from exc
    if not finite or value < 0 or (positive and value == 0):
        raise ValueError(f"{name} must be finite and {'positive' if positive else 'nonnegative'}")


def _finite_result(name: str, value: float, *, positive: bool = False) -> float:
    if not math.isfinite(value) or (positive and value <= 0):
        raise ValueError(f"{name} is not representable")
    return value


@dataclass(frozen=True)
class HarmonicPipe:
    """Nominal diameter and extreme-diameter difference, both in metres."""

    diameter_m: float
    out_of_roundness_m: float

    def __post_init__(self) -> None:
        _finite("diameter_m", self.diameter_m, positive=True)
        _finite("out_of_roundness_m", self.out_of_roundness_m)
        if self.out_of_roundness_m / 2 >= self.diameter_m:
            raise ValueError("diameter difference must be less than twice the diameter")
        _finite_result("radius", self.diameter_m / 2, positive=True)
        if self.out_of_roundness_m:
            if self.out_of_roundness_m / 4 < sys.float_info.min:
                raise ValueError("radial amplitude is below the normal floating-point range")
            _finite_result("ovality", self.ovality_fraction, positive=True)
            _finite_result("half ovality", self.ovality_fraction / 2, positive=True)

    @property
    def ovality_fraction(self) -> float:
        """Reuse the scalar diameter-difference helper without D +/- q/2 cancellation.

        For q>0, the scaled positive pair (q, q/2) has difference q/2;
        twice the existing ratio gives q/D without subtracting nearby diameters.
        These are algebraic helper arguments, not measured extreme diameters.
        """
        q = self.out_of_roundness_m
        if q == 0:
            return 0.0
        if q / 2 == 0:
            raise ValueError("diameter difference is not representable")
        half_ratio = _out_of_roundness(self.diameter_m, q, q / 2)
        if half_ratio < sys.float_info.min:
            raise ValueError("half ovality is below the normal floating-point range")
        return _finite_result("ovality", 2 * half_ratio, positive=True)

    @classmethod
    def from_inches_and_mm(cls, diameter_in: float, out_of_roundness_mm: float) -> HarmonicPipe:
        """Convert boundary units with the repository's exact length factors."""
        _finite("diameter_in", diameter_in, positive=True)
        _finite("out_of_roundness_mm", out_of_roundness_mm)
        diameter_m = diameter_in * resolve_units("in", None).metres_per_unit
        difference_m = out_of_roundness_mm * resolve_units("mm", None).metres_per_unit
        if out_of_roundness_mm and difference_m == 0:
            raise ValueError("converted diameter difference is not representable")
        return cls(diameter_m, difference_m)


@dataclass(frozen=True)
class RoundnessLimit:
    """Required caller policy: diameter (m), relative fraction, absolute cap (m)."""

    diameter_m: float
    relative_limit_fraction: float
    absolute_limit_m: float


@dataclass(frozen=True)
class RoundnessAllowance:
    """Uncapped relative allowance and selected allowance, in metres."""

    relative_limit_m: float
    capped_limit_m: float


def capped_roundness_limit(inputs: RoundnessLimit) -> RoundnessAllowance:
    """Compute D*f and min(D*f, cap); no engineering limit is inferred.

    API 5L code, 44th edition, Section 9.11.3.1, Table 10 is named by
    a legacy engineering workbook. DNV-OS-F101: clause not stated in source.
    The caller must establish which fraction and cap apply.
    """
    _finite("diameter_m", inputs.diameter_m, positive=True)
    _finite("relative_limit_fraction", inputs.relative_limit_fraction)
    _finite("absolute_limit_m", inputs.absolute_limit_m)
    relative = _finite_result("relative limit", inputs.diameter_m * inputs.relative_limit_fraction)
    if inputs.relative_limit_fraction and relative == 0:
        raise ValueError("relative limit is not representable")
    return RoundnessAllowance(relative, min(relative, inputs.absolute_limit_m))


def diameter_thickness_ratio(diameter_m: float, thickness_m: float) -> float:
    """Reuse PipeGeometry.d_over_t for dimensions in metres; no adequacy check."""
    _finite("diameter_m", diameter_m, positive=True)
    _finite("thickness_m", thickness_m, positive=True)
    return PipeGeometry(
        outer_diameter=diameter_m,
        wall_thickness=thickness_m,
        fabrication_tolerance=0.0,
    ).d_over_t


@dataclass(frozen=True)
class ProfilePoint:
    """One angle (degrees) with radial and Cartesian outputs (metres)."""

    angle_deg: float
    radius_m: float
    x_m: float
    y_m: float
    circle_x_m: float
    circle_y_m: float
    radial_difference_m: float


def sample_profile(pipe: HarmonicPipe, angles_deg: Iterable[float]) -> tuple[ProfilePoint, ...]:
    """Evaluate each finite angle; periodic reduction prevents angular overflow.

    The supplied sampling grid is retained, including duplicated end points.
    Signed angles are allowed. Radial deviation is evaluated as q/4*cos(2*theta)
    to avoid cancellation when subtracting nearly equal coordinate norms.
    No interpolation or FE solution is performed.
    """
    points = []
    for angle in angles_deg:
        if isinstance(angle, bool) or not isinstance(angle, (int, float)):
            raise ValueError("angle_deg must be a finite number")
        _finite("angle magnitude", abs(angle))
        theta = math.radians(angle % 360)
        difference = pipe.out_of_roundness_m / 4 * math.cos(2 * theta)
        radius = pipe.diameter_m / 2 + difference
        _finite_result("profile radius", radius, positive=True)
        x, y = radius * math.cos(theta), radius * math.sin(theta)
        cx, cy = pipe.diameter_m / 2 * math.cos(theta), pipe.diameter_m / 2 * math.sin(theta)
        for name, value in (("x", x), ("y", y), ("circle x", cx), ("circle y", cy), ("radial difference", difference)):
            _finite_result(name, value)
        points.append(ProfilePoint(angle, radius, x, y, cx, cy, difference))
    return tuple(points)


@dataclass(frozen=True)
class ProfileSummary:
    """Sampled coordinate maxima, offset axes, area proxies and diameter difference."""

    max_x_m: float
    max_y_m: float
    offset_max_x_m: float
    offset_max_y_m: float
    axis_product_area_m2: float
    circle_area_m2: float
    max_radial_difference_m: float
    min_radial_difference_m: float
    diameter_difference_m: float


def summarize_profile(
    pipe: HarmonicPipe, angles_deg: Iterable[float], *, axis_offset_m: float
) -> ProfileSummary:
    """Summarize sampled geometry; require all four cardinal directions.

    Axes are max(x), max(y); offset axes add the required offset. Area proxy
    is pi*max(x)*max(y), circle area is pi*(D/2)^2, and diameter difference
    is twice the difference between maximum and minimum radial deviations.
    Cardinal samples within 1e-9 degrees prevent one-sided grids from implying
    a section area; they do not establish convergence of sampled coordinate maxima.
    """
    _finite("axis_offset_m", axis_offset_m)
    points = sample_profile(pipe, angles_deg)
    angles = tuple(point.angle_deg % 360 for point in points)
    if not all(
        any(
            math.isclose(angle, cardinal, rel_tol=0.0, abs_tol=1e-9)
            or (cardinal == 0 and math.isclose(angle, 360.0, rel_tol=0.0, abs_tol=1e-9))
            for angle in angles
        )
        for cardinal in (0, 90, 180, 270)
    ):
        raise ValueError("section summary requires all four cardinal directions")
    a, b = max(point.x_m for point in points), max(point.y_m for point in points)
    low = min(point.radial_difference_m for point in points)
    high = max(point.radial_difference_m for point in points)
    try:
        circle_area = cross_sectional_area(pipe.diameter_m, 0.0)
    except OverflowError as exc:
        raise ValueError("circle area is not representable") from exc
    result = ProfileSummary(a, b, a + axis_offset_m, b + axis_offset_m,
                            math.pi * a * b, circle_area,
                            high, low, (high - low) * 2)
    for field in fields(result):
        _finite_result(field.name, getattr(result, field.name), positive=field.name in {
            "max_x_m", "max_y_m", "axis_product_area_m2", "circle_area_m2"
        })
    return result
