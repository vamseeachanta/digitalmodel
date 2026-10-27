# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Illustrative three-leg statics, drag loads and sampled operating bounds.

This is equilibrium screening, without structural capacity, foundation combined
loading, dynamic response, load factors or a qualified environmental model.
"""

from __future__ import annotations

import math
from collections.abc import Callable, Sequence
from dataclasses import dataclass

from digitalmodel.geotechnical.spudcan import finite


def _signed_finite(name: str, value: float) -> None:
    if isinstance(value, bool) or not isinstance(value, (float, int)) or not math.isfinite(value):
        raise ValueError(f'{name} must be a finite number')


@dataclass(frozen=True)
class StaticScreen:
    """Signed reactions and separate checks; preload is not structural capacity."""

    reactions_kn: tuple[float, float, float]
    contact_ok: bool
    preload_ok: bool
    sliding_ok: bool
    maximum_preload_utilization: float
    sliding_utilization: float | None

    @property
    def admissible(self) -> bool:
        """Return whether all three illustrative inequalities hold."""
        return self.contact_ok and self.preload_ok and self.sliding_ok

    @property
    def failing_checks(self) -> tuple[str, ...]:
        """Return the separate reasons this static state is inadmissible."""
        return tuple(name for name, ok in (
            ('contact', self.contact_ok), ('preload', self.preload_ok),
            ('sliding', self.sliding_ok)) if not ok)


def screen_three_leg_unit(
    *, vertical_load_kn: float, supports_m: Sequence[tuple[float, float]],
    moment_x_knm: float, moment_y_knm: float, horizontal_force_kn: float,
    preload_per_leg_kn: float, sliding_resistance_kn: float,
) -> StaticScreen:
    """Solve force and moment equilibrium for three noncollinear supports.

    Upward reactions Ri obey sum(Ri)=V, sum(xi Ri)=My and sum(yi Ri)=-Mx.
    Applied moments are external, about the same origin as support coordinates.
    Include any eccentric gravity moment in the supplied external moments.
    Zero or negative reaction fails contact. Sliding resistance is supplied,
    independently qualified by the caller; preload is a load-history comparator.
    """
    finite('vertical load', vertical_load_kn, strict=True)
    finite('preload', preload_per_leg_kn, strict=True)
    finite('horizontal force', horizontal_force_kn)
    finite('sliding resistance', sliding_resistance_kn)
    _signed_finite('moment x', moment_x_knm)
    _signed_finite('moment y', moment_y_knm)
    if len(supports_m) != 3 or any(len(point) != 2 for point in supports_m):
        raise ValueError('exactly three two-dimensional support coordinates are required')
    for x, y in supports_m:
        _signed_finite('support x', x)
        _signed_finite('support y', y)
    (x0, y0), (x1, y1), (x2, y2) = supports_m
    a, b, c, d = x0 - x2, x1 - x2, y0 - y2, y1 - y2
    # Numerical geometry tolerance relative to squared coordinate differences;
    # not a physical minimum support spacing or an acceptance criterion.
    determinant = a * d - b * c
    scale = max(abs(a), abs(b), abs(c), abs(d))
    if scale == 0 or abs(determinant) <= 1e-12 * scale**2:
        raise ValueError('supports must be noncollinear and numerically well separated')
    rhs_x = moment_y_knm - x2 * vertical_load_kn
    rhs_y = -moment_x_knm - y2 * vertical_load_kn
    r0 = (rhs_x * d - b * rhs_y) / determinant
    r1 = (a * rhs_y - rhs_x * c) / determinant
    reactions = (r0, r1, vertical_load_kn - r0 - r1)
    for value in reactions:
        _signed_finite('reaction', value)
    # The 1e-10 contact margin resolves floating-point zero relative to V.
    return StaticScreen(
        reactions, min(reactions) > 1e-10 * vertical_load_kn,
        max(reactions) <= preload_per_leg_kn,
        horizontal_force_kn <= sliding_resistance_kn,
        max(reactions) / preload_per_leg_kn,
        horizontal_force_kn / sliding_resistance_kn if sliding_resistance_kn > 0 else None,
    )


@dataclass(frozen=True)
class EnvironmentalActions:
    """Co-directional drag forces and their assumed application heights."""

    horizontal_force_kn: float
    moment_y_knm: float
    wind_force_kn: float
    water_force_kn: float
    surface_orbital_velocity_m_s: float


def environmental_actions(
    *, hs_m: float, tp_s: float, wind_m_s: float, current_m_s: float,
    water_depth_m: float, air_gap_m: float, wind_area_m2: float,
    submerged_area_m2: float, wind_drag_coefficient: float,
    water_drag_coefficient: float,
) -> EnvironmentalActions:
    """Calculate illustrative aligned wind and water drag, F=0.5 rho Cd A U².

    Water velocity uses a linear regular-wave surface proxy with H=Hs:
    u = pi H/T coth(kh), omega²=g k tanh(kh). This is an illustrative assumption,
    not a crest, significant response or extreme design-wave conversion.
    Apply that surface velocity to an assumed effective submerged area, then
    apply its resultant at half water depth. Wind acts at depth plus air gap.
    Ignore inertia, shielding, member geometry and dynamic amplification.

    Reference air density 1.225 kg/m3 follows the AeroDyn input in Appendix B.1
    of Definition of a 5-MW Reference Wind Turbine for Offshore System
    Development (2009): https://www.osti.gov/servlets/purl/947422
    Its Appendix A gravity 9.80665 m/s2 is rounded here to 9.81 m/s2. Reference
    water density 1025 kg/m3 follows Table 4-2 of Definition of the Floating
    System for Phase IV of OC3 (2010): https://www.osti.gov/servlets/purl/979456
    These are adopted reference constants, not measured site fluid properties.
    H/depth <= 0.3 is an illustrative exclusion assumed, based on previous
    experience, not a cited breaking limit or a linear-theory validity test.
    The 80 dispersion bisections are a numerical iteration budget.
    """
    inputs = locals().copy()
    for name, value in inputs.items():
        finite(name, value, strict=name in ('tp_s', 'water_depth_m'))
    if hs_m > 0.3 * water_depth_m:
        raise ValueError('illustrative linear-wave proxy requires H/depth <= 0.3')
    omega2 = (2 * math.pi / tp_s)**2
    lo, hi = 0.0, max(1.0 / water_depth_m, omega2 / 9.81)
    while 9.81 * hi * math.tanh(hi * water_depth_m) < omega2:
        hi *= 2
    for _ in range(80):
        mid = (lo + hi) / 2
        if 9.81 * mid * math.tanh(mid * water_depth_m) < omega2:
            lo = mid
        else:
            hi = mid
    orbital = math.pi * hs_m / tp_s / math.tanh((lo + hi) / 2 * water_depth_m)
    wind = 0.5 * 1.225 * wind_drag_coefficient * wind_area_m2 * wind_m_s**2 / 1000
    water = 0.5 * 1025 * water_drag_coefficient * submerged_area_m2 * (current_m_s + orbital)**2 / 1000
    return EnvironmentalActions(
        wind + water, wind * (water_depth_m + air_gap_m) + water * water_depth_m / 2,
        wind, water, orbital,
    )


@dataclass(frozen=True)
class StaticEnvelopePoint:
    """Last admissible sampled Hs and first failed Hs; no interpolation claim."""

    tp_s: float
    hs_limit_m: float | None
    first_failing_hs_m: float | None
    status: str
    governing_checks: tuple[str, ...]


def static_operating_envelope(
    periods_s: Sequence[float], heights_m: Sequence[float],
    evaluate: Callable[[float, float], StaticScreen],
) -> list[StaticEnvelopePoint]:
    """Bound static admissibility on an explicit Hs/Tp grid.

    Wind, current, headings and other inputs are supplied by the evaluation
    callback. A failed state followed by a safe state is rejected rather than
    hidden. Results are static screening bounds, not forecast motion limits.
    """
    if not periods_s or not heights_m:
        raise ValueError('period and height grids must not be empty')
    for period in periods_s:
        finite('period', period, strict=True)
    for height in heights_m:
        finite('height', height)
    if len(set(periods_s)) != len(periods_s):
        raise ValueError('periods must be distinct')
    if any(b <= a for a, b in zip(heights_m, heights_m[1:])):
        raise ValueError('heights must be strictly increasing')
    result = []
    for period in periods_s:
        last_safe, first_failed, checks = None, None, ()
        for height in heights_m:
            state = evaluate(height, period)
            if state.admissible:
                if first_failed is not None:
                    raise ValueError('safe re-entry after a failed state requires a different envelope representation')
                last_safe = height
            elif first_failed is None:
                first_failed, checks = height, state.failing_checks
        status = 'grid_capped' if first_failed is None else 'bounded_on_grid'
        if last_safe is None:
            status = 'no_admissible_sample'
        result.append(StaticEnvelopePoint(period, last_safe, first_failed, status, checks))
    return result
