# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Conditional water and liquid rates from a caller-supplied watercut table.

Oil and water volumes must share the same stated volume conditions. Watercut
is water divided by total liquid volume: q_water = q_oil * f / (1 - f).
Times are days, cumulative oil is bbl, and all rates are bbl/day. The curve
is piecewise linear in cumulative oil; it is neither clamped nor extrapolated.
No calibration, water-volume integration, recovery or reserves qualification
is supplied, and no facility or economic limit is inferred.
"""

import math
from bisect import bisect_left
from collections.abc import Iterable, Sequence
from dataclasses import dataclass
from numbers import Real

from .production_schedule import (
    WellSchedule,
    field_cumulative_production,
    field_rate_at_time,
)


@dataclass(frozen=True)
class WaterProductionRates:
    """Immutable oil volume in bbl, watercut fraction and rates in bbl/day."""

    cumulative_oil_bbl: float
    oil_rate_bbl_per_day: float
    watercut_fraction: float
    water_rate_bbl_per_day: float
    liquid_rate_bbl_per_day: float


def _finite_number(value: float) -> float:
    if not isinstance(value, Real) or isinstance(value, bool):
        raise ValueError("Knots must contain finite nonboolean real numbers")
    try:
        number = float(value)
    except OverflowError as error:
        raise ValueError("Knots must contain finite numbers") from error
    if not math.isfinite(number):
        raise ValueError("Knots must contain finite numbers")
    return number


def _validated_curve(
    knots: Sequence[tuple[float, float]],
) -> list[tuple[float, float]]:
    curve = []
    for knot in knots:
        try:
            volume, fraction = knot
        except (TypeError, ValueError) as error:
            raise ValueError("Each knot must contain two numbers") from error
        volume, fraction = _finite_number(volume), _finite_number(fraction)
        if volume < 0 or (curve and volume <= curve[-1][0]):
            raise ValueError("Cumulative oil knots must be nonnegative and increasing")
        if not 0 <= fraction < 1:
            raise ValueError("Watercut fractions must satisfy 0 <= f < 1")
        curve.append((volume, fraction))
    if len(curve) < 2:
        raise ValueError("At least two watercut knots are required")
    return curve


def water_rates_at_time(
    time_days: float,
    wells: Iterable[WellSchedule],
    watercut_knots: Sequence[tuple[float, float]],
) -> WaterProductionRates:
    """Return rates conditional on a fully validated cumulative-oil curve.

    Each knot is (cumulative_oil_bbl, watercut_fraction). Volumes must increase
    strictly; fractions may increase or decrease within 0 <= f < 1. Exact
    knots retain their fractions. All knots and wells, including inactive
    wells, are validated. A well iterable is consumed once without mutation.

    Oil segment endpoints follow the schedule API: a zero ramp jumps at
    startup, ramp/plateau endpoints retain peak rate, and volume at startup
    is zero. Finite negative field times give zero oil after validation;
    their zero cumulative volume must still lie inside the supplied curve.

    Raises:
        ValueError: Invalid time, well or knot; cumulative oil outside the
            curve; or a nonfinite production total or water/liquid rate.
    """
    curve = _validated_curve(watercut_knots)
    scheduled = tuple(wells)
    try:
        oil_rate = field_rate_at_time(time_days, scheduled)
        cumulative = field_cumulative_production(time_days, scheduled)
    except OverflowError as error:
        raise ValueError("Production totals must be finite") from error
    if not math.isfinite(cumulative) or not math.isfinite(oil_rate):
        raise ValueError("Production totals must be finite")
    if not curve[0][0] <= cumulative <= curve[-1][0]:
        raise ValueError("Cumulative oil is outside the supplied curve range")
    index = bisect_left([knot[0] for knot in curve], cumulative)
    volume, fraction = curve[index]
    if cumulative != volume:
        left_volume, left_fraction = curve[index - 1]
        weight = (cumulative - left_volume) / (volume - left_volume)
        fraction = left_fraction + (fraction - left_fraction) * weight
    water_rate = oil_rate * (fraction / (1 - fraction))
    liquid_rate = oil_rate + water_rate
    if not math.isfinite(water_rate) or not math.isfinite(liquid_rate):
        raise ValueError("Water and liquid rates must be finite")
    return WaterProductionRates(cumulative, oil_rate, fraction, water_rate, liquid_rate)
