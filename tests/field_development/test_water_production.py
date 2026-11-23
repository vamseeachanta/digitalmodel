# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand-derived rates in bbl/day from synthetic oil volumes in bbl."""

import math
from dataclasses import FrozenInstanceError, asdict, replace

import pytest

from digitalmodel.field_development.production_schedule import WellSchedule
from digitalmodel.field_development.water_production import water_rates_at_time

INVALID = [math.nan, math.inf, -math.inf, True, False, "1", None, 1j, 10**400]
CURVE = [(0, 0.1), (2250, 0.3)]


@pytest.fixture
def wells():
    return [WellSchedule(start, 10, 10, 100, 0.1) for start in (0, 10)]


def test_hand_rates_and_immutable_repeatable_result(wells):
    """At day 15: f=0.2, water=150*0.2/0.8, liquid=150/0.8."""
    before = [asdict(well) for well in wells], list(CURVE)
    result = water_rates_at_time(15, iter(wells), CURVE)
    assert asdict(result) == pytest.approx(
        dict(cumulative_oil_bbl=1125, oil_rate_bbl_per_day=150,
             watercut_fraction=0.2, water_rate_bbl_per_day=37.5,
             liquid_rate_bbl_per_day=187.5), rel=1e-12, abs=1e-10
    )
    assert result == water_rates_at_time(15, wells, CURVE)
    assert result == water_rates_at_time(15, iter(wells), CURVE)
    assert ([asdict(well) for well in wells], CURVE) == before
    with pytest.raises(FrozenInstanceError):
        result.watercut_fraction = 0


@pytest.mark.parametrize("time,volume,rate,fraction", [
    (-1, 0, 0, 0.1), (0, 0, 0, 0.1), (10, 500, 100, 0.3),
    (15, 1125, 150, 0.2), (20, 2000, 200, 0.06), (30, 0, 0, 0),
])
def test_exact_knots_interior_decreasing_and_zero(wells, time, volume, rate, fraction):
    curve = [(0, 0.1), (500, 0.3), (1125, 0.2), (2375, 0)]
    if time == 30:
        wells, curve = [], [(0, 0), (1, 0.5)]
    result = water_rates_at_time(time, wells, curve)
    assert result.cumulative_oil_bbl == pytest.approx(volume, rel=1e-12, abs=1e-10)
    assert result.oil_rate_bbl_per_day == pytest.approx(rate, rel=1e-12, abs=1e-10)
    assert result.watercut_fraction == pytest.approx(fraction, rel=1e-12, abs=1e-10)
    assert result.water_rate_bbl_per_day == pytest.approx(
        rate * fraction / (1 - fraction), rel=1e-12, abs=1e-10
    )


@pytest.mark.parametrize("time,expected", [(0, 0.1), (1, 0.2), (2, 0.3)])
def test_both_range_endpoints_and_interior_knot(time, expected):
    well = WellSchedule(0, 0, 10, 1, 0)
    result = water_rates_at_time(time, [well], [(0, 0.1), (1, 0.2), (2, 0.3)])
    assert result.watercut_fraction == expected


@pytest.mark.parametrize("curve", [
    [], [(0, 0.1)], [(0, 0), (0, 0.2)], [(1, 0), (0, 0.2)],
    [(-1, 0), (1, 0)], [(0, -0.1), (1, 0)], [(0, 0), (1, 1)],
    [(0, 0), (1, 1.1)], [(0,), (1, 0)], [(0, 0, 0), (1, 0)],
])
@pytest.mark.parametrize("time", [-1, 15])
def test_invalid_curve_even_at_inactive_time(wells, curve, time):
    with pytest.raises(ValueError):
        water_rates_at_time(time, wells, curve)


@pytest.mark.parametrize("value", INVALID)
@pytest.mark.parametrize("coordinate", [0, 1])
def test_invalid_knot_numbers(wells, value, coordinate):
    knot = [0, 0.1]
    knot[coordinate] = value
    with pytest.raises(ValueError):
        water_rates_at_time(-1, wells, [knot, (2250, 0.3)])


@pytest.mark.parametrize("time", INVALID)
def test_invalid_field_time(wells, time):
    with pytest.raises(ValueError):
        water_rates_at_time(time, wells, CURVE)


@pytest.mark.parametrize("parameter", [
    "start_day", "ramp_duration_days", "plateau_duration_days",
    "plateau_rate_bbl_per_day", "decline_rate_per_day",
])
@pytest.mark.parametrize("value", [-1, *INVALID])
@pytest.mark.parametrize("time", [-1, 0, 15])
def test_invalid_future_well(wells, parameter, value, time):
    future = replace(wells[1], start_day=100)
    future = replace(future, **{parameter: value})
    with pytest.raises(ValueError, match=parameter):
        water_rates_at_time(time, iter([wells[0], future]), CURVE)


@pytest.mark.parametrize("time,curve", [
    (-1, [(1, 0), (2250, 0.3)]), (15, [(0, 0), (1000, 0.3)]),
])
def test_no_extrapolation(wells, curve, time):
    with pytest.raises(ValueError, match="range"):
        water_rates_at_time(time, wells, curve)


@pytest.mark.parametrize("time,rate,fraction", [
    (10, 1e308, 0), (1, 1e308, 0),
    (0, 1e308, 0.9), (0, 1e308, 0.5),
])
def test_nonfinite_totals_and_rate_overflow(time, rate, fraction):
    """Reject infinite volume, overflowing sums, water and liquid rates."""
    wells = [WellSchedule(0, 0, 100, rate, 0)]
    if time == 1:
        wells *= 2
    with pytest.raises(ValueError, match="finite"):
        water_rates_at_time(time, wells, [(0, fraction), (1e308, fraction)])


def test_large_finite_rate_does_not_overflow_intermediate_product():
    well = WellSchedule(0, 0, 10, 1e308, 0)
    result = water_rates_at_time(0, [well], [(0, 0.1), (1, 0.1)])
    assert math.isfinite(result.water_rate_bbl_per_day)
    assert result.liquid_rate_bbl_per_day == pytest.approx(1e308 / 0.9)


def test_zero_fraction_preserves_oil_rate(wells):
    result = water_rates_at_time(15, wells, [(0, 0), (2250, 0)])
    assert result.water_rate_bbl_per_day == 0
    assert result.liquid_rate_bbl_per_day == 150
