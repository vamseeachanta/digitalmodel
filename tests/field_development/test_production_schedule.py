# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic independent wells: days, bbl/day, bbl and decline in 1/day.

These hand calculations are mathematical examples, not measured type curves,
engineering defaults or reserves estimates. Ramp triangles, plateau rectangles
and finite exponential integrals supply the expectations without scalar calls.
"""

import math
from dataclasses import FrozenInstanceError, asdict, replace

import pytest

from digitalmodel.field_development.production_schedule import (
    WellSchedule,
    field_cumulative_production,
    field_rate_at_time,
)

FUNCTIONS = [field_rate_at_time, field_cumulative_production]
PROFILE_FIELDS = (
    "ramp_duration_days",
    "plateau_duration_days",
    "plateau_rate_bbl_per_day",
    "decline_rate_per_day",
)
INVALID = [-1.0, math.nan, math.inf, -math.inf, True, False, "1", None, 1j, 10**400]


@pytest.fixture
def wells():
    return [WellSchedule(start, 10.0, 10.0, 100.0, 0.1) for start in (0.0, 10.0)]


@pytest.mark.parametrize("function,index", zip(FUNCTIONS, (0, 1)))
@pytest.mark.parametrize(
    "time_days,expected",
    [
        (-1.0, (0.0, 0.0)),
        (0.0, (0.0, 0.0)),
        (5.0, (50.0, 125.0)),
        (10.0, (100.0, 500.0)),
        (15.0, (150.0, 1125.0)),
        (20.0, (200.0, 2000.0)),
        (30.0, (100 * (1 + math.exp(-1)), 3000 + 1000 * (1 - math.exp(-1)))),
        (
            40.0,
            (
                100 * (math.exp(-2) + math.exp(-1)),
                3000 + 1000 * (2 - math.exp(-2) - math.exp(-1)),
            ),
        ),
    ],
)
def test_staggered_totals(wells, function, index, time_days, expected):
    """Each well contributes a 500 bbl ramp and a 1000 bbl plateau.

    At day 15 the second ramp adds 125 bbl; at day 30 the first tail adds
    1000*(1-exp(-1)) bbl. Coincident plateaus add without a facility cap.
    """
    assert function(time_days, wells) == pytest.approx(
        expected[index], rel=1e-12, abs=1e-10
    )


@pytest.mark.parametrize("function,index", zip(FUNCTIONS, (0, 1)))
@pytest.mark.parametrize(
    "ramp,plateau,decline,elapsed,expected",
    [
        (10, 10, 0.1, -1, (0, 0)),
        (10, 10, 0.1, 0, (0, 0)),
        (10, 10, 0.1, 10, (100, 500)),
        (10, 10, 0.1, 20, (100, 1500)),
        (0, 10, 0.1, -1, (0, 0)),
        (0, 10, 0.1, 0, (100, 0)),
        (0, 10, 0.1, 10, (100, 1000)),
        (0, 10, 0.1, 20, (100 * math.exp(-1), 1000 + 1000 * (1 - math.exp(-1)))),
        (10, 0, 0.1, 10, (100, 500)),
        (10, 0, 0.1, 20, (100 * math.exp(-1), 500 + 1000 * (1 - math.exp(-1)))),
        (0, 0, 0.1, 0, (100, 0)),
        (0, 0, 0.1, 10, (100 * math.exp(-1), 1000 * (1 - math.exp(-1)))),
        (10, 10, 0, 30, (100, 2500)),
        (0, 0, 0, 10, (100, 1000)),
    ],
)
def test_single_well_endpoints(
    function, index, ramp, plateau, decline, elapsed, expected
):
    """Zero ramp jumps at startup; zero plateau starts decline at ramp end.

    Exact endpoints retain the peak, N(start)=0, and D=0 holds the peak.
    A seven-day offset tests elapsed well time rather than field time.
    """
    well = WellSchedule(7.0, ramp, plateau, 100.0, decline)
    assert function(7.0 + elapsed, [well]) == pytest.approx(
        expected[index], rel=1e-12, abs=1e-10
    )


@pytest.mark.parametrize("function", FUNCTIONS)
@pytest.mark.parametrize("time_days", [-1e308, -1.0, 0.0, 10.0])
def test_empty_schedule(function, time_days):
    assert function(time_days, []) == 0.0


@pytest.mark.parametrize("function", FUNCTIONS)
def test_negative_field_time_avoids_elapsed_overflow(wells, function):
    assert function(-1e308, [replace(wells[0], start_day=1e308)]) == 0.0


@pytest.mark.parametrize("function,index", zip(FUNCTIONS, (0, 1)))
def test_generator_matches_list_without_mutation(wells, function, index):
    before = [asdict(well) for well in wells]
    expected = pytest.approx((150.0, 1125.0)[index], rel=1e-12, abs=1e-10)
    assert function(15.0, iter(wells)) == expected
    assert function(15.0, wells) == expected
    assert [asdict(well) for well in wells] == before
    assert [well.start_day for well in wells] == [0.0, 10.0]


def test_schedule_is_frozen(wells):
    with pytest.raises(FrozenInstanceError):
        wells[0].start_day = 1.0


@pytest.mark.parametrize("function", FUNCTIONS)
def test_sum_preserves_small_well_contributions(function):
    """At day 1, immediate constant profiles have equal rate and volume.

    Summing 1e16+1+1 preserves two small wells with an accurate sum.
    """
    wells = [WellSchedule(0, 0, 10, rate, 0) for rate in (1e16, 1, 1)]
    assert function(1.0, wells) == 10000000000000002.0


@pytest.mark.parametrize("function", FUNCTIONS)
@pytest.mark.parametrize("time_days", INVALID[1:])
@pytest.mark.parametrize("empty", [False, True])
def test_invalid_field_time(wells, function, time_days, empty):
    with pytest.raises(ValueError, match="time_days"):
        function(time_days, [] if empty else wells)


@pytest.mark.parametrize("function", FUNCTIONS)
@pytest.mark.parametrize("start_day", INVALID)
@pytest.mark.parametrize("time_days", [-1.0, 0.0, 15.0])
def test_invalid_start_day(wells, function, start_day, time_days):
    bad = replace(wells[1], start_day=start_day)
    with pytest.raises(ValueError, match="start_day"):
        function(time_days, [wells[0], bad])


@pytest.mark.parametrize("function", FUNCTIONS)
@pytest.mark.parametrize("parameter", PROFILE_FIELDS)
@pytest.mark.parametrize("value", INVALID)
@pytest.mark.parametrize("time_days", [-1.0, 0.0, 15.0])
def test_invalid_inactive_profile(wells, function, parameter, value, time_days):
    """Bad future wells also invalidate negative and zero field times."""
    bad = replace(wells[1], start_day=100.0, **{parameter: value})
    with pytest.raises(ValueError, match=parameter):
        function(time_days, iter([wells[0], bad]))
