# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Causal warning checks against independently specified synthetic events."""

from dataclasses import replace
import math

import pytest

from digitalmodel.marine_ops.self_elevating_monitoring import (
    FAILURE_MODES,
    SOURCES,
    AlertRule,
    evaluate,
    synthetic_cases,
)


def test_settlement_stops_without_alert():
    case = synthetic_cases()["settled"]
    assert not evaluate(case.time_s, case.channels, case.rules)
    assert case.channels["settlement_m"][-1] < 0.2


def test_acceleration_warns_before_synthetic_punch_through():
    case = synthetic_cases()["accelerating"]
    alerts = evaluate(case.time_s, case.channels, case.rules)
    assert {a.rule for a in alerts} == {r.name for r in case.rules}
    assert all(a.time_s < case.event_s for a in alerts)
    acceleration = next(a for a in alerts if a.rule == "settlement acceleration")
    assert acceleration.time_s < 400
    # The held preload distinguishes continuing settlement from a load ramp.
    at = case.time_s.index(acceleration.time_s)
    assert len(set(case.channels["preload_kn"][at - 3:at + 1])) == 1


def test_storm_both_instruments_warn_before_respective_limits():
    case = synthetic_cases()["storm"]
    alerts = evaluate(case.time_s, case.channels, case.rules)
    assert len(alerts) == 2
    for alert in alerts:
        channel = next(r.channel for r in case.rules if r.name == alert.rule)
        limit_time = next(t for t, v in zip(case.time_s, case.channels[channel])
                          if abs(v) >= case.limits[channel])
        assert alert.time_s < limit_time
        assert alert.time_s < case.event_s


def test_alert_is_causal_and_requires_consecutive_samples():
    rule = AlertRule("tilt", "tilt_deg", "magnitude", 0.5, "deg", 2, 5)
    values = [0, 0.5, 0, -0.5, -0.6, 99]
    assert not evaluate([0, 1, 2, 3], {"tilt_deg": values[:4]}, [rule])
    full = evaluate(list(range(6)), {"tilt_deg": values}, [rule])
    prefix = evaluate(list(range(5)), {"tilt_deg": values[:5]}, [rule])
    assert full == prefix
    assert full[0].time_s == 4
    assert full[0].value == 0.6


def test_irregular_clock_derivatives_use_elapsed_time():
    time = [0, 2, 5, 9]
    # d²(t²)/dt² = 2, including at unequal intervals.
    rule = AlertRule("accel", "depth_m", "acceleration", 2, "m/s²", 2, 10)
    alerts = evaluate(time, {"depth_m": [t*t for t in time]}, [rule])
    assert alerts[0].time_s == 9
    assert alerts[0].value == pytest.approx(2)
    rate = replace(rule, metric="rise_rate", threshold=6, unit="m/s", persistence=1)
    assert evaluate(time, {"depth_m": [t*t for t in time]}, [rate])[0].time_s == 5


@pytest.mark.parametrize("time,values", [
    ([], []), ([0, 0], [0, 1]), ([1, 0], [0, 1]),
    ([0, math.nan], [0, 1]), ([0, 1], [0, math.inf]),
    ([0, 1], [0]), ([0, 10], [0, 1]),
])
def test_invalid_or_gapped_measurements_are_rejected(time, values):
    rule = AlertRule("tilt", "tilt_deg", "magnitude", 0.5, "deg", 1, 5)
    with pytest.raises(ValueError):
        evaluate(time, {"tilt_deg": values}, [rule])


def test_missing_channel_and_duplicate_rule_name_rejected():
    rule = AlertRule("tilt", "tilt_deg", "magnitude", 0.5, "deg", 1, 5)
    with pytest.raises(ValueError, match="missing channel"):
        evaluate([0], {}, [rule])
    with pytest.raises(ValueError, match="unique"):
        evaluate([0], {"tilt_deg": [0]}, [rule, rule])


@pytest.mark.parametrize("changes", [
    {"threshold": 0}, {"threshold": math.nan}, {"threshold": math.inf},
    {"metric": "unknown"}, {"persistence": 0}, {"persistence": 1.5},
    {"max_gap_s": 0}, {"max_gap_s": math.inf}, {"unit": ""},
])
def test_invalid_rule_is_rejected(changes):
    rule = AlertRule("tilt", "tilt_deg", "magnitude", 0.5, "deg", 1, 5)
    with pytest.raises(ValueError):
        replace(rule, **changes)


def test_mapping_covers_six_modes_and_has_public_source_basis():
    assert len(FAILURE_MODES) == 6
    for mode in FAILURE_MODES:
        assert mode.instruments and mode.precursor and mode.logic and mode.lead_time
        assert mode.sources
        assert set(mode.sources) <= SOURCES.keys()
    for source in SOURCES.values():
        assert source.url.startswith("https://")
        assert source.terms and source.retrieved == "2026-10-27"
