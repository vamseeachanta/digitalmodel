# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Synthetic checks; numerical expectations are hand calculations below.

Curve knots and limits are assumed test inputs, not standard-derived values.
Removing any trigger, changing its stated inequality, extrapolating the curve,
or clearing the reassessment latch would violate the corresponding checks.
"""

import json
import runpy
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.marine_ops.penetration_monitoring import (
    Measurement,
    MonitoringCriteria,
    PenetrationMonitor,
    PlanBAction,
    PredictionPoint,
    Trigger,
)


@pytest.fixture
def curve():
    """A synthetic peak at (3 m, 180 kN), followed by falling resistance."""
    return tuple(PredictionPoint(*row) for row in (
        (0, 0, 0, 0.5), (1, 100, 0.5, 1.5), (2, 160, 1.5, 2.5),
        (3, 180, 2.5, 3.5), (4, 140, 3.5, 4.5), (5, 220, 4.5, 5.5),
    ))


@pytest.fixture
def criteria():
    return MonitoringCriteria(
        stiffness_drop_fraction=0.5,
        rapid_penetration_rate_m_s=0.015,
        constant_load_tolerance_kn=1,
        upward_movement_tolerance_m=0.002,
    )


def test_interpolates_band_and_includes_both_boundaries(curve, criteria):
    # At 50 kN: fraction 50/100; lower = 0.25, upper = 1.0 m.
    for depth in (0.25, 1.0):
        result = PenetrationMonitor(curve, criteria).update(Measurement(0, 50, depth))
        assert result.predicted_intervals_m == ((0.25, 1.0),)
        assert result.active_triggers == ()
        assert result.requires_reassessment is False


@pytest.mark.parametrize('depth', [0.24, 1.01])
def test_flags_penetration_outside_either_band_edge(curve, criteria, depth):
    result = PenetrationMonitor(curve, criteria).update(Measurement(0, 50, depth))
    assert result.active_triggers == (Trigger.OUTSIDE_BAND,)
    assert result.alerts[0].actions == (
        PlanBAction.HOLD_LOAD, PlanBAction.STOP_AND_REASSESS,
        PlanBAction.MOVE_ON_PAPER,
    )


def test_preserves_disconnected_intervals_on_layered_curve(curve, criteria):
    # At 160 kN: ascending depth 2 +/- 0.5; descending depth 3.5 +/- 0.5;
    # recovery depth 4.25 +/- 0.5. The last two overlap into [3, 4.75].
    monitor = PenetrationMonitor(curve, criteria)
    result = monitor.update(Measurement(0, 160, 2.75))
    assert result.predicted_intervals_m == ((1.5, 2.5), (3.0, 4.75))
    assert result.ambiguous_prediction is True
    assert Trigger.OUTSIDE_BAND in result.active_triggers


def test_horizontal_curve_segment_has_a_depth_interval(criteria):
    # A plateau at 100 kN extends from depth 1 to 2, with +/- 0.5 m bounds.
    curve = (PredictionPoint(1, 100, 0.5, 1.5), PredictionPoint(2, 100, 1.5, 2.5))
    result = PenetrationMonitor(curve, criteria).update(Measurement(0, 100, 2))
    assert result.predicted_intervals_m == ((0.5, 2.5),)
    assert result.active_triggers == ()


@pytest.mark.parametrize('load', [0, 230])
def test_does_not_extrapolate_outside_load_domain(curve, criteria, load):
    # The truncated curve spans 100..220 kN; 0 and 230 have no intersection.
    result = PenetrationMonitor(curve[1:], criteria).update(Measurement(0, load, 1))
    assert result.predicted_intervals_m == ()
    assert result.active_triggers == (Trigger.PREDICTION_UNAVAILABLE,)
    assert result.requires_reassessment is True


def test_stiffness_drop_is_flagged_before_the_predicted_peak(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    monitor.update(Measurement(0, 0, 0))
    first = monitor.update(Measurement(10, 100, 1))
    result = monitor.update(Measurement(20, 140, 2))
    # K1 = (100-0)/(1-0) = 100 kN/m. K2 = 40/1 = 40 kN/m.
    # Fractional drop = 1 - 40/100 = 0.6, above assumed threshold 0.5.
    assert first.stiffness_kn_m == 100
    assert first.active_triggers == ()
    assert result.stiffness_kn_m == 40
    assert result.stiffness_drop_fraction == pytest.approx(0.6)
    assert result.next_peak_penetration_m == 3
    assert result.active_triggers == (Trigger.STIFFNESS_DROP,)
    assert PlanBAction.REDUCE_TO_REVIEWED_STAGE in result.alerts[0].actions
    assert PlanBAction.HULL_TO_WATER in result.alerts[0].actions


def test_stiffness_drop_at_exact_fraction_threshold_is_flagged(curve, criteria):
    monitor = PenetrationMonitor(curve, replace(criteria, stiffness_drop_fraction=0.6))
    for sample in (Measurement(0, 0, 0), Measurement(10, 100, 1)):
        monitor.update(sample)
    assert Trigger.STIFFNESS_DROP in monitor.update(Measurement(20, 140, 2)).active_triggers


def test_stiffness_drop_remains_armed_deeper_than_nominal_peak(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    readings = (Measurement(0, 160, 2.4), Measurement(10, 170, 2.6),
                Measurement(20, 175, 3.1))
    results = [monitor.update(sample) for sample in readings]
    assert all(Trigger.OUTSIDE_BAND not in r.active_triggers for r in results)
    # K falls from 10/0.2 = 50 to 5/0.5 = 10 kN/m, below the peak load.
    assert results[-1].stiffness_drop_fraction == pytest.approx(0.8)
    assert results[-1].next_peak_penetration_m == 3
    assert Trigger.STIFFNESS_DROP in results[-1].active_triggers


@pytest.mark.parametrize('depths,loads', [
    ((0, 1, 2), (0, 100, 140)),
    ((0, 0.25, 0.75, 1.25, 1.75), (0, 25, 75, 110, 130)),
    ((0, 0.5, 1, 1.02, 1.5, 2), (0, 50, 100, 100.8, 120, 140)),
])
def test_bilinear_stiffness_drop_at_three_sampling_cadences(curve, criteria, depths, loads):
    monitor = PenetrationMonitor(curve, criteria)
    results = [monitor.update(Measurement(i * 10, load, depth))
               for i, (load, depth) in enumerate(zip(loads, depths))]
    # Both records sample K = 100 to 1 m, then K = 40 kN/m.
    assert results[-1].stiffness_kn_m == 40
    assert results[-1].stiffness_drop_fraction == pytest.approx(0.6)
    assert Trigger.STIFFNESS_DROP in results[-1].active_triggers
    assert all(Trigger.OUTSIDE_BAND not in r.active_triggers for r in results)
    assert all(Trigger.RAPID_PENETRATION not in r.active_triggers for r in results)


def test_slow_ramp_accumulates_sub_tolerance_loading_steps(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    depths = (0, 0.005, 0.01, 0.015, 0.02, 0.025, 0.03, 0.0425, 0.055, 0.0675)
    results = [monitor.update(Measurement(i * 10, i * 0.5, depth))
               for i, depth in enumerate(depths)]
    # Anchor secants: 1.5/0.015 = 100; 1.5/0.0375 = 40 kN/m.
    assert results[2].stiffness_kn_m is None  # Exactly 1 kN is not loading.
    assert results[3].stiffness_kn_m == 100
    assert results[6].stiffness_kn_m == 100
    assert results[-1].stiffness_kn_m == 40
    assert results[-1].stiffness_drop_fraction == pytest.approx(0.6)
    assert results[-1].active_triggers == (Trigger.STIFFNESS_DROP,)


@pytest.mark.parametrize('tolerance,loads,expected', [
    (1, (0, 1, 2), 100),
    (0.3, (0, 0.1, 0.2, 0.3, 0.30000000000000004), 15.000000000000002),
    (0, (0, 0.5, 1), 50),
])
def test_loading_anchor_uses_a_strict_decimal_tolerance(curve, criteria, tolerance, loads, expected):
    monitor = PenetrationMonitor(curve, replace(criteria, constant_load_tolerance_kn=tolerance))
    depth_step = 0.005 if tolerance == 0.3 else 0.01
    results = [monitor.update(Measurement(i * 10, load, i * depth_step))
               for i, load in enumerate(loads)]
    # 2/0.02 = 100; just above 0.3/0.02 = 15; 0.5/0.01 = 50 kN/m.
    assert results[-1].stiffness_kn_m == pytest.approx(expected)
    if tolerance:
        assert all(r.stiffness_kn_m is None for r in results[:-1])


def test_small_load_reversals_preserve_the_loading_reference(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    readings = ((0, 0), (100, 1), (100.8, 1.02), (100.4, 1.03), (120, 1.5))
    results = [monitor.update(Measurement(i * 10, load, depth))
               for i, (load, depth) in enumerate(readings)]
    # The dip is within tolerance of the anchor: K = 20/0.5 = 40 kN/m.
    assert results[-1].stiffness_kn_m == 40
    assert results[-1].stiffness_drop_fraction == pytest.approx(0.6)
    assert results[-1].active_triggers == (Trigger.STIFFNESS_DROP,)


def test_load_dip_beyond_tolerance_resets_the_loading_reference(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    readings = ((0, 0), (100, 1), (100.8, 1.02), (98, 1.03), (120, 1.5))
    results = [monitor.update(Measurement(i * 10, load, depth))
               for i, (load, depth) in enumerate(readings)]
    # A 2 kN dip below the anchor resets: K = 22/0.47, no reference.
    assert results[-1].stiffness_kn_m == pytest.approx(22 / 0.47)
    assert results[-1].stiffness_drop_fraction is None
    assert Trigger.STIFFNESS_DROP not in results[-1].active_triggers


@pytest.mark.parametrize('load,resets', [(99.701, False), (99.7, False), (99.699, True)])
def test_unload_threshold_uses_decimal_anchor_difference(curve, criteria, load, resets):
    monitor = PenetrationMonitor(curve, replace(criteria, constant_load_tolerance_kn=0.3))
    readings = ((0, 0), (100, 1), (load, 1.01), (120, 1.5))
    results = [monitor.update(Measurement(i * 10, reading_load, depth))
               for i, (reading_load, depth) in enumerate(readings)]
    assert (results[-1].stiffness_drop_fraction is None) is resets
    assert (Trigger.STIFFNESS_DROP in results[-1].active_triggers) is not resets


def test_repeated_unloads_reset_the_anchor(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    readings = ((0, 0), (100, 1), (99.5, 1), (99, 1), (98.5, 1), (108.5, 2))
    results = [monitor.update(Measurement(i * 10, load, depth))
               for i, (load, depth) in enumerate(readings)]
    # Dips accumulate against the anchor; 98.5 is more than 1 kN below 100.
    assert results[-1].stiffness_kn_m == 10
    assert results[-1].stiffness_drop_fraction is None
    assert Trigger.STIFFNESS_DROP not in results[-1].active_triggers


def test_unloading_at_exact_tolerance_preserves_the_reference(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 0, 0), Measurement(10, 100, 1),
                   Measurement(20, 99, 1)):
        monitor.update(sample)
    result = monitor.update(Measurement(30, 110, 2))
    # Exactly 1 kN below the anchor is within tolerance: K = 10/1.
    assert result.stiffness_kn_m == 10
    assert result.stiffness_drop_fraction == pytest.approx(0.9)
    assert Trigger.STIFFNESS_DROP in result.active_triggers


@pytest.mark.parametrize('depth', [1.019, 0.999, 0.998])
def test_depth_noise_within_anchor_tolerance_preserves_reference(curve, criteria, depth):
    criteria = replace(criteria, upward_movement_tolerance_m=0.002)
    monitor = PenetrationMonitor(curve, criteria)
    readings = ((0, 0), (100, 1), (100.8, 1.02), (100.4, depth), (120, 1.5))
    results = [monitor.update(Measurement(i * 10, load, measured_depth))
               for i, (load, measured_depth) in enumerate(readings)]
    # A 1 mm adjacent rise, or up to 2 mm above the anchor, retains Kref.
    assert results[-1].stiffness_kn_m == 40
    assert results[-1].stiffness_drop_fraction == pytest.approx(0.6)
    assert Trigger.STIFFNESS_DROP in results[-1].active_triggers


@pytest.mark.parametrize('tolerance,depth', [(0.002, 0.997), (0, 0.999)])
def test_depth_reversal_beyond_supplied_tolerance_resets_reference(curve, criteria, tolerance, depth):
    monitor = PenetrationMonitor(
        curve, replace(criteria, upward_movement_tolerance_m=tolerance))
    readings = ((0, 0), (100, 1), (100.8, 1.02), (100.4, depth), (120, 1.5))
    results = [monitor.update(Measurement(i * 10, load, measured_depth))
               for i, (load, measured_depth) in enumerate(readings)]
    assert results[-1].stiffness_kn_m == pytest.approx(19.6 / (1.5 - depth))
    assert results[-1].stiffness_drop_fraction is None
    assert Trigger.STIFFNESS_DROP not in results[-1].active_triggers


def test_upward_movement_clears_pending_loading(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 0, 0), Measurement(10, 100, 1),
                   Measurement(20, 100.8, 1.2)):
        monitor.update(sample)
    interrupted = monitor.update(Measurement(30, 110, 0.99))
    assert interrupted.stiffness_kn_m is None
    assert interrupted.stiffness_drop_fraction is None
    result = monitor.update(Measurement(40, 120, 1.99))
    assert result.stiffness_kn_m == 10  # Reload K = (120-110)/(1.99-0.99).
    assert result.stiffness_drop_fraction is None


@pytest.mark.parametrize('rejected', [
    Measurement(20, 0, 0), Measurement(25, float('nan'), 1.03),
    Measurement(25, 101, -1), Measurement(25, True, 1.03),
])
def test_rejected_reading_preserves_pending_loading_and_adjacent_rate(curve, criteria, rejected):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 0, 0), Measurement(10, 100, 1),
                   Measurement(20, 100.8, 1.02)):
        monitor.update(sample)
    with pytest.raises(ValueError):
        monitor.update(rejected)
    result = monitor.update(Measurement(30, 120, 1.5))
    # Anchor K = 20/0.5 = 40; adjacent rate = (1.5-1.02)/10 = 0.048.
    assert result.stiffness_kn_m == 40
    assert result.stiffness_drop_fraction == pytest.approx(0.6)
    assert result.penetration_rate_m_s == pytest.approx(0.048)
    assert result.active_triggers == (Trigger.STIFFNESS_DROP,)


def test_gradual_softening_is_compared_with_maximum_loading_stiffness(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    monitor.update(Measurement(0, 0, 0))
    load = 0
    results = []
    for i, stiffness in enumerate((100, 90, 80, 70, 60, 50, 40), 1):
        load += stiffness / 10
        results.append(monitor.update(Measurement(i * 10, load, i / 10)))
    assert all(Trigger.STIFFNESS_DROP not in r.active_triggers for r in results[:5])
    assert results[-2].stiffness_drop_fraction == 0.5
    assert results[-1].stiffness_drop_fraction == pytest.approx(0.6)
    assert all(Trigger.STIFFNESS_DROP in r.active_triggers for r in results[-2:])
    assert results[-1].alerts == ()


def test_stiffness_reference_tracks_a_new_loading_maximum(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 0, 0), Measurement(10, 50, 0.5),
                   Measurement(20, 110, 1), Measurement(30, 145, 1.5)):
        result = monitor.update(sample)
    # Maximum K = 60/0.5 = 120, current K = 35/0.5 = 70 kN/m.
    assert result.stiffness_drop_fraction == pytest.approx(1 - 70 / 120)
    assert Trigger.STIFFNESS_DROP not in result.active_triggers
    result = monitor.update(Measurement(40, 170, 2))
    assert result.stiffness_drop_fraction == pytest.approx(1 - 50 / 120)
    assert Trigger.STIFFNESS_DROP in result.active_triggers


@pytest.mark.parametrize('interruption', [
    Measurement(20, 90, 1), Measurement(20, 110, 0.9),
])
def test_unload_or_upward_movement_resets_stiffness_reference(curve, criteria, interruption):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 0, 0), Measurement(10, 100, 1), interruption):
        monitor.update(sample)
    result = monitor.update(Measurement(30, interruption.load_kn + 10,
                                        interruption.penetration_m + 1))
    assert result.stiffness_drop_fraction is None
    assert Trigger.STIFFNESS_DROP not in result.active_triggers


def test_rejected_reading_does_not_change_stiffness_reference(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    monitor.update(Measurement(0, 0, 0))
    monitor.update(Measurement(10, 100, 1))
    with pytest.raises(ValueError, match='increasing'):
        monitor.update(Measurement(10, 0, 0))
    result = monitor.update(Measurement(20, 140, 2))
    assert result.stiffness_drop_fraction == pytest.approx(0.6)
    assert Trigger.STIFFNESS_DROP in result.active_triggers


def test_zero_depth_loading_step_preserves_finite_stiffness_reference(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    monitor.update(Measurement(0, 0, 0))
    monitor.update(Measurement(10, 100, 1))
    stiff_step = monitor.update(Measurement(20, 120, 1))
    assert stiff_step.stiffness_kn_m is None
    assert stiff_step.stiffness_drop_fraction is None
    assert Trigger.STIFFNESS_DROP not in stiff_step.active_triggers
    result = monitor.update(Measurement(30, 140, 2))
    # The unresolved step retains the anchor: K = 40/1, Kref = 100 kN/m.
    assert result.stiffness_kn_m == 40
    assert result.stiffness_drop_fraction == pytest.approx(0.6)
    assert Trigger.STIFFNESS_DROP in result.active_triggers


def test_initial_zero_depth_loading_step_does_not_invent_finite_reference(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 0, 0), Measurement(10, 50, 0)):
        assert monitor.update(sample).stiffness_kn_m is None
    result = monitor.update(Measurement(20, 100, 1))
    assert result.stiffness_kn_m == 100  # Accumulated K = (100-0)/(1-0).
    assert result.stiffness_drop_fraction is None
    assert Trigger.STIFFNESS_DROP not in result.active_triggers
    assert monitor.update(Measurement(30, 120, 2)).stiffness_drop_fraction == pytest.approx(0.8)


def test_unresolved_depth_steps_accumulate_until_depth_increases(curve, criteria):
    monitor = PenetrationMonitor(curve, replace(criteria, constant_load_tolerance_kn=0.5))
    readings = ((0, 0), (3, 0.01), (4, 0.01), (5, 0.01), (6, 0.02))
    results = [monitor.update(Measurement(i * 10, load, depth))
               for i, (load, depth) in enumerate(readings)]
    # Both resolved secants are 3/0.01 = 300 kN/m, with no stiffness loss.
    assert results[1].stiffness_kn_m == 300
    assert all(r.stiffness_kn_m is None for r in results[2:4])
    assert results[-1].stiffness_kn_m == 300
    assert results[-1].stiffness_drop_fraction == 0
    assert Trigger.STIFFNESS_DROP not in results[-1].active_triggers


@pytest.mark.parametrize('load,depth,armed', [
    (175, 3.5, True), (175, 3.51, False), (180, 3.1, False), (181, 3.1, False),
])
def test_pre_peak_warning_uses_upper_depth_bound_and_strict_load_limit(curve, criteria, load, depth, armed):
    monitor = PenetrationMonitor(curve, criteria)
    monitor.update(Measurement(0, 160, 2.4))
    monitor.update(Measurement(10, 170, 2.6))
    result = monitor.update(Measurement(20, load, depth))
    assert (result.next_peak_penetration_m is not None) is armed
    assert (Trigger.STIFFNESS_DROP in result.active_triggers) is armed


def test_stiffness_drop_is_not_claimed_after_peak(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 150, 3.6), Measurement(10, 160, 3.7)):
        monitor.update(sample)
    # K falls from 100 to 10 kN/m, but depth 4.7 is beyond the only peak.
    result = monitor.update(Measurement(20, 170, 4.7))
    assert result.next_peak_penetration_m is None
    assert Trigger.STIFFNESS_DROP not in result.active_triggers


@pytest.mark.parametrize('later_peak', [False, True])
def test_peak_below_measured_load_is_skipped_even_before_its_depth(curve, criteria, later_peak):
    if later_peak:
        curve += (PredictionPoint(6, 200, 5.5, 6.5),)
    result = PenetrationMonitor(curve, criteria).update(Measurement(0, 185, 2.8))
    assert result.next_peak_penetration_m == (5 if later_peak else None)
    assert result.active_triggers == (Trigger.OUTSIDE_BAND,)


def test_no_peak_means_stiffness_warning_is_unavailable(curve, criteria):
    monitor = PenetrationMonitor(curve[:3], criteria)
    for sample in (Measurement(0, 0, 0), Measurement(10, 100, 1)):
        monitor.update(sample)
    result = monitor.update(Measurement(20, 140, 2))
    assert result.next_peak_penetration_m is None
    assert Trigger.STIFFNESS_DROP not in result.active_triggers


@pytest.mark.parametrize('load', [99, 100, 101])
def test_rapid_penetration_at_constant_load_includes_tolerance(curve, criteria, load):
    monitor = PenetrationMonitor(curve, criteria)
    monitor.update(Measurement(0, 100, 1))
    result = monitor.update(Measurement(20, load, 1.4))
    # Rate = (1.4-1)/20 = 0.02 m/s; |load-100| <= 1 kN.
    assert result.penetration_rate_m_s == pytest.approx(0.02)
    assert result.active_triggers == (Trigger.RAPID_PENETRATION,)
    assert PlanBAction.HULL_TO_WATER in result.alerts[0].actions


def test_rapid_rate_threshold_is_strict(curve, criteria):
    monitor = PenetrationMonitor(curve, replace(criteria, rapid_penetration_rate_m_s=0.02))
    monitor.update(Measurement(0, 100, 1))
    # Exactly 0.02 m/s does not exceed the configured rate.
    assert monitor.update(Measurement(20, 100, 1.4)).active_triggers == ()


@pytest.mark.parametrize('sample', [Measurement(20, 102, 1.4), Measurement(20, 100, 0.8)])
def test_loading_and_upward_movement_are_not_constant_load_penetration(curve, criteria, sample):
    monitor = PenetrationMonitor(curve, criteria)
    monitor.update(Measurement(0, 100, 1))
    assert Trigger.RAPID_PENETRATION not in monitor.update(sample).active_triggers


@pytest.mark.parametrize('hold', [Measurement(20, 100, 1), Measurement(20, 101, 1.1),
                                 Measurement(20, 100, 1.1)])
def test_hold_preserves_the_stiffness_comparison(curve, criteria, hold):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 0, 0), Measurement(10, 100, 1), hold):
        monitor.update(sample)
    result = monitor.update(Measurement(30, 140, 2))
    # A hold preserves the anchor: K = (140-100)/(2-1) = 40, drop = 0.6.
    assert result.stiffness_kn_m == 40
    assert result.stiffness_drop_fraction == pytest.approx(0.6)
    assert Trigger.STIFFNESS_DROP in result.active_triggers


@pytest.mark.parametrize('load,depth,stiffness,triggered', [
    (105, 1.15, 100 / 3, True),
    (110, 1.2, 50, True),
    (140, 1.5, 80, False),
])
def test_hold_creep_sensitivity_depends_on_post_hold_load_step(
        curve, criteria, load, depth, stiffness, triggered):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 0, 0), Measurement(10, 100, 1),
                   Measurement(20, 100, 1.1)):
        monitor.update(sample)
    result = monitor.update(Measurement(30, load, depth))
    # Post-hold soil K stays 100; creep 0.1 m is in the secant denominator.
    # At the 0.5 drop limit, load steps up to creep * K = 10 kN trigger.
    assert result.stiffness_kn_m == pytest.approx(stiffness)
    assert result.stiffness_drop_fraction == pytest.approx(1 - stiffness / 100)
    assert (Trigger.STIFFNESS_DROP in result.active_triggers) is triggered


def test_unresolved_transition_can_inflate_maximum_stiffness_reference(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 0, 0), Measurement(10, 100, 1)):
        monitor.update(sample)
    spike = monitor.update(Measurement(20, 120, 1.001))
    result = monitor.update(Measurement(30, 140, 1.201))
    assert spike.stiffness_kn_m == 20000  # 20 kN over 1 mm.
    assert result.stiffness_kn_m == 100
    assert result.stiffness_drop_fraction == pytest.approx(0.995)
    assert Trigger.STIFFNESS_DROP in result.active_triggers


def test_alert_events_are_edge_triggered_but_reassessment_stays_latched(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    first = monitor.update(Measurement(0, 50, 1.1))
    repeated = monitor.update(Measurement(10, 50, 1.1))
    recovered = monitor.update(Measurement(20, 50, 1.0))
    renewed = monitor.update(Measurement(30, 50, 1.1))
    assert len(first.alerts) == len(renewed.alerts) == 1
    assert repeated.active_triggers == (Trigger.OUTSIDE_BAND,)
    assert repeated.alerts == recovered.alerts == ()
    assert recovered.active_triggers == ()
    assert recovered.requires_reassessment is True
    assert PenetrationMonitor(curve, criteria).update(Measurement(0, 50, 1)).requires_reassessment is False


@pytest.mark.parametrize('field,value', [
    ('elapsed_s', -1), ('elapsed_s', float('nan')), ('load_kn', -1),
    ('load_kn', float('inf')), ('penetration_m', -1), ('penetration_m', True),
])
def test_rejects_invalid_measurements_without_advancing_state(curve, criteria, field, value):
    monitor = PenetrationMonitor(curve, criteria)
    monitor.update(Measurement(0, 100, 1))
    with pytest.raises(ValueError):
        monitor.update(replace(Measurement(10, 100, 1), **{field: value}))
    assert monitor.update(Measurement(10, 100, 1)).active_triggers == ()


@pytest.mark.parametrize('time', [0, 9])
def test_rejects_duplicate_or_reversed_times(curve, criteria, time):
    monitor = PenetrationMonitor(curve, criteria)
    monitor.update(Measurement(10, 100, 1))
    with pytest.raises(ValueError, match='increasing'):
        monitor.update(Measurement(time, 100, 1))


@pytest.mark.parametrize('field,value', [
    ('stiffness_drop_fraction', 0), ('stiffness_drop_fraction', 1),
    ('stiffness_drop_fraction', float('nan')),
    ('rapid_penetration_rate_m_s', 0), ('rapid_penetration_rate_m_s', -1),
    ('constant_load_tolerance_kn', -1), ('constant_load_tolerance_kn', True),
    ('upward_movement_tolerance_m', -1), ('upward_movement_tolerance_m', True),
    ('upward_movement_tolerance_m', float('inf')),
    ('upward_movement_tolerance_m', float('nan')),
])
def test_rejects_invalid_criteria(criteria, field, value):
    with pytest.raises(ValueError):
        replace(criteria, **{field: value})


@pytest.mark.parametrize('bad_curve', [
    (), (PredictionPoint(0, 0, 0, 0),),
    (PredictionPoint(1, 0, 0, 1), PredictionPoint(1, 1, 0, 1)),
    (PredictionPoint(1, 0, 0, 1), PredictionPoint(0, 1, 0, 1)),
    (PredictionPoint(0, 0, 0, 0), PredictionPoint(1, -1, 0, 1)),
    (PredictionPoint(0, 0, 0, 0), PredictionPoint(1, 1, 1.1, 2)),
    (PredictionPoint(0, 0, 0, 0), PredictionPoint(1, 1, 0, 0.9)),
    (PredictionPoint(0, 0, 0, 0), PredictionPoint(1, float('nan'), 0, 1)),
])
def test_rejects_invalid_prediction(bad_curve, criteria):
    with pytest.raises(ValueError):
        PenetrationMonitor(bad_curve, criteria)


def test_example_exports_one_entry_per_trigger_and_offline_plotly_json(tmp_path):
    from plotly.io import from_json

    root = Path(__file__).resolve().parents[2]
    example = runpy.run_path(str(root / 'examples/domains/geotechnical/jackup_penetration_monitoring.py'))
    example['run_example'](tmp_path)
    payload = json.loads((tmp_path / 'penetration-monitoring.json').read_text(encoding='utf-8'))
    figure = from_json((tmp_path / 'penetration-monitoring.plotly.json').read_text(encoding='utf-8'))
    for name in ('penetration-monitoring.json', 'penetration-monitoring.plotly.json'):
        assert (tmp_path / name).read_bytes().endswith(b'\n')
        assert b'\r\n' not in (tmp_path / name).read_bytes()
    # Synthetic sequence: K drops 100 -> 40 at 20 s; 0.2/10 > 0.015 m/s
    # at 40 s; 2.6 > 1 + (150-100)/(160-100) + 0.5 at 50 s.
    events = [(row['sample']['elapsed_s'], alert['trigger'])
              for row in payload['results'] for alert in row['alerts']]
    assert events == [(20, 'stiffness_drop'), (40, 'rapid_penetration'), (50, 'outside_band')]
    assert payload['synthetic'] is True
    assert payload['qualification'] == 'illustration_only'
    assert set(payload['unpublished_input_ranges']) == {
        'air_gap_m', 'draft_m', 'significant_wave_height_m', 'tide_m',
        'ballast_t', 'preload_mode',
    }
    assert all(item['basis'] == 'assumed, based on previous experience'
               for item in payload['unpublished_input_ranges'].values())
    assert figure.layout.xaxis.title.text == 'Penetration (m)'
    assert figure.layout.yaxis.title.text == 'Leg load (kN)'
    assert len(figure.data) == 7  # Lower/upper/nominal, measured, three triggers.
    assert payload['results'][-1]['requires_reassessment'] is True
