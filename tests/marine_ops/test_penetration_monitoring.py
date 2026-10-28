# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

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


def test_stiffness_drop_is_not_claimed_after_peak(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 150, 3.6), Measurement(10, 160, 3.7)):
        monitor.update(sample)
    # K falls from 100 to 10 kN/m, but depth 4.7 is beyond the only peak.
    result = monitor.update(Measurement(20, 170, 4.7))
    assert result.next_peak_penetration_m is None
    assert Trigger.STIFFNESS_DROP not in result.active_triggers


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


def test_hold_breaks_the_stiffness_comparison(curve, criteria):
    monitor = PenetrationMonitor(curve, criteria)
    for sample in (Measurement(0, 0, 0), Measurement(10, 100, 1), Measurement(20, 100, 1)):
        monitor.update(sample)
    # After a hold there are not two consecutive positive loading increments.
    assert Trigger.STIFFNESS_DROP not in monitor.update(Measurement(30, 140, 2)).active_triggers


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
    payload = json.loads((tmp_path / 'penetration-monitoring.json').read_text())
    figure = from_json((tmp_path / 'penetration-monitoring.plotly.json').read_text())
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
