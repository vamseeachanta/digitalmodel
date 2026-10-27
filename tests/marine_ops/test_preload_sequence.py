# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic per-leg preload histories and independent screening expectations."""

from dataclasses import replace

import pytest

from digitalmodel.geotechnical.spudcan import PenetrationPoint
from digitalmodel.marine_ops.preload_sequence import (
    PreloadCriteria,
    PreloadSample,
    PreloadStage,
    screen_preload_sequence,
)


CURVE = (PenetrationPoint(0, 0, 0, 'synthetic'),
         PenetrationPoint(2, 100, 100, 'synthetic'))


def sequence(holding=None, preload=None):
    """Use hours, kN and metres; the planned relation is z = Q / 50."""
    return (
        PreloadStage('preparation', (PreloadSample(0, 0, 0),)),
        PreloadStage('preload', preload or (
            PreloadSample(0.1, 25, 0.5), PreloadSample(0.2, 50, 1))),
        PreloadStage('holding', holding or (
            PreloadSample(0.25, 50, 1), PreloadSample(0.75, 50, 1.01),
            PreloadSample(1, 50, 1.0101), PreloadSample(1.25, 50, 1.0102))),
        PreloadStage('discharge', (PreloadSample(3, 10, 1.0102),)),
    )


def screen(stages=None, curve=CURVE, criteria=None, target=50):
    return screen_preload_sequence(
        stages or sequence(), target_preload_per_leg_kn=target,
        planned_curve=curve, criteria=criteria or PreloadCriteria())


def test_four_stages_hold_one_hour_and_compare_each_sample():
    result = screen()
    assert result.stage_order == ('preparation', 'preload', 'holding', 'discharge')
    assert result.hold_start_h == 0.25
    assert result.hold_end_h == 1.25
    assert result.hold_duration_h == 1
    assert result.hold_met
    assert result.hold_reasons == ()
    assert result.settlement_rate_m_h == pytest.approx(0.0004)
    assert not result.punch_through
    assert len(result.steps) == 8
    assert result.steps[2].planned_penetration_m == pytest.approx(1)
    assert result.steps[-2].deviation_m == pytest.approx(0.0102)
    assert result.target_preload_per_leg_kn == 50
    assert result.criteria == PreloadCriteria()


@pytest.mark.parametrize('holding,reasons', [
    ((PreloadSample(0.25, 50, 1), PreloadSample(0.5, 50, 1.01)),
     ('hold_shorter_than_minimum', 'settlement_rate_not_below_limit')),
    ((PreloadSample(0.25, 50, 1), PreloadSample(1.25, 50, 1.1)),
     ('settlement_rate_not_below_limit',)),
    ((PreloadSample(0.25, 50, 1), PreloadSample(0.5, 50, 1)),
     ('hold_shorter_than_minimum',)),
])
def test_hold_fails_with_specific_duration_and_rate_reasons(holding, reasons):
    result = screen(sequence(holding=holding))
    assert not result.hold_met
    assert result.hold_reasons == reasons


def test_full_preload_interruption_restarts_clock_and_excludes_discharge():
    holding = (PreloadSample(0.25, 50, 1), PreloadSample(1, 49, 1),
               PreloadSample(1.1, 50, 1), PreloadSample(1.5, 50, 1))
    result = screen(sequence(holding=holding))
    assert result.hold_start_h == 1.1
    assert result.hold_end_h == 1.5
    assert result.hold_duration_h == pytest.approx(0.4)
    assert result.hold_reasons == ('hold_shorter_than_minimum',)
    assert not result.hold_met


def test_no_full_preload_or_no_window_is_explicit():
    result = screen(sequence(holding=(PreloadSample(0.25, 49, 1),)))
    assert result.hold_start_h is None
    assert result.settlement_rate_m_h is None
    assert 'full_preload_not_maintained' in result.hold_reasons
    single = screen(sequence(holding=(PreloadSample(0.25, 50, 1),)))
    assert 'insufficient_settlement_window' in single.hold_reasons


def test_window_interpolation_and_rebound_do_not_cancel_settlement():
    holding = (PreloadSample(0.25, 50, 1), PreloadSample(1, 50, 1),
               PreloadSample(1.125, 50, 1.01), PreloadSample(1.25, 50, 1))
    result = screen(sequence(holding=holding))
    assert result.settlement_rate_m_h == pytest.approx(0.08)
    assert not result.hold_met
    # Window begins at 0.95 h, between readings, on a linear 0.02 m/h trace.
    linear = (PreloadSample(0.25, 50, 1), PreloadSample(1.25, 50, 1.02))
    result = screen(sequence(holding=linear), criteria=PreloadCriteria(settlement_window_h=0.3))
    assert result.settlement_rate_m_h == pytest.approx(0.02)


def test_criteria_are_inputs_and_one_hour_is_a_floor():
    holding = (PreloadSample(0.25, 50, 1), PreloadSample(1.25, 50, 1.002))
    assert not screen(sequence(holding=holding)).hold_met
    assert screen(sequence(holding=holding), criteria=PreloadCriteria(
        settlement_rate_limit_m_h=0.003)).hold_met
    assert not screen(criteria=PreloadCriteria(minimum_hold_h=2)).hold_met
    with pytest.raises(ValueError, match='one hour'):
        screen(criteria=PreloadCriteria(minimum_hold_h=0.5))


def test_sharp_penetration_at_small_load_increment_flags_punch_through():
    preload = (PreloadSample(0.1, 25, 0.5), PreloadSample(0.2, 26, 1))
    result = screen(sequence(preload=preload))
    step = result.steps[2]
    assert step.penetration_per_load_m_kn == pytest.approx(0.5)
    assert step.planned_penetration_m == pytest.approx(0.52)
    assert step.deviation_m == pytest.approx(0.48)
    assert step.punch_through_reasons == ('sharp_penetration_at_small_load_increment',)
    assert result.punch_through
    assert not screen(sequence(preload=preload), criteria=PreloadCriteria(
        punch_through_rate_m_kn=0.6)).punch_through
    assert not screen(sequence(preload=preload), criteria=PreloadCriteria(
        small_load_increment_fraction=0.01)).punch_through


def test_zero_load_increment_during_preloading_flags_without_infinite_result():
    preload = (PreloadSample(0.1, 25, 0.5), PreloadSample(0.2, 25, 1))
    result = screen(sequence(preload=preload))
    assert result.punch_through
    assert result.steps[2].penetration_per_load_m_kn is None
    assert result.steps[2].punch_through_reasons == ('penetration_at_constant_preload_step',)


def test_capacity_drop_below_tip_flags_even_below_peak_load():
    curve = (CURVE[0], PenetrationPoint(1, 100, 100, 'peak'),
             PenetrationPoint(2, 60, 60, 'falling'),
             PenetrationPoint(3, 120, 120, 'recovery'))
    result = screen(curve=curve)
    assert result.punch_through
    assert result.steps[2].planned_penetration_m == pytest.approx(0.5)
    assert 'planned_capacity_drop_below_tip' in result.steps[2].punch_through_reasons
    # A tip past the falling branch has no deeper capacity drop on this curve.
    deep = sequence(preload=(PreloadSample(0.1, 25, 2.5), PreloadSample(0.2, 50, 2.6)))
    assert not screen(deep, curve=curve).steps[2].punch_through_reasons


def test_curve_range_is_reported_without_extrapolation():
    stages = sequence(preload=(PreloadSample(0.1, 110, 2.5), PreloadSample(0.2, 120, 3)))
    result = screen(stages, target=120)
    assert result.steps[2].planned_penetration_m is None
    assert result.steps[2].deviation_m is None
    assert not result.steps[2].within_planned_depth


@pytest.mark.parametrize('criteria', [
    PreloadCriteria(settlement_window_h=0),
    PreloadCriteria(settlement_rate_limit_m_h=float('nan')),
    PreloadCriteria(punch_through_rate_m_kn=-1),
    PreloadCriteria(small_load_increment_fraction=1.1),
    PreloadCriteria(minimum_hold_h=True),
])
def test_invalid_criteria_rejected(criteria):
    with pytest.raises(ValueError):
        screen(criteria=criteria)


@pytest.mark.parametrize('kind', ['order', 'empty', 'time', 'load', 'depth', 'curve', 'target'])
def test_invalid_histories_and_curve_rejected(kind):
    stages = list(sequence())
    curve, target = CURVE, 50
    if kind == 'order':
        stages.reverse()
    elif kind == 'empty':
        stages[1] = replace(stages[1], samples=())
    elif kind == 'time':
        stages[1] = replace(stages[1], samples=(PreloadSample(0, 25, 0.5),))
    elif kind == 'load':
        stages[1] = replace(stages[1], samples=(PreloadSample(0.1, True, 0.5),))
    elif kind == 'depth':
        stages[1] = replace(stages[1], samples=(PreloadSample(0.1, 25, float('inf')),))
    elif kind == 'curve':
        curve = tuple(reversed(CURVE))
    else:
        target = 0
    with pytest.raises(ValueError):
        screen(tuple(stages), curve=curve, target=target)
