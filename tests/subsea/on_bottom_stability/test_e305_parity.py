# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic hand calculations; coefficients are invented test inputs."""

import math

import pytest


def case(**changes):
    from digitalmodel.subsea.on_bottom_stability.e305_simplified import Case

    # Both safety factors are invented placeholders, unrelated to any standard factor.
    data = dict(role='extreme_wave', wave_velocity_m_s=0.0, current_m_s=2.0,
                period_s=4.0, R=0.5, Fw=3.0, cd=3.0, cm=0.0, cl=2.0,
                mu=0.5, target_safety_factor=1.25, gamma_st=1.4,
                reduction_application='wave_velocity', phase_rad=0.0)
    return Case(**(data | changes))


def assess(value):
    from digitalmodel.subsea.on_bottom_stability.e305_simplified import evaluate_case

    return evaluate_case(value, diameter_m=1.0, rho_water_kg_m3=2.0,
                         available_weight_n_m=200.0)


def test_current_counted_once_or_explicitly_duplicated():
    # FD=12, FL=8; demand=(12+0.5*8)/0.5*3*1.25=120 N/m.
    result = assess(case())
    assert result.required_weight_n_m == pytest.approx(120.0)
    assert result.drag_n_m == pytest.approx(12.0)
    assert result.lift_n_m == pytest.approx(8.0)
    assert result.utilization == pytest.approx(0.6)
    assert assess(case(duplicate_current=True)).required_weight_n_m == pytest.approx(480.0)
    assert result.os_f101_status == 'not_established'
    assert result.qualified is False


def test_single_wave_instant_and_simultaneous_phase_scan():
    # Synthetic CM=4/pi cancels the displaced area. Reduced wave=1 m/s;
    # acceleration amplitude=pi/2, so FI amplitude=pi and design demand=7.5*pi.
    inputs = dict(wave_velocity_m_s=2.0, current_m_s=0.0,
                  cd=0.0, cl=0.0, cm=4/math.pi)
    assert assess(case(**inputs)).required_weight_n_m == pytest.approx(0.0)
    scan = assess(case(**inputs, phase_rad=None, phase_count=4))
    assert scan.required_weight_n_m == pytest.approx(7.5*math.pi)
    assert abs(scan.inertia_n_m) == pytest.approx(math.pi)
    assert scan.lift_n_m == pytest.approx(0.0)
    assert scan.qualified is False


def test_typed_fw_is_used_and_reported_without_a_table_lookup():
    result = assess(case(Fw=6.0))
    assert result.required_weight_n_m == pytest.approx(240.0)
    assert result.Fw == 6.0
    assert result.factor_source == 'typed_parity'
    assert result.citations == ()


def test_wave_2_period_is_independent():
    values = dict(wave_velocity_m_s=2.0, current_m_s=0.0, cd=0.0,
                  cl=0.0, cm=4/math.pi, phase_rad=-math.pi/2)
    wave_1 = assess(case(**values))
    wave_2 = assess(case(**values, role='extreme_current', period_s=8.0))
    assert wave_1.required_weight_n_m == pytest.approx(7.5*math.pi)
    assert wave_2.required_weight_n_m == pytest.approx(3.75*math.pi)
    assert wave_1.period_s == 4.0
    assert wave_2.period_s == 8.0


def test_reduction_convention_and_dimensionless_ratios_are_explicit():
    result = assess(case(reduction_application='wave_and_current'))
    assert result.required_weight_n_m == pytest.approx(30.0)
    assert result.reduction_application == 'wave_and_current'
    wave = assess(case(wave_velocity_m_s=2.0))
    assert wave.K == pytest.approx(8.0)
    assert wave.M == pytest.approx(1.0)
    assert assess(case()).M is None
    with pytest.raises(ValueError):
        case(reduction_application='inferred')


@pytest.mark.parametrize('changes', [dict(Fw=0), dict(mu=0), dict(cd=-1),
                                     dict(R=float('nan')), dict(period_s=0),
                                     dict(phase_count=3), dict(gamma_st=-1)])
def test_parity_rejects_invalid_inputs(changes):
    with pytest.raises(ValueError):
        case(**changes)
