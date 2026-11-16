"""Illustrative examples; expectations are independent hand formulas."""
import math

import pytest

from digitalmodel.hydrodynamics.diffraction.rao_conventions import (
    wave_frequency_hz,
    wave_period_s,
)


def test_frequency_and_period_use_one_cycle_per_two_pi_radians():
    # Illustrative example: an angular frequency selected independently.
    omega_rad_s = 2.0 * math.pi / 8.0
    assert wave_frequency_hz(omega_rad_s) == pytest.approx(1.0 / 8.0)
    assert wave_period_s(omega_rad_s) == pytest.approx(8.0)


def test_lag_to_lead_and_axis_reversal_keep_full_turns():
    from digitalmodel.hydrodynamics.diffraction.rao_conventions import (
        lag_to_lead_phase_rad,
    )
    # Illustrative example. cos(wt - lag) = cos(wt + lead).
    lag_rad = 2.0 * math.pi + math.pi / 6.0
    assert lag_to_lead_phase_rad(lag_rad) == -lag_rad
    assert lag_to_lead_phase_rad(lag_rad, reverse_axis=True) == -lag_rad + math.pi
    time_angle_rad = 0.7
    assert math.cos(time_angle_rad - lag_rad) == pytest.approx(
        math.cos(time_angle_rad + lag_to_lead_phase_rad(lag_rad))
    )
    assert -math.cos(time_angle_rad - lag_rad) == pytest.approx(
        math.cos(time_angle_rad + lag_to_lead_phase_rad(lag_rad, reverse_axis=True))
    )


def test_six_component_conversion_uses_si_and_reflects_only_odd_motions():
    from digitalmodel.hydrodynamics.diffraction.rao_conventions import (
        RAOInput,
        convert_lag_rao_to_lead,
        export_rao_degrees,
    )
    # Illustrative example in natural surge/sway/heave/roll/pitch/yaw order.
    translation = (2.0, 3.0, 4.0)
    rotation_deg = (0.5, 1.0, 1.5)
    phase_deg = (30.0, 60.0, 120.0, 240.0, 300.0, 45.0)
    sample = RAOInput.from_degrees(
        angular_frequency_rad_s=math.pi / 4.0,
        translation_m_per_m=translation,
        rotation_deg_per_m=rotation_deg,
        phase_deg=phase_deg,
    )
    assert sample.rotation_rad_per_m == pytest.approx(
        tuple(x * math.pi / 180.0 for x in rotation_deg)
    )
    assert sample.phase_rad == pytest.approx(
        tuple(x * math.pi / 180.0 for x in phase_deg)
    )
    result = export_rao_degrees(convert_lag_rao_to_lead(sample, mirrored_about_x=True))
    assert result.frequency_hz == pytest.approx(1.0 / 8.0)
    assert result.period_s == pytest.approx(8.0)
    assert result.translation_m_per_m == translation
    assert result.rotation_deg_per_m == pytest.approx(rotation_deg)
    # Hand formula: -phase, with +180 for sway, roll and yaw only.
    assert result.phase_deg == pytest.approx(
        tuple(-p + (180.0 if i in (1, 3, 5) else 0.0)
              for i, p in enumerate(phase_deg))
    )
    unreflected = export_rao_degrees(convert_lag_rao_to_lead(sample))
    assert unreflected.phase_deg == pytest.approx(tuple(-p for p in phase_deg))
    assert sample.phase_rad[0] == pytest.approx(math.pi / 6.0)


@pytest.mark.parametrize('source_rad, expected_rad', [
    (0.0, math.pi),
    (math.pi / 3.0, -2.0 * math.pi / 3.0),
    (math.pi, 0.0),
    (3.0 * math.pi / 2.0, math.pi / 2.0),
    (2.0 * math.pi, math.pi),
])
def test_opposite_heading_reference(source_rad, expected_rad):
    from digitalmodel.hydrodynamics.diffraction.rao_conventions import (
        opposite_axis_heading_rad,
    )
    assert opposite_axis_heading_rad(source_rad) == pytest.approx(expected_rad)


@pytest.mark.parametrize('source_rad', [-0.1, 7.0, math.inf, math.nan])
def test_opposite_heading_rejects_outside_a_full_turn(source_rad):
    from digitalmodel.hydrodynamics.diffraction.rao_conventions import (
        opposite_axis_heading_rad,
    )
    with pytest.raises(ValueError):
        opposite_axis_heading_rad(source_rad)


@pytest.mark.parametrize('omega_rad_s', [0.0, -2.0, math.inf, math.nan, True, '2'])
def test_frequency_requires_finite_positive_real_numbers(omega_rad_s):
    for compute in (wave_frequency_hz, wave_period_s):
        with pytest.raises(ValueError):
            compute(omega_rad_s)


def _illustrative_si_sample(**changes):
    from digitalmodel.hydrodynamics.diffraction.rao_conventions import RAOInput
    values = dict(
        angular_frequency_rad_s=math.pi / 4.0,
        translation_m_per_m=(2.0, 3.0, 4.0),
        rotation_rad_per_m=(0.01, 0.02, 0.03),
        phase_rad=(0.0, 0.5, 1.0, 1.5, 2.0, 2.5),
    )
    values.update(changes)
    return RAOInput(**values)


@pytest.mark.parametrize('changes', [
    dict(angular_frequency_rad_s=0.0),
    dict(translation_m_per_m=(-1.0, 0.0, 0.0)),
    dict(rotation_rad_per_m=(0.0, math.inf, 0.0)),
    dict(rotation_rad_per_m=(0.0, -1.0, 0.0)),
    dict(phase_rad=(0.0,) * 5),
    dict(phase_rad=(0.0, 0.0, 0.0, 0.0, 0.0, math.nan)),
    dict(translation_m_per_m=(1.0, 2.0)),
    dict(translation_m_per_m=(True, 0.0, 0.0)),
    dict(translation_m_per_m=('1', 0.0, 0.0)),
])
def test_direct_si_construction_validates_every_field(changes):
    with pytest.raises(ValueError):
        _illustrative_si_sample(**changes)


def test_frozen_input_copies_sequences_to_immutable_tuples():
    from dataclasses import FrozenInstanceError
    original = [2.0, 3.0, 4.0]
    sample = _illustrative_si_sample(translation_m_per_m=original)
    original[0] = 99.0
    assert sample.translation_m_per_m == (2.0, 3.0, 4.0)
    with pytest.raises(FrozenInstanceError):
        sample.angular_frequency_rad_s = 4.0


@pytest.mark.parametrize('phase_rad', [math.inf, math.nan, True, '1'])
def test_phase_rejects_non_finite_or_non_numeric_values(phase_rad):
    from digitalmodel.hydrodynamics.diffraction.rao_conventions import lag_to_lead_phase_rad
    with pytest.raises(ValueError):
        lag_to_lead_phase_rad(phase_rad)


def test_flags_require_booleans():
    from digitalmodel.hydrodynamics.diffraction.rao_conventions import (
        convert_lag_rao_to_lead,
        lag_to_lead_phase_rad,
    )
    with pytest.raises(ValueError):
        lag_to_lead_phase_rad(0.0, reverse_axis='false')
    with pytest.raises(ValueError):
        convert_lag_rao_to_lead(_illustrative_si_sample(), mirrored_about_x=1)


def test_zero_amplitudes_and_double_conversion_identity():
    from digitalmodel.hydrodynamics.diffraction.rao_conventions import convert_lag_rao_to_lead
    sample = _illustrative_si_sample(
        translation_m_per_m=(0.0, 0.0, 0.0), rotation_rad_per_m=(0.0, 0.0, 0.0),
    )
    for mirrored in (False, True):
        twice = convert_lag_rao_to_lead(
            convert_lag_rao_to_lead(sample, mirrored_about_x=mirrored),
            mirrored_about_x=mirrored,
        )
        assert twice.phase_rad == pytest.approx(sample.phase_rad)
        assert twice.translation_m_per_m == (0.0, 0.0, 0.0)
        assert twice.rotation_rad_per_m == (0.0, 0.0, 0.0)


@pytest.mark.parametrize('changes', [
    dict(rotation_deg_per_m=(-0.5, 0.0, 0.0)),
    dict(rotation_deg_per_m=(0.5, math.inf, 0.0)),
    dict(rotation_deg_per_m=(0.5, 0.0)),
    dict(phase_deg=('30', 0.0, 0.0, 0.0, 0.0, 0.0)),
    dict(phase_deg=(0.0,) * 7),
])
def test_degree_boundary_validates_before_converting(changes):
    from digitalmodel.hydrodynamics.diffraction.rao_conventions import RAOInput
    values = dict(
        angular_frequency_rad_s=math.pi,
        translation_m_per_m=(0.0, 0.0, 0.0),
        rotation_deg_per_m=(0.0, 0.0, 0.0), phase_deg=(0.0,) * 6,
    )
    values.update(changes)
    with pytest.raises(ValueError):
        RAOInput.from_degrees(**values)


def test_nonrepresentable_degree_export_is_rejected():
    from digitalmodel.hydrodynamics.diffraction.rao_conventions import export_rao_degrees
    sample = _illustrative_si_sample(phase_rad=(1e308, 0.0, 0.0, 0.0, 0.0, 0.0))
    with pytest.raises(ValueError, match='representable'):
        export_rao_degrees(sample)


@pytest.mark.parametrize('compute', [wave_frequency_hz, wave_period_s])
def test_nonrepresentable_frequency_or_period_is_rejected(compute):
    with pytest.raises(ValueError):
        compute(5e-324)


def test_oversized_integer_inputs_raise_validation_errors():
    # Illustrative invalid input: outside the representable SI float domain.
    with pytest.raises(ValueError):
        wave_frequency_hz(10**400)
    with pytest.raises(ValueError):
        _illustrative_si_sample(phase_rad=(10**400, 0.0, 0.0, 0.0, 0.0, 0.0))
