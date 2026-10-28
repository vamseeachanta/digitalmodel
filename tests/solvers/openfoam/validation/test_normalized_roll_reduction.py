# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Arithmetic qualification using the independent hand calculation below.

Declare theta = theta0 sin(omega t), M = -K theta - B theta_dot.
Thus M = a cos(omega t) + b sin(omega t), a = -B omega theta0,
b = -K theta0. Choose theta0 = 10 degrees = pi/18 radians and omega
= 2 rad/s (T = pi seconds). Choosing b = -2 N.m gives K = 36/pi
N.m/rad; choosing a = -1 N.m gives B = 9/pi N.m.s/rad.
All signal constants below are synthetic choices, not measured data.
The grid has four cycles and 80 intervals per cycle. Caller policies in
these tests require two cycles and a design-matrix condition number <= 10;
neither policy is an engineering acceptance criterion.
Numerical tolerances test double-precision arithmetic only.
"""

from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.validation import sloshing_sweep_reduction as reduction
from digitalmodel.solvers.openfoam.validation.sloshing_sweep import CONTRACT_FIELDS


def _history():
    t = np.linspace(0.0, 4.0 * math.pi, 321)
    return t, -np.cos(2.0 * t) - 2.0 * np.sin(2.0 * t)


def _reduce(t, m, **overrides):
    options = dict(
        roll_amplitude_deg=10.0,
        settled_window=(0.0, 4.0 * math.pi),
        min_cycles=2.0,
        max_condition_number=10.0,
    )
    options.update(overrides)
    return reduction.reduce_roll_moment_normalized(t, m, math.pi, **options)


def test_pure_damping_units():
    t, _ = _history()
    result = _reduce(t, -np.cos(2.0 * t))
    assert result.damping == pytest.approx(9.0 / math.pi)
    assert result.stiffness == pytest.approx(0.0, abs=1e-12)
    assert result.a == pytest.approx(-1.0)
    assert result.units["damping"] == "N.m.s/rad"
    assert result.units["a"] == "N.m"


def test_pure_stiffness_sign():
    t, _ = _history()
    result = _reduce(t, -2.0 * np.sin(2.0 * t))
    assert result.stiffness == pytest.approx(36.0 / math.pi)
    assert result.damping == pytest.approx(0.0, abs=1e-12)
    assert result.b == pytest.approx(-2.0)
    assert result.units["stiffness"] == "N.m/rad"
    assert result.units["b"] == "N.m"


def test_mixed_coefficients():
    t, m = _history()
    result = _reduce(t, m + 3.0)  # Chosen constant offset, independent of motion.
    assert result.stiffness == pytest.approx(36.0 / math.pi)
    assert result.damping == pytest.approx(9.0 / math.pi)
    assert result.mean_moment == pytest.approx(3.0)
    assert result.theta0_rad == pytest.approx(math.pi / 18.0)
    assert result.omega_rad_s == pytest.approx(2.0)
    assert result.residual_rms == pytest.approx(0.0, abs=1e-12)
    assert result.sample_count == 321
    assert result.cycles == pytest.approx(4.0)


def test_transient_window():
    t, m = _history()
    m[t < 2.0 * math.pi] += 100.0 * t[t < 2.0 * math.pi]
    result = _reduce(t, m, settled_window=(2.0 * math.pi, 4.0 * math.pi))
    assert result.stiffness == pytest.approx(36.0 / math.pi)
    assert result.damping == pytest.approx(9.0 / math.pi)
    assert result.selected_window == pytest.approx((2.0 * math.pi, 4.0 * math.pi))
    assert result.sample_count == 161


def test_harmonic_diagnostics():
    t, m = _history()
    # Chosen second-harmonic amplitude 0.5 and third-harmonic amplitude 0.25.
    m += 0.5 * np.cos(4.0 * t) + 0.25 * np.sin(6.0 * t)
    result = _reduce(t, m)
    assert result.stiffness == pytest.approx(36.0 / math.pi)
    assert result.damping == pytest.approx(9.0 / math.pi)
    assert result.harmonic_amplitudes[2] == pytest.approx(0.5)
    assert result.harmonic_amplitudes[3] == pytest.approx(0.25)
    # RSS(0.5, 0.25) / hypot(-1, -2) = 0.25, by hand.
    assert result.harmonic_ratio == pytest.approx(0.25)
    assert result.residual_rms == pytest.approx(0.0, abs=1e-12)
    assert result.first_harmonic_residual_rms > 0.0


def test_residual_matches_hand_calculated_sample_rms():
    # Four complete, uniformly sampled cycles without a duplicate endpoint.
    # Orthogonality gives RMS = sqrt((0.5**2 + 0.25**2)/2) = sqrt(5/32).
    t = np.arange(320) * (math.pi / 80.0)
    m = (-np.cos(2.0 * t) - 2.0 * np.sin(2.0 * t)
         + 0.5 * np.cos(4.0 * t) + 0.25 * np.sin(6.0 * t))
    result = _reduce(t, m, settled_window=(t[0], t[-1]))
    assert result.first_harmonic_residual_rms == pytest.approx(math.sqrt(5.0 / 32.0))
    assert result.residual_rms == pytest.approx(0.0, abs=1e-12)


@pytest.mark.parametrize("degrees, period, expected_k, expected_b", [
    # theta0=pi/36, omega=1: -b/theta0=72/pi; -a/(omega*theta0)=36/pi.
    (5.0, 2.0 * math.pi, 72.0 / math.pi, 36.0 / math.pi),
    # theta0=pi/9, omega=4: -b/theta0=18/pi; -a/(omega*theta0)=9/(4*pi).
    (20.0, math.pi / 2.0, 18.0 / math.pi, 9.0 / (4.0 * math.pi)),
])
def test_motion_normalization_scaling(degrees, period, expected_k, expected_b):
    t, _ = _history()
    phase = 2.0 * math.pi * t / period
    result = reduction.reduce_roll_moment_normalized(
        t, -np.cos(phase) - 2.0 * np.sin(phase), period,
        roll_amplitude_deg=degrees, settled_window=(t[0], t[-1]),
        min_cycles=2.0, max_condition_number=10.0,
    )
    assert result.stiffness == pytest.approx(expected_k)
    assert result.damping == pytest.approx(expected_b)


def test_reinforcing_response_preserves_negative_coefficients():
    t, m = _history()
    result = _reduce(t, -m)
    assert result.stiffness == pytest.approx(-36.0 / math.pi)
    assert result.damping == pytest.approx(-9.0 / math.pi)


def test_absolute_phase_and_partial_cycle_window():
    t, _ = _history()
    # Chosen nonzero absolute origin and nonintegral-cycle selection.
    t += 0.17
    m = -np.cos(2.0 * t) - 2.0 * np.sin(2.0 * t) + 0.5 * np.cos(4.0 * t)
    result = _reduce(t, m, settled_window=(t[0] + 0.5, t[-1] - 0.7))
    assert result.stiffness == pytest.approx(36.0 / math.pi)
    assert result.damping == pytest.approx(9.0 / math.pi)
    assert result.harmonic_amplitudes[2] == pytest.approx(0.5)


def test_large_finite_moments_do_not_overflow_residual_squares():
    # Chosen floating-point stress scale; all physical results remain finite.
    t, m = _history()
    result = _reduce(t, m * 1e200)
    assert result.stiffness == pytest.approx((36.0 / math.pi) * 1e200)
    assert result.damping == pytest.approx((9.0 / math.pi) * 1e200)
    assert math.isfinite(result.residual_rms)


def test_zero_signal_has_undefined_harmonic_ratio():
    t, m = _history()
    result = _reduce(t, np.zeros_like(m))
    assert result.harmonic_ratio is None
    assert result.stiffness == result.damping == result.residual_rms == 0.0


def test_nonuniform_grid():
    t, _ = _history()
    # Smooth monotone warp preserves endpoints and all four cycles.
    t += 0.01 * np.sin(t)
    result = _reduce(t, -np.cos(2.0 * t) - 2.0 * np.sin(2.0 * t))
    assert result.stiffness == pytest.approx(36.0 / math.pi)
    assert result.damping == pytest.approx(9.0 / math.pi)


@pytest.mark.parametrize("field", ["times", "moment"])
@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf])
def test_nonfinite_samples(field, value):
    t, m = _history()
    (t if field == "times" else m)[0] = value
    with pytest.raises(ValueError, match="finite"):
        _reduce(t, m, settled_window=(2.0 * math.pi, 4.0 * math.pi))


@pytest.mark.parametrize("kind", ["duplicate", "reversed", "two_dimensional", "mismatch", "empty"])
def test_invalid_sample_grid(kind):
    t, m = _history()
    if kind == "duplicate":
        t[1] = t[0]
    elif kind == "reversed":
        t, m = t[::-1], m[::-1]
    elif kind == "two_dimensional":
        t = t[:, None]
    elif kind == "mismatch":
        m = m[:-1]
    else:
        t, m = [], []
    with pytest.raises(ValueError):
        _reduce(t, m)


@pytest.mark.parametrize("window", [None, (0.0,), (0.0, 1.0, 2.0), (1.0, 0.0),
                                   (0.0, 0.0), (-1.0, 4.0 * math.pi),
                                   (0.0, 5.0 * math.pi), (0.0, math.inf)])
def test_invalid_window(window):
    t, m = _history()
    with pytest.raises(ValueError, match="window"):
        _reduce(t, m, settled_window=window)


def test_window_is_required():
    t, m = _history()
    with pytest.raises(TypeError, match="settled_window"):
        reduction.reduce_roll_moment_normalized(
            t, m, math.pi, roll_amplitude_deg=10.0,
            min_cycles=2.0, max_condition_number=10.0,
        )


@pytest.mark.parametrize("name", ["roll_amplitude_deg", "min_cycles", "max_condition_number"])
@pytest.mark.parametrize("value", [0.0, -1.0, math.nan, math.inf])
def test_invalid_motion_and_policies(name, value):
    t, m = _history()
    with pytest.raises(ValueError):
        _reduce(t, m, **{name: value})


@pytest.mark.parametrize("period", [0.0, -1.0, math.nan, math.inf, 1e-320, 1e308])
def test_invalid_frequency(period):
    t, m = _history()
    with pytest.raises(ValueError):
        reduction.reduce_roll_moment_normalized(
            t, m, period, roll_amplitude_deg=10.0,
            settled_window=(0.0, 4.0 * math.pi), min_cycles=2.0,
            max_condition_number=10.0,
        )


def test_insufficient_cycles():
    t, m = _history()
    with pytest.raises(ValueError, match="cycles"):
        _reduce(t, m, settled_window=(0.0, math.pi), min_cycles=2.0)
    # Requested boundaries cannot stand in for actual sampled coverage.
    with pytest.raises(ValueError, match="cycles"):
        _reduce(t, m, settled_window=(0.01, 2.0 * math.pi), min_cycles=2.0)


def test_too_few_selected_samples():
    t, m = _history()
    with pytest.raises(ValueError, match="samples"):
        _reduce(t, m, settled_window=(t[0], t[6]))


@pytest.mark.parametrize("policy", ["min_cycles", "max_condition_number"])
def test_policy_cannot_be_less_than_one(policy):
    t, m = _history()
    with pytest.raises(ValueError):
        _reduce(t, m, **{policy: 0.5})


def test_ambiguous_sampling():
    # Every sample has the same drive phase: no harmonic can be identified.
    t = np.arange(9) * math.pi
    with pytest.raises(ValueError):
        _reduce(t, np.ones_like(t), settled_window=(0.0, 8.0 * math.pi))


def test_sparse_sampling_and_explicit_condition_limit():
    t = np.linspace(0.0, 4.0 * math.pi, 17)
    with pytest.raises(ValueError, match="sampling"):
        _reduce(t, np.sin(2.0 * t))
    t, m = _history()
    # A limit of one rejects the non-orthonormal DC/cos/sin matrix.
    with pytest.raises(ValueError, match="condition"):
        _reduce(t, m, max_condition_number=1.0)


def test_raw_contract_remains_explicit():
    t, m = _history()
    row = reduction.reduce_roll_moment(
        t, m, math.pi, fill_level=0.5, roll_amplitude_deg=10.0,
    )
    assert tuple(row) == CONTRACT_FIELDS
    assert row["in_phase_coeff"] == pytest.approx(2.0)  # Raw N.m, not K.
    assert row["quad_coeff"] == pytest.approx(1.0)  # Raw N.m, not B.
    assert "not normalized" in reduction.reduce_roll_moment.__doc__
