# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent analytic checks of the base-driven linear oscillator.

All physical values below are synthetic choices in SI units. Expected histories
are obtained by substituting the written solutions into u''+2*zeta*w*u'+w²*u=-ag,
not by another time integrator. Numerical tolerances are test accuracy targets.
"""

import numpy as np
import pytest

from digitalmodel.seismic.sdof import integrate_sdof


def test_zero_input_stays_zero():
    # Homogeneous equation and zero initial state imply u=v=a=0 by uniqueness.
    time = np.linspace(-2.0, 2.0, 101)
    result = integrate_sdof(time, np.zeros_like(time), natural_period_s=1.0)

    np.testing.assert_array_equal(result.time_s, time)
    for values in (
        result.relative_displacement_m,
        result.relative_velocity_m_s,
        result.relative_acceleration_m_s2,
        result.absolute_acceleration_m_s2,
    ):
        np.testing.assert_array_equal(values, np.zeros_like(time))


@pytest.mark.parametrize("damping_ratio", [0.0, 0.05])
def test_free_vibration_matches_closed_form(damping_ratio):
    # Characteristic roots are -zeta*w +/- i*wd, wd=w*sqrt(1-zeta²).
    # u=exp(-zeta*w*tau)*(C*cos(wd*tau)+D*sin(wd*tau)).
    # At tau=0: C=u0, D=(v0+zeta*w*u0)/wd. Differentiate for v and a.
    w, u0, v0 = 2.0, 0.3, -0.2
    tau = np.linspace(0.0, 2.0 * np.pi, 4001)
    time = tau + 7.0  # Initial state belongs to the supplied first time sample.
    decay = damping_ratio * w
    wd = w * np.sqrt(1.0 - damping_ratio**2)
    d = (v0 + decay * u0) / wd
    wave = u0 * np.cos(wd * tau) + d * np.sin(wd * tau)
    wave_dot = wd * (-u0 * np.sin(wd * tau) + d * np.cos(wd * tau))
    exact_u = np.exp(-decay * tau) * wave
    exact_v = np.exp(-decay * tau) * (wave_dot - decay * wave)
    exact_a = np.exp(-decay * tau) * (
        (decay**2 - wd**2) * wave - 2.0 * decay * wave_dot
    )

    result = integrate_sdof(
        time,
        np.zeros_like(time),
        natural_period_s=2.0 * np.pi / w,
        damping_ratio=damping_ratio,
        initial_displacement_m=u0,
        initial_velocity_m_s=v0,
    )

    # Absolute error targets: 5 micrometres, 10 micrometres/s, 20 micrometres/s².
    np.testing.assert_allclose(
        result.relative_displacement_m, exact_u, rtol=0, atol=5e-6,
    )
    np.testing.assert_allclose(result.relative_velocity_m_s, exact_v, rtol=0, atol=1e-5)
    np.testing.assert_allclose(
        result.relative_acceleration_m_s2, exact_a, rtol=0, atol=2e-5,
    )


def test_sine_base_acceleration_converges():
    # For zeta=0, ag=A*sin(O*tau), w != O: a particular solution is
    # -A*sin(O*tau)/(w²-O²). Adding A*O*sin(w*tau)/(w*(w²-O²))
    # cancels its initial velocity; both initial displacements are zero.
    # Differentiation gives the independent v and a expressions below.
    w, forcing_w, amplitude = 2.0, 1.0, 0.4
    coefficient = amplitude / (w**2 - forcing_w**2)
    errors = []
    for intervals in (128, 256, 512):  # Two successive time-step halvings.
        tau = np.linspace(0.0, 2.0 * np.pi, intervals + 1)
        ground = amplitude * np.sin(forcing_w * tau)
        exact_u = -coefficient * (
            np.sin(forcing_w * tau) - forcing_w / w * np.sin(w * tau)
        )
        exact_v = -coefficient * forcing_w * (
            np.cos(forcing_w * tau) - np.cos(w * tau)
        )
        exact_a = coefficient * (
            forcing_w**2 * np.sin(forcing_w * tau)
            - forcing_w * w * np.sin(w * tau)
        )
        result = integrate_sdof(tau, ground, natural_period_s=2.0 * np.pi / w)
        errors.append([
            np.max(np.abs(actual - exact))
            for actual, exact in (
                (result.relative_displacement_m, exact_u),
                (result.relative_velocity_m_s, exact_v),
                (result.relative_acceleration_m_s2, exact_a),
            )
        ])
        np.testing.assert_array_equal(
            result.absolute_acceleration_m_s2,
            result.relative_acceleration_m_s2 + ground,
        )

    errors = np.asarray(errors)
    # O(dt²) predicts factor 4 on halving; allow factors 3.5 through 4.5.
    ratios = errors[:-1] / errors[1:]
    assert np.all((ratios > 3.5) & (ratios < 4.5))
    # Fine-grid SI absolute targets: 0.1 mm, 0.2 mm/s, 0.4 mm/s².
    assert np.all(errors[-1] < [1e-4, 2e-4, 4e-4])
    print("Refinement maximum errors (m, m/s, m/s²):", errors.tolist())


def test_initial_acceleration_follows_equilibrium():
    # w=2, zeta=0.1, u0=0.3, v0=-0.2, ag0=0.7:
    # a0=-ag0-2*zeta*w*v0-w²*u0=-0.7+0.08-1.2=-1.82 m/s².
    result = integrate_sdof(
        [5.0, 5.1], [0.7, 0.4], natural_period_s=np.pi,
        damping_ratio=0.1, mass_kg=3.0,
        initial_displacement_m=0.3, initial_velocity_m_s=-0.2,
    )
    assert result.relative_displacement_m[0] == 0.3
    assert result.relative_velocity_m_s[0] == -0.2
    assert result.relative_acceleration_m_s2[0] == pytest.approx(-1.82)
    assert result.absolute_acceleration_m_s2[0] == pytest.approx(-1.12)
    # Mass cancels for base acceleration at fixed period and damping ratio.
    unit_mass = integrate_sdof(
        [5.0, 5.1], [0.7, 0.4], natural_period_s=np.pi,
        damping_ratio=0.1, mass_kg=1.0,
        initial_displacement_m=0.3, initial_velocity_m_s=-0.2,
    )
    np.testing.assert_array_equal(
        result.relative_displacement_m, unit_mass.relative_displacement_m,
    )


@pytest.mark.parametrize("damping_ratio", [0.0, 0.05, 1.0, 2.0])
def test_static_equilibrium_and_samplewise_force_balance(damping_ratio):
    # With ag=0.8 and w=2, u=-ag/w²=-0.2 and v=a=0 for any damping.
    time = np.linspace(0.0, 2.0, 41)
    ground = np.full_like(time, 0.8)
    result = integrate_sdof(
        time, ground, natural_period_s=np.pi, damping_ratio=damping_ratio,
        initial_displacement_m=-0.2,
    )
    np.testing.assert_allclose(result.relative_displacement_m, -0.2, rtol=0, atol=1e-14)
    np.testing.assert_allclose(result.relative_velocity_m_s, 0.0, rtol=0, atol=1e-14)
    np.testing.assert_allclose(
        result.relative_acceleration_m_s2, 0.0, rtol=0, atol=1e-14,
    )
    np.testing.assert_allclose(
        result.absolute_acceleration_m_s2, ground, rtol=0, atol=1e-14,
    )


def test_samplewise_dynamic_force_balance_and_input_ownership():
    time = np.linspace(-1.0, 3.0, 201)
    ground = 0.4 * np.cos(time)
    saved_time, saved_ground = time.copy(), ground.copy()
    result = integrate_sdof(
        time, ground, natural_period_s=np.pi, damping_ratio=0.1,
    )
    # Normalised equation: a+0.4*v+4*u=-ag for w=2, zeta=0.1.
    residual = (result.relative_acceleration_m_s2
                + 0.4 * result.relative_velocity_m_s
                + 4.0 * result.relative_displacement_m + ground)
    np.testing.assert_allclose(residual, 0.0, rtol=0, atol=1e-14)
    np.testing.assert_array_equal(time, saved_time)
    np.testing.assert_array_equal(ground, saved_ground)
    time[:] = 0.0
    ground[:] = 0.0
    np.testing.assert_array_equal(result.time_s, saved_time)
    np.testing.assert_array_equal(
        result.absolute_acceleration_m_s2,
        result.relative_acceleration_m_s2 + saved_ground,
    )


@pytest.mark.parametrize("time,ground,message", [
    ([], [], "at least two"),
    ([0.0], [0.0], "at least two"),
    ([0.0, 1.0], [0.0], "same length"),
    ([[0.0, 1.0]], [0.0, 0.0], "one-dimensional"),
    ([0.0, 1.0], [[0.0, 0.0]], "one-dimensional"),
    ([0.0, np.nan], [0.0, 0.0], "finite"),
    ([0.0, np.inf], [0.0, 0.0], "finite"),
    ([0.0, 1.0], [np.nan, 0.0], "finite"),
    ([0.0, 1.0], [0.0, np.inf], "finite"),
    ([1.0, 1.0], [0.0, 0.0], "increasing"),
    ([1.0, 0.0], [0.0, 0.0], "increasing"),
    ([0.0, 0.1, 0.21], [0.0, 0.0, 0.0], "uniform"),
    # A relative uniformity check must also reject nonuniform tiny time steps.
    ([0.0, 1e-14, 3e-14], [0.0, 0.0, 0.0], "uniform"),
])
def test_invalid_time_and_ground_samples_raise(time, ground, message):
    with pytest.raises(ValueError, match=message):
        integrate_sdof(time, ground, natural_period_s=1.0)


@pytest.mark.parametrize("name,value", [
    ("natural_period_s", 0.0), ("natural_period_s", -1.0),
    ("natural_period_s", np.nan), ("natural_period_s", np.inf),
    ("mass_kg", 0.0), ("mass_kg", -1.0),
    ("mass_kg", np.nan), ("mass_kg", np.inf),
    ("damping_ratio", -0.1), ("damping_ratio", np.nan),
    ("damping_ratio", np.inf),
    ("initial_displacement_m", np.nan), ("initial_displacement_m", np.inf),
    ("initial_velocity_m_s", np.nan), ("initial_velocity_m_s", np.inf),
    ("natural_period_s", [1.0]), ("mass_kg", None),
    ("damping_ratio", "invalid"),
])
def test_invalid_physical_inputs_raise(name, value):
    options = {"natural_period_s": 1.0, name: value}
    with pytest.raises(ValueError, match=name):
        integrate_sdof([0.0, 0.1], [0.0, 0.0], **options)


def test_unsupported_method_raises():
    with pytest.raises(ValueError, match="average_acceleration"):
        integrate_sdof(
            [0.0, 0.1], [0.0, 0.0], natural_period_s=1.0,
            method="linear_acceleration",
        )


def test_finite_inputs_exceeding_arithmetic_range_raise():
    # Squaring w=2*pi/tiny exceeds the largest representable float.
    with pytest.raises(ValueError, match="floating-point range"):
        integrate_sdof(
            [0.0, 1.0], [0.0, 0.0],
            natural_period_s=np.finfo(float).tiny,
        )
    # Finite endpoints need not have a finite representable difference.
    largest = np.finfo(float).max
    with pytest.raises(ValueError, match="intervals must be finite"):
        integrate_sdof([-largest, largest], [0.0, 0.0], natural_period_s=1.0)


def test_undamped_energy_remains_bounded_with_coarse_steps():
    # Multiply the free equation by v: d(v²+w²*u²)/dt=0. Average acceleration
    # preserves this quadratic invariant even for h greater than a period.
    # Synthetic w=2, u0=0.3, v0=-0.2 give v0²+w²*u0²=0.4 m²/s².
    time = np.arange(51, dtype=float) * (1.5 * np.pi)
    result = integrate_sdof(
        time, np.zeros_like(time), natural_period_s=np.pi,
        initial_displacement_m=0.3, initial_velocity_m_s=-0.2,
    )
    energy_per_half_mass = (
        result.relative_velocity_m_s**2 + 4.0 * result.relative_displacement_m**2
    )
    np.testing.assert_allclose(energy_per_half_mass, 0.4, rtol=0, atol=1e-13)
