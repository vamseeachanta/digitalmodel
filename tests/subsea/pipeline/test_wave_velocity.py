# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Wave examples from independent limiting cases and quadrature integrals.

Sources: Coastal Dynamics, 2021 online edition, Appendix A, dispersion relation
and Table A.2 (bed-origin vertical coordinate); sections 3.4.3 and 3.4.4,
spectral moments and H_m0=4*sqrt(m0). Source links are in the reference document.
Gravity: NIST SP 811, 2008 edition, Appendix B.8, standard free-fall acceleration.
All sea states, grids and tolerances below are synthetic test choices.
"""

import math

import numpy as np
import pytest

from digitalmodel.subsea.pipeline.free_span.wave_velocity import (
    compute_wave_velocity,
    jonswap_gamma,
    jonswap_spectrum,
    solve_dispersion,
    spectral_moments,
    velocity_transfer,
)


G = 9.80665  # m/s²; NIST SP 811 (2008), Appendix B.8, standard free fall.


@pytest.mark.parametrize(
    "omega,depth,expected,relative",
    [
        (math.sqrt(G), 100.0, 1.0, 1e-12),
        (0.001 * math.sqrt(G), 0.01, 0.01, 2e-9),
        (math.sqrt(G / 10.0), 10.0, 0.119968, 4e-6),
    ],
    ids=["deep", "shallow", "intermediate"],
)
def test_dispersion_limits_and_hand_bisection(omega, depth, expected, relative):
    """Deep: omega²/g=1; tanh(100) differs from 1 by less than 3e-87.

    Shallow: omega/sqrt(g*h)=.001/sqrt(.01)=.01; kh=.0001.
    tanh expansion gives relative k correction (kh)²/6 = 1.67e-9;
    the 2e-9 tolerance includes this finite-depth approximation error.
    Intermediate: x=kh, x*tanh(x)=1, bracket [1,1.5]. Hand bisection:
      step 1:  x=1.25000000, f= .06035455
      step 2:  x=1.12500000, f=-.08953630
      step 3:  x=1.18750000, f=-.01461023
      step 8:  x=1.20117188, f= .00179140
      step 13: x=1.19964600, f=-.00003916
      step 18: x=1.19967842, f=-.00000026.
    Thus k=.119968 /m to six decimal places; 4e-6 relative tolerance
    accommodates only the last-place rounding of this hand answer.
    Residual tolerance 1e-11 is independent of that rounded oracle.
    """
    k = solve_dispersion(np.array([omega]), depth, tol=1e-12)
    assert k[0] == pytest.approx(expected, rel=relative)
    residual = omega**2 - G * k[0] * math.tanh(k[0] * depth)
    assert residual == pytest.approx(0.0, abs=1e-11)


def test_dispersion_vector_convergence_tolerance():
    """Different depths relative to wavelength share the 1e-10 residual budget."""
    omega = np.array([0.01, 0.5, 2.0])  # Synthetic angular frequencies [rad/s].
    k = solve_dispersion(omega, 10.0, tol=1e-12, max_iter=100)
    assert k.shape == omega.shape
    assert np.all(k > 0)
    assert omega**2 - G * k * np.tanh(10.0 * k) == pytest.approx(np.zeros(3), abs=1e-10)


@pytest.mark.parametrize("iterations", [0, 1])
@pytest.mark.xfail(
    strict=True,
    reason="Iteration exhaustion returns an unconverged result; see reference document",
)
def test_dispersion_refuses_exhausted_iteration_budget(iterations):
    """For x*tanh(x)=1, x0=1 is not a root; zero/one steps cannot converge."""
    with pytest.raises(RuntimeError, match="converg"):
        solve_dispersion(np.array([math.sqrt(G / 10.0)]), 10.0, max_iter=iterations)


def test_velocity_transfer_hand_hyperbolic_ratio():
    """Synthetic omega=2,k=.1,h=10,z'=5: G=2*cosh(.5)/sinh(1).

    exp(.5)=1.6487212707; exp(-.5)=.6065306597 -> cosh(.5)=1.1276259652.
    exp(1)=2.7182818285; exp(-1)=.3678794412 -> sinh(1)=1.1752011936.
    G=1.9190347513 /s. Absolute 1e-10 covers the displayed hand rounding.
    Source: Coastal Dynamics (2021), Appendix A, Table A.2 horizontal velocity.
    """
    actual = velocity_transfer(np.array([2.0]), np.array([0.1]), 10.0, 5.0)
    assert actual[0] == pytest.approx(1.9190347513, abs=1e-10)


def test_rectangular_spectrum_exact_moments():
    """S=2 on [1,3]: m0=2*(3-1)=4; m2=2*(27-1)/3=52/3.

    Source: Coastal Dynamics (2021), section 3.4.3, moment definition.
    For 1000 trapezoids dx=.002, quadratic integration error is
    2*(3-1)*dx²/6=2.667e-6. Absolute m2 tolerance 3e-6 covers it;
    m0 integrates a constant exactly (1e-12 floating-point budget).
    """
    omega = np.linspace(1.0, 3.0, 1001)
    m0, m2 = spectral_moments(omega, np.full_like(omega, 2.0))
    assert m0 == pytest.approx(4.0, abs=1e-12)
    assert m2 == pytest.approx(52.0 / 3.0, abs=3e-6)


@pytest.mark.parametrize(
    "gamma",
    [
        1.0,
        pytest.param(
            2.0,
            marks=pytest.mark.xfail(
                strict=True,
                raises=AssertionError,
                reason="Enhanced spectrum energy exceeds Hs squared / 16",
            ),
        ),
        pytest.param(
            5.0,
            marks=pytest.mark.xfail(
                strict=True,
                raises=AssertionError,
                reason="Enhanced spectrum energy exceeds Hs squared / 16",
            ),
        ),
    ],
)
def test_jonswap_integral_preserves_specified_wave_height(gamma):
    """Synthetic Hs=4,Tp=8: m0=Hs²/16=16/16=1 m², for any gamma.

    Source: Coastal Dynamics (2021), section 3.4.4, H_m0 definition.
    Independent tail derivation for gamma=1: set x=omega/omega_peak and
    assume the shape family x^-5*exp(-b/x⁴), b>0. Its log derivative is
    -5/x+4*b/x⁵; a peak at x=1 requires b=5/4=1.25. With u=b/x⁴,
    du=-4*b*x^-5 dx, so the integral over (0,infinity) is 1/(4*b).
    Thus the unit-integral density is q(x)=4*b*x^-5*exp(-b/x⁴), with
    antiderivative exp(-b/x⁴). The upper tail above R is 1-exp(-b/R⁴);
    the lower tail below L is exp(-b/L⁴). These expressions are derived
    here, not attributed to the wave-height reference. They apply only
    to gamma=1; the expected m0 remains the independent H_m0 relation.
    Integrate [.01,20]*omega_peak with 50001 bins. At R=20 the omitted
    upper fraction is below b/R⁴=1/128000<8e-6; at L=.01 the lower
    fraction is exp(-125000000)<1e-100. Absolute 1e-4
    allows truncation and quadrature; gamma is an explicit synthetic input.
    No coefficient is inferred from an unopened standard.
    """
    omega = np.linspace(0.01, 20.0, 50001) * (math.pi / 4.0)
    spectrum = jonswap_spectrum(omega, Hs=4.0, Tp=8.0, gamma=gamma)
    integrate = getattr(np, "trapezoid", None) or np.trapz
    assert integrate(spectrum, omega) == pytest.approx(1.0, abs=1e-4)


def test_jonswap_gamma_scale_invariance_and_monotonicity():
    """Only invariants: scaling (Hs,Tp) by (4,2) preserves Tp/sqrt(Hs).

    Coefficient values and range boundaries are NOT_EVALUATED: the cited
    standard clause was not available as an authorized open source.
    Synthetic periods exercise a wide range; 1e-12 covers floating-point error.
    """
    periods = [2.0, 4.0, 20.0]
    values = [jonswap_gamma(1.0, period) for period in periods]
    assert values[0] >= values[1] >= values[2]
    assert values[0] > values[-1]
    assert values == pytest.approx(
        [jonswap_gamma(4.0, 2.0 * period) for period in periods], abs=1e-12
    )


@pytest.mark.parametrize("height,period", [(0.0, 8.0), (4.0, 0.0)])
def test_zero_sea_state_has_zero_velocity_and_moments(height, period):
    """Zero excitation has exact zero velocity; no fatigue/design qualification."""
    result = compute_wave_velocity(height, period, 10.0, 0.0)
    assert (result.Uw_ms, result.Tu_s, result.M0, result.M2) == (0.0, 0.0, 0.0, 0.0)
