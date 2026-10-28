# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""ITTC 7.5-02-07-02.2 (2024 rev. 07), eq. 19; hand quadrature controls."""

import math
from dataclasses import replace

import numpy as np
import pytest

from digitalmodel.hydrodynamics.added_resistance import (
    OperatingPoint,
    RegularWaves,
    Ship,
    Status,
)
from digitalmodel.hydrodynamics.added_resistance.reflection import (
    ReflectionEvidence,
    stawave1_equivalent_regular_table,
)
from digitalmodel.hydrodynamics.added_resistance.spectra import (
    integrate_regular_table,
    mean_added_resistance,
    pm_spectrum,
)


def test_unit_area_spectrum():
    """Triangle on omega=[1,2,3], S=[0,1,0] has area 1 m².

    Unit K=1 N/m² gives 2*integral S*K=2 N from equation (19).
    """
    assert mean_added_resistance(
        [1, 2, 3], [0, 1, 0], [1, 1, 1], omega_min=1, omega_max=3
    ) == pytest.approx(2)


@pytest.mark.parametrize("width", [0.5, 0.05, 0.005])
def test_delta_limit_matches_regular_wave(width):
    """Dirac approximation at omega=2 has area a²/2=2 m² for a=2 m.

    A triangle of half-width d and height 2/d has exactly that area.
    Linear K=[2-d,2,2+d] N/m² has weighted mean K(2)=2, so
    2*integral(S*K)=8 N, identical to regular R=K*a²=2*4.
    """
    omega = [2 - width, 2, 2 + width]
    assert mean_added_resistance(
        omega, [0, 2 / width, 0], omega, omega_min=omega[0], omega_max=omega[-1]
    ) == pytest.approx(8)


def test_nonconstant_response_hand_integral():
    """S=[1,2,1], K=[3,4,5] at omega=[1,2,3].

    Product samples=[3,8,5]; trapezoid areas=5.5+6.5=12; mean R=24 N.
    This checks trapezoidal product quadrature, not an exact curved integrand.
    """
    assert mean_added_resistance(
        [1, 2, 3], [1, 2, 1], [3, 4, 5], omega_min=1, omega_max=3
    ) == pytest.approx(24)


def test_constant_spectrum_reproduces_stawave1():
    """Hs=2 => m0=Hs²/16=0.25. Triangle height=0.25 over [1,3].

    K=50000 N/m² from G-1 hand calculation, giving 2*0.25*50000=25000 N.
    Wavelength order is reversed in frequency; a varies to test division by a².
    """
    ship = Ship(100, 20, 80, 6, 1000, 10)
    lengths = [2 * math.pi * 10, 2 * math.pi * 10 / 9]
    rows = stawave1_equivalent_regular_table(
        ship,
        OperatingPoint(5, 0),
        RegularWaves(lengths, [1, 2]),
        ReflectionEvidence(2, 0.1, "measured", True),
    )
    assert integrate_regular_table(
        rows, [1, 2, 3], [0, 0.25, 0], omega_min=1, omega_max=3
    ) == pytest.approx(25000)


def test_unevaluated_table_and_missing_coverage_rejected():
    ship = Ship(100, 20, 80, 6, 1000, 10)
    rows = stawave1_equivalent_regular_table(
        ship,
        OperatingPoint(5, 0),
        RegularWaves([20 * math.pi, 20 * math.pi / 9], [1, 1]),
        ReflectionEvidence(2, 0.1, "measured", True),
    )
    with pytest.raises(ValueError, match="coverage"):
        integrate_regular_table(
            rows, [0.5, 2, 3], [0, 1, 0], omega_min=0.5, omega_max=3
        )
    rows[0] = replace(rows[0], status=Status.NOT_EVALUATED, resistance_n=None)
    with pytest.raises(ValueError, match="NOT_EVALUATED"):
        integrate_regular_table(rows, [1, 2, 3], [0, 1, 0], omega_min=1, omega_max=3)


@pytest.mark.parametrize(
    "omega,spectrum,response",
    [
        ([1], [1], [1]),
        ([2, 1], [1, 1], [1, 1]),
        ([1, 1], [1, 1], [1, 1]),
        ([1, 2], [1], [1, 1]),
        ([1, 2], [1, -1], [1, 1]),
        ([1, 2], [1, 1], [1, -1]),
        ([1, 2], [1, float("nan")], [1, 1]),
        ([1, float("inf")], [1, 1], [1, 1]),
    ],
)
def test_invalid_quadrature_inputs(omega, spectrum, response):
    with pytest.raises(ValueError):
        mean_added_resistance(omega, spectrum, response, omega_min=1, omega_max=2)


@pytest.mark.parametrize(
    "low,high", [(0, 3), (3, 1), (1.1, 3), (1, 2.9), (float("nan"), 3)]
)
def test_cutoff_must_be_explicit_and_match_grid(low, high):
    with pytest.raises(ValueError):
        mean_added_resistance(
            [1, 2, 3], [0, 1, 0], [1, 1, 1], omega_min=low, omega_max=high
        )


def test_pm_normalized_energy_and_tail_are_separate():
    """Repository two-parameter spectrum form: S=A*w^-5*exp[-beta*(wp/w)^4], beta=5/4.

    Substitution y=beta*(wp/w)^4 gives normalized cumulative variance
    below w equal to exp[-beta*(wp/w)^4]. At [wp/2, 5*wp], omitted
    fraction = exp(-20) + 1-exp(-0.002). This analytic hand expression
    is independent of the reused generator and numerical quadrature.
    The generated finite band is normalized to Hs²/16=0.25 m² for Hs=2.
    """
    wp = 2 * math.pi / 6
    result = pm_spectrum(2, 6, omega_min=wp / 2, omega_max=5 * wp, n_points=2001)
    assert result.omitted_variance_fraction == pytest.approx(
        math.exp(-20) - math.expm1(-0.002)
    )
    assert result.variance_m2 == pytest.approx(0.25)
    assert result.normalized_to_hs
    assert np.all(result.density_m2_s >= 0)


def test_tail_issue_exposed_for_default_like_cutoff():
    """Issue #2613: normalization of m0 cannot restore missing high-frequency tail."""
    narrow = pm_spectrum(2, 5, omega_min=0.02, omega_max=2, n_points=2001)
    wide = pm_spectrum(2, 5, omega_min=0.02, omega_max=10, n_points=2001)
    assert narrow.omitted_variance_fraction > wide.omitted_variance_fraction
    assert narrow.variance_m2 == pytest.approx(wide.variance_m2)


@pytest.mark.parametrize(
    "hs,tp,low,high,n",
    [
        (-1, 6, 0.1, 5, 100),
        (1, 0, 0.1, 5, 100),
        (1, 6, 0, 5, 100),
        (1, 6, 5, 1, 100),
        (1, 6, 0.1, 5, 1),
        (1, 6, 0.1, 5, 2.5),
    ],
)
def test_invalid_pm_inputs(hs, tp, low, high, n):
    with pytest.raises(ValueError):
        pm_spectrum(hs, tp, omega_min=low, omega_max=high, n_points=n)


def test_calm_pm_has_zero_energy():
    band = pm_spectrum(0, 6, omega_min=0.1, omega_max=5, n_points=100)
    assert band.variance_m2 == 0
