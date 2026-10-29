# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic arithmetic probes; no engineering design inputs.

Hand calculation: one-sided S(f)=1 on [99/100, 101/100] Hz, zero outside,
so M_k=(b**(k+1)-a**(k+1))/(k+1), k=0,1,2,4. Rice rates are
sqrt(M_2/M_0) and sqrt(M_4/M_2) cycles/s. Angular moments are (2*pi)**k*M_k.
Stretching frequency by c=7/2 with S_c(f)=S(f/c)/c preserves variance and
multiplies each rate by c. These rational endpoints and scale are arbitrary
synthetic choices. See docs/domains/fatigue/spectral-frequency-units.md.

For N=32 equal subintervals, the composite trapezoidal remainder is bounded
by (b-a)*h**2*max|k*(k-1)*f**(k-2)|/12 (zero for k=0,1).
Positive moment intervals propagate through sqrt(numerator/denominator)
using the lower numerator and upper denominator for the lower rate, and
vice versa. A floating-point allowance gamma_(8*N+32) also covers arithmetic:
u=ulp(1)/2, gamma_n=n*u/(1-n*u). The operation budget includes grid, powers,
quadrature, conversion and ratios; it is independent of production results.
"""

import math
from fractions import Fraction

import numpy as np
import pytest

from digitalmodel.fatigue.spectral_fatigue import (
    SpectralMoments,
    benasciutti_tovo_damage,
    compute_spectral_moments,
    dirlik_damage,
    narrow_band_damage,
    wirsching_light_damage,
)
from digitalmodel.structural.fatigue.frequency_domain import (
    NarrowBandMethod,
    SpectralMoments as AngularMoments,
)

ORDERS = (0, 1, 2, 4)
SCALES = (Fraction(1), Fraction(7, 2))
SUBINTERVALS = 32
UNIT_ROUNDOFF = math.ulp(1.0) / 2
OPERATIONS = 8 * SUBINTERVALS + 32
ROUNDOFF = OPERATIONS * UNIT_ROUNDOFF / (1 - OPERATIONS * UNIT_ROUNDOFF)


def _rectangle(scale):
    """Return closed-form Hz moments and bounded numerical inputs."""
    a, b = Fraction(99, 100) * scale, Fraction(101, 100) * scale
    exact = {
        k: float((b ** (k + 1) - a ** (k + 1)) / ((k + 1) * scale))
        for k in ORDERS
    }
    h = (b - a) / SUBINTERVALS
    errors = {}
    for k in ORDERS:
        derivative = k * (k - 1) * b ** (k - 2) if k >= 2 else 0
        quadrature = (b - a) * h**2 * derivative / (12 * scale)
        errors[k] = float(quadrature) + ROUNDOFF * float((b - a) * b**k / scale)
    frequency = np.linspace(float(a), float(b), SUBINTERVALS + 1)
    psd = np.ones_like(frequency) / float(scale)
    return exact, errors, frequency, psd


def _hz_moments(exact):
    return SpectralMoments(**{f"m{k}": exact[k] for k in ORDERS})


def _rate_bounds(exact, errors, numerator, denominator):
    lower = math.sqrt(
        (exact[numerator] - errors[numerator])
        / (exact[denominator] + errors[denominator])
    )
    upper = math.sqrt(
        (exact[numerator] + errors[numerator])
        / (exact[denominator] - errors[denominator])
    )
    return lower * (1 - ROUNDOFF), upper * (1 + ROUNDOFF)


@pytest.mark.parametrize("scale", SCALES)
def test_hz_moments_produce_hz_crossing_rates(scale):
    exact, _, _, _ = _rectangle(scale)
    moments = _hz_moments(exact)
    assert moments.nu_0 == pytest.approx(math.sqrt(exact[2] / exact[0]), rel=ROUNDOFF)
    assert moments.nu_p == pytest.approx(math.sqrt(exact[4] / exact[2]), rel=ROUNDOFF)
    base, _, _, _ = _rectangle(SCALES[0])
    assert moments.nu_0 == pytest.approx(
        float(scale) * math.sqrt(base[2] / base[0]), rel=ROUNDOFF,
    )
    assert moments.nu_p == pytest.approx(
        float(scale) * math.sqrt(base[4] / base[2]), rel=ROUNDOFF,
    )


@pytest.mark.parametrize("scale", SCALES)
def test_integrated_rectangular_psd_matches_bounded_quadrature(scale):
    exact, errors, frequency, psd = _rectangle(scale)
    moments = compute_spectral_moments(frequency, psd)
    for k in ORDERS:
        assert abs(getattr(moments, f"m{k}") - exact[k]) <= errors[k]
    for rate, numerator, denominator in ((moments.nu_0, 2, 0), (moments.nu_p, 4, 2)):
        lower, upper = _rate_bounds(exact, errors, numerator, denominator)
        assert lower <= rate <= upper


@pytest.mark.parametrize("scale", SCALES)
def test_structural_angular_moments_agree(scale):
    exact, errors, frequency, psd = _rectangle(scale)
    angular = {k: math.tau**k * exact[k] for k in ORDERS}
    moments = AngularMoments(
        **{f"m{k}": angular[k] for k in ORDERS},
        lambda1=angular[1] / angular[0],
        lambda2=math.sqrt(angular[2] / angular[0]),
        nu=math.sqrt(angular[0] * angular[2]) / angular[1],
        alpha=angular[2] / math.sqrt(angular[0] * angular[4]),
        q=angular[1] / math.sqrt(angular[0] * angular[2]),
    )
    method = NarrowBandMethod()
    integrated = method._calculate_spectral_moments(frequency, psd)
    hz = compute_spectral_moments(frequency, psd)
    for k in ORDERS:
        assert abs(getattr(integrated, f"m{k}") / math.tau**k - exact[k]) <= errors[k]
    for calculate, numerator, denominator, hz_rate in (
        (method._calculate_zero_crossing_rate, 2, 0, hz.nu_0),
        (method._calculate_peak_rate, 4, 2, hz.nu_p),
    ):
        expected = math.sqrt(exact[numerator] / exact[denominator])
        assert calculate(moments) == pytest.approx(expected, rel=ROUNDOFF)
        lower, upper = _rate_bounds(exact, errors, numerator, denominator)
        assert lower <= calculate(integrated) <= upper
        assert calculate(integrated) == pytest.approx(hz_rate, rel=ROUNDOFF)


@pytest.mark.parametrize("integrated", [False, True])
def test_narrow_band_absolute_damage_matches_rayleigh_second_moment(integrated):
    """Hand calculation: range R=2*amplitude, E[R²]=8*M0, N(R)=A/R².

    Choose synthetic exponent 2, A=1, log10(A)=0; Ddot=nu0*8*M0/A.
    Durations 1 and 37 seconds probe the existing duration-independent rate
    contract. Annualization retains the API's 365.25 days, 24 hours, 3600 s.
    """
    results = []
    for scale in SCALES:
        exact, errors, frequency, psd = _rectangle(scale)
        moments = compute_spectral_moments(frequency, psd) if integrated else _hz_moments(exact)
        lower, upper = _rate_bounds(exact, errors, 2, 0)
        damage_lower = lower * 8 * (exact[0] - errors[0])
        damage_upper = upper * 8 * (exact[0] + errors[0])
        expected = math.sqrt(exact[2] / exact[0]) * 8 * exact[0]
        result = narrow_band_damage(moments, sn_slope=2, sn_intercept=0)
        if integrated:
            assert damage_lower <= result.damage_rate <= damage_upper
        else:
            assert result.damage_rate == pytest.approx(expected, rel=ROUNDOFF)
        peak_lower, peak_upper = _rate_bounds(exact, errors, 4, 2)
        assert peak_lower <= result.peak_rate <= peak_upper
        assert result.damage_per_year == pytest.approx(
            result.damage_rate * 365.25 * 24 * 3600, rel=ROUNDOFF,
        )
        assert narrow_band_damage(moments, 2, 0, duration=37) == result
        results.append(result)
    assert results[1].damage_rate == pytest.approx(
        float(SCALES[1]) * results[0].damage_rate, rel=ROUNDOFF,
    )
    assert results[1].peak_rate == pytest.approx(
        float(SCALES[1]) * results[0].peak_rate, rel=ROUNDOFF,
    )


@pytest.mark.parametrize("method", [wirsching_light_damage, dirlik_damage, benasciutti_tovo_damage])
def test_dimensionless_corrections_preserve_frequency_scaling(method):
    """Frequency scaling cancels in moment ratios; no absolute model qualification."""
    base, _, _, _ = _rectangle(SCALES[0])
    scaled, _, _, _ = _rectangle(SCALES[1])
    first, second = _hz_moments(base), _hz_moments(scaled)
    a, b = method(first, 2, 0), method(second, 2, 0)
    assert b.damage_rate == pytest.approx(float(SCALES[1]) * a.damage_rate, rel=ROUNDOFF)
    assert b.peak_rate == pytest.approx(float(SCALES[1]) * a.peak_rate, rel=ROUNDOFF)
    assert b.damage_rate / narrow_band_damage(second, 2, 0).damage_rate == pytest.approx(
        a.damage_rate / narrow_band_damage(first, 2, 0).damage_rate, rel=ROUNDOFF,
    )
