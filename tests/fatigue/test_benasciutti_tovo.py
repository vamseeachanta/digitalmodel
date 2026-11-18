# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""BT2 qualification against Metals 11 (2021), 1333, Eqs. (11), (13).

Source: https://doi.org/10.3390/met11091333, Table 1 (band-limited PSDs).
Numerical tolerances below concern arithmetic or published rounding only;
none is an engineering acceptance factor.
"""

import math

import pytest

from digitalmodel.fatigue.spectral_fatigue import (
    SpectralMoments,
    benasciutti_tovo_damage,
    narrow_band_damage,
)


# Table 1: rectangular PSD with upper/lower frequency ratio c=5.
# Integrating a unit-area rectangle on [1,5] gives Mj=(5**(j+1)-1)/(4*(j+1)).
# Rescale frequency by sqrt(3/31): m0=m2=1, m1=3*sqrt(3/31),
# m4=(781/5)*(3/31)**2. Units here are angular frequency and unit stress.
def rectangular_moments():
    return SpectralMoments(
        m0=1.0, m1=3 * math.sqrt(3 / 31), m2=1.0,
        m4=(781 / 5) * (3 / 31) ** 2,
    )


@pytest.mark.parametrize(
    "slope, published_ratio, independent_dirlik",
    [
        (3, 0.980, 4.3274607691640465),
        (5, 0.920, 84.04310693166342),
        (7, 0.867, 2331.7809483219658),
    ],
)
def test_published_broadband_benchmark(slope, published_ratio, independent_dirlik):
    """Reproduce Table 1, c=5, D_TB/D_DK for b=3,5,7.

    Independent hand calculation: alpha1=3/sqrt(31/3),
    alpha2=(31/3)/sqrt(781/5), xm=alpha1*alpha2.
    Eq. (8): d1=2*(xm-alpha2**2)/(1+alpha2**2),
    R=(alpha2-xm-d1**2)/(1-alpha2-d1+d1**2),
    d2=(1-alpha2-d1+d1**2)/(1-R), d3=1-d1-d2,
    Q=1.25*(alpha2-d3-d2*R)/d1.
    Eq. (9), converted from amplitude to range, gives
    D_DK=2**b/(2*pi*alpha2) * [d1*Q**b*Gamma(1+b)
    + 2**(b/2)*Gamma(1+b/2)*(d2*abs(R)**b+d3)].
    Substitution gives the three independent_dirlik constants above, for
    m0=m2=A=1. No production Dirlik calculation supplies the denominator.
    The table prints three decimals: half a last-place unit is 0.0005.
    """
    result = benasciutti_tovo_damage(rectangular_moments(), slope, 0.0)
    assert abs(result.damage_rate / independent_dirlik - published_ratio) <= 0.0005


def test_hand_calculated_correction_and_equivalent_range():
    """Independent substitution into Eqs. (11), (13), c=5 and b=3.

    alpha1=0.9332565252573827, alpha2=0.8267989650858009;
    w=0.6388789872844973, lambda=w+(1-w)*alpha2**2
    =0.8857400580055089. Rayleigh Eq. (5) in range convention gives
    D_NB=(2*sqrt(2))**3*Gamma(2.5)/(2*pi)=6*sqrt(2/pi).
    Hence D_BT=4.240309903003382 per second for A=1.
    Section 2.1, converting the peak rate from rad/s to cycles/s:
    nu_p=sqrt(m4/m2)/(2*pi)=sqrt(7029/4805)/(2*pi).
    Eq. (4): S_eq=(D_BT/nu_p)**(1/3) for A=1.
    Relative tolerance 1e-12 retains twelve decimal digits of this calculation.
    """
    moments = rectangular_moments()
    result = benasciutti_tovo_damage(moments, 3.0, 0.0)
    assert result.damage_rate == pytest.approx(4.240309903003382, rel=1e-12)
    nb = narrow_band_damage(moments, 3.0, 0.0)
    assert result.damage_rate / nb.damage_rate == pytest.approx(
        0.8857400580055089, rel=1e-12,
    )
    # Output rounds ranges to three decimals; half that resolution is 0.0005.
    analytical_peak_rate = math.sqrt(7029 / 4805) / (2 * math.pi)
    expected_range = (4.240309903003382 / analytical_peak_rate) ** (1 / 3)
    assert abs(result.equivalent_stress_range - expected_range) <= 0.0005
    assert result.damage_per_year / result.damage_rate == pytest.approx(
        nb.damage_per_year / nb.damage_rate, rel=1e-12,
    )


@pytest.mark.parametrize("slope", [1, 2, 3, 5, 7])  # Eq. (11): b >= 1.
@pytest.mark.parametrize("variance, angular_frequency", [
    (1, 1), (0.03, 3), (2.3, 7.1), (0.01, 14.9), (0.01, 0.3),
])
def test_exact_narrowband_limit(slope, variance, angular_frequency):
    """A spectral line has m_j=variance*omega**j; Eq. (11) gives lambda=1."""
    moments = SpectralMoments(**{
        f"m{j}": variance * angular_frequency**j for j in (0, 1, 2, 4)
    })
    actual = benasciutti_tovo_damage(moments, slope, 0)
    expected = narrow_band_damage(moments, slope, 0)
    assert actual.damage_rate == expected.damage_rate
    assert actual.equivalent_stress_range == expected.equivalent_stress_range


@pytest.mark.parametrize("offset", [-2, 2])
def test_unity_normalization_roundoff_allowed(offset):
    """Two roots and two divisions allow gamma_4, approximately two ulps.

    Accuracy and Stability of Numerical Algorithms, 2nd ed. (2002),
    Sections 2.2 and 3.1, Lemma 3.1: gamma_n=n*u/(1-n*u).
    For binary64, u=2**-53, so gamma_4 is just above 2**-51.
    These perturbations are arithmetic allowances, not design margins.
    """
    moments = SpectralMoments(m0=1, m1=1 + offset * math.ulp(1.0), m2=1, m4=1)
    actual = benasciutti_tovo_damage(moments, 3, 0)
    expected = narrow_band_damage(moments, 3, 0)
    assert actual.damage_rate == expected.damage_rate


def test_near_unity_ratios_share_narrowband_limit():
    """Ratios just below unity have the same rounding budget as those above."""
    moments = SpectralMoments(
        m0=1, m1=1 - math.ulp(1.0), m2=1 - math.ulp(1.0), m4=1,
    )
    actual = benasciutti_tovo_damage(moments, 3, 0)
    expected = narrow_band_damage(moments, 3, 0)
    assert actual.damage_rate == expected.damage_rate


@pytest.mark.parametrize("width", [0.1, 0.01, 0.001, 0.0001])
def test_narrowband_convergence(width):
    """Uniform [1-h,1+h]: m1=1,m2=1+h²/3,m4=1+2h²+h⁴/5.

    Integrated monomials give the moments. Eq. (11), b=3, bounds the
    correction by alpha2² and 1, so |lambda-1| <= 1-alpha2².
    No empirical closeness threshold is required.
    """
    moments = SpectralMoments(
        m0=1, m1=1, m2=1 + width**2 / 3,
        m4=1 + 2 * width**2 + width**4 / 5,
    )
    nb = narrow_band_damage(moments, 3, 0)
    ratio = benasciutti_tovo_damage(moments, 3, 0).damage_rate / nb.damage_rate
    assert moments.irregularity_factor**2 <= ratio <= 1


@pytest.mark.parametrize("slope", [1, 2, 3, 5, 7])
@pytest.mark.parametrize("upper", [1.5, 5, 20])  # Table 1 band-limited PSDs.
def test_correction_bounds(slope, upper):
    """Eq. (11): alpha2**(b-1) <= lambda <= 1 for b >= 1.

    Unit-area PSD on [1,c] has m_j=(c**(j+1)-1)/((c-1)*(j+1)).
    """
    moments = SpectralMoments(**{
        f"m{j}": (upper ** (j + 1) - 1) / ((upper - 1) * (j + 1))
        for j in (0, 1, 2, 4)
    })
    result = benasciutti_tovo_damage(moments, slope, 0)
    nb = narrow_band_damage(moments, slope, 0)
    ratio = result.damage_rate / nb.damage_rate
    assert math.isfinite(result.damage_rate) and result.damage_rate >= 0
    assert moments.irregularity_factor ** (slope - 1) <= ratio <= 1


@pytest.mark.parametrize("field", ["m0", "m1", "m2", "m4"])
@pytest.mark.parametrize("invalid", [0, -1, math.nan, math.inf])
def test_invalid_moments_rejected(field, invalid):
    """Moment ratios in Eq. (2) need finite positive nonzero moments."""
    data = rectangular_moments().model_dump()
    data[field] = invalid
    with pytest.raises(ValueError, match="moments"):
        benasciutti_tovo_damage(SpectralMoments(**data), 3, 0)


@pytest.mark.parametrize("data", [
    dict(m0=1, m1=2, m2=1, m4=2),  # alpha1 > 1
    dict(m0=1, m1=1, m2=2, m4=1),  # alpha2 > 1
    dict(m0=1, m1=0.1, m2=1, m4=2),  # alpha1 < alpha2
    dict(m0=1, m1=1 - 5 * math.ulp(1.0), m2=1, m4=1),  # ordering beyond roundoff
    dict(m0=1, m1=1 + 3 * math.ulp(1.0), m2=1, m4=1),  # upper bound beyond roundoff
    dict(m0=1, m1=1e-17, m2=1, m4=1e32),  # small ratios still require ordering
])
def test_inconsistent_moment_ratios_rejected(data):
    """Nonnegative-frequency PSD moments require 0 < alpha2 <= alpha1 <= 1."""
    with pytest.raises(ValueError, match="moments"):
        benasciutti_tovo_damage(SpectralMoments(**data), 3, 0)


@pytest.mark.parametrize("slope", [0, 0.5, math.nan, math.inf])
def test_slope_outside_correction_domain_rejected(slope):
    """b >= 1 is the algebraic domain of the reduction bound, Eq. (11)."""
    with pytest.raises(ValueError, match="sn_slope"):
        benasciutti_tovo_damage(rectangular_moments(), slope, 0)


@pytest.mark.parametrize("intercept", [math.nan, math.inf, -math.inf])
def test_nonfinite_intercept_rejected(intercept):
    with pytest.raises(ValueError, match="sn_intercept"):
        benasciutti_tovo_damage(rectangular_moments(), 3, intercept)


def test_independent_narrowband_anchor():
    """Eq. (5), unit variance, unit angular frequency, A=1 and b=3.

    Gamma(5/2)=3*sqrt(pi)/4, hence D_NB=6*sqrt(2/pi).
    Relative tolerance 1e-12 checks twelve decimal digits of arithmetic.
    """
    moments = SpectralMoments(m0=1, m1=1, m2=1, m4=1)
    assert narrow_band_damage(moments, 3, 0).damage_rate == pytest.approx(
        6 * math.sqrt(2 / math.pi), rel=1e-12,
    )
