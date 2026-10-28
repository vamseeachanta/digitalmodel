"""Independent spectral checks for #2746.

Source: Metals (2021) 11, 1333, doi:10.3390/met11091333,
Equations (1), (5), (8), (9), and Table 1. S-N parameters below are
synthetic test inputs, not qualified material properties.
"""

import math

import pytest

from digitalmodel.fatigue.spectral_fatigue import (
    SpectralMoments, _dirlik_coefficients, dirlik_damage, narrow_band_damage,
)


def test_line_spectrum_has_cyclic_frequency_rate():
    """Variance 100 MPa² at .1 Hz gives moments [100,10,1,.01].

    Eq. (5), expressed in ranges: E[r³]=(2 sqrt(200))³ Gamma(2.5).
    Synthetic m=3, log10(A)=12; D/year=.1 E[r³]/A * 31557600.
    Julian year conversion = 365.25 d * 24 h/d * 3600 s/h.
    """
    moments = SpectralMoments(m0=100, m1=10, m2=1, m4=0.01)
    assert moments.nu_0 == pytest.approx(0.1)
    assert moments.nu_p == pytest.approx(0.1)
    expected = 0.1 * (2 * math.sqrt(200))**3 * math.gamma(2.5) / 10**12
    result = narrow_band_damage(moments, 3, 12)
    assert result.damage_rate == pytest.approx(expected, rel=1e-12)
    assert result.damage_per_year == pytest.approx(expected * 31557600, rel=1e-12)
    assert dirlik_damage(moments, 3, 12).damage_rate == pytest.approx(expected, rel=0.02)


def test_published_bimodal_parameters():
    """Table 1: alpha1=.9, alpha2=.6, peak rate=20/s (bimodal row).

    Choose variance=1; then m2=144, m4=57600, m1=10.8.
    Eq. (8): xm=.54, D1=9/34, R=-291/5935, D2=1408969/7197256,
    D3=1-D1-D2, Q=45/136. Integrate Eq. (7) independently:
    E[Z³]=6 D1 Q³ + 3 sqrt(2 pi)/2 * (D2 abs(R)³+D3).
    Eq. (9), converted from amplitudes to ranges, D/s=20*8*E[Z³]/A.
    """
    moments = SpectralMoments(m0=1, m1=10.8, m2=144, m4=57600)
    d1, d2, d3, q, r = _dirlik_coefficients(moments)
    assert d1 + d2 + d3 == pytest.approx(1, abs=1e-12)
    a, b, c, scale, rayleigh = 9/34, 1408969/7197256, 1-9/34-1408969/7197256, 45/136, -291/5935
    expected_z3 = 6*a*scale**3 + 3*math.sqrt(2*math.pi)/2*(b*abs(rayleigh)**3+c)
    assert (d1, d2, d3, q, r) == pytest.approx((a, b, c, scale, rayleigh))
    assert dirlik_damage(moments, 3, 12).damage_rate == pytest.approx(
        20*8*expected_z3/10**12, rel=0.01)


def test_invalid_dirlik_weights_raise():
    """Inconsistent synthetic moments give xm < gamma² and negative D1."""
    with pytest.raises(ValueError, match="Dirlik"):
        dirlik_damage(SpectralMoments(m0=1, m1=0.1, m2=1, m4=2), 3, 12)
