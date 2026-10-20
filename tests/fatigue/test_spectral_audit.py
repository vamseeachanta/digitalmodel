"""Numerical regression anchors for audit section 1 and 2."""
import math

import numpy as np
import pytest

from digitalmodel.fatigue import spectral_fatigue as sf


def test_hz_line_rate_and_annual_damage():
    moments = sf.SpectralMoments(m0=100, m1=10, m2=1, m4=0.01)
    assert moments.nu_0 == pytest.approx(0.1, rel=1e-12)
    assert moments.nu_p == pytest.approx(0.1, rel=1e-12)
    # Eq. (5), Metals 11(9), 1333 (2021), converted amplitude -> range.
    # 0.1 * (2*sqrt(2)*10)^3 * Gamma(2.5) / 10^12.164 * 31557600.
    assert sf.narrow_band_damage(moments, 3, 12.164).damage_per_year == pytest.approx(
        0.06506915205879445, rel=1e-6)


def test_dirlik_audit_moment_anchor_and_normalization():
    # alpha2=0.6, xm=0.57. Eq. (8),(9), Metals 11(9), 1333 (2021).
    moments = sf.SpectralMoments(m0=1, m1=0.57 / math.sqrt(0.6), m2=0.6, m4=1)
    result = sf.dirlik_damage(moments, 3, 0)
    assert result.damage_rate / moments.nu_p / 8 == pytest.approx(2.208, rel=0.001)
    d1, d2, d3, _, r = sf.dirlik_coefficients(moments)
    assert d1 + d2 + d3 == pytest.approx(1, abs=1e-12)
    assert min(d1, d2, d3) >= 0
    assert r < 0  # This valid mixture must retain the negative R.


@pytest.mark.parametrize("width", [0, 1e-5, 0.001])
def test_dirlik_narrow_band_limit(width):
    moments = sf.SpectralMoments(m0=100, m1=10, m2=1 + width, m4=0.01 + width / 10)
    assert sf.dirlik_damage(moments, 3, 12.164).damage_rate == pytest.approx(
        sf.narrow_band_damage(moments, 3, 12.164).damage_rate, rel=0.02)


def test_invalid_dirlik_weights_raise():
    with pytest.raises(ValueError, match="Dirlik"):
        sf.dirlik_damage(sf.SpectralMoments(m0=1, m1=0.1, m2=0.6, m4=1), 3, 12)


def test_published_single_mode_example_within_one_percent():
    # estimate_fatigue_psd.pdf, Revision A (2014), Appendix C, Table C-1,
    # Figure C-2; Table 1 welded steel; Table 2 Dirlik damage = 0.00377.
    # https://www.vibrationdata.com/tutorials2/estimate_fatigue_psd.pdf
    # First axial mode: modal mass rho*A*L/2 and strain phi'(0)=omega_n/c
    # give stress/force = 2*c*omega_n/(A*L*dynamic_stiffness).
    # Consistent stress units here are ksi; Table 1 A is amplitude-based.
    frequency = np.linspace(10, 2500, 100001)
    omega = 2 * np.pi * frequency
    speed, length, area, damping = 1.96e5, 84, 0.785, 0.05
    natural = np.pi * speed / (2 * length)
    transfer = 2 * speed * natural / (area * length * (
        natural**2 - omega**2 + 2j * damping * natural * omega))
    psd = 1000 * abs(transfer)**2 / 1e6  # lbf^2/Hz -> ksi^2/Hz
    moments = sf.compute_spectral_moments(frequency, psd)
    assert moments.rms == pytest.approx(4.91, rel=0.001)
    # Eq. (A-11): range intercept is 2^m times amplitude intercept.
    result = sf.dirlik_damage(moments, 3.5, math.log10(2**3.5 * 1.26e11))
    assert result.damage_rate * 600 == pytest.approx(0.00377, rel=0.01)
