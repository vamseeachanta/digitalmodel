# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Energy and response regressions for the two spectra in issue #2543.

Basis: SIMA public documentation, SIMO / Modelling / Environment / Waves /
JONSWAP Spectra, sections 1 and 2 (accessed 2026-10-22):
https://simasite.azurewebsites.net/docs/latest/simo/modelling/environment/waves/Jonswap.html

The 1% energy tolerance and gamma values come from issue #2543. Heights,
periods, gains and grid sizes below are synthetic numerical test inputs.
"""

import math
from pathlib import Path
import runpy

import numpy as np
import pytest

from digitalmodel.marine_ops.installation.crane_tip_motion import (
    _jonswap_spectrum,
    crane_tip_significant_motion,
    crane_tip_significant_velocity,
)
from digitalmodel.subsea.pipeline.free_span.wave_velocity import (
    compute_wave_velocity,
    jonswap_spectrum,
)

_trapezoid = getattr(np, "trapezoid", None) or np.trapz


@pytest.fixture(params=["crane", "free_span"])
def spectrum(request):
    """Adapt the peak-frequency and peak-period APIs to the same inputs."""
    if request.param == "crane":
        return lambda omega, hs, tp, gamma: _jonswap_spectrum(
            omega, hs, 2 * np.pi / tp, gamma
        )
    return jonswap_spectrum


@pytest.mark.parametrize("gamma", [1.0, 3.3, 7.0])
@pytest.mark.parametrize("hs", [2.0, 4.0])
@pytest.mark.parametrize("tp", [6.0, 12.0])
def test_spectrum_integrates_to_wave_variance(spectrum, gamma, hs, tp):
    """Hs=4*sqrt(m0) gives m0=Hs²/16, independent of Tp and gamma.

    Independent calculation: for Hs=2 m, m0=4/16=0.25 m²;
    for Hs=4 m, m0=16/16=1 m². This is the acceptance relation in
    issue #2543. With x=omega/wp the unenhanced tail above x=100 is
    1-exp(-1.25/100**4), below 0.000002% (SIMA section 1 shape).
    The logarithmic grid resolves the peak without a Tp-dependent cutoff.
    """
    wp = 2 * np.pi / tp
    omega = wp * np.geomspace(0.01, 100.0, 20001)
    density = spectrum(omega, hs, tp, gamma)
    assert np.all(np.isfinite(density))
    assert np.all(density >= 0)
    # Issue #2543: relative error in m0, rather than recovered Hs, within 1%.
    assert _trapezoid(density, omega) == pytest.approx(hs**2 / 16, rel=0.01)


def test_unenhanced_density_matches_analytical_shape(spectrum):
    """SIMA sections 1-2: gamma=1 removes enhancement.

    With x=omega/wp, integrating 5*x^-5*exp(-5/(4*x^4)) gives 1.
    Thus S=(Hs²/(16*wp))*5*x^-5*exp(-5/(4*x^4)).
    The comparison budget is 1%/10000, below the issue's energy tolerance.
    """
    hs, tp = 2.0, 8.0  # Synthetic inputs; m0=0.25 m² by hand.
    wp = 2 * np.pi / tp
    x = np.array([0.5, 1.0, 2.0])  # Below, at and above the peak.
    expected = hs**2 / (16 * wp) * 5 * x**-5 * np.exp(-5 / (4 * x**4))
    assert spectrum(x * wp, hs, tp, 1.0) == pytest.approx(expected, rel=0.01 / 10000)


@pytest.mark.parametrize("gamma", [3.3, 7.0])
def test_peak_density_matches_independent_quadrature(spectrum, gamma):
    """Independent scalar Simpson calculation from SIMA section 1.

    Write q(x)=5*x^-5*exp(-5/(4*x^4)), a=exp(-(x-1)²/(2*sigma²)).
    The unenhanced integral is exactly 1, so I=1+integral(q*(gamma**a-1)).
    Integrate the excess on [0.5,2] using composite Simpson's rule with
    6000 intervals; sigma is 0.07 below x=1 and 0.09 above it.
    Outside that interval, |gamma**a-1| is below 2e-11 for gamma<=7.
    At x=1, S=Hs²/(16*wp)*5*exp(-5/4)*gamma/I.
    This scalar integration does not use the production normalising factor.
    The comparison budget is 1%/10000 (issue #2543).
    """
    intervals = 6000  # Independent Simpson mesh, with x=1 at a node.
    step = (2.0 - 0.5) / intervals
    terms = []
    for index in range(intervals + 1):
        x = 0.5 + index * step
        sigma = 0.07 if x <= 1.0 else 0.09  # SIMA section 1.
        a = math.exp(-((x - 1) ** 2) / (2 * sigma**2))
        excess = 5 * x**-5 * math.exp(-5 / (4 * x**4)) * math.expm1(math.log(gamma) * a)
        weight = 1 if index in (0, intervals) else (4 if index % 2 else 2)
        terms.append(weight * excess)
    integral = 1 + step / 3 * math.fsum(terms)
    hs, tp = 2.0, 8.0  # Synthetic inputs.
    wp = 2 * math.pi / tp
    expected = hs**2 / (16 * wp) * 5 * math.exp(-5 / 4) * gamma / integral
    actual = spectrum(np.array([wp]), hs, tp, gamma)[0]
    assert actual == pytest.approx(expected, rel=0.01 / 10000)


def test_truncated_grid_does_not_redistribute_missing_energy(spectrum):
    """S is a density: its value at a frequency cannot depend on other bins."""
    hs, tp, gamma = 2.0, 8.0, 7.0  # Synthetic sea state; issue #2543 gamma.
    omega = (2 * np.pi / tp) * np.geomspace(0.01, 100.0, 20001)
    full = spectrum(omega, hs, tp, gamma)
    retained = (omega >= 2 * np.pi / tp) & (omega <= 4 * np.pi / tp)
    partial = spectrum(omega[retained], hs, tp, gamma)
    np.testing.assert_array_equal(partial, full[retained])
    assert _trapezoid(partial, omega[retained]) < hs**2 / 16


@pytest.mark.parametrize("gamma", [1.0, 3.3, 7.0])
def test_zero_height_has_no_energy(spectrum, gamma):
    """Hs²/16=0 implies S=0; zero frequency retains the existing zero limit."""
    omega = np.array([0.0, 0.5, 1.0, 2.0])  # Synthetic frequencies.
    np.testing.assert_array_equal(spectrum(omega, 0.0, 8.0, gamma), 0.0)


@pytest.mark.parametrize("gamma", [1.0, 3.3, 7.0])
@pytest.mark.parametrize("gain", [1.0, 2.0])
def test_constant_rao_motion_matches_hand_calculation(gamma, gain):
    """m0_response=gain²*Hs²/16, so 2*sqrt(m0_response)=gain*Hs/2.

    Uses the existing significant single-amplitude convention in
    crane_tip_significant_motion, not a double-amplitude interpretation.
    Hs=2 m gives 1 m for unit gain and 2 m for gain=2.
    The 1% comparison budget is from issue #2543.
    """
    hs, tp = 2.0, 8.0  # Synthetic hand-calculation inputs.
    omega = (2 * np.pi / tp) * np.geomspace(0.01, 100.0, 20001)
    result = crane_tip_significant_motion(
        np.full_like(omega, gain), hs, tp, omega, gamma=gamma
    )
    assert result == pytest.approx(gain * hs / 2, rel=0.01)


@pytest.mark.parametrize("gamma", [1.0, 3.3, 7.0])
def test_velocity_with_inverse_frequency_rao_matches_hand_calculation(gamma):
    """Synthetic RAO=1/omega gives omega²*RAO²=1 and v_sig=Hs/2.

    For Hs=2 m the numerical velocity is 1 m/s. This isolates spectral
    energy scaling through the velocity API. Tolerance: issue #2543, 1%.
    """
    hs, tp = 2.0, 8.0
    omega = (2 * np.pi / tp) * np.geomspace(0.01, 100.0, 20001)
    result = crane_tip_significant_velocity(1 / omega, hs, tp, omega, gamma=gamma)
    assert result == pytest.approx(hs / 2, rel=0.01)


@pytest.mark.parametrize("tp", [6.0, 12.0])
def test_free_span_automatic_gamma_preserves_energy(tp):
    """SIMA section 3 outer branches give gamma=5 or gamma=1.

    Synthetic Hs=4: Tp=6<=3.6*2 gives gamma=5; Tp=12>=5*2 gives
    gamma=1. For either case m0=4²/16=1 m².
    The assertion tolerance is the 1% requirement from issue #2543.
    """
    hs = 4.0
    omega = (2 * np.pi / tp) * np.geomspace(0.01, 100.0, 20001)
    assert _trapezoid(jonswap_spectrum(omega, hs, tp), omega) == pytest.approx(
        1.0, rel=0.01
    )


@pytest.mark.parametrize("gamma", [1.0, 3.3, 7.0])
def test_free_span_velocity_scaling(gamma):
    """Linear transfer: doubling Hs multiplies both moments by 4.

    Hand calculation: Uw=2*sqrt(M0) scales by sqrt(4)=2;
    Tu=2*pi*sqrt(M0/M2) scales by sqrt(4/4)=1. These are the
    equations documented in wave_velocity.py. All dimensions here are
    synthetic inputs; the ratio comparison budget is 1%/10000 from #2543.
    """
    small = compute_wave_velocity(2.0, 8.0, 20.0, 1.0, gamma=gamma)
    large = compute_wave_velocity(4.0, 8.0, 20.0, 1.0, gamma=gamma)
    assert large.Uw_ms / small.Uw_ms == pytest.approx(2.0, rel=0.01 / 10000)
    assert large.Tu_s / small.Tu_s == pytest.approx(1.0, rel=0.01 / 10000)
    assert large.M0 / small.M0 == pytest.approx(4.0, rel=0.01 / 10000)
    assert large.M2 / small.M2 == pytest.approx(4.0, rel=0.01 / 10000)


def test_worked_example_matches_hand_calculation():
    """Synthetic Hs=4: wave m0=16/16=1 and unit-RAO motion=4/2=2.

    The variance tolerance is 1% from issue #2543, as in the main regression.
    """
    example = (
        Path(__file__).resolve().parents[1]
        / "examples/demos/installation/jonswap_normalisation.py"
    )
    rows = runpy.run_path(str(example))["run_example"]()
    assert [row["gamma"] for row in rows] == [1.0, 3.3, 7.0]
    for row in rows:
        assert row["crane_variance_m2"] == pytest.approx(1.0, rel=0.01)
        assert row["free_span_variance_m2"] == pytest.approx(1.0, rel=0.01)
        assert row["motion_m"] == pytest.approx(2.0, rel=0.01)
