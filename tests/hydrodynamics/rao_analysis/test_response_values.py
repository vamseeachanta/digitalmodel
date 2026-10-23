# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic for the public RAO response contract.

Formula source: Orcina, Vessel theory: RAOs and phases, displacement RAOs:
x = R a cos(omega t - phi). Synthetic inputs below are arithmetic examples,
not empirical coefficients or engineering acceptance limits.
"""

import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.rao_analysis import (
    regular_wave_response,
    spectral_response,
)
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


@pytest.mark.parametrize("rao,expected", [(3, [6, 0, -6]), (0, [0, 0, 0]), (1, [2, 0, -2])])
def test_regular_amplitude(rao, expected):
    """Hand: a=2, omega=1; cos(0, pi/2, pi)=(1,0,-1); multiply by R.

    Absolute tolerance 1e-12 bounds floating point cosine at pi/2; this is a
    numerical arithmetic tolerance, not a design criterion.
    """
    result = regular_wave_response([1], [rao], [2], [0, math.pi / 2, math.pi])
    np.testing.assert_allclose(result, expected, atol=1e-12, rtol=0)


def test_two_frequency_superposition():
    """Hand: 3*2*cos(t)+3*1*cos(2t) gives (9,-3,-3) at 0,pi/2,pi."""
    result = regular_wave_response([1, 2], [3, 3], [2, 1], [0, math.pi / 2, math.pi])
    np.testing.assert_allclose(result, [9, -3, -3], atol=1e-12, rtol=0)


@pytest.mark.parametrize("convention,expected", [("lag", 2), ("lead", -2)])
@pytest.mark.parametrize("unit,phase", [("rad", math.pi / 2), ("deg", 90)])
def test_phase_conventions(convention, expected, unit, phase):
    """Hand at omega*t=pi/2: 2*cos(pi/2 minus/plus pi/2) = +2/-2."""
    result = regular_wave_response(
        [1], [1], [2], [math.pi / 2], rao_phases=[phase],
        phase_unit=unit, phase_convention=convention,
    )
    assert result[0] == pytest.approx(expected)


def test_wave_phase_is_a_crest_phase():
    """Hand: wave cos(t+pi/2), response lag pi/2; at t=0 response=6."""
    result = regular_wave_response(
        [1], [3], [2], [0], wave_phases=[90], rao_phases=[90], phase_unit="deg",
    )
    assert result[0] == pytest.approx(6)


@pytest.mark.parametrize("axis,values", [("omega", [1, 2]), ("frequency", [1/(2*math.pi), 1/math.pi]), ("period", [2*math.pi, math.pi])])
def test_axis_equivalence(axis, values):
    """Definitions: omega=2*pi*f=2*pi/T; periods descend as omega ascends."""
    result = regular_wave_response(values, [3, 3], [2, 1], [0, math.pi / 2], axis=axis)
    np.testing.assert_allclose(result, [9, -3], atol=1e-12, rtol=0)


def test_complex_spectral_response():
    """Hand: |3+4i|^2=25; flat S=2 over [1,3] gives m0=100, 2sqrt=20."""
    result = spectral_response([1, 2, 3], [3+4j]*3, [2]*3)
    np.testing.assert_array_equal(result.spectrum, [50]*3)
    assert result.m0 == 100
    assert result.rms == 10
    assert result.significant_amplitude == 20


def test_zero_spectral_response():
    """Hand: zero RAO gives zero variance and zero significant amplitude."""
    result = spectral_response([1, 2], [0, 0], [2, 2])
    assert result.m0 == result.rms == result.significant_amplitude == 0


def test_pm_tail_and_unity_response():
    r"""Waves in Ocean Engineering (1991), Waves in Ocean Engineering p107, via Orcina wave spectra.

    Unnormalized modified Pierson–Moskowitz: S=(5/16) Hs^2 wp^4 w^-5 exp[-5/4(wp/w)^4].
    Substitution u=(wp/w)^4 gives m0(0,W)=Hs^2/16 exp[-5/4(wp/W)^4].
    Hs=4 -> infinite-band m0=1 and significant single amplitude=2.
    At W=10wp the omitted fraction <0.000125; trapezoid error on this grid
    is below 0.000001, hence a hand-bounded relative tolerance of 0.0002.
    At W=2 rad/s, Tp=5 s, omitted fraction exceeds 0.1 (10 percent).
    This raw spectrum deliberately bypasses finite-grid renormalization.
    """
    wp = 2 * math.pi / 5
    omega = np.linspace(wp / 100, 10 * wp, 40001)
    density = (5/16) * 4**2 * wp**4 * omega**-5 * np.exp(-(5/4)*(wp/omega)**4)
    result = spectral_response(omega, np.ones_like(omega), density)
    assert result.m0 == pytest.approx(1, rel=0.0002)
    assert result.significant_amplitude == pytest.approx(2, rel=0.0002)
    finite_exact = math.exp(-(5/4) * (wp / omega[-1])**4)
    assert result.m0 == pytest.approx(finite_exact, rel=0.000001)
    truncated = omega <= 2
    short = spectral_response(omega[truncated], np.ones(sum(truncated)), density[truncated])
    assert 1 - short.m0 > 0.1


@pytest.mark.parametrize("gamma", [1, 3])
def test_jonswap_unity_response(gamma):
    """Hs=4 -> Hs^2/16=1, significant single amplitude=Hs/2=2.

    Waves in Ocean Engineering (1991) pp107-108 via Orcina wave spectra; gamma is a synthetic
    input. The existing generator normalizes the discrete grid; the separate
    raw Pierson–Moskowitz test checks the tail independently. 1e-12 is round-off tolerance.
    """
    wp = 2 * math.pi / 5
    omega, density = WaveSpectra().jonswap(4, 5, gamma=gamma, freq_min=wp/100, freq_max=10*wp, n_points=40001)
    result = spectral_response(omega, np.ones_like(omega), density)
    assert result.m0 == pytest.approx(1, abs=1e-12)
    assert result.significant_amplitude == pytest.approx(2, abs=1e-12)


@pytest.mark.parametrize("grid", [[2, 1], [1, 1], [1, 3, 2], [0, 1], [1, np.nan]])
def test_regular_rejects_invalid_grid(grid):
    with pytest.raises(ValueError, match="frequency"):
        regular_wave_response(grid, np.ones(len(grid)), np.ones(len(grid)), [0])


@pytest.mark.parametrize("grid", [[2, 1], [1, 1], [1, 3, 2], [0, 1], [1, np.nan], [1]])
def test_spectral_rejects_invalid_grid(grid):
    with pytest.raises(ValueError, match="frequency"):
        spectral_response(grid, np.ones(len(grid)), np.ones(len(grid)))


@pytest.mark.parametrize("rao,waves", [([1], [1, 1]), ([1, 1], [1]), ([[1, 1]], [1, 1])])
def test_mismatched_spectral_arrays(rao, waves):
    with pytest.raises(ValueError):
        spectral_response([1, 2], rao, waves)


@pytest.mark.parametrize("kwargs", [{"rao_amplitudes": [1]}, {"wave_amplitudes": [1]}, {"rao_phases": [0]}, {"wave_phases": [0]}, {"phase_unit": "turn"}, {"phase_convention": "unknown"}, {"axis": "unknown"}])
def test_regular_contract_refusals(kwargs):
    inputs = dict(axis_values=[1, 2], rao_amplitudes=[1, 1], wave_amplitudes=[1, 1], times=[0])
    inputs.update(kwargs)
    with pytest.raises(ValueError):
        regular_wave_response(**inputs)


@pytest.mark.parametrize("rao,waves", [([np.inf, 1], [1, 1]), ([1, 1], [-1, 1]), ([1, 1], [np.nan, 1])])
def test_invalid_spectral_values(rao, waves):
    with pytest.raises(ValueError):
        spectral_response([1, 2], rao, waves)


def test_phase_units_do_not_convert_rotational_amplitudes():
    """Hand: R=3 deg/m, a=2 m, phase=0 -> response=6 deg with either phase unit."""
    for unit in ['rad', 'deg']:
        result = regular_wave_response([1], [3], [2], [0], rao_phases=[0], phase_unit=unit)
        assert result[0] == 6


@pytest.mark.parametrize('inputs', [dict(rao_amplitudes=[-1]), dict(wave_amplitudes=[-1]), dict(times=[np.nan]), dict(wave_amplitudes=[1j]), dict(rao_phases=[np.inf])])
def test_regular_invalid_values(inputs):
    kwargs = dict(axis_values=[1], rao_amplitudes=[1], wave_amplitudes=[1], times=[0])
    kwargs.update(inputs)
    with pytest.raises(ValueError):
        regular_wave_response(**kwargs)


def test_ascending_period_axis_is_refused():
    """Periods increasing from 1 to 2 s imply decreasing angular frequency."""
    with pytest.raises(ValueError, match='frequency'):
        regular_wave_response([1, 2], [1, 1], [1, 1], [0], axis='period')
