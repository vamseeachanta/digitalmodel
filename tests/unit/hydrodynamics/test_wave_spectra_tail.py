"""Independent spectral-tail references and legacy response guards for #2613."""

import math
from functools import lru_cache

import numpy as np
import pytest
from click.testing import CliRunner
from scipy.integrate import quad

from digitalmodel.hydrodynamics.models import RAOData, WaveParameters, WaveSpectrumType
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra

PERIODS = [5.0, 6.0, 10.0, 15.0]
FAMILIES = ["pierson_moskowitz", "jonswap"]
GENERATORS = FAMILIES + ["bretschneider", "issc"]
TRAPZ = getattr(np, "trapezoid", None) or np.trapz


def _shape(omega, tp, gamma):
    wp = 2 * math.pi / tp
    sigma = np.where(omega <= wp, 0.07, 0.09)
    return omega**-5 * np.exp(-1.25 * (wp / omega)**4) * gamma ** (
        np.exp(-(omega - wp)**2 / (2 * sigma**2 * wp**2)))


@lru_cache(maxsize=None)
def _reference(tp, family):
    wp = 2 * math.pi / tp
    if family == "pierson_moskowitz":
        b = 1.25 * wp**4
        moments = {n: math.gamma(1 - n / 4) * b**(n / 4) for n in (-1, 0, 1, 2)}
        return moments, 1 / (4 * b)
    bounds = [1e-3, wp, 3 * wp, 30 * wp, math.inf]
    raw = {n: sum(quad(lambda w: w**n * _shape(w, tp, 3.3), a, b,
                       epsabs=1e-11, epsrel=1e-11)[0]
                  for a, b in zip(bounds[:-1], bounds[1:])) for n in (-1, 0, 1, 2)}
    return {n: value / raw[0] for n, value in raw.items()}, raw[0]


def _check_moments(tp, family):
    omega, density = getattr(WaveSpectra(), family)(hs=4.0, tp=tp)
    reference, _ = _reference(tp, family)
    for n, tolerance in [(0, 1e-9), (1, 0.005), (2, 0.01), (-1, 0.005)]:
        assert TRAPZ(omega**n * density, omega) == pytest.approx(reference[n], rel=tolerance)


@pytest.mark.parametrize("tp", PERIODS)
def test_default_grid_moments_pm(tp):
    _check_moments(tp, "pierson_moskowitz")


@pytest.mark.parametrize("tp", PERIODS)
def test_default_grid_moments_jonswap(tp):
    _check_moments(tp, "jonswap")


@pytest.mark.parametrize("family", FAMILIES)
@pytest.mark.parametrize("tp", PERIODS)
def test_default_grid_ordinates(tp, family):
    omega, density = getattr(WaveSpectra(), family)(hs=4.0, tp=tp)
    _, integral = _reference(tp, family)
    mask = (omega >= 0.5 * 2 * math.pi / tp) & (omega <= 3 * 2 * math.pi / tp)
    expected = _shape(omega[mask], tp, 3.3 if family == "jonswap" else 1.0) / integral
    np.testing.assert_allclose(density[mask], expected, rtol=0.005, atol=0)


@pytest.mark.parametrize("family", FAMILIES)
@pytest.mark.parametrize("tp", PERIODS)
def test_default_grid_periods(tp, family):
    ws = WaveSpectra()
    omega, density = getattr(ws, family)(hs=4.0, tp=tp)
    reference, _ = _reference(tp, family)
    m0, m1 = (TRAPZ(omega**n * density, omega) for n in (0, 1))
    assert ws.zero_crossing_period_from_spectrum(omega, density) == pytest.approx(
        2 * math.pi * math.sqrt(reference[0] / reference[2]), rel=0.005)
    assert 2 * math.pi * m0 / m1 == pytest.approx(
        2 * math.pi * reference[0] / reference[1], rel=0.005)
    assert ws.energy_period_from_spectrum(omega, density) == pytest.approx(
        2 * math.pi * reference[-1] / reference[0], rel=0.005)


def test_wave_power_hand_value():
    ws = WaveSpectra()
    omega, density = ws.pierson_moskowitz(hs=4.0, tp=10.0)
    assert ws.energy_period_from_spectrum(omega, density) == pytest.approx(8.5722, rel=0.005)
    assert ws.wave_power(omega, density) / 1000 == pytest.approx(67.29, rel=0.005)
    assert ws.wave_power(omega, density, rho=1000) == pytest.approx(
        ws.wave_power(omega, density) * 1000 / 1025, rel=1e-12)


@pytest.mark.parametrize("family", GENERATORS)
def test_explicit_legacy_grid_unchanged(family):
    omega = np.linspace(0.02, 2.0, 100)
    hs, tp, g = 4.0, 10.0, 9.81
    wp = 2 * np.pi / tp
    if family == "issc":
        f = omega / (2 * np.pi)
        raw = 0.11 * hs**2 * (0.772 * tp)**(-4) * f**(-5) * np.exp(
            -0.44 * (0.772 * tp * f)**(-4)) / (2 * np.pi)
    elif family == "bretschneider":
        raw = (5.0 / 16.0 * hs**2 * wp**4) / omega**5 * np.exp(-5.0 / 4.0 * (wp / omega)**4)
    else:
        alpha = 5.0 / 16.0 * hs**2 * wp**4 / g**2
        raw = alpha * g**2 / omega**5 * np.exp(-1.25 * (wp / omega)**4)
        if family == "jonswap":
            sigma = np.where(omega <= wp, 0.07, 0.09)
            raw *= 3.3 ** np.exp(-(omega - wp)**2 / (2 * sigma**2 * wp**2))
    expected = raw * (hs**2 / 16.0 / float(TRAPZ(raw, omega)))
    actual_omega, actual = getattr(WaveSpectra(), family)(hs=hs, tp=tp, freq_max=2.0)
    assert np.array_equal(actual_omega, omega)
    assert np.array_equal(actual, expected)


@pytest.mark.parametrize("family", GENERATORS)
@pytest.mark.parametrize("kwargs, count", [({}, 200), ({"n_points": 300}, 300)])
def test_default_grid_resolution(family, kwargs, count):
    omega, _ = getattr(WaveSpectra(), family)(hs=4.0, tp=6.0, **kwargs)
    assert omega[-1] == 15 * 2 * math.pi / 6.0
    assert len(omega) == count


def test_wave_parameters_resolved_attributes():
    params = WaveParameters(WaveSpectrumType.JONSWAP, 4.0, 6.0)
    assert params.freq_max == 15 * 2 * math.pi / 6.0
    assert params.n_frequencies == 200
    assert params.frequency_array()[-1] == params.freq_max
    assert params.to_dict()["frequency_range"] == {
        "min_rad_s": 0.02, "max_rad_s": params.freq_max, "n_points": 200}
    assert WaveParameters(WaveSpectrumType.JONSWAP, 4.0, 6.0, freq_max=2.0).n_frequencies == 100
    explicit_count = WaveParameters(WaveSpectrumType.JONSWAP, 4.0, 6.0, n_frequencies=50)
    assert explicit_count.freq_max == params.freq_max
    assert len(explicit_count.frequency_array()) == 50
    for lower in (params.freq_max, params.freq_max + 1):
        with pytest.raises(ValueError, match="freq_min"):
            WaveParameters(WaveSpectrumType.JONSWAP, 4.0, 6.0, freq_min=lower)


def test_cli_spectrum_default_grid(monkeypatch):
    from digitalmodel.hydrodynamics.cli import cli

    seen = []
    original = WaveSpectra.generate_spectrum
    def capture(self, params):
        seen.append(params)
        return original(self, params)
    monkeypatch.setattr(WaveSpectra, "generate_spectrum", capture)
    result = CliRunner().invoke(cli, ["spectrum", "--hs", "4", "--tp", "5"])
    assert result.exit_code == 0, result.output
    assert seen[0].freq_max == 15 * 2 * math.pi / 5
    assert seen[0].n_frequencies == 200


def test_spectrum_statistics_keys():
    ws = WaveSpectra()
    omega, density = ws.pierson_moskowitz(hs=4.0, tp=10.0)
    stats = ws.spectrum_statistics(omega, density)
    assert {"m0", "m1", "m2", "m4", "Hs_m", "Tz_s", "Tp_s", "omega_p_rad_s",
            "spectral_width", "m_minus1", "Te_s"} <= stats.keys()
    assert stats["m_minus1"] == pytest.approx(TRAPZ(density / omega, omega))
    assert stats["Te_s"] == pytest.approx(ws.energy_period_from_spectrum(omega, density))


def test_parametric_fatigue_legacy_grid_golden():
    from digitalmodel.parametric.generate import _spectral_fatigue_annual_damage

    damage = _spectral_fatigue_annual_damage(
        {"Hs": 3.5, "Tp": 10.0}, gamma=3.3, stress_gain_MPa_per_m=18.0,
        sn_slope=3.0, sn_intercept=12.164)
    assert damage == pytest.approx(0.0467936825806, rel=1e-9)


def test_rao_spectral_fatigue_legacy_grid_golden():
    from digitalmodel.rao_spectral_fatigue.workflow import _stress_psd_from_rao
    from digitalmodel.fatigue.spectral_fatigue import compute_spectral_moments, dirlik_damage

    freq, psd = _stress_psd_from_rao(
        [0, 0.05, 0.1, 0.2, 0.3], [5, 20, 10, 2, 0],
        {"type": "jonswap", "Hs": 3.5, "Tp": 6.0, "gamma": 3.3}, "synthetic")
    damage = dirlik_damage(compute_spectral_moments(np.array(freq), np.array(psd)),
                           sn_slope=3.0, sn_intercept=12.164, duration=1.0)
    assert damage.damage_per_year == pytest.approx(0.0222508383231, rel=1e-9)


def test_catalog_legacy_grid_golden(monkeypatch):
    from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog, SeaStateDefinition

    amplitudes = np.full((20, 3, 6), 0.5)
    amplitudes[:, :, 2] = 1.0
    rao = RAOData(frequencies=np.linspace(0.1, 2.0, 20), directions=np.array([0, 90, 180]),
                  amplitudes=amplitudes, phases=np.zeros_like(amplitudes), vessel_name="synthetic")
    catalog = HullCatalog()
    monkeypatch.setattr(catalog, "get_hull", lambda _: None)
    sea = SeaStateDefinition(significant_height=3.0, peak_period=6.0)
    motion = catalog.compute_motions("synthetic", sea, rao)
    acceleration = catalog.compute_accelerations("synthetic", sea, rao, (100, 0, 10), (50, 0, 5))
    assert motion.significant_values["heave_sig"] == pytest.approx(1.5, rel=1e-9)
    assert acceleration["vertical"] == pytest.approx(56.2937938597, rel=1e-9)
