"""wave_forecast tests (digitalmodel #1357)."""

import numpy as np
import pytest

from digitalmodel.motion_forecast import (
    AnalyticRAO,
    reconstruct_motion,
)
from digitalmodel.motion_forecast.models import GRAVITY, WaveComponent, WaveForecast
from digitalmodel.motion_forecast.wave_forecast import (
    SyntheticWaveField,
    WaveFieldSource,
    coherence_horizon,
    directional_spread,
    dpz_horizon,
    surface_elevation,
    synthesize_directional_forecast,
)
from digitalmodel.motion_forecast.wave_source import synthesize_forecast


# ---- horizons ----

def test_coherence_horizon_narrowband_longer_than_broadband():
    narrow = synthesize_directional_forecast(2.5, 10.0, gamma=7.0, n_dir=1)
    broad = synthesize_directional_forecast(2.5, 10.0, gamma=1.0, n_dir=1)
    # wide clamp so we compare the raw coherence time, not the rails
    hn = coherence_horizon(narrow.components, tau_min=1.0, tau_max=1e4)
    hb = coherence_horizon(broad.components, tau_min=1.0, tau_max=1e4)
    assert hn > hb


def test_coherence_horizon_monochromatic_is_tau_max():
    one = [WaveComponent(0.7, 1.0, 0.0)]
    assert coherence_horizon(one, tau_max=120.0) == 120.0


def test_coherence_horizon_clamped_to_realistic_band():
    h = coherence_horizon(
        synthesize_directional_forecast(2.5, 10.0, gamma=1.0, n_dir=1).components)
    assert 15.0 <= h <= 120.0   # never the vendor 4-5 min


def test_dpz_horizon_matches_formula():
    comps = [WaveComponent(0.5, 1.0, 0.0), WaveComponent(1.0, 1.0, 0.0)]
    L = 200.0
    tau = dpz_horizon(comps, aperture=L)
    assert tau == pytest.approx(2 * L * (1.0 - 0.5) / GRAVITY, abs=1e-9)


# ---- directional ----

def test_directional_spread_sums_to_one():
    h = np.linspace(-90.0, 90.0, 7)
    w = directional_spread(h, mean_heading=0.0, s=10.0)
    assert w.sum() == pytest.approx(1.0)
    assert np.argmax(w) == 3   # peak at the mean


def test_directional_forecast_preserves_variance_and_mean_heading():
    fc = synthesize_directional_forecast(3.0, 10.0, mean_heading=25.0,
                                         spread_s=15.0, n_freq=40, n_dir=9)
    m0 = sum(0.5 * c.amplitude**2 for c in fc.components)
    assert 4.0 * np.sqrt(m0) == pytest.approx(3.0, rel=0.02)   # Hs preserved
    e = np.array([0.5 * c.amplitude**2 for c in fc.components])
    hdg = np.array([c.heading for c in fc.components])
    assert np.average(hdg, weights=e) == pytest.approx(25.0, abs=1e-6)


def test_n_dir_one_matches_long_crested_amplitudes():
    fc = synthesize_directional_forecast(2.5, 9.0, mean_heading=20.0,
                                         n_freq=32, n_dir=1)
    ref = synthesize_forecast(2.5, 9.0, heading=20.0, n_components=32)
    a1 = np.sort([c.amplitude for c in fc.components])
    a2 = np.sort([c.amplitude for c in ref.components])
    assert np.allclose(a1, a2)
    assert all(c.heading == 20.0 for c in fc.components)


# ---- surface ----

def test_surface_elevation_single_component_closed_form():
    w0, a0, phi0, hdg = 0.7, 1.3, 0.4, 30.0
    fc = WaveForecast([WaveComponent(w0, a0, phi0, hdg)], horizon=60.0)
    t = np.linspace(0, 20, 101)
    x, y = 40.0, 0.0
    k = w0 * w0 / GRAVITY
    proj = np.cos(np.deg2rad(hdg)) * x + np.sin(np.deg2rad(hdg)) * y
    expected = a0 * np.cos(w0 * t + phi0 - k * proj)
    assert np.allclose(surface_elevation(fc, x, y, t), expected, atol=1e-9)


def test_surface_at_reference_is_sum_of_component_cosines():
    fc = synthesize_directional_forecast(2.0, 9.0, n_freq=16, n_dir=3)
    t = np.linspace(0, 10, 51)
    expected = sum(c.amplitude * np.cos(c.omega * t + c.phase) for c in fc.components)
    assert np.allclose(surface_elevation(fc, 0.0, 0.0, t), expected, atol=1e-9)


# ---- ingest seam ----

def test_synthetic_wave_field_flows_through_reconstruct():
    src = SyntheticWaveField(3.0, 10.0, n_dir=5, spread_s=12.0)
    assert isinstance(src, WaveFieldSource)
    fc = src.read()
    assert isinstance(fc, WaveForecast) and fc.horizon > 0
    rao = AnalyticRAO({"heave": lambda w, b: 1.0 + 0j})
    mf = reconstruct_motion(fc, rao, dt=0.5)
    assert mf.dof["heave"].shape == mf.t.shape
