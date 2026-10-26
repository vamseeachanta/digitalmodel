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


def test_zero_aperture_raises_not_silent_fallback():
    # aperture=0.0 must fail loudly (not falsy-fall-back to coherence_horizon)
    with pytest.raises(ValueError, match="aperture"):
        synthesize_directional_forecast(2.5, 9.0, n_dir=1, aperture=0.0)


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


@pytest.mark.parametrize("vector_times", [False, True], ids=["scalar", "vector"])
def test_finite_depth_surface_phase_reference(vector_times):
    # Basis for both finite-depth cases: USACE Coastal Engineering Manual,
    # EM 1110-2-1100 (2002), Part II, sections II-1-2.c(2) and c(3),
    # wave celerity, length and period; the sinusoidal wave profile.
    # Independent hand calculation, with chosen synthetic inputs (SI units):
    # k=0.2, h=3 give kh=0.6, so omega=sqrt(g*k*tanh(k*h)), not sqrt(g*k).
    # At heading 90 degrees, d_hat=(0, 1). From reference (3, -4), choose
    # delta_x=10 and delta_y=pi/(2*k). Then k*d_hat.delta_p=pi/2.
    # With phi=pi/2, theta=omega*t: at successive quarter-periods,
    # cos(theta)=(1, 0, -1, 0, 1). Reversing the spatial phase sign instead
    # gives theta=omega*t+pi and reverses the extrema.
    k, depth, amplitude = 0.2, 3.0, 1.3
    omega = np.sqrt(GRAVITY * k * np.tanh(k * depth))
    period = 2.0 * np.pi / omega
    reference = (3.0, -4.0)
    x, y = reference[0] + 10.0, reference[1] + np.pi / (2.0 * k)
    fc = WaveForecast(
        [WaveComponent(omega, amplitude, np.pi / 2.0, 90.0)],
        horizon=period,
        phase_reference_location=reference,
        water_depth=depth,
    )
    times = period * np.array([0.0, 0.25, 0.5, 0.75, 1.0])
    expected = [amplitude, 0.0, -amplitude, 0.0, amplitude]
    # The plan's 1e-9 m absolute tolerance allows dispersion-solver roundoff;
    # no relative tolerance is needed for these exact extrema and zero crossings.
    if vector_times:
        actual = surface_elevation(fc, x, y, times)
        assert actual.shape == times.shape
        np.testing.assert_allclose(actual, expected, rtol=0.0, atol=1e-9)
    else:
        for time, elevation in zip(times, expected):
            actual = surface_elevation(fc, x, y, float(time))
            assert np.ndim(actual) == 0
            assert actual == pytest.approx(elevation, rel=0.0, abs=1e-9)


def test_surface_translation_preserves_phase():
    # Independent coordinate identity: (p+s)-(p_ref+s)=p-p_ref.
    # Chosen synthetic inputs: reference (3, -4), observation (11, 7),
    # translation (-6, 9) m, and oblique heading 30 degrees exercise both axes.
    # Reuse the explicit finite-depth dispersion construction k=0.2, h=3 m;
    # amplitude=1.3 m and phase=pi/3 are arbitrary, nonzero fixture values.
    k, depth = 0.2, 3.0
    omega = np.sqrt(GRAVITY * k * np.tanh(k * depth))
    period = 2.0 * np.pi / omega
    components = [WaveComponent(omega, 1.3, np.pi / 3.0, 30.0)]
    reference, observation, shift = (3.0, -4.0), (11.0, 7.0), (-6.0, 9.0)
    fc = WaveForecast(
        components, horizon=period,
        phase_reference_location=reference, water_depth=depth,
    )
    shifted = WaveForecast(
        components, horizon=period,
        phase_reference_location=tuple(a + b for a, b in zip(reference, shift)),
        water_depth=depth,
    )
    x, y = observation
    sx, sy = shift
    # Quarter-period samples span a full cycle; use the plan's 1e-9 m tolerance.
    times = period * np.array([0.0, 0.25, 0.5, 0.75, 1.0])
    # Hand calculation: delta=(8,11), projected onto (sqrt(3)/2,1/2), gives
    # alpha=pi/3-(4*sqrt(3)+11/2)/5. At quarter-period samples the elevations
    # are 1.3*(cos(alpha),-sin(alpha),-cos(alpha),sin(alpha),cos(alpha)).
    # These literals are independent of the evaluator and its wavenumber helper.
    expected = [
        0.171557303637, 1.288630316099, -0.171557303637,
        -1.288630316099, 0.171557303637,
    ]
    original = surface_elevation(fc, x, y, times)
    translated = surface_elevation(shifted, x + sx, y + sy, times)
    np.testing.assert_allclose(original, expected, rtol=0.0, atol=1e-9)
    np.testing.assert_allclose(translated, expected, rtol=0.0, atol=1e-9)
    np.testing.assert_allclose(
        translated, original,
        rtol=0.0, atol=1e-9,
    )


# ---- ingest seam ----

def test_synthetic_wave_field_flows_through_reconstruct():
    src = SyntheticWaveField(3.0, 10.0, n_dir=5, spread_s=12.0)
    assert isinstance(src, WaveFieldSource)
    fc = src.read()
    assert isinstance(fc, WaveForecast) and fc.horizon > 0
    rao = AnalyticRAO({"heave": lambda w, b: 1.0 + 0j})
    mf = reconstruct_motion(fc, rao, dt=0.5)
    assert mf.dof["heave"].shape == mf.t.shape
