"""Independent moment checks for the direct two-parameter default grid.

Source: Offshore Hydromechanics, first edition (2001), section 5.4.3,
equations (5.123)-(5.124); moments and Tz: (5.115)-(5.118).
Public textbook: https://ocw.tudelft.nl/courses/offshore-hydromechanics/

Hand calculation: write S(w) = A*w**(-5)*exp(-B/w**4). Setting the
derivative to zero at wp gives B = (5/4)*wp**4. Integration with
u = B/w**4 gives m_n = A/4 * B**((n-4)/4) * Gamma(1-n/4), n < 4.
Since m0 = Hs**2/16, A = Hs**2*B/4. Thus the exact infinite-domain
moment is Hs**2/16 * B**(n/4) * Gamma(1-n/4), independently of the
implementation. Equation (5.118) then gives Tz = Tp/(5*pi/4)**(1/4).

The manufactured Hs=4 m makes m0=1 m**2. Periods 5, 6 and 10 s are
the defect cases in issue #2613; 20 s also checks a lower peak frequency.
For R=w_max/wp=10, exp(-B/w**4) <= 1 bounds the missing moment fraction
by (5/4)**(1-n/4) / ((1-n/4)*Gamma(1-n/4)*R**(4-n)). For n=2 this
is less than 0.013; the corresponding Tz bias is less than 0.007.
The regression budgets (0.02 for moments, 0.01 for Tz) round these
bounds upward to allow trapezoidal quadrature on the existing default
grid. They are numerical test budgets, not engineering acceptance limits.
See also docs/domains/hydrodynamics/pm-default-grid.md for the derivation.
"""

import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


@pytest.mark.parametrize("tp", [5.0, 6.0, 10.0, 20.0])
@pytest.mark.parametrize("order", [0, 1, 2])
def test_default_pm_moments(tp, order):
    """Compare the unchanged point count with the independent exact moments."""
    generator = WaveSpectra()
    # Manufactured inputs and numerical budgets: module hand calculation above.
    hs = 4.0
    omega, spectrum = generator.pierson_moskowitz(hs=hs, tp=tp)
    b = (5.0 / 4.0) * (2.0 * math.pi / tp) ** 4
    expected = hs**2 / 16.0 * b ** (order / 4.0) * math.gamma(1 - order / 4.0)
    assert generator.spectral_moment(omega, spectrum, order) == pytest.approx(
        expected, rel=0.02
    )


@pytest.mark.parametrize("tp", [5.0, 6.0, 10.0, 20.0])
def test_default_pm_zero_crossing_period(tp):
    """Exact Tz and the numerical budget follow the hand calculation above."""
    generator = WaveSpectra()
    omega, spectrum = generator.pierson_moskowitz(hs=4.0, tp=tp)
    expected_tz = tp / (5.0 * math.pi / 4.0) ** 0.25
    assert generator.zero_crossing_period_from_spectrum(
        omega, spectrum
    ) == pytest.approx(expected_tz, rel=0.01)


def test_explicit_pm_grid_retains_legacy_samples():
    """Manufactured range and count check caller control, with exact linspace."""
    # Explicit endpoint examples from the original API, not physical constants.
    omega, _ = WaveSpectra().pierson_moskowitz(
        hs=4.0, tp=5.0, freq_min=0.02, freq_max=2.0, n_points=100
    )
    np.testing.assert_array_equal(omega, np.linspace(0.02, 2.0, 100))


def test_default_pm_grid_scales_upper_endpoint_with_peak():
    """R=10 follows the tail-bound calculation in the module docstring."""
    omega, _ = WaveSpectra().pierson_moskowitz(hs=4.0, tp=5.0)
    assert omega[-1] == pytest.approx(10.0 * 2.0 * math.pi / 5.0)
    # Preserve the original sample count and lower endpoint, not physical limits.
    assert len(omega) == 100
    assert omega[0] == 0.02


@pytest.mark.parametrize("tp", [0.0, -1.0, math.inf, math.nan])
def test_automatic_pm_grid_rejects_invalid_peak_period(tp):
    """Zero, negative and non-finite periods cannot define a positive peak."""
    with pytest.raises(ValueError, match="tp must be finite and positive"):
        WaveSpectra().pierson_moskowitz(hs=4.0, tp=tp)


@pytest.mark.parametrize("freq_min", [0.0, -1.0, math.inf, math.nan, 20.0])
def test_automatic_pm_grid_rejects_invalid_lower_limit(freq_min):
    """Limits must be finite, positive and below the derived upper endpoint."""
    with pytest.raises(ValueError, match="freq_min"):
        WaveSpectra().pierson_moskowitz(hs=4.0, tp=5.0, freq_min=freq_min)


@pytest.mark.parametrize("tp", [6.0, 10.0])
@pytest.mark.parametrize("rao_stress", [[1.0, 1.0, 1.0], [0.5, 2.0, 1.0]])
def test_pm_fatigue_workflow_retains_legacy_grid_and_damage(tp, rao_stress, tmp_path):
    """Pin the fatigue caller to its former grid, including clamped RAO ends.

    Manufactured flat and shaped RAOs cover the transfer and interpolation.
    The explicit pre-change grid supplies the reference PSD and Dirlik damage;
    the direct spectrum's automatic cutoff must not change this workflow.
    """
    from digitalmodel.fatigue.spectral_fatigue import (
        compute_spectral_moments,
        dirlik_damage,
    )
    from digitalmodel.rao_spectral_fatigue.workflow import (
        _stress_psd_from_rao,
        router,
    )

    rao_freq = [0.01, 0.15, 0.30]
    wave = {"type": "pierson_moskowitz", "Hs": 4.0, "Tp": tp}
    omega, s_omega = WaveSpectra().pierson_moskowitz(
        hs=4.0, tp=tp, freq_min=0.02, freq_max=2.0, n_points=100
    )
    expected_freq = omega / (2.0 * np.pi)
    expected_psd = (
        np.interp(expected_freq, rao_freq, rao_stress) ** 2 * s_omega * 2.0 * np.pi
    )
    actual_freq, actual_psd = _stress_psd_from_rao(rao_freq, rao_stress, wave, "test")
    np.testing.assert_array_equal(actual_freq, expected_freq)
    np.testing.assert_allclose(actual_psd, expected_psd, rtol=1e-13, atol=0.0)

    expected_moments = compute_spectral_moments(expected_freq, expected_psd)
    actual_moments = compute_spectral_moments(actual_freq, actual_psd)
    for name in ("m0", "m1", "m2", "m4"):
        assert getattr(actual_moments, name) == pytest.approx(
            getattr(expected_moments, name), rel=1e-13
        )

    # Omit method to exercise the workflow's default Dirlik damage chain.
    cfg = router(
        {
            "rao_spectral_fatigue": {
                "design_life_years": 20.0,
                "dff": 1.0,
                "sn_curve": {"slope": 3.0, "log_intercept": 12.0},
                "output_dir": str(tmp_path),
                "locations": [
                    {
                        "id": "test",
                        "rao_frequency_Hz": rao_freq,
                        "rao_stress_MPa_per_m": rao_stress,
                        "sea_states": [{"wave_spectrum": wave}],
                    }
                ],
            },
        }
    )
    expected_damage = dirlik_damage(expected_moments, 3.0, 12.0, duration=1.0)
    location = cfg["rao_spectral_fatigue"]["locations"][0]
    assert location["annual_damage"] == pytest.approx(
        expected_damage.damage_per_year, rel=1e-13
    )
