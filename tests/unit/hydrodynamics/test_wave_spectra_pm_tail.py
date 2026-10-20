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


def _legacy_pm_stress_reference(tp, rao_stress):
    """Scalar spectrum shape, unit variance, and piecewise-linear manufactured RAO.

    S(w) is proportional to w**(-5)*exp(-B/w**4), B=(5/4)*(2*pi/Tp)**4.
    Normalize by the explicit trapezoidal sum (Hs=4 gives variance one).
    Convert with f=w/(2*pi), S_f=2*pi*S_w; multiply by the squared RAO.
    No production spectrum, interpolation, or moment routine supplies this PSD.
    """
    omega = [0.02 + 0.02 * j for j in range(100)]
    b = (5.0 / 4.0) * (2.0 * math.pi / tp) ** 4
    shape = [w**-5 * math.exp(-b / w**4) for w in omega]
    area = math.fsum(
        (left + right) * (hi - lo) / 2.0
        for lo, hi, left, right in zip(omega, omega[1:], shape, shape[1:])
    )
    freq = [w / (2.0 * math.pi) for w in omega]
    psd = []
    for f, density in zip(freq, shape):
        if f <= 0.01:
            gain = rao_stress[0]
        elif f >= 0.30:
            gain = rao_stress[2]
        elif f <= 0.15:
            gain = rao_stress[0] + (rao_stress[1] - rao_stress[0]) * (f - 0.01) / 0.14
        else:
            gain = rao_stress[1] + (rao_stress[2] - rao_stress[1]) * (f - 0.15) / 0.15
        psd.append(gain**2 * density / area * 2.0 * math.pi)
    return freq, psd


# Frozen scalar-sum moments and numerical PDF-integral damage references.
# The derivation, manufactured S-N curve, and integration are documented in
# pm-default-grid.md under "Independent fatigue compatibility references".
# These constants were evaluated without importing application modules.
@pytest.mark.parametrize(
    "tp,rao_stress,expected_moments,expected_damage",
    [
        (
            6.0,
            [1.0, 1.0, 1.0],
            (
                1.0,
                0.19513540901113186,
                0.040191277393263704,
                0.0019914222000040316,
            ),
            0.00018786016016534025,
        ),
        (
            6.0,
            [0.5, 2.0, 1.0],
            (
                2.8383511789461116,
                0.5184435223468276,
                0.09894429399190821,
                0.004138906877876767,
            ),
            0.0011075912420625768,
        ),
        (
            10.0,
            [1.0, 1.0, 1.0],
            (
                1.0,
                0.12595528597438582,
                0.01757178754139701,
                0.00048628228522241116,
            ),
            1.8318609056159307e-05,
        ),
        (
            10.0,
            [0.5, 2.0, 1.0],
            (
                2.5961426296263834,
                0.33901211749593946,
                0.04775802962098414,
                0.0012145922367087425,
            ),
            0.00016976163174373126,
        ),
    ],
)
def test_pm_fatigue_workflow_retains_legacy_grid_and_damage(
    tp, rao_stress, expected_moments, expected_damage, tmp_path,
):
    """Compare PSD, moments, and default damage with independent references."""
    from digitalmodel.fatigue.spectral_fatigue import compute_spectral_moments
    from digitalmodel.rao_spectral_fatigue.workflow import (
        _stress_psd_from_rao,
        router,
    )

    rao_freq = [0.01, 0.15, 0.30]
    wave = {"type": "pierson_moskowitz", "Hs": 4.0, "Tp": tp}
    expected_freq, expected_psd = _legacy_pm_stress_reference(tp, rao_stress)
    actual_freq, actual_psd = _stress_psd_from_rao(rao_freq, rao_stress, wave, "test")
    np.testing.assert_array_equal(actual_freq, expected_freq)
    np.testing.assert_allclose(actual_psd, expected_psd, rtol=5e-13, atol=0.0)

    actual_moments = compute_spectral_moments(actual_freq, actual_psd)
    for name, expected in zip(("m0", "m1", "m2", "m4"), expected_moments):
        assert getattr(actual_moments, name) == pytest.approx(expected, rel=1e-13)

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
    location = cfg["rao_spectral_fatigue"]["locations"][0]
    assert location["annual_damage"] == pytest.approx(expected_damage, rel=1e-12)
