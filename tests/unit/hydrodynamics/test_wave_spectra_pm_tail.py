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


def _independent_pm_stress_psd(tp, rao_stress):
    """Manufactured Hs=4 reference on w_i=0.02+0.02*i; no production calls.

    S(w) is proportional to w^-5 exp[-1.25*(wp/w)^4], normalized by
    a scalar trapezoidal sum to unit variance. Piecewise-linear H(f),
    clamped at the endpoints, gives S_stress(f)=2*pi*H(f)^2*S(w).
    """
    omega = [0.02 + 0.02 * i for i in range(100)]
    wp = 2.0 * math.pi / tp
    raw = [w**-5 * math.exp(-1.25 * (wp / w)**4) for w in omega]
    area = math.fsum((a + b) * 0.01 for a, b in zip(raw, raw[1:]))
    frequencies = [w / (2.0 * math.pi) for w in omega]
    psd = []
    for f, density in zip(frequencies, raw):
        if f <= 0.01:
            gain = rao_stress[0]
        elif f <= 0.15:
            gain = rao_stress[0] + (rao_stress[1] - rao_stress[0]) * (f - 0.01) / 0.14
        elif f <= 0.30:
            gain = rao_stress[1] + (rao_stress[2] - rao_stress[1]) * (f - 0.15) / 0.15
        else:
            gain = rao_stress[2]
        psd.append(2.0 * math.pi * gain**2 * density / area)
    return frequencies, psd


@pytest.mark.parametrize(
    "tp,rao_stress,expected_moments,expected_damage",
    [
        (6.0, [1.0, 1.0, 1.0],
         (1.0, 0.19513540901113183, 0.0401912773932637, 0.0019914222000040316),
         0.0001878601601653398),
        (6.0, [0.5, 2.0, 1.0],
         (2.838351178946111, 0.5184435223468274, 0.0989442939919082,
          0.004138906877876766),
         0.0011075912420625772),
        (10.0, [1.0, 1.0, 1.0],
         (1.0, 0.12595528597438585, 0.017571787541397012, 0.0004862822852224112),
         1.8318609056159303e-05),
        (10.0, [0.5, 2.0, 1.0],
         (2.596142629626383, 0.33901211749593946, 0.04775802962098414,
          0.0012145922367087427),
         0.0001697616317437313),
    ],
    ids=["short-flat", "short-shaped", "long-flat", "long-shaped"],
)
def test_pm_fatigue_workflow_retains_legacy_grid_and_damage(
    tp, rao_stress, expected_moments, expected_damage, tmp_path
):
    """Pin the fatigue caller to its former grid, including clamped RAO ends.

    Manufactured flat and shaped RAOs cover the transfer and interpolation.
    References use scalar finite-grid sums and independent quadrature of the
    exponential/Rayleigh mixture's third moment, with slope 3, intercept 12
    and 31557600 seconds/year. Equations and legacy conventions are documented
    in pm-default-grid.md; these values pin compatibility, not model validity.
    """
    from digitalmodel.fatigue.spectral_fatigue import (
        compute_spectral_moments,
    )
    from digitalmodel.rao_spectral_fatigue.workflow import (
        _stress_psd_from_rao,
        router,
    )

    rao_freq = [0.01, 0.15, 0.30]
    wave = {"type": "pierson_moskowitz", "Hs": 4.0, "Tp": tp}
    expected_freq, expected_psd = _independent_pm_stress_psd(tp, rao_stress)
    actual_freq, actual_psd = _stress_psd_from_rao(rao_freq, rao_stress, wave, "test")
    np.testing.assert_allclose(actual_freq, expected_freq, rtol=1e-14, atol=0.0)
    np.testing.assert_allclose(actual_psd, expected_psd, rtol=1e-13, atol=0.0)

    actual_moments = compute_spectral_moments(actual_freq, actual_psd)
    for name, expected in zip(("m0", "m1", "m2", "m4"), expected_moments):
        assert getattr(actual_moments, name) == pytest.approx(
            expected, rel=1e-12, abs=0.0
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
    location = cfg["rao_spectral_fatigue"]["locations"][0]
    assert location["annual_damage"] == pytest.approx(
        expected_damage, rel=1e-12, abs=0.0
    )
