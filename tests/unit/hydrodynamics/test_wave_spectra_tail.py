# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent moment checks for the frequency grid and deep-water flux.

Sources: Orcina, OrcaFlex online help (accessed 2026), Waves: Wave spectra,
ISSC and JONSWAP equations; WEC-Sim Theory (accessed 2026), Irregular Waves
and Irregular Wave Power. Links and the tail-error derivation are in
docs/domains/hydrodynamics/wave-spectra.md.

All sea states and fluid properties below are synthetic test inputs. The
relative tolerance of 0.5 percent is a numerical budget: the Pierson-Moskowitz m2 tail above
20*wp is bounded by 2*sqrt(5/4)/(sqrt(pi)*20**2), below 0.32 percent;
the remainder allows quadrature error. It is not a design acceptance limit.
"""

import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.models import WaveParameters, WaveSpectrumType
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


MOMENT_RTOL = 0.005  # Numerical budget derived in this module's docstring.
GENERATORS = ["jonswap", "pierson_moskowitz", "bretschneider", "issc"]


def pm_moment(hs, tp, order):
    """Hand integral: A*w^-5*exp(-B/w^4), u=B/w^4.

    m_n=A/4*B**((n-4)/4)*Gamma(1-n/4), A=5*Hs^2*wp^4/16,
    B=5*wp^4/4. Thus m_n=(Hs^2/16)*wp^n*(5/4)^(n/4)*Gamma(1-n/4).
    This is the Orcina ISSC equation transformed from Hz to rad/s.
    """
    return (hs**2 / 16 * (2 * math.pi / tp)**order
            * (5 / 4)**(order / 4) * math.gamma(1 - order / 4))


def enhanced_moments(hs, tp, gamma):
    """Independent Simpson hand recipe, using reciprocal frequency y=wp/w.

    The Orcina JONSWAP equation becomes y^(3-n)*exp(-5*y^4/4)*G(y)dy,
    G=gamma^exp(-(1/y-1)^2/(2*sigma^2)); sigma=0.07 above y=1,
    else 0.09. Integrate y=0..4 with 20000 even panels; omitted low
    frequencies are suppressed by exp(-320). Normalize using the n=0
    integral and Hs^2/16. These are quadrature choices, not sea-state data.
    No production spectral or moment routine supplies the expected values.
    """
    panels = 20000
    step = 4 / panels
    totals = {}
    for order in (-1, 0, 1, 2):
        terms = []
        for i in range(1, panels + 1):
            y = i * step
            sigma = 0.07 if y >= 1 else 0.09
            enhancement = gamma**math.exp(-(1 / y - 1)**2 / (2 * sigma**2))
            value = y**(3 - order) * math.exp(-5 * y**4 / 4) * enhancement
            weight = 1 if i == panels else (4 if i % 2 else 2)
            terms.append(weight * value)
        totals[order] = math.fsum(terms) * step / 3
    return {n: hs**2 / 16 * (2 * math.pi / tp)**n * value / totals[0]
            for n, value in totals.items()}


@pytest.mark.parametrize("tp", [5.0, 6.0, 10.0, 20.0])
@pytest.mark.parametrize("method", ["pierson_moskowitz", "bretschneider", "jonswap"])
def test_default_moments_match_infinite_pm(tp, method):
    """Gamma=1 reduces JONSWAP exactly to the analytic Pierson-Moskowitz hand integral."""
    ws = WaveSpectra()
    options = {"gamma": 1.0} if method == "jonswap" else {}
    omega, density = getattr(ws, method)(hs=4.0, tp=tp, **options)
    for order in (-1, 0, 1, 2):
        assert ws.spectral_moment(omega, density, order) == pytest.approx(
            pm_moment(4, tp, order), rel=MOMENT_RTOL)
    expected_tz = 2 * math.pi * math.sqrt(pm_moment(4, tp, 0) / pm_moment(4, tp, 2))
    assert ws.zero_crossing_period_from_spectrum(omega, density) == pytest.approx(
        expected_tz, rel=MOMENT_RTOL)


@pytest.mark.parametrize("gamma", [3.3, 7.0])
@pytest.mark.parametrize("tp", [5.0, 6.0, 10.0])
def test_enhanced_jonswap_moments_periods_and_power(tp, gamma):
    """Use the independent reciprocal-frequency integral and flux algebra."""
    expected = enhanced_moments(4, tp, gamma)
    ws = WaveSpectra()
    omega, density = ws.jonswap(4, tp, gamma)
    for order, value in expected.items():
        assert ws.spectral_moment(omega, density, order) == pytest.approx(value, rel=MOMENT_RTOL)
    stats = ws.spectrum_statistics(omega, density)
    assert stats["m_minus1"] == pytest.approx(expected[-1], rel=MOMENT_RTOL)
    assert stats["Tz_s"] == pytest.approx(
        2 * math.pi * math.sqrt(expected[0] / expected[2]), rel=MOMENT_RTOL)
    assert stats["Te_s"] == pytest.approx(2 * math.pi * expected[-1] / expected[0], rel=MOMENT_RTOL)
    # Synthetic fluid inputs; WEC-Sim flux with deep-water cg=g/(2*w).
    assert ws.deep_water_wave_power(omega, density, water_density=1000, gravity=10) == pytest.approx(
        1000 * 10**2 / 2 * expected[-1], rel=MOMENT_RTOL)


@pytest.mark.parametrize("method", GENERATORS)
def test_dispatch_grid_and_explicit_bounds(method):
    """A synthetic grid: dispatch and direct calls must use the same samples."""
    ws = WaveSpectra()
    params = WaveParameters(WaveSpectrumType(method), 4, 6)
    omega, density = ws.generate_spectrum(params)
    np.testing.assert_array_equal(omega, params.frequency_array())
    np.testing.assert_array_equal(omega, getattr(ws, method)(4, 6)[0])
    assert omega[-1] == pytest.approx(20 * params.peak_frequency)
    explicit, _ = getattr(ws, method)(4, 6, freq_min=0.1, freq_max=2, n_points=37)
    np.testing.assert_array_equal(explicit, np.linspace(0.1, 2, 37))
    assert np.all(np.isfinite(density))


@pytest.mark.parametrize("method", GENERATORS)
@pytest.mark.parametrize("options", [
    {"tp": 0}, {"tp": float("nan")}, {"hs": -1}, {"hs": float("inf")},
    {"freq_min": 0}, {"freq_max": float("inf")}, {"freq_max": 0.01},
    {"n_points": 1}, {"n_points": 2.5},
])
def test_invalid_generator_inputs(method, options):
    values = {"hs": 4, "tp": 6, **options}
    with pytest.raises(ValueError):
        getattr(WaveSpectra(), method)(**values)


@pytest.mark.parametrize("gamma", [0, -1, float("nan"), float("inf")])
def test_invalid_peakedness(gamma):
    with pytest.raises(ValueError):
        WaveSpectra().jonswap(4, 6, gamma=gamma)


def test_pm_energy_period_and_power():
    """Te=2*pi*m(-1)/m0; P=rho*g^2*m(-1)/2, from cited flux equation."""
    ws = WaveSpectra()
    omega, density = ws.pierson_moskowitz(4, 6)
    expected = pm_moment(4, 6, -1)
    assert ws.energy_period_from_spectrum(omega, density) == pytest.approx(
        2 * math.pi * expected, rel=MOMENT_RTOL)  # Hs=4 => m0=1.
    assert ws.deep_water_wave_power(omega, density, water_density=1000, gravity=10) == pytest.approx(
        50000 * expected, rel=MOMENT_RTOL)  # 1000*10^2/2.
    zero = np.zeros_like(density)
    assert ws.deep_water_wave_power(omega, zero, water_density=1000, gravity=10) == 0
    assert math.isnan(ws.energy_period_from_spectrum(omega, zero))


@pytest.mark.parametrize("frequency_dtype", [np.int32, np.int64, np.uint64])
def test_statistics_accept_integer_frequencies(frequency_dtype):
    """Integer grids retain the statistics supported by floating grids."""
    ws = WaveSpectra()
    omega = np.array([1, 2, 3], dtype=frequency_dtype)
    density = np.array([1.0, 2.0, 1.0])
    stats = ws.spectrum_statistics(omega, density)
    assert stats == pytest.approx(ws.spectrum_statistics(omega.astype(float), density))
    # Unit-spaced trapezoids: m(-1)=(1+1)/2+(1+1/3)/2=5/3;
    # m0=(1+2)/2+(2+1)/2=3; m1=(1+4)/2+(4+3)/2=6;
    # m2=(1+8)/2+(8+9)/2=13; m4=(1+32)/2+(32+81)/2=73.
    assert stats["m_minus1"] == pytest.approx(1.6666666666666667)
    assert stats["m0"] == pytest.approx(3.0)
    assert stats["m1"] == pytest.approx(6.0)
    assert stats["m2"] == pytest.approx(13.0)
    assert stats["m4"] == pytest.approx(73.0)
    np.testing.assert_array_equal(omega, [1, 2, 3])
    assert omega.dtype == np.dtype(frequency_dtype)


@pytest.mark.parametrize("frequency_dtype", [np.int32, np.int64, np.uint64])
def test_energy_period_accepts_integer_frequencies(frequency_dtype):
    """Energy periods agree for integer and floating frequency grids."""
    ws = WaveSpectra()
    omega = np.array([1, 2, 3], dtype=frequency_dtype)
    density = np.array([1.0, 2.0, 1.0])
    period = ws.energy_period_from_spectrum(omega, density)
    assert period == pytest.approx(ws.energy_period_from_spectrum(omega.astype(float), density))
    # The hand-integrated moments above give Te=2*pi*(5/3)/3=10*pi/9 s.
    assert period == pytest.approx(3.490658503988659)


@pytest.mark.parametrize("frequency_dtype", [np.int32, np.int64, np.uint64])
def test_power_accepts_integer_frequencies(frequency_dtype):
    """Incident power agrees for integer and floating frequency grids."""
    ws = WaveSpectra()
    omega = np.array([1, 2, 3], dtype=frequency_dtype)
    density = np.array([1.0, 2.0, 1.0])
    fluid = {"water_density": 1000, "gravity": 10}
    power = ws.deep_water_wave_power(omega, density, **fluid)
    assert power == pytest.approx(ws.deep_water_wave_power(omega.astype(float), density, **fluid))
    # Synthetic fluid inputs give P=1000*10^2*(5/3)/2=250000/3 W/m.
    assert power == pytest.approx(83333.33333333333)


@pytest.mark.parametrize("fluid", [{"water_density": 0, "gravity": 10},
                                    {"water_density": 1000, "gravity": float("nan")}])
def test_invalid_power_properties(fluid):
    ws = WaveSpectra()
    omega, density = ws.pierson_moskowitz(4, 6)
    with pytest.raises(ValueError):
        ws.deep_water_wave_power(omega, density, **fluid)
