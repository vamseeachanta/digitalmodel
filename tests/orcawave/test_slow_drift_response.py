"""Independent closed-form references for offline slow-drift surge."""

import numpy as np
import pytest

from digitalmodel.orcawave.drift_forces import (
    MeanDriftCoefficients,
    QTFMatrix,
    full_qtf_force_spectrum,
    full_qtf_slowly_varying,
    low_frequency_surge_response,
    naess_expected_extreme,
    newman_approximation,
    surge_response_kernel,
)


def make_qtf(omega, values):
    values = np.asarray(values, dtype=complex)
    return QTFMatrix(
        dof="surge", heading_deg=0., frequencies_rad_s=list(omega),
        real_part=values.real.tolist(), imag_part=values.imag.tolist(),
    )


def test_resonant_bichromatic_response_and_separate_mean():
    # A 10 kN sinusoid at resonance: amplitude F/(C*mu)=0.5 m.
    # mass=10^7 kg=10^4 kN s²/m, K=100 kN/m, mu=0.1 rad/s.
    mu = np.array([0., .1, .2])
    force_psd = np.array([0., 500., 0.])  # sum(S)*dmu=10²/2 kN²
    result = low_frequency_surge_response(
        force_psd, mu, mass_kg=1e7, stiffness_kn_m=100.,
        damping_kn_s_m=200., mean_force_kn=-125.,
    )
    assert result.std_m == pytest.approx(.5 / np.sqrt(2), rel=1e-12)
    assert result.mean_offset_m == pytest.approx(-1.25)
    np.testing.assert_allclose(result.spectrum_m2_s_rad, [0., 1.25, 0.])


@pytest.mark.parametrize("ratio,frequency_hz", [(.13, .0093), (.08, .0095), (.04, .0099)])
def test_published_table_one_parameters_against_resonant_closed_form(ratio, frequency_hz):
    """Lande-Sudall & Stansby (2026), AOR 176, 105292, Table 1.

    Published damping/frequency pairs are inputs to a synthetic oscillator,
    not reproduction of the paper's decay tests or aerodynamic model.
    """
    natural = 2*np.pi*frequency_hz
    stiffness = 100.7  # kN/m, paper section 2
    mass_kg = 1000 * stiffness / natural**2  # inferred for this scalar check
    result = low_frequency_surge_response(
        [0., 1./natural], [0., natural], mass_kg=mass_kg,
        stiffness_kn_m=stiffness, damping_ratio=ratio,
    )
    # Unit force variance at resonance; |H|=1/(2*zeta*K).
    assert result.std_m == pytest.approx(1/(2*ratio*stiffness), rel=1e-12)


def test_complex_pair_force_spectrum_and_quadratic_kernel_moments():
    omega = np.array([.7, .8])
    spectrum = np.array([5., 1.25])  # wave amplitudes 1 m, 0.5 m
    qtf = make_qtf(omega, [[10., 3.+4j], [3.-4j, 20.]])
    force_psd = full_qtf_force_spectrum(qtf, spectrum, omega)
    # Beat amplitude = 2*1*0.5*|3+4j| = 5 kN.
    np.testing.assert_allclose(force_psd, [0., 125.], rtol=1e-12)
    kernel = surge_response_kernel(
        qtf, spectrum, omega, mass_kg=1e7, stiffness_kn_m=100.,
        damping_kn_s_m=200.,
    )
    # At resonance |H|=1/20 m/kN; response amplitude = .25 m.
    assert kernel.mean_offset_m == pytest.approx(.15)
    assert kernel.variance_m2 == pytest.approx(.25**2 / 2)
    # Kac–Siegert quadratures: X-mu = sum(lambda*(U²+V²)).
    np.testing.assert_allclose(kernel.eigenvalues_m, [-.0625, .0625])
    assert 2 * kernel.eigenvalues_m.sum() == pytest.approx(0., abs=1e-15)
    np.testing.assert_allclose(kernel.matrix_m, kernel.matrix_m.conj().T)


def test_newman_and_full_spectra_agree_in_every_beat_bin():
    omega = np.linspace(.2, 1.5, 131)
    diagonal = 10 * omega**2
    spectrum = np.exp(-((omega - .65) / .15)**2)
    qtf = make_qtf(omega, (diagonal[:, None] + diagonal[None, :]) / 2)
    coeffs = MeanDriftCoefficients(
        heading_deg=0., frequencies_rad_s=omega.tolist(),
        surge_kn_m2=diagonal.tolist(), sway_kn_m2=diagonal.tolist(),
        yaw_knm_m2=diagonal.tolist(),
    )
    full = full_qtf_force_spectrum(qtf, spectrum, omega)
    np.testing.assert_allclose(full, newman_approximation(coeffs, spectrum, omega))
    assert full.sum() * .01 == pytest.approx(
        full_qtf_slowly_varying(qtf, spectrum, omega)**2, rel=1e-12,
    )
    kernel = surge_response_kernel(
        qtf, spectrum, omega, mass_kg=1e7, stiffness_kn_m=100., damping_ratio=.05,
    )
    response = low_frequency_surge_response(
        full, np.arange(len(omega)) * .01, mass_kg=1e7,
        stiffness_kn_m=100., damping_ratio=.05,
    )
    assert kernel.variance_m2 == pytest.approx(response.std_m**2, rel=1e-12)


def test_naess_closed_form_signed_tails_and_mean():
    # Eq. 21 with Q2=10 and lambda1=.25 m; independently evaluated in Decimal.
    rate = np.exp(5) / 10800
    eigenvalues = np.array([-.5, .25, .25])
    upper = naess_expected_extreme(eigenvalues, rate, 10800, mean_offset_m=3.)
    lower = naess_expected_extreme(
        eigenvalues, rate, 10800, mean_offset_m=3., tail="lower",
    )
    expected_amplitude = 3.272988295588410
    assert upper == pytest.approx(3. + expected_amplitude, rel=1e-13)
    assert lower == pytest.approx(3. - 2 * expected_amplitude, rel=1e-13)


def test_naess_zero_response_and_duration_monotonicity():
    assert naess_expected_extreme([0., 0.], .01, 10800, mean_offset_m=-2.) == -2.
    short = naess_expected_extreme([-.5, .5], .01, 10800)
    long = naess_expected_extreme([-.5, .5], .01, 21600)
    assert long > short
    # Twice the amplitude is a height, never an automatic factor in this API.
    assert naess_expected_extreme([-1., 1.], .01, 10800) == pytest.approx(2 * short)


def test_kernel_retains_receptance_phase_and_is_phase_invariant():
    omega = np.array([.5, .6, .7])
    spectrum = np.array([1., 2., 3.])
    values = np.array([[4., 3.+2j, 1.-4j], [3.-2j, 5., -2.+1j],
                       [1.+4j, -2.-1j, 6.]])
    kwargs = dict(mass_kg=1e7, stiffness_kn_m=100., damping_kn_s_m=200.)
    result = surge_response_kernel(make_qtf(omega, values), spectrum, omega, **kwargs)
    # delta_01=-.1; H=+i/20 at resonance, B01=.1*sqrt(2)*(3+2i)*i/20.
    assert result.matrix_m[0, 1] == pytest.approx(
        .1 * np.sqrt(2) * (-2.+3j) / 20., rel=1e-12,
    )
    phases = np.exp(1j * np.array([.3, 2.1, -.8]))
    rotated = phases[:, None] * values * phases[None, :].conj()
    other = surge_response_kernel(make_qtf(omega, rotated), spectrum, omega, **kwargs)
    np.testing.assert_allclose(other.eigenvalues_m, result.eigenvalues_m, atol=1e-14)


def test_diagonal_load_has_mean_but_no_dynamic_spectrum_or_extreme():
    omega = np.array([.7, .8])
    qtf = make_qtf(omega, np.diag([10., -20.]))
    kernel = surge_response_kernel(
        qtf, [5., 1.25], omega, mass_kg=1e7,
        stiffness_kn_m=100., damping_ratio=.05,
    )
    np.testing.assert_array_equal(full_qtf_force_spectrum(qtf, [5., 1.25], omega), 0.)
    assert kernel.variance_m2 == 0.
    assert kernel.mean_offset_m == pytest.approx(.05)
    assert naess_expected_extreme(
        kernel.eigenvalues_m, .01, 10800, mean_offset_m=kernel.mean_offset_m,
    ) == pytest.approx(.05)


@pytest.mark.parametrize("change", [
    {"mass_kg": 0.}, {"mass_kg": np.nan}, {"stiffness_kn_m": -1.},
    {"damping_kn_s_m": 0.}, {"damping_kn_s_m": np.inf},
    {"damping_ratio": .05}, {"mean_force_kn": np.nan},
    {"force_spectrum": [1., 500., 0.]}, {"force_spectrum": [0., -1., 0.]},
    {"force_spectrum": [0., np.inf, 0.]}, {"force_spectrum": [0., 500.]},
    {"mu_rad_s": [0., .1, .21]}, {"mu_rad_s": [0., .1, .1]},
    {"mu_rad_s": [.1, .2, .3]}, {"mu_rad_s": [0., np.nan, .2]},
    {"force_spectrum": [0., 500.+1j, 0.]},
])
def test_response_refuses_invalid_physics_and_discretization(change):
    inputs = dict(force_spectrum=[0., 500., 0.], mu_rad_s=[0., .1, .2],
                  mass_kg=1e7, stiffness_kn_m=100., damping_kn_s_m=200.)
    inputs.update(change)
    with pytest.raises(ValueError):
        low_frequency_surge_response(**inputs)


@pytest.mark.parametrize("kind", [
    "nonuniform", "triangle", "imaginary_diagonal", "asymmetric", "nan",
    "shape", "imag_shape", "mismatched_grid", "negative_spectrum", "sway",
])
def test_kernel_refuses_invalid_qtf(kind):
    omega = np.array([.5, .6, .7])
    values = np.ones((3, 3), dtype=complex)
    spectrum = np.ones(3)
    if kind == "nonuniform":
        omega[-1] = .71
    elif kind == "triangle":
        values = np.triu(values)
    elif kind == "imaginary_diagonal":
        values[0, 0] += 1j
    elif kind == "asymmetric":
        values[0, 1] = 2.
    elif kind == "nan":
        values[0, 1] = np.nan
    elif kind == "shape":
        values = values[:2, :2]
    elif kind == "negative_spectrum":
        spectrum[0] = -1.
    qtf = make_qtf(omega, values)
    if kind == "mismatched_grid":
        qtf.frequencies_rad_s[0] = .4
    if kind == "imag_shape":
        qtf.imag_part = [[0.], [0.], [0.]]
    if kind == "sway":
        qtf.dof = "sway"
    with pytest.raises(ValueError):
        surge_response_kernel(
            qtf, spectrum, omega, mass_kg=1e7, stiffness_kn_m=100., damping_ratio=.05,
        )


@pytest.mark.parametrize("change", [
    {"upcrossing_rate_hz": 0.}, {"upcrossing_rate_hz": np.nan},
    {"duration_s": -1.}, {"duration_s": np.inf}, {"duration_s": 1.},
    {"duration_s": 101.}, {"eigenvalues_m": [np.nan]},
    {"eigenvalues_m": []}, {"mean_offset_m": np.inf}, {"tail": "height"},
])
def test_naess_refuses_invalid_or_too_short_inputs(change):
    inputs = dict(eigenvalues_m=[-.5, .5], upcrossing_rate_hz=.01, duration_s=10800)
    inputs.update(change)
    with pytest.raises(ValueError):
        naess_expected_extreme(**inputs)
