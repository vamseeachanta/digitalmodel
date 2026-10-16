"""Synthetic QTF regression and independent time-domain references for issue 2617.

Reference convention: Orcina, 'Vessel theory: Wave drift and sum frequency loads',
Second order load theory: F(t) = Re sum_ij Q_ij a_i a_j exp(i(w_i-w_j)t).
All coefficients, sea states and oscillator parameters below are synthetic.
Run this file directly with PYTHONPATH=src to print the reproducible metrics.
"""

import json

import numpy as np
import pytest
from scipy.signal import lsim

from digitalmodel.orcawave.drift_forces import (
    MeanDriftCoefficients,
    QTFMatrix,
    compute_mean_drift_force,
    full_qtf_slowly_varying,
    newman_approximation,
)
from digitalmodel.orcawave.wave_spectrum import jonswap


def _qtf(omega, values):
    """Construct a physical Hermitian QTF, in kN/m^2."""
    values = np.asarray(values, dtype=complex)
    return QTFMatrix(
        dof="surge", heading_deg=0.0, frequencies_rad_s=omega.tolist(),
        real_part=values.real.tolist(), imag_part=values.imag.tolist(),
    )


def _coefficients(omega, values):
    return MeanDriftCoefficients(
        heading_deg=0.0, frequencies_rad_s=omega.tolist(),
        surge_kn_m2=np.asarray(values).tolist(),
        sway_kn_m2=np.zeros(len(omega)).tolist(),
        yaw_knm_m2=np.zeros(len(omega)).tolist(),
    )


def narrow_band_reference():
    """Average independent record variances; do not treat correlated samples as iid.

    Each record spans exactly 2*pi/dw seconds. The FFT evaluates the complex
    wave envelope, then F=Q*|z|^2 independently of the production pair sum.
    Nyquist=2.048 rad/s exceeds the largest difference frequency, 1.024 rad/s.
    """
    omega = np.linspace(0.2, 1.224, 2049)
    dw = omega[1] - omega[0]
    spectrum = jonswap(omega, hs=2.0, tp=2 * np.pi / 0.8, gamma=10.0)
    coefficient = 10.0  # kN/m^2; constant Newman QTF
    amplitudes = np.sqrt(2 * spectrum * dw)
    rng = np.random.default_rng(2617)
    variances = []
    for _ in range(32):
        phases = rng.uniform(0.0, 2 * np.pi, (64, len(omega)))
        envelope = np.fft.ifft(
            amplitudes * np.exp(1j * phases), n=8192, axis=1,
        ) * 8192
        force = coefficient * np.abs(envelope)**2
        variances.extend(np.var(force, axis=1).tolist())
    variances = np.asarray(variances)
    qtf = _qtf(omega, np.full((len(omega), len(omega)), coefficient))
    full_variance = full_qtf_slowly_varying(qtf, spectrum, omega)**2
    newman = newman_approximation(
        _coefficients(omega, np.full(len(omega), coefficient)), spectrum, omega,
    )
    return {
        "seed": 2617, "records": len(variances), "samples_per_record": 8192,
        "duration_per_record_s": float(2 * np.pi / dw),
        "monte_carlo_variance_kn2": float(variances.mean()),
        "relative_95pct_halfwidth": float(
            1.96 * variances.std(ddof=1) / np.sqrt(len(variances)) / variances.mean()
        ),
        "full_variance_kn2": full_variance,
        "newman_variance_kn2": float(newman.sum() * dw),
    }


def surge_oscillator_reference():
    """Time-integrate m*x''+c*x'+k*x=F for a bichromatic random-phase force.

    A complete number of beat cycles removes finite-window phase error. Four
    independent phase records check phase invariance; 64 cycles are discarded
    before measuring 128 cycles (exp(-zeta*wn*t) < 4e-18 at the cutoff).
    lsim integrates a piecewise-linear forcing with 256 samples per beat cycle.
    """
    omega = np.array([0.7, 0.8])
    dw = omega[1] - omega[0]
    spectrum = np.array([5.0, 1.25])  # amplitudes 1 m and 0.5 m
    mass, damping, stiffness = 10000.0, 200.0, 100.0  # kN*s^2/m, kN*s/m, kN/m
    delta = omega[:, None] - omega[None, :]
    transfer = 1 / (stiffness - mass * delta**2 + 1j * damping * delta)
    response_qtf = _qtf(omega, 10.0 * np.abs(transfer))
    predicted = full_qtf_slowly_varying(response_qtf, spectrum, omega)
    period = 2 * np.pi / dw
    time = np.arange(192 * 256) * period / 256
    rng = np.random.default_rng(2617)
    response_stds = []
    force_variances = []
    for _ in range(4):
        phases = rng.uniform(0.0, 2 * np.pi, 2)
        envelope = np.sum(
            np.sqrt(2 * spectrum * dw)[:, None]
            * np.exp(1j * ((omega - omega[0])[:, None] * time + phases[:, None])),
            axis=0,
        )
        force = 10.0 * np.abs(envelope)**2
        _, response, _ = lsim(([1.0], [mass, damping, stiffness]), force, time)
        response_stds.append(float(np.std(response[64 * 256:])))
        force_variances.append(float(np.var(force[64 * 256:])))
    return {
        "predicted_std_m": predicted,
        "monte_carlo_std_m": float(np.mean(response_stds)),
        "phase_record_stds_m": response_stds,
        "force_variance_kn2": float(np.mean(force_variances)),
        "samples_per_cycle": 256, "retained_cycles": 128, "discarded_cycles": 64,
    }


def broadband_surge_oscillator_reference():
    """Filter a fixed-amplitude, random-phase force ensemble in periodic steady state.

    The synthetic sea matches narrow_band_reference. Its many pairs share beat
    bins, unlike the bichromatic case. The envelope-squared force is independent
    of the production pair sum. Nyquist 2.048 rad/s exceeds all beats (1.024).
    The complex oscillator transfer is applied to the real force FFT, with DC
    removed and negative-frequency conjugate symmetry supplied by irfft.
    """
    omega = np.linspace(0.2, 1.224, 2049)
    dw = omega[1] - omega[0]
    spectrum = jonswap(omega, hs=2.0, tp=2 * np.pi / 0.8, gamma=10.0)
    amplitudes = np.sqrt(2 * spectrum * dw)
    mass, damping, stiffness = 10000.0, 200.0, 100.0  # kN*s^2/m, kN*s/m, kN/m
    delta = omega[:, None] - omega[None, :]
    response_qtf = _qtf(
        omega, 10.0 / np.abs(stiffness - mass * delta**2 + 1j * damping * delta),
    )
    predicted = full_qtf_slowly_varying(response_qtf, spectrum, omega)
    samples = 8192
    beat_omega = dw * np.arange(samples // 2 + 1)
    transfer = 1 / (stiffness - mass * beat_omega**2 + 1j * damping * beat_omega)
    rng = np.random.default_rng(2617)
    variances = []
    for _ in range(8):
        phases = rng.uniform(0.0, 2 * np.pi, (64, len(omega)))
        envelope = np.fft.ifft(
            amplitudes * np.exp(1j * phases), n=samples, axis=1,
        ) * samples
        force_fft = np.fft.rfft(10.0 * np.abs(envelope)**2, axis=1)
        force_fft[:, 0] = 0.0
        response = np.fft.irfft(force_fft * transfer, n=samples, axis=1)
        variances.extend(np.var(response, axis=1).tolist())
    variances = np.asarray(variances)
    return {
        "seed": 2617, "records": len(variances), "samples_per_record": samples,
        "duration_per_record_s": float(2 * np.pi / dw),
        "predicted_std_m": predicted,
        "monte_carlo_std_m": float(np.sqrt(variances.mean())),
        "relative_variance_95pct_halfwidth": float(
            1.96 * variances.std(ddof=1) / np.sqrt(len(variances)) / variances.mean()
        ),
    }


@pytest.mark.parametrize("pair", [10.0 + 0j, 3.0 + 4j])
def test_bichromatic_pair_variance_excludes_double_count(pair):
    omega = np.array([0.7, 0.8])
    spectrum = np.array([5.0, 1.25])
    qtf = _qtf(omega, [[10.0, pair], [pair.conjugate(), 10.0]])
    # One real beat force: amplitude=2*a1*a2*|Q12|; variance=amplitude^2/2.
    expected_variance = (2 * 1.0 * 0.5 * abs(pair))**2 / 2
    actual = full_qtf_slowly_varying(qtf, spectrum, omega)**2
    assert actual == pytest.approx(expected_variance, rel=1e-12)


@pytest.mark.parametrize("count", [1, 4])
def test_diagonal_qtf_has_mean_load_but_zero_fluctuation(count):
    omega = 0.7 + 0.1 * np.arange(count)
    qtf = _qtf(omega, 10.0 * np.eye(count))
    assert full_qtf_slowly_varying(qtf, np.ones(count), omega) == 0.0


@pytest.mark.parametrize("diagonal", [[10., 10.], [10., 20.], [-10., -20.]])
def test_newman_full_agree_with_full_endpoint_bin_weights(diagonal):
    omega = np.array([0.7, 0.8])
    spectrum = np.array([5.0, 1.25])
    diagonal = np.asarray(diagonal)
    qtf = _qtf(omega, 0.5 * (diagonal[:, None] + diagonal[None, :]))
    newman = newman_approximation(_coefficients(omega, diagonal), spectrum, omega)
    assert newman[0] == 0.0
    assert newman.sum() * (omega[1] - omega[0]) == pytest.approx(
        full_qtf_slowly_varying(qtf, spectrum, omega)**2, rel=1e-12,
    )


def test_mean_drift_qtf_normalization_matches_wave_amplitudes():
    omega = np.array([0.6, 0.7, 0.8, 0.9])
    spectrum = np.array([0.0, 5.0, 1.25, 0.0])
    coeffs = _coefficients(omega, np.full(4, 10.0))
    mean_force, _, _ = compute_mean_drift_force(coeffs, spectrum, omega)
    assert mean_force == pytest.approx(10.0 * (1.0**2 + 0.5**2), rel=1e-12)


@pytest.mark.slow
def test_narrow_band_monte_carlo_force_variance():
    result = narrow_band_reference()
    assert result["relative_95pct_halfwidth"] < 0.01
    assert result["full_variance_kn2"] == pytest.approx(
        result["monte_carlo_variance_kn2"], rel=0.01,
    )
    assert result["full_variance_kn2"] == pytest.approx(
        result["newman_variance_kn2"], rel=1e-12,
    )


def test_linear_surge_oscillator_std_matches_time_domain():
    result = surge_oscillator_reference()
    assert result["force_variance_kn2"] == pytest.approx(50.0, rel=1e-12)
    assert result["predicted_std_m"] == pytest.approx(
        result["monte_carlo_std_m"], rel=0.05,
    )
    assert np.ptp(result["phase_record_stds_m"]) < 1e-10


@pytest.mark.slow
def test_broadband_surge_oscillator_std_matches_monte_carlo():
    """Response SD shall be within 5 % of the fixed-amplitude phase ensemble."""
    result = broadband_surge_oscillator_reference()
    assert result["relative_variance_95pct_halfwidth"] < 0.02
    assert result["predicted_std_m"] == pytest.approx(
        result["monte_carlo_std_m"], rel=0.05,
    )


if __name__ == "__main__":
    print(json.dumps({
        "narrow_band": narrow_band_reference(),
        "surge_oscillator": surge_oscillator_reference(),
        "broadband_surge_oscillator": broadband_surge_oscillator_reference(),
    }, indent=2))
