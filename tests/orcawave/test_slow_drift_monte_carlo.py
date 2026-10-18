"""Random-phase FFT references: synthetic JONSWAP, no solver or external data.

Run directly with PYTHONPATH=src to print all metrics. The period is exactly
3 hours, so FFT bins and the oscillator's forcing frequencies coincide.
"""

from functools import lru_cache
import json

import numpy as np
import pytest

from digitalmodel.orcawave.drift_forces import (
    QTFMatrix,
    full_qtf_force_spectrum,
    low_frequency_surge_response,
    surge_response_kernel,
)
from digitalmodel.orcawave.slow_drift_response import naess_expected_extreme
from digitalmodel.orcawave.wave_spectrum import jonswap


@lru_cache(maxsize=1)
def ensemble_reference():
    """256 independent fixed-amplitude phase records, 16384 samples each."""
    duration, count, samples = 10800., 2049, 16384
    width = 2 * np.pi / duration
    omega = .2 + width * np.arange(count)
    spectrum = jonswap(omega, hs=6., tp=10.)
    qtf = QTFMatrix(
        dof="surge", heading_deg=0., frequencies_rad_s=omega.tolist(),
        real_part=np.full((count, count), 10.).tolist(),
        imag_part=np.zeros((count, count)).tolist(),
    )
    params = dict(mass_kg=1e7, stiffness_kn_m=1e4 * (2*np.pi/100)**2,
                  damping_ratio=.05)
    force_psd = full_qtf_force_spectrum(qtf, spectrum, omega)
    response = low_frequency_surge_response(
        force_psd, width * np.arange(count), **params,
    )
    kernel = surge_response_kernel(qtf, spectrum, omega, **params)
    # Independent time-domain force: Q*|sum(a_i exp(i*(w_i*t+phi_i)))|².
    rng = np.random.default_rng(206)
    phases = rng.uniform(0, 2*np.pi, (256, count))
    envelope = np.fft.ifft(np.sqrt(2*spectrum*width) * np.exp(1j*phases),
                           n=samples, axis=1) * samples
    force = 10 * np.abs(envelope)**2
    force_fft = np.fft.rfft(force, axis=1)
    force_fft[:, 0] = 0.
    mu = width * np.arange(samples//2 + 1)
    # Independent scalar steady-state ODE solution in the Fourier domain.
    stiffness = params["stiffness_kn_m"]
    damping = 2 * .05 * np.sqrt(1e4 * stiffness)
    transfer = 1/(stiffness - 1e4*mu**2 + 1j*damping*mu)
    surge = np.fft.irfft(force_fft * transfer, n=samples, axis=1)
    return _metrics(force, surge, force_psd, response, kernel, duration, width)


def _metrics(force, surge, force_psd, response, kernel, duration, width):
    crossings = np.count_nonzero((surge[:, :-1] < 0) & (surge[:, 1:] >= 0), axis=1)
    rate = float(crossings.mean()/duration)
    maxima = surge.max(axis=1)
    return dict(
        seed=206, records=len(surge), samples_per_record=surge.shape[1],
        duration_s=duration, natural_period_s=100., damping_ratio=.05,
        force_variance_kn2=float(force_psd.sum()*width),
        mc_force_variance_kn2=float(np.var(force, axis=1).mean()),
        spectral_std_m=response.std_m,
        mc_std_m=float(np.sqrt(np.var(surge, axis=1).mean())),
        kernel_variance_m2=kernel.variance_m2,
        mean_offset_m=kernel.mean_offset_m,
        mc_mean_offset_m=float(force.mean() / (1e4*(2*np.pi/100)**2)),
        observed_upcrossing_rate_hz=rate,
        naess_dynamic_amplitude_m=naess_expected_extreme(kernel.eigenvalues_m, rate, duration),
        mc_dynamic_amplitude_m=float(maxima.mean()),
        mc_maxima_95pct_halfwidth_m=float(1.96*maxima.std(ddof=1)/np.sqrt(len(maxima))),
    )


@pytest.mark.slow
def test_broadband_force_and_surge_match_random_phase_ensemble():
    data = ensemble_reference()
    assert data["force_variance_kn2"] == pytest.approx(data["mc_force_variance_kn2"], rel=.05)
    assert data["spectral_std_m"] == pytest.approx(data["mc_std_m"], rel=.05)
    # Identity checks of the shared inputs, not independent ensemble evidence.
    assert data["kernel_variance_m2"] == pytest.approx(data["spectral_std_m"]**2, rel=1e-10)
    assert data["mean_offset_m"] == pytest.approx(data["mc_mean_offset_m"], rel=1e-10)


@pytest.mark.slow
@pytest.mark.xfail(strict=True, raises=AssertionError,
                   reason="Eq. 21 asymptote misses the 10% 3-h criterion; see method note")
def test_naess_three_hour_mean_maximum_within_ten_percent():
    data = ensemble_reference()
    assert data["naess_dynamic_amplitude_m"] == pytest.approx(
        data["mc_dynamic_amplitude_m"], rel=.10,
    ), data


if __name__ == "__main__":
    print(json.dumps(ensemble_reference(), indent=2))
