"""
ABOUTME: Motion response statistics for OrcaWave analysis workflows.
Compute significant and maximum responses from RAO + wave spectrum,
short-term Rayleigh distribution, long-term scatter diagram integration,
and Motion Sickness Incidence (MSI) per ISO 2631-1.
"""

from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class ResponseSpectrum(BaseModel):
    """Response spectrum S_r(omega) = |RAO(omega)|^2 * S_wave(omega)."""

    frequencies: List[float] = Field(..., description="Frequency vector (rad/s)")
    spectral_density: List[float] = Field(
        ...,
        description="Response spectral density",
    )
    m0: float = Field(..., description="Zeroth spectral moment (variance)")
    m2: float = Field(..., description="Second spectral moment")
    m4: float = Field(..., description="Fourth spectral moment")


class ShortTermStatistics(BaseModel):
    """Short-term response statistics assuming narrow-band Rayleigh."""

    significant: float = Field(..., description="Significant response (double amplitude)")
    rms: float = Field(..., description="RMS response")
    mean_zero_crossing_period: float = Field(..., description="Tz in seconds")
    most_probable_maximum_3hr: float = Field(
        ...,
        description="Most probable maximum in 3-hour sea state",
    )
    expected_maximum_3hr: float = Field(
        ...,
        description="Expected maximum in 3-hour sea state",
    )
    spectral_bandwidth: float = Field(
        ...,
        description="Bandwidth parameter epsilon",
    )


class ScatterCell(BaseModel):
    """One cell from a wave scatter diagram."""

    hs: float = Field(..., description="Significant wave height (m)", gt=0)
    tp: float = Field(..., description="Peak period (s)", gt=0)
    probability: float = Field(
        ...,
        description="Probability of occurrence (0-1)",
        ge=0,
        le=1,
    )


class LongTermResult(BaseModel):
    """Long-term extreme response from scatter diagram integration."""

    return_period_years: float
    extreme_response: float = Field(
        ...,
        description="Long-term extreme response value",
    )
    contributions: List[Dict[str, float]] = Field(
        default_factory=list,
        description="Per-cell contributions [{hs, tp, probability, response}]",
    )


class MSIResult(BaseModel):
    """Motion Sickness Incidence result per ISO 2631-1."""

    msi_percent: float = Field(
        ...,
        description="Percentage of occupants expected to be sick",
    )
    vertical_acceleration_rms: float = Field(
        ...,
        description="Vertical RMS acceleration (m/s^2)",
    )
    exposure_hours: float
    frequency_weighted: bool = Field(default=True)


# ---------------------------------------------------------------------------
# Response spectrum
# ---------------------------------------------------------------------------

def compute_response_spectrum(
    omega: np.ndarray,
    rao_amplitude: np.ndarray,
    wave_spectrum: np.ndarray,
) -> ResponseSpectrum:
    """Compute the response spectrum S_r = |RAO|^2 * S_wave.

    Args:
        omega: Frequency vector (rad/s).
        rao_amplitude: RAO amplitude at each frequency.
        wave_spectrum: Wave spectral density at each frequency.

    Returns:
        ResponseSpectrum with moments.
    """
    s_r = rao_amplitude**2 * wave_spectrum

    m0 = float(np.trapz(s_r, omega))
    m2 = float(np.trapz(omega**2 * s_r, omega))
    m4 = float(np.trapz(omega**4 * s_r, omega))

    return ResponseSpectrum(
        frequencies=omega.tolist(),
        spectral_density=s_r.tolist(),
        m0=m0,
        m2=m2,
        m4=m4,
    )


# ---------------------------------------------------------------------------
# Short-term statistics
# ---------------------------------------------------------------------------

def short_term_statistics(
    response: ResponseSpectrum,
    duration_s: float = 10800.0,
) -> ShortTermStatistics:
    """Compute short-term statistics from a response spectrum.

    Assumes narrow-band Rayleigh distribution for peaks.

    Args:
        response: Response spectrum with moments.
        duration_s: Duration of sea state in seconds (default 3 hours).

    Returns:
        ShortTermStatistics.
    """
    m0 = max(response.m0, 1e-30)
    m2 = max(response.m2, 1e-30)
    m4 = max(response.m4, 1e-30)

    sigma = np.sqrt(m0)
    tz = 2.0 * np.pi * np.sqrt(m0 / m2)
    epsilon = np.sqrt(1.0 - m2**2 / (m0 * m4))

    # Number of zero-crossings in duration
    n_cycles = duration_s / tz if tz > 0 else 1.0

    # Significant response (average of highest 1/3 peaks)
    significant = 2.0 * sigma  # = 4 * sqrt(m0) / 2 for double amplitude

    # Most probable maximum (Rayleigh)
    mp_max = sigma * np.sqrt(2.0 * np.log(max(n_cycles, 1.0)))

    # Expected maximum (Cartwright & Longuet-Higgins)
    euler_gamma = 0.5772
    e_max = mp_max + euler_gamma / (2.0 * mp_max / sigma**2) if mp_max > 0 else sigma

    return ShortTermStatistics(
        significant=float(significant),
        rms=float(sigma),
        mean_zero_crossing_period=float(tz),
        most_probable_maximum_3hr=float(mp_max),
        expected_maximum_3hr=float(e_max),
        spectral_bandwidth=float(epsilon),
    )


# ---------------------------------------------------------------------------
# Rayleigh distribution helpers
# ---------------------------------------------------------------------------

def rayleigh_exceedance(amplitude: float, sigma: float) -> float:
    """Probability of exceeding a given amplitude (Rayleigh distribution).

    P(X > x) = exp(-x^2 / (2 * sigma^2))

    Args:
        amplitude: Response amplitude threshold.
        sigma: RMS response (sqrt(m0)).

    Returns:
        Exceedance probability.
    """
    if sigma <= 0:
        return 0.0
    return float(np.exp(-amplitude**2 / (2.0 * sigma**2)))


def rayleigh_quantile(probability: float, sigma: float) -> float:
    """Response amplitude for a given exceedance probability (Rayleigh).

    Args:
        probability: Exceedance probability (0 < p <= 1).
        sigma: RMS response (sqrt(m0)).

    Returns:
        Response amplitude.
    """
    if probability <= 0 or probability > 1 or sigma <= 0:
        return 0.0
    return float(sigma * np.sqrt(-2.0 * np.log(probability)))


# ---------------------------------------------------------------------------
# Long-term distribution
# ---------------------------------------------------------------------------

def long_term_extreme(
    scatter: List[ScatterCell],
    rao_amplitude: np.ndarray,
    omega: np.ndarray,
    spectrum_func,
    return_period_years: float = 100.0,
    duration_per_cell_s: float = 10800.0,
) -> LongTermResult:
    """Compute long-term extreme response by integrating over a scatter diagram.

    For each scatter cell, the short-term distribution is computed and the
    long-term exceedance probability is obtained by weighting.

    Args:
        scatter: List of scatter diagram cells with (hs, tp, probability).
        rao_amplitude: RAO amplitude array (same size as omega).
        omega: Frequency vector (rad/s).
        spectrum_func: Callable(omega, hs, tp) -> S(omega).
        return_period_years: Target return period.
        duration_per_cell_s: Duration per sea state (s).

    Returns:
        LongTermResult with extreme response and per-cell contributions.
    """
    target_prob = 1.0 / (return_period_years * 365.25 * 24 * 3600 / duration_per_cell_s)

    contributions = []
    sigmas = []
    probs = []

    for cell in scatter:
        s_wave = spectrum_func(omega, cell.hs, cell.tp)
        resp = compute_response_spectrum(omega, rao_amplitude, s_wave)
        sigma = np.sqrt(max(resp.m0, 0.0))
        sigmas.append(sigma)
        probs.append(cell.probability)
        contributions.append({
            "hs": cell.hs,
            "tp": cell.tp,
            "probability": cell.probability,
            "sigma": float(sigma),
        })

    # Weighted long-term exceedance: find x such that
    # sum_i [ p_i * exp(-x^2/(2*sigma_i^2)) ] = target_prob
    sigmas_arr = np.array(sigmas)
    probs_arr = np.array(probs)

    # Binary search for x
    x_lo, x_hi = 0.0, 50.0 * max(sigmas) if max(sigmas) > 0 else 1.0
    for _ in range(100):
        x_mid = 0.5 * (x_lo + x_hi)
        q = np.sum(
            probs_arr * np.exp(-x_mid**2 / (2.0 * np.maximum(sigmas_arr, 1e-30) ** 2))
        )
        if q > target_prob:
            x_lo = x_mid
        else:
            x_hi = x_mid

    extreme = 0.5 * (x_lo + x_hi)

    return LongTermResult(
        return_period_years=return_period_years,
        extreme_response=float(extreme),
        contributions=contributions,
    )


# ---------------------------------------------------------------------------
# Motion Sickness Incidence
# ---------------------------------------------------------------------------

def motion_sickness_incidence(
    vertical_acc_rms: float,
    exposure_hours: float = 2.0,
    dominant_frequency_hz: float = 0.15,
) -> MSIResult:
    """Compute Motion Sickness Incidence per ISO 2631-1.

    MSI (%) = K_m * a_w * sqrt(t)

    where K_m is the frequency-dependent sickness constant and
    a_w is the frequency-weighted vertical acceleration RMS.

    The ISO frequency weighting peaks at ~0.167 Hz (6s period).

    Args:
        vertical_acc_rms: RMS vertical acceleration (m/s^2).
        exposure_hours: Exposure duration (hours).
        dominant_frequency_hz: Dominant frequency of vertical motion (Hz).

    Returns:
        MSIResult.
    """
    # ISO 2631-1 frequency weighting Wf for motion sickness
    # Peaks at ~0.167 Hz; simplified transfer function
    f = dominant_frequency_hz
    if f < 0.1:
        w_factor = f / 0.1
    elif f <= 0.315:
        w_factor = 1.0
    elif f <= 0.63:
        w_factor = 0.315 / f
    else:
        w_factor = (0.315 / f) ** 2

    a_w = vertical_acc_rms * w_factor

    # MSI formula: McCauley et al. (1976) / ISO 2631-1 Annex D
    # MSI% = 1/3 * (a_w)^1.4 * (t_minutes)^0.5  (simplified)
    t_minutes = exposure_hours * 60.0
    km = 1.0 / 3.0

    msi = km * (a_w**1.4) * np.sqrt(t_minutes)
    msi = min(msi, 100.0)  # cap at 100%

    return MSIResult(
        msi_percent=float(msi),
        vertical_acceleration_rms=float(a_w),
        exposure_hours=exposure_hours,
        frequency_weighted=True,
    )
