"""
ABOUTME: Motion response statistics for OrcaWave analysis workflows.
Compute significant and maximum responses from RAO + wave spectrum,
short-term Rayleigh distribution, long-term scatter diagram integration,
and Motion Sickness Incidence (MSI) per ISO 2631-1.
"""

from typing import Dict, List, Optional

import numpy as np
from pydantic import BaseModel, ConfigDict, Field
import warnings


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------


class ResponseSpectrum(BaseModel):
    """Response spectrum S_r(omega) = |RAO(omega)|^2 * S_wave(omega)."""

    model_config = ConfigDict(allow_inf_nan=False)

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

    model_config = ConfigDict(allow_inf_nan=False)

    significant_single_amplitude: float
    significant_double_amplitude: float

    @property
    def significant(self) -> float:
        """Deprecated compatibility alias for the significant single amplitude."""
        warnings.warn(
            "Use significant_single_amplitude or significant_double_amplitude",
            DeprecationWarning,
            stacklevel=2,
        )
        return self.significant_single_amplitude

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

    model_config = ConfigDict(allow_inf_nan=False)

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

    model_config = ConfigDict(allow_inf_nan=False)

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

    model_config = ConfigDict(allow_inf_nan=False)

    msi_percent: float = Field(
        ...,
        description="Percentage of occupants expected to be sick",
    )
    vertical_acceleration_rms: float = Field(
        ...,
        description="Vertical RMS acceleration (m/s^2)",
    )
    exposure_hours: float
    msdv_z: float
    qualification: str = (
        "computational_reference_only; licensed edition verification pending"
    )
    citations: List[str]
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
    from .wave_spectrum import validate_spectral_grid

    validate_spectral_grid(omega, wave_spectrum)
    if np.shape(rao_amplitude) != np.shape(omega) or not np.all(
        np.isfinite(rao_amplitude)
    ):
        raise ValueError("RAO must have one finite amplitude per frequency")
    s_r = np.abs(rao_amplitude) ** 2 * wave_spectrum

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
    m0, m2, m4 = response.m0, response.m2, response.m4
    if min(m0, m2, m4) <= 0 or not np.isfinite(duration_s) or duration_s <= 0:
        raise ValueError("Positive moments and duration are required")
    if m2**2 > m0 * m4 * (1 + 1e-12):
        raise ValueError("Spectral moments violate m2**2 <= m0*m4")

    sigma = np.sqrt(m0)
    tz = 2.0 * np.pi * np.sqrt(m0 / m2)
    epsilon = np.sqrt(max(0.0, 1.0 - m2**2 / (m0 * m4)))

    # Number of zero-crossings in duration
    n_cycles = duration_s / tz if tz > 0 else 1.0

    if n_cycles <= 1:
        raise ValueError("Rayleigh maximum asymptote requires more than one cycle")

    # Most probable maximum (Rayleigh)
    mp_max = sigma * np.sqrt(2.0 * np.log(max(n_cycles, 1.0)))

    # Narrow-band Rayleigh/Gumbel asymptote; see engineering-citations.md, R1.
    euler_gamma = 0.5772156649015329  # Euler-Mascheroni constant
    e_max = mp_max + euler_gamma * sigma**2 / mp_max

    return ShortTermStatistics(
        significant_single_amplitude=float(2 * sigma),
        significant_double_amplitude=float(4 * sigma),
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

    P(X > x) = exp(-(x / sigma)^2 / 2).
    Source: SciPy 1.14.1 reference (2024), scipy.stats.rayleigh, Notes;
    integrate the standardized density u*exp(-u^2/2) over [x/sigma, infinity).
    https://docs.scipy.org/doc/scipy-1.14.1/reference/generated/scipy.stats.rayleigh.html

    Args:
        amplitude: Finite, nonnegative response amplitude threshold.
        sigma: Finite, positive RMS response (sqrt(m0)).

    Returns:
        Exceedance probability.

    Raises:
        ValueError: Threshold or scale is outside the declared domain.
    """
    if not np.isfinite(amplitude) or amplitude < 0:
        raise ValueError("Require finite amplitude >= 0")
    if not np.isfinite(sigma) or sigma <= 0:
        raise ValueError("Require finite sigma > 0")
    # Form the dimensionless ratio before squaring to avoid losing a valid
    # probability when dimensional inputs are individually very large or small.
    # For extreme ratios, overflow/underflow gives the valid limiting values
    # exp(-infinity)=0 and exp(0)=1; invalid operations remain errors.
    with np.errstate(over="ignore", under="ignore"):
        ratio = np.float64(amplitude) / sigma
        return float(np.exp(-0.5 * ratio**2))


def rayleigh_quantile(probability: float, sigma: float) -> float:
    """Response amplitude for a given exceedance probability (Rayleigh).

    Args:
        probability: Exceedance probability (0 < p <= 1).
        sigma: RMS response (sqrt(m0)).

    Returns:
        Response amplitude.
    """
    if (
        not np.isfinite(probability)
        or not np.isfinite(sigma)
        or probability <= 0
        or probability > 1
        or sigma <= 0
    ):
        raise ValueError("Require 0 < probability <= 1 and finite sigma > 0")
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
    duration_per_cell_s: Optional[float] = None,
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
        duration_per_cell_s: Deprecated, ignored duration per sea state (s).
            Cycle rates are derived from each response zero-crossing period.

    Returns:
        LongTermResult with extreme response and per-cell contributions.
    """
    if not scatter or not np.isclose(
        sum(c.probability for c in scatter), 1.0, atol=1e-6, rtol=0
    ):
        raise ValueError(
            "Scatter probabilities must sum to 1 within absolute tolerance 1e-6"
        )
    if duration_per_cell_s is not None:
        if not np.isfinite(duration_per_cell_s) or duration_per_cell_s <= 0:
            raise ValueError("Sea-state duration must be positive and finite")
        warnings.warn(
            "duration_per_cell_s is deprecated and ignored; response cycle rates are used",
            DeprecationWarning,
            stacklevel=2,
        )
    if not np.isfinite(return_period_years) or return_period_years <= 0:
        raise ValueError("Return period must be positive and finite")
    # Expected exceedance count = years_in_seconds * sum(p_i/Tz_i * q_i).
    target_prob = 1.0 / (return_period_years * 365.25 * 24 * 3600)

    contributions = []
    sigmas = []
    probs = []

    for cell in scatter:
        s_wave = spectrum_func(omega, cell.hs, cell.tp)
        resp = compute_response_spectrum(omega, rao_amplitude, s_wave)
        if resp.m0 <= 0 or resp.m2 <= 0:
            raise ValueError("Long-term extremes require positive response moments")
        sigma = np.sqrt(resp.m0)
        tz = 2 * np.pi * np.sqrt(resp.m0 / resp.m2)
        sigmas.append(sigma)
        probs.append(cell.probability / tz)
        contributions.append(
            {
                "hs": cell.hs,
                "tp": cell.tp,
                "probability": cell.probability,
                "sigma": float(sigma),
                "zero_crossing_period_s": float(tz),
            }
        )

    # Weighted long-term exceedance: find x such that
    # sum_i [ p_i/Tz_i * exp(-x^2/(2*sigma_i^2)) ] = 1/return_seconds
    sigmas_arr = np.array(sigmas)
    probs_arr = np.array(probs)

    # Binary search for x
    x_lo, x_hi = 0.0, 50.0 * max(sigmas) if max(sigmas) > 0 else 1.0
    for _ in range(100):
        x_mid = 0.5 * (x_lo + x_hi)
        q = np.sum(
            probs_arr * np.exp(-(x_mid**2) / (2.0 * np.maximum(sigmas_arr, 1e-30) ** 2))
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


def wf_weighting(frequency_hz: float) -> float:
    """Wf modulus, ISO 8041:2005 5.6, equations (8)-(12), Table 3.

    Applies to a single sinusoidal component. Broadband RMS must be weighted
    component by component before combining, never at a dominant frequency.
    """
    if not np.isfinite(frequency_hz) or frequency_hz <= 0:
        raise ValueError("Frequency must be positive and finite")
    s = 2j * np.pi * frequency_hz
    w1, w2, w4, w5, w6 = 2 * np.pi * np.array([0.08, 0.63, 0.25, 0.0625, 0.1])
    q = 1 / np.sqrt(2)
    high = 1 / (1 + w1 / (q * s) + (w1 / s) ** 2)
    low = 1 / (1 + s / (q * w2) + (s / w2) ** 2)
    transition = 1 / (1 + s / (0.86 * w4) + (s / w4) ** 2)
    step = (
        (1 + s / (0.8 * w5) + (s / w5) ** 2)
        / (1 + s / (0.8 * w6) + (s / w6) ** 2)
        * (w5 / w6) ** 2
    )
    return float(abs(high * low * transition * step))


def motion_sickness_incidence(
    vertical_acc_rms: float,
    exposure_hours: float,
    dominant_frequency_hz: Optional[float] = None,
    *,
    already_weighted: bool = False,
) -> MSIResult:
    """Constant-exposure MSDV estimate, ISO 2631-1:1997 Annex D.

    D.1(b), equation (D.2): MSDVz = aw * sqrt(T), with T in seconds.
    D.2: MSI percent = Km * MSDVz, Km=1/3 for the reference population.
    Input is either a single-frequency RMS or an explicitly Wf-weighted RMS.
    A dominant frequency alone cannot qualify a broadband acceleration.
    A dominant frequency is required only for unweighted sinusoidal input.
    See docs/engineering-citations.md for applicability and source sidecar.
    """
    if not np.all(np.isfinite([vertical_acc_rms, exposure_hours])):
        raise ValueError("MSI inputs must be finite")
    if vertical_acc_rms < 0 or exposure_hours <= 0:
        raise ValueError("Require nonnegative acceleration and positive exposure")
    if not already_weighted and (
        dominant_frequency_hz is None
        or not np.isfinite(dominant_frequency_hz)
        or dominant_frequency_hz <= 0
    ):
        raise ValueError("Unweighted input requires a positive finite frequency")
    a_w = (
        vertical_acc_rms
        if already_weighted
        else vertical_acc_rms * wf_weighting(dominant_frequency_hz)
    )
    dose = a_w * np.sqrt(exposure_hours * 3600)
    return MSIResult(
        msi_percent=float(min(dose / 3, 100)),
        vertical_acceleration_rms=float(a_w),
        exposure_hours=exposure_hours,
        frequency_weighted=True,
        msdv_z=float(dose),
        citations=[
            "ISO 2631-1:1997 D.1(b), Eq. (D.2), D.2",
            "ISO 8041:2005 5.6, Eqs. (8)-(12), Table 3 Wf",
        ],
    )
