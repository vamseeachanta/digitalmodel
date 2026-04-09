#!/usr/bin/env python3
# ABOUTME: Seakeeping module — 6-DOF spectral motion analysis for floating platforms.
# ABOUTME: Response spectra, spectral moments, motion statistics, exceedance, operability.
"""
Seakeeping analysis for frequency-domain motion response.

Computes vessel motion statistics from RAOs and wave spectra using
spectral methods.  Key capabilities:

- Response spectrum from RAO * wave spectrum
- Spectral moments (m0, m2, m4, ...) via numerical integration
- Significant motion amplitudes (s_{1/3} = 2*sqrt(m0))
- Rayleigh exceedance probabilities
- Operability analysis from scatter diagram + motion criteria

References:
    DNV-RP-C205: Environmental Conditions and Environmental Loads
    Journée & Massie: Offshore Hydromechanics, Ch. 6
    PNA Vol III: Motions in Waves
"""

import math
from typing import Dict, List, Optional, Sequence

import numpy as np

from .wave_spectra import WaveSpectra


def compute_response_spectrum(
    rao_amplitudes: np.ndarray,
    wave_spectrum: np.ndarray,
) -> np.ndarray:
    """Compute response spectrum from RAO amplitudes and a wave spectrum.

    S_response(w) = |RAO(w)|^2 * S_wave(w)

    Args:
        rao_amplitudes: RAO amplitude array (dimensionless or m/m, deg/m).
        wave_spectrum: Wave spectral density array (m^2*s).

    Returns:
        Response spectral density array, same length as inputs.

    Raises:
        ValueError: If arrays have different lengths.
    """
    if len(rao_amplitudes) != len(wave_spectrum):
        raise ValueError(
            f"RAO and wave spectrum arrays must have same length "
            f"(got {len(rao_amplitudes)} and {len(wave_spectrum)})"
        )
    return rao_amplitudes ** 2 * wave_spectrum


def spectral_moments(
    frequencies: np.ndarray,
    spectrum: np.ndarray,
    orders: Optional[Sequence[int]] = None,
) -> Dict[int, float]:
    """Compute spectral moments of arbitrary order.

    m_n = integral( w^n * S(w) dw )

    Args:
        frequencies: Frequency array (rad/s).
        spectrum: Spectral density array (m^2*s or response units).
        orders: Moment orders to compute. Defaults to [0, 2, 4].

    Returns:
        Dictionary mapping order -> moment value.
    """
    if orders is None:
        orders = [0, 2, 4]

    _trapz = np.trapezoid if hasattr(np, "trapezoid") else np.trapz

    result: Dict[int, float] = {}
    for n in orders:
        integrand = frequencies ** n * spectrum
        result[n] = float(_trapz(integrand, frequencies))
    return result


def significant_amplitude(m0: float) -> float:
    """Significant single-amplitude from zeroth spectral moment.

    s_{1/3} = 2 * sqrt(m0)

    This is the average of the highest 1/3 of amplitudes for a
    narrow-band Gaussian process (Rayleigh-distributed peaks).

    Args:
        m0: Zeroth spectral moment (variance of the process).

    Returns:
        Significant single-amplitude.

    Raises:
        ValueError: If m0 is negative.
    """
    if m0 < 0:
        raise ValueError("m0 must be non-negative")
    return 2.0 * math.sqrt(m0)


def motion_exceedance(m0: float, threshold: float) -> float:
    """Rayleigh exceedance probability for a motion amplitude.

    P(X > threshold) = exp( -threshold^2 / (2 * m0) )

    Assumes narrow-band Gaussian process with Rayleigh-distributed peaks.

    Args:
        m0: Zeroth spectral moment (variance).
        threshold: Motion amplitude threshold.

    Returns:
        Probability of exceeding threshold (0 to 1).

    Raises:
        ValueError: If m0 is not positive.
    """
    if m0 <= 0:
        raise ValueError("m0 must be positive for exceedance calculation")
    return math.exp(-(threshold ** 2) / (2.0 * m0))


def operability_analysis(
    rao_freqs: np.ndarray,
    rao_amplitudes: np.ndarray,
    scatter_diagram: List[Dict],
    criteria: Dict[str, float],
    spectrum_type: str = "jonswap",
    gamma: float = 3.3,
) -> Dict:
    """Compute operability percentage from scatter diagram and motion criteria.

    For each sea state in the scatter diagram, generates a wave spectrum,
    computes the response spectrum, derives significant motion amplitude,
    and checks against the criteria.

    Args:
        rao_freqs: Frequency array for the RAO (rad/s).
        rao_amplitudes: RAO amplitude array at those frequencies.
        scatter_diagram: List of dicts, each with keys:
            - ``hs``: significant wave height (m)
            - ``tp``: peak period (s)
            - ``probability``: occurrence probability (0-1)
        criteria: Dict mapping DOF name to maximum allowable significant
            amplitude. Currently a single DOF is applied (first key).
        spectrum_type: Wave spectrum type ('jonswap', 'pm', 'bretschneider').
        gamma: JONSWAP peakedness parameter.

    Returns:
        Dictionary with:
            - ``operability_pct``: weighted percentage of operable sea states
            - ``sea_state_results``: per-sea-state breakdown

    Raises:
        ValueError: If any probability is negative.
    """
    # Validate probabilities
    for entry in scatter_diagram:
        if entry["probability"] < 0:
            raise ValueError(
                f"Scatter diagram probability must be non-negative, "
                f"got {entry['probability']}"
            )

    ws = WaveSpectra()
    criterion_value = next(iter(criteria.values()))

    operable_weight = 0.0
    total_weight = 0.0
    sea_state_results = []

    for entry in scatter_diagram:
        hs = entry["hs"]
        tp = entry["tp"]
        prob = entry["probability"]
        total_weight += prob

        # Generate wave spectrum on the RAO frequency grid
        freq_min = float(rao_freqs[0])
        freq_max = float(rao_freqs[-1])
        n_points = len(rao_freqs)

        if spectrum_type == "jonswap":
            omega, S_wave = ws.jonswap(
                hs=hs, tp=tp, gamma=gamma,
                freq_min=freq_min, freq_max=freq_max, n_points=n_points,
            )
        elif spectrum_type == "pm":
            omega, S_wave = ws.pierson_moskowitz(
                hs=hs, tp=tp,
                freq_min=freq_min, freq_max=freq_max, n_points=n_points,
            )
        elif spectrum_type == "bretschneider":
            omega, S_wave = ws.bretschneider(
                hs=hs, tp=tp,
                freq_min=freq_min, freq_max=freq_max, n_points=n_points,
            )
        else:
            raise ValueError(f"Unknown spectrum type: {spectrum_type}")

        # Response spectrum and m0
        S_resp = compute_response_spectrum(rao_amplitudes, S_wave)
        moments = spectral_moments(omega, S_resp, orders=[0])
        m0 = moments[0]
        sig_amp = significant_amplitude(m0)

        is_operable = sig_amp <= criterion_value
        if is_operable:
            operable_weight += prob

        sea_state_results.append({
            "hs": hs,
            "tp": tp,
            "probability": prob,
            "m0": m0,
            "significant_amplitude": sig_amp,
            "operable": is_operable,
        })

    operability_pct = (operable_weight / total_weight * 100.0) if total_weight > 0 else 0.0

    return {
        "operability_pct": operability_pct,
        "sea_state_results": sea_state_results,
    }
