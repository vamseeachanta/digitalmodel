"""Sea-state scatter diagram fatigue analysis (DNV-RP-C203 Appendix C/D).

Computes cumulative fatigue damage by iterating over a scatter diagram
of sea states, computing spectral fatigue damage for each using existing
frequency domain methods (Dirlik, Tovo-Benasciutti), and summing per
Miner's rule.

The module orchestrates existing infrastructure:
- frequency_domain.py  -- spectral damage rate per sea state
- sn_curves.py         -- S-N curve definitions
- hydrodynamics/wave_spectra.py -- JONSWAP/PM spectra (optional)
"""

from __future__ import annotations

import numpy as np
from dataclasses import dataclass, field
from typing import Callable, Literal, Sequence

from .frequency_domain import (
    DirlikMethod,
    NarrowBandMethod,
    TovoBenasciuttiMethod,
    FrequencyDomainResult,
)
from .sn_curves import SNCurveBase


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------


@dataclass
class SeaStateEntry:
    """Single row of a scatter diagram.

    Parameters
    ----------
    hs_m : float
        Significant wave height (m).  Zero is valid (calm sea).
    tp_s : float
        Peak spectral period (s).  Must be > 0.
    probability : float
        Fractional probability of occurrence (0-1).
    duration_hours : float
        Explicit duration in hours.  If 0 the duration is derived from
        ``design_life * probability`` during analysis.
    """

    hs_m: float
    tp_s: float
    probability: float
    duration_hours: float = 0.0


@dataclass
class ScatterFatigueResult:
    """Result container for scatter diagram fatigue analysis.

    Attributes
    ----------
    total_damage : float
        Cumulative Miner's-rule damage over all sea states.
    fatigue_life_years : float
        ``design_life_years / total_damage``.  ``inf`` when damage is zero.
    per_sea_state : list[dict]
        Per-sea-state breakdown with keys ``hs``, ``tp``, ``probability``,
        ``damage``, ``damage_fraction``.
    method : str
        Spectral method name (e.g. ``"dirlik"``).
    n_sea_states : int
        Number of entries in the scatter table.
    design_life_years : float
        Design life used for the analysis.
    """

    total_damage: float
    fatigue_life_years: float
    per_sea_state: list[dict]
    method: str
    n_sea_states: int
    design_life_years: float


# ---------------------------------------------------------------------------
# Method registry
# ---------------------------------------------------------------------------

_METHOD_MAP = {
    "dirlik": DirlikMethod,
    "tovo-benasciutti": TovoBenasciuttiMethod,
    "narrow_band": NarrowBandMethod,
}


# ---------------------------------------------------------------------------
# Main public function
# ---------------------------------------------------------------------------


def scatter_fatigue_damage(
    scatter_table: Sequence[SeaStateEntry],
    stress_transfer_function: Callable[[np.ndarray], np.ndarray] | np.ndarray,
    sn_curve: SNCurveBase,
    frequencies: np.ndarray,
    method: Literal["dirlik", "tovo-benasciutti", "narrow_band"] = "dirlik",
    design_life_years: float = 25.0,
    wave_spectrum_func: Callable | None = None,
) -> ScatterFatigueResult:
    """Compute fatigue damage from a sea-state scatter diagram.

    For each sea state the function:
    1. Generates the wave spectrum S_wave(f) via *wave_spectrum_func*.
    2. Computes the response PSD:  S_resp(f) = |H(f)|^2 * S_wave(f).
    3. Feeds S_resp into the chosen spectral method to obtain damage rate.
    4. Multiplies damage rate by the sea-state duration (probability *
       design life).

    Total damage is the sum over all sea states (Miner's rule).

    Parameters
    ----------
    scatter_table : sequence of SeaStateEntry
        Scatter diagram rows.  Probabilities must sum to 1.0 +/- 1e-3.
    stress_transfer_function : callable or ndarray
        Stress RAO.  If callable, ``H(f)`` returns complex or real
        amplitudes at each frequency.  If ndarray, interpreted as
        ``|H(f)|`` values on *frequencies*.
    sn_curve : SNCurveBase
        S-N curve used for damage evaluation.
    frequencies : ndarray
        Frequency vector in **Hz**.
    method : str
        Spectral fatigue method (``"dirlik"``, ``"tovo-benasciutti"``,
        ``"narrow_band"``).
    design_life_years : float
        Service life in years (default 25).
    wave_spectrum_func : callable or None
        ``wave_spectrum_func(f_hz, hs, tp) -> S(f)`` returning the
        one-sided wave PSD in **m^2/Hz** on the *frequencies* grid.
        If *None*, tries to use ``hydrodynamics.wave_spectra.WaveSpectra``
        JONSWAP.

    Returns
    -------
    ScatterFatigueResult

    Raises
    ------
    ValueError
        If probabilities do not sum to ~1.0 or method is unknown.
    """

    # --- validate inputs ---------------------------------------------------
    if method not in _METHOD_MAP:
        raise ValueError(
            f"Unknown method '{method}'. Choose from {list(_METHOD_MAP.keys())}."
        )

    prob_sum = sum(entry.probability for entry in scatter_table)
    if abs(prob_sum - 1.0) > 1e-3:
        raise ValueError(
            f"Probabilities must sum to 1.0 (+/- 1e-3), got {prob_sum:.6f}."
        )

    # Resolve wave spectrum function
    spectrum_func = _resolve_wave_spectrum_func(wave_spectrum_func)

    # Build transfer function squared
    h_squared = _transfer_function_squared(stress_transfer_function, frequencies)

    # Constants
    design_life_seconds = design_life_years * 365.25 * 24 * 3600.0
    analyzer = _METHOD_MAP[method]()

    # --- per-sea-state loop ------------------------------------------------
    per_state: list[dict] = []
    total_damage = 0.0

    for entry in scatter_table:
        # Duration for this sea state
        if entry.duration_hours > 0:
            duration_s = entry.duration_hours * 3600.0
        else:
            duration_s = design_life_seconds * entry.probability

        # Handle calm sea (Hs == 0) -> zero PSD -> zero damage
        if entry.hs_m == 0.0:
            per_state.append(
                {
                    "hs": entry.hs_m,
                    "tp": entry.tp_s,
                    "probability": entry.probability,
                    "damage": 0.0,
                    "damage_fraction": 0.0,
                }
            )
            continue

        # Wave PSD at each frequency
        wave_psd = spectrum_func(frequencies, entry.hs_m, entry.tp_s)
        wave_psd = np.asarray(wave_psd, dtype=float)

        # Response PSD
        response_psd = h_squared * wave_psd

        # If response is all zeros, skip expensive spectral calc
        if np.all(response_psd <= 0):
            per_state.append(
                {
                    "hs": entry.hs_m,
                    "tp": entry.tp_s,
                    "probability": entry.probability,
                    "damage": 0.0,
                    "damage_fraction": 0.0,
                }
            )
            continue

        # Spectral damage rate (damage per second)
        fd_result: FrequencyDomainResult = analyzer.calculate_damage_rate(
            frequencies, response_psd, sn_curve
        )

        state_damage = fd_result.damage_rate * duration_s
        total_damage += state_damage

        per_state.append(
            {
                "hs": entry.hs_m,
                "tp": entry.tp_s,
                "probability": entry.probability,
                "damage": state_damage,
                "damage_fraction": 0.0,  # filled after loop
            }
        )

    # --- post-process fractions --------------------------------------------
    if total_damage > 0:
        for entry_dict in per_state:
            entry_dict["damage_fraction"] = entry_dict["damage"] / total_damage

    fatigue_life = (
        design_life_years / total_damage if total_damage > 0 else float("inf")
    )

    return ScatterFatigueResult(
        total_damage=total_damage,
        fatigue_life_years=fatigue_life,
        per_sea_state=per_state,
        method=method,
        n_sea_states=len(scatter_table),
        design_life_years=design_life_years,
    )


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _transfer_function_squared(
    tf: Callable[[np.ndarray], np.ndarray] | np.ndarray,
    frequencies: np.ndarray,
) -> np.ndarray:
    """Return |H(f)|^2 array from callable or array transfer function."""
    if callable(tf):
        h_vals = np.asarray(tf(frequencies))
        return np.abs(h_vals) ** 2
    else:
        h_vals = np.asarray(tf, dtype=float)
        if h_vals.shape != frequencies.shape:
            raise ValueError(
                f"Transfer function array shape {h_vals.shape} does not "
                f"match frequencies shape {frequencies.shape}."
            )
        return np.abs(h_vals) ** 2


def _resolve_wave_spectrum_func(
    user_func: Callable | None,
) -> Callable:
    """Return a ``(f_hz, hs, tp) -> S(f)`` callable.

    If *user_func* is provided it is returned directly.  Otherwise the
    hydrodynamics JONSWAP implementation is wrapped to convert from
    angular frequency to Hz.
    """
    if user_func is not None:
        return user_func

    try:
        from ...hydrodynamics.wave_spectra import WaveSpectra
    except ImportError as exc:
        raise ImportError(
            "No wave_spectrum_func provided and "
            "digitalmodel.hydrodynamics.wave_spectra is not available. "
            "Either pass wave_spectrum_func or install the hydrodynamics "
            "sub-package."
        ) from exc

    ws = WaveSpectra()

    def _jonswap_wrapper(f_hz: np.ndarray, hs: float, tp: float) -> np.ndarray:
        """JONSWAP wrapper converting angular-frequency output to Hz."""
        omega = 2 * np.pi * f_hz
        omega_arr, s_omega = ws.jonswap(
            hs=hs,
            tp=tp,
            gamma=3.3,
            freq_min=float(omega.min()),
            freq_max=float(omega.max()),
            n_points=len(f_hz),
        )
        # Interpolate onto the exact omega grid requested
        s_interp = np.interp(omega, omega_arr, s_omega)
        # Convert density from per-rad/s to per-Hz:  S(f) = 2*pi * S(omega)
        return s_interp * 2 * np.pi

    return _jonswap_wrapper
