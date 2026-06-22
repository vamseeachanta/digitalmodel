"""
DNV-RP-C203 bi-linear S-N endurance — closed-form forward / inverse.
====================================================================

A small, dependency-light companion to :mod:`sn_curves` and
:mod:`sn_library_api` that exposes the S-N relation in its raw
log-log form, exactly as it appears in standard DNV-RP-C203 design
spreadsheets::

    log10(N) = log10(a) - m * log10(Δσ)        (forward)
        Δσ  = 10 ** ((log10(a) - log10(N)) / m) (inverse)

The curve is bi-linear about the knee point N_D = 1e7 cycles:

    * N <= 1e7  -> first segment   (m1, log_a1)
    * N >  1e7  -> second segment  (m2, log_a2)

Equivalently in the stress domain the high-stress branch (Δσ above the
fatigue limit at the knee) uses (m1, log_a1) and the low-stress branch
uses (m2, log_a2).

This module deliberately *reuses* the DNV curve constants already held
in :data:`digitalmodel.fatigue.sn_curves.DNV_CURVES` and the
:func:`digitalmodel.fatigue.damage.thickness_correction` helper rather
than re-declaring them.  It adds only the closed-form endurance/inverse
pair, which the existing pyLife-backed API does not expose directly.

References
----------
- DNV-RP-C203, Fatigue Design of Offshore Steel Structures,
  Tables 2-1 (in air) and 2-2 (seawater with cathodic protection),
  S-N relation eq. (2.4.1) and thickness effect eq. (2.4.3).

Issue: corpus calculators epic #767 / #779.
"""

from __future__ import annotations

import math
from typing import Union

import numpy as np

from .sn_curves import (
    _RAW_CURVES,
    _SEAWATER_CP_ADJUSTMENTS,
    _FREE_CORROSION_LOG_A,
    _ND,
)
from .damage import thickness_correction  # re-export the DNV thickness rule

__all__ = [
    "sn_curve_parameters",
    "endurance_cycles",
    "stress_range_at_cycles",
    "thickness_correction",
]

ArrayLike = Union[float, np.ndarray]


def sn_curve_parameters(name: str, environment: str = "air") -> dict:
    """Return the bi-linear S-N parameters for a DNV-RP-C203 detail class.

    Parameters
    ----------
    name : str
        Detail category, e.g. ``"D"``, ``"F"``, ``"B1"`` (case-insensitive).
    environment : str
        One of ``"air"``, ``"seawater_cp"``, ``"free_corrosion"``.

    Returns
    -------
    dict
        Keys ``m1``, ``log_a1``, ``m2``, ``log_a2``, ``nd`` (knee cycles).
        For ``free_corrosion`` the curve is single-slope, so the second
        segment mirrors the first.
    """
    key = name.upper()
    if key not in _RAW_CURVES:
        raise ValueError(
            f"Unknown curve '{name}'. Available: {sorted(_RAW_CURVES)}"
        )
    base = _RAW_CURVES[key]
    m1 = base["k_1"]

    if environment == "air":
        log_a1 = base["log_a1"]
        m2 = base["k_2"]
        log_a2 = base["log_a2"]
    elif environment == "seawater_cp":
        adj = _SEAWATER_CP_ADJUSTMENTS[key]
        log_a1 = adj["log_a1"]
        m2 = base["k_2"]
        log_a2 = adj["log_a2"]
    elif environment == "free_corrosion":
        log_a1 = _FREE_CORROSION_LOG_A[key]
        # single slope throughout — no endurance knee
        m2 = m1
        log_a2 = log_a1
    else:
        raise ValueError(
            f"Unknown environment '{environment}'. Use 'air', "
            "'seawater_cp', or 'free_corrosion'."
        )

    return {
        "m1": m1,
        "log_a1": log_a1,
        "m2": m2,
        "log_a2": log_a2,
        "nd": _ND,
    }


def _stress_at_knee(params: dict) -> float:
    """Stress range at the knee point N_D (the fatigue limit there)."""
    return 10 ** ((params["log_a1"] - math.log10(params["nd"])) / params["m1"])


def endurance_cycles(
    stress_range: ArrayLike,
    name: str,
    environment: str = "air",
) -> ArrayLike:
    """Allowable cycles N for a stress range Δσ (DNV-RP-C203 S-N).

    Forward relation ``log10(N) = log10(a) - m * log10(Δσ)`` with the
    bi-linear segment selected by whether Δσ lies above or below the
    fatigue limit at the knee (N_D = 1e7).

    Parameters
    ----------
    stress_range : float or array-like
        Stress range Δσ in MPa.
    name : str
        DNV detail category.
    environment : str
        ``"air"``, ``"seawater_cp"`` or ``"free_corrosion"``.

    Returns
    -------
    float or np.ndarray
        Allowable cycles N. ``inf`` for non-positive stress.
    """
    p = sn_curve_parameters(name, environment)
    s = np.asarray(stress_range, dtype=float)
    scalar = s.ndim == 0
    s = np.atleast_1d(s)

    s_knee = _stress_at_knee(p)
    N = np.full_like(s, np.inf, dtype=float)
    pos = s > 0
    high = pos & (s >= s_knee)
    low = pos & (s < s_knee)

    N[high] = 10 ** (p["log_a1"] - p["m1"] * np.log10(s[high]))
    N[low] = 10 ** (p["log_a2"] - p["m2"] * np.log10(s[low]))

    return float(N[0]) if scalar else N


def stress_range_at_cycles(
    cycles: ArrayLike,
    name: str,
    environment: str = "air",
) -> ArrayLike:
    """Stress range Δσ that fails at N cycles (inverse S-N).

    ``Δσ = 10 ** ((log10(a) - log10(N)) / m)`` with the segment chosen
    by the knee ``N_D = 1e7`` (N <= N_D -> segment 1, else segment 2).

    Parameters
    ----------
    cycles : float or array-like
        Number of cycles to failure N.
    name : str
        DNV detail category.
    environment : str
        ``"air"``, ``"seawater_cp"`` or ``"free_corrosion"``.

    Returns
    -------
    float or np.ndarray
        Stress range Δσ in MPa.
    """
    p = sn_curve_parameters(name, environment)
    n = np.asarray(cycles, dtype=float)
    scalar = n.ndim == 0
    n = np.atleast_1d(n)

    s = np.full_like(n, np.nan, dtype=float)
    seg1 = n <= p["nd"]
    seg2 = ~seg1
    s[seg1] = 10 ** ((p["log_a1"] - np.log10(n[seg1])) / p["m1"])
    s[seg2] = 10 ** ((p["log_a2"] - np.log10(n[seg2])) / p["m2"])

    return float(s[0]) if scalar else s
