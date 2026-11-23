"""
DNV-RP-C203 (2021) S-N Curve Library

Provides all detail categories from DNV-RP-C203 Table 2-1 (in-air),
with environment adjustments for seawater with cathodic protection
and free corrosion per Table 2-2.

Uses pyLife's WoehlerCurve for cycle calculations.
"""

import math

import numpy as np
import pandas as pd
from pylife.materiallaws.woehlercurve import WoehlerCurve

# ── DNV-RP-C203 Table 2-1: In-Air S-N Curves ──────────────────────
# Each entry: k_1, log_a1 (for N <= ND), k_2, log_a2 (for N > ND), ND
# SD is computed as 10^((log_a1 - log10(ND)) / k_1)

_ND = 1e7  # Knee point for all curves (in-air)

_RAW_CURVES = {
    "B1": {"k_1": 4.0, "log_a1": 15.117, "k_2": 5.0, "log_a2": 17.146},
    "B2": {"k_1": 4.0, "log_a1": 14.885, "k_2": 5.0, "log_a2": 16.856},
    "C":  {"k_1": 3.0, "log_a1": 12.592, "k_2": 5.0, "log_a2": 16.320},
    "C1": {"k_1": 3.0, "log_a1": 12.449, "k_2": 5.0, "log_a2": 16.081},
    "C2": {"k_1": 3.0, "log_a1": 12.301, "k_2": 5.0, "log_a2": 15.835},
    "D":  {"k_1": 3.0, "log_a1": 12.164, "k_2": 5.0, "log_a2": 15.606},
    "E":  {"k_1": 3.0, "log_a1": 12.010, "k_2": 5.0, "log_a2": 15.350},
    "F":  {"k_1": 3.0, "log_a1": 11.855, "k_2": 5.0, "log_a2": 15.091},
    "F1": {"k_1": 3.0, "log_a1": 11.699, "k_2": 5.0, "log_a2": 14.832},
    "F3": {"k_1": 3.0, "log_a1": 11.546, "k_2": 5.0, "log_a2": 14.576},
    "G":  {"k_1": 3.0, "log_a1": 11.398, "k_2": 5.0, "log_a2": 14.330},
    "W1": {"k_1": 3.0, "log_a1": 11.261, "k_2": 5.0, "log_a2": 14.101},
    "W2": {"k_1": 3.0, "log_a1": 11.107, "k_2": 5.0, "log_a2": 13.855},
    "W3": {"k_1": 3.0, "log_a1": 10.970, "k_2": 5.0, "log_a2": 13.617},
}

# ── Seawater with Cathodic Protection (DNV-RP-C203 Table 2-2) ─────
# Same slopes, but log_a values are reduced. ND shifts to 1e6 for
# the knee point in some interpretations; here we use the DNV table
# values directly. For seawater_cp the curves keep a bi-linear form
# but with reduced intercepts.
_SEAWATER_CP_ADJUSTMENTS = {
    "B1": {"log_a1": 14.917, "log_a2": 17.146},
    "B2": {"log_a1": 14.685, "log_a2": 16.856},
    "C":  {"log_a1": 12.192, "log_a2": 16.320},
    "C1": {"log_a1": 12.049, "log_a2": 16.081},
    "C2": {"log_a1": 11.901, "log_a2": 15.835},
    "D":  {"log_a1": 11.764, "log_a2": 15.606},
    "E":  {"log_a1": 11.610, "log_a2": 15.350},
    "F":  {"log_a1": 11.455, "log_a2": 15.091},
    "F1": {"log_a1": 11.299, "log_a2": 14.832},
    "F3": {"log_a1": 11.146, "log_a2": 14.576},
    "G":  {"log_a1": 10.998, "log_a2": 14.330},
    "W1": {"log_a1": 10.861, "log_a2": 14.101},
    "W2": {"log_a1": 10.707, "log_a2": 13.855},
    "W3": {"log_a1": 10.570, "log_a2": 13.617},
}

# ── Free Corrosion (DNV-RP-C203 Table 2-2) ────────────────────────
# Single slope (no endurance limit): k_2 = k_1, log_a2 = log_a1
_FREE_CORROSION_LOG_A = {
    "B1": 14.917, "B2": 14.685,
    "C":  12.192, "C1": 12.049, "C2": 11.901,
    "D":  11.764, "E":  11.610, "F":  11.455,
    "F1": 11.299, "F3": 11.146, "G":  10.998,
    "W1": 10.861, "W2": 10.707, "W3": 10.570,
}


def _compute_sd(log_a1: float, k_1: float, nd: float) -> float:
    """Compute endurance stress SD at the knee point ND."""
    return 10 ** ((log_a1 - math.log10(nd)) / k_1)


def _build_curve_dict() -> dict:
    """Build the full DNV_CURVES dictionary with computed SD values."""
    curves = {}
    for name, params in _RAW_CURVES.items():
        sd = _compute_sd(params["log_a1"], params["k_1"], _ND)
        curves[name] = {
            "k_1": params["k_1"],
            "k_2": params["k_2"],
            "log_a1": params["log_a1"],
            "log_a2": params["log_a2"],
            "ND": _ND,
            "SD": round(sd, 2),
        }
    return curves


DNV_CURVES = _build_curve_dict()


def get_sn_curve(name: str, environment: str = "air") -> WoehlerCurve:
    """
    Return a pyLife WoehlerCurve for a DNV-RP-C203 detail category.

    Parameters
    ----------
    name : str
        Curve name, e.g. 'F', 'D', 'B1'. Case-sensitive.
    environment : str
        One of 'air', 'seawater_cp', 'free_corrosion'.

    Returns
    -------
    WoehlerCurve
        pyLife WoehlerCurve instance ready for .cycles() calls.
    """
    name = name.upper()
    if name not in _RAW_CURVES:
        raise ValueError(
            f"Unknown curve '{name}'. Available: {sorted(_RAW_CURVES.keys())}"
        )

    base = _RAW_CURVES[name]
    k_1 = base["k_1"]
    nd = _ND

    if environment == "air":
        log_a1 = base["log_a1"]
        k_2 = base["k_2"]
    elif environment == "seawater_cp":
        adj = _SEAWATER_CP_ADJUSTMENTS[name]
        log_a1 = adj["log_a1"]
        k_2 = base["k_2"]
    elif environment == "free_corrosion":
        log_a1 = _FREE_CORROSION_LOG_A[name]
        # No endurance limit: single slope throughout
        k_2 = k_1
    else:
        raise ValueError(
            f"Unknown environment '{environment}'. "
            "Use 'air', 'seawater_cp', or 'free_corrosion'."
        )

    sd = _compute_sd(log_a1, k_1, nd)

    params = pd.Series({
        "k_1": k_1,
        "SD": sd,
        "ND": nd,
        "k_2": k_2,
    })
    return WoehlerCurve(params)
