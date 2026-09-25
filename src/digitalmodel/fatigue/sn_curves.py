"""
DNV-RP-C203 (2021) S-N Curve Library

Provides all detail categories from DNV-RP-C203 Table 2-1 (in-air),
with environment adjustments for seawater with cathodic protection
(Table 2-2) and free corrosion (Table 2-4). Table IDs are those of the
2021 edition; see :mod:`digitalmodel.fatigue.c203_editions` for the
2011 layout (free corrosion in Table 2-3).

Values come from :mod:`digitalmodel.fatigue.c203_sn_tables`, verified
against the 2011 edition, Tables 2-1 to 2-3 (#2165):

* in air: knee at ND = 1e7 cycles, m2 = 5 beyond it;
* seawater with CP: knee at ND = 1e6 cycles, m2 = 5 beyond it;
* free corrosion: single slope m = 3.0 for every class (B1 and B2 included),
  no knee and no fatigue limit.

Uses pyLife's WoehlerCurve for cycle calculations. pyLife anchors both
segments at the knee stress SD = 10^((log_a1 - log10(ND)) / k_1); the second
segment then matches the tabulated log a2 within table rounding.
"""

import math

import pandas as pd
from pylife.materiallaws.woehlercurve import WoehlerCurve

from . import c203_sn_tables as _c203

# ── DNV-RP-C203 Table 2-1: In-Air S-N Curves ──────────────────────
# Each entry: k_1, log_a1 (for N <= ND), k_2, log_a2 (for N > ND), ND
# SD is computed as 10^((log_a1 - log10(ND)) / k_1)

_ND = _c203.N_KNEE_AIR  # Knee point, in air
_ND_SEAWATER_CP = _c203.N_KNEE_SEAWATER_CP  # Knee point, seawater with CP

_RAW_CURVES = {
    name: {"k_1": p.m1, "log_a1": p.log_a1_air, "k_2": p.m2, "log_a2": p.log_a2}
    for name, p in _c203.BILINEAR.items()
}

# ── Seawater with Cathodic Protection (DNV-RP-C203 Table 2-2) ─────
# Same slopes and second segment as in air; reduced first-segment intercept
# and the knee at 1e6 cycles.
_SEAWATER_CP_ADJUSTMENTS = {
    name: {"log_a1": p.log_a1_cp, "log_a2": p.log_a2}
    for name, p in _c203.BILINEAR.items()
}

# ── Free Corrosion (DNV-RP-C203 Table 2-4, 2021 layout; values verified
# against 2011 Table 2-3) ─────────────────────────────────────────
# Single slope m = 3.0 for every class: k_2 = k_1, no endurance limit.
_FREE_CORROSION_LOG_A = {name: fc.log_a for name, fc in _c203.FREE_CORROSION.items()}


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

    if environment == "air":
        k_1 = base["k_1"]
        log_a1 = base["log_a1"]
        k_2 = base["k_2"]
        nd = _ND
    elif environment == "seawater_cp":
        k_1 = base["k_1"]
        log_a1 = _SEAWATER_CP_ADJUSTMENTS[name]["log_a1"]
        k_2 = base["k_2"]
        nd = _ND_SEAWATER_CP
    elif environment == "free_corrosion":
        # Single slope m = 3 throughout; ND is only pyLife's reference point.
        k_1 = _c203.M_FREE_CORROSION
        log_a1 = _FREE_CORROSION_LOG_A[name]
        k_2 = k_1
        nd = _ND
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
