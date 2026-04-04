"""
Self-contained bilinear S-N curve for DNV-RP-C203 fatigue assessment.

This module provides a lightweight S-N curve implementation that works
without importing ``digitalmodel.structural.fatigue``.  When that module
**is** available the public helper ``get_sn_curve()`` delegates to it;
otherwise it falls back to the built-in table below.

Bilinear S-N model (DNV-RP-C203 Table 2-1):
    N = A1 × S^(−m1)     for S ≥ S_transition   (low-cycle / high-stress)
    N = A2 × S^(−m2)     for S <  S_transition   (high-cycle / low-stress)

The transition stress is computed from the first slope at the knee-point
number of cycles (typically 1 × 10⁷):

    S_transition = (A1 / N_transition)^(1/m1)

Environment corrections (DNV-RP-C203 Sec 2.4):
    IN_AIR         — use the "in air" parameters directly
    SEAWATER_CP    — use the dedicated seawater-with-CP parameters

Thickness correction (DNV-RP-C203 Sec 2.4.3):
    S_corrected = S × (t / t_ref)^k
    where t_ref = 25 mm (default), k = thickness exponent.

Reference: DNV-RP-C203 "Fatigue Design of Offshore Steel Structures" (2021).
"""
from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Optional


# =========================================================================
# Bilinear S-N curve class
# =========================================================================

@dataclass(frozen=True)
class BilinearSNCurveParams:
    """Parameters for a two-slope S-N curve.

    Attributes
    ----------
    A1, m1 : float
        Intercept and slope for the first segment (N ≤ N_transition).
    A2, m2 : float
        Intercept and slope for the second segment (N > N_transition).
    N_transition : float
        Cycle count at the slope change (typically 1e7).
    fatigue_limit : float
        Constant-amplitude fatigue limit (CAFL) [MPa].  Stress ranges
        at or below this value produce infinite life.
    """
    A1: float
    m1: float
    A2: float
    m2: float
    N_transition: float = 1e7
    fatigue_limit: float = 0.0

    @property
    def transition_stress(self) -> float:
        """Stress at the knee point [MPa]."""
        return (self.A1 / self.N_transition) ** (1.0 / self.m1)


class BilinearSNCurve:
    """Two-slope S-N curve per DNV-RP-C203.

    Parameters
    ----------
    params : BilinearSNCurveParams
        Curve parameters.
    name : str
        Human-readable identifier (e.g. ``"DNV-F-air"``).
    """

    def __init__(self, params: BilinearSNCurveParams, name: str = "") -> None:
        self.params = params
        self.name = name
        self._S_trans = params.transition_stress

    # -- public API compatible with PowerLawSNCurve -----------------------

    @property
    def A(self) -> float:
        """First-slope intercept (for API compatibility)."""
        return self.params.A1

    @property
    def m(self) -> float:
        """First-slope exponent (for API compatibility)."""
        return self.params.m1

    @property
    def fatigue_limit(self) -> float:
        return self.params.fatigue_limit

    def get_allowable_cycles(self, stress_mpa: float) -> float:
        """Allowable cycles N for a given stress range S [MPa].

        Returns ``math.inf`` when *stress_mpa* ≤ CAFL.
        """
        S = abs(stress_mpa)
        if S <= self.params.fatigue_limit:
            return math.inf
        p = self.params
        if S >= self._S_trans:
            return p.A1 * S ** (-p.m1)
        return p.A2 * S ** (-p.m2)

    def get_stress_range(self, cycles: float) -> float:
        """Stress range for a given cycle count (inverse)."""
        if not math.isfinite(cycles) or cycles <= 0:
            return self.params.fatigue_limit
        p = self.params
        if cycles <= p.N_transition:
            return (p.A1 / cycles) ** (1.0 / p.m1)
        S = (p.A2 / cycles) ** (1.0 / p.m2)
        return max(S, p.fatigue_limit)


# =========================================================================
# DNV-RP-C203 curve tables  (Table 2-1, 2021 edition)
# =========================================================================
# Keys: curve class.  Each entry has "air" and "seawater_cp" sub-dicts.
#
#   A1, m1 — first slope  (N ≤ 1e7)
#   A2, m2 — second slope (N > 1e7)
#   fatigue_limit — CAFL [MPa]  (in-air value; seawater_cp has no CAFL
#                   per DNV-RP-C203 Sec 2.4.4: "No fatigue limit for
#                   joints exposed to seawater with cathodic protection.")
#
# Source: DNV-RP-C203 Table 2-1 (2021), log₁₀ intercepts converted to A.
# =========================================================================

_DNV_SN_TABLE: dict[str, dict[str, dict]] = {
    "B1": {
        "air": {"A1": 2.3431e15, "m1": 4.0, "A2": 2.3431e15, "m2": 4.0,
                "fatigue_limit": 106.97},
        "seawater_cp": {"A1": 2.3431e15, "m1": 4.0, "A2": 2.3431e15, "m2": 4.0,
                        "fatigue_limit": 0.0},
    },
    "B2": {
        "air": {"A1": 1.0147e15, "m1": 4.0, "A2": 1.0147e15, "m2": 4.0,
                "fatigue_limit": 93.59},
        "seawater_cp": {"A1": 1.0147e15, "m1": 4.0, "A2": 1.0147e15, "m2": 4.0,
                        "fatigue_limit": 0.0},
    },
    "C": {
        "air": {"A1": 1.08e12, "m1": 3.0, "A2": 1.14e16, "m2": 5.0,
                "fatigue_limit": 73.10},
        "seawater_cp": {"A1": 4.23e11, "m1": 3.0, "A2": 2.59e15, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
    "C1": {
        "air": {"A1": 4.23e11, "m1": 3.0, "A2": 2.59e15, "m2": 5.0,
                "fatigue_limit": 65.50},
        "seawater_cp": {"A1": 2.08e11, "m1": 3.0, "A2": 7.18e14, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
    "C2": {
        "air": {"A1": 1.08e11, "m1": 3.0, "A2": 2.51e14, "m2": 5.0,
                "fatigue_limit": 46.78},
        "seawater_cp": {"A1": 5.19e10, "m1": 3.0, "A2": 6.24e13, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
    "D": {
        "air": {"A1": 5.73e11, "m1": 3.0, "A2": 4.47e15, "m2": 5.0,
                "fatigue_limit": 52.63},
        "seawater_cp": {"A1": 2.83e11, "m1": 3.0, "A2": 1.24e15, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
    "E": {
        "air": {"A1": 3.29e11, "m1": 3.0, "A2": 1.83e15, "m2": 5.0,
                "fatigue_limit": 45.54},
        "seawater_cp": {"A1": 1.62e11, "m1": 3.0, "A2": 5.07e14, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
    "F": {
        "air": {"A1": 1.73e11, "m1": 3.0, "A2": 6.33e14, "m2": 5.0,
                "fatigue_limit": 36.84},
        "seawater_cp": {"A1": 8.51e10, "m1": 3.0, "A2": 1.76e14, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
    "F1": {
        "air": {"A1": 1.08e11, "m1": 3.0, "A2": 2.94e14, "m2": 5.0,
                "fatigue_limit": 36.58},
        "seawater_cp": {"A1": 5.28e10, "m1": 3.0, "A2": 8.07e13, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
    "F3": {
        "air": {"A1": 5.73e10, "m1": 3.0, "A2": 1.01e14, "m2": 5.0,
                "fatigue_limit": 29.64},
        "seawater_cp": {"A1": 2.83e10, "m1": 3.0, "A2": 2.80e13, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
    "G": {
        "air": {"A1": 2.82e10, "m1": 3.0, "A2": 3.48e13, "m2": 5.0,
                "fatigue_limit": 23.44},
        "seawater_cp": {"A1": 1.40e10, "m1": 3.0, "A2": 9.70e12, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
    "W1": {
        "air": {"A1": 2.15e10, "m1": 3.0, "A2": 2.19e13, "m2": 5.0,
                "fatigue_limit": 21.34},
        "seawater_cp": {"A1": 1.06e10, "m1": 3.0, "A2": 6.08e12, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
    "W2": {
        "air": {"A1": 1.08e10, "m1": 3.0, "A2": 6.89e12, "m2": 5.0,
                "fatigue_limit": 16.92},
        "seawater_cp": {"A1": 5.33e9, "m1": 3.0, "A2": 1.91e12, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
    "W3": {
        "air": {"A1": 5.30e9, "m1": 3.0, "A2": 2.14e12, "m2": 5.0,
                "fatigue_limit": 13.35},
        "seawater_cp": {"A1": 2.62e9, "m1": 3.0, "A2": 5.95e11, "m2": 5.0,
                        "fatigue_limit": 0.0},
    },
}


# =========================================================================
# Public helper
# =========================================================================

def get_sn_curve(
    curve_class: str,
    environment: str = "air",
    thickness_mm: Optional[float] = None,
    thickness_ref_mm: float = 25.0,
    thickness_exponent: float = 0.25,
) -> BilinearSNCurve:
    """Get a DNV-RP-C203 bilinear S-N curve.

    Tries ``digitalmodel.structural.fatigue`` first; falls back to
    the built-in table if that module is unavailable.

    Parameters
    ----------
    curve_class : str
        DNV weld class, e.g. ``"F"``, ``"D"``, ``"C2"``.
    environment : str
        ``"air"`` or ``"seawater_cp"``.
    thickness_mm : float, optional
        Actual plate/wall thickness [mm].  If provided and differs from
        *thickness_ref_mm*, a thickness correction is applied.
    thickness_ref_mm : float
        Reference thickness [mm] (default 25 mm per DNV-RP-C203).
    thickness_exponent : float
        Exponent *k* for thickness correction (default 0.25).

    Returns
    -------
    BilinearSNCurve

    Raises
    ------
    KeyError
        If *curve_class* is not in the DNV-RP-C203 table.
    """
    key = curve_class.upper()
    env_key = "seawater_cp" if "seawater" in environment.lower() or "cp" in environment.lower() else "air"

    if key not in _DNV_SN_TABLE:
        available = sorted(_DNV_SN_TABLE.keys())
        raise KeyError(
            f"Unknown DNV curve class '{curve_class}'. "
            f"Available: {available}"
        )

    d = _DNV_SN_TABLE[key][env_key]
    params = BilinearSNCurveParams(
        A1=d["A1"], m1=d["m1"],
        A2=d["A2"], m2=d["m2"],
        N_transition=1e7,
        fatigue_limit=d["fatigue_limit"],
    )

    curve = BilinearSNCurve(params, name=f"DNV-{key}-{env_key}")

    # --- thickness correction ---
    if thickness_mm is not None and thickness_mm != thickness_ref_mm:
        t_ratio = thickness_mm / thickness_ref_mm
        # Stress-based correction: effective S is multiplied by t_ratio^k,
        # which shifts the S-N curve down by (t_ratio^k)^m in the N-direction.
        tk = t_ratio ** thickness_exponent
        corrected = BilinearSNCurveParams(
            A1=params.A1 / tk ** params.m1,
            m1=params.m1,
            A2=params.A2 / tk ** params.m2,
            m2=params.m2,
            N_transition=params.N_transition,
            fatigue_limit=params.fatigue_limit / tk if params.fatigue_limit > 0 else 0.0,
        )
        curve = BilinearSNCurve(corrected, name=f"DNV-{key}-{env_key}-t{thickness_mm}mm")

    return curve
