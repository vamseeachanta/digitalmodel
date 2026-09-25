"""
Bilinear S-N curve for DNV-RP-C203 fatigue assessment (free spans).

The curve parameters come from :mod:`digitalmodel.fatigue.c203_sn_tables`,
verified against DNV-RP-C203 (October 2011) Tables 2-1 and 2-2 (#2165). This
module keeps no copy of its own.

Bilinear S-N model:
    N = A1 × S^(−m1)     for S ≥ S_transition   (high stress)
    N = A2 × S^(−m2)     for S <  S_transition   (low stress)

The transition stress is computed from the first slope at the knee:

    S_transition = (A1 / N_transition)^(1/m1)

Knee per environment (2011 Sec 2.4):
    IN_AIR         — N_transition = 1e7; A1, A2 from Table 2-1
    SEAWATER_CP    — N_transition = 1e6; A1 from Table 2-2, and the same second
                     segment (A2) as in air

The curve object keeps the in-air tabulated fatigue limit as a cut-off (a
screening convention for the constant-amplitude VIV check) and no cut-off for
seawater with CP; :class:`.SpanFatigueDamage` uses the selected curve's own
limit and never carries the in-air limit to seawater with CP (#2165).

Thickness correction (DNV-RP-C203 Sec 2.4.3), applied only above t_ref:
    S_corrected = S × (t / t_ref)^k
    where t_ref = 25 mm (default) and k is the class exponent from the tables
    unless given.
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
# DNV-RP-C203 curve parameters, from fatigue.c203_sn_tables (#2165)
# =========================================================================


def _dnv_params(curve_class: str, env_key: str) -> BilinearSNCurveParams:
    """Curve parameters for one class and environment from the verified tables."""
    # Imported here so that importing the free-span package stays light.
    from digitalmodel.fatigue import c203_sn_tables as c203

    p = c203.BILINEAR[curve_class]
    if env_key == "air":
        log_a1, n_knee, limit = p.log_a1_air, c203.N_KNEE_AIR, p.fatigue_limit_mpa
    else:
        log_a1, n_knee, limit = p.log_a1_cp, c203.N_KNEE_SEAWATER_CP, 0.0
    return BilinearSNCurveParams(
        A1=10.0**log_a1,
        m1=p.m1,
        A2=10.0**p.log_a2,
        m2=p.m2,
        N_transition=n_knee,
        fatigue_limit=limit,
    )


def _class_thickness_exponent(curve_class: str) -> float:
    from digitalmodel.fatigue import c203_sn_tables as c203

    return c203.BILINEAR[curve_class].k


def _available_classes() -> list[str]:
    from digitalmodel.fatigue import c203_sn_tables as c203

    return list(c203.CLASSES)


# =========================================================================
# Public helper
# =========================================================================


def get_sn_curve(
    curve_class: str,
    environment: str = "air",
    thickness_mm: Optional[float] = None,
    thickness_ref_mm: float = 25.0,
    thickness_exponent: Optional[float] = None,
) -> BilinearSNCurve:
    """Get a DNV-RP-C203 bilinear S-N curve.

    Parameters come from :mod:`digitalmodel.fatigue.c203_sn_tables`.

    Parameters
    ----------
    curve_class : str
        DNV weld class, e.g. ``"F"``, ``"D"``, ``"C2"``.
    environment : str
        ``"air"`` or ``"seawater_cp"``.
    thickness_mm : float, optional
        Actual plate/wall thickness [mm].  If provided and greater than
        *thickness_ref_mm*, a thickness correction is applied.
    thickness_ref_mm : float
        Reference thickness [mm] (default 25 mm per DNV-RP-C203).
    thickness_exponent : float, optional
        Exponent *k* for thickness correction; ``None`` (default) takes the
        class value from DNV-RP-C203 (#2165).

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

    available = _available_classes()
    if key not in available:
        raise KeyError(
            f"Unknown DNV curve class '{curve_class}'. "
            f"Available: {sorted(available)}"
        )

    params = _dnv_params(key, env_key)

    curve = BilinearSNCurve(params, name=f"DNV-{key}-{env_key}")

    # --- thickness correction ---
    if thickness_mm is not None and thickness_mm > thickness_ref_mm:
        if thickness_exponent is None:
            thickness_exponent = _class_thickness_exponent(key)
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
