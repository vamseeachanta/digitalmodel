# ABOUTME: DNV-RP-F101 corroded-pipeline remaining-strength for a single
# ABOUTME: longitudinal metal-loss defect (allowable-stress + PSF) and interacting defects.
"""DNV-RP-F101 corroded-pipeline burst-capacity assessment.

Implements DNV-RP-F101 (Recommended Practice, Corroded Pipelines) for a single
longitudinal corrosion defect under internal pressure, plus the standard's
defect-interaction rule for colonies of defects.

Two formats are provided:

* **Allowable-stress design (Part B / "Calibrated Safety Factor" simplified
  form)** — the capacity (burst) pressure of a single defect:

      P_cap = (2 * t * f_u / (D - t)) * (1 - (d/t)) / (1 - (d/t) / Q)
      Q     = sqrt(1 + 0.31 * (L / sqrt(D * t))**2)

  where ``f_u`` is the ultimate tensile strength (SMTS), ``d`` the defect depth,
  ``t`` the wall thickness, ``L`` the axial defect length and ``D`` the outside
  diameter.  ``Q`` is the DNV length-correction (bulging) factor.  The allowable
  operating pressure applies a usage factor ``F``:

      P_allow = F * P_cap

* **Partial-safety-factor design (Part A / LRFD)** — the same geometry with
  DNV partial safety factors and a measurement-tolerance allowance on the
  relative depth:

      (d/t)*  = (d/t) + epsilon_d * StD
      P_corr  = gamma_m * (2 t f_u / (D - t))
                * (1 - gamma_d*(d/t)*) / (1 - gamma_d*(d/t)*/Q)

  with ``gamma_m`` the partial safety factor on the model/capacity,
  ``gamma_d`` the factor on the corrected defect depth, ``epsilon_d`` the depth
  factor on the measurement standard deviation and ``StD`` the relative-depth
  measurement standard deviation (fraction of ``t``).

Units are US customary throughout: lengths in inches, stresses and pressures in
psi.  All formula constants follow DNV-RP-F101 (2015/2017) Sections 2 (PSF) and
3 (allowable-stress).  The default factors below are documented at their point
of use; callers may override every factor.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import List, Optional, Sequence, Tuple

import numpy as np

from digitalmodel.codes import DNV_RP_F101
from digitalmodel.materials import legacy_smts_psi_dict

# ---------------------------------------------------------------------------
# Material — API 5L specified minimum TENSILE strength (SMTS), psi.
# DNV-RP-F101 capacity uses f_u = SMTS (not SMYS).  Values are the API 5L /
# ISO 3183 PSL2 minimum UTS for each grade (rounded from MPa): X52 = 460 MPa,
# X60 = 520 MPa, X65 = 535 MPa, X70 = 570 MPa, X80 = 625 MPa.
# Sourced from the canonical grade matrix (issue #1089), which reproduces these
# psi values exactly (PSL2 SMTS MPa -> psi, rounded to the nearest 100 psi).
# ---------------------------------------------------------------------------
SMTS_PSI = legacy_smts_psi_dict()

# Default usage factor for the allowable-stress format.  DNV-RP-F101 allowable
# operating pressure for the longitudinal-stress / hoop-controlled case is
# commonly taken at the design-factor level (~0.72 for transmission pipelines,
# ASME B31.8 location class 1); overridable per safety class / location.
_DEFAULT_USAGE_FACTOR = 0.72

# DNV-RP-F101 partial-safety-factor defaults (allowable-stress -> LRFD).
# gamma_m  : partial safety factor on the predicted capacity, by safety class
#            (DNV-RP-F101 Table — Normal class ~0.77, High class ~0.74 for the
#            usual inspection-accuracy band).  Default = Normal safety class.
# gamma_d  : factor on the corrected relative depth (depends on measurement
#            accuracy StD; ~1.0 for very accurate, up to ~1.2 for poorer ILI).
# epsilon_d: depth factor applied to the relative-depth standard deviation
#            (DNV uses 1.0 at the standard fractile for relative-depth sizing).
# StD      : relative-depth measurement standard deviation (fraction of t);
#            ~0.08 t is a typical high-resolution MFL ILI tolerance.
_DEFAULT_GAMMA_M = 0.77
_DEFAULT_GAMMA_D = 1.0
_DEFAULT_EPSILON_D = 1.0
_DEFAULT_STD_REL_DEPTH = 0.08

# DNV-RP-F101 validity limit on relative defect depth.  The capacity equation is
# calibrated for d/t up to 0.85; deeper defects are outside the qualified range
# and should be assessed by repair/replace criteria rather than extrapolated.
_DNV_F101_MAX_DT = 0.85


@dataclass
class DNVF101Result:
    """DNV-RP-F101 remaining-strength result for a corroded pipe defect."""

    method: str                       # DNV-F101-AS | DNV-F101-PSF | ...-interacting
    capacity_pressure_psi: float      # predicted burst (capacity) pressure
    allowable_pressure_psi: float     # F * capacity (AS) or P_corr (PSF)
    Q: float                          # length-correction factor
    d_over_t: float                   # governing relative depth
    intact_pressure_psi: float        # 2 t f_u / (D - t) (defect-free)
    acceptable: Optional[bool] = None  # allowable >= MAOP (if maop_psi given)
    details: dict = field(default_factory=dict)
    code_reference: str = DNV_RP_F101.label  # governing code (all formats)


# ---------------------------------------------------------------------------
# Core factors
# ---------------------------------------------------------------------------
def length_correction_factor(D: float, t: float, L: float) -> float:
    """DNV-RP-F101 length-correction (bulging) factor Q.

    ``Q = sqrt(1 + 0.31 * (L / sqrt(D*t))**2)``.
    """
    return math.sqrt(1.0 + 0.31 * (L / math.sqrt(D * t)) ** 2)


def _intact_pressure(t: float, f_u: float, D: float) -> float:
    """Defect-free hoop capacity ``2 t f_u / (D - t)``."""
    return 2.0 * t * f_u / (D - t)


def _capacity(t: float, f_u: float, D: float, dt: float, Q: float) -> float:
    """DNV-RP-F101 single-defect capacity for relative depth ``dt`` and ``Q``."""
    dt = min(dt, 0.999)  # guard full-penetration singularity
    return _intact_pressure(t, f_u, D) * (1.0 - dt) / (1.0 - dt / Q)


# ---------------------------------------------------------------------------
# Single defect — allowable-stress format
# ---------------------------------------------------------------------------
def dnv_f101_single_defect(
    D: float, t: float, d: float, L: float, smts_psi: float,
    *, maop_psi: Optional[float] = None, usage_factor: float = _DEFAULT_USAGE_FACTOR,
) -> DNVF101Result:
    """DNV-RP-F101 allowable-stress single-defect capacity and allowable pressure.

    Args:
        D: pipe outside diameter (in).
        t: nominal wall thickness (in).
        d: maximum defect depth (in).
        L: axial defect length (in).
        smts_psi: ultimate tensile strength f_u = SMTS (psi).
        maop_psi: maximum allowable operating pressure for an accept/reject flag.
        usage_factor: F applied to the capacity for the allowable pressure.
    """
    _validate(D, t, d, L)
    f_u = smts_psi
    dt = d / t
    Q = length_correction_factor(D, t, L)
    p_cap = _capacity(t, f_u, D, dt, Q)
    p_allow = usage_factor * p_cap
    within = dt <= _DNV_F101_MAX_DT
    return DNVF101Result(
        method="DNV-F101-AS",
        capacity_pressure_psi=p_cap,
        allowable_pressure_psi=p_allow,
        Q=Q,
        d_over_t=dt,
        intact_pressure_psi=_intact_pressure(t, f_u, D),
        acceptable=(None if maop_psi is None else bool(p_allow >= maop_psi)),
        details={"usage_factor": usage_factor, "L_in": L,
                 "smts_psi": f_u, "format": "allowable_stress",
                 "within_applicability": bool(within),
                 "applicability_note": (
                     None if within else
                     f"d/t={dt:.3f} exceeds DNV-RP-F101 validity limit "
                     f"{_DNV_F101_MAX_DT:.2f}; assess by repair/replace criteria")},
    )


# ---------------------------------------------------------------------------
# Single defect — partial-safety-factor (LRFD) format
# ---------------------------------------------------------------------------
def dnv_f101_psf(
    D: float, t: float, d: float, L: float, smts_psi: float,
    *, maop_psi: Optional[float] = None,
    gamma_m: float = _DEFAULT_GAMMA_M, gamma_d: float = _DEFAULT_GAMMA_D,
    epsilon_d: float = _DEFAULT_EPSILON_D, std_rel_depth: float = _DEFAULT_STD_REL_DEPTH,
) -> DNVF101Result:
    """DNV-RP-F101 partial-safety-factor (Part A / LRFD) corroded capacity.

    ``(d/t)* = (d/t) + epsilon_d * StD`` and

        P_corr = gamma_m * (2 t f_u/(D-t)) * (1 - gamma_d*(d/t)*)
                 / (1 - gamma_d*(d/t)*/Q).

    Args mirror :func:`dnv_f101_single_defect`; the partial safety factors and
    the relative-depth measurement standard deviation ``std_rel_depth`` (fraction
    of ``t``) carry DNV defaults (see module constants) and are overridable.
    The PSF pressure is reported as the allowable pressure; the un-factored
    capacity is also returned for reference.
    """
    _validate(D, t, d, L)
    f_u = smts_psi
    dt = d / t
    dt_star = dt + epsilon_d * std_rel_depth
    dt_star = min(dt_star, 0.999)
    Q = length_correction_factor(D, t, L)
    intact = _intact_pressure(t, f_u, D)
    p_corr = gamma_m * intact * (1.0 - gamma_d * dt_star) / (1.0 - gamma_d * dt_star / Q)
    p_corr = max(p_corr, 0.0)
    p_cap = _capacity(t, f_u, D, dt, Q)  # un-factored mean capacity, for reference
    return DNVF101Result(
        method="DNV-F101-PSF",
        capacity_pressure_psi=p_cap,
        allowable_pressure_psi=p_corr,
        Q=Q,
        d_over_t=dt,
        intact_pressure_psi=intact,
        acceptable=(None if maop_psi is None else bool(p_corr >= maop_psi)),
        details={"gamma_m": gamma_m, "gamma_d": gamma_d, "epsilon_d": epsilon_d,
                 "std_rel_depth": std_rel_depth, "d_over_t_star": dt_star,
                 "L_in": L, "smts_psi": f_u, "format": "partial_safety_factor"},
    )


# ---------------------------------------------------------------------------
# Interacting defects
# ---------------------------------------------------------------------------
def interaction_spacing_limit(D: float, t: float) -> float:
    """DNV-RP-F101 axial interaction limit: defects interact if the gap between
    them is shorter than ``2 * sqrt(D * t)``."""
    return 2.0 * math.sqrt(D * t)


def dnv_f101_interacting(
    D: float, t: float, defects: Sequence[Tuple[float, float, float]],
    smts_psi: float, *, maop_psi: Optional[float] = None,
    usage_factor: float = _DEFAULT_USAGE_FACTOR,
) -> DNVF101Result:
    """Assess a colony of longitudinal defects with the DNV-RP-F101 interaction rule.

    Args:
        defects: list of ``(position_in, length_in, depth_in)`` tuples, where
            ``position_in`` is the axial start coordinate of each defect.
        usage_factor / maop_psi: as for :func:`dnv_f101_single_defect`.

    Adjacent defects whose axial gap is smaller than ``2*sqrt(D*t)`` are merged
    into a composite defect spanning from the first start to the last end
    (combined length) with a grouped (maximum) depth, then assessed as a single
    defect.  Every individual defect and every interacting composite is
    evaluated; the governing (lowest-capacity) case is returned.
    """
    _validate_geometry(D, t)
    if not defects:
        raise ValueError("defects must be a non-empty sequence.")

    items = []
    for pos, length, depth in defects:
        if length < 0:
            raise ValueError(f"defect length {length} must be >= 0.")
        if not (0.0 <= depth <= t):
            raise ValueError(f"defect depth {depth} must be in [0, t={t}].")
        items.append((float(pos), float(length), float(depth)))
    # Sort by axial start position.
    items.sort(key=lambda it: it[0])
    starts = np.array([it[0] for it in items], dtype=float)
    lengths = np.array([it[1] for it in items], dtype=float)
    depths = np.array([it[2] for it in items], dtype=float)
    ends = starts + lengths
    limit = interaction_spacing_limit(D, t)

    candidates: List[dict] = []

    # 1) Every individual defect on its own.
    for k in range(len(items)):
        res = dnv_f101_single_defect(
            D, t, depths[k], lengths[k], smts_psi, usage_factor=usage_factor)
        candidates.append({"res": res, "kind": "single", "members": [k]})

    # 2) Every maximal run of consecutive interacting defects, and each
    #    contiguous sub-run within it, merged into a composite.
    n = len(items)
    for i in range(n):
        for j in range(i + 1, n):
            # gap between defect (j-1) and defect j along the run
            gap = starts[j] - ends[j - 1]
            if gap >= limit:
                break  # run broken; no further j interacts with i..
            comp_length = float(ends[j] - starts[i])
            comp_depth = float(np.max(depths[i:j + 1]))
            res = dnv_f101_single_defect(
                D, t, comp_depth, comp_length, smts_psi, usage_factor=usage_factor)
            candidates.append(
                {"res": res, "kind": "composite", "members": list(range(i, j + 1)),
                 "comp_length_in": comp_length, "comp_depth_in": comp_depth})

    governing = min(candidates, key=lambda c: c["res"].capacity_pressure_psi)
    gres = governing["res"]
    return DNVF101Result(
        method="DNV-F101-interacting",
        capacity_pressure_psi=gres.capacity_pressure_psi,
        allowable_pressure_psi=gres.allowable_pressure_psi,
        Q=gres.Q,
        d_over_t=gres.d_over_t,
        intact_pressure_psi=gres.intact_pressure_psi,
        acceptable=(None if maop_psi is None
                    else bool(gres.allowable_pressure_psi >= maop_psi)),
        details={"interaction_limit_in": limit, "governing_kind": governing["kind"],
                 "governing_members": governing["members"],
                 "n_defects": int(n), "usage_factor": usage_factor,
                 "n_candidates": int(len(candidates))},
    )


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------
def _validate_geometry(D: float, t: float) -> None:
    if D <= 0 or t <= 0 or t >= D:
        raise ValueError(f"Invalid geometry: D={D}, t={t} (need D > t > 0).")


def _validate(D: float, t: float, d: float, L: float) -> None:
    _validate_geometry(D, t)
    if not (0.0 <= d <= t):
        raise ValueError(f"defect depth d={d} must be in [0, t={t}].")
    if L < 0:
        raise ValueError(f"defect length L={L} must be >= 0.")
