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
  measurement standard deviation (fraction of ``t``).  ``gamma_m``, ``gamma_d``
  and ``epsilon_d`` are selected from the DNV-RP-F101 Part-A tables by **safety
  class** (low/medium/high/'very high') and the **sizing-accuracy band**
  ``StD[d/t]`` (see
  :func:`gamma_m_factor`, :func:`gamma_d_factor`, :func:`epsilon_d_fractile`);
  any factor may still be overridden explicitly.

Interacting-defect colonies (:func:`dnv_f101_interacting`) follow DNV-RP-F101
Sec. 3.8.2: for a run of adjacent interacting defects the combined defect takes
the total length ``L_comb`` (defect lengths + interior spacings) and an
**area-averaged combined depth** ``d_comb = sum(d_i l_i)/L_comb`` — total
metal-loss area over the total combined length (gaps included), not the maximum
depth — and is then assessed as a single defect.

Units are US customary throughout: lengths in inches, stresses and pressures in
psi.  All formula constants follow DNV-RP-F101 (2015/2017) Sections 2 (PSF) and
3 (allowable-stress).  The default factors below are documented at their point
of use; callers may override every factor.

.. note::
   The allowable-stress ``usage_factor`` default 0.72 is the **ASME B31.8
   location-class-1 design factor**, *not* a DNV safety-class usage factor.  Use
   the PSF format (:func:`dnv_f101_psf`) with an explicit ``safety_class`` for
   DNV-RP-F101 code-compliant safety.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import List, Optional, Sequence, Tuple

import numpy as np

from digitalmodel.codes import DNV_RP_F101

# ---------------------------------------------------------------------------
# Material — API 5L specified minimum TENSILE strength (SMTS), psi.
# DNV-RP-F101 capacity uses f_u = SMTS (not SMYS).  Values are the API 5L /
# ISO 3183 PSL2 minimum UTS for each grade (rounded from MPa): X52 = 460 MPa,
# X60 = 520 MPa, X65 = 535 MPa, X70 = 570 MPa, X80 = 625 MPa.
# ---------------------------------------------------------------------------
SMTS_PSI = {
    "X42": 60_200.0,  # 415 MPa
    "X46": 63_100.0,  # 435 MPa
    "X52": 66_700.0,  # 460 MPa
    "X56": 71_100.0,  # 490 MPa
    "X60": 75_400.0,  # 520 MPa
    "X65": 77_600.0,  # 535 MPa
    "X70": 82_700.0,  # 570 MPa
    "X80": 90_600.0,  # 625 MPa
}

# Default usage factor for the allowable-stress format.
#
# NOTE on 0.72: this is the ASME B31.8 location-class-1 DESIGN FACTOR (hoop-stress
# usage factor for onshore transmission pipelines), NOT a DNV-RP-F101 safety-class
# usage factor.  DNV-RP-F101's own calibrated safety derives from the Part-A
# partial-safety-factor format (see ``dnv_f101_psf`` and the gamma_m table below),
# not from a single scalar usage factor.  We expose 0.72 only as a familiar
# allowable-stress screening level; for code-compliant DNV safety use the PSF
# format with the appropriate ``safety_class``.  Location class 2/3/4 would use
# 0.60/0.50/0.40 respectively (ASME B31.8), overridable via ``usage_factor``.
_DEFAULT_USAGE_FACTOR = 0.72

# ---------------------------------------------------------------------------
# DNV-RP-F101 Part-A partial-safety-factor (LRFD) tables.
#
# The Part-A factored capacity for a single longitudinal defect is
#
#     (d/t)*  = (d/t) + epsilon_d * StD[d/t]
#     P_corr  = gamma_m * (2 t f_u/(D-t)) * (1 - gamma_d (d/t)*)
#                                          / (1 - gamma_d (d/t)*/Q)
#
# with the factors selected by SAFETY CLASS and by the relative measurement
# accuracy band StD[d/t] (the sizing standard deviation as a fraction of t).
#
# gamma_m (model factor) — DNV-RP-F101 (Jan-2015) Table 3-2, by safety class
# (Low / Medium / High / Very High) and by whether the inspection sizes depth
# RELATIVE to t (e.g. MFL ILI) or ABSOLUTE (e.g. UT).  The legacy scalar default
# 0.77 == Very-High class / absolute sizing (NOT Normal — the prior code label
# was wrong; per Table 3-2 0.77 is the Very-High absolute value).
# ---------------------------------------------------------------------------
_GAMMA_M_TABLE = {
    # safety_class: {sizing_method: gamma_m}   (DNV-RP-F101 Table 3-2)
    "low": {"relative": 0.90, "absolute": 0.94},
    "medium": {"relative": 0.85, "absolute": 0.88},
    "high": {"relative": 0.80, "absolute": 0.82},
    "very high": {"relative": 0.76, "absolute": 0.77},
}

# Safety-class name normalisation.  DNV uses Low/Medium/High/Very High; "normal"
# is accepted as an alias for "medium" for callers who use that wording.
_SAFETY_CLASS_ALIASES = {
    "low": "low",
    "medium": "medium",
    "normal": "medium",
    "high": "high",
    "very high": "very high",
    "very_high": "very high",
    "veryhigh": "very high",
    "vhigh": "very high",
}

# Default DNV-RP-F101 PSF selection — Very-High safety class with absolute depth
# sizing, chosen because that pair reproduces the legacy scalar default
# gamma_m = 0.77 (DNV-RP-F101 Table 3-2).  Callers should set ``safety_class``
# to the pipeline's actual class rather than relying on this conservative
# default.
_DEFAULT_SAFETY_CLASS = "very high"
_DEFAULT_MEASUREMENT_METHOD = "absolute"


def _normalise_safety_class(safety_class: str) -> str:
    sc = safety_class.strip().lower()
    if sc not in _SAFETY_CLASS_ALIASES:
        raise ValueError(
            f"safety_class {safety_class!r} must be one of "
            f"low / medium / high / 'very high'."
        )
    return _SAFETY_CLASS_ALIASES[sc]


# Relative-depth measurement standard deviation (fraction of t); ~0.08 t is a
# typical high-resolution MFL ILI tolerance.  The Part-A gamma_d / epsilon_d
# tables below are calibrated over StD[d/t] in [0, 0.16].
_DEFAULT_STD_REL_DEPTH = 0.08
_DNV_F101_MAX_STD = 0.16

# DNV-RP-F101 validity limit on relative defect depth.  The capacity equation is
# calibrated for d/t up to 0.85; deeper defects are outside the qualified range
# and should be assessed by repair/replace criteria rather than extrapolated.
_DNV_F101_MAX_DT = 0.85


def gamma_m_factor(
    safety_class: str = _DEFAULT_SAFETY_CLASS,
    measurement_method: str = _DEFAULT_MEASUREMENT_METHOD,
) -> float:
    """DNV-RP-F101 (Jan-2015) Table 3-2 model partial safety factor ``gamma_m``.

    Looked up from the safety-class / sizing-method table.  ``safety_class`` is
    one of ``low``/``medium``/``high``/``very high`` (``normal`` is an alias for
    ``medium``); ``measurement_method`` is ``relative`` (depth sized as a
    fraction of t, e.g. MFL) or ``absolute`` (e.g. UT).
    """
    sc = _normalise_safety_class(safety_class)
    mm = measurement_method.strip().lower()
    if mm not in ("relative", "absolute"):
        raise ValueError(
            f"measurement_method {measurement_method!r} must be 'relative' or 'absolute'."
        )
    return _GAMMA_M_TABLE[sc][mm]


def epsilon_d_fractile(std_rel_depth: float) -> float:
    """DNV-RP-F101 Part-A fractile factor ``epsilon_d`` for the corrosion depth.

    The corrected relative depth is ``(d/t)* = (d/t) + epsilon_d * StD[d/t]``.
    Per the DNV-RP-F101 table (StD[d/t] denoted ``a``):

        a <= 0.04        -> epsilon_d = 0
        0.04 < a <= 0.16 -> epsilon_d = -1.33 + 37.5 a - 104.2 a**2

    (The quadratic gives epsilon_d ~ 0, 1.0, 2.0 at a = 0.04, 0.08, 0.16 — so
    the legacy scalar default epsilon_d = 1.0 corresponds to StD[d/t] = 0.08.)
    For ``a`` above the calibrated 0.16 the value is clamped at the 0.16 point.
    """
    a = std_rel_depth
    if a <= 0.04:
        return 0.0
    a = min(a, _DNV_F101_MAX_STD)
    return -1.33 + 37.5 * a - 104.2 * a * a


def gamma_d_factor(
    std_rel_depth: float, safety_class: str = _DEFAULT_SAFETY_CLASS
) -> float:
    """DNV-RP-F101 (Jan-2015) Table 3-8 depth partial safety factor ``gamma_d``.

    Tabulated as a function of the sizing standard deviation ``a = StD[d/t]``
    and the safety class:

        Low       : a < 0.04         -> 1.0 + 4.0 a
                    0.04 <= a < 0.08  -> 1.0 + 5.5 a - 37.5 a**2
                    0.08 <= a <= 0.16 -> 1.2
        Medium    : a <= 0.16         -> 1.0 + 4.6 a - 13.9 a**2
        High      : a <= 0.16         -> 1.0 + 4.3 a - 4.1 a**2
        Very High : a < 0.03          -> 1.0 + 4.0 a
                    0.03 <= a <= 0.16  -> 0.92 + 7.1 a - 8.3 a**2

    (``normal`` aliases ``medium``.)  Larger StD and higher safety class both
    raise ``gamma_d`` (more conservative); e.g. at a = 0.08 gamma_d is 1.20
    (Low) / 1.279 (Medium) / 1.318 (High) / 1.435 (Very High), matching the
    Table 3-7 tabulated points.  ``a`` above 0.16 is clamped to the 0.16 point.
    """
    a = min(max(std_rel_depth, 0.0), _DNV_F101_MAX_STD)
    sc = _normalise_safety_class(safety_class)
    if sc == "low":
        if a < 0.04:
            return 1.0 + 4.0 * a
        if a < 0.08:
            return 1.0 + 5.5 * a - 37.5 * a * a
        return 1.2
    if sc == "medium":
        return 1.0 + 4.6 * a - 13.9 * a * a
    if sc == "high":
        return 1.0 + 4.3 * a - 4.1 * a * a
    # very high
    if a < 0.03:
        return 1.0 + 4.0 * a
    return 0.92 + 7.1 * a - 8.3 * a * a


@dataclass
class DNVF101Result:
    """DNV-RP-F101 remaining-strength result for a corroded pipe defect."""

    method: str  # DNV-F101-AS | DNV-F101-PSF | ...-interacting
    capacity_pressure_psi: float  # predicted burst (capacity) pressure
    allowable_pressure_psi: float  # F * capacity (AS) or P_corr (PSF)
    Q: float  # length-correction factor
    d_over_t: float  # governing relative depth
    intact_pressure_psi: float  # 2 t f_u / (D - t) (defect-free)
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
    D: float,
    t: float,
    d: float,
    L: float,
    smts_psi: float,
    *,
    maop_psi: Optional[float] = None,
    usage_factor: float = _DEFAULT_USAGE_FACTOR,
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
        details={
            "usage_factor": usage_factor,
            "L_in": L,
            "smts_psi": f_u,
            "format": "allowable_stress",
            "within_applicability": bool(within),
            "applicability_note": (
                None
                if within
                else f"d/t={dt:.3f} exceeds DNV-RP-F101 validity limit "
                f"{_DNV_F101_MAX_DT:.2f}; assess by repair/replace criteria"
            ),
        },
    )


# ---------------------------------------------------------------------------
# Single defect — partial-safety-factor (LRFD) format
# ---------------------------------------------------------------------------
def dnv_f101_psf(
    D: float,
    t: float,
    d: float,
    L: float,
    smts_psi: float,
    *,
    maop_psi: Optional[float] = None,
    safety_class: str = _DEFAULT_SAFETY_CLASS,
    measurement_method: str = _DEFAULT_MEASUREMENT_METHOD,
    gamma_m: Optional[float] = None,
    gamma_d: Optional[float] = None,
    epsilon_d: Optional[float] = None,
    std_rel_depth: float = _DEFAULT_STD_REL_DEPTH,
) -> DNVF101Result:
    """DNV-RP-F101 partial-safety-factor (Part A / LRFD) corroded capacity.

    ``(d/t)* = (d/t) + epsilon_d * StD`` and

        P_corr = gamma_m * (2 t f_u/(D-t)) * (1 - gamma_d*(d/t)*)
                 / (1 - gamma_d*(d/t)*/Q).

    The partial safety factors are taken from the DNV-RP-F101 (Jan-2015) Part-A
    tables: ``gamma_m`` from :func:`gamma_m_factor` (by ``safety_class`` =
    low/medium/high/'very high' and ``measurement_method`` = relative/absolute),
    and ``gamma_d`` / ``epsilon_d`` from :func:`gamma_d_factor` /
    :func:`epsilon_d_fractile` (by ``safety_class`` and the sizing standard
    deviation ``std_rel_depth`` = StD[d/t], fraction of ``t``).  The defaults
    (Very-High class, absolute sizing, StD = 0.08) reproduce the historical
    scalar ``gamma_m = 0.77`` (which Table 3-2 places at Very-High/absolute, not
    Normal as the prior code assumed).  Any of ``gamma_m``/``gamma_d``/
    ``epsilon_d`` may be passed explicitly to override the table lookup.

    The PSF pressure is reported as the allowable pressure; the un-factored mean
    capacity is also returned for reference.
    """
    _validate(D, t, d, L)
    f_u = smts_psi
    dt = d / t
    if gamma_m is None:
        gamma_m = gamma_m_factor(safety_class, measurement_method)
    if gamma_d is None:
        gamma_d = gamma_d_factor(std_rel_depth, safety_class)
    if epsilon_d is None:
        epsilon_d = epsilon_d_fractile(std_rel_depth)
    dt_star = dt + epsilon_d * std_rel_depth
    dt_star = min(dt_star, 0.999)
    Q = length_correction_factor(D, t, L)
    intact = _intact_pressure(t, f_u, D)
    p_corr = (
        gamma_m * intact * (1.0 - gamma_d * dt_star) / (1.0 - gamma_d * dt_star / Q)
    )
    p_corr = max(p_corr, 0.0)
    p_cap = _capacity(t, f_u, D, dt, Q)  # un-factored mean capacity, for reference
    within_dt = dt <= _DNV_F101_MAX_DT
    within_std = std_rel_depth <= _DNV_F101_MAX_STD
    within = within_dt and within_std
    notes = []
    if not within_dt:
        notes.append(
            f"d/t={dt:.3f} exceeds DNV-RP-F101 validity limit {_DNV_F101_MAX_DT:.2f}; "
            f"assess by repair/replace criteria"
        )
    if not within_std:
        notes.append(
            f"StD[d/t]={std_rel_depth:.3f} exceeds the Part-A PSF calibration "
            f"range {_DNV_F101_MAX_STD:.2f}; gamma_d/epsilon_d clamped at 0.16"
        )
    return DNVF101Result(
        method="DNV-F101-PSF",
        capacity_pressure_psi=p_cap,
        allowable_pressure_psi=p_corr,
        Q=Q,
        d_over_t=dt,
        intact_pressure_psi=intact,
        acceptable=(None if maop_psi is None else bool(p_corr >= maop_psi)),
        details={
            "gamma_m": gamma_m,
            "gamma_d": gamma_d,
            "epsilon_d": epsilon_d,
            "std_rel_depth": std_rel_depth,
            "d_over_t_star": dt_star,
            "safety_class": _normalise_safety_class(safety_class),
            "measurement_method": measurement_method.strip().lower(),
            "L_in": L,
            "smts_psi": f_u,
            "format": "partial_safety_factor",
            "within_applicability": bool(within),
            "applicability_note": ("; ".join(notes) if notes else None),
        },
    )


# ---------------------------------------------------------------------------
# Interacting defects
# ---------------------------------------------------------------------------
def interaction_spacing_limit(D: float, t: float) -> float:
    """DNV-RP-F101 axial interaction limit: defects interact if the gap between
    them is shorter than ``2 * sqrt(D * t)``."""
    return 2.0 * math.sqrt(D * t)


def dnv_f101_interacting(
    D: float,
    t: float,
    defects: Sequence[Tuple[float, float, float]],
    smts_psi: float,
    *,
    maop_psi: Optional[float] = None,
    usage_factor: float = _DEFAULT_USAGE_FACTOR,
) -> DNVF101Result:
    """Assess a colony of longitudinal defects with the DNV-RP-F101 interaction rule.

    Args:
        defects: list of ``(position_in, length_in, depth_in)`` tuples, where
            ``position_in`` is the axial start coordinate of each defect.
        usage_factor / maop_psi: as for :func:`dnv_f101_single_defect`.

    Adjacent defects whose axial gap is smaller than ``2*sqrt(D*t)`` are merged
    into a composite defect per DNV-RP-F101 Sec. 3.8.2 (combined-defect rule):

        L_comb (Step 7) = l_m + sum_{i=n..m-1} (l_i + s_i)  = total span from the
            first start to the last end, INCLUDING the inter-defect spacings s_i.
        d_comb (Step 8) = sum(d_i * l_i) / L_comb           = total metal-loss
            area divided by the TOTAL combined length (gaps included, so the
            spacings dilute the average depth).

    The composite is assessed as a single defect with ``(L_comb, d_comb)``.
    Because the same total span feeds both Q (length correction) and the depth
    average, the two are consistent.  Every individual defect and every
    interacting composite (each contiguous sub-run) is evaluated; the governing
    (lowest-capacity) case is returned.  This area-averaging replaces the earlier
    max-depth grouping, which was conservative but mislabelled as the DNV method.
    Equal-depth members reduce to the max depth only when they are touching
    (zero spacing); a non-zero gap dilutes the combined depth below the members'.
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
            D, t, depths[k], lengths[k], smts_psi, usage_factor=usage_factor
        )
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
            # DNV-RP-F101 Sec. 3.8.2 Step 8 area-averaged combined depth:
            # total metal-loss area (sum d_i*l_i) divided by the TOTAL combined
            # length L_comb (== comp_length, which INCLUDES the interior gaps).
            # The same span feeds Q above, keeping length and depth consistent.
            member_lengths = lengths[i : j + 1]
            member_depths = depths[i : j + 1]
            if comp_length > 0.0:
                comp_depth = float(np.sum(member_depths * member_lengths) / comp_length)
            else:  # degenerate zero-length span; fall back to mean depth
                comp_depth = float(np.mean(member_depths))
            res = dnv_f101_single_defect(
                D, t, comp_depth, comp_length, smts_psi, usage_factor=usage_factor
            )
            candidates.append(
                {
                    "res": res,
                    "kind": "composite",
                    "members": list(range(i, j + 1)),
                    "comp_length_in": comp_length,
                    "comp_depth_in": comp_depth,
                }
            )

    governing = min(candidates, key=lambda c: c["res"].capacity_pressure_psi)
    gres = governing["res"]
    gov_within = bool(gres.details.get("within_applicability", True))
    gov_details = {
        "interaction_limit_in": limit,
        "governing_kind": governing["kind"],
        "governing_members": governing["members"],
        "n_defects": int(n),
        "usage_factor": usage_factor,
        "n_candidates": int(len(candidates)),
        "within_applicability": gov_within,
        "applicability_note": gres.details.get("applicability_note"),
    }
    if governing["kind"] == "composite":
        gov_details["comp_length_in"] = governing["comp_length_in"]
        gov_details["comp_depth_in"] = governing["comp_depth_in"]
    return DNVF101Result(
        method="DNV-F101-interacting",
        capacity_pressure_psi=gres.capacity_pressure_psi,
        allowable_pressure_psi=gres.allowable_pressure_psi,
        Q=gres.Q,
        d_over_t=gres.d_over_t,
        intact_pressure_psi=gres.intact_pressure_psi,
        acceptable=(
            None if maop_psi is None else bool(gres.allowable_pressure_psi >= maop_psi)
        ),
        details=gov_details,
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
