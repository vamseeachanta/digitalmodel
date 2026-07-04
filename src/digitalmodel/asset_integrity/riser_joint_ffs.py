# ABOUTME: Drilling-riser joint FFS — Level-1 flaw-acceptance envelopes, collapse-
# ABOUTME: limited water depth, string fatigue-zone placement and fleet rollup.
"""Drilling-riser joint fitness-for-service (#1292).

Life extension of marine drilling-riser joints: a corroded/flawed joint stays in
service — in the right string position — instead of being pulled for repair.
Composes the validated engines only; no new physics:

- **Level-1 flaw-acceptance envelope** — max acceptable surface-flaw length vs
  depth at the joint's design pressure.  Base metal is inverted from the
  golden-tested remaining-strength methods (:mod:`ffs_acceptance_curves` /
  :mod:`corroded_pipe`); the weld region is inverted from the BS 7910:2013
  Option-1 failure-assessment diagram (:mod:`assessment.crack_fad`, #1270) —
  semi-elliptical surface flaw, Newman-Raju K, Annex P reference stress —
  with membrane stress defaulting to the Barlow hoop stress p*D/(2t) at the
  design pressure (bending 0 unless supplied) and toughness defaulting to the
  BS 7910 Annex J lower bound at CVN 27 J when no weld-metal toughness is
  given.  Practice depth caps (0.85*WT base / 0.60*WT weld) apply ON TOP of
  the computed envelopes; a campaign-end variant reserves growth allowance
  (corrosion rate x campaign duration), so a flaw accepted today is still
  acceptable when the joint is pulled.
- **Collapse-limited water depth** — API RP 1111 hydrostatic collapse
  (p_el / p_y interaction, identical equations to
  ``structural.analysis.wall_thickness_codes.api_rp_1111``) applied at the
  measured minimum wall to give the deepest water the joint may run in.
- **String-zone placement** — the riser string is banded top/mid/bottom by
  fatigue severity (top/bottom high: vessel and seabed interfaces; mid low).
  Placement rules follow the source integrity program: a joint unfit for even
  the low-fatigue band AND not strong enough at half the campaign water depth
  is flagged for repair; a configurable minimum count of high-fatigue-
  qualified joints is maintained in inventory.
- **Fleet rollup** — campaign-by-campaign joint status counts from a GML
  results register (the schema of
  ``tests/asset_integrity/test_data/real_inspection/gml_results_register.csv``).

The fatigue-severity margins are practice parameters (defaults from the source
program) pending a fatigue flaw-growth clock (follow-up to #1270); the
strength, fracture and remaining-life legs are code-anchored.
Units: inches / psi / feet / years (crack_fad works in mm / MPa; converted
internally at 25.4 mm/in and 145.038 psi/MPa).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional

import pandas as pd

from .assessment.crack_fad import assess_crack_like_flaw, kmat_from_charpy
from .corroded_pipe import SMYS_PSI
from .dnv_rp_f101 import SMTS_PSI
from .ffs_acceptance_curves import _max_acceptable_length

# Seawater hydrostatic gradient (psi per ft of depth) — 64 lb/ft3 seawater.
SEAWATER_PSI_PER_FT = 0.4444

# Practice depth caps from the source riser-IM program: acceptance-chart depth
# never exceeds these wall-thickness fractions regardless of computed strength.
DEPTH_CAP_BASE_METAL = 0.85
DEPTH_CAP_WELD = 0.60

# API RP 1111 collapse design factor (seamless/ERW pipe).
COLLAPSE_DESIGN_FACTOR = 0.70

# Unit conversions between this module (inches/psi) and crack_fad (mm/MPa).
MM_PER_IN = 25.4
PSI_PER_MPA = 145.038

#: Default weld-metal Charpy energy (J) when neither ``weld_k_mat`` nor
#: ``weld_cvn_joules`` is supplied: 27 J is the classic minimum impact
#: requirement of line-pipe / offshore structural specs, run through the
#: BS 7910 Annex J lower-bound correlation — a deliberately conservative
#: stand-in for an untested weld.  Supply measured toughness when available.
DEFAULT_WELD_CVN_JOULES = 27.0

STEEL_E_PSI = 30.0e6
STEEL_POISSON = 0.30


# ---------------------------------------------------------------------------
# Level-1 flaw-acceptance envelope
# ---------------------------------------------------------------------------
def _max_acceptable_crack_length_in(
    a_in: float,
    wt_in: float,
    sigma_m_mpa: float,
    sigma_b_mpa: float,
    sigma_y_mpa: float,
    sigma_u_mpa: float,
    k_mat: Optional[float],
    cvn_joules: Optional[float],
    max_length_in: float,
) -> float:
    """Largest FAD-acceptable total surface-flaw length at a fixed depth.

    Bisects flaw length against :func:`assessment.crack_fad.assess_crack_like_flaw`
    (a longer flaw is strictly worse at fixed depth: both K and the Annex P
    reference stress grow with length).  The shortest assessable flaw is
    semicircular (L = 2a — the Newman-Raju a/c <= 1 validity limit); if even
    that is unacceptable, returns 0.0 (conservative, mirrors the metal-loss
    inversion's vanishing-flaw check).
    """
    a_mm = a_in * MM_PER_IN
    t_mm = wt_in * MM_PER_IN

    def ok(length_in: float) -> bool:
        return assess_crack_like_flaw(
            a_mm=a_mm, c_mm=length_in * MM_PER_IN / 2.0, t_mm=t_mm,
            sigma_m_mpa=sigma_m_mpa, sigma_b_mpa=sigma_b_mpa,
            sigma_y_mpa=sigma_y_mpa, sigma_u_mpa=sigma_u_mpa,
            k_mat=k_mat, cvn_joules=cvn_joules,
        ).acceptable

    lo = 2.0 * a_in
    if lo > max_length_in or not ok(lo):
        return 0.0
    if ok(max_length_in):
        return max_length_in
    hi = max_length_in
    for _ in range(60):
        mid = 0.5 * (lo + hi)
        if ok(mid):
            lo = mid
        else:
            hi = mid
    return lo


def level1_flaw_envelope(
    od_in: float,
    wt_in: float,
    grade: str,
    design_pressure_psi: float,
    *,
    region: str = "base",
    method: str = "modified_b31g",
    corrosion_rate_in_per_yr: float = 0.0,
    campaign_years: float = 0.0,
    depth_fracs: Optional[list] = None,
    max_length_in: float = 40.0,
    weld_sigma_m_mpa: Optional[float] = None,
    weld_sigma_b_mpa: float = 0.0,
    weld_k_mat: Optional[float] = None,
    weld_cvn_joules: Optional[float] = None,
) -> dict:
    """Acceptable surface-flaw length vs depth chart for one joint region.

    Base metal inverts the ``method`` metal-loss engine (Modified B31G by
    default).  The weld region inverts the BS 7910 Option-1 FAD
    (:mod:`assessment.crack_fad`, #1270), treating the charted flaw as a
    semi-elliptical surface crack; the 0.60*WT practice cap is applied ON TOP
    of the fracture envelope.  Weld assumptions (all overridable):

    - membrane stress ``weld_sigma_m_mpa`` defaults to the Barlow hoop stress
      p*D/(2t) at ``design_pressure_psi`` (thin-wall, OD basis — the same
      pressure-containment state the base-metal envelope protects);
    - ``weld_sigma_b_mpa`` (through-wall bending, e.g. riser bending at the
      flaw) defaults to 0 — supply the project value when bending governs;
    - toughness: pass measured ``weld_k_mat`` (MPa*sqrt(m)), or
      ``weld_cvn_joules`` for the BS 7910 Annex J lower-bound correlation;
      with neither, a conservative CVN = 27 J default is used
      (:data:`DEFAULT_WELD_CVN_JOULES`);
    - yield/tensile from the grade dicts (SMYS/SMTS, API 5L PSL2).

    ``campaign_years`` > 0 gives the campaign-*end* envelope: each charted
    depth is assessed at ``d + corrosion_rate * campaign_years`` so the flaw
    still passes after growing through the campaign.

    Returns dict with ``depth_in`` / ``depth_frac`` / ``max_acceptable_length_in``
    (0.0 where the region cap or the strength/fracture check excludes the
    depth); weld results also carry the stress/toughness basis used.
    """
    if region not in ("base", "weld"):
        raise ValueError(f"region must be 'base' or 'weld', got '{region}'.")
    if grade not in SMYS_PSI:
        raise ValueError(f"unknown grade '{grade}'. Use one of {sorted(SMYS_PSI)}.")
    cap = DEPTH_CAP_BASE_METAL if region == "base" else DEPTH_CAP_WELD
    growth_in = corrosion_rate_in_per_yr * campaign_years
    depth_fracs = depth_fracs or [round(0.05 * i, 2) for i in range(1, 18)]  # 0.05..0.85

    weld_basis: dict = {}
    if region == "weld":
        sigma_m = (weld_sigma_m_mpa if weld_sigma_m_mpa is not None
                   else design_pressure_psi * od_in / (2.0 * wt_in) / PSI_PER_MPA)
        sigma_y = SMYS_PSI[grade] / PSI_PER_MPA
        sigma_u = SMTS_PSI[grade] / PSI_PER_MPA
        cvn = weld_cvn_joules
        if weld_k_mat is None and cvn is None:
            cvn = DEFAULT_WELD_CVN_JOULES
        k_mat_used = (weld_k_mat if weld_k_mat is not None
                      else kmat_from_charpy(cvn, thickness_mm=wt_in * MM_PER_IN))
        weld_basis = {
            "weld_sigma_m_mpa": round(sigma_m, 2),
            "weld_sigma_b_mpa": weld_sigma_b_mpa,
            "weld_k_mat_mpa_sqrt_m": round(k_mat_used, 1),
            "weld_cvn_joules": cvn,
        }

    depth_in, max_len = [], []
    for frac in depth_fracs:
        d = frac * wt_in
        d_end = d + growth_in
        if frac > cap or (d_end / wt_in) > cap or d_end >= wt_in:
            length = 0.0
        elif region == "weld":
            length = _max_acceptable_crack_length_in(
                d_end, wt_in, sigma_m, weld_sigma_b_mpa, sigma_y, sigma_u,
                weld_k_mat, cvn, max_length_in,
            )
        else:
            length = _max_acceptable_length(
                method, od_in, wt_in, d_end, grade, design_pressure_psi, max_length_in
            )
        depth_in.append(round(d, 4))
        max_len.append(round(length, 3))
    return {
        "domain": "riser_joint_flaw_envelope",
        "od_in": od_in, "wt_in": wt_in, "grade": grade,
        "method": "bs7910_option1_fad" if region == "weld" else method,
        "design_pressure_psi": design_pressure_psi,
        "region": region, "depth_cap_frac": cap,
        "corrosion_rate_in_per_yr": corrosion_rate_in_per_yr,
        "campaign_years": campaign_years,
        "depth_frac": list(depth_fracs), "depth_in": depth_in,
        "max_acceptable_length_in": max_len,
        **weld_basis,
    }


# ---------------------------------------------------------------------------
# Collapse-limited water depth (API RP 1111 equations)
# ---------------------------------------------------------------------------
def collapse_pressure_psi(od_in: float, wt_in: float, smys_psi: float) -> dict:
    """API RP 1111 collapse pressure: elastic/yield interaction.

    p_el = 2E(t/D)^3/(1-nu^2);  p_y = 2*SMYS*t/D;
    p_c = p_el*p_y/sqrt(p_el^2 + p_y^2).
    Same equations as ``wall_thickness_codes.api_rp_1111._check_collapse``.
    """
    if od_in <= 0 or wt_in <= 0 or wt_in >= od_in / 2:
        raise ValueError("require 0 < wt < OD/2.")
    ratio = wt_in / od_in
    p_el = 2.0 * STEEL_E_PSI * ratio**3 / (1.0 - STEEL_POISSON**2)
    p_y = 2.0 * smys_psi * ratio
    p_c = p_el * p_y / math.sqrt(p_el**2 + p_y**2)
    return {"p_el_psi": p_el, "p_y_psi": p_y, "p_c_psi": p_c}


def acceptable_water_depth_ft(
    od_in: float,
    wt_min_in: float,
    grade: str,
    *,
    design_factor: float = COLLAPSE_DESIGN_FACTOR,
    differential_head_fraction: float = 1.0,
) -> float:
    """Deepest water (ft) the joint may run in, from measured minimum wall.

    The factored collapse pressure of the thinnest measured section must cover
    the net external head.  ``differential_head_fraction`` = 1.0 is the
    fully-evacuated riser (most conservative); a mud/seawater-filled collapse
    scenario uses the fraction of full seawater head that acts as net
    differential (project design basis).
    """
    if not 0.0 < differential_head_fraction <= 1.0:
        raise ValueError("differential_head_fraction must be in (0, 1].")
    p_c = collapse_pressure_psi(od_in, wt_min_in, SMYS_PSI[grade])["p_c_psi"]
    return (design_factor * p_c
            / (SEAWATER_PSI_PER_FT * differential_head_fraction))


# ---------------------------------------------------------------------------
# String-zone placement
# ---------------------------------------------------------------------------
#: Fatigue-severity margin per string zone: remaining life must cover the
#: campaign duration times this factor.  Top/bottom are the wave+VIV vessel and
#: seabed interfaces (high severity); mid-string is the low-fatigue band.
#: Practice parameters pending a fatigue flaw-growth clock (#1270 follow-up).
ZONE_FATIGUE_MARGIN = {"top": 3.0, "mid": 1.0, "bottom": 3.0}


@dataclass
class JointPlacement:
    """Placement verdict for one riser joint."""

    joint_id: str
    min_life_years: float
    acceptable_depth_ft: float
    eligible_zones: list = field(default_factory=list)
    verdict: str = "ACCEPT"          # ACCEPT | RESTRICTED | REPAIR
    reason: str = ""


def place_joint(
    joint_id: str,
    min_life_years: float,
    wt_min_in: float,
    *,
    od_in: float,
    grade: str,
    campaign_water_depth_ft: float,
    campaign_years: float,
    zone_margins: Optional[dict] = None,
    differential_head_fraction: float = 1.0,
) -> JointPlacement:
    """Assign a joint to permissible string zones, or flag it.

    A zone is eligible when remaining life covers the campaign with that
    zone's fatigue margin AND the joint is collapse-qualified at the campaign
    water depth.  Source-program repair rule: a joint that neither fits the
    low-fatigue (mid) band nor stays collapse-qualified at 50% of the campaign
    depth goes to repair; a joint failing only on depth is RESTRICTED
    (usable in shallower campaigns).
    """
    margins = zone_margins or ZONE_FATIGUE_MARGIN
    depth_ok_ft = acceptable_water_depth_ft(
        od_in, wt_min_in, grade,
        differential_head_fraction=differential_head_fraction)
    strength_ok = depth_ok_ft >= campaign_water_depth_ft
    life_zones = [z for z, m in margins.items()
                  if min_life_years >= campaign_years * m]
    zones = sorted(life_zones) if strength_ok else []
    placement = JointPlacement(
        joint_id=joint_id,
        min_life_years=min_life_years,
        acceptable_depth_ft=round(depth_ok_ft, 1),
        eligible_zones=zones,
    )
    if zones:
        return placement
    strong_at_half = depth_ok_ft >= 0.5 * campaign_water_depth_ft
    fits_mid = min_life_years >= campaign_years * margins.get("mid", 1.0)
    if not fits_mid and not strong_at_half:
        placement.verdict = "REPAIR"
        placement.reason = (
            "unfit for low-fatigue band and not collapse-qualified at 50% "
            "campaign depth")
    elif not fits_mid:
        placement.verdict = "REPAIR"
        placement.reason = "remaining life below one campaign for every zone"
    elif not strong_at_half:
        placement.verdict = "REPAIR"
        placement.reason = (
            "not collapse-qualified at 50% campaign depth "
            f"(qualifies to {depth_ok_ft:.0f} ft)")
    else:
        placement.verdict = "RESTRICTED"
        placement.reason = (
            f"life-fit for {'/'.join(sorted(life_zones))} but only "
            f"collapse-qualified to {depth_ok_ft:.0f} ft")
    return placement


# ---------------------------------------------------------------------------
# Fleet rollup
# ---------------------------------------------------------------------------
def fleet_rollup(
    register: pd.DataFrame,
    *,
    component: str = "Main",
    campaign_years: float = 3.0,
    n_campaigns: int = 4,
    min_high_fatigue_joints: int = 12,
    high_fatigue_margin: float = 3.0,
) -> dict:
    """Campaign-by-campaign fleet status from a GML results register.

    ``register`` follows the real-inspection fixture schema (columns
    ``component / joint_id / min_life_years``).  A joint's governing life is
    its worst scan.  Per campaign horizon N the joint is FIT if life covers
    N campaigns, HIGH-FATIGUE-QUALIFIED if it covers ``high_fatigue_margin``
    times that, else REPAIR (reason: fatigue/corrosion life).
    """
    comp = register[register["component"] == component]
    if comp.empty:
        raise ValueError(f"no rows for component '{component}'.")
    life = comp.groupby("joint_id")["min_life_years"].min()

    campaigns = []
    for n in range(1, n_campaigns + 1):
        horizon = n * campaign_years
        fit = int((life >= horizon).sum())
        hf = int((life >= horizon * high_fatigue_margin).sum())
        campaigns.append({
            "campaign": n,
            "horizon_years": horizon,
            "fit_joints": fit,
            "high_fatigue_qualified": hf,
            "repair_joints": int(len(life) - fit),
            "high_fatigue_shortfall": max(0, min_high_fatigue_joints - hf),
        })
    return {
        "component": component,
        "n_joints": int(len(life)),
        "campaign_years": campaign_years,
        "min_high_fatigue_joints": min_high_fatigue_joints,
        "campaigns": campaigns,
    }


# ---------------------------------------------------------------------------
# Engine adapter (basename "riser_joint_ffs")
# ---------------------------------------------------------------------------
class RiserJointFFSWorkflow:
    """Engine adapter — dispatches on ``cfg['riser_joint_ffs']['mode']``.

    Modes: ``envelope`` (Level-1 chart data), ``placement`` (one joint),
    ``rollup`` (fleet register).  Results park on ``cfg[basename]`` for the
    ``kind: in_memory`` result locator.
    """

    def router(self, cfg: dict) -> dict:
        block = cfg.get("riser_joint_ffs")
        if not block:
            raise KeyError(
                "riser_joint_ffs workflow requires a 'riser_joint_ffs' block")
        mode = block.get("mode", "envelope")
        if mode == "envelope":
            result = level1_flaw_envelope(**block["envelope"])
        elif mode == "placement":
            placement = place_joint(**block["placement"])
            result = {
                "domain": "riser_joint_placement",
                "joint_id": placement.joint_id,
                "min_life_years": placement.min_life_years,
                "acceptable_depth_ft": placement.acceptable_depth_ft,
                "eligible_zones": placement.eligible_zones,
                "verdict": placement.verdict,
                "reason": placement.reason,
            }
        elif mode == "rollup":
            spec = dict(block["rollup"])
            register = pd.read_csv(spec.pop("register_csv"))
            result = fleet_rollup(register, **spec)
        else:
            raise ValueError(f"unknown riser_joint_ffs mode '{mode}'.")
        cfg[cfg["basename"]] = result
        return cfg
