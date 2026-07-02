# ABOUTME: Drilling-riser joint FFS — Level-1 flaw-acceptance envelopes, collapse-
# ABOUTME: limited water depth, string fatigue-zone placement and fleet rollup.
"""Drilling-riser joint fitness-for-service (#1292).

Life extension of marine drilling-riser joints: a corroded/flawed joint stays in
service — in the right string position — instead of being pulled for repair.
Composes the validated engines only; no new physics:

- **Level-1 flaw-acceptance envelope** — max acceptable surface-flaw length vs
  depth at the joint's design pressure, inverted from the golden-tested
  remaining-strength methods (:mod:`ffs_acceptance_curves` /
  :mod:`corroded_pipe`).  Practice depth caps split the chart into base-metal
  and weld regions; a campaign-end variant reserves growth allowance
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
program) pending the Part 9 flaw-growth clock (#1270); the strength and
remaining-life legs are code-anchored.  Units: inches / psi / feet / years.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional

import pandas as pd

from .corroded_pipe import SMYS_PSI
from .ffs_acceptance_curves import _max_acceptable_length

# Seawater hydrostatic gradient (psi per ft of depth) — 64 lb/ft3 seawater.
SEAWATER_PSI_PER_FT = 0.4444

# Practice depth caps from the source riser-IM program: acceptance-chart depth
# never exceeds these wall-thickness fractions regardless of computed strength.
DEPTH_CAP_BASE_METAL = 0.85
DEPTH_CAP_WELD = 0.60

# API RP 1111 collapse design factor (seamless/ERW pipe).
COLLAPSE_DESIGN_FACTOR = 0.70

STEEL_E_PSI = 30.0e6
STEEL_POISSON = 0.30


# ---------------------------------------------------------------------------
# Level-1 flaw-acceptance envelope
# ---------------------------------------------------------------------------
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
) -> dict:
    """Acceptable surface-flaw length vs depth chart for one joint region.

    ``campaign_years`` > 0 gives the campaign-*end* envelope: each charted
    depth is assessed at ``d + corrosion_rate * campaign_years`` so the flaw
    still passes after growing through the campaign.

    Returns dict with ``depth_in`` / ``depth_frac`` / ``max_acceptable_length_in``
    (0.0 where the region cap or strength check excludes the depth).
    """
    if region not in ("base", "weld"):
        raise ValueError(f"region must be 'base' or 'weld', got '{region}'.")
    if grade not in SMYS_PSI:
        raise ValueError(f"unknown grade '{grade}'. Use one of {sorted(SMYS_PSI)}.")
    cap = DEPTH_CAP_BASE_METAL if region == "base" else DEPTH_CAP_WELD
    growth_in = corrosion_rate_in_per_yr * campaign_years
    depth_fracs = depth_fracs or [round(0.05 * i, 2) for i in range(1, 18)]  # 0.05..0.85

    depth_in, max_len = [], []
    for frac in depth_fracs:
        d = frac * wt_in
        d_end = d + growth_in
        if frac > cap or (d_end / wt_in) > cap or d_end >= wt_in:
            length = 0.0
        else:
            length = _max_acceptable_length(
                method, od_in, wt_in, d_end, grade, design_pressure_psi, max_length_in
            )
        depth_in.append(round(d, 4))
        max_len.append(round(length, 3))
    return {
        "domain": "riser_joint_flaw_envelope",
        "od_in": od_in, "wt_in": wt_in, "grade": grade, "method": method,
        "design_pressure_psi": design_pressure_psi,
        "region": region, "depth_cap_frac": cap,
        "corrosion_rate_in_per_yr": corrosion_rate_in_per_yr,
        "campaign_years": campaign_years,
        "depth_frac": list(depth_fracs), "depth_in": depth_in,
        "max_acceptable_length_in": max_len,
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
#: Practice parameters pending the #1270 flaw-growth clock.
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
