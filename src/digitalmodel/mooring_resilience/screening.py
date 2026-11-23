"""Mooring-system resilience screening.

Composes the existing pre-computed atlases into a single *resilience* view for a
spread-moored unit (FPSO / FPU / floating wind), addressing the lifecycle
themes raised in issue #974: extreme-weather severity, ageing units, foundation
("strong foundations") capacity, and single-line-failure (damaged) redundancy.

For a given mooring configuration + metocean case it reports four checks and a
traffic-light:

1. **Intact tension utilization** -- peak line tension from the
   ``fpso_mooring_full`` atlas vs line MBL, with an ASD safety factor.
2. **Damaged tension utilization** -- one-line-failure peak (intact peak scaled
   by a redistribution factor) vs MBL, with the (lower) damaged safety factor.
3. **Foundation utilization** -- damaged peak vs anchor holding capacity from
   the ``anchor_capacity`` atlas, with a geotechnical safety factor.
4. **Fatigue margin** -- closed-form DNV chain T-N life vs design life
   (screening proxy; the ``mooring_fatigue`` atlas is the higher-fidelity path).

Safety factors follow API RP 2SK (ASD): intact 1.67, one-line damaged 1.25;
foundation 1.5 per DNV-OS-E301 practice. All factors are exposed on
:class:`ResilienceFactors`.

Out-of-range atlas queries are not extrapolated -- the check is marked for
escalation (a full on-demand run), mirroring the repo's parametric-query policy.

> Screening tool only. A full mooring analysis (e.g. OrcaFlex) remains the
> document of record.
"""

from __future__ import annotations

import math
import warnings
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

from digitalmodel.parametric.atlas import Atlas
from digitalmodel.parametric.query import DEFAULT_ATLAS_ROOT


@dataclass(frozen=True)
class MooringConfig:
    n_lines: int
    mbl_kN: float              # minimum breaking load per line
    line_area_mm2: float       # nominal steel area (chain) for fatigue stress
    anchor_diameter_m: float   # suction anchor diameter
    anchor_length_m: float     # suction anchor skirt length
    soil_su_kpa: float         # undrained shear strength
    water_depth_m: float


@dataclass(frozen=True)
class Metocean:
    Hs: float   # significant wave height [m]
    Tp: float   # peak period [s]


@dataclass(frozen=True)
class ResilienceFactors:
    fos_intact: float = 1.67       # API RP 2SK ASD, intact
    fos_damaged: float = 1.25      # API RP 2SK ASD, one-line damaged
    fos_foundation: float = 1.5    # geotechnical SF (DNV-OS-E301 practice)
    redistribution: float = 1.30   # damaged/intact peak-tension ratio (spread)
    design_life_years: float = 25.0
    annual_cycles: float = 3.15e6  # ~1 wave cycle / 10 s over a year
    # Dynamic tension range scales with sea state: dT = k * Hs (screening proxy).
    dyn_tension_per_Hs_kN: float = 45.0
    # DNV-OS-E301 / DNV-RP-C203 studless chain T-N: N = a_D * S^-m  (S in MPa)
    sn_a_D: float = 6.0e10
    sn_m: float = 3.0


@dataclass(frozen=True)
class ResilienceResult:
    t_intact_kN: float
    t_damaged_kN: float
    util_intact: float
    util_damaged: float
    foundation_capacity_kN: float
    util_foundation: float
    fatigue_life_years: float
    fatigue_margin: float
    governing_util: float
    light: str                 # GREEN | AMBER | RED | ESCALATE
    notes: list[str] = field(default_factory=list)


def _atlas(atlas_root: Path | str | None, basename: str) -> Atlas:
    root = Path(atlas_root) if atlas_root is not None else DEFAULT_ATLAS_ROOT
    return Atlas.load(root, basename)


def intact_tension_kN(cfg: MooringConfig, met: Metocean,
                      atlas_root: Path | str | None = None):
    """Peak intact line tension [kN] from the fpso_mooring_full atlas.

    Returns (value_kN_or_nan, in_range, reason).
    """
    a = _atlas(atlas_root, "fpso_mooring_full")
    p = a.predict({"Hs": met.Hs, "Tp": met.Tp, "water_depth_m": cfg.water_depth_m})
    val = p.value / 1000.0 if p.in_range else math.nan  # N -> kN
    return val, p.in_range, p.reason


def foundation_capacity_kN(cfg: MooringConfig, atlas_root: Path | str | None = None):
    """Anchor holding capacity [kN] from the anchor_capacity atlas.

    Returns (value_kN_or_nan, in_range, reason).
    """
    a = _atlas(atlas_root, "anchor_capacity")
    p = a.predict({"diameter_m": cfg.anchor_diameter_m,
                   "length_m": cfg.anchor_length_m,
                   "su_kpa": cfg.soil_su_kpa})
    return (p.value if p.in_range else math.nan), p.in_range, p.reason


def _fatigue_life_years(met: Metocean, cfg: MooringConfig,
                        f: ResilienceFactors) -> float:
    """Closed-form DNV chain T-N screening life [years].

    Fatigue is driven by the *dynamic* tension range (taken proportional to Hs),
    not the static peak -- a constant-amplitude rainflow-equivalent proxy.
    """
    tension_range_kN = f.dyn_tension_per_Hs_kN * met.Hs
    stress_range_MPa = (tension_range_kN * 1e3) / cfg.line_area_mm2  # kN->N / mm^2
    if stress_range_MPa <= 0:
        return math.inf
    n_allow = f.sn_a_D * stress_range_MPa ** (-f.sn_m)
    annual_damage = f.annual_cycles / n_allow
    return math.inf if annual_damage <= 0 else 1.0 / annual_damage


def _traffic_light(gov_util: float, fatigue_margin: float,
                   escalate: bool) -> str:
    if escalate:
        return "ESCALATE"
    if gov_util > 1.0 or fatigue_margin < 1.0:
        return "RED"
    if gov_util > 0.8 or fatigue_margin < 2.0:
        return "AMBER"
    return "GREEN"


def assess(cfg: MooringConfig, met: Metocean,
           factors: ResilienceFactors | None = None,
           atlas_root: Path | str | None = None) -> ResilienceResult:
    """Full resilience screening for one config + metocean case."""
    f = factors or ResilienceFactors()
    notes: list[str] = []

    t_intact, intact_ok, intact_reason = intact_tension_kN(cfg, met, atlas_root)
    cap, cap_ok, cap_reason = foundation_capacity_kN(cfg, atlas_root)

    escalate = not (intact_ok and cap_ok)
    if not intact_ok:
        notes.append(f"tension atlas out of range: {intact_reason}")
    if not cap_ok:
        notes.append(f"anchor-capacity atlas out of range: {cap_reason}")

    t_damaged = t_intact * f.redistribution if intact_ok else math.nan

    util_intact = (f.fos_intact * t_intact / cfg.mbl_kN) if intact_ok else math.nan
    util_damaged = (f.fos_damaged * t_damaged / cfg.mbl_kN) if intact_ok else math.nan
    util_found = (f.fos_foundation * t_damaged / cap) if (intact_ok and cap_ok) else math.nan

    life = _fatigue_life_years(met, cfg, f) if intact_ok else math.nan
    margin = (life / f.design_life_years) if intact_ok else math.nan

    finite_utils = [u for u in (util_intact, util_damaged, util_found) if not math.isnan(u)]
    gov = max(finite_utils) if finite_utils else math.nan

    light = _traffic_light(gov if not math.isnan(gov) else 0.0,
                           margin if not math.isnan(margin) else math.inf,
                           escalate)

    return ResilienceResult(
        t_intact_kN=t_intact, t_damaged_kN=t_damaged,
        util_intact=util_intact, util_damaged=util_damaged,
        foundation_capacity_kN=cap, util_foundation=util_found,
        fatigue_life_years=life, fatigue_margin=margin,
        governing_util=gov, light=light, notes=notes,
    )


_CITATION_WARNED = False


def safety_factor_citations(repo_root: Optional[Path] = None) -> dict:
    """Return ``{"intact", "damaged", "foundation"}`` CitedValues for the mooring
    safety factors, sourced from the existing DNV-OS-E301 registry.

    The values match :class:`ResilienceFactors` (``fos_intact`` / ``fos_damaged``
    / ``fos_foundation``). In standalone mode (no resolvable wiki page) it degrades
    gracefully: emits a one-shot warning and returns ``{}`` rather than failing —
    mirroring ``offshore_container.factor_citations``. Where the wiki resolves, a
    missing or mismatched page fails closed (raises).
    """
    global _CITATION_WARNED
    from digitalmodel.citations.registry import (
        MooringCondition,
        get_anchor_safety_factor,
        get_mooring_safety_factor,
    )
    from digitalmodel.citations.schema import CitationResolutionError

    try:
        return {
            "intact": get_mooring_safety_factor(
                MooringCondition.INTACT_QUASI_STATIC, repo_root=repo_root),
            "damaged": get_mooring_safety_factor(
                MooringCondition.DAMAGED_QUASI_STATIC, repo_root=repo_root),
            "foundation": get_anchor_safety_factor(repo_root=repo_root),
        }
    except CitationResolutionError as exc:
        if not _CITATION_WARNED:
            warnings.warn(
                "DNV-OS-E301 mooring citations unavailable (standalone mode): "
                f"{exc}. Configure LLM_WIKI_PATH to enable calc citations.",
                RuntimeWarning, stacklevel=2,
            )
            _CITATION_WARNED = True
        return {}
