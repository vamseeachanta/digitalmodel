# ABOUTME: Rig Capability Assessment workflow (stages 1-4) — onshore rig screen + scoring.
# ABOUTME: Issue #821 — rule-based, public-data, no network/solver. See deckhand rig-capability-assessment.md.
"""Onshore drilling-rig capability assessment for field development.

Pure-python, deterministic, explainable. Given a well/field program and a set of
candidate rigs (from a bundled public rig-CLASS defaults table or per-rig
overrides), it:

  1. derives the required rig capability from the well-design envelope,
  2. ingests candidate rigs (defaults keyed by ``source`` + optional overrides),
  3. applies hard-gate screening (pass/fail with explicit reasons),
  4. scores & ranks survivors on a weighted capability fit.

No network access, no licensed solver, no downhole physics. Rig-class defaults are
**marketing-grade, not contractual** (see ``RIG_CLASS_DEFAULTS`` source notes);
binding capability needs the per-rig IADC daily drilling report / spec sheet.

The one publicly-stated numeric "super-spec" threshold (Helmerich & Payne FY2025
10-K) is used as the default hard-gate set: AC drive, >=1,500 HP drawworks,
>=750,000 lb hookload, 7,500 psi mud system, multi-well-pad (walking) capability.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

# --- super-spec defaults (H&P FY2025 10-K numeric definition) ---------------
SUPERSPEC_MIN_HOOKLOAD_LBF = 750_000.0
SUPERSPEC_MIN_DRAWWORKS_HP = 1_500.0
SUPERSPEC_MIN_MUD_PSI = 7_500.0

# Hookload design factor applied to the heaviest planned string when an explicit
# min hookload is not supplied (covers overpull / margin-of-overpull on stuck pipe).
DEFAULT_HOOKLOAD_DESIGN_FACTOR = 1.25

# Capability-margin headroom that scores 1.0 (e.g. 50% above the gate is "full marks").
MARGIN_SATURATION = 0.50
# Walk speed (ft/hr) that scores 1.0 on the pad-mobility dimension.
WALK_SPEED_SATURATION_FT_HR = 100.0

DEFAULT_SCORING_WEIGHTS = {
    "capability_margin": 0.25,
    "pad_mobility": 0.20,
    "automation_digital": 0.20,
    "power_emissions": 0.15,
    "ht_readiness": 0.20,
}


# Public rig-CLASS defaults. Values are class/marketing-grade and attributed.
# ht_track_record is a coarse 0-1 geothermal high-temperature readiness prior
# (no contractor publishes this) — override per engagement with real evidence.
RIG_CLASS_DEFAULTS: dict[str, dict[str, Any]] = {
    "hp_flexrig": {
        "model": "H&P FlexRig (Flex3/Flex5 class)",
        "contractor": "Helmerich & Payne",
        "hookload_lbf": 750_000.0,
        "drawworks_hp": 1_500.0,
        "mud_psi": 7_500.0,
        "mud_pump_total_hp": 3_200.0,
        "top_drive": True,
        "drive_type": "AC",
        "pad": "walking",
        "walk_speed_ft_hr": 100.0,
        "automation": True,
        "data_interop": True,
        "bi_fuel_or_highline": True,
        "ht_track_record": 0.5,
        "source": "hpinc.com/drilling-automation/flexrig-solutions; FY2025 10-K (CIK 0000046765). Marketing-grade.",
    },
    "precision_st1500": {
        "model": "Precision Super Triple ST-1500",
        "contractor": "Precision Drilling",
        "hookload_lbf": 825_000.0,
        "drawworks_hp": 1_500.0,
        "mud_psi": 7_500.0,
        "mud_pump_total_hp": 3_200.0,
        "top_drive": True,
        "drive_type": "AC",
        "pad": "walking",
        "walk_speed_ft_hr": 100.0,
        "automation": True,
        "data_interop": True,
        "bi_fuel_or_highline": True,
        "ht_track_record": 0.3,
        "source": "iadc.org/.../2020_Super_Triple_1500HP.pdf (ST-1500 spec sheet). Marketing-grade.",
    },
    "pten_apex": {
        "model": "Patterson-UTI APEX XC",
        "contractor": "Patterson-UTI",
        "hookload_lbf": 1_000_000.0,
        "drawworks_hp": 1_500.0,
        "mud_psi": 7_500.0,
        "mud_pump_total_hp": 3_200.0,
        "top_drive": True,
        "drive_type": "AC",  # APEX XC class; AC/top-drive rating not separately disclosed by PTEN.
        "pad": "walking",
        "walk_speed_ft_hr": 100.0,
        "automation": True,
        "data_interop": True,
        "bi_fuel_or_highline": True,
        "ht_track_record": 0.2,
        "source": "patenergy.com/services/apex-fleet; 10-K (CIK 0000889900). AC/TD rating not disclosed; assumed AC for XC.",
    },
    "nabors_pace_x800": {
        "model": "Nabors PACE-X800",
        "contractor": "Nabors",
        "hookload_lbf": 750_000.0,
        "drawworks_hp": 1_500.0,
        "mud_psi": 7_500.0,
        "mud_pump_total_hp": 3_000.0,
        "top_drive": True,
        "drive_type": "AC",
        "pad": "skidding",  # PACE-X800 uses an X-Y skid system, not omnidirectional walking.
        "walk_speed_ft_hr": 0.0,
        "automation": True,
        "data_interop": True,
        "bi_fuel_or_highline": True,
        "ht_track_record": 0.2,
        "source": "nabors.com/.../pace-x/ (PACE-X800: X-Y skid, 2x1500 HP, 7,500 psi, SmartROS). Marketing-grade.",
    },
}


# --- data model -------------------------------------------------------------
@dataclass
class RequiredCapability:
    """Hard-gate thresholds a rig must meet to qualify."""

    min_hookload_lbf: float
    min_drawworks_hp: float
    min_mud_psi: float
    top_drive: bool = True
    drive_type: str = "AC"  # "AC" requires AC; "any" disables the gate
    pad_capability: str = "walking"  # "walking" | "skidding" | "any"
    automation: bool = True
    data_interop: bool = True  # require real-time data sharing (e.g. WITSML)


@dataclass
class RigAssessment:
    """Per-rig screen + score result."""

    rig_id: str
    model: str
    contractor: str
    qualified: bool
    fail_reasons: list[str] = field(default_factory=list)
    fit_score: float = 0.0
    dimension_scores: dict[str, float] = field(default_factory=dict)
    source: str = ""


def _as_bool(value: Any) -> bool:
    if isinstance(value, bool):
        return value
    if isinstance(value, str):
        return value.strip().lower() in {"1", "true", "yes", "required", "y"}
    return bool(value)


def derive_required_capability(cfg: dict[str, Any]) -> RequiredCapability:
    """Stage 1 — required capability from explicit overrides, else well design."""
    explicit = cfg.get("required_capability") or {}
    well = cfg.get("program", {}).get("well_design", {})

    heaviest = float(well.get("heaviest_string_lbf", 0.0) or 0.0)
    derived_hookload = heaviest * DEFAULT_HOOKLOAD_DESIGN_FACTOR
    min_hookload = float(
        explicit.get("min_hookload_lbf")
        or max(derived_hookload, SUPERSPEC_MIN_HOOKLOAD_LBF)
    )

    min_mud = float(
        explicit.get("min_mud_psi")
        or well.get("mud_max_circulating_psi")
        or SUPERSPEC_MIN_MUD_PSI
    )
    min_hp = float(explicit.get("min_drawworks_hp") or SUPERSPEC_MIN_DRAWWORKS_HP)

    return RequiredCapability(
        min_hookload_lbf=min_hookload,
        min_drawworks_hp=min_hp,
        min_mud_psi=min_mud,
        top_drive=_as_bool(explicit.get("top_drive", True)),
        drive_type=str(explicit.get("drive_type", "AC")),
        pad_capability=str(explicit.get("pad_capability", "walking")),
        automation=_as_bool(explicit.get("automation", True)),
        data_interop=_as_bool(explicit.get("data_interop", True)),
    )


def resolve_rig(spec: dict[str, Any]) -> dict[str, Any]:
    """Stage 2 — merge a candidate's bundled class defaults with per-rig overrides."""
    source_key = spec.get("source")
    base: dict[str, Any] = {}
    if source_key and source_key in RIG_CLASS_DEFAULTS:
        base = dict(RIG_CLASS_DEFAULTS[source_key])
    overrides = spec.get("overrides") or {}
    merged = {**base, **overrides}
    merged["rig_id"] = spec.get("id", merged.get("model", "rig"))
    merged.setdefault("contractor", spec.get("contractor", "unknown"))
    merged.setdefault("model", merged["rig_id"])
    return merged


def screen_rig(rig: dict[str, Any], req: RequiredCapability) -> list[str]:
    """Stage 3 — hard-gate screen; returns the list of unmet gates (empty = pass)."""
    fails: list[str] = []
    if float(rig.get("hookload_lbf", 0)) < req.min_hookload_lbf:
        fails.append(
            f"hookload {rig.get('hookload_lbf', 0):,.0f} < required {req.min_hookload_lbf:,.0f} lbf"
        )
    if float(rig.get("drawworks_hp", 0)) < req.min_drawworks_hp:
        fails.append(
            f"drawworks {rig.get('drawworks_hp', 0):,.0f} < required {req.min_drawworks_hp:,.0f} HP"
        )
    if float(rig.get("mud_psi", 0)) < req.min_mud_psi:
        fails.append(
            f"mud system {rig.get('mud_psi', 0):,.0f} < required {req.min_mud_psi:,.0f} psi"
        )
    if req.top_drive and not _as_bool(rig.get("top_drive", False)):
        fails.append("no top drive")
    if req.drive_type.upper() == "AC" and str(rig.get("drive_type", "")).upper() != "AC":
        fails.append(f"drive type {rig.get('drive_type', '?')} != AC")
    if req.pad_capability != "any" and rig.get("pad") != req.pad_capability:
        fails.append(f"pad capability {rig.get('pad', '?')} != {req.pad_capability}")
    if req.automation and not _as_bool(rig.get("automation", False)):
        fails.append("no drilling automation")
    if req.data_interop and not _as_bool(rig.get("data_interop", False)):
        fails.append("no real-time data interoperability")
    return fails


def _margin_score(value: float, gate: float) -> float:
    if gate <= 0:
        return 1.0
    headroom = (value - gate) / gate
    return max(0.0, min(1.0, headroom / MARGIN_SATURATION))


def score_rig(
    rig: dict[str, Any], req: RequiredCapability, weights: dict[str, float]
) -> tuple[float, dict[str, float]]:
    """Stage 4 — weighted fit score for a qualifying rig."""
    capability_margin = (
        _margin_score(float(rig.get("hookload_lbf", 0)), req.min_hookload_lbf)
        + _margin_score(float(rig.get("drawworks_hp", 0)), req.min_drawworks_hp)
        + _margin_score(float(rig.get("mud_psi", 0)), req.min_mud_psi)
    ) / 3.0

    pad_mobility = min(
        1.0, float(rig.get("walk_speed_ft_hr", 0)) / WALK_SPEED_SATURATION_FT_HR
    )

    automation_digital = 0.5 * _as_bool(rig.get("automation", False)) + 0.5 * _as_bool(
        rig.get("data_interop", False)
    )

    power_emissions = 0.6 * (str(rig.get("drive_type", "")).upper() == "AC") + 0.4 * _as_bool(
        rig.get("bi_fuel_or_highline", False)
    )

    ht_readiness = max(0.0, min(1.0, float(rig.get("ht_track_record", 0.0))))

    dims = {
        "capability_margin": round(capability_margin, 4),
        "pad_mobility": round(pad_mobility, 4),
        "automation_digital": round(automation_digital, 4),
        "power_emissions": round(power_emissions, 4),
        "ht_readiness": round(ht_readiness, 4),
    }
    total_w = sum(weights.values()) or 1.0
    fit = sum(weights.get(k, 0.0) * v for k, v in dims.items()) / total_w
    return round(fit, 4), dims


def assess(cfg_params: dict[str, Any]) -> dict[str, Any]:
    """Run stages 1-4 and return a JSON-able summary."""
    req = derive_required_capability(cfg_params)
    weights = {**DEFAULT_SCORING_WEIGHTS, **(cfg_params.get("scoring_weights") or {})}

    candidates = cfg_params.get("candidate_rigs") or []
    results: list[RigAssessment] = []
    for spec in candidates:
        rig = resolve_rig(spec)
        fails = screen_rig(rig, req)
        qualified = not fails
        fit, dims = (0.0, {})
        if qualified:
            fit, dims = score_rig(rig, req, weights)
        results.append(
            RigAssessment(
                rig_id=rig["rig_id"],
                model=rig.get("model", rig["rig_id"]),
                contractor=rig.get("contractor", "unknown"),
                qualified=qualified,
                fail_reasons=fails,
                fit_score=fit,
                dimension_scores=dims,
                source=rig.get("source", ""),
            )
        )

    # rank qualified by fit (desc), then disqualified
    ranked = sorted(
        results, key=lambda r: (r.qualified, r.fit_score), reverse=True
    )
    for rank, r in enumerate((x for x in ranked if x.qualified), start=1):
        r.dimension_scores = {"_rank": rank, **r.dimension_scores}

    return {
        "calculation": "rig_capability_assessment",
        "required_capability": {
            "min_hookload_lbf": req.min_hookload_lbf,
            "min_drawworks_hp": req.min_drawworks_hp,
            "min_mud_psi": req.min_mud_psi,
            "top_drive": req.top_drive,
            "drive_type": req.drive_type,
            "pad_capability": req.pad_capability,
            "automation": req.automation,
            "data_interop": req.data_interop,
        },
        "scoring_weights": weights,
        "n_candidates": len(results),
        "n_qualified": sum(1 for r in results if r.qualified),
        "ranked_rigs": [
            {
                "rig_id": r.rig_id,
                "model": r.model,
                "contractor": r.contractor,
                "qualified": r.qualified,
                "fit_score": r.fit_score,
                "fail_reasons": r.fail_reasons,
                "dimension_scores": r.dimension_scores,
                "source": r.source,
            }
            for r in ranked
        ],
        "caveat": (
            "Rig-class defaults are marketing-grade, not contractual. Stages 1-4 only "
            "(screen + score); KPI baselining and paid rig-level feeds are out of scope."
        ),
    }


def router(cfg: dict[str, Any]) -> dict[str, Any]:
    """Engine entry point for basename == 'rig_capability_assessment'."""
    params = cfg.get("rig_capability_assessment", {})
    summary = assess(params)

    output_cfg = params.get("outputs", {})
    directory = Path(output_cfg.get("directory", "results"))
    if not directory.is_absolute():
        directory = Path(cfg.get("_config_dir_path", Path.cwd())) / directory
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get("summary_json", "rig_capability_summary.json")
    summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")

    summary["summary_json"] = str(summary_path)
    cfg["rig_capability_assessment"] = {**params, **summary}
    return cfg
