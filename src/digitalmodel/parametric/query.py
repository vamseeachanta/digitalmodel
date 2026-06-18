"""``parametric_query`` workflow router — serve an interpolated answer from a
pre-computed atlas, or flag escalation when out of range / low confidence.

Decision 3 of the spec. For mooring_fatigue the atlas holds per-cell *damage*;
total damage is the additive Miner sum over the queried bins, and fatigue life
is derived analytically afterwards.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.parametric.atlas import Atlas

REPO_ROOT = Path(__file__).resolve().parents[3]
DEFAULT_ATLAS_ROOT = REPO_ROOT / "atlases"

DISCLAIMER = (
    "Screening estimate from a pre-computed atlas — not a certified deliverable. "
    "A full on-demand run remains the document of record."
)


def router(cfg: dict) -> dict:
    settings = cfg.get("parametric_query") or {}
    basename = settings.get("atlas")
    if not basename:
        raise ValueError("parametric_query.atlas (workflow basename) is required")
    point = settings.get("point") or {}
    policy = settings.get("policy") or {}
    on_out_of_range = policy.get("on_out_of_range", "escalate")
    if on_out_of_range not in {"escalate", "error"}:
        raise ValueError(
            "parametric_query.policy.on_out_of_range must be 'escalate' or 'error' "
            "(clamp/extrapolate are not permitted)"
        )

    atlas_root = Path(settings.get("atlas_root", DEFAULT_ATLAS_ROOT))
    atlas = Atlas.load(atlas_root, basename)

    result = _evaluate(atlas, point)
    if not result["in_range"] and on_out_of_range == "error":
        raise ValueError(f"parametric_query out of range: {result['reason']}")

    cfg["parametric_query"] = {**settings, "result": result}
    _write_result(cfg, settings, result)
    return cfg


def _evaluate(atlas: Atlas, point: dict[str, Any]) -> dict[str, Any]:
    if atlas.basename != "mooring_fatigue":
        raise NotImplementedError(
            f"parametric_query pilot only wires mooring_fatigue, got {atlas.basename!r}"
        )

    shared = {"area_mm2": point.get("area_mm2"), "sn_curve": point.get("sn_curve")}
    bins = point.get("tension_range_bins")
    if bins is None:
        bins = [{"tension_range_kN": point["tension_range_kN"],
                 "n_cycles": point["n_cycles"]}]

    total_damage = 0.0
    for b in bins:
        prediction = atlas.predict({**shared, **b})
        if not prediction.in_range:
            return _escalation(atlas, prediction.reason)
        total_damage += prediction.value

    design_life = float(point["design_life_years"])
    dff = float(point["dff"])
    life = math.inf if total_damage <= 0 else design_life / total_damage
    required_life = design_life * dff
    dff_margin = math.inf if math.isinf(life) else life / required_life

    e = atlas.max_rel_error
    if math.isinf(life):
        band = [math.inf, math.inf]
    else:
        band = [design_life / (total_damage * (1 + e)),
                design_life / (total_damage * (1 - e))]

    return {
        "response": "fatigue_life_years",
        "value": life,
        "total_damage": total_damage,
        "dff_margin": dff_margin,
        "screening_status": "pass" if dff_margin >= 1.0 else "fail",
        "confidence": {"band": band, "basis": f"holdout max_rel_error = {e:.4f}"},
        "in_range": True,
        "stale": False,
        "disclaimer": DISCLAIMER,
        "provenance": {
            "atlas_id": atlas.atlas_id,
            "code_version": atlas.provenance.get("code_version"),
            "standards": atlas.provenance.get("standards"),
        },
    }


def _escalation(atlas: Atlas, reason: str) -> dict[str, Any]:
    return {
        "response": "fatigue_life_years",
        "value": None,
        "in_range": False,
        "reason": reason,
        "action": "escalate",
        "disclaimer": (
            "Outside the atlas coverage — routed to a full on-demand run "
            "(24h SLA). No value is extrapolated."
        ),
        "provenance": {"atlas_id": atlas.atlas_id},
    }


def _write_result(cfg: dict, settings: dict[str, Any], result: dict[str, Any]) -> None:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        config_dir = cfg.get("_config_dir_path") or (
            Path(cfg["_config_file_path"]).parent
            if cfg.get("_config_file_path")
            else Path.cwd()
        )
        output_dir = Path(config_dir) / output_dir
    output_dir.mkdir(parents=True, exist_ok=True)
    stem = (
        Path(cfg["_config_file_path"]).stem
        if cfg.get("_config_file_path")
        else "parametric_query"
    )
    (output_dir / f"{stem}_result.yml").write_text(
        yaml.safe_dump(result, sort_keys=False)
    )
