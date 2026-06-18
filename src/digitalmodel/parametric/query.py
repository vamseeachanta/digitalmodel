"""``parametric_query`` workflow router — serve an interpolated answer from a
pre-computed atlas, or flag escalation when out of range / low confidence.

Decision 3 of the spec. Result *shaping* is per-workflow (a Miner sum for
fatigue, a verdict for a code check, a direct value for an RAO), but the
interpolation and the out-of-range rail live in Atlas.predict and are shared.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any, Callable

import yaml

from digitalmodel.parametric.atlas import Atlas

REPO_ROOT = Path(__file__).resolve().parents[3]
DEFAULT_ATLAS_ROOT = REPO_ROOT / "atlases"

SCREENING_DISCLAIMER = (
    "Screening estimate from a pre-computed atlas — not a certified deliverable. "
    "A full on-demand run remains the document of record."
)
ESCALATE_DISCLAIMER = (
    "Outside the atlas coverage — routed to a full on-demand run (24h SLA). "
    "No value is extrapolated."
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

    handler = _HANDLERS.get(basename)
    if handler is None:
        raise NotImplementedError(f"no parametric_query handler for atlas {basename!r}")

    atlas_root = Path(settings.get("atlas_root", DEFAULT_ATLAS_ROOT))
    atlas = Atlas.load(atlas_root, basename)

    stale_reason = _staleness(atlas)
    if stale_reason is not None:
        result = {**_escalation(atlas, stale_reason), "stale": True}
    else:
        result = handler(atlas, point)
    if not result["in_range"] and on_out_of_range == "error":
        raise ValueError(f"parametric_query out of range: {result.get('reason')}")

    cfg["parametric_query"] = {**settings, "result": result}
    _write_result(cfg, settings, result)
    return cfg


def _staleness(atlas: Atlas) -> str | None:
    """Return a reason string if the atlas is stale (its build basis has moved
    since it was generated), else None. Detectable per the refresh contract
    (#799): a stale atlas must escalate, not serve a superseded answer."""
    workflow_id = atlas.provenance.get("workflow_id")
    stored = atlas.provenance.get("content_fingerprint")
    if not workflow_id or not stored:
        return None  # atlas predates fingerprinting; nothing to check against
    from digitalmodel.parametric import refresh

    try:
        current = refresh.content_fingerprint(workflow_id)
    except Exception:
        return None  # cannot recompute (e.g. registry absent) -> do not false-alarm
    if current != stored:
        return (
            "atlas stale: build basis (spec / template / source / standard) "
            "changed since generation — refresh required"
        )
    return None


def _provenance(atlas: Atlas) -> dict[str, Any]:
    return {
        "atlas_id": atlas.atlas_id,
        "code_version": atlas.provenance.get("code_version"),
        "standards": atlas.provenance.get("standards"),
    }


def _escalation(atlas: Atlas, reason: str) -> dict[str, Any]:
    return {
        "value": None,
        "in_range": False,
        "reason": reason,
        "action": "escalate",
        "disclaimer": ESCALATE_DISCLAIMER,
        "provenance": {"atlas_id": atlas.atlas_id},
    }


# -- per-workflow handlers ---------------------------------------------------


def _handle_mooring_fatigue(atlas: Atlas, point: dict[str, Any]) -> dict[str, Any]:
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
        "disclaimer": SCREENING_DISCLAIMER,
        "provenance": _provenance(atlas),
    }


def _utilisation_result(atlas: Atlas, u: float, band: list[float]) -> dict[str, Any]:
    # straddle rule: if the confidence band crosses the limit, the verdict is
    # not decidable from the atlas -> escalate even though it is in range.
    if band[0] < 1.0 < band[1]:
        return _escalation(
            atlas,
            f"utilisation {u:.3f} band [{band[0]:.3f}, {band[1]:.3f}] straddles 1.0",
        )
    return {
        "response": "utilisation",
        "value": u,
        "screening_status": "pass" if u <= 1.0 else "fail",
        "confidence": {"band": band, "basis": f"holdout max_rel_error = {atlas.max_rel_error:.4f}"},
        "in_range": True,
        "stale": False,
        "disclaimer": SCREENING_DISCLAIMER,
        "provenance": _provenance(atlas),
    }


def _handle_utilisation(atlas: Atlas, point: dict[str, Any]) -> dict[str, Any]:
    """Boundary class where the atlas predicts the utilisation directly
    (code_check, free_span): interpolate, then threshold at 1.0."""
    prediction = atlas.predict(point)
    if not prediction.in_range:
        return _escalation(atlas, prediction.reason)
    u = prediction.value
    e = atlas.max_rel_error
    return _utilisation_result(atlas, u, [u * (1 - e), u * (1 + e)])


def _handle_capacity_demand(atlas: Atlas, point: dict[str, Any]) -> dict[str, Any]:
    """Boundary class where the atlas predicts a CAPACITY (pile, anchor) and the
    load case (demand, factor of safety) is applied at query time:
    utilisation = demand * FoS / capacity. The atlas axes stay geometry/soil
    only; the demand is not baked into the grid."""
    prediction = atlas.predict(point)
    if not prediction.in_range:
        return _escalation(atlas, prediction.reason)
    capacity = prediction.value
    if capacity <= 0:
        return _escalation(atlas, "non-positive capacity")
    demand = float(point["demand_kN"])
    fos = float(point.get("factor_of_safety", 1.0))
    e = atlas.max_rel_error
    u = demand * fos / capacity
    # capacity carries +/- e error, so utilisation carries -/+ e (inverse)
    band = [demand * fos / (capacity * (1 + e)), demand * fos / (capacity * (1 - e))]
    result = _utilisation_result(atlas, u, band)
    if result["in_range"]:
        result["capacity_kN"] = capacity
    return result


def _handle_rao(atlas: Atlas, point: dict[str, Any]) -> dict[str, Any]:
    # query is natural in period; the atlas axis is frequency (the workflow's
    # interpolation space), so convert here.
    lookup = dict(point)
    if "frequency_rad_s" not in lookup and "period_s" in lookup:
        lookup["frequency_rad_s"] = 2.0 * math.pi / float(lookup["period_s"])

    prediction = atlas.predict(lookup)
    if not prediction.in_range:
        return _escalation(atlas, prediction.reason)

    v = prediction.value
    e = atlas.max_rel_error
    return {
        "response": atlas.response,
        "value": v,
        "confidence": {"band": [v * (1 - e), v * (1 + e)],
                       "basis": f"holdout max_rel_error = {e:.4f}"},
        "in_range": True,
        "stale": False,
        "disclaimer": SCREENING_DISCLAIMER,
        "provenance": _provenance(atlas),
    }


_HANDLERS: dict[str, Callable[[Atlas, dict[str, Any]], dict[str, Any]]] = {
    "mooring_fatigue": _handle_mooring_fatigue,
    "code_check": _handle_utilisation,
    "free_span": _handle_utilisation,
    "rao_tabulation": _handle_rao,
    "pile_capacity": _handle_capacity_demand,
    "anchor_capacity": _handle_capacity_demand,
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
