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
    unknown_policy = set(policy) - {"on_out_of_range", "confidence"}
    if unknown_policy:
        raise ValueError(f"unknown policy keys: {sorted(unknown_policy)}")
    # Deprecated compatibility with shipped inputs. Validation is always
    # enforced below; this legacy spelling cannot relax the confidence gate.
    if "confidence" in policy and policy["confidence"] != "required":
        raise ValueError("deprecated policy.confidence only permits 'required'")
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
    allowed = {ax.name for ax in atlas.axes}
    for ax in atlas.axes:
        if ax.derived:
            allowed.update(ax.derived["inputs"])
    if handler is _handle_fatigue_bins:
        allowed.update(_FATIGUE_CONTROL_KEYS)
        for bin_point in point.get("tension_range_bins", []):
            if set(bin_point) - {"tension_range_kN", "n_cycles"}:
                raise ValueError("unknown point keys in fatigue bin")
    elif handler is _handle_capacity_demand:
        allowed.update({"demand_kN", "factor_of_safety"})
    elif handler is _handle_annual_damage:
        allowed.update({"design_life_years", "dff"})
    elif handler is _handle_rao:
        allowed.add("period_s")
    unknown_point = set(point) - allowed
    if unknown_point:
        raise ValueError(f"unknown point keys: {sorted(unknown_point)}")

    stale_reason = _staleness(atlas)
    if atlas.provenance.get("kind") != "library" and atlas.validation.get("passes") is not True:
        result = _escalation(atlas, "atlas validation has not passed")
    elif stale_reason is not None:
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
    from digitalmodel.parametric import refresh

    # licensed-solver libraries (#831): staleness judged vs the declared
    # operator expectation, not a recompute.
    if atlas.provenance.get("kind") == "library":
        try:
            reasons = refresh.library_drift(atlas)  # checks THIS atlas, no re-load
        except Exception:
            return "library staleness could not be established"
        if reasons:
            return f"library stale: {'; '.join(reasons)} — licensed run required"
        return None

    workflow_id = atlas.provenance.get("workflow_id")
    stored = atlas.provenance.get("content_fingerprint")
    if not workflow_id or not stored:
        return "atlas fingerprint or workflow identity is missing"

    try:
        current = refresh.content_fingerprint(workflow_id)
    except Exception:
        return "atlas fingerprint could not be recomputed"
    if current != stored:
        return (
            "atlas stale: build basis (spec / template / source / standard) "
            "changed since generation — refresh required"
        )
    return None


def _provenance(atlas: Atlas) -> dict[str, Any]:
    prov = {
        "atlas_id": atlas.atlas_id,
        "code_version": atlas.provenance.get("code_version"),
        "standards": atlas.provenance.get("standards"),
        "response_kwargs": atlas.provenance.get("response_kwargs", {}),
    }
    # Licensed-solver libraries (#831/#1346): surface the solver block so a
    # STUB self-identifies at the query-result surface — a synthetic placeholder
    # (solver.licensed False, version "STUB") must never read like a real cached
    # solver value. (Additive; also closes this gap for the orcaflex/diffraction
    # libraries.)
    if atlas.provenance.get("kind") == "library":
        prov["solver"] = atlas.provenance.get("solver")
    return prov


def _escalation(atlas: Atlas, reason: str) -> dict[str, Any]:
    return {
        "value": None,
        "in_range": False,
        "reason": reason,
        "action": "escalate",
        "disclaimer": ESCALATE_DISCLAIMER,
        "provenance": _provenance(atlas),
    }


# -- per-workflow handlers ---------------------------------------------------


_FATIGUE_CONTROL_KEYS = {
    "tension_range_bins", "design_life_years", "dff", "tension_range_kN", "n_cycles",
}


def _handle_fatigue_bins(atlas: Atlas, point: dict[str, Any]) -> dict[str, Any]:
    """Per-cell damage atlas, Miner-summed over the queried bins, with fatigue
    life derived analytically. Shared by mooring_fatigue (area/sn_curve slice)
    and synthetic_rope_mooring_fatigue (MBL slice) — the slice axes are simply
    whatever point fields are not bin-level / load-case control keys."""
    shared = {k: v for k, v in point.items() if k not in _FATIGUE_CONTROL_KEYS}
    bins = point.get("tension_range_bins")
    if bins is None:
        bins = [{"tension_range_kN": point["tension_range_kN"],
                 "n_cycles": point["n_cycles"]}]
    if not bins or any(
        not math.isfinite(float(b.get("n_cycles", 0))) or float(b.get("n_cycles", 0)) <= 0
        for b in bins
    ):
        return _escalation(atlas, "nonempty fatigue bins with positive finite cycles required")

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

    # local error of the worst bin (#828) — tighter than the global figure
    errors = [atlas.local_error({**shared, **b}) for b in bins]
    if any(e is None or not math.isfinite(e) or not 0 <= e < 1 for e in errors):
        return _escalation(atlas, "fatigue interpolation error not established below unity")
    e = max(errors)
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
        "confidence": {"band": band, "basis": f"local interp error = {e:.4f}"},
        "in_range": True,
        "stale": False,
        "disclaimer": SCREENING_DISCLAIMER,
        "provenance": _provenance(atlas),
    }


def _utilisation_result(atlas: Atlas, u: float, band: list[float], e: float) -> dict[str, Any]:
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
        "confidence": {"band": band, "basis": f"local interp error = {e:.4f}"},
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
    e = atlas.local_error(point)
    if e is None or not 0 <= e < 1:
        return _escalation(atlas, "utilisation interpolation error not established below unity")
    return _utilisation_result(atlas, u, [u * (1 - e), u * (1 + e)], e)


def _handle_capacity_demand(atlas: Atlas, point: dict[str, Any]) -> dict[str, Any]:
    """Boundary class where the atlas predicts a CAPACITY (pile, anchor) and the
    load case (demand, factor of safety) is applied at query time:
    utilisation = demand * FoS / capacity. The atlas axes stay geometry/soil
    only; the demand is not baked into the grid."""
    if "factor_of_safety" not in point:
        raise ValueError("factor_of_safety is required")
    fos = float(point["factor_of_safety"])
    if not math.isfinite(fos) or fos <= 0:
        raise ValueError("factor_of_safety must be positive and finite")
    prediction = atlas.predict(point)
    if not prediction.in_range:
        return _escalation(atlas, prediction.reason)
    capacity = prediction.value
    if capacity <= 0:
        return _escalation(atlas, "non-positive capacity")
    demand = float(point["demand_kN"])
    e = atlas.local_error(point)
    if e is None or not 0 <= e < 1:
        return _escalation(atlas, "capacity interpolation error not established below unity")
    u = demand * fos / capacity
    # capacity carries +/- e error, so utilisation carries -/+ e (inverse)
    band = [demand * fos / (capacity * (1 + e)), demand * fos / (capacity * (1 - e))]
    result = _utilisation_result(atlas, u, band, e)
    if result["in_range"]:
        result["capacity_kN"] = capacity
    result["factor_of_safety"] = fos
    return result


def _handle_value(atlas: Atlas, point: dict[str, Any]) -> dict[str, Any]:
    """Plain value class: interpolate the response and report it with a band
    (rao heave, fpso max line tension)."""
    prediction = atlas.predict(point)
    if not prediction.in_range:
        return _escalation(atlas, prediction.reason)
    v = prediction.value
    e = atlas.local_error(point)
    return {
        "response": atlas.response,
        "value": v,
        "confidence": {"band": [v * (1 - e), v * (1 + e)] if e is not None else None,
                       "basis": f"local interp error = {e:.4f}" if e is not None else "not established"},
        "in_range": True,
        "stale": False,
        "disclaimer": SCREENING_DISCLAIMER,
        "provenance": _provenance(atlas),
    }


def _handle_rao(atlas: Atlas, point: dict[str, Any]) -> dict[str, Any]:
    # query is natural in period; the atlas axis is frequency (the workflow's
    # interpolation space), so convert here, then treat as a plain value.
    lookup = dict(point)
    if "frequency_rad_s" not in lookup and "period_s" in lookup:
        lookup["frequency_rad_s"] = 2.0 * math.pi / float(lookup["period_s"])
    return _handle_value(atlas, lookup)


def _handle_annual_damage(atlas: Atlas, point: dict[str, Any]) -> dict[str, Any]:
    """Single-sea-state spectral fatigue: atlas predicts annual damage; life =
    1/annual_damage, margin against design_life*dff (spectral_fatigue)."""
    prediction = atlas.predict(point)
    if not prediction.in_range:
        return _escalation(atlas, prediction.reason)
    annual = prediction.value
    design_life = float(point["design_life_years"])
    dff = float(point["dff"])
    life = math.inf if annual <= 0 else 1.0 / annual
    dff_margin = math.inf if math.isinf(life) else life / (design_life * dff)
    e = atlas.local_error(point)
    if e is None or not 0 <= e < 1:
        return _escalation(atlas, "annual damage interpolation error not established below unity")
    band = ([math.inf, math.inf] if math.isinf(life)
            else [1.0 / (annual * (1 + e)), 1.0 / (annual * (1 - e))])
    return {
        "response": "fatigue_life_years",
        "value": life,
        "annual_damage": annual,
        "dff_margin": dff_margin,
        "screening_status": "pass" if dff_margin >= 1.0 else "fail",
        "confidence": {"band": band, "basis": f"local interp error = {e:.4f}"},
        "in_range": True,
        "stale": False,
        "disclaimer": SCREENING_DISCLAIMER,
        "provenance": _provenance(atlas),
    }


_HANDLERS: dict[str, Callable[[Atlas, dict[str, Any]], dict[str, Any]]] = {
    "mooring_fatigue": _handle_fatigue_bins,
    "synthetic_rope_mooring_fatigue": _handle_fatigue_bins,
    "code_check": _handle_utilisation,
    "free_span": _handle_utilisation,
    # DNV-RP-F105 free-span screening utilisation (as-surveyed / allowable span):
    # the atlas predicts utilisation directly, threshold at 1.0 (#982 pattern).
    "span_rectification": _handle_utilisation,
    # FOWT watch-circle vs dynamic-cable MBR utilisation (parametrics atlas #975):
    # the atlas predicts utilisation directly, threshold at 1.0.
    "fowt_mooring": _handle_utilisation,
    # AISC lifting-lug / padeye governing utilisation atlas (#979 pattern).
    "lifting_lug": _handle_utilisation,
    # ESP pump-hydraulics governing screening utilisation atlas (#979 pattern).
    "esp_pump_hydraulics": _handle_utilisation,
    # Mudmat bearing-capacity governing screening utilisation atlas (#979 pattern).
    "mudmat_bearing_capacity": _handle_utilisation,
    # API 510/570/653 remaining-life (yr) plain-value atlas (#979 pattern):
    # interpolate the scalar remaining life directly, report it with a band.
    "inspection_planning": _handle_value,
    "rao_tabulation": _handle_rao,
    "pile_capacity": _handle_capacity_demand,
    "anchor_capacity": _handle_capacity_demand,
    "spectral_fatigue": _handle_annual_damage,
    # Combined wave+VIV riser fatigue annual-damage atlas (#979 pattern): the
    # atlas predicts annual damage; the handler derives life + DFF margin.
    "riser_fatigue": _handle_annual_damage,
    "fpso_mooring_full": _handle_value,
    "viv_analysis": _handle_value,
    # weather-window planned-operability % atlas (#979 pattern): a plain value,
    # workability x persistence against a fixed reference hindcast.
    "weather_window": _handle_value,
    # licensed-solver sparse library (#801): exact-match the case key, then
    # interpolate freq x heading within it; an uncovered case escalates.
    "diffraction_library": _handle_value,
    "orcaflex_library": _handle_value,
    # drilling-riser von-Mises dynamic-amplification library (#1346): exact-match
    # the operating-mode key, interpolate offset x current x Hs x Tp within.
    "drilling_riser_envelope": _handle_value,
    # drilling-riser operability exact node-cache (#1283): the atlas predicts the
    # governing utilisation directly; threshold at 1.0 with the straddle rule so a
    # marginal boundary point escalates to the exact analytical run.
    "drilling_riser_operability": _handle_utilisation,
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
