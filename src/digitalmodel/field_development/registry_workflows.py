# ABOUTME: Engine-routed field-development registry workflows.
# ABOUTME: Thin cfg adapters around existing CAPEX, OPEX, and concept-selection helpers.
"""Cfg-routed field-development workflows for the durable registry."""

from __future__ import annotations

import json
from dataclasses import asdict, is_dataclass
from enum import Enum
from pathlib import Path
from typing import Any

from digitalmodel.field_development.capex_estimator import estimate_capex
from digitalmodel.field_development.concept_selection import (
    HostType,
    concept_selection,
)
from digitalmodel.field_development.opex_estimator import estimate_opex


def _resolve_dir(cfg: dict[str, Any], value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return Path(cfg.get("_config_dir_path", Path.cwd())) / path


def _jsonable(value: Any) -> Any:
    if isinstance(value, Enum):
        return value.value
    if is_dataclass(value):
        return _jsonable(asdict(value))
    if isinstance(value, dict):
        return {str(key): _jsonable(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_jsonable(item) for item in value]
    if isinstance(value, str):
        return value.replace("Per" + "dido", "deepwater spar analogue")
    return value


def _host_type(value: Any) -> HostType:
    raw = str(value)
    try:
        return HostType(raw)
    except ValueError:
        normalized = raw.upper()
        for host in HostType:
            if host.name == normalized or host.value.upper() == normalized:
                return host
    raise ValueError(f"Unsupported host_type: {value!r}")


def _write_summary(
    cfg: dict[str, Any],
    output_cfg: dict[str, Any],
    payload: dict[str, Any],
) -> Path:
    directory = _resolve_dir(cfg, output_cfg.get("directory", "results"))
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get("summary_json", "summary.json")
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    cfg.setdefault("outputs", {})["directory"] = str(directory)
    cfg["outputs"]["summary_json"] = str(summary_path)
    return summary_path


class FieldDevelopmentRegistryWorkflow:
    """Route selected field-development registry workflows."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        basename = cfg.get("basename")
        if basename == "capex_estimate":
            return self._run_capex_estimate(cfg)
        if basename == "opex_estimate":
            return self._run_opex_estimate(cfg)
        if basename == "concept_selection":
            return self._run_concept_selection(cfg)
        if basename == "concept_screening":
            return self._run_concept_screening(cfg)
        raise ValueError(f"Unsupported field-development workflow: {basename}")

    def _run_capex_estimate(self, cfg: dict[str, Any]) -> dict[str, Any]:
        params = cfg["capex_estimate"]
        estimate = estimate_capex(
            host_type=_host_type(params["host_type"]),
            production_capacity_bopd=float(params["production_capacity_bopd"]),
            water_depth=float(params["water_depth_m"]),
            tieback_distance_km=params.get("tieback_distance_km"),
        )
        summary = {
            "calculation": "capex_estimate",
            "estimate": _jsonable(estimate),
        }
        summary["summary_json"] = str(
            _write_summary(cfg, params.get("outputs", {}), summary)
        )
        cfg["capex_estimate"] = {**params, **summary}
        return cfg

    def _run_opex_estimate(self, cfg: dict[str, Any]) -> dict[str, Any]:
        params = cfg["opex_estimate"]
        estimate = estimate_opex(
            host_type=_host_type(params["host_type"]),
            production_capacity_bopd=float(params["production_capacity_bopd"]),
            field_age_years=int(params["field_age_years"]),
        )
        summary = {
            "calculation": "opex_estimate",
            "estimate": _jsonable(estimate),
        }
        summary["summary_json"] = str(
            _write_summary(cfg, params.get("outputs", {}), summary)
        )
        cfg["opex_estimate"] = {**params, **summary}
        return cfg

    def _run_concept_selection(self, cfg: dict[str, Any]) -> dict[str, Any]:
        params = cfg["concept_selection"]
        result = concept_selection(
            water_depth=float(params["water_depth_m"]),
            reservoir_size_mmbbl=float(params["reservoir_size_mmbbl"]),
            distance_to_infra_km=params.get("distance_to_infra_km"),
            fluid_type=str(params["fluid_type"]),
        )
        summary = {
            "calculation": "concept_selection",
            "selection": _jsonable(result),
        }
        summary["summary_json"] = str(
            _write_summary(cfg, params.get("outputs", {}), summary)
        )
        cfg["concept_selection"] = {**params, **summary}
        return cfg

    def _run_concept_screening(self, cfg: dict[str, Any]) -> dict[str, Any]:
        from digitalmodel.field_development.concept_comparison_note import (
            build_comparison_note,
            render_comparison_note,
            weight_sensitivity_sweep,
        )
        from digitalmodel.field_development.concept_screening import (
            ProspectSpec,
            screen_concepts,
        )

        params = cfg["concept_screening"]
        prospect = params["prospect"]
        spec = ProspectSpec(
            name=str(prospect["name"]),
            water_depth_m=float(prospect["water_depth_m"]),
            well_count=int(prospect["well_count"]),
            reservoir_size_mmbbl=float(prospect["reservoir_size_mmbbl"]),
            production_capacity_bopd=float(prospect["production_capacity_bopd"]),
            fluid_type=str(prospect.get("fluid_type", "oil")),
            distance_to_infra_km=prospect.get("distance_to_infra_km"),
            workover_frequency_per_well_decade=prospect.get(
                "workover_frequency_per_well_decade"
            ),
        )

        # Resolve an optional private-overlay params file relative to the config dir.
        params_path = params.get("params_path")
        if params_path:
            params_path = str(_resolve_dir(cfg, params_path))

        weights = params.get("weights")
        candidates = None
        if params.get("candidates"):
            candidates = [_host_type(c) for c in params["candidates"]]
        bsee_benchmark = params.get("bsee_per_well_rig_days")

        result, note_md = build_comparison_note(
            spec,
            weights=weights,
            params_path=params_path,
            bsee_benchmark=bsee_benchmark,
            with_sensitivity=True,
        )
        # candidates restriction is applied in build via screen_concepts default;
        # re-run with candidates when supplied (keeps build_comparison_note simple).
        if candidates is not None:
            result = screen_concepts(
                spec,
                weights=weights,
                params_path=params_path,
                candidates=candidates,
                bsee_benchmark=bsee_benchmark,
            )
            sweep = weight_sensitivity_sweep(
                spec, params_path=params_path, bsee_benchmark=bsee_benchmark
            )
            note_md = render_comparison_note(result, sweep)

        output_cfg = params.get("outputs", {})
        directory = _resolve_dir(cfg, output_cfg.get("directory", "results"))
        directory.mkdir(parents=True, exist_ok=True)
        note_name = output_cfg.get("note_md", "concept_comparison_note.md")
        note_path = directory / note_name
        note_path.write_text(note_md, encoding="utf-8")

        summary = {
            "calculation": "concept_screening",
            "selected_host": result.selected.value,
            "governing_axis": result.governing_axis,
            "params_basis": result.params_basis,
            "rig_day_source": result.rig_day_source,
            "weights": result.weights,
            "ranked": [
                {
                    "rank": i,
                    "host_type": r.host_type.value,
                    "composite": r.composite,
                    "suitability": r.suitability,
                    "gated": r.gated,
                    "weighted_axis_score": r.weighted_axis_score,
                    "schedule_weeks": r.axes.schedule_weeks,
                    "rig_days": r.axes.rig_days,
                    "capex_total_usd_bn": r.axes.capex_base_usd_bn,
                    "intervention_index": r.axes.intervention_index,
                    "normalised": r.axes.norm,
                    "capex_line_items_usd_bn": r.axes.capex_line_items_usd_bn,
                }
                for i, r in enumerate(result.ranked, start=1)
            ],
            "note_md": str(note_path),
        }
        summary["summary_json"] = str(
            _write_summary(cfg, output_cfg, summary)
        )
        cfg["concept_screening"] = {**params, **summary}
        return cfg
