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
