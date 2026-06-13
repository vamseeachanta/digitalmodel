# ABOUTME: Engine-routed geotechnical screeners (pile axial capacity, anchor holding).
# ABOUTME: Routes the `geotechnical` basename to existing offline calculators and writes a JSON summary.
"""Engine-routed geotechnical screeners.

Supported `geotechnical.calculation` values:
- ``pile_axial_capacity``  -> API RP 2GEO alpha-method axial pile capacity (single or multilayer clay)
- ``drag_anchor``          -> DNV-RP-E302 / API RP 2SK empirical drag-anchor holding capacity
- ``suction_anchor``       -> DNV-RP-E303 suction-caisson capacity in clay

Each calculation wraps the matching pure-function calculator, applies a user
supplied design factor of safety, and writes a JSON summary sidecar.
"""

from __future__ import annotations

import json
from dataclasses import asdict
from pathlib import Path
from typing import Any

from digitalmodel.geotechnical.anchors import (
    drag_anchor_capacity,
    suction_anchor_capacity,
)
from digitalmodel.geotechnical.pile_capacity import (
    alpha_method_capacity,
    alpha_method_capacity_multilayer,
)


def _resolve_dir(cfg: dict[str, Any], value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return Path(cfg.get("_config_dir_path", Path.cwd())) / path


def _write_summary(cfg: dict[str, Any], output_cfg: dict[str, Any], payload: dict[str, Any]) -> Path:
    directory = _resolve_dir(cfg, output_cfg.get("directory", "results/geotechnical"))
    directory.mkdir(parents=True, exist_ok=True)
    filename = output_cfg.get("summary_json", "geotechnical_summary.json")
    summary_path = directory / filename
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    cfg.setdefault("outputs", {})["directory"] = str(directory)
    cfg["outputs"]["summary_json"] = str(summary_path)
    return summary_path


class GeotechnicalWorkflow:
    """Route selected geotechnical calculations through existing helpers."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        workflow_cfg = cfg.get("geotechnical", {})
        calculation = workflow_cfg.get("calculation")
        if calculation == "pile_axial_capacity":
            return self._run_pile_axial_capacity(cfg, workflow_cfg)
        if calculation == "drag_anchor":
            return self._run_drag_anchor(cfg, workflow_cfg)
        if calculation == "suction_anchor":
            return self._run_suction_anchor(cfg, workflow_cfg)
        raise ValueError(f"Unsupported geotechnical calculation: {calculation}")

    def _run_pile_axial_capacity(
        self, cfg: dict[str, Any], workflow_cfg: dict[str, Any]
    ) -> dict[str, Any]:
        params = workflow_cfg["pile_axial_capacity"]
        pile = params["pile"]
        design = params.get("design", {})
        nc = float(pile.get("Nc", 9.0))

        layers = params.get("soil_layers")
        if layers:
            tuple_layers = [
                (float(layer["thickness_m"]), float(layer["Su_kpa"]), float(layer["sigma_v_kpa"]))
                for layer in layers
            ]
            result = alpha_method_capacity_multilayer(
                D=float(pile["diameter_m"]), layers=tuple_layers, Nc=nc
            )
        else:
            soil = params["soil"]
            result = alpha_method_capacity(
                D=float(pile["diameter_m"]),
                L=float(pile["embedded_length_m"]),
                Su=float(soil["Su_kpa"]),
                sigma_v=float(soil["sigma_v_kpa"]),
                Nc=nc,
            )

        fos = float(design.get("factor_of_safety", 2.0))
        applied_load_kn = design.get("applied_load_kn")
        allowable_kn = result.total_capacity_kn / fos
        summary = {
            "calculation": "pile_axial_capacity",
            "standard": result.standard,
            "result": asdict(result),
            "design": {
                "factor_of_safety": fos,
                "allowable_capacity_kn": allowable_kn,
            },
        }
        if applied_load_kn is not None:
            applied = float(applied_load_kn)
            summary["design"]["applied_load_kn"] = applied
            summary["design"]["utilization"] = applied / allowable_kn
            summary["design"]["status"] = "PASS" if applied <= allowable_kn else "FAIL"

        summary["summary_json"] = str(
            _write_summary(cfg, params.get("outputs", {}), summary)
        )
        cfg["geotechnical"] = {
            "calculation": "pile_axial_capacity",
            "pile_axial_capacity": {**params, **summary},
        }
        return cfg

    def _run_drag_anchor(
        self, cfg: dict[str, Any], workflow_cfg: dict[str, Any]
    ) -> dict[str, Any]:
        params = workflow_cfg["drag_anchor"]
        anchor = params["anchor"]
        design = params.get("design", {})
        result = drag_anchor_capacity(
            anchor_weight_kn=float(anchor["weight_kn"]),
            soil_type=str(anchor["soil_type"]),
            anchor_type=str(anchor["anchor_type"]),
        )
        fos = float(design.get("factor_of_safety", 1.5))
        allowable_kn = result.holding_capacity_kn / fos
        summary = {
            "calculation": "drag_anchor",
            "standard": result.standard,
            "result": asdict(result),
            "design": {
                "factor_of_safety": fos,
                "allowable_holding_kn": allowable_kn,
            },
        }
        required = design.get("required_holding_kn")
        if required is not None:
            req = float(required)
            summary["design"]["required_holding_kn"] = req
            summary["design"]["utilization"] = req / allowable_kn
            summary["design"]["status"] = "PASS" if req <= allowable_kn else "FAIL"

        summary["summary_json"] = str(
            _write_summary(cfg, params.get("outputs", {}), summary)
        )
        cfg["geotechnical"] = {
            "calculation": "drag_anchor",
            "drag_anchor": {**params, **summary},
        }
        return cfg

    def _run_suction_anchor(
        self, cfg: dict[str, Any], workflow_cfg: dict[str, Any]
    ) -> dict[str, Any]:
        params = workflow_cfg["suction_anchor"]
        caisson = params["caisson"]
        soil = params["soil"]
        design = params.get("design", {})
        result = suction_anchor_capacity(
            diameter_m=float(caisson["diameter_m"]),
            length_m=float(caisson["length_m"]),
            su_kpa=float(soil["Su_kpa"]),
            alpha=float(soil.get("alpha", 0.65)),
            nc=float(soil.get("Nc", 9.0)),
            wall_thickness_m=float(caisson.get("wall_thickness_m", 0.04)),
        )
        fos = float(design.get("factor_of_safety", 1.5))
        allowable_kn = result.total_capacity_kn / fos
        summary = {
            "calculation": "suction_anchor",
            "standard": result.standard,
            "result": asdict(result),
            "design": {
                "factor_of_safety": fos,
                "allowable_capacity_kn": allowable_kn,
            },
        }
        summary["summary_json"] = str(
            _write_summary(cfg, params.get("outputs", {}), summary)
        )
        cfg["geotechnical"] = {
            "calculation": "suction_anchor",
            "suction_anchor": {**params, **summary},
        }
        return cfg
