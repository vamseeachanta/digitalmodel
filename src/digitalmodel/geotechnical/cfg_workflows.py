# ABOUTME: Thin engine-routed wrappers for geotechnical registry workflows.
# ABOUTME: Converts cfg blocks into existing pure geotechnical calculator calls.
"""Configuration-routed geotechnical workflows."""

from __future__ import annotations

import json
from dataclasses import asdict
from pathlib import Path
from typing import Any


def _resolve_dir(cfg: dict[str, Any], value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return Path(cfg.get("_config_dir_path", Path.cwd())) / path


def _write_summary(
    cfg: dict[str, Any],
    output_cfg: dict[str, Any],
    default_directory: str,
    default_filename: str,
    payload: dict[str, Any],
) -> Path:
    directory = _resolve_dir(cfg, output_cfg.get("directory", default_directory))
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get("summary_json", default_filename)
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    cfg.setdefault("outputs", {})["directory"] = str(directory)
    cfg["outputs"]["summary_json"] = str(summary_path)
    return summary_path


def _design_status(required: float | None, allowable: float) -> dict[str, Any]:
    design: dict[str, Any] = {"allowable_capacity_kn": allowable}
    if required is not None:
        design["required_capacity_kn"] = required
        design["utilization"] = required / allowable
        design["status"] = "PASS" if required <= allowable else "FAIL"
    return design


class AnchorCapacityWorkflow:
    """Run drag-anchor and suction-caisson capacity calculators from cfg."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        from digitalmodel.geotechnical.anchors import (
            drag_anchor_capacity,
            suction_anchor_capacity,
        )

        workflow_cfg = cfg.get("anchor_capacity", {})
        results: dict[str, Any] = {}
        design: dict[str, Any] = {}

        if "drag_anchor" in workflow_cfg:
            drag = workflow_cfg["drag_anchor"]
            result = drag_anchor_capacity(
                anchor_weight_kn=float(drag["weight_kn"]),
                soil_type=str(drag["soil_type"]),
                anchor_type=str(drag["anchor_type"]),
            )
            fos = float(drag.get("design", {}).get("factor_of_safety", 1.5))
            required = drag.get("design", {}).get("required_holding_kn")
            results["drag_anchor"] = asdict(result)
            drag_design = _design_status(
                None if required is None else float(required),
                result.holding_capacity_kn / fos,
            )
            drag_design["factor_of_safety"] = fos
            design["drag_anchor"] = drag_design

        if "suction_anchor" in workflow_cfg:
            suction = workflow_cfg["suction_anchor"]
            caisson = suction["caisson"]
            soil = suction["soil"]
            result = suction_anchor_capacity(
                diameter_m=float(caisson["diameter_m"]),
                length_m=float(caisson["length_m"]),
                su_kpa=float(soil["Su_kpa"]),
                alpha=float(soil.get("alpha", 0.65)),
                nc=float(soil.get("Nc", 9.0)),
                wall_thickness_m=float(caisson.get("wall_thickness_m", 0.04)),
            )
            fos = float(suction.get("design", {}).get("factor_of_safety", 1.5))
            required = suction.get("design", {}).get("required_capacity_kn")
            results["suction_anchor"] = asdict(result)
            suction_design = _design_status(
                None if required is None else float(required),
                result.total_capacity_kn / fos,
            )
            suction_design["factor_of_safety"] = fos
            design["suction_anchor"] = suction_design

        statuses = [item.get("status") for item in design.values()]
        design["status"] = (
            "PASS" if all(status == "PASS" for status in statuses) else "FAIL"
        )
        payload = {**workflow_cfg, "results": results, "design": design}
        payload["summary_json"] = str(
            _write_summary(
                cfg,
                workflow_cfg.get("outputs", {}),
                "results/anchor_capacity",
                "anchor_capacity_summary.json",
                payload,
            )
        )
        cfg["anchor_capacity"] = payload
        return cfg


class PileCapacityWorkflow:
    """Run API RP 2GEO alpha-method pile capacity from cfg."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        from digitalmodel.geotechnical.pile_capacity import (
            alpha_method_capacity,
            alpha_method_capacity_multilayer,
        )

        workflow_cfg = cfg.get("pile_capacity", {})
        pile = workflow_cfg["pile"]
        nc = float(pile.get("Nc", 9.0))
        if workflow_cfg.get("soil_layers"):
            layers = [
                (
                    float(layer["thickness_m"]),
                    float(layer["Su_kpa"]),
                    float(layer["sigma_v_kpa"]),
                )
                for layer in workflow_cfg["soil_layers"]
            ]
            result = alpha_method_capacity_multilayer(
                D=float(pile["diameter_m"]), layers=layers, Nc=nc
            )
        else:
            soil = workflow_cfg["soil"]
            result = alpha_method_capacity(
                D=float(pile["diameter_m"]),
                L=float(pile["embedded_length_m"]),
                Su=float(soil["Su_kpa"]),
                sigma_v=float(soil["sigma_v_kpa"]),
                Nc=nc,
            )

        design_cfg = workflow_cfg.get("design", {})
        fos = float(design_cfg.get("factor_of_safety", 2.0))
        applied = design_cfg.get("applied_load_kn")
        design = _design_status(
            None if applied is None else float(applied),
            result.total_capacity_kn / fos,
        )
        design["factor_of_safety"] = fos
        payload = {
            **workflow_cfg,
            "standard": result.standard,
            "result": asdict(result),
        }
        payload["design"] = design
        payload["summary_json"] = str(
            _write_summary(
                cfg,
                workflow_cfg.get("outputs", {}),
                "results/pile_capacity",
                "pile_capacity_summary.json",
                payload,
            )
        )
        cfg["pile_capacity"] = payload
        return cfg


class ScourWorkflow:
    """Run DNV-RP-F107 scour calculators from cfg."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        from digitalmodel.geotechnical.scour import (
            monopile_scour_depth,
            pipeline_scour_depth,
            rock_armour_thickness,
        )

        workflow_cfg = cfg.get("scour", {})
        results: dict[str, Any] = {}
        if "pipeline" in workflow_cfg:
            pipeline = workflow_cfg["pipeline"]
            results["pipeline"] = asdict(pipeline_scour_depth(**pipeline))
        if "monopile" in workflow_cfg:
            monopile = workflow_cfg["monopile"]
            results["monopile"] = asdict(monopile_scour_depth(**monopile))
        if "rock_armour" in workflow_cfg:
            armour = workflow_cfg["rock_armour"]
            results["rock_armour"] = asdict(rock_armour_thickness(**armour))

        payload = {**workflow_cfg, "results": results}
        payload["summary_json"] = str(
            _write_summary(
                cfg,
                workflow_cfg.get("outputs", {}),
                "results/scour",
                "scour_summary.json",
                payload,
            )
        )
        cfg["scour"] = payload
        return cfg


class MudmatBearingCapacityWorkflow:
    """Run mudmat (shallow foundation) bearing-capacity screening from cfg."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        from digitalmodel.geotechnical.mudmat import mudmat_bearing_capacity

        workflow_cfg = cfg.get("mudmat_bearing_capacity", {})
        foundation = workflow_cfg["foundation"]
        soil = workflow_cfg["soil"]
        loads = workflow_cfg["loads"]

        result = mudmat_bearing_capacity(
            width_b_m=float(foundation["width_B_m"]),
            length_l_m=float(foundation["length_L_m"]),
            embedment_depth_m=float(foundation.get("embedment_depth_m", 0.0)),
            condition=str(soil["condition"]),
            submerged_unit_weight_kn_m3=float(soil["submerged_unit_weight_kN_m3"]),
            vertical_load_kn=float(loads["vertical_kN"]),
            moment_knm=float(loads.get("moment_kNm", 0.0)),
            undrained_shear_strength_kpa=(
                None
                if soil.get("undrained_shear_strength_kpa") is None
                else float(soil["undrained_shear_strength_kpa"])
            ),
            friction_angle_deg=(
                None
                if soil.get("friction_angle_deg") is None
                else float(soil["friction_angle_deg"])
            ),
            effective_cohesion_kpa=float(soil.get("effective_cohesion_kpa", 0.0)),
            interface_friction_angle_deg=(
                None
                if soil.get("interface_friction_angle_deg") is None
                else float(soil["interface_friction_angle_deg"])
            ),
        )

        fos = float(workflow_cfg.get("design", {}).get("factor_of_safety", 2.0))
        applied_v = float(loads["vertical_kN"])
        applied_h = float(loads.get("horizontal_kN", 0.0))
        allowable_v = result.vertical_capacity_kn / fos
        allowable_h = result.sliding_capacity_kn / fos
        bearing_util = applied_v / allowable_v if allowable_v > 0 else float("inf")
        sliding_util = (
            (applied_h / allowable_h) if applied_h > 0 and allowable_h > 0 else 0.0
        )
        governing = "bearing" if bearing_util >= sliding_util else "sliding"
        passes = bearing_util <= 1.0 and sliding_util <= 1.0

        design = {
            "factor_of_safety": fos,
            "allowable_vertical_capacity_kn": allowable_v,
            "allowable_sliding_capacity_kn": allowable_h,
            "bearing_utilization": bearing_util,
            "sliding_utilization": sliding_util,
            "governing_check": governing,
            "screening_status": "pass" if passes else "fail",
        }
        payload = {
            **workflow_cfg,
            "standard": result.standard,
            "result": asdict(result),
            "design": design,
        }
        payload["summary_json"] = str(
            _write_summary(
                cfg,
                workflow_cfg.get("outputs", {}),
                "results/mudmat_bearing_capacity",
                "mudmat_bearing_capacity_summary.json",
                payload,
            )
        )
        cfg["mudmat_bearing_capacity"] = payload
        cfg["screening_status"] = design["screening_status"]
        return cfg
