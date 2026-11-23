"""Engine-routed FPSO spread-mooring screening workflow."""

from __future__ import annotations

import json
from dataclasses import asdict, replace
from pathlib import Path
from typing import Any

import numpy as np

from . import (
    AnchorProperties,
    ConditionType,
    DesignLoadCase,
    EnvironmentalConditions,
    MooringDesigner,
    MooringLine,
    MooringSystem,
    MooringType,
    VesselParticulars,
    get_material,
)


class FPSOMooringWorkflow:
    """Prepare and screen a symmetric FPSO spread-mooring system."""

    def router(self, cfg: dict) -> dict:
        system = self._build_system(cfg)
        environment = self._build_environment(cfg)
        designer = MooringDesigner(system)
        load_case = DesignLoadCase(
            name=cfg.get("analysis", {}).get("load_case", "intact_screening"),
            condition=ConditionType.INTACT,
            environment=environment,
        )
        dynamic_factor = cfg.get("analysis", {}).get("dynamic_factor", 1.0)
        loads = designer.calculate_environmental_loads(environment)
        results = designer.analyze_intact_condition(
            load_case,
            dynamic_factor=dynamic_factor,
        )
        summary = designer.generate_design_summary(results)

        cfg["fpso_mooring"] = self._output(loads, summary, results)
        summary_path = self._write_summary(cfg)
        cfg.setdefault("outputs", {})["summary_json"] = str(summary_path)
        return cfg

    def _build_system(self, cfg: dict) -> MooringSystem:
        vessel = VesselParticulars(**cfg["vessel"])
        mooring = cfg["mooring"]
        lines = self._build_lines(mooring)
        return MooringSystem(
            system_type=MooringType.SPREAD,
            water_depth=mooring["water_depth"],
            lines=lines,
            vessel=vessel,
            name=mooring.get("name", "fpso_spread_mooring"),
        )

    def _build_lines(self, mooring: dict) -> list[MooringLine]:
        n_lines = mooring["n_lines"]
        material = replace(
            get_material(mooring["material"]),
            length=mooring["line_length"],
        )
        lines = []
        for idx in range(n_lines):
            angle = idx * 360.0 / n_lines
            angle_rad = np.radians(angle)
            lines.append(self._build_line(idx, angle_rad, mooring, material))
        return lines

    def _build_line(
        self,
        idx: int,
        angle_rad: float,
        mooring: dict,
        material: Any,
    ) -> MooringLine:
        anchor_radius = mooring["anchor_radius"]
        fairlead_radius = mooring["fairlead_radius"]
        water_depth = mooring["water_depth"]
        fairlead_depth = mooring["fairlead_depth"]
        anchor = AnchorProperties(
            anchor_type=mooring.get("anchor_type", "drag"),
            holding_capacity=mooring["anchor_holding_capacity"],
            location=(
                anchor_radius * np.cos(angle_rad),
                anchor_radius * np.sin(angle_rad),
                -water_depth,
            ),
        )
        return MooringLine(
            line_id=f"ML{idx + 1}",
            segments=[material],
            anchor=anchor,
            fairlead_location=(
                fairlead_radius * np.cos(angle_rad),
                fairlead_radius * np.sin(angle_rad),
                -fairlead_depth,
            ),
            pretension=mooring["pretension"],
        )

    def _build_environment(self, cfg: dict) -> EnvironmentalConditions:
        return EnvironmentalConditions(**cfg["environment"])

    def _output(self, loads, summary: dict, results: list) -> dict:
        return _json_safe({
            "environmental_loads": asdict(loads),
            "summary": summary,
            "line_results": [asdict(result) for result in results],
        })

    def _write_summary(self, cfg: dict) -> Path:
        output_cfg = cfg.get("output", {})
        output_path = output_cfg.get("summary_json")
        if output_path:
            path = Path(output_path)
        else:
            path = self._result_folder(cfg) / f"{self._result_file_base(cfg)}_summary.json"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(cfg["fpso_mooring"], indent=2))
        return path

    def _result_folder(self, cfg: dict) -> Path:
        analysis = cfg.get("Analysis", {})
        if analysis.get("result_folder"):
            return Path(analysis["result_folder"])
        if "_config_dir_path" in cfg:
            return Path(cfg["_config_dir_path"]) / "results"
        return Path("results")

    def _result_file_base(self, cfg: dict) -> str:
        analysis = cfg.get("Analysis", {})
        if analysis.get("file_name"):
            return Path(str(analysis["file_name"])).stem
        if cfg.get("_config_file_path"):
            return Path(str(cfg["_config_file_path"])).stem
        return "input"


def _json_safe(value: Any) -> Any:
    if isinstance(value, dict):
        return {key: _json_safe(item) for key, item in value.items()}
    if isinstance(value, list):
        return [_json_safe(item) for item in value]
    if isinstance(value, tuple):
        return [_json_safe(item) for item in value]
    if isinstance(value, np.generic):
        return value.item()
    return value
