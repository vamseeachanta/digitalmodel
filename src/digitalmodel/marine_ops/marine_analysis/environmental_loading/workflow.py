"""Engine-routed OCIMF environmental loading workflow."""

from __future__ import annotations

import json
from dataclasses import asdict
from pathlib import Path
from typing import Any

from .ocimf import (
    EnvironmentalConditions,
    EnvironmentalForces,
    OCIMFDatabase,
    VesselGeometry,
)


def _resolve_path(cfg: dict[str, Any], value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return Path(cfg.get("_config_dir_path", Path.cwd())) / path


def _loads_payload(result: Any) -> dict[str, dict[str, float]]:
    return {
        "wind": {
            "fx_N": result.wind_fx,
            "fy_N": result.wind_fy,
            "mz_Nm": result.wind_mz,
        },
        "current": {
            "fx_N": result.current_fx,
            "fy_N": result.current_fy,
            "mz_Nm": result.current_mz,
        },
        "total": {
            "fx_N": result.total_fx,
            "fy_N": result.total_fy,
            "mz_Nm": result.total_mz,
        },
    }


class OCIMFWorkflow:
    """Thin adapter around the existing OCIMF database and force calculator."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        workflow_cfg = cfg.get("ocimf", {})
        database = OCIMFDatabase(str(_resolve_path(cfg, workflow_cfg["database"])))
        conditions = EnvironmentalConditions(**workflow_cfg["conditions"])
        geometry = VesselGeometry(**workflow_cfg["vessel_geometry"])
        displacement = workflow_cfg["displacement"]

        calculator = EnvironmentalForces(database)
        result = calculator.calculate_total_forces(
            conditions,
            geometry,
            displacement,
        )
        payload = self._payload(database, conditions, displacement, result)
        cfg["ocimf"] = {**workflow_cfg, "results": payload}
        self._write_outputs(cfg, workflow_cfg.get("output", {}), payload)
        return cfg

    def _payload(
        self,
        database: OCIMFDatabase,
        conditions: EnvironmentalConditions,
        displacement: float,
        result: Any,
    ) -> dict[str, Any]:
        return {
            "loads": _loads_payload(result),
            "wind_coefficients": database.get_coefficients(
                conditions.wind_direction,
                displacement,
            ).to_dict(),
            "current_coefficients": database.get_coefficients(
                conditions.current_direction,
                displacement,
            ).to_dict(),
            "conditions": asdict(conditions),
            "vessel_geometry": asdict(result.geometry),
            "displacement": displacement,
        }

    def _write_outputs(
        self,
        cfg: dict[str, Any],
        output_cfg: dict[str, str],
        payload: dict[str, Any],
    ) -> None:
        if "result_json" not in output_cfg:
            return
        path = _resolve_path(cfg, output_cfg["result_json"])
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
        cfg.setdefault("outputs", {})["result_json"] = str(path)
