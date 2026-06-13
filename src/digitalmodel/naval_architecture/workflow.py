"""Engine-routed naval architecture screeners."""

from __future__ import annotations

from pathlib import Path
from typing import Any

from digitalmodel.naval_architecture.rudder_stock_torque import (
    run_rudder_stock_torque_sweep,
    validate_rudder_stock_torque_input,
    write_rudder_stock_torque_results,
)
from digitalmodel.naval_architecture.yaw_moment import (
    run_yaw_moment_sweep,
    validate_yaw_moment_input,
    write_yaw_moment_results,
)


def _resolve_dir(cfg: dict[str, Any], value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return Path(cfg.get("_config_dir_path", Path.cwd())) / path


def _stringify_paths(value: Any) -> Any:
    if isinstance(value, Path):
        return str(value)
    if isinstance(value, dict):
        return {key: _stringify_paths(item) for key, item in value.items()}
    if isinstance(value, list):
        return [_stringify_paths(item) for item in value]
    return value


class NavalArchitectureWorkflow:
    """Route selected naval architecture calculations through existing helpers."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        workflow_cfg = cfg.get("naval_arch", {})
        calculation = workflow_cfg.get("calculation")
        if calculation == "yaw_moment":
            return self._run_yaw_moment(cfg, workflow_cfg)
        if calculation == "rudder_stock_torque":
            return self._run_rudder_stock_torque(cfg, workflow_cfg)
        raise ValueError(f"Unsupported naval_arch calculation: {calculation}")

    def _run_yaw_moment(
        self,
        cfg: dict[str, Any],
        workflow_cfg: dict[str, Any],
    ) -> dict[str, Any]:
        input_payload = workflow_cfg["yaw_moment"]
        config = validate_yaw_moment_input(input_payload)
        result = run_yaw_moment_sweep(config)
        chart_formats = config.chart_formats if config.chart_enabled else ()
        output_dir = _resolve_dir(cfg, config.output_directory)
        manifest = write_yaw_moment_results(
            result,
            output_dir,
            table_formats=config.table_formats,
            chart_formats=chart_formats,
        )
        cfg["naval_arch"] = {
            "calculation": "yaw_moment",
            "yaw_moment": {
                **input_payload,
                "result": result,
                "artifacts": _stringify_paths(manifest),
            },
        }
        cfg.setdefault("outputs", {})["directory"] = str(output_dir)
        return cfg

    def _run_rudder_stock_torque(
        self,
        cfg: dict[str, Any],
        workflow_cfg: dict[str, Any],
    ) -> dict[str, Any]:
        input_payload = workflow_cfg["rudder_stock_torque"]
        config = validate_rudder_stock_torque_input(input_payload)
        result = run_rudder_stock_torque_sweep(config)
        chart_formats = config.chart_formats if config.chart_enabled else ()
        output_dir = _resolve_dir(cfg, config.output_directory)
        manifest = write_rudder_stock_torque_results(
            result,
            output_dir,
            table_formats=config.table_formats,
            chart_formats=chart_formats,
        )
        cfg["naval_arch"] = {
            "calculation": "rudder_stock_torque",
            "rudder_stock_torque": {
                **input_payload,
                "result": result,
                "artifacts": _stringify_paths(manifest),
            },
        }
        cfg.setdefault("outputs", {})["directory"] = str(output_dir)
        return cfg
