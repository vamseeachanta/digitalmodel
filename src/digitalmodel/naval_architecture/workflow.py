"""Engine-routed naval architecture screeners."""

from __future__ import annotations

import json
from dataclasses import asdict, is_dataclass
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


def _json_ready(value: Any) -> Any:
    if is_dataclass(value):
        return _json_ready(asdict(value))
    if isinstance(value, Path):
        return str(value)
    if isinstance(value, dict):
        return {key: _json_ready(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_json_ready(item) for item in value]
    return value


def _write_summary(
    output_dir: Path, stem: str, result: dict[str, Any]
) -> dict[str, Path]:
    output_dir.mkdir(parents=True, exist_ok=True)
    summary_json = output_dir / f"{stem}_summary.json"
    with summary_json.open("w", encoding="utf-8") as stream:
        json.dump(_json_ready(result), stream, indent=2, sort_keys=True)
        stream.write("\n")
    return {"summary_json": summary_json}


class NavalArchitectureWorkflow:
    """Route selected naval architecture calculations through existing helpers."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        workflow_cfg = cfg.get("naval_arch", {})
        calculation = workflow_cfg.get("calculation")
        handlers = {
            "yaw_moment": self._run_yaw_moment,
            "rudder_stock_torque": self._run_rudder_stock_torque,
            "damage_stability": self._run_damage_stability,
            "platform_stability": self._run_platform_stability,
            "hull_resistance": self._run_hull_resistance,
            "hull_seakeeping": self._run_hull_seakeeping,
        }
        if calculation in handlers:
            return handlers[calculation](cfg, workflow_cfg)
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

    def _run_damage_stability(
        self,
        cfg: dict[str, Any],
        workflow_cfg: dict[str, Any],
    ) -> dict[str, Any]:
        from digitalmodel.naval_architecture.workflow_calculations import (
            run_damage_stability,
        )

        return self._run_calculation(
            cfg, workflow_cfg, "damage_stability", run_damage_stability
        )

    def _run_platform_stability(
        self,
        cfg: dict[str, Any],
        workflow_cfg: dict[str, Any],
    ) -> dict[str, Any]:
        from digitalmodel.naval_architecture.workflow_calculations import (
            run_platform_stability,
        )

        return self._run_calculation(
            cfg, workflow_cfg, "platform_stability", run_platform_stability
        )

    def _run_hull_resistance(
        self,
        cfg: dict[str, Any],
        workflow_cfg: dict[str, Any],
    ) -> dict[str, Any]:
        from digitalmodel.naval_architecture.workflow_calculations import (
            run_hull_resistance,
        )

        return self._run_calculation(
            cfg, workflow_cfg, "hull_resistance", run_hull_resistance
        )

    def _run_hull_seakeeping(
        self,
        cfg: dict[str, Any],
        workflow_cfg: dict[str, Any],
    ) -> dict[str, Any]:
        from digitalmodel.naval_architecture.workflow_calculations import (
            run_hull_seakeeping,
        )

        return self._run_calculation(
            cfg, workflow_cfg, "hull_seakeeping", run_hull_seakeeping
        )

    def _run_calculation(
        self,
        cfg: dict[str, Any],
        workflow_cfg: dict[str, Any],
        calculation: str,
        runner: Any,
    ) -> dict[str, Any]:
        input_payload = workflow_cfg[calculation]
        result = runner(input_payload)
        return self._store_result(
            cfg,
            calculation,
            input_payload,
            result,
            calculation,
        )

    def _store_result(
        self,
        cfg: dict[str, Any],
        calculation: str,
        input_payload: dict[str, Any],
        result: dict[str, Any],
        output_stem: str,
    ) -> dict[str, Any]:
        output_dir = _resolve_dir(
            cfg,
            input_payload.get("outputs", {}).get("directory", f"results/{calculation}"),
        )
        manifest = _write_summary(output_dir, output_stem, result)
        cfg["naval_arch"] = {
            "calculation": calculation,
            calculation: {
                **input_payload,
                "result": _json_ready(result),
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
