"""Workflow adapter for offline passing-ship force calculations."""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Any

from digitalmodel.hydrodynamics.passing_ship.calculator import PassingShipCalculator
from digitalmodel.hydrodynamics.passing_ship.configuration import (
    CalculationConfig,
    EnvironmentalConfig,
    VesselConfig,
)


REPO_ROOT = Path(__file__).resolve().parents[4]


def router(cfg: dict) -> dict:
    settings = cfg.get("passing_ship") or {}
    calc = PassingShipCalculator(
        moored_vessel=VesselConfig(**settings["moored_vessel"]),
        passing_vessel=VesselConfig(**settings["passing_vessel"]),
        environment=EnvironmentalConfig(**settings.get("environment", {})),
        calculation_config=CalculationConfig(**settings.get("calculation", {})),
    )
    scenario = _scenario(calc.calculation_config)
    forces = calc.calculate_forces(**scenario, use_cache=False)

    csv_path = _results_csv_path(cfg, settings)
    row = _result_row(scenario, forces)
    _write_results_csv(csv_path, [row])

    cfg["passing_ship"] = {
        "formulation": _formulation(calc.environment),
        "separation_m": scenario["separation"],
        "stagger_m": scenario["stagger"],
        "velocity_m_s": scenario["velocity"],
        "forces": {
            "surge_N": forces["surge"],
            "sway_N": forces["sway"],
            "yaw_Nm": forces["yaw"],
        },
        "results_csv": _display_path(csv_path),
    }
    return cfg


def _scenario(calculation: CalculationConfig) -> dict[str, float]:
    return {
        "separation": _required_positive(
            calculation.lateral_separation,
            "lateral_separation",
        ),
        "stagger": _required_float(calculation.stagger_distance, "stagger_distance"),
        "velocity": _required_positive(
            calculation.passing_velocity,
            "passing_velocity",
        ),
    }


def _required_positive(value: float | None, name: str) -> float:
    value = _required_float(value, name)
    if value <= 0.0:
        raise ValueError(f"passing_ship calculation.{name} must be positive")
    return value


def _required_float(value: float | None, name: str) -> float:
    if value is None:
        raise ValueError(f"passing_ship calculation.{name} is required")
    return float(value)


def _formulation(environment: EnvironmentalConfig) -> str:
    if environment.is_infinite_depth:
        return "wang_infinite_depth"
    return "wang_finite_depth"


def _result_row(
    scenario: dict[str, float],
    forces: dict[str, float],
) -> dict[str, float]:
    return {
        "separation_m": scenario["separation"],
        "stagger_m": scenario["stagger"],
        "velocity_m_s": scenario["velocity"],
        "surge_N": forces["surge"],
        "sway_N": forces["sway"],
        "yaw_Nm": forces["yaw"],
    }


def _results_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir / f"{_input_stem(cfg)}_passing_ship.csv"


def _write_results_csv(path: Path, rows: list[dict[str, float]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = [
        "separation_m",
        "stagger_m",
        "velocity_m_s",
        "surge_N",
        "sway_N",
        "yaw_Nm",
    ]
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "passing_ship"))


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
