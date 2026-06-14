"""UV-workflow router for S-N curve endurance calculations."""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any

from digitalmodel.fatigue.sn_library_api import calculate_endurance, get_curve


REPO_ROOT = Path(__file__).resolve().parents[3]


def router(cfg: dict) -> dict:
    settings = cfg.get("sn_curve") or {}
    curve_ids = _curve_ids(settings)
    stress_ranges = _stress_ranges(settings)
    expected_environment = settings.get("environment")

    curves = [get_curve(curve_id) for curve_id in curve_ids]
    if expected_environment:
        _validate_environment(curves, str(expected_environment))

    rows = []
    for curve in curves:
        for stress_range in stress_ranges:
            cycles = calculate_endurance(curve, stress_range)
            rows.append((curve.curve_id, stress_range, _csv_cycles_value(cycles)))

    csv_path = _curve_csv_path(cfg, settings)
    _write_curve_csv(csv_path, rows)

    cfg["sn_curve"] = {
        "curves": [_curve_summary(curve) for curve in curves],
        "points": len(rows),
        "curve_csv": _display_path(csv_path),
    }
    return cfg


def _curve_ids(settings: dict[str, Any]) -> list[str]:
    if settings.get("curve_ids") is not None:
        curve_ids = settings["curve_ids"]
        if not isinstance(curve_ids, list):
            raise ValueError("sn_curve curve_ids must be a list")
        if not curve_ids:
            raise ValueError("sn_curve curve_ids must not be empty")
        return [str(curve_id) for curve_id in curve_ids]

    if settings.get("curve_id") is not None:
        return [str(settings["curve_id"])]

    raise ValueError("sn_curve requires curve_id or curve_ids")


def _stress_ranges(settings: dict[str, Any]) -> list[float]:
    grid = settings.get(
        "stress_ranges",
        settings.get("stress_range_grid", settings.get("stress_grid")),
    )
    if isinstance(grid, dict):
        start = float(grid["start"])
        stop = float(grid["stop"])
        num = int(grid["num"])
        if num < 2:
            raise ValueError("sn_curve stress range grid num must be at least 2")
        step = (stop - start) / (num - 1)
        stress_ranges = [start + index * step for index in range(num)]
    elif isinstance(grid, list):
        stress_ranges = [float(value) for value in grid]
    else:
        raise ValueError("sn_curve requires stress_ranges as a list or mapping")

    if not stress_ranges:
        raise ValueError("sn_curve stress_ranges must not be empty")
    if any(value < 0.0 for value in stress_ranges):
        raise ValueError("sn_curve stress_ranges must be non-negative")
    if any(
        stress_ranges[index] <= stress_ranges[index - 1]
        for index in range(1, len(stress_ranges))
    ):
        raise ValueError("sn_curve stress_ranges must be strictly increasing")
    return stress_ranges


def _validate_environment(curves: list[Any], expected_environment: str) -> None:
    mismatches = [
        curve.curve_id
        for curve in curves
        if curve.environment.lower() != expected_environment.lower()
    ]
    if mismatches:
        joined = ", ".join(mismatches)
        raise ValueError(f"sn_curve environment mismatch for curve(s): {joined}")


def _curve_summary(curve: Any) -> dict[str, Any]:
    return {
        "curve_id": curve.curve_id,
        "standard": curve.standard,
        "weld_class": curve.weld_class,
        "environment": curve.environment,
        "m1": curve.m1,
        "log_a1": curve.log_a1,
        "m2": curve.m2,
        "log_a2": curve.log_a2,
        "knee_point": curve.knee_point,
        "endurance_limit": curve.endurance_limit,
    }


def _curve_csv_path(cfg: dict, settings: dict) -> Path:
    cfg_dir = _config_dir(cfg)
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = cfg_dir / output_dir
    stem = _input_stem(cfg)
    return output_dir / f"{stem}_sn_curve.csv"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "sn_curve"))


def _write_curve_csv(path: Path, rows: list[tuple[str, float, Any]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(["curve_id", "stress_range_mpa", "allowable_cycles_n"])
        writer.writerows(rows)


def _csv_cycles_value(value: Any) -> str | float:
    if value is None:
        return "inf"
    cycles = float(value)
    if math.isinf(cycles):
        return "inf"
    return cycles


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
