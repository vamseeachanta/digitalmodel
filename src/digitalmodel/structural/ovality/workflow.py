"""UV-workflow router for pipe out-of-roundness checks.

The DNV-OS-F101 initial ovality parameter is O0 = (Dmax - Dmin) / D.
This workflow reports that geometry value and compares it with a configurable
allowable fraction.
"""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Any


REPO_ROOT = Path(__file__).resolve().parents[4]


def router(cfg: dict) -> dict:
    settings = cfg.get("pipe_ovality") or {}
    nominal_od = _required_positive_float(settings, "nominal_OD")
    allowable = _optional_positive_float(settings, "allowable", 0.03)
    max_od = _optional_float(settings, "max_OD")
    min_od = _optional_float(settings, "min_OD")

    if max_od is not None or min_od is not None:
        if max_od is None or min_od is None:
            raise ValueError("pipe_ovality requires both max_OD and min_OD")
        out_of_roundness = _out_of_roundness(nominal_od, max_od, min_od)
    else:
        out_of_roundness = _required_ovality_fraction(settings)
        max_od = nominal_od * (1.0 + out_of_roundness / 2.0)
        min_od = nominal_od * (1.0 - out_of_roundness / 2.0)

    row = {
        "nominal_OD": nominal_od,
        "max_OD": max_od,
        "min_OD": min_od,
        "out_of_roundness": out_of_roundness,
        "ovality_percent": out_of_roundness * 100.0,
        "allowable": allowable,
        "passes": out_of_roundness <= allowable,
    }
    csv_path = _results_csv_path(cfg, settings)
    _write_results_csv(csv_path, row)

    cfg["pipe_ovality"] = {
        **row,
        "results_csv": _display_path(csv_path),
    }
    return cfg


def _out_of_roundness(nominal_od: float, max_od: float, min_od: float) -> float:
    if max_od <= 0.0:
        raise ValueError("pipe_ovality max_OD must be positive")
    if min_od <= 0.0:
        raise ValueError("pipe_ovality min_OD must be positive")
    if max_od < min_od:
        raise ValueError("pipe_ovality max_OD must be greater than or equal to min_OD")
    return (max_od - min_od) / nominal_od


def _required_ovality_fraction(settings: dict[str, Any]) -> float:
    for key in ("ovality_fraction", "out_of_roundness", "ovality"):
        if settings.get(key) is not None:
            value = float(settings[key])
            if value < 0.0:
                raise ValueError(f"pipe_ovality {key} must be non-negative")
            return value
    raise ValueError("pipe_ovality requires max_OD/min_OD or ovality_fraction")


def _results_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    stem = _input_stem(cfg)
    return output_dir / f"{stem}_ovality.csv"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "pipe_ovality"))


def _write_results_csv(path: Path, row: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(row.keys()))
        writer.writeheader()
        writer.writerow(row)


def _required_positive_float(settings: dict[str, Any], name: str) -> float:
    value = settings.get(name)
    if value is None:
        raise ValueError(f"pipe_ovality {name} is required")
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"pipe_ovality {name} must be positive")
    return value


def _optional_positive_float(
    settings: dict[str, Any],
    name: str,
    default: float,
) -> float:
    value = settings.get(name)
    if value is None:
        return default
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"pipe_ovality {name} must be positive")
    return value


def _optional_float(settings: dict[str, Any], name: str) -> float | None:
    value = settings.get(name)
    if value is None:
        return None
    return float(value)


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
