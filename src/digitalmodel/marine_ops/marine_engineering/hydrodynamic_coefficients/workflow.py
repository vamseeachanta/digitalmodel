"""Workflow adapter for offline hydrodynamic coefficient interpolation."""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Any

import numpy as np

from .coefficients import CoefficientDatabase


REPO_ROOT = Path(__file__).resolve().parents[5]


def router(cfg: dict) -> dict:
    settings = cfg.get("hydro_coefficients") or {}
    database = CoefficientDatabase.from_csv(_data_dir(cfg, settings))
    _validate_database(database)

    rows = [_query_row(database, query) for query in _query_points(settings)]
    csv_path = _query_csv_path(cfg, settings)
    _write_query_csv(csv_path, rows)

    min_frequency, max_frequency = database.get_frequency_range()
    cfg["hydro_coefficients"] = {
        "n_frequencies": int(len(database.frequencies)),
        "frequency_min_rad_s": min_frequency,
        "frequency_max_rad_s": max_frequency,
        "query_csv": _display_path(csv_path),
        "queries": rows,
        "causality": _causality(database, settings),
    }
    return cfg


def _data_dir(cfg: dict, settings: dict[str, Any]) -> Path:
    data_dir = Path(settings.get("data_dir", "data"))
    if not data_dir.is_absolute():
        data_dir = _config_dir(cfg) / data_dir
    return data_dir


def _validate_database(database: CoefficientDatabase) -> None:
    if len(database.frequencies) == 0:
        raise ValueError("hydro_coefficients data_dir contains no coefficient CSVs")
    if len(database.added_mass_matrices) != len(database.damping_matrices):
        raise ValueError("hydro_coefficients added mass and damping counts differ")
    if not np.all(np.isfinite(database.frequencies)):
        raise ValueError("hydro_coefficients frequencies must be finite")


def _query_points(settings: dict[str, Any]) -> list[dict[str, Any]]:
    queries = settings.get("query_points")
    if not isinstance(queries, list) or not queries:
        raise ValueError("hydro_coefficients query_points must be a non-empty list")
    return queries


def _query_row(
    database: CoefficientDatabase,
    query: dict[str, Any],
) -> dict[str, float | str]:
    frequency = _positive_float(query, "frequency_rad_s")
    dof_i = str(query.get("dof_i", "Heave"))
    dof_j = str(query.get("dof_j", dof_i))
    return {
        "frequency_rad_s": frequency,
        "dof_i": dof_i,
        "dof_j": dof_j,
        "added_mass": database.get_added_mass(frequency, dof_i, dof_j),
        "damping": database.get_damping(frequency, dof_i, dof_j),
    }


def _causality(
    database: CoefficientDatabase,
    settings: dict[str, Any],
) -> dict[str, float | str | bool]:
    causality = settings.get("causality_check") or {}
    dof_i = str(causality.get("dof_i", "Surge"))
    dof_j = str(causality.get("dof_j", dof_i))
    tolerance = float(causality.get("tolerance", 0.1))
    is_valid, max_error = database.validate_causality(dof_i, dof_j, tolerance)
    return {
        "dof_i": dof_i,
        "dof_j": dof_j,
        "tolerance": tolerance,
        "is_valid": bool(is_valid),
        "max_error": float(max_error),
    }


def _query_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir / f"{_input_stem(cfg)}_hydro_coefficients.csv"


def _write_query_csv(path: Path, rows: list[dict[str, float | str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = ["frequency_rad_s", "dof_i", "dof_j", "added_mass", "damping"]
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def _positive_float(settings: dict[str, Any], name: str) -> float:
    value = float(settings[name])
    if value <= 0.0 or not np.isfinite(value):
        raise ValueError(f"hydro_coefficients {name} must be positive")
    return value


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "hydro_coefficients"))


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
