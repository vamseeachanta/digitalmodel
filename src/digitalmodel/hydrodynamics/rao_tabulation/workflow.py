"""UV-workflow router for offline 6-DOF RAO tabulation.

Period inputs use the hydrodynamics convention omega_rad_s = 2*pi/period_s.
Frequency inputs are angular frequencies in rad/s.
"""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any

import numpy as np

from digitalmodel.hydrodynamics.interpolator import CoefficientsInterpolator
from digitalmodel.hydrodynamics.models import MatrixDOF, RAOData


REPO_ROOT = Path(__file__).resolve().parents[4]
DOFS = tuple(dof.name.lower() for dof in MatrixDOF)
PHASE_COLUMNS = tuple(f"{name}_phase_deg" for name in DOFS)


def router(cfg: dict) -> dict:
    settings = cfg.get("rao_tabulation") or {}
    rao_data = _rao_data(settings)
    queries = _query_points(settings)
    method = _interpolation_method(settings)
    rows = _interpolated_rows(rao_data, queries, method)
    csv_path = _results_csv_path(cfg, settings)
    _write_results_csv(csv_path, rows)

    peak = max(rows, key=lambda row: float(row["heave"]))
    cfg["rao_tabulation"] = {
        "vessel_name": rao_data.vessel_name,
        "frequency_convention": "omega_rad_s = 2*pi/period_s",
        "interpolation_method": method,
        "n_query_points": len(rows),
        "dofs": list(DOFS),
        "rao_csv": _display_path(csv_path),
        "peak_heave_amplitude": float(peak["heave"]),
        "peak_heave_period_s": float(peak["period_s"]),
        "peak_heave_heading_deg": float(peak["heading_deg"]),
    }
    return cfg


def _rao_data(settings: dict[str, Any]) -> RAOData:
    database = _database(settings)
    frequencies = _frequency_axis(database)
    directions = _directions_axis(database)
    amplitudes = _rao_array(database, "amplitudes", frequencies, directions)
    phases = _rao_array(database, "phases_deg", frequencies, directions)

    freq_order = _sorted_indices(frequencies, "rao_tabulation frequencies")
    dir_order = _sorted_indices(directions, "rao_tabulation headings")
    return RAOData(
        frequencies=frequencies[freq_order],
        directions=directions[dir_order],
        amplitudes=amplitudes[freq_order][:, dir_order, :],
        phases=phases[freq_order][:, dir_order, :],
        vessel_name=str(
            database.get("vessel_name", settings.get("vessel_name", "Vessel"))
        ),
    )


def _database(settings: dict[str, Any]) -> dict[str, Any]:
    database = settings.get("rao_database", settings.get("database", settings))
    if not isinstance(database, dict):
        raise ValueError("rao_tabulation rao_database must be a mapping")
    return database


def _frequency_axis(database: dict[str, Any]) -> np.ndarray:
    periods = _first_value(database, ("periods_s", "periods", "period_s"))
    freqs = _first_value(
        database, ("frequencies_rad_s", "frequencies", "frequency_rad_s")
    )
    if (periods is None) == (freqs is None):
        raise ValueError(
            "rao_tabulation requires exactly one of periods_s or frequencies_rad_s"
        )
    if periods is not None:
        period_values = _float_vector(periods, "rao_tabulation periods_s")
        if np.any(period_values <= 0.0):
            raise ValueError("rao_tabulation periods_s must be positive")
        return (2.0 * math.pi) / period_values

    frequency_values = _float_vector(freqs, "rao_tabulation frequencies_rad_s")
    if np.any(frequency_values <= 0.0):
        raise ValueError("rao_tabulation frequencies_rad_s must be positive")
    return frequency_values


def _directions_axis(database: dict[str, Any]) -> np.ndarray:
    directions = _first_value(
        database, ("headings_deg", "directions_deg", "headings", "directions")
    )
    if directions is None:
        raise ValueError("rao_tabulation headings_deg is required")
    return _float_vector(directions, "rao_tabulation headings_deg")


def _rao_array(
    database: dict[str, Any],
    name: str,
    frequencies: np.ndarray,
    directions: np.ndarray,
) -> np.ndarray:
    value = _first_value(database, (name, "phases" if name == "phases_deg" else name))
    if value is None:
        raise ValueError(f"rao_tabulation rao_database.{name} is required")
    values = np.asarray(value, dtype=float)
    expected_shape = (len(frequencies), len(directions), len(DOFS))
    if values.shape != expected_shape:
        raise ValueError(
            f"rao_tabulation rao_database.{name} shape {values.shape} "
            f"does not match expected {expected_shape}"
        )
    if not np.all(np.isfinite(values)):
        raise ValueError(f"rao_tabulation rao_database.{name} must be finite")
    return values


def _query_points(settings: dict[str, Any]) -> list[dict[str, float]]:
    raw_queries = settings.get("query_points", settings.get("queries"))
    if not isinstance(raw_queries, list) or not raw_queries:
        raise ValueError("rao_tabulation query_points must be a non-empty list")
    queries = []
    for index, raw_query in enumerate(raw_queries):
        if not isinstance(raw_query, dict):
            raise ValueError(f"rao_tabulation query_points[{index}] must be a mapping")
        period_s, frequency = _query_period_and_frequency(raw_query, index)
        heading = _query_heading(raw_query, index)
        queries.append(
            {"period_s": period_s, "frequency": frequency, "heading_deg": heading}
        )
    return queries


def _query_period_and_frequency(
    query: dict[str, Any], index: int
) -> tuple[float, float]:
    period = _first_value(query, ("period_s", "period", "periods_s"))
    frequency = _first_value(
        query, ("frequency_rad_s", "frequency", "frequencies_rad_s")
    )
    if period is None and frequency is None:
        raise ValueError(
            f"rao_tabulation query_points[{index}] requires period_s or frequency_rad_s"
        )
    if period is not None:
        period_s = float(period)
        if period_s <= 0.0:
            raise ValueError(
                f"rao_tabulation query_points[{index}].period_s must be positive"
            )
        frequency_from_period = (2.0 * math.pi) / period_s
    if frequency is not None:
        frequency_value = float(frequency)
        if frequency_value <= 0.0:
            raise ValueError(
                f"rao_tabulation query_points[{index}].frequency_rad_s must be positive"
            )
        period_from_frequency = (2.0 * math.pi) / frequency_value
    if period is not None and frequency is not None:
        if not math.isclose(frequency_value, frequency_from_period, rel_tol=1.0e-9):
            raise ValueError(
                f"rao_tabulation query_points[{index}] period_s and frequency_rad_s disagree"
            )
    if period is not None:
        return period_s, frequency_from_period
    return period_from_frequency, frequency_value


def _query_heading(query: dict[str, Any], index: int) -> float:
    heading = _first_value(
        query, ("heading_deg", "direction_deg", "heading", "direction")
    )
    if heading is None:
        raise ValueError(
            f"rao_tabulation query_points[{index}].heading_deg is required"
        )
    heading_value = float(heading)
    if not math.isfinite(heading_value):
        raise ValueError(
            f"rao_tabulation query_points[{index}].heading_deg must be finite"
        )
    return heading_value


def _interpolation_method(settings: dict[str, Any]) -> str:
    method = str(settings.get("interpolation_method", "linear")).strip().lower()
    if method not in {"linear", "nearest"}:
        raise ValueError(
            "rao_tabulation interpolation_method must be linear or nearest"
        )
    return method


def _interpolated_rows(
    rao_data: RAOData,
    queries: list[dict[str, float]],
    method: str,
) -> list[dict[str, float]]:
    interpolator = CoefficientsInterpolator()
    interpolator.load_raos(rao_data)
    rows = []
    for query in queries:
        _check_query_bounds(rao_data, query)
        interpolated = interpolator.interpolate_all_dofs(
            np.array([query["frequency"]], dtype=float),
            np.array([query["heading_deg"]], dtype=float),
            method=method,
        )
        rows.append(_result_row(query, interpolated))
    return rows


def _check_query_bounds(rao_data: RAOData, query: dict[str, float]) -> None:
    frequency = query["frequency"]
    heading = query["heading_deg"]
    if frequency < np.min(rao_data.frequencies) or frequency > np.max(
        rao_data.frequencies
    ):
        raise ValueError(
            "rao_tabulation query frequency is outside the RAO database range"
        )
    if heading < np.min(rao_data.directions) or heading > np.max(rao_data.directions):
        raise ValueError(
            "rao_tabulation query heading is outside the RAO database range"
        )


def _result_row(query: dict[str, float], interpolated: RAOData) -> dict[str, float]:
    amplitudes = interpolated.amplitudes[0, 0, :]
    phases = interpolated.phases[0, 0, :]
    if not np.all(np.isfinite(amplitudes)) or not np.all(np.isfinite(phases)):
        raise ValueError("rao_tabulation interpolation returned non-finite values")
    row = {
        "period_s": float(query["period_s"]),
        "heading_deg": float(query["heading_deg"]),
    }
    row.update({dof: float(amplitudes[index]) for index, dof in enumerate(DOFS)})
    row.update(
        {phase: float(phases[index]) for index, phase in enumerate(PHASE_COLUMNS)}
    )
    return row


def _results_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    stem = _input_stem(cfg)
    return output_dir / f"{stem}_rao.csv"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "rao_tabulation"))


def _write_results_csv(path: Path, rows: list[dict[str, float]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = ["period_s", "heading_deg", *DOFS, *PHASE_COLUMNS]
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def _float_vector(value: Any, name: str) -> np.ndarray:
    values = np.asarray(value, dtype=float)
    if values.ndim != 1 or len(values) == 0:
        raise ValueError(f"{name} must be a non-empty list")
    if not np.all(np.isfinite(values)):
        raise ValueError(f"{name} must be finite")
    return values


def _sorted_indices(values: np.ndarray, name: str) -> np.ndarray:
    order = np.argsort(values)
    sorted_values = values[order]
    if np.any(np.diff(sorted_values) <= 0.0):
        raise ValueError(f"{name} must contain unique values")
    return order


def _first_value(source: dict[str, Any], aliases: tuple[str, ...]) -> Any:
    for alias in aliases:
        if source.get(alias) is not None:
            return source[alias]
    return None


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
