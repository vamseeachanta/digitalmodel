"""Durable workflow for metallic mooring-line fatigue screening."""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd

from digitalmodel.fatigue.damage import miner_damage
from digitalmodel.fatigue.rainflow import rainflow_count, stress_histogram
from digitalmodel.fatigue.sn_curves import get_sn_curve


REPO_ROOT = Path(__file__).resolve().parents[3]
SUPPORTED_MATERIALS = {"CHAIN", "STEEL-WIRE", "STEEL_WIRE"}
SYNTHETIC_MATERIALS = {"SYNTHETIC", "SYNTHETIC-ROPE", "POLYESTER", "NYLON", "HMPE"}


def router(cfg: dict) -> dict:
    settings = cfg.get("mooring_fatigue") or {}
    design_life_years = _positive_float(settings, "design_life_years")
    dff = _positive_float(settings, "dff")
    curve_name, environment = _curve_settings(settings)
    sn_curve = get_sn_curve(curve_name, environment)

    rows: list[dict[str, Any]] = []
    summaries = []
    for line in _lines(settings):
        line_rows, summary = _evaluate_line(line, sn_curve, design_life_years, dff)
        rows.extend(line_rows)
        summaries.append(summary)

    governing = max(summaries, key=lambda item: item["total_damage"])
    csv_path = _results_csv_path(cfg, settings)
    summary_path = _summary_csv_path(cfg, settings)
    _write_csv(csv_path, rows)
    _write_csv(summary_path, summaries)

    cfg["mooring_fatigue"] = {
        "design_life_years": design_life_years,
        "dff": dff,
        "sn_curve": {"curve": curve_name, "environment": environment},
        "governing_line": governing["line_id"],
        "governing_damage": governing["total_damage"],
        "governing_fatigue_life_years": governing["fatigue_life_years"],
        "governing_dff_margin": governing["dff_margin"],
        "lines": summaries,
        "results_csv": _display_path(csv_path),
        "summary_csv": _display_path(summary_path),
    }
    return cfg


def _evaluate_line(
    line: dict[str, Any],
    sn_curve: Any,
    design_life_years: float,
    dff: float,
) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    line_id = str(line.get("id", line.get("name", "line")))
    material = _material(line)
    bins = _stress_bins(line)
    damage_df = miner_damage(pd.DataFrame(bins), sn_curve)
    total_damage = float(damage_df.attrs["total_damage"])
    fatigue_life = _fatigue_life_years(design_life_years, total_damage)
    required_life = design_life_years * dff
    dff_margin = _safe_divide(fatigue_life, required_life)
    rows = _damage_rows(line_id, material, bins, damage_df)
    summary = {
        "line_id": line_id,
        "material": material,
        "total_damage": total_damage,
        "fatigue_life_years": fatigue_life,
        "required_life_years": required_life,
        "dff_margin": dff_margin,
        "passes_dff": dff_margin >= 1.0,
    }
    return rows, summary


def _stress_bins(line: dict[str, Any]) -> list[dict[str, float]]:
    if line.get("stress_range_bins") is not None:
        bins = [_stress_bin_from_stress(item) for item in line["stress_range_bins"]]
        return _require_bins(bins)
    if line.get("tension_range_bins") is not None:
        area_mm2 = _line_area_mm2(line)
        bins = [
            _stress_bin_from_tension(item, area_mm2)
            for item in line["tension_range_bins"]
        ]
        return _require_bins(bins)
    if line.get("stress_history_MPa") is not None:
        return _require_bins(_history_bins(line["stress_history_MPa"], line))
    if line.get("tension_history_kN") is not None:
        area_mm2 = _line_area_mm2(line)
        stress = [
            float(value) * 1000.0 / area_mm2
            for value in line["tension_history_kN"]
        ]
        return _require_bins(_history_bins(stress, line))
    raise ValueError("mooring_fatigue line requires stress/tension bins or history")


def _stress_bin_from_tension(item: dict[str, Any], area_mm2: float) -> dict[str, float]:
    tension_range_kN = _first_float(
        item,
        ("tension_range_kN", "range_kN", "range", "tension_range"),
        "tension_range_kN",
    )
    return {
        "stress_range": tension_range_kN * 1000.0 / area_mm2,
        "cycles": _cycles(item),
        "tension_range_kN": tension_range_kN,
    }


def _stress_bin_from_stress(item: dict[str, Any]) -> dict[str, float]:
    stress_range = _first_float(
        item,
        ("stress_range_MPa", "stress_range_mpa", "stress_range", "range"),
        "stress_range_MPa",
    )
    return {
        "stress_range": stress_range,
        "cycles": _cycles(item),
        "tension_range_kN": math.nan,
    }


def _history_bins(values: list[Any], line: dict[str, Any]) -> list[dict[str, float]]:
    cycles = rainflow_count(np.asarray(values, dtype=float))
    histogram = stress_histogram(cycles, n_bins=int(line.get("n_bins", 20)))
    repeat_count = float(line.get("n_repeats", line.get("n_histories", 1.0)))
    histogram["cycles"] = histogram["cycles"] * repeat_count
    return [
        {
            "stress_range": float(row["stress_range"]),
            "cycles": float(row["cycles"]),
            "tension_range_kN": math.nan,
        }
        for _, row in histogram.iterrows()
    ]


def _damage_rows(
    line_id: str,
    material: str,
    bins: list[dict[str, float]],
    damage_df: pd.DataFrame,
) -> list[dict[str, Any]]:
    rows = []
    for index, row in damage_df.iterrows():
        rows.append(
            {
                "line_id": line_id,
                "material": material,
                "bin_index": int(index),
                "tension_range_kN": bins[index]["tension_range_kN"],
                "stress_range_MPa": float(row["stress_range"]),
                "cycles": float(row["cycles"]),
                "allowable_cycles": float(row["allowable_cycles"]),
                "damage": float(row["damage"]),
            }
        )
    return rows


def _material(line: dict[str, Any]) -> str:
    material = str(line.get("material", "CHAIN")).strip().upper().replace(" ", "-")
    if material in SYNTHETIC_MATERIALS:
        raise ValueError("mooring_fatigue synthetic ropes are out of scope")
    if material not in SUPPORTED_MATERIALS:
        raise ValueError(f"mooring_fatigue unsupported material: {material}")
    return "STEEL-WIRE" if material == "STEEL_WIRE" else material


def _curve_settings(settings: dict[str, Any]) -> tuple[str, str]:
    curve = settings.get("sn_curve") or {}
    return str(curve.get("curve", "D")), str(curve.get("environment", "seawater_cp"))


def _lines(settings: dict[str, Any]) -> list[dict[str, Any]]:
    lines = settings.get("lines")
    if not isinstance(lines, list) or not lines:
        raise ValueError("mooring_fatigue lines must be a non-empty list")
    return lines


def _line_area_mm2(line: dict[str, Any]) -> float:
    return _first_float(line, ("area_mm2", "cross_section_area_mm2"), "area_mm2")


def _cycles(item: dict[str, Any]) -> float:
    return _first_float(item, ("n_cycles", "cycles", "count"), "n_cycles")


def _require_bins(bins: list[dict[str, float]]) -> list[dict[str, float]]:
    if not bins:
        raise ValueError("mooring_fatigue line must contain at least one cycle bin")
    return bins


def _positive_float(settings: dict[str, Any], name: str) -> float:
    value = settings.get(name)
    if value is None:
        raise ValueError(f"mooring_fatigue {name} is required")
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"mooring_fatigue {name} must be positive")
    return value


def _first_float(source: dict[str, Any], aliases: tuple[str, ...], label: str) -> float:
    for alias in aliases:
        if source.get(alias) is not None:
            value = float(source[alias])
            if value <= 0.0:
                raise ValueError(f"mooring_fatigue {label} must be positive")
            return value
    raise ValueError(f"mooring_fatigue {label} is required")


def _fatigue_life_years(design_life_years: float, damage: float) -> float:
    if damage <= 0.0:
        return float("inf")
    return design_life_years / damage


def _safe_divide(numerator: float, denominator: float) -> float:
    if math.isinf(numerator):
        return float("inf")
    return numerator / denominator


def _results_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    return _output_dir(cfg, settings) / f"{_input_stem(cfg)}_mooring_fatigue.csv"


def _summary_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    return _output_dir(cfg, settings) / f"{_input_stem(cfg)}_mooring_fatigue_summary.csv"


def _output_dir(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "mooring_fatigue"))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError("mooring_fatigue cannot write empty results")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
