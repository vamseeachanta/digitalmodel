# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

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
        basis_curve = _curve_settings({"sn_curve": line.get("sn_curve_basis")})
        if basis_curve != (curve_name, environment):
            raise ValueError(
                "mooring_fatigue sn_curve_basis must match selected curve and environment"
            )
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
        "design_controls_basis": "case-specific inputs",
        "sn_curve": {"curve": curve_name, "environment": environment},
        "governing_line": governing["line_id"],
        "governing_damage": governing["total_damage"],
        "governing_fatigue_life_years": governing["fatigue_life_years"],
        "governing_dff_margin": governing["dff_margin"],
        # Pass only when the worst line meets life >= design life * DFF.
        "screening_status": "pass" if governing["dff_margin"] >= 1.0 else "fail",
        "screening_criterion": "all lines: fatigue life >= design life * DFF",
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
    controls = {"design_life_years": design_life_years, "dff": dff}
    design_life_years = _positive_float(controls, "design_life_years")
    dff = _positive_float(controls, "dff")
    required_life = _positive_float(
        {"required_life_years": design_life_years * dff}, "required_life_years"
    )
    line_id = str(line.get("id", line.get("name", "line")))
    material = _material(line)
    curve_basis = _validate_curve_basis(line, sn_curve)
    bins = _stress_bins(line)
    damage_df = miner_damage(pd.DataFrame(bins), sn_curve)
    total_damage = _positive_float(damage_df.attrs, "total_damage")
    for _, row in damage_df.iterrows():
        _positive_float(row, "allowable_cycles")
        _positive_float(row, "damage")
    fatigue_life = _fatigue_life_years(design_life_years, total_damage)
    dff_margin = _safe_divide(fatigue_life, required_life)
    rows = _damage_rows(line_id, material, bins, damage_df)
    summary = {
        "line_id": line_id,
        "material": material,
        "total_damage": total_damage,
        "fatigue_life_years": fatigue_life,
        "required_life_years": required_life,
        "dff_margin": dff_margin,
        # Pass criterion: fatigue life >= design life * DFF (margin >= 1).
        "passes_dff": dff_margin >= 1.0,
        "dff_criterion": "fatigue life >= design life * DFF",
        "sn_curve_basis": curve_basis,
        "load_basis": "cycle bins over design life",
        "n_repeats": None,
    }
    if "stress_history_MPa" in line or "tension_history_kN" in line:
        summary["load_basis"] = "explicit history repetitions over design life"
        summary["n_repeats"] = _first_float(
            line, ("n_repeats", "n_histories"), "n_repeats"
        )
    return rows, summary


def _stress_bins(line: dict[str, Any]) -> list[dict[str, float]]:
    representations = (
        "stress_range_bins", "tension_range_bins",
        "stress_history_MPa", "tension_history_kN",
    )
    supplied = [key for key in representations if key in line]
    if len(supplied) != 1:
        raise ValueError("mooring_fatigue requires exactly one load representation")
    values = line[supplied[0]]
    if not isinstance(values, (list, tuple)) or not values:
        raise ValueError("mooring_fatigue load representation must be a non-empty list")
    if supplied[0].endswith("_bins") and any(
        key in line for key in ("n_repeats", "n_histories", "n_bins")
    ):
        raise ValueError("mooring_fatigue history controls require a history input")
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
        tension = _finite_history(line["tension_history_kN"])
        with np.errstate(over="ignore", invalid="ignore"):
            stress = tension * 1000.0 / area_mm2
        return _require_bins(_history_bins(stress, line))
    raise ValueError("mooring_fatigue line requires stress/tension bins or history")


def _stress_bin_from_tension(item: dict[str, Any], area_mm2: float) -> dict[str, float]:
    area_mm2 = _positive_float({"area_mm2": area_mm2}, "area_mm2")
    tension_range_kN = _first_float(
        item,
        ("tension_range_kN", "range_kN", "range", "tension_range"),
        "tension_range_kN",
    )
    stress_range = _positive_float(
        {"converted_stress": tension_range_kN * 1000.0 / area_mm2},
        "converted_stress",
    )
    return {
        "stress_range": stress_range,
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
    repeat_count = _first_float(line, ("n_repeats", "n_histories"), "n_repeats")
    signal = _finite_history(values)
    n_bins = line.get("n_bins", 20)
    if isinstance(n_bins, bool) or not isinstance(n_bins, int) or n_bins <= 0:
        raise ValueError("mooring_fatigue n_bins must be a positive integer")
    cycles = rainflow_count(signal)
    if cycles.empty or not np.isfinite(cycles.to_numpy()).all():
        raise ValueError("mooring_fatigue history must produce finite cycle bins")
    if (cycles["stress_range"] <= 0).any():
        raise ValueError("mooring_fatigue history must produce positive stress ranges")
    histogram = stress_histogram(cycles, n_bins=n_bins)
    with np.errstate(over="ignore", invalid="ignore"):
        histogram["cycles"] = histogram["cycles"] * repeat_count
    return _require_bins([
        {
            "stress_range": float(row["stress_range"]),
            "cycles": float(row["cycles"]),
            "tension_range_kN": math.nan,
        }
        for _, row in histogram.iterrows()
    ])


def _finite_history(values: Any) -> np.ndarray:
    try:
        signal = np.asarray(values, dtype=float)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError("mooring_fatigue history must contain finite numbers") from exc
    if signal.ndim != 1 or signal.size < 2 or not np.isfinite(signal).all():
        raise ValueError("mooring_fatigue history must contain finite samples")
    return signal


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
    material = _required_text(line, "material").upper().replace(" ", "-")
    if material in SYNTHETIC_MATERIALS:
        raise ValueError("mooring_fatigue synthetic ropes are out of scope")
    if material not in SUPPORTED_MATERIALS:
        raise ValueError(f"mooring_fatigue unsupported material: {material}")
    return "STEEL-WIRE" if material == "STEEL_WIRE" else material


def _curve_settings(settings: dict[str, Any]) -> tuple[str, str]:
    curve = settings.get("sn_curve")
    if not isinstance(curve, dict):
        raise ValueError("mooring_fatigue sn_curve declaration is required")
    return _required_text(curve, "curve").upper(), _required_text(curve, "environment")


def _required_text(source: dict[str, Any], name: str) -> str:
    value = source.get(name)
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"mooring_fatigue {name} declaration is required")
    return value.strip()


def _validate_curve_basis(line: dict[str, Any], sn_curve: Any) -> dict[str, str]:
    """Require a declared component/detail/protection basis matching the curve.

    The generic detail library cannot infer chain or wire suitability. The
    reference must justify that engineering selection; matching declarations
    does not independently certify the reference's technical adequacy.
    """
    basis = line.get("sn_curve_basis")
    if not isinstance(basis, dict):
        raise ValueError("mooring_fatigue sn_curve_basis is required for each line")
    material = _material(basis)
    name, environment = _curve_settings({"sn_curve": basis})
    declared_curve = get_sn_curve(name, environment)
    if material != _material(line) or not declared_curve.to_pandas().equals(sn_curve.to_pandas()):
        raise ValueError("mooring_fatigue sn_curve_basis must match material and selected curve")
    return {
        "material": material,
        "curve": name,
        "environment": environment,
        **{key: _required_text(basis, key) for key in ("detail", "reference", "protection_basis")},
    }


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
    for item in bins:
        _positive_float(item, "stress_range")
        _positive_float(item, "cycles")
    return bins


def _positive_float(settings: dict[str, Any], name: str) -> float:
    value = settings.get(name)
    if value is None:
        raise ValueError(f"mooring_fatigue {name} is required")
    try:
        if isinstance(value, bool):
            raise ValueError
        value = float(value)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError(f"mooring_fatigue {name} must be finite and positive") from exc
    if not math.isfinite(value) or value <= 0.0:
        raise ValueError(f"mooring_fatigue {name} must be finite and positive")
    return value


def _first_float(source: dict[str, Any], aliases: tuple[str, ...], label: str) -> float:
    supplied = [key for key in aliases if key in source]
    if len(supplied) > 1:
        raise ValueError(f"mooring_fatigue {label} requires exactly one alias")
    if not supplied:
        raise ValueError(f"mooring_fatigue {label} is required")
    return _positive_float({label: source[supplied[0]]}, label)


def _fatigue_life_years(design_life_years: float, damage: float) -> float:
    design_life_years = _positive_float(
        {"design_life_years": design_life_years}, "design_life_years"
    )
    damage = _positive_float({"total_damage": damage}, "total_damage")
    return _positive_float(
        {"fatigue_life_years": design_life_years / damage}, "fatigue_life_years"
    )


def _safe_divide(numerator: float, denominator: float) -> float:
    denominator = _positive_float({"required_life_years": denominator}, "required_life_years")
    numerator = _positive_float({"fatigue_life_years": numerator}, "fatigue_life_years")
    return _positive_float({"dff_margin": numerator / denominator}, "dff_margin")


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
    with path.open("w", encoding="utf-8", newline="\n") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0].keys()), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
