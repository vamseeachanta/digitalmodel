"""Durable workflow for synthetic mooring-rope service-life screening."""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any


REPO_ROOT = Path(__file__).resolve().parents[3]
METALLIC_MATERIALS = {"CHAIN", "STEEL-WIRE", "STEEL_WIRE", "WIRE"}
DEFAULT_SYNTHETIC_MATERIALS = {"POLYESTER", "SYNTHETIC"}
ESCALATED_SYNTHETIC_MATERIALS = {"NYLON", "HMPE"}


def router(cfg: dict) -> dict:
    settings = cfg.get("synthetic_rope_mooring_fatigue") or {}
    design_life_years = _positive_float(
        settings,
        "design_life_years",
        "synthetic_rope_mooring_fatigue design_life_years",
    )
    dff = _positive_float(settings, "dff", "synthetic_rope_mooring_fatigue dff")
    config_tn = _tn_curve(settings.get("tn_curve") or {}, "tn_curve")
    config_creep = _creep(settings.get("creep") or {}, "creep")
    min_tension = _min_tension(settings.get("min_tension") or {})

    rows: list[dict[str, Any]] = []
    summaries = []
    for line in _lines(settings):
        line_rows, summary = _evaluate_line(
            line,
            config_tn,
            config_creep,
            min_tension,
            design_life_years,
            dff,
        )
        rows.extend(line_rows)
        summaries.append(summary)

    governing = min(summaries, key=lambda item: item["line_margin"])
    csv_path = _results_csv_path(cfg, settings)
    summary_path = _summary_csv_path(cfg, settings)
    _write_csv(csv_path, rows)
    _write_csv(summary_path, summaries)

    cfg["synthetic_rope_mooring_fatigue"] = _workflow_summary(
        design_life_years,
        dff,
        config_tn,
        governing,
        summaries,
        csv_path,
        summary_path,
    )
    cfg["screening_status"] = cfg["synthetic_rope_mooring_fatigue"][
        "screening_status"
    ]
    return cfg


def _workflow_summary(
    design_life_years: float,
    dff: float,
    config_tn: dict[str, float],
    governing: dict[str, Any],
    summaries: list[dict[str, Any]],
    csv_path: Path,
    summary_path: Path,
) -> dict[str, Any]:
    return {
        "design_life_years": design_life_years,
        "dff": dff,
        "tn_curve": config_tn,
        "governing_line": governing["line_id"],
        "governing_mechanism": governing["governing_mechanism"],
        "governing_margin": governing["line_margin"],
        "screening_status": "pass"
        if all(summary["passes"] for summary in summaries)
        else "fail",
        "lines": summaries,
        "results_csv": _display_path(csv_path),
        "summary_csv": _display_path(summary_path),
    }


def _evaluate_line(
    line: dict[str, Any],
    config_tn: dict[str, float],
    config_creep: dict[str, float],
    min_tension: dict[str, float],
    design_life_years: float,
    dff: float,
) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    line_id, material, mbl, lm, min_tension_kn, temperature = _line_inputs(line)
    tn = _tn_curve(line.get("tn_curve") or config_tn, f"{line_id} tn_curve")
    creep = _creep(line.get("creep") or config_creep, f"{line_id} creep")
    bins = _tension_bins(line, line_id)
    rows, total_damage = _fatigue_rows(
        line_id,
        material,
        temperature,
        bins,
        mbl,
        lm,
        tn,
    )
    summary = _line_summary(
        line_id,
        material,
        temperature,
        total_damage,
        lm,
        min_tension_kn / mbl,
        creep,
        min_tension,
        design_life_years,
        dff,
    )
    return rows, summary


def _line_inputs(
    line: dict[str, Any],
) -> tuple[str, str, float, float, float, float | None]:
    line_id = str(line.get("id", line.get("name", "line")))
    material = _material(line)
    _validate_material_models(line, material)
    mbl = _positive_float(line, "MBL_kN", f"{line_id} MBL_kN")
    mean_tension = _positive_float(
        line,
        "mean_tension_kN",
        f"{line_id} mean_tension_kN",
    )
    min_tension_kn = _nonnegative_float(
        line,
        "min_tension_kN",
        f"{line_id} min_tension_kN",
    )
    temperature = line.get("temperature_C")
    if temperature is not None:
        temperature = float(temperature)
    return line_id, material, mbl, mean_tension / mbl, min_tension_kn, temperature


def _fatigue_rows(
    line_id: str,
    material: str,
    temperature: float | None,
    bins: list[dict[str, float]],
    mbl: float,
    lm: float,
    tn: dict[str, float],
) -> tuple[list[dict[str, Any]], float]:
    a_eff = tn["intercept"] * math.exp(-tn["mean_load_knockdown"] * lm)
    rows = []
    total_damage = 0.0
    for index, item in enumerate(bins):
        normalised_range = item["tension_range_kN"] / mbl
        allowable_cycles = a_eff * normalised_range ** (-tn["slope"])
        damage = item["n_cycles"] / allowable_cycles
        total_damage += damage
        rows.append(
            {
                "line_id": line_id,
                "material": material,
                "temperature_C": temperature,
                "bin_index": index,
                "tension_range_kN": item["tension_range_kN"],
                "normalised_range": normalised_range,
                "n_cycles": item["n_cycles"],
                "allowable_cycles": allowable_cycles,
                "damage": damage,
            }
        )

    if total_damage <= 0.0:
        raise ValueError(f"{line_id} total_damage must be positive")
    return rows, total_damage


def _line_summary(
    line_id: str,
    material: str,
    temperature: float | None,
    total_damage: float,
    lm: float,
    min_ratio: float,
    creep: dict[str, float],
    min_tension: dict[str, float],
    design_life_years: float,
    dff: float,
) -> dict[str, Any]:
    fatigue_life_years = design_life_years / total_damage
    required_years = design_life_years * dff
    fatigue_margin = fatigue_life_years / required_years
    creep_life_years = creep["reference_life_years"] * 10 ** (
        (creep["load_ratio_ref"] - lm) * creep["decades_per_load_ratio"]
    )
    creep_margin = creep_life_years / required_years
    compression_margin = min_ratio / min_tension["min_ratio_allow"]
    margins = {
        "fatigue": fatigue_margin,
        "creep": creep_margin,
        "compression": compression_margin,
    }
    governing_mechanism = min(margins, key=margins.get)
    line_pass = (
        fatigue_margin >= 1.0
        and creep_margin >= 1.0
        and compression_margin >= 1.0
    )
    summary = {
        "line_id": line_id,
        "material": material,
        "temperature_C": temperature,
        "total_damage": total_damage,
        "fatigue_life_years": fatigue_life_years,
        "fatigue_margin": fatigue_margin,
        "creep_life_years": creep_life_years,
        "creep_margin": creep_margin,
        "min_tension_ratio": min_ratio,
        "compression_margin": compression_margin,
        "governing_mechanism": governing_mechanism,
        "line_margin": margins[governing_mechanism],
        "passes": line_pass,
    }
    return summary


def _material(line: dict[str, Any]) -> str:
    if line.get("material") is None:
        raise ValueError("synthetic_rope_mooring_fatigue material is required")
    material = str(line["material"]).strip().upper().replace(" ", "-")
    if material in METALLIC_MATERIALS:
        raise ValueError(
            "synthetic_rope_mooring_fatigue metallic lines use "
            "digitalmodel:mooring-fatigue"
        )
    if material in DEFAULT_SYNTHETIC_MATERIALS | ESCALATED_SYNTHETIC_MATERIALS:
        return material
    raise ValueError(f"synthetic_rope_mooring_fatigue unsupported material: {material}")


def _validate_material_models(line: dict[str, Any], material: str) -> None:
    if material == "NYLON" and not line.get("tn_curve"):
        raise ValueError(_material_model_error(material))
    if material == "HMPE" and (not line.get("tn_curve") or not line.get("creep")):
        raise ValueError(_material_model_error(material))


def _material_model_error(material: str) -> str:
    return (
        f"synthetic_rope_mooring_fatigue {material} requires a material-specific "
        "T-N curve (and creep model for HMPE); escalate -- no validated default"
    )


def _lines(settings: dict[str, Any]) -> list[dict[str, Any]]:
    lines = settings.get("lines")
    if not isinstance(lines, list) or not lines:
        raise ValueError(
            "synthetic_rope_mooring_fatigue lines must be a non-empty list"
        )
    return lines


def _tension_bins(line: dict[str, Any], line_id: str) -> list[dict[str, float]]:
    bins = line.get("tension_range_bins")
    if not isinstance(bins, list) or not bins:
        raise ValueError(f"{line_id} tension_range_bins must be a non-empty list")
    return [
        {
            "tension_range_kN": _positive_float(
                item,
                "tension_range_kN",
                f"{line_id} tension_range_kN",
            ),
            "n_cycles": _positive_float(item, "n_cycles", f"{line_id} n_cycles"),
        }
        for item in bins
    ]


def _tn_curve(source: dict[str, Any], label: str) -> dict[str, float]:
    return {
        "intercept": _positive_float(source, "intercept", f"{label}.intercept"),
        "slope": _positive_float(source, "slope", f"{label}.slope"),
        "mean_load_knockdown": _nonnegative_float(
            source,
            "mean_load_knockdown",
            f"{label}.mean_load_knockdown",
        ),
    }


def _creep(source: dict[str, Any], label: str) -> dict[str, float]:
    return {
        "reference_life_years": _positive_float(
            source,
            "reference_life_years",
            f"{label}.reference_life_years",
        ),
        "load_ratio_ref": _positive_float(
            source,
            "load_ratio_ref",
            f"{label}.load_ratio_ref",
        ),
        "decades_per_load_ratio": _positive_float(
            source,
            "decades_per_load_ratio",
            f"{label}.decades_per_load_ratio",
        ),
    }


def _min_tension(source: dict[str, Any]) -> dict[str, float]:
    return {
        "min_ratio_allow": _positive_float(
            source,
            "min_ratio_allow",
            "min_tension.min_ratio_allow",
        )
    }


def _positive_float(source: dict[str, Any], name: str, label: str) -> float:
    value = source.get(name)
    if value is None:
        raise ValueError(f"synthetic_rope_mooring_fatigue {label} is required")
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"synthetic_rope_mooring_fatigue {label} must be positive")
    return value


def _nonnegative_float(source: dict[str, Any], name: str, label: str) -> float:
    value = source.get(name)
    if value is None:
        raise ValueError(f"synthetic_rope_mooring_fatigue {label} is required")
    value = float(value)
    if value < 0.0:
        raise ValueError(
            f"synthetic_rope_mooring_fatigue {label} must be non-negative"
        )
    return value


def _results_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    return _output_dir(cfg, settings) / (
        f"{_input_stem(cfg)}_synthetic_rope_mooring_fatigue.csv"
    )


def _summary_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    return _output_dir(cfg, settings) / (
        f"{_input_stem(cfg)}_synthetic_rope_mooring_fatigue_summary.csv"
    )


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
    return str(cfg.get("basename", "synthetic_rope_mooring_fatigue"))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError("synthetic_rope_mooring_fatigue cannot write empty results")
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
