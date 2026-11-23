"""Headless durable workflow for deepwater rigid-jumper installation screening."""

from __future__ import annotations

import csv
import json
import math
from pathlib import Path
from types import SimpleNamespace
from typing import Any

from digitalmodel.installation.jumper_installation.calculations import (
    governing_phase,
    load_jumper_data,
    load_vessel_data,
    run_parametric_sweep,
    status_from_utils,
)


REPO_ROOT = Path(__file__).resolve().parents[4]


def router(cfg: dict) -> dict:
    settings = cfg.get("jumper_installation") or {}
    config = _workflow_config(settings)
    vessels = load_vessel_data(_config_path(cfg, settings["vessels"]["catalog"]))
    common, jumpers = load_jumper_data(_config_path(cfg, settings["jumpers"]["catalog"]))
    results, _ = run_parametric_sweep(vessels, common, jumpers, config=config)

    output_dir = _output_dir(cfg, settings)
    cases_csv = output_dir / f"{_input_stem(cfg)}_jumper_installation_cases.csv"
    summary_json = output_dir / f"{_input_stem(cfg)}_jumper_installation_summary.json"
    rows = [_case_row(result) for result in results]
    summary = _summary(results, rows, cases_csv, summary_json, config)
    _write_cases_csv(cases_csv, rows)
    _write_summary_json(summary_json, summary, results)

    cfg["jumper_installation"] = {
        "screening_status": summary["screening_status"],
        "governing_phase": summary["governing_phase"],
        "max_utilisation": summary["max_utilisation"],
        "summary": summary,
        "cases": rows,
    }
    cfg.setdefault("outputs", {})["cases_csv"] = _display_path(cases_csv)
    cfg["outputs"]["summary_json"] = _display_path(summary_json)
    _print_self_check(rows, summary)
    return cfg


def _workflow_config(settings: dict[str, Any]) -> SimpleNamespace:
    constants = settings.get("constants") or {}
    physical = settings.get("physical_constants") or {}
    lift = settings.get("lift_span_model") or {}
    tiein = settings.get("tiein_span_model") or {}
    jumpers = settings.get("jumpers") or {}
    sweep = settings.get("sweep") or {}
    vessels = settings.get("vessels") or {}

    return SimpleNamespace(
        vessels=list(vessels.get("selected", [])),
        lengths_m=[float(value) for value in jumpers.get("lengths_m", [])],
        depths=[int(value) for value in sweep.get("depths_m", [])],
        hs=[float(value) for value in sweep.get("hs_m", [])],
        daf_liftoff=_float(constants, "daf_liftoff"),
        daf_splash=_float(constants, "daf_splash"),
        rigging_mass_kg=_float(constants, "rigging_mass_kg"),
        cs_slamming=_float(constants, "cs_slamming"),
        cd_cylinder=_float(constants, "cd_cylinder"),
        ca_cylinder=_float(constants, "ca_cylinder"),
        v_lowering=_float(constants, "v_lowering"),
        v_current=_float(constants, "v_current"),
        splash_submerged_length_m=_float(constants, "splash_submerged_length_m"),
        wire_allowable_factor=_float(constants, "wire_allowable_factor"),
        bending_allowable=_float(constants, "bending_allowable"),
        cable_unit_weight_sub_n_per_m=_float(
            constants, "cable_unit_weight_sub_n_per_m"
        ),
        tie_in_tolerance_mm=_float(constants, "tie_in_tolerance_mm"),
        reference_hs=_float(constants, "reference_hs"),
        tp_coefficient=_float(constants, "tp_coefficient"),
        go_marginal_threshold=_float(constants, "go_marginal_threshold"),
        nogo_utilisation=_float(constants, "nogo_utilisation"),
        seawater_density_kg_m3=_float(physical, "seawater_density_kg_m3"),
        gravity_m_s2=_float(physical, "gravity_m_s2"),
        lift_span_fraction=_float(lift, "lift_span_fraction"),
        lift_span_m=_optional_float(lift, "lift_span_m"),
        lift_moment_coeff=_float(lift, "moment_coeff"),
        tiein_unsupported_span_fraction=_float(
            tiein, "tiein_unsupported_span_fraction"
        ),
        tiein_unsupported_span_m=_optional_float(tiein, "tiein_unsupported_span_m"),
        tiein_include_self_weight=bool(tiein.get("include_self_weight", False)),
        tiein_deflection_coeff=_float(tiein, "deflection_coeff"),
    )


def _summary(
    results: list[dict[str, Any]],
    rows: list[dict[str, Any]],
    cases_csv: Path,
    summary_json: Path,
    config: SimpleNamespace,
) -> dict[str, Any]:
    if not results:
        raise ValueError("jumper_installation produced no cases")
    governing = max(results, key=lambda item: float(item["max_utilisation"]))
    phase_utils = {
        key: float(value) for key, value in governing["phase_utilisations"].items()
    }
    status = status_from_utils(phase_utils, config=config)
    return {
        "screening_status": "fail" if status == "NO_GO" else "pass",
        "overall_status": status,
        "governing_phase": governing_phase(phase_utils),
        "max_utilisation": float(governing["max_utilisation"]),
        "n_cases": len(results),
        "vessels": sorted({str(row["vessel"]) for row in rows}),
        "jumper_lengths_m": sorted({float(row["length_m"]) for row in rows}),
        "water_depths_m": sorted({float(row["water_depth_m"]) for row in rows}),
        "hs_m": sorted({float(row["hs_m"]) for row in rows}),
        "cases_csv": _display_path(cases_csv),
        "summary_json": _display_path(summary_json),
    }


def _case_row(result: dict[str, Any]) -> dict[str, Any]:
    phase_utils = result["phase_utilisations"]
    row = {
        "vessel": result["vessel"],
        "jumper_name": result["jumper_name"],
        "length_m": result["length_m"],
        "water_depth_m": result["water_depth_m"],
        "hs_m": result["hs_m"],
        "tp_s": result["tp_s"],
        "overall_status": result["overall_status"],
        "screening_status": (
            "fail" if result["overall_status"] == "NO_GO" else "pass"
        ),
        "governing_phase": result["governing_phase"],
        "max_utilisation": result["max_utilisation"],
    }
    for phase, utilisation in phase_utils.items():
        row[f"{phase}_utilisation"] = utilisation
    return row


def _write_cases_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = list(rows[0].keys())
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def _write_summary_json(
    path: Path,
    summary: dict[str, Any],
    results: list[dict[str, Any]],
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "screening_status": summary["screening_status"],
        "governing_phase": summary["governing_phase"],
        "max_utilisation": summary["max_utilisation"],
        "summary": summary,
        "cases": results,
    }
    path.write_text(json.dumps(_json_safe(payload), indent=2), encoding="utf-8")


def _print_self_check(rows: list[dict[str, Any]], summary: dict[str, Any]) -> None:
    print("SELF-CHECK jumper-installation")
    print("  swept Hs cases:")
    for row in rows:
        print(
            "    "
            f"Hs={row['hs_m']:.1f} m, "
            f"governing_phase={row['governing_phase']}, "
            f"max_utilisation={row['max_utilisation']:.4f}, "
            f"screening_status={row['screening_status']}"
        )
    print(
        "  summary: "
        f"governing_phase={summary['governing_phase']}, "
        f"max_utilisation={summary['max_utilisation']:.4f}, "
        f"screening_status={summary['screening_status']}"
    )


def _output_dir(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(str(settings.get("output_dir", "results")))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir


def _config_path(cfg: dict, value: str) -> Path:
    path = Path(str(value))
    if not path.is_absolute():
        path = _config_dir(cfg) / path
    return path


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "jumper_installation"))


def _float(settings: dict[str, Any], name: str) -> float:
    value = settings.get(name)
    if value is None:
        raise ValueError(f"jumper_installation {name} is required")
    value = float(value)
    if not math.isfinite(value):
        raise ValueError(f"jumper_installation {name} must be finite")
    return value


def _optional_float(settings: dict[str, Any], name: str) -> float | None:
    value = settings.get(name)
    if value is None:
        return None
    value = float(value)
    if value <= 0.0 or not math.isfinite(value):
        raise ValueError(f"jumper_installation {name} must be positive when set")
    return value


def _json_safe(value: Any) -> Any:
    if isinstance(value, dict):
        return {key: _json_safe(item) for key, item in value.items()}
    if isinstance(value, list):
        return [_json_safe(item) for item in value]
    if isinstance(value, tuple):
        return [_json_safe(item) for item in value]
    return value


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
