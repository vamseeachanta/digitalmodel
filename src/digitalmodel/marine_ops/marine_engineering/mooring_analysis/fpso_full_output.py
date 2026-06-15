"""Output helpers for the FPSO full mooring durable workflow."""

from __future__ import annotations

import csv
import json
from pathlib import Path
from typing import Any

import numpy as np


REPO_ROOT = Path(__file__).resolve().parents[5]


def result_folder(cfg: dict) -> Path:
    analysis = cfg.get("Analysis", {})
    if analysis.get("result_folder"):
        return Path(analysis["result_folder"])
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"]) / "results"
    return Path("results")


def write_outputs(cfg: dict, result: dict, result_folder: Path) -> None:
    summary_path = _output_path(
        cfg,
        result_folder,
        "summary_json",
        "fpso_mooring_full_summary.json",
    )
    tensions_path = _output_path(
        cfg,
        result_folder,
        "line_tensions_csv",
        "fpso_mooring_full_line_tensions.csv",
    )
    summary_path.write_text(json.dumps(result, indent=2))
    _write_tensions_csv(tensions_path, result["line_tensions"])
    cfg.setdefault("outputs", {})["summary_json"] = _display_path(summary_path)
    cfg["outputs"]["line_tensions_csv"] = _display_path(tensions_path)


def print_self_check(cfg: dict, result: dict) -> None:
    vessel = cfg["vessel"]
    environment = cfg["environment"]
    equilibrium = result["static_equilibrium"]
    summary = result["summary"]
    print("SELF-CHECK fpso-mooring-full")
    print(
        "  vessel: "
        f"LOA={vessel['loa']} m, beam={vessel['beam']} m, "
        f"draft={vessel['draft']} m, displacement={vessel['displacement']} t"
    )
    print(
        "  environment: "
        f"Hs={environment['Hs']} m, Tp={environment['Tp']} s, "
        f"wind={environment['wind_speed']} m/s @ "
        f"{environment['wind_heading']} deg, "
        f"current={environment['current_speed']} m/s @ "
        f"{environment['current_heading']} deg"
    )
    print(
        "  computed offset: "
        f"{equilibrium['offset_m']:.3f} m "
        f"(x={equilibrium['offset_x_m']:.3f}, "
        f"y={equilibrium['offset_y_m']:.3f})"
    )
    print(
        "  max line tension: "
        f"{summary['max_line_tension_MN']:.3f} MN "
        f"({summary['max_tension_line']})"
    )


def json_safe(value: Any) -> Any:
    if isinstance(value, dict):
        return {key: json_safe(item) for key, item in value.items()}
    if isinstance(value, list):
        return [json_safe(item) for item in value]
    if isinstance(value, tuple):
        return [json_safe(item) for item in value]
    if isinstance(value, np.ndarray):
        return [json_safe(item) for item in value.tolist()]
    if isinstance(value, np.generic):
        return value.item()
    return value


def _write_tensions_csv(path: Path, rows: list[dict]) -> None:
    fieldnames = [
        "line_id",
        "heading_deg",
        "horizontal_span_m",
        "top_tension_N",
        "horizontal_tension_N",
        "anchor_tension_N",
        "safety_factor",
    ]
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def _output_path(
    cfg: dict,
    result_folder: Path,
    key: str,
    filename: str,
) -> Path:
    configured = cfg.get("output", {}).get(key)
    path = Path(configured) if configured else result_folder / filename
    if not path.is_absolute():
        path = REPO_ROOT / path
    path.parent.mkdir(parents=True, exist_ok=True)
    return path


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
