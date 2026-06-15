"""Headless field-wide artificial-lift dynacard health workflow."""

from __future__ import annotations

import csv
import json
from collections import Counter
from copy import deepcopy
from pathlib import Path
from typing import Any

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    ALL_GENERATORS,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    CardData,
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
    SurfaceUnit,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow


REPO_ROOT = Path(__file__).resolve().parents[4]
FAILURE_CLASSIFICATIONS = {"GAS_LOCK", "ROD_PARTING", "STUCK_PUMP"}
CRITICAL_CLASSIFICATIONS = {"PUMP_TAGGING"}
STATUS_RANK = {"normal": 0, "warning": 1, "critical": 2, "failure": 3}


def router(cfg: dict) -> dict:
    """Run the durable field-health workflow and update the engine config."""
    settings = cfg.get("artificial_lift_field_health") or {}
    rows = run_field_troubleshooter(settings)
    output_dir = _output_dir(cfg, settings)
    file_base = _input_stem(cfg)
    wells_csv = output_dir / f"{file_base}_field_health_wells.csv"
    summary_json = output_dir / f"{file_base}_field_health_summary.json"
    summary = _field_summary(rows, wells_csv, summary_json, settings)

    _write_wells_csv(wells_csv, rows)
    _write_summary_json(summary_json, summary, rows)

    cfg["screening_status"] = summary["screening_status"]
    cfg["artificial_lift_field_health"] = {
        "screening_status": summary["screening_status"],
        "n_wells": summary["n_wells"],
        "field_status_counts": summary["field_status_counts"],
        "worst_wells": summary["worst_wells"],
        "wells": rows,
        "summary": summary,
    }
    cfg.setdefault("outputs", {})["well_status_csv"] = _display_path(wells_csv)
    cfg["outputs"]["summary_json"] = _display_path(summary_json)
    _print_self_check(rows, summary)
    return cfg


def run_field_troubleshooter(settings: dict[str, Any]) -> list[dict[str, Any]]:
    """Run dynacard diagnosis across the configured field wells."""
    wells = settings.get("wells") or []
    if not wells:
        raise ValueError("artificial_lift_field_health requires at least one well")

    defaults = settings.get("well_defaults") or {}
    solver_method = str(settings.get("solver_method", "gibbs"))
    rows = []
    for well in wells:
        ctx = load_and_map_well(well, defaults)
        result = DynacardWorkflow(ctx, solver_method=solver_method).run_full_analysis()
        rows.append(_well_row(result, solver_method))
    return rows


def load_and_map_well(
    well: dict[str, Any],
    defaults: dict[str, Any] | None = None,
) -> DynacardAnalysisContext:
    """Map an inline field-health well definition to dynacard context."""
    well_cfg = _merge_well(defaults or {}, well)
    return DynacardAnalysisContext(
        api14=str(well_cfg["api14"]),
        surface_card=_surface_card(well_cfg),
        rod_string=_rod_sections(well_cfg),
        pump=PumpProperties(**well_cfg["pump"]),
        surface_unit=SurfaceUnit(**well_cfg.get("surface_unit", {})),
        spm=float(well_cfg.get("spm", 10.0)),
    )


def _surface_card(well_cfg: dict[str, Any]) -> CardData:
    if "surface_card" in well_cfg:
        return CardData(**well_cfg["surface_card"])

    synthetic = well_cfg.get("synthetic_card")
    if not synthetic:
        raise ValueError(f"{well_cfg['api14']} requires surface_card or synthetic_card")

    mode = str(synthetic["mode"])
    try:
        generator = ALL_GENERATORS[mode]
    except KeyError as exc:
        raise ValueError(f"Unsupported synthetic_card mode: {mode}") from exc
    return generator(seed=int(synthetic.get("seed", 0)))


def _rod_sections(well_cfg: dict[str, Any]) -> list[RodSection]:
    if "rod_string" in well_cfg:
        rod_cfgs = well_cfg["rod_string"]
    elif "rods" in well_cfg:
        rod_cfgs = well_cfg["rods"]
    else:
        rod_cfgs = [well_cfg["rod"]]
    return [RodSection(**rod_cfg) for rod_cfg in rod_cfgs]


def _well_row(result: Any, solver_method: str) -> dict[str, Any]:
    classification = _classification(result.diagnostic_message)
    health_status = _health_status(classification)
    return {
        "api14": result.ctx.api14,
        "health_status": health_status,
        "diagnostic_classification": classification,
        "diagnostic_message": result.diagnostic_message,
        "pump_fillage": round(float(result.pump_fillage), 6),
        "inferred_production": round(float(result.inferred_production), 6),
        "buckling_detected": bool(result.buckling_detected),
        "solver_method": solver_method,
    }


def _field_summary(
    rows: list[dict[str, Any]],
    wells_csv: Path,
    summary_json: Path,
    settings: dict[str, Any],
) -> dict[str, Any]:
    status_counts = _status_counts(rows)
    fail_states = set(settings.get("fail_health_states") or ["critical", "failure"])
    worst_limit = int(settings.get("worst_wells_limit", 5))
    worst_wells = sorted(
        rows,
        key=lambda row: (-STATUS_RANK[row["health_status"]], row["pump_fillage"]),
    )[:worst_limit]
    return {
        "screening_status": (
            "fail" if any(row["health_status"] in fail_states for row in rows) else "pass"
        ),
        "n_wells": len(rows),
        "field_status_counts": status_counts,
        "worst_wells": worst_wells,
        "well_status_csv": _display_path(wells_csv),
        "summary_json": _display_path(summary_json),
    }


def _classification(message: str) -> str:
    prefix = "Classification: "
    text = message.split(".", 1)[0]
    return text.removeprefix(prefix)


def _health_status(classification: str) -> str:
    if classification == "NORMAL":
        return "normal"
    if classification in FAILURE_CLASSIFICATIONS:
        return "failure"
    if classification in CRITICAL_CLASSIFICATIONS:
        return "critical"
    return "warning"


def _status_counts(rows: list[dict[str, Any]]) -> dict[str, int]:
    counts = Counter(row["health_status"] for row in rows)
    return {
        status: counts[status]
        for status in STATUS_RANK
        if counts[status] > 0
    }


def _write_wells_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def _write_summary_json(
    path: Path,
    summary: dict[str, Any],
    rows: list[dict[str, Any]],
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "screening_status": summary["screening_status"],
        "summary": summary,
        "wells": rows,
    }
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


def _print_self_check(
    rows: list[dict[str, Any]],
    summary: dict[str, Any],
) -> None:
    print("SELF-CHECK artificial-lift-field-health")
    print(f"  number_of_wells: {summary['n_wells']}")
    print("  per-well health status:")
    for row in rows:
        print(
            "    "
            f"{row['api14']}: "
            f"health_status={row['health_status']}, "
            f"diagnostic={row['diagnostic_classification']}, "
            f"pump_fillage={row['pump_fillage']:.6f}"
        )
    print(f"  field status counts: {summary['field_status_counts']}")
    print(f"  screening_status: {summary['screening_status']}")


def _merge_well(
    defaults: dict[str, Any],
    well: dict[str, Any],
) -> dict[str, Any]:
    merged = deepcopy(defaults)
    for key, value in well.items():
        if isinstance(value, dict) and isinstance(merged.get(key), dict):
            merged[key].update(value)
        else:
            merged[key] = value
    return merged


def _output_dir(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(str(settings.get("output_dir", "results")))
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
    return str(cfg.get("basename", "artificial_lift_field_health"))


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
