# ABOUTME: W7 engine router — basename 'well_access'; reads calibration.yml, writes JSON+MD.
# ABOUTME: Deterministic. Empirical BSEE rate extraction is the worldenergydata-side input.
"""
digitalmodel.well_access.router
===============================

Engine entry point for ``basename: well_access``. Reads the well program, an
access-multiplier table, resource pools, and a **calibration** block (inline or
referenced as a sibling ``calibration.yml``), runs the lifecycle chain, and writes
a JSON summary plus a markdown lifecycle note.

Data-dependency flag (cross-repo, load-bearing)
-----------------------------------------------
The empirical intervention rates / durations / uptime that populate the
calibration come from BSEE WAR history extracted on the **worldenergydata** side
(``bsee/analysis/well_access_calibration/`` -> ``calibration.yml``). This engine
is deterministic given that calibration; it never fabricates rates. The shipped
example calibration is a DOCUMENTED real-anchored profile, not the live extract.
"""

from __future__ import annotations

import json
from dataclasses import asdict, is_dataclass
from enum import Enum
from pathlib import Path
from typing import Any

import yaml

from .lifecycle_summary import compare_architectures, run_lifecycle
from .models import AccessClass, WellProgram


def _jsonable(value: Any) -> Any:
    if isinstance(value, Enum):
        return value.value
    if is_dataclass(value):
        return _jsonable(asdict(value))
    if isinstance(value, dict):
        return {str(k): _jsonable(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [_jsonable(v) for v in value]
    return value


def _resolve(cfg: dict[str, Any], value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return Path(cfg.get("_config_dir_path", Path.cwd())) / path


def _load_calibration(cfg: dict[str, Any], params: dict[str, Any]) -> dict[str, Any]:
    """Calibration is inline under ``calibration:`` or referenced via ``calibration_path``."""
    if params.get("calibration_path"):
        path = _resolve(cfg, params["calibration_path"])
        with open(path, "r", encoding="utf-8") as fh:
            return yaml.safe_load(fh)
    return params.get("calibration", {})


def _render_note(summary_payload: dict[str, Any]) -> str:
    """Deterministic markdown lifecycle note."""
    lines: list[str] = []
    p = summary_payload
    lines.append(f"# Well-access lifecycle summary — {p['well_program']['access_class']}")
    lines.append("")
    lines.append(
        "> **Scope (screening-grade).** Statistical/parametric well-access model. "
        "NOT reservoir/flow/downhole simulation. Uptime is a time-availability "
        "fraction, not a rate. Queue is comparative, not commitment-grade. "
        "Intervention rates are calibration INPUTS (BSEE-anchored, worldenergydata-side)."
    )
    lines.append("")
    wp = p["well_program"]
    lines.append("## Well program (inputs)")
    lines.append("")
    lines.append("| Parameter | Value |")
    lines.append("|---|---|")
    lines.append(f"| Wells | {wp['n_wells']} |")
    lines.append(f"| Field life | {wp['field_life_years']} yr |")
    lines.append(f"| Access class | {wp['access_class']} |")
    lines.append(f"| Facility availability | {wp['facility_availability']:.3f} |")
    lines.append("")

    pr = p["primary"]
    lines.append("## Intervention frequency (demand / realized / deferred)")
    lines.append("")
    lines.append("| Type | Demand | Realized | Deferred | Low-conf |")
    lines.append("|---|---|---|---|---|")
    for e in pr["events"]:
        lines.append(
            f"| {e['intervention_type']} | {e['demand']:.2f} | {e['realized']:.2f} | "
            f"{e['deferred']:.2f} | {'yes' if e['low_confidence'] else 'no'} |"
        )
    lines.append(
        f"| **total** | **{pr['total_demand_events']:.2f}** | "
        f"**{pr['total_realized_events']:.2f}** | "
        f"**{pr['total_deferred_events']:.2f}** | |"
    )
    lines.append("")

    lines.append("## Resource demand")
    lines.append("")
    lines.append("| Pool | Demand-days | Available-days | rho | Wait-days | Mob-days |")
    lines.append("|---|---|---|---|---|---|")
    for r in pr["resources"]:
        lines.append(
            f"| {r['resource_class']} | {r['demand_days']:.0f} | "
            f"{r['available_days']:.0f} | {r['rho']:.2f} | {r['wait_days']:.0f} | "
            f"{r['mobilisation_days']:.0f} |"
        )
    lines.append("")

    u = pr["uptime"]
    lines.append("## Lifecycle uptime")
    lines.append("")
    lines.append(
        f"U = A_facility ({u['facility_availability']:.3f}) x "
        f"(1 - f_intervention {u['f_intervention']:.4f} - "
        f"f_deferred {u['f_deferred']:.4f}) = **{u['field_uptime']:.4f}**"
    )
    lines.append("")

    if p.get("comparison"):
        c = p["comparison"]
        d = c["deltas"]
        lines.append("## Dry-tree vs subsea comparison")
        lines.append("")
        lines.append("| Metric | Delta | Reading |")
        lines.append("|---|---|---|")
        lines.append(
            f"| Uptime (dry-tree − subsea) | {d['uptime_delta']:+.4f} | "
            "positive = dry-tree higher uptime |"
        )
        lines.append(
            f"| Deferred backlog (subsea − dry-tree), events | "
            f"{d['deferred_backlog_delta_events']:+.2f} | positive = subsea defers more |"
        )
        lines.append(
            f"| Mobilisation (subsea − dry-tree), days | "
            f"{d['mobilisation_delta_days']:+.0f} | sign-aware, see note below |"
        )
        lines.append("")
        lines.append(c["interpretation"])
        lines.append("")

    for note in pr.get("notes", []):
        lines.append(f"> _{note}_")
        lines.append("")

    lines.append("---")
    lines.append(
        "_Deterministic well-access model (digitalmodel well_access). Calibration is "
        "BSEE-anchored (worldenergydata-side input); FrPS multiplier is an "
        "engineering assumption, not a result._"
    )
    lines.append("")
    return "\n".join(lines)


def router(cfg: dict[str, Any]) -> dict[str, Any]:
    """Engine entry for basename == 'well_access'."""
    params = cfg["well_access"]
    wp_cfg = params["well_program"]
    well_program = WellProgram(
        n_wells=int(wp_cfg["n_wells"]),
        field_life_years=int(wp_cfg["field_life_years"]),
        access_class=AccessClass(str(wp_cfg["access_class"])),
        first_production_year=int(wp_cfg.get("first_production_year", 0)),
        facility_availability=float(wp_cfg.get("facility_availability", 0.95)),
    )

    calibration = _load_calibration(cfg, params)
    access_table = params.get("access_multiplier", {})
    resources_cfg = params.get("resources", [])
    deferred_loss = float(params.get("deferred_loss_per_event_days", 30.0))

    primary = run_lifecycle(
        well_program, calibration, access_table, resources_cfg, deferred_loss
    )

    payload: dict[str, Any] = {
        "calculation": "well_access",
        "well_program": _jsonable(well_program),
        "deferred_loss_per_event_days": deferred_loss,
        "primary": _jsonable(primary),
    }

    # Optional dry-tree vs subsea comparison block.
    cmp_cfg = params.get("comparison")
    if cmp_cfg:
        comparison = compare_architectures(
            well_program,
            AccessClass(str(cmp_cfg["dry_tree_class"])),
            AccessClass(str(cmp_cfg["subsea_class"])),
            calibration,
            access_table,
            resources_cfg,
            deferred_loss,
        )
        payload["comparison"] = {
            "deltas": comparison["deltas"],
            "interpretation": comparison["interpretation"],
            "dry_tree": _jsonable(comparison["dry_tree"]),
            "subsea": _jsonable(comparison["subsea"]),
        }

    note_md = _render_note(payload)

    output_cfg = params.get("outputs", {})
    directory = _resolve(cfg, output_cfg.get("directory", "results"))
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get("summary_json", "well_access_summary.json")
    note_path = directory / output_cfg.get("note_md", "well_access_lifecycle_note.md")
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    note_path.write_text(note_md, encoding="utf-8")

    payload["summary_json"] = str(summary_path)
    payload["note_md"] = str(note_path)
    cfg["well_access"] = {**params, **payload}
    return cfg
