"""Offline blunt-metal-loss screen (inches/psi); no licensed tables."""

from __future__ import annotations

import html
from copy import deepcopy
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd

from digitalmodel.asset_integrity.applicability import collect
from digitalmodel.asset_integrity.corroded_pipe import (
    b31g_original,
    modified_b31g,
    rstreng_effective_area,
)
from digitalmodel.asset_integrity.rstreng_2d import rstreng_2d_river_bottom
from digitalmodel.asset_integrity.dnv_rp_f101 import dnv_f101_single_defect
from digitalmodel.asset_integrity.circumferential_defect import (
    api579_part5_circumferential_netsection,
)
from digitalmodel.asset_integrity.assessment.ffs_decision import DecisionBands, decide
from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport

VALIDATION_RECORDS = (
    "docs/domains/asset-integrity/b31g-validation-2026-06-27.md",
    "docs/domains/rstreng-2d-validation-2026-06-29.md",
    "docs/domains/circumferential-defect-validation-2026-06-29.md",
    "docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md",
)
RECORD_BASE = "https://github.com/vamseeachanta/digitalmodel/blob/main/"


def _positive_value(inputs, key):
    value = inputs.get(key)
    if not isinstance(value, (int, float)) or isinstance(value, bool):
        raise ValueError(f"{key} must be a number")
    if not np.isfinite(value) or value <= 0:
        raise ValueError(f"{key} must be finite and positive")


def _measurement_array(inputs, key):
    if key not in inputs:
        raise ValueError(f"Missing measurement field: {key}")
    array = np.asarray(inputs[key])
    if not np.issubdtype(array.dtype, np.number):
        raise ValueError(f"{key} requires numerical measurements")
    return array.astype(float)


def _validate(inputs):
    keys = (
        "nominal_od_in",
        "nominal_wt_in",
        "smys_psi",
        "smts_psi",
        "design_pressure_psi",
        "axial_stress_psi",
        "circumferential_width_in",
        "safety_factor",
        "usage_factor",
        "axial_design_factor",
    )
    for key in keys:
        _positive_value(inputs, key)
    if not isinstance(inputs.get("component_id"), str) or not inputs["component_id"]:
        raise ValueError("component_id must be a nonempty string")
    if (
        inputs["safety_factor"] < 1
        or inputs["usage_factor"] > 1
        or inputs["axial_design_factor"] > 1
    ):
        raise ValueError("safety_factor >= 1 and design factors <= 1 required")
    t, diameter = inputs["nominal_wt_in"], inputs["nominal_od_in"]
    if t >= diameter / 2 or inputs["smts_psi"] < inputs["smys_psi"]:
        raise ValueError("Invalid pipe geometry or tensile strength below yield")
    grid = _measurement_array(inputs, "grid")
    positions = _measurement_array(inputs, "axial_positions_in")
    if (
        grid.ndim != 2
        or grid.shape[0] < 2
        or grid.shape[1] < 1
        or positions.shape != (grid.shape[0],)
    ):
        raise ValueError("A rectangular axial-by-circumferential grid is required")
    if (
        not np.isfinite(grid).all()
        or not np.isfinite(positions).all()
        or np.any(np.diff(positions) <= 0)
        or np.any(grid < 0)
        or np.any(grid > t)
    ):
        raise ValueError("Finite ordered positions and thickness in [0, t] required")
    return positions, grid, t - grid


def _pressure_row(name, result, capacity, allowable, demand):
    return dict(
        method=name,
        capacity_pressure_psi=float(capacity),
        allowable_pressure_psi=float(allowable),
        allowable_axial_stress_psi=None,
        demand_psi=float(demand),
        margin=float(allowable / demand),
        applicability=result.applicability.to_dict(),
    )


def _methods(inputs, positions, depths):
    diameter, t = inputs["nominal_od_in"], inputs["nominal_wt_in"]
    depth, length = float(depths.max()), float(positions[-1] - positions[0])
    yield_stress = inputs["smys_psi"]
    demand = inputs["design_pressure_psi"]
    sf = inputs["safety_factor"]
    results = [
        b31g_original(diameter, t, depth, length, yield_stress, safety_factor=sf),
        modified_b31g(diameter, t, depth, length, yield_stress, safety_factor=sf),
        rstreng_effective_area(
            diameter, t, positions, depths.max(axis=1), yield_stress, safety_factor=sf
        ),
        rstreng_2d_river_bottom(
            diameter, t, positions, depths, yield_stress, safety_factor=sf
        ),
    ]
    names = ("B31G", "Modified_B31G", "RSTRENG", "RSTRENG-2D")
    rows = [
        _pressure_row(
            name, res, res.failure_pressure_psi, res.safe_pressure_psi, demand
        )
        for name, res in zip(names, results)
    ]
    dnv = dnv_f101_single_defect(
        diameter,
        t,
        depth,
        length,
        inputs["smts_psi"],
        usage_factor=inputs["usage_factor"],
    )
    rows.append(
        _pressure_row(
            "DNV-RP-F101",
            dnv,
            dnv.capacity_pressure_psi,
            dnv.allowable_pressure_psi,
            demand,
        )
    )
    row, circ = _circumferential_row(inputs, depth, length)
    rows.append(row)
    return rows, collect([*results, dnv, circ])


def _circumferential_row(inputs, depth, length):
    diameter, t = inputs["nominal_od_in"], inputs["nominal_wt_in"]
    circ = api579_part5_circumferential_netsection(
        diameter - t,
        t,
        depth,
        inputs["circumferential_width_in"],
        inputs["smys_psi"],
        s=length,
    )
    allowable = circ.allowable_axial_stress_psi * inputs["axial_design_factor"]
    row = dict(
        method="Circumferential net-section",
        capacity_pressure_psi=None,
        allowable_pressure_psi=None,
        allowable_axial_stress_psi=allowable,
        demand_psi=float(inputs["axial_stress_psi"]),
        margin=allowable / inputs["axial_stress_psi"],
        applicability=circ.applicability.to_dict(),
        level1_extent_screen_ok=circ.level1_screen_ok,
    )
    return row, circ


def _decision(rows, applicability):
    governing = min(rows, key=lambda row: row["margin"])
    # This screen uses capacity/demand, not API 579 RSF severity bands:
    # ratio >= 1 -> ACCEPT, 0 <= ratio < 1 -> DERATE, any flag -> ESCALATE.
    decision = decide(
        governing["margin"],
        1.0,
        float("inf"),
        "pipeline",
        bands=DecisionBands(monitor_band=0, derate_floor=0, repair_life_yr=0),
        applicability=applicability,
    ).to_dict()
    decision.pop("rsf")
    decision.pop("rsf_a")
    decision["remaining_life_yr"] = None
    criterion = "Current demand passes: minimum allowable/demand >= 1.000."
    if decision["verdict"] == "ESCALATE":
        criterion = (
            "A fitness verdict is not established; engineering review required. "
            + "; ".join(applicability.notes)
        )
    pressure = min(row["allowable_pressure_psi"] for row in rows[:-1])
    axial = rows[-1]["allowable_axial_stress_psi"]
    decision["demand_limits_psi"] = (
        dict(pressure=pressure, axial_membrane_stress=axial)
        if applicability.ok
        else None
    )
    if decision["verdict"] == "DERATE":
        pressure_failed = any(row["margin"] < 1 for row in rows[:-1])
        axial_failed = rows[-1]["margin"] < 1
        decision["action"] = (
            "REDUCE PRESSURE AND AXIAL STRESS"
            if pressure_failed and axial_failed
            else "RE_RATE" if pressure_failed else "REDUCE AXIAL STRESS"
        )
        criterion = (
            f"Demand exceeds allowable: pressure limit={pressure:.3f} psi; "
            f"axial membrane stress limit={axial:.3f} psi. "
            "Reduce each exceeded demand to its limit; engineering review required."
        )
    decision["governing_criterion"] = (
        f"{governing['method']}: allowable/demand={governing['margin']:.3f}; "
        + criterion
    )
    return governing["method"], decision


def assess(inputs):
    """Assess one UT thickness grid; flag any extrapolation before disposition."""
    positions, grid, depths = _validate(inputs)
    rows, applicability = _methods(inputs, positions, depths)
    governing, decision = _decision(rows, applicability)
    limitations = [
        "The caller-defined grid window is assumed to bound one defect, including intact gaps."
    ]
    if np.any(depths[[0, -1]] > 0):
        limitations.append(
            "Loss reaches an axial grid boundary; confirm the inspection window captures the full defect."
        )
    source = deepcopy(inputs)
    source.update(grid=grid.tolist(), axial_positions_in=positions.tolist())
    return dict(
        methods=rows,
        governing_method=governing,
        decision=decision,
        inputs=source,
        limitations=limitations,
        applicability=applicability.to_dict(),
        validation_records=list(VALIDATION_RECORDS),
    )


def _comparison(rows):
    header = (
        "Method",
        "capacity pressure (psi)",
        "allowable pressure (psi)",
        "allowable axial stress (psi)",
        "demand (psi)",
        "allowable/demand",
        "Applicability",
    )
    table = "<h2>Method comparison</h2><table><tr>"
    table += "".join(f"<th>{name}</th>" for name in header) + "</tr>"
    for row in rows:
        app = row["applicability"]
        status = "NO APPLICABILITY FLAG" if app["ok"] else "OUTSIDE APPLICABILITY"
        status += ": " + "; ".join(app["notes"]) if app["notes"] else ""
        if row.get("level1_extent_screen_ok") is False:
            status += "; Level-1 extent screen failed; membrane check performed"
        values = [
            row["method"],
            *[
                row[key]
                for key in (
                    "capacity_pressure_psi",
                    "allowable_pressure_psi",
                    "allowable_axial_stress_psi",
                    "demand_psi",
                    "margin",
                )
            ],
            status,
        ]
        table += (
            "<tr>"
            + "".join(
                "<td>"
                + html.escape(
                    "not applicable"
                    if v is None
                    else f"{v:.3f}" if isinstance(v, float) else str(v)
                )
                + "</td>"
                for v in values
            )
            + "</tr>"
        )
    return table + "</table>"


def _assessment_basis(inputs):
    return (
        "<h2>Assessment basis</h2><table>"
        + "".join(
            f"<tr><td>{label}</td><td>{inputs[key]:.3f}</td></tr>"
            for label, key in (
                ("OD (in)", "nominal_od_in"),
                ("Wall thickness (in)", "nominal_wt_in"),
                ("SMYS (psi)", "smys_psi"),
                ("SMTS (psi)", "smts_psi"),
                ("Circumferential width (in)", "circumferential_width_in"),
                ("Safety factor (1)", "safety_factor"),
                (
                    "Usage factor applied to DNV capacity (caller supplied, 1)",
                    "usage_factor",
                ),
                ("Axial design factor (1)", "axial_design_factor"),
            )
        )
        + "</table>"
    )


def render_report(inputs, result):
    """Reuse FFSReport presentation without implying a Part 5 L1/L2 assessment."""
    from digitalmodel.asset_integrity.applicability import Applicability

    decision = result["decision"]
    component = html.escape(str(inputs["component_id"]))
    grid = pd.DataFrame(inputs["grid"])
    sections = [
        FFSReport._html_head(
            str(inputs["component_id"]), datetime.now(timezone.utc).date().isoformat()
        ),
        "<h1>Pipeline corroded-defect screen</h1>",
        f"<p>Component: {component}</p>",
        "<p>Input provenance: "
        + html.escape(
            str(
                inputs.get(
                    "data_origin", "caller supplied; measurement provenance unverified"
                )
            )
        )
        + "</p>",
        f"<p>Verdict: {decision['verdict']}; "
        + html.escape(decision["governing_criterion"])
        + "</p>",
        _assessment_basis(inputs),
        _comparison(result["methods"]),
        "<p>RSTRENG-2D uses the maximum-depth projection and therefore reproduces RSTRENG; not full 2D "
        "interaction. Circumferential capacity is an axial membrane "
        "screen using SMYS as flow stress and a caller-supplied axial design factor; combined loading and edition-matched "
        "Part 5 qualification are not established. Remaining life is not evaluated. "
        "The safety factor and DNV usage factor are caller supplied.</p>",
        "<p>" + " ".join(html.escape(note) for note in result["limitations"]) + "</p>",
        FFSReport._section_appendix(grid),
        "<footer><h2>Validation records</h2><ul>",
    ]
    sections.extend(
        f'<li><a href="{RECORD_BASE}{path}">{path}</a></li>'
        for path in VALIDATION_RECORDS
    )
    sections.extend(
        [
            "</ul></footer>",
            _report_footer(Applicability(**result["applicability"])),
        ]
    )
    return "\n".join(sections)


def _report_footer(applicability):
    footer = FFSReport._html_foot(applicability)
    flags, marker, _ = footer.partition("<div class='disclaimer'>")
    if not marker:
        raise ValueError("FFSReport footer structure changed; report review required")
    return flags + (
        "<div class='disclaimer'>Blunt-metal-loss screening only. "
        "No combined-loading or edition-matched Part 5 qualification is established. "
        "Engineering review is required before an operating decision.</div></body></html>"
    )


def router(cfg):
    """Engine adapter with an indexed result and offline HTML artifact."""
    inputs = cfg["pipeline_defect_screen"]
    result = assess(inputs)
    folder = Path(cfg["Analysis"]["result_folder"])
    folder.mkdir(parents=True, exist_ok=True)
    stem = str(cfg["Analysis"]["file_name"])
    if not stem or Path(stem).name != stem or "\\" in stem:
        raise ValueError("Report input stem must be a single filename")
    report = folder / f"{stem}-pipeline-defect-screen.html"
    report.write_text(render_report(inputs, result), encoding="utf-8")
    result["report_file"] = report.name
    cfg[cfg["basename"]] = result
    return cfg
