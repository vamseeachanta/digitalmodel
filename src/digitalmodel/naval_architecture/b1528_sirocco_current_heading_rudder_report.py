# ABOUTME: B1528 SIROCCO current-heading/rudder force-component sweep report.
# ABOUTME: Generates ship-fixed force/moment tables plus interactive dropdown charts.
"""B1528/SIROCCO current-heading/rudder force component report utilities.

This module extends the existing B1528 moored-current rudder-only calculation to
an approved visualization sweep over current speed, heading offset, and rudder
angle.  It reports rudder-induced loads in a ship-fixed COG frame.  It is not a
validated oblique-current hull/rudder/MMG model and deliberately excludes hull
current loads and mooring-line stiffness.
"""

from __future__ import annotations

import csv
import json
import math
from dataclasses import dataclass
from importlib import resources
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.naval_architecture.b1528_sirocco_yaw_report import (
    GITHUB_REPO_BLOB,
    KNOT_TO_M_PER_S,
    NON_ROTATING_PROPELLER_CR,
    PROPELLER_ROTATION_FACTOR_NOTE,
)

REPORT_REVIEW_TARGET_DATE = "2026-05-09"
REPORT_SCOPE = (
    "rudder-induced SIROCCO current-heading/rudder force-component sweep at COG; "
    "ship-fixed X/Y/N components are derived by rotating local current-frame "
    "loads by heading offset; hull current loads, bank effect, tug loads, "
    "mooring-line stiffness, current-profile variation, and propeller race are "
    "excluded; no class compliance conclusion"
)
ZERO_EFFECTIVE_ANGLE_NOTE = (
    "When rudder_angle_deg equals heading_offset_deg, alpha is zero and this "
    "rudder-induced component is zero; that is not total hull current load."
)


@dataclass(frozen=True)
class B1528CurrentHeadingRudderConfig:
    """Validated B1528 current-heading/rudder report configuration."""

    case_id: str
    aliases: tuple[str, ...]
    source_pack_issue: str
    report_issue: str
    plan_path: str
    lbp_m: float
    yaw_lever_m: float
    rudder_area_m2: float
    rudder_span_m: float
    ship_sog_kn: float
    current_speeds_kn: tuple[float, ...]
    chart_default_current_speed_kn: float
    default_speed_policy: str
    heading_offsets_deg: tuple[float, ...]
    rudder_angles_deg: tuple[float, ...]
    rho_kg_m3: float
    beta: float
    prop_rotation_factor: float
    prop_rotation_factor_logic: str
    force_convention: dict[str, str]
    limitations: tuple[str, ...]
    raw: dict[str, Any]


def load_packaged_b1528_current_heading_rudder_config() -> B1528CurrentHeadingRudderConfig:
    """Load packaged B1528 SIROCCO current-heading/rudder YAML."""

    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "b1528_sirocco_current_heading_rudder.yml"
    )
    return validate_b1528_current_heading_rudder_config(
        yaml.safe_load(resource.read_text(encoding="utf-8"))
    )


def validate_b1528_current_heading_rudder_config(
    payload: dict[str, Any],
) -> B1528CurrentHeadingRudderConfig:
    """Validate the B1528 current-heading/rudder YAML contract."""

    required = {
        "case",
        "source_pack",
        "current_heading_rudder_report",
        "vessel",
        "rudder",
        "scenario",
        "environment",
        "workbook_regression",
        "force_convention",
        "limitations",
    }
    missing = sorted(required - set(payload))
    if missing:
        raise ValueError(f"missing required top-level sections: {missing}")

    scenario = payload["scenario"]
    workbook = payload["workbook_regression"]
    cfg = B1528CurrentHeadingRudderConfig(
        case_id=str(payload["case"]["id"]),
        aliases=tuple(str(value) for value in payload["case"].get("aliases", [])),
        source_pack_issue=str(payload["source_pack"]["issue"]),
        report_issue=str(payload["current_heading_rudder_report"]["issue"]),
        plan_path=str(payload["current_heading_rudder_report"]["plan"]),
        lbp_m=_positive("lbp_m", payload["vessel"]["lbp_m"]),
        yaw_lever_m=_positive("yaw_lever_m", payload["vessel"]["yaw_lever_m"]),
        rudder_area_m2=_positive("rudder_area_m2", payload["rudder"]["area_m2"]),
        rudder_span_m=_positive("rudder_span_m", payload["rudder"]["span_m"]),
        ship_sog_kn=_nonnegative("ship_sog_kn", scenario["ship_sog_kn"]),
        current_speeds_kn=tuple(float(value) for value in scenario["current_speeds_kn"]),
        chart_default_current_speed_kn=float(scenario["chart_default_current_speed_kn"]),
        default_speed_policy=str(scenario["default_speed_policy"]),
        heading_offsets_deg=tuple(float(value) for value in scenario["heading_offsets_deg"]),
        rudder_angles_deg=tuple(float(value) for value in scenario["rudder_angles_deg"]),
        rho_kg_m3=_positive("rho_kg_m3", payload["environment"]["rho_kg_m3"]),
        beta=_positive("beta", workbook["beta"]),
        prop_rotation_factor=_positive("prop_rotation_factor", workbook["prop_rotation_factor"]),
        prop_rotation_factor_logic=str(workbook["prop_rotation_factor_logic"]),
        force_convention={key: str(value) for key, value in payload["force_convention"].items()},
        limitations=tuple(str(value) for value in payload["limitations"]),
        raw=payload,
    )
    if cfg.current_speeds_kn != (1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5):
        raise ValueError("current_speeds_kn must match the approved engineering sweep")
    expected_angles = tuple(float(value) for value in range(-10, 11))
    if cfg.heading_offsets_deg != expected_angles:
        raise ValueError("heading_offsets_deg must be -10..+10 deg in 1 deg steps")
    if cfg.rudder_angles_deg != expected_angles:
        raise ValueError("rudder_angles_deg must be -10..+10 deg in 1 deg steps")
    if cfg.chart_default_current_speed_kn != 4.56:
        raise ValueError("chart default current speed must be exact 4.56 kn")
    if cfg.prop_rotation_factor != NON_ROTATING_PROPELLER_CR:
        raise ValueError("current-heading/rudder report requires neutral Cr=1.0")
    return cfg


def speed_planes_for_calculation(cfg: B1528CurrentHeadingRudderConfig) -> tuple[float, ...]:
    """Return engineering speeds plus exact chart-default speed if absent."""

    speeds = list(cfg.current_speeds_kn)
    if cfg.chart_default_current_speed_kn not in speeds:
        speeds.append(cfg.chart_default_current_speed_kn)
    return tuple(sorted(speeds))


def run_b1528_current_heading_rudder_report(
    config: B1528CurrentHeadingRudderConfig | None = None,
) -> dict[str, Any]:
    """Run the current speed × heading offset × rudder angle sweep."""

    cfg = config or load_packaged_b1528_current_heading_rudder_config()
    rows = [
        _row(cfg, speed, heading, rudder)
        for speed in speed_planes_for_calculation(cfg)
        for heading in cfg.heading_offsets_deg
        for rudder in cfg.rudder_angles_deg
    ]
    result = {"metadata": _metadata(cfg), "rows": rows}
    result["summary"] = _summary(rows)
    result["sample_working_example"] = _sample_working_example(result)
    return result


def write_b1528_current_heading_rudder_report(
    result: dict[str, Any], output_dir: str | Path
) -> dict[str, str]:
    """Write CSV/JSON/provenance/manifest plus Markdown and interactive HTML."""

    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)
    csv_path = out / "b1528_sirocco_current_heading_rudder_results.csv"
    json_path = out / "b1528_sirocco_current_heading_rudder_results.json"
    provenance_path = out / "b1528_sirocco_current_heading_rudder_provenance.json"
    md_path = out / "b1528_sirocco_current_heading_rudder_report.md"
    html_path = out / "b1528_sirocco_current_heading_rudder_report.html"
    manifest_path = out / "b1528_sirocco_current_heading_rudder_manifest.json"

    headers = list(result["rows"][0])
    with csv_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=headers, lineterminator="\n")
        writer.writeheader()
        writer.writerows(result["rows"])
    json_path.write_text(json.dumps(result, indent=2), encoding="utf-8")
    provenance_path.write_text(json.dumps(_provenance(result), indent=2), encoding="utf-8")
    md_path.write_text(_markdown_report(result), encoding="utf-8")
    html_path.write_text(_html_report(result), encoding="utf-8")
    manifest = {
        "csv": str(csv_path),
        "json": str(json_path),
        "provenance": str(provenance_path),
        "markdown_report": str(md_path),
        "html_report": str(html_path),
        "manifest": str(manifest_path),
    }
    manifest_path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")
    return manifest


def _row(
    cfg: B1528CurrentHeadingRudderConfig,
    current_speed_kn: float,
    heading_offset_deg: float,
    rudder_angle_deg: float,
) -> dict[str, Any]:
    speed_m_s = current_speed_kn * KNOT_TO_M_PER_S
    base_force_N = cfg.beta * cfg.rudder_area_m2 * speed_m_s**2 * cfg.prop_rotation_factor
    psi_rad = math.radians(heading_offset_deg)
    alpha_deg = rudder_angle_deg - heading_offset_deg
    alpha_rad = math.radians(alpha_deg)
    normal_force_N = base_force_N * math.sin(alpha_rad)
    x_local_N = base_force_N * math.sin(alpha_rad) ** 2
    y_local_N = base_force_N * math.sin(alpha_rad) * math.cos(alpha_rad)
    x_ship_N = x_local_N * math.cos(psi_rad) - y_local_N * math.sin(psi_rad)
    y_ship_N = x_local_N * math.sin(psi_rad) + y_local_N * math.cos(psi_rad)
    n_ship_Nm = y_ship_N * cfg.yaw_lever_m
    is_extra_default = (
        current_speed_kn == cfg.chart_default_current_speed_kn
        and current_speed_kn not in cfg.current_speeds_kn
    )
    return {
        "case_id": cfg.case_id,
        "current_speed_kn": current_speed_kn,
        "is_chart_default_extra_speed": is_extra_default,
        "heading_offset_deg": heading_offset_deg,
        "rudder_angle_deg": rudder_angle_deg,
        "effective_rudder_inflow_angle_deg": alpha_deg,
        "current_speed_m_s": speed_m_s,
        "prop_rotation_factor": cfg.prop_rotation_factor,
        "prop_rotation_factor_basis": cfg.prop_rotation_factor_logic,
        "beta": cfg.beta,
        "rudder_area_m2": cfg.rudder_area_m2,
        "yaw_lever_m": cfg.yaw_lever_m,
        "base_force_N": base_force_N,
        "normal_force_N": normal_force_N,
        "force_x_local_downstream_N": x_local_N,
        "force_y_local_port_of_current_N": y_local_N,
        "force_x_ship_N": x_ship_N,
        "force_y_ship_port_N": y_ship_N,
        "force_z_heave_N": 0.0,
        "moment_k_roll_Nm": 0.0,
        "moment_m_pitch_Nm": 0.0,
        "moment_n_yaw_bow_port_Nm": n_ship_Nm,
        "moment_n_yaw_bow_port_kN_m": n_ship_Nm / 1000.0,
        "resultant_horizontal_force_N": math.hypot(x_ship_N, y_ship_N),
        "mooring_reaction_x_N": -x_ship_N,
        "mooring_reaction_y_N": -y_ship_N,
        "mooring_reaction_n_Nm": -n_ship_Nm,
        "force_component_basis": "rudder induced only; local current-frame loads rotated into ship-fixed COG X/Y/N",
    }


def _metadata(cfg: B1528CurrentHeadingRudderConfig) -> dict[str, Any]:
    return {
        "case_id": cfg.case_id,
        "aliases": list(cfg.aliases),
        "review_target_date": REPORT_REVIEW_TARGET_DATE,
        "scope": REPORT_SCOPE,
        "zero_effective_angle_note": ZERO_EFFECTIVE_ANGLE_NOTE,
        "method": (
            "V=kn*0.51444; F=beta*A_R*V^2*Cr; alpha=delta-psi; "
            "X_local=F*sin(alpha)^2; Y_local=F*sin(alpha)*cos(alpha); "
            "X_ship=X_local*cos(psi)-Y_local*sin(psi); "
            "Y_ship=X_local*sin(psi)+Y_local*cos(psi); N=Y_ship*yaw_lever"
        ),
        "current_speeds_kn": list(cfg.current_speeds_kn),
        "chart_default_current_speed_kn": cfg.chart_default_current_speed_kn,
        "chart_default_extra_speed_included": cfg.chart_default_current_speed_kn not in cfg.current_speeds_kn,
        "default_speed_policy": cfg.default_speed_policy,
        "heading_offsets_deg": list(cfg.heading_offsets_deg),
        "rudder_angles_deg": list(cfg.rudder_angles_deg),
        "design_data": {
            "lbp_m": cfg.lbp_m,
            "yaw_lever_m": cfg.yaw_lever_m,
            "rudder_area_m2": cfg.rudder_area_m2,
            "rudder_span_m": cfg.rudder_span_m,
            "ship_sog_kn": cfg.ship_sog_kn,
            "rho_kg_m3": cfg.rho_kg_m3,
            "beta": cfg.beta,
            "prop_rotation_factor": cfg.prop_rotation_factor,
            "force_convention": cfg.force_convention,
        },
        "analysis_assumptions": [
            "Vessel is moored with ship speed over ground equal to 0 kn.",
            "Heading/rudder effective-angle convention: alpha = rudder_angle_deg - heading_offset_deg.",
            "Positive heading rotates the local downstream current-force axis toward port from +X_ship.",
            "Local-to-ship transform rotates local current-frame X/Y loads into ship-fixed COG axes.",
            cfg.default_speed_policy,
            "Only rudder-induced COG force components are reported; hull current loads require vessel current coefficients.",
            "Mooring reactions are equal and opposite loads for static-equilibrium context only.",
        ],
        "limitations": list(cfg.limitations),
        "traceability_links": {
            "source_pack_issue": cfg.source_pack_issue,
            "current_heading_rudder_issue": cfg.report_issue,
            "plan": f"{GITHUB_REPO_BLOB}/{cfg.plan_path}",
            "durable_report": f"{GITHUB_REPO_BLOB}/docs/domains/marine-engineering/b1528-sirocco-current-heading-rudder-report.md",
            "generated_markdown_report": f"{GITHUB_REPO_BLOB}/outputs/b1528_sirocco/current_heading_rudder/b1528_sirocco_current_heading_rudder_report.md",
            "generated_html_report": f"{GITHUB_REPO_BLOB}/outputs/b1528_sirocco/current_heading_rudder/b1528_sirocco_current_heading_rudder_report.html",
            "packaged_input_yaml": f"{GITHUB_REPO_BLOB}/src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml",
            "report_generator": f"{GITHUB_REPO_BLOB}/src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py",
            "moored_current_reference": f"{GITHUB_REPO_BLOB}/src/digitalmodel/naval_architecture/b1528_sirocco_moored_current_report.py",
        },
        "prop_rotation_factor_note": PROPELLER_ROTATION_FACTOR_NOTE,
    }


def _summary(rows: list[dict[str, Any]]) -> dict[str, Any]:
    max_y = max(rows, key=lambda row: abs(row["force_y_ship_port_N"]))
    max_n = max(rows, key=lambda row: abs(row["moment_n_yaw_bow_port_Nm"]))
    max_resultant = max(rows, key=lambda row: row["resultant_horizontal_force_N"])
    return {
        "row_count": len(rows),
        "requested_engineering_row_count": sum(not row["is_chart_default_extra_speed"] for row in rows),
        "extra_default_row_count": sum(row["is_chart_default_extra_speed"] for row in rows),
        "max_abs_ship_sway_force_N": abs(max_y["force_y_ship_port_N"]),
        "max_abs_ship_sway_signed_force_N": max_y["force_y_ship_port_N"],
        "max_abs_ship_sway_case": _case_label(max_y),
        "max_abs_yaw_moment_kN_m": abs(max_n["moment_n_yaw_bow_port_kN_m"]),
        "max_abs_yaw_signed_moment_kN_m": max_n["moment_n_yaw_bow_port_kN_m"],
        "max_abs_yaw_case": _case_label(max_n),
        "max_resultant_horizontal_force_N": max_resultant["resultant_horizontal_force_N"],
        "max_resultant_case": _case_label(max_resultant),
    }


def _sample_working_example(result: dict[str, Any]) -> dict[str, Any]:
    row = next(
        item
        for item in result["rows"]
        if item["current_speed_kn"] == 4.56
        and item["heading_offset_deg"] == 0.0
        and item["rudder_angle_deg"] == 1.0
    )
    return {
        "data_point": "chart-default 4.56 kn, heading 0 deg, rudder +1 deg, Cr=1.0",
        "current_speed_kn": row["current_speed_kn"],
        "current_speed_m_s": row["current_speed_m_s"],
        "heading_offset_deg": row["heading_offset_deg"],
        "rudder_angle_deg": row["rudder_angle_deg"],
        "effective_rudder_inflow_angle_deg": row["effective_rudder_inflow_angle_deg"],
        "beta": row["beta"],
        "rudder_area_m2": row["rudder_area_m2"],
        "cr": row["prop_rotation_factor"],
        "base_force_N": row["base_force_N"],
        "normal_force_N": row["normal_force_N"],
        "force_x_ship_N": row["force_x_ship_N"],
        "force_y_ship_port_N": row["force_y_ship_port_N"],
        "moment_n_yaw_bow_port_kN_m": row["moment_n_yaw_bow_port_kN_m"],
    }


def _provenance(result: dict[str, Any]) -> dict[str, Any]:
    metadata = result["metadata"]
    return {
        "scope": metadata["scope"],
        "method": metadata["method"],
        "heading_rudder_effective_angle_convention": "alpha = rudder_angle_deg - heading_offset_deg",
        "local_to_ship_transform": {
            "x_ship": "X_local*cos(psi)-Y_local*sin(psi)",
            "y_ship": "X_local*sin(psi)+Y_local*cos(psi)",
            "n_ship": "Y_ship*yaw_lever_m",
        },
        "default_speed_policy": metadata["default_speed_policy"],
        "scope_exclusions": metadata["limitations"],
        "design_data": metadata["design_data"],
        "analysis_assumptions": metadata["analysis_assumptions"],
        "traceability_links": metadata["traceability_links"],
        "sample_working_example": result["sample_working_example"],
        "summary": result["summary"],
    }


def _markdown_report(result: dict[str, Any]) -> str:
    metadata = result["metadata"]
    sample = result["sample_working_example"]
    links = metadata["traceability_links"]
    lines = [
        "# B1528 SIROCCO Current-Heading/Rudder Force Component Report",
        "",
        f"Prepared for engineer review on {metadata['review_target_date']}.",
        "",
        "## Scope",
        "",
        metadata["scope"],
        "",
        "This is not a validated oblique-current hull/rudder interaction model; hull current loads are excluded.",
        "",
        "## Traceability links",
        "",
        f"- GitHub issue: [digitalmodel #598]({links['current_heading_rudder_issue']})",
        f"- Approved plan: [issue #598 plan]({links['plan']})",
        f"- Source pack issue: [workspace-hub #2569]({links['source_pack_issue']})",
        f"- Packaged input YAML: [b1528_sirocco_current_heading_rudder.yml]({links['packaged_input_yaml']})",
        f"- Report generator: [b1528_sirocco_current_heading_rudder_report.py]({links['report_generator']})",
        "",
        "## Design data",
        "",
        *_design_rows_markdown(metadata["design_data"]),
        "",
        "## Analysis methodology and assumptions",
        "",
        "```text",
        metadata["method"],
        "```",
        "",
        "- Heading/rudder effective-angle convention: `alpha = rudder_angle_deg - heading_offset_deg`.",
        "- Local-to-ship transform: `X_ship=X_local*cos(psi)-Y_local*sin(psi)`, `Y_ship=X_local*sin(psi)+Y_local*cos(psi)`.",
        f"- {metadata['default_speed_policy']}",
        f"- {metadata['zero_effective_angle_note']}",
        "",
        "## Sweep coverage",
        "",
        f"- Requested engineering rows: `{result['summary']['requested_engineering_row_count']}`.",
        f"- Extra 4.56 kn chart-default rows: `{result['summary']['extra_default_row_count']}`.",
        f"- Total generated rows: `{result['summary']['row_count']}`.",
        "",
        "## Sample working example",
        "",
        f"Data point: `{sample['data_point']}`.",
        "",
        f"- Speed conversion: `V = {sample['current_speed_kn']:.2f} kn * {KNOT_TO_M_PER_S:.5f} = {sample['current_speed_m_s']:.5f} m/s`.",
        f"- Base force: `F = {sample['beta']:.1f} * {sample['rudder_area_m2']:.6f} * {sample['current_speed_m_s']:.5f}^2 * {sample['cr']:.1f} = {sample['base_force_N']:.3f} N`.",
        f"- Ship-fixed COG components: `X_ship = {sample['force_x_ship_N']:.3f} N`, `Y_ship = {sample['force_y_ship_port_N']:.3f} N`, `N_ship = {sample['moment_n_yaw_bow_port_kN_m']:.6f} kN-m`.",
        "",
        "## Interpretation charts",
        "",
        "Chart 1 plots ship-fixed `X_ship`, `Y_ship`, and resultant horizontal force versus heading for the selected current speed and rudder angle. Chart 2 is a signed yaw-moment heatmap over heading offset and rudder angle for the selected current speed.",
        "",
        "## Limitations",
        "",
        *[f"- {item}" for item in metadata["limitations"]],
    ]
    return "\n".join(lines)


def _html_report(result: dict[str, Any]) -> str:
    metadata = result["metadata"]
    rows_json = json.dumps(result["rows"], separators=(",", ":"))
    speed_options = "\n".join(
        _speed_option(speed, metadata["chart_default_current_speed_kn"])
        for speed in sorted({row["current_speed_kn"] for row in result["rows"]})
    )
    rudder_options = "\n".join(
        f'<option value="{float(angle):.1f}"{ " selected" if float(angle) == 10.0 else ""}>{float(angle):.1f} deg</option>'
        for angle in metadata["rudder_angles_deg"]
    )
    summary_json = json.dumps(result["summary"], indent=2)
    return f"""<!doctype html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>B1528 SIROCCO Current-Heading/Rudder Forces</title>
<script src=\"https://cdn.plot.ly/plotly-2.35.2.min.js\"></script>
<style>
:root {{ --page-bg:#eef2f7; --paper:#fff; --ink:#172033; --muted:#607085; --line:#d7dee8; --accent:#155e95; --soft:#e8f2fb; }}
* {{ box-sizing:border-box; }}
body {{ margin:0; background:var(--page-bg); color:var(--ink); font-family:Aptos, 'Segoe UI', system-ui, sans-serif; line-height:1.55; }}
.report-shell {{ width:min(1180px, calc(100% - 56px)); margin:0 auto; padding:34px 0 50px; }}
.report-page {{ background:var(--paper); border:1px solid var(--line); border-radius:10px; padding:36px 44px; }}
h1 {{ margin:0 0 10px; font-size:2rem; }}
h2 {{ margin:28px 0 10px; font-size:1.25rem; }}
.eyebrow {{ color:var(--accent); font-size:.78rem; font-weight:700; letter-spacing:.08em; text-transform:uppercase; }}
.note-panel {{ background:var(--soft); border-left:4px solid var(--accent); padding:12px 16px; margin:16px 0; }}
.controls {{ display:flex; flex-wrap:wrap; gap:14px; align-items:end; padding:14px; border:1px solid var(--line); border-radius:8px; background:#f8fafc; }}
.controls label {{ display:flex; flex-direction:column; gap:5px; font-weight:700; color:var(--muted); }}
select {{ min-width:190px; padding:8px 10px; border:1px solid var(--line); border-radius:6px; background:white; color:var(--ink); }}
.chart {{ width:100%; height:460px; margin:16px 0 30px; border:1px solid var(--line); border-radius:8px; background:white; }}
.data-table {{ width:100%; border-collapse:collapse; margin:12px 0; font-size:.92rem; }}
.data-table th,.data-table td {{ border-bottom:1px solid var(--line); padding:8px 10px; text-align:left; vertical-align:top; }}
.method-block {{ background:#f8fafc; border:1px solid var(--line); border-radius:6px; padding:14px 16px; white-space:pre-wrap; overflow-x:auto; }}
@media print {{ @page {{ size:A4 landscape; margin:12mm; }} body {{ background:white; }} .report-shell {{ width:100%; padding:0; }} .report-page {{ border:0; padding:0; }} .chart {{ break-inside:avoid; height:360px; }} }}
</style>
</head>
<body>
<div class=\"report-shell\"><main class=\"report-page\">
<p class=\"eyebrow\">B1528 SIROCCO · Issue #598</p>
<h1>Current-heading/rudder force component chart set</h1>
<p>{REPORT_SCOPE}</p>
<div class=\"note-panel\"><strong>Default speed policy:</strong> 4.56 kn is an extra chart-default case outside the requested engineering sweep list. Rows are flagged with <code>is_chart_default_extra_speed</code>.</div>

<h2>Controls</h2>
<div class=\"controls\">
<label for=\"current-speed-select\">Current speed
<select id=\"current-speed-select\">{speed_options}</select>
</label>
<label for=\"rudder-angle-select\">Rudder angle for Chart 1
<select id=\"rudder-angle-select\">{rudder_options}</select>
</label>
</div>

<h2>Chart 1 — Ship-fixed force components over heading</h2>
<p>Shows <code>X_ship</code>, <code>Y_ship</code>, and resultant horizontal force in kN for the selected current speed and rudder angle. All 21 rudder angles are selectable.</p>
<div id=\"force-components-chart\" class=\"chart\" aria-label=\"Ship-fixed force component chart\"></div>

<h2>Chart 2 — Ship-fixed yaw moment heatmap over heading × rudder</h2>
<p>Shows signed <code>N_ship yaw moment (kN-m)</code> over heading offset and rudder angle for the selected current speed.</p>
<div id=\"yaw-moment-heatmap\" class=\"chart\" aria-label=\"Yaw moment heatmap\"></div>

<h2>Method and provenance</h2>
<pre class=\"method-block\">{metadata['method']}</pre>
<ul>
<li>Heading/rudder effective-angle convention: alpha = rudder_angle_deg - heading_offset_deg.</li>
<li>Local-to-ship transform rotates local current-frame X/Y loads into ship-fixed COG axes.</li>
<li>{ZERO_EFFECTIVE_ANGLE_NOTE}</li>
<li>Scope exclusion: hull current loads are not included; this is not a validated oblique-current hull/rudder interaction model.</li>
</ul>

<h2>Summary</h2>
<pre class=\"method-block\">{summary_json}</pre>
</main></div>
<script>
const ROWS = {rows_json};
const STANDARD_CHART_HEIGHT = 460;
const CHART_CONFIG = {{responsive: true, displaylogo: false}};
function selectedSpeed() {{ return parseFloat(document.getElementById('current-speed-select').value); }}
function selectedRudder() {{ return parseFloat(document.getElementById('rudder-angle-select').value); }}
function same(a, b) {{ return Math.abs(a - b) < 1e-9; }}
function byHeading(a, b) {{ return a.heading_offset_deg - b.heading_offset_deg; }}
function rowsForSpeed(speed) {{ return ROWS.filter(row => same(row.current_speed_kn, speed)); }}
function updateForceChart() {{
  const speed = selectedSpeed();
  const rudder = selectedRudder();
  const rows = ROWS.filter(row => same(row.current_speed_kn, speed) && same(row.rudder_angle_deg, rudder)).sort(byHeading);
  const x = rows.map(row => row.heading_offset_deg);
  const hover = rows.map(row => `alpha=${{row.effective_rudder_inflow_angle_deg}} deg<br>X_local=${{(row.force_x_local_downstream_N/1000).toFixed(3)}} kN<br>Y_local=${{(row.force_y_local_port_of_current_N/1000).toFixed(3)}} kN<br>N_ship=${{row.moment_n_yaw_bow_port_kN_m.toFixed(3)}} kN-m`);
  const traces = [
    {{x, y: rows.map(row => row.force_x_ship_N/1000), name:'X_ship (kN)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>X_ship=%{{y:.3f}} kN<br>%{{customdata}}<extra></extra>'}},
    {{x, y: rows.map(row => row.force_y_ship_port_N/1000), name:'Y_ship port (kN)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>Y_ship=%{{y:.3f}} kN<br>%{{customdata}}<extra></extra>'}},
    {{x, y: rows.map(row => row.resultant_horizontal_force_N/1000), name:'Resultant H (kN)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>Resultant=%{{y:.3f}} kN<br>%{{customdata}}<extra></extra>'}}
  ];
  Plotly.newPlot('force-components-chart', traces, {{title:`Ship-fixed force component (kN) · current ${{speed}} kn · rudder ${{rudder}} deg`, xaxis:{{title:'Heading offset (deg)'}}, yaxis:{{title:'Ship-fixed force component (kN)', zeroline:true}}, height:STANDARD_CHART_HEIGHT, margin:{{l:70,r:28,t:60,b:60}}, legend:{{orientation:'h'}}}}, CHART_CONFIG);
}}
function updateYawHeatmap() {{
  const speed = selectedSpeed();
  const speedRows = rowsForSpeed(speed);
  const headings = [...new Set(speedRows.map(row => row.heading_offset_deg))].sort((a,b)=>a-b);
  const rudders = [...new Set(speedRows.map(row => row.rudder_angle_deg))].sort((a,b)=>a-b);
  const z = rudders.map(rudder => headings.map(heading => {{
    const row = speedRows.find(item => same(item.heading_offset_deg, heading) && same(item.rudder_angle_deg, rudder));
    return row ? row.moment_n_yaw_bow_port_kN_m : null;
  }}));
  const trace = {{type:'heatmap', x:headings, y:rudders, z, colorscale:'RdBu', reversescale:true, zmid:0, colorbar:{{title:'N_ship yaw moment (kN-m)'}}, hovertemplate:'heading=%{{x}} deg<br>rudder=%{{y}} deg<br>N_ship yaw moment (kN-m)=%{{z:.3f}}<extra></extra>'}};
  Plotly.newPlot('yaw-moment-heatmap', [trace], {{title:`N_ship yaw moment (kN-m) · current ${{speed}} kn`, xaxis:{{title:'Heading offset (deg)'}}, yaxis:{{title:'Rudder angle (deg)'}}, height:STANDARD_CHART_HEIGHT, margin:{{l:70,r:90,t:60,b:60}}}}, CHART_CONFIG);
}}
function updateCharts() {{ updateForceChart(); updateYawHeatmap(); }}
document.getElementById('current-speed-select').addEventListener('change', updateCharts);
document.getElementById('rudder-angle-select').addEventListener('change', updateForceChart);
updateCharts();
</script>
</body>
</html>
"""


def _speed_option(speed: float, default_speed: float) -> str:
    selected = " selected" if speed == default_speed else ""
    suffix = " (chart default extra)" if speed == default_speed else ""
    return f'<option value="{speed:g}"{selected}>{speed:g} kn{suffix}</option>'


def _design_rows_markdown(design_data: dict[str, Any]) -> list[str]:
    rows = ["| Field | Value |", "|---|---:|"]
    for key, value in design_data.items():
        if isinstance(value, dict):
            continue
        rows.append(f"| `{key}` | `{value}` |")
    return rows


def _case_label(row: dict[str, Any]) -> str:
    return (
        f"current {row['current_speed_kn']:g} kn, heading {row['heading_offset_deg']:g} deg, "
        f"rudder {row['rudder_angle_deg']:g} deg"
    )


def _positive(name: str, value: Any) -> float:
    number = float(value)
    if number <= 0:
        raise ValueError(f"{name} must be positive")
    return number


def _nonnegative(name: str, value: Any) -> float:
    number = float(value)
    if number < 0:
        raise ValueError(f"{name} must be nonnegative")
    return number
