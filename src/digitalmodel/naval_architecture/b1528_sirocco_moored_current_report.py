# ABOUTME: B1528 SIROCCO moored-current rudder force-component report workflow.
# ABOUTME: Reports rudder-induced X/Y/Z/K/M/N COG loads for engineer review.
"""B1528/SIROCCO moored-current rudder force component report utilities.

This module implements a bounded engineering review case: SIROCCO moored,
current passing the rudder at 3.5 kn, rudder angles 1-5 deg to port and
starboard, and propeller rotation correction set to neutral Cr=1.0.

The calculation reports rudder-induced loads reduced to COG. It does not
calculate hull current load, bank effects, tug loads, mooring-line stiffness,
propeller race, or class/IMO compliance.
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
    WORKSPACE_HUB_ISSUES,
)

REPORT_REVIEW_TARGET_DATE = "2026-05-06"
REPORT_SCOPE = (
    "rudder-induced moored-current force-component sweep at COG; current aligned "
    "with vessel centerline; hull current force, bank effect, tug loads, "
    "mooring-line stiffness, current-profile variation, and propeller race are "
    "excluded; no class compliance conclusion"
)


@dataclass(frozen=True)
class B1528MooredCurrentConfig:
    """Validated B1528 SIROCCO moored-current report configuration."""

    case_id: str
    aliases: tuple[str, ...]
    source_pack_issue: str
    moored_current_report_issue: str
    static_report_issue: str
    time_trace_report_issue: str
    lbp_m: float
    yaw_lever_m: float
    rudder_area_m2: float
    rudder_span_m: float
    ship_sog_kn: float
    current_speed_kn: float
    current_relative_direction: str
    rudder_angles_deg: tuple[float, ...]
    rho_kg_m3: float
    beta: float
    prop_rotation_factor: float
    prop_rotation_factor_logic: str
    force_convention: dict[str, str]
    limitations: tuple[str, ...]
    raw: dict[str, Any]

    @property
    def current_speed_m_s(self) -> float:
        """Current speed in m/s."""

        return self.current_speed_kn * KNOT_TO_M_PER_S

    @property
    def ship_sog_m_s(self) -> float:
        """Ship speed over ground in m/s."""

        return self.ship_sog_kn * KNOT_TO_M_PER_S


def load_packaged_b1528_moored_current_config() -> B1528MooredCurrentConfig:
    """Load packaged B1528 SIROCCO moored-current YAML."""

    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "b1528_sirocco_moored_current.yml"
    )
    return validate_b1528_moored_current_config(
        yaml.safe_load(resource.read_text(encoding="utf-8"))
    )


def validate_b1528_moored_current_config(payload: dict[str, Any]) -> B1528MooredCurrentConfig:
    """Validate B1528 moored-current YAML contract."""

    required = {
        "case",
        "source_pack",
        "moored_current_report",
        "static_report",
        "time_trace_report",
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
    cfg = B1528MooredCurrentConfig(
        case_id=str(payload["case"]["id"]),
        aliases=tuple(str(v) for v in payload["case"].get("aliases", [])),
        source_pack_issue=str(payload["source_pack"]["issue"]),
        moored_current_report_issue=str(payload["moored_current_report"]["issue"]),
        static_report_issue=str(payload["static_report"]["issue"]),
        time_trace_report_issue=str(payload["time_trace_report"]["issue"]),
        lbp_m=_positive("lbp_m", payload["vessel"]["lbp_m"]),
        yaw_lever_m=_positive("yaw_lever_m", payload["vessel"]["yaw_lever_m"]),
        rudder_area_m2=_positive("rudder_area_m2", payload["rudder"]["area_m2"]),
        rudder_span_m=_positive("rudder_span_m", payload["rudder"]["span_m"]),
        ship_sog_kn=_nonnegative("ship_sog_kn", scenario["ship_sog_kn"]),
        current_speed_kn=_positive("current_speed_kn", scenario["current_speed_kn"]),
        current_relative_direction=str(scenario["current_relative_direction"]),
        rudder_angles_deg=tuple(
            _positive("rudder_angle_deg", value)
            for value in scenario["rudder_angles_deg"]
        ),
        rho_kg_m3=_positive("rho_kg_m3", payload["environment"]["rho_kg_m3"]),
        beta=_positive("beta", workbook["beta"]),
        prop_rotation_factor=_positive(
            "prop_rotation_factor", workbook["prop_rotation_factor"]
        ),
        prop_rotation_factor_logic=str(workbook["prop_rotation_factor_logic"]),
        force_convention={
            key: str(value) for key, value in payload["force_convention"].items()
        },
        limitations=tuple(str(value) for value in payload["limitations"]),
        raw=payload,
    )
    if cfg.prop_rotation_factor != NON_ROTATING_PROPELLER_CR:
        raise ValueError("moored-current report requires neutral Cr=1.0")
    return cfg


def run_b1528_moored_current_report(
    config: B1528MooredCurrentConfig | None = None,
) -> dict[str, Any]:
    """Run port/starboard rudder-force sweep for the moored-current case."""

    cfg = config or load_packaged_b1528_moored_current_config()
    rows = []
    for side in ("port", "starboard"):
        side_multiplier = 1.0 if side == "port" else -1.0
        for magnitude in cfg.rudder_angles_deg:
            rows.append(_row(cfg, side, side_multiplier * magnitude))

    result = {
        "metadata": _metadata(cfg),
        "rows": rows,
    }
    result["sample_working_example"] = _sample_working_example(result)
    result["summary"] = _summary(rows)
    return result


def write_b1528_moored_current_report(result: dict[str, Any], output_dir: str | Path) -> dict[str, str]:
    """Write CSV/JSON/provenance/manifest plus Markdown and interactive HTML report."""

    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)
    csv_path = out / "b1528_sirocco_moored_current_results.csv"
    json_path = out / "b1528_sirocco_moored_current_results.json"
    provenance_path = out / "b1528_sirocco_moored_current_provenance.json"
    md_path = out / "b1528_sirocco_moored_current_report.md"
    html_path = out / "b1528_sirocco_moored_current_report.html"
    manifest_path = out / "b1528_sirocco_moored_current_manifest.json"

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


def _row(cfg: B1528MooredCurrentConfig, side: str, rudder_angle_deg: float) -> dict[str, Any]:
    alpha_rad = math.radians(rudder_angle_deg)
    speed_m_s = cfg.current_speed_m_s
    base_force_N = cfg.beta * cfg.rudder_area_m2 * speed_m_s**2 * cfg.prop_rotation_factor
    normal_force_N = base_force_N * math.sin(alpha_rad)
    sway_force_N = base_force_N * math.sin(alpha_rad) * math.cos(alpha_rad)
    surge_drag_N = abs(base_force_N * math.sin(alpha_rad) ** 2)
    yaw_moment_Nm = sway_force_N * cfg.yaw_lever_m
    resultant_horizontal_force_N = math.hypot(surge_drag_N, sway_force_N)
    return {
        "case_id": cfg.case_id,
        "side": side,
        "rudder_angle_deg": rudder_angle_deg,
        "rudder_angle_magnitude_deg": abs(rudder_angle_deg),
        "ship_sog_kn": cfg.ship_sog_kn,
        "current_speed_kn": cfg.current_speed_kn,
        "current_speed_m_s": speed_m_s,
        "prop_rotation_factor": cfg.prop_rotation_factor,
        "prop_rotation_factor_basis": cfg.prop_rotation_factor_logic,
        "beta": cfg.beta,
        "rudder_area_m2": cfg.rudder_area_m2,
        "yaw_lever_m": cfg.yaw_lever_m,
        "base_force_N": base_force_N,
        "normal_force_N": normal_force_N,
        "force_x_surge_downstream_N": surge_drag_N,
        "force_y_sway_port_N": sway_force_N,
        "force_z_heave_N": 0.0,
        "moment_k_roll_Nm": 0.0,
        "moment_m_pitch_Nm": 0.0,
        "moment_n_yaw_bow_port_Nm": yaw_moment_Nm,
        "moment_n_yaw_bow_port_kN_m": yaw_moment_Nm / 1000.0,
        "resultant_horizontal_force_N": resultant_horizontal_force_N,
        "mooring_reaction_x_upstream_N": -surge_drag_N,
        "mooring_reaction_y_N": -sway_force_N,
        "mooring_reaction_n_Nm": -yaw_moment_Nm,
        "force_component_basis": "rudder induced only; X positive downstream/current drag, Y positive port, N positive bow to port",
    }


def _metadata(cfg: B1528MooredCurrentConfig) -> dict[str, Any]:
    return {
        "case_id": cfg.case_id,
        "aliases": list(cfg.aliases),
        "review_target_date": REPORT_REVIEW_TARGET_DATE,
        "scope": REPORT_SCOPE,
        "method": "V=3.5 kn*0.51444; F=beta*A_R*V^2*Cr; Fn=F*sin(delta); X=F*sin(delta)^2 downstream; Y=F*sin(delta)*cos(delta); N=Y*(0.6*LBP)",
        "design_data": {
            "lbp_m": cfg.lbp_m,
            "yaw_lever_m": cfg.yaw_lever_m,
            "rudder_area_m2": cfg.rudder_area_m2,
            "rudder_span_m": cfg.rudder_span_m,
            "ship_sog_kn": cfg.ship_sog_kn,
            "current_speed_kn": cfg.current_speed_kn,
            "current_speed_m_s": cfg.current_speed_m_s,
            "rudder_angles_deg": list(cfg.rudder_angles_deg),
            "rho_kg_m3": cfg.rho_kg_m3,
            "beta": cfg.beta,
            "prop_rotation_factor": cfg.prop_rotation_factor,
            "force_convention": cfg.force_convention,
        },
        "analysis_assumptions": [
            "Vessel is moored with ship speed over ground equal to 0 kn.",
            "Current is aligned with the vessel centerline and passes the rudder at 3.5 kn.",
            "Rudder-induced loads are calculated with the B1528/Barrass workbook force family.",
            "Cr=1.0 is used because the propeller is not rotating or no propeller-rotation correction is applied.",
            "Only rudder-induced COG force components are reported; hull current loads require vessel current coefficients.",
            "Mooring reactions are shown as equal and opposite loads for static equilibrium context only.",
        ],
        "limitations": list(cfg.limitations),
        "traceability_links": {
            "source_pack_issue": f"{WORKSPACE_HUB_ISSUES}/2569",
            "moored_current_report_issue": f"{WORKSPACE_HUB_ISSUES}/2642",
            "static_yaw_report_issue": f"{WORKSPACE_HUB_ISSUES}/2570",
            "time_trace_report_issue": f"{WORKSPACE_HUB_ISSUES}/2571",
            "durable_moored_current_report": f"{GITHUB_REPO_BLOB}/docs/domains/marine-engineering/b1528-sirocco-moored-current-report.md",
            "durable_static_report": f"{GITHUB_REPO_BLOB}/docs/domains/marine-engineering/b1528-sirocco-yaw-moment-report.md",
            "durable_time_trace_report": f"{GITHUB_REPO_BLOB}/docs/domains/marine-engineering/b1528-sirocco-time-trace-report.md",
            "generated_markdown_report": f"{GITHUB_REPO_BLOB}/outputs/b1528_sirocco/moored_current/b1528_sirocco_moored_current_report.md",
            "generated_html_report": f"{GITHUB_REPO_BLOB}/outputs/b1528_sirocco/moored_current/b1528_sirocco_moored_current_report.html",
            "packaged_input_yaml": f"{GITHUB_REPO_BLOB}/src/digitalmodel/naval_architecture/data/b1528_sirocco_moored_current.yml",
            "report_generator": f"{GITHUB_REPO_BLOB}/src/digitalmodel/naval_architecture/b1528_sirocco_moored_current_report.py",
            "cr_note_source": f"{GITHUB_REPO_BLOB}/src/digitalmodel/naval_architecture/b1528_sirocco_yaw_report.py",
            "master_calculation_review": f"{GITHUB_REPO_BLOB}/docs/domains/marine-engineering/rudder-and-ship-force-calculation-review.md",
        },
        "prop_rotation_factor_note": PROPELLER_ROTATION_FACTOR_NOTE,
    }


def _summary(rows: list[dict[str, Any]]) -> dict[str, Any]:
    max_sway = max(rows, key=lambda row: abs(row["force_y_sway_port_N"]))
    max_yaw = max(rows, key=lambda row: abs(row["moment_n_yaw_bow_port_Nm"]))
    max_resultant = max(rows, key=lambda row: row["resultant_horizontal_force_N"])
    return {
        "max_abs_sway_force_N": max_sway["force_y_sway_port_N"],
        "max_abs_sway_case": _case_label(max_sway),
        "max_abs_yaw_moment_kN_m": max_yaw["moment_n_yaw_bow_port_kN_m"],
        "max_abs_yaw_case": _case_label(max_yaw),
        "max_resultant_horizontal_force_N": max_resultant["resultant_horizontal_force_N"],
        "max_resultant_case": _case_label(max_resultant),
    }


def _sample_working_example(result: dict[str, Any]) -> dict[str, Any]:
    row = next(
        item
        for item in result["rows"]
        if item["side"] == "port" and item["rudder_angle_magnitude_deg"] == 1.0
    )
    alpha_rad = math.radians(row["rudder_angle_deg"])
    return {
        "data_point": "moored current, 3.5 kn, port rudder 1 deg, Cr=1.0",
        "current_speed_kn": row["current_speed_kn"],
        "current_speed_m_s": row["current_speed_m_s"],
        "rudder_angle_deg": row["rudder_angle_deg"],
        "rudder_angle_rad": alpha_rad,
        "beta": row["beta"],
        "rudder_area_m2": row["rudder_area_m2"],
        "cr": row["prop_rotation_factor"],
        "base_force_N": row["base_force_N"],
        "normal_force_N": row["normal_force_N"],
        "force_x_surge_downstream_N": row["force_x_surge_downstream_N"],
        "force_y_sway_port_N": row["force_y_sway_port_N"],
        "moment_n_yaw_bow_port_kN_m": row["moment_n_yaw_bow_port_kN_m"],
    }


def _provenance(result: dict[str, Any]) -> dict[str, Any]:
    return {
        "scope": result["metadata"]["scope"],
        "method": result["metadata"]["method"],
        "design_data": result["metadata"]["design_data"],
        "analysis_assumptions": result["metadata"]["analysis_assumptions"],
        "limitations": result["metadata"]["limitations"],
        "traceability_links": result["metadata"]["traceability_links"],
        "sample_working_example": result["sample_working_example"],
        "summary": result["summary"],
    }


def _markdown_report(result: dict[str, Any]) -> str:
    metadata = result["metadata"]
    links = metadata["traceability_links"]
    sample = result["sample_working_example"]
    table_lines = [
        "| Side | Rudder (deg) | X downstream (N) | Y port (N) | N bow-port (kN-m) | Resultant H (N) |",
        "|---|---:|---:|---:|---:|---:|",
    ]
    for row in result["rows"]:
        table_lines.append(
            f"| {row['side']} | {row['rudder_angle_deg']:.1f} | {row['force_x_surge_downstream_N']:.3f} | {row['force_y_sway_port_N']:.3f} | {row['moment_n_yaw_bow_port_kN_m']:.6f} | {row['resultant_horizontal_force_N']:.3f} |"
        )
    design_rows = _design_rows_markdown(metadata["design_data"])
    assumption_rows = [f"- {item}" for item in metadata["analysis_assumptions"]]
    limitation_rows = [f"- {item}" for item in metadata["limitations"]]
    return "\n".join(
        [
            "# B1528 SIROCCO Moored-Current Rudder Force Component Report",
            "",
            f"Prepared for engineer review on {metadata['review_target_date']}.",
            "",
            "## Scope",
            "",
            metadata["scope"],
            "",
            "## Traceability links",
            "",
            f"- Source pack issue: [workspace-hub #2569]({links['source_pack_issue']})",
            f"- Moored-current report issue: [workspace-hub #2642]({links['moored_current_report_issue']})",
            f"- Static yaw report issue: [workspace-hub #2570]({links['static_yaw_report_issue']})",
            f"- Time-trace report issue: [workspace-hub #2571]({links['time_trace_report_issue']})",
            f"- Durable report: [b1528-sirocco-moored-current-report.md]({links['durable_moored_current_report']})",
            f"- Packaged input YAML: [b1528_sirocco_moored_current.yml]({links['packaged_input_yaml']})",
            f"- Report generator: [b1528_sirocco_moored_current_report.py]({links['report_generator']})",
            f"- Master calculation review: [rudder-and-ship-force-calculation-review.md]({links['master_calculation_review']})",
            "",
            "## Design data",
            "",
            *design_rows,
            "",
            "## Analysis methodology and assumptions",
            "",
            "```text",
            metadata["method"],
            "```",
            "",
            *assumption_rows,
            "",
            "## Force-component table at COG",
            "",
            *table_lines,
            "",
            "Unsupported components in this bounded rudder-only calculation are reported as zero: Z, K, and M. Hull current force components are not included without current coefficients.",
            "",
            "## Sample working example",
            "",
            f"Data point: `{sample['data_point']}`.",
            "",
            f"- Speed conversion: `V = {sample['current_speed_kn']:.1f} kn * {KNOT_TO_M_PER_S:.5f} = {sample['current_speed_m_s']:.5f} m/s`.",
            f"- Base force: `F = {sample['beta']:.1f} * {sample['rudder_area_m2']:.6f} * {sample['current_speed_m_s']:.5f}^2 * {sample['cr']:.1f} = {sample['base_force_N']:.3f} N`.",
            f"- Normal force: `Fn = F * sin({sample['rudder_angle_deg']:.1f} deg) = {sample['normal_force_N']:.3f} N`.",
            f"- COG components: `X = {sample['force_x_surge_downstream_N']:.3f} N`, `Y = {sample['force_y_sway_port_N']:.3f} N`, `N = {sample['moment_n_yaw_bow_port_kN_m']:.6f} kN-m`.",
            "",
            "## Limitations",
            "",
            *limitation_rows,
            "",
            "## References",
            "",
            f"- B1528 source pack issue: [workspace-hub #2569]({links['source_pack_issue']})",
            f"- B1528 moored-current generated HTML report: [generated HTML]({links['generated_html_report']})",
            f"- B1528 static yaw report: [durable report]({links['durable_static_report']})",
            f"- B1528 time-trace report: [durable report]({links['durable_time_trace_report']})",
            f"- Propeller rotation factor note source: [b1528_sirocco_yaw_report.py]({links['cr_note_source']})",
            "",
            "## Interactive charts",
            "",
            "Open `b1528_sirocco_moored_current_report.html` for force and moment charts.",
        ]
    )


def _html_report(result: dict[str, Any]) -> str:
    metadata = result["metadata"]
    links = metadata["traceability_links"]
    rows_json = json.dumps(result["rows"])
    sample_json = json.dumps(result["sample_working_example"])
    design_table = _design_table_html(metadata["design_data"])
    assumptions = "".join(f"<li>{item}</li>" for item in metadata["analysis_assumptions"])
    limitations = "".join(f"<li>{item}</li>" for item in metadata["limitations"])
    rows_table = _rows_table_html(result["rows"])
    return f"""<!doctype html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>B1528 SIROCCO Moored-Current Rudder Forces</title>
<script src=\"https://cdn.plot.ly/plotly-2.35.2.min.js\"></script>
<style>
:root {{
  --page-bg: #eef2f7;
  --paper: #ffffff;
  --ink: #18212f;
  --muted: #5d6877;
  --line: #d5dde8;
  --accent: #155e95;
  --accent-soft: #e8f2fb;
}}
* {{ box-sizing: border-box; }}
body {{
  margin: 0;
  background: var(--page-bg);
  color: var(--ink);
  font-family: Aptos, "Segoe UI", system-ui, sans-serif;
  font-size: 16px;
  line-height: 1.55;
}}
a {{ color: var(--accent); }}
.report-shell {{ width: min(1180px, calc(100% - 64px)); margin: 0 auto; padding: 40px 0 56px; }}
.report-page {{ background: var(--paper); border: 1px solid var(--line); border-radius: 8px; padding: 40px 48px 48px; }}
.report-header {{ border-bottom: 1px solid var(--line); margin-bottom: 28px; padding-bottom: 20px; }}
.eyebrow {{ color: var(--accent); font-size: 0.78rem; font-weight: 700; letter-spacing: 0.08em; margin: 0 0 8px; text-transform: uppercase; }}
h1 {{ font-size: 2.1rem; line-height: 1.15; margin: 0 0 12px; }}
h2 {{ font-size: 1.25rem; margin: 0 0 12px; }}
p {{ margin: 0 0 12px; }}
.report-section {{ margin: 30px 0; }}
.note-panel {{ background: var(--accent-soft); border-left: 4px solid var(--accent); padding: 14px 18px; }}
.data-table {{ width: 100%; border-collapse: collapse; margin: 14px 0 22px; font-size: 0.92rem; }}
.data-table th, .data-table td {{ border-bottom: 1px solid var(--line); padding: 8px 10px; text-align: left; vertical-align: top; }}
.data-table th {{ color: var(--muted); font-weight: 700; }}
.force-table th, .force-table td {{ text-align: right; }}
.force-table th:first-child, .force-table td:first-child {{ text-align: left; }}
.chart {{ width: 100%; height: 430px; margin: 18px 0 32px; border: 1px solid var(--line); border-radius: 6px; background: var(--paper); }}
.diagram-grid {{ display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 18px; margin-top: 18px; }}
.schematic {{ margin: 0; border: 1px solid var(--line); border-radius: 8px; background: #ffffff; padding: 12px; }}
.schematic svg {{ display: block; width: 100%; height: auto; }}
.schematic text {{ fill: var(--ink); font-family: Aptos, "Segoe UI", system-ui, sans-serif; font-size: 14px; }}
.schematic .small-label {{ fill: var(--muted); font-size: 12px; }}
figcaption {{ color: var(--muted); font-size: 0.86rem; margin-top: 8px; }}
.method-block {{ background: #f8fafc; border: 1px solid var(--line); border-radius: 6px; color: #243246; font-family: "Cascadia Mono", "SFMono-Regular", Consolas, monospace; font-size: 0.86rem; overflow-x: auto; padding: 14px 16px; white-space: pre-wrap; }}
@media (max-width: 760px) {{
  .report-shell {{ width: min(100% - 28px, 1180px); padding: 18px 0 32px; }}
  .report-page {{ padding: 26px 20px 30px; }}
  h1 {{ font-size: 1.7rem; }}
  .chart {{ height: 380px; }}
  .diagram-grid {{ grid-template-columns: 1fr; }}
}}
@media print {{
  @page {{ size: A4 landscape; margin: 12mm; }}
  body {{ background: #ffffff; }}
  .report-shell {{ width: 100%; padding: 0; }}
  .report-page {{ border: 0; border-radius: 0; padding: 0; }}
  .report-section, .chart, .schematic {{ break-inside: avoid; page-break-inside: avoid; }}
  .chart {{ height: 360px; margin: 12px 0 22px; }}
}}
</style>
</head>
<body>
<main class=\"report-shell\">
<article class=\"report-page\">
<header class=\"report-header\">
<p class=\"eyebrow\">B1528 SIROCCO</p>
<h1>Moored-Current Rudder Force Component Report</h1>
<p>Prepared for engineer review on {metadata['review_target_date']}.</p>
<p>{metadata['scope']}</p>
</header>
<section class=\"report-section\">
<h2>Traceability links</h2>
<ul>
<li><a href=\"{links['source_pack_issue']}\">workspace-hub #2569 source pack</a></li>
<li><a href=\"{links['moored_current_report_issue']}\">workspace-hub #2642 moored-current report issue</a></li>
<li><a href=\"{links['static_yaw_report_issue']}\">workspace-hub #2570 static yaw report issue</a></li>
<li><a href=\"{links['time_trace_report_issue']}\">workspace-hub #2571 time-trace report issue</a></li>
<li><a href=\"{links['durable_moored_current_report']}\">durable moored-current report</a></li>
<li><a href=\"{links['packaged_input_yaml']}\">packaged input YAML</a></li>
<li><a href=\"{links['report_generator']}\">report generator</a></li>
</ul>
</section>
<section class=\"report-section\">
<h2>Design data</h2>
{design_table}
{_schematics_html(metadata['design_data'])}
</section>
<section class=\"report-section\">
<h2>Analysis methodology and assumptions</h2>
<pre class=\"method-block\">{metadata['method']}</pre>
<ul>{assumptions}</ul>
</section>
<section class=\"report-section note-panel\">
<h2>Propeller rotation factor Cr</h2>
<p>{metadata['prop_rotation_factor_note']}</p>
<p>Cr={metadata['design_data']['prop_rotation_factor']:.1f}: {result['rows'][0]['prop_rotation_factor_basis']}</p>
</section>
<section class=\"report-section\">
<h2>Force components at COG</h2>
{rows_table}
</section>
<section class=\"report-section\">
<div id=\"sway-chart\" class=\"chart\"></div>
<div id=\"yaw-chart\" class=\"chart\"></div>
<div id=\"surge-chart\" class=\"chart\"></div>
<div id=\"resultant-chart\" class=\"chart\"></div>
<div id=\"sample-chart\" class=\"chart\"></div>
</section>
<section class=\"report-section\">
<h2>Limitations</h2>
<ul>{limitations}</ul>
</section>
<section class=\"report-section\">
<h2>References</h2>
<ol>
<li><a href=\"{links['source_pack_issue']}\">B1528 source pack issue</a></li>
<li><a href=\"{links['moored_current_report_issue']}\">B1528 moored-current report issue</a></li>
<li><a href=\"{links['durable_moored_current_report']}\">B1528 durable moored-current report</a></li>
<li><a href=\"{links['durable_static_report']}\">B1528 static yaw report</a></li>
<li><a href=\"{links['durable_time_trace_report']}\">B1528 time-trace report</a></li>
<li><a href=\"{links['master_calculation_review']}\">Master rudder and ship-force calculation review</a></li>
</ol>
</section>
</article>
</main>
<script>
const rows = {rows_json};
const sample = {sample_json};
const STANDARD_CHART_HEIGHT = 430;
const CHART_CONFIG = {{responsive: true, displaylogo: false}};
function sideTrace(yKey) {{
  return ['port', 'starboard'].map(side => {{
    const pts = rows.filter(r => r.side === side).sort((a,b) => a.rudder_angle_magnitude_deg - b.rudder_angle_magnitude_deg);
    return {{x: pts.map(r => r.rudder_angle_magnitude_deg), y: pts.map(r => r[yKey]), mode: 'lines+markers', name: side, line: {{width: 2.2}}}};
  }});
}}
function chartLayout(title, yTitle) {{
  return {{
    title: {{text: title, x: 0.02, xanchor: 'left', font: {{size: 18, color: '#18212f'}}}},
    height: STANDARD_CHART_HEIGHT,
    autosize: true,
    margin: {{l: 78, r: 36, t: 64, b: 60}},
    font: {{family: 'Aptos, Segoe UI, system-ui, sans-serif', size: 13, color: '#253244'}},
    paper_bgcolor: '#ffffff',
    plot_bgcolor: '#ffffff',
    hovermode: 'x unified',
    legend: {{orientation: 'h', x: 0, y: -0.22}},
    xaxis: {{title: {{text: 'Rudder angle magnitude (deg)'}}, gridcolor: '#e6ebf2', zerolinecolor: '#cfd7e3', automargin: true}},
    yaxis: {{title: {{text: yTitle}}, gridcolor: '#e6ebf2', zerolinecolor: '#cfd7e3', automargin: true}}
  }};
}}
Plotly.newPlot('sway-chart', sideTrace('force_y_sway_port_N'), chartLayout('Sway force at COG vs rudder angle', 'Y force, port positive (N)'), CHART_CONFIG);
Plotly.newPlot('yaw-chart', sideTrace('moment_n_yaw_bow_port_kN_m'), chartLayout('Yaw moment at COG vs rudder angle', 'N moment, bow-port positive (kN-m)'), CHART_CONFIG);
Plotly.newPlot('surge-chart', sideTrace('force_x_surge_downstream_N'), chartLayout('Surge drag component vs rudder angle', 'X downstream drag (N)'), CHART_CONFIG);
Plotly.newPlot('resultant-chart', sideTrace('resultant_horizontal_force_N'), chartLayout('Horizontal resultant force vs rudder angle', 'Resultant horizontal force (N)'), CHART_CONFIG);
Plotly.newPlot('sample-chart', [{{
  x: [Math.abs(sample.rudder_angle_deg)],
  y: [sample.moment_n_yaw_bow_port_kN_m],
  mode: 'markers+text',
  text: [`${{sample.moment_n_yaw_bow_port_kN_m.toFixed(3)}} kN-m`],
  textposition: 'top center',
  marker: {{size: 14}},
  name: 'sample point'
}}], chartLayout('Sample verification point: 3.5 kn current, port 1 deg', 'N moment (kN-m)'), CHART_CONFIG);
</script>
</body>
</html>
"""


def _design_rows_markdown(design: dict[str, Any]) -> list[str]:
    rows = ["| Item | Value |", "|---|---:|"]
    for label, value in _design_rows(design):
        rows.append(f"| {label} | `{value}` |")
    return rows


def _design_table_html(design: dict[str, Any]) -> str:
    rows = "\n".join(f"<tr><th>{label}</th><td>{value}</td></tr>" for label, value in _design_rows(design))
    return f"<table class=\"data-table\"><tbody>{rows}</tbody></table>"


def _design_rows(design: dict[str, Any]) -> list[tuple[str, str]]:
    convention = design["force_convention"]
    return [
        ("LBP", f"{design['lbp_m']:.3f} m"),
        ("Yaw lever", f"{design['yaw_lever_m']:.3f} m"),
        ("Rudder area", f"{design['rudder_area_m2']:.6f} m^2"),
        ("Rudder span", f"{design['rudder_span_m']:.3f} m"),
        ("Ship speed over ground", f"{design['ship_sog_kn']:.3f} kn"),
        ("Current speed", f"{design['current_speed_kn']:.3f} kn / {design['current_speed_m_s']:.5f} m/s"),
        ("Rudder angle sweep", ", ".join(f"+/-{value:g} deg" for value in design["rudder_angles_deg"])),
        ("Beta", f"{design['beta']:.1f}"),
        ("Cr", f"{design['prop_rotation_factor']:.1f}"),
        ("X convention", convention["x_positive"]),
        ("Y convention", convention["y_positive"]),
        ("N convention", convention["yaw_positive"]),
    ]


def _rows_table_html(rows: list[dict[str, Any]]) -> str:
    body = "\n".join(
        "<tr>"
        f"<td>{row['side']}</td>"
        f"<td>{row['rudder_angle_deg']:.1f}</td>"
        f"<td>{row['force_x_surge_downstream_N']:.3f}</td>"
        f"<td>{row['force_y_sway_port_N']:.3f}</td>"
        f"<td>{row['force_z_heave_N']:.1f}</td>"
        f"<td>{row['moment_k_roll_Nm']:.1f}</td>"
        f"<td>{row['moment_m_pitch_Nm']:.1f}</td>"
        f"<td>{row['moment_n_yaw_bow_port_kN_m']:.6f}</td>"
        "</tr>"
        for row in rows
    )
    return f"""<table class=\"data-table force-table\">
<thead><tr><th>Side</th><th>Rudder (deg)</th><th>X (N)</th><th>Y (N)</th><th>Z (N)</th><th>K (N-m)</th><th>M (N-m)</th><th>N (kN-m)</th></tr></thead>
<tbody>{body}</tbody>
</table>"""


def _schematics_html(design: dict[str, Any]) -> str:
    return f"""
<div class=\"diagram-grid\">
  <figure class=\"schematic\">
    <svg viewBox=\"0 0 620 300\" role=\"img\" aria-label=\"Moored current rudder load schematic\">
      <defs>
        <marker id=\"arrow-current\" markerWidth=\"10\" markerHeight=\"10\" refX=\"8\" refY=\"5\" orient=\"auto\"><path d=\"M0,0 L10,5 L0,10 Z\" fill=\"#155e95\" /></marker>
        <marker id=\"arrow-force\" markerWidth=\"10\" markerHeight=\"10\" refX=\"8\" refY=\"5\" orient=\"auto\"><path d=\"M0,0 L10,5 L0,10 Z\" fill=\"#dc2626\" /></marker>
      </defs>
      <rect x=\"1\" y=\"1\" width=\"618\" height=\"298\" rx=\"10\" fill=\"#f8fafc\" stroke=\"#d5dde8\" />
      <path d=\"M95 150 C128 74, 414 74, 535 150 C414 226, 128 226, 95 150 Z\" fill=\"#ffffff\" stroke=\"#155e95\" stroke-width=\"3\" />
      <line x1=\"70\" y1=\"150\" x2=\"555\" y2=\"150\" stroke=\"#94a3b8\" stroke-dasharray=\"8 6\" />
      <circle cx=\"310\" cy=\"150\" r=\"8\" fill=\"#155e95\" />
      <text x=\"310\" y=\"130\" text-anchor=\"middle\">COG</text>
      <rect x=\"118\" y=\"122\" width=\"12\" height=\"56\" fill=\"#111827\" />
      <line x1=\"40\" y1=\"72\" x2=\"146\" y2=\"72\" stroke=\"#155e95\" stroke-width=\"4\" marker-end=\"url(#arrow-current)\" />
      <text x=\"40\" y=\"52\">current 3.5 kn</text>
      <line x1=\"124\" y1=\"150\" x2=\"124\" y2=\"82\" stroke=\"#dc2626\" stroke-width=\"3\" marker-end=\"url(#arrow-force)\" />
      <text x=\"138\" y=\"95\">Y rudder force</text>
      <line x1=\"124\" y1=\"150\" x2=\"184\" y2=\"150\" stroke=\"#dc2626\" stroke-width=\"3\" marker-end=\"url(#arrow-force)\" />
      <text x=\"184\" y=\"140\">X drag</text>
      <text x=\"96\" y=\"260\">Forces are transferred to COG with N = Y * {design['yaw_lever_m']:.1f} m.</text>
    </svg>
    <figcaption>Moored-current rudder-load transfer to COG.</figcaption>
  </figure>
  <figure class=\"schematic\">
    <svg viewBox=\"0 0 620 300\" role=\"img\" aria-label=\"COG force component convention schematic\">
      <defs>
        <marker id=\"arrow-axis\" markerWidth=\"10\" markerHeight=\"10\" refX=\"8\" refY=\"5\" orient=\"auto\"><path d=\"M0,0 L10,5 L0,10 Z\" fill=\"#155e95\" /></marker>
      </defs>
      <rect x=\"1\" y=\"1\" width=\"618\" height=\"298\" rx=\"10\" fill=\"#f8fafc\" stroke=\"#d5dde8\" />
      <circle cx=\"305\" cy=\"155\" r=\"10\" fill=\"#155e95\" />
      <text x=\"305\" y=\"135\" text-anchor=\"middle\">COG</text>
      <line x1=\"305\" y1=\"155\" x2=\"455\" y2=\"155\" stroke=\"#155e95\" stroke-width=\"4\" marker-end=\"url(#arrow-axis)\" />
      <text x=\"458\" y=\"146\">+X downstream</text>
      <line x1=\"305\" y1=\"155\" x2=\"305\" y2=\"62\" stroke=\"#155e95\" stroke-width=\"4\" marker-end=\"url(#arrow-axis)\" />
      <text x=\"315\" y=\"63\">+Y port</text>
      <path d=\"M225 211 A98 98 0 0 1 389 211\" fill=\"none\" stroke=\"#0f766e\" stroke-width=\"4\" marker-end=\"url(#arrow-axis)\" />
      <text x=\"265\" y=\"252\">+N bow to port</text>
      <text x=\"92\" y=\"50\">Reported force vector at COG</text>
      <text x=\"92\" y=\"74\" class=\"small-label\">Z, K, and M are zero in this rudder-only planar model.</text>
    </svg>
    <figcaption>COG force and moment sign convention used in the table.</figcaption>
  </figure>
</div>"""


def _case_label(row: dict[str, Any]) -> str:
    return f"{row['side']} {row['rudder_angle_magnitude_deg']:.1f} deg"


def _positive(name: str, value: float) -> float:
    value = float(value)
    if not math.isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be finite and positive")
    return value


def _nonnegative(name: str, value: float) -> float:
    value = float(value)
    if not math.isfinite(value) or value < 0:
        raise ValueError(f"{name} must be finite and non-negative")
    return value
