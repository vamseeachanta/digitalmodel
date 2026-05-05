# ABOUTME: B1528 SIROCCO preliminary Nomoto time-trace report workflow.
# ABOUTME: Keeps rudder force and yaw moment as diagnostics to avoid double-counting Nomoto K/T.
"""B1528/SIROCCO preliminary time-trace report utilities.

This module implements the approved #2571 bounded model: first-order Nomoto
heading/yaw-rate state update with rudder-local inflow feedback used for
rudder-force and yaw-moment diagnostics only. It is not a full MMG simulation,
incident reconstruction, IMO compliance assessment, or class compliance result.
"""

from __future__ import annotations

import csv
import json
import math
from dataclasses import dataclass, replace
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
from digitalmodel.naval_architecture.yaw_moment import rudder_yaw_moment

SCOPE_CAVEAT = (
    "preliminary first-order Nomoto time trace with rudder-local inflow diagnostics; "
    "rudder force and yaw moment are diagnostic only; source-gap sensitivity mode; "
    "not a full MMG simulation; not an incident reconstruction; not an IMO compliance assessment; "
    "no class compliance conclusion"
)
TIME_TRACE_CR_NOTE = (
    f"{PROPELLER_ROTATION_FACTOR_NOTE} This time-trace report does not apply "
    "workbook side-dependent Cr values because its rudder diagnostics use the "
    "reusable digitalmodel static-yaw force model, not the legacy workbook "
    "regression. The reported time-trace diagnostic rows therefore carry "
    "Cr=1.0 as the neutral non-rotating/no-rotation-correction multiplier."
)
TIME_TRACE_CR_BASIS = (
    "Cr=1.0 neutral time-trace diagnostic value: non-rotating propeller or no "
    "workbook propeller-rotation correction applied"
)


@dataclass(frozen=True)
class B1528TimeTraceConfig:
    """Validated B1528 SIROCCO time-trace configuration."""

    case_id: str
    aliases: tuple[str, ...]
    source_pack_issue: str
    static_report_issue: str
    lbp_m: float
    rudder_x_from_cg_m: float
    rudder_area_m2: float
    rudder_span_m: float
    speed_kn: float
    rudder_angle_deg: float
    duration_s: float
    dt_s: float
    nomoto_k_per_s: float
    nomoto_t_s: float
    rho_kg_m3: float
    benchmark_classification: str
    limitations: tuple[str, ...]
    raw: dict[str, Any]

    @property
    def speed_m_s(self) -> float:
        """Scenario forward speed in m/s."""

        return self.speed_kn * KNOT_TO_M_PER_S

    def with_updates(self, **updates: Any) -> "B1528TimeTraceConfig":
        """Return a validated copy with selected scalar updates."""

        candidate = replace(self, **updates)
        _validate_scalar_config(candidate)
        return candidate


def load_packaged_b1528_time_trace_config() -> B1528TimeTraceConfig:
    """Load packaged B1528 SIROCCO time-trace YAML."""

    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "b1528_sirocco_time_trace.yml"
    )
    return validate_b1528_time_trace_config(
        yaml.safe_load(resource.read_text(encoding="utf-8"))
    )


def validate_b1528_time_trace_config(payload: dict[str, Any]) -> B1528TimeTraceConfig:
    """Validate the B1528 time-trace YAML contract."""

    required = {
        "case",
        "source_pack",
        "static_report",
        "vessel",
        "rudder",
        "scenario",
        "nomoto",
        "environment",
        "benchmark",
        "limitations",
    }
    missing = sorted(required - set(payload))
    if missing:
        raise ValueError(f"missing required top-level sections: {missing}")

    scenario = payload["scenario"]
    cfg = B1528TimeTraceConfig(
        case_id=str(payload["case"]["id"]),
        aliases=tuple(str(v) for v in payload["case"].get("aliases", [])),
        source_pack_issue=str(payload["source_pack"]["issue"]),
        static_report_issue=str(payload["static_report"]["issue"]),
        lbp_m=_positive("lbp_m", payload["vessel"]["lbp_m"]),
        rudder_x_from_cg_m=float(payload["vessel"]["rudder_x_from_cg_m"]),
        rudder_area_m2=_positive("rudder_area_m2", payload["rudder"]["area_m2"]),
        rudder_span_m=_positive("rudder_span_m", payload["rudder"]["span_m"]),
        speed_kn=_positive("speed_kn", scenario["speed_kn"]),
        rudder_angle_deg=float(scenario["rudder_angle_deg"]),
        duration_s=_positive("duration_s", scenario["duration_s"]),
        dt_s=_positive("dt_s", scenario["dt_s"]),
        nomoto_k_per_s=_positive("nomoto_k_per_s", payload["nomoto"]["k_per_s"]),
        nomoto_t_s=_positive("nomoto_t_s", payload["nomoto"]["t_s"]),
        rho_kg_m3=_positive("rho_kg_m3", payload["environment"]["rho_kg_m3"]),
        benchmark_classification=str(payload["benchmark"]["classification"]),
        limitations=tuple(str(v) for v in payload["limitations"]),
        raw=payload,
    )
    _validate_scalar_config(cfg)
    if cfg.rudder_x_from_cg_m >= 0.0:
        raise ValueError("rudder_x_from_cg_m must be aft/negative in the adopted vessel-fixed sign convention")
    return cfg


def simulate_b1528_time_trace(config: B1528TimeTraceConfig | None = None) -> dict[str, Any]:
    """Simulate a first-order Nomoto time trace with rudder-local inflow diagnostics."""

    cfg = config or load_packaged_b1528_time_trace_config()
    dt = cfg.dt_s
    steps = int(round(cfg.duration_s / dt))
    rudder_cmd_rad = math.radians(cfg.rudder_angle_deg)
    x = y = psi = r = 0.0
    rows: list[dict[str, Any]] = []

    for step in range(steps + 1):
        rows.append(_row(cfg, step * dt, x, y, psi, r, rudder_cmd_rad))
        if step == steps:
            break
        # Semi-implicit Euler: update yaw rate/heading before position so short-step
        # sensitivity is bounded without over-claiming high-order integration fidelity.
        state = _derivatives(cfg, r, psi, rudder_cmd_rad)
        r += state["r_dot"] * dt
        psi += r * dt
        x += cfg.speed_m_s * math.cos(psi) * dt
        y += cfg.speed_m_s * math.sin(psi) * dt

    return {
        "metadata": _metadata(cfg),
        "rows": rows,
        "metrics": _metrics(cfg, rows),
        "benchmark": _benchmark_panel(),
    }


def run_b1528_time_trace_report(config: B1528TimeTraceConfig | None = None) -> dict[str, Any]:
    """Run default, opposite-rudder, and zero-rudder scenarios for reporting."""

    cfg = config or load_packaged_b1528_time_trace_config()
    scenarios = [
        ("positive_rudder", cfg),
        ("negative_rudder", cfg.with_updates(rudder_angle_deg=-cfg.rudder_angle_deg)),
        ("zero_rudder", cfg.with_updates(rudder_angle_deg=0.0)),
    ]
    runs = []
    for scenario_id, scenario_cfg in scenarios:
        run = simulate_b1528_time_trace(scenario_cfg)
        run["scenario_id"] = scenario_id
        runs.append(run)
    result = {
        "metadata": _metadata(cfg),
        "runs": runs,
        "benchmark": _benchmark_panel(),
    }
    result["sample_working_example"] = _sample_working_example(result)
    return result


def write_b1528_time_trace_report(result: dict[str, Any], output_dir: str | Path) -> dict[str, str]:
    """Write CSV/JSON/provenance/manifest plus Markdown and interactive HTML report."""

    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)
    csv_path = out / "b1528_sirocco_time_trace_results.csv"
    json_path = out / "b1528_sirocco_time_trace_results.json"
    provenance_path = out / "b1528_sirocco_time_trace_provenance.json"
    md_path = out / "b1528_sirocco_time_trace_report.md"
    html_path = out / "b1528_sirocco_time_trace_report.html"
    manifest_path = out / "b1528_sirocco_time_trace_manifest.json"

    flat_rows = []
    for run in result["runs"]:
        for row in run["rows"]:
            flat_rows.append({"scenario_id": run["scenario_id"], **row})
    with csv_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(flat_rows[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(flat_rows)

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


def _derivatives(
    cfg: B1528TimeTraceConfig, yaw_rate_rad_s: float, heading_rad: float, rudder_cmd_rad: float
) -> dict[str, float]:
    local = _local_rudder_state(cfg, yaw_rate_rad_s, rudder_cmd_rad)
    r_dot = (cfg.nomoto_k_per_s * local["effective_alpha_rad"] - yaw_rate_rad_s) / cfg.nomoto_t_s
    return {
        "r_dot": r_dot,
        "psi_dot": yaw_rate_rad_s,
        "x_dot": cfg.speed_m_s * math.cos(heading_rad),
        "y_dot": cfg.speed_m_s * math.sin(heading_rad),
    }


def _row(
    cfg: B1528TimeTraceConfig,
    time_s: float,
    x_m: float,
    y_m: float,
    heading_rad: float,
    yaw_rate_rad_s: float,
    rudder_cmd_rad: float,
) -> dict[str, Any]:
    local = _local_rudder_state(cfg, yaw_rate_rad_s, rudder_cmd_rad)
    diagnostic = rudder_yaw_moment(
        velocity_m_s=local["local_speed_m_s"],
        rho_kg_m3=cfg.rho_kg_m3,
        rudder_area_m2=cfg.rudder_area_m2,
        rudder_span_m=cfg.rudder_span_m,
        rudder_angle_deg=math.degrees(local["effective_alpha_rad"]),
        x_rudder_from_cg_m=cfg.rudder_x_from_cg_m,
        behind_hull=True,
        positive_force_direction="port",
    )
    return {
        "time_s": time_s,
        "x_m": x_m,
        "y_m": y_m,
        "heading_deg": math.degrees(heading_rad),
        "yaw_rate_deg_s": math.degrees(yaw_rate_rad_s),
        "rudder_command_deg": cfg.rudder_angle_deg,
        "rudder_inflow_angle_deg": math.degrees(local["beta_r_rad"]),
        "effective_rudder_angle_deg": math.degrees(local["effective_alpha_rad"]),
        "rudder_local_speed_m_s": local["local_speed_m_s"],
        "prop_rotation_factor": NON_ROTATING_PROPELLER_CR,
        "prop_rotation_factor_basis": TIME_TRACE_CR_BASIS,
        "diagnostic_transverse_force_N": diagnostic.transverse_force_N,
        "diagnostic_normal_force_N": diagnostic.scalar_normal_force_N,
        "diagnostic_yaw_moment_kN_m": diagnostic.yaw_moment_Nm / 1000.0,
    }


def _local_rudder_state(
    cfg: B1528TimeTraceConfig, yaw_rate_rad_s: float, rudder_cmd_rad: float
) -> dict[str, float]:
    v_r = cfg.rudder_x_from_cg_m * yaw_rate_rad_s
    beta_r = math.atan2(-cfg.rudder_x_from_cg_m * yaw_rate_rad_s, cfg.speed_m_s)
    alpha_r = rudder_cmd_rad - beta_r
    return {
        "lateral_speed_m_s": v_r,
        "beta_r_rad": beta_r,
        "effective_alpha_rad": alpha_r,
        "local_speed_m_s": math.hypot(cfg.speed_m_s, v_r),
    }


def _metrics(cfg: B1528TimeTraceConfig, rows: list[dict[str, Any]]) -> dict[str, Any]:
    final = rows[-1]
    return {
        "final_heading_deg": final["heading_deg"],
        "final_yaw_rate_deg_s": final["yaw_rate_deg_s"],
        "advance_m": final["x_m"],
        "tactical_diameter_m": abs(final["y_m"]),
        "max_abs_effective_rudder_angle_deg": max(abs(row["effective_rudder_angle_deg"]) for row in rows),
        "dt_s": cfg.dt_s,
        "duration_s": cfg.duration_s,
    }


def _metadata(cfg: B1528TimeTraceConfig) -> dict[str, Any]:
    return {
        "case_id": cfg.case_id,
        "aliases": list(cfg.aliases),
        "source_pack_issue": cfg.source_pack_issue,
        "static_report_issue": cfg.static_report_issue,
        "scope": SCOPE_CAVEAT,
        "method": "v_R=x_R*r; beta_R=atan2(-x_R*r,U); alpha_R=delta-beta_R; U_R=hypot(U,v_R); r_dot=(K*alpha_R-r)/T; psi_dot=r; x_dot=U*cos(psi); y_dot=U*sin(psi)",
        "diagnostic_boundary": "rudder force and yaw moment are diagnostic only and are not fed back into r_dot",
        "prop_rotation_factor_note": TIME_TRACE_CR_NOTE,
        "prop_rotation_factor": NON_ROTATING_PROPELLER_CR,
        "prop_rotation_factor_basis": TIME_TRACE_CR_BASIS,
        "design_data": {
            "case_id": cfg.case_id,
            "lbp_m": cfg.lbp_m,
            "rudder_x_from_cg_m": cfg.rudder_x_from_cg_m,
            "rudder_area_m2": cfg.rudder_area_m2,
            "rudder_span_m": cfg.rudder_span_m,
            "speed_kn": cfg.speed_kn,
            "speed_m_s": cfg.speed_m_s,
            "rudder_angle_deg": cfg.rudder_angle_deg,
            "duration_s": cfg.duration_s,
            "dt_s": cfg.dt_s,
            "rho_kg_m3": cfg.rho_kg_m3,
            "nomoto_k_per_s": cfg.nomoto_k_per_s,
            "nomoto_t_s": cfg.nomoto_t_s,
            "prop_rotation_factor": NON_ROTATING_PROPELLER_CR,
            "sign_convention": "vessel-fixed x positive forward from CG; y positive port; rudder x is negative/aft",
        },
        "analysis_assumptions": [
            "First-order Nomoto yaw-rate model is used for preliminary source-gap sensitivity only.",
            "Nomoto K/T values are assumed and are not calibrated B1528 telemetry evidence.",
            "Rudder-local inflow feedback changes diagnostic alpha_R and U_R.",
            "Rudder force and yaw moment are diagnostic only and are not fed back into r_dot.",
            "Cr=1.0 is used as the neutral non-rotating/no workbook propeller-rotation correction multiplier.",
            "Benchmark source data are narrative anchors, not instrumented x/y trajectory data.",
        ],
        "traceability_links": {
            "source_pack_issue": f"{WORKSPACE_HUB_ISSUES}/2569",
            "static_yaw_report_issue": f"{WORKSPACE_HUB_ISSUES}/2570",
            "time_trace_report_issue": f"{WORKSPACE_HUB_ISSUES}/2571",
            "durable_report": f"{GITHUB_REPO_BLOB}/docs/domains/marine-engineering/b1528-sirocco-time-trace-report.md",
            "generated_markdown_report": f"{GITHUB_REPO_BLOB}/outputs/b1528_sirocco/time_trace/b1528_sirocco_time_trace_report.md",
            "generated_html_report": f"{GITHUB_REPO_BLOB}/outputs/b1528_sirocco/time_trace/b1528_sirocco_time_trace_report.html",
            "packaged_input_yaml": f"{GITHUB_REPO_BLOB}/src/digitalmodel/naval_architecture/data/b1528_sirocco_time_trace.yml",
            "report_generator": f"{GITHUB_REPO_BLOB}/src/digitalmodel/naval_architecture/b1528_sirocco_time_trace.py",
            "master_calculation_review": f"{GITHUB_REPO_BLOB}/docs/domains/marine-engineering/rudder-and-ship-force-calculation-review.md",
        },
        "nomoto": {"K_per_s": cfg.nomoto_k_per_s, "T_s": cfg.nomoto_t_s, "calibration": "assumed_source_gap_sensitivity"},
    }


def _benchmark_panel() -> dict[str, Any]:
    return {
        "panel_id": "benchmark-source-gap",
        "classification": "narrative_benchmark_not_instrumented_validation_dataset",
        "source": "docs/projects/acma/B1528/sirocco-turning-benchmark.yaml",
        "summary": "Extracted heading/SOG narrative anchors have no x/y coordinates and include tug/current/anchor effects; this report therefore shows source-gap/sensitivity context rather than fabricated trajectory overlay.",
    }


def _provenance(result: dict[str, Any]) -> dict[str, Any]:
    return {
        "source_pack_issue": result["metadata"]["source_pack_issue"],
        "static_report_issue": result["metadata"]["static_report_issue"],
        "formula_boundary": result["metadata"]["method"],
        "diagnostic_boundary": result["metadata"]["diagnostic_boundary"],
        "design_data": result["metadata"]["design_data"],
        "analysis_assumptions": result["metadata"]["analysis_assumptions"],
        "prop_rotation_factor_note": result["metadata"]["prop_rotation_factor_note"],
        "traceability_links": result["metadata"]["traceability_links"],
        "sample_working_example": result["sample_working_example"],
        "limitations": result["metadata"]["scope"],
        "benchmark": result["benchmark"],
    }


def _design_data_rows(design: dict[str, Any]) -> list[tuple[str, str]]:
    return [
        ("Case ID", str(design["case_id"])),
        ("LBP", f"{design['lbp_m']:.3f} m"),
        ("Rudder x from CG", f"{design['rudder_x_from_cg_m']:.3f} m"),
        ("Rudder area", f"{design['rudder_area_m2']:.6f} m^2"),
        ("Rudder span", f"{design['rudder_span_m']:.3f} m"),
        ("Forward speed", f"{design['speed_kn']:.3f} kn / {design['speed_m_s']:.5f} m/s"),
        ("Rudder command", f"{design['rudder_angle_deg']:.3f} deg"),
        ("Duration / time step", f"{design['duration_s']:.1f} s / {design['dt_s']:.1f} s"),
        ("Water density", f"{design['rho_kg_m3']:.1f} kg/m^3"),
        ("Nomoto K / T", f"{design['nomoto_k_per_s']:.6f} 1/s / {design['nomoto_t_s']:.3f} s"),
        ("Propeller rotation factor", f"Cr = {design['prop_rotation_factor']:.1f}"),
        ("Sign convention", design["sign_convention"]),
    ]


def _design_data_markdown(design: dict[str, Any]) -> list[str]:
    rows = ["| Item | Value |", "|---|---:|"]
    for label, value in _design_data_rows(design):
        rows.append(f"| {label} | `{value}` |")
    return rows


def _assumptions_markdown(assumptions: list[str]) -> list[str]:
    return [f"- {item}" for item in assumptions]


def _references_markdown(links: dict[str, str]) -> list[str]:
    return [
        f"- Source pack issue: [workspace-hub #2569]({links['source_pack_issue']})",
        f"- Static yaw report issue: [workspace-hub #2570]({links['static_yaw_report_issue']})",
        f"- Time-trace report issue: [workspace-hub #2571]({links['time_trace_report_issue']})",
        f"- Packaged input YAML: [b1528_sirocco_time_trace.yml]({links['packaged_input_yaml']})",
        f"- Report generator: [b1528_sirocco_time_trace.py]({links['report_generator']})",
        f"- Durable report page: [b1528-sirocco-time-trace-report.md]({links['durable_report']})",
        f"- Master calculation review: [rudder-and-ship-force-calculation-review.md]({links['master_calculation_review']})",
        f"- Generated HTML report: [b1528_sirocco_time_trace_report.html]({links['generated_html_report']})",
    ]


def _design_data_table_html(design: dict[str, Any]) -> str:
    rows = "\n".join(
        f"<tr><th>{label}</th><td>{value}</td></tr>"
        for label, value in _design_data_rows(design)
    )
    return f"""<table class=\"data-table\">
<tbody>
{rows}
</tbody>
</table>"""


def _engineering_schematics_html(design: dict[str, Any]) -> str:
    return f"""
<div class=\"diagram-grid\">
  <figure class=\"schematic\">
    <svg viewBox=\"0 0 620 310\" role=\"img\" aria-label=\"Vessel plan-view geometry schematic\">
      <defs>
        <marker id=\"arrow-blue\" markerWidth=\"10\" markerHeight=\"10\" refX=\"8\" refY=\"5\" orient=\"auto\">
          <path d=\"M0,0 L10,5 L0,10 Z\" fill=\"#155e95\" />
        </marker>
        <marker id=\"arrow-gray\" markerWidth=\"10\" markerHeight=\"10\" refX=\"8\" refY=\"5\" orient=\"auto\">
          <path d=\"M0,0 L10,5 L0,10 Z\" fill=\"#64748b\" />
        </marker>
      </defs>
      <rect x=\"1\" y=\"1\" width=\"618\" height=\"308\" rx=\"10\" fill=\"#f8fafc\" stroke=\"#d5dde8\" />
      <path d=\"M96 155 C125 76, 415 76, 535 155 C415 234, 125 234, 96 155 Z\" fill=\"#ffffff\" stroke=\"#155e95\" stroke-width=\"3\" />
      <line x1=\"80\" y1=\"155\" x2=\"555\" y2=\"155\" stroke=\"#94a3b8\" stroke-dasharray=\"8 6\" />
      <circle cx=\"310\" cy=\"155\" r=\"8\" fill=\"#155e95\" />
      <text x=\"310\" y=\"135\" text-anchor=\"middle\">CG</text>
      <line x1=\"310\" y1=\"155\" x2=\"135\" y2=\"155\" stroke=\"#ef4444\" stroke-width=\"3\" marker-end=\"url(#arrow-gray)\" />
      <rect x=\"118\" y=\"125\" width=\"12\" height=\"60\" fill=\"#ef4444\" />
      <text x=\"124\" y=\"112\" text-anchor=\"middle\">Rudder</text>
      <text x=\"220\" y=\"144\" text-anchor=\"middle\">x_R = {design['rudder_x_from_cg_m']:.1f} m</text>
      <line x1=\"96\" y1=\"258\" x2=\"535\" y2=\"258\" stroke=\"#155e95\" stroke-width=\"2\" marker-end=\"url(#arrow-blue)\" marker-start=\"url(#arrow-blue)\" />
      <line x1=\"96\" y1=\"238\" x2=\"96\" y2=\"270\" stroke=\"#155e95\" />
      <line x1=\"535\" y1=\"238\" x2=\"535\" y2=\"270\" stroke=\"#155e95\" />
      <text x=\"316\" y=\"286\" text-anchor=\"middle\">LBP = {design['lbp_m']:.1f} m</text>
      <line x1=\"310\" y1=\"155\" x2=\"420\" y2=\"155\" stroke=\"#155e95\" stroke-width=\"3\" marker-end=\"url(#arrow-blue)\" />
      <text x=\"424\" y=\"146\">+x forward</text>
      <line x1=\"310\" y1=\"155\" x2=\"310\" y2=\"92\" stroke=\"#155e95\" stroke-width=\"3\" marker-end=\"url(#arrow-blue)\" />
      <text x=\"320\" y=\"91\">+y port</text>
      <text x=\"104\" y=\"52\">Plan-view geometry schematic</text>
      <text x=\"104\" y=\"72\" class=\"small-label\">Schematic only; not a scaled hull drawing.</text>
    </svg>
    <figcaption>Vessel-fixed geometry convention used by the time-trace calculation.</figcaption>
  </figure>
  <figure class=\"schematic\">
    <svg viewBox=\"0 0 620 310\" role=\"img\" aria-label=\"Rudder local inflow and diagnostic force schematic\">
      <defs>
        <marker id=\"arrow-blue-inflow\" markerWidth=\"10\" markerHeight=\"10\" refX=\"8\" refY=\"5\" orient=\"auto\">
          <path d=\"M0,0 L10,5 L0,10 Z\" fill=\"#155e95\" />
        </marker>
        <marker id=\"arrow-green\" markerWidth=\"10\" markerHeight=\"10\" refX=\"8\" refY=\"5\" orient=\"auto\">
          <path d=\"M0,0 L10,5 L0,10 Z\" fill=\"#0f766e\" />
        </marker>
        <marker id=\"arrow-red\" markerWidth=\"10\" markerHeight=\"10\" refX=\"8\" refY=\"5\" orient=\"auto\">
          <path d=\"M0,0 L10,5 L0,10 Z\" fill=\"#dc2626\" />
        </marker>
      </defs>
      <rect x=\"1\" y=\"1\" width=\"618\" height=\"308\" rx=\"10\" fill=\"#f8fafc\" stroke=\"#d5dde8\" />
      <line x1=\"95\" y1=\"178\" x2=\"520\" y2=\"178\" stroke=\"#94a3b8\" stroke-dasharray=\"8 6\" />
      <line x1=\"220\" y1=\"218\" x2=\"432\" y2=\"128\" stroke=\"#0f766e\" stroke-width=\"4\" marker-end=\"url(#arrow-green)\" />
      <line x1=\"220\" y1=\"218\" x2=\"432\" y2=\"218\" stroke=\"#155e95\" stroke-width=\"3\" marker-end=\"url(#arrow-blue-inflow)\" />
      <line x1=\"220\" y1=\"218\" x2=\"220\" y2=\"128\" stroke=\"#155e95\" stroke-width=\"3\" marker-end=\"url(#arrow-blue-inflow)\" />
      <line x1=\"425\" y1=\"103\" x2=\"445\" y2=\"253\" stroke=\"#111827\" stroke-width=\"7\" />
      <line x1=\"430\" y1=\"178\" x2=\"430\" y2=\"83\" stroke=\"#dc2626\" stroke-width=\"3\" marker-end=\"url(#arrow-red)\" />
      <path d=\"M280 218 A82 82 0 0 1 303 165\" fill=\"none\" stroke=\"#0f766e\" stroke-width=\"2\" />
      <text x=\"305\" y=\"193\">beta_R</text>
      <text x=\"335\" y=\"111\">U_R</text>
      <text x=\"330\" y=\"238\">U = {design['speed_m_s']:.4f} m/s</text>
      <text x=\"125\" y=\"148\">v_R = x_R r</text>
      <text x=\"450\" y=\"82\">diagnostic F_y</text>
      <text x=\"454\" y=\"155\">delta = {design['rudder_angle_deg']:.1f} deg</text>
      <text x=\"91\" y=\"52\">Rudder-local inflow and diagnostic force convention</text>
      <text x=\"91\" y=\"72\" class=\"small-label\">alpha_R = delta - beta_R; yaw moment is reported but not fed back into r_dot.</text>
      <text x=\"92\" y=\"286\">Cr = {design['prop_rotation_factor']:.1f}; neutral no-rotation correction multiplier.</text>
    </svg>
    <figcaption>Rudder-local velocity, effective angle, and diagnostic yaw moment boundary.</figcaption>
  </figure>
</div>"""


def _assumptions_html(assumptions: list[str]) -> str:
    items = "\n".join(f"<li>{item}</li>" for item in assumptions)
    return f"<ul class=\"method-list\">\n{items}\n</ul>"


def _references_html(links: dict[str, str]) -> str:
    items = [
        ("Source pack issue", links["source_pack_issue"]),
        ("Static yaw report issue", links["static_yaw_report_issue"]),
        ("Time-trace report issue", links["time_trace_report_issue"]),
        ("Packaged input YAML", links["packaged_input_yaml"]),
        ("Report generator", links["report_generator"]),
        ("Durable report page", links["durable_report"]),
        ("Master calculation review", links["master_calculation_review"]),
        ("Generated Markdown report", links["generated_markdown_report"]),
        ("Generated HTML report", links["generated_html_report"]),
    ]
    rows = "\n".join(f"<li><a href=\"{href}\">{label}</a></li>" for label, href in items)
    return f"<ol class=\"reference-list\">\n{rows}\n</ol>"


def _markdown_report(result: dict[str, Any]) -> str:
    sample = result["sample_working_example"]
    links = result["metadata"]["traceability_links"]
    design = result["metadata"]["design_data"]
    metric_lines = ["| Scenario | Final heading (deg) | Final yaw rate (deg/s) | Advance (m) | Tactical diameter proxy (m) |", "|---|---:|---:|---:|---:|"]
    for run in result["runs"]:
        m = run["metrics"]
        metric_lines.append(
            f"| {run['scenario_id']} | {m['final_heading_deg']:.6f} | {m['final_yaw_rate_deg_s']:.6f} | {m['advance_m']:.3f} | {m['tactical_diameter_m']:.3f} |"
        )
    return "\n".join(
        [
            "# B1528 SIROCCO Time-Trace Benchmark Report",
            "",
            f"This report supports [workspace-hub #2571]({links['time_trace_report_issue']}) with a preliminary Nomoto time trace and rudder-local inflow feedback.",
            "",
            "## Traceability links",
            "",
            f"- Source pack issue: [workspace-hub #2569]({links['source_pack_issue']})",
            f"- Static yaw report issue: [workspace-hub #2570]({links['static_yaw_report_issue']})",
            f"- Time-trace report issue: [workspace-hub #2571]({links['time_trace_report_issue']})",
            f"- Durable report page: [b1528-sirocco-time-trace-report.md]({links['durable_report']})",
            f"- Generated Markdown report: [b1528_sirocco_time_trace_report.md]({links['generated_markdown_report']})",
            f"- Generated HTML report: [b1528_sirocco_time_trace_report.html]({links['generated_html_report']})",
            "",
            "## Design data",
            "",
            *_design_data_markdown(design),
            "",
            "The HTML/PDF report includes schematic figures for vessel-fixed geometry and rudder-local inflow/diagnostic force convention.",
            "",
            "## Method boundary",
            "",
            result["metadata"]["scope"],
            "",
            "The yaw-rate equation is Nomoto-driven. Rudder force and yaw moment are diagnostic only and are not fed back into `r_dot`, avoiding double-counting of Nomoto `K/T` and direct moment balance.",
            "",
            "## Analysis methodology and assumptions",
            "",
            "Calculation sequence:",
            "",
            "```text",
            result["metadata"]["method"],
            "```",
            "",
            *_assumptions_markdown(result["metadata"]["analysis_assumptions"]),
            "",
            "## Propeller rotation factor Cr",
            "",
            result["metadata"]["prop_rotation_factor_note"],
            "",
            f"Time-trace diagnostic rows use `{result['metadata']['prop_rotation_factor_basis']}`.",
            "",
            "## Sample working example",
            "",
            f"Data point: `{sample['data_point']}`.",
            "",
            f"- Initial local speed: `U_R = {sample['initial_local_speed_m_s']:.5f} m/s`.",
            f"- Initial effective rudder angle: `alpha_R = {sample['initial_effective_rudder_angle_deg']:.6f} deg = {sample['initial_effective_rudder_angle_rad']:.8f} rad`.",
            f"- Initial Nomoto acceleration: `r_dot = ({sample['nomoto_k_per_s']:.6f} * {sample['initial_effective_rudder_angle_rad']:.8f} - 0) / {sample['nomoto_t_s']:.3f} = {sample['initial_r_dot_rad_s2']:.10f} rad/s^2`.",
            f"- After `{sample['sample_dt_s']:.1f} s`, calculated yaw rate is `{sample['calculated_yaw_rate_deg_s']:.9f} deg/s`; the generated row reports `{sample['reported_yaw_rate_deg_s']:.9f} deg/s`.",
            f"- Initial diagnostic yaw moment is `{sample['initial_diagnostic_yaw_moment_kN_m']:.6f} kN-m` using neutral `Cr={sample['prop_rotation_factor']:.1f}`.",
            "",
            "The HTML report includes a sample-verification chart that highlights this early yaw-rate data point.",
            "",
            "## Scenario metrics",
            "",
            *metric_lines,
            "",
            "## Benchmark source-gap panel",
            "",
            f"`{result['benchmark']['panel_id']}`: {result['benchmark']['summary']}",
            "",
            "## References",
            "",
            *_references_markdown(links),
            "",
            "## Interactive charts",
            "",
            "Open `b1528_sirocco_time_trace_report.html` for trajectory, heading, yaw-rate, effective rudder angle, yaw moment, benchmark-source-gap, and sample-verification panels.",
        ]
    )


def _html_report(result: dict[str, Any]) -> str:
    sample = result["sample_working_example"]
    links = result["metadata"]["traceability_links"]
    design = result["metadata"]["design_data"]
    design_table_html = _design_data_table_html(design)
    schematics_html = _engineering_schematics_html(design)
    assumptions_html = _assumptions_html(result["metadata"]["analysis_assumptions"])
    references_html = _references_html(links)
    rows_json = json.dumps(
        [
            {"scenario_id": run["scenario_id"], **row}
            for run in result["runs"]
            for row in run["rows"]
        ]
    )
    benchmark_json = json.dumps(result["benchmark"])
    sample_json = json.dumps(sample)
    return f"""<!doctype html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>B1528 SIROCCO Time Trace</title>
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
.report-shell {{
  width: min(1180px, calc(100% - 64px));
  margin: 0 auto;
  padding: 40px 0 56px;
}}
.report-page {{
  background: var(--paper);
  border: 1px solid var(--line);
  border-radius: 8px;
  padding: 40px 48px 48px;
}}
.report-header {{
  border-bottom: 1px solid var(--line);
  margin-bottom: 28px;
  padding-bottom: 20px;
}}
.eyebrow {{
  color: var(--accent);
  font-size: 0.78rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  margin: 0 0 8px;
  text-transform: uppercase;
}}
h1 {{
  font-size: 2.1rem;
  line-height: 1.15;
  margin: 0 0 12px;
}}
h2 {{
  font-size: 1.25rem;
  margin: 0 0 12px;
}}
p {{ margin: 0 0 12px; }}
ul {{
  margin: 0;
  padding-left: 1.2rem;
}}
.report-section {{
  margin: 30px 0;
}}
.note-panel {{
  background: var(--accent-soft);
  border-left: 4px solid var(--accent);
  padding: 14px 18px;
}}
.note-panel p:last-child {{ margin-bottom: 0; }}
.chart {{
  width: 100%;
  height: 430px;
  margin: 18px 0 32px;
  border: 1px solid var(--line);
  border-radius: 6px;
  background: var(--paper);
}}
.source-gap {{
  background: #f8fafc;
  border: 1px solid var(--line);
  border-left: 4px solid var(--accent);
  border-radius: 6px;
  padding: 16px 18px;
}}
.data-table {{
  width: 100%;
  border-collapse: collapse;
  margin: 14px 0 22px;
  font-size: 0.94rem;
}}
.data-table th,
.data-table td {{
  border-bottom: 1px solid var(--line);
  padding: 9px 10px;
  text-align: left;
  vertical-align: top;
}}
.data-table th {{
  width: 32%;
  color: var(--muted);
  font-weight: 700;
}}
.diagram-grid {{
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 18px;
  margin-top: 18px;
}}
.schematic {{
  margin: 0;
  border: 1px solid var(--line);
  border-radius: 8px;
  background: #ffffff;
  padding: 12px;
}}
.schematic svg {{
  display: block;
  width: 100%;
  height: auto;
}}
.schematic text {{
  fill: var(--ink);
  font-family: Aptos, "Segoe UI", system-ui, sans-serif;
  font-size: 14px;
}}
.schematic .small-label {{
  fill: var(--muted);
  font-size: 12px;
}}
figcaption {{
  color: var(--muted);
  font-size: 0.86rem;
  margin-top: 8px;
}}
.method-block {{
  background: #f8fafc;
  border: 1px solid var(--line);
  border-radius: 6px;
  color: #243246;
  font-family: "Cascadia Mono", "SFMono-Regular", Consolas, monospace;
  font-size: 0.86rem;
  overflow-x: auto;
  padding: 14px 16px;
  white-space: pre-wrap;
}}
.method-list,
.reference-list {{
  padding-left: 1.3rem;
}}
.method-list li,
.reference-list li {{
  margin: 6px 0;
}}
@media (max-width: 760px) {{
  .report-shell {{
    width: min(100% - 28px, 1180px);
    padding: 18px 0 32px;
  }}
  .report-page {{
    padding: 26px 20px 30px;
  }}
  h1 {{ font-size: 1.7rem; }}
  .chart {{ height: 380px; }}
  .diagram-grid {{ grid-template-columns: 1fr; }}
}}
@media print {{
  @page {{ size: A4 landscape; margin: 12mm; }}
  body {{ background: #ffffff; }}
  .report-shell {{
    width: 100%;
    padding: 0;
  }}
  .report-page {{
    border: 0;
    border-radius: 0;
    padding: 0;
  }}
  .report-section, .chart, .source-gap {{
    break-inside: avoid;
    page-break-inside: avoid;
  }}
  .chart {{
    height: 360px;
    margin: 12px 0 22px;
  }}
  .diagram-grid {{
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }}
}}
</style>
</head>
<body>
<main class=\"report-shell\">
<article class=\"report-page\">
<header class=\"report-header\">
<p class=\"eyebrow\">B1528 SIROCCO</p>
<h1>Time-Trace Benchmark Report</h1>
<p>{result['metadata']['scope']}</p>
<p>Rudder force and yaw moment are diagnostic only in the Nomoto mode.</p>
</header>
<section class=\"report-section\">
<h2>Traceability links</h2>
<ul>
<li><a href=\"{links['source_pack_issue']}\">workspace-hub #2569 source pack</a></li>
<li><a href=\"{links['static_yaw_report_issue']}\">workspace-hub #2570 static yaw report issue</a></li>
<li><a href=\"{links['time_trace_report_issue']}\">workspace-hub #2571 time-trace report issue</a></li>
<li><a href=\"{links['durable_report']}\">durable report page</a></li>
<li><a href=\"{links['generated_markdown_report']}\">generated Markdown report on GitHub</a></li>
</ul>
</section>
<section class=\"report-section\">
<h2>Design data</h2>
{design_table_html}
{schematics_html}
</section>
<section class=\"report-section\">
<h2>Analysis methodology and assumptions</h2>
<p>The calculation sequence follows the source-gap preliminary Nomoto workflow below. Rudder force and yaw moment are reported as diagnostics and are not fed back into the Nomoto yaw-rate state.</p>
<pre class=\"method-block\">{result['metadata']['method']}</pre>
{assumptions_html}
</section>
<section class=\"report-section note-panel\">
<h2>Propeller rotation factor Cr</h2>
<p>{result['metadata']['prop_rotation_factor_note']}</p>
<p>{result['metadata']['prop_rotation_factor_basis']}</p>
</section>
<section class=\"report-section\">
<h2>Sample working example</h2>
<p>{sample['data_point']}: alpha_R={sample['initial_effective_rudder_angle_deg']:.6f} deg, r_dot={sample['initial_r_dot_rad_s2']:.10f} rad/s^2, reported yaw rate after {sample['sample_dt_s']:.1f} s={sample['reported_yaw_rate_deg_s']:.9f} deg/s, Cr={sample['prop_rotation_factor']:.1f}.</p>
</section>
<section class=\"report-section\">
<div id=\"trajectory-chart\" class=\"chart\"></div>
<div id=\"heading-chart\" class=\"chart\"></div>
<div id=\"yaw-rate-chart\" class=\"chart\"></div>
<div id=\"alpha-chart\" class=\"chart\"></div>
<div id=\"moment-chart\" class=\"chart\"></div>
</section>
<section id=\"benchmark-source-gap\" class=\"report-section source-gap\"></section>
<section class=\"report-section\">
<div id=\"sample-chart\" class=\"chart\"></div>
</section>
<section class=\"report-section\">
<h2>References</h2>
{references_html}
</section>
</article>
</main>
<script>
const rows = {rows_json};
const benchmark = {benchmark_json};
const sample = {sample_json};
const scenarios = [...new Set(rows.map(r => r.scenario_id))];
const STANDARD_CHART_HEIGHT = 430;
const CHART_CONFIG = {{responsive: true, displaylogo: false}};
function traces(xKey, yKey) {{
  return scenarios.map(s => {{
    const pts = rows.filter(r => r.scenario_id === s);
    return {{
      x: pts.map(r => r[xKey]),
      y: pts.map(r => r[yKey]),
      mode: 'lines',
      name: s,
      line: {{width: 2.2}}
    }};
  }});
}}
function chartLayout(title, xTitle, yTitle, options = {{}}) {{
  return {{
    title: {{text: title, x: 0.02, xanchor: 'left', font: {{size: 18, color: '#18212f'}}}},
    height: STANDARD_CHART_HEIGHT,
    autosize: true,
    margin: {{l: 72, r: 36, t: 64, b: 60}},
    font: {{family: 'Aptos, Segoe UI, system-ui, sans-serif', size: 13, color: '#253244'}},
    paper_bgcolor: '#ffffff',
    plot_bgcolor: '#ffffff',
    hovermode: 'x unified',
    legend: {{orientation: 'h', x: 0, y: -0.22}},
    xaxis: {{title: {{text: xTitle}}, gridcolor: '#e6ebf2', zerolinecolor: '#cfd7e3', automargin: true}},
    yaxis: {{title: {{text: yTitle}}, gridcolor: '#e6ebf2', zerolinecolor: '#cfd7e3', automargin: true}},
    ...options
  }};
}}
Plotly.newPlot('trajectory-chart', traces('x_m', 'y_m'), chartLayout('Trajectory', 'x (m)', 'y (m)', {{hovermode: 'closest'}}), CHART_CONFIG);
Plotly.newPlot('heading-chart', traces('time_s', 'heading_deg'), chartLayout('Heading angle vs time', 'Time (s)', 'Heading angle (deg)'), CHART_CONFIG);
Plotly.newPlot('yaw-rate-chart', traces('time_s', 'yaw_rate_deg_s'), chartLayout('Yaw rate vs time', 'Time (s)', 'Yaw rate (deg/s)'), CHART_CONFIG);
Plotly.newPlot('alpha-chart', traces('time_s', 'effective_rudder_angle_deg'), chartLayout('Effective rudder angle vs time', 'Time (s)', 'Effective rudder angle (deg)'), CHART_CONFIG);
Plotly.newPlot('moment-chart', traces('time_s', 'diagnostic_yaw_moment_kN_m'), chartLayout('Yaw moment vs time (diagnostic, Cr=1)', 'Time (s)', 'Yaw moment (kN-m)'), CHART_CONFIG);
document.getElementById('benchmark-source-gap').innerHTML = `<h2>Benchmark source gap</h2><p>${{benchmark.summary}}</p>`;
const sampleWindow = rows.filter(r => r.scenario_id === sample.scenario_id && r.time_s <= Math.max(10, sample.sample_time_s));
Plotly.newPlot('sample-chart', [
  {{x: sampleWindow.map(r => r.time_s), y: sampleWindow.map(r => r.yaw_rate_deg_s), mode: 'lines+markers', name: 'positive_rudder yaw rate', line: {{width: 2.2}}}},
  {{x: [sample.sample_time_s], y: [sample.reported_yaw_rate_deg_s], mode: 'markers+text', text: [`${{sample.reported_yaw_rate_deg_s.toFixed(9)}} deg/s`], textposition: 'top center', marker: {{size: 14}}, name: 'sample point'}}
], chartLayout('Sample verification point: first Nomoto yaw-rate step', 'Time (s)', 'Yaw rate (deg/s)'), CHART_CONFIG);
</script>
</body>
</html>
"""


def _sample_working_example(result: dict[str, Any]) -> dict[str, Any]:
    run = next(item for item in result["runs"] if item["scenario_id"] == "positive_rudder")
    first = run["rows"][0]
    second = run["rows"][1]
    sample_dt_s = second["time_s"] - first["time_s"]
    alpha_rad = math.radians(first["effective_rudder_angle_deg"])
    k_per_s = result["metadata"]["nomoto"]["K_per_s"]
    t_s = result["metadata"]["nomoto"]["T_s"]
    initial_r_dot_rad_s2 = (k_per_s * alpha_rad) / t_s
    calculated_yaw_rate_deg_s = math.degrees(initial_r_dot_rad_s2 * sample_dt_s)
    return {
        "data_point": "positive_rudder first time step",
        "scenario_id": "positive_rudder",
        "sample_time_s": second["time_s"],
        "sample_dt_s": sample_dt_s,
        "initial_local_speed_m_s": first["rudder_local_speed_m_s"],
        "initial_effective_rudder_angle_deg": first["effective_rudder_angle_deg"],
        "initial_effective_rudder_angle_rad": alpha_rad,
        "nomoto_k_per_s": k_per_s,
        "nomoto_t_s": t_s,
        "initial_r_dot_rad_s2": initial_r_dot_rad_s2,
        "calculated_yaw_rate_deg_s": calculated_yaw_rate_deg_s,
        "reported_yaw_rate_deg_s": second["yaw_rate_deg_s"],
        "prop_rotation_factor": first["prop_rotation_factor"],
        "prop_rotation_factor_basis": first["prop_rotation_factor_basis"],
        "initial_diagnostic_yaw_moment_kN_m": first["diagnostic_yaw_moment_kN_m"],
    }


def _positive(name: str, value: float) -> float:
    value = float(value)
    if not math.isfinite(value) or value <= 0.0:
        raise ValueError(f"{name} must be finite and positive")
    return value


def _validate_scalar_config(cfg: B1528TimeTraceConfig) -> None:
    for name in ("duration_s", "dt_s", "speed_kn", "nomoto_t_s", "nomoto_k_per_s"):
        _positive(name, getattr(cfg, name))
    if cfg.dt_s > cfg.duration_s:
        raise ValueError("dt_s must be no larger than duration_s")
    if abs(cfg.rudder_angle_deg) > 35.0:
        raise ValueError("rudder_angle_deg outside bounded preliminary range")
