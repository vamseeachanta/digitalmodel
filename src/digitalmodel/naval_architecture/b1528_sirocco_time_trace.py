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

from digitalmodel.naval_architecture.b1528_sirocco_yaw_report import KNOT_TO_M_PER_S
from digitalmodel.naval_architecture.yaw_moment import rudder_yaw_moment

SCOPE_CAVEAT = (
    "preliminary first-order Nomoto time trace with rudder-local inflow diagnostics; "
    "rudder force and yaw moment are diagnostic only; source-gap sensitivity mode; "
    "not a full MMG simulation; not an incident reconstruction; not an IMO compliance assessment; "
    "no class compliance conclusion"
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
    return {
        "metadata": _metadata(cfg),
        "runs": runs,
        "benchmark": _benchmark_panel(),
    }


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
        writer = csv.DictWriter(stream, fieldnames=list(flat_rows[0]))
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
        "limitations": result["metadata"]["scope"],
        "benchmark": result["benchmark"],
    }


def _markdown_report(result: dict[str, Any]) -> str:
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
            "This report supports #2571 with a preliminary Nomoto time trace and rudder-local inflow feedback.",
            "",
            "## Method boundary",
            "",
            result["metadata"]["scope"],
            "",
            "The yaw-rate equation is Nomoto-driven. Rudder force and yaw moment are diagnostic only and are not fed back into `r_dot`, avoiding double-counting of Nomoto `K/T` and direct moment balance.",
            "",
            "## Scenario metrics",
            "",
            *metric_lines,
            "",
            "## Benchmark source-gap panel",
            "",
            f"`{result['benchmark']['panel_id']}`: {result['benchmark']['summary']}",
            "",
            "## Interactive charts",
            "",
            "Open `b1528_sirocco_time_trace_report.html` for trajectory, heading, yaw-rate, effective rudder angle, yaw moment, and benchmark-source-gap panels.",
        ]
    )


def _html_report(result: dict[str, Any]) -> str:
    rows_json = json.dumps(
        [
            {"scenario_id": run["scenario_id"], **row}
            for run in result["runs"]
            for row in run["rows"]
        ]
    )
    benchmark_json = json.dumps(result["benchmark"])
    return f"""<!doctype html>
<html lang=\"en\">
<head><meta charset=\"utf-8\"><title>B1528 SIROCCO Time Trace</title><script src=\"https://cdn.plot.ly/plotly-2.35.2.min.js\"></script></head>
<body>
<h1>B1528 SIROCCO Time-Trace Benchmark Report</h1>
<p>{result['metadata']['scope']}</p>
<p>Rudder force and yaw moment are diagnostic only in the Nomoto mode.</p>
<div id=\"trajectory-chart\"></div>
<div id=\"heading-chart\"></div>
<div id=\"yaw-rate-chart\"></div>
<div id=\"alpha-chart\"></div>
<div id=\"moment-chart\"></div>
<div id=\"benchmark-source-gap\"></div>
<script>
const rows = {rows_json};
const benchmark = {benchmark_json};
const scenarios = [...new Set(rows.map(r => r.scenario_id))];
function traces(xKey, yKey) {{ return scenarios.map(s => {{ const pts = rows.filter(r => r.scenario_id === s); return {{x: pts.map(r => r[xKey]), y: pts.map(r => r[yKey]), mode: 'lines', name: s}}; }}); }}
Plotly.newPlot('trajectory-chart', traces('x_m', 'y_m'), {{title: 'Trajectory', xaxis: {{title: 'x (m)'}}, yaxis: {{title: 'y (m)'}}}});
Plotly.newPlot('heading-chart', traces('time_s', 'heading_deg'), {{title: 'Heading vs time', xaxis: {{title: 'Time (s)'}}, yaxis: {{title: 'Heading (deg)'}}}});
Plotly.newPlot('yaw-rate-chart', traces('time_s', 'yaw_rate_deg_s'), {{title: 'Yaw rate vs time', xaxis: {{title: 'Time (s)'}}, yaxis: {{title: 'Yaw rate (deg/s)'}}}});
Plotly.newPlot('alpha-chart', traces('time_s', 'effective_rudder_angle_deg'), {{title: 'Effective rudder angle vs time', xaxis: {{title: 'Time (s)'}}, yaxis: {{title: 'Effective rudder angle (deg)'}}}});
Plotly.newPlot('moment-chart', traces('time_s', 'diagnostic_yaw_moment_kN_m'), {{title: 'Diagnostic yaw moment vs time', xaxis: {{title: 'Time (s)'}}, yaxis: {{title: 'Yaw moment (kN-m)'}}}});
document.getElementById('benchmark-source-gap').innerHTML = `<h2>benchmark-source-gap</h2><p>${{benchmark.summary}}</p>`;
</script>
</body>
</html>
"""


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
