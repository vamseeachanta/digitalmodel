# ABOUTME: B1528 SIROCCO static yaw-moment workbook-regression and report wrapper.
# ABOUTME: Separates legacy workbook semantics from reusable digitalmodel yaw-moment output.
"""B1528/SIROCCO static rudder-yaw report utilities.

This project wrapper is intentionally bounded. It prepares static yaw-moment
sweeps and report artifacts for B1528 SIROCCO using the #2569 source pack. It is
not a full MMG simulation, incident reconstruction, or IMO/class compliance
assessment.
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

from digitalmodel.naval_architecture.yaw_moment import rudder_yaw_moment

KNOT_TO_M_PER_S = 0.51444
G_M_PER_S2 = 9.80665


@dataclass(frozen=True)
class B1528YawConfig:
    """Validated B1528 SIROCCO static yaw-moment configuration."""

    case_id: str
    aliases: tuple[str, ...]
    source_pack_issue: str
    lbp_m: float
    rudder_area_m2: float
    rudder_span_m: float
    legacy_yaw_lever_m: float
    workbook_beta: float
    prop_rotation_factors: dict[str, float]
    speeds_kn: list[float]
    rudder_angles_deg: list[float]
    rho_kg_m3: float
    modes: tuple[str, ...]
    raw: dict[str, Any]


def load_packaged_b1528_yaw_config() -> B1528YawConfig:
    """Load packaged B1528 SIROCCO static yaw-moment YAML."""

    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "b1528_sirocco_yaw_moment.yml"
    )
    return validate_b1528_yaw_config(yaml.safe_load(resource.read_text(encoding="utf-8")))


def validate_b1528_yaw_config(payload: dict[str, Any]) -> B1528YawConfig:
    """Validate the B1528 YAML contract used by the static report."""

    required = {"case", "source_pack", "vessel", "rudder", "workbook_regression", "sweep", "environment", "calculation_modes"}
    missing = sorted(required - set(payload))
    if missing:
        raise ValueError(f"missing required top-level sections: {missing}")

    vessel = payload["vessel"]
    rudder = payload["rudder"]
    workbook = payload["workbook_regression"]
    sweep = payload["sweep"]

    speeds = [float(v) for v in sweep["speeds_kn"]]
    angles = [float(v) for v in sweep["rudder_angles_deg"]]
    if 2.5 not in speeds:
        raise ValueError("B1528 sweep must include the requested 2.5 kn speed")
    if -1.0 not in angles or 1.0 not in angles:
        raise ValueError("B1528 sweep must include requested -1 and +1 degree rudder cases")

    cfg = B1528YawConfig(
        case_id=str(payload["case"]["id"]),
        aliases=tuple(str(v) for v in payload["case"].get("aliases", [])),
        source_pack_issue=str(payload["source_pack"]["issue"]),
        lbp_m=_positive("lbp_m", vessel["lbp_m"]),
        rudder_area_m2=_positive("rudder_area_m2", rudder["area_m2"]),
        rudder_span_m=_positive("rudder_span_m", rudder["span_m"]),
        legacy_yaw_lever_m=_positive("legacy_yaw_lever_m", workbook["legacy_yaw_lever_m"]),
        workbook_beta=_positive("workbook_beta", workbook["beta"]),
        prop_rotation_factors={k: float(v) for k, v in workbook["prop_rotation_factors"].items()},
        speeds_kn=speeds,
        rudder_angles_deg=angles,
        rho_kg_m3=_positive("rho_kg_m3", payload["environment"]["rho_kg_m3"]),
        modes=tuple(str(v) for v in payload["calculation_modes"]),
        raw=payload,
    )
    if set(cfg.modes) != {"workbook_regression", "digitalmodel_static_yaw"}:
        raise ValueError("B1528 report must keep workbook_regression and digitalmodel_static_yaw modes separate")
    return cfg


def _positive(name: str, value: float) -> float:
    value = float(value)
    if not math.isfinite(value) or value <= 0.0:
        raise ValueError(f"{name} must be finite and positive")
    return value


def run_b1528_static_yaw_report(config: B1528YawConfig | None = None) -> dict[str, Any]:
    """Run workbook-regression and reusable static-yaw sweeps."""

    cfg = config or load_packaged_b1528_yaw_config()
    rows: list[dict[str, Any]] = []
    for speed_kn in cfg.speeds_kn:
        speed_m_s = speed_kn * KNOT_TO_M_PER_S
        for angle in cfg.rudder_angles_deg:
            for rotation_case in ("port", "stbd"):
                cr = cfg.prop_rotation_factors[rotation_case]
                rows.append(_workbook_regression_row(cfg, speed_kn, speed_m_s, angle, rotation_case, cr))
            rows.append(_digitalmodel_static_row(cfg, speed_kn, speed_m_s, angle))
    return {
        "metadata": {
            "case_id": cfg.case_id,
            "aliases": list(cfg.aliases),
            "source_pack_issue": cfg.source_pack_issue,
            "scope": "preliminary static rudder-induced yaw moment; not a full MMG simulation; not an incident reconstruction; not an IMO compliance assessment; no class compliance conclusion",
            "workbook_note": "evaluated workbook yaw moment uses Fn via C23; workbook text mentions Ft",
        },
        "rows": rows,
        "operating_points": _operating_points(rows),
    }


def _workbook_regression_row(
    cfg: B1528YawConfig,
    speed_kn: float,
    speed_m_s: float,
    angle_deg: float,
    rotation_case: str,
    cr: float,
) -> dict[str, Any]:
    base_force_N = cfg.workbook_beta * cfg.rudder_area_m2 * speed_m_s**2 * cr
    alpha_rad = math.radians(angle_deg)
    transverse_force_N = base_force_N * math.sin(alpha_rad) * math.cos(alpha_rad)
    normal_force_N = base_force_N * math.sin(alpha_rad)
    yaw_moment_mt_m = normal_force_N / 1000.0 / G_M_PER_S2 * cfg.legacy_yaw_lever_m
    yaw_moment_kN_m = yaw_moment_mt_m * G_M_PER_S2
    return {
        "calculation_mode": "workbook_regression",
        "speed_kn": speed_kn,
        "speed_m_s": speed_m_s,
        "rudder_angle_deg": angle_deg,
        "rotation_case": rotation_case,
        "prop_rotation_factor": cr,
        "base_force_N": base_force_N,
        "transverse_force_N": transverse_force_N,
        "normal_force_N": normal_force_N,
        "workbook_force_for_yaw_moment": "Fn",
        "yaw_moment_mt_m": yaw_moment_mt_m,
        "yaw_moment_kN_m": yaw_moment_kN_m,
        "lever_arm_m": cfg.legacy_yaw_lever_m,
        "lever_mapping": "legacy_barrass_0_6_lbp",
    }


def _digitalmodel_static_row(
    cfg: B1528YawConfig,
    speed_kn: float,
    speed_m_s: float,
    angle_deg: float,
) -> dict[str, Any]:
    result = rudder_yaw_moment(
        velocity_m_s=speed_m_s,
        rho_kg_m3=cfg.rho_kg_m3,
        rudder_area_m2=cfg.rudder_area_m2,
        rudder_span_m=cfg.rudder_span_m,
        rudder_angle_deg=angle_deg,
        x_rudder_from_cg_m=-cfg.legacy_yaw_lever_m,
        behind_hull=True,
        positive_force_direction="port",
    )
    return {
        "calculation_mode": "digitalmodel_static_yaw",
        "speed_kn": speed_kn,
        "speed_m_s": speed_m_s,
        "rudder_angle_deg": angle_deg,
        "rotation_case": "not_applicable",
        "prop_rotation_factor": None,
        "base_force_N": None,
        "transverse_force_N": result.transverse_force_N,
        "normal_force_N": result.scalar_normal_force_N,
        "workbook_force_for_yaw_moment": "not_applicable",
        "yaw_moment_mt_m": result.yaw_moment_Nm / 1000.0 / G_M_PER_S2,
        "yaw_moment_kN_m": result.yaw_moment_Nm / 1000.0,
        "lever_arm_m": -cfg.legacy_yaw_lever_m,
        "lever_mapping": "legacy_0_6_lbp_mapped_for_comparison_only",
    }


def _operating_points(rows: list[dict[str, Any]]) -> list[dict[str, Any]]:
    return [
        row
        for row in rows
        if row["speed_kn"] == 2.5
        and row["rudder_angle_deg"] in {-1.0, 1.0}
        and (
            row["calculation_mode"] == "digitalmodel_static_yaw"
            or (row["rudder_angle_deg"] == 1.0 and row["rotation_case"] == "port")
            or (row["rudder_angle_deg"] == -1.0 and row["rotation_case"] == "stbd")
        )
    ]


def write_b1528_static_yaw_report(result: dict[str, Any], output_dir: str | Path) -> dict[str, str]:
    """Write CSV/JSON/provenance/manifest plus markdown and interactive HTML report."""

    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)
    csv_path = out / "b1528_sirocco_yaw_moment_results.csv"
    json_path = out / "b1528_sirocco_yaw_moment_results.json"
    provenance_path = out / "b1528_sirocco_yaw_moment_provenance.json"
    md_path = out / "b1528_sirocco_yaw_moment_report.md"
    html_path = out / "b1528_sirocco_yaw_moment_report.html"
    manifest_path = out / "b1528_sirocco_yaw_moment_manifest.json"

    rows = result["rows"]
    headers = list(rows[0])
    with csv_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=headers)
        writer.writeheader()
        writer.writerows(rows)
    json_path.write_text(json.dumps(result, indent=2), encoding="utf-8")
    provenance = _provenance(result)
    provenance_path.write_text(json.dumps(provenance, indent=2), encoding="utf-8")
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


def _provenance(result: dict[str, Any]) -> dict[str, Any]:
    return {
        "source_pack_issue": result["metadata"]["source_pack_issue"],
        "formula_boundary": {
            "workbook_regression": "F=beta*AR*V^2*Cr; Ft=F*sin(alpha)*cos(alpha); Fn=F*sin(alpha); evaluated yaw moment=(Fn/1000/g)*(0.6*LBP)",
            "digitalmodel_static_yaw": "digitalmodel yaw_moment.rudder_yaw_moment with legacy 0.6*LBP mapped for comparison only",
        },
        "limitations": result["metadata"]["scope"],
    }


def _markdown_report(result: dict[str, Any]) -> str:
    op_rows = result["operating_points"]
    table_lines = [
        "| Mode | Speed (kn) | Rudder (deg) | Rotation | Yaw moment (kN-m) | Notes |",
        "|---|---:|---:|---|---:|---|",
    ]
    for row in op_rows:
        table_lines.append(
            f"| {row['calculation_mode']} | {row['speed_kn']:.1f} | {row['rudder_angle_deg']:.1f} | {row['rotation_case']} | {row['yaw_moment_kN_m']:.6f} | {row['lever_mapping']} |"
        )
    return "\n".join(
        [
            "# B1528 SIROCCO Static Yaw-Moment Report",
            "",
            "This report supports issue #2570 using the completed #2569 source pack.",
            "",
            "## 2.5 kn ±1° operating-point table",
            "",
            *table_lines,
            "",
            "## Method boundary",
            "",
            "Workbook-regression rows reproduce the evaluated workbook family: the workbook text mentions Ft, but the evaluated yaw-moment cell uses Fn via C23. Digitalmodel rows are separately labeled and map the legacy 0.6*LBP lever to x_rudder_from_cg_m for comparison only.",
            "",
            "This is not a full MMG simulation, not an incident reconstruction, not an IMO compliance assessment, and no class compliance conclusion is made.",
            "",
            "## Interactive charts",
            "",
            "Open `b1528_sirocco_yaw_moment_report.html` for interactive Plotly charts of yaw moment versus rudder angle and speed.",
        ]
    )


def _html_report(result: dict[str, Any]) -> str:
    rows_json = json.dumps(result["rows"])
    return f"""<!doctype html>
<html lang=\"en\">
<head><meta charset=\"utf-8\"><title>B1528 SIROCCO Yaw Moment</title><script src=\"https://cdn.plot.ly/plotly-2.35.2.min.js\"></script></head>
<body>
<h1>B1528 SIROCCO Static Yaw-Moment Report</h1>
<p>Preliminary static rudder-induced yaw moment; not a full MMG simulation; not an incident reconstruction; not an IMO compliance assessment; no class compliance conclusion.</p>
<div id=\"angle-chart\"></div>
<div id=\"speed-chart\"></div>
<script>
const rows = {rows_json};
function groupedTrace(filterMode, xKey, groupKey) {{
  const subset = rows.filter(r => r.calculation_mode === filterMode && (filterMode !== 'workbook_regression' || r.rotation_case !== 'stbd' || r.rudder_angle_deg <= 0));
  const groups = [...new Set(subset.map(r => r[groupKey]))];
  return groups.map(g => {{
    const pts = subset.filter(r => r[groupKey] === g).sort((a,b) => a[xKey]-b[xKey]);
    return {{x: pts.map(r => r[xKey]), y: pts.map(r => r.yaw_moment_kN_m), mode: 'lines+markers', name: `${{filterMode}} ${{groupKey}}=${{g}}`}};
  }});
}}
Plotly.newPlot('angle-chart', groupedTrace('workbook_regression', 'rudder_angle_deg', 'speed_kn'), {{title: 'Workbook-regression yaw moment vs rudder angle', xaxis: {{title: 'Rudder angle (deg)'}}, yaxis: {{title: 'Yaw moment (kN-m)'}}}});
Plotly.newPlot('speed-chart', groupedTrace('digitalmodel_static_yaw', 'speed_kn', 'rudder_angle_deg'), {{title: 'Digitalmodel static yaw moment vs speed', xaxis: {{title: 'Speed (kn)'}}, yaxis: {{title: 'Yaw moment (kN-m)'}}}});
</script>
</body>
</html>
"""
