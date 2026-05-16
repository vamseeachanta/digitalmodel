# ABOUTME: B1528 SIROCCO current-heading/rudder force-component sweep report.
# ABOUTME: Generates ship-fixed force/moment tables plus interactive dropdown charts.
"""B1528/SIROCCO current-heading/rudder force component report utilities.

This module extends the existing B1528 moored-current rudder-only calculation to
an approved visualization sweep over current speed, heading offset, and rudder
angle.  It reports rudder-induced loads, first-cut OCIMF-inspired hull-current
review loads, and summed resultants in a ship-fixed COG frame.  It is not a validated
oblique-current hull/rudder/MMG model and deliberately excludes mooring-line
stiffness, tug loads, bank effects, and class compliance conclusions.
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
DEFAULT_CHART_RUDDER_ANGLE_DEG = 0.0
REPORT_ARTIFACT_STEM = "b1528_sirocco_current_heading_rudder_10deg_limit"
REPORT_DISPLAY_TITLE = "B1528 SIROCCO Current-Heading/Rudder Force Comparison — 10 deg Rudder Limit"
REPORT_DISPLAY_HEADING = "B1528 SIROCCO current-heading/rudder force comparison — 10 deg rudder limit"
OCIMF_CURRENT_CX_BASE = 1.05
OCIMF_CURRENT_CM_SCALE = 0.55
REPORT_SCOPE = (
    "SIROCCO current-heading/rudder force-component sweep at COG; rudder-induced "
    "ship-fixed X/Y/N components are derived by rotating local current-frame "
    "loads by heading offset, first-cut OCIMF-inspired hull current review loads "
    "are estimated separately using transparent placeholder heading functions, "
    "and the report sums current+rudder resultants; bank "
    "effect, tug loads, mooring-line stiffness, current-profile variation, "
    "propeller race, and class compliance conclusion are excluded"
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
    beam_m: float
    draft_m: float
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
        beam_m=_positive("beam_m", payload["vessel"].get("beam_m", 32.26)),
        draft_m=_positive("draft_m", payload["vessel"].get("draft_m", 12.2)),
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
    """Write CSV/JSON/provenance/manifest plus Markdown, interactive HTML, and PDF."""

    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)
    csv_path = out / f"{REPORT_ARTIFACT_STEM}_results.csv"
    json_path = out / f"{REPORT_ARTIFACT_STEM}_results.json"
    provenance_path = out / f"{REPORT_ARTIFACT_STEM}_provenance.json"
    md_path = out / f"{REPORT_ARTIFACT_STEM}_report.md"
    html_path = out / f"{REPORT_ARTIFACT_STEM}_report.html"
    pdf_path = out / f"{REPORT_ARTIFACT_STEM}_report.pdf"
    manifest_path = out / f"{REPORT_ARTIFACT_STEM}_manifest.json"

    headers = list(result["rows"][0])
    with csv_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=headers, lineterminator="\n")
        writer.writeheader()
        writer.writerows(result["rows"])
    json_path.write_text(json.dumps(result, indent=2), encoding="utf-8")
    provenance_path.write_text(json.dumps(_provenance(result), indent=2), encoding="utf-8")
    md_path.write_text(_markdown_report(result), encoding="utf-8")
    html_path.write_text(_html_report(result), encoding="utf-8")
    _write_pdf_from_html(html_path, pdf_path)
    manifest = {
        "csv": str(csv_path),
        "json": str(json_path),
        "provenance": str(provenance_path),
        "markdown_report": str(md_path),
        "html_report": str(html_path),
        "pdf_report": str(pdf_path),
        "manifest": str(manifest_path),
    }
    manifest_path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")
    return manifest


def _write_pdf_from_html(html_path: Path, pdf_path: Path) -> None:
    """Render the interactive HTML report to a static PDF using Playwright."""

    try:
        from playwright.sync_api import sync_playwright
    except ImportError as exc:  # pragma: no cover - dependency availability is environment-specific
        raise RuntimeError("Playwright is required to render the B1528 SIROCCO PDF report") from exc

    with sync_playwright() as playwright:
        browser = playwright.chromium.launch(headless=True)
        page = browser.new_page(viewport={"width": 1600, "height": 1000}, device_scale_factor=1)
        page.goto(html_path.resolve().as_uri(), wait_until="networkidle")
        page.emulate_media(media="print")
        page.pdf(path=str(pdf_path), format="A4", landscape=True, print_background=True)
        browser.close()


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
    heading_sin = math.sin(psi_rad)
    heading_abs_cos = abs(math.cos(psi_rad))
    dynamic_pressure_Pa = 0.5 * cfg.rho_kg_m3 * speed_m_s**2
    frontal_area_current_m2 = cfg.beam_m * cfg.draft_m
    lateral_area_current_m2 = cfg.lbp_m * cfg.draft_m
    ocimf_cx = OCIMF_CURRENT_CX_BASE * heading_abs_cos
    ocimf_cy = heading_sin
    ocimf_cm = OCIMF_CURRENT_CM_SCALE * heading_sin
    current_x_N = dynamic_pressure_Pa * frontal_area_current_m2 * ocimf_cx
    current_y_N = dynamic_pressure_Pa * lateral_area_current_m2 * ocimf_cy
    current_n_Nm = dynamic_pressure_Pa * lateral_area_current_m2 * cfg.lbp_m * ocimf_cm
    total_x_N = current_x_N + x_ship_N
    total_y_N = current_y_N + y_ship_N
    total_n_Nm = current_n_Nm + n_ship_Nm
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
        "ocimf_first_cut_dynamic_pressure_Pa": dynamic_pressure_Pa,
        "ocimf_first_cut_frontal_area_current_m2": frontal_area_current_m2,
        "ocimf_first_cut_lateral_area_current_m2": lateral_area_current_m2,
        "ocimf_first_cut_cx_current": ocimf_cx,
        "ocimf_first_cut_cy_current": ocimf_cy,
        "ocimf_first_cut_cm_current": ocimf_cm,
        "ocimf_current_force_x_ship_N": current_x_N,
        "ocimf_current_force_y_ship_port_N": current_y_N,
        "ocimf_current_moment_n_yaw_bow_port_Nm": current_n_Nm,
        "ocimf_current_moment_n_yaw_bow_port_kN_m": current_n_Nm / 1000.0,
        "ocimf_current_resultant_horizontal_force_N": math.hypot(current_x_N, current_y_N),
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
        "total_force_x_ship_N": total_x_N,
        "total_force_y_ship_port_N": total_y_N,
        "total_moment_n_yaw_bow_port_Nm": total_n_Nm,
        "total_moment_n_yaw_bow_port_kN_m": total_n_Nm / 1000.0,
        "total_resultant_horizontal_force_N": math.hypot(total_x_N, total_y_N),
        "rudder_to_ocimf_current_resultant_ratio": math.hypot(x_ship_N, y_ship_N) / math.hypot(current_x_N, current_y_N) if math.hypot(current_x_N, current_y_N) else 0.0,
        "mooring_reaction_x_N": -total_x_N,
        "mooring_reaction_y_N": -total_y_N,
        "mooring_reaction_n_Nm": -total_n_Nm,
        "force_component_basis": "rudder induced plus first-cut OCIMF-inspired hull-current review comparison; resultant is summed in ship-fixed COG X/Y/N",
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
            "beam_m": cfg.beam_m,
            "draft_m": cfg.draft_m,
            "yaw_lever_m": cfg.yaw_lever_m,
            "rudder_area_m2": cfg.rudder_area_m2,
            "rudder_span_m": cfg.rudder_span_m,
            "ship_sog_kn": cfg.ship_sog_kn,
            "rho_kg_m3": cfg.rho_kg_m3,
            "beta": cfg.beta,
            "prop_rotation_factor": cfg.prop_rotation_factor,
            "force_convention": cfg.force_convention,
            "ocimf_current_cx_base": OCIMF_CURRENT_CX_BASE,
            "ocimf_current_cm_scale": OCIMF_CURRENT_CM_SCALE,
        },
        "analysis_assumptions": [
            "Vessel is moored with ship speed over ground equal to 0 kn.",
            "Heading/rudder effective-angle convention: alpha = rudder_angle_deg - heading_offset_deg.",
            "Positive heading rotates the local downstream current-force axis toward port from +X_ship.",
            "Local-to-ship transform rotates local current-frame X/Y loads into ship-fixed COG axes.",
            cfg.default_speed_policy,
            "OCIMF-inspired hull current loads are first-cut review loads using projected beam*draft and LBP*draft areas plus transparent placeholder heading coefficients Cx=1.05*abs(cos(psi)), Cy=sin(psi), Cm=0.55*sin(psi); they are not vessel-specific OCIMF/current-coefficient curves or certified coefficients.",
            "Mooring reactions are equal and opposite loads for static-equilibrium context only.",
        ],
        "limitations": list(cfg.limitations),
        "traceability_links": {
            "source_pack_issue": cfg.source_pack_issue,
            "current_heading_rudder_issue": cfg.report_issue,
            "plan": f"{GITHUB_REPO_BLOB}/{cfg.plan_path}",
            "durable_report": f"{GITHUB_REPO_BLOB}/docs/domains/marine-engineering/b1528-sirocco-current-heading-rudder-report.md",
            "generated_markdown_report": f"{GITHUB_REPO_BLOB}/outputs/b1528_sirocco/current_heading_rudder_10deg_limit/{REPORT_ARTIFACT_STEM}_report.md",
            "generated_html_report": f"{GITHUB_REPO_BLOB}/outputs/b1528_sirocco/current_heading_rudder_10deg_limit/{REPORT_ARTIFACT_STEM}_report.html",
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
    max_current_resultant = max(rows, key=lambda row: row["ocimf_current_resultant_horizontal_force_N"])
    max_total_resultant = max(rows, key=lambda row: row["total_resultant_horizontal_force_N"])
    max_total_n = max(rows, key=lambda row: abs(row["total_moment_n_yaw_bow_port_Nm"]))
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
        "max_ocimf_current_resultant_horizontal_force_N": max_current_resultant["ocimf_current_resultant_horizontal_force_N"],
        "max_ocimf_current_resultant_case": _case_label(max_current_resultant),
        "max_total_resultant_horizontal_force_N": max_total_resultant["total_resultant_horizontal_force_N"],
        "max_total_resultant_case": _case_label(max_total_resultant),
        "max_abs_total_yaw_moment_kN_m": abs(max_total_n["total_moment_n_yaw_bow_port_kN_m"]),
        "max_abs_total_yaw_signed_moment_kN_m": max_total_n["total_moment_n_yaw_bow_port_kN_m"],
        "max_abs_total_yaw_case": _case_label(max_total_n),
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
        "ocimf_current_force_x_ship_N": row["ocimf_current_force_x_ship_N"],
        "ocimf_current_force_y_ship_port_N": row["ocimf_current_force_y_ship_port_N"],
        "ocimf_current_moment_n_yaw_bow_port_kN_m": row["ocimf_current_moment_n_yaw_bow_port_kN_m"],
        "force_x_ship_N": row["force_x_ship_N"],
        "force_y_ship_port_N": row["force_y_ship_port_N"],
        "moment_n_yaw_bow_port_kN_m": row["moment_n_yaw_bow_port_kN_m"],
        "total_force_x_ship_N": row["total_force_x_ship_N"],
        "total_force_y_ship_port_N": row["total_force_y_ship_port_N"],
        "total_moment_n_yaw_bow_port_kN_m": row["total_moment_n_yaw_bow_port_kN_m"],
        "total_resultant_horizontal_force_N": row["total_resultant_horizontal_force_N"],
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
        "ocimf_first_cut_current_loads": {
            "dynamic_pressure": "q=0.5*rho*V^2",
            "surge_force": "X_current=q*(beam*draft)*(1.05*abs(cos(psi)))",
            "sway_force": "Y_current=q*(LBP*draft)*sin(psi)",
            "yaw_moment_about_cog": "N_current=q*(LBP*draft)*LBP*(0.55*sin(psi))",
            "coefficient_caveat": "OCIMF-inspired placeholder heading functions, not vessel-specific OCIMF current-coefficient curves",
            "resultant": "total = current review load + rudder-induced component, summed in ship-fixed COG axes",
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
        f"# {REPORT_DISPLAY_TITLE} Report",
        "",
        f"Prepared for engineer review on {metadata['review_target_date']}.",
        "",
        "## Scope",
        "",
        metadata["scope"],
        "",
        "This includes first-cut hull-current + rudder totals for comparison only; it is not a validated whole-vessel current-load, oblique-current hull/rudder interaction, mooring, tug, bank-effect, or compliance model. The current terms are OCIMF-inspired placeholder heading functions, not vessel-specific OCIMF current-coefficient curves.",
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
        "- First-cut OCIMF-inspired current review loads: `q=0.5*rho*V^2`, `Cx=1.05*abs(cos(psi))`, `Cy=sin(psi)`, `Cm=0.55*sin(psi)`, `Xc=q*(beam*draft)*Cx`, `Yc=q*(LBP*draft)*Cy`, `Nc=q*(LBP*draft)*LBP*Cm`.",
        "- Resultants are summed at COG in ship-fixed axes: `X_total=X_current+X_rudder`, `Y_total=Y_current+Y_rudder`, `N_total=N_current+N_rudder`.",
        f"- {metadata['default_speed_policy']}",
        f"- {metadata['zero_effective_angle_note']}",
        "",
        "## Sweep coverage",
        "",
        f"- Requested engineering rows: `{result['summary']['requested_engineering_row_count']}`.",
        f"- Extra 4.56 kn chart-default rows: `{result['summary']['extra_default_row_count']}`.",
        f"- Total generated rows: `{result['summary']['row_count']}`.",
        "",
        "## Heading/rudder schematic",
        "",
        "Use the report schematic to read the geometry convention before reviewing force magnitudes: ship-fixed +X is forward, +Y is port, heading offset `psi` is positive bow-to-port, rudder command `delta` is positive to port, and the rudder inflow angle is `alpha = delta - psi`.",
        "",
        "## Sample working example",
        "",
        f"Data point: `{sample['data_point']}`.",
        "",
        f"- Speed conversion: `V = {sample['current_speed_kn']:.2f} kn * {KNOT_TO_M_PER_S:.5f} = {sample['current_speed_m_s']:.5f} m/s`.",
        f"- Base force: `F = {sample['beta']:.1f} * {sample['rudder_area_m2']:.6f} * {sample['current_speed_m_s']:.5f}^2 * {sample['cr']:.1f} = {sample['base_force_N']:.3f} N`.",
        f"- Rudder-induced COG components: `X_rudder = {sample['force_x_ship_N']:.3f} N`, `Y_rudder = {sample['force_y_ship_port_N']:.3f} N`, `N_rudder = {sample['moment_n_yaw_bow_port_kN_m']:.6f} kN-m`.",
        f"- OCIMF-inspired current-review components at this same point: `X_current = {sample['ocimf_current_force_x_ship_N']:.3f} N`, `Y_current = {sample['ocimf_current_force_y_ship_port_N']:.3f} N`, `N_current = {sample['ocimf_current_moment_n_yaw_bow_port_kN_m']:.6f} kN-m`.",
        f"- Combined resultant at COG: `X_total = {sample['total_force_x_ship_N']:.3f} N`, `Y_total = {sample['total_force_y_ship_port_N']:.3f} N`, `N_total = {sample['total_moment_n_yaw_bow_port_kN_m']:.6f} kN-m`, `R_total = {sample['total_resultant_horizontal_force_N'] / 1000.0:.6f} kN`.",
        "",
        "## OCIMF current vs rudder vs resultant",
        "",
        "The report exposes individual OCIMF-inspired current-review, rudder-induced, and total rows using `ocimf_current_*`, rudder `force_*`/`moment_*`, and `total_*` fields. Yaw moment is about the ship-fixed COG with positive bow-to-port convention.",
        "",
        "## Interpretation charts",

        "Chart 1 plots rudder-induced ship-fixed `X_ship`, `Y_ship`, and resultant horizontal force versus heading for the selected current speed and rudder angle. Chart 2 is a signed rudder-induced yaw-moment heatmap. Chart 3 overlays OCIMF-inspired current-review, rudder, and total horizontal resultants. Chart 4 overlays OCIMF-inspired current-review, rudder, and total yaw moment about COG. The HTML report also includes selected-speed and selected-case summary panels that update with the controls.",
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
        f'<option value="{float(angle):.1f}"{ " selected" if float(angle) == DEFAULT_CHART_RUDDER_ANGLE_DEG else ""}>{float(angle):.1f} deg</option>'
        for angle in metadata["rudder_angles_deg"]
    )
    summary_json = json.dumps(result["summary"], indent=2)
    design_data = metadata["design_data"]
    sample = result["sample_working_example"]
    input_rows_html = "\n".join(
        [
            f"<tr><th>LBP</th><td>{design_data['lbp_m']:.3f} m</td></tr>",
            f"<tr><th>Beam</th><td>{design_data['beam_m']:.3f} m</td></tr>",
            f"<tr><th>Draft</th><td>{design_data['draft_m']:.3f} m</td></tr>",
            f"<tr><th>Current frontal projected area</th><td>{design_data['beam_m'] * design_data['draft_m']:.3f} m²</td></tr>",
            f"<tr><th>Current lateral projected area</th><td>{design_data['lbp_m'] * design_data['draft_m']:.3f} m²</td></tr>",
            f"<tr><th>Yaw lever</th><td>{design_data['yaw_lever_m']:.3f} m</td></tr>",
            f"<tr><th>Rudder area</th><td>{design_data['rudder_area_m2']:.6f} m²</td></tr>",
            f"<tr><th>Rudder span</th><td>{design_data['rudder_span_m']:.3f} m</td></tr>",
            f"<tr><th>Ship speed over ground</th><td>{design_data['ship_sog_kn']:.3f} kn</td></tr>",
            f"<tr><th>Water density</th><td>{design_data['rho_kg_m3']:.1f} kg/m³</td></tr>",
            f"<tr><th>Barrass/workbook β</th><td>{design_data['beta']:.1f}</td></tr>",
            f"<tr><th>Propeller rotation factor Cr</th><td>{design_data['prop_rotation_factor']:.1f}</td></tr>",
            f"<tr><th>Current speed sweep</th><td>{', '.join(f'{speed:g}' for speed in metadata['current_speeds_kn'])} kn + chart-default {metadata['chart_default_current_speed_kn']:.2f} kn</td></tr>",
            f"<tr><th>Heading/rudder grid</th><td>{min(metadata['heading_offsets_deg']):.0f}..{max(metadata['heading_offsets_deg']):.0f} deg heading × {min(metadata['rudder_angles_deg']):.0f}..{max(metadata['rudder_angles_deg']):.0f} deg rudder, 1 deg step</td></tr>",
        ]
    )
    sample_rows_html = "\n".join(
        [
            f"<li><code>V = {sample['current_speed_kn']:.2f} kn × {KNOT_TO_M_PER_S:.5f} = {sample['current_speed_m_s']:.5f} m/s</code>; displayed as <strong>4.56 kn = {sample['current_speed_m_s']:.5f} m/s</strong>.</li>",
            f"<li><code>alpha = δ - ψ = {sample['rudder_angle_deg']:.1f} deg - {sample['heading_offset_deg']:.1f} deg = {sample['effective_rudder_inflow_angle_deg']:.1f} deg</code>.</li>",
            f"<li><code>F = {sample['beta']:.1f} × {sample['rudder_area_m2']:.6f} × {sample['current_speed_m_s']:.5f}² × {sample['cr']:.1f} = {sample['base_force_N']:.3f} N</code>.</li>",
            f"<li><code>Fn = F × sin(alpha) = {sample['normal_force_N']:.3f} N</code>.</li>",
            f"<li><code>X_ship = {sample['force_x_ship_N']:.3f} N</code>, <code>Y_ship = {sample['force_y_ship_port_N']:.3f} N</code>, <code>N_ship = {sample['moment_n_yaw_bow_port_kN_m']:.6f} kN-m</code>.</li>",
            f"<li><code>q = 0.5 × rho × V²</code>; current coefficients use <code>Cx=1.05×abs(cos(psi))</code>, <code>Cy=sin(psi)</code>, <code>Cm=0.55×sin(psi)</code> as transparent placeholder heading functions.</li>",
            f"<li><code>X_current = {sample['ocimf_current_force_x_ship_N']:.3f} N</code>, <code>Y_current = {sample['ocimf_current_force_y_ship_port_N']:.3f} N</code>, <code>N_current = {sample['ocimf_current_moment_n_yaw_bow_port_kN_m']:.6f} kN-m</code>; totals are component sums at COG.</li>",
        ]
    )
    return f"""<!doctype html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>{REPORT_DISPLAY_TITLE}</title>
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
.print-section {{ break-inside:avoid; page-break-inside:avoid; }}
.print-page {{ break-after:page; page-break-after:always; }}
.chart-section {{ break-inside:avoid; page-break-inside:avoid; margin-top:18px; }}
.calc-list {{ margin:10px 0 0 20px; padding:0; }}
.calc-list li {{ margin:8px 0; }}
.schematic-card {{ border:1px solid var(--line); border-radius:10px; background:#fbfdff; padding:16px; margin:16px 0; }}
.schematic-grid {{ display:grid; grid-template-columns:minmax(320px, 1fr) minmax(240px, .62fr); gap:18px; align-items:center; }}
.schematic-svg {{ width:100%; min-height:300px; border:1px solid var(--line); border-radius:8px; background:linear-gradient(180deg,#f8fbff 0%,#eef6ff 100%); }}
.schematic-legend {{ margin:0; padding-left:20px; color:var(--muted); }}
.schematic-legend li {{ margin:8px 0; }}
.schematic-readout {{ display:grid; grid-template-columns:1fr 1fr; gap:8px; margin-top:12px; }}
.schematic-readout div {{ background:white; border:1px solid var(--line); border-radius:6px; padding:8px 10px; }}
.axis-line {{ stroke:#172033; stroke-width:2; stroke-dasharray:5 5; }}
.current-line {{ stroke:#0b6bcb; stroke-width:4; marker-end:url(#arrow-blue); }}
.rudder-line {{ stroke:#c2410c; stroke-width:4; marker-end:url(#arrow-orange); }}
.force-line {{ stroke:#15803d; stroke-width:3; marker-end:url(#arrow-green); }}
.ship-hull {{ fill:#ffffff; stroke:#172033; stroke-width:3; }}
.rudder-blade {{ fill:#fed7aa; stroke:#c2410c; stroke-width:2; }}
.svg-label {{ font:600 13px Aptos, Segoe UI, sans-serif; fill:#172033; }}
.svg-muted {{ font:12px Aptos, Segoe UI, sans-serif; fill:#607085; }}
@media print {{ @page {{ size:A4 landscape; margin:12mm; }} body {{ background:white; }} .report-shell {{ width:100%; padding:0; }} .report-page {{ border:0; padding:0; }} .print-section,.chart-section {{ break-inside:avoid; page-break-inside:avoid; }} .chart-section {{ break-after:page; page-break-after:always; }} .chart {{ break-inside:avoid; height:300px; margin-bottom:18px; }} .schematic-grid {{ grid-template-columns:1fr 1fr; }} }}
</style>
</head>
<body>
<div class=\"report-shell\"><main class=\"report-page\">
<p class=\"eyebrow\">B1528 SIROCCO · Issue #598</p>
<h1>{REPORT_DISPLAY_HEADING}</h1>
<p>This interactive report visualizes <strong>rudder-induced</strong>, <strong>OCIMF-inspired hull-current review</strong>, and summed <strong>total</strong> force/yaw-moment components for B1528 SIROCCO current-heading/rudder combinations with rudder angles limited to {min(metadata['rudder_angles_deg']):.0f} deg through +{max(metadata['rudder_angles_deg']):.0f} deg. Totals are comparison loads only; this is not a validated whole-vessel current-load, mooring, tug, bank-effect, or compliance model.</p>
<p>{REPORT_SCOPE}</p>
<div class=\"note-panel\"><strong>Default speed policy:</strong> 4.56 kn is an extra chart-default case outside the requested engineering sweep list. Rows are flagged with <code>is_chart_default_extra_speed</code>.</div>

<section id=\"input-data-section\" class=\"print-section\">
<h2>Input data</h2>
<p>Key input values used by both the interactive HTML and print/PDF report views.</p>
<table class=\"data-table\" aria-label=\"Input data for B1528 SIROCCO current-heading rudder calculation\">
<tbody>
{input_rows_html}
</tbody>
</table>
</section>

<section id=\"sample-calculation-section\" class=\"print-section print-page\">
<h2>Sample calculation</h2>
<p>Representative hand-check point: <strong>{sample['data_point']}</strong>.</p>
<ol class=\"calc-list\">
{sample_rows_html}
</ol>
</section>

<section id="heading-rudder-schematic" class="print-section schematic-card">
<h2>Ship/current/rudder geometry schematic</h2>
<p>This schematic updates with the current speed/rudder controls and uses the selected breakdown heading. It shows ship-fixed axes, the current heading offset <code>ψ</code>, rudder command <code>δ</code>, and effective rudder inflow angle <code>alpha = δ - ψ</code>. Positive bow-to-port convention applies to <code>ψ</code>, <code>δ</code>, <code>Y_ship</code>, and yaw moment.</p>
<div class="schematic-grid">
<svg id="ship-current-rudder-svg" class="schematic-svg" viewBox="0 0 620 360" role="img" aria-label="Ship current heading and rudder geometry schematic">
<defs>
<marker id="arrow-blue" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L0,6 L9,3 z" fill="#0b6bcb" /></marker>
<marker id="arrow-orange" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L0,6 L9,3 z" fill="#c2410c" /></marker>
<marker id="arrow-green" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L0,6 L9,3 z" fill="#15803d" /></marker>
</defs>
<line x1="310" y1="315" x2="310" y2="45" class="axis-line" />
<line x1="130" y1="185" x2="490" y2="185" class="axis-line" />
<text x="318" y="54" class="svg-label">+X ship / bow</text>
<text x="132" y="176" class="svg-label">+Y ship / port</text>
<g id="schematic-current-heading-line" transform="rotate(0 310 185)">
<line x1="310" y1="315" x2="310" y2="78" class="current-line" />
</g>
<g id="schematic-rudder-line" transform="rotate(0 310 268)">
<line x1="310" y1="268" x2="310" y2="318" class="rudder-line" />
</g>
<g id="schematic-total-force-line" transform="rotate(0 310 185)">
<line x1="310" y1="185" x2="250" y2="112" class="force-line" />
</g>
<path class="ship-hull" d="M310 68 C360 110 382 165 382 245 C382 285 350 306 310 316 C270 306 238 285 238 245 C238 165 260 110 310 68 Z" />
<line x1="310" y1="88" x2="310" y2="300" stroke="#172033" stroke-width="1.5" stroke-dasharray="4 4" />
<rect id="schematic-rudder-blade" x="302" y="278" width="16" height="42" rx="3" class="rudder-blade" />
<circle cx="310" cy="185" r="5" fill="#172033" />
<text x="318" y="190" class="svg-muted">COG</text>
<text id="schematic-psi-label" x="410" y="88" class="svg-label">ψ = 0°</text>
<text id="schematic-delta-label" x="330" y="333" class="svg-label">δ = 0°</text>
<text id="schematic-alpha-label" x="412" y="310" class="svg-label">alpha = 0°</text>
<text id="schematic-force-label" x="176" y="105" class="svg-label">total force resultant</text>
</svg>
<div>
<ul class="schematic-legend">
<li><strong>Black dashed axes:</strong> ship-fixed COG frame; +X forward and +Y port.</li>
<li><strong>Blue vector:</strong> current heading offset <code>ψ</code> relative to ship centerline.</li>
<li><strong>Orange vector:</strong> selected rudder command <code>δ</code>; the selector intentionally controls this angle for Chart 1/3/4 review.</li>
<li><strong>Green vector:</strong> selected-case total horizontal force direction from current + rudder components.</li>
<li><strong>Effective inflow:</strong> <code>alpha = δ - ψ</code>; when alpha is zero, rudder-induced load is zero but hull-current load remains.</li>
</ul>
<div class="schematic-readout" aria-label="Selected schematic geometry readout">
<div><strong>Speed</strong><br><span id="schematic-speed-readout">—</span></div>
<div><strong>Heading ψ</strong><br><span id="schematic-heading-readout">—</span></div>
<div><strong>Rudder δ</strong><br><span id="schematic-rudder-readout">—</span></div>
<div><strong>Effective alpha</strong><br><span id="schematic-alpha-readout">—</span></div>
</div>
</div>
</div>
</section>

<h2>Controls</h2>
<div class=\"controls\">
<label for=\"current-speed-select\">Current speed
<select id=\"current-speed-select\">{speed_options}</select>
</label>
<label for=\"rudder-angle-select\">Rudder angle for Chart 1
<select id=\"rudder-angle-select\">{rudder_options}</select>
</label>
</div>

<h2>Selected-speed envelope summary</h2>
<p>Envelope values below update with the current-speed selector and use the full heading × rudder grid at that speed. This panel is intended to keep the chart default <strong>4.56 kn</strong> visible in screenshots while preserving requested sweep row counts.</p>
<table id="selected-speed-envelope-summary" class="data-table" aria-label="Selected speed envelope summary">
<thead><tr><th>Metric</th><th>Envelope case</th><th>Value</th></tr></thead>
<tbody>
<tr><td>Selected speed</td><td id="selected-speed-case">—</td><td id="selected-speed-value">—</td></tr>
<tr><td>Max |Y_ship| at selected speed</td><td id="selected-speed-max-y-case">—</td><td id="selected-speed-max-y-value">—</td></tr>
<tr><td>Max |N_ship| at selected speed</td><td id="selected-speed-max-n-case">—</td><td id="selected-speed-max-n-value">—</td></tr>
<tr><td>Max resultant horizontal force</td><td id="selected-speed-max-h-case">—</td><td id="selected-speed-max-h-value">—</td></tr>
</tbody>
</table>

<section class=\"chart-section\">
<h2>Chart 1 — Rudder-induced ship-fixed force components over heading</h2>
<p>Shows rudder-induced <code>X_ship</code>, <code>Y_ship</code>, and resultant horizontal force in kN for the selected current speed and rudder angle. All 21 rudder angles are selectable; the default rudder angle is neutral <strong>0.0 deg</strong>.</p>
<div id=\"force-components-chart\" class=\"chart\" aria-label=\"Rudder-induced ship-fixed force component chart\"></div>
</section>

<section class=\"chart-section\">
<h2>Chart 2 — Rudder-induced ship-fixed yaw moment heatmap over heading × rudder</h2>
<p>Shows signed rudder-induced <code>N_ship yaw moment (kN-m)</code> over heading offset and rudder angle for the selected current speed.</p>
<div id=\"yaw-moment-heatmap\" class=\"chart\" aria-label=\"Rudder-induced yaw moment heatmap\"></div>
</section>

<section class=\"chart-section\">
<h2>Chart 3 — OCIMF-inspired current vs rudder vs total horizontal resultant</h2>
<p>Compares individual horizontal resultants from the first-cut OCIMF-inspired hull-current review estimate and rudder-induced component, plus the vector-summed total resultant in ship-fixed COG axes for the selected speed and rudder angle. Current coefficients are transparent placeholder heading functions, not vessel-specific OCIMF curves.</p>
<div id=\"ocimf-rudder-resultant-force-chart\" class=\"chart\" aria-label=\"OCIMF current versus rudder versus total horizontal resultant chart\"></div>
</section>

<section class=\"chart-section\">
<h2>Chart 4 — OCIMF-inspired current vs rudder vs total yaw moment about COG</h2>
<p>Compares signed yaw moment about COG from the OCIMF-inspired current-review component, the rudder-induced component, and the summed total yaw moment. Positive yaw moment is bow-to-port.</p>
<div id=\"ocimf-rudder-resultant-yaw-chart\" class=\"chart\" aria-label=\"OCIMF current versus rudder versus total yaw moment about COG chart\"></div>
</section>

<section id=\"selected-case-force-breakdown\" class=\"print-section\">
<h2>Selected-case force breakdown</h2>
<p>Updates with current speed and rudder angle. Values are shown at the heading with maximum absolute total yaw moment for the selected trace, so individual and resultant force paths can be reviewed together.</p>
<table class=\"data-table\" aria-label=\"Selected-case OCIMF current rudder resultant force breakdown\">
<thead><tr><th>Component</th><th>X ship (kN)</th><th>Y ship port (kN)</th><th>Horizontal resultant (kN)</th><th>Yaw moment about COG (kN-m)</th></tr></thead>
<tbody>
<tr><td>OCIMF current</td><td id=\"breakdown-current-x\">—</td><td id=\"breakdown-current-y\">—</td><td id=\"breakdown-current-r\">—</td><td id=\"breakdown-current-n\">—</td></tr>
<tr><td>Rudder induced</td><td id=\"breakdown-rudder-x\">—</td><td id=\"breakdown-rudder-y\">—</td><td id=\"breakdown-rudder-r\">—</td><td id=\"breakdown-rudder-n\">—</td></tr>
<tr><td>Total / mooring reaction opposite</td><td id=\"breakdown-total-x\">—</td><td id=\"breakdown-total-y\">—</td><td id=\"breakdown-total-r\">—</td><td id=\"breakdown-total-n\">—</td></tr>
</tbody>
</table>
<p id=\"breakdown-case-label\" class=\"note-panel\">—</p>
</section>

<h2>Method and provenance</h2>
<pre class=\"method-block\">{metadata['method']}</pre>
<ul>
<li>Heading/rudder effective-angle convention: alpha = rudder_angle_deg - heading_offset_deg.</li>
<li>Local-to-ship transform rotates local current-frame X/Y loads into ship-fixed COG axes.</li>
<li>Current review equations: q=0.5*rho*V²; Cx=1.05*abs(cos(psi)); Cy=sin(psi); Cm=0.55*sin(psi); X_current=q*(beam*draft)*Cx; Y_current=q*(LBP*draft)*Cy; N_current=q*(LBP*draft)*LBP*Cm.</li>
<li>{ZERO_EFFECTIVE_ANGLE_NOTE}</li>
<li>Scope note: hull current loads are first-cut OCIMF-inspired comparison loads using placeholder heading functions only; this is not a validated whole-vessel current-load or oblique-current hull/rudder interaction model.</li>
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
function absMax(rows, key) {{ return rows.reduce((best, row) => Math.abs(row[key]) > Math.abs(best[key]) ? row : best, rows[0]); }}
function maxBy(rows, key) {{ return rows.reduce((best, row) => row[key] > best[key] ? row : best, rows[0]); }}
function caseLabel(row) {{ return `heading ${{row.heading_offset_deg}} deg · rudder ${{row.rudder_angle_deg}} deg`; }}
function updateText(id, text) {{ document.getElementById(id).textContent = text; }}
function updateSelectedSpeedEnvelope() {{
  const speed = selectedSpeed();
  const speedRows = rowsForSpeed(speed);
  const maxY = absMax(speedRows, 'force_y_ship_port_N');
  const maxN = absMax(speedRows, 'moment_n_yaw_bow_port_kN_m');
  const maxH = maxBy(speedRows, 'resultant_horizontal_force_N');
  const extra = speedRows.some(row => row.is_chart_default_extra_speed) ? 'chart-default extra plane' : 'requested engineering sweep plane';
  updateText('selected-speed-case', extra);
  updateText('selected-speed-value', `${{speed}} kn · ${{speedRows.length}} rows`);
  updateText('selected-speed-max-y-case', caseLabel(maxY));
  updateText('selected-speed-max-y-value', `${{(maxY.force_y_ship_port_N/1000).toFixed(3)}} kN`);
  updateText('selected-speed-max-n-case', caseLabel(maxN));
  updateText('selected-speed-max-n-value', `${{maxN.moment_n_yaw_bow_port_kN_m.toFixed(3)}} kN-m`);
  updateText('selected-speed-max-h-case', caseLabel(maxH));
  updateText('selected-speed-max-h-value', `${{(maxH.resultant_horizontal_force_N/1000).toFixed(3)}} kN`);
}}
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
  Plotly.newPlot('force-components-chart', traces, {{title:`Rudder-induced ship-fixed force component (kN) · current ${{speed}} kn · rudder ${{rudder}} deg`, xaxis:{{title:'Heading offset ψ (deg)', zeroline:true, gridcolor:'#e5eaf1'}}, yaxis:{{title:'Rudder-induced ship-fixed force component (kN)', zeroline:true, gridcolor:'#e5eaf1'}}, height:STANDARD_CHART_HEIGHT, margin:{{l:76,r:30,t:66,b:64}}, legend:{{orientation:'h', y:-0.22}}, hovermode:'x unified', template:'plotly_white'}}, CHART_CONFIG);
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
  const trace = {{type:'heatmap', x:headings, y:rudders, z, colorscale:'RdBu', reversescale:true, zmid:0, colorbar:{{title:'Rudder-induced N_ship yaw moment (kN-m)'}}, hovertemplate:'heading=%{{x}} deg<br>rudder=%{{y}} deg<br>Rudder-induced N_ship yaw moment=%{{z:.3f}} kN-m<extra></extra>'}};
  Plotly.newPlot('yaw-moment-heatmap', [trace], {{title:`Rudder-induced N_ship yaw moment (kN-m) · current ${{speed}} kn`, xaxis:{{title:'Heading offset ψ (deg)', gridcolor:'#e5eaf1'}}, yaxis:{{title:'Rudder angle δ (deg)', gridcolor:'#e5eaf1'}}, height:STANDARD_CHART_HEIGHT, margin:{{l:76,r:132,t:66,b:64}}, template:'plotly_white'}}, CHART_CONFIG);
}}
function selectedTraceRows() {{
  const speed = selectedSpeed();
  const rudder = selectedRudder();
  return ROWS.filter(row => same(row.current_speed_kn, speed) && same(row.rudder_angle_deg, rudder)).sort(byHeading);
}}
function updateResultantForceChart() {{
  const speed = selectedSpeed();
  const rudder = selectedRudder();
  const rows = selectedTraceRows();
  const x = rows.map(row => row.heading_offset_deg);
  const hover = rows.map(row => `Xc=${{(row.ocimf_current_force_x_ship_N/1000).toFixed(3)}} kN<br>Yc=${{(row.ocimf_current_force_y_ship_port_N/1000).toFixed(3)}} kN<br>Xr=${{(row.force_x_ship_N/1000).toFixed(3)}} kN<br>Yr=${{(row.force_y_ship_port_N/1000).toFixed(3)}} kN<br>Xt=${{(row.total_force_x_ship_N/1000).toFixed(3)}} kN<br>Yt=${{(row.total_force_y_ship_port_N/1000).toFixed(3)}} kN`);
  const traces = [
    {{x, y: rows.map(row => row.ocimf_current_resultant_horizontal_force_N/1000), name:'OCIMF current resultant (kN)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>OCIMF current resultant=%{{y:.3f}} kN<br>%{{customdata}}<extra></extra>'}},
    {{x, y: rows.map(row => row.resultant_horizontal_force_N/1000), name:'Rudder resultant (kN)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>Rudder resultant=%{{y:.3f}} kN<br>%{{customdata}}<extra></extra>'}},
    {{x, y: rows.map(row => row.total_resultant_horizontal_force_N/1000), name:'Total resultant (kN)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>Total resultant=%{{y:.3f}} kN<br>%{{customdata}}<extra></extra>'}}
  ];
  Plotly.newPlot('ocimf-rudder-resultant-force-chart', traces, {{title:`OCIMF current vs rudder vs total horizontal resultant (kN) · current ${{speed}} kn · rudder ${{rudder}} deg`, xaxis:{{title:'Heading offset ψ (deg)', zeroline:true, gridcolor:'#e5eaf1'}}, yaxis:{{title:'Horizontal resultant force (kN)', rangemode:'tozero', gridcolor:'#e5eaf1'}}, height:STANDARD_CHART_HEIGHT, margin:{{l:76,r:30,t:66,b:64}}, legend:{{orientation:'h', y:-0.22}}, hovermode:'x unified', template:'plotly_white'}}, CHART_CONFIG);
}}
function updateResultantYawChart() {{
  const speed = selectedSpeed();
  const rudder = selectedRudder();
  const rows = selectedTraceRows();
  const x = rows.map(row => row.heading_offset_deg);
  const hover = rows.map(row => `Nc=${{row.ocimf_current_moment_n_yaw_bow_port_kN_m.toFixed(3)}} kN-m<br>Nr=${{row.moment_n_yaw_bow_port_kN_m.toFixed(3)}} kN-m<br>Nt=${{row.total_moment_n_yaw_bow_port_kN_m.toFixed(3)}} kN-m`);
  const traces = [
    {{x, y: rows.map(row => row.ocimf_current_moment_n_yaw_bow_port_kN_m), name:'OCIMF current N (kN-m)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>OCIMF current yaw=%{{y:.3f}} kN-m<br>%{{customdata}}<extra></extra>'}},
    {{x, y: rows.map(row => row.moment_n_yaw_bow_port_kN_m), name:'Rudder N (kN-m)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>Rudder yaw=%{{y:.3f}} kN-m<br>%{{customdata}}<extra></extra>'}},
    {{x, y: rows.map(row => row.total_moment_n_yaw_bow_port_kN_m), name:'Total N (kN-m)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>Total yaw=%{{y:.3f}} kN-m<br>%{{customdata}}<extra></extra>'}}
  ];
  Plotly.newPlot('ocimf-rudder-resultant-yaw-chart', traces, {{title:`OCIMF current vs rudder vs total yaw moment about COG (kN-m) · current ${{speed}} kn · rudder ${{rudder}} deg`, xaxis:{{title:'Heading offset ψ (deg)', zeroline:true, gridcolor:'#e5eaf1'}}, yaxis:{{title:'Yaw moment about COG (kN-m)', zeroline:true, gridcolor:'#e5eaf1'}}, height:STANDARD_CHART_HEIGHT, margin:{{l:82,r:30,t:66,b:64}}, legend:{{orientation:'h', y:-0.22}}, hovermode:'x unified', template:'plotly_white'}}, CHART_CONFIG);
}}
function fmtKn(value) {{ return `${{(value/1000).toFixed(3)}}`; }}
function fmtMoment(value) {{ return `${{value.toFixed(3)}}`; }}
function setSvgRotation(id, angleDeg, cx, cy) {{ document.getElementById(id).setAttribute('transform', `rotate(${{angleDeg}} ${{cx}} ${{cy}})`); }}
function updateHeadingRudderSchematic(row) {{
  const psi = row.heading_offset_deg;
  const delta = row.rudder_angle_deg;
  const alpha = row.effective_rudder_inflow_angle_deg;
  const totalForceAngle = Math.atan2(row.total_force_y_ship_port_N, row.total_force_x_ship_N) * 180 / Math.PI;
  setSvgRotation('schematic-current-heading-line', -psi, 310, 185);
  setSvgRotation('schematic-rudder-line', -delta, 310, 268);
  setSvgRotation('schematic-total-force-line', -totalForceAngle + 50, 310, 185);
  document.getElementById('schematic-rudder-blade').setAttribute('transform', `rotate(${{-delta}} 310 299)`);
  updateText('schematic-psi-label', `ψ = ${{psi}}°`);
  updateText('schematic-delta-label', `δ = ${{delta}}°`);
  updateText('schematic-alpha-label', `alpha = ${{alpha}}°`);
  updateText('schematic-speed-readout', `${{row.current_speed_kn}} kn`);
  updateText('schematic-heading-readout', `${{psi}} deg`);
  updateText('schematic-rudder-readout', `${{delta}} deg`);
  updateText('schematic-alpha-readout', `${{alpha}} deg`);
}}
function updateSelectedCaseBreakdown() {{
  const rows = selectedTraceRows();
  const selected = absMax(rows, 'total_moment_n_yaw_bow_port_kN_m');
  updateText('breakdown-current-x', fmtKn(selected.ocimf_current_force_x_ship_N));
  updateText('breakdown-current-y', fmtKn(selected.ocimf_current_force_y_ship_port_N));
  updateText('breakdown-current-r', fmtKn(selected.ocimf_current_resultant_horizontal_force_N));
  updateText('breakdown-current-n', fmtMoment(selected.ocimf_current_moment_n_yaw_bow_port_kN_m));
  updateText('breakdown-rudder-x', fmtKn(selected.force_x_ship_N));
  updateText('breakdown-rudder-y', fmtKn(selected.force_y_ship_port_N));
  updateText('breakdown-rudder-r', fmtKn(selected.resultant_horizontal_force_N));
  updateText('breakdown-rudder-n', fmtMoment(selected.moment_n_yaw_bow_port_kN_m));
  updateText('breakdown-total-x', fmtKn(selected.total_force_x_ship_N));
  updateText('breakdown-total-y', fmtKn(selected.total_force_y_ship_port_N));
  updateText('breakdown-total-r', fmtKn(selected.total_resultant_horizontal_force_N));
  updateText('breakdown-total-n', fmtMoment(selected.total_moment_n_yaw_bow_port_kN_m));
  updateHeadingRudderSchematic(selected);
  updateText('breakdown-case-label', `Selected breakdown case: current ${{selected.current_speed_kn}} kn · ${{caseLabel(selected)}} · total yaw reaction for mooring context = ${{(-selected.total_moment_n_yaw_bow_port_kN_m).toFixed(3)}} kN-m.`);
}}
function updateCharts() {{ updateSelectedSpeedEnvelope(); updateForceChart(); updateYawHeatmap(); updateResultantForceChart(); updateResultantYawChart(); updateSelectedCaseBreakdown(); }}
document.getElementById('current-speed-select').addEventListener('change', updateCharts);
document.getElementById('rudder-angle-select').addEventListener('change', () => {{ updateForceChart(); updateResultantForceChart(); updateResultantYawChart(); updateSelectedCaseBreakdown(); }});
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
