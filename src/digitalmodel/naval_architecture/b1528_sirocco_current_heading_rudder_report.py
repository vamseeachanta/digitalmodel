# ABOUTME: B1528 SIROCCO current-heading/rudder force-component sweep report.
# ABOUTME: Generates ship-fixed force/moment tables plus interactive dropdown charts.
"""B1528/SIROCCO current-heading/rudder force component report utilities.

This module extends the existing B1528 moored-current rudder-only calculation to
an approved visualization sweep over current speed, heading offset, and rudder
angle. It reports rudder-induced loads and generic/reference OCIMF hull-current
review components in a ship-fixed COG frame. It is not a validated oblique-current
hull/rudder/MMG model and deliberately excludes mooring-line stiffness, tug loads,
bank effects, and class compliance conclusions.
"""

from __future__ import annotations

import csv
import json
import math
import os
from dataclasses import asdict, dataclass
from functools import lru_cache
from importlib import resources
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.citations.schema import Citation, validate_citation
from digitalmodel.naval_architecture.b1528_sirocco_yaw_report import (
    GITHUB_REPO_BLOB,
    KNOT_TO_M_PER_S,
    NON_ROTATING_PROPELLER_CR,
    PROPELLER_ROTATION_FACTOR_NOTE,
)

REPORT_REVIEW_TARGET_DATE = "2026-05-09"
DEFAULT_CHART_RUDDER_ANGLE_DEG = 28.0
REPORT_ARTIFACT_STEM = "b1528_sirocco_current_rudder_force"
REPORT_DISPLAY_TITLE = "B1528 SIROCCO Current/Rudder Force Review — Issue #2760"
REPORT_DISPLAY_HEADING = "B1528 SIROCCO current/rudder force review — Issue #2760"
REPORT_SCOPE = (
    "SIROCCO issue #2760 current/rudder force-component review at COG; "
    "rudder-induced ship-fixed X/Y/N components are derived by rotating local "
    "current-frame loads by heading offset, and generic/reference OCIMF tanker-current "
    "review loads are estimated separately from the licensed off-repo workbook route; "
    "bank effect, tug loads, mooring-line stiffness, current-profile variation, "
    "propeller race, and class compliance conclusion are excluded"
)
ZERO_EFFECTIVE_ANGLE_NOTE = (
    "When rudder_angle_deg equals heading_offset_deg, alpha is zero and this "
    "rudder-induced component is zero; that is not total hull current load."
)
OCIMF_WORKBOOK_PATH_ENV = "OCIMF_WORKBOOK_PATH"
OCIMF_WORKBOOK_SOURCE_ID = "licensed-off-repo-ocimf-workbook"
OCIMF_PROVENANCE_README = "docs/data/OCIMF_CORPUS_README.md"


def _repo_root() -> Path:
    return Path(__file__).resolve().parents[3]


def _resolve_ocimf_workbook_path() -> Path:
    raw_path = os.environ.get(OCIMF_WORKBOOK_PATH_ENV)
    if not raw_path:
        raise FileNotFoundError(
            f"Set {OCIMF_WORKBOOK_PATH_ENV} to the licensed OCIMF coefficient workbook before "
            "running the #2760 current/rudder calculation. The workbook is intentionally "
            "kept off-repo and is not emitted in report artifacts."
        )
    return Path(raw_path).expanduser()


def _resolve_ocimf_provenance_readme() -> Path:
    return _repo_root() / OCIMF_PROVENANCE_README


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
    water_depth_m: float
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


def issue_2760_source_preflight() -> dict[str, Any]:
    """Fail-closed source/citation preflight for approved issue #2760 implementation."""

    workbook_path = _resolve_ocimf_workbook_path()
    provenance_readme = _resolve_ocimf_provenance_readme()
    if not workbook_path.exists():
        raise FileNotFoundError(
            f"Required licensed OCIMF workbook from {OCIMF_WORKBOOK_PATH_ENV} is missing"
        )
    if not provenance_readme.exists():
        raise FileNotFoundError(f"Required OCIMF provenance README is missing: {OCIMF_PROVENANCE_README}")

    ocimf_citation = Citation(
        code_id="OCIMF-MEG4",
        publisher="Oil Companies International Marine Forum",
        revision="4th edition, 2018",
        section="Annex A current coefficient figures A9/A10/A11",
        wiki_path="wikis/marine-engineering/wiki/standards/ocimf-meg4.md",
        note="Generic/reference tanker-current coefficient basis for #2760; not SIROCCO-specific.",
    )
    rudder_citation = Citation(
        code_id="B1528-SIROCCO-RUDDER-YAW-INPUTS",
        publisher="ACMA project source pack",
        revision="2026-05-01",
        section="Rudder force and yaw-moment workbook inputs",
        wiki_path="wikis/acma-projects/wiki/concepts/b1528-sirocco-rudder-yaw-moment-inputs.md",
        note="Project-specific B1528 rudder geometry and normal-force constant used for screening calculation.",
    )
    validate_citation(ocimf_citation)
    validate_citation(rudder_citation)

    return {
        "ocimf": {
            "workbook_source_id": OCIMF_WORKBOOK_SOURCE_ID,
            "provenance_readme": OCIMF_PROVENANCE_README,
            "citation": ocimf_citation,
            "license_boundary": "pointer-only-no-coefficient-corpus",
            "basis": "generic/reference tanker-current coefficient route for screening review",
        },
        "rudder": {
            "citation": rudder_citation,
            "basis": "normal-force rudder model, rpm=0, Cr=1.0",
        },
    }


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
        water_depth_m=_positive("water_depth_m", payload["environment"].get("water_depth_m", 100.0)),
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
    if cfg.current_speeds_kn != (0.0, 1.0, 2.0, 3.0, 3.08, 4.0):
        raise ValueError("current_speeds_kn must match the approved issue #2760 0..4 kn sweep including 3.08 kn")
    expected_heading_angles = tuple(float(value) for value in range(-5, 6))
    if cfg.heading_offsets_deg != expected_heading_angles:
        raise ValueError("heading_offsets_deg must be -5..+5 deg in 1 deg steps")
    expected_rudder_angles = tuple(float(value) for value in range(0, 29, 2))
    if cfg.rudder_angles_deg != expected_rudder_angles:
        raise ValueError("rudder_angles_deg must be 0..28 deg port in 2 deg steps")
    if cfg.chart_default_current_speed_kn != 3.08:
        raise ValueError("chart default current speed must be exact 3.08 kn")
    if cfg.prop_rotation_factor != NON_ROTATING_PROPELLER_CR:
        raise ValueError("current-heading/rudder report requires neutral Cr=1.0")
    return cfg


def speed_planes_for_calculation(cfg: B1528CurrentHeadingRudderConfig) -> tuple[float, ...]:
    """Return engineering speeds plus exact chart-default speed if absent."""

    speeds = list(cfg.current_speeds_kn)
    if cfg.chart_default_current_speed_kn not in speeds:
        speeds.append(cfg.chart_default_current_speed_kn)
    return tuple(sorted(speeds))


def select_ocimf_loaded_tanker_current_basis(cfg: B1528CurrentHeadingRudderConfig) -> dict[str, Any]:
    """Document the geometry-driven OCIMF tanker-current basis for #2760.

    MEG Annex A publishes loaded-tanker current curves by water-depth/draft
    bucket. For the approved #2760 screening use, B1528 is off-class and the
    deepest available loaded-tanker bucket is used when the configured WD/T
    exceeds that domain.
    """

    wd_over_t = cfg.water_depth_m / cfg.draft_m
    if wd_over_t <= 0.0:
        raise ValueError("water_depth_m / draft_m must be positive for OCIMF basis selection")
    if wd_over_t > 6.0:
        selected_bucket = ">6"
        selected_figures = ["A9", "A10", "A11"]
        rejected = ["A5-A8 shallower loaded-tanker Cxc buckets", "A12-A14 ballast 40%T tanker curves"]
    else:
        raise ValueError(
            "#2760 requires a documented loaded-tanker current curve basis; "
            f"configured WD/T={wd_over_t:.3f} is outside the approved >6 screening bucket"
        )
    return {
        "selection_rule": "max_available_wd_over_t_for_loaded_tanker_current",
        "wd_over_t": wd_over_t,
        "water_depth_m": cfg.water_depth_m,
        "draft_m": cfg.draft_m,
        "selected_wd_over_t_bucket": selected_bucket,
        "selected_figures": selected_figures,
        "selected_curve_family": "loaded_tanker_current",
        "rejected_alternatives": rejected,
        "limitation": (
            "B1528 SIROCCO is not an OCIMF tanker-class vessel; loaded-tanker "
            "curves are used off-class as a generic/reference screening basis only."
        ),
    }


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
    citation_path = out / f"{REPORT_ARTIFACT_STEM}_citations.json"
    md_path = out / f"{REPORT_ARTIFACT_STEM}_report.md"
    docx_path = out / f"{REPORT_ARTIFACT_STEM}_report.docx"
    html_path = out / f"{REPORT_ARTIFACT_STEM}_report.html"
    pdf_path = out / f"{REPORT_ARTIFACT_STEM}_report.pdf"
    manifest_path = out / f"{REPORT_ARTIFACT_STEM}_manifest.json"

    headers = list(result["rows"][0])
    with csv_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=headers, lineterminator="\n")
        writer.writeheader()
        writer.writerows(result["rows"])
    json_path.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    provenance_path.write_text(json.dumps(_provenance(result), indent=2) + "\n", encoding="utf-8")
    citation_path.write_text(json.dumps(_citation_sidecar(result), indent=2) + "\n", encoding="utf-8")
    md_path.write_text(_markdown_report(result) + "\n", encoding="utf-8")
    _write_docx_report(result, docx_path)
    html_path.write_text(_html_report(result), encoding="utf-8")
    _write_pdf_from_html(html_path, pdf_path)
    manifest = {
        "csv": str(csv_path),
        "json": str(json_path),
        "provenance": str(provenance_path),
        "citation_sidecar": str(citation_path),
        "markdown_report": str(md_path),
        "docx_report": str(docx_path),
        "html_report": str(html_path),
        "pdf_report": str(pdf_path),
        "manifest": str(manifest_path),
    }
    manifest_path.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    return manifest


def _write_docx_report(result: dict[str, Any], docx_path: Path) -> None:
    """Write a Word report package with the same core sections as Markdown/HTML."""

    try:
        from docx import Document
    except ImportError as exc:  # pragma: no cover - dependency availability is environment-specific
        raise RuntimeError("python-docx is required to render the B1528 SIROCCO Word report") from exc

    metadata = result["metadata"]
    sample = result["sample_working_example"]
    document = Document()
    document.add_heading(REPORT_DISPLAY_TITLE, level=0)
    document.add_paragraph(f"Prepared for engineer review on {metadata['review_target_date']}.")

    document.add_heading("Scope", level=1)
    document.add_paragraph(metadata["scope"])
    document.add_paragraph(
        "This package supersedes the prior B1528 SIROCCO moored-current report basis for issue #2760. "
        "It reports current-review, rudder-induced, and combined X/Y/N components about the ship-fixed COG; "
        "it is not a class-compliance or validated whole-vessel maneuvering model."
    )

    document.add_heading("Design data", level=1)
    table = document.add_table(rows=1, cols=2)
    table.rows[0].cells[0].text = "Parameter"
    table.rows[0].cells[1].text = "Value"
    for label, value in _design_rows_plain(metadata["design_data"]):
        cells = table.add_row().cells
        cells[0].text = label
        cells[1].text = value

    document.add_heading("Analysis methodology and assumptions", level=1)
    document.add_paragraph(metadata["method"])
    for assumption in metadata["analysis_assumptions"]:
        document.add_paragraph(assumption, style="List Bullet")

    document.add_heading("Heading/rudder schematic", level=1)
    document.add_paragraph(
        "Plan-view convention: +X is forward, +Y is port, current heading ψ is positive bow-to-port, "
        "rudder command δ is positive to port, and effective rudder inflow angle is α = δ - ψ. "
        "The HTML report contains the interactive SVG schematic with stable IDs."
    )

    document.add_heading("Sample working example", level=1)
    for label, value in [
        ("Data point", sample["data_point"]),
        ("Current speed", f"{sample['current_speed_kn']:.2f} kn = {sample['current_speed_m_s']:.5f} m/s"),
        ("Base force", f"{sample['base_force_N']:.3f} N"),
        ("Rudder-induced components", f"X={sample['force_x_ship_N']:.3f} N, Y={sample['force_y_ship_port_N']:.3f} N, N={sample['moment_n_yaw_bow_port_kN_m']:.6f} kN-m"),
        ("Current-review components", f"X={sample['ocimf_current_force_x_ship_N']:.3f} N, Y={sample['ocimf_current_force_y_ship_port_N']:.3f} N, N={sample['ocimf_current_moment_n_yaw_bow_port_kN_m']:.6f} kN-m"),
        ("Combined COG components", f"X={sample['total_force_x_ship_N']:.3f} N, Y={sample['total_force_y_ship_port_N']:.3f} N, N={sample['total_moment_n_yaw_bow_port_kN_m']:.6f} kN-m"),
    ]:
        document.add_paragraph(f"{label}: {value}", style="List Bullet")

    document.add_heading("OCIMF current vs rudder component sums", level=1)
    document.add_paragraph(
        "The CSV/JSON data expose separate ocimf_current_*, rudder force_*/moment_*, and total_* fields. "
        "Yaw moment is about the COG with positive bow-to-port convention. The OCIMF direct yaw moment "
        "and Y × arm check are side-by-side review quantities, not an equality criterion."
    )

    document.add_heading("Limitations", level=1)
    for limitation in metadata["limitations"]:
        document.add_paragraph(limitation, style="List Bullet")

    document.save(str(docx_path))


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


def _load_ocimf_workbook_basis() -> dict[str, list[tuple[float, float]]]:
    """Load the minimal #2760 OCIMF coefficient curves from the licensed workbook."""

    issue_2760_source_preflight()
    workbook_path = _resolve_ocimf_workbook_path()
    return _load_ocimf_workbook_basis_for_path(str(workbook_path), workbook_path.stat().st_mtime_ns)


@lru_cache(maxsize=4)
def _load_ocimf_workbook_basis_for_path(
    workbook_path_text: str, workbook_mtime_ns: int
) -> dict[str, list[tuple[float, float]]]:
    """Read report-specific OCIMF curves; cache is keyed by path and mtime."""

    del workbook_mtime_ns  # value participates in the cache key.
    workbook_path = Path(workbook_path_text)
    try:
        import openpyxl
    except ImportError as exc:  # pragma: no cover - dependency availability is environment-specific
        raise RuntimeError("openpyxl is required to resolve OCIMF workbook coefficients") from exc

    wb = openpyxl.load_workbook(workbook_path, data_only=True, read_only=True)
    required_sheets = {"Data 5a-9a", "Data 10a-14a"}
    missing_sheets = sorted(required_sheets.difference(wb.sheetnames))
    if missing_sheets:
        raise ValueError(f"OCIMF workbook missing required #2760 sheets: {missing_sheets}")
    sheet_a5_a9 = wb["Data 5a-9a"]
    sheet_a10_a14 = wb["Data 10a-14a"]

    def points(ws: Any, angle_col: int, value_col: int, start_row: int = 3) -> list[tuple[float, float]]:
        curve: list[tuple[float, float]] = []
        for row_idx in range(start_row, ws.max_row + 1):
            angle = ws.cell(row_idx, angle_col).value
            value = ws.cell(row_idx, value_col).value
            if isinstance(angle, (int, float)) and isinstance(value, (int, float)):
                curve.append((float(angle), float(value)))
        if len(curve) < 2:
            raise ValueError(f"OCIMF workbook curve at columns {angle_col}/{value_col} has insufficient numeric data")
        return sorted(curve)

    return {
        # Figure A9: loaded tanker longitudinal current Cxc, WD/T >4.4, conventional bow.
        "cxc": points(sheet_a5_a9, 23, 24),
        # Figure A10: loaded tanker lateral current Cyc, WD/T >6.
        "cyc": points(sheet_a10_a14, 1, 7),
        # Figure A11: loaded tanker yaw moment Cxyc, WD/T >6.
        "cxyc": points(sheet_a10_a14, 9, 15),
    }


def _interp_curve(curve: list[tuple[float, float]], x: float) -> float:
    """Linearly interpolate a numeric OCIMF curve."""

    if x <= curve[0][0]:
        return curve[0][1]
    if x >= curve[-1][0]:
        return curve[-1][1]
    for (x0, y0), (x1, y1) in zip(curve, curve[1:]):
        if x0 <= x <= x1:
            if x1 == x0:
                return y0
            frac = (x - x0) / (x1 - x0)
            return y0 + frac * (y1 - y0)
    raise ValueError(f"interpolation value {x} outside OCIMF curve domain")


def resolve_ocimf_loaded_tanker_current_coefficients(
    cfg: B1528CurrentHeadingRudderConfig,
    heading_offset_deg: float,
) -> dict[str, Any]:
    """Resolve generic/reference OCIMF loaded-tanker current coefficients.

    Issue #2760 defines heading as current off bow, port positive. The OCIMF
    workbook curves used here are tabulated on a 0..180 degree heading basis,
    with head-current cases near 180 degrees, so ±5 degrees off bow maps to 175.
    The workbook publishes Cyc as positive magnitude and Cxyc as signed values
    that encode yaw-moment direction across heading; the per-call sign factor
    only mirrors port vs. starboard heading and MUST preserve the workbook's
    own Cxyc sign (otherwise yaw direction inverts in the Cxyc<0 regime — see
    Annex A figure A11 below ~98° and above ~180° where Cxyc is negative).
    """

    preflight = issue_2760_source_preflight()
    basis_selection = select_ocimf_loaded_tanker_current_basis(cfg)
    curves = _load_ocimf_workbook_basis()
    heading = float(heading_offset_deg)
    sign = 1.0 if heading >= 0.0 else -1.0
    table_angle = 180.0 - abs(heading)
    cxc = _interp_curve(curves["cxc"], table_angle)
    if abs(heading) < 1e-12:
        cyc = 0.0
        cxyc = 0.0
    else:
        cyc = sign * _interp_curve(curves["cyc"], table_angle)
        cxyc = sign * _interp_curve(curves["cxyc"], table_angle)
    return {
        "angle_table_deg": table_angle,
        "cxc": cxc,
        "cyc": cyc,
        "cxyc": cxyc,
        "source_workbook": preflight["ocimf"]["workbook_source_id"],
        "source_code_id": preflight["ocimf"]["citation"].code_id,
        "basis": "OCIMF loaded tanker current curves: A9 Cxc WD/T>4.4 conventional, A10/A11 WD/T>6",
        "basis_selection": basis_selection,
    }


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
    dynamic_pressure_Pa = 0.5 * cfg.rho_kg_m3 * speed_m_s**2
    frontal_area_current_m2 = cfg.beam_m * cfg.draft_m
    lateral_area_current_m2 = cfg.lbp_m * cfg.draft_m
    ocimf_coefficients = resolve_ocimf_loaded_tanker_current_coefficients(
        cfg, heading_offset_deg=heading_offset_deg
    )
    ocimf_cx = ocimf_coefficients["cxc"]
    ocimf_cy = ocimf_coefficients["cyc"]
    ocimf_cm = ocimf_coefficients["cxyc"]
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
        "total_force_x_ship_N": total_x_N,
        "total_force_y_ship_port_N": total_y_N,
        "total_moment_n_yaw_bow_port_Nm": total_n_Nm,
        "total_moment_n_yaw_bow_port_kN_m": total_n_Nm / 1000.0,
        "mooring_reaction_x_N": -total_x_N,
        "mooring_reaction_y_N": -total_y_N,
        "mooring_reaction_n_Nm": -total_n_Nm,
        "force_component_basis": "issue #2760 rudder induced plus generic/reference OCIMF tanker-current review comparison; components are summed in ship-fixed COG X/Y/N",
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
            "water_depth_m": cfg.water_depth_m,
            "water_depth_to_draft_ratio": cfg.water_depth_m / cfg.draft_m,
            "yaw_lever_m": cfg.yaw_lever_m,
            "rudder_area_m2": cfg.rudder_area_m2,
            "rudder_span_m": cfg.rudder_span_m,
            "ship_sog_kn": cfg.ship_sog_kn,
            "rho_kg_m3": cfg.rho_kg_m3,
            "beta": cfg.beta,
            "prop_rotation_factor": cfg.prop_rotation_factor,
            "force_convention": cfg.force_convention,
            "ocimf_current_cx_basis": "interpolated OCIMF loaded-tanker A9 longitudinal-current curve from approved workbook route",
            "ocimf_current_cm_basis": "interpolated OCIMF loaded-tanker A11 yaw-moment curve from approved workbook route",
            "ocimf_basis_selection": select_ocimf_loaded_tanker_current_basis(cfg),
        },
        "analysis_assumptions": [
            "Vessel is moored with ship speed over ground equal to 0 kn.",
            "Heading/rudder effective-angle convention: α = rudder_angle_deg - heading_offset_deg.",
            "Positive heading rotates the local downstream current-force axis toward port from +X_ship.",
            "Local-to-ship transform rotates local current-frame X/Y loads into ship-fixed COG axes.",
            cfg.default_speed_policy,
            "OCIMF reference hull current loads are first-cut review loads using projected beam*draft and LBP*draft areas with loaded-tanker current coefficients interpolated from the approved OCIMF workbook route; they are not vessel-specific OCIMF/current-coefficient curves or certified coefficients.",
            "Mooring reactions are equal and opposite loads for static-equilibrium context only.",
        ],
        "limitations": list(cfg.limitations),
        "traceability_links": {
            "source_pack_issue": cfg.source_pack_issue,
            "current_heading_rudder_issue": cfg.report_issue,
            "plan": f"{GITHUB_REPO_BLOB}/{cfg.plan_path}",
            "durable_report": f"{GITHUB_REPO_BLOB}/docs/domains/marine-engineering/b1528-sirocco-current-rudder-force-report.md",
            "generated_markdown_report": f"{GITHUB_REPO_BLOB}/outputs/b1528_sirocco/current_rudder_force/{REPORT_ARTIFACT_STEM}_report.md",
            "generated_html_report": f"{GITHUB_REPO_BLOB}/outputs/b1528_sirocco/current_rudder_force/{REPORT_ARTIFACT_STEM}_report.html",
            "packaged_input_yaml": f"{GITHUB_REPO_BLOB}/src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml",
            "report_generator": f"{GITHUB_REPO_BLOB}/src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py",
            "moored_current_reference": f"{GITHUB_REPO_BLOB}/src/digitalmodel/naval_architecture/b1528_sirocco_moored_current_report.py",
        },
        "prop_rotation_factor_note": PROPELLER_ROTATION_FACTOR_NOTE,
    }


def _summary(rows: list[dict[str, Any]]) -> dict[str, Any]:
    max_y = max(rows, key=lambda row: abs(row["force_y_ship_port_N"]))
    max_n = max(rows, key=lambda row: abs(row["moment_n_yaw_bow_port_Nm"]))
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
        "max_abs_total_yaw_moment_kN_m": abs(max_total_n["total_moment_n_yaw_bow_port_kN_m"]),
        "max_abs_total_yaw_signed_moment_kN_m": max_total_n["total_moment_n_yaw_bow_port_kN_m"],
        "max_abs_total_yaw_case": _case_label(max_total_n),
    }


def _sample_working_example(result: dict[str, Any]) -> dict[str, Any]:
    row = next(
        item
        for item in result["rows"]
        if item["current_speed_kn"] == 3.08
        and item["heading_offset_deg"] == 5.0
        and item["rudder_angle_deg"] == 28.0
    )
    return {
        "data_point": "issue #2760 default 3.08 kn, heading +5 deg, rudder +28 deg, Cr=1.0",
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
    }


def _provenance(result: dict[str, Any]) -> dict[str, Any]:
    metadata = result["metadata"]
    return {
        "scope": metadata["scope"],
        "method": metadata["method"],
        "heading_rudder_effective_angle_convention": "α = rudder_angle_deg - heading_offset_deg",
        "local_to_ship_transform": {
            "x_ship": "X_local*cos(psi)-Y_local*sin(psi)",
            "y_ship": "X_local*sin(psi)+Y_local*cos(psi)",
            "n_ship": "Y_ship*yaw_lever_m",
        },
        "ocimf_first_cut_current_loads": {
            "dynamic_pressure": "q=0.5*rho*V^2",
            "surge_force": "X_current=q*(beam*draft)*Cxc_interp",
            "sway_force": "Y_current=q*(LBP*draft)*Cyc_interp_signed",
            "yaw_moment_about_cog": "N_current=q*(LBP*draft)*LBP*Cxyc_interp_signed",
            "coefficient_caveat": "Generic/reference OCIMF tanker-current coefficients resolved through the approved licensed off-repo workbook route; not vessel-specific SIROCCO current-coefficient curves",
            "component_sum": "total X/Y/N components = current review load + rudder-induced component, summed in ship-fixed COG axes",
        },
        "default_speed_policy": metadata["default_speed_policy"],
        "scope_exclusions": metadata["limitations"],
        "design_data": metadata["design_data"],
        "analysis_assumptions": metadata["analysis_assumptions"],
        "traceability_links": metadata["traceability_links"],
        "sample_working_example": result["sample_working_example"],
        "summary": result["summary"],
    }



def _citation_sidecar(result: dict[str, Any]) -> dict[str, Any]:
    preflight = issue_2760_source_preflight()
    metadata = result["metadata"]
    citations = [
        asdict(preflight["ocimf"]["citation"]),
        asdict(preflight["rudder"]["citation"]),
    ]
    return {
        "citations": citations,
        "ocimf_basis_selection": metadata["design_data"]["ocimf_basis_selection"],
        "license_boundary": preflight["ocimf"]["license_boundary"],
        "source_artifacts": {
            "ocimf_workbook": preflight["ocimf"]["workbook_source_id"],
            "provenance_readme": preflight["ocimf"]["provenance_readme"],
        },
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
        "This includes first-cut hull-current + rudder component sums for comparison only; it is not a validated whole-vessel current-load, oblique-current hull/rudder interaction, mooring, tug, bank-effect, or compliance model. The current terms use a generic/reference OCIMF tanker-current basis resolved through the approved licensed off-repo workbook route, not vessel-specific SIROCCO current-coefficient curves.",
        "",
        "## Traceability links",
        "",
        f"- GitHub issue: [workspace-hub #2760]({links['current_heading_rudder_issue']})",
        f"- Approved plan: [issue #2760 plan]({links['plan']})",
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
        "- Heading/rudder effective-angle convention: `α = rudder_angle_deg - heading_offset_deg`.",
        "- Local-to-ship transform: `X_ship=X_local*cos(psi)-Y_local*sin(psi)`, `Y_ship=X_local*sin(psi)+Y_local*cos(psi)`.",
        "- First-cut generic/reference OCIMF tanker-current review loads: `q=0.5*rho*V^2`; `Cxc`, `Cyc`, and `Cxyc` are interpolated from the approved off-repo OCIMF workbook/provenance route; `Xc=q*(beam*draft)*Cxc`, `Yc=q*(LBP*draft)*Cyc`, `Nc=q*(LBP*draft)*LBP*Cxyc`. These report-specific coefficient lookups are not a reusable coefficient corpus.",
        "- Component sums at COG in ship-fixed axes: `X_total=X_current+X_rudder`, `Y_total=Y_current+Y_rudder`, `N_total=N_current+N_rudder`.",
        f"- {metadata['default_speed_policy']}",
        f"- {metadata['zero_effective_angle_note']}",
        "",
        "## Sweep coverage",
        "",
        f"- Requested engineering rows: `{result['summary']['requested_engineering_row_count']}`.",
        f"- Extra chart-default rows: `{result['summary']['extra_default_row_count']}`.",
        f"- Total generated rows: `{result['summary']['row_count']}`.",
        "",
        "## Heading/rudder schematic",
        "",
        "Use the report schematic to read the geometry convention before reviewing components: ship-fixed +X is forward, +Y is port, heading offset `psi` is positive bow-to-port, rudder command `delta` is positive to port, and the rudder inflow angle is `α = delta - psi`.",
        "",
        "## Sample working example",
        "",
        f"Data point: `{sample['data_point']}`.",
        "",
        f"- Speed conversion: `V = {sample['current_speed_kn']:.2f} kn * {KNOT_TO_M_PER_S:.5f} = {sample['current_speed_m_s']:.5f} m/s`.",
        f"- Base force: `F = {sample['beta']:.1f} * {sample['rudder_area_m2']:.6f} * {sample['current_speed_m_s']:.5f}^2 * {sample['cr']:.1f} = {sample['base_force_N']:.3f} N`.",
        f"- Rudder-induced COG components: `X_rudder = {sample['force_x_ship_N']:.3f} N`, `Y_rudder = {sample['force_y_ship_port_N']:.3f} N`, `N_rudder = {sample['moment_n_yaw_bow_port_kN_m']:.6f} kN-m`.",
        f"- Generic/reference OCIMF current-review components at this same point: `X_current = {sample['ocimf_current_force_x_ship_N']:.3f} N`, `Y_current = {sample['ocimf_current_force_y_ship_port_N']:.3f} N`, `N_current = {sample['ocimf_current_moment_n_yaw_bow_port_kN_m']:.6f} kN-m`.",
        f"- Combined COG components: `X_total = {sample['total_force_x_ship_N']:.3f} N`, `Y_total = {sample['total_force_y_ship_port_N']:.3f} N`, `N_total = {sample['total_moment_n_yaw_bow_port_kN_m']:.6f} kN-m`.",
        "",
        "## OCIMF current vs rudder component sums",
        "",
        "The report exposes individual generic/reference OCIMF current-review, rudder-induced, and total rows using `ocimf_current_*`, rudder `force_*`/`moment_*`, and `total_*` fields. Yaw moment is about the ship-fixed COG with positive bow-to-port convention.",
        "",
        "## Interpretation charts",

        "Chart 1 plots rudder-induced ship-fixed `X_ship` and `Y_ship` versus heading for the selected current speed and rudder angle. Chart 2 overlays OCIMF reference current-review, rudder, and total yaw moment about COG. The HTML report also includes selected-speed and selected-case summary panels that update with the controls.",
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
            f"<tr><th>Heading/rudder grid</th><td>{min(metadata['heading_offsets_deg']):.0f}..{max(metadata['heading_offsets_deg']):.0f} deg heading, 1 deg step × {min(metadata['rudder_angles_deg']):.0f}..{max(metadata['rudder_angles_deg']):.0f} deg rudder, 2 deg step</td></tr>",
        ]
    )
    sample_rows_html = "\n".join(
        [
            f"<li><code>V = {sample['current_speed_kn']:.2f} kn × {KNOT_TO_M_PER_S:.5f} = {sample['current_speed_m_s']:.5f} m/s</code>; issue default speed.</li>",
            f"<li><code>α = δ - ψ = {sample['rudder_angle_deg']:.1f} deg - {sample['heading_offset_deg']:.1f} deg = {sample['effective_rudder_inflow_angle_deg']:.1f} deg</code>.</li>",
            f"<li><code>F = {sample['beta']:.1f} × {sample['rudder_area_m2']:.6f} × {sample['current_speed_m_s']:.5f}² × {sample['cr']:.1f} = {sample['base_force_N']:.3f} N</code>.</li>",
            f"<li><code>Fn = F × sin(alpha) = {sample['normal_force_N']:.3f} N</code>.</li>",
            f"<li><code>X_ship = {sample['force_x_ship_N']:.3f} N</code>, <code>Y_ship = {sample['force_y_ship_port_N']:.3f} N</code>, <code>N_ship = {sample['moment_n_yaw_bow_port_kN_m']:.6f} kN-m</code>.</li>",
            f"<li><code>q = 0.5 × rho × V²</code>; current coefficients use report-specific OCIMF loaded-tanker workbook interpolation tied to the approved provenance route.</li>",
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
<p class=\"eyebrow\">B1528 SIROCCO · Issue #2760</p>
<h1>{REPORT_DISPLAY_HEADING}</h1>
<p>This interactive report visualizes <strong>rudder-induced</strong>, <strong>OCIMF reference hull-current review</strong>, and summed <strong>total</strong> force/yaw-moment components for B1528 SIROCCO current-heading/rudder combinations with rudder angles from {min(metadata['rudder_angles_deg']):.0f} deg through +{max(metadata['rudder_angles_deg']):.0f} deg. Component sums are comparison loads only; this is not a validated whole-vessel current-load, mooring, tug, bank-effect, or compliance model.</p>
<p>{REPORT_SCOPE}</p>
<div class=\"note-panel\"><strong>Default speed policy:</strong> {metadata['default_speed_policy']}</div>

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
<p>This schematic updates with the current speed/rudder controls and uses the selected breakdown heading. It shows ship-fixed axes, the current heading offset <code>ψ</code>, rudder command <code>δ</code>, and effective rudder inflow angle <code>α = δ - ψ</code>. Positive bow-to-port convention applies to <code>ψ</code>, <code>δ</code>, <code>Y_ship</code>, and yaw moment.</p>
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
<text id="schematic-alpha-label" x="412" y="310" class="svg-label">α = 0°</text>
<text id="schematic-force-label" x="176" y="105" class="svg-label">total force direction</text>
</svg>
<div>
<ul class="schematic-legend">
<li><strong>Black dashed axes:</strong> ship-fixed COG frame; +X forward and +Y port.</li>
<li><strong>Blue vector:</strong> current heading offset <code>ψ</code> relative to ship centerline.</li>
<li><strong>Orange vector:</strong> selected rudder command <code>δ</code>; the selector intentionally controls this angle for Chart 1/3/4 review.</li>
<li><strong>Green vector:</strong> selected-case total X/Y force direction from current + rudder components.</li>
<li><strong>Effective inflow:</strong> <code>α = δ - ψ</code>; when alpha is zero, rudder-induced load is zero but hull-current load remains.</li>
</ul>
<div class="schematic-readout" aria-label="Selected schematic geometry readout">
<div><strong>Speed</strong><br><span id="schematic-speed-readout">—</span></div>
<div><strong>Heading ψ</strong><br><span id="schematic-heading-readout">—</span></div>
<div><strong>Rudder δ</strong><br><span id="schematic-rudder-readout">—</span></div>
<div><strong>Effective α</strong><br><span id="schematic-alpha-readout">—</span></div>
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
<p>Envelope values below update with the current-speed selector and use the full heading × rudder grid at that speed. The selected default is <strong>3.08 kn</strong>; 4 kn is only the upper bound of the plotted/table range.</p>
<table id="selected-speed-envelope-summary" class="data-table" aria-label="Selected speed envelope summary">
<thead><tr><th>Metric</th><th>Envelope case</th><th>Value</th></tr></thead>
<tbody>
<tr><td>Selected speed</td><td id="selected-speed-case">—</td><td id="selected-speed-value">—</td></tr>
<tr><td>Max |Y_ship| at selected speed</td><td id="selected-speed-max-y-case">—</td><td id="selected-speed-max-y-value">—</td></tr>
<tr><td>Max |N_ship| at selected speed</td><td id="selected-speed-max-n-case">—</td><td id="selected-speed-max-n-value">—</td></tr>
<tr><td>Max |X_ship| at selected speed</td><td id="selected-speed-max-h-case">—</td><td id="selected-speed-max-h-value">—</td></tr>
</tbody>
</table>

<section class=\"chart-section\">
<h2>Chart 1 — Rudder-induced ship-fixed force components over heading</h2>
<p>Shows rudder-induced <code>X_ship</code>, <code>Y_ship</code>, in kN for the selected current speed and rudder angle. All rudder angles are selectable; the default rudder angle is <strong>28.0 deg port</strong>.</p>
<div id=\"force-components-chart\" class=\"chart\" aria-label=\"Rudder-induced ship-fixed force component chart\"></div>
</section>

<section class=\"chart-section\">
<h2>Chart 2 — Rudder-induced ship-fixed yaw moment chart over heading × rudder</h2>
<p>Shows signed rudder-induced <code>N_ship yaw moment (kN-m)</code> over heading offset and rudder angle for the selected current speed.</p>
<div id=\"yaw-moment-chart\" class=\"chart\" aria-label=\"Rudder-induced yaw moment chart\"></div>
</section>

<section class=\"chart-section\">
<h2>Chart 3 — Generic OCIMF current vs rudder vs total Y force</h2>
<p>Compares ship-fixed <code>Y_ship</code> force from the first-cut generic/reference OCIMF tanker-current review estimate, the rudder-induced force, and their component sum for the selected speed and rudder angle. Current coefficients are tied to the approved licensed off-repo workbook route, not vessel-specific SIROCCO curves.</p>
<div id=\"ocimf-rudder-component-force-chart\" class=\"chart\" aria-label=\"OCIMF current versus rudder versus total Y force chart\"></div>
</section>

<section class=\"chart-section\">
<h2>Chart 4 — Generic OCIMF current vs rudder vs total yaw moment about COG</h2>
<p>Compares signed yaw moment about COG from the generic/reference OCIMF current-review component, the rudder-induced component, and the summed total yaw moment. Positive yaw moment is bow-to-port.</p>
<div id=\"ocimf-rudder-component-yaw-chart\" class=\"chart\" aria-label=\"OCIMF current versus rudder versus total yaw moment about COG chart\"></div>
</section>

<section id=\"selected-case-force-breakdown\" class=\"print-section\">
<h2>Selected-case force breakdown</h2>
<p>Updates with current speed and rudder angle. Values are shown at the heading with maximum absolute total yaw moment for the selected trace, so individual X/Y/N component paths can be reviewed together.</p>
<table class=\"data-table\" aria-label=\"Selected-case OCIMF current rudder component force breakdown\">
<thead><tr><th>Component</th><th>X ship (kN)</th><th>Y ship port (kN)</th><th>Yaw moment about COG (kN-m)</th></tr></thead>
<tbody>
<tr><td>OCIMF current</td><td id=\"breakdown-current-x\">—</td><td id=\"breakdown-current-y\">—</td><td id=\"breakdown-current-n\">—</td></tr>
<tr><td>Rudder induced</td><td id=\"breakdown-rudder-x\">—</td><td id=\"breakdown-rudder-y\">—</td><td id=\"breakdown-rudder-n\">—</td></tr>
<tr><td>Total / mooring reaction opposite</td><td id=\"breakdown-total-x\">—</td><td id=\"breakdown-total-y\">—</td><td id=\"breakdown-total-n\">—</td></tr>
</tbody>
</table>
<p id=\"breakdown-case-label\" class=\"note-panel\">—</p>
</section>

<h2>Method and provenance</h2>
<pre class=\"method-block\">{metadata['method']}</pre>
<ul>
<li>Heading/rudder effective-angle convention: α = rudder_angle_deg - heading_offset_deg.</li>
<li>Local-to-ship transform rotates local current-frame X/Y loads into ship-fixed COG axes.</li>
<li>Current review equations: q=0.5*rho*V²; Cxc, Cyc, and Cxyc are interpolated from OCIMF loaded-tanker A9/A10/A11 workbook curves through the approved off-repo source route; X_current=q*(beam*draft)*Cxc; Y_current=q*(LBP*draft)*Cyc; N_current=q*(LBP*draft)*LBP*Cxyc.</li>
<li>{ZERO_EFFECTIVE_ANGLE_NOTE}</li>
<li>Scope note: hull current loads are first-cut generic/reference OCIMF tanker-current comparison loads; this is not a validated whole-vessel current-load or oblique-current hull/rudder interaction model.</li>
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
  const maxH = absMax(speedRows, 'force_x_ship_N');
  const extra = speedRows.some(row => row.is_chart_default_extra_speed) ? 'chart-default extra plane' : 'requested engineering sweep plane';
  updateText('selected-speed-case', extra);
  updateText('selected-speed-value', `${{speed}} kn · ${{speedRows.length}} rows`);
  updateText('selected-speed-max-y-case', caseLabel(maxY));
  updateText('selected-speed-max-y-value', `${{(maxY.force_y_ship_port_N/1000).toFixed(3)}} kN`);
  updateText('selected-speed-max-n-case', caseLabel(maxN));
  updateText('selected-speed-max-n-value', `${{maxN.moment_n_yaw_bow_port_kN_m.toFixed(3)}} kN-m`);
  updateText('selected-speed-max-h-case', caseLabel(maxH));
  updateText('selected-speed-max-h-value', `${{(maxH.force_x_ship_N/1000).toFixed(3)}} kN`);
}}
function updateForceChart() {{
  const speed = selectedSpeed();
  const rudder = selectedRudder();
  const rows = ROWS.filter(row => same(row.current_speed_kn, speed) && same(row.rudder_angle_deg, rudder)).sort(byHeading);
  const x = rows.map(row => row.heading_offset_deg);
  const hover = rows.map(row => `alpha=${{row.effective_rudder_inflow_angle_deg}} deg<br>X_local=${{(row.force_x_local_downstream_N/1000).toFixed(3)}} kN<br>Y_local=${{(row.force_y_local_port_of_current_N/1000).toFixed(3)}} kN<br>N_ship=${{row.moment_n_yaw_bow_port_kN_m.toFixed(3)}} kN-m`);
  const traces = [
    {{x, y: rows.map(row => row.force_x_ship_N/1000), name:'X_ship (kN)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>X_ship=%{{y:.3f}} kN<br>%{{customdata}}<extra></extra>'}},
    {{x, y: rows.map(row => row.force_y_ship_port_N/1000), name:'Y_ship port (kN)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>Y_ship=%{{y:.3f}} kN<br>%{{customdata}}<extra></extra>'}}
  ];
  Plotly.newPlot('force-components-chart', traces, {{title:`Rudder-induced ship-fixed force component (kN) · current ${{speed}} kn · rudder ${{rudder}} deg`, xaxis:{{title:'Heading offset ψ (deg)', zeroline:true, gridcolor:'#e5eaf1'}}, yaxis:{{title:'Rudder-induced ship-fixed force component (kN)', zeroline:true, gridcolor:'#e5eaf1'}}, height:STANDARD_CHART_HEIGHT, margin:{{l:76,r:30,t:66,b:64}}, legend:{{orientation:'h', y:-0.22}}, hovermode:'x unified', template:'plotly_white'}}, CHART_CONFIG);
}}
function updateYawChart() {{
  const speed = selectedSpeed();
  const speedRows = rowsForSpeed(speed);
  const headings = [...new Set(speedRows.map(row => row.heading_offset_deg))].sort((a,b)=>a-b);
  const rudders = [...new Set(speedRows.map(row => row.rudder_angle_deg))].sort((a,b)=>a-b);
  const z = rudders.map(rudder => headings.map(heading => {{
    const row = speedRows.find(item => same(item.heading_offset_deg, heading) && same(item.rudder_angle_deg, rudder));
    return row ? row.moment_n_yaw_bow_port_kN_m : null;
  }}));
  const rudder = selectedRudder();
  const rows = ROWS.filter(row => same(row.current_speed_kn, speed) && same(row.rudder_angle_deg, rudder)).sort(byHeading);
  const trace = {{x: rows.map(row => row.heading_offset_deg), y: rows.map(row => row.moment_n_yaw_bow_port_kN_m), name:'Rudder N (kN-m)', mode:'lines+markers', hovertemplate:'heading=%{{x}} deg<br>Rudder N=%{{y:.3f}} kN-m<extra></extra>'}};
  Plotly.newPlot('yaw-moment-chart', [trace], {{title:`Rudder-induced N_ship yaw moment (kN-m) · current ${{speed}} kn · rudder ${{rudder}} deg`, xaxis:{{title:'Heading offset ψ (deg)', gridcolor:'#e5eaf1'}}, yaxis:{{title:'Rudder N yaw moment (kN-m)', gridcolor:'#e5eaf1'}}, height:STANDARD_CHART_HEIGHT, margin:{{l:76,r:40,t:66,b:64}}, template:'plotly_white'}}, CHART_CONFIG);
}}
function selectedTraceRows() {{
  const speed = selectedSpeed();
  const rudder = selectedRudder();
  return ROWS.filter(row => same(row.current_speed_kn, speed) && same(row.rudder_angle_deg, rudder)).sort(byHeading);
}}
function updateComponentForceChart() {{
  const speed = selectedSpeed();
  const rudder = selectedRudder();
  const rows = selectedTraceRows();
  const x = rows.map(row => row.heading_offset_deg);
  const hover = rows.map(row => `Xc=${{(row.ocimf_current_force_x_ship_N/1000).toFixed(3)}} kN<br>Yc=${{(row.ocimf_current_force_y_ship_port_N/1000).toFixed(3)}} kN<br>Xr=${{(row.force_x_ship_N/1000).toFixed(3)}} kN<br>Yr=${{(row.force_y_ship_port_N/1000).toFixed(3)}} kN<br>Xt=${{(row.total_force_x_ship_N/1000).toFixed(3)}} kN<br>Yt=${{(row.total_force_y_ship_port_N/1000).toFixed(3)}} kN`);
  const traces = [
    {{x, y: rows.map(row => row.ocimf_current_force_y_ship_port_N/1000), name:'OCIMF current component (kN)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>OCIMF current component=%{{y:.3f}} kN<br>%{{customdata}}<extra></extra>'}},
    {{x, y: rows.map(row => row.force_y_ship_port_N/1000), name:'Rudder component (kN)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>Rudder component=%{{y:.3f}} kN<br>%{{customdata}}<extra></extra>'}},
    {{x, y: rows.map(row => row.total_force_y_ship_port_N/1000), name:'Total component (kN)', mode:'lines+markers', customdata:hover, hovertemplate:'heading=%{{x}} deg<br>Total component=%{{y:.3f}} kN<br>%{{customdata}}<extra></extra>'}}
  ];
  Plotly.newPlot('ocimf-rudder-component-force-chart', traces, {{title:`OCIMF current vs rudder vs total Y force (kN) · current ${{speed}} kn · rudder ${{rudder}} deg`, xaxis:{{title:'Heading offset ψ (deg)', zeroline:true, gridcolor:'#e5eaf1'}}, yaxis:{{title:'Y_ship force (kN)', rangemode:'tozero', gridcolor:'#e5eaf1'}}, height:STANDARD_CHART_HEIGHT, margin:{{l:76,r:30,t:66,b:64}}, legend:{{orientation:'h', y:-0.22}}, hovermode:'x unified', template:'plotly_white'}}, CHART_CONFIG);
}}
function updateComponentYawChart() {{
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
  Plotly.newPlot('ocimf-rudder-component-yaw-chart', traces, {{title:`OCIMF current vs rudder vs total yaw moment about COG (kN-m) · current ${{speed}} kn · rudder ${{rudder}} deg`, xaxis:{{title:'Heading offset ψ (deg)', zeroline:true, gridcolor:'#e5eaf1'}}, yaxis:{{title:'Yaw moment about COG (kN-m)', zeroline:true, gridcolor:'#e5eaf1'}}, height:STANDARD_CHART_HEIGHT, margin:{{l:82,r:30,t:66,b:64}}, legend:{{orientation:'h', y:-0.22}}, hovermode:'x unified', template:'plotly_white'}}, CHART_CONFIG);
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
  updateText('schematic-alpha-label', `α = ${{alpha}}°`);
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
  updateText('breakdown-current-n', fmtMoment(selected.ocimf_current_moment_n_yaw_bow_port_kN_m));
  updateText('breakdown-rudder-x', fmtKn(selected.force_x_ship_N));
  updateText('breakdown-rudder-y', fmtKn(selected.force_y_ship_port_N));
  updateText('breakdown-rudder-n', fmtMoment(selected.moment_n_yaw_bow_port_kN_m));
  updateText('breakdown-total-x', fmtKn(selected.total_force_x_ship_N));
  updateText('breakdown-total-y', fmtKn(selected.total_force_y_ship_port_N));
  updateText('breakdown-total-n', fmtMoment(selected.total_moment_n_yaw_bow_port_kN_m));
  updateHeadingRudderSchematic(selected);
  updateText('breakdown-case-label', `Selected breakdown case: current ${{selected.current_speed_kn}} kn · ${{caseLabel(selected)}} · total yaw reaction for mooring context = ${{(-selected.total_moment_n_yaw_bow_port_kN_m).toFixed(3)}} kN-m.`);
}}
function updateCharts() {{ updateSelectedSpeedEnvelope(); updateForceChart(); updateYawChart(); updateComponentForceChart(); updateComponentYawChart(); updateSelectedCaseBreakdown(); }}
document.getElementById('current-speed-select').addEventListener('change', updateCharts);
document.getElementById('rudder-angle-select').addEventListener('change', () => {{ updateForceChart(); updateComponentForceChart(); updateComponentYawChart(); updateSelectedCaseBreakdown(); }});
updateCharts();
</script>
</body>
</html>
"""


def _speed_option(speed: float, default_speed: float) -> str:
    selected = " selected" if speed == default_speed else ""
    suffix = " (chart default extra)" if speed == default_speed else ""
    return f'<option value="{speed:g}"{selected}>{speed:g} kn{suffix}</option>'


def _design_rows_plain(design_data: dict[str, Any]) -> list[tuple[str, str]]:
    return [
        (str(key), str(value))
        for key, value in design_data.items()
        if not isinstance(value, dict)
    ]


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
