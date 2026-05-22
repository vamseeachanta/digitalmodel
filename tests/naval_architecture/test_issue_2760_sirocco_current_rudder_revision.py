# ABOUTME: Issue #2760 approved-contract tests for B1528 SIROCCO current/rudder revision.
# ABOUTME: Locks approval-gated domains, source gates, and removal of placeholder OCIMF/resultant presentation.

import inspect
import json
from pathlib import Path

import pytest

from digitalmodel.citations.schema import Citation, validate_citation
import digitalmodel.naval_architecture.b1528_sirocco_current_heading_rudder_report as report_module
from digitalmodel.naval_architecture.b1528_sirocco_current_heading_rudder_report import (
    KNOT_TO_M_PER_S,
    load_packaged_b1528_current_heading_rudder_config,
    run_b1528_current_heading_rudder_report,
)


def _find_row(rows, speed, heading, rudder):
    return next(
        row
        for row in rows
        if row["current_speed_kn"] == pytest.approx(speed)
        and row["heading_offset_deg"] == pytest.approx(heading)
        and row["rudder_angle_deg"] == pytest.approx(rudder)
    )


def test_issue_2760_current_speed_unit_conversion():
    assert 3.08 * KNOT_TO_M_PER_S == pytest.approx(1.5844752)


def test_issue_2760_approved_domains_and_defaults():
    cfg = load_packaged_b1528_current_heading_rudder_config()

    assert cfg.chart_default_current_speed_kn == pytest.approx(3.08)
    assert min(cfg.current_speeds_kn) == pytest.approx(0.0)
    assert max(cfg.current_speeds_kn) == pytest.approx(4.0)
    assert 3.08 in cfg.current_speeds_kn
    assert cfg.heading_offsets_deg == tuple(float(value) for value in range(-5, 6))
    assert cfg.rudder_angles_deg == tuple(float(value) for value in range(0, 29, 2))
    assert cfg.prop_rotation_factor == pytest.approx(1.0)
    assert cfg.report_issue.endswith("/2760")
    assert cfg.plan_path.endswith("2026-05-20-issue-2760-b1528-sirocco-force-review-revision.md")


def test_issue_2760_placeholder_ocimf_constants_removed_from_source():
    source = inspect.getsource(report_module)

    assert "OCIMF_CURRENT_CX_BASE" not in source
    assert "OCIMF_CURRENT_CM_SCALE" not in source
    assert "ocimf_cy = heading_sin" not in source
    assert "1.05 * heading_abs_cos" not in source
    assert "0.55 * heading_sin" not in source
    assert "transparent reference heading coefficients" not in source.lower()
    assert "placeholder heading functions" not in source.lower()


def test_issue_2760_default_ocimf_coefficients_are_interpolated_from_workbook():
    cfg = load_packaged_b1528_current_heading_rudder_config()

    coeffs = report_module.resolve_ocimf_loaded_tanker_current_coefficients(cfg, heading_offset_deg=5.0)
    centerline = report_module.resolve_ocimf_loaded_tanker_current_coefficients(cfg, heading_offset_deg=0.0)
    starboard = report_module.resolve_ocimf_loaded_tanker_current_coefficients(cfg, heading_offset_deg=-5.0)

    assert coeffs["angle_table_deg"] == pytest.approx(175.0)
    # B1528 uses the large WD/T generic loaded-tanker basis: A9 conventional Cxc
    # and A10/A11 >6 curves, interpolated from the licensed workbook at 175 deg.
    assert coeffs["cxc"] == pytest.approx(-0.0324, abs=5e-4)
    assert coeffs["cyc"] == pytest.approx(0.03406, abs=5e-4)
    assert coeffs["cxyc"] == pytest.approx(0.00843, abs=5e-4)
    assert centerline["angle_table_deg"] == pytest.approx(180.0)
    assert centerline["cyc"] == pytest.approx(0.0, abs=1e-9)
    assert centerline["cxyc"] == pytest.approx(0.0, abs=1e-9)
    assert starboard["angle_table_deg"] == pytest.approx(175.0)
    assert starboard["cyc"] == pytest.approx(-coeffs["cyc"])
    assert starboard["cxyc"] == pytest.approx(-coeffs["cxyc"])
    assert coeffs["source_workbook"] == "licensed-off-repo-ocimf-workbook"


def test_issue_2760_cxyc_preserves_workbook_sign_in_negative_regime():
    """Sentinel for the latent abs() defect on Cxyc.

    OCIMF Annex A figure A11 (loaded-tanker yaw moment coefficient, WD/T>6) is
    signed across heading: positive for table_angles ~98..180 deg, negative for
    table_angles below ~98 deg. The implementation must apply the heading
    direction sign WITHOUT stripping the workbook's own Cxyc sign, or yaw
    moment direction silently inverts whenever a caller passes heading
    magnitudes >~85 deg. This is value-preserving for #2760's bounded sweep
    (table_angle 170..180 deg, Cxyc positive) but the function is callable
    with arbitrary headings and must remain truthful to the workbook.
    """

    cfg = load_packaged_b1528_current_heading_rudder_config()

    # heading=+95 deg (port) -> table_angle=85 deg -> Annex A11 Cxyc is negative
    port_far = report_module.resolve_ocimf_loaded_tanker_current_coefficients(
        cfg, heading_offset_deg=95.0
    )
    # heading=-95 deg (starboard) -> table_angle=85 deg -> sign-mirrored
    stbd_far = report_module.resolve_ocimf_loaded_tanker_current_coefficients(
        cfg, heading_offset_deg=-95.0
    )

    assert port_far["angle_table_deg"] == pytest.approx(85.0)
    assert stbd_far["angle_table_deg"] == pytest.approx(85.0)
    # Cxyc at table_angle 85 deg interpolates between A11 cells at 80 deg
    # (~-0.0370) and 90 deg (~-0.0180); workbook value is unambiguously negative.
    assert port_far["cxyc"] < 0.0, (
        f"Port heading at table_angle 85 deg must yield negative Cxyc per Annex A11; "
        f"got {port_far['cxyc']:+.5f} (abs() defect would invert this to positive)"
    )
    assert stbd_far["cxyc"] > 0.0, (
        f"Starboard heading at table_angle 85 deg must yield positive Cxyc by "
        f"port/starboard sign symmetry; got {stbd_far['cxyc']:+.5f}"
    )
    # Sign symmetry must hold across the port/starboard pair
    assert port_far["cxyc"] == pytest.approx(-stbd_far["cxyc"])
    # Cyc is positive-magnitude in workbook; heading sign flip yields opposite-signed Cyc
    assert port_far["cyc"] > 0.0
    assert stbd_far["cyc"] < 0.0


def test_issue_2760_ocimf_source_gate_and_citation_sidecars_resolve():
    preflight = report_module.issue_2760_source_preflight()

    assert "workbook_path" not in preflight["ocimf"]
    assert preflight["ocimf"]["workbook_source_id"] == "licensed-off-repo-ocimf-workbook"
    assert isinstance(preflight["ocimf"]["citation"], Citation)
    assert preflight["ocimf"]["citation"].code_id == "OCIMF-MEG4"
    assert "Annex A" in preflight["ocimf"]["citation"].section
    validate_citation(preflight["ocimf"]["citation"])
    assert preflight["ocimf"]["license_boundary"] == "pointer-only-no-coefficient-corpus"
    assert isinstance(preflight["rudder"]["citation"], Citation)
    assert preflight["rudder"]["citation"].code_id == "B1528-SIROCCO-RUDDER-YAW-INPUTS"
    validate_citation(preflight["rudder"]["citation"])




def test_issue_2760_ocimf_source_gate_requires_workbook_env(monkeypatch):
    monkeypatch.delenv(report_module.OCIMF_WORKBOOK_PATH_ENV, raising=False)

    with pytest.raises(FileNotFoundError, match=report_module.OCIMF_WORKBOOK_PATH_ENV):
        report_module.issue_2760_source_preflight()


def test_issue_2760_ocimf_source_gate_rejects_missing_workbook(monkeypatch, tmp_path):
    missing_workbook = tmp_path / "missing-ocimf.xlsx"
    monkeypatch.setenv(report_module.OCIMF_WORKBOOK_PATH_ENV, str(missing_workbook))

    with pytest.raises(FileNotFoundError, match="licensed OCIMF workbook"):
        report_module.issue_2760_source_preflight()


def test_issue_2760_ocimf_preflight_does_not_return_local_workbook_path(monkeypatch, tmp_path):
    workbook = tmp_path / "OCIMF Coef.xlsx"
    workbook.write_bytes(b"placeholder")
    monkeypatch.setenv(report_module.OCIMF_WORKBOOK_PATH_ENV, str(workbook))

    preflight = report_module.issue_2760_source_preflight()

    assert "workbook_path" not in preflight["ocimf"]
    assert str(workbook) not in repr(preflight)
    assert preflight["ocimf"]["workbook_source_id"] == "licensed-off-repo-ocimf-workbook"


def test_issue_2760_generated_provenance_emits_citation_sidecar(tmp_path):
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)

    sidecar_path = Path(manifest["citation_sidecar"])
    sidecar = json.loads(sidecar_path.read_text(encoding="utf-8"))

    assert sidecar_path.name == "b1528_sirocco_current_rudder_force_citations.json"
    assert {item["code_id"] for item in sidecar["citations"]} == {
        "OCIMF-MEG4",
        "B1528-SIROCCO-RUDDER-YAW-INPUTS",
    }
    assert sidecar["ocimf_basis_selection"]["selected_figures"] == ["A9", "A10", "A11"]
    assert sidecar["ocimf_basis_selection"]["selection_rule"] == "max_available_wd_over_t_for_loaded_tanker_current"
    assert sidecar["ocimf_basis_selection"]["wd_over_t"] == pytest.approx(
        result["metadata"]["design_data"]["water_depth_to_draft_ratio"]
    )
    assert sidecar["license_boundary"] == "pointer-only-no-coefficient-corpus"
    assert sidecar["source_artifacts"] == {
        "ocimf_workbook": "licensed-off-repo-ocimf-workbook",
        "provenance_readme": "docs/data/OCIMF_CORPUS_README.md",
    }
    sidecar_text = sidecar_path.read_text(encoding="utf-8")
    assert "/mnt/" not in sidecar_text


def test_issue_2760_ocimf_basis_selection_is_geometry_documented():
    cfg = load_packaged_b1528_current_heading_rudder_config()

    selection = report_module.select_ocimf_loaded_tanker_current_basis(cfg)

    assert selection["selected_figures"] == ["A9", "A10", "A11"]
    assert selection["wd_over_t"] == pytest.approx(cfg.water_depth_m / cfg.draft_m)
    assert selection["selected_wd_over_t_bucket"] == ">6"
    assert selection["selection_rule"] == "max_available_wd_over_t_for_loaded_tanker_current"
    assert "off-class" in selection["limitation"].lower()
    assert selection["rejected_alternatives"]


def test_issue_2760_default_current_and_rudder_signs_are_port_positive():
    result = run_b1528_current_heading_rudder_report()
    row = _find_row(result["rows"], 3.08, 5.0, 28.0)

    assert row["current_speed_m_s"] == pytest.approx(1.5844752)
    assert row["ocimf_current_force_y_ship_port_N"] > 0.0
    assert row["ocimf_current_moment_n_yaw_bow_port_Nm"] > 0.0
    assert row["force_y_ship_port_N"] > 0.0
    assert row["moment_n_yaw_bow_port_Nm"] > 0.0
    assert row["force_component_basis"].startswith("issue #2760")


def test_issue_2760_main_report_output_removes_resultants_and_heatmap(tmp_path):
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)

    report = Path(manifest["markdown_report"]).read_text(encoding="utf-8").lower()
    html = Path(manifest["html_report"]).read_text(encoding="utf-8").lower()
    manifest_text = Path(manifest["manifest"]).read_text(encoding="utf-8").lower()

    for text in (report, html, manifest_text):
        assert "heatmap" not in text
        assert "resultant" not in text
        assert "total horizontal force" not in text


def test_issue_2760_docx_output_opens_and_contains_required_sections(tmp_path):
    from docx import Document

    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)

    docx_path = Path(manifest["docx_report"])
    assert docx_path.name == "b1528_sirocco_current_rudder_force_report.docx"
    assert docx_path.exists()

    document = Document(str(docx_path))
    text = "\n".join(paragraph.text for paragraph in document.paragraphs).lower()

    assert "b1528 sirocco current/rudder force review" in text
    assert "scope" in text
    assert "design data" in text
    assert "analysis methodology and assumptions" in text
    assert "heading/rudder schematic" in text
    assert "sample working example" in text
    assert "ocimf current vs rudder component sums" in text
    assert "limitations" in text
    assert "heatmap" not in text
    assert "resultant" not in text
    assert "total horizontal force" not in text


def test_issue_2760_html_javascript_uses_only_x_y_n_component_fields(tmp_path):
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)

    row_fields = set(result["rows"][0])
    html = Path(manifest["html_report"]).read_text(encoding="utf-8").lower()
    removed_resultant_fields = {
        "component_horizontal_force_N",
        "ocimf_current_component_horizontal_force_N",
        "total_component_horizontal_force_N",
    }

    assert removed_resultant_fields.isdisjoint(row_fields)
    for field in removed_resultant_fields:
        assert field.lower() not in html
    assert "component check" not in html
    assert "horizontal component" not in html
    assert "horizontal force" not in html
    assert "resultant" not in html
    assert "default rudder angle is neutral" not in html
    assert "default rudder angle is <strong>28" in html
    assert "ocimf loaded-tanker a9/a10/a11 workbook curves" in html
    assert "1.05*abs(cos" not in html
    assert "cm=0.55*sin" not in html
    assert "duplicate trace" not in html
    assert "maxh = absmax(speedrows, 'force_x_ship_n')" in html
    assert "maxh = maxby(speedrows, 'force_x_ship_n')" not in html
    assert "Current heading θ (deg)" not in html
    assert "heading offset ψ (deg)" in html
