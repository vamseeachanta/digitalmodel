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
    assert max(cfg.current_speeds_kn) == pytest.approx(5.0)  # Pass H sensitivity extension
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
    text = "\n".join(paragraph.text for paragraph in document.paragraphs)
    text_lower = text.lower()

    # Pass G canonical 6-section layout (DOCX matches MD/HTML contract)
    assert "1. Introduction" in text
    assert "2. Design Data" in text
    assert "3. Axes" in text and "Sign Conventions" in text
    assert "4. Load Due to Current" in text
    assert "5. Load Due to Rudder" in text
    assert "6. Limitations" in text
    # Pass G professional citation: OCIMF MEG4 + Annex A figure references
    assert "ocimf meg" in text_lower
    assert "annex a" in text_lower
    # Freshman-grad English labels (rejects pre-Pass-A code-identifier text)
    assert "longitudinal" in text_lower
    assert "transverse" in text_lower
    # Removed sections (sentinel reject)
    assert "heatmap" not in text_lower
    assert "resultant" not in text_lower
    assert "total horizontal force" not in text_lower
    # Pre-Pass-A headings must not appear (proves Pass G actually ran)
    assert "Scope" not in text  # superseded by "1. Introduction"
    assert "Analysis methodology and assumptions" not in text  # superseded by §3
    assert "Heading/rudder schematic" not in text  # superseded by per-section schematic references


def test_issue_2760_canonical_section_layout_in_markdown(tmp_path):
    """Pass A contract: MD must follow the 6-section canonical layout from
    issue thread comment 4492317819 and approved plan §3.1.

    Sections appear in order: Introduction, Design Data & Assumptions,
    Axes & Sign Conventions, Load Due to Current, Load Due to Rudder, Limitations.
    Each major calculation section has its own schematic block (Pass B fills SVG).
    """
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)
    md = Path(manifest["markdown_report"]).read_text(encoding="utf-8")

    expected_headings_in_order = [
        "## 1. Introduction",
        "## 2. Design Data & Assumptions",
        "## 3. Axes & Sign Conventions",
        "## 4. Load Due to Current",
        "## 5. Load Due to Rudder",
        "## 6. Limitations",
    ]
    positions = [md.find(h) for h in expected_headings_in_order]
    for heading, pos in zip(expected_headings_in_order, positions):
        assert pos >= 0, f"Missing canonical section heading: {heading!r}"
    assert positions == sorted(positions), (
        f"Section headings out of order: {list(zip(expected_headings_in_order, positions))}"
    )


def test_issue_2760_canonical_section_layout_in_html(tmp_path):
    """Pass A contract: HTML must follow the same 6-section canonical layout."""
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)
    html = Path(manifest["html_report"]).read_text(encoding="utf-8")

    expected_headings_in_order = [
        ">1. Introduction<",
        ">2. Design Data &amp; Assumptions<",
        ">3. Axes &amp; Sign Conventions<",
        ">4. Load Due to Current<",
        ">5. Load Due to Rudder<",
        ">6. Limitations<",
    ]
    positions = [html.find(h) for h in expected_headings_in_order]
    for heading, pos in zip(expected_headings_in_order, positions):
        assert pos >= 0, f"Missing canonical HTML section heading: {heading!r}"
    assert positions == sorted(positions), (
        f"HTML headings out of order: {list(zip(expected_headings_in_order, positions))}"
    )


def test_issue_2760_per_section_schematic_anchors_exist(tmp_path):
    """Pass A→B handoff contract: each major calculation section has its own
    schematic placeholder/anchor. Pass B fills the SVG content; Pass A
    only needs the anchor structure to exist so the SVGs have a home.
    """
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)
    html = Path(manifest["html_report"]).read_text(encoding="utf-8")

    required_schematic_ids = [
        "schematic-axes-conventions",
        "schematic-current-loading",
        "schematic-current-moment",
        "schematic-rudder-loading",
    ]
    for sid in required_schematic_ids:
        assert f'id="{sid}"' in html, f"Missing per-section schematic anchor: {sid}"


def test_issue_2760_pass_b_schematic_svgs_have_real_content(tmp_path):
    """Pass B/G contract: per-section schematic SVGs (current-loading,
    current-moment, rudder-loading) must contain real ship+arrow markup,
    not just placeholder text. Each must:
    - declare a ship hull element (transparent OR OCIMF-style muted fill;
      ship-hull-transparent class is reused for both styles)
    - contain at least one arrow with marker-end (OCIMF-style hairline 1.5pt
      via ocimf-arrow class, OR legacy bold 5pt via schematic-bold-arrow-*)
    - include a CoG marker (circle or text)
    - bake in default-value annotations (PDF/DOCX-safe; no JS dependency)

    Pass G updated the visual contract from "bold colored arrows + adjacent
    labels" to OCIMF Annex A style (hairline navy arrows, symbols-only labels,
    English mapping in caption block below figure). Test relaxed accordingly.
    """
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)
    html = Path(manifest["html_report"]).read_text(encoding="utf-8")

    import re
    for sid in ("schematic-current-loading", "schematic-current-moment", "schematic-rudder-loading"):
        # Extract the SVG block by id
        match = re.search(
            rf'<svg id="{sid}".*?</svg>',
            html, re.DOTALL,
        )
        assert match is not None, f"Missing or unparseable SVG for {sid}"
        svg = match.group(0)
        assert "[Pass B will fill" not in svg, (
            f"{sid} still contains Pass A placeholder text — Pass B did not replace it"
        )
        # Ship hull element present (style — transparent vs. OCIMF muted — is a
        # presentation choice; the class name was retained across Pass B→G).
        assert 'class="ship-hull-transparent"' in svg, f"{sid} missing ship hull element"
        # At least one arrow with marker-end. Pass G OCIMF-style is hairline 1.5pt
        # via ocimf-arrow class; Pass B legacy bold via schematic-bold-arrow-*.
        has_arrow = (
            "ocimf-arrow" in svg
            or "schematic-bold-arrow-" in svg
            or re.search(r'stroke-width\s*[:=]\s*"?[1-9]', svg) is not None
        )
        assert has_arrow, f"{sid} has no arrow element"
        assert "marker-end" in svg, f"{sid} has no marker-end arrowhead reference"
        # CoG marker (text or implicit via circle + label)
        assert ("CoG" in svg or "COG" in svg or "C.G." in svg or "centre of gravity" in svg.lower()), (
            f"{sid} missing CoG marker label"
        )


def test_issue_2760_pass_h_revisions(tmp_path):
    """Pass H contract: §4.2 transverse current force chart, §5.1 renamed to
    Whicker-Fehlner - Model A, §5.4/§5.5 lead-in key variables, §5.6
    current-speed sensitivity (0..5 kn) with two new charts, old §5.6 model
    comparison relocated to Appendix A, §3 schematic uses OCIMF style.
    """
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)
    html = Path(manifest["html_report"]).read_text(encoding="utf-8")
    md = Path(manifest["markdown_report"]).read_text(encoding="utf-8")

    # H1: §3 schematic now uses OCIMF style (per-section-schematic class + cardinal labels)
    assert 'id="schematic-axes-conventions" class="per-section-schematic"' in html
    # H2: new transverse current force chart at §4.2
    assert 'id="current-transverse-force-chart"' in html
    # H3: §5.1 title renamed
    assert "5.1. Sample calculation at default values (Whicker-Fehlner - Model A)" in html
    assert "(Whicker-Fehlner - Model A)" in md
    # H4: key-variables lead-in in §5.4 and §5.5
    assert html.count("Key variables for this section:") >= 2
    # H5: new §5.6 sensitivity charts + sweep extended to 5 kn
    assert "5.6. Current-speed sensitivity (0..5 knots)" in html
    assert 'id="sensitivity-current-load-chart"' in html
    assert 'id="sensitivity-rudder-load-chart"' in html
    assert max(result["metadata"]["current_speeds_kn"]) == pytest.approx(5.0)
    # H6: old §5.6 rudder model comparison moved to Appendix A
    assert "Appendix A — Rudder model comparison" in html
    assert 'id="appendix-a-rudder-model-comparison"' in html
    # Old §5.6 heading must no longer appear
    assert "5.6. Rudder model comparison" not in html


def test_issue_2760_pass_g_schematics_have_ocimf_caption_block(tmp_path):
    """Pass G contract: each per-section schematic SVG is followed by a
    caption block (.schematic-caption div) carrying the full English
    mapping for the on-figure symbols, per OCIMF Annex A style. This
    declutters the figure and matches OCIMF MEG3/MEG4 house style where
    figures carry symbols only and captions explain.
    """
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)
    html = Path(manifest["html_report"]).read_text(encoding="utf-8")

    # At least 3 schematic-caption blocks (one per per-section schematic)
    assert html.count('class="schematic-caption"') >= 3, (
        "Pass G expects at least 3 OCIMF-style caption blocks under the per-section schematics"
    )
    # Captions cite OCIMF MEG4 [1] (numeric reference) and explain symbols
    html_low = html.lower()
    assert "ocimf meg4" in html_low and "annex a" in html_low, (
        "Pass G captions must cite OCIMF MEG4 Annex A explicitly per professional citation style"
    )


def test_issue_2760_pass_c_simple_plate_rudder_helper_exists_and_is_sane():
    """Pass C contract: a thin-plate rudder normal-force helper exists,
    cites Faltinsen 1990 §6.5 in its docstring, and produces sane values:
    - F=0 at α=0
    - F monotonically increases with α through small-angle range
    - F is capped/bounded above a stall angle (no runaway at large α)
    - F scales as V²
    """
    import math
    helper = getattr(report_module, "_rudder_simple_plate_force_N", None)
    assert helper is not None, (
        "Pass C must add _rudder_simple_plate_force_N(area_m2, velocity_m_s, "
        "alpha_deg, rho_kg_m3) per approved plan §128"
    )
    docstring = (helper.__doc__ or "").lower()
    assert "faltinsen" in docstring, (
        "Pass C helper must cite Faltinsen, Sea Loads on Ships and Offshore "
        "Structures, 1990, §6.5 (or equivalent thin-plate marine reference)"
    )

    rho = 1025.0
    area = 44.94
    v = 1.5845  # 3.08 kn in m/s

    # α=0 → zero force
    assert abs(helper(area, v, 0.0, rho)) < 1e-9

    # Small-angle monotonic increase
    f_5 = helper(area, v, 5.0, rho)
    f_15 = helper(area, v, 15.0, rho)
    assert f_5 > 0
    assert f_15 > f_5

    # V² scaling: doubling V quadruples F at the same α
    f_v = helper(area, v, 10.0, rho)
    f_2v = helper(area, 2 * v, 10.0, rho)
    assert f_2v == pytest.approx(4.0 * f_v, rel=1e-6)

    # Stall cap: at very large α (e.g. 45°), F must not exceed the maximum
    # the formula reaches in the un-stalled regime — i.e., bounded behaviour
    f_28 = helper(area, v, 28.0, rho)
    f_45 = helper(area, v, 45.0, rho)
    f_60 = helper(area, v, 60.0, rho)
    assert f_45 <= f_28 + 1e-9, (
        "Simple-plate model must apply stall cap; got "
        f"f(28°)={f_28:.1f} N, f(45°)={f_45:.1f} N"
    )
    assert f_60 <= f_28 + 1e-9


def test_issue_2760_pass_c_simple_plate_fields_in_row_dict():
    """Pass C contract: each row in the sweep carries simple-plate model
    fields alongside the existing Whicker-Fehlner fields, so the HTML
    side-by-side chart can plot both without recomputation.
    """
    result = run_b1528_current_heading_rudder_report()
    row = result["rows"][0]
    required_fields = [
        "simple_plate_normal_force_N",
        "simple_plate_force_x_ship_N",
        "simple_plate_force_y_ship_port_N",
        "simple_plate_moment_n_yaw_bow_port_Nm",
    ]
    for field in required_fields:
        assert field in row, f"Pass C row dict missing required field: {field}"
    # At the default heading/rudder/speed, simple-plate Y should be same sign
    # as Whicker-Fehlner Y (both lift the ship toward port for δ=+28°/ψ=+5°)
    default_row = next(
        r for r in result["rows"]
        if r["current_speed_kn"] == pytest.approx(3.08)
        and r["heading_offset_deg"] == pytest.approx(5.0)
        and r["rudder_angle_deg"] == pytest.approx(28.0)
    )
    assert default_row["simple_plate_force_y_ship_port_N"] > 0
    assert default_row["force_y_ship_port_N"] > 0


def test_issue_2760_pass_e_yaw_side_by_side_chart_and_field(tmp_path):
    """Pass E contract: §4.2 includes a side-by-side yaw chart comparing
    OCIMF direct yaw moment (Method A: Nc = q·A_l·LBP·Cxyc) with
    Y_current × lever_arm (Method B). Per plan §126, this is a sanity
    check, NOT an equality test — caption must state that explicitly.

    A new row field `current_y_times_lever_arm_moment_n_yaw_bow_port_kN_m`
    carries Method B's value for the chart.
    """
    result = run_b1528_current_heading_rudder_report()
    row = result["rows"][0]
    assert "current_y_times_lever_arm_moment_n_yaw_bow_port_kN_m" in row, (
        "Pass E requires Method B (Y×arm) yaw moment field in row dict"
    )

    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)
    html = Path(manifest["html_report"]).read_text(encoding="utf-8")
    assert 'id="yaw-side-by-side-chart"' in html, "Pass E chart missing"
    assert "method a" in html.lower() and "method b" in html.lower(), (
        "Pass E caption must label Methods A and B"
    )
    # Plan §126: not an equality test — caption must state methods may differ
    html_low = html.lower()
    assert ("may differ" in html_low or "different assumptions" in html_low or
            "not an equality" in html_low or "not equality-based" in html_low), (
        "Pass E caption must explicitly state methods rest on different "
        "assumptions / not equality-based per plan §126"
    )


def test_issue_2760_pass_d_decimal_rounding_in_display_values(tmp_path):
    """Pass D contract: kN and kN·m display values are rounded to 0 decimals
    when |val| ≥ 1, and to 2 decimals otherwise (per plan §130 and user
    request for freshman-grad-readable presentation).

    Tests the JS formatters for breakdown/envelope/hover (HTML literal
    .toFixed(0) where the value is in kN/kN·m, not the underlying numeric
    JSON which retains full precision).
    """
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)
    html = Path(manifest["html_report"]).read_text(encoding="utf-8")

    # The JS chart hover and breakdown formatters should round kN/kN·m
    # display to 0 decimals (canonical short form). Reject lingering
    # 3-decimal display patterns in hovertemplates and fmt helpers.
    assert "toFixed(3)} kN" not in html, (
        "Pass D requires kN hover values rounded to 0 decimals, not 3"
    )
    # The fmtKn/fmtMoment helpers should round large values (|val|>=1) to 0.
    # Allow nested .toFixed(2) usage for the small-value branch and for the
    # rudder-model comparison normal force (which is in N not kN).
    assert "function fmtKn" in html
    assert ".toFixed(0)" in html, "Pass D needs at least one .toFixed(0) display rounder"


def test_issue_2760_pass_d_force_labels_use_english_terms(tmp_path):
    """Pass D contract: HTML uses English force labels
    "longitudinal" / "transverse" / "yaw moment" alongside the symbolic
    X/Y/N — not just bare X_ship/Y_ship code identifiers.
    """
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)
    html = Path(manifest["html_report"]).read_text(encoding="utf-8").lower()
    md = Path(manifest["markdown_report"]).read_text(encoding="utf-8").lower()

    # Both HTML and MD must say "longitudinal" / "transverse" / "yaw moment"
    # at least once in the body content (already done in Pass A, sentinel here).
    for doc, name in ((html, "HTML"), (md, "MD")):
        assert "longitudinal" in doc, f"{name} missing 'longitudinal' label"
        assert "transverse" in doc, f"{name} missing 'transverse' label"
        assert "yaw moment" in doc, f"{name} missing 'yaw moment' label"
    # The chart axis title must use English not code identifier
    assert "transverse y" in html or "transverse force" in html


def test_issue_2760_pass_c_html_has_side_by_side_rudder_comparison(tmp_path):
    """Pass C contract: HTML §5 contains a Model A vs Model B side-by-side
    comparison table at the default case AND a chart comparing both
    rudder models across the rudder sweep.
    """
    result = run_b1528_current_heading_rudder_report()
    manifest = report_module.write_b1528_current_heading_rudder_report(result, tmp_path)
    html = Path(manifest["html_report"]).read_text(encoding="utf-8")

    # Side-by-side default-case comparison table id
    assert 'id="rudder-model-comparison-table"' in html or "model a vs model b" in html.lower()
    # Side-by-side chart id
    assert 'id="rudder-model-comparison-chart"' in html


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
    # Pass A restructure: rudder default is stated in §5.2 chart caption.
    # Canonical form pinned (no OR-disjunction; per feedback_silent_verdict_flip_defect_class).
    assert "default rudder angle: <strong>28° port</strong>" in html.lower()
    # Pass G G6: citation upgraded from "ocimf loaded-tanker a9/a10/a11 workbook curves"
    # to professional form "OCIMF MEG4 (2018) [1] Annex A figures A9/A10/A11"
    assert "ocimf meg4 (2018) [1] annex a figures a9/a10/a11" in html
    assert "1.05*abs(cos" not in html
    assert "cm=0.55*sin" not in html
    assert "duplicate trace" not in html
    assert "maxh = absmax(speedrows, 'force_x_ship_n')" in html
    assert "maxh = maxby(speedrows, 'force_x_ship_n')" not in html
    assert "Current heading θ (deg)" not in html
    assert "heading offset ψ (deg)" in html
