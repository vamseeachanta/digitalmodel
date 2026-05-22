# ABOUTME: Tests for B1528 SIROCCO current/rudder force-component review.
# ABOUTME: Locks issue #2760 grid contract, ship-fixed transform, chart controls, and report artifacts.

import json
import math
from pathlib import Path

import pytest

from digitalmodel.naval_architecture.b1528_sirocco_current_heading_rudder_report import (
    KNOT_TO_M_PER_S,
    load_packaged_b1528_current_heading_rudder_config,
    run_b1528_current_heading_rudder_report,
    validate_b1528_current_heading_rudder_config,
    write_b1528_current_heading_rudder_report,
)


def _find_row(rows, speed, heading, rudder):
    return next(
        row
        for row in rows
        if row["current_speed_kn"] == pytest.approx(speed)
        and row["heading_offset_deg"] == pytest.approx(heading)
        and row["rudder_angle_deg"] == pytest.approx(rudder)
    )


def test_yaml_loads_issue_2760_sweep_and_default_contract():
    cfg = load_packaged_b1528_current_heading_rudder_config()

    assert cfg.case_id == "b1528_sirocco_current_heading_rudder_forces"
    assert "SIROCCO" in cfg.aliases
    assert "Sorrocco" in cfg.aliases
    assert cfg.current_speeds_kn == (0.0, 1.0, 2.0, 3.0, 3.08, 4.0)
    assert cfg.chart_default_current_speed_kn == pytest.approx(3.08)
    assert cfg.heading_offsets_deg == tuple(float(value) for value in range(-5, 6))
    assert cfg.rudder_angles_deg == tuple(float(value) for value in range(0, 29, 2))
    assert max(cfg.rudder_angles_deg) == pytest.approx(28.0)
    assert "issue #2760" in cfg.raw["case"]["description"].lower()
    assert cfg.raw["outputs"]["directory"].endswith("current_rudder_force")
    assert cfg.prop_rotation_factor == pytest.approx(1.0)
    assert cfg.lbp_m == pytest.approx(225.5)
    assert cfg.yaw_lever_m == pytest.approx(135.3)
    assert cfg.rudder_area_m2 == pytest.approx(44.93956319369854)
    assert "3.08 kn" in cfg.default_speed_policy.lower()
    assert "local-to-ship" in cfg.force_convention["transform"].lower()


def test_package_level_exports_available():
    import digitalmodel.naval_architecture as naval_architecture

    assert callable(naval_architecture.load_packaged_b1528_current_heading_rudder_config)
    assert callable(naval_architecture.run_b1528_current_heading_rudder_report)
    assert callable(naval_architecture.write_b1528_current_heading_rudder_report)


def test_full_sweep_row_counts_and_default_plane_flags():
    result = run_b1528_current_heading_rudder_report()
    rows = result["rows"]

    assert len(rows) == 990
    assert sum(not row["is_chart_default_extra_speed"] for row in rows) == 990
    assert sum(row["is_chart_default_extra_speed"] for row in rows) == 0
    assert sorted({row["current_speed_kn"] for row in rows}) == [0.0, 1.0, 2.0, 3.0, 3.08, 4.0]
    assert result["summary"]["row_count"] == 990
    assert result["summary"]["requested_engineering_row_count"] == 990
    assert result["summary"]["extra_default_row_count"] == 0
    assert result["summary"]["max_abs_ship_sway_force_N"] >= 0.0
    assert abs(result["summary"]["max_abs_ship_sway_signed_force_N"]) == pytest.approx(
        result["summary"]["max_abs_ship_sway_force_N"]
    )
    assert result["summary"]["max_abs_yaw_moment_kN_m"] >= 0.0
    assert abs(result["summary"]["max_abs_yaw_signed_moment_kN_m"]) == pytest.approx(
        result["summary"]["max_abs_yaw_moment_kN_m"]
    )


def test_zero_effective_inflow_angle_identity_and_scope_wording():
    result = run_b1528_current_heading_rudder_report()
    row = _find_row(result["rows"], 3.08, 0.0, 0.0)

    assert row["effective_rudder_inflow_angle_deg"] == pytest.approx(0.0)
    assert row["force_x_local_downstream_N"] == pytest.approx(0.0, abs=1e-9)
    assert row["force_y_local_port_of_current_N"] == pytest.approx(0.0, abs=1e-9)
    assert row["force_x_ship_N"] == pytest.approx(0.0, abs=1e-9)
    assert row["force_y_ship_port_N"] == pytest.approx(0.0, abs=1e-9)
    assert row["moment_n_yaw_bow_port_Nm"] == pytest.approx(0.0, abs=1e-9)
    assert "not total hull current load" in result["metadata"]["zero_effective_angle_note"].lower()


def test_centerline_regression_matches_existing_fixed_report_formula():
    result = run_b1528_current_heading_rudder_report()
    row = _find_row(result["rows"], 3.08, 0.0, 28.0)
    alpha = math.radians(28.0)
    speed_m_s = 3.08 * KNOT_TO_M_PER_S
    base_force = 600.0 * 44.93956319369854 * speed_m_s**2 * 1.0
    expected_x = base_force * math.sin(alpha) ** 2
    expected_y = base_force * math.sin(alpha) * math.cos(alpha)

    assert row["base_force_N"] == pytest.approx(base_force)
    assert row["force_x_local_downstream_N"] == pytest.approx(expected_x)
    assert row["force_y_local_port_of_current_N"] == pytest.approx(expected_y)
    assert row["force_x_ship_N"] == pytest.approx(expected_x)
    assert row["force_y_ship_port_N"] == pytest.approx(expected_y)
    assert row["moment_n_yaw_bow_port_kN_m"] == pytest.approx(expected_y * 135.3 / 1000.0)


def test_sign_cases_for_port_rudder_and_zero_rudder():
    rows = run_b1528_current_heading_rudder_report()["rows"]
    port = _find_row(rows, 3.08, 0.0, 28.0)
    zero = _find_row(rows, 3.08, 0.0, 0.0)

    assert port["force_y_ship_port_N"] > 0.0
    assert port["moment_n_yaw_bow_port_Nm"] > 0.0
    assert zero["force_y_ship_port_N"] == pytest.approx(0.0)
    assert zero["moment_n_yaw_bow_port_Nm"] == pytest.approx(0.0)
    assert port["force_x_local_downstream_N"] >= 0.0


def test_heading_rudder_interaction_rotates_local_force_to_ship_frame():
    rows = run_b1528_current_heading_rudder_report()["rows"]
    oblique = _find_row(rows, 3.08, 4.0, 28.0)
    centerline_equivalent_alpha = _find_row(rows, 3.08, 0.0, 24.0)

    assert oblique["effective_rudder_inflow_angle_deg"] == pytest.approx(
        centerline_equivalent_alpha["effective_rudder_inflow_angle_deg"]
    )
    assert oblique["force_x_local_downstream_N"] == pytest.approx(
        centerline_equivalent_alpha["force_x_local_downstream_N"]
    )
    assert oblique["force_y_local_port_of_current_N"] == pytest.approx(
        centerline_equivalent_alpha["force_y_local_port_of_current_N"]
    )
    assert oblique["force_x_ship_N"] != pytest.approx(centerline_equivalent_alpha["force_x_ship_N"])
    assert oblique["force_y_ship_port_N"] != pytest.approx(centerline_equivalent_alpha["force_y_ship_port_N"])


def test_ship_fixed_transform_independent_formula():
    rows = run_b1528_current_heading_rudder_report()["rows"]
    row = _find_row(rows, 2.0, -5.0, 10.0)
    psi = math.radians(-5.0)
    expected_x = row["force_x_local_downstream_N"] * math.cos(psi) - row[
        "force_y_local_port_of_current_N"
    ] * math.sin(psi)
    expected_y = row["force_x_local_downstream_N"] * math.sin(psi) + row[
        "force_y_local_port_of_current_N"
    ] * math.cos(psi)

    assert row["force_x_ship_N"] == pytest.approx(expected_x)
    assert row["force_y_ship_port_N"] == pytest.approx(expected_y)
    assert row["moment_n_yaw_bow_port_Nm"] == pytest.approx(expected_y * 135.3)
    assert row["mooring_reaction_x_N"] == pytest.approx(-row["total_force_x_ship_N"])
    assert row["mooring_reaction_y_N"] == pytest.approx(-row["total_force_y_ship_port_N"])
    assert row["mooring_reaction_n_Nm"] == pytest.approx(-row["total_moment_n_yaw_bow_port_Nm"])


def test_speed_squared_scaling_for_same_heading_and_rudder():
    rows = run_b1528_current_heading_rudder_report()["rows"]
    slow = _find_row(rows, 2.0, -3.0, 10.0)
    fast = _find_row(rows, 4.0, -3.0, 10.0)

    for key in [
        "base_force_N",
        "force_x_local_downstream_N",
        "force_y_local_port_of_current_N",
        "force_x_ship_N",
        "force_y_ship_port_N",
        "moment_n_yaw_bow_port_Nm",
    ]:
        assert fast[key] / slow[key] == pytest.approx(4.0)


def test_formula_sample_for_issue_2760_default_speed():
    result = run_b1528_current_heading_rudder_report()
    sample = result["sample_working_example"]
    alpha = math.radians(23.0)
    speed_m_s = 3.08 * KNOT_TO_M_PER_S
    base_force = 600.0 * 44.93956319369854 * speed_m_s**2 * 1.0
    psi = math.radians(5.0)
    x_local = base_force * math.sin(alpha) ** 2
    y_local = base_force * math.sin(alpha) * math.cos(alpha)
    expected_x = x_local * math.cos(psi) - y_local * math.sin(psi)
    expected_y = x_local * math.sin(psi) + y_local * math.cos(psi)

    assert sample["data_point"] == "issue #2760 default 3.08 kn, heading +5 deg, rudder +28 deg, Cr=1.0"
    assert sample["current_speed_m_s"] == pytest.approx(speed_m_s)
    assert sample["base_force_N"] == pytest.approx(base_force)
    assert sample["normal_force_N"] == pytest.approx(base_force * math.sin(alpha))
    assert sample["force_x_ship_N"] == pytest.approx(expected_x)
    assert sample["force_y_ship_port_N"] == pytest.approx(expected_y)
    assert sample["moment_n_yaw_bow_port_kN_m"] == pytest.approx(expected_y * 135.3 / 1000.0)


def test_ocimf_current_rudder_and_total_components_are_reported_about_cog():
    result = run_b1528_current_heading_rudder_report()
    row = _find_row(result["rows"], 3.08, 5.0, 28.0)
    q = 0.5 * 1025.0 * row["current_speed_m_s"] ** 2
    expected_current_x = q * (32.26 * 12.2) * row["ocimf_first_cut_cx_current"]
    expected_current_y = q * (225.5 * 12.2) * row["ocimf_first_cut_cy_current"]
    expected_current_n = q * (225.5 * 12.2) * 225.5 * row["ocimf_first_cut_cm_current"]

    assert row["ocimf_first_cut_cx_current"] == pytest.approx(-0.0324, abs=5e-4)
    assert row["ocimf_first_cut_cy_current"] == pytest.approx(0.03406, abs=5e-4)
    assert row["ocimf_first_cut_cm_current"] == pytest.approx(0.00843, abs=5e-4)
    assert row["ocimf_current_force_x_ship_N"] == pytest.approx(expected_current_x)
    assert row["ocimf_current_force_y_ship_port_N"] == pytest.approx(expected_current_y)
    assert row["ocimf_current_moment_n_yaw_bow_port_Nm"] == pytest.approx(expected_current_n)
    assert row["total_force_x_ship_N"] == pytest.approx(
        row["ocimf_current_force_x_ship_N"] + row["force_x_ship_N"]
    )
    assert row["total_force_y_ship_port_N"] == pytest.approx(
        row["ocimf_current_force_y_ship_port_N"] + row["force_y_ship_port_N"]
    )
    assert row["total_moment_n_yaw_bow_port_Nm"] == pytest.approx(
        row["ocimf_current_moment_n_yaw_bow_port_Nm"] + row["moment_n_yaw_bow_port_Nm"]
    )


def test_report_outputs_include_issue_2760_dropdown_chart_contract_and_provenance(tmp_path):
    result = run_b1528_current_heading_rudder_report()
    manifest = write_b1528_current_heading_rudder_report(result, tmp_path)

    expected_names = {
        "csv": "b1528_sirocco_current_rudder_force_results.csv",
        "json": "b1528_sirocco_current_rudder_force_results.json",
        "provenance": "b1528_sirocco_current_rudder_force_provenance.json",
        "markdown_report": "b1528_sirocco_current_rudder_force_report.md",
        "html_report": "b1528_sirocco_current_rudder_force_report.html",
        "pdf_report": "b1528_sirocco_current_rudder_force_report.pdf",
        "manifest": "b1528_sirocco_current_rudder_force_manifest.json",
    }
    for key, expected_name in expected_names.items():
        assert Path(manifest[key]).exists(), key
        assert Path(manifest[key]).name == expected_name

    html = Path(manifest["html_report"]).read_text(encoding="utf-8")
    report = Path(manifest["markdown_report"]).read_text(encoding="utf-8")
    parsed = json.loads(Path(manifest["json"]).read_text(encoding="utf-8"))
    provenance = json.loads(Path(manifest["provenance"]).read_text(encoding="utf-8"))

    assert 'id="current-speed-select"' in html
    assert '<option value="3.08" selected>3.08 kn' in html
    for speed in ["0", "1", "2", "3", "3.08", "4"]:
        assert f'<option value="{speed}"' in html
    assert 'id="rudder-angle-select"' in html
    assert '<option value="28.0" selected>28.0 deg</option>' in html
    for angle in range(0, 29, 2):
        assert f'<option value="{float(angle):.1f}"' in html
    assert '<title>B1528 SIROCCO Current/Rudder Force Review — Issue #2760</title>' in html
    assert '<h1>B1528 SIROCCO current/rudder force review — Issue #2760</h1>' in html
    assert 'selected-speed-envelope-summary' in html
    assert 'Selected-speed envelope summary' in html
    assert 'id="force-components-chart"' in html
    assert 'id="yaw-moment-heatmap"' not in html
    # Pass A restructure: axes/sign-convention schematic ID is now `schematic-axes-conventions`
    # (superseded `heading-rudder-schematic` + `ship-current-rudder-svg`); per-section schematic
    # anchors (current-loading, current-moment, rudder-loading) live in §4-§5.
    assert 'id="schematic-axes-conventions"' in html
    assert 'id="schematic-current-loading"' in html
    assert 'id="schematic-current-moment"' in html
    assert 'id="schematic-rudder-loading"' in html
    assert 'id="schematic-current-heading-line"' in html
    assert 'id="schematic-rudder-line"' in html
    # HTML uses the canonical all-Unicode form `α = δ - ψ` (ASCII hyphen, per preserved
    # sample-calc list). Reject ASCII-spelled-out `alpha` form regression.
    assert "α = δ - ψ" in html
    assert "alpha = δ - ψ" not in html
    assert "bow-to-port" in html.lower()

    # Pass A canonical 6-section layout (Introduction / Design Data / Axes / Load due to current
    # / Load due to rudder / Limitations); MD reads at freshman-engineering-grad level.
    assert "## 1. Introduction" in report
    assert "## 2. Design Data & Assumptions" in report
    assert "## 3. Axes & Sign Conventions" in report
    assert "## 4. Load Due to Current" in report
    assert "## 5. Load Due to Rudder" in report
    assert "## 6. Limitations" in report
    assert "longitudinal" in report.lower()
    assert "transverse" in report.lower()
    assert "yaw moment" in report.lower()
    assert "screening calculation" in report.lower()
    assert "3.08" in report  # default speed retained
    assert "placeholder heading functions" not in report.lower()
    assert "not vessel-specific to sirocco" in report.lower() or "not ship-specific sirocco" in report.lower()

    assert parsed["metadata"]["chart_default_current_speed_kn"] == pytest.approx(3.08)
    assert parsed["metadata"]["chart_default_extra_speed_included"] is False
    assert parsed["summary"]["row_count"] == 990
    assert "local_to_ship_transform" in provenance
    assert "placeholder" not in provenance["ocimf_first_cut_current_loads"]["coefficient_caveat"].lower()
    assert "default_speed_policy" in provenance
    assert "scope_exclusions" in provenance


def test_generated_csv_has_expected_rows(tmp_path):
    result = run_b1528_current_heading_rudder_report()
    manifest = write_b1528_current_heading_rudder_report(result, tmp_path)
    csv_text = Path(manifest["csv"]).read_text(encoding="utf-8")

    assert csv_text.splitlines()[0].startswith("case_id,current_speed_kn")
    assert len(csv_text.splitlines()) == 991
    assert ",3.08,False,5.0,28.0," in csv_text
