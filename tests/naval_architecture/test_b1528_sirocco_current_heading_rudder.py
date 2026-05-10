# ABOUTME: Tests for B1528 SIROCCO current-heading/rudder force-component sweep.
# ABOUTME: Locks grid contract, ship-fixed transform, chart controls, and report artifacts.

import json
import math
from pathlib import Path

import pytest

from digitalmodel.naval_architecture.b1528_sirocco_current_heading_rudder_report import (
    KNOT_TO_M_PER_S,
    load_packaged_b1528_current_heading_rudder_config,
    run_b1528_current_heading_rudder_report,
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


def test_yaml_loads_exact_sweep_and_default_contract():
    cfg = load_packaged_b1528_current_heading_rudder_config()

    assert cfg.case_id == "b1528_sirocco_current_heading_rudder_forces"
    assert "SIROCCO" in cfg.aliases
    assert "Sorrocco" in cfg.aliases
    assert cfg.current_speeds_kn == (1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5)
    assert cfg.chart_default_current_speed_kn == pytest.approx(4.56)
    assert cfg.heading_offsets_deg == tuple(float(value) for value in range(-10, 11))
    assert cfg.rudder_angles_deg == tuple(float(value) for value in range(-10, 11))
    assert cfg.prop_rotation_factor == pytest.approx(1.0)
    assert cfg.lbp_m == pytest.approx(225.5)
    assert cfg.yaw_lever_m == pytest.approx(135.3)
    assert cfg.rudder_area_m2 == pytest.approx(44.93956319369854)
    assert "extra chart-default" in cfg.default_speed_policy.lower()
    assert "local-to-ship" in cfg.force_convention["transform"].lower()


def test_package_level_exports_available():
    import digitalmodel.naval_architecture as naval_architecture

    assert callable(naval_architecture.load_packaged_b1528_current_heading_rudder_config)
    assert callable(naval_architecture.run_b1528_current_heading_rudder_report)
    assert callable(naval_architecture.write_b1528_current_heading_rudder_report)


def test_full_sweep_row_counts_and_extra_default_plane_flags():
    result = run_b1528_current_heading_rudder_report()
    rows = result["rows"]

    assert len(rows) == 3969
    assert sum(not row["is_chart_default_extra_speed"] for row in rows) == 3528
    assert sum(row["is_chart_default_extra_speed"] for row in rows) == 441
    assert sorted({row["current_speed_kn"] for row in rows}) == [
        1.0,
        1.5,
        2.0,
        2.5,
        3.0,
        3.5,
        4.0,
        4.5,
        4.56,
    ]
    assert result["summary"]["row_count"] == 3969
    assert result["summary"]["requested_engineering_row_count"] == 3528
    assert result["summary"]["extra_default_row_count"] == 441
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
    row = _find_row(result["rows"], 4.56, 5.0, 5.0)

    assert row["effective_rudder_inflow_angle_deg"] == pytest.approx(0.0)
    assert row["force_x_local_downstream_N"] == pytest.approx(0.0, abs=1e-9)
    assert row["force_y_local_port_of_current_N"] == pytest.approx(0.0, abs=1e-9)
    assert row["force_x_ship_N"] == pytest.approx(0.0, abs=1e-9)
    assert row["force_y_ship_port_N"] == pytest.approx(0.0, abs=1e-9)
    assert row["moment_n_yaw_bow_port_Nm"] == pytest.approx(0.0, abs=1e-9)
    assert "not total hull current load" in result["metadata"]["zero_effective_angle_note"].lower()


def test_centerline_regression_matches_existing_fixed_report_formula():
    result = run_b1528_current_heading_rudder_report()
    row = _find_row(result["rows"], 3.5, 0.0, 5.0)
    alpha = math.radians(5.0)
    speed_m_s = 3.5 * KNOT_TO_M_PER_S
    base_force = 600.0 * 44.93956319369854 * speed_m_s**2 * 1.0
    expected_x = base_force * math.sin(alpha) ** 2
    expected_y = base_force * math.sin(alpha) * math.cos(alpha)

    assert row["base_force_N"] == pytest.approx(base_force)
    assert row["force_x_local_downstream_N"] == pytest.approx(expected_x)
    assert row["force_y_local_port_of_current_N"] == pytest.approx(expected_y)
    assert row["force_x_ship_N"] == pytest.approx(expected_x)
    assert row["force_y_ship_port_N"] == pytest.approx(expected_y)
    assert row["moment_n_yaw_bow_port_kN_m"] == pytest.approx(expected_y * 135.3 / 1000.0)


def test_sign_cases_for_positive_and_negative_rudder():
    rows = run_b1528_current_heading_rudder_report()["rows"]
    port = _find_row(rows, 4.56, 0.0, 10.0)
    starboard = _find_row(rows, 4.56, 0.0, -10.0)

    assert port["force_y_ship_port_N"] > 0.0
    assert port["moment_n_yaw_bow_port_Nm"] > 0.0
    assert starboard["force_y_ship_port_N"] < 0.0
    assert starboard["moment_n_yaw_bow_port_Nm"] < 0.0
    assert port["force_x_local_downstream_N"] == pytest.approx(starboard["force_x_local_downstream_N"])
    assert port["force_x_local_downstream_N"] >= 0.0


def test_heading_rudder_interaction_rotates_local_force_to_ship_frame():
    rows = run_b1528_current_heading_rudder_report()["rows"]
    oblique = _find_row(rows, 4.56, 5.0, 10.0)
    centerline_equivalent_alpha = _find_row(rows, 4.56, 0.0, 5.0)

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
    row = _find_row(rows, 2.5, -7.0, 3.0)
    psi = math.radians(-7.0)
    expected_x = row["force_x_local_downstream_N"] * math.cos(psi) - row[
        "force_y_local_port_of_current_N"
    ] * math.sin(psi)
    expected_y = row["force_x_local_downstream_N"] * math.sin(psi) + row[
        "force_y_local_port_of_current_N"
    ] * math.cos(psi)

    assert row["force_x_ship_N"] == pytest.approx(expected_x)
    assert row["force_y_ship_port_N"] == pytest.approx(expected_y)
    assert row["moment_n_yaw_bow_port_Nm"] == pytest.approx(expected_y * 135.3)
    assert row["mooring_reaction_x_N"] == pytest.approx(-expected_x)
    assert row["mooring_reaction_y_N"] == pytest.approx(-expected_y)
    assert row["mooring_reaction_n_Nm"] == pytest.approx(-expected_y * 135.3)


def test_speed_squared_scaling_for_same_heading_and_rudder():
    rows = run_b1528_current_heading_rudder_report()["rows"]
    slow = _find_row(rows, 2.0, -3.0, 9.0)
    fast = _find_row(rows, 4.0, -3.0, 9.0)

    for key in [
        "base_force_N",
        "force_x_local_downstream_N",
        "force_y_local_port_of_current_N",
        "force_x_ship_N",
        "force_y_ship_port_N",
        "moment_n_yaw_bow_port_Nm",
        "resultant_horizontal_force_N",
    ]:
        assert fast[key] / slow[key] == pytest.approx(4.0)


def test_formula_sample_for_default_speed():
    result = run_b1528_current_heading_rudder_report()
    sample = result["sample_working_example"]
    alpha = math.radians(1.0)
    speed_m_s = 4.56 * KNOT_TO_M_PER_S
    base_force = 600.0 * 44.93956319369854 * speed_m_s**2 * 1.0
    expected_x = base_force * math.sin(alpha) ** 2
    expected_y = base_force * math.sin(alpha) * math.cos(alpha)

    assert sample["data_point"] == "chart-default 4.56 kn, heading 0 deg, rudder +1 deg, Cr=1.0"
    assert sample["current_speed_m_s"] == pytest.approx(speed_m_s)
    assert sample["base_force_N"] == pytest.approx(base_force)
    assert sample["normal_force_N"] == pytest.approx(base_force * math.sin(alpha))
    assert sample["force_x_ship_N"] == pytest.approx(expected_x)
    assert sample["force_y_ship_port_N"] == pytest.approx(expected_y)
    assert sample["moment_n_yaw_bow_port_kN_m"] == pytest.approx(expected_y * 135.3 / 1000.0)


def test_report_outputs_include_dropdown_chart_contract_and_provenance(tmp_path):
    result = run_b1528_current_heading_rudder_report()
    manifest = write_b1528_current_heading_rudder_report(result, tmp_path)

    for key in ["csv", "json", "provenance", "markdown_report", "html_report", "manifest"]:
        assert Path(manifest[key]).exists(), key

    html = Path(manifest["html_report"]).read_text(encoding="utf-8")
    report = Path(manifest["markdown_report"]).read_text(encoding="utf-8")
    parsed = json.loads(Path(manifest["json"]).read_text(encoding="utf-8"))
    provenance = json.loads(Path(manifest["provenance"]).read_text(encoding="utf-8"))

    assert 'id="current-speed-select"' in html
    assert '<option value="4.56" selected>4.56 kn' in html
    for speed in ["1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "4.56"]:
        assert f'<option value="{speed}"' in html
    assert 'id="rudder-angle-select"' in html
    assert '<option value="10.0" selected>10.0 deg</option>' in html
    for angle in range(-10, 11):
        assert f'<option value="{float(angle):.1f}"' in html
    assert 'id="force-components-chart"' in html
    assert 'id="yaw-moment-heatmap"' in html
    assert "Plotly.newPlot('force-components-chart'" in html
    assert "Plotly.newPlot('yaw-moment-heatmap'" in html
    assert "Ship-fixed force component (kN)" in html
    assert "N_ship yaw moment (kN-m)" in html
    assert "is_chart_default_extra_speed" in html
    assert "4.56 kn is an extra chart-default case" in html
    assert "Chart 1" in html and "Chart 2" in html

    assert "heading/rudder effective-angle convention" in report.lower()
    assert "local-to-ship transform" in report.lower()
    assert "4.56 kn is an extra chart-default case" in report
    assert "hull current loads" in report.lower()
    assert "not a validated oblique-current hull/rudder interaction model" in report.lower()

    assert parsed["metadata"]["chart_default_current_speed_kn"] == pytest.approx(4.56)
    assert parsed["metadata"]["chart_default_extra_speed_included"] is True
    assert parsed["summary"]["row_count"] == 3969
    assert "local_to_ship_transform" in provenance
    assert "default_speed_policy" in provenance
    assert "scope_exclusions" in provenance


def test_generated_csv_has_expected_rows(tmp_path):
    result = run_b1528_current_heading_rudder_report()
    manifest = write_b1528_current_heading_rudder_report(result, tmp_path)
    csv_text = Path(manifest["csv"]).read_text(encoding="utf-8")

    assert csv_text.splitlines()[0].startswith("case_id,current_speed_kn")
    assert len(csv_text.splitlines()) == 3970
    assert ",4.56,True," in csv_text
