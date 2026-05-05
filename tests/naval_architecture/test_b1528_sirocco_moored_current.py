# ABOUTME: Tests for B1528 SIROCCO moored-current rudder force component report.
# ABOUTME: Locks 3.5 kn current inputs, Cr=1.0 logic, COG components, and report artifacts.

import json
import math
from pathlib import Path

import pytest

from digitalmodel.naval_architecture.b1528_sirocco_moored_current_report import (
    KNOT_TO_M_PER_S,
    load_packaged_b1528_moored_current_config,
    run_b1528_moored_current_report,
    write_b1528_moored_current_report,
)


def test_b1528_moored_current_yaml_loads():
    cfg = load_packaged_b1528_moored_current_config()

    assert cfg.case_id == "b1528_sirocco_moored_current_rudder_forces"
    assert "SIROCCO" in cfg.aliases
    assert "Sorrocco" in cfg.aliases
    assert cfg.source_pack_issue.endswith("/2569")
    assert cfg.moored_current_report_issue.endswith("/2642")
    assert cfg.static_report_issue.endswith("/2570")
    assert cfg.time_trace_report_issue.endswith("/2571")
    assert cfg.ship_sog_kn == pytest.approx(0.0)
    assert cfg.current_speed_kn == pytest.approx(3.5)
    assert cfg.current_speed_m_s == pytest.approx(3.5 * KNOT_TO_M_PER_S)
    assert cfg.rudder_angles_deg == (1.0, 2.0, 3.0, 4.0, 5.0)
    assert cfg.prop_rotation_factor == pytest.approx(1.0)
    assert "Cr=1.0" in cfg.prop_rotation_factor_logic


def test_package_level_exports_available():
    import digitalmodel.naval_architecture as naval_architecture

    assert callable(naval_architecture.load_packaged_b1528_moored_current_config)
    assert callable(naval_architecture.run_b1528_moored_current_report)
    assert callable(naval_architecture.write_b1528_moored_current_report)


def test_moored_current_force_component_rows_are_symmetric():
    result = run_b1528_moored_current_report()
    rows = result["rows"]

    assert len(rows) == 10
    assert {row["side"] for row in rows} == {"port", "starboard"}

    for angle in (1.0, 2.0, 3.0, 4.0, 5.0):
        port = next(
            row
            for row in rows
            if row["side"] == "port" and row["rudder_angle_magnitude_deg"] == angle
        )
        starboard = next(
            row
            for row in rows
            if row["side"] == "starboard"
            and row["rudder_angle_magnitude_deg"] == angle
        )

        assert port["force_x_surge_downstream_N"] == pytest.approx(
            starboard["force_x_surge_downstream_N"]
        )
        assert port["force_y_sway_port_N"] == pytest.approx(
            -starboard["force_y_sway_port_N"]
        )
        assert port["moment_n_yaw_bow_port_kN_m"] == pytest.approx(
            -starboard["moment_n_yaw_bow_port_kN_m"]
        )
        assert port["resultant_horizontal_force_N"] == pytest.approx(
            starboard["resultant_horizontal_force_N"]
        )
        assert port["force_z_heave_N"] == pytest.approx(0.0)
        assert port["moment_k_roll_Nm"] == pytest.approx(0.0)
        assert port["moment_m_pitch_Nm"] == pytest.approx(0.0)
        assert port["mooring_reaction_x_upstream_N"] == pytest.approx(
            -port["force_x_surge_downstream_N"]
        )
        assert port["mooring_reaction_y_N"] == pytest.approx(
            -port["force_y_sway_port_N"]
        )
        assert port["mooring_reaction_n_Nm"] == pytest.approx(
            -port["moment_n_yaw_bow_port_Nm"]
        )


def test_sample_working_example_matches_formula():
    result = run_b1528_moored_current_report()
    sample = result["sample_working_example"]
    alpha = math.radians(1.0)
    speed_m_s = 3.5 * KNOT_TO_M_PER_S
    base_force = 600.0 * 44.93956319369854 * speed_m_s**2 * 1.0
    expected_x = base_force * math.sin(alpha) ** 2
    expected_y = base_force * math.sin(alpha) * math.cos(alpha)
    expected_n = expected_y * 135.3 / 1000.0

    assert sample["data_point"] == "moored current, 3.5 kn, port rudder 1 deg, Cr=1.0"
    assert sample["current_speed_m_s"] == pytest.approx(1.80054)
    assert sample["base_force_N"] == pytest.approx(base_force)
    assert sample["normal_force_N"] == pytest.approx(base_force * math.sin(alpha))
    assert sample["force_x_surge_downstream_N"] == pytest.approx(expected_x)
    assert sample["force_y_sway_port_N"] == pytest.approx(expected_y)
    assert sample["moment_n_yaw_bow_port_kN_m"] == pytest.approx(expected_n)
    assert sample["moment_n_yaw_bow_port_kN_m"] == pytest.approx(206.382376875536)


def test_report_outputs_include_professional_sections_and_traceability(tmp_path):
    result = run_b1528_moored_current_report()
    manifest = write_b1528_moored_current_report(result, tmp_path)

    for key in ["csv", "json", "provenance", "markdown_report", "html_report", "manifest"]:
        assert Path(manifest[key]).exists(), key

    html = Path(manifest["html_report"]).read_text(encoding="utf-8")
    report = Path(manifest["markdown_report"]).read_text(encoding="utf-8")

    assert 'class="report-shell"' in html
    assert 'class="report-page"' in html
    assert 'class="chart"' in html
    assert "@page { size: A4 landscape; margin: 12mm; }" in html
    assert "const STANDARD_CHART_HEIGHT = 430" in html
    assert "const CHART_CONFIG = {responsive: true, displaylogo: false}" in html
    assert "Design data" in html
    assert "Analysis methodology and assumptions" in html
    assert "Force components at COG" in html
    assert "References" in html
    assert "current 3.5 kn" in html
    assert "Cr=1.0" in html
    assert "Cr=1.0" in report
    assert "workspace-hub #2642" in report
    assert "https://github.com/vamseeachanta/workspace-hub/issues/2569" in report
    assert "https://github.com/vamseeachanta/workspace-hub/issues/2570" in report
    assert "https://github.com/vamseeachanta/workspace-hub/issues/2571" in report
    assert "Prepared for engineer review on 2026-05-06" in report
    assert "Sample working example" in report
    assert "hull current force" in report.lower()
    assert "not included" in report.lower()
    assert "class compliance" not in report.lower().replace("no class compliance", "")
    assert "Plotly.newPlot('sway-chart'" in html
    assert "Plotly.newPlot('yaw-chart'" in html
    assert "Plotly.newPlot('surge-chart'" in html
    assert "Plotly.newPlot('resultant-chart'" in html
    assert "Plotly.newPlot('sample-chart'" in html
    assert "<th>X (N)</th><th>Y (N)</th><th>Z (N)</th>" in html
    assert "<th>K (N-m)</th><th>M (N-m)</th><th>N (kN-m)</th>" in html

    parsed = json.loads(Path(manifest["json"]).read_text(encoding="utf-8"))
    assert parsed["metadata"]["review_target_date"] == "2026-05-06"
    assert parsed["metadata"]["design_data"]["current_speed_kn"] == pytest.approx(3.5)
    assert parsed["metadata"]["design_data"]["prop_rotation_factor"] == pytest.approx(1.0)
    assert parsed["metadata"]["traceability_links"]["moored_current_report_issue"].endswith(
        "/2642"
    )
    assert "sample_working_example" in parsed

    provenance = json.loads(Path(manifest["provenance"]).read_text(encoding="utf-8"))
    assert "analysis_assumptions" in provenance
    assert provenance["traceability_links"]["durable_moored_current_report"].endswith(
        "b1528-sirocco-moored-current-report.md"
    )
