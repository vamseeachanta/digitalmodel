# ABOUTME: Tests for B1528 SIROCCO preliminary Nomoto time-trace report workflow.
# ABOUTME: Locks source-gap caveats, rudder-local inflow diagnostics, and generated artifacts.

from pathlib import Path

import pytest

from digitalmodel.naval_architecture.b1528_sirocco_time_trace import (
    load_packaged_b1528_time_trace_config,
    run_b1528_time_trace_report,
    simulate_b1528_time_trace,
    write_b1528_time_trace_report,
)


def test_b1528_time_trace_yaml_loads():
    cfg = load_packaged_b1528_time_trace_config()

    assert cfg.case_id == "b1528_sirocco_time_trace"
    assert cfg.source_pack_issue.endswith("/2569")
    assert cfg.static_report_issue.endswith("/2570")
    assert cfg.speed_kn == pytest.approx(2.5)
    assert cfg.nomoto_k_per_s > 0
    assert cfg.nomoto_t_s > 0
    assert cfg.rudder_x_from_cg_m < 0
    assert "source-gap" in " ".join(cfg.limitations).lower()


def test_zero_rudder_straight_trace():
    cfg = load_packaged_b1528_time_trace_config()
    zero_cfg = cfg.with_updates(rudder_angle_deg=0.0, duration_s=120.0)

    result = simulate_b1528_time_trace(zero_cfg)
    final = result["rows"][-1]

    assert abs(final["yaw_rate_deg_s"]) < 1.0e-9
    assert abs(final["heading_deg"]) < 1.0e-9
    assert abs(final["y_m"]) < 1.0e-9
    assert abs(final["effective_rudder_angle_deg"]) < 1.0e-9


def test_positive_negative_symmetry():
    cfg = load_packaged_b1528_time_trace_config().with_updates(duration_s=180.0)

    pos = simulate_b1528_time_trace(cfg.with_updates(rudder_angle_deg=1.0))["rows"][-1]
    neg = simulate_b1528_time_trace(cfg.with_updates(rudder_angle_deg=-1.0))["rows"][-1]

    assert pos["heading_deg"] == pytest.approx(-neg["heading_deg"], rel=2.0e-3, abs=2.0e-3)
    assert pos["yaw_rate_deg_s"] == pytest.approx(-neg["yaw_rate_deg_s"], rel=2.0e-3, abs=2.0e-3)
    assert pos["y_m"] == pytest.approx(-neg["y_m"], rel=2.0e-3, abs=2.0e-3)
    assert pos["diagnostic_yaw_moment_kN_m"] == pytest.approx(-neg["diagnostic_yaw_moment_kN_m"], rel=2.0e-3, abs=2.0e-3)


def test_effective_attack_feedback_changes():
    cfg = load_packaged_b1528_time_trace_config().with_updates(rudder_angle_deg=1.0, duration_s=180.0)
    rows = simulate_b1528_time_trace(cfg)["rows"]

    initial_alpha = rows[0]["effective_rudder_angle_deg"]
    final_alpha = rows[-1]["effective_rudder_angle_deg"]
    final_local_speed = rows[-1]["rudder_local_speed_m_s"]

    assert initial_alpha == pytest.approx(1.0)
    assert abs(final_alpha - cfg.rudder_angle_deg) > 0.05
    assert final_local_speed > cfg.speed_m_s


def test_integration_step_sensitivity():
    cfg = load_packaged_b1528_time_trace_config().with_updates(rudder_angle_deg=1.0, duration_s=240.0)

    coarse = simulate_b1528_time_trace(cfg.with_updates(dt_s=2.0))["metrics"]
    fine = simulate_b1528_time_trace(cfg.with_updates(dt_s=1.0))["metrics"]

    assert fine["final_heading_deg"] == pytest.approx(coarse["final_heading_deg"], rel=0.03)
    assert fine["advance_m"] == pytest.approx(coarse["advance_m"], rel=0.03)
    assert fine["tactical_diameter_m"] == pytest.approx(coarse["tactical_diameter_m"], rel=0.06, abs=1.0)


def test_report_contains_interactive_charts(tmp_path):
    cfg = load_packaged_b1528_time_trace_config().with_updates(duration_s=120.0)
    result = run_b1528_time_trace_report(cfg)
    manifest = write_b1528_time_trace_report(result, tmp_path)

    for key in ["csv", "json", "provenance", "markdown_report", "html_report", "manifest"]:
        assert Path(manifest[key]).exists(), key

    html = Path(manifest["html_report"]).read_text(encoding="utf-8")
    assert "Plotly.newPlot('trajectory-chart'" in html
    assert "Plotly.newPlot('heading-chart'" in html
    assert "Plotly.newPlot('yaw-rate-chart'" in html
    assert "Plotly.newPlot('alpha-chart'" in html
    assert "Plotly.newPlot('moment-chart'" in html
    assert "benchmark-source-gap" in html

    report = Path(manifest["markdown_report"]).read_text(encoding="utf-8")
    assert "rudder-local inflow feedback" in report
    assert "diagnostic only" in report
    assert "source-gap" in report


def test_no_mmg_or_compliance_overclaim(tmp_path):
    cfg = load_packaged_b1528_time_trace_config().with_updates(duration_s=60.0)
    manifest = write_b1528_time_trace_report(run_b1528_time_trace_report(cfg), tmp_path)

    text = Path(manifest["markdown_report"]).read_text(encoding="utf-8").lower()
    html = Path(manifest["html_report"]).read_text(encoding="utf-8").lower()
    combined = text + html

    assert "not a full mmg simulation" in combined
    assert "not an incident reconstruction" in combined
    assert "not an imo compliance assessment" in combined
    assert "no class compliance conclusion" in combined
    assert "diagnostic only" in combined
