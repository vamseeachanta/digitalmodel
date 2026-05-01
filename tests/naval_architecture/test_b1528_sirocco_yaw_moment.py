# ABOUTME: TDD tests for B1528 SIROCCO static yaw-moment report inputs.
# ABOUTME: Locks source-pack values, workbook-regression semantics, and report artifacts for issue #2570.

import json
import math
from importlib import resources
from pathlib import Path

import pytest
import yaml


def test_packaged_b1528_yaml_loads_with_source_refs():
    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "b1528_sirocco_yaw_moment.yml"
    )
    payload = yaml.safe_load(resource.read_text(encoding="utf-8"))

    assert payload["case"]["id"] == "b1528_sirocco_static_yaw_moment"
    assert "SIROCCO" in payload["case"]["aliases"]
    assert "Sorrocco" in payload["case"]["aliases"]
    assert payload["source_pack"]["issue"] == "https://github.com/vamseeachanta/workspace-hub/issues/2569"
    assert payload["calculation_modes"] == ["workbook_regression", "digitalmodel_static_yaw"]


def test_b1528_required_values_from_source_pack():
    from digitalmodel.naval_architecture.b1528_sirocco_yaw_report import (
        load_packaged_b1528_yaw_config,
    )

    config = load_packaged_b1528_yaw_config()

    assert config.lbp_m == pytest.approx(225.5)
    assert config.rudder_area_m2 == pytest.approx(44.93956319369854)
    assert config.legacy_yaw_lever_m == pytest.approx(135.3)
    assert config.workbook_beta == pytest.approx(600.0)
    assert config.speeds_kn == [0.0, 1.0, 2.5, 5.0]
    assert -1.0 in config.rudder_angles_deg
    assert 1.0 in config.rudder_angles_deg


def test_b1528_workbook_regression_operating_points_match_source_pack():
    from digitalmodel.naval_architecture.b1528_sirocco_yaw_report import (
        load_packaged_b1528_yaw_config,
        run_b1528_static_yaw_report,
    )

    result = run_b1528_static_yaw_report(load_packaged_b1528_yaw_config())
    workbook_rows = [
        row for row in result["rows"] if row["calculation_mode"] == "workbook_regression"
    ]
    plus = next(
        row
        for row in workbook_rows
        if row["speed_kn"] == 2.5 and row["rudder_angle_deg"] == 1.0 and row["rotation_case"] == "port"
    )
    minus = next(
        row
        for row in workbook_rows
        if row["speed_kn"] == 2.5 and row["rudder_angle_deg"] == -1.0 and row["rotation_case"] == "stbd"
    )

    assert plus["yaw_moment_kN_m"] == pytest.approx(112.158527, rel=2e-5)
    assert minus["yaw_moment_kN_m"] == pytest.approx(-98.467815, rel=2e-5)
    assert plus["workbook_force_for_yaw_moment"] == "Fn"
    assert plus["normal_force_N"] > plus["transverse_force_N"]
    assert math.copysign(1.0, minus["yaw_moment_kN_m"]) == -1.0


def test_digitalmodel_static_yaw_rows_are_separately_labeled():
    from digitalmodel.naval_architecture.b1528_sirocco_yaw_report import (
        load_packaged_b1528_yaw_config,
        run_b1528_static_yaw_report,
    )

    result = run_b1528_static_yaw_report(load_packaged_b1528_yaw_config())
    modes = {row["calculation_mode"] for row in result["rows"]}
    assert modes == {"workbook_regression", "digitalmodel_static_yaw"}
    digital_rows = [row for row in result["rows"] if row["calculation_mode"] == "digitalmodel_static_yaw"]
    assert digital_rows
    assert all(row["lever_mapping"] == "legacy_0_6_lbp_mapped_for_comparison_only" for row in digital_rows)


def test_interactive_report_outputs_and_no_compliance_overclaim(tmp_path):
    from digitalmodel.naval_architecture.b1528_sirocco_yaw_report import (
        load_packaged_b1528_yaw_config,
        run_b1528_static_yaw_report,
        write_b1528_static_yaw_report,
    )

    result = run_b1528_static_yaw_report(load_packaged_b1528_yaw_config())
    manifest = write_b1528_static_yaw_report(result, tmp_path)

    for key in ["csv", "json", "provenance", "manifest", "html_report", "markdown_report"]:
        assert key in manifest
        assert manifest[key]
    html = (tmp_path / "b1528_sirocco_yaw_moment_report.html").read_text(encoding="utf-8")
    md = (tmp_path / "b1528_sirocco_yaw_moment_report.md").read_text(encoding="utf-8")
    assert "Plotly.newPlot" in html
    assert "2.5 kn ±1° operating-point table" in md
    assert "not an IMO compliance assessment" in md
    assert "not a full MMG" in md
    assert "class compliance" not in md.lower().replace("no class compliance", "")

    payload = json.loads((tmp_path / "b1528_sirocco_yaw_moment_results.json").read_text())
    assert payload["metadata"]["source_pack_issue"].endswith("/2569")


def test_packaged_b1528_yaml_declared_as_package_data():
    pyproject = Path("pyproject.toml").read_text(encoding="utf-8")
    assert 'digitalmodel = ["naval_architecture/data/*.yml"]' in pyproject
