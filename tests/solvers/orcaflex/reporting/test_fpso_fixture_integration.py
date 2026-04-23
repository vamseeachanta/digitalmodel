from __future__ import annotations

from tests.solvers.orcaflex.reporting.fixture_helpers import (
    build_report_from_metadata,
    fixture_metadata_path,
    generate_fixture_report,
    load_fpso_fixture_metadata,
)


def test_fpso_fixture_metadata_file_exists() -> None:
    assert fixture_metadata_path("fpso_turret").exists()


def test_fpso_fixture_metadata_has_grounded_core_values() -> None:
    metadata = load_fpso_fixture_metadata()
    assert metadata["fixture"]["name"] == "fpso_turret"
    assert metadata["environment"]["water_depth_m"] == 80.0
    assert metadata["environment"]["wave_type"] == "Dean stream"
    assert metadata["environment"]["current_speed_mps"] == 2.0
    assert metadata["model"]["object_counts_by_type"]["Vessel"] == 1
    assert metadata["model"]["object_counts_by_type"]["Line"] >= 4


def test_fpso_fixture_metadata_contains_expected_named_objects() -> None:
    metadata = load_fpso_fixture_metadata()
    vessel_names = [item["name"] for item in metadata["objects"]["vessels"]]
    buoy_names = [item["name"] for item in metadata["objects"]["buoys"]]
    line_names = [item["name"] for item in metadata["objects"]["lines"]]
    assert "Vessel" in vessel_names
    assert "Turret buoy" in buoy_names
    assert "Leg1" in line_names


def test_fpso_build_report_from_metadata() -> None:
    metadata = load_fpso_fixture_metadata()
    report = build_report_from_metadata(metadata)
    assert report.project_name == "c03_turret_moored_fpso"
    assert report.structure_id == "fpso_turret"
    assert report.structure_type == "mooring"
    assert report.geometry is not None
    assert report.materials is not None
    assert report.boundary_conditions is not None
    assert report.loads is not None
    assert report.mesh is not None


def test_fpso_report_generation_has_expected_sections(tmp_path) -> None:
    report_path = generate_fixture_report("fpso_turret", tmp_path, include_plotlyjs=False)
    html_text = report_path.read_text(encoding="utf-8")
    assert "c03_turret_moored_fpso" in html_text
    assert 'id="model-overview"' in html_text
    assert 'id="geometry"' in html_text
    assert 'id="materials"' in html_text
    assert 'id="boundary-conditions"' in html_text
    assert 'id="mesh"' in html_text
    assert 'id="loads"' in html_text
