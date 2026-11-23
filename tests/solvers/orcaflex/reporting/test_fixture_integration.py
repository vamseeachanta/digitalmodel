from __future__ import annotations

from pathlib import Path

from tests.solvers.orcaflex.reporting.fixture_helpers import (
    generate_minimal_fixture_report,
    load_minimal_fixture_metadata,
    minimal_fixture_metadata_path,
)


REQUIRED_TOP_LEVEL_KEYS = {
    "schema_version",
    "fixture",
    "provenance",
    "model",
    "general",
    "environment",
    "objects",
    "report_summary",
    "normalization",
}


def test_minimal_fixture_metadata_file_exists() -> None:
    assert minimal_fixture_metadata_path().exists()


def test_minimal_fixture_metadata_matches_expected_schema() -> None:
    metadata = load_minimal_fixture_metadata()
    assert REQUIRED_TOP_LEVEL_KEYS.issubset(metadata.keys())
    assert isinstance(metadata["model"]["object_counts_by_type"], dict)
    assert isinstance(metadata["objects"]["vessels"], list)
    assert isinstance(metadata["objects"]["lines"], list)


def test_minimal_fixture_metadata_has_stable_core_values() -> None:
    metadata = load_minimal_fixture_metadata()
    assert metadata["fixture"]["name"] == "minimal_test"
    assert metadata["model"]["state"] == 4
    assert metadata["general"]["simulation_duration_s"] == 10.0
    assert metadata["environment"]["water_depth_m"] == 100.0
    assert metadata["model"]["object_counts_by_type"]["Vessel"] == 1
    assert metadata["model"]["object_counts_by_type"]["Line"] == 1


def test_minimal_fixture_metadata_object_names_are_stable() -> None:
    metadata = load_minimal_fixture_metadata()
    vessel_names = [item["name"] for item in metadata["objects"]["vessels"]]
    line_names = [item["name"] for item in metadata["objects"]["lines"]]
    assert "TestVessel" in vessel_names
    assert "MooringLine1" in line_names


def test_build_report_from_minimal_fixture_metadata(tmp_path: Path) -> None:
    report_path = generate_minimal_fixture_report(tmp_path, include_plotlyjs=False)
    assert report_path.exists()
    html_text = report_path.read_text(encoding="utf-8")
    assert "minimal_test" in html_text
    assert "<html" in html_text.lower()


def test_generated_report_from_minimal_fixture_contains_expected_sections(tmp_path: Path) -> None:
    report_path = generate_minimal_fixture_report(tmp_path, include_plotlyjs=False)
    html_text = report_path.read_text(encoding="utf-8")
    assert 'id="model-overview"' in html_text
    assert 'id="geometry"' in html_text
    assert 'id="materials"' in html_text
    assert 'id="boundary-conditions"' in html_text
    assert 'id="mesh"' in html_text
    assert 'id="loads"' in html_text


def test_minimal_fixture_metadata_excludes_volatile_fields() -> None:
    metadata = load_minimal_fixture_metadata()
    serialized = str(metadata)
    assert "generated_at" not in serialized
    assert "C:\\" not in serialized
    assert "/Users/" not in serialized


def test_report_summary_structure_type_guess_is_supported() -> None:
    metadata = load_minimal_fixture_metadata()
    assert metadata["report_summary"]["structure_type"] in {
        "riser",
        "pipeline",
        "jumper",
        "mooring",
        "installation",
    }
