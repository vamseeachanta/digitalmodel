from __future__ import annotations

from tests.solvers.orcaflex.reporting.fixture_helpers import (
    fixture_snapshot_path,
    generate_fixture_report,
)
from tests.solvers.orcaflex.reporting.snapshot_helpers import (
    load_normalized_snapshot,
    normalized_report_text,
)


def test_fpso_snapshot_baseline_exists() -> None:
    assert fixture_snapshot_path("fpso_turret").exists()


def test_fpso_report_matches_snapshot(tmp_path) -> None:
    report_path = generate_fixture_report("fpso_turret", tmp_path, include_plotlyjs=False)
    actual = normalized_report_text(report_path)
    expected = load_normalized_snapshot(fixture_snapshot_path("fpso_turret"))
    assert actual == expected


def test_fpso_snapshot_contains_expected_structural_markers(tmp_path) -> None:
    report_path = generate_fixture_report("fpso_turret", tmp_path, include_plotlyjs=False)
    actual = normalized_report_text(report_path)
    assert 'id="model-overview"' in actual
    assert 'id="geometry"' in actual
    assert 'id="materials"' in actual
    assert 'id="boundary-conditions"' in actual
    assert 'id="mesh"' in actual
    assert 'id="loads"' in actual
