from __future__ import annotations

from tests.solvers.orcaflex.reporting.fixture_helpers import (
    generate_minimal_fixture_report,
    minimal_fixture_snapshot_path,
)
from tests.solvers.orcaflex.reporting.snapshot_helpers import (
    load_normalized_snapshot,
    normalize_report_html,
    normalized_report_text,
)


def test_minimal_fixture_snapshot_baseline_exists() -> None:
    assert minimal_fixture_snapshot_path().exists()


def test_normalize_report_html_removes_brittle_content() -> None:
    html_text = """
    <html>
      <body>
        <div>2026-04-22T12:34:56Z</div>
        <div>/mnt/local-analysis/workspace-hub/example</div>
      </body>
    </html>
    """
    normalized = normalize_report_html(html_text)
    assert "2026-04-22T12:34:56Z" not in normalized
    assert "/mnt/local-analysis/workspace-hub/example" not in normalized
    assert "<TIMESTAMP>" in normalized
    assert "<PATH>" in normalized


def test_minimal_fixture_report_matches_snapshot(tmp_path) -> None:
    report_path = generate_minimal_fixture_report(tmp_path, include_plotlyjs=False)
    actual = normalized_report_text(report_path)
    expected = load_normalized_snapshot(minimal_fixture_snapshot_path())
    assert actual == expected


def test_snapshot_scope_is_structural_not_incidental(tmp_path) -> None:
    report_path = generate_minimal_fixture_report(tmp_path, include_plotlyjs=False)
    actual = normalized_report_text(report_path)
    assert "<html" in actual.lower()
    assert 'id="model-overview"' in actual
    assert 'id="geometry"' in actual
    assert 'id="materials"' in actual
    assert 'id="boundary-conditions"' in actual
    assert 'id="mesh"' in actual
    assert 'id="loads"' in actual


def test_mooring_with_raos_snapshot_is_deferred_until_minimal_fixture_stable() -> None:
    # TODO: replace with real extension test only after minimal fixture snapshot path is stable.
    assert True
