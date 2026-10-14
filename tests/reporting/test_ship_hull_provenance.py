# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Ship-hull provenance correction retains historical results without recalculation."""

import hashlib
import json
import os
from pathlib import Path
import shutil
import sys
import subprocess

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts/reporting"))
from build_cp_review_portfolio import load_fixture, verify_pack
from cp_portfolio_storage import tree_digest
from cp_review_document import validate_comments

REPORTS = ROOT / "docs/domains/cathodic_protection/reports"


def _historical_candidate(tmp_path: Path) -> Path:
    """Exercise the original report bytes, independent of the corrected checkout."""
    candidate = tmp_path / "reports"
    shutil.copytree(REPORTS, candidate)
    entry = json.loads((ROOT / "scripts/reporting/cp_portfolio_inputs.json").read_text())["ships.yml"]
    basis = entry["provenance"]["inspected_revision"]
    environment = {key: value for key, value in os.environ.items() if not key.startswith("GIT_")}
    for target in (candidate / "cases/S04").iterdir():
        relative = (REPORTS / target.relative_to(candidate)).relative_to(ROOT)
        original = subprocess.run(
            ["git", "-C", str(ROOT), "show", f"{basis}:{relative.as_posix()}"],
            env=environment, check=True, capture_output=True,
        ).stdout
        target.write_bytes(original)
    return candidate


def test_ship_fixture_does_not_echo_an_ignored_capacity() -> None:
    """The alloy selection replaces the legacy scalar at its input source."""
    cfg = yaml.safe_load((ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/ships.yml").read_text())
    assert "anode_current_capacity" not in cfg["inputs"]["anode"]
    assert cfg["inputs"]["anode"]["alloy"] == "A1"


def test_client_derived_fixture_cannot_be_rebuilt_as_synthetic() -> None:
    """Release classification prevents a fresh synthetic numerical build."""
    with pytest.raises(ValueError, match="missing repository fixture release record"):
        load_fixture("ships.yml")


def test_provenance_correction_preserves_results_and_other_cases(tmp_path: Path) -> None:
    """Only the named case and its index evidence may change."""
    from correct_ship_hull_provenance import correct_ship_provenance

    candidate = _historical_candidate(tmp_path)
    before = tree_digest(candidate)
    correct_ship_provenance(candidate)
    after = tree_digest(candidate)
    for name, digest in before.items():
        if name.startswith("cases/") and not name.startswith("cases/S04/"):
            assert after[name] == digest
    assert after["cases/S04/results.json"] == before["cases/S04/results.json"]
    assert after["cases/S04/report_citations.json"] == before["cases/S04/report_citations.json"]
    case = candidate / "cases/S04"
    verify_pack(case)
    page = (case / "report.html").read_text()
    validate_comments(page, json.loads((case / "report.comments.json").read_text()))
    assert "inputs.anode.anode_current_capacity" not in page
    assert "inputs.anode.alloy" in page
    assert "client-derived" in page
    assert "Revision history" in page.split('<section id="review-2">')[0]
    assert "Hull cathodic protection analysis" in page
    row = next(row for row in json.loads((candidate / "coverage.json").read_text())["rows"] if row["id"] == "S04")
    assert row["pack_state"] == "blocked"
    assert row["source_class"] == "client_derived_pending_release"
    release = json.loads((case / "release.json").read_text())
    assert release["numerical_rebuild"] == "blocked_pending_source_qualification"
    fixture = ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/ships.yml"
    assert release["input_sha256"] == hashlib.sha256(fixture.read_bytes()).hexdigest()
    assert json.loads((candidate / "portfolio-receipt.json").read_text()) == tree_digest(candidate)


def test_correction_preserves_review_comments_by_refusing_overwrite(tmp_path: Path) -> None:
    """A report with existing reviewer comments remains untouched."""
    from correct_ship_hull_provenance import correct_ship_provenance
    from build_cp_review_portfolio import write_json, write_receipt

    candidate = _historical_candidate(tmp_path)
    case = candidate / "cases/S04"
    sidecar = json.loads((case / "report.comments.json").read_text())
    sidecar["prior_rounds"] = [{"revision": "earlier"}]
    write_json(case / "report.comments.json", sidecar)
    write_receipt(case)
    before = tree_digest(candidate)
    with pytest.raises(ValueError, match="review comments exist"):
        correct_ship_provenance(candidate)
    assert tree_digest(candidate) == before


def test_provenance_correction_is_idempotent(tmp_path: Path) -> None:
    """A second build changes neither report history nor recorded bytes."""
    from correct_ship_hull_provenance import correct_ship_provenance

    candidate = _historical_candidate(tmp_path)
    correct_ship_provenance(candidate)
    before = tree_digest(candidate)
    correct_ship_provenance(candidate)
    assert tree_digest(candidate) == before


def test_correction_refuses_a_damaged_pack(tmp_path: Path) -> None:
    """Checksum failure stops the correction before any other artifact changes."""
    from correct_ship_hull_provenance import correct_ship_provenance

    candidate = _historical_candidate(tmp_path)
    report = candidate / "cases/S04/report.html"
    report.write_text(report.read_text() + "<p>Changed</p>")
    before = tree_digest(candidate)
    with pytest.raises(ValueError, match="checksum mismatch"):
        correct_ship_provenance(candidate)
    assert tree_digest(candidate) == before
