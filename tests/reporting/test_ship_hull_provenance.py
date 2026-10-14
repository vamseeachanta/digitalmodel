# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Ship-hull provenance correction retains historical results without recalculation."""

import hashlib
import json
from pathlib import Path
import shutil
import re
import sys

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts/reporting"))
from build_cp_review_portfolio import load_fixture, verify_pack
from cp_portfolio_storage import tree_digest
from cp_review_document import validate_comments

REPORTS = ROOT / "docs/domains/cathodic_protection/reports"


def _historical_page(page: str) -> str:
    """Reconstruct prior presentation controls without accessing Git history."""
    history = re.search(r'<section id="revision-history">(.*?)</section>', page, re.S)
    assert history is not None
    prior = history.group(1).replace("<h2>Revision history</h2>", '<div class="cap">Revision history</div>')
    prior = re.sub(r"<tr><td>01</td>.*?</tr>", "", prior)
    page = page[:history.start()] + page[history.end():]
    page = re.sub(r'<section id="source-provenance">.*?</section>', "", page, flags=re.S)
    page = page.replace('<section id="review-1">', prior + '<section id="review-1">', 1)
    page = page.replace("Hull cathodic protection analysis", "CP module review: Ship hull")
    page = page.replace("<td>Revision</td><td>01</td>", "<td>Revision</td><td>00</td>")
    page = page.replace("Rev 01", "Rev 00")
    page = page.replace("<td>Date</td><td>2026-10-14</td>", "<td>Date</td><td>2026-10-03</td>")
    page = page.replace(
        "<tr><td>inputs.anode.alloy</td><td>A1</td></tr>",
        "<tr><td>inputs.anode.anode_current_capacity</td><td>retired input</td></tr>",
    )
    for key, title in (("revision-history", "Revision history"), ("source-provenance", "Provenance note")):
        page = page.replace(f'<li><a href="#{key}">{title}</a></li>', "")
    pattern = r'<script id="cp-review-config" type="application/json">(.*?)</script>'
    match = re.search(pattern, page, re.S)
    assert match is not None
    config = json.loads(match.group(1))
    config["revision"] = "00"
    config["sections"] = [item for item in config["sections"] if item["id"] not in {"revision-history", "source-provenance"}]
    core = re.sub(r"<!--CP_REVIEW_START-->.*?<!--CP_REVIEW_END-->", "", page, flags=re.S)
    config["content_sha256"] = hashlib.sha256(core.encode()).hexdigest()
    return page[:match.start(1)] + json.dumps(config) + page[match.end(1):]


def _historical_candidate(tmp_path: Path) -> Path:
    """Make a presentation fixture with unchanged numerical records and citations."""
    from build_cp_review_portfolio import write_json, write_receipt
    from cp_review_document import comment_seed

    candidate = tmp_path / "reports"
    shutil.copytree(REPORTS, candidate)
    case = candidate / "cases/S04"
    page = _historical_page((case / "report.html").read_text())
    (case / "report.html").write_text(page)
    write_json(case / "report.comments.json", comment_seed(page, "S04", "00"))
    cfg = yaml.safe_load((case / "input.yml").read_text())
    cfg["inputs"]["anode"].pop("alloy")
    cfg["inputs"]["anode"]["anode_current_capacity"] = "retired input"
    document = cfg["report"]["document"]
    document.update(title="CP module review: Ship hull", revision="00", date="2026-10-03")
    document["revision_history"] = [row for row in document["revision_history"] if row["rev"] == "00"]
    (case / "input.yml").write_text(yaml.safe_dump(cfg, sort_keys=False))
    release = json.loads((case / "release.json").read_text())
    release.pop("provenance_correction")
    write_json(case / "release.json", release)
    write_receipt(case)
    write_json(candidate / "portfolio-receipt.json", tree_digest(candidate))
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


def test_historical_fixture_preserves_numerical_records(tmp_path: Path) -> None:
    """Reconstructing presentation controls changes no result or citation bytes."""
    candidate = _historical_candidate(tmp_path)
    for name in ("results.json", "report_citations.json", "source-citations.json"):
        assert (candidate / "cases/S04" / name).read_bytes() == (REPORTS / "cases/S04" / name).read_bytes()
    verify_pack(candidate / "cases/S04")
