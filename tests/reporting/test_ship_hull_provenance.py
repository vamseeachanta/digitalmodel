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


def _legacy_page() -> str:
    """Exercise the original nested caption and table without source constants."""
    config = {
        "report_id": "S04", "revision": "00",
        "sections": [
            {"id": "review-1", "title": "Executive Summary"},
            {"id": "review-2", "title": "Introduction"},
        ],
    }
    return (
        '<html><body><header>CP module review: Ship hull — Rev 00'
        '<table><tr><td>Revision</td><td>00</td></tr>'
        '<tr><td>Date</td><td>2026-10-03</td></tr></table>'
        '    <div class="kv revhist"><div class="cap">Revision history</div>'
        '<table><thead><tr><th>Rev</th><th>Date</th></tr></thead><tbody>'
        '<tr><td>00</td><td>2026-10-03</td><td>Internal review draft</td>'
        '<td></td><td>Pending</td><td>Pending</td></tr>'
        '</tbody></table></div></header>'
        '<nav><li><a href="#review-2">Introduction</a></li></nav>'
        '<section id="review-1"><div class="prose">Summary</div></section>'
        '<section id="review-2"><p>Introduction</p></section>'
        '<table><tr><td>Anode current capacity</td>'
        '<td>historical table entry</td><td>Ah/kg</td></tr>'
        '<tr><td>inputs.anode.anode_current_capacity</td>'
        '<td>retired input</td></tr></table>'
        '<!--CP_REVIEW_START-->'
        '<script id="cp-review-config" type="application/json">'
        + json.dumps(config) + '</script><!--CP_REVIEW_END--></body></html>'
    )


def _historical_candidate(tmp_path: Path) -> Path:
    """Combine independent legacy markup with unchanged numerical records."""
    from build_cp_review_portfolio import write_json, write_receipt
    from cp_review_document import comment_seed

    candidate = tmp_path / "reports"
    shutil.copytree(REPORTS, candidate)
    case = candidate / "cases/S04"
    page = _legacy_page()
    (case / "report.html").write_text(page, encoding="utf-8", newline="\n")
    write_json(case / "report.comments.json", comment_seed(page, "S04", "00"))
    cfg = yaml.safe_load((case / "input.yml").read_text(encoding="utf-8"))
    cfg["inputs"]["anode"].pop("alloy")
    cfg["inputs"]["anode"]["anode_current_capacity"] = "retired input"
    document = cfg["report"]["document"]
    document.update(title="CP module review: Ship hull", revision="00", date="2026-10-03")
    document["revision_history"] = [row for row in document["revision_history"] if row["rev"] == "00"]
    (case / "input.yml").write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8", newline="\n")
    release = json.loads((case / "release.json").read_text(encoding="utf-8"))
    release.pop("provenance_correction")
    write_json(case / "release.json", release)
    write_receipt(case)
    write_json(candidate / "portfolio-receipt.json", tree_digest(candidate))
    return candidate


def test_ship_fixture_does_not_echo_an_ignored_capacity() -> None:
    """The alloy selection replaces the legacy scalar at its input source."""
    cfg = yaml.safe_load((ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/ships.yml").read_text(encoding="utf-8"))
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
    page = (case / "report.html").read_text(encoding="utf-8")
    validate_comments(page, json.loads((case / "report.comments.json").read_text(encoding="utf-8")))
    assert "inputs.anode.anode_current_capacity" not in page
    assert "inputs.anode.alloy" in page
    assert "client-derived" in page
    assert "Revision history" in page.split('<section id="review-2">')[0]
    assert "Hull cathodic protection analysis" in page
    row = next(row for row in json.loads((candidate / "coverage.json").read_text(encoding="utf-8"))["rows"] if row["id"] == "S04")
    assert row["pack_state"] == "blocked"
    assert row["source_class"] == "client_derived_pending_release"
    release = json.loads((case / "release.json").read_text(encoding="utf-8"))
    assert release["numerical_rebuild"] == "blocked_pending_source_qualification"
    fixture = ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/ships.yml"
    assert release["input_sha256"] == hashlib.sha256(fixture.read_bytes()).hexdigest()
    assert json.loads((candidate / "portfolio-receipt.json").read_text(encoding="utf-8")) == tree_digest(candidate)


def test_correction_preserves_review_comments_by_refusing_overwrite(tmp_path: Path) -> None:
    """A report with existing reviewer comments remains untouched."""
    from correct_ship_hull_provenance import correct_ship_provenance
    from build_cp_review_portfolio import write_json, write_receipt

    candidate = _historical_candidate(tmp_path)
    case = candidate / "cases/S04"
    sidecar = json.loads((case / "report.comments.json").read_text(encoding="utf-8"))
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
    report.write_text(report.read_text(encoding="utf-8") + "<p>Changed</p>", encoding="utf-8", newline="\n")
    before = tree_digest(candidate)
    with pytest.raises(ValueError, match="checksum mismatch"):
        correct_ship_provenance(candidate)
    assert tree_digest(candidate) == before


def test_structural_candidate_preserves_numerical_records(tmp_path: Path) -> None:
    """Structural test setup changes no result or citation bytes."""
    candidate = _historical_candidate(tmp_path)
    for name in ("results.json", "report_citations.json", "source-citations.json"):
        assert (candidate / "cases/S04" / name).read_bytes() == (REPORTS / "cases/S04" / name).read_bytes()
    verify_pack(candidate / "cases/S04")


def _assert_history_location(page: str) -> None:
    """Both revisions belong after the summary, outside the title block."""
    section = re.search(r'<section id="revision-history">(.*?)</section>', page, re.S)
    assert section is not None
    assert "<td>00</td>" in section.group(1)
    assert "<td>01</td>" in section.group(1)
    assert "</table>" in section.group(1)
    title_block = page.split('<section id="review-1">')[0]
    assert 'class="kv revhist"' not in title_block
    assert "<td>00</td>" not in title_block
    assert page.index('</section>') < section.start()


def test_correction_moves_complete_revision_history() -> None:
    """A nested caption cannot truncate the revision table match."""
    from correct_ship_hull_provenance import _correct_page

    page = _correct_page(_legacy_page(), {
        key: "Source qualification pending."
        for key in ("hull_area_basis", "capacity_basis", "limitations", "way_forward")
    })
    _assert_history_location(page)


def test_correction_replaces_capacity_with_table_locator() -> None:
    """The design-data capacity cell cites the source without its table value."""
    from correct_ship_hull_provenance import _correct_page

    page = _correct_page(_legacy_page(), {
        key: "Source qualification pending."
        for key in ("hull_area_basis", "capacity_basis", "limitations", "way_forward")
    })
    row = re.search(r"<tr><td>Anode current capacity</td>(.*?)</tr>", page)
    assert row is not None
    assert "historical table entry" not in row.group(1)
    assert "ABS Guidance Notes on Cathodic Protection of Ships" in row.group(1)
    assert "December 2017, Section 3, Table 4" in row.group(1)
    assert "Typical Properties of Aluminum Anodes in Normal Operation Conditions" in row.group(1)


def test_existing_reissue_is_repaired(tmp_path: Path) -> None:
    """The already-issued pack receives the same presentation correction."""
    from correct_ship_hull_provenance import correct_ship_provenance

    candidate = tmp_path / "existing"
    shutil.copytree(REPORTS, candidate)
    before = tree_digest(candidate)
    correct_ship_provenance(candidate)
    page = (candidate / "cases/S04/report.html").read_text(encoding="utf-8")
    _assert_history_location(page)
    row = re.search(r"<tr><td>Anode current capacity</td>(.*?)</tr>", page)
    assert row is not None and "Section 3, Table 4" in row.group(1)
    after = tree_digest(candidate)
    for name, digest in before.items():
        if name.startswith("cases/") and not name.startswith("cases/S04/"):
            assert after[name] == digest
    for name in ("results.json", "report_citations.json", "source-citations.json"):
        assert after[f"cases/S04/{name}"] == before[f"cases/S04/{name}"]
    cfg = yaml.safe_load((candidate / "cases/S04/input.yml").read_text(encoding="utf-8"))
    assert [row["rev"] for row in cfg["report"]["document"]["revision_history"]] == ["00", "01"]
    verify_pack(candidate / "cases/S04")
    correct_ship_provenance(candidate)
    assert tree_digest(candidate) == after


def test_truncated_reissue_is_repaired_without_duplicate_rows(tmp_path: Path) -> None:
    """Repair the displaced table even after the published pack is corrected."""
    from build_cp_review_portfolio import write_json, write_receipt
    from correct_ship_hull_provenance import correct_ship_provenance
    from cp_review_document import comment_seed

    candidate = tmp_path / "truncated"
    shutil.copytree(REPORTS, candidate)
    case = candidate / "cases/S04"
    page = (case / "report.html").read_text(encoding="utf-8")
    section = re.search(r'<section id="revision-history">(.*?)</section>', page, re.S)
    assert section is not None
    table = re.search(r"<table>.*?</table>", section.group(1), re.S)
    assert table is not None
    prior_table = re.sub(r"<tr><td>01</td>.*?</tr>", "", table.group(), flags=re.S)
    broken = '<section id="revision-history"><div class="kv revhist"><h2>Revision history</h2></section>'
    page = page[:section.start()] + broken + page[section.end():]
    page = page.replace('<section id="review-1">', prior_table + '</div><section id="review-1">', 1)
    (case / "report.html").write_text(page, encoding="utf-8", newline="\n")
    write_json(case / "report.comments.json", comment_seed(page, "S04", "01"))
    write_receipt(case)
    correct_ship_provenance(candidate)
    _assert_history_location((case / "report.html").read_text(encoding="utf-8"))
    verify_pack(case)
    before = tree_digest(candidate)
    correct_ship_provenance(candidate)
    assert tree_digest(candidate) == before
