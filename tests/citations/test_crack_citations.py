"""Citation getters for the crack-like-flaw capability (#2157 P3; owner cards E01, E02, G09).

Contract: metadata-only citations (code_id, publisher, revision, section) that resolve
fail-closed against a wiki page's frontmatter. No clause text ships (owner card G10).
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError
from digitalmodel.citations.registry import get_api579_reference, get_bs7910_reference

FIXTURES = Path(__file__).resolve().parent / "fixtures"
REPO = Path(__file__).resolve().parents[2]


@pytest.mark.parametrize("root", [FIXTURES, REPO], ids=["fixtures", "repo-overlay"])
def test_api579_reference_resolves(root):
    cv = get_api579_reference("Part 9, Level 2 FAD", repo_root=root)
    c = cv.citation
    assert c.code_id == "api-std-579-asme-ffs-1"
    assert c.revision == "2016"
    assert c.section == "Part 9, Level 2 FAD"
    assert c.source_sibling == "generic"
    assert cv.units == "reference"


@pytest.mark.parametrize("root", [FIXTURES, REPO], ids=["fixtures", "repo-overlay"])
def test_bs7910_reference_resolves(root):
    cv = get_bs7910_reference("Option 1 FAD", repo_root=root)
    assert cv.citation.code_id == "bs-7910"
    assert cv.citation.revision == "2013"
    assert cv.citation.publisher == "BSI"


def test_missing_page_fails_closed(tmp_path):
    with pytest.raises(CitationResolutionError) as exc:
        get_api579_reference("Part 9", repo_root=tmp_path)
    assert "api-std-579-asme-ffs-1" in str(exc.value)


def test_revision_mismatch_fails_closed(tmp_path):
    page = tmp_path / "knowledge/wikis/engineering-standards/wiki/standards/bs-7910.md"
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: bs-7910\npublisher: BSI\nrevision: \"2019\"\n---\n# stub\n",
        encoding="utf-8",
    )
    with pytest.raises(CitationResolutionError):
        get_bs7910_reference("Option 1", repo_root=tmp_path)


def test_overlay_stubs_are_metadata_only():
    base = REPO / "knowledge/wikis/engineering-standards/wiki/standards"
    for name in ("api-std-579.md", "bs-7910.md"):
        text = (base / name).read_text(encoding="utf-8")
        body = text.split("---", 2)[2]
        # bibliographic stub only: short body, no clause text
        assert len(body.strip().splitlines()) <= 12, name
