# ABOUTME: Tests for the FFS offering-catalog loader, query helpers, Markdown renderer, damage-mechanism
# ABOUTME: crosswalk and capability-map generation (issue #2197). The rendered page must never drift.
"""Guards for ``digitalmodel.asset_integrity.offering_catalog``.

The committed page ``docs/domains/asset-integrity/ffs-offering-catalog.md`` and the
``engines.ffs`` block of ``docs/capability-map/capabilities-added.yml`` are generated
from the YAML catalogs. These tests fail on any drift, so the page can only change by
editing the data and re-running ``python -m digitalmodel.asset_integrity.offering_catalog render``.
"""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest
import yaml

from digitalmodel.asset_integrity import offering_catalog as oc

REPO = Path(__file__).resolve().parents[2]
PAGE = REPO / "docs" / "domains" / "asset-integrity" / "ffs-offering-catalog.md"
CAPMAP = REPO / "docs" / "capability-map" / "capabilities-added.yml"


@pytest.fixture(scope="module")
def cat() -> oc.Catalog:
    return oc.load()


# ---------------------------------------------------------------------------
# rendered page
# ---------------------------------------------------------------------------


def test_rendered_page_equals_committed_page_byte_for_byte(cat):
    rendered = oc.render_markdown(cat).encode("utf-8")
    committed = PAGE.read_bytes()
    assert rendered == committed, (
        "docs/domains/asset-integrity/ffs-offering-catalog.md drifted from the YAML catalogs; "
        "run: python -m digitalmodel.asset_integrity.offering_catalog render"
    )


def test_page_carries_generated_header(cat):
    first_line = oc.render_markdown(cat).splitlines()[0]
    assert first_line.startswith("<!-- GENERATED") and "do not edit" in first_line


def test_page_has_all_sections_in_order(cat):
    page = oc.render_markdown(cat)
    n_ffs = len(cat.ffs["industries"])
    n_design = len(cat.design["screens"])
    headings = [ln for ln in page.splitlines() if ln.startswith("## ")]
    numbered = [h for h in headings if h[3].isdigit()]
    assert len(numbered) == n_ffs + n_design == 9
    assert [int(h.split(".")[0][3:]) for h in numbered] == list(range(1, 10))
    tail = [h for h in headings if not h[3].isdigit()]
    assert tail[0] == "## How to read these tables"
    assert tail[1] == "## Coverage summary"
    assert tail[2].startswith("## Sources")
    # column layout: Asset column only where the industry asks for it
    for ind in cat.ffs["industries"]:
        block = page.split(f"## {cat.section_number(ind['id'])}. {ind['title']}", 1)[
            1
        ].split("\n## ", 1)[0]
        header = next(ln for ln in block.splitlines() if ln.startswith("| "))
        if oc.asset_column(ind):
            assert header.startswith("| Asset | Defect / mechanism |"), ind["id"]
        else:
            assert header.startswith("| Defect / mechanism |"), ind["id"]


def test_coverage_summary_counts_ffs_rows_only(cat):
    page = oc.render_markdown(cat)
    block = page.split("## Coverage summary", 1)[1]
    summary = cat.summary()
    assert summary["total"] == len(cat.rows)
    for status in oc.STATUSES:
        assert f"| {status} | {summary[status]} |" in block
    assert f"| total | {summary['total']} |" in block


def test_sources_block_is_rendered_from_yaml(cat):
    page = oc.render_markdown(cat)
    for item in cat.ffs["sources"]["items"]:
        for link in item["links"]:
            assert f"[{link['label']}]({link['url']})" in page


# ---------------------------------------------------------------------------
# query helpers
# ---------------------------------------------------------------------------


def test_gaps_equal_none_rows(cat):
    none_rows = [r for r in cat.rows if r.status == "none"]
    assert cat.gaps() == none_rows
    assert all(r.engines == () for r in cat.gaps())
    assert len(cat.gaps()) == cat.summary()["none"]
    # design screens have their own `none` rows; they are separate by default
    design_gaps = [r for r in cat.design_rows if r.status == "none"]
    assert cat.gaps(include_design=True) == none_rows + design_gaps


def test_by_industry_by_code_by_status(cat):
    ids = [ind["id"] for ind in cat.ffs["industries"]]
    assert sum(len(cat.by_industry(i)) for i in ids) == len(cat.rows)
    with pytest.raises(KeyError):
        cat.by_industry("no-such-industry")
    api579 = cat.by_code("api-579-1")
    assert api579 and all("api-579-1" in r.codes for r in api579)
    assert not cat.by_code("dnv-rp-f105")
    assert cat.by_code("dnv-rp-f105", include_design=True)
    with pytest.raises(KeyError):
        cat.by_code("no-such-code")
    for status in oc.STATUSES:
        assert len(cat.by_status(status)) == cat.summary()[status]
    with pytest.raises(ValueError):
        cat.by_status("shipped")


def test_validator_passes_on_committed_catalogs(cat):
    assert oc.validate(cat) == []


# ---------------------------------------------------------------------------
# damage-mechanism crosswalk (API RP 571 names -> API 579 parts)
# ---------------------------------------------------------------------------


def test_crosswalk_mechanisms_map_to_valid_parts(cat):
    parts = set(cat.ffs["codes"]["api-579-1"]["parts"])
    xw = cat.crosswalk
    assert xw["taxonomy"] == "api-rp-571" and xw["target"] == "api-579-1"
    names = []
    for cat_group in xw["categories"]:
        assert cat_group["name"] and cat_group["mechanisms"]
        for m in cat_group["mechanisms"]:
            names.append(m["name"])
            assert m["parts"], m["name"]
            assert set(m["parts"]) <= parts, (
                f"{m['name']}: parts {m['parts']} not in API 579 parts"
            )
    assert len(names) == len(set(names)), "duplicate mechanism names"
    assert 30 <= len(names) <= 80
    assert oc.check_crosswalk(cat) == []


def test_crosswalk_lookup_helper(cat):
    hits = cat.mechanisms_for_part(9)
    assert hits and all(9 in m["parts"] for m in hits)
    assert cat.mechanisms_for_part(99) == []


# ---------------------------------------------------------------------------
# capability map
# ---------------------------------------------------------------------------


def test_capability_map_ffs_entries_equal_catalog_engines(cat):
    doc = yaml.safe_load(CAPMAP.read_text(encoding="utf-8"))
    assert doc["sections"]["ffs"] != "unknown"
    assert set(doc["sections"]["ffs"]) == {"date", "pr"}
    entries = doc["engines"]["ffs"]
    assert set(entries) == set(cat.ffs["engines"])
    assert entries == oc.capability_map_entries(cat)
    # the committed file is exactly what the renderer produces
    assert oc.render_capability_map(
        cat, CAPMAP.read_text(encoding="utf-8")
    ) == CAPMAP.read_text(encoding="utf-8")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def test_cli_check_passes_on_committed_files_and_fails_on_drift(tmp_path, capsys):
    rc = oc.main(["check", "--page", str(PAGE), "--capability-map", str(CAPMAP)])
    assert rc == 0

    drifted = tmp_path / "ffs-offering-catalog.md"
    shutil.copy(PAGE, drifted)
    text = drifted.read_text(encoding="utf-8")
    drifted.write_text(
        text.replace("| total |", "| total (edited) |", 1),
        encoding="utf-8",
        newline="\n",
    )
    rc = oc.main(["check", "--page", str(drifted), "--capability-map", str(CAPMAP)])
    assert rc == 1
    assert "drift" in capsys.readouterr().out.lower()


def test_cli_render_writes_page_and_capability_map(tmp_path):
    page = tmp_path / "page.md"
    capmap = tmp_path / "capabilities-added.yml"
    shutil.copy(CAPMAP, capmap)
    rc = oc.main(["render", "--page", str(page), "--capability-map", str(capmap)])
    assert rc == 0
    assert page.read_bytes() == PAGE.read_bytes()
    assert capmap.read_bytes() == CAPMAP.read_bytes()
