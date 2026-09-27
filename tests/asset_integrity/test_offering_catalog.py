# ABOUTME: Schema and consistency tests for the FFS offering catalog data
# ABOUTME: (industries x assets x defects x codes x engine status). Epic #1057.
"""Guards for ``asset_integrity/data/ffs_offering_catalog.yml``.

The catalog is the single source for the offering lookup tables. The rules live in
``digitalmodel.asset_integrity.offering_catalog`` (the loader's validator, #2197);
these tests call them one rule at a time so a failure names the rule that broke.
Every reference must resolve, statuses follow the documented semantics, licensed
numerics stay out, and the rendered page's coverage summary matches the data.
"""

from __future__ import annotations

import re
from collections import Counter
from pathlib import Path

import pytest

from digitalmodel.asset_integrity import offering_catalog as oc

REPO = Path(__file__).resolve().parents[2]
CATALOG = (
    REPO
    / "src"
    / "digitalmodel"
    / "asset_integrity"
    / "data"
    / "ffs_offering_catalog.yml"
)
DESIGN_CATALOG = (
    REPO
    / "src"
    / "digitalmodel"
    / "asset_integrity"
    / "data"
    / "ffs_design_screen_catalog.yml"
)
PAGE = REPO / "docs" / "domains" / "asset-integrity" / "ffs-offering-catalog.md"
REGISTRY = REPO / "docs" / "registry" / "workflows.yaml"
DECKHAND_PATHS = (
    REPO.parent / "deckhand" / "config" / "deckhand" / "routing" / "paths.yaml"
)

STATUSES = oc.STATUSES
TIERS = oc.TIERS


@pytest.fixture(scope="module")
def cat() -> oc.Catalog:
    assert oc.FFS_CATALOG == CATALOG and oc.DESIGN_CATALOG == DESIGN_CATALOG
    return oc.load()


@pytest.fixture(scope="module")
def catalog(cat) -> dict:
    return cat.ffs


@pytest.fixture(scope="module")
def rows(cat) -> list[oc.Row]:
    return cat.rows


@pytest.fixture(scope="module")
def registry_ids() -> set[str]:
    return oc.load_registry_ids(REGISTRY)


def test_top_level_shape(catalog):
    assert catalog["schema_version"] == 2
    assert set(catalog["tiers"]) == TIERS
    assert catalog["codes"] and catalog["engines"] and catalog["industries"]
    assert catalog["sources"]["items"], "sources block feeds the rendered page"
    for key, code in catalog["codes"].items():
        assert code.get("short"), (
            f"{key}: code needs a short label for the rendered tables"
        )


def test_engine_entries_are_consistent(cat, registry_ids):
    assert oc.check_engines(cat, registry_ids=registry_ids, import_modules=True) == []


@pytest.mark.skipif(not DECKHAND_PATHS.exists(), reason="deckhand checkout not present")
def test_routes_exist_in_deckhand(cat):
    assert oc.check_routes(cat, DECKHAND_PATHS.read_text(encoding="utf-8")) == []


def test_defect_rows_resolve(cat):
    assert oc.check_rows(cat) == []


def test_status_semantics(cat):
    """Row status describes what can be delivered today for that mechanism.

    - ``none`` rows list no engines (nothing exists and nothing is filed).
    - Every other row lists at least one engine and is never stronger than the
      strongest engine it depends on.
    - ``planned`` rows point at a planned engine or carry their own issue.
    - ``live`` rows require every listed engine to be live or validated (a live
      deliverable cannot lean on an unvalidated engine).
    """
    assert oc.check_status_semantics(cat) == []


def test_no_licensed_numeric_thresholds_in_catalog(cat):
    """Percent or mm thresholds attributed to a standard must not appear in the catalog text."""
    assert oc.check_no_licensed_thresholds(cat) == []
    # the rule itself must still bite
    assert re.findall(oc.THRESHOLD_RE, "screen at <10% wall loss") == ["<10%"]


def test_design_screen_catalog_is_consistent(cat, registry_ids):
    """Design screens (owner decision D5) live apart from FFS verdicts but obey the same rules."""
    assert cat.design["source_catalog"] == "ffs_offering_catalog.yml"
    assert oc.check_design(cat, registry_ids=registry_ids, import_modules=True) == []


def test_page_summary_matches_catalog(rows):
    counts = Counter(r.status for r in rows)
    page = PAGE.read_text(encoding="utf-8")
    block = page.split("## Coverage summary", 1)[1]
    for status in STATUSES:
        m = re.search(rf"^\| {status} \| (\d+) \|", block, re.M)
        assert m, f"page summary lacks a row for {status}"
        assert int(m.group(1)) == counts.get(status, 0), (
            f"{status}: page {m.group(1)} vs data {counts.get(status, 0)}"
        )
    m = re.search(r"^\| total \| (\d+) \|", block, re.M)
    assert m and int(m.group(1)) == len(rows)
