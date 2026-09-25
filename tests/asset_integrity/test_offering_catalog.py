# ABOUTME: Schema and consistency tests for the FFS offering catalog data
# ABOUTME: (industries x assets x defects x codes x engine status). Epic #1057.
"""Guards for ``asset_integrity/data/ffs_offering_catalog.yml``.

The catalog is the single source for the offering lookup tables. These tests
keep it honest: every reference resolves, statuses follow the documented
semantics, licensed numerics stay out, and the rendered page's coverage
summary matches the data.
"""

from __future__ import annotations

import importlib
import re
from collections import Counter
from pathlib import Path

import pytest
import yaml

REPO = Path(__file__).resolve().parents[2]
CATALOG = REPO / "src" / "digitalmodel" / "asset_integrity" / "data" / "ffs_offering_catalog.yml"
PAGE = REPO / "docs" / "domains" / "asset-integrity" / "ffs-offering-catalog.md"
REGISTRY = REPO / "docs" / "registry" / "workflows.yaml"
DECKHAND_PATHS = REPO.parent / "deckhand" / "config" / "deckhand" / "routing" / "paths.yaml"

STATUSES = {"live", "routed", "validated", "engine", "planned", "none"}
TIERS = {"T1", "T2", "T3"}
ENGINE_STATUSES_NEEDING_MODULE = {"live", "routed", "validated", "engine"}


@pytest.fixture(scope="module")
def catalog() -> dict:
    return yaml.safe_load(CATALOG.read_text(encoding="utf-8"))


@pytest.fixture(scope="module")
def rows(catalog) -> list[dict]:
    out = []
    for ind in catalog["industries"]:
        for asset in ind["assets"]:
            for d in asset["defects"]:
                out.append({**d, "_industry": ind["id"], "_asset": asset["asset"]})
    return out


@pytest.fixture(scope="module")
def registry_ids() -> set[str]:
    reg = yaml.safe_load(REGISTRY.read_text(encoding="utf-8"))
    return {w["id"] for w in reg["workflows"]}


def test_top_level_shape(catalog):
    assert catalog["schema_version"] == 2
    assert set(catalog["tiers"]) == TIERS
    assert catalog["codes"] and catalog["engines"] and catalog["industries"]


def test_engine_entries_are_consistent(catalog, registry_ids):
    for key, eng in catalog["engines"].items():
        assert eng["status"] in STATUSES, key
        if eng["status"] in ENGINE_STATUSES_NEEDING_MODULE:
            assert "module" in eng, f"{key}: engine status needs a module"
            importlib.import_module(f"digitalmodel.{eng['module']}")
        if eng["status"] == "planned":
            assert isinstance(eng.get("issue"), int), f"{key}: planned needs an issue"
        if eng["status"] in {"live", "routed"}:
            assert eng.get("workflow") in registry_ids, f"{key}: workflow id not in registry"
            assert eng.get("route") is True, f"{key}: live/routed requires route: true"
        if eng["status"] in {"live", "validated"}:
            rec = eng.get("validation")
            assert rec and (REPO / rec).exists(), f"{key}: {eng['status']} needs an existing validation record"
        if "workflow" in eng:
            assert eng["workflow"] in registry_ids, f"{key}: unknown workflow {eng['workflow']}"


@pytest.mark.skipif(not DECKHAND_PATHS.exists(), reason="deckhand checkout not present")
def test_routes_exist_in_deckhand(catalog):
    text = DECKHAND_PATHS.read_text(encoding="utf-8")
    for key, eng in catalog["engines"].items():
        if eng.get("route") is True:
            assert f"digitalmodel:{eng['workflow']}" in text, f"{key}: no deckhand route"


def test_defect_rows_resolve(catalog, rows):
    codes, engines = catalog["codes"], catalog["engines"]
    parts = catalog["codes"]["api-579-1"]["parts"]
    for r in rows:
        where = f"{r['_industry']} / {r['_asset']} / {r['mechanism']}"
        assert r["status"] in STATUSES, where
        assert r["tiers"] and set(r["tiers"]) <= TIERS, where
        for c in r["codes"]:
            assert c in codes, f"{where}: unknown code {c}"
        for e in r["engines"]:
            assert e in engines, f"{where}: unknown engine {e}"
        for p in r.get("parts", []):
            assert p in parts, f"{where}: unknown API 579 part {p}"
        if r.get("parts"):
            assert "api-579-1" in r["codes"], f"{where}: parts given without api-579-1"


ORDER = {"none": 0, "planned": 1, "engine": 2, "routed": 3, "validated": 3, "live": 4}


def test_status_semantics(catalog, rows):
    """Row status describes what can be delivered today for that mechanism.

    - ``none`` rows list no engines (nothing exists and nothing is filed).
    - Every other row lists at least one engine and is never stronger than the
      strongest engine it depends on.
    - ``planned`` rows point at a planned engine or carry their own issue.
    - ``live`` rows require every listed engine to be live or validated (a live
      deliverable cannot lean on an unvalidated engine).
    """
    engines = catalog["engines"]
    for r in rows:
        where = f"{r['_industry']} / {r['mechanism']}"
        if r["status"] == "none":
            assert r["engines"] == [], f"{where}: 'none' row lists engines {r['engines']}"
            continue
        assert r["engines"], f"{where}: status {r['status']} without engines"
        strongest = max(ORDER[engines[e]["status"]] for e in r["engines"])
        assert ORDER[r["status"]] <= strongest, f"{where}: row status stronger than its engines"
        if r["status"] == "planned":
            assert any(engines[e]["status"] == "planned" for e in r["engines"]) or isinstance(
                r.get("issue"), int
            ), f"{where}: planned row without a planned engine or issue"
        if r["status"] == "live":
            assert all(engines[e]["status"] in {"live", "validated"} for e in r["engines"]), where


def test_no_licensed_numeric_thresholds_in_catalog():
    """Percent or mm thresholds attributed to a standard must not appear in the catalog text."""
    text = CATALOG.read_text(encoding="utf-8")
    suspicious = re.findall(r"(?:<|>|≥|≤)\s?\d+\s?(?:%|mm)", text)
    assert not suspicious, f"threshold-looking values in catalog: {suspicious}"


def test_page_summary_matches_catalog(rows):
    counts = Counter(r["status"] for r in rows)
    page = PAGE.read_text(encoding="utf-8")
    block = page.split("## Coverage summary", 1)[1]
    for status in STATUSES:
        m = re.search(rf"^\| {status} \| (\d+) \|", block, re.M)
        assert m, f"page summary lacks a row for {status}"
        assert int(m.group(1)) == counts.get(status, 0), f"{status}: page {m.group(1)} vs data {counts.get(status, 0)}"
    m = re.search(r"^\| total \| (\d+) \|", block, re.M)
    assert m and int(m.group(1)) == len(rows)
