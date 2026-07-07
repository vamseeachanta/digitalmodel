# ABOUTME: TDD suite for the capabilities IA inventory (issue #1444) — section
# ABOUTME: census bijection, cluster totality, joins with gaps, freshness gate.

import importlib.util
import json
from pathlib import Path

import pytest
import yaml

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts" / "capabilities" / "build_capabilities_inventory.py"

spec = importlib.util.spec_from_file_location("capinv", SCRIPT)
ci = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ci)

INDEX = REPO / "docs" / "api" / "capabilities" / "index.html"
CLUSTERS = REPO / "docs" / "capability-map" / "capabilities-clusters.yml"
ADDED = REPO / "docs" / "capability-map" / "capabilities-added.yml"


# 1 — census bijection on the LIVE page: nav hrefs <-> section ids, 1:1


def test_section_census_bijection_live_page():
    sections = ci.parse_sections(INDEX.read_text())
    ids = [s["id"] for s in sections]
    assert len(ids) == len(set(ids))
    nav = ci.parse_nav(INDEX.read_text())
    assert set(nav) == set(ids)
    assert len(nav) == len(ids)
    for s in sections:
        assert s["title"]


# 2 — parser failure fixtures fail closed with diagnostics


def test_parser_failure_fixtures():
    dup = '<nav><a href="#a">A</a></nav><section id="a"></section><section id="a"></section>'
    with pytest.raises(ci.CensusError, match="duplicate"):
        ci.census(dup)
    nav_only = '<nav><a href="#a">A</a><a href="#ghost">G</a></nav><section id="a"><h2>A</h2></section>'
    with pytest.raises(ci.CensusError, match="ghost"):
        ci.census(nav_only)
    sec_only = '<nav><a href="#a">A</a></nav><section id="a"><h2>A</h2></section><section id="b"><h2>B</h2></section>'
    with pytest.raises(ci.CensusError, match="b"):
        ci.census(sec_only)


# 3 — cluster map: schema, totality, disjointness vs the live census


def test_cluster_mapping_total_disjoint_and_schema():
    doc = yaml.safe_load(CLUSTERS.read_text())
    assert doc["schema_version"]
    assigned = []
    for c in doc["clusters"]:
        assert c["key"] and c["label"] and c["value_statement"]
        assigned += c["sections"]
    assert len(assigned) == len(set(assigned)), "section in two clusters"
    live = {s["id"] for s in ci.parse_sections(INDEX.read_text())}
    assert set(assigned) == live, (
        f"cluster map out of sync: missing={live - set(assigned)} "
        f"stale={set(assigned) - live}"
    )


# 4 — explorer discovery excludes the frozen capabilities/api duplicates


def test_explorer_discovery_excludes_frozen_dupes():
    found = ci.discover_explorers(REPO)
    assert found, "no explorers discovered"
    assert all("capabilities/api" not in p for p in found)
    live = {
        str(p.relative_to(REPO))
        for p in (REPO / "docs" / "api").rglob("*-explorer.html")
        if "capabilities/api" not in str(p)
    }
    assert set(found) == live


# 5 — joins one-to-one; gaps and unlinked explicit; PDF joins via sec-<anchor>


def test_joins_one_to_one_with_gaps_and_unlinked():
    inv = ci.build_inventory(REPO)
    by_id = {s["id"]: s for s in inv["sections"]}
    assert len(by_id) == len(inv["sections"])
    for s in inv["sections"]:
        assert s["cluster"]
        assert isinstance(s["explorers"], list)
        assert s["pdf"] is None or s["pdf"].startswith("sec-") is False
    gaps = [s["id"] for s in inv["sections"] if s["pdf"] is None]
    assert gaps == inv["pdf_gaps"]
    # every discovered explorer either linked to exactly one section or listed
    linked = [e for s in inv["sections"] for e in s["explorers"]]
    assert len(linked) == len(set(linked)), "explorer joined to two sections"
    assert set(linked) | set(inv["unlinked_explorers"]) == set(
        ci.discover_explorers(REPO)
    )


# 6 — recency from metadata only; no git calls; unknown honest


def test_recency_from_metadata_only():
    # guard: no git INVOCATION mechanism (prose mentions of git are fine —
    # the docstring explains why git-derived dating is invalid here)
    src = SCRIPT.read_text()
    for needle in ("subprocess", "os.system", "os.popen", "from git", "import git"):
        assert needle not in src, f"extractor must not shell out / use git: {needle}"
    added = yaml.safe_load(ADDED.read_text())
    inv = ci.build_inventory(REPO)
    for s in inv["sections"]:
        assert "added" in s
        if s["added"] != "unknown":
            assert s["added"]["date"] and s["added"]["pr"]
    known = [s for s in inv["sections"] if s["added"] != "unknown"]
    assert known, "recency seed empty — expected PR-evidenced entries"
    assert len(inv["recent"]) <= added.get("recent_n", 8)


# 7 — --check mode fails on drift


def test_check_mode_fails_on_drift(tmp_path):
    out = tmp_path / "inv.json"
    ci.write_inventory(ci.build_inventory(REPO), out)
    assert ci.check(REPO, out) == 0
    doc = json.loads(out.read_text())
    doc["sections"][0]["cluster"] = "tampered"
    out.write_text(json.dumps(doc, indent=2, sort_keys=True))
    assert ci.check(REPO, out) != 0


# 8 — rendered spec tables match the generated JSON


def test_md_tables_match_inventory_json(tmp_path):
    inv = ci.build_inventory(REPO)
    md = ci.render_spec(inv)
    for s in inv["sections"]:
        assert f"#{s['id']}" in md, f"section {s['id']} missing from spec tables"
    for gap in inv["pdf_gaps"]:
        assert gap in md
    assert str(len(inv["sections"])) in md


# 9 — committed artifacts are fresh (the real CI gate)


def test_committed_inventory_is_fresh():
    committed = REPO / "docs" / "capability-map" / "capabilities-inventory.json"
    assert committed.exists(), "run build_capabilities_inventory.py and commit"
    assert ci.check(REPO, committed) == 0, (
        "committed inventory drifted — regenerate: "
        ".venv/bin/python scripts/capabilities/build_capabilities_inventory.py"
    )
