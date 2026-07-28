# ABOUTME: TDD suite for the capabilities IA inventory (issue #1444) — section
# ABOUTME: census schema, cluster totality, joins with gaps, freshness gate.

import ast
import html
import importlib.util
import json
import re
from pathlib import Path

import pytest
import yaml

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts" / "capabilities" / "build_capabilities_inventory.py"

spec = importlib.util.spec_from_file_location("capinv", SCRIPT)
ci = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ci)

SECTIONS = REPO / "docs" / "capability-map" / "capabilities-sections.yml"
CLUSTERS = REPO / "docs" / "capability-map" / "capabilities-clusters.yml"
ADDED = REPO / "docs" / "capability-map" / "capabilities-added.yml"


# 1 — the declared census: unique, titled, URL-safe anchors


def test_section_census_schema():
    sections = ci.census(ci.load_sections(REPO))
    ids = [s["id"] for s in sections]
    assert len(ids) == len(set(ids))
    for s in sections:
        assert s["title"]
        assert isinstance(s["hrefs"], list)


# 1a — titles are data, not markup.
#
# The census was extracted from HTML, so titles arrived carrying &amp; and
# &mdash;. Those entities are meaningless in YAML and leak into every consumer
# that is not an HTML renderer — the generated spec tables, the one-pagers, any
# future API. Decoded once in #1637; this keeps them from creeping back with
# the next hand-added section.


def test_titles_are_plain_text():
    offenders = [
        (s["id"], s["title"])
        for s in ci.load_sections(REPO)
        if html.unescape(s["title"]) != s["title"]
    ]
    assert not offenders, (
        f"section titles must be plain text, not HTML entities: {offenders}"
    )


# 1b — the IA is declared in YAML, not scraped out of rendered markup.
#
# docs/api/capabilities/index.html was both the gallery and the schema until
# #1573 (C10) redirected it to aceengineer.com — which silently deleted the
# schema and broke every census (dm#1637). Nothing here may depend on that
# page again, or the next presentation change re-breaks the same 8 tests.


def test_census_does_not_read_rendered_html():
    # Structural, not textual: the docstring *explains* the page it no longer
    # reads, so a substring search would flag its own rationale. Only string
    # literals the code could open count — docstrings are excluded by skipping
    # bare-expression strings.
    tree = ast.parse(SCRIPT.read_text())
    docstrings = {
        id(n.value)
        for n in ast.walk(tree)
        if isinstance(n, ast.Expr) and isinstance(n.value, ast.Constant)
    }
    referenced = [
        n.value
        for n in ast.walk(tree)
        if isinstance(n, ast.Constant)
        and isinstance(n.value, str)
        and id(n) not in docstrings
        and "index.html" in n.value
    ]
    assert not referenced, (
        f"the census must not read the rendered capabilities page: {referenced} "
        "— the IA lives in capabilities-sections.yml (dm#1637)"
    )
    assert ci.build_inventory(REPO)["source"] == ci.SECTIONS_YML


# 2 — census failure fixtures fail closed with diagnostics


def test_census_failure_fixtures():
    with pytest.raises(ci.CensusError, match="no sections"):
        ci.census([])
    dup = [
        {"id": "a", "title": "A", "hrefs": []},
        {"id": "a", "title": "A again", "hrefs": []},
    ]
    with pytest.raises(ci.CensusError, match="duplicate"):
        ci.census(dup)
    bad_anchor = [{"id": "Not An Anchor", "title": "A", "hrefs": []}]
    with pytest.raises(ci.CensusError, match="kebab-case"):
        ci.census(bad_anchor)
    untitled = [{"id": "a", "title": "  ", "hrefs": []}]
    with pytest.raises(ci.CensusError, match="no title"):
        ci.census(untitled)


# 3 — cluster map: schema, totality, disjointness vs the declared census


def test_cluster_mapping_total_disjoint_and_schema():
    doc = yaml.safe_load(CLUSTERS.read_text())
    assert doc["schema_version"]
    assigned = []
    for c in doc["clusters"]:
        assert c["key"] and c["label"] and c["value_statement"]
        assigned += c["sections"]
    assert len(assigned) == len(set(assigned)), "section in two clusters"
    live = {s["id"] for s in ci.load_sections(REPO)}
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


# ---------------------------------------------------------------------------
# One-pager coverage ratchet + standards grounding (issue #1456).
#
# After #1456 closes, total coverage IS the contract: every live section
# carries a kind="section" SPECS entry AND its committed PDF. A future section
# without its one-pager fails CI here — that is the point (plan:
# workspace-hub docs/plans/2026-07-06-issue-dm-1456-section-onepager-gaps.md).
# Escape hatch: list a section id under `onepager_exempt:` in
# capabilities-clusters.yml (empty today; adding to it is a reviewable diff,
# never a silent bypass).
# ---------------------------------------------------------------------------

_GH_HREF_RE = re.compile(
    r"https://github\.com/vamseeachanta/digitalmodel/(?:blob|tree)/main/(.+)"
)
# standards-designator tokens inside a SPECS `std` line (publisher prefix
# followed by a numbered designation, e.g. "DNV-RP-C203", "API RP 2A-WSD")
_STD_TOKEN_RE = re.compile(
    r"\b(?:API|DNV|DNVGL|ASME|BS|PD|ISO|EN|NORSOK|NACE|IACS|IMO|MIL|SPE|IIW)"
    r"[A-Za-z0-9 .()/-]*\d[A-Za-z0-9.()/-]*"
)
_BINARY_SUFFIXES = {".pdf", ".png", ".jpg", ".jpeg", ".gif", ".svg", ".ico"}


def _onepager_exempt() -> set:
    return set(yaml.safe_load(CLUSTERS.read_text()).get("onepager_exempt", []))


def _norm(s: str) -> str:
    return re.sub(r"[^A-Z0-9]", "", s.upper())


def _section_linked_files(section: dict) -> list:
    """Repo files a live section links: GitHub blob/tree URLs into this repo
    (dirs recursed) plus page-relative hrefs under docs/api/capabilities/."""
    out = []
    for href in section["hrefs"]:
        m = _GH_HREF_RE.match(href)
        if m:
            p = REPO / m.group(1)
        elif href.startswith("http"):
            continue  # external non-repo deliverable
        else:
            p = (REPO / "docs" / "api" / "capabilities" / href).resolve()
        if p.is_dir():
            out += [f for f in sorted(p.rglob("*")) if f.is_file()]
        elif p.is_file():
            out.append(p)
    return out


# 10 — ratchet (a): section-kind SPECS ids <-> live section anchors, 1:1


def test_specs_section_ids_biject_live_sections():
    exempt = _onepager_exempt()
    live = {s["id"] for s in ci.load_sections(REPO)}
    covered = set(ci.load_pdf_specs(REPO))  # anchors with a sec-* SPECS entry
    assert exempt <= live, f"stale onepager_exempt entries: {sorted(exempt - live)}"
    orphans = covered - live
    assert not orphans, (
        f"sec-* SPECS entries with no live section: {sorted(orphans)}"
    )
    missing = live - covered - exempt
    assert not missing, (
        f"live sections without a kind='section' SPECS entry: {sorted(missing)} "
        "— author the entry in scripts/capabilities/build_onepagers.py or add "
        "the id to onepager_exempt in capabilities-clusters.yml (reviewed diff)"
    )


# 11 — ratchet (b): every section one-pager PDF is committed (pdf_gaps == []
# in the inventory is a SPECS-presence signal only; THIS closes the disk hole)


def test_every_section_pdf_committed():
    pdf_by_section = ci.load_pdf_specs(REPO)
    assert pdf_by_section, "no section-kind SPECS entries found"
    for sec, rel in sorted(pdf_by_section.items()):
        p = REPO / rel
        assert p.exists(), f"one-pager PDF for #{sec} not committed: {rel}"
        size = p.stat().st_size
        assert 0 < size < 2_000_000, f"{rel} size {size} outside (0, 2MB)"


# 12 — standards grounding (#1391 lesson): every standards token in a
# section-kind `std` line must grep-match a file that section links


def test_specs_standards_grounded():
    sections = {s["id"]: s for s in ci.load_sections(REPO)}
    script = REPO / "scripts" / "capabilities" / "build_onepagers.py"
    spec_ = importlib.util.spec_from_file_location("build_onepagers_g", script)
    bo = importlib.util.module_from_spec(spec_)
    spec_.loader.exec_module(bo)

    norm_cache: dict = {}

    def evidence(token: str, files: list) -> bool:
        want = _norm(token)
        for f in files:
            if f.suffix.lower() in _BINARY_SUFFIXES or f.stat().st_size > 2_000_000:
                continue
            if f not in norm_cache:
                try:
                    norm_cache[f] = _norm(f.read_text(errors="ignore"))
                except OSError:
                    norm_cache[f] = ""
            if want in norm_cache[f]:
                return True
        return False

    ungrounded = []
    checked = 0
    for entry in bo.SPECS:
        if entry.get("kind") != "section":
            continue
        anchor = entry["id"][len("sec-"):]
        section = sections.get(anchor)
        if section is None:
            continue  # covered by the bijection test
        files = _section_linked_files(section)
        for token in _STD_TOKEN_RE.findall(entry["std"]):
            checked += 1
            if not evidence(token, files):
                ungrounded.append((entry["id"], token))
    # LIVENESS. The `section is None -> continue` above is safe only while SOME
    # anchors resolve. Between #1573 and #1637 the census resolved NONE: every
    # anchor missed, all 42 standards claims skipped their evidence check, and
    # this test reported PASS for 15 days while verifying nothing. It was one of
    # the 4 "passing" tests in a shard that was 8/12 red. Assert we checked
    # something, so the vacuous case is loud instead of green.
    assert checked, (
        "standards-grounding guard checked ZERO tokens -- the census resolved no "
        "anchors, or its sections declare no hrefs. A green result here would be "
        "meaningless (dm#1637)"
    )
    assert not ungrounded, (
        f"standards named in `std` lines with zero grep evidence in the "
        f"section's linked files (overclaim per dm#1391): {ungrounded}"
    )
