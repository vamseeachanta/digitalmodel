#!/usr/bin/env python3
"""ABOUTME: Capabilities IA inventory builder (issue #1444) — census, clusters,
ABOUTME: reference-index joins, recency, spec rendering, and --check freshness.

Reads (never edits) the machine source-of-truth files under
docs/capability-map/, and emits:

  docs/capability-map/capabilities-inventory.json   (machine contract)
  docs/capability-map/capabilities-ia-spec-1444.md  (rendered spec; tables are
                                                     generated, never hand-typed)

``--check`` regenerates in memory and exits non-zero if the committed
inventory drifted (CI freshness gate via the tests/DOMAINS.md capabilities row).

The census used to scrape docs/api/capabilities/index.html, which served double
duty: it rendered the gallery AND declared the IA. #1573 (C10) retired the
rendering job -- aceengineer.com/capabilities is now the single canonical
surface -- and the declaration went with it, taking every id, title and href
down with a page nobody meant to use as a schema (dm#1637). The IA now lives in
capabilities-sections.yml, where a presentation decision cannot delete it.

Recency comes ONLY from capabilities-added.yml (explicit, PR-evidenced
metadata): the repo's git history was truncated by the 2026-07 slim, so no
git-derived dating is valid.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import re
from pathlib import Path

import yaml

SCHEMA_VERSION = "1.0"
HERE = Path(__file__).resolve()
REPO_DEFAULT = HERE.parents[2]

CAP_MAP = "docs/capability-map"
SECTIONS_YML = "docs/capability-map/capabilities-sections.yml"
CANONICAL_SURFACE = "https://www.aceengineer.com/capabilities/"
FROZEN_DIR = "capabilities/api"  # frozen work-item assets — excluded from discovery


class CensusError(RuntimeError):
    """Census failures fail closed with an actionable message."""


# ---------------------------------------------------------------------------
# census
# ---------------------------------------------------------------------------

#: Anchors are cited by external links, so the id vocabulary is a contract, not
#: a naming preference. Reject anything that could not survive as a URL fragment.
_ID_RE = re.compile(r"^[a-z0-9]+(?:-[a-z0-9]+)*$")


def load_sections(repo: Path | str = REPO_DEFAULT) -> list[dict]:
    """Raw section records from capabilities-sections.yml, unvalidated."""
    doc = yaml.safe_load((Path(repo) / SECTIONS_YML).read_text())
    return [
        {
            "id": s["id"],
            "title": s.get("title", ""),
            "hrefs": list(s.get("hrefs") or []),
        }
        for s in (doc.get("sections") or [])
    ]


def census(sections: list[dict]) -> list[dict]:
    """Schema-checked census of the declared sections. Fail closed.

    Guards what the YAML cannot express on its own: unique, URL-safe anchors
    and a non-empty title per section. The nav/section bijection the HTML
    census enforced is gone because the nav was the *other* half of a
    duplicated declaration -- one list cannot disagree with itself.
    """
    if not sections:
        raise CensusError(f"no sections declared in {SECTIONS_YML}")
    ids = [s["id"] for s in sections]
    dupes = {i for i in ids if ids.count(i) > 1}
    if dupes:
        raise CensusError(f"duplicate section ids: {sorted(dupes)}")
    malformed = sorted(i for i in ids if not _ID_RE.match(str(i)))
    if malformed:
        raise CensusError(
            f"section ids must be lowercase kebab-case URL anchors: {malformed}"
        )
    untitled = sorted(s["id"] for s in sections if not str(s["title"]).strip())
    if untitled:
        raise CensusError(f"sections with no title: {untitled}")
    return sections


# ---------------------------------------------------------------------------
# discovery + joins
# ---------------------------------------------------------------------------


def discover_explorers(repo: Path) -> list[str]:
    """Live explorer pages across docs/api/**, excluding the frozen
    capabilities/api work-item assets."""
    root = repo / "docs" / "api"
    return sorted(
        str(p.relative_to(repo))
        for p in root.rglob("*-explorer.html")
        if FROZEN_DIR not in str(p)
    )


def registered_site_paths(repo: Path | str = REPO_DEFAULT) -> set[str]:
    """Every page the IA registers, as a site path relative to ``docs/api/``.

    Section hrefs are written page-relative to docs/api/capabilities/ (that is
    where the gallery used to live, and the standards-grounding ratchet still
    resolves them from there). Builders declare their page as ``SITE_PATH``
    relative to docs/api/, so this is the join that answers "is this page
    reachable from the capability IA at all?" — the question the operability
    explorer and monitor registration tests ask (dm#1639).
    """
    repo = Path(repo)
    base = (repo / "docs" / "api" / "capabilities").resolve()
    api_root = (repo / "docs" / "api").resolve()
    out: set[str] = set()
    for section in load_sections(repo):
        for href in section["hrefs"]:
            if href.startswith(("http://", "https://", "mailto:")):
                continue
            target = (base / href).resolve()
            try:
                out.add(str(target.relative_to(api_root)))
            except ValueError:
                continue  # links outside docs/api/ are not site pages
    return out


def load_pdf_specs(repo: Path) -> dict[str, str]:
    """section id -> pdf relpath, from build_onepagers SPECS (importlib on the
    real path — the module builds dict()s, so AST parsing will not work)."""
    script = repo / "scripts" / "capabilities" / "build_onepagers.py"
    spec = importlib.util.spec_from_file_location("build_onepagers_ro", script)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    out = {}
    for entry in mod.SPECS:
        if entry.get("kind") == "section" and entry["id"].startswith("sec-"):
            sec = entry["id"][len("sec-") :]
            out[sec] = f"docs/api/capabilities/pdf/{entry['id']}.pdf"
    return out


def load_clusters(repo: Path) -> dict:
    return yaml.safe_load((repo / CAP_MAP / "capabilities-clusters.yml").read_text())


def load_added(repo: Path) -> dict:
    return yaml.safe_load((repo / CAP_MAP / "capabilities-added.yml").read_text())


def build_inventory(repo: Path | str = REPO_DEFAULT) -> dict:
    repo = Path(repo)
    sections = census(load_sections(repo))
    clusters_doc = load_clusters(repo)
    added_doc = load_added(repo)

    cluster_of: dict[str, str] = {}
    for c in clusters_doc["clusters"]:
        for sid in c["sections"]:
            if sid in cluster_of:
                raise CensusError(f"section {sid} assigned to two clusters")
            cluster_of[sid] = c["key"]

    explorers = discover_explorers(repo)
    aliases = clusters_doc.get("explorer_aliases", {})  # explorer relpath -> section id
    pdf_by_section = load_pdf_specs(repo)
    added = added_doc.get("sections", {})

    # explorer -> section: in-section href match first, then alias map
    by_name = {Path(e).name: e for e in explorers}
    owner: dict[str, str] = {}
    for s in sections:
        for href in s["hrefs"]:
            name = Path(href).name
            if name in by_name:
                e = by_name[name]
                if e in owner and owner[e] != s["id"]:
                    raise CensusError(
                        f"explorer {e} linked from two sections "
                        f"({owner[e]}, {s['id']}) — add an explorer_aliases entry"
                    )
                owner[e] = s["id"]
    for e, sid in aliases.items():
        owner.setdefault(e, sid)

    inv_sections = []
    for s in sections:
        sid = s["id"]
        if sid not in cluster_of:
            raise CensusError(f"section {sid} missing from capabilities-clusters.yml")
        inv_sections.append(
            {
                "id": sid,
                "title": s["title"],
                "cluster": cluster_of[sid],
                "explorers": sorted(e for e, o in owner.items() if o == sid),
                "pdf": pdf_by_section.get(sid),
                "added": added.get(sid, "unknown"),
            }
        )

    stale_clusters = set(cluster_of) - {s["id"] for s in sections}
    if stale_clusters:
        raise CensusError(
            f"cluster map lists unknown sections: {sorted(stale_clusters)}"
        )

    known = [
        s
        for s in inv_sections
        if s["added"] != "unknown" and isinstance(s["added"], dict)
    ]
    recent = sorted(known, key=lambda s: s["added"]["date"], reverse=True)
    n = added_doc.get("recent_n", 8)

    return {
        "schema_version": SCHEMA_VERSION,
        "source": SECTIONS_YML,
        "sections": inv_sections,
        "clusters": [
            {k: c[k] for k in ("key", "label", "value_statement", "sections")}
            for c in clusters_doc["clusters"]
        ],
        "pdf_gaps": [s["id"] for s in inv_sections if s["pdf"] is None],
        "unlinked_explorers": sorted(set(explorers) - set(owner)),
        "recent": [
            {"id": s["id"], "date": s["added"]["date"], "pr": s["added"]["pr"]}
            for s in recent[:n]
        ],
    }


# ---------------------------------------------------------------------------
# outputs
# ---------------------------------------------------------------------------


def _canon(doc: dict) -> str:
    return json.dumps(doc, indent=2, sort_keys=True, ensure_ascii=False) + "\n"


def write_inventory(inv: dict, out_path: Path) -> None:
    Path(out_path).write_text(_canon(inv))


def check(repo: Path | str, committed_path: Path | str) -> int:
    """0 when the committed inventory matches a fresh regeneration."""
    fresh = _canon(build_inventory(repo))
    committed = Path(committed_path).read_text()
    return 0 if fresh == committed else 1


def render_spec(inv: dict) -> str:
    lines = [
        "# Capabilities page — information-architecture spec (issue #1444)",
        "",
        "> GENERATED tables (source of truth: `capabilities-sections.yml`, "
        "`capabilities-clusters.yml`, `capabilities-added.yml`). Regenerate "
        "with:",
        "> `.venv/bin/python scripts/capabilities/build_capabilities_inventory.py`",
        "> Presentation lives at "
        f"[{CANONICAL_SURFACE}]({CANONICAL_SURFACE}) (C10, #1573) — this spec "
        "describes the IA, and does not edit the rendered surface.",
        "",
        f"Sections declared: **{len(inv['sections'])}** · clusters: "
        f"**{len(inv['clusters'])}** · PDF coverage gaps: "
        f"**{len(inv['pdf_gaps'])}** · unlinked explorers: "
        f"**{len(inv['unlinked_explorers'])}**",
        "",
        "## Cluster taxonomy",
        "",
    ]
    by_id = {s["id"]: s for s in inv["sections"]}
    for c in inv["clusters"]:
        lines.append(f"### {c['label']} (`{c['key']}`)")
        lines.append(f"*{c['value_statement']}*")
        lines.append("")
        for sid in c["sections"]:
            s = by_id[sid]
            lines.append(f"- [`#{sid}`]({CANONICAL_SURFACE}#{sid}) — {s['title']}")
        lines.append("")
    lines += [
        "## Reference index (citable front doors)",
        "",
        "| Section | Cluster | Live explorer(s) | 1-pager PDF | Added |",
        "|---|---|---|---|---|",
    ]
    for s in inv["sections"]:
        exp = "<br>".join(f"`{e}`" for e in s["explorers"]) or "*gap*"
        pdf = f"`{s['pdf']}`" if s["pdf"] else "*gap*"
        if isinstance(s["added"], dict):
            added = f"{s['added']['date']} (#{s['added']['pr']})"
        else:
            added = "unknown"
        lines.append(
            f"| [`#{s['id']}`]({CANONICAL_SURFACE}#{s['id']}) "
            f"| {s['cluster']} | {exp} | {pdf} | {added} |"
        )
    lines += [
        "",
        f"**PDF gap set ({len(inv['pdf_gaps'])}):** "
        + ", ".join(f"`{g}`" for g in inv["pdf_gaps"]),
        "",
        "**Unlinked explorers:** "
        + (", ".join(f"`{e}`" for e in inv["unlinked_explorers"]) or "none"),
        "",
        "## Recently added (strip content model)",
        "",
        "Display contract: top-N below (N from `capabilities-added.yml:recent_n`),"
        " newest first; entries without PR evidence stay off the strip (honest"
        " `unknown`, never a fabricated date — repo history was truncated by the"
        " 2026-07 git slim, so recency is explicit metadata).",
        "",
    ]
    for r in inv["recent"]:
        lines.append(f"- `#{r['id']}` — {r['date']} (PR #{r['pr']})")
    lines += [
        "",
        "## Anchor-stability contract",
        "",
        "The revamp MUST preserve every anchor below (external links already "
        "cite them). Enforcement pattern: route manifest + link-graph CI gate "
        "(see worldenergydata #850).",
        "",
        "```",
        " ".join(s["id"] for s in inv["sections"]),
        "```",
        "",
    ]
    return "\n".join(lines)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--check", action="store_true")
    ap.add_argument("--repo", default=str(REPO_DEFAULT))
    args = ap.parse_args()
    repo = Path(args.repo)
    committed = repo / CAP_MAP / "capabilities-inventory.json"
    if args.check:
        rc = check(repo, committed)
        print("fresh" if rc == 0 else "DRIFTED — regenerate and commit")
        return rc
    inv = build_inventory(repo)
    write_inventory(inv, committed)
    (repo / CAP_MAP / "capabilities-ia-spec-1444.md").write_text(render_spec(inv))
    print(
        f"  wrote capabilities-inventory.json ({len(inv['sections'])} sections, "
        f"{len(inv['pdf_gaps'])} pdf gaps, "
        f"{len(inv['unlinked_explorers'])} unlinked explorers)"
    )
    print("  wrote capabilities-ia-spec-1444.md")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
