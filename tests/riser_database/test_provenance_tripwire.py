"""#1259 provenance-token tripwire (plan v2 D4) — in-context hard gate.

Auto-extracts provenance name tokens and project numbers from the PRIVATE
registry's linked source pages (slugs + `sources:` frontmatter) and asserts
none appear, case/separator-normalized, in ANY public riser-DB artifact.

This is the scoped, regex-capable complement to the workspace-hub deny list:
famous public-domain field/rig names cannot live in the GLOBAL deny list
(they legitimately appear in worldenergydata's regulatory datasets), so THIS
test is where they are enforced for digitalmodel. Loud skip standalone.
"""
from __future__ import annotations

import os
import re
from pathlib import Path

import pytest

from digitalmodel.riser_database.loader import DEFAULT_DB_ROOT

REPO_ROOT = Path(__file__).resolve().parents[2]
REGISTRY_REL = "wikis/riser-projects/wiki/datasets/stackup-registry.md"
_KNOWN_WIKI_CLONES = (
    Path("/mnt/local-analysis/llm-wiki"),  # abs-path-allowed
    Path.home() / "workspace-hub" / "llm-wiki",
)

#: Generic engineering/document words that appear in slugs but are not
#: provenance names. Anything NOT here and >=4 chars is treated as a name.
_STOPWORDS = {
    "analysis", "assessment", "basis", "compliant", "concept", "conductors",
    "data", "design", "deep", "development", "drilling", "engineering",
    "fatigue", "foundation", "joint", "library", "mode", "multi", "normalization",
    "overview", "presentation", "proj", "projects", "registry", "results",
    "riser", "risers", "sidetrack", "source", "sources", "stackup", "study",
    "summary", "ttr", "well", "wellhead", "wellheads", "wells", "wiki", "wikis",
    "workbook", "client", "share", "operational", "market",
}

_PUBLIC_ARTIFACTS = (
    DEFAULT_DB_ROOT / "config_catalog.csv",
    DEFAULT_DB_ROOT / "standards_crosswalk.csv",
    DEFAULT_DB_ROOT / "material_sn_scf_dff.csv",
    DEFAULT_DB_ROOT / "riser_stackup.csv",
    DEFAULT_DB_ROOT / "rig_riser_interface.csv",
    DEFAULT_DB_ROOT / "manifest.yaml",
    REPO_ROOT / "scripts" / "riser_database" / "sources.yml",
    REPO_ROOT / "scripts" / "riser_database" / "build_tables.py",
    REPO_ROOT / "docs" / "riser_database.md",
    REPO_ROOT / "src" / "digitalmodel" / "riser_database" / "loader.py",
    REPO_ROOT / "src" / "digitalmodel" / "riser_database" / "getters.py",
)


def _normalize(text: str) -> str:
    return re.sub(r"[\s_\-]+", "", text).casefold()


def _wiki_base() -> Path:
    candidates = []
    env = os.environ.get("LLM_WIKI_PATH")
    if env:
        candidates.append(Path(env))
    candidates.extend(_KNOWN_WIKI_CLONES)
    for base in candidates:
        if (base / REGISTRY_REL).is_file():
            return base
    pytest.skip(
        "llm-wiki not available (standalone mode) — the provenance-token "
        "tripwire is part of the in-context merge gate"
    )


def _provenance_tokens() -> tuple[set[str], set[str]]:
    """(name_tokens, number_tokens) from registry-linked source pages."""
    base = _wiki_base()
    registry = (base / REGISTRY_REL).read_text()
    linked = set(re.findall(r"\.\./sources/([a-z0-9\-]+)\.md", registry))
    names: set[str] = set()
    numbers: set[str] = set()

    def harvest(text: str) -> None:
        for word in re.findall(r"[A-Za-z]{4,}", text):
            if word.casefold() not in _STOPWORDS:
                names.add(word.casefold())
        for num in re.findall(r"\b[1-9]\d{3,4}\b", text):
            numbers.add(num)

    for slug in linked:
        harvest(slug.replace("-", " "))
        page = base / "wikis/riser-projects/wiki/sources" / f"{slug}.md"
        if not page.is_file():
            continue
        frontmatter = page.read_text().split("---", 2)[1]
        for line in frontmatter.splitlines():
            if "ace-share:" in line:
                harvest(line)
    assert names or numbers, "token extraction came back empty — parser drift?"
    return names, numbers


def test_no_provenance_tokens_in_public_artifacts():
    names, numbers = _provenance_tokens()
    violations = []
    for path in _PUBLIC_ARTIFACTS:
        if not path.is_file():
            continue
        raw = path.read_text()
        normalized = _normalize(raw)
        for token in names:
            if _normalize(token) in normalized:
                violations.append((path.name, f"name-token #{sorted(names).index(token)}"))
        for num in numbers:
            if re.search(rf"\b{num}\b", raw):
                violations.append((path.name, f"project-number {num}"))
    assert not violations, (
        "provenance tokens leaked into public artifacts (tokens not echoed): "
        f"{violations}"
    )
