"""Suite 3 (#1245): standards_crosswalk identifiers resolve in the llm-wiki
standards registry (in-context; loud skip standalone).

Registry gotcha guarded here: API reclassified RP 2RD as STD 2RD — the
registry key is ``api-std-2rd`` and there is NO ``api-rp-2rd`` entry.
"""
from __future__ import annotations

import json
import os
from pathlib import Path

import pytest

from digitalmodel.riser_database.loader import RiserDatabase

_KNOWN_WIKI_CLONES = (
    Path("/mnt/local-analysis/llm-wiki"),  # abs-path-allowed
    Path.home() / "workspace-hub" / "llm-wiki",
)


def _registry() -> dict:
    candidates = []
    env = os.environ.get("LLM_WIKI_PATH")
    if env:
        candidates.append(Path(env))
    candidates.extend(_KNOWN_WIKI_CLONES)
    for base in candidates:
        path = base / "data" / "standards-registry.json"
        if path.is_file():
            return json.loads(path.read_text())
    pytest.skip(
        "llm-wiki standards-registry.json not available (standalone mode) — "
        "this suite is part of the in-context merge gate and must run green "
        "in a workspace-hub checkout before the PR"
    )


def test_registry_stats_sane():
    registry = _registry()
    # Never hardcode the standards count — read it from the registry itself.
    assert isinstance(registry["stats"]["standards"], int)
    assert registry["stats"]["standards"] > 0


def test_every_crosswalk_code_id_resolves_in_registry():
    registry = _registry()
    by_id = {entry["code_id"]: entry for entry in registry["standards"]}
    failures = []
    for row in RiserDatabase.load().crosswalk_rows:
        entry = by_id.get(row.code_id)
        if entry is None:
            failures.append((row.code_id, "not in registry"))
            continue
        if row.wiki_path not in entry["pages"]:
            failures.append((row.code_id, f"wiki_path not in pages[]: {row.wiki_path}"))
        if row.family_key != entry["family_key"]:
            failures.append((row.code_id, "family_key mismatch"))
    assert not failures, failures


def test_2rd_uses_the_std_identifier():
    ids = {row.code_id for row in RiserDatabase.load().crosswalk_rows}
    assert "api-std-2rd" in ids
    assert "api-rp-2rd" not in ids
    registry_ids = {entry["code_id"] for entry in _registry()["standards"]}
    assert "api-rp-2rd" not in registry_ids  # guards against silent re-keying
    assert "api-std-2rd" in registry_ids


def test_registry_revision_mirrors_registry():
    registry = _registry()
    by_id = {entry["code_id"]: entry for entry in registry["standards"]}
    for row in RiserDatabase.load().crosswalk_rows:
        revisions = by_id[row.code_id]["revisions"]
        assert row.registry_revision in revisions, (
            row.code_id,
            row.registry_revision,
            revisions,
        )
