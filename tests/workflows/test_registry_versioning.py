"""Versioned-routing schema for docs/registry/workflows.yaml (schema_version 2).

Each workflow row MAY carry an optional algorithm-version triple
(``version`` / ``status`` / ``latest``); absence means v1 / stable / latest.
These invariants guard the multi-version case so Deckhand can resolve a pinned
``<repo>:<id>@N`` or the latest-stable default unambiguously.
"""

from pathlib import Path

import yaml

REGISTRY_PATH = Path(__file__).resolve().parents[2] / "docs" / "registry" / "workflows.yaml"
_VERSION_STATUSES = {"stable", "deprecated", "experimental", "retired"}


def _load() -> dict:
    return yaml.safe_load(REGISTRY_PATH.read_text(encoding="utf-8"))


def test_registry_is_schema_version_2() -> None:
    assert _load()["schema_version"] == 2


def test_workflow_registry_version_invariants() -> None:
    workflows = _load()["workflows"]
    groups: dict[str, list[dict]] = {}
    for row in workflows:
        groups.setdefault(str(row["id"]), []).append(row)

    for wid, rows in groups.items():
        # Any version field present must be a positive int with a valid status.
        for row in rows:
            if "version" in row:
                assert isinstance(row["version"], int) and row["version"] >= 1, wid
            assert row.get("status", "stable") in _VERSION_STATUSES, wid
        if len(rows) == 1:
            continue
        # A workflow id with multiple rows is a genuine multi-version workflow:
        # every row must pin a distinct version and exactly one latest-stable.
        versions = [row.get("version") for row in rows]
        assert all(isinstance(v, int) for v in versions), f"{wid}: versions required"
        assert len(set(versions)) == len(versions), f"{wid}: duplicate versions"
        latest = [row for row in rows if row.get("latest")]
        assert len(latest) == 1, f"{wid}: need exactly one latest:true row"
        assert latest[0].get("status", "stable") == "stable", f"{wid}: latest not stable"
