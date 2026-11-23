"""Versioned-routing schema for docs/registry/workflows.yaml (schema_version 2).

Each workflow row MAY carry an optional algorithm-version triple
(``version`` / ``status`` / ``latest``); absence means v1 / stable / latest.
These invariants guard the multi-version case so a caller can resolve a pinned
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


class _DuplicateKeyLoader(yaml.SafeLoader):
    """SafeLoader that refuses a mapping with a repeated key.

    ``yaml.safe_load`` keeps the last value of a repeated key silently. Rows
    are added to this file by many concurrent pull requests, and a merge
    conflict resolved by keeping both sides of an edited row leaves repeated
    keys that would otherwise load without error.
    """


def _construct_unique_mapping(loader: yaml.SafeLoader, node: yaml.MappingNode) -> dict:
    seen: set = set()
    for key_node, _ in node.value:
        key = loader.construct_object(key_node, deep=True)
        if key in seen:
            raise ValueError(f"repeated key {key!r} at line {key_node.start_mark.line + 1}")
        seen.add(key)
    return loader.construct_mapping(node, deep=True)


_DuplicateKeyLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _construct_unique_mapping
)


def test_registry_has_no_repeated_keys() -> None:
    strict = yaml.load(REGISTRY_PATH.read_text(encoding="utf-8"), Loader=_DuplicateKeyLoader)
    assert strict == _load()


def test_repeated_key_loader_refuses_a_doubled_row_field() -> None:
    doubled = "workflows:\n  - id: a\n    input: x.yml\n    input: y.yml\n"
    try:
        yaml.load(doubled, Loader=_DuplicateKeyLoader)
    except ValueError as error:
        assert "repeated key 'input' at line 4" in str(error)
    else:
        raise AssertionError("a repeated row key loaded without error")


def test_registry_row_inputs_and_tests_exist() -> None:
    repo_root = REGISTRY_PATH.parents[2]
    for row in _load()["workflows"]:
        wid = row["id"]
        assert row.get("input") and (repo_root / row["input"]).is_file(), f"{wid}: input"
        test_file = str(row.get("test", "")).split("::", 1)[0]
        assert test_file and (repo_root / test_file).is_file(), f"{wid}: test file"
