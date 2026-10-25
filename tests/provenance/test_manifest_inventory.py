# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Ancestry oracles use synthetic commit graphs, never the checkout history."""
import importlib.util
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest
import yaml

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/provenance/inventory_atlas_manifests.py"
spec = importlib.util.spec_from_file_location("manifest_inventory", SCRIPT)
inventory = importlib.util.module_from_spec(spec)
spec.loader.exec_module(inventory)


def git(root, *args):
    env = {key: value for key, value in os.environ.items() if not key.startswith("GIT_")}
    return subprocess.check_output(["git", "-C", str(root), *args], text=True,
                                   encoding="utf-8", env=env).strip()


@pytest.fixture
def history(tmp_path):
    return make_history(tmp_path)


def make_history(tmp_path):
    root = tmp_path / "repository"
    root.mkdir(parents=True)
    git(root, "init", "--initial-branch=main")
    # Construct disposable commit objects without Git identity configuration or
    # environment overrides. The synthetic ident has no address or domain.
    # Git commit-object format: tree, optional parents, ident lines, blank, text.
    tree = git(root, "mktree")

    def commit(message, *parents):
        parent_lines = "".join(f"parent {parent}\n" for parent in parents)
        data = (f"tree {tree}\n{parent_lines}"
                "author Fixture <fixture> 0 +0000\n"
                "committer Fixture <fixture> 0 +0000\n\n" + message)
        return subprocess.check_output(
            ["git", "-C", str(root), "hash-object", "-t", "commit", "-w", "--stdin"],
            input=data, text=True, encoding="utf-8",
            env={key: value for key, value in os.environ.items() if not key.startswith("GIT_")},
        ).strip()

    first = commit("reachable fixture\n")
    tip = commit("tip fixture\n", first)
    orphan = commit("orphan fixture\n")
    git(root, "update-ref", "refs/heads/main", tip)
    git(root, "update-ref", "refs/remotes/origin/main", tip)
    for label, revision in (("reachable", first), ("orphan", orphan), ("unknown", "0" * len(tip))):
        path = root / "atlases" / label / "fixture" / "manifest.yaml"
        path.parent.mkdir(parents=True)
        path.write_text(yaml.safe_dump({"provenance": {"code_version": revision}}),
                        encoding="utf-8", newline="\n")
    return root, first, tip, orphan


def snapshot(root):
    """Include caller work files, objects, refs, reflogs, config and index."""
    return {path.relative_to(root).as_posix(): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in root.rglob("*") if path.is_file()}


def bind_caller(monkeypatch, root):
    for name, path in {
        "GIT_DIR": root / ".git",
        "GIT_WORK_TREE": root,
        "GIT_COMMON_DIR": root / ".git",
        "GIT_INDEX_FILE": root / ".git/index",
        "GIT_OBJECT_DIRECTORY": root / ".git/objects",
        "GIT_ALTERNATE_OBJECT_DIRECTORIES": root / ".git/objects",
    }.items():
        monkeypatch.setenv(name, str(path))


def test_history_fixture_preserves_bound_caller(history, tmp_path, monkeypatch):
    caller, _, _, _ = history
    protected = caller / "protected.txt"
    protected.write_text("protected content\n", encoding="utf-8", newline="\n")
    git(caller, "add", "--", "protected.txt")
    before = snapshot(caller)
    bind_caller(monkeypatch, caller)
    target, _, tip, _ = make_history(tmp_path / "isolated")
    assert snapshot(caller) == before, "fixture changed caller files or Git metadata"
    assert (target / ".git").is_dir()
    assert git(target, "rev-parse", "HEAD") == tip
    (target / "protected.txt").write_text("target content\n", encoding="utf-8", newline="\n")
    git(target, "add", "--", "protected.txt")
    assert snapshot(caller) == before, "fixture Git child changed caller index"


def test_inventory_preserves_bound_caller(history, tmp_path, monkeypatch):
    caller, _, _, _ = history
    target, _, tip, _ = make_history(tmp_path / "isolated")
    before = snapshot(caller)
    bind_caller(monkeypatch, caller)
    report = inventory.inventory_manifests(target)
    assert snapshot(caller) == before, "inventory changed caller files or Git metadata"
    assert report["history_state"] == "COMPLETE"
    assert report["inventory_revision"] == tip
    assert report["counts"] == {"REACHABLE": 1, "UNREACHABLE": 1, "UNAVAILABLE": 1}


def test_historical_debt_survives_absent_non_ancestor_objects(history, tmp_path):
    root, _, _, orphan = history
    clone = tmp_path / "retained-history"
    git(root, "clone", "--no-local", root.as_uri(), str(clone))
    # A normal complete clone retains main, but no unreferenced orphan object.
    probe = subprocess.run(
        ["git", "-C", str(clone), "cat-file", "-e", orphan],
        capture_output=True, check=False,
        env={key: value for key, value in os.environ.items() if not key.startswith("GIT_")},
    )
    assert probe.returncode != 0
    for path in root.glob(inventory.SEARCH_PATTERN):
        target = clone / path.relative_to(root)
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(path.read_bytes())
    previous = inventory.inventory_manifests(root)
    exceptions = {row["manifest"]: {
        "code_version": row["code_version"], "status": row["status"],
        "inventory_reason": row["reason"], "reason": "Recorded historical debt.",
    } for row in previous["manifests"] if row["status"] != "REACHABLE"}
    current = inventory.inventory_manifests(clone)
    assert current["history_state"] == "COMPLETE"
    assert current["counts"] == {"REACHABLE": 1, "UNREACHABLE": 0, "UNAVAILABLE": 2}
    assert not inventory.manifest_revision_failures(current, exceptions)
    orphan_row = next(row for row in current["manifests"] if row["code_version"] == orphan)
    assert (orphan_row["status"], orphan_row["reason"]) == ("UNAVAILABLE", "revision_unknown")


def test_inventory_reports_graph_and_exact_pattern(history):
    root, first, tip, orphan = history
    result = inventory.inventory_manifests(root)
    assert result["search_pattern"] == "atlases/*/*/manifest.yaml"
    assert result["inventory_revision"] == tip
    rows = {Path(row["manifest"]).parts[1]: row for row in result["manifests"]}
    assert rows["reachable"]["code_version"] == first
    assert rows["reachable"]["status"] == "REACHABLE"
    assert rows["orphan"]["code_version"] == orphan
    assert rows["orphan"]["status"] == "UNREACHABLE"
    assert rows["unknown"]["status"] == "UNAVAILABLE"
    assert rows["unknown"]["reason"] == "revision_unknown"
    assert result["counts"] == {"REACHABLE": 1, "UNREACHABLE": 1, "UNAVAILABLE": 1}


def test_shallow_clone_never_claims_complete_ancestry(history, tmp_path):
    root, _, _, _ = history
    clone = tmp_path / "shallow"
    git(root, "clone", "--depth=1", root.as_uri(), str(clone))
    manifest = clone / "atlases/toy/fixture/manifest.yaml"
    manifest.parent.mkdir(parents=True)
    manifest.write_text(yaml.safe_dump({"provenance": {"code_version": git(clone, "rev-parse", "HEAD")}}),
                        encoding="utf-8", newline="\n")
    report = inventory.inventory_manifests(clone)
    assert report["history_state"] == "SHALLOW"
    assert report["manifests"][0]["status"] == "UNAVAILABLE"
    assert report["manifests"][0]["reason"] == "history_shallow"


def test_source_archive_and_missing_retained_ref_are_unavailable(history, tmp_path):
    root, _, _, _ = history
    git(root, "update-ref", "-d", "refs/remotes/origin/main")
    result = inventory.inventory_manifests(root)
    assert result["history_state"] == "RETAINED_REF_UNAVAILABLE"
    assert all(row["status"] == "UNAVAILABLE" for row in result["manifests"])
    archive = tmp_path / "archive"
    manifest = archive / "atlases/toy/fixture/manifest.yaml"
    manifest.parent.mkdir(parents=True)
    manifest.write_text("provenance: {code_version: abcdef}\n", encoding="utf-8", newline="\n")
    result = inventory.inventory_manifests(archive)
    assert result["history_state"] == "SOURCE_ARCHIVE"
    assert result["inventory_revision"] is None
    assert result["counts"]["UNAVAILABLE"] == 1


def test_cli_emits_json(history):
    root, _, tip, _ = history
    output = subprocess.check_output(
        [__import__("sys").executable, str(SCRIPT), "--repo-root", str(root)], text=True,
        encoding="utf-8",
        env={key: value for key, value in os.environ.items() if not key.startswith("GIT_")},
    )
    assert json.loads(output)["inventory_revision"] == tip


def test_git_operational_failure_never_becomes_non_ancestor(history, monkeypatch):
    root, _, _, _ = history
    original = inventory._git

    def failing_git(repo, *args):
        if args[0] == "merge-base":
            return subprocess.CompletedProcess(args, returncode=2, stdout="", stderr="unavailable")
        return original(repo, *args)

    monkeypatch.setattr(inventory, "_git", failing_git)
    row = next(row for row in inventory.inventory_manifests(root)["manifests"]
               if "/reachable/" in row["manifest"])
    assert row["status"] == "UNAVAILABLE"
    assert row["reason"] == "ancestry_check_unavailable"


@pytest.mark.parametrize("returncode", (1, 2, 128))
def test_revision_lookup_operational_failure_cannot_match_debt(history, monkeypatch, returncode):
    root, first, _, _ = history
    original = inventory._git

    def failing_git(repo, *args):
        if args[0] == "rev-parse" and args[-1] == f"{first}^{{commit}}":
            return subprocess.CompletedProcess(args, returncode, stdout="", stderr="lookup failed")
        return original(repo, *args)

    monkeypatch.setattr(inventory, "_git", failing_git)
    report = inventory.inventory_manifests(root)
    row = next(row for row in report["manifests"] if row["code_version"] == first)
    assert row["status"] == "UNAVAILABLE"
    assert row["reason"] == "revision_lookup_unavailable"
    exception = {"code_version": first, "status": "UNAVAILABLE",
                 "inventory_reason": "revision_unknown", "reason": "Historical debt."}
    assert inventory.manifest_revision_failures(report, {row["manifest"]: exception})


@pytest.mark.parametrize("corrupt", (False, True))
def test_non_commit_or_corrupt_object_cannot_match_historical_debt(history, corrupt):
    root, _, _, _ = history
    tree = git(root, "rev-parse", "HEAD^{tree}")
    protected = root / ".git/objects" / tree[:2] / tree[2:]
    before = hashlib.sha256(protected.read_bytes()).hexdigest()
    if corrupt:
        # Create a new corrupt fixture; existing Git objects remain read-only.
        tree = "a" * len(tree)
        object_path = root / ".git/objects" / tree[:2] / tree[2:]
        object_path.parent.mkdir(exist_ok=True)
        object_path.write_bytes(b"invalid synthetic object\n")
    manifest = root / "atlases/reachable/fixture/manifest.yaml"
    manifest.write_text(yaml.safe_dump({"provenance": {"code_version": tree}}),
                        encoding="utf-8", newline="\n")
    try:
        report = inventory.inventory_manifests(root)
        row = next(row for row in report["manifests"] if row["code_version"] == tree)
        assert (row["status"], row["reason"]) == ("UNAVAILABLE", "revision_lookup_unavailable")
        exception = {"code_version": tree, "status": "UNAVAILABLE",
                     "inventory_reason": "revision_unknown", "reason": "Historical debt."}
        assert inventory.manifest_revision_failures(report, {row["manifest"]: exception})
    finally:
        assert hashlib.sha256(protected.read_bytes()).hexdigest() == before


def test_git_absent_is_explicit(history, monkeypatch):
    root, _, _, _ = history

    def unavailable(*args):
        raise FileNotFoundError("Git unavailable")

    monkeypatch.setattr(inventory, "_git", unavailable)
    report = inventory.inventory_manifests(root)
    assert report["history_state"] == "GIT_UNAVAILABLE"
    assert report["counts"]["UNAVAILABLE"] == len(report["manifests"])


@pytest.mark.parametrize("text, reason", (
    ("provenance: {}\n", "revision_missing"),
    ("provenance: {code_version: '--help'}\n", "revision_invalid"),
    ("[unterminated", "manifest_invalid"),
))
def test_bad_manifest_fails_closed(history, text, reason):
    root, _, _, _ = history
    path = root / "atlases/reachable/fixture/manifest.yaml"
    path.write_text(text, encoding="utf-8", newline="\n")
    row = next(row for row in inventory.inventory_manifests(root)["manifests"]
               if "/reachable/" in row["manifest"])
    assert row["status"] == "UNAVAILABLE"
    assert row["reason"] == reason


@pytest.mark.parametrize("after, returncode", (
    ('values: [1, 2]\n', 0),
    ('values: [1, 3]\n', 1),
    ('values: [1, 3]\nvalues: [1, 2]\n', 1),
))
def test_yaml_formatting_check_rejects_semantic_changes_and_duplicate_keys(tmp_path, after, returncode):
    before_path, after_path = tmp_path / "before.yaml", tmp_path / "after.yaml"
    before_path.write_text("values:\n  - 1\n  - 2\n", encoding="utf-8", newline="\n")
    after_path.write_text(after, encoding="utf-8", newline="\n")
    checker = SCRIPT.parents[2] / "scripts/enforcement/check_yaml_semantic_only.py"
    result = subprocess.run(
        [sys.executable, str(checker), str(before_path), str(after_path)],
        capture_output=True, text=True, encoding="utf-8", check=False,
    )
    assert result.returncode == returncode
