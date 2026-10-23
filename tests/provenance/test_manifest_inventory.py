"""Ancestry oracles use synthetic commit graphs, never the checkout history."""
import importlib.util
import json
from pathlib import Path
import subprocess

import pytest
import yaml

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/provenance/inventory_atlas_manifests.py"
spec = importlib.util.spec_from_file_location("manifest_inventory", SCRIPT)
inventory = importlib.util.module_from_spec(spec)
spec.loader.exec_module(inventory)


def git(root, *args):
    return subprocess.check_output(["git", "-C", str(root), *args], text=True).strip()


@pytest.fixture
def history(tmp_path):
    root = tmp_path / "repository"
    root.mkdir()
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
            input=data, text=True,
        ).strip()

    first = commit("reachable fixture\n")
    tip = commit("tip fixture\n", first)
    orphan = commit("orphan fixture\n")
    git(root, "update-ref", "refs/heads/main", tip)
    git(root, "update-ref", "refs/remotes/origin/main", tip)
    for label, revision in (("reachable", first), ("orphan", orphan), ("unknown", "0" * len(tip))):
        path = root / "atlases" / label / "fixture" / "manifest.yaml"
        path.parent.mkdir(parents=True)
        path.write_text(yaml.safe_dump({"provenance": {"code_version": revision}}))
    return root, first, tip, orphan


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
    manifest.write_text(yaml.safe_dump({"provenance": {"code_version": git(clone, "rev-parse", "HEAD")}}))
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
    manifest.write_text("provenance: {code_version: abcdef}\n")
    result = inventory.inventory_manifests(archive)
    assert result["history_state"] == "SOURCE_ARCHIVE"
    assert result["inventory_revision"] is None
    assert result["counts"]["UNAVAILABLE"] == 1


def test_cli_emits_json(history):
    root, _, tip, _ = history
    output = subprocess.check_output(
        [__import__("sys").executable, str(SCRIPT), "--repo-root", str(root)], text=True,
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
    path.write_text(text)
    row = next(row for row in inventory.inventory_manifests(root)["manifests"]
               if "/reachable/" in row["manifest"])
    assert row["status"] == "UNAVAILABLE"
    assert row["reason"] == reason
