# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Retention boundaries use disposable histories and real inventory evidence."""

from __future__ import annotations

import copy
import json
import subprocess
from pathlib import Path

import pytest

from digitalmodel.parametric import historical_provenance as history

ROOT = Path(__file__).resolve().parents[2]
INDEX = "atlases/historical-provenance.json"


@pytest.fixture
def repository(tmp_path, monkeypatch, request):
    # Never let a caller's Git bindings redirect fixture mutations.
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        monkeypatch.delenv(name, raising=False)
    monkeypatch.delenv("GITHUB_REF", raising=False)
    monkeypatch.setenv("GIT_CONFIG_COUNT", "1")
    monkeypatch.setenv("GIT_CONFIG_KEY_0", "core.autocrlf")
    monkeypatch.setenv("GIT_CONFIG_VALUE_0", "false")
    root = tmp_path / "repository"
    root.mkdir()

    def git(*args, data=None):
        return subprocess.check_output(
            ["git", "-C", str(root), *args], input=data, stderr=subprocess.PIPE,
        ).decode().strip()

    git("init", "-b", "main")
    # Disposable fixtures shall also run where no contributor identity exists.
    # These local settings affect only the newly initialized test repository.
    git("config", "user.name", "Provenance fixture")
    git("config", "user.email", "fixture")
    row = copy.deepcopy(next(
        row for row in json.loads((ROOT / INDEX).read_text())["atlases"]
        if row["status"] == "VERIFIED"
        and row["workflow_id"] == getattr(request, "param", "mooring-fatigue")
    ))
    reader = history.git_reader(ROOT, row["source_revision"])
    for name in row["sources"]:
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(reader(name))
    revision = subprocess.check_output(["git", "-C", str(ROOT), "rev-parse", "HEAD"]).decode().strip()
    artifacts = history.git_reader(ROOT, revision)
    for name in row["artifacts"]:
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(artifacts(name))

    def commit(message, *parents):
        # Build both merge styles explicitly, without changing the real checkout.
        git("add", ".")
        tree = git("write-tree")
        arguments = ["commit-tree", tree]
        for parent in parents:
            arguments.extend(["-p", parent])
        return git(*arguments, data=(message + "\n").encode())

    source = commit("Default source")
    row["source_revision"] = source
    wrapper = root / history.WRAPPER
    original = wrapper.read_bytes()
    wrapper.write_bytes(original + b"\n# Feature wrapper snapshot.\n")
    feature_source = commit("Feature source", source)
    feature_row = copy.deepcopy(row)
    feature_row["source_revision"] = feature_source
    feature_row.update(history.source_record(row["workflow_id"], history.archive_reader(root)))
    wrapper.write_bytes(original + b"\n# Unrelated wrapper snapshot.\n")
    unrelated = commit("Unrelated source", source)
    git("update-ref", "refs/heads/unrelated", unrelated)
    wrapper.write_bytes(original)

    def prepare(kind="modified"):
        before = [] if kind == "added" else [feature_row if kind == "unchanged" else row]
        index = root / INDEX
        index.write_text(json.dumps({"atlases": before}), encoding="utf-8")
        base = commit("Default inventory", source)
        git("update-ref", "refs/heads/main", base)
        git("update-ref", "refs/remotes/origin/main", base)
        wrapper.write_bytes(original + b"\n# Feature wrapper snapshot.\n")
        index.write_text(json.dumps({"atlases": [feature_row]}), encoding="utf-8")
        head = commit("Feature inventory", base, feature_source)
        git("update-ref", "refs/heads/feature", head)
        git("symbolic-ref", "HEAD", "refs/heads/feature")
        return dict(root=root, git=git, commit=commit, base=base, head=head,
                    row=copy.deepcopy(feature_row), unrelated=unrelated,
                    source=source, before=before)

    return prepare


@pytest.mark.parametrize("kind", ["added", "modified"])
@pytest.mark.parametrize("repository", ["mooring-fatigue", "riser-fatigue"], indirect=True)
def test_open_pull_request_retains_only_its_changed_entry(repository, kind):
    case = repository(kind)
    result = history.verify_git(case["row"], case["root"])
    assert result["status"] == "VERIFIED"
    assert result["working_tree_drift"] == []


def test_unchanged_entry_keeps_default_branch_retention(repository):
    case = repository("unchanged")
    with pytest.raises(history.EvidenceError, match="unchanged.*default branch"):
        history.verify_git(case["row"], case["root"])


@pytest.mark.parametrize("revision", ["unrelated", "unreachable", "default"])
def test_changed_entry_cannot_claim_revision_outside_its_head(repository, revision):
    case = repository()
    row = case["row"]
    if revision == "unreachable":
        row["source_revision"] = case["commit"]("Unreferenced source")
    elif revision == "default":
        # Even a retained default-branch revision must belong to the PR head
        # when the entry requests the PR-specific retention exception.
        row["source_revision"] = case["unrelated"]
        case["git"]("update-ref", "refs/remotes/origin/main", case["unrelated"])
    else:
        row["source_revision"] = case["unrelated"]
    (case["root"] / INDEX).write_text(json.dumps({"atlases": [row]}))
    head = case["commit"]("Invalid provenance claim", case["head"])
    case["git"]("update-ref", "refs/heads/feature", head)
    with pytest.raises(history.EvidenceError, match="pull request head"):
        history.verify_git(row, case["root"])


def test_uncommitted_entry_cannot_obtain_pull_request_retention(repository):
    case = repository()
    row = copy.deepcopy(case["row"])
    row["reason"] = "Uncommitted metadata"
    with pytest.raises(history.EvidenceError, match="committed inventory"):
        history.verify_git(row, case["root"])


@pytest.mark.parametrize("style", ["merge", "squash"])
@pytest.mark.parametrize("repository", ["mooring-fatigue", "riser-fatigue"], indirect=True)
def test_default_branch_after_merge_requires_retained_sources(repository, style):
    case = repository()
    parents = (case["base"], case["head"]) if style == "merge" else (case["base"],)
    merged = case["commit"]("Merged inventory", *parents)
    case["git"]("update-ref", "refs/heads/main", merged)
    case["git"]("update-ref", "refs/remotes/origin/main", merged)
    case["git"]("symbolic-ref", "HEAD", "refs/heads/main")
    # Keeping the old feature and PR refs must never revive a squash orphan.
    case["git"]("update-ref", "refs/pull/17/head", case["head"])
    if style == "merge":
        assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"
    else:
        with pytest.raises(history.EvidenceError, match="squash.*merge commit"):
            history.verify_git(case["row"], case["root"], pr_head="refs/pull/17/head")


def test_ci_merge_checkout_uses_pull_request_head(repository, monkeypatch):
    case = repository()
    case["git"]("update-ref", "refs/pull/17/head", case["head"])
    synthetic = case["commit"]("Synthetic check merge", case["base"], case["head"])
    case["git"]("update-ref", "refs/heads/check", synthetic)
    case["git"]("symbolic-ref", "HEAD", "refs/heads/check")
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"


def test_ci_requires_the_actual_pull_request_head_ref(repository, monkeypatch):
    case = repository()
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    with pytest.raises(history.EvidenceError, match="retained history"):
        history.verify_git(case["row"], case["root"])


def test_supplied_head_must_belong_to_current_checkout(repository):
    case = repository()
    with pytest.raises(history.EvidenceError, match="head.*checkout"):
        history.verify_git(case["row"], case["root"], pr_head="unrelated")


@pytest.mark.parametrize("damage", ["sources", "artifacts", "basis", "algorithm"])
def test_pull_request_retention_preserves_evidence_checks(repository, damage):
    case = repository()
    row = case["row"]
    if damage in {"sources", "artifacts"}:
        name = next(iter(row[damage]))
        row[damage][name] = "0" * 64
    elif damage == "basis":
        row[damage]["standards"] = []
    else:
        row[damage] = "unsupported"
    (case["root"] / INDEX).write_text(json.dumps({"atlases": [row]}))
    head = case["commit"]("Damaged evidence", case["head"])
    case["git"]("update-ref", "refs/heads/feature", head)
    with pytest.raises(history.EvidenceError):
        history.verify_git(row, case["root"])


def test_default_branch_stays_strict_with_stale_remote_ref(repository):
    case = repository()
    merged = case["commit"]("Squashed inventory", case["base"])
    case["git"]("update-ref", "refs/heads/main", merged)
    case["git"]("symbolic-ref", "HEAD", "refs/heads/main")
    with pytest.raises(history.EvidenceError, match="squash.*merge commit"):
        history.verify_git(case["row"], case["root"])


def test_ci_cannot_override_its_head_with_the_synthetic_merge(repository, monkeypatch):
    case = repository()
    case["git"]("update-ref", "refs/pull/17/head", case["head"])
    synthetic = case["commit"]("Synthetic check merge", case["base"], case["head"])
    case["git"]("update-ref", "refs/heads/check", synthetic)
    case["git"]("symbolic-ref", "HEAD", "refs/heads/check")
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    with pytest.raises(history.EvidenceError, match="CI pull request head"):
        history.verify_git(case["row"], case["root"], pr_head="HEAD")


def test_ci_rejects_source_only_in_synthetic_merge(repository, monkeypatch):
    case = repository()
    case["git"]("update-ref", "refs/pull/17/head", case["head"])
    row = case["row"]
    row["source_revision"] = case["unrelated"]
    (case["root"] / INDEX).write_text(json.dumps({"atlases": [row]}))
    synthetic = case["commit"]("Synthetic-only entry", case["head"], case["unrelated"])
    case["git"]("update-ref", "refs/heads/check", synthetic)
    case["git"]("symbolic-ref", "HEAD", "refs/heads/check")
    monkeypatch.setenv("GITHUB_REF", "refs/pull/17/merge")
    with pytest.raises(history.EvidenceError, match="committed inventory"):
        history.verify_git(row, case["root"])


def test_verifier_ignores_inherited_repository_bindings(repository, monkeypatch):
    case = repository()
    caller_head = history._git(ROOT, "rev-parse", "HEAD")
    caller_index = (ROOT / INDEX).read_bytes()
    git_dir = history._git(ROOT, "rev-parse", "--absolute-git-dir").decode().strip()
    common_dir = history._git(ROOT, "rev-parse", "--git-common-dir").decode().strip()
    for name, value in (("GIT_DIR", git_dir), ("GIT_WORK_TREE", str(ROOT)),
                        ("GIT_COMMON_DIR", str(ROOT / common_dir)),
                        ("GIT_INDEX_FILE", str(ROOT / ".git"))):
        monkeypatch.setenv(name, value)
    assert history.verify_git(case["row"], case["root"])["status"] == "VERIFIED"
    assert history._git(ROOT, "rev-parse", "HEAD") == caller_head
    assert (ROOT / INDEX).read_bytes() == caller_index


def test_cli_accepts_explicit_pull_request_head(repository, monkeypatch, capsys):
    case = repository()
    verify = history.verify_git

    def in_fixture(record, **options):
        return verify(record, case["root"], **options)

    monkeypatch.setattr(history, "verify_git", in_fixture)
    assert history.main(["--index", str(case["root"] / INDEX), "--pr-head", "feature"]) == 0
    assert json.loads(capsys.readouterr().out)["status"] == "VERIFIED"


def test_detached_default_branch_ci_stays_strict_with_stale_ref(repository, monkeypatch):
    case = repository()
    merged = case["commit"]("Squashed inventory", case["base"])
    # A detached default-branch checkout with an older retained remote ref.
    case["git"]("update-ref", "--no-deref", "HEAD", merged)
    monkeypatch.setenv("GITHUB_REF", "refs/heads/main")
    with pytest.raises(history.EvidenceError, match="squash.*merge commit"):
        history.verify_git(case["row"], case["root"])
