"""Content provenance examples using disposable Git object databases."""
from __future__ import annotations

import hashlib
import os
import subprocess

import pytest

from . import test_no_regression_traces as traces


# Synthetic source examples: distinct return strings make the producer changes
# independent of the provenance implementation and of the workbook data.
CAPTURED_SOURCE = b"def make_polar_overlay():\n    return 'captured'\n"
CHANGED_SOURCE = b"def make_polar_overlay():\n    return 'changed'\n"


def _git(repo, *args, input=None):
    return subprocess.check_output(
        ["git", *args], cwd=repo, env=traces._git_environment(),
        input=input, stderr=subprocess.PIPE,
    ).decode("utf-8").strip()


def _commit(repo, source, message, parent=None):
    """Write synthetic objects without using or changing any Git identity.

    Tree mode and commit fields follow git-mktree and git-commit-tree formats;
    the epoch timestamp is fixed so examples have reproducible object IDs.
    """
    blob = _git(repo, "hash-object", "-w", "--stdin", input=source)
    tree = _git(
        repo, "mktree", input=f"100644 blob {blob}\tproducer.py\n".encode(),
    )
    fields = [f"tree {tree}"]
    if parent is not None:
        fields.append(f"parent {parent}")
    fields.extend([
        "author Fixture <fixture> 0 +0000",
        "committer Fixture <fixture> 0 +0000",
        "", message, "",
    ])
    commit = _git(
        repo, "hash-object", "-t", "commit", "-w", "--stdin",
        input="\n".join(fields).encode(),
    )
    return commit, blob


@pytest.fixture
def squash_capture(tmp_path):
    """A feature capture and its squash share a tree, but no feature ancestry."""
    repo = tmp_path / "repository"
    repo.mkdir()
    _git(repo, "init", "--quiet")
    base, base_blob = _commit(repo, CHANGED_SOURCE, "Initial producer")
    capture, blob = _commit(repo, CAPTURED_SOURCE, "Capture producer", base)
    squash, _ = _commit(repo, CAPTURED_SOURCE, "Squashed producer", base)
    head, _ = _commit(repo, CHANGED_SOURCE, "Refactor producer", squash)
    _git(repo, "update-ref", "refs/heads/feature", capture)
    _git(repo, "update-ref", "refs/heads/squashed", head)
    _git(repo, "symbolic-ref", "HEAD", "refs/heads/squashed")
    fixture = {
        "source_commit_sha": capture,
        "build_script_path": "producer.py",
        "build_script_function": "make_polar_overlay",
        "build_script_blob_sha": blob,
    }
    return repo, fixture, squash, base_blob


@pytest.mark.parametrize("revision", ["feature", "squash"])
def test_identical_capture_survives_squash_history(squash_capture, revision):
    repo, fixture, squash, _ = squash_capture
    ancestor = subprocess.run(
        ["git", "merge-base", "--is-ancestor", fixture["source_commit_sha"], "HEAD"],
        cwd=repo, env=traces._git_environment(), capture_output=True,
    )
    assert ancestor.returncode == 1  # git-merge-base: false ancestry, not an error.
    assert _git(repo, "rev-parse", "HEAD:producer.py") != fixture["build_script_blob_sha"]
    if revision == "squash":
        fixture["source_commit_sha"] = squash
    traces._assert_capture_source_identity(repo, fixture)


def test_changed_generating_content_is_rejected(squash_capture):
    repo, fixture, _, _ = squash_capture
    fixture["source_commit_sha"] = _git(repo, "rev-parse", "HEAD")
    with pytest.raises(AssertionError, match="generating source blob differs"):
        traces._assert_capture_source_identity(repo, fixture)


def test_mismatched_blob_digest_is_rejected(squash_capture):
    repo, fixture, _, other_blob = squash_capture
    fixture["build_script_blob_sha"] = other_blob
    with pytest.raises(AssertionError, match="generating source blob differs"):
        traces._assert_capture_source_identity(repo, fixture)


@pytest.mark.parametrize("field,value", [
    ("source_commit_sha", "refs/heads/unavailable"),
    ("build_script_path", "unavailable.py"),
])
def test_missing_historical_source_is_rejected(squash_capture, field, value):
    repo, fixture, _, _ = squash_capture
    fixture[field] = value
    with pytest.raises(AssertionError, match="historical generating source is unavailable"):
        traces._assert_capture_source_identity(repo, fixture)


def test_missing_generating_function_is_rejected(squash_capture):
    repo, fixture, _, _ = squash_capture
    fixture["build_script_function"] = "unavailable_function"
    with pytest.raises(AssertionError, match="generating function is absent"):
        traces._assert_capture_source_identity(repo, fixture)


BINDING_CASES = [
    "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE",
    "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES", "all",
]
# Index and alternate bindings are checked directly below; the integration
# commands need not touch the index or distinguish identical alternate objects.
REPOSITORY_VARIABLES = [
    *BINDING_CASES[:-1], "GIT_NAMESPACE", "GIT_PREFIX", "GIT_IMPLICIT_WORK_TREE",
    "GIT_GRAFT_FILE", "GIT_SHALLOW_FILE", "GIT_REPLACE_REF_BASE", "GIT_CONFIG",
    "GIT_CONFIG_PARAMETERS", "GIT_CONFIG_COUNT", "GIT_NO_REPLACE_OBJECTS",
    "GIT_INTERNAL_SUPER_PREFIX", "GIT_CONFIG_KEY_0", "GIT_CONFIG_VALUE_0",
]


@pytest.mark.parametrize("name", REPOSITORY_VARIABLES)
def test_git_environment_removes_binding_without_changing_caller(monkeypatch, name):
    monkeypatch.setenv(name, "inherited-repository-binding")
    monkeypatch.setenv("CAPTURE_TEST_SETTING", "preserved")
    env = traces._git_environment()
    assert name not in env
    assert env["CAPTURE_TEST_SETTING"] == "preserved"
    assert os.environ[name] == "inherited-repository-binding"


def test_git_environment_clears_git_declared_repository_variables(tmp_path, monkeypatch):
    names = _git(tmp_path, "rev-parse", "--local-env-vars").splitlines()
    assert {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"} <= set(names)
    for name in names:
        monkeypatch.setenv(name, "inherited-repository-binding")
    assert set(names).isdisjoint(traces._git_environment())


def _snapshot(repo):
    """Include caller files, refs, index, config and the complete object store."""
    return {
        path.relative_to(repo).as_posix(): hashlib.sha256(path.read_bytes()).hexdigest()
        for path in repo.rglob("*") if path.is_file()
    }


def _bind_caller(tmp_path, monkeypatch, binding):
    # Clear bindings before setup so even a negative probe stays disposable.
    for name in list(os.environ):
        if name in REPOSITORY_VARIABLES or name.startswith(("GIT_CONFIG_KEY_", "GIT_CONFIG_VALUE_")):
            monkeypatch.delenv(name, raising=False)
    caller = tmp_path / "caller"
    caller.mkdir()
    _git(caller, "init", "--quiet")
    _git(caller, "symbolic-ref", "HEAD", "refs/heads/caller")
    commit, _ = _commit(caller, CHANGED_SOURCE, "Caller producer")
    _git(caller, "update-ref", "refs/heads/caller", commit)
    _git(caller, "read-tree", "HEAD")
    (caller / "keep.txt").write_text(
        "Caller content\n", encoding="utf-8", newline="\n",
    )
    before = _snapshot(caller)
    bindings = {
        "GIT_DIR": caller / ".git",
        "GIT_WORK_TREE": caller,
        "GIT_COMMON_DIR": caller / ".git",
        "GIT_INDEX_FILE": caller / ".git" / "index",
        "GIT_OBJECT_DIRECTORY": caller / ".git" / "objects",
        "GIT_ALTERNATE_OBJECT_DIRECTORIES": caller / ".git" / "objects",
    }
    for name, value in bindings.items():
        if binding in (name, "all"):
            monkeypatch.setenv(name, str(value))
    return caller, before


@pytest.mark.parametrize("binding", BINDING_CASES)
def test_capture_fixture_preserves_bound_caller(tmp_path, monkeypatch, request, binding):
    caller, before = _bind_caller(tmp_path, monkeypatch, binding)
    try:
        repo, fixture, _, _ = request.getfixturevalue("squash_capture")
        assert (repo / ".git").is_dir(), "fixture Git database must be local"
        traces._assert_capture_source_identity(repo, fixture)
    finally:
        assert _snapshot(caller) == before, "fixture changed caller files or Git metadata"


@pytest.mark.parametrize("binding", BINDING_CASES)
def test_capture_validator_ignores_bound_caller(squash_capture, tmp_path, monkeypatch, binding):
    repo, fixture, _, _ = squash_capture
    caller, before = _bind_caller(tmp_path, monkeypatch, binding)
    try:
        traces._assert_capture_source_identity(repo, fixture)
    finally:
        assert _snapshot(caller) == before, "validator changed caller files or Git metadata"


@pytest.mark.parametrize("binding", BINDING_CASES)
def test_repository_discovery_ignores_bound_caller(tmp_path, monkeypatch, binding):
    expected_root = traces._repo_root()
    caller, before = _bind_caller(tmp_path, monkeypatch, binding)
    try:
        assert traces._repo_root() == expected_root
    finally:
        assert _snapshot(caller) == before, "discovery changed caller files or Git metadata"


@pytest.mark.parametrize("binding", BINDING_CASES)
def test_ancestry_probe_ignores_bound_caller(squash_capture, tmp_path, monkeypatch, binding):
    caller, before = _bind_caller(tmp_path, monkeypatch, binding)
    try:
        test_identical_capture_survives_squash_history(squash_capture, "feature")
    finally:
        assert _snapshot(caller) == before, "ancestry probe changed caller files or Git metadata"
