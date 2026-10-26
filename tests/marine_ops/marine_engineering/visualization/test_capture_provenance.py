"""Content provenance examples using disposable Git object databases."""
from __future__ import annotations

import subprocess

import pytest

from . import test_no_regression_traces as traces


# Synthetic source examples: distinct return strings make the producer changes
# independent of the provenance implementation and of the workbook data.
CAPTURED_SOURCE = b"def make_polar_overlay():\n    return 'captured'\n"
CHANGED_SOURCE = b"def make_polar_overlay():\n    return 'changed'\n"


def _git(repo, *args, input=None):
    return subprocess.check_output(
        ["git", *args], cwd=repo, input=input, stderr=subprocess.PIPE,
    ).decode().strip()


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
        cwd=repo, capture_output=True,
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
