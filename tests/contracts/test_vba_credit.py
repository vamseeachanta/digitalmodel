# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Repository-wide regression for role-based spreadsheet credits."""

import hashlib
import os
from pathlib import Path
import subprocess

import pytest
import yaml


@pytest.fixture
def tracked_repo(tmp_path):
    """Create a disposable index without commits or inherited Git bindings."""
    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        env.pop(key, None)
    subprocess.run(["git", "init", "-q", str(tmp_path)], env=env, check=True)

    def track(path, text):
        target = tmp_path / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text, encoding="utf-8")
        subprocess.run(["git", "add", "--", path], cwd=tmp_path, env=env, check=True)
        return target

    return tmp_path, track


def test_missing_tracked_text_is_reported(tracked_repo):
    """An incomplete checkout cannot establish repository-wide coverage."""
    root, track = tracked_repo
    track("docs/missing.md", "role credit").unlink()
    digest = hashlib.sha256(b"example").hexdigest()
    assert _credit_violations(root, digest) == ["Missing tracked text: docs/missing.md"]


def test_credit_scan_collects_offending_paths(tracked_repo):
    """All offending tracked paths are reported in one scan."""
    root, track = tracked_repo
    track("README.md", "EXAMPLE")
    track("docs/copy.html", "prefixexamplesuffix")
    track("docs/clean.md", "role credit")
    (root / "untracked.md").write_text("example", encoding="utf-8")
    digest = hashlib.sha256(b"example").hexdigest()
    assert _credit_violations(root, digest) == [
        "Personal VBA credit returned in README.md",
        "Personal VBA credit returned in docs/copy.html",
    ]


def test_credit_scan_clears_inherited_git_bindings(tracked_repo, monkeypatch):
    """Caller Git settings cannot redirect discovery to another repository."""
    root, track = tracked_repo
    track("README.md", "example")
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        monkeypatch.setenv(key, str(root / "unavailable"))
    digest = hashlib.sha256(b"example").hexdigest()
    assert _credit_violations(root, digest) == [
        "Personal VBA credit returned in README.md",
    ]


def test_credit_scan_preserves_size_and_format_exclusions(tracked_repo):
    """Large text and unselected model formats retain their existing exclusions."""
    root, track = tracked_repo
    track("large.txt", "example" + " " * (1024 * 1024))
    track("model.dat", "example")
    digest = hashlib.sha256(b"example").hexdigest()
    assert _credit_violations(root, digest) == []


def test_credit_guard_runs_in_pr_harness():
    """Documentation-only changes must still execute the repository scan."""
    root = Path(__file__).resolve().parents[2]
    workflow = yaml.safe_load(
        (root / ".github/workflows/quality-gates-by-domain.yml").read_text()
    )
    triggers = workflow.get("on", workflow.get(True))
    assert "pull_request" in triggers
    # Event types may be explicit; path and branch filters could bypass the scan.
    assert not {"paths", "paths-ignore", "branches", "branches-ignore"} & set(
        triggers["pull_request"] or {}
    )
    if "types" in (triggers["pull_request"] or {}):
        assert {"opened", "synchronize", "reopened"} <= set(
            triggers["pull_request"]["types"]
        )
    job = workflow["jobs"]["ci-harness-tests"]
    assert "if" not in job
    commands = "\n".join(step.get("run", "") for step in job["steps"])
    assert "tests/contracts/test_vba_credit.py" in commands


def _credit_violations(root, retired_digest):
    """Scan tracked text at test runtime, reporting incomplete checkout coverage."""
    import re

    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        env.pop(key, None)
    paths = subprocess.check_output(
        ["git", "ls-files", "-z"], cwd=root, env=env,
    ).decode("utf-8").split("\0")
    suffixes = {
        ".py", ".md", ".html", ".htm", ".yaml", ".yml", ".json", ".csv",
        ".txt", ".toml", ".rst", ".xml", ".js", ".css", ".sh", ".ini", ".cfg",
    }
    violations = []
    for relative_path in paths:
        path = root / relative_path
        if path.suffix.lower() not in suffixes or path.is_symlink():
            continue
        if not path.is_file():
            violations.append(f"Missing tracked text: {relative_path}")
            continue
        if path.stat().st_size > 1024 * 1024:
            continue
        text = path.read_text(encoding="utf-8", errors="replace").casefold()
        # Numeric plot arrays need no hashing; deduplicate repeated words per file.
        words = set(re.findall(r"[a-z]{7,}", text))
        if any(
            hashlib.sha256(word[index:index + 7].encode()).hexdigest()
            == retired_digest
            for word in words
            for index in range(len(word) - 7 + 1)
        ):
            violations.append(f"Personal VBA credit returned in {relative_path}")
    return violations


def test_vba_credit_without_personal_name_in_tracked_text():
    """Prevent copies of the retired credit throughout tracked public text."""
    root = Path(__file__).resolve().parents[2]
    # Pin the retired ASCII surname by digest without republishing it.
    retired_digest = "8bd08eb81fab474e28e164e23a554bfabacc98c4ccbc98d2651a1d73278e02cb"
    violations = _credit_violations(root, retired_digest)
    assert not violations, "\n".join(violations)


def test_credit_guard_import_does_not_require_git(monkeypatch):
    """Missing Git cannot break collection of unrelated tests."""
    import runpy

    def unavailable(*args, **kwargs):
        raise FileNotFoundError("git unavailable")

    monkeypatch.setattr(subprocess, "check_output", unavailable)
    namespace = runpy.run_path(__file__)
    assert callable(namespace["test_vba_credit_without_personal_name_in_tracked_text"])


def test_credit_scan_requires_git_at_runtime(tmp_path, monkeypatch):
    """Unavailable discovery must fail execution rather than imply coverage."""
    def unavailable(*args, **kwargs):
        raise FileNotFoundError("git unavailable")

    monkeypatch.setattr(subprocess, "check_output", unavailable)
    with pytest.raises(FileNotFoundError, match="git unavailable"):
        _credit_violations(tmp_path, hashlib.sha256(b"example").hexdigest())
