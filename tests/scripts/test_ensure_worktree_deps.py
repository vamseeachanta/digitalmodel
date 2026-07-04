"""Tests for scripts/dev/ensure-worktree-deps.sh (digitalmodel, wh#3368 #4).

Fully hermetic: each test builds a throwaway git repo + a sibling dir in
tmp_path and runs the script — no dependence on the real digitalmodel checkout.
"""

from __future__ import annotations

import os
import subprocess
from pathlib import Path

import pytest

# Resolve the script whether run from the repo (scripts/dev/) or the scratchpad.
_here = Path(__file__).resolve()
_candidates = [
    _here.parent / "ensure-worktree-deps.sh",
    _here.parents[2] / "scripts" / "dev" / "ensure-worktree-deps.sh",
]
SCRIPT = next((p for p in _candidates if p.exists()), _candidates[0])


def _git(cwd: Path, *args: str) -> None:
    subprocess.run(["git", "-C", str(cwd), *args], check=True,
                   capture_output=True, text=True)


def _make_repo(tmp_path: Path) -> Path:
    """A main checkout named 'digitalmodel' with a 'assetutilities' sibling."""
    root = tmp_path / "digitalmodel"
    root.mkdir()
    _git(root, "init", "-q", "-b", "main")
    _git(root, "config", "user.email", "t@e.com")
    _git(root, "config", "user.name", "T")
    _git(root, "config", "commit.gpgsign", "false")
    (root / "README.md").write_text("# dm\n")
    _git(root, "add", "-A")
    _git(root, "commit", "-q", "-m", "init")
    return root


def _run(cwd: Path):
    return subprocess.run(["bash", str(SCRIPT)], cwd=str(cwd),
                          capture_output=True, text=True)


def test_creates_symlink_to_real_sibling(tmp_path: Path):
    root = _make_repo(tmp_path)
    sibling = tmp_path / "assetutilities"
    sibling.mkdir()
    r = _run(root)
    assert r.returncode == 0, r.stderr
    link = root / ".claude" / "worktrees" / "assetutilities"
    assert link.is_symlink()
    assert link.resolve() == sibling.resolve()


def test_idempotent_second_run_reports_already_linked(tmp_path: Path):
    root = _make_repo(tmp_path)
    (tmp_path / "assetutilities").mkdir()
    _run(root)
    r = _run(root)
    assert r.returncode == 0
    assert "already linked" in r.stdout
    link = root / ".claude" / "worktrees" / "assetutilities"
    assert link.is_symlink()


def test_missing_sibling_warns_but_does_not_fail_or_link(tmp_path: Path):
    root = _make_repo(tmp_path)  # no sibling created
    r = _run(root)
    assert r.returncode == 0
    assert "not found" in r.stderr
    assert not (root / ".claude" / "worktrees" / "assetutilities").exists()


def test_resolves_from_inside_a_worktree(tmp_path: Path):
    """Run from a nested worktree — the link must land at the MAIN root."""
    root = _make_repo(tmp_path)
    (tmp_path / "assetutilities").mkdir()
    wt = root / ".claude" / "worktrees" / "agent-xyz"
    wt.parent.mkdir(parents=True, exist_ok=True)
    _git(root, "worktree", "add", "-q", "--detach", str(wt))
    r = _run(wt)
    assert r.returncode == 0, r.stderr
    # Link created under the MAIN root, not under the worktree.
    link = root / ".claude" / "worktrees" / "assetutilities"
    assert link.is_symlink()
    assert link.resolve() == (tmp_path / "assetutilities").resolve()


def test_replaces_a_stale_wrong_symlink(tmp_path: Path):
    root = _make_repo(tmp_path)
    sibling = tmp_path / "assetutilities"
    sibling.mkdir()
    link = root / ".claude" / "worktrees" / "assetutilities"
    link.parent.mkdir(parents=True, exist_ok=True)
    link.symlink_to(tmp_path / "nowhere")  # stale/broken link
    r = _run(root)
    assert r.returncode == 0
    assert link.resolve() == sibling.resolve()
