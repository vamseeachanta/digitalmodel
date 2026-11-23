"""Disposable-repository regression for fixture generator isolation and source pins."""
import hashlib
import importlib.util
import json
import os
import platform
from pathlib import Path
import subprocess
import sys

import numpy as np
import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/validation/generate_mesh_legacy_pickles.py"
SOURCE = "src/digitalmodel/naval_architecture/mesh_hydrostatics.py"
HULL = "src/digitalmodel/naval_architecture/hull_fixtures.py"
OUTPUT = "tests/fixtures/test_vectors/naval_architecture/mesh_legacy_pickles.json"
BINDINGS = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE")


def git(root, *args):
    env = {k: v for k, v in os.environ.items() if k not in BINDINGS}
    return subprocess.check_output(["git", *args], cwd=root, env=env, text=True).strip()


def snapshot(root):
    return {str(p.relative_to(root)): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in root.rglob("*") if p.is_file()}


def prepare_probe(tmp_path):
    spec = importlib.util.spec_from_file_location("fixture_generator", SCRIPT)
    generator = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(generator)
    target, decoy = tmp_path / "target", tmp_path / "decoy"
    for repo in (target, decoy):
        repo.mkdir()
        git(repo, "init", "-q")
        git(repo, "config", "user.name", "Fixture test")
        git(repo, "config", "user.email", "fixture@example.invalid")
        (repo / "sentinel.txt").write_text("repository-specific data: " + repo.name)
    for path in (SOURCE, HULL):
        destination = target / path
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text(git(ROOT, "show", f"{generator.BASELINE}:{path}") + "\n")
    for package in ("digitalmodel", "digitalmodel/naval_architecture"):
        (target / "src" / package / "__init__.py").write_text("")
    for repo in (target, decoy):
        git(repo, "add", ".")
        git(repo, "commit", "-qm", "test: disposable analytic fixture source\n\n"
            "Co-Authored-By: Codex <noreply@openai.com>")
    script = target / "scripts/validation/generate_mesh_legacy_pickles.py"
    script.parent.mkdir(parents=True)
    script.write_text(SCRIPT.read_text().replace(generator.BASELINE, git(target, "rev-parse", "HEAD")))
    output = target / OUTPUT
    output.parent.mkdir(parents=True)
    output.write_text("deliberately ungenerated fixture\n")
    (target / HULL).write_text('raise RuntimeError("tampered working tree hull")\n')
    env = dict(os.environ, PYTHONPATH=str(target / "src"),
               GIT_DIR=str(decoy / ".git"), GIT_WORK_TREE=str(decoy),
               GIT_COMMON_DIR=str(decoy / ".git"), GIT_INDEX_FILE=str(decoy / ".git/index"))
    return target, decoy, script, env


@pytest.mark.skipif(np.__version__ != "2.4.4" or platform.python_version() != "3.11.14",
                    reason="fixture reproduction pins NumPy 2.4.4 and Python 3.11.14")
def test_generator_ignores_hostile_git_bindings_and_worktree_hull(tmp_path):
    target, decoy, script, env = prepare_probe(tmp_path)
    before_target, before_decoy = snapshot(target), snapshot(decoy)
    result = subprocess.run([sys.executable, str(script)], cwd=decoy, env=env,
                            capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    after_target = snapshot(target)
    assert snapshot(decoy) == before_decoy
    changed = [p for p in before_target if before_target[p] != after_target[p]]
    assert changed == [OUTPUT]
    assert set(before_target) == set(after_target)
    assert result.stdout.strip() == after_target[OUTPUT]
    record = json.loads((target / OUTPUT).read_text())
    assert set(record["pickles"]) == {"mesh", "clipped", "result"}
