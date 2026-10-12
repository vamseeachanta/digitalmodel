"""Regression coverage for ignore anchoring and tracked include chains."""

from contextlib import redirect_stderr, redirect_stdout
from functools import cache
import importlib.util
import io
import os
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/check_include_targets.py"
BASELINE = ROOT / "scripts/ci/include_target_baseline.txt"


def child_env():
    """Fixture Git operations will resolve the supplied working directory."""
    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        env.pop(key, None)
    env["PYTHONPATH"] = str(ROOT / "src")
    return env


def git(repo, *args, check=True):
    return subprocess.run(
        ["git", *args], cwd=repo, env=child_env(), capture_output=True,
        text=True, check=check,
    )


@pytest.fixture
def repo(tmp_path):
    """A throwaway repository will require no commit or identity settings."""
    git(tmp_path, "init")
    (tmp_path / ".gitignore").write_bytes((ROOT / ".gitignore").read_bytes())
    return tmp_path


def write(repo, name, content):
    path = repo / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")
    return path


@cache
def load_checker():
    spec = importlib.util.spec_from_file_location("include_target_checker", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def run_check(repo, baseline=None, *args):
    command = [sys.executable, str(SCRIPT), "--root", str(repo)]
    if baseline is not None:
        command.extend(["--baseline", str(baseline)])
    command.extend(args)
    if repo == ROOT:
        return subprocess.run(
            command, cwd=repo, env=child_env(), capture_output=True, text=True,
        )
    stdout, stderr = io.StringIO(), io.StringIO()
    with redirect_stdout(stdout), redirect_stderr(stderr):
        code = load_checker().main(command[2:])
    return subprocess.CompletedProcess(command, code, stdout.getvalue(), stderr.getvalue())


@pytest.mark.parametrize(
    ("path", "ignored"),
    [
        pytest.param("docs/a/env/case.yml", False, id="docs-yaml-pin"),
        pytest.param("a/b/env/case.yml", False, id="nested-engineering"),
        pytest.param("env/pyvenv.cfg", True, id="root-venv-pin"),
        pytest.param("docs/a/env/pyvenv.cfg", True, id="docs-venv-pin"),
    ],
)
def test_env_ignore_patterns(repo, path, ignored):
    """Nested YAML will be visible; root and docs virtual environments stay ignored.

    The docs YAML and both virtual-environment cases are regression pins.
    """
    write(repo, path, "")
    result = git(repo, "check-ignore", "-q", path, check=False)
    assert result.returncode == (0 if ignored else 1), path


@pytest.mark.parametrize("key", ["includefile", "BaseFile"])
def test_tracked_target_passes(repo, key):
    """A reference to a tracked relative target will pass."""
    write(repo, "models/run.yml", f"{key}: ../inputs/case.yml\n")
    write(repo, "inputs/case.yml", "General: {}\n")
    git(repo, "add", "models/run.yml", "inputs/case.yml")
    result = run_check(repo)
    assert result.returncode == 0, result.stdout + result.stderr


@pytest.mark.parametrize("key", ["includefile", "BaseFile"])
def test_absent_target_fails(repo, key):
    """An absent target will fail and identify the referring file and target."""
    write(repo, "models/run.yml", f"{key}: missing.yml\n")
    git(repo, "add", "models/run.yml")
    result = run_check(repo)
    assert result.returncode == 1
    assert "models/run.yml" in result.stdout
    assert "missing.yml" in result.stdout


def test_ignored_target_reports_rule(repo):
    """An ignored on-disk target will fail with the ignore rule and its line."""
    write(repo, "models/run.yml", "includefile: ../env/case.yml\n")
    write(repo, "env/case.yml", "General: {}\n")
    git(repo, "add", "models/run.yml")
    result = run_check(repo)
    assert result.returncode == 1
    assert "models/run.yml" in result.stdout
    assert "env/case.yml" in result.stdout
    assert ".gitignore:141:" in result.stdout
    assert "/env/" in result.stdout


def test_reincluded_untracked_target_has_no_ignore_rule(repo):
    """A re-included but untracked target will fail without blaming a negation rule."""
    write(repo, "models/run.yml", "includefile: ../docs/a/env/case.yml\n")
    write(repo, "docs/a/env/case.yml", "General: {}\n")
    git(repo, "add", "models/run.yml")
    result = run_check(repo)
    assert result.returncode == 1
    assert "docs/a/env/case.yml" in result.stdout
    assert "ignore rule:" not in result.stdout


@pytest.mark.parametrize("layout", ["mixed-fragments", "legacy-comment"])
def test_reference_extraction_does_not_validate_unrelated_model_data(repo, layout):
    """Tracked scalar references will pass across model fragments and legacy comments."""
    if layout == "mixed-fragments":
        content = b"General: {}\n- includefile: case.yml\nBaseFile: case.yml\n"
    else:
        content = b"# angle: 90\xb0\nincludefile: case.yml\n"
    model = write(repo, "models/run.yml", "")
    model.write_bytes(content)
    write(repo, "models/case.yml", "General: {}\n")
    git(repo, "add", "models/run.yml", "models/case.yml")
    result = run_check(repo)
    assert result.returncode == 0, result.stdout + result.stderr


def test_duplicate_keys_and_scalar_aliases_preserve_missing_references(repo):
    """Repeated keys and scalar aliases will retain every untracked reference."""
    content = (
        "reference: &case absent.yml\n"
        "BaseFile: *case\n"
        "includefile: missing.yml\n"
        "includefile: tracked.yml\n"
        "description: |\n  includefile: prose.yml\n"
    )
    write(repo, "models/run.yml", content)
    write(repo, "models/tracked.yml", "General: {}\n")
    git(repo, "add", "models/run.yml", "models/tracked.yml")
    result = run_check(repo)
    assert result.returncode == 1
    assert "absent.yml" in result.stdout
    assert "missing.yml" in result.stdout
    assert "prose.yml" not in result.stdout


def test_aliases_are_scoped_to_their_document(repo):
    """A later document's anchor will not hide an earlier missing reference."""
    write(repo, "models/run.yml", (
        "---\nreference: &case missing.yml\nBaseFile: *case\n"
        "---\nreference: &case tracked.yml\nBaseFile: *case\n"
    ))
    write(repo, "models/tracked.yml", "General: {}\n")
    git(repo, "add", "models/run.yml", "models/tracked.yml")
    result = run_check(repo)
    assert result.returncode == 1
    assert "missing.yml" in result.stdout


def test_baseline_suppresses_existing_reference(repo):
    """Only a broken reference recorded in the baseline will pass."""
    write(repo, "models/run.yml", "includefile: missing.yml\n")
    git(repo, "add", "models/run.yml")
    baseline = repo / "baseline.txt"
    generated = run_check(repo, baseline, "--write-baseline")
    assert generated.returncode == 0, generated.stdout + generated.stderr
    assert "run.yml" in baseline.read_text(encoding="utf-8")
    assert "missing.yml" in baseline.read_text(encoding="utf-8")
    result = run_check(repo, baseline)
    assert result.returncode == 0, result.stdout + result.stderr


@pytest.mark.parametrize("repair", ["track-target", "remove-reference"])
def test_stale_baseline_fails(repo, repair):
    """A baseline entry will become stale after tracking or removing its reference."""
    write(repo, "models/run.yml", "BaseFile: missing.yml\n")
    git(repo, "add", "models/run.yml")
    baseline = repo / "baseline.txt"
    result = run_check(repo, baseline, "--write-baseline")
    assert result.returncode == 0, result.stdout + result.stderr
    if repair == "track-target":
        write(repo, "models/missing.yml", "General: {}\n")
        git(repo, "add", "models/missing.yml")
    else:
        write(repo, "models/run.yml", "General: {}\n")
    result = run_check(repo, baseline)
    assert result.returncode == 1
    assert "stale baseline" in result.stdout.lower()
    assert "run.yml" in result.stdout
    assert "missing.yml" in result.stdout


def test_real_tree_matches_baseline():
    """The real tracked tree will contain exactly the committed baseline defects."""
    result = run_check(ROOT, BASELINE)
    assert result.returncode == 0, result.stdout + result.stderr
