# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

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
    env["GIT_CONFIG_GLOBAL"] = os.devnull
    env["GIT_CONFIG_NOSYSTEM"] = "1"
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
    path.write_text(content, encoding="utf-8", newline="\n")
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


@pytest.mark.parametrize("ignorecase", [False, True], ids=["case-sensitive", "case-insensitive"])
@pytest.mark.parametrize(
    ("path", "ignored"),
    [
        pytest.param("docs/a/env/case.yml", False, id="docs-yaml-pin"),
        pytest.param("a/b/env/case.yml", False, id="nested-engineering"),
        pytest.param("a/b/ENV/case.yml", False, id="uppercase-nested-engineering"),
        pytest.param("env/pyvenv.cfg", True, id="root-venv-pin"),
        pytest.param("ENV/pyvenv.cfg", True, id="uppercase-root-venv-pin"),
        pytest.param("docs/a/env/pyvenv.cfg", True, id="docs-venv-pin"),
    ],
)
def test_env_ignore_patterns(repo, path, ignored, ignorecase):
    """Nested YAML will be visible; root and docs virtual environments stay ignored.

    The docs YAML and both virtual-environment cases are regression pins.
    """
    write(repo, path, "")
    result = git(repo, "-c", f"core.ignorecase={str(ignorecase).lower()}",
                 "check-ignore", "-q", path, check=False)
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
    ignore = repo / ".gitignore"
    ignore.write_text("# Fixture line offset.\n" + ignore.read_text(encoding="utf-8"),
                      encoding="utf-8", newline="\n")
    write(repo, "models/run.yml", "includefile: ../env/case.yml\n")
    write(repo, "env/case.yml", "General: {}\n")
    git(repo, "add", "models/run.yml")
    result = run_check(repo)
    assert result.returncode == 1
    assert "models/run.yml" in result.stdout
    assert "env/case.yml" in result.stdout
    rules = ignore.read_text(encoding="utf-8").splitlines()
    assert rules.count("/env/") == 1
    assert "env/" not in rules
    line_number = rules.index("/env/") + 1
    assert f".gitignore:{line_number}:/env/" in result.stdout


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


@pytest.mark.parametrize("ignorecase", [False, True], ids=["case-sensitive", "case-insensitive"])
@pytest.mark.parametrize(
    ("path", "ignored"),
    [
        ("a/b/env/pyvenv.cfg", True),
        ("a/b/env/bin/python", True),
        ("a/b/env/bin/activate", True),
        ("a/b/env/bin/pip", True),
        ("a/b/env/Scripts/python.exe", True),
        ("a/b/env/Scripts/activate.bat", True),
        ("a/b/env/Scripts/pip.exe", True),
        ("a/b/env/lib/python3.11/site-packages/module.py", True),
        ("a/b/env/lib64/python3.11/site-packages/module.py", True),
        ("a/b/env/Lib/site-packages/module.py", True),
        ("a/b/env/include/python3.11/header.h", True),
        ("a/b/env/case.dat", False),
        ("a/b/env/case.yml", False),
        ("a/b/env/inputs/case.dat", False),
        ("a/b/env/inputs/case.yml", False),
        ("a/b/env/include/case.dat", False),
        ("a/b/env/library/case.yml", False),
        ("a/b/env/bin/case.dat", False),
        ("a/b/env/Scripts/case.yml", False),
    ],
)
def test_nested_env_structure(repo, path, ignored, ignorecase):
    """Environment paths will be ignored while engineering inputs stay visible."""
    write(repo, "a/b/env/pyvenv.cfg", "")
    write(repo, path, "")
    result = git(repo, "-c", f"core.ignorecase={str(ignorecase).lower()}",
                 "check-ignore", "-q", path, check=False)
    assert result.returncode == (0 if ignored else 1), path


def test_global_git_configuration_is_isolated(repo, monkeypatch):
    """A global ignore file will not alter fixture ignore decisions."""
    write(repo, "global-ignore", "*.yml\n")
    config = write(repo, "global-config", "[core]\n    excludesFile = global-ignore\n")
    monkeypatch.setenv("GIT_CONFIG_GLOBAL", str(config))
    write(repo, "models/case.yml", "")
    result = git(repo, "check-ignore", "-q", "models/case.yml", check=False)
    assert result.returncode == 1
    assert child_env()["GIT_CONFIG_NOSYSTEM"] == "1"


def test_baseline_can_only_shrink():
    """Baseline growth will require a visible edit to the regression ceiling."""
    entries = load_checker().read_baseline(BASELINE)
    # This ceiling may be lowered, never raised without a visible test edit.
    assert len(entries) <= 198


@pytest.mark.parametrize("key", ["includefile", "BaseFile"])
def test_tracked_target_missing_from_worktree_fails(repo, key):
    """A tracked name alone cannot supply the bytes needed by an include chain.

    Independent fixture: the index contains models/case.dat, but its working
    file is renamed away. Thus run.yml has one unresolved target, case.dat.
    Git ls-files documents that cached paths remain listed after local removal:
    https://git-scm.com/docs/git-ls-files (cached and deleted options).
    """
    write(repo, "models/run.yml", f"{key}: case.dat\n")
    target = write(repo, "models/case.dat", "General: {}\n")
    git(repo, "add", "models/run.yml", "models/case.dat")
    target.rename(repo / "retained-case.dat")
    result = run_check(repo)
    assert result.returncode == 1
    assert "models/run.yml" in result.stdout
    assert "case.dat" in result.stdout
    assert "missing from working tree" in result.stdout


def test_missing_tracked_target_can_be_baselined_and_repaired(repo):
    """One missing target is baselined; restoring its bytes makes that row stale.

    Expected sets are calculated directly: {('models/run.yml', 'case.dat')}
    before restoration, and the empty set after restoration. The baseline
    must equal the broken-reference set, rather than hide a repaired defect.
    """
    write(repo, "models/run.yml", "BaseFile: case.dat\n")
    target = write(repo, "models/case.dat", "General: {}\n")
    git(repo, "add", "models/run.yml", "models/case.dat")
    retained = repo / "retained-case.dat"
    target.rename(retained)
    baseline = repo / "baseline.txt"
    generated = run_check(repo, baseline, "--write-baseline")
    assert generated.returncode == 0, generated.stdout + generated.stderr
    assert load_checker().read_baseline(baseline) == {("models/run.yml", "case.dat")}
    assert run_check(repo, baseline).returncode == 0
    retained.rename(target)
    git(repo, "add", "models/case.dat")
    result = run_check(repo, baseline)
    assert result.returncode == 1
    assert "stale baseline entry" in result.stdout
