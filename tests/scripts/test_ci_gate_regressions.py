# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Exercise filename handling and failure propagation through the PR gate."""

import importlib.util
import os
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "lint_changed", ROOT / "scripts/ci/lint_changed.py"
)
lint_changed = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(lint_changed)


@pytest.mark.parametrize("name", ["résumé.py", "with spaces.py", "with\ttab.py", "with\nline.py"])
def test_changed_python_files_preserves_git_filenames(tmp_path, monkeypatch, name):
    """Git must return actual names, including Unicode and line separators."""
    if os.name == "nt" and any(char in name for char in "\t\n"):
        pytest.skip("Windows filenames cannot contain control characters")
    for directory, source in [("before", "pass\n"), ("after", "answer = 42\n")]:
        (tmp_path / directory).mkdir()
        (tmp_path / directory / name).write_text(source, encoding="utf-8")

    def diff_without_commits(*args):
        # Exercise real Git's quoting and delimiters without creating commits.
        assert args[:2] == ("diff", "--name-only")
        options = [arg for arg in args[2:] if arg.startswith("-") and arg != "--"]
        result = subprocess.run(
            ["git", "diff", "--no-index", "--name-only", *options, "before", "after"],
            capture_output=True, text=True, encoding="utf-8", errors="surrogateescape",
        )
        assert result.returncode == 1, result.stderr
        return result.stdout

    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(lint_changed, "git", diff_without_commits)
    expected = f"after/{name}"
    assert lint_changed.changed_python_files("base") == [expected]
    assert Path(expected).read_text(encoding="utf-8") == "answer = 42\n"


def test_ci_matrix_memory_failure_fails_pytest(tmp_path):
    """Run the real matrix test and quarantine hook with excessive memory use."""
    plugin = tmp_path / "memory_probe.py"
    plugin.write_text(
        "from types import SimpleNamespace\n"
        "import pytest\n"
        "import psutil\n"
        "@pytest.fixture(autouse=True)\n"
        "def excessive_memory(monkeypatch):\n"
        "    readings = iter([0, 1024 ** 3])\n"
        "    process = SimpleNamespace(memory_info=lambda: "
        "SimpleNamespace(rss=next(readings)))\n"
        "    monkeypatch.setattr(psutil, 'Process', lambda: process)\n"
    )
    result = subprocess.run(
        [sys.executable, "-m", "pytest", "-p", "memory_probe",
         "tests/test_performance_baselines.py::TestPerformanceBaselines::"
         "test_matrix_operations_baseline", "-q", "-p", "no:cacheprovider",
         f"--basetemp={tmp_path / 'inner'}"],
        cwd=ROOT, capture_output=True, text=True,
        env={**os.environ, "CI": "true", "PYTHONPATH": os.pathsep.join(
            [str(tmp_path), os.environ.get("PYTHONPATH", "")]
        ), "OPENBLAS_NUM_THREADS": "1", "OMP_NUM_THREADS": "1"},
    )
    assert result.returncode == 1, result.stdout + result.stderr
    assert "max_memory_mb" in result.stdout
    assert "1 failed" in result.stdout
    assert "xfailed" not in result.stdout


@pytest.mark.parametrize("node_id", [
    "tests/test_performance_baselines.py::TestPerformanceBaselines::"
    "test_basic_math_operations_baseline",
    "tests/test_performance_baselines.py::TestPerformanceBaselines::"
    "test_matrix_operations_baseline",
    "tests/workflows/test_openfoam_batch_layout.py::test_lock_heartbeat_refreshes",
    "tests/workflows/test_openfoam_batch_layout.py::"
    "test_process_start_prevents_pid_reuse",
    "tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_plate_with_hole_kt",
    "tests/solvers/calculix/test_fem_chain.py::TestFEMChain::test_solve_returns_success",
])
def test_correctness_checks_are_not_quarantined(node_id):
    """Apply the production collection hook, including broad node matching."""
    spec = importlib.util.spec_from_file_location("gate_conftest", ROOT / "tests/conftest.py")
    conftest = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(conftest)
    markers = []

    class Item:
        nodeid = node_id

    item = Item()
    item.add_marker = markers.append
    conftest.pytest_collection_modifyitems(None, [item])
    assert not markers, node_id


def test_quarantine_entries_record_restore_conditions():
    entries = (ROOT / "tests/quarantine.txt").read_text(encoding="utf-8").splitlines()
    assert any("#3720" in line for line in entries)
    for line in entries:
        if line and not line.startswith("#"):
            assert "; restore:" in line, line.partition(" | ")[0]


@pytest.mark.parametrize("active", [0, 2])
@pytest.mark.parametrize("override", [False, True])
def test_full_run_dispatch_uses_checkout_metadata(tmp_path, active, override):
    stub = tmp_path / "gh"
    stub.write_text(
        "#!/usr/bin/env bash\n"
        'printf "%s|%s\\n" "$PWD" "$*" >> "$CALL_LOG"\n'
        'case "$1 $2" in\n'
        '  "repo view") echo example/repository ;;\n'
        '  "run list") echo "$ACTIVE_RUNS" ;;\n'
        '  "workflow run") exit 0 ;;\n'
        '  *) exit 99 ;;\n'
        "esac\n"
    )
    stub.chmod(0o755)
    log = tmp_path / "calls.txt"
    env = {**os.environ, "PATH": str(tmp_path) + os.pathsep + os.environ["PATH"],
           "CALL_LOG": str(log), "ACTIVE_RUNS": str(active)}
    env.pop("MAIN_FULL_RUN_REPO", None)
    if override:
        env["MAIN_FULL_RUN_REPO"] = "example/override"
    result = subprocess.run(
        ["bash", str(ROOT / "scripts/ci/dispatch_main_full_run.sh")],
        cwd=tmp_path, env=env, capture_output=True, text=True,
    )
    assert result.returncode == 0, result.stderr
    calls = log.read_text().splitlines()
    metadata = [line for line in calls if "|repo view " in line]
    assert bool(metadata) is not override
    if metadata:
        assert metadata == [f"{ROOT}|repo view --json nameWithOwner --jq .nameWithOwner"]
    repo = "example/override" if override else "example/repository"
    assert any(f"|run list -R {repo} " in line for line in calls)
    dispatches = [line for line in calls if "|workflow run " in line]
    assert len(dispatches) == (0 if active else 1)
    if dispatches:
        assert dispatches[0].endswith(f"workflow run full-matrix-sweep.yml -R {repo} --ref main")
