"""No-regression + legal-sanity tests for the visualization package (digitalmodel#616).

TDD #13 (no-regression on trace count) + TDD #15 (legal-sanity scan).
Both fixtures live under fixtures/ in this directory.
"""
from __future__ import annotations

import ast
import json
import os
import pathlib
import subprocess
import sys

import pandas as pd
import pytest


FIXTURE_DIR = pathlib.Path(__file__).parent / "fixtures"
TRACE_SIG_FIXTURE = FIXTURE_DIR / "ocimf_explorer_pre_refactor_trace_signature.json"


# ---------- TDD #13: refactored make_polar_overlay preserves trace signature ----------


def _git_environment() -> dict[str, str]:
    """Keep Git children independent of the caller's repository bindings."""
    repository_variables = {
        "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE",
        "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES",
        "GIT_NAMESPACE", "GIT_PREFIX", "GIT_IMPLICIT_WORK_TREE",
        "GIT_GRAFT_FILE", "GIT_SHALLOW_FILE", "GIT_REPLACE_REF_BASE",
        "GIT_CONFIG", "GIT_CONFIG_PARAMETERS", "GIT_CONFIG_COUNT",
        "GIT_NO_REPLACE_OBJECTS", "GIT_INTERNAL_SUPER_PREFIX",
    }
    return {
        name: value for name, value in os.environ.items()
        if name not in repository_variables
        and not name.startswith(("GIT_CONFIG_KEY_", "GIT_CONFIG_VALUE_"))
    }


def _repo_root() -> pathlib.Path:
    return pathlib.Path(subprocess.check_output(
        ["git", "rev-parse", "--show-toplevel"],
        cwd=pathlib.Path(__file__).resolve().parent, env=_git_environment(),
        text=True, encoding="utf-8",
    ).strip())


def _assert_capture_source_identity(repo_root: pathlib.Path, fixture: dict) -> None:
    """Resolve the recorded producer and check its content, regardless of history.

    The expected blob is taken from the original source commit's tree, rather
    than from the refactored working file. Git hashes the complete source bytes;
    parsing also confirms the recorded function exists without executing it.
    """
    source_spec = (
        f"{fixture['source_commit_sha']}^{{commit}}:{fixture['build_script_path']}"
    )
    source = subprocess.run(
        ["git", "cat-file", "blob", source_spec],
        cwd=repo_root, env=_git_environment(), capture_output=True,
    )
    assert source.returncode == 0, (
        "historical generating source is unavailable; "
        f"provide the recorded commit and path: {source_spec}"
    )
    actual_blob = subprocess.check_output(
        ["git", "hash-object", "--stdin"],
        cwd=repo_root, env=_git_environment(), input=source.stdout,
    ).decode("utf-8").strip()
    assert actual_blob == fixture["build_script_blob_sha"], (
        f"generating source blob differs for {source_spec}: "
        f"expected {fixture['build_script_blob_sha']}, got {actual_blob}"
    )
    assert any(
        isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
        and node.name == fixture["build_script_function"]
        for node in ast.parse(source.stdout).body
    ), f"generating function is absent: {fixture['build_script_function']}"


def test_capture_source_matches_recorded_blob():
    """The historical capture identifies the exact pre-refactor producer blob."""
    fixture = json.loads(TRACE_SIG_FIXTURE.read_text(encoding="utf-8"))
    _assert_capture_source_identity(_repo_root(), fixture)


def test_refactored_polar_signatures_match_fixture():
    """Refactored make_polar_overlay (post-Phase-5) produces the same trace signature
    as captured pre-refactor. Compares trace_count, sum_theta_lengths, sum_r_lengths.
    """
    pytest.importorskip("openpyxl")  # required to read the OCIMF Coef.xlsx
    repo_root = _repo_root()
    # Add the build-script directory to sys.path so we can import the (refactored) module
    import sys
    bce_dir = str(repo_root / "scripts" / "python" / "digitalmodel" / "ocimf")
    if bce_dir not in sys.path:
        sys.path.insert(0, bce_dir)
    import importlib
    import build_coefficient_explorer as bce
    importlib.reload(bce)  # in case it was imported earlier in the session

    # The workbook location used to be a module constant holding an absolute
    # path, which is the disclosure removed by digitalmodel#1965. It is now
    # supplied by the caller, so this test takes it from the environment and
    # skips when it is not configured -- exactly as it already skipped whenever
    # the hardcoded location did not exist on the running machine.
    workbook = os.environ.get("OCIMF_COEFFICIENT_WORKBOOK", "")
    if not workbook or not pathlib.Path(workbook).is_file():
        pytest.skip(
            "OCIMF workbook unavailable; set OCIMF_COEFFICIENT_WORKBOOK to run"
        )

    rows = bce.load_all(workbook)
    df = pd.DataFrame(rows)

    fixture = json.loads(TRACE_SIG_FIXTURE.read_text(encoding="utf-8"))

    for call in fixture["polar_call_sites"]:
        fig = bce.make_polar_overlay(df, call["figure_key"], call["coef"], call["group_col"])
        key = f"{call['figure_key']}_{call['coef']}_{call['group_col']}"
        expected = fixture["signatures"][key]
        expected_names = set(expected["trace_names"])

        # Filter the refactored figure's data traces to those that match the
        # pre-refactor named-data-trace set. The refactor is ALLOWED to ADD
        # silhouette and force-arrow traces (named differently or unnamed) —
        # the regression contract is preservation of the DATA-TRACE signature
        # only.
        data_traces = [t for t in fig.data if (t.name or "") in expected_names]

        actual_trace_count = len(data_traces)
        actual_theta_sum = sum(len(t.theta) for t in data_traces)
        actual_r_sum = sum(len(t.r) for t in data_traces)
        assert actual_trace_count == expected["trace_count"], (
            f"{key}: data-trace count drift; expected {expected['trace_count']}, "
            f"got {actual_trace_count}; refactored figure had {len(fig.data)} total traces "
            f"with names {[t.name for t in fig.data]}"
        )
        assert actual_theta_sum == expected["sum_theta_lengths"], (
            f"{key}: sum_theta_lengths drift; expected {expected['sum_theta_lengths']}, got {actual_theta_sum}"
        )
        assert actual_r_sum == expected["sum_r_lengths"], (
            f"{key}: sum_r_lengths drift; expected {expected['sum_r_lengths']}, got {actual_r_sum}"
        )


# ---------- TDD #15: legal-sanity scan over the new module + tests ----------


def test_no_client_identifiers_in_visualization_package_sources():
    """r2 m3 strengthened: invoke the workspace-hub legal-sanity-scan if accessible,
    OR enumerate patterns from .legal-deny-list.yaml and grep. Hardcoding 3 patterns
    is rejected per the legal-baseline policy.
    """
    repo_root = _repo_root()
    pkg_dir = repo_root / "src" / "digitalmodel" / "marine_ops" / "marine_engineering" / "visualization"

    # Scan ONLY production source files. Baseline HTML fixtures preserve pre-existing
    # vendor-path strings byte-for-byte by design. The legal-baseline concern is
    # what ships in production code, not what test files contain.
    targets = sorted([str(p) for p in pkg_dir.glob("*.py")])
    assert targets, "no visualization source files found to scan"

    # Try workspace-hub legal-sanity-scan first when WORKSPACE_HUB_ROOT points
    # at a sibling checkout; otherwise fall back to pattern-driven grep.
    workspace_hub_root = os.environ.get("WORKSPACE_HUB_ROOT")
    workspace_hub_scan = (
        pathlib.Path(workspace_hub_root) / "scripts" / "legal" / "legal-sanity-scan.sh"
        if workspace_hub_root
        else None
    )
    script_accepts_files = False
    if workspace_hub_scan is not None and workspace_hub_scan.is_file():
        probe = subprocess.run(
            ["bash", str(workspace_hub_scan), *targets],
            capture_output=True, text=True, encoding="utf-8", env=_git_environment(),
        )
        # If the script accepts the file-list interface, returncode is 0 (clean) or 1 (flagged).
        # If it rejects the args (e.g., "Unknown argument"), it returns 2 with stderr noise — fall back.
        if probe.returncode in (0, 1) and "Unknown argument" not in (probe.stderr or ""):
            script_accepts_files = True
            assert probe.returncode == 0, (
                f"legal-sanity-scan flagged content:\nstdout={probe.stdout}\nstderr={probe.stderr}"
            )

    if not script_accepts_files:
        # Fallback: this repository's own identifier gate, which reads the
        # structural patterns and salted-hash names from .legal-deny-list.yaml.
        # A hard-coded list here would publish the names it guards against.
        gate = repo_root / "scripts" / "legal" / "check_identifiers.py"
        assert gate.is_file(), "identifier gate missing"
        out = subprocess.run(
            [sys.executable, str(gate), *targets],
            capture_output=True, text=True, encoding="utf-8", errors="replace",
            env=_git_environment(),
        )
        assert out.returncode == 0, (
            f"identifier gate flagged production source:\n{out.stdout}\n{out.stderr}"
        )
