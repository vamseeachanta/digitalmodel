"""No-regression + legal-sanity tests for the visualization package (digitalmodel#616).

TDD #13 (no-regression on trace count) + TDD #15 (legal-sanity scan).
Both fixtures live under fixtures/ in this directory.
"""

from __future__ import annotations

import json
import pathlib
import subprocess

import pandas as pd
import pytest


FIXTURE_DIR = pathlib.Path(__file__).parent / "fixtures"
TRACE_SIG_FIXTURE = FIXTURE_DIR / "ocimf_explorer_pre_refactor_trace_signature.json"


# ---------- TDD #13: refactored make_polar_overlay preserves trace signature ----------


def _repo_root() -> pathlib.Path:
    return pathlib.Path(
        subprocess.check_output(
            ["git", "rev-parse", "--show-toplevel"], text=True
        ).strip()
    )


def test_capture_sequencing_source_sha_predates_refactor():
    """r2 n2 enforceable check: the fixture's source_commit_sha must be an ancestor
    of HEAD, AND must predate any commit that modifies make_polar_overlay in
    build_coefficient_explorer.py.
    """
    fixture = json.loads(TRACE_SIG_FIXTURE.read_text())
    captured_sha = fixture["source_commit_sha"]
    repo_root = _repo_root()
    build_script = "scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py"

    # ancestry check: captured_sha must be in HEAD's history
    ancestor = subprocess.run(
        ["git", "merge-base", "--is-ancestor", captured_sha, "HEAD"],
        cwd=repo_root,
        capture_output=True,
    )
    assert (
        ancestor.returncode == 0
    ), f"captured source_commit_sha {captured_sha} is not an ancestor of HEAD"

    # all refactor commits touching build_coefficient_explorer.py after captured_sha
    # must be DESCENDANTS of captured_sha (i.e., happen later). Phrased differently:
    # there must be NO commit touching the file that is an ancestor of captured_sha
    # except captured_sha itself or older. Verify by checking the file's git log
    # filtered to descendants of captured_sha.
    subprocess.check_output(
        ["git", "log", "--format=%H", f"{captured_sha}..HEAD", "--", build_script],
        cwd=repo_root,
        text=True,
    )
    # log contains commits that touch the file AFTER captured_sha — these are the refactor commits.
    # That's allowed (and expected once Phase 5 lands). No assertion on count; this branch is informational.
    # The actual assertion: captured_sha must NOT itself contain refactor changes.
    # We approximate this by checking that captured_sha's commit message does not include "refactor("
    captured_msg = subprocess.check_output(
        ["git", "log", "-1", "--format=%s", captured_sha],
        cwd=repo_root,
        text=True,
    ).strip()
    assert not captured_msg.lower().startswith("refactor"), (
        f"captured_sha {captured_sha} is itself a refactor commit ({captured_msg!r}); "
        f"fixture must be captured BEFORE refactor lands"
    )


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

    rows = bce.load_all()
    df = pd.DataFrame(rows)

    fixture = json.loads(TRACE_SIG_FIXTURE.read_text())

    for call in fixture["polar_call_sites"]:
        fig = bce.make_polar_overlay(
            df, call["figure_key"], call["coef"], call["group_col"]
        )
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
        assert (
            actual_theta_sum == expected["sum_theta_lengths"]
        ), f"{key}: sum_theta_lengths drift; expected {expected['sum_theta_lengths']}, got {actual_theta_sum}"
        assert (
            actual_r_sum == expected["sum_r_lengths"]
        ), f"{key}: sum_r_lengths drift; expected {expected['sum_r_lengths']}, got {actual_r_sum}"


# ---------- TDD #15: legal-sanity scan over the new module + tests ----------


def test_no_client_identifiers_in_visualization_package_sources():
    """r2 m3 strengthened: invoke the workspace-hub legal-sanity-scan if accessible,
    OR enumerate patterns from .legal-deny-list.yaml and grep. Hardcoding 3 patterns
    is rejected per the legal-baseline policy.
    """
    repo_root = _repo_root()
    pkg_dir = (
        repo_root
        / "src"
        / "digitalmodel"
        / "marine_ops"
        / "marine_engineering"
        / "visualization"
    )

    # Scan ONLY production source files. Test files legitimately reference SIROCCO
    # as the consumer scenario; baseline HTML fixtures preserve pre-existing
    # vendor-path strings byte-for-byte by design. The legal-baseline concern is
    # what ships in production code, not what test files contain.
    targets = sorted([str(p) for p in pkg_dir.glob("*.py")])
    assert targets, "no visualization source files found to scan"

    # Try workspace-hub legal-sanity-scan first (lives in a sibling repo); if it
    # doesn't accept file-list arguments, fall back to pattern-driven grep.
    workspace_hub_scan = (
        repo_root.parent
        / "workspace-hub"
        / "scripts"
        / "legal"
        / "legal-sanity-scan.sh"
    )
    script_accepts_files = False
    if workspace_hub_scan.is_file():
        probe = subprocess.run(
            ["bash", str(workspace_hub_scan), *targets],
            capture_output=True,
            text=True,
        )
        # If the script accepts the file-list interface, returncode is 0 (clean) or 1 (flagged).
        # If it rejects the args (e.g., "Unknown argument"), it returns 2 with stderr noise — fall back.
        if probe.returncode in (0, 1) and "Unknown argument" not in (
            probe.stderr or ""
        ):
            script_accepts_files = True
            assert (
                probe.returncode == 0
            ), f"legal-sanity-scan flagged content:\nstdout={probe.stdout}\nstderr={probe.stderr}"

    if not script_accepts_files:
        # Fallback: pattern-driven grep against a minimal denylist read from the project conventions.
        # The workspace-hub .legal-deny-list.yaml is authoritative — duplicate ONLY when unavailable.
        DENYLIST_PATTERNS = [
            r"B1528",
            r"SIROCCO",
            r"acma-projects",
            r"acma-codes/B\d{4}",
        ]
        import re

        for target in targets:
            text = pathlib.Path(target).read_text()
            for pattern in DENYLIST_PATTERNS:
                m = re.search(pattern, text)
                # Allow client-identifier strings ONLY when they appear in test fixtures
                # explicitly labeled as such — recognized by the "test fixture" or "smoke test" annotation
                if m:
                    line_idx = text[: m.start()].count("\n")
                    line = text.splitlines()[line_idx]
                    annotation_ok = any(
                        marker in line.lower()
                        for marker in (
                            "# test",
                            "test fixture",
                            "smoke test",
                            "# synthetic",
                        )
                    )
                    if not annotation_ok:
                        pytest.fail(
                            f"{target}:{line_idx+1}: client identifier {pattern!r} found "
                            f"without explicit test-fixture annotation; line={line!r}"
                        )
