# Copyright (c) 2026 AceEngineer. Licensed under the MIT License.
"""Report evals: every results.json meets the contract, and the offline evals pass.

The CP edition eval needs network access for the BSEE file and is run by the
scheduled report-eval job, not here.
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import pytest

EVALS = Path(__file__).resolve().parents[2] / "examples" / "report-evals"
ALL = sorted(p.name for p in EVALS.iterdir() if (p / "results.json").exists())
OFFLINE = ["issue-001-hull-roll-screening", "issue-001-sloshing-frequency"]


def _run(folder: str, *args: str) -> subprocess.CompletedProcess:
    return subprocess.run([sys.executable, str(EVALS / folder / "eval.py"), *args],
                          capture_output=True, text=True, encoding="utf-8", timeout=600)


@pytest.mark.unit
@pytest.mark.parametrize("folder", ALL)
def test_results_json_meets_contract(folder):
    out = _run(folder, "--validate-only")
    assert out.returncode == 0, out.stdout + out.stderr


@pytest.mark.parametrize("folder", OFFLINE)
def test_offline_eval_passes(folder):
    out = _run(folder)
    assert out.returncode == 0, out.stdout + out.stderr
    assert "GRADE: PASS" in out.stdout


def test_compare_grades_reader_file(tmp_path):
    """A reader file with one wrong value fails; with one value missing it is partial."""
    good = tmp_path / "good.json"
    good.write_text('{"results": [{"id": "reduction_18s_2pct_alpha005", "value": 46.1}]}', encoding="utf-8")
    assert _run(OFFLINE[0], "--compare", str(good)).returncode == 2  # other results missing
    bad = tmp_path / "bad.json"
    bad.write_text('{"results": [{"id": "reduction_18s_2pct_alpha005", "value": 40.0}]}', encoding="utf-8")
    assert _run(OFFLINE[0], "--compare", str(bad)).returncode == 1
