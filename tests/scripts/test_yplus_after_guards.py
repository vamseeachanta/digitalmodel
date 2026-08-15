"""Guards on the #1173 post-solve y+ collector.

This script runs unattended on a remote host against a solve that costs ~26 h
to reproduce. The properties worth pinning are not what it computes — they are
what it must never do:

  1. never write into a live case's configuration;
  2. never report a y+ for a case that produced no solution;
  3. never start while the solver still holds the cores.

Each has bitten this repo or this issue before, which is why they are asserted
rather than trusted to review.
"""
from __future__ import annotations

import re
import shutil
import subprocess
from pathlib import Path

import pytest

SCRIPT = (Path(__file__).resolve().parents[2]
          / "scripts" / "cfd" / "yplus_after.sh")


@pytest.fixture(scope="module")
def source() -> str:
    assert SCRIPT.is_file(), f"collector not found at {SCRIPT}"
    return SCRIPT.read_text()


@pytest.mark.skipif(shutil.which("bash") is None, reason="bash not available")
def test_script_is_syntactically_valid():
    """A syntax error only surfaces after a 26 h solve finishes."""
    r = subprocess.run(["bash", "-n", str(SCRIPT)],
                       capture_output=True, text=True)
    assert r.returncode == 0, r.stderr


def test_waits_for_the_solver_before_taking_the_cores(source: str):
    """postProcess wants all 8 ranks, which interFoam holds while running."""
    assert "pgrep -f \"solve_chain.sh\"" in source
    assert re.search(r"while\s+pgrep.*solve_chain", source), \
        "must wait on the chain, not sleep a fixed interval"


def test_never_writes_to_the_live_case_configuration(source: str):
    """Editing a running case's controlDict can abort the solve.

    The collector is a separate later-running script precisely so it never
    needs to. Any write into system/ or constant/ is a regression.
    """
    forbidden = re.compile(
        r">\s*\S*(system|constant)/|"          # redirect into config dirs
        r"\b(sed\s+-i|tee)\b.*(controlDict|fvSolution|fvSchemes)|"
        r"\bcp\b[^\n]*\b(controlDict|fvSolution|fvSchemes)\b"
    )
    offenders = [ln.strip() for ln in source.splitlines()
                 if forbidden.search(ln) and not ln.strip().startswith("#")]
    assert not offenders, f"collector writes into a live case: {offenders}"


def test_skips_a_case_that_produced_no_written_time(source: str):
    """Otherwise postProcess emits y+ for the initial condition.

    That number looks like a result and is not one — the exact shape of
    defect this issue's gates exist to prevent.
    """
    assert "processor0/[1-9]*" in source, \
        "must require a written time directory beyond 0"
    assert re.search(r'ntimes.*-eq 0', source), "must skip when none exist"


def test_a_zero_exit_with_no_parsed_summary_is_not_treated_as_success(
        source: str):
    """rc=0 with an unrecognised log format is a format mismatch, not a pass."""
    assert re.search(r'hits.*-eq 0', source)
    assert "WARN" in source


def test_runs_both_levels_and_does_not_stop_at_the_first_failure(source: str):
    """The companion is what separates grid error from modelling error.

    Losing it because production failed would discard the more diagnostic of
    the two.
    """
    assert "for case in kcs_production kcs_companion" in source
    body = source[source.index("for case in"):]
    assert "continue" in body, "a failed level must not abort the loop"
    assert "exit 1" not in body.split("YPLUS COLLECTION COMPLETE")[0] \
        .replace('say "FATAL cannot source OpenFOAM"; exit 1', ""), \
        "only the OpenFOAM-source failure may exit early"


def test_declares_that_it_gates_nothing(source: str):
    """It was added after the running coefficients were seen.

    The provenance of that decision has to survive in the file, or a later
    reader will reasonably assume y+ was always part of the criteria.
    """
    assert "NOT A GATE" in source.upper()
    assert re.search(r"V1/V2a/V2b/V3 stand unchanged", source)
