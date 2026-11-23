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


@pytest.fixture(scope="module")
def code(source: str) -> str:
    """Executable lines only.

    The comments in this script quote the exact failures they guard against,
    so a naive substring search finds the prose and passes while the code
    does the wrong thing. Asserting on comment-stripped source is the
    difference between testing the guard and testing its documentation.
    """
    return "\n".join(ln for ln in source.splitlines()
                     if not ln.lstrip().startswith("#"))


@pytest.mark.skipif(shutil.which("bash") is None, reason="bash not available")
def test_script_is_syntactically_valid():
    """A syntax error only surfaces after a 26 h solve finishes."""
    r = subprocess.run(["bash", "-n", str(SCRIPT)],
                       capture_output=True, text=True)
    assert r.returncode == 0, r.stderr


def test_waits_for_the_solver_before_taking_the_cores(code: str):
    """postProcess wants every rank, which interFoam holds while running.

    This used to assert the literal string `pgrep -f "solve_chain.sh"`, which
    pinned a NAME rather than a PROPERTY: the suite stayed green while the
    orchestrator it named had never existed in the repo at all. It also
    mandated the one construct every other script in this directory is
    forbidden to use.
    """
    assert re.search(r"while\s+\[\s+!\s+-f\s+\"\$CHAIN_MARKER\"", code), \
        "must wait on the chain's terminal marker, not sleep a fixed interval"


def test_never_waits_on_a_process_name(code: str):
    """`pgrep -f` matches the command line CARRYING the pattern.

    So a collector launched over ssh with the chain's name in its own argv
    matches itself and waits forever. That self-match produced a 13.5 h zombie
    on this fleet, and `pkill -f` with the same pattern killed the operator's
    own session.
    """
    for banned in ("pgrep -f", "pkill -f"):
        assert banned not in code, f"{banned} self-matches its own invocation"


def test_distinguishes_how_the_chain_ended(code: str):
    """Process absence conflates completion, failure, budget kill and death.

    The chain writes a marker on both outcomes, so the collector can record
    which one happened instead of treating silence as success.
    """
    assert "CHAIN_VERDICT" in code


def test_mpirun_closes_stdin(code: str):
    """mpirun reads and closes stdin.

    An mpirun inside a piped script swallows the remainder of the script; the
    lines after it silently never run while the wrapper still reports success.
    """
    # Join line continuations first: the invocation is wrapped across two
    # lines, so a per-line regex sees only the head and misses the redirect
    # that is genuinely there -- a false failure is as bad as a false pass.
    joined = re.sub(r"\\\s*\n\s*", " ", code)
    runs = re.findall(r"mpirun[^\n]*", joined)
    assert runs, "guard would pass vacuously if no mpirun were present"
    for r in runs:
        assert "/dev/null" in r, f"mpirun without stdin redirect: {r}"


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


def test_runs_every_registered_level_and_does_not_stop_at_the_first_failure(
        code: str):
    """The companion is what separates grid error from modelling error.

    Losing it because production failed would discard the more diagnostic of
    the two.

    This previously asserted the literal `for case in kcs_production
    kcs_companion`. That pinned a NAME: it broke when the list moved into the
    registry although nothing was wrong, and it would have kept passing had
    the list silently shrunk to a single level.
    """
    assert re.search(r"for case in \$\(cfd_cases\)", code), \
        "the level list must come from the registry, not be hard-coded"
    body = code[code.index("for case in"):]
    assert "continue" in body, "a failed level must not abort the loop"
    assert "exit 1" not in body.split("YPLUS COLLECTION COMPLETE")[0], \
        "a failed level must not exit the collector"


def test_uses_the_solver_postprocess_form_not_bare_postprocess(code: str):
    """Bare `postProcess -func yPlus` does not construct the turbulence model.

    It writes a field of zeros and exits 0. This is not hypothetical — the
    first version of this script did exactly that against the real solved
    case, and reported "OK ... 1 patch summaries" over min=0 max=0 average=0.
    OpenFOAM prints the remedy itself: use `<solver> -postProcess`.
    """
    # Every invocation must be the solver form. Counting is clearer than a
    # lookbehind here, which reads the hyphen in "-postProcess" and lets the
    # bare form through.
    total = code.count("postProcess -func yPlus")
    solver_form = code.count("interFoam -postProcess -func yPlus")
    assert total > 0, "the collector no longer runs yPlus at all"
    assert solver_form == total, (
        f"{total - solver_form} bare postProcess invocation(s) remain; "
        "the bare form produces a silently zero y+")


def test_rejects_an_identically_zero_result(code: str):
    """y+ = 0 on a wetted wall is physically impossible, so it is a failure.

    A parsed line is not a measured line. The original guard counted
    summaries and passed on a field of zeros.
    """
    assert "identically zero" in code
    assert "min = 0" in code and "max = 0" in code


def test_rejects_a_missing_turbulence_model(code: str):
    """The exact warning OpenFOAM emits when y+ was not calculated."""
    assert "Unable to find turbulence model" in code


@pytest.mark.parametrize("failure", [
    "Unable to find turbulence model",
    "identically zero",
])
def test_every_null_result_path_continues_rather_than_reporting_ok(
        code: str, failure: str):
    """Each null-result branch must skip the OK line, not fall through to it."""
    idx = code.index(failure)
    tail = code[idx:idx + 300]
    assert "continue" in tail, f"{failure!r} branch does not skip the OK report"


def test_declares_that_it_gates_nothing(source: str):
    """It was added after the running coefficients were seen.

    The provenance of that decision has to survive in the file, or a later
    reader will reasonably assume y+ was always part of the criteria.
    """
    assert "NOT A GATE" in source.upper()
    assert re.search(r"V1/V2a/V2b/V3 stand unchanged", source)
