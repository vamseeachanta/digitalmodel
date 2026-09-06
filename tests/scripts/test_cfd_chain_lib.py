"""Behavioural guards on the solve-chain shell library.

These are deliberately NOT static-source assertions. The defect that prompted
this file passed every reasonable reading of the source: `pgrep -xc` prints
"0" *and* exits 1 when nothing matches, so `pgrep -xc ... || echo 0` appended
a second line and the helper returned "0\\n0". `[ "$n" -lt 1 ]` then failed
with "integer expression expected" -- and inside an `if`, that error reads as
FALSE, so a launcher reported "solver running" over a dead solve.

It was invisible in every test where a solver happened to be alive. Only
calling the function with nothing running exposes it, so these tests run the
shell rather than read it.
"""
from __future__ import annotations

import os
import subprocess
from pathlib import Path

import pytest

LIB = (Path(__file__).resolve().parents[2]
       / "scripts" / "cfd" / "lib" / "cfd_chain.sh")
FS_GATE = LIB.parents[1] / "fs_gate.sh"

NO_SUCH = "definitelynotarealprocessname"


def run(snippet: str) -> subprocess.CompletedProcess:
    """Source the library and run a snippet against it."""
    assert LIB.is_file(), f"library not found at {LIB}"
    return subprocess.run(
        ["bash", "-c", f'source "{LIB}"\n{snippet}'],
        capture_output=True, text=True,
    )


def test_library_parses():
    assert subprocess.run(["bash", "-n", str(LIB)]).returncode == 0


def test_solver_ranks_is_a_single_clean_zero_when_nothing_matches():
    out = run(f"cfd_solver_ranks {NO_SUCH}").stdout
    assert out == "0", f"expected exactly '0', got {out!r}"
    assert "\n" not in out, "a trailing newline turns this into a two-line value"


def test_solver_ranks_result_survives_an_integer_test():
    """The real failure mode: not a wrong number, but an unusable one."""
    r = run(f'n="$(cfd_solver_ranks {NO_SUCH})"\n'
            'if [ "$n" -lt 1 ]; then echo ABSENT; else echo PRESENT; fi')
    assert "integer expression expected" not in r.stderr, r.stderr
    assert r.stdout.strip() == "ABSENT", (
        "a dead solver must read as absent; reading as PRESENT is how a "
        "launcher certifies a solve that never started")


def test_solver_ranks_counts_a_live_process():
    out = run("cfd_solver_ranks bash").stdout.strip()
    assert out.isdigit() and int(out) >= 1, f"expected a positive count, got {out!r}"


def test_root_fails_closed_when_unset():
    r = run("unset DM_CFD_ROOT\ncfd_root")
    assert r.returncode != 0, "an unset case root must be fatal, not defaulted"
    assert "DM_CFD_ROOT" in r.stderr


def test_root_rejects_a_nonexistent_directory():
    r = run('DM_CFD_ROOT=/nonexistent/case/root cfd_root')
    assert r.returncode != 0


@pytest.mark.parametrize("banned", ["pgrep -f", "pkill -f"])
def test_library_never_matches_on_command_line(banned: str):
    """`-f` matches the ssh command line carrying the pattern.

    That self-match produced a 13.5 h zombie supervisor on this fleet, and
    `pkill -f` with the same pattern killed the operator's own session.
    """
    code = "\n".join(ln for ln in LIB.read_text().splitlines()
                     if not ln.lstrip().startswith("#"))
    assert banned not in code


def test_case_directory_is_a_parameter_not_a_benchmark_name():
    """The chain must not require a client hull to live under `kcs_cases`.

    It did: the subdirectory was a literal, so the orchestration was shaped
    around the benchmark it happened to be written for. The default is kept
    for existing roots, but it has to be overridable or client work cannot
    use the committed chain at all.
    """
    r = run('DM_CFD_ROOT=/tmp cfd_case_dir demo')
    assert r.stdout.strip() == "/tmp/kcs_cases/demo", r.stdout
    r = run('DM_CFD_ROOT=/tmp DM_CFD_CASES_DIR=cases cfd_case_dir demo')
    assert r.stdout.strip() == "/tmp/cases/demo", r.stdout


def test_campaign_is_inferred_from_case_working_directory(tmp_path: Path):
    home = tmp_path / "home"
    case = home / "cfd" / "sea_trial" / "cases" / "run-01"
    case.mkdir(parents=True)
    result = subprocess.run(
        ["bash", "-c", f'source "{LIB}"\ncfd_campaign'],
        cwd=case, env=dict(os.environ, HOME=str(home)), capture_output=True, text=True,
    )
    assert result.stdout == "sea_trial"
    assert "campaign sea_trial" in result.stderr
    assert "working directory" in result.stderr


def test_campaign_uses_literal_fallback_when_working_directory_is_unrelated(tmp_path: Path):
    home = tmp_path / "home"
    home.mkdir()
    result = subprocess.run(
        ["bash", "-c", f'source "{LIB}"\ncfd_campaign'],
        cwd=tmp_path, env=dict(os.environ, HOME=str(home)), capture_output=True, text=True,
    )
    assert result.stdout == "campaign"
    assert "campaign campaign" in result.stderr
    assert "literal fallback" in result.stderr


def test_fs_gate_infers_campaign_from_case_working_directory(tmp_path: Path):
    home = tmp_path / "home"
    case = home / "cfd" / "sea_trial" / "cases" / "run-01"
    case.mkdir(parents=True)
    result = subprocess.run(
        ["bash", str(FS_GATE), "run-01"], cwd=case,
        env=dict(os.environ, HOME=str(home)), capture_output=True, text=True,
    )
    assert "resolved campaign sea_trial from working directory" in result.stderr
    assert "no case" not in result.stdout + result.stderr


def test_fs_gate_literal_campaign_fallback_still_selects_case(tmp_path: Path):
    home = tmp_path / "home"
    case = home / "cfd" / "campaign" / "cases" / "run-01"
    unrelated = tmp_path / "elsewhere"
    case.mkdir(parents=True); unrelated.mkdir()
    result = subprocess.run(
        ["bash", str(FS_GATE), "run-01"], cwd=unrelated,
        env=dict(os.environ, HOME=str(home)), capture_output=True, text=True,
    )
    assert "resolved campaign campaign from literal fallback" in result.stderr
    assert "no case" not in result.stdout + result.stderr
