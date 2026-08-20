"""Guards on the #1173 / #2023 CFD supervision scripts.

These four scripts supervise unattended OpenFOAM solves that cost days of
compute to reproduce. They are also the scripts where this fleet's worst
failures actually happened, so the properties pinned here are not "what they
compute" but "what they must never do again":

  1. never detect a supervised process by command-line pattern — `pgrep -f`
     matches the ssh command line carrying the pattern, so a supervisor sees
     itself and never exits (a 13.5 h zombie poller), and `pkill -f` with the
     same pattern killed the operator's own session;
  2. never read a fixed coefficient filename — OpenFOAM VERSIONS function
     object output on re-run (coefficient.dat -> coefficient_0.dat), so a
     fixed name silently reads the PREVIOUS, dead run. Caught live: the
     watcher was applying its criterion to a dead 9,011-iteration history
     while the live run sat at 455;
  3. never arm the ITTC watcher against a case whose controlDict has no
     matching `abort` function object — the watcher only touches a file, and
     a case shipped without the block, so the trigger did nothing;
  4. never invoke `mpirun` without `< /dev/null` — mpirun reads and closes
     stdin, which swallowed a solver launch line while the wrapper reported
     success;
  5. never let silence be readable as either success or failure — a terminal
     marker on BOTH paths;
  6. never hard-code a host path — the originals ran on exactly one host and
     one account because they hard-coded the case root.

Each of these has bitten this issue, which is why they are asserted rather
than trusted to review.
"""
from __future__ import annotations

import re
import shutil
import subprocess
from pathlib import Path

import pytest

CFD = Path(__file__).resolve().parents[2] / "scripts" / "cfd"

POLLER = CFD / "poller.sh"
WATCH = CFD / "ittc_watch.sh"
AUTO = CFD / "auto_solve.sh"
SOLVE = CFD / "solve_case.sh"

ALL_SCRIPTS = (POLLER, WATCH, AUTO, SOLVE)


def _source(path: Path) -> str:
    assert path.is_file(), f"supervision script not found at {path}"
    return path.read_text()


def _strip_comments(source: str) -> str:
    """Executable lines only.

    The comments in these scripts quote the exact failures they guard
    against, so a naive substring search finds the prose and passes while the
    code does the wrong thing. Asserting on comment-stripped source is the
    difference between testing the guard and testing its documentation.
    """
    return "\n".join(ln for ln in source.splitlines()
                     if not ln.lstrip().startswith("#"))


@pytest.fixture(scope="module")
def sources() -> dict[Path, str]:
    return {p: _source(p) for p in ALL_SCRIPTS}


@pytest.fixture(scope="module")
def code(sources: dict[Path, str]) -> dict[Path, str]:
    return {p: _strip_comments(s) for p, s in sources.items()}


# --------------------------------------------------------------------------- #
# The scripts exist and parse.
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=lambda p: p.name)
@pytest.mark.skipif(shutil.which("bash") is None, reason="bash not available")
def test_script_is_syntactically_valid(script: Path):
    """A syntax error only surfaces after a multi-day solve is already lost."""
    assert script.is_file(), f"missing {script}"
    r = subprocess.run(["bash", "-n", str(script)],
                       capture_output=True, text=True)
    assert r.returncode == 0, r.stderr


@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=lambda p: p.name)
def test_sources_the_shared_contract(code: dict[Path, str], script: Path):
    """Every guard lives in lib/cfd_chain.sh; a script that reimplements one
    reimplements the defect it was written against."""
    body = code[script]
    assert re.search(r"(source|\.)\s+.*lib/cfd_chain\.sh", body), \
        f"{script.name} does not source the shared contract"


# --------------------------------------------------------------------------- #
# 1. Process detection must never be pattern-based.
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=lambda p: p.name)
def test_never_detects_a_process_by_command_line_pattern(
        code: dict[Path, str], script: Path):
    """`pgrep -f` matches the ssh command line carrying the pattern.

    The supervisor then sees itself and never exits: a 13.5 h zombie poller
    on this fleet. `pkill -f` with the same pattern killed the operator's own
    session. Executable-name matching (`pgrep -x`, via cfd_solver_running /
    cfd_solver_ranks) cannot self-match.
    """
    offenders = [ln.strip() for ln in code[script].splitlines()
                 if re.search(r"\bp(grep|kill)\b[^\n]*\s-\w*f", ln)]
    assert not offenders, \
        f"{script.name} matches processes by command line: {offenders}"


@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=lambda p: p.name)
def test_any_process_detection_goes_through_the_shared_helpers(
        code: dict[Path, str], script: Path):
    """Direct pgrep is allowed only in the `-x` (executable-name) form."""
    for ln in code[script].splitlines():
        if re.search(r"\bpgrep\b", ln):
            assert re.search(r"\bpgrep\s+-\w*x", ln), \
                f"{script.name}: non -x pgrep: {ln.strip()}"


def test_the_watcher_and_launcher_use_the_shared_liveness_helpers(
        code: dict[Path, str]):
    """Liveness is the property both scripts got wrong; it has one implementation."""
    assert "cfd_solver_running" in code[WATCH], \
        "watcher must use cfd_solver_running, not its own detection"
    assert "cfd_solver_ranks" in code[SOLVE], \
        "launcher must confirm ranks via cfd_solver_ranks"


def test_the_poller_terminates_by_pid_never_by_name(code: dict[Path, str]):
    """A budget kill is the one place a mistake destroys days of compute.

    Every signal must name a pid or a numeric group, never a pattern.
    """
    body = code[POLLER]
    kills = [ln.strip() for ln in body.splitlines() if re.search(r"\bkill\b", ln)]
    assert kills, "the poller no longer enforces the budget at all"
    for ln in kills:
        assert not re.search(r"kill[^\n]*\s(interFoam|mpirun|\"\$CASE)", ln), \
            f"poller kills by name/pattern: {ln}"
    assert re.search(r"ps\s+-o\s+pgid=", body), \
        "poller must resolve process groups from ps"


def test_the_poller_takes_its_kill_target_from_the_launch_record(
        code: dict[Path, str]):
    """The target must be explicit, not inherited from whatever `ps` says at
    kill time, and it must still agree with the live group of the pid."""
    body = code[POLLER]
    assert "rec_pgid" in body, "the poller ignores the recorded pgid"
    assert re.search(r"disagrees with the live group", body), \
        "a record whose pgid no longer matches the live group must be refused"


def test_the_launcher_records_the_real_process_group_not_its_own(
        code: dict[Path, str]):
    """A job started with `&` from a non-interactive shell does NOT become a
    group leader — it inherits the launcher's group.

    A record that writes `$$` therefore names the LAUNCHER's group, and a
    budget kill against it decapitates the supervisor instead of the job. The
    original record did exactly that, on every run.
    """
    body = code[SOLVE]
    assert re.search(r'SOLVER_PGID="\$\(ps\s+-o\s+pgid=\s+-p\s+"\$SOLVER_PID"', body), \
        "launcher must read the real process group of the launched pid"
    assert '"pgid": int(pgid)' in body, \
        "the launch record must carry the verified pgid"
    assert not re.search(r'"pgid"\s*:\s*\$\$', body), \
        "the record must never write the launcher's own pid as the pgid"


def test_the_poller_refuses_to_signal_its_own_or_an_ancestors_group(
        code: dict[Path, str]):
    """A self-only check is not enough.

    The solve driver is started with `&` from the chain script, so it inherits
    the CHAIN's group. If the poller runs inside that same group,
    `kill -TERM -<pgid>` kills the supervisor that is meant to observe the
    kill — and the group belongs to an ancestor, not to this process, so an
    "own group" comparison passes. Verified live while building this port: an
    own-group check passed and the signal still terminated the launching
    shell.
    """
    body = code[POLLER]
    assert "_forbidden_pgids" in body, \
        "the poller has no forbidden-group set"
    assert re.search(r"_ppid_of", body), \
        "the forbidden set must be built by walking the ancestor chain"
    # The refusal must precede every group signal.
    refusal = body.index("_forbidden_pgids()")
    check = body.index("for f in $(_forbidden_pgids)")
    first_group_kill = body.index('kill -TERM -"$target"')
    assert refusal < check < first_group_kill, \
        "the ancestor-group refusal does not precede the group signal"


def test_the_poller_proves_the_target_group_holds_the_job(
        code: dict[Path, str]):
    """Not-obviously-wrong is not the same as verified.

    Before signalling a group, at least one process in it must be the solver
    executable — positive proof the target is the job and not a supervisor.
    """
    body = code[POLLER]
    assert "_group_holds_solver" in body
    assert re.search(r"if\s+!\s+_group_holds_solver", body), \
        "an unverified group must be refused, not signalled"
    assert body.index("if ! _group_holds_solver") < body.index('kill -TERM -"$target"')


def test_the_poller_signals_ranks_by_pid_before_escalating_to_the_group(
        code: dict[Path, str]):
    """Killing the whole group also kills the driver, so the chain can never
    log its own termination line.

    TERM the ranks by their own pids first: the driver's `wait` returns, the
    chain observes the termination, and the group signal stays available for
    ranks that survive.
    """
    body = code[POLLER]
    rank_kill = body.index('kill -TERM "$rp"')
    group_kill = body.index('kill -TERM -"$target"')
    assert rank_kill < group_kill, \
        "the group is signalled before the ranks are given a chance to exit"
    assert re.search(r"escalating to process group", body), \
        "the group signal must be an escalation, not the first move"


@pytest.mark.parametrize("script", [POLLER, SOLVE], ids=lambda p: p.name)
def test_rank_counts_from_the_shared_helper_are_normalised(
        code: dict[Path, str], script: Path):
    """cfd_solver_ranks is `pgrep -xc NAME || echo 0`.

    `pgrep -xc` PRINTS "0" and ALSO exits 1 when nothing matches, so the
    fallback appends a second line and the helper returns "0\\n0". Left
    unnormalised, `[ "$n" -lt 1 ]` errors with "integer expression expected",
    and inside an `if` that error reads as FALSE — i.e. as a solver that
    started. Observed while building this port.
    """
    body = code[script]
    for ln in body.splitlines():
        if "cfd_solver_ranks" in ln and "=" in ln:
            assert "tail -1" in ln, \
                f"{script.name}: unnormalised rank count: {ln.strip()}"


# --------------------------------------------------------------------------- #
# 2. The coefficient file must be resolved newest-first.
# --------------------------------------------------------------------------- #

def test_coefficient_file_is_resolved_newest_first(code: dict[Path, str]):
    """OpenFOAM versions function-object output instead of clobbering it.

    A fixed name reads the PREVIOUS run. Caught live: the criterion was being
    applied to a dead 9,011-iteration history while the live run sat at 455 —
    the stop was effectively not armed.
    """
    body = code[WATCH]
    assert re.search(r"ls\s+-t\b[^\n]*coefficient\*\.dat", body), \
        "watcher must resolve the newest coefficient*.dat, not a fixed name"
    assert re.search(r"head\s+-1", body), \
        "watcher must take only the newest match"


def test_no_fixed_coefficient_filename_anywhere(code: dict[Path, str]):
    """`coefficient.dat` as a literal is the defect itself."""
    for script, body in code.items():
        bad = [ln.strip() for ln in body.splitlines()
               if re.search(r"coefficient\.dat", ln)]
        assert not bad, f"{script.name} pins a fixed coefficient file: {bad}"


# --------------------------------------------------------------------------- #
# 3. The stop is inert without the case-side abort function object.
# --------------------------------------------------------------------------- #

def test_the_launcher_verifies_the_abort_wiring_before_arming_the_watcher(
        code: dict[Path, str]):
    """The watcher only touches a file; the case must be wired to read it.

    A case shipped without the `abort` function object, so the trigger did
    nothing and the run went to endTime. The launcher must check the case's
    controlDict and fail loudly, not arm a watcher whose stop cannot act.
    """
    body = code[SOLVE]
    assert "controlDict" in body, "launcher never inspects the case controlDict"
    assert re.search(r"type\s+abort", body), \
        "launcher must require an `abort` function object"
    assert "ITTC_CONVERGED" in body, \
        "launcher must require the abort file to be the ITTC trigger"
    assert re.search(r"writeNow", body), \
        "launcher must require action writeNow so the last time is written"
    # The check must be fatal, and it must precede the watcher launch.
    assert re.search(r"cfd_die[^\n]*abort", body, re.IGNORECASE), \
        "missing abort wiring must be fatal, not a warning"
    assert body.index("type abort") < body.index("ittc_watch"), \
        "the abort wiring is verified after the watcher is armed"


def test_the_launcher_clears_a_stale_trigger_before_solving(
        code: dict[Path, str]):
    """A leftover ITTC_CONVERGED from the previous run stops the new solve
    at its first write."""
    assert re.search(r"rm\s+-f[^\n]*ITTC_CONVERGED", code[SOLVE]), \
        "launcher must remove a stale convergence trigger before launching"


# --------------------------------------------------------------------------- #
# 4. mpirun reads and closes stdin.
# --------------------------------------------------------------------------- #

def test_every_mpirun_invocation_closes_stdin(code: dict[Path, str]):
    """mpirun reads and closes stdin.

    Piped via `ssh bash -s`, an mpirun swallowed the remainder of the script:
    the solver launch never executed, ssh returned 0, and the lane reported
    OK while dead.
    """
    seen = 0
    for script, body in code.items():
        for ln in body.splitlines():
            if re.search(r"\bmpirun\b", ln):
                seen += 1
                assert "< /dev/null" in ln, \
                    f"{script.name}: mpirun without `< /dev/null`: {ln.strip()}"
    assert seen > 0, "no mpirun invocation found; the launcher no longer solves"


# --------------------------------------------------------------------------- #
# 5. Terminal markers on both paths.
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=lambda p: p.name)
def test_writes_a_terminal_marker_on_failure_and_on_success(
        code: dict[Path, str], script: Path):
    """Silence must never be readable as success OR as failure.

    An EXIT trap covers every non-zero path including cfd_die, which a
    hand-placed marker at the end of the happy path does not.
    """
    body = code[script]
    assert re.search(r"trap\s+'[^']*cfd_marker_fail[^']*'\s+EXIT", body), \
        f"{script.name} has no EXIT trap writing a failure marker"
    assert "cfd_marker_ok" in body, \
        f"{script.name} never writes a success marker"


@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=lambda p: p.name)
def test_the_marker_path_is_set_before_the_trap_is_installed(
        code: dict[Path, str], script: Path):
    """A trap that fires before CFD_MARKER exists writes nothing."""
    body = code[script]
    assert body.index("CFD_MARKER") < body.index("trap "), \
        f"{script.name} installs the EXIT trap before CFD_MARKER is resolvable"


# --------------------------------------------------------------------------- #
# 6. No host-specific assumptions.
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=lambda p: p.name)
def test_no_hard_coded_host_paths(code: dict[Path, str], script: Path):
    """The originals hard-coded the case root, which is why they ran on
    exactly one host and one account."""
    body = code[script]
    forbidden = re.compile(r"\$HOME/cfd|/home/[a-z]|/Users/|/mnt/(ace|local-analysis|dde)/")
    offenders = [ln.strip() for ln in body.splitlines() if forbidden.search(ln)]
    assert not offenders, f"{script.name} hard-codes a host path: {offenders}"


@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=lambda p: p.name)
def test_case_directories_come_from_the_shared_resolver(
        code: dict[Path, str], script: Path):
    """cfd_case_dir / cfd_root fail closed when DM_CFD_ROOT is unset.

    Solving into an unintended directory is worse than not solving.
    """
    body = code[script]
    assert re.search(r"cfd_(case_dir|root)\b", body), \
        f"{script.name} resolves its case root without the shared resolver"


@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=lambda p: p.name)
def test_never_sources_the_openfoam_bashrc_directly(
        code: dict[Path, str], script: Path):
    """openfoam2312/etc/bashrc aborts the shell under `set -e` or `set -u`.

    cfd_load_openfoam saves and restores the caller's flags around it. Both
    flags have bitten this chain separately.
    """
    body = code[script]
    offenders = [ln.strip() for ln in body.splitlines()
                 if re.search(r"(source|^\s*\.)\s+\S*etc/bashrc", ln)]
    assert not offenders, \
        f"{script.name} sources the OpenFOAM bashrc directly: {offenders}"


def test_the_launcher_loads_openfoam_through_the_shared_helper(
        code: dict[Path, str]):
    assert "cfd_load_openfoam" in code[SOLVE], \
        "the launcher must load OpenFOAM via cfd_load_openfoam"


def test_case_names_are_not_baked_into_the_supervision_scripts(
        code: dict[Path, str]):
    """The registry in config/cfd/kcs_chain.yml is the case list.

    A supervisor that names its case cannot supervise the next one.
    """
    for script, body in code.items():
        baked = [ln.strip() for ln in body.splitlines()
                 if re.search(r"\bkcs_(fine|production|companion|prod_yplus)\b", ln)]
        assert not baked, f"{script.name} bakes in a case name: {baked}"


# --------------------------------------------------------------------------- #
# The ITTC criterion itself.
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("param", [
    "MIN_ITER", "WINDOW", "WINDOWS", "SPREAD_PCT", "DRIFT_PCT", "HOLD",
])
def test_ittc_parameters_are_named_and_overridable(
        code: dict[Path, str], param: str):
    """They were tuned per run (cold start vs mapped start); a literal buried
    in the criterion cannot be retuned without editing the criterion."""
    assert re.search(rf'^{param}="\$\{{{param}:-', code[WATCH], re.M), \
        f"{param} is not an overridable named parameter"


def test_the_criterion_is_the_ittc_one_not_an_invented_one(
        code: dict[Path, str]):
    """7.5-03-01-01 §4.1 oscillatory convergence: U_I = 1/2 (S_U - S_L).

    Stop only when the trailing window means' spread is an order below the
    grid-to-grid difference the run exists to measure.
    """
    body = code[WATCH]
    assert "U_I=" in body, "the reported uncertainty U_I is gone"
    assert "spread/2" in body, "U_I must be half the window-mean spread"
    assert re.search(r"spread\s*<=\s*spread_pct", body), "spread test missing"
    assert re.search(r"drift\s*<=\s*drift_pct", body), "drift test missing"
    assert re.search(r'pass.*-ge.*HOLD', body), \
        "the stop must be held for HOLD consecutive checks"


def test_the_cold_start_versus_mapped_start_min_iter_is_recorded(
        sources: dict[Path, str]):
    """10000 (cold) and 6000 (mapFieldsPar-prolonged) were the values used.

    Losing that provenance turns a derived parameter into a magic number.
    """
    body = sources[WATCH]
    assert "10000" in body and "6000" in body, \
        "the two MIN_ITER values actually used are not recorded"
    assert re.search(r"cold start", body, re.IGNORECASE)
    assert re.search(r"mapFieldsPar", body)


def test_the_watcher_reports_the_ittc_reference(sources: dict[Path, str]):
    assert "7.5-03-01-01" in sources[WATCH]


# --------------------------------------------------------------------------- #
# Runaway bounds — an unattended waiter with no bound is the zombie class.
# --------------------------------------------------------------------------- #

def test_the_poller_enforces_a_budget_from_the_registry(
        code: dict[Path, str]):
    """budget_hours is a runaway bound, not a schedule."""
    body = code[POLLER]
    assert re.search(r"cfd_case_get\s+\S+\s+budget_hours", body), \
        "the budget must come from the case registry, not a bash array"


def test_the_mesh_wait_is_bounded(code: dict[Path, str]):
    """An unattended `while ! grep ...; do sleep; done` with no bound is
    exactly the shape that produced a 13.5 h zombie."""
    body = code[AUTO]
    assert re.search(r"WAIT_HOURS|wait_hours|DEADLINE|deadline", body), \
        "auto_solve waits for meshing with no deadline"


def test_the_gate_halts_rather_than_launching_when_cells_are_out_of_bounds(
        code: dict[Path, str]):
    """The mesh density came from a multiplier never run on this hull, so the
    cell count is a prediction. Landing far from target means recalibrate,
    not spend days solving a mesh nobody looked at."""
    body = code[AUTO]
    assert "cfd_mesh_cells" in body, \
        "cell counts must come from the shared reader (empty != 0 cells)"
    # Every HALT branch must precede the launch itself. (The entry point is
    # validated earlier, before the multi-hour wait — failing fast on a
    # missing solve entry is not a gate bypass.)
    launch = body.index("setsid")
    for m in re.finditer(r"HALT", body):
        assert m.start() < launch, "a HALT branch appears after the launch"
    assert re.search(r"exit\s+2", body), \
        "an out-of-bounds mesh must exit non-zero and distinguishably"
