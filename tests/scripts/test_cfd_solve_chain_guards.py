"""Guards on the #1173 OpenFOAM ship-resistance solve chain (#2023).

These three scripts — ``chain_stage45.sh`` (mesh), ``solve_chain.sh`` (solve),
``stage45_driver.sh`` (the per-case worker both call) — drive a 60 h unattended
solve on a remote host. They existed for months only as untracked copies on
that host, which is the gap this issue closes. What is asserted here is not
what they compute; it is the set of properties that made the untracked
originals unreproducible or unsafe:

  1. **no host-specific state.** The originals hard-coded ``ROOT="$HOME/..."``,
     the case list as ``for case in kcs_production kcs_companion``, and the
     runaway budgets as a bash associative array. All three must now come from
     ``DM_CFD_ROOT`` and ``config/cfd/kcs_chain.yml``, or a clean checkout on a
     second host reproduces nothing.
  2. **never ``pgrep -f`` a supervised process.** The pattern matches the ssh
     command line carrying it, so a supervisor sees itself and waits forever.
     That produced a 13.5 h zombie on this fleet.
  3. **every ``mpirun`` gets ``< /dev/null``.** mpirun reads and closes stdin;
     without the redirect it swallows the remainder of a piped script. This
     silently skipped a solver launch while the wrapper reported success.
  4. **a terminal marker on failure, not only on success.** A lane that marks
     only success makes silence ambiguous, and in practice silence reads as
     success.
  5. **the OpenFOAM bashrc is never sourced under ``set -e``/``set -u``.** It
     dereferences unset variables and calls ``pop_var_context``; either flag
     aborts the shell. ``cfd_load_openfoam`` saves and restores the flags, so
     the scripts must go through it rather than sourcing the bashrc directly.

Every assertion below is against **comment-stripped** source. These scripts
document the exact defects they guard against, quoting the offending commands
verbatim, so a naive substring search finds the prose and passes while the code
does the wrong thing. Where the property being pinned *is* a comment — a WHY
that records a hard-won failure and must not be dropped in a later tidy-up —
the assertion is against the raw source and says so.
"""
from __future__ import annotations

import re
import shutil
import subprocess
from pathlib import Path

import pytest

SCRIPTS_DIR = Path(__file__).resolve().parents[2] / "scripts" / "cfd"

CHAIN = SCRIPTS_DIR / "chain_stage45.sh"
SOLVE = SCRIPTS_DIR / "solve_chain.sh"
DRIVER = SCRIPTS_DIR / "stage45_driver.sh"

ALL_SCRIPTS = (CHAIN, SOLVE, DRIVER)
ALL_IDS = [p.name for p in ALL_SCRIPTS]


def _source(path: Path) -> str:
    assert path.is_file(), f"chain script not found at {path}"
    return path.read_text()


def _strip_comments(source: str) -> str:
    """Executable lines only.

    See the module docstring: the comments quote the failures they guard
    against, so asserting on raw source tests the documentation rather than
    the guard.
    """
    return "\n".join(ln for ln in source.splitlines()
                     if not ln.lstrip().startswith("#"))


def _join_continuations(code: str) -> str:
    """Fold ``\\``-continued lines into one logical line.

    A redirect may legally sit on the continuation of the command it applies
    to, so a per-line check for ``< /dev/null`` needs the whole statement.
    """
    return re.sub(r"\\\n\s*", " ", code)


@pytest.fixture(scope="module")
def sources() -> dict[str, str]:
    return {p.name: _source(p) for p in ALL_SCRIPTS}


@pytest.fixture(scope="module")
def codes(sources: dict[str, str]) -> dict[str, str]:
    return {name: _strip_comments(src) for name, src in sources.items()}


# --------------------------------------------------------------------------- #
# 0. The scripts exist and parse.
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=ALL_IDS)
def test_script_exists_and_is_executable_bash(script: Path):
    """A missing sibling is the original defect: the one committed script of
    the twelve referenced eleven files that were not in the repo."""
    assert script.is_file(), f"{script.name} was not ported"
    assert script.read_text().startswith("#!"), "no interpreter line"


@pytest.mark.skipif(shutil.which("bash") is None, reason="bash not available")
@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=ALL_IDS)
def test_script_is_syntactically_valid(script: Path):
    """A syntax error in the solve stage surfaces only after meshing has
    already burned hours of the budget."""
    r = subprocess.run(["bash", "-n", str(script)],
                       capture_output=True, text=True)
    assert r.returncode == 0, r.stderr


@pytest.mark.skipif(shutil.which("shellcheck") is None,
                    reason="shellcheck not installed")
@pytest.mark.parametrize("script", ALL_SCRIPTS, ids=ALL_IDS)
def test_shellcheck_is_clean(script: Path):
    """SC1091 is excluded: the sourced library is resolved at runtime from
    the script's own directory, which shellcheck cannot follow statically."""
    r = subprocess.run(["shellcheck", "-e", "SC1091", str(script)],
                       capture_output=True, text=True)
    assert r.returncode == 0, r.stdout + r.stderr


# --------------------------------------------------------------------------- #
# 1. No host-specific state.
# --------------------------------------------------------------------------- #

# ``/dev/null`` is the one absolute path a portable script may name.
_ABS_PATH = re.compile(
    r"(?:^|[\s\"'=:(])(?:~/|\$HOME\b|\$\{HOME[:}]|/home/|/Users/|/mnt/|/opt/"
    r"|/usr/(?!bin/env\b))"
)


@pytest.mark.parametrize("name", ALL_IDS)
def test_no_hardcoded_host_paths_in_executable_lines(codes, name):
    """``ROOT="$HOME/cfd/dm1173"`` is why the originals ran on one account of
    one host. The repo forbids it outright
    (scripts/enforcement/check-no-abs-paths.sh)."""
    offenders = [ln.strip() for ln in codes[name].splitlines()
                 if _ABS_PATH.search(ln)]
    assert not offenders, f"{name} hard-codes a machine path: {offenders}"


@pytest.mark.parametrize("name", ALL_IDS)
def test_case_root_comes_from_the_environment_and_fails_closed(codes, name):
    """``DM_CFD_ROOT`` has no default on purpose: solving into an unintended
    directory is worse than not solving. A script that supplies a fallback
    re-opens exactly the hole this issue closes."""
    code = codes[name]
    assert "cfd_root" in code, f"{name} does not resolve the root via cfd_root"
    assert not re.search(r"DM_CFD_ROOT\s*=", code), \
        f"{name} assigns DM_CFD_ROOT instead of requiring it"
    assert not re.search(r"DM_CFD_ROOT:[-=]", code), \
        f"{name} defaults DM_CFD_ROOT; it must fail closed"


@pytest.mark.parametrize("name", ALL_IDS)
def test_sources_the_shared_chain_library(codes, name):
    """One definition of the guards, not three drifting copies — the host
    copies had already drifted from the repo's before this port."""
    assert "lib/cfd_chain.sh" in codes[name]


@pytest.mark.parametrize("name", ALL_IDS)
def test_case_names_are_never_hardcoded(codes, name):
    """``for case in kcs_production kcs_companion`` cannot express the four
    levels the registry now carries, and silently ignored the two it did not
    name."""
    assert "kcs_" not in codes[name], \
        f"{name} names a specific case; the registry must supply it"


@pytest.mark.parametrize("name", ["chain_stage45.sh", "solve_chain.sh"])
def test_the_case_list_is_read_from_the_registry(codes, name):
    """Both multi-case stages must enumerate from config/cfd/kcs_chain.yml."""
    assert "cfd_cases" in codes[name], \
        f"{name} does not read the case list from the registry"


def test_budgets_are_read_from_the_registry_not_a_bash_array(codes):
    """``declare -A BUDGET=( [kcs_production]=60 ... )`` lived only on the
    solve host, so the runaway bound was unreviewable and unreproducible."""
    code = codes["solve_chain.sh"]
    assert "declare -A" not in code, "budgets are still a host-local array"
    assert "budget_hours" in code and "cfd_case_get" in code, \
        "budget must come from cfd_case_get <case> budget_hours"


# --------------------------------------------------------------------------- #
# 2. Never pgrep -f a supervised process.
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("name", ALL_IDS)
def test_never_matches_a_process_by_full_command_line(codes, name):
    """``pgrep -f "interFoam -parallel"`` matches the ssh command line that
    carries it, and matches the grepping supervisor itself. One waiter ran
    13.5 h past its job this way; ``pkill -f`` with the same pattern killed
    the operator's own session."""
    code = codes[name]
    assert not re.search(r"\bp(?:grep|kill)\s+(?:-\w+\s+)*-\w*f", code), \
        f"{name} matches a process by full command line"


def test_stage1_liveness_uses_the_executable_name_probe(codes):
    """The mesh stage must still notice a dead Stage 1 rather than waiting
    forever — but through cfd_solver_running, which uses ``pgrep -x``."""
    assert "cfd_solver_running" in codes["chain_stage45.sh"]


# --------------------------------------------------------------------------- #
# 3. Every mpirun gets an explicit stdin redirect.
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("name", ALL_IDS)
def test_every_mpirun_closes_its_stdin(codes, name):
    """mpirun reads and closes stdin. Without ``< /dev/null`` it consumes the
    rest of a piped script — which silently skipped a solver launch while the
    wrapper reported success."""
    statements = _join_continuations(codes[name]).splitlines()
    bad = [s.strip() for s in statements
           if re.search(r"\bmpirun\b", s) and "< /dev/null" not in s]
    assert not bad, f"{name}: mpirun without a stdin redirect: {bad}"


def test_the_driver_actually_runs_the_solver_in_parallel(codes):
    """Guards against the above assertion passing vacuously."""
    assert re.search(r"\bmpirun\b", codes["stage45_driver.sh"]), \
        "the driver no longer launches a parallel solver at all"


@pytest.mark.parametrize("name", ALL_IDS)
def test_backgrounded_supervisors_also_close_their_stdin(codes, name):
    """Same failure mode one level up: a detached supervisor inheriting the
    chain's stdin steals the script's remaining lines."""
    statements = _join_continuations(codes[name]).splitlines()
    bad = [s.strip() for s in statements
           if re.search(r"\bsetsid\b", s) and "< /dev/null" not in s]
    assert not bad, f"{name}: detached launch without a stdin redirect: {bad}"


# --------------------------------------------------------------------------- #
# 4. Terminal markers on both outcomes.
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("name", ALL_IDS)
def test_writes_a_terminal_marker_on_failure_as_well_as_success(codes, name):
    """Silence is not evidence. A lane that marks only success cannot be
    distinguished from one still running, and gets read as success."""
    code = codes[name]
    assert "cfd_marker_ok" in code, f"{name} never marks success"
    assert "cfd_marker_fail" in code, f"{name} never marks failure"


@pytest.mark.parametrize("name", ALL_IDS)
def test_an_unexpected_exit_still_lands_on_the_failure_marker(codes, name):
    """Explicit calls cover the paths the author thought of. The trap covers
    the ones that killed the untracked originals — ``set -u`` aborts, an
    unhandled signal, a poller group-kill."""
    assert re.search(r"^\s*trap\s+\S+\s+EXIT", codes[name], re.M), \
        f"{name} has no EXIT trap, so an unexpected death marks nothing"


def test_a_failed_level_stops_the_chain_rather_than_solving_the_next(codes):
    """A level that failed invalidates the grid-convergence ratio the next
    level exists to compute; spending 24 more hours on it is waste."""
    code = codes["solve_chain.sh"]
    tail = code[code.index("cfd_case_get"):]
    assert re.search(r"rc\D*-ne 0", tail), "no non-zero rc branch in the loop"


# --------------------------------------------------------------------------- #
# 5. OpenFOAM is loaded through the tolerant wrapper.
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("name", ALL_IDS)
def test_openfoam_is_never_sourced_directly(codes, name):
    """openfoam2312/etc/bashrc dereferences unset variables and calls
    pop_var_context; under ``set -e`` or ``set -u`` it aborts the shell.
    Both have bitten this chain. cfd_load_openfoam saves and restores the
    caller's flags around the source."""
    code = codes[name]
    assert "etc/bashrc" not in code, \
        f"{name} sources the OpenFOAM bashrc directly"


def test_the_driver_loads_openfoam_before_running_any_foam_utility(codes):
    assert "cfd_load_openfoam" in codes["stage45_driver.sh"]


def test_the_driver_does_not_run_under_errexit(codes):
    """The driver captures each stage's rc to record it in TIMING.csv;
    ``set -e`` would abort at the failing command, before the row is written,
    losing the one artifact that says which stage failed and how long it
    took."""
    assert not re.search(r"^\s*set\s+-\S*e", codes["stage45_driver.sh"], re.M)


# --------------------------------------------------------------------------- #
# 6. Verdicts read output text, not exit codes.
# --------------------------------------------------------------------------- #

def test_mesh_verdict_reads_the_report_not_the_exit_code(codes):
    """checkMesh returns 0 even when it reports failed checks, so an
    exit-code gate certifies a mesh it has just been told is bad."""
    code = codes["stage45_driver.sh"]
    assert "Mesh OK" in code, "the mesh verdict no longer reads log.checkMesh"
    assert "Failed" in code, "failed-check lines are no longer detected"


def test_the_launch_record_is_written_before_the_solver_is_released(codes):
    """A poller reconnecting after a link drop has to find the run without
    having witnessed the launch."""
    code = codes["stage45_driver.sh"]
    assert "detached_run.json" in code
    assert code.index("detached_run.json") < code.rindex("mpirun"), \
        "the launch record is written after the solver starts"


# --------------------------------------------------------------------------- #
# 7. WHY comments survive. Asserted against RAW source by design.
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("name,fragment", [
    # Why the two levels are not solved concurrently.
    ("solve_chain.sh", "efficiency collapsing above 8 ranks"),
    # Why the budget must never be read as a schedule.
    ("solve_chain.sh", "RUNAWAY BOUND"),
    # Why meshing waits for Stage 1 instead of overlapping it.
    ("chain_stage45.sh", "corrupt the per-cell-iteration rate measurement"),
    # Why the mesh verdict ignores the exit code.
    ("stage45_driver.sh", "checkMesh"),
    # Why the launch record precedes the solver.
    ("stage45_driver.sh", "BEFORE the solver is released"),
])
def test_hard_won_rationale_is_preserved(sources, name, fragment):
    """These comments record failures that cost hours of solve time each.
    A later reader who cannot see why the chain is sequential will make it
    concurrent again."""
    assert fragment in sources[name], \
        f"{name} lost the rationale for: {fragment!r}"


def test_setfields_is_gated_on_the_case_actually_being_two_phase(
        codes: dict[str, str]):
    """setFields initialises a volume fraction; a single-phase case has none.

    Running it unconditionally is not a stage that fails, it is a stage that
    should never have run -- and it aborts the mesh phase AFTER the mesh is
    built. Asserted on comment-stripped source, and asserted to be a
    STRUCTURAL test: gating on a case name would be a backdoor, and the same
    reasoning governs the forces density-source guard.
    """
    code = codes["stage45_driver.sh"]
    assert "setFieldsDict" in code, "the gate must test for the dict"
    assert re.search(r"compgen -G .0\.orig/alpha", code), (
        "the gate must test for an alpha field, not a case name")
    # The bare invocation must not appear outside the conditional.
    for line in code.splitlines():
        if "tstage setFields" in line:
            assert line.startswith("    "), (
                f"setFields must be inside the gate, found at top level: {line!r}")


def test_the_solver_is_read_from_the_case_not_hardcoded(codes: dict[str, str]):
    """interFoam was hardcoded -- right for every two-phase case and silently
    wrong for a single-phase one, which failed one second into a solve phase
    that had just spent 35 minutes meshing.

    Asserted on comment-stripped source so the prose explaining the defect
    cannot satisfy the test that guards against it.
    """
    code = codes["stage45_driver.sh"]
    assert re.search(r"awk.*\^application", code), (
        "the solver must be read from the case controlDict")
    assert 'mpirun -np "$RANKS" "$SOLVER"' in code, (
        "the launch must use the solver read from the case")
    assert "interFoam" not in code, (
        "no solver name may remain hardcoded in executable lines")
