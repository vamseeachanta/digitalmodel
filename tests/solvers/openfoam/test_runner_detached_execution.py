"""Detached execution, declared budgets and the completed stage plan (#1173).

Three properties are under test here, and each corresponds to a way a
multi-day solve can be lost:

* an ATTACHED stage that would exceed its timeout is REFUSED up front, rather
  than truncated mid-way and surfacing as a crash hours in;
* a DETACHED stage must declare a budget and must NOT carry a fictitious
  timeout, because a subprocess timeout does not bound a reparented process;
* the stage plan covers the tutorial's whole Allrun, because without
  ``redistributePar`` a parallel run cannot start at all and the entire cost
  model — which assumes eight ranks — is silently invalid.
"""

from __future__ import annotations

import json
import os
import re
import subprocess
import sys
import time
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.runner import (
    OpenFOAMRunConfig,
    OpenFOAMRunner,
    OpenFOAMRunStatus,
    poll_detached_run,
)


def _make_case(tmp_path: Path, solver: str = "interFoam") -> Path:
    case = tmp_path / "case"
    for sub in ("system", "constant", "0"):
        (case / sub).mkdir(parents=True)
    (case / "system" / "controlDict").write_text(f"application {solver};\n")
    return case


# --------------------------------------------------------------------------- #
#  The attached path — a real safety property
# --------------------------------------------------------------------------- #

def test_runner_refuses_attached_stage_exceeding_timeout(tmp_path) -> None:
    """Refused at preflight, not truncated.

    A stage killed at its timeout reports as a failure indistinguishable from a
    crash, hours after the point where the problem was already knowable.
    """
    case = _make_case(tmp_path)
    runner = OpenFOAMRunner(
        OpenFOAMRunConfig(
            timeout_seconds=7200,
            estimated_wallclock_seconds=26_000.0,
        )
    )
    result = runner.run(case)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "exceeds timeout_seconds" in (result.error_message or "")
    assert not result.stages, "nothing should have been launched"


def test_attached_stage_within_its_timeout_is_not_refused(tmp_path) -> None:
    """The guard must not be vacuous."""
    case = _make_case(tmp_path)
    runner = OpenFOAMRunner(
        OpenFOAMRunConfig(
            timeout_seconds=7200,
            estimated_wallclock_seconds=100.0,
            dry_run=True,
        )
    )
    assert runner.run(case).status is OpenFOAMRunStatus.DRY_RUN


def test_attached_stage_requires_a_timeout(tmp_path) -> None:
    case = _make_case(tmp_path)
    result = OpenFOAMRunner(OpenFOAMRunConfig(timeout_seconds=None)).run(case)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "must set timeout_seconds" in (result.error_message or "")


# --------------------------------------------------------------------------- #
#  The detached path — a configuration-consistency check, named as one
# --------------------------------------------------------------------------- #

def test_detached_stage_requires_declared_budget(tmp_path) -> None:
    case = _make_case(tmp_path)
    result = OpenFOAMRunner(
        OpenFOAMRunConfig(detach=True, timeout_seconds=None)
    ).run(case)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "wallclock_budget_hours" in (result.error_message or "")


def test_detached_stage_rejects_a_fictitious_timeout(tmp_path) -> None:
    """A large timeout on a detached run is a safety property that does not
    exist: once the process is reparented, nothing in-process bounds it."""
    case = _make_case(tmp_path)
    result = OpenFOAMRunner(
        OpenFOAMRunConfig(
            detach=True,
            wallclock_budget_hours=48.0,
            timeout_seconds=300_000,
        )
    ).run(case)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "timeout_seconds=None" in (result.error_message or "")


def test_detached_stage_refuses_an_estimate_over_its_own_budget(tmp_path) -> None:
    case = _make_case(tmp_path)
    result = OpenFOAMRunner(
        OpenFOAMRunConfig(
            detach=True,
            timeout_seconds=None,
            wallclock_budget_hours=2.0,
            estimated_wallclock_seconds=40 * 3600.0,
        )
    ).run(case)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "exceeds the declared budget" in (result.error_message or "")


# --------------------------------------------------------------------------- #
#  The poller — where the fail-closed property actually lives
# --------------------------------------------------------------------------- #

def _launch_sleeper(case: Path, seconds: int, budget_hours: float):
    """Stand in for a detached solver: own session, writes a progress log."""
    log = case / "log.sleeper"
    script = (
        f"import time,sys\n"
        f"for i in range(1,{seconds * 10}):\n"
        f"    print('Time = %d' % i, flush=True)\n"
        f"    time.sleep(0.1)\n"
    )
    with log.open("w") as handle:
        proc = subprocess.Popen(
            [sys.executable, "-c", script],
            cwd=str(case), stdout=handle, stderr=subprocess.STDOUT,
            stdin=subprocess.DEVNULL, start_new_session=True,
        )
    (case / "detached_run.json").write_text(
        json.dumps({
            "pid": proc.pid, "pgid": proc.pid, "argv": ["sleeper"],
            "started_epoch": time.time(),
            "wallclock_budget_hours": budget_hours,
            "log_file": "log.sleeper",
        })
    )
    return proc


def test_poll_reports_progress_without_terminating(tmp_path) -> None:
    case = _make_case(tmp_path)
    proc = _launch_sleeper(case, seconds=30, budget_hours=1.0)
    pid = proc.pid
    try:
        time.sleep(0.6)
        result = poll_detached_run(case)
        assert result.running
        assert not result.terminated
        assert not result.over_budget
        assert result.iterations is not None and result.iterations >= 1
    finally:
        os.killpg(os.getpgid(pid), 9)


def test_poll_terminates_detached_run_past_declared_budget(tmp_path) -> None:
    """The genuine enforcement point, with a real process group."""
    case = _make_case(tmp_path)
    # A budget already exhausted at launch.
    proc = _launch_sleeper(case, seconds=60, budget_hours=1.0 / 3600.0)
    pid = proc.pid
    try:
        time.sleep(1.2)
        result = poll_detached_run(case)
        assert result.over_budget
        assert result.terminated
        assert not result.running
        assert "exceeded the declared budget" in (result.reason or "")

        record = json.loads((case / "detached_run.terminated.json").read_text())
        assert record["pid"] == pid
        assert record["budget_seconds"] == pytest.approx(1.0)

        # Reap the child before asserting it is gone. A killed process that
        # has not been waited on stays a ZOMBIE, and os.kill(pid, 0) succeeds
        # against a zombie — so without this the assertion would be testing
        # the test harness's reaping, not the poller's termination.
        proc.wait(timeout=5)
        assert proc.returncode is not None
    finally:
        try:
            os.killpg(os.getpgid(pid), 9)
        except (ProcessLookupError, PermissionError):
            pass


def test_poll_is_idempotent_and_reconnectable(tmp_path) -> None:
    """The poller must work from a session that never saw the launch — that is
    the whole point of enforcing out of band over a link that flaps."""
    case = _make_case(tmp_path)
    proc = _launch_sleeper(case, seconds=30, budget_hours=1.0)
    pid = proc.pid
    try:
        first = poll_detached_run(case)
        second = poll_detached_run(case)
        assert first.pid == second.pid == pid
        assert second.elapsed_seconds >= first.elapsed_seconds
    finally:
        os.killpg(os.getpgid(pid), 9)


def test_poll_without_a_launch_record_fails_closed(tmp_path) -> None:
    with pytest.raises(FileNotFoundError):
        poll_detached_run(_make_case(tmp_path))


# --------------------------------------------------------------------------- #
#  The stage plan — derived from the tutorial, not hand-written
# --------------------------------------------------------------------------- #

def _emitted_stage_names(**kwargs) -> list[str]:
    runner = OpenFOAMRunner(
        OpenFOAMRunConfig(dtchull_pipeline=True, solver="interFoam", **kwargs)
    )
    return [argv[0] for _status, argv in runner._stage_plan("interFoam", False)]


def test_stage_plan_includes_every_stage_the_short_plan_omitted() -> None:
    names = _emitted_stage_names(ranks=8)
    for required in (
        "surfaceFeatureExtract", "blockMesh", "topoSet", "refineMesh",
        "snappyHexMesh", "restore0Dir", "setFields", "redistributePar",
        "renumberMesh", "interFoam",
    ):
        assert required in names, f"stage plan omits {required}"
    assert names.count("topoSet") == 6
    assert names.count("refineMesh") == 6


def test_parallel_run_decomposes_before_it_solves() -> None:
    """Without redistributePar the ranks each get a serial case. That does not
    slow the run down, it invalidates the schedule it was costed against."""
    names = _emitted_stage_names(ranks=8)
    assert "redistributePar" in names
    assert names.index("redistributePar") < names.index("interFoam")
    assert names.index("renumberMesh") < names.index("interFoam")
    assert names.index("restore0Dir") < names.index("setFields")
    assert names.index("snappyHexMesh") < names.index("restore0Dir")


def test_serial_run_does_not_decompose() -> None:
    names = _emitted_stage_names(ranks=1)
    assert "redistributePar" not in names
    assert "interFoam" in names


def test_parallel_utilities_are_wrapped_in_mpirun() -> None:
    runner = OpenFOAMRunner(OpenFOAMRunConfig(ranks=8))
    wrapped = runner._mpi_wrap(["interFoam"])
    assert wrapped[:3] == ["mpirun", "-np", "8"]
    assert wrapped[-1] == "-parallel"
    # serial utilities are left alone, exactly as the tutorial's Allrun does
    assert runner._mpi_wrap(["blockMesh"]) == ["blockMesh"]
    assert runner._mpi_wrap(["snappyHexMesh", "-overwrite"]) == [
        "snappyHexMesh", "-overwrite"
    ]


def _tutorial_allrun() -> Path | None:
    root = os.environ.get("FOAM_TUTORIALS")
    if not root:
        return None
    cand = (Path(root) / "multiphase" / "interFoam" / "RAS" / "DTCHull"
            / "Allrun")
    return cand if cand.is_file() else None


@pytest.mark.skipif(_tutorial_allrun() is None, reason="FOAM_TUTORIALS unset")
def test_stage_plan_covers_dtchull_allrun() -> None:
    """Asserted against the command sequence PARSED from the shipped Allrun.

    A hand-maintained list is exactly what drifted, so this test does not carry
    one: it reads the tutorial and requires the emitted plan to be a superset
    of what it finds.
    """
    allrun = _tutorial_allrun()
    assert allrun is not None, "guard did not skip; refusing to pass silently"

    # Join shell line-continuations first. The tutorial writes
    #     runApplication -s "$i" \
    #         topoSet -dict system/topoSetDict.${i}
    # so a line-oriented parse captures the backslash instead of the command.
    text = re.sub(r"\\\n\s*", " ", allrun.read_text())
    commands = set()
    for match in re.finditer(
        r"^\s*(?:runApplication|runParallel)(?:\s+-s\s+\S+)?\s+(\S+)",
        text, re.MULTILINE,
    ):
        cmd = match.group(1)
        if cmd.startswith("$"):
            cmd = "interFoam"  # $(getApplication)
        commands.add(cmd)
    # restore0Dir is called as a bare shell function, not via runApplication
    if "restore0Dir" in text:
        commands.add("restore0Dir")

    assert "redistributePar" in commands, "parsed the wrong file"
    emitted = set(_emitted_stage_names(ranks=8))
    missing = commands - emitted
    assert not missing, f"stage plan does not cover the Allrun: {sorted(missing)}"


def test_restore_0_dir_is_implemented_not_shelled_out(tmp_path) -> None:
    """restore0Dir is a SHELL FUNCTION in the tutorials' RunFunctions, not an
    executable. Handing it to subprocess would fail with ENOENT at the exact
    point where the mesh is finished and the solve is about to start.

    The step is not optional: snappyHexMesh consumes 0/ while meshing, so the
    initial fields must come back from 0.orig/ before setFields runs.
    """
    case = _make_case(tmp_path)
    (case / "0.orig").mkdir()
    (case / "0.orig" / "U").write_text("initial U\n")
    (case / "0.orig" / "alpha.water").write_text("initial alpha\n")
    # snappyHexMesh has left 0/ in a state we must not keep
    (case / "0" / "stale").write_text("meshing residue\n")

    stage = OpenFOAMRunner(OpenFOAMRunConfig())._restore_0_dir(case)
    assert stage.ok, stage.error_message
    assert (case / "0" / "U").read_text() == "initial U\n"
    assert (case / "0" / "alpha.water").is_file()
    assert not (case / "0" / "stale").exists(), "meshing residue survived"


def test_restore_0_dir_fails_closed_without_a_source(tmp_path) -> None:
    case = _make_case(tmp_path)
    stage = OpenFOAMRunner(OpenFOAMRunConfig())._restore_0_dir(case)
    assert not stage.ok
    assert "no 0.orig/" in (stage.error_message or "")


# --------------------------------------------------------------------------- #
#  Divergence markers are SOLVER vocabulary (#1173)
# --------------------------------------------------------------------------- #

_REAL_BLOCKMESH_TAIL = """
Creating block mesh topology
  boundingBox: (-26 -19 -16) (16 0 4)
Check topology
    Basic statistics
Writing polyMesh with 0 cellZones
End
"""

_REAL_CHECKMESH_TAIL = """
Mesh stats
    points:           919139
    cells:            845539
Overall domain bounding box (-26 -19 -16) (16 0 4)
Mesh OK.
End
"""

_REAL_SOLVER_DIVERGENCE = """
Time = 42
smoothSolver:  Solving for omega, Initial residual = 1
bounding omega, min: -1.2e+03 max: 5e+05 average: 12
End
"""


def test_mesh_utilities_are_not_scanned_for_divergence_markers() -> None:
    """A successful blockMesh prints 'boundingBox:'. A successful checkMesh
    prints 'Overall domain bounding box'. Both contain the substring
    'bounding', which is a DIVERGENCE marker.

    Scanning mesh logs for it reported every successful mesh as a divergence
    failure. The defect could not fire where it was exercised — CI has no
    OpenFOAM, so every run short-circuits to DRY_RUN before a stage executes —
    so absence of a toolchain read as absence of a problem.

    These are real log excerpts from an actual DTCHull run.
    """
    for name, log in (
        ("blockMesh", _REAL_BLOCKMESH_TAIL),
        ("checkMesh", _REAL_CHECKMESH_TAIL),
    ):
        assert "bounding" in log.lower(), "excerpt no longer exercises the trap"
        assert OpenFOAMRunner._detect_error(
            name, 0, log, check_divergence=False
        ) is None, f"{name} must not be failed by a bounding BOX"
        # and the unscoped form still trips, proving the guard is load-bearing
        assert OpenFOAMRunner._detect_error(
            name, 0, log, check_divergence=True
        ) is not None


def test_solver_divergence_is_still_caught() -> None:
    """The fix must not disarm the check where it means something. In a solver
    log 'bounding' means the solution is being clipped."""
    assert OpenFOAMRunner._detect_error(
        "interFoam", 0, _REAL_SOLVER_DIVERGENCE, check_divergence=True
    ) is not None


def test_fatal_markers_apply_to_every_stage() -> None:
    """A FOAM FATAL is a FOAM FATAL, mesh utility or not."""
    fatal = "--> FOAM FATAL ERROR: cannot find file\n"
    for name in ("blockMesh", "checkMesh", "interFoam"):
        assert OpenFOAMRunner._detect_error(
            name, 0, fatal, check_divergence=False
        ) is not None


def test_nonzero_exit_still_fails_regardless() -> None:
    assert OpenFOAMRunner._detect_error(
        "blockMesh", 1, "fine\n", check_divergence=False
    ) is not None
