#!/usr/bin/env python3
"""Fail-closed subprocess runner for prepared OpenFOAM cases."""

from __future__ import annotations

import json
import os
import shutil
import signal
import subprocess
import time
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Callable, Optional

from .prebuilt_mesh import (
    PrebuiltExecution,
    PrebuiltMeshError,
    prepare_prebuilt_execution,
)

# OpenFOAM utilities can return rc=0 yet write a fatal error to their log.
# NOTE: do NOT add "Floating point exception" here — OpenFOAM's normal startup
# banner prints "trapFpe: Floating point exception trapping enabled (FOAM_SIGFPE)."
# on EVERY successful run, so a substring match false-positives every solve. A
# real FPE *crash* aborts the process with a non-zero return code, which the
# return_code != 0 check below already catches.
_ERROR_MARKERS = (
    "FOAM FATAL ERROR",
    "FOAM FATAL IO ERROR",
)
# Divergence markers — present in solver logs when the solution blows up.
# Kept in lockstep with smoke.py:_DIVERGENCE_MARKERS — one fail-closed
# divergence policy, mirrored on the attested-runner path. Lowercase because
# _detect_error matches case-insensitively, exactly as smoke.py does.
_DIVERGENCE_MARKERS = (
    "divergence",
    "maximum number of iterations exceeded",
    "bounding",
)


class OpenFOAMRunStatus(str, Enum):
    PENDING = "pending"
    PREPARING = "preparing"
    MESHING = "meshing"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    DRY_RUN = "dry_run"


@dataclass
class OpenFOAMRunConfig:
    """Execution configuration for an OpenFOAM case run.

    Attributes:
        solver: Solver application to run (e.g. ``interFoam``, ``simpleFoam``).
            Read from the case's ``controlDict`` if left ``None``.
        mesh_utility: Mesh generator to run first (``blockMesh`` by default).
        run_snappy: Run ``snappyHexMesh -overwrite`` after blockMesh (3D from STL).
        merge_meshes_source: Merge another case's mesh into this one right
            after meshing (``mergeMeshes . <source> -overwrite``), *before*
            topoSet — overset cases combine a background and a component
            mesh this way (e.g. the wave-excited floating body). The path
            is resolved relative to the case directory.
        run_topo_set: Run ``topoSet`` after meshing — cases that carve a body
            out of the background mesh (e.g. the floating-body decay case).
        subset_mesh_set: Cell set for ``subsetMesh -overwrite <set> -patch
            <subset_mesh_patch>`` after topoSet; both must be given together.
        subset_mesh_patch: Patch that receives the exposed subset faces.
        run_set_fields: Run ``setFields`` after meshing, before the solver —
            required by VOF/multiphase cases that initialise a phase region
            from ``system/setFieldsDict`` (e.g. the dam-break water column).
        run_solver: Run the solver after the mesh stages. ``False`` turns
            the run into mesh-prep only (used for overset component
            sub-cases whose mesh is merged into another case); ``to_vtk``
            is ignored when the solver is skipped.
        to_vtk: Run ``foamToVTK`` after the solver for PyVista/ParaView.
        timeout_seconds: Hard wall-clock cap on any single utility. Genuinely
            bounds ATTACHED stages, because the parent holds the child. Must be
            ``None`` on a detached stage, where it bounds nothing — see
            ``detach``.
        estimated_wallclock_seconds: Expected duration of the longest stage. On
            the attached path this is compared against ``timeout_seconds`` at
            preflight so an over-budget stage is REFUSED up front rather than
            truncated mid-way and surfacing as a crash.
        ranks: MPI ranks. ``> 1`` runs the solver and the redistribution stages
            under ``mpirun``, and requires ``system/decomposeParDict`` to
            declare the same ``numberOfSubdomains``.
        detach: Launch the solver with ``setsid nohup`` and return immediately.
            Nothing in-process bounds a detached run, so the budget is enforced
            out of band by the poller — see ``wallclock_budget_hours``.
        wallclock_budget_hours: Declared budget for a detached stage. Required
            when ``detach`` is set. This is where the fail-closed property
            actually lives: the poller compares elapsed time against it and
            terminates the process group when it is exceeded.
        dry_run: Skip execution; report DRY_RUN (used for plan/validation).
        dtchull_pipeline: Emit the full DTCHull ``Allrun`` stage sequence
            (feature extraction, the six topoSet/refineMesh pairs, layer
            addition, field restoration, redistribution and renumbering)
            instead of the short generic sequence. Without the redistribution
            stage a parallel run cannot start at all.
    """

    solver: Optional[str] = None
    mesh_utility: str = "blockMesh"
    run_snappy: bool = False
    merge_meshes_source: Optional[str] = None
    run_topo_set: bool = False
    subset_mesh_set: Optional[str] = None
    subset_mesh_patch: Optional[str] = None
    run_set_fields: bool = False
    run_solver: bool = True
    to_vtk: bool = True
    timeout_seconds: Optional[int] = 7200
    estimated_wallclock_seconds: Optional[float] = None
    ranks: int = 1
    detach: bool = False
    wallclock_budget_hours: Optional[float] = None
    dry_run: bool = False
    dtchull_pipeline: bool = False


@dataclass
class StageResult:
    """Outcome of a single utility invocation (one stage of the run)."""

    name: str
    return_code: Optional[int] = None
    log_file: Optional[Path] = None
    duration_seconds: float = 0.0
    error_message: Optional[str] = None

    @property
    def ok(self) -> bool:
        return self.return_code == 0 and self.error_message is None


@dataclass
class OpenFOAMRunResult:
    """Metadata-only result of an OpenFOAM case run."""

    status: OpenFOAMRunStatus
    case_dir: Path
    solver: Optional[str] = None
    stages: list[StageResult] = field(default_factory=list)
    error_message: Optional[str] = None
    duration_seconds: float = 0.0

    @property
    def vtk_dir(self) -> Optional[Path]:
        vtk = self.case_dir / "VTK"
        return vtk if vtk.is_dir() else None


class OpenFOAMRunner:
    """Run a prepared OpenFOAM case directory, fail-closed.

    The case directory must already contain ``system/``, ``constant/`` and
    ``0/`` (produced by :class:`OpenFOAMCaseBuilder`). This class does not
    author any dict files — it only executes utilities inside the case.
    """

    def __init__(self, config: Optional[OpenFOAMRunConfig] = None,
                 executable_verifier: Optional[Callable[[str], Path]] = None) -> None:
        self._config = config or OpenFOAMRunConfig()
        self._executable_verifier = executable_verifier

    # ------------------------------------------------------------------ #
    #  public API                                                         #
    # ------------------------------------------------------------------ #
    def run(
        self,
        case_dir: Path | str,
        *,
        prebuilt_manifest: Path | str | None = None,
    ) -> OpenFOAMRunResult:
        """Mesh + solve + (optionally) convert. Fail-closed on a missing
        OpenFOAM install or a missing/invalid case directory."""
        case = Path(case_dir)
        result = OpenFOAMRunResult(
            status=OpenFOAMRunStatus.PREPARING,
            case_dir=case,
            solver=self._config.solver,
        )
        using_prebuilt = prebuilt_manifest is not None
        preflight = self._preflight(case, result, using_prebuilt)
        if preflight is None:
            return result
        solver, stages = preflight

        execution: PrebuiltExecution | None = None
        start = time.monotonic()
        try:
            execution_case = case
            if prebuilt_manifest is not None:
                execution = prepare_prebuilt_execution(
                    case,
                    prebuilt_manifest,
                    timeout_seconds=self._config.timeout_seconds,
                )
                execution_case = execution.case_dir
                result.case_dir = execution_case
                if self._read_solver(execution_case) != solver:
                    raise PrebuiltMeshError(
                        "attested snapshot solver does not match preflight selection"
                    )
            self._execute_stages(result, execution_case, stages, start)
            if execution is not None:
                execution.verify_unchanged()
        except PrebuiltMeshError as exc:
            self._fail(result, str(exc))
            result.duration_seconds = time.monotonic() - start
        finally:
            if execution is not None:
                execution.release()
        return result

    def _preflight(
        self,
        case: Path,
        result: OpenFOAMRunResult,
        using_prebuilt: bool,
    ) -> tuple[str, list[tuple[OpenFOAMRunStatus, list[str]]]] | None:
        problem = self._validate_case(case)
        if problem is not None:
            self._fail(result, problem)
            return None
        # Configuration consistency is checked FIRST, before the dry-run and
        # toolchain-availability short circuits below.
        #
        # Ordering is load-bearing. If this ran after the availability check, a
        # detached stage with a fictitious timeout or a missing budget would
        # report DRY_RUN on any host without OpenFOAM — every CI box — and the
        # contradiction would only surface on the one host that can actually
        # run it, at the moment the multi-day solve was supposed to start. A
        # configuration error is an error everywhere.
        try:
            self._execution_preflight()
        except ValueError as exc:
            self._fail(result, str(exc))
            return None
        solver = self._config.solver or self._read_solver(case)
        result.solver = solver
        if solver is None:
            self._fail(
                result,
                "Could not determine solver: pass config.solver or add "
                "'application' to system/controlDict",
            )
            return None
        if using_prebuilt and solver != "interFoam":
            self._fail(result, "prebuilt mesh execution requires interFoam")
            return None
        if self._config.dry_run:
            result.status = OpenFOAMRunStatus.DRY_RUN
            return None
        required = self._required_executable(solver, using_prebuilt)
        if required is not None and not self._openfoam_available(required):
            result.status = OpenFOAMRunStatus.DRY_RUN
            result.error_message = (
                "No OpenFOAM installation found on PATH "
                f"('{required}' not found); no results produced"
            )
            return None
        try:
            stages = self._stage_plan(solver, using_prebuilt)
        except ValueError as exc:
            self._fail(result, str(exc))
            return None
        return solver, stages

    def _execution_preflight(self) -> None:
        """Two different checks, because the two execution paths differ in kind.

        This split exists because demanding a fail-closed wall-clock preflight
        AND a detached run is a contradiction as usually written. Once a
        command is wrapped in ``setsid nohup`` the parent returns immediately
        and ``timeout_seconds`` no longer bounds the solve at all; raising it to
        some large number makes the check trivially satisfiable and protects
        nothing. So:

        * ATTACHED — ``timeout_seconds`` genuinely bounds the work, and the
          preflight is a REAL safety property: refuse to start a stage whose
          estimated wall-clock exceeds it, rather than truncating it mid-way
          and reporting the truncation as a crash.
        * DETACHED — nothing in-process bounds the work. The preflight is a
          CONFIGURATION-CONSISTENCY CHECK and is named as one. It asserts a
          budget is declared and that ``timeout_seconds`` is ``None`` rather
          than a fictitious large number. It does not claim to stop a runaway;
          the poller does that.
        """
        config = self._config
        if config.ranks < 1:
            raise ValueError(f"ranks must be >= 1, got {config.ranks}")

        if config.detach:
            if config.wallclock_budget_hours is None:
                raise ValueError(
                    "a detached stage must declare wallclock_budget_hours; "
                    "nothing in-process bounds it, so the budget is the only "
                    "thing the poller can enforce"
                )
            if config.wallclock_budget_hours <= 0:
                raise ValueError(
                    "wallclock_budget_hours must be positive, got "
                    f"{config.wallclock_budget_hours}"
                )
            if config.timeout_seconds is not None:
                raise ValueError(
                    "a detached stage must set timeout_seconds=None. A "
                    "subprocess timeout does not bound a process that has "
                    "already been reparented, and a large value here is a "
                    "safety property that does not exist."
                )
            estimate = config.estimated_wallclock_seconds
            if estimate is not None:
                budget = config.wallclock_budget_hours * 3600.0
                if estimate > budget:
                    raise ValueError(
                        f"estimated wall-clock {estimate / 3600.0:.1f} h "
                        f"exceeds the declared budget "
                        f"{config.wallclock_budget_hours:.1f} h"
                    )
            return

        # Attached path.
        if config.timeout_seconds is None:
            raise ValueError(
                "an attached stage must set timeout_seconds; it is the only "
                "thing bounding the child process"
            )
        estimate = config.estimated_wallclock_seconds
        if estimate is not None and estimate > config.timeout_seconds:
            raise ValueError(
                f"stage estimated at {estimate:.0f} s exceeds timeout_seconds "
                f"({config.timeout_seconds} s). Refusing to start: an "
                f"over-budget stage truncated mid-way surfaces as a crash "
                f"hours in, which is strictly worse than refusing now. Raise "
                f"timeout_seconds deliberately, or run detached with a "
                f"declared budget."
            )

    def _stage_plan(
        self, solver: str, using_prebuilt: bool
    ) -> list[tuple[OpenFOAMRunStatus, list[str]]]:
        if using_prebuilt and any(
            (
                self._config.run_snappy,
                self._config.merge_meshes_source,
                self._config.run_topo_set,
                self._config.subset_mesh_set,
                self._config.subset_mesh_patch,
            )
        ):
            raise ValueError("prebuilt mesh cannot use mesh-modifying stages")
        if self._config.dtchull_pipeline:
            return self._dtchull_stage_plan(solver)
        stages: list[tuple[OpenFOAMRunStatus, list[str]]] = []
        if not using_prebuilt:
            stages.append((OpenFOAMRunStatus.MESHING, [self._config.mesh_utility]))
        if self._config.run_snappy:
            stages.append(
                (OpenFOAMRunStatus.MESHING, ["snappyHexMesh", "-overwrite"])
            )
        if self._config.merge_meshes_source:
            stages.append((
                OpenFOAMRunStatus.MESHING,
                ["mergeMeshes", ".", self._config.merge_meshes_source,
                 "-overwrite"],
            ))
        if self._config.run_topo_set:
            stages.append((OpenFOAMRunStatus.MESHING, ["topoSet"]))
        if self._config.subset_mesh_set or self._config.subset_mesh_patch:
            if not (self._config.subset_mesh_set and self._config.subset_mesh_patch):
                raise ValueError(
                    "subset_mesh_set and subset_mesh_patch must be given together"
                )
            stages.append((
                OpenFOAMRunStatus.MESHING,
                ["subsetMesh", "-overwrite", self._config.subset_mesh_set,
                 "-patch", self._config.subset_mesh_patch],
            ))
        if self._config.run_set_fields:
            stages.append((OpenFOAMRunStatus.MESHING, ["setFields"]))
        if self._config.run_solver:
            stages.append((OpenFOAMRunStatus.RUNNING, [solver]))
            if self._config.to_vtk:
                stages.append((OpenFOAMRunStatus.RUNNING, ["foamToVTK"]))
        return stages

    def _dtchull_stage_plan(
        self, solver: str
    ) -> list[tuple[OpenFOAMRunStatus, list[str]]]:
        """The full DTCHull ``Allrun`` sequence, in order.

        The short generic plan — blockMesh / snappyHexMesh / topoSet /
        setFields / solver — is not merely incomplete for this case, it cannot
        run at all on more than one rank. ``redistributePar -decompose`` is
        what creates the ``processor*`` directories; without it ``mpirun``
        starts N copies of a serial case. Every hour of the cost model assumes
        eight ranks, so the omission does not slow the run down, it silently
        invalidates the schedule it was costed against.

        Also required and absent from the short plan: ``surfaceFeatureExtract``
        (snappy's feature refinement reads its output), the SIX
        ``topoSet``/``refineMesh`` pairs that build the free-surface refinement,
        ``restore0Dir`` (snappy consumes ``0/``, so the initial fields must be
        restored from ``0.orig/`` before ``setFields``), and ``renumberMesh``.

        ``test_stage_plan_covers_dtchull_allrun`` asserts this list against the
        command sequence PARSED from the shipped tutorial's Allrun rather than
        against a hand-written expectation, because a hand-written list is
        exactly what drifted.
        """
        mesh = OpenFOAMRunStatus.MESHING
        stages: list[tuple[OpenFOAMRunStatus, list[str]]] = [
            (mesh, ["surfaceFeatureExtract"]),
            (mesh, ["blockMesh"]),
        ]
        for i in range(1, 7):
            stages.append((mesh, ["topoSet", "-dict", f"system/topoSetDict.{i}"]))
            stages.append(
                (mesh, ["refineMesh", "-dict", "system/refineMeshDict",
                        "-overwrite"])
            )
        stages.append((mesh, ["snappyHexMesh", "-overwrite"]))
        stages.append((mesh, ["checkMesh"]))
        stages.append((mesh, ["restore0Dir"]))
        stages.append((mesh, ["setFields"]))
        if self._config.ranks > 1:
            stages.append((mesh, ["redistributePar", "-decompose"]))
            stages.append((mesh, ["renumberMesh", "-overwrite"]))
        if self._config.run_solver:
            stages.append((OpenFOAMRunStatus.RUNNING, [solver]))
            if self._config.ranks > 1:
                stages.append(
                    (OpenFOAMRunStatus.RUNNING,
                     ["redistributePar", "-reconstruct"])
                )
        return stages

    #: Utilities that must run under mpirun when ranks > 1. Everything else in
    #: the pipeline is serial in the tutorial's own Allrun and stays serial.
    _PARALLEL_UTILITIES = frozenset(
        {"redistributePar", "renumberMesh", "interFoam", "overInterDyMFoam"}
    )

    def _mpi_wrap(self, argv: list[str]) -> list[str]:
        """Wrap a parallel utility in ``mpirun``, adding ``-parallel``."""
        if self._config.ranks <= 1:
            return argv
        if Path(argv[0]).name not in self._PARALLEL_UTILITIES:
            return argv
        return (
            ["mpirun", "-np", str(self._config.ranks)]
            + argv
            + ["-parallel"]
        )

    def _execute_stages(
        self,
        result: OpenFOAMRunResult,
        case: Path,
        stages: list[tuple[OpenFOAMRunStatus, list[str]]],
        start: float,
    ) -> None:
        for status, argv in stages:
            launch_argv = list(argv)
            if self._executable_verifier is not None:
                launch_argv[0] = str(self._executable_verifier(argv[0]))
            launch_argv = self._mpi_wrap(launch_argv)
            result.status = status
            detached = (
                self._config.detach
                and status is OpenFOAMRunStatus.RUNNING
                and Path(argv[0]).name == (result.solver or "")
            )
            if detached:
                stage = self._launch_detached(case, launch_argv)
                result.stages.append(stage)
                result.status = OpenFOAMRunStatus.RUNNING
                result.duration_seconds = time.monotonic() - start
                return
            # Divergence markers are solver vocabulary; see _detect_error.
            stage = self._run_stage(
                case,
                launch_argv,
                check_divergence=(status is OpenFOAMRunStatus.RUNNING),
            )
            if self._executable_verifier is not None:
                self._executable_verifier(argv[0])
            result.stages.append(stage)
            if not stage.ok:
                result.status = OpenFOAMRunStatus.FAILED
                result.error_message = (
                    f"Stage '{stage.name}' failed: {stage.error_message}"
                )
                result.duration_seconds = time.monotonic() - start
                return

        result.status = OpenFOAMRunStatus.COMPLETED
        result.duration_seconds = time.monotonic() - start

    @staticmethod
    def _restore_0_dir(case: Path) -> StageResult:
        """Restore ``0/`` from ``0.orig/`` — implemented, not shelled out to.

        ``restore0Dir`` is a SHELL FUNCTION defined in the tutorials'
        ``RunFunctions``, not an executable on PATH. Emitting it as a stage
        name and handing it to ``subprocess`` would fail with ENOENT at the
        exact point in the pipeline where the mesh is finished and the solve is
        about to start — after all the meshing work, before any of it is used.

        The step itself is not optional: ``snappyHexMesh`` consumes ``0/``
        while meshing, so the initial fields have to come back from
        ``0.orig/`` before ``setFields`` runs.
        """
        stage = StageResult(name="restore0Dir")
        start = time.monotonic()
        source = case / "0.orig"
        target = case / "0"
        if not source.is_dir():
            stage.return_code = 1
            stage.error_message = f"restore0Dir: no 0.orig/ in {case}"
            return stage
        try:
            if target.is_dir():
                shutil.rmtree(target)
            shutil.copytree(source, target)
        except OSError as exc:
            stage.return_code = 1
            stage.error_message = f"restore0Dir failed: {exc}"
            return stage
        stage.return_code = 0
        stage.duration_seconds = time.monotonic() - start
        return stage

    def _launch_detached(self, case: Path, argv: list[str]) -> StageResult:
        """Start the solver in its own session and return immediately.

        ``setsid`` is the load-bearing part. It puts the solver in a NEW
        process group with no controlling terminal, so a dropped SSH session
        cannot deliver SIGHUP to it. The execution host is reached over a link
        with a known flap history and the production solve runs for days; a run
        that dies with the session is a run that never finishes.

        The launch record is written before the process is released so that a
        poller reconnecting after a disconnect can find the process group
        without having witnessed the launch.
        """
        name = Path(argv[0]).name
        log_file = case / f"log.{name}"
        record = case / "detached_run.json"
        stage = StageResult(name=name, log_file=log_file)

        started = time.time()
        with log_file.open("w") as handle:
            proc = subprocess.Popen(  # noqa: S603
                argv,
                cwd=str(case),
                stdout=handle,
                stderr=subprocess.STDOUT,
                stdin=subprocess.DEVNULL,
                start_new_session=True,
            )
        record.write_text(
            json.dumps(
                {
                    "pid": proc.pid,
                    "pgid": proc.pid,
                    "argv": argv,
                    "started_epoch": started,
                    "wallclock_budget_hours": self._config.wallclock_budget_hours,
                    "log_file": log_file.name,
                },
                indent=2,
            )
        )
        stage.return_code = 0
        stage.duration_seconds = 0.0
        return stage

    def _required_executable(self, solver: str, using_prebuilt: bool) -> str | None:
        if not using_prebuilt:
            return self._config.mesh_utility
        if self._config.run_set_fields:
            return "setFields"
        return solver if self._config.run_solver else None

    @staticmethod
    def _fail(result: OpenFOAMRunResult, message: str) -> OpenFOAMRunResult:
        result.status = OpenFOAMRunStatus.FAILED
        result.error_message = message
        return result

    # ------------------------------------------------------------------ #
    #  internals                                                          #
    # ------------------------------------------------------------------ #
    @staticmethod
    def _validate_case(case: Path) -> Optional[str]:
        if not case.is_dir():
            return f"Case directory not found: {case}"
        for sub in ("system", "constant", "0"):
            if not (case / sub).is_dir():
                return f"Case directory missing required '{sub}/': {case}"
        if not (case / "system" / "controlDict").is_file():
            return f"Case missing system/controlDict: {case}"
        return None

    @staticmethod
    def _read_solver(case: Path) -> Optional[str]:
        """Read the ``application`` keyword from system/controlDict."""
        control = case / "system" / "controlDict"
        try:
            for raw in control.read_text(errors="replace").splitlines():
                line = raw.split("//", 1)[0].strip().rstrip(";").strip()
                if line.startswith("application"):
                    parts = line.split()
                    if len(parts) >= 2:
                        return parts[1]
        except OSError:
            return None
        return None

    @staticmethod
    def _openfoam_available(executable: str) -> bool:
        return shutil.which(executable) is not None

    def _run_stage(
        self, case: Path, argv: list[str], *, check_divergence: bool = True
    ) -> StageResult:
        name = Path(argv[0]).name
        if name == "restore0Dir":
            return self._restore_0_dir(case)
        log_file = case / f"log.{name}"
        stage = StageResult(name=name, log_file=log_file)
        start = time.monotonic()
        try:
            proc = subprocess.run(  # noqa: S603 - argv is a fixed utility name.
                argv,
                cwd=str(case),
                capture_output=True,
                text=True,
                timeout=self._config.timeout_seconds,
                check=False,
            )
        except (OSError, subprocess.TimeoutExpired) as exc:
            stage.duration_seconds = time.monotonic() - start
            stage.error_message = f"{name} invocation failed: {exc}"
            return stage

        # OpenFOAM utilities write their banner+progress to stdout, but a FOAM
        # FATAL can land on stderr at rc=0. Persist and inspect BOTH, or the
        # stage log is not the whole record and the verdict is not honest.
        combined = (proc.stdout or "") + (proc.stderr or "")
        try:
            log_file.write_text(combined)
        except OSError:
            stage.log_file = None

        stage.return_code = proc.returncode
        stage.duration_seconds = time.monotonic() - start
        stage.error_message = self._detect_error(
            name, proc.returncode, combined, check_divergence=check_divergence
        )
        return stage

    @staticmethod
    def _detect_error(
        name: str,
        return_code: int,
        output: Optional[str],
        *,
        check_divergence: bool = True,
    ) -> Optional[str]:
        """Classify a stage's outcome from its return code and its log.

        ``check_divergence`` MUST be False for anything that is not the solver.

        The divergence markers are solver vocabulary. In a solver log
        "bounding" means ``bounding k``, ``bounding omega``, ``bounding
        alpha.water`` — the solution is being clipped, which is a real signal.
        In a MESH utility log it means ``boundingBox:``, which every successful
        blockMesh, snappyHexMesh and checkMesh prints.

        Scanning mesh logs for it therefore reported every successful mesh as a
        divergence failure. The defect was invisible because it cannot fire
        where it is exercised: CI has no OpenFOAM, so every run short-circuits
        to DRY_RUN before a stage executes, and the only hosts that would have
        caught it are the ones nothing routinely runs the attested path on.
        Absence of a toolchain read as absence of a problem.
        """
        if return_code != 0:
            return f"{name} returned non-zero exit code {return_code}"
        lowered = (output or "").lower()
        # Fatal markers apply to every stage — a FOAM FATAL is a FOAM FATAL.
        for marker in _ERROR_MARKERS:
            if marker.lower() in lowered:
                return f"{name} log contains '{marker}'"
        if not check_divergence:
            return None
        for marker in _DIVERGENCE_MARKERS:
            if marker in lowered:
                return f"{name} log contains '{marker}'"
        return None


# --------------------------------------------------------------------------- #
#  The poller — the real enforcement point for a detached run
# --------------------------------------------------------------------------- #

@dataclass
class PollResult:
    """One observation of a detached run."""

    running: bool
    pid: int
    elapsed_seconds: float
    budget_seconds: float
    iterations: Optional[int] = None
    terminated: bool = False
    reason: Optional[str] = None

    @property
    def over_budget(self) -> bool:
        return self.elapsed_seconds > self.budget_seconds


def poll_detached_run(
    case_dir: Path | str, *, terminate_over_budget: bool = True
) -> PollResult:
    """Check a detached run, and terminate it if it has exceeded its budget.

    THIS is where the fail-closed property lives. A subprocess timeout cannot
    bound a process that has been reparented into its own session, so the
    budget is enforced out of band: every poll compares elapsed wall time
    against the budget declared at launch and kills the process GROUP when it
    is exceeded, writing a termination record.

    The honest limitation, stated rather than glossed: enforcement only happens
    while something is polling. If the link is down the budget is unenforced
    for as long as the poller is absent. That is a weaker property than an
    in-process bound would be if an in-process bound were possible here — it is
    not — and it is strictly stronger than the subprocess timeout it replaces,
    which bounds nothing at all on this path.

    The call is short, idempotent and re-connectable, so it can be driven from
    a cron entry or a shell loop on the host itself rather than from the
    session that launched the run.
    """
    case = Path(case_dir)
    record_path = case / "detached_run.json"
    if not record_path.is_file():
        raise FileNotFoundError(f"no detached_run.json in {case}")
    record = json.loads(record_path.read_text())

    pid = int(record["pid"])
    budget_seconds = float(record["wallclock_budget_hours"]) * 3600.0
    elapsed = time.time() - float(record["started_epoch"])
    running = _process_alive(pid)

    iterations = None
    log_file = case / str(record.get("log_file", ""))
    if log_file.is_file():
        iterations = _last_iteration(log_file)

    result = PollResult(
        running=running,
        pid=pid,
        elapsed_seconds=elapsed,
        budget_seconds=budget_seconds,
        iterations=iterations,
    )

    if running and result.over_budget and terminate_over_budget:
        _terminate_group(pid)
        result.terminated = True
        result.running = False
        result.reason = (
            f"elapsed {elapsed / 3600.0:.2f} h exceeded the declared budget "
            f"{budget_seconds / 3600.0:.2f} h"
        )
        (case / "detached_run.terminated.json").write_text(
            json.dumps(
                {
                    "pid": pid,
                    "elapsed_seconds": elapsed,
                    "budget_seconds": budget_seconds,
                    "iterations": iterations,
                    "reason": result.reason,
                    "terminated_epoch": time.time(),
                },
                indent=2,
            )
        )
    return result


def _process_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def _terminate_group(pid: int) -> None:
    """Kill the whole process GROUP, not just the launched process.

    The launched process is ``mpirun``; the ranks doing the work are its
    children. Signalling only the parent can leave eight solver processes
    running with nothing supervising them, which is a worse state than the one
    being corrected.
    """
    for sig in (signal.SIGTERM, signal.SIGKILL):
        try:
            os.killpg(os.getpgid(pid), sig)
        except (ProcessLookupError, PermissionError):
            return
        for _ in range(20):
            if not _process_alive(pid):
                return
            time.sleep(0.25)


def _last_iteration(log_file: Path) -> Optional[int]:
    """Last ``Time = N`` in a solver log — progress without parsing the world."""
    last = None
    try:
        for raw in log_file.read_text(errors="replace").splitlines():
            if raw.startswith("Time = "):
                try:
                    last = int(float(raw.split("=", 1)[1].strip()))
                except ValueError:
                    continue
    except OSError:
        return None
    return last
