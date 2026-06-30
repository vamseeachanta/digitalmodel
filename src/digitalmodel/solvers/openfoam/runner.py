#!/usr/bin/env python3
"""
ABOUTME: Fail-closed OpenFOAM solver runner. The openfoam module otherwise only
*generates* case directories (case_builder); this executes them.

Unlike a single-executable solver (MAPDL), an OpenFOAM run is a *sequence* of
utilities invoked inside the case directory:

    blockMesh            -> system/blockMeshDict        -> log.blockMesh
    [snappyHexMesh]      -> system/snappyHexMeshDict    -> log.snappyHexMesh
    <solver>             -> e.g. interFoam / simpleFoam -> log.<solver>
    [foamToVTK]          -> VTK/ for PyVista/ParaView   -> log.foamToVTK

This mirrors the ANSYS/OrcaWave/AQWA runner contract so the same fixed
``uv run python -m digitalmodel <input>`` lane command can drive a CFD solve:

  - **fail-closed**: if no OpenFOAM is on PATH and a real solve was requested,
    the run falls back to DRY_RUN (the workflow then raises, so the lane can
    never record a false finish);
  - per-utility log capture (``log.<utility>``) for convergence tracking;
  - metadata-only result (status, paths, return codes) — heavy field/VTK data
    stays in the case directory on the host.

Invocation is subprocess-based (no PyFoam hard dependency).
"""

from __future__ import annotations

import shutil
import subprocess
import time
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Optional

# OpenFOAM utilities can return rc=0 yet write a fatal error to their log.
_ERROR_MARKERS = (
    "FOAM FATAL ERROR",
    "FOAM FATAL IO ERROR",
    "Floating point exception",
)
# Divergence markers — present in solver logs when the solution blows up.
_DIVERGENCE_MARKERS = ("bounding", "Maximum number of iterations exceeded")


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
        to_vtk: Run ``foamToVTK`` after the solver for PyVista/ParaView.
        timeout_seconds: Hard wall-clock cap on any single utility.
        dry_run: Skip execution; report DRY_RUN (used for plan/validation).
    """

    solver: Optional[str] = None
    mesh_utility: str = "blockMesh"
    run_snappy: bool = False
    to_vtk: bool = True
    timeout_seconds: int = 7200
    dry_run: bool = False


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

    def __init__(self, config: Optional[OpenFOAMRunConfig] = None) -> None:
        self._config = config or OpenFOAMRunConfig()

    # ------------------------------------------------------------------ #
    #  public API                                                         #
    # ------------------------------------------------------------------ #
    def run(self, case_dir: Path | str) -> OpenFOAMRunResult:
        """Mesh + solve + (optionally) convert. Fail-closed on a missing
        OpenFOAM install or a missing/invalid case directory."""
        case = Path(case_dir)
        result = OpenFOAMRunResult(
            status=OpenFOAMRunStatus.PREPARING,
            case_dir=case,
            solver=self._config.solver,
        )

        problem = self._validate_case(case)
        if problem is not None:
            result.status = OpenFOAMRunStatus.FAILED
            result.error_message = problem
            return result

        solver = self._config.solver or self._read_solver(case)
        result.solver = solver
        if solver is None:
            result.status = OpenFOAMRunStatus.FAILED
            result.error_message = (
                "Could not determine solver: pass config.solver or add "
                "'application' to system/controlDict"
            )
            return result

        if self._config.dry_run:
            result.status = OpenFOAMRunStatus.DRY_RUN
            return result

        if not self._openfoam_available():
            result.status = OpenFOAMRunStatus.DRY_RUN
            result.error_message = (
                "No OpenFOAM installation found on PATH "
                f"('{self._config.mesh_utility}' not found); no results produced"
            )
            return result

        start = time.monotonic()
        # Build the ordered list of (status, utility-argv) stages.
        stages: list[tuple[OpenFOAMRunStatus, list[str]]] = [
            (OpenFOAMRunStatus.MESHING, [self._config.mesh_utility]),
        ]
        if self._config.run_snappy:
            stages.append(
                (OpenFOAMRunStatus.MESHING, ["snappyHexMesh", "-overwrite"])
            )
        stages.append((OpenFOAMRunStatus.RUNNING, [solver]))
        if self._config.to_vtk:
            stages.append((OpenFOAMRunStatus.RUNNING, ["foamToVTK"]))

        for status, argv in stages:
            result.status = status
            stage = self._run_stage(case, argv)
            result.stages.append(stage)
            if not stage.ok:
                result.status = OpenFOAMRunStatus.FAILED
                result.error_message = (
                    f"Stage '{stage.name}' failed: {stage.error_message}"
                )
                result.duration_seconds = time.monotonic() - start
                return result

        result.status = OpenFOAMRunStatus.COMPLETED
        result.duration_seconds = time.monotonic() - start
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

    def _openfoam_available(self) -> bool:
        return shutil.which(self._config.mesh_utility) is not None

    def _run_stage(self, case: Path, argv: list[str]) -> StageResult:
        name = argv[0]
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

        # OpenFOAM utilities write their banner+progress to stdout; persist it.
        try:
            log_file.write_text(proc.stdout or "")
        except OSError:
            stage.log_file = None

        stage.return_code = proc.returncode
        stage.duration_seconds = time.monotonic() - start
        stage.error_message = self._detect_error(name, proc.returncode, proc.stdout)
        return stage

    @staticmethod
    def _detect_error(
        name: str, return_code: int, stdout: Optional[str]
    ) -> Optional[str]:
        if return_code != 0:
            return f"{name} returned non-zero exit code {return_code}"
        text = stdout or ""
        for marker in _ERROR_MARKERS:
            if marker in text:
                return f"{name} log contains '{marker}'"
        return None
