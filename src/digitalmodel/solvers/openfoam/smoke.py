"""Fail-closed MPI smoke planning and semantic completion checks."""

from __future__ import annotations

import hashlib
import math
import re
import subprocess
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Sequence

from .smoke_evidence import EvidenceValidationError, write_evidence


SMOKE_LOAD_THRESHOLD = 1.5
_MAX_LOG_BYTES = 64 * 1024
_NUMBER = r"[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?"
_POINT = re.compile(rf"\(\s*({_NUMBER})\s+({_NUMBER})\s+({_NUMBER})\s*\)")
_TIME = re.compile(rf"^\s*Time\s*=\s*({_NUMBER})\s*$", re.MULTILINE)
_FATAL_MARKERS = ("foam fatal error", "foam fatal io error")
_DIVERGENCE_MARKERS = ("divergence", "maximum number of iterations exceeded", "bounding")


class SmokeError(RuntimeError):
    """Raised when the smoke cannot prove its required semantics."""


@dataclass(frozen=True)
class SmokePlan:
    ranks: int
    visible_cpus: int
    selected_ranks: int
    threshold: float = SMOKE_LOAD_THRESHOLD

    def __post_init__(self) -> None:
        if self.threshold != SMOKE_LOAD_THRESHOLD:
            raise SmokeError("smoke load threshold is fixed at 1.5")

    @property
    def commands(self) -> tuple[tuple[str, ...], ...]:
        return (
            ("setFields",),
            ("decomposePar", "-force"),
            ("mpirun", "-np", str(self.ranks), "interFoam", "-parallel"),
            ("reconstructParMesh", "-latestTime"),
            ("reconstructPar", "-latestTime", "-no-fields", "-no-lagrangian"),
        )

    @property
    def flat_argv(self) -> tuple[str, ...]:
        return tuple(token for command in self.commands for token in command)


@dataclass(frozen=True)
class StageEvidence:
    name: str
    argv: tuple[str, ...]
    return_code: int
    stdout_sha256: str
    stderr_sha256: str
    log_sha256: str
    log_path: str

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "argv": list(self.argv),
            "return_code": self.return_code,
            "stdout_sha256": self.stdout_sha256,
            "stderr_sha256": self.stderr_sha256,
            "log_sha256": self.log_sha256,
            "log_path": self.log_path,
        }


@dataclass(frozen=True)
class MotionCheck:
    max_displacement: float
    max_rotation_error: float
    axis: tuple[float, float, float]
    angle_radians: float
    origin: tuple[float, float, float]
    point_count: int
    initial_points_sha256: str
    reconstructed_points_sha256: str
    tolerance: float


@dataclass(frozen=True)
class SmokeResult:
    plan: SmokePlan
    stages: tuple[StageEvidence, ...]
    final_time: float
    step_count: int
    wall_seconds: float
    motion: MotionCheck
    expected_end_time: float
    time_tolerance: float

    @property
    def status(self) -> str:
        return "completed"

    @property
    def max_displacement(self) -> float:
        return self.motion.max_displacement

    @property
    def max_rotation_error(self) -> float:
        return self.motion.max_rotation_error

    @property
    def seconds_per_step(self) -> float:
        return self.wall_seconds / self.step_count


def plan_smoke(ranks: int, visible_cpus: int, selected_ranks: int) -> SmokePlan:
    """Validate dispatcher binding and return the fixed post-bridge plan."""
    if not isinstance(ranks, int) or isinstance(ranks, bool) or ranks < 1:
        raise SmokeError("ranks must be at least one")
    if not isinstance(visible_cpus, int) or visible_cpus < 1:
        raise SmokeError("visible CPUs must be positive")
    if ranks > visible_cpus:
        raise SmokeError("ranks must not exceed visible CPUs")
    if ranks != selected_ranks:
        raise SmokeError("ranks must equal selected dispatcher ranks")
    return SmokePlan(ranks, visible_cpus, selected_ranks)


def parse_points(path: Path | str) -> tuple[tuple[float, float, float], ...]:
    """Parse OpenFOAM vector points from a serialized points file."""
    source = Path(path)
    try:
        text = source.read_text(encoding="utf-8")
    except OSError as exc:
        raise SmokeError(f"points file cannot be read: {source}") from exc
    matches = _POINT.findall(text)
    if not matches:
        raise SmokeError(f"points file contains no coordinates: {source}")
    declared = re.search(r"\n\s*(\d+)\s*\n\s*\(", text)
    if declared is None or int(declared.group(1)) != len(matches):
        raise SmokeError("points file declared count does not match coordinates")
    return tuple(tuple(float(value) for value in match) for match in matches)


def verify_rigid_rotation(
    initial: Sequence[Sequence[float]],
    reconstructed: Sequence[Sequence[float]],
    *,
    rotation_radians: float,
    length: float,
    origin: tuple[float, float, float] = (0.0, 0.0, 0.0),
    initial_points_sha256: str = "",
    reconstructed_points_sha256: str = "",
) -> MotionCheck:
    """Verify a nonzero prescribed rigid rotation about the frozen z axis."""
    if not math.isfinite(rotation_radians) or abs(rotation_radians) <= 0.0:
        raise SmokeError("prescribed rotation must be nonzero")
    if not math.isfinite(length) or length <= 0.0:
        raise SmokeError("rotation length must be positive")
    if len(initial) != len(reconstructed) or not initial:
        raise SmokeError("initial and reconstructed point counts differ")
    cosine, sine = math.cos(rotation_radians), math.sin(rotation_radians)
    max_displacement = 0.0
    max_error = 0.0
    for before, after in zip(initial, reconstructed):
        expected = _rotate_z(before, cosine, sine, origin)
        displacement = math.sqrt(sum((after[i] - before[i]) ** 2 for i in range(3)))
        error = math.sqrt(sum((after[i] - expected[i]) ** 2 for i in range(3)))
        max_displacement = max(max_displacement, displacement)
        max_error = max(max_error, error)
    if max_displacement <= 1e-8 * length:
        raise SmokeError("prescribed rotation produced no measurable displacement")
    tolerance = 1e-6 * length
    if max_error > tolerance:
        raise SmokeError(f"rotation error {max_error} exceeds tolerance")
    return MotionCheck(
        max_displacement,
        max_error,
        (0.0, 0.0, 1.0),
        rotation_radians,
        origin,
        len(initial),
        initial_points_sha256,
        reconstructed_points_sha256,
        tolerance,
    )


def execute_smoke(
    plan: SmokePlan, case_dir: Path | str, *, end_time: float,
    time_precision: int, length: float, rotation_radians: float,
    origin: tuple[float, float, float] = (0.0, 0.0, 0.0),
    timeout_seconds: int = 7200,
    runner: Callable[..., subprocess.CompletedProcess] = subprocess.run,
) -> SmokeResult:
    """Run the exact plan and require semantic completion before returning."""
    if timeout_seconds <= 0:
        raise SmokeError("timeout_seconds must be positive")
    started = time.monotonic()
    case = Path(case_dir)
    initial_path = case / "constant" / "polyMesh" / "points"
    initial = parse_points(initial_path)
    initial_digest = _sha256_file(initial_path)
    stages: list[StageEvidence] = []
    final_time: float | None = None
    step_count = 0
    for index, argv in enumerate(plan.commands):
        stage, output = _run_stage(case, index, argv, runner, timeout_seconds)
        stages.append(stage)
        _reject_stage_failure(stage, output)
        if index == 2:
            final_time, step_count = _require_final_time(
                output, end_time, time_precision
            )
    if final_time is None:
        raise SmokeError("solver stage did not execute")
    reconstructed_path = _reconstructed_points_path(case)
    reconstructed = parse_points(reconstructed_path)
    motion = verify_rigid_rotation(
        initial,
        reconstructed,
        rotation_radians=rotation_radians,
        length=length,
        origin=origin,
        initial_points_sha256=initial_digest,
        reconstructed_points_sha256=_sha256_file(reconstructed_path),
    )
    return SmokeResult(
        plan,
        tuple(stages),
        final_time,
        step_count,
        time.monotonic() - started,
        motion,
        end_time,
        10 ** (-time_precision),
    )


def write_reduced_evidence(
    path: Path | str,
    result: SmokeResult,
    *,
    bridge_manifest: dict[str, Any],
    artifacts: dict[str, Any],
    source_provenance: dict[str, Any],
    dependency_provenance: dict[str, Any],
    execution_class: str,
    dispatcher: dict[str, Any],
) -> None:
    """Atomically write reduced evidence for a semantically completed smoke."""
    try:
        write_evidence(
            path,
            result,
            bridge_manifest=bridge_manifest,
            artifacts=artifacts,
            source_provenance=source_provenance,
            dependency_provenance=dependency_provenance,
            execution_class=execution_class,
            dispatcher=dispatcher,
        )
    except EvidenceValidationError as exc:
        raise SmokeError(str(exc)) from exc


def _rotate_z(
    point: Sequence[float], cosine: float, sine: float, origin: Sequence[float]
) -> tuple[float, float, float]:
    dx, dy = point[0] - origin[0], point[1] - origin[1]
    return (
        origin[0] + cosine * dx - sine * dy,
        origin[1] + sine * dx + cosine * dy,
        point[2],
    )


def _run_stage(
    case: Path,
    index: int,
    argv: tuple[str, ...],
    runner: Callable[..., subprocess.CompletedProcess],
    timeout_seconds: int,
) -> tuple[StageEvidence, str]:
    try:
        process = runner(
            list(argv),
            cwd=str(case),
            capture_output=True,
            text=True,
            check=False,
            shell=False,
            timeout=timeout_seconds,
        )
    except (OSError, subprocess.TimeoutExpired) as exc:
        raise SmokeError(f"{argv[0]} invocation failed: {exc}") from exc
    stdout, stderr = process.stdout or "", process.stderr or ""
    combined = stdout + stderr
    persisted = combined[-_MAX_LOG_BYTES:]
    log_name = f"log.smoke-{index + 1:02d}-{argv[0]}"
    (case / log_name).write_text(persisted, encoding="utf-8")
    stage = StageEvidence(
        argv[0],
        argv,
        process.returncode,
        _sha256(stdout),
        _sha256(stderr),
        _sha256(persisted),
        log_name,
    )
    return stage, combined


def _reject_stage_failure(stage: StageEvidence, output: str) -> None:
    lowered = output.lower()
    marker = next((item for item in _FATAL_MARKERS if item in lowered), None)
    marker = marker or next((item for item in _DIVERGENCE_MARKERS if item in lowered), None)
    if stage.return_code != 0:
        raise SmokeError(f"{stage.name} returned nonzero exit code {stage.return_code}")
    if marker:
        raise SmokeError(f"{stage.name} output contains failure marker: {marker}")


def _require_final_time(output: str, end_time: float, precision: int) -> tuple[float, int]:
    if precision < 0:
        raise SmokeError("time precision must be nonnegative")
    if not re.search(r"^\s*End\s*$", output, re.MULTILINE):
        raise SmokeError("solver ended before OpenFOAM End marker")
    matches = _TIME.findall(output)
    if not matches:
        raise SmokeError("solver output contains no Time marker")
    final_time = float(matches[-1])
    if abs(final_time - end_time) > 10 ** (-precision):
        raise SmokeError("solver final time is outside timePrecision tolerance")
    return final_time, len(matches)


def _reconstructed_points_path(case: Path) -> Path:
    numeric = sorted(
        (item for item in case.iterdir() if item.is_dir() and _is_time_name(item.name)),
        key=lambda item: float(item.name),
        reverse=True,
    )
    candidates = [item / "polyMesh" / "points" for item in numeric]
    candidates.extend(item / "points" for item in numeric)
    candidates.append(case / "constant" / "polyMesh" / "points")
    for candidate in candidates:
        if candidate.is_file():
            return candidate
    raise SmokeError("reconstructed points file is missing")


def _is_time_name(value: str) -> bool:
    try:
        float(value)
    except ValueError:
        return False
    return True


def _sha256(value: str) -> str:
    return hashlib.sha256(value.encode("utf-8")).hexdigest()


def _sha256_file(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()
