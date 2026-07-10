#!/usr/bin/env python
"""Run the synthetic L-tank Gmsh/OpenFOAM MPI machinery smoke."""

from __future__ import annotations

import argparse
import hashlib
import math
import os
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Any

from digitalmodel.solvers.gmsh_meshing.tank_fixture import (
    TankFixtureSpec,
    build_tank_fixture,
    load_tank_fixture_spec,
)
from digitalmodel.solvers.openfoam.gmsh_bridge import (
    BridgeToolchain,
    GmshBridgeError,
    prepare_gmsh_poly_mesh,
)
from digitalmodel.solvers.openfoam.poly_mesh_contract import DEFAULT_BOUNDARY_CONTRACT
from digitalmodel.solvers.openfoam.prebuilt_mesh import (
    PrebuiltMeshError,
    prepare_prebuilt_execution,
)
from digitalmodel.solvers.openfoam.smoke import (
    SmokeError,
    SmokeResult,
    execute_smoke,
    plan_smoke,
    write_reduced_evidence,
)
from digitalmodel.solvers.openfoam.smoke_evidence import capture_pre_run_artifacts
from digitalmodel.solvers.openfoam.validation.sloshing_3d import (
    Sloshing3DConfig,
    write_sloshing_case,
)


REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_INPUT = REPO_ROOT / "examples/cfd/synthetic_l_tank/input.yml"
DEFAULT_EVIDENCE = REPO_ROOT / "docs/api/cfd/synthetic-l-tank-smoke.json"
OPENFOAM_PACKAGE_VERSION = "2312.260127-2"
OPENMPI_PACKAGE_VERSION = "4.1.6-7ubuntu2"
ASSETUTILITIES_COMMIT = "993f1b5ddc90b56ecf531bedb1b84f5efe096700"
ASSETUTILITIES_ROOT = (REPO_ROOT.parent / "assetutilities").resolve()


def visible_cpu_count() -> int:
    """Return CPUs visible to this process, honoring the scheduler affinity."""
    try:
        return len(os.sched_getaffinity(0))
    except AttributeError:
        return os.cpu_count() or 1


def run_pipeline(
    *,
    ranks: int,
    selected_ranks: int,
    visible_cpus: int,
    work_dir: Path,
    input_yaml: Path,
    evidence: Path,
    cpb: int,
    end_time: float,
    delta_t: float,
    time_precision: int,
    execution_class: str,
    projected_load_per_core: float | None = None,
) -> SmokeResult:
    """Build, convert, snapshot, execute, and attest one synthetic smoke."""
    plan = plan_smoke(ranks, visible_cpus, selected_ranks)
    metrics = _execution_metrics(
        visible_cpus, projected_load_per_core, execution_class
    )
    dispatcher = {
        "ranks": ranks,
        "selected_ranks": selected_ranks,
        "visible_cpus": visible_cpus,
        "load_threshold": plan.threshold,
        "projected_load_per_core": projected_load_per_core,
        **metrics,
    }
    source_before = _source_provenance()
    dependency_before = _dependency_provenance()
    spec, source, case = _prepare_case(
        work_dir, input_yaml, cpb, end_time, delta_t, ranks
    )
    bridge = prepare_gmsh_poly_mesh(case, source, toolchain=_toolchain())
    execution = prepare_prebuilt_execution(case, bridge.manifest_path)
    return _execute_attested_smoke(
        plan, execution, bridge, spec, evidence, end_time, time_precision,
        execution_class, dispatcher, source_before, dependency_before,
    )


def _execute_attested_smoke(
    plan, execution, bridge, spec, evidence, end_time, time_precision,
    execution_class, dispatcher, source_before, dependency_before,
) -> SmokeResult:
    try:
        artifacts = _capture_pre_run_artifacts(execution.case_dir)
        angle = _prescribed_yaw_angle(execution.case_dir, end_time)
        origin = (0.5 * spec.tank.breadth, 0.0, 0.5 * spec.tank.length)
        result = execute_smoke(
            plan,
            execution.case_dir,
            end_time=end_time,
            time_precision=time_precision,
            length=spec.tank.length,
            rotation_radians=angle,
            origin=origin,
        )
        execution.verify_unchanged()
        source_after = _source_provenance()
        dependency_after = _dependency_provenance()
        _require_unchanged(source_before, source_after, "source")
        _require_unchanged(dependency_before, dependency_after, "assetutilities")
        write_reduced_evidence(
            evidence,
            result,
            bridge_manifest=bridge.manifest,
            artifacts=artifacts,
            source_provenance={
                "pre_execution": source_before,
                "post_execution": source_after,
            },
            dependency_provenance={
                "assetutilities": {
                    "pre_execution": dependency_before,
                    "post_execution": dependency_after,
                }
            },
            execution_class=execution_class,
            dispatcher=dispatcher,
        )
        return result
    finally:
        execution.release()


def _prepare_case(
    work_dir: Path,
    input_yaml: Path,
    cpb: int,
    end_time: float,
    delta_t: float,
    ranks: int,
) -> tuple[TankFixtureSpec, Path, Path]:
    work_dir.mkdir(parents=True, exist_ok=True)
    spec = load_tank_fixture_spec(input_yaml)
    run_dir = Path(tempfile.mkdtemp(prefix="synthetic-l-tank.run-", dir=work_dir))
    run_dir.chmod(0o700)
    case = run_dir / "case"
    write_sloshing_case(
        case,
        Sloshing3DConfig(
            cpb=cpb,
            end_time=end_time,
            delta_t=delta_t,
            mesh_mode="prebuilt",
            length=spec.tank.length,
            breadth=spec.tank.breadth,
            height=spec.tank.height,
            decompose_ranks=ranks,
        ),
        boundary_contract=DEFAULT_BOUNDARY_CONTRACT,
    )
    case.mkdir(parents=True, exist_ok=True)
    shutil.copy2(input_yaml, case / "input.yml")
    source = case / "source.msh"
    build_tank_fixture(spec, source)
    return spec, source, case


def _toolchain() -> BridgeToolchain:
    try:
        import gmsh
    except ImportError as exc:
        raise SmokeError("locked CFD environment cannot import gmsh") from exc
    if gmsh.__version__ != "4.15.1":
        raise SmokeError(f"gmsh must equal 4.15.1, received {gmsh.__version__}")
    return BridgeToolchain(
        gmsh.__version__,
        _package_version("openfoam2312-default", OPENFOAM_PACKAGE_VERSION),
        _package_version("openmpi-bin", OPENMPI_PACKAGE_VERSION),
    )


def _package_version(package: str, expected: str) -> str:
    try:
        process = subprocess.run(
            ["dpkg-query", "-W", "-f=${Version}", package],
            capture_output=True,
            text=True,
            check=False,
            shell=False,
            timeout=30,
        )
    except (OSError, subprocess.TimeoutExpired) as exc:
        raise SmokeError(f"cannot attest {package}: {exc}") from exc
    actual = process.stdout.strip()
    if process.returncode != 0 or actual != expected:
        raise SmokeError(f"{package} must equal {expected}, received {actual or 'missing'}")
    return actual


def _capture_pre_run_artifacts(case: Path) -> dict[str, Any]:
    return capture_pre_run_artifacts(REPO_ROOT, case)


def _execution_metrics(
    visible_cpus: int,
    projected_load_per_core: float | None,
    execution_class: str,
) -> dict[str, float]:
    if projected_load_per_core is None and execution_class != "test":
        raise SmokeError("dispatcher projected load/core is required")
    if projected_load_per_core is not None and (
        isinstance(projected_load_per_core, bool)
        or not math.isfinite(projected_load_per_core)
        or projected_load_per_core < 0
        or projected_load_per_core > 1.5
    ):
        raise SmokeError("projected load/core must be between 0 and 1.5")
    try:
        load1 = os.getloadavg()[0]
    except OSError as exc:
        raise SmokeError("one-minute load average is unavailable") from exc
    load_per_core = load1 / visible_cpus
    if not math.isfinite(load_per_core) or load_per_core < 0 or load_per_core > 1.5:
        raise SmokeError("actual load/core exceeds fixed threshold 1.5")
    return {"actual_load1": load1, "actual_load_per_core": load_per_core}


def _source_provenance() -> dict[str, Any]:
    return _repo_provenance(REPO_ROOT)


def _dependency_provenance() -> dict[str, Any]:
    provenance = _repo_provenance(ASSETUTILITIES_ROOT)
    if provenance["commit"] != ASSETUTILITIES_COMMIT:
        raise SmokeError(
            f"assetutilities commit must equal {ASSETUTILITIES_COMMIT}"
        )
    return provenance


def _require_unchanged(before: dict[str, Any], after: dict[str, Any], label: str) -> None:
    if before != after:
        raise SmokeError(f"{label} provenance changed during smoke execution")


def _repo_provenance(repo_root: Path) -> dict[str, Any]:
    status = _git_output(repo_root, "status", "--porcelain", "--untracked-files=all")
    if status:
        raise SmokeError(f"{repo_root.name} checkout must be clean before execution")
    commit = _git_output(repo_root, "rev-parse", "HEAD")
    tracked = _git_output(repo_root, "ls-files", "-s", "-z", binary=True)
    return {
        "clean": True,
        "commit": commit,
        "tracked_sources_sha256": hashlib.sha256(tracked).hexdigest(),
    }


def _git_output(repo_root: Path, *args: str, binary: bool = False) -> Any:
    try:
        process = subprocess.run(
            ["git", *args],
            cwd=repo_root,
            capture_output=True,
            text=not binary,
            check=False,
            shell=False,
            timeout=30,
        )
    except (OSError, subprocess.TimeoutExpired) as exc:
        raise SmokeError(f"git {' '.join(args)} failed to execute") from exc
    if process.returncode != 0:
        raise SmokeError(f"git {' '.join(args)} failed")
    return process.stdout if binary else process.stdout.strip()


def _prescribed_yaw_angle(case: Path, end_time: float) -> float:
    text = (case / "constant" / "dynamicMeshDict").read_text(encoding="utf-8")
    omega_match = re.search(r"\bomega\s+([0-9.eE+-]+);", text)
    amplitude_match = re.search(r"\bamplitude\s+\(0\s+0\s+([0-9.eE+-]+)\);", text)
    if not omega_match or not amplitude_match:
        raise SmokeError("dynamicMeshDict does not describe z-axis yaw")
    omega = float(omega_match.group(1))
    amplitude = math.radians(float(amplitude_match.group(1)))
    angle = amplitude * math.sin(omega * end_time)
    if abs(angle) <= 0.0:
        raise SmokeError("configured yaw is zero at the final time")
    return angle


def _projected_load_from_environment(execution_class: str) -> float | None:
    value = os.environ.get("CFD_DISPATCH_PROJECTED_LOAD_PER_CORE")
    if value is None:
        if execution_class == "test":
            return None
        raise ValueError("CFD_DISPATCH_PROJECTED_LOAD_PER_CORE is required")
    try:
        return float(value)
    except ValueError as exc:
        raise ValueError("CFD_DISPATCH_PROJECTED_LOAD_PER_CORE must be numeric") from exc


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ranks", type=int)
    parser.add_argument("--work-dir", type=Path, default=Path.home() / "cfd_work")
    parser.add_argument("--input-yaml", type=Path, default=DEFAULT_INPUT)
    parser.add_argument("--evidence", type=Path, default=DEFAULT_EVIDENCE)
    parser.add_argument("--cpb", type=int, default=6)
    parser.add_argument("--end-time", type=float, default=0.30)
    parser.add_argument("--delta-t", type=float, default=0.001)
    parser.add_argument("--time-precision", type=int, default=8)
    parser.add_argument("--visible-cpus", type=int)
    parser.add_argument(
        "--execution-class",
        choices=("dedicated", "shared-fallback", "test"),
        default=os.environ.get("CFD_EXECUTION_CLASS"),
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    selected_text = os.environ.get("CFD_DISPATCH_RANKS")
    if selected_text is None:
        print("CFD_DISPATCH_RANKS is required; run through the dispatcher")
        return 2
    try:
        selected = int(selected_text)
    except ValueError:
        print("CFD_DISPATCH_RANKS must be an integer")
        return 2
    if args.ranks is not None and args.ranks != selected:
        print("--ranks must equal CFD_DISPATCH_RANKS")
        return 2
    if args.execution_class is None:
        print("CFD_EXECUTION_CLASS is required; run through the dispatcher")
        return 2
    try:
        projected_load = _projected_load_from_environment(args.execution_class)
    except ValueError as exc:
        print(exc)
        return 2
    ranks = args.ranks or selected
    try:
        run_pipeline(
            ranks=ranks,
            selected_ranks=selected,
            visible_cpus=(
                args.visible_cpus
                if args.visible_cpus is not None
                else visible_cpu_count()
            ),
            work_dir=args.work_dir,
            input_yaml=args.input_yaml,
            evidence=args.evidence,
            cpb=args.cpb,
            end_time=args.end_time,
            delta_t=args.delta_t,
            time_precision=args.time_precision,
            execution_class=args.execution_class,
            projected_load_per_core=projected_load,
        )
    except (GmshBridgeError, OSError, PrebuiltMeshError, SmokeError, ValueError) as exc:
        print(f"synthetic smoke failed: {exc}")
        return 1
    print(f"synthetic smoke completed: ranks={ranks} evidence={args.evidence}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
