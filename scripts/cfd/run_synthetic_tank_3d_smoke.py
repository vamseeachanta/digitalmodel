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
    hash_tree,
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
from digitalmodel.solvers.openfoam.validation.sloshing_3d import (
    Sloshing3DConfig,
    write_sloshing_case,
)


REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_INPUT = REPO_ROOT / "examples/cfd/synthetic_l_tank/input.yml"
DEFAULT_EVIDENCE = REPO_ROOT / "docs/api/cfd/synthetic-l-tank-smoke.json"
OPENFOAM_PACKAGE_VERSION = "2312.260127-2"
OPENMPI_PACKAGE_VERSION = "4.1.6-7ubuntu2"


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
) -> SmokeResult:
    """Build, convert, snapshot, execute, and attest one synthetic smoke."""
    plan = plan_smoke(ranks, visible_cpus, selected_ranks)
    execution_metrics = _execution_metrics(visible_cpus)
    spec, source, case = _prepare_case(
        work_dir, input_yaml, cpb, end_time, delta_t, ranks
    )
    bridge = prepare_gmsh_poly_mesh(case, source, toolchain=_toolchain())
    execution = prepare_prebuilt_execution(case, bridge.manifest_path)
    try:
        angle = _prescribed_yaw_angle(execution.case_dir, end_time)
        result = execute_smoke(
            plan,
            execution.case_dir,
            end_time=end_time,
            time_precision=time_precision,
            length=spec.tank.length,
            rotation_radians=angle,
            origin=(0.5 * spec.tank.breadth, 0.0, 0.5 * spec.tank.length),
        )
        execution.verify_unchanged()
        context = _evidence_context(
            execution_case=execution.case_dir,
            bridge_manifest=bridge.manifest,
            execution_class=execution_class,
            execution_metrics=execution_metrics,
        )
        write_reduced_evidence(
            evidence,
            result,
            bridge_manifest=bridge.manifest,
            **context,
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
    case = work_dir / "synthetic-l-tank-case"
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


def _evidence_context(
    *,
    execution_case: Path,
    bridge_manifest: dict[str, Any],
    execution_class: str,
    execution_metrics: dict[str, float],
) -> dict[str, Any]:
    if bridge_manifest.get("status") != "completed":
        raise SmokeError("bridge manifest must be completed")
    artifacts = {
        "uv_lock": _file_artifact(REPO_ROOT / "uv.lock", "uv.lock"),
        "input": _file_artifact(execution_case / "input.yml", "input.yml"),
        "source_msh": _file_artifact(
            execution_case / "source.msh", "source.msh"
        ),
        "initial_fields": _tree_artifact(execution_case / "0", "0"),
        "system": _tree_artifact(execution_case / "system", "system"),
        "poly_mesh": _tree_artifact(
            execution_case / "constant" / "polyMesh", "constant/polyMesh"
        ),
    }
    return {
        "artifacts": artifacts,
        "source_provenance": _source_provenance(),
        "execution_class": execution_class,
        "execution_metrics": execution_metrics,
    }


def _execution_metrics(visible_cpus: int) -> dict[str, float]:
    try:
        load1 = os.getloadavg()[0]
    except OSError as exc:
        raise SmokeError("one-minute load average is unavailable") from exc
    return {"load1": load1, "load_per_core": load1 / visible_cpus}


def _source_provenance() -> dict[str, Any]:
    status = _git_output("status", "--porcelain", "--untracked-files=all")
    if status:
        raise SmokeError("source checkout must be clean before live execution")
    commit = _git_output("rev-parse", "HEAD")
    tracked = _git_output("ls-files", "-s", "-z", binary=True)
    return {
        "clean": True,
        "commit": commit,
        "tracked_sources_sha256": hashlib.sha256(tracked).hexdigest(),
    }


def _git_output(*args: str, binary: bool = False) -> Any:
    try:
        process = subprocess.run(
            ["git", *args],
            cwd=REPO_ROOT,
            capture_output=True,
            text=not binary,
            check=False,
            shell=False,
            timeout=30,
        )
    except subprocess.TimeoutExpired as exc:
        raise SmokeError(f"git {' '.join(args)} timed out") from exc
    if process.returncode != 0:
        raise SmokeError(f"git {' '.join(args)} failed")
    return process.stdout if binary else process.stdout.strip()


def _file_artifact(path: Path, relative: str) -> dict[str, Any]:
    content = path.read_bytes()
    return {
        "path": relative,
        "size": len(content),
        "sha256": hashlib.sha256(content).hexdigest(),
    }


def _tree_artifact(path: Path, relative: str) -> dict[str, Any]:
    tree = hash_tree(path)
    return {
        "path": relative,
        "file_count": tree.file_count,
        "total_bytes": tree.total_bytes,
        "tree_sha256": tree.sha256,
    }


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
        )
    except (GmshBridgeError, OSError, PrebuiltMeshError, SmokeError, ValueError) as exc:
        print(f"synthetic smoke failed: {exc}")
        return 1
    print(f"synthetic smoke completed: ranks={ranks} evidence={args.evidence}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
