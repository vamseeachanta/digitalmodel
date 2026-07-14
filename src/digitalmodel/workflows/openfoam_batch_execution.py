"""Legacy execution mechanics for OpenFOAM batch pool and MPI modes."""

from __future__ import annotations

import shutil
import subprocess
import sys
import time
from contextlib import nullcontext
from concurrent.futures import ThreadPoolExecutor
from copy import deepcopy
from pathlib import Path
from typing import Any, Callable

from loguru import logger

from digitalmodel.workflows.openfoam_batch_config import (
    DEFAULT_MESH_UTILITY,
    DEFAULT_TIMEOUT_SECONDS,
)
from digitalmodel.workflows.openfoam_batch_legacy_layout import (
    clean_case_dir,
    has_processor_dirs,
    prune_processor_dirs,
    set_start_from_latest_time,
    write_decompose_par_dict,
)
from digitalmodel.workflows.openfoam_batch_results import (
    load_checkpoint,
    load_external_checkpoint,
    make_row,
    write_checkpoint,
    write_external_checkpoint,
)

SOLVER_ERROR_MESSAGE = (
    "OpenFOAM solver / utilities are not on PATH on this host. "
    "openfoam_run_batch is a requires-solver workflow: dispatch it to a "
    "solver-capable host (run 'openfoam doctor --require-solver' to confirm) "
    "or set run_batch.mock: true for a solver-free case-build dry run."
)


def _compat(name: str, fallback: Callable) -> Callable:
    """Honor legacy facade monkeypatch points without a circular import."""
    facade = sys.modules.get("digitalmodel.workflows.openfoam_run_batch")
    return getattr(facade, name, fallback) if facade else fallback


def _case_lock(item: dict):
    layout = item.get("layout")
    return layout.lock(item["name"]) if layout else nullcontext()


def _load_case_checkpoint(item: dict) -> dict | None:
    if item.get("layout"):
        return load_external_checkpoint(item["layout"], item["name"], item["identity"])
    return _compat("_load_checkpoint", load_checkpoint)(item["work_dir"])


def _write_case_checkpoint(item: dict, row: dict) -> None:
    if item.get("layout"):
        write_external_checkpoint(item["layout"], item["name"], item["identity"], row)
    else:
        _compat("_write_checkpoint", write_checkpoint)(item["work_dir"], row)


def _clean_case(item: dict) -> None:
    if item.get("layout"):
        item["layout"].clean_case(item["name"])
    else:
        _compat("_clean_case_dir", clean_case_dir)(item["work_dir"])


def _prune_case(item: dict) -> None:
    if item.get("layout"):
        item["layout"].prune_processors(item["name"])
    else:
        _compat("_prune_processor_dirs", prune_processor_dirs)(item["work_dir"])


def run_pool(
    rendered: list[dict[str, Any]],
    run_settings: dict,
    workers: int,
    mock: bool,
) -> list[dict[str, Any]]:
    """Execute cases through the bounded legacy thread pool."""
    rows_by_index: dict[int, dict[str, Any]] = {}
    runner = _compat("_run_case_pool", run_case_pool)
    with ThreadPoolExecutor(max_workers=min(workers, len(rendered))) as pool:
        futures = {
            pool.submit(runner, item, run_settings, mock): item
            for item in rendered
        }
        for future, item in futures.items():
            try:
                rows_by_index[item["index"]] = future.result()
            except Exception as exc:  # noqa: BLE001 - per-case isolation
                row = _compat("_row", make_row)
                rows_by_index[item["index"]] = row(
                    item, status="failed", error=str(exc)
                )
    return [rows_by_index[item["index"]] for item in rendered]


def run_case_pool(
    item: dict[str, Any], run_settings: dict, mock: bool
) -> dict[str, Any]:
    """Execute one legacy pool case with checkpoint retry semantics."""
    with _case_lock(item):
        return _run_case_pool_locked(item, run_settings, mock)


def _run_case_pool_locked(item, run_settings, mock) -> dict[str, Any]:
    checkpoint = _load_case_checkpoint(item)
    if checkpoint is not None:
        return checkpoint
    start = time.monotonic()
    try:
        _clean_case(item)
        case_dir = _compat("_build_case", build_case)(item)
        if mock:
            row = _compat("_row", make_row)(
                item,
                status="completed",
                case_dir=case_dir,
                solver=item["settings"].get("solver"),
                mock=True,
            )
        else:
            row = _compat("_solve_serial", solve_serial)(
                item, case_dir, run_settings
            )
    except Exception as exc:  # noqa: BLE001 - per-case isolation
        row = _compat("_row", make_row)(item, status="failed", error=str(exc))
    row["wall_seconds"] = round(time.monotonic() - start, 3)
    _write_case_checkpoint(item, row)
    return row


def solve_serial(
    item: dict[str, Any], case_dir: Path, run_settings: dict
) -> dict[str, Any]:
    """Run one prepared case through the fail-closed single-rank runner."""
    from digitalmodel.solvers.openfoam.runner import (
        OpenFOAMRunConfig,
        OpenFOAMRunner,
    )

    settings = item["settings"]
    run_cfg = OpenFOAMRunConfig(
        solver=settings.get("solver"),
        mesh_utility=settings.get("mesh_utility", DEFAULT_MESH_UTILITY),
        run_snappy=bool(settings.get("run_snappy", False)),
        run_set_fields=bool(settings.get("run_set_fields", False)),
        to_vtk=bool(settings.get("to_vtk", False)),
        timeout_seconds=int(run_settings.get("timeout_seconds", 43200)),
    )
    witnesses = item.get("executables")
    guard = witnesses.launch if witnesses else None
    result = OpenFOAMRunner(run_cfg, executable_guard=guard).run(case_dir)
    status = str(getattr(result.status, "value", result.status)).lower()
    row = _compat("_row", make_row)
    if status == "completed":
        return row(item, status="completed", case_dir=case_dir, solver=result.solver)
    return row(
        item,
        status="failed",
        case_dir=case_dir,
        solver=result.solver,
        error=result.error_message or f"runner status {status}",
    )


def _prepare_mpi_case(
    item: dict[str, Any], run_settings: dict, workers: int, mock: bool
) -> tuple[Path, list[list[str]], str, bool]:
    settings = item["settings"]
    solver = settings["solver"]
    reconstruct = bool(run_settings.get("reconstruct", True))
    layout = item.get("layout")
    resuming = not mock and bool(run_settings.get("resume", False))
    if resuming:
        resuming = (
            layout.has_processor_dirs(item["name"])
            if layout else _compat("_has_processor_dirs", has_processor_dirs)(item["work_dir"])
        )
    if resuming:
        case_dir = item["work_dir"]
        if layout:
            layout.set_start_from_latest_time(item["name"])
        else:
            _compat("_set_start_from_latest_time", set_start_from_latest_time)(case_dir)
    else:
        _clean_case(item)
        case_dir = _compat("_build_case", build_case)(item)
    plan = _compat("mpi_command_plan", mpi_command_plan)(
        solver=solver,
        workers=workers,
        mesh_utility=settings.get("mesh_utility", DEFAULT_MESH_UTILITY),
        run_set_fields=bool(settings.get("run_set_fields", False)),
        reconstruct=reconstruct,
        resume=resuming,
    )
    if not mock and not resuming:
        if layout:
            layout.write_decompose_par_dict(item["name"], workers)
        else:
            _compat("_write_decompose_par_dict", write_decompose_par_dict)(case_dir, workers)
    return case_dir, plan, solver, reconstruct


def run_case_mpi(
    item: dict[str, Any],
    run_settings: dict,
    workers: int,
    mock: bool,
    command_runner: Callable[..., int] | None = None,
) -> dict[str, Any]:
    """Execute one legacy MPI case with resume and reconstruction semantics."""
    with _case_lock(item):
        return _run_case_mpi_locked(
            item, run_settings, workers, mock, command_runner
        )


def _run_case_mpi_locked(
    item, run_settings, workers, mock, command_runner
) -> dict[str, Any]:
    checkpoint = _load_case_checkpoint(item)
    if checkpoint is not None:
        return checkpoint
    solver = item["settings"].get("solver")
    row_factory = _compat("_row", make_row)
    if not solver:
        row = row_factory(
            item, status="failed", error="mode: mpi requires base.solver to be set"
        )
        _write_case_checkpoint(item, row)
        return row
    timeout = int(run_settings.get("timeout_seconds", DEFAULT_TIMEOUT_SECONDS))
    start = time.monotonic()
    try:
        case_dir, plan, solver, reconstruct = _prepare_mpi_case(
            item, run_settings, workers, mock
        )
        if mock:
            row = row_factory(
                item, status="completed", case_dir=case_dir, solver=solver, mock=True
            )
            row["mpi_plan"] = [" ".join(argv) for argv in plan]
        else:
            run = command_runner or _compat("_run_command", run_command)
            execute = _compat("_execute_mpi_plan", execute_mpi_plan)
            row = execute(item, case_dir, plan, solver, run, timeout)
            if reconstruct and row["status"] == "completed":
                _prune_case(item)
    except Exception as exc:  # noqa: BLE001 - checkpoint the failure
        row = row_factory(item, status="failed", error=str(exc))
    row["wall_seconds"] = round(time.monotonic() - start, 3)
    _write_case_checkpoint(item, row)
    return row


def execute_mpi_plan(
    item: dict[str, Any],
    case_dir: Path,
    plan: list[list[str]],
    solver: str,
    run: Callable[..., int],
    timeout: int,
) -> dict[str, Any]:
    """Execute the ordered MPI argv and return the first stage failure."""
    row = _compat("_row", make_row)
    for argv in plan:
        witnesses = item.get("executables")
        launch = _mpi_launch_guard(witnesses, argv, solver)
        with launch as bound:
            command = bound if isinstance(bound, list) else argv
            rc = run(command, case_dir, case_dir / f"log.{argv[0]}", timeout)
        if rc != 0:
            return row(
                item,
                status="failed",
                case_dir=case_dir,
                solver=solver,
                error=f"stage '{argv[0]}' returned non-zero exit code {rc}",
            )
    return row(item, status="completed", case_dir=case_dir, solver=solver)


def _mpi_launch_guard(witnesses, argv: list[str], solver: str):
    if not witnesses:
        return nullcontext()
    if hasattr(witnesses, "launch_argv"):
        return witnesses.launch_argv(argv)
    executable = argv[0]
    if executable == "mpirun" and hasattr(witnesses, "launch_many"):
        return witnesses.launch_many([executable, solver])
    return witnesses.launch(executable)


def mpi_command_plan(
    solver: str,
    workers: int,
    mesh_utility: str = DEFAULT_MESH_UTILITY,
    run_set_fields: bool = False,
    reconstruct: bool = True,
    resume: bool = False,
) -> list[list[str]]:
    """Return the exact legacy ordered MPI utility argv."""
    plan: list[list[str]] = []
    if not resume:
        plan.append([mesh_utility])
        if run_set_fields:
            plan.append(["setFields"])
        plan.append(["decomposePar", "-force"])
    plan.append(
        ["mpirun", "-np", str(workers), "--oversubscribe", solver, "-parallel"]
    )
    if reconstruct:
        plan.append(["reconstructPar"])
    return plan


def build_case(item: dict[str, Any]) -> Path:
    """Author a case through the existing license-free workflow."""
    from digitalmodel.solvers.openfoam.workflow import OpenFOAMWorkflow

    settings = deepcopy(item["settings"])
    settings["operation"] = "build_case"
    build_cfg = {
        "basename": "openfoam",
        "Analysis": {"result_folder": str(item["work_dir"].parent)},
        "openfoam": settings,
    }
    OpenFOAMWorkflow().router(build_cfg)
    return Path(build_cfg["openfoam"]["case_dir"])


def solver_ready(
    mode: str, mesh_utility: str, solver: str | None, reconstruct: bool = True
) -> bool:
    """Require every utility that the selected legacy plan will invoke."""
    required = [mesh_utility]
    if solver:
        required.append(solver)
    if mode == "mpi":
        required += ["decomposePar", "mpirun"]
        if reconstruct:
            required.append("reconstructPar")
    return all(shutil.which(executable) is not None for executable in required)


def run_command(argv: list[str], cwd: Path, log: Path, timeout: int) -> int:
    """Execute one fail-closed utility stage and persist its combined log."""
    try:
        with log.open("w") as stream:
            process = subprocess.run(  # noqa: S603 - fixed utility argv.
                argv,
                cwd=str(cwd),
                stdout=stream,
                stderr=subprocess.STDOUT,
                timeout=timeout,
                check=False,
            )
    except (OSError, subprocess.TimeoutExpired) as exc:
        logger.error("openfoam_run_batch: {} invocation failed: {}", argv[0], exc)
        return 1
    return process.returncode
