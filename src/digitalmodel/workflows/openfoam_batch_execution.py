"""Execution and checkpoint helpers for OpenFOAM batch runs."""

from __future__ import annotations

import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import time
from concurrent.futures import ThreadPoolExecutor
from copy import deepcopy
from typing import Any, Callable

from loguru import logger

from digitalmodel.workflows.openfoam_batch_identity import file_sha256
from digitalmodel.workflows.openfoam_batch_layout import WorkLayout
from digitalmodel.workflows.openfoam_batch_results import redact_rows, row


CHECKPOINT_FILENAME = "_result.json"
DEFAULT_MESH_UTILITY = "blockMesh"
DEFAULT_TIMEOUT_SECONDS = 43200


def make_checkpoint(*, identity_sha256: str, owner_token: str, case: str,
                    row: dict[str, Any]) -> dict[str, Any]:
    return {"schema": 2, "identity_sha256": identity_sha256,
            "owner_token": owner_token, "case": case, "row": row}


def checkpoint_matches(payload: object, identity_sha256: str,
                       owner_token: str, case: str) -> bool:
    return (isinstance(payload, dict) and payload.get("schema") == 2
            and payload.get("identity_sha256") == identity_sha256
            and payload.get("owner_token") == owner_token
            and payload.get("case") == case and isinstance(payload.get("row"), dict)
            and payload["row"].get("status") == "completed")


def load_checkpoint(work_dir: Path, *, identity_sha256: str | None = None,
                    owner_token: str | None = None, case: str | None = None) -> dict[str, Any] | None:
    path = work_dir / CHECKPOINT_FILENAME
    try:
        payload = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError):
        return None
    if identity_sha256 is None:  # legacy compatibility
        return payload if isinstance(payload, dict) and payload.get("status") == "completed" else None
    return payload["row"] if checkpoint_matches(payload, identity_sha256, owner_token or "", case or "") else None


def write_checkpoint(work_dir: Path, row: dict[str, Any], *,
                     identity_sha256: str | None = None,
                     owner_token: str | None = None, case: str | None = None) -> None:
    work_dir.mkdir(parents=True, exist_ok=True)
    payload = row if identity_sha256 is None else make_checkpoint(
        identity_sha256=identity_sha256, owner_token=owner_token or "",
        case=case or "", row=row)
    target = work_dir / CHECKPOINT_FILENAME
    tmp = work_dir / f"{CHECKPOINT_FILENAME}.{os.getpid()}.tmp"
    tmp.write_text(json.dumps(payload, indent=2) + "\n")
    os.replace(tmp, target)


def run_cases(rendered: list[dict[str, Any]], run_settings: dict, *, mode: str,
              workers: int, mock: bool, layout: WorkLayout | None = None,
              builder: Callable[[dict[str, Any]], Path] | None = None,
              tool_bindings: dict[str, tuple[Path, str]] | None = None) -> list[dict[str, Any]]:
    build = builder or build_case
    if mode == "mpi":
        if len(rendered) != 1:
            raise ValueError("openfoam_run_batch mode: mpi runs exactly ONE case")
        return [run_case_mpi(rendered[0], run_settings, workers, mock,
                             layout=layout, builder=build, tool_bindings=tool_bindings)]
    rows: dict[int, dict[str, Any]] = {}
    with ThreadPoolExecutor(max_workers=min(workers, len(rendered))) as pool:
        futures = {pool.submit(run_case_pool, item, run_settings, mock,
                               layout=layout, builder=build,
                               tool_bindings=tool_bindings): item
                   for item in rendered}
        for future, item in futures.items():
            try:
                rows[item["index"]] = future.result()
            except Exception as exc:  # noqa: BLE001
                rows[item["index"]] = row(item, status="failed", error=str(exc))
    return [rows[item["index"]] for item in rendered]


def run_case_pool(item: dict[str, Any], run_settings: dict, mock: bool,
                  *, layout: WorkLayout | None = None,
                  builder: Callable[[dict[str, Any]], Path] | None = None,
                  tool_bindings: dict[str, tuple[Path, str]] | None = None) -> dict[str, Any]:
    build = builder or build_case
    with _case_lock(layout, item["name"]):
        checkpoint = _checkpoint(item, layout)
        if checkpoint is not None:
            return checkpoint
        start = time.monotonic()
        try:
            _clean(item, layout)
            case_dir = build(item)
            result = (row(item, status="completed", case_dir=case_dir,
                          solver=item["settings"].get("solver"), mock=True)
                      if mock else solve_serial(item, case_dir, run_settings, tool_bindings))
        except Exception as exc:  # noqa: BLE001
            result = row(item, status="failed", error=str(exc))
        result["wall_seconds"] = round(time.monotonic() - start, 3)
        return _save(item, result, layout)


def solve_serial(item: dict[str, Any], case_dir: Path,
                 run_settings: dict,
                 tool_bindings: dict[str, tuple[Path, str]] | None = None) -> dict[str, Any]:
    from digitalmodel.solvers.openfoam.runner import OpenFOAMRunConfig, OpenFOAMRunner
    settings = item["settings"]
    config = OpenFOAMRunConfig(
        solver=settings.get("solver"),
        mesh_utility=settings.get("mesh_utility", DEFAULT_MESH_UTILITY),
        run_snappy=bool(settings.get("run_snappy", False)),
        run_set_fields=bool(settings.get("run_set_fields", False)),
        to_vtk=bool(settings.get("to_vtk", False)),
        timeout_seconds=int(run_settings.get("timeout_seconds", DEFAULT_TIMEOUT_SECONDS)),
    )
    verifier = None
    if tool_bindings:
        def verifier(name: str) -> None:
            _verify_executable(*tool_bindings[name])
    result = OpenFOAMRunner(config, executable_verifier=verifier).run(case_dir)
    status = str(getattr(result.status, "value", result.status)).lower()
    if status == "completed":
        return row(item, status="completed", case_dir=case_dir, solver=result.solver)
    return row(item, status="failed", case_dir=case_dir, solver=result.solver,
               error=result.error_message or f"runner status {status}")


def run_case_mpi(item: dict[str, Any], run_settings: dict, workers: int,
                 mock: bool, command_runner: Callable[..., int] | None = None,
                 *, layout: WorkLayout | None = None,
                 builder: Callable[[dict[str, Any]], Path] | None = None,
                 tool_bindings: dict[str, tuple[Path, str]] | None = None) -> dict[str, Any]:
    build = builder or build_case
    with _case_lock(layout, item["name"]):
        checkpoint = _checkpoint(item, layout)
        if checkpoint is not None:
            return checkpoint
        return _run_case_mpi_unlocked(item, run_settings, workers, mock,
                                      command_runner, layout, build, tool_bindings)


def _run_case_mpi_unlocked(item: dict[str, Any], run_settings: dict, workers: int,
                           mock: bool, command_runner: Callable[..., int] | None,
                           layout: WorkLayout | None,
                           builder: Callable[[dict[str, Any]], Path],
                           tool_bindings: dict[str, tuple[Path, str]] | None) -> dict[str, Any]:
    solver = item["settings"].get("solver")
    if not solver:
        return _save(item, row(item, status="failed", error="mode: mpi requires base.solver"), layout)
    reconstruct = bool(run_settings.get("reconstruct", True))
    resume = bool(run_settings.get("resume", False)) and _has_processors(item["work_dir"])
    start = time.monotonic()
    try:
        case_dir = _prepare_mpi_case(item, resume, layout, builder)
        plan = mpi_command_plan(solver, workers,
            item["settings"].get("mesh_utility", DEFAULT_MESH_UTILITY),
            bool(item["settings"].get("run_set_fields", False)), reconstruct, resume)
        if mock:
            result = row(item, status="completed", case_dir=case_dir, solver=solver, mock=True)
            result["mpi_plan"] = [" ".join(argv) for argv in plan]
        else:
            for binding in (tool_bindings or {}).values():
                _verify_executable(*binding)
            if not resume:
                write_decompose_par_dict(case_dir, workers)
            result = execute_mpi_plan(item, case_dir, plan, solver,
                command_runner or run_command,
                int(run_settings.get("timeout_seconds", DEFAULT_TIMEOUT_SECONDS)),
                tool_bindings=tool_bindings)
            for binding in (tool_bindings or {}).values():
                _verify_executable(*binding)
            if reconstruct and result["status"] == "completed":
                _prune(item, layout)
    except Exception as exc:  # noqa: BLE001
        result = row(item, status="failed", error=str(exc))
    result["wall_seconds"] = round(time.monotonic() - start, 3)
    return _save(item, result, layout)


def execute_mpi_plan(item: dict[str, Any], case_dir: Path, plan: list[list[str]],
                     solver: str, run: Callable[..., int], timeout: int,
                     *, tool_bindings: dict[str, tuple[Path, str]] | None = None) -> dict[str, Any]:
    for argv in plan:
        binding = (tool_bindings or {}).get(argv[0])
        rc = run(argv, case_dir, case_dir / f"log.{argv[0]}", timeout,
                 expected_executable=binding) if binding else run(
                     argv, case_dir, case_dir / f"log.{argv[0]}", timeout)
        if rc:
            return row(item, status="failed", case_dir=case_dir, solver=solver,
                       error=f"stage '{argv[0]}' returned non-zero exit code {rc}")
    return row(item, status="completed", case_dir=case_dir, solver=solver)


def build_case(item: dict[str, Any]) -> Path:
    from digitalmodel.solvers.openfoam.workflow import OpenFOAMWorkflow
    settings = deepcopy(item["settings"])
    settings["operation"] = "build_case"
    config = {"basename": "openfoam",
              "Analysis": {"result_folder": str(item["work_dir"].parent)},
              "openfoam": settings}
    OpenFOAMWorkflow().router(config)
    return Path(config["openfoam"]["case_dir"])


def _prepare_mpi_case(item: dict[str, Any], resume: bool,
                      layout: WorkLayout | None,
                      builder: Callable[[dict[str, Any]], Path]) -> Path:
    if resume:
        set_start_from_latest_time(item["work_dir"])
        return item["work_dir"]
    _clean(item, layout)
    return builder(item)


def _checkpoint(item: dict[str, Any], layout: WorkLayout | None) -> dict[str, Any] | None:
    if layout is None:
        return load_checkpoint(item["work_dir"])
    return load_checkpoint(item["work_dir"], identity_sha256=layout.identity_sha256,
                           owner_token=layout.owner_token, case=item["name"])


def _save(item: dict[str, Any], result: dict[str, Any],
          layout: WorkLayout | None) -> dict[str, Any]:
    if layout is None:
        write_checkpoint(item["work_dir"], result)
        return result
    safe = redact_rows([result], layout.operator_root)[0]
    write_checkpoint(item["work_dir"], safe, identity_sha256=layout.identity_sha256,
                     owner_token=layout.owner_token, case=item["name"])
    return safe


def _clean(item: dict[str, Any], layout: WorkLayout | None) -> None:
    if layout is not None:
        layout.clean_case(item["name"])
    elif item["work_dir"].is_dir():
        shutil.rmtree(item["work_dir"], ignore_errors=True)


def _prune(item: dict[str, Any], layout: WorkLayout | None) -> None:
    if layout is not None:
        layout.prune_processors(item["name"])
    else:
        for path in item["work_dir"].glob("processor*"):
            shutil.rmtree(path, ignore_errors=True)


def _has_processors(case_dir: Path) -> bool:
    return case_dir.is_dir() and any(case_dir.glob("processor*"))


def _case_lock(layout: WorkLayout | None, case: str):
    from contextlib import nullcontext
    return layout.lock(f"case-{case}") if layout is not None else nullcontext()


def mpi_command_plan(solver: str, workers: int, mesh_utility: str = DEFAULT_MESH_UTILITY,
                     run_set_fields: bool = False, reconstruct: bool = True,
                     resume: bool = False) -> list[list[str]]:
    plan: list[list[str]] = []
    if not resume:
        plan.append([mesh_utility])
        if run_set_fields:
            plan.append(["setFields"])
        plan.append(["decomposePar", "-force"])
    plan.append(["mpirun", "-np", str(workers), "--oversubscribe", solver, "-parallel"])
    if reconstruct:
        plan.append(["reconstructPar"])
    return plan


def run_command(argv: list[str], cwd: Path, log: Path, timeout: int,
                expected_executable: tuple[Path, str] | None = None) -> int:
    if expected_executable is not None:
        _verify_executable(*expected_executable)
    try:
        with log.open("w") as stream:
            proc = subprocess.run(argv, cwd=str(cwd), stdout=stream,
                                  stderr=subprocess.STDOUT, timeout=timeout, check=False)
    except (OSError, subprocess.TimeoutExpired) as exc:
        logger.error("openfoam_run_batch: {} invocation failed: {}", argv[0], exc)
        return 1
    if expected_executable is not None:
        _verify_executable(*expected_executable)
    return proc.returncode


def write_decompose_par_dict(case_dir: Path, workers: int) -> None:
    text = ("FoamFile { version 2.0; format ascii; class dictionary; "
            "object decomposeParDict; }\n\n"
            f"numberOfSubdomains {workers};\nmethod scotch;\n")
    (case_dir / "system" / "decomposeParDict").write_text(text)


def set_start_from_latest_time(case_dir: Path) -> None:
    control = case_dir / "system" / "controlDict"
    if not control.is_file():
        raise RuntimeError("resume requested but system/controlDict is missing")
    text = control.read_text()
    patched = re.sub(r"startFrom\s+\w+\s*;", "startFrom       latestTime;", text)
    control.write_text(patched if patched != text or "startFrom" in text
                       else text + "\nstartFrom       latestTime;\n")


def _verify_executable(path: Path, expected_sha256: str) -> None:
    if not path.is_file() or path.is_symlink() or file_sha256(path) != expected_sha256:
        raise RuntimeError(f"selected executable {path.name!r} changed before/during launch")
