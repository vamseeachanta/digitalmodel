"""OpenFOAM CFD batch workflow — one request saturates a CFD node (issue #1560).

One input.yml -> a deterministic case matrix that saturates a dedicated CFD box
in one of two ways (``run_batch.mode``):

* ``pool`` (default): N cases run CONCURRENTLY, each single-rank, through a
  bounded thread pool sized to ~90% of host cores (owner resource policy,
  dm#1553). Each case reuses the existing fail-closed per-case path
  (OpenFOAMWorkflow build + OpenFOAMRunner solve) — the same code the
  ``basename: openfoam`` engine route uses (landed via #1161).
* ``mpi``: ONE large case spread across ranks —
  ``decomposePar -force`` (scotch) -> ``mpirun -np <workers> <solver> -parallel``
  -> optional ``reconstructPar`` -> ``processor*`` dirs ALWAYS pruned (mirrors
  ``scripts/cfd/run_sloshing_3d_benchmark.py``).

Case generation reuses parametric_run's helpers (``_load_cases`` /
``_set_dotted``) so the factorial/range/csv/yaml_matrix schema and dotted-path
mapping behave identically to orcaflex_run_batch (#1554).

Idempotent/resumable: each case writes a ``_result.json`` checkpoint under its
work dir; a re-run skips a case whose checkpoint is ``completed`` (a retry after
a queue timeout safely re-enqueues the same input). Small conclusions
(``results/cases.csv`` + ``results/batch_summary.json``) land under ``results/``
for the licensed-run queue's return allowlist; heavy case trees (meshes, fields,
VTK, ``processor*``) stay in the work dir and never reach ``results/``.
"""

from __future__ import annotations

import json
import math
import os
import shutil
import subprocess
import time
from concurrent.futures import ThreadPoolExecutor
from copy import deepcopy
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable

import pandas as pd
from loguru import logger

from digitalmodel.workflows.parametric_run import _load_cases, _set_dotted

# Owner resource policy (dm#1553): a CFD node is saturated by sizing the pool
# (or the MPI rank count) to ~90% of the host's cores.
WORKER_CORE_FRACTION = 0.9
DEFAULT_MODE = "pool"
DEFAULT_MESH_UTILITY = "blockMesh"
DEFAULT_OUTPUT_DIR = "results"
DEFAULT_WORK_DIR = "batch_runs"
CHECKPOINT_FILENAME = "_result.json"
# results/ is the licensed-run return allowlist: small rollups only. These are
# the ONLY suffixes allowed under it; heavy case trees stay in the work dir.
_RESULTS_ALLOWED_SUFFIXES = {".csv", ".json"}
VALID_MODES = ("pool", "mpi")

SOLVER_ERROR_MESSAGE = (
    "OpenFOAM solver / utilities are not on PATH on this host. "
    "openfoam_run_batch is a requires-solver workflow: dispatch it to a "
    "solver-capable host (run 'openfoam doctor --require-solver' to confirm) "
    "or set run_batch.mock: true for a solver-free case-build dry run."
)

_YAML_SUFFIXES = {".yml", ".yaml"}


def router(cfg: dict) -> dict:
    settings = cfg.get("openfoam_run_batch") or {}
    cfg_dir = Path(cfg.get("_config_dir_path") or Path.cwd())

    run_settings = settings.get("run_batch") or {}
    mode = run_settings.get("mode", DEFAULT_MODE)
    if mode not in VALID_MODES:
        raise ValueError(
            f"openfoam_run_batch run_batch.mode must be pool|mpi, got {mode}"
        )
    mock = bool(run_settings.get("mock", False))
    workers = resolve_workers(run_settings)

    base = settings.get("base") or {}
    if not base.get("case_type"):
        raise ValueError("openfoam_run_batch.base.case_type is required")
    mesh_utility = base.get("mesh_utility", DEFAULT_MESH_UTILITY)
    solver = base.get("solver")

    # Fail-closed once, up front: a real run on a solver-less host must never
    # silently downgrade to a build-only dry run (the licensed-run lane would
    # record a false finish). Per-case failures below (divergence, bad mesh)
    # are isolated; a missing toolchain is not.
    if not mock and not _solver_ready(mode, mesh_utility, solver):
        raise RuntimeError(SOLVER_ERROR_MESSAGE)

    variants = settings.get("variants") or {}
    cases = _resolve_case_matrix(settings.get("cases"), variants, cfg_dir)
    mapping = variants.get("mapping") or {}

    results_dir = _resolve_dir(
        run_settings.get("output_dir", DEFAULT_OUTPUT_DIR), cfg_dir
    )
    work_dir = _resolve_dir(run_settings.get("work_dir", DEFAULT_WORK_DIR), cfg_dir)

    rendered = _render_cases(base, cases, mapping, work_dir)

    started_at = datetime.now(timezone.utc)
    if mode == "mpi":
        if len(rendered) != 1:
            raise ValueError(
                "openfoam_run_batch mode: mpi runs exactly ONE case across "
                f"ranks; the matrix produced {len(rendered)} cases. Use "
                "mode: pool for a multi-case sweep."
            )
        rows = [_run_case_mpi(rendered[0], run_settings, workers, mock)]
    else:
        rows = _run_pool(rendered, run_settings, workers, mock)
    finished_at = datetime.now(timezone.utc)

    manifest_path = results_dir / "cases.csv"
    summary_path = results_dir / "batch_summary.json"
    _write_manifest(rows, manifest_path)
    summary = _write_summary(
        rows=rows,
        path=summary_path,
        mode=mode,
        workers=workers,
        mock=mock,
        started_at=started_at,
        finished_at=finished_at,
    )
    if summary["failed"]:
        logger.warning(
            "openfoam_run_batch: {} of {} cases failed; see {}",
            summary["failed"],
            summary["total_cases"],
            manifest_path,
        )

    settings["cases"] = rows
    settings["outputs"] = {
        "manifest": str(manifest_path),
        "summary": str(summary_path),
    }
    cfg["openfoam_run_batch"] = settings
    return cfg


def default_workers(cpu_count: int | None = None) -> int:
    """~90% of host cores, floored, never below 1 (dm#1553 resource policy)."""
    cores = cpu_count if cpu_count is not None else (os.cpu_count() or 1)
    return max(1, math.floor(WORKER_CORE_FRACTION * cores))


def resolve_workers(run_settings: dict) -> int:
    explicit = run_settings.get("workers")
    if explicit is None:
        return default_workers()
    workers = int(explicit)
    if workers < 1:
        raise ValueError(f"run_batch.workers must be >= 1, got {explicit}")
    return workers


# --------------------------------------------------------------------------- #
#  case matrix (reuses parametric_run's case generation)                      #
# --------------------------------------------------------------------------- #
def _resolve_case_matrix(
    explicit: list[dict] | None, variants: dict, cfg_dir: Path
) -> list[dict[str, Any]]:
    if explicit:
        if variants:
            raise ValueError(
                "openfoam_run_batch: give either cases: [...] or variants:, "
                "not both"
            )
        return [dict(case) for case in explicit]
    if variants:
        return _load_cases(variants, cfg_dir)
    # No matrix -> a single case built from base alone.
    return [{}]


def _render_cases(
    base: dict,
    cases: list[dict[str, Any]],
    mapping: dict[str, str],
    work_dir: Path,
) -> list[dict[str, Any]]:
    base_name = base.get("name") or f"{base['case_type']}_case"
    rendered: list[dict[str, Any]] = []
    for index, case in enumerate(cases):
        case_settings = deepcopy(base)
        # A "name" field names the case dir; every other field is a swept knob
        # injected into the per-case OpenFOAM settings via a dotted path.
        params = {k: v for k, v in case.items() if k != "name"}
        for name, value in params.items():
            _set_dotted(case_settings, mapping.get(name, name), value)
        case_name = case.get("name") or f"{base_name}_{index:03d}"
        case_settings["name"] = case_name
        rendered.append(
            {
                "index": index,
                "name": case_name,
                "case": params,
                "settings": case_settings,
                "work_dir": work_dir / case_name,
            }
        )
    return rendered


# --------------------------------------------------------------------------- #
#  pool mode — bounded thread pool of single-rank cases                       #
# --------------------------------------------------------------------------- #
def _run_pool(
    rendered: list[dict[str, Any]],
    run_settings: dict,
    workers: int,
    mock: bool,
) -> list[dict[str, Any]]:
    # Each leaf blocks on its own OpenFOAM subprocess -> threads give true
    # parallelism with no process-spawn overhead (same shape as the sloshing
    # backbone sweep). Pool is bounded to `workers` so the node is saturated,
    # not oversubscribed.
    rows_by_index: dict[int, dict[str, Any]] = {}
    with ThreadPoolExecutor(max_workers=min(workers, len(rendered))) as pool:
        futures = {
            pool.submit(_run_case_pool, item, run_settings, mock): item
            for item in rendered
        }
        for future, item in futures.items():
            try:
                rows_by_index[item["index"]] = future.result()
            except Exception as exc:  # noqa: BLE001 - per-case isolation
                rows_by_index[item["index"]] = _row(
                    item, status="failed", error=str(exc)
                )
    return [rows_by_index[item["index"]] for item in rendered]


def _run_case_pool(
    item: dict[str, Any], run_settings: dict, mock: bool
) -> dict[str, Any]:
    checkpoint = _load_checkpoint(item["work_dir"])
    if checkpoint is not None:
        return checkpoint

    start = time.monotonic()
    try:
        case_dir = _build_case(item)
        if mock:
            # Mock leaf: the real (license-free) builder authored the case tree,
            # but no solver ran. Never a silent downgrade — mock is explicit.
            row = _row(item, status="completed", case_dir=case_dir,
                       solver=item["settings"].get("solver"), mock=True)
        else:
            row = _solve_serial(item, case_dir, run_settings)
    except Exception as exc:  # noqa: BLE001 - per-case isolation
        row = _row(item, status="failed", error=str(exc))
    row["wall_seconds"] = round(time.monotonic() - start, 3)
    _write_checkpoint(item["work_dir"], row)
    return row


def _solve_serial(
    item: dict[str, Any], case_dir: Path, run_settings: dict
) -> dict[str, Any]:
    """Run one prepared case single-rank through the fail-closed runner."""
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
    result = OpenFOAMRunner(run_cfg).run(case_dir)
    status = str(getattr(result.status, "value", result.status)).lower()
    if status == "completed":
        return _row(item, status="completed", case_dir=case_dir,
                    solver=result.solver)
    return _row(
        item,
        status="failed",
        case_dir=case_dir,
        solver=result.solver,
        error=result.error_message or f"runner status {status}",
    )


# --------------------------------------------------------------------------- #
#  mpi mode — one case across ranks (decomposePar -> mpirun -> reconstruct)    #
# --------------------------------------------------------------------------- #
def _run_case_mpi(
    item: dict[str, Any],
    run_settings: dict,
    workers: int,
    mock: bool,
    command_runner: Callable[..., int] | None = None,
) -> dict[str, Any]:
    checkpoint = _load_checkpoint(item["work_dir"])
    if checkpoint is not None:
        return checkpoint

    run = command_runner or _run_command
    settings = item["settings"]
    solver = settings.get("solver")
    if not solver:
        row = _row(item, status="failed",
                   error="mode: mpi requires base.solver to be set")
        _write_checkpoint(item["work_dir"], row)
        return row
    reconstruct = bool(run_settings.get("reconstruct", True))
    timeout = int(run_settings.get("timeout_seconds", 43200))

    start = time.monotonic()
    try:
        case_dir = _build_case(item)
        plan = mpi_command_plan(
            solver=solver,
            workers=workers,
            mesh_utility=settings.get("mesh_utility", DEFAULT_MESH_UTILITY),
            run_set_fields=bool(settings.get("run_set_fields", False)),
            reconstruct=reconstruct,
        )
        if mock:
            # Explicit mock: record the exact command plan without executing.
            row = _row(item, status="completed", case_dir=case_dir,
                       solver=solver, mock=True)
            row["mpi_plan"] = [" ".join(argv) for argv in plan]
        else:
            _write_decompose_par_dict(case_dir, workers)
            row = _execute_mpi_plan(item, case_dir, plan, solver, run, timeout)
    except Exception as exc:  # noqa: BLE001 - checkpoint the failure
        row = _row(item, status="failed", error=str(exc))
    finally:
        # ALWAYS prune processor* dirs — decomposed sub-meshes are heavy and
        # must never leak into results/ or bloat the work dir (benchmark rule).
        _prune_processor_dirs(item["work_dir"])
    row["wall_seconds"] = round(time.monotonic() - start, 3)
    _write_checkpoint(item["work_dir"], row)
    return row


def _execute_mpi_plan(
    item: dict[str, Any],
    case_dir: Path,
    plan: list[list[str]],
    solver: str,
    run: Callable[..., int],
    timeout: int,
) -> dict[str, Any]:
    for argv in plan:
        log = case_dir / f"log.{argv[0]}"
        rc = run(argv, case_dir, log, timeout)
        if rc != 0:
            return _row(
                item,
                status="failed",
                case_dir=case_dir,
                solver=solver,
                error=f"stage '{argv[0]}' returned non-zero exit code {rc}",
            )
    return _row(item, status="completed", case_dir=case_dir, solver=solver)


def mpi_command_plan(
    solver: str,
    workers: int,
    mesh_utility: str = DEFAULT_MESH_UTILITY,
    run_set_fields: bool = False,
    reconstruct: bool = True,
) -> list[list[str]]:
    """Ordered utility argv for an MPI solve: mesh -> decompose -> solve ->
    (reconstruct). decomposePar ALWAYS precedes mpirun; the solver runs
    ``-parallel`` under ``mpirun -np <workers>`` (mirrors the 3D benchmark)."""
    plan: list[list[str]] = [[mesh_utility]]
    if run_set_fields:
        plan.append(["setFields"])
    plan.append(["decomposePar", "-force"])
    plan.append(
        ["mpirun", "-np", str(workers), "--oversubscribe", solver, "-parallel"]
    )
    if reconstruct:
        plan.append(["reconstructPar"])
    return plan


# --------------------------------------------------------------------------- #
#  shared helpers                                                             #
# --------------------------------------------------------------------------- #
def _build_case(item: dict[str, Any]) -> Path:
    """Author the case tree via the license-free OpenFOAM builder (reuses the
    engine handler's build path) under this case's isolated work dir."""
    from digitalmodel.solvers.openfoam.workflow import OpenFOAMWorkflow

    settings = deepcopy(item["settings"])
    settings["operation"] = "build_case"
    # Build into work_dir's parent so the case tree IS item["work_dir"]
    # (name == the case dir): the tree, its log.* files and the _result.json
    # checkpoint all live in one isolated per-case dir (backbone convention).
    build_cfg = {
        "basename": "openfoam",
        "Analysis": {"result_folder": str(item["work_dir"].parent)},
        "openfoam": settings,
    }
    OpenFOAMWorkflow().router(build_cfg)
    return Path(build_cfg["openfoam"]["case_dir"])


def _solver_ready(mode: str, mesh_utility: str, solver: str | None) -> bool:
    """The CFD ready-gate: the utilities this run will invoke are on PATH.
    Mirrors OpenFOAMRunner._openfoam_available / 'openfoam doctor'."""
    required = [mesh_utility]
    if solver:
        required.append(solver)
    if mode == "mpi":
        required += ["decomposePar", "mpirun"]
    return all(shutil.which(exe) is not None for exe in required)


def _run_command(argv: list[str], cwd: Path, log: Path, timeout: int) -> int:
    """Real subprocess stage (fail-closed on OSError/timeout). Persists the
    utility's stdout+stderr to log.<utility> inside the case dir."""
    try:
        with log.open("w") as fh:
            proc = subprocess.run(  # noqa: S603 - argv is fixed utility names.
                argv,
                cwd=str(cwd),
                stdout=fh,
                stderr=subprocess.STDOUT,
                timeout=timeout,
                check=False,
            )
    except (OSError, subprocess.TimeoutExpired) as exc:
        logger.error("openfoam_run_batch: {} invocation failed: {}", argv[0], exc)
        return 1
    return proc.returncode


_DECOMPOSE_PAR_DICT = """\
FoamFile {{ version 2.0; format ascii; class dictionary; object decomposeParDict; }}

numberOfSubdomains {n};
method scotch;
"""


def _write_decompose_par_dict(case_dir: Path, workers: int) -> None:
    """Author system/decomposeParDict for `workers` scotch subdomains (the
    case builder emits a placeholder; the MPI run pins it to the rank count)."""
    (case_dir / "system" / "decomposeParDict").write_text(
        _DECOMPOSE_PAR_DICT.format(n=workers)
    )


def _prune_processor_dirs(case_dir: Path) -> None:
    if not case_dir.is_dir():
        return
    for proc_dir in case_dir.glob("processor*"):
        shutil.rmtree(proc_dir, ignore_errors=True)


def _load_checkpoint(work_dir: Path) -> dict[str, Any] | None:
    checkpoint = work_dir / CHECKPOINT_FILENAME
    if not checkpoint.is_file():
        return None
    try:
        row = json.loads(checkpoint.read_text())
    except (OSError, json.JSONDecodeError):
        return None
    # Only a completed case is idempotently skipped; a failed checkpoint is
    # re-run so a transient failure can be retried by re-enqueuing the input.
    return row if row.get("status") == "completed" else None


def _write_checkpoint(work_dir: Path, row: dict[str, Any]) -> None:
    work_dir.mkdir(parents=True, exist_ok=True)
    (work_dir / CHECKPOINT_FILENAME).write_text(json.dumps(row, indent=2) + "\n")


def _row(
    item: dict[str, Any],
    *,
    status: str,
    case_dir: Path | None = None,
    solver: str | None = None,
    error: str | None = None,
    mock: bool = False,
) -> dict[str, Any]:
    return {
        "index": item["index"],
        "name": item["name"],
        **item["case"],
        "status": status,
        "solver": solver,
        "mock": mock,
        "error": error,
        "case_dir": str(case_dir) if case_dir else None,
        "wall_seconds": 0.0,
    }


def _write_manifest(rows: list[dict[str, Any]], path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(rows).to_csv(path, index=False)


def _write_summary(
    rows: list[dict[str, Any]],
    path: Path,
    mode: str,
    workers: int,
    mock: bool,
    started_at: datetime,
    finished_at: datetime,
) -> dict:
    completed = sum(1 for row in rows if row["status"] == "completed")
    summary = {
        "workflow": "openfoam_run_batch",
        "mode": mode,
        "total_cases": len(rows),
        "completed": completed,
        "failed": len(rows) - completed,
        "workers": workers,
        "host_cpu_count": os.cpu_count(),
        "mock": mock,
        "started_at_utc": started_at.isoformat(),
        "finished_at_utc": finished_at.isoformat(),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(summary, indent=2) + "\n")
    return summary


def _resolve_path(path_value: str, cfg_dir: Path) -> Path:
    path = Path(path_value)
    if path.is_absolute():
        return path
    return cfg_dir / path


def _resolve_dir(path_value: str, cfg_dir: Path) -> Path:
    resolved = _resolve_path(path_value, cfg_dir)
    resolved.mkdir(parents=True, exist_ok=True)
    return resolved
