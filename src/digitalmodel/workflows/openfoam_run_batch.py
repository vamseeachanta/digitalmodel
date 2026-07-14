"""Behavior-preserving facade for the OpenFOAM CFD batch workflow (#1560)."""

from __future__ import annotations

import os
import shutil
from datetime import datetime, timezone
from pathlib import Path

from loguru import logger

from digitalmodel.workflows.openfoam_batch_config import (
    DEFAULT_MESH_UTILITY,
    DEFAULT_MODE,
    DEFAULT_OUTPUT_DIR,
    DEFAULT_TIMEOUT_SECONDS,
    DEFAULT_WORK_DIR,
    VALID_MODES,
    WORKER_CORE_FRACTION,
    _RESERVED_ROW_KEYS,
    default_workers,
    render_cases,
    resolve_case_matrix,
    resolve_dir,
    resolve_path,
    resolve_workers,
)
from digitalmodel.workflows.openfoam_batch_execution import (
    SOLVER_ERROR_MESSAGE,
    build_case,
    execute_mpi_plan,
    mpi_command_plan,
    run_case_mpi,
    run_case_pool,
    run_command,
    run_pool,
    solve_serial,
    solver_ready,
)
from digitalmodel.workflows.openfoam_batch_layout import (
    DECOMPOSE_PAR_DICT,
    clean_case_dir,
    has_processor_dirs,
    prune_processor_dirs,
    set_start_from_latest_time,
    write_decompose_par_dict,
)
from digitalmodel.workflows.openfoam_batch_results import (
    CHECKPOINT_FILENAME,
    RESULTS_ALLOWED_SUFFIXES,
    load_checkpoint,
    make_row,
    write_checkpoint,
    write_manifest,
    write_summary,
)

__all__ = [
    "CHECKPOINT_FILENAME", "WORKER_CORE_FRACTION", "_RESERVED_ROW_KEYS",
    "default_workers", "mpi_command_plan", "os", "resolve_workers", "router", "shutil",
]

_RESULTS_ALLOWED_SUFFIXES = RESULTS_ALLOWED_SUFFIXES
_DECOMPOSE_PAR_DICT = DECOMPOSE_PAR_DICT
_resolve_case_matrix = resolve_case_matrix
_render_cases = render_cases
_resolve_path = resolve_path
_resolve_dir = resolve_dir
_run_pool = run_pool
_run_case_pool = run_case_pool
_solve_serial = solve_serial
_run_case_mpi = run_case_mpi
_execute_mpi_plan = execute_mpi_plan
_build_case = build_case
_solver_ready = solver_ready
_run_command = run_command
_write_decompose_par_dict = write_decompose_par_dict
_prune_processor_dirs = prune_processor_dirs
_has_processor_dirs = has_processor_dirs
_clean_case_dir = clean_case_dir
_set_start_from_latest_time = set_start_from_latest_time
_load_checkpoint = load_checkpoint
_write_checkpoint = write_checkpoint
_row = make_row
_write_manifest = write_manifest
_write_summary = write_summary


def _prepare_batch(cfg: dict) -> dict:
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
    reconstruct = bool(run_settings.get("reconstruct", True))
    if not mock and not _solver_ready(mode, mesh_utility, solver, reconstruct):
        raise RuntimeError(SOLVER_ERROR_MESSAGE)
    variants = settings.get("variants") or {}
    cases = _resolve_case_matrix(settings.get("cases"), variants, cfg_dir)
    mapping = settings.get("mapping") or variants.get("mapping") or {}
    results_dir = _resolve_dir(
        run_settings.get("output_dir", DEFAULT_OUTPUT_DIR), cfg_dir
    )
    work_dir = _resolve_dir(
        run_settings.get("work_dir", DEFAULT_WORK_DIR), cfg_dir
    )
    return {
        "settings": settings,
        "run_settings": run_settings,
        "mode": mode,
        "mock": mock,
        "workers": workers,
        "timeout": int(
            run_settings.get("timeout_seconds", DEFAULT_TIMEOUT_SECONDS)
        ),
        "results_dir": results_dir,
        "rendered": _render_cases(base, cases, mapping, work_dir),
    }


def _execute_batch(batch: dict) -> tuple[list[dict], datetime, datetime]:
    started_at = datetime.now(timezone.utc)
    rendered = batch["rendered"]
    if batch["mode"] == "mpi":
        if len(rendered) != 1:
            raise ValueError(
                "openfoam_run_batch mode: mpi runs exactly ONE case across "
                f"ranks; the matrix produced {len(rendered)} cases. Use "
                "mode: pool for a multi-case sweep."
            )
        rows = [
            _run_case_mpi(
                rendered[0],
                batch["run_settings"],
                batch["workers"],
                batch["mock"],
            )
        ]
    else:
        rows = _run_pool(
            rendered, batch["run_settings"], batch["workers"], batch["mock"]
        )
    return rows, started_at, datetime.now(timezone.utc)


def _finalize_batch(
    cfg: dict,
    batch: dict,
    rows: list[dict],
    started_at: datetime,
    finished_at: datetime,
) -> dict:
    manifest_path = batch["results_dir"] / "cases.csv"
    summary_path = batch["results_dir"] / "batch_summary.json"
    _write_manifest(rows, manifest_path)
    summary = _write_summary(
        rows=rows,
        path=summary_path,
        mode=batch["mode"],
        workers=batch["workers"],
        mock=batch["mock"],
        timeout_seconds=batch["timeout"],
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
    settings = batch["settings"]
    settings["cases"] = rows
    settings["outputs"] = {
        "manifest": str(manifest_path),
        "summary": str(summary_path),
    }
    cfg["openfoam_run_batch"] = settings
    return cfg


def router(cfg: dict) -> dict:
    """Route a legacy request through the decomposed batch implementation."""
    batch = _prepare_batch(cfg)
    rows, started_at, finished_at = _execute_batch(batch)
    return _finalize_batch(cfg, batch, rows, started_at, finished_at)
