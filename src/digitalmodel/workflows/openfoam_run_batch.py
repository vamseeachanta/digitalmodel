"""Behavior-preserving facade for the OpenFOAM CFD batch workflow (#1560)."""

from __future__ import annotations

import json  # noqa: F401 - legacy public facade surface
import math  # noqa: F401 - legacy public facade surface
import os  # noqa: F401 - legacy public facade surface
import re  # noqa: F401 - legacy public facade surface
import shutil  # noqa: F401 - legacy public facade surface
import subprocess  # noqa: F401 - legacy public facade surface
import time  # noqa: F401 - legacy public facade surface
from concurrent.futures import ThreadPoolExecutor  # noqa: F401
from copy import deepcopy  # noqa: F401 - legacy public facade surface
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable  # noqa: F401 - legacy public facade surface

import pandas as pd  # noqa: F401 - legacy public facade surface
from loguru import logger

from digitalmodel.workflows.openfoam_batch_config import (  # noqa: F401
    DEFAULT_MESH_UTILITY,
    DEFAULT_MODE,
    DEFAULT_OUTPUT_DIR,
    DEFAULT_TIMEOUT_SECONDS,
    DEFAULT_WORK_DIR,
    VALID_MODES,
    WORKER_CORE_FRACTION,
    _RESERVED_ROW_KEYS,
    default_workers,
    render_cases as _render_cases_impl,
    resolve_case_matrix as _resolve_case_matrix_impl,
    resolve_dir as _resolve_dir_impl,
    resolve_path as _resolve_path_impl,
    resolve_workers,
)
from digitalmodel.workflows.openfoam_batch_execution import (  # noqa: F401
    SOLVER_ERROR_MESSAGE,
    build_case as _build_case_impl,
    execute_mpi_plan as _execute_mpi_plan_impl,
    mpi_command_plan,
    run_case_mpi as _run_case_mpi_impl,
    run_case_pool as _run_case_pool_impl,
    run_command as _run_command_impl,
    run_pool as _run_pool_impl,
    solve_serial as _solve_serial_impl,
    solver_ready as _solver_ready_impl,
)
from digitalmodel.workflows.openfoam_batch_layout import (  # noqa: F401
    DECOMPOSE_PAR_DICT as _DECOMPOSE_PAR_DICT,
    clean_case_dir as _clean_case_dir_impl,
    has_processor_dirs as _has_processor_dirs_impl,
    prune_processor_dirs as _prune_processor_dirs_impl,
    set_start_from_latest_time as _set_start_from_latest_time_impl,
    write_decompose_par_dict as _write_decompose_par_dict_impl,
)
from digitalmodel.workflows.openfoam_batch_results import (  # noqa: F401
    CHECKPOINT_FILENAME,
    RESULTS_ALLOWED_SUFFIXES as _RESULTS_ALLOWED_SUFFIXES,
    load_checkpoint as _load_checkpoint_impl,
    make_row as _make_row_impl,
    write_checkpoint as _write_checkpoint_impl,
    write_manifest as _write_manifest_impl,
    write_summary as _write_summary_impl,
)

_resolve_case_matrix = _resolve_case_matrix_impl
_render_cases = _render_cases_impl
_resolve_path = _resolve_path_impl
_resolve_dir = _resolve_dir_impl
_run_pool = _run_pool_impl
_run_case_pool = _run_case_pool_impl
_solve_serial = _solve_serial_impl
_run_case_mpi = _run_case_mpi_impl
_execute_mpi_plan = _execute_mpi_plan_impl
_build_case = _build_case_impl
_solver_ready = _solver_ready_impl
_run_command = _run_command_impl
_write_decompose_par_dict = _write_decompose_par_dict_impl
_prune_processor_dirs = _prune_processor_dirs_impl
_has_processor_dirs = _has_processor_dirs_impl
_clean_case_dir = _clean_case_dir_impl
_set_start_from_latest_time = _set_start_from_latest_time_impl
_load_checkpoint = _load_checkpoint_impl
_write_checkpoint = _write_checkpoint_impl
_row = _make_row_impl
_write_manifest = _write_manifest_impl
_write_summary = _write_summary_impl


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
    timeout = int(run_settings.get("timeout_seconds", DEFAULT_TIMEOUT_SECONDS))
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
        "timeout": timeout,
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
