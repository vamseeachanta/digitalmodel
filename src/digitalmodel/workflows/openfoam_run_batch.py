"""Behavior-preserving facade for the OpenFOAM CFD batch workflow (#1560)."""

from __future__ import annotations

import json  # noqa: F401
import math  # noqa: F401 - legacy public facade surface
import os  # noqa: F401 - legacy public facade surface
import re  # noqa: F401 - legacy public facade surface
import shutil  # noqa: F401 - legacy public facade surface
import subprocess  # noqa: F401 - legacy public facade surface
import time  # noqa: F401 - legacy public facade surface
import contextlib as _contextlib
from concurrent.futures import ThreadPoolExecutor  # noqa: F401
from copy import deepcopy  # noqa: F401 - legacy public facade surface
from datetime import datetime, timezone
from pathlib import Path  # noqa: F401 - legacy public facade surface
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
    build_run_identity as _build_run_identity,
    default_workers,
    render_cases as _render_cases_impl,
    resolve_execution_authority as _resolve_execution_authority_impl,
    resolve_case_matrix as _resolve_case_matrix_impl,
    resolve_dir as _resolve_dir_impl,
    resolve_path as _resolve_path_impl,
    resolve_workers,
)
from digitalmodel.workflows.openfoam_batch_routing import (
    prepare_batch as _prepare_batch_impl,
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
)
from digitalmodel.workflows.openfoam_batch_legacy_layout import (  # noqa: F401
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
    redact_external_rows as _redact_external_rows_impl,
    validate_result_policy_config as _validate_result_policy_config_impl,
    write_checkpoint as _write_checkpoint_impl,
    write_external_results as _write_external_results_impl,
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
_write_external_results = _write_external_results_impl
_redact_external_rows = _redact_external_rows_impl
_validate_result_policy_config = _validate_result_policy_config_impl
_prepare_batch = _prepare_batch_impl


def _execute_batch(batch: dict) -> tuple[list[dict], datetime, datetime]:
    started_at = datetime.now(timezone.utc)
    rendered = batch["rendered"]
    lock = batch["layout"].lock("run") if batch["layout"] else _contextlib.nullcontext()
    with lock:
        if batch["mode"] == "mpi":
            if len(rendered) != 1:
                raise ValueError(
                    "openfoam_run_batch mode: mpi runs exactly ONE case across "
                    f"ranks; the matrix produced {len(rendered)} cases. Use "
                    "mode: pool for a multi-case sweep."
                )
            rows = [_run_case_mpi(rendered[0], batch["run_settings"], batch["workers"], batch["mock"])]
        else:
            rows = _run_pool(rendered, batch["run_settings"], batch["workers"], batch["mock"])
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
    if batch["layout"]:
        rows = _redact_external_rows(rows, batch["layout"].root_path)
    summary_args = {
        "rows": rows, "mode": batch["mode"], "workers": batch["workers"],
        "mock": batch["mock"], "timeout_seconds": batch["timeout"],
        "started_at": started_at, "finished_at": finished_at,
    }
    if batch["layout"]:
        summary = _write_external_results(batch["layout"].output, **summary_args)
    else:
        _write_manifest(rows, manifest_path)
        summary = _write_summary(path=summary_path, **summary_args)
    if summary["failed"]:
        locator = (
            f"{batch['output']}/cases.csv" if batch["layout"] else str(manifest_path)
        )
        logger.warning(
            "openfoam_run_batch: {} of {} cases failed; see {}",
            summary["failed"],
            summary["total_cases"],
            locator,
        )
    settings = batch["settings"]
    settings["cases"] = rows
    settings["outputs"] = (
        {"manifest": f"{batch['output']}/cases.csv",
         "summary": f"{batch['output']}/batch_summary.json"}
        if batch["layout"] else
        {"manifest": str(manifest_path), "summary": str(summary_path)}
    )
    cfg["openfoam_run_batch"] = settings
    return cfg


def router(cfg: dict) -> dict:
    """Route a legacy or owned external request through the batch workflow."""
    batch = _prepare_batch(cfg)
    try:
        rows, started_at, finished_at = _execute_batch(batch)
        return _finalize_batch(cfg, batch, rows, started_at, finished_at)
    finally:
        if batch["layout"]:
            batch["layout"].close()
