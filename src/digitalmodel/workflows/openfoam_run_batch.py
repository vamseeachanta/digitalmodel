"""OpenFOAM CFD batch workflow with optional owned external work roots."""

from __future__ import annotations

from datetime import datetime, timezone
import os
from pathlib import Path
import shutil
from typing import Any

from digitalmodel.workflows.openfoam_batch_config import (
    RESERVED_ROW_KEYS as _RESERVED_ROW_KEYS,
    default_workers,
    render_cases as _render_cases,
    resolve_batch_paths,
    resolve_case_matrix as _resolve_case_matrix,
    resolve_workers,
)
from digitalmodel.workflows.openfoam_batch_execution import (
    CHECKPOINT_FILENAME,
    DEFAULT_MESH_UTILITY,
    DEFAULT_TIMEOUT_SECONDS,
    build_case as _build_case,
    execute_mpi_plan as _execute_mpi_plan,
    load_checkpoint as _load_checkpoint,
    mpi_command_plan,
    run_case_mpi as _run_case_mpi,
    run_case_pool as _run_case_pool,
    run_cases,
    run_command as _run_command,
    set_start_from_latest_time as _set_start_from_latest_time,
    solve_serial as _solve_serial,
    write_checkpoint as _write_checkpoint,
    write_decompose_par_dict as _write_decompose_par_dict,
)
from digitalmodel.workflows.openfoam_batch_identity import build_run_identity, file_sha256
from digitalmodel.workflows.openfoam_batch_layout import WorkLayout
from digitalmodel.workflows.openfoam_batch_results import (
    redact_rows,
    row as _row,
    write_results,
)


# Keep the helper surface used by existing callers while implementation lives
# in focused modules.
__all__ = [
    "CHECKPOINT_FILENAME",
    "_RESERVED_ROW_KEYS",
    "_build_case",
    "_execute_mpi_plan",
    "_load_checkpoint",
    "_render_cases",
    "_resolve_case_matrix",
    "_row",
    "_run_case_mpi",
    "_run_case_pool",
    "_run_command",
    "_set_start_from_latest_time",
    "_solve_serial",
    "_write_checkpoint",
    "_write_decompose_par_dict",
    "default_workers",
    "mpi_command_plan",
    "resolve_workers",
    "router",
]


DEFAULT_MODE = "pool"
DEFAULT_OUTPUT_DIR = "results"
DEFAULT_WORK_DIR = "batch_runs"
VALID_MODES = ("pool", "mpi")
_RESULTS_ALLOWED_SUFFIXES = {".csv", ".json"}
SOLVER_ERROR_MESSAGE = (
    "requires-solver: OpenFOAM solver / utilities are not on PATH on this host. "
    "Dispatch to a solver-capable host or set run_batch.mock: true."
)


def router(cfg: dict) -> dict:
    settings, run_settings, base, cfg_dir = _settings(cfg)
    mode, mock, workers = _validate_run(run_settings, base)
    paths = resolve_batch_paths(run_settings, cfg_dir)
    layout, tool_bindings, input_bindings = _external_layout(
        cfg, settings, run_settings, base, paths, mode, workers, mock)
    work_dir = paths.legacy_work_dir if layout is None else layout.run_dir
    cases = _resolve_case_matrix(settings.get("cases"), settings.get("variants") or {}, cfg_dir)
    mapping = settings.get("mapping") or (settings.get("variants") or {}).get("mapping") or {}
    rendered = _render_cases(base, cases, mapping, work_dir)
    _verify_file_bindings(input_bindings, "referenced input")
    started = datetime.now(timezone.utc)
    if layout is None:
        rows = run_cases(rendered, run_settings, mode=mode, workers=workers,
                         mock=mock, builder=_build_case)
    else:
        with layout.lock("run"):
            rows = run_cases(rendered, run_settings, mode=mode, workers=workers,
                             mock=mock, layout=layout, builder=_build_case,
                             tool_bindings=tool_bindings)
    safe_rows = redact_rows(rows, paths.operator_root)
    outputs = write_results(safe_rows, paths.output_dir, mode=mode, workers=workers,
                            mock=mock, timeout_seconds=int(run_settings.get(
                                "timeout_seconds", DEFAULT_TIMEOUT_SECONDS)),
                            started_at=started, finished_at=datetime.now(timezone.utc),
                            extensions=run_settings.get("result_extensions"))
    settings["cases"], settings["outputs"] = safe_rows, outputs
    cfg["openfoam_run_batch"] = settings
    return cfg


def _settings(cfg: dict) -> tuple[dict, dict, dict, Path]:
    settings = cfg.get("openfoam_run_batch") or {}
    run_settings = settings.get("run_batch") or {}
    base = settings.get("base") or {}
    if not base.get("case_type"):
        raise ValueError("openfoam_run_batch.base.case_type is required")
    cfg_dir = Path(cfg.get("_config_dir_path") or Path.cwd())
    return settings, run_settings, base, cfg_dir


def _validate_run(run_settings: dict, base: dict) -> tuple[str, bool, int]:
    mode = run_settings.get("mode", DEFAULT_MODE)
    if mode not in VALID_MODES:
        raise ValueError(f"openfoam_run_batch run_batch.mode must be pool|mpi, got {mode}")
    mock, workers = bool(run_settings.get("mock", False)), resolve_workers(run_settings)
    if not mock and not _solver_ready(mode, base.get("mesh_utility", DEFAULT_MESH_UTILITY),
                                      base.get("solver"), bool(run_settings.get("reconstruct", True))):
        raise RuntimeError(SOLVER_ERROR_MESSAGE)
    return mode, mock, workers


def _external_layout(cfg: dict, settings: dict, run_settings: dict, base: dict,
                     paths, mode: str, workers: int, mock: bool
                     ) -> tuple[WorkLayout | None, dict[str, tuple[Path, str]],
                                list[tuple[Path, str]]]:
    if not paths.external:
        paths.legacy_work_dir.mkdir(parents=True, exist_ok=True)
        return None, {}, []
    if not mock and not base.get("solver"):
        raise ValueError("external real runs require base.solver for executable identity")
    inputs = _referenced_inputs(cfg, settings, paths.cfg_dir)
    tools = [] if mock else _selected_tools(mode, base, run_settings)
    identity = build_run_identity(
        effective_config=settings, referenced_inputs=inputs,
        selected_executables=tools, visible_rank_count=os.cpu_count() or 1,
        dispatcher_rank_limit=workers,
        package_root=Path(__file__).resolve().parents[1],
    )
    bindings = {item[0]: (item[1], file_sha256(item[1])) for item in tools}
    input_bindings = [(item[1], file_sha256(item[1])) for item in inputs]
    return (WorkLayout.create(paths.operator_root, paths.namespace, identity.identity_sha256,
                              identity.as_dict()),
            bindings, input_bindings)


def _verify_file_bindings(bindings: list[tuple[Path, str]], label: str) -> None:
    for path, expected in bindings:
        if file_sha256(path) != expected:
            raise RuntimeError(f"{label} {path.name!r} changed before execution")


def _referenced_inputs(cfg: dict, settings: dict, cfg_dir: Path) -> list[tuple[str, Path]]:
    found = []
    config_file = cfg.get("_config_file_path")
    if config_file and Path(config_file).is_file():
        found.append(("request", Path(config_file)))
    for role, value in _walk_strings(settings):
        path = Path(value)
        candidate = path if path.is_absolute() else cfg_dir / path
        if candidate.is_file():
            found.append((role, candidate))
        elif candidate.is_dir() and not candidate.is_symlink():
            for asset in candidate.rglob("*"):
                if asset.is_file():
                    found.append((f"{role}/{asset.relative_to(candidate).as_posix()}", asset))
    return list({(role, path.resolve()) for role, path in found})


def _walk_strings(value: Any, prefix: str = "config"):
    if isinstance(value, dict):
        for key, item in value.items():
            yield from _walk_strings(item, f"{prefix}.{key}")
    elif isinstance(value, list):
        for index, item in enumerate(value):
            yield from _walk_strings(item, f"{prefix}[{index}]")
    elif isinstance(value, str):
        yield prefix, value


def _selected_tools(mode: str, base: dict, run_settings: dict) -> list[tuple[str, Path]]:
    names = [base.get("mesh_utility", DEFAULT_MESH_UTILITY), base.get("solver")]
    if mode == "mpi":
        names += ["decomposePar", "mpirun"]
        if base.get("run_set_fields"):
            names.append("setFields")
        if bool(run_settings.get("reconstruct", True)):
            names.append("reconstructPar")
    else:
        if base.get("run_snappy"):
            names.append("snappyHexMesh")
        if base.get("merge_meshes_source"):
            names.append("mergeMeshes")
        if base.get("run_topo_set"):
            names.append("topoSet")
        if base.get("subset_mesh_set") or base.get("subset_mesh_patch"):
            names.append("subsetMesh")
        if base.get("run_set_fields"):
            names.append("setFields")
        if base.get("to_vtk"):
            names.append("foamToVTK")
    return [(name, Path(shutil.which(name) or "")) for name in names if name]


def _solver_ready(mode: str, mesh_utility: str, solver: str | None,
                  reconstruct: bool = True) -> bool:
    required = [mesh_utility] + ([solver] if solver else [])
    if mode == "mpi":
        required += ["decomposePar", "mpirun"] + (["reconstructPar"] if reconstruct else [])
    return all(shutil.which(name) is not None for name in required)


def _clean_case_dir(case_dir: Path) -> None:
    if case_dir.is_dir():
        shutil.rmtree(case_dir, ignore_errors=True)


def _prune_processor_dirs(case_dir: Path) -> None:
    for path in case_dir.glob("processor*") if case_dir.is_dir() else ():
        shutil.rmtree(path, ignore_errors=True)


def _has_processor_dirs(case_dir: Path) -> bool:
    return case_dir.is_dir() and any(case_dir.glob("processor*"))
