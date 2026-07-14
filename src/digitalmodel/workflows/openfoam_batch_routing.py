"""Engine-owned evidence collection and external OpenFOAM layout routing."""

from __future__ import annotations

import importlib.metadata
import os
import shutil
import subprocess
import sys
from pathlib import Path
from stat import S_ISDIR
from typing import Callable

from digitalmodel.workflows.openfoam_batch_config import (
    DEFAULT_MESH_UTILITY,
    DEFAULT_MODE,
    DEFAULT_OUTPUT_DIR,
    DEFAULT_TIMEOUT_SECONDS,
    DEFAULT_WORK_DIR,
    VALID_MODES,
    build_run_identity,
    render_cases,
    resolve_case_matrix,
    resolve_dir,
    resolve_execution_authority,
    resolve_workers,
)
from digitalmodel.workflows.openfoam_batch_execution import (
    SOLVER_ERROR_MESSAGE,
    solver_ready,
)
from digitalmodel.workflows.openfoam_batch_executables import ExecutableSet
from digitalmodel.workflows.openfoam_batch_layout import WorkLayout


class ExternalEvidenceError(ValueError, RuntimeError):
    """External evidence failure compatible with the prior fail-closed gate."""


def _compat_builder() -> Callable:
    facade = sys.modules.get("digitalmodel.workflows.openfoam_run_batch")
    if facade:
        return getattr(facade, "_build_run_identity", build_run_identity)
    return build_run_identity


def _request_path(cfg: dict) -> Path:
    value = cfg.get("_config_file_path")
    if not value:
        raise ExternalEvidenceError(
            "owned external layout requires engine config file evidence"
        )
    path = Path(value)
    if not path.is_absolute() or not path.is_file():
        raise ExternalEvidenceError("owned external layout config file evidence is invalid")
    return path


def _external_output(value: object, cfg_dir: Path) -> str:
    if not isinstance(value, str):
        raise ValueError("external output_dir must be input-local")
    path = Path(value)
    if path.is_absolute() or any(part in {"", ".", ".."} for part in path.parts):
        raise ValueError("external output_dir must be input-local")
    current = cfg_dir
    for part in path.parts:
        current /= part
        if current.is_symlink():
            raise ValueError("external output_dir must not contain symlinks")
    return value


def _referenced_inputs(settings: dict, cfg_dir: Path) -> dict[str, Path]:
    variants = settings.get("variants") or {}
    if variants.get("source") != "csv":
        return {}
    value = variants.get("file")
    if not isinstance(value, str):
        raise ValueError("CSV matrix requires engine-readable file evidence")
    path = Path(value)
    return {"case-matrix": path if path.is_absolute() else cfg_dir / path}


def _selected_tools(
    rendered: list[dict], run_settings: dict, mock: bool
) -> dict[str, Path]:
    if mock:
        return {}
    names = {
        item["settings"].get("mesh_utility", DEFAULT_MESH_UTILITY)
        for item in rendered
    }
    names.update(item["settings"].get("solver") for item in rendered)
    if run_settings.get("mode", "pool") == "pool":
        for item in rendered:
            settings = item["settings"]
            if settings.get("run_snappy", False):
                names.add("snappyHexMesh")
            if settings.get("run_set_fields", False):
                names.add("setFields")
            if settings.get("to_vtk", False):
                names.add("foamToVTK")
    if run_settings.get("mode", "pool") == "mpi":
        names.update({"decomposePar", "mpirun"})
        if run_settings.get("reconstruct", True):
            names.add("reconstructPar")
    selected = {}
    for name in sorted(item for item in names if item):
        resolved = shutil.which(name)
        if not resolved:
            raise ValueError(f"selected executable evidence is missing for {name}")
        selected[name] = Path(resolved)
    return selected


def _directory_identity(path: Path) -> tuple[int, int]:
    stat = path.stat(follow_symlinks=False)
    if not S_ISDIR(stat.st_mode):
        raise ValueError("external output_dir parent is invalid")
    return stat.st_dev, stat.st_ino


def _distribution_evidence() -> tuple[Path, str, Path | None]:
    package_root = Path(__file__).resolve().parents[1]
    distribution = importlib.metadata.distribution("digitalmodel")
    probe = subprocess.run(
        ["git", "-C", str(package_root), "rev-parse", "--show-toplevel"],
        capture_output=True,
        check=False,
    )
    root = None if probe.returncode == 0 else Path(distribution.locate_file(""))
    return package_root, distribution.version, root


def prepare_external(
    cfg: dict,
    settings: dict,
    run_settings: dict,
    authority,
    base: dict,
    cases: list[dict],
    mapping: dict,
    workers: int,
    mock: bool,
) -> tuple[WorkLayout, list[dict]]:
    """Build identity before atomically opening the owned external namespace."""
    if "run_identity" in run_settings:
        raise ValueError("caller-provided run identity is forbidden")
    request = _request_path(cfg)
    cfg_dir = request.parent
    cfg_dir_identity = _directory_identity(cfg_dir)
    work_name = run_settings.get("work_dir", DEFAULT_WORK_DIR)
    provisional = render_cases(base, cases, mapping, Path("unused"))
    package_root, version, distribution_root = _distribution_evidence()
    selected = _selected_tools(provisional, run_settings, mock)
    executables = ExecutableSet.capture(selected)
    identity = _compat_builder()(
        config_path=request,
        package_root=package_root,
        package_name="digitalmodel",
        package_version=version,
        effective_config=settings,
        referenced_inputs=_referenced_inputs(settings, cfg_dir),
        selected_executables=selected,
        visible_rank_count=os.cpu_count() or 1,
        dispatcher_rank_limit=workers,
        result_policy_version="result-policy-v1",
        work_layout_version="work-layout-v1",
        distribution_root=distribution_root,
    )
    if _directory_identity(cfg_dir) != cfg_dir_identity:
        raise ValueError("external output_dir parent changed during evidence collection")
    executables.validate_all()
    layout = WorkLayout.create(authority, identity, work_name)
    rendered = render_cases(base, cases, mapping, layout.work_path)
    for item in rendered:
        item.update(layout=layout, identity=identity, executables=executables)
    return layout, rendered


def prepare_batch(cfg: dict) -> dict:
    """Validate and prepare legacy or external execution without identity gaps."""
    settings = cfg.get("openfoam_run_batch") or {}
    cfg_dir = Path(cfg.get("_config_dir_path") or Path.cwd())
    run_settings = settings.get("run_batch") or {}
    authority = resolve_execution_authority(run_settings, cfg_dir)
    mode = run_settings.get("mode", DEFAULT_MODE)
    if mode not in VALID_MODES:
        raise ValueError(f"openfoam_run_batch run_batch.mode must be pool|mpi, got {mode}")
    mock = bool(run_settings.get("mock", False))
    workers = resolve_workers(run_settings)
    base = settings.get("base") or {}
    if not base.get("case_type"):
        raise ValueError("openfoam_run_batch.base.case_type is required")
    mesh = base.get("mesh_utility", DEFAULT_MESH_UTILITY)
    reconstruct = bool(run_settings.get("reconstruct", True))
    if not mock and not solver_ready(mode, mesh, base.get("solver"), reconstruct):
        raise RuntimeError(SOLVER_ERROR_MESSAGE)
    variants = settings.get("variants") or {}
    cases = resolve_case_matrix(settings.get("cases"), variants, cfg_dir)
    mapping = settings.get("mapping") or variants.get("mapping") or {}
    timeout = int(run_settings.get("timeout_seconds", DEFAULT_TIMEOUT_SECONDS))
    if authority.context == "legacy":
        results_dir = resolve_dir(
            run_settings.get("output_dir", DEFAULT_OUTPUT_DIR), cfg_dir
        )
        work = resolve_dir(run_settings.get("work_dir", DEFAULT_WORK_DIR), cfg_dir)
        layout, rendered = None, render_cases(base, cases, mapping, work)
    else:
        output = _external_output(
            run_settings.get("output_dir", DEFAULT_OUTPUT_DIR), cfg_dir
        )
        layout, rendered = prepare_external(
            cfg, settings, run_settings, authority, base, cases, mapping, workers, mock
        )
        results_dir = resolve_dir(output, cfg_dir)
    return {
        "settings": settings, "run_settings": run_settings, "mode": mode,
        "mock": mock, "workers": workers, "timeout": timeout,
        "authority": authority, "results_dir": results_dir,
        "layout": layout, "rendered": rendered,
    }
