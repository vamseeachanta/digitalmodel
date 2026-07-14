"""Legacy-compatible configuration helpers for OpenFOAM batch execution."""

from __future__ import annotations

import math
import os
from copy import deepcopy
from pathlib import Path
from typing import Any

from digitalmodel.workflows.parametric_run import _load_cases, _set_dotted

WORKER_CORE_FRACTION = 0.9
DEFAULT_MODE = "pool"
DEFAULT_MESH_UTILITY = "blockMesh"
DEFAULT_OUTPUT_DIR = "results"
DEFAULT_WORK_DIR = "batch_runs"
VALID_MODES = ("pool", "mpi")
DEFAULT_TIMEOUT_SECONDS = 43200

_RESERVED_ROW_KEYS = frozenset(
    {
        "index",
        "name",
        "status",
        "solver",
        "mock",
        "error",
        "case_dir",
        "wall_seconds",
        "mpi_plan",
    }
)


def default_workers(cpu_count: int | None = None) -> int:
    """Return the owner-policy worker count, floored and never below one."""
    cores = cpu_count if cpu_count is not None else (os.cpu_count() or 1)
    return max(1, math.floor(WORKER_CORE_FRACTION * cores))


def resolve_workers(run_settings: dict) -> int:
    """Resolve an explicit worker count or apply the owner default."""
    explicit = run_settings.get("workers")
    if explicit is None:
        return default_workers()
    workers = int(explicit)
    if workers < 1:
        raise ValueError(f"run_batch.workers must be >= 1, got {explicit}")
    return workers


def resolve_case_matrix(
    explicit: list[dict] | None, variants: dict, cfg_dir: Path
) -> list[dict[str, Any]]:
    """Resolve explicit or generated cases while preserving legacy order."""
    if explicit:
        if variants:
            raise ValueError(
                "openfoam_run_batch: give either cases: [...] or variants:, "
                "not both"
            )
        return [dict(case) for case in explicit]
    if variants:
        return _load_cases(variants, cfg_dir)
    return [{}]


def render_cases(
    base: dict,
    cases: list[dict[str, Any]],
    mapping: dict[str, str],
    work_dir: Path,
) -> list[dict[str, Any]]:
    """Render the deterministic per-case settings and work directories."""
    base_name = base.get("name") or f"{base['case_type']}_case"
    rendered: list[dict[str, Any]] = []
    for index, case in enumerate(cases):
        case_settings = deepcopy(base)
        params = {key: value for key, value in case.items() if key != "name"}
        collisions = _RESERVED_ROW_KEYS.intersection(params)
        if collisions:
            raise ValueError(
                "openfoam_run_batch: case parameter name(s) "
                f"{sorted(collisions)} collide with reserved manifest "
                "columns; rename the knob and route it onto the settings "
                "path via mapping: (e.g. solver_app: solver)"
            )
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


def resolve_path(path_value: str, cfg_dir: Path) -> Path:
    """Resolve a path relative to the input configuration directory."""
    path = Path(path_value)
    if path.is_absolute():
        return path
    return cfg_dir / path


def resolve_dir(path_value: str, cfg_dir: Path) -> Path:
    """Resolve and create a legacy batch directory."""
    resolved = resolve_path(path_value, cfg_dir)
    resolved.mkdir(parents=True, exist_ok=True)
    return resolved
