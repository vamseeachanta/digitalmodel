"""Configuration authority for OpenFOAM batch work placement."""

from __future__ import annotations

from dataclasses import dataclass
from copy import deepcopy
import math
import os
from pathlib import Path
import re
from typing import Mapping

from digitalmodel.workflows.parametric_run import _load_cases, _set_dotted


WORKER_CORE_FRACTION = 0.9
HOSTED_CONTEXT = "hosted-deckhand"
TRUSTED_LOCAL_CONTEXT = "trusted-local"
DEFAULT_OUTPUT_DIR = "results"
DEFAULT_WORK_DIR = "batch_runs"
_COMPONENT = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.-]*$")
RESERVED_ROW_KEYS = frozenset({"index", "name", "status", "solver", "mock",
                               "error", "case_dir", "wall_seconds", "mpi_plan"})


@dataclass(frozen=True)
class BatchPaths:
    execution_context: str
    cfg_dir: Path
    output_dir: Path
    operator_root: Path | None
    namespace: str
    legacy_work_dir: Path | None

    @property
    def external(self) -> bool:
        return self.operator_root is not None


def default_workers(cpu_count: int | None = None) -> int:
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


def resolve_case_matrix(explicit: list[dict] | None, variants: dict,
                        cfg_dir: Path) -> list[dict]:
    if explicit:
        if variants:
            raise ValueError("openfoam_run_batch: give either cases or variants, not both")
        return [dict(item) for item in explicit]
    return _load_cases(variants, cfg_dir) if variants else [{}]


def render_cases(base: dict, cases: list[dict], mapping: dict[str, str],
                 work_dir: Path) -> list[dict]:
    base_name = base.get("name") or f"{base['case_type']}_case"
    rendered = []
    for index, case in enumerate(cases):
        settings = deepcopy(base)
        params = {key: value for key, value in case.items() if key != "name"}
        collisions = RESERVED_ROW_KEYS.intersection(params)
        if collisions:
            raise ValueError(
                "case parameter names collide with reserved manifest columns: "
                f"{sorted(collisions)}"
            )
        for name, value in params.items():
            _set_dotted(settings, mapping.get(name, name), value)
        name = case.get("name") or f"{base_name}_{index:03d}"
        settings["name"] = name
        rendered.append({"index": index, "name": name, "case": params,
                         "settings": settings, "work_dir": work_dir / name})
    return rendered


def validate_namespace(value: object) -> str:
    raw = str(value or "default")
    if "\\" in raw or "//" in raw or any(ord(ch) < 32 for ch in raw):
        raise ValueError("run_batch.work_root_namespace is malformed")
    parts = raw.split("/")
    if not parts or any(part in {"", ".", ".."} or not _COMPONENT.fullmatch(part) for part in parts):
        raise ValueError("run_batch.work_root_namespace is malformed")
    return "/".join(parts)


def resolve_batch_paths(
    run_settings: dict, cfg_dir: Path, *, env: Mapping[str, str] | None = None
) -> BatchPaths:
    environment = os.environ if env is None else env
    cfg_dir = cfg_dir.resolve()
    output_dir = _resolve(run_settings.get("output_dir", DEFAULT_OUTPUT_DIR), cfg_dir)
    hosted = environment.get("DIGITALMODEL_EXECUTION_CONTEXT") == HOSTED_CONTEXT
    if hosted:
        if "work_root" in run_settings:
            raise ValueError("hosted run_batch.work_root is forbidden; operator environment owns the root")
        root = environment.get("DIGITALMODEL_WORK_ROOT", "")
        operator_root = _validated_root(root, "DIGITALMODEL_WORK_ROOT")
        output_dir = _input_local_output(run_settings, cfg_dir)
        return BatchPaths(HOSTED_CONTEXT, cfg_dir, output_dir, operator_root,
                          validate_namespace(run_settings.get("work_root_namespace")), None)
    context = run_settings.get("execution_context")
    if context == TRUSTED_LOCAL_CONTEXT:
        operator_root = _validated_root(run_settings.get("work_root", ""), "run_batch.work_root")
        if _inside_git_checkout(operator_root):
            raise ValueError("run_batch.work_root must be outside every Git checkout")
        output_dir = _input_local_output(run_settings, cfg_dir)
        return BatchPaths(TRUSTED_LOCAL_CONTEXT, cfg_dir, output_dir, operator_root,
                          validate_namespace(run_settings.get("work_root_namespace")), None)
    if context not in (None, ""):
        raise ValueError(f"unsupported run_batch.execution_context {context!r}")
    work_dir = _resolve(run_settings.get("work_dir", DEFAULT_WORK_DIR), cfg_dir)
    return BatchPaths("legacy", cfg_dir, output_dir, None, "", work_dir)


def _resolve(value: object, base: Path) -> Path:
    path = Path(str(value))
    return path if path.is_absolute() else base / path


def _input_local_output(run_settings: dict, cfg_dir: Path) -> Path:
    raw = Path(str(run_settings.get("output_dir", DEFAULT_OUTPUT_DIR)))
    candidate = raw if raw.is_absolute() else cfg_dir / raw
    resolved = candidate.resolve()
    try:
        resolved.relative_to(cfg_dir)
    except ValueError as exc:
        raise ValueError("external-mode output_dir must remain input-local") from exc
    return resolved


def _validated_root(value: object, label: str) -> Path:
    path = Path(str(value))
    if not str(value) or not path.is_absolute() or not path.is_dir():
        raise ValueError(f"{label} must name a precreated absolute directory")
    if _has_symlink_component(path):
        raise ValueError(f"{label} may not contain symlink components")
    if not os.access(path, os.W_OK):
        raise ValueError(f"{label} is not writable")
    return path.resolve()


def _has_symlink_component(path: Path) -> bool:
    current = Path(path.anchor)
    for part in path.parts[1:]:
        current /= part
        if current.is_symlink():
            return True
    return False


def _inside_git_checkout(path: Path) -> bool:
    current = path.resolve()
    for candidate in (current, *current.parents):
        if (candidate / ".git").exists():
            return True
    return False
