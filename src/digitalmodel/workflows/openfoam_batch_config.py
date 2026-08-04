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
DEFAULT_MESH_UTILITY = "blockMesh"
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


def validate_workers(requested: object, *, visible_rank_count: int,
                     dispatcher_rank_limit: int | None = None) -> int:
    """Return the validated rank count, or reject before any mutation.

    A dispatcher limit is a host capability ceiling, never a second request.
    The bool check precedes the int check because ``int(True)`` is 1, so a
    boolean would otherwise be accepted silently as a one-rank run.
    """
    if isinstance(requested, bool):
        raise TypeError(f"run_batch.workers must be an integer, got {requested!r}")
    if not isinstance(requested, int):
        raise ValueError(f"run_batch.workers must be an integer, got {requested!r}")
    if requested < 1:
        raise ValueError(f"run_batch.workers must be >= 1, got {requested}")
    ceiling = visible_rank_count
    if dispatcher_rank_limit is not None:
        ceiling = min(ceiling, dispatcher_rank_limit)
    if requested > ceiling:
        raise ValueError(
            f"run_batch.workers {requested} exceeds the available rank ceiling {ceiling}")
    return requested


def resolve_case_matrix(explicit: list[dict] | None, variants: dict,
                        cfg_dir: Path) -> list[dict]:
    if explicit:
        if variants:
            raise ValueError("openfoam_run_batch: give either cases or variants, not both")
        return [dict(item) for item in explicit]
    return _load_cases(variants, cfg_dir) if variants else [{}]


def _canonical(base: dict) -> bool:
    return "case_definition" in base or "execution" in base


def base_view(base: dict) -> dict:
    """Return the solver/utility view of either base form.

    The batch router picks executables and checks solver readiness from a flat
    set of keys. A canonical base carries the same facts under
    ``case_definition.authored`` and ``execution``, so both forms are reduced
    here rather than teaching every call site about the schema.
    """
    if not _canonical(base):
        return base
    authored = (base.get("case_definition") or {}).get("authored") or {}
    execution = base.get("execution") or {}
    return {
        "case_type": authored.get("case_type"),
        "solver": authored.get("solver"),
        "mesh_utility": execution.get("mesh_utility", DEFAULT_MESH_UTILITY),
        "run_snappy": execution.get("run_snappy", False),
        "run_set_fields": execution.get("run_set_fields", False),
        "to_vtk": execution.get("to_vtk", False),
    }


def _base_name(base: dict) -> str:
    """Default case-name stem for either base form."""
    if _canonical(base):
        authored = (base.get("case_definition") or {}).get("authored") or {}
        stem = authored.get("name") or authored.get("case_type")
        if not stem:
            raise ValueError(
                "openfoam_run_batch.base.case_definition.authored requires "
                "case_type"
            )
        return str(stem)
    if not base.get("case_type"):
        raise ValueError("openfoam_run_batch.base.case_type is required")
    return str(base.get("name") or f"{base['case_type']}_case")


def _validate_mapping_target(target: str) -> None:
    """Refuse a knob target that is not a single accepted mutable leaf.

    Mapping onto a container, the schema discriminator, or a path the schema
    does not accept would either be silently ignored downstream or corrupt the
    case-source union, so both are rejected before any case is rendered.
    """
    from digitalmodel.solvers.openfoam.case_definition import (
        ACCEPTED_LEAF_CONSUMERS,
    )

    if target in ACCEPTED_LEAF_CONSUMERS:
        return
    prefixes = {
        parent
        for leaf in ACCEPTED_LEAF_CONSUMERS
        for parent in _ancestors(leaf)
    }
    if target in prefixes:
        raise ValueError(
            f"openfoam_run_batch mapping target {target!r} names a container, "
            "not a single accepted leaf"
        )
    raise ValueError(
        f"openfoam_run_batch mapping target {target!r} is not an accepted "
        "case-definition leaf"
    )


def _ancestors(dotted: str) -> list[str]:
    parts = dotted.split(".")
    return [".".join(parts[:index]) for index in range(1, len(parts))]


def render_cases(base: dict, cases: list[dict], mapping: dict[str, str],
                 work_dir: Path) -> list[dict]:
    base_name = _base_name(base)
    canonical = _canonical(base)
    if canonical:
        for target in mapping.values():
            _validate_mapping_target(target)
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
            target = mapping.get(name, name)
            if canonical and name not in mapping:
                _validate_mapping_target(target)
            _set_dotted(settings, target, value)
        name = case.get("name") or f"{base_name}_{index:03d}"
        if canonical:
            # The canonical schema owns the case name; a root-level "name"
            # would be refused as an unknown key by the generic adapter.
            settings["case_definition"]["authored"]["name"] = name
        else:
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
