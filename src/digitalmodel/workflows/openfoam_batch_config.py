"""Legacy-compatible configuration helpers for OpenFOAM batch execution."""

from __future__ import annotations

import base64
import csv
import hashlib
import json
import math
import os
import subprocess
import sys
from copy import deepcopy
from pathlib import Path
from typing import Any, Callable, Mapping, NamedTuple

from digitalmodel.workflows.parametric_run import _load_cases, _set_dotted

WORKER_CORE_FRACTION = 0.9
DEFAULT_MODE = "pool"
DEFAULT_MESH_UTILITY = "blockMesh"
DEFAULT_OUTPUT_DIR = "results"
DEFAULT_WORK_DIR = "batch_runs"
VALID_MODES = ("pool", "mpi")
DEFAULT_TIMEOUT_SECONDS = 43200

_RESERVED_ROW_KEYS = frozenset(
    "index name status solver mock error case_dir wall_seconds mpi_plan".split()
)


class ExecutionAuthority(NamedTuple):
    context: str
    root: Path | None
    namespace: Path | None


def canonical_json_bytes(value: Any) -> bytes:
    encoded = json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
        allow_nan=False,
    )
    return (encoded + "\n").encode("ascii")


def _contains_symlink(path: Path) -> bool:
    current = Path(path.anchor)
    for component in path.parts[1:]:
        current /= component
        if current.is_symlink():
            return True
    return False


def _validate_root(value: str | None, label: str) -> Path:
    if not value:
        raise ValueError(f"{label} is required")
    root = Path(value)
    if not root.is_absolute():
        raise ValueError(f"{label} must be an absolute path")
    if _contains_symlink(root):
        raise ValueError(f"{label} must not contain a symlink")
    if not root.is_dir() or not os.access(root, os.W_OK):
        raise ValueError(f"{label} must be a precreated writable directory")
    return root.resolve()


def _namespace(value: object, root: Path) -> Path:
    raw = "default" if value is None else str(value)
    parts = raw.split("/")
    invalid = (
        not raw
        or Path(raw).is_absolute()
        or "\\" in raw
        or any(part in {"", ".", ".."} for part in parts)
        or any(ord(char) < 32 or ord(char) == 127 for char in raw)
    )
    if invalid:
        raise ValueError("work_root_namespace must contain portable components")
    namespace = Path(*parts)
    if _contains_symlink(root / namespace):
        raise ValueError("work_root_namespace must not contain a symlink")
    return namespace


def _inside_git_checkout(path: Path) -> bool:
    probe = subprocess.run(
        ["git", "-C", str(path), "rev-parse", "--show-toplevel"],
        capture_output=True,
        text=True,
        check=False,
    )
    return probe.returncode == 0


def resolve_execution_authority(
    run_settings: Mapping[str, Any],
    cfg_dir: Path,
    environment: Mapping[str, str] | None = None,
) -> ExecutionAuthority:
    env = os.environ if environment is None else environment
    marker = env.get("DIGITALMODEL_EXECUTION_CONTEXT")
    requested = run_settings.get("execution_context")
    if marker:
        if marker != "hosted-deckhand":
            raise ValueError("DIGITALMODEL_EXECUTION_CONTEXT is invalid")
        if requested is not None or "work_root" in run_settings:
            raise ValueError("hosted YAML cannot set execution_context or work_root")
        root = _validate_root(
            env.get("DIGITALMODEL_WORK_ROOT"), "DIGITALMODEL_WORK_ROOT"
        )
        return ExecutionAuthority(
            marker, root, _namespace(run_settings.get("work_root_namespace"), root)
        )
    if requested == "trusted-local":
        root = _validate_root(run_settings.get("work_root"), "run_batch.work_root")
        if _inside_git_checkout(root):
            raise ValueError("run_batch.work_root must be outside every Git checkout")
        return ExecutionAuthority(
            requested, root, _namespace(run_settings.get("work_root_namespace"), root)
        )
    if requested is not None:
        raise ValueError("run_batch.execution_context must be trusted-local")
    if "work_root" in run_settings or "work_root_namespace" in run_settings:
        raise ValueError("external work root requires explicit trusted-local context")
    return ExecutionAuthority("legacy", None, None)


def _sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _git_output(repo: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", "-C", str(repo), *args],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode:
        raise ValueError("source identity requires a Git checkout")
    return result.stdout.strip()


def _source_package(package_root: Path, candidates: list[Path]) -> tuple[dict, Path]:
    repo = Path(_git_output(package_root, "rev-parse", "--show-toplevel"))
    relative = [str(path.resolve().relative_to(repo.resolve())) for path in candidates]
    if _git_output(repo, "status", "--porcelain=v1", "--", *relative):
        raise ValueError("source identity candidate paths must be clean")
    package_rel = str(package_root.resolve().relative_to(repo.resolve()))
    tracked = _git_output(repo, "ls-files", "-z", "--", package_rel).split("\0")
    records = _actual_records(repo, [item for item in tracked if item], repo)
    if not records:
        raise ValueError("source package must contain tracked files")
    source = {
        "git_commit_sha": _git_output(repo, "rev-parse", "HEAD"),
        "tracked_tree_clean": True,
        "content_sha256": _sha256(canonical_json_bytes(records)),
    }
    return source, repo


def _actual_records(base: Path, names: list[str], relative_to: Path) -> list[dict]:
    records = []
    for name in sorted(names):
        path = base / name
        if not path.is_file() or path.is_symlink():
            raise ValueError(f"identity input is missing or unsafe: {name}")
        data = path.read_bytes()
        records.append(
            {
                "safe_relative_path": path.relative_to(relative_to).as_posix(),
                "size_bytes": len(data),
                "content_sha256": _sha256(data),
            }
        )
    return records


def _record_hash(encoded: str) -> str:
    algorithm, value = encoded.split("=", 1)
    if algorithm != "sha256":
        raise ValueError("wheel RECORD must use sha256")
    padded = value + "=" * (-len(value) % 4)
    return base64.urlsafe_b64decode(padded).hex()


def _wheel_package(package_root: Path, distribution_root: Path) -> dict:
    record_glob = f"{package_root.name.replace('-', '_')}-*.dist-info/RECORD"
    records = list(distribution_root.glob(record_glob))
    if len(records) != 1:
        raise ValueError("wheel RECORD is missing or ambiguous")
    with records[0].open(newline="") as stream:
        rows = {row[0]: row[1:] for row in csv.reader(stream) if row}
    package_files = [path for path in package_root.rglob("*") if path.is_file()]
    actual_names = {
        path.relative_to(distribution_root).as_posix() for path in package_files
    }
    recorded_names = {name for name in rows if name.startswith(package_root.name + "/")}
    if actual_names - recorded_names:
        raise ValueError("wheel package contains an unrecorded file")
    if recorded_names - actual_names:
        raise ValueError("wheel RECORD references a missing package file")
    actual = _actual_records(
        distribution_root, sorted(recorded_names), distribution_root
    )
    for item in actual:
        digest, size = rows[item["safe_relative_path"]]
        if (
            not digest
            or int(size) != item["size_bytes"]
            or _record_hash(digest) != item["content_sha256"]
        ):
            raise ValueError("wheel RECORD does not match actual package bytes")
    return {
        "git_commit_sha": None,
        "tracked_tree_clean": None,
        "content_sha256": _sha256(canonical_json_bytes(actual)),
    }


def _input_records(
    config_path: Path | None, inputs: Mapping[str, Path], repo: Path | None
) -> list[dict]:
    entries = dict(inputs)
    if config_path is not None:
        entries = {"request": config_path, **entries}
    records = []
    for role, path in sorted(entries.items()):
        data = Path(path).read_bytes()
        safe_path = (
            Path(path).name
            if repo is None
            else Path(path).resolve().relative_to(repo.resolve()).as_posix()
        )
        records.append(
            {
                "role": role,
                "safe_relative_path": safe_path,
                "size_bytes": len(data),
                "content_sha256": _sha256(data),
            }
        )
    return records


def _executable_records(executables: Mapping[str, Path]) -> list[dict]:
    records = []
    for role, path in sorted(executables.items()):
        candidate = Path(path)
        if not candidate.is_file() or candidate.is_symlink():
            raise ValueError(f"selected executable for {role} is missing or unsafe")
        records.append(
            {
                "role": role,
                "basename": candidate.name,
                "content_sha256": _sha256(candidate.read_bytes()),
            }
        )
    return records


def build_run_identity(
    *,
    config_path: Path | None,
    package_root: Path,
    package_name: str,
    package_version: str,
    effective_config: Mapping[str, Any],
    referenced_inputs: Mapping[str, Path],
    selected_executables: Mapping[str, Path],
    visible_rank_count: int,
    dispatcher_rank_limit: int,
    result_policy_version: str,
    work_layout_version: str,
    distribution_root: Path | None = None,
) -> dict:
    if visible_rank_count < 1 or not 1 <= dispatcher_rank_limit <= visible_rank_count:
        raise ValueError("rank ceilings must be positive and dispatcher <= visible")
    candidates = [Path(path) for path in referenced_inputs.values()]
    if config_path is not None:
        candidates.append(Path(config_path))
    if distribution_root is None:
        source, repo = _source_package(
            Path(package_root), candidates + [Path(package_root)]
        )
    else:
        source, repo = _wheel_package(Path(package_root), Path(distribution_root)), None
    source.update(package_name=package_name, package_version=package_version)
    inputs = _input_records(config_path, referenced_inputs, repo)
    identity = {
        "schema_version": 1,
        "identity_kind": "openfoam-run-v1",
        "source": source,
        "effective_config_sha256": _sha256(canonical_json_bytes(effective_config)),
        "referenced_inputs": inputs,
        "selected_executables": _executable_records(selected_executables),
        "host_capabilities": {
            "visible_rank_count": int(visible_rank_count),
            "dispatcher_rank_limit": int(dispatcher_rank_limit),
        },
        "result_policy_version": result_policy_version,
        "work_layout_version": work_layout_version,
    }
    identity["identity_sha256"] = _sha256(canonical_json_bytes(identity))
    return identity


def _compat(name: str, fallback: Callable) -> Callable:
    """Honor a legacy facade monkeypatch without importing it recursively."""
    facade = sys.modules.get("digitalmodel.workflows.openfoam_run_batch")
    return getattr(facade, name, fallback) if facade else fallback


def default_workers(cpu_count: int | None = None) -> int:
    """Return the owner-policy worker count, floored and never below one."""
    cores = cpu_count if cpu_count is not None else (os.cpu_count() or 1)
    return max(1, math.floor(WORKER_CORE_FRACTION * cores))


def resolve_workers(run_settings: dict) -> int:
    """Resolve an explicit worker count or apply the owner default."""
    explicit = run_settings.get("workers")
    if explicit is None:
        return _compat("default_workers", default_workers)()
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
                "openfoam_run_batch: give either cases: [...] or variants:, not both"
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
    resolved = _compat("_resolve_path", resolve_path)(path_value, cfg_dir)
    resolved.mkdir(parents=True, exist_ok=True)
    return resolved
