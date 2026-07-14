"""Content-bound run identity for reusable external OpenFOAM work."""

from __future__ import annotations

from dataclasses import asdict, dataclass
import hashlib
import importlib.metadata
import json
from pathlib import Path
import subprocess
from typing import Any, Iterable


IDENTITY_KIND = "digitalmodel-openfoam-batch"
RESULT_POLICY_VERSION = "result-policy-v1"
WORK_LAYOUT_VERSION = "work-layout-v1"


@dataclass(frozen=True)
class RunIdentity:
    schema_version: int
    identity_kind: str
    source: dict[str, Any]
    effective_config_sha256: str
    referenced_inputs: list[dict[str, Any]]
    selected_executables: list[dict[str, Any]]
    host_capabilities: dict[str, int]
    result_policy_version: str
    work_layout_version: str
    identity_sha256: str

    def canonical_json(self) -> str:
        return _canonical(asdict(self))

    def as_dict(self) -> dict[str, Any]:
        return asdict(self)


def build_run_identity(
    *, effective_config: dict[str, Any],
    referenced_inputs: Iterable[tuple[str, Path]],
    selected_executables: Iterable[tuple[str, Path]],
    visible_rank_count: int, dispatcher_rank_limit: int,
    package_root: Path,
) -> RunIdentity:
    inputs = [_file_record(role, path, input_file=True) for role, path in referenced_inputs]
    tools = [_file_record(role, path, input_file=False) for role, path in selected_executables]
    source = _source_record(package_root)
    body = {
        "schema_version": 1,
        "identity_kind": IDENTITY_KIND,
        "source": source,
        "effective_config_sha256": _sha_bytes(_canonical(effective_config).encode()),
        "referenced_inputs": sorted(inputs, key=lambda item: (item["role"], item.get("safe_relative_path", ""))),
        "selected_executables": sorted(tools, key=lambda item: (item["role"], item["basename"])),
        "host_capabilities": {"visible_rank_count": int(visible_rank_count),
                              "dispatcher_rank_limit": int(dispatcher_rank_limit)},
        "result_policy_version": RESULT_POLICY_VERSION,
        "work_layout_version": WORK_LAYOUT_VERSION,
    }
    digest = _sha_bytes(_canonical(body).encode())
    return RunIdentity(**body, identity_sha256=digest)


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _file_record(role: str, path: Path, *, input_file: bool) -> dict[str, Any]:
    path = Path(path)
    label = "referenced input" if input_file else "selected executable"
    if not path.is_file() or path.is_symlink():
        raise ValueError(f"{label} is missing or symlinked: {path.name}")
    record = {"role": str(role), "size_bytes": path.stat().st_size,
              "content_sha256": file_sha256(path)}
    record["safe_relative_path" if input_file else "basename"] = path.name
    return record


def _source_record(package_root: Path) -> dict[str, Any]:
    root = Path(package_root)
    files = sorted(path for path in root.rglob("*.py") if path.is_file() and not path.is_symlink())
    digest = hashlib.sha256()
    for path in files:
        rel = path.relative_to(root).as_posix()
        digest.update(rel.encode() + b"\0" + bytes.fromhex(file_sha256(path)))
    try:
        version = importlib.metadata.version("digitalmodel")
    except importlib.metadata.PackageNotFoundError:
        version = "source"
    return {"git_commit_sha": _git_sha(root), "tracked_tree_clean": _git_clean(root),
            "package_name": "digitalmodel", "package_version": version,
            "content_sha256": digest.hexdigest()}


def _git_sha(path: Path) -> str:
    out = subprocess.run(["git", "-C", str(path), "rev-parse", "HEAD"], capture_output=True, text=True)
    return out.stdout.strip() if out.returncode == 0 else "not-a-git-checkout"


def _git_clean(path: Path) -> bool:
    out = subprocess.run(["git", "-C", str(path), "status", "--porcelain", "--untracked-files=no"],
                         capture_output=True, text=True)
    return out.returncode == 0 and not out.stdout.strip()


def _canonical(value: Any) -> str:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True,
                      allow_nan=False)


def _sha_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()
