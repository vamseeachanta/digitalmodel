"""Content-bound run identity for reusable external OpenFOAM work."""

from __future__ import annotations

from dataclasses import asdict, dataclass
import base64
import csv
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
    before = path.stat(follow_symlinks=False)
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    after = path.stat(follow_symlinks=False)
    if (before.st_dev, before.st_ino, before.st_size, before.st_mtime_ns) != (
            after.st_dev, after.st_ino, after.st_size, after.st_mtime_ns):
        raise ValueError(f"file changed while hashing: {path.name}")
    return digest.hexdigest()


def verify_wheel_record(package_root: Path, record_path: Path) -> dict[str, Any]:
    package_root = Path(package_root).resolve()
    record_path = Path(record_path).resolve()
    install_root = record_path.parent.parent
    verified: dict[str, str] = {}
    try:
        rows = list(csv.reader(record_path.read_text().splitlines()))
    except (OSError, csv.Error) as exc:
        raise ValueError("wheel RECORD is unreadable") from exc
    for row in rows:
        if len(row) != 3:
            raise ValueError("wheel RECORD row is malformed")
        relative, encoded_hash, encoded_size = row
        path = (install_root / relative).resolve()
        if path == record_path and not encoded_hash and not encoded_size:
            continue
        if not encoded_hash.startswith("sha256=") or not encoded_size.isdigit():
            raise ValueError("wheel RECORD lacks sha256 or size")
        if not path.is_file() or path.is_symlink() or path.stat().st_size != int(encoded_size):
            raise ValueError("wheel RECORD size/file mismatch")
        expected = encoded_hash.split("=", 1)[1]
        actual = base64.urlsafe_b64encode(bytes.fromhex(file_sha256(path))).rstrip(b"=").decode()
        if actual != expected:
            raise ValueError("wheel RECORD hash mismatch")
        verified[relative] = file_sha256(path)
    package_files = {path.relative_to(install_root).as_posix()
                     for path in package_root.rglob("*") if path.is_file()}
    if not package_files.issubset(verified):
        raise ValueError("wheel RECORD omits installed package files")
    content = _sha_bytes(_canonical(verified).encode())
    return {"package_name": "digitalmodel", "package_version": record_path.parent.name,
            "content_sha256": content, "wheel_record_verified": True}


def _file_record(role: str, path: Path, *, input_file: bool) -> dict[str, Any]:
    path = Path(path)
    label = "referenced input" if input_file else "selected executable"
    if not path.is_file() or path.is_symlink():
        raise ValueError(f"{label} is missing or symlinked: {path.name}")
    if input_file:
        _assert_git_clean_path(path)
    record = {"role": str(role), "size_bytes": path.stat().st_size,
              "content_sha256": file_sha256(path)}
    record["safe_relative_path" if input_file else "basename"] = path.name
    return record


def _assert_git_clean_path(path: Path) -> None:
    probe = subprocess.run(["git", "-C", str(path.parent), "rev-parse", "--is-inside-work-tree"],
                           capture_output=True, text=True)
    if probe.returncode:
        return
    status = subprocess.run(
        ["git", "-C", str(path.parent), "status", "--porcelain",
         "--untracked-files=all", "--", path.name], capture_output=True, text=True)
    if status.returncode or status.stdout.strip():
        raise ValueError(f"referenced input is dirty: {path.name}")


def _source_record(package_root: Path) -> dict[str, Any]:
    root = Path(package_root)
    tracked = subprocess.run(
        ["git", "-C", str(root), "ls-files", "-z", "--", "."],
        capture_output=True,
    )
    if tracked.returncode == 0 and tracked.stdout:
        status = subprocess.run(
            ["git", "-C", str(root), "status", "--porcelain", "--untracked-files=all", "--", "."],
            capture_output=True, text=True,
        )
        if status.returncode or status.stdout.strip():
            raise ValueError("digitalmodel package source is dirty")
        files = [root / Path(item.decode())
                 for item in tracked.stdout.split(b"\0") if item]
    else:
        records = sorted(root.parent.glob("digitalmodel-*.dist-info/RECORD"))
        if records:
            wheel = verify_wheel_record(root, records[0])
            return {"git_commit_sha": "installed-wheel", "tracked_tree_clean": True,
                    **wheel}
        files = sorted(path for path in root.rglob("*.py")
                       if path.is_file() and not path.is_symlink())
    digest = hashlib.sha256()
    for path in files:
        rel = path.relative_to(root).as_posix()
        digest.update(rel.encode() + b"\0" + bytes.fromhex(file_sha256(path)))
    try:
        version = importlib.metadata.version("digitalmodel")
    except importlib.metadata.PackageNotFoundError:
        version = "source"
    return {"git_commit_sha": _git_sha(root), "tracked_tree_clean": True,
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
