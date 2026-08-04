"""Content-addressed indexes for completed OpenFOAM artifact trees."""

from __future__ import annotations

import os
import re
import stat
from dataclasses import asdict, dataclass
from decimal import Decimal, InvalidOperation
from hashlib import sha256
from pathlib import Path, PurePosixPath
from typing import Sequence

TREE_DOMAIN = b"dm-artifact-tree-v1"
ARTIFACT_ID_DOMAIN = b"dm-artifact-id-v1"
GENERATION_DOMAIN = b"dm-generation-id-v1"
COMMIT_DOMAIN = b"dm-commit-v1"
ARTIFACT_SCHEMA_VERSION = 1
ARTIFACT_KINDS = (
    "mesh_tree",
    "field_tree",
    "vtk_tree",
    "postprocessing_tree",
)

_TIME_NAME = re.compile(r"[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?\Z")
_DIRECTORY_FLAGS = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW
_FILE_FLAGS = os.O_RDONLY | os.O_NOFOLLOW
_UINT64_MAX = (1 << 64) - 1


class ArtifactIndexError(ValueError):
    """Raised when an artifact tree or index value is unsafe or unstable."""


def _uint64(value: int, label: str) -> bytes:
    if not isinstance(value, int) or isinstance(value, bool):
        raise ArtifactIndexError(f"{label} must be an integer")
    if not 0 <= value <= _UINT64_MAX:
        raise ArtifactIndexError(f"{label} is outside uint64 range")
    return value.to_bytes(8, "big")


def frame(data: bytes) -> bytes:
    """Prefix bytes with their unsigned 64-bit big-endian length."""
    if not isinstance(data, bytes):
        raise ArtifactIndexError("framed values must be bytes")
    return _uint64(len(data), "frame length") + data


def _safe_relative(value: str, label: str) -> None:
    try:
        encoded = value.encode("utf-8")
    except UnicodeEncodeError as error:
        raise ArtifactIndexError(f"{label} is not valid UTF-8") from error
    path = PurePosixPath(value)
    if not encoded or path.is_absolute() or "\\" in value:
        raise ArtifactIndexError(f"{label} must be a safe relative POSIX path")
    if any(part in ("", ".", "..") for part in value.split("/")):
        raise ArtifactIndexError(f"{label} must be a safe relative POSIX path")


def _sha256_bytes(value: str, label: str) -> bytes:
    if len(value) != 64 or value.lower() != value:
        raise ArtifactIndexError(f"{label} must be lowercase SHA-256 hex")
    try:
        decoded = bytes.fromhex(value)
    except ValueError as error:
        raise ArtifactIndexError(f"{label} must be lowercase SHA-256 hex") from error
    if len(decoded) != 32:
        raise ArtifactIndexError(f"{label} must be lowercase SHA-256 hex")
    return decoded


@dataclass(frozen=True)
class FileRecord:
    """A stable content record for one file beneath a snapshot root."""

    path: str
    size_bytes: int
    sha256: str

    def __post_init__(self) -> None:
        _safe_relative(self.path, "file path")
        _uint64(self.size_bytes, "file size")
        _sha256_bytes(self.sha256, "file digest")


@dataclass(frozen=True)
class ArtifactRecord:
    """Portable metadata for one completed artifact kind."""

    schema_version: int
    artifact_id: str
    kind: str
    run_identity_sha256: str
    input_sha256: str
    source_sha256: str
    tool_sha256: str
    generation_id: str
    selection: str
    size_bytes: int
    file_count: int
    content_sha256: str
    locator: str

    def as_dict(self) -> dict[str, object]:
        """Return a JSON-serializable record without adding host paths."""
        return asdict(self)


def tree_digest(records: Sequence[FileRecord]) -> str:
    """Hash the canonical, path-sorted framed tree stream."""
    ordered = sorted(records, key=lambda item: item.path.encode("utf-8"))
    if len({item.path for item in ordered}) != len(ordered):
        raise ArtifactIndexError("tree contains duplicate paths")
    digest = sha256(frame(TREE_DOMAIN))
    for record in ordered:
        digest.update(frame(b"file"))
        digest.update(frame(record.path.encode("utf-8")))
        digest.update(frame(_uint64(record.size_bytes, "file size")))
        digest.update(frame(_sha256_bytes(record.sha256, "file digest")))
    return digest.hexdigest()


def artifact_id(
    *,
    run_identity_sha256: str,
    generation_id: str,
    kind: str,
    selection: str,
    size_bytes: int,
    file_count: int,
    content_sha256: str,
) -> str:
    """Compute the portable identity of one artifact record."""
    values = (
        run_identity_sha256.encode("utf-8"),
        generation_id.encode("utf-8"),
        kind.encode("utf-8"),
        selection.encode("utf-8"),
        _uint64(size_bytes, "artifact size"),
        _uint64(file_count, "file count"),
        _sha256_bytes(content_sha256, "content digest"),
    )
    digest = sha256(frame(ARTIFACT_ID_DOMAIN))
    for value in values:
        digest.update(frame(value))
    return digest.hexdigest()


def host_local_locator(
    run_identity_sha256: str,
    generation_id: str,
    artifact_id_value: str,
) -> str:
    """Build an opaque host-local locator containing no filesystem path."""
    components = (run_identity_sha256, generation_id, artifact_id_value)
    if any(not value or "/" in value or "\\" in value for value in components):
        raise ArtifactIndexError("locator components must be nonempty and opaque")
    return "host-local:///" + "/".join(components)


def is_numeric_time_name(name: str) -> bool:
    """Return whether a name is a finite, nonnegative OpenFOAM time value."""
    if _TIME_NAME.fullmatch(name) is None:
        return False
    try:
        value = Decimal(name)
    except InvalidOperation:
        return False
    return value.is_finite() and value >= 0


def _stat_identity(value: os.stat_result) -> tuple[int, int, int, int, int]:
    return (
        value.st_dev,
        value.st_ino,
        value.st_mode,
        value.st_size,
        value.st_ctime_ns,
    )


def _hash_open_file(parent_fd: int, name: str, listed: os.stat_result) -> FileRecord:
    descriptor = os.open(name, _FILE_FLAGS, dir_fd=parent_fd)
    try:
        before = os.fstat(descriptor)
        if not stat.S_ISREG(before.st_mode) or _stat_identity(before) != _stat_identity(listed):
            raise ArtifactIndexError(f"file changed before hashing: {name}")
        digest = sha256()
        while chunk := os.read(descriptor, 1024 * 1024):
            digest.update(chunk)
        after = os.fstat(descriptor)
        if _stat_identity(before) != _stat_identity(after):
            raise ArtifactIndexError(f"file changed while hashing: {name}")
        return FileRecord(name, before.st_size, digest.hexdigest())
    finally:
        os.close(descriptor)


def _walk_directory(directory_fd: int, prefix: str) -> list[FileRecord]:
    records: list[FileRecord] = []
    for name in os.listdir(directory_fd):
        _safe_relative(name, "directory entry")
        listed = os.stat(name, dir_fd=directory_fd, follow_symlinks=False)
        relative = f"{prefix}/{name}" if prefix else name
        if stat.S_ISREG(listed.st_mode):
            record = _hash_open_file(directory_fd, name, listed)
            records.append(FileRecord(relative, record.size_bytes, record.sha256))
        elif stat.S_ISDIR(listed.st_mode):
            child_fd = os.open(name, _DIRECTORY_FLAGS, dir_fd=directory_fd)
            try:
                if _stat_identity(os.fstat(child_fd)) != _stat_identity(listed):
                    raise ArtifactIndexError(f"directory changed before walk: {relative}")
                records.extend(_walk_directory(child_fd, relative))
            finally:
                os.close(child_fd)
        else:
            raise ArtifactIndexError(f"unsupported artifact entry: {relative}")
    return records


def snapshot_tree(root: Path) -> list[FileRecord]:
    """Snapshot a tree through no-follow descriptors and hash opened files."""
    try:
        root_fd = os.open(root, _DIRECTORY_FLAGS)
        try:
            records = _walk_directory(root_fd, "")
        finally:
            os.close(root_fd)
    except (OSError, UnicodeError) as error:
        raise ArtifactIndexError(f"cannot safely snapshot artifact tree: {error}") from error
    return sorted(records, key=lambda item: item.path.encode("utf-8"))


def verify_unchanged(root: Path, expected: Sequence[FileRecord]) -> None:
    """Require an exact second snapshot, including paths and content digests."""
    if snapshot_tree(root) != sorted(expected, key=lambda item: item.path.encode("utf-8")):
        raise ArtifactIndexError("artifact tree changed after snapshot")


def _candidate_has_files(case_root: Path, relative: str) -> bool:
    candidate = case_root.joinpath(*relative.split("/"))
    try:
        return bool(snapshot_tree(candidate))
    except ArtifactIndexError:
        if not os.path.lexists(candidate):
            return False
        raise


def _time_roots(case_root: Path) -> list[str]:
    try:
        root_fd = os.open(case_root, _DIRECTORY_FLAGS)
        try:
            names = os.listdir(root_fd)
            entries = [(name, os.stat(name, dir_fd=root_fd, follow_symlinks=False)) for name in names]
        finally:
            os.close(root_fd)
    except OSError as error:
        raise ArtifactIndexError(f"cannot inspect case root: {error}") from error
    time_names = [name for name, value in entries if is_numeric_time_name(name) and stat.S_ISDIR(value.st_mode)]
    values: dict[Decimal, str] = {}
    for name in time_names:
        numeric = Decimal(name)
        if numeric in values:
            raise ArtifactIndexError(f"duplicate numeric time spellings: {values[numeric]}, {name}")
        values[numeric] = name
    return sorted((name for name in time_names if _candidate_has_files(case_root, name)), key=lambda name: name.encode("utf-8"))


def select_roots(case_root: Path) -> dict[str, list[str]]:
    """Select disjoint, nonempty completed artifact roots below a case."""
    candidates = {
        "mesh_tree": ["constant/polyMesh"],
        "field_tree": _time_roots(case_root),
        "vtk_tree": ["VTK"],
        "postprocessing_tree": ["postProcessing"],
    }
    return {
        kind: [root for root in roots if _candidate_has_files(case_root, root)]
        for kind, roots in candidates.items()
        if roots and any(_candidate_has_files(case_root, root) for root in roots)
    }


def _snapshot_selection(case_root: Path, roots: Sequence[str]) -> list[FileRecord]:
    records: list[FileRecord] = []
    snapshots: list[tuple[Path, list[FileRecord]]] = []
    for root in roots:
        path = case_root.joinpath(*root.split("/"))
        snapshot = snapshot_tree(path)
        snapshots.append((path, snapshot))
        records.extend(
            FileRecord(f"{root}/{item.path}", item.size_bytes, item.sha256)
            for item in snapshot
        )
    for path, snapshot in snapshots:
        verify_unchanged(path, snapshot)
    return sorted(records, key=lambda item: item.path.encode("utf-8"))


def _make_artifact_record(
    kind: str,
    roots: Sequence[str],
    records: Sequence[FileRecord],
    identities: tuple[str, str, str, str, str],
) -> ArtifactRecord:
    run_id, input_id, source_id, tool_id, generation_id = identities
    selection = ",".join(roots)
    content_id = tree_digest(records)
    size = sum(item.size_bytes for item in records)
    identifier = artifact_id(
        run_identity_sha256=run_id,
        generation_id=generation_id,
        kind=kind,
        selection=selection,
        size_bytes=size,
        file_count=len(records),
        content_sha256=content_id,
    )
    return ArtifactRecord(
        ARTIFACT_SCHEMA_VERSION, identifier, kind, run_id, input_id,
        source_id, tool_id, generation_id, selection, size, len(records),
        content_id, host_local_locator(run_id, generation_id, identifier),
    )


def build_index(
    case_root: Path,
    *,
    run_identity_sha256: str,
    input_sha256: str,
    source_sha256: str,
    tool_sha256: str,
    generation_id: str,
) -> dict[str, ArtifactRecord]:
    """Build one record per present kind; field roots use comma-joined names."""
    identities = (
        run_identity_sha256,
        input_sha256,
        source_sha256,
        tool_sha256,
        generation_id,
    )
    result: dict[str, ArtifactRecord] = {}
    for kind, roots in select_roots(case_root).items():
        records = _snapshot_selection(case_root, roots)
        result[kind] = _make_artifact_record(kind, roots, records, identities)
    return result
