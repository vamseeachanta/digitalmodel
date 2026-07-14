"""Portable label and namespace validation for OpenFOAM run identity."""

from __future__ import annotations

import base64
import csv
import hashlib
import io
import json
import os
import re
import stat
from functools import partial
from pathlib import Path
from typing import Callable, Mapping, NamedTuple

_TOKEN = re.compile(r"[A-Za-z0-9][A-Za-z0-9._+-]{0,127}", re.ASCII)
_COMPONENT = re.compile(
    r"[A-Za-z0-9](?:[A-Za-z0-9._-]{0,61}[A-Za-z0-9])?", re.ASCII
)
_WINDOWS_RESERVED = frozenset(
    {"CON", "PRN", "AUX", "NUL"}
    | {f"COM{number}" for number in range(1, 10)}
    | {f"LPT{number}" for number in range(1, 10)}
)


def _reserved(value: str) -> bool:
    """Return whether a portable component maps to a Windows device name."""
    return value.split(".", 1)[0].upper() in _WINDOWS_RESERVED


def portable_token(value: object, label: str) -> str:
    """Validate a path-free portable ASCII identity label or version."""
    if not isinstance(value, str) or not _TOKEN.fullmatch(value) or _reserved(value):
        raise ValueError(f"{label} must be a portable path-free ASCII token")
    return value


def portable_namespace(value: object) -> Path:
    """Parse a bounded platform-independent relative namespace."""
    raw = "default" if value is None else value
    if not isinstance(raw, str) or len(raw) > 63:
        raise ValueError("work_root_namespace must contain portable components")
    parts = raw.split("/")
    if not parts or any(not _COMPONENT.fullmatch(part) for part in parts):
        raise ValueError("work_root_namespace must contain portable components")
    if any(_reserved(part) for part in parts):
        raise ValueError("work_root_namespace must contain portable components")
    return Path(*parts)


def normalized_distribution_name(value: str) -> str:
    """Return the wheel/dist-info normalization of a validated name."""
    portable_token(value, "package_name")
    return re.sub(r"[-_.]+", "-", value).lower()


def normalized_distribution_version(value: str) -> str:
    """Return a conservative normalized dist-info version token."""
    portable_token(value, "package_version")
    return value.replace("-", "_").lower()


def _sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


class FileWitness(NamedTuple):
    path: Path
    signature: tuple[int, int, int, int, int]
    content_sha256: str


class MembershipWitness(NamedTuple):
    expected: frozenset[str]
    probe: Callable[[], frozenset[str]]
    label: str


def _regular_signature(path: Path, label: str) -> tuple[int, int, int, int, int]:
    try:
        info = path.lstat()
    except OSError as error:
        raise ValueError(f"{label} is missing or unsafe") from error
    if not stat.S_ISREG(info.st_mode):
        raise ValueError(f"{label} is missing or unsafe")
    return (info.st_dev, info.st_ino, info.st_size, info.st_mtime_ns, info.st_ctime_ns)


def read_identity_file(path: Path, label: str) -> tuple[bytes, FileWitness]:
    """Read a regular lexical path and bind its inode and content."""
    candidate = Path(path)
    before = _regular_signature(candidate, label)
    data = candidate.read_bytes()
    after = _regular_signature(candidate, label)
    if before != after or len(data) != after[2]:
        raise ValueError(f"{label} changed during identity construction")
    return data, FileWitness(candidate, after, _sha256(data))


def revalidate_identity_files(witnesses: list[FileWitness]) -> None:
    """Reopen every identity input and reject path, inode, or byte drift."""
    for expected in witnesses:
        _, current = read_identity_file(expected.path, "identity input")
        if current != expected:
            raise ValueError("identity input changed during identity construction")


def revalidate_memberships(witnesses: list[MembershipWitness]) -> None:
    """Reject package or provenance membership drift."""
    for witness in witnesses:
        if witness.probe() != witness.expected:
            raise ValueError(f"{witness.label} membership changed")


def lexical_relative_path(repo: Path, path: Path) -> str:
    """Return a Git path without resolving away the candidate itself."""
    candidate = Path(os.path.abspath(path))
    try:
        return candidate.relative_to(repo.resolve()).as_posix()
    except ValueError as error:
        raise ValueError("identity input must remain inside the source checkout") from error


def input_records(
    config_path: Path | None,
    inputs: Mapping[str, Path],
    repo: Path | None,
    witnesses: list[FileWitness],
) -> list[dict]:
    entries = dict(inputs)
    if config_path is not None:
        if "request" in entries:
            raise ValueError("reserved input role request cannot be shadowed")
        entries["request"] = config_path
    records = []
    for role, path in sorted(entries.items()):
        portable_token(role, "referenced input role")
        data, witness = read_identity_file(Path(path), "referenced input")
        witnesses.append(witness)
        safe_path = Path(path).name if repo is None else lexical_relative_path(repo, path)
        records.append({"role": role, "safe_relative_path": safe_path,
                        "size_bytes": len(data), "content_sha256": _sha256(data)})
    return records


def executable_records(
    executables: Mapping[str, Path], witnesses: list[FileWitness]
) -> list[dict]:
    records = []
    for role, path in sorted(executables.items()):
        portable_token(role, "selected executable role")
        candidate = Path(path)
        portable_token(candidate.name, "selected executable basename")
        data, witness = read_identity_file(candidate, f"selected executable for {role}")
        witnesses.append(witness)
        records.append({"role": role, "basename": candidate.name,
                        "content_sha256": _sha256(data)})
    return records


def _record_hash(encoded: str) -> str:
    algorithm, value = encoded.split("=", 1)
    if algorithm != "sha256":
        raise ValueError("wheel RECORD must use sha256")
    return base64.urlsafe_b64decode(value + "=" * (-len(value) % 4)).hex()


def _canonical(value: object) -> bytes:
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":"),
                         ensure_ascii=True, allow_nan=False)
    return (encoded + "\n").encode("ascii")


def _matching_record_names(root: Path, name: str, version: str) -> frozenset[str]:
    wanted = (normalized_distribution_name(name),
              normalized_distribution_version(version))
    matches = []
    for record in root.glob("*.dist-info/RECORD"):
        stem = record.parent.name.removesuffix(".dist-info")
        if "-" in stem:
            found_name, found_version = stem.rsplit("-", 1)
            found = (normalized_distribution_name(found_name), found_version.lower())
            if found == wanted:
                matches.append(record.relative_to(root).as_posix())
    return frozenset(matches)


def _wheel_package_names(root: Path, site: Path) -> frozenset[str]:
    return frozenset(
        path.relative_to(site).as_posix()
        for path in root.rglob("*")
        if path.is_file() or path.is_symlink()
    )


def wheel_package(
    root: Path,
    site: Path,
    name: str,
    version: str,
    witnesses: list[FileWitness] | None = None,
    memberships: list[MembershipWitness] | None = None,
) -> dict:
    """Verify matching wheel provenance from the exact RECORD and package bytes."""
    observed = [] if witnesses is None else witnesses
    first_observation = len(observed)
    observed_memberships = [] if memberships is None else memberships
    first_membership = len(observed_memberships)
    record_names = _matching_record_names(site, name, version)
    if len(record_names) != 1:
        raise ValueError("matching wheel distribution is missing or ambiguous")
    record = site / next(iter(record_names))
    package_names = _wheel_package_names(root, site)
    observed_memberships.extend([
        MembershipWitness(record_names,
                          partial(_matching_record_names, site, name, version),
                          "wheel RECORD"),
        MembershipWitness(package_names, partial(_wheel_package_names, root, site),
                          "wheel package"),
    ])
    record_bytes, record_witness = read_identity_file(record, "wheel RECORD")
    observed.append(record_witness)
    rows = {row[0]: row[1:] for row in csv.reader(
        io.StringIO(record_bytes.decode("utf-8"), newline="")) if row}
    actual_names = set(package_names)
    recorded_names = {item for item in rows if item.startswith(root.name + "/")}
    if actual_names - recorded_names:
        raise ValueError("wheel package contains an unrecorded file")
    if recorded_names - actual_names:
        raise ValueError("wheel RECORD references a missing package file")
    actual = []
    for item in sorted(recorded_names):
        path = site / item
        data, witness = read_identity_file(path, f"wheel package file {item}")
        observed.append(witness)
        digest, size = rows[item]
        if not digest or int(size) != len(data) or _record_hash(digest) != _sha256(data):
            raise ValueError("wheel RECORD does not match actual package bytes")
        actual.append({"safe_relative_path": item, "size_bytes": len(data),
                       "content_sha256": _sha256(data)})
    content = {"package_files": actual, "record_sha256": _sha256(record_bytes)}
    revalidate_identity_files(observed[first_observation:])
    revalidate_memberships(observed_memberships[first_membership:])
    return {"git_commit_sha": None, "tracked_tree_clean": None,
            "content_sha256": _sha256(_canonical(content))}
