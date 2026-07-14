"""Portable label and namespace validation for OpenFOAM run identity."""

from __future__ import annotations

import re
import base64
import csv
import hashlib
import io
import json
from pathlib import Path

_TOKEN = re.compile(r"[A-Za-z0-9][A-Za-z0-9._+-]{0,127}", re.ASCII)
_COMPONENT = re.compile(r"[A-Za-z0-9][A-Za-z0-9._-]{0,62}", re.ASCII)
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


def _record_hash(encoded: str) -> str:
    algorithm, value = encoded.split("=", 1)
    if algorithm != "sha256":
        raise ValueError("wheel RECORD must use sha256")
    return base64.urlsafe_b64decode(value + "=" * (-len(value) % 4)).hex()


def _canonical(value: object) -> bytes:
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":"),
                         ensure_ascii=True, allow_nan=False)
    return (encoded + "\n").encode("ascii")


def _matching_record(root: Path, name: str, version: str) -> Path:
    wanted = (normalized_distribution_name(name),
              normalized_distribution_version(version))
    matches = []
    for record in root.glob("*.dist-info/RECORD"):
        stem = record.parent.name.removesuffix(".dist-info")
        if "-" in stem:
            found_name, found_version = stem.rsplit("-", 1)
            found = (normalized_distribution_name(found_name), found_version.lower())
            if found == wanted:
                matches.append(record)
    if len(matches) != 1:
        raise ValueError("matching wheel distribution is missing or ambiguous")
    return matches[0]


def wheel_package(root: Path, site: Path, name: str, version: str) -> dict:
    """Verify matching wheel provenance from the exact RECORD and package bytes."""
    record = _matching_record(site, name, version)
    record_bytes = record.read_bytes()
    rows = {row[0]: row[1:] for row in csv.reader(
        io.StringIO(record_bytes.decode("utf-8"), newline="")) if row}
    package_files = [path for path in root.rglob("*") if path.is_file()]
    actual_names = {path.relative_to(site).as_posix() for path in package_files}
    recorded_names = {item for item in rows if item.startswith(root.name + "/")}
    if actual_names - recorded_names:
        raise ValueError("wheel package contains an unrecorded file")
    if recorded_names - actual_names:
        raise ValueError("wheel RECORD references a missing package file")
    actual = []
    for item in sorted(recorded_names):
        path = site / item
        if not path.is_file() or path.is_symlink():
            raise ValueError(f"identity input is missing or unsafe: {item}")
        data = path.read_bytes()
        digest, size = rows[item]
        if not digest or int(size) != len(data) or _record_hash(digest) != _sha256(data):
            raise ValueError("wheel RECORD does not match actual package bytes")
        actual.append({"safe_relative_path": item, "size_bytes": len(data),
                       "content_sha256": _sha256(data)})
    content = {"package_files": actual, "record_sha256": _sha256(record_bytes)}
    return {"git_commit_sha": None, "tracked_tree_clean": None,
            "content_sha256": _sha256(_canonical(content))}
