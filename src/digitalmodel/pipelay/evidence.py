# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Byte-level evidence for issued screening, without physical qualification.

A test-only hash in a test file is not an issued manifest. This module persists
the received config and hashes both artefacts by reading the written files.
"""

import hashlib
import json
import re
from importlib import metadata
from pathlib import Path

# Version of this local evidence contract, not an engineering criterion.
SCHEMA_VERSION = 1


def write_evidence_manifest(summary_path: Path, input_bytes: bytes) -> Path:
    """Save received JSON bytes and bind their readback to the written summary.

    ``input_bytes`` is UTF-8 JSON with sorted keys and ``allow_nan=False``,
    captured before router mutates its argument. Package metadata supplies the
    tool revision (distribution version); no source revision is inferred.
    """
    summary_path = Path(summary_path)
    input_path = summary_path.with_name(summary_path.name + ".input.json")
    manifest_path = summary_path.with_name(summary_path.name + ".manifest.json")
    input_path.write_bytes(input_bytes)
    try:
        tool_revision = metadata.version("digitalmodel")
    except metadata.PackageNotFoundError:
        tool_revision = "NOT_EVALUATED"
    manifest = {
        "schema_version": SCHEMA_VERSION,
        "hash_algorithm": "sha256",
        "tool_revision": tool_revision,
        "input": {
            "file": input_path.name,
            "sha256": hashlib.sha256(input_path.read_bytes()).hexdigest(),
        },
        "output": {
            "file": summary_path.name,
            "sha256": hashlib.sha256(summary_path.read_bytes()).hexdigest(),
        },
    }
    manifest_path.write_text(
        json.dumps(manifest, indent=2, sort_keys=True, allow_nan=False) + "\n",
        encoding="utf-8",
    )
    return manifest_path


def _failure(artefact: str, reason: str) -> dict:
    return {"artefact": artefact, "reason": reason}


def _unique_object(pairs: list) -> dict:
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("Duplicate manifest key")
        result[key] = value
    return result


def _validate_manifest(manifest: object, manifest_path: Path) -> str | None:
    if not isinstance(manifest, dict):
        return "invalid_schema"
    keys = {"schema_version", "hash_algorithm", "tool_revision", "input", "output"}
    if manifest.keys() - keys:
        return "unknown_keys"
    if manifest.keys() != keys:
        return "invalid_schema"
    if type(manifest["schema_version"]) is not int or manifest["schema_version"] != SCHEMA_VERSION:
        return "unsupported_schema_version"
    if manifest["hash_algorithm"] != "sha256":
        return "unsupported_hash_algorithm"
    if not isinstance(manifest["tool_revision"], str) or not manifest["tool_revision"]:
        return "invalid_schema"
    suffix = ".manifest.json"
    if not manifest_path.name.endswith(suffix):
        return "invalid_schema"
    output_name = manifest_path.name[:-len(suffix)]
    for name, filename in (("input", output_name + ".input.json"), ("output", output_name)):
        record = manifest[name]
        if not isinstance(record, dict):
            return "invalid_schema"
        if record.keys() - {"file", "sha256"}:
            return "unknown_keys"
        if record.keys() != {"file", "sha256"}:
            return "invalid_schema"
        # Only the expected siblings can be read; no manifest-directed traversal.
        if record["file"] != filename or not filename:
            return "invalid_schema"
        digest = record["sha256"]
        # SHA-256 encodes a 256-bit digest as 64 hexadecimal digits.
        if not isinstance(digest, str) or not re.fullmatch(r"[0-9a-f]{64}", digest):
            return "invalid_schema"
    return None


def verify_evidence_manifest(manifest_path: Path) -> dict:
    """Re-read issued artefacts, returning ``ok`` and structured failure reasons.

    Unknown fields, duplicate keys, unsupported contracts and malformed data
    fail closed. Missing/unreadable files and changed bytes identify the
    affected artefact. A test-only hash is not an issued manifest. This checks
    consistency against an unsigned manifest, not authenticity or physics.
    """
    manifest_path = Path(manifest_path)
    try:
        raw = manifest_path.read_bytes()
    except FileNotFoundError:
        return {"ok": False, "failures": [_failure("manifest", "missing_file")]}
    except OSError:
        return {"ok": False, "failures": [_failure("manifest", "unreadable_file")]}
    try:
        manifest = json.loads(raw, object_pairs_hook=_unique_object)
    except (ValueError, UnicodeError):
        return {"ok": False, "failures": [_failure("manifest", "invalid_json")]}
    reason = _validate_manifest(manifest, manifest_path)
    if reason:
        return {"ok": False, "failures": [_failure("manifest", reason)]}
    failures = []
    for name in ("input", "output"):
        record = manifest[name]
        try:
            data = (manifest_path.parent / record["file"]).read_bytes()
        except FileNotFoundError:
            failures.append(_failure(name, "missing_file"))
        except OSError:
            failures.append(_failure(name, "unreadable_file"))
        else:
            if hashlib.sha256(data).hexdigest() != record["sha256"]:
                failures.append(_failure(name, "digest_mismatch"))
    return {"ok": not failures, "failures": failures}
