# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Portable fingerprints for generated OrcaFlex input bundles, without a solver.

Contract: docs/agent-pack.md, AQG r0 revision 0, G0.7/C07 and G0.8/C08.
SHA-256: RFC 6234 (2011), sections 4 and 6.2. The record encoding below is
this manifest format's definition, not an engineering acceptance coefficient.
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from pathlib import Path, PureWindowsPath
from typing import Iterable

import yaml
from yaml.nodes import MappingNode, ScalarNode, SequenceNode

MANIFEST_NAME = "input-manifest.json"
SCHEMA = "orcaflex-input-manifest-v1"
REVISION_ROLES = ("generator", "template", "design_basis")


class InputManifestError(ValueError):
    """An input bundle cannot be completely fingerprinted."""


@dataclass(frozen=True)
class FingerprintCheck:
    """G0.7 result only; G0.8 revision resolution is not implemented yet."""

    status: str
    findings: tuple[str, ...] = ()


def _canonical_json(value: object) -> bytes:
    return json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=False,
        allow_nan=False,
    ).encode("utf-8")


def _fingerprint(files: dict[str, str]) -> str:
    """H(UTF8(path + NUL + lowercase digest + LF), in sorted path order)."""
    records = "".join(f"{name}\0{files[name]}\n" for name in sorted(files))
    return hashlib.sha256(records.encode("utf-8")).hexdigest()


def _input_path(root: Path, parent: Path, reference: str) -> Path:
    if not isinstance(reference, str) or not reference.strip():
        raise InputManifestError("Input reference must be a nonempty string")
    if any(char in reference for char in "\0\n\r"):
        raise InputManifestError("Input reference contains a record delimiter")
    if Path(reference).is_absolute() or PureWindowsPath(reference).drive:
        raise InputManifestError("Input reference must be relative to the bundle")
    # OrcaFlex files can be prepared with Windows separators on another system.
    path = (parent / reference.replace("\\", "/")).resolve()
    if not path.is_relative_to(root):
        raise InputManifestError("Input dependency is outside the bundle")
    if not path.is_file():
        raise InputManifestError(f"Missing input: {path.relative_to(root).as_posix()}")
    return path


def _references(content: bytes, name: str) -> list[str]:
    """Use representation nodes so repeated include keys cannot be discarded."""
    try:
        documents = list(yaml.compose_all(content, Loader=yaml.SafeLoader))
    except (yaml.YAMLError, UnicodeError) as exc:
        raise InputManifestError(f"Cannot parse YAML input: {name}") from exc
    references = []
    seen = set()
    pending = list(documents)
    while pending:
        node = pending.pop()
        if node is None or id(node) in seen:
            continue
        seen.add(id(node))
        if isinstance(node, MappingNode):
            for key, value in node.value:
                if isinstance(key, ScalarNode) and key.value.lower() in {
                    "basefile",
                    "includefile",
                }:
                    if (
                        not isinstance(value, ScalarNode)
                        or value.tag != "tag:yaml.org,2002:str"
                    ):
                        raise InputManifestError(f"Invalid file reference in: {name}")
                    references.append(value.value)
                else:
                    pending.append(value)
        elif isinstance(node, SequenceNode):
            pending.extend(node.value)
    return references


def _capture(root: Path, inputs: Iterable[str]) -> dict:
    root = root.resolve()
    seeds = sorted(
        {_input_path(root, root, name).relative_to(root).as_posix() for name in inputs}
    )
    if not seeds:
        raise InputManifestError("At least one input is required")
    files: dict[str, str] = {}
    opaque: list[str] = []
    active: set[Path] = set()
    # Iterative DFS avoids a Python recursion limit for long dependency chains.
    pending = [(root / name, False) for name in reversed(seeds)]
    while pending:
        path, leaving = pending.pop()
        if leaving:
            active.remove(path)
            continue
        name = path.relative_to(root).as_posix()
        if path in active:
            raise InputManifestError(f"Input dependency cycle: {name}")
        if name in files:
            continue
        try:
            content = path.read_bytes()
        except OSError as exc:
            raise InputManifestError(f"Cannot read input: {name}") from exc
        files[name] = hashlib.sha256(content).hexdigest()
        if path.suffix.lower() not in {".yml", ".yaml"}:
            opaque.append(name)
            continue
        active.add(path)
        pending.append((path, True))
        for ref in reversed(_references(content, name)):
            pending.append((_input_path(root, path.parent, ref), False))
    return {
        "schema": SCHEMA,
        "algorithm": "sha256",
        "roots": seeds,
        "files": dict(sorted(files.items())),
        "fingerprint": _fingerprint(files),
        "opaque_dependencies": sorted(opaque),
        # C08 requires all three immutable revisions to resolve (AQG r0 G0.8).
        # No revision value is inferred from a basename, version or mutable ref.
        "revisions": {
            role: {
                "status": "NOT_EVALUATED",
                "reason": "Revision evidence not supplied",
            }
            for role in REVISION_ROLES
        },
    }


def write_input_manifest(root: Path, inputs: Iterable[str]) -> Path:
    """Hash explicit seeds and their BaseFile/include closure inside *root*.

    Seeds must include every generated file, including unreferenced parameters.
    Dependencies outside the bundle are rejected; binary inputs are hashed but
    their uninspectable dependency graphs result in NOT_EVALUATED coverage.
    The manifest is an integrity record, not a signature or solver attestation.
    """
    root = Path(root).resolve()
    payload = _capture(root, inputs)
    if MANIFEST_NAME in payload["files"]:
        raise InputManifestError("The manifest cannot be its own input")
    payload["manifest_sha256"] = hashlib.sha256(_canonical_json(payload)).hexdigest()
    path = root / MANIFEST_NAME
    path.write_bytes(_canonical_json(payload) + b"\n")
    return path


def verify_input_manifest(root: Path) -> FingerprintCheck:
    """Independently re-read the seed closure and recompute all file hashes.

    PASS refers only to the declared YAML composition inputs (C07). Callers
    must inspect revisions separately; C08 remains NOT_EVALUATED in this slice.
    """
    root = Path(root).resolve()
    try:
        recorded = json.loads((root / MANIFEST_NAME).read_bytes())
        if not isinstance(recorded, dict) or recorded.get("schema") != SCHEMA:
            raise InputManifestError("Unsupported input manifest schema")
        roots = recorded.get("roots")
        if not isinstance(roots, list) or not all(
            isinstance(name, str) for name in roots
        ):
            raise InputManifestError("Invalid manifest roots")
        current = _capture(root, roots)
        recorded_files = recorded.get("files")
        if not isinstance(recorded_files, dict):
            raise InputManifestError("Invalid manifest file records")
        findings = []
        for name in sorted(current["files"].keys() | recorded_files.keys()):
            if name not in recorded_files:
                findings.append(f"Missing digest: {name}")
            elif name not in current["files"]:
                findings.append(f"Unexpected digest: {name}")
            elif recorded_files[name] != current["files"][name]:
                findings.append(f"Digest mismatch: {name}")
        if recorded.get("fingerprint") != current["fingerprint"]:
            findings.append("Aggregate fingerprint mismatch")
        digest = recorded.pop("manifest_sha256", None)
        if digest != hashlib.sha256(_canonical_json(recorded)).hexdigest():
            findings.append("Manifest metadata fingerprint mismatch")
        if recorded != current:
            findings.append("Manifest differs from recomputed input evidence")
        if findings:
            return FingerprintCheck("FAIL", tuple(findings))
        opaque = current["opaque_dependencies"]
        if opaque:
            return FingerprintCheck(
                "NOT_EVALUATED",
                tuple(f"Opaque dependency graph: {name}" for name in opaque),
            )
        return FingerprintCheck("PASS")
    except (OSError, ValueError, TypeError) as exc:
        # Do not expose absolute filesystem paths or parser snippets in reports.
        message = (
            str(exc)
            if isinstance(exc, InputManifestError)
            else "Cannot read input manifest"
        )
        return FingerprintCheck("FAIL", (message,))
