"""Transactional Gmsh-to-OpenFOAM polyMesh conversion and attestation."""
from __future__ import annotations

import hashlib
import json
import os
import shutil
import stat
import subprocess
import tempfile
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable

from digitalmodel.solvers.gmsh_meshing._msh2_contract import (
    inspect_msh2,
    validate_msh2_contract,
)
from .poly_mesh_contract import (
    DEFAULT_BOUNDARY_CONTRACT,
    BoundaryContract,
    PolyMeshContract,
    PolyMeshContractError,
    validate_poly_mesh_contract,
)

MANIFEST_NAME = "polyMesh.manifest.json"
COMMANDS = (
    ("gmshToFoam", "source.msh"),
    ("changeDictionary", "-constant", "-subDict", "dictionaryReplacement"),
    ("checkMesh", "-allGeometry", "-allTopology"),
)
_SAFE_NAME_CHARACTERS = frozenset(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-"
)
class GmshBridgeError(RuntimeError):
    """Raised when conversion cannot produce a completed attestation."""
@dataclass(frozen=True)
class BridgeToolchain:
    gmsh_version: str
    openfoam_package: str
    openmpi_package: str

    def to_dict(self) -> dict[str, str]:
        return {
            "gmsh": self.gmsh_version,
            "openfoam_package": self.openfoam_package,
            "openmpi_package": self.openmpi_package,
        }
PINNED_TOOLCHAIN = BridgeToolchain("4.15.1", "2312.260127-2", "4.1.6-7ubuntu2")
@dataclass(frozen=True)
class FileDigest:
    path: str
    size: int
    sha256: str
@dataclass(frozen=True)
class TreeDigest:
    sha256: str
    file_count: int
    total_bytes: int
    files: tuple[FileDigest, ...]
@dataclass(frozen=True)
class CommandEvidence:
    argv: tuple[str, ...]
    return_code: int
    stdout_sha256: str
    stderr_sha256: str

    def to_dict(self) -> dict[str, Any]:
        return {
            "argv": list(self.argv),
            "return_code": self.return_code,
            "stdout_sha256": self.stdout_sha256,
            "stderr_sha256": self.stderr_sha256,
        }
@dataclass(frozen=True)
class BridgeResult:
    manifest_path: Path
    manifest: dict[str, Any]
    poly_mesh_digest: TreeDigest
@dataclass(frozen=True)
class _SourceCapture:
    digest: FileDigest
    device: int
    inode: int
@dataclass(frozen=True)
class _StagedInputs:
    source: _SourceCapture
    source_contract: Any
    case_inputs: TreeDigest
def _stat_identity(value: os.stat_result) -> tuple[int, ...]:
    return (value.st_dev, value.st_ino, value.st_size, value.st_mtime_ns, value.st_ctime_ns)
def _file_digest(path: Path, relative: str) -> FileDigest:
    flags = os.O_RDONLY | getattr(os, "O_NOFOLLOW", 0)
    try:
        descriptor = os.open(path, flags)
    except OSError as exc:
        raise GmshBridgeError(f"cannot open regular input {relative}: {exc}") from exc
    try:
        before = os.fstat(descriptor)
        if not stat.S_ISREG(before.st_mode):
            raise GmshBridgeError(f"input is not a regular file: {relative}")
        digest = hashlib.sha256()
        while chunk := os.read(descriptor, 1024 * 1024):
            digest.update(chunk)
        if _stat_identity(before) != _stat_identity(os.fstat(descriptor)):
            raise GmshBridgeError(f"input mutated while hashing: {relative}")
        return FileDigest(relative, before.st_size, digest.hexdigest())
    finally:
        os.close(descriptor)
def _digest_files(root: Path, candidates: list[Path]) -> TreeDigest:
    files = [_file_digest(path, path.relative_to(root).as_posix()) for path in candidates]
    files.sort(key=lambda item: item.path)
    if not files:
        raise GmshBridgeError("tree contains no regular files")
    digest = hashlib.sha256()
    for item in files:
        digest.update(item.path.encode("utf-8"))
        digest.update(b"\0" + str(item.size).encode("ascii") + b"\0")
        digest.update(item.sha256.encode("ascii") + b"\n")
    return TreeDigest(
        digest.hexdigest(), len(files), sum(item.size for item in files), tuple(files)
    )
def hash_tree(root: Path | str) -> TreeDigest:
    """Hash sorted regular files as path NUL size NUL content-sha newline."""
    tree = Path(root)
    if tree.is_symlink() or not tree.is_dir():
        raise GmshBridgeError(f"tree root must be a regular directory: {tree}")
    candidates = []
    for candidate in tree.rglob("*"):
        if candidate.is_symlink():
            raise GmshBridgeError(f"symlink is forbidden in tree: {candidate}")
        mode = candidate.stat().st_mode
        if stat.S_ISDIR(mode):
            continue
        if not stat.S_ISREG(mode):
            raise GmshBridgeError(f"non-regular tree entry is forbidden: {candidate}")
        candidates.append(candidate)
    return _digest_files(tree, candidates)
def hash_case_inputs(
    case_dir: Path | str,
    *,
    include_initial_fields: bool = True,
    include_manifest: bool = False,
) -> TreeDigest:
    """Hash case inputs while excluding generated mesh, logs, and results."""
    case = Path(case_dir)
    names = ("0", "system", "constant") if include_initial_fields else ("system", "constant")
    candidates = []
    for name in names:
        root = case / name
        if root.is_symlink() or not root.is_dir():
            raise GmshBridgeError(f"case is missing regular input directory: {name}")
        for candidate in root.rglob("*"):
            relative = candidate.relative_to(case)
            if relative.parts[:2] == ("constant", "polyMesh"):
                continue
            if relative.as_posix() == f"constant/{MANIFEST_NAME}" and not include_manifest:
                continue
            if not candidate.is_dir():
                candidates.append(candidate)
    for name in ("source.msh", "input.yml"):
        candidate = case / name
        if candidate.exists() or candidate.is_symlink():
            candidates.append(candidate)
    return _digest_files(case, candidates)
def prepare_gmsh_poly_mesh(
    case_dir: Path | str,
    source_msh: Path | str,
    *,
    toolchain: BridgeToolchain,
    boundary_contract: BoundaryContract = DEFAULT_BOUNDARY_CONTRACT,
    command_runner: Callable[..., subprocess.CompletedProcess] | None = None,
) -> BridgeResult:
    """Convert one captured source MSH and attest the promoted mesh."""
    case, source = Path(case_dir), Path(source_msh)
    manifest_path = case / "constant" / MANIFEST_NAME
    _validate_fresh_inputs(case, source, manifest_path, toolchain)
    original_inputs = hash_case_inputs(case)
    stage = Path(tempfile.mkdtemp(prefix=f".{case.name}.gmsh-stage-", dir=case.parent))
    try:
        staged = _populate_stage(case, source, stage, boundary_contract)
        if original_inputs != staged.case_inputs:
            raise GmshBridgeError("case inputs mutated during staging")
        _verify_source_capture(source, staged.source)
        return _convert_and_promote(
            case, source, staged, stage, manifest_path, toolchain,
            boundary_contract, command_runner or subprocess.run, original_inputs,
        )
    except PolyMeshContractError as exc:
        raise GmshBridgeError(str(exc)) from exc
    except OSError as exc:
        raise GmshBridgeError(f"conversion filesystem operation failed: {exc}") from exc
    finally:
        shutil.rmtree(stage, ignore_errors=True)
def _validate_fresh_inputs(
    case: Path, source: Path, manifest: Path, toolchain: BridgeToolchain
) -> None:
    if case.is_symlink() or not case.is_dir():
        raise GmshBridgeError(f"case must be a regular directory: {case}")
    for required in (case / "0", case / "constant", case / "system"):
        if not required.is_dir():
            raise GmshBridgeError(f"case is missing required directory: {required.name}")
    if not (case / "system" / "controlDict").is_file():
        raise GmshBridgeError("case is missing system/controlDict")
    _reject_links(case)
    if (case / "constant" / "polyMesh").exists() or manifest.exists():
        raise GmshBridgeError("existing or orphan polyMesh output is forbidden")
    if source.is_symlink() or not source.is_file():
        raise GmshBridgeError(f"source MSH must be a regular file: {source}")
    safe = source.name.endswith(".msh") and not source.name.startswith(".")
    safe = safe and source.name.isascii() and all(c in _SAFE_NAME_CHARACTERS for c in source.name)
    if not safe:
        raise GmshBridgeError(f"unsafe source MSH name: {source.name}")
    if source.absolute() != (case / "source.msh").absolute():
        raise GmshBridgeError("source MSH must be the case-local source.msh")
    if toolchain != PINNED_TOOLCHAIN:
        raise GmshBridgeError("bridge toolchain does not match pinned versions")
def _populate_stage(
    case: Path, source: Path, stage: Path, contract: BoundaryContract
) -> _StagedInputs:
    if case.stat().st_dev != stage.stat().st_dev:
        raise GmshBridgeError("staging case must use the destination filesystem")
    for name in ("0", "constant", "system"):
        shutil.copytree(case / name, stage / name, symlinks=False)
    if (case / "input.yml").is_file():
        shutil.copy2(case / "input.yml", stage / "input.yml")
    source_capture = _copy_source_once(source, stage / "source.msh")
    source_contract = _validated_source_contract(stage / "source.msh")
    case_inputs = hash_case_inputs(stage)
    (stage / "system" / "changeDictionaryDict").write_text(
        _change_dictionary_text(contract), encoding="utf-8"
    )
    return _StagedInputs(source_capture, source_contract, case_inputs)

def _copy_source_once(source: Path, destination: Path) -> _SourceCapture:
    descriptor = os.open(source, os.O_RDONLY | getattr(os, "O_NOFOLLOW", 0))
    digest, size = hashlib.sha256(), 0
    try:
        before = os.fstat(descriptor)
        if not stat.S_ISREG(before.st_mode):
            raise GmshBridgeError("source MSH must remain a regular file")
        with destination.open("xb") as output:
            while chunk := os.read(descriptor, 1024 * 1024):
                output.write(chunk)
                digest.update(chunk)
                size += len(chunk)
        current = source.stat(follow_symlinks=False)
        changed = _stat_identity(before) != _stat_identity(os.fstat(descriptor))
        changed = changed or (current.st_dev, current.st_ino) != (before.st_dev, before.st_ino)
        if changed:
            raise GmshBridgeError("source MSH mutated while staging")
        return _SourceCapture(
            FileDigest("source.msh", size, digest.hexdigest()), before.st_dev, before.st_ino
        )
    finally:
        os.close(descriptor)

def _validated_source_contract(source: Path) -> Any:
    contract = inspect_msh2(source)
    try:
        validate_msh2_contract(contract)
    except ValueError as exc:
        raise GmshBridgeError(str(exc)) from exc
    return contract

def _verify_source_capture(source: Path, captured: _SourceCapture) -> None:
    current = source.stat(follow_symlinks=False)
    digest = _file_digest(source, "source.msh")
    identity = (current.st_dev, current.st_ino) == (captured.device, captured.inode)
    if not identity or (digest.size, digest.sha256) != (
        captured.digest.size, captured.digest.sha256
    ):
        raise GmshBridgeError("source MSH mutated after staging")

def _convert_and_promote(
    case: Path, source: Path, staged: _StagedInputs, stage: Path,
    manifest_path: Path, toolchain: BridgeToolchain,
    boundary_contract: BoundaryContract,
    runner: Callable[..., subprocess.CompletedProcess], original_inputs: TreeDigest,
) -> BridgeResult:
    command_evidence, check_output = _execute_commands(stage, runner)
    mesh_contract = _validate_staged_mesh(stage, check_output, boundary_contract)
    tree_digest = hash_tree(stage / "constant" / "polyMesh")
    _verify_source_capture(source, staged.source)
    if original_inputs != hash_case_inputs(case):
        raise GmshBridgeError("case inputs mutated during conversion")
    _promote_poly_mesh(stage / "constant" / "polyMesh", case / "constant" / "polyMesh")
    manifest = _build_manifest(
        staged, tree_digest, mesh_contract, command_evidence, toolchain, boundary_contract
    )
    try:
        _atomic_write_json(manifest_path, manifest)
    except OSError as exc:
        raise GmshBridgeError(f"manifest write failed: {exc}") from exc
    return BridgeResult(manifest_path, manifest, tree_digest)

def _execute_commands(
    stage: Path, runner: Callable[..., subprocess.CompletedProcess]
) -> tuple[tuple[CommandEvidence, ...], str]:
    evidence, check_output = [], ""
    for command in COMMANDS:
        try:
            process = runner(
                list(command), cwd=str(stage), capture_output=True, text=True,
                check=False, shell=False,
            )
        except (OSError, subprocess.TimeoutExpired) as exc:
            raise GmshBridgeError(f"{command[0]} invocation failed: {exc}") from exc
        stdout, stderr = process.stdout or "", process.stderr or ""
        if process.returncode != 0:
            raise GmshBridgeError(f"{command[0]} returned non-zero exit code {process.returncode}")
        evidence.append(CommandEvidence(
            command, 0, hashlib.sha256(stdout.encode()).hexdigest(),
            hashlib.sha256(stderr.encode()).hexdigest(),
        ))
        if command[0] == "checkMesh":
            check_output = stdout + stderr
    return tuple(evidence), check_output

def _validate_staged_mesh(
    stage: Path, output: str, contract: BoundaryContract
) -> PolyMeshContract:
    return validate_poly_mesh_contract(
        stage / "constant" / "polyMesh", check_mesh_output=output,
        check_mesh_return_code=0, boundary_contract=contract,
    )

def _change_dictionary_text(contract: BoundaryContract) -> str:
    entries = "\n".join(
        f"        {name} {{ type {contract.expected_type(name)}; }}"
        for name in contract.patch_names
    )
    return (
        "FoamFile { version 2.0; format ascii; class dictionary; "
        "object changeDictionaryDict; }\ndictionaryReplacement\n{\n    boundary\n    {\n"
        f"{entries}\n    }}\n}}\n"
    )

def _promote_poly_mesh(source: Path, destination: Path) -> None:
    if source.stat().st_dev != destination.parent.stat().st_dev:
        raise GmshBridgeError("polyMesh promotion must remain on one filesystem")
    if destination.exists() or destination.is_symlink():
        raise GmshBridgeError("destination polyMesh appeared during conversion")
    os.replace(source, destination)

def _build_manifest(
    staged: _StagedInputs, tree: TreeDigest, mesh: PolyMeshContract,
    commands: tuple[CommandEvidence, ...], toolchain: BridgeToolchain,
    boundary_contract: BoundaryContract,
) -> dict[str, Any]:
    source = staged.source.digest
    return {
        "schema_version": 1,
        "status": "completed",
        "created_utc": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
        "source_msh": {
            "path": source.path, "sha256": source.sha256,
            "size": source.size, "format": staged.source_contract.version,
        },
        "case_inputs": _tree_evidence(staged.case_inputs),
        "poly_mesh": {"path": "constant/polyMesh", **_tree_evidence(tree)},
        "contract": {
            "patches": list(mesh.patch_names),
            "wall_patches": list(boundary_contract.wall_patches),
            "atmosphere_patch": boundary_contract.atmosphere_patch,
            "fluid_zone": boundary_contract.fluid_zone,
            "cells": mesh.cell_count, "faces": mesh.face_count,
            "internal_faces": mesh.internal_face_count,
        },
        "commands": [command.to_dict() for command in commands],
        "toolchain": toolchain.to_dict(),
    }

def _tree_evidence(tree: TreeDigest) -> dict[str, int | str]:
    return {
        "tree_sha256": tree.sha256,
        "file_count": tree.file_count,
        "total_bytes": tree.total_bytes,
    }

def _reject_links(root: Path) -> None:
    for candidate in root.rglob("*"):
        if candidate.is_symlink():
            raise GmshBridgeError(f"symlink is forbidden in case: {candidate.relative_to(root)}")

def _atomic_write_json(destination: Path, payload: dict[str, Any]) -> None:
    descriptor, name = tempfile.mkstemp(
        prefix=f".{destination.name}.", suffix=".tmp", dir=destination.parent
    )
    os.close(descriptor)
    temporary = Path(name)
    try:
        temporary.write_text(
            json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )
        os.replace(temporary, destination)
    finally:
        temporary.unlink(missing_ok=True)
