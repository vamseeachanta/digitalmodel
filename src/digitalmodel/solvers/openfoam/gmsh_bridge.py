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


def _sha256_bytes(content: bytes) -> str:
    return hashlib.sha256(content).hexdigest()


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def hash_tree(root: Path | str) -> TreeDigest:
    """Hash sorted regular files as path NUL size NUL content-sha newline."""
    tree = Path(root)
    if tree.is_symlink() or not tree.is_dir():
        raise GmshBridgeError(f"tree root must be a regular directory: {tree}")
    files: list[FileDigest] = []
    for candidate in tree.rglob("*"):
        relative = candidate.relative_to(tree).as_posix()
        if candidate.is_symlink():
            raise GmshBridgeError(f"symlink is forbidden in tree: {relative}")
        mode = candidate.stat().st_mode
        if stat.S_ISDIR(mode):
            continue
        if not stat.S_ISREG(mode):
            raise GmshBridgeError(f"non-regular tree entry is forbidden: {relative}")
        files.append(
            FileDigest(relative, candidate.stat().st_size, _sha256_file(candidate))
        )
    files.sort(key=lambda item: item.path)
    if not files:
        raise GmshBridgeError("tree contains no regular files")
    digest = hashlib.sha256()
    for item in files:
        digest.update(item.path.encode("utf-8"))
        digest.update(b"\0")
        digest.update(str(item.size).encode("ascii"))
        digest.update(b"\0")
        digest.update(item.sha256.encode("ascii"))
        digest.update(b"\n")
    return TreeDigest(
        sha256=digest.hexdigest(),
        file_count=len(files),
        total_bytes=sum(item.size for item in files),
        files=tuple(files),
    )


def prepare_gmsh_poly_mesh(
    case_dir: Path | str,
    source_msh: Path | str,
    *,
    toolchain: BridgeToolchain,
    boundary_contract: BoundaryContract = DEFAULT_BOUNDARY_CONTRACT,
    command_runner: Callable[..., subprocess.CompletedProcess] | None = None,
) -> BridgeResult:
    """Convert a source MSH in a sibling stage and attest the promoted tree."""
    case = Path(case_dir)
    source = Path(source_msh)
    manifest_path = case / "constant" / MANIFEST_NAME
    _validate_fresh_inputs(case, source, manifest_path)
    source_contract = _validated_source_contract(source)
    stage = Path(
        tempfile.mkdtemp(prefix=f".{case.name}.gmsh-stage-", dir=case.parent)
    )
    try:
        runner = command_runner or subprocess.run
        return _convert_and_promote(
            case,
            source,
            source_contract,
            stage,
            manifest_path,
            toolchain,
            boundary_contract,
            runner,
        )
    except PolyMeshContractError as exc:
        raise GmshBridgeError(str(exc)) from exc
    except OSError as exc:
        raise GmshBridgeError(f"conversion filesystem operation failed: {exc}") from exc
    finally:
        shutil.rmtree(stage, ignore_errors=True)


def _validated_source_contract(source: Path) -> Any:
    contract = inspect_msh2(source)
    try:
        validate_msh2_contract(contract)
    except ValueError as exc:
        raise GmshBridgeError(str(exc)) from exc
    return contract


def _convert_and_promote(
    case: Path,
    source: Path,
    source_contract: Any,
    stage: Path,
    manifest_path: Path,
    toolchain: BridgeToolchain,
    boundary_contract: BoundaryContract,
    runner: Callable[..., subprocess.CompletedProcess],
) -> BridgeResult:
    _populate_stage(case, source, stage, boundary_contract)
    command_evidence, check_output = _execute_commands(stage, runner)
    mesh_contract = _validate_staged_mesh(stage, check_output, boundary_contract)
    stage_poly_mesh = stage / "constant" / "polyMesh"
    tree_digest = hash_tree(stage_poly_mesh)
    _promote_poly_mesh(stage_poly_mesh, case / "constant" / "polyMesh")
    manifest = _build_manifest(
        source,
        source_contract,
        tree_digest,
        mesh_contract,
        command_evidence,
        toolchain,
        boundary_contract,
    )
    try:
        _atomic_write_json(manifest_path, manifest)
    except OSError as exc:
        raise GmshBridgeError(f"manifest write failed: {exc}") from exc
    return BridgeResult(manifest_path, manifest, tree_digest)


def _validate_fresh_inputs(case: Path, source: Path, manifest_path: Path) -> None:
    if case.is_symlink() or not case.is_dir():
        raise GmshBridgeError(f"case must be a regular directory: {case}")
    for required in (case / "0", case / "constant", case / "system"):
        if not required.is_dir():
            raise GmshBridgeError(f"case is missing required directory: {required.name}")
    if not (case / "system" / "controlDict").is_file():
        raise GmshBridgeError("case is missing system/controlDict")
    _reject_links(case)
    destination = case / "constant" / "polyMesh"
    if destination.exists() or destination.is_symlink():
        raise GmshBridgeError("existing or orphan polyMesh output is forbidden")
    if manifest_path.exists() or manifest_path.is_symlink():
        raise GmshBridgeError("existing completed manifest is forbidden")
    if source.is_symlink() or not source.is_file():
        raise GmshBridgeError(f"source MSH must be a regular file: {source}")
    if not _safe_source_name(source.name):
        raise GmshBridgeError(f"unsafe source MSH name: {source.name}")


def _safe_source_name(name: str) -> bool:
    return (
        name.endswith(".msh")
        and not name.startswith(".")
        and name.isascii()
        and all(character in _SAFE_NAME_CHARACTERS for character in name)
    )


def _reject_links(root: Path) -> None:
    for candidate in root.rglob("*"):
        if candidate.is_symlink():
            raise GmshBridgeError(
                f"symlink is forbidden in case: {candidate.relative_to(root)}"
            )


def _populate_stage(
    case: Path,
    source: Path,
    stage: Path,
    contract: BoundaryContract,
) -> None:
    if case.stat().st_dev != stage.stat().st_dev:
        raise GmshBridgeError("staging case must use the destination filesystem")
    shutil.copytree(case, stage, dirs_exist_ok=True, symlinks=False)
    shutil.copy2(source, stage / "source.msh")
    change_dictionary = stage / "system" / "changeDictionaryDict"
    change_dictionary.write_text(_change_dictionary_text(contract), encoding="utf-8")


def _change_dictionary_text(contract: BoundaryContract) -> str:
    entries = []
    for patch_name in contract.patch_names:
        patch_type = contract.expected_type(patch_name)
        entries.append(f"        {patch_name} {{ type {patch_type}; }}")
    body = "\n".join(entries)
    return (
        "FoamFile { version 2.0; format ascii; class dictionary; "
        "object changeDictionaryDict; }\n"
        "dictionaryReplacement\n{\n    boundary\n    {\n"
        f"{body}\n"
        "    }\n}\n"
    )


def _execute_commands(
    stage: Path, runner: Callable[..., subprocess.CompletedProcess]
) -> tuple[tuple[CommandEvidence, ...], str]:
    evidence = []
    check_output = ""
    for command in COMMANDS:
        try:
            process = runner(
                list(command),
                cwd=str(stage),
                capture_output=True,
                text=True,
                check=False,
                shell=False,
            )
        except (OSError, subprocess.TimeoutExpired) as exc:
            raise GmshBridgeError(f"{command[0]} invocation failed: {exc}") from exc
        stdout = process.stdout or ""
        stderr = process.stderr or ""
        if process.returncode != 0:
            raise GmshBridgeError(
                f"{command[0]} returned non-zero exit code {process.returncode}"
            )
        evidence.append(
            CommandEvidence(
                command,
                process.returncode,
                _sha256_bytes(stdout.encode("utf-8")),
                _sha256_bytes(stderr.encode("utf-8")),
            )
        )
        if command[0] == "checkMesh":
            check_output = stdout + stderr
    return tuple(evidence), check_output


def _validate_staged_mesh(
    stage: Path, output: str, contract: BoundaryContract
) -> PolyMeshContract:
    return validate_poly_mesh_contract(
        stage / "constant" / "polyMesh",
        check_mesh_output=output,
        check_mesh_return_code=0,
        boundary_contract=contract,
    )


def _promote_poly_mesh(source: Path, destination: Path) -> None:
    if source.stat().st_dev != destination.parent.stat().st_dev:
        raise GmshBridgeError("polyMesh promotion must remain on one filesystem")
    if destination.exists() or destination.is_symlink():
        raise GmshBridgeError("destination polyMesh appeared during conversion")
    os.replace(source, destination)


def _build_manifest(
    source: Path,
    source_contract: Any,
    tree: TreeDigest,
    mesh: PolyMeshContract,
    commands: tuple[CommandEvidence, ...],
    toolchain: BridgeToolchain,
    boundary_contract: BoundaryContract,
) -> dict[str, Any]:
    return {
        "schema_version": 1,
        "status": "completed",
        "created_utc": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
        "source_msh": {
            "path": source.name,
            "sha256": _sha256_file(source),
            "format": source_contract.version,
        },
        "poly_mesh": {
            "path": "constant/polyMesh",
            "tree_sha256": tree.sha256,
            "file_count": tree.file_count,
            "total_bytes": tree.total_bytes,
        },
        "contract": {
            "patches": list(mesh.patch_names),
            "wall_patches": list(boundary_contract.wall_patches),
            "atmosphere_patch": boundary_contract.atmosphere_patch,
            "fluid_zone": boundary_contract.fluid_zone,
            "cells": mesh.cell_count,
            "faces": mesh.face_count,
            "internal_faces": mesh.internal_face_count,
        },
        "commands": [command.to_dict() for command in commands],
        "toolchain": toolchain.to_dict(),
    }


def _atomic_write_json(destination: Path, payload: dict[str, Any]) -> None:
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{destination.name}.", suffix=".tmp", dir=destination.parent
    )
    os.close(descriptor)
    temporary = Path(temporary_name)
    try:
        temporary.write_text(
            json.dumps(payload, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
        os.replace(temporary, destination)
    finally:
        temporary.unlink(missing_ok=True)
