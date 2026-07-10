"""Fail-closed attestation and snapshot handling for prebuilt OpenFOAM meshes."""

from __future__ import annotations

import hashlib
import json
import os
import shutil
import stat
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from .gmsh_bridge import MANIFEST_NAME, GmshBridgeError, hash_tree
from .poly_mesh_contract import (
    BoundaryContract,
    PolyMeshContractError,
    validate_poly_mesh_contract,
)


class PrebuiltMeshError(RuntimeError):
    """Raised when a prebuilt mesh cannot be trusted for execution."""


@dataclass
class PrebuiltExecution:
    source_case: Path
    case_dir: Path
    lock_path: Path
    expected_mesh_sha256: str
    protected_sha256: str

    def verify_unchanged(self) -> None:
        mesh = hash_tree(self.case_dir / "constant" / "polyMesh")
        if mesh.sha256 != self.expected_mesh_sha256:
            raise PrebuiltMeshError("prebuilt polyMesh was mutated during execution")
        if _hash_protected_inputs(self.case_dir) != self.protected_sha256:
            raise PrebuiltMeshError("protected case inputs were mutated during execution")

    def release(self) -> None:
        self.lock_path.unlink(missing_ok=True)


def prepare_prebuilt_execution(
    case_dir: Path | str, manifest_path: Path | str
) -> PrebuiltExecution:
    """Validate an attested case, lock it, and create a private run snapshot."""
    case = Path(case_dir)
    manifest = Path(manifest_path)
    lock = case.parent / f".{case.name}.digitalmodel-run.lock"
    _acquire_lock(lock)
    snapshot: Path | None = None
    try:
        payload = _load_manifest(case, manifest)
        _reject_links(case)
        _validate_attested_mesh(case, payload)
        snapshot = Path(tempfile.mkdtemp(prefix=f".{case.name}.run-", dir=case.parent))
        shutil.copytree(case, snapshot, dirs_exist_ok=True, symlinks=False)
        snapshot.chmod(0o700)
        _validate_attested_mesh(snapshot, payload)
        return PrebuiltExecution(
            source_case=case,
            case_dir=snapshot,
            lock_path=lock,
            expected_mesh_sha256=payload["poly_mesh"]["tree_sha256"],
            protected_sha256=_hash_protected_inputs(snapshot),
        )
    except (GmshBridgeError, OSError, PolyMeshContractError) as exc:
        if snapshot is not None:
            shutil.rmtree(snapshot, ignore_errors=True)
        lock.unlink(missing_ok=True)
        raise PrebuiltMeshError(str(exc)) from exc
    except Exception:
        if snapshot is not None:
            shutil.rmtree(snapshot, ignore_errors=True)
        lock.unlink(missing_ok=True)
        raise


def _acquire_lock(lock: Path) -> None:
    try:
        descriptor = os.open(lock, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
    except FileExistsError as exc:
        raise PrebuiltMeshError(f"prebuilt execution lock already exists: {lock.name}") from exc
    try:
        os.write(descriptor, b"digitalmodel prebuilt execution\n")
    finally:
        os.close(descriptor)


def _load_manifest(case: Path, manifest: Path) -> dict[str, Any]:
    expected = case / "constant" / MANIFEST_NAME
    if manifest != expected:
        raise PrebuiltMeshError(f"manifest must be the case-local {MANIFEST_NAME}")
    if manifest.is_symlink():
        raise PrebuiltMeshError("prebuilt manifest symlink is forbidden")
    if not manifest.is_file():
        raise PrebuiltMeshError(f"prebuilt manifest is not a regular file: {manifest}")
    try:
        payload = json.loads(manifest.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise PrebuiltMeshError(f"prebuilt manifest is invalid: {exc}") from exc
    if not isinstance(payload, dict):
        raise PrebuiltMeshError("prebuilt manifest root must be an object")
    _validate_manifest_fields(payload)
    return payload


def _validate_manifest_fields(payload: dict[str, Any]) -> None:
    if type(payload.get("schema_version")) is not int or payload["schema_version"] != 1:
        raise PrebuiltMeshError("prebuilt manifest schema_version must equal 1")
    if payload.get("status") != "completed":
        raise PrebuiltMeshError("prebuilt manifest status must be completed")
    poly_mesh = _mapping(payload, "poly_mesh")
    if poly_mesh.get("path") != "constant/polyMesh":
        raise PrebuiltMeshError("prebuilt manifest poly_mesh path is invalid")
    _sha256(poly_mesh.get("tree_sha256"), "poly_mesh.tree_sha256")
    _nonnegative_integer(poly_mesh, "file_count")
    _nonnegative_integer(poly_mesh, "total_bytes")
    contract = _mapping(payload, "contract")
    _validate_contract_fields(contract)
    toolchain = _mapping(payload, "toolchain")
    for key in ("gmsh", "openfoam_package", "openmpi_package"):
        if not isinstance(toolchain.get(key), str) or not toolchain[key].strip():
            raise PrebuiltMeshError(f"prebuilt manifest toolchain.{key} is required")


def _validate_contract_fields(contract: dict[str, Any]) -> None:
    walls = _string_list(contract, "wall_patches")
    patches = _string_list(contract, "patches")
    atmosphere = contract.get("atmosphere_patch")
    fluid = contract.get("fluid_zone")
    if not walls or not isinstance(atmosphere, str) or not atmosphere:
        raise PrebuiltMeshError("prebuilt manifest boundary contract is incomplete")
    if not isinstance(fluid, str) or not fluid:
        raise PrebuiltMeshError("prebuilt manifest fluid_zone is required")
    if len(set(walls)) != len(walls) or atmosphere in walls:
        raise PrebuiltMeshError("prebuilt manifest patch names must be unique")
    if patches != walls + [atmosphere] or "defaultFaces" in patches:
        raise PrebuiltMeshError("prebuilt manifest patches do not match its contract")
    for key in ("cells", "faces", "internal_faces"):
        _nonnegative_integer(contract, key)


def _validate_attested_mesh(case: Path, payload: dict[str, Any]) -> None:
    poly_manifest = payload["poly_mesh"]
    tree = hash_tree(case / poly_manifest["path"])
    if (
        tree.sha256 != poly_manifest["tree_sha256"]
        or tree.file_count != poly_manifest["file_count"]
        or tree.total_bytes != poly_manifest["total_bytes"]
    ):
        raise PrebuiltMeshError("prebuilt polyMesh digest evidence does not match")
    contract_payload = payload["contract"]
    contract = BoundaryContract(
        wall_patches=tuple(contract_payload["wall_patches"]),
        atmosphere_patch=contract_payload["atmosphere_patch"],
        fluid_zone=contract_payload["fluid_zone"],
    )
    validate_poly_mesh_contract(
        case / "constant" / "polyMesh",
        check_mesh_output=_attested_check_mesh_output(contract_payload),
        check_mesh_return_code=0,
        boundary_contract=contract,
    )


def _attested_check_mesh_output(contract: dict[str, Any]) -> str:
    return (
        f"cells: {contract['cells']}\n"
        f"faces: {contract['faces']}\n"
        f"internal faces: {contract['internal_faces']}\n"
        "Failed 0 mesh checks.\n"
        "Mesh OK.\n"
    )


def _hash_protected_inputs(case: Path) -> str:
    entries: list[tuple[str, int, str]] = []
    candidates = [
        candidate
        for directory in (case / "system", case / "constant")
        for candidate in directory.rglob("*")
    ]
    candidates.extend(
        candidate
        for candidate in (case / "source.msh", case / "input.yml")
        if candidate.exists() or candidate.is_symlink()
    )
    for candidate in candidates:
        relative = candidate.relative_to(case).as_posix()
        if candidate.is_symlink():
            raise PrebuiltMeshError(f"symlink is forbidden in protected inputs: {relative}")
        mode = candidate.stat().st_mode
        if stat.S_ISDIR(mode):
            continue
        if not stat.S_ISREG(mode):
            raise PrebuiltMeshError(f"protected input is not a regular file: {relative}")
        content_sha = hashlib.sha256(candidate.read_bytes()).hexdigest()
        entries.append((relative, candidate.stat().st_size, content_sha))
    digest = hashlib.sha256()
    for relative, size, content_sha in sorted(entries):
        digest.update(relative.encode("utf-8"))
        digest.update(b"\0")
        digest.update(str(size).encode("ascii"))
        digest.update(b"\0")
        digest.update(content_sha.encode("ascii"))
        digest.update(b"\n")
    return digest.hexdigest()


def _reject_links(root: Path) -> None:
    if root.is_symlink() or not root.is_dir():
        raise PrebuiltMeshError(f"prebuilt case must be a regular directory: {root}")
    for candidate in root.rglob("*"):
        if candidate.is_symlink():
            raise PrebuiltMeshError(
                f"symlink is forbidden in prebuilt case: {candidate.relative_to(root)}"
            )


def _mapping(payload: dict[str, Any], key: str) -> dict[str, Any]:
    value = payload.get(key)
    if not isinstance(value, dict):
        raise PrebuiltMeshError(f"prebuilt manifest {key} must be an object")
    return value


def _string_list(payload: dict[str, Any], key: str) -> list[str]:
    value = payload.get(key)
    if not isinstance(value, list) or not all(isinstance(item, str) and item for item in value):
        raise PrebuiltMeshError(f"prebuilt manifest {key} must be a string list")
    return value


def _nonnegative_integer(payload: dict[str, Any], key: str) -> int:
    value = payload.get(key)
    if type(value) is not int or value < 0:
        raise PrebuiltMeshError(f"prebuilt manifest {key} must be a nonnegative integer")
    return value


def _sha256(value: Any, field: str) -> str:
    if not isinstance(value, str) or len(value) != 64:
        raise PrebuiltMeshError(f"prebuilt manifest {field} must be a SHA-256 digest")
    if any(character not in "0123456789abcdef" for character in value):
        raise PrebuiltMeshError(f"prebuilt manifest {field} must be lowercase hexadecimal")
    return value
