"""Fail-closed attestation and snapshot handling for prebuilt OpenFOAM meshes."""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import tempfile
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from .gmsh_bridge import (
    COMMANDS,
    MANIFEST_NAME,
    PINNED_TOOLCHAIN,
    GmshBridgeError,
    TreeDigest,
    hash_case_inputs,
    hash_tree,
)
from .poly_mesh_contract import (
    BoundaryContract,
    PolyMeshContractError,
    validate_poly_mesh_contract,
)


_ROOT_FIELDS = {
    "schema_version", "status", "created_utc", "source_msh", "case_inputs",
    "poly_mesh", "contract", "commands", "toolchain",
}
_TREE_FIELDS = {"tree_sha256", "file_count", "total_bytes"}
_COMMAND_FIELDS = {"argv", "return_code", "stdout_sha256", "stderr_sha256"}
_CONTRACT_FIELDS = {
    "patches", "wall_patches", "atmosphere_patch", "fluid_zone", "cells",
    "faces", "internal_faces",
}


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
        _reject_links(self.case_dir)
        mesh = hash_tree(self.case_dir / "constant" / "polyMesh")
        if mesh.sha256 != self.expected_mesh_sha256:
            raise PrebuiltMeshError("prebuilt polyMesh was mutated during execution")
        if _hash_protected_inputs(self.case_dir) != self.protected_sha256:
            raise PrebuiltMeshError("protected case inputs were mutated during execution")

    def release(self) -> None:
        self.lock_path.unlink(missing_ok=True)


def prepare_prebuilt_execution(
    case_dir: Path | str,
    manifest_path: Path | str,
    *,
    timeout_seconds: int = 7200,
) -> PrebuiltExecution:
    """Validate source/snapshot/source, then run real checkMesh in the snapshot."""
    case, manifest = Path(case_dir), Path(manifest_path)
    lock = case.parent / f".{case.name}.digitalmodel-run.lock"
    _acquire_lock(lock)
    snapshot: Path | None = None
    try:
        payload = _load_manifest(case, manifest)
        _reject_links(case)
        _validate_bound_case(case, payload)
        snapshot = Path(tempfile.mkdtemp(prefix=f".{case.name}.run-", dir=case.parent))
        shutil.copytree(case, snapshot, dirs_exist_ok=True, symlinks=False)
        snapshot.chmod(0o700)
        _reject_links(snapshot)
        _validate_bound_case(snapshot, payload)
        _validate_bound_case(case, payload)
        protected_sha256 = _hash_protected_inputs(snapshot)
        _run_check_mesh(snapshot, payload, timeout_seconds)
        _validate_mesh_digest(snapshot, payload)
        if _hash_protected_inputs(snapshot) != protected_sha256:
            raise PrebuiltMeshError("checkMesh mutated protected case inputs")
        return PrebuiltExecution(
            source_case=case,
            case_dir=snapshot,
            lock_path=lock,
            expected_mesh_sha256=payload["poly_mesh"]["tree_sha256"],
            protected_sha256=protected_sha256,
        )
    except (GmshBridgeError, OSError, PolyMeshContractError) as exc:
        _cleanup_failed(snapshot, lock)
        raise PrebuiltMeshError(str(exc)) from exc
    except Exception:
        _cleanup_failed(snapshot, lock)
        raise


def _cleanup_failed(snapshot: Path | None, lock: Path) -> None:
    if snapshot is not None:
        shutil.rmtree(snapshot, ignore_errors=True)
    lock.unlink(missing_ok=True)


def _acquire_lock(lock: Path) -> None:
    try:
        descriptor = os.open(lock, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
    except FileExistsError as exc:
        raise PrebuiltMeshError(
            f"prebuilt execution lock already exists: {lock.name}"
        ) from exc
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
    _exact_keys(payload, _ROOT_FIELDS, "root")
    if type(payload["schema_version"]) is not int or payload["schema_version"] != 1:
        raise PrebuiltMeshError("prebuilt manifest schema_version must equal 1")
    if payload["status"] != "completed":
        raise PrebuiltMeshError("prebuilt manifest status must be completed")
    _validate_created_utc(payload["created_utc"])
    _validate_source_fields(_mapping(payload, "source_msh"))
    _validate_tree_fields(_mapping(payload, "case_inputs"), "case_inputs")
    poly_mesh = _mapping(payload, "poly_mesh")
    _exact_keys(poly_mesh, _TREE_FIELDS | {"path"}, "poly_mesh")
    if poly_mesh["path"] != "constant/polyMesh":
        raise PrebuiltMeshError("prebuilt manifest poly_mesh path is invalid")
    _validate_tree_fields(poly_mesh, "poly_mesh", exact=False)
    _validate_contract_fields(_mapping(payload, "contract"))
    _validate_commands(payload["commands"])
    toolchain = _mapping(payload, "toolchain")
    if toolchain != PINNED_TOOLCHAIN.to_dict():
        raise PrebuiltMeshError("prebuilt manifest toolchain does not match pinned versions")


def _validate_created_utc(value: Any) -> None:
    if not isinstance(value, str) or not value.endswith("Z"):
        raise PrebuiltMeshError("prebuilt manifest created_utc must be UTC")
    try:
        datetime.fromisoformat(value.removesuffix("Z") + "+00:00")
    except ValueError as exc:
        raise PrebuiltMeshError("prebuilt manifest created_utc is invalid") from exc


def _validate_source_fields(source: dict[str, Any]) -> None:
    _exact_keys(source, {"path", "sha256", "size", "format"}, "source_msh")
    if source["path"] != "source.msh" or source["format"] != "2.2":
        raise PrebuiltMeshError("prebuilt manifest source_msh metadata is invalid")
    _sha256(source["sha256"], "source_msh.sha256")
    _nonnegative_integer(source, "size")


def _validate_tree_fields(
    payload: dict[str, Any], label: str, *, exact: bool = True
) -> None:
    if exact:
        _exact_keys(payload, _TREE_FIELDS, label)
    _sha256(payload.get("tree_sha256"), f"{label}.tree_sha256")
    _nonnegative_integer(payload, "file_count")
    _nonnegative_integer(payload, "total_bytes")


def _validate_contract_fields(contract: dict[str, Any]) -> None:
    _exact_keys(contract, _CONTRACT_FIELDS, "contract")
    walls, patches = _string_list(contract, "wall_patches"), _string_list(contract, "patches")
    atmosphere, fluid = contract.get("atmosphere_patch"), contract.get("fluid_zone")
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


def _validate_commands(value: Any) -> None:
    if not isinstance(value, list) or len(value) != len(COMMANDS):
        raise PrebuiltMeshError("prebuilt manifest commands are incomplete")
    for command, expected in zip(value, COMMANDS):
        if not isinstance(command, dict):
            raise PrebuiltMeshError("prebuilt manifest command must be an object")
        _exact_keys(command, _COMMAND_FIELDS, "command")
        return_code = command["return_code"]
        if command["argv"] != list(expected) or type(return_code) is not int or return_code != 0:
            raise PrebuiltMeshError("prebuilt manifest command evidence is invalid")
        _sha256(command["stdout_sha256"], "command.stdout_sha256")
        _sha256(command["stderr_sha256"], "command.stderr_sha256")


def _validate_bound_case(case: Path, payload: dict[str, Any]) -> None:
    inputs = hash_case_inputs(case)
    _require_tree_evidence(inputs, payload["case_inputs"], "case input")
    source = next((item for item in inputs.files if item.path == "source.msh"), None)
    expected = payload["source_msh"]
    if source is None or (source.sha256, source.size) != (
        expected["sha256"], expected["size"]
    ):
        raise PrebuiltMeshError("prebuilt source MSH evidence does not match")
    _validate_mesh_digest(case, payload)


def _validate_mesh_digest(case: Path, payload: dict[str, Any]) -> None:
    tree = hash_tree(case / payload["poly_mesh"]["path"])
    _require_tree_evidence(tree, payload["poly_mesh"], "polyMesh digest")


def _require_tree_evidence(
    actual: TreeDigest, expected: dict[str, Any], label: str
) -> None:
    if (actual.sha256, actual.file_count, actual.total_bytes) != (
        expected["tree_sha256"], expected["file_count"], expected["total_bytes"]
    ):
        raise PrebuiltMeshError(f"prebuilt {label} evidence does not match")


def _run_check_mesh(case: Path, payload: dict[str, Any], timeout: int) -> None:
    command = list(COMMANDS[-1])
    try:
        process = subprocess.run(  # noqa: S603 - fixed OpenFOAM utility.
            command, cwd=str(case), capture_output=True, text=True, check=False,
            shell=False, timeout=timeout,
        )
    except (OSError, subprocess.TimeoutExpired) as exc:
        raise PrebuiltMeshError(f"checkMesh invocation failed: {exc}") from exc
    contract_payload = payload["contract"]
    contract = BoundaryContract(
        wall_patches=tuple(contract_payload["wall_patches"]),
        atmosphere_patch=contract_payload["atmosphere_patch"],
        fluid_zone=contract_payload["fluid_zone"],
    )
    validate_poly_mesh_contract(
        case / "constant" / "polyMesh",
        check_mesh_output=(process.stdout or "") + (process.stderr or ""),
        check_mesh_return_code=process.returncode,
        boundary_contract=contract,
    )


def _hash_protected_inputs(case: Path) -> str:
    return hash_case_inputs(
        case, include_initial_fields=False, include_manifest=True
    ).sha256


def _reject_links(root: Path) -> None:
    if root.is_symlink() or not root.is_dir():
        raise PrebuiltMeshError(f"prebuilt case must be a regular directory: {root}")
    for candidate in root.rglob("*"):
        if candidate.is_symlink():
            raise PrebuiltMeshError(
                f"symlink is forbidden in prebuilt case: {candidate.relative_to(root)}"
            )


def _exact_keys(payload: dict[str, Any], expected: set[str], label: str) -> None:
    if set(payload) != expected:
        raise PrebuiltMeshError(f"prebuilt manifest {label} fields are invalid")


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
