"""Closed schema, privacy validation, and atomic I/O for CFD smoke evidence."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import re
import stat
import sys
import tempfile
from datetime import datetime, timezone
from pathlib import Path, PurePosixPath
from typing import Any, Mapping, Sequence

from .gmsh_bridge import hash_tree


ASSETUTILITIES_COMMIT = "993f1b5ddc90b56ecf531bedb1b84f5efe096700"
TOP_LEVEL_FIELDS = frozenset("schema_version status purpose engineering_claim created_utc execution_class dispatcher timing motion stages bridge artifacts source_provenance dependency_provenance".split())
ARTIFACT_KEYS = frozenset("uv_lock input_yaml source_msh initial_fields case_dictionaries poly_mesh".split())
_HASH = re.compile(r"[0-9a-f]{64}\Z")
_COMMIT = re.compile(r"[0-9a-f]{40}\Z")
_IPV4 = re.compile(r"(?<![0-9])(?:[0-9]{1,3}\.){3}[0-9]{1,3}(?![0-9])")
_EMAIL = re.compile(r"\b[^\s/@]+@[^\s/@]+\.[^\s/@]+\b")
_INFRASTRUCTURE = re.compile(r"\b(?:ace-linux-[0-9]+|gpu-claw|localhost)\b", re.I)
_FORBIDDEN_KEYS = {"host", "hostname", "ssh", "ssh_target", "address", "ip", "ip_address", "machine", "node"}
_PROVENANCE_FIELDS = {"clean", "commit", "tracked_sources_sha256"}

class EvidenceValidationError(ValueError):
    """Raised when evidence is incomplete, unsafe, or internally inconsistent."""

def _exact(record: Any, fields: set[str] | frozenset[str], label: str) -> Mapping:
    if not isinstance(record, Mapping) or set(record) != set(fields):
        actual = sorted(record) if isinstance(record, Mapping) else type(record).__name__
        raise EvidenceValidationError(f"{label} fields are not exact: {actual}")
    return record

def _number(value: Any, label: str, *, minimum: float = 0.0) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise EvidenceValidationError(f"{label} must be numeric")
    number = float(value)
    if not math.isfinite(number) or number < minimum:
        raise EvidenceValidationError(f"{label} is outside its allowed range")
    return number

def _integer(value: Any, label: str, *, minimum: int = 0) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < minimum:
        raise EvidenceValidationError(f"{label} must be an integer >= {minimum}")
    return value


def _digest(value: Any, label: str) -> str:
    if not isinstance(value, str) or _HASH.fullmatch(value) is None:
        raise EvidenceValidationError(f"{label} must be a lowercase SHA-256")
    return value


def _safe_path(value: Any, label: str) -> str:
    if not isinstance(value, str) or not value or "\\" in value or "\x00" in value:
        raise EvidenceValidationError(f"{label} must be a safe relative POSIX path")
    path = PurePosixPath(value)
    if path.is_absolute() or ".." in path.parts or value.startswith("~"):
        raise EvidenceValidationError(f"{label} must be a safe relative POSIX path")
    return value


def _utc(value: Any, label: str) -> None:
    if not isinstance(value, str) or not value.endswith("Z"):
        raise EvidenceValidationError(f"{label} must be a UTC timestamp")
    try:
        datetime.fromisoformat(value[:-1] + "+00:00")
    except ValueError as exc:
        raise EvidenceValidationError(f"{label} must be a UTC timestamp") from exc


def validate_privacy(value: Any, path: str = "$") -> None:
    """Reject machine identity, addresses, and absolute paths anywhere in evidence."""
    if isinstance(value, Mapping):
        for key, item in value.items():
            if not isinstance(key, str) or key.lower() in _FORBIDDEN_KEYS:
                raise EvidenceValidationError(f"privacy violation at {path}: unsafe key")
            validate_privacy(item, f"{path}.{key}")
        return
    if isinstance(value, Sequence) and not isinstance(value, (str, bytes, bytearray)):
        for index, item in enumerate(value):
            validate_privacy(item, f"{path}[{index}]")
        return
    if not isinstance(value, str):
        return
    absolute = value.startswith(("/", "~/", "\\")) or re.match(r"^[A-Za-z]:[\\/]", value)
    sensitive = _IPV4.search(value) or _EMAIL.search(value) or _INFRASTRUCTURE.search(value)
    if absolute or sensitive or "://" in value:
        raise EvidenceValidationError(f"privacy violation at {path}")


def _validate_provenance(record: Any, label: str) -> Mapping:
    item = _exact(record, _PROVENANCE_FIELDS, label)
    if item["clean"] is not True:
        raise EvidenceValidationError(f"{label} must attest a clean checkout")
    if not isinstance(item["commit"], str) or _COMMIT.fullmatch(item["commit"]) is None:
        raise EvidenceValidationError(f"{label}.commit must be a lowercase commit SHA")
    _digest(item["tracked_sources_sha256"], f"{label}.tracked_sources_sha256")
    return item


def _validate_provenance_chains(payload: Mapping) -> None:
    fields = {"pre_execution", "post_execution"}
    source = _exact(payload["source_provenance"], fields, "source provenance")
    before = _validate_provenance(source["pre_execution"], "source pre-execution")
    after = _validate_provenance(source["post_execution"], "source post-execution")
    if before != after:
        raise EvidenceValidationError("source provenance changed during execution")
    dependencies = _exact(payload["dependency_provenance"], {"assetutilities"}, "dependencies")
    asset = _exact(dependencies["assetutilities"], fields, "assetutilities")
    before = _validate_provenance(asset["pre_execution"], "assetutilities pre-execution")
    after = _validate_provenance(asset["post_execution"], "assetutilities post-execution")
    if before != after or before["commit"] != ASSETUTILITIES_COMMIT:
        raise EvidenceValidationError("assetutilities provenance is unpinned or changed")


def _validate_dispatcher(record: Any, execution_class: str) -> Mapping:
    fields = set("ranks selected_ranks visible_cpus load_threshold projected_load_per_core actual_load1 actual_load_per_core".split())
    item = _exact(record, fields, "dispatcher")
    ranks = _integer(item["ranks"], "dispatcher.ranks", minimum=1)
    selected = _integer(item["selected_ranks"], "dispatcher.selected_ranks", minimum=1)
    cpus = _integer(item["visible_cpus"], "dispatcher.visible_cpus", minimum=1)
    if ranks != selected or ranks > cpus or item["load_threshold"] != 1.5:
        raise EvidenceValidationError("dispatcher rank binding or fixed threshold is invalid")
    actual_load = _number(item["actual_load1"], "dispatcher.actual_load1")
    actual_per_core = _number(item["actual_load_per_core"], "dispatcher.actual_load_per_core")
    projected = item["projected_load_per_core"]
    if projected is None and execution_class != "test":
        raise EvidenceValidationError("dispatcher projected load is required")
    if projected is not None and _number(projected, "dispatcher projected load/core") > 1.5:
        raise EvidenceValidationError("dispatcher projected load/core exceeds 1.5")
    if actual_per_core > 1.5 or not math.isclose(actual_per_core, actual_load / cpus, rel_tol=1e-12):
        raise EvidenceValidationError("dispatcher actual load/core is invalid")
    return item


def _validate_timing(record: Any) -> None:
    fields = set("expected_end_time observed_end_time tolerance step_count wall_seconds seconds_per_step".split())
    item = _exact(record, fields, "timing")
    expected = _number(item["expected_end_time"], "timing.expected_end_time")
    observed = _number(item["observed_end_time"], "timing.observed_end_time")
    tolerance = _number(item["tolerance"], "timing.tolerance", minimum=1e-300)
    steps = _integer(item["step_count"], "timing.step_count", minimum=1)
    wall = _number(item["wall_seconds"], "timing.wall_seconds", minimum=1e-300)
    per_step = _number(item["seconds_per_step"], "timing.seconds_per_step", minimum=1e-300)
    if abs(observed - expected) > tolerance or not math.isclose(per_step, wall / steps, rel_tol=1e-12):
        raise EvidenceValidationError("timing evidence is internally inconsistent")


def _validate_motion(record: Any) -> None:
    fields = set("axis angle_radians origin point_count initial_points_sha256 reconstructed_points_sha256 max_displacement max_rotation_error tolerance".split())
    item = _exact(record, fields, "motion")
    if item["axis"] != [0.0, 0.0, 1.0]:
        raise EvidenceValidationError("motion axis must be the prescribed z axis")
    if not isinstance(item["origin"], list) or len(item["origin"]) != 3:
        raise EvidenceValidationError("motion origin must contain three coordinates")
    for index, coordinate in enumerate(item["origin"]):
        _number(coordinate, f"motion.origin[{index}]", minimum=-float("inf"))
    _number(abs(item["angle_radians"]), "motion angle", minimum=1e-300)
    _integer(item["point_count"], "motion.point_count", minimum=1)
    _digest(item["initial_points_sha256"], "motion.initial_points_sha256")
    _digest(item["reconstructed_points_sha256"], "motion.reconstructed_points_sha256")
    _number(item["max_displacement"], "motion displacement", minimum=1e-300)
    error = _number(item["max_rotation_error"], "motion rotation error")
    tolerance = _number(item["tolerance"], "motion tolerance", minimum=1e-300)
    if error > tolerance:
        raise EvidenceValidationError("motion evidence does not prove the prescribed rotation")


def _validate_artifact(record: Any, label: str, *, tree: bool) -> Mapping:
    fields = {"path", "file_count", "total_bytes", "tree_sha256"} if tree else {"path", "size", "sha256"}
    item = _exact(record, fields, label)
    _safe_path(item["path"], f"{label}.path")
    if tree:
        _integer(item["file_count"], f"{label}.file_count", minimum=1)
        _integer(item["total_bytes"], f"{label}.total_bytes", minimum=1)
        _digest(item["tree_sha256"], f"{label}.tree_sha256")
    else:
        _integer(item["size"], f"{label}.size", minimum=1)
        _digest(item["sha256"], f"{label}.sha256")
    return item


def _validate_artifacts(record: Any) -> Mapping:
    artifacts = _exact(record, ARTIFACT_KEYS, "artifact keys")
    expected_paths = {"uv_lock": "uv.lock", "input_yaml": "input.yml", "source_msh": "source.msh"}
    for name, path in expected_paths.items():
        if _validate_artifact(artifacts[name], name, tree=False)["path"] != path:
            raise EvidenceValidationError(f"{name} path is not canonical")
    for name, path in {"initial_fields": "0", "poly_mesh": "constant/polyMesh"}.items():
        if _validate_artifact(artifacts[name], name, tree=True)["path"] != path:
            raise EvidenceValidationError(f"{name} path is not canonical")
    dictionary_fields = {"roots", "file_count", "total_bytes", "tree_sha256"}
    dictionaries = _exact(artifacts["case_dictionaries"], dictionary_fields, "case_dictionaries")
    if dictionaries["roots"] != ["constant", "system"]:
        raise EvidenceValidationError("case dictionary roots are not canonical")
    _integer(dictionaries["file_count"], "case dictionaries file count", minimum=1)
    _integer(dictionaries["total_bytes"], "case dictionaries bytes", minimum=1)
    _digest(dictionaries["tree_sha256"], "case dictionaries digest")
    return artifacts


def _validate_bridge(record: Any, artifacts: Mapping) -> None:
    fields = set("schema_version status created_utc source_msh case_inputs poly_mesh contract commands toolchain".split())
    bridge = _exact(record, fields, "bridge")
    if bridge["schema_version"] != 1 or bridge["status"] != "completed":
        raise EvidenceValidationError("bridge must be completed schema version 1")
    _utc(bridge["created_utc"], "bridge.created_utc")
    source = _exact(bridge["source_msh"], {"path", "sha256", "format"}, "bridge source")
    mesh = _exact(
        bridge["poly_mesh"], {"path", "tree_sha256", "file_count", "total_bytes"}, "bridge mesh"
    )
    if source["path"] != "source.msh" or source["format"] != "2.2":
        raise EvidenceValidationError("bridge source metadata is invalid")
    if _digest(source["sha256"], "bridge source digest") != artifacts["source_msh"]["sha256"]:
        raise EvidenceValidationError("bridge source digest does not match artifacts")
    inputs = _exact(bridge["case_inputs"], {"tree_sha256", "file_count", "total_bytes"}, "bridge inputs")
    _digest(inputs["tree_sha256"], "bridge inputs digest")
    for name in ("file_count", "total_bytes"):
        _integer(inputs[name], f"bridge inputs {name}", minimum=1)
    if mesh["path"] != "constant/polyMesh" or mesh["tree_sha256"] != artifacts["poly_mesh"]["tree_sha256"]:
        raise EvidenceValidationError("bridge mesh digest does not match artifacts")
    _validate_bridge_contract(bridge["contract"])
    _validate_bridge_commands(bridge["commands"])
    toolchain = _exact(
        bridge["toolchain"], {"gmsh", "openfoam_package", "openmpi_package"}, "bridge toolchain"
    )
    expected = {"gmsh": "4.15.1", "openfoam_package": "2312.260127-2", "openmpi_package": "4.1.6-7ubuntu2"}
    if dict(toolchain) != expected:
        raise EvidenceValidationError("bridge toolchain is not the measured pinned toolchain")


def _validate_bridge_contract(record: Any) -> None:
    fields = set("patches wall_patches atmosphere_patch fluid_zone cells faces internal_faces".split())
    item = _exact(record, fields, "bridge contract")
    if item["patches"] != ["walls", "atmosphere"] or item["wall_patches"] != ["walls"]:
        raise EvidenceValidationError("bridge patch contract is invalid")
    if item["atmosphere_patch"] != "atmosphere" or item["fluid_zone"] != "fluid":
        raise EvidenceValidationError("bridge zone contract is invalid")
    _integer(item["cells"], "bridge cells", minimum=1)
    faces = _integer(item["faces"], "bridge faces", minimum=1)
    internal = _integer(item["internal_faces"], "bridge internal faces", minimum=1)
    if internal >= faces:
        raise EvidenceValidationError("bridge mesh counts are invalid")


def _validate_bridge_commands(records: Any) -> None:
    expected = [["gmshToFoam", "source.msh"], ["changeDictionary", "-constant", "-subDict", "dictionaryReplacement"], ["checkMesh", "-allGeometry", "-allTopology"]]
    if not isinstance(records, list) or len(records) != len(expected):
        raise EvidenceValidationError("bridge quality command evidence is incomplete")
    fields = {"argv", "return_code", "stdout_sha256", "stderr_sha256"}
    for index, (record, argv) in enumerate(zip(records, expected)):
        item = _exact(record, fields, f"bridge command {index}")
        if item["argv"] != argv or item["return_code"] != 0:
            raise EvidenceValidationError("bridge quality command failed or changed")
        _digest(item["stdout_sha256"], f"bridge command {index} stdout")
        _digest(item["stderr_sha256"], f"bridge command {index} stderr")


def _validate_stages(records: Any, ranks: int) -> None:
    expected = [["setFields"], ["decomposePar", "-force"], ["mpirun", "-np", str(ranks), "interFoam", "-parallel"], ["reconstructParMesh", "-latestTime"], ["reconstructPar", "-latestTime", "-no-fields", "-no-lagrangian"]]
    if not isinstance(records, list) or len(records) != len(expected):
        raise EvidenceValidationError("smoke stage evidence is incomplete")
    fields = {"name", "argv", "return_code", "stdout_sha256", "stderr_sha256", "log_sha256", "log_path"}
    for index, (record, argv) in enumerate(zip(records, expected)):
        item = _exact(record, fields, f"stage {index}")
        if item["name"] != argv[0] or item["argv"] != argv or item["return_code"] != 0:
            raise EvidenceValidationError("smoke stage failed or changed")
        _safe_path(item["log_path"], f"stage {index} log path")
        for name in ("stdout_sha256", "stderr_sha256", "log_sha256"):
            _digest(item[name], f"stage {index} {name}")


def validate_evidence(payload: Any) -> dict[str, Any]:
    """Validate the complete schema and return the original evidence mapping."""
    validate_privacy(payload)
    item = _exact(payload, TOP_LEVEL_FIELDS, "top-level")
    if item["schema_version"] != 1 or item["status"] != "completed":
        raise EvidenceValidationError("evidence must be completed schema version 1")
    if item["purpose"] != "methodology_bridge_validation_only" or item["engineering_claim"] != "none":
        raise EvidenceValidationError("evidence purpose or engineering claim is invalid")
    _utc(item["created_utc"], "created_utc")
    execution_class = item["execution_class"]
    if execution_class not in {"dedicated", "shared-fallback", "test"}:
        raise EvidenceValidationError("execution_class is not approved")
    dispatcher = _validate_dispatcher(item["dispatcher"], execution_class)
    _validate_timing(item["timing"])
    _validate_motion(item["motion"])
    artifacts = _validate_artifacts(item["artifacts"])
    _validate_bridge(item["bridge"], artifacts)
    _validate_stages(item["stages"], dispatcher["ranks"])
    _validate_provenance_chains(item)
    return payload


def build_evidence_payload(
    result: Any,
    *,
    bridge_manifest: dict[str, Any],
    artifacts: dict[str, Any],
    source_provenance: dict[str, Any],
    dependency_provenance: dict[str, Any],
    execution_class: str,
    dispatcher: dict[str, Any],
) -> dict[str, Any]:
    motion = result.motion
    timing = {"expected_end_time": result.expected_end_time, "observed_end_time": result.final_time, "tolerance": result.time_tolerance, "step_count": result.step_count, "wall_seconds": result.wall_seconds, "seconds_per_step": result.seconds_per_step}
    motion_record = {"axis": list(motion.axis), "angle_radians": motion.angle_radians, "origin": list(motion.origin), "point_count": motion.point_count, "initial_points_sha256": motion.initial_points_sha256, "reconstructed_points_sha256": motion.reconstructed_points_sha256, "max_displacement": motion.max_displacement, "max_rotation_error": motion.max_rotation_error, "tolerance": motion.tolerance}
    payload = {
        "schema_version": 1, "status": result.status,
        "purpose": "methodology_bridge_validation_only", "engineering_claim": "none",
        "created_utc": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
        "execution_class": execution_class, "dispatcher": dispatcher, "timing": timing,
        "motion": motion_record, "stages": [stage.to_dict() for stage in result.stages],
        "bridge": bridge_manifest, "artifacts": artifacts,
        "source_provenance": source_provenance, "dependency_provenance": dependency_provenance,
    }
    return validate_evidence(payload)


def write_evidence(path: Path | str, result: Any, **context: Any) -> None:
    destination = Path(path)
    destination.parent.mkdir(parents=True, exist_ok=True)
    payload = build_evidence_payload(result, **context)
    descriptor, name = tempfile.mkstemp(prefix=f".{destination.name}.", suffix=".tmp", dir=destination.parent)
    temporary = Path(name)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(payload, stream, indent=2, sort_keys=True)
            stream.write("\n")
        temporary.replace(destination)
    finally:
        temporary.unlink(missing_ok=True)


def _file_artifact(path: Path, relative: str) -> dict[str, Any]:
    content = path.read_bytes()
    return {"path": relative, "size": len(content), "sha256": hashlib.sha256(content).hexdigest()}


def _tree_artifact(path: Path, relative: str) -> dict[str, Any]:
    tree = hash_tree(path)
    return {"path": relative, "file_count": tree.file_count, "total_bytes": tree.total_bytes, "tree_sha256": tree.sha256}


def _case_dictionary_artifact(case: Path) -> dict[str, Any]:
    files: list[tuple[str, bytes]] = []
    for root_name in ("constant", "system"):
        root = case / root_name
        if root.is_symlink() or not root.is_dir():
            raise EvidenceValidationError(f"case dictionary root is invalid: {root_name}")
        for candidate in root.rglob("*"):
            relative = candidate.relative_to(case)
            if relative.parts[:2] == ("constant", "polyMesh") or candidate.name == "polyMesh.manifest.json":
                continue
            if candidate.is_symlink():
                raise EvidenceValidationError(f"case dictionary symlink is forbidden: {relative}")
            mode = candidate.stat().st_mode
            if stat.S_ISDIR(mode):
                continue
            if not stat.S_ISREG(mode):
                raise EvidenceValidationError(f"case dictionary entry is not regular: {relative}")
            files.append((relative.as_posix(), candidate.read_bytes()))
    digest = hashlib.sha256()
    total_bytes = 0
    for relative, content in sorted(files):
        content_hash = hashlib.sha256(content).hexdigest()
        digest.update(f"{relative}\0{len(content)}\0{content_hash}\n".encode())
        total_bytes += len(content)
    return {"roots": ["constant", "system"], "file_count": len(files), "total_bytes": total_bytes, "tree_sha256": digest.hexdigest()}


def capture_pre_run_artifacts(repo_root: Path, case: Path) -> dict[str, Any]:
    """Digest all reduced evidence inputs before setFields mutates the case."""
    return {"uv_lock": _file_artifact(repo_root / "uv.lock", "uv.lock"), "input_yaml": _file_artifact(case / "input.yml", "input.yml"), "source_msh": _file_artifact(case / "source.msh", "source.msh"), "initial_fields": _tree_artifact(case / "0", "0"), "case_dictionaries": _case_dictionary_artifact(case), "poly_mesh": _tree_artifact(case / "constant" / "polyMesh", "constant/polyMesh")}


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("evidence", type=Path)
    args = parser.parse_args(argv)
    try:
        payload = json.loads(args.evidence.read_text(encoding="utf-8"))
        validate_evidence(payload)
    except (OSError, json.JSONDecodeError, EvidenceValidationError) as exc:
        print(f"invalid smoke evidence: {exc}", file=sys.stderr)
        return 1
    print(f"valid smoke evidence: {args.evidence}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
