"""Attested prebuilt-mesh execution tests for the OpenFOAM runner."""

from __future__ import annotations

import hashlib
import json
import shutil
import subprocess
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.gmsh_bridge import hash_tree
from digitalmodel.solvers.openfoam.runner import (
    OpenFOAMRunConfig,
    OpenFOAMRunner,
    OpenFOAMRunStatus,
)


VALID_CHECK_MESH = """Mesh stats
    faces:            9
    internal faces:   3
    cells:            4
Failed 0 mesh checks.
Mesh OK.
"""
COMMANDS = (
    ("gmshToFoam", "source.msh"),
    ("changeDictionary", "-constant", "-subDict", "dictionaryReplacement"),
    ("checkMesh", "-allGeometry", "-allTopology"),
)
TOOLCHAIN = {
    "gmsh": "4.15.1",
    "openfoam_package": "2312.260127-2",
    "openmpi_package": "4.1.6-7ubuntu2",
}


def _write_poly_mesh(poly_mesh: Path) -> None:
    poly_mesh.mkdir(parents=True)
    (poly_mesh / "boundary").write_text(
        "FoamFile { object boundary; }\n2\n(\n"
        "walls { type wall; nFaces 4; startFace 3; }\n"
        "atmosphere { type patch; nFaces 2; startFace 7; }\n)\n",
        encoding="utf-8",
    )
    (poly_mesh / "cellZones").write_text(
        "FoamFile { object cellZones; }\n1\n(\nfluid\n{\ntype cellZone;\n"
        "cellLabels List<label> 4 (0 1 2 3);\n}\n)\n",
        encoding="utf-8",
    )
    (poly_mesh / "points").write_text("synthetic points\n", encoding="utf-8")


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _case_input_files(case: Path) -> list[Path]:
    files = []
    for name in ("0", "system", "constant"):
        files.extend(path for path in (case / name).rglob("*") if path.is_file())
    excluded = {case / "constant" / "polyMesh.manifest.json"}
    files = [
        path
        for path in files
        if path not in excluded and (case / "constant" / "polyMesh") not in path.parents
    ]
    files.extend(path for path in (case / "source.msh", case / "input.yml") if path.is_file())
    return sorted(files, key=lambda path: path.relative_to(case).as_posix())


def _case_input_evidence(case: Path) -> dict[str, int | str]:
    digest = hashlib.sha256()
    files = _case_input_files(case)
    for path in files:
        relative = path.relative_to(case).as_posix()
        digest.update(relative.encode())
        digest.update(b"\0")
        digest.update(str(path.stat().st_size).encode())
        digest.update(b"\0")
        digest.update(_sha256(path).encode())
        digest.update(b"\n")
    return {
        "tree_sha256": digest.hexdigest(),
        "file_count": len(files),
        "total_bytes": sum(path.stat().st_size for path in files),
    }


def _command_evidence() -> list[dict]:
    return [
        {
            "argv": list(argv),
            "return_code": 0,
            "stdout_sha256": "1" * 64,
            "stderr_sha256": "2" * 64,
        }
        for argv in COMMANDS
    ]


def _manifest_payload(case: Path) -> dict:
    source = case / "source.msh"
    tree = hash_tree(case / "constant" / "polyMesh")
    return {
        "schema_version": 1,
        "status": "completed",
        "created_utc": "2026-07-10T00:00:00Z",
        "source_msh": {
            "path": "source.msh",
            "sha256": _sha256(source),
            "size": source.stat().st_size,
            "format": "2.2",
        },
        "case_inputs": _case_input_evidence(case),
        "poly_mesh": {
            "path": "constant/polyMesh",
            "tree_sha256": tree.sha256,
            "file_count": tree.file_count,
            "total_bytes": tree.total_bytes,
        },
        "contract": {
            "patches": ["walls", "atmosphere"],
            "wall_patches": ["walls"],
            "atmosphere_patch": "atmosphere",
            "fluid_zone": "fluid",
            "cells": 4,
            "faces": 9,
            "internal_faces": 3,
        },
        "commands": _command_evidence(),
        "toolchain": dict(TOOLCHAIN),
    }


def _make_attested_case(root: Path) -> tuple[Path, Path]:
    case = root / "case"
    for subdir in ("0", "constant", "system"):
        (case / subdir).mkdir(parents=True)
    (case / "system" / "controlDict").write_text(
        "FoamFile { object controlDict; }\napplication interFoam;\n", encoding="utf-8"
    )
    (case / "0" / "alpha.water").write_text("initial\n", encoding="utf-8")
    (case / "constant" / "g").write_text("gravity\n", encoding="utf-8")
    (case / "source.msh").write_text("synthetic source\n", encoding="utf-8")
    (case / "input.yml").write_text("fixture: synthetic\n", encoding="utf-8")
    _write_poly_mesh(case / "constant" / "polyMesh")
    manifest = case / "constant" / "polyMesh.manifest.json"
    manifest.write_text(
        json.dumps(_manifest_payload(case), indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return case, manifest


def _rewrite_manifest(manifest: Path, payload: dict) -> None:
    manifest.write_text(json.dumps(payload, sort_keys=True) + "\n", encoding="utf-8")


def _patch_execution(
    monkeypatch: pytest.MonkeyPatch,
    *,
    mutate_path: str | None = None,
    check_mesh_ok: bool = True,
) -> list[tuple[list[str], Path]]:
    calls: list[tuple[list[str], Path]] = []
    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.runner.shutil.which",
        lambda name: f"/usr/bin/{name}",
    )

    def fake_run(argv, **kwargs):
        cwd = Path(kwargs["cwd"])
        calls.append((list(argv), cwd))
        if argv[0] == "checkMesh":
            output = VALID_CHECK_MESH if check_mesh_ok else "Failed 1 mesh checks.\n"
            return subprocess.CompletedProcess(argv, 0, stdout=output, stderr="")
        if mutate_path and argv[0] == "interFoam":
            (cwd / mutate_path).write_text("mutated\n", encoding="utf-8")
        return subprocess.CompletedProcess(argv, 0, stdout="End\n", stderr="")

    monkeypatch.setattr("digitalmodel.solvers.openfoam.runner.subprocess.run", fake_run)
    return calls


def _run(case: Path, manifest: Path, config: OpenFOAMRunConfig | None = None):
    return OpenFOAMRunner(config or OpenFOAMRunConfig(to_vtk=False)).run(
        case, prebuilt_manifest=manifest
    )


def test_attested_prebuilt_mesh_runs_in_private_snapshot(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    source_digest = hash_tree(case / "constant" / "polyMesh").sha256
    calls = _patch_execution(monkeypatch)
    result = _run(case, manifest, OpenFOAMRunConfig(run_set_fields=True, to_vtk=False))
    assert result.status is OpenFOAMRunStatus.COMPLETED
    assert [argv for argv, _ in calls] == [
        ["checkMesh", "-allGeometry", "-allTopology"],
        ["setFields"],
        ["interFoam"],
    ]
    assert result.case_dir != case
    assert result.case_dir.parent == case.parent
    assert result.case_dir.name.startswith(f".{case.name}.run-")
    assert result.case_dir.stat().st_mode & 0o077 == 0
    assert all(cwd == result.case_dir for _, cwd in calls)
    assert hash_tree(case / "constant" / "polyMesh").sha256 == source_digest
    assert not list(case.parent.glob(f".{case.name}.digitalmodel-run.lock"))


def test_tampered_mesh_is_rejected_before_execution(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    (case / "constant" / "polyMesh" / "points").write_text("tampered\n")
    calls = _patch_execution(monkeypatch)
    result = _run(case, manifest)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "digest" in result.error_message.lower()
    assert calls == []


def test_manifest_contract_must_match_structural_mesh(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    payload = json.loads(manifest.read_text(encoding="utf-8"))
    payload["contract"]["patches"] = ["atmosphere", "walls"]
    _rewrite_manifest(manifest, payload)
    calls = _patch_execution(monkeypatch)
    result = _run(case, manifest)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "patch" in result.error_message.lower()
    assert calls == []


def test_manifest_symlink_is_rejected(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    target = tmp_path / "manifest.json"
    manifest.replace(target)
    manifest.symlink_to(target)
    calls = _patch_execution(monkeypatch)
    result = _run(case, manifest)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "symlink" in result.error_message.lower()
    assert calls == []


def test_post_run_mesh_mutation_fails_attestation(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    calls = _patch_execution(monkeypatch, mutate_path="constant/polyMesh/points")
    result = _run(case, manifest)
    assert [argv[0] for argv, _ in calls] == ["checkMesh", "interFoam"]
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "mutated" in result.error_message.lower()
    assert not list(case.parent.glob(f".{case.name}.digitalmodel-run.lock"))


def test_existing_execution_lock_fails_before_snapshot(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    lock = case.parent / f".{case.name}.digitalmodel-run.lock"
    lock.write_text("existing\n", encoding="utf-8")
    calls = _patch_execution(monkeypatch)
    result = _run(case, manifest)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "lock" in result.error_message.lower()
    assert calls == []


@pytest.mark.parametrize(
    "relative",
    ["system/controlDict", "constant/g", "source.msh", "input.yml"],
)
def test_post_run_protected_input_mutation_fails_attestation(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, relative: str
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    calls = _patch_execution(monkeypatch, mutate_path=relative)

    result = _run(case, manifest)

    assert [argv[0] for argv, _ in calls] == ["checkMesh", "interFoam"]
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "protected" in result.error_message.lower()


def test_post_run_initial_field_mutation_is_allowed(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    calls = _patch_execution(monkeypatch, mutate_path="0/alpha.water")

    result = _run(case, manifest)

    assert [argv[0] for argv, _ in calls] == ["checkMesh", "interFoam"]
    assert result.status is OpenFOAMRunStatus.COMPLETED


@pytest.mark.parametrize(
    "defect",
    ["missing_source_msh", "missing_case_inputs", "missing_commands", "fabricated", "toolchain"],
)
def test_invalid_manifest_evidence_is_rejected(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, defect: str
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    payload = json.loads(manifest.read_text(encoding="utf-8"))
    if defect.startswith("missing_"):
        del payload[defect.removeprefix("missing_")]
    elif defect == "fabricated":
        payload["fabricated"] = True
        payload["commands"][0]["return_code"] = 7
    else:
        payload["toolchain"]["gmsh"] = "4.15.2"
    _rewrite_manifest(manifest, payload)
    calls = _patch_execution(monkeypatch)

    result = _run(case, manifest)

    assert result.status is OpenFOAMRunStatus.FAILED
    assert calls == []


@pytest.mark.parametrize("target", ["source", "snapshot"])
def test_case_input_mutation_during_snapshot_is_rejected(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, target: str
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    copytree = shutil.copytree

    def corrupt_copy(source, destination, *args, **kwargs):
        copied = copytree(source, destination, *args, **kwargs)
        if Path(source) == case:
            root = case if target == "source" else Path(destination)
            (root / "0" / "alpha.water").write_text("mutated\n", encoding="utf-8")
        return copied

    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.prebuilt_mesh.shutil.copytree", corrupt_copy
    )
    calls = _patch_execution(monkeypatch)
    result = _run(case, manifest)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "case input" in result.error_message.lower()
    assert calls == []


def test_real_check_mesh_failure_blocks_solver(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    calls = _patch_execution(monkeypatch, check_mesh_ok=False)
    result = _run(case, manifest)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "checkmesh" in result.error_message.lower()
    assert [argv[0] for argv, _ in calls] == ["checkMesh"]


@pytest.mark.parametrize("relative", ["system/controlDict", "0/alpha.water"])
def test_unattested_case_input_change_is_rejected(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, relative: str
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    content = (
        "application interFoam;\n// unattested\n"
        if relative.startswith("system/")
        else "unattested\n"
    )
    (case / relative).write_text(content, encoding="utf-8")
    calls = _patch_execution(monkeypatch)
    result = _run(case, manifest)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "case input" in result.error_message.lower()
    assert calls == []


def test_prebuilt_mode_rejects_arbitrary_solver(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    calls = _patch_execution(monkeypatch)
    config = OpenFOAMRunConfig(solver="simpleFoam", to_vtk=False)
    result = _run(case, manifest, config)
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "interfoam" in result.error_message.lower()
    assert calls == []
