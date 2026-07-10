"""Attested prebuilt-mesh execution tests for the OpenFOAM runner."""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.gmsh_bridge import hash_tree
from digitalmodel.solvers.openfoam.runner import (
    OpenFOAMRunConfig,
    OpenFOAMRunner,
    OpenFOAMRunStatus,
)


def _write_poly_mesh(poly_mesh: Path) -> None:
    poly_mesh.mkdir(parents=True)
    (poly_mesh / "boundary").write_text(
        """FoamFile { object boundary; }
2
(
walls { type wall; nFaces 4; startFace 3; }
atmosphere { type patch; nFaces 2; startFace 7; }
)
""",
        encoding="utf-8",
    )
    (poly_mesh / "cellZones").write_text(
        """FoamFile { object cellZones; }
1
(
fluid
{
type cellZone;
cellLabels List<label> 4 (0 1 2 3);
}
)
""",
        encoding="utf-8",
    )
    (poly_mesh / "points").write_text("synthetic points\n", encoding="utf-8")


def _make_attested_case(root: Path) -> tuple[Path, Path]:
    case = root / "case"
    for subdir in ("0", "constant", "system"):
        (case / subdir).mkdir(parents=True)
    (case / "system" / "controlDict").write_text(
        "FoamFile { object controlDict; }\napplication interFoam;\n",
        encoding="utf-8",
    )
    (case / "source.msh").write_text("synthetic source\n", encoding="utf-8")
    _write_poly_mesh(case / "constant" / "polyMesh")
    tree = hash_tree(case / "constant" / "polyMesh")
    manifest = case / "constant" / "polyMesh.manifest.json"
    manifest.write_text(
        json.dumps(
            {
                "schema_version": 1,
                "status": "completed",
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
                "toolchain": {
                    "gmsh": "4.15.1",
                    "openfoam_package": "2312.260127-2",
                    "openmpi_package": "4.1.6-7ubuntu2",
                },
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )
    return case, manifest


def _patch_execution(
    monkeypatch: pytest.MonkeyPatch,
    *,
    mutate_mesh: bool = False,
    mutate_source: bool = False,
):
    calls: list[tuple[list[str], Path]] = []
    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.runner.shutil.which",
        lambda name: f"/usr/bin/{name}",
    )

    def fake_run(argv, **kwargs):
        cwd = Path(kwargs["cwd"])
        calls.append((list(argv), cwd))
        if mutate_mesh and argv[0] == "interFoam":
            (cwd / "constant" / "polyMesh" / "points").write_text(
                "mutated\n", encoding="utf-8"
            )
        if mutate_source and argv[0] == "interFoam":
            (cwd / "source.msh").write_text("mutated source\n", encoding="utf-8")
        return subprocess.CompletedProcess(argv, 0, stdout="End\n", stderr="")

    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.runner.subprocess.run", fake_run
    )
    return calls


def test_attested_prebuilt_mesh_runs_in_private_snapshot(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    source_digest = hash_tree(case / "constant" / "polyMesh").sha256
    calls = _patch_execution(monkeypatch)
    config = OpenFOAMRunConfig(run_set_fields=True, to_vtk=False)

    result = OpenFOAMRunner(config).run(case, prebuilt_manifest=manifest)

    assert result.status is OpenFOAMRunStatus.COMPLETED
    assert [argv for argv, _ in calls] == [["setFields"], ["interFoam"]]
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
    (case / "constant" / "polyMesh" / "points").write_text(
        "tampered\n", encoding="utf-8"
    )
    calls = _patch_execution(monkeypatch)

    result = OpenFOAMRunner(OpenFOAMRunConfig(to_vtk=False)).run(
        case, prebuilt_manifest=manifest
    )

    assert result.status is OpenFOAMRunStatus.FAILED
    assert "digest" in result.error_message.lower()
    assert calls == []


def test_manifest_contract_must_match_structural_mesh(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    payload = json.loads(manifest.read_text(encoding="utf-8"))
    payload["contract"]["patches"] = ["atmosphere", "walls"]
    manifest.write_text(json.dumps(payload), encoding="utf-8")
    calls = _patch_execution(monkeypatch)

    result = OpenFOAMRunner(OpenFOAMRunConfig(to_vtk=False)).run(
        case, prebuilt_manifest=manifest
    )

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

    result = OpenFOAMRunner(OpenFOAMRunConfig(to_vtk=False)).run(
        case, prebuilt_manifest=manifest
    )

    assert result.status is OpenFOAMRunStatus.FAILED
    assert "symlink" in result.error_message.lower()
    assert calls == []


def test_post_run_mesh_mutation_fails_attestation(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    calls = _patch_execution(monkeypatch, mutate_mesh=True)

    result = OpenFOAMRunner(OpenFOAMRunConfig(to_vtk=False)).run(
        case, prebuilt_manifest=manifest
    )

    assert [argv for argv, _ in calls] == [["interFoam"]]
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

    result = OpenFOAMRunner(OpenFOAMRunConfig(to_vtk=False)).run(
        case, prebuilt_manifest=manifest
    )

    assert result.status is OpenFOAMRunStatus.FAILED
    assert "lock" in result.error_message.lower()
    assert calls == []


def test_post_run_source_mesh_mutation_fails_attestation(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case, manifest = _make_attested_case(tmp_path)
    calls = _patch_execution(monkeypatch, mutate_source=True)

    result = OpenFOAMRunner(OpenFOAMRunConfig(to_vtk=False)).run(
        case, prebuilt_manifest=manifest
    )

    assert [argv for argv, _ in calls] == [["interFoam"]]
    assert result.status is OpenFOAMRunStatus.FAILED
    assert "protected" in result.error_message.lower()
