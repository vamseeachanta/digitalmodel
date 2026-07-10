"""Transactional tests for the synthetic Gmsh-to-OpenFOAM bridge."""

from __future__ import annotations

import hashlib
import json
import subprocess
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.gmsh_bridge import (
    BridgeToolchain,
    GmshBridgeError,
    hash_tree,
    prepare_gmsh_poly_mesh,
)


TOOLCHAIN = BridgeToolchain(
    gmsh_version="4.15.1",
    openfoam_package="2312.260127-2",
    openmpi_package="4.1.6-7ubuntu2",
)

VALID_CHECK_MESH = """Mesh stats
    faces:            9
    internal faces:   3
    cells:            4
Failed 0 mesh checks.
Mesh OK.
"""


def make_case(root: Path) -> Path:
    case = root / "case"
    for subdir in ("0", "constant", "system"):
        (case / subdir).mkdir(parents=True)
    (case / "system" / "controlDict").write_text(
        "FoamFile { object controlDict; }\napplication interFoam;\n",
        encoding="utf-8",
    )
    (case / "0" / "alpha.water").write_text("initial field\n", encoding="utf-8")
    (case / "constant" / "g").write_text("gravity\n", encoding="utf-8")
    (case / "input.yml").write_text("fixture: synthetic\n", encoding="utf-8")
    return case


def write_source_msh(path: Path) -> Path:
    path.write_text(
        """$MeshFormat
2.2 0 8
$EndMeshFormat
$PhysicalNames
3
3 1 "fluid"
2 2 "walls"
2 3 "atmosphere"
$EndPhysicalNames
$Nodes
4
1 0 0 0
2 1 0 0
3 0 1 0
4 0 0 1
$EndNodes
$Elements
5
1 2 2 2 1 1 3 2
2 2 2 2 2 1 2 4
3 2 2 2 3 2 3 4
4 2 2 3 4 1 4 3
5 4 2 1 5 1 2 3 4
$EndElements
""",
        encoding="ascii",
    )
    return path


def write_converted_poly_mesh(stage: Path, *, symlink: bool = False) -> None:
    poly_mesh = stage / "constant" / "polyMesh"
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
    if symlink:
        (poly_mesh / "unsafe-link").symlink_to("points")


def successful_runner(calls: list, *, symlink: bool = False):
    def run(argv, **kwargs):
        calls.append((list(argv), dict(kwargs)))
        if argv[0] == "gmshToFoam":
            write_converted_poly_mesh(Path(kwargs["cwd"]), symlink=symlink)
        stdout = VALID_CHECK_MESH if argv[0] == "checkMesh" else "End\n"
        return subprocess.CompletedProcess(argv, 0, stdout=stdout, stderr="")

    return run


def test_hash_tree_uses_canonical_path_nul_size_nul_content_digest(
    tmp_path: Path,
) -> None:
    (tmp_path / "nested").mkdir()
    (tmp_path / "a.txt").write_bytes(b"alpha\n")
    (tmp_path / "nested" / "b.bin").write_bytes(bytes((0, 255, 16)))

    digest = hash_tree(tmp_path)

    assert digest.sha256 == "d9d298cf3ee6050aa0566b6aaa28034c1e8a90ee5143a79c80e747e09b48e4e5"
    assert digest.file_count == 2
    assert digest.total_bytes == 9
    assert [(item.path, item.size, item.sha256) for item in digest.files] == [
        (
            "a.txt",
            6,
            "b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060",
        ),
        (
            "nested/b.bin",
            3,
            "2da45f2cd1f9c8e69a67abf7a6b26c282533d0a7686787a9533265418680d4d2",
        ),
    ]


def test_conversion_runs_fixed_argv_promotes_mesh_and_writes_manifest_last(
    tmp_path: Path, monkeypatch
) -> None:
    case = make_case(tmp_path)
    source = write_source_msh(case / "source.msh")
    calls: list = []
    manifest_observations: list[bool] = []
    from digitalmodel.solvers.openfoam import gmsh_bridge
    real_write = gmsh_bridge._atomic_write_json

    def observe_manifest(destination: Path, payload: dict) -> None:
        manifest_observations.append((case / "constant" / "polyMesh").is_dir())
        real_write(destination, payload)

    monkeypatch.setattr(gmsh_bridge, "_atomic_write_json", observe_manifest)

    result = prepare_gmsh_poly_mesh(
        case,
        source,
        toolchain=TOOLCHAIN,
        command_runner=successful_runner(calls),
    )

    assert [argv for argv, _ in calls] == [
        ["gmshToFoam", "source.msh"],
        ["changeDictionary", "-constant", "-subDict", "dictionaryReplacement"],
        ["checkMesh", "-allGeometry", "-allTopology"],
    ]
    assert all(kwargs["shell"] is False for _, kwargs in calls)
    assert all(kwargs["check"] is False for _, kwargs in calls)
    assert all(Path(kwargs["cwd"]).parent == case.parent for _, kwargs in calls)
    assert manifest_observations == [True]
    assert result.manifest_path.is_file()
    assert result.manifest["status"] == "completed"
    assert result.manifest["schema_version"] == 1
    assert result.manifest["toolchain"] == TOOLCHAIN.to_dict()
    assert result.manifest["source_msh"] == {
        "path": "source.msh",
        "sha256": hashlib.sha256(source.read_bytes()).hexdigest(),
        "size": source.stat().st_size,
        "format": "2.2",
    }
    assert result.manifest["case_inputs"]["file_count"] == 5
    assert result.manifest["case_inputs"]["total_bytes"] > source.stat().st_size
    assert len(result.manifest["case_inputs"]["tree_sha256"]) == 64
    assert result.manifest["poly_mesh"]["tree_sha256"] == hash_tree(
        case / "constant" / "polyMesh"
    ).sha256
    assert json.loads(result.manifest_path.read_text()) == result.manifest
    assert not list(case.parent.glob(f".{case.name}.gmsh-stage-*"))


@pytest.mark.parametrize("failed_stage", ["gmshToFoam", "changeDictionary"])
def test_command_failure_leaves_no_promoted_mesh_or_completed_manifest(
    tmp_path: Path, failed_stage: str
) -> None:
    case = make_case(tmp_path)
    source = write_source_msh(case / "source.msh")

    def run(argv, **kwargs):
        if argv[0] == "gmshToFoam":
            write_converted_poly_mesh(Path(kwargs["cwd"]))
        code = 3 if argv[0] == failed_stage else 0
        return subprocess.CompletedProcess(argv, code, stdout="End\n", stderr="bad")

    with pytest.raises(GmshBridgeError, match=failed_stage):
        prepare_gmsh_poly_mesh(
            case, source, toolchain=TOOLCHAIN, command_runner=run
        )

    assert not (case / "constant" / "polyMesh").exists()
    assert not (case / "constant" / "polyMesh.manifest.json").exists()


def test_semantic_check_mesh_failure_cannot_promote(tmp_path: Path) -> None:
    case = make_case(tmp_path)
    source = write_source_msh(case / "source.msh")

    def run(argv, **kwargs):
        if argv[0] == "gmshToFoam":
            write_converted_poly_mesh(Path(kwargs["cwd"]))
        stdout = (
            VALID_CHECK_MESH.replace("Failed 0", "Failed 1")
            if argv[0] == "checkMesh"
            else "End\n"
        )
        return subprocess.CompletedProcess(argv, 0, stdout=stdout, stderr="")

    with pytest.raises(GmshBridgeError, match="Failed 1 mesh checks"):
        prepare_gmsh_poly_mesh(
            case, source, toolchain=TOOLCHAIN, command_runner=run
        )

    assert not (case / "constant" / "polyMesh").exists()


def test_symlink_in_converted_tree_is_rejected(tmp_path: Path) -> None:
    case = make_case(tmp_path)
    source = write_source_msh(case / "source.msh")

    with pytest.raises(GmshBridgeError, match="symlink"):
        prepare_gmsh_poly_mesh(
            case,
            source,
            toolchain=TOOLCHAIN,
            command_runner=successful_runner([], symlink=True),
        )

    assert not (case / "constant" / "polyMesh").exists()


def test_failure_after_promotion_leaves_orphan_that_retry_refuses(
    tmp_path: Path, monkeypatch
) -> None:
    case = make_case(tmp_path)
    source = write_source_msh(case / "source.msh")
    from digitalmodel.solvers.openfoam import gmsh_bridge

    def fail_manifest(*_args, **_kwargs) -> None:
        raise OSError("simulated manifest failure")

    monkeypatch.setattr(gmsh_bridge, "_atomic_write_json", fail_manifest)

    with pytest.raises(GmshBridgeError, match="manifest"):
        prepare_gmsh_poly_mesh(
            case,
            source,
            toolchain=TOOLCHAIN,
            command_runner=successful_runner([]),
        )

    assert (case / "constant" / "polyMesh").is_dir()
    assert not (case / "constant" / "polyMesh.manifest.json").exists()
    with pytest.raises(GmshBridgeError, match="existing.*polyMesh"):
        prepare_gmsh_poly_mesh(
            case,
            source,
            toolchain=TOOLCHAIN,
            command_runner=successful_runner([]),
        )


@pytest.mark.parametrize("kind", ["missing", "symlink", "unsafe_name"])
def test_source_input_failures_happen_before_commands(
    tmp_path: Path, kind: str
) -> None:
    case = make_case(tmp_path)
    valid = write_source_msh(tmp_path / "valid.msh")
    source = tmp_path / "missing.msh"
    if kind == "symlink":
        source.symlink_to(valid)
    elif kind == "unsafe_name":
        source = tmp_path / "unsafe name.msh"
        source.write_bytes(valid.read_bytes())
    calls: list = []

    with pytest.raises(GmshBridgeError):
        prepare_gmsh_poly_mesh(
            case,
            source,
            toolchain=TOOLCHAIN,
            command_runner=successful_runner(calls),
        )

    assert calls == []


def test_source_mutation_after_staging_is_rejected(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    case = make_case(tmp_path)
    source = write_source_msh(case / "source.msh")
    from digitalmodel.solvers.openfoam import gmsh_bridge

    populate = gmsh_bridge._populate_stage

    def mutate_after_copy(*args, **kwargs):
        captured = populate(*args, **kwargs)
        source.write_bytes(source.read_bytes() + b"\n")
        return captured

    monkeypatch.setattr(gmsh_bridge, "_populate_stage", mutate_after_copy)

    with pytest.raises(GmshBridgeError, match="source MSH mutated"):
        prepare_gmsh_poly_mesh(
            case,
            source,
            toolchain=TOOLCHAIN,
            command_runner=successful_runner([]),
        )
