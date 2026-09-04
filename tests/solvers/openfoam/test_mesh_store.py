"""Master mesh store: identity rule, store operations, and the bash/python
agreement that keeps the solve-host runtime and the library from drifting."""

from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam import mesh_store as ms

REPO = Path(__file__).resolve().parents[3]
BASH_SCRIPT = REPO / "scripts" / "cfd" / "mesh_store.sh"

SNAPPY = """/*--------------------------------*- C++ -*----------------------------------*\\
| =========                 |                                                 |
\\*---------------------------------------------------------------------------*/
FoamFile { version 2.0; format ascii; class dictionary; object snappyHexMeshDict; }
castellatedMesh true;   // castellate
snap            true;
addLayers       true;
geometry
{
    hull.stl { type triSurfaceMesh; name hull; }
}
castellatedMeshControls
{
    maxGlobalCells   9000000;
    refinementSurfaces { hull { level (4 5); } }
}
addLayersControls { relativeSizes false; firstLayerThickness 1.8764e-03; nSurfaceLayers 6; }
"""


def make_case(root: Path, name: str, *, snappy: str = SNAPPY, stl: bytes = b"solid hull\nendsolid hull\n",
              with_mesh: bool = True, ncells: int = 1234) -> Path:
    case = root / "cases" / name
    (case / "system").mkdir(parents=True)
    (case / "constant" / "triSurface").mkdir(parents=True)
    (case / "0.orig").mkdir()
    (case / "system" / "blockMeshDict").write_text("FoamFile {} vertices ((0 0 0) (1 1 1));\n")
    (case / "system" / "surfaceFeatureExtractDict").write_text("hull.stl { extractionMethod extractFromSurface; }\n")
    (case / "system" / "snappyHexMeshDict").write_text(snappy)
    (case / "system" / "meshQualityDict").write_text("maxNonOrtho 70;\n")
    (case / "system" / "refineMeshDict").write_text("set c0; coordinateSystem global;\n")
    for i in (1, 2, 10):  # 10 after 2: numeric, not lexical, order
        (case / "system" / f"topoSetDict.{i}").write_text(f"actions ( {{ name c0; type cellSet; level {i}; }} );\n")
    # solve dictionaries: NOT identity inputs
    (case / "system" / "controlDict").write_text("application interFoam; endTime 8000;\n")
    (case / "system" / "fvSolution").write_text("PIMPLE { nOuterCorrectors 2; }\n")
    (case / "system" / "decomposeParDict").write_text("numberOfSubdomains 8; method scotch;\n")
    (case / "constant" / "triSurface" / "hull.stl").write_bytes(stl)
    (case / "constant" / "transportProperties").write_text("water { nu 1.19e-06; }\n")
    if with_mesh:
        pm = case / "constant" / "polyMesh"
        pm.mkdir()
        (pm / "owner").write_bytes(b"FoamFile { note \"nPoints:99 nCells:%d nFaces:5 nInternalFaces:3\"; }\n" % ncells + b"\x00" * 64)
        for f in ("points", "faces", "neighbour", "boundary"):
            (pm / f).write_bytes(f.encode() * 32)
        (pm / "sets").mkdir()
        (pm / "sets" / "nonOrthoFaces").write_text("diagnostic\n")
        (case / "log.snappyHexMesh").write_text(
            "patch  faces  layers  overall thickness\n"
            "hull   100    6       0.02  [m]  96.1\n"
            "hull   100    6       0.02  0.9  96.1\n"
        )
        (case / "log.checkMesh").write_text("Checking geometry...\nMesh OK.\nEnd\n")
    return case


# ---------------------------------------------------------------------------
# identity


def test_identity_is_twelve_hex_and_stable(tmp_path):
    case = make_case(tmp_path, "a")
    ident = ms.mesh_identity(case)
    assert len(ident) == 12 and int(ident, 16) >= 0
    assert ms.mesh_identity(case) == ident


def test_identity_ignores_comments_and_whitespace_but_not_values(tmp_path):
    base = ms.mesh_identity(make_case(tmp_path, "a"))
    recommented = SNAPPY.replace("// castellate", "/* renamed */ // other").replace("snap            true;", "snap\ttrue;   ")
    assert ms.mesh_identity(make_case(tmp_path, "b", snappy=recommented)) == base
    changed = SNAPPY.replace("level (4 5)", "level (5 6)")
    assert ms.mesh_identity(make_case(tmp_path, "c", snappy=changed)) != base


def test_identity_tracks_surfaces_but_not_solve_dicts(tmp_path):
    base_case = make_case(tmp_path, "a")
    base = ms.mesh_identity(base_case)
    (base_case / "system" / "controlDict").write_text("application simpleFoam; endTime 1;\n")
    (base_case / "system" / "decomposeParDict").write_text("numberOfSubdomains 16; method hierarchical;\n")
    (base_case / "0.orig" / "U").write_text("uniform (6.7 0 0);\n")
    assert ms.mesh_identity(base_case) == base
    assert ms.mesh_identity(make_case(tmp_path, "b", stl=b"solid hull\nfacet\nendsolid\n")) != base


def test_mesh_inputs_order_is_fixed_and_numeric_for_topo_sets(tmp_path):
    case = make_case(tmp_path, "a")
    inputs = ms.mesh_inputs(case)
    assert inputs[:5] == list(ms.MESH_DICTS)
    assert inputs[5:8] == ["system/topoSetDict.1", "system/topoSetDict.2", "system/topoSetDict.10"]
    assert inputs[8:] == ["constant/triSurface/hull.stl"]
    assert "system/controlDict" not in inputs


def test_identity_refuses_non_meshable_case(tmp_path):
    case = tmp_path / "cases" / "empty"
    (case / "system").mkdir(parents=True)
    (case / "system" / "blockMeshDict").write_text("x;\n")
    with pytest.raises(ms.MeshStoreError):
        ms.mesh_identity(case)


def test_normalise_matches_documented_pipeline():
    text = "a   b\t\tc // comment\n\n  /* block */ d  \n\t\n"
    assert ms.normalise_dict_text(text) == b"a b c\nd\n"


# ---------------------------------------------------------------------------
# store operations


def test_promote_link_find_roundtrip(tmp_path):
    store = ms.MeshStore(tmp_path)
    a = make_case(tmp_path, "db_G04_ms")
    assert store.find(a) is None
    entry = store.promote(a, "db_G04_ms")
    assert entry.path == tmp_path / "meshes" / f"{entry.identity}-db_G04_ms"
    assert (a / "constant" / "polyMesh").is_symlink()
    assert os.readlink(a / "constant" / "polyMesh") == "../../../meshes/%s/polyMesh" % entry.path.name
    assert (a / "constant" / "polyMesh" / "owner").is_file()  # resolves through the link
    prov = entry.provenance()
    assert prov["identity"] == entry.identity
    assert prov["cells"] == 1234
    assert prov["checkMesh"] == "PASS"
    assert prov["hull_layer_coverage_pct"] == 96.1
    assert set(prov["inputs"]) == set(ms.mesh_inputs(a))
    assert (entry.path / "inputs" / "system" / "snappyHexMeshDict").is_file()
    assert (entry.path / "logs" / "log.snappyHexMesh").is_file()
    # the master is read-only
    assert not os.access(entry.polymesh / "owner", os.W_OK)

    sibling = make_case(tmp_path, "db_G04_msR", with_mesh=False)
    assert store.find(sibling).path == entry.path
    store.link(sibling, entry.identity)
    assert (sibling / "constant" / "polyMesh" / "owner").is_file()
    assert (sibling / "log.snappyHexMesh").is_file()  # gates read the build logs
    ok, msg = store.verify(sibling)
    assert ok and msg.startswith("OK")


def test_promote_refuses_duplicate_identity_and_links(tmp_path):
    store = ms.MeshStore(tmp_path)
    a = make_case(tmp_path, "a")
    store.promote(a, "a")
    b = make_case(tmp_path, "b")
    with pytest.raises(ms.MeshStoreError, match="already in store"):
        store.promote(b, "b")
    with pytest.raises(ms.MeshStoreError, match="already a link"):
        store.promote(a, "again")


def test_dedupe_requires_byte_identical_mesh(tmp_path):
    store = ms.MeshStore(tmp_path)
    entry = store.promote(make_case(tmp_path, "a"), "a")
    same = make_case(tmp_path, "b")
    (same / "constant" / "polyMesh" / "sets" / "nonOrthoFaces").write_text("different diagnostics\n")
    assert store.dedupe(same, entry.path) is True
    assert (same / "constant" / "polyMesh").is_symlink()
    assert store.dedupe(same, entry.path) is False  # already linked
    other = make_case(tmp_path, "c", ncells=999)
    with pytest.raises(ms.MeshStoreError, match="differs"):
        store.dedupe(other, entry.path)
    assert not (other / "constant" / "polyMesh").is_symlink()


def test_verify_reports_drifted_inputs(tmp_path):
    store = ms.MeshStore(tmp_path)
    a = make_case(tmp_path, "a")
    store.promote(a, "a")
    (a / "system" / "snappyHexMeshDict").write_text(SNAPPY.replace("nSurfaceLayers 6", "nSurfaceLayers 8"))
    ok, msg = store.verify(a)
    assert not ok and msg.startswith("MISMATCH")


def test_status_and_drop(tmp_path):
    store = ms.MeshStore(tmp_path)
    entry = store.promote(make_case(tmp_path, "a"), "a")
    rows = store.status()
    assert rows == [{"master": entry.path.name, "cells": 1234, "checkMesh": "PASS", "linked_cases": ["a"]}]
    with pytest.raises(ms.MeshStoreError, match="still links"):
        store.drop(entry.path)
    (tmp_path / "cases" / "a" / "constant" / "polyMesh").unlink()
    store.drop(entry.identity)
    assert not entry.path.exists()
    assert store.status() == []


def test_link_refuses_to_shadow_a_private_mesh(tmp_path):
    store = ms.MeshStore(tmp_path)
    entry = store.promote(make_case(tmp_path, "a"), "a")
    b = make_case(tmp_path, "b")
    with pytest.raises(ms.MeshStoreError, match="real directory"):
        store.link(b, entry.path)


def test_cli_id_find_status(tmp_path, capsys):
    a = make_case(tmp_path, "a")
    assert ms.main(["--root", str(tmp_path), "id", "a"]) == 0
    ident = capsys.readouterr().out.strip()
    assert ms.main(["--root", str(tmp_path), "find", "a"]) == 1
    assert ms.main(["--root", str(tmp_path), "promote", "a", "tagA"]) == 0
    assert ms.main(["--root", str(tmp_path), "find", "a"]) == 0
    assert capsys.readouterr().out.strip().endswith(f"{ident}-tagA")
    assert ms.main(["--root", str(tmp_path), "status"]) == 0
    assert "tagA" in capsys.readouterr().out


# ---------------------------------------------------------------------------
# bash runtime agreement


@pytest.mark.skipif(not (shutil.which("bash") and shutil.which("sha256sum")), reason="needs bash + coreutils")
def test_bash_runtime_agrees_with_library_identity(tmp_path):
    """The solve host runs scripts/cfd/mesh_store.sh; the library is what the
    case builder and tests use. One identity rule, checked here, not assumed."""
    assert BASH_SCRIPT.is_file(), BASH_SCRIPT
    case = make_case(tmp_path, "agree")
    env = dict(os.environ, DM_CFD_ROOT=str(tmp_path))
    out = subprocess.run(["bash", str(BASH_SCRIPT), "id", "agree"], env=env, capture_output=True, text=True, check=True)
    assert out.stdout.strip() == ms.mesh_identity(case)


@pytest.mark.skipif(not (shutil.which("bash") and shutil.which("sha256sum")), reason="needs bash + coreutils")
def test_bash_promote_is_readable_by_library(tmp_path):
    case = make_case(tmp_path, "agree")
    env = dict(os.environ, DM_CFD_ROOT=str(tmp_path))
    subprocess.run(["bash", str(BASH_SCRIPT), "promote", "agree", "agree"], env=env, capture_output=True, text=True, check=True)
    store = ms.MeshStore(tmp_path)
    entry = store.find(case)
    assert entry is not None and entry.tag == "agree"
    assert entry.provenance()["cells"] == 1234
    ok, _ = store.verify(case)
    assert ok
