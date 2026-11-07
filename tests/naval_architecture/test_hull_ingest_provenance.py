# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Route evidence for synthetic render-meshed and mesh-free reader fixtures.

The reader double implements the File3dm/BrepFace.GetMesh interface consumed
by hull_ingest (repository source, read_3dm_triangles and mesh inventory).
Only file decoding and the CAD kernel are replaced; dispatch, mesh extraction,
normalisation and manifest writing run normally. No CAD dependency is needed.

Independent hand calculation: a tetrahedron with axis intercepts 6, 2, 2 m
has four triangular faces, four vertices and volume 6*2*2/6 = 4 m3.
These are synthetic inputs, not engineering limits. Full cached coverage is
4/4; retaining two faces gives 2/4. Test deflections are arbitrary caller
inputs; the stub's effective default is deliberately distinct from None.
"""

from __future__ import annotations

import json
from dataclasses import replace
from types import SimpleNamespace
from unittest.mock import Mock

import pytest

from digitalmodel.naval_architecture import brep_tessellate as bt
from digitalmodel.naval_architecture import hull_ingest as hi


POINTS = [(0.0, 0.0, 0.0), (6.0, 0.0, 0.0), (0.0, 2.0, 0.0), (0.0, 0.0, 2.0)]
FACES = [(0, 2, 1), (0, 1, 3), (0, 3, 2), (1, 2, 3)]
TRIANGLES = [tuple(POINTS[i] for i in face) for face in FACES]


class Mesh:
    def __init__(self, triangles):
        self.Vertices = [
            SimpleNamespace(X=x, Y=y, Z=z) for tri in triangles for x, y, z in tri
        ]
        self.Faces = [(i, i + 1, i + 2, i + 2) for i in range(0, len(self.Vertices), 3)]


class Brep:
    def __init__(self, cached_faces):
        self.Faces = [
            SimpleNamespace(GetMesh=lambda _, m=mesh: m)
            for mesh in [
                Mesh([tri]) if i < cached_faces else None
                for i, tri in enumerate(TRIANGLES)
            ]
        ]


class Extrusion:
    def __init__(self, cached_faces):
        self.brep = Brep(cached_faces)

    def ToBrep(self, _):
        return self.brep


@pytest.fixture
def reader_fixture(tmp_path, monkeypatch):
    """Install an in-memory .3dm decoder and an observable kernel result."""
    source = tmp_path / "synthetic.3dm"
    source.write_bytes(b"synthetic reader fixture")
    model = SimpleNamespace(
        # Source: hull_ingest.RHINO_UNIT_CODES, metres enumeration.
        Settings=SimpleNamespace(ModelUnitSystem=4),
        Layers=[SimpleNamespace(Name="Hull"), SimpleNamespace(Name="Excluded")],
        Objects=[],
    )
    decoder = SimpleNamespace(
        File3dm=SimpleNamespace(Read=lambda _: model),
        MeshType=SimpleNamespace(Any="any"),
    )
    monkeypatch.setattr(hi, "_import_rhino3dm", lambda: decoder)
    result = bt.BrepTessellation(
        triangles=TRIANGLES,
        declared_units="m",
        face_count=4,
        faces_converted=4,
        linear_deflection=0.125,
        layer_faces={"Hull": 4},
    )
    kernel = Mock(return_value=result)
    monkeypatch.setattr(bt, "tessellate_3dm", kernel)

    def install(geometry, *, excluded=None):
        model.Objects = [
            SimpleNamespace(Geometry=geometry, Attributes=SimpleNamespace(LayerIndex=0))
        ]
        if excluded is not None:
            model.Objects.append(
                SimpleNamespace(
                    Geometry=excluded, Attributes=SimpleNamespace(LayerIndex=1)
                )
            )
        return source, kernel

    return install


@pytest.mark.parametrize("fallback", [False, True])
def test_cached_manifest_never_claims_a_rebuild(reader_fixture, tmp_path, fallback):
    """Four cached triangular faces => 4/4 coverage; kernel calls = zero."""
    source, kernel = reader_fixture(Brep(4))
    manifest = hi.ingest_3dm(
        source,
        tmp_path / "out",
        forward="+x",
        tessellate_breps=fallback,
        linear_deflection=0.25,
    )
    record = manifest.to_dict()["provenance"]["tessellation"]
    assert record == {
        "source": "cached-render-mesh",
        "cached_mesh_face_coverage": {"meshed": 4, "total": 4},
    }
    assert manifest.displacement_m3 == pytest.approx(4.0)
    assert "read 4 triangles via cached-render-mesh" in manifest.notes
    assert not any("were rebuilt" in note for note in manifest.notes)
    unused_note = (
        "CAD fallback was enabled but unused; linear_deflection was "
        "not applied. Varying it does not test tessellation convergence."
    )
    assert (unused_note in manifest.notes) is fallback
    kernel.assert_not_called()
    on_disk = json.loads((tmp_path / "out" / "hull_manifest.json").read_text())
    assert on_disk["provenance"]["tessellation"] == record


@pytest.mark.parametrize("requested", [None, 0.25])
def test_mesh_free_manifest_records_effective_kernel_deflection(
    reader_fixture,
    tmp_path,
    requested,
):
    """A stub returns 0.125 file units, even when the request was None/0.25."""
    source, kernel = reader_fixture(Brep(0))
    manifest = hi.ingest_3dm(
        source,
        tmp_path / "out",
        forward="+x",
        tessellate_breps=True,
        linear_deflection=requested,
    )
    record = manifest.to_dict()["provenance"]["tessellation"]
    assert record == {
        "source": "occ-rebuild",
        "cached_mesh_face_coverage": {"meshed": 0, "total": 4},
        "linear_deflection": 0.125,
    }
    assert any("were rebuilt" in note for note in manifest.notes)
    assert "read 4 triangles via occ-rebuild" in manifest.notes
    kernel.assert_called_once_with(source, layers=None, linear_deflection=requested)
    assert manifest.displacement_m3 == pytest.approx(4.0)
    on_disk = json.loads((tmp_path / "out" / "hull_manifest.json").read_text())
    assert on_disk["provenance"]["tessellation"] == record


def test_mesh_free_reader_still_requires_opt_in(reader_fixture):
    source, kernel = reader_fixture(Brep(0))
    with pytest.raises(hi.NoMeshGeometryError):
        hi.read_3dm_triangles(source)
    kernel.assert_not_called()


@pytest.mark.parametrize("kind", [Brep, Extrusion])
def test_partial_coverage_is_counted_after_layer_selection(reader_fixture, kind):
    """Two selected cached faces / four selected faces; excluded faces add zero."""
    source, kernel = reader_fixture(kind(2), excluded=Brep(4))
    geometry = hi.read_3dm_triangles(source, layers=["Hull"], tessellate_breps=True)
    assert geometry.source == "cached-render-mesh"
    assert geometry.to_provenance()["cached_mesh_face_coverage"] == {
        "meshed": 2,
        "total": 4,
    }
    assert len(geometry.triangles) == 2
    kernel.assert_not_called()


def test_native_mesh_is_not_reported_as_a_cached_brep(reader_fixture):
    """A standalone mesh has no Brep faces, so cached face coverage is 0/0."""
    source, kernel = reader_fixture(Mesh(TRIANGLES))
    geometry = hi.read_3dm_triangles(source, tessellate_breps=True)
    assert geometry.source == "mesh-object"
    assert geometry.to_provenance() == {
        "source": "mesh-object",
        "cached_mesh_face_coverage": {"meshed": 0, "total": 0},
    }
    kernel.assert_not_called()


def test_native_and_cached_sources_are_both_recorded(reader_fixture):
    source, kernel = reader_fixture(Brep(4), excluded=Mesh(TRIANGLES))
    geometry = hi.read_3dm_triangles(source, tessellate_breps=True)
    assert geometry.source == "cached-render-mesh+mesh-object"
    assert geometry.to_provenance()["cached_mesh_face_coverage"] == {
        "meshed": 4,
        "total": 4,
    }
    kernel.assert_not_called()


def test_cached_deflection_sweep_exposes_the_inert_parameter(reader_fixture):
    """Arbitrary input deflections differ by a factor of two; caches do not."""
    source, kernel = reader_fixture(Brep(4))
    coarse = hi.read_3dm_triangles(
        source, tessellate_breps=True, linear_deflection=0.25
    )
    fine = hi.read_3dm_triangles(source, tessellate_breps=True, linear_deflection=0.125)
    assert coarse.triangles == fine.triangles
    assert coarse.source == fine.source == "cached-render-mesh"
    assert "linear_deflection" not in coarse.to_provenance()
    assert "linear_deflection" not in fine.to_provenance()
    kernel.assert_not_called()


def test_selected_unmeshed_object_is_included_in_cached_face_coverage(reader_fixture):
    """A fully cached tetrahedron plus a mesh-free one has 4/8 cached faces."""
    source, kernel = reader_fixture(Brep(4), excluded=Brep(0))
    geometry = hi.read_3dm_triangles(source, tessellate_breps=True)
    assert geometry.to_provenance()["cached_mesh_face_coverage"] == {
        "meshed": 4,
        "total": 8,
    }
    assert geometry.objects_without_mesh == 1
    kernel.assert_not_called()


def test_in_memory_ingestion_does_not_invent_a_cad_route(tmp_path):
    manifest = hi.ingest_triangles(
        TRIANGLES, tmp_path, declared_units="m", forward="+x"
    )
    assert "tessellation" not in manifest.to_dict()["provenance"]


def test_appendage_route_is_recorded_separately_from_the_hull(reader_fixture, tmp_path):
    """Cached hull coverage is 4/4; a separate mesh-free region has 0/4.

    Coincident synthetic bodies exercise evidence propagation only; no union
    area acceptance limit is inferred from this artificial assembly.
    """
    source, kernel = reader_fixture(Brep(4), excluded=Brep(0))
    manifest = hi.ingest_3dm(
        source,
        tmp_path / "out",
        layers=["Hull"],
        forward="+x",
        tessellate_breps=True,
        appendage_layers={"fin": ["Excluded"]},
        subdivision_depth=0,
    )
    provenance = manifest.to_dict()["provenance"]
    assert provenance["tessellation"]["source"] == "cached-render-mesh"
    assert provenance["appendage_tessellation"]["fin"] == {
        "source": "occ-rebuild",
        "cached_mesh_face_coverage": {"meshed": 0, "total": 4},
        "linear_deflection": 0.125,
    }
    kernel.assert_called_once_with(source, layers=["Excluded"], linear_deflection=None)


@pytest.mark.parametrize("contributing_faces", [0, 2])
def test_cached_coverage_counts_only_meshes_that_contribute_triangles(
    reader_fixture, monkeypatch, contributing_faces
):
    """Four nonempty caches; extraction returns triangles for zero or two."""
    source, kernel = reader_fixture(Brep(4))
    extract = hi._triangles_of_mesh

    def extract_selected(mesh):
        triangles = extract(mesh)
        return [tri for tri in triangles if tri in TRIANGLES[:contributing_faces]]

    monkeypatch.setattr(hi, "_triangles_of_mesh", extract_selected)
    geometry = hi.read_3dm_triangles(source, tessellate_breps=True)
    assert geometry.to_provenance()["cached_mesh_face_coverage"] == {
        "meshed": contributing_faces,
        "total": 4,
    }
    if contributing_faces:
        assert geometry.source == "cached-render-mesh"
        assert len(geometry.triangles) == contributing_faces
        kernel.assert_not_called()
    else:
        assert geometry.source == "occ-rebuild"
        kernel.assert_called_once_with(source, layers=None, linear_deflection=None)


def test_ingestion_copies_nested_hull_and_appendage_evidence(tmp_path):
    hull = {
        "source": "cached-render-mesh",
        "cached_mesh_face_coverage": {"meshed": 4, "total": 4},
    }
    appendage = {
        "source": "occ-rebuild",
        "cached_mesh_face_coverage": {"meshed": 0, "total": 4},
    }
    manifest = hi.ingest_triangles(
        TRIANGLES,
        tmp_path,
        declared_units="m",
        forward="+x",
        tessellation=hull,
        appendage_tessellation={"fin": appendage},
    )
    hull["cached_mesh_face_coverage"]["meshed"] = 1
    appendage["cached_mesh_face_coverage"]["total"] = 8
    assert manifest.tessellation["cached_mesh_face_coverage"]["meshed"] == 4
    assert manifest.appendage_tessellation["fin"]["cached_mesh_face_coverage"] == {
        "meshed": 0,
        "total": 4,
    }


def test_serialization_copies_nested_hull_and_appendage_evidence(tmp_path):
    manifest = hi.ingest_triangles(
        TRIANGLES,
        tmp_path,
        declared_units="m",
        forward="+x",
        tessellation={
            "source": "cached-render-mesh",
            "cached_mesh_face_coverage": {"meshed": 4, "total": 4},
        },
        appendage_tessellation={
            "fin": {
                "source": "occ-rebuild",
                "cached_mesh_face_coverage": {"meshed": 0, "total": 4},
            }
        },
    )
    snapshot = manifest.to_dict()["provenance"]
    snapshot["tessellation"]["cached_mesh_face_coverage"]["meshed"] = 1
    snapshot["appendage_tessellation"]["fin"]["cached_mesh_face_coverage"]["total"] = 8
    fresh = manifest.to_dict()["provenance"]
    assert fresh["tessellation"]["cached_mesh_face_coverage"]["meshed"] == 4
    assert fresh["appendage_tessellation"]["fin"]["cached_mesh_face_coverage"] == {
        "meshed": 0,
        "total": 4,
    }
    manifest.write(tmp_path / "copy.json")
    assert json.loads((tmp_path / "copy.json").read_text())["provenance"] == fresh


@pytest.mark.parametrize("reference_available", [False, True])
def test_rebuild_preserves_fidelity_evidence(
    reader_fixture, tmp_path, reference_available
):
    """Summary values come from TrimFidelity, in unchanged file units."""
    source, kernel = reader_fixture(Brep(0))
    box = ((0.0, 0.0, 0.0), (6.0, 2.0, 2.0))
    reference = box if reference_available else None
    fidelity = bt.TrimFidelity(
        mesh_bbox=box,
        reference_bbox=reference,
        trim_bbox=reference,
        overshoot={"x": 0.0},
        trim_deviation={"x": 0.125} if reference_available else {},
    )
    kernel.return_value = replace(kernel.return_value, fidelity=fidelity)
    manifest = hi.ingest_3dm(
        source, tmp_path / "out", forward="+x", tessellate_breps=True
    )
    expected = {
        "within_reference_box": fidelity.within_reference_box,
        "max_overshoot": fidelity.max_overshoot,
        "max_trim_deviation": fidelity.max_trim_deviation,
        "reference_bbox": [list(point) for point in box] if reference_available else None,
        "trim_bbox": [list(point) for point in box] if reference_available else None,
    }
    assert manifest.to_dict()["provenance"]["tessellation"]["fidelity"] == expected
    on_disk = json.loads((tmp_path / "out" / "hull_manifest.json").read_text())
    assert on_disk["provenance"]["tessellation"]["fidelity"] == expected


def test_rebuild_still_rejects_reference_box_overshoot(reader_fixture, tmp_path):
    source, kernel = reader_fixture(Brep(0))
    box = ((0.0, 0.0, 0.0), (6.0, 2.0, 2.0))
    kernel.return_value = replace(
        kernel.return_value,
        fidelity=bt.TrimFidelity(
            mesh_bbox=box,
            reference_bbox=box,
            trim_bbox=box,
            overshoot={"x": 0.125},
        ),
    )
    with pytest.raises(hi.NotWatertightError, match="trim boundaries were not honoured"):
        hi.ingest_3dm(source, tmp_path / "out", forward="+x", tessellate_breps=True)
    assert not (tmp_path / "out" / "hull_manifest.json").exists()
