"""Synthetic W0 regression: public geometry cannot diverge from its digest."""
import numpy as np
import pytest

from digitalmodel.naval_architecture.mesh_hydrostatics import (
    ClippedHull, Quantity, TriMesh, box_mesh, clip_at_waterline, compute_hydrostatics,
)


def assert_immutable(array):
    """Check assignment, write-flag restoration, views and all ndarray bases."""
    assert not array.flags.writeable
    with pytest.raises(ValueError):
        array.flat[0] = 0
    for exposed in (array, array.view(), array.reshape(-1)):
        with pytest.raises(ValueError):
            exposed.setflags(write=True)
    base = array.base
    while isinstance(base, np.ndarray):
        with pytest.raises(ValueError):
            base.setflags(write=True)
        base = base.base


def test_box_beam_cannot_double_with_unchanged_hash():
    vertices, faces = box_mesh(20.0, 4.0, 3.0)
    mesh = TriMesh(vertices, faces, units="m", axes=("forward", "port", "up"))
    before = compute_hydrostatics(mesh, draft=1.5)
    digest = mesh.source_digest
    assert before["V"].value == pytest.approx(120.0)
    with pytest.raises(ValueError):
        mesh.vertices.setflags(write=True)
    with pytest.raises(ValueError):
        mesh.vertices[:, 1] *= 2
    for array in (mesh.vertices, mesh.faces, *mesh.bounds):
        assert_immutable(array)
    vertices[:, 1] *= 2
    faces[:] = 0
    after = compute_hydrostatics(mesh, draft=1.5)
    assert mesh.source_digest == digest
    assert before.to_dict() == after.to_dict()
    assert after["V"].value == pytest.approx(120.0)
    assert after["B_wl"].value == pytest.approx(4.0)


def test_clipped_hull_arrays_are_immutable_and_isolated():
    mesh = TriMesh(*box_mesh(20, 4, 3), units="m", axes=("forward", "port", "up"))
    clipped = clip_at_waterline(mesh, 1.5)
    names = ("vertices", "faces", "artificial", "waterline_points", "plane_point",
             "plane_normal", "waterplane_x_axis")
    originals = [getattr(clipped, name).copy() for name in names]
    constructed = ClippedHull(*originals)
    for name, caller_array in zip(names, originals):
        expected = caller_array.copy()
        caller_array[:] = 0
        np.testing.assert_array_equal(getattr(constructed, name), expected)
        assert_immutable(getattr(constructed, name))
        assert_immutable(getattr(clipped, name))
    assert_immutable(clipped.face_areas())


def test_quantity_array_and_grid_reads_preserve_result_identity():
    caller_array = np.array([1.0, 2.0])
    quantity = Quantity(caller_array, "m", "computed", "synthetic")
    caller_array[:] = 0
    np.testing.assert_array_equal(quantity.value, [1.0, 2.0])
    assert_immutable(quantity.value)
    mesh = TriMesh(*box_mesh(20, 4, 3), units="m", axes=("forward", "port", "up"))
    stations, waterlines = np.array([0.0]), np.array([0.5, 1.0])
    result = compute_hydrostatics(mesh, 1.5, grid_stations=stations,
                                grid_waterlines=waterlines)
    expected = result.to_dict()
    result["half_breadth"].value[0][0] = 99
    result.to_dict()["quantities"]["half_breadth"]["value"][0][0] = 99
    stations[:] = 8
    waterlines[:] = 2
    assert result.to_dict() == expected
    assert result["half_breadth"].value == [[2.0, 2.0]]
    assert all(q.input_hash == result.input_hash for q in result.values())


def test_geometry_metadata_cannot_be_rebound():
    mesh = TriMesh(*box_mesh(20, 4, 3), units="m", axes=("forward", "port", "up"))
    assert mesh.source_digest == "b1068466fc4d686a7643fc10a40d153bc134de22856cdf7752eb9c7b7386248d"
    for name in ("vertices", "faces", "source_digest", "scale_diag", "units", "axes"):
        with pytest.raises(AttributeError):
            setattr(mesh, name, None)
    clipped = clip_at_waterline(mesh, 1.5)
    with pytest.raises(AttributeError):
        clipped.vertices = clipped.vertices * 2


def test_factory_outputs_are_defensive_authoring_arrays():
    from digitalmodel.naval_architecture.mesh_hydrostatics import wigley_mesh
    for factory, args in ((box_mesh, (20, 4, 3)), (wigley_mesh, (100, 10, 6.25))):
        first = factory(*args)
        second = factory(*args)
        for a, b in zip(first, second):
            assert not np.shares_memory(a, b)
            expected = b.copy()
            a[:] = 0
            np.testing.assert_array_equal(b, expected)


def test_quantity_list_input_is_isolated_and_object_array_is_refused():
    values = [[1.0, 2.0]]
    quantity = Quantity(values, "m", "computed", "synthetic")
    values[0][0] = 99
    assert quantity.value == [[1.0, 2.0]]
    with pytest.raises(TypeError, match="object"):
        Quantity(np.array([object()], dtype=object), "1", "declared", "synthetic")


def test_noncontiguous_inputs_preserve_digest_and_quantity_shape():
    vertices, faces = box_mesh(20, 4, 3)
    ordinary = TriMesh(vertices, faces, units="m", axes=("forward", "port", "up"))
    fortran = TriMesh(np.asfortranarray(vertices), np.asfortranarray(faces),
                      units="m", axes=("forward", "port", "up"))
    assert fortran.source_digest == ordinary.source_digest
    caller_array = np.arange(12.0).reshape(3, 4).T[:, ::2]
    quantity = Quantity(caller_array, "m", "computed", "synthetic")
    np.testing.assert_array_equal(quantity.value, caller_array)
    assert quantity.value.shape == caller_array.shape
    assert_immutable(quantity.value)
    empty = Quantity(np.empty((0, 3)), "m", "computed", "synthetic").value
    assert empty.shape == (0, 3)
    with pytest.raises(ValueError):
        empty.setflags(write=True)


def test_public_array_metadata_cannot_change_retained_geometry():
    mesh = TriMesh(*box_mesh(20, 4, 3), units="m", axes=("forward", "port", "up"))
    before = compute_hydrostatics(mesh, 1.5).to_dict()
    exposed = mesh.vertices
    exposed.shape = (exposed.size,)
    assert mesh.vertices.shape == (8, 3)
    exposed = mesh.faces
    exposed.dtype = np.uint8
    assert mesh.faces.dtype == np.int64
    assert compute_hydrostatics(mesh, 1.5).to_dict() == before
    clipped = clip_at_waterline(mesh, 1.5)
    original_shape = clipped.vertices.shape
    exposed = clipped.vertices
    exposed.shape = (exposed.size,)
    assert clipped.vertices.shape == original_shape
    quantity = Quantity(np.ones((2, 3)), "m", "computed", "synthetic")
    exposed = quantity.value
    exposed.shape = (6,)
    assert quantity.value.shape == (2, 3)


@pytest.mark.parametrize("roundtrip", ["deepcopy", "pickle"])
def test_copying_preserves_immutable_arrays_and_provenance(roundtrip):
    import copy
    import pickle
    clone = copy.deepcopy if roundtrip == "deepcopy" else lambda obj: pickle.loads(pickle.dumps(obj))
    mesh = TriMesh(*box_mesh(20, 4, 3), units="m", axes=("forward", "port", "up"))
    copied = clone(mesh)
    for array in (copied.vertices, copied.faces):
        assert_immutable(array)
    assert compute_hydrostatics(copied, 1.5).to_dict() == compute_hydrostatics(mesh, 1.5).to_dict()
    clipped = clone(clip_at_waterline(mesh, 1.5))
    for name in ("vertices", "faces", "artificial", "waterline_points", "plane_point",
                 "plane_normal", "waterplane_x_axis"):
        assert_immutable(getattr(clipped, name))
    quantity = clone(Quantity(np.ones((2, 3)), "m", "computed", "synthetic"))
    assert_immutable(quantity.value)
    result = clone(compute_hydrostatics(mesh, 1.5))
    result.quantities["V"] = Quantity(240.0, "m^3", "computed", result.input_hash)
    assert result["V"].value == pytest.approx(120.0)


def test_result_mappings_and_quantity_dataclass_compatibility():
    from dataclasses import asdict, replace
    mesh = TriMesh(*box_mesh(20, 4, 3), units="m", axes=("forward", "port", "up"))
    result = compute_hydrostatics(mesh, 1.5)
    expected = result.to_dict()
    for mapping in (result.quantities, result.conventions):
        key = next(iter(mapping))
        mapping[key] = None
        del mapping[key]
    assert result.to_dict() == expected
    q = Quantity([[2.0]], "m", "computed", "synthetic")
    assert replace(q, reason="copied").value == q.value
    assert asdict(q)["value"] == [[2.0]]
    assert "value=[[2.0]]" in repr(q)


def test_masked_quantity_is_refused_without_silently_losing_mask():
    with pytest.raises(TypeError, match="subclass"):
        Quantity(np.ma.array([1.0, 2.0], mask=[True, False]), "m", "computed", "synthetic")


def test_result_dataclass_serialization_preserves_native_dict_api():
    from dataclasses import asdict, astuple
    mesh = TriMesh(*box_mesh(20, 4, 3), units="m", axes=("forward", "port", "up"))
    result = compute_hydrostatics(mesh, 1.5)
    assert asdict(result)["quantities"]["V"]["value"] == pytest.approx(120.0)
    assert astuple(result)[0]["V"][0] == pytest.approx(120.0)


def test_snapshot_dataclasses_do_not_expose_storage_through_vars():
    mesh = TriMesh(*box_mesh(20, 4, 3), units="m", axes=("forward", "port", "up"))
    for obj in (clip_at_waterline(mesh, 1.5), Quantity(np.ones(3), "m", "computed", "synthetic")):
        with pytest.raises(TypeError):
            vars(obj)
