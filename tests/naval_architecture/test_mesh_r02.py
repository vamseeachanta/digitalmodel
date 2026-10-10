"""R02 clipping, bounded station work and decomposition regressions (analytic data)."""
import ast
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.naval_architecture import mesh_hydrostatics as hydro


def test_post_clip_physical_degeneracy_is_refused(monkeypatch):
    from digitalmodel.naval_architecture import mesh_clipping

    mesh = hydro.TriMesh(*hydro.box_mesh(20, 4, 3), units="m", axes=hydro.CANONICAL_AXES)
    original = mesh_clipping._clip_keep_below

    def collapsed(*args, **kwargs):
        vertices, faces, art, boundary, b_art = original(*args, **kwargs)
        vertices[faces[0, 2]] = vertices[faces[0, 1]]
        return vertices, faces, art, boundary, b_art

    monkeypatch.setattr(mesh_clipping, "_clip_keep_below", collapsed)
    with pytest.raises(hydro.MeshContractError, match="post-clip.*degenerate.*physical"):
        hydro.clip_at_waterline(mesh, draft=1.5)


def test_resolved_cut_slivers_remain_valid():
    mesh = hydro.TriMesh(*hydro.box_mesh(20, 4, 3), units="m", axes=hydro.CANONICAL_AXES)
    clipped = hydro.clip_at_waterline(mesh, draft=1e-6)
    assert hydro.volume_tetra(clipped.vertices, clipped.faces) == pytest.approx(80e-6)


def test_legacy_identity_and_pickle():
    import pickle

    mesh = hydro.TriMesh(*hydro.box_mesh(20, 4, 3), units="m", axes=hydro.CANONICAL_AXES)
    assert mesh.source_digest == "b1068466fc4d686a7643fc10a40d153bc134de22856cdf7752eb9c7b7386248d"
    result = hydro.compute_hydrostatics(mesh, 1.5)
    assert result.input_hash == "5abd9f58725d7e536de6c63021e69b596fb540ab8604b294330e3c9b69013aae"
    for cls in (hydro.TriMesh, hydro.ClippedHull, hydro.Quantity, hydro.HydrostaticsResult):
        assert cls.__module__ == hydro.__name__
    restored = pickle.loads(pickle.dumps(result))
    assert restored.to_dict() == result.to_dict()


def test_baseline_pickles_load_with_immutable_arrays():
    import base64
    import json
    import pickle
    import hashlib

    fixture = Path(__file__).parents[1] / "fixtures/test_vectors/naval_architecture/mesh_legacy_pickles.json"
    digest = hashlib.sha256(fixture.read_bytes()).hexdigest()
    assert digest == "32c56eadd96486a65dd48cfce8ab66fc501b8a9e851f7a46a23212996ea3e197"
    record = json.loads(fixture.read_text())
    assert record["source_revision"] == "4f7bfc0cb4d9f62d7485fe5b03fb00550b2320eb"
    restored = {key: pickle.loads(base64.b64decode(data))
                for key, data in record["pickles"].items()}
    assert restored["result"]["V"].value == pytest.approx(120, abs=1e-9)
    assert hydro.compute_hydrostatics(restored["mesh"], 1.5)["V"].value == pytest.approx(120, abs=1e-9)
    for key in ("mesh", "clipped"):
        with pytest.raises(ValueError):
            restored[key].vertices.setflags(write=True)


@pytest.mark.parametrize("offset", [0.0, 1e6])
def test_small_resolved_cut_is_translation_invariant(offset):
    vertices, faces = hydro.box_mesh(20, 4, 3)
    vertices[:, 0] += offset
    mesh = hydro.TriMesh(vertices, faces, units="m", axes=hydro.CANONICAL_AXES)
    clipped = hydro.clip_at_waterline(mesh, draft=1e-6)
    assert hydro.volume_tetra(clipped.vertices, clipped.faces) == pytest.approx(80e-6)


def test_physical_resolution_floor_and_artificial_fan_exemption():
    from digitalmodel.naval_architecture.mesh_clipping import _check_physical_faces

    eps = np.finfo(float).eps
    faces = np.array([[0, 1, 2]])
    for height, refused in ((eps, True), (8 * eps, False), (0.0, True)):
        vertices = np.array([[0, 0, 0], [1, 0, 0], [0, height, 0]])
        if refused:
            with pytest.raises(hydro.MeshContractError, match="physical faces"):
                _check_physical_faces(vertices, faces, np.array([False]))
        else:
            _check_physical_faces(vertices, faces, np.array([False]))
        _check_physical_faces(vertices, faces, np.array([True]))


def test_repeated_stations_compute_once(monkeypatch):
    from digitalmodel.naval_architecture import mesh_sections

    mesh = hydro.TriMesh(*hydro.box_mesh(20, 4, 3), units="m", axes=hydro.CANONICAL_AXES)
    original = mesh_sections._section
    calls = []

    def counted(*args, **kwargs):
        calls.append(args[1])
        return original(*args, **kwargs)

    monkeypatch.setattr(mesh_sections, "_section", counted)
    result = hydro.compute_hydrostatics(
        mesh, draft=1.5, bulb_station=0, transom_station=0,
        grid_stations=[0, 0, -10, 10, -10], grid_waterlines=[0.5, 1.5],
    )
    assert len(calls) == 3
    assert result["A_BT"].value == result["A_T"].value == pytest.approx(6, abs=1e-9)
    np.testing.assert_allclose(result["half_breadth"].value, np.full((5, 2), 2))


def test_station_clipper_receives_compact_vertices(monkeypatch):
    from digitalmodel.naval_architecture import mesh_sections

    mesh = hydro.TriMesh(*hydro.wigley_mesh(100, 10, 6.25, nx=80, nz=20),
                         units="m", axes=hydro.CANONICAL_AXES)
    clipped = hydro.clip_at_waterline(mesh, draft=6.25)
    original = mesh_sections._clip_keep_below
    sizes = []

    def counted(vertices, faces, *args):
        sizes.append(len(vertices))
        assert faces.max() < len(vertices)
        return original(vertices, faces, *args)

    monkeypatch.setattr(mesh_sections, "_clip_keep_below", counted)
    index = mesh_sections.SectionIndex(clipped, 0, 1e-10)
    area, _ = index.section(20.3)
    assert area > 0
    assert max(sizes) < len(clipped.vertices) / 10


def _full_body_section(clipped, station, midship, eps):
    from digitalmodel.naval_architecture.mesh_clipping import _clip_keep_below, _cap_area_vector

    sign = -1 if midship >= station else 1
    normal = np.array([sign, 0, 0])
    vertices, _, _, boundary, artificial = _clip_keep_below(
        clipped.vertices, clipped.faces, clipped.artificial, normal, sign * station, eps)
    if not len(boundary):
        return 0.0, np.zeros((0, 2, 3))
    _, av = _cap_area_vector(vertices, boundary)
    segments = vertices[boundary[~artificial]]
    return float(av @ normal), segments


def test_indexed_sections_match_full_body_cut_at_endpoints_and_near_vertices():
    from digitalmodel.naval_architecture.mesh_sections import SectionIndex

    vertices, faces = hydro.box_mesh(20, 4, 3)
    vertices[vertices[:, 0] > 0, 1] *= 1.25
    mesh = hydro.TriMesh(vertices, faces, units="m", axes=hydro.CANONICAL_AXES)
    clipped = hydro.clip_at_waterline(mesh, 1.5, trim=0.2)
    eps = 1e-12 * mesh.scale_diag
    index = SectionIndex(clipped, 0, eps)
    stations = [-10, -10 + eps / 2, 0, np.nextafter(0.0, 1.0), 10 - eps / 2, 10]
    for station in stations:
        expected, expected_segments = _full_body_section(clipped, station, 0, eps)
        area, segments = index.section(station)
        assert area == pytest.approx(expected, rel=1e-10, abs=1e-9)
        assert sorted(map(tuple, segments.reshape(-1, 6))) == pytest.approx(
            np.array(sorted(map(tuple, expected_segments.reshape(-1, 6)))), abs=1e-9)
        with pytest.raises(ValueError):
            segments.setflags(write=True)
    assert len(index._cache) == len(stations)


@pytest.mark.parametrize("shape", ["wigley", "twin_box"])
def test_fine_and_twin_sections_match_full_body(shape):
    from digitalmodel.naval_architecture.mesh_sections import SectionIndex

    if shape == "wigley":
        vertices, faces = hydro.wigley_mesh(100, 10, 6.25, nx=60, nz=20)
    else:
        a, f = hydro.box_mesh(100, 4, 9)
        b = a.copy()
        a[:, 1] -= 4
        b[:, 1] += 4
        vertices, faces = np.vstack([a, b]), np.vstack([f, f + len(a)])
    vertices[:, 0] += 1e6 + 0.12345
    mesh = hydro.TriMesh(vertices, faces, units="m", axes=hydro.CANONICAL_AXES)
    midship = 1e6 + 0.12345
    clipped = hydro.clip_at_waterline(mesh, 6.25, trim=0.12345)
    eps = 1e-12 * mesh.scale_diag
    index = SectionIndex(clipped, midship, eps)
    for relative in (-50, -37.3, -25 + eps * 4, 0, 11.1, 25 - eps * 4, 50):
        station = midship + relative
        expected, _ = _full_body_section(clipped, station, midship, eps)
        assert index.section(station)[0] == pytest.approx(expected, rel=1e-10, abs=1e-9)


def test_mesh_decomposition_limits_and_public_imports():
    root = Path(hydro.__file__).parent
    modules = ["mesh_hydrostatics", "mesh_geometry", "mesh_topology", "mesh_clipping",
               "mesh_sections", "mesh_results", "mesh_validation"]
    for name in modules:
        source = (root / f"{name}.py").read_text()
        assert len(source.splitlines()) <= 400, name
        for node in ast.walk(ast.parse(source)):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                assert node.end_lineno - node.lineno + 1 <= 50, (name, node.name)
    for name in ("TriMesh", "ClippedHull", "Quantity", "HydrostaticsResult",
                 "clip_at_waterline", "volume_tetra", "volume_divergence", "box_mesh",
                 "wigley_mesh", "wigley_half_breadth", "wigley_wetted_area_reference"):
        assert callable(getattr(hydro, name))


def test_baseline_wildcard_public_surface_is_preserved():
    # Literal public namespace from hash-pinned source at 4f7bfc0c.
    baseline = {
        "CANONICAL_AXES", "ClippedHull", "HydrostaticsResult", "Iterator", "Mapping",
        "MeshContractError", "Optional", "Quantity", "SCHEMA_VERSION", "Sequence",
        "TriMesh", "UNIT_SCALE", "annotations", "box_mesh", "clip_at_waterline",
        "compute_hydrostatics", "dataclass", "deepcopy", "field", "hashlib", "json",
        "math", "np", "volume_divergence", "volume_tetra", "wigley_half_breadth",
        "wigley_mesh", "wigley_wetted_area_reference",
    }
    namespace = {}
    exec("from digitalmodel.naval_architecture.mesh_hydrostatics import *", namespace)
    assert baseline <= namespace.keys()
