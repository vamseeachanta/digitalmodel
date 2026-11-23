"""Offline behavioural checks for the public seed corpus (issue 1700).

Dimensions originate in each entry's provenance. Analytic formulae below
derive volume from those dimensions; no fluid density or vessel mass is assumed.
Box volume tolerance is measured against generated meshes and welded GDF readback.
"""

from collections import Counter
import hashlib
from pathlib import Path
import socket

import numpy as np
import pytest
import yaml

from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import GDFHandler
from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog
from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    HullMeshGenerator,
    MeshGeneratorConfig,
)
from digitalmodel.visualization.design_tools.hull_hydrostatics import HullHydrostatics


ROOT = Path(__file__).resolve().parents[3]
LIBRARY = ROOT / "data" / "hull_library"
# Ten times the largest generated/readback relative volume error for the boxes.
BOX_VOLUME_RELATIVE_TOLERANCE = 6.502887117676437e-13
HULL_IDS = (
    "unit_box", "generic_barge", "generic_tanker", "rebuilt_wamit_test04_barge",
    "rebuilt_wamit_test16_barge", "rebuilt_wamit_test01_cylinder",
)


@pytest.fixture(autouse=True)
def prohibit_network(monkeypatch):
    def reject_connection(*args, **kwargs):
        raise AssertionError("Seed validation must remain offline")

    monkeypatch.setattr(socket.socket, "connect", reject_connection)
    monkeypatch.setattr(socket, "create_connection", reject_connection)


@pytest.fixture(scope="module")
def registry():
    return yaml.safe_load((LIBRARY / "hull_registry.yaml").read_text())


@pytest.fixture(scope="module")
def catalog():
    return HullCatalog(LIBRARY / "profiles")


@pytest.fixture(params=HULL_IDS)
def entry(request, registry):
    return next(hull for hull in registry["hulls"]
                if hull["hull_id"] == request.param)


@pytest.fixture
def generated(entry, catalog):
    profile = catalog.get_hull(entry["hull_id"]).profile
    # Default resolution is repository-defined; full hull avoids implicit
    # symmetry factors in signed-volume and submerged-boundary checks.
    mesh = HullMeshGenerator().generate(
        profile, MeshGeneratorConfig(symmetry=False, close_ends=True)
    )
    return profile, mesh


def submerged_boundary_edges(mesh):
    """Count unmatched edges below the waterplane, including missing end caps."""
    edges = Counter()
    for panel in mesh.panels:
        indices = list(dict.fromkeys(int(i) for i in panel if i >= 0))
        for a, b in zip(indices, indices[1:] + indices[:1]):
            edges[tuple(sorted((a, b)))] += 1
    return [edge for edge, count in edges.items()
            if count != 2 and np.any(mesh.vertices[list(edge), 2] < 0)]


def signed_mesh_volume(mesh):
    """Triangulate each panel; sum signed tetrahedra with the origin.

    The waterplane closure has zero contribution because z=0. This formula
    is used only after establishing that no submerged boundary is missing.
    """
    volume = 0.0
    for panel in mesh.panels:
        points = mesh.vertices[list(dict.fromkeys(int(i) for i in panel
                                                if i >= 0))]
        for i in range(1, len(points) - 1):
            volume += np.dot(points[0], np.cross(points[i], points[i + 1])) / 6
    return volume


def test_registry_resolves_exactly_the_committed_profiles(registry, catalog):
    ids = [entry["hull_id"] for entry in registry["hulls"]]
    assert len(ids) == len(set(ids))
    assert set(ids) == set(HULL_IDS) == set(catalog.list_hulls())
    assert {ROOT / entry["profile_path"] for entry in registry["hulls"]} == set(
        (LIBRARY / "profiles").glob("*.yaml")
    )


def test_entry_matches_profile_and_records_provenance(entry, catalog):
    profile = catalog.get_hull(entry["hull_id"]).profile
    assert profile == profile.load_yaml(ROOT / entry["profile_path"])
    for key in ("length_bp", "beam", "draft", "block_coefficient", "displacement"):
        assert entry["principal_dimensions"][key] == getattr(profile, key)
    assert entry["principal_dimensions"]["displacement"] is None
    provenance = entry["provenance"]
    for key in ("source", "url", "page_table", "retrieval_date", "reuse_terms",
                "geometry_disposition", "derivation"):
        assert provenance[key]
    assert entry["rao_database_entries"] == []
    assert entry["rao_status"] == "not established"
    for path_key, digest_key in (("profile_path", "profile_sha256"),
                                 ("mesh_files", "mesh_sha256")):
        path = entry[path_key]
        path = path[0] if isinstance(path, list) else path
        assert hashlib.sha256((ROOT / path).read_bytes()).hexdigest() == entry[digest_key]


def test_generated_mesh_passes_existing_quality_checks(entry, generated):
    _, mesh = generated
    report = GDFHandler().validate_mesh(mesh)
    assert report.is_valid, report.errors
    assert report.n_degenerate_panels == 0
    assert report.n_duplicate_vertices == 0
    assert report.has_consistent_normals
    assert np.all(mesh.panel_areas > 0)
    assert np.all(np.isfinite(mesh.vertices))
    assert mesh.n_panels > 0


def test_gdf_export_preserves_geometry(entry, generated, tmp_path):
    _, mesh = generated
    handler = GDFHandler()
    loaded = handler.read(handler.write(mesh, tmp_path / "roundtrip.gdf"))
    stored = handler.read(ROOT / entry["mesh_files"][0])
    assert loaded.n_panels == stored.n_panels == mesh.n_panels
    # GDFHandler.write emits six decimal places: the rounding error is at
    # most half the least significant decimal, derived from that format.
    decimal_places = 6
    rounding_error = 10 ** -decimal_places / 2
    for candidate in (loaded, stored):
        np.testing.assert_allclose(candidate.vertices[candidate.panels],
                                   mesh.vertices[mesh.panels],
                                   rtol=0, atol=rounding_error)
        np.testing.assert_allclose(candidate.bounding_box, mesh.bounding_box,
                                   rtol=0, atol=rounding_error)
        np.testing.assert_allclose(candidate.total_area, mesh.total_area,
                                   rtol=entry["validation"]["relative_tolerance"])
        report = handler.validate_mesh(candidate)
        assert report.is_valid, report.errors
        assert report.n_degenerate_panels == report.n_duplicate_vertices == 0
        assert report.has_consistent_normals
        assert not submerged_boundary_edges(candidate)
        if entry["analytic_volume_m3"] is not None:
            assert signed_mesh_volume(candidate) == pytest.approx(
                entry["analytic_volume_m3"],
                rel=entry["validation"]["relative_tolerance"],
            )


def test_geometry_qualification_and_analytic_particular(entry, generated):
    profile, mesh = generated
    validation = entry["validation"]
    assert not submerged_boundary_edges(mesh)
    if entry["analytic_volume_m3"] is None:
        assert validation["status"] == "reference-only"
        assert "analytic volume" in validation["reason"]
        return
    assert validation["status"] == "qualified-geometry"
    if entry["hull_type"] == "cylinder":
        radius = profile.beam / 2
        expected = np.pi * radius ** 2 * profile.draft
        for station in profile.stations:
            for _, y in station.waterline_offsets:
                assert y ** 2 + (station.x_position - radius) ** 2 == pytest.approx(
                    radius ** 2
                )
    elif entry["hull_type"] == "barge":
        expected = profile.length_bp * profile.beam * profile.draft
    else:
        pytest.fail(f"No analytic volume is defined for {entry['hull_type']}")
    assert expected == pytest.approx(entry["analytic_volume_m3"])
    assert signed_mesh_volume(mesh) == pytest.approx(
        expected, rel=validation["relative_tolerance"]
    )
    assert HullHydrostatics(profile).compute_displaced_volume() == pytest.approx(
        expected, rel=validation["relative_tolerance"]
    )


def test_generic_shapes_are_scaled_repository_fixtures(catalog, box_profile,
                                                      ship_profile):
    for name, fixture in (("generic_barge", box_profile),
                          ("generic_tanker", ship_profile)):
        profile = catalog.get_hull(name).profile
        assert "generic/synthetic" in profile.source
        scale = (profile.length_bp / fixture.length_bp,
                 profile.draft / fixture.draft, profile.beam / fixture.beam)
        for target, original in zip(profile.stations, fixture.stations):
            assert target.x_position == original.x_position * scale[0]
            assert target.waterline_offsets == [
                (z * scale[1], y * scale[2])
                for z, y in original.waterline_offsets
            ]
        assert len(profile.stations) == len(fixture.stations)


@pytest.mark.parametrize("symmetry", [False, True])
def test_box_end_caps_close_submerged_surface(box_profile, symmetry):
    mesh = HullMeshGenerator().generate(
        box_profile, MeshGeneratorConfig(symmetry=symmetry, close_ends=True)
    )
    edges = Counter()
    faces = []
    for panel in mesh.panels:
        indices = list(dict.fromkeys(int(i) for i in panel if i >= 0))
        faces.append(tuple(sorted(indices)))
        for a, b in zip(indices, indices[1:] + indices[:1]):
            edges[tuple(sorted((a, b)))] += 1
    assert len(faces) == len(set(faces))
    assert np.all(mesh.panel_areas > 0)
    for edge, count in edges.items():
        points = mesh.vertices[list(edge)]
        boundary = np.all(points[:, 2] == 0) or (
            symmetry and np.all(points[:, 1] == 0)
        )
        assert count == (1 if boundary else 2)
    multiplier = 2 if symmetry else 1
    expected = box_profile.length_bp * box_profile.beam * box_profile.draft
    assert multiplier * signed_mesh_volume(mesh) == pytest.approx(
        expected, rel=BOX_VOLUME_RELATIVE_TOLERANCE
    )
    for x, direction in ((0, -1), (box_profile.length_bp, 1)):
        caps = np.all(mesh.vertices[mesh.panels, 0] == x, axis=1)
        assert np.any(caps)
        np.testing.assert_allclose(mesh.normals[caps, 0], direction)
        np.testing.assert_allclose(mesh.normals[caps, 1:], 0)


@pytest.mark.parametrize("symmetry, expected_digest", [
    (False, "cbe165628b592b696d7087ad1b107fa8f290de22ce9c5b15bfffecab573252d9"),
    (True, "94be75a1caa3d3f3b6492b2e0ea428ee403f6f23136110d43f37ae90e21a1d2a"),
])
def test_zero_area_end_sections_preserve_pre_fix_panels(catalog, symmetry,
                                                        expected_digest):
    # Vertex and panel bytes derived at the reviewed PR head, before end caps.
    profile = catalog.get_hull("rebuilt_wamit_test01_cylinder").profile
    mesh = HullMeshGenerator().generate(
        profile, MeshGeneratorConfig(symmetry=symmetry, close_ends=True)
    )
    digest = hashlib.sha256(mesh.vertices.tobytes() + mesh.panels.tobytes()).hexdigest()
    assert digest == expected_digest


# Principal dimensions (m), as quoted by the second-provider source review.
SOURCE_DIMENSIONS = [
    ("rebuilt_wamit_test04_barge", 80.0, 20.0, 10.0),  # WAMIT v7.5 Appendix A.4
    ("rebuilt_wamit_test16_barge", 80.0, 10.0, 5.0),  # WAMIT v7.5 Appendix A.16
    ("rebuilt_wamit_test01_cylinder", 2 * 1.0, 2 * 1.0, 0.5),  # Appendix A.1: R=1 m
    ("generic_barge", 100.0, 30.0, 6.0),  # Issue 1700: generic barge
    ("generic_tanker", 250.0, 45.0, 16.0),  # Issue 1700: generic tanker
]


@pytest.mark.parametrize("hull_id, length, beam, draft", SOURCE_DIMENSIONS)
def test_source_dimensions_are_pinned(registry, catalog, hull_id, length, beam, draft):
    entry = next(h for h in registry["hulls"] if h["hull_id"] == hull_id)
    profile = catalog.get_hull(hull_id).profile
    for key, expected in (("length_bp", length), ("beam", beam), ("draft", draft)):
        assert entry["principal_dimensions"][key] == expected
        assert getattr(profile, key) == expected


def test_rebuilt_manual_terms_and_fixture_name(registry):
    for entry in registry["hulls"]:
        if entry["classification"] == "published benchmark":
            assert entry["provenance"]["reuse_terms"].startswith(
                "Copyright WAMIT, Inc. 1999-2023. All Rights Reserved."
            )
        coefficient = entry["principal_dimensions"]["block_coefficient"]
        if coefficient is not None:
            assert isinstance(coefficient, float)
            raw_profile = yaml.safe_load((ROOT / entry["profile_path"]).read_text())
            assert isinstance(raw_profile["block_coefficient"], float)
    box = next(h for h in registry["hulls"] if h["hull_id"] == "unit_box")
    assert "not a unit-dimension box" in box["provenance"]["derivation"]


def test_end_closure_defaults_off():
    assert MeshGeneratorConfig().close_ends is False


@pytest.mark.parametrize("symmetry", [False, True])
def test_default_box_mesh_keeps_open_ends_and_panel_count(box_profile, symmetry):
    generator = HullMeshGenerator()
    config = MeshGeneratorConfig(symmetry=symmetry)
    mesh = generator.generate(box_profile, config)
    n_x, n_z = generator._compute_grid_resolution(box_profile, config)
    multiplier = 1 if symmetry else 2
    assert mesh.n_panels == multiplier * n_x * (n_z + 1)
    unmatched = submerged_boundary_edges(mesh)
    for x in (0, box_profile.length_bp):
        assert any(np.all(mesh.vertices[list(edge), 0] == x)
                   for edge in unmatched)


def test_box_volume_tolerances_are_measured(registry):
    for hull in registry["hulls"]:
        if hull["hull_type"] == "barge":
            validation = hull["validation"]
            assert validation["relative_tolerance"] <= 1e-6
            assert validation["tolerance_source"] == (
                "Measured in test_public_hull_registry.py from the generated "
                "closed mesh and welded GDF readback against L*B*T."
            )


def test_tanker_measured_mesh_volume(registry, catalog):
    tanker = next(h for h in registry["hulls"] if h["hull_id"] == "generic_tanker")
    mesh = HullMeshGenerator().generate(
        catalog.get_hull("generic_tanker").profile,
        MeshGeneratorConfig(symmetry=False, close_ends=True),
    )
    # Repeated generated measurements agree; allow the numerical tolerance
    # measured for the same signed-volume integration on the exact boxes.
    assert signed_mesh_volume(mesh) == pytest.approx(
        tanker["mesh_volume_m3"], rel=BOX_VOLUME_RELATIVE_TOLERANCE
    )
    assert tanker["analytic_volume_m3"] is None
    assert tanker["validation"]["status"] == "reference-only"
    assert all("mesh_volume_m3" not in h for h in registry["hulls"]
               if h["hull_id"] != "generic_tanker")
