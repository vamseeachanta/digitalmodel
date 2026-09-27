"""Tests for HullProd curvature screening (#2170 D1-D4) and PCHIP station lofting (D3).

Analytical comparators (closed-form class): unit sphere I_D = K * L_ref^2 = 4 with
L_ref = 2; open cylinder is exactly developable (a_single = 1, I_D = 0); Wigley hull
class fractions 0.63 / 0.37 at 160x40 (HullProd reference control). HullProd-dependent
tests skip when the optional extra is not installed.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)
from digitalmodel.hydrodynamics.hull_library.curvature_screen import (
    CREASE_DOMINATED_HULL_TYPES,
    CurvatureSignature,
    hullprod_available,
    panel_mesh_to_trimesh,
    saddle_warning_threshold,
    screen_panel_mesh,
    screen_profile,
    screen_trimesh,
)
from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    HullMeshGenerator,
    MeshGeneratorConfig,
    _shape_preserving_interp,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import HullType

needs_hullprod = pytest.mark.skipif(
    not hullprod_available(), reason="optional hullprod extra not installed"
)


# ---------------------------------------------------------------------------
# D3: shape-preserving station lofting (no hullprod needed)
# ---------------------------------------------------------------------------


class TestShapePreservingInterp:
    def test_exact_at_nodes(self):
        x = np.array([0.0, 25.0, 50.0, 75.0, 100.0])
        y = np.array([3.0, 7.0, 10.0, 9.0, 2.0])
        assert np.allclose(_shape_preserving_interp(x, y, x, (0.0, 0.0)), y)

    def test_no_overshoot_between_monotone_nodes(self):
        x = np.array([0.0, 2.0, 5.0, 8.0])
        y = np.array([0.0, 7.0, 10.0, 10.0])
        xs = np.linspace(0.0, 8.0, 401)
        ys = _shape_preserving_interp(x, y, xs, (0.0, 10.0))
        assert ys.min() >= -1e-12 and ys.max() <= 10.0 + 1e-12
        assert np.all(np.diff(ys) >= -1e-12)

    def test_two_points_is_linear(self):
        ys = _shape_preserving_interp(
            np.array([0.0, 10.0]),
            np.array([0.0, 5.0]),
            np.array([2.5, 7.5]),
            (0.0, 5.0),
        )
        assert np.allclose(ys, [1.25, 3.75])

    def test_duplicate_nodes_and_fill(self):
        x = np.array([0.0, 0.0, 5.0, 10.0])
        y = np.array([1.0, 1.0, 4.0, 2.0])
        ys = _shape_preserving_interp(x, y, np.array([-1.0, 5.0, 11.0]), (-9.0, 9.0))
        assert ys[0] == -9.0 and ys[2] == 9.0 and np.isclose(ys[1], 4.0)

    def test_generated_ship_bow_waterline_is_curved_not_ruled(self, ship_profile):
        """The bow waterline between stations 0 and 25 m must leave the chord (no ruled loft)
        while staying inside the station envelope (no overshoot)."""
        mesh = HullMeshGenerator().generate(
            ship_profile, MeshGeneratorConfig(target_panels=2000)
        )
        # Vertices on the design waterline (z ~ 0), starboard half, bow segment
        wl = mesh.vertices[np.abs(mesh.vertices[:, 2]) < 1e-9]
        wl = wl[np.argsort(wl[:, 0])]
        seg = wl[(wl[:, 0] > 0.5) & (wl[:, 0] < 24.5)]
        x, y = seg[:, 0], seg[:, 1]
        # Station offsets at the waterline (z_keel = 8): 5.0 at x=0, 10.0 at x=25
        chord = 5.0 + (10.0 - 5.0) * x / 25.0
        assert np.max(np.abs(y - chord)) > 0.1
        assert np.all(y >= 5.0 - 1e-9) and np.all(y <= 10.0 + 1e-9)
        # Along-x second derivative is bounded (smooth), not concentrated at the station
        d2 = np.gradient(np.gradient(y, x), x)
        assert np.all(np.isfinite(d2))


# ---------------------------------------------------------------------------
# D4/D6: thresholds and hull-type policy (no hullprod needed)
# ---------------------------------------------------------------------------


class TestPolicy:
    def test_monohull_threshold_and_crease_types(self):
        assert saddle_warning_threshold(HullType.SHIP) == pytest.approx(0.35)
        assert saddle_warning_threshold("fpso") == pytest.approx(0.35)
        for t in CREASE_DOMINATED_HULL_TYPES:
            assert saddle_warning_threshold(t) is None
        assert saddle_warning_threshold(None) is None
        assert saddle_warning_threshold("not-a-type") is None

    def test_saddle_warning_message(self):
        kwargs = {
            "I_D": 1.0,
            "I_D_plus": 0.5,
            "I_D_minus": 0.5,
            "a_flat": 0.1,
            "a_single": 0.1,
            "a_elliptic": 0.2,
            "lref": 100.0,
            "lref_mode": "explicit_user",
            "reliability": "caution",
            "status": "mesh_representation_sensitive",
            "valid_area_fraction": 0.95,
            "panel_count": 10,
            "vertex_count": 8,
            "hullprod_version": "1.0.1",
        }
        assert CurvatureSignature(
            a_saddle=0.6, hull_type="ship", **kwargs
        ).saddle_warning()
        assert (
            CurvatureSignature(
                a_saddle=0.2, hull_type="ship", **kwargs
            ).saddle_warning()
            is None
        )
        assert (
            CurvatureSignature(
                a_saddle=0.9, hull_type="spar", **kwargs
            ).saddle_warning()
            is None
        )

    def test_signature_vector_order(self):
        sig = CurvatureSignature(
            I_D=1,
            I_D_plus=2,
            I_D_minus=3,
            a_flat=4,
            a_single=5,
            a_elliptic=6,
            a_saddle=7,
            lref=1,
            lref_mode="x",
            reliability="good",
            status="valid",
            valid_area_fraction=1,
            panel_count=1,
            vertex_count=1,
            hullprod_version="1",
        )
        assert sig.as_vector() == [1, 2, 3, 4, 5, 6, 7]


class TestPanelMeshConversion:
    def test_quads_split_and_symmetry_expanded(self):
        verts = np.array(
            [[0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0], [2, 0, 0], [2, 1, 0]],
            dtype=float,
        )
        panels = np.array([[0, 1, 2, 3], [1, 4, 5, 2]], dtype=np.int32)
        mesh = PanelMesh(
            vertices=verts,
            panels=panels,
            format_origin=MeshFormat.UNKNOWN,
            symmetry_plane="y",
        )
        pytest.importorskip("trimesh")
        tri = panel_mesh_to_trimesh(mesh)
        assert len(tri.faces) == 8  # 2 quads -> 4 tris, mirrored -> 8
        assert tri.vertices[:, 1].min() < 0 < tri.vertices[:, 1].max()
        tri_half = panel_mesh_to_trimesh(mesh, expand_symmetry=False)
        assert len(tri_half.faces) == 4


# ---------------------------------------------------------------------------
# D1: analytical controls through the adapter (hullprod required)
# ---------------------------------------------------------------------------


def _cylinder(radius=6.0, height=26.0, nc=48, nz=26):
    trimesh = pytest.importorskip("trimesh")
    th = np.linspace(0, 2 * np.pi, nc, endpoint=False)
    z = np.linspace(-height / 2, height / 2, nz + 1)
    verts = np.array(
        [[radius * np.cos(t), radius * np.sin(t), zz] for zz in z for t in th]
    )
    faces = []
    for j in range(nz):
        for i in range(nc):
            a, b = j * nc + i, j * nc + (i + 1) % nc
            c, d = (j + 1) * nc + i, (j + 1) * nc + (i + 1) % nc
            faces.extend([[a, b, d], [a, d, c]])
    return trimesh.Trimesh(verts, np.asarray(faces), process=False)


def _wigley(length=100.0, breadth=10.0, draft=6.25, nx=160, nz=40):
    trimesh = pytest.importorskip("trimesh")
    x = np.linspace(-0.5 * length, 0.5 * length, nx + 1)
    z = np.linspace(-draft, 0.0, nz + 1)
    xx, zz = np.meshgrid(x, z, indexing="ij")
    yy = 0.5 * breadth * (1.0 - (2.0 * xx / length) ** 2) * (1.0 - (zz / draft) ** 2)
    verts = np.column_stack((xx.ravel(), yy.ravel(), zz.ravel()))
    row = nz + 1
    faces = []
    for i in range(nx):
        for j in range(nz):
            a, b, c, d = (
                i * row + j,
                i * row + j + 1,
                (i + 1) * row + j,
                (i + 1) * row + j + 1,
            )
            faces.extend(
                ([a, c, b], [b, c, d]) if (i + j) % 2 else ([a, c, d], [a, d, b])
            )
    return trimesh.Trimesh(verts, np.asarray(faces), process=False)


@needs_hullprod
class TestAnalyticalControls:
    def test_unit_sphere_I_D_is_four(self):
        trimesh = pytest.importorskip("trimesh")
        sphere = trimesh.creation.icosphere(subdivisions=3, radius=1.0)
        sig = screen_trimesh(sphere, lref=2.0, keep_fields=False).signature
        assert sig.I_D == pytest.approx(4.0, rel=1e-3)
        assert sig.I_D_minus == pytest.approx(0.0, abs=1e-6)
        assert sig.a_elliptic == pytest.approx(1.0)
        assert sig.reliability == "good"

    def test_cylinder_is_developable(self):
        sig = screen_trimesh(_cylinder(), lref=12.0, keep_fields=False).signature
        assert sig.I_D == pytest.approx(0.0, abs=1e-9)
        assert sig.a_single == pytest.approx(1.0)

    def test_wigley_class_fractions(self):
        sig = screen_trimesh(_wigley(), lref=100.0).signature
        assert sig.a_elliptic == pytest.approx(0.636, abs=0.02)
        assert sig.a_saddle == pytest.approx(0.364, abs=0.02)
        assert sig.I_D == pytest.approx(4.16, abs=0.1)

    def test_fields_map_to_vertices(self):
        result = screen_trimesh(_wigley(nx=40, nz=10), lref=100.0, keep_fields=True)
        assert result.fields is not None
        n = len(result.fields.vertices)
        assert result.fields.K.shape == (n,) and result.fields.H.shape == (n,)
        assert result.fields.valid.dtype == bool
        assert "citation" in result.provenance


# ---------------------------------------------------------------------------
# D2/D3/D6: generated hulls and catalog integration (hullprod required)
# ---------------------------------------------------------------------------


@needs_hullprod
class TestGeneratedHulls:
    def test_ship_profile_has_elliptic_ends_after_pchip_lofting(self, ship_profile):
        """Linear lofting gave K <= 0 (60 % saddle, no elliptic bow); PCHIP restores it."""
        result = screen_profile(ship_profile, MeshGeneratorConfig(target_panels=2000))
        sig = result.signature
        assert sig.lref == pytest.approx(100.0)
        assert sig.lref_mode == "explicit_user"
        assert sig.hull_type == "ship" and not sig.crease_dominated
        assert sig.a_elliptic > sig.a_saddle
        assert sig.a_elliptic > 0.5
        assert sig.I_D_plus > sig.I_D_minus

    def test_pure_scaling_keeps_signature(self, ship_profile):
        """Dimensionless signature is invariant to uniform L/B/T scaling (D2 rationale)."""
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            _scale_profile,
        )

        cfg = MeshGeneratorConfig(target_panels=1500)
        base = screen_profile(ship_profile, cfg).signature
        scaled_profile = _scale_profile(
            ship_profile,
            {"length_scale": 1.5, "beam_scale": 1.5, "draft_scale": 1.5},
            "scaled",
        )
        scaled = screen_profile(scaled_profile, cfg).signature
        assert scaled.lref == pytest.approx(150.0)
        assert np.allclose(scaled.as_vector(), base.as_vector(), rtol=0.02, atol=0.01)

    def test_catalog_screen_hull_stores_signature(self, ship_profile):
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog

        catalog = HullCatalog()
        catalog.register_hull(ship_profile)
        assert catalog.get_hull(ship_profile.name).curvature_signature is None
        sig = catalog.screen_hull(
            ship_profile.name, MeshGeneratorConfig(target_panels=800)
        )
        assert catalog.get_hull(ship_profile.name).curvature_signature is sig
        assert sig.hullprod_version

    def test_parametric_space_generate_signatures(self, ship_profile):
        from digitalmodel.hydrodynamics.hull_library.catalog import HullCatalog
        from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
            HullParametricSpace,
            ParametricRange,
        )

        catalog = HullCatalog()
        catalog.register_hull(ship_profile)
        space = HullParametricSpace(
            base_hull_id=ship_profile.name,
            ranges={"length_scale": ParametricRange(min=1.0, max=1.2, steps=2)},
        )
        rows = list(
            space.generate_signatures(catalog, MeshGeneratorConfig(target_panels=600))
        )
        assert len(rows) == 2
        assert rows[0][2].lref == pytest.approx(100.0)
        assert rows[1][2].lref == pytest.approx(120.0)

    def test_crease_dominated_type_is_annotated(self, box_profile):
        box_profile.hull_type = HullType.SEMI_PONTOON
        mesh = HullMeshGenerator().generate(
            box_profile, MeshGeneratorConfig(target_panels=400)
        )
        sig = screen_panel_mesh(
            mesh, lref=box_profile.length_bp, hull_type=box_profile.hull_type
        ).signature
        assert sig.crease_dominated
        assert any("crease" in n for n in sig.notes)
        assert sig.saddle_warning() is None

    def test_panel_catalog_round_trips_signature(self, ship_profile, tmp_path):
        from digitalmodel.hydrodynamics.hull_library.panel_catalog import (
            PanelCatalog,
            PanelCatalogEntry,
            PanelFormat,
        )

        sig = screen_profile(
            ship_profile, MeshGeneratorConfig(target_panels=400)
        ).signature
        catalog = PanelCatalog(
            entries=[
                PanelCatalogEntry(
                    hull_id="ship",
                    hull_type=HullType.SHIP,
                    name="ship",
                    source="test",
                    panel_format=PanelFormat.GDF,
                    file_path="ship.gdf",
                    curvature_signature=sig,
                )
            ]
        )
        path = catalog.to_yaml(tmp_path / "catalog.yaml")
        loaded = PanelCatalog.from_yaml(path)
        assert loaded.entries[0].curvature_signature is not None
        assert loaded.entries[0].curvature_signature.as_vector() == pytest.approx(
            sig.as_vector()
        )


# ---------------------------------------------------------------------------
# D4: diffraction quality gate integration (hullprod required)
# ---------------------------------------------------------------------------


def _double_cone(sectors: int = 52):
    """Closed double cone: two apex vertices of valence ``sectors`` (> 50 is POOR)."""
    trimesh = pytest.importorskip("trimesh")
    th = np.linspace(0, 2 * np.pi, sectors, endpoint=False)
    rim = np.column_stack([np.cos(th), np.sin(th), np.zeros(sectors)])
    verts = np.vstack([rim, [[0, 0, 1.0]], [[0, 0, -1.0]]])
    top, bot = sectors, sectors + 1
    faces = []
    for i in range(sectors):
        j = (i + 1) % sectors
        faces.append([i, j, top])
        faces.append([j, i, bot])
    return trimesh.Trimesh(verts, np.asarray(faces), process=False)


@needs_hullprod
class TestQualityGate:
    SAMPLE_GDF = (
        Path(__file__).parents[1] / "bemrosetta" / "fixtures" / "sample_box.gdf"
    )

    def test_box_gdf_carries_signature_and_does_not_block(self):
        from digitalmodel.hydrodynamics.diffraction.quality_gates import (
            run_mesh_quality_gate,
        )

        result = run_mesh_quality_gate(
            self.SAMPLE_GDF, "box", hull_type="barge", lref=10.0
        )
        assert result.status != "FAIL"
        assert result.curvature is not None
        assert result.curvature["reliability"] in {"good", "caution"}
        assert "I_D" in result.curvature and result.curvature["lref"] == 10.0
        assert "curvature" in result.to_dict()

    def test_poor_reliability_blocks_only_with_curvature_gate(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.quality_gates import (
            run_mesh_quality_gate,
        )

        path = tmp_path / "double_cone.stl"
        _double_cone().export(path)

        gated = run_mesh_quality_gate(path, "cone")
        assert gated.status == "FAIL"
        assert any("curvature reliability POOR" in b for b in gated.blocking)
        assert gated.curvature["reliability"] == "poor"

        ungated = run_mesh_quality_gate(path, "cone", curvature=False)
        assert ungated.curvature is None
        assert not any("POOR" in b for b in ungated.blocking)


# Issue 2253: diagonal choice must not manufacture curvature on analytic vertices.
def _wigley_quads(nx=425, nz=17):
    reference = _wigley(length=100, breadth=20, draft=8, nx=nx, nz=nz)
    row = nz + 1
    panels = [
        [i * row + j, (i + 1) * row + j, (i + 1) * row + j + 1, i * row + j + 1]
        for i in range(nx)
        for j in range(nz)
    ]
    return (
        PanelMesh(
            vertices=reference.vertices.copy(),
            panels=np.asarray(panels, dtype=np.int32),
        ),
        reference,
    )


@pytest.mark.parametrize("split", ["shortest", "alternate"])
@needs_hullprod
def test_analytic_quad_wigley_matches_triangle_and_brep(split):
    mesh, reference = _wigley_quads()
    actual = screen_panel_mesh(mesh, lref=100, quad_split=split, keep_fields=False)
    triangle = screen_trimesh(reference, lref=100, keep_fields=False)
    assert actual.signature.I_D == pytest.approx(triangle.signature.I_D, rel=0.10)
    assert actual.signature.I_D == pytest.approx(7.1197, rel=0.15)
    assert actual.signature.quad_split == split
    assert actual.provenance["quad_split"] == split
    assert triangle.signature.quad_split is None
    assert triangle.provenance["quad_split"] is None


@needs_hullprod
@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason="Plan fixed>25 premise: consistent analytic quads measure 6.617684; generated winding inflates curvature",
)
def test_fixed_quad_wigley_documents_inflation():
    mesh, _ = _wigley_quads()
    result = screen_panel_mesh(mesh, lref=100, quad_split="fixed", keep_fields=False)
    assert result.signature.I_D > 25
    assert result.signature.quad_split == "fixed"


@pytest.mark.parametrize("split", ["shortest", "alternate"])
def test_even_width_structured_grid_uses_checkerboard_ties(split):
    pytest.importorskip("trimesh")
    vertices = np.array([[i, j, 0] for i in range(3) for j in range(3)], float)
    panels = np.array(
        [[0, 3, 4, 1], [1, 4, 5, 2], [3, 6, 7, 4], [4, 7, 8, 5]], np.int32
    )
    tri = panel_mesh_to_trimesh(PanelMesh(vertices, panels), quad_split=split)
    edges = [set(map(tuple, tri.vertices[face])) for face in tri.faces]
    diagonals = [
        set(map(tuple, vertices[pair])) for pair in ([0, 4], [4, 2], [6, 4], [4, 8])
    ]
    assert all(sum(diagonal <= edge for edge in edges) == 2 for diagonal in diagonals)
    assert np.all(tri.face_normals[:, 2] > 0)


def test_unstructured_ties_alternate_by_panel_index():
    pytest.importorskip("trimesh")
    vertices = np.array(
        [
            [x, y, 0]
            for offset in (0, 3)
            for x, y in ((offset, 0), (offset + 1, 0), (offset + 1, 1), (offset, 1))
        ],
        float,
    )
    mesh = PanelMesh(vertices, np.arange(8, dtype=np.int32).reshape(2, 4))
    tri = panel_mesh_to_trimesh(mesh)
    triangles = [set(map(tuple, tri.vertices[face])) for face in tri.faces]
    for pair in ([0, 2], [5, 7]):
        diagonal = set(map(tuple, vertices[pair]))
        assert sum(diagonal <= face for face in triangles) == 2


@pytest.mark.parametrize("scale", [1e-6, 1, 1e6])
def test_shortest_uses_three_dimensional_diagonal(scale):
    pytest.importorskip("trimesh")
    vertices = scale * np.array([[0, 0, 0], [2, 0, 0], [1, 1, 3], [0, 1, 0]], float)
    mesh = PanelMesh(vertices, np.array([[0, 1, 2, 3]], np.int32))
    tri = panel_mesh_to_trimesh(mesh)
    diagonal = set(map(tuple, vertices[[1, 3]]))
    assert all(diagonal <= set(map(tuple, tri.vertices[f])) for f in tri.faces)


@pytest.mark.parametrize("panels", [[[0, 1, 2]], [[0, 1, 2, -1]], [[0, 1, 2, 2]]])
@needs_hullprod
def test_triangle_encodings_have_no_quad_split(panels):
    sphere = pytest.importorskip("trimesh").creation.icosphere(subdivisions=2)
    if len(panels[0]) == 3:
        faces = sphere.faces
    elif panels[0][-1] == -1:
        faces = np.column_stack((sphere.faces, np.full(len(sphere.faces), -1)))
    else:
        faces = np.column_stack((sphere.faces, sphere.faces[:, -1]))
    result = screen_panel_mesh(PanelMesh(sphere.vertices, faces), lref=2)
    assert result.signature.quad_split is None
    assert result.provenance["quad_split"] is None
    assert result.signature.I_D == pytest.approx(4, rel=0.01)


def test_invalid_quad_split_rejected():
    mesh, _ = _wigley_quads(nx=2, nz=2)
    with pytest.raises(ValueError, match="quad_split"):
        panel_mesh_to_trimesh(mesh, quad_split="invalid")


@needs_hullprod
def test_synthetic_semisub_cylinder_component_stays_developable():
    trimesh = pytest.importorskip("trimesh")
    cylinder = _cylinder()
    # Convert the cylinder's triangle pairs back to their original quad panels.
    quads = np.column_stack((cylinder.faces[::2], cylinder.faces[1::2, 2]))
    column = panel_mesh_to_trimesh(PanelMesh(cylinder.vertices, quads))
    box = trimesh.creation.box(extents=[20, 5, 4])
    box.apply_translation([30, 0, -15])
    fixture = trimesh.util.concatenate([column, box])
    result = screen_trimesh(fixture, lref=12, hull_type=HullType.SEMI_PONTOON)
    assert result.signature.crease_dominated
    component = fixture.submesh(
        [np.arange(len(column.faces))], append=True, repair=False
    )
    signature = screen_trimesh(component, lref=12).signature
    assert signature.a_single == pytest.approx(1)
    assert signature.I_D == pytest.approx(0, abs=1e-9)


@pytest.mark.parametrize("representation", ["mesh", "both"])
def test_profile_forwards_quad_split(monkeypatch, ship_profile, representation):
    from types import SimpleNamespace

    from digitalmodel.hydrodynamics.hull_library import curvature_screen as module

    sig = SimpleNamespace(model_dump=dict)
    result = SimpleNamespace(signature=sig, provenance={})
    seen = []
    monkeypatch.setattr(module, "_screen_profile_brep", lambda *a, **kw: result)
    monkeypatch.setattr(module, "representation_delta", lambda *a: {})

    def capture(mesh, **kwargs):
        seen.append(kwargs["quad_split"])
        return result

    monkeypatch.setattr(module, "screen_panel_mesh", capture)
    screen_profile(
        ship_profile,
        MeshGeneratorConfig(target_panels=100),
        representation=representation,
        quad_split="alternate",
    )
    assert seen == ["alternate"]


@needs_hullprod
def test_fixed_inflation_with_generated_winding_on_exact_vertices():
    mesh, _ = _wigley_quads()
    mesh.panels = mesh.panels[:, [0, 3, 2, 1]]
    mesh._compute_normals()
    HullMeshGenerator()._orient_normals_outward(mesh)
    # The existing centroid heuristic flips only part of this open half hull.
    assert np.any(mesh.normals[:, 1] < 0) and np.any(mesh.normals[:, 1] > 0)
    result = screen_panel_mesh(mesh, lref=100, quad_split="fixed")
    assert result.signature.I_D > 25


def test_fixed_reproduces_legacy_triangles():
    trimesh = pytest.importorskip("trimesh")
    mesh, _ = _wigley_quads(nx=4, nz=4)
    faces = [face for a, b, c, d in mesh.panels for face in ([a, b, c], [a, c, d])]
    legacy = trimesh.Trimesh(mesh.vertices, faces, process=True)
    legacy.merge_vertices()
    legacy.update_faces(legacy.nondegenerate_faces())
    actual = panel_mesh_to_trimesh(mesh, quad_split="fixed")
    assert np.array_equal(actual.vertices, legacy.vertices)
    assert np.array_equal(actual.faces, legacy.faces)


@pytest.mark.parametrize(
    "rotation,reverse", [(0, True), (1, False), (1, True), (2, False)]
)
def test_structured_ties_preserve_physical_diagonal_under_winding(rotation, reverse):
    pytest.importorskip("trimesh")
    vertices = np.array([[i, j, 0] for i in range(3) for j in range(3)], float)
    panels = np.array(
        [[0, 3, 4, 1], [1, 4, 5, 2], [3, 6, 7, 4], [4, 7, 8, 5]], np.int32
    )
    before = panel_mesh_to_trimesh(PanelMesh(vertices, panels))
    panels = np.roll(panels, rotation, axis=1)
    if reverse:
        panels = panels[:, ::-1]
    after = panel_mesh_to_trimesh(PanelMesh(vertices, panels))
    assert {tuple(sorted(f)) for f in before.faces} == {
        tuple(sorted(f)) for f in after.faces
    }
    assert np.all(after.face_normals[:, 2] == (-1 if reverse else 1))


def test_mixed_panels_and_sparse_index_fallback():
    pytest.importorskip("trimesh")
    vertices = np.zeros((10002, 3))
    vertices[[0, 1, 10000, 10001]] = [[0, 0, 0], [1, 0, 0], [0, 1, 0], [1, 1, 0]]
    panels = np.array(
        [[0, 1, 10001, 10000], [0, 1, 10001, -1], [0, 0, 10001, 10000]], np.int32
    )
    tri = panel_mesh_to_trimesh(PanelMesh(vertices, panels))
    assert len(tri.faces) == 4
    assert tri.metadata["quad_split"] == "shortest"


def test_spike_shortest_diagonal_and_ties_match_adapter():
    import runpy

    pytest.importorskip("trimesh")
    script = (
        Path(__file__).parents[3]
        / "docs/spikes/2026-09-25-hullprod-curvature-screening/gdf_to_stl.py"
    )
    converter = runpy.run_path(str(script))["quads_to_mesh"]
    quads = np.array(
        [
            [[0, 0, 0], [2, 0, 0], [1, 1, 3], [0, 1, 0]],
            [[4, 0, 0], [5, 0, 0], [5, 1, 0], [4, 1, 0]],
            [[7, 0, 0], [8, 0, 0], [8, 1, 0], [8, 1, 0]],
        ],
        float,
    )
    actual = converter(quads)
    expected = panel_mesh_to_trimesh(
        PanelMesh(quads.reshape(-1, 3), np.arange(12, dtype=np.int32).reshape(-1, 4))
    )
    assert np.array_equal(actual.vertices, expected.vertices)
    assert np.array_equal(actual.faces, expected.faces)


@pytest.mark.parametrize("isx,isy", [(0, 1), (1, 0), (1, 1)])
def test_spike_mirrors_selected_diagonal(isx, isy):
    import runpy

    pytest.importorskip("trimesh")
    script = (
        Path(__file__).parents[3]
        / "docs/spikes/2026-09-25-hullprod-curvature-screening/gdf_to_stl.py"
    )
    converter = runpy.run_path(str(script))
    quads = np.array(
        [[[x, 2, 0], [x + 1, 2, 1], [x + 1, 3, 0], [x, 3, 1]] for x in (2, 5)], float
    )
    actual = converter["mirror_mesh"](converter["quads_to_mesh"](quads), isx, isy)
    mesh = PanelMesh(
        quads.reshape(-1, 3),
        np.arange(8, dtype=np.int32).reshape(2, 4),
        symmetry_plane=("x" if isx else "") + ("y" if isy else ""),
    )
    expected = panel_mesh_to_trimesh(mesh)

    def triangles(tri):
        return {tuple(sorted(map(tuple, tri.vertices[f]))) for f in tri.faces}

    assert triangles(actual) == triangles(expected)
