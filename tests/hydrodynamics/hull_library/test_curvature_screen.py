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
        kwargs = dict(
            I_D=1.0,
            I_D_plus=0.5,
            I_D_minus=0.5,
            a_flat=0.1,
            a_single=0.1,
            a_elliptic=0.2,
            lref=100.0,
            lref_mode="explicit_user",
            reliability="caution",
            status="mesh_representation_sensitive",
            valid_area_fraction=0.95,
            panel_count=10,
            vertex_count=8,
            hullprod_version="1.0.1",
        )
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
