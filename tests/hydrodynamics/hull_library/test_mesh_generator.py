"""Tests for hull mesh generator -- converting hull profiles to panel meshes."""

import pytest
import numpy as np
from numpy.testing import assert_array_less


class TestMeshGeneratorConfig:
    """Tests for MeshGeneratorConfig."""

    def test_default_config(self):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            MeshGeneratorConfig,
        )

        config = MeshGeneratorConfig()
        assert config.target_panels == 1000
        assert config.waterline_refinement == 2.0
        assert config.symmetry is True

    def test_custom_config(self):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            MeshGeneratorConfig,
        )

        config = MeshGeneratorConfig(target_panels=500, symmetry=False)
        assert config.target_panels == 500
        assert config.symmetry is False


class TestHullMeshGeneratorBoxHull:
    """Test mesh generation with a simple rectangular box hull."""

    def test_generates_panel_mesh(self, box_profile):
        """Generator produces a PanelMesh instance."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100, symmetry=True)
        mesh = gen.generate(box_profile, config)
        # Check it's a PanelMesh (duck-type check)
        assert hasattr(mesh, "vertices")
        assert hasattr(mesh, "panels")
        assert mesh.n_panels > 0
        assert mesh.n_vertices > 0

    def test_vertices_below_waterline(self, box_profile):
        """All vertices should be at or below waterline (z <= 0 in marine convention)."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100)
        mesh = gen.generate(box_profile, config)
        # In marine convention: waterline at z=0, keel at z=-draft
        assert np.all(mesh.vertices[:, 2] <= 1e-10)

    def test_quad_panels(self, box_profile):
        """Generated mesh should use quadrilateral panels."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100)
        mesh = gen.generate(box_profile, config)
        assert mesh.is_quad_mesh

    def test_normals_point_outward(self, box_profile):
        """Panel normals should point outward (into fluid domain)."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100, symmetry=False)
        mesh = gen.generate(box_profile, config)
        # For a box-like hull, the centroid of the mesh should be "inside"
        centroid = np.mean(mesh.vertices, axis=0)
        for i in range(mesh.n_panels):
            center = mesh.panel_centers[i]
            normal = mesh.normals[i]
            to_center = center - centroid
            # Normal should point away from centroid (dot product > 0)
            assert np.dot(normal, to_center) >= -1e-6, (
                f"Panel {i} normal points inward"
            )

    def test_symmetry_flag_sets_symmetry_plane(self, box_profile):
        """When symmetry=True, PanelMesh symmetry_plane should be 'y'."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100, symmetry=True)
        mesh = gen.generate(box_profile, config)
        assert mesh.symmetry_plane == "y"

    def test_symmetry_generates_starboard_only(self, box_profile):
        """With symmetry=True, all y-coordinates should be >= 0 (starboard half)."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100, symmetry=True)
        mesh = gen.generate(box_profile, config)
        assert np.all(mesh.vertices[:, 1] >= -1e-10)

    def test_no_symmetry_generates_both_sides(self, box_profile):
        """With symmetry=False, mesh has both positive and negative y vertices."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200, symmetry=False)
        mesh = gen.generate(box_profile, config)
        assert np.any(mesh.vertices[:, 1] > 0.1)
        assert np.any(mesh.vertices[:, 1] < -0.1)

    def test_panel_count_approximate(self, box_profile):
        """Generated panel count should be within 50% of target."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200, symmetry=True)
        mesh = gen.generate(box_profile, config)
        assert mesh.n_panels >= 100  # at least 50% of target
        assert mesh.n_panels <= 400  # at most 200% of target

    def test_bounding_box_matches_hull(self, box_profile):
        """Bounding box should approximately match hull dimensions."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200, symmetry=False)
        mesh = gen.generate(box_profile, config)
        bb_min, bb_max = mesh.bounding_box
        # x range should be ~0 to 100
        assert bb_min[0] >= -1.0
        assert bb_max[0] <= 101.0
        # y range should be ~-10 to 10 (half-breadth)
        assert bb_min[1] >= -11.0
        assert bb_max[1] <= 11.0
        # z range should be ~-10 (keel) to 0 (waterline)
        assert bb_min[2] >= -11.0
        assert bb_max[2] <= 1.0

    def test_no_degenerate_panels(self, box_profile):
        """No panels should have zero area."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100)
        mesh = gen.generate(box_profile, config)
        assert np.all(mesh.panel_areas > 1e-10)

    def test_mesh_name_from_profile(self, box_profile):
        """Mesh name should be derived from hull profile name."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100)
        mesh = gen.generate(box_profile, config)
        assert "unit_box" in mesh.name


class TestHullMeshGeneratorShipHull:
    """Test mesh generation with a shaped (non-rectangular) hull."""

    def test_ship_hull_generates_mesh(self, ship_profile):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200)
        mesh = gen.generate(ship_profile, config)
        assert mesh.n_panels > 0

    def test_ship_hull_has_varying_breadth(self, ship_profile):
        """Ship hull vertices should show varying y-offsets along x."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200, symmetry=True)
        mesh = gen.generate(ship_profile, config)
        # Get max y at midship region (x ~ 50) vs bow (x ~ 100)
        mid_mask = (mesh.vertices[:, 0] > 40) & (mesh.vertices[:, 0] < 60)
        bow_mask = mesh.vertices[:, 0] > 90
        if np.any(mid_mask) and np.any(bow_mask):
            mid_max_y = np.max(mesh.vertices[mid_mask, 1])
            bow_max_y = np.max(mesh.vertices[bow_mask, 1])
            assert mid_max_y > bow_max_y


class TestAdaptiveDensity:
    """Tests for curvature-adaptive panel distribution."""

    def test_adaptive_config_flag(self):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            MeshGeneratorConfig,
        )

        config = MeshGeneratorConfig(adaptive_density=True)
        assert config.adaptive_density is True

    def test_adaptive_generates_valid_mesh(self, ship_profile):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200, adaptive_density=True)
        mesh = gen.generate(ship_profile, config)
        assert mesh.n_panels > 0
        assert mesh.is_quad_mesh
        assert np.all(mesh.panel_areas > 1e-10)

    def test_adaptive_clusters_panels_at_ends(self, ship_profile):
        """With adaptive density, panels at bow/stern should be smaller than midship."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(
            target_panels=300, adaptive_density=True, symmetry=True
        )
        mesh = gen.generate(ship_profile, config)

        # Compare average panel area at midship (x: 40-60) vs bow (x: 85-100)
        mid_mask = (mesh.panel_centers[:, 0] > 40) & (
            mesh.panel_centers[:, 0] < 60
        )
        bow_mask = mesh.panel_centers[:, 0] > 85
        if np.sum(mid_mask) > 0 and np.sum(bow_mask) > 0:
            mid_avg_area = np.mean(mesh.panel_areas[mid_mask])
            bow_avg_area = np.mean(mesh.panel_areas[bow_mask])
            # Midship panels should be larger (coarser) than bow panels
            assert mid_avg_area > bow_avg_area

    def test_adaptive_same_panel_count_as_uniform(self, ship_profile):
        """Adaptive should produce similar total panel count to uniform."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        uniform_config = MeshGeneratorConfig(
            target_panels=200, adaptive_density=False
        )
        adaptive_config = MeshGeneratorConfig(
            target_panels=200, adaptive_density=True
        )
        uniform_mesh = gen.generate(ship_profile, uniform_config)
        adaptive_mesh = gen.generate(ship_profile, adaptive_config)
        # Panel counts should be within 30% of each other
        ratio = adaptive_mesh.n_panels / uniform_mesh.n_panels
        assert 0.7 < ratio < 1.3

    def test_adaptive_box_similar_to_uniform(self, box_profile):
        """For a box hull (zero curvature), adaptive should be similar to uniform."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(
            target_panels=200, adaptive_density=True
        )
        mesh = gen.generate(box_profile, config)
        # For a box, max panel area / min panel area should be modest
        # (no extreme variation since curvature is zero everywhere)
        area_ratio = np.max(mesh.panel_areas) / np.min(mesh.panel_areas)
        assert area_ratio < 5.0  # shouldn't vary wildly for a box


class TestCoarsenMesh:
    """Tests for the coarsen_mesh standalone function."""

    @staticmethod
    def _make_fine_box_mesh(box_profile):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=500, symmetry=True)
        return gen.generate(box_profile, config)

    def test_coarsen_reduces_panel_count(self, box_profile):
        from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import (
            coarsen_mesh,
        )

        fine_mesh = self._make_fine_box_mesh(box_profile)
        coarse_mesh = coarsen_mesh(fine_mesh, target_panels=100)
        assert coarse_mesh.n_panels < fine_mesh.n_panels
        assert coarse_mesh.n_panels > 0

    def test_coarsen_preserves_bounding_box(self, box_profile):
        """Coarsened mesh should have similar bounding box."""
        from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import (
            coarsen_mesh,
        )

        fine_mesh = self._make_fine_box_mesh(box_profile)
        coarse_mesh = coarsen_mesh(fine_mesh, target_panels=100)
        fine_bb_min, fine_bb_max = fine_mesh.bounding_box
        coarse_bb_min, coarse_bb_max = coarse_mesh.bounding_box
        # Bounding box should be within 10% of hull dimensions
        np.testing.assert_allclose(coarse_bb_min, fine_bb_min, atol=5.0)
        np.testing.assert_allclose(coarse_bb_max, fine_bb_max, atol=5.0)

    def test_coarsen_no_degenerate_panels(self, box_profile):
        from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import (
            coarsen_mesh,
        )

        fine_mesh = self._make_fine_box_mesh(box_profile)
        coarse_mesh = coarsen_mesh(fine_mesh, target_panels=100)
        assert np.all(coarse_mesh.panel_areas > 1e-10)

    def test_coarsen_preserves_quad_format(self, box_profile):
        from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import (
            coarsen_mesh,
        )

        fine_mesh = self._make_fine_box_mesh(box_profile)
        coarse_mesh = coarsen_mesh(fine_mesh, target_panels=100)
        assert coarse_mesh.is_quad_mesh

    def test_coarsen_preserves_features_flag(self, ship_profile):
        """With preserve_features=True, curved regions keep more detail."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )
        from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import (
            coarsen_mesh,
        )

        gen = HullMeshGenerator()
        fine = gen.generate(
            ship_profile, MeshGeneratorConfig(target_panels=500, symmetry=True)
        )
        coarse = coarsen_mesh(fine, target_panels=100, preserve_features=True)
        assert coarse.n_panels < fine.n_panels
        assert coarse.n_panels > 0


def _assert_no_fold_edges(mesh):
    """Check directed incidence independently of the orientation implementation."""
    edges = {}
    for panel in mesh.panels:
        vertices = list(dict.fromkeys(int(i) for i in panel if i >= 0))
        for start, end in zip(vertices, vertices[1:] + vertices[:1]):
            edges.setdefault(tuple(sorted((start, end))), []).append((start, end))
    shared = [incidence for incidence in edges.values() if len(incidence) > 1]
    assert shared, "fixture must exercise shared edges"
    for incidence in shared:
        assert len(incidence) == 2, "non-manifold shared edge"
        assert incidence[0] == incidence[1][::-1], "fold edge"


class TestWindingConsistency:
    @pytest.mark.parametrize("profile_name", ["ship_profile", "box_profile"])
    @pytest.mark.parametrize("symmetry", [True, False])
    @pytest.mark.parametrize("adaptive", [False, True])
    def test_generated_shared_edges(self, request, profile_name, symmetry, adaptive):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        mesh = HullMeshGenerator().generate(
            request.getfixturevalue(profile_name),
            MeshGeneratorConfig(
                target_panels=200, symmetry=symmetry, adaptive_density=adaptive
            ),
        )
        _assert_no_fold_edges(mesh)
        assert mesh.metadata["winding"]["components"] == 1
        assert mesh.metadata["winding"]["method"] == "adjacency_bfs"
        if not symmetry:
            outward = mesh.panel_centers - mesh.vertices.mean(axis=0)
            dots = np.einsum("ij,ij->i", mesh.normals, outward)
            assert np.mean(dots > 0) >= 0.99

    def test_wigley_preserves_natural_winding(self, monkeypatch):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )
        from digitalmodel.hydrodynamics.hull_library.parametric_form import (
            MonohullFormParameters,
            generate_profile,
        )

        original = HullMeshGenerator._orient_normals_outward
        entry_panels = []

        def capture(generator, mesh):
            entry_panels.append(mesh.panels.copy())
            original(generator, mesh)

        monkeypatch.setattr(HullMeshGenerator, "_orient_normals_outward", capture)
        params = MonohullFormParameters(
            length_bp=100,
            beam=20,
            draft=8,
            depth=14,
            wigley=True,
            cb=4 / 9,
            bilge_radius_fraction=0,
        )
        mesh = HullMeshGenerator().generate(
            generate_profile(params), MeshGeneratorConfig(target_panels=8000)
        )
        assert mesh.n_panels == 7225
        _assert_no_fold_edges(mesh)
        flipped = np.count_nonzero(np.any(mesh.panels != entry_panels[0], axis=1))
        assert flipped in (0, mesh.n_panels)
        assert mesh.metadata["winding"] == {
            "components": 1,
            "flipped_panels": flipped,
            "method": "adjacency_bfs",
        }

    def test_repairs_scrambled_quads_and_is_idempotent(self, ship_profile):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        generator = HullMeshGenerator()
        mesh = generator.generate(
            ship_profile, MeshGeneratorConfig(target_panels=200, symmetry=False)
        )
        expected = mesh.panels.copy()
        original_areas = mesh.panel_areas.copy()
        scrambled = np.arange(0, mesh.n_panels, 3)
        mesh.panels[scrambled] = mesh.panels[scrambled][:, [0, 3, 2, 1]]
        mesh._compute_normals()
        generator._orient_normals_outward(mesh)
        _assert_no_fold_edges(mesh)
        np.testing.assert_array_equal(mesh.panels, expected)
        assert mesh.metadata["winding"]["flipped_panels"] == len(scrambled)
        np.testing.assert_allclose(mesh.panel_areas, original_areas)
        points = mesh.vertices[mesh.panels]
        normals = np.cross(points[:, 1] - points[:, 0], points[:, 2] - points[:, 0])
        normals /= np.linalg.norm(normals, axis=1)[:, None]
        np.testing.assert_allclose(mesh.normals, normals, atol=1e-12)
        generator._orient_normals_outward(mesh)
        np.testing.assert_array_equal(mesh.panels, expected)
        assert mesh.metadata["winding"]["flipped_panels"] == 0

    @pytest.mark.parametrize("encoding", ["triangle", "padded", "collapsed"])
    def test_disconnected_components_and_triangle_encodings(self, encoding):
        from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
        )

        tetra = np.array([[0, 0, 0], [1, 0, 0], [0, 1, 0], [0, 0, 1]], float)
        faces = np.array([[0, 2, 1], [0, 1, 3], [0, 3, 2], [1, 2, 3]], np.int32)
        panels = np.vstack((faces, (faces + 4)[:, [0, 2, 1]]))
        if encoding == "padded":
            panels = np.column_stack((panels, np.full(8, -1)))
        elif encoding == "collapsed":
            panels = np.array(
                [np.insert(face, i % 3, face[i % 3]) for i, face in enumerate(panels)]
            )
        mesh = PanelMesh(
            vertices=np.vstack((tetra, tetra + [10, 0, 0], [[1000, 0, 0]])),
            panels=panels,
            metadata={"retained": True},
        )
        HullMeshGenerator()._orient_normals_outward(mesh)
        _assert_no_fold_edges(mesh)
        assert mesh.metadata["retained"] is True
        assert mesh.metadata["winding"] == {
            "components": 2,
            "flipped_panels": 4,
            "method": "adjacency_bfs",
        }
        if encoding == "padded":
            np.testing.assert_array_equal(mesh.panels[:, -1], -1)
        for i, panel in enumerate(mesh.panels):
            indices = list(dict.fromkeys(int(j) for j in panel if j >= 0))
            points = mesh.vertices[indices]
            normal = np.cross(points[1] - points[0], points[2] - points[0])
            normal /= np.linalg.norm(normal)
            np.testing.assert_allclose(mesh.normals[i], normal, atol=1e-12)
            center = tetra.mean(axis=0) + ([0, 0, 0] if i < 4 else [10, 0, 0])
            assert np.dot(normal, points.mean(axis=0) - center) > 0

    def test_empty_mesh_diagnostics(self):
        from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
        )

        mesh = PanelMesh(np.empty((0, 3)), np.empty((0, 4), dtype=np.int32))
        HullMeshGenerator()._orient_normals_outward(mesh)
        assert mesh.metadata["winding"] == {
            "components": 0,
            "flipped_panels": 0,
            "method": "adjacency_bfs",
        }
        assert mesh.panels.shape == (0, 4)

    @pytest.mark.parametrize(
        "panels,message",
        [
            ([[0, 1, 2], [1, 0, 3], [0, 1, 4]], "non-manifold"),
            ([[0, 1, 3, 2], [2, 3, 5, 4], [4, 5, 0, 1]], "not orientable"),
        ],
    )
    def test_rejects_inconsistent_topology_without_mutation(self, panels, message):
        from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
        )

        mesh = PanelMesh(
            np.array(
                [[0, 0, 0], [0, 1, 0], [1, 0, 0], [1, 1, 0], [2, 0, 1], [2, 1, 1]],
                float,
            ),
            np.array(panels, np.int32),
        )
        before = mesh.panels.copy()
        with pytest.raises(ValueError, match=message):
            HullMeshGenerator()._orient_normals_outward(mesh)
        np.testing.assert_array_equal(mesh.panels, before)
