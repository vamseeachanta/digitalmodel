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
        assert config.mesh_below_waterline_only is True

    def test_custom_config(self):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            MeshGeneratorConfig,
        )

        config = MeshGeneratorConfig(target_panels=500, symmetry=False)
        assert config.target_panels == 500
        assert config.symmetry is False


class TestHullMeshGeneratorBoxHull:
    """Test mesh generation with a simple rectangular box hull."""

    def _make_box_profile(self):
        """Create a 100m x 20m x 10m box (half-breadth = 10m)."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        # Box: constant cross-section from AP to FP
        # waterline_offsets: (z, y) where z is draft from keel, y is half-breadth
        # For a box: at any z from 0 to 10m, half-breadth is constant 10m
        stations = [
            HullStation(
                x_position=0.0,
                waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
            ),
            HullStation(
                x_position=50.0,
                waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
            ),
            HullStation(
                x_position=100.0,
                waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
            ),
        ]
        return HullProfile(
            name="unit_box",
            hull_type=HullType.BARGE,
            stations=stations,
            length_bp=100.0,
            beam=20.0,
            draft=10.0,
            depth=12.0,
            source="test",
        )

    def test_generates_panel_mesh(self):
        """Generator produces a PanelMesh instance."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_box_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100, symmetry=True)
        mesh = gen.generate(profile, config)
        # Check it's a PanelMesh (duck-type check)
        assert hasattr(mesh, "vertices")
        assert hasattr(mesh, "panels")
        assert mesh.n_panels > 0
        assert mesh.n_vertices > 0

    def test_vertices_below_waterline(self):
        """All vertices should be at or below waterline (z <= 0 in marine convention)."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_box_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(
            target_panels=100, mesh_below_waterline_only=True
        )
        mesh = gen.generate(profile, config)
        # In marine convention: waterline at z=0, keel at z=-draft
        assert np.all(mesh.vertices[:, 2] <= 1e-10)

    def test_quad_panels(self):
        """Generated mesh should use quadrilateral panels."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_box_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100)
        mesh = gen.generate(profile, config)
        assert mesh.is_quad_mesh

    def test_normals_point_outward(self):
        """Panel normals should point outward (into fluid domain)."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_box_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100, symmetry=False)
        mesh = gen.generate(profile, config)
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

    def test_symmetry_flag_sets_symmetry_plane(self):
        """When symmetry=True, PanelMesh symmetry_plane should be 'y'."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_box_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100, symmetry=True)
        mesh = gen.generate(profile, config)
        assert mesh.symmetry_plane == "y"

    def test_symmetry_generates_starboard_only(self):
        """With symmetry=True, all y-coordinates should be >= 0 (starboard half)."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_box_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100, symmetry=True)
        mesh = gen.generate(profile, config)
        assert np.all(mesh.vertices[:, 1] >= -1e-10)

    def test_no_symmetry_generates_both_sides(self):
        """With symmetry=False, mesh has both positive and negative y vertices."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_box_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200, symmetry=False)
        mesh = gen.generate(profile, config)
        assert np.any(mesh.vertices[:, 1] > 0.1)
        assert np.any(mesh.vertices[:, 1] < -0.1)

    def test_panel_count_approximate(self):
        """Generated panel count should be within 50% of target."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_box_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200, symmetry=True)
        mesh = gen.generate(profile, config)
        assert mesh.n_panels >= 100  # at least 50% of target
        assert mesh.n_panels <= 400  # at most 200% of target

    def test_bounding_box_matches_hull(self):
        """Bounding box should approximately match hull dimensions."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_box_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200, symmetry=False)
        mesh = gen.generate(profile, config)
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

    def test_no_degenerate_panels(self):
        """No panels should have zero area."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_box_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100)
        mesh = gen.generate(profile, config)
        assert np.all(mesh.panel_areas > 1e-10)

    def test_mesh_name_from_profile(self):
        """Mesh name should be derived from hull profile name."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_box_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=100)
        mesh = gen.generate(profile, config)
        assert "unit_box" in mesh.name


class TestHullMeshGeneratorShipHull:
    """Test mesh generation with a shaped (non-rectangular) hull."""

    def _make_ship_profile(self):
        """Create a simple ship-like profile with varying cross sections."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        stations = [
            # Stern (narrow)
            HullStation(
                x_position=0.0,
                waterline_offsets=[
                    (0.0, 0.0),
                    (2.0, 3.0),
                    (5.0, 4.0),
                    (8.0, 5.0),
                ],
            ),
            # Midship (widest)
            HullStation(
                x_position=50.0,
                waterline_offsets=[
                    (0.0, 0.0),
                    (2.0, 7.0),
                    (5.0, 9.0),
                    (8.0, 10.0),
                ],
            ),
            # Bow (narrow again)
            HullStation(
                x_position=100.0,
                waterline_offsets=[
                    (0.0, 0.0),
                    (2.0, 2.0),
                    (5.0, 3.0),
                    (8.0, 4.0),
                ],
            ),
        ]
        return HullProfile(
            name="test_ship",
            hull_type=HullType.SHIP,
            stations=stations,
            length_bp=100.0,
            beam=20.0,
            draft=8.0,
            depth=12.0,
            source="test",
        )

    def test_ship_hull_generates_mesh(self):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_ship_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200)
        mesh = gen.generate(profile, config)
        assert mesh.n_panels > 0

    def test_ship_hull_has_varying_breadth(self):
        """Ship hull vertices should show varying y-offsets along x."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_ship_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200, symmetry=True)
        mesh = gen.generate(profile, config)
        # Get max y at midship region (x ~ 50) vs bow (x ~ 100)
        mid_mask = (mesh.vertices[:, 0] > 40) & (mesh.vertices[:, 0] < 60)
        bow_mask = mesh.vertices[:, 0] > 90
        if np.any(mid_mask) and np.any(bow_mask):
            mid_max_y = np.max(mesh.vertices[mid_mask, 1])
            bow_max_y = np.max(mesh.vertices[bow_mask, 1])
            assert mid_max_y > bow_max_y


class TestAdaptiveDensity:
    """Tests for curvature-adaptive panel distribution."""

    def _make_ship_profile(self):
        """Ship with narrow bow/stern and wide parallel body."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        stations = [
            HullStation(
                x_position=0.0,
                waterline_offsets=[
                    (0.0, 0.0),
                    (2.0, 3.0),
                    (5.0, 4.0),
                    (8.0, 5.0),
                ],
            ),
            HullStation(
                x_position=25.0,
                waterline_offsets=[
                    (0.0, 0.0),
                    (2.0, 7.0),
                    (5.0, 9.0),
                    (8.0, 10.0),
                ],
            ),
            HullStation(
                x_position=50.0,
                waterline_offsets=[
                    (0.0, 0.0),
                    (2.0, 7.0),
                    (5.0, 10.0),
                    (8.0, 10.0),
                ],
            ),
            HullStation(
                x_position=75.0,
                waterline_offsets=[
                    (0.0, 0.0),
                    (2.0, 7.0),
                    (5.0, 9.0),
                    (8.0, 10.0),
                ],
            ),
            HullStation(
                x_position=100.0,
                waterline_offsets=[
                    (0.0, 0.0),
                    (2.0, 2.0),
                    (5.0, 3.0),
                    (8.0, 4.0),
                ],
            ),
        ]
        return HullProfile(
            name="test_ship",
            hull_type=HullType.SHIP,
            stations=stations,
            length_bp=100.0,
            beam=20.0,
            draft=8.0,
            depth=12.0,
            source="test",
        )

    def test_adaptive_config_flag(self):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            MeshGeneratorConfig,
        )

        config = MeshGeneratorConfig(adaptive_density=True)
        assert config.adaptive_density is True

    def test_adaptive_generates_valid_mesh(self):
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_ship_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=200, adaptive_density=True)
        mesh = gen.generate(profile, config)
        assert mesh.n_panels > 0
        assert mesh.is_quad_mesh
        assert np.all(mesh.panel_areas > 1e-10)

    def test_adaptive_clusters_panels_at_ends(self):
        """With adaptive density, panels at bow/stern should be smaller than midship."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_ship_profile()
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(
            target_panels=300, adaptive_density=True, symmetry=True
        )
        mesh = gen.generate(profile, config)

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

    def test_adaptive_same_panel_count_as_uniform(self):
        """Adaptive should produce similar total panel count to uniform."""
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        profile = self._make_ship_profile()
        gen = HullMeshGenerator()
        uniform_config = MeshGeneratorConfig(
            target_panels=200, adaptive_density=False
        )
        adaptive_config = MeshGeneratorConfig(
            target_panels=200, adaptive_density=True
        )
        uniform_mesh = gen.generate(profile, uniform_config)
        adaptive_mesh = gen.generate(profile, adaptive_config)
        # Panel counts should be within 30% of each other
        ratio = adaptive_mesh.n_panels / uniform_mesh.n_panels
        assert 0.7 < ratio < 1.3

    def test_adaptive_box_similar_to_uniform(self):
        """For a box hull (zero curvature), adaptive should be similar to uniform."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        stations = [
            HullStation(
                x_position=0.0,
                waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
            ),
            HullStation(
                x_position=50.0,
                waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
            ),
            HullStation(
                x_position=100.0,
                waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
            ),
        ]
        box = HullProfile(
            name="box",
            hull_type=HullType.BARGE,
            stations=stations,
            length_bp=100.0,
            beam=20.0,
            draft=10.0,
            depth=12.0,
            source="test",
        )
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(
            target_panels=200, adaptive_density=True
        )
        mesh = gen.generate(box, config)
        # For a box, max panel area / min panel area should be modest
        # (no extreme variation since curvature is zero everywhere)
        area_ratio = np.max(mesh.panel_areas) / np.min(mesh.panel_areas)
        assert area_ratio < 5.0  # shouldn't vary wildly for a box


class TestCoarsenMesh:
    """Tests for the coarsen_mesh standalone function."""

    def _make_fine_box_mesh(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )

        stations = [
            HullStation(
                x_position=0.0,
                waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
            ),
            HullStation(
                x_position=50.0,
                waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
            ),
            HullStation(
                x_position=100.0,
                waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
            ),
        ]
        box = HullProfile(
            name="box",
            hull_type=HullType.BARGE,
            stations=stations,
            length_bp=100.0,
            beam=20.0,
            draft=10.0,
            depth=12.0,
            source="test",
        )
        gen = HullMeshGenerator()
        config = MeshGeneratorConfig(target_panels=500, symmetry=True)
        return gen.generate(box, config)

    def test_coarsen_reduces_panel_count(self):
        from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import (
            coarsen_mesh,
        )

        fine_mesh = self._make_fine_box_mesh()
        coarse_mesh = coarsen_mesh(fine_mesh, target_panels=100)
        assert coarse_mesh.n_panels < fine_mesh.n_panels
        assert coarse_mesh.n_panels > 0

    def test_coarsen_preserves_bounding_box(self):
        """Coarsened mesh should have similar bounding box."""
        from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import (
            coarsen_mesh,
        )

        fine_mesh = self._make_fine_box_mesh()
        coarse_mesh = coarsen_mesh(fine_mesh, target_panels=100)
        fine_bb_min, fine_bb_max = fine_mesh.bounding_box
        coarse_bb_min, coarse_bb_max = coarse_mesh.bounding_box
        # Bounding box should be within 10% of hull dimensions
        np.testing.assert_allclose(coarse_bb_min, fine_bb_min, atol=5.0)
        np.testing.assert_allclose(coarse_bb_max, fine_bb_max, atol=5.0)

    def test_coarsen_no_degenerate_panels(self):
        from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import (
            coarsen_mesh,
        )

        fine_mesh = self._make_fine_box_mesh()
        coarse_mesh = coarsen_mesh(fine_mesh, target_panels=100)
        assert np.all(coarse_mesh.panel_areas > 1e-10)

    def test_coarsen_preserves_quad_format(self):
        from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import (
            coarsen_mesh,
        )

        fine_mesh = self._make_fine_box_mesh()
        coarse_mesh = coarsen_mesh(fine_mesh, target_panels=100)
        assert coarse_mesh.is_quad_mesh

    def test_coarsen_preserves_features_flag(self):
        """With preserve_features=True, curved regions keep more detail."""
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )
        from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
            HullMeshGenerator,
            MeshGeneratorConfig,
        )
        from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import (
            coarsen_mesh,
        )

        # Ship hull with curves
        stations = [
            HullStation(
                x_position=0.0,
                waterline_offsets=[
                    (0.0, 0.0),
                    (2.0, 3.0),
                    (5.0, 4.0),
                    (8.0, 5.0),
                ],
            ),
            HullStation(
                x_position=50.0,
                waterline_offsets=[
                    (0.0, 0.0),
                    (2.0, 7.0),
                    (5.0, 10.0),
                    (8.0, 10.0),
                ],
            ),
            HullStation(
                x_position=100.0,
                waterline_offsets=[
                    (0.0, 0.0),
                    (2.0, 2.0),
                    (5.0, 3.0),
                    (8.0, 4.0),
                ],
            ),
        ]
        ship = HullProfile(
            name="ship",
            hull_type=HullType.SHIP,
            stations=stations,
            length_bp=100.0,
            beam=20.0,
            draft=8.0,
            depth=12.0,
            source="test",
        )
        gen = HullMeshGenerator()
        fine = gen.generate(
            ship, MeshGeneratorConfig(target_panels=500, symmetry=True)
        )
        coarse = coarsen_mesh(fine, target_panels=100, preserve_features=True)
        assert coarse.n_panels < fine.n_panels
        assert coarse.n_panels > 0
