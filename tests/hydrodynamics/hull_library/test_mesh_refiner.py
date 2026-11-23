"""Tests for hull panel mesh refiner.

Tests cover single and multi-level refinement, quality metrics computation,
mesh family generation, GDF export, and convergence summary formatting.
"""

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)
from digitalmodel.hydrodynamics.hull_library.mesh_refiner import (
    MeshFamilyMember,
    MeshQualityMetrics,
    compute_quality_metrics,
    convergence_summary,
    export_mesh_family,
    generate_mesh_family,
    refine_mesh,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def unit_box_mesh() -> PanelMesh:
    """A 1x1x1 box mesh with 5 panels (no top -- open waterplane).

    Vertices span X=[0,1], Y=[0,1], Z=[-1,0].
    """
    vertices = np.array(
        [
            [0, 0, -1],  # 0
            [1, 0, -1],  # 1
            [1, 1, -1],  # 2
            [0, 1, -1],  # 3
            [0, 0, 0],   # 4
            [1, 0, 0],   # 5
            [1, 1, 0],   # 6
            [0, 1, 0],   # 7
        ],
        dtype=np.float64,
    )
    panels = np.array(
        [
            [0, 1, 2, 3],  # bottom
            [0, 1, 5, 4],  # front (port)
            [1, 2, 6, 5],  # starboard
            [2, 3, 7, 6],  # back
            [3, 0, 4, 7],  # port
        ],
        dtype=np.int32,
    )
    return PanelMesh(
        vertices=vertices,
        panels=panels,
        name="unit_box",
        format_origin=MeshFormat.GDF,
        symmetry_plane=None,
        reference_point=[0.0, 0.0, 0.0],
        metadata={},
    )


# ---------------------------------------------------------------------------
# TestRefineOnce
# ---------------------------------------------------------------------------


class TestRefineOnce:
    """Tests for single-level mesh refinement."""

    def test_refine_quadruples_panels(self, unit_box_mesh: PanelMesh) -> None:
        """5 panels should become 20 panels after 1 level of refinement."""
        refined = refine_mesh(unit_box_mesh, levels=1)
        assert refined.n_panels == 20

    def test_refine_preserves_bounding_box(self, unit_box_mesh: PanelMesh) -> None:
        """Bounding box extents should be identical after refinement."""
        refined = refine_mesh(unit_box_mesh, levels=1)
        orig_min, orig_max = unit_box_mesh.bounding_box
        ref_min, ref_max = refined.bounding_box
        np.testing.assert_allclose(ref_min, orig_min, atol=1e-10)
        np.testing.assert_allclose(ref_max, orig_max, atol=1e-10)

    def test_refine_new_vertices_on_edges(self, unit_box_mesh: PanelMesh) -> None:
        """All new vertices should lie within the original bounding box."""
        refined = refine_mesh(unit_box_mesh, levels=1)
        orig_min, orig_max = unit_box_mesh.bounding_box
        for v in refined.vertices:
            assert np.all(v >= orig_min - 1e-10), f"Vertex {v} below bbox min {orig_min}"
            assert np.all(v <= orig_max + 1e-10), f"Vertex {v} above bbox max {orig_max}"

    def test_refine_normals_consistent(self, unit_box_mesh: PanelMesh) -> None:
        """All refined normals should be unit vectors (computed via __post_init__)."""
        refined = refine_mesh(unit_box_mesh, levels=1)
        assert refined.normals is not None
        norms = np.linalg.norm(refined.normals, axis=1)
        np.testing.assert_allclose(norms, 1.0, atol=1e-10)

    def test_refine_no_degenerate_panels(self, unit_box_mesh: PanelMesh) -> None:
        """All panel areas should be positive after refinement."""
        refined = refine_mesh(unit_box_mesh, levels=1)
        assert np.all(refined.panel_areas > 0)

    def test_refine_name_suffix(self, unit_box_mesh: PanelMesh) -> None:
        """Refined mesh name should include '_refined' suffix."""
        refined = refine_mesh(unit_box_mesh, levels=1)
        assert "_refined" in refined.name

    def test_refine_metadata_tracks_level(self, unit_box_mesh: PanelMesh) -> None:
        """Metadata should record refinement_levels count."""
        refined = refine_mesh(unit_box_mesh, levels=1)
        assert refined.metadata["refinement_levels"] == 1

    def test_refine_rejects_zero_levels(self, unit_box_mesh: PanelMesh) -> None:
        """levels < 1 should raise ValueError."""
        with pytest.raises(ValueError, match="levels"):
            refine_mesh(unit_box_mesh, levels=0)

    def test_refine_preserves_format_origin(self, unit_box_mesh: PanelMesh) -> None:
        """format_origin should be carried through to the refined mesh."""
        refined = refine_mesh(unit_box_mesh, levels=1)
        assert refined.format_origin == MeshFormat.GDF


# ---------------------------------------------------------------------------
# TestRefineMultipleLevels
# ---------------------------------------------------------------------------


class TestRefineMultipleLevels:
    """Tests for multi-level mesh refinement."""

    def test_two_levels_gives_16x_panels(self, unit_box_mesh: PanelMesh) -> None:
        """5 panels -> ~80 panels after 2 levels (4^2 = 16x)."""
        refined = refine_mesh(unit_box_mesh, levels=2)
        # Exact count is 80 unless degenerate removal changes it
        assert refined.n_panels == 80

    def test_vertex_count_increases(self, unit_box_mesh: PanelMesh) -> None:
        """Each refinement level should add more vertices."""
        r1 = refine_mesh(unit_box_mesh, levels=1)
        r2 = refine_mesh(unit_box_mesh, levels=2)
        assert r1.n_vertices > unit_box_mesh.n_vertices
        assert r2.n_vertices > r1.n_vertices

    def test_three_levels(self, unit_box_mesh: PanelMesh) -> None:
        """5 panels -> ~320 panels after 3 levels (4^3 = 64x)."""
        refined = refine_mesh(unit_box_mesh, levels=3)
        # Expect 5 * 64 = 320 (minus any degenerate removal)
        assert refined.n_panels == 320

    def test_total_area_preserved(self, unit_box_mesh: PanelMesh) -> None:
        """Total surface area should be preserved through refinement."""
        original_area = float(np.sum(unit_box_mesh.panel_areas))
        for levels in (1, 2, 3):
            refined = refine_mesh(unit_box_mesh, levels=levels)
            refined_area = float(np.sum(refined.panel_areas))
            assert refined_area == pytest.approx(original_area, rel=1e-8), (
                f"Area mismatch at level {levels}: {refined_area} vs {original_area}"
            )

    def test_metadata_accumulates_levels(self, unit_box_mesh: PanelMesh) -> None:
        """Each level should increment refinement_levels in metadata."""
        refined = refine_mesh(unit_box_mesh, levels=3)
        assert refined.metadata["refinement_levels"] == 3


# ---------------------------------------------------------------------------
# TestComputeQualityMetrics
# ---------------------------------------------------------------------------


class TestComputeQualityMetrics:
    """Tests for compute_quality_metrics."""

    def test_unit_box_metrics(self, unit_box_mesh: PanelMesh) -> None:
        """Unit box should have known area=1.0 per face (5 faces total area 5)."""
        metrics = compute_quality_metrics(unit_box_mesh)
        assert metrics.panel_count == 5
        assert metrics.vertex_count == 8
        assert metrics.total_area == pytest.approx(5.0, rel=1e-6)
        assert metrics.min_area == pytest.approx(1.0, rel=1e-6)
        assert metrics.max_area == pytest.approx(1.0, rel=1e-6)
        assert metrics.mean_area == pytest.approx(1.0, rel=1e-6)

    def test_metrics_all_positive(self, unit_box_mesh: PanelMesh) -> None:
        """All area and ratio values should be positive for a good mesh."""
        metrics = compute_quality_metrics(unit_box_mesh)
        assert metrics.min_area > 0
        assert metrics.max_area > 0
        assert metrics.mean_area > 0
        assert metrics.total_area > 0
        assert metrics.min_aspect_ratio > 0
        assert metrics.max_aspect_ratio > 0
        assert metrics.mean_aspect_ratio > 0

    def test_degenerate_count_zero_for_good_mesh(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """Unit box should have zero degenerate panels."""
        metrics = compute_quality_metrics(unit_box_mesh)
        assert metrics.degenerate_count == 0

    def test_unit_box_aspect_ratio(self, unit_box_mesh: PanelMesh) -> None:
        """All panels on the unit box are squares: aspect ratio should be ~1.0."""
        metrics = compute_quality_metrics(unit_box_mesh)
        assert metrics.min_aspect_ratio == pytest.approx(1.0, rel=1e-6)
        assert metrics.max_aspect_ratio == pytest.approx(1.0, rel=1e-6)

    def test_returns_mesh_quality_metrics_type(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """Return type should be MeshQualityMetrics."""
        metrics = compute_quality_metrics(unit_box_mesh)
        assert isinstance(metrics, MeshQualityMetrics)


# ---------------------------------------------------------------------------
# TestGenerateMeshFamily
# ---------------------------------------------------------------------------


class TestGenerateMeshFamily:
    """Tests for generate_mesh_family."""

    def test_default_factors_produce_5_members(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """Default factors [0.25, 0.5, 1.0, 2.0, 4.0] should produce 5 members."""
        family = generate_mesh_family(unit_box_mesh)
        assert len(family) == 5

    def test_sorted_by_panel_count(self, unit_box_mesh: PanelMesh) -> None:
        """Family members should be sorted in ascending panel count order."""
        family = generate_mesh_family(unit_box_mesh)
        panel_counts = [m.metrics.panel_count for m in family]
        assert panel_counts == sorted(panel_counts)

    def test_original_in_family(self, unit_box_mesh: PanelMesh) -> None:
        """A member with factor 1.0 (medium) should be present."""
        family = generate_mesh_family(unit_box_mesh)
        medium = [m for m in family if abs(m.factor - 1.0) < 1e-6]
        assert len(medium) == 1
        assert medium[0].level_name == "medium"
        assert medium[0].metrics.panel_count == unit_box_mesh.n_panels

    def test_coarsened_has_fewer_panels(self, unit_box_mesh: PanelMesh) -> None:
        """factor 0.5 member should have fewer panels than original (or equal if min)."""
        family = generate_mesh_family(unit_box_mesh)
        medium = [m for m in family if abs(m.factor - 1.0) < 1e-6][0]
        coarse = [m for m in family if abs(m.factor - 0.5) < 1e-6][0]
        assert coarse.metrics.panel_count <= medium.metrics.panel_count

    def test_refined_has_more_panels(self, unit_box_mesh: PanelMesh) -> None:
        """factor 2.0 member should have more panels than original."""
        family = generate_mesh_family(unit_box_mesh)
        medium = [m for m in family if abs(m.factor - 1.0) < 1e-6][0]
        fine = [m for m in family if abs(m.factor - 2.0) < 1e-6][0]
        assert fine.metrics.panel_count > medium.metrics.panel_count

    def test_custom_factors(self, unit_box_mesh: PanelMesh) -> None:
        """Custom factors [0.5, 1.0, 2.0] should produce 3 members."""
        family = generate_mesh_family(unit_box_mesh, factors=[0.5, 1.0, 2.0])
        assert len(family) == 3

    def test_all_members_have_metrics(self, unit_box_mesh: PanelMesh) -> None:
        """Every family member should have a MeshQualityMetrics instance."""
        family = generate_mesh_family(unit_box_mesh)
        for member in family:
            assert isinstance(member.metrics, MeshQualityMetrics)
            assert member.metrics.panel_count > 0

    def test_family_members_have_level_names(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """Each member should have a meaningful level_name string."""
        family = generate_mesh_family(unit_box_mesh)
        names = {m.level_name for m in family}
        assert "medium" in names
        assert "fine" in names
        assert "very_fine" in names


# ---------------------------------------------------------------------------
# TestExportMeshFamily
# ---------------------------------------------------------------------------


class TestExportMeshFamily:
    """Tests for export_mesh_family."""

    def test_exports_all_files(
        self, unit_box_mesh: PanelMesh, tmp_path: Path
    ) -> None:
        """One GDF file per family member should be created."""
        family = generate_mesh_family(unit_box_mesh, factors=[0.5, 1.0, 2.0])
        paths = export_mesh_family(family, tmp_path)
        assert len(paths) == 3
        for p in paths:
            assert p.exists()
            assert p.suffix == ".gdf"
            assert p.stat().st_size > 0

    def test_gdf_files_readable(
        self, unit_box_mesh: PanelMesh, tmp_path: Path
    ) -> None:
        """GDFHandler should be able to read each exported file."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import GDFHandler

        family = generate_mesh_family(unit_box_mesh, factors=[1.0, 2.0])
        paths = export_mesh_family(family, tmp_path)
        handler = GDFHandler()
        for p in paths:
            read_back = handler.read(p)
            assert read_back.n_panels > 0
            assert read_back.n_vertices > 0


# ---------------------------------------------------------------------------
# TestConvergenceSummary
# ---------------------------------------------------------------------------


class TestConvergenceSummary:
    """Tests for convergence_summary."""

    def test_produces_markdown_table(self, unit_box_mesh: PanelMesh) -> None:
        """Output should contain markdown header separator '|----'."""
        family = generate_mesh_family(unit_box_mesh, factors=[1.0, 2.0])
        summary = convergence_summary(family)
        assert "|----" in summary
        assert "| Level" in summary
        assert "| Factor" in summary

    def test_all_members_in_summary(self, unit_box_mesh: PanelMesh) -> None:
        """There should be one data row per family member."""
        family = generate_mesh_family(unit_box_mesh, factors=[0.5, 1.0, 2.0])
        summary = convergence_summary(family)
        # Header + separator + data rows = 2 + len(family)
        lines = summary.strip().split("\n")
        assert len(lines) == 2 + len(family)

    def test_level_names_in_summary(self, unit_box_mesh: PanelMesh) -> None:
        """Each level_name should appear in the summary output."""
        family = generate_mesh_family(unit_box_mesh, factors=[1.0, 2.0])
        summary = convergence_summary(family)
        for member in family:
            assert member.level_name in summary
