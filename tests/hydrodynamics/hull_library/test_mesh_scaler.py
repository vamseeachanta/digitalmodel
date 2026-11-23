"""Tests for hull panel mesh scaler.

Tests cover uniform scaling, parametric scaling, target-dimension scaling,
validation, and GDF round-trip export.
"""

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)
from digitalmodel.hydrodynamics.hull_library.mesh_scaler import (
    ScaleDimensions,
    ScaleResult,
    export_scaled_gdf,
    scale_mesh_parametric,
    scale_mesh_to_target,
    scale_mesh_uniform,
    validate_scaled_mesh,
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
            [0, 0, -1],  # 0 - bottom AP port
            [1, 0, -1],  # 1 - bottom FP port
            [1, 1, -1],  # 2 - bottom FP stbd
            [0, 1, -1],  # 3 - bottom AP stbd
            [0, 0, 0],   # 4 - WL AP port
            [1, 0, 0],   # 5 - WL FP port
            [1, 1, 0],   # 6 - WL FP stbd
            [0, 1, 0],   # 7 - WL AP stbd
        ],
        dtype=np.float64,
    )
    panels = np.array(
        [
            [0, 1, 2, 3],  # bottom
            [0, 1, 5, 4],  # front (port side)
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


@pytest.fixture
def barge_mesh() -> PanelMesh:
    """A 100x20x8 barge mesh (open waterplane box)."""
    vertices = np.array(
        [
            [0, 0, -8],
            [100, 0, -8],
            [100, 20, -8],
            [0, 20, -8],
            [0, 0, 0],
            [100, 0, 0],
            [100, 20, 0],
            [0, 20, 0],
        ],
        dtype=np.float64,
    )
    panels = np.array(
        [
            [0, 1, 2, 3],  # bottom
            [0, 1, 5, 4],  # front
            [1, 2, 6, 5],  # starboard
            [2, 3, 7, 6],  # back
            [3, 0, 4, 7],  # port
        ],
        dtype=np.int32,
    )
    return PanelMesh(
        vertices=vertices,
        panels=panels,
        name="barge_100x20x8",
        format_origin=MeshFormat.GDF,
        symmetry_plane=None,
        reference_point=[0.0, 0.0, 0.0],
        metadata={},
    )


# ---------------------------------------------------------------------------
# TestScaleMeshUniform
# ---------------------------------------------------------------------------


class TestScaleMeshUniform:
    """Tests for scale_mesh_uniform."""

    def test_uniform_scale_doubles_dimensions(self, unit_box_mesh: PanelMesh) -> None:
        """Scaling by 2.0 should double all bounding box extents."""
        scaled = scale_mesh_uniform(unit_box_mesh, 2.0)
        bb_min, bb_max = scaled.bounding_box
        assert bb_max[0] - bb_min[0] == pytest.approx(2.0)
        assert bb_max[1] - bb_min[1] == pytest.approx(2.0)
        assert bb_max[2] - bb_min[2] == pytest.approx(2.0)

    def test_uniform_scale_preserves_topology(self, unit_box_mesh: PanelMesh) -> None:
        """Panel count and connectivity must not change."""
        scaled = scale_mesh_uniform(unit_box_mesh, 3.0)
        assert scaled.n_panels == unit_box_mesh.n_panels
        assert scaled.n_vertices == unit_box_mesh.n_vertices
        np.testing.assert_array_equal(scaled.panels, unit_box_mesh.panels)

    def test_uniform_scale_preserves_aspect_ratios(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """All edges scale equally, so aspect ratios should be identical."""
        from digitalmodel.hydrodynamics.hull_library.mesh_scaler import (
            _compute_aspect_ratios,
        )

        original_ratios = _compute_aspect_ratios(unit_box_mesh)
        scaled = scale_mesh_uniform(unit_box_mesh, 5.0)
        scaled_ratios = _compute_aspect_ratios(scaled)
        np.testing.assert_allclose(scaled_ratios, original_ratios, atol=1e-10)

    def test_uniform_identity_scale(self, unit_box_mesh: PanelMesh) -> None:
        """Factor 1.0 should produce an identical mesh."""
        scaled = scale_mesh_uniform(unit_box_mesh, 1.0)
        np.testing.assert_allclose(scaled.vertices, unit_box_mesh.vertices, atol=1e-12)

    def test_uniform_fractional_scale(self, unit_box_mesh: PanelMesh) -> None:
        """Factor 0.5 should halve all dimensions."""
        scaled = scale_mesh_uniform(unit_box_mesh, 0.5)
        bb_min, bb_max = scaled.bounding_box
        assert bb_max[0] - bb_min[0] == pytest.approx(0.5)
        assert bb_max[1] - bb_min[1] == pytest.approx(0.5)
        assert bb_max[2] - bb_min[2] == pytest.approx(0.5)

    def test_uniform_scale_rejects_zero(self, unit_box_mesh: PanelMesh) -> None:
        """Factor of zero should raise ValueError."""
        with pytest.raises(ValueError, match="positive"):
            scale_mesh_uniform(unit_box_mesh, 0.0)

    def test_uniform_scale_rejects_negative(self, unit_box_mesh: PanelMesh) -> None:
        """Negative factor should raise ValueError."""
        with pytest.raises(ValueError, match="positive"):
            scale_mesh_uniform(unit_box_mesh, -1.0)

    def test_uniform_scale_updates_reference_point(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """Reference point should also be scaled."""
        # Set a non-zero reference point
        unit_box_mesh.reference_point = [10.0, 5.0, -2.0]
        scaled = scale_mesh_uniform(unit_box_mesh, 3.0)
        assert scaled.reference_point == pytest.approx([30.0, 15.0, -6.0])

    def test_uniform_scale_sets_metadata(self, unit_box_mesh: PanelMesh) -> None:
        """Metadata should record the scale factor and type."""
        scaled = scale_mesh_uniform(unit_box_mesh, 2.5)
        assert scaled.metadata["scale_factor"] == pytest.approx(2.5)
        assert scaled.metadata["scale_type"] == "uniform"


# ---------------------------------------------------------------------------
# TestScaleMeshParametric
# ---------------------------------------------------------------------------


class TestScaleMeshParametric:
    """Tests for scale_mesh_parametric."""

    def test_parametric_stretches_length_only(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """sx=2, sy=1, sz=1 should double X extent only."""
        scaled = scale_mesh_parametric(unit_box_mesh, 2.0, 1.0, 1.0)
        bb_min, bb_max = scaled.bounding_box
        assert bb_max[0] - bb_min[0] == pytest.approx(2.0)
        assert bb_max[1] - bb_min[1] == pytest.approx(1.0)
        assert bb_max[2] - bb_min[2] == pytest.approx(1.0)

    def test_parametric_stretches_beam_only(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """sx=1, sy=2, sz=1 should double Y extent only."""
        scaled = scale_mesh_parametric(unit_box_mesh, 1.0, 2.0, 1.0)
        bb_min, bb_max = scaled.bounding_box
        assert bb_max[0] - bb_min[0] == pytest.approx(1.0)
        assert bb_max[1] - bb_min[1] == pytest.approx(2.0)
        assert bb_max[2] - bb_min[2] == pytest.approx(1.0)

    def test_parametric_stretches_draft_only(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """sx=1, sy=1, sz=2 should double Z extent only."""
        scaled = scale_mesh_parametric(unit_box_mesh, 1.0, 1.0, 2.0)
        bb_min, bb_max = scaled.bounding_box
        assert bb_max[0] - bb_min[0] == pytest.approx(1.0)
        assert bb_max[1] - bb_min[1] == pytest.approx(1.0)
        assert bb_max[2] - bb_min[2] == pytest.approx(2.0)

    def test_parametric_independent_factors(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """sx=1.5, sy=0.8, sz=1.2 should scale each axis independently."""
        scaled = scale_mesh_parametric(unit_box_mesh, 1.5, 0.8, 1.2)
        bb_min, bb_max = scaled.bounding_box
        assert bb_max[0] - bb_min[0] == pytest.approx(1.5)
        assert bb_max[1] - bb_min[1] == pytest.approx(0.8)
        assert bb_max[2] - bb_min[2] == pytest.approx(1.2)

    def test_parametric_preserves_panel_count(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """Topology (panel and vertex count) must be unchanged."""
        scaled = scale_mesh_parametric(unit_box_mesh, 2.0, 3.0, 0.5)
        assert scaled.n_panels == unit_box_mesh.n_panels
        assert scaled.n_vertices == unit_box_mesh.n_vertices

    def test_parametric_normals_recomputed(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """After parametric scaling, normals should be recomputed (not None)."""
        scaled = scale_mesh_parametric(unit_box_mesh, 2.0, 1.0, 1.0)
        assert scaled.normals is not None
        assert scaled.normals.shape == (scaled.n_panels, 3)
        # All normal vectors should be unit length
        norms = np.linalg.norm(scaled.normals, axis=1)
        np.testing.assert_allclose(norms, 1.0, atol=1e-10)

    def test_parametric_rejects_zero_factor(
        self, unit_box_mesh: PanelMesh
    ) -> None:
        """Zero factor for any axis should raise ValueError."""
        with pytest.raises(ValueError, match="positive"):
            scale_mesh_parametric(unit_box_mesh, 0.0, 1.0, 1.0)

    def test_parametric_sets_metadata(self, unit_box_mesh: PanelMesh) -> None:
        """Metadata should record factors and type."""
        scaled = scale_mesh_parametric(unit_box_mesh, 1.5, 2.0, 0.8)
        assert scaled.metadata["scale_factors"] == pytest.approx((1.5, 2.0, 0.8))
        assert scaled.metadata["scale_type"] == "parametric"


# ---------------------------------------------------------------------------
# TestScaleMeshToTarget
# ---------------------------------------------------------------------------


class TestScaleMeshToTarget:
    """Tests for scale_mesh_to_target."""

    def test_scale_barge_to_larger(self, barge_mesh: PanelMesh) -> None:
        """Scale a 100x20x8 barge to 150x30x12."""
        source = ScaleDimensions(length_m=100.0, beam_m=20.0, draft_m=8.0)
        target = ScaleDimensions(length_m=150.0, beam_m=30.0, draft_m=12.0)
        result = scale_mesh_to_target(barge_mesh, source, target)

        assert isinstance(result, ScaleResult)
        assert result.scale_factors == pytest.approx((1.5, 1.5, 1.5))

        bb_min, bb_max = result.mesh.bounding_box
        assert bb_max[0] - bb_min[0] == pytest.approx(150.0)
        assert bb_max[1] - bb_min[1] == pytest.approx(30.0)
        assert abs(bb_min[2]) == pytest.approx(12.0)

    def test_scale_barge_to_smaller(self, barge_mesh: PanelMesh) -> None:
        """Scale a 100x20x8 barge down to 50x10x4."""
        source = ScaleDimensions(length_m=100.0, beam_m=20.0, draft_m=8.0)
        target = ScaleDimensions(length_m=50.0, beam_m=10.0, draft_m=4.0)
        result = scale_mesh_to_target(barge_mesh, source, target)

        assert result.scale_factors == pytest.approx((0.5, 0.5, 0.5))

        bb_min, bb_max = result.mesh.bounding_box
        assert bb_max[0] - bb_min[0] == pytest.approx(50.0)
        assert bb_max[1] - bb_min[1] == pytest.approx(10.0)
        assert abs(bb_min[2]) == pytest.approx(4.0)

    def test_scale_result_contains_validation(
        self, barge_mesh: PanelMesh
    ) -> None:
        """ScaleResult.validation must be populated with expected keys."""
        source = ScaleDimensions(length_m=100.0, beam_m=20.0, draft_m=8.0)
        target = ScaleDimensions(length_m=200.0, beam_m=40.0, draft_m=16.0)
        result = scale_mesh_to_target(barge_mesh, source, target)

        v = result.validation
        assert "bounding_box_ok" in v
        assert "actual_dims" in v
        assert "no_degenerate_panels" in v
        assert "degenerate_count" in v
        assert "normals_consistent" in v
        assert "max_aspect_ratio" in v
        assert "high_aspect_panels" in v

    def test_scale_round_trip(self, barge_mesh: PanelMesh) -> None:
        """Scale up then down should restore original geometry within tolerance."""
        source = ScaleDimensions(length_m=100.0, beam_m=20.0, draft_m=8.0)
        target = ScaleDimensions(length_m=200.0, beam_m=40.0, draft_m=16.0)

        # Scale up
        up = scale_mesh_to_target(barge_mesh, source, target)
        # Scale back down
        down = scale_mesh_to_target(up.mesh, target, source)

        np.testing.assert_allclose(
            down.mesh.vertices, barge_mesh.vertices, atol=1e-10
        )

    def test_scale_non_uniform_ratios(self, barge_mesh: PanelMesh) -> None:
        """Non-uniform scale factors should be correctly computed."""
        source = ScaleDimensions(length_m=100.0, beam_m=20.0, draft_m=8.0)
        target = ScaleDimensions(length_m=120.0, beam_m=25.0, draft_m=10.0)
        result = scale_mesh_to_target(barge_mesh, source, target)

        assert result.scale_factors == pytest.approx((1.2, 1.25, 1.25))
        assert result.source_dims == source
        assert result.target_dims == target


# ---------------------------------------------------------------------------
# TestValidateScaledMesh
# ---------------------------------------------------------------------------


class TestValidateScaledMesh:
    """Tests for validate_scaled_mesh."""

    def test_valid_mesh_passes(self, barge_mesh: PanelMesh) -> None:
        """A well-formed barge mesh should pass all validation checks."""
        dims = ScaleDimensions(length_m=100.0, beam_m=20.0, draft_m=8.0)
        v = validate_scaled_mesh(barge_mesh, dims)

        assert v["bounding_box_ok"] is True
        assert v["no_degenerate_panels"] is True
        assert v["degenerate_count"] == 0

    def test_detects_dimension_mismatch(self, barge_mesh: PanelMesh) -> None:
        """Wrong target dimensions should fail bounding_box_ok."""
        wrong_dims = ScaleDimensions(length_m=200.0, beam_m=40.0, draft_m=16.0)
        v = validate_scaled_mesh(barge_mesh, wrong_dims)
        assert v["bounding_box_ok"] is False

    def test_detects_degenerate_panels(self) -> None:
        """A mesh with zero-area panels should be flagged."""
        # Create a panel where all 4 vertices are collinear
        vertices = np.array(
            [
                [0, 0, 0],
                [1, 0, 0],
                [2, 0, 0],
                [3, 0, 0],
            ],
            dtype=np.float64,
        )
        panels = np.array([[0, 1, 2, 3]], dtype=np.int32)
        mesh = PanelMesh(
            vertices=vertices,
            panels=panels,
            name="degenerate",
            format_origin=MeshFormat.GDF,
        )
        dims = ScaleDimensions(length_m=3.0, beam_m=1.0, draft_m=1.0)
        v = validate_scaled_mesh(mesh, dims)

        assert v["no_degenerate_panels"] is False
        assert v["degenerate_count"] >= 1

    def test_high_aspect_ratio_flagged(self) -> None:
        """An extremely stretched panel should have high aspect ratio."""
        # Create a very elongated quad: 100m long x 0.1m wide
        vertices = np.array(
            [
                [0, 0, 0],
                [100, 0, 0],
                [100, 0.1, 0],
                [0, 0.1, 0],
            ],
            dtype=np.float64,
        )
        panels = np.array([[0, 1, 2, 3]], dtype=np.int32)
        mesh = PanelMesh(
            vertices=vertices,
            panels=panels,
            name="high_aspect",
            format_origin=MeshFormat.GDF,
        )
        dims = ScaleDimensions(length_m=100.0, beam_m=0.1, draft_m=1.0)
        v = validate_scaled_mesh(mesh, dims)

        assert v["max_aspect_ratio"] > 10.0
        assert v["high_aspect_panels"] >= 1

    def test_actual_dims_reported(self, barge_mesh: PanelMesh) -> None:
        """Validation should report actual measured dimensions."""
        dims = ScaleDimensions(length_m=100.0, beam_m=20.0, draft_m=8.0)
        v = validate_scaled_mesh(barge_mesh, dims)

        assert v["actual_dims"]["length_m"] == pytest.approx(100.0)
        assert v["actual_dims"]["beam_m"] == pytest.approx(20.0)
        assert v["actual_dims"]["draft_m"] == pytest.approx(8.0)


# ---------------------------------------------------------------------------
# TestExportScaledGdf
# ---------------------------------------------------------------------------


class TestExportScaledGdf:
    """Tests for export_scaled_gdf."""

    def test_export_creates_file(
        self, barge_mesh: PanelMesh, tmp_path: Path
    ) -> None:
        """GDF file should be written to the specified path."""
        out = tmp_path / "scaled.gdf"
        result = export_scaled_gdf(barge_mesh, out)
        assert result.exists()
        assert result.stat().st_size > 0

    def test_export_readable(
        self, barge_mesh: PanelMesh, tmp_path: Path
    ) -> None:
        """Exported GDF should be readable by GDFHandler."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import (
            GDFHandler,
        )

        out = tmp_path / "readable.gdf"
        export_scaled_gdf(barge_mesh, out)

        handler = GDFHandler()
        read_back = handler.read(out)
        assert read_back.n_panels == barge_mesh.n_panels

    def test_round_trip_preserves_geometry(
        self, barge_mesh: PanelMesh, tmp_path: Path
    ) -> None:
        """Write then read should give equivalent vertex positions."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import (
            GDFHandler,
        )

        out = tmp_path / "roundtrip.gdf"
        export_scaled_gdf(barge_mesh, out)

        handler = GDFHandler()
        read_back = handler.read(out)

        # GDF deduplicates vertices, so we compare panel center positions
        # which are independent of vertex ordering/dedup.
        original_centers = np.sort(barge_mesh.panel_centers, axis=0)
        readback_centers = np.sort(read_back.panel_centers, axis=0)
        np.testing.assert_allclose(original_centers, readback_centers, atol=1e-4)


# ---------------------------------------------------------------------------
# TestScaleDimensions
# ---------------------------------------------------------------------------


class TestScaleDimensions:
    """Tests for the ScaleDimensions dataclass validation."""

    def test_valid_dimensions(self) -> None:
        dims = ScaleDimensions(length_m=100.0, beam_m=20.0, draft_m=8.0)
        assert dims.length_m == 100.0
        assert dims.beam_m == 20.0
        assert dims.draft_m == 8.0

    def test_rejects_zero_length(self) -> None:
        with pytest.raises(ValueError, match="length_m"):
            ScaleDimensions(length_m=0.0, beam_m=20.0, draft_m=8.0)

    def test_rejects_negative_beam(self) -> None:
        with pytest.raises(ValueError, match="beam_m"):
            ScaleDimensions(length_m=100.0, beam_m=-5.0, draft_m=8.0)

    def test_rejects_negative_draft(self) -> None:
        with pytest.raises(ValueError, match="draft_m"):
            ScaleDimensions(length_m=100.0, beam_m=20.0, draft_m=-1.0)
