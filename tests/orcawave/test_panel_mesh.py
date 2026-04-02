"""Tests for orcawave.panel_mesh module."""

import numpy as np
import pytest

from digitalmodel.orcawave.panel_mesh import (
    GDFMesh,
    PanelQuad,
    compute_displaced_volume,
    compute_mesh_summary,
    compute_panel_quality,
    compute_waterplane_area,
    read_gdf,
    write_gdf,
)


def _make_unit_panel(z: float = 0.0) -> PanelQuad:
    """Helper: create a 1m x 1m panel at height z."""
    return PanelQuad(
        vertices=[
            [0.0, 0.0, z],
            [1.0, 0.0, z],
            [1.0, 1.0, z],
            [0.0, 1.0, z],
        ]
    )


def _make_box_mesh() -> GDFMesh:
    """Helper: create a simple box mesh (6 panels) below waterline."""
    panels = [
        # Bottom (z = -2)
        PanelQuad(vertices=[[0, 0, -2], [2, 0, -2], [2, 2, -2], [0, 2, -2]]),
        # Front (x = 0)
        PanelQuad(vertices=[[0, 0, -2], [0, 2, -2], [0, 2, 0], [0, 0, 0]]),
        # Back (x = 2)
        PanelQuad(vertices=[[2, 0, -2], [2, 0, 0], [2, 2, 0], [2, 2, -2]]),
        # Left (y = 0)
        PanelQuad(vertices=[[0, 0, -2], [0, 0, 0], [2, 0, 0], [2, 0, -2]]),
        # Right (y = 2)
        PanelQuad(vertices=[[0, 2, -2], [2, 2, -2], [2, 2, 0], [0, 2, 0]]),
        # Top (z = 0, waterplane)
        PanelQuad(vertices=[[0, 0, 0], [0, 2, 0], [2, 2, 0], [2, 0, 0]]),
    ]
    return GDFMesh(
        header="Test box mesh",
        ulen=1.0,
        gravity=9.81,
        isx=0,
        isy=0,
        panels=panels,
    )


class TestPanelQuality:
    """Test panel quality metric computation."""

    def test_unit_square_area(self):
        panel = _make_unit_panel()
        metrics = compute_panel_quality(panel, 0)
        np.testing.assert_allclose(metrics.area, 1.0, atol=0.01)

    def test_unit_square_aspect_ratio(self):
        panel = _make_unit_panel()
        metrics = compute_panel_quality(panel, 0)
        np.testing.assert_allclose(metrics.aspect_ratio, 1.0, atol=0.01)

    def test_unit_square_skewness(self):
        panel = _make_unit_panel()
        metrics = compute_panel_quality(panel, 0)
        np.testing.assert_allclose(metrics.skewness, 0.0, atol=0.01)

    def test_high_aspect_ratio(self):
        panel = PanelQuad(
            vertices=[
                [0.0, 0.0, 0.0],
                [10.0, 0.0, 0.0],
                [10.0, 0.1, 0.0],
                [0.0, 0.1, 0.0],
            ]
        )
        metrics = compute_panel_quality(panel)
        assert metrics.aspect_ratio > 5.0


class TestGDFReadWrite:
    """Test GDF format read/write."""

    def test_write_then_read(self):
        mesh = _make_box_mesh()
        text = write_gdf(mesh)
        parsed = read_gdf(text)
        assert parsed.header == "Test box mesh"
        assert len(parsed.panels) == 6
        np.testing.assert_allclose(parsed.ulen, 1.0)

    def test_gdf_format_structure(self):
        mesh = GDFMesh(
            header="My mesh",
            ulen=1.0,
            gravity=9.81,
            isx=1,
            isy=0,
            panels=[_make_unit_panel()],
        )
        text = write_gdf(mesh)
        lines = text.strip().splitlines()
        assert lines[0] == "My mesh"
        assert "1" in lines[2]  # isx
        assert lines[3].strip() == "1"  # npan

    def test_symmetry_flags(self):
        mesh = _make_box_mesh()
        mesh.isx = 1
        mesh.isy = 1
        text = write_gdf(mesh)
        parsed = read_gdf(text)
        assert parsed.isx == 1
        assert parsed.isy == 1


class TestMeshSummary:
    """Test mesh summary computation."""

    def test_box_mesh_panel_count(self):
        mesh = _make_box_mesh()
        summary = compute_mesh_summary(mesh)
        assert summary.n_panels == 6

    def test_box_mesh_has_volume(self):
        mesh = _make_box_mesh()
        summary = compute_mesh_summary(mesh)
        # Box is 2x2x2, volume = 8 m^3 (no symmetry)
        assert summary.displaced_volume > 0

    def test_empty_mesh(self):
        mesh = GDFMesh(panels=[])
        summary = compute_mesh_summary(mesh)
        assert summary.n_panels == 0
        assert summary.total_area == 0.0

    def test_refinement_suggestions_few_panels(self):
        mesh = GDFMesh(panels=[_make_unit_panel()])
        summary = compute_mesh_summary(mesh)
        assert any("fewer than 100" in s for s in summary.refinement_suggestions)
