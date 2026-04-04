"""Tests for benchmark_mesh_schematic module.

ABOUTME: Tests for build_mesh_schematic_html, _build_panel_scatter_html,
_add_fdf_surface_traces, and _build_panel_mesh3d_html.
Covers panel geometry rendering, FDF surface traces, GDF fallback via
mocked MeshPipeline, and edge cases like empty data.
"""
from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, List
from unittest.mock import MagicMock, patch

import numpy as np
import plotly.graph_objects as go
import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_mesh_schematic import (
    _add_fdf_surface_traces,
    _build_panel_mesh3d_html,
    _build_panel_scatter_html,
    build_mesh_schematic_html,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_rao_helpers import (
    get_solver_style,
)

# Re-use conftest helpers directly
from tests.hydrodynamics.diffraction.conftest import _make_solver_results


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_panel_geometry(n_panels: int = 10, with_vertices: bool = False) -> list:
    """Create synthetic panel_geometry_data (list of dicts)."""
    rng = np.random.default_rng(42)
    panels = []
    for i in range(n_panels):
        panel: Dict[str, Any] = {
            "centroid": [float(rng.uniform(-5, 5)),
                         float(rng.uniform(-5, 5)),
                         float(rng.uniform(-3, 0))],
            "area": float(rng.uniform(0.01, 2.0)),
            "objectName": "Hull" if i % 2 == 0 else "Column",
        }
        if with_vertices:
            # Quad panel: 4 vertices
            cx, cy, cz = panel["centroid"]
            panel["vertices"] = [
                [cx - 0.5, cy - 0.5, cz],
                [cx + 0.5, cy - 0.5, cz],
                [cx + 0.5, cy + 0.5, cz],
                [cx - 0.5, cy + 0.5, cz],
            ]
        panels.append(panel)
    return panels


def _make_fdf_panels(n: int = 5) -> List[List[List[float]]]:
    """Create synthetic FDF panel list: each panel is 4 [x, y, z] vertices."""
    rng = np.random.default_rng(99)
    panels = []
    for _ in range(n):
        cx = float(rng.uniform(-10, 10))
        cy = float(rng.uniform(0, 10))  # y >= 0 for symmetry quadrant
        panel = [
            [cx - 1.0, cy - 1.0, 0.0],
            [cx + 1.0, cy - 1.0, 0.0],
            [cx + 1.0, cy + 1.0, 0.0],
            [cx - 1.0, cy + 1.0, 0.0],
        ]
        panels.append(panel)
    return panels


def _mock_mesh():
    """Create a mock mesh object with vertices, panels, n_panels, n_vertices, total_area."""
    mesh = MagicMock()
    mesh.vertices = np.array([
        [0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0],
        [0, 0, -1], [1, 0, -1], [1, 1, -1], [0, 1, -1],
    ], dtype=float)
    mesh.panels = np.array([
        [0, 1, 2, 3],
        [4, 5, 6, 7],
    ])
    mesh.n_panels = 2
    mesh.n_vertices = 8
    mesh.total_area = 2.0
    return mesh


# ---------------------------------------------------------------------------
# 1. _build_panel_scatter_html
# ---------------------------------------------------------------------------


class TestBuildPanelScatterHtml:
    """Tests for the scatter-based panel centroid HTML builder."""

    def test_returns_html_string(self):
        """Output should be a non-empty string containing HTML."""
        panels = _make_panel_geometry(10)
        html = _build_panel_scatter_html(panels, title="Test Scatter")
        assert isinstance(html, str)
        assert len(html) > 0

    def test_contains_plotly_content(self):
        """HTML should contain Plotly chart content."""
        panels = _make_panel_geometry(5)
        html = _build_panel_scatter_html(panels, title="Test")
        # Plotly CDN or div content
        assert "plotly" in html.lower() or "div" in html.lower()

    def test_custom_title_in_output(self):
        """Custom title should appear in the output HTML."""
        panels = _make_panel_geometry(3)
        html = _build_panel_scatter_html(panels, title="My Custom Title")
        assert "My Custom Title" in html

    def test_multiple_bodies_produce_traces(self):
        """Panels with different objectName should produce multiple traces."""
        panels = _make_panel_geometry(6)
        # panels alternate between "Hull" and "Column"
        html = _build_panel_scatter_html(panels, title="Multi-body")
        assert "Hull" in html
        assert "Column" in html

    def test_custom_height(self):
        """Custom height should be set in the HTML."""
        panels = _make_panel_geometry(3)
        html = _build_panel_scatter_html(panels, title="Test", height=800)
        assert "800" in html

    def test_empty_panels_returns_html(self):
        """Empty panel list should still return valid HTML without errors."""
        html = _build_panel_scatter_html([], title="Empty")
        assert isinstance(html, str)


# ---------------------------------------------------------------------------
# 2. _add_fdf_surface_traces
# ---------------------------------------------------------------------------


class TestAddFdfSurfaceTraces:
    """Tests for the FDF free-surface trace adder."""

    def test_adds_traces_to_figure(self):
        """Should add at least 2 traces (mesh + edges) to the figure."""
        fig = go.Figure()
        fdf_panels = _make_fdf_panels(5)
        initial_count = len(fig.data)
        _add_fdf_surface_traces(fig, fdf_panels)
        assert len(fig.data) > initial_count
        # Should add Mesh3d + Scatter3d = 2 traces
        assert len(fig.data) == initial_count + 2

    def test_mesh_trace_is_mesh3d(self):
        """First added trace should be Mesh3d for the surface."""
        fig = go.Figure()
        fdf_panels = _make_fdf_panels(3)
        _add_fdf_surface_traces(fig, fdf_panels)
        assert isinstance(fig.data[0], go.Mesh3d)

    def test_edge_trace_is_scatter3d(self):
        """Second added trace should be Scatter3d for edges."""
        fig = go.Figure()
        fdf_panels = _make_fdf_panels(3)
        _add_fdf_surface_traces(fig, fdf_panels)
        assert isinstance(fig.data[1], go.Scatter3d)

    def test_y_mirror_doubles_vertices(self):
        """Y-mirroring should produce more vertices than original panels."""
        fig = go.Figure()
        fdf_panels = _make_fdf_panels(4)
        _add_fdf_surface_traces(fig, fdf_panels)
        mesh_trace = fig.data[0]
        # Each panel has 4 vertices; 4 panels = 16 vertices
        # Y-mirror doubles to 32
        n_verts = len(mesh_trace.x)
        # Original 4 verts * 4 panels = 16, mirrored = 32
        assert n_verts == 4 * 4 * 2  # 32

    def test_single_panel(self):
        """A single quad panel should still produce valid traces."""
        fig = go.Figure()
        single_panel = [[[0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0]]]
        _add_fdf_surface_traces(fig, single_panel)
        assert len(fig.data) == 2

    def test_free_surface_zone_named(self):
        """Mesh3d trace should be named 'Free-surface zone'."""
        fig = go.Figure()
        _add_fdf_surface_traces(fig, _make_fdf_panels(2))
        assert fig.data[0].name == "Free-surface zone"


# ---------------------------------------------------------------------------
# 3. _build_panel_mesh3d_html
# ---------------------------------------------------------------------------


class TestBuildPanelMesh3dHtml:
    """Tests for the Mesh3d-based panel HTML builder."""

    def test_returns_html_with_vertices(self):
        """Panels with vertices should produce non-empty HTML."""
        panels = _make_panel_geometry(5, with_vertices=True)
        html = _build_panel_mesh3d_html(panels, title="Mesh3d Test")
        assert isinstance(html, str)
        assert len(html) > 0

    def test_contains_body_panels_trace(self):
        """HTML should reference 'Body panels'."""
        panels = _make_panel_geometry(5, with_vertices=True)
        html = _build_panel_mesh3d_html(panels, title="Test")
        assert "Body panels" in html

    def test_view_buttons_present(self):
        """HTML should contain view buttons (Perspective, Plan, Elevation)."""
        panels = _make_panel_geometry(3, with_vertices=True)
        html = _build_panel_mesh3d_html(panels, title="Test")
        assert "Perspective" in html
        assert "Plan" in html
        assert "Elevation" in html

    def test_no_vertices_produces_empty_mesh(self):
        """Panels without vertices should not crash (no Mesh3d data)."""
        panels = _make_panel_geometry(3, with_vertices=False)
        html = _build_panel_mesh3d_html(panels, title="No Verts")
        assert isinstance(html, str)

    def test_fdf_path_triggers_fdf_traces(self, tmp_path: Path):
        """When fdf_path is given with valid data, free-surface traces added."""
        # Write a minimal FDF file
        fdf_file = tmp_path / "test.fdf"
        lines = [
            "Title line",
            "1.0",       # RINNER
            "4 0",       # NPF, NTCL
            "0",         # NAL
            # 4 panels: x1 x2 x3 x4 y1 y2 y3 y4
            "0.0 1.0 1.0 0.0 0.0 0.0 1.0 1.0",
            "1.0 2.0 2.0 1.0 0.0 0.0 1.0 1.0",
        ]
        fdf_file.write_text("\n".join(lines))

        panels = _make_panel_geometry(3, with_vertices=True)
        html = _build_panel_mesh3d_html(
            panels, fdf_path=str(fdf_file), title="With FDF",
        )
        assert "Free-surface zone" in html

    def test_custom_height(self):
        """Custom height should be reflected in the HTML output."""
        panels = _make_panel_geometry(3, with_vertices=True)
        html = _build_panel_mesh3d_html(panels, title="Test", height=700)
        assert "700" in html


# ---------------------------------------------------------------------------
# 4. build_mesh_schematic_html — panel_geometry path
# ---------------------------------------------------------------------------


class TestBuildMeshSchematicWithPanelGeometry:
    """Tests for build_mesh_schematic_html when panel_geometry is available."""

    @patch(
        "digitalmodel.hydrodynamics.diffraction.mesh_pipeline.MeshPipeline",
    )
    def test_scatter_path_without_vertices(self, mock_pipeline_cls):
        """Without vertices, should use scatter (centroid) path."""
        panels = _make_panel_geometry(5, with_vertices=False)
        metadata = {"SolverA": {"panel_geometry": panels}}
        results = {"SolverA": _make_solver_results("SolverA")}

        html = build_mesh_schematic_html(
            solver_names=["SolverA"],
            solver_results=results,
            solver_metadata=metadata,
            get_solver_style=get_solver_style,
        )
        assert "Panel Mesh Geometry" in html
        assert "5 panels" in html
        # MeshPipeline should NOT have been instantiated
        mock_pipeline_cls.assert_not_called()

    @patch(
        "digitalmodel.hydrodynamics.diffraction.mesh_pipeline.MeshPipeline",
    )
    def test_mesh3d_path_with_vertices(self, mock_pipeline_cls):
        """With vertices, should use Mesh3d path."""
        panels = _make_panel_geometry(8, with_vertices=True)
        metadata = {"SolverA": {"panel_geometry": panels}}
        results = {"SolverA": _make_solver_results("SolverA")}

        html = build_mesh_schematic_html(
            solver_names=["SolverA"],
            solver_results=results,
            solver_metadata=metadata,
            get_solver_style=get_solver_style,
        )
        assert "Panel Mesh Geometry" in html
        assert "8 panels" in html
        assert "symmetry-expanded" in html
        mock_pipeline_cls.assert_not_called()


# ---------------------------------------------------------------------------
# 5. build_mesh_schematic_html — GDF fallback path (mocked)
# ---------------------------------------------------------------------------


class TestBuildMeshSchematicGdfFallback:
    """Tests for build_mesh_schematic_html GDF fallback via MeshPipeline."""

    @patch(
        "digitalmodel.hydrodynamics.diffraction.mesh_pipeline.MeshPipeline",
    )
    def test_gdf_fallback_loads_mesh(self, mock_pipeline_cls):
        """When no panel_geometry but mesh_path exists, MeshPipeline is used."""
        mock_pipe = MagicMock()
        mock_pipe.load.return_value = _mock_mesh()
        mock_pipeline_cls.return_value = mock_pipe

        results = {"SolverA": _make_solver_results("SolverA")}
        metadata = {"SolverA": {"mesh_path": "/fake/path/hull.gdf"}}

        html = build_mesh_schematic_html(
            solver_names=["SolverA"],
            solver_results=results,
            solver_metadata=metadata,
            get_solver_style=get_solver_style,
        )
        assert "Panel Mesh Geometry" in html
        mock_pipe.load.assert_called_once()

    @patch(
        "digitalmodel.hydrodynamics.diffraction.mesh_pipeline.MeshPipeline",
    )
    def test_gdf_fallback_summary_table(self, mock_pipeline_cls):
        """GDF fallback should produce a summary table with panel count."""
        mock_pipe = MagicMock()
        mock_pipe.load.return_value = _mock_mesh()
        mock_pipeline_cls.return_value = mock_pipe

        results = {"SolverA": _make_solver_results("SolverA")}
        metadata = {"SolverA": {"mesh_path": "/fake/hull.gdf"}}

        html = build_mesh_schematic_html(
            solver_names=["SolverA"],
            solver_results=results,
            solver_metadata=metadata,
            get_solver_style=get_solver_style,
        )
        # Summary table should have panels count and vertices count
        assert "2" in html  # n_panels
        assert "8" in html  # n_vertices

    @patch(
        "digitalmodel.hydrodynamics.diffraction.mesh_pipeline.MeshPipeline",
    )
    def test_no_mesh_path_returns_empty(self, mock_pipeline_cls):
        """No panel_geometry and no mesh_path -> empty string."""
        results = {"SolverA": _make_solver_results("SolverA")}
        metadata = {"SolverA": {}}

        html = build_mesh_schematic_html(
            solver_names=["SolverA"],
            solver_results=results,
            solver_metadata=metadata,
            get_solver_style=get_solver_style,
        )
        assert html == ""

    @patch(
        "digitalmodel.hydrodynamics.diffraction.mesh_pipeline.MeshPipeline",
    )
    def test_load_failure_returns_empty(self, mock_pipeline_cls):
        """If MeshPipeline.load raises, should return empty string."""
        mock_pipe = MagicMock()
        mock_pipe.load.side_effect = RuntimeError("File not found")
        mock_pipeline_cls.return_value = mock_pipe

        results = {"SolverA": _make_solver_results("SolverA")}
        metadata = {"SolverA": {"mesh_path": "/bad/path.gdf"}}

        html = build_mesh_schematic_html(
            solver_names=["SolverA"],
            solver_results=results,
            solver_metadata=metadata,
            get_solver_style=get_solver_style,
        )
        assert html == ""

    @patch(
        "digitalmodel.hydrodynamics.diffraction.mesh_pipeline.MeshPipeline",
    )
    def test_multiple_solvers_gdf(self, mock_pipeline_cls):
        """Multiple solvers with different meshes should produce combined HTML."""
        mesh_a = _mock_mesh()
        mesh_b = _mock_mesh()
        mesh_b.n_panels = 4
        mesh_b.n_vertices = 12
        mesh_b.total_area = 4.0
        mesh_b.vertices = np.array([
            [2, 0, 0], [3, 0, 0], [3, 1, 0], [2, 1, 0],
            [2, 0, -1], [3, 0, -1], [3, 1, -1], [2, 1, -1],
            [2, 0, -2], [3, 0, -2], [3, 1, -2], [2, 1, -2],
        ], dtype=float)
        mesh_b.panels = np.array([[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [0, 4, 5, 1]])

        mock_pipe = MagicMock()
        mock_pipe.load.side_effect = [mesh_a, mesh_b]
        mock_pipeline_cls.return_value = mock_pipe

        results = {
            "SolverA": _make_solver_results("SolverA"),
            "SolverB": _make_solver_results("SolverB"),
        }
        metadata = {
            "SolverA": {"mesh_path": "/path/a.gdf"},
            "SolverB": {"mesh_path": "/path/b.gdf"},
        }

        html = build_mesh_schematic_html(
            solver_names=["SolverA", "SolverB"],
            solver_results=results,
            solver_metadata=metadata,
            get_solver_style=get_solver_style,
        )
        assert "SolverA" in html
        assert "SolverB" in html
        assert "Panel Mesh Geometry" in html
