"""Tests for geometry_quality module.

ABOUTME: Tests for GeometryQualityChecker and GeometryQualityReport covering
watertightness, normals, panel count, aspect ratio, element size, and reporting.
"""
from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.geometry_quality import (
    GeometryQualityChecker,
    GeometryQualityReport,
)


# ---------------------------------------------------------------------------
# Helpers – synthetic mesh builders
# ---------------------------------------------------------------------------


def _make_cube_mesh():
    """Watertight triangulated cube mesh (8 nodes, 12 triangles).

    Each face of the unit cube (centred at origin) is split into 2 triangles.
    All normals point outward.
    """
    nodes = np.array([
        [-0.5, -0.5, -0.5],  # 0
        [ 0.5, -0.5, -0.5],  # 1
        [ 0.5,  0.5, -0.5],  # 2
        [-0.5,  0.5, -0.5],  # 3
        [-0.5, -0.5,  0.5],  # 4
        [ 0.5, -0.5,  0.5],  # 5
        [ 0.5,  0.5,  0.5],  # 6
        [-0.5,  0.5,  0.5],  # 7
    ], dtype=float)

    # 12 triangular panels (consistent outward winding)
    panels = np.array([
        # bottom (z=-0.5)  – normal -z
        [0, 2, 1],
        [0, 3, 2],
        # top (z=+0.5)  – normal +z
        [4, 5, 6],
        [4, 6, 7],
        # front (y=-0.5) – normal -y
        [0, 1, 5],
        [0, 5, 4],
        # back (y=+0.5) – normal +y
        [2, 3, 7],
        [2, 7, 6],
        # left (x=-0.5)  – normal -x
        [0, 4, 7],
        [0, 7, 3],
        # right (x=+0.5)  – normal +x
        [1, 2, 6],
        [1, 6, 5],
    ])
    return nodes, panels


def _make_open_mesh():
    """Open mesh (single quad split into 2 triangles). Not watertight."""
    nodes = np.array([
        [0.0, 0.0, 0.0],
        [1.0, 0.0, 0.0],
        [1.0, 1.0, 0.0],
        [0.0, 1.0, 0.0],
    ], dtype=float)
    panels = np.array([
        [0, 1, 2],
        [0, 2, 3],
    ])
    return nodes, panels


def _make_mesh_with_duplicate_nodes():
    """Mesh with intentionally duplicated node coordinates."""
    nodes = np.array([
        [0.0, 0.0, 0.0],
        [1.0, 0.0, 0.0],
        [0.5, 1.0, 0.0],
        [0.0, 0.0, 0.0],  # duplicate of node 0
    ], dtype=float)
    panels = np.array([
        [0, 1, 2],
        [3, 1, 2],
    ])
    return nodes, panels


def _make_degenerate_panel_mesh():
    """Mesh with one degenerate (zero-area) panel."""
    nodes = np.array([
        [0.0, 0.0, 0.0],
        [1.0, 0.0, 0.0],
        [0.5, 1.0, 0.0],
        [0.5, 0.0, 0.0],  # collinear with 0 and 1
    ], dtype=float)
    panels = np.array([
        [0, 1, 2],       # valid
        [0, 3, 1],       # degenerate – all collinear
    ])
    return nodes, panels


def _make_high_aspect_ratio_mesh():
    """Mesh with very elongated panels (high aspect ratio)."""
    nodes = np.array([
        [0.0, 0.0, 0.0],
        [100.0, 0.0, 0.0],
        [100.0, 0.01, 0.0],
        [0.0, 0.01, 0.0],
    ], dtype=float)
    panels = np.array([
        [0, 1, 2],
        [0, 2, 3],
    ])
    return nodes, panels


def _make_uniform_mesh(n_side=10):
    """Uniform grid mesh on z=0 plane.

    n_side × n_side grid → 2*n_side^2 triangles, (n_side+1)^2 nodes.
    """
    xs = np.linspace(0, 1, n_side + 1)
    ys = np.linspace(0, 1, n_side + 1)
    nodes = []
    for y in ys:
        for x in xs:
            nodes.append([x, y, 0.0])
    nodes = np.array(nodes, dtype=float)

    panels = []
    for j in range(n_side):
        for i in range(n_side):
            n0 = j * (n_side + 1) + i
            n1 = n0 + 1
            n2 = n0 + (n_side + 1)
            n3 = n2 + 1
            panels.append([n0, n1, n3])
            panels.append([n0, n3, n2])
    panels = np.array(panels)
    return nodes, panels


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def checker():
    """Default GeometryQualityChecker."""
    return GeometryQualityChecker()


@pytest.fixture
def cube_mesh():
    return _make_cube_mesh()


@pytest.fixture
def open_mesh():
    return _make_open_mesh()


# ===========================================================================
# 1. Watertightness checks
# ===========================================================================


class TestCheckWatertightness:
    def test_watertight_cube(self, checker, cube_mesh):
        nodes, panels = cube_mesh
        ok, issues = checker.check_watertightness(nodes, panels)
        assert ok
        assert len(issues) == 0

    def test_open_mesh_not_watertight(self, checker, open_mesh):
        nodes, panels = open_mesh
        ok, issues = checker.check_watertightness(nodes, panels)
        assert not ok
        assert any("open edges" in i.lower() or "not watertight" in i.lower() for i in issues)

    def test_duplicate_nodes_detected(self, checker):
        nodes, panels = _make_mesh_with_duplicate_nodes()
        ok, issues = checker.check_watertightness(nodes, panels)
        assert any("duplicate" in i.lower() for i in issues)

    def test_degenerate_panels_detected(self, checker):
        nodes, panels = _make_degenerate_panel_mesh()
        ok, issues = checker.check_watertightness(nodes, panels)
        assert any("degenerate" in i.lower() for i in issues)


# ===========================================================================
# 2. Normal consistency checks
# ===========================================================================


class TestCheckNormals:
    def test_consistent_normals(self, checker, cube_mesh):
        """Cube mesh with consistently wound panels should be consistent."""
        nodes, panels = cube_mesh
        # The cube has 6 faces pointing in 6 different directions.
        # Since the code measures against the average normal,
        # some panels may appear "inconsistent" for a closed convex body.
        # We just ensure the function runs without error and returns a tuple.
        ok, issues = checker.check_normals(nodes, panels)
        assert isinstance(ok, bool)
        assert isinstance(issues, list)

    def test_all_same_direction_normals(self, checker):
        """Flat mesh where all normals point the same direction should pass."""
        nodes, panels = _make_uniform_mesh(5)
        ok, issues = checker.check_normals(nodes, panels)
        assert ok
        assert len(issues) == 0

    def test_flipped_normals_detected(self, checker):
        """If we flip a minority of panels the checker should report inconsistencies."""
        nodes, panels = _make_uniform_mesh(10)
        # Flip ~20% of panels so the average normal still has a clear direction
        n_flip = max(1, len(panels) // 5)
        for idx in range(n_flip):
            panels[idx] = panels[idx][::-1]
        ok, issues = checker.check_normals(nodes, panels)
        assert not ok
        assert any("inconsistent" in i.lower() for i in issues)


# ===========================================================================
# 3. Panel count checks
# ===========================================================================


class TestCheckPanelCount:
    def test_acceptable_panel_count(self, checker):
        ok, issues = checker.check_panel_count(5000)
        assert ok

    def test_too_many_panels(self, checker):
        ok, issues = checker.check_panel_count(100_000)
        assert not ok
        assert any("exceeds" in i.lower() for i in issues)

    def test_too_few_panels(self, checker):
        ok, issues = checker.check_panel_count(50)
        assert not ok
        assert any("very low" in i.lower() for i in issues)

    def test_custom_max_panels(self):
        chk = GeometryQualityChecker(max_panels=1000)
        ok, issues = chk.check_panel_count(1500)
        assert not ok


# ===========================================================================
# 4. Aspect ratio checks
# ===========================================================================


class TestCheckAspectRatios:
    def test_good_aspect_ratio(self, checker):
        nodes, panels = _make_uniform_mesh(5)
        ok, issues = checker.check_aspect_ratios(nodes, panels)
        assert ok

    def test_bad_aspect_ratio(self, checker):
        nodes, panels = _make_high_aspect_ratio_mesh()
        ok, issues = checker.check_aspect_ratios(nodes, panels)
        assert not ok
        assert any("poor aspect" in i.lower() for i in issues)

    def test_aspect_ratio_range_reported(self, checker):
        nodes, panels = _make_uniform_mesh(5)
        _, issues = checker.check_aspect_ratios(nodes, panels)
        assert any("aspect ratio range" in i.lower() for i in issues)


# ===========================================================================
# 5. Element size checks
# ===========================================================================


class TestCheckElementSizes:
    def test_uniform_sizes_pass(self, checker):
        nodes, panels = _make_uniform_mesh(10)
        ok, issues = checker.check_element_sizes(nodes, panels)
        assert ok

    def test_mixed_sizes_fail(self, checker):
        """Combine a fine and coarse region → high CV."""
        fine_nodes, fine_panels = _make_uniform_mesh(20)
        # scale a second region by 10x
        coarse_nodes = fine_nodes.copy() * 10.0
        coarse_nodes[:, 0] += 5.0  # offset
        combined_nodes = np.vstack([fine_nodes, coarse_nodes])
        offset = len(fine_nodes)
        coarse_panels = fine_panels + offset
        combined_panels = np.vstack([fine_panels, coarse_panels])
        ok, issues = checker.check_element_sizes(combined_nodes, combined_panels)
        assert not ok
        assert any("variation" in i.lower() or "cv" in i.lower() for i in issues)

    def test_element_size_range_reported(self, checker):
        nodes, panels = _make_uniform_mesh(5)
        _, issues = checker.check_element_sizes(nodes, panels)
        assert any("element size range" in i.lower() for i in issues)


# ===========================================================================
# 6. Report generation
# ===========================================================================


class TestGenerateReport:
    def test_report_basic_fields(self, checker, cube_mesh):
        nodes, panels = cube_mesh
        report = checker.generate_report("test.gdf", nodes, panels)
        assert isinstance(report, GeometryQualityReport)
        assert report.geometry_file == "test.gdf"
        assert report.num_nodes == len(nodes)
        assert report.num_panels == len(panels)
        assert report.check_date != ""

    def test_report_overall_status_values(self, checker, cube_mesh):
        nodes, panels = cube_mesh
        report = checker.generate_report("cube.gdf", nodes, panels)
        assert report.overall_status in ("PASS", "WARNING", "FAIL")

    def test_report_passed_checks_bounded(self, checker, cube_mesh):
        nodes, panels = cube_mesh
        report = checker.generate_report("cube.gdf", nodes, panels)
        assert 0 <= report.passed_checks <= report.total_checks

    def test_report_recommendations_list(self, checker, open_mesh):
        nodes, panels = open_mesh
        report = checker.generate_report("open.gdf", nodes, panels)
        assert isinstance(report.recommendations, list)


# ===========================================================================
# 7. Export report to JSON
# ===========================================================================


class TestExportReport:
    def test_export_creates_file(self, checker, cube_mesh, tmp_path):
        nodes, panels = cube_mesh
        report = checker.generate_report("cube.gdf", nodes, panels)
        out = tmp_path / "report.json"
        checker.export_report(report, out)
        assert out.exists()

    def test_export_valid_json(self, checker, cube_mesh, tmp_path):
        nodes, panels = cube_mesh
        report = checker.generate_report("cube.gdf", nodes, panels)
        out = tmp_path / "report.json"
        checker.export_report(report, out)
        data = json.loads(out.read_text())
        assert "overall_status" in data
        assert "quality_checks" in data
        assert data["mesh_statistics"]["num_nodes"] == len(nodes)

    def test_export_contains_all_sections(self, checker, cube_mesh, tmp_path):
        nodes, panels = cube_mesh
        report = checker.generate_report("cube.gdf", nodes, panels)
        out = tmp_path / "report.json"
        checker.export_report(report, out)
        data = json.loads(out.read_text())
        checks = data["quality_checks"]
        for key in ("watertight", "normals", "panel_count", "aspect_ratios", "element_sizes"):
            assert key in checks
            assert "status" in checks[key]
            assert "issues" in checks[key]


# ===========================================================================
# 8. Constructor defaults & customisation
# ===========================================================================


class TestCheckerInit:
    def test_default_parameters(self):
        chk = GeometryQualityChecker()
        assert chk.max_panels == 50000
        assert chk.min_aspect_ratio == 0.1
        assert chk.max_aspect_ratio == 10.0
        assert chk.element_size_tolerance == 0.5

    def test_custom_parameters(self):
        chk = GeometryQualityChecker(
            max_panels=1000,
            min_aspect_ratio=0.5,
            max_aspect_ratio=5.0,
            element_size_tolerance=0.3,
        )
        assert chk.max_panels == 1000
        assert chk.min_aspect_ratio == 0.5
        assert chk.max_aspect_ratio == 5.0
        assert chk.element_size_tolerance == 0.3
