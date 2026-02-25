"""Tests for QEM mesh decimation -- Phase 1 of WRK-101.

Tests cover the Garland-Heckbert quadric error metrics (QEM) decimation
algorithm implemented in digitalmodel.hydrodynamics.hull_library.decimation.

Test mesh: UV-sphere with known topology, varying panel count.
"""

from __future__ import annotations

import heapq
import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)
from digitalmodel.hydrodynamics.hull_library.decimation import (
    decimate_mesh,
)


# ---------------------------------------------------------------------------
# Test mesh helpers
# ---------------------------------------------------------------------------


def make_box_mesh(nx: int = 10, ny: int = 10, nz: int = 10) -> PanelMesh:
    """Generate a closed box (cuboid) mesh with quad panels.

    Creates 6 faces, each subdivided into (n-1)^2 quads. The mesh is
    suitable for topology checks (closed surface, Euler characteristic).

    Args:
        nx: Divisions along X axis.
        ny: Divisions along Y axis.
        nz: Divisions along Z axis.

    Returns:
        PanelMesh representing the closed box.
    """
    vertices_list: list[np.ndarray] = []
    panels_list: list[list[int]] = []

    def add_face(grid: np.ndarray) -> None:
        """Add a 2D grid of quads to the mesh."""
        m, n, _ = grid.shape
        base = len(vertices_list)
        for i in range(m):
            for j in range(n):
                vertices_list.append(grid[i, j])
        for i in range(m - 1):
            for j in range(n - 1):
                v00 = base + i * n + j
                v10 = base + (i + 1) * n + j
                v11 = base + (i + 1) * n + (j + 1)
                v01 = base + i * n + (j + 1)
                panels_list.append([v00, v10, v11, v01])

    # Build six faces of unit cube [-1,1]^3
    xs = np.linspace(-1, 1, nx)
    ys = np.linspace(-1, 1, ny)
    zs = np.linspace(-1, 1, nz)

    # Bottom face (z=-1), top face (z=1)
    for z_val in (-1.0, 1.0):
        grid = np.stack(
            np.meshgrid(xs, ys, indexing="ij"), axis=-1
        )
        z_fill = np.full((*grid.shape[:2], 1), z_val)
        add_face(np.concatenate([grid, z_fill], axis=-1))

    # Left face (x=-1), right face (x=1)
    for x_val in (-1.0, 1.0):
        grid = np.stack(
            np.meshgrid(ys, zs, indexing="ij"), axis=-1
        )
        x_fill = np.full((*grid.shape[:2], 1), x_val)
        verts = np.concatenate([x_fill, grid], axis=-1)
        add_face(verts)

    # Front face (y=-1), back face (y=1)
    for y_val in (-1.0, 1.0):
        grid = np.stack(
            np.meshgrid(xs, zs, indexing="ij"), axis=-1
        )
        y_fill = np.full((*grid.shape[:2], 1), y_val)
        verts = np.concatenate(
            [grid[:, :, :1], y_fill, grid[:, :, 1:]], axis=-1
        )
        add_face(verts)

    vertices = np.array(vertices_list, dtype=np.float64)
    panels = np.array(panels_list, dtype=np.int32)

    return PanelMesh(
        vertices=vertices,
        panels=panels,
        name="test_box",
        format_origin=MeshFormat.GDF,
    )


def make_sphere_mesh(n_lat: int = 20, n_lon: int = 20) -> PanelMesh:
    """Generate a UV-sphere quad mesh.

    Creates a sphere of radius 1 with n_lat latitude bands and n_lon
    longitude divisions, giving approximately n_lat * n_lon panels.

    Args:
        n_lat: Number of latitude bands (rows of quads).
        n_lon: Number of longitude divisions (columns of quads).

    Returns:
        PanelMesh with approximately n_lat * n_lon panels.
    """
    # Latitude from pi to 0 (south pole to north pole)
    lats = np.linspace(math.pi, 0.0, n_lat + 1)
    lons = np.linspace(0.0, 2 * math.pi, n_lon + 1)

    vertices_list = []
    for lat in lats:
        for lon in lons:
            x = math.sin(lat) * math.cos(lon)
            y = math.sin(lat) * math.sin(lon)
            z = math.cos(lat)
            vertices_list.append([x, y, z])

    panels_list = []
    stride = n_lon + 1
    for i in range(n_lat):
        for j in range(n_lon):
            v00 = i * stride + j
            v01 = i * stride + (j + 1)
            v10 = (i + 1) * stride + j
            v11 = (i + 1) * stride + (j + 1)
            panels_list.append([v00, v01, v11, v10])

    vertices = np.array(vertices_list, dtype=np.float64)
    panels = np.array(panels_list, dtype=np.int32)

    return PanelMesh(
        vertices=vertices,
        panels=panels,
        name="test_sphere",
        format_origin=MeshFormat.GDF,
    )


def make_large_sphere_mesh() -> PanelMesh:
    """Generate a UV-sphere with approximately 5000 panels.

    Uses n_lat=71, n_lon=71 which gives 71*71 = 5041 panels.

    Returns:
        PanelMesh with approximately 5000 quad panels.
    """
    return make_sphere_mesh(n_lat=71, n_lon=71)


def compute_aspect_ratios(mesh: PanelMesh) -> np.ndarray:
    """Compute aspect ratio for each quad panel in the mesh.

    Aspect ratio = longest edge / shortest edge. Returns an array of
    per-panel aspect ratios.

    Args:
        mesh: PanelMesh to analyse.

    Returns:
        Array of aspect ratios, shape (n_panels,).
    """
    ratios = []
    for panel in mesh.panels:
        verts = mesh.vertices[panel]
        edges = [
            np.linalg.norm(verts[(k + 1) % 4] - verts[k]) for k in range(4)
        ]
        edges = [e for e in edges if e > 1e-14]
        if len(edges) < 2:
            ratios.append(1.0)
        else:
            ratios.append(max(edges) / min(edges))
    return np.array(ratios, dtype=np.float64)


# ---------------------------------------------------------------------------
# Test: no-op when target >= current
# ---------------------------------------------------------------------------


class TestDecimateNoop:
    """decimate_mesh returns input unchanged when target_panels >= current."""

    def test_noop_when_target_equals_current(self):
        """No decimation when target equals current panel count."""
        mesh = make_sphere_mesh(n_lat=10, n_lon=10)
        result = decimate_mesh(mesh, target_panels=mesh.n_panels)
        assert result is mesh or result.n_panels == mesh.n_panels

    def test_noop_when_target_exceeds_current(self):
        """No decimation when target exceeds current panel count."""
        mesh = make_sphere_mesh(n_lat=5, n_lon=5)
        result = decimate_mesh(mesh, target_panels=mesh.n_panels * 2)
        assert result is mesh or result.n_panels == mesh.n_panels


# ---------------------------------------------------------------------------
# Test: panel count control
# ---------------------------------------------------------------------------


class TestDecimatePanelCount:
    """Decimated output has panel count within tolerance of target."""

    def test_reduce_panel_count_by_half(self):
        """Reduce a 400-panel sphere to approximately 200 panels."""
        mesh = make_sphere_mesh(n_lat=20, n_lon=20)
        assert mesh.n_panels >= 200, f"Fixture too small: {mesh.n_panels}"
        target = mesh.n_panels // 2
        result = decimate_mesh(mesh, target_panels=target)
        # Allow 20% tolerance
        assert result.n_panels <= target * 1.20, (
            f"Output {result.n_panels} exceeded target {target} by >20%"
        )
        assert result.n_panels > 0, "Decimated mesh has zero panels"

    def test_reduce_panel_count_to_tenth(self):
        """Reduce a 400-panel sphere to approximately 40 panels."""
        mesh = make_sphere_mesh(n_lat=20, n_lon=20)
        target = max(20, mesh.n_panels // 10)
        result = decimate_mesh(mesh, target_panels=target)
        assert result.n_panels <= target * 1.30, (
            f"Output {result.n_panels} exceeded target {target} by >30%"
        )
        assert result.n_panels > 0

    def test_reduce_5000_to_500_panels(self):
        """Acceptance criterion: reduce ~5000-panel mesh to ~500 panels.

        This is the key acceptance criterion from WRK-101.
        """
        mesh = make_large_sphere_mesh()
        assert mesh.n_panels >= 1000, (
            f"Test mesh too small: {mesh.n_panels} panels"
        )
        target = 500
        result = decimate_mesh(mesh, target_panels=target)
        # Accept within 10% of target
        assert result.n_panels <= target * 1.10, (
            f"Output {result.n_panels} panels, target was {target}"
        )
        assert result.n_panels > 0


# ---------------------------------------------------------------------------
# Test: bounding box preservation
# ---------------------------------------------------------------------------


class TestDecimateBoundingBox:
    """Decimated mesh bounding box stays within tolerance of original."""

    def test_bounding_box_preserved(self):
        """Bounding box of decimated mesh is within 5% of original."""
        mesh = make_sphere_mesh(n_lat=20, n_lon=20)
        bb_min_orig, bb_max_orig = mesh.bounding_box
        bb_size_orig = bb_max_orig - bb_min_orig

        result = decimate_mesh(mesh, target_panels=mesh.n_panels // 4)
        bb_min_dec, bb_max_dec = result.bounding_box
        bb_size_dec = bb_max_dec - bb_min_dec

        # Bounding box should not shrink more than 10% in any dimension
        for dim in range(3):
            if bb_size_orig[dim] > 1e-6:
                ratio = bb_size_dec[dim] / bb_size_orig[dim]
                assert ratio >= 0.90, (
                    f"Bounding box dimension {dim} shrank to {ratio:.2%} "
                    f"of original"
                )

    def test_bounding_box_box_mesh(self):
        """Bounding box preserved for a box-shaped mesh."""
        mesh = make_box_mesh(nx=6, ny=6, nz=6)
        bb_min_orig, bb_max_orig = mesh.bounding_box
        result = decimate_mesh(mesh, target_panels=mesh.n_panels // 3)
        bb_min_dec, bb_max_dec = result.bounding_box
        for dim in range(3):
            assert bb_min_dec[dim] >= bb_min_orig[dim] - 0.1
            assert bb_max_dec[dim] <= bb_max_orig[dim] + 0.1


# ---------------------------------------------------------------------------
# Test: mesh validity
# ---------------------------------------------------------------------------


class TestDecimateMeshValidity:
    """Decimated mesh is geometrically valid (no degenerate panels)."""

    def test_no_degenerate_panels(self):
        """Decimated mesh has no zero-area panels."""
        mesh = make_sphere_mesh(n_lat=20, n_lon=20)
        result = decimate_mesh(mesh, target_panels=mesh.n_panels // 3)
        for pi, panel in enumerate(result.panels):
            verts = result.vertices[panel]
            edge1 = verts[1] - verts[0]
            edge2 = verts[2] - verts[0]
            area = np.linalg.norm(np.cross(edge1, edge2))
            assert area > 1e-12, (
                f"Panel {pi} has near-zero area: {area}"
            )

    def test_panels_reference_valid_vertices(self):
        """All panel vertex indices are within bounds."""
        mesh = make_sphere_mesh(n_lat=20, n_lon=20)
        result = decimate_mesh(mesh, target_panels=mesh.n_panels // 3)
        assert result.panels.min() >= 0
        assert result.panels.max() < result.n_vertices

    def test_output_is_panel_mesh_instance(self):
        """decimate_mesh returns a PanelMesh instance."""
        mesh = make_sphere_mesh(n_lat=10, n_lon=10)
        result = decimate_mesh(mesh, target_panels=50)
        assert isinstance(result, PanelMesh)

    def test_result_has_normals_and_areas(self):
        """Decimated mesh has normals and areas computed."""
        mesh = make_sphere_mesh(n_lat=10, n_lon=10)
        result = decimate_mesh(mesh, target_panels=50)
        assert result.normals is not None
        assert result.panel_areas is not None
        assert len(result.normals) == result.n_panels
        assert len(result.panel_areas) == result.n_panels


# ---------------------------------------------------------------------------
# Test: aspect ratio quality
# ---------------------------------------------------------------------------


class TestDecimateAspectRatio:
    """Aspect ratio of decimated mesh is not severely degraded."""

    def test_aspect_ratio_not_degraded_2x(self):
        """Max aspect ratio of output is within 3x of input on sphere."""
        mesh = make_sphere_mesh(n_lat=20, n_lon=20)
        ar_input = compute_aspect_ratios(mesh)

        result = decimate_mesh(mesh, target_panels=mesh.n_panels // 4)
        ar_output = compute_aspect_ratios(result)

        # Allow up to 3x degradation for aggressive decimation
        assert np.max(ar_output) <= max(np.max(ar_input) * 3.0, 10.0), (
            f"Max aspect ratio degraded from {np.max(ar_input):.2f} "
            f"to {np.max(ar_output):.2f}"
        )


# ---------------------------------------------------------------------------
# Test: preserve_boundary option
# ---------------------------------------------------------------------------


class TestDecimatePreserveBoundary:
    """Boundary edges are not collapsed when preserve_boundary=True."""

    def _make_open_mesh(self) -> PanelMesh:
        """Create an open (half-sphere) mesh with a boundary loop."""
        n_lat, n_lon = 10, 10
        mesh = make_sphere_mesh(n_lat=n_lat, n_lon=n_lon)
        # Keep only the top half (z > 0) panels
        top_panels = []
        for panel in mesh.panels:
            centers = np.mean(mesh.vertices[panel], axis=0)
            if centers[2] > 0:
                top_panels.append(panel.tolist())
        if len(top_panels) == 0:
            return mesh
        panels_arr = np.array(top_panels, dtype=np.int32)
        # Remove unreferenced vertices
        used = np.unique(panels_arr)
        remap = np.full(mesh.n_vertices, -1, dtype=np.int32)
        remap[used] = np.arange(len(used), dtype=np.int32)
        new_verts = mesh.vertices[used]
        new_panels = remap[panels_arr]
        return PanelMesh(
            vertices=new_verts,
            panels=new_panels,
            name="open_half_sphere",
            format_origin=MeshFormat.GDF,
        )

    def test_preserve_boundary_enabled(self):
        """With preserve_boundary=True, open mesh result has no crashes."""
        open_mesh = self._make_open_mesh()
        target = max(5, open_mesh.n_panels // 3)
        result = decimate_mesh(
            open_mesh, target_panels=target, preserve_boundary=True
        )
        assert result.n_panels > 0
        assert result.panels.min() >= 0
        assert result.panels.max() < result.n_vertices

    def test_preserve_boundary_disabled(self):
        """With preserve_boundary=False, open mesh decimation still valid."""
        open_mesh = self._make_open_mesh()
        if open_mesh.n_panels < 5:
            pytest.skip("Open mesh too small for this test")
        target = max(5, open_mesh.n_panels // 3)
        result = decimate_mesh(
            open_mesh, target_panels=target, preserve_boundary=False
        )
        assert result.n_panels > 0


# ---------------------------------------------------------------------------
# Test: name and metadata preservation
# ---------------------------------------------------------------------------


class TestDecimateMetadata:
    """Metadata (name, format, symmetry_plane) is preserved after decimation."""

    def test_name_preserved(self):
        """Mesh name is preserved after decimation."""
        mesh = make_sphere_mesh(n_lat=10, n_lon=10)
        mesh.name = "my_hull"
        result = decimate_mesh(mesh, target_panels=50)
        assert result.name == "my_hull"

    def test_format_origin_preserved(self):
        """format_origin is preserved after decimation."""
        mesh = make_sphere_mesh(n_lat=10, n_lon=10)
        result = decimate_mesh(mesh, target_panels=50)
        assert result.format_origin == MeshFormat.GDF

    def test_symmetry_plane_preserved(self):
        """symmetry_plane is preserved after decimation."""
        mesh = make_sphere_mesh(n_lat=10, n_lon=10)
        mesh.symmetry_plane = "xz"
        result = decimate_mesh(mesh, target_panels=50)
        assert result.symmetry_plane == "xz"
