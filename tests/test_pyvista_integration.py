"""
ABOUTME: Integration tests verifying PyVista/VTK 3D visualization works in the ecosystem.

Covers mesh creation, offscreen rendering, file I/O, scalar operations, mesh
processing, and optional hull_library panel mesh roundtrip.

All tests run headless (pv.OFF_SCREEN = True).
"""

from __future__ import annotations

import numpy as np
import pytest

import pyvista as pv

# Headless mode — must be set before any Plotter is created
pv.OFF_SCREEN = True


# ---------------------------------------------------------------------------
# 1. Import and version checks
# ---------------------------------------------------------------------------


def test_import_pyvista():
    """PyVista is importable and has a version string."""
    assert hasattr(pv, "__version__")
    assert isinstance(pv.__version__, str)
    assert len(pv.__version__) > 0


def test_vtk_version():
    """VTK version is >= 9.0."""
    import vtk

    version_str = vtk.vtkVersion.GetVTKVersion()
    major = int(version_str.split(".")[0])
    assert major >= 9, f"VTK major version {major} < 9"


# ---------------------------------------------------------------------------
# 2. Basic mesh creation
# ---------------------------------------------------------------------------


def test_create_sphere():
    """pv.Sphere() creates a valid PolyData mesh."""
    sphere = pv.Sphere()
    assert isinstance(sphere, pv.PolyData)
    assert sphere.n_points > 0
    assert sphere.n_cells > 0


def test_create_box():
    """pv.Box() creates a valid PolyData mesh."""
    box = pv.Box()
    assert isinstance(box, pv.PolyData)
    assert box.n_points > 0
    assert box.n_cells > 0


def test_create_cylinder():
    """pv.Cylinder() creates a valid PolyData mesh."""
    cylinder = pv.Cylinder()
    assert isinstance(cylinder, pv.PolyData)
    assert cylinder.n_points > 0
    assert cylinder.n_cells > 0


# ---------------------------------------------------------------------------
# 3. Offscreen rendering
# ---------------------------------------------------------------------------


def test_offscreen_plotter():
    """Plotter(off_screen=True) can be created and closed without error."""
    plotter = pv.Plotter(off_screen=True)
    plotter.add_mesh(pv.Sphere())
    plotter.show(auto_close=False)
    plotter.close()


def test_screenshot_png(tmp_path):
    """plotter.screenshot() produces a valid PNG file."""
    out_file = tmp_path / "screenshot.png"
    plotter = pv.Plotter(off_screen=True, window_size=(400, 300))
    plotter.add_mesh(pv.Sphere(), color="steelblue")
    plotter.show(auto_close=False)
    plotter.screenshot(str(out_file))
    plotter.close()

    assert out_file.exists()
    assert out_file.stat().st_size > 0
    # Check PNG magic bytes
    with open(out_file, "rb") as f:
        header = f.read(8)
    assert header[:4] == b"\x89PNG", "File does not have PNG header"


# ---------------------------------------------------------------------------
# 4. File I/O
# ---------------------------------------------------------------------------


def test_stl_export(tmp_path):
    """mesh.save() exports a valid STL file."""
    sphere = pv.Sphere()
    stl_path = tmp_path / "sphere.stl"
    sphere.save(str(stl_path))

    assert stl_path.exists()
    assert stl_path.stat().st_size > 0

    # Reload and verify
    reloaded = pv.read(str(stl_path))
    assert isinstance(reloaded, pv.PolyData)
    assert reloaded.n_points > 0


# ---------------------------------------------------------------------------
# 5. Scalar operations
# ---------------------------------------------------------------------------


def test_scalar_coloring():
    """Adding scalar data to a mesh works for both point and cell data."""
    sphere = pv.Sphere()

    # Point scalars
    z_values = sphere.points[:, 2]
    sphere["elevation"] = z_values
    assert "elevation" in sphere.point_data
    assert len(sphere["elevation"]) == sphere.n_points

    # Cell scalars
    cell_ids = np.arange(sphere.n_cells, dtype=float)
    sphere.cell_data["cell_id"] = cell_ids
    assert "cell_id" in sphere.cell_data
    assert len(sphere.cell_data["cell_id"]) == sphere.n_cells


# ---------------------------------------------------------------------------
# 6. Spline and tube geometry
# ---------------------------------------------------------------------------


def test_spline_tube():
    """Spline + tube() creates valid pipe geometry for riser/mooring visualization."""
    # Define a simple catenary-like path
    n = 30
    t = np.linspace(0, 1, n)
    points = np.column_stack([
        t * 50.0,
        np.zeros(n),
        -20.0 * np.cosh((t - 0.5) * 2.0) + 20.0 * np.cosh(1.0),
    ])

    spline = pv.Spline(points, n_points=100)
    assert isinstance(spline, pv.PolyData)
    assert spline.n_points == 100

    pipe = spline.tube(radius=0.2, n_sides=16)
    assert isinstance(pipe, pv.PolyData)
    assert pipe.n_points > spline.n_points  # tube adds surface points
    assert pipe.n_cells > 0


# ---------------------------------------------------------------------------
# 7. Mesh quality
# ---------------------------------------------------------------------------


def test_mesh_quality():
    """Cell quality computation returns valid quality metrics."""
    sphere = pv.Sphere(theta_resolution=20, phi_resolution=20)

    # Use cell_quality (new API, key=measure name) with fallback to deprecated API
    if hasattr(sphere, "cell_quality"):
        quality = sphere.cell_quality(quality_measure="scaled_jacobian")
        q_key = "scaled_jacobian"
    else:
        quality = sphere.compute_cell_quality(quality_measure="scaled_jacobian")
        q_key = "CellQuality"

    assert q_key in quality.cell_data
    q_values = quality.cell_data[q_key]
    assert len(q_values) == quality.n_cells
    assert q_values.min() >= -1.0  # scaled_jacobian range is [-1, 1]
    assert q_values.max() <= 1.0


# ---------------------------------------------------------------------------
# 8. Hull library panel mesh roundtrip
# ---------------------------------------------------------------------------


def test_panel_mesh_roundtrip():
    """If decimation_vtk is importable, panel_mesh_to_pyvista roundtrip works."""
    try:
        from digitalmodel.hydrodynamics.hull_library.decimation_vtk import (
            panel_mesh_to_pyvista,
            pyvista_to_panel_mesh,
        )
        from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
            MeshFormat,
            PanelMesh,
        )
    except ImportError:
        pytest.skip("decimation_vtk or mesh_models not importable")

    # Simple quad mesh (unit cube faces)
    vertices = np.array([
        [0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0],
        [0, 0, 1], [1, 0, 1], [1, 1, 1], [0, 1, 1],
    ], dtype=np.float64)
    panels = np.array([
        [0, 1, 2, 3],
        [4, 5, 6, 7],
        [0, 1, 5, 4],
        [1, 2, 6, 5],
        [2, 3, 7, 6],
        [3, 0, 4, 7],
    ], dtype=np.int32)

    mesh = PanelMesh(
        vertices=vertices,
        panels=panels,
        name="test_cube",
        format_origin=MeshFormat.GDF,
    )

    # Forward conversion
    polydata = panel_mesh_to_pyvista(mesh)
    assert isinstance(polydata, pv.PolyData)
    assert polydata.n_points == 8
    assert polydata.n_cells == 6

    # Reverse conversion
    result = pyvista_to_panel_mesh(polydata, "roundtrip_cube")
    assert isinstance(result, PanelMesh)
    assert result.n_vertices == 8
    assert result.n_panels == 6


# ---------------------------------------------------------------------------
# 9. Mesh processing operations
# ---------------------------------------------------------------------------


def test_merge_meshes():
    """Merging multiple meshes produces a combined PolyData."""
    sphere = pv.Sphere(center=(0, 0, 0))
    box = pv.Box(bounds=(2, 3, -0.5, 0.5, -0.5, 0.5))

    merged = sphere.merge(box)
    assert isinstance(merged, pv.PolyData)
    assert merged.n_points == sphere.n_points + box.n_points
    assert merged.n_cells == sphere.n_cells + box.n_cells


def test_clip_mesh():
    """mesh.clip() produces a valid clipped mesh."""
    sphere = pv.Sphere(radius=1.0)

    clipped = sphere.clip(normal="z", origin=(0, 0, 0))
    assert isinstance(clipped, pv.PolyData)
    assert clipped.n_cells > 0
    # All remaining points should have z <= 0 (with some tolerance for boundary)
    assert clipped.points[:, 2].max() <= 0.1


def test_threshold():
    """mesh.threshold() filters cells based on scalar values."""
    sphere = pv.Sphere(theta_resolution=20, phi_resolution=20)

    # Add point scalars
    sphere["elevation"] = sphere.points[:, 2]

    # Threshold to keep only cells where elevation > 0 (upper hemisphere)
    thresholded = sphere.threshold(value=0.0, scalars="elevation")
    assert thresholded.n_cells > 0
    assert thresholded.n_cells < sphere.n_cells
