"""
ABOUTME: Evaluation script demonstrating PyVista 3D visualization capabilities.

Demonstrates mesh creation, scalar coloring, offscreen rendering, STL export,
pipe/spline geometry (relevant for riser/mooring visualization), mesh quality
metrics, and optional GDF panel mesh loading from hull_library.

Run with:
    uv run scripts/examples/pyvista_3d_evaluation.py

Outputs saved to /tmp/pyvista_eval/
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import numpy as np
import pyvista as pv

# Headless mode — no display required
pv.OFF_SCREEN = True

OUTPUT_DIR = Path("/tmp/pyvista_eval")


def ensure_output_dir() -> None:
    """Create the output directory if it doesn't exist."""
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    print(f"Output directory: {OUTPUT_DIR}")


def demo_basic_meshes() -> dict[str, pv.PolyData]:
    """Create basic geometric primitives: sphere, box, cylinder."""
    print("\n=== Basic Mesh Creation ===")

    sphere = pv.Sphere(radius=1.0, theta_resolution=30, phi_resolution=30)
    print(f"  Sphere: {sphere.n_points} points, {sphere.n_cells} cells")

    box = pv.Box(bounds=(-1, 1, -0.5, 0.5, -0.3, 0.3))
    print(f"  Box:    {box.n_points} points, {box.n_cells} cells")

    cylinder = pv.Cylinder(radius=0.5, height=2.0, resolution=40)
    print(f"  Cylinder: {cylinder.n_points} points, {cylinder.n_cells} cells")

    return {"sphere": sphere, "box": box, "cylinder": cylinder}


def demo_scalar_coloring(sphere: pv.PolyData) -> None:
    """Demonstrate scalar coloring on a mesh — e.g. stress/pressure visualization."""
    print("\n=== Scalar Coloring ===")

    # Add elevation-based scalars (simulates a field like pressure)
    z_values = sphere.points[:, 2]
    sphere["elevation"] = z_values
    print(f"  Added 'elevation' scalar: min={z_values.min():.3f}, max={z_values.max():.3f}")

    # Add cell-centered scalar (simulates per-panel results)
    cell_centers = sphere.cell_centers().points
    radial_dist = np.linalg.norm(cell_centers, axis=1)
    sphere.cell_data["radial_distance"] = radial_dist
    print(f"  Added 'radial_distance' cell scalar: {len(radial_dist)} values")

    # Render with scalar coloring
    plotter = pv.Plotter(off_screen=True, window_size=(1024, 768))
    plotter.add_mesh(sphere, scalars="elevation", cmap="coolwarm", show_edges=False)
    plotter.add_scalar_bar("Elevation (Z)", vertical=True)
    plotter.camera_position = "xy"
    out_path = OUTPUT_DIR / "sphere_elevation.png"
    plotter.screenshot(str(out_path))
    plotter.close()
    print(f"  Saved scalar coloring screenshot: {out_path}")


def demo_offscreen_rendering(meshes: dict[str, pv.PolyData]) -> None:
    """Demonstrate offscreen rendering of multiple meshes to PNG."""
    print("\n=== Offscreen Rendering ===")

    plotter = pv.Plotter(off_screen=True, window_size=(1280, 960))
    colors = {"sphere": "steelblue", "box": "coral", "cylinder": "mediumseagreen"}
    positions = {"sphere": (0, 0, 0), "box": (3, 0, 0), "cylinder": (-3, 0, 0)}

    for name, mesh in meshes.items():
        shifted = mesh.copy()
        shifted.translate(positions[name], inplace=True)
        plotter.add_mesh(shifted, color=colors[name], show_edges=True, label=name)

    plotter.add_legend()
    plotter.camera_position = "xy"
    plotter.camera.zoom(0.8)

    out_path = OUTPUT_DIR / "multi_mesh_scene.png"
    plotter.screenshot(str(out_path))
    plotter.close()
    print(f"  Saved multi-mesh scene: {out_path}")


def demo_stl_export(meshes: dict[str, pv.PolyData]) -> None:
    """Demonstrate STL export for CAD interoperability."""
    print("\n=== STL Export ===")

    for name, mesh in meshes.items():
        out_path = OUTPUT_DIR / f"{name}.stl"
        mesh.save(str(out_path))
        file_size = out_path.stat().st_size
        print(f"  Exported {name}.stl ({file_size:,} bytes)")


def demo_spline_pipe() -> pv.PolyData:
    """Create pipe geometry from a spline — relevant for riser/mooring visualization."""
    print("\n=== Spline Pipe Geometry (Riser/Mooring) ===")

    # Define a catenary-like spline path
    n_pts = 50
    t = np.linspace(0, 1, n_pts)
    x = t * 100.0  # horizontal span 100m
    y = np.zeros(n_pts)
    z = -50.0 * np.cosh((t - 0.5) * 2.0) + 50.0 * np.cosh(1.0)  # catenary shape

    points = np.column_stack([x, y, z])
    spline = pv.Spline(points, n_points=200)
    print(f"  Spline: {spline.n_points} points, length={spline.length:.1f}")

    # Create tube (pipe) around spline — outer diameter 0.3m
    pipe = spline.tube(radius=0.3, n_sides=20)
    print(f"  Pipe: {pipe.n_points} points, {pipe.n_cells} cells")

    # Render the pipe
    plotter = pv.Plotter(off_screen=True, window_size=(1280, 720))
    plotter.add_mesh(pipe, color="orange", smooth_shading=True)
    plotter.add_mesh(spline, color="red", line_width=3, label="centerline")
    plotter.add_legend()
    plotter.camera_position = "xz"

    out_path = OUTPUT_DIR / "riser_pipe.png"
    plotter.screenshot(str(out_path))
    plotter.close()
    print(f"  Saved pipe rendering: {out_path}")

    # Export pipe STL
    stl_path = OUTPUT_DIR / "riser_pipe.stl"
    pipe.save(str(stl_path))
    print(f"  Exported pipe STL: {stl_path} ({stl_path.stat().st_size:,} bytes)")

    return pipe


def demo_mesh_quality(sphere: pv.PolyData) -> None:
    """Demonstrate mesh quality metrics — useful for FEA/CFD preprocessing."""
    print("\n=== Mesh Quality Metrics ===")

    # Use cell_quality (new API, key=measure name) with fallback to deprecated API
    if hasattr(sphere, "cell_quality"):
        quality = sphere.cell_quality(quality_measure="scaled_jacobian")
        q_key = "scaled_jacobian"
    else:
        quality = sphere.compute_cell_quality(quality_measure="scaled_jacobian")
        q_key = "CellQuality"
    q_values = quality.cell_data[q_key]
    print(f"  Quality measure: scaled_jacobian")
    print(f"  Min: {q_values.min():.4f}")
    print(f"  Max: {q_values.max():.4f}")
    print(f"  Mean: {q_values.mean():.4f}")
    print(f"  Std: {q_values.std():.4f}")

    # Render quality map
    plotter = pv.Plotter(off_screen=True, window_size=(1024, 768))
    plotter.add_mesh(quality, scalars=q_key, cmap="RdYlGn", show_edges=True)
    plotter.add_scalar_bar("Scaled Jacobian", vertical=True)
    plotter.camera_position = "xy"

    out_path = OUTPUT_DIR / "mesh_quality.png"
    plotter.screenshot(str(out_path))
    plotter.close()
    print(f"  Saved quality map: {out_path}")


def demo_panel_mesh_roundtrip() -> None:
    """Load a GDF panel mesh via hull_library and convert to PyVista, if available."""
    print("\n=== Panel Mesh Roundtrip (hull_library) ===")

    try:
        from digitalmodel.hydrodynamics.hull_library.decimation_vtk import (
            panel_mesh_to_pyvista,
            pyvista_to_panel_mesh,
        )
        from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
            MeshFormat,
            PanelMesh,
        )
    except ImportError as e:
        print(f"  Skipped — cannot import hull_library: {e}")
        return

    # Create a simple panel mesh (quad box)
    vertices = np.array([
        [0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0],  # bottom
        [0, 0, 1], [1, 0, 1], [1, 1, 1], [0, 1, 1],  # top
    ], dtype=np.float64)
    panels = np.array([
        [0, 1, 2, 3],  # bottom
        [4, 5, 6, 7],  # top
        [0, 1, 5, 4],  # front
        [1, 2, 6, 5],  # right
        [2, 3, 7, 6],  # back
        [3, 0, 4, 7],  # left
    ], dtype=np.int32)

    panel_mesh = PanelMesh(
        vertices=vertices,
        panels=panels,
        name="eval_box",
        format_origin=MeshFormat.GDF,
    )
    print(f"  PanelMesh: {panel_mesh.n_vertices} vertices, {panel_mesh.n_panels} panels")

    # Convert to PyVista
    polydata = panel_mesh_to_pyvista(panel_mesh)
    print(f"  PyVista PolyData: {polydata.n_points} points, {polydata.n_cells} cells")

    # Convert back
    result = pyvista_to_panel_mesh(polydata, "roundtrip_box")
    print(f"  Roundtrip PanelMesh: {result.n_vertices} vertices, {result.n_panels} panels")

    # Render the panel mesh
    plotter = pv.Plotter(off_screen=True, window_size=(800, 600))
    plotter.add_mesh(polydata, color="lightblue", show_edges=True, edge_color="navy")
    plotter.camera_position = "iso"

    out_path = OUTPUT_DIR / "panel_mesh_box.png"
    plotter.screenshot(str(out_path))
    plotter.close()
    print(f"  Saved panel mesh rendering: {out_path}")


def print_system_info() -> None:
    """Print PyVista and VTK version info."""
    import vtk

    print("=== System Info ===")
    print(f"  Python:  {sys.version.split()[0]}")
    print(f"  PyVista: {pv.__version__}")
    print(f"  VTK:     {vtk.vtkVersion.GetVTKVersion()}")
    print(f"  NumPy:   {np.__version__}")
    print(f"  Backend: {pv.global_theme.jupyter_backend if hasattr(pv.global_theme, 'jupyter_backend') else 'N/A'}")
    print(f"  OFF_SCREEN: {pv.OFF_SCREEN}")


def main() -> None:
    """Run the full PyVista evaluation suite."""
    print("=" * 60)
    print("  PyVista 3D Visualization — Evaluation Script")
    print("=" * 60)

    print_system_info()
    ensure_output_dir()

    # 1. Basic meshes
    meshes = demo_basic_meshes()

    # 2. Scalar coloring
    demo_scalar_coloring(meshes["sphere"].copy())

    # 3. Offscreen rendering
    demo_offscreen_rendering(meshes)

    # 4. STL export
    demo_stl_export(meshes)

    # 5. Spline pipe geometry
    demo_spline_pipe()

    # 6. Mesh quality
    demo_mesh_quality(meshes["sphere"].copy())

    # 7. Panel mesh roundtrip
    demo_panel_mesh_roundtrip()

    # Summary
    print("\n" + "=" * 60)
    output_files = sorted(OUTPUT_DIR.glob("*"))
    print(f"  Evaluation complete. {len(output_files)} files in {OUTPUT_DIR}:")
    for f in output_files:
        print(f"    {f.name:30s} {f.stat().st_size:>10,} bytes")
    print("=" * 60)


if __name__ == "__main__":
    main()
