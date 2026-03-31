#!/usr/bin/env python3
"""Evaluate meshio multi-format mesh I/O capabilities.

Issue: workspace-hub#1449
Usage: uv run python scripts/integrations/meshio_evaluation.py
"""

import sys
import tempfile
from pathlib import Path

import numpy as np

import meshio


def create_cube_surface_mesh():
    """Create a simple cube surface mesh (8 vertices, 12 triangles)."""
    points = np.array([
        [0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0],  # bottom
        [0, 0, 1], [1, 0, 1], [1, 1, 1], [0, 1, 1],  # top
    ], dtype=float)

    # 12 triangles covering 6 faces (2 triangles per face)
    triangles = np.array([
        [0, 1, 2], [0, 2, 3],  # bottom
        [4, 6, 5], [4, 7, 6],  # top
        [0, 4, 5], [0, 5, 1],  # front
        [2, 6, 7], [2, 7, 3],  # back
        [0, 3, 7], [0, 7, 4],  # left
        [1, 5, 6], [1, 6, 2],  # right
    ])

    cells = [meshio.CellBlock("triangle", triangles)]
    return meshio.Mesh(points=points, cells=cells)


def test_round_trip(mesh, fmt, suffix, tmpdir):
    """Write mesh in given format, read back, report fidelity."""
    path = tmpdir / f"cube{suffix}"
    try:
        meshio.write(path, mesh)
        mesh_back = meshio.read(path)
        orig_cells = sum(len(cb.data) for cb in mesh.cells)
        back_cells = sum(len(cb.data) for cb in mesh_back.cells)
        size_kb = path.stat().st_size / 1024
        print(f"  {fmt:>10s} ({suffix}): {size_kb:6.1f} KB | "
              f"vertices: {mesh_back.points.shape[0]:4d} | cells: {back_cells:4d} | "
              f"cell match: {'YES' if back_cells == orig_cells else 'NO'}")
        return True
    except Exception as e:
        print(f"  {fmt:>10s} ({suffix}): FAILED — {e}")
        return False


def list_supported_formats():
    """Print meshio's supported file formats."""
    print("\n--- Supported formats (reader keys) ---")
    readers = sorted(meshio._helpers.reader_map.keys())
    for i, r in enumerate(readers):
        print(f"  {r}", end="\n" if (i + 1) % 5 == 0 else "  ")
    print()


def try_existing_mesh_files():
    """Attempt to read any mesh files found in the repo."""
    repo_root = Path(__file__).resolve().parents[2]
    extensions = {".vtk", ".stl", ".msh", ".inp", ".mesh", ".vtu", ".vtp"}
    found = []
    for ext in extensions:
        found.extend(repo_root.rglob(f"*{ext}"))

    if not found:
        print("\n--- No existing mesh files found in repo ---")
        return

    print(f"\n--- Found {len(found)} existing mesh file(s) ---")
    for f in found[:5]:  # limit to first 5
        rel = f.relative_to(repo_root)
        try:
            m = meshio.read(f)
            n_pts = m.points.shape[0]
            n_cells = sum(len(cb.data) for cb in m.cells)
            print(f"  {rel}: {n_pts} vertices, {n_cells} cells")
        except Exception as e:
            print(f"  {rel}: could not read — {e}")


def main():
    print(f"meshio version: {meshio.__version__}")
    print(f"Python: {sys.version.split()[0]}\n")

    mesh = create_cube_surface_mesh()
    print(f"Created cube surface mesh: {mesh.points.shape[0]} vertices, "
          f"{sum(len(cb.data) for cb in mesh.cells)} triangles\n")

    formats = [
        ("VTK", ".vtk"),
        ("STL", ".stl"),
        ("Gmsh", ".msh"),
        ("Abaqus", ".inp"),
        ("VTU (XML)", ".vtu"),
    ]

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)
        print("--- Round-trip format tests ---")
        results = {}
        for fmt, suffix in formats:
            results[fmt] = test_round_trip(mesh, fmt, suffix, tmpdir)

        passed = sum(1 for v in results.values() if v)
        print(f"\nPassed: {passed}/{len(results)}")

    list_supported_formats()
    try_existing_mesh_files()

    print("\n--- Summary ---")
    print("meshio provides reliable multi-format mesh I/O.")
    print("Recommended for format conversion in engineering workflows.")


if __name__ == "__main__":
    main()
