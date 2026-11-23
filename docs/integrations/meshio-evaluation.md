# meshio Multi-Format Mesh I/O — Evaluation

**Issue:** vamseeachanta/workspace-hub#1449
**Date:** 2026-03-31
**Version tested:** 5.3.5

## Overview

[meshio](https://github.com/nschloe/meshio) is an MIT-licensed Python library for reading and writing mesh files in 25+ formats. It provides a unified `Mesh` data structure and converts between Abaqus, ANSYS, Nastran, Gmsh, VTK, STL, Exodus, CGNS, and many more. Essential glue for multi-tool engineering workflows (OpenFOAM, ANSYS, FEniCS, Gmsh).

## Installation

```bash
uv add "meshio>=5.3.0,<6.0.0"
```

## Key API Patterns

```python
import meshio

# Read any supported format
mesh = meshio.read("model.vtk")
print(mesh.points.shape)       # (N, 3) vertex coordinates
print(mesh.cells)              # list of CellBlock objects

# Write to a different format
meshio.write("model.stl", mesh)

# Create mesh programmatically
import numpy as np
points = np.array([[0,0,0], [1,0,0], [1,1,0], [0,1,0]], dtype=float)
cells = [meshio.CellBlock("triangle", np.array([[0,1,2], [0,2,3]]))]
mesh = meshio.Mesh(points=points, cells=cells)
meshio.write("output.vtk", mesh)

# CLI conversion
# meshio convert input.msh output.vtk
```

## Round-Trip Format Test Results

| Format | Extension | File Size | Vertices | Cells | Round-trip |
|--------|-----------|-----------|----------|-------|------------|
| VTK | .vtk | 0.8 KB | 8 | 12 | Pass |
| STL | .stl | 1.4 KB | 8 | 12 | Pass |
| Gmsh | .msh | 0.6 KB | 8 | 12 | Pass |
| Abaqus | .inp | 0.8 KB | 8 | 12 | Pass |
| VTU (XML) | .vtu | 0.9 KB | 8 | 12 | Pass |

Test mesh: cube surface (8 vertices, 12 triangles).

## Test Coverage

8 integration tests at `tests/test_meshio_integration.py`:
- Import verification and version check
- VTK write/read round-trip (vertex and cell count fidelity)
- STL format conversion with geometry bounds verification
- Error handling for nonexistent files

## Existing Repo Mesh Files

meshio successfully reads `docs/domains/design/mesh/simple_plate/simple_part.msh` (81 vertices, 128 cells). Some Gmsh tutorial files with cell data incompatibilities are not readable — this is a known meshio limitation with certain Gmsh view data formats.

## Engineering Workflow Fit

- **Format bridge**: convert between solver formats (Gmsh -> Abaqus -> VTK) without manual editing
- **Post-processing pipeline**: read solver output meshes into Python for analysis/visualization
- **Mesh QA**: verify vertex/cell counts and bounding boxes after conversion
- **Complements Gmsh**: meshio reads Gmsh output for downstream processing in FEniCS, OpenFOAM, etc.

## Recommendation

**Integrate.** meshio is pure Python, MIT-licensed, well-maintained (2300+ stars), and passes all round-trip tests. It fills a critical gap in multi-tool engineering workflows where format conversion is a recurring friction point.

## Artifacts

- Evaluation script: `scripts/integrations/meshio_evaluation.py`
- Tests: `tests/test_meshio_integration.py`
- Dependency: `pyproject.toml` — `meshio>=5.3.0,<6.0.0`
