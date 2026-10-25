# Worked example: rectangular panel scaling

## Summary

A synthetic tilted rectangle demonstrates `scale_mesh_uniform` and
`scale_mesh_parametric`. Independent Cartesian geometry gives the transformed
vertices, reference point and area. Both operations preserve panel connectivity
and leave the source arrays and metadata unchanged. The executable example uses
only the analytic vertices below and writes a JSON record in a supplied directory.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-25 | Added an executable Cartesian example to make panel scaling reproducible. |

## Inputs

Coordinates are in metres and areas in square metres. Scale factors are
dimensionless. All geometry, factors and descriptive metadata are synthetic
assumptions [^inputs]. No physical or unit-conversion constants are used.

| Input | Value | Basis |
| --- | --- | --- |
| Ordered vertices | A = (1, 2, -3), B = (5, 2, -3), C = (5, 5, 1), D = (1, 5, 1) m | Assumption [^inputs] |
| Connectivity | One quadrilateral, indices (0, 1, 2, 3) in A, B, C, D order | Assumption [^inputs] |
| Reference point | R = (2, -1, 3) m | Assumption [^inputs] |
| Uniform factor | f = 2 | Assumption [^inputs] |
| Unequal axis factors | (sx, sy, sz) = (3, 2, 0.5) | Assumption [^inputs] |
| Source attributes | Name `synthetic_rectangle`, unknown file format, no symmetry plane | Assumption [^inputs] |

The functions multiply coordinates about the Cartesian origin; the reference
point is transformed with the same factors, rather than used as a scaling pivot.
The API behavior is described in [mesh_scaler.py](../mesh_scaler.py).

## Derivation

The following results are independent hand calculations from the input table.
The rectangle identity is area = length × perpendicular width, and the length
of a Cartesian vector is the square root of the sum of its squared components.
These geometric identities supply the expected results; no mesh routine supplies
an expected value.

For the source rectangle, AB = (4, 0, 0) m and AD = (0, 3, 4) m. Their dot
product vanishes, so they are perpendicular. Their lengths are 4 m and
sqrt(3² + 4²) = 5 m, giving area 20 m². The arithmetic mean of the corners is
(3, 3.5, -1) m. The chosen winding gives a unit normal proportional to
(0, -4, 3), hence (0, -0.8, 0.6). All these values follow from the input
coordinates [^inputs].

Uniform scaling maps P = (x, y, z) to (2x, 2y, 2z), and R to (4, -2, 6) m.
The perpendicular edges become (8, 0, 0) m and (0, 6, 8) m, with lengths
8 m and 10 m. Area is therefore 80 m², also 2² × 20 m² [^inputs].

Unequal scaling maps P to (3x, 2y, 0.5z), and R to (6, -2, 1.5) m.
The edges become (12, 0, 0) m and (0, 6, 2) m, which remain perpendicular.
Area is 12 × sqrt(6² + 2²) = 12 sqrt(40) m², approximately 75.9 m².
The exact radical is used for comparison [^inputs]. This tilted panel's area
cannot be obtained by multiplying the source area by sx × sy alone.

| Vertex | Source (m) | Uniform result (m) | Unequal-axis result (m) |
| --- | --- | --- | --- |
| A | (1, 2, -3) | (2, 4, -6) | (3, 4, -1.5) |
| B | (5, 2, -3) | (10, 4, -6) | (15, 4, -1.5) |
| C | (5, 5, 1) | (10, 10, 2) | (15, 10, 0.5) |
| D | (1, 5, 1) | (2, 10, 2) | (3, 10, 0.5) |

Table: coordinates obtained by the component multiplications above [^inputs].

## Tolerance basis

The comparison uses zero relative tolerance and an assumed absolute numerical
budget of 1e-12 in the relevant units [^tolerance]. The coordinate products are
exactly representable binary fractions; area requires a square root. At these
magnitudes, the budget exceeds `80 * np.finfo(np.float64).eps` while remaining
small compared with the assumed dimensions and areas. The value 80 is the
largest derived area above. This is a floating-point comparison budget, not a
manufacturing or hydrodynamic acceptance tolerance. Connectivity and source
snapshots are compared exactly.

## Executable example

Save the sole Python block as `example.py` outside the checkout and run
`python example.py OUTPUT_DIRECTORY` with the repository's `src` directory on
`PYTHONPATH` and its Python dependencies available. `OUTPUT_DIRECTORY` is a
supplied temporary directory; the only output is `panel-scaling.json` there.
The block runs offline, creates no historical-data inputs, and performs no
executable discovery. The
[documented-example test](../../../../../tests/hydrodynamics/hull_library/test_documented_panel_scaling.py)
extracts this exact block and executes it in an isolated temporary directory.

```python
import argparse
from copy import deepcopy
import json
import math
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from digitalmodel.hydrodynamics.hull_library.mesh_scaler import (
    scale_mesh_parametric,
    scale_mesh_uniform,
)


def snapshot(mesh):
    """Record all source arrays and descriptive attributes without aliasing."""
    return {
        "vertices": mesh.vertices.tolist(),
        "panels": mesh.panels.tolist(),
        "normals": mesh.normals.tolist(),
        "panel_areas": mesh.panel_areas.tolist(),
        "panel_centers": mesh.panel_centers.tolist(),
        "reference_point": list(mesh.reference_point),
        "metadata": deepcopy(mesh.metadata),
        "name": mesh.name,
        "format_origin": mesh.format_origin.value,
        "symmetry_plane": mesh.symmetry_plane,
        "n_vertices": mesh.n_vertices,
        "n_panels": mesh.n_panels,
        "total_area": mesh.total_area,
    }


parser = argparse.ArgumentParser(description="Run synthetic panel scaling")
parser.add_argument("output_dir", type=Path)
output_dir = parser.parse_args().output_dir

# Synthetic assumptions: Inputs table and footnote inputs.
source = PanelMesh(
    vertices=np.array(
        [[1, 2, -3], [5, 2, -3], [5, 5, 1], [1, 5, 1]], dtype=np.float64
    ),
    panels=np.array([[0, 1, 2, 3]], dtype=np.int32),
    name="synthetic_rectangle",
    reference_point=[2, -1, 3],
    metadata={
        "basis": "synthetic assumptions",
        "units": {"coordinates": "m", "area": "m^2"},
    },
)
source_before = snapshot(source)
uniform = scale_mesh_uniform(source, 2)
source_after_uniform = snapshot(source)
assert source_after_uniform == source_before
parametric = scale_mesh_parametric(source, 3, 2, 0.5)
source_after_parametric = snapshot(source)
assert source_after_parametric == source_before

# Independent Cartesian results: Derivation section, not scaling-function output.
expected = (
    (
        uniform,
        [[2, 4, -6], [10, 4, -6], [10, 10, 2], [2, 10, 2]],
        [4, -2, 6],
        80.0,
    ),
    (
        parametric,
        [[3, 4, -1.5], [15, 4, -1.5], [15, 10, 0.5], [3, 10, 0.5]],
        [6, -2, 1.5],
        12 * math.sqrt(40),
    ),
)
atol = 1e-12  # Assumed comparison budget: Tolerance basis and footnote tolerance.
for mesh, vertices, reference, area in expected:
    np.testing.assert_allclose(mesh.vertices, vertices, rtol=0, atol=atol)
    np.testing.assert_allclose(mesh.reference_point, reference, rtol=0, atol=atol)
    np.testing.assert_allclose(mesh.panel_areas, [area], rtol=0, atol=atol)
    np.testing.assert_allclose(mesh.total_area, area, rtol=0, atol=atol)
    np.testing.assert_array_equal(mesh.panels, source_before["panels"])
    assert mesh.n_vertices == source.n_vertices
    assert mesh.n_panels == source.n_panels

record = {
    "source_before": source_before,
    "source_after_uniform": source_after_uniform,
    "source_after_parametric": source_after_parametric,
    "uniform": snapshot(uniform),
    "parametric": snapshot(parametric),
}
output_dir.mkdir(parents=True, exist_ok=True)
(output_dir / "panel-scaling.json").write_text(
    json.dumps(record, indent=2, allow_nan=False) + "\n", encoding="utf-8"
)
```

## Limits

This example covers positive uniform and unequal-axis scaling of a single
analytic rectangle. It does not establish hydrodynamic suitability, mesh
convergence, hydrostatics or a closed surface. It does not use historical hull
data, response operators, mesh generation campaigns, boundary representations,
solver execution or executable discovery. It checks that scaling itself does
not mutate source metadata, including a nested mapping; it does not promise
deep independence if output metadata is later edited. The full module report,
builder and index remain open under issue #3317.

[^inputs]: Assumed synthetic coordinates, connectivity, factors and metadata,
    chosen in this example solely for an independent Cartesian hand calculation.
    They are not measured engineering data.

[^tolerance]: Assumed numerical comparison budget for the synthetic geometry in
    this example, justified by the floating-point scale in Tolerance basis.
