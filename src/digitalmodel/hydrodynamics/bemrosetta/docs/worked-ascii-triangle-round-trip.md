# Worked example: ASCII STL triangle round trip

## Summary

An in-memory synthetic right triangle is written as ASCII STL and read back
through `STLHandler`. Area, axis-aligned bounding box and signed unit normal are
compared as geometric quantities, without requiring vertex indices or array
ordering to survive serialization. A translated case checks position and area
independently. This is a mesh conversion example, not solver qualification.

## Revision history

| Revision | Date       | What changed and why                                       |
| -------- | ---------- | ---------------------------------------------------------- |
| A        | 2026-10-25 | Added a synthetic triangle example with executable checks. |
| B        | 2026-10-25 | Stated the example scope, acceptance checks and deferred report work. |

## Plan-lite

The scope is one synthetic triangle written and restored through the offline
ASCII STL handler, with one translated case. Acceptance checks compare both
original and restored area, bounds and signed unit normal against the
independent hand calculations below. Translation must shift the bounds while
preserving area and normal direction. Comparisons use the stated absolute
tolerance with zero relative tolerance; vertex ordering is not an acceptance
criterion. The tests execute the document's sole Python block verbatim.

The full module report, report builder and index are deferred. General mesh
equivalence, other STL cases and solver qualification require separate work.

## Inputs and basis

The following coordinates are chosen synthetic assumptions, not measured data.
A common arbitrary length unit is used throughout; area uses its square. STL
stores coordinate numbers without assigning a physical unit. No physical
constant or unit conversion factor is used.

| Input                  | Assumed value                            | Basis                         |
| ---------------------- | ---------------------------------------- | ----------------------------- |
| Ordered vertices A,B,C | (1, 2, 3), (5, 2, 3), (1, 5, 3)          | Synthetic assumptions         |
| Optional translation t | (10, -4, 2); default (0, 0, 0)           | Synthetic assumptions         |
| Triangle connectivity  | A then B then C, indices (0, 1, 2, -1)   | Local mesh convention below   |
| Output                 | `triangle.stl` in a supplied temp folder | Disposable synthetic artifact |

The local implementation references are
[`PanelMesh`](../models/mesh_models.py) for connectivity, area, bounds and normal
calculation, and [`STLHandler`](../mesh/stl_handler.py) for ASCII serialization
and reading. The unused fourth panel index is `-1`, the triangle padding sentinel
in these implementations; `0`, `1` and `2` identify the assumed ordered vertices.
The writer's `.6e` format uses six digits after the decimal point in scientific
notation. All chosen coordinates, including the translated ones, are exactly
representable in that format. The reader deduplicates vertices and recomputes
normals from connectivity instead of trusting the STL facet-normal field.

## Independent hand derivation

The ordered edges are B - A = (4, 0, 0) and C - A = (0, 3, 0).
They are perpendicular, so the base is 4 and height is 3 length units.
The triangle occupies half of the corresponding rectangle:
area = base × height / 2 = 4 × 3 / 2 = **6 square units**.
The factor 1/2 is this geometric partition, not an empirical constant.

By the right-hand cross-product definition, the ordered edges give
(B - A) × (C - A) = (0, 0, 4 × 3) = (0, 0, 12).
Its magnitude is 12, so the signed unit normal is **(0, 0, 1)**.
These expected values are hand calculations from the assumed coordinates,
independent of `PanelMesh` methods.

Taking minima and maxima of the stated coordinates component by component gives
**minimum (1, 2, 3)** and **maximum (5, 5, 3)**. The equal vertical extrema
are expected for a planar triangle.

Adding t = (10, -4, 2) gives A' = (11, -2, 5), B' = (15, -2, 5),
and C' = (11, 1, 5). The translated bounds are therefore
**minimum (11, -2, 5)** and **maximum (15, 1, 5)**.
Translation cancels in each edge difference, preserving the base, height,
area of 6 square units and unit normal (0, 0, 1).

### Numerical comparison criterion

An absolute tolerance of `1e-12` is an assumed numerical comparison criterion
for this example: length units for bounds, square units for area, and
dimensionless for normal components. Relative tolerance is zero, so a zero
normal component cannot gain a relative allowance. These small integer inputs
have no ASCII coordinate rounding loss; their edge differences, cross product
and area are also exactly representable in binary floating point. The tolerance
allows floating-point evaluation noise while remaining far below a change of
one coordinate unit or a reversed normal. It is not a general STL error bound
or an engineering acceptance tolerance. The tests use these same criteria but
compare against the independent hand results above, as well as checking the
translation case.

## Executable example

Use the repository's Python environment with `src` on `PYTHONPATH` and its
runtime dependencies installed. Save the single block below as a script outside
the repository, then invoke `python example.py TEMP_DIRECTORY`. Supply an
existing disposable temporary directory; the script writes only `triangle.stl`
there. Add `--offset 10 -4 2` for the translated assumptions above. The caller
controls the directory's lifetime and cleanup; no generated STL is retained in
the repository. The `results` mapping exposes the before/after geometry for the
executable documentation tests.

```python
import argparse
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.bemrosetta.mesh.stl_handler import STLHandler
from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh

parser = argparse.ArgumentParser(description="Synthetic ASCII STL round trip")
parser.add_argument("temporary_directory", type=Path)
parser.add_argument("--offset", nargs=3, type=float, default=(0.0, 0.0, 0.0))
args = parser.parse_args()
output_directory = args.temporary_directory.resolve(strict=True)
if not output_directory.is_dir():
    raise NotADirectoryError(output_directory)

# Synthetic assumptions from Inputs and basis; no physical unit conversion.
vertices = np.array(
    [[1.0, 2.0, 3.0], [5.0, 2.0, 3.0], [1.0, 5.0, 3.0]], dtype=np.float64
)
vertices = vertices + np.asarray(args.offset, dtype=np.float64)
# Ordered A, B, C with the documented triangle padding sentinel.
mesh = PanelMesh(
    vertices=vertices, panels=np.array([[0, 1, 2, -1]], dtype=np.int32)
)
handler = STLHandler()
stl_path = output_directory / "triangle.stl"
handler.write(mesh, stl_path, binary=False)
restored = handler.read(stl_path)

results = {}
for label, item in (("before", mesh), ("after", restored)):
    results[label] = {
        "area": item.total_area,
        "bounds": np.array(item.bounding_box),
        "normals": item.normals.copy(),
    }

# Numerical comparison criterion above; no assumption about vertex ordering.
atol = 1e-12
for quantity in ("area", "bounds", "normals"):
    np.testing.assert_allclose(
        results["after"][quantity], results["before"][quantity], rtol=0, atol=atol
    )
print(results)
```

## Limits

This example covers a single nondegenerate planar triangle and one assumed
translation. Its area, bounds and normal checks do not establish general mesh
equivalence or preservation of arbitrary decimal coordinates. ASCII precision,
duplicate vertices, malformed facets, binary STL and quadrilateral splitting
need separate cases. A single open triangle has no enclosed volume or defined
outward hull orientation; the positive vertical direction follows only from
the stated winding.

Only the offline mesh handler is called. No coefficient validator, executable
discovery, tool-invoking converter, external mesh or licensed solver is used.
Package initialization may import additional Python modules; the example makes
no calls to their conversion, discovery or solver interfaces. The full module
report, builder and index remain outside this worked-example slice.
