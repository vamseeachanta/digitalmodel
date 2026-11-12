# Static trajectory clearance

`minimum_surface_gap` calculates the minimum Cartesian distance between two
piecewise-linear centerlines and subtracts two explicit, constant radii.
It returns the centerline witness points, segment indices and segment parameters.
This is an elementary geometry helper for the broader interference analysis.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| Initial | 2026-10-27 | Add the static geometry contract and a hand-calculated example. |

## Input and result contract

Each trajectory contains at least two three-coordinate points in metres in the
same Cartesian frame. Lists, tuples and iterable numeric arrays are supported;
no array package is required. Radii are finite, nonnegative and in metres.
Malformed shapes, non-real values, booleans and nonfinite values raise
`ValueError`. Repeated points define zero-length point segments.

The signed gap equals `centerline_distance - radius_a - radius_b`.
A negative gap denotes overlap of the radius offsets, zero denotes tangency,
and a positive gap denotes separation. End caps are rounded radius offsets of
the endpoint. This definition does not model flat pipe end faces.

Witnesses lie on the centerlines, with zero-based segment indices and affine
parameters from zero to one. Point segments use parameter zero. Ties retain the
first candidate encountered; reversing or swapping trajectories can change
which equally close witnesses are returned without changing the minimum gap
beyond floating-point rounding.

## Runnable example

All numbers below are synthetic Cartesian inputs. The independent hand
calculation uses the right triangle with transverse sides of 3 m and 4 m:
the centerline separation is 5 m, and the gap is 5 - 0.2 - 0.3 = 4.5 m.
These radii are example inputs, not recommended design values.

```pycon
>>> from digitalmodel.subsea.riser import minimum_surface_gap
>>> result = minimum_surface_gap(
...     [(0, 0, 0), (10, 0, 0)],
...     [(0, 3, 4), (10, 3, 4)],
...     radius_a=0.2, radius_b=0.3,
... )
>>> result.centerline_distance
5.0
>>> result.surface_gap
4.5
>>> result.witness_a, result.witness_b
((0.0, 0.0, 0.0), (0.0, 3.0, 4.0))

```

Run the saved example from the repository root:

```sh
PYTHONPATH=src uv run --no-project python -m doctest src/digitalmodel/subsea/riser/docs/trajectory-clearance.md
```

## Calculation basis and limits

For each segment pair the helper checks endpoint projections and the interior
common perpendicular, when it lies on both segments. These candidates exhaust
the boundary and interior minima of the squared Euclidean distance on the
segment parameter square. Parallel and collinear segments use the boundary
candidates. Unit directions and cross products avoid a subtraction of nearly
equal squared dot products; no engineering tolerance or spacing is embedded.

The calculation takes time proportional to the product of the segment counts.
Binary64 arithmetic can lose small offsets at large global coordinates; use a
local Cartesian origin. Unrepresentable intermediate geometry or radius sums
raise `ValueError`. This is not an exact-arithmetic predicate.

No hydrodynamic interference factor, dynamic motion envelope, standard minimum
spacing or compliance verdict is inferred. Those require separate inputs,
methods and validation. Integration into a broader interference workflow remains
outside this helper's scope.
