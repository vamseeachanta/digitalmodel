# Wall-shear reattachment extraction

`extract_reattachment` finds the first negative-to-positive streamwise wall-shear
crossing strictly downstream of an explicit step location. It returns the
absolute crossing coordinate, the distance `x` from the step and `x_over_height`.
This helper operates on supplied samples and does not execute a solver.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-27 | Define extraction and independent examples so absent crossings cannot be mistaken for zero length. |

## Input and result contract

Import from `digitalmodel.solvers.openfoam.validation.reattachment`.
Supply coordinate and shear sequences of equal length with at least two samples.
Coordinates must increase strictly downstream; all samples and geometry must be
finite, and step height must be positive. Coordinates, step location and step
height must use consistent length units. Shear is the signed streamwise
component, with forward flow positive; a shear magnitude cannot identify reversal.

`ReattachmentResult.status` is `found` or `not_found`. The numerical fields
`crossing_coordinate`, `x` and `x_over_height` are `None` when no crossing is
resolved. With a crossing at coordinate `c`, step location `s` and height `h`,
`x = c - s` and `x_over_height = x / h`.

The helper interpolates only the adjacent samples that bracket a rising sign
change. Its root must lie strictly after the step; the bracket may straddle the
step. A sampled zero requires negative shear before and positive shear after it.
For consecutive zeros, the first zero is reported once positive shear follows.
Tangencies, terminal zeros, falling crossings and upstream roots are ignored.
Invalid profiles raise `ValueError`; they are not silently sorted or filtered.

## Saved example and independent hand calculation

These are constructed arithmetic examples, not experimental data. The straight
line through coordinates 11 and 15 with shears -1 and +1 has its zero at their
midpoint, 13. With the step at 10 and height 2, the distance is 3 and the height
ratio is 1.5. Sampling the midpoint explicitly gives the same result. All
constants below come from this construction; zero denotes zero shear.

Run this saved Python block with the source directory on `PYTHONPATH`:

```python
from digitalmodel.solvers.openfoam.validation.reattachment import extract_reattachment

result = extract_reattachment(
    [11.0, 15.0], [-1.0, 1.0], step_location=10.0, step_height=2.0
)
assert result.status == "found"
assert result.crossing_coordinate == 13.0
assert result.x == 3.0
assert result.x_over_height == 1.5

sampled = extract_reattachment(
    [11.0, 13.0, 15.0], [-1.0, 0.0, 1.0], step_location=10.0, step_height=2.0
)
assert sampled == result

absent = extract_reattachment(
    [11.0, 15.0], [-1.0, -1.0], step_location=10.0, step_height=2.0
)
assert absent.status == "not_found"
assert absent.x is None
assert absent.x_over_height is None
print(f"x = {result.x:.1f}; x/height = {result.x_over_height:.1f}")
```

## Limits and remaining work

This is partial scope for #1175. It does not build a backward-facing-step case,
parse native wall-shear output, run a solver, compare turbulence models or
establish agreement with experimental reattachment lengths. Those tasks remain.
The caller must supply the correct wall, component and sign convention. Sampling
resolution, noise and zero plateaus limit the inferred location; the helper
provides no uncertainty estimate and does not infer unresolved crossings.
