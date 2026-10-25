# Synthetic inertia-shift worked example

## Summary

This example exercises only `basis.parallel_axis` with a synthetic point mass.
It supplies the worked-example slice of #3318; the parent issue remains open.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-25 | Add an executable synthetic example and independent rendered-value tests so the inertia shift can be checked without external data. |

## Build

From the repository root, with Python, NumPy and pytest available:

```sh
PYTHONPATH=src uv run --no-project --with pytest python docs/reports/modules/fowt_quasi_static/build_worked_example.py --output-dir "$TMPDIR/worked-example"
```

`--output-dir` is required. The builder creates `worked-example.html` only in
that directory. Importing the builder produces no output. The calculation
needs NumPy, which is already used by `basis.parallel_axis`; it does not use
external input data, solvers, network access or shared report indexes.
Once dependencies are available, generation can run offline. Package resolution
by `uv` is separate from report generation.

## Inputs, method and answer

Source: the synthetic worked-example specification in #3318. A point mass of
2 kg is placed at Cartesian offset (1, 2, 0) m relative to the reference origin.
Its intrinsic inertia about its own location is zero by definition.

The independent hand calculation sums squared distances to each axis and
uses negative coordinate products for the tensor off-diagonals:

- `Ixx = m(y^2 + z^2) = 2(2^2 + 0^2) = 8 kg m^2`.
- `Iyy = m(x^2 + z^2) = 2(1^2 + 0^2) = 2 kg m^2`.
- `Izz = m(x^2 + y^2) = 2(1^2 + 2^2) = 10 kg m^2`.
- `Ixy = Iyx = -mxy = -2 × 1 × 2 = -4 kg m^2`.
- `Ixz = Izx = -mxz = 0 kg m^2` and `Iyz = Izy = -myz = 0 kg m^2`.

All displayed answers follow from the synthetic inputs and these definitions.
The zero-offset control places the same mass at (0, 0, 0) m, making every
squared distance and coordinate product zero. The builder evaluates the shift
through `parallel_axis`; tests calculate expectations independently and read
back every rendered tensor entry, input and unit from a subprocess build.
Rebuilds in the same and different output directories must be byte-identical.

## Check

```sh
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/marine_ops/fowt_quasi_static/test_worked_example.py -q -rA -p no:cacheprovider
```

The existing full module suite has additional dependencies. The worked-example
tests use only the builder's dependency and pytest.

## Limits and follow-up

`NOT_EVALUATED`: distributed-body aggregation, turbine datasets, hydrodynamics,
response, mooring equilibrium, mooring qualification and full-module
qualification. This is a point-mass demonstration. Full reports, response or
mooring assessments, and shared indexes remain follow-up work under #3318.
