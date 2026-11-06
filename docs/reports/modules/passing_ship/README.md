# Synthetic normalized-section worked example

## Summary

This example checks `s1_function` and `ds1_dx` against an independent
polynomial at synthetic stations. The result is a dimensionless area shape
and a slope per metre. Hydrodynamic force qualification is **NOT_EVALUATED**.
This is the worked-example slice of #3326; the parent issue remains open.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-25 | Added the synthetic polynomial example and subprocess readback tests to expose its numerical basis and limits. |

## Build

From the repository root, with NumPy and SciPy available in the active Python
environment, run:

```sh
PYTHONPATH=src uv run --no-project --with pytest python docs/reports/modules/passing_ship/build_worked_example.py --output-dir "$TMPDIR/worked-example"
```

Select a writable output directory. The builder creates only
`worked_example.html` there. It does not update shared indexes, execute at import
time or embed output paths and build timestamps. Rebuilds have identical bytes.
The existing formulation source is loaded directly to avoid importing unrelated
plotting and solver packages. NumPy and SciPy are dependencies of that source;
the example requires no external input data, network access, licensed software
or large computation. Dependency installation is separate from offline execution.

## Independent hand calculation H1

All numerical inputs here are synthetic definitions for this example, not
measurements or calibrated coefficients. Define a length `L = 4 m` and stations
`x = (-2, -1, 0, 1, 2) m`, measured from the midpoint. Independently require a
quadratic with zero values at both endpoints and a unit midpoint:

```text
S(x) = a*x^2 + b*x + c
S(0) = 1                         => c = 1
S(-2 m) = 0                      => 4 m^2*a - 2 m*b + 1 = 0
S( 2 m) = 0                      => 4 m^2*a + 2 m*b + 1 = 0
Subtract the endpoint equations  => 4 m*b = 0 => b = 0 m^-1
Substitute back                  => a = -1/4 m^-2
S(x) = 1 - x^2/(4 m^2)
dS/dx = -x/(2 m^2), for -2 m < x < 2 m
```

Direct substitution gives the independent oracle below. The builder obtains its
computed values from the shipped helpers, rather than from this oracle.

| x [m] | S [dimensionless] | Polynomial slope [m^-1] | Slope meaning |
| --- | --- | --- | --- |
| -2 | 0 | 1 | one-sided from interior |
| -1 | 3/4 | 1/2 | interior derivative |
| 0 | 1 | 0 | interior derivative |
| 1 | 3/4 | -1/2 | interior derivative |
| 2 | 0 | -1 | one-sided from interior |

The implementation in
`src/digitalmodel/hydrodynamics/passing_ship/formulations.py` uses
`S = 1 - (2*x/L)^2` and `dS/dx = -8*x/L^2` on the closed interval and
returns zero outside it. At `L = 4 m` these reduce to H1. The endpoint
slope values returned by `ds1_dx` are polynomial limits from the interior;
the two-sided derivative of the zero-extended shape does not exist there.

## Verification and limits

`test_section_shape_example_matches_hand_polynomial` runs the builder as a
subprocess and reads the rendered inputs, computed values, units and limits.
Its expected values come from H1, without importing the builder or formulation
helpers. `test_section_shape_example_rebuild_is_deterministic` checks import
side effects, identical rebuild bytes in the same and different directories,
and the files created in the temporary working directory.

```sh
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/hydrodynamics/passing_ship -q -rA -p no:cacheprovider
```

S is not a physical area or force. No physical area scale, density, speed,
separation or depth is supplied, and no force in N or moment in N m is computed.
The example supplies no calibration or external coefficient table and makes no
full-module qualification claim. Physical-force evidence, full reports and
shared indexes remain follow-up work requiring separate review.

## Module report index

[Module report](report.html): the ten-section passing-ship module analysis adds
a source-linked method map, H1 controls, computed section results, an engineering
change panel and explicit limits. The existing standalone builder and example
above retain their command and output contract. The parent issue remains open
for broader numerical qualification.

The module report has its own [builder](build_report.py), [change record](changes.json)
and [source record](sources.json). With NumPy and SciPy available in the active
Python environment, rebuild from the repository root using an explicit scratch
output directory:

```sh
PYTHONPATH=src uv run --no-project --with pytest python docs/reports/modules/passing_ship/build_report.py --output-dir "$TMPDIR/passing-ship-report"
```

The command creates only `report.html`, `changes.json` and `sources.json` in the
supplied directory. All intermediate test and build output belongs in scratch
storage. No runtime timestamp or output path enters the artifacts. The generated
page embeds its presentation and calculation record and needs no external
resources; repository-relative source links resolve in the delivered location.

H1 above is also the report's input and equation basis. Each station value and
slope, including the one-sided endpoint limits, is independently checked. For
these integer, half and quarter inputs, the helper operations have exactly
representable binary intermediates, so the roundoff budget and absolute/relative
test tolerances are zero. This does not set a tolerance for force integration.
Visible comparisons use one decimal place: the exact shape `3/4` displays as
`0.8`, while the tests compare unrounded values. Reproducibility alone supplies
no physical validation.

The new [report regression](../../../../tests/hydrodynamics/passing_ship/test_module_report.py)
checks every displayed quantity and unit against H1 and fresh helper calls,
changes computed values to detect stale output, and compares two isolated
rebuilds with the delivered files. It also checks source locators, revision
placement, scope limits and the package pointer. Physical forces, force
coefficients, separation/depth validity and full force-calculator qualification
remain **NOT_EVALUATED**.
