# Pipe section sizing worked example

## Summary

This synthetic example documents geometric sizing prerequisites through
`PipeSizing.pipe_section_properties`. Outside diameter 4 m and inside diameter
2 m are arithmetic probes, not a product specification. Pressure containment,
collapse and combined loading are **NOT_EVALUATED**. The parent report issue
remains open; no full-module qualification is claimed.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-25 | Initial geometry example with independent integrals and a reproducible HTML builder. |
| 1 | 2026-10-28 | Added bounded module reporting and source records without changing the standalone example. |

## Build and read

From the repository root, with a writable temporary directory selected:

```bash
PYTHONPATH=src uv run --no-project --with pytest python docs/reports/modules/pipe_capacity/build_worked_example.py --output-dir "$TMPDIR/worked-example"
```

Read `worked_example.html` in the selected directory. The builder creates only
that page and its parent directory as needed; it has no import-time execution
and updates no shared index. The page embeds its equations, inputs, results,
units, revision history and limits, with no external resources. Rebuilding in
the same or a different output directory produces identical page bytes.

The builder uses the standard library and the existing sizing class, without
external input data, licensed software, network access or large computation.
The documented uv command may need network access to prepare pytest if it is
not cached; an already prepared Python interpreter can run the builder offline.

## Independent basis

The defined synthetic diameters give inner radius 1 m and outer radius 2 m.
With polar area element `dA = r dr dtheta` and angle from `0` to `2*pi`,
direct integration gives:

```text
A  = 2*pi * [r^2/2]_1^2 = 3*pi m^2
Ix = pi   * [r^4/4]_1^2 = 15*pi/4 m^4 = Iy
J  = 2*pi * [r^4/4]_1^2 = 15*pi/2 m^4 = Ix + Iy
```

The angular integral is `2*pi` for area and polar moment; the integral of
`sin(theta)^2` is `pi` for the centroidal second moment. These hand integrals
are the source of the numerical coefficients and expected test values.
The existing method calculates `A`, `I` and `J` using a minimal geometry mapping;
the symbol `I` denotes either centroidal diametral second moment. Lengths are interpreted
as metres because the method itself performs no unit conversion.
The page displays values to one decimal place; tests check unrounded method
results and the rounded rendered values independently.

## Checks and follow-up

```bash
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/structural/pipe_capacity/test_worked_example.py -q -rA -p no:cacheprovider
```

The tests execute the builder in a temporary directory and check independent
integrals, rendered numbers and units, scope, deterministic rebuilds, output
confinement, import safety and Git ignore exceptions for the deliverable files.
The broader pipe capacity test directory also requires the existing module's
runtime dependencies; the example tests require only pytest.

Full module reporting, pressure containment, collapse, combined-loading
qualification and shared report indexes remain follow-up work. A section selected
for service requires material properties, dimensional allowances, load cases
and the applicable design assessments before a capacity conclusion is drawn.

## Module report index

[Module report](report.html) adds a ten-section method map, independent geometry
controls, computed results, evidence limits and a concrete qualification plan.
The earlier standalone example and its command above remain unchanged. The
earlier reference to full module reporting remains historical context: this
bounded report supplies scaffolding, while broader numerical qualification
and shared report indexes remain follow-up work.

Rebuild from the repository root with an explicit directory in scratch storage:

```bash
PYTHONPATH=src uv run --no-project --with pytest python docs/reports/modules/pipe_capacity/build_report.py --output-dir "$TMPDIR/pipe-capacity-report"
```

The builder writes only `report.html`, `sources.json` and `changes.json` there;
intermediate records stay in memory. The page is self-contained. Rebuilds from
different directories produce identical bytes, without runtime timestamps or
absolute paths. [Source records](sources.json) bind exact repository symbols,
line ranges and source digests to evidence status. [Change records](changes.json)
retain the revision reason and the page's engineering change panel.

The new test suite is `tests/structural/pipe_capacity/test_module_report.py`.
It checks all unrounded geometric properties against independent polar
integrals, units, parsed visible values, controlled result mutation, deterministic
rebuilds and report pointers. Its roundoff budget is derived for the synthetic
probe from binary64 arithmetic and subtraction amplification; it does not
establish an engineering tolerance. Reproducibility is not physical validation.

Pressure containment, collapse, combined loading, material strength and code
compliance remain **NOT_EVALUATED**. The parent issue remains open for broader
qualification; the public router's custom sizing path is not qualified by the
common sizing call in this example. Production calculations are unchanged.
