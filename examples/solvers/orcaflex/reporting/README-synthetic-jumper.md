# Synthetic jumper HTML example

This example constructs `OrcaFlexAnalysisReport` from `GeometryData` and
`TimeSeriesData`, then writes HTML with embedded Plotly JavaScript and CSS.
It demonstrates the reporting interface without a solver file, solver binding,
external asset URL or network access. No fitness verdict is made.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-26 | Add a synthetic example to document the existing HTML interface. |

## Run

From the repository root, with the reporting dependencies installed:

```bash
PYTHONPATH=src uv run --no-sources python examples/solvers/orcaflex/reporting/synthetic_jumper.py --output-dir "$OUTPUT_DIR"
```

Set `OUTPUT_DIR` to an explicit destination outside the checkout. The example
creates that directory if needed and writes only `synthetic_jumper.html` there.
An existing report with that name is replaced. The required argument prevents
an implicit output location. Open the resulting file locally to view the charts;
no PDF or sidecar assets are written.

For use from Python, `build_report()` returns the populated model and
`render_example(output_dir)` writes it using
`generate_orcaflex_report(..., include_plotlyjs=True)`. Runtime dependencies are
the existing reporting stack, including Pydantic and Plotly; no solver extra is
needed. Dependency installation is separate from offline report generation.

## Data basis

The following invented fixture is the source of every numeric input in the
example. These are prescribed illustration values, not measured data, an earlier
design or solver output. No engineering acceptance criteria are supplied.

The geometry is a planar dogleg in a local Cartesian system, z-positive-up.
Water depth is 10.0 m. Corners are sharp for illustration and do not represent
fabricated bends. The base geometry builder labels its plots as a static
configuration; here that label describes only the supplied polyline.

| Label | Arc length [m] | X [m] | Y [m] | Z [m] |
| --- | --- | --- | --- | --- |
| End A | 0.0 | 0.0 | 0.0 | -8.0 |
| Corner A | 3.0 | 3.0 | 0.0 | -8.0 |
| Corner B | 7.0 | 3.0 | 4.0 | -8.0 |
| End B | 10.0 | 6.0 | 4.0 | -8.0 |

The prescribed histories use linear interpolation between the samples below.
Ramp end time is 0.0 s, so there is no excluded ramp interval. The histories
are independent of the geometry and carry no claim of mechanical consistency.

| Time [s] | Synthetic tension at End A [kN] | Synthetic bending at Corner A [kN·m] |
| --- | --- | --- |
| 0.0 | 10.0 | -2.0 |
| 1.0 | 14.0 | 0.0 |
| 2.0 | 10.0 | 2.0 |
| 3.0 | 6.0 | 0.0 |
| 4.0 | 10.0 | -2.0 |

## Hand calculation

The axis-aligned leg lengths are 3.0 m, 4.0 m and 3.0 m. Cumulative arc lengths
are therefore 0.0 m, 3.0 m, 7.0 m and 10.0 m.

Tension is a triangular perturbation of 10.0 kN with amplitude 4.0 kN:
the minimum is 10.0 - 4.0 = 6.0 kN at 3.0 s and the maximum is
10.0 + 4.0 = 14.0 kN at 1.0 s. Bending has minimum -2.0 kN·m at
0.0 s and 4.0 s, and maximum 2.0 kN·m at 2.0 s. Linear segments cannot
exceed their endpoint extrema. The smoke test compares these independent
expectations against the arrays actually embedded in the HTML charts.

## Current boundary and unavailable sections

`JumperRenderer` delegates dynamic content to the base builder. This example
uses that existing behavior; it does not implement the full jumper installation
report requested in issue #479.

- Available: header, model overview, geometry and key points, labelled time
  histories with units, summary and the base notation appendix.
- Unpopulated mandatory sections display “No data available”: executive
  summary, materials, boundary conditions, mesh, loads, analysis setup, static
  results and design checks. The header displays “NO CHECKS”.
- Optional other structures, extreme results and fatigue sections are absent.
  Dynamic envelopes and statistical summary are explicitly unavailable.
- Pipe properties, weight tally, rigging, crane configuration, solver section
  definitions, centre of gravity per kit, end rotations, go/no-go criteria and
  calculation references remain unavailable. No utilization or fitness verdict
  is inferred from the synthetic histories. The notation appendix is not a
  calculation reference list.
- PDF export, solver extraction and installation pipeline integration remain
  follow-up work. The base navigation can still link to omitted optional sections.

## Check

```bash
PYTHONPATH=src uv run --no-sources python -m pytest tests/solvers/orcaflex/reporting/test_synthetic_jumper_example.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/bt-example"
```

The smoke test runs the command-line example in scratch with network operations,
solver imports and writes outside the chosen output directory rejected. It
parses the generated HTML, checks embedded assets and chart data, and confirms
that missing sections introduce no renderer error block or fitness verdict.
