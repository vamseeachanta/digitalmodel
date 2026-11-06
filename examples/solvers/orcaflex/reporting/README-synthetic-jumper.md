# Open lifting inputs in a jumper HTML example

This example constructs `OrcaFlexAnalysisReport` from `GeometryData` and
`TimeSeriesData`, then writes HTML with embedded Plotly JavaScript and CSS.
It demonstrates the reporting interface without a solver file, solver binding,
external asset URL or network access. No fitness verdict is made.
The numerical inputs come from the open MoorDyn-C 2.3.1 lifting example;
engineering validation is not established by this rendering demonstration.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-26 | Add a synthetic example to document the existing HTML interface. |
| 1 | 2026-10-26 | Replace arbitrary inputs with cited open lifting inputs and document their limits. |

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

The source is the published **MoorDyn-C 2.3.1** lifting test. The
[official input documentation](https://moordyn.readthedocs.io/en/master/inputs.html#the-v2-input-file)
links the public test directory. Select release tag `v2.3.1` and read:

- `tests/Mooring/lifting.txt`: POINT PROPERTIES gives the two initial endpoints,
  LINES gives `UnstrLen` as 100.5 m, and OPTIONS gives `WtrDpth` as 500 m.
- `tests/lifting.cpp`, `lifting()`: `lift_vel` is 1.0 m/s, the prescribed line
  length rate is its negative, `dt` is 1.0 s, and time starts at 0.0 s.

The source files were read on 2026-10-26. Their raw-byte SHA-256 digests are:

| Source file | SHA-256 |
| --- | --- |
| `tests/Mooring/lifting.txt` | `f5a2c94ffbe7548140ad2397508a550da9d5ba2f61560954b3a8f228e8b65305` |
| `tests/lifting.cpp` | `2198da037c82a5e31ae89f49930b23e2b8e87c135c99b388fb7976549b501087` |

Table 1. Digests identify the release files used for the numerical transcription.

Only numerical facts are transcribed; upstream implementation code is not
copied. No source file or network access is required at runtime. These are
published test inputs, not measurements or results produced by this example.
No engineering acceptance criteria are supplied.

The geometry is the straight chord joining the initial specified endpoints in
a local Cartesian system, z-positive-up. Water depth is 500 m. The 100 m chord
length is distinct from the 100.5 m unstretched line length. No static equilibrium
or deformed shape is calculated. The base geometry builder labels its plot as
a static configuration; here that label describes only the initial input chord.

| Label | Arc length [m] | X [m] | Y [m] | Z [m] |
| --- | --- | --- | --- | --- |
| Fixed end | 0.0 | 0.0 | 0.0 | 0.0 |
| Free end | 100.0 | 0.0 | 0.0 | -100.0 |

Table 2. Initial input endpoints and cumulative chord lengths.

The histories show the prescribed unstretched length and its rate, sampled at
the first five samples at 1.0 s spacing in the source's 0.0 to 10.0 s test interval.
The displayed interval is 0.0 to 4.0 s; no samples from it are excluded as ramp.
These histories are input schedules, not calculated tension, bending, endpoint
position or motion. The initial input chord is not an evolving line shape.

| Time [s] | Prescribed unstretched length [m] | Prescribed length rate [m/s] |
| --- | --- | --- |
| 0.0 | 100.5 | -1.0 |
| 1.0 | 99.5 | -1.0 |
| 2.0 | 98.5 | -1.0 |
| 3.0 | 97.5 | -1.0 |
| 4.0 | 96.5 | -1.0 |

Table 3. Prescribed length schedule; no solver response is represented.

## Hand calculation

The endpoints differ only in Z, so the chord length is
abs(-100.0 - 0.0) = 100.0 m. Cumulative chord lengths are 0.0 m and 100.0 m.

Constant-rate kinematics uses `L(t) = L(0) + rate * t`, following
[OpenStax, University Physics Volume 1, first edition (2016), section 3.4, equation 3.10](https://openstax.org/books/university-physics-volume-1/pages/3-4-motion-with-constant-acceleration).
For the source's 100.5 m initial unstretched length and -1.0 m/s rate,
the maximum is 100.5 - 1.0 * 0.0 = 100.5 m at 0.0 s; the minimum over the
displayed interval is 100.5 - 1.0 * 4.0 = 96.5 m at 4.0 s.
The rate is (96.5 - 100.5) / 4.0 = -1.0 m/s throughout that interval.
The smoke test compares these independent expectations against the arrays
actually embedded in the HTML charts, using numerical tolerances.

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
  is inferred from the prescribed histories. The notation appendix is not a
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
