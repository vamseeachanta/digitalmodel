# Touchdown catenary geometry worked example

This example exposes `CatenaryAnalyzer.solve_catenary` with synthetic inputs,
an independent hand calculation and a readable answer. It covers the
worked-example slice of #3319. Design qualification is **NOT_EVALUATED**;
the parent issue remains open.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-25 | Add a reproducible synthetic geometry example with independent result readback. |

## Run

From the repository root, in a Python environment containing NumPy and PyYAML
(the existing package imports require them), select an output directory:

```bash
PYTHONPATH=src python docs/reports/modules/mooring_analysis/build_worked_example.py --output-dir "$TMPDIR/worked-example"
```

The only generated file is `worked_example.html` in that directory. Building
needs no external input data, network, licensed software or large computation.
Importing the builder creates no output. The page includes its inputs, equations,
source, solver results, units, limits and revision history. There are no shared
index changes or full-module qualification claims.

## Inputs, method and answer

The explicit synthetic inputs are transcribed from
`tests/subsea/mooring_analysis/test_catenary_geometry.py::test_vertical_span_closed_form`,
case A; the builder does not import a test fixture. They are not material data:

| Input | Value | Unit | Basis |
| --- | --- | --- | --- |
| Vertical span and water depth, z | 100.0 | m | Synthetic case A |
| Total line length, L | 500.0 | m | Synthetic case A |
| Submerged mass per length, `weight_water` | 1000.0 | kg/m | Synthetic case A, API convention |
| Horizontal tension, H | 981.0 | kN | Synthetic case A |
| Gravity, g | 9.81 | m/s² | Existing `CatenaryAnalyzer` convention |
| Chain diameter, unused | 1.0 | mm | Synthetic placeholder |
| Breaking load, unused | 1.0 | kN | Synthetic placeholder |
| Axial stiffness EA, unused | 1.0 | kN | Synthetic placeholder |

Source: [General Physics I: Classical Mechanics, section 19.3, The Catenary
(2024 web edition)](https://phys.libretexts.org/@go/page/92208).
Its identities are z = a(cosh(x/a) − 1), s = a sinh(x/a), a = H/w.
Here x is horizontal reach from touchdown, z is vertical rise and s is
suspended arc length. The conversion w = `weight_water` × g / 1000 gives
9.81 kN/m; the divisor converts N to kN. Thus a = 981 / 9.81 = 100 m.

Using cosh²(u) − sinh²(u) = 1 gives s² = z² + 2az. Consequently
s = 100√3 m and cosh(x/a) = 2 gives x = 100 ln(2 + √3) m.
Vertical force balance gives V = ws; T² = H² + (ws)² = (H + wz)²,
so T = H + wz = 981 + 9.81 × 100 = 1962 kN.

| Quantity | Independent expression | Answer | Unit |
| --- | --- | --- | --- |
| Suspended length, s | 100√3 | 173.2 | m |
| Horizontal reach, x | 100 ln(2 + √3) | 131.7 | m |
| Fairlead tension, T | H + wz | 1962.0 | kN |

Results are displayed to one decimal place. The subprocess tests read the
visible numbers and units back and compare them with the independent
expressions, allowing half the display increment for rounding. Rebuild tests
compare bytes after repeated builds and after changing the output directory.

## Assumptions and limits

The line has uniform positive submerged weight, is inextensible and begins at
a horizontal touchdown tangent. Sufficient line length is required: the
synthetic 500 m total exceeds the suspended 100√3 m. The remaining grounded
length is 326.8 m, calculated as L − s. This is a touchdown-to-fairlead span;
general two-suspended-endpoint geometry is outside the example.

Stiffness, design-factor acceptance, model export, environmental loading,
dynamics and the separate historical port are **NOT_EVALUATED**. The arbitrary
breaking load and EA are unused placeholders. The full module report, design
qualification and shared indexes remain follow-up work under the parent issue.
