# Flexible pipe annulus example

This synthetic example computes full-wall layer areas and dry mass per unit
length through the existing cross-section API. It covers geometry and mass only.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-25 | Add a reproducible geometry and mass example with independent integral checks. |

## Synthetic section

The following values define the case; they are chosen synthetic inputs, not
measured dimensions, material specifications or values from a standard. Both
layers are homogeneous solid annuli, contiguous from inside to outside.

| Layer | Inner diameter (m) | Outer diameter (m) | Radial thickness (m) | Density (kg/m³) |
| --- | --- | --- | --- | --- |
| Pressure sheath | 0.200 | 0.210 | 0.005 | 950 |
| Outer sheath | 0.210 | 0.230 | 0.010 | 1200 |

Area is the polar integral `A = ∫∫ r dr dθ = π(OD² − ID²)/4` over the annulus.
Uniform density gives layer mass per length `m′ = ρA`; section dry mass per
length is the sum of the layer masses. The independent derivation and the
declared bounds are repeated in the [rebuild test](../../../../../tests/subsea/flexible_pipes/test_annulus_example.py)
docstring. No expected result is taken from the implementation.

## Run

From the repository root, with `uv` available:

```sh
PYTHONPATH=src uv run --no-project python docs/reports/modules/flexible_pipes/examples/annulus.py
```

This API path uses the Python standard library only; Python must satisfy the
repository's declared version requirement. `PYTHONPATH=src` makes the source
package available without installing the wider application's dependencies.
Output is strict JSON with layer areas in m² and dry masses in kg/m, including
the section total. Repeated runs emit identical JSON. Imports produce no output
or files. By default the script writes only to stdout; `--output <file>` writes
the same JSON to the requested file instead. Parent directories must exist.

## Limits of use

`NOT_EVALUATED`: stiffness interaction, API compliance and design qualification.
The example does not represent wound armour, void fractions, bore contents,
buoyancy, stress, fatigue or bend-stiffener performance. It establishes only
the declared synthetic annulus geometry and full-wall dry mass calculation.
The full module report, figures, changes panel and registry remain outside
this example; the remaining scope of issue #3343 stays open.
