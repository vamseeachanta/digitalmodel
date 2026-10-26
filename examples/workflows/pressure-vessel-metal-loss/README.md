# Cylindrical-shell general metal loss calculation

## Summary

An ASSUMED closed cylindrical shell is evaluated at a fixed pressure as uniform
external metal loss reduces its thickness. The calculation supplies chart data
for membrane stress and remaining thickness. The thickness reference is labelled
`the result of our implementation`; the comparison supplies no service verdict.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-25 | Added an assumed-input example, independent hand checks and reproducible chart data to demonstrate general metal loss. |

## Inputs and scope

Every value in [input.json](input.json) is ASSUMED for this illustrative calculation.
No measured geometry or qualified material properties are used.

| Input | Value | Basis |
| --- | --- | --- |
| Net internal pressure | 2.0 MPa | ASSUMED, constant internal minus external pressure |
| Inner radius | 600.0 mm | ASSUMED, fixed during external loss |
| Nominal thickness | 16.0 mm | ASSUMED |
| Allowable stress input | 100.0 MPa | ASSUMED arithmetic input, no material or temperature qualification |
| Joint efficiency input | 1.0 | ASSUMED arithmetic input |
| Uniform external metal loss | 0.0 to 8.0 mm, step 0.5 mm | ASSUMED sample grid |
| Corrosion allowance in thickness reference | 0.0 mm | ASSUMED, no future allowance included |

Table 1. Inputs in mm and MPa.

The shell has closed ends and is evaluated away from ends and discontinuities.
The maximum initial thickness-to-radius ratio is 2.7 %, decreasing with loss.
The thin-shell approximation neglects radial stress and through-wall variation.
No head, nozzle, weld detail, local loss, pitting, external pressure instability,
thermal load, external axial load or bending is evaluated.

## Calculation basis and hand checks

The primary basis is first-principles static equilibrium, derived in the module
docstring of
[pressure_vessel_metal_loss.py](../../../src/digitalmodel/asset_integrity/pressure_vessel_metal_loss.py).
For a longitudinal diametral cut of length L, pressure force on projected area
is p × 2rL, and the two cut walls carry 2tL × hoop stress. Equilibrium gives
hoop stress = pr/t. At a transverse cut, closed-end thrust pπr² balances the
thin-wall axial membrane force 2πrt × longitudinal stress, giving pr/(2t).
Remaining thickness is nominal thickness minus metal loss.

The independent hand calculation uses a unit-length longitudinal cut. The
pressure force is 2400.0 N. At 16.0 mm wall thickness, the two cut wall areas
total 32.0 mm², giving hoop stress 75.0 MPa. At 8.0 mm thickness, they total
16.0 mm², giving 150.0 MPa. Closed-end longitudinal stresses are 37.5 MPa and
75.0 MPa respectively. These values are explicit test expectations.

The thickness reference calls the existing
`ASMEThicknessCalc.ug27_cylindrical_shell` implementation with the stated pressure,
radius, assumed allowable stress and efficiency, and zero corrosion allowance.
The returned thickness is 12.1 mm when rounded to one decimal place. It stays
constant because these inputs stay constant. No standard document was used to
derive or qualify that implementation in this example.

The signed thickness difference is calculated as remaining thickness minus the
unrounded implementation thickness, then rounded for the CSV. It changes from
3.9 mm at zero loss to −4.1 mm at 8.0 mm loss. Its sign expresses that arithmetic
comparison only. No acceptance level or service decision follows from it.

## Chart data and reproduction

- [membrane-stress.csv](membrane-stress.csv): metal loss on the horizontal axis;
  hoop and closed-end longitudinal stress on the vertical axis, in MPa.
- [remaining-thickness.csv](remaining-thickness.csv): metal loss on the horizontal
  axis; remaining thickness, the implementation thickness reference and their
  signed difference on the vertical axis, in mm.
- [manifest.json](manifest.json): all chart rows, assumed inputs, calculation
  basis and SHA-256 digests of inputs, sources and CSV bytes.

CSV values are rounded once to 0.1 mm or 0.1 MPa. The manifest stores the exact
CSV strings. Calculations retain full precision before presentation rounding.
These files provide chart data; a rendered figure is outside this example.

Run from the repository root, with the repository dependencies available:

```sh
PYTHONPATH=src uv run python scripts/reporting/build_pressure_vessel_metal_loss.py --output examples/workflows/pressure-vessel-metal-loss
PYTHONPATH=src uv run python -m pytest tests/asset_integrity/test_pressure_vessel_metal_loss.py tests/ansys/test_pressure_vessel.py -q -rA -p no:cacheprovider
```

Tests check both hand calculations, input rejection, delegation to the existing
thickness calculation, positive and negative thickness differences, exact
regeneration of the committed chart data, source digests and agreement between
every CSV row and its manifest row.

## Data gaps and next steps

Acceptance criteria and assessment levels are data gaps. Qualified allowable
stress at temperature, fabrication details, future corrosion, inspection
uncertainty and the condition of heads and discontinuities are also data gaps.
The maintainer must establish these inputs and the applicable assessment basis
before drawing an operating conclusion. This example establishes the numerical
trend under the stated assumptions only.
