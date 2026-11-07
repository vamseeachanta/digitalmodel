# Tank settlement screening analysis

This offline workflow reduces a shell-elevation survey and diagnostic edge and
bottom geometry. The four API 653 Annex B acceptance checks return
`NOT_EVALUATED`; the shared FFS decision is `ESCALATE`, with the workflow action
`ESCALATE-TO-LEVEL-3`. Current-edition criteria and applicability have not been
verified. No operating, re-level or repair decision is established.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-20 | Initial diagnostic workflow; unresolved acceptance criteria are exposed rather than inferred. |
| 1 | 2026-10-24 | Validate input keys, expose survey conditioning and coverage uncertainty, suppress undefined directions, preserve small report values and clarify routing. |

## Run offline

From the repository root, with the project dependencies installed:

```sh
uv run python -m digitalmodel.asset_integrity.tank_settlement_workflow \
  examples/workflows/api653-tank-settlement-screen/input.yml
```

The CLI will write `settlement-summary.json` and `settlement-report.html` to the
input directory's `results/` folder. `--output-dir <directory>` selects a separate
output folder. The registered invocation is standalone and does not use the
central engine or import a licensed solver. The JSON retains calculation
precision; the human-facing report uses three significant figures so small
nonzero tilts, angles and residuals remain visible.

## Input basis

The committed input is synthetic test data, not measured or qualified engineering
input. Dimensions are assumed for this hand calculation only. It supplies:

- Tank diameter and height in metres, fill height in metres, course thicknesses
  in millimetres, and a material-grade description. No material property is
  inferred from the description. These fields are retained for follow-up.
- Shell station angles in degrees in `[0, 360)`, with one datum and direction;
  elevations in millimetres, positive upwards. The closing station is not
  repeated. Station order may vary; irregular spacing is supported by the fit
  but is not code-qualified.
- Edge radial extent in metres, shell elevation and interior reference elevation
  in millimetres from the same datum. The secant slope includes any planar tilt;
  it is not an Annex B corrected edge-settlement measure.
- Bottom-profile distances in metres, increasing and nonnegative, and elevations
  in millimetres. Departures from an endpoint chord are diagnostic only; one
  profile does not establish the code-defined depression radius or boundary.

Edge and bottom inputs may be omitted; their missing-data flags are then returned.
Omission never becomes a zero measurement. Four distinct shell stations are the
software minimum for three fit coefficients plus a residual; this is not an
API survey-spacing requirement.

### Input and output schema

The configuration must be a mapping with `basename: tank_settlement_screen` and
a `tank_settlement` mapping. Within `tank_settlement`, `tank` and `shell_survey`
are required; `edge`, `bottom_profile` and `component_id` are optional. Each
supplied survey must be a mapping. Missing required keys, unexpected keys in
these input blocks and non-mapping blocks raise `ValueError`.

| Input block | Required keys; no additional keys allowed |
| --- | --- |
| `tank` | `diameter_m`, `height_m`, `fill_height_m`, `course_thicknesses_mm`, `material_grade` |
| `shell_survey` | `angles_deg`, `elevations_mm` |
| `edge` | `radial_extent_m`, `shell_elevation_mm`, `reference_elevation_mm` |
| `bottom_profile` | `distances_m`, `elevations_mm` |

The result retains `schema_version: 1`. Downstream workflow routing reads the
top-level `action` field, currently `ESCALATE-TO-LEVEL-3`. The nested
`decision.action` is the shared FFS vocabulary value `ESCALATE`; it records the
assessment decision and is not the workflow routing field. Neither establishes
a code verdict or authorizes a solver run.

`geometry.shell.design_matrix_condition_number` is the ratio of the largest to
smallest singular value of the design matrix with columns `1`, `cos(theta)` and
`sin(theta)`. It indicates sensitivity of the fit to survey errors; it has no
acceptance threshold. `largest_station_gap_deg` remains a geometric diagnostic.
`decision.applicability.flags` always includes `SHELL_COVERAGE_UNVERIFIED` until
Annex B station count and spacing requirements are verified, including for an
evenly spaced survey. A good fit statistic does not qualify survey coverage.

`geometry.shell.high_point_azimuth_deg` is `null` when the fitted first-harmonic
amplitude is zero or numerically negligible relative to the largest absolute
elevation departure from the mean. The numerical cutoff is a relative factor of
`1e-12`, independent of units or any standard's acceptance criteria. A survey
containing only second-harmonic variation therefore has no fitted high-point
direction. The fitted amplitude and residuals remain available in the JSON.

## Method and worked answer

A planar elevation over a circle is written as
`z(theta) = a + b cos(theta) + c sin(theta)`. The coefficients minimise the sum
of squared residuals. This is ordinary linear least squares, as described in the
[NIST/SEMATECH e-Handbook (2003), sections 4.1.4.1 and 4.4.3.1](https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd431.htm).
The calculation uses the documented
[NumPy least-squares routine](https://numpy.org/doc/stable/reference/generated/numpy.linalg.lstsq.html).
These are mathematical references, not API acceptance criteria.

For the synthetic example, eight stations are separated by 45 degrees:
`z = 10 + 3 cos(theta) + 4 sin(theta) + 2 cos(2 theta)` mm.
Discrete orthogonality of the constant, first and second harmonics gives the
independent hand result:

- Fitted coefficients `(a, b, c) = (10.0, 3.0, 4.0)` mm.
- First-harmonic amplitude `sqrt(3² + 4²) = 5.0` mm.
- Fitted diametral elevation difference `2 × 5 = 10.0` mm.
- Tilt slope `10 mm / 10 m = 1.0 mm/m`; high-point azimuth `atan2(4,3) = 53.1°`.
- Residuals `(2.0, 0.0, -2.0, 0.0, 2.0, 0.0, -2.0, 0.0)` mm.
  Squared residual sum is 16.0 mm²; total squared deviation is 116.0 mm².
  `R² = 1 - 16/116`; displayed as 86.2 %. This is a fit statistic, not a passed
  Annex B cosine-fit validity check. Uniform elevations have undefined `R²`.
- Edge drop is `-2 - (-12) = 10.0` mm across 2.0 m: `5.0 mm/m`.
- The bottom chord through `(0 m, 0 mm)` and `(4 m, 4 mm)` gives elevations
  `(0.0, 1.0, 3.0, 4.0)` mm at the input stations. Measured-minus-chord
  departures are `(0.0, -5.0, -2.0, 0.0)` mm: maximum depression 5.0 mm,
  maximum bulge 0.0 mm.

Maximum absolute shell residual is not the Annex B local out-of-plane deflection.
The JSON and report deliberately label the results as diagnostic geometry.

## Acceptance evidence still needed

API 653 *Tank Inspection, Repair, Alteration, and Reconstruction*, requested
sixth edition (2024), Annex B, is the issue's target. The requested edition,
current clause numbering, fit-validity criteria, allowable equations, edge/bottom
curves and applicability restrictions have not been verified from that edition.
No coefficient, material property or acceptance limit is taken from memory or
transcribed from another edition. No API clause text is reproduced here.

Before a code verdict can be issued, the maintained knowledge source will need
verified edition-specific pointers and source access. Survey spacing, datum and
repeatability, localized distortion, combined settlement, roof/nozzle behaviour,
weld orientation, bottom geometry and material basis will need engineering
review. A verified screen may establish whether Level 3 work is needed; the
current escalation action does not itself prescribe a solver run.

The implementation covers survey reduction, diagnostic profiles and an offline
decision/report workflow. Later work comprises verified Annex B limit checks,
code-defined local-deflection and depression/edge methods, ACCEPT / RE-LEVEL /
REPAIR mapping, and the separate routing repository's Deckhand entry. This
experimental standalone workflow is not advertised as the latest stable route.

The tests contain independent hand calculations and exercise the committed
example through its CLI. No published Annex B acceptance example has been
reproduced, and no field or solver qualification is claimed.
