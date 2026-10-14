---
name: fatigue-assessment
description: Assess load-block fatigue damage and life from stress histories or histograms, export traced reports, or connect signed jumper bend-radius histories to fatigue.
---

# Fatigue assessment

Use `digitalmodel.fatigue.assessment.AssessmentInput.model_validate(mapping)`
and `assess_fatigue(input)` for the validated native calculation. The adjacent
`input.schema.json` is generated from `AssessmentInput.model_json_schema()`;
validate JSON structure with the repository's `jsonschema` dependency, then
validate the runtime model for equal array lengths and unique case names.

## Input and selection

- `load_cases`: named, additive cases within one load block. A `histogram`
  supplies `stress_ranges_mpa` and `cycle_counts`; a `history` supplies ordered
  `stress_history_mpa`. Histories retain half-cycle residuals and exact ranges.
- `block_years`: duration represented by the complete set of cases, positive.
- `design_years`: required exposure duration, positive.
- `design_fatigue_factor`: supplied engineering factor, at least one.
- `design_code`: `DNV-RP-C203`; `sn_curve_name`: an existing library class;
  `environment`: `air`, `seawater_cp`, or `free_corrosion`.

Select the curve from the qualified weld/detail classification and environment.
Geometry alone does not establish the curve. Other design-code libraries remain
available through the existing structural fatigue skill; this contract does not
automatically translate their curve classes. Do not supply non-finite values,
negative ranges/counts, duplicate case names, or undocumented corrections.

## Result and export

The result returns native per-cycle endurance and damage, case contributions,
block damage, unfactored life in years, design utilisation, acceptance against
utilisation `<= 1.0`, and the case with the largest damage contribution. Cases
are accumulated; alternative scenarios require separate assessments. Zero damage
returns null life, meaning unbounded life under the supplied loading.

Call `digitalmodel.fatigue.assessment_export.export_assessment(input,
output_dir, document=DocumentMeta(...), pdf="auto")` for native JSON, Excel,
HTML and a PDF when the shared report engine has a renderer. Supply the document
date and revision description; re-issues retain earlier revision rows. Native
JSON is read back before report assembly. The export manifest records its digest
and PDF availability. Read the JSON and workbook back against the criterion;
unit tests on synthetic loads do not establish a real component's fatigue life.

## Connector hook

Use `digitalmodel.subsea.connectors.assess_jumper_fatigue` with a dynamic
catalogue entry, a `PipeSection`, signed nonzero `radius_history_m`, explicit
Young's modulus in Pa, and the assessment settings. Radius sign defines bending
direction at one fixed hot spot. Positive-only radii do not imply stress reversal.
The supplied SCF scales the stress history before native rainflow counting.

This is a homogeneous elastic section approximation. It does not establish
layer stresses or qualify a bonded/unbonded flexible pipe; supply a qualified
stress history directly when that approximation is not applicable. Displacement
histories require a structural transfer model and are not converted automatically.

## Calculation basis and limitations

Miner damage and duration scaling reuse `digitalmodel.fatigue.damage`; counting
reuses `digitalmodel.fatigue.rainflow`; bending stress reuses
`digitalmodel.subsea.connectors.bending_analysis`. No S-N table is copied.

- DNV-RP-C203, *Fatigue design of offshore steel structures*, 2021,
  *S-N curves in air* and *S-N curves in seawater with cathodic protection*.
  The library's values were checked against 2011; agreement with 2021 is not
  established by this wrapper.
- ASTM E1049-85, *Standard Practices for Cycle Counting in Fatigue Analysis*,
  *Rainflow Counting*.

The method does not establish mean-stress, thickness, multiaxial, crack-growth,
or geometry-derived hot-spot corrections. An SCF requires an independent basis;
surface-stress extrapolation remains available in `digitalmodel.fatigue.hotspot_stress`.
If design utilisation exceeds one, revise the component or loading and repeat.
Confirm the load-block duration, curve classification and corrections before use.
