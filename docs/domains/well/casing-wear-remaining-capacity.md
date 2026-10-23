# Casing wear remaining-capacity analysis

The experimental `casing-wear-remaining-capacity` workflow retains inspection
measurements and well load cases, performs descriptive bookkeeping, and returns
`NOT_EVALUATED` for every remaining-capacity check. No allowable operating
pressure, remaining collapse capacity, burst capacity or fitness-for-service
acceptance is established. The workflow requires no licensed solver.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-20 | Inspection intake, explicit capacity evidence gaps and durable reports are added because damaged-section applicability has not been qualified. |

Table 1. Documentation revision history.

## Scope and method

Slice 1 validates ultrasonic wall or caliper internal-radius samples and retains
their depth and azimuth, observation timestamp and source reference. A nonempty
inspection source reference is required; it does not establish calibration,
coverage or intended-use readiness. All dimensions are millimetres, depth is in
metres, azimuth is in degrees and pressure is in MPa. Axial force is in kN,
tension-positive; temperature is in degrees Celsius. Measured ovality is retained
as a dimensionless input; its measurement definition remains unqualified.

For ultrasonic measurements only, the descriptive minimum is `min(samples)` and
the lower wall bound is `min(samples) - measurement_uncertainty_mm`. This is the
definition of a deterministic absolute-error bound, not a standard equation or a
probabilistic confidence limit. The uncertainty is required and has no default.
A zero or negative bound is retained with a warning that positive wall is not
established. Neither quantity is a qualified equivalent uniform wall. There is
no interpolation, future corrosion projection or implicit manufacturing-tolerance
deduction. Caliper radius is never converted to wall without independent
outer-surface geometry; the workflow leaves that conversion unevaluated.

Slice 2 retains uniform-corrosion or wear-groove classification, material yield
strength, measured ovality, temperature, axial force and explicit design factors
for each load case. Each pressure pair must use the same pressure reference.
Bookkeeping uses the definitions `burst differential = max(p_internal -
p_external, 0)` and `collapse differential = max(p_external - p_internal, 0)`.
These definitions do not determine any capacity. Factors must be positive inputs;
no factor is supplied or qualified by the implementation.

The following checks return a null value, `NOT_EVALUATED`, missing evidence and
source references for every load case:

| Check | Evidence needed before evaluation |
| --- | --- |
| Wall equivalence | A validated damaged-section conversion, including log coverage, calibration and uncertainty. |
| Collapse | Damaged-wall applicability, D/t regime, ovality correction, axial and temperature methods, and validation data. |
| Burst | Damaged-section internal-yield or rupture method, wall basis and combined-load applicability. |
| Combined loads | Temperature-dependent properties and an axial-pressure interaction criterion. |
| Design factors | Qualified remaining burst and collapse capacities to compare with supplied factors. |

Table 2. Capacity checks and qualification gaps.

Slice 3 provides a standalone command, registry entry, deterministic JSON result
and escaped HTML analysis. The JSON preserves every supplied input and source
reference, its canonical SHA-256 digest and the validation status. JSON and HTML
are read back after writing. A fresh output directory is required, so existing
results cannot be overwritten. The report is an evidence-gap analysis, not an
issued engineering capacity assessment. Its revision history identifies the
template revision; the inspection timestamp is recorded separately.

## Worked example

The following synthetic inputs demonstrate intake and arithmetic only. Their
material properties, loads, factors and uncertainty are illustrative required
inputs, not published values or acceptance criteria. Save the JSON outside the
repository in an approved scratch directory as `inspection.json`.

```json
{
  "source_ref": "synthetic-inspection",
  "observed_at": "2026-10-01T12:00:00Z",
  "log_kind": "ultrasonic_wall",
  "damage": "uniform_corrosion",
  "od_mm": 200.0,
  "nominal_wall_mm": 10.0,
  "yield_strength_mpa": 500.0,
  "measurement_uncertainty_mm": 0.2,
  "measured_ovality": 0.01,
  "samples": [
    {"depth_m": 100.0, "azimuth_deg": 0.0, "value_mm": 8.0},
    {"depth_m": 100.0, "azimuth_deg": 90.0, "value_mm": 7.0}
  ],
  "load_cases": [
    {"name": "injection", "internal_pressure_mpa": 20.0,
     "external_pressure_mpa": 5.0, "axial_force_kn": 100.0,
     "temperature_c": 80.0, "burst_design_factor": 1.2,
     "collapse_design_factor": 1.3}
  ]
}
```

From the repository root, using the new scratch output directory `assessment`:

```bash
PYTHONPATH=src python -m digitalmodel.well.tubulars.casing_wear_report \
  "$TMPDIR/inspection.json" --output-directory "$TMPDIR/assessment"
```

The independent hand calculation is:

- Minimum observed wall = min(8.000, 7.000) = **7.000 mm**.
- Lower wall bound = 7.000 - 0.200 = **6.800 mm**.
- Burst differential = max(20.0 - 5.0, 0) = **15.0 MPa**.
- Collapse differential = max(5.0 - 20.0, 0) = **0.0 MPa**.
- Equivalent thickness, remaining capacities and design-factor verdicts =
  **NOT_EVALUATED**, with null numeric results.

The files `assessment.json` and `analysis.html` contain the result. Successful
command completion means the input was processed and files passed readback;
it does not mean a casing load case meets its design factors.

## Sources and validation record

The opened public paper *Elastoplastic numerical evaluation on the collapse
pressure of a worn casing tube*, Revista de Engenharia e Tecnologia 11(3), 2019,
[abstract and sections 2.1, 2.2 and 5](https://revistas.uepg.br/index.php/ret/article/download/13194/209209211470/209209220074),
distinguishes nominal-pipe formulas from worn-section methods and describes
validation of its unworn numerical model. It does not qualify this workflow as
a prediction of damaged-casing strength. The source was checked on 2026-10-20.

API TR 5C3, *Calculating Performance Properties of Pipe Used as Casing or Tubing*,
first edition (2008, as cited by the public paper), and ISO/TR 10400, *Equations
and calculations for the properties of casing, tubing, drill pipe and line pipe
used as casing or tubing*, are reference targets only. ISO edition and applicable
damaged-section clauses require confirmation. No original standard was qualified
for calculation in this implementation; no standard coefficients, limits or
example properties are transcribed. Existing nominal functions in
`digitalmodel.well.tubulars.casing` are not called.

The automated tests validate input domains, independent bookkeeping calculations,
uniform-corrosion and wear-groove gap flags, caliper limitations, source retention,
deterministic hashing, HTML escaping and CLI output readback. They are software
tests, not physical validation. The API published-example golden tests requested
in [the issue](https://github.com/vamseeachanta/digitalmodel/issues/2201) remain
**NOT_EVALUATED**. No qualified damaged-casing capacity result is produced.

## Later slices

An applicable damaged-casing method and independent benchmark will be needed
before implementing equivalent-wall conversion, regime-selected collapse,
ovality and axial/temperature corrections, burst/rupture or design-factor
acceptance. A qualified validation record will need the governing edition,
method limits, measurement basis, expected values and errors. Until those are
available, this workflow will continue to return explicit evidence gaps.
