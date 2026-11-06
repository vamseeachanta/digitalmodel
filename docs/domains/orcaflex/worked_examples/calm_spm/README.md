# CALM buoy and single point mooring worked example

## Summary

This offline workflow prepares a generic buoy and tanker with six anchor legs,
a hawser and a transfer hose. It generates a case table, validates native YAML
structure, checks imported whole-line extrema against supplied limits and builds
an analysis in the repository's standard report form. Engineering model generation
requires supplied hose properties and a source record for each field; the
engineering input deliberately leaves these unresolved. The committed report
uses a separate synthetic arithmetic test fixture. Native solver loadability,
static equilibrium, dynamic response and engineering acceptance are
`NOT_EVALUATED`. No licensed solver or binding is used.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 00 | 2026-10-20 | Added reproducible case preparation, arithmetic example and report; native response evidence remains pending. |
| 01 | 2026-10-25 | Made hose values and field-level sources required before model generation; separated synthetic arithmetic inputs from engineering inputs. |

## Inputs and sources

All paths below are relative to `docs/domains/orcaflex/`.

| Input | Basis |
| --- | --- |
| Buoy, six anchor legs, chain and hawser types, water depth and analysis stages | `templates/mooring_systems/calm_buoy_hybrid/base/calm_buoy_base.yml` and its component includes. Each file is recorded with a SHA-256 digest. |
| Generic tanker type and vessel settings | `templates/mooring_systems/calm_buoy/includes/04_vessel_types.yml` and `06_vessels.yml`. These contain illustrative RAOs and exclude the tanker from statics. |
| Regular wave height 3.5 m, period 12.0 s, surface current 1.0 m/s and wind 12.0 m/s | The hybrid base's Environment section. Its Dean stream wave uses regular height, not significant wave height. The existing depth-varying current factors are retained. |
| Eight headings, in 45.0 degree steps | Independent case construction: divide a full turn into eight equal sectors. This is a sensitivity selection, with no directional probability or return period. |
| Three conditions | A constructed zero-load reference, aligned loads with the generic magnitudes, and the same magnitudes with constructed wind/current offsets of plus/minus 30.0 degrees. |
| Tanker position and connection geometry | Constructed geometry in `input.yml`. The selected tanker centre and connection give a horizontal separation of 150.0 − 50.0 = 100.0 m. |
| Hose dimensions, mass, stiffnesses and coefficients | Unresolved required inputs in `input.yml`. Every numerical field in `coupling.hose_line_type` requires its corresponding field-level reference in `coupling.hose_property_sources`. No numerical defaults are provided. |
| Acceptance criteria | `input.yml` supplies none. `arithmetic_criteria.yml` supplies constructed limits solely for the arithmetic example. No code factor, allowable tension or manufacturer MBR is assumed. |

The reviewed inventory also includes `library/model_library/` entries
`c05_single_point_mooring`, `c06_calm_buoy`, `c06_discretised_calm_buoy` and
`c07_metocean_buoy`; the `calm_buoy`, `calm_buoy_hybrid` and `salm_hybrid`
templates; `library/buoy_types/`; and `CALM_BUOY_LEARNINGS_AND_PATTERNS.md`.
The vendor-derived entries inform the modelling boundary only; their model data
are not copied. The SALM template has a different, single-anchor topology and is
not substituted for the six-leg system. No private comparator is used.

## Method

The workflow flattens generic object includes and adds explicit tanker coupling
and hose inputs. It retains native multicolumn line sections and records component
and generated-model digests. The combined matrix contains three conditions times
eight headings, giving 24 cases. Every generated case passes
`OrcaFlexYAMLValidator`; this is a structural check with its warnings retained in
the manifest, not a native load test.

For stored results, the scalar definitions are:

- Tension utilisation: Ut = maximum effective tension / allowable tension.
  Source: [OrcaFlex help, Line results: Forces, Normalised tension](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Lineresults,Forces.htm), public web edition accessed 2026-10-20.
- Curvature utilisation: Uk = maximum curvature × constant minimum bend radius.
  This is curvature divided by allowable curvature, where allowable curvature is
  the reciprocal of the supplied MBR. Source: [OrcaFlex help, Line results: Moments, Normalised curvature and Bend radius](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Lineresults,Moments.htm), same public edition and access date.
- Bend radius: R = 1 / curvature. At zero curvature the radius is infinite,
  represented as JSON null. The Moments source defines this convention and warns
  that mesh sensitivity is needed for curvature accuracy.

Unity is the mathematical boundary of these normalised ratios. Limits must be
positive, finite, explicitly sourced input values in kN for tension and metres
for MBR. Only constant MBR is supported; pressure or tension dependent limits
require a separate assessment. No environmental force equation is applied and no
hawser or anchor tension is predicted by the offline workflow.

## Required hose inputs

`input.yml` is the engineering input template. Supply all nine hose properties:
`OD`, `ID`, `MassPerUnitLength`, `EA`, `EI`, `GJ`, `PoissonRatio`, `Cd` and `Ca`.
For each field, supply a nonempty reference under `coupling.hose_property_sources`
that identifies the data document and its table, section or entry. Use traceable
public example data or qualified user-supplied data. A general coupling source
alone does not satisfy this requirement. Missing or null values and missing or
blank field-level references are rejected before any output directory is created.
The workflow checks numeric shape and finiteness; the supplied source's authority,
units and applicability still require engineering review.

After completing a copy of the engineering input, pass that file to the CLI in
place of the arithmetic fixture below. Omit `--responses` and `--criteria` to
leave response checks pending until actual exports and qualified criteria exist.
The unmodified engineering input is expected to fail; no test data are substituted.

## Run the arithmetic example

Run from the repository root with the package's dependencies available. Set
`CALM_OUTPUT` to an external output directory before running. No files need to be
written into the checkout.

```bash
PYTHONPATH=src uv run --no-project --with pyyaml --with loguru --with numpy \
  --with scipy --with pydantic --with jinja2 --with plotly \
  python -m digitalmodel.solvers.orcaflex.calm_workflow \
  tests/solvers/orcaflex/fixtures/calm_spm_arithmetic.yml \
  --repo-root . --output "$CALM_OUTPUT" \
  --responses docs/domains/orcaflex/worked_examples/calm_spm/hand_calculation.json \
  --criteria docs/domains/orcaflex/worked_examples/calm_spm/arithmetic_criteria.yml
```

`tests/solvers/orcaflex/fixtures/calm_spm_arithmetic.yml` contains synthetic hose
properties and explicit synthetic field-level source records solely to exercise
model serialisation and scalar arithmetic. It has no physical or manufacturer
basis and must not be used as a qualified engineering input. Its model digests
bind the constructed response fixture only; they are not solver evidence.

The outputs are `models/case-001.yml` through `models/case-024.yml`,
`manifest.json` and `report.html`. Identical model files are reused on a repeated
run; a different existing model is refused. The committed `report.html` and
`manifest.json` were regenerated using this arithmetic fixture. Generated native
models are kept outside the repository; their digests are in the manifest.

Required engineering hose properties, their field-level references and criteria
must be established independently before supplying actual solver exports.

## Independent answer

`hand_calculation.json` is a constructed record for the first, still-water case,
provided only to exercise reporting. Its nonzero extrema do not represent the
physical response to that case. It gives a maximum effective tension of 60.0 kN
for each anchor leg and the hawser, and a maximum hose curvature of 0.2 per metre.
The arithmetic limits are 100.0 kN and an MBR of 4.0 m.

| Quantity | Independent hand calculation | Answer |
| --- | --- | --- |
| Hawser and each anchor-leg utilisation | 60.0 / 100.0 | 0.6 |
| Hose curvature utilisation | 0.2 / (1.0 / 4.0) | 0.8 |
| Minimum hose bend radius | 1.0 / 0.2 | 5.0 m |
| Failure test, in the unit tests | 120.0 / 100.0 | 1.2 |

The eight populated checks are `ILLUSTRATIVE`. The remaining 184 checks are
`NOT_EVALUATED`; the overall engineering assessment remains `NOT_EVALUATED`.
The expected values are written out independently in the tests and are not
computed by calling the workflow under test.

## Stored-result contract

The response file contains a JSON list. Each record has `case_id`, the exact
`model_sha256` from preparation, `evidence_kind` (`solver_export` or
`hand_calculation`), `solver_version`, `source`, `period_s` and `extrema`.
A real export requires a nonempty recorded version; all real records must use
one version. Hand calculations require a null solver version and can never pass
an engineering check.

The period must cover the complete analysis stage, excluding build-up. Each
object entry in `extrema` supplies `quantity`, `unit`, `max` and
`coverage: whole_line`. Six anchor legs and the hawser require
`effective_tension` in `kN`; the hose requires `curvature` in `1/m`.
An endpoint-only tension cannot establish a whole-line maximum and is rejected.
The exporter must establish the coverage assertion before supplying it.

Unknown objects, duplicate records, model mismatches, wrong periods/units,
nonfinite or negative extrema, and mixed solver versions are rejected. Missing
objects or criteria become `NOT_EVALUATED`. The full supplied response collection
is digested in the assessment. Scalar comparison success is separate from native
model qualification, which remains `NOT_EVALUATED`.

## Validation and way forward

Tests cover independent arithmetic, threshold equality, straight-hose behaviour,
missing evidence, rejected inputs, deterministic relocation, model preservation,
all-case YAML validation, unresolved hose values and sources, rejection before
output creation, fixture separation, the CLI and the standard report form.
The input source records and public source locators are in `sources.json`.

The inherited generic buoy and tanker data require engineering review. In
particular, tanker hydrodynamic and environmental load coefficients, inclusion in
statics, line units, hose properties, pretension, equilibrium and realistic wave
conditions remain unqualified. The engineering hose input fields remain unresolved.
The separate arithmetic fixture supplies synthetic values solely for testing and
is not a physical design.

Later work requires reviewed, qualified inputs and separately authorized native
runs on one recorded solver version. Export hawser and anchor-leg tension maxima
and full-hose curvature over the complete analysis period, then establish time,
mesh and stochastic-seed sensitivity. Review stationkeeping, anchor holding
capacity, hose contact, fatigue and tension-dependent bend criteria separately.
No solver execution or engineering acceptance is claimed here.
