# Holtrop resistance reference qualification

The legacy fixture is now loaded by tests and by the diagnostic command. All
three enabled rows and every declared output receive `NOT_EVALUATED` for
engineering accuracy. The expected numbers have no traced primary worked
example; matching them would not establish accuracy. The resistance equations
and approximate targets remain unchanged pending source qualification.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-20 | Account for unqualified fixture targets and expose every resistance component so missing evidence cannot be interpreted as agreement. |

## Scope and source access

This implements three bounded parts of issue #2020:

1. Load every enabled fixture row, account for each expected key, and reject
   duplicate identifiers, unknown outputs and ambiguous enable flags.
2. Report the complete component sum, including appendage, bulb and transom
   terms, alongside the total returned by the existing engine.
3. Provide this reproducible example and tests for reference qualification and
   independent component accounting.

The input basis is **assumed, based on previous experience**; the fixture does
not establish measured hull data or a published Series 60 experiment.

The primary source sought was *An approximate power prediction method* (1982).
The [institutional publication page](https://www.marin.nl/en/publications/an-approximate-power-prediction-method)
returned HTTP 200 on 2026-10-20, but the download block required access. No paper
or equations were obtained. Two candidate institutional addresses for
*A statistical re-analysis of resistance and propulsion data* (1984) returned
HTTP 404. Neither paper has been used to select or correct numerical coefficients.

The diagnostic normalization uses the definitions in EN400, *Principles of Ship
Performance*, Summer 2020, Chapter 7, also cited by the repository's resistance
and friction-scaling modules:

- `qS = rho S V² / 2`, in newtons;
- `CT = RT / qS`, dimensionless;
- `PE = RT V`, in watts;
- `Fn = V / sqrt(g L)`, dimensionless.

No empirical acceptance limit is inferred from these definitions. The current
engine sums `RF (1+k1) + RAPP + RW + RB + RTR + RA`; the diagnostic exposes that
accounting without qualifying any regression coefficient.

## Worked example

From the repository root, with uv available:

```sh
PYTHONPATH=src uv run --no-project --with pyyaml python scripts/validation/reproduce_holtrop_discrepancy.py
```

Inputs are read from
`tests/fixtures/test_vectors/naval_architecture/holtrop_mennen.yaml`.
The two resistance rows use the following existing, unqualified input vectors;
all remaining inputs, including viscosity, gravity and density, are printed in
the JSON or retained in the fixture. The command does not read historical output
artifacts as reference data.

| Input | Fine form | Full form |
| --- | --- | --- |
| L, B, T (m) | 121.92, 16.26, 6.50 | 205.0, 32.0, 12.0 |
| CB, CM, CWP | 0.60, 0.977, 0.70 | 0.80, 0.995, 0.88 |
| CP, LCB (% L, existing engine convention) | 0.614, -2.02 | 0.804, -0.5 |
| Speed (m/s) | 7.72 | 7.72 |
| Forward draft (m) | 6.50 | 12.0 |
| Bulb area, transom area, bulb height, stern coefficient | 0, 0, 0, 0 | 0, 0, 0, 0 |
| Appendages | none | none |

The method computes the wetted area and each resistance component, then divides
by the same `qS`. The following are **current diagnostic predictions**, rounded
for display; they are not independent expected values or qualified performance.

| Output | Fine form | Full form |
| --- | --- | --- |
| Wetted area (m²) | 2429.2 | 9598.8 |
| Flat-plate friction RF (kN) | 116.9 | 433.3 |
| Wave resistance RW (kN) | 31.3 | 115.7 |
| Correlation resistance RA (kN) | 35.5 | 103.3 |
| RAPP, RB, RTR (kN), each | 0.0 | 0.0 |
| Total resistance RT (kN) | 195.6 | 772.9 |
| Effective power (kW) | 1510.0 | 5966.9 |
| CT × 1000 | 2.6 | 2.6 |
| Difference from unqualified fixture CT (%) | -31.5 | +50.6 |
| Engineering accuracy | NOT_EVALUATED | NOT_EVALUATED |

Percentage difference uses the fixture target as denominator. It is a measure
of disagreement with that target, not predictive error against an experiment.
At common `Fn = 0.20`, using length-specific speeds, the diagnostic gives CT ×
1000 of 2.5 and 3.2, a separation of 29.8 % relative to the fine form. That
comparison demonstrates that the code responds to geometry and operating
point; it establishes neither a required ordering nor a predictive accuracy
limit. Published applicability ranges have not been qualified.

## Independent arithmetic test

The component-accounting test supplies synthetic component values rather than
calling the regression to obtain its expected answer. With `rho=10`, `S=2`,
`V=3`, `RF=5`, `(1+k1)=2`, `RW=7`, `RAPP=11`, `RB=13`, `RTR=17` and `CA=0.1`,
`qS=90`, `RA=9`, and `RT=5×2+11+7+13+17+9=67`. Therefore `CT=67/90` and
`PE=67×3=201`. These are chosen arithmetic inputs, not empirical coefficients.
This checks normalization and inclusion of optional components without claiming
agreement with a published hull example.

## Qualification contract and limits

`assess_fixture` deliberately accepts only the explicit unverified reference
state. It returns a record for each enabled row and a check for every declared
output key, including the tolerance metadata and both form-factor band bounds.
The tolerance has no computed engineering value. A number that exactly matches
its target still returns `NOT_EVALUATED`. No target is silently accepted, removed
or marked disabled. The command includes fixture, method, diagnostic and command
source digests, allowing the calculation to be tied to the inspected bytes.

Coefficient corrections, a formulation-by-formulation source map, published
worked-example expectations and rounding-derived tolerances remain later work.
They require accessible primary equations and examples. The existing approximate
CT targets, form-factor band and tolerance must then be re-sourced rather than
used to tune the model. This change does not resolve those accuracy defects or
qualify the method for design, powering estimates or CFD benchmarking.
