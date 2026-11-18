# Synthetic pipeline fitness-for-service assessment

The finished example is [the HTML assessment](../../../docs/reports/pipeline-ffs-assessment.html).
Open Sections 1, 3.5 and 7 first. All inputs are synthetic; the document does not
qualify an operating asset.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 00 | 2026-10-21 | Documented the supported pressure bracket, outcome definitions and independent worked inversion; existing workflow guidance is retained. |
| 01 | 2026-10-21 | Excluded zero-wall endpoints from numerical life, limited qualification claims to the checked shell-parameter range and revised the worked pressure demand. |

From the root, with the package source on `PYTHONPATH` and the reporting and
assessment dependencies installed in an isolated environment:

```sh
python scripts/reporting/build_pipeline_ffs_report.py
python -m pytest tests/asset_integrity/test_pipeline_ffs_assessment.py
```

The builder uses the two existing inputs referenced by `input.yml`. Their SHA-256
pins normalise CRLF to LF without rewriting the originals. Supplementary values
needed by the six-method comparison are explicit assumptions; no grids are
invented or adjusted. `assumptions.yml` records genuine assessment assumptions
and method citations. Licensed factors, limits and equation constants are not
reproduced in this workflow's own inputs, report and manifest. The existing
referenced Section A source retains its numerical factors. The builder obtains
the Section B factors from named numerical-method defaults at calculation time.

Both grids are bounded local patches; general-loss averaging is not applied.
Section A requires formal local-loss evaluation because its remaining strength
factor is below the allowable factor; its field data is adequate. The current
reduced rating exceeds demand. Section B requires added readings to resolve at
least three more degraded cells: two degraded cells resolve its flaw against an
assessment default of five.
Interim operation is a decision for the maintainer of the pipeline record.

Applicability flags and severe loss below the replacement floor require
escalation regardless of sampling. Otherwise sampling adequacy is checked first;
the strength-factor referral is evaluated only after sampling meets the
Section 3.5 criterion.
Acceptance or monitoring gives accept only with positive remaining life.
Pressure reduction uses pressure ratios only; the axial limit remains
a separate restriction. Section A life is projected to design pressure on the
reduced local-loss route, holding the flaw extent, background wall and loads
fixed. Section B retains the linear pressure-wall projection. Neither route
predicts all future defect-capacity crossings or authorizes operation.

The local-loss calculation calls the existing relation with the wall away from
the flaw as its reference. The constructor argument named `t_min_in` supplies
that reference internally; the actual required pressure wall is kept separate.
The uncorroded pressure rating is obtained by inverting the existing linear
B31.8 pressure-wall relation, then `reduced_mawp` applies the strength reduction.
The reduced rating participates in the decision. This correction does not
establish complete edition-matched qualification of the simplified relation.
The life-limit search checks **lambda-range applicability only**, establishes a
passing/failing pressure bracket before bisection and retains its evidence.
Remaining-thickness ratio, minimum remaining wall and the other Part 5
prerequisites are not checked; full Part 5 applicability is not established.
[The qualification requirement](https://github.com/vamseeachanta/digitalmodel/issues/2812)
is covered by the worked inversion and regression cases below. The two synthetic
assessment results are unchanged.

The HTML contains inline vector figures and requires no network. Its companion
manifest retains full-precision results and source/content digests, excluding
method factors and quoted thresholds. Diagnostic pressure-wall and strength-factor
comparisons are labelled separately from the reduced-rating pressure criterion. Text outputs
are UTF-8 with LF. The fixed document date is not read from the clock; repeat
builds are intended to be byte-identical. The document-control block is marked
synthetic example through document metadata and names no person or unperformed
check or approval. Other assessments receive no implicit synthetic label or
signature placeholders. Assessment presentation does receive a default one-row
revision history labelled "Issued" when no explicit history is supplied.
Provenance remains mandatory and is retained in the manifest. The generated-page
registry owns both outputs and names their synthetic input dependencies.

Numerical behavior, original inputs, measured-data fixtures and other workflows
are unchanged. The pressure-wall documentation identifies Location Class 1,
Division 2. No external engineering program is used.

## Inputs and worked answer

All values in this example are synthetic inputs, rather than code acceptance
limits. Units are inches and psi. The allowable strength factor is deliberately
set to unity for the numerical demonstration; it is not a standard default.

```python
from types import SimpleNamespace

import pandas as pd

from scripts.reporting.build_pipeline_ffs_report import local_loss_limit_wall

frame = pd.DataFrame([[1.0, 0.6], [1.0, 0.6]], index=[0.0, 1.0])
component = SimpleNamespace(
    nominal_od_in=12.0, nominal_wt_in=1.0,
    rsf_a=1.0, design_pressure_psi=800.0,
)
result = local_loss_limit_wall(frame, component, 1.0, 1000.0)
assert result["status"] == "CROSSING"
print(f"Life-limit wall: {result['wall_in']:.3f} in")
print(f"Life at assumed loss rate 0.010 in/yr: {(0.6-result['wall_in'])/0.010:.1f} yr")
```

The reference wall is 1.000 in, the inside diameter is 10.0 in, and the
two degraded rows give a flaw length of 2.0 in. The existing simplified bulging
relation gives a factor of 1.148, rounded here for display. Full precision is
used in the calculation. With `r = demand × allowable_factor / uncorroded_rating`,
the target strength ratio is 0.8. Independent rearrangement of the existing
relation gives:

```text
RSF = Rt / (1 - (1 - Rt) / M)
Rt_at_target = r * (M - 1) / (M - r)
wall_at_target = reference_wall * Rt_at_target
life = (current_minimum_wall - wall_at_target) / loss_rate
```

The life-limit wall is **0.340 in** and life is **26.0 yr** at the assumed rate
of **0.010 in/yr**. The regression test writes out the independent hand
calculation, including the source of the legacy bulging coefficients, rather
than deriving its expected wall from the search. Fixed geometry, background
wall, material and pressure rating are assumed; lateral flaw growth and other
failure modes are outside this projection.

## Supported domain and evidence

The engine determines the **lambda-range** domain from its existing
shell-parameter check, including its full-reference-wall exception and its
degraded-row definition. Geometry is checked before any projected strength is
evaluated. This interval does not check the remaining-thickness ratio or minimum
remaining wall and does not establish complete Part 5 applicability. The lower
zero-wall endpoint is mathematical evidence with `applicability.ok: false`; it
does not qualify a perforated wall. In the short-flaw branch with unit bulging
factor, only the right-hand pressure limit is recorded. If no crossing occurs
before that unqualified endpoint, life is not established and no numerical wall
limit is returned. Other zero-wall evaluations are labelled `evaluated`.

For fixed geometry, the derivative of the strength ratio with respect to `Rt`
is `M * (M - 1) / (M - 1 + Rt)**2`, which is nonnegative for `M >= 1`.
Increasing wall can only remove degraded rows, increasing the rating as well.
Endpoint ordering is checked before searching and every midpoint pressure is
checked against the retained pressure interval. One pressure floating-point
step is allowed for rounding. A change in flaw extent across the final adjacent
walls is reported as a bound, since a discontinuity does not establish a root.

| Outcome | Meaning and life treatment |
| --- | --- |
| `CROSSING` | Positive-wall failing and passing pressures within the checked lambda range bracket a continuous crossing; the passing wall is retained. |
| `BOUNDARY` | The target reaches a positive supported endpoint; the projection is explicitly censored there. |
| `BOUNDED` | No failing pressure exists before a positive supported boundary, or a flaw-extent jump brackets the target; only a projection bound is established. |
| `TARGET_UNREACHABLE` | Current rating is already below demand; no future depletion crossing exists and remaining pressure life is zero. |
| `INAPPLICABLE` | No lambda-range depletion interval exists, or no crossing occurs before an unqualified zero-wall endpoint; no numerical life limit is returned. |
| `NON_MONOTONIC` | Evaluated pressures violate the required ordering; no numerical life limit is returned. |

`initial_bracket` and `final_bracket` retain wall, pressure, flaw length and
lambda-range applicability for both endpoints, together with the target and
supported domain. `qualification_scope` explicitly records `lambda-range only`;
the unqualified zero-wall endpoint is retained only as mathematical evidence.
For bounded outcomes these records are domain evidence, not a claim of a
passing/failing pressure crossing. The report builder identifies censored life
bases and refuses to issue a numerical life when no limit is established.

## Rebuild and checks

Run with the package source on `PYTHONPATH`:

```bash
PYTHONPATH=src uv run python scripts/reporting/build_pipeline_ffs_report.py
PYTHONPATH=src uv run python -m pytest tests/asset_integrity/test_local_loss_life.py tests/asset_integrity/test_pipeline_ffs_assessment.py tests/asset_integrity/test_ffs_assessment.py tests/asset_integrity/test_ffs_published_examples.py
```

The builder regenerates the published report and manifest and their retained
copies. Report revision 03 records the qualification limits. The synthetic
Section A life remains 11.5 yr; Section B life remains 25.9 yr. Additional
engineering qualification is required before applying either projection to an
operating line.

## Sources and limits

- API 579-1/ASME FFS-1, *Fitness-For-Service*, 2021, Part 5, *Assessment of Local
  Metal Loss*: the existing simplified strength implementation is documented in
  [level2_engine.py](../../../src/digitalmodel/asset_integrity/assessment/level2_engine.py).
  The hand solution reuses `Level2Engine._folias_factor`; its simplified bulging
  coefficients are attributed to that implementation, without an edition claim.
  This change does not establish a complete, edition-qualified assessment to the
  standard.
- API 579-1/ASME FFS-1, 2021, Part 2, *Fitness-For-Service Engineering Assessment
  Procedure*: the existing reduced-rating helper is reused without duplicating
  engineering coefficients.
- NIST, *Handbook of Mathematical Functions*, 2010, §3.8(iii),
  [Bisection Method](https://dlmf.nist.gov/3.8#iii): opposite-sign endpoint
  residuals establish the pressure bracket. The algebraic inverse and derivative
  above are independent rearrangements of the existing relation.
- [Applicability-bracket requirement](https://github.com/vamseeachanta/digitalmodel/issues/2812)
  and [two-section worked assessment](https://github.com/vamseeachanta/digitalmodel/pull/2467).
