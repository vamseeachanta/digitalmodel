# FFS quantity and methodology semantics

Generic decisions distinguish the dimensionless margin, the operating quantity
being reduced, and the assessment methodology. Pressure and axial stress have
identical physical dimensions; units alone cannot identify an operating action.
Legacy pressure decisions retain their existing RSF and MAWP contract.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-20 | Separate quantity meaning from units and report method declarations to prevent pressure and standard mislabelling. |

## Decision contract

`decide(..., margin_label="capacity/demand", bands=...)` uses the supplied label
in the criterion and serialized `margin_label`. An explicit label requires
explicit `DecisionBands`: existing pressure severity defaults do not define a
capacity/demand policy. The margin must remain dimensionless and larger must
mean safer. The caller supplies the allowable and all policy bands; this API
does not select an engineering acceptance limit for a new method.

`scaled_limit(..., quantity="axial_stress", action="REDUCE AXIAL STRESS")` carries
operating meaning separately from the Pint dimension. `derated`, `derated_units`
and `derated_quantity` identify the reduced limit. Only `quantity="mawp"`
produces `rerated_mawp_psi`; `reduced_mawp` supplies that tag automatically.
An unspecified rating quantity is `limit`, with generic operating wording for
pressure-class policies. Class-specific tension and loading actions are retained.
Explicit actions override wording only for a DERATE verdict; applicability flags
still produce ESCALATE.

`rsf` and `rsf_a` aliases appear only when the effective label is `RSF`.
Other margin kinds serialize `margin` and `margin_allowable` without those
aliases. Legacy pressure calls keep `rerated_mawp_psi=None` when no rating was
computed. A non-MAWP rating omits the pressure field even when its unit is psi.
Existing calls that omit a label continue to use their class policy. Existing
workflow adapters retain responsibility for their own method and demand rules.

## Report contract

`ReportMethodology(name, qualification, references)` contains caller-declared
text. Both `FFSReport.generate_screening_html` and the metal-loss
`FFSReport.generate_html` accept it through the keyword-only `methodology`
argument. Text and references are escaped as HTML. A declaration records what
the caller says was used; it does not certify compliance or validate a method.
Omission prints `Methodology and code qualification: NOT_EVALUATED` rather than
claiming that API 579 criteria were applied.

Use `generate_screening_html` for a membrane screen, capacity/demand comparison,
or other method outside the existing Part 4/5 metal-loss tables.
`generate_html` remains the specialized metal-loss layout, including its existing
API 579 section headings and references; overriding its footer does not change
those tables or qualify another method.

## Worked synthetic example

The executable example is
[`quantity_semantics.py`](../../../examples/domains/asset_integrity/quantity_semantics.py).
It returns a JSON-friendly decision and an offline HTML report without writing
files. All scenario values are assumed, based on previous experience, and are
illustrative caller inputs rather than standard coefficients or limits.

| Input | Value | Basis |
| --- | --- | --- |
| Capacity | 60.0 MPa | Synthetic scenario definition |
| Current demand / initial limit | 80.0 MPa | Synthetic scenario definition |
| Allowable capacity/demand | 1.0 | Caller-selected comparison policy |
| Monitor band | 0.1 | Caller-selected policy |
| Derate floor | 0.0 | Caller-selected policy |
| Repair life band | 2.0 yr | Caller-selected policy |
| Remaining life | 5.0 yr | Assumed input, not a life calculation |

Independent hand calculation: capacity/demand = 60 / 80 = 0.75. The caller
selects the existing linear scaling helper: limit times margin divided by
allowable, bounded between zero and the original limit. Thus
80 MPa × 0.75 / 1 = 60 MPa. The margin is below the allowable and above the
supplied floor, so the decision is DERATE with action `REDUCE AXIAL STRESS`.
There are no RSF aliases or MAWP fields in that decision.

This is an algebra and serialization example, not a qualified membrane-stress
assessment. Combined loading, physical capacity derivation and edition-matched
code qualification are `NOT_EVALUATED`. An actual application must supply those
engineering checks and their sources; proportional scaling alone cannot
establish a permissible operating limit.

## Basis and verification

No engineering equation or acceptance limit is added. The existing implementation
basis is `assessment/ffs_decision.py::scaled_limit`; the MAWP-specific wrapper
already identifies API 579-1/ASME FFS-1 (2021), Section 2.4.2.2. That reference
applies to the pressure wrapper, not to this synthetic stress example. No value
from a licensed publication is reproduced here.

The hand calculation above supplies independent expected values in
`tests/asset_integrity/test_ffs_quantity_semantics.py`. Regression coverage checks
stress versus pressure identity, non-RSF serialization, required bands, policy
wording, legacy pressure fields, applicability escalation, escaped metadata,
unspecified qualification, and execution of the worked example.
