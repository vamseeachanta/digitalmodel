# Pipeline inspection-planning worked example

## Summary

The [executable example](../../../examples/asset_integrity/example_pipeline_inspection.py)
reproduces publicly sourced thickness arithmetic with `corrosion_rate_mm_yr`
and `plan_inspection`. This narrow integrity-management aid does not establish
DNV-RP-F116 compliance or a qualified inspection interval.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-27 | Added synthetic arithmetic and an exhausted-wall control to explain the existing planner's limits. |
| 1 | 2026-10-27 | Replaced invented inputs with a cited public example and a derived boundary control; clarified interval provenance and screening criteria. |

## Input basis

P1 is [How often should ultrasonic thickness testing be done?](https://highpointsolutions.com.au/knowledge/how-often-should-ultrasonic-thickness-testing-be-done/),
published 9 September 2026, section "A worked example", checked 27 October 2026.
The following values reproduce that public article's example; they are not
independently verified equipment measurements. Saved inputs therefore retain
`measured: false` and `assumed: true`, with a source URL, section, unit and basis.
No numerical value is extracted from a licensed publication.

| Input | Public example | Boundary control | Basis in P1 |
| --- | --- | --- | --- |
| Previous wall (mm) | 7.300 | 7.300 | 2023 reading |
| Current wall (mm) | 6.400 | 4.500 | 2026 reading; control reuses the required wall |
| Elapsed time (yr) | 3.0 | 3.0 | 2026 minus 2023 |
| Required wall (mm) | 4.500 | 4.500 | Published assessment threshold; underlying design calculation unavailable |
| Supplied ceiling (yr) | 3.0 | 3.0 | Published suggested schedule; not a code maximum |

Table 1. Inputs sourced from P1, with the boundary transformation stated explicitly.

The control sets current wall equal to the published required wall. It is a
constructed zero-margin boundary, not another reading or a prediction.
The published long-term alternative of 3.8 yr is reused only as a counterfactual
ceiling to exercise the other interval branch. Neither ceiling is qualified for
a pipeline. The example accepts only these cited choices; missing, invalid,
non-finite or unsupported ceilings are rejected.

## Independent hand calculation

From Table 1, without using planner output:

```text
rate = (previous - current) / elapsed
     = (7.300 - 6.400) / 3.0 = 0.300 mm/yr
remaining thickness life = (current - required) / rate
                         = (6.400 - 4.500) / 0.300 = 19/3 yr, displayed as 6.333 yr
half-life = (19/3) / 2 = 19/6 yr, displayed as 3.167 yr
interval with published schedule = min(19/6, 3.0) = 3.000 yr
interval with counterfactual ceiling = min(19/6, 3.8), displayed as 3.167 yr
```

The equations and half-life factor are reproduced from P1's method and worked
example. Method references are API 570, *Piping Inspection Code: In-service
Inspection, Rating, Repair, and Alteration of Piping Systems*, 5th edition
(February 2024): 7.1.2 (corrosion rates), 7.2 (remaining life), and 6.3.3
(thickness measurement inspection). The publisher's
[2026 body of knowledge](https://www.api.org/-/media/Files/Certification/ICP/ICP-Certification-Programs/570/2026%20Files/Feburary%202026_570_BOK_final.pdf)
identifies these calculation clauses; its
[effectivity sheet](https://www.api.org/-/media/Files/Certification/ICP/ICP-Certification-Programs/570/2026%20Files/Feburary%202026_570_Publications%20EffectivitySheet_Final.pdf)
identifies the edition. No claim of a complete code assessment is made.

The [existing planner](../../../src/digitalmodel/asset_integrity/inspection_planning.py)
uses `min(remaining_life / 2, supplied cap)`. Its `code_max_interval_years`
parameter and returned API 510 / 570 / 653 label do not qualify this example's
ceiling or establish pipeline applicability. Computation precedes rounding;
reported results use three decimals and input precision is retained.

For the boundary control, the diagnostic rate is
`(7.300 - 4.500) / 3.0 = 14/15 mm/yr`, displayed as `0.933 mm/yr`.
The wall margin is `4.500 - 4.500 = 0.000 mm`. Positive-life screening requires
current wall greater than required wall; this control fails that criterion.
Remaining life and interval are both `0.000 yr`. Immediate qualified assessment
is needed to determine repair, re-rating or retirement.

## Run and check offline

From the repository root, with Python and pytest already available:

```bash
PYTHONPATH=src python examples/asset_integrity/example_pipeline_inspection.py
PYTHONPATH=src python -m pytest tests/asset_integrity/test_pipeline_inspection_example.py -q -rA -p no:cacheprovider
```

The JSON output includes source metadata, input provenance, governing criteria,
plans and the way forward. Execution uses the standard library and the local
planner without a solver or network request. Independent rational-arithmetic
tests cover both interval branches, the boundary control, input provenance,
unsupported ceilings and console limitations.

## Limits and remaining work

The estimate assumes constant linear metal loss and comparable readings. It
excludes measurement uncertainty, spatial coverage, localized defects, other
damage mechanisms, strength assessment and changing conditions. P1 supplies
the required wall without its design calculation, so equipment suitability is
not established. A planner `pass` means positive wall margin in this example,
judged against the displayed required wall; it does not select an interval.

This is partial scope for issue #244. A qualified pipeline integrity-management
application still needs the applicable standard and edition reviewed, traceable
design and inspection evidence, threat identification, risk and consequence
assessment, inspection methods and coverage, acceptance criteria, response
actions and a reviewed interval-selection procedure. No standard-derived
interval is entered or endorsed here.
