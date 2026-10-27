# Synthetic pipeline inspection-planning example

## Summary

The [executable example](../../../examples/asset_integrity/example_pipeline_inspection.py)
calls `corrosion_rate_mm_yr` and `plan_inspection` using synthetic thickness
readings and an explicitly supplied interval cap. This arithmetic example is a
narrow integrity-management aid. It does not establish DNV-RP-F116 compliance
or a qualified inspection interval.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-27 | Added synthetic arithmetic and an exhausted-wall control to explain the existing planner's limits. |

## Input basis

Basis S1 is an invented dataset for demonstrating arithmetic. Basis S2 changes
only the current wall to exercise the below-required-wall control. Every value
below is an assumed synthetic input; no field measurements or standard-derived
limits are used. The simulated thickness readings therefore carry `measured:
false` and `assumed: true`, as do the other inputs. Each saved input also states
its unit and basis.

| Input | S1 | S2 | Basis |
| --- | --- | --- | --- |
| Previous wall | 12.000 mm | 12.000 mm | Synthetic simulated reading |
| Current wall | 11.000 mm | 7.000 mm | Synthetic simulated reading |
| Elapsed time | 4.0 yr | 4.0 yr | Synthetic measurement separation |
| Required wall | 8.000 mm | 8.000 mm | Assumed illustration threshold; no design calculation |
| Supplied maximum interval | 3.0 yr | 3.0 yr | Arbitrary illustration cap; no standard basis |

Table 1. Synthetic inputs, defined here and in the executable example's input basis.

## Independent hand calculation

From S1, using only the inputs in Table 1:

```text
rate = (previous - current) / elapsed
     = (12.000 - 11.000) / 4.0 = 0.25 mm/yr
remaining thickness life = (current - required) / rate
                         = (11.000 - 8.000) / 0.25 = 12.0 yr
```

The [existing planner](../../../src/digitalmodel/asset_integrity/inspection_planning.py)
uses `min(remaining_life / 2, supplied cap)`. The divisor is the existing
half-life convention, not a DNV-RP-F116 rule established by this example.
For S1, half-life is `12.0 / 2 = 6.0 yr`, so the supplied `3.0 yr` cap governs
and the arithmetic interval is `3.0 yr`. The function's parameter is named
`code_max_interval_years`; its name does not establish a standard basis for the
value passed here. The returned `standard` label identifies the existing
API 510 / 570 / 653 planner, without qualifying its application to a pipeline.

For S2, the rate is `(12.000 - 7.000) / 4.0 = 1.25 mm/yr`, but the wall margin
is `7.000 - 8.000 = -1.000 mm`. The existing planner returns failed screening,
zero remaining life and zero interval. No positive remaining-life claim or
deferred inspection is justified. Immediate qualified assessment is needed
to determine repair, re-rating or retirement.

## Run and check offline

From the repository root, with Python and pytest already available:

```bash
PYTHONPATH=src python examples/asset_integrity/example_pipeline_inspection.py
PYTHONPATH=src python -m pytest tests/asset_integrity/test_pipeline_inspection_example.py -q -rA -p no:cacheprovider
```

The console output is JSON with the scope limitation, planner convention,
flagged inputs, plans and way forward for both cases. Execution uses the standard
library and the local inspection planner, without a solver or network request.
`test_pipeline_inspection_example_hand_life` independently derives the rate and
life using rational arithmetic from S1. An alternate synthetic cap of `9.0 yr`
checks that the half-life of `6.0 yr` governs when the cap is larger.

## Limits and remaining work

The life estimate assumes constant linear metal loss and comparable thickness
readings. It excludes measurement uncertainty, spatial coverage, localized
defects, other damage mechanisms, strength assessment and changing conditions.
The assumed required wall has no demonstrated design basis. A planner result of
`pass` indicates its arithmetic screening only.

This is partial scope for issue #244. A qualified pipeline integrity-management
application still needs the applicable standard and edition reviewed, traceable
design and inspection evidence, threat identification, risk and consequence
assessment, inspection methods and coverage, acceptance criteria, response
actions and a reviewed interval-selection procedure. No standard-derived
interval is entered or endorsed here.
