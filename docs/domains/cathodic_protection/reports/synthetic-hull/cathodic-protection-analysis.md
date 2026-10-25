# Synthetic hull cathodic protection analysis

## Summary

The 48-anode trial does not meet the final-current criterion. Final current governs the aggregate requirement of 50 anodes. The 50-anode re-sized arrangement meets the calculated aggregate comparisons on the assumed basis.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-25 | Initial synthetic case for reproducible hull screening. |

## Inputs and basis

All case inputs are ASSUMED, based on previous experience. No actual hull, site or supplier is represented. The [input file](../../../../../tests/fixtures/cathodic_protection/workflow_inputs/synthetic_hull.json) maps every input to its unit and assumption basis. No public hull source is claimed.

The rectangular submerged shell includes the bottom, both sides and both ends. It excludes the waterplane and appendages. Coated and bare areas are counted separately. The ideal anode net mass deducts its through core. Geometry is an idealisation, not a product drawing.

## Method and criteria

The existing `ABS_gn_ships_2018` key selects ABS Guidance Notes on Cathodic Protection of Ships, December 2017. The key date is historical. Clause subjects: Current demand; Anode current output; Anode resistance; Minimum net anode mass; Final dimensions; Hull anode distribution and spacing. The implementation is in `src/digitalmodel/cathodic_protection/abs_ships.py` and its table, layout and result modules.

Initial and final demand use the dynamic bare-steel density. Mean demand weights dynamic and static exposure, with the arithmetic mean of the initial and final coating fractions. Long-flush final resistance uses depleted length plus equivalent width. These are existing route interpretations, not a new assessment of the standard. Mean-current mass uses a non-leap design year. Counts round upward; the maximum mass, initial, mean, final and layout requirement governs.

The protective and anode potentials and alloy capacity are assumed performance inputs constrained by existing route lookups. The public citation fixtures check metadata only; no standards document or private material was opened. Source qualification and applicability remain open.

## Results

The [results file](cathodic-protection-analysis.citations.json) retains unrounded calculation evidence. Its `arrangements`, `checks`, `zone_demand_A` and evidence keys follow the reference-jacket sidecar. No sensitivity sweep or reporting-standard assessment is supplied.

| Arrangement | Anodes | Check | Required | Provided | Unit | Margin (%) | Result |
|---|---:|---|---:|---:|---|---:|---|
| stated | 48 | Mean-current lifetime mass | 565.1 | 2332.8 | kg | +312.8 | PASS |
| stated | 48 | Initial current output | 47.6 | 66.8 | A | +40.4 | PASS |
| stated | 48 | Mean current output | 27.4 | 60.4 | A | +120.2 | PASS |
| stated | 48 | Final current output | 62.1 | 60.4 | A | -2.8 | FAIL |
| rechecked | 50 | Mean-current lifetime mass | 565.1 | 2430.0 | kg | +330.0 | PASS |
| rechecked | 50 | Initial current output | 47.6 | 69.6 | A | +46.3 | PASS |
| rechecked | 50 | Mean current output | 27.4 | 62.9 | A | +129.4 | PASS |
| rechecked | 50 | Final current output | 62.1 | 62.9 | A | +1.3 | PASS |

## Distribution and limits

General spacing and layout flags pass on their ASSUMED declarations. Port and starboard pairing is retained during re-sizing. No coordinate schedule demonstrates those declarations. Deepest bottom and remote bow and stern positions require assessment. Special stern, propeller and opening geometry and local potentials are NOT_EVALUATED. Aggregate PASS does not establish protection at any position. Only external sacrificial-anode screening is covered.

## Recommendations and monitoring plan

At design, add 2 anodes (97.2 kg net alloy) for the re-sized aggregate arrangement. Confirm coating, environment, qualified anode performance and placement before acceptance. If monitoring or later retrofit cannot be supported, resolve these uncertainties in the initial design.

In service, measure the deepest bottom and remote bow and stern positions first, then appendages and shielded areas. Establish a qualified protective criterion and reference electrode, baseline survey and survey interval. Less negative readings need assessment against that criterion. Frequency, trigger potential, retrofit quantity and intervention date are NOT_EVALUATED: operating exposure, depletion, layout, survey and access dates are missing. The design increment is not a remaining-life retrofit prediction. An engineer must establish the monitoring and intervention plan.

## Reproduction and checks

```sh
PYTHONPATH=src uv run --no-project --with pydantic --with loguru --with scipy python scripts/reporting/build_synthetic_hull_analysis.py
```

The builder uses only the public input and citation-metadata fixtures. Tests independently calculate shell area, weighted demand, net anode mass, lifetime mass, initial and depleted output and the governing count. Reproduction checks compare all evidence and report bytes. This synthetic example has no marketing clearance record; outward use remains subject to the maintainers' review.
