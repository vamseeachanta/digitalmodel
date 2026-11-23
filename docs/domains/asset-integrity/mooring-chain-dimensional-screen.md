# Mooring-chain dimensional screen

Owner decision O71 authorizes methods supported by documents opened during the
job and explicit gaps for unsupported calculations. This module screens inspected
locations for dimensional loss; remaining breaking strength, fatigue life and
overall fitness for service are not established.

## Scope and inputs

`asset_integrity.mooring_chain_dimensions.screen_dimensions` accepts typed
inspection and reading records. The implemented subset covers mobile offshore
drilling unit moorings with R3 studs mechanically secured at the flash-butt-weld
end and fillet welded at the other end, or R4 studs mechanically secured at both
ends. Other grades, attachments, studless chains and permanent installations are
out of scope. The attachment subset follows [IACS Recommendation 38](https://iacs.s3.af-south-1.amazonaws.com/wp-content/uploads/2022/05/20093306/rec38rev2.pdf),
sections 1 and 5.1; this is not fleet or whole-line qualification.

Every input is required. Nominal diameter is an exact declared design reference
in millimetres, not a measured baseline. Each location supplies two perpendicular
diameters and an absolute uncertainty bound in millimetres. Location identifiers
are unique; the admitted regions are the bend and fairlead shoulder. Coverage,
perpendicular readings and uniform section loss require explicit
confirmation. Visual observations for cracks, missing studs, loose studs,
geometry anomalies and pitting are inspected-negative, inspected-positive or
unknown. Unknown or positive observations prevent dimensional clearance.
Missing confirmations emit `UNIFORM_LOSS_NOT_CONFIRMED`,
`PERPENDICULAR_READINGS_NOT_CONFIRMED` or `COVERAGE_NOT_CONFIRMED`,
respectively, for uniform loss, perpendicular readings or coverage.

The maximum diameter-loss fraction and minimum remaining-area ratio are operator
inputs with a required reference. No discard threshold, corrosion rate,
measurement accuracy or fatigue coefficient has a default. Source criteria are
not embedded; the operator reference shall establish suitability of the limits
for the assessed chain and use.

## Calculation and disposition

The perpendicular average follows IACS section 5.2, page 4. Arithmetic is derived
independently: mean diameter is `(diameter_a + diameter_b) / 2`; diameter loss is
`1 - mean_diameter / nominal_diameter`. For confirmed uniform loss,
`area_ratio = (mean_diameter / nominal_diameter) ** 2`, because circular area is
proportional to diameter squared. This equivalent-circular area proxy does not
measure an elliptical or irregular section and is not a residual breaking-load
model. IACS section 5.4, page 5, identifies the average-diameter
basis for area evaluation.

Both diameter readings use the same supplied absolute uncertainty bound.
Subtracting and adding that bound gives the mean-diameter interval. Diameter
loss reverses its endpoints; area ratio squares them. No distribution,
independence assumption or confidence level is inferred. Uncertainty bounds
that reach a nonpositive diameter are invalid. Bounds above the nominal design
reference are outside the implemented loss model.

Each sampled interval is compared with both operator limits. Entirely meeting
both limits, including equality, gives `WITHIN_LIMITS`. Entirely exceeding
either limit gives `EXCEEDS_LIMITS`. Overlapping a limit gives `INDETERMINATE`.
The area comparison uses products: squared mean-diameter bounds against the
area limit times squared nominal diameter, without squaring a rounded quotient.
A common exact power-of-two scale places nominal diameter in `[1, 2)` to avoid
large-area overflow. No tolerance is added; reported area ratios retain their
squared-quotient definition. Diameter-loss comparisons retain
`(nominal - bound) / nominal`, which meets the hand-exact `30 / 100 = 0.30` equality.
Unsupported scope, section shape, coverage or observations give `OUT_OF_SCOPE`,
with the existing `Applicability` type carrying the reason. No overall verdict
is issued, even when every inspected location meets its dimensional limits.

Fairlead shoulder readings use the same local calculation, following IACS
section 5.2. Visual defects and unknown observations require review under IACS
sections 2.2.2, 5.3 and 5.4; repair eligibility is not inferred. This policy
escalates all such observations rather than substituting unverified tolerances.

## Opened sources and rights

- International Association of Classification Societies, *Guidelines for the
  Survey of Offshore Mooring Chain Cable in Use*, Recommendation 38, revision 2,
  July 2020; sections 1, 2.2.2, 5.1 through 5.4, pages 1 through 5; URL above.
  No reuse licence is stated in the inspected PDF. Citation only; no clause or
  table is reproduced.
- ABS Consulting for the Bureau of Safety and Environmental Enforcement,
  [*Study on Mooring System Integrity Management for Floating Structures*](https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program//730-aa.pdf),
  final report, August 4, 2015; Table 7, page 40, and section 1.2.3, page 5.
  No reuse licence is stated in the inspected passages; citation only.
- Lone, Maincon, Gabrielsen, Sauder, Larsen and Leira,
  [*Analysis of S-N data for new and corroded mooring chains at varying mean load levels using a hierarchical model*](https://engrxiv.org/preprint/download/2377/4605),
  engrXiv preprint submitted to Marine Structures, May 27, 2022; sections 1 and
  3.1, pages 1, 2 and 6. The opened landing page states Creative Commons
  Attribution 4.0 International. No coefficients or dataset excerpt is taken.

These documents were opened on October 12, 2026. Result sidecars identify the
dimensional-method and interlink-exclusion sources and locators; operator-limit
provenance remains separate. No new standards-derived numerical coefficient is
introduced.

## Gaps and validation

- Remaining breaking load requires a validated degraded-link strength method
  and accessible break-test data. Geometric area loss alone does not close it.
- Small, medium and mega pitting classes and stress concentration require the
  underlying SCORCH paper and Chain FEARS definitions. The opened repository
  record supplies no class-boundary method.
- Interlink wear prediction requires HSE RR1092, particularly Appendix A and
  its calibration evidence. The opened FABIG summary identifies the method;
  the publisher PDF returned HTTP 404. Regulator Table 7 requires a contact
  double-diameter reading and baseline, which cannot identify separate worn
  diameters without additional measurements.
- Ovalization acceptance requires a qualified tolerance and geometry method.
  Visual anomaly flags do not supply that tolerance.
- Fatigue remaining life requires a cited chain curve qualified for reduced
  section, mean load and service history. The existing S-N library contains
  welded-detail curves. The opened Lone model assumes negligible section loss
  in section 3.1; applying it to measured reduced section is not established.

`tests/asset_integrity/test_mooring_chain_dimensions.py` states independent
arithmetic in each numerical case: perpendicular averaging, squared area ratio,
limit equality, separate locations and interval overlap. Scope cases cite IACS
sections 1, 5.1 and 5.2; interlink exclusion cites regulator Table 7. Synthetic
operator limits test comparisons and are not engineering criteria. These cases
validate arithmetic and exclusions; published residual-strength golden tests
remain a gap. Workflow, report, route and catalogue wiring are left for later.
