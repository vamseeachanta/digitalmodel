# Part 8 piping geometry screen

The document follows the domain placement of
`level3-and-part9-program-2026-09-25.md`. The calculation follows
API 579-1/ASME FFS-1, June 2016, Part 8; no equivalence to the December
2021 edition is established.

## Scope and inputs

The pure functions in `shell_distortion_screen.py` evaluate individual
ASME B31.3 piping geometry criteria from Table 8.4, printed page 8-24.
A PASS applies to the assessed feature. Component fitness for continued
operation is not established by a feature result.

Inputs comprise specified and measured extreme outside diameters, wall
thickness, documented purchase specification and weld angular magnitude.
Lengths use millimetres; angles use degrees; ratios are dimensionless.
Original tolerance applicability, noncyclic service and operation outside
the creep range require confirmed engineering evidence. These confirmations
are inputs, not findings calculated by the screen.

## Calculation steps

1. Positive finite dimensions, ordered extreme diameters and an internal
   pipe diameter greater than zero are checked. Angular magnitude must be
   finite and nonnegative.
2. Construction-code and service eligibility are checked against clauses
   8.2.1, 8.2.2, 8.2.4, 8.2.5.1 and 8.4.1(a). An original fatigue analysis,
   coexisting damage or local shell distortion excludes the bounded screen.
3. For purchased straight pipe, the diameter difference is divided by
   specified outside diameter and compared with the applicable individual
   Table 8.4 limit. ASTM A530 additionally requires the table's thin-wall
   condition. Unsupported purchase specifications return NOT_APPLICABLE.
4. For angular weld misalignment, the measured angular magnitude is compared
   with the individual Table 8.4 angular criterion.
5. Utilisation is governing value divided by the cited limit. A value no
   greater than the limit returns PASS; exceeding it returns NEEDS_ASSESSMENT.
   Inapplicable cases return no utilisation. Every result carries a Citation
   sidecar; missing or mismatched citation metadata blocks calculation.

## Applicability and validation

Only the listed purchased-pipe specifications and the angular weld criterion
are supported. Fittings, elbows, centreline offsets, tank tolerances,
general shell distortions and combined defects require separate assessment.
A qualifying result does not establish cyclic-service eligibility, remaining
life, maximum allowable working pressure or the absence of other defects.

The tests use Table 8.4 values and synthetic hand calculations. For specified
outside diameter 1000 millimetres and diameter difference 10 millimetres,
the ratio is 0.01. For the listed welded-pipe criterion, utilisation is 1.
For ASTM A530, difference 15 millimetres gives ratio 0.015 and utilisation 1;
wall thickness 30 millimetres gives the inclusive wall ratio 0.03.
An angular magnitude of 3 degrees gives utilisation 1. Both sides of each
limit, scope exclusions, invalid inputs and missing citations are tested.
No published worked example solution was opened. Validation of a complete
component assessment against such an example remains outstanding.

## Gaps

- Part 8 Level 2 and additional fabrication criteria are not implemented.
  Coupled stress assessment and all applicability requirements are not
  validated by these individual Table 8.4 tests.
- Part 14 remains not started because its operative 2016 clauses were not
  available to this work. The cited fatigue paper abstract
  was opened, but the full paper and normative screening procedure were not.
- Part 13 remains not started. The 2007 clauses 13.2 and 13.4 were opened;
  the revised 2016 method was not accessible. A validated 2007 citation
  target and current-edition reconciliation are not established.
- Part 11 remains not started. The 2007 clauses 11.2 through 11.4 and
  Table 11.12 were opened. A matching validated 2007 citation target and
  published example solution are not established. Inspection and metallurgy
  remain prerequisites for assigning heat-exposure zones.
- API 579-2/ASME FFS-2 2009 example solutions and WRC Bulletin 550 were not
  available to this work. Validation against worked solutions remains
  outstanding.
- Full API 579-1/ASME FFS-1 December 2021 clauses were not
  available to this work; equivalence to that edition is not established.
- Workflow, routing, catalogue and decision/report integration are not
  added. Existing component-level patterns do not establish additive wiring
  for an individual-feature screen without overstating its scope.
