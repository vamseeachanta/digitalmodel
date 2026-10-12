# Dented tubular axial calculation

Scope: issue 2198.
The document follows the scope, comparison, and limits pattern of
[riser-joint validation](riser-joint-ffs-validation-2026-10-10.md).
The calculation implements a bounded axial subset of Cho (1987), Volume I,
[doctoral thesis](https://theses.gla.ac.uk/77600/1/10997383.pdf),
section 6.3.2, equations (6.13) to (6.17), pages 193 to 194.
The source designation is *Design Approximations for Offshore Tubulars Against
Collisions*. Applied bending and hydrostatic pressure are excluded.

## Inputs and calculation

All inputs and outputs are dimensionless. Stress ratios require consistent
stress units. Input fields have no implicit support or loading assumptions.

- Mid-thickness diameter divided by wall thickness supplies the diameter ratio.
  Outside diameter cannot be substituted; the source notation distinguishes
  the two diameters (page 3).
- Reduced slenderness is the square root of yield stress divided by elastic
  column buckling stress (notation, page 8). The upstream determination of
  elastic column buckling stress is outside this calculation.
- Dent depth is divided by mid-thickness diameter. Bow at the plastic neutral
  axis is divided by member length (notation, page 7; section 6.4.2.1).
- Elastic modulus divided by yield stress supplies the material ratio.
  Applied axial compressive stress divided by yield stress supplies demand.
- Boolean conditions confirm simply supported ends, a sharp midlength dent,
  the specified bow reference, absence of applied bending, and zero pressure.

The overall imperfection term is calculated from bow and reduced slenderness
using equation (6.14). The dent multiplier uses dent depth and diameter ratio
in equation (6.15). Equation (6.16) gives a pressure multiplier of one for zero
pressure. Their product supplies the imperfection in equation (6.13).
Equation (6.17) supplies the lower-root residual stress, normalized by yield.
Utilization is normalized axial demand divided by predicted residual capacity.

The result compares demand with predicted capacity. The verdicts
`within_predicted_capacity` and `exceeds_predicted_capacity` do not establish
engineering acceptance or an API assessment level. Unsupported conditions
return `outside_applicability`, named flags, and no capacity or utilization.
Each result carries the source designation, year, equation/section locators,
and the public source URL. Experimental qualification remains unestablished.

## Applicability

Cho section 6.3.1, page 193, lists an axial study grid spanning:
mid-thickness diameter/thickness from 20 to 60; reduced slenderness from
0.25 to 1.50; normalized dent depth from 0 to 0.15; normalized bow from
0.0005 to 0.02. Elastic modulus/yield stress is held at 600.
The extrema bound the implemented envelope. Continuous interpolation between
study points is an implementation assumption, not independent validation.
Values outside the envelope or a material ratio other than 600 are excluded.

Section 6.4.2.1, page 197, limits the formulation to simply supported members
with a sharp midlength dent and warns about the bow reference. An
opposite-generatrix measurement requires a correction that this function does
not perform. A fixed-platform member with other end restraints does not qualify.

## Hand verification and published measurements

A synthetic study-grid case uses diameter/thickness 40, reduced slenderness 1,
dent ratio 0.1, bow ratio 0.005, material ratio 600, and axial demand 0.2.
Equations (6.14) and (6.15) give terms 0.5440414350 and 1.5775725438.
Their product is 0.858264830595756. With critical stress/yield equal to one,
equation (6.17) gives capacity/yield 0.4081429636.
Demand/capacity is 0.4900243734, below the comparison threshold of one.
The test tolerance of 0.0000000001 covers decimal rounding only.

Cho Tables 4.1 and 4.3, pages 113 and 122, were opened. Specimen B1 measured
collapse load is 43.1 kilonewtons and measured stress/yield is 0.45. Its elastic
modulus/yield ratio is 212000/516, approximately 410.853, rather than 600.
The specimen is excluded by the study-material guard. This is an applicability
regression, not a golden experimental validation of predicted capacity.
Section 6.4.2.1 and Table 6.1 discuss prediction scatter; measured capacity
cannot be substituted for an exact formula output.

The offline tests check the hand calculation, demand comparison, both sides
of every envelope limit, material constraint, unsupported conditions, invalid
inputs, and citation locators. They do not establish fixed-platform qualification.

## Gaps

- Applied bending, hydrostatic-pressure capacity, and fixed-platform end-restraint
  corrections are not implemented in this axial subset.
- Uniform corrosion assessment is described by NORSOK N-004, revision 2 (2004),
  section 10.6.3. Full existing member/joint re-check reuse is blocked by the
  current simplified checks: API RP 2A-WSD, edition 21 (2000), equation
  (3.2.2-1) has an inelastic column denominator absent from the existing
  compression implementation; bending equations (3.2.3-1b) and (3.2.3-1c)
  are absent from the unconditional bending allowable. Existing behavior
  is preserved; correction requires separate work.
- Flooded/severed-member Level 3 mapping is not implemented. An eligible
  assessment clause establishing that mapping was not opened. The held API
  RP 2SIM edition 1 (2014) copy is excluded because of a third-party marking.
- Published ISO 19902 and NORSOK N-006 full texts were not found in the archive.
  ISO draft extracts do not establish a published edition. The research note
  specifies neither edition; a published edition remains required.
- Landet and Lotsberg (1992), *Laboratory Testing of Ultimate Capacity of Dented
  Tubular Members*, and OTC 6938 (1992) full papers were not found in the archive
  or opened through an eligible public source. Their specimen validation is absent.
- Generalized decision/report wiring, durable workflow, and catalogue promotion
  are not implemented. Existing sibling workflows issue engineering verdicts;
  no exact additive pattern for this prediction-only result was established.
- The chatbot route is not built: concept retired by the owner.
- Golden experimental validation and an independent second-provider review
  remain outstanding; the hand calculation alone does not satisfy acceptance.
