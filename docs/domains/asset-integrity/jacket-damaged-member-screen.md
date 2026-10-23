# Damaged and corroded jacket member analysis

The `jacket-damaged-member-screen` workflow combines bounded dent/bow predictions,
uniform wall-loss preparation and a system-damage handoff. Every shared fitness
decision is **ESCALATE** and engineering acceptance is **NOT_EVALUATED**. A demand
below a research prediction does not establish an API RP 2SIM or ISO 19902 verdict.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 0 | 2026-10-20 | Initial offline workflow to carry diagnostics and qualification gaps together. |
| 1 | 2026-10-22 | Remove unqualified strength outputs, preserve campaign flags and report precision, and reject missing inputs. |

## Run the worked example

From the repository root, with the package dependencies installed:

```sh
PYTHONPATH=src python -m digitalmodel.structural.jacket_topside.damaged_member_screen \
  examples/workflows/jacket-damaged-member-screen/input.yml \
  --output results/jacket-damaged-member-screen.json
```

The command writes strict JSON and a sibling HTML analysis. The JSON retains
calculation precision; the HTML displays thicknesses and dimensionless ratios to
three decimals, and other numbers to one decimal. Output paths must end in `.json`
and cannot overwrite the input. The command
imports no solver adapter and executes no solver. It is a standalone Python-module
workflow; the general engine and the retired chatbot route are not used.
The callable interfaces are `screen_member(item)` and `screen_campaign(config)`.

All example inputs are synthetic demonstrations, not measurements. The example
contains three independently assessed members; its results are not a combined
platform resistance calculation.
The report headline retains the ordered, deduplicated union of every member's
flags. Revision history preserves the initial entry and records fixed issue dates.

## Inputs, method and answer

**Dent and bow.** All dimensionless quantities and support/load conditions are
required explicitly. The input follows [Design Approximations for Offshore Tubulars
Against Collisions (1987), Volume I](https://theses.gla.ac.uk/77600/1/10997383.pdf),
section 6.3.1 and equations 6.13–6.17, printed pages 193–194. The existing
[dented axial method](jacket-dented-axial-method.md) defines the midwall diameter,
plastic-neutral-axis bow reference, study envelope and material restriction.
The shipped example uses that method's hand-calculation case. Its predicted
capacity is 40.8 % of yield and demand is 49.0 % of predicted capacity.
The demand is below the research prediction; experimental qualification remains
missing, so the shared fitness decision is ESCALATE. Out-of-range geometry,
material ratios, restraints, applied bending or pressure suppress the prediction.

**Uniform wall loss.** The method basis is the [BOEMRE TA&R 677 comparison report
(2012), Appendix A](https://www.bsee.gov/sites/bsee.gov/files/research-reports/677aa.pdf),
which compares ISO 19902:2007, “Corroded tubular members”, and NORSOK N-004,
“Corroded members”. The locator is a secondary citation, not verified against the
standard text:
`BOEMRE TA&R 677, Appendix A: ISO 19902:2007 A13.8 / NORSOK N-004 corroded members (secondary citation)`.
Approximately uniform wall loss is represented by an explicitly
supplied remaining thickness. The unchanged outside diameter is an input-model
assumption. External diameter loss, localized patches and joint corrosion require
separate treatment. A minimum reading is not silently treated as a representative
average. Inspection sufficiency is outside this implementation.
Declaring uniform corrosion without `remaining_thickness` raises
`CORROSION_INPUT_MISSING`, including when dent data is supplied. Each applicability
flag carries its own explanation in the same order.

Geometry and stresses use inches, kips, kip-inches and ksi. Material properties,
effective length and moment factor are required inputs, with no new material or
code coefficients supplied by the workflow. Axial force and moment magnitudes
are nonnegative; the explicit `is_tension` flag records the axial sense.
The example has outside diameter 10.0 in, nominal wall 1.000 in and remaining wall
0.500 in. The unchanged actions are axial force 30.0 kip and moment 20.0 kip-in.

Independent hand calculation integrates an annulus:
`d = D - 2t`, `A = pi*(D²-d²)/4`, `I = pi*(D⁴-d⁴)/64`, `S = 2I/D`.
The axial relation follows from `N = integral(sigma dA)` for uniform stress;
bending follows [Stress Analysis Manual (1986), section 1.3.1.1, equation
1-1](https://engineeringlibrary.org/reference/simple-beam-bending-air-force-stress-manual).
The coefficients in the annulus formulas are mathematical integration factors,
not empirical strength constants. The example gives area 14.9 in², section
modulus 33.8 in³, axial stress 2.0 ksi and bending stress 0.6 ksi. The reported
ratios are D/t = 20.000 and KL/r = 29.732, with
`r = sqrt(I/A)`. These geometry and stress diagnostics carry no allowable stress,
unity check or pass field. Compression, compactness and tension-interaction
strength checks remain unqualified and are not executed by this workflow.
The corrosion status remains NOT_EVALUATED.

**Flooded or severed member.** Explicit boolean flags take precedence over local
calculations and request Level 3 engineering review. This mapping is workflow
policy requested by the issue, not an inferred API clause. No reserve strength
ratio is calculated. The handoff lists system connectivity, damage records,
load combinations, boundary conditions and assessment criteria as required inputs.
The severed example returns no local capacity and an ESCALATE decision.

## Qualification and remaining scope

The new tests write out independent annulus and stress calculations, reuse the
existing sourced dent hand case, and test input rejection, system-damage priority,
combined-damage exclusion, report escaping and strict JSON/HTML CLI readback.
They do not constitute golden experimental validation. The published specimen
excluded by the existing material guard remains excluded.

The three delivered slices are dent prediction composition, uniform-wall member
preparation diagnostics, and shared decision/report plus handoff/CLI.
Joint corrosion re-checks, dent bending/pressure reductions, combined damage,
code-qualified acceptance, a published applicable experimental series, and general
engine/catalog promotion remain later work. API RP 2SIM and ISO 19902 are reference
families here; no unsourced assessment-level or acceptance coefficient is inferred.
