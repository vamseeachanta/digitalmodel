# Hull-girder longitudinal strength screening (SF/BM vs allowables)

## What this is

`digitalmodel.naval_architecture.hull_girder_screening` and the routed
workflow basename `hull_girder_screening` compute the still-water shear-force
and bending-moment distribution of a loading condition along the hull and
report utilisation against user-supplied allowable SF/BM curves interpolated
at frame positions, with an optional hull-girder section-modulus check
(simple midship scantlings or a class-approved SM value).

## Governance — screening tier only

**This is a screening tool.** It ranks and gates loading conditions (e.g.
across a ballast sequence or a T&S booklet condition set) so that attention
goes to the governing cases. **Any class-compliance conclusion still requires
the approved loading instrument (loadicator) or a class-endorsed
calculation.** Allowable SF/BM curves and approved section-modulus values are
*inputs*, supplied per project from the approved loading manual / class
documentation — nothing in this module derives or substitutes for them. This
mirrors the governance posture of the loading-computer screening lane.

## Method

1. **Weight distribution builder.** Lightship blocks and tank weights are
   linear-density trapezoids over their longitudinal extent, pinned to total
   weight and LCG (an LCG outside the middle third of the extent is rejected
   — subdivide the block). Point weights are smeared over a short extent.
   Stations carry the cell-averaged density over their tributary cell, so
   grid-aligned uniform blocks integrate exactly under the trapezoidal rule.
2. **Buoyancy** in one of three modes:
   * `box` — rectangular barge: linear (trimmed-waterplane) distribution
     pinned so the discrete force and moment exactly balance the weight curve;
   * `hydrostatics_table` — Bonjean-style immersed sectional area vs draft at
     stations; the trimmed waterline `(T_aft, T_fwd)` is solved by damped
     Newton iteration so buoyancy balances displacement and LCB aligns with
     LCG;
   * `direct` — a buoyancy-per-metre curve exported from a hydrostatics
     program (GHS, ShipLoad, ...).
3. **Still-water SF/BM.** Load curve `q(x) = w(x) − b(x)`; `SF = ∫ q dx`;
   `BM = ∫ SF dx` (cumulative trapezoidal). **Positive BM = hogging.**
   In equilibrium both integrals vanish at the free ends: the *raw* end
   residuals are reported as fractions of displacement (and
   displacement x length) and gated against a closure tolerance (default
   0.5 %); an optional linear closure correction (standard loadicator
   practice) then makes the discrete integrals close exactly. The correction
   never masks a bad balance — the gate uses the uncorrected residuals.
4. **Utilisation vs allowables.** Allowable SF (positive/negative) and BM
   (hogging/sagging) magnitude curves are linearly interpolated at the
   reporting frames (default: the allowable-curve knots); utilisation is
   `|value| / allowable` with pass/fail against a configurable limit.
5. **Section modulus (optional).** Either a class-approved SM (deck/keel) or
   simple midship scantlings (horizontal/vertical strakes or lumped areas →
   neutral axis, inertia, SM). Bending stress at the still-water hogging and
   sagging extremes is checked against the IACS permissible `175/k` via the
   validated `hull_girder_strength` module; optionally the IACS UR S11 wave
   bending moment is combined per condition.

Units: SI marine practice — tonnes, metres, t/m, t (SF), t·m (BM), kN / kN·m
equivalents with g = 9.81; `x` from the aft end.

## Validation

`tests/naval_architecture/test_hull_girder_screening.py` (+ the workflow
tests) pin the implementation to closed-form hand calculations in the
standard published box-barge worked-example form (PNA Vol. I longitudinal
strength; Hughes & Paik ch. 3; Barrass & Derrett shear/bending examples) —
synthetic fixtures only, no client-derived values:

* Box barge, L = 100 m, W = 10 000 t over the middle half, uniform buoyancy:
  SF extreme W/4 = 2 500 t at the quarter points; midship BM
  −WL/16 = −62 500 t·m (sagging); SF/BM close to ~0 at both free ends.
  The mirrored condition reproduces +WL/16 (hogging) — sign convention.
* Bonjean waterline solve recovers the closed-form box-hull drafts
  (T_mean = W/(ρBL); end-draft difference 12·W·(LCG − L/2)/(ρBL²)).
* Box-girder section properties against a hand-calculated neutral axis,
  inertia and deck/keel section moduli; stress/utilisation against `175/k`.
* Deliberately unbalanced direct-buoyancy input trips the closure gate.

A golden fixture reproducing a real loading-instrument output (ShipLoad /
GHS longitudinal strength) is planned once the source booklets are extracted
(OCR-tier unlock); approved allowable curves from issued loading manuals
follow the same route. Until then the allowable curves in the examples are
synthetic placeholders.

## Workflow usage

```yaml
basename: hull_girder_screening
hull_girder_screening:
  length_m: 100.0
  n_stations: 401
  weights:
    lightship:
      - {name: hull, weight_t: 2000.0, x_start_m: 0.0, x_end_m: 100.0}
    tanks:
      - {name: WBT1, weight_t: 500.0, x_start_m: 10.0, x_end_m: 25.0, lcg_m: 17.0}
    point_weights:
      - {name: crane, weight_t: 50.0, x_m: 60.0, extent_m: 4.0}
  buoyancy:
    method: box           # box | hydrostatics_table | direct
  allowables:
    shear_force:
      x_m: [0.0, 25.0, 75.0, 100.0]
      positive_t: [800.0, 3000.0, 3000.0, 800.0]
    bending_moment:
      x_m: [0.0, 50.0, 100.0]
      hogging_t_m: [20000.0, 80000.0, 20000.0]
      sagging_t_m: [20000.0, 70000.0, 20000.0]
  frames:
    - {name: "Fr 20", x_m: 25.0}
  section_modulus:
    yield_mpa: 235.0
    approved_sm: {deck_m3: 5.5, keel_m3: 6.1, source: "loading manual"}
  output_dir: results
```

Outputs: a station table CSV (`x, weight/m, buoyancy/m, SF, BM`), a frame
utilisation CSV and a one-row summary CSV — ready to feed the `report_pack`
workflow as data tables — plus the same content in the returned config under
`hull_girder_screening` (with `screening_status` stamped on the config).

## Relationship to sibling modules

* `loading_computer` (basename `loading_computer`) is the self-contained
  box-hull loadicator (equilibrium + intact/damage stability + a basic SF/BM
  against a single allowable). `hull_girder_screening` generalises the
  longitudinal-strength leg: arbitrary hulls via Bonjean tables or direct
  buoyancy export, position-dependent allowable curves, frame-based
  utilisation reporting and the section-modulus check.
* `hull_girder_strength` supplies the IACS UR S11 wave bending moment and
  the permissible-stress / section-modulus yield check (reused here).
* `report_pack` consumes the CSV outputs for the standard report deliverable.
