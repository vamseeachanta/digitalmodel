# Vessel stability screening (intact + damage criteria, KG limits)

## What this is

`digitalmodel.naval_architecture.vessel_stability_screening`,
`digitalmodel.naval_architecture.damage_stability_screening` and the routed
workflow basename `vessel_stability_screening` run a deterministic
stability screen from config: a draft-indexed hydrostatic table, a loading
condition with per-tank free-surface correction, a GZ curve from a GZ table
or KN cross-curves, cited stability criteria (IMO IS Code 2008 Part A
intact set, 46 CFR 170.170 weather criterion, 46 CFR 173.005-series style
lifting/crane-heel criteria), a max-KG (KG-limit) screen per criterion, and
a `damage_cases` section (slice 2): flooded-compartment groups by the
added-weight method or directly supplied damaged hydrostatics/KN, screened
against cited survival criteria (46 CFR Part 174 per the applicable vessel
class, IMO MODU Code 2009 damage criteria).

This is the L1+L2 slice pair of the stability/hydrostatics/T&S-booklet
lane: hydrostatics come in as a *table* (from GHS, a curves-of-form sheet,
or a published example) and damaged hydrostatics/KN come in per case (GHS
supplies them per job) — geometry-based hydrostatics/flooding computation
and the T&S booklet report assembly are later slices.

## Governance — screening tier only

**This is a screening tool.** It ranks and gates intact loading conditions
and damage cases (e.g. across a T&S-booklet condition set, a lift plan or
a damage-case matrix) so attention goes to the governing cases. **The
PE-stamped stability booklet / damage-stability analysis / class or USCG
submittal governs**; GHS remains the licensed cross-check and the source
of damaged hydrostatics. Criteria thresholds are *cited config inputs*
(foam_system Citation pattern — standard/edition/clause mandatory): the
IMO IS Code 2008 defaults are baked in with their citation and are
overridable; the 46 CFR weather-criterion wind pressure, the lifting
criteria and **all damage survival criteria** must be supplied and cited
by the user against the governing edition (damage rule values vary by
vessel class — 46 CFR Part 174 subparts, MODU Code unit types — so none
are baked in).

## Method

1. **Hydrostatic-table interface.** Draft-indexed rows (draft,
   displacement, KM, and optionally LCB, LCF, MCT1cm, TPC) from inline YAML
   or CSV; linear interpolation in displacement (or draft); no
   extrapolation — out-of-range queries raise.
2. **Loading-condition builder.** Lightship + tank/cargo weight items sum
   to displacement, KG and LCG. Free-surface correction is the standard
   virtual rise `FSC = sum(FSM_i) / W` from per-tank free-surface moments,
   given directly (t·m) or as rectangular tank dimensions
   (`FSM = rho·l·b³/12`); `KG_fluid = KG + FSC`.
3. **Equilibrium estimate.** Mean draft at the condition displacement;
   `GM = KM − KG` (solid and fluid); trim (positive by the stern) from
   `trim_cm = W·(LCB − LCG)/MCT1cm`, distributed about the LCF to end
   drafts when LBP is given. Longitudinal positions from the aft
   perpendicular.
4. **GZ curve.** From a pre-corrected GZ table, a KN cross-curve at the
   condition displacement, or a KN grid interpolated linearly in
   displacement: `GZ = KN − KG_fluid·sin(phi)`. Areas by trapezoidal
   integration (shared with the validated IS-Code helpers in
   `naval_architecture.damage_stability`).
5. **Criteria table (all rows cited).**
   * IMO IS Code 2008 (Res. MSC.267(85)) Part A 2.2: areas 0–30°, 0–40°
     (capped at the downflooding angle), 30–40°, GZ at 30°, angle of max
     GZ, initial GM0 — evaluated on the fluid GM.
   * 46 CFR 170.170 weather criterion:
     `GM ≥ P·A·H / (W·tan(T))`, `T = min(14°, deck-edge immersion)`; the
     wind pressure `P` (service- and edition-dependent) is a cited config
     input.
   * Lifting/crane heel (46 CFR 173.005-series style): heeling arm
     `HA(phi) = HM·cos(phi)/W` from a hook load × transverse outreach (or
     a direct heeling moment); static equilibrium heel at the first
     GZ/HA intersection (bisection); criteria are a cited maximum
     equilibrium heel (capped by deck-edge immersion) and an optional
     residual-righting-energy / heeling-energy ratio over the residual
     range (to the least of the configured limit, downflooding angle and
     the tabulated GZ range). A lift whose heeling arm exceeds GZ over the
     whole range has no static equilibrium and fails hard.
6. **Max-KG screening.** For each criterion, bisect the fluid KG on
   [0, KM] for the largest value that still passes at the condition
   displacement → KG-limit table with the governing (lowest) criterion.
   Criteria that still pass at KG = KM are flagged `limited_by_km`;
   criteria failing even at KG = 0 report no limit.
7. **Damage cases (slice 2).** Two input paths per case:
   * **Added-weight** — flooded compartments (volume × permeability,
     floodwater centroid VCG/LCG/TCG, floodwater FSM given directly or as
     rectangular dimensions). Floodwater enters the loading condition as
     weight items, so displacement, KG, LCG, trim and the fluid GM come
     from the *intact* hydrostatic table at the flooded displacement (no
     extrapolation). Static heel from
     `tan(heel) = |Σ w·tcg| / (W′·GM′_fluid)` (None → fail when the
     post-damage GM is not positive). *Free-communication note*: a
     compartment open to the sea must supply its floodwater FSM
     (`free_communication: true` enforces this); added-weight-plus-FSM is
     a screening approximation of free communication — the rigorous
     lost-buoyancy solution stays with the licensed tool.
   * **Direct** — damaged-condition hydrostatics supplied per case
     (displacement, damaged fluid GM, static heel, optionally trim and
     the damaged fluid KG), the practical screening path: GHS supplies
     these per job privately.

   Either path may add a damaged GZ table or damaged KN cross-curve
   (`GZ = KN − KG′_fluid·sin(phi)`). The heeling arm is the flooding
   transverse moment (cosine arm) plus an optional constant wind arm from
   `wind_heeling_moment_t_m`; the curve equilibrium (first up-crossing of
   `GZ − HA`, bisected) supersedes the small-angle heel. Range of positive
   stability runs from equilibrium to the vanishing angle, capped at the
   least of the downflooding angle (per-case or vessel-level) and the
   margin-line immersion angle. Cited survival criteria (**all
   config-supplied — rule values vary by vessel class and edition**): max
   static heel, min damaged GM, min margin to downflooding/margin line,
   min range beyond equilibrium, min residual righting area, min
   residual-to-heeling area ratio (typical sources: 46 CFR Part 174 per
   the applicable class; IMO MODU Code 2009, Res. A.1023(26)). A
   configured criterion whose input is missing raises; an indeterminate
   value (no static equilibrium, negative GM, equilibrium beyond the
   downflooding angle) fails.

Units: SI marine practice — m, t, degrees, m·rad; MCT in t·m/cm.

## Outputs

Loading-condition, equilibrium, GZ-curve (with heeling-arm columns when a
lift is configured), criteria (with a citation column), KG-limit,
damage-case and damage-criteria (per case, cited) and one-row summary CSVs
— all report_pack-ready — plus the same content in the returned config;
`screening_status` is stamped pass/fail across the intact criteria and
every damage case.

## Validation

`tests/naval_architecture/test_vessel_stability_screening.py` (+ the
workflow tests) pin the implementation to a synthetic box-barge golden
fixture in the standard published closed forms (Barrass & Derrett box-barge
hydrostatics/trim; PNA Vol. I; wall-sided GZ formula) — synthetic fixtures
only, no client-derived values:

* Box barge 100 m × 20 m, W = 8200 t → T = 4.0 m, KM = 10.3333 m,
  KG = 4.9024 m, FSC = 0.0500 m exactly, GM_fluid = 5.3809 m; trim
  0.2341 m by the stern from a 4000 t·m trimming moment
  (MCT1cm = 170.833 t·m/cm).
* Wall-sided KN cross-curve: GZ(30°) = 3.3849 m; analytic GZ areas
  (0–30° = 0.8073 m·rad, 0–40° = 1.5567 m·rad) recovered by the
  trapezoidal table integration to ~0.3 %.
* 46 CFR 170.170 hand value: GM_required = 165/2044.49 = 0.0807 m.
* Crane lift 100 t × 15 m: equilibrium heel 1.9377° (closed-form
  intersection on the tabulated curve); a heeling arm exceeding max GZ
  reports no static equilibrium and fails.
* Max-KG hand values: GM0 criterion gives KG_limit = KM − 0.15
  = 10.1833 m; weather gives KM − 0.0807 = 10.2526 m; the 5° lift-heel
  limit gives 8.2744 m and governs.
* Edge cases: negative GM (GM0 fails), FSM dominance (FSC = 10 m flips
  GM_fluid negative), out-of-range hydrostatics, uncited criteria
  rejected.

`tests/naval_architecture/test_damage_stability_screening.py` (+ the
damage workflow tests) extend the same fixture with a hand-verified
flooding case:

* Flooded wing compartment 500 m³ × 0.8 permeability = 410 t at
  tcg 4.0 m, floodwater FSM 451 t·m → W′ = 8610 t, table draft
  T′ = 4.2 m and KM′ = 10.1 m exactly, KG′_f = 4.8643 m, FSC′ = 0.10 m
  exactly, GM′_f = 5.2358 m; W′·GM′_f = 45080 t·m exactly, so
  tan(heel) = 1640/45080 → 2.0835°; trim 0.7141 m by the stern.
* Damaged wall-sided KN curve: GZ(30°) = 3.2793 m; curve equilibrium
  against the flooding-moment arm 2.0737°; with downflooding at 30°,
  margin and range = 27.93° and residual area ≈ 0.692 m·rad (analytic
  wall-sided integral recovered to ~0.4 %).
* Edge cases: negative post-damage GM (heel indeterminate → fail),
  equilibrium beyond the downflooding angle (negative margin, zero
  range → fail), no static equilibrium under a capsizing arm,
  vanishing-angle capping, uncited or unknown criteria rejected, GZ
  thresholds without GZ data rejected.

A GHS golden fixture (run file + output listing) is the planned
cross-check once the extraction lane lands (llm-wiki-acma#227/#262);
geometry-based flooding and T&S-booklet assembly via `report_pack` are
later slices (llm-wiki-acma#239).

## Config

See the module docstring of
`src/digitalmodel/vessel_stability_screening/workflow.py` for the full
YAML schema and `src/digitalmodel/base_configs/modules/vessel_stability_screening/vessel_stability_screening.yml`
for the base config.
