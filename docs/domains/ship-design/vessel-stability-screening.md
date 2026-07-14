# Vessel intact stability screening (criteria + KG limits)

## What this is

`digitalmodel.naval_architecture.vessel_stability_screening` and the routed
workflow basename `vessel_stability_screening` run a deterministic intact
stability screen from config: a draft-indexed hydrostatic table, a loading
condition with per-tank free-surface correction, a GZ curve from a GZ table
or KN cross-curves, cited stability criteria (IMO IS Code 2008 Part A
intact set, 46 CFR 170.170 weather criterion, 46 CFR 173.005-series style
lifting/crane-heel criteria) and a max-KG (KG-limit) screen per criterion.

This is the L1 slice of the stability/hydrostatics/T&S-booklet lane:
hydrostatics come in as a *table* (from GHS, a curves-of-form sheet, or a
published example) — geometry-based hydrostatics, damage stability and the
T&S booklet report assembly are later slices.

## Governance — screening tier only

**This is a screening tool.** It ranks and gates intact loading conditions
(e.g. across a T&S-booklet condition set or a lift plan) so attention goes
to the governing cases. **The PE-stamped stability booklet / class or USCG
submittal governs**; GHS remains the licensed cross-check. Criteria
thresholds are *cited config inputs* (foam_system Citation pattern —
standard/edition/clause mandatory): the IMO IS Code 2008 defaults are baked
in with their citation and are overridable; the 46 CFR weather-criterion
wind pressure and the lifting criteria must be supplied and cited by the
user against the governing edition.

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

Units: SI marine practice — m, t, degrees, m·rad; MCT in t·m/cm.

## Outputs

Loading-condition, equilibrium, GZ-curve (with heeling-arm columns when a
lift is configured), criteria (with a citation column), KG-limit and
one-row summary CSVs — all report_pack-ready — plus the same content in
the returned config; `screening_status` is stamped pass/fail from the
evaluated criteria.

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

A GHS golden fixture (run file + output listing) is the planned
cross-check once the extraction lane lands (llm-wiki-acma#227/#262);
damage stability and T&S-booklet assembly via `report_pack` are the next
slices (llm-wiki-acma#239).

## Config

See the module docstring of
`src/digitalmodel/vessel_stability_screening/workflow.py` for the full
YAML schema and `src/digitalmodel/base_configs/modules/vessel_stability_screening/vessel_stability_screening.yml`
for the base config.
