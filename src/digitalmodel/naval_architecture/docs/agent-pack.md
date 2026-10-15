# Naval architecture knowledge pack

| Header field | Value |
| --- | --- |
| Module | naval_architecture |
| Repository | digitalmodel |
| Package path | src/digitalmodel/naval_architecture |
| Pack revision | 0, 2026-10-15 |
| Code revision | `d96aad6b05f0da8b01be3cc490277bf8080bf663` |
| Analysis family | EC — Python engineering calculations |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT |
| Implementation-map ref | origin/codex/implmap-work |
| Implementation-map commit | b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407 |
| Implementation-map data revision | 947fa50ff9a9e210de7e8f90e01b37add85c5954 |

## 1. Purpose and scope

The engine adapter routes damage stability, platform stability, hull resistance, hull seakeeping, yaw moment, rudder-stock torque and maneuvering-envelope calculations through existing helpers. [src/digitalmodel/naval_architecture/workflow.py:L70-L84]

The registry assigns the package beta maturity and routes tests to `tests/naval_architecture/`; this label does not qualify an engineering result. [docs/registry/module-routing.yaml:L195-L200]

The main calculation path covered here comprises imperial fundamentals and buoyancy, righting-arm corrections, SI damage screening, simplified natural periods and motion response, and the box-hull loading computer. [src/digitalmodel/naval_architecture/fundamentals.py:L44-L49] [src/digitalmodel/naval_architecture/hydrostatics.py:L27-L33] [src/digitalmodel/naval_architecture/stability.py:L26-L38] [src/digitalmodel/naval_architecture/damage_stability.py:L87-L149] [src/digitalmodel/naval_architecture/seakeeping.py:L32-L65] [src/digitalmodel/naval_architecture/loading_computer.py:L508-L562]

The loading computer uses rectangular hull geometry, a wall-sided GZ relation and a distributed-load strength screen; its GZ relation is limited by deck-edge immersion or bilge emergence. [src/digitalmodel/naval_architecture/loading_computer.py:L205-L258] [src/digitalmodel/naval_architecture/loading_computer.py:L261-L270] [src/digitalmodel/naval_architecture/loading_computer.py:L379-L405]

The supplied-table intact and damage screeners retain an approved stability analysis as the governing calculation; the direct damage route consumes supplied damaged hydrostatics rather than computing geometry flooding. [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L33-L42] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L7-L15]

Coverage decision: detailed review of the larger geometry, resistance-transfer, hull-girder and catalog-data branches remains open in section 10, issue pending (1).

## 2. Inputs and outputs

### Inputs

| Entry / quantity | Units | Range, default and source | Evidence |
| --- | --- | --- | --- |
| `mass_to_weight`, `buoyant_force`, `submerged_volume` | slug, lb, ft³, LT | Caller scalars; exact `saltwater` selects salt water, other strings select fresh water in the buoyancy helpers | [src/digitalmodel/naval_architecture/fundamentals.py:L44-L49] [src/digitalmodel/naval_architecture/hydrostatics.py:L27-L33] [src/digitalmodel/naval_architecture/hydrostatics.py:L75-L83] |
| Weight changes and inclining data | LT, ft, tangent dimensionless | Negative added weight removes weight; port subtracts TCG and other directions add it | [src/digitalmodel/naval_architecture/hydrostatics.py:L86-L131] |
| `gz_from_cross_curves`, TCG correction, free-surface correction | heel deg, KN/KG/TCG ft; tank ft; fluid slug/ft³ | KG above keel; scalar inputs; effective GM subtracts free-surface rise | [src/digitalmodel/naval_architecture/stability.py:L26-L38] [src/digitalmodel/naval_architecture/stability.py:L84-L122] |
| `reynolds_number`, `ittc_1957_cf`, `froude_speed_scaling` | ft, kn, ft²/s; dimensionless Reynolds number; model ft/s | Reynolds coefficient rejects nonpositive input but does not guard its positive singularity; see section 6 | [src/digitalmodel/naval_architecture/resistance.py:L35-L58] [src/digitalmodel/naval_architecture/resistance.py:L98-L112] |
| SI GZ curve and flooding | deg, m, m³, m²; permeability dimensionless | Permeability in [0, 1]; interpolation clamps at the supplied endpoints | [src/digitalmodel/naval_architecture/damage_stability.py:L27-L45] [src/digitalmodel/naval_architecture/damage_stability.py:L152-L170] |
| Natural periods, encounter frequency and spectrum | t, m, m², rad/s, m/s, deg; damping dimensionless | Positive GM/GML; heading 0 following, 180 head seas; damping default 0.05 | [src/digitalmodel/naval_architecture/seakeeping.py:L32-L65] [src/digitalmodel/naval_architecture/seakeeping.py:L110-L158] |
| `BoxHull`, `WeightItem`, `DamageCase` | m, t, t/m³; permeability dimensionless | Hull dimensions positive, weights nonnegative, total weight positive; damage breadth defaults to full beam | [src/digitalmodel/naval_architecture/loading_computer.py:L77-L123] [src/digitalmodel/naval_architecture/loading_computer.py:L195-L202] [src/digitalmodel/naval_architecture/loading_computer.py:L325-L333] |
| `TriMesh` / mesh condition | declared m or mm; draft/trim m | Declared right-handed frame; baseline at canonical z=0; closed, outward geometry | [src/digitalmodel/naval_architecture/mesh_hydrostatics.py:L6-L28] |

### Outputs and conventions

| Output | Units and conventions | Evidence |
| --- | --- | --- |
| Cited scalar wrappers | `value`, `units`, `citations`; lb, ft, s or dimensionless according to function | [src/digitalmodel/naval_architecture/fundamentals.py:L52-L88] [src/digitalmodel/naval_architecture/stability.py:L41-L81] [src/digitalmodel/naval_architecture/seakeeping.py:L68-L107] |
| GZ areas and criteria | m·rad, m, deg; per-criterion rows and aggregate boolean | [src/digitalmodel/naval_architecture/damage_stability.py:L48-L75] [src/digitalmodel/naval_architecture/damage_stability.py:L87-L149] |
| Loading result | displacement t; volume m³; draft/trim/GM m; shear t; bending t·m and kN·m; summary JSON | [src/digitalmodel/naval_architecture/loading_computer.py:L130-L189] [src/digitalmodel/naval_architecture/loading_computer.py:L508-L594] |
| Box-hull coordinates | x=0 aft, positive forward; vertical CG above keel; positive trim by bow | [src/digitalmodel/naval_architecture/loading_computer.py:L77-L112] [src/digitalmodel/naval_architecture/loading_computer.py:L223-L242] |
| Supplied-table stability coordinates | longitudinal origin aft perpendicular, positive forward; positive trim by stern | [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L39-L42] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L50-L53] |
| Yaw sweep coordinates | +x forward, +y port, +z up; positive yaw bow to port; force N, moment N·m; CSV/JSON | [src/digitalmodel/naval_architecture/yaw_moment.py:L110-L165] [src/digitalmodel/naval_architecture/yaw_moment.py:L168-L188] [src/digitalmodel/naval_architecture/yaw_moment.py:L362-L405] |
| Mesh hydrostatics | SI quantities tagged computed/declared with an input hash; trim forward minus aft draft | [src/digitalmodel/naval_architecture/mesh_hydrostatics.py:L29-L48] [src/digitalmodel/naval_architecture/mesh_hydrostatics.py:L94-L125] |

Do not transfer a signed trim value between the box-hull and supplied-table APIs without converting its convention. [src/digitalmodel/naval_architecture/loading_computer.py:L223-L242] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L39-L42]

UNVERIFIED: one common origin, phase convention and unit schema across every geometry, transfer and export branch has not been established; issue pending (1).

## 3. Method and governing standards

| Method / reference as found | Implementation and scope | Evidence |
| --- | --- | --- |
| USNA EN400, Principles of Ship Performance; tests label Summer 2020, Chapters 1–4, 7 and 10 | Weight, buoyancy, CG, inclining experiment, Simpson integration, GZ/free surface, resistance scaling and submarine helpers; textbook expectations remain regression evidence until checked independently | [tests/naval_architecture/test_en400_worked_examples.py:L3-L13] [src/digitalmodel/naval_architecture/hydrostatics.py:L86-L131] [src/digitalmodel/naval_architecture/integration.py:L13-L42] [src/digitalmodel/naval_architecture/stability.py:L84-L122] [src/digitalmodel/naval_architecture/submarines.py:L14-L59] |
| ITTC, 1957 model–ship correlation line | `resistance.ittc_1957_cf`; opt-in cited wrapper resolves an EN400 reference; SI `friction_scaling.ittc57_cf` delegates to it after validating the domain | [src/digitalmodel/naval_architecture/resistance.py:L51-L95] [src/digitalmodel/naval_architecture/friction_scaling.py:L98-L123] |
| ITTC Recommended Procedure 7.5-02-03-01.4, 1978 ITTC Performance Prediction Method, simplified resistance transfer | `transfer_model_to_ship`, inverse transfer and Prohaska regression; allowance terms are caller-declared or zero with a flag | [src/digitalmodel/naval_architecture/friction_scaling.py:L4-L20] [src/digitalmodel/naval_architecture/friction_scaling.py:L257-L345] |
| IMO, International Code on Intact Stability, 2008, Resolution MSC.267(85), Part A general intact criteria | `check_imo_intact_stability` integrates tabulated GZ and checks six criteria; the supplied-table screener carries clause records | [src/digitalmodel/naval_architecture/damage_stability.py:L1-L24] [src/digitalmodel/naval_architecture/damage_stability.py:L87-L149] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L470-L568] |
| IACS UR S11, Longitudinal Strength Standard, Rev.7 (2010), vertical wave bending moment and shear force | `wave_coefficient`, `wave_bending_moment`, distribution factors and `wave_shear_force`; `hull_girder_ultimate_moment` is a simplified capacity estimate | [src/digitalmodel/naval_architecture/hull_girder_strength.py:L60-L78] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L121-L137] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L143-L219] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L303-L311] |
| IACS UR S11A / CSR Common Structural Rules, hull-girder ultimate strength / Smith procedure | `smith_ultimate` solves neutral-axis equilibrium and marches curvature; code names CSR Pt 1 Ch 5 Sec 4 but gives no edition in its reference block | [src/digitalmodel/naval_architecture/hull_girder_smith.py:L3-L47] [src/digitalmodel/naval_architecture/hull_girder_smith.py:L357-L418] |
| DNV-RP-C201, Buckling Strength of Plated Structures; edition absent from this wrapper | `compression_flange_ultimate_stress` delegates to the structural stiffened-panel solver; Smith factories also delegate compression capacities | [src/digitalmodel/naval_architecture/hull_girder_strength.py:L350-L364] [src/digitalmodel/naval_architecture/hull_girder_smith.py:L39-L47] |
| DNV-OS-E301; no governing calculation clause established here | Mentions in EN400 wrapper documentation describe a citation-interface precedent, not a mooring design calculation | [src/digitalmodel/naval_architecture/fundamentals.py:L52-L63] [src/digitalmodel/naval_architecture/hydrostatics.py:L36-L47] [src/digitalmodel/naval_architecture/stability.py:L41-L54] [src/digitalmodel/naval_architecture/resistance.py:L61-L72] [src/digitalmodel/naval_architecture/seakeeping.py:L68-L79] |
| OCIMF, Prediction of Wind and Current Loads on VLCCs, second edition (1994); clause absent in code | `current_yaw_moment_ocimf` computes current yaw moment; `current_lateral_force_ocimf` computes current lateral force; supplied coefficients, no coefficient table reproduced | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L28-L36] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L253-L266] |
| Wind threshold balance | `threshold_speed_for_steerage` equates wind loading with rudder authority and solves speed | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L211-L246] |
| Wind heel | `floating_platform_stability.compute_wind_heel` converts supplied wind pressure to force and balances moment against righting moment, with sine clamping | [src/digitalmodel/naval_architecture/floating_platform_stability.py:L209-L252] |
| Whicker & Fehlner (1958), rudder lift and force | `maneuverability.rudder_lift_coefficient` and `rudder_normal_force` evaluate the lift relation and dynamic-pressure force; `rudder_lift_slope_per_rad` evaluates the slope separately | [src/digitalmodel/naval_architecture/maneuverability.py:L1-L56] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L144-L154] |
| Current heading balance | `critical_current_speed_for_heading` directly inverts the current-moment relation after obtaining rudder authority; `rudder_angle_to_hold_heading` inverts rudder angle | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L279-L336] |
| Natural periods and linear motion screen | `natural_heave_period` uses mass/waterplane stiffness; `simple_heave_rao` is a scalar oscillator magnitude; spectral response uses rectangular quadrature, without phase | [src/digitalmodel/naval_architecture/seakeeping.py:L55-L65] [src/digitalmodel/naval_architecture/seakeeping.py:L146-L193] |
| Scantling and compliance screens | Representative location coefficients and simplified compliance expressions coexist with explicit minimum plate/section checks | [src/digitalmodel/naval_architecture/scantlings.py:L3-L42] [src/digitalmodel/naval_architecture/compliance.py:L19-L42] |

The code names IACS UR S11 Rev.7 (2010), while the codes register assigns 2020 to the same module; this is an edition discrepancy, issue pending (1). [src/digitalmodel/naval_architecture/hull_girder_strength.py:L60-L68] [docs/domains/codes-register.md:L123-L130]

For the licensed-standard references above, clause text not checked against the licensed copy; code locators identify implementation evidence rather than reproduce tabulated requirements.

The EN400 `_cited` interfaces emit optional sidecars: an unconfigured resolver warns once and returns no citations, while other resolution errors propagate. [src/digitalmodel/naval_architecture/fundamentals.py:L52-L88] [src/digitalmodel/naval_architecture/hydrostatics.py:L36-L72]

UNVERIFIED: complete edition/clause reconciliation for ITTC, IACS, IMO, OCIMF and DNV references, including downstream buckling references, remains open; issue pending (1).

## 4. Validated worked examples

Execution record: BASE and AFTER results below use Python pytest with source path set to `src`, `uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`, unchanged repository addopts, at the code revision in the header.

### Closed-form validation cases

| Case | Independent expectation / input source | Expected quantity and tolerance | Existing test node ID | BASE / AFTER |
| --- | --- | --- | --- | --- |
| V1 box buoyancy balance | Rectangular-prism geometry and weight sum; test fixtures define geometry and four weights | Sum 6000.0 t; displaced volume times density equals weight to relative 1e-12, dimensionless; draft equals volume / plan area | `tests/naval_architecture/test_loading_computer.py::test_equilibrium_balances_weight_and_buoyancy` | PASS / PASS [tests/naval_architecture/test_loading_computer.py:L30-L66] |
| V2 uniform spectral response | Constant unit RAO and unit spectral density over ten bins of 0.1 rad/s; direct integral gives variance 1.0 m² | Significant amplitude 2.0 m; absolute error < 0.01 m | `tests/naval_architecture/test_seakeeping.py::TestSpectralAnalysis::test_significant_motion` | PASS / PASS [tests/naval_architecture/test_seakeeping.py:L147-L159] |

These cases validate closed-form relationships for their synthetic inputs; neither establishes regulatory approval or a complete seakeeping solution. [tests/naval_architecture/test_loading_computer.py:L49-L66] [tests/naval_architecture/test_seakeeping.py:L147-L159]

### Regression examples

| Case | Source and input | Expected quantity / tolerance | Existing test node ID | BASE / AFTER |
| --- | --- | --- | --- | --- |
| R1 buoyancy example | EN400-labelled in-test inputs; published answer not independently opened | 1280.0 lb for supplied volume/water selector; absolute error < 1.0 lb | `tests/naval_architecture/test_en400_worked_examples.py::TestEN400Ch2HullFormGeometry::test_example_2_1_buoyant_force` | PASS / PASS [tests/naval_architecture/test_en400_worked_examples.py:L64-L71] |
| R2 righting-arm correction | EN400-labelled in-test cross-curve inputs at zero heel | 0.0 ft; absolute error < 0.15 ft | `tests/naval_architecture/test_en400_worked_examples.py::TestEN400Ch4Stability::test_righting_arm_gz_correction[heel_0]` | PASS / PASS [tests/naval_architecture/test_en400_worked_examples.py:L177-L199] |
| R3 JSON router round trip | Synthetic `_settings()` fixture and temporary output directory | Summary displacement 6000.0 t with pytest default approximation; finite GM and matching status | `tests/naval_architecture/test_loading_computer.py::test_router_writes_summary_and_stamps_status` | PASS / PASS [tests/naval_architecture/test_loading_computer.py:L254-L271] |

UNVERIFIED: EN400-labelled answers and approximate Holtrop fixture targets have not been qualified against an independently opened publication; EN400 coverage is issue pending (1), Holtrop reference qualification is #2020. [tests/naval_architecture/test_en400_worked_examples.py:L3-L13] [tests/naval_architecture/test_holtrop_reproduction.py:L19-L28]

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations: G0 inputs/units/citations, G1 imports/tests, G2 hand or published check; G3 and G4 NOT_APPLICABLE for this offline calculation scope; G5 evidence applies to every family. (AQG r0, sections 9–11.)

The checks below are acceptance instructions; tolerances are local check criteria, not standard tabulations.

| Check | Gate | Executable route and numeric criterion with unit | Evidence / current result |
| --- | --- | --- | --- |
| C0 | G0 | `test_overload_sinks_the_box_raises`: `solve_equilibrium` rejects the overloaded test box; uncaught invalid-input cases = 0 cases | Pytest node; BASE PASS [tests/naval_architecture/test_loading_computer.py:L86-L94] |
| C1 | G0 | `test_permeability_must_be_valid`: `lost_buoyancy_sinkage` rejects 1.5 permeability, dimensionless; missed rejection = 0 cases | Pytest node; BASE PASS [tests/naval_architecture/test_damage_stability.py:L158-L164] |
| C2 | G1 | Import and execute `tests/naval_architecture/`; failed tests = 0 cases and collection errors = 0 modules | Full-domain collection has 3 errors; separate file batches retain the results available without the missing test plugin; environment limitation in section 10 |
| C3 | G2 | V1 `solve_equilibrium`: absolute buoyancy/weight relative error ≤ 1e-12, dimensionless | V1 node; BASE PASS [tests/naval_architecture/test_loading_computer.py:L49-L66] |
| C4 | G2 | V2 `significant_motion`: error < 0.01 m against constant-spectrum integral | V2 node; BASE PASS [tests/naval_architecture/test_seakeeping.py:L147-L159] |
| C5 | G5 | R3 `router`: exactly 1 readable summary JSON file, 0 missing required result/status fields; retain node result and input fingerprint in review evidence | R3 node verifies JSON and status; BASE PASS for the file/status assertions; evidence packaging remains issue pending (1) [tests/naval_architecture/test_loading_computer.py:L254-L271] |
| C6 | G2 | Audit `natural_roll_period` at equal beam/GM and two different positive `c_roll` values; expected coefficient-dependent response; parameter uses in executable body ≥ 1 use | Static code check detects 0 uses; issue pending (2), no shipped detection check exists [src/digitalmodel/naval_architecture/seakeeping.py:L32-L52] |
| C7 | G0 | Audit `flooded_gm` and platform `compute_gm` dimensions: each BM or GM increment must have length exponent 1, unit m; unresolved mass dimension = 0 factors | Static dimensional check finds tonnes in the first denominator and a dimensionless area-power/volume term in the second; issue pending (3), no shipped detection check exists [src/digitalmodel/naval_architecture/damage_stability.py:L173-L182] [src/digitalmodel/naval_architecture/floating_platform_stability.py:L112-L147] |
| C8 | G5 | Recompute section-9 inventory and grep; missing inventory rows = 0 rows; further unlisted in-package implementations ≤ 1 definition | Inventory and definition audit; section 9 |

G3 NOT_APPLICABLE: no transient solver job is part of these Python calculation checks; G4 NOT_APPLICABLE: the family set uses G2 hand checks here, and no diffraction-program run is claimed. (AQG r0, section 10, Python calculations.)

C5 does not establish an AQG-signed report appendix; retain per-case inputs, hashes, results, deviations and independent review evidence before using the outputs in a report. (AQG r0, sections 9 and 11.)

## 6. Known failure modes and pitfalls

| Symptom / cause | Detection | Incident or open record | Date / evidence |
| --- | --- | --- | --- |
| Silent empty citation list when the resolver is unconfigured; other citation errors raise | Wrapper warning/error tests in the main batch; inspect returned `citations` count before relying on provenance | issue pending (1) | 2026-10-15 [src/digitalmodel/naval_architecture/fundamentals.py:L52-L88] |
| Density lookup clamps outside its table; buoyancy water selectors treat any non-saltwater string as fresh water | Input review C0/C1 pattern; density boundary tests cover clamping, but no selector-typo rejection check exists | issue pending (1) | 2026-10-15 [src/digitalmodel/naval_architecture/fundamentals.py:L91-L125] [tests/naval_architecture/test_na_fundamentals.py:L72-L81] |
| Existing curves-of-form example is expected to fail because the required ship table is unavailable | Existing worked-example node records XFAIL; never count it as validation | #1751 | 2026-10-15 [tests/naval_architecture/test_en400_worked_examples.py:L115-L125] |
| `c_roll` is accepted and documented but the executable roll-period body uses a fixed beam multiplier | C6; no shipped detection check exists | issue pending (2) | 2026-10-15 [src/digitalmodel/naval_architecture/seakeeping.py:L32-L52] |
| `flooded_gm` divides lost inertia in m⁴ by displacement in tonnes; the existing test repeats that expression, without a density conversion | C7; no shipped detection check exists; do not use this helper as an independent hand reference | issue pending (3) | 2026-10-15 [src/digitalmodel/naval_architecture/damage_stability.py:L173-L182] [tests/naval_architecture/test_damage_stability.py:L166-L178] |
| Platform `compute_gm` adds an area-to-the-three-halves / volume term, which is dimensionless, to lengths; it cannot be treated as a geometric BM reference | C7; no shipped dimensional detection check exists | issue pending (3) | 2026-10-15 [src/digitalmodel/naval_architecture/floating_platform_stability.py:L112-L147] |
| Approximate Holtrop fixture values are not a qualified physical oracle; diagnostic tests explicitly preserve their unverified status | `test_fixed_speed_diagnostics_preserve_reference_status_and_components`; no independent published-value detection check exists | #2020 | 2026-10-15 [tests/naval_architecture/test_holtrop_reproduction.py:L19-L28] |
| Positive Reynolds number alone does not avoid the ITTC singularity; SI wrapper uses a stricter domain check | Compare input guard in `resistance.ittc_1957_cf` with `friction_scaling._check_re`; no legacy-API singularity detection check established here | issue pending (5) | 2026-10-15 [src/digitalmodel/naval_architecture/resistance.py:L51-L58] [src/digitalmodel/naval_architecture/friction_scaling.py:L98-L114] |
| Documentation calls the Smith implementation a future step, but the package already contains `smith_ultimate` | Compare the validation record with the implementation; no shipped documentation freshness check exists | issue pending (1) | 2026-10-15 [docs/domains/hull-girder-strength-validation-2026-06-28.md:L44-L57] [src/digitalmodel/naval_architecture/hull_girder_smith.py:L357-L418] |

No incident issue was supplied as a mandatory incident list; the confirmed findings above are separated from feature requests and historical issue descriptions.

## 7. Lessons log

Append-only record format: date · work reference · lesson · evidence · pack change · shared-log entry.

| Date | Work reference | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-10-15 | Initial pack | Preserve the two opposite trim conventions at API boundaries | [src/digitalmodel/naval_architecture/loading_computer.py:L223-L242] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L39-L42] | Section 2 warning | Pending; issue pending (1) |
| 2026-10-15 | Initial pack | An expected-value assertion repeating a dimensionally inconsistent expression is regression evidence only | [tests/naval_architecture/test_damage_stability.py:L166-L178] | Sections 5–6 exclude it from hand references | Pending; issue pending (3) |

## 8. Ownership

Proposed roles: engineering calculations quality manager; deputy: engineering calculations deputy quality manager; producing discipline: naval architecture calculations; escalation: the family quality manager decides method and classification disputes, and the maintainer assigns cross-family work.

UNVERIFIED: named role appointments and a shared-log entry have not been established from repository evidence; issue pending (1).

## 9. Dependencies

Upstream data are supplied hydrostatic/GZ/KN tables or declared geometry and loading inputs; the public fixture examples and box-hull test inputs are the reproducible data basis for the cases above. [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L7-L24] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L7-L15] [tests/naval_architecture/test_loading_computer.py:L30-L66]

The downstream hull-resistance adapter consumes Holtrop resistance; the maneuvering-envelope composition consumes the separate propeller/rudder force implementation; hull-girder compression capacity delegates to the structural solver. [src/digitalmodel/naval_architecture/workflow_calculations.py:L132-L169] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L5-L12] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L350-L364]

Dependency inventory identifiers: `naval_architecture` (engineering calculations); `hydrodynamics/propeller_rudder` and `structural/structural_analysis` require inventory-row reconciliation in issue pending (1).

### Solver-name occurrences, text-only audit

| Dependency name | Every occurrence in package Python source | Meaning / execution boundary |
| --- | --- | --- |
| GHS | [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L7-L9] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L33-L37] | Source of supplied hydrostatic tables and independent approved stability cross-check; no execution claimed |
| GHS | [src/digitalmodel/naval_architecture/damage_stability_screening.py:L11-L15] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L46-L48] | Source of supplied damaged hydrostatics and governing cross-check; #2089 tracks the separate adapter |
| OpenFOAM | [src/digitalmodel/naval_architecture/hull_ingest.py:L2-L4] [src/digitalmodel/naval_architecture/hull_ingest.py:L1073-L1076] | Target geometry preparation describes an STL/manifest; no solver launch claimed |

No solver or licensed interface was imported, queried or run in this pack preparation; optional CAD readers are distinct from the named solver dependencies.

### Implementation-map inventory and counted-family audit

Map snapshot metadata: commit and data revision are in the header. Scope is the entire package, including all Python subfolders; no subordinate pack is excluded. Inventory below retains all 131 rows of the one family with in-package implementations, including rows outside the package and rows the map classifies as examples or helpers. Row locators below resolve against the code pin; map row membership is the separately pinned snapshot record.

Family ranking by in-package rows: `wind-and-current-force-routes` = 2 rows, map order 7; it is the only counted family. The in-package map rows are `maneuvering_envelope.threshold_speed_for_steerage` and `maneuvering_envelope_workflow.run_maneuvering_envelope_sweep`. [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L211-L246] [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L233-L345]

Exact Python `re` patterns are read from that family's `search_signals` at the map pin, case-sensitive; none matches `naval_architecture` case-insensitively, so none is omitted. Source hits are assigned to the smallest enclosing function/class, with module-level statements grouped as one definition. The exact patterns and classified hit inventory follow.

algorithm:

```text
(?:0\.5|1\s*/\s*2).*(?:rho|density).*(?:cd|drag|area)|(?:wind|current|drag|lift)_forces?|calculate_(?:wind|current)|OCIMF|current_profile|wind_height_coefficient
```

vocabulary:

```text
OCIMF|wind.load|current.load|wind.force|current.force|drag.coefficient|current.profile
```

constants_or_keys:

```text
Cxw|Cyw|CXw|CYw|CXc|CYc|drag_coefficient|lift_coefficient|wind_speed|current_velocity
```

The counted-family grep found 26 matching lines in 12 enclosing definitions, including one module-level group; classification below counts no further unlisted implementation after the section-3 entries.

| Definition / lines | Classification and basis |
| --- | --- |
| `floating_platform_stability.py::compute_wind_heel` | Computes a result directly; explicitly listed in section 3, so not a further unlisted implementation [src/digitalmodel/naval_architecture/floating_platform_stability.py:L231-L232] |
| `maneuverability.py::rudder_lift_coefficient` | Computes a result directly; explicitly listed in section 3, so not a further unlisted implementation [src/digitalmodel/naval_architecture/maneuverability.py:L19-L19] |
| `maneuverability.py::rudder_normal_force` | Computes a result directly; explicitly listed in section 3, so not a further unlisted implementation [src/digitalmodel/naval_architecture/maneuverability.py:L53-L56] |
| `maneuvering_envelope.py::<module>` | Documentation/comments only; no independent computation [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L22-L250] |
| `maneuvering_envelope.py::rudder_lift_slope_per_rad` | Docstring hit; slope implementation also listed in section 3 [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L148-L148] |
| `maneuvering_envelope.py::threshold_speed_for_steerage` | Listed map implementation [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L213-L246] |
| `maneuvering_envelope.py::current_yaw_moment_ocimf` | Computes a result directly; explicitly listed in section 3, so not a further unlisted implementation [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L256-L256] |
| `maneuvering_envelope.py::current_lateral_force_ocimf` | Computes a result directly; explicitly listed in section 3, so not a further unlisted implementation [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L262-L266] |
| `maneuvering_envelope_workflow.py::ManeuveringEnvelopeInput` | Data holder or validation helper [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L109-L110] |
| `maneuvering_envelope_workflow.py::validate_maneuvering_envelope_input` | Data holder or validation helper [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L204-L205] |
| `maneuvering_envelope_workflow.py::_provenance_metadata` | Serialised reference metadata [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L224-L224] |
| `maneuvering_envelope_workflow.py::run_maneuvering_envelope_sweep` | Listed map workflow; delegates calculation functions [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L266-L279] |

| Map row | Implementation identity retained from snapshot | Code-pin locator |
| --- | --- | --- |
| 1 | `examples/demos/gtm/demo_03_deepwater_mudmat_installation.py::calc_splash_zone` | [examples/demos/gtm/demo_03_deepwater_mudmat_installation.py:L312-L324] |
| 2 | `examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py::calc_phase_3_splash` | [examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py:L375-L387] |
| 3 | `examples/demos/gtm/results_store_demo05.py::(module-level statements)` | [examples/demos/gtm/results_store_demo05.py:L51-L54] |
| 4 | `examples/demos/gtm/tests/test_demo_05_results_store.py::_good_case` | [examples/demos/gtm/tests/test_demo_05_results_store.py:L80-L92] |
| 5 | `examples/domains/fpso/fpso_mooring_analysis.py::calculate_environmental_forces` | [examples/domains/fpso/fpso_mooring_analysis.py:L194-L206] |
| 6 | `examples/domains/fpso/fpso_mooring_analysis.py::main` | [examples/domains/fpso/fpso_mooring_analysis.py:L497-L509] |
| 7 | `examples/domains/ocimf/ocimf_demo.py::main` | [examples/domains/ocimf/ocimf_demo.py:L24-L36] |
| 8 | `examples/domains/workflows/01_spread_mooring_8point.py::calculate_environmental_forces` | [examples/domains/workflows/01_spread_mooring_8point.py:L123-L135] |
| 9 | `examples/domains/workflows/03_calm_buoy.py::analyze_calm_buoy` | [examples/domains/workflows/03_calm_buoy.py:L53-L65] |
| 10 | `examples/domains/workflows/05_jackup_installation.py::analyze_jackup_mooring` | [examples/domains/workflows/05_jackup_installation.py:L37-L49] |
| 11 | `examples/domains/workflows/06_floating_wind_turbine.py::analyze_floating_wind_mooring` | [examples/domains/workflows/06_floating_wind_turbine.py:L37-L49] |
| 12 | `examples/domains/workflows/07_wave_energy_converter.py::analyze_wec_mooring` | [examples/domains/workflows/07_wave_energy_converter.py:L36-L48] |
| 13 | `examples/domains/workflows/08_aquaculture_mooring.py::analyze_aquaculture_mooring` | [examples/domains/workflows/08_aquaculture_mooring.py:L35-L47] |
| 14 | `examples/domains/workflows/09_spm_terminal.py::analyze_spm_terminal` | [examples/domains/workflows/09_spm_terminal.py:L34-L46] |
| 15 | `examples/domains/workflows/10_dp_backup_mooring.py::analyze_dp_backup_mooring` | [examples/domains/workflows/10_dp_backup_mooring.py:L36-L48] |
| 16 | `scripts/capabilities/build_onepagers.py::(module-level statements)` | [scripts/capabilities/build_onepagers.py:L36-L39] |
| 17 | `scripts/python/digitalmodel/legacy/extract_ocimf_database.py::(module-level statements)` | [scripts/python/digitalmodel/legacy/extract_ocimf_database.py:L25-L28] |
| 18 | `scripts/python/digitalmodel/legacy/extract_ocimf_database.py::extract_ocimf_database` | [scripts/python/digitalmodel/legacy/extract_ocimf_database.py:L220-L232] |
| 19 | `scripts/python/digitalmodel/legacy/generate_integration_charts.py::(module-level statements)` | [scripts/python/digitalmodel/legacy/generate_integration_charts.py:L40-L43] |
| 20 | `scripts/python/digitalmodel/legacy/generate_ocimf_report.py::main` | [scripts/python/digitalmodel/legacy/generate_ocimf_report.py:L16-L28] |
| 21 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py::PerformanceProfiler._setup_test_data` | [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L112-L119] |
| 22 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py::PerformanceProfiler.profile_ocimf_lookup` | [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L196-L208] |
| 23 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py::PerformanceProfiler.run_all` | [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L499-L511] |
| 24 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py::main` | [scripts/python/digitalmodel/legacy/profile_marine_modules.py:L518-L530] |
| 25 | `scripts/python/digitalmodel/legacy/validate_phase2.py::Phase2Validator.generate_excel_reference_data` | [scripts/python/digitalmodel/legacy/validate_phase2.py:L104-L116] |
| 26 | `scripts/python/digitalmodel/legacy/validate_phase2.py::Phase2Validator.export_statistics_to_csv` | [scripts/python/digitalmodel/legacy/validate_phase2.py:L853-L865] |
| 27 | `scripts/python/digitalmodel/legacy/validate_phase2.py::Phase2Validator.export_json_summary` | [scripts/python/digitalmodel/legacy/validate_phase2.py:L1191-L1203] |
| 28 | `scripts/python/digitalmodel/legacy/validate_phase2.py::Phase2Validator.run_complete_validation` | [scripts/python/digitalmodel/legacy/validate_phase2.py:L1216-L1228] |
| 29 | `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py::(module-level statements)` | [scripts/python/digitalmodel/legacy/visualize_ocimf_data.py:L22-L25] |
| 30 | `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py::main` | [scripts/python/digitalmodel/legacy/visualize_ocimf_data.py:L371-L383] |
| 31 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py::make_polar_overlay` | [scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py:L406-L418] |
| 32 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py::parse_args` | [scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py:L802-L814] |
| 33 | `src/digitalmodel/ansys/pressure_vessel.py::PressureVesselGenerator.generate_wind_load` | [src/digitalmodel/ansys/pressure_vessel.py:L704-L716] |
| 34 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py::SyntheticRopeDatabaseClient._generate_rope_spec` | [src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py:L171-L183] |
| 35 | `src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py::main` | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L367-L379] |
| 36 | `src/digitalmodel/drilling_riser/drift_off.py::environmental_force_n` | [src/digitalmodel/drilling_riser/drift_off.py:L97-L109] |
| 37 | `src/digitalmodel/drilling_riser/envelope.py::CurrentProfile.drag_load_n_per_m` | [src/digitalmodel/drilling_riser/envelope.py:L108-L119] |
| 38 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py::as_analysed_sections` | [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L248-L260] |
| 39 | `src/digitalmodel/dynmoor_mooring/workflow.py::router` | [src/digitalmodel/dynmoor_mooring/workflow.py:L101-L113] |
| 40 | `src/digitalmodel/engine.py::engine` | [src/digitalmodel/engine.py:L90-L102] |
| 41 | `src/digitalmodel/hydrodynamics/cli.py::ocimf_wind_cmd` | [src/digitalmodel/hydrodynamics/cli.py:L115-L127] |
| 42 | `src/digitalmodel/hydrodynamics/cli.py::ocimf_current_cmd` | [src/digitalmodel/hydrodynamics/cli.py:L173-L185] |
| 43 | `src/digitalmodel/hydrodynamics/cli.py::combined_env_cmd` | [src/digitalmodel/hydrodynamics/cli.py:L229-L241] |
| 44 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py::FatigueAnalysisComponents.get_total_viv_fatigue` | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L99-L111] |
| 45 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py::FatigueAnalysisComponents.tabulate_viv_results` | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L114-L126] |
| 46 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py::VIVFatigueAnalysisComponents.get_total_viv_fatigue` | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L176-L188] |
| 47 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py::VIVFatigueAnalysisComponents.tabulate_viv_results` | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L200-L212] |
| 48 | `src/digitalmodel/infrastructure/utils/ETL_components.py::ETL_components.transform_viv_current_reformat_1` | [src/digitalmodel/infrastructure/utils/ETL_components.py:L746-L758] |
| 49 | `src/digitalmodel/installation/installation_pamphlet/calculations.py::compute_splashzone_envelope` | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L456-L468] |
| 50 | `src/digitalmodel/installation/jumper_installation/calculations.py::calc_phase_3_splash` | [src/digitalmodel/installation/jumper_installation/calculations.py:L163-L175] |
| 51 | `src/digitalmodel/marine_ops/marine_analysis/__main__.py::main` | [src/digitalmodel/marine_ops/marine_analysis/__main__.py:L27-L39] |
| 52 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py::OCIMFDatabase.get_coefficients` | [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L306-L318] |
| 53 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py::EnvironmentalForces.calculate_total_forces` | [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L628-L640] |
| 54 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py::create_sample_database` | [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L861-L873] |
| 55 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py::OCIMFWorkflow.router` | [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py:L48-L60] |
| 56 | `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py::(module-level statements)` | [src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py:L25-L28] |
| 57 | `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py::extract_ocimf_database` | [src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py:L220-L232] |
| 58 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py::PerformanceProfiler._setup_test_data` | [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L110-L117] |
| 59 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py::PerformanceProfiler.profile_ocimf_lookup` | [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L188-L200] |
| 60 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py::PerformanceProfiler.run_all` | [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L494-L506] |
| 61 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py::main` | [src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py:L513-L525] |
| 62 | `src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py::main` | [src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py:L644-L656] |
| 63 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py::Phase2Validator.generate_excel_reference_data` | [src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py:L104-L116] |
| 64 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py::Phase2Validator.export_statistics_to_csv` | [src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py:L853-L865] |
| 65 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py::Phase2Validator.export_json_summary` | [src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py:L1191-L1203] |
| 66 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py::Phase2Validator.run_complete_validation` | [src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py:L1216-L1228] |
| 67 | `src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py::(module-level statements)` | [src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py:L40-L43] |
| 68 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py::_output_dir` | [src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py:L25-L31] |
| 69 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py::_cli_output_dir` | [src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py:L34-L36] |
| 70 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py::load_data` | [src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py:L55-L64] |
| 71 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py::main` | [src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py:L448-L460] |
| 72 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py::OCIMFDatabase.get_coefficients` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L320-L332] |
| 73 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py::EnvironmentalForces.calculate_total_forces` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L642-L654] |
| 74 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py::create_sample_database` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L875-L887] |
| 75 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py::FPSOMooringFullWorkflow._environmental_forces` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L77-L89] |
| 76 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py::FPSOMooringFullWorkflow._ocimf_database_path` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L103-L113] |
| 77 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py::_resolve_arrow_direction_in_body_frame` | [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L39-L51] |
| 78 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py::polar_force_overlay` | [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L273-L285] |
| 79 | `src/digitalmodel/naval_architecture/maneuvering_envelope.py::threshold_speed_for_steerage` | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L211-L223] |
| 80 | `src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py::run_maneuvering_envelope_sweep` | [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L233-L245] |
| 81 | `src/digitalmodel/orcaflex/installation_analysis.py::SplashZoneInput.calculate_splash_zone_loads` | [src/digitalmodel/orcaflex/installation_analysis.py:L245-L257] |
| 82 | `src/digitalmodel/orcawave/drift_forces.py::compute_wind_current_drift` | [src/digitalmodel/orcawave/drift_forces.py:L224-L236] |
| 83 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py::KnowledgeIntegrator._extract_configurations` | [src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py:L104-L116] |
| 84 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py::EnvironmentBuilder.build` | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py:L171-L183] |
| 85 | `src/digitalmodel/solvers/orcaflex/reporting/extractors/loads_extractor.py::extract_loads` | [src/digitalmodel/solvers/orcaflex/reporting/extractors/loads_extractor.py:L12-L24] |
| 86 | `src/digitalmodel/structural/parachute/chute_assessment.py::calculate_dual_chute_drag` | [src/digitalmodel/structural/parachute/chute_assessment.py:L81-L93] |
| 87 | `src/digitalmodel/structural/parachute/chute_assessment.py::assess_all_load_cases` | [src/digitalmodel/structural/parachute/chute_assessment.py:L131-L143] |
| 88 | `src/digitalmodel/structural/parachute/chute_assessment.py::simulate_deployment_transient` | [src/digitalmodel/structural/parachute/chute_assessment.py:L189-L201] |
| 89 | `src/digitalmodel/structural/parachute/chute_assessment.py::export_results_yaml` | [src/digitalmodel/structural/parachute/chute_assessment.py:L296-L308] |
| 90 | `src/digitalmodel/structural/parachute/parachute_drag.py::calculate_drag_force` | [src/digitalmodel/structural/parachute/parachute_drag.py:L37-L49] |
| 91 | `src/digitalmodel/subsea/mooring_analysis/cli.py::design_verification` | [src/digitalmodel/subsea/mooring_analysis/cli.py:L159-L171] |
| 92 | `src/digitalmodel/subsea/mooring_analysis/designer.py::MooringDesigner.calculate_environmental_loads` | [src/digitalmodel/subsea/mooring_analysis/designer.py:L60-L72] |
| 93 | `src/digitalmodel/subsea/mooring_analysis/dynmoor.py::build_wind_model.force` | [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L1094-L1096] |
| 94 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py::_phase_demand` | [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L194-L205] |
| 95 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py::hydrodynamic_loads` | [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L171-L183] |
| 96 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py::drag_force_per_meter` | [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L214-L226] |
| 97 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py::lateral_stability_check` | [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L291-L303] |
| 98 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py::check_lateral_stability` | [src/digitalmodel/subsea/on_bottom_stability/simplified.py:L363-L375] |
| 99 | `src/digitalmodel/subsea/pipeline/integrity_workflows.py::run_on_bottom_stability` | [src/digitalmodel/subsea/pipeline/integrity_workflows.py:L7-L19] |
| 100 | `src/digitalmodel/subsea/viv_analysis/screening.py::VIVScreening.screen_with_current_profile` | [src/digitalmodel/subsea/viv_analysis/screening.py:L278-L290] |
| 101 | `src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py::viv_analysis` | [src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py:L52-L64] |
| 102 | `src/digitalmodel/subsea/viv_analysis/vortex_shedding.py::VortexSheddingAnalyzer.analyze_with_profile` | [src/digitalmodel/subsea/viv_analysis/vortex_shedding.py:L160-L172] |
| 103 | `src/digitalmodel/workflows/workflow_automation/templates.py::create_mooring_design_template` | [src/digitalmodel/workflows/workflow_automation/templates.py:L306-L318] |
| 104 | `src/digitalmodel/workflows/workflow_automation/workflows.py::MooringSystemDesignWorkflow.create` | [src/digitalmodel/workflows/workflow_automation/workflows.py:L169-L181] |
| 105 | `tests/contracts/test_force_units_boundary_contract.py::(module-level statements)` | [tests/contracts/test_force_units_boundary_contract.py:L52-L55] |
| 106 | `tests/legal/test_published_pages_have_no_internal_paths.py::_load_producer` | [tests/legal/test_published_pages_have_no_internal_paths.py:L148-L156] |
| 107 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py::sample_database_path` | [tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py:L31-L43] |
| 108 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py::ocimf_db` | [tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py:L51-L53] |
| 109 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py::env_forces` | [tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py:L57-L59] |
| 110 | `tests/marine_ops/marine_engineering/integration/integration_example.py::main` | [tests/marine_ops/marine_engineering/integration/integration_example.py:L32-L44] |
| 111 | `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py::TestOCIMFMooringIntegration.test_combined_environmental_forces` | [tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py:L105-L117] |
| 112 | `tests/marine_ops/marine_engineering/test_marine_eng_perf.py::ocimf_database` | [tests/marine_ops/marine_engineering/test_marine_eng_perf.py:L33-L39] |
| 113 | `tests/marine_ops/marine_engineering/test_marine_eng_perf.py::pytest_benchmark_update_json` | [tests/marine_ops/marine_engineering/test_marine_eng_perf.py:L364-L373] |
| 114 | `tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py::test_refactored_polar_signatures_match_fixture` | [tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py:L72-L84] |
| 115 | `tests/solvers/orcaflex/reporting/fixture_helpers.py::_build_loads` | [tests/solvers/orcaflex/reporting/fixture_helpers.py:L161-L173] |
| 116 | `tests/structural/parachute/test_chute_assessment.py::TestAssessAllLoadCases.test_governing_force_within_1_percent_of_hand_calc` | [tests/structural/parachute/test_chute_assessment.py:L136-L148] |
| 117 | `tests/structural/parachute/test_parachute_drag.py::TestDragForce.test_drag_200_mph` | [tests/structural/parachute/test_parachute_drag.py:L70-L80] |
| 118 | `tests/structural/parachute/test_parachute_drag.py::TestDragForce.test_drag_250_mph` | [tests/structural/parachute/test_parachute_drag.py:L82-L91] |
| 119 | `tests/structural/parachute/test_parachute_drag.py::TestDragForce.test_force_scales_with_v_squared` | [tests/structural/parachute/test_parachute_drag.py:L98-L103] |
| 120 | `tests/structural/parachute/test_parachute_drag.py::TestDragForce.test_result_has_newtons` | [tests/structural/parachute/test_parachute_drag.py:L105-L108] |
| 121 | `tests/structural/parachute/test_parachute_drag.py::TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_newtons` | [tests/structural/parachute/test_parachute_drag.py:L173-L185] |
| 122 | `tests/structural/parachute/test_parachute_drag.py::TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_lbs` | [tests/structural/parachute/test_parachute_drag.py:L188-L200] |
| 123 | `tests/subsea/catenary_riser/conftest.py::catenary_environment` | [tests/subsea/catenary_riser/conftest.py:L19-L30] |
| 124 | `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestLiftForce.test_known_lift_force` | [tests/subsea/on_bottom_stability/test_dnv_rp_f109.py:L103-L110] |
| 125 | `tests/subsea/pipeline/test_on_bottom_stability.py::TestDragForce.test_scenario_1` | [tests/subsea/pipeline/test_on_bottom_stability.py:L72-L78] |
| 126 | `tests/subsea/pipeline/test_on_bottom_stability.py::TestLiftForce.test_scenario_1` | [tests/subsea/pipeline/test_on_bottom_stability.py:L82-L88] |
| 127 | `tests/test_automation/fixtures/engineering_fixtures.py::get_dnv_circular_mock` | [tests/test_automation/fixtures/engineering_fixtures.py:L145-L157] |
| 128 | `tests/test_automation/fixtures/engineering_fixtures.py::get_dnv_rectangular_mock` | [tests/test_automation/fixtures/engineering_fixtures.py:L170-L182] |
| 129 | `tests/test_on_bottom_stability.py::TestHydrodynamicLoads.test_drag_force_per_meter` | [tests/test_on_bottom_stability.py:L54-L66] |
| 130 | `tests/test_on_bottom_stability.py::TestHydrodynamicLoads.test_lift_force_per_meter` | [tests/test_on_bottom_stability.py:L68-L80] |
| 131 | `tests/workflows/test_durable_workflows.py::test_workflow_registry` | [tests/workflows/test_durable_workflows.py:L52-L64] |

### Whole-repository follow-up

The map currently has no in-package rows for hydrostatic integration, GZ/damage criteria, hull resistance/transfer, natural-period/spectral response, load-curve strength, hull-girder ultimate strength or propeller/maneuverability methods as separate families; these require map additions before becoming counted families, issue pending (4).

Repository-wide inspection includes `scripts/` and the full 131-row family inventory above; alternate force calculators and the legacy/current marine-analysis routes are map follow-up work rather than additional in-package misses, issue pending (4). [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L628-L640] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L642-L654] [src/digitalmodel/subsea/mooring_analysis/designer.py:L60-L72]

The repository search also finds independent ITTC friction calculations outside this package; their addition to the map is follow-up work, issue pending (4). [src/digitalmodel/solvers/openfoam/validation/ship_resistance.py:L116-L129] [src/digitalmodel/solvers/openfoam/validation/double_body_form_factor.py:L140-L148]

The resistance diagnostic in `scripts/` delegates to the package Holtrop implementation and normalises its component results; it is a diagnostic helper rather than an independent Holtrop implementation. [scripts/validation/reproduce_holtrop_discrepancy.py:L24-L51]

## 10. Coverage and open items

Coverage record: the main-path files and references named above were read at the code pin; all package Python files were enumerated and parsed for the family audit. Full line-by-line coverage of the remaining geometry, transfer, catalog and structural branches is deferred to issue pending (1).

### Source coverage ledger

Full main-path reads, targeted reads and enumeration-only coverage are distinguished below; the last two states retain the detailed-review gap in issue pending (1).

| Package-relative Python file | Read coverage at the pin |
| --- | --- |
| `__init__.py` | Full main-path read [src/digitalmodel/naval_architecture/__init__.py:L1-L8] |
| `appendage_submergence.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/appendage_submergence.py:L1-L8] |
| `brep_tessellate.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/brep_tessellate.py:L1-L8] |
| `compliance.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/compliance.py:L1-L8] |
| `constants.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/constants.py:L1-L8] |
| `curves_of_form.py` | Full main-path read [src/digitalmodel/naval_architecture/curves_of_form.py:L1-L8] |
| `damage_stability.py` | Full main-path read [src/digitalmodel/naval_architecture/damage_stability.py:L1-L8] |
| `damage_stability_screening.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/damage_stability_screening.py:L1-L8] |
| `data/__init__.py` | Full main-path read [src/digitalmodel/naval_architecture/data/__init__.py:L1-L2] |
| `floating_platform_stability.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/floating_platform_stability.py:L1-L8] |
| `friction_scaling.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/friction_scaling.py:L1-L8] |
| `fundamentals.py` | Full main-path read [src/digitalmodel/naval_architecture/fundamentals.py:L1-L8] |
| `gyradius.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/gyradius.py:L1-L8] |
| `holtrop_coefficients.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/holtrop_coefficients.py:L1-L8] |
| `holtrop_mennen.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/holtrop_mennen.py:L1-L8] |
| `hull_cap.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_cap.py:L1-L8] |
| `hull_fixtures.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_fixtures.py:L1-L8] |
| `hull_form.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_form.py:L1-L8] |
| `hull_girder_screening.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_girder_screening.py:L1-L8] |
| `hull_girder_smith.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/hull_girder_smith.py:L1-L8] |
| `hull_girder_strength.py` | Full main-path read [src/digitalmodel/naval_architecture/hull_girder_strength.py:L1-L8] |
| `hull_ingest.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/hull_ingest.py:L1-L8] |
| `hull_properties.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_properties.py:L1-L8] |
| `hull_regions.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_regions.py:L1-L8] |
| `hydrostatics.py` | Full main-path read [src/digitalmodel/naval_architecture/hydrostatics.py:L1-L8] |
| `integration.py` | Full main-path read [src/digitalmodel/naval_architecture/integration.py:L1-L8] |
| `kcs_geometry.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/kcs_geometry.py:L1-L8] |
| `knowledge.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/knowledge.py:L1-L8] |
| `loading_computer.py` | Full main-path read [src/digitalmodel/naval_architecture/loading_computer.py:L1-L8] |
| `maneuverability.py` | Full main-path read [src/digitalmodel/naval_architecture/maneuverability.py:L1-L8] |
| `maneuvering_envelope.py` | Full main-path read [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L1-L8] |
| `maneuvering_envelope_workflow.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L1-L8] |
| `mesh_clipping.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/mesh_clipping.py:L1-L8] |
| `mesh_geometry.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/mesh_geometry.py:L1-L8] |
| `mesh_hydrostatics.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/mesh_hydrostatics.py:L1-L8] |
| `mesh_results.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/mesh_results.py:L1-L8] |
| `mesh_sections.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/mesh_sections.py:L1-L8] |
| `mesh_topology.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/mesh_topology.py:L1-L8] |
| `mesh_validation.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/mesh_validation.py:L1-L8] |
| `prohaska.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/prohaska.py:L1-L8] |
| `propeller.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/propeller.py:L1-L8] |
| `region_report.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/region_report.py:L1-L8] |
| `resistance.py` | Full main-path read [src/digitalmodel/naval_architecture/resistance.py:L1-L8] |
| `rudder_stock_torque.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/rudder_stock_torque.py:L1-L8] |
| `scantlings.py` | Full main-path read [src/digitalmodel/naval_architecture/scantlings.py:L1-L8] |
| `seakeeping.py` | Full main-path read [src/digitalmodel/naval_architecture/seakeeping.py:L1-L8] |
| `ship_data.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/ship_data.py:L1-L8] |
| `ship_dimensions.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/ship_dimensions.py:L1-L8] |
| `solid_occlusion.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/solid_occlusion.py:L1-L8] |
| `stability.py` | Full main-path read [src/digitalmodel/naval_architecture/stability.py:L1-L8] |
| `submarines.py` | Full main-path read [src/digitalmodel/naval_architecture/submarines.py:L1-L8] |
| `symmetry_cut.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/symmetry_cut.py:L1-L8] |
| `trim_hydrostatics.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/trim_hydrostatics.py:L1-L8] |
| `turning_circle.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/turning_circle.py:L1-L8] |
| `turning_circle_artifacts.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/turning_circle_artifacts.py:L1-L8] |
| `vessel_stability_screening.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L1-L8] |
| `wigley_geometry.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/wigley_geometry.py:L1-L8] |
| `workflow.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/workflow.py:L1-L8] |
| `workflow_calculations.py` | Full main-path read [src/digitalmodel/naval_architecture/workflow_calculations.py:L1-L8] |
| `yaw_moment.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/yaw_moment.py:L1-L8] |

### Test inventory

| Test file | BASE / AFTER evidence |
| --- | --- |
| `tests/naval_architecture/test_alt_fuel_ship_sizing.py` | PASSED 38; identical outcome set AFTER [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L1-L8] |
| `tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py` | PASSED 14; identical outcome set AFTER [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L1-L8] |
| `tests/naval_architecture/test_array_integrity.py` | PASSED 14; identical outcome set AFTER [tests/naval_architecture/test_array_integrity.py:L1-L8] |
| `tests/naval_architecture/test_brep_tessellate.py` | Collection SKIP: needs the 'cad' extra [tests/naval_architecture/test_brep_tessellate.py:L1-L8] |
| `tests/naval_architecture/test_compliance.py` | PASSED 9; identical outcome set AFTER [tests/naval_architecture/test_compliance.py:L1-L8] |
| `tests/naval_architecture/test_damage_stability.py` | PASSED 12; identical outcome set AFTER [tests/naval_architecture/test_damage_stability.py:L1-L8] |
| `tests/naval_architecture/test_damage_stability_screening.py` | PASSED 21; identical outcome set AFTER [tests/naval_architecture/test_damage_stability_screening.py:L1-L8] |
| `tests/naval_architecture/test_en400_worked_examples.py` | PASSED 23, XFAIL 1; identical outcome set AFTER [tests/naval_architecture/test_en400_worked_examples.py:L1-L8] |
| `tests/naval_architecture/test_floating_platform_stability.py` | PASSED 21; identical outcome set AFTER [tests/naval_architecture/test_floating_platform_stability.py:L1-L8] |
| `tests/naval_architecture/test_floating_platform_stability_extended.py` | PASSED 70; identical outcome set AFTER [tests/naval_architecture/test_floating_platform_stability_extended.py:L1-L8] |
| `tests/naval_architecture/test_friction_scaling.py` | PASSED 36; identical outcome set AFTER [tests/naval_architecture/test_friction_scaling.py:L1-L8] |
| `tests/naval_architecture/test_gyradius.py` | PASSED 21; identical outcome set AFTER [tests/naval_architecture/test_gyradius.py:L1-L8] |
| `tests/naval_architecture/test_gyradius_extended.py` | PASSED 96; identical outcome set AFTER [tests/naval_architecture/test_gyradius_extended.py:L1-L8] |
| `tests/naval_architecture/test_gz_curves.py` | PASSED 25; identical outcome set AFTER [tests/naval_architecture/test_gz_curves.py:L1-L8] |
| `tests/naval_architecture/test_holtrop_mennen.py` | PASSED 20; identical outcome set AFTER [tests/naval_architecture/test_holtrop_mennen.py:L1-L8] |
| `tests/naval_architecture/test_holtrop_reproduction.py` | PASSED 3; identical outcome set AFTER [tests/naval_architecture/test_holtrop_reproduction.py:L1-L8] |
| `tests/naval_architecture/test_hull_cap.py` | PASSED 31; identical outcome set AFTER [tests/naval_architecture/test_hull_cap.py:L1-L8] |
| `tests/naval_architecture/test_hull_form.py` | PASSED 19; identical outcome set AFTER [tests/naval_architecture/test_hull_form.py:L1-L8] |
| `tests/naval_architecture/test_hull_girder_screening.py` | PASSED 23; identical outcome set AFTER [tests/naval_architecture/test_hull_girder_screening.py:L1-L8] |
| `tests/naval_architecture/test_hull_girder_screening_workflow.py` | PASSED 18; identical outcome set AFTER [tests/naval_architecture/test_hull_girder_screening_workflow.py:L1-L8] |
| `tests/naval_architecture/test_hull_girder_smith.py` | PASSED 11; identical outcome set AFTER [tests/naval_architecture/test_hull_girder_smith.py:L1-L8] |
| `tests/naval_architecture/test_hull_girder_strength.py` | PASSED 26; identical outcome set AFTER [tests/naval_architecture/test_hull_girder_strength.py:L1-L8] |
| `tests/naval_architecture/test_hull_ingest.py` | PASSED 28; identical outcome set AFTER [tests/naval_architecture/test_hull_ingest.py:L1-L8] |
| `tests/naval_architecture/test_hull_regions.py` | PASSED 26; identical outcome set AFTER [tests/naval_architecture/test_hull_regions.py:L1-L8] |
| `tests/naval_architecture/test_hydrostatics_citations.py` | PASSED 4; identical outcome set AFTER [tests/naval_architecture/test_hydrostatics_citations.py:L1-L8] |
| `tests/naval_architecture/test_kcs_geometry.py` | PASSED 12; identical outcome set AFTER [tests/naval_architecture/test_kcs_geometry.py:L1-L8] |
| `tests/naval_architecture/test_knowledge.py` | PASSED 11; identical outcome set AFTER [tests/naval_architecture/test_knowledge.py:L1-L8] |
| `tests/naval_architecture/test_loading_computer.py` | PASSED 13; identical outcome set AFTER [tests/naval_architecture/test_loading_computer.py:L1-L8] |
| `tests/naval_architecture/test_maneuverability.py` | PASSED 22; identical outcome set AFTER [tests/naval_architecture/test_maneuverability.py:L1-L8] |
| `tests/naval_architecture/test_mesh_benchmark.py` | PASSED 10; identical outcome set AFTER [tests/naval_architecture/test_mesh_benchmark.py:L1-L8] |
| `tests/naval_architecture/test_mesh_hydrostatics.py` | PASSED 48; identical outcome set AFTER [tests/naval_architecture/test_mesh_hydrostatics.py:L1-L8] |
| `tests/naval_architecture/test_mesh_pickle_generator.py` | ; identical outcome set AFTER [tests/naval_architecture/test_mesh_pickle_generator.py:L1-L8] |
| `tests/naval_architecture/test_mesh_r02.py` | PASSED 15; identical outcome set AFTER [tests/naval_architecture/test_mesh_r02.py:L1-L8] |
| `tests/naval_architecture/test_na_fundamentals.py` | PASSED 15; identical outcome set AFTER [tests/naval_architecture/test_na_fundamentals.py:L1-L8] |
| `tests/naval_architecture/test_na_fundamentals_citations.py` | PASSED 4; identical outcome set AFTER [tests/naval_architecture/test_na_fundamentals_citations.py:L1-L8] |
| `tests/naval_architecture/test_propeller_4_quadrant.py` | PASSED 59; identical outcome set AFTER [tests/naval_architecture/test_propeller_4_quadrant.py:L1-L8] |
| `tests/naval_architecture/test_resistance_citations.py` | PASSED 4; identical outcome set AFTER [tests/naval_architecture/test_resistance_citations.py:L1-L8] |
| `tests/naval_architecture/test_rudder_stock_torque_sweep.py` | NOT_RUN: collection error, unregistered `xdist_group` marker (missing pytest-xdist) [tests/naval_architecture/test_rudder_stock_torque_sweep.py:L1-L8] |
| `tests/naval_architecture/test_scantlings.py` | PASSED 9; identical outcome set AFTER [tests/naval_architecture/test_scantlings.py:L1-L8] |
| `tests/naval_architecture/test_seakeeping.py` | PASSED 15; identical outcome set AFTER [tests/naval_architecture/test_seakeeping.py:L1-L8] |
| `tests/naval_architecture/test_seakeeping_citations.py` | PASSED 4; identical outcome set AFTER [tests/naval_architecture/test_seakeeping_citations.py:L1-L8] |
| `tests/naval_architecture/test_ship_data.py` | PASSED 11; identical outcome set AFTER [tests/naval_architecture/test_ship_data.py:L1-L8] |
| `tests/naval_architecture/test_ship_dimensions_template.py` | PASSED 9; identical outcome set AFTER [tests/naval_architecture/test_ship_dimensions_template.py:L1-L8] |
| `tests/naval_architecture/test_stability_citations.py` | PASSED 4; identical outcome set AFTER [tests/naval_architecture/test_stability_citations.py:L1-L8] |
| `tests/naval_architecture/test_transfer_citation_regressions.py` | PASSED 30; identical outcome set AFTER [tests/naval_architecture/test_transfer_citation_regressions.py:L1-L8] |
| `tests/naval_architecture/test_trim_hydrostatics.py` | PASSED 20; identical outcome set AFTER [tests/naval_architecture/test_trim_hydrostatics.py:L1-L8] |
| `tests/naval_architecture/test_turning_circle_estimator.py` | NOT_RUN: collection error, unregistered `xdist_group` marker (missing pytest-xdist) [tests/naval_architecture/test_turning_circle_estimator.py:L1-L8] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py` | PASSED 60; identical outcome set AFTER [tests/naval_architecture/test_vessel_fleet_adapter.py:L1-L8] |
| `tests/naval_architecture/test_vessel_stability_screening.py` | PASSED 35; identical outcome set AFTER [tests/naval_architecture/test_vessel_stability_screening.py:L1-L8] |
| `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py` | PASSED 20; identical outcome set AFTER [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L1-L8] |
| `tests/naval_architecture/test_vessel_stability_screening_workflow.py` | PASSED 17; identical outcome set AFTER [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L1-L8] |
| `tests/naval_architecture/test_wigley_geometry.py` | PASSED 4; identical outcome set AFTER [tests/naval_architecture/test_wigley_geometry.py:L1-L8] |
| `tests/naval_architecture/test_yaw_moment_sweep.py` | NOT_RUN: collection error, unregistered `xdist_group` marker (missing pytest-xdist) [tests/naval_architecture/test_yaw_moment_sweep.py:L1-L8] |

### Documentation and durable workflows

The discovered documentation includes the following identity-neutral paths; the coverage status marks which references remain discovery leads, issue pending (1).

| Documentation found by module-name search | Read coverage |
| --- | --- |
| `docs/reviews/hull-library-cross-review.md` | Discovery lead; detailed read open |
| `docs/registry/module-routing.yaml` | Referenced lines read |
| `docs/plans/2026-05-05-issue-483-curves-py-decomposition.md` | Discovery lead; detailed read open |
| `docs/capability-map/capabilities-inventory.json` | Discovery lead; detailed read open |
| `docs/maps/digitalmodel-operator-map.md` | Referenced lines read |
| `docs/capability-map/capabilities-added.yml` | Discovery lead; detailed read open |
| `docs/capability-map/capabilities-ia-spec-1444.md` | Discovery lead; detailed read open |
| `docs/capability-map/capabilities-clusters.yml` | Discovery lead; detailed read open |
| `docs/capability-map/capabilities-sections.yml` | Discovery lead; detailed read open |
| `docs/plans/2026-05-05-issue-461-recover-ship-dimensions-template.md` | Discovery lead; detailed read open |
| `docs/plans/2026-05-05-issue-457-ship-dimensions-template-loader.md` | Discovery lead; detailed read open |
| `docs/domains/hull-girder-strength-validation-2026-06-28.md` | Referenced lines read |
| `docs/plans/2026-09-27-issue-2239-analytical-resistance-methods.md` | Discovery lead; detailed read open |
| `docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md` | Discovery lead; detailed read open |
| `docs/plans/2026-05-07-issue-596-repo-structure-normalization.md` | Discovery lead; detailed read open |
| `docs/domains/ship-design/software_options.puml` | Discovery lead; detailed read open |
| `docs/domains/ship-design/alt-fuel-lh2-sizing.md` | Discovery lead; detailed read open |
| `docs/research/hydrodynamics-academic-references.md` | Discovery lead; detailed read open |
| `docs/domains/ship-design/hull-girder-strength-screening.md` | Discovery lead; detailed read open |
| `docs/domains/ship-design/vessel-stability-screening.md` | Discovery lead; detailed read open |
| `docs/plans/2026-09-11-issue-2089-ghs-canary.html` | Discovery lead; detailed read open |
| `docs/api/structural/ship-resistance-explorer.html` | Discovery lead; detailed read open |
| `docs/plans/2026-05-05-issue-463-ship-dimensions-manual-curation.md` | Discovery lead; detailed read open |
| `docs/plans/2026-09-11-issue-2089-ghs-canary.md` | Discovery lead; detailed read open |
| `docs/vision/CALCULATIONS-VISION.md` | Discovery lead; detailed read open |
| `docs/plans/2026-09-27-issue-2191-parametric-monohull-form.md` | Discovery lead; detailed read open |
| `docs/plans/2026-06-11-issue-700-ci-baseline-domain-gates.md` | Discovery lead; detailed read open |
| `docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md` | Discovery lead; detailed read open |
| `docs/domains/ship-design/stiff_vs_tender_ships.md` | Discovery lead; detailed read open |
| `docs/domains/scantlings-validation-2026-06-28.md` | Referenced lines read |
| `docs/domains/codes-register.md` | Referenced lines read |
| `docs/domains/manoeuvring-rudder-reference.md` | Discovery lead; detailed read open |
| `docs/domains/hull-girder-smith-validation-2026-06-28.md` | Discovery lead; detailed read open |
| `docs/api/hydro/rudder-maneuvering-explorer.html` | Discovery lead; detailed read open |
| `docs/domains/hull_library/hullprod-curvature-screening-evaluation.md` | Discovery lead; detailed read open |
| `docs/domains/marine-engineering/yaw-moment-sweep.md` | Discovery lead; detailed read open |
| `docs/domains/marine-engineering/rudder-stock-torque-sweep.md` | Discovery lead; detailed read open |
| `docs/domains/marine-engineering/marine_3d_models_catalog.md` | Discovery lead; detailed read open |
| `docs/domains/hydrodynamics/propeller-rudder-literature.md` | Discovery lead; detailed read open |
| `docs/domains/hydrodynamics/propeller-4-quadrant-data.md` | Discovery lead; detailed read open |

Discovery counted 58 matching documentation files; 18 identity-bearing historical/evidence paths are retained only in the local discovery record and require curated public references, issue pending (1).

The registry lists seven `naval_arch` workflows: yaw moment, rudder-stock torque, maneuvering envelope, damage stability, platform stability, hull resistance and hull seakeeping; all are marked offline, and none is promoted here to validation evidence. [docs/registry/workflows.yaml:L790-L819] [docs/registry/workflows.yaml:L1054-L1089]

Their existing node form is `tests/workflows/test_durable_workflows.py::test_workflow_registry[<workflow-id>]`; workflow execution is deferred because the existing test writes its declared example output paths inside the repository, while the present evidence set uses isolated temporary outputs only, issue pending (1). [tests/workflows/test_durable_workflows.py:L51-L63]

The calculation-citation contract was read; the cited wrappers in section 3 are opt-in, and the returned empty-sidecar behavior remains an explicit evidence limitation. [src/digitalmodel/naval_architecture/fundamentals.py:L52-L88]

| Open item | Responsible role | Issue |
| --- | --- | --- |
| U1: common origin/phase/unit schema for all remaining branches, section 2 | Engineering calculations quality manager | issue pending (1) |
| U2: full edition/clause reconciliation and licensed-copy checks, section 3 | Engineering calculations quality manager | issue pending (1) |
| U3: independently opened EN400 expectations and qualified resistance reference, section 4 | Engineering calculations quality manager | issue pending (1); #2020 |
| U4: role appointments and shared-log entry, section 8 | Maintainer | issue pending (1) |
| Remaining source/doc review, expanded per-method validation, upstream inventory-row reconciliation, QA evidence appendix and public data scope | Engineering calculations quality manager | issue pending (1) |
| Ineffective roll coefficient parameter | Maintainer | issue pending (2) |
| Dimensional consistency of flooded-GM and platform-GM helpers | Maintainer | issue pending (3) |
| Legacy ITTC positive-input singularity | Maintainer | issue pending (5) |
| Add missing method families to the map and classify repository-wide alternate implementations | Engineering calculations quality manager | issue pending (4) |
| Full-domain collection requires the declared test tooling; optional CAD/data/pinned-environment checks were skipped as recorded below | Maintainer | issue pending (1) |
| Draft issue numbers require replacement by issue numbers before pack clearance | Maintainer | issue pending (1) |

### Offline execution record

| Command scope | BASE | AFTER | Run pin |
| --- | --- | --- | --- |
| Full module directory, unchanged addopts | 1070 collected; 3 collection errors; 1 collection skip; no test bodies executed | Same | `d96aad6b05f0da8b01be3cc490277bf8080bf663` |
| Main-path batch, 13 files | 211 PASS, 0 FAIL, 0 SKIP, 1 XFAIL | Same; per-node outcome lists identical | Header code pin |
| Remaining batch, 37 files | 849 PASS, 0 FAIL, 10 SKIP including 1 collection skip | Same; per-node outcome lists identical | Header code pin |

The full-directory collection is not a passing suite: `test_rudder_stock_torque_sweep.py`, `test_turning_circle_estimator.py` and `test_yaw_moment_sweep.py` fail collection with `'xdist_group' not found in markers configuration option`; the missing pytest-xdist plugin registers that marker in a complete test environment. The shipped strict-marker configuration was retained. [tests/naval_architecture/test_rudder_stock_torque_sweep.py:L19-L23] [tests/naval_architecture/test_turning_circle_estimator.py:L18-L22] [tests/naval_architecture/test_yaw_moment_sweep.py:L20-L24] [pytest.ini:L46-L52]

The independently invoked file batches account for 1070 collected test nodes: 1060 pass, one expected failure and nine skipped nodes; the optional CAD module contributes one additional collection-skip event. They do not establish case counts in the three modules that could not collect.

| Skipped or expected-failure check | Reason as observed | Tracking |
| --- | --- | --- |
| `tests/naval_architecture/test_brep_tessellate.py (module collection)` | Needs the `cad` extra | issue pending (1) [tests/naval_architecture/test_brep_tessellate.py:L38-L38] |
| `tests/naval_architecture/test_hull_cap.py::test_real_open_hull_caps_to_a_watertight_stl` | Optional readable hull fixture is not configured | issue pending (1) [tests/naval_architecture/test_hull_cap.py:L648-L648] |
| `tests/naval_architecture/test_hull_ingest.py::test_real_3dm_ingests_to_a_manifest` | Optional readable hull fixture is not configured | issue pending (1) [tests/naval_architecture/test_hull_ingest.py:L600-L600] |
| `tests/naval_architecture/test_hull_regions.py::test_real_3dm_appendages_ingest_as_separate_closed_regions` | Optional hull and appendage-layer settings are absent | issue pending (1) [tests/naval_architecture/test_hull_regions.py:L624-L624] |
| `tests/naval_architecture/test_mesh_pickle_generator.py::test_generator_ignores_hostile_git_bindings_and_worktree_hull` | Requires NumPy 2.4.4 and Python 3.11.14 | issue pending (1) [tests/naval_architecture/test_mesh_pickle_generator.py:L66-L66] |
| `tests/naval_architecture/test_trim_hydrostatics.py::test_real_hull_condition_matrix_is_physically_sane` | Optional readable hull fixture is not configured | issue pending (1) [tests/naval_architecture/test_trim_hydrostatics.py:L494-L494] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestCSVBulkRegistration::test_register_17_vessels` | Required adjacent CSV source is unavailable in this checkout | issue pending (1) [tests/naval_architecture/test_vessel_fleet_adapter.py:L695-L695] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestCSVBulkRegistration::test_all_have_required_keys` | Required adjacent CSV source is unavailable in this checkout | issue pending (1) [tests/naval_architecture/test_vessel_fleet_adapter.py:L704-L704] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestDrillingRigHullValidationCSV::test_validate_only_supported_rigs_with_geometry` | Required adjacent CSV source is unavailable in this checkout | issue pending (1) [tests/naval_architecture/test_vessel_fleet_adapter.py:L847-L847] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestDrillingRigHullValidationCSV::test_register_supported_geometry_subset` | Required adjacent CSV source is unavailable in this checkout | issue pending (1) [tests/naval_architecture/test_vessel_fleet_adapter.py:L867-L867] |
| `tests/naval_architecture/test_en400_worked_examples.py::TestEN400Ch3Hydrostatics::test_example_3_1_displacement_from_curves_of_form` | Existing XFAIL: curves-of-form needs ship hydrostatic data tables | #1751 [tests/naval_architecture/test_en400_worked_examples.py:L115-L125] |

No solver run, licensed-copy comparison, external reference retrieval, private-sidecar value or external data transfer is included in this evidence record.
