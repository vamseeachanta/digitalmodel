# Naval architecture knowledge pack

Revision 4 tightens input validation and identifies the limited stability scope.
Revision 3 supplements the historical evidence below with the missing-input
contract and resonance regression for #2597. The original code pin and test
records remain historical; the supplement does not requalify other methods.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 2 | 2026-10-15 | Existing pack records calculation interfaces and evidence. |
| 3 | 2026-10-21 | Add completeness and resonance pitfalls, regression checks and worked inputs for #2597. |
| 4 | 2026-10-21 | Reject malformed direct inputs and incomplete curves, clarify stability scope and pin revision 3 evidence. |

| Header field | Value |
| --- | --- |
| Module | naval_architecture |
| Repository | digitalmodel |
| Package path | src/digitalmodel/naval_architecture |
| Pack revision | 4, 2026-10-21 |
| Historical code revision | `d96aad6b05f0da8b01be3cc490277bf8080bf663` |
| Revision 3 code revision (C11/C12) | `902f77a12f4efda52cbfbc1e400e7ada93ca919f` |
| Analysis family | EC — Python engineering calculations |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT |
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

UNVERIFIED: one common origin, phase convention and unit schema across every geometry, transfer and export branch has not been established; issue pending (6).

## 3. Method and governing standards

| Method / reference as found | Implementation and scope | Evidence |
| --- | --- | --- |
| USNA EN400, Principles of Ship Performance; tests label Summer 2020, Chapters 1–4, 7 and 10 | Weight, buoyancy, CG, inclining experiment, Simpson integration, GZ/free surface, resistance scaling and submarine helpers; textbook expectations remain regression evidence until checked independently | [tests/naval_architecture/test_en400_worked_examples.py:L3-L13] [src/digitalmodel/naval_architecture/hydrostatics.py:L86-L131] [src/digitalmodel/naval_architecture/integration.py:L13-L42] [src/digitalmodel/naval_architecture/stability.py:L84-L122] [src/digitalmodel/naval_architecture/submarines.py:L14-L59] |
| ITTC, 1957 model–ship correlation line | `resistance.ittc_1957_cf`; opt-in cited wrapper resolves an EN400 reference; SI `friction_scaling.ittc57_cf` delegates to it after validating the domain; `holtrop_mennen.frictional_resistance` and `appendage_resistance` each compute the line inline with only positive speed/length guards | [src/digitalmodel/naval_architecture/resistance.py:L51-L95] [src/digitalmodel/naval_architecture/friction_scaling.py:L98-L123] [src/digitalmodel/naval_architecture/holtrop_mennen.py:L66-L73] [src/digitalmodel/naval_architecture/holtrop_mennen.py:L108-L122] |
| ITTC Recommended Procedure 7.5-02-03-01.4, 1978 ITTC Performance Prediction Method, 2021 Revision 05, sections 2.3 and 2.4.1 | `transfer_model_to_ship`, inverse transfer and Prohaska regression; allowance terms are caller-declared or zero with a flag | [src/digitalmodel/naval_architecture/friction_scaling.py:L4-L20] [src/digitalmodel/naval_architecture/friction_scaling.py:L257-L345] [src/digitalmodel/citations/registry.py:L174-L192] |
| IMO, International Code on Intact Stability, 2008, Resolution MSC.267(85), Part A general intact criteria | `check_imo_intact_stability` integrates tabulated GZ and checks six criteria; the supplied-table screener carries clause records | [src/digitalmodel/naval_architecture/damage_stability.py:L1-L24] [src/digitalmodel/naval_architecture/damage_stability.py:L87-L149] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L470-L568] |
| IACS UR S11, Longitudinal Strength Standard, Rev.7 (2010), vertical wave bending moment and shear force | `wave_coefficient`, `wave_bending_moment`, distribution factors and `wave_shear_force`; `hull_girder_ultimate_moment` is a simplified capacity estimate | [src/digitalmodel/naval_architecture/hull_girder_strength.py:L60-L78] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L121-L137] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L143-L219] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L303-L311] |
| IACS UR S11A / CSR Common Structural Rules, hull-girder ultimate strength / Smith procedure | `smith_ultimate` solves neutral-axis equilibrium and marches curvature; code names CSR Pt 1 Ch 5 Sec 4 but gives no edition in its reference block; the codes register assigns S11A edition 2021 to `hull_girder_strength.py`, requiring reconciliation in issue pending (7) | [src/digitalmodel/naval_architecture/hull_girder_smith.py:L3-L47] [src/digitalmodel/naval_architecture/hull_girder_smith.py:L357-L418] [docs/domains/codes-register.md:L128-L128] |
| DNV-RP-C201, Buckling Strength of Plated Structures; edition absent from this wrapper | `compression_flange_ultimate_stress` delegates to the structural stiffened-panel solver; Smith factories also delegate compression capacities | [src/digitalmodel/naval_architecture/hull_girder_strength.py:L350-L364] [src/digitalmodel/naval_architecture/hull_girder_smith.py:L39-L47] |
| DNV-OS-E301, Position Mooring; citation-registry revision 2021-07; no governing calculation clause established here | Mentions in EN400 wrapper documentation describe a citation-interface precedent, not a mooring design calculation | [src/digitalmodel/naval_architecture/fundamentals.py:L52-L63] [src/digitalmodel/naval_architecture/hydrostatics.py:L36-L47] [src/digitalmodel/naval_architecture/stability.py:L41-L54] [src/digitalmodel/naval_architecture/resistance.py:L61-L72] [src/digitalmodel/naval_architecture/seakeeping.py:L68-L79] [src/digitalmodel/subsea/mooring_analysis/README.md:L363-L365] [src/digitalmodel/citations/registry.py:L27-L32] |
| OCIMF, Prediction of Wind and Current Loads on VLCCs, second edition (1994); clause absent in code | `current_yaw_moment_ocimf` computes current yaw moment; `current_lateral_force_ocimf` computes current lateral force; supplied coefficients, no coefficient table reproduced | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L28-L36] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L253-L266] |
| Holtrop & Mennen (1982), Holtrop (1984), statistical resistance method; paper titles/clauses not recorded | `holtrop_mennen` computes resistance components and `holtrop_coefficients` supplies the regression coefficients; the hull-resistance route remains unqualified while #2020 is open | [src/digitalmodel/naval_architecture/holtrop_mennen.py:L1-L5] [src/digitalmodel/naval_architecture/holtrop_coefficients.py:L1-L7] [src/digitalmodel/naval_architecture/workflow_calculations.py:L132-L169] |
| Clarke, Gedling & Hine (1983), Trans. RINA 125; clause not recorded | `clarke_derivatives` computes linear derivatives; `course_stability_discriminant` evaluates the course-stability criterion | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L28-L35] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L99-L137] |
| Nomoto et al. (1957), Int. Shipbuilding Progress 4(35); clause not recorded | `nomoto_steady_yaw_rate` computes steady yaw; `steady_turning_radius_over_L` applies the nondimensional steady-radius relation | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L28-L35] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L161-L175] [src/digitalmodel/naval_architecture/maneuverability.py:L59-L80] |
| Söding (1982), Schiffstechnik 29; Brix (1993); clause not recorded | `rudder_angle_to_hold_heading` delegates rudder authority to `soding_forces`; the dependency documentation names Brix, Manoeuvring Technical Manual (1993), and also names a Söding 1998 formulation, requiring edition reconciliation in U2 | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L28-L35] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L279-L313] [docs/domains/hydrodynamics/propeller-rudder-literature.md:L145-L150] |
| Lyster & Knights (1979), Trans. NECIES 95; clause not recorded | `tactical_diameter_over_L` uses a caller-adjustable transient screening factor; it is not a first-principles result | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L177-L186] |
| IMO MSC.137(76) (2002), Standards for Ship Manoeuvrability, turning ability | `imo_turning_compliance` compares advance and tactical-diameter ratios against the coded limits; clause text not checked against the licensed copy | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L28-L35] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L196-L205] |
| DNV-RU-SHIP Pt.3; codes-register edition 2021; exact rule subsection absent | `minimum_plate_thickness`, `plate_thickness_for_pressure` and `required_section_modulus` supply simplified scantling checks; `check_beam_draft_ratio` and `check_length_beam_ratio` supply proportion screens; clause text not checked against the licensed copy | [src/digitalmodel/naval_architecture/scantlings.py:L3-L32] [src/digitalmodel/naval_architecture/scantlings.py:L58-L129] [src/digitalmodel/naval_architecture/compliance.py:L27-L42] [docs/domains/codes-register.md:L129-L130] |
| ABS Rules for Steel Vessels; edition and clause absent | Named in `scantlings` reference block; no distinct ABS-specific function established; clause text not checked against the licensed copy | [src/digitalmodel/naval_architecture/scantlings.py:L3-L32] |
| IACS CSR, Common Structural Rules; codes-register edition 2022; exact scantling clause absent | `scantlings` labels its prescriptive checks DNV-RU-SHIP / IACS CSR; clause text not checked against the licensed copy | [src/digitalmodel/naval_architecture/scantlings.py:L3-L32] [docs/domains/codes-register.md:L130-L130] |
| ICLL 1966; full title and clause not recorded | `check_min_freeboard` implements a simplified Type B relation; clause text not checked against the licensed copy | [src/digitalmodel/naval_architecture/compliance.py:L9-L24] |
| Wind threshold balance | `threshold_speed_for_steerage` equates wind loading with rudder authority and solves speed | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L211-L246] |
| Wind heel | `floating_platform_stability.compute_wind_heel` converts supplied wind pressure to force and balances moment against righting moment, with sine clamping | [src/digitalmodel/naval_architecture/floating_platform_stability.py:L209-L252] |
| Whicker & Fehlner (1958), DTMB Report 933, Free-Stream Characteristics of a Family of Low-Aspect-Ratio, All-Movable Control Surfaces for Application to Ship Design | `maneuverability.rudder_lift_coefficient` and `rudder_normal_force` evaluate the lift relation and dynamic-pressure force; `rudder_lift_slope_per_rad` evaluates the slope separately | [src/digitalmodel/naval_architecture/maneuverability.py:L1-L56] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L144-L154] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L28-L35]; DTIC, [report catalogue](https://archive.org/details/DTIC_ADA014272), 1958, title metadata opened 2026-10-15 |
| Current heading balance | `critical_current_speed_for_heading` directly inverts the current-moment relation after obtaining rudder authority; `rudder_angle_to_hold_heading` inverts rudder angle | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L279-L336] |
| Natural periods and linear motion screen | `natural_heave_period` uses mass/waterplane stiffness; `simple_heave_rao` is a scalar oscillator magnitude; spectral response uses rectangular quadrature, without phase | [src/digitalmodel/naval_architecture/seakeeping.py:L55-L65] [src/digitalmodel/naval_architecture/seakeeping.py:L146-L193] |
| Scantling and compliance screens | Representative location coefficients and simplified compliance expressions coexist with explicit minimum plate/section checks | [src/digitalmodel/naval_architecture/scantlings.py:L3-L42] [src/digitalmodel/naval_architecture/compliance.py:L19-L42] |

The code names IACS UR S11 Rev.7 (2010), while the codes register assigns 2020 to the same module; this is an edition discrepancy, issue pending (7). [src/digitalmodel/naval_architecture/hull_girder_strength.py:L60-L68] [docs/domains/codes-register.md:L123-L130]

For the licensed-standard references above, clause text not checked against the licensed copy; code locators identify implementation evidence rather than reproduce tabulated requirements.

The EN400 `_cited` interfaces emit optional sidecars: an unconfigured resolver warns once and returns no citations, while other resolution errors propagate. [src/digitalmodel/naval_architecture/fundamentals.py:L52-L88] [src/digitalmodel/naval_architecture/hydrostatics.py:L36-L72]

UNVERIFIED: complete edition/clause reconciliation for ITTC, IACS, IMO, OCIMF and DNV references, including downstream buckling references, remains open; issue pending (7).

## 4. Validated worked examples

Execution record: BASE and AFTER results below use Python pytest with source path set to `src`, `uv run --no-sources --extra test --extra cad python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch> --junitxml=<scratch>`, unchanged repository addopts, on BASE `79805f1bffb0f9542d5fca97494d3adfba7aeffe` and its uncommitted documentation changes; calculation/test source equals the code revision in the header. The declared `test` and `cad` extras are installed, and all module and workflow executions use a scratch copy.

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
| R3 JSON router round trip | Synthetic `_settings()` fixture and temporary output directory | Summary displacement 6000.0 t with pytest default approximation; GM is not NaN and matching status | `tests/naval_architecture/test_loading_computer.py::test_router_writes_summary_and_stamps_status` | PASS / PASS [tests/naval_architecture/test_loading_computer.py:L254-L271] |

UNVERIFIED: EN400-labelled answers and approximate Holtrop fixture targets have not been qualified against an independently opened publication; EN400 coverage is #1848, Holtrop reference qualification is #2020. [tests/naval_architecture/test_en400_worked_examples.py:L3-L13] [tests/naval_architecture/test_holtrop_reproduction.py:L19-L28]

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations: G0 inputs/units/citations, G1 imports/tests, G2 hand or published check; G3 and G4 NOT_APPLICABLE for this offline calculation scope; G5 evidence applies to every family. (AQG r0, sections 9–11.)

The checks below are acceptance instructions; tolerances are local check criteria, not standard tabulations.

| Check | Gate | Executable route and numeric criterion with unit | Evidence / current result |
| --- | --- | --- | --- |
| C0 | G0 | `test_overload_sinks_the_box_raises`: `solve_equilibrium` rejects the overloaded test box; uncaught invalid-input cases = 0 cases | Pytest node; BASE / AFTER PASS [tests/naval_architecture/test_loading_computer.py:L86-L94] |
| C1 | G0 | `test_permeability_must_be_valid`: `lost_buoyancy_sinkage` rejects 1.5 permeability, dimensionless; missed rejection = 0 cases | Pytest node; BASE / AFTER PASS [tests/naval_architecture/test_damage_stability.py:L158-L164] |
| C2 | G1 | Import and execute all 53 `tests/naval_architecture/` files with declared `test` and `cad` extras; include the three wheel-building sweep modules in a scratch copy; failed tests = 0 cases and collection errors = 0 modules | BASE / AFTER: 1129 PASS, 10 SKIP (9 not exercised), 1 XFAIL, 0 collection errors; identical per-node outcomes; per-file and skip records in section 10 |
| C3 | G2 | V1 `solve_equilibrium`: absolute buoyancy/weight relative error ≤ 1e-12, dimensionless | V1 node; BASE / AFTER PASS [tests/naval_architecture/test_loading_computer.py:L49-L66] |
| C4 | G2 | V2 `significant_motion`: error < 0.01 m against constant-spectrum integral | V2 node; BASE / AFTER PASS [tests/naval_architecture/test_seakeeping.py:L147-L159] |
| C5 | G5 | R3 `router`: exactly 1 readable summary JSON file, 0 missing required result/status fields; retain node result and input fingerprint in review evidence | R3 node verifies JSON and status; BASE / AFTER PASS for the file/status assertions; evidence packaging remains issue pending (1) [tests/naval_architecture/test_loading_computer.py:L254-L271] |
| C6 | G2 | Audit `natural_roll_period` at equal beam/GM and two different positive `c_roll` values; expected coefficient-dependent response; parameter uses in executable body ≥ 1 use | FAIL: static code check detects 0 uses; issue pending (2), no shipped detection check exists [src/digitalmodel/naval_architecture/seakeeping.py:L32-L52] |
| C7 | G0 | Audit `flooded_gm` and platform `compute_gm` dimensions: each BM or GM increment must have length exponent 1, unit m; unresolved mass dimension = 0 factors | FAIL: static dimensional check finds tonnes in the first denominator and a dimensionless area-power/volume term in the second; issue pending (3), no shipped detection check exists [src/digitalmodel/naval_architecture/damage_stability.py:L173-L182] [src/digitalmodel/naval_architecture/floating_platform_stability.py:L112-L147] |
| C8 | G5 | Recompute section-9 inventory and grep; missing inventory rows = 0 rows; further unlisted in-package implementations ≤ 1 definition | Inventory and definition audit; section 9 |
| C9 | G5 | Configured `mass_to_weight_cited` returns exactly 1 EN400 citation, count; missing-page resolution raises in 1 case, while unconfigured resolution records 0 citations and a warning in 1 case | BASE / AFTER PASS; existing citation nodes [tests/naval_architecture/test_na_fundamentals_citations.py:L36-L68] |
| C10 | G5 | Curves-of-form worked-example outcome is retained as exactly 1 XFAIL record, count, with 0 promotions to validated examples | BASE / AFTER PASS for evidence classification; no missing-table detection check exists [tests/naval_architecture/test_en400_worked_examples.py:L115-L125] |

G3 NOT_APPLICABLE: no transient solver job is part of these Python calculation checks; G4 NOT_APPLICABLE: the family set uses G2 hand checks here, and no diffraction-program run is claimed. (AQG r0, section 10, Python calculations.)

C5 does not establish an AQG-signed report appendix; retain per-case inputs, hashes, results, deviations and independent review evidence before using the outputs in a report. (AQG r0, sections 9 and 11.)

### Revision 3 supplement

The C11/C12 evidence is pinned to the revision 3 code revision in the header;
the earlier checks retain the historical code revision.

| Check | Gate | Executable route and criterion | Evidence |
| --- | --- | --- | --- |
| C11 | G0 | `tests/naval_architecture/test_compliance_missing_inputs.py`: no passing aggregate on missing or malformed required data; counts partition all intended checks | Synthetic input/status cases; ratio hand calculation in test docstring |
| C12 | G2 | `tests/naval_architecture/test_heave_rao_resonance.py`: undamped resonance raises; finite damped and off-resonance values agree with the documented hand calculations | Mechanics Map, 2021 online edition, section 15.5; numerical tolerances in test docstrings |

Worked inputs, method, answers and remaining rule-basis limits:
[compliance-inputs-example.md](compliance-inputs-example.md).

## 6. Known failure modes and pitfalls
sweep: not run (legacy pack)

| Symptom / cause | Detection | Incident or open record | Date / evidence |
| --- | --- | --- | --- |
| Missing depth, measured section modulus or other required input previously produced a passing or omitted row; the empty aggregate also passed | C11: every intended row is reported; missing or invalid inputs are NOT_EVALUATED and cannot pass the aggregate | #2597 | 2026-10-21; `compliance.py`, `tests/naval_architecture/test_compliance_missing_inputs.py` |
| The simplified heave oscillator previously returned a finite cap at undamped resonance | C12: the existing fix raises ValueError for an unbounded response and preserves finite near-resonance magnitudes | #2597 | 2026-10-21; `seakeeping.py::simple_heave_rao`, `tests/naval_architecture/test_heave_rao_resonance.py` |
| Silent empty citation list when the resolver is unconfigured; other citation errors raise | C9; inspect returned `citations` count before relying on provenance | issue pending (6) | 2026-10-15 [src/digitalmodel/naval_architecture/fundamentals.py:L52-L88] |
| Density lookup clamps outside its table; buoyancy water selectors treat any non-saltwater string as fresh water | No detection check exists for either out-of-table clamping or selector typos; the density boundary tests preserve the clamp rather than reject it | issue pending (6) | 2026-10-15 [src/digitalmodel/naval_architecture/fundamentals.py:L91-L125] [tests/naval_architecture/test_na_fundamentals.py:L72-L81] |
| Existing curves-of-form example is expected to fail because the required ship table is unavailable | C10 preserves the XFAIL evidence; no missing-table detection check exists; the repository node does not qualify the calculation | #1751 | 2026-10-15 [tests/naval_architecture/test_en400_worked_examples.py:L115-L125] |
| Open #2020 reports a hull-form-insensitive total resistance coefficient; the diagnostic records components without an accuracy verdict, so the symptom is neither confirmed nor refuted here. The hull-resistance route remains unqualified until #2020 closes; fixture targets are not a physical oracle | No detection check exists against a qualified independent oracle; repository diagnostic tests preserve reference status | #2020 | 2026-10-15 [scripts/validation/reproduce_holtrop_discrepancy.py:L24-L56] [tests/naval_architecture/test_holtrop_reproduction.py:L19-L28] [src/digitalmodel/naval_architecture/workflow_calculations.py:L132-L169] |
| `c_roll` is accepted and documented but the executable roll-period body uses a fixed beam multiplier | C6; no shipped detection check exists | issue pending (2) | 2026-10-15 [src/digitalmodel/naval_architecture/seakeeping.py:L32-L52] |
| `flooded_gm` divides lost inertia in m⁴ by displacement in tonnes; the existing test repeats that expression, without a density conversion | C7; no shipped detection check exists; do not use this helper as an independent hand reference | issue pending (3) | 2026-10-15 [src/digitalmodel/naval_architecture/damage_stability.py:L173-L182] [tests/naval_architecture/test_damage_stability.py:L166-L178] |
| Platform `compute_gm` adds an area-to-the-three-halves / volume term, which is dimensionless, to lengths; it cannot be treated as a geometric BM reference | C7; no shipped dimensional detection check exists | issue pending (3) | 2026-10-15 [src/digitalmodel/naval_architecture/floating_platform_stability.py:L112-L147] |
| Positive Reynolds number alone does not avoid the ITTC singularity in `resistance.ittc_1957_cf` or the inline `holtrop_mennen.frictional_resistance` and `appendage_resistance`; SI wrapper uses a stricter domain check | No detection check exists for legacy or inline singularities; compare input guards with `friction_scaling._check_re` | issue pending (5) | 2026-10-15 [src/digitalmodel/naval_architecture/resistance.py:L51-L58] [src/digitalmodel/naval_architecture/friction_scaling.py:L98-L114] [src/digitalmodel/naval_architecture/holtrop_mennen.py:L66-L73] [src/digitalmodel/naval_architecture/holtrop_mennen.py:L108-L122] |
| ITTC transfer bibliography names 1978/2017, while the citation registry names 2021 Revision 05; maneuvering source names Söding (1982), while the literature note names a 1998 formulation | No detection check exists for adopted-edition agreement | issue pending (7) | 2026-10-15 [docs/domains/hydrodynamics/propeller-rudder-literature.md:L145-L150] [docs/domains/hydrodynamics/propeller-rudder-literature.md:L665-L667] [src/digitalmodel/citations/registry.py:L174-L192] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L28-L35] |
| IACS UR S11 edition discrepancy: code names Rev.7 (2010), codes register names 2020; it also assigns S11A edition 2021 to `hull_girder_strength.py`, while the Smith reference block has no edition | No detection check exists | issue pending (7) | 2026-10-15 [src/digitalmodel/naval_architecture/hull_girder_strength.py:L60-L68] [docs/domains/codes-register.md:L127-L128] [src/digitalmodel/naval_architecture/hull_girder_smith.py:L3-L47] |
| Box-hull positive trim is by bow; supplied-table positive trim is by stern; transferring the value silently reverses its meaning | No detection check exists at this API boundary | issue pending (6) | 2026-10-15 [src/digitalmodel/naval_architecture/loading_computer.py:L223-L242] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L39-L42] |
| Documentation calls the Smith implementation a future step, but the package already contains `smith_ultimate` | No detection check exists for documentation freshness; compare the validation record with the implementation | issue pending (7) | 2026-10-15 [docs/domains/hull-girder-strength-validation-2026-06-28.md:L44-L57] [src/digitalmodel/naval_architecture/hull_girder_smith.py:L357-L418] |
| Local dependency scanner misses the expected `xlsxwriter` call site, while the hosted gate associated with BASE passes the contract; the environment-dependent cause is unestablished | No detection check exists for interpreter/platform parity | issue pending (10) | 2026-10-15 [tests/contracts/test_string_addressed_dependencies.py:L620-L640] |

No incident issue was supplied as a mandatory incident list; the confirmed findings above are separated from feature requests and historical issue descriptions.

## 7. Lessons log

Append-only record format: date · work reference · lesson · evidence · pack change · shared-log entry.

| Date | Work reference | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-10-15 | Initial pack | Preserve the two opposite trim conventions at API boundaries | [src/digitalmodel/naval_architecture/loading_computer.py:L223-L242] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L39-L42] | Section 2 warning | Pending; issue pending (6) |
| 2026-10-15 | Initial pack | An expected-value assertion repeating a dimensionally inconsistent expression is regression evidence only | [tests/naval_architecture/test_damage_stability.py:L166-L178] | Sections 5–6 exclude it from hand references | Pending; issue pending (3) |

## 8. Ownership

Proposed roles: engineering calculations quality manager; deputy: engineering calculations deputy quality manager; producing discipline: naval architecture calculations; escalation: the family quality manager decides method and classification disputes, and the maintainer assigns cross-family work.

UNVERIFIED: named role appointments and a shared-log entry have not been established from repository evidence; issue pending (8).

## 9. Dependencies

Upstream data are supplied hydrostatic/GZ/KN tables or declared geometry and loading inputs; the public fixture examples and box-hull test inputs are the reproducible data basis for the cases above. [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L7-L24] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L7-L15] [tests/naval_architecture/test_loading_computer.py:L30-L66]

The downstream hull-resistance adapter consumes Holtrop resistance; the maneuvering-envelope composition consumes the separate propeller/rudder force implementation; hull-girder compression capacity delegates to the structural solver. [src/digitalmodel/naval_architecture/workflow_calculations.py:L132-L169] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L5-L12] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L350-L364]

Dependency inventory identifiers: `naval_architecture` (engineering calculations); `hydrodynamics/propeller_rudder` and `structural/structural_analysis` require inventory-row reconciliation in issue pending (1).

Cross-module test ownership: the five files identified in section 10 test `alt_fuel_ship_sizing`, `hull_girder_screening` and `vessel_stability_screening`, whose separate registrations prevent double-counting their 107 nodes as this package's implementation coverage. [docs/registry/module-routing.yaml:L363-L368] [docs/registry/module-routing.yaml:L381-L386] [docs/registry/module-routing.yaml:L471-L476]

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

The repository search finds inline ITTC friction calculations inside this package in `holtrop_mennen.frictional_resistance` and `appendage_resistance`, as well as independent implementations outside it; resistance-family map additions must retain these copies, issue pending (4). [src/digitalmodel/naval_architecture/holtrop_mennen.py:L66-L73] [src/digitalmodel/naval_architecture/holtrop_mennen.py:L108-L122] [src/digitalmodel/solvers/openfoam/validation/ship_resistance.py:L116-L129] [src/digitalmodel/solvers/openfoam/validation/double_body_form_factor.py:L140-L148]

The resistance diagnostic in `scripts/` delegates to the package Holtrop implementation and normalises its component results; it is a diagnostic helper rather than an independent Holtrop implementation. [scripts/validation/reproduce_holtrop_discrepancy.py:L24-L51]

## 10. Coverage and open items

Coverage record: the main-path files and references named above were read at the code pin; all package Python files were enumerated and parsed for the family audit. Full line-by-line coverage of the remaining geometry, transfer, catalog and structural branches is deferred to issue pending (1).

### Source coverage ledger

Full main-path reads, targeted reads and enumeration-only coverage are distinguished below; the last two states retain the detailed-review gap in issue pending (1).

| Package-relative Python file | Read coverage at the pin |
| --- | --- |
| `__init__.py` | Full main-path read [src/digitalmodel/naval_architecture/__init__.py:L1-L96] |
| `appendage_submergence.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/appendage_submergence.py:L1-L223] |
| `brep_tessellate.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/brep_tessellate.py:L1-L950] |
| `compliance.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/compliance.py:L1-L163] |
| `constants.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/constants.py:L1-L309] |
| `curves_of_form.py` | Full main-path read [src/digitalmodel/naval_architecture/curves_of_form.py:L1-L55] |
| `damage_stability.py` | Full main-path read [src/digitalmodel/naval_architecture/damage_stability.py:L1-L182] |
| `damage_stability_screening.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/damage_stability_screening.py:L1-L666] |
| `data/__init__.py` | Full main-path read [src/digitalmodel/naval_architecture/data/__init__.py:L1-L2] |
| `floating_platform_stability.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/floating_platform_stability.py:L1-L330] |
| `friction_scaling.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/friction_scaling.py:L1-L345] |
| `fundamentals.py` | Full main-path read [src/digitalmodel/naval_architecture/fundamentals.py:L1-L125] |
| `gyradius.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/gyradius.py:L1-L289] |
| `holtrop_coefficients.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/holtrop_coefficients.py:L1-L141] |
| `holtrop_mennen.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/holtrop_mennen.py:L1-L199] |
| `hull_cap.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_cap.py:L1-L601] |
| `hull_fixtures.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_fixtures.py:L1-L162] |
| `hull_form.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_form.py:L1-L369] |
| `hull_girder_screening.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_girder_screening.py:L1-L953] |
| `hull_girder_smith.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/hull_girder_smith.py:L1-L418] |
| `hull_girder_strength.py` | Full main-path read [src/digitalmodel/naval_architecture/hull_girder_strength.py:L1-L364] |
| `hull_ingest.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/hull_ingest.py:L1-L1510] |
| `hull_properties.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_properties.py:L1-L196] |
| `hull_regions.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/hull_regions.py:L1-L265] |
| `hydrostatics.py` | Full main-path read [src/digitalmodel/naval_architecture/hydrostatics.py:L1-L131] |
| `integration.py` | Full main-path read [src/digitalmodel/naval_architecture/integration.py:L1-L42] |
| `kcs_geometry.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/kcs_geometry.py:L1-L745] |
| `knowledge.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/knowledge.py:L1-L151] |
| `loading_computer.py` | Full main-path read [src/digitalmodel/naval_architecture/loading_computer.py:L1-L594] |
| `maneuverability.py` | Full main-path read [src/digitalmodel/naval_architecture/maneuverability.py:L1-L128] |
| `maneuvering_envelope.py` | Full main-path read [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L1-L363] |
| `maneuvering_envelope_workflow.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L1-L394] |
| `mesh_clipping.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/mesh_clipping.py:L1-L233] |
| `mesh_geometry.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/mesh_geometry.py:L1-L131] |
| `mesh_hydrostatics.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/mesh_hydrostatics.py:L1-L245] |
| `mesh_results.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/mesh_results.py:L1-L107] |
| `mesh_sections.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/mesh_sections.py:L1-L83] |
| `mesh_topology.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/mesh_topology.py:L1-L95] |
| `mesh_validation.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/mesh_validation.py:L1-L132] |
| `prohaska.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/prohaska.py:L1-L100] |
| `propeller.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/propeller.py:L1-L367] |
| `region_report.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/region_report.py:L1-L253] |
| `resistance.py` | Full main-path read [src/digitalmodel/naval_architecture/resistance.py:L1-L112] |
| `rudder_stock_torque.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/rudder_stock_torque.py:L1-L630] |
| `scantlings.py` | Full main-path read [src/digitalmodel/naval_architecture/scantlings.py:L1-L148] |
| `seakeeping.py` | Full main-path read [src/digitalmodel/naval_architecture/seakeeping.py:L1-L193] |
| `ship_data.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/ship_data.py:L1-L370] |
| `ship_dimensions.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/ship_dimensions.py:L1-L204] |
| `solid_occlusion.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/solid_occlusion.py:L1-L402] |
| `stability.py` | Full main-path read [src/digitalmodel/naval_architecture/stability.py:L1-L122] |
| `submarines.py` | Full main-path read [src/digitalmodel/naval_architecture/submarines.py:L1-L59] |
| `symmetry_cut.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/symmetry_cut.py:L1-L345] |
| `trim_hydrostatics.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/trim_hydrostatics.py:L1-L400] |
| `turning_circle.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/turning_circle.py:L1-L384] |
| `turning_circle_artifacts.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/turning_circle_artifacts.py:L1-L353] |
| `vessel_stability_screening.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L1-L983] |
| `wigley_geometry.py` | Enumerated and AST parsed; detailed read open [src/digitalmodel/naval_architecture/wigley_geometry.py:L1-L204] |
| `workflow.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/workflow.py:L1-L256] |
| `workflow_calculations.py` | Full main-path read [src/digitalmodel/naval_architecture/workflow_calculations.py:L1-L207] |
| `yaw_moment.py` | Targeted read; remaining lines open [src/digitalmodel/naval_architecture/yaw_moment.py:L1-L578] |

### Test inventory

Five files exercise separate registered modules: `alt_fuel_ship_sizing` (52 nodes), `hull_girder_screening` (18 workflow nodes) and `vessel_stability_screening` (37 workflow nodes), totaling 107 nodes. They remain in the 1140-node run, but are cross-module evidence rather than additional naval-architecture implementations. [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L1-L636] [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L1-L286] [tests/naval_architecture/test_hull_girder_screening_workflow.py:L1-L399] [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L1-L360] [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L1-L409]

| Test file | BASE / AFTER evidence |
| --- | --- |
| `tests/naval_architecture/test_alt_fuel_ship_sizing.py` | Separate module `alt_fuel_ship_sizing`; 38 PASS; identical outcome set AFTER [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L88-L636] |
| `tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py` | Separate module `alt_fuel_ship_sizing`; 14 PASS; identical outcome set AFTER [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L36-L286] |
| `tests/naval_architecture/test_array_integrity.py` | 14 PASS; identical outcome set AFTER [tests/naval_architecture/test_array_integrity.py:L25-L206] |
| `tests/naval_architecture/test_brep_tessellate.py` | 15 PASS, 1 SKIP; identical outcome set AFTER [tests/naval_architecture/test_brep_tessellate.py:L119-L472] |
| `tests/naval_architecture/test_compliance.py` | 9 PASS; identical outcome set AFTER [tests/naval_architecture/test_compliance.py:L41-L150] |
| `tests/naval_architecture/test_damage_stability.py` | 12 PASS; identical outcome set AFTER [tests/naval_architecture/test_damage_stability.py:L47-L208] |
| `tests/naval_architecture/test_damage_stability_screening.py` | 21 PASS; identical outcome set AFTER [tests/naval_architecture/test_damage_stability_screening.py:L179-L513] |
| `tests/naval_architecture/test_en400_worked_examples.py` | 23 PASS, 1 XFAIL; identical outcome set AFTER [tests/naval_architecture/test_en400_worked_examples.py:L28-L327] |
| `tests/naval_architecture/test_floating_platform_stability.py` | 21 PASS; identical outcome set AFTER [tests/naval_architecture/test_floating_platform_stability.py:L28-L248] |
| `tests/naval_architecture/test_floating_platform_stability_extended.py` | 70 PASS; identical outcome set AFTER [tests/naval_architecture/test_floating_platform_stability_extended.py:L46-L584] |
| `tests/naval_architecture/test_friction_scaling.py` | 36 PASS; identical outcome set AFTER [tests/naval_architecture/test_friction_scaling.py:L36-L296] |
| `tests/naval_architecture/test_gyradius.py` | 21 PASS; identical outcome set AFTER [tests/naval_architecture/test_gyradius.py:L23-L174] |
| `tests/naval_architecture/test_gyradius_extended.py` | 96 PASS; identical outcome set AFTER [tests/naval_architecture/test_gyradius_extended.py:L41-L370] |
| `tests/naval_architecture/test_gz_curves.py` | 25 PASS; identical outcome set AFTER [tests/naval_architecture/test_gz_curves.py:L75-L291] |
| `tests/naval_architecture/test_holtrop_mennen.py` | 20 PASS; identical outcome set AFTER [tests/naval_architecture/test_holtrop_mennen.py:L26-L190] |
| `tests/naval_architecture/test_holtrop_reproduction.py` | 3 PASS; identical outcome set AFTER [tests/naval_architecture/test_holtrop_reproduction.py:L19-L50] |
| `tests/naval_architecture/test_hull_cap.py` | 31 PASS, 1 SKIP; identical outcome set AFTER [tests/naval_architecture/test_hull_cap.py:L226-L690] |
| `tests/naval_architecture/test_hull_form.py` | 19 PASS; identical outcome set AFTER [tests/naval_architecture/test_hull_form.py:L13-L211] |
| `tests/naval_architecture/test_hull_girder_screening.py` | 23 PASS; identical outcome set AFTER [tests/naval_architecture/test_hull_girder_screening.py:L72-L444] |
| `tests/naval_architecture/test_hull_girder_screening_workflow.py` | Separate module `hull_girder_screening`; 18 PASS; identical outcome set AFTER [tests/naval_architecture/test_hull_girder_screening_workflow.py:L58-L399] |
| `tests/naval_architecture/test_hull_girder_smith.py` | 11 PASS; identical outcome set AFTER [tests/naval_architecture/test_hull_girder_smith.py:L119-L360] |
| `tests/naval_architecture/test_hull_girder_strength.py` | 26 PASS; identical outcome set AFTER [tests/naval_architecture/test_hull_girder_strength.py:L37-L222] |
| `tests/naval_architecture/test_hull_ingest.py` | 28 PASS, 1 SKIP; identical outcome set AFTER [tests/naval_architecture/test_hull_ingest.py:L172-L611] |
| `tests/naval_architecture/test_hull_regions.py` | 26 PASS, 1 SKIP; identical outcome set AFTER [tests/naval_architecture/test_hull_regions.py:L110-L677] |
| `tests/naval_architecture/test_hydrostatics_citations.py` | 4 PASS; identical outcome set AFTER [tests/naval_architecture/test_hydrostatics_citations.py:L27-L65] |
| `tests/naval_architecture/test_kcs_geometry.py` | 12 PASS; identical outcome set AFTER [tests/naval_architecture/test_kcs_geometry.py:L41-L237] |
| `tests/naval_architecture/test_knowledge.py` | 11 PASS; identical outcome set AFTER [tests/naval_architecture/test_knowledge.py:L15-L114] |
| `tests/naval_architecture/test_loading_computer.py` | 13 PASS; identical outcome set AFTER [tests/naval_architecture/test_loading_computer.py:L49-L271] |
| `tests/naval_architecture/test_maneuverability.py` | 22 PASS; identical outcome set AFTER [tests/naval_architecture/test_maneuverability.py:L30-L249] |
| `tests/naval_architecture/test_mesh_benchmark.py` | 10 PASS; identical outcome set AFTER [tests/naval_architecture/test_mesh_benchmark.py:L15-L106] |
| `tests/naval_architecture/test_mesh_hydrostatics.py` | 48 PASS; identical outcome set AFTER [tests/naval_architecture/test_mesh_hydrostatics.py:L55-L619] |
| `tests/naval_architecture/test_mesh_pickle_generator.py` | 1 SKIP; identical outcome set AFTER [tests/naval_architecture/test_mesh_pickle_generator.py:L68-L81] |
| `tests/naval_architecture/test_mesh_r02.py` | 15 PASS; identical outcome set AFTER [tests/naval_architecture/test_mesh_r02.py:L14-L227] |
| `tests/naval_architecture/test_na_fundamentals.py` | 15 PASS; identical outcome set AFTER [tests/naval_architecture/test_na_fundamentals.py:L18-L82] |
| `tests/naval_architecture/test_na_fundamentals_citations.py` | 4 PASS; identical outcome set AFTER [tests/naval_architecture/test_na_fundamentals_citations.py:L30-L68] |
| `tests/naval_architecture/test_propeller_4_quadrant.py` | 59 PASS; identical outcome set AFTER [tests/naval_architecture/test_propeller_4_quadrant.py:L38-L372] |
| `tests/naval_architecture/test_resistance_citations.py` | 4 PASS; identical outcome set AFTER [tests/naval_architecture/test_resistance_citations.py:L26-L64] |
| `tests/naval_architecture/test_rudder_stock_torque_sweep.py` | 19 PASS; identical outcome set AFTER [tests/naval_architecture/test_rudder_stock_torque_sweep.py:L35-L428] |
| `tests/naval_architecture/test_scantlings.py` | 9 PASS; identical outcome set AFTER [tests/naval_architecture/test_scantlings.py:L20-L93] |
| `tests/naval_architecture/test_seakeeping.py` | 15 PASS; identical outcome set AFTER [tests/naval_architecture/test_seakeeping.py:L13-L167] |
| `tests/naval_architecture/test_seakeeping_citations.py` | 4 PASS; identical outcome set AFTER [tests/naval_architecture/test_seakeeping_citations.py:L27-L65] |
| `tests/naval_architecture/test_ship_data.py` | 11 PASS; identical outcome set AFTER [tests/naval_architecture/test_ship_data.py:L11-L125] |
| `tests/naval_architecture/test_ship_dimensions_template.py` | 9 PASS; identical outcome set AFTER [tests/naval_architecture/test_ship_dimensions_template.py:L41-L169] |
| `tests/naval_architecture/test_stability_citations.py` | 4 PASS; identical outcome set AFTER [tests/naval_architecture/test_stability_citations.py:L26-L64] |
| `tests/naval_architecture/test_transfer_citation_regressions.py` | 30 PASS; identical outcome set AFTER [tests/naval_architecture/test_transfer_citation_regressions.py:L21-L209] |
| `tests/naval_architecture/test_trim_hydrostatics.py` | 20 PASS, 1 SKIP; identical outcome set AFTER [tests/naval_architecture/test_trim_hydrostatics.py:L191-L532] |
| `tests/naval_architecture/test_turning_circle_estimator.py` | 14 PASS; identical outcome set AFTER [tests/naval_architecture/test_turning_circle_estimator.py:L35-L277] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py` | 60 PASS, 4 SKIP; identical outcome set AFTER [tests/naval_architecture/test_vessel_fleet_adapter.py:L84-L882] |
| `tests/naval_architecture/test_vessel_stability_screening.py` | 35 PASS; identical outcome set AFTER [tests/naval_architecture/test_vessel_stability_screening.py:L153-L505] |
| `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py` | Separate module `vessel_stability_screening`; 20 PASS; identical outcome set AFTER [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L151-L409] |
| `tests/naval_architecture/test_vessel_stability_screening_workflow.py` | Separate module `vessel_stability_screening`; 17 PASS; identical outcome set AFTER [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L116-L360] |
| `tests/naval_architecture/test_wigley_geometry.py` | 4 PASS; identical outcome set AFTER [tests/naval_architecture/test_wigley_geometry.py:L28-L75] |
| `tests/naval_architecture/test_yaw_moment_sweep.py` | 21 PASS; identical outcome set AFTER [tests/naval_architecture/test_yaw_moment_sweep.py:L37-L285] |

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

Their existing node form is `tests/workflows/test_durable_workflows.py::test_workflow_registry[<workflow-id>]`; all seven were executed in a scratch copy because the existing test writes declared output paths below its repository root. These are workflow regression checks, with no independent physical expectation promoted to validation. [tests/workflows/test_durable_workflows.py:L51-L63]

The calculation-citation contract was read; the cited wrappers in section 3 are opt-in, and the returned empty-sidecar behavior remains an explicit evidence limitation. [src/digitalmodel/naval_architecture/fundamentals.py:L52-L88]

| Open item | Responsible role | Issue |
| --- | --- | --- |
| U1: common origin/phase/unit schema for all remaining branches, section 2 | Engineering calculations quality manager | issue pending (6) |
| U2: full edition/clause reconciliation and licensed-copy checks, section 3 | Engineering calculations quality manager | issue pending (7) |
| U3: independently opened EN400 expectations and qualified resistance reference, section 4; strengthen V2 with a non-uniform closed-form spectrum (issue pending (1)) | Engineering calculations quality manager | #1848; #2020 |
| U4: role appointments and shared-log entry, section 8 | Maintainer | issue pending (8) |
| Remaining source/doc review, expanded per-method validation, upstream inventory-row reconciliation, QA evidence appendix and public data scope | Engineering calculations quality manager | issue pending (1) |
| Ineffective roll coefficient parameter | Maintainer | issue pending (2) |
| Dimensional consistency of flooded-GM and platform-GM helpers | Maintainer | issue pending (3) |
| Legacy ITTC positive-input singularity | Maintainer | issue pending (5) |
| Add missing method families to the map and classify repository-wide alternate implementations | Engineering calculations quality manager | issue pending (4) |
| Nine nodes are not exercised: a real `.3dm` hull file and the adjacent `construction_vessels.csv` and `drilling_rigs.csv` files are absent; one node is skipped by its version pin; declared test and CAD extras are installed | Maintainer | issue pending (9) |
| Local dependency-scanner result differs from hosted CI; no cause established | Maintainer | issue pending (10) |
| Draft issue numbers require replacement by issue numbers before pack clearance | Maintainer | issue pending (1) |

### Offline execution record

Execution results are measured records, not claims inferred from the code locators in the ledgers. All runs use the section-4 command form with `--extra test --extra cad`, unchanged addopts and no parallel workers. The test sources and calculation code are identical between BASE and the code pin. AFTER is the uncommitted documentation tree on BASE.

| Command scope | BASE | AFTER | Run revision |
| --- | --- | --- | --- |
| All 53 module test files, scratch copy | 1140 collected: 1129 PASS, 0 FAIL, 10 SKIP (9 not exercised), 1 XFAIL; 0 NOT_RUN, 0 collection errors | Same; per-node outcomes identical | BASE / uncommitted AFTER |
| Seven `naval_arch` workflow nodes, scratch copy | 7 collected: 7 PASS, 0 FAIL, 0 SKIP, 0 NOT_RUN | Same; per-node outcomes identical | BASE / uncommitted AFTER |
| `tests-contracts` domain from the quality-gates configuration, including routing contracts | 910 collected: 848 PASS, 1 FAIL, 61 SKIP, 0 NOT_RUN | Same; per-node outcomes identical | BASE / uncommitted AFTER |

The former three collection-error modules now pass: rudder-stock torque 19 nodes, turning circle 14 nodes and yaw moment 21 nodes. The `cad` extra adds 15 passing synthetic Brep checks and one real-fixture skip; no module-collection skip remains.

Local interpreter/platform: CPython 3.12.3, Linux aarch64; NumPy 1.26.4. The contracts failure is `tests/contracts/test_string_addressed_dependencies.py::test_the_three_known_defects_are_still_derived`, which expects an `xlsxwriter` call site that the scanner does not derive. BASE and uncommitted AFTER both record 848 PASS, 1 FAIL and 61 SKIP. The hosted `tests-contracts` gate associated with BASE instead passes: 849 PASS, 0 FAIL and 61 SKIP, with CPython 3.11.17 on Linux x86_64, checking out synthetic merge `a4d85bf41ec126088f110021aa7334deba4cbbc1`. This is a local-environment discrepancy, not evidence of a failing hosted gate; the cause is unestablished, issue pending (10). The test file is unchanged from `origin/main`; no main export is used for this run. [tests/contracts/test_string_addressed_dependencies.py:L620-L640] Hosted evidence: GitHub Actions, Quality Gates by Domain, `tests-contracts`, run 38769225811, job 116541990180, 2026-10-15, execution log.

| Workflow ID (node suffix) | BASE / AFTER |
| --- | --- |
| `naval-arch-yaw-moment` | PASS / PASS |
| `rudder-stock-torque` | PASS / PASS |
| `naval-arch-maneuvering-envelope` | PASS / PASS |
| `damage-stability` | PASS / PASS |
| `platform-stability` | PASS / PASS |
| `hull-resistance` | PASS / PASS; output remains unqualified under #2020 |
| `hull-seakeeping` | PASS / PASS |

| Skipped or expected-failure check | Reason as observed | Tracking |
| --- | --- | --- |
| `tests/naval_architecture/test_brep_tessellate.py::test_real_3dm_hull_passes_the_acceptance_gate` | SKIPPED; not exercised: real `.3dm` hull file selected by `DIGITALMODEL_3DM_HULL` absent | issue pending (9) [tests/naval_architecture/test_brep_tessellate.py:L429-L433] |
| `tests/naval_architecture/test_hull_cap.py::test_real_open_hull_caps_to_a_watertight_stl` | SKIPPED; not exercised: real `.3dm` hull file selected by `DIGITALMODEL_3DM_HULL` absent | issue pending (9) [tests/naval_architecture/test_hull_cap.py:L640-L649] |
| `tests/naval_architecture/test_hull_ingest.py::test_real_3dm_ingests_to_a_manifest` | SKIPPED; not exercised: real `.3dm` hull file selected by `DIGITALMODEL_HULL_3DM` absent | issue pending (9) [tests/naval_architecture/test_hull_ingest.py:L592-L601] |
| `tests/naval_architecture/test_hull_regions.py::test_real_3dm_appendages_ingest_as_separate_closed_regions` | SKIPPED; not exercised: real `.3dm` hull file selected by `DIGITALMODEL_3DM_HULL` and `DIGITALMODEL_3DM_APPENDAGE_LAYERS` selections absent | issue pending (9) [tests/naval_architecture/test_hull_regions.py:L620-L631] |
| `tests/naval_architecture/test_mesh_pickle_generator.py::test_generator_ignores_hostile_git_bindings_and_worktree_hull` | SKIPPED: requires NumPy 2.4.4 and Python 3.11.14; the NumPy pin is excluded by the declared runtime range `numpy>=1.24.0,<2.0.0`, so this node cannot run in any declared environment | issue pending (9) [tests/naval_architecture/test_mesh_pickle_generator.py:L65-L68] [pyproject.toml:L50-L50] |
| `tests/naval_architecture/test_trim_hydrostatics.py::test_real_hull_condition_matrix_is_physically_sane` | SKIPPED; not exercised: real `.3dm` hull file selected by `DIGITALMODEL_3DM_HULL` absent | issue pending (9) [tests/naval_architecture/test_trim_hydrostatics.py:L494-L498] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestCSVBulkRegistration::test_register_17_vessels` | SKIPPED; not exercised: `construction_vessels.csv` in adjacent `worldenergydata/data/modules/vessel_fleet/curated/` absent | issue pending (9) [tests/naval_architecture/test_vessel_fleet_adapter.py:L642-L667] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestCSVBulkRegistration::test_all_have_required_keys` | SKIPPED; not exercised: `construction_vessels.csv` in adjacent `worldenergydata/data/modules/vessel_fleet/curated/` absent | issue pending (9) [tests/naval_architecture/test_vessel_fleet_adapter.py:L642-L667] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestDrillingRigHullValidationCSV::test_validate_only_supported_rigs_with_geometry` | SKIPPED; not exercised: `drilling_rigs.csv` in adjacent `worldenergydata/data/modules/vessel_fleet/curated/` absent | issue pending (9) [tests/naval_architecture/test_vessel_fleet_adapter.py:L652-L660] [tests/naval_architecture/test_vessel_fleet_adapter.py:L835-L839] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestDrillingRigHullValidationCSV::test_register_supported_geometry_subset` | SKIPPED; not exercised: `drilling_rigs.csv` in adjacent `worldenergydata/data/modules/vessel_fleet/curated/` absent | issue pending (9) [tests/naval_architecture/test_vessel_fleet_adapter.py:L652-L660] [tests/naval_architecture/test_vessel_fleet_adapter.py:L835-L839] |
| `tests/naval_architecture/test_en400_worked_examples.py::TestEN400Ch3Hydrostatics::test_example_3_1_displacement_from_curves_of_form` | Existing XFAIL: curves-of-form needs ship hydrostatic data tables | #1751 [tests/naval_architecture/test_en400_worked_examples.py:L115-L125] |

No solver run, licensed-copy comparison, private-sidecar value or external data transfer is included in this evidence record. The DTIC open catalogue was consulted for report title metadata only.

## Turning depth applicability (#1704)

The Nomoto estimator and IMO turning screen now refuse supplied h/T <= 4,
using IMO MSC/Circ.1053 (2002), Annex 2.1.2. Missing depth is explicitly
unverified; a depth-screen pass does not validate Nomoto coefficients.
Both YAML workflows propagate `environment.water_depth_m` and configured
draft. See the [source and benchmark note](../../hydrodynamics/parametric_hull_analysis/README.md)
for the published 3.57-fold tactical-diameter increase and full references.
