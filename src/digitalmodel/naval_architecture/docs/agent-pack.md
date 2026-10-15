# Naval architecture module knowledge pack

| Header | Value |
| --- | --- |
| Module | `naval_architecture` |
| Repository | `digitalmodel` |
| Package path | `src/digitalmodel/naval_architecture` |
| Pack revision | 0, 2026-10-15 |
| Code revision | `e6e540259a1c555eeb0c151468ec959b4217407e` |
| Analysis family | Engineering calculations; registry code `EC` |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT; independent clearance pending |

Code citations below resolve at the code revision; implementation-map citations in section 9 resolve at the separately stated map artifact revision.

## 1. Purpose and scope

The package contains elementary buoyancy and weight-shift calculations, cross-curve stability corrections, flooding screening, empirical resistance prediction and simplified motion estimates. [src/digitalmodel/naval_architecture/hydrostatics.py:L27-L33] [src/digitalmodel/naval_architecture/hydrostatics.py:L86-L131] [src/digitalmodel/naval_architecture/stability.py:L26-L38] [src/digitalmodel/naval_architecture/damage_stability.py:L152-L182] [src/digitalmodel/naval_architecture/holtrop_mennen.py:L170-L199] [src/digitalmodel/naval_architecture/seakeeping.py:L32-L65]

The routing registry records maturity `beta` and routes the domain to `tests/naval_architecture/`. [docs/registry/module-routing.yaml:L195-L200]

This pack covers the package, including its geometry and screening helpers; adjacent hydrodynamics, solver and ship-design packages are dependencies rather than additional pack scope. The main paths are the offline engine workflows for damage stability, platform stability, resistance and seakeeping, together with the maneuvering sweeps. [src/digitalmodel/naval_architecture/workflow_calculations.py:L10-L80] [src/digitalmodel/naval_architecture/workflow_calculations.py:L83-L207] [docs/registry/workflows.yaml:L790-L819] [docs/registry/workflows.yaml:L1054-L1089]

The input/output tables enumerate the main calculation routes; remaining helper-contract enumeration and independent qualification are tracked in O5, issue pending (6).

The yaw-moment route is a preliminary force-times-lever-arm estimate; rudder-stock torque assumes a supplied constant perpendicular arm; the turning-circle estimator assumes constant speed and rudder angle in a first-order Nomoto model. None of these routes establishes class compliance or a full maneuvering simulation. [src/digitalmodel/naval_architecture/yaw_moment.py:L3-L8] [src/digitalmodel/naval_architecture/rudder_stock_torque.py:L3-L10] [src/digitalmodel/naval_architecture/turning_circle.py:L3-L9]

The seakeeping route estimates natural periods and a single-degree-of-freedom transfer-function magnitude; its implementation does not implement the strip-theory claim in its opening comment. [src/digitalmodel/naval_architecture/seakeeping.py:L1-L12] [src/digitalmodel/naval_architecture/seakeeping.py:L32-L65] [src/digitalmodel/naval_architecture/seakeeping.py:L110-L193]

UNVERIFIED: U1 — suitability of the screening criteria for a particular design or statutory submission is not established by this pack; the licensed clauses and calculation qualifications remain open in section 10.

## 2. Inputs and outputs

### Inputs

| Route / input | Unit | Allowed range and default | Source / contract |
| --- | --- | --- | --- |
| `buoyant_force(volume_ft3, water)` | ft³; water selector | No volume guard; selector defaults to `saltwater`, every other string selects fresh water | [src/digitalmodel/naval_architecture/hydrostatics.py:L22-L33] |
| `vertical_cg_after_weight_change` | LT, ft | Removal uses negative weight; no guard against zero final displacement | [src/digitalmodel/naval_architecture/hydrostatics.py:L86-L100] |
| `transverse_cg_after_weight_shift` | LT, ft | `direction='port'` subtracts transverse moment; other strings add | [src/digitalmodel/naval_architecture/hydrostatics.py:L103-L118] |
| `simpsons_first_rule` ordinates and spacing | Consistent length units | Equally spaced; odd count, at least 3; no spacing-sign guard | [src/digitalmodel/naval_architecture/integration.py:L13-L42] |
| `gz_from_cross_curves` heel, KN, KG | deg, ft, ft | Caller supplies KN at the governing displacement; no numerical range guard | [src/digitalmodel/naval_architecture/stability.py:L26-L38] |
| `free_surface_correction` displacement, KG/KM, rectangular tank dimensions, fluid density | LT; ft; slug/ft³ | Caller supplies dimensions and density; no positive-displacement guard | [src/digitalmodel/naval_architecture/stability.py:L94-L122] |
| `check_imo_intact_stability` angle/GZ arrays and GM | deg, m, m | Array validity is the caller's responsibility; interpolation clamps beyond endpoint angles | [src/digitalmodel/naval_architecture/damage_stability.py:L27-L45] [src/digitalmodel/naval_architecture/damage_stability.py:L87-L149] |
| `lost_buoyancy_sinkage` compartment volume, permeability, waterplane area | m³, 1, m² | Permeability in [0,1]; waterplane positivity is not checked | [src/digitalmodel/naval_architecture/damage_stability.py:L152-L170] |
| `total_resistance` principal dimensions, form coefficients, speed, appendage/bulb/transom inputs | m, 1, m/s, m² | Length and speed positive; component checks are partial; appendage lists default empty | [src/digitalmodel/naval_architecture/holtrop_mennen.py:L170-L194] [src/digitalmodel/naval_architecture/holtrop_mennen.py:L28-L32] |
| Natural periods: beam/GM, displacement/waterplane, length/longitudinal GM | m, t, m² | Roll GM and pitch GML positive; heave inputs have no explicit positivity guard | [src/digitalmodel/naval_architecture/seakeeping.py:L32-L65] [src/digitalmodel/naval_architecture/seakeeping.py:L110-L123] |
| Encounter frequency: wave frequency, speed, heading | rad/s, m/s, deg | Heading 0 means following seas; 180 means head seas | [src/digitalmodel/naval_architecture/seakeeping.py:L126-L143] |
| Yaw and rudder-stock sweeps | m/s or kn; deg; kg/m³; m²; m | Finite inputs; speeds nonnegative; area/span/density positive; stock arm nonnegative | [src/digitalmodel/naval_architecture/yaw_moment.py:L129-L141] [src/digitalmodel/naval_architecture/yaw_moment.py:L254-L264] [src/digitalmodel/naval_architecture/rudder_stock_torque.py:L132-L141] |
| Nomoto sweep | m/s or kn; deg; 1/s; s | Speed, time constant, duration and step positive; gain nonnegative; duration greater than step | [src/digitalmodel/naval_architecture/turning_circle.py:L92-L99] [src/digitalmodel/naval_architecture/turning_circle.py:L278-L324] |
| Maneuvering envelope: geometry, propulsion, wind, current, loading conditions | SI internally; sweep kn or m/s | Required sections, nonempty loading/current lists, finite values, positive geometry and inflow factors; defaults are in the validator | [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L121-L214] |

The table-based intact screener takes `HydroRow` records (draft m, displacement t, KM m, optional LCB/LCF m, MTC t·m/cm and TPC t/cm), requires at least two rows with increasing draft and displacement, and rejects queries outside the supplied range. `WeightItem` requires positive weight t and nonnegative free-surface moment t·m; loading aggregation computes KG/LCG and the virtual free-surface rise. [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L95-L207] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L214-L264]

For that screener, longitudinal positions start at the aft perpendicular and increase forward; trim is positive by the stern. This differs from the mesh API's forward-minus-aft trim convention. Missing LCB/MTC/LCF evidence leaves trim/end drafts unavailable rather than supplying geometry. [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L39-L42] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L284-L334] [src/digitalmodel/naval_architecture/mesh_results.py:L85-L94]

The damage screener offers added-weight flooding against an intact hydrostatic table or directly supplied damaged-condition hydrostatics; it does not solve geometry flooding. Flooded-compartment volume m³ must be positive, permeability lies in (0,1], free-surface moment t·m is nonnegative, and free communication requires a nonzero supplied moment. Heel is a positive magnitude toward the flooded side. [src/digitalmodel/naval_architecture/damage_stability_screening.py:L6-L30] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L50-L53] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L98-L147]

### Outputs and conventions

| Output | Unit / format | Meaning and limits | Evidence |
| --- | --- | --- | --- |
| Buoyant force / submerged volume / CG / inclining GM | lb / ft³ / ft / ft | Scalar functions retain imperial units | [src/digitalmodel/naval_architecture/hydrostatics.py:L27-L33] [src/digitalmodel/naval_architecture/hydrostatics.py:L75-L131] |
| GZ / corrected GZ / free-surface correction and effective GM | ft | KG is measured above keel; port transverse shift subtracts | [src/digitalmodel/naval_architecture/stability.py:L26-L38] [src/digitalmodel/naval_architecture/stability.py:L84-L122] [src/digitalmodel/naval_architecture/hydrostatics.py:L110-L118] |
| GZ area / criteria / sinkage | m·rad / dictionary / m | Trapezoidal area uses radian increments; criteria return per-check values and overall conjunction | [src/digitalmodel/naval_architecture/damage_stability.py:L48-L75] [src/digitalmodel/naval_architecture/damage_stability.py:L101-L149] |
| Resistance / effective power | N / W | Component sum; effective power is resistance times speed | [src/digitalmodel/naval_architecture/holtrop_mennen.py:L178-L199] |
| Natural periods / encounter frequency / simplified heave RAO | s / rad/s / 1 | RAO is magnitude only; the singular denominator uses a finite fallback | [src/digitalmodel/naval_architecture/seakeeping.py:L126-L158] |
| Significant motion | Same length unit as the supplied spectrum basis | Equal-length RAO/spectrum arrays; rectangular spectral sum, then twice its square root | [src/digitalmodel/naval_architecture/seakeeping.py:L176-L193] |
| Yaw sweep | CSV/JSON; N, N·m | About CG, +x forward, +y port, +z up, positive yaw bow-to-port; force-direction mapping explicit | [src/digitalmodel/naval_architecture/yaw_moment.py:L121-L173] [src/digitalmodel/naval_architecture/yaw_moment.py:L176-L188] [src/digitalmodel/naval_architecture/yaw_moment.py:L362-L405] |
| Rudder-stock sweep | CSV/JSON; N·m and kN·m | Holding torque equals minus hydrodynamic torque; stock-axis sign is declared in metadata | [src/digitalmodel/naval_architecture/rudder_stock_torque.py:L150-L200] |
| Turning history / advance / transfer / tactical diameter | CSV/JSON; s, m, deg, rad/s | Initial position, heading and yaw rate are zero; metrics interpolate heading crossings and return `None` plus warnings when unreached | [src/digitalmodel/naval_architecture/turning_circle.py:L130-L178] [src/digitalmodel/naval_architecture/turning_circle.py:L206-L255] [src/digitalmodel/naval_architecture/turning_circle_artifacts.py:L51-L86] |
| Envelope | CSV/JSON; kn, MN·m, deg, 1 | One row per loading/current combination; required rudder angle is null beyond available authority | [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L261-L345] [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L348-L384] |

No common unit system may be assumed across these APIs: the elementary hydrostatics/GZ functions use imperial units while damage-stability and resistance workflows use SI quantities. [src/digitalmodel/naval_architecture/hydrostatics.py:L75-L95] [src/digitalmodel/naval_architecture/stability.py:L26-L38] [src/digitalmodel/naval_architecture/workflow_calculations.py:L20-L43] [src/digitalmodel/naval_architecture/workflow_calculations.py:L95-L104]

## 3. Method and governing standards

| Method / implementation | Basis and scope | Citation sidecar / qualification |
| --- | --- | --- |
| `mass_to_weight`, `buoyant_force`, CG moments and inclining GM | Weight equals mass times gravity; buoyancy equals specific weight times volume; weighted moments and inclining moment balance | Opt-in `mass_to_weight_cited` and `buoyant_force_cited` return value, units and citations; uncited legacy calls remain available. [src/digitalmodel/naval_architecture/fundamentals.py:L44-L88] [src/digitalmodel/naval_architecture/hydrostatics.py:L27-L72] [src/digitalmodel/naval_architecture/hydrostatics.py:L86-L131] |
| `simpsons_first_rule` | Simpson one-third quadrature, equal station spacing and odd ordinate count | No Citation sidecar in this function. [src/digitalmodel/naval_architecture/integration.py:L13-L42] |
| `gz_from_cross_curves`, `gz_with_tcg_correction`, `free_surface_correction` | KN minus KG sine heel; transverse correction uses cosine heel; rectangular tank free-surface moment | Opt-in GZ cited wrapper uses EN400 Chapter 4; free-surface function returns a tuple without sidecar. [src/digitalmodel/naval_architecture/stability.py:L26-L122] |
| `interpolate_gz`, `gz_area_under_curve`, `angle_of_max_gz`, `check_imo_intact_stability` | Endpoint-clamped linear interpolation, trapezoidal GZ integration and sampled maximum; IMO IS Code 2008, resolution MSC.267(85), intact stability | Source names the code but not a full clause/edition qualification; clause text not checked against the licensed copy; no Citation sidecar in the criteria dictionary. [src/digitalmodel/naval_architecture/damage_stability.py:L1-L14] [src/digitalmodel/naval_architecture/damage_stability.py:L27-L149] |
| `lost_buoyancy_sinkage`, `flooded_gm` | Parallel sinkage from flooded volume divided by intact waterplane; the separate flooded-GM helper has the dimensional defect in section 6 | Flooded-GM helper shall not serve as an independent hand-check reference. [src/digitalmodel/naval_architecture/damage_stability.py:L152-L182] |
| `ittc_1957_cf`, `frictional_resistance`, `appendage_resistance` | ITTC 1957 friction line is evaluated independently in these three definitions; imperial Reynolds API and SI resistance APIs differ | Only `ittc_1957_cf_cited` emits the EN400 Chapter 7 sidecar; clause text not checked against the licensed copy. [src/digitalmodel/naval_architecture/resistance.py:L35-L95] [src/digitalmodel/naval_architecture/holtrop_mennen.py:L66-L73] [src/digitalmodel/naval_architecture/holtrop_mennen.py:L108-L122] |
| `total_resistance` and coefficient helpers | Holtrop–Mennen 1982 / Holtrop 1984 revision; friction, appendage, wave, bulb, transom and allowance terms; coefficient functions identify equations where available | Source attribution is not independent qualification of every coefficient; no Citation sidecar in the scalar result. [src/digitalmodel/naval_architecture/holtrop_mennen.py:L1-L6] [src/digitalmodel/naval_architecture/holtrop_mennen.py:L170-L199] [src/digitalmodel/naval_architecture/holtrop_coefficients.py:L48-L76] |
| `prohaska_form_factor` | QR least squares of transformed response; Student-t confidence interval with two fitted parameters; supplied observations must satisfy the documented Froude window, minimum count and condition limit | Returns fit, residual RMS, interval, condition number and error-model declaration; relative noise is only approximately covered. [src/digitalmodel/naval_architecture/prohaska.py:L15-L46] [src/digitalmodel/naval_architecture/prohaska.py:L51-L100] |
| `natural_roll_period`, `natural_heave_period`, `natural_pitch_period`, `simple_heave_rao`, `significant_motion` | Simplified oscillator periods, transfer magnitude and rectangular spectral integration; references name PNA Vol III, EN400 Chapter 8 and Offshore Hydromechanics | Opt-in cited heave wrapper only; roll coefficient argument is ignored and singular RAO is capped, as section 6 records. [src/digitalmodel/naval_architecture/seakeeping.py:L9-L12] [src/digitalmodel/naval_architecture/seakeeping.py:L32-L123] [src/digitalmodel/naval_architecture/seakeeping.py:L146-L193] |
| `rudder_lift_coefficient`, `rudder_normal_force`, `rudder_lift_slope_per_rad` | Whicker–Fehlner lift; effective aspect ratio doubles behind the hull; normal force uses dynamic pressure times area and lift coefficient; the slope is calculated separately in the envelope module | Explicit further definitions for the counted map family; no independent Citation sidecar. [src/digitalmodel/naval_architecture/maneuverability.py:L19-L56] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L144-L154] |
| `compute_wind_heel` | Wind pressure times projected area times arm, opposed by displacement/GM righting moment | Explicit further definition for the counted map family. [src/digitalmodel/naval_architecture/floating_platform_stability.py:L209-L252] |
| `threshold_speed_for_steerage`, `current_yaw_moment_ocimf`, `current_lateral_force_ocimf` | Rudder/wind force balance; OCIMF-style current moment and force use supplied load coefficients; OCIMF, Prediction of Wind and Current Loads on VLCCs, 2nd edition, 1994, as cited by the source | Clause/table not identified in the source; clause text not checked against the licensed copy; both current-load formula definitions are listed here explicitly. [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L28-L35] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L211-L266] |
| `clarke_derivatives`, `course_stability_discriminant`, `rudder_angle_to_hold_heading`, `critical_current_speed_for_heading` | Clarke/Gedling/Hine 1983 linear derivatives; course discriminant; composed Soding/Brix rudder authority and sine inversion | Screening only; missing or exceeded authority returns a failed heading-hold result, not a computed angle. [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L100-L137] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L279-L336] |
| `rudder_yaw_moment`, `rudder_stock_torque`, `simulate_nomoto_turning_circle` | Force-times-arm and first-order Nomoto response; exact yaw-rate exponential and heading increment, midpoint position update | Provenance dictionaries describe the calculation but are not standards Citation instances. [src/digitalmodel/naval_architecture/yaw_moment.py:L143-L164] [src/digitalmodel/naval_architecture/rudder_stock_torque.py:L143-L173] [src/digitalmodel/naval_architecture/turning_circle.py:L130-L178] [src/digitalmodel/naval_architecture/turning_circle_artifacts.py:L68-L86] |

UNVERIFIED: U2 — the independent textbook answers claimed by the EN400 test documentation were not checked against an opened source publication; those cases remain regression evidence in section 4. [tests/naval_architecture/test_en400_worked_examples.py:L1-L16]

UNVERIFIED: U3 — standards clauses and citation-sidecar coverage outside the opt-in EN400 wrappers are not fully qualified; the edition mismatch and required follow-up are recorded in sections 6 and 10.

### Additional methods and standards inventory

| Method group | Implementations, basis and limits |
| --- | --- |
| Hull-girder loads and strength | `wave_coefficient`, wave bending/shear and distribution functions name IACS UR S11, Rev.7 (2010), sections 2.2.1 and 2.2.2; the codes register names 2020. The discrepancy is unresolved; clause text not checked against the licensed copy. [src/digitalmodel/naval_architecture/hull_girder_strength.py:L61-L81] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L94-L110] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L123-L180] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L202-L221] [docs/domains/codes-register.md:L127-L128] |
| Scantlings and panel capacity | Scantling helpers name DNV-RU-SHIP / IACS CSR and describe their coefficients as representative; the hull-girder compression and Smith helpers delegate buckling capacity to structural code and name DNV-RP-C201 without an edition/clause. Clause text not checked against the licensed copy. [src/digitalmodel/naval_architecture/scantlings.py:L3-L36] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L350-L364] [src/digitalmodel/naval_architecture/hull_girder_smith.py:L133-L199] |
| Intact screening | `ImoIntactCriteria` records the IMO IS Code, edition 2008, Part A 2.2.1–2.2.4; this local Citation record differs from the elementary scalar criteria route's absence of a Citation sidecar. Clause text not checked against the licensed copy. [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L483-L489] [src/digitalmodel/naval_architecture/damage_stability.py:L87-L149] |
| ITTC scale transfer | `transfer_model_to_ship` documentation identifies ITTC Recommended Procedure 7.5-02-03-01.4, 1978 Performance Prediction Method; ordinary mode records an unresolved transfer citation, strict mode validates both references. The strict registry pins 2021 Revision 05, sections 2.3 and 2.4.1, while the literature document names a 2017 revision; this metadata difference remains unresolved. Clause text not checked against the licensed copy. [src/digitalmodel/naval_architecture/friction_scaling.py:L10-L16] [src/digitalmodel/naval_architecture/friction_scaling.py:L45-L48] [src/digitalmodel/naval_architecture/friction_scaling.py:L272-L295] [src/digitalmodel/naval_architecture/friction_scaling.py:L335-L345] [src/digitalmodel/citations/registry.py:L174-L192] [docs/domains/hydrodynamics/propeller-rudder-literature.md:L665-L666] |
| DNV-OS-E301 mention | The EN400 wrapper comments cite DNV-OS-E301 as a provenance-wrapper precedent; those comments do not identify a governing naval method, edition or clause. Clause text not checked against the licensed copy. [src/digitalmodel/naval_architecture/fundamentals.py:L55-L63] [src/digitalmodel/naval_architecture/hydrostatics.py:L39-L47] [src/digitalmodel/naval_architecture/resistance.py:L62-L70] [src/digitalmodel/naval_architecture/stability.py:L48-L56] [src/digitalmodel/naval_architecture/seakeeping.py:L74-L82] |
| Additional maneuvering references | The bibliography also names OCIMF MEG4, IMO Res. MSC.137(76) (2002) and DNV Rules for Ships minimum rudder area without clause locators in that entry; edition/title completion and applicability remain U3, issue pending (4). Clause text not checked against the licensed copy. [docs/domains/manoeuvring-rudder-reference.md:L109-L110] |
| Loading and trim screening | Rectangular loading-computer equilibrium, wall-sided GZ and single-compartment damage are screening approximations; internal still-water forces use t-force and moments t·m, with explicit conversions where required. Mesh trim tables wrap the same condition hydrostatics route rather than defining another integral. [src/digitalmodel/naval_architecture/loading_computer.py:L3-L49] [src/digitalmodel/naval_architecture/loading_computer.py:L195-L299] [src/digitalmodel/naval_architecture/loading_computer.py:L379-L489] [src/digitalmodel/naval_architecture/trim_hydrostatics.py:L306-L400] |
| Mesh hydrostatics | `compute_hydrostatics` clips triangles at the waterline, excludes artificial cap faces from wetted area, and integrates volume/sections; `Quantity` carries units, provenance, digest and missing-value reason. Intracomponent triangle intersections are not checked. [src/digitalmodel/naval_architecture/mesh_hydrostatics.py:L19-L25] [src/digitalmodel/naval_architecture/mesh_hydrostatics.py:L94-L125] [src/digitalmodel/naval_architecture/mesh_hydrostatics.py:L159-L245] [src/digitalmodel/naval_architecture/mesh_results.py:L13-L40] |
| Geometry ingestion | `ingest_triangles` normalizes/caps/checks geometry and emits STL plus manifest; capping is an explicit repair; BRep tessellation requires optional geometry libraries and rejects periodic input. [src/digitalmodel/naval_architecture/hull_ingest.py:L1053-L1077] [src/digitalmodel/naval_architecture/hull_cap.py:L540-L601] [src/digitalmodel/naval_architecture/brep_tessellate.py:L154-L214] [src/digitalmodel/naval_architecture/brep_tessellate.py:L275-L279] |
| Appendage geometry | `prepare_appendage` applies the hull transform and checks closure; `classify_wetted_area` publishes external, occluded and undecided areas; this is bounded subdivision rather than an exact solid Boolean. [src/digitalmodel/naval_architecture/hull_regions.py:L99-L165] [src/digitalmodel/naval_architecture/solid_occlusion.py:L239-L274] [src/digitalmodel/naval_architecture/solid_occlusion.py:L316-L362] |
| Concept hull estimates | `capacity_plan`, `stability_curve_estimate`, `hull_weight_groups` and hull-form approximations are empirical or simplified estimates; they do not replace supplied hydrostatic curves. [src/digitalmodel/naval_architecture/hull_properties.py:L64-L128] [src/digitalmodel/naval_architecture/hull_properties.py:L131-L196] [src/digitalmodel/naval_architecture/hull_form.py:L331-L369] |
| Curves and submarine buoyancy | `displacement_at_draft` interpolates supplied tables or estimates from registered dimensions; interpolation clamps endpoints; submarine helpers compute ballast differences, density compensation and vertical weight moments. [src/digitalmodel/naval_architecture/curves_of_form.py:L11-L55] [src/digitalmodel/naval_architecture/submarines.py:L14-L59] |

Mesh units are explicitly `m` or `mm`, with SI condition inputs and outputs; the canonical frame is right-handed, x forward, y port, z up, keel baseline z=0. Trim is forward minus aft draft about the midship waterline; LCB is percent waterline length forward of midship, measured along the waterplane. [src/digitalmodel/naval_architecture/mesh_hydrostatics.py:L6-L14] [src/digitalmodel/naval_architecture/mesh_results.py:L85-L94]

The strict transfer tests use a synthetic frontmatter-only fixture; the fixture does not establish production-wiki availability or validate the underlying standard values. [tests/citations/fixtures/knowledge/wikis/marine-engineering/wiki/standards/ittc-1978-performance-prediction.md:L1-L9] [tests/naval_architecture/test_transfer_citation_regressions.py:L17-L28]

The ingestion route requires z-up and a cardinal horizontal forward direction. Its declared `lpp_m` uses waterline length as a proxy, its longitudinal origin is the wetted aft extremity, and absent draft treats the full hull as submerged; `force=True` may emit an open surface with `watertight=false`. These limits require an explicit design-basis disposition before reuse. [src/digitalmodel/naval_architecture/hull_ingest.py:L387-L421] [src/digitalmodel/naval_architecture/hull_ingest.py:L1210-L1289] [src/digitalmodel/naval_architecture/hull_ingest.py:L1349-L1351]

## 4. Validated worked examples

The box case has an analytical reference independent of the module outputs. The response at zero rudder remains an analytical reference until its blocked test can execute; regression cases are identified separately. Tolerances are the assertions already encoded in their test nodes. [tests/naval_architecture/test_mesh_hydrostatics.py:L25-L64] [tests/naval_architecture/test_turning_circle_estimator.py:L26-L46]

| ID / classification | Independent source and input | Expected quantity / tolerance basis | Existing test node ID | Offline result at code pin |
| --- | --- | --- | --- | --- |
| V1 / validated | Closed rectangular prism; test-defined L=20 m, B=4 m, depth=3 m, draft=1.5 m | V=LBT=120 m³; physical S=LB+2LT+2BT=152 m²; coefficients=1; relative tolerance 1e-9 (1), LCB absolute 1e-9 (%) | `tests/naval_architecture/test_mesh_hydrostatics.py::test_box_hydrostatics_exact` | BASE PASS; AFTER PASS [tests/naval_architecture/test_mesh_hydrostatics.py:L25-L64] |
| V2 / analytical reference | Zero-rudder closed form for the initial-value Nomoto equation; inputs in `BASE_CASE` | Heading=0 deg and lateral position=0 m, each absolute error <1e-9 in its unit; turning crossings absent | `tests/naval_architecture/test_turning_circle_estimator.py::test_nomoto_response_zero_rudder_stays_straight` | BASE / AFTER NOT_RUN: unregistered `xdist_group` prevents module collection (O9) [tests/naval_architecture/test_turning_circle_estimator.py:L26-L46] |
| R1 / regression | EN400-labelled example 1.1, inline input, source publication not opened (U2) | 1 slug produces 32.17 lb; absolute error <0.01 lb, as encoded | `tests/naval_architecture/test_en400_worked_examples.py::TestEN400Ch1Fundamentals::test_example_1_1_mass_to_weight` | BASE PASS; AFTER PASS [tests/naval_architecture/test_en400_worked_examples.py:L25-L35] |
| R2 / regression | Nomoto time-step refinement on inline `BASE_CASE` | Advance and tactical diameter agree for 1.0 s and 0.5 s steps within relative 0.03 (1); this checks consistency rather than an independent trajectory | `tests/naval_architecture/test_turning_circle_estimator.py::test_nomoto_time_step_reduction_stability` | BASE / AFTER NOT_RUN: unregistered `xdist_group` prevents module collection (O9) [tests/naval_architecture/test_turning_circle_estimator.py:L26-L33] [tests/naval_architecture/test_turning_circle_estimator.py:L72-L84] |

The EN400 file also contains hydrostatics, stability, resistance and submarine exercises; its source-attribution claim is retained as unverified rather than extending the validated count to the entire file. The curves-of-form example carries a pre-existing expected-failure annotation. [tests/naval_architecture/test_en400_worked_examples.py:L1-L16] [tests/naval_architecture/test_en400_worked_examples.py:L120-L138] [tests/naval_architecture/test_en400_worked_examples.py:L173-L327]

## 5. QA gates

AQG r0 section 10, Python calculations, supplies G0 inputs/units/citations, G1 imports/tests and G2 independent hand/published checks; G3 and G4 are NOT_APPLICABLE because this pack does not run production dynamics or a diffraction solver. G5 applies to every family. (AQG r0, sections 9–10.)

The following are executable checks for subsequent use; a defined criterion is not an assertion that an analysis has passed it.

| Check ID | Gate / applicability | Module-specific execution and numeric pass criterion with unit | Evidence |
| --- | --- | --- | --- |
| C01 | G0 / applicable | Run `test_units_mm_equals_m`; compare every computed mesh quantity after declared mm-to-m conversion: relative error ≤1e-12 (1), absolute floor 1e-12 in the output unit; unsupported unit declarations produce 0 accepted meshes (count) | [tests/naval_architecture/test_mesh_hydrostatics.py:L193-L206] |
| C02 | G0 / applicable | Run `test_ittc_1957_cf_cited_emits_en400_citation` and `test_ittc_1957_cf_cited_fail_closed_on_missing_page`; exactly 1 citation (count) with expected code/publisher/revision/chapter; exactly 1 resolution exception (count) for the missing configured page | [tests/naval_architecture/test_resistance_citations.py:L34-L51] |
| C03 | G1 / applicable | Run the offline package suite in Python and load the downstream OpenFOAM manifest through `load_hull_manifest` in `test_loads_every_contract_field`; require 0 rejected valid manifests (count), 0 newly failed nodes (count) before/after documentation edits and 0 solver launches (count); retain every failure and skip reason | [docs/registry/module-routing.yaml:L195-L200] [tests/solvers/openfoam/test_hull_manifest.py:L27-L40] |
| C04 | G2 / applicable | Execute V1: volume and physical wetted area against the rectangular-prism closed forms, relative error ≤1e-9 (1), with quantities in m³ and m² | [tests/naval_architecture/test_mesh_hydrostatics.py:L53-L64] |
| C05 | G2 / applicable | Execute V2: zero-rudder Nomoto solution, maximum absolute heading <1e-9 deg and lateral displacement <1e-9 m | [tests/naval_architecture/test_turning_circle_estimator.py:L36-L46] |
| C06 | G2 / applicable | Execute R2 refinement: advance and tactical-diameter relative difference ≤0.03 (1), outputs m; retain coarse and fine records without interpreting this regression check as physical validation | [tests/naval_architecture/test_turning_circle_estimator.py:L72-L84] |
| C07 | G3 / NOT_APPLICABLE | No production dynamics solver is run; the Nomoto ODE is a preliminary Python estimator, not the production-dynamics gate | [src/digitalmodel/naval_architecture/turning_circle.py:L3-L9] |
| C08 | G4 / NOT_APPLICABLE | No diffraction calculation is in the module workflow routes; geometry hydrostatics is checked under G2 | [src/digitalmodel/naval_architecture/workflow.py:L70-L84] |
| C09 | G5 / applicable | For each issued calculation, generate a record with input digests, code pin, function/node ID, units, reference, criterion and result: 0 missing fields (count), 0 reported results without a matching digest (count), 0 unresolved deviations (count) before clearance | AQG r0, sections 9 and 13; digest-bearing numerical payload: [src/digitalmodel/naval_architecture/mesh_results.py:L13-L40] [src/digitalmodel/naval_architecture/mesh_results.py:L45-L80] |

C09 is a required record-assembly check, not an existing package-wide appendix generator; provenance dictionaries alone do not provide the AQG signed record. [src/digitalmodel/naval_architecture/turning_circle_artifacts.py:L68-L86] (AQG r0, sections 9 and 13.)

## 6. Known failure modes and pitfalls

| Symptom / cause | Detection and disposition | Incident / evidence / date |
| --- | --- | --- |
| Missing compliance inputs can yield a passing check: absent depth returns pass; missing actual section modulus is replaced by a large default; an empty check list passes `all` | Proposed D01: remove required depth/section-modulus inputs and require 0 successful compliance conclusions (count). No detection check exists that rejects these defaults; do not use an incomplete compliance dictionary as independent design evidence | issue pending (3), 2026-10-15. [src/digitalmodel/naval_architecture/compliance.py:L45-L54] [src/digitalmodel/naval_architecture/compliance.py:L137-L162] |
| Unconfigured citation wrappers warn once and return empty citations; the legacy scalar APIs emit no sidecars | C02 catches a missing configured page, but not an unconfigured workspace; require 0 uncited standards-derived conclusions (count) before qualification | issue pending (4), 2026-10-15. [src/digitalmodel/naval_architecture/resistance.py:L61-L95] |
| Out-of-range curves-of-form interpolation clamps; density interpolation also clamps rather than refuses | Require 0 out-of-range inputs (count) against the supplied table bounds; the existing density tests intentionally exercise clamp behaviour | issue pending (6), 2026-10-15. [src/digitalmodel/naval_architecture/curves_of_form.py:L45-L55] [src/digitalmodel/naval_architecture/fundamentals.py:L112-L123] [tests/naval_architecture/test_na_fundamentals.py:L72-L78] |
| `natural_roll_period` accepts `c_roll` but uses a fixed gyration fraction | Proposed D02: vary `c_roll` for fixed beam/GM and compare with the documented parameter contract; 0 ignored coefficient changes (count). No detection check exists for parameter sensitivity; exclude this route as an adjustable-coefficient hand reference | issue pending (1), 2026-10-15. [src/digitalmodel/naval_architecture/seakeeping.py:L32-L52] |
| `flooded_gm` subtracts inertia in m⁴ divided by displacement in tonnes from GM in metres, without converting mass displacement to volume | Proposed D03: derive lost-inertia correction from an explicitly supplied volume/density basis; 0 dimensional inconsistencies (count). No detection check exists for the dimensional contract; this helper is excluded as a hand-check reference | issue pending (2), 2026-10-15. [src/digitalmodel/naval_architecture/damage_stability.py:L173-L182] |
| IACS UR S11 register and implementation name different editions | Proposed D04: compare the qualified calculation citation to the codes register; 0 edition mismatches (count). No detection check exists for this pair | issue pending (4), 2026-10-15. [docs/domains/codes-register.md:L127-L128] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L61-L81] |
| Resistance coefficient qualification remains separate from a passing regression suite | Require independent coefficient and total-resistance fixtures for the named revision; 0 unqualified governing coefficients (count). No complete independent detection check exists in this pack | [issue 2020](../../../../../../issues/2020); source attribution: [src/digitalmodel/naval_architecture/holtrop_coefficients.py:L1-L8] [src/digitalmodel/naval_architecture/holtrop_mennen.py:L170-L194] |
| Simplified RAO denominator fallback can replace a resonance singularity with a finite value | Require 0 fallback-limited results (count) in any qualified motion result, or a documented disposition; no detection check exists in the pack for this acceptance condition | issue pending (6), 2026-10-15. [src/digitalmodel/naval_architecture/seakeeping.py:L146-L158] |
| Short Nomoto run does not reach heading crossings | Existing `test_metrics_warn_when_heading_target_not_reached` requires missing metrics and warnings; require 0 `None` metrics (count) when quoting a turning diameter | [tests/naval_architecture/test_turning_circle_estimator.py:L87-L100] [src/digitalmodel/naval_architecture/turning_circle.py:L206-L234] |
| GHS-supplied hydrostatics are external evidence, not native solver qualification by this pack | C09 shall record external data identity, units and qualification; native licensed checks are NOT_RUN, prohibited for this pack | [issue 2089](../../../../../../issues/2089); [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L6-L10] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L11-L15] |

The baseline offline suite stops collection in three sweep modules because `xdist_group` is not registered under strict-marker checking; two section-4 nodes therefore have no execution outcome until that profile is repaired. Detection requires 0 unregistered-marker errors (count); the current collection error remains a baseline limitation, issue pending (8). [pytest.ini:L45-L58] [tests/naval_architecture/test_rudder_stock_torque_sweep.py:L17-L23] [tests/naval_architecture/test_turning_circle_estimator.py:L16-L22] [tests/naval_architecture/test_yaw_moment_sweep.py:L18-L24]

## 7. Lessons log

| Date | Entry | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-10-15 | Initial pack | Preserve the distinction between elementary imperial APIs, SI workflow inputs and mesh geometry units | [src/digitalmodel/naval_architecture/hydrostatics.py:L75-L95] [src/digitalmodel/naval_architecture/workflow_calculations.py:L20-L43] [src/digitalmodel/naval_architecture/mesh_hydrostatics.py:L6-L14] | Sections 2–3 declare the boundaries | issue pending (6) |

## 8. Ownership

The proposed clearance roles are the engineering calculations quality manager and designated deputy; the producing role is calculation engineering. Unresolved tolerances, source editions, ambiguous criteria and missing input evidence shall go to the family quality manager; cross-family conflicts shall go to the maintainer. Independent review is required before clearance. (AQG r0, section 11.)

UNVERIFIED: U4 — appointment of a named deputy and recording in a shared lessons index were not established; the pack assigns roles only, with follow-up in section 10.

## 9. Dependencies

| Dependency / route | Relationship and limit | Evidence |
| --- | --- | --- |
| Citation registry and schema | EN400 opt-in sidecars; configured resolution fails closed, unconfigured use can degrade | [src/digitalmodel/naval_architecture/fundamentals.py:L52-L88] |
| Hydrodynamics propeller/rudder | Maneuvering envelope composes Soding/Brix rudder authority | [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L279-L336] |
| Structural panel capacity | Naval compression/Smith helpers delegate buckling capacity | [src/digitalmodel/naval_architecture/hull_girder_strength.py:L350-L364] [src/digitalmodel/naval_architecture/hull_girder_smith.py:L133-L199] |
| Geometry readers | Optional geometry packages are loaded lazily for hull/BRep input; the dependency routes are inspected as text | [src/digitalmodel/naval_architecture/hull_ingest.py:L223-L235] [src/digitalmodel/naval_architecture/brep_tessellate.py:L154-L214] |
| GHS | External flooding hydrostatics and approved stability calculation; all four package mentions are listed here; version is not specified | [src/digitalmodel/naval_architecture/damage_stability_screening.py:L11-L15] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L45-L49] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L6-L10] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L35-L38] |
| OpenFOAM | Downstream target of normalized hull STL/manifest; both package mentions are listed; no solver version is specified | [src/digitalmodel/naval_architecture/hull_ingest.py:L1-L8] [src/digitalmodel/naval_architecture/hull_ingest.py:L1072-L1077] |
| Python workflows | Seven registry routes: yaw moment, stock torque, maneuvering envelope, damage stability, platform stability, resistance and seakeeping | [docs/registry/workflows.yaml:L790-L819] [docs/registry/workflows.yaml:L1054-L1089] |

### Implementation-map inventory

The map artifact pin is `b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407`; its declared data revision is `947fa50ff9a9e210de7e8f90e01b37add85c5954`; artifact SHA-256 is `ff8e59964cf4719b34caca3880e0b213a79c48546b49fb7fcb8ca6394e78001a`. [docs/registry/implementation-map.yaml:L1-L4]

The only map family with implementation rows inside this package is `wind-and-current-force-routes`: 2 in-package rows and 131 inventory rows overall; it is the only counted family. [docs/registry/implementation-map.yaml:L13775-L15055]

| Path | Symbol | Map source |
|---|---|---|
| `examples/demos/gtm/demo_03_deepwater_mudmat_installation.py` | `calc_splash_zone` | [docs/registry/implementation-map.yaml:L13782-L13790] |
| `examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py` | `calc_phase_3_splash` | [docs/registry/implementation-map.yaml:L13791-L13799] |
| `examples/demos/gtm/results_store_demo05.py` | `<module>` | [docs/registry/implementation-map.yaml:L13800-L13808] |
| `examples/demos/gtm/tests/test_demo_05_results_store.py` | `_good_case` | [docs/registry/implementation-map.yaml:L13809-L13817] |
| `examples/domains/fpso/fpso_mooring_analysis.py` | `calculate_environmental_forces` | [docs/registry/implementation-map.yaml:L13818-L13826] |
| `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | [docs/registry/implementation-map.yaml:L13827-L13837] |
| `examples/domains/ocimf/ocimf_demo.py` | `main` | [docs/registry/implementation-map.yaml:L13838-L13846] |
| `examples/domains/workflows/01_spread_mooring_8point.py` | `calculate_environmental_forces` | [docs/registry/implementation-map.yaml:L13847-L13855] |
| `examples/domains/workflows/03_calm_buoy.py` | `analyze_calm_buoy` | [docs/registry/implementation-map.yaml:L13856-L13864] |
| `examples/domains/workflows/05_jackup_installation.py` | `analyze_jackup_mooring` | [docs/registry/implementation-map.yaml:L13865-L13873] |
| `examples/domains/workflows/06_floating_wind_turbine.py` | `analyze_floating_wind_mooring` | [docs/registry/implementation-map.yaml:L13874-L13882] |
| `examples/domains/workflows/07_wave_energy_converter.py` | `analyze_wec_mooring` | [docs/registry/implementation-map.yaml:L13883-L13891] |
| `examples/domains/workflows/08_aquaculture_mooring.py` | `analyze_aquaculture_mooring` | [docs/registry/implementation-map.yaml:L13892-L13900] |
| `examples/domains/workflows/09_spm_terminal.py` | `analyze_spm_terminal` | [docs/registry/implementation-map.yaml:L13901-L13909] |
| `examples/domains/workflows/10_dp_backup_mooring.py` | `analyze_dp_backup_mooring` | [docs/registry/implementation-map.yaml:L13910-L13918] |
| `scripts/capabilities/build_onepagers.py` | `<module>` | [docs/registry/implementation-map.yaml:L13919-L13927] |
| `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `<module>` | [docs/registry/implementation-map.yaml:L13928-L13936] |
| `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `extract_ocimf_database` | [docs/registry/implementation-map.yaml:L13937-L13947] |
| `scripts/python/digitalmodel/legacy/generate_integration_charts.py` | `<module>` | [docs/registry/implementation-map.yaml:L13948-L13956] |
| `scripts/python/digitalmodel/legacy/generate_ocimf_report.py` | `main` | [docs/registry/implementation-map.yaml:L13957-L13965] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler._setup_test_data` | [docs/registry/implementation-map.yaml:L13966-L13976] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | [docs/registry/implementation-map.yaml:L13977-L13987] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.run_all` | [docs/registry/implementation-map.yaml:L13988-L14000] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `main` | [docs/registry/implementation-map.yaml:L14001-L14011] |
| `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | [docs/registry/implementation-map.yaml:L14012-L14022] |
| `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | [docs/registry/implementation-map.yaml:L14023-L14033] |
| `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_json_summary` | [docs/registry/implementation-map.yaml:L14034-L14044] |
| `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.run_complete_validation` | [docs/registry/implementation-map.yaml:L14045-L14060] |
| `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `<module>` | [docs/registry/implementation-map.yaml:L14061-L14069] |
| `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `main` | [docs/registry/implementation-map.yaml:L14070-L14078] |
| `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `make_polar_overlay` | [docs/registry/implementation-map.yaml:L14079-L14087] |
| `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `parse_args` | [docs/registry/implementation-map.yaml:L14088-L14096] |
| `src/digitalmodel/ansys/pressure_vessel.py` | `PressureVesselGenerator.generate_wind_load` | [docs/registry/implementation-map.yaml:L14097-L14105] |
| `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` | `SyntheticRopeDatabaseClient._generate_rope_spec` | [docs/registry/implementation-map.yaml:L14106-L14114] |
| `src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py` | `main` | [docs/registry/implementation-map.yaml:L14115-L14123] |
| `src/digitalmodel/drilling_riser/drift_off.py` | `environmental_force_n` | [docs/registry/implementation-map.yaml:L14124-L14132] |
| `src/digitalmodel/drilling_riser/envelope.py` | `CurrentProfile.drag_load_n_per_m` | [docs/registry/implementation-map.yaml:L14133-L14141] |
| `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `as_analysed_sections` | [docs/registry/implementation-map.yaml:L14142-L14150] |
| `src/digitalmodel/dynmoor_mooring/workflow.py` | `router` | [docs/registry/implementation-map.yaml:L14151-L14159] |
| `src/digitalmodel/engine.py` | `engine` | [docs/registry/implementation-map.yaml:L14160-L14171] |
| `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_wind_cmd` | [docs/registry/implementation-map.yaml:L14172-L14180] |
| `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_current_cmd` | [docs/registry/implementation-map.yaml:L14181-L14189] |
| `src/digitalmodel/hydrodynamics/cli.py` | `combined_env_cmd` | [docs/registry/implementation-map.yaml:L14190-L14198] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_total_viv_fatigue` | [docs/registry/implementation-map.yaml:L14199-L14207] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L14208-L14216] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_total_viv_fatigue` | [docs/registry/implementation-map.yaml:L14217-L14225] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L14226-L14234] |
| `src/digitalmodel/infrastructure/utils/ETL_components.py` | `ETL_components.transform_viv_current_reformat_1` | [docs/registry/implementation-map.yaml:L14235-L14243] |
| `src/digitalmodel/installation/installation_pamphlet/calculations.py` | `compute_splashzone_envelope` | [docs/registry/implementation-map.yaml:L14244-L14252] |
| `src/digitalmodel/installation/jumper_installation/calculations.py` | `calc_phase_3_splash` | [docs/registry/implementation-map.yaml:L14253-L14261] |
| `src/digitalmodel/marine_ops/marine_analysis/__main__.py` | `main` | [docs/registry/implementation-map.yaml:L14262-L14270] |
| `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | [docs/registry/implementation-map.yaml:L14271-L14281] |
| `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | [docs/registry/implementation-map.yaml:L14282-L14292] |
| `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `create_sample_database` | [docs/registry/implementation-map.yaml:L14293-L14303] |
| `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py` | `OCIMFWorkflow.router` | [docs/registry/implementation-map.yaml:L14304-L14312] |
| `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `<module>` | [docs/registry/implementation-map.yaml:L14313-L14321] |
| `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `extract_ocimf_database` | [docs/registry/implementation-map.yaml:L14322-L14332] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler._setup_test_data` | [docs/registry/implementation-map.yaml:L14333-L14345] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | [docs/registry/implementation-map.yaml:L14346-L14356] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.run_all` | [docs/registry/implementation-map.yaml:L14357-L14369] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `main` | [docs/registry/implementation-map.yaml:L14370-L14380] |
| `src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py` | `main` | [docs/registry/implementation-map.yaml:L14381-L14389] |
| `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | [docs/registry/implementation-map.yaml:L14390-L14400] |
| `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | [docs/registry/implementation-map.yaml:L14401-L14411] |
| `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_json_summary` | [docs/registry/implementation-map.yaml:L14412-L14422] |
| `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.run_complete_validation` | [docs/registry/implementation-map.yaml:L14423-L14438] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py` | `<module>` | [docs/registry/implementation-map.yaml:L14439-L14447] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_output_dir` | [docs/registry/implementation-map.yaml:L14448-L14456] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_cli_output_dir` | [docs/registry/implementation-map.yaml:L14457-L14465] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `load_data` | [docs/registry/implementation-map.yaml:L14466-L14474] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `main` | [docs/registry/implementation-map.yaml:L14475-L14486] |
| `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | [docs/registry/implementation-map.yaml:L14487-L14497] |
| `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | [docs/registry/implementation-map.yaml:L14498-L14508] |
| `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `create_sample_database` | [docs/registry/implementation-map.yaml:L14509-L14519] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._environmental_forces` | [docs/registry/implementation-map.yaml:L14520-L14530] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._ocimf_database_path` | [docs/registry/implementation-map.yaml:L14531-L14539] |
| `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `_resolve_arrow_direction_in_body_frame` | [docs/registry/implementation-map.yaml:L14540-L14548] |
| `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `polar_force_overlay` | [docs/registry/implementation-map.yaml:L14549-L14557] |
| `src/digitalmodel/naval_architecture/maneuvering_envelope.py` | `threshold_speed_for_steerage` | [docs/registry/implementation-map.yaml:L14558-L14566] |
| `src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py` | `run_maneuvering_envelope_sweep` | [docs/registry/implementation-map.yaml:L14567-L14575] |
| `src/digitalmodel/orcaflex/installation_analysis.py` | `SplashZoneInput.calculate_splash_zone_loads` | [docs/registry/implementation-map.yaml:L14576-L14584] |
| `src/digitalmodel/orcawave/drift_forces.py` | `compute_wind_current_drift` | [docs/registry/implementation-map.yaml:L14585-L14593] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` | `KnowledgeIntegrator._extract_configurations` | [docs/registry/implementation-map.yaml:L14594-L14602] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder.build` | [docs/registry/implementation-map.yaml:L14603-L14611] |
| `src/digitalmodel/solvers/orcaflex/reporting/extractors/loads_extractor.py` | `extract_loads` | [docs/registry/implementation-map.yaml:L14612-L14620] |
| `src/digitalmodel/structural/parachute/chute_assessment.py` | `calculate_dual_chute_drag` | [docs/registry/implementation-map.yaml:L14621-L14631] |
| `src/digitalmodel/structural/parachute/chute_assessment.py` | `assess_all_load_cases` | [docs/registry/implementation-map.yaml:L14632-L14643] |
| `src/digitalmodel/structural/parachute/chute_assessment.py` | `simulate_deployment_transient` | [docs/registry/implementation-map.yaml:L14644-L14653] |
| `src/digitalmodel/structural/parachute/chute_assessment.py` | `export_results_yaml` | [docs/registry/implementation-map.yaml:L14654-L14662] |
| `src/digitalmodel/structural/parachute/parachute_drag.py` | `calculate_drag_force` | [docs/registry/implementation-map.yaml:L14663-L14671] |
| `src/digitalmodel/subsea/mooring_analysis/cli.py` | `design_verification` | [docs/registry/implementation-map.yaml:L14672-L14680] |
| `src/digitalmodel/subsea/mooring_analysis/designer.py` | `MooringDesigner.calculate_environmental_loads` | [docs/registry/implementation-map.yaml:L14681-L14689] |
| `src/digitalmodel/subsea/mooring_analysis/dynmoor.py` | `build_wind_model.force` | [docs/registry/implementation-map.yaml:L14690-L14698] |
| `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `_phase_demand` | [docs/registry/implementation-map.yaml:L14699-L14707] |
| `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `hydrodynamic_loads` | [docs/registry/implementation-map.yaml:L14708-L14716] |
| `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `drag_force_per_meter` | [docs/registry/implementation-map.yaml:L14717-L14725] |
| `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lateral_stability_check` | [docs/registry/implementation-map.yaml:L14726-L14734] |
| `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `check_lateral_stability` | [docs/registry/implementation-map.yaml:L14735-L14745] |
| `src/digitalmodel/subsea/pipeline/integrity_workflows.py` | `run_on_bottom_stability` | [docs/registry/implementation-map.yaml:L14746-L14754] |
| `src/digitalmodel/subsea/viv_analysis/screening.py` | `VIVScreening.screen_with_current_profile` | [docs/registry/implementation-map.yaml:L14755-L14763] |
| `src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py` | `viv_analysis` | [docs/registry/implementation-map.yaml:L14764-L14772] |
| `src/digitalmodel/subsea/viv_analysis/vortex_shedding.py` | `VortexSheddingAnalyzer.analyze_with_profile` | [docs/registry/implementation-map.yaml:L14773-L14781] |
| `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_mooring_design_template` | [docs/registry/implementation-map.yaml:L14782-L14790] |
| `src/digitalmodel/workflows/workflow_automation/workflows.py` | `MooringSystemDesignWorkflow.create` | [docs/registry/implementation-map.yaml:L14791-L14799] |
| `tests/contracts/test_force_units_boundary_contract.py` | `<module>` | [docs/registry/implementation-map.yaml:L14800-L14808] |
| `tests/legal/test_published_pages_have_no_internal_paths.py` | `_load_producer` | [docs/registry/implementation-map.yaml:L14809-L14817] |
| `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `sample_database_path` | [docs/registry/implementation-map.yaml:L14818-L14826] |
| `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `ocimf_db` | [docs/registry/implementation-map.yaml:L14827-L14835] |
| `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `env_forces` | [docs/registry/implementation-map.yaml:L14836-L14844] |
| `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | [docs/registry/implementation-map.yaml:L14845-L14854] |
| `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py` | `TestOCIMFMooringIntegration.test_combined_environmental_forces` | [docs/registry/implementation-map.yaml:L14855-L14863] |
| `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `ocimf_database` | [docs/registry/implementation-map.yaml:L14864-L14873] |
| `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `pytest_benchmark_update_json` | [docs/registry/implementation-map.yaml:L14874-L14882] |
| `tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py` | `test_refactored_polar_signatures_match_fixture` | [docs/registry/implementation-map.yaml:L14883-L14891] |
| `tests/solvers/orcaflex/reporting/fixture_helpers.py` | `_build_loads` | [docs/registry/implementation-map.yaml:L14892-L14900] |
| `tests/structural/parachute/test_chute_assessment.py` | `TestAssessAllLoadCases.test_governing_force_within_1_percent_of_hand_calc` | [docs/registry/implementation-map.yaml:L14901-L14909] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_200_mph` | [docs/registry/implementation-map.yaml:L14910-L14919] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_250_mph` | [docs/registry/implementation-map.yaml:L14920-L14929] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_force_scales_with_v_squared` | [docs/registry/implementation-map.yaml:L14930-L14939] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_result_has_newtons` | [docs/registry/implementation-map.yaml:L14940-L14949] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_newtons` | [docs/registry/implementation-map.yaml:L14950-L14959] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_lbs` | [docs/registry/implementation-map.yaml:L14960-L14969] |
| `tests/subsea/catenary_riser/conftest.py` | `catenary_environment` | [docs/registry/implementation-map.yaml:L14970-L14978] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py` | `TestLiftForce.test_known_lift_force` | [docs/registry/implementation-map.yaml:L14979-L14987] |
| `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestDragForce.test_scenario_1` | [docs/registry/implementation-map.yaml:L14988-L14996] |
| `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestLiftForce.test_scenario_1` | [docs/registry/implementation-map.yaml:L14997-L15005] |
| `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_circular_mock` | [docs/registry/implementation-map.yaml:L15006-L15014] |
| `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_rectangular_mock` | [docs/registry/implementation-map.yaml:L15015-L15023] |
| `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_drag_force_per_meter` | [docs/registry/implementation-map.yaml:L15024-L15032] |
| `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_lift_force_per_meter` | [docs/registry/implementation-map.yaml:L15033-L15041] |
| `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | [docs/registry/implementation-map.yaml:L15042-L15052] |

### Counted-family grep and classification

The counted family is `wind-and-current-force-routes`; its three exact Python regular expressions produce 12 enclosing-definition hits across the package, and none of its signals matches `naval_architecture` case-insensitively and therefore none is excluded. The six formula definitions below are listed in section 3. The two mapped hits are `threshold_speed_for_steerage` and `run_maneuvering_envelope_sweep`. [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L211-L246] [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L233-L345]

| Additional formula definition to include in section 3 | Role and evidence |
|---|---|
| `floating_platform_stability.compute_wind_heel` | Computes wind force, heeling moment and equilibrium heel. [src/digitalmodel/naval_architecture/floating_platform_stability.py:L209-L252] |
| `maneuverability.rudder_lift_coefficient` | Computes a lift coefficient rather than delegating its formula. [src/digitalmodel/naval_architecture/maneuverability.py:L19-L38] |
| `maneuverability.rudder_normal_force` | Uses the lift coefficient but evaluates the dynamic-pressure/area force formula itself. [src/digitalmodel/naval_architecture/maneuverability.py:L41-L56] |
| `maneuvering_envelope.rudder_lift_slope_per_rad` | Evaluates a lift-slope formula itself. [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L144-L154] |
| `maneuvering_envelope.current_yaw_moment_ocimf` | Evaluates a current yaw moment itself. [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L253-L258] |
| `maneuvering_envelope.current_lateral_force_ocimf` | Evaluates a current lateral force itself. [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L261-L266] |

The remaining four unmapped hit definitions are the module-level documentation/comments in `maneuvering_envelope`, the `ManeuveringEnvelopeInput` data class, `validate_maneuvering_envelope_input`, and `_provenance_metadata`; these provide documentation, data, input validation and metadata rather than further force implementations. [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L3-L36] [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L250-L250] [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L89-L230]

A8 self-check: all six additional formula definitions are listed in section 3; the exact counted-family grep has 0 further unlisted formula definitions, within the pass criterion of at most 1 (count). Independent review remains pending.

### External dependencies named by the package

| Dependency | Every mention in package source | Interpretation from surrounding source |
|---|---|---|
| GHS | [src/digitalmodel/naval_architecture/damage_stability_screening.py:L11-L15] [src/digitalmodel/naval_architecture/damage_stability_screening.py:L45-L49] | External licensed tool supplies flooding hydrostatics and a class/submission calculation; screening is not that approved calculation. |
| GHS | [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L6-L10] [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L35-L38] | Hydrostatics may be user-supplied from an external tool, which remains the licensed cross-check. |
| OpenFOAM | [src/digitalmodel/naval_architecture/hull_ingest.py:L1-L8] [src/digitalmodel/naval_architecture/hull_ingest.py:L1072-L1077] | Geometry-normalisation output is described as ready for the external solver. |

These six textual mentions are the complete case-insensitive matches for `OpenFOAM|GHS` in the package before the pack is added; source is read as text only.

### Whole-repository follow-up search

A repository-wide Python-definition/formula grep includes all tracked folders, including `scripts/`; an additional scripts-only keyword search produces 67 textual hits. Readback identifies four force implementations absent from the map inventory: the wind/current methods listed below compute their own dynamic-pressure products, while the map lists each class's `calculate_total_forces` wrapper instead. These are outside-package map gaps and do not change the counted-family A8 outcome. [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L560-L626] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L574-L640]

- `marine_analysis.environmental_loading.ocimf.EnvironmentalForces.calculate_wind_forces`. [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L560-L593]
- `marine_analysis.environmental_loading.ocimf.EnvironmentalForces.calculate_current_forces`. [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L595-L626]
- `marine_engineering.environmental_loading.ocimf.EnvironmentalForces.calculate_wind_forces`. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L574-L607]
- `marine_engineering.environmental_loading.ocimf.EnvironmentalForces.calculate_current_forces`. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L609-L640]

Further implementations of uncounted naval method families include independent trapezoidal section hydrostatics in `visualization.design_tools.hull_hydrostatics.HullHydrostatics`, and independent friction-correlation/Froude formulas in the external-solver validation package. [src/digitalmodel/visualization/design_tools/hull_hydrostatics.py:L13-L161] [src/digitalmodel/solvers/openfoam/validation/ship_resistance.py:L116-L135]

Scripts readback distinguishes delegates from independent implementations: `reproduce_holtrop_discrepancy.diagnose` delegates resistance components to the naval module and adds diagnostic nondimensionalisation; `build_ship_resistance_explorer.build_study` delegates resistance and power and adds a Froude-number expression; neither is an additional resistance implementation. [scripts/validation/reproduce_holtrop_discrepancy.py:L24-L53] [scripts/capabilities/build_ship_resistance_explorer.py:L46-L63]

Map follow-up, issue pending (5), should add rows for naval hydrostatics, intact/damage stability, resistance/friction/scaling, seakeeping, manoeuvrability, hull-girder strength, scantlings, loading/compliance, hull geometry and propeller/gyradius methods covered by section 3 but currently without in-package map rows; the counted family also needs its additional formula rows and outside-package force-method gaps recorded. This is inventory maintenance, not a change to the current A8 counted-family set.

## 10. Coverage and open items

| Item | Limit / next check | Responsible role / issue |
| --- | --- | --- |
| O1 / U1, U3 | Standards applicability, editions and Citation coverage are not fully qualified; resolve the S11 register discrepancy and identify every governing clause without transcribing licensed tables | Engineering calculations quality manager; issue pending (4); resistance source work also [issue 2020](../../../../../../issues/2020) |
| O2 / U2 | Open and check the independent EN400 source before promoting textbook-labelled regression cases to validated examples | Engineering calculations quality manager; issue pending (4) |
| O3 | Resolve the unused roll coefficient, flooded-GM dimensional contract and optimistic compliance defaults; no module code is changed by this pack | Calculation engineering; issue pending (1), issue pending (2), issue pending (3) |
| O4 | Extend naval map families and additional force-formula rows; outside-package formulas remain map gaps rather than A8 misses | Maintainer; issue pending (5); related mesh/resistance scope [issue 2239](../../../../../../issues/2239) |
| O5 / U5 | Independent qualification of extended helpers and complete long-document review exceeds the main-path coverage of this revision; qualify loading/trim, platform approximations, Smith, registry estimates, geometry repairs/occlusion, gyradius and generated extracted constants separately | Engineering calculations quality manager; issue pending (6) |
| O6 | Native GHS parser/output qualification is NOT_RUN; external hydrostatics are not a native run result | Engineering calculations quality manager; [issue 2089](../../../../../../issues/2089) |
| O7 / U4 | Deputy appointment and a shared lessons-index entry are not established in this revision | Maintainer; issue pending (6) |
| O8 | Default second-quadrant propeller torque contradicts its sign documentation; no sign-detection test exists for that quadrant | Calculation engineering; issue pending (7); [src/digitalmodel/naval_architecture/propeller.py:L233-L252] [tests/naval_architecture/test_propeller_4_quadrant.py:L243-L278] |
| O9 | Strict-marker collection errors prevent three sweep modules from collecting and leave their test-node total unestablished; retain the errors and repair the test dependency/marker profile before claiming complete execution | Maintainer; issue pending (8) |

UNVERIFIED: U5 — source links establish dependency routes, but a cross-module inventory-row reconciliation and solver-version qualification were not completed; O5, issue pending (6).

### Reading and test inventory

The main-path source inspection covers the elementary functions, workflow calculations, maneuverability/envelope, yaw/stock/turning estimators, and the supplemental calculation files below; all package Python files and discovered tests have full static text reads; long documents have targeted cited-span inspection plus retained discovery extracts. This is an inspection inventory, not a claim of complete independent method validation; O5 holds the remaining independent qualification and long-document review, issue pending (6).

Package Python inventory (full static text reads; independent qualification remains bounded; O5, issue pending (6)):

- `src/digitalmodel/naval_architecture/__init__.py`. [src/digitalmodel/naval_architecture/__init__.py:L1-L1]
- `src/digitalmodel/naval_architecture/appendage_submergence.py`. [src/digitalmodel/naval_architecture/appendage_submergence.py:L1-L1]
- `src/digitalmodel/naval_architecture/brep_tessellate.py`. [src/digitalmodel/naval_architecture/brep_tessellate.py:L1-L1]
- `src/digitalmodel/naval_architecture/compliance.py`. [src/digitalmodel/naval_architecture/compliance.py:L1-L1]
- `src/digitalmodel/naval_architecture/constants.py`. [src/digitalmodel/naval_architecture/constants.py:L1-L1]
- `src/digitalmodel/naval_architecture/curves_of_form.py`. [src/digitalmodel/naval_architecture/curves_of_form.py:L1-L1]
- `src/digitalmodel/naval_architecture/damage_stability.py`. [src/digitalmodel/naval_architecture/damage_stability.py:L1-L1]
- `src/digitalmodel/naval_architecture/damage_stability_screening.py`. [src/digitalmodel/naval_architecture/damage_stability_screening.py:L1-L1]
- `src/digitalmodel/naval_architecture/data/__init__.py`. [src/digitalmodel/naval_architecture/data/__init__.py:L1-L1]
- `src/digitalmodel/naval_architecture/floating_platform_stability.py`. [src/digitalmodel/naval_architecture/floating_platform_stability.py:L1-L1]
- `src/digitalmodel/naval_architecture/friction_scaling.py`. [src/digitalmodel/naval_architecture/friction_scaling.py:L1-L1]
- `src/digitalmodel/naval_architecture/fundamentals.py`. [src/digitalmodel/naval_architecture/fundamentals.py:L1-L1]
- `src/digitalmodel/naval_architecture/gyradius.py`. [src/digitalmodel/naval_architecture/gyradius.py:L1-L1]
- `src/digitalmodel/naval_architecture/holtrop_coefficients.py`. [src/digitalmodel/naval_architecture/holtrop_coefficients.py:L1-L1]
- `src/digitalmodel/naval_architecture/holtrop_mennen.py`. [src/digitalmodel/naval_architecture/holtrop_mennen.py:L1-L1]
- `src/digitalmodel/naval_architecture/hull_cap.py`. [src/digitalmodel/naval_architecture/hull_cap.py:L1-L1]
- `src/digitalmodel/naval_architecture/hull_fixtures.py`. [src/digitalmodel/naval_architecture/hull_fixtures.py:L1-L1]
- `src/digitalmodel/naval_architecture/hull_form.py`. [src/digitalmodel/naval_architecture/hull_form.py:L1-L1]
- `src/digitalmodel/naval_architecture/hull_girder_screening.py`. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L1-L1]
- `src/digitalmodel/naval_architecture/hull_girder_smith.py`. [src/digitalmodel/naval_architecture/hull_girder_smith.py:L1-L1]
- `src/digitalmodel/naval_architecture/hull_girder_strength.py`. [src/digitalmodel/naval_architecture/hull_girder_strength.py:L1-L1]
- `src/digitalmodel/naval_architecture/hull_ingest.py`. [src/digitalmodel/naval_architecture/hull_ingest.py:L1-L1]
- `src/digitalmodel/naval_architecture/hull_properties.py`. [src/digitalmodel/naval_architecture/hull_properties.py:L1-L1]
- `src/digitalmodel/naval_architecture/hull_regions.py`. [src/digitalmodel/naval_architecture/hull_regions.py:L1-L1]
- `src/digitalmodel/naval_architecture/hydrostatics.py`. [src/digitalmodel/naval_architecture/hydrostatics.py:L1-L1]
- `src/digitalmodel/naval_architecture/integration.py`. [src/digitalmodel/naval_architecture/integration.py:L1-L1]
- `src/digitalmodel/naval_architecture/kcs_geometry.py`. [src/digitalmodel/naval_architecture/kcs_geometry.py:L1-L1]
- `src/digitalmodel/naval_architecture/knowledge.py`. [src/digitalmodel/naval_architecture/knowledge.py:L1-L1]
- `src/digitalmodel/naval_architecture/loading_computer.py`. [src/digitalmodel/naval_architecture/loading_computer.py:L1-L1]
- `src/digitalmodel/naval_architecture/maneuverability.py`. [src/digitalmodel/naval_architecture/maneuverability.py:L1-L1]
- `src/digitalmodel/naval_architecture/maneuvering_envelope.py`. [src/digitalmodel/naval_architecture/maneuvering_envelope.py:L1-L1]
- `src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py`. [src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py:L1-L1]
- `src/digitalmodel/naval_architecture/mesh_clipping.py`. [src/digitalmodel/naval_architecture/mesh_clipping.py:L1-L1]
- `src/digitalmodel/naval_architecture/mesh_geometry.py`. [src/digitalmodel/naval_architecture/mesh_geometry.py:L1-L1]
- `src/digitalmodel/naval_architecture/mesh_hydrostatics.py`. [src/digitalmodel/naval_architecture/mesh_hydrostatics.py:L1-L1]
- `src/digitalmodel/naval_architecture/mesh_results.py`. [src/digitalmodel/naval_architecture/mesh_results.py:L1-L1]
- `src/digitalmodel/naval_architecture/mesh_sections.py`. [src/digitalmodel/naval_architecture/mesh_sections.py:L1-L1]
- `src/digitalmodel/naval_architecture/mesh_topology.py`. [src/digitalmodel/naval_architecture/mesh_topology.py:L1-L1]
- `src/digitalmodel/naval_architecture/mesh_validation.py`. [src/digitalmodel/naval_architecture/mesh_validation.py:L1-L1]
- `src/digitalmodel/naval_architecture/prohaska.py`. [src/digitalmodel/naval_architecture/prohaska.py:L1-L1]
- `src/digitalmodel/naval_architecture/propeller.py`. [src/digitalmodel/naval_architecture/propeller.py:L1-L1]
- `src/digitalmodel/naval_architecture/region_report.py`. [src/digitalmodel/naval_architecture/region_report.py:L1-L1]
- `src/digitalmodel/naval_architecture/resistance.py`. [src/digitalmodel/naval_architecture/resistance.py:L1-L1]
- `src/digitalmodel/naval_architecture/rudder_stock_torque.py`. [src/digitalmodel/naval_architecture/rudder_stock_torque.py:L1-L1]
- `src/digitalmodel/naval_architecture/scantlings.py`. [src/digitalmodel/naval_architecture/scantlings.py:L1-L1]
- `src/digitalmodel/naval_architecture/seakeeping.py`. [src/digitalmodel/naval_architecture/seakeeping.py:L1-L1]
- `src/digitalmodel/naval_architecture/ship_data.py`. [src/digitalmodel/naval_architecture/ship_data.py:L1-L1]
- `src/digitalmodel/naval_architecture/ship_dimensions.py`. [src/digitalmodel/naval_architecture/ship_dimensions.py:L1-L1]
- `src/digitalmodel/naval_architecture/solid_occlusion.py`. [src/digitalmodel/naval_architecture/solid_occlusion.py:L1-L1]
- `src/digitalmodel/naval_architecture/stability.py`. [src/digitalmodel/naval_architecture/stability.py:L1-L1]
- `src/digitalmodel/naval_architecture/submarines.py`. [src/digitalmodel/naval_architecture/submarines.py:L1-L1]
- `src/digitalmodel/naval_architecture/symmetry_cut.py`. [src/digitalmodel/naval_architecture/symmetry_cut.py:L1-L1]
- `src/digitalmodel/naval_architecture/trim_hydrostatics.py`. [src/digitalmodel/naval_architecture/trim_hydrostatics.py:L1-L1]
- `src/digitalmodel/naval_architecture/turning_circle.py`. [src/digitalmodel/naval_architecture/turning_circle.py:L1-L1]
- `src/digitalmodel/naval_architecture/turning_circle_artifacts.py`. [src/digitalmodel/naval_architecture/turning_circle_artifacts.py:L1-L1]
- `src/digitalmodel/naval_architecture/vessel_stability_screening.py`. [src/digitalmodel/naval_architecture/vessel_stability_screening.py:L1-L1]
- `src/digitalmodel/naval_architecture/wigley_geometry.py`. [src/digitalmodel/naval_architecture/wigley_geometry.py:L1-L1]
- `src/digitalmodel/naval_architecture/workflow.py`. [src/digitalmodel/naval_architecture/workflow.py:L1-L1]
- `src/digitalmodel/naval_architecture/workflow_calculations.py`. [src/digitalmodel/naval_architecture/workflow_calculations.py:L1-L1]
- `src/digitalmodel/naval_architecture/yaw_moment.py`. [src/digitalmodel/naval_architecture/yaw_moment.py:L1-L1]

Offline suite files (execution statuses will be recorded below; O5, issue pending (6)):

- `tests/naval_architecture/test_alt_fuel_ship_sizing.py`. [tests/naval_architecture/test_alt_fuel_ship_sizing.py:L1-L1]
- `tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py`. [tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py:L1-L1]
- `tests/naval_architecture/test_array_integrity.py`. [tests/naval_architecture/test_array_integrity.py:L1-L1]
- `tests/naval_architecture/test_brep_tessellate.py`. [tests/naval_architecture/test_brep_tessellate.py:L1-L1]
- `tests/naval_architecture/test_compliance.py`. [tests/naval_architecture/test_compliance.py:L1-L1]
- `tests/naval_architecture/test_damage_stability.py`. [tests/naval_architecture/test_damage_stability.py:L1-L1]
- `tests/naval_architecture/test_damage_stability_screening.py`. [tests/naval_architecture/test_damage_stability_screening.py:L1-L1]
- `tests/naval_architecture/test_en400_worked_examples.py`. [tests/naval_architecture/test_en400_worked_examples.py:L1-L1]
- `tests/naval_architecture/test_floating_platform_stability.py`. [tests/naval_architecture/test_floating_platform_stability.py:L1-L1]
- `tests/naval_architecture/test_floating_platform_stability_extended.py`. [tests/naval_architecture/test_floating_platform_stability_extended.py:L1-L1]
- `tests/naval_architecture/test_friction_scaling.py`. [tests/naval_architecture/test_friction_scaling.py:L1-L1]
- `tests/naval_architecture/test_gyradius.py`. [tests/naval_architecture/test_gyradius.py:L1-L1]
- `tests/naval_architecture/test_gyradius_extended.py`. [tests/naval_architecture/test_gyradius_extended.py:L1-L1]
- `tests/naval_architecture/test_gz_curves.py`. [tests/naval_architecture/test_gz_curves.py:L1-L1]
- `tests/naval_architecture/test_holtrop_mennen.py`. [tests/naval_architecture/test_holtrop_mennen.py:L1-L1]
- `tests/naval_architecture/test_holtrop_reproduction.py`. [tests/naval_architecture/test_holtrop_reproduction.py:L1-L1]
- `tests/naval_architecture/test_hull_cap.py`. [tests/naval_architecture/test_hull_cap.py:L1-L1]
- `tests/naval_architecture/test_hull_form.py`. [tests/naval_architecture/test_hull_form.py:L1-L1]
- `tests/naval_architecture/test_hull_girder_screening.py`. [tests/naval_architecture/test_hull_girder_screening.py:L1-L1]
- `tests/naval_architecture/test_hull_girder_screening_workflow.py`. [tests/naval_architecture/test_hull_girder_screening_workflow.py:L1-L1]
- `tests/naval_architecture/test_hull_girder_smith.py`. [tests/naval_architecture/test_hull_girder_smith.py:L1-L1]
- `tests/naval_architecture/test_hull_girder_strength.py`. [tests/naval_architecture/test_hull_girder_strength.py:L1-L1]
- `tests/naval_architecture/test_hull_ingest.py`. [tests/naval_architecture/test_hull_ingest.py:L1-L1]
- `tests/naval_architecture/test_hull_regions.py`. [tests/naval_architecture/test_hull_regions.py:L1-L1]
- `tests/naval_architecture/test_hydrostatics_citations.py`. [tests/naval_architecture/test_hydrostatics_citations.py:L1-L1]
- `tests/naval_architecture/test_kcs_geometry.py`. [tests/naval_architecture/test_kcs_geometry.py:L1-L1]
- `tests/naval_architecture/test_knowledge.py`. [tests/naval_architecture/test_knowledge.py:L1-L1]
- `tests/naval_architecture/test_loading_computer.py`. [tests/naval_architecture/test_loading_computer.py:L1-L1]
- `tests/naval_architecture/test_maneuverability.py`. [tests/naval_architecture/test_maneuverability.py:L1-L1]
- `tests/naval_architecture/test_mesh_benchmark.py`. [tests/naval_architecture/test_mesh_benchmark.py:L1-L1]
- `tests/naval_architecture/test_mesh_hydrostatics.py`. [tests/naval_architecture/test_mesh_hydrostatics.py:L1-L1]
- `tests/naval_architecture/test_mesh_pickle_generator.py`. [tests/naval_architecture/test_mesh_pickle_generator.py:L1-L1]
- `tests/naval_architecture/test_mesh_r02.py`. [tests/naval_architecture/test_mesh_r02.py:L1-L1]
- `tests/naval_architecture/test_na_fundamentals.py`. [tests/naval_architecture/test_na_fundamentals.py:L1-L1]
- `tests/naval_architecture/test_na_fundamentals_citations.py`. [tests/naval_architecture/test_na_fundamentals_citations.py:L1-L1]
- `tests/naval_architecture/test_propeller_4_quadrant.py`. [tests/naval_architecture/test_propeller_4_quadrant.py:L1-L1]
- `tests/naval_architecture/test_resistance_citations.py`. [tests/naval_architecture/test_resistance_citations.py:L1-L1]
- `tests/naval_architecture/test_rudder_stock_torque_sweep.py`. [tests/naval_architecture/test_rudder_stock_torque_sweep.py:L1-L1]
- `tests/naval_architecture/test_scantlings.py`. [tests/naval_architecture/test_scantlings.py:L1-L1]
- `tests/naval_architecture/test_seakeeping.py`. [tests/naval_architecture/test_seakeeping.py:L1-L1]
- `tests/naval_architecture/test_seakeeping_citations.py`. [tests/naval_architecture/test_seakeeping_citations.py:L1-L1]
- `tests/naval_architecture/test_ship_data.py`. [tests/naval_architecture/test_ship_data.py:L1-L1]
- `tests/naval_architecture/test_ship_dimensions_template.py`. [tests/naval_architecture/test_ship_dimensions_template.py:L1-L1]
- `tests/naval_architecture/test_stability_citations.py`. [tests/naval_architecture/test_stability_citations.py:L1-L1]
- `tests/naval_architecture/test_transfer_citation_regressions.py`. [tests/naval_architecture/test_transfer_citation_regressions.py:L1-L1]
- `tests/naval_architecture/test_trim_hydrostatics.py`. [tests/naval_architecture/test_trim_hydrostatics.py:L1-L1]
- `tests/naval_architecture/test_turning_circle_estimator.py`. [tests/naval_architecture/test_turning_circle_estimator.py:L1-L1]
- `tests/naval_architecture/test_vessel_fleet_adapter.py`. [tests/naval_architecture/test_vessel_fleet_adapter.py:L1-L1]
- `tests/naval_architecture/test_vessel_stability_screening.py`. [tests/naval_architecture/test_vessel_stability_screening.py:L1-L1]
- `tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py`. [tests/naval_architecture/test_vessel_stability_screening_damage_workflow.py:L1-L1]
- `tests/naval_architecture/test_vessel_stability_screening_workflow.py`. [tests/naval_architecture/test_vessel_stability_screening_workflow.py:L1-L1]
- `tests/naval_architecture/test_wigley_geometry.py`. [tests/naval_architecture/test_wigley_geometry.py:L1-L1]
- `tests/naval_architecture/test_yaw_moment_sweep.py`. [tests/naval_architecture/test_yaw_moment_sweep.py:L1-L1]

Additional offline test files discovered by module imports:

- `tests/solvers/openfoam/test_hull_manifest.py`. [tests/solvers/openfoam/test_hull_manifest.py:L1-L1]
- `tests/solvers/openfoam/test_mesh_pipeline.py`. [tests/solvers/openfoam/test_mesh_pipeline.py:L1-L1]
- `tests/solvers/openfoam/validation/test_referent_free_resistance.py`. [tests/solvers/openfoam/validation/test_referent_free_resistance.py:L1-L1]
- `tests/test_maneuvering_envelope.py`. [tests/test_maneuvering_envelope.py:L1-L1]
- `tests/docs/test_digitalmodel_routing_contract.py`. [tests/docs/test_digitalmodel_routing_contract.py:L1-L1]

Documentation discovery inventory (bounded excerpts and cited spans; O5, issue pending (6)):

- `docs/api/hydro/rudder-maneuvering-explorer.html`. [docs/api/hydro/rudder-maneuvering-explorer.html:L1-L1]
- `docs/api/structural/ship-resistance-explorer.html`. [docs/api/structural/ship-resistance-explorer.html:L1-L1]
- `docs/capability-map/capabilities-added.yml`. [docs/capability-map/capabilities-added.yml:L1-L1]
- `docs/capability-map/capabilities-clusters.yml`. [docs/capability-map/capabilities-clusters.yml:L1-L1]
- `docs/capability-map/capabilities-ia-spec-1444.md`. [docs/capability-map/capabilities-ia-spec-1444.md:L1-L1]
- `docs/capability-map/capabilities-inventory.json`. [docs/capability-map/capabilities-inventory.json:L1-L1]
- `docs/capability-map/capabilities-sections.yml`. [docs/capability-map/capabilities-sections.yml:L1-L1]
- `docs/domains/codes-register.md`. [docs/domains/codes-register.md:L1-L1]
- `docs/domains/hull-girder-smith-validation-2026-06-28.md`. [docs/domains/hull-girder-smith-validation-2026-06-28.md:L1-L1]
- `docs/domains/hull-girder-strength-validation-2026-06-28.md`. [docs/domains/hull-girder-strength-validation-2026-06-28.md:L1-L1]
- `docs/domains/hull_library/hullprod-curvature-screening-evaluation.md`. [docs/domains/hull_library/hullprod-curvature-screening-evaluation.md:L1-L1]
- `docs/domains/hydrodynamics/propeller-4-quadrant-data.md`. [docs/domains/hydrodynamics/propeller-4-quadrant-data.md:L1-L1]
- `docs/domains/hydrodynamics/propeller-rudder-literature.md`. [docs/domains/hydrodynamics/propeller-rudder-literature.md:L1-L1]
- `docs/domains/manoeuvring-rudder-reference.md`. [docs/domains/manoeuvring-rudder-reference.md:L1-L1]
- `docs/domains/marine-engineering/rudder-stock-torque-sweep.md`. [docs/domains/marine-engineering/rudder-stock-torque-sweep.md:L1-L1]
- `docs/domains/marine-engineering/yaw-moment-sweep.md`. [docs/domains/marine-engineering/yaw-moment-sweep.md:L1-L1]
- `docs/domains/scantlings-validation-2026-06-28.md`. [docs/domains/scantlings-validation-2026-06-28.md:L1-L1]
- `docs/domains/ship-design/alt-fuel-lh2-sizing.md`. [docs/domains/ship-design/alt-fuel-lh2-sizing.md:L1-L1]
- `docs/domains/ship-design/hull-girder-strength-screening.md`. [docs/domains/ship-design/hull-girder-strength-screening.md:L1-L1]
- `docs/domains/ship-design/stiff_vs_tender_ships.md`. [docs/domains/ship-design/stiff_vs_tender_ships.md:L1-L1]
- `docs/domains/ship-design/vessel-stability-screening.md`. [docs/domains/ship-design/vessel-stability-screening.md:L1-L1]
- `docs/maps/digitalmodel-operator-map.md`. [docs/maps/digitalmodel-operator-map.md:L1-L1]
- `docs/plans/2026-05-05-issue-457-ship-dimensions-template-loader.md`. [docs/plans/2026-05-05-issue-457-ship-dimensions-template-loader.md:L1-L1]
- `docs/plans/2026-05-05-issue-461-recover-ship-dimensions-template.md`. [docs/plans/2026-05-05-issue-461-recover-ship-dimensions-template.md:L1-L1]
- `docs/plans/2026-05-05-issue-463-ship-dimensions-manual-curation.md`. [docs/plans/2026-05-05-issue-463-ship-dimensions-manual-curation.md:L1-L1]
- `docs/plans/2026-05-05-issue-483-curves-py-decomposition.md`. [docs/plans/2026-05-05-issue-483-curves-py-decomposition.md:L1-L1]
- `docs/plans/2026-05-07-issue-596-repo-structure-normalization.md`. [docs/plans/2026-05-07-issue-596-repo-structure-normalization.md:L1-L1]
- `docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md`. [docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md:L1-L1]
- `docs/plans/2026-06-11-issue-700-ci-baseline-domain-gates.md`. [docs/plans/2026-06-11-issue-700-ci-baseline-domain-gates.md:L1-L1]
- `docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md`. [docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md:L1-L1]
- `docs/plans/2026-09-11-issue-2089-ghs-canary.html`. [docs/plans/2026-09-11-issue-2089-ghs-canary.html:L1-L1]
- `docs/plans/2026-09-11-issue-2089-ghs-canary.md`. [docs/plans/2026-09-11-issue-2089-ghs-canary.md:L1-L1]
- `docs/plans/2026-09-27-issue-2191-parametric-monohull-form.md`. [docs/plans/2026-09-27-issue-2191-parametric-monohull-form.md:L1-L1]
- `docs/plans/2026-09-27-issue-2239-analytical-resistance-methods.md`. [docs/plans/2026-09-27-issue-2239-analytical-resistance-methods.md:L1-L1]
- `docs/plans/2026-10-10-sr2314-r02.html`. [docs/plans/2026-10-10-sr2314-r02.html:L1-L1]
- `docs/plans/evidence/2254/accepted-manifest.json`. [docs/plans/evidence/2254/accepted-manifest.json:L1-L1]
- `docs/plans/evidence/2254/claude-r2-review.md`. [docs/plans/evidence/2254/claude-r2-review.md:L1-L1]
- `docs/plans/evidence/2254/plan-review-disposition.md`. [docs/plans/evidence/2254/plan-review-disposition.md:L1-L1]
- `docs/plans/evidence/2254/validation.html`. [docs/plans/evidence/2254/validation.html:L1-L1]
- `docs/plans/evidence/sr2314/claude-artifact-r1.md`. [docs/plans/evidence/sr2314/claude-artifact-r1.md:L1-L1]
- `docs/plans/evidence/sr2314/claude-plan-r1.md`. [docs/plans/evidence/sr2314/claude-plan-r1.md:L1-L1]
- `docs/plans/evidence/sr2314/final-acceptance.json`. [docs/plans/evidence/sr2314/final-acceptance.json:L1-L1]
- `docs/plans/evidence/sr2314/focused-environment-matrix.json`. [docs/plans/evidence/sr2314/focused-environment-matrix.json:L1-L1]
- `docs/plans/evidence/sr2314/fz2344-plan.md`. [docs/plans/evidence/sr2314/fz2344-plan.md:L1-L1]
- `docs/plans/evidence/sr2314/fz2344-review-receipt.json`. [docs/plans/evidence/sr2314/fz2344-review-receipt.json:L1-L1]
- `docs/plans/evidence/sr2314/fz2344-validation.html`. [docs/plans/evidence/sr2314/fz2344-validation.html:L1-L1]
- `docs/plans/evidence/sr2314/generator-isolation.json`. [docs/plans/evidence/sr2314/generator-isolation.json:L1-L1]
- `docs/plans/evidence/sr2314/holtrop-reproduction.html`. [docs/plans/evidence/sr2314/holtrop-reproduction.html:L1-L1]
- `docs/plans/evidence/sr2314/holtrop-reproduction.json`. [docs/plans/evidence/sr2314/holtrop-reproduction.json:L1-L1]
- `docs/plans/evidence/sr2314/mesh-performance.html`. [docs/plans/evidence/sr2314/mesh-performance.html:L1-L1]
- `docs/plans/evidence/sr2314/validation.html`. [docs/plans/evidence/sr2314/validation.html:L1-L1]
- `docs/registry/module-routing.yaml`. [docs/registry/module-routing.yaml:L1-L1]
- `docs/reviews/hull-library-cross-review.md`. [docs/reviews/hull-library-cross-review.md:L1-L1]
- `docs/vision/CALCULATIONS-VISION.md`. [docs/vision/CALCULATIONS-VISION.md:L1-L1]

One demonstration locator containing an identity is retained only in the private scratch reading manifest; no values or name from it enter this public pack. Its public-wording disposition remains O5, issue pending (6).

### Execution record

Common command form: `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`. The full-path executable, external project environment and distinct temporary roots are used; strict-marker checking is retained and no addopts override is used. Code pin: `e6e540259a1c555eeb0c151468ec959b4217407e`. [pytest.ini:L45-L58]

The original command uses `tests/naval_architecture/ tests/contracts/` and stops at collection: BASE 0 executed / 0 passed / 0 failed / 1 skipped / 3 collection errors; 1,525 collected items are not executed by that invocation. AFTER: 0 executed / 0 passed / 0 failed / 1 skipped / 3 collection errors, matching BASE. The partitioned runs below retain every one of the 53 naval test files, the contract directory and the five additional files listed above: 59 path entries with none removed. Each marker-error file is attempted separately, and the other files remain in their lexical-order groups.

| Scope | BASE run/pass/fail/skip/XFAIL/errors | AFTER run/pass/fail/skip/XFAIL/errors | Unexecuted coverage |
| --- | --- | --- | --- |
| 43 explicit paths; naval range `test_alt_fuel_ship_sizing.py` to `test_resistance_citations.py` | 1376/1375/0/5/1/0 | 1376/1375/0/5/1/0 | 5 skip records |
| `tests/naval_architecture/test_rudder_stock_torque_sweep.py` | 0/0/0/0/0/1 | 0/0/0/0/0/1 | Module collection blocked; node total unestablished |
| 8 explicit paths; naval range `test_scantlings.py` to `test_trim_hydrostatics.py` | 102/102/0/1/0/0 | 102/102/0/1/0/0 | 1 skip records |
| `tests/naval_architecture/test_turning_circle_estimator.py` | 0/0/0/0/0/1 | 0/0/0/0/0/1 | Module collection blocked; node total unestablished |
| 5 explicit paths; naval range `test_vessel_fleet_adapter.py` to `test_wigley_geometry.py` | 136/136/0/4/0/0 | 136/136/0/4/0/0 | 4 skip records |
| `tests/naval_architecture/test_yaw_moment_sweep.py` | 0/0/0/0/0/1 | 0/0/0/0/0/1 | Module collection blocked; node total unestablished |

The three collection errors read: ``'xdist_group' not found in `markers` configuration option``. They affect `test_rudder_stock_torque_sweep.py`, `test_turning_circle_estimator.py` and `test_yaw_moment_sweep.py`; each module's uncollected node total remains unestablished (O9, issue pending (8)). [tests/naval_architecture/test_rudder_stock_torque_sweep.py:L17-L23] [tests/naval_architecture/test_turning_circle_estimator.py:L16-L22] [tests/naval_architecture/test_yaw_moment_sweep.py:L18-L24]

In the partitioned BASE runs, the package test folder has 1,060 PASSED node records, 1 XFAIL and 9 skipped collected cases: 1,070 collected nodes. A further CAD module is skipped at collection and three marker-bearing modules cannot collect, so this is a partial re-count rather than the full module test total. The full partitioned scope, including contracts and supplemental tests, reports 1,613 passed, 0 failed, 10 skip records and 1 XFAIL; the 10 include the CAD module-level skip. The three collection errors are retained separately from executed-test failures. Skipped cases are NOT_RUN coverage; the complete NOT_RUN test-node count cannot be calculated for uncollected modules.

| Non-passing scope | Observed reason | Follow-up |
| --- | --- | --- |
| `tests/naval_architecture/test_brep_tessellate.py (module collection)` | SKIPPED / NOT_RUN: needs the 'cad' extra | O5, issue pending (6); [tests/naval_architecture/test_brep_tessellate.py:L38-L38] |
| `tests/naval_architecture/test_hull_cap.py (module collection)` | SKIPPED / NOT_RUN: set DIGITALMODEL_3DM_HULL to a readable .3dm hull to run this | O5, issue pending (6); [tests/naval_architecture/test_hull_cap.py:L648-L648] |
| `tests/naval_architecture/test_hull_ingest.py (module collection)` | SKIPPED / NOT_RUN: set DIGITALMODEL_HULL_3DM to a readable .3dm to run this | O5, issue pending (6); [tests/naval_architecture/test_hull_ingest.py:L600-L600] |
| `tests/naval_architecture/test_hull_regions.py (module collection)` | SKIPPED / NOT_RUN: set DIGITALMODEL_3DM_HULL and DIGITALMODEL_3DM_APPENDAGE_LAYERS (name=Layer,name=Layer) to run the multi-region acceptance gate | O5, issue pending (6); [tests/naval_architecture/test_hull_regions.py:L624-L624] |
| `tests/naval_architecture/test_mesh_pickle_generator.py (module collection)` | SKIPPED / NOT_RUN: fixture reproduction pins NumPy 2.4.4 and Python 3.11.14 | O5, issue pending (6); [tests/naval_architecture/test_mesh_pickle_generator.py:L66-L66] |
| `tests/naval_architecture/test_trim_hydrostatics.py (module collection)` | SKIPPED / NOT_RUN: set DIGITALMODEL_3DM_HULL to a .3dm hull to run this check | O5, issue pending (6); [tests/naval_architecture/test_trim_hydrostatics.py:L494-L494] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestCSVBulkRegistration::test_register_17_vessels` | SKIPPED / NOT_RUN: worldenergydata CSV not available (standalone digitalmodel checkout) | O5, issue pending (6); [tests/naval_architecture/test_vessel_fleet_adapter.py:L695-L695] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestCSVBulkRegistration::test_all_have_required_keys` | SKIPPED / NOT_RUN: worldenergydata CSV not available (standalone digitalmodel checkout) | O5, issue pending (6); [tests/naval_architecture/test_vessel_fleet_adapter.py:L704-L704] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestDrillingRigHullValidationCSV::test_validate_only_supported_rigs_with_geometry` | SKIPPED / NOT_RUN: worldenergydata drilling-rigs CSV not available | O5, issue pending (6); [tests/naval_architecture/test_vessel_fleet_adapter.py:L847-L847] |
| `tests/naval_architecture/test_vessel_fleet_adapter.py::TestDrillingRigHullValidationCSV::test_register_supported_geometry_subset` | SKIPPED / NOT_RUN: worldenergydata drilling-rigs CSV not available | O5, issue pending (6); [tests/naval_architecture/test_vessel_fleet_adapter.py:L867-L867] |
| `tests/naval_architecture/test_en400_worked_examples.py::TestEN400Ch3Hydrostatics::test_example_3_1_displacement_from_curves_of_form` | XFAIL: curves-of-form exercise requires ship hydrostatic data tables; the annotation is pre-existing | O5, issue pending (6); [tests/naval_architecture/test_en400_worked_examples.py:L120-L138] |

AFTER partitioned results match BASE: 1,613 passed, 0 failed, 10 skipped, 1 XFAIL and the same three module collection errors; 0 newly failed nodes (count).


Native licensed solver runs are NOT_RUN: solver execution and licensed interface imports are excluded from this offline pack. The package references to GHS and OpenFOAM are inventoried in section 9. [src/digitalmodel/naval_architecture/damage_stability_screening.py:L11-L15] [src/digitalmodel/naval_architecture/hull_ingest.py:L1-L8]

The historical inventory's test total and validation-record count are not used as current evidence. The execution record supplies counts for the available scope; a complete test-node total is not established while three modules cannot collect (O9, issue pending (8)). Independent-source expansion and the missing validation-record inventory remain O5, issue pending (6).
