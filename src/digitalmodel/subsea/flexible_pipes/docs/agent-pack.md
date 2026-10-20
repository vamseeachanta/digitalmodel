# Flexible pipe knowledge pack

| Header | Value |
| --- | --- |
| Module | subsea/flexible_pipes |
| Repository | digitalmodel |
| Package path | src/digitalmodel/subsea/flexible_pipes |
| Pack revision | 0 |
| Code revision | c099a5e1317e6bed63d876252d87cbb12c774372 |
| Analysis family | Engineering calculations (EC); AQG r0, section 10, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package provides deterministic layered-section properties, tensile-armour stress/utilisation checks and a bend-stiffener radius screen for unbonded flexible pipes. [src/digitalmodel/subsea/flexible_pipes/__init__.py:L1-L12]

EA and EI are indicative smeared-property estimates, rather than a full layer-interaction solution; the bend-stiffener calculation is a first-order screening estimate rather than a nonlinear taper design. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L34-L49] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L31-L40]

Fatigue, carcass collapse, pressure-armour capacity and catenary/lazy-wave integration are explicitly deferred. [src/digitalmodel/subsea/flexible_pipes/__init__.py:L10-L12]

The routing registry assigns `stable` to the containing `subsea` domain; this is a domain classification, not evidence of independent qualification of these calculations. [docs/registry/module-routing.yaml:L419-L424]

No depth, temperature, pressure or solver-version applicability envelope is enforced by these input dataclasses and scalar functions; these quantities are absent from their signatures. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L113-L127] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L110-L127] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L64-L80]

## 2. Inputs and outputs

Input ranges below describe implemented validation, not material qualification. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L121-L127] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L117-L127] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L82-L87]

| Input / upstream source | Unit | Implemented range | Default | Evidence |
| --- | --- | --- | --- | --- |
| `Layer.layer_type` | enum | LayerType has seven labels; no runtime membership check | required | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L73-L82] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L113-L127] |
| `Layer.inner_diameter`, `thickness` | m | each > 0 | required | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L93-L97] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L121-L125] |
| `Layer.density` | kg/m³ | >= 0 | required | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L98-L99] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L126-L127] |
| `Layer.youngs_modulus` | Pa | no constructor check; <= 0 suppresses stiffness | 0 Pa | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L100-L102] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L117-L119] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L155-L165] |
| `Layer.metallic_area` | m² | no sign check; None selects annular area | None | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L103-L106] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L147-L149] |
| `Layer.lay_angle_deg` | degrees from pipe axis | no angle check | 0 degrees | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L107-L119] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L159-L160] |
| `FlexiblePipeSection.layers` | ordered Layer list | nonempty; contiguous inner to outer | required | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L181-L200] |
| `FlexiblePipeSection.name`, `tol` | string, m | no tolerance sign check | flexible_pipe, 1e-6 m | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L186-L200] |
| `fluid_density`, `content_density` | kg/m³ | no sign check | water-density constant, 0 respectively | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L216-L240] |
| `TensileArmour.metallic_area`, `smys`, `mean_radius` | m², Pa, m | each > 0 | required | [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L108-L125] |
| `TensileArmour.lay_angle_deg` | degrees from pipe axis | strictly between 0 and 90 | required | [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L110-L121] |
| `TensileArmour.wire_thickness`, `youngs_modulus` | m, Pa | thickness >= 0; modulus unchecked | 0 m; modulus literal in code | [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L114-L127] |
| `effective_tension` from caller | N | stress path >= 0; stiffener path > 0 | required | [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L160-L165] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L77-L85] |
| `applied_radius`, `minimum_bend_radius` from caller | m | bending function checks applied > 0; design-check MBR lacks a sign guard | both None in verify function | [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L173-L175] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L178-L184] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L215-L223] |
| `load_case`, `utilisation_factor` | enum, dimensionless | override in (0,1]; otherwise dictionary lookup | NORMAL, None | [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L91-L103] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L148-L157] |
| `bending_stiffness`, `effective_tension`, `deflection_angle` | N·m², N, rad | each > 0 | required | [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L64-L88] |
| `target_radius` / stiffener `minimum_bend_radius` | m | > 0 | required | [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L91-L106] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L109-L124] |

The section API uses SI dimensions, degrees for layer lay angle and radians for stiffener deflection; positive submerged weight means heavier than the surrounding fluid. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L93-L110] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L231-L240] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L38-L40]

These APIs use scalar geometry relative to the pipe axis; no global coordinate origin, phase or file-format unit conversion is defined by these signatures. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L93-L110] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L110-L115] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L64-L80]

| Output | Unit / convention | Evidence |
| --- | --- | --- |
| Layer mean/outer diameter; annular/structural area | m; m² | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L129-L149] |
| Layer dry mass; layer EA; layer EI | kg/m; N; N·m² | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L151-L174] |
| Section bore/outer diameter | m | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L202-L210] |
| Dry, displaced and content mass per length | kg/m | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L212-L224] |
| Submerged weight per length | N/m, positive downward effective weight | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L226-L240] |
| Section tensile-armour area; total EA/EI | m²; N; N·m² | [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L242-L256] |
| `FlexCheckResult` stresses; three utilisations; governing; passes; maximum | Pa; dimensionless; label; bool; dimensionless maximum | [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L130-L145] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L226-L243] |
| `BendStiffenerResult.r_min`, `ei_required`, `u_curvature`, `passes` | m; N·m²; dimensionless; bool | [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L54-L61] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L123-L131] |

## 3. Method and governing standards

The equations here describe the implementation; they do not establish compliance with a licensed standard. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L45-L49] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L67-L75] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L31-L37]

| Method | Implementing definition and expression | Basis and limits |
| --- | --- | --- |
| Layer geometry | `Layer.mean_diameter = ID+t`; `outer_diameter = ID+2t`; `annular_area = π(ro²-ri²)` | Geometric closed form. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L129-L144] |
| Structural area | `Layer.structural_area`: supplied metallic area, otherwise annular area | Full wall fallback can overstate wound armour area. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L103-L106] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L147-L149] |
| Layer mass | `Layer.mass_per_length = density*π*mean_diameter*thickness` | Full cylindrical wall, independent of supplied metallic area. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L151-L153] |
| Axial stiffness | `Layer.axial_stiffness = E*A*cos(alpha)^3` | Degrees converted to radians; <= 0 modulus returns zero. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L155-L160] |
| Bending stiffness | `Layer.bending_stiffness = E*A*R²*cos(alpha)/2` | Smeared cylinder; <= 0 modulus returns zero. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L162-L174] |
| Section aggregation | `dry_mass_per_length`, `tensile_armour_area`, `axial_stiffness`, `bending_stiffness` sum their respective layer contributions | Armour area filters the tensile-armour enum. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L212-L214] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L242-L256] |
| Displaced and content mass | `FlexiblePipeSection.displaced_mass_per_length = fluid_density*π*ro²`; `content_mass_per_length = content_density*π*rb²` | Full outer-envelope displacement and full bore contents. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L216-L224] |
| Effective weight | `FlexiblePipeSection.submerged_weight_per_length = (dry+content-displaced)*GRAVITY` | Submerged-weight term; no pressure-area effective-tension correction. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L226-L240] |
| Wire tension and allowable stress | `armour_tension_stress = T/(A*cos(alpha))`; `allowable_wire_stress = eta*smys` | Equal load sharing of intact armour packages is a stated assumption; default factors are explicitly indicative. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L67-L75] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L148-L165] |
| Wire bending | `armour_bending_stress = E*(wire_thickness/2)/applied_radius` | Uses wire half-thickness, not mean armour radius. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L49-L56] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L168-L175] |
| Utilisation | `verify_flexible_pipe`: tension/allowable, MBR/applied radius, (tension+bending)/allowable; every utilisation <= 1 passes | Omitted curvature inputs become zero utilisation; see section 6. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L210-L243] |
| Termination screening | `developed_min_radius = sqrt(EI/T)/theta`; `required_effective_ei = T*(theta*target_radius)^2`; `screen_bend_stiffener` checks radius >= MBR | Beam-on-tension estimate; not a taper solver. [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L31-L40] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L64-L131] |

| Standard cited by code/docs | Edition / clause evidence | Qualification status |
| --- | --- | --- |
| API 17J, Specification for Unbonded Flexible Pipe | Code references an indicative table envelope without edition or clause title; exact factors are intentionally not reproduced here. | UNVERIFIED: edition, clause title and default-factor provenance; clause text not checked against the licensed copy. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L30-L37] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L67-L80] |
| API 17B, Recommended Practice for Flexible Pipe | Plan names cross-section design; implementation references the document without an edition or numbered clause. | UNVERIFIED: governing edition and exact clauses; clause text not checked against the licensed copy. [docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md:L18-L22] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L56-L59] |
| ISO 13628-2, Unbonded flexible pipe specification, as named in code | Appears alongside API 17J without edition or clause. | UNVERIFIED: equivalence, edition and exact clause; clause text not checked against the licensed copy. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L56-L59] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L77-L80] |
| API 17L1 / 17L2, Flexible Pipe Ancillary Equipment, as named in code | Bend-stiffener reference only; no edition or clause given. | UNVERIFIED: exact designation titles, editions and clauses; clause text not checked against the licensed copy. [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L31-L45] |

The research note records a historical API RP 17B edition; the implementation does not pin an edition, so a code-versus-docs edition match cannot be established. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L56-L59] [docs/research/ffs-unbonded-flexible-pipe-integrity-screen.md:L50-L59]

The stress and stiffener result dataclasses return numeric/bool fields without a `Citation` sidecar; source traceability and independent mechanical validation remain open under issue pending (3). [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L130-L145] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L234-L243] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L54-L61] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L126-L131]

## 4. Validated worked examples and regression examples

Offline execution record: 2026-10-18, code revision `c099a5e1317e6bed63d876252d87cbb12c774372`; 20 run, 20 passed, 0 failed, 0 skipped, 0 NOT_RUN at BASE and AFTER. The command form is `PYTHONPATH=src uv run --no-sources python -m pytest tests/subsea/flexible_pipes/test_flexible_pipes.py -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-base`; the executable was invoked by its configured full path. The AFTER run used the same command with a separate scratch basetemp on the uncommitted documentation tree; module code remained at the pin. Test inputs are inline Python fixtures, not an external worked-example file. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L30-L104] [tests/subsea/flexible_pipes/test_flexible_pipes.py:L110-L248]

V1 uses the independently evaluated closed form for annular volume per unit length: density times `π*((ID/2+t)^2-(ID/2)^2)`. With the test's geometry and density, this gives 25.278 kg/m to the precision of the existing expectation. This validates shell geometry/mass only, not flexible-pipe mechanics. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L30-L36]

The remaining cases are regression examples: their expectations document implemented formulas or state transitions without an independently sourced mechanical qualification. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L39-L248]

| Case / kind | Expected output, tolerance and basis | Existing test node ID | Offline result |
| --- | --- | --- | --- |
| V1 / validated | 25.278 kg/m; relative 1e-4, rounded annular-volume closed form. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L30-L36] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_layer_mass_per_length_hand_verified` | PASS |
| R1 / regression | EA 2.2762e8 N; relative 1e-3, inline hand formula. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L39-L52] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_layer_axial_stiffness_hand_verified` | PASS |
| R2 / regression | EA = 0 N and EI = 0 N·m² for omitted modulus; exact. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L55-L58] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_layer_no_modulus_has_zero_stiffness` | PASS |
| R3 / regression | Bore 0.200 m, OD 0.268 m, armour area 4e-3 m²; pytest approximate comparison. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L75-L80] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_section_geometry_and_armour_area` | PASS |
| R4 / regression | 1 noncontiguous stack raises ValueError; exact exception contract. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L83-L87] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_section_rejects_non_contiguous_layers` | PASS |
| R5 / regression | Empty-bore weight > 0 N/m; content increases it by > 0 N/m. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L90-L97] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_submerged_weight_sign` | PASS |
| R6 / regression | 2 invalid layer geometries raise ValueError. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L100-L104] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_layer_validation` | PASS |
| R7 / regression | Wire tension stress 288.68e6 Pa; relative 1e-4, inline hand expression. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L121-L125] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_armour_tension_stress_hand_verified` | PASS |
| R8 / regression | Default and override allowables match code expectations; 1 invalid factor rejected; no standard table values reproduced. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L128-L133] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_allowable_wire_stress_factors` | PASS |
| R9 / regression | Wire bending stress 124.2e6 Pa; relative 1e-4, inline hand expression. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L136-L139] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_armour_bending_stress_hand_verified` | PASS |
| R10 / regression | Pass true; curvature 0.8 dimensionless governs; stress ratios within relative 1e-3. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L142-L156] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_verify_within_limit_passes` | PASS |
| R11 / regression | Pass false; combined utilisation > 1 dimensionless governs. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L159-L170] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_verify_over_tension_fails` | PASS |
| R12 / regression | Pass false; curvature 4/3 dimensionless governs, pytest approximate comparison. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L173-L183] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_verify_mbr_violation_fails` | PASS |
| R13 / regression | Tension utilisation 1 dimensionless passes; relative 1e-9. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L186-L193] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_verify_boundary_unity` | PASS |
| R14 / regression | Extreme-case tension utilisation is lower by > 0 dimensionless. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L196-L199] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_extreme_load_case_relaxes_allowable` | PASS |
| R15 / regression | Developed radius 15.8114 m; relative 1e-4, inline screen formula. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L205-L209] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_developed_min_radius_hand_verified` | PASS |
| R16 / regression | Required EI 5e5 N·m²; relative 1e-4, inverse-formula round trip. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L212-L216] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_required_effective_ei_roundtrip` | PASS |
| R17 / regression | Pass true; radius 15.8114 m and curvature 4/15.8114 dimensionless; relative 1e-4. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L219-L227] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_screen_bend_stiffener_passes` | PASS |
| R18 / regression | Pass false; radius 3.1623 m within relative 1e-4, curvature > 1 dimensionless. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L230-L239] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_screen_bend_stiffener_fails_below_mbr` | PASS |
| R19 / regression | 3 invalid stiffener inputs raise ValueError. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L242-L248] | `tests/subsea/flexible_pipes/test_flexible_pipes.py::test_bend_stiffener_validation` | PASS |

Where pytest approximate tolerances are implicit, the tests do not state a separate engineering uncertainty budget; the explicit relative tolerances above are assertion tolerances, not experimental uncertainty. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L30-L58] [tests/subsea/flexible_pipes/test_flexible_pipes.py:L75-L80] [tests/subsea/flexible_pipes/test_flexible_pipes.py:L121-L139]

## 5. QA gates and executable checks

Gate applicability follows AQG r0, section 10, Python calculations: G0 inputs/units/citations, G1 imports/tests, G2 hand or published check, G3 and G4 NOT_APPLICABLE, G5 evidence appendix (AQG r0, sections 9–10).

The following are reproducible pack checks; a passing check covers its stated case only, and does not close the uncovered failure paths in section 6. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L30-L104] [tests/subsea/flexible_pipes/test_flexible_pipes.py:L121-L248]

| Gate | Applicability and check IDs | Numeric pass criterion and unit | Evidence / executable route |
| --- | --- | --- | --- |
| G0 | Applicable: FP-I1 geometry guards | 2 of 2 invalid geometries rejected; 0 unexpected exceptions, count | Run section-4 R6. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L100-L104] |
| G0 | Applicable: FP-I2 layer adjacency | Reject the 0.005 m diametral gap; 1 of 1 rejection, count | Run R4; production default tolerance 1e-6 m. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L83-L87] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L188-L200] |
| G0 | Applicable: FP-I3 stiffener guards | 3 of 3 nonpositive inputs rejected; 0 unexpected accepts, count | Run R19. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L242-L248] |
| G0 | Applicable: FP-I4 intentional zero stiffness | EA exactly 0 N and EI exactly 0 N·m² for the no-modulus sheath | Run R2; does not cover negative modulus. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L55-L58] |
| G1 | Applicable: FP-T1 package import and offline suite | 20 of 20 collected cases pass; 0 failed, 0 skipped, count | Run section-4 command; package functions imported by tests, no solver route. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L11-L25] [tests/subsea/flexible_pipes/test_flexible_pipes.py:L30-L248] |
| G2 | Applicable: FP-H1 independent shell mass | Absolute mass error <= 1e-4 times 25.278 kg/m | Run V1 and independently evaluate annular volume times density. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L30-L36] |
| G2 | Applicable: FP-H2 stress arithmetic regression | Tension error <= 1e-4 times 288.68e6 Pa; bending error <= 1e-4 times 124.2e6 Pa | Run R7 and R9; this checks arithmetic, not standard qualification. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L121-L139] |
| G2 | Applicable: FP-H3 screen arithmetic regression | Radius error <= 1e-4 times 15.8114 m; EI error <= 1e-4 times 5e5 N·m² | Run R15 and R16; independent screen applicability remains open. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L205-L216] |
| G3 | NOT_APPLICABLE | No time-domain evolution in this package's scalar section/check/screen API | [src/digitalmodel/subsea/flexible_pipes/__init__.py:L3-L12] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L64-L131] |
| G4 | NOT_APPLICABLE | No diffraction mesh, body or hydrodynamic solution in the exported API | [src/digitalmodel/subsea/flexible_pipes/__init__.py:L39-L60] |
| G5 | Applicable: FP-E1 example evidence completeness | 20 of 20 section-4 node IDs have inputs, expectations, result and pin; 0 missing fields, count | Resolve section-4 IDs against test definitions and compare the offline result log; retain input/code fingerprints with the calculation record. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L30-L248] |

FP-E1 is a pack evidence check, not an implemented result exporter; result objects do not embed a QA appendix or fingerprints. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L130-L145] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L54-L61]

## 6. Known failure modes and pitfalls

sweep: 26 found

Sweep scope is all four Python files, including dataclasses, defaults, guards, formulas and return paths; intentional assumptions are included alongside defects. [src/digitalmodel/subsea/flexible_pipes/__init__.py:L1-L60] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L69-L256] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L91-L243] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L54-L131]

Exact discovery commands (executed before the pack was added):

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|density|unit|cache|default' src/digitalmodel/subsea/flexible_pipes --glob '*.py'
rg -n 'return|raise|: float|Optional|math\.|UTILISATION_FACTORS' src/digitalmodel/subsea/flexible_pipes --glob '*.py'
```

No exception handler, record-skipping continue, external-text numeric conversion, empty-result cache or error-path None/list/dict return exists in the reviewed definitions; the rows below cover the defaults, fallbacks and unit assumptions that do exist. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L69-L256] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L91-L243] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L54-L131]

| ID | Source / symptom and cause | Detection check | Follow-up / date |
| --- | --- | --- | --- |
| S01 | Water-density constant assumes seawater and is not an input measurement. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L69-L70] | no detection check exists | issue pending (3); 2026-10-18 |
| S02 | Gravity constant fixes the acceleration used for all effective weights. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L69-L70] | no detection check exists | issue pending (3); 2026-10-18 |
| S03 | Omitted layer modulus defaults to zero; stiffness can silently vanish. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L117-L119] | FP-I4 covers the intended omitted-modulus case only | issue pending (2); 2026-10-18 |
| S04 | Missing metallic area selects the full annulus; wound armour area can be overstated. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L118-L119] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L147-L149] | no detection check exists | issue pending (2); 2026-10-18 |
| S05 | Omitted lay angle is axial; both stiffness projections use that default. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L119-L119] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L159-L173] | no detection check exists | issue pending (2); 2026-10-18 |
| S06 | Nonpositive modulus returns exactly zero axial stiffness, including invalid negative modulus. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L155-L160] | FP-I4 checks omitted modulus only; no detection check exists for negative modulus | issue pending (1); 2026-10-18 |
| S07 | Nonpositive modulus returns exactly zero bending stiffness, including invalid negative modulus. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L162-L165] | FP-I4 checks omitted modulus only; no detection check exists for negative modulus | issue pending (1); 2026-10-18 |
| S08 | Dry mass uses a full shell regardless of metallic fill area or helical wire length. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L151-L153] | FP-H1 checks shell arithmetic only; no detection check exists for physical fill applicability | #1793; 2026-10-18 |
| S09 | Default continuity tolerance is a diametral 1e-6 m; negative tolerances are unchecked. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L186-L200] | FP-I2 detects the illustrated gap; no detection check exists for invalid tolerance | issue pending (1); 2026-10-18 |
| S10 | Displacement defaults to the seawater constant and the entire outer envelope. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L216-L219] | no detection check exists | issue pending (3); 2026-10-18 |
| S11 | Content mass defaults to zero density, equivalent to empty contents. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L221-L224] | no detection check exists | issue pending (2); 2026-10-18 |
| S12 | Submerged weight independently defaults content density to zero. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L226-L238] | no detection check exists | issue pending (2); 2026-10-18 |
| S13 | Submerged weight independently defaults fluid density to the water constant. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L226-L240] | no detection check exists | issue pending (3); 2026-10-18 |
| S14 | Negative or non-finite supplied fluid/content density has no explicit rejection. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L216-L240] | no detection check exists | issue pending (1); 2026-10-18 |
| S15 | Layer area, angle and modulus lack sign/range/finite checks beyond the listed geometry guards. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L121-L127] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L147-L174] | FP-I1 covers geometry only; no detection check exists for these properties | issue pending (1); 2026-10-18 |
| S16 | SI lengths/density/modulus and degrees from the axis are caller assumptions; positive effective weight is downward. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L93-L110] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L231-L240] | no detection check exists for unit conversion | issue pending (3); 2026-10-18 |
| S17 | Omitted wire thickness is zero, so bending demand can be suppressed. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L110-L127] | no detection check exists | issue pending (2); 2026-10-18 |
| S18 | Armour modulus defaults to a steel-like literal and lacks sign/finite validation. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L114-L127] | no detection check exists | issue pending (1); 2026-10-18 |
| S19 | Omitted factor selects an indicative dictionary value through the normal load-case default. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L148-L157] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L178-L184] | R8 is an arithmetic regression; no detection check exists for governing-standard suitability | issue pending (3); 2026-10-18 |
| S20 | The wire bending formula returns zero when thickness is zero without a missing-data indicator. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L168-L175] | no detection check exists | issue pending (2); 2026-10-18 |
| S21 | Missing either radius input sets curvature utilisation to zero, allowing a tension-only pass. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L215-L218] | no detection check exists for incomplete curvature assessment | issue pending (2); 2026-10-18 |
| S22 | Missing applied radius sets bending stress to zero without a completeness flag. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L220-L224] | no detection check exists | issue pending (2); 2026-10-18 |
| S23 | Negative MBR is not rejected; zero applied radius reaches division before the bending guard. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L215-L223] | no detection check exists | issue pending (1); 2026-10-18 |
| S24 | Result field comment describes an infinite sentinel but the function assigns zero. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L134-L138] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L215-L218] | no detection check exists | issue pending (2); 2026-10-18 |
| S25 | Armour inputs assume SI and degrees; mean radius is validated but wire bending uses thickness instead. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L108-L127] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L160-L175] | FP-H2 checks positive-input arithmetic; no detection check exists for unit mistakes/non-finite inputs | issue pending (1); 2026-10-18 |
| S26 | Stiffener inputs assume N, N·m², metres and radians; positivity guards do not explicitly reject NaN/infinity. [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L38-L40] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L64-L106] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L121-L130] | FP-I3 checks zero inputs only; no detection check exists for non-finite/unit errors | issue pending (1); 2026-10-18 |

The issue search found enhancement and integrity-screen scopes, rather than a recorded incident in this package: #494 tracks flexible-pipe capability, #1793 deeper cross-section mechanics and #2200 integrity-screen research. None is treated as evidence that an incident was reproduced; no detection check exists for the deferred fatigue/collapse/integrity calculations. [src/digitalmodel/subsea/flexible_pipes/__init__.py:L10-L12] [docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md:L18-L33] [docs/research/ffs-unbonded-flexible-pipe-integrity-screen.md:L3-L9]

## 7. Lessons log

| Date | Entry | Lesson / evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial documentation | A zero utilisation can mean a missing curvature check, rather than a checked zero demand. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L215-L224] | Separate shell-mass validation from mechanics regressions; expose omitted checks in section 6 | Not assigned; issue pending (2) |

## 8. Ownership

Review routing for this draft: engineering calculations quality manager; deputy role: engineering calculations deputy quality manager; producing responsibility: calculation documentation; ambiguous source clauses and applicability questions go to the family quality manager, then the maintainer for cross-family scope decisions (AQG r0, sections 10–11).

UNVERIFIED: a named deputy assignment and a clearance record are not established by this draft; issue pending (3).

## 9. Dependencies and implementation inventory

The implementation depends on Python `math`, dataclasses, enums and typing; its package initializer re-exports the local section/check/screen definitions. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L62-L67] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L83-L88] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L48-L51] [src/digitalmodel/subsea/flexible_pipes/__init__.py:L15-L37]

The tests consume this package directly; the proposed catenary adapter remains deferred, rather than an established downstream integration. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L11-L25] [src/digitalmodel/subsea/flexible_pipes/__init__.py:L10-L12] [docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md:L24-L33]

The neighbouring generic cross-section report explicitly excludes flexible-pipe mechanics; its fixtures are not validation data for this package. [docs/subsea/cross_sections/offshore_cross_section_report.md:L71-L78]

Inventory source: `docs/registry/implementation-map.yaml` on `codex/implmap-work`, map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, declared data/code revision `270c8759c374b67a7f032953e7fcdb14e0224461`. The selected family signals and five in-package rows match the corresponding ranges available at the pack's code pin. [docs/registry/implementation-map.yaml:L2139-L2150] [docs/registry/implementation-map.yaml:L4253-L4307]

| Map family | In-package implementation | Code-pin evidence |
| --- | --- | --- |
| effective-tension-riser-stack-up | `bend_stiffener.developed_min_radius` | [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L64-L88] |
| effective-tension-riser-stack-up | `bend_stiffener.required_effective_ei` | [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L91-L106] |
| effective-tension-riser-stack-up | `bend_stiffener.screen_bend_stiffener` | [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L109-L131] |
| effective-tension-riser-stack-up | `design_checks_17J.armour_tension_stress` | [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L160-L165] |
| effective-tension-riser-stack-up | `design_checks_17J.verify_flexible_pipe` | [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L178-L243] |

There is one module map family, with five in-package rows, so it is the only counted family; no package subfolder with its own pack is excluded. [docs/registry/implementation-map.yaml:L4253-L4307] [src/digitalmodel/subsea/flexible_pipes/__init__.py:L15-L37]

Counted-family search uses these Python `re` patterns exactly, case-sensitive; none matches the package path `subsea/flexible_pipes` case-insensitively, so none is excluded. [docs/registry/implementation-map.yaml:L2141-L2144]

```text
effective_tension|effectiveTension|top_tension|tensioner|stack_up|internal_pressure.*(?:area|A_i)|external_pressure.*(?:area|A_o)
effective.tension|tensioner|stack.up
internal_pressure|external_pressure|fluid_density|buoyancy_module
```

| Hit definition at the code pin | Classification and disposition |
| --- | --- |
| The five definitions inventoried above | Listed implementations; screen wrapper also delegates to the two radius/stiffness functions. [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L123-L124] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L210-L224] |
| `design_checks_17J` module-level definition | Documentation hits at the module docstring; no further executable calculation there. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L1-L28] |
| `FlexiblePipeSection.displaced_mass_per_length` | Component-mass helper for effective weight; formula explicitly listed in section 3. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L216-L219] |
| `FlexiblePipeSection.submerged_weight_per_length` | Further submerged-weight computation, explicitly listed in section 3; map follow-up issue pending (4). [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L226-L240] |

A8 self-check: all five map rows are inventoried; the grep yields eight enclosing definitions, of which five are listed, one is documentation and two are the section-3 helper/weight definitions. There are 0 unlisted further second implementations, within the <= 1 acceptance threshold. The extra weight formula still needs map reconciliation. [docs/registry/implementation-map.yaml:L4253-L4307] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L1-L28] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L216-L240]

Repository-wide discovery also searched tracked Python files, including `scripts/`, using the same patterns and a supplementary exact-symbol/formula search:

```bash
rg -n --glob '*.py' 'def (developed_min_radius|required_effective_ei|screen_bend_stiffener|armour_tension_stress|armour_bending_stress|verify_flexible_pipe)|sqrt\([^\n]*(?:[Ee][Ii]|bending_stiffness)[^\n]*/[^\n]*(?:tension|[Tt])|cos\(alpha\) \*\* 3|wire_thickness / 2' .
```

The supplementary hits include riser-response and vibration-frequency formulas, which compute different results rather than the local wire-stress or termination-screen result. [src/digitalmodel/drilling_riser/riser_response.py:L15-L33] [src/digitalmodel/subsea/viv_analysis/frequency_calculator.py:L148-L165] [src/digitalmodel/orcaflex/viv_screening.py:L180-L198]

Map follow-up: inventory geometry/mass and smeared EA/EI, wire bending and allowable-stress families separately; reconcile the additional submerged-weight definition. Families without in-package map rows do not affect this A8 result; issue pending (4). [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L129-L174] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L226-L256] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L148-L175]

## 10. Coverage and open items

Read inventory: every line of the four package Python files and the dedicated test file was inspected; no package README, internal test folder or data fixture is present in this code-pin package inventory. [src/digitalmodel/subsea/flexible_pipes/__init__.py:L1-L60] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L1-L256] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L1-L243] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L1-L131] [tests/subsea/flexible_pipes/test_flexible_pipes.py:L1-L248]

| Documentation discovered by module/method search | Read scope / evidence |
| --- | --- |
| `docs/subsea/cross_sections/offshore_cross_section_report.md` | Module or method context reviewed; no external-source qualification adopted. [docs/subsea/cross_sections/offshore_cross_section_report.md:L25-L26] |
| `docs/skills/fatigue-assessment/SKILL.md` | Module or method context reviewed; no external-source qualification adopted. [docs/skills/fatigue-assessment/SKILL.md:L58-L59] |
| `docs/registry/implementation-map.md` | Module or method context reviewed; no external-source qualification adopted. [docs/registry/implementation-map.md:L18-L19] |
| `docs/field-development/subsea-production-systems-mapping.md` | Module or method context reviewed; no external-source qualification adopted. [docs/field-development/subsea-production-systems-mapping.md:L23-L24] |
| `docs/domains/asset-integrity/ffs-offering-catalog.md` | Module or method context reviewed; no external-source qualification adopted. [docs/domains/asset-integrity/ffs-offering-catalog.md:L100-L101] |
| `docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md` | Module or method context reviewed; no external-source qualification adopted. [docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md:L1-L2] |
| `docs/research/ffs-fixed-platform-damaged-member-screen.md` | Module or method context reviewed; no external-source qualification adopted. [docs/research/ffs-fixed-platform-damaged-member-screen.md:L7-L8] |
| `docs/research/ffs-unbonded-flexible-pipe-integrity-screen.md` | Module or method context reviewed; no external-source qualification adopted. [docs/research/ffs-unbonded-flexible-pipe-integrity-screen.md:L1-L2] |
| `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md` | Module or method context reviewed; no external-source qualification adopted. [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L83-L84] |
| `docs/domains/pipelines/_subsea_pipelines.md` | Module or method context reviewed; no external-source qualification adopted. [docs/domains/pipelines/_subsea_pipelines.md:L17-L18] |
| `docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md` | Module or method context reviewed; no external-source qualification adopted. [docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md:L134-L135] |
| `docs/domains/orcaflex/examples/ORCAFLEX_MODELS.md` | Module or method context reviewed; no external-source qualification adopted. [docs/domains/orcaflex/examples/ORCAFLEX_MODELS.md:L22-L23] |
| `docs/domains/orcaflex/templates/VALIDATION_REPORT.md` | Module or method context reviewed; no external-source qualification adopted. [docs/domains/orcaflex/templates/VALIDATION_REPORT.md:L149-L150] |
| `docs/domains/orcaflex/templates/USAGE_GUIDE.md` | Module or method context reviewed; no external-source qualification adopted. [docs/domains/orcaflex/templates/USAGE_GUIDE.md:L501-L502] |
| `docs/domains/orcaflex/templates/risers/flexible_riser_hybrid/README.md` | Module or method context reviewed; no external-source qualification adopted. [docs/domains/orcaflex/templates/risers/flexible_riser_hybrid/README.md:L11-L12] |

The supplemental command ran `tests/contracts tests/docs/test_digitalmodel_routing_contract.py tests/docs/test_ffs_unbonded_flexible_pipe_integrity_screen.py` with the same pytest form: 501 run/passed, 0 failed, 0 skipped, 0 NOT_RUN at BASE and AFTER. The complete dedicated test inventory is the 20 section-4 nodes; related research-note tests check document structure rather than pipe mechanics. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L30-L248] [tests/docs/test_ffs_unbonded_flexible_pipe_integrity_screen.py:L68-L109]

The calculation citation contract was read; the package result objects lack its numeric-result citation sidecar. This is a source/evidence gap, not a claim of code compliance. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L130-L145] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L54-L61]

| Open item | Responsible role | Tracking and evidence |
| --- | --- | --- |
| Reject negative/non-finite properties, densities and radii; define angle/tolerance ranges | Calculation maintainer | issue pending (1); S06–S07, S09, S14–S15, S18, S23, S25–S26. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L121-L127] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L215-L223] |
| Distinguish intentionally omitted checks from computed zero demand; fix contradictory curvature field documentation | Calculation maintainer | issue pending (2); S03–S05, S11–S12, S17, S20–S22, S24. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L134-L138] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L215-L224] |
| UNVERIFIED: API 17J governing edition, exact clause/title and indicative factor provenance | Calculation quality manager | issue pending (3); clause text not checked against the licensed copy. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L30-L37] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L67-L80] |
| UNVERIFIED: API 17B governing edition and cross-section/fatigue clauses | Calculation quality manager | issue pending (3); clause text not checked against the licensed copy. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L56-L59] [docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md:L18-L22] |
| UNVERIFIED: ISO 13628-2 edition, equivalence and clause references | Calculation quality manager | issue pending (3); clause text not checked against the licensed copy. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L77-L80] |
| UNVERIFIED: API 17L1/17L2 exact designation titles, editions and clauses | Calculation quality manager | issue pending (3); clause text not checked against the licensed copy. [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L31-L45] |
| UNVERIFIED: named deputy assignment and clearance record | Maintainer | issue pending (3); draft review routing only |
| Add result Citation sidecars, input/code fingerprints and persistent QA evidence; independently qualify mechanical reference cases | Calculation maintainer | issue pending (3); only shell mass has an independent closed-form reference in this pack. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L130-L145] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L54-L61] [tests/subsea/flexible_pipes/test_flexible_pipes.py:L30-L36] |
| Reconcile broad-family map inventory and missing method families | Map maintainer | issue pending (4); see section 9. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L129-L174] [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L148-L175] |
| Fatigue, collapse, pressure capacity, nonlinear taper design and catenary integration remain outside implemented main path | Calculation maintainer | #494, #1793; no detection check exists for unimplemented results. [src/digitalmodel/subsea/flexible_pipes/__init__.py:L10-L12] [src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py:L31-L37] |
| Integrity-screen research is not an implemented remaining-life method in this package | Calculation maintainer | #2200; no detection check exists here. [docs/research/ffs-unbonded-flexible-pipe-integrity-screen.md:L3-L15] |

No client-derived values or private sidecar entry are required for the inline synthetic tests used by this pack. [tests/subsea/flexible_pipes/test_flexible_pipes.py:L30-L104] [tests/subsea/flexible_pipes/test_flexible_pipes.py:L110-L248]
