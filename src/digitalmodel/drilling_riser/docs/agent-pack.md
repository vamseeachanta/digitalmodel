# Drilling-riser module knowledge pack

| Header | Value |
| --- | --- |
| Module | `drilling_riser` (top level) |
| Repository | `digitalmodel` |
| Package | `src/digitalmodel/drilling_riser/` |
| Scope | Python files directly in the package; sub-packages are dependencies |
| Pack revision | 1, 2026-10-15 |
| Code revision | `1d130ba6c3d9a3b05eb7685b735a1ef6de0752a8` |
| Map revision | `b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407` |
| Analysis family | OrcaFlex riser and mooring; `MR` |
| QM role | Riser and mooring quality manager |
| Status | DRAFT |

Code citations below resolve at the code revision; citations to `docs/registry/implementation-map.yaml` resolve at the separate map revision, whose recorded data revision is `947fa50ff9a9e210de7e8f90e01b37add85c5954`. [docs/registry/implementation-map.yaml:L1-L8]

## 1. Purpose and scope

The component adapter normalises CSV records, the assembly model expands component counts, and the stack-up workflow writes an effective-tension profile and scalar sizing results. [src/digitalmodel/drilling_riser/adapter.py:L119-L197] [src/digitalmodel/drilling_riser/assembly.py:L74-L180] [src/digitalmodel/drilling_riser/workflow.py:L32-L76]

The taper-stress-joint workflow computes stationwise membrane stresses; its sizing utilisation omits bending, and the reported section modulus is unused in that utilisation; the operating-envelope engine combines static beam response, motion contributions and equipment limits; the drift-off screen estimates a disconnect window under constant acceleration. [src/digitalmodel/drilling_riser/tsj_workflow.py:L74-L111] [src/digitalmodel/drilling_riser/envelope.py:L278-L446] [src/digitalmodel/drilling_riser/drift_off.py:L181-L257]

The routing registry records maturity as `development`; its existing global-model sub-entry has a separate pack. [docs/registry/module-routing.yaml:L99-L108]

Static riser screening assumes constant positive tension, uniform lateral load and moment-free ends, excluding dynamic amplification, vortex-induced vibration and vessel-motion dynamics. [src/digitalmodel/drilling_riser/riser_response.py:L3-L31] [src/digitalmodel/drilling_riser/riser_response.py:L98-L101]

Depth-resolved current data are carried but only surface speed enters drag; optional equipment limits can remain inactive when their capacity data are absent. [src/digitalmodel/drilling_riser/envelope.py:L88-L119] [src/digitalmodel/drilling_riser/envelope.py:L143-L153] [src/digitalmodel/drilling_riser/envelope.py:L367-L403]

Campaign functions build specifications and operate on a supplied solver model, while the actual text writer and response extraction live in the global-model dependency. [src/digitalmodel/drilling_riser/campaign.py:L134-L239] [src/digitalmodel/drilling_riser/campaign.py:L835-L952]

## 2. Inputs and outputs

### Inputs

| Input | Unit | Range and default | Source and evidence |
| --- | --- | --- | --- |
| Component CSV | in, ft, kips, psi; converted to mm, m, kN, MPa | Missing identifier skips a record; non-finite numeric fields are omitted; submerged weight may be negative | `normalize_riser_component_record`; conversion table [src/digitalmodel/drilling_riser/adapter.py:L22-L91] [src/digitalmodel/drilling_riser/adapter.py:L119-L150] |
| Component count and length | count; m | Count at least 1; unresolved length requires a caller override or raises | `StackupItem`, `total_length_m` [src/digitalmodel/drilling_riser/assembly.py:L43-L56] [src/digitalmodel/drilling_riser/assembly.py:L157-L180] |
| Stack-up weight, pressure, geometry, yield strength | kN; MPa; mm; MPa | Required scalars are converted to float; positivity is not checked by `_required_float`; dynamic and wall factors have implementation defaults | `workflow.router`, `_required_float` [src/digitalmodel/drilling_riser/workflow.py:L32-L51] [src/digitalmodel/drilling_riser/workflow.py:L162-L178] |
| Depth fractions | dimensionless | Nonempty, strictly increasing, within [0, 1]; mapping grid needs at least 2 points | `_depth_factors` [src/digitalmodel/drilling_riser/workflow.py:L79-L103] |
| Slip-ring fluid columns | kN/m³; m; m² | All six optional fields together, or none; factors are caller-overridable | `_optional_slip_ring_tension` [src/digitalmodel/drilling_riser/workflow.py:L106-L128] |
| Tensioner counts, efficiency, angular allowance factor | count; dimensionless | Integer conversion of counts; surviving count positive; efficiency in (0, 1]; angular allowance factor at least 1 | `tensioner_system_factor`, `minimum_top_tension_16q` [src/digitalmodel/drilling_riser/assembly.py:L186-L236] |
| TSJ station geometry, tension, pressures and strength | m; mm; N or kN; Pa or MPa; Pa | Positive tension, strength and design factor; nonnegative pressures and positions; increasing stations; OD greater than twice wall; pressure defaults to zero | `tsj_workflow` parser [src/digitalmodel/drilling_riser/tsj_workflow.py:L19-L37] [src/digitalmodel/drilling_riser/tsj_workflow.py:L114-L179] |
| Beam length, offset, tension, stiffness, lateral load | m; m; N; N·m²; N/m | Length, tension and stiffness positive; at least 3 grid nodes, default 201 | `solve_static_response` [src/digitalmodel/drilling_riser/riser_response.py:L72-L107] |
| Conductor shear, stand-off, soil modulus and stiffness | N; m; N/m²; N·m² | Nonnegative shear and stand-off; positive modulus and stiffness; default 201 depth nodes | `solve_conductor_moment` [src/digitalmodel/drilling_riser/conductor_response.py:L48-L86] |
| Envelope axes and criteria | % water depth; m/s; Hs m; Tp s; angles deg; factors dimensionless | Caller supplies sweep and criteria; motion-response contributions default to zero; dynamic lookup defaults off | `compute_operating_envelope` [src/digitalmodel/drilling_riser/envelope.py:L278-L318] |
| Telemetry | offset m; heading deg; tension and thrust kN at input | Offset absent: record omitted; other absent channels remain optional; forces convert to N | `parse_snapshots` and helpers [src/digitalmodel/drilling_riser/telemetry_inputs.py:L107-L158] |
| Drift configuration | mass kg; EDS s; thrust efficiency dimensionless | Consumes the lower effective mass; initial offset, velocity and elapsed time default to zero | `drift_off_screen` [src/digitalmodel/drilling_riser/drift_off.py:L181-L209] [src/digitalmodel/drilling_riser/drift_off.py:L245-L257] |

### Outputs and conventions

| Output | Unit and interpretation | Evidence |
| --- | --- | --- |
| Stack-up summary and CSV | Required tension kN; required wall mm; profile columns `depth_factor`, `effective_tension_kn`; default relative output folder `results` beside input | [src/digitalmodel/drilling_riser/workflow.py:L54-L76] [src/digitalmodel/drilling_riser/workflow.py:L131-L159] |
| TSJ summary and CSV | Area m²; section modulus m³; true-wall force N; axial, hoop and equivalent stresses Pa; utilisation dimensionless; maximum selects governing station | [src/digitalmodel/drilling_riser/tsj_workflow.py:L49-L111] [src/digitalmodel/drilling_riser/tsj_workflow.py:L182-L209] |
| Radial tool clearance | mm; half the difference between riser ID and tool OD | [src/digitalmodel/drilling_riser/tool_passage.py:L15-L37] |
| Section properties | Consistent input units; imperial helper returns m², m⁴, N and N·m² | [src/digitalmodel/drilling_riser/section.py:L34-L49] [src/digitalmodel/drilling_riser/section.py:L114-L146] |
| Static response | z from lower joint at 0 to upper joint at L; offset is upper-end displacement; signed slopes rad, moments N·m and lower shear magnitude N | [src/digitalmodel/drilling_riser/riser_response.py:L11-L14] [src/digitalmodel/drilling_riser/riser_response.py:L41-L69] [src/digitalmodel/drilling_riser/riser_response.py:L120-L140] |
| Conductor response | Depth positive below mudline; stand-off above mudline; moment N·m | [src/digitalmodel/drilling_riser/conductor_response.py:L3-L12] [src/digitalmodel/drilling_riser/conductor_response.py:L33-L45] |
| Envelope | Arrays ordered offset × current × sea state; NaN marks absent or excluded limits; dimensionless utilisation and boolean allowable mask | [src/digitalmodel/drilling_riser/envelope.py:L182-L207] [src/digitalmodel/drilling_riser/envelope.py:L407-L446] |
| Drift result | Watch radius and disconnect offset m; time and margin s; forces N; held station uses infinite time; provenance identifies quasi-static screening | [src/digitalmodel/drilling_riser/drift_off.py:L79-L94] [src/digitalmodel/drilling_riser/drift_off.py:L211-L257] |
| Campaign horizontal offset | Percent water depth mapped to x = r cos(heading), y = r sin(heading), in m | [src/digitalmodel/drilling_riser/campaign.py:L96-L99] |

The static beam response has no harmonic phase input; metocean bin boundaries are upper edges and sea-state representatives use occupied-cell midpoints. [src/digitalmodel/drilling_riser/riser_response.py:L72-L97] [src/digitalmodel/drilling_riser/metocean_inputs.py:L32-L91]

## 3. Method and governing standards

The following are implementation descriptions rather than confirmation of compliance with a licensed standard; clause text not checked against the licensed copy.

| Reference as recorded | Implemented method and provenance boundary | Evidence |
| --- | --- | --- |
| API RP 16Q, *Design, Selection, Operation and Maintenance of Marine Drilling Riser Systems*, first edition 1993, reaffirmed 2001; tension factors and minimum slip-ring tension | `minimum_slip_ring_tension` combines factored submerged weight, buoyancy and fluid pressure-area terms; getter sidecars identify the tension-factor clause | [src/digitalmodel/drilling_riser/stackup.py:L111-L163] [src/digitalmodel/riser_database/getters.py:L61-L69] [src/digitalmodel/riser_database/getters.py:L175-L219] |
| API RP 16Q; tensioner-system reduction | `tensioner_system_factor` computes N/[efficiency × (N − failed)]; `minimum_top_tension_16q` additionally applies the caller's angular allowance factor | [src/digitalmodel/drilling_riser/assembly.py:L186-L236] |
| Uncited screening defaults; Barlow relation | `top_tension_required` multiplies weight by a dynamic factor; `effective_tension` subtracts the depth fraction of weight; `wall_thickness_required` computes P × OD × SF/(2 × SMYS); these are separate from the tensioner-failure chain | [src/digitalmodel/drilling_riser/stackup.py:L28-L108] [src/digitalmodel/drilling_riser/assembly.py:L8-L20] |
| Quantity assembly and schedule approximations | `RiserStackupModel` sums count-weighted loads and splits gross weight from buoyancy; the two schedule-loader definitions build records using converted weights, steel wet fractions and proportional short-joint weights | [src/digitalmodel/drilling_riser/assembly.py:L110-L155] [src/digitalmodel/drilling_riser/schedule_assembly.py:L302-L495] [src/digitalmodel/drilling_riser/schedule_assembly.py:L532-L724] |
| Schedule-specific tension convention | `ScheduleAssembly.minimum_top_tension_16q_kn` calls slip-ring composition with neutral weight factors and then the schedule's top-tension multiplier; it is not the explicit surviving-tensioner calculation | [src/digitalmodel/drilling_riser/schedule_assembly.py:L196-L212] |
| Hollow circular section; reference title recorded as Roark's Formulas for Stress and Strain | `cross_sectional_area`, `moment_of_inertia`, `polar_moment_of_inertia`, `shear_modulus`, `section_properties` and imperial conversion compute A, I, J, G, EA, EI and GJ | [src/digitalmodel/drilling_riser/section.py:L9-L21] [src/digitalmodel/drilling_riser/section.py:L52-L146] |
| API RP 2RD, *Recommended Practice for Design of Risers for Floating Production Systems (FPSs) and Tension-Leg Platforms (TLPs)*; membrane screening, implementation edition not specified | `tsj_workflow._section_result` adds internal pressure-area force and subtracts external pressure-area force from effective tension, divides by steel area, computes thin-wall hoop stress and von Mises membrane stress, then divides by allowable stress | [src/digitalmodel/drilling_riser/tsj_workflow.py:L1-L5] [src/digitalmodel/drilling_riser/tsj_workflow.py:L74-L111] |
| API STD 2RD, *Dynamic Risers for Floating Production Systems*, second edition 2013; dynamic riser design criteria | `envelope._von_mises_utilisation` delegates to `check_api_rp_2rd`; historical RP names remain in the API; the getter emits a standards sidecar before the envelope extracts scalar values | [src/digitalmodel/orcaflex/code_check_engine.py:L1-L15] [src/digitalmodel/drilling_riser/envelope.py:L246-L275] [src/digitalmodel/riser_database/getters.py:L222-L243] |
| Static tensioned beam equation | `solve_static_response` solves EI y'''' − T y'' = w with pinned ends in exponential form; positive constant T and EI are required | [src/digitalmodel/drilling_riser/riser_response.py:L3-L31] [src/digitalmodel/drilling_riser/riser_response.py:L98-L140] |
| Beam on elastic foundation; Hetényi, Beams on Elastic Foundation, 1946, named in code | `solve_conductor_moment` evaluates the semi-infinite Winkler beam with a shear-induced stand-off end moment; layered soil, axial load and dynamics are excluded | [src/digitalmodel/drilling_riser/conductor_response.py:L3-L23] [src/digitalmodel/drilling_riser/conductor_response.py:L79-L93] |
| Static drag and operating-limit composition | `CurrentProfile.drag_load_n_per_m` evaluates uniform quadratic drag; `compute_operating_envelope` computes setdown, flex-joint, stress, stroke, moonpool and optional conductor utilisation, then selects the governing active limit | [src/digitalmodel/drilling_riser/envelope.py:L108-L119] [src/digitalmodel/drilling_riser/envelope.py:L325-L446] |
| Constant-acceleration screen | `environmental_force_n` delegates environmental loads and converts kN to N; `watch_circle_limit` scans the envelope; `drift_off_screen` solves remaining = v₀t + at²/2 and subtracts the EDS interval | [src/digitalmodel/drilling_riser/drift_off.py:L97-L178] [src/digitalmodel/drilling_riser/drift_off.py:L236-L257] |
| Algebraic damping and geometry helpers | `rayleigh_damping_coefficients`, `modal_damping_equivalent`, `significant_wave_height_limit`, `operability_fraction`, `watch_circle_radius_m`, and the three tool-passage functions implement their documented algebra | [src/digitalmodel/drilling_riser/damping.py:L51-L107] [src/digitalmodel/drilling_riser/operability.py:L10-L81] [src/digitalmodel/drilling_riser/tool_passage.py:L15-L88] |
| DNV-RP-C205, *Environmental Conditions and Environmental Loads*, §3.3; scatter-bin convention | `reduce_to_scatter` counts upper-edge bins and `scatter_to_seastates` emits occupied midpoints; the header records §3.3 but no edition or clause title | [src/digitalmodel/drilling_riser/metocean_inputs.py:L1-L18] [src/digitalmodel/drilling_riser/metocean_inputs.py:L32-L91] |
| API Spec 16F, *Specification for Marine Drilling Riser Equipment*; running-equipment reference | The tool-passage header names the standard, while its functions perform radial-clearance and spacing arithmetic; edition and clause are absent | [src/digitalmodel/drilling_riser/tool_passage.py:L1-L4] [src/digitalmodel/drilling_riser/tool_passage.py:L15-L88] |
| Solver continuation algorithms, no standards compliance asserted | `seeded_statics`, `_statics_path`, `robust_statics`, `open_water_statics`, `hang_off_statics` and `RiserCampaignAdapter.statics` launch supplied model solves, ramp or walk intermediate states and retain path failures | [src/digitalmodel/drilling_riser/campaign.py:L242-L268] [src/digitalmodel/drilling_riser/campaign.py:L400-L731] [src/digitalmodel/drilling_riser/campaign.py:L888-L910] |
| Atlas and correction policies | `governing_utilisation` caps the maximum active response; `build_atlas` measures holdout interpolation error; `screen_operability` escalates uncertainty across the utilisation boundary; residual correction uses max(corrected, raw) | [src/digitalmodel/drilling_riser/operability_atlas.py:L109-L149] [src/digitalmodel/drilling_riser/operability_atlas.py:L178-L237] [src/digitalmodel/drilling_riser/operability_screening.py:L53-L89] [src/digitalmodel/drilling_riser/response_correction.py:L83-L141] |

The checked-in API STD 2RD edition labels agree between getter metadata, atlas configuration and code-check documentation; the historical discrepancy is recorded by #2161 rather than treated as a current mismatch. [src/digitalmodel/riser_database/getters.py:L71-L78] [src/digitalmodel/drilling_riser/operability_configs.yml:L15-L17] [src/digitalmodel/orcaflex/code_check_engine.py:L13-L14]

Publication titles and catalogue editions are metadata only: API's *2024 Publications Catalog*, entries “RP 16Q” (second edition, April 2017), “Std 2RD” (second edition, September 2013) and “Spec 16F” (second edition, November 2017), confirms the titles above; the code does not establish which 16F edition applies. ([API, 2024 Publications Catalog](https://www.api.org/-/media/files/publications/2024-catalog/2024-publication-catalog.pdf), entries named above.) The RP 2RD title and first edition, June 1998, are listed in 30 CFR §250.198(e)(62); that publication record does not establish this workflow's implemented edition. ([U.S. Government Publishing Office, 30 CFR §250.198, 2025 edition, paragraph (e)(62)](https://www.govinfo.gov/content/pkg/CFR-2025-title30-vol2/pdf/CFR-2025-title30-vol2-sec250-198.pdf).) DNV's public metadata page confirms the C205 title, while the module supplies the §3.3 locator. ([DNV, DNV-RP-C205, Environmental Conditions and Environmental Loads, publication metadata](https://www.dnv.com/energy/standards-guidelines/dnv-rp-c205-environmental-conditions-and-environmental-loads/).) [src/digitalmodel/drilling_riser/metocean_inputs.py:L7-L12]

UNVERIFIED: Missing implementation editions and clause titles remain open, including the API RP 16Q 1993 clause locators versus the 2017 second-edition metadata; the getter explicitly records that edition question. The loaded-beam shear case V3 has a closed-form oracle, but full beam-response provenance and independent conductor, taper and damping qualification remain open; clause text not checked against the licensed copy; issue pending (2). [src/digitalmodel/riser_database/getters.py:L61-L69] [tests/drilling_riser/test_riser_response.py:L116-L120]

## 4. Validated worked examples

The three validated cases below use independently specified closed-form solutions of the boundary-value problem, including loaded-beam lower shear, rather than snapshots of the implementation; their synthetic input constants are defined in the test module. [tests/drilling_riser/test_riser_response.py:L1-L31] [tests/drilling_riser/test_riser_response.py:L116-L120]

| Case and classification | Input and independent expectation | Tolerance basis | Existing test node ID | Offline result at the code pin |
| --- | --- | --- | --- | --- |
| V1: quiescent, validated closed form | Test `_BASE`, zero offset and zero current: zero deflection, slopes and moment | Absolute 1e-9 m, 1e-12 rad and 1e-3 N·m, as asserted | `tests/drilling_riser/test_riser_response.py::test_no_offset_no_current_is_zero` | PASS; BASE and AFTER [tests/drilling_riser/test_riser_response.py:L43-L49] |
| V2: imposed offset, validated closed form | Test `_BASE`, 30 m offset and zero current: slope X/L = 0.02 rad, straight deflection and zero moment | Relative 1e-6 on slopes; absolute 1e-6 m on shape and 1 N·m on moment | `tests/drilling_riser/test_riser_response.py::test_pure_offset_is_rigid_tilt_no_moment` | PASS; BASE and AFTER [tests/drilling_riser/test_riser_response.py:L55-L65] |
| V3: loaded-beam lower shear, validated closed form | Synthetic `_BASE`, offset X = 30 m and drag load w from the test: lower shear T × X/L + w × L/2, in N | Relative 1e-9 dimensionless, as asserted | `tests/drilling_riser/test_riser_response.py::test_shear_lower_matches_closed_form` | PASS; BASE and AFTER [tests/drilling_riser/test_riser_response.py:L116-L120] |
| R1: stack-up workflow, regression | Synthetic YAML constructed by the test; effective tensions 1300, 800 and 300 kN at the three depth fractions | Existing `pytest.approx` comparisons; output location and profile-column checks | `tests/drilling_riser/test_riser_stackup_workflow.py::test_riser_stackup_workflow_writes_profile_next_to_input` | PASS; BASE and AFTER [tests/drilling_riser/test_riser_stackup_workflow.py:L10-L57] |
| R2: taper workflow, regression | Synthetic four-station YAML constructed by the test; governing position 30 m, finite utilisation in (0, 1) | Existing station, row-count and summary comparisons; no independent benchmark stress is asserted | `tests/drilling_riser/test_tsj_workflow.py::test_tsj_sizing_workflow_writes_section_utilisation_next_to_input` | PASS; BASE and AFTER [tests/drilling_riser/test_tsj_workflow.py:L11-L64] |
| R3: assembly golden, blocked regression | External contract supplies the expected tension; no private value is reproduced here | Contract absolute and relative tolerances in `_assert_golden`, not an independent standard annex | `tests/drilling_riser/test_assembly_golden.py::test_16q_min_top_tension_golden` | NOT_RUN: genuinely private reference set absent; collected SKIPPED [tests/drilling_riser/test_assembly_golden.py:L104-L148] |
| R4: drive-off reproduction, blocked regression candidate | Recorded fixture lacks a complete DP state, environment, section, criteria and configuration; no output equivalence is claimed | No comparison tolerance established; coupled transient physics is missing | `tests/drilling_riser/test_drive_off_worked_example.py::test_reproduce_drive_off_worked_example` | FAIL on direct invocation of the existing test body at BASE and AFTER: reaches its unconditional `pytest.fail` because complete case inputs and coupled response are absent; suite result SKIPPED by the unconditional marker; #162 [tests/drilling_riser/test_drive_off_worked_example.py:L92-L142] [tests/drilling_riser/test_drive_off_worked_example.py:L154-L182] |

The durable `riser-stackup` and `tsj-sizing` examples are also exercised through their workflow-registry nodes; they check integration and expected outputs, not independent external validation. [docs/registry/workflows.yaml:L407-L421] [tests/workflows/test_durable_workflows.py:L658-L688]

Offline run records use `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`, with the repository's existing addopts unchanged. BASE is `a4938a25314454cd74dfd8cabcc0524d4cabcade`; AFTER is its uncommitted documentation change. Both have direct-package source and the recorded test files identical to the code pin. NOT_RUN is a subset of collected skips for genuinely absent private references, never an additional test count. R4 was also invoked directly as an existing Python test function, without changing its suite marker; that separate diagnostic failed at both stages. [tests/drilling_riser/test_drive_off_worked_example.py:L154-L182]

| Selection | BASE: collected / passed / failed / skipped / NOT_RUN | AFTER | Evidence boundary |
| --- | --- | --- | --- |
| 27 direct module test files and the envelope-library parametric test file | 273 / 245 / 1 / 27 / 26 | 273 / 245 / 1 / 27 / 26 | 26 private-reference NOT_RUN; R4 is a suite skip with a separate failing direct invocation |
| `tests/contracts` (subset of domain below) | 455 / 454 / 1 / 0 / 0 | 455 / 454 / 1 / 0 / 0 | Force-unit and dependency-scanner contracts |
| Full `tests-contracts` domain from the quality-gate configuration | 909 / 848 / 1 / 60 / 0, plus 1 collection skip | 909 / 848 / 1 / 60 / 0, plus 1 collection skip | Routing contract: 10 PASS at BASE and AFTER; 61 total skips are stale or absent compatibility/migration surfaces |
| Two workflow-registry nodes above | 2 / 2 / 0 / 0 / 0 | 2 / 2 / 0 / 0 / 0 | Synthetic durable examples |
| `tests/riser_database/test_citations.py` | 19 / 18 / 0 / 1 / 1 | 19 / 18 / 0 / 1 / 1 | One additional external-criteria skip |

The nonpassing module test is `tests/drilling_riser/test_operability_atlas.py::test_rebuild_is_byte_identical`: the rebuilt `grid.parquet` differs in bytes; the contract failure is `tests/contracts/test_string_addressed_dependencies.py::test_the_three_known_defects_are_still_derived`, whose expected `xlsxwriter` call site is not found. [tests/drilling_riser/test_operability_atlas.py:L99-L109] [tests/contracts/test_string_addressed_dependencies.py:L620-L635]

The 27 skipped nodes comprise 11 assembly goldens, 11 schedule checks, two private-criteria envelope checks, one registry-batch check, one getter-backed correction check and the blocked drive-off reproduction. [tests/drilling_riser/test_assembly_golden.py:L104-L119] [tests/drilling_riser/test_schedule_assembly.py:L45-L58] [tests/drilling_riser/test_envelope.py:L40-L55] [tests/drilling_riser/test_registry_batch.py:L35-L42] [tests/drilling_riser/test_response_correction.py:L149-L168] [tests/drilling_riser/test_drive_off_worked_example.py:L167-L182]

Nonpassing module/reference nodes retained in both runs: 27 skips remain NOT_RUN because their genuinely private reference set is absent; R4 is a suite skip and a separately executed failing reproduction body:

- `tests/drilling_riser/test_assembly_golden.py::test_16q_min_top_tension_golden` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_assembly_golden.py::test_16q_rsu0014_setting_side_consistency` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_assembly_golden.py::test_16q_rsu0023_mud_weight_sweep_golden` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_assembly_golden.py::test_16q_rsu0023_tsrmin_composition_golden` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_assembly_golden.py::test_16q_wave2_min_vertical_top_tension_golden[RSU-0010-keys0-min_slip_ring_tension_kips-n_wires-min_vertical_top_tension_kips]` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_assembly_golden.py::test_16q_wave2_min_vertical_top_tension_golden[RSU-0012-keys1-min_slip_ring_tension_kips-n_wires-min_vertical_top_tension_kips]` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_assembly_golden.py::test_16q_wave2_min_vertical_top_tension_golden[RSU-0014-keys2-tsr_min_kips-n_tensioners-api_min_vertical_top_tension_kips]` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_assembly_golden.py::test_16q_wave2_min_vertical_top_tension_golden[RSU-0023-keys3-tsr_min_kips-n_tensioners-tmin_vert_kips]` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_assembly_golden.py::test_16q_wave2_overall_min_top_tension_golden` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_assembly_golden.py::test_rsu0077_shallow_water_tension_to_rotate_contract_schema` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_assembly_golden.py::test_setting_side_values_present_but_not_asserted` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_drive_off_worked_example.py::test_reproduce_drive_off_worked_example` — SKIPPED in pytest; direct invocation FAIL, complete case inputs and coupled response absent (#162).
- `tests/drilling_riser/test_envelope.py::test_16q_and_amjig_can_produce_different_allowable_masks` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_envelope.py::test_resolve_amjig_criteria_from_private_wiki` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_registry_batch.py::test_rsu0077_registry_batch_is_runnable_source` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_response_correction.py::test_cited_limit_is_untouched_by_correction` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_schedule_assembly.py::test_rsu0019_endpoints_within_3_percent` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_schedule_assembly.py::test_rsu0021_endpoints_finding_band` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_schedule_assembly.py::test_rsu0021_lmrp_inclusion_hypothesis` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_schedule_assembly.py::test_rsu0038_schedule_is_raster_only` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_schedule_assembly.py::test_rsu0040_endpoints_finding_band` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_schedule_assembly.py::test_rsu0040_five_bare_swap_consistency` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_schedule_assembly.py::test_schedule_assembly_structure[RSU-0019]` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_schedule_assembly.py::test_schedule_assembly_structure[RSU-0021]` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_schedule_assembly.py::test_schedule_assembly_structure[RSU-0040]` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_schedule_assembly.py::test_tension_is_mud_linear_increasing` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_schedule_assembly.py::test_unknown_rsu_fails_closed` — SKIPPED / NOT_RUN.
- `tests/riser_database/test_citations.py::test_get_amjig_envelope_criteria_emits_private_wiki_citation` — SKIPPED / NOT_RUN.
- `tests/drilling_riser/test_operability_atlas.py::test_rebuild_is_byte_identical` — FAIL, byte comparison.
- `tests/contracts/test_string_addressed_dependencies.py::test_the_three_known_defects_are_still_derived` — FAIL, missing expected dependency call site.

The criteria-getter skip is the absent private criteria page; the test retains its existing fixture check. [tests/riser_database/test_citations.py:L43-L56]

## 5. QA gates

Gate applicability follows AQG r0, section 10, OrcaFlex riser and mooring: G0–G3 and G5 apply; G4 is conditional on new diffraction data (AQG r0, sections 3 and 10).

The criteria below are proposed executable checks for this module, not completed solver qualification; offline PASS applies only to the named offline evidence, and does not satisfy G1–G3.

| Check | Gate and applicability | Module-specific procedure and numeric pass criterion | Evidence and present status |
| --- | --- | --- | --- |
| C01 | G0, applicable | Normalise the component fixture: added + skipped = 36 records, skipped = 0 records; converted length agrees within relative 1e-6 dimensionless | PASS in `test_register_riser_components_all_36` [tests/drilling_riser/test_adapter_integration.py:L78-L90] |
| C02 | G0, applicable | Verify `parse_snapshots` conversion and `snapshot_to_offset_pct` inverse against `watch_circle_radius_m`: 0 unmatched records; reconstructed offset within the existing `pytest.approx` tolerance (default relative 1e-6 dimensionless) | PASS in the telemetry inverse test [tests/drilling_riser/test_telemetry_inputs.py:L49-L92] |
| C03 | G0, applicable | Omit buoyancy uplift for a buoyed component: `gross_submerged_weight_and_uplift` must raise, with 0 computed demands from incomplete inputs | PASS in the assembly fail-closed test [tests/drilling_riser/test_assembly.py:L71-L81] |
| C04 | G0, applicable | Before using `EnvelopeResult.allowable_mask`, require 0 cells accepted with an escalated dynamic stress channel; inspect `dynamic_provenance.escalated_points` and per-limit NaN values | Offline diagnostic confirmed an allowable cell with NaN stress and 1 escalated point; no detection check exists for the complete mask condition; issue pending (3) [src/digitalmodel/drilling_riser/envelope.py:L375-L391] [src/digitalmodel/drilling_riser/envelope.py:L412-L432] [tests/drilling_riser/test_envelope.py:L298-L308] |
| C05 | G0, applicable | Before `watch_circle_limit` selects the largest allowable index, require 0 false-to-true transitions (count) in each slice of `allowable_mask` ordered by increasing offset; otherwise reject the watch-radius screen | Proposed check; no detection check exists for a non-monotonic mask; issue pending (3) [src/digitalmodel/drilling_riser/drift_off.py:L149-L173] |
| C11 | G1, applicable to campaign model output | `RiserCampaignAdapter.build` delegates a `master.yml` to the global writer; load that exact file through `global_model.orcaflex_run.load_model` in the target OrcaFlex version; require 0 load errors, 0 warnings, 0 missing objects and 0 version mismatches (counts) | NOT_RUN: licensed target program and version unavailable; do not substitute Python YAML parsing [src/digitalmodel/drilling_riser/campaign.py:L847-L853] [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L22-L42] |
| C21 | G2, applicable | On the generated configuration, statics must converge; compare solver top tension to a separately qualified segment-weight balance within 5.0% and flex-joint angle to the applicable independent reference within 1.0 deg; include every object, with 0 unchecked objects | NOT_RUN: solver required; V1/V2 are offline limiting cases only; do not use an unreconciled global-model reference as the independent oracle [src/digitalmodel/drilling_riser/campaign.py:L758-L810] [tests/drilling_riser/test_riser_response.py:L43-L65] (AQG r0, G2.1–G2.6) |
| C22 | G2, applicable | Run at least 6 offsets per governing direction through `RiserCampaignAdapter.statics`; 0 failed solves, 0 unplanned compression states; retain tension-ring equilibrium and yaw checks, plus the initial restoring-force comparison within 20.0% | NOT_RUN: solver required; campaign continuation is not itself evidence of monotonic restoration [src/digitalmodel/drilling_riser/campaign.py:L568-L634] [src/digitalmodel/drilling_riser/campaign.py:L758-L810] [src/digitalmodel/drilling_riser/campaign.py:L888-L910] (AQG r0, G2.5) |
| C31 | G3, applicable | Run the campaign model for 1 s with production settings; require stop time reached, 0 warnings/errors/nonfinite results and dynamic/static tension ratio within [0.75, 1.25], with 0 new slack/compression states | NOT_RUN: solver required; retain solver images and time histories [src/digitalmodel/drilling_riser/campaign.py:L912-L943] (AQG r0, G3.1–G3.4) |
| C41 | G4, conditional | NOT_APPLICABLE to this offline documentation change: no new diffraction data are introduced; if a future `case_spec` shifts an RAO origin or introduces new vessel data, require 1 linked diffraction record per new dataset and 0 missing records | Conditional check; no diffraction qualification claimed [src/digitalmodel/drilling_riser/campaign.py:L157-L161] (AQG r0, section 8 and family mapping) |
| C51 | G5, applicable | Build an evidence index for each campaign result: code pin, input fingerprint, criteria revision, gate statuses and exceptions; require 0 unresolved links and 0 accepted results without a complete required-gate record (counts) | NOT_RUN for a solver report: this pack supplies offline evidence only; report-template work remains #814 [src/digitalmodel/drilling_riser/campaign.py:L835-L987] (AQG r0, G5.1–G5.3) |

The global-model pack owns the generated campaign-model gate procedures; C11, C21, C22, C31 and C51 here are caller obligations that reuse those procedures, with no separate qualification claim. [src/digitalmodel/drilling_riser/global_model/docs/agent-pack.md:L180-L192]

C21 and C22 require a qualified independent reference and configuration-specific physical bounds; passing this pack's offline examples alone does not establish solver model acceptance (AQG r0, G2.3–G2.6).

## 6. Known failure modes and pitfalls

| Symptom or pitfall | Cause and detection | Incident or follow-up; observation date | Evidence |
| --- | --- | --- | --- |
| Silent telemetry/metocean misalignment, then truncation | `parse_snapshots` drops missing-offset records before index pairing, shifting every later snapshot onto the wrong metocean condition; `zip` truncates unequal streams; an empty parsed stream or missing initial timestamp also raises; no detection check exists | issue pending (5); 2026-10-15 | [src/digitalmodel/drilling_riser/telemetry_inputs.py:L135-L158] [src/digitalmodel/drilling_riser/twin_loop.py:L268-L295] [src/digitalmodel/drilling_riser/twin_loop.py:L125-L129] |
| Apparently allowable envelope with missing checks | Absent capacities leave limits NaN and excluded from selection; C03 protects assembly inputs, but no detection check exists for capacity completeness before accepting an envelope | #811; 2026-10-15 | [src/digitalmodel/drilling_riser/envelope.py:L143-L153] [src/digitalmodel/drilling_riser/envelope.py:L367-L418] |
| Dynamic stress channel escalates but allowable mask can still be computed from remaining limits | Out-of-domain stress becomes NaN; governing selection substitutes negative infinity; C04 detects acceptance with missing stress; the existing test checks NaN and counter only | issue pending (3); 2026-10-15 | [src/digitalmodel/drilling_riser/envelope.py:L375-L418] [tests/drilling_riser/test_envelope.py:L298-L308] |
| Force-unit error could falsely report held station | Environmental loads return kN while thrust is N; C02 and the explicit environmental-force test guard boundary conversions | #1447; 2026-10-15 | [tests/contracts/test_force_units_boundary_contract.py:L1-L10] [src/digitalmodel/drilling_riser/drift_off.py:L122-L125] [tests/drilling_riser/test_drift_off.py:L89-L105] |
| Historical incomplete-campaign acceptance | The pinned sub-package now requires expected cases and ledger outcomes before a passing aggregate; C51 must retain that coverage contract; existing sub-package tests detect missing and failed cases | #2277 remains open; historical defect with a current guard; 2026-10-15 | [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L244-L277] [tests/drilling_riser/postprocess/test_summary_coverage.py:L1-L118] |
| Drift-off screen mistaken for coupled drive-off reproduction | Scalar constant acceleration does not reproduce coupled time histories; R4 direct invocation fails on the absent complete inputs and coupled response; no detection check exists for the missing coupled physics | #162; 2026-10-15 | [tests/drilling_riser/test_drive_off_worked_example.py:L154-L182] [src/digitalmodel/drilling_riser/drift_off.py:L61-L67] |
| Private-reference tests unavailable | Contract and schedule tests skip when external registry cannot resolve; no detection check exists for unavailable private-reference evidence; no private values have been used as public reference data | #1470; 2026-10-15 | [tests/drilling_riser/test_assembly_golden.py:L104-L119] [tests/drilling_riser/test_schedule_assembly.py:L45-L58] |
| Dormant depth-resolved current | Current profiles round-trip while drag uses only surface speed; no detection check exists for a depth-integrated response because it is not implemented | #811; 2026-10-15 | [src/digitalmodel/drilling_riser/envelope.py:L88-L119] |
| Byte-identical atlas rebuild fails | BASE rebuild differs at `grid.parquet`; no section-5 check exists; detected only by `test_rebuild_is_byte_identical`, which compares bytes rather than establishing a physical discrepancy | issue pending (1); 2026-10-15 | [tests/drilling_riser/test_operability_atlas.py:L99-L109] |
| Dependency-scanner contract assertion fails | Expected `xlsxwriter` call site is not derived; no section-5 check exists; detected by `test_the_three_known_defects_are_still_derived` | issue pending (6); 2026-10-15 | [tests/contracts/test_string_addressed_dependencies.py:L620-L635] |
| TSJ sizing utilisation omits bending | Section modulus is reported but unused in axial/hoop membrane utilisation; no detection check exists for bending completeness | issue pending (2); 2026-10-15 | [src/digitalmodel/drilling_riser/tsj_workflow.py:L74-L111] |
| Non-monotonic envelope can overstate watch radius | `watch_circle_limit` takes the largest allowable offset index rather than the first crossing; C05 requires 0 false-to-true transitions before accepting that radius | issue pending (3); 2026-10-15 | [src/digitalmodel/drilling_riser/drift_off.py:L149-L173] |
| Standards reference mistaken for verified compliance | Missing edition/clause information and uncited methodology defaults need source confirmation; no detection check exists for licensed-text equivalence | issue pending (2), hand-check provenance #2528, historical edition correction #2161; 2026-10-15 | [src/digitalmodel/drilling_riser/stackup.py:L13-L25] [src/digitalmodel/drilling_riser/tsj_workflow.py:L1-L5] [src/digitalmodel/drilling_riser/tool_passage.py:L1-L4] |

## 7. Lessons log

| Date | Change reference | Lesson and evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- |
| 2026-10-15 | Initial pack | Constant-factor top tension and surviving-tensioner minimum are distinct calculations [src/digitalmodel/drilling_riser/assembly.py:L8-L20] | Separate input and method rows | Not created; issue pending (4) |
| 2026-10-15 | Initial pack | A missing transient input set cannot be replaced by a generic screening calculation [tests/drilling_riser/test_drive_off_worked_example.py:L154-L182] | Keep reproduction NOT_RUN | #162 |
| 2026-10-15 | Initial pack | Summary coverage now requires expected cases and final ledger outcomes [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L244-L277] | Historical incident with current guard | #2277 |
| 2026-10-15 | Pack revision 1 | Missing telemetry offsets shift index-paired environmental records [src/digitalmodel/drilling_riser/telemetry_inputs.py:L135-L158] [src/digitalmodel/drilling_riser/twin_loop.py:L268-L279] | Put silent misalignment first and retain the missing detection check | issue pending (5) |

## 8. Ownership

| Responsibility | Required role |
| --- | --- |
| Engineering review | Riser and mooring quality manager on an independent provider |
| Deputy review | Independent riser and mooring reviewer; appointment by the maintainer |
| Pack production | Engineering documentation contributor |
| Engineering classification or criteria ambiguity | Family quality manager |
| Cross-family evidence | Relevant family quality manager, including diffraction review when G4 applies |
| Publication and unresolved scope | Maintainer |

These are required review responsibilities, not a record of completed review; DRAFT remains the pack status.

## 9. Dependencies

### Upstream, downstream and excluded sub-packages

| Dependency | Connection and evidence |
| --- | --- |
| `worldenergydata` component vocabulary | CSV input is normalised by the adapter; no solver interface is needed for its conversion [src/digitalmodel/drilling_riser/adapter.py:L1-L17] [src/digitalmodel/drilling_riser/adapter.py:L67-L91] |
| `riser_database` and `citations` | Criteria getters validate standards sidecars; the envelope takes their numeric values; private criteria require external reference data [src/digitalmodel/riser_database/getters.py:L175-L219] [src/digitalmodel/drilling_riser/envelope.py:L214-L256] |
| `subsea/mooring_analysis` | Environmental loads feed drift-off through an explicit kN-to-N boundary [src/digitalmodel/drilling_riser/drift_off.py:L97-L131] |
| `parametric` and `residual` | Atlas interpolation supports screening; residual models supply angle tracking while decisions retain the maximum of corrected and raw values [src/digitalmodel/drilling_riser/operability_screening.py:L53-L89] [src/digitalmodel/drilling_riser/response_correction.py:L83-L122] |
| `global_model` (excluded sub-package) | Specification classes, text generation and solver extraction are campaign dependencies; the routing entry points to its own pack [src/digitalmodel/drilling_riser/campaign.py:L835-L952] [docs/registry/module-routing.yaml:L105-L108] |
| `postprocess` (excluded sub-package) | Stress-range constants and response-channel extraction support campaign outputs; coverage aggregation belongs to that sub-package [src/digitalmodel/drilling_riser/campaign.py:L63-L65] [src/digitalmodel/drilling_riser/campaign.py:L913-L950] [src/digitalmodel/drilling_riser/postprocess/evaluate.py:L266-L284] |
| `stackup_drawing` (excluded sub-package) | Separate drawing workflow registered independently of stack-up and sizing [docs/registry/workflows.yaml:L407-L430] |
| Durable workflows | `riser-stackup` and `tsj-sizing` have registered input, output and test routes [docs/registry/workflows.yaml:L407-L421] |

UNVERIFIED: No target solver version, load record, licensed static/dynamic qualification or new-diffraction evidence is established by this offline pack; #2433 and #814 track benchmark and report evidence respectively.

### OrcaFlex references in the direct package

Each source-text occurrence, including lowercase module references and declarations that the solver is unnecessary, is indexed here; no interface is invoked by this index.

| File | Occurrences and dependency classification | Evidence |
| --- | --- | --- |
| `campaign.py` | Header: solver campaign; runner reference; coupling-output explanation; lazy runner exceptions at four sites; lazy solver extraction helper at five sites; adapter declaration | [src/digitalmodel/drilling_riser/campaign.py:L1-L1] [src/digitalmodel/drilling_riser/campaign.py:L51-L51] [src/digitalmodel/drilling_riser/campaign.py:L102-L105] [src/digitalmodel/drilling_riser/campaign.py:L586-L586] [src/digitalmodel/drilling_riser/campaign.py:L651-L651] [src/digitalmodel/drilling_riser/campaign.py:L699-L699] [src/digitalmodel/drilling_riser/campaign.py:L735-L735] [src/digitalmodel/drilling_riser/campaign.py:L741-L741] [src/digitalmodel/drilling_riser/campaign.py:L772-L772] [src/digitalmodel/drilling_riser/campaign.py:L777-L777] [src/digitalmodel/drilling_riser/campaign.py:L836-L836] [src/digitalmodel/drilling_riser/campaign.py:L913-L913] [src/digitalmodel/drilling_riser/campaign.py:L956-L956] |
| `drift_off.py` | Deferred licensed solver named in disclaimer | [src/digitalmodel/drilling_riser/drift_off.py:L61-L67] |
| `envelope.py` | Offline code-check dependency; dynamic atlas described as a solver stub | [src/digitalmodel/drilling_riser/envelope.py:L42-L42] [src/digitalmodel/drilling_riser/envelope.py:L298-L304] |
| `metocean_inputs.py` | ScatterDiagram type reference and import; Python data dependency | [src/digitalmodel/drilling_riser/metocean_inputs.py:L7-L9] [src/digitalmodel/drilling_riser/metocean_inputs.py:L24-L25] |
| `operability_atlas.py` | Code-check source path in the fingerprint list | [src/digitalmodel/drilling_riser/operability_atlas.py:L90-L98] |
| `section.py` | Downstream line-type purpose in docstring | [src/digitalmodel/drilling_riser/section.py:L3-L7] |
| `tsj_workflow.py` | Explicit absence of solver dependency in docstring | [src/digitalmodel/drilling_riser/tsj_workflow.py:L1-L5] |

### Implementation-map inventory

Every map row for the three package families is retained below, including tests, helpers, other packages and scripts; a map classification is recorded evidence, not an endorsement of physical equivalence. The package boundary excludes `global_model`, `postprocess` and `stackup_drawing` from the counted in-package rows.

| Family | In-package rows | All map rows |
| --- | --- | --- |
| `effective-tension-riser-stack-up` | 25 | 264 [docs/registry/implementation-map.yaml:L1667-L1668] |
| `orcaflex-text-model-writers` | 5 | 440 [docs/registry/implementation-map.yaml:L4235-L4236] |
| `wind-and-current-force-routes` | 2 | 131 [docs/registry/implementation-map.yaml:L13775-L13776] |

#### Effective tension and riser stack-up

| Path | Definition | Map code line | Map citation |
| --- | --- | --- | --- |
| `docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py` | `main` | 45 | [docs/registry/implementation-map.yaml:L1674-L1677] |
| `docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py` | `main` | 44 | [docs/registry/implementation-map.yaml:L1683-L1686] |
| `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` | `main` | 47 | [docs/registry/implementation-map.yaml:L1692-L1695] |
| `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` | `main` | 48 | [docs/registry/implementation-map.yaml:L1701-L1704] |
| `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r01_catenary` | 140 | [docs/registry/implementation-map.yaml:L1710-L1713] |
| `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r02_lazy_wave` | 187 | [docs/registry/implementation-map.yaml:L1719-L1722] |
| `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r05_lay_table` | 317 | [docs/registry/implementation-map.yaml:L1728-L1731] |
| `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r06_multi_static` | 353 | [docs/registry/implementation-map.yaml:L1737-L1740] |
| `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `extract_vessel_params` | 273 | [docs/registry/implementation-map.yaml:L1746-L1749] |
| `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `vessel_band_specs` | 293 | [docs/registry/implementation-map.yaml:L1755-L1758] |
| `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `find_required_tension` | 563 | [docs/registry/implementation-map.yaml:L1766-L1769] |
| `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `_build_infeasible_result` | 777 | [docs/registry/implementation-map.yaml:L1777-L1780] |
| `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `run_single_case` | 826 | [docs/registry/implementation-map.yaml:L1786-L1789] |
| `examples/demos/gtm/results_store_demo04.py` | `<module>` | 52 | [docs/registry/implementation-map.yaml:L1798-L1801] |
| `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_good_case` | 79 | [docs/registry/implementation-map.yaml:L1807-L1810] |
| `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_infeasible_case` | 127 | [docs/registry/implementation-map.yaml:L1816-L1819] |
| `examples/demos/gtm/tests/test_demo_04_results_store.py` | `test_999_sentinel_and_none_null_round_trip` | 342 | [docs/registry/implementation-map.yaml:L1825-L1828] |
| `examples/domains/workflows/04_pipeline_installation.py` | `analyze_installation_catenary` | 51 | [docs/registry/implementation-map.yaml:L1837-L1840] |
| `examples/workflows/riser-stackup-registry-batch/run.py` | `_schedule_row` | 115 | [docs/registry/implementation-map.yaml:L1846-L1849] |
| `examples/workflows/riser-stackup-registry-batch/run.py` | `_assemble_row` | 219 | [docs/registry/implementation-map.yaml:L1855-L1858] |
| `scripts/capabilities/build_wall_thickness_3d.py` | `<module>` | 36 | [docs/registry/implementation-map.yaml:L1866-L1869] |
| `scripts/capabilities/build_wall_thickness_3d.py` | `_analyze_point` | 171 | [docs/registry/implementation-map.yaml:L1875-L1878] |
| `scripts/capabilities/build_wall_thickness_3d.py` | `build_study` | 222 | [docs/registry/implementation-map.yaml:L1884-L1887] |
| `scripts/capabilities/build_wall_thickness_3d_page.py` | `_axes` | 34 | [docs/registry/implementation-map.yaml:L1895-L1898] |
| `scripts/capabilities/build_wall_thickness_3d_page.py` | `_pack_rows` | 75 | [docs/registry/implementation-map.yaml:L1904-L1907] |
| `scripts/capabilities/build_wall_thickness_explorer.py` | `_run` | 48 | [docs/registry/implementation-map.yaml:L1913-L1916] |
| `scripts/orcaflex/check_generic_scr_slwr.py` | `metrics` | 37 | [docs/registry/implementation-map.yaml:L1922-L1925] |
| `scripts/orcaflex/check_generic_scr_slwr.py` | `calculate` | 148 | [docs/registry/implementation-map.yaml:L1931-L1934] |
| `scripts/orcaflex/prepare_slay_campaign.py` | `catenary` | 27 | [docs/registry/implementation-map.yaml:L1940-L1943] |
| `scripts/orcaflex/prepare_slay_campaign.py` | `case_spec` | 69 | [docs/registry/implementation-map.yaml:L1949-L1952] |
| `scripts/orcaflex/prepare_slay_campaign.py` | `prepare_case` | 90 | [docs/registry/implementation-map.yaml:L1958-L1961] |
| `scripts/orcaflex/prepare_slay_campaign.py` | `prepare` | 116 | [docs/registry/implementation-map.yaml:L1970-L1973] |
| `scripts/parametric/build_orcaflex_library.py` | `main` | 39 | [docs/registry/implementation-map.yaml:L1983-L1986] |
| `scripts/python/digitalmodel/analysis/APIST2RD_Methods.py` | `APISTD2RDCalculation` | 6 | [docs/registry/implementation-map.yaml:L1992-L1995] |
| `scripts/python/digitalmodel/analysis/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | 205 | [docs/registry/implementation-map.yaml:L2001-L2004] |
| `scripts/python/digitalmodel/analysis/RiserStackup.py` | `StackUpDiagram` | 26 | [docs/registry/implementation-map.yaml:L2012-L2015] |
| `scripts/python/digitalmodel/analysis/StackUp_3000Ft.py` | `main` | 10 | [docs/registry/implementation-map.yaml:L2021-L2024] |
| `scripts/python/digitalmodel/analysis/StackUp_9500Ft.py` | `main` | 10 | [docs/registry/implementation-map.yaml:L2030-L2033] |
| `scripts/reporting/build_riser_pipe_comparison.py` | `evaluate` | 50 | [docs/registry/implementation-map.yaml:L2039-L2042] |
| `scripts/reporting/build_riser_pipe_comparison.py` | `_validate` | 84 | [docs/registry/implementation-map.yaml:L2050-L2053] |
| `scripts/reporting/build_riser_pipe_comparison.py` | `numeric_values` | 151 | [docs/registry/implementation-map.yaml:L2059-L2062] |
| `scripts/reporting/build_riser_pipe_comparison.py` | `_input_table` | 210 | [docs/registry/implementation-map.yaml:L2068-L2071] |
| `scripts/riser_database/build_tables.py` | `<module>` | 24 | [docs/registry/implementation-map.yaml:L2077-L2080] |
| `src/digitalmodel/asset_integrity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | 696 | [docs/registry/implementation-map.yaml:L2086-L2089] |
| `src/digitalmodel/code_checks/dnv_os_f101.py` | `local_buckling_lcc` | 91 | [docs/registry/implementation-map.yaml:L2098-L2101] |
| `src/digitalmodel/code_checks/workflow.py` | `_run_api_rp_2rd` | 58 | [docs/registry/implementation-map.yaml:L2107-L2110] |
| `src/digitalmodel/code_checks/workflow.py` | `_run_dnv_os_f201` | 82 | [docs/registry/implementation-map.yaml:L2116-L2119] |
| `src/digitalmodel/drilling_riser/assembly.py` | `minimum_top_tension_16q` | 208 | [docs/registry/implementation-map.yaml:L2125-L2128] |
| `src/digitalmodel/drilling_riser/assembly.py` | `_unit_count` | 254 | [docs/registry/implementation-map.yaml:L2134-L2137] |
| `src/digitalmodel/drilling_riser/assembly.py` | `check_rig_capability` | 266 | [docs/registry/implementation-map.yaml:L2143-L2146] |
| `src/digitalmodel/drilling_riser/campaign.py` | `case_spec` | 134 | [docs/registry/implementation-map.yaml:L2154-L2157] |
| `src/digitalmodel/drilling_riser/campaign.py` | `_ring_tensioners` | 358 | [docs/registry/implementation-map.yaml:L2165-L2168] |
| `src/digitalmodel/drilling_riser/campaign.py` | `set_line_tension` | 362 | [docs/registry/implementation-map.yaml:L2174-L2177] |
| `src/digitalmodel/drilling_riser/campaign.py` | `calibrate_line_tension` | 371 | [docs/registry/implementation-map.yaml:L2185-L2188] |
| `src/digitalmodel/drilling_riser/campaign.py` | `_statics_path` | 400 | [docs/registry/implementation-map.yaml:L2198-L2201] |
| `src/digitalmodel/drilling_riser/campaign.py` | `robust_statics` | 568 | [docs/registry/implementation-map.yaml:L2208-L2211] |
| `src/digitalmodel/drilling_riser/campaign.py` | `_tensioner_vertical_n` | 734 | [docs/registry/implementation-map.yaml:L2221-L2224] |
| `src/digitalmodel/drilling_riser/campaign.py` | `tensioner_vertical_target_n` | 752 | [docs/registry/implementation-map.yaml:L2230-L2233] |
| `src/digitalmodel/drilling_riser/campaign.py` | `physical_state_checks` | 758 | [docs/registry/implementation-map.yaml:L2239-L2242] |
| `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.prepare` | 855 | [docs/registry/implementation-map.yaml:L2252-L2255] |
| `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.extract` | 912 | [docs/registry/implementation-map.yaml:L2264-L2267] |
| `src/digitalmodel/drilling_riser/envelope.py` | `compute_operating_envelope` | 278 | [docs/registry/implementation-map.yaml:L2273-L2276] |
| `src/digitalmodel/drilling_riser/global_model/build.py` | `equivalent_string_top_tension_n` | 35 | [docs/registry/implementation-map.yaml:L2282-L2285] |
| `src/digitalmodel/drilling_riser/global_model/build.py` | `_tensioner_geometry` | 51 | [docs/registry/implementation-map.yaml:L2291-L2294] |
| `src/digitalmodel/drilling_riser/global_model/build.py` | `winch_tension_n` | 58 | [docs/registry/implementation-map.yaml:L2300-L2303] |
| `src/digitalmodel/drilling_riser/global_model/build.py` | `build_generic_spec` | 180 | [docs/registry/implementation-map.yaml:L2311-L2314] |
| `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `effective_tension_chain` | 45 | [docs/registry/implementation-map.yaml:L2323-L2326] |
| `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tension_references` | 64 | [docs/registry/implementation-map.yaml:L2332-L2335] |
| `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tensioner_lateral_stiffness_n_m` | 164 | [docs/registry/implementation-map.yaml:L2343-L2346] |
| `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `reference_periods` | 216 | [docs/registry/implementation-map.yaml:L2352-L2355] |
| `src/digitalmodel/drilling_riser/global_model/hang_off.py` | `hung_weight_n` | 48 | [docs/registry/implementation-map.yaml:L2363-L2366] |
| `src/digitalmodel/drilling_riser/global_model/open_water.py` | `OpenWaterRiserSpec._closes` | 114 | [docs/registry/implementation-map.yaml:L2373-L2376] |
| `src/digitalmodel/drilling_riser/global_model/open_water.py` | `build_open_water_generic_spec` | 143 | [docs/registry/implementation-map.yaml:L2382-L2385] |
| `src/digitalmodel/drilling_riser/global_model/open_water.py` | `tension_references` | 218 | [docs/registry/implementation-map.yaml:L2391-L2394] |
| `src/digitalmodel/drilling_riser/global_model/open_water.py` | `reference_periods` | 272 | [docs/registry/implementation-map.yaml:L2401-L2404] |
| `src/digitalmodel/drilling_riser/global_model/open_water.py` | `release_reference` | 293 | [docs/registry/implementation-map.yaml:L2410-L2413] |
| `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `ring_vertical_residual_n` | 181 | [docs/registry/implementation-map.yaml:L2421-L2424] |
| `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `open_water_physical_checks` | 309 | [docs/registry/implementation-map.yaml:L2430-L2433] |
| `src/digitalmodel/drilling_riser/global_model/spec.py` | `Tensioners._failed` | 146 | [docs/registry/implementation-map.yaml:L2439-L2442] |
| `src/digitalmodel/drilling_riser/global_model/spec.py` | `RiserGlobalModelSpec._geometry_closes` | 405 | [docs/registry/implementation-map.yaml:L2448-L2451] |
| `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `_missing_open_water` | 140 | [docs/registry/implementation-map.yaml:L2457-L2460] |
| `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `missing_channels` | 188 | [docs/registry/implementation-map.yaml:L2466-L2469] |
| `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract` | 317 | [docs/registry/implementation-map.yaml:L2477-L2480] |
| `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract_open_water` | 409 | [docs/registry/implementation-map.yaml:L2486-L2489] |
| `src/digitalmodel/drilling_riser/postprocess/checks.py` | `t_min` | 172 | [docs/registry/implementation-map.yaml:L2495-L2498] |
| `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tension_setting_max` | 177 | [docs/registry/implementation-map.yaml:L2504-L2507] |
| `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tensioner_stroke` | 198 | [docs/registry/implementation-map.yaml:L2513-L2516] |
| `src/digitalmodel/drilling_riser/schedule_assembly.py` | `ScheduleAssembly.minimum_top_tension_16q_kn` | 196 | [docs/registry/implementation-map.yaml:L2522-L2525] |
| `src/digitalmodel/drilling_riser/schedule_assembly.py` | `_load_oc_assembly` | 532 | [docs/registry/implementation-map.yaml:L2531-L2534] |
| `src/digitalmodel/drilling_riser/stackup.py` | `effective_tension` | 85 | [docs/registry/implementation-map.yaml:L2540-L2543] |
| `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `parse_snapshots` | 135 | [docs/registry/implementation-map.yaml:L2549-L2552] |
| `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `snapshot_to_tension_n` | 178 | [docs/registry/implementation-map.yaml:L2558-L2561] |
| `src/digitalmodel/drilling_riser/tsj_workflow.py` | `router` | 19 | [docs/registry/implementation-map.yaml:L2567-L2570] |
| `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_section_result` | 74 | [docs/registry/implementation-map.yaml:L2579-L2582] |
| `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_effective_tension_n` | 146 | [docs/registry/implementation-map.yaml:L2588-L2591] |
| `src/digitalmodel/drilling_riser/workflow.py` | `router` | 32 | [docs/registry/implementation-map.yaml:L2597-L2600] |
| `src/digitalmodel/drilling_riser/workflow.py` | `_write_profile_csv` | 154 | [docs/registry/implementation-map.yaml:L2609-L2612] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_to_shear7_result` | 251 | [docs/registry/implementation-map.yaml:L2618-L2621] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | 308 | [docs/registry/implementation-map.yaml:L2627-L2630] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | 374 | [docs/registry/implementation-map.yaml:L2636-L2639] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_fatigue_summary_result` | 493 | [docs/registry/implementation-map.yaml:L2645-L2648] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_fatigue_damage_results_to_df` | 521 | [docs/registry/implementation-map.yaml:L2654-L2657] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | 554 | [docs/registry/implementation-map.yaml:L2668-L2671] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | 564 | [docs/registry/implementation-map.yaml:L2677-L2680] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | 11 | [docs/registry/implementation-map.yaml:L2686-L2689] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | 494 | [docs/registry/implementation-map.yaml:L2698-L2701] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | 530 | [docs/registry/implementation-map.yaml:L2709-L2712] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | 535 | [docs/registry/implementation-map.yaml:L2723-L2726] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | 557 | [docs/registry/implementation-map.yaml:L2737-L2740] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | 587 | [docs/registry/implementation-map.yaml:L2749-L2752] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | 619 | [docs/registry/implementation-map.yaml:L2760-L2763] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | 677 | [docs/registry/implementation-map.yaml:L2769-L2772] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | 691 | [docs/registry/implementation-map.yaml:L2780-L2783] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | 704 | [docs/registry/implementation-map.yaml:L2791-L2794] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.prepare_stack_up_properties_df` | 8 | [docs/registry/implementation-map.yaml:L2800-L2803] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_from_endA_in_df` | 56 | [docs/registry/implementation-map.yaml:L2815-L2818] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_shear7_x_by_L_in_df` | 95 | [docs/registry/implementation-map.yaml:L2824-L2827] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_mudline_in_df` | 107 | [docs/registry/implementation-map.yaml:L2833-L2836] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_MSL_in_df` | 164 | [docs/registry/implementation-map.yaml:L2842-L2845] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_inservice_weights_and_tensions` | 173 | [docs/registry/implementation-map.yaml:L2851-L2854] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.assign_key_properties_to_df` | 179 | [docs/registry/implementation-map.yaml:L2865-L2868] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stack_in_service_weights` | 344 | [docs/registry/implementation-map.yaml:L2874-L2877] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_effective_tension` | 415 | [docs/registry/implementation-map.yaml:L2883-L2886] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stretch_in_riser` | 529 | [docs/registry/implementation-map.yaml:L2892-L2895] |
| `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.title` | 21 | [docs/registry/implementation-map.yaml:L2901-L2904] |
| `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.riser_joints_to_shear7_zones` | 58 | [docs/registry/implementation-map.yaml:L2910-L2913] |
| `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.add_block_3` | 152 | [docs/registry/implementation-map.yaml:L2919-L2922] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_to_shear7_result` | 454 | [docs/registry/implementation-map.yaml:L2928-L2931] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | 533 | [docs/registry/implementation-map.yaml:L2937-L2940] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | 631 | [docs/registry/implementation-map.yaml:L2946-L2949] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | 799 | [docs/registry/implementation-map.yaml:L2955-L2958] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_fatigue_damage_results_to_df` | 866 | [docs/registry/implementation-map.yaml:L2964-L2967] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | 942 | [docs/registry/implementation-map.yaml:L2978-L2981] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | 953 | [docs/registry/implementation-map.yaml:L2987-L2990] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py` | `_write_tensions_csv` | 90 | [docs/registry/implementation-map.yaml:L2996-L2999] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._line_state` | 291 | [docs/registry/implementation-map.yaml:L3005-L3008] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._solve_line` | 323 | [docs/registry/implementation-map.yaml:L3016-L3019] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._result` | 369 | [docs/registry/implementation-map.yaml:L3025-L3028] |
| `src/digitalmodel/orcaflex/mooring_design.py` | `solve_catenary` | 251 | [docs/registry/implementation-map.yaml:L3034-L3037] |
| `src/digitalmodel/orcaflex/pipelay_analysis.py` | `JLayConfig.calculate_sagbend` | 222 | [docs/registry/implementation-map.yaml:L3043-L3046] |
| `src/digitalmodel/orcaflex/riser_config.py` | `SCRDesignInput.calculate_geometry` | 170 | [docs/registry/implementation-map.yaml:L3052-L3055] |
| `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_stroke` | 339 | [docs/registry/implementation-map.yaml:L3061-L3064] |
| `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_top_tension` | 377 | [docs/registry/implementation-map.yaml:L3070-L3073] |
| `src/digitalmodel/orcaflex/riser_input_schema.py` | `_TopTensionedInput.to_legacy` | 244 | [docs/registry/implementation-map.yaml:L3079-L3082] |
| `src/digitalmodel/orcaflex/viv_screening.py` | `BeamProperties.natural_frequency` | 185 | [docs/registry/implementation-map.yaml:L3088-L3091] |
| `src/digitalmodel/parametric/generate.py` | `_code_check_utilisation` | 65 | [docs/registry/implementation-map.yaml:L3097-L3100] |
| `src/digitalmodel/pipelay/workflow.py` | `run_jlay` | 62 | [docs/registry/implementation-map.yaml:L3106-L3109] |
| `src/digitalmodel/riser_configuration_screen/static.py` | `_lazy_row` | 92 | [docs/registry/implementation-map.yaml:L3115-L3118] |
| `src/digitalmodel/riser_configuration_screen/static.py` | `scr_offsets` | 166 | [docs/registry/implementation-map.yaml:L3124-L3127] |
| `src/digitalmodel/riser_configuration_screen/workflow.py` | `_validate` | 28 | [docs/registry/implementation-map.yaml:L3133-L3136] |
| `src/digitalmodel/riser_configuration_screen/workflow.py` | `_utilisation` | 63 | [docs/registry/implementation-map.yaml:L3142-L3145] |
| `src/digitalmodel/riser_configuration_screen/workflow.py` | `_checks` | 97 | [docs/registry/implementation-map.yaml:L3151-L3154] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/groups_builder.py` | `GroupsBuilder._build_from_spec` | 93 | [docs/registry/implementation-map.yaml:L3160-L3163] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `LineTypeBuilder.build` | 85 | [docs/registry/implementation-map.yaml:L3169-L3172] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.should_generate` | 33 | [docs/registry/implementation-map.yaml:L3178-L3181] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.build` | 40 | [docs/registry/implementation-map.yaml:L3187-L3190] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `_apply_overrides` | 411 | [docs/registry/implementation-map.yaml:L3196-L3199] |
| `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.mooring_pre_tension_analysis` | 127 | [docs/registry/implementation-map.yaml:L3205-L3208] |
| `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.evaluate_line_length_next_iteration` | 289 | [docs/registry/implementation-map.yaml:L3216-L3219] |
| `src/digitalmodel/solvers/orcaflex/opp_range_graph.py` | `OPPRangeGraph.postProcessRange` | 34 | [docs/registry/implementation-map.yaml:L3225-L3228] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.postProcessRange` | 401 | [docs/registry/implementation-map.yaml:L3234-L3237] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | 14 | [docs/registry/implementation-map.yaml:L3243-L3246] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | 673 | [docs/registry/implementation-map.yaml:L3255-L3258] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | 743 | [docs/registry/implementation-map.yaml:L3266-L3269] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | 748 | [docs/registry/implementation-map.yaml:L3280-L3283] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | 777 | [docs/registry/implementation-map.yaml:L3294-L3297] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | 837 | [docs/registry/implementation-map.yaml:L3306-L3309] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | 917 | [docs/registry/implementation-map.yaml:L3317-L3320] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | 1048 | [docs/registry/implementation-map.yaml:L3326-L3329] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | 1071 | [docs/registry/implementation-map.yaml:L3337-L3340] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | 1110 | [docs/registry/implementation-map.yaml:L3348-L3351] |
| `src/digitalmodel/structural/analysis/cli.py` | `check` | 83 | [docs/registry/implementation-map.yaml:L3357-L3360] |
| `src/digitalmodel/structural/analysis/wall_thickness.py` | `WallThicknessAnalyzer.check_combined_loading` | 435 | [docs/registry/implementation-map.yaml:L3366-L3369] |
| `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py` | `ApiStd2rdStrategy._check_combined` | 161 | [docs/registry/implementation-map.yaml:L3375-L3378] |
| `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py` | `DnvStF101Strategy._check_combined_loading` | 268 | [docs/registry/implementation-map.yaml:L3384-L3387] |
| `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py` | `run_wall_thickness_compare` | 41 | [docs/registry/implementation-map.yaml:L3393-L3396] |
| `src/digitalmodel/structural/analysis/wall_thickness_lookup.py` | `MTLookupGenerator.generate` | 60 | [docs/registry/implementation-map.yaml:L3402-L3405] |
| `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `ParametricSweep.run` | 66 | [docs/registry/implementation-map.yaml:L3411-L3414] |
| `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `PipelinePhase.to_design_loads` | 141 | [docs/registry/implementation-map.yaml:L3420-L3423] |
| `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `create_standard_phases` | 156 | [docs/registry/implementation-map.yaml:L3429-L3432] |
| `src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py` | `LoadScalingProcessor.process_all_conditions` | 186 | [docs/registry/implementation-map.yaml:L3438-L3441] |
| `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | 290 | [docs/registry/implementation-map.yaml:L3447-L3450] |
| `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | 81 | [docs/registry/implementation-map.yaml:L3456-L3459] |
| `src/digitalmodel/structural/fatigue_apps/load_scaling.py` | `LoadScalingProcessor.load_reference_data` | 101 | [docs/registry/implementation-map.yaml:L3465-L3468] |
| `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | 355 | [docs/registry/implementation-map.yaml:L3474-L3477] |
| `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_fatigue_condition` | 458 | [docs/registry/implementation-map.yaml:L3483-L3486] |
| `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_sea_state` | 506 | [docs/registry/implementation-map.yaml:L3492-L3495] |
| `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `main` | 566 | [docs/registry/implementation-map.yaml:L3501-L3504] |
| `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py` | `ProductionDataHandlerWithProgress.process_single_analysis` | 83 | [docs/registry/implementation-map.yaml:L3510-L3513] |
| `src/digitalmodel/structural/parametric_coordinator.py` | `ParametricCoordinator._run_wall_thickness` | 255 | [docs/registry/implementation-map.yaml:L3519-L3522] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | 985 | [docs/registry/implementation-map.yaml:L3528-L3531] |
| `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | 342 | [docs/registry/implementation-map.yaml:L3540-L3543] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | 1272 | [docs/registry/implementation-map.yaml:L3551-L3554] |
| `src/digitalmodel/subsea/catenary_riser/cli.py` | `simple_catenary` | 43 | [docs/registry/implementation-map.yaml:L3563-L3566] |
| `src/digitalmodel/subsea/catenary_riser/cli.py` | `lazy_wave_analysis` | 182 | [docs/registry/implementation-map.yaml:L3572-L3575] |
| `src/digitalmodel/subsea/catenary_riser/lazy_wave.py` | `LazyWaveAnalyzer.analyze` | 34 | [docs/registry/implementation-map.yaml:L3581-L3584] |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.analyze_riser` | 42 | [docs/registry/implementation-map.yaml:L3590-L3593] |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_catenary_from_H` | 103 | [docs/registry/implementation-map.yaml:L3602-L3605] |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_H_from_top_tension` | 174 | [docs/registry/implementation-map.yaml:L3611-L3614] |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.check_design_limits` | 396 | [docs/registry/implementation-map.yaml:L3620-L3623] |
| `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `developed_min_radius` | 64 | [docs/registry/implementation-map.yaml:L3629-L3632] |
| `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `required_effective_ei` | 91 | [docs/registry/implementation-map.yaml:L3638-L3641] |
| `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `screen_bend_stiffener` | 109 | [docs/registry/implementation-map.yaml:L3647-L3650] |
| `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `armour_tension_stress` | 160 | [docs/registry/implementation-map.yaml:L3659-L3662] |
| `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `verify_flexible_pipe` | 178 | [docs/registry/implementation-map.yaml:L3668-L3671] |
| `src/digitalmodel/subsea/pipeline/global_buckling/hobbs_lateral.py` | `effective_driving_force` | 294 | [docs/registry/implementation-map.yaml:L3679-L3682] |
| `src/digitalmodel/subsea/riser/riser_case_screening.py` | `RiserLoadCase.to_loads` | 44 | [docs/registry/implementation-map.yaml:L3688-L3691] |
| `src/digitalmodel/subsea/riser/riser_case_screening.py` | `run_riser_screening` | 125 | [docs/registry/implementation-map.yaml:L3697-L3700] |
| `src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py` | `StackUpDiagram` | 27 | [docs/registry/implementation-map.yaml:L3706-L3709] |
| `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_riser_model` | 28 | [docs/registry/implementation-map.yaml:L3715-L3718] |
| `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.read_riser_input_data` | 41 | [docs/registry/implementation-map.yaml:L3729-L3732] |
| `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.assign_key_properties_to_self` | 50 | [docs/registry/implementation-map.yaml:L3738-L3741] |
| `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.get_fea_properties` | 120 | [docs/registry/implementation-map.yaml:L3747-L3750] |
| `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_tensioner_data` | 297 | [docs/registry/implementation-map.yaml:L3756-L3759] |
| `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.evaluate_stack_up_properties` | 325 | [docs/registry/implementation-map.yaml:L3765-L3768] |
| `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.save_data` | 374 | [docs/registry/implementation-map.yaml:L3774-L3777] |
| `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.alter_properties_for_Shear7_software` | 403 | [docs/registry/implementation-map.yaml:L3783-L3786] |
| `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_natural_frequency` | 59 | [docs/registry/implementation-map.yaml:L3792-L3795] |
| `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator._frequency_tension_controlled` | 164 | [docs/registry/implementation-map.yaml:L3803-L3806] |
| `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_effective_tension` | 248 | [docs/registry/implementation-map.yaml:L3812-L3815] |
| `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.check_stability` | 288 | [docs/registry/implementation-map.yaml:L3821-L3824] |
| `src/digitalmodel/viv_screening/inputs.py` | `_beam` | 63 | [docs/registry/implementation-map.yaml:L3832-L3835] |
| `src/digitalmodel/workflows/workflow_automation/cli.py` | `riser_analysis_cmd` | 84 | [docs/registry/implementation-map.yaml:L3841-L3844] |
| `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_riser_analysis_template` | 257 | [docs/registry/implementation-map.yaml:L3850-L3853] |
| `src/digitalmodel/workflows/workflow_automation/workflows.py` | `CompleteRiserAnalysisWorkflow.create` | 35 | [docs/registry/implementation-map.yaml:L3859-L3862] |
| `tests/asset_integrity/test_calculations.py` | `_make_pipe_cfg` | 29 | [docs/registry/implementation-map.yaml:L3868-L3871] |
| `tests/capabilities/test_wall_thickness_3d_page.py` | `_study` | 32 | [docs/registry/implementation-map.yaml:L3877-L3880] |
| `tests/contracts/test_force_units_boundary_contract.py` | `<module>` | 52 | [docs/registry/implementation-map.yaml:L3886-L3889] |
| `tests/contracts/test_force_units_boundary_contract.py` | `test_envelope_von_mises_n_to_kn_seam` | 454 | [docs/registry/implementation-map.yaml:L3895-L3898] |
| `tests/drilling_riser/global_model/conftest.py` | `synthetic_spec` | 42 | [docs/registry/implementation-map.yaml:L3906-L3909] |
| `tests/drilling_riser/global_model/test_batch_params.py` | `test_physical_checks_use_the_remaining_tensioners` | 78 | [docs/registry/implementation-map.yaml:L3915-L3918] |
| `tests/drilling_riser/global_model/test_campaign.py` | `_fake` | 65 | [docs/registry/implementation-map.yaml:L3925-L3928] |
| `tests/drilling_riser/global_model/test_open_water.py` | `open_water_dict` | 43 | [docs/registry/implementation-map.yaml:L3935-L3938] |
| `tests/drilling_riser/global_model/test_open_water_campaign.py` | `_doc` | 17 | [docs/registry/implementation-map.yaml:L3944-L3947] |
| `tests/drilling_riser/global_model/test_orcaflex_solver.py` | `test_statics_tension_and_modes_agree_with_hand_checks` | 17 | [docs/registry/implementation-map.yaml:L3953-L3956] |
| `tests/drilling_riser/global_model/test_statics_path.py` | `test_branch_tolerance_and_ring_buoyancy` | 287 | [docs/registry/implementation-map.yaml:L3966-L3969] |
| `tests/drilling_riser/global_model/test_tensioner_representation.py` | `_vertical` | 24 | [docs/registry/implementation-map.yaml:L3976-L3979] |
| `tests/drilling_riser/postprocess/test_aggregate.py` | `_mc` | 20 | [docs/registry/implementation-map.yaml:L3985-L3988] |
| `tests/drilling_riser/postprocess/test_aggregate.py` | `_case` | 44 | [docs/registry/implementation-map.yaml:L3994-L3997] |
| `tests/drilling_riser/test_drive_off_worked_example.py` | `_recorded_arguments` | 48 | [docs/registry/implementation-map.yaml:L4005-L4008] |
| `tests/drilling_riser/test_schedule_assembly.py` | `_deltas` | 57 | [docs/registry/implementation-map.yaml:L4014-L4017] |
| `tests/infrastructure/unit/test_pipe_capacity_codes.py` | `_base_cfg` | 6 | [docs/registry/implementation-map.yaml:L4023-L4026] |
| `tests/orcaflex/test_generic_scr_slwr.py` | `test_analytical_reference_reproduces_from_basis` | 192 | [docs/registry/implementation-map.yaml:L4032-L4035] |
| `tests/parametric/test_atlas.py` | `_code_check_atlas` | 84 | [docs/registry/implementation-map.yaml:L4041-L4044] |
| `tests/reporting/test_riser_pipe_comparison.py` | `reference_input` | 24 | [docs/registry/implementation-map.yaml:L4050-L4053] |
| `tests/riser_configuration_screen/test_workflow.py` | `settings` | 17 | [docs/registry/implementation-map.yaml:L4059-L4062] |
| `tests/solvers/orcaflex/analysis/scripts/verify_configs.py` | `verify_configuration` | 13 | [docs/registry/implementation-map.yaml:L4068-L4071] |
| `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py` | `_make_slay_spec` | 39 | [docs/registry/implementation-map.yaml:L4077-L4080] |
| `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` | `_make_pipeline_spec` | 22 | [docs/registry/implementation-map.yaml:L4086-L4089] |
| `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` | `_make_pipeline_spec` | 21 | [docs/registry/implementation-map.yaml:L4095-L4098] |
| `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_slay_campaign` | 126 | [docs/registry/implementation-map.yaml:L4104-L4107] |
| `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py` | `_make_base_spec_data` | 8 | [docs/registry/implementation-map.yaml:L4113-L4116] |
| `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` | `extract_static_range_results` | 165 | [docs/registry/implementation-map.yaml:L4122-L4125] |
| `tests/solvers/orcaflex/modular_generator/test_post_validator.py` | `_make_valid_model` | 26 | [docs/registry/implementation-map.yaml:L4131-L4134] |
| `tests/solvers/orcaflex/test_slay_layout.py` | `test_support_types_are_defined_before_vessel_references` | 90 | [docs/registry/implementation-map.yaml:L4140-L4143] |
| `tests/specialized/umbilical_analysis/conftest.py` | `installation_parameters` | 24 | [docs/registry/implementation-map.yaml:L4149-L4152] |
| `tests/subsea/catenary_riser/test_catenary.py` | `run_catenary_riser` | 13 | [docs/registry/implementation-map.yaml:L4158-L4161] |
| `tests/test_automation/fixtures/engineering_fixtures.py` | `get_catenary_analysis_mock` | 97 | [docs/registry/implementation-map.yaml:L4167-L4170] |
| `tests/test_automation/scripts/batch_fix_tests.py` | `<module>` | 12 | [docs/registry/implementation-map.yaml:L4176-L4179] |
| `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | 25 | [docs/registry/implementation-map.yaml:L4185-L4188] |
| `tests/test_rainflow_with_effective_tension.py` | `main` | 117 | [docs/registry/implementation-map.yaml:L4194-L4197] |
| `tests/test_wall_thickness_compare_workflow.py` | `_cfg` | 22 | [docs/registry/implementation-map.yaml:L4205-L4208] |
| `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | 52 | [docs/registry/implementation-map.yaml:L4214-L4217] |
| `tests/workflows/test_viv_screening_workflow.py` | `screening_input` | 18 | [docs/registry/implementation-map.yaml:L4223-L4226] |

#### OrcaFlex text-model writers

| Path | Definition | Map code line | Map citation |
| --- | --- | --- | --- |
| `docs/domains/examples/orcaflex/run_universal_example.py` | `main` | 218 | [docs/registry/implementation-map.yaml:L4242-L4245] |
| `docs/domains/orcaflex/examples/gom_scr_design_study/generate_configs.py` | `<module>` | 10 | [docs/registry/implementation-map.yaml:L4251-L4254] |
| `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` | `main` | 47 | [docs/registry/implementation-map.yaml:L4260-L4263] |
| `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` | `main` | 48 | [docs/registry/implementation-map.yaml:L4269-L4272] |
| `docs/domains/orcaflex/risers/drilling/wd9500ft/Preprocessing code.py` | `strengthInputFilePreperation` | 2 | [docs/registry/implementation-map.yaml:L4278-L4281] |
| `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction.py` | `run_orcawave_diffraction` | 29 | [docs/registry/implementation-map.yaml:L4287-L4290] |
| `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction_improved.py` | `run_diffraction_with_monitoring` | 166 | [docs/registry/implementation-map.yaml:L4296-L4299] |
| `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction_improved.py` | `run_batch_execution` | 276 | [docs/registry/implementation-map.yaml:L4305-L4308] |
| `docs/domains/orcawave/examples/L02 OC4 Semi-sub/run_orcawave.py` | `_load_data_compat` | 37 | [docs/registry/implementation-map.yaml:L4316-L4319] |
| `docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/run_orcawave.py` | `_load_data_compat` | 39 | [docs/registry/implementation-map.yaml:L4329-L4332] |
| `docs/domains/orcawave/examples/L04 Sectional bodies/run_orcawave.py` | `_load_data_compat` | 40 | [docs/registry/implementation-map.yaml:L4342-L4345] |
| `docs/domains/orcawave/examples/L05 Panel pressures/run_orcawave.py` | `_load_data_compat` | 40 | [docs/registry/implementation-map.yaml:L4355-L4358] |
| `examples/domains/workflows/utils.py` | `OrcaFlexExporter.create_mooring_yml` | 256 | [docs/registry/implementation-map.yaml:L4368-L4371] |
| `scripts/audit_spec_library.py` | `write_audit_yaml` | 241 | [docs/registry/implementation-map.yaml:L4377-L4380] |
| `scripts/benchmark/generate_owd_ymls.py` | `generate_yml` | 48 | [docs/registry/implementation-map.yaml:L4386-L4389] |
| `scripts/benchmark/run_spar_benchmark.py` | `main` | 155 | [docs/registry/implementation-map.yaml:L4395-L4398] |
| `scripts/benchmark/solve_owd_extract.py` | `solve_and_extract` | 116 | [docs/registry/implementation-map.yaml:L4404-L4407] |
| `scripts/benchmark/validate_owd_vs_spec.py` | `solve_owd` | 426 | [docs/registry/implementation-map.yaml:L4413-L4416] |
| `scripts/benchmark/validate_owd_vs_spec.py` | `solve_spec` | 546 | [docs/registry/implementation-map.yaml:L4422-L4425] |
| `scripts/benchmark/validate_owd_vs_spec.py` | `_solve_spec_via_orcfxapi` | 660 | [docs/registry/implementation-map.yaml:L4433-L4436] |
| `scripts/benchmark_model_library.py` | `<module>` | 26 | [docs/registry/implementation-map.yaml:L4442-L4445] |
| `scripts/benchmark_model_library.py` | `load_and_run` | 330 | [docs/registry/implementation-map.yaml:L4451-L4454] |
| `scripts/benchmark_model_library.py` | `run_spec_driven` | 346 | [docs/registry/implementation-map.yaml:L4460-L4463] |
| `scripts/benchmark_model_library.py` | `load_json` | 551 | [docs/registry/implementation-map.yaml:L4469-L4472] |
| `scripts/benchmark_model_library.py` | `_trace` | 645 | [docs/registry/implementation-map.yaml:L4478-L4481] |
| `scripts/benchmark_riser_library.py` | `apply_mesh_preset` | 358 | [docs/registry/implementation-map.yaml:L4487-L4490] |
| `scripts/benchmark_riser_library.py` | `run_mesh_sensitivity` | 430 | [docs/registry/implementation-map.yaml:L4496-L4499] |
| `scripts/benchmark_riser_library.py` | `benchmark_model` | 524 | [docs/registry/implementation-map.yaml:L4505-L4508] |
| `scripts/build_sme_report.py` | `make_chart_js` | 70 | [docs/registry/implementation-map.yaml:L4516-L4519] |
| `scripts/build_sme_report.py` | `make_profile_js` | 105 | [docs/registry/implementation-map.yaml:L4525-L4528] |
| `scripts/ci/check_include_targets.py` | `extract_references` | 36 | [docs/registry/implementation-map.yaml:L4534-L4537] |
| `scripts/consolidate_panels.py` | `_build_manual_entries` | 182 | [docs/registry/implementation-map.yaml:L4543-L4546] |
| `scripts/convert_orcaflex_to_yml.py` | `OrcaFlexConverter.convert_file` | 122 | [docs/registry/implementation-map.yaml:L4552-L4555] |
| `scripts/create_flattened_models.py` | `create_flattened_model_dir` | 19 | [docs/registry/implementation-map.yaml:L4561-L4564] |
| `scripts/extract_library_validation.py` | `process_model_dir` | 64 | [docs/registry/implementation-map.yaml:L4570-L4573] |
| `scripts/extract_property_inventory.py` | `_field_to_section_name` | 145 | [docs/registry/implementation-map.yaml:L4579-L4582] |
| `scripts/extract_property_inventory.py` | `write_yaml` | 273 | [docs/registry/implementation-map.yaml:L4588-L4591] |
| `scripts/extract_riser_validation.py` | `main` | 56 | [docs/registry/implementation-map.yaml:L4597-L4600] |
| `scripts/extract_s7_specs.py` | `save_spec` | 164 | [docs/registry/implementation-map.yaml:L4606-L4609] |
| `scripts/fix_orcaflex_structure.py` | `create_env_section_wrappers` | 20 | [docs/registry/implementation-map.yaml:L4615-L4618] |
| `scripts/fix_orcaflex_structure.py` | `create_fixed_analysis_model` | 39 | [docs/registry/implementation-map.yaml:L4624-L4627] |
| `scripts/fix_orcaflex_structure.py` | `fix_sections_for_discretised` | 90 | [docs/registry/implementation-map.yaml:L4633-L4636] |
| `scripts/generate_all_specs.py` | `<module>` | 39 | [docs/registry/implementation-map.yaml:L4642-L4645] |
| `scripts/generate_all_specs.py` | `extract_spec` | 230 | [docs/registry/implementation-map.yaml:L4651-L4654] |
| `scripts/generate_all_specs.py` | `save_spec_yml` | 265 | [docs/registry/implementation-map.yaml:L4660-L4663] |
| `scripts/generate_analysis_files.py` | `create_analysis_file` | 21 | [docs/registry/implementation-map.yaml:L4669-L4672] |
| `scripts/generate_calm_buoy_project.py` | `CALMBuoyProjectGenerator.generate_orcaflex_modules` | 310 | [docs/registry/implementation-map.yaml:L4678-L4681] |
| `scripts/generate_calm_buoy_project.py` | `CALMBuoyProjectGenerator._populate_environment_module` | 388 | [docs/registry/implementation-map.yaml:L4690-L4693] |
| `scripts/generate_calm_buoy_project.py` | `CALMBuoyProjectGenerator._apply_module_customizations` | 464 | [docs/registry/implementation-map.yaml:L4699-L4702] |
| `scripts/generate_calm_buoy_project.py` | `CALMBuoyProjectGenerator._update_base_file_paths` | 691 | [docs/registry/implementation-map.yaml:L4710-L4713] |
| `scripts/generate_calm_buoy_project.py` | `CALMBuoyProjectGenerator.save_updated_config` | 766 | [docs/registry/implementation-map.yaml:L4719-L4722] |
| `scripts/generate_env_files.py` | `create_wave_file` | 38 | [docs/registry/implementation-map.yaml:L4728-L4731] |
| `scripts/generate_env_files.py` | `create_current_file` | 79 | [docs/registry/implementation-map.yaml:L4737-L4740] |
| `scripts/generate_env_files.py` | `create_wind_file` | 108 | [docs/registry/implementation-map.yaml:L4746-L4749] |
| `scripts/initialize_skill_metadata.py` | `create_version_metadata_block` | 78 | [docs/registry/implementation-map.yaml:L4755-L4758] |
| `scripts/inventory/orcaflex_legacy_inventory.py` | `_report_type` | 28 | [docs/registry/implementation-map.yaml:L4764-L4767] |
| `scripts/inventory/orcaflex_legacy_inventory.py` | `classify` | 118 | [docs/registry/implementation-map.yaml:L4773-L4776] |
| `scripts/inventory/orcaflex_legacy_inventory.py` | `main` | 170 | [docs/registry/implementation-map.yaml:L4784-L4787] |
| `scripts/maintenance/archive_candidates.py` | `<module>` | 32 | [docs/registry/implementation-map.yaml:L4795-L4798] |
| `scripts/maintenance/archive_candidates.py` | `main` | 663 | [docs/registry/implementation-map.yaml:L4804-L4807] |
| `scripts/mesh_sensitivity_riser.py` | `apply_mesh_preset` | 101 | [docs/registry/implementation-map.yaml:L4813-L4816] |
| `scripts/orcaflex/check_generic_scr_slwr.py` | `scr_reference` | 77 | [docs/registry/implementation-map.yaml:L4822-L4825] |
| `scripts/orcaflex/check_generic_scr_slwr.py` | `slwr_reference` | 103 | [docs/registry/implementation-map.yaml:L4831-L4834] |
| `scripts/orcaflex/check_generic_scr_slwr.py` | `calculate` | 148 | [docs/registry/implementation-map.yaml:L4840-L4843] |
| `scripts/orcaflex/prepare_slay_campaign.py` | `case_spec` | 69 | [docs/registry/implementation-map.yaml:L4852-L4855] |
| `scripts/orcaflex/prepare_slay_campaign.py` | `prepare_case` | 90 | [docs/registry/implementation-map.yaml:L4861-L4864] |
| `scripts/orcaflex/prepare_slay_campaign.py` | `prepare` | 116 | [docs/registry/implementation-map.yaml:L4872-L4875] |
| `scripts/prepare_mooring_qualification.py` | `_generated_sections` | 37 | [docs/registry/implementation-map.yaml:L4884-L4887] |
| `scripts/python/digitalmodel/doc_tools/check_structure_creation.py` | `ContentAnalyzer._categorize_folder` | 309 | [docs/registry/implementation-map.yaml:L4893-L4896] |
| `scripts/python/digitalmodel/doc_tools/frontmatter_generator.py` | `FrontmatterGenerator.frontmatter_to_yaml` | 380 | [docs/registry/implementation-map.yaml:L4902-L4905] |
| `scripts/python/digitalmodel/legacy/51_-_SerializationYAMLPart1.py` | `<module>` | 4 | [docs/registry/implementation-map.yaml:L4911-L4914] |
| `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `make_comparison_loaded_vs_ballast_cxc` | 315 | [docs/registry/implementation-map.yaml:L4920-L4923] |
| `scripts/python/digitalmodel/orcawave/analyze_tug_30m_1.py` | `load_config` | 12 | [docs/registry/implementation-map.yaml:L4929-L4932] |
| `scripts/python/digitalmodel/orcawave/analyze_tug_30m_1.py` | `generate_optimized_config` | 199 | [docs/registry/implementation-map.yaml:L4938-L4941] |
| `scripts/python/digitalmodel/orcawave/create_gdf_consistent.py` | `create_config_with_gdf` | 345 | [docs/registry/implementation-map.yaml:L4947-L4950] |
| `scripts/python/digitalmodel/orcawave/create_working_examples.py` | `create_simple_vessel_config` | 15 | [docs/registry/implementation-map.yaml:L4956-L4959] |
| `scripts/python/digitalmodel/orcawave/create_working_examples.py` | `create_oc4_semisub_config` | 144 | [docs/registry/implementation-map.yaml:L4965-L4968] |
| `scripts/python/digitalmodel/orcawave/create_working_examples.py` | `create_minimal_test_config` | 272 | [docs/registry/implementation-map.yaml:L4974-L4977] |
| `scripts/python/digitalmodel/orcawave/final_working_config.py` | `create_final_working_config` | 18 | [docs/registry/implementation-map.yaml:L4983-L4986] |
| `scripts/python/digitalmodel/orcawave/final_working_config.py` | `create_extended_version` | 167 | [docs/registry/implementation-map.yaml:L4994-L4997] |
| `scripts/python/digitalmodel/orcawave/fix_orcawave_yaml.py` | `create_tug_30m_1_working` | 60 | [docs/registry/implementation-map.yaml:L5003-L5006] |
| `scripts/python/digitalmodel/orcawave/fix_tug_30m_1_errors.py` | `fix_configuration` | 14 | [docs/registry/implementation-map.yaml:L5012-L5015] |
| `scripts/python/digitalmodel/orcawave/fix_tug_30m_1_errors.py` | `create_minimal_test_config` | 137 | [docs/registry/implementation-map.yaml:L5023-L5026] |
| `scripts/python/digitalmodel/orcawave/fix_working_model.py` | `fix_working_model` | 11 | [docs/registry/implementation-map.yaml:L5032-L5035] |
| `scripts/python/digitalmodel/orcawave/fix_working_model.py` | `create_minimal_with_test_box` | 143 | [docs/registry/implementation-map.yaml:L5041-L5044] |
| `scripts/python/digitalmodel/orcawave/validate_orcawave_configs.py` | `OrcaWaveValidator.run_validation` | 189 | [docs/registry/implementation-map.yaml:L5050-L5053] |
| `scripts/python/digitalmodel/tools/add_orcaflex_test_configs.py` | `create_batch_config` | 14 | [docs/registry/implementation-map.yaml:L5061-L5064] |
| `scripts/python/digitalmodel/tools/standardize_orcaflex_tests.py` | `OrcaFlexTestStandardizer.create_standard_config` | 203 | [docs/registry/implementation-map.yaml:L5070-L5073] |
| `scripts/run_riser_analysis.py` | `run_library` | 366 | [docs/registry/implementation-map.yaml:L5079-L5082] |
| `scripts/run_semisub_quality_check.py` | `main` | 14 | [docs/registry/implementation-map.yaml:L5088-L5091] |
| `scripts/sanitize_s7_models.py` | `<module>` | 89 | [docs/registry/implementation-map.yaml:L5097-L5100] |
| `scripts/sanitize_s7_models.py` | `process_dat_file` | 550 | [docs/registry/implementation-map.yaml:L5106-L5109] |
| `scripts/semantic_validate.py` | `<module>` | 53 | [docs/registry/implementation-map.yaml:L5115-L5118] |
| `scripts/sync_monolithic_yml.py` | `convert_dat_to_yml` | 72 | [docs/registry/implementation-map.yaml:L5124-L5127] |
| `Identity-bearing locator withheld; use the cited map row` | `Definition identified by the cited map row` | 128 | [docs/registry/implementation-map.yaml:L5133-L5136] |
| `Identity-bearing locator withheld; use the cited map row` | `Definition identified by the cited map row` | 207 | [docs/registry/implementation-map.yaml:L5142-L5145] |
| `scripts/update_skill_versions.py` | `update_skill_file` | 147 | [docs/registry/implementation-map.yaml:L5151-L5154] |
| `scripts/validate_orcaflex_model.py` | `ModelValidator.extract_model_info` | 181 | [docs/registry/implementation-map.yaml:L5162-L5165] |
| `scripts/verification/verify_orcaflex_campaign.py` | `_save_body` | 131 | [docs/registry/implementation-map.yaml:L5171-L5174] |
| `scripts/verification/verify_orcaflex_campaign.py` | `main` | 203 | [docs/registry/implementation-map.yaml:L5180-L5183] |
| `src/digitalmodel/asset_integrity/common/data.py` | `SaveData.saveDataYaml` | 417 | [docs/registry/implementation-map.yaml:L5191-L5194] |
| `src/digitalmodel/asset_integrity/common/data.py` | `SaveData.WriteOrcaFlexYMLFile` | 614 | [docs/registry/implementation-map.yaml:L5202-L5205] |
| `src/digitalmodel/asset_integrity/common/data.py` | `<module>` | 910 | [docs/registry/implementation-map.yaml:L5213-L5216] |
| `src/digitalmodel/data_systems/data_procurement/common/stream_handler.py` | `StreamHandler._format_orcaflex_yml` | 367 | [docs/registry/implementation-map.yaml:L5222-L5225] |
| `src/digitalmodel/data_systems/data_procurement/mooring/client.py` | `MooringClient.to_orcaflex_mooring` | 515 | [docs/registry/implementation-map.yaml:L5231-L5234] |
| `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/chain_db_client.py` | `ChainDatabaseClient.to_orcaflex_line_type` | 354 | [docs/registry/implementation-map.yaml:L5240-L5243] |
| `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` | `SyntheticRopeDatabaseClient.to_orcaflex_line_type` | 416 | [docs/registry/implementation-map.yaml:L5249-L5252] |
| `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/wire_rope_db_client.py` | `WireRopeDatabaseClient.to_orcaflex_line_type` | 339 | [docs/registry/implementation-map.yaml:L5258-L5261] |
| `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` | `PipeSpecificationClient.to_orcaflex_line_type` | 440 | [docs/registry/implementation-map.yaml:L5267-L5270] |
| `src/digitalmodel/data_systems/data_procurement/vessel/client.py` | `VesselClient.to_orcaflex_vessel` | 351 | [docs/registry/implementation-map.yaml:L5276-L5279] |
| `src/digitalmodel/drilling_riser/campaign.py` | `case_spec` | 134 | [docs/registry/implementation-map.yaml:L5285-L5288] |
| `src/digitalmodel/drilling_riser/campaign.py` | `_probe` | 313 | [docs/registry/implementation-map.yaml:L5294-L5297] |
| `src/digitalmodel/drilling_riser/campaign.py` | `_capped_probe` | 328 | [docs/registry/implementation-map.yaml:L5303-L5306] |
| `src/digitalmodel/drilling_riser/campaign.py` | `_statics_path` | 400 | [docs/registry/implementation-map.yaml:L5314-L5317] |
| `src/digitalmodel/drilling_riser/campaign.py` | `physical_state_checks` | 758 | [docs/registry/implementation-map.yaml:L5326-L5329] |
| `src/digitalmodel/drilling_riser/global_model/build.py` | `_line_type` | 112 | [docs/registry/implementation-map.yaml:L5335-L5338] |
| `src/digitalmodel/drilling_riser/global_model/build.py` | `build_generic_spec` | 180 | [docs/registry/implementation-map.yaml:L5344-L5347] |
| `src/digitalmodel/drilling_riser/global_model/build.py` | `write_model` | 475 | [docs/registry/implementation-map.yaml:L5355-L5358] |
| `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `reference_periods` | 216 | [docs/registry/implementation-map.yaml:L5366-L5369] |
| `src/digitalmodel/drilling_riser/global_model/open_water.py` | `build_open_water_generic_spec` | 143 | [docs/registry/implementation-map.yaml:L5375-L5378] |
| `src/digitalmodel/drilling_riser/postprocess/stress_range.py` | `extract` | 71 | [docs/registry/implementation-map.yaml:L5384-L5387] |
| `src/digitalmodel/drilling_riser/postprocess/stress_range.py` | `merge_stress_range` | 93 | [docs/registry/implementation-map.yaml:L5393-L5396] |
| `src/digitalmodel/drilling_riser/postprocess/stress_range.py` | `extract_significant` | 345 | [docs/registry/implementation-map.yaml:L5402-L5405] |
| `src/digitalmodel/floating_wind/solver_tier.py` | `prepare_variant` | 292 | [docs/registry/implementation-map.yaml:L5411-L5414] |
| `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` | `OrcaFlexConverter.convert_to_vessel_yaml` | 146 | [docs/registry/implementation-map.yaml:L5420-L5423] |
| `src/digitalmodel/hydrodynamics/diffraction/benchmark_mesh_schematic.py` | `_add_fdf_surface_traces` | 274 | [docs/registry/implementation-map.yaml:L5429-L5432] |
| `src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py` | `run_aqwa` | 170 | [docs/registry/implementation-map.yaml:L5438-L5441] |
| `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | `DiffractionSpec.to_yaml` | 1046 | [docs/registry/implementation-map.yaml:L5447-L5450] |
| `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` | `OrcaFlexExporter.export_vessel_type` | 71 | [docs/registry/implementation-map.yaml:L5456-L5459] |
| `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | `_write_yml` | 701 | [docs/registry/implementation-map.yaml:L5465-L5468] |
| `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | `OrcaWaveRunner._execute_via_api` | 500 | [docs/registry/implementation-map.yaml:L5474-L5477] |
| `src/digitalmodel/hydrodynamics/hull_library/panel_catalog.py` | `PanelCatalog.to_yaml` | 114 | [docs/registry/implementation-map.yaml:L5483-L5486] |
| `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` | `RAOAnalysis.write_filtered_orcaflex_seastate_rao_file` | 41 | [docs/registry/implementation-map.yaml:L5492-L5495] |
| `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` | `RAOAnalysis.get_shape_data` | 544 | [docs/registry/implementation-map.yaml:L5501-L5504] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_viv_modes_excited` | 27 | [docs/registry/implementation-map.yaml:L5510-L5513] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.tabulate_viv_results` | 114 | [docs/registry/implementation-map.yaml:L5519-L5522] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.tabulate_overall_viv_results` | 169 | [docs/registry/implementation-map.yaml:L5528-L5531] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_viv_histograms` | 364 | [docs/registry/implementation-map.yaml:L5537-L5540] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_combined_histograms` | 467 | [docs/registry/implementation-map.yaml:L5548-L5551] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_fatigue_summary_result` | 493 | [docs/registry/implementation-map.yaml:L5559-L5562] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | 586 | [docs/registry/implementation-map.yaml:L5568-L5571] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` | `DNVRPH103_hydrodynamics_circular.save_model` | 56 | [docs/registry/implementation-map.yaml:L5577-L5580] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` | `DNVRPH103_hydrodynamics_circular.update_6d_buoy` | 71 | [docs/registry/implementation-map.yaml:L5588-L5591] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` | `DNVRPH103_hydrodynamics_rectangular.save_model` | 54 | [docs/registry/implementation-map.yaml:L5597-L5600] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` | `DNVRPH103_hydrodynamics_rectangular.update_6d_buoy` | 69 | [docs/registry/implementation-map.yaml:L5608-L5611] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | 11 | [docs/registry/implementation-map.yaml:L5617-L5620] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.riser_joints_to_line_types` | 64 | [docs/registry/implementation-map.yaml:L5629-L5632] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.GetGeneral` | 138 | [docs/registry/implementation-map.yaml:L5642-L5645] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.lineTypes` | 379 | [docs/registry/implementation-map.yaml:L5653-L5656] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.WriteOrcaflexModel` | 791 | [docs/registry/implementation-map.yaml:L5664-L5667] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.build_model` | 802 | [docs/registry/implementation-map.yaml:L5673-L5676] |
| `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.AssignGeneral` | 51 | [docs/registry/implementation-map.yaml:L5684-L5687] |
| `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.AssignBuoyProperties` | 81 | [docs/registry/implementation-map.yaml:L5693-L5696] |
| `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.AssignLineProperties` | 87 | [docs/registry/implementation-map.yaml:L5702-L5705] |
| `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.AssignLineTypeProperties` | 93 | [docs/registry/implementation-map.yaml:L5711-L5714] |
| `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.AssignVesselProperties` | 99 | [docs/registry/implementation-map.yaml:L5720-L5723] |
| `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.save_output` | 205 | [docs/registry/implementation-map.yaml:L5729-L5732] |
| `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.load_variations` | 217 | [docs/registry/implementation-map.yaml:L5738-L5741] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_viv_modes_excited` | 40 | [docs/registry/implementation-map.yaml:L5747-L5750] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.tabulate_viv_results` | 200 | [docs/registry/implementation-map.yaml:L5756-L5759] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.tabulate_overall_viv_results` | 319 | [docs/registry/implementation-map.yaml:L5765-L5768] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_viv_histograms` | 618 | [docs/registry/implementation-map.yaml:L5774-L5777] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_combined_histograms` | 766 | [docs/registry/implementation-map.yaml:L5785-L5788] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | 799 | [docs/registry/implementation-map.yaml:L5796-L5799] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | 982 | [docs/registry/implementation-map.yaml:L5805-L5808] |
| `src/digitalmodel/infrastructure/utils/data.py` | `SaveData.saveDataYaml` | 416 | [docs/registry/implementation-map.yaml:L5814-L5817] |
| `src/digitalmodel/infrastructure/utils/data.py` | `SaveData.WriteOrcaFlexYMLFile` | 613 | [docs/registry/implementation-map.yaml:L5825-L5828] |
| `src/digitalmodel/infrastructure/utils/data.py` | `<module>` | 926 | [docs/registry/implementation-map.yaml:L5836-L5839] |
| `src/digitalmodel/marine_ops/installation/jumper_input.py` | `generate_input` | 69 | [docs/registry/implementation-map.yaml:L5845-L5848] |
| `src/digitalmodel/marine_ops/installation/jumper_installation_schema.py` | `JumperInstallationSpec.to_yaml` | 279 | [docs/registry/implementation-map.yaml:L5857-L5860] |
| `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py` | `build_line_types` | 91 | [docs/registry/implementation-map.yaml:L5866-L5869] |
| `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py` | `build_modular_spec` | 209 | [docs/registry/implementation-map.yaml:L5875-L5878] |
| `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` | `UnifiedRAOReader._detect_format` | 252 | [docs/registry/implementation-map.yaml:L5886-L5889] |
| `src/digitalmodel/orcaflex/model_builder.py` | `VesselConfig.to_orcaflex_dict` | 113 | [docs/registry/implementation-map.yaml:L5895-L5898] |
| `src/digitalmodel/orcaflex/model_builder.py` | `Buoy6DConfig.to_orcaflex_dict` | 147 | [docs/registry/implementation-map.yaml:L5904-L5907] |
| `src/digitalmodel/orcaflex/model_builder.py` | `LineConfig.to_orcaflex_dict` | 183 | [docs/registry/implementation-map.yaml:L5913-L5916] |
| `src/digitalmodel/orcaflex/model_builder.py` | `build_scr_model` | 259 | [docs/registry/implementation-map.yaml:L5922-L5925] |
| `src/digitalmodel/orcaflex/model_builder.py` | `build_lazy_wave_model` | 315 | [docs/registry/implementation-map.yaml:L5931-L5934] |
| `src/digitalmodel/orcaflex/model_builder.py` | `build_mooring_line` | 370 | [docs/registry/implementation-map.yaml:L5940-L5943] |
| `src/digitalmodel/orcaflex/reporting/sections/model_summary.py` | `build_model_summary` | 20 | [docs/registry/implementation-map.yaml:L5949-L5952] |
| `src/digitalmodel/orcaflex/reporting/sections/range_graphs.py` | `build_range_graphs` | 6 | [docs/registry/implementation-map.yaml:L5958-L5961] |
| `src/digitalmodel/orcawave/reporting/sections/rao_plots.py` | `build_rao_plots` | 23 | [docs/registry/implementation-map.yaml:L5967-L5970] |
| `src/digitalmodel/parametric/query.py` | `_write_result` | 339 | [docs/registry/implementation-map.yaml:L5976-L5979] |
| `src/digitalmodel/parametrics/pilots.py` | `orcawave_depth_sweep.runner` | 100 | [docs/registry/implementation-map.yaml:L5985-L5988] |
| `src/digitalmodel/solvers/benchmark/solvers.py` | `_write_diffraction_inputs` | 118 | [docs/registry/implementation-map.yaml:L5994-L5997] |
| `src/digitalmodel/solvers/fea_model/LineType_components.py` | `LineType.get_general` | 43 | [docs/registry/implementation-map.yaml:L6003-L6006] |
| `src/digitalmodel/solvers/fea_model/general_components.py` | `General.getGeneralOrcaFlexSetUp` | 7 | [docs/registry/implementation-map.yaml:L6012-L6015] |
| `src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py` | `load_csv` | 184 | [docs/registry/implementation-map.yaml:L6021-L6024] |
| `src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py` | `get_data` | 291 | [docs/registry/implementation-map.yaml:L6030-L6033] |
| `src/digitalmodel/solvers/orcaflex/core/configuration.py` | `OrcaFlexConfig.to_yaml` | 407 | [docs/registry/implementation-map.yaml:L6039-L6042] |
| `src/digitalmodel/solvers/orcaflex/core/model_interface.py` | `OrcaFlexModelWrapper.save_simulation` | 509 | [docs/registry/implementation-map.yaml:L6048-L6051] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py` | `FeatureAnalyzer.analyze_file` | 76 | [docs/registry/implementation-map.yaml:L6057-L6060] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py` | `FeatureAnalyzer.generate_summary` | 169 | [docs/registry/implementation-map.yaml:L6066-L6069] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py` | `OrcaFlexBatchConverter._real_convert` | 163 | [docs/registry/implementation-map.yaml:L6075-L6078] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py` | `OrcaFlexBatchConverter._mock_convert` | 193 | [docs/registry/implementation-map.yaml:L6087-L6090] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/converter.py` | `YamlConverter.convert_batch` | 36 | [docs/registry/implementation-map.yaml:L6096-L6099] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py` | `DownloadedExamplesIntegrator._infer_components` | 108 | [docs/registry/implementation-map.yaml:L6105-L6108] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py` | `DownloadedExamplesIntegrator._extract_common_configs` | 356 | [docs/registry/implementation-map.yaml:L6114-L6117] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` | `KnowledgeIntegrator._extract_patterns` | 83 | [docs/registry/implementation-map.yaml:L6123-L6126] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` | `KnowledgeIntegrator.build_searchable_index` | 223 | [docs/registry/implementation-map.yaml:L6132-L6135] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py` | `OrcFxAPIConverter._initialize_api` | 55 | [docs/registry/implementation-map.yaml:L6141-L6144] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py` | `OrcFxAPIConverter.convert_file` | 105 | [docs/registry/implementation-map.yaml:L6150-L6153] |
| `src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py` | `detect_format` | 30 | [docs/registry/implementation-map.yaml:L6159-L6162] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` | `ModularToSingleConverter.convert` | 29 | [docs/registry/implementation-map.yaml:L6168-L6171] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` | `ModularToSingleConverter._merge_master` | 107 | [docs/registry/implementation-map.yaml:L6181-L6184] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` | `ModularToSingleConverter._extract_includefiles.traverse` | 191 | [docs/registry/implementation-map.yaml:L6190-L6193] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` | `ModularToSpecConverter.convert` | 96 | [docs/registry/implementation-map.yaml:L6199-L6202] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` | `SingleToModularConverter._write_master_file` | 230 | [docs/registry/implementation-map.yaml:L6208-L6211] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` | `SingleToModularConverter._write_parameters_file` | 262 | [docs/registry/implementation-map.yaml:L6217-L6220] |
| `src/digitalmodel/solvers/orcaflex/inverse_resolver.py` | `_resolve_mooring` | 342 | [docs/registry/implementation-map.yaml:L6226-L6229] |
| `src/digitalmodel/solvers/orcaflex/inverse_resolver.py` | `_resolve_riser` | 475 | [docs/registry/implementation-map.yaml:L6235-L6238] |
| `src/digitalmodel/solvers/orcaflex/library_generator.py` | `LibraryGenerator.generate_component` | 234 | [docs/registry/implementation-map.yaml:L6244-L6247] |
| `src/digitalmodel/solvers/orcaflex/library_generator.py` | `LibraryGenerator.write_index` | 379 | [docs/registry/implementation-map.yaml:L6253-L6256] |
| `src/digitalmodel/solvers/orcaflex/model_catalogue_render.py` | `object_counts` | 120 | [docs/registry/implementation-map.yaml:L6262-L6265] |
| `src/digitalmodel/solvers/orcaflex/model_catalogue_render.py` | `geometry` | 190 | [docs/registry/implementation-map.yaml:L6271-L6274] |
| `src/digitalmodel/solvers/orcaflex/model_generator/__init__.py` | `OrcaFlexModelGenerator.generate_from_template` | 206 | [docs/registry/implementation-map.yaml:L6280-L6283] |
| `src/digitalmodel/solvers/orcaflex/model_generator/__init__.py` | `OrcaFlexModelGenerator._assemble_model` | 268 | [docs/registry/implementation-map.yaml:L6291-L6294] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` | `ModularModelGenerator.generate` | 154 | [docs/registry/implementation-map.yaml:L6300-L6303] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` | `ModularModelGenerator._write_parameters` | 228 | [docs/registry/implementation-map.yaml:L6312-L6315] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` | `ModularModelGenerator._write_master` | 248 | [docs/registry/implementation-map.yaml:L6321-L6324] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` | `ModularModelGenerator.generate_with_overrides` | 264 | [docs/registry/implementation-map.yaml:L6330-L6333] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/general_builder.py` | `GeneralBuilder.build` | 23 | [docs/registry/implementation-map.yaml:L6340-L6343] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py` | `GenericModelBuilder.build` | 201 | [docs/registry/implementation-map.yaml:L6349-L6352] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py` | `LinesBuilder.build` | 46 | [docs/registry/implementation-map.yaml:L6358-L6361] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `LineTypeBuilder.build` | 85 | [docs/registry/implementation-map.yaml:L6367-L6370] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_lines_builder.py` | `RiserLinesBuilder.build` | 40 | [docs/registry/implementation-map.yaml:L6376-L6379] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py` | `RiserLineTypeBuilder.build` | 34 | [docs/registry/implementation-map.yaml:L6385-L6388] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py` | `RiserLineTypeBuilder._build_riser_line_type` | 56 | [docs/registry/implementation-map.yaml:L6396-L6399] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_vessel_builder.py` | `RiserVesselBuilder.build` | 32 | [docs/registry/implementation-map.yaml:L6405-L6408] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py` | `VesselBuilder.build` | 37 | [docs/registry/implementation-map.yaml:L6414-L6417] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.build` | 40 | [docs/registry/implementation-map.yaml:L6423-L6426] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/cli.py` | `cmd_generate` | 170 | [docs/registry/implementation-map.yaml:L6432-L6435] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py` | `MonolithicExtractor._extract_simulation` | 368 | [docs/registry/implementation-map.yaml:L6441-L6444] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py` | `MonolithicExtractor._extract_general_properties` | 568 | [docs/registry/implementation-map.yaml:L6450-L6453] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` | `PostGenerationValidator._check_line_type_references` | 281 | [docs/registry/implementation-map.yaml:L6459-L6462] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` | `PostGenerationValidator._check_vessel_references` | 340 | [docs/registry/implementation-map.yaml:L6468-L6471] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` | `PostGenerationValidator._check_winch_references` | 414 | [docs/registry/implementation-map.yaml:L6477-L6480] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` | `_iter_includefile_refs.traverse` | 747 | [docs/registry/implementation-map.yaml:L6486-L6489] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` | `MooringRouter.route` | 105 | [docs/registry/implementation-map.yaml:L6495-L6498] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` | `MooringRouter._chain_linetype` | 161 | [docs/registry/implementation-map.yaml:L6504-L6507] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` | `MooringRouter._wire_linetype` | 201 | [docs/registry/implementation-map.yaml:L6513-L6516] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` | `MooringRouter._polyester_linetype` | 236 | [docs/registry/implementation-map.yaml:L6522-L6525] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py` | `VesselRouter.route` | 28 | [docs/registry/implementation-map.yaml:L6531-L6534] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py` | `VesselRouter.from_hull_catalog` | 57 | [docs/registry/implementation-map.yaml:L6540-L6543] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `CampaignGenerator.generate` | 539 | [docs/registry/implementation-map.yaml:L6549-L6552] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/writers/basefile.py` | `write_variation_model` | 194 | [docs/registry/implementation-map.yaml:L6558-L6561] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py` | `extract_includefiles.traverse` | 45 | [docs/registry/implementation-map.yaml:L6567-L6570] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py` | `ModularInputValidator._collect_issues_from_level_1` | 219 | [docs/registry/implementation-map.yaml:L6576-L6579] |
| `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.prepare_includefile_for_all_lines` | 509 | [docs/registry/implementation-map.yaml:L6585-L6588] |
| `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.prepare_includefile_for_fender_compression` | 573 | [docs/registry/implementation-map.yaml:L6594-L6597] |
| `src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py` | `MockOrcaFlexAPI.load_model` | 120 | [docs/registry/implementation-map.yaml:L6603-L6606] |
| `src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py` | `OrcaFlexAPI.save_model` | 255 | [docs/registry/implementation-map.yaml:L6612-L6615] |
| `src/digitalmodel/solvers/orcaflex/operability_analysis.py` | `OperabilityAnalyzer.generate_operability_envelope` | 83 | [docs/registry/implementation-map.yaml:L6621-L6624] |
| `src/digitalmodel/solvers/orcaflex/opp_visualization.py` | `OPPVisualization.combine_models` | 128 | [docs/registry/implementation-map.yaml:L6630-L6633] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.save_data` | 107 | [docs/registry/implementation-map.yaml:L6639-L6642] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.saveSummaryToExcel` | 285 | [docs/registry/implementation-map.yaml:L6650-L6653] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.save_histograms` | 819 | [docs/registry/implementation-map.yaml:L6659-L6662] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.save_cfg_files_from_multiple_files` | 853 | [docs/registry/implementation-map.yaml:L6668-L6671] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.save_RAOs` | 963 | [docs/registry/implementation-map.yaml:L6677-L6680] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py` | `OrcaFlexConverterEnhanced._real_convert` | 234 | [docs/registry/implementation-map.yaml:L6686-L6689] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py` | `OrcaFlexConverterEnhanced._mock_convert` | 265 | [docs/registry/implementation-map.yaml:L6697-L6700] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` | `OrcaFlexCustomAnalysis.process_fea` | 83 | [docs/registry/implementation-map.yaml:L6706-L6709] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` | `OrcaFlexCustomAnalysis.clean_model` | 198 | [docs/registry/implementation-map.yaml:L6718-L6721] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` | `OrcaFlexCustomAnalysis.save_model_with_calculated_positions` | 244 | [docs/registry/implementation-map.yaml:L6727-L6730] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` | `OrcaFlexCustomAnalysis.iterate_to_target_value` | 264 | [docs/registry/implementation-map.yaml:L6736-L6739] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` | `OrcaFlexCustomAnalysis.save_data` | 342 | [docs/registry/implementation-map.yaml:L6745-L6748] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` | `OrcInstallation.create_model_for_water_depth` | 29 | [docs/registry/implementation-map.yaml:L6754-L6757] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` | `OrcInstallation.create_installation_depth_model` | 159 | [docs/registry/implementation-map.yaml:L6763-L6766] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` | `OrcInstallation.create_installation_depth_model2` | 199 | [docs/registry/implementation-map.yaml:L6772-L6775] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py` | `OrcaflexIterativeRuns.save_iterative_file` | 64 | [docs/registry/implementation-map.yaml:L6781-L6784] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py` | `OrcaflexIterativeRuns.run_iterative_simulations` | 152 | [docs/registry/implementation-map.yaml:L6790-L6793] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` | `OrcaflexLineTypes.get_umbilical_properties` | 25 | [docs/registry/implementation-map.yaml:L6799-L6802] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` | `OrcaflexLineTypes.assign_key_umbilical_properties` | 35 | [docs/registry/implementation-map.yaml:L6811-L6814] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` | `OrcaflexLineTypes.save_model` | 62 | [docs/registry/implementation-map.yaml:L6820-L6823] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | 14 | [docs/registry/implementation-map.yaml:L6829-L6832] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.riser_joints_to_line_types` | 86 | [docs/registry/implementation-map.yaml:L6841-L6844] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.GetGeneral` | 179 | [docs/registry/implementation-map.yaml:L6854-L6857] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.lineTypes` | 501 | [docs/registry/implementation-map.yaml:L6865-L6868] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.WriteOrcaflexModel` | 1233 | [docs/registry/implementation-map.yaml:L6876-L6879] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.build_model` | 1247 | [docs/registry/implementation-map.yaml:L6885-L6888] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel.py` | `OrcaFlexOptimizedParallelAnalysis.process_single_file` | 89 | [docs/registry/implementation-map.yaml:L6896-L6899] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py` | `process_single_file_worker` | 39 | [docs/registry/implementation-map.yaml:L6905-L6908] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_parallel_analysis.py` | `OrcaFlexParallelAnalysis.process_single_file` | 59 | [docs/registry/implementation-map.yaml:L6914-L6917] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` | `OrcaflexUtilities.update_model` | 136 | [docs/registry/implementation-map.yaml:L6923-L6926] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` | `OrcaflexUtilities.update_input_file` | 164 | [docs/registry/implementation-map.yaml:L6932-L6935] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` | `OrcaflexUtilities.get_full_yaml_file_and_save` | 444 | [docs/registry/implementation-map.yaml:L6941-L6944] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` | `OrcaflexUtilities.get_general_yml` | 471 | [docs/registry/implementation-map.yaml:L6952-L6955] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` | `OrcaflexUtilities.save_cfg_files_from_multiple_files` | 725 | [docs/registry/implementation-map.yaml:L6961-L6964] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_yml_converter.py` | `convert_to_yml` | 15 | [docs/registry/implementation-map.yaml:L6970-L6973] |
| `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator._merge_yaml` | 180 | [docs/registry/implementation-map.yaml:L6979-L6982] |
| `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator._merge_lines` | 199 | [docs/registry/implementation-map.yaml:L6991-L6994] |
| `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator._merge_6dbuoys` | 228 | [docs/registry/implementation-map.yaml:L7000-L7003] |
| `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator._parse_pipeline_path` | 269 | [docs/registry/implementation-map.yaml:L7009-L7012] |
| `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator._parse_boundary_conditions` | 293 | [docs/registry/implementation-map.yaml:L7018-L7021] |
| `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator.create_plan_view` | 513 | [docs/registry/implementation-map.yaml:L7027-L7030] |
| `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator.create_elevation_view` | 604 | [docs/registry/implementation-map.yaml:L7036-L7039] |
| `src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py` | `LoadVessel.aqwa_group_item` | 24 | [docs/registry/implementation-map.yaml:L7045-L7048] |
| `src/digitalmodel/solvers/orcaflex/reporting/extractors/aggregator.py` | `_extract_dynamic_results_all` | 259 | [docs/registry/implementation-map.yaml:L7054-L7057] |
| `src/digitalmodel/solvers/orcaflex/reporting/renderers/riser.py` | `RiserRenderer._build_riser_dynamic_results` | 34 | [docs/registry/implementation-map.yaml:L7063-L7066] |
| `src/digitalmodel/solvers/orcaflex/run_to_sim.py` | `find_model_files` | 236 | [docs/registry/implementation-map.yaml:L7072-L7075] |
| `src/digitalmodel/solvers/orcaflex/spec_author.py` | `AuthoredSpec.to_spec_yaml` | 403 | [docs/registry/implementation-map.yaml:L7081-L7084] |
| `src/digitalmodel/solvers/orcaflex/template_generator.py` | `TemplateGenerator.generate` | 327 | [docs/registry/implementation-map.yaml:L7090-L7093] |
| `src/digitalmodel/solvers/orcaflex/template_generator.py` | `TemplateGenerator._generate_reference` | 382 | [docs/registry/implementation-map.yaml:L7101-L7104] |
| `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` | `InstallationVtoHost.installation_step` | 27 | [docs/registry/implementation-map.yaml:L7110-L7113] |
| `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` | `InstallationVtoHost.get_target_line_length` | 54 | [docs/registry/implementation-map.yaml:L7119-L7122] |
| `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` | `InstallationVtoHost.get_target_reference_distance` | 90 | [docs/registry/implementation-map.yaml:L7130-L7133] |
| `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` | `Lower2ndEnd.installation_step` | 27 | [docs/registry/implementation-map.yaml:L7139-L7142] |
| `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` | `Lower2ndEnd.get_target_line_length` | 54 | [docs/registry/implementation-map.yaml:L7148-L7151] |
| `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` | `Lower2ndEnd.get_target_reference_distance` | 90 | [docs/registry/implementation-map.yaml:L7159-L7162] |
| `src/digitalmodel/solvers/orcaflex/universal/universal_runner.py` | `UniversalOrcaFlexRunner._process_single_model` | 371 | [docs/registry/implementation-map.yaml:L7168-L7171] |
| `src/digitalmodel/solvers/orcaflex/universal_cli.py` | `create_config` | 337 | [docs/registry/implementation-map.yaml:L7177-L7180] |
| `src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py` | `_mapping_issues` | 97 | [docs/registry/implementation-map.yaml:L7186-L7189] |
| `src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py` | `inspect_source` | 115 | [docs/registry/implementation-map.yaml:L7195-L7198] |
| `src/digitalmodel/solvers/orcaflex/yaml_utils.py` | `orcaflex_dump` | 63 | [docs/registry/implementation-map.yaml:L7206-L7209] |
| `src/digitalmodel/solvers/orcaflex/yaml_utils.py` | `orcaflex_load` | 88 | [docs/registry/implementation-map.yaml:L7215-L7218] |
| `src/digitalmodel/solvers/orcaflex/yaml_validator.py` | `OrcaFlexYAMLValidator._detect_format` | 266 | [docs/registry/implementation-map.yaml:L7224-L7227] |
| `src/digitalmodel/solvers/orcaflex/yaml_validator.py` | `OrcaFlexYAMLValidator._check_singular_section_keys` | 707 | [docs/registry/implementation-map.yaml:L7233-L7236] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | `OrcaWaveOrchestrator._create_vessel_config` | 71 | [docs/registry/implementation-map.yaml:L7242-L7245] |
| `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | `OrcaWaveOrchestrator.list_available_vessels` | 652 | [docs/registry/implementation-map.yaml:L7251-L7254] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | `OrcaFlexConverter.save_orcaflex_file` | 402 | [docs/registry/implementation-map.yaml:L7260-L7263] |
| `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | `OrcaFlexConverter._save_compact_format` | 422 | [docs/registry/implementation-map.yaml:L7271-L7274] |
| `src/digitalmodel/solvers/smoke/model_manifest.py` | `_tree` | 96 | [docs/registry/implementation-map.yaml:L7280-L7283] |
| `src/digitalmodel/solvers/smoke/model_probe.py` | `check_settings` | 36 | [docs/registry/implementation-map.yaml:L7289-L7292] |
| `src/digitalmodel/solvers/smoke/probes.py` | `_saved_proof` | 137 | [docs/registry/implementation-map.yaml:L7298-L7301] |
| `src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py` | `prepareFatiguePostProcessing` | 69 | [docs/registry/implementation-map.yaml:L7307-L7310] |
| `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `orcaflexModel` | 13 | [docs/registry/implementation-map.yaml:L7318-L7321] |
| `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `lineTypes` | 555 | [docs/registry/implementation-map.yaml:L7328-L7331] |
| `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `WriteOrcaflexModel` | 1211 | [docs/registry/implementation-map.yaml:L7337-L7340] |
| `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `build_model` | 1222 | [docs/registry/implementation-map.yaml:L7346-L7349] |
| `src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py` | `OrcaFlexModelGenerator.generate_model_yml` | 165 | [docs/registry/implementation-map.yaml:L7358-L7361] |
| `src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py` | `OrcaFlexModelGenerator.generate_model_dict` | 218 | [docs/registry/implementation-map.yaml:L7367-L7370] |
| `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.get_fea_properties` | 120 | [docs/registry/implementation-map.yaml:L7376-L7379] |
| `src/digitalmodel/viv_screening/workflow.py` | `save_echo` | 254 | [docs/registry/implementation-map.yaml:L7385-L7388] |
| `src/digitalmodel/web/digitaltwinfeed/BuoySALM/BuoySALM.py` | `OrcaFlex.get` | 83 | [docs/registry/implementation-map.yaml:L7394-L7397] |
| `src/digitalmodel/web/digitaltwinfeed/FFS/FFS.py` | `OrcaFlex.get` | 68 | [docs/registry/implementation-map.yaml:L7406-L7409] |
| `src/digitalmodel/web/digitaltwinfeed/OffshoreRiser/OffshoreRiser.py` | `OrcaFlex.get` | 75 | [docs/registry/implementation-map.yaml:L7418-L7421] |
| `src/digitalmodel/workflows/automation/go_by_folder/metadata/analysis.py` | `AnalysisMetadataGenerator._detect_workflow_type` | 340 | [docs/registry/implementation-map.yaml:L7430-L7433] |
| `src/digitalmodel/workflows/installation_added_mass_bracket.py` | `integration_basis` | 21 | [docs/registry/implementation-map.yaml:L7439-L7442] |
| `src/digitalmodel/workflows/installation_added_mass_bracket.py` | `named_body` | 45 | [docs/registry/implementation-map.yaml:L7448-L7451] |
| `src/digitalmodel/workflows/installation_added_mass_bracket.py` | `expected_change` | 94 | [docs/registry/implementation-map.yaml:L7457-L7460] |
| `src/digitalmodel/workflows/installation_added_mass_evidence.py` | `read_cases` | 141 | [docs/registry/implementation-map.yaml:L7468-L7471] |
| `src/digitalmodel/workflows/installation_added_mass_materialize.py` | `_write_request` | 107 | [docs/registry/implementation-map.yaml:L7477-L7480] |
| `src/digitalmodel/workflows/installation_added_mass_materialize.py` | `materialize_bracket_case` | 130 | [docs/registry/implementation-map.yaml:L7488-L7491] |
| `src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py` | `_source` | 24 | [docs/registry/implementation-map.yaml:L7500-L7503] |
| `src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py` | `_case_files` | 88 | [docs/registry/implementation-map.yaml:L7509-L7512] |
| `src/digitalmodel/workflows/installation_numerical_compare.py` | `_apply_change` | 126 | [docs/registry/implementation-map.yaml:L7520-L7523] |
| `src/digitalmodel/workflows/installation_numerical_compare.py` | `_canonical_tables` | 166 | [docs/registry/implementation-map.yaml:L7532-L7535] |
| `src/digitalmodel/workflows/installation_numerical_compare.py` | `_runtime` | 214 | [docs/registry/implementation-map.yaml:L7543-L7546] |
| `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_field` | 21 | [docs/registry/implementation-map.yaml:L7552-L7555] |
| `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_line` | 28 | [docs/registry/implementation-map.yaml:L7561-L7564] |
| `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_value` | 38 | [docs/registry/implementation-map.yaml:L7570-L7573] |
| `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_anchors` | 61 | [docs/registry/implementation-map.yaml:L7582-L7585] |
| `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_timing` | 89 | [docs/registry/implementation-map.yaml:L7593-L7596] |
| `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_ordered` | 108 | [docs/registry/implementation-map.yaml:L7602-L7605] |
| `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_apply_native` | 161 | [docs/registry/implementation-map.yaml:L7615-L7618] |
| `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_counts` | 168 | [docs/registry/implementation-map.yaml:L7626-L7629] |
| `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_variation` | 179 | [docs/registry/implementation-map.yaml:L7636-L7639] |
| `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_native_case` | 190 | [docs/registry/implementation-map.yaml:L7648-L7651] |
| `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_request` | 214 | [docs/registry/implementation-map.yaml:L7662-L7665] |
| `src/digitalmodel/workflows/installation_seastates.py` | `_snapshot` | 41 | [docs/registry/implementation-map.yaml:L7671-L7674] |
| `src/digitalmodel/workflows/installation_seastates.py` | `_structure` | 47 | [docs/registry/implementation-map.yaml:L7680-L7683] |
| `src/digitalmodel/workflows/installation_seastates.py` | `_request` | 154 | [docs/registry/implementation-map.yaml:L7689-L7692] |
| `src/digitalmodel/workflows/installation_seastates.py` | `generate_case` | 169 | [docs/registry/implementation-map.yaml:L7698-L7701] |
| `src/digitalmodel/workflows/installation_seastates.py` | `prepare_matrix` | 203 | [docs/registry/implementation-map.yaml:L7711-L7714] |
| `src/digitalmodel/workflows/installation_seastates.py` | `_change_payload` | 244 | [docs/registry/implementation-map.yaml:L7723-L7726] |
| `src/digitalmodel/workflows/installation_seastates.py` | `materialize_case` | 261 | [docs/registry/implementation-map.yaml:L7732-L7735] |
| `src/digitalmodel/workflows/installation_variant_study.py` | `_verify_master` | 72 | [docs/registry/implementation-map.yaml:L7746-L7749] |
| `src/digitalmodel/workflows/installation_variant_study.py` | `_verify_settings` | 82 | [docs/registry/implementation-map.yaml:L7755-L7758] |
| `src/digitalmodel/workflows/installation_variant_study.py` | `prepare_variant_study` | 117 | [docs/registry/implementation-map.yaml:L7764-L7767] |
| `src/digitalmodel/workflows/mcp_server/orcawave/core/mcp_server.py` | `OrcaWaveMCPServer._register_tools.export_to_orcaflex` | 486 | [docs/registry/implementation-map.yaml:L7777-L7780] |
| `src/digitalmodel/workflows/orcaflex_reproduce.py` | `_prepare` | 230 | [docs/registry/implementation-map.yaml:L7786-L7789] |
| `src/digitalmodel/workflows/orcaflex_reproduce_results.py` | `extract_results` | 153 | [docs/registry/implementation-map.yaml:L7795-L7798] |
| `src/digitalmodel/workflows/parametric_run.py` | `_write_yaml` | 210 | [docs/registry/implementation-map.yaml:L7804-L7807] |
| `src/digitalmodel/workflows/skills/skill_resolver.py` | `SkillResolver._parse_skill_file` | 98 | [docs/registry/implementation-map.yaml:L7813-L7816] |
| `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` | `_model` | 152 | [docs/registry/implementation-map.yaml:L7822-L7825] |
| `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` | `_apply` | 170 | [docs/registry/implementation-map.yaml:L7831-L7834] |
| `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` | `build_candidate` | 208 | [docs/registry/implementation-map.yaml:L7840-L7843] |
| `src/digitalmodel/workflows/structure_installation_prepare.py` | `_dependencies` | 30 | [docs/registry/implementation-map.yaml:L7852-L7855] |
| `tests/drilling_riser/global_model/test_batch_params.py` | `base_spec` | 23 | [docs/registry/implementation-map.yaml:L7861-L7864] |
| `tests/drilling_riser/global_model/test_campaign.py` | `base_spec` | 21 | [docs/registry/implementation-map.yaml:L7874-L7877] |
| `tests/drilling_riser/global_model/test_event_models.py` | `_case` | 20 | [docs/registry/implementation-map.yaml:L7887-L7890] |
| `tests/drilling_riser/global_model/test_hang_off.py` | `_generic` | 75 | [docs/registry/implementation-map.yaml:L7896-L7899] |
| `tests/drilling_riser/global_model/test_hang_off.py` | `_case` | 110 | [docs/registry/implementation-map.yaml:L7907-L7910] |
| `tests/drilling_riser/global_model/test_open_water.py` | `_ow_case` | 354 | [docs/registry/implementation-map.yaml:L7916-L7919] |
| `tests/drilling_riser/global_model/test_statics_path.py` | `base_spec` | 28 | [docs/registry/implementation-map.yaml:L7925-L7928] |
| `tests/drilling_riser/global_model/test_tensioner_representation.py` | `_line` | 31 | [docs/registry/implementation-map.yaml:L7938-L7941] |
| `tests/drilling_riser/global_model/test_w5_channels.py` | `base_spec` | 73 | [docs/registry/implementation-map.yaml:L7947-L7950] |
| `tests/drilling_riser/postprocess/test_w510_significant_range.py` | `_doc` | 174 | [docs/registry/implementation-map.yaml:L7960-L7963] |
| `tests/legal/test_sanitize_s7_audit_private.py` | `setup` | 47 | [docs/registry/implementation-map.yaml:L7971-L7974] |
| `tests/maintenance/test_archive_safety.py` | `test_deep_reference_and_registry_paths_are_excluded` | 59 | [docs/registry/implementation-map.yaml:L7980-L7983] |
| `tests/maintenance/test_archive_safety.py` | `test_generator_partitions_move_keep_hold_and_excluded` | 256 | [docs/registry/implementation-map.yaml:L7989-L7992] |
| `tests/orcaflex/test_generic_scr_slwr.py` | `test_specs_match_basis_and_static_geometry` | 56 | [docs/registry/implementation-map.yaml:L7998-L8001] |
| `tests/orcaflex/test_generic_scr_slwr.py` | `test_analytical_reference_reproduces_from_basis` | 192 | [docs/registry/implementation-map.yaml:L8007-L8010] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` | `TestModularToSpecBoundaryReporting.test_expected_gaps_classified` | 393 | [docs/registry/implementation-map.yaml:L8016-L8019] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` | `TestModularToSpecBoundaryReporting.test_actionable_gaps_classified` | 411 | [docs/registry/implementation-map.yaml:L8025-L8028] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` | `TestModularToSpecBoundaryReporting.test_all_unmapped_are_categorized` | 436 | [docs/registry/implementation-map.yaml:L8034-L8037] |
| `tests/solvers/orcaflex/modular_generator/builders/test_general_builder.py` | `_build` | 54 | [docs/registry/implementation-map.yaml:L8043-L8046] |
| `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` | `_build` | 70 | [docs/registry/implementation-map.yaml:L8052-L8055] |
| `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` | `_build` | 72 | [docs/registry/implementation-map.yaml:L8061-L8064] |
| `tests/solvers/orcaflex/modular_generator/goldens/basefile/capture.py` | `main` | 79 | [docs/registry/implementation-map.yaml:L8070-L8073] |
| `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_floating_campaign` | 30 | [docs/registry/implementation-map.yaml:L8079-L8082] |
| `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_slay_campaign` | 126 | [docs/registry/implementation-map.yaml:L8088-L8091] |
| `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py` | `_write_campaign_yaml` | 98 | [docs/registry/implementation-map.yaml:L8097-L8100] |
| `tests/solvers/orcaflex/modular_generator/test_collection_collision.py` | `_write_model` | 52 | [docs/registry/implementation-map.yaml:L8106-L8109] |
| `tests/solvers/orcaflex/modular_generator/test_gom_tieback_jumper_pipeline.py` | `_sections` | 45 | [docs/registry/implementation-map.yaml:L8115-L8118] |
| `tests/solvers/orcaflex/modular_generator/test_gom_tieback_jumper_pipeline.py` | `test_generation_writes_master_and_includes` | 87 | [docs/registry/implementation-map.yaml:L8124-L8127] |
| `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py` | `TestJumperLineTypes.test_three_jumper_variants_emitted` | 108 | [docs/registry/implementation-map.yaml:L8135-L8138] |
| `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py` | `TestJumperLineReferences.test_jumper_line_uses_all_three_variants` | 187 | [docs/registry/implementation-map.yaml:L8144-L8147] |
| `tests/solvers/orcaflex/modular_generator/test_riser_variant_semantic_proof.py` | `_assert_line_type_references_resolve` | 74 | [docs/registry/implementation-map.yaml:L8153-L8156] |
| `tests/solvers/orcaflex/modular_generator/test_semantic_roundtrip.py` | `_write_mono_yaml` | 31 | [docs/registry/implementation-map.yaml:L8162-L8165] |
| `tests/solvers/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/scripts/run_models_to_sim.py` | `find_model_files` | 198 | [docs/registry/implementation-map.yaml:L8171-L8174] |
| `tests/solvers/orcaflex/reporting/fixture_helpers.py` | `_build_boundary_conditions` | 126 | [docs/registry/implementation-map.yaml:L8180-L8183] |
| `tests/solvers/orcaflex/single_file_parallel_test.py` | `SingleFileParallelStrategy._get_line_results` | 135 | [docs/registry/implementation-map.yaml:L8189-L8192] |
| `tests/solvers/orcaflex/single_file_parallel_test.py` | `SingleFileParallelStrategy._get_vessel_results` | 149 | [docs/registry/implementation-map.yaml:L8198-L8201] |
| `tests/solvers/orcaflex/test_model_catalogue.py` | `test_structural_owners_and_path_references` | 203 | [docs/registry/implementation-map.yaml:L8207-L8210] |
| `tests/solvers/orcaflex/test_model_catalogue_references.py` | `test_same_named_unrelated_folders_are_not_references` | 24 | [docs/registry/implementation-map.yaml:L8216-L8219] |
| `tests/solvers/orcaflex/test_model_header_guard.py` | `header_repository_findings` | 25 | [docs/registry/implementation-map.yaml:L8225-L8228] |
| `tests/solvers/orcaflex/test_modular_input_validation_fixes.py` | `master_with_missing_include` | 51 | [docs/registry/implementation-map.yaml:L8234-L8237] |
| `tests/solvers/orcaflex/test_slay_layout.py` | `test_support_types_are_defined_before_vessel_references` | 90 | [docs/registry/implementation-map.yaml:L8243-L8246] |
| `tests/solvers/orcaflex/test_slay_load_form.py` | `_resolve_case.read` | 38 | [docs/registry/implementation-map.yaml:L8252-L8255] |
| `tests/solvers/orcaflex/test_slay_load_form.py` | `_pipe_seed_points` | 117 | [docs/registry/implementation-map.yaml:L8261-L8264] |
| `tests/solvers/orcaflex/test_template_generator.py` | `sample_base_template` | 42 | [docs/registry/implementation-map.yaml:L8270-L8273] |
| `tests/solvers/orcaflex/test_template_generator.py` | `sample_variation` | 74 | [docs/registry/implementation-map.yaml:L8279-L8282] |
| `tests/solvers/orcaflex/test_template_generator.py` | `sample_case_template` | 101 | [docs/registry/implementation-map.yaml:L8288-L8291] |
| `tests/solvers/orcaflex/test_template_generator.py` | `TestTemplateGenerator.test_generate_with_basefile_reference` | 261 | [docs/registry/implementation-map.yaml:L8297-L8300] |
| `tests/solvers/orcaflex/test_template_generator.py` | `TestTemplateGenerator.test_reference_paths_resolve_from_arbitrary_output_dir` | 285 | [docs/registry/implementation-map.yaml:L8306-L8309] |
| `tests/solvers/orcaflex/test_template_generator.py` | `TestTemplateGenerator.test_reference_paths_in_cases_dir_stay_relative` | 319 | [docs/registry/implementation-map.yaml:L8315-L8318] |
| `tests/solvers/smoke/test_model_manifest.py` | `manifest` | 16 | [docs/registry/implementation-map.yaml:L8324-L8327] |
| `tests/test_engine.py` | `temp_config_file` | 814 | [docs/registry/implementation-map.yaml:L8333-L8336] |
| `tests/test_main_exit_status.py` | `_solve_input` | 110 | [docs/registry/implementation-map.yaml:L8342-L8345] |
| `tests/test_skill_resolver.py` | `_write_skill` | 22 | [docs/registry/implementation-map.yaml:L8351-L8354] |
| `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | 52 | [docs/registry/implementation-map.yaml:L8360-L8363] |
| `tests/workflows/test_durable_workflows.py` | `test_synthetic_rope_tn_curve_matches_dnv_os_e301_table_f3` | 1790 | [docs/registry/implementation-map.yaml:L8369-L8372] |
| `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | 1855 | [docs/registry/implementation-map.yaml:L8378-L8381] |
| `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | 1914 | [docs/registry/implementation-map.yaml:L8387-L8390] |
| `tests/workflows/test_durable_workflows.py` | `test_vessel_seakeeping_significant_amplitude_matches_library` | 1984 | [docs/registry/implementation-map.yaml:L8396-L8399] |
| `tests/workflows/test_installation_instability_diagnostic.py` | `fake` | 47 | [docs/registry/implementation-map.yaml:L8405-L8408] |
| `tests/workflows/test_orcaflex_batch_contract.py` | `test_duration_render_preserves_native_default_spelling_and_source` | 97 | [docs/registry/implementation-map.yaml:L8414-L8417] |
| `tests/workflows/test_orcaflex_reproduce.py` | `resume_case` | 107 | [docs/registry/implementation-map.yaml:L8423-L8426] |
| `tests/workflows/test_structure_hydrodynamic_candidate.py` | `case` | 16 | [docs/registry/implementation-map.yaml:L8432-L8435] |

#### Wind and current force routes

| Path | Definition | Map code line | Map citation |
| --- | --- | --- | --- |
| `examples/demos/gtm/demo_03_deepwater_mudmat_installation.py` | `calc_splash_zone` | 312 | [docs/registry/implementation-map.yaml:L13782-L13785] |
| `examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py` | `calc_phase_3_splash` | 375 | [docs/registry/implementation-map.yaml:L13791-L13794] |
| `examples/demos/gtm/results_store_demo05.py` | `<module>` | 51 | [docs/registry/implementation-map.yaml:L13800-L13803] |
| `examples/demos/gtm/tests/test_demo_05_results_store.py` | `_good_case` | 80 | [docs/registry/implementation-map.yaml:L13809-L13812] |
| `examples/domains/fpso/fpso_mooring_analysis.py` | `calculate_environmental_forces` | 194 | [docs/registry/implementation-map.yaml:L13818-L13821] |
| `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | 497 | [docs/registry/implementation-map.yaml:L13827-L13830] |
| `examples/domains/ocimf/ocimf_demo.py` | `main` | 24 | [docs/registry/implementation-map.yaml:L13838-L13841] |
| `examples/domains/workflows/01_spread_mooring_8point.py` | `calculate_environmental_forces` | 123 | [docs/registry/implementation-map.yaml:L13847-L13850] |
| `examples/domains/workflows/03_calm_buoy.py` | `analyze_calm_buoy` | 53 | [docs/registry/implementation-map.yaml:L13856-L13859] |
| `examples/domains/workflows/05_jackup_installation.py` | `analyze_jackup_mooring` | 37 | [docs/registry/implementation-map.yaml:L13865-L13868] |
| `examples/domains/workflows/06_floating_wind_turbine.py` | `analyze_floating_wind_mooring` | 37 | [docs/registry/implementation-map.yaml:L13874-L13877] |
| `examples/domains/workflows/07_wave_energy_converter.py` | `analyze_wec_mooring` | 36 | [docs/registry/implementation-map.yaml:L13883-L13886] |
| `examples/domains/workflows/08_aquaculture_mooring.py` | `analyze_aquaculture_mooring` | 35 | [docs/registry/implementation-map.yaml:L13892-L13895] |
| `examples/domains/workflows/09_spm_terminal.py` | `analyze_spm_terminal` | 34 | [docs/registry/implementation-map.yaml:L13901-L13904] |
| `examples/domains/workflows/10_dp_backup_mooring.py` | `analyze_dp_backup_mooring` | 36 | [docs/registry/implementation-map.yaml:L13910-L13913] |
| `scripts/capabilities/build_onepagers.py` | `<module>` | 36 | [docs/registry/implementation-map.yaml:L13919-L13922] |
| `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `<module>` | 25 | [docs/registry/implementation-map.yaml:L13928-L13931] |
| `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `extract_ocimf_database` | 220 | [docs/registry/implementation-map.yaml:L13937-L13940] |
| `scripts/python/digitalmodel/legacy/generate_integration_charts.py` | `<module>` | 40 | [docs/registry/implementation-map.yaml:L13948-L13951] |
| `scripts/python/digitalmodel/legacy/generate_ocimf_report.py` | `main` | 16 | [docs/registry/implementation-map.yaml:L13957-L13960] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler._setup_test_data` | 112 | [docs/registry/implementation-map.yaml:L13966-L13969] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | 196 | [docs/registry/implementation-map.yaml:L13977-L13980] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.run_all` | 499 | [docs/registry/implementation-map.yaml:L13988-L13991] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `main` | 518 | [docs/registry/implementation-map.yaml:L14001-L14004] |
| `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | 104 | [docs/registry/implementation-map.yaml:L14012-L14015] |
| `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | 853 | [docs/registry/implementation-map.yaml:L14023-L14026] |
| `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_json_summary` | 1191 | [docs/registry/implementation-map.yaml:L14034-L14037] |
| `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.run_complete_validation` | 1216 | [docs/registry/implementation-map.yaml:L14045-L14048] |
| `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `<module>` | 22 | [docs/registry/implementation-map.yaml:L14061-L14064] |
| `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `main` | 371 | [docs/registry/implementation-map.yaml:L14070-L14073] |
| `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `make_polar_overlay` | 406 | [docs/registry/implementation-map.yaml:L14079-L14082] |
| `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `parse_args` | 802 | [docs/registry/implementation-map.yaml:L14088-L14091] |
| `src/digitalmodel/ansys/pressure_vessel.py` | `PressureVesselGenerator.generate_wind_load` | 704 | [docs/registry/implementation-map.yaml:L14097-L14100] |
| `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` | `SyntheticRopeDatabaseClient._generate_rope_spec` | 171 | [docs/registry/implementation-map.yaml:L14106-L14109] |
| `src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py` | `main` | 367 | [docs/registry/implementation-map.yaml:L14115-L14118] |
| `src/digitalmodel/drilling_riser/drift_off.py` | `environmental_force_n` | 97 | [docs/registry/implementation-map.yaml:L14124-L14127] |
| `src/digitalmodel/drilling_riser/envelope.py` | `CurrentProfile.drag_load_n_per_m` | 108 | [docs/registry/implementation-map.yaml:L14133-L14136] |
| `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `as_analysed_sections` | 248 | [docs/registry/implementation-map.yaml:L14142-L14145] |
| `src/digitalmodel/dynmoor_mooring/workflow.py` | `router` | 101 | [docs/registry/implementation-map.yaml:L14151-L14154] |
| `src/digitalmodel/engine.py` | `engine` | 90 | [docs/registry/implementation-map.yaml:L14160-L14163] |
| `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_wind_cmd` | 115 | [docs/registry/implementation-map.yaml:L14172-L14175] |
| `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_current_cmd` | 173 | [docs/registry/implementation-map.yaml:L14181-L14184] |
| `src/digitalmodel/hydrodynamics/cli.py` | `combined_env_cmd` | 229 | [docs/registry/implementation-map.yaml:L14190-L14193] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_total_viv_fatigue` | 99 | [docs/registry/implementation-map.yaml:L14199-L14202] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.tabulate_viv_results` | 114 | [docs/registry/implementation-map.yaml:L14208-L14211] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_total_viv_fatigue` | 176 | [docs/registry/implementation-map.yaml:L14217-L14220] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.tabulate_viv_results` | 200 | [docs/registry/implementation-map.yaml:L14226-L14229] |
| `src/digitalmodel/infrastructure/utils/ETL_components.py` | `ETL_components.transform_viv_current_reformat_1` | 746 | [docs/registry/implementation-map.yaml:L14235-L14238] |
| `src/digitalmodel/installation/installation_pamphlet/calculations.py` | `compute_splashzone_envelope` | 456 | [docs/registry/implementation-map.yaml:L14244-L14247] |
| `src/digitalmodel/installation/jumper_installation/calculations.py` | `calc_phase_3_splash` | 163 | [docs/registry/implementation-map.yaml:L14253-L14256] |
| `src/digitalmodel/marine_ops/marine_analysis/__main__.py` | `main` | 27 | [docs/registry/implementation-map.yaml:L14262-L14265] |
| `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | 306 | [docs/registry/implementation-map.yaml:L14271-L14274] |
| `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | 628 | [docs/registry/implementation-map.yaml:L14282-L14285] |
| `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `create_sample_database` | 861 | [docs/registry/implementation-map.yaml:L14293-L14296] |
| `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py` | `OCIMFWorkflow.router` | 48 | [docs/registry/implementation-map.yaml:L14304-L14307] |
| `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `<module>` | 25 | [docs/registry/implementation-map.yaml:L14313-L14316] |
| `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `extract_ocimf_database` | 220 | [docs/registry/implementation-map.yaml:L14322-L14325] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler._setup_test_data` | 110 | [docs/registry/implementation-map.yaml:L14333-L14336] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | 188 | [docs/registry/implementation-map.yaml:L14346-L14349] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.run_all` | 494 | [docs/registry/implementation-map.yaml:L14357-L14360] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `main` | 513 | [docs/registry/implementation-map.yaml:L14370-L14373] |
| `src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py` | `main` | 644 | [docs/registry/implementation-map.yaml:L14381-L14384] |
| `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | 104 | [docs/registry/implementation-map.yaml:L14390-L14393] |
| `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | 853 | [docs/registry/implementation-map.yaml:L14401-L14404] |
| `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_json_summary` | 1191 | [docs/registry/implementation-map.yaml:L14412-L14415] |
| `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.run_complete_validation` | 1216 | [docs/registry/implementation-map.yaml:L14423-L14426] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py` | `<module>` | 40 | [docs/registry/implementation-map.yaml:L14439-L14442] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_output_dir` | 25 | [docs/registry/implementation-map.yaml:L14448-L14451] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_cli_output_dir` | 34 | [docs/registry/implementation-map.yaml:L14457-L14460] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `load_data` | 55 | [docs/registry/implementation-map.yaml:L14466-L14469] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `main` | 448 | [docs/registry/implementation-map.yaml:L14475-L14478] |
| `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | 320 | [docs/registry/implementation-map.yaml:L14487-L14490] |
| `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | 642 | [docs/registry/implementation-map.yaml:L14498-L14501] |
| `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `create_sample_database` | 875 | [docs/registry/implementation-map.yaml:L14509-L14512] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._environmental_forces` | 77 | [docs/registry/implementation-map.yaml:L14520-L14523] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._ocimf_database_path` | 103 | [docs/registry/implementation-map.yaml:L14531-L14534] |
| `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `_resolve_arrow_direction_in_body_frame` | 39 | [docs/registry/implementation-map.yaml:L14540-L14543] |
| `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `polar_force_overlay` | 273 | [docs/registry/implementation-map.yaml:L14549-L14552] |
| `src/digitalmodel/naval_architecture/maneuvering_envelope.py` | `threshold_speed_for_steerage` | 211 | [docs/registry/implementation-map.yaml:L14558-L14561] |
| `src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py` | `run_maneuvering_envelope_sweep` | 233 | [docs/registry/implementation-map.yaml:L14567-L14570] |
| `src/digitalmodel/orcaflex/installation_analysis.py` | `SplashZoneInput.calculate_splash_zone_loads` | 245 | [docs/registry/implementation-map.yaml:L14576-L14579] |
| `src/digitalmodel/orcawave/drift_forces.py` | `compute_wind_current_drift` | 224 | [docs/registry/implementation-map.yaml:L14585-L14588] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` | `KnowledgeIntegrator._extract_configurations` | 104 | [docs/registry/implementation-map.yaml:L14594-L14597] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder.build` | 171 | [docs/registry/implementation-map.yaml:L14603-L14606] |
| `src/digitalmodel/solvers/orcaflex/reporting/extractors/loads_extractor.py` | `extract_loads` | 12 | [docs/registry/implementation-map.yaml:L14612-L14615] |
| `src/digitalmodel/structural/parachute/chute_assessment.py` | `calculate_dual_chute_drag` | 81 | [docs/registry/implementation-map.yaml:L14621-L14624] |
| `src/digitalmodel/structural/parachute/chute_assessment.py` | `assess_all_load_cases` | 131 | [docs/registry/implementation-map.yaml:L14632-L14635] |
| `src/digitalmodel/structural/parachute/chute_assessment.py` | `simulate_deployment_transient` | 189 | [docs/registry/implementation-map.yaml:L14644-L14647] |
| `src/digitalmodel/structural/parachute/chute_assessment.py` | `export_results_yaml` | 296 | [docs/registry/implementation-map.yaml:L14654-L14657] |
| `src/digitalmodel/structural/parachute/parachute_drag.py` | `calculate_drag_force` | 37 | [docs/registry/implementation-map.yaml:L14663-L14666] |
| `src/digitalmodel/subsea/mooring_analysis/cli.py` | `design_verification` | 159 | [docs/registry/implementation-map.yaml:L14672-L14675] |
| `src/digitalmodel/subsea/mooring_analysis/designer.py` | `MooringDesigner.calculate_environmental_loads` | 60 | [docs/registry/implementation-map.yaml:L14681-L14684] |
| `src/digitalmodel/subsea/mooring_analysis/dynmoor.py` | `build_wind_model.force` | 1094 | [docs/registry/implementation-map.yaml:L14690-L14693] |
| `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `_phase_demand` | 194 | [docs/registry/implementation-map.yaml:L14699-L14702] |
| `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `hydrodynamic_loads` | 171 | [docs/registry/implementation-map.yaml:L14708-L14711] |
| `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `drag_force_per_meter` | 214 | [docs/registry/implementation-map.yaml:L14717-L14720] |
| `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lateral_stability_check` | 291 | [docs/registry/implementation-map.yaml:L14726-L14729] |
| `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `check_lateral_stability` | 363 | [docs/registry/implementation-map.yaml:L14735-L14738] |
| `src/digitalmodel/subsea/pipeline/integrity_workflows.py` | `run_on_bottom_stability` | 7 | [docs/registry/implementation-map.yaml:L14746-L14749] |
| `src/digitalmodel/subsea/viv_analysis/screening.py` | `VIVScreening.screen_with_current_profile` | 278 | [docs/registry/implementation-map.yaml:L14755-L14758] |
| `src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py` | `viv_analysis` | 52 | [docs/registry/implementation-map.yaml:L14764-L14767] |
| `src/digitalmodel/subsea/viv_analysis/vortex_shedding.py` | `VortexSheddingAnalyzer.analyze_with_profile` | 160 | [docs/registry/implementation-map.yaml:L14773-L14776] |
| `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_mooring_design_template` | 306 | [docs/registry/implementation-map.yaml:L14782-L14785] |
| `src/digitalmodel/workflows/workflow_automation/workflows.py` | `MooringSystemDesignWorkflow.create` | 169 | [docs/registry/implementation-map.yaml:L14791-L14794] |
| `tests/contracts/test_force_units_boundary_contract.py` | `<module>` | 52 | [docs/registry/implementation-map.yaml:L14800-L14803] |
| `tests/legal/test_published_pages_have_no_internal_paths.py` | `_load_producer` | 148 | [docs/registry/implementation-map.yaml:L14809-L14812] |
| `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `sample_database_path` | 31 | [docs/registry/implementation-map.yaml:L14818-L14821] |
| `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `ocimf_db` | 51 | [docs/registry/implementation-map.yaml:L14827-L14830] |
| `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `env_forces` | 57 | [docs/registry/implementation-map.yaml:L14836-L14839] |
| `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | 32 | [docs/registry/implementation-map.yaml:L14845-L14848] |
| `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py` | `TestOCIMFMooringIntegration.test_combined_environmental_forces` | 105 | [docs/registry/implementation-map.yaml:L14855-L14858] |
| `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `ocimf_database` | 33 | [docs/registry/implementation-map.yaml:L14864-L14867] |
| `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `pytest_benchmark_update_json` | 364 | [docs/registry/implementation-map.yaml:L14874-L14877] |
| `tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py` | `test_refactored_polar_signatures_match_fixture` | 72 | [docs/registry/implementation-map.yaml:L14883-L14886] |
| `tests/solvers/orcaflex/reporting/fixture_helpers.py` | `_build_loads` | 161 | [docs/registry/implementation-map.yaml:L14892-L14895] |
| `tests/structural/parachute/test_chute_assessment.py` | `TestAssessAllLoadCases.test_governing_force_within_1_percent_of_hand_calc` | 136 | [docs/registry/implementation-map.yaml:L14901-L14904] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_200_mph` | 70 | [docs/registry/implementation-map.yaml:L14910-L14913] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_250_mph` | 82 | [docs/registry/implementation-map.yaml:L14920-L14923] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_force_scales_with_v_squared` | 98 | [docs/registry/implementation-map.yaml:L14930-L14933] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_result_has_newtons` | 105 | [docs/registry/implementation-map.yaml:L14940-L14943] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_newtons` | 173 | [docs/registry/implementation-map.yaml:L14950-L14953] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_lbs` | 188 | [docs/registry/implementation-map.yaml:L14960-L14963] |
| `tests/subsea/catenary_riser/conftest.py` | `catenary_environment` | 19 | [docs/registry/implementation-map.yaml:L14970-L14973] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py` | `TestLiftForce.test_known_lift_force` | 103 | [docs/registry/implementation-map.yaml:L14979-L14982] |
| `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestDragForce.test_scenario_1` | 72 | [docs/registry/implementation-map.yaml:L14988-L14991] |
| `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestLiftForce.test_scenario_1` | 82 | [docs/registry/implementation-map.yaml:L14997-L15000] |
| `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_circular_mock` | 145 | [docs/registry/implementation-map.yaml:L15006-L15009] |
| `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_rectangular_mock` | 170 | [docs/registry/implementation-map.yaml:L15015-L15018] |
| `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_drag_force_per_meter` | 54 | [docs/registry/implementation-map.yaml:L15024-L15027] |
| `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_lift_force_per_meter` | 68 | [docs/registry/implementation-map.yaml:L15033-L15036] |
| `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | 52 | [docs/registry/implementation-map.yaml:L15042-L15045] |

### Counted-family definition audit

The audit uses the map patterns exactly as Python `re` expressions, case-sensitive, on the direct package Python files at the code pin. A pattern matching the package-relative path case-insensitively is omitted; no pattern is omitted for this package. Hits are assigned to the innermost function or class, with unmatched top-level statements grouped as one definition per file.

| Family | Whole-repository matching files / lines | Matching files in scripts |
| --- | --- | --- |
| `effective-tension-riser-stack-up` | 382 / 2837 | 18 [docs/registry/implementation-map.yaml:L1667-L1673] |
| `orcaflex-text-model-writers` | 2556 / 19567 | 431 [docs/registry/implementation-map.yaml:L4235-L4241] |
| `wind-and-current-force-routes` | 284 / 1948 | 21 [docs/registry/implementation-map.yaml:L13775-L13781] |

The repository-wide match counts above are discovery data retained from the initial pack, not re-derived in this revision; outside-package findings are map follow-up, not additional package implementations. The full map inventory above includes the legacy stack-up and OrcaFlex model paths under `scripts/` and the vertical-riser packages.

| Family | Package definition | Classification and reason | Evidence |
| --- | --- | --- | --- |
| `effective-tension-riser-stack-up` | `__init__.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/__init__.py:L1-L72] |
| `effective-tension-riser-stack-up` | `assembly.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/assembly.py:L1-L306] |
| `effective-tension-riser-stack-up` | `assembly.py::StackupItem` | Data holder; helper | [src/digitalmodel/drilling_riser/assembly.py:L44-L56] |
| `effective-tension-riser-stack-up` | `assembly.py::tensioner_system_factor` | Method listed in section 3; no additional implementation | [src/digitalmodel/drilling_riser/assembly.py:L186-L205] |
| `effective-tension-riser-stack-up` | `assembly.py::minimum_top_tension_16q` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/assembly.py:L208-L236] |
| `effective-tension-riser-stack-up` | `assembly.py::RigCapabilityCheck` | Data holder; helper | [src/digitalmodel/drilling_riser/assembly.py:L243-L251] |
| `effective-tension-riser-stack-up` | `assembly.py::_unit_count` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/assembly.py:L254-L263] |
| `effective-tension-riser-stack-up` | `assembly.py::check_rig_capability` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/assembly.py:L266-L306] |
| `effective-tension-riser-stack-up` | `campaign.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/campaign.py:L1-L990] |
| `effective-tension-riser-stack-up` | `campaign.py::case_spec` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L134-L239] |
| `effective-tension-riser-stack-up` | `campaign.py::_ring_tensioners` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L358-L359] |
| `effective-tension-riser-stack-up` | `campaign.py::set_line_tension` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L362-L368] |
| `effective-tension-riser-stack-up` | `campaign.py::calibrate_line_tension` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L371-L393] |
| `effective-tension-riser-stack-up` | `campaign.py::_statics_path` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L400-L565] |
| `effective-tension-riser-stack-up` | `campaign.py::robust_statics` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L568-L634] |
| `effective-tension-riser-stack-up` | `campaign.py::hang_off_statics` | Method listed in section 3; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L695-L731] |
| `effective-tension-riser-stack-up` | `campaign.py::_tensioner_vertical_n` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L734-L737] |
| `effective-tension-riser-stack-up` | `campaign.py::tensioner_vertical_target_n` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L752-L755] |
| `effective-tension-riser-stack-up` | `campaign.py::physical_state_checks` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L758-L810] |
| `effective-tension-riser-stack-up` | `campaign.py::RiserCampaignAdapter.prepare` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L855-L886] |
| `effective-tension-riser-stack-up` | `campaign.py::RiserCampaignAdapter.extract` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L912-L952] |
| `effective-tension-riser-stack-up` | `envelope.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/envelope.py:L1-L446] |
| `effective-tension-riser-stack-up` | `envelope.py::RigEnvelopeLimits` | Data holder; helper | [src/digitalmodel/drilling_riser/envelope.py:L143-L153] |
| `effective-tension-riser-stack-up` | `envelope.py::compute_operating_envelope` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/envelope.py:L278-L446] |
| `effective-tension-riser-stack-up` | `operability_atlas.py::governing_utilisation` | Delegates response to listed envelope implementation; utilisation postprocessing helper | [src/digitalmodel/drilling_riser/operability_atlas.py:L109-L149] |
| `effective-tension-riser-stack-up` | `riser_response.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/riser_response.py:L1-L140] |
| `effective-tension-riser-stack-up` | `schedule_assembly.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/schedule_assembly.py:L1-L777] |
| `effective-tension-riser-stack-up` | `schedule_assembly.py::ScheduleAssembly.minimum_top_tension_16q_kn` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/schedule_assembly.py:L196-L212] |
| `effective-tension-riser-stack-up` | `schedule_assembly.py::Schedule loader at the cited definition` | Method listed in section 3; no additional implementation | [src/digitalmodel/drilling_riser/schedule_assembly.py:L302-L495] |
| `effective-tension-riser-stack-up` | `schedule_assembly.py::_load_oc_assembly` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/schedule_assembly.py:L532-L724] |
| `effective-tension-riser-stack-up` | `section.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/section.py:L1-L146] |
| `effective-tension-riser-stack-up` | `stackup.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/stackup.py:L1-L163] |
| `effective-tension-riser-stack-up` | `stackup.py::top_tension_required` | Method listed in section 3; no additional implementation | [src/digitalmodel/drilling_riser/stackup.py:L28-L50] |
| `effective-tension-riser-stack-up` | `stackup.py::effective_tension` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/stackup.py:L85-L108] |
| `effective-tension-riser-stack-up` | `telemetry_inputs.py::TelemetrySnapshot` | Data holder; helper | [src/digitalmodel/drilling_riser/telemetry_inputs.py:L81-L96] |
| `effective-tension-riser-stack-up` | `telemetry_inputs.py::parse_snapshots` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/telemetry_inputs.py:L135-L158] |
| `effective-tension-riser-stack-up` | `telemetry_inputs.py::snapshot_to_tension_n` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/telemetry_inputs.py:L178-L184] |
| `effective-tension-riser-stack-up` | `tsj_workflow.py::router` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/tsj_workflow.py:L19-L71] |
| `effective-tension-riser-stack-up` | `tsj_workflow.py::_section_result` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/tsj_workflow.py:L74-L111] |
| `effective-tension-riser-stack-up` | `tsj_workflow.py::_effective_tension_n` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/tsj_workflow.py:L146-L160] |
| `effective-tension-riser-stack-up` | `workflow.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/workflow.py:L1-L186] |
| `effective-tension-riser-stack-up` | `workflow.py::router` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/workflow.py:L32-L76] |
| `effective-tension-riser-stack-up` | `workflow.py::_write_profile_csv` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/workflow.py:L154-L159] |
| `orcaflex-text-model-writers` | `adapter.py::_iter_records` | Input reader, output serialiser or context wrapper; helper | [src/digitalmodel/drilling_riser/adapter.py:L153-L161] |
| `orcaflex-text-model-writers` | `campaign.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/campaign.py:L1-L990] |
| `orcaflex-text-model-writers` | `campaign.py::split_at_couplings` | Mesh-section preparation; no text-model result or solver launch | [src/digitalmodel/drilling_riser/campaign.py:L102-L131] |
| `orcaflex-text-model-writers` | `campaign.py::case_spec` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L134-L239] |
| `orcaflex-text-model-writers` | `campaign.py::robust_statics` | Method listed in section 3; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L568-L634] |
| `orcaflex-text-model-writers` | `campaign.py::open_water_statics` | Method listed in section 3; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L645-L685] |
| `orcaflex-text-model-writers` | `campaign.py::hang_off_statics` | Method listed in section 3; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L695-L731] |
| `orcaflex-text-model-writers` | `campaign.py::_tensioner_vertical_n` | Delegates model writing or extraction to global_model; helper | [src/digitalmodel/drilling_riser/campaign.py:L734-L737] |
| `orcaflex-text-model-writers` | `campaign.py::_end_gz_n` | Delegates model writing or extraction to global_model; helper | [src/digitalmodel/drilling_riser/campaign.py:L740-L744] |
| `orcaflex-text-model-writers` | `campaign.py::physical_state_checks` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/campaign.py:L758-L810] |
| `orcaflex-text-model-writers` | `campaign.py::RiserCampaignAdapter` | Delegates model writing or extraction to global_model; helper | [src/digitalmodel/drilling_riser/campaign.py:L835-L987] |
| `orcaflex-text-model-writers` | `campaign.py::RiserCampaignAdapter.build` | Delegates model writing or extraction to global_model; helper | [src/digitalmodel/drilling_riser/campaign.py:L847-L853] |
| `orcaflex-text-model-writers` | `campaign.py::RiserCampaignAdapter.extract` | Delegates model writing or extraction to global_model; helper | [src/digitalmodel/drilling_riser/campaign.py:L912-L952] |
| `orcaflex-text-model-writers` | `campaign.py::RiserCampaignAdapter._extract_open_water` | Delegates model writing or extraction to global_model; helper | [src/digitalmodel/drilling_riser/campaign.py:L955-L987] |
| `orcaflex-text-model-writers` | `drift_off.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/drift_off.py:L1-L257] |
| `orcaflex-text-model-writers` | `envelope.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/envelope.py:L1-L446] |
| `orcaflex-text-model-writers` | `envelope.py::compute_operating_envelope` | Docstring and configuration-file reference; helper | [src/digitalmodel/drilling_riser/envelope.py:L278-L446] |
| `orcaflex-text-model-writers` | `metocean_inputs.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/metocean_inputs.py:L1-L104] |
| `orcaflex-text-model-writers` | `operability_atlas.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/operability_atlas.py:L1-L237] |
| `orcaflex-text-model-writers` | `section.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/section.py:L1-L146] |
| `orcaflex-text-model-writers` | `tsj_workflow.py::<module>` | Module documentation, imports or constants; no additional formula definition | [src/digitalmodel/drilling_riser/tsj_workflow.py:L1-L250] |
| `orcaflex-text-model-writers` | `tsj_workflow.py::_write_results_csv` | Input reader, output serialiser or context wrapper; helper | [src/digitalmodel/drilling_riser/tsj_workflow.py:L204-L209] |
| `orcaflex-text-model-writers` | `twin_loop.py::_load_context` | Configuration reader; helper | [src/digitalmodel/drilling_riser/twin_loop.py:L221-L256] |
| `orcaflex-text-model-writers` | `workflow.py::_write_profile_csv` | Input reader, output serialiser or context wrapper; helper | [src/digitalmodel/drilling_riser/workflow.py:L154-L159] |
| `wind-and-current-force-routes` | `drift_off.py::environmental_force_n` | Inventory row above; no additional implementation | [src/digitalmodel/drilling_riser/drift_off.py:L97-L131] |
| `wind-and-current-force-routes` | `envelope.py::compute_operating_envelope` | Beam response or composition of listed drag/envelope routes; no independent force-route implementation | [src/digitalmodel/drilling_riser/envelope.py:L278-L446] |
| `wind-and-current-force-routes` | `riser_response.py::solve_static_response` | Beam response or composition of listed drag/envelope routes; no independent force-route implementation | [src/digitalmodel/drilling_riser/riser_response.py:L72-L140] |
| `wind-and-current-force-routes` | `twin_loop.py::evaluate_point` | Beam response or composition of listed drag/envelope routes; no independent force-route implementation | [src/digitalmodel/drilling_riser/twin_loop.py:L79-L144] |
| `wind-and-current-force-routes` | `twin_loop.py::run_twin_loop` | Beam response or composition of listed drag/envelope routes; no independent force-route implementation | [src/digitalmodel/drilling_riser/twin_loop.py:L259-L295] |

Inventory: 835 rows across three counted families, with 72 family-definition hit assignments; further unlisted independent implementations after sections 3 and 9: 0 (count). This is the producing classification, pending independent review.

### Map follow-up

Add or refine map coverage for the package methods in section 3 that lack their own package family: tubular stiffness, taper membrane sizing, beam-column response, conductor foundation response, drift timing, damping, tool passage, metocean binning and operability decisions. Existing map families for batch-run stacks and RAO conversion have no direct-package rows; campaign launch and RAO-origin handling therefore remain follow-up until an updated map includes such rows; issue pending (4). [src/digitalmodel/drilling_riser/section.py:L52-L146] [src/digitalmodel/drilling_riser/tsj_workflow.py:L74-L111] [src/digitalmodel/drilling_riser/campaign.py:L157-L161] [src/digitalmodel/drilling_riser/campaign.py:L242-L268]

Map row additions also need `minimum_slip_ring_tension`, `top_tension_required`, `tensioner_system_factor` and the count-weighted sums of `RiserStackupModel`; the existing effective-tension family does not inventory all of them. Tubular stiffness should include `envelope.RiserSection.second_moment_m4`, `envelope.ConductorInput.ei_nm2`, `tsj_workflow._section_result` and the bore-area helper `schedule_assembly._circle_area_m2`, besides `section.py`; issue pending (4). [src/digitalmodel/drilling_riser/stackup.py:L28-L40] [src/digitalmodel/drilling_riser/stackup.py:L111-L163] [src/digitalmodel/drilling_riser/assembly.py:L110-L155] [src/digitalmodel/drilling_riser/assembly.py:L186-L214] [src/digitalmodel/drilling_riser/envelope.py:L130-L170] [src/digitalmodel/drilling_riser/tsj_workflow.py:L84-L88] [src/digitalmodel/drilling_riser/schedule_assembly.py:L148-L149]

## 10. Coverage and open items

### Open items

The maintainer is the routing contact for each item; engineering criteria and implementation classification are decided by the family quality manager.

| Item | Scope and disposition | Issue |
| --- | --- | --- |
| U1 | Section 3 UNVERIFIED provenance: missing implementation editions and clause titles, the 16Q 1993/2017 edition question, full beam-response and damping references; V3 independently checks loaded shear only; licensed clause text remains unchecked; review flat-factor and schedule assumptions without copying restricted values | issue pending (2) |
| U2 | Section 9 UNVERIFIED solver version and G1–G3 qualification; independent benchmark missing; report evidence and G5 not executed | #2433; #814 |
| T1 | Atlas rebuild byte mismatch at BASE; preserve the failing test and investigate physical equivalence separately | issue pending (1) |
| T2 | Missing dynamic stress must not produce an accepted envelope; C04 is a proposed guard, and C05 guards non-monotonic watch-radius masks | issue pending (3) |
| T3 | Dropped missing-offset telemetry shifts later index-paired metocean conditions; unequal streams truncate, and missing timestamps or an empty stream lack a diagnostic contract | issue pending (5) |
| T4 | External contract, schedule and private-criteria reference checks remain NOT_RUN; no private sidecar values are reproduced; sidecar entry: to be created if later work needs restricted evidence | #1470 |
| T5 | Map follow-up: refine helpers and launch routes, add missing package families, review outside-package discovery candidates, and add shared-log references | issue pending (4) |
| T6 | Remaining engineering coverage: no full independent TSJ, damping, conductor, telemetry or campaign qualification; sub-packages retain separate scope | #811; #2433; #2528; issue pending (2) |
| T7 | Summary-coverage issue remains open despite the guard at the code pin; retain expected-case and ledger evidence in future consumers | #2277 |
| T8 | Blocked coupled drive-off reproduction and unresolved input/tolerance evidence | #162 |
| T9 | API STD 2RD historical edition labels corrected at the pin; API RP 16Q still pairs 1993 clause locators with a reference noting the 2017 edition; independently confirm applicability before compliance claims | #2161; issue pending (2) |
| T10 | Workflow and inventory documentation can lag code; reconcile stale router and count claims using the current source and tests | issue pending (2) |
| T11 | Dependency-scanner contract assertion fails on unchanged BASE code; no implementation change is included | issue pending (6) |

### Read coverage

Direct Python source files read (sub-package engineering implementations excluded from this pack):

| File | Evidence range |
| --- | --- |
| `__init__.py` | [src/digitalmodel/drilling_riser/__init__.py:L1-L72] |
| `adapter.py` | [src/digitalmodel/drilling_riser/adapter.py:L1-L197] |
| `assembly.py` | [src/digitalmodel/drilling_riser/assembly.py:L1-L306] |
| `calc_contracts.py` | [src/digitalmodel/drilling_riser/calc_contracts.py:L1-L158] |
| `campaign.py` | [src/digitalmodel/drilling_riser/campaign.py:L1-L990] |
| `conductor_response.py` | [src/digitalmodel/drilling_riser/conductor_response.py:L1-L93] |
| `damping.py` | [src/digitalmodel/drilling_riser/damping.py:L1-L107] |
| `drift_off.py` | [src/digitalmodel/drilling_riser/drift_off.py:L1-L257] |
| `envelope.py` | [src/digitalmodel/drilling_riser/envelope.py:L1-L446] |
| `metocean_inputs.py` | [src/digitalmodel/drilling_riser/metocean_inputs.py:L1-L104] |
| `operability.py` | [src/digitalmodel/drilling_riser/operability.py:L1-L81] |
| `operability_atlas.py` | [src/digitalmodel/drilling_riser/operability_atlas.py:L1-L237] |
| `operability_screening.py` | [src/digitalmodel/drilling_riser/operability_screening.py:L1-L89] |
| `response_correction.py` | [src/digitalmodel/drilling_riser/response_correction.py:L1-L141] |
| `riser_response.py` | [src/digitalmodel/drilling_riser/riser_response.py:L1-L140] |
| `schedule_assembly.py` | [src/digitalmodel/drilling_riser/schedule_assembly.py:L1-L777] |
| `section.py` | [src/digitalmodel/drilling_riser/section.py:L1-L146] |
| `stackup.py` | [src/digitalmodel/drilling_riser/stackup.py:L1-L163] |
| `telemetry_inputs.py` | [src/digitalmodel/drilling_riser/telemetry_inputs.py:L1-L191] |
| `tool_passage.py` | [src/digitalmodel/drilling_riser/tool_passage.py:L1-L88] |
| `tsj_workflow.py` | [src/digitalmodel/drilling_riser/tsj_workflow.py:L1-L250] |
| `twin_loop.py` | [src/digitalmodel/drilling_riser/twin_loop.py:L1-L295] |
| `workflow.py` | [src/digitalmodel/drilling_riser/workflow.py:L1-L186] |

Tests read and run directly in the package are listed below; the extra envelope-library tests and criteria-getter tests cover external Python dependencies. Solver tests in the separate global-model package are outside this test selection.

| Test file | Evidence range |
| --- | --- |
| `tests/drilling_riser/test_adapter_integration.py` | [tests/drilling_riser/test_adapter_integration.py:L1-L106] |
| `tests/drilling_riser/test_assembly.py` | [tests/drilling_riser/test_assembly.py:L1-L185] |
| `tests/drilling_riser/test_assembly_golden.py` | [tests/drilling_riser/test_assembly_golden.py:L1-L344] |
| `tests/drilling_riser/test_calc_contracts.py` | [tests/drilling_riser/test_calc_contracts.py:L1-L126] |
| `tests/drilling_riser/test_conductor_response.py` | [tests/drilling_riser/test_conductor_response.py:L1-L70] |
| `tests/drilling_riser/test_damping_doc_verified.py` | [tests/drilling_riser/test_damping_doc_verified.py:L1-L100] |
| `tests/drilling_riser/test_drift_off.py` | [tests/drilling_riser/test_drift_off.py:L1-L261] |
| `tests/drilling_riser/test_drift_off_constructed_benchmark.py` | [tests/drilling_riser/test_drift_off_constructed_benchmark.py:L1-L220] |
| `tests/drilling_riser/test_drive_off_worked_example.py` | [tests/drilling_riser/test_drive_off_worked_example.py:L1-L182] |
| `tests/drilling_riser/test_envelope.py` | [tests/drilling_riser/test_envelope.py:L1-L368] |
| `tests/drilling_riser/test_metocean_inputs.py` | [tests/drilling_riser/test_metocean_inputs.py:L1-L131] |
| `tests/drilling_riser/test_operability_atlas.py` | [tests/drilling_riser/test_operability_atlas.py:L1-L150] |
| `tests/drilling_riser/test_operability_atlas_governance.py` | [tests/drilling_riser/test_operability_atlas_governance.py:L1-L85] |
| `tests/drilling_riser/test_operability_doc_verified.py` | [tests/drilling_riser/test_operability_doc_verified.py:L1-L97] |
| `tests/drilling_riser/test_operability_explorer.py` | [tests/drilling_riser/test_operability_explorer.py:L1-L160] |
| `tests/drilling_riser/test_operability_monitor.py` | [tests/drilling_riser/test_operability_monitor.py:L1-L289] |
| `tests/drilling_riser/test_registry_batch.py` | [tests/drilling_riser/test_registry_batch.py:L1-L165] |
| `tests/drilling_riser/test_response_correction.py` | [tests/drilling_riser/test_response_correction.py:L1-L183] |
| `tests/drilling_riser/test_riser_response.py` | [tests/drilling_riser/test_riser_response.py:L1-L128] |
| `tests/drilling_riser/test_riser_stackup_workflow.py` | [tests/drilling_riser/test_riser_stackup_workflow.py:L1-L57] |
| `tests/drilling_riser/test_schedule_assembly.py` | [tests/drilling_riser/test_schedule_assembly.py:L1-L181] |
| `tests/drilling_riser/test_section_properties.py` | [tests/drilling_riser/test_section_properties.py:L1-L87] |
| `tests/drilling_riser/test_stackup_doc_verified.py` | [tests/drilling_riser/test_stackup_doc_verified.py:L1-L144] |
| `tests/drilling_riser/test_telemetry_inputs.py` | [tests/drilling_riser/test_telemetry_inputs.py:L1-L218] |
| `tests/drilling_riser/test_tool_passage_doc_verified.py` | [tests/drilling_riser/test_tool_passage_doc_verified.py:L1-L95] |
| `tests/drilling_riser/test_tsj_workflow.py` | [tests/drilling_riser/test_tsj_workflow.py:L1-L64] |
| `tests/drilling_riser/test_twin_loop.py` | [tests/drilling_riser/test_twin_loop.py:L1-L138] |
| `tests/parametric/test_drilling_riser_envelope_library.py` | [tests/parametric/test_drilling_riser_envelope_library.py:L1-L76] |
| `tests/riser_database/test_citations.py` | [tests/riser_database/test_citations.py:L1-L281] |

The direct module selection collects 266 tests; adding the seven envelope-library tests yields the 273-test selection in section 4. The inventory's approximate count is not used as a validation claim.

Documentation discovery searched Markdown and reStructuredText under `docs/` for the module, stack-up and sizing terms; relevant passages were read, while broad historical plans and third-party catalogues were treated as context rather than engineering acceptance. The following locators identify the discovered documents; historical counts and maturity claims are not adopted.

| Document | Matching context |
| --- | --- |
| `docs/riser_database.md` | [docs/riser_database.md:L118-L255] |
| Demonstration-readiness document; identity-bearing path withheld | Git blob `0dad48e5a60834fd4dbb2894f9f305ee367503a9`, matching lines 15, 38, 52; context only; issue pending (2) |
| `docs/cad-inventory/ecosystem-and-automation-surface.md` | [docs/cad-inventory/ecosystem-and-automation-surface.md:L81-L81] |
| `docs/vision/CALCULATIONS-VISION.md` | [docs/vision/CALCULATIONS-VISION.md:L34-L76] |
| `docs/cad-inventory/project-domain-map.md` | [docs/cad-inventory/project-domain-map.md:L43-L45] |
| `docs/migration/legacy-batch-workflows.md` | [docs/migration/legacy-batch-workflows.md:L150-L173] |
| `docs/migration/legacy-engineering-flow-catalog.md` | [docs/migration/legacy-engineering-flow-catalog.md:L29-L75] |
| `docs/UNITS.md` | [docs/UNITS.md:L3-L7] |
| `docs/maps/digitalmodel-operator-map.md` | [docs/maps/digitalmodel-operator-map.md:L40-L40] |
| `docs/capability-map/capabilities-ia-spec-1444.md` | [docs/capability-map/capabilities-ia-spec-1444.md:L70-L70] |
| `docs/field-development/subsea-production-systems-mapping.md` | [docs/field-development/subsea-production-systems-mapping.md:L90-L90] |
| `docs/domains/SPEC_AUDIT_REPORT.md` | [docs/domains/SPEC_AUDIT_REPORT.md:L81-L122] |
| `docs/plans/2026-05-05-issue-272-engineering-standards-citation-audit.md` | [docs/plans/2026-05-05-issue-272-engineering-standards-citation-audit.md:L23-L23] |
| `docs/plans/2026-07-08-issue-1470-registry-batch-coverage.md` | [docs/plans/2026-07-08-issue-1470-registry-batch-coverage.md:L18-L196] |
| `docs/plans/2026-07-16-issue-1602-riser-data-orcaflex-hf-plan.md` | [docs/plans/2026-07-16-issue-1602-riser-data-orcaflex-hf-plan.md:L67-L67] |
| `docs/domains/diagrams/basename/vertical_riser/vertical_riser.md` | [docs/domains/diagrams/basename/vertical_riser/vertical_riser.md:L5-L5] |
| `docs/domains/drilling/drilling_risers.md` | [docs/domains/drilling/drilling_risers.md:L6-L6] |
| `docs/domains/project-docs/RISER_PHASE1_SUMMARY.md` | [docs/domains/project-docs/RISER_PHASE1_SUMMARY.md:L317-L325] |
| `docs/domains/orcaflex/examples/catalog/complete_catalog.md` | [docs/domains/orcaflex/examples/catalog/complete_catalog.md:L65-L68] |
| `docs/domains/orcaflex/examples/PARAMETER_REFERENCE.md` | [docs/domains/orcaflex/examples/PARAMETER_REFERENCE.md:L126-L127] |
| `docs/domains/orcaflex/examples/ORCAFLEX_MODELS.md` | [docs/domains/orcaflex/examples/ORCAFLEX_MODELS.md:L56-L299] |
| `docs/domains/offshore-engineering/riser-fatigue-fundamentals.md` | [docs/domains/offshore-engineering/riser-fatigue-fundamentals.md:L92-L105] |
| `docs/domains/data/DATA_REORGANIZATION_PLAN.md` | [docs/domains/data/DATA_REORGANIZATION_PLAN.md:L150-L366] |
| `docs/domains/orcaflex/library/model_library/frps_ssr_global_riser/README.md` | [docs/domains/orcaflex/library/model_library/frps_ssr_global_riser/README.md:L99-L99] |

The registry, workflow routes, standards getter metadata, force-unit contract and the separate global-model load route were also read at the code pin; the calculation-citation and data-handling contracts were available as read-only reference material. [docs/registry/module-routing.yaml:L99-L108] [docs/registry/workflows.yaml:L407-L430] [src/digitalmodel/riser_database/getters.py:L61-L78] [tests/contracts/test_force_units_boundary_contract.py:L1-L41] [src/digitalmodel/drilling_riser/global_model/orcaflex_run.py:L22-L42]

Whole-repository method discovery and every implementation-map row for the package families are in section 9; sub-package and external candidates are map follow-up rather than additional in-package misses.
