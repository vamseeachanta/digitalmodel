# Pipeline installation coverage analysis

## Summary

Analytical S-lay and J-lay screens, reel-lay screening, floating-tow model composition, installation integrity functions and on-bottom stability calculations exist at `digitalmodel@d6f8e760c5c3148c7af60df57728c2f7daf3bdd8`. A validated native solver campaign is not established by the inspected source and test definitions. The corrected focused baseline passes 113 tests; three unresolved gaps are recorded as strict expected failures. These tests document defects or missing criteria without changing numerical implementations. No native solver execution or validation is claimed.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 0 | 2026-10-14 | Initial source and test inventory; distinguish analytical and model-generation capability from validated solver evidence. |
| 1 | 2026-10-14 | Separate standard-clause attribution from named tests in API tables; correct column rendering without changing source findings. |
| 2 | 2026-10-14 | Expand acceptance-table locators and include the standalone S-lay demonstration APIs so the named existing code is traceable. |
| 3 | 2026-10-15 | Correct the focused test selection, add strict expected-failure evidence, execute builder tests, and clarify provenance and public-source limits. |

Table 1. Revision history.

## Basis and scope

The source inventory, original test references and model counts in this report refer to `d6f8e760c5c3148c7af60df57728c2f7daf3bdd8`. Source is inspected as text and Python syntax; model candidates are enumerated as tracked files, without loading a solver. Test presence means an assertion or call exists, not that it executed or validates a standard independently. Neither numerical allowables nor licensed standard text are reproduced. Revision 3 test evidence uses unchanged calculation and builder sources at `1fbcd89fa6c446d7baf09d4ccdc71cd30f09f498`, with the three new gap tests identified below.

The location `docs/domains/pipeline-installation/coverage-2026-10.md` follows the domain-note layout in `docs/domains/README.md` and `docs/README.md`, and keeps this cross-module inventory separate from historical model input folders. The operator map routes the installation and pipelay packages to domain documentation.

## Model inventory

| Search at the source revision | Tracked files | Interpretation | Provenance |
|---|---:|---|---|
| `git ls-files 'docs/domains/orcaflex/pipeline/installation/*'` | 173 | All tracked files, including tables, images, scripts and rendered material. | Not established |
| Same search restricted to `.yml`, `.yaml`, `.dat`, `.sim` | 116 | 110 YAML and 6 DAT model candidates; dependencies and variants are not independent solved cases. | Not established |
| `git ls-files 'docs/domains/orcaflex/installation/pipeline/*'` | 472 | All are YAML; this second tree explains the approximate 470-file observation. | Not established |
| Union of the two pipeline trees, model extensions only | 588 | 116 + 472; different directory trees, no pathname intersection. | Not established |
| `git ls-files` filtered by installation/pipelay and model extensions | 673 | Broad filename candidates include structure installation, fixtures, examples and manifests; not a count of pipeline models or solved cases. | Not established |

Table 2. Reconciled counts. The 173 and approximately 470 observations describe different directory trees and different inclusion rules.

The broad filter is `rg -i 'installation.*\.(yml|yaml|dat|sim)$|pipelay.*\.(yml|yaml|dat|sim)$'`. Of its 673 matches, 588 belong to the two pipeline trees, 60 to adjacent manifold/mudmat installation trees, and 25 to other examples, templates, fixtures and configurations: 588 + 60 + 25 = 673. These are filename candidates, not validated complete models. Provenance is not established for either pipeline tree or the broader candidate set. Maintainer review of source rights and identifying names is required before any file is reused as a template; codenaming review is pending. No binary model is parsed.

## Installation methods

| Method | Existing capability at the source revision | Gap and next evidence |
|---|---|---|
| S-lay | Elastic overbend/sagbend screens; stinger geometry; API bending-strain screen; modular vessel, stinger, tensioner and roller builders | Native static configuration, roller loads, independent comparison and a qualified campaign remain unestablished; continue [issue 2431](https://github.com/vamseeachanta/digitalmodel/issues/2431). |
| J-lay | `JLayConfig.calculate_sagbend`, `pipelay.workflow.run_jlay`, YAML example | Analytical catenary/strain screen only; dynamics and native output remain unestablished. |
| Reel-lay | `pipelay.workflow.run_reel_lay`, YAML example | Reeling/residual-curvature screen; cyclic plasticity, ovalisation and native confirmation remain unestablished. |
| Tow | Floating installation schemas, buoyancy/tug composition and line builders | Composed inputs exist; no tow acceptance result is established; continue [issue 133](https://github.com/vamseeachanta/digitalmodel/issues/133). |
| Shore pull-in | Related planned scope in [issue 1739](https://github.com/vamseeachanta/digitalmodel/issues/1739) and [issue 1773](https://github.com/vamseeachanta/digitalmodel/issues/1773) | No qualified shore pull-in workflow is established in the inventoried APIs. |
| On-bottom stability | Canonical and simplified analytical force, weight and stability functions | The legacy import test is ignored by normal discovery and excluded from the corrected focused selection; source assertions do not establish native validation or edition equivalence. |

Table 3. Method coverage, limited to inspected functions and their named tests.

## Acceptance assessment

| Acceptance item | Status | Evidence and limitation |
|---|---|---|
| Portable library package with clean namespace | Not met | Generic `digitalmodel.installation.pipelay` and `digitalmodel.pipelay` namespaces exist. Extraction provenance and a complete source-rights audit are not established. |
| Model template creation utilities | Met, source capability only | `solvers/orcaflex/modular_generator/__init__.py::ModularModelGenerator.generate`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestSlayEndToEnd.test_generate_slay_model` passed within 27 builder tests at the tested head. This establishes YAML generation, not solver validation or template provenance. |
| Load case management: generate, run, track status | Not met | `solvers/orcaflex/modular_generator/schema/campaign.py::CampaignSpec.generate_run_specs` and `CampaignGenerator.generate` supply generation. A pipeline-specific native run/status read-back is not established. |
| Results extraction: time-domain, envelope, static | Not met | `solvers/orcaflex/reporting/extractors/results_extractor.py` is a related extraction surface; validated installation outputs for all three categories are not established. |
| Fatigue: rainflow counting and damage accumulation | Not met | `fatigue/rainflow.py` and `fatigue/damage.py` are related library surfaces. A traced installation-history-to-damage workflow is not established. |
| Legal compliance: zero violations in legal scan | Not met | The historical identifier gate is retired. Source rights and independent credential scanning remain required; passing a content hook does not substantiate historical extraction rights. |
| All extracted code has pytest coverage | Not met | Test definitions are inventoried below. The corrected focused selection passes, with three additional expected failures. This selection does not measure complete extracted-code coverage. |
| Documentation: API reference and usage examples | Not met | This report inventories APIs; `examples/workflows/jlay-pipelay/input.yml` and `examples/workflows/reel-lay-pipelay/input.yml` exist. A complete portable installation API/usage guide and validated worked campaign remain open. |

Table 4. Acceptance assessment for #387 in the related workspace repository; source paths are under `src/digitalmodel/` unless another root is shown, all at the source revision above.

## Public API and test inventory

Inputs list parameter names without defaults; methods also consume their instance/spec fields. Class fields are listed separately. Private helpers, enum values and imported symbols are excluded. A direct test reference is found by a symbol/member access within a named test; indirect use through a fixture, constructor, wrapper or generator is recorded as such, without claiming measured coverage. Same-named methods in unrelated classes cannot establish target coverage. Supporting schema/builders are configuration utilities, not standard acceptance calculations.

Standard locators below are assertions found in repository source, not independent checks of a licensed publication. A missing edition, clause number or clause title remains missing; no stronger mapping is inferred from the topic.

### `src/digitalmodel/installation/pipelay/stress.py`

The module names DNV-ST-F101 and DNV-OS-F101 as separate publications titled Submarine Pipeline Systems; neither has an edition, clause number or clause title specified. Equivalence between the publications is not established.

`SmysCheck` input/result fields: `longitudinal_stress`, `allowable_stress`, `utilisation`, `percent_smys`, `passes`.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `bending_stress_from_radius` | Outer-fibre longitudinal bending stress of a tube bent to a radius. | `outer_diameter`, `bend_radius`, `youngs_modulus` | `tests/installation/test_pipelay_stress.py::test_bending_stress_from_radius`; `tests/installation/test_pipelay_stress.py::test_bending_stress_invalid_radius` | No function-specific standard clause or edition specified. |
| `axial_stress` | Membrane (axial) stress sigma = N / A. | `axial_force`, `steel_area` | `tests/installation/test_pipelay_stress.py::test_axial_stress`; `tests/installation/test_pipelay_stress.py::test_axial_stress_invalid_area` | No engineering-standard clause specified. |
| `smys_stress_check` | Longitudinal-stress %SMYS acceptance check for an over/sagbend section. | `outer_diameter`, `bend_radius`, `youngs_modulus`, `smys`, `allowable_fraction`, `axial_force`, `steel_area` | `tests/installation/test_pipelay_stress.py::test_smys_check_pure_bending_passes`; `tests/installation/test_pipelay_stress.py::test_smys_check_with_axial_term`; `tests/installation/test_pipelay_stress.py::test_smys_check_fails_when_overstressed`; `tests/installation/test_pipelay_stress.py::test_smys_check_axial_requires_area`; additional direct references exist in the same test source | No function-specific standard clause or edition specified. |
| `allowable_bend_radius` | Minimum bend radius for pure bending to stay within f * SMYS. | `outer_diameter`, `youngs_modulus`, `smys`, `allowable_fraction` | `tests/installation/test_pipelay_stress.py::test_allowable_bend_radius`; `tests/installation/test_pipelay_stress.py::test_allowable_bend_radius_consistent_with_check`; `tests/installation/test_pipelay_stress.py::test_allowable_bend_radius_invalid` | No function-specific standard clause or edition specified. |

Table 5. Public functions in `installation/pipelay/stress.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/installation/pipelay/integrity.py`

DNV-ST-F101, Submarine Pipeline Systems for weld repair; edition and clause unspecified. Concrete crushing cites Verley and Ness (1995), incomplete bibliography. Other functions are mechanics, buoyancy and catenary relations, without a standard clause.

`CrushingCheck` input/result fields: `bending_strain`, `concrete_strain`, `allowable_strain`, `utilisation`, `passes`.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `bending_strain_from_radius` | Outer-fibre bending strain of a tube bent to a radius of curvature. | `outer_diameter`, `bend_radius` | `tests/installation/test_pipelay_integrity.py::test_bending_strain_from_radius`; `tests/installation/test_pipelay_integrity.py::test_bending_strain_invalid_radius` | No engineering-standard clause specified. |
| `concrete_crushing_strain` | Compressive strain seen by the concrete coating. | `bending_strain`, `strain_concentration_factor` | `tests/installation/test_pipelay_integrity.py::test_concrete_crushing_strain_scf`; `tests/installation/test_pipelay_integrity.py::test_concrete_crushing_strain_invalid_scf` | No engineering-standard clause specified. |
| `concrete_crushing_check` | Verley & Ness concrete-crushing limiting-strain check in the overbend. | `outer_diameter`, `bend_radius`, `crushing_strain_limit`, `strain_concentration_factor`, `factor_of_safety` | `tests/installation/test_pipelay_integrity.py::test_concrete_crushing_check_passes`; `tests/installation/test_pipelay_integrity.py::test_concrete_crushing_check_fails_with_fos`; `tests/installation/test_pipelay_integrity.py::test_concrete_crushing_check_scf_drives_failure` | No engineering-standard clause specified. |
| `minimum_route_radius` | Minimum stable route / lay radius R = F / (S * W). | `bottom_tension`, `lateral_friction`, `submerged_weight_per_length` | `tests/installation/test_pipelay_integrity.py::test_minimum_route_radius`; `tests/installation/test_pipelay_integrity.py::test_minimum_route_radius_invalid` | No engineering-standard clause specified. |
| `route_radius_factor_of_safety` | Factor of safety of a planned route radius against the minimum stable one. | `planned_radius`, `minimum_radius` | `tests/installation/test_pipelay_integrity.py::test_route_radius_factor_of_safety` | No engineering-standard clause specified. |
| `weld_repair_allowable_length` | Allowable weld-repair (cut-out) length. | `total_weld_length`, `allowable_fraction`, `minimum_repair_length` | `tests/installation/test_pipelay_integrity.py::test_weld_repair_fraction_governs`; `tests/installation/test_pipelay_integrity.py::test_weld_repair_minimum_governs`; `tests/installation/test_pipelay_integrity.py::test_weld_repair_invalid_fraction` | DNV-ST-F101 context; clause and edition missing. |
| `is_weld_repair_acceptable` | True if a proposed repair length is between the minimum and the fractional limit. | `repair_length`, `total_weld_length`, `allowable_fraction`, `minimum_repair_length` | `tests/installation/test_pipelay_integrity.py::test_is_weld_repair_acceptable` | DNV-ST-F101 context; clause and edition missing. |
| `displaced_weight_per_length` | Buoyancy force per unit length of a fully submerged circular pipe. | `outer_diameter`, `water_density`, `gravity` | `tests/installation/test_pipelay_integrity.py::test_displaced_weight_per_length` | No engineering-standard clause specified. |
| `submerged_weight_per_length` | Submerged (wet) unit weight w_sub = w_dry - buoyancy. | `dry_weight_per_length`, `outer_diameter`, `water_density`, `gravity` | `tests/installation/test_pipelay_integrity.py::test_submerged_weight_per_length` | No engineering-standard clause specified. |
| `horizontal_lay_tension` | Horizontal tension of a catenary sagbend H = w_sub * R. | `submerged_weight_per_length`, `sagbend_radius` | `tests/installation/test_pipelay_integrity.py::test_horizontal_lay_tension` | No engineering-standard clause specified. |
| `top_lay_tension` | Top (lay) tension of a free-hanging catenary span. | `submerged_weight_per_length`, `water_depth`, `sagbend_radius` | `tests/installation/test_pipelay_integrity.py::test_top_lay_tension`; `tests/installation/test_pipelay_integrity.py::test_top_lay_tension_equals_wsub_times_R_plus_d` | No engineering-standard clause specified. |

Table 6. Public functions in `installation/pipelay/integrity.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/orcaflex/pipelay_analysis.py`

DNV-OS-F101, Submarine Pipeline Systems, Sections 5 and 11 as stated in source; edition and clause titles unspecified. S-lay/J-lay geometry also cites Bai and Bai (2014) and Mousselli (1981). A DNV-ST-F101 clause is not established by those DNV-OS-F101 citations.

`PipelayPipeProperties` input/result fields: `outer_diameter`, `wall_thickness`, `steel_density`, `youngs_modulus`, `smys`, `poissons_ratio`, `coating_thickness`, `coating_density`, `seawater_density`.

`SLayConfig` input/result fields: `water_depth`, `pipe`, `stinger_radius`, `stinger_length`, `departure_angle`, `lay_tension`, `strain_limit_overbend`, `strain_limit_sagbend`.

`JLayConfig` input/result fields: `water_depth`, `pipe`, `tower_angle`, `top_tension`.

`LayOperability` input/result fields: `max_hs_lay`, `max_hs_welding`, `max_hs_aband_recovery`, `lay_rate_calm`, `lay_rate_degradation`.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `PipelayPipeProperties.inner_diameter` | Derives internal diameter from steel diameter and wall thickness. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `PipelayPipeProperties.total_od` | Derives coated outer diameter. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `PipelayPipeProperties.moment_of_inertia` | Computes moment of inertia; numerical criteria are not reproduced. | Instance/spec fields; no explicit argument | `tests/orcaflex/test_pipelay_analysis.py::TestPipelayPipeProperties.test_moment_of_inertia_positive` | No engineering-standard clause specified. |
| `PipelayPipeProperties.bending_stiffness` | Computes bending stiffness; numerical criteria are not reproduced. | Instance/spec fields; no explicit argument | `tests/orcaflex/test_pipelay_analysis.py::TestPipelayPipeProperties.test_bending_stiffness_positive` | No engineering-standard clause specified. |
| `PipelayPipeProperties.steel_area` | Computes steel area; numerical criteria are not reproduced. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `PipelayPipeProperties.submerged_weight_per_m` | Submerged weight per unit length (N/m). | Instance/spec fields; no explicit argument | `tests/orcaflex/test_pipelay_analysis.py::TestPipelayPipeProperties.test_submerged_weight_positive` | No engineering-standard clause specified. |
| `SLayConfig.calculate_overbend_stress` | Calculate overbend (stinger) stress. | Instance/spec fields; no explicit argument | `tests/orcaflex/test_pipelay_analysis.py::TestSLayConfig.test_overbend_stress`; `tests/orcaflex/test_pipelay_analysis.py::TestSLayConfig.test_min_stinger_radius` | No engineering-standard clause specified. |
| `SLayConfig.calculate_sagbend_stress` | Estimate sagbend bending stress. | Instance/spec fields; no explicit argument | `tests/orcaflex/test_pipelay_analysis.py::TestSLayConfig.test_sagbend_stress` | No engineering-standard clause specified. |
| `SLayConfig.calculate_departure_angle_table` | Generate departure angle vs water depth table. | Instance/spec fields; no explicit argument | `tests/orcaflex/test_pipelay_analysis.py::TestSLayConfig.test_departure_angle_table` | No engineering-standard clause specified. |
| `JLayConfig.calculate_sagbend` | Calculate sagbend geometry for J-Lay. | Instance/spec fields; no explicit argument | `tests/orcaflex/test_pipelay_analysis.py::TestJLayConfig.test_sagbend_calculation`; `tests/orcaflex/test_pipelay_analysis.py::TestJLayConfig.test_near_vertical_departure`; `tests/orcaflex/test_pipelay_analysis.py::TestJLayConfig.test_deeper_water_longer_span` | No engineering-standard clause specified. |
| `LayOperability.lay_rate_at_hs` | Calculate lay rate at given Hs. | `hs` | `tests/orcaflex/test_pipelay_analysis.py::TestLayOperability.test_calm_seas_max_rate`; `tests/orcaflex/test_pipelay_analysis.py::TestLayOperability.test_above_limit_zero_rate` | DNV-OS-F101 §11 context; edition and clause title missing. |
| `LayOperability.generate_operability_table` | Generate Hs vs lay rate table. | Instance/spec fields; no explicit argument | `tests/orcaflex/test_pipelay_analysis.py::TestLayOperability.test_operability_table_format` | DNV-OS-F101 §11 context; edition and clause title missing. |
| `calculate_stinger_radius` | Calculate minimum stinger radius from allowable overbend strain. | `pipe_od`, `max_overbend_strain` | `tests/orcaflex/test_pipelay_analysis.py::TestStingerRadius.test_basic_calculation`; `tests/orcaflex/test_pipelay_analysis.py::TestStingerRadius.test_larger_pipe_needs_larger_radius`; `tests/orcaflex/test_pipelay_analysis.py::TestStingerRadius.test_tighter_strain_needs_larger_radius` | DNV-OS-F101 §11 context; edition and clause title missing. |

Table 7. Public functions in `orcaflex/pipelay_analysis.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/orcaflex/installation_analysis.py`

Adjacent lifting mechanics: source cites DNV-RP-H103 Sections 4, 5.2 and 6, and Noble Denton 0027/ND Sections 3, 5 and 7. No F101, F109 or RP 1111 clause applies by source attribution; edition and clause titles are not established here.

`VesselRAO` input/result fields: `vessel_type`, `heave_rao`, `pitch_rao`, `roll_rao`, `crane_radius`, `crane_height`, `natural_period_heave`, `natural_period_roll`.

`DAFInput` input/result fields: `static_weight_kN`, `crane_tip_heave_m`, `crane_tip_velocity_mps`, `sling_stiffness_kN_per_m`, `lifted_mass_kg`, `added_mass_factor`.

`SlingConfig` input fields: `num_slings`, `sling_angle_deg`, `sling_mbl_kN`, `safety_factor`, `skew_load_factor`, `tilt_factor`, `most_loaded_share`, `lift_points_m`, `cog_offset_m`.

The sling screen separates the most-loaded padeye share PL at the extreme CoG from sling-length/fabrication tolerance effects SKL. DNVGL-ST-E273, *2.7-3 Portable offshore units*, Edition April 2016, §3.5.5 (Lift points – in-plane loads), pp. 34–35, supplies the default minima: 1.10 for two/three legs and 1.33 for four matched legs; §3.5.4 (Skew load effect) describes sling-length inaccuracies. The four-leg output states the matched-sling assumption and EN 13414-1 tolerances. [DNV rules and standards](https://www.dnv.com/rules-standards/).

For two/three legs, supplying plan lift points and an extreme CoG offset from their centroid solves vertical force and moment equilibrium. For four legs, supply an independently assessed `most_loaded_share` (PL); no four-point stiffness/skew solver is implied. All legs use the specified common sling angle. With neither geometry nor share, output explicitly states that equal split is assumed; this is a fallback, not a qualification of CoG uncertainty. Single-leg SKL=1 is a modelling convention. An explicit SKL below the leg-count minimum produces `pass: false`; tension uses at least the minimum. Output retains the entered `skew_load_factor_input`, governing `skew_load_factor` and below-minimum flag. A default remains `None` in serialized inputs, so a model-dump round trip preserves its provenance.

Output preserves `sling_tension_per_leg_kN` as the most-loaded leg tension and adds the share, its basis, SKL, its source/edition and a publisher-portal citation sidecar. The hook-load tension screen does not implement the full RSF padeye design check, including its 1.2 multiplier and separate design-load basis. The cited clause applies without spreader bars and with angle from vertical at least 30 degrees (`sling_angle_deg <=60` from horizontal); §1.4.3 normally limits POU MGW to 100 t. Inputs `has_spreader_bars`, `portable_unit_mgw_t` and `matched_slings` declare these conditions. Outside these limits, with unmatched slings or more than four legs, validation reports "outside clause scope" and requires both an explicit `skew_load_factor` and a nonblank `skew_load_factor_justification`. Validated overrides retain that scope warning in the result. Omitted MGW is reported as an unverified <=100 t assumption; dynamic hook load cannot establish MGW. This is a POU standard; marine-operation lifts require an explicit SKL assessed against DNV-ST-N001, *Marine operations and marine warranty*, Section 16, using the governing project edition.

`SplashZoneInput` input/result fields: `dry_weight_kN`, `submerged_weight_kN`, `projected_area_m2`, `height_m`, `slam_coefficient`, `drag_coefficient`, `added_mass_coefficient`, `crane_tip_heave_m`, `wave_period_s`, `seawater_density`.

`WeightItem` input/result fields: `description`, `dry_weight_kN`, `submerged_weight_kN`, `cog_x`, `cog_y`, `cog_z`.

`WeightManagement` input/result fields: `items`, `contingency_pct`.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `VesselRAO.crane_tip_heave` | Estimate crane tip heave single amplitude (m). | `hs` | `tests/orcaflex/test_installation_analysis.py::TestVesselRAO.test_crane_tip_heave_zero_hs`; `tests/orcaflex/test_installation_analysis.py::TestVesselRAO.test_crane_tip_heave_increases_with_hs` | Lifting reference context above; no F101/F109/RP 1111 clause assigned. |
| `VesselRAO.crane_tip_velocity` | Estimate crane tip velocity (m/s). | `hs`, `tp` | `tests/orcaflex/test_installation_analysis.py::TestVesselRAO.test_crane_tip_velocity_positive`; `tests/orcaflex/test_installation_analysis.py::TestVesselRAO.test_velocity_increases_with_shorter_period` | Lifting reference context above; no F101/F109/RP 1111 clause assigned. |
| `DAFInput.natural_period` | Natural period of crane-sling-load system (s). | Instance/spec fields; no explicit argument | `tests/orcaflex/test_installation_analysis.py::TestDAF.test_natural_period_positive` | Lifting reference context above; no F101/F109/RP 1111 clause assigned. |
| `DAFInput.calculate_daf` | Calculate DAF using the simplified single-degree-of-freedom method. | `wave_period` | `tests/orcaflex/test_installation_analysis.py::TestDAF.test_daf_at_least_one`; `tests/orcaflex/test_installation_analysis.py::TestDAF.test_daf_increases_with_heave`; `tests/orcaflex/test_installation_analysis.py::TestDAF.test_daf_clamps_high_dynamic_loading` | Lifting reference context above; no F101/F109/RP 1111 clause assigned. |
| `SlingConfig.calculate_sling_tension` | Calculate most-loaded sling tension with separate PL and SKL. | `hook_load_kN`, instance fields above | `tests/orcaflex/test_installation_analysis.py::TestSlingConfig.test_offset_cog_exceeds_equal_split`; `tests/orcaflex/test_installation_analysis.py::TestSlingConfig.test_three_leg_hand_calculation`; `tests/orcaflex/test_installation_analysis.py::TestSlingConfig.test_four_leg_extreme_cog_share_hand_calculation`; `tests/orcaflex/test_installation_analysis.py::TestSlingConfig.test_default_skl_by_leg_count` | DNVGL-ST-E273, 2.7-3 Portable offshore units, Edition April 2016, §§3.5.4–3.5.5; SKL minima and PL definition only, not a full RSF verification. |
| `SplashZoneInput.calculate_splash_zone_loads` | Calculate hydrodynamic loads during splash zone transit. | Instance/spec fields; no explicit argument | `tests/orcaflex/test_installation_analysis.py::TestSplashZone.test_slamming_force_positive`; `tests/orcaflex/test_installation_analysis.py::TestSplashZone.test_max_load_exceeds_dry_weight`; `tests/orcaflex/test_installation_analysis.py::TestSplashZone.test_zero_heave_small_dynamic` | Lifting reference context above; no F101/F109/RP 1111 clause assigned. |
| `WeightManagement.total_dry_weight` | Total dry weight (kN). | Instance/spec fields; no explicit argument | `tests/orcaflex/test_installation_analysis.py::TestWeightManagement.test_total_weight` | Lifting reference context above; no F101/F109/RP 1111 clause assigned. |
| `WeightManagement.total_submerged_weight` | Total submerged weight (kN). | Instance/spec fields; no explicit argument | `tests/orcaflex/test_installation_analysis.py::TestWeightManagement.test_total_weight` | Lifting reference context above; no F101/F109/RP 1111 clause assigned. |
| `WeightManagement.total_with_contingency` | Total dry weight with contingency (kN). | Instance/spec fields; no explicit argument | `tests/orcaflex/test_installation_analysis.py::TestWeightManagement.test_contingency` | Lifting reference context above; no F101/F109/RP 1111 clause assigned. |
| `WeightManagement.calculate_cog` | Calculate combined centre of gravity. | Instance/spec fields; no explicit argument | `tests/orcaflex/test_installation_analysis.py::TestWeightManagement.test_cog_calculation` | Lifting reference context above; no F101/F109/RP 1111 clause assigned. |
| `WeightManagement.generate_summary` | Generate weight management summary table. | Instance/spec fields; no explicit argument | `tests/orcaflex/test_installation_analysis.py::TestWeightManagement.test_summary_table` | Lifting reference context above; no F101/F109/RP 1111 clause assigned. |

Table 8. Public functions in `orcaflex/installation_analysis.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/pipelay/workflow.py`

Source cites Bai and Bai (2014), Chapters 5 and 6, and DNV-OS-F101, Submarine Pipeline Systems, Section 11; edition and clause title unspecified. Screening, not a complete installation code assessment.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `run_jlay` | J-lay sagbend screening (offline, analytical). | `settings` | `tests/pipelay/test_pipelay_workflow.py::test_jlay_higher_tension_reduces_sagbend_strain` | No engineering-standard clause specified. |
| `run_reel_lay` | Reel-lay reeling-strain + residual-curvature screening (offline). | `settings` | `tests/pipelay/test_pipelay_workflow.py::test_reeling_strain_exceeding_limit_fails` | No engineering-standard clause specified. |
| `router` | Engine entry point for basename: pipelay. | `cfg` | `tests/pipelay/test_pipelay_workflow.py::test_router_rejects_unknown_method` | No engineering-standard clause specified. |

Table 9. Public functions in `pipelay/workflow.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/subsea/pipeline/api_rp_1111_installation.py`

API RP 1111, Design, Construction, Operation, and Maintenance of Offshore Hydrocarbon Pipelines, fourth edition (2009), Section 6.2 Installation Loads and source locator 6.2.1. Allowable provenance is not independently checked.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `slay_sagbend_bending_strain` | Sagbend bending strain for S-lay installation. | `D`, `R_sagbend` | `tests/subsea/pipeline/test_api_rp_1111_installation.py::test_slay_sagbend_bending_strain_large_radius`; `tests/subsea/pipeline/test_api_rp_1111_installation.py::test_slay_sagbend_bending_strain_small_radius`; `tests/subsea/pipeline/test_api_rp_1111_installation.py::test_slay_sagbend_invalid_radius` | API RP 1111 §6.2.1; source assertion, title/edition above. |
| `slay_overbend_bending_strain` | Overbend bending strain over the stinger. | `D`, `R_overbend` | `tests/subsea/pipeline/test_api_rp_1111_installation.py::test_slay_overbend_bending_strain` | API RP 1111 §6.2.1; source assertion, title/edition above. |
| `slay_allowable_bending_strain` | Computes slay allowable bending strain; numerical criteria are not reproduced. | None | `tests/subsea/pipeline/test_api_rp_1111_installation.py::test_slay_allowable_bending_strain` | API RP 1111 §6.2.1; source assertion, title/edition above. |
| `slay_bending_check` | Computes slay bending check; numerical criteria are not reproduced. | `D`, `R_sagbend`, `R_overbend` | `tests/subsea/pipeline/test_api_rp_1111_installation.py::test_slay_bending_check_pass`; `tests/subsea/pipeline/test_api_rp_1111_installation.py::test_slay_bending_check_fail_sagbend` | API RP 1111 §6.2.1; source assertion, title/edition above. |

Table 10. Public functions in `subsea/pipeline/api_rp_1111_installation.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py`

DNV-RP-F109, On-Bottom Stability Design of Submarine Pipelines, October 2021; force/lift §3.2.1, weight §2.2, absolute stability §4.3.1, generalized stability §4.3.2. Clause titles are not supplied in the function citations.

`StabilityResult` input/result fields: `utilisation`, `is_stable`, `details`.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `hydrodynamic_force_per_meter` | Computes hydrodynamic force per meter; numerical criteria are not reproduced. | `rho_w_kg_m3`, `D_outer_m`, `U_m_s`, `a_m_s2`, `C_D`, `C_M` | `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestHydrodynamicForce.test_zero_velocity_and_acceleration_gives_zero`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestHydrodynamicForce.test_pure_drag_force`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestHydrodynamicForce.test_pure_inertia_force`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestHydrodynamicForce.test_negative_velocity_reverses_drag`; additional direct references exist in the same test source | DNV-RP-F109 §3.2.1; source assertion, title/edition above. |
| `lift_force_per_meter` | Computes lift force per meter; numerical criteria are not reproduced. | `rho_w_kg_m3`, `D_outer_m`, `U_m_s`, `C_L` | `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestLiftForce.test_zero_velocity_gives_zero_lift`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestLiftForce.test_known_lift_force`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestLiftForce.test_lift_always_positive` | DNV-RP-F109 §3.2.1; source assertion, title/edition above. |
| `submerged_weight_per_meter` | Computes submerged weight per meter; numerical criteria are not reproduced. | `D_outer_m`, `t_wall_m`, `rho_steel_kg_m3`, `rho_coat_kg_m3`, `t_coat_m`, `rho_contents_kg_m3`, `rho_w_kg_m3` | `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestSubmergedWeight.test_positive_submerged_weight_for_steel_pipe`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestSubmergedWeight.test_empty_thin_wall_pipe_can_be_buoyant`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestSubmergedWeight.test_weight_increases_with_heavier_contents` | DNV-RP-F109 §2.2; source assertion, title/edition above. |
| `absolute_stability_check` | Computes absolute stability check; numerical criteria are not reproduced. | `W_s_N_m`, `F_H_N_m`, `F_L_N_m`, `mu_soil`, `gamma_SC` | `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestAbsoluteStability.test_stable_pipe`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestAbsoluteStability.test_unstable_pipe`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestAbsoluteStability.test_zero_submerged_weight_gives_infinite_utilisation`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestAbsoluteStability.test_negative_submerged_weight_is_unstable`; additional direct references exist in the same test source | DNV-RP-F109 §4.3.1; source assertion, title/edition above. |
| `generalized_stability_check` | Computes generalized stability check; numerical criteria are not reproduced. | `W_s_N_m`, `F_H_N_m`, `F_L_N_m`, `mu_soil`, `F_R_N_m`, `gamma_SC` | `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestGeneralizedStability.test_stable_with_soil_resistance`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestGeneralizedStability.test_unstable_without_soil_resistance`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestGeneralizedStability.test_zero_capacity_gives_infinite_utilisation`; `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py::TestGeneralizedStability.test_lift_exceeding_weight_is_unstable` | DNV-RP-F109 §4.3.2; source assertion, title/edition above. |

Table 11. Public functions in `subsea/on_bottom_stability/dnv_rp_f109.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/subsea/on_bottom_stability/simplified.py`

DNV-RP-F109, On-Bottom Stability Design of Submarine Pipelines; lateral check §4.3.2. The vertical check and component wrappers do not supply separate clauses; edition is not specified in this module.

`SubmergedWeightResult` input/result fields: `ws_n_per_m`, `buoyancy_n_per_m`, `dry_weight_n_per_m`.

`HydrodynamicLoadResult` input/result fields: `drag_n_per_m`, `inertia_n_per_m`, `total_horizontal_n_per_m`, `lift_n_per_m`.

`StabilityResult` input/result fields: `is_stable`, `utilization`, `required_weight_n_per_m`, `standard`.

`VerticalStabilityResult` input/result fields: `is_stable`, `utilization`, `standard`.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `StabilityResult.utilisation` | UK-spelling alias for :attr:utilization. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `VerticalStabilityResult.utilisation` | UK-spelling alias for :attr:utilization. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `submerged_weight` | Submerged weight of coated pipe per unit length. | `od_steel_m`, `wt_steel_m`, `coating_thickness_m`, `rho_steel`, `rho_coating`, `rho_contents`, `rho_seawater` | `tests/test_on_bottom_stability.py::TestSubmergedWeight.test_concrete_coated_pipe`; `tests/test_on_bottom_stability.py::TestSubmergedWeight.test_empty_pipe_may_float` | No engineering-standard clause specified. |
| `hydrodynamic_loads` | Hydrodynamic forces on a seabed pipe per Morison equation. | `od_total_m`, `water_velocity_m_s`, `water_acceleration_m_s2`, `rho_seawater`, `cd`, `cm`, `cl` | `tests/test_on_bottom_stability.py::TestHydrodynamicLoads.test_drag_and_inertia_forces` | No engineering-standard clause specified. |
| `drag_force_per_meter` | Computes drag force per meter; numerical criteria are not reproduced. | `current_velocity_ms`, `pipe_od_m`, `water_density_kg_m3`, `drag_coeff` | `tests/test_on_bottom_stability.py::TestHydrodynamicLoads.test_drag_force_per_meter` | No engineering-standard clause specified. |
| `lift_force_per_meter` | Computes lift force per meter; numerical criteria are not reproduced. | `current_velocity_ms`, `pipe_od_m`, `water_density_kg_m3`, `lift_coeff` | `tests/test_on_bottom_stability.py::TestHydrodynamicLoads.test_lift_force_per_meter` | No engineering-standard clause specified. |
| `inertia_force_per_meter` | Computes inertia force per meter; numerical criteria are not reproduced. | `acceleration_ms2`, `pipe_od_m`, `water_density_kg_m3`, `inertia_coeff` | `tests/test_on_bottom_stability.py::TestHydrodynamicLoads.test_inertia_force_per_meter` | No engineering-standard clause specified. |
| `lateral_stability_check` | Computes lateral stability check; numerical criteria are not reproduced. | `submerged_weight_n_per_m`, `horizontal_force_n_per_m`, `lift_force_n_per_m`, `friction_coefficient`, `safety_factor` | `tests/test_on_bottom_stability.py::TestLateralStability.test_stable_pipe`; `tests/test_on_bottom_stability.py::TestLateralStability.test_unstable_pipe`; `tests/test_on_bottom_stability.py::TestLateralStability.test_zero_weight_raises` | DNV-RP-F109 §4.3.2; source assertion, edition missing. |
| `check_lateral_stability` | Lateral stability check with kN/m inputs. | `submerged_weight_per_meter_kn`, `horizontal_load_per_meter_kn`, `lift_load_per_meter_kn`, `friction_coeff`, `safety_factor` | `tests/test_on_bottom_stability.py::TestLateralStability.test_check_lateral_stability_passes`; `tests/test_on_bottom_stability.py::TestLateralStability.test_check_lateral_stability_fails` | DNV-RP-F109 §4.3.2; source assertion, edition missing. |
| `check_vertical_stability` | Computes check vertical stability; numerical criteria are not reproduced. | `submerged_weight_per_meter_kn`, `lift_load_per_meter_kn`, `safety_factor` | `tests/test_on_bottom_stability.py::TestVerticalStability.test_vertical_stability_passes`; `tests/test_on_bottom_stability.py::TestVerticalStability.test_vertical_stability_fails`; `tests/test_on_bottom_stability.py::TestVerticalStability.test_zero_weight_raises` | No engineering-standard clause specified. |

Table 12. Public functions in `subsea/on_bottom_stability/simplified.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/solvers/orcaflex/modular_generator/schema/equipment.py`

Model schema, composition or builder utility; no engineering-standard clause is specified.

`TugProperties` input/result fields: `mass`, `volume`, `height`, `moments_of_inertia`.

`Tugs` input/result fields: `count`, `spacing`, `first_position`, `properties`.

`Rollers` input/result fields: `position`, `supports`, `support_type`.

`RollerStation` input/result fields: `position`, `support_count`, `support_type`, `v_angle`, `diameter`, `friction_coefficient`, `height_offset`.

`RollerArrangement` input/result fields: `type`, `stations`, `spacing_pattern`.

`BuoyancyModuleProperties` input/result fields: `mass`, `volume`, `height`.

`BuoyancyModules` input/result fields: `spacing`, `properties`, `start_arc_length`, `end_arc_length`.

`Ramp` input/result fields: `name`, `type`, `origin`, `size`, `rotation`.

`VesselProperties` input/result fields: `loa`, `beam`, `depth`, `draft`, `displacement`, `gmt`, `cog`, `gyration_radii`, `rao_reference`.

`VesselMooring` input/result fields: `drums`, `winch_capacity`, `pattern`.

`Vessel` input/result fields: `name`, `properties`, `mooring`, `position`, `orientation`.

`StingerSection` input/result fields: `length`, `bend_radius`.

`StingerRoller` input/result fields: `arc_length`, `support_type`, `z_offset`.

`Stinger` input/result fields: `radius`, `sections`, `rollers`, `origin_position`, `origin_orientation`.

`Tensioner` input/result fields: `name`, `capacity_kn`, `stiffness`, `damping`, `position_on_vessel`, `tension_value`.

`Equipment` input/result fields: `tugs`, `rollers`, `roller_arrangement`, `buoyancy_modules`, `ramps`, `vessel`, `stinger`, `tensioner`.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `Tugs.validate_position` | Computes validate position; numerical criteria are not reproduced. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Rollers.validate_position` | Computes validate position; numerical criteria are not reproduced. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `RollerStation.validate_position` | Derived instance/spec query or validation; see implementation. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `RollerArrangement.validate_v_angle_for_type` | Computes validate v angle for type; numerical criteria are not reproduced. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `RollerArrangement.uniform` | Create evenly-spaced stations along the X-axis (straight routes only). | `type`, `count`, `first_position`, `spacing`, `**station_kwargs` | `tests/solvers/orcaflex/modular_generator/schema/test_roller_arrangement.py::TestRollerArrangementUniform.test_creates_correct_count`; `tests/solvers/orcaflex/modular_generator/schema/test_roller_arrangement.py::TestRollerArrangementUniform.test_creates_correct_positions`; `tests/solvers/orcaflex/modular_generator/schema/test_roller_arrangement.py::TestRollerArrangementUniform.test_sets_spacing_pattern`; `tests/solvers/orcaflex/modular_generator/schema/test_roller_arrangement.py::TestRollerArrangementUniform.test_forwards_station_kwargs`; additional direct references exist in the same test source | No engineering-standard clause specified. |
| `BuoyancyModules.validate_arc_length_range` | Validate start is before end when both are specified. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Ramp.validate_vector3` | Computes validate vector3; numerical criteria are not reproduced. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `VesselProperties.validate_vector3` | Derived instance/spec query or validation; see implementation. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Vessel.validate_vector3` | Derived instance/spec query or validation; see implementation. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Stinger.validate_vector3` | Derived instance/spec query or validation; see implementation. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Equipment.resolve_roller_compat` | Auto-convert legacy rollers to RollerArrangement if roller_arrangement not set. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Equipment.get_effective_rollers` | Get the effective roller arrangement (from either field). | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/schema/test_equipment_compat.py::TestEquipmentRollerCompat.test_roller_arrangement_takes_precedence`; `tests/solvers/orcaflex/modular_generator/schema/test_equipment_compat.py::TestEquipmentRollerCompat.test_neither_set_returns_none`; `tests/solvers/orcaflex/modular_generator/schema/test_equipment_compat.py::TestEquipmentRollerCompat.test_floating_spec_loads_without_error` | No engineering-standard clause specified. |

Table 13. Public functions in `solvers/orcaflex/modular_generator/schema/equipment.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py`

Model schema, composition or builder utility; no engineering-standard clause is specified.

`Dimensions` input/result fields: `outer_diameter`, `wall_thickness`.

`Coating` input/result fields: `thickness`, `density`, `name`.

`Coatings` input/result fields: `corrosion`, `weight`.

`Segment` input/result fields: `type`, `length`, `segment_length`, `start_arc_length`.

`Pipeline` input/result fields: `name`, `material`, `dimensions`, `coatings`, `segments`, `youngs_modulus`, `poissons_ratio`.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `Dimensions.validate_wall_thickness_ratio` | Validate wall thickness is less than half the outer diameter. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Coating.validate_thickness` | Validate coating thickness is reasonable. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Coating.validate_density` | Validate coating density is physically reasonable. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Segment.validate_length` | Validate segment length is positive and reasonable. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Segment.validate_segment_length` | Validate mesh segment length is reasonable. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Segment.validate_segment_count` | Validate that segment length divides evenly into total length. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `Pipeline.validate_segments` | Validate segment list is not empty and has unique types. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |

Table 14. Public functions in `solvers/orcaflex/modular_generator/schema/pipeline.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py`

Model schema, composition or builder utility; no engineering-standard clause is specified.

`ProjectInputSpec` input/result fields: `metadata`, `environment`, `pipeline`, `riser`, `mooring`, `generic`, `equipment`, `simulation`.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `ProjectInputSpec.validate_consistency` | Cross-validate model components for physical consistency. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `ProjectInputSpec.get_total_pipeline_length` | Calculate total pipeline length from all segments. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `ProjectInputSpec.get_tug_positions` | Generate positions for all tugs based on spacing configuration. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `ProjectInputSpec.get_buoyancy_module_positions` | Generate arc-length positions for buoyancy modules. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `ProjectInputSpec.is_s_lay` | Check if this is an S-lay installation model. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_slay_schema.py::TestInstallationTypeDetection.test_slay_spec_is_s_lay`; `tests/solvers/orcaflex/modular_generator/test_slay_schema.py::TestInstallationTypeDetection.test_floating_spec_is_not_s_lay` | No engineering-standard clause specified. |
| `ProjectInputSpec.is_floating` | Check if this is a floating installation model. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_slay_schema.py::TestInstallationTypeDetection.test_slay_spec_is_not_floating`; `tests/solvers/orcaflex/modular_generator/test_slay_schema.py::TestInstallationTypeDetection.test_floating_spec_is_floating` | No engineering-standard clause specified. |
| `ProjectInputSpec.is_riser` | Check if this is a riser model. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserSpecLoading.test_load_a01_catenary_spec`; `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserSpecLoading.test_spec_model_type_methods`; `tests/solvers/orcaflex/modular_generator/test_generic_schema.py::TestProjectInputSpecGeneric.test_is_riser_returns_false_for_generic`; `tests/solvers/orcaflex/modular_generator/test_extractor.py::TestFullRoundTrip.test_round_trip_with_line_types` | No engineering-standard clause specified. |
| `ProjectInputSpec.is_pipeline` | Check if this is a pipeline model. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserSpecLoading.test_load_a01_catenary_spec`; `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserSpecLoading.test_spec_model_type_methods`; `tests/solvers/orcaflex/modular_generator/test_generic_schema.py::TestProjectInputSpecGeneric.test_is_pipeline_returns_false_for_generic`; `tests/solvers/orcaflex/modular_generator/test_extractor.py::TestFullRoundTrip.test_round_trip_with_line_types` | No engineering-standard clause specified. |
| `ProjectInputSpec.is_mooring` | Check if this is a mooring model. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `ProjectInputSpec.is_generic` | Check if this is a generic model. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_generic_schema.py::TestProjectInputSpecGeneric.test_is_generic_returns_true`; `tests/solvers/orcaflex/modular_generator/test_extractor.py::TestFullRoundTrip.test_round_trip_with_line_types`; `tests/solvers/orcaflex/modular_generator/test_extractor.py::TestFullRoundTrip.test_round_trip_empty_model` | No engineering-standard clause specified. |
| `ProjectInputSpec.get_riser_line_names` | Get names of all riser lines. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserSpecLoading.test_spec_model_type_methods` | No engineering-standard clause specified. |
| `ProjectInputSpec.get_riser_line_type_names` | Get names of all riser line types. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |

Table 15. Public functions in `solvers/orcaflex/modular_generator/schema/root.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py`

Model schema, composition or builder utility; no engineering-standard clause is specified.

`EnvironmentVariation` input/result fields: `name`, `waves`, `current`, `wind`.

`SoilVariation` input/result fields: `name`, `stiffness`, `friction_coefficient`, `slope`.

`InstallationSection` input/result fields: `builder_file`, `template`, `variables`, `enabled`.

`ParameterSweep` input/result fields: `parameter`, `values`, `alias`.

`CampaignMatrix` input/result fields: `water_depths`, `route_lengths`, `tensions`, `environments`, `soils`, `sweeps`.

`CampaignSpec` input/result fields: `schema_version`, `base`, `campaign`, `sections`, `output_naming`, `max_runs`.

`CampaignResult` input/result fields: `run_count`, `skipped_count`, `run_dirs`, `errors`, `matrix_summary`, `manifest_path`.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `CampaignMatrix.validate_water_depths` | Derived instance/spec query or validation; see implementation. | `v` | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `CampaignMatrix.combinations` | Generate cartesian product of all non-None parameters and sweeps. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py::TestCampaignMatrixCombinations.test_depths_only`; `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py::TestCampaignMatrixCombinations.test_depths_x_environments`; `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py::TestCampaignMatrixCombinations.test_full_cartesian`; `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py::TestCampaignMatrixCombinations.test_campaign_matrix_single_sweep_only`; additional direct references exist in the same test source | No engineering-standard clause specified. |
| `CampaignSpec.from_legacy_config` | Convert legacy OrcInstallation config to CampaignSpec. | `cfg` | `tests/solvers/orcaflex/modular_generator/test_legacy_compat.py::TestFromLegacyConfigConvertsDeltaElevations.test_from_legacy_config_converts_delta_elevations`; `tests/solvers/orcaflex/modular_generator/test_legacy_compat.py::TestFromLegacyConfigReturnsCampaignSpec.test_from_legacy_config_returns_campaign_spec`; `tests/solvers/orcaflex/modular_generator/test_legacy_compat.py::TestFromLegacyConfigMissingStructureRaises.test_from_legacy_config_missing_structure_raises`; `tests/solvers/orcaflex/modular_generator/test_legacy_compat.py::TestFromLegacyConfigMissingDeltaElevationsRaises.test_from_legacy_config_missing_delta_elevations_raises`; additional direct references exist in the same test source | No engineering-standard clause specified. |
| `CampaignSpec.validate_installation_type_constraints` | Cross-validate campaign parameters against installation type. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `CampaignSpec.validate_output_naming_coverage` | Warn if varying parameters are not in output_naming template. | Instance/spec fields; no explicit argument | No direct named test identified; indirect coverage requires trace. | No engineering-standard clause specified. |
| `CampaignSpec.generate_run_specs` | Yield (name, modified_spec) pairs. Streaming - one at a time. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py::TestOutputNamingFormatSpecs.test_format_spec_in_template_is_accepted`; `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py::TestOutputNamingFormatSpecs.test_alias_with_format_spec_is_accepted`; `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py::TestSweepNaming.test_sweep_naming_with_alias`; `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py::TestSweepNaming.test_sweep_naming_slug_fallback`; additional direct references exist in the same test source | No engineering-standard clause specified. |
| `CampaignGenerator.preview` | Dry-run: returns parameter combinations without generating files. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py::TestCampaignGeneratorPreview.test_campaign_generator_preview`; `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py::TestCampaignPreviewMatchesGenerationCount.test_campaign_preview_matches_generation_count` | No engineering-standard clause specified. |
| `CampaignGenerator.validate` | Validate campaign consistency. Returns list of warnings. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py::TestCampaignGeneratorValidate.test_campaign_generator_validate_duplicate_names`; `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py::TestCampaignE2E.test_validate_with_sweeps_does_not_crash` | No engineering-standard clause specified. |
| `CampaignGenerator.generate` | Generate all campaign runs. | `output_dir`, `force`, `resume`, `spec_only` | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py::TestCampaignGeneratorGenerate.test_campaign_generator_generate_creates_directories`; `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py::TestCampaignGeneratorGenerate.test_campaign_generator_generate_force_overwrites`; `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py::TestCampaignGeneratorGenerate.test_campaign_generator_generate_resume_skips_existing`; `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py::TestCampaignGeneratorGenerate.test_campaign_generator_generate_returns_result`; additional direct references exist in the same test source | No engineering-standard clause specified. |

Table 16. Public functions in `solvers/orcaflex/modular_generator/schema/campaign.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/solvers/orcaflex/modular_generator/sections.py`

Model schema, composition or builder utility; no engineering-standard clause is specified.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `VariableResolver.resolve` | Replace ${key} and ${key:format} tokens in template content. | `template_content`, `variables` | `tests/solvers/orcaflex/modular_generator/test_sections.py::TestVariableResolver.test_resolve_simple_token`; `tests/solvers/orcaflex/modular_generator/test_sections.py::TestVariableResolver.test_resolve_format_spec`; `tests/solvers/orcaflex/modular_generator/test_sections.py::TestVariableResolver.test_resolve_whole_number_float_auto_format`; `tests/solvers/orcaflex/modular_generator/test_sections.py::TestVariableResolver.test_resolve_unresolved_token_raises`; additional direct references exist in the same test source | No engineering-standard clause specified. |

Table 17. Public functions in `solvers/orcaflex/modular_generator/sections.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py`

Model schema, composition or builder utility; no engineering-standard clause is specified.

`GenerationResult` input/result fields: `master_file`, `include_files`, `variables_resolved`, `warnings`.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `ModularModelGenerator.from_spec` | Create generator from an in-memory ProjectInputSpec. | `spec` | `tests/solvers/orcaflex/modular_generator/test_sections.py::TestGenerateWithOverrides.test_generate_with_overrides_no_overrides`; `tests/solvers/orcaflex/modular_generator/test_sections.py::TestGenerateWithOverrides.test_generate_with_overrides_disabled_section`; `tests/solvers/orcaflex/modular_generator/test_sections.py::TestGenerateWithOverrides.test_generate_with_overrides_custom_template`; `tests/solvers/orcaflex/modular_generator/test_sections.py::TestGenerateWithOverrides.test_generate_with_overrides_returns_generation_result`; additional direct references exist in the same test source | No engineering-standard clause specified. |
| `ModularModelGenerator.generate` | Derived instance/spec query or validation; see implementation. | `output_dir` | `tests/solvers/orcaflex/modular_generator/test_sections.py::TestGenerateWithOverrides.test_generate_with_overrides_no_overrides`; `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserModularGeneration.test_generate_riser_modular_output`; `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserModularGeneration.test_generated_line_types_content`; `tests/solvers/orcaflex/modular_generator/test_riser_generation.py::TestRiserModularGeneration.test_generated_vessel_content`; additional direct references exist in the same test source | No engineering-standard clause specified. |
| `ModularModelGenerator.generate_with_overrides` | Generate model with optional section overrides. | `output_dir`, `sections`, `variables`, `template_base_dir` | `tests/solvers/orcaflex/modular_generator/test_sections.py::TestGenerateWithOverrides.test_generate_with_overrides_no_overrides`; `tests/solvers/orcaflex/modular_generator/test_sections.py::TestGenerateWithOverrides.test_generate_with_overrides_disabled_section`; `tests/solvers/orcaflex/modular_generator/test_sections.py::TestGenerateWithOverrides.test_generate_with_overrides_custom_template`; `tests/solvers/orcaflex/modular_generator/test_sections.py::TestGenerateWithOverrides.test_generate_with_overrides_returns_generation_result`; additional direct references exist in the same test source | No engineering-standard clause specified. |

Table 18. Public functions in `solvers/orcaflex/modular_generator/__init__.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_type_builder.py`

Model schema, composition or builder utility; no engineering-standard clause is specified.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `VesselTypeBuilder.should_generate` | Only generate for S-lay models with vessel defined. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestShouldGenerate.test_vessel_type_builder_generates_for_slay`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestShouldGenerate.test_vessel_type_builder_skips_for_floating` | No engineering-standard clause specified. |
| `VesselTypeBuilder.build` | Build VesselTypes section from vessel properties. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestVesselTypeBuilder.test_output_has_vessel_types_key`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestVesselTypeBuilder.test_vessel_type_name`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestVesselTypeBuilder.test_vessel_type_length`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestVesselTypeBuilder.test_registers_vessel_type_name` | No engineering-standard clause specified. |

Table 19. Public functions in `solvers/orcaflex/modular_generator/builders/vessel_type_builder.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py`

Model schema, composition or builder utility; no engineering-standard clause is specified.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `VesselBuilder.should_generate` | Only generate for S-lay models with vessel defined. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestShouldGenerate.test_vessel_builder_generates_for_slay`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestShouldGenerate.test_vessel_builder_skips_for_floating` | No engineering-standard clause specified. |
| `VesselBuilder.build` | Build Vessels section from vessel and stinger configuration. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestVesselTypeBuilder.test_output_has_vessel_types_key`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestVesselTypeBuilder.test_vessel_type_name`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestVesselTypeBuilder.test_vessel_type_length`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestVesselTypeBuilder.test_registers_vessel_type_name`; additional direct references exist in the same test source | No engineering-standard clause specified. |

Table 20. Public functions in `solvers/orcaflex/modular_generator/builders/vessel_builder.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py`

Model schema, composition or builder utility; no engineering-standard clause is specified.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `WinchBuilder.should_generate` | Only generate for S-lay models with tensioner defined. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestShouldGenerate.test_winch_builder_generates_for_slay`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestShouldGenerate.test_winch_builder_skips_for_floating` | No engineering-standard clause specified. |
| `WinchBuilder.build` | Build Winches section from tensioner configuration. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestWinchBuilder.test_output_has_winches_key`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestWinchBuilder.test_winch_tension_value`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestWinchBuilder.test_winch_connects_vessel_to_pipeline`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestWinchBuilder.test_registers_winch_name` | No engineering-standard clause specified. |

Table 21. Public functions in `solvers/orcaflex/modular_generator/builders/winch_builder.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py`

Model schema, composition or builder utility; no engineering-standard clause is specified.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `LinesBuilder.should_generate` | Only generate for pipeline models. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py::TestShouldGenerate.test_pipeline_model_generates`; `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py::TestShouldGenerate.test_generic_model_skips` | No engineering-standard clause specified. |
| `LinesBuilder.build` | Build the Lines section from pipeline and equipment configuration. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestLinesBuilderSlay.test_slay_end_a_connects_to_vessel`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestLinesBuilderSlay.test_slay_end_b_is_anchored`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestLinesBuilderSlay.test_floating_end_a_is_fixed`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestLinesBuilderSlay.test_floating_end_b_is_buoy`; additional direct references exist in the same test source | No engineering-standard clause specified. |

Table 22. Public functions in `solvers/orcaflex/modular_generator/builders/lines_builder.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `src/digitalmodel/solvers/orcaflex/modular_generator/builders/buoys_builder.py`

Model schema, composition or builder utility; no engineering-standard clause is specified.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `BuoysBuilder.should_generate` | Derived instance/spec query or validation; see implementation. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestShouldGenerate.test_buoys_builder_skips_for_slay`; `tests/solvers/orcaflex/modular_generator/test_slay_builders.py::TestShouldGenerate.test_buoys_builder_generates_for_floating`; `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py::TestBuoysBuilderShouldGenerate.test_floating_generates`; `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py::TestBuoysBuilderShouldGenerate.test_slay_without_rollers_skips`; additional direct references exist in the same test source | No engineering-standard clause specified. |
| `BuoysBuilder.build` | Derived instance/spec query or validation; see implementation. | Instance/spec fields; no explicit argument | `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py::TestBuoysBuilderRollerArrangement.test_single_station_generates_one_buoy`; `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py::TestBuoysBuilderRollerArrangement.test_multi_station_generates_numbered_buoys`; `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py::TestBuoysBuilderRollerArrangement.test_roller_buoy_names_in_context`; `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py::TestBuoysBuilderRollerArrangement.test_legacy_rollers_still_works`; additional direct references exist in the same test source | No engineering-standard clause specified. |
| `BuoysBuilder.get_support_geometry` | Shim: delegates to RollerBuilder.get_support_geometry for call-site compat. | `station`, `roller_type` | `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py::TestGetSupportGeometryShim.test_shim_delegates_to_roller_builder`; `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py::TestSupportGeometry.test_v_roller_geometry`; `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py::TestSupportGeometry.test_flat_roller_geometry`; `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py::TestSupportGeometry.test_cradle_geometry`; additional direct references exist in the same test source | No engineering-standard clause specified. |

Table 23. Public functions in `solvers/orcaflex/modular_generator/builders/buoys_builder.py` at the source revision. Standards attribution in the paragraph above applies only where the source specifies it.

### `examples/demos/gtm/demo_04_shallow_water_pipelay.py`

The S-lay demonstration has self-contained catenary/stress mechanics, rather than dispatching to the library APIs inventoried above. Source references include DNV-ST-F101, Submarine Pipeline Systems (2021), Section 5 for combined stress, and API RP 1111 (2015) by title without a calculation-specific clause. These source assertions do not substantiate fixed criteria or a native solver result. Its golden, CLI and results-store tests are inspected as text only; the baseline stop prevents adding or executing a new gap-test/code part.

| Public function/member | Computation or action | Inputs | Named test references (source only) | Standard clause attribution |
|---|---|---|---|---|
| `validate_run_id` | Validate a run_id is a single safe path segment; return it unchanged if valid. | `run_id` | `examples/demos/gtm/tests/test_demo_04_cli.py::test_validate_run_id_accepts_good`; `examples/demos/gtm/tests/test_demo_04_cli.py::test_validate_run_id_rejects_bad` | No calculation-specific standard clause established. |
| `utilisation_color` | Return hex color for utilisation: green / amber / red. | `util`, `config` | `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_display_color_thresholds_independent_of_go_logic` | No calculation-specific standard clause established. |
| `go_nogo_label` | Map status string to display label. | `status` | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |
| `pipe_label` | Performs pipe label; numerical criteria are not reproduced. | `nominal_size` | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |
| `load_vessels` | Load vessel data from the pipelay_vessels catalog. | `config` | `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_summary_matches_golden`; `examples/demos/gtm/tests/test_demo_04_results_store.py::test_baseline_cases_csv_matches_demo_output`; `examples/demos/gtm/tests/test_demo_04_results_store.py::test_baseline_rebuild_identity` | No calculation-specific standard clause established. |
| `load_pipes` | Load pipeline data from the pipelines catalog and select target wall thicknesses. | `config` | `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_summary_matches_golden`; `examples/demos/gtm/tests/test_demo_04_results_store.py::test_baseline_cases_csv_matches_demo_output`; `examples/demos/gtm/tests/test_demo_04_results_store.py::test_baseline_rebuild_identity` | No calculation-specific standard clause established. |
| `extract_vessel_params` | Extract the key pipelay parameters from the vessel JSON structure. | `vessel` | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |
| `vessel_band_specs` | Derive per-vessel display specs (stinger angle band + tensioner capacity) from the | `vessels` | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |
| `calc_submerged_weight` | Return submerged weight per unit length in kN/m. | `pipe`, `config` | Indirect through produced fixture: `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_produced_results_equal_golden_in_order_field_by_field` | No calculation-specific standard clause established. |
| `calc_catenary` | Solve the catenary geometry for given horizontal tension and water depth. | `H`, `w_sub`, `depth` | Indirect through produced fixture: `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_produced_results_equal_golden_in_order_field_by_field` | No calculation-specific standard clause established. |
| `calc_sagbend_stress` | Calculate sagbend stress near the TDP. | `H`, `w_sub`, `od_m`, `wt_m`, `a_steel_m2`, `config` | Indirect through produced fixture: `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_produced_results_equal_golden_in_order_field_by_field` | DNV-ST-F101 (2021) §5; source assertion, clause title not specified. |
| `calc_overbend_stress` | Calculate overbend bending stress at the stinger. | `stinger_length_m`, `theta_dep_rad`, `od_m`, `config` | Indirect through produced fixture: `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_produced_results_equal_golden_in_order_field_by_field` | No calculation-specific standard clause established. |
| `find_required_tension` | Find the minimum horizontal tension H that satisfies all installation checks. | `pipe`, `depth`, `vessel_params`, `config` | Indirect through produced fixture: `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_produced_results_equal_golden_in_order_field_by_field` | No calculation-specific standard clause established. |
| `run_single_case` | Run all installation checks for a single vessel/pipe/depth combination. | `vessel`, `pipe`, `depth`, `config` | Indirect through produced fixture: `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_produced_results_equal_golden_in_order_field_by_field` | No calculation-specific standard clause established. |
| `run_parametric_sweep` | Run the full parametric sweep across all combinations. | `vessels`, `pipes`, `config` | `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_summary_matches_golden`; `examples/demos/gtm/tests/test_demo_04_results_store.py::test_baseline_cases_csv_matches_demo_output`; `examples/demos/gtm/tests/test_demo_04_results_store.py::test_baseline_rebuild_identity` | No calculation-specific standard clause established. |
| `build_chart_1_go_nogo_matrix` | Go/No-Go heatmap — side-by-side for two vessels. | `results_df`, `config` | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |
| `build_chart_2_sagbend_heatmap` | Sagbend utilisation heatmap — side-by-side per vessel. | `results_df`, `config` | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |
| `build_chart_3_tension_vs_depth` | Required top tension vs water depth — lines per pipe size, | `results_df`, `vessels`, `config` | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |
| `build_chart_4_departure_angle` | Stinger departure angle vs water depth — lines per pipe size, | `results_df`, `vessels`, `config` | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |
| `build_chart_5_vessel_comparison` | Grouped bar chart: max pipe size installable per depth per vessel. | `results_df`, `config` | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |
| `build_summary_table` | Build summary table for all cases. | `results_df`, `config` | `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_summary_matches_golden` | No calculation-specific standard clause established. |
| `build_report` | Build the branded HTML report. | `fig1`, `fig2`, `fig3`, `fig4`, `fig5`, `summary_df`, `all_results`, `total_cases`, `vessels`, `config`, `output_path` | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |
| `serialise_results` | Prepare results for JSON serialisation — strip non-serialisable types. | `all_results` | `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_sagbend_stress_basis_toggle_pinned`; `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_bending_only_24in_30m_lower_than_combined`; `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_edited_tension_margin_propagates_and_diverges`; `examples/demos/gtm/tests/test_demo_04_baseline_golden.py::test_edited_pipe_selection_wt_propagates` | No calculation-specific standard clause established. |
| `deserialise_results` | Reconstruct results list and DataFrame from JSON cache. | `json_data` | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |
| `lookup_main` | Performs lookup main; numerical criteria are not reproduced. | `argv` | `examples/demos/gtm/tests/test_demo_04_cli.py::test_lookup_vessel_id`; `examples/demos/gtm/tests/test_demo_04_cli.py::test_lookup_pipe_size`; `examples/demos/gtm/tests/test_demo_04_cli.py::test_lookup_water_depth_int_match`; `examples/demos/gtm/tests/test_demo_04_cli.py::test_lookup_status_marginal` | No calculation-specific standard clause established. |
| `rebuild_main` | rebuild-db subcommand: regenerate the SQLite cache + index.csv from the text source. | `argv` | `examples/demos/gtm/tests/test_demo_04_cli.py::test_rebuild_db_reports` | No calculation-specific standard clause established. |
| `main` | Run the full demo pipeline. | None | No direct named test identified; wrapper/CLI trace requires confirmation. | No calculation-specific standard clause established. |

Table 24. Demonstration public functions at the source revision; source presence and golden definitions are distinct from executed or native validation. The standalone mechanics should be reconciled with the library under the existing duplicate-implementation scope before shared production use.

## Gaps and limitations

- The earlier collection error was caused by explicitly selecting `tests/subsea/pipeline/test_on_bottom_stability.py`, which `tests/conftest.py` already ignores. It was a self-imposed stop, not evidence that default collection fails. Removing that explicit target yields 113 passing tests; no full-repository collection result is inferred.
- `SplashZoneInput.calculate_splash_zone_loads` converts the added-mass term before adding unconverted force terms, then converts the sum again (`orcaflex/installation_analysis.py:282-283`). The new component-sum test exposes this defect as a strict expected failure; existing sign/order tests continue to pass. The numerical fix remains separate work.
- `weld_repair_allowable_length` and `is_weld_repair_acceptable` treat a fractional allowance below a minimum differently. A strict expected-failure test records the unresolved contract with the reason "governing interpretation not established"; it selects no numerical acceptance criterion.
- J-lay vertical departure uses an assumed fallback for the horizontal component. The depth-invariant departure-angle table belongs to `SLayConfig`, not `JLayConfig`; its depth sweep is recorded as a strict expected failure. These screens require independent analytical and native-output evidence before design use.
- Reel-lay screening omits validated cyclic plasticity and ovalisation. Native results, installation fatigue histories and independent program benchmarking remain later work under [issue 140](https://github.com/vamseeachanta/digitalmodel/issues/140) and [issue 1658](https://github.com/vamseeachanta/digitalmodel/issues/1658).
- Standard-linked defaults, editions, clause titles and citation sidecars are incomplete. A source label is not a validated engineering basis.

## Tests and evidence

The corrected focused selection excludes the ignored legacy import test:

```sh
PYTHONPATH=src uv run --no-project \
  --with 'pytest<9' --with 'numpy<2' --with 'pandas<3' --with pydantic \
  --with pyyaml --with loguru --with munch --with 'deepdiff<8' --with jinja2 \
  --with 'plotly==5.17.0' --with scipy --with matplotlib --with openpyxl \
  --with requests --with colorama --with rich --with jsonschema --with rainflow \
  --with ruamel.yaml --with webcolors --with xlsxwriter --with tabulate \
  --with xmltodict --with python-docx --with seaborn --with pillow --with tqdm \
  --with beautifulsoup4 --with psutil --with scrapy --with selenium \
  --with imgkit --with pint --with pydantic-settings --with h5py --with sympy \
  python -m pytest \
  tests/installation/test_pipelay_stress.py \
  tests/installation/test_pipelay_integrity.py tests/pipelay \
  tests/orcaflex/test_pipelay_analysis.py \
  tests/orcaflex/test_installation_analysis.py tests/subsea/on_bottom_stability \
  tests/subsea/pipeline/test_api_rp_1111_installation.py \
  tests/test_on_bottom_stability.py \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/bt-focused"
```

The run used Python 3.12.3, pytest 8.4.2 and additional import dependencies supplied with `--with`; no project sync, lock update or solver installation was used. `tests/pipelay` and its example inputs were copied to external scratch and that copied test path replaced `tests/pipelay` in the command, because these workflow tests write beside their inputs. The calculations were imported from this checkout, with the existing companion utility source available. Outputs and pytest temporary directories remained in external scratch.

| Check | Before gap tests at the tested head | With gap tests | Limit |
|---|---|---|---|
| Corrected focused selection above | 113 passed | 113 passed, 3 xfailed | 116 items collected after adding tests; no unexpected failure or skip. |
| `tests/solvers/orcaflex/modular_generator/test_slay_builders.py` | 27 passed | 27 passed | Includes S-lay generation and floating-model regression; no native solve. |
| `tests/reporting/test_engine_golden.py` | 16 passed | 16 passed | Goldens were not regenerated. |

Table 25. Executed evidence at the tested head and on the uncommitted revision 3 changes.

Before adding the expected-failure markers, the three new tests collected and failed in their bodies: the splash-zone hook load was 641.2 kN versus the 700.5 kN component sum (0.2 kN rounding tolerance); the weld-repair case reported the unresolved interpretation; and the depth sweep returned one angle for multiple depths. No failure was caused by import. Each test now uses `pytest.mark.xfail(strict=True)` with a named gap. The two numerical checks restrict expected exceptions to `AssertionError`; the interpretation check explicitly fails after exercising both contracts and makes no numerical assertion. An unexpected pass fails the suite. These outcomes document unresolved gaps and do not establish engineering acceptance.

- `tests/orcaflex/test_installation_analysis.py::TestSplashZone.test_hook_load_equals_force_component_sum`
- `tests/installation/test_pipelay_integrity.py::test_weld_repair_minimum_fraction_interpretation_unresolved`
- `tests/orcaflex/test_pipelay_analysis.py::TestSLayConfig.test_departure_angle_recomputed_with_depth`

## External source readings

| Repository revision | Source opened | Use and limitation |
|---|---|---|
| Internal method notes (not public) | Method summaries | Context only; not publicly retrievable, licensed-standard qualification or measured outputs. |
| Internal source indexes (not public) | Source and dataset indexes | Existing source indexes inspected; no new dataset maintainer or source identity is inferred. |
| `worldenergydata@0306ce2d04e8e68f97f4084c256b6c998843729d` | `packages/worldenergydata-vessel_fleet/src/worldenergydata/vessel_fleet/_data/curated/construction_vessels.csv` | Pipelay method, tension, diameter and depth fields exist; no vessel data is imported or qualified for calculation. |
| `aceengineer-website@4f4371b903ac852a7260af9b70bb9e0690e4f5a5` | `content/demos/pipelay.html` | Existing screening/demo presentation; not an independent native result. No publication change. |
| `workspace-hub@bea0d5db18fbfe8d4fc80e0c67c1e018e1befa33` | `docs/gtm/capability-map.md`; `config/mission/mission-map.yaml` | S-lay is labelled Production; an installation-wide mission entry is absent. Proposed patches will describe partial analytical/model-generation capability and outstanding validation. |

Table 26. Read-only sources at named commits. Private method-page contents and source locator details are not reproduced.

Public issue records supply historical planning context, not validated results. Internal planning records (not public) are omitted from the public evidence basis.

## Way forward

Resolve the splash-zone unit defect under its component-sum test, establish the governing weld-repair interpretation, and qualify the departure-angle calculation before removing the corresponding expected-failure markers. Reuse the existing roller, section, campaign and substitution infrastructure for the remaining acceptance checks of #133, rather than treating it as absent. Establish the analytical comparator and qualified native outputs before claiming production installation capability. Establish source rights, governing criteria and results retention before any solver campaign. The existing model candidates require a provenance audit before reuse; no native validation is established here.
