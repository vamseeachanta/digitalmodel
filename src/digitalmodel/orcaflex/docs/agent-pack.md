# OrcaFlex top-level module knowledge pack

| Header | Value |
| --- | --- |
| Module | orcaflex, top-level Python files |
| Repository | digitalmodel |
| Path | `src/digitalmodel/orcaflex/` |
| Pack revision | 0, 2026-10-15 |
| Code revision | `1d130ba6c3d9a3b05eb7685b735a1ef6de0752a8` |
| Analysis family | OrcaFlex riser and mooring; MR |
| Family quality manager role | Riser and mooring quality manager |
| Status | DRAFT |

Code citations use the code revision above; implementation-map citations in section 9 use map commit `b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407`, whose data revision is `947fa50ff9a9e210de7e8f90e01b37add85c5954`. [docs/registry/implementation-map.yaml:L1-L4]

## 1. Purpose and scope

The public facade exports preliminary environment, object-building, mooring, riser, pipelay, installation, weather-window, parametric, code-check, VIV and post-processing utilities alongside reporting and QA entry points. [src/digitalmodel/orcaflex/__init__.py:L19-L159]

This pack covers Python files directly inside the header path; Subpackage internals are excluded and inventoried as dependencies in section 9. The routing registry labels this domain `production` and routes its tests to `tests/orcaflex/`; that label alone does not establish engineering qualification. [docs/registry/module-routing.yaml:L219-L224]

The analytical path works on configuration models and arrays; `build_scr_model`, `build_lazy_wave_model` and `build_mooring_line` return dictionaries, while the FOWT workflow is a preliminary circular-arc cable screen. [src/digitalmodel/orcaflex/model_builder.py:L259-L422] [src/digitalmodel/orcaflex/fowt_mooring_workflow.py:L1-L8]

`solve_catenary` is a single-segment, inextensible geometry estimate; `MooringLineDesign.estimate_catenary` replaces segment weights with their length-weighted average before delegating. It is not an independent segmented elastic reference for solver statics. [src/digitalmodel/orcaflex/mooring_design.py:L251-L335] [src/digitalmodel/orcaflex/mooring_design.py:L385-L403]

The unified riser schema supplies validation and legacy adapters; its environment block is informational, and it does not implement a licensed riser execution route. [src/digitalmodel/orcaflex/riser_input_schema.py:L23-L30] [src/digitalmodel/orcaflex/riser_input_schema.py:L110-L123]

UNVERIFIED: independent physical validation, a supported target solver version and broad applicability beyond the explicit idealisations have not been established here; section 10, items U2 and U3 track these limits.

## 2. Inputs and outputs

Input defaults below are implementation settings, not approved design-basis values; standards-derived factors and material-library numbers are deliberately referenced without reproducing their tables. [src/digitalmodel/orcaflex/mooring_design.py:L350-L378] [src/digitalmodel/orcaflex/data/synthetic_rope_profiles.yml:L1-L8]

| Input / entry point | Unit | Allowed range and default | Source and evidence |
| --- | --- | --- | --- |
| `solve_catenary(water_depth, line_length, submerged_weight_per_m, fairlead_depth, pretension)` | m; N/m; kN | Fairlead depth defaults to 0 m; pretension is optional; positive weight, line longer than vertical span, and pretension above suspended vertical weight are checked | Caller; [src/digitalmodel/orcaflex/mooring_design.py:L251-L308] |
| `MooringLineDesign` segments / tension / condition | m; kN; dimensionless factors | Positive depth, tension and segment lengths; nonnegative fairlead depth; conditions `intact` or `damaged`; material keys address the library | Caller and material library; [src/digitalmodel/orcaflex/mooring_design.py:L343-L378] [src/digitalmodel/orcaflex/mooring_design.py:L541-L563] |
| Unified riser YAML | m; kg/m³; deg | Discriminator SCR, SLWR, TTR, drilling or flexible; blocks reject extra keys; positive depth and dimensions; nonnegative coating, corrosion and contents density | `load_riser_input` and `validate_riser_input`; [src/digitalmodel/orcaflex/riser_input_schema.py:L56-L108] [src/digitalmodel/orcaflex/riser_input_schema.py:L282-L345] |
| Object builders: positions / section properties | m; kg/m; N·m²; N | Positive OD, mass, EA, length and target segment length; nonnegative ID and EI; configurations carry their own defaults | Caller; [src/digitalmodel/orcaflex/model_builder.py:L60-L91] [src/digitalmodel/orcaflex/model_builder.py:L98-L176] |
| Current / wave / wind | m; m/s; s; deg; m²/Hz | Nonnegative current and wind speeds; positive water depth, wave height and period; direction range is enforced on profile inputs | `CurrentProfile`, `WaveSpectrumParams`, `WindProfile`; [src/digitalmodel/orcaflex/environment.py:L54-L72] [src/digitalmodel/orcaflex/environment.py:L121-L131] [src/digitalmodel/orcaflex/environment.py:L191-L201] |
| Installation / pipelay | kN; kg; m; s; Pa; kg/m³ | Positive loads, stiffnesses and pipe dimensions; sling angle strictly between horizontal and vertical; departure angle measured from horizontal | `DAFInput`, `SlingConfig`, `SplashZoneInput`, `SLayConfig`; [src/digitalmodel/orcaflex/installation_analysis.py:L112-L122] [src/digitalmodel/orcaflex/installation_analysis.py:L185-L195] [src/digitalmodel/orcaflex/installation_analysis.py:L229-L243] [src/digitalmodel/orcaflex/pipelay_analysis.py:L98-L110] |
| Code-check arrays | m; kN; kN·m; MPa; pipe strengths in Pa | Caller supplies arc-length, tension and moment arrays; pressure arguments are optional; section 6 records the omitted pressure effect | Caller; [src/digitalmodel/orcaflex/code_check_engine.py:L103-L156] [src/digitalmodel/orcaflex/code_check_engine.py:L219-L268] |
| Weather / statistics / sweep | m; s; h; caller response unit | Persistence time step defaults to 3 h; scatter occurrence arrays are optional; sweeps use explicit values or inclusive start/stop/step | Caller; [src/digitalmodel/orcaflex/weather_window.py:L28-L44] [src/digitalmodel/orcaflex/weather_window.py:L177-L210] [src/digitalmodel/orcaflex/batch_parametric.py:L28-L51] |
| Synthetic rope history CSV | % MBL; event count | Required columns are checked; loads must be in the coded percentage bounds; empty and nonnumeric records raise | UTF-8 CSV, `LoadHistory.from_csv`; [src/digitalmodel/orcaflex/synthetic_rope_design.py:L23-L29] [src/digitalmodel/orcaflex/synthetic_rope_design.py:L51-L100] |
| FOWT cable / watch circle | m | All geometry and MBR inputs positive; touchdown is the origin; heave is excluded from the horizontal excursion calculation | Caller; [src/digitalmodel/orcaflex/mooring_design_fowt.py:L104-L134] [src/digitalmodel/orcaflex/mooring_design_fowt.py:L181-L215] |

| Output / producer | Units / format | Contract and evidence |
| --- | --- | --- |
| `CatenaryResult` | kN; m; deg from horizontal | Horizontal and top tensions, angle, suspended/grounded length and anchor radius, rounded by the function; [src/digitalmodel/orcaflex/mooring_design.py:L234-L248] [src/digitalmodel/orcaflex/mooring_design.py:L310-L335] |
| Riser geometry / weight | m; kN; N/m | SCR dictionaries, lazy-wave sizing, TTR stroke/tension and positive-sinking submerged weight; [src/digitalmodel/orcaflex/riser_config.py:L220-L229] [src/digitalmodel/orcaflex/riser_config.py:L312-L320] [src/digitalmodel/orcaflex/riser_config.py:L368-L395] [src/digitalmodel/orcaflex/riser_config.py:L402-L438] |
| Object / environment dictionaries | Python dict, pre-serialization | Configurations mix geometric SI inputs with explicit N-based section stiffnesses and kg-based mass; no native solver unit conversion is performed by these return statements; [src/digitalmodel/orcaflex/model_builder.py:L113-L125] [src/digitalmodel/orcaflex/model_builder.py:L147-L158] [src/digitalmodel/orcaflex/model_builder.py:L296-L312] [src/digitalmodel/orcaflex/environment.py:L107-L114] |
| Code checks | dimensionless utilisation, pass flag; m, kN, kN·m | Per-position result lists and mooring safety-factor result; [src/digitalmodel/orcaflex/code_check_engine.py:L93-L100] [src/digitalmodel/orcaflex/code_check_engine.py:L210-L216] [src/digitalmodel/orcaflex/code_check_engine.py:L305-L313] |
| Post-processing | Input response unit; ratios dimensionless; arc length m | Time-series summary, range graph, fitted extremes and multi-seed summary; [src/digitalmodel/orcaflex/postprocessor.py:L28-L44] [src/digitalmodel/orcaflex/postprocessor.py:L120-L158] [src/digitalmodel/orcaflex/postprocessor.py:L165-L174] [src/digitalmodel/orcaflex/postprocessor.py:L269-L282] |
| Parametric cases | DataFrame, CSV and dictionaries; units labelled by caller | Full-factorial cases, per-case output directories and summary metrics; [src/digitalmodel/orcaflex/batch_parametric.py:L78-L150] [src/digitalmodel/orcaflex/batch_parametric.py:L157-L167] |
| Rope results | EA in kN; creep in %; Citation sidecars | Material selection, stiffness, creep, fatigue severity and manufacturing/installation QA items; [src/digitalmodel/orcaflex/synthetic_rope_design.py:L267-L307] [src/digitalmodel/orcaflex/synthetic_rope_design.py:L310-L412] |
| FOWT / VIV workflow files | JSON and CSV; m, m/s, Hz, ratios | FOWT writes the configured summary; VIV writes `results.json` and `cases.csv` with citation metadata; [src/digitalmodel/orcaflex/fowt_mooring_workflow.py:L61-L68] [src/digitalmodel/orcaflex/viv_screening_workflow.py:L98-L178] |
| QA facade | Delegated object | Dynamically loads the repository documentation QA implementation and calls its facade; [src/digitalmodel/orcaflex/qa.py:L9-L39] |

Current depth is positive downward and current-point direction is going-to from north; SCR hang-off elevation is relative to mean sea level and its angle is from vertical. `CatenaryResult.top_angle` instead uses horizontal as its reference. [src/digitalmodel/orcaflex/environment.py:L54-L58] [src/digitalmodel/orcaflex/riser_config.py:L162-L168] [src/digitalmodel/orcaflex/mooring_design.py:L240-L248]

Spread/turret anchors use x = radius·sin(azimuth), y = radius·cos(azimuth), whereas `build_mooring_line` uses x = radius·cos(azimuth), y = radius·sin(azimuth); transferring an azimuth requires an explicit frame conversion. [src/digitalmodel/orcaflex/mooring_design.py:L618-L625] [src/digitalmodel/orcaflex/mooring_design.py:L649-L668] [src/digitalmodel/orcaflex/model_builder.py:L395-L410]

UNVERIFIED: a single target-program origin, heading/phase convention, supported solver-version range and native-file unit contract across these dictionaries have not been demonstrated; section 10 item U3 tracks this gap.

## 3. Method and governing standards

All standard locators below describe references in code, not independent confirmation of compliance; clause text not checked against the licensed copy. No tabulated standard value is reproduced.

| Method and implementing definitions | Reference as recorded, edition / locator | Evidence and citation sidecar |
| --- | --- | --- |
| `solve_catenary`; `MooringLineDesign.estimate_catenary`; `calculate_pretension` | Faltinsen, *Sea Loads on Ships and Offshore Structures*, 1990, chapter 6; geometry and force balance as coded | Direct hyperbolic geometry and weighted-average delegation; no sidecar on the catenary result; [src/digitalmodel/orcaflex/mooring_design.py:L1-L12] [src/digitalmodel/orcaflex/mooring_design.py:L251-L335] [src/digitalmodel/orcaflex/mooring_design.py:L385-L403] [src/digitalmodel/orcaflex/mooring_design.py:L696-L717] |
| `MooringLineDesign.check_mbl`; `check_mbl_with_safety_factor`; `SpreadMooringConfig.generate_layout`; `TurretMooringConfig.generate_layout`; `estimate_line_length` | API RP 2SK, *Design and Analysis of Stationkeeping Systems*, section 5 / section 5.4; DNV-OS-E301, *Position Mooring*, section 2.2.3; edition in citation registry `2021-07` | Legacy utilisation omits SF; explicit method multiplies by resolved SF and emits a Citation list; [src/digitalmodel/orcaflex/mooring_design.py:L405-L479] [src/digitalmodel/orcaflex/mooring_design.py:L522-L564] [src/digitalmodel/orcaflex/mooring_design.py:L572-L693] [src/digitalmodel/citations/registry.py:L26-L31] [src/digitalmodel/citations/registry.py:L94-L113] |
| `SCRDesignInput.calculate_geometry`; `LazyWaveDesignInput.size_buoyancy_section`; `TTRDesignInput.calculate_stroke` / `calculate_top_tension`; `RiserPipeProperties` area, mass, EI and EA properties; `calculate_weight_in_water`; `estimate_scr_hang_off_angle` | API RP 2RD, *Design of Risers for Floating Production Systems*, sections 4 and 5.2–5.4; Bai & Bai, *Subsea Pipelines and Risers*, 2005, chapters 16–18 | Inextensible SCR, approximate buoyancy sizing and additive motion/thermal stroke; no Citation sidecars from these methods; [src/digitalmodel/orcaflex/riser_config.py:L66-L150] [src/digitalmodel/orcaflex/riser_config.py:L157-L229] [src/digitalmodel/orcaflex/riser_config.py:L236-L320] [src/digitalmodel/orcaflex/riser_config.py:L327-L462] |
| `check_api_rp_2rd`; `APIRP2RDInput.plastic_moment` / `yield_tension`; `check_dnv_os_f201`; `DNVOSF201Input.characteristic_yield` / resistance properties; `check_mooring_api_2sk`; `UtilisationEnvelope` | API STD 2RD, *Dynamic Risers for Floating Production Systems*, second edition 2013; DNV-OS-F201, *Dynamic Risers*, 2010, section 5; API RP 2SK, 2005, section 6.2 / Table 1 | Array checks and envelope, without per-result Citation sidecars; pressure and capacity inconsistencies are in section 6; [src/digitalmodel/orcaflex/code_check_engine.py:L12-L18] [src/digitalmodel/orcaflex/code_check_engine.py:L59-L156] [src/digitalmodel/orcaflex/code_check_engine.py:L163-L268] [src/digitalmodel/orcaflex/code_check_engine.py:L275-L388] |
| `CurrentProfile.generate_profile` / `to_orcaflex_dict`; `WaveSpectrumParams.tz` / `spectral_density` / `to_orcaflex_dict`; `WindProfile.speed_at_height`; `EnvironmentalMatrix.generate_uls_cases` / `generate_als_cases` | DNV-RP-C205, *Environmental Conditions and Environmental Loads*, sections 7, 3.5 and 2.3; API RP 2MET, section 5; API RP 2SK, section 6.3 / Table 2; DNV-OS-E301, section 2; editions unspecified at these uses | Current and wind profile laws, normalised spectrum and environmental combinations; no emitted Citation objects; [src/digitalmodel/orcaflex/environment.py:L61-L114] [src/digitalmodel/orcaflex/environment.py:L121-L184] [src/digitalmodel/orcaflex/environment.py:L191-L223] [src/digitalmodel/orcaflex/environment.py:L247-L333] |
| `VesselConfig.to_orcaflex_dict`; `Buoy6DConfig.to_orcaflex_dict`; `LineConfig.to_orcaflex_dict`; `LineSectionProperties.weight_in_water_per_m`; `Buoy6DConfig.net_buoyancy`; `build_scr_model`; `build_lazy_wave_model`; `build_mooring_line` | OrcaFlex Object Data reference, version unspecified; API RP 2RD, section 5 / 5.3; API RP 2SK, section 5 | Dictionary generation and simplified geometric sizing; no native save/load attestation or Citation payload in these factories; [src/digitalmodel/orcaflex/model_builder.py:L60-L207] [src/digitalmodel/orcaflex/model_builder.py:L259-L422] |
| `VesselRAO.crane_tip_heave` / `crane_tip_velocity`; `DAFInput.calculate_daf`; `SlingConfig.calculate_sling_tension`; `SplashZoneInput.calculate_splash_zone_loads`; `WeightManagement.calculate_cog` / `generate_summary` | DNV-RP-H103 / DNV-RP-N103, *Modelling and Analysis of Marine Operations*, sections 4, 5.2 and 6; editions unspecified; lifting guideline references in source | SRSS motion, capped dynamic amplification, sling equilibrium, splash loads and weight accounting; [src/digitalmodel/orcaflex/installation_analysis.py:L50-L178] [src/digitalmodel/orcaflex/installation_analysis.py:L185-L298] [src/digitalmodel/orcaflex/installation_analysis.py:L305-L394] |
| `SLayConfig.calculate_overbend_stress` / `calculate_sagbend_stress` / `calculate_departure_angle_table`; `JLayConfig.calculate_sagbend`; `LayOperability.lay_rate_at_hs`; `calculate_stinger_radius` | Bai & Bai, *Subsea Pipeline Design, Analysis and Installation*, 2014, chapters 4–6; Mousselli, *Offshore Pipeline Design, Analysis and Methods*, 1981, chapter 5; DNV-OS-F101, *Submarine Pipeline Systems*, sections 5 / 11, edition unspecified | Elastic bending, catenary sag and preliminary rate reduction; no Citation sidecars; [src/digitalmodel/orcaflex/pipelay_analysis.py:L9-L12] [src/digitalmodel/orcaflex/pipelay_analysis.py:L98-L324] |
| `ScatterDiagram` generation / threshold sum; `OperabilityInput.calculate_operability`; `analyse_persistence`; `SeasonalOperability`; `WoWEstimate.estimate_wow_hours` | DNV-RP-C205, section 3.3; DNV-OS-H101, section 3; editions unspecified | Synthetic scatter when no occurrences are supplied, threshold counting and contiguous windows; [src/digitalmodel/orcaflex/weather_window.py:L28-L132] [src/digitalmodel/orcaflex/weather_window.py:L177-L234] [src/digitalmodel/orcaflex/weather_window.py:L241-L311] |
| `strouhal_number`; `VIVScreeningInput.check_reduced_velocity`; `BeamProperties.natural_frequency`; `viv_screening`; `estimate_response_amplitude` | DNV-RP-C205, 2019, section 9 / Table 9-1 / Figure 9-3; DNV-RP-F105, *Free Spanning Pipelines*, 2017, sections 4.3 / 5; Blevins, *Flow-Induced Vibrations*, 1990, Table 7-2 | Screening, beam frequency and empirical amplitude; adapter emits metadata sidecars; [src/digitalmodel/orcaflex/viv_screening.py:L9-L12] [src/digitalmodel/orcaflex/viv_screening.py:L45-L141] [src/digitalmodel/orcaflex/viv_screening.py:L148-L332] [src/digitalmodel/orcaflex/viv_screening_workflow.py:L43-L56] |
| `select_rope_material_result`; `axial_stiffness_for_load_history`; `creep_elongation`; `axial_compression_fatigue_check`; `qa_program` | API RP 2SM, full title unresolved, revision recorded as `2001+2007`, sections 4, 5.4–5.6, 7 / 8 | Citation-emitting bounded screening; coefficients are local defaults requiring replacement for final design; [src/digitalmodel/orcaflex/synthetic_rope_design.py:L155-L165] [src/digitalmodel/orcaflex/synthetic_rope_design.py:L233-L412] [src/digitalmodel/orcaflex/data/synthetic_rope_profiles.yml:L1-L8] |
| `WatchCircleConstraint.check`; `_arc_radius_from_chord` | Circular-arc chord relation and bisection; DNV-RP-0360, *Subsea power cables in shallow water*, metadata revision 2016, dynamic cable MBR | Fixed touchdown / elevation, horizontal excursion; optional citation emission fails softly; [src/digitalmodel/orcaflex/mooring_design_fowt.py:L15-L66] [src/digitalmodel/orcaflex/mooring_design_fowt.py:L188-L310] |
| `compute_time_series_stats`; `generate_range_graph`; `fit_gumbel`; `fit_weibull`; `process_multi_seed` | Arithmetic statistics; Gumbel, *Statistics of Extremes*, 1958; DNV-RP-C205, section 3.4; API RP 2SK, section 6.3; standard editions unspecified here | SciPy fits, response summaries and caller-unit results; Weibull fixes location at zero despite its three-parameter docstring; [src/digitalmodel/orcaflex/postprocessor.py:L47-L90] [src/digitalmodel/orcaflex/postprocessor.py:L120-L158] [src/digitalmodel/orcaflex/postprocessor.py:L177-L262] [src/digitalmodel/orcaflex/postprocessor.py:L285-L329] |
| `ParameterSweep.get_values`; `ParametricStudy.generate_case_matrix` / `generate_case_configs`; `ParametricResultsSummary.get_critical_case` | Full-factorial product and metric ranking | Minimum clearance selects the smallest completed value; other metrics select maxima; [src/digitalmodel/orcaflex/batch_parametric.py:L28-L111] [src/digitalmodel/orcaflex/batch_parametric.py:L170-L233] |

The riser helper still names RP 2RD without an edition, while the code-check engine identifies STD 2RD second edition 2013; the codes register explicitly preserves unresolved RP metadata and distinguishes OS-F201 from ST-F201. These identifiers must not be silently treated as interchangeable; #1247 and issue pending (1) track the unresolved provenance. [src/digitalmodel/orcaflex/riser_config.py:L12-L15] [src/digitalmodel/orcaflex/code_check_engine.py:L12-L18] [docs/domains/codes-register.md:L44-L68] [docs/domains/codes-register.md:L99-L105]

UNVERIFIED: publisher-confirmed editions and clause titles for the incomplete references, and full calc-citation coverage beyond the explicit sidecars above, remain open under section 10 item U1.
## 4. Validated worked examples

These cases qualify only the stated arithmetic or idealised geometry; passing them does not qualify an environmental load case, a material table or a solver model. [tests/orcaflex/test_postprocessor.py:L24-L37] [tests/orcaflex/test_pipelay_analysis.py:L118-L125]

| Case / classification | Independent source / input | Expected value and tolerance | Existing test node ID | Last offline result |
| --- | --- | --- | --- | --- |
| V1, validated arithmetic | Closed-form RMS definition applied to synthetic caller-unit samples `[3, 4]`; inline input | `sqrt(12.5)` response units, absolute tolerance `1e-4` response units, as asserted by the test | `tests/orcaflex/test_postprocessor.py::TestTimeSeriesStats::test_rms_calculation` | PASS at BASE and after; [tests/orcaflex/test_postprocessor.py:L33-L37] |
| V2, validated elastic geometry | Closed-form outer-fibre bending relation `strain = D/(2R)`; synthetic OD 0.3 m and caller-supplied strain 0.002; inline input | 75.0 m, absolute tolerance `7.5e-5` m measured from `pytest.approx(75.0).tolerance`; these are numerical test tolerances, not standard acceptance limits | `tests/orcaflex/test_pipelay_analysis.py::TestStingerRadius::test_basic_calculation` | PASS at BASE and after; [tests/orcaflex/test_pipelay_analysis.py:L118-L125] [src/digitalmodel/orcaflex/pipelay_analysis.py:L307-L324] |
| R1, regression workflow | `examples/workflows/fowt-mooring/input.yml`; circular-arc output asserted by the module's own smoke test, without an independent dynamic-cable reference | Governing radius 136.554 m and margin 132.054 m, each absolute tolerance 0.01 m; true pass flag | `tests/orcaflex/test_fowt_mooring_workflow.py::test_fowt_mooring_workflow_offline` | PASS at BASE and after; [examples/workflows/fowt-mooring/input.yml:L9-L26] [tests/orcaflex/test_fowt_mooring_workflow.py:L29-L53] |
| R2, regression registry route | Same synthetic input; workflow registry declares offline runtime | One declared summary file exists; returned basename is `fowt_mooring`; this test adds no independent expected physical value | `tests/workflows/test_durable_workflows.py::test_workflow_registry[fowt-mooring]` | PASS at BASE and after; [docs/registry/workflows.yaml:L510-L517] [tests/workflows/test_durable_workflows.py:L53-L65] |

The circular-arc unit-test preamble labels its result hand-verified, but its printed half-angle is inconsistent with the accompanying radius; it is not used as an independent qualification reference in this pack. [tests/orcaflex/test_mooring_design_fowt.py:L5-L12]

UNVERIFIED: a published, revision-qualified physical riser/mooring or dynamic-cable benchmark has not been demonstrated for these top-level helpers; U2 in section 10 tracks this gap.

## 5. QA gates

The applicable gate set is G0, G1, G2, G3 and G5; G4 is conditional on newly imported diffraction data. The following are proposed module-specific acceptance checks, not completed solver gate records. (AQG r0, sections 3–10.)

| Check | Gate / applicability | Executable check and numeric pass criterion with unit | Evidence / current result |
| --- | --- | --- | --- |
| Q0a | G0, applicable | `load_riser_input` must reject every invalid discriminator and extra field in the existing schema tests: 0 accepted invalid records, count | Offline node results; PASS for the existing test set; [tests/orcaflex/test_riser_input_schema.py:L70-L106] [src/digitalmodel/orcaflex/riser_input_schema.py:L296-L323] |
| Q0b | G0, applicable | For `build_scr_model`, `build_lazy_wave_model` and `build_mooring_line`, enumerate every referenced line type and connection: 0 unresolved references, count; serialize only after a target-version unit/name contract is recorded | Proposed native-dictionary boundary audit; NOT_RUN; issue pending (8); [src/digitalmodel/orcaflex/model_builder.py:L259-L422] |
| Q0c | G0, applicable | Recompute section lengths from `LineConfig.sections` and compare `total_length`: absolute difference ≤ 1e-9 m; before solver loading, require 0 invalid YAML tokens and 0 unresolved object keys, count | Existing section sum is testable offline; full native-data admission remains open; [src/digitalmodel/orcaflex/model_builder.py:L167-L207] |
| Q0d | G0, applicable | Recompute SHA-256 over sorted relative input-path/digest pairs for the exported model, includes and imported data: 0 digest mismatches, count; reject every missing provenance field, count = 0 | Proposed export manifest, not supplied by the dictionary factories; NOT_RUN; issue pending (8); [src/digitalmodel/orcaflex/model_builder.py:L296-L312] [src/digitalmodel/orcaflex/model_builder.py:L413-L422] |
| Q0e | G0, applicable | For `SpreadMooringConfig.generate_layout` and `ParametricStudy.create_directory_structure`, induce invalid catenary / unwritable output: 0 apparently successful fallback records, count | Proposed detection for silent fallbacks; current code does not meet this criterion; issue pending (7); [src/digitalmodel/orcaflex/mooring_design.py:L610-L627] [src/digitalmodel/orcaflex/batch_parametric.py:L121-L133] |
| Q0f | G0, applicable | Before `solve_catenary` / `WatchCircleConstraint.check`, require 0 outputs with suspended length greater than supplied line length or chord greater than supplied cable length, count | Proposed geometry admissibility check; current guards are incomplete; issue pending (6); [src/digitalmodel/orcaflex/mooring_design.py:L288-L335] [src/digitalmodel/orcaflex/mooring_design_fowt.py:L200-L215] [src/digitalmodel/orcaflex/mooring_design_fowt.py:L291-L297] |
| Q0g | G0, applicable | `WaveSpectrumParams.to_orcaflex_dict` must emit a target-version spectrum selector matching the requested spectrum, with 0 incompatible parameter keys, count; azimuth adapters must agree at 0° within 0.01 m | Proposed native-name/frame check; NOT_RUN against a program; issue pending (8); [src/digitalmodel/orcaflex/environment.py:L176-L184] [src/digitalmodel/orcaflex/model_builder.py:L395-L410] [src/digitalmodel/orcaflex/mooring_design.py:L618-L625] |
| Q1a | G1, applicable | OrcaFlex route: translate factory dictionaries to a complete native model, then `LoadData` in the declared target version, or use an equal-version demonstration program where supported; 0 errors and 0 warnings, count; object-count difference = 0 objects | NOT_RUN: version-qualified target program unavailable; native serialization and version attestation require issue pending (8); [src/digitalmodel/orcaflex/model_builder.py:L296-L312] |
| Q2a | G2, applicable | At zero environmental load, compare each native model line's top tension to an independently qualified catenary: error ≤ 5.0%; top angle after horizontal/vertical conversion: error ≤ 1.0°; touchdown distance error ≤ 5.0% | NOT_RUN: statics needs a solver; do not substitute this package's averaged catenary for an independent elastic reference; [src/digitalmodel/orcaflex/mooring_design.py:L385-L403] [src/digitalmodel/orcaflex/mooring_design.py:L240-L248] (AQG r0, section 6.) |
| Q2b | G2, applicable | For each generated distinct configuration, use at least 6 offset steps, count; 0 unconverged states, 0 unexpected slack/compression states and 0 unchecked components, count; first-step restoring force within 20.0% of the independent estimate | NOT_RUN: licensed statics and independent reference absent; issue pending (8); [src/digitalmodel/orcaflex/model_builder.py:L370-L422] (AQG r0, section 6.) |
| Q2c | G2, applicable analytical check | `compute_time_series_stats([3,4]).rms` must match `sqrt(12.5)` within 1e-4 response units, and `calculate_stinger_radius(0.3,0.002)` must match 75.0 m within pytest's existing tolerance | V1 and V2 PASS; [tests/orcaflex/test_postprocessor.py:L33-L37] [tests/orcaflex/test_pipelay_analysis.py:L121-L125] |
| Q2d | G2, applicable analytical check | `SplashZoneInput.calculate_splash_zone_loads`: maximum hook load minus dry weight must equal the sum of the three reported dynamic-force components within 0.2 kN, allowing their rounding | Proposed balance detects the double conversion; current default result fails; issue pending (3); [src/digitalmodel/orcaflex/installation_analysis.py:L277-L297] |
| Q2e | G2, applicable analytical check | `check_api_rp_2rd` and `check_dnv_os_f201`: pressure-sensitive benchmark must show a nonzero utilisation change for a nonzero pressure increment, dimensionless difference > 0.0; capacity used by the API array check must agree with `plastic_moment` within 1e-9 N·m | Proposed detections; current implementation omits pressure contribution and uses differing plastic capacities; issues pending (4) and (5); [src/digitalmodel/orcaflex/code_check_engine.py:L79-L85] [src/digitalmodel/orcaflex/code_check_engine.py:L127-L153] [src/digitalmodel/orcaflex/code_check_engine.py:L243-L268] |
| Q3a | G3, applicable | From converged `build_scr_model` / `build_mooring_line` native cases, complete 1.0 s at production settings; 0 nonfinite responses and 0 warnings/errors, count; line-tension change ≤ 25.0% from statics | NOT_RUN: licensed dynamics absent; issue pending (8); [src/digitalmodel/orcaflex/model_builder.py:L259-L312] [src/digitalmodel/orcaflex/model_builder.py:L370-L422] (AQG r0, section 7.) |
| Q4a | G4, conditional | For new diffraction data attached to `VesselConfig`, cite a G4 record with 0 unresolved input-fingerprint links, count; weight/buoyancy difference ≤ 1.0%, independently integrated volume difference ≤ 0.5% | NOT_APPLICABLE to the synthetic scalar RAO/dictionary examples, which do not import a new diffraction run; any later import activates this check; [src/digitalmodel/orcaflex/model_builder.py:L98-L125] [src/digitalmodel/orcaflex/installation_analysis.py:L50-L65] (AQG r0, sections 8 and 10.) |
| Q5a | G5, applicable | For `run_orcaflex_qa` output and any downstream report, require every claimed result's fingerprint and gate record to resolve: 0 missing evidence links, count; 0 unsigned applicable gates, count | NOT_RUN: facade delegates and emits no AQG evidence bundle itself; issue pending (2); [src/digitalmodel/orcaflex/qa.py:L29-L39] (AQG r0, section 9.) |
| Q5b | G5, applicable | `check_watch_circle_vs_cable` and SF-adjusted MBL outputs used as standards-cited evidence must carry at least 1 resolved citation, count; 0 silently missing pages, count | Proposed citation completeness check; #2018 and #1247 remain open; [src/digitalmodel/orcaflex/mooring_design_fowt.py:L231-L271] [src/digitalmodel/orcaflex/mooring_design.py:L451-L479] [src/digitalmodel/orcaflex/mooring_design.py:L559-L564] |

Checks feeding G0 also require the AQG parse, selector-order, starting-shape, units and provenance checks for every complete native input; dictionary-only unit tests do not supply those records. (AQG r0, section 4.)

## 6. Known failure modes and pitfalls

The entries below distinguish source-confirmed behavior from an executed detection; proposed checks have no new committed tests in this documentation change.

| Symptom / cause | Detection | Incident or follow-up / date | Source evidence |
| --- | --- | --- | --- |
| Spread layout silently substitutes a length-derived anchor radius after any catenary exception; output-directory creation reports paths even after an OS error | Q0e; no detection check exists in the current tests for the failure signal | issue pending (7), 2026-10-15 | [src/digitalmodel/orcaflex/mooring_design.py:L610-L627] [src/digitalmodel/orcaflex/batch_parametric.py:L121-L133] |
| FOWT citation emission returns an empty list when a resolved tree lacks the page; missing-tree mode warns once, and the numeric check still proceeds | Q5b; no detection check exists in the FOWT tests for a populated tree with a missing page | #2018; 2026-10-15 source confirmation | [src/digitalmodel/orcaflex/mooring_design_fowt.py:L231-L271] |
| Range graphs silently fill missing position columns with zeros; weather operability only uses Hs/Tp despite accepting wind/current limits | Q0e extended to reject missing response columns / unsupported active criteria; no detection check exists for these omissions | issue pending (7), 2026-10-15 | [src/digitalmodel/orcaflex/postprocessor.py:L135-L149] [src/digitalmodel/orcaflex/weather_window.py:L108-L132] |
| Non-JONSWAP wave dictionary requests emit `Dean stream` and a `None` gamma; native compatibility and scalar-unit conversion are not demonstrated | Q0b / Q0g / Q1a; current dictionary tests do not load a native program | issue pending (8), 2026-10-15 | [src/digitalmodel/orcaflex/environment.py:L176-L184] [src/digitalmodel/orcaflex/model_builder.py:L296-L312] |
| Added-mass force is divided before addition to Newton-valued splash/drag forces and then divided again, understating its hook-load contribution | Q2d; no detection check exists for force-component closure in the current installation tests | issue pending (3), 2026-10-15 | [src/digitalmodel/orcaflex/installation_analysis.py:L277-L297] |
| API pressure is copied into the result but omitted from utilisation; DNV pressure arguments are not consumed by its calculation | Q2e; no detection check exists for pressure sensitivity in the current code-check tests | issue pending (4), 2026-10-15 | [src/digitalmodel/orcaflex/code_check_engine.py:L127-L156] [src/digitalmodel/orcaflex/code_check_engine.py:L219-L268] |
| `APIRP2RDInput.plastic_moment` differs algebraically from the capacity recomputed inside the array checker | Q2e capacity comparison; no detection check exists for equality in the current code-check tests | issue pending (5), 2026-10-15 | [src/digitalmodel/orcaflex/code_check_engine.py:L79-L85] [src/digitalmodel/orcaflex/code_check_engine.py:L130-L142] |
| Catenary pretension can require more suspended length than the supplied line; a cable chord exceeding arc length returns infinite radius and can pass | Q0f; the existing straight-cable test explicitly accepts chord greater than length, so it is regression evidence rather than physical validation | issue pending (6), 2026-10-15 | [src/digitalmodel/orcaflex/mooring_design.py:L288-L335] [src/digitalmodel/orcaflex/mooring_design_fowt.py:L200-L215] [src/digitalmodel/orcaflex/mooring_design_fowt.py:L291-L297] [tests/orcaflex/test_mooring_design_fowt.py:L38-L40] |
| Code-check and riser helpers have docstring references without complete emitted standard provenance | Q5b and standards audit; current numerical results alone do not detect missing metadata | #1247 and issue pending (1), 2026-10-15 | [src/digitalmodel/orcaflex/code_check_engine.py:L147-L156] [src/digitalmodel/orcaflex/riser_config.py:L220-L229] |
| `fit_weibull` fixes location at zero despite the three-parameter description; `process_multi_seed` preamble gives the opposite sign from its mode computation; FOWT test preamble has an inconsistent half-angle | Documentation/source consistency audit; no detection check exists for these prose discrepancies | issue pending (2), 2026-10-15 | [src/digitalmodel/orcaflex/postprocessor.py:L223-L244] [src/digitalmodel/orcaflex/postprocessor.py:L294-L317] [tests/orcaflex/test_mooring_design_fowt.py:L5-L12] |

The API names retained for compatibility must not be used to infer a standard edition or treat these simplified checks as full pressure-sensitive acceptance calculations. [src/digitalmodel/orcaflex/code_check_engine.py:L3-L18] [docs/domains/codes-register.md:L99-L105]
## 7. Lessons log

| Date | Record ID | Lesson / evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- |
| 2026-10-15 | initial-pack | A passing scalar smoke test does not test dynamic-force closure; the splash routine converts the added-mass term twice | Added Q2d and the section-6 failure mode | Unassigned; issue pending (2); [src/digitalmodel/orcaflex/installation_analysis.py:L277-L297] |

## 8. Ownership

The designated family role is the riser and mooring quality manager; the deputy role is a second riser and mooring quality reviewer. These are responsibility slots for this draft, not records of clearance or named appointments.

UNVERIFIED: deputy appointment, producing engineering desk and shared-log identifier are not established by source; the maintainer assigns them through issue pending (2), section 10 item U4.

Method ambiguities, tolerance changes and unresolved checks go to the family quality manager; cross-family scope decisions go to the maintainer. This is the escalation rule for this draft.

## 9. Dependencies

### Runtime and repository boundaries

| Boundary | Dependency / role | Evidence |
| --- | --- | --- |
| Analytical models | NumPy, pandas, SciPy, Pydantic and PyYAML | Numerical arrays, DataFrames, distribution fits and input models; [src/digitalmodel/orcaflex/postprocessor.py:L15-L21] [src/digitalmodel/orcaflex/environment.py:L17-L22] [src/digitalmodel/orcaflex/mooring_design.py:L21-L29] |
| Material and citation infrastructure | `digitalmodel.materials`, `digitalmodel.citations`; material-library data | Riser grade values are obtained through the material registry; synthetic-rope profiles are read from package data; [src/digitalmodel/orcaflex/riser_config.py:L25-L59] [src/digitalmodel/orcaflex/mooring_design.py:L141-L170] |
| Engine adapters | `FOWTMooringWorkflow`, `MooringMBLWorkflow`, `VIVParametricScreeningWorkflow` | Adapters delegate to the analytical functions, then serialize or return results; [src/digitalmodel/orcaflex/fowt_mooring_workflow.py:L25-L42] [src/digitalmodel/orcaflex/mooring_workflow.py:L27-L50] [src/digitalmodel/orcaflex/viv_screening_workflow.py:L59-L95] |
| Reporting subpackage | Separate pack scope; real result loading uses OrcFxAPI | The root imports the reporting facade; its deferred loader constructs a model and calls `LoadSimulation`; [src/digitalmodel/orcaflex/__init__.py:L19-L20] [src/digitalmodel/orcaflex/reporting/builder.py:L81-L97] |
| Documentation QA | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | Facade dynamically loads this implementation; that script has an interface import at module scope, read as text only; [src/digitalmodel/orcaflex/qa.py:L9-L25] [docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py:L27-L46] |
| Workflow registry | `fowt-mooring` | Registered input, summary output, test node and offline runtime; [docs/registry/workflows.yaml:L510-L517] |

UNVERIFIED: dependency inventory-row identifiers and supported solver versions are not established by these imports; section 10 item U3 retains that limitation.

### Explicit solver-name references

The following inventory enumerates every case-sensitive `OrcaFlex` or `OrcFxAPI` occurrence line in the directly scoped source; declarations of interface-free operation are distinguished from the reporting/QA boundary rather than treated as a solver launch.

| Source line | Dependency classification | Citation |
| --- | --- | --- |
| `__init__.py:1` | Facade documentation or reporting export context | [src/digitalmodel/orcaflex/__init__.py:L1-L1] |
| `__init__.py:3` | Facade documentation or reporting export context | [src/digitalmodel/orcaflex/__init__.py:L3-L3] |
| `__init__.py:4` | Facade documentation or reporting export context | [src/digitalmodel/orcaflex/__init__.py:L4-L4] |
| `__init__.py:6` | Facade documentation or reporting export context | [src/digitalmodel/orcaflex/__init__.py:L6-L6] |
| `__init__.py:22` | Facade documentation or reporting export context | [src/digitalmodel/orcaflex/__init__.py:L22-L22] |
| `batch_parametric.py:1` | Analytical scope or downstream solver context | [src/digitalmodel/orcaflex/batch_parametric.py:L1-L1] |
| `batch_parametric.py:7` | Interface-free declaration | [src/digitalmodel/orcaflex/batch_parametric.py:L7-L7] |
| `code_check_engine.py:10` | Interface-free declaration | [src/digitalmodel/orcaflex/code_check_engine.py:L10-L10] |
| `environment.py:1` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/environment.py:L1-L1] |
| `environment.py:13` | Interface-free declaration | [src/digitalmodel/orcaflex/environment.py:L13-L13] |
| `environment.py:14` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/environment.py:L14-L14] |
| `environment.py:108` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/environment.py:L108-L108] |
| `environment.py:177` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/environment.py:L177-L177] |
| `fowt_mooring_workflow.py:6` | Analytical scope or downstream solver context | [src/digitalmodel/orcaflex/fowt_mooring_workflow.py:L6-L6] |
| `installation_analysis.py:1` | Analytical scope or downstream solver context | [src/digitalmodel/orcaflex/installation_analysis.py:L1-L1] |
| `installation_analysis.py:7` | Interface-free declaration | [src/digitalmodel/orcaflex/installation_analysis.py:L7-L7] |
| `model_builder.py:1` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/model_builder.py:L1-L1] |
| `model_builder.py:3` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/model_builder.py:L3-L3] |
| `model_builder.py:6` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/model_builder.py:L6-L6] |
| `model_builder.py:8` | Interface-free declaration | [src/digitalmodel/orcaflex/model_builder.py:L8-L8] |
| `model_builder.py:11` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/model_builder.py:L11-L11] |
| `model_builder.py:29` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/model_builder.py:L29-L29] |
| `model_builder.py:64` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/model_builder.py:L64-L64] |
| `model_builder.py:101` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/model_builder.py:L101-L101] |
| `model_builder.py:114` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/model_builder.py:L114-L114] |
| `model_builder.py:148` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/model_builder.py:L148-L148] |
| `model_builder.py:164` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/model_builder.py:L164-L164] |
| `model_builder.py:184` | Configuration dictionary / object-data reference | [src/digitalmodel/orcaflex/model_builder.py:L184-L184] |
| `mooring_design.py:7` | Interface-free declaration | [src/digitalmodel/orcaflex/mooring_design.py:L7-L7] |
| `pipelay_analysis.py:1` | Analytical scope or downstream solver context | [src/digitalmodel/orcaflex/pipelay_analysis.py:L1-L1] |
| `pipelay_analysis.py:7` | Interface-free declaration | [src/digitalmodel/orcaflex/pipelay_analysis.py:L7-L7] |
| `postprocessor.py:1` | Analytical scope or downstream solver context | [src/digitalmodel/orcaflex/postprocessor.py:L1-L1] |
| `postprocessor.py:7` | Interface-free declaration | [src/digitalmodel/orcaflex/postprocessor.py:L7-L7] |
| `postprocessor.py:272` | Analytical scope or downstream solver context | [src/digitalmodel/orcaflex/postprocessor.py:L272-L272] |
| `qa.py:1` | Documentation QA delegation | [src/digitalmodel/orcaflex/qa.py:L1-L1] |
| `qa.py:30` | Documentation QA delegation | [src/digitalmodel/orcaflex/qa.py:L30-L30] |
| `riser_config.py:4` | Analytical scope or downstream solver context | [src/digitalmodel/orcaflex/riser_config.py:L4-L4] |
| `riser_config.py:10` | Interface-free declaration | [src/digitalmodel/orcaflex/riser_config.py:L10-L10] |
| `riser_input_schema.py:25` | External execution-script dependency note | [src/digitalmodel/orcaflex/riser_input_schema.py:L25-L25] |
| `viv_screening.py:7` | Interface-free declaration | [src/digitalmodel/orcaflex/viv_screening.py:L7-L7] |
| `weather_window.py:7` | Interface-free declaration | [src/digitalmodel/orcaflex/weather_window.py:L7-L7] |

### Implementation-map inventory

Each inventory identifier below is the family ID plus its one-based implementation-row ordinal in the pinned map. Each citation resolves the complete implementation record, including its path, symbol, relation and recorded evidence; an ordinal is not a newly assigned implementation identity. All rows are listed, including rows outside the header package and under `scripts/`; only direct-package rows contribute to the family ranking.

| Family | In-package rows | All map rows | Counted search |
| --- | --- | --- | --- |
| `catenary-mooring-line-statics` | 4 | 137 | no |
| `effective-tension-riser-stack-up` | 7 | 264 | yes |
| `orcaflex-text-model-writers` | 6 | 440 | yes |
| `wave-spectra` | 3 | 139 | no |
| `wind-and-current-force-routes` | 1 | 131 | no |
| `api-5ct-5l-grade-tables` | 9 | 39 | yes |

The counts above are a projection of the complete row inventory below at the map pin; they do not reclassify the map’s broad families.

<details>
<summary>catenary-mooring-line-statics: all 137 implementation rows</summary>

| Map row | Package boundary | Definition / implementation locator |
| --- | --- | --- |
| `catenary-mooring-line-statics/001` | outside | [docs/registry/implementation-map.yaml:L57-L67] |
| `catenary-mooring-line-statics/002` | outside | [docs/registry/implementation-map.yaml:L68-L78] |
| `catenary-mooring-line-statics/003` | outside | [docs/registry/implementation-map.yaml:L79-L89] |
| `catenary-mooring-line-statics/004` | outside | [docs/registry/implementation-map.yaml:L90-L100] |
| `catenary-mooring-line-statics/005` | outside | [docs/registry/implementation-map.yaml:L101-L113] |
| `catenary-mooring-line-statics/006` | outside | [docs/registry/implementation-map.yaml:L114-L124] |
| `catenary-mooring-line-statics/007` | outside | [docs/registry/implementation-map.yaml:L125-L135] |
| `catenary-mooring-line-statics/008` | outside | [docs/registry/implementation-map.yaml:L136-L146] |
| `catenary-mooring-line-statics/009` | outside | [docs/registry/implementation-map.yaml:L147-L157] |
| `catenary-mooring-line-statics/010` | outside | [docs/registry/implementation-map.yaml:L158-L168] |
| `catenary-mooring-line-statics/011` | outside | [docs/registry/implementation-map.yaml:L169-L179] |
| `catenary-mooring-line-statics/012` | outside | [docs/registry/implementation-map.yaml:L180-L192] |
| `catenary-mooring-line-statics/013` | outside | [docs/registry/implementation-map.yaml:L193-L204] |
| `catenary-mooring-line-statics/014` | outside | [docs/registry/implementation-map.yaml:L205-L215] |
| `catenary-mooring-line-statics/015` | outside | [docs/registry/implementation-map.yaml:L216-L226] |
| `catenary-mooring-line-statics/016` | outside | [docs/registry/implementation-map.yaml:L227-L237] |
| `catenary-mooring-line-statics/017` | outside | [docs/registry/implementation-map.yaml:L238-L248] |
| `catenary-mooring-line-statics/018` | outside | [docs/registry/implementation-map.yaml:L249-L259] |
| `catenary-mooring-line-statics/019` | outside | [docs/registry/implementation-map.yaml:L260-L270] |
| `catenary-mooring-line-statics/020` | outside | [docs/registry/implementation-map.yaml:L271-L281] |
| `catenary-mooring-line-statics/021` | outside | [docs/registry/implementation-map.yaml:L282-L292] |
| `catenary-mooring-line-statics/022` | outside | [docs/registry/implementation-map.yaml:L293-L303] |
| `catenary-mooring-line-statics/023` | outside | [docs/registry/implementation-map.yaml:L304-L314] |
| `catenary-mooring-line-statics/024` | outside | [docs/registry/implementation-map.yaml:L315-L327] |
| `catenary-mooring-line-statics/025` | outside | [docs/registry/implementation-map.yaml:L328-L340] |
| `catenary-mooring-line-statics/026` | outside | [docs/registry/implementation-map.yaml:L341-L351] |
| `catenary-mooring-line-statics/027` | outside | [docs/registry/implementation-map.yaml:L352-L362] |
| `catenary-mooring-line-statics/028` | outside | [docs/registry/implementation-map.yaml:L363-L373] |
| `catenary-mooring-line-statics/029` | outside | [docs/registry/implementation-map.yaml:L374-L384] |
| `catenary-mooring-line-statics/030` | outside | [docs/registry/implementation-map.yaml:L385-L397] |
| `catenary-mooring-line-statics/031` | outside | [docs/registry/implementation-map.yaml:L398-L408] |
| `catenary-mooring-line-statics/032` | outside | [docs/registry/implementation-map.yaml:L409-L419] |
| `catenary-mooring-line-statics/033` | outside | [docs/registry/implementation-map.yaml:L420-L430] |
| `catenary-mooring-line-statics/034` | outside | [docs/registry/implementation-map.yaml:L431-L441] |
| `catenary-mooring-line-statics/035` | outside | [docs/registry/implementation-map.yaml:L442-L452] |
| `catenary-mooring-line-statics/036` | outside | [docs/registry/implementation-map.yaml:L453-L463] |
| `catenary-mooring-line-statics/037` | outside | [docs/registry/implementation-map.yaml:L464-L474] |
| `catenary-mooring-line-statics/038` | outside | [docs/registry/implementation-map.yaml:L475-L485] |
| `catenary-mooring-line-statics/039` | outside | [docs/registry/implementation-map.yaml:L486-L498] |
| `catenary-mooring-line-statics/040` | outside | [docs/registry/implementation-map.yaml:L499-L509] |
| `catenary-mooring-line-statics/041` | outside | [docs/registry/implementation-map.yaml:L510-L520] |
| `catenary-mooring-line-statics/042` | outside | [docs/registry/implementation-map.yaml:L521-L531] |
| `catenary-mooring-line-statics/043` | outside | [docs/registry/implementation-map.yaml:L532-L542] |
| `catenary-mooring-line-statics/044` | outside | [docs/registry/implementation-map.yaml:L543-L553] |
| `catenary-mooring-line-statics/045` | outside | [docs/registry/implementation-map.yaml:L554-L564] |
| `catenary-mooring-line-statics/046` | outside | [docs/registry/implementation-map.yaml:L565-L575] |
| `catenary-mooring-line-statics/047` | outside | [docs/registry/implementation-map.yaml:L576-L588] |
| `catenary-mooring-line-statics/048` | outside | [docs/registry/implementation-map.yaml:L589-L599] |
| `catenary-mooring-line-statics/049` | outside | [docs/registry/implementation-map.yaml:L600-L612] |
| `catenary-mooring-line-statics/050` | outside | [docs/registry/implementation-map.yaml:L613-L623] |
| `catenary-mooring-line-statics/051` | outside | [docs/registry/implementation-map.yaml:L624-L634] |
| `catenary-mooring-line-statics/052` | outside | [docs/registry/implementation-map.yaml:L635-L645] |
| `catenary-mooring-line-statics/053` | inside | `src/digitalmodel/orcaflex/mooring_design.py::solve_catenary`; [docs/registry/implementation-map.yaml:L646-L656] |
| `catenary-mooring-line-statics/054` | inside | `src/digitalmodel/orcaflex/mooring_design.py::MooringLineDesign.estimate_catenary`; [docs/registry/implementation-map.yaml:L657-L669] |
| `catenary-mooring-line-statics/055` | inside | `src/digitalmodel/orcaflex/pipelay_analysis.py::JLayConfig.calculate_sagbend`; [docs/registry/implementation-map.yaml:L670-L680] |
| `catenary-mooring-line-statics/056` | inside | `src/digitalmodel/orcaflex/riser_config.py::SCRDesignInput.calculate_geometry`; [docs/registry/implementation-map.yaml:L681-L691] |
| `catenary-mooring-line-statics/057` | outside | [docs/registry/implementation-map.yaml:L692-L702] |
| `catenary-mooring-line-statics/058` | outside | [docs/registry/implementation-map.yaml:L703-L713] |
| `catenary-mooring-line-statics/059` | outside | [docs/registry/implementation-map.yaml:L714-L724] |
| `catenary-mooring-line-statics/060` | outside | [docs/registry/implementation-map.yaml:L725-L735] |
| `catenary-mooring-line-statics/061` | outside | [docs/registry/implementation-map.yaml:L736-L746] |
| `catenary-mooring-line-statics/062` | outside | [docs/registry/implementation-map.yaml:L747-L757] |
| `catenary-mooring-line-statics/063` | outside | [docs/registry/implementation-map.yaml:L758-L768] |
| `catenary-mooring-line-statics/064` | outside | [docs/registry/implementation-map.yaml:L769-L779] |
| `catenary-mooring-line-statics/065` | outside | [docs/registry/implementation-map.yaml:L780-L790] |
| `catenary-mooring-line-statics/066` | outside | [docs/registry/implementation-map.yaml:L791-L804] |
| `catenary-mooring-line-statics/067` | outside | [docs/registry/implementation-map.yaml:L805-L817] |
| `catenary-mooring-line-statics/068` | outside | [docs/registry/implementation-map.yaml:L818-L828] |
| `catenary-mooring-line-statics/069` | outside | [docs/registry/implementation-map.yaml:L829-L844] |
| `catenary-mooring-line-statics/070` | outside | [docs/registry/implementation-map.yaml:L845-L857] |
| `catenary-mooring-line-statics/071` | outside | [docs/registry/implementation-map.yaml:L858-L868] |
| `catenary-mooring-line-statics/072` | outside | [docs/registry/implementation-map.yaml:L869-L882] |
| `catenary-mooring-line-statics/073` | outside | [docs/registry/implementation-map.yaml:L883-L893] |
| `catenary-mooring-line-statics/074` | outside | [docs/registry/implementation-map.yaml:L894-L904] |
| `catenary-mooring-line-statics/075` | outside | [docs/registry/implementation-map.yaml:L905-L915] |
| `catenary-mooring-line-statics/076` | outside | [docs/registry/implementation-map.yaml:L916-L926] |
| `catenary-mooring-line-statics/077` | outside | [docs/registry/implementation-map.yaml:L927-L939] |
| `catenary-mooring-line-statics/078` | outside | [docs/registry/implementation-map.yaml:L940-L950] |
| `catenary-mooring-line-statics/079` | outside | [docs/registry/implementation-map.yaml:L951-L961] |
| `catenary-mooring-line-statics/080` | outside | [docs/registry/implementation-map.yaml:L962-L972] |
| `catenary-mooring-line-statics/081` | outside | [docs/registry/implementation-map.yaml:L973-L983] |
| `catenary-mooring-line-statics/082` | outside | [docs/registry/implementation-map.yaml:L984-L994] |
| `catenary-mooring-line-statics/083` | outside | [docs/registry/implementation-map.yaml:L995-L1007] |
| `catenary-mooring-line-statics/084` | outside | [docs/registry/implementation-map.yaml:L1008-L1018] |
| `catenary-mooring-line-statics/085` | outside | [docs/registry/implementation-map.yaml:L1019-L1029] |
| `catenary-mooring-line-statics/086` | outside | [docs/registry/implementation-map.yaml:L1030-L1042] |
| `catenary-mooring-line-statics/087` | outside | [docs/registry/implementation-map.yaml:L1043-L1053] |
| `catenary-mooring-line-statics/088` | outside | [docs/registry/implementation-map.yaml:L1054-L1068] |
| `catenary-mooring-line-statics/089` | outside | [docs/registry/implementation-map.yaml:L1069-L1079] |
| `catenary-mooring-line-statics/090` | outside | [docs/registry/implementation-map.yaml:L1080-L1090] |
| `catenary-mooring-line-statics/091` | outside | [docs/registry/implementation-map.yaml:L1091-L1101] |
| `catenary-mooring-line-statics/092` | outside | [docs/registry/implementation-map.yaml:L1102-L1114] |
| `catenary-mooring-line-statics/093` | outside | [docs/registry/implementation-map.yaml:L1115-L1125] |
| `catenary-mooring-line-statics/094` | outside | [docs/registry/implementation-map.yaml:L1126-L1136] |
| `catenary-mooring-line-statics/095` | outside | [docs/registry/implementation-map.yaml:L1137-L1149] |
| `catenary-mooring-line-statics/096` | outside | [docs/registry/implementation-map.yaml:L1150-L1160] |
| `catenary-mooring-line-statics/097` | outside | [docs/registry/implementation-map.yaml:L1161-L1171] |
| `catenary-mooring-line-statics/098` | outside | [docs/registry/implementation-map.yaml:L1172-L1182] |
| `catenary-mooring-line-statics/099` | outside | [docs/registry/implementation-map.yaml:L1183-L1193] |
| `catenary-mooring-line-statics/100` | outside | [docs/registry/implementation-map.yaml:L1194-L1204] |
| `catenary-mooring-line-statics/101` | outside | [docs/registry/implementation-map.yaml:L1205-L1215] |
| `catenary-mooring-line-statics/102` | outside | [docs/registry/implementation-map.yaml:L1216-L1228] |
| `catenary-mooring-line-statics/103` | outside | [docs/registry/implementation-map.yaml:L1229-L1241] |
| `catenary-mooring-line-statics/104` | outside | [docs/registry/implementation-map.yaml:L1242-L1256] |
| `catenary-mooring-line-statics/105` | outside | [docs/registry/implementation-map.yaml:L1257-L1269] |
| `catenary-mooring-line-statics/106` | outside | [docs/registry/implementation-map.yaml:L1270-L1282] |
| `catenary-mooring-line-statics/107` | outside | [docs/registry/implementation-map.yaml:L1283-L1297] |
| `catenary-mooring-line-statics/108` | outside | [docs/registry/implementation-map.yaml:L1298-L1310] |
| `catenary-mooring-line-statics/109` | outside | [docs/registry/implementation-map.yaml:L1311-L1323] |
| `catenary-mooring-line-statics/110` | outside | [docs/registry/implementation-map.yaml:L1324-L1336] |
| `catenary-mooring-line-statics/111` | outside | [docs/registry/implementation-map.yaml:L1337-L1348] |
| `catenary-mooring-line-statics/112` | outside | [docs/registry/implementation-map.yaml:L1349-L1360] |
| `catenary-mooring-line-statics/113` | outside | [docs/registry/implementation-map.yaml:L1361-L1372] |
| `catenary-mooring-line-statics/114` | outside | [docs/registry/implementation-map.yaml:L1373-L1384] |
| `catenary-mooring-line-statics/115` | outside | [docs/registry/implementation-map.yaml:L1385-L1397] |
| `catenary-mooring-line-statics/116` | outside | [docs/registry/implementation-map.yaml:L1398-L1410] |
| `catenary-mooring-line-statics/117` | outside | [docs/registry/implementation-map.yaml:L1411-L1425] |
| `catenary-mooring-line-statics/118` | outside | [docs/registry/implementation-map.yaml:L1426-L1438] |
| `catenary-mooring-line-statics/119` | outside | [docs/registry/implementation-map.yaml:L1439-L1451] |
| `catenary-mooring-line-statics/120` | outside | [docs/registry/implementation-map.yaml:L1452-L1466] |
| `catenary-mooring-line-statics/121` | outside | [docs/registry/implementation-map.yaml:L1467-L1479] |
| `catenary-mooring-line-statics/122` | outside | [docs/registry/implementation-map.yaml:L1480-L1490] |
| `catenary-mooring-line-statics/123` | outside | [docs/registry/implementation-map.yaml:L1491-L1503] |
| `catenary-mooring-line-statics/124` | outside | [docs/registry/implementation-map.yaml:L1504-L1516] |
| `catenary-mooring-line-statics/125` | outside | [docs/registry/implementation-map.yaml:L1517-L1527] |
| `catenary-mooring-line-statics/126` | outside | [docs/registry/implementation-map.yaml:L1528-L1540] |
| `catenary-mooring-line-statics/127` | outside | [docs/registry/implementation-map.yaml:L1541-L1551] |
| `catenary-mooring-line-statics/128` | outside | [docs/registry/implementation-map.yaml:L1552-L1564] |
| `catenary-mooring-line-statics/129` | outside | [docs/registry/implementation-map.yaml:L1565-L1575] |
| `catenary-mooring-line-statics/130` | outside | [docs/registry/implementation-map.yaml:L1576-L1586] |
| `catenary-mooring-line-statics/131` | outside | [docs/registry/implementation-map.yaml:L1587-L1597] |
| `catenary-mooring-line-statics/132` | outside | [docs/registry/implementation-map.yaml:L1598-L1608] |
| `catenary-mooring-line-statics/133` | outside | [docs/registry/implementation-map.yaml:L1609-L1619] |
| `catenary-mooring-line-statics/134` | outside | [docs/registry/implementation-map.yaml:L1620-L1630] |
| `catenary-mooring-line-statics/135` | outside | [docs/registry/implementation-map.yaml:L1631-L1641] |
| `catenary-mooring-line-statics/136` | outside | [docs/registry/implementation-map.yaml:L1642-L1652] |
| `catenary-mooring-line-statics/137` | outside | [docs/registry/implementation-map.yaml:L1653-L1663] |

</details>

<details>
<summary>effective-tension-riser-stack-up: all 264 implementation rows</summary>

| Map row | Package boundary | Definition / implementation locator |
| --- | --- | --- |
| `effective-tension-riser-stack-up/001` | outside | [docs/registry/implementation-map.yaml:L1674-L1682] |
| `effective-tension-riser-stack-up/002` | outside | [docs/registry/implementation-map.yaml:L1683-L1691] |
| `effective-tension-riser-stack-up/003` | outside | [docs/registry/implementation-map.yaml:L1692-L1700] |
| `effective-tension-riser-stack-up/004` | outside | [docs/registry/implementation-map.yaml:L1701-L1709] |
| `effective-tension-riser-stack-up/005` | outside | [docs/registry/implementation-map.yaml:L1710-L1718] |
| `effective-tension-riser-stack-up/006` | outside | [docs/registry/implementation-map.yaml:L1719-L1727] |
| `effective-tension-riser-stack-up/007` | outside | [docs/registry/implementation-map.yaml:L1728-L1736] |
| `effective-tension-riser-stack-up/008` | outside | [docs/registry/implementation-map.yaml:L1737-L1745] |
| `effective-tension-riser-stack-up/009` | outside | [docs/registry/implementation-map.yaml:L1746-L1754] |
| `effective-tension-riser-stack-up/010` | outside | [docs/registry/implementation-map.yaml:L1755-L1765] |
| `effective-tension-riser-stack-up/011` | outside | [docs/registry/implementation-map.yaml:L1766-L1776] |
| `effective-tension-riser-stack-up/012` | outside | [docs/registry/implementation-map.yaml:L1777-L1785] |
| `effective-tension-riser-stack-up/013` | outside | [docs/registry/implementation-map.yaml:L1786-L1797] |
| `effective-tension-riser-stack-up/014` | outside | [docs/registry/implementation-map.yaml:L1798-L1806] |
| `effective-tension-riser-stack-up/015` | outside | [docs/registry/implementation-map.yaml:L1807-L1815] |
| `effective-tension-riser-stack-up/016` | outside | [docs/registry/implementation-map.yaml:L1816-L1824] |
| `effective-tension-riser-stack-up/017` | outside | [docs/registry/implementation-map.yaml:L1825-L1836] |
| `effective-tension-riser-stack-up/018` | outside | [docs/registry/implementation-map.yaml:L1837-L1845] |
| `effective-tension-riser-stack-up/019` | outside | [docs/registry/implementation-map.yaml:L1846-L1854] |
| `effective-tension-riser-stack-up/020` | outside | [docs/registry/implementation-map.yaml:L1855-L1865] |
| `effective-tension-riser-stack-up/021` | outside | [docs/registry/implementation-map.yaml:L1866-L1874] |
| `effective-tension-riser-stack-up/022` | outside | [docs/registry/implementation-map.yaml:L1875-L1883] |
| `effective-tension-riser-stack-up/023` | outside | [docs/registry/implementation-map.yaml:L1884-L1894] |
| `effective-tension-riser-stack-up/024` | outside | [docs/registry/implementation-map.yaml:L1895-L1903] |
| `effective-tension-riser-stack-up/025` | outside | [docs/registry/implementation-map.yaml:L1904-L1912] |
| `effective-tension-riser-stack-up/026` | outside | [docs/registry/implementation-map.yaml:L1913-L1921] |
| `effective-tension-riser-stack-up/027` | outside | [docs/registry/implementation-map.yaml:L1922-L1930] |
| `effective-tension-riser-stack-up/028` | outside | [docs/registry/implementation-map.yaml:L1931-L1939] |
| `effective-tension-riser-stack-up/029` | outside | [docs/registry/implementation-map.yaml:L1940-L1948] |
| `effective-tension-riser-stack-up/030` | outside | [docs/registry/implementation-map.yaml:L1949-L1957] |
| `effective-tension-riser-stack-up/031` | outside | [docs/registry/implementation-map.yaml:L1958-L1969] |
| `effective-tension-riser-stack-up/032` | outside | [docs/registry/implementation-map.yaml:L1970-L1982] |
| `effective-tension-riser-stack-up/033` | outside | [docs/registry/implementation-map.yaml:L1983-L1991] |
| `effective-tension-riser-stack-up/034` | outside | [docs/registry/implementation-map.yaml:L1992-L2000] |
| `effective-tension-riser-stack-up/035` | outside | [docs/registry/implementation-map.yaml:L2001-L2011] |
| `effective-tension-riser-stack-up/036` | outside | [docs/registry/implementation-map.yaml:L2012-L2020] |
| `effective-tension-riser-stack-up/037` | outside | [docs/registry/implementation-map.yaml:L2021-L2029] |
| `effective-tension-riser-stack-up/038` | outside | [docs/registry/implementation-map.yaml:L2030-L2038] |
| `effective-tension-riser-stack-up/039` | outside | [docs/registry/implementation-map.yaml:L2039-L2049] |
| `effective-tension-riser-stack-up/040` | outside | [docs/registry/implementation-map.yaml:L2050-L2058] |
| `effective-tension-riser-stack-up/041` | outside | [docs/registry/implementation-map.yaml:L2059-L2067] |
| `effective-tension-riser-stack-up/042` | outside | [docs/registry/implementation-map.yaml:L2068-L2076] |
| `effective-tension-riser-stack-up/043` | outside | [docs/registry/implementation-map.yaml:L2077-L2085] |
| `effective-tension-riser-stack-up/044` | outside | [docs/registry/implementation-map.yaml:L2086-L2097] |
| `effective-tension-riser-stack-up/045` | outside | [docs/registry/implementation-map.yaml:L2098-L2106] |
| `effective-tension-riser-stack-up/046` | outside | [docs/registry/implementation-map.yaml:L2107-L2115] |
| `effective-tension-riser-stack-up/047` | outside | [docs/registry/implementation-map.yaml:L2116-L2124] |
| `effective-tension-riser-stack-up/048` | outside | [docs/registry/implementation-map.yaml:L2125-L2133] |
| `effective-tension-riser-stack-up/049` | outside | [docs/registry/implementation-map.yaml:L2134-L2142] |
| `effective-tension-riser-stack-up/050` | outside | [docs/registry/implementation-map.yaml:L2143-L2153] |
| `effective-tension-riser-stack-up/051` | outside | [docs/registry/implementation-map.yaml:L2154-L2164] |
| `effective-tension-riser-stack-up/052` | outside | [docs/registry/implementation-map.yaml:L2165-L2173] |
| `effective-tension-riser-stack-up/053` | outside | [docs/registry/implementation-map.yaml:L2174-L2184] |
| `effective-tension-riser-stack-up/054` | outside | [docs/registry/implementation-map.yaml:L2185-L2197] |
| `effective-tension-riser-stack-up/055` | outside | [docs/registry/implementation-map.yaml:L2198-L2207] |
| `effective-tension-riser-stack-up/056` | outside | [docs/registry/implementation-map.yaml:L2208-L2220] |
| `effective-tension-riser-stack-up/057` | outside | [docs/registry/implementation-map.yaml:L2221-L2229] |
| `effective-tension-riser-stack-up/058` | outside | [docs/registry/implementation-map.yaml:L2230-L2238] |
| `effective-tension-riser-stack-up/059` | outside | [docs/registry/implementation-map.yaml:L2239-L2251] |
| `effective-tension-riser-stack-up/060` | outside | [docs/registry/implementation-map.yaml:L2252-L2263] |
| `effective-tension-riser-stack-up/061` | outside | [docs/registry/implementation-map.yaml:L2264-L2272] |
| `effective-tension-riser-stack-up/062` | outside | [docs/registry/implementation-map.yaml:L2273-L2281] |
| `effective-tension-riser-stack-up/063` | outside | [docs/registry/implementation-map.yaml:L2282-L2290] |
| `effective-tension-riser-stack-up/064` | outside | [docs/registry/implementation-map.yaml:L2291-L2299] |
| `effective-tension-riser-stack-up/065` | outside | [docs/registry/implementation-map.yaml:L2300-L2310] |
| `effective-tension-riser-stack-up/066` | outside | [docs/registry/implementation-map.yaml:L2311-L2322] |
| `effective-tension-riser-stack-up/067` | outside | [docs/registry/implementation-map.yaml:L2323-L2331] |
| `effective-tension-riser-stack-up/068` | outside | [docs/registry/implementation-map.yaml:L2332-L2342] |
| `effective-tension-riser-stack-up/069` | outside | [docs/registry/implementation-map.yaml:L2343-L2351] |
| `effective-tension-riser-stack-up/070` | outside | [docs/registry/implementation-map.yaml:L2352-L2362] |
| `effective-tension-riser-stack-up/071` | outside | [docs/registry/implementation-map.yaml:L2363-L2372] |
| `effective-tension-riser-stack-up/072` | outside | [docs/registry/implementation-map.yaml:L2373-L2381] |
| `effective-tension-riser-stack-up/073` | outside | [docs/registry/implementation-map.yaml:L2382-L2390] |
| `effective-tension-riser-stack-up/074` | outside | [docs/registry/implementation-map.yaml:L2391-L2400] |
| `effective-tension-riser-stack-up/075` | outside | [docs/registry/implementation-map.yaml:L2401-L2409] |
| `effective-tension-riser-stack-up/076` | outside | [docs/registry/implementation-map.yaml:L2410-L2420] |
| `effective-tension-riser-stack-up/077` | outside | [docs/registry/implementation-map.yaml:L2421-L2429] |
| `effective-tension-riser-stack-up/078` | outside | [docs/registry/implementation-map.yaml:L2430-L2438] |
| `effective-tension-riser-stack-up/079` | outside | [docs/registry/implementation-map.yaml:L2439-L2447] |
| `effective-tension-riser-stack-up/080` | outside | [docs/registry/implementation-map.yaml:L2448-L2456] |
| `effective-tension-riser-stack-up/081` | outside | [docs/registry/implementation-map.yaml:L2457-L2465] |
| `effective-tension-riser-stack-up/082` | outside | [docs/registry/implementation-map.yaml:L2466-L2476] |
| `effective-tension-riser-stack-up/083` | outside | [docs/registry/implementation-map.yaml:L2477-L2485] |
| `effective-tension-riser-stack-up/084` | outside | [docs/registry/implementation-map.yaml:L2486-L2494] |
| `effective-tension-riser-stack-up/085` | outside | [docs/registry/implementation-map.yaml:L2495-L2503] |
| `effective-tension-riser-stack-up/086` | outside | [docs/registry/implementation-map.yaml:L2504-L2512] |
| `effective-tension-riser-stack-up/087` | outside | [docs/registry/implementation-map.yaml:L2513-L2521] |
| `effective-tension-riser-stack-up/088` | outside | [docs/registry/implementation-map.yaml:L2522-L2530] |
| `effective-tension-riser-stack-up/089` | outside | [docs/registry/implementation-map.yaml:L2531-L2539] |
| `effective-tension-riser-stack-up/090` | outside | [docs/registry/implementation-map.yaml:L2540-L2548] |
| `effective-tension-riser-stack-up/091` | outside | [docs/registry/implementation-map.yaml:L2549-L2557] |
| `effective-tension-riser-stack-up/092` | outside | [docs/registry/implementation-map.yaml:L2558-L2566] |
| `effective-tension-riser-stack-up/093` | outside | [docs/registry/implementation-map.yaml:L2567-L2578] |
| `effective-tension-riser-stack-up/094` | outside | [docs/registry/implementation-map.yaml:L2579-L2587] |
| `effective-tension-riser-stack-up/095` | outside | [docs/registry/implementation-map.yaml:L2588-L2596] |
| `effective-tension-riser-stack-up/096` | outside | [docs/registry/implementation-map.yaml:L2597-L2608] |
| `effective-tension-riser-stack-up/097` | outside | [docs/registry/implementation-map.yaml:L2609-L2617] |
| `effective-tension-riser-stack-up/098` | outside | [docs/registry/implementation-map.yaml:L2618-L2626] |
| `effective-tension-riser-stack-up/099` | outside | [docs/registry/implementation-map.yaml:L2627-L2635] |
| `effective-tension-riser-stack-up/100` | outside | [docs/registry/implementation-map.yaml:L2636-L2644] |
| `effective-tension-riser-stack-up/101` | outside | [docs/registry/implementation-map.yaml:L2645-L2653] |
| `effective-tension-riser-stack-up/102` | outside | [docs/registry/implementation-map.yaml:L2654-L2667] |
| `effective-tension-riser-stack-up/103` | outside | [docs/registry/implementation-map.yaml:L2668-L2676] |
| `effective-tension-riser-stack-up/104` | outside | [docs/registry/implementation-map.yaml:L2677-L2685] |
| `effective-tension-riser-stack-up/105` | outside | [docs/registry/implementation-map.yaml:L2686-L2697] |
| `effective-tension-riser-stack-up/106` | outside | [docs/registry/implementation-map.yaml:L2698-L2708] |
| `effective-tension-riser-stack-up/107` | outside | [docs/registry/implementation-map.yaml:L2709-L2722] |
| `effective-tension-riser-stack-up/108` | outside | [docs/registry/implementation-map.yaml:L2723-L2736] |
| `effective-tension-riser-stack-up/109` | outside | [docs/registry/implementation-map.yaml:L2737-L2748] |
| `effective-tension-riser-stack-up/110` | outside | [docs/registry/implementation-map.yaml:L2749-L2759] |
| `effective-tension-riser-stack-up/111` | outside | [docs/registry/implementation-map.yaml:L2760-L2768] |
| `effective-tension-riser-stack-up/112` | outside | [docs/registry/implementation-map.yaml:L2769-L2779] |
| `effective-tension-riser-stack-up/113` | outside | [docs/registry/implementation-map.yaml:L2780-L2790] |
| `effective-tension-riser-stack-up/114` | outside | [docs/registry/implementation-map.yaml:L2791-L2799] |
| `effective-tension-riser-stack-up/115` | outside | [docs/registry/implementation-map.yaml:L2800-L2814] |
| `effective-tension-riser-stack-up/116` | outside | [docs/registry/implementation-map.yaml:L2815-L2823] |
| `effective-tension-riser-stack-up/117` | outside | [docs/registry/implementation-map.yaml:L2824-L2832] |
| `effective-tension-riser-stack-up/118` | outside | [docs/registry/implementation-map.yaml:L2833-L2841] |
| `effective-tension-riser-stack-up/119` | outside | [docs/registry/implementation-map.yaml:L2842-L2850] |
| `effective-tension-riser-stack-up/120` | outside | [docs/registry/implementation-map.yaml:L2851-L2864] |
| `effective-tension-riser-stack-up/121` | outside | [docs/registry/implementation-map.yaml:L2865-L2873] |
| `effective-tension-riser-stack-up/122` | outside | [docs/registry/implementation-map.yaml:L2874-L2882] |
| `effective-tension-riser-stack-up/123` | outside | [docs/registry/implementation-map.yaml:L2883-L2891] |
| `effective-tension-riser-stack-up/124` | outside | [docs/registry/implementation-map.yaml:L2892-L2900] |
| `effective-tension-riser-stack-up/125` | outside | [docs/registry/implementation-map.yaml:L2901-L2909] |
| `effective-tension-riser-stack-up/126` | outside | [docs/registry/implementation-map.yaml:L2910-L2918] |
| `effective-tension-riser-stack-up/127` | outside | [docs/registry/implementation-map.yaml:L2919-L2927] |
| `effective-tension-riser-stack-up/128` | outside | [docs/registry/implementation-map.yaml:L2928-L2936] |
| `effective-tension-riser-stack-up/129` | outside | [docs/registry/implementation-map.yaml:L2937-L2945] |
| `effective-tension-riser-stack-up/130` | outside | [docs/registry/implementation-map.yaml:L2946-L2954] |
| `effective-tension-riser-stack-up/131` | outside | [docs/registry/implementation-map.yaml:L2955-L2963] |
| `effective-tension-riser-stack-up/132` | outside | [docs/registry/implementation-map.yaml:L2964-L2977] |
| `effective-tension-riser-stack-up/133` | outside | [docs/registry/implementation-map.yaml:L2978-L2986] |
| `effective-tension-riser-stack-up/134` | outside | [docs/registry/implementation-map.yaml:L2987-L2995] |
| `effective-tension-riser-stack-up/135` | outside | [docs/registry/implementation-map.yaml:L2996-L3004] |
| `effective-tension-riser-stack-up/136` | outside | [docs/registry/implementation-map.yaml:L3005-L3015] |
| `effective-tension-riser-stack-up/137` | outside | [docs/registry/implementation-map.yaml:L3016-L3024] |
| `effective-tension-riser-stack-up/138` | outside | [docs/registry/implementation-map.yaml:L3025-L3033] |
| `effective-tension-riser-stack-up/139` | inside | `src/digitalmodel/orcaflex/mooring_design.py::solve_catenary`; [docs/registry/implementation-map.yaml:L3034-L3042] |
| `effective-tension-riser-stack-up/140` | inside | `src/digitalmodel/orcaflex/pipelay_analysis.py::JLayConfig.calculate_sagbend`; [docs/registry/implementation-map.yaml:L3043-L3051] |
| `effective-tension-riser-stack-up/141` | inside | `src/digitalmodel/orcaflex/riser_config.py::SCRDesignInput.calculate_geometry`; [docs/registry/implementation-map.yaml:L3052-L3060] |
| `effective-tension-riser-stack-up/142` | inside | `src/digitalmodel/orcaflex/riser_config.py::TTRDesignInput.calculate_stroke`; [docs/registry/implementation-map.yaml:L3061-L3069] |
| `effective-tension-riser-stack-up/143` | inside | `src/digitalmodel/orcaflex/riser_config.py::TTRDesignInput.calculate_top_tension`; [docs/registry/implementation-map.yaml:L3070-L3078] |
| `effective-tension-riser-stack-up/144` | inside | `src/digitalmodel/orcaflex/riser_input_schema.py::_TopTensionedInput.to_legacy`; [docs/registry/implementation-map.yaml:L3079-L3087] |
| `effective-tension-riser-stack-up/145` | inside | `src/digitalmodel/orcaflex/viv_screening.py::BeamProperties.natural_frequency`; [docs/registry/implementation-map.yaml:L3088-L3096] |
| `effective-tension-riser-stack-up/146` | outside | [docs/registry/implementation-map.yaml:L3097-L3105] |
| `effective-tension-riser-stack-up/147` | outside | [docs/registry/implementation-map.yaml:L3106-L3114] |
| `effective-tension-riser-stack-up/148` | outside | [docs/registry/implementation-map.yaml:L3115-L3123] |
| `effective-tension-riser-stack-up/149` | outside | [docs/registry/implementation-map.yaml:L3124-L3132] |
| `effective-tension-riser-stack-up/150` | outside | [docs/registry/implementation-map.yaml:L3133-L3141] |
| `effective-tension-riser-stack-up/151` | outside | [docs/registry/implementation-map.yaml:L3142-L3150] |
| `effective-tension-riser-stack-up/152` | outside | [docs/registry/implementation-map.yaml:L3151-L3159] |
| `effective-tension-riser-stack-up/153` | outside | [docs/registry/implementation-map.yaml:L3160-L3168] |
| `effective-tension-riser-stack-up/154` | outside | [docs/registry/implementation-map.yaml:L3169-L3177] |
| `effective-tension-riser-stack-up/155` | outside | [docs/registry/implementation-map.yaml:L3178-L3186] |
| `effective-tension-riser-stack-up/156` | outside | [docs/registry/implementation-map.yaml:L3187-L3195] |
| `effective-tension-riser-stack-up/157` | outside | [docs/registry/implementation-map.yaml:L3196-L3204] |
| `effective-tension-riser-stack-up/158` | outside | [docs/registry/implementation-map.yaml:L3205-L3215] |
| `effective-tension-riser-stack-up/159` | outside | [docs/registry/implementation-map.yaml:L3216-L3224] |
| `effective-tension-riser-stack-up/160` | outside | [docs/registry/implementation-map.yaml:L3225-L3233] |
| `effective-tension-riser-stack-up/161` | outside | [docs/registry/implementation-map.yaml:L3234-L3242] |
| `effective-tension-riser-stack-up/162` | outside | [docs/registry/implementation-map.yaml:L3243-L3254] |
| `effective-tension-riser-stack-up/163` | outside | [docs/registry/implementation-map.yaml:L3255-L3265] |
| `effective-tension-riser-stack-up/164` | outside | [docs/registry/implementation-map.yaml:L3266-L3279] |
| `effective-tension-riser-stack-up/165` | outside | [docs/registry/implementation-map.yaml:L3280-L3293] |
| `effective-tension-riser-stack-up/166` | outside | [docs/registry/implementation-map.yaml:L3294-L3305] |
| `effective-tension-riser-stack-up/167` | outside | [docs/registry/implementation-map.yaml:L3306-L3316] |
| `effective-tension-riser-stack-up/168` | outside | [docs/registry/implementation-map.yaml:L3317-L3325] |
| `effective-tension-riser-stack-up/169` | outside | [docs/registry/implementation-map.yaml:L3326-L3336] |
| `effective-tension-riser-stack-up/170` | outside | [docs/registry/implementation-map.yaml:L3337-L3347] |
| `effective-tension-riser-stack-up/171` | outside | [docs/registry/implementation-map.yaml:L3348-L3356] |
| `effective-tension-riser-stack-up/172` | outside | [docs/registry/implementation-map.yaml:L3357-L3365] |
| `effective-tension-riser-stack-up/173` | outside | [docs/registry/implementation-map.yaml:L3366-L3374] |
| `effective-tension-riser-stack-up/174` | outside | [docs/registry/implementation-map.yaml:L3375-L3383] |
| `effective-tension-riser-stack-up/175` | outside | [docs/registry/implementation-map.yaml:L3384-L3392] |
| `effective-tension-riser-stack-up/176` | outside | [docs/registry/implementation-map.yaml:L3393-L3401] |
| `effective-tension-riser-stack-up/177` | outside | [docs/registry/implementation-map.yaml:L3402-L3410] |
| `effective-tension-riser-stack-up/178` | outside | [docs/registry/implementation-map.yaml:L3411-L3419] |
| `effective-tension-riser-stack-up/179` | outside | [docs/registry/implementation-map.yaml:L3420-L3428] |
| `effective-tension-riser-stack-up/180` | outside | [docs/registry/implementation-map.yaml:L3429-L3437] |
| `effective-tension-riser-stack-up/181` | outside | [docs/registry/implementation-map.yaml:L3438-L3446] |
| `effective-tension-riser-stack-up/182` | outside | [docs/registry/implementation-map.yaml:L3447-L3455] |
| `effective-tension-riser-stack-up/183` | outside | [docs/registry/implementation-map.yaml:L3456-L3464] |
| `effective-tension-riser-stack-up/184` | outside | [docs/registry/implementation-map.yaml:L3465-L3473] |
| `effective-tension-riser-stack-up/185` | outside | [docs/registry/implementation-map.yaml:L3474-L3482] |
| `effective-tension-riser-stack-up/186` | outside | [docs/registry/implementation-map.yaml:L3483-L3491] |
| `effective-tension-riser-stack-up/187` | outside | [docs/registry/implementation-map.yaml:L3492-L3500] |
| `effective-tension-riser-stack-up/188` | outside | [docs/registry/implementation-map.yaml:L3501-L3509] |
| `effective-tension-riser-stack-up/189` | outside | [docs/registry/implementation-map.yaml:L3510-L3518] |
| `effective-tension-riser-stack-up/190` | outside | [docs/registry/implementation-map.yaml:L3519-L3527] |
| `effective-tension-riser-stack-up/191` | outside | [docs/registry/implementation-map.yaml:L3528-L3539] |
| `effective-tension-riser-stack-up/192` | outside | [docs/registry/implementation-map.yaml:L3540-L3550] |
| `effective-tension-riser-stack-up/193` | outside | [docs/registry/implementation-map.yaml:L3551-L3562] |
| `effective-tension-riser-stack-up/194` | outside | [docs/registry/implementation-map.yaml:L3563-L3571] |
| `effective-tension-riser-stack-up/195` | outside | [docs/registry/implementation-map.yaml:L3572-L3580] |
| `effective-tension-riser-stack-up/196` | outside | [docs/registry/implementation-map.yaml:L3581-L3589] |
| `effective-tension-riser-stack-up/197` | outside | [docs/registry/implementation-map.yaml:L3590-L3601] |
| `effective-tension-riser-stack-up/198` | outside | [docs/registry/implementation-map.yaml:L3602-L3610] |
| `effective-tension-riser-stack-up/199` | outside | [docs/registry/implementation-map.yaml:L3611-L3619] |
| `effective-tension-riser-stack-up/200` | outside | [docs/registry/implementation-map.yaml:L3620-L3628] |
| `effective-tension-riser-stack-up/201` | outside | [docs/registry/implementation-map.yaml:L3629-L3637] |
| `effective-tension-riser-stack-up/202` | outside | [docs/registry/implementation-map.yaml:L3638-L3646] |
| `effective-tension-riser-stack-up/203` | outside | [docs/registry/implementation-map.yaml:L3647-L3658] |
| `effective-tension-riser-stack-up/204` | outside | [docs/registry/implementation-map.yaml:L3659-L3667] |
| `effective-tension-riser-stack-up/205` | outside | [docs/registry/implementation-map.yaml:L3668-L3678] |
| `effective-tension-riser-stack-up/206` | outside | [docs/registry/implementation-map.yaml:L3679-L3687] |
| `effective-tension-riser-stack-up/207` | outside | [docs/registry/implementation-map.yaml:L3688-L3696] |
| `effective-tension-riser-stack-up/208` | outside | [docs/registry/implementation-map.yaml:L3697-L3705] |
| `effective-tension-riser-stack-up/209` | outside | [docs/registry/implementation-map.yaml:L3706-L3714] |
| `effective-tension-riser-stack-up/210` | outside | [docs/registry/implementation-map.yaml:L3715-L3728] |
| `effective-tension-riser-stack-up/211` | outside | [docs/registry/implementation-map.yaml:L3729-L3737] |
| `effective-tension-riser-stack-up/212` | outside | [docs/registry/implementation-map.yaml:L3738-L3746] |
| `effective-tension-riser-stack-up/213` | outside | [docs/registry/implementation-map.yaml:L3747-L3755] |
| `effective-tension-riser-stack-up/214` | outside | [docs/registry/implementation-map.yaml:L3756-L3764] |
| `effective-tension-riser-stack-up/215` | outside | [docs/registry/implementation-map.yaml:L3765-L3773] |
| `effective-tension-riser-stack-up/216` | outside | [docs/registry/implementation-map.yaml:L3774-L3782] |
| `effective-tension-riser-stack-up/217` | outside | [docs/registry/implementation-map.yaml:L3783-L3791] |
| `effective-tension-riser-stack-up/218` | outside | [docs/registry/implementation-map.yaml:L3792-L3802] |
| `effective-tension-riser-stack-up/219` | outside | [docs/registry/implementation-map.yaml:L3803-L3811] |
| `effective-tension-riser-stack-up/220` | outside | [docs/registry/implementation-map.yaml:L3812-L3820] |
| `effective-tension-riser-stack-up/221` | outside | [docs/registry/implementation-map.yaml:L3821-L3831] |
| `effective-tension-riser-stack-up/222` | outside | [docs/registry/implementation-map.yaml:L3832-L3840] |
| `effective-tension-riser-stack-up/223` | outside | [docs/registry/implementation-map.yaml:L3841-L3849] |
| `effective-tension-riser-stack-up/224` | outside | [docs/registry/implementation-map.yaml:L3850-L3858] |
| `effective-tension-riser-stack-up/225` | outside | [docs/registry/implementation-map.yaml:L3859-L3867] |
| `effective-tension-riser-stack-up/226` | outside | [docs/registry/implementation-map.yaml:L3868-L3876] |
| `effective-tension-riser-stack-up/227` | outside | [docs/registry/implementation-map.yaml:L3877-L3885] |
| `effective-tension-riser-stack-up/228` | outside | [docs/registry/implementation-map.yaml:L3886-L3894] |
| `effective-tension-riser-stack-up/229` | outside | [docs/registry/implementation-map.yaml:L3895-L3905] |
| `effective-tension-riser-stack-up/230` | outside | [docs/registry/implementation-map.yaml:L3906-L3914] |
| `effective-tension-riser-stack-up/231` | outside | [docs/registry/implementation-map.yaml:L3915-L3924] |
| `effective-tension-riser-stack-up/232` | outside | [docs/registry/implementation-map.yaml:L3925-L3934] |
| `effective-tension-riser-stack-up/233` | outside | [docs/registry/implementation-map.yaml:L3935-L3943] |
| `effective-tension-riser-stack-up/234` | outside | [docs/registry/implementation-map.yaml:L3944-L3952] |
| `effective-tension-riser-stack-up/235` | outside | [docs/registry/implementation-map.yaml:L3953-L3965] |
| `effective-tension-riser-stack-up/236` | outside | [docs/registry/implementation-map.yaml:L3966-L3975] |
| `effective-tension-riser-stack-up/237` | outside | [docs/registry/implementation-map.yaml:L3976-L3984] |
| `effective-tension-riser-stack-up/238` | outside | [docs/registry/implementation-map.yaml:L3985-L3993] |
| `effective-tension-riser-stack-up/239` | outside | [docs/registry/implementation-map.yaml:L3994-L4004] |
| `effective-tension-riser-stack-up/240` | outside | [docs/registry/implementation-map.yaml:L4005-L4013] |
| `effective-tension-riser-stack-up/241` | outside | [docs/registry/implementation-map.yaml:L4014-L4022] |
| `effective-tension-riser-stack-up/242` | outside | [docs/registry/implementation-map.yaml:L4023-L4031] |
| `effective-tension-riser-stack-up/243` | outside | [docs/registry/implementation-map.yaml:L4032-L4040] |
| `effective-tension-riser-stack-up/244` | outside | [docs/registry/implementation-map.yaml:L4041-L4049] |
| `effective-tension-riser-stack-up/245` | outside | [docs/registry/implementation-map.yaml:L4050-L4058] |
| `effective-tension-riser-stack-up/246` | outside | [docs/registry/implementation-map.yaml:L4059-L4067] |
| `effective-tension-riser-stack-up/247` | outside | [docs/registry/implementation-map.yaml:L4068-L4076] |
| `effective-tension-riser-stack-up/248` | outside | [docs/registry/implementation-map.yaml:L4077-L4085] |
| `effective-tension-riser-stack-up/249` | outside | [docs/registry/implementation-map.yaml:L4086-L4094] |
| `effective-tension-riser-stack-up/250` | outside | [docs/registry/implementation-map.yaml:L4095-L4103] |
| `effective-tension-riser-stack-up/251` | outside | [docs/registry/implementation-map.yaml:L4104-L4112] |
| `effective-tension-riser-stack-up/252` | outside | [docs/registry/implementation-map.yaml:L4113-L4121] |
| `effective-tension-riser-stack-up/253` | outside | [docs/registry/implementation-map.yaml:L4122-L4130] |
| `effective-tension-riser-stack-up/254` | outside | [docs/registry/implementation-map.yaml:L4131-L4139] |
| `effective-tension-riser-stack-up/255` | outside | [docs/registry/implementation-map.yaml:L4140-L4148] |
| `effective-tension-riser-stack-up/256` | outside | [docs/registry/implementation-map.yaml:L4149-L4157] |
| `effective-tension-riser-stack-up/257` | outside | [docs/registry/implementation-map.yaml:L4158-L4166] |
| `effective-tension-riser-stack-up/258` | outside | [docs/registry/implementation-map.yaml:L4167-L4175] |
| `effective-tension-riser-stack-up/259` | outside | [docs/registry/implementation-map.yaml:L4176-L4184] |
| `effective-tension-riser-stack-up/260` | outside | [docs/registry/implementation-map.yaml:L4185-L4193] |
| `effective-tension-riser-stack-up/261` | outside | [docs/registry/implementation-map.yaml:L4194-L4204] |
| `effective-tension-riser-stack-up/262` | outside | [docs/registry/implementation-map.yaml:L4205-L4213] |
| `effective-tension-riser-stack-up/263` | outside | [docs/registry/implementation-map.yaml:L4214-L4222] |
| `effective-tension-riser-stack-up/264` | outside | [docs/registry/implementation-map.yaml:L4223-L4231] |

</details>

<details>
<summary>orcaflex-text-model-writers: all 440 implementation rows</summary>

| Map row | Package boundary | Definition / implementation locator |
| --- | --- | --- |
| `orcaflex-text-model-writers/001` | outside | [docs/registry/implementation-map.yaml:L4242-L4250] |
| `orcaflex-text-model-writers/002` | outside | [docs/registry/implementation-map.yaml:L4251-L4259] |
| `orcaflex-text-model-writers/003` | outside | [docs/registry/implementation-map.yaml:L4260-L4268] |
| `orcaflex-text-model-writers/004` | outside | [docs/registry/implementation-map.yaml:L4269-L4277] |
| `orcaflex-text-model-writers/005` | outside | [docs/registry/implementation-map.yaml:L4278-L4286] |
| `orcaflex-text-model-writers/006` | outside | [docs/registry/implementation-map.yaml:L4287-L4295] |
| `orcaflex-text-model-writers/007` | outside | [docs/registry/implementation-map.yaml:L4296-L4304] |
| `orcaflex-text-model-writers/008` | outside | [docs/registry/implementation-map.yaml:L4305-L4315] |
| `orcaflex-text-model-writers/009` | outside | [docs/registry/implementation-map.yaml:L4316-L4328] |
| `orcaflex-text-model-writers/010` | outside | [docs/registry/implementation-map.yaml:L4329-L4341] |
| `orcaflex-text-model-writers/011` | outside | [docs/registry/implementation-map.yaml:L4342-L4354] |
| `orcaflex-text-model-writers/012` | outside | [docs/registry/implementation-map.yaml:L4355-L4367] |
| `orcaflex-text-model-writers/013` | outside | [docs/registry/implementation-map.yaml:L4368-L4376] |
| `orcaflex-text-model-writers/014` | outside | [docs/registry/implementation-map.yaml:L4377-L4385] |
| `orcaflex-text-model-writers/015` | outside | [docs/registry/implementation-map.yaml:L4386-L4394] |
| `orcaflex-text-model-writers/016` | outside | [docs/registry/implementation-map.yaml:L4395-L4403] |
| `orcaflex-text-model-writers/017` | outside | [docs/registry/implementation-map.yaml:L4404-L4412] |
| `orcaflex-text-model-writers/018` | outside | [docs/registry/implementation-map.yaml:L4413-L4421] |
| `orcaflex-text-model-writers/019` | outside | [docs/registry/implementation-map.yaml:L4422-L4432] |
| `orcaflex-text-model-writers/020` | outside | [docs/registry/implementation-map.yaml:L4433-L4441] |
| `orcaflex-text-model-writers/021` | outside | [docs/registry/implementation-map.yaml:L4442-L4450] |
| `orcaflex-text-model-writers/022` | outside | [docs/registry/implementation-map.yaml:L4451-L4459] |
| `orcaflex-text-model-writers/023` | outside | [docs/registry/implementation-map.yaml:L4460-L4468] |
| `orcaflex-text-model-writers/024` | outside | [docs/registry/implementation-map.yaml:L4469-L4477] |
| `orcaflex-text-model-writers/025` | outside | [docs/registry/implementation-map.yaml:L4478-L4486] |
| `orcaflex-text-model-writers/026` | outside | [docs/registry/implementation-map.yaml:L4487-L4495] |
| `orcaflex-text-model-writers/027` | outside | [docs/registry/implementation-map.yaml:L4496-L4504] |
| `orcaflex-text-model-writers/028` | outside | [docs/registry/implementation-map.yaml:L4505-L4515] |
| `orcaflex-text-model-writers/029` | outside | [docs/registry/implementation-map.yaml:L4516-L4524] |
| `orcaflex-text-model-writers/030` | outside | [docs/registry/implementation-map.yaml:L4525-L4533] |
| `orcaflex-text-model-writers/031` | outside | [docs/registry/implementation-map.yaml:L4534-L4542] |
| `orcaflex-text-model-writers/032` | outside | [docs/registry/implementation-map.yaml:L4543-L4551] |
| `orcaflex-text-model-writers/033` | outside | [docs/registry/implementation-map.yaml:L4552-L4560] |
| `orcaflex-text-model-writers/034` | outside | [docs/registry/implementation-map.yaml:L4561-L4569] |
| `orcaflex-text-model-writers/035` | outside | [docs/registry/implementation-map.yaml:L4570-L4578] |
| `orcaflex-text-model-writers/036` | outside | [docs/registry/implementation-map.yaml:L4579-L4587] |
| `orcaflex-text-model-writers/037` | outside | [docs/registry/implementation-map.yaml:L4588-L4596] |
| `orcaflex-text-model-writers/038` | outside | [docs/registry/implementation-map.yaml:L4597-L4605] |
| `orcaflex-text-model-writers/039` | outside | [docs/registry/implementation-map.yaml:L4606-L4614] |
| `orcaflex-text-model-writers/040` | outside | [docs/registry/implementation-map.yaml:L4615-L4623] |
| `orcaflex-text-model-writers/041` | outside | [docs/registry/implementation-map.yaml:L4624-L4632] |
| `orcaflex-text-model-writers/042` | outside | [docs/registry/implementation-map.yaml:L4633-L4641] |
| `orcaflex-text-model-writers/043` | outside | [docs/registry/implementation-map.yaml:L4642-L4650] |
| `orcaflex-text-model-writers/044` | outside | [docs/registry/implementation-map.yaml:L4651-L4659] |
| `orcaflex-text-model-writers/045` | outside | [docs/registry/implementation-map.yaml:L4660-L4668] |
| `orcaflex-text-model-writers/046` | outside | [docs/registry/implementation-map.yaml:L4669-L4677] |
| `orcaflex-text-model-writers/047` | outside | [docs/registry/implementation-map.yaml:L4678-L4689] |
| `orcaflex-text-model-writers/048` | outside | [docs/registry/implementation-map.yaml:L4690-L4698] |
| `orcaflex-text-model-writers/049` | outside | [docs/registry/implementation-map.yaml:L4699-L4709] |
| `orcaflex-text-model-writers/050` | outside | [docs/registry/implementation-map.yaml:L4710-L4718] |
| `orcaflex-text-model-writers/051` | outside | [docs/registry/implementation-map.yaml:L4719-L4727] |
| `orcaflex-text-model-writers/052` | outside | [docs/registry/implementation-map.yaml:L4728-L4736] |
| `orcaflex-text-model-writers/053` | outside | [docs/registry/implementation-map.yaml:L4737-L4745] |
| `orcaflex-text-model-writers/054` | outside | [docs/registry/implementation-map.yaml:L4746-L4754] |
| `orcaflex-text-model-writers/055` | outside | [docs/registry/implementation-map.yaml:L4755-L4763] |
| `orcaflex-text-model-writers/056` | outside | [docs/registry/implementation-map.yaml:L4764-L4772] |
| `orcaflex-text-model-writers/057` | outside | [docs/registry/implementation-map.yaml:L4773-L4783] |
| `orcaflex-text-model-writers/058` | outside | [docs/registry/implementation-map.yaml:L4784-L4794] |
| `orcaflex-text-model-writers/059` | outside | [docs/registry/implementation-map.yaml:L4795-L4803] |
| `orcaflex-text-model-writers/060` | outside | [docs/registry/implementation-map.yaml:L4804-L4812] |
| `orcaflex-text-model-writers/061` | outside | [docs/registry/implementation-map.yaml:L4813-L4821] |
| `orcaflex-text-model-writers/062` | outside | [docs/registry/implementation-map.yaml:L4822-L4830] |
| `orcaflex-text-model-writers/063` | outside | [docs/registry/implementation-map.yaml:L4831-L4839] |
| `orcaflex-text-model-writers/064` | outside | [docs/registry/implementation-map.yaml:L4840-L4851] |
| `orcaflex-text-model-writers/065` | outside | [docs/registry/implementation-map.yaml:L4852-L4860] |
| `orcaflex-text-model-writers/066` | outside | [docs/registry/implementation-map.yaml:L4861-L4871] |
| `orcaflex-text-model-writers/067` | outside | [docs/registry/implementation-map.yaml:L4872-L4883] |
| `orcaflex-text-model-writers/068` | outside | [docs/registry/implementation-map.yaml:L4884-L4892] |
| `orcaflex-text-model-writers/069` | outside | [docs/registry/implementation-map.yaml:L4893-L4901] |
| `orcaflex-text-model-writers/070` | outside | [docs/registry/implementation-map.yaml:L4902-L4910] |
| `orcaflex-text-model-writers/071` | outside | [docs/registry/implementation-map.yaml:L4911-L4919] |
| `orcaflex-text-model-writers/072` | outside | [docs/registry/implementation-map.yaml:L4920-L4928] |
| `orcaflex-text-model-writers/073` | outside | [docs/registry/implementation-map.yaml:L4929-L4937] |
| `orcaflex-text-model-writers/074` | outside | [docs/registry/implementation-map.yaml:L4938-L4946] |
| `orcaflex-text-model-writers/075` | outside | [docs/registry/implementation-map.yaml:L4947-L4955] |
| `orcaflex-text-model-writers/076` | outside | [docs/registry/implementation-map.yaml:L4956-L4964] |
| `orcaflex-text-model-writers/077` | outside | [docs/registry/implementation-map.yaml:L4965-L4973] |
| `orcaflex-text-model-writers/078` | outside | [docs/registry/implementation-map.yaml:L4974-L4982] |
| `orcaflex-text-model-writers/079` | outside | [docs/registry/implementation-map.yaml:L4983-L4993] |
| `orcaflex-text-model-writers/080` | outside | [docs/registry/implementation-map.yaml:L4994-L5002] |
| `orcaflex-text-model-writers/081` | outside | [docs/registry/implementation-map.yaml:L5003-L5011] |
| `orcaflex-text-model-writers/082` | outside | [docs/registry/implementation-map.yaml:L5012-L5022] |
| `orcaflex-text-model-writers/083` | outside | [docs/registry/implementation-map.yaml:L5023-L5031] |
| `orcaflex-text-model-writers/084` | outside | [docs/registry/implementation-map.yaml:L5032-L5040] |
| `orcaflex-text-model-writers/085` | outside | [docs/registry/implementation-map.yaml:L5041-L5049] |
| `orcaflex-text-model-writers/086` | outside | [docs/registry/implementation-map.yaml:L5050-L5060] |
| `orcaflex-text-model-writers/087` | outside | [docs/registry/implementation-map.yaml:L5061-L5069] |
| `orcaflex-text-model-writers/088` | outside | [docs/registry/implementation-map.yaml:L5070-L5078] |
| `orcaflex-text-model-writers/089` | outside | [docs/registry/implementation-map.yaml:L5079-L5087] |
| `orcaflex-text-model-writers/090` | outside | [docs/registry/implementation-map.yaml:L5088-L5096] |
| `orcaflex-text-model-writers/091` | outside | [docs/registry/implementation-map.yaml:L5097-L5105] |
| `orcaflex-text-model-writers/092` | outside | [docs/registry/implementation-map.yaml:L5106-L5114] |
| `orcaflex-text-model-writers/093` | outside | [docs/registry/implementation-map.yaml:L5115-L5123] |
| `orcaflex-text-model-writers/094` | outside | [docs/registry/implementation-map.yaml:L5124-L5132] |
| `orcaflex-text-model-writers/095` | outside | [docs/registry/implementation-map.yaml:L5133-L5141] |
| `orcaflex-text-model-writers/096` | outside | [docs/registry/implementation-map.yaml:L5142-L5150] |
| `orcaflex-text-model-writers/097` | outside | [docs/registry/implementation-map.yaml:L5151-L5161] |
| `orcaflex-text-model-writers/098` | outside | [docs/registry/implementation-map.yaml:L5162-L5170] |
| `orcaflex-text-model-writers/099` | outside | [docs/registry/implementation-map.yaml:L5171-L5179] |
| `orcaflex-text-model-writers/100` | outside | [docs/registry/implementation-map.yaml:L5180-L5190] |
| `orcaflex-text-model-writers/101` | outside | [docs/registry/implementation-map.yaml:L5191-L5201] |
| `orcaflex-text-model-writers/102` | outside | [docs/registry/implementation-map.yaml:L5202-L5212] |
| `orcaflex-text-model-writers/103` | outside | [docs/registry/implementation-map.yaml:L5213-L5221] |
| `orcaflex-text-model-writers/104` | outside | [docs/registry/implementation-map.yaml:L5222-L5230] |
| `orcaflex-text-model-writers/105` | outside | [docs/registry/implementation-map.yaml:L5231-L5239] |
| `orcaflex-text-model-writers/106` | outside | [docs/registry/implementation-map.yaml:L5240-L5248] |
| `orcaflex-text-model-writers/107` | outside | [docs/registry/implementation-map.yaml:L5249-L5257] |
| `orcaflex-text-model-writers/108` | outside | [docs/registry/implementation-map.yaml:L5258-L5266] |
| `orcaflex-text-model-writers/109` | outside | [docs/registry/implementation-map.yaml:L5267-L5275] |
| `orcaflex-text-model-writers/110` | outside | [docs/registry/implementation-map.yaml:L5276-L5284] |
| `orcaflex-text-model-writers/111` | outside | [docs/registry/implementation-map.yaml:L5285-L5293] |
| `orcaflex-text-model-writers/112` | outside | [docs/registry/implementation-map.yaml:L5294-L5302] |
| `orcaflex-text-model-writers/113` | outside | [docs/registry/implementation-map.yaml:L5303-L5313] |
| `orcaflex-text-model-writers/114` | outside | [docs/registry/implementation-map.yaml:L5314-L5325] |
| `orcaflex-text-model-writers/115` | outside | [docs/registry/implementation-map.yaml:L5326-L5334] |
| `orcaflex-text-model-writers/116` | outside | [docs/registry/implementation-map.yaml:L5335-L5343] |
| `orcaflex-text-model-writers/117` | outside | [docs/registry/implementation-map.yaml:L5344-L5354] |
| `orcaflex-text-model-writers/118` | outside | [docs/registry/implementation-map.yaml:L5355-L5365] |
| `orcaflex-text-model-writers/119` | outside | [docs/registry/implementation-map.yaml:L5366-L5374] |
| `orcaflex-text-model-writers/120` | outside | [docs/registry/implementation-map.yaml:L5375-L5383] |
| `orcaflex-text-model-writers/121` | outside | [docs/registry/implementation-map.yaml:L5384-L5392] |
| `orcaflex-text-model-writers/122` | outside | [docs/registry/implementation-map.yaml:L5393-L5401] |
| `orcaflex-text-model-writers/123` | outside | [docs/registry/implementation-map.yaml:L5402-L5410] |
| `orcaflex-text-model-writers/124` | outside | [docs/registry/implementation-map.yaml:L5411-L5419] |
| `orcaflex-text-model-writers/125` | outside | [docs/registry/implementation-map.yaml:L5420-L5428] |
| `orcaflex-text-model-writers/126` | outside | [docs/registry/implementation-map.yaml:L5429-L5437] |
| `orcaflex-text-model-writers/127` | outside | [docs/registry/implementation-map.yaml:L5438-L5446] |
| `orcaflex-text-model-writers/128` | outside | [docs/registry/implementation-map.yaml:L5447-L5455] |
| `orcaflex-text-model-writers/129` | outside | [docs/registry/implementation-map.yaml:L5456-L5464] |
| `orcaflex-text-model-writers/130` | outside | [docs/registry/implementation-map.yaml:L5465-L5473] |
| `orcaflex-text-model-writers/131` | outside | [docs/registry/implementation-map.yaml:L5474-L5482] |
| `orcaflex-text-model-writers/132` | outside | [docs/registry/implementation-map.yaml:L5483-L5491] |
| `orcaflex-text-model-writers/133` | outside | [docs/registry/implementation-map.yaml:L5492-L5500] |
| `orcaflex-text-model-writers/134` | outside | [docs/registry/implementation-map.yaml:L5501-L5509] |
| `orcaflex-text-model-writers/135` | outside | [docs/registry/implementation-map.yaml:L5510-L5518] |
| `orcaflex-text-model-writers/136` | outside | [docs/registry/implementation-map.yaml:L5519-L5527] |
| `orcaflex-text-model-writers/137` | outside | [docs/registry/implementation-map.yaml:L5528-L5536] |
| `orcaflex-text-model-writers/138` | outside | [docs/registry/implementation-map.yaml:L5537-L5547] |
| `orcaflex-text-model-writers/139` | outside | [docs/registry/implementation-map.yaml:L5548-L5558] |
| `orcaflex-text-model-writers/140` | outside | [docs/registry/implementation-map.yaml:L5559-L5567] |
| `orcaflex-text-model-writers/141` | outside | [docs/registry/implementation-map.yaml:L5568-L5576] |
| `orcaflex-text-model-writers/142` | outside | [docs/registry/implementation-map.yaml:L5577-L5587] |
| `orcaflex-text-model-writers/143` | outside | [docs/registry/implementation-map.yaml:L5588-L5596] |
| `orcaflex-text-model-writers/144` | outside | [docs/registry/implementation-map.yaml:L5597-L5607] |
| `orcaflex-text-model-writers/145` | outside | [docs/registry/implementation-map.yaml:L5608-L5616] |
| `orcaflex-text-model-writers/146` | outside | [docs/registry/implementation-map.yaml:L5617-L5628] |
| `orcaflex-text-model-writers/147` | outside | [docs/registry/implementation-map.yaml:L5629-L5641] |
| `orcaflex-text-model-writers/148` | outside | [docs/registry/implementation-map.yaml:L5642-L5652] |
| `orcaflex-text-model-writers/149` | outside | [docs/registry/implementation-map.yaml:L5653-L5663] |
| `orcaflex-text-model-writers/150` | outside | [docs/registry/implementation-map.yaml:L5664-L5672] |
| `orcaflex-text-model-writers/151` | outside | [docs/registry/implementation-map.yaml:L5673-L5683] |
| `orcaflex-text-model-writers/152` | outside | [docs/registry/implementation-map.yaml:L5684-L5692] |
| `orcaflex-text-model-writers/153` | outside | [docs/registry/implementation-map.yaml:L5693-L5701] |
| `orcaflex-text-model-writers/154` | outside | [docs/registry/implementation-map.yaml:L5702-L5710] |
| `orcaflex-text-model-writers/155` | outside | [docs/registry/implementation-map.yaml:L5711-L5719] |
| `orcaflex-text-model-writers/156` | outside | [docs/registry/implementation-map.yaml:L5720-L5728] |
| `orcaflex-text-model-writers/157` | outside | [docs/registry/implementation-map.yaml:L5729-L5737] |
| `orcaflex-text-model-writers/158` | outside | [docs/registry/implementation-map.yaml:L5738-L5746] |
| `orcaflex-text-model-writers/159` | outside | [docs/registry/implementation-map.yaml:L5747-L5755] |
| `orcaflex-text-model-writers/160` | outside | [docs/registry/implementation-map.yaml:L5756-L5764] |
| `orcaflex-text-model-writers/161` | outside | [docs/registry/implementation-map.yaml:L5765-L5773] |
| `orcaflex-text-model-writers/162` | outside | [docs/registry/implementation-map.yaml:L5774-L5784] |
| `orcaflex-text-model-writers/163` | outside | [docs/registry/implementation-map.yaml:L5785-L5795] |
| `orcaflex-text-model-writers/164` | outside | [docs/registry/implementation-map.yaml:L5796-L5804] |
| `orcaflex-text-model-writers/165` | outside | [docs/registry/implementation-map.yaml:L5805-L5813] |
| `orcaflex-text-model-writers/166` | outside | [docs/registry/implementation-map.yaml:L5814-L5824] |
| `orcaflex-text-model-writers/167` | outside | [docs/registry/implementation-map.yaml:L5825-L5835] |
| `orcaflex-text-model-writers/168` | outside | [docs/registry/implementation-map.yaml:L5836-L5844] |
| `orcaflex-text-model-writers/169` | outside | [docs/registry/implementation-map.yaml:L5845-L5856] |
| `orcaflex-text-model-writers/170` | outside | [docs/registry/implementation-map.yaml:L5857-L5865] |
| `orcaflex-text-model-writers/171` | outside | [docs/registry/implementation-map.yaml:L5866-L5874] |
| `orcaflex-text-model-writers/172` | outside | [docs/registry/implementation-map.yaml:L5875-L5885] |
| `orcaflex-text-model-writers/173` | outside | [docs/registry/implementation-map.yaml:L5886-L5894] |
| `orcaflex-text-model-writers/174` | inside | `src/digitalmodel/orcaflex/model_builder.py::VesselConfig.to_orcaflex_dict`; [docs/registry/implementation-map.yaml:L5895-L5903] |
| `orcaflex-text-model-writers/175` | inside | `src/digitalmodel/orcaflex/model_builder.py::Buoy6DConfig.to_orcaflex_dict`; [docs/registry/implementation-map.yaml:L5904-L5912] |
| `orcaflex-text-model-writers/176` | inside | `src/digitalmodel/orcaflex/model_builder.py::LineConfig.to_orcaflex_dict`; [docs/registry/implementation-map.yaml:L5913-L5921] |
| `orcaflex-text-model-writers/177` | inside | `src/digitalmodel/orcaflex/model_builder.py::build_scr_model`; [docs/registry/implementation-map.yaml:L5922-L5930] |
| `orcaflex-text-model-writers/178` | inside | `src/digitalmodel/orcaflex/model_builder.py::build_lazy_wave_model`; [docs/registry/implementation-map.yaml:L5931-L5939] |
| `orcaflex-text-model-writers/179` | inside | `src/digitalmodel/orcaflex/model_builder.py::build_mooring_line`; [docs/registry/implementation-map.yaml:L5940-L5948] |
| `orcaflex-text-model-writers/180` | outside | [docs/registry/implementation-map.yaml:L5949-L5957] |
| `orcaflex-text-model-writers/181` | outside | [docs/registry/implementation-map.yaml:L5958-L5966] |
| `orcaflex-text-model-writers/182` | outside | [docs/registry/implementation-map.yaml:L5967-L5975] |
| `orcaflex-text-model-writers/183` | outside | [docs/registry/implementation-map.yaml:L5976-L5984] |
| `orcaflex-text-model-writers/184` | outside | [docs/registry/implementation-map.yaml:L5985-L5993] |
| `orcaflex-text-model-writers/185` | outside | [docs/registry/implementation-map.yaml:L5994-L6002] |
| `orcaflex-text-model-writers/186` | outside | [docs/registry/implementation-map.yaml:L6003-L6011] |
| `orcaflex-text-model-writers/187` | outside | [docs/registry/implementation-map.yaml:L6012-L6020] |
| `orcaflex-text-model-writers/188` | outside | [docs/registry/implementation-map.yaml:L6021-L6029] |
| `orcaflex-text-model-writers/189` | outside | [docs/registry/implementation-map.yaml:L6030-L6038] |
| `orcaflex-text-model-writers/190` | outside | [docs/registry/implementation-map.yaml:L6039-L6047] |
| `orcaflex-text-model-writers/191` | outside | [docs/registry/implementation-map.yaml:L6048-L6056] |
| `orcaflex-text-model-writers/192` | outside | [docs/registry/implementation-map.yaml:L6057-L6065] |
| `orcaflex-text-model-writers/193` | outside | [docs/registry/implementation-map.yaml:L6066-L6074] |
| `orcaflex-text-model-writers/194` | outside | [docs/registry/implementation-map.yaml:L6075-L6086] |
| `orcaflex-text-model-writers/195` | outside | [docs/registry/implementation-map.yaml:L6087-L6095] |
| `orcaflex-text-model-writers/196` | outside | [docs/registry/implementation-map.yaml:L6096-L6104] |
| `orcaflex-text-model-writers/197` | outside | [docs/registry/implementation-map.yaml:L6105-L6113] |
| `orcaflex-text-model-writers/198` | outside | [docs/registry/implementation-map.yaml:L6114-L6122] |
| `orcaflex-text-model-writers/199` | outside | [docs/registry/implementation-map.yaml:L6123-L6131] |
| `orcaflex-text-model-writers/200` | outside | [docs/registry/implementation-map.yaml:L6132-L6140] |
| `orcaflex-text-model-writers/201` | outside | [docs/registry/implementation-map.yaml:L6141-L6149] |
| `orcaflex-text-model-writers/202` | outside | [docs/registry/implementation-map.yaml:L6150-L6158] |
| `orcaflex-text-model-writers/203` | outside | [docs/registry/implementation-map.yaml:L6159-L6167] |
| `orcaflex-text-model-writers/204` | outside | [docs/registry/implementation-map.yaml:L6168-L6180] |
| `orcaflex-text-model-writers/205` | outside | [docs/registry/implementation-map.yaml:L6181-L6189] |
| `orcaflex-text-model-writers/206` | outside | [docs/registry/implementation-map.yaml:L6190-L6198] |
| `orcaflex-text-model-writers/207` | outside | [docs/registry/implementation-map.yaml:L6199-L6207] |
| `orcaflex-text-model-writers/208` | outside | [docs/registry/implementation-map.yaml:L6208-L6216] |
| `orcaflex-text-model-writers/209` | outside | [docs/registry/implementation-map.yaml:L6217-L6225] |
| `orcaflex-text-model-writers/210` | outside | [docs/registry/implementation-map.yaml:L6226-L6234] |
| `orcaflex-text-model-writers/211` | outside | [docs/registry/implementation-map.yaml:L6235-L6243] |
| `orcaflex-text-model-writers/212` | outside | [docs/registry/implementation-map.yaml:L6244-L6252] |
| `orcaflex-text-model-writers/213` | outside | [docs/registry/implementation-map.yaml:L6253-L6261] |
| `orcaflex-text-model-writers/214` | outside | [docs/registry/implementation-map.yaml:L6262-L6270] |
| `orcaflex-text-model-writers/215` | outside | [docs/registry/implementation-map.yaml:L6271-L6279] |
| `orcaflex-text-model-writers/216` | outside | [docs/registry/implementation-map.yaml:L6280-L6290] |
| `orcaflex-text-model-writers/217` | outside | [docs/registry/implementation-map.yaml:L6291-L6299] |
| `orcaflex-text-model-writers/218` | outside | [docs/registry/implementation-map.yaml:L6300-L6311] |
| `orcaflex-text-model-writers/219` | outside | [docs/registry/implementation-map.yaml:L6312-L6320] |
| `orcaflex-text-model-writers/220` | outside | [docs/registry/implementation-map.yaml:L6321-L6329] |
| `orcaflex-text-model-writers/221` | outside | [docs/registry/implementation-map.yaml:L6330-L6339] |
| `orcaflex-text-model-writers/222` | outside | [docs/registry/implementation-map.yaml:L6340-L6348] |
| `orcaflex-text-model-writers/223` | outside | [docs/registry/implementation-map.yaml:L6349-L6357] |
| `orcaflex-text-model-writers/224` | outside | [docs/registry/implementation-map.yaml:L6358-L6366] |
| `orcaflex-text-model-writers/225` | outside | [docs/registry/implementation-map.yaml:L6367-L6375] |
| `orcaflex-text-model-writers/226` | outside | [docs/registry/implementation-map.yaml:L6376-L6384] |
| `orcaflex-text-model-writers/227` | outside | [docs/registry/implementation-map.yaml:L6385-L6395] |
| `orcaflex-text-model-writers/228` | outside | [docs/registry/implementation-map.yaml:L6396-L6404] |
| `orcaflex-text-model-writers/229` | outside | [docs/registry/implementation-map.yaml:L6405-L6413] |
| `orcaflex-text-model-writers/230` | outside | [docs/registry/implementation-map.yaml:L6414-L6422] |
| `orcaflex-text-model-writers/231` | outside | [docs/registry/implementation-map.yaml:L6423-L6431] |
| `orcaflex-text-model-writers/232` | outside | [docs/registry/implementation-map.yaml:L6432-L6440] |
| `orcaflex-text-model-writers/233` | outside | [docs/registry/implementation-map.yaml:L6441-L6449] |
| `orcaflex-text-model-writers/234` | outside | [docs/registry/implementation-map.yaml:L6450-L6458] |
| `orcaflex-text-model-writers/235` | outside | [docs/registry/implementation-map.yaml:L6459-L6467] |
| `orcaflex-text-model-writers/236` | outside | [docs/registry/implementation-map.yaml:L6468-L6476] |
| `orcaflex-text-model-writers/237` | outside | [docs/registry/implementation-map.yaml:L6477-L6485] |
| `orcaflex-text-model-writers/238` | outside | [docs/registry/implementation-map.yaml:L6486-L6494] |
| `orcaflex-text-model-writers/239` | outside | [docs/registry/implementation-map.yaml:L6495-L6503] |
| `orcaflex-text-model-writers/240` | outside | [docs/registry/implementation-map.yaml:L6504-L6512] |
| `orcaflex-text-model-writers/241` | outside | [docs/registry/implementation-map.yaml:L6513-L6521] |
| `orcaflex-text-model-writers/242` | outside | [docs/registry/implementation-map.yaml:L6522-L6530] |
| `orcaflex-text-model-writers/243` | outside | [docs/registry/implementation-map.yaml:L6531-L6539] |
| `orcaflex-text-model-writers/244` | outside | [docs/registry/implementation-map.yaml:L6540-L6548] |
| `orcaflex-text-model-writers/245` | outside | [docs/registry/implementation-map.yaml:L6549-L6557] |
| `orcaflex-text-model-writers/246` | outside | [docs/registry/implementation-map.yaml:L6558-L6566] |
| `orcaflex-text-model-writers/247` | outside | [docs/registry/implementation-map.yaml:L6567-L6575] |
| `orcaflex-text-model-writers/248` | outside | [docs/registry/implementation-map.yaml:L6576-L6584] |
| `orcaflex-text-model-writers/249` | outside | [docs/registry/implementation-map.yaml:L6585-L6593] |
| `orcaflex-text-model-writers/250` | outside | [docs/registry/implementation-map.yaml:L6594-L6602] |
| `orcaflex-text-model-writers/251` | outside | [docs/registry/implementation-map.yaml:L6603-L6611] |
| `orcaflex-text-model-writers/252` | outside | [docs/registry/implementation-map.yaml:L6612-L6620] |
| `orcaflex-text-model-writers/253` | outside | [docs/registry/implementation-map.yaml:L6621-L6629] |
| `orcaflex-text-model-writers/254` | outside | [docs/registry/implementation-map.yaml:L6630-L6638] |
| `orcaflex-text-model-writers/255` | outside | [docs/registry/implementation-map.yaml:L6639-L6649] |
| `orcaflex-text-model-writers/256` | outside | [docs/registry/implementation-map.yaml:L6650-L6658] |
| `orcaflex-text-model-writers/257` | outside | [docs/registry/implementation-map.yaml:L6659-L6667] |
| `orcaflex-text-model-writers/258` | outside | [docs/registry/implementation-map.yaml:L6668-L6676] |
| `orcaflex-text-model-writers/259` | outside | [docs/registry/implementation-map.yaml:L6677-L6685] |
| `orcaflex-text-model-writers/260` | outside | [docs/registry/implementation-map.yaml:L6686-L6696] |
| `orcaflex-text-model-writers/261` | outside | [docs/registry/implementation-map.yaml:L6697-L6705] |
| `orcaflex-text-model-writers/262` | outside | [docs/registry/implementation-map.yaml:L6706-L6717] |
| `orcaflex-text-model-writers/263` | outside | [docs/registry/implementation-map.yaml:L6718-L6726] |
| `orcaflex-text-model-writers/264` | outside | [docs/registry/implementation-map.yaml:L6727-L6735] |
| `orcaflex-text-model-writers/265` | outside | [docs/registry/implementation-map.yaml:L6736-L6744] |
| `orcaflex-text-model-writers/266` | outside | [docs/registry/implementation-map.yaml:L6745-L6753] |
| `orcaflex-text-model-writers/267` | outside | [docs/registry/implementation-map.yaml:L6754-L6762] |
| `orcaflex-text-model-writers/268` | outside | [docs/registry/implementation-map.yaml:L6763-L6771] |
| `orcaflex-text-model-writers/269` | outside | [docs/registry/implementation-map.yaml:L6772-L6780] |
| `orcaflex-text-model-writers/270` | outside | [docs/registry/implementation-map.yaml:L6781-L6789] |
| `orcaflex-text-model-writers/271` | outside | [docs/registry/implementation-map.yaml:L6790-L6798] |
| `orcaflex-text-model-writers/272` | outside | [docs/registry/implementation-map.yaml:L6799-L6810] |
| `orcaflex-text-model-writers/273` | outside | [docs/registry/implementation-map.yaml:L6811-L6819] |
| `orcaflex-text-model-writers/274` | outside | [docs/registry/implementation-map.yaml:L6820-L6828] |
| `orcaflex-text-model-writers/275` | outside | [docs/registry/implementation-map.yaml:L6829-L6840] |
| `orcaflex-text-model-writers/276` | outside | [docs/registry/implementation-map.yaml:L6841-L6853] |
| `orcaflex-text-model-writers/277` | outside | [docs/registry/implementation-map.yaml:L6854-L6864] |
| `orcaflex-text-model-writers/278` | outside | [docs/registry/implementation-map.yaml:L6865-L6875] |
| `orcaflex-text-model-writers/279` | outside | [docs/registry/implementation-map.yaml:L6876-L6884] |
| `orcaflex-text-model-writers/280` | outside | [docs/registry/implementation-map.yaml:L6885-L6895] |
| `orcaflex-text-model-writers/281` | outside | [docs/registry/implementation-map.yaml:L6896-L6904] |
| `orcaflex-text-model-writers/282` | outside | [docs/registry/implementation-map.yaml:L6905-L6913] |
| `orcaflex-text-model-writers/283` | outside | [docs/registry/implementation-map.yaml:L6914-L6922] |
| `orcaflex-text-model-writers/284` | outside | [docs/registry/implementation-map.yaml:L6923-L6931] |
| `orcaflex-text-model-writers/285` | outside | [docs/registry/implementation-map.yaml:L6932-L6940] |
| `orcaflex-text-model-writers/286` | outside | [docs/registry/implementation-map.yaml:L6941-L6951] |
| `orcaflex-text-model-writers/287` | outside | [docs/registry/implementation-map.yaml:L6952-L6960] |
| `orcaflex-text-model-writers/288` | outside | [docs/registry/implementation-map.yaml:L6961-L6969] |
| `orcaflex-text-model-writers/289` | outside | [docs/registry/implementation-map.yaml:L6970-L6978] |
| `orcaflex-text-model-writers/290` | outside | [docs/registry/implementation-map.yaml:L6979-L6990] |
| `orcaflex-text-model-writers/291` | outside | [docs/registry/implementation-map.yaml:L6991-L6999] |
| `orcaflex-text-model-writers/292` | outside | [docs/registry/implementation-map.yaml:L7000-L7008] |
| `orcaflex-text-model-writers/293` | outside | [docs/registry/implementation-map.yaml:L7009-L7017] |
| `orcaflex-text-model-writers/294` | outside | [docs/registry/implementation-map.yaml:L7018-L7026] |
| `orcaflex-text-model-writers/295` | outside | [docs/registry/implementation-map.yaml:L7027-L7035] |
| `orcaflex-text-model-writers/296` | outside | [docs/registry/implementation-map.yaml:L7036-L7044] |
| `orcaflex-text-model-writers/297` | outside | [docs/registry/implementation-map.yaml:L7045-L7053] |
| `orcaflex-text-model-writers/298` | outside | [docs/registry/implementation-map.yaml:L7054-L7062] |
| `orcaflex-text-model-writers/299` | outside | [docs/registry/implementation-map.yaml:L7063-L7071] |
| `orcaflex-text-model-writers/300` | outside | [docs/registry/implementation-map.yaml:L7072-L7080] |
| `orcaflex-text-model-writers/301` | outside | [docs/registry/implementation-map.yaml:L7081-L7089] |
| `orcaflex-text-model-writers/302` | outside | [docs/registry/implementation-map.yaml:L7090-L7100] |
| `orcaflex-text-model-writers/303` | outside | [docs/registry/implementation-map.yaml:L7101-L7109] |
| `orcaflex-text-model-writers/304` | outside | [docs/registry/implementation-map.yaml:L7110-L7118] |
| `orcaflex-text-model-writers/305` | outside | [docs/registry/implementation-map.yaml:L7119-L7129] |
| `orcaflex-text-model-writers/306` | outside | [docs/registry/implementation-map.yaml:L7130-L7138] |
| `orcaflex-text-model-writers/307` | outside | [docs/registry/implementation-map.yaml:L7139-L7147] |
| `orcaflex-text-model-writers/308` | outside | [docs/registry/implementation-map.yaml:L7148-L7158] |
| `orcaflex-text-model-writers/309` | outside | [docs/registry/implementation-map.yaml:L7159-L7167] |
| `orcaflex-text-model-writers/310` | outside | [docs/registry/implementation-map.yaml:L7168-L7176] |
| `orcaflex-text-model-writers/311` | outside | [docs/registry/implementation-map.yaml:L7177-L7185] |
| `orcaflex-text-model-writers/312` | outside | [docs/registry/implementation-map.yaml:L7186-L7194] |
| `orcaflex-text-model-writers/313` | outside | [docs/registry/implementation-map.yaml:L7195-L7205] |
| `orcaflex-text-model-writers/314` | outside | [docs/registry/implementation-map.yaml:L7206-L7214] |
| `orcaflex-text-model-writers/315` | outside | [docs/registry/implementation-map.yaml:L7215-L7223] |
| `orcaflex-text-model-writers/316` | outside | [docs/registry/implementation-map.yaml:L7224-L7232] |
| `orcaflex-text-model-writers/317` | outside | [docs/registry/implementation-map.yaml:L7233-L7241] |
| `orcaflex-text-model-writers/318` | outside | [docs/registry/implementation-map.yaml:L7242-L7250] |
| `orcaflex-text-model-writers/319` | outside | [docs/registry/implementation-map.yaml:L7251-L7259] |
| `orcaflex-text-model-writers/320` | outside | [docs/registry/implementation-map.yaml:L7260-L7270] |
| `orcaflex-text-model-writers/321` | outside | [docs/registry/implementation-map.yaml:L7271-L7279] |
| `orcaflex-text-model-writers/322` | outside | [docs/registry/implementation-map.yaml:L7280-L7288] |
| `orcaflex-text-model-writers/323` | outside | [docs/registry/implementation-map.yaml:L7289-L7297] |
| `orcaflex-text-model-writers/324` | outside | [docs/registry/implementation-map.yaml:L7298-L7306] |
| `orcaflex-text-model-writers/325` | outside | [docs/registry/implementation-map.yaml:L7307-L7317] |
| `orcaflex-text-model-writers/326` | outside | [docs/registry/implementation-map.yaml:L7318-L7327] |
| `orcaflex-text-model-writers/327` | outside | [docs/registry/implementation-map.yaml:L7328-L7336] |
| `orcaflex-text-model-writers/328` | outside | [docs/registry/implementation-map.yaml:L7337-L7345] |
| `orcaflex-text-model-writers/329` | outside | [docs/registry/implementation-map.yaml:L7346-L7357] |
| `orcaflex-text-model-writers/330` | outside | [docs/registry/implementation-map.yaml:L7358-L7366] |
| `orcaflex-text-model-writers/331` | outside | [docs/registry/implementation-map.yaml:L7367-L7375] |
| `orcaflex-text-model-writers/332` | outside | [docs/registry/implementation-map.yaml:L7376-L7384] |
| `orcaflex-text-model-writers/333` | outside | [docs/registry/implementation-map.yaml:L7385-L7393] |
| `orcaflex-text-model-writers/334` | outside | [docs/registry/implementation-map.yaml:L7394-L7405] |
| `orcaflex-text-model-writers/335` | outside | [docs/registry/implementation-map.yaml:L7406-L7417] |
| `orcaflex-text-model-writers/336` | outside | [docs/registry/implementation-map.yaml:L7418-L7429] |
| `orcaflex-text-model-writers/337` | outside | [docs/registry/implementation-map.yaml:L7430-L7438] |
| `orcaflex-text-model-writers/338` | outside | [docs/registry/implementation-map.yaml:L7439-L7447] |
| `orcaflex-text-model-writers/339` | outside | [docs/registry/implementation-map.yaml:L7448-L7456] |
| `orcaflex-text-model-writers/340` | outside | [docs/registry/implementation-map.yaml:L7457-L7467] |
| `orcaflex-text-model-writers/341` | outside | [docs/registry/implementation-map.yaml:L7468-L7476] |
| `orcaflex-text-model-writers/342` | outside | [docs/registry/implementation-map.yaml:L7477-L7487] |
| `orcaflex-text-model-writers/343` | outside | [docs/registry/implementation-map.yaml:L7488-L7499] |
| `orcaflex-text-model-writers/344` | outside | [docs/registry/implementation-map.yaml:L7500-L7508] |
| `orcaflex-text-model-writers/345` | outside | [docs/registry/implementation-map.yaml:L7509-L7519] |
| `orcaflex-text-model-writers/346` | outside | [docs/registry/implementation-map.yaml:L7520-L7531] |
| `orcaflex-text-model-writers/347` | outside | [docs/registry/implementation-map.yaml:L7532-L7542] |
| `orcaflex-text-model-writers/348` | outside | [docs/registry/implementation-map.yaml:L7543-L7551] |
| `orcaflex-text-model-writers/349` | outside | [docs/registry/implementation-map.yaml:L7552-L7560] |
| `orcaflex-text-model-writers/350` | outside | [docs/registry/implementation-map.yaml:L7561-L7569] |
| `orcaflex-text-model-writers/351` | outside | [docs/registry/implementation-map.yaml:L7570-L7581] |
| `orcaflex-text-model-writers/352` | outside | [docs/registry/implementation-map.yaml:L7582-L7592] |
| `orcaflex-text-model-writers/353` | outside | [docs/registry/implementation-map.yaml:L7593-L7601] |
| `orcaflex-text-model-writers/354` | outside | [docs/registry/implementation-map.yaml:L7602-L7614] |
| `orcaflex-text-model-writers/355` | outside | [docs/registry/implementation-map.yaml:L7615-L7625] |
| `orcaflex-text-model-writers/356` | outside | [docs/registry/implementation-map.yaml:L7626-L7635] |
| `orcaflex-text-model-writers/357` | outside | [docs/registry/implementation-map.yaml:L7636-L7647] |
| `orcaflex-text-model-writers/358` | outside | [docs/registry/implementation-map.yaml:L7648-L7661] |
| `orcaflex-text-model-writers/359` | outside | [docs/registry/implementation-map.yaml:L7662-L7670] |
| `orcaflex-text-model-writers/360` | outside | [docs/registry/implementation-map.yaml:L7671-L7679] |
| `orcaflex-text-model-writers/361` | outside | [docs/registry/implementation-map.yaml:L7680-L7688] |
| `orcaflex-text-model-writers/362` | outside | [docs/registry/implementation-map.yaml:L7689-L7697] |
| `orcaflex-text-model-writers/363` | outside | [docs/registry/implementation-map.yaml:L7698-L7710] |
| `orcaflex-text-model-writers/364` | outside | [docs/registry/implementation-map.yaml:L7711-L7722] |
| `orcaflex-text-model-writers/365` | outside | [docs/registry/implementation-map.yaml:L7723-L7731] |
| `orcaflex-text-model-writers/366` | outside | [docs/registry/implementation-map.yaml:L7732-L7745] |
| `orcaflex-text-model-writers/367` | outside | [docs/registry/implementation-map.yaml:L7746-L7754] |
| `orcaflex-text-model-writers/368` | outside | [docs/registry/implementation-map.yaml:L7755-L7763] |
| `orcaflex-text-model-writers/369` | outside | [docs/registry/implementation-map.yaml:L7764-L7776] |
| `orcaflex-text-model-writers/370` | outside | [docs/registry/implementation-map.yaml:L7777-L7785] |
| `orcaflex-text-model-writers/371` | outside | [docs/registry/implementation-map.yaml:L7786-L7794] |
| `orcaflex-text-model-writers/372` | outside | [docs/registry/implementation-map.yaml:L7795-L7803] |
| `orcaflex-text-model-writers/373` | outside | [docs/registry/implementation-map.yaml:L7804-L7812] |
| `orcaflex-text-model-writers/374` | outside | [docs/registry/implementation-map.yaml:L7813-L7821] |
| `orcaflex-text-model-writers/375` | outside | [docs/registry/implementation-map.yaml:L7822-L7830] |
| `orcaflex-text-model-writers/376` | outside | [docs/registry/implementation-map.yaml:L7831-L7839] |
| `orcaflex-text-model-writers/377` | outside | [docs/registry/implementation-map.yaml:L7840-L7851] |
| `orcaflex-text-model-writers/378` | outside | [docs/registry/implementation-map.yaml:L7852-L7860] |
| `orcaflex-text-model-writers/379` | outside | [docs/registry/implementation-map.yaml:L7861-L7873] |
| `orcaflex-text-model-writers/380` | outside | [docs/registry/implementation-map.yaml:L7874-L7886] |
| `orcaflex-text-model-writers/381` | outside | [docs/registry/implementation-map.yaml:L7887-L7895] |
| `orcaflex-text-model-writers/382` | outside | [docs/registry/implementation-map.yaml:L7896-L7906] |
| `orcaflex-text-model-writers/383` | outside | [docs/registry/implementation-map.yaml:L7907-L7915] |
| `orcaflex-text-model-writers/384` | outside | [docs/registry/implementation-map.yaml:L7916-L7924] |
| `orcaflex-text-model-writers/385` | outside | [docs/registry/implementation-map.yaml:L7925-L7937] |
| `orcaflex-text-model-writers/386` | outside | [docs/registry/implementation-map.yaml:L7938-L7946] |
| `orcaflex-text-model-writers/387` | outside | [docs/registry/implementation-map.yaml:L7947-L7959] |
| `orcaflex-text-model-writers/388` | outside | [docs/registry/implementation-map.yaml:L7960-L7970] |
| `orcaflex-text-model-writers/389` | outside | [docs/registry/implementation-map.yaml:L7971-L7979] |
| `orcaflex-text-model-writers/390` | outside | [docs/registry/implementation-map.yaml:L7980-L7988] |
| `orcaflex-text-model-writers/391` | outside | [docs/registry/implementation-map.yaml:L7989-L7997] |
| `orcaflex-text-model-writers/392` | outside | [docs/registry/implementation-map.yaml:L7998-L8006] |
| `orcaflex-text-model-writers/393` | outside | [docs/registry/implementation-map.yaml:L8007-L8015] |
| `orcaflex-text-model-writers/394` | outside | [docs/registry/implementation-map.yaml:L8016-L8024] |
| `orcaflex-text-model-writers/395` | outside | [docs/registry/implementation-map.yaml:L8025-L8033] |
| `orcaflex-text-model-writers/396` | outside | [docs/registry/implementation-map.yaml:L8034-L8042] |
| `orcaflex-text-model-writers/397` | outside | [docs/registry/implementation-map.yaml:L8043-L8051] |
| `orcaflex-text-model-writers/398` | outside | [docs/registry/implementation-map.yaml:L8052-L8060] |
| `orcaflex-text-model-writers/399` | outside | [docs/registry/implementation-map.yaml:L8061-L8069] |
| `orcaflex-text-model-writers/400` | outside | [docs/registry/implementation-map.yaml:L8070-L8078] |
| `orcaflex-text-model-writers/401` | outside | [docs/registry/implementation-map.yaml:L8079-L8087] |
| `orcaflex-text-model-writers/402` | outside | [docs/registry/implementation-map.yaml:L8088-L8096] |
| `orcaflex-text-model-writers/403` | outside | [docs/registry/implementation-map.yaml:L8097-L8105] |
| `orcaflex-text-model-writers/404` | outside | [docs/registry/implementation-map.yaml:L8106-L8114] |
| `orcaflex-text-model-writers/405` | outside | [docs/registry/implementation-map.yaml:L8115-L8123] |
| `orcaflex-text-model-writers/406` | outside | [docs/registry/implementation-map.yaml:L8124-L8134] |
| `orcaflex-text-model-writers/407` | outside | [docs/registry/implementation-map.yaml:L8135-L8143] |
| `orcaflex-text-model-writers/408` | outside | [docs/registry/implementation-map.yaml:L8144-L8152] |
| `orcaflex-text-model-writers/409` | outside | [docs/registry/implementation-map.yaml:L8153-L8161] |
| `orcaflex-text-model-writers/410` | outside | [docs/registry/implementation-map.yaml:L8162-L8170] |
| `orcaflex-text-model-writers/411` | outside | [docs/registry/implementation-map.yaml:L8171-L8179] |
| `orcaflex-text-model-writers/412` | outside | [docs/registry/implementation-map.yaml:L8180-L8188] |
| `orcaflex-text-model-writers/413` | outside | [docs/registry/implementation-map.yaml:L8189-L8197] |
| `orcaflex-text-model-writers/414` | outside | [docs/registry/implementation-map.yaml:L8198-L8206] |
| `orcaflex-text-model-writers/415` | outside | [docs/registry/implementation-map.yaml:L8207-L8215] |
| `orcaflex-text-model-writers/416` | outside | [docs/registry/implementation-map.yaml:L8216-L8224] |
| `orcaflex-text-model-writers/417` | outside | [docs/registry/implementation-map.yaml:L8225-L8233] |
| `orcaflex-text-model-writers/418` | outside | [docs/registry/implementation-map.yaml:L8234-L8242] |
| `orcaflex-text-model-writers/419` | outside | [docs/registry/implementation-map.yaml:L8243-L8251] |
| `orcaflex-text-model-writers/420` | outside | [docs/registry/implementation-map.yaml:L8252-L8260] |
| `orcaflex-text-model-writers/421` | outside | [docs/registry/implementation-map.yaml:L8261-L8269] |
| `orcaflex-text-model-writers/422` | outside | [docs/registry/implementation-map.yaml:L8270-L8278] |
| `orcaflex-text-model-writers/423` | outside | [docs/registry/implementation-map.yaml:L8279-L8287] |
| `orcaflex-text-model-writers/424` | outside | [docs/registry/implementation-map.yaml:L8288-L8296] |
| `orcaflex-text-model-writers/425` | outside | [docs/registry/implementation-map.yaml:L8297-L8305] |
| `orcaflex-text-model-writers/426` | outside | [docs/registry/implementation-map.yaml:L8306-L8314] |
| `orcaflex-text-model-writers/427` | outside | [docs/registry/implementation-map.yaml:L8315-L8323] |
| `orcaflex-text-model-writers/428` | outside | [docs/registry/implementation-map.yaml:L8324-L8332] |
| `orcaflex-text-model-writers/429` | outside | [docs/registry/implementation-map.yaml:L8333-L8341] |
| `orcaflex-text-model-writers/430` | outside | [docs/registry/implementation-map.yaml:L8342-L8350] |
| `orcaflex-text-model-writers/431` | outside | [docs/registry/implementation-map.yaml:L8351-L8359] |
| `orcaflex-text-model-writers/432` | outside | [docs/registry/implementation-map.yaml:L8360-L8368] |
| `orcaflex-text-model-writers/433` | outside | [docs/registry/implementation-map.yaml:L8369-L8377] |
| `orcaflex-text-model-writers/434` | outside | [docs/registry/implementation-map.yaml:L8378-L8386] |
| `orcaflex-text-model-writers/435` | outside | [docs/registry/implementation-map.yaml:L8387-L8395] |
| `orcaflex-text-model-writers/436` | outside | [docs/registry/implementation-map.yaml:L8396-L8404] |
| `orcaflex-text-model-writers/437` | outside | [docs/registry/implementation-map.yaml:L8405-L8413] |
| `orcaflex-text-model-writers/438` | outside | [docs/registry/implementation-map.yaml:L8414-L8422] |
| `orcaflex-text-model-writers/439` | outside | [docs/registry/implementation-map.yaml:L8423-L8431] |
| `orcaflex-text-model-writers/440` | outside | [docs/registry/implementation-map.yaml:L8432-L8441] |

</details>

<details>
<summary>wave-spectra: all 139 implementation rows</summary>

| Map row | Package boundary | Definition / implementation locator |
| --- | --- | --- |
| `wave-spectra/001` | outside | [docs/registry/implementation-map.yaml:L12155-L12165] |
| `wave-spectra/002` | outside | [docs/registry/implementation-map.yaml:L12166-L12176] |
| `wave-spectra/003` | outside | [docs/registry/implementation-map.yaml:L12177-L12187] |
| `wave-spectra/004` | outside | [docs/registry/implementation-map.yaml:L12188-L12198] |
| `wave-spectra/005` | outside | [docs/registry/implementation-map.yaml:L12199-L12212] |
| `wave-spectra/006` | outside | [docs/registry/implementation-map.yaml:L12213-L12223] |
| `wave-spectra/007` | outside | [docs/registry/implementation-map.yaml:L12224-L12234] |
| `wave-spectra/008` | outside | [docs/registry/implementation-map.yaml:L12235-L12245] |
| `wave-spectra/009` | outside | [docs/registry/implementation-map.yaml:L12246-L12256] |
| `wave-spectra/010` | outside | [docs/registry/implementation-map.yaml:L12257-L12267] |
| `wave-spectra/011` | outside | [docs/registry/implementation-map.yaml:L12268-L12278] |
| `wave-spectra/012` | outside | [docs/registry/implementation-map.yaml:L12279-L12291] |
| `wave-spectra/013` | outside | [docs/registry/implementation-map.yaml:L12292-L12302] |
| `wave-spectra/014` | outside | [docs/registry/implementation-map.yaml:L12303-L12313] |
| `wave-spectra/015` | outside | [docs/registry/implementation-map.yaml:L12314-L12324] |
| `wave-spectra/016` | outside | [docs/registry/implementation-map.yaml:L12325-L12335] |
| `wave-spectra/017` | outside | [docs/registry/implementation-map.yaml:L12336-L12346] |
| `wave-spectra/018` | outside | [docs/registry/implementation-map.yaml:L12347-L12357] |
| `wave-spectra/019` | outside | [docs/registry/implementation-map.yaml:L12358-L12368] |
| `wave-spectra/020` | outside | [docs/registry/implementation-map.yaml:L12369-L12379] |
| `wave-spectra/021` | outside | [docs/registry/implementation-map.yaml:L12380-L12390] |
| `wave-spectra/022` | outside | [docs/registry/implementation-map.yaml:L12391-L12401] |
| `wave-spectra/023` | outside | [docs/registry/implementation-map.yaml:L12402-L12412] |
| `wave-spectra/024` | outside | [docs/registry/implementation-map.yaml:L12413-L12423] |
| `wave-spectra/025` | outside | [docs/registry/implementation-map.yaml:L12424-L12436] |
| `wave-spectra/026` | outside | [docs/registry/implementation-map.yaml:L12437-L12447] |
| `wave-spectra/027` | outside | [docs/registry/implementation-map.yaml:L12448-L12458] |
| `wave-spectra/028` | outside | [docs/registry/implementation-map.yaml:L12459-L12469] |
| `wave-spectra/029` | outside | [docs/registry/implementation-map.yaml:L12470-L12480] |
| `wave-spectra/030` | outside | [docs/registry/implementation-map.yaml:L12481-L12491] |
| `wave-spectra/031` | outside | [docs/registry/implementation-map.yaml:L12492-L12502] |
| `wave-spectra/032` | outside | [docs/registry/implementation-map.yaml:L12503-L12513] |
| `wave-spectra/033` | outside | [docs/registry/implementation-map.yaml:L12514-L12524] |
| `wave-spectra/034` | outside | [docs/registry/implementation-map.yaml:L12525-L12537] |
| `wave-spectra/035` | outside | [docs/registry/implementation-map.yaml:L12538-L12549] |
| `wave-spectra/036` | outside | [docs/registry/implementation-map.yaml:L12550-L12561] |
| `wave-spectra/037` | outside | [docs/registry/implementation-map.yaml:L12562-L12573] |
| `wave-spectra/038` | outside | [docs/registry/implementation-map.yaml:L12574-L12589] |
| `wave-spectra/039` | outside | [docs/registry/implementation-map.yaml:L12590-L12600] |
| `wave-spectra/040` | outside | [docs/registry/implementation-map.yaml:L12601-L12611] |
| `wave-spectra/041` | outside | [docs/registry/implementation-map.yaml:L12612-L12622] |
| `wave-spectra/042` | outside | [docs/registry/implementation-map.yaml:L12623-L12635] |
| `wave-spectra/043` | outside | [docs/registry/implementation-map.yaml:L12636-L12646] |
| `wave-spectra/044` | outside | [docs/registry/implementation-map.yaml:L12647-L12659] |
| `wave-spectra/045` | outside | [docs/registry/implementation-map.yaml:L12660-L12673] |
| `wave-spectra/046` | outside | [docs/registry/implementation-map.yaml:L12674-L12684] |
| `wave-spectra/047` | outside | [docs/registry/implementation-map.yaml:L12685-L12697] |
| `wave-spectra/048` | outside | [docs/registry/implementation-map.yaml:L12698-L12710] |
| `wave-spectra/049` | outside | [docs/registry/implementation-map.yaml:L12711-L12722] |
| `wave-spectra/050` | outside | [docs/registry/implementation-map.yaml:L12723-L12733] |
| `wave-spectra/051` | outside | [docs/registry/implementation-map.yaml:L12734-L12746] |
| `wave-spectra/052` | outside | [docs/registry/implementation-map.yaml:L12747-L12757] |
| `wave-spectra/053` | outside | [docs/registry/implementation-map.yaml:L12758-L12768] |
| `wave-spectra/054` | outside | [docs/registry/implementation-map.yaml:L12769-L12779] |
| `wave-spectra/055` | outside | [docs/registry/implementation-map.yaml:L12780-L12792] |
| `wave-spectra/056` | outside | [docs/registry/implementation-map.yaml:L12793-L12803] |
| `wave-spectra/057` | outside | [docs/registry/implementation-map.yaml:L12804-L12816] |
| `wave-spectra/058` | outside | [docs/registry/implementation-map.yaml:L12817-L12827] |
| `wave-spectra/059` | outside | [docs/registry/implementation-map.yaml:L12828-L12839] |
| `wave-spectra/060` | inside | `src/digitalmodel/orcaflex/environment.py::WaveSpectrumParams.tz`; [docs/registry/implementation-map.yaml:L12840-L12850] |
| `wave-spectra/061` | inside | `src/digitalmodel/orcaflex/environment.py::WaveSpectrumParams.spectral_density`; [docs/registry/implementation-map.yaml:L12851-L12861] |
| `wave-spectra/062` | inside | `src/digitalmodel/orcaflex/environment.py::WaveSpectrumParams.to_orcaflex_dict`; [docs/registry/implementation-map.yaml:L12862-L12872] |
| `wave-spectra/063` | outside | [docs/registry/implementation-map.yaml:L12873-L12883] |
| `wave-spectra/064` | outside | [docs/registry/implementation-map.yaml:L12884-L12894] |
| `wave-spectra/065` | outside | [docs/registry/implementation-map.yaml:L12895-L12907] |
| `wave-spectra/066` | outside | [docs/registry/implementation-map.yaml:L12908-L12919] |
| `wave-spectra/067` | outside | [docs/registry/implementation-map.yaml:L12920-L12930] |
| `wave-spectra/068` | outside | [docs/registry/implementation-map.yaml:L12931-L12941] |
| `wave-spectra/069` | outside | [docs/registry/implementation-map.yaml:L12942-L12954] |
| `wave-spectra/070` | outside | [docs/registry/implementation-map.yaml:L12955-L12971] |
| `wave-spectra/071` | outside | [docs/registry/implementation-map.yaml:L12972-L12982] |
| `wave-spectra/072` | outside | [docs/registry/implementation-map.yaml:L12983-L12993] |
| `wave-spectra/073` | outside | [docs/registry/implementation-map.yaml:L12994-L13004] |
| `wave-spectra/074` | outside | [docs/registry/implementation-map.yaml:L13005-L13015] |
| `wave-spectra/075` | outside | [docs/registry/implementation-map.yaml:L13016-L13028] |
| `wave-spectra/076` | outside | [docs/registry/implementation-map.yaml:L13029-L13039] |
| `wave-spectra/077` | outside | [docs/registry/implementation-map.yaml:L13040-L13053] |
| `wave-spectra/078` | outside | [docs/registry/implementation-map.yaml:L13054-L13066] |
| `wave-spectra/079` | outside | [docs/registry/implementation-map.yaml:L13067-L13077] |
| `wave-spectra/080` | outside | [docs/registry/implementation-map.yaml:L13078-L13088] |
| `wave-spectra/081` | outside | [docs/registry/implementation-map.yaml:L13089-L13101] |
| `wave-spectra/082` | outside | [docs/registry/implementation-map.yaml:L13102-L13115] |
| `wave-spectra/083` | outside | [docs/registry/implementation-map.yaml:L13116-L13126] |
| `wave-spectra/084` | outside | [docs/registry/implementation-map.yaml:L13127-L13139] |
| `wave-spectra/085` | outside | [docs/registry/implementation-map.yaml:L13140-L13150] |
| `wave-spectra/086` | outside | [docs/registry/implementation-map.yaml:L13151-L13161] |
| `wave-spectra/087` | outside | [docs/registry/implementation-map.yaml:L13162-L13172] |
| `wave-spectra/088` | outside | [docs/registry/implementation-map.yaml:L13173-L13183] |
| `wave-spectra/089` | outside | [docs/registry/implementation-map.yaml:L13184-L13194] |
| `wave-spectra/090` | outside | [docs/registry/implementation-map.yaml:L13195-L13205] |
| `wave-spectra/091` | outside | [docs/registry/implementation-map.yaml:L13206-L13216] |
| `wave-spectra/092` | outside | [docs/registry/implementation-map.yaml:L13217-L13227] |
| `wave-spectra/093` | outside | [docs/registry/implementation-map.yaml:L13228-L13238] |
| `wave-spectra/094` | outside | [docs/registry/implementation-map.yaml:L13239-L13249] |
| `wave-spectra/095` | outside | [docs/registry/implementation-map.yaml:L13250-L13262] |
| `wave-spectra/096` | outside | [docs/registry/implementation-map.yaml:L13263-L13276] |
| `wave-spectra/097` | outside | [docs/registry/implementation-map.yaml:L13277-L13287] |
| `wave-spectra/098` | outside | [docs/registry/implementation-map.yaml:L13288-L13298] |
| `wave-spectra/099` | outside | [docs/registry/implementation-map.yaml:L13299-L13309] |
| `wave-spectra/100` | outside | [docs/registry/implementation-map.yaml:L13310-L13320] |
| `wave-spectra/101` | outside | [docs/registry/implementation-map.yaml:L13321-L13331] |
| `wave-spectra/102` | outside | [docs/registry/implementation-map.yaml:L13332-L13342] |
| `wave-spectra/103` | outside | [docs/registry/implementation-map.yaml:L13343-L13353] |
| `wave-spectra/104` | outside | [docs/registry/implementation-map.yaml:L13354-L13364] |
| `wave-spectra/105` | outside | [docs/registry/implementation-map.yaml:L13365-L13375] |
| `wave-spectra/106` | outside | [docs/registry/implementation-map.yaml:L13376-L13386] |
| `wave-spectra/107` | outside | [docs/registry/implementation-map.yaml:L13387-L13397] |
| `wave-spectra/108` | outside | [docs/registry/implementation-map.yaml:L13398-L13408] |
| `wave-spectra/109` | outside | [docs/registry/implementation-map.yaml:L13409-L13419] |
| `wave-spectra/110` | outside | [docs/registry/implementation-map.yaml:L13420-L13430] |
| `wave-spectra/111` | outside | [docs/registry/implementation-map.yaml:L13431-L13441] |
| `wave-spectra/112` | outside | [docs/registry/implementation-map.yaml:L13442-L13452] |
| `wave-spectra/113` | outside | [docs/registry/implementation-map.yaml:L13453-L13465] |
| `wave-spectra/114` | outside | [docs/registry/implementation-map.yaml:L13466-L13478] |
| `wave-spectra/115` | outside | [docs/registry/implementation-map.yaml:L13479-L13491] |
| `wave-spectra/116` | outside | [docs/registry/implementation-map.yaml:L13492-L13504] |
| `wave-spectra/117` | outside | [docs/registry/implementation-map.yaml:L13505-L13517] |
| `wave-spectra/118` | outside | [docs/registry/implementation-map.yaml:L13518-L13530] |
| `wave-spectra/119` | outside | [docs/registry/implementation-map.yaml:L13531-L13543] |
| `wave-spectra/120` | outside | [docs/registry/implementation-map.yaml:L13544-L13556] |
| `wave-spectra/121` | outside | [docs/registry/implementation-map.yaml:L13557-L13569] |
| `wave-spectra/122` | outside | [docs/registry/implementation-map.yaml:L13570-L13582] |
| `wave-spectra/123` | outside | [docs/registry/implementation-map.yaml:L13583-L13593] |
| `wave-spectra/124` | outside | [docs/registry/implementation-map.yaml:L13594-L13604] |
| `wave-spectra/125` | outside | [docs/registry/implementation-map.yaml:L13605-L13615] |
| `wave-spectra/126` | outside | [docs/registry/implementation-map.yaml:L13616-L13626] |
| `wave-spectra/127` | outside | [docs/registry/implementation-map.yaml:L13627-L13637] |
| `wave-spectra/128` | outside | [docs/registry/implementation-map.yaml:L13638-L13648] |
| `wave-spectra/129` | outside | [docs/registry/implementation-map.yaml:L13649-L13659] |
| `wave-spectra/130` | outside | [docs/registry/implementation-map.yaml:L13660-L13670] |
| `wave-spectra/131` | outside | [docs/registry/implementation-map.yaml:L13671-L13681] |
| `wave-spectra/132` | outside | [docs/registry/implementation-map.yaml:L13682-L13692] |
| `wave-spectra/133` | outside | [docs/registry/implementation-map.yaml:L13693-L13703] |
| `wave-spectra/134` | outside | [docs/registry/implementation-map.yaml:L13704-L13714] |
| `wave-spectra/135` | outside | [docs/registry/implementation-map.yaml:L13715-L13725] |
| `wave-spectra/136` | outside | [docs/registry/implementation-map.yaml:L13726-L13736] |
| `wave-spectra/137` | outside | [docs/registry/implementation-map.yaml:L13737-L13749] |
| `wave-spectra/138` | outside | [docs/registry/implementation-map.yaml:L13750-L13760] |
| `wave-spectra/139` | outside | [docs/registry/implementation-map.yaml:L13761-L13771] |

</details>

<details>
<summary>wind-and-current-force-routes: all 131 implementation rows</summary>

| Map row | Package boundary | Definition / implementation locator |
| --- | --- | --- |
| `wind-and-current-force-routes/001` | outside | [docs/registry/implementation-map.yaml:L13782-L13790] |
| `wind-and-current-force-routes/002` | outside | [docs/registry/implementation-map.yaml:L13791-L13799] |
| `wind-and-current-force-routes/003` | outside | [docs/registry/implementation-map.yaml:L13800-L13808] |
| `wind-and-current-force-routes/004` | outside | [docs/registry/implementation-map.yaml:L13809-L13817] |
| `wind-and-current-force-routes/005` | outside | [docs/registry/implementation-map.yaml:L13818-L13826] |
| `wind-and-current-force-routes/006` | outside | [docs/registry/implementation-map.yaml:L13827-L13837] |
| `wind-and-current-force-routes/007` | outside | [docs/registry/implementation-map.yaml:L13838-L13846] |
| `wind-and-current-force-routes/008` | outside | [docs/registry/implementation-map.yaml:L13847-L13855] |
| `wind-and-current-force-routes/009` | outside | [docs/registry/implementation-map.yaml:L13856-L13864] |
| `wind-and-current-force-routes/010` | outside | [docs/registry/implementation-map.yaml:L13865-L13873] |
| `wind-and-current-force-routes/011` | outside | [docs/registry/implementation-map.yaml:L13874-L13882] |
| `wind-and-current-force-routes/012` | outside | [docs/registry/implementation-map.yaml:L13883-L13891] |
| `wind-and-current-force-routes/013` | outside | [docs/registry/implementation-map.yaml:L13892-L13900] |
| `wind-and-current-force-routes/014` | outside | [docs/registry/implementation-map.yaml:L13901-L13909] |
| `wind-and-current-force-routes/015` | outside | [docs/registry/implementation-map.yaml:L13910-L13918] |
| `wind-and-current-force-routes/016` | outside | [docs/registry/implementation-map.yaml:L13919-L13927] |
| `wind-and-current-force-routes/017` | outside | [docs/registry/implementation-map.yaml:L13928-L13936] |
| `wind-and-current-force-routes/018` | outside | [docs/registry/implementation-map.yaml:L13937-L13947] |
| `wind-and-current-force-routes/019` | outside | [docs/registry/implementation-map.yaml:L13948-L13956] |
| `wind-and-current-force-routes/020` | outside | [docs/registry/implementation-map.yaml:L13957-L13965] |
| `wind-and-current-force-routes/021` | outside | [docs/registry/implementation-map.yaml:L13966-L13976] |
| `wind-and-current-force-routes/022` | outside | [docs/registry/implementation-map.yaml:L13977-L13987] |
| `wind-and-current-force-routes/023` | outside | [docs/registry/implementation-map.yaml:L13988-L14000] |
| `wind-and-current-force-routes/024` | outside | [docs/registry/implementation-map.yaml:L14001-L14011] |
| `wind-and-current-force-routes/025` | outside | [docs/registry/implementation-map.yaml:L14012-L14022] |
| `wind-and-current-force-routes/026` | outside | [docs/registry/implementation-map.yaml:L14023-L14033] |
| `wind-and-current-force-routes/027` | outside | [docs/registry/implementation-map.yaml:L14034-L14044] |
| `wind-and-current-force-routes/028` | outside | [docs/registry/implementation-map.yaml:L14045-L14060] |
| `wind-and-current-force-routes/029` | outside | [docs/registry/implementation-map.yaml:L14061-L14069] |
| `wind-and-current-force-routes/030` | outside | [docs/registry/implementation-map.yaml:L14070-L14078] |
| `wind-and-current-force-routes/031` | outside | [docs/registry/implementation-map.yaml:L14079-L14087] |
| `wind-and-current-force-routes/032` | outside | [docs/registry/implementation-map.yaml:L14088-L14096] |
| `wind-and-current-force-routes/033` | outside | [docs/registry/implementation-map.yaml:L14097-L14105] |
| `wind-and-current-force-routes/034` | outside | [docs/registry/implementation-map.yaml:L14106-L14114] |
| `wind-and-current-force-routes/035` | outside | [docs/registry/implementation-map.yaml:L14115-L14123] |
| `wind-and-current-force-routes/036` | outside | [docs/registry/implementation-map.yaml:L14124-L14132] |
| `wind-and-current-force-routes/037` | outside | [docs/registry/implementation-map.yaml:L14133-L14141] |
| `wind-and-current-force-routes/038` | outside | [docs/registry/implementation-map.yaml:L14142-L14150] |
| `wind-and-current-force-routes/039` | outside | [docs/registry/implementation-map.yaml:L14151-L14159] |
| `wind-and-current-force-routes/040` | outside | [docs/registry/implementation-map.yaml:L14160-L14171] |
| `wind-and-current-force-routes/041` | outside | [docs/registry/implementation-map.yaml:L14172-L14180] |
| `wind-and-current-force-routes/042` | outside | [docs/registry/implementation-map.yaml:L14181-L14189] |
| `wind-and-current-force-routes/043` | outside | [docs/registry/implementation-map.yaml:L14190-L14198] |
| `wind-and-current-force-routes/044` | outside | [docs/registry/implementation-map.yaml:L14199-L14207] |
| `wind-and-current-force-routes/045` | outside | [docs/registry/implementation-map.yaml:L14208-L14216] |
| `wind-and-current-force-routes/046` | outside | [docs/registry/implementation-map.yaml:L14217-L14225] |
| `wind-and-current-force-routes/047` | outside | [docs/registry/implementation-map.yaml:L14226-L14234] |
| `wind-and-current-force-routes/048` | outside | [docs/registry/implementation-map.yaml:L14235-L14243] |
| `wind-and-current-force-routes/049` | outside | [docs/registry/implementation-map.yaml:L14244-L14252] |
| `wind-and-current-force-routes/050` | outside | [docs/registry/implementation-map.yaml:L14253-L14261] |
| `wind-and-current-force-routes/051` | outside | [docs/registry/implementation-map.yaml:L14262-L14270] |
| `wind-and-current-force-routes/052` | outside | [docs/registry/implementation-map.yaml:L14271-L14281] |
| `wind-and-current-force-routes/053` | outside | [docs/registry/implementation-map.yaml:L14282-L14292] |
| `wind-and-current-force-routes/054` | outside | [docs/registry/implementation-map.yaml:L14293-L14303] |
| `wind-and-current-force-routes/055` | outside | [docs/registry/implementation-map.yaml:L14304-L14312] |
| `wind-and-current-force-routes/056` | outside | [docs/registry/implementation-map.yaml:L14313-L14321] |
| `wind-and-current-force-routes/057` | outside | [docs/registry/implementation-map.yaml:L14322-L14332] |
| `wind-and-current-force-routes/058` | outside | [docs/registry/implementation-map.yaml:L14333-L14345] |
| `wind-and-current-force-routes/059` | outside | [docs/registry/implementation-map.yaml:L14346-L14356] |
| `wind-and-current-force-routes/060` | outside | [docs/registry/implementation-map.yaml:L14357-L14369] |
| `wind-and-current-force-routes/061` | outside | [docs/registry/implementation-map.yaml:L14370-L14380] |
| `wind-and-current-force-routes/062` | outside | [docs/registry/implementation-map.yaml:L14381-L14389] |
| `wind-and-current-force-routes/063` | outside | [docs/registry/implementation-map.yaml:L14390-L14400] |
| `wind-and-current-force-routes/064` | outside | [docs/registry/implementation-map.yaml:L14401-L14411] |
| `wind-and-current-force-routes/065` | outside | [docs/registry/implementation-map.yaml:L14412-L14422] |
| `wind-and-current-force-routes/066` | outside | [docs/registry/implementation-map.yaml:L14423-L14438] |
| `wind-and-current-force-routes/067` | outside | [docs/registry/implementation-map.yaml:L14439-L14447] |
| `wind-and-current-force-routes/068` | outside | [docs/registry/implementation-map.yaml:L14448-L14456] |
| `wind-and-current-force-routes/069` | outside | [docs/registry/implementation-map.yaml:L14457-L14465] |
| `wind-and-current-force-routes/070` | outside | [docs/registry/implementation-map.yaml:L14466-L14474] |
| `wind-and-current-force-routes/071` | outside | [docs/registry/implementation-map.yaml:L14475-L14486] |
| `wind-and-current-force-routes/072` | outside | [docs/registry/implementation-map.yaml:L14487-L14497] |
| `wind-and-current-force-routes/073` | outside | [docs/registry/implementation-map.yaml:L14498-L14508] |
| `wind-and-current-force-routes/074` | outside | [docs/registry/implementation-map.yaml:L14509-L14519] |
| `wind-and-current-force-routes/075` | outside | [docs/registry/implementation-map.yaml:L14520-L14530] |
| `wind-and-current-force-routes/076` | outside | [docs/registry/implementation-map.yaml:L14531-L14539] |
| `wind-and-current-force-routes/077` | outside | [docs/registry/implementation-map.yaml:L14540-L14548] |
| `wind-and-current-force-routes/078` | outside | [docs/registry/implementation-map.yaml:L14549-L14557] |
| `wind-and-current-force-routes/079` | outside | [docs/registry/implementation-map.yaml:L14558-L14566] |
| `wind-and-current-force-routes/080` | outside | [docs/registry/implementation-map.yaml:L14567-L14575] |
| `wind-and-current-force-routes/081` | inside | `src/digitalmodel/orcaflex/installation_analysis.py::SplashZoneInput.calculate_splash_zone_loads`; [docs/registry/implementation-map.yaml:L14576-L14584] |
| `wind-and-current-force-routes/082` | outside | [docs/registry/implementation-map.yaml:L14585-L14593] |
| `wind-and-current-force-routes/083` | outside | [docs/registry/implementation-map.yaml:L14594-L14602] |
| `wind-and-current-force-routes/084` | outside | [docs/registry/implementation-map.yaml:L14603-L14611] |
| `wind-and-current-force-routes/085` | outside | [docs/registry/implementation-map.yaml:L14612-L14620] |
| `wind-and-current-force-routes/086` | outside | [docs/registry/implementation-map.yaml:L14621-L14631] |
| `wind-and-current-force-routes/087` | outside | [docs/registry/implementation-map.yaml:L14632-L14643] |
| `wind-and-current-force-routes/088` | outside | [docs/registry/implementation-map.yaml:L14644-L14653] |
| `wind-and-current-force-routes/089` | outside | [docs/registry/implementation-map.yaml:L14654-L14662] |
| `wind-and-current-force-routes/090` | outside | [docs/registry/implementation-map.yaml:L14663-L14671] |
| `wind-and-current-force-routes/091` | outside | [docs/registry/implementation-map.yaml:L14672-L14680] |
| `wind-and-current-force-routes/092` | outside | [docs/registry/implementation-map.yaml:L14681-L14689] |
| `wind-and-current-force-routes/093` | outside | [docs/registry/implementation-map.yaml:L14690-L14698] |
| `wind-and-current-force-routes/094` | outside | [docs/registry/implementation-map.yaml:L14699-L14707] |
| `wind-and-current-force-routes/095` | outside | [docs/registry/implementation-map.yaml:L14708-L14716] |
| `wind-and-current-force-routes/096` | outside | [docs/registry/implementation-map.yaml:L14717-L14725] |
| `wind-and-current-force-routes/097` | outside | [docs/registry/implementation-map.yaml:L14726-L14734] |
| `wind-and-current-force-routes/098` | outside | [docs/registry/implementation-map.yaml:L14735-L14745] |
| `wind-and-current-force-routes/099` | outside | [docs/registry/implementation-map.yaml:L14746-L14754] |
| `wind-and-current-force-routes/100` | outside | [docs/registry/implementation-map.yaml:L14755-L14763] |
| `wind-and-current-force-routes/101` | outside | [docs/registry/implementation-map.yaml:L14764-L14772] |
| `wind-and-current-force-routes/102` | outside | [docs/registry/implementation-map.yaml:L14773-L14781] |
| `wind-and-current-force-routes/103` | outside | [docs/registry/implementation-map.yaml:L14782-L14790] |
| `wind-and-current-force-routes/104` | outside | [docs/registry/implementation-map.yaml:L14791-L14799] |
| `wind-and-current-force-routes/105` | outside | [docs/registry/implementation-map.yaml:L14800-L14808] |
| `wind-and-current-force-routes/106` | outside | [docs/registry/implementation-map.yaml:L14809-L14817] |
| `wind-and-current-force-routes/107` | outside | [docs/registry/implementation-map.yaml:L14818-L14826] |
| `wind-and-current-force-routes/108` | outside | [docs/registry/implementation-map.yaml:L14827-L14835] |
| `wind-and-current-force-routes/109` | outside | [docs/registry/implementation-map.yaml:L14836-L14844] |
| `wind-and-current-force-routes/110` | outside | [docs/registry/implementation-map.yaml:L14845-L14854] |
| `wind-and-current-force-routes/111` | outside | [docs/registry/implementation-map.yaml:L14855-L14863] |
| `wind-and-current-force-routes/112` | outside | [docs/registry/implementation-map.yaml:L14864-L14873] |
| `wind-and-current-force-routes/113` | outside | [docs/registry/implementation-map.yaml:L14874-L14882] |
| `wind-and-current-force-routes/114` | outside | [docs/registry/implementation-map.yaml:L14883-L14891] |
| `wind-and-current-force-routes/115` | outside | [docs/registry/implementation-map.yaml:L14892-L14900] |
| `wind-and-current-force-routes/116` | outside | [docs/registry/implementation-map.yaml:L14901-L14909] |
| `wind-and-current-force-routes/117` | outside | [docs/registry/implementation-map.yaml:L14910-L14919] |
| `wind-and-current-force-routes/118` | outside | [docs/registry/implementation-map.yaml:L14920-L14929] |
| `wind-and-current-force-routes/119` | outside | [docs/registry/implementation-map.yaml:L14930-L14939] |
| `wind-and-current-force-routes/120` | outside | [docs/registry/implementation-map.yaml:L14940-L14949] |
| `wind-and-current-force-routes/121` | outside | [docs/registry/implementation-map.yaml:L14950-L14959] |
| `wind-and-current-force-routes/122` | outside | [docs/registry/implementation-map.yaml:L14960-L14969] |
| `wind-and-current-force-routes/123` | outside | [docs/registry/implementation-map.yaml:L14970-L14978] |
| `wind-and-current-force-routes/124` | outside | [docs/registry/implementation-map.yaml:L14979-L14987] |
| `wind-and-current-force-routes/125` | outside | [docs/registry/implementation-map.yaml:L14988-L14996] |
| `wind-and-current-force-routes/126` | outside | [docs/registry/implementation-map.yaml:L14997-L15005] |
| `wind-and-current-force-routes/127` | outside | [docs/registry/implementation-map.yaml:L15006-L15014] |
| `wind-and-current-force-routes/128` | outside | [docs/registry/implementation-map.yaml:L15015-L15023] |
| `wind-and-current-force-routes/129` | outside | [docs/registry/implementation-map.yaml:L15024-L15032] |
| `wind-and-current-force-routes/130` | outside | [docs/registry/implementation-map.yaml:L15033-L15041] |
| `wind-and-current-force-routes/131` | outside | [docs/registry/implementation-map.yaml:L15042-L15052] |

</details>

<details>
<summary>api-5ct-5l-grade-tables: all 39 implementation rows</summary>

| Map row | Package boundary | Definition / implementation locator |
| --- | --- | --- |
| `api-5ct-5l-grade-tables/001` | outside | [docs/registry/implementation-map.yaml:L21648-L21656] |
| `api-5ct-5l-grade-tables/002` | outside | [docs/registry/implementation-map.yaml:L21657-L21666] |
| `api-5ct-5l-grade-tables/003` | outside | [docs/registry/implementation-map.yaml:L21667-L21675] |
| `api-5ct-5l-grade-tables/004` | outside | [docs/registry/implementation-map.yaml:L21676-L21685] |
| `api-5ct-5l-grade-tables/005` | outside | [docs/registry/implementation-map.yaml:L21686-L21694] |
| `api-5ct-5l-grade-tables/006` | outside | [docs/registry/implementation-map.yaml:L21695-L21703] |
| `api-5ct-5l-grade-tables/007` | outside | [docs/registry/implementation-map.yaml:L21704-L21712] |
| `api-5ct-5l-grade-tables/008` | outside | [docs/registry/implementation-map.yaml:L21713-L21721] |
| `api-5ct-5l-grade-tables/009` | outside | [docs/registry/implementation-map.yaml:L21722-L21730] |
| `api-5ct-5l-grade-tables/010` | outside | [docs/registry/implementation-map.yaml:L21731-L21739] |
| `api-5ct-5l-grade-tables/011` | outside | [docs/registry/implementation-map.yaml:L21740-L21748] |
| `api-5ct-5l-grade-tables/012` | outside | [docs/registry/implementation-map.yaml:L21749-L21757] |
| `api-5ct-5l-grade-tables/013` | outside | [docs/registry/implementation-map.yaml:L21758-L21768] |
| `api-5ct-5l-grade-tables/014` | outside | [docs/registry/implementation-map.yaml:L21769-L21777] |
| `api-5ct-5l-grade-tables/015` | outside | [docs/registry/implementation-map.yaml:L21778-L21786] |
| `api-5ct-5l-grade-tables/016` | outside | [docs/registry/implementation-map.yaml:L21787-L21796] |
| `api-5ct-5l-grade-tables/017` | inside | `src/digitalmodel/orcaflex/mooring_design.py::(module statements)`; [docs/registry/implementation-map.yaml:L21797-L21806] |
| `api-5ct-5l-grade-tables/018` | inside | `src/digitalmodel/orcaflex/mooring_design.py::MooringLineSegment`; [docs/registry/implementation-map.yaml:L21807-L21815] |
| `api-5ct-5l-grade-tables/019` | inside | `src/digitalmodel/orcaflex/mooring_design.py::MooringLineDesign.estimate_catenary`; [docs/registry/implementation-map.yaml:L21816-L21824] |
| `api-5ct-5l-grade-tables/020` | inside | `src/digitalmodel/orcaflex/mooring_design.py::MooringLineDesign.check_mbl`; [docs/registry/implementation-map.yaml:L21825-L21833] |
| `api-5ct-5l-grade-tables/021` | inside | `src/digitalmodel/orcaflex/mooring_design.py::MooringLineDesign.check_mbl_with_safety_factor`; [docs/registry/implementation-map.yaml:L21834-L21842] |
| `api-5ct-5l-grade-tables/022` | inside | `src/digitalmodel/orcaflex/riser_config.py::(module statements)`; [docs/registry/implementation-map.yaml:L21843-L21852] |
| `api-5ct-5l-grade-tables/023` | inside | `src/digitalmodel/orcaflex/riser_config.py::RiserPipeProperties.steel_mass_per_m`; [docs/registry/implementation-map.yaml:L21853-L21861] |
| `api-5ct-5l-grade-tables/024` | inside | `src/digitalmodel/orcaflex/riser_config.py::RiserPipeProperties.bending_stiffness`; [docs/registry/implementation-map.yaml:L21862-L21870] |
| `api-5ct-5l-grade-tables/025` | inside | `src/digitalmodel/orcaflex/riser_config.py::RiserPipeProperties.axial_stiffness`; [docs/registry/implementation-map.yaml:L21871-L21879] |
| `api-5ct-5l-grade-tables/026` | outside | [docs/registry/implementation-map.yaml:L21880-L21889] |
| `api-5ct-5l-grade-tables/027` | outside | [docs/registry/implementation-map.yaml:L21890-L21898] |
| `api-5ct-5l-grade-tables/028` | outside | [docs/registry/implementation-map.yaml:L21899-L21907] |
| `api-5ct-5l-grade-tables/029` | outside | [docs/registry/implementation-map.yaml:L21908-L21916] |
| `api-5ct-5l-grade-tables/030` | outside | [docs/registry/implementation-map.yaml:L21917-L21925] |
| `api-5ct-5l-grade-tables/031` | outside | [docs/registry/implementation-map.yaml:L21926-L21934] |
| `api-5ct-5l-grade-tables/032` | outside | [docs/registry/implementation-map.yaml:L21935-L21943] |
| `api-5ct-5l-grade-tables/033` | outside | [docs/registry/implementation-map.yaml:L21944-L21953] |
| `api-5ct-5l-grade-tables/034` | outside | [docs/registry/implementation-map.yaml:L21954-L21962] |
| `api-5ct-5l-grade-tables/035` | outside | [docs/registry/implementation-map.yaml:L21963-L21972] |
| `api-5ct-5l-grade-tables/036` | outside | [docs/registry/implementation-map.yaml:L21973-L21982] |
| `api-5ct-5l-grade-tables/037` | outside | [docs/registry/implementation-map.yaml:L21983-L21991] |
| `api-5ct-5l-grade-tables/038` | outside | [docs/registry/implementation-map.yaml:L21992-L22000] |
| `api-5ct-5l-grade-tables/039` | outside | [docs/registry/implementation-map.yaml:L22001-L22009] |

</details>

### Counted-family search and classification

The search used Python `re`, case-sensitive, with the pinned map's patterns exactly as stored. Signals whose pattern matched `orcaflex` case-insensitively were omitted: only the text-writer vocabulary signal matched that package path. The scan covered every directly scoped `.py` file, assigned line hits to the innermost enclosing function/class and grouped module-level statements once per file. The definition classifications below expose all hits; source comments/docstrings and data holders do not add implementations.

#### `api-5ct-5l-grade-tables`

| Definition | Classification / reason | Evidence |
| --- | --- | --- |
| `__init__.py::(module)` | Import, data holder, enum or lookup/delegation for listed methods; helper | [src/digitalmodel/orcaflex/__init__.py:L38-L186] |
| `code_check_engine.py::APIRP2RDInput` | Section 3 code-check capacity method or input data; no independent grade lookup | [src/digitalmodel/orcaflex/code_check_engine.py:L66-L85] |
| `code_check_engine.py::APIRP2RDInput.plastic_moment` | Section 3 code-check capacity method or input data; no independent grade lookup | [src/digitalmodel/orcaflex/code_check_engine.py:L83-L83] |
| `code_check_engine.py::APIRP2RDInput.yield_tension` | Section 3 code-check capacity method or input data; no independent grade lookup | [src/digitalmodel/orcaflex/code_check_engine.py:L90-L90] |
| `code_check_engine.py::check_api_rp_2rd` | Section 3 code-check capacity method or input data; no independent grade lookup | [src/digitalmodel/orcaflex/code_check_engine.py:L113-L134] |
| `code_check_engine.py::DNVOSF201Input` | Section 3 code-check capacity method or input data; no independent grade lookup | [src/digitalmodel/orcaflex/code_check_engine.py:L170-L170] |
| `code_check_engine.py::DNVOSF201Input.characteristic_yield` | Section 3 code-check capacity method or input data; no independent grade lookup | [src/digitalmodel/orcaflex/code_check_engine.py:L189-L189] |
| `model_builder.py::(module)` | Import, data holder, enum or lookup/delegation for listed methods; helper | [src/digitalmodel/orcaflex/model_builder.py:L215-L215] |
| `model_builder.py::build_lazy_wave_model` | Import, data holder, enum or lookup/delegation for listed methods; helper | [src/digitalmodel/orcaflex/model_builder.py:L358-L358] |
| `model_builder.py::build_mooring_line` | Import, data holder, enum or lookup/delegation for listed methods; helper | [src/digitalmodel/orcaflex/model_builder.py:L419-L420] |
| `mooring_design.py::(module)` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/mooring_design.py:L174-L174] |
| `mooring_design.py::MooringLineSegment` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/mooring_design.py:L346-L346] |
| `mooring_design.py::MooringLineDesign.estimate_catenary` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/mooring_design.py:L390-L390] |
| `mooring_design.py::MooringLineDesign.check_mbl` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/mooring_design.py:L416-L416] |
| `mooring_design.py::MooringLineDesign.check_mbl_with_safety_factor` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/mooring_design.py:L551-L551] |
| `pipelay_analysis.py::PipelayPipeProperties` | Import, data holder, enum or lookup/delegation for listed methods; helper | [src/digitalmodel/orcaflex/pipelay_analysis.py:L53-L53] |
| `riser_config.py::PipeGrade` | Import, data holder, enum or lookup/delegation for listed methods; helper | [src/digitalmodel/orcaflex/riser_config.py:L43-L48] |
| `riser_config.py::(module)` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/riser_config.py:L56-L59] |
| `riser_config.py::RiserPipeProperties` | Import, data holder, enum or lookup/delegation for listed methods; helper | [src/digitalmodel/orcaflex/riser_config.py:L75-L75] |
| `riser_config.py::RiserPipeProperties.steel_mass_per_m` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/riser_config.py:L113-L113] |
| `riser_config.py::RiserPipeProperties.bending_stiffness` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/riser_config.py:L142-L142] |
| `riser_config.py::RiserPipeProperties.axial_stiffness` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/riser_config.py:L149-L149] |
| `riser_input_schema.py::MaterialBlock` | Import, data holder, enum or lookup/delegation for listed methods; helper | [src/digitalmodel/orcaflex/riser_input_schema.py:L100-L100] |
| `synthetic_rope_design.py::(module)` | Import, data holder, enum or lookup/delegation for listed methods; helper | [src/digitalmodel/orcaflex/synthetic_rope_design.py:L15-L15] |
| `synthetic_rope_design.py::_synthetic_profile_entry` | Import, data holder, enum or lookup/delegation for listed methods; helper | [src/digitalmodel/orcaflex/synthetic_rope_design.py:L179-L187] |

#### `effective-tension-riser-stack-up`

| Definition | Classification / reason | Evidence |
| --- | --- | --- |
| `code_check_engine.py::check_dnv_os_f201` | Input/result data or pressure-array interface, not an additional effective-tension algorithm | [src/digitalmodel/orcaflex/code_check_engine.py:L224-L238] |
| `mooring_design.py::CatenaryResult` | Input/result data or pressure-array interface, not an additional effective-tension algorithm | [src/digitalmodel/orcaflex/mooring_design.py:L241-L241] |
| `mooring_design.py::solve_catenary` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/mooring_design.py:L329-L329] |
| `pipelay_analysis.py::SLayConfig` | Input/result data or pressure-array interface, not an additional effective-tension algorithm | [src/digitalmodel/orcaflex/pipelay_analysis.py:L108-L108] |
| `pipelay_analysis.py::JLayConfig` | Input/result data or pressure-array interface, not an additional effective-tension algorithm | [src/digitalmodel/orcaflex/pipelay_analysis.py:L220-L220] |
| `pipelay_analysis.py::JLayConfig.calculate_sagbend` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/pipelay_analysis.py:L232-L232] |
| `riser_config.py::SCRDesignInput.calculate_geometry` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/riser_config.py:L223-L223] |
| `riser_config.py::TTRDesignInput` | Input/result data or pressure-array interface, not an additional effective-tension algorithm | [src/digitalmodel/orcaflex/riser_config.py:L336-L336] |
| `riser_config.py::TTRDesignInput.calculate_stroke` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/riser_config.py:L340-L356] |
| `riser_config.py::TTRDesignInput.calculate_top_tension` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/riser_config.py:L377-L394] |
| `riser_input_schema.py::_TopTensionedInput` | Input/result data or pressure-array interface, not an additional effective-tension algorithm | [src/digitalmodel/orcaflex/riser_input_schema.py:L237-L237] |
| `riser_input_schema.py::_TopTensionedInput.to_legacy` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/riser_input_schema.py:L251-L251] |
| `viv_screening.py::BeamProperties` | Input/result data or pressure-array interface, not an additional effective-tension algorithm | [src/digitalmodel/orcaflex/viv_screening.py:L158-L158] |
| `viv_screening.py::BeamProperties.natural_frequency` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/viv_screening.py:L194-L207] |

#### `orcaflex-text-model-writers`

| Definition | Classification / reason | Evidence |
| --- | --- | --- |
| `__init__.py::(module)` | Import/docstring, input loader or export/serialization of a section-3 result; helper | [src/digitalmodel/orcaflex/__init__.py:L33-L168] |
| `environment.py::CurrentProfile.to_orcaflex_dict` | Dictionary-producing method explicitly listed in section 3 | [src/digitalmodel/orcaflex/environment.py:L107-L107] |
| `environment.py::WaveSpectrumParams.to_orcaflex_dict` | Dictionary-producing method explicitly listed in section 3 | [src/digitalmodel/orcaflex/environment.py:L176-L176] |
| `fowt_mooring_workflow.py::(module)` | Import/docstring, input loader or export/serialization of a section-3 result; helper | [src/digitalmodel/orcaflex/fowt_mooring_workflow.py:L4-L5] |
| `fowt_mooring_workflow.py::FOWTMooringWorkflow.router` | Import/docstring, input loader or export/serialization of a section-3 result; helper | [src/digitalmodel/orcaflex/fowt_mooring_workflow.py:L37-L37] |
| `fowt_mooring_workflow.py::FOWTMooringWorkflow._write_summary` | Import/docstring, input loader or export/serialization of a section-3 result; helper | [src/digitalmodel/orcaflex/fowt_mooring_workflow.py:L67-L67] |
| `model_builder.py::ObjectType` | Object type or configuration data holder; helper | [src/digitalmodel/orcaflex/model_builder.py:L28-L28] |
| `model_builder.py::VesselConfig` | Object type or configuration data holder; helper | [src/digitalmodel/orcaflex/model_builder.py:L98-L98] |
| `model_builder.py::VesselConfig.to_orcaflex_dict` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/model_builder.py:L113-L116] |
| `model_builder.py::Buoy6DConfig.to_orcaflex_dict` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/model_builder.py:L147-L150] |
| `model_builder.py::LineConfig` | Object type or configuration data holder; helper | [src/digitalmodel/orcaflex/model_builder.py:L161-L161] |
| `model_builder.py::LineConfig.to_orcaflex_dict` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/model_builder.py:L183-L196] |
| `model_builder.py::build_scr_model` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/model_builder.py:L283-L310] |
| `model_builder.py::build_lazy_wave_model` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/model_builder.py:L335-L365] |
| `model_builder.py::build_mooring_line` | Listed map implementation; retained as inventoried | [src/digitalmodel/orcaflex/model_builder.py:L399-L420] |
| `mooring_design.py::_synthetic_rope_profile_config_path` | Import/docstring, input loader or export/serialization of a section-3 result; helper | [src/digitalmodel/orcaflex/mooring_design.py:L142-L142] |
| `riser_input_schema.py::(module)` | Import/docstring, input loader or export/serialization of a section-3 result; helper | [src/digitalmodel/orcaflex/riser_input_schema.py:L3-L3] |
| `riser_input_schema.py::MaterialBlock.to_pipe_properties` | Import/docstring, input loader or export/serialization of a section-3 result; helper | [src/digitalmodel/orcaflex/riser_input_schema.py:L107-L107] |
| `riser_input_schema.py::EnvironmentBlock` | Import/docstring, input loader or export/serialization of a section-3 result; helper | [src/digitalmodel/orcaflex/riser_input_schema.py:L115-L115] |
| `riser_input_schema.py::load_riser_input` | Import/docstring, input loader or export/serialization of a section-3 result; helper | [src/digitalmodel/orcaflex/riser_input_schema.py:L306-L319] |
| `synthetic_rope_design.py::_profile_config_path` | Import/docstring, input loader or export/serialization of a section-3 result; helper | [src/digitalmodel/orcaflex/synthetic_rope_design.py:L33-L33] |
| `viv_screening_workflow.py::write_viv_outputs` | Import/docstring, input loader or export/serialization of a section-3 result; helper | [src/digitalmodel/orcaflex/viv_screening_workflow.py:L172-L172] |

Further second implementations found by the counted-family search: **0**, summed across the three counted families; the additional environment dictionary producers are already explicit in section 3. This is an inventory/search result, not family quality clearance. [src/digitalmodel/orcaflex/environment.py:L107-L114] [src/digitalmodel/orcaflex/environment.py:L176-L184]

A repository-wide companion search covered 5,552 tracked Python files, including `scripts/`, `docs/`, examples and tests. Its file-hit counts were 336 for catenary, 382 for effective tension, 2,556 for text writers, 485 for wave spectra, 284 for wind/current force routes and 322 for grade tables; these are lexical hits, not implementation counts. The map inventory above is the implementation list; broad lexical matches outside the package are follow-up candidates, not additional counted-package misses.

### Map follow-up and adjacent packages

The package's circular-arc cable constraint, VIV screening, synthetic-rope selection/stiffness/creep/fatigue, code utilisation, weather persistence/operability, extreme-value statistics and parametric case generation are method families covered in section 3 without their own in-package implementation-map family. They require map additions before they can be counted separately; issue pending (9). [src/digitalmodel/orcaflex/mooring_design_fowt.py:L274-L310] [src/digitalmodel/orcaflex/viv_screening.py:L248-L332] [src/digitalmodel/orcaflex/synthetic_rope_design.py:L233-L412] [src/digitalmodel/orcaflex/code_check_engine.py:L103-L156] [src/digitalmodel/orcaflex/weather_window.py:L177-L234] [src/digitalmodel/orcaflex/postprocessor.py:L177-L262] [src/digitalmodel/orcaflex/batch_parametric.py:L78-L111]

The existing families for catenary, waves and wind/current force routes are inventoried but not among the three counted searches. Their further findings affect the map follow-up, not the counted result. Direct current/wind profile producers and hydrostatic/pipe-section calculations also need family-level map treatment under issue pending (9). [src/digitalmodel/orcaflex/environment.py:L74-L114] [src/digitalmodel/orcaflex/environment.py:L203-L223] [src/digitalmodel/orcaflex/riser_config.py:L80-L150]

`subsea/mooring_analysis` and the canonical marine catenary package have distinct interfaces; the inline top-level catenary remains a separate formula, while `solvers/orcaflex` supplies model-generation/execution surfaces outside this pack's scope. The map inventory preserves those distinct entries; #1811 tracks catenary convergence and unit discipline. [src/digitalmodel/orcaflex/mooring_design.py:L251-L335] [src/digitalmodel/subsea/mooring_analysis/catenary.py:L1-L35] [src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:L1-L35]
## 10. Coverage and open items

### Evidence coverage

Source reading covered every directly contained Python file listed below. Subpackage internals are dependency boundaries rather than a claim of full reporting coverage.

| Source read | Pinned source |
| --- | --- |
| `__init__.py` | [src/digitalmodel/orcaflex/__init__.py:L1-L273] |
| `batch_parametric.py` | [src/digitalmodel/orcaflex/batch_parametric.py:L1-L258] |
| `code_check_engine.py` | [src/digitalmodel/orcaflex/code_check_engine.py:L1-L388] |
| `environment.py` | [src/digitalmodel/orcaflex/environment.py:L1-L333] |
| `fowt_mooring_workflow.py` | [src/digitalmodel/orcaflex/fowt_mooring_workflow.py:L1-L68] |
| `installation_analysis.py` | [src/digitalmodel/orcaflex/installation_analysis.py:L1-L394] |
| `model_builder.py` | [src/digitalmodel/orcaflex/model_builder.py:L1-L422] |
| `mooring_design.py` | [src/digitalmodel/orcaflex/mooring_design.py:L1-L717] |
| `mooring_design_fowt.py` | [src/digitalmodel/orcaflex/mooring_design_fowt.py:L1-L331] |
| `mooring_workflow.py` | [src/digitalmodel/orcaflex/mooring_workflow.py:L1-L93] |
| `pipelay_analysis.py` | [src/digitalmodel/orcaflex/pipelay_analysis.py:L1-L324] |
| `postprocessor.py` | [src/digitalmodel/orcaflex/postprocessor.py:L1-L329] |
| `qa.py` | [src/digitalmodel/orcaflex/qa.py:L1-L39] |
| `riser_config.py` | [src/digitalmodel/orcaflex/riser_config.py:L1-L462] |
| `riser_input_schema.py` | [src/digitalmodel/orcaflex/riser_input_schema.py:L1-L345] |
| `synthetic_rope_design.py` | [src/digitalmodel/orcaflex/synthetic_rope_design.py:L1-L412] |
| `viv_screening.py` | [src/digitalmodel/orcaflex/viv_screening.py:L1-L332] |
| `viv_screening_workflow.py` | [src/digitalmodel/orcaflex/viv_screening_workflow.py:L1-L193] |
| `weather_window.py` | [src/digitalmodel/orcaflex/weather_window.py:L1-L311] |

The durable workflow input, registry row, test assertions, routing entry, synthetic-rope data metadata, citation registry, codes-register riser-label discussion and the synthetic-rope article were inspected for the main path. [examples/workflows/fowt-mooring/input.yml:L1-L33] [docs/registry/workflows.yaml:L510-L517] [tests/orcaflex/test_fowt_mooring_workflow.py:L29-L53] [docs/registry/module-routing.yaml:L219-L224] [src/digitalmodel/orcaflex/data/synthetic_rope_profiles.yml:L1-L8] [src/digitalmodel/citations/registry.py:L26-L52] [docs/domains/codes-register.md:L44-L105] [docs/domains/articles/api_rp_2sm_synthetic_mooring.md:L1-L19]

### Test inventory and execution record

The module test discovery found 26 `test_*.py` files and 359 test-function definitions; parametrisation produces 418 node cases across the complete `tests/orcaflex/` inventory. The executed set contains 417 passes and one NOT_RUN node, with 44 of the passes belonging to the separate reporting subpackage tests. These are observed test counts at the code pin, replacing the unverified inventory estimate.

The record below uses `PYTHONPATH=src uv run --no-sources python -m pytest <targets> -q -rA -p no:cacheprovider --basetemp=<scratch>`; the executable was addressed explicitly, all output went to scratch, no `addopts` override was used, and the after runs used the uncommitted documentation tree at the same code pin.

| Command targets | BASE | After | Reason / limit |
| --- | --- | --- | --- |
| `tests/orcaflex/test_calm_data_loader.py` | 2 passed, 0 failed, 0 skipped | 2 passed, 0 failed, 0 skipped | Existing empty-data behavior is only smoke coverage |
| All remaining files below, with the five mock-only QA nodes selected by explicit node ID | 415 passed, 0 failed, 0 skipped | 415 passed, 0 failed, 0 skipped | Existing mock fixtures exercise reporting without a real interface |
| `tests/workflows/test_durable_workflows.py::test_workflow_registry[fowt-mooring]` | 1 passed, 0 failed, 0 skipped | 1 passed, 0 failed, 0 skipped | Synthetic workflow regression |
| `tests/contracts` and `tests/docs/test_digitalmodel_routing_contract.py` | 464 passed, 1 failed, 0 skipped | 464 passed, 1 failed, 0 skipped | Baseline dependency-scanner assertion described below |
| `tests/orcaflex/test_qa.py::TestLoadExamplesQAModule::test_raises_when_script_missing` | NOT_RUN, 1 node | NOT_RUN, 1 node | Calls a loader whose existing script attempts `import OrcFxAPI`; requires an interface import; [tests/orcaflex/test_qa.py:L20-L35] [docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py:L39-L44] |

Baseline: `tests/contracts/test_string_addressed_dependencies.py::test_the_three_known_defects_are_still_derived` fails at its `xlsxwriter` call-site assertion; the same assertion exists unchanged on the default branch. [tests/contracts/test_string_addressed_dependencies.py:L620-L644]

No licensed solver/interface execution, licence query or external engineering benchmark was performed. G1–G4 solver records remain NOT_RUN or conditional as section 5 states.

| Test file inspected | Source / node inventory |
| --- | --- |
| `tests/orcaflex/reporting/test_orcaflex_reporting.py` | [tests/orcaflex/reporting/test_orcaflex_reporting.py:L1-L137] |
| `tests/orcaflex/reporting/test_sections.py` | [tests/orcaflex/reporting/test_sections.py:L1-L380] |
| `tests/orcaflex/test_batch_parametric.py` | [tests/orcaflex/test_batch_parametric.py:L1-L154] |
| `tests/orcaflex/test_calm_data_loader.py` | [tests/orcaflex/test_calm_data_loader.py:L1-L209] |
| `tests/orcaflex/test_code_check_engine.py` | [tests/orcaflex/test_code_check_engine.py:L1-L164] |
| `tests/orcaflex/test_config.py` | [tests/orcaflex/test_config.py:L1-L196] |
| `tests/orcaflex/test_environment.py` | [tests/orcaflex/test_environment.py:L1-L166] |
| `tests/orcaflex/test_extreme_value_post_example.py` | [tests/orcaflex/test_extreme_value_post_example.py:L1-L81] |
| `tests/orcaflex/test_fowt_mooring_workflow.py` | [tests/orcaflex/test_fowt_mooring_workflow.py:L1-L53] |
| `tests/orcaflex/test_generate_report.py` | [tests/orcaflex/test_generate_report.py:L1-L191] |
| `tests/orcaflex/test_generic_scr_slwr.py` | [tests/orcaflex/test_generic_scr_slwr.py:L1-L536] |
| `tests/orcaflex/test_installation_analysis.py` | [tests/orcaflex/test_installation_analysis.py:L1-L208] |
| `tests/orcaflex/test_model_builder.py` | [tests/orcaflex/test_model_builder.py:L1-L141] |
| `tests/orcaflex/test_model_default_tokens.py` | [tests/orcaflex/test_model_default_tokens.py:L1-L453] |
| `tests/orcaflex/test_mooring_design.py` | [tests/orcaflex/test_mooring_design.py:L1-L179] |
| `tests/orcaflex/test_mooring_design_citations.py` | [tests/orcaflex/test_mooring_design_citations.py:L1-L238] |
| `tests/orcaflex/test_mooring_design_fowt.py` | [tests/orcaflex/test_mooring_design_fowt.py:L1-L196] |
| `tests/orcaflex/test_pipelay_analysis.py` | [tests/orcaflex/test_pipelay_analysis.py:L1-L137] |
| `tests/orcaflex/test_postprocessor.py` | [tests/orcaflex/test_postprocessor.py:L1-L152] |
| `tests/orcaflex/test_qa.py` | [tests/orcaflex/test_qa.py:L1-L106] |
| `tests/orcaflex/test_report_builder.py` | [tests/orcaflex/test_report_builder.py:L1-L144] |
| `tests/orcaflex/test_riser_config.py` | [tests/orcaflex/test_riser_config.py:L1-L163] |
| `tests/orcaflex/test_riser_input_schema.py` | [tests/orcaflex/test_riser_input_schema.py:L1-L181] |
| `tests/orcaflex/test_synthetic_rope_design.py` | [tests/orcaflex/test_synthetic_rope_design.py:L1-L292] |
| `tests/orcaflex/test_viv_screening.py` | [tests/orcaflex/test_viv_screening.py:L1-L161] |
| `tests/orcaflex/test_weather_window.py` | [tests/orcaflex/test_weather_window.py:L1-L147] |

Additional import-path matches outside this test directory are discovery leads, not a claim that their unrelated domains were run; the remaining cross-domain coverage is issue pending (2). [tests/contracts/test_force_units_boundary_contract.py:L1-L25] [tests/workflows/test_durable_workflows.py:L1-L17]

### Documentation discovery

A broad documentation search for the program name found 402 Markdown files; a narrower literal `digitalmodel.orcaflex` or `src/digitalmodel/orcaflex/` search found 45. The public documentation locators follow; they are discovery entries, not endorsements of README counts, validation claims or complete historical-document coverage. Two identity-bearing locators are withheld from this public listing and remain in the local discovery record; issue pending (2).

| Documentation found | First matching line |
| --- | --- |
| `docs/README.md` | [docs/README.md:L26-L26] |
| `docs/maps/digitalmodel-operator-map.md` | [docs/maps/digitalmodel-operator-map.md:L55-L55] |
| `docs/data/OCIMF_CORPUS_README.md` | [docs/data/OCIMF_CORPUS_README.md:L108-L108] |
| `docs/migration/legacy-batch-workflows.md` | [docs/migration/legacy-batch-workflows.md:L91-L91] |
| `docs/plans/2026-07-11-issue-1505-synthetic-viv-parametric-hf-pilot.md` | [docs/plans/2026-07-11-issue-1505-synthetic-viv-parametric-hf-pilot.md:L37-L37] |
| `docs/plans/2026-05-20-issue-617-citation-resolver-hardening.md` | [docs/plans/2026-05-20-issue-617-citation-resolver-hardening.md:L17-L17] |
| `docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md` | [docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md:L9-L9] |
| `docs/plans/2026-05-05-issue-582-api-rp-2i-marine-inspection.md` | [docs/plans/2026-05-05-issue-582-api-rp-2i-marine-inspection.md:L9-L9] |
| `docs/plans/2026-06-12-issue-703-orcaflex-detector-routing.md` | [docs/plans/2026-06-12-issue-703-orcaflex-detector-routing.md:L39-L39] |
| `docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md` | [docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md:L41-L41] |
| `docs/plans/2026-06-12-issue-704-tests-orcaflex-baseline.md` | [docs/plans/2026-06-12-issue-704-tests-orcaflex-baseline.md:L26-L26] |
| `docs/plans/2026-06-11-issue-700-ci-baseline-domain-gates.md` | [docs/plans/2026-06-11-issue-700-ci-baseline-domain-gates.md:L14-L14] |
| `docs/plans/2026-05-05-issue-578-w2w-gangway-operability.md` | [docs/plans/2026-05-05-issue-578-w2w-gangway-operability.md:L9-L9] |
| `docs/plans/2026-05-05-issue-584-api-rp-2sm-synthetic-rope-mooring.md` | [docs/plans/2026-05-05-issue-584-api-rp-2sm-synthetic-rope-mooring.md:L9-L9] |
| `docs/plans/2026-05-05-issue-576-fowt-watch-circle-cable-curvature.md` | [docs/plans/2026-05-05-issue-576-fowt-watch-circle-cable-curvature.md:L9-L9] |
| `docs/domains/articles/api_rp_2sm_synthetic_mooring.md` | [docs/domains/articles/api_rp_2sm_synthetic_mooring.md:L3-L3] |
| `docs/plans/evidence/2026-08-10-issue-1983-triage.md` | [docs/plans/evidence/2026-08-10-issue-1983-triage.md:L108-L108] |
| `docs/domains/testing/VALIDATION_FRAMEWORK_BEST_PRACTICES.md` | [docs/domains/testing/VALIDATION_FRAMEWORK_BEST_PRACTICES.md:L88-L88] |
| `docs/domains/testing/engine_test_strategy.md` | [docs/domains/testing/engine_test_strategy.md:L230-L230] |
| `docs/domains/testing/engine_test_strategy_summary.md` | [docs/domains/testing/engine_test_strategy_summary.md:L131-L131] |
| `docs/domains/testing/engine_test_implementation_plan.md` | [docs/domains/testing/engine_test_implementation_plan.md:L209-L209] |
| `docs/domains/infrastructure/parallel_processing_readme.md` | [docs/domains/infrastructure/parallel_processing_readme.md:L44-L44] |
| `docs/domains/codes-register.md` | [docs/domains/codes-register.md:L100-L100] |
| `docs/domains/migration/signal_analysis_migration_plan.md` | [docs/domains/migration/signal_analysis_migration_plan.md:L119-L119] |
| `docs/domains/migration/orcaflex_migration_guide.md` | [docs/domains/migration/orcaflex_migration_guide.md:L24-L24] |
| `docs/domains/orcaflex/ORCAFLEX_AUTO_OPTIMIZATION_GUIDE.md` | [docs/domains/orcaflex/ORCAFLEX_AUTO_OPTIMIZATION_GUIDE.md:L40-L40] |
| `docs/domains/orcaflex/QUICKSTART_MODEL_GENERATOR.md` | [docs/domains/orcaflex/QUICKSTART_MODEL_GENERATOR.md:L45-L45] |
| `docs/domains/orcaflex/CHANGELOG_ORCAFLEX_OPTIMIZATION.md` | [docs/domains/orcaflex/CHANGELOG_ORCAFLEX_OPTIMIZATION.md:L92-L92] |
| `docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md` | [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L197-L197] |
| `docs/domains/orcaflex/dynamic-analysis-guide.md` | [docs/domains/orcaflex/dynamic-analysis-guide.md:L14-L14] |
| `docs/domains/core/PROVENANCE_SYSTEM.md` | [docs/domains/core/PROVENANCE_SYSTEM.md:L529-L529] |
| `docs/domains/orcaflex/universal_runner_guide.md` | [docs/domains/orcaflex/universal_runner_guide.md:L4-L4] |
| `docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md` | [docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md:L46-L46] |
| `docs/domains/orcaflex/QUICKSTART_FILE_CONVERSION.md` | [docs/domains/orcaflex/QUICKSTART_FILE_CONVERSION.md:L18-L18] |
| `docs/domains/orcaflex/DYNAMIC_ANALYSIS_RELEASE_NOTES.md` | [docs/domains/orcaflex/DYNAMIC_ANALYSIS_RELEASE_NOTES.md:L25-L25] |
| `docs/domains/orcaflex/examples/ORCAFLEX_MODELS.md` | [docs/domains/orcaflex/examples/ORCAFLEX_MODELS.md:L326-L326] |
| `docs/domains/orcaflex/examples/gom_scr_design_study/README.md` | [docs/domains/orcaflex/examples/gom_scr_design_study/README.md:L131-L131] |
| `docs/domains/project-docs/MODULE_PORTFOLIO_2026.md` | [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L349-L349] |
| `docs/domains/project-docs/TEST_RESULTS_SUMMARY.md` | [docs/domains/project-docs/TEST_RESULTS_SUMMARY.md:L278-L278] |
| `docs/domains/orcaflex/VALIDATION_QUICK_START.md` | [docs/domains/orcaflex/VALIDATION_QUICK_START.md:L23-L23] |
| `docs/domains/orcaflex/OPERABILITY_ANALYSIS_QUICK_START.md` | [docs/domains/orcaflex/OPERABILITY_ANALYSIS_QUICK_START.md:L196-L196] |
| `docs/domains/orcaflex/DATA_LOADER_USAGE.md` | [docs/domains/orcaflex/DATA_LOADER_USAGE.md:L7-L7] |
| `docs/domains/orcaflex/templates/risers/scr_catenary/README.md` | [docs/domains/orcaflex/templates/risers/scr_catenary/README.md:L54-L54] |

Legacy and generated documentation needs a separate claim-by-claim audit before its maturity, model counts, speedups or validation wording can be adopted. This pack uses executed test counts and explicitly classified examples instead; issue pending (2). [docs/README.md:L31-L54]

### Open items

All entries are assigned to the maintainer for issue assignment and to the relevant family reviewer for engineering disposition; a pending draft is not an opened issue or clearance record.

| Item | Status / required work | Responsible role | Issue |
| --- | --- | --- | --- |
| U1 | UNVERIFIED: complete standard titles, edition identity and clause titles for the references in section 3; clause text not checked against the licensed copy; audit sidecar coverage and preserve RP/STD and OS/ST distinctions | Riser/mooring reviewer with standards access | #1247; issue pending (1) |
| U2 | UNVERIFIED: independently sourced physical riser/mooring and dynamic-cable expected values, dataset revision and tolerances; scalar arithmetic tests are insufficient | Riser/mooring reviewer | #2423 |
| U3 | UNVERIFIED: native dictionary schema, unit conversion, frame/phase conventions, target solver version, load/statics/dynamics attestation and dependency inventory-row identities | Riser/mooring reviewer | issue pending (8) |
| U4 | UNVERIFIED: appointed deputy, producing engineering desk and shared-log reference; fill the responsibility slots before clearance | Maintainer | issue pending (2) |
| Coverage remainder | Detailed historical-doc audit, cross-domain test selection, reporting dependency coverage and prose discrepancies in section 6; do not import historical validation/maturity/performance claims | Maintainer and family reviewer | issue pending (2) |
| Splash-force conversion | Resolve added-mass hook-load closure and add a force-balance test | Installation reviewer | issue pending (3) |
| Pressure omission | Define supported pressure-sensitive criterion and reject unsupported pressure inputs until qualified | Riser/mooring reviewer | issue pending (4) |
| Capacity mismatch | Reconcile the API input property's plastic capacity with the capacity used in the array checker | Riser/mooring reviewer | issue pending (5) |
| Inadmissible geometry | Reject impossible suspended-line/cable geometries before returning a pass | Riser/mooring reviewer | issue pending (6) |
| Silent fallbacks | Expose failed layout/directory creation, missing response columns and unsupported weather criteria | Maintainer and family reviewer | issue pending (7) |
| FOWT citation | Resolve missing-page, domain and overlay handling; require complete citation evidence | Riser/mooring reviewer | #2018 |
| Catenary overlap | Retain unit/assumption distinctions among the inline, subsea and marine catenary implementations; no independent-hand-check endorsement | Riser/mooring reviewer | #1811 |
| Map follow-up | Add the uncovered section-3 method families and review repository-wide lexical candidates outside the package before classifying map gaps | Maintainer and family reviewer | issue pending (9) |

Pending draft numbers refer to the separate issue-draft document; the maintainer opens the drafts and replaces these references with issue numbers before clearance.
