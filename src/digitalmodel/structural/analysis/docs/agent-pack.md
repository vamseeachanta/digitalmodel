# Structural analysis module knowledge pack

| Header | Value |
| --- | --- |
| Module | structural/analysis |
| Repository | digitalmodel |
| Path | `src/digitalmodel/structural/analysis` |
| Pack revision | 0, 2026-10-15 |
| Code revision | `1cacb74d17952505f453d7eda54f09dce1acc045` |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| QM role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The public API exposes pipe wall-thickness checks, code comparisons, parameter sweeps, operational phases, lookup tables and interactive reports. [src/digitalmodel/structural/analysis/__init__.py:L4-L80]

`WallThicknessAnalyzer.perform_analysis` dispatches to a registered strategy and returns the largest check utilisation, its governing check and `is_safe = (max_utilisation <= 1.0)`. [src/digitalmodel/structural/analysis/wall_thickness.py:L637-L678]

The package boundary includes `wall_thickness_codes`; member capacity, plate buckling and column buckling are exported by the separate `structural/structural_analysis` package. [src/digitalmodel/structural/structural_analysis/__init__.py:L30-L59]

The routing registry describes the structural domain as stable; that domain label does not establish independent validation of this package. [docs/registry/module-routing.yaml:L301-L310]

DNV-ST-F201 currently exposes pressure containment only, while the other strategies enumerate their own pressure and combined-load checks. [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L97-L141] [src/digitalmodel/structural/analysis/wall_thickness_codes/__init__.py:L4-L13]

## 2. Inputs and outputs

Input contract for the calculation API:

| Name | Unit | Allowed range enforced by code | Default | Upstream source and evidence |
| --- | --- | --- | --- | --- |
| `PipeGeometry.outer_diameter`, `wall_thickness` | m | Both positive; wall thickness below half the diameter | Required | Caller; [src/digitalmodel/structural/analysis/wall_thickness.py:L90-L109] |
| `corrosion_allowance` | m | Nonnegative; no check that allowance is below wall thickness | 0 m | Caller; [src/digitalmodel/structural/analysis/wall_thickness.py:L94-L121] |
| `fabrication_tolerance` | 1 | [0, 1) | Code default; omitted here to avoid reproducing a standards-derived allowance | Caller; [src/digitalmodel/structural/analysis/wall_thickness.py:L94-L116] |
| `PipeMaterial.grade`, `smys`, `smts` | label, Pa, Pa | Positive strengths; SMTS at least SMYS | Required | Caller; [src/digitalmodel/structural/analysis/wall_thickness.py:L132-L151] |
| `youngs_modulus`, `poissons_ratio`, `fabrication_type` | Pa, 1, enum | Positive modulus; Poisson ratio has no explicit bounds check | Code defaults and seamless fabrication | Caller; [src/digitalmodel/structural/analysis/wall_thickness.py:L139-L160] |
| `DesignLoads.internal_pressure`, `external_pressure` | Pa | No explicit range validation | 0 Pa | Caller; [src/digitalmodel/structural/analysis/wall_thickness.py:L163-L174] |
| `bending_moment`, `effective_tension` | N m, N | No explicit range validation | 0 N m, 0 N | Caller; [src/digitalmodel/structural/analysis/wall_thickness.py:L163-L174] |
| `DesignFactors.safety_class` | enum | low, medium, high | medium | Caller; [src/digitalmodel/structural/analysis/wall_thickness.py:L74-L77] [src/digitalmodel/structural/analysis/wall_thickness.py:L177-L200] |
| `code`, `edition` | enum, year | Registered code; edition validation only in supporting strategies | DNV-ST-F101, unspecified | Caller; [src/digitalmodel/structural/analysis/wall_thickness.py:L225-L239] [src/digitalmodel/structural/analysis/wall_thickness.py:L637-L655] |
| `SweepConfig` parameter arrays | m, Pa, N m, N | Product of arrays; invalid geometries and unknown grades omitted | See constructor; grade values supplied by legacy adapter | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L36-L57] [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L66-L147] |
| `create_standard_phases` depth and pressure | m, Pa | Negative depth rejected | Depth and design pressure required | Caller; [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L56-L73] [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L156-L220] |

Output contract:

| Name or format | Unit | Meaning and range | Destination and evidence |
| --- | --- | --- | --- |
| `WallThicknessResult.checks`, `max_utilisation`, `is_safe`, `governing_check` | 1, 1, bool, label | Per-check demand/resistance; largest value controls unity comparison | API caller; [src/digitalmodel/structural/analysis/wall_thickness.py:L203-L212] [src/digitalmodel/structural/analysis/wall_thickness.py:L656-L671] |
| `details` | Key dependent: m, Pa, N, N m, 1 | Per-check intermediate quantities; not one common numeric unit | API caller; [src/digitalmodel/structural/analysis/wall_thickness.py:L284-L294] [src/digitalmodel/structural/analysis/wall_thickness.py:L487-L495] |
| Sweep DataFrame | Explicit m, mm, MPa, kN m, kN columns; utilisation in 1 | One retained row per valid combination | Caller; [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L122-L147] |
| Phase DataFrames | 1 for utilisation | Long check rows or one summary row per phase/code | Caller; [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L239-L275] |
| Lookup CSV | kN m, kN, 1, bool | Grid plus per-code maximum utilisation and allowable flag | `MTLookupGenerator.to_csv`; [src/digitalmodel/structural/analysis/wall_thickness_lookup.py:L92-L146] |
| Comparison HTML, Markdown, JSON | mm, MPa for displayed geometry/pressure; utilisation in 1 | Cross-code matrix, summary and envelope verdict | Configured output directory; [src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py:L74-L109] [src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py:L143-L185] |
| Interactive HTML and Excel | Labels state display units; tabular input columns retained in Excel | Plotly report or workbook | Configured destination; [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L490-L575] [src/digitalmodel/structural/analysis/wall_thickness_interactive_report.py:L212-L336] |

The core checks use signed internal minus external pressure and magnitudes of bending moment and tension in the DNV combined check; the Von Mises plots evaluate both pipe-wall fibres. These scalar cross-section calculations define no spatial coordinate origin or wave phase. [src/digitalmodel/structural/analysis/wall_thickness.py:L277-L282] [src/digitalmodel/structural/analysis/wall_thickness.py:L458-L481] [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L271-L300]

`t1` subtracts corrosion then applies fabrication reduction; `t2` subtracts corrosion alone. The convenience `net_internal_pressure` property clamps at zero, whereas the check implementations subtract the pressures directly. [src/digitalmodel/structural/analysis/wall_thickness.py:L111-L121] [src/digitalmodel/structural/analysis/wall_thickness.py:L172-L174] [src/digitalmodel/structural/analysis/wall_thickness.py:L276-L282]

## 3. Method and governing standards

The entries below describe the implemented methods and the references named by code, rather than establishing compliance; clause text not checked against the licensed copy. [src/digitalmodel/structural/analysis/wall_thickness.py:L5-L17]

| Standard and code label | Named clause or method | Implementing definitions and evidence |
| --- | --- | --- |
| DNV-ST-F101, Submarine Pipeline Systems; code labels 2007 and 2021 | Pressure containment, local buckling/collapse, propagation buckling, combined loading | `DnvStF101Strategy._check_pressure_containment`, `_check_collapse`, `_solve_collapse_pressure`, `_check_propagation_buckling`, `_check_combined_loading`; [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L23-L105] [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L117-L315]; clause names in [src/digitalmodel/structural/analysis/wall_thickness.py:L251-L268] [src/digitalmodel/structural/analysis/wall_thickness.py:L302-L327] [src/digitalmodel/structural/analysis/wall_thickness.py:L396-L408] [src/digitalmodel/structural/analysis/wall_thickness.py:L435-L475] |
| API RP 1111, Design, Construction, Operation and Maintenance of Offshore Hydrocarbon Pipelines (Limit State Design); code labels third edition 1999 and fourth edition 2015 | Burst pressure, collapse pressure, propagation buckling | `ApiRp1111Strategy._check_burst`, `_check_collapse`, `_check_propagation`; [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L21-L68] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L100-L199]; clause names in [src/digitalmodel/structural/analysis/wall_thickness.py:L503-L508] [src/digitalmodel/structural/analysis/wall_thickness.py:L549-L554] [src/digitalmodel/structural/analysis/wall_thickness.py:L597-L602] |
| API RP 2RD, code title Dynamic Risers for FPS; result label 2013 is unresolved under #2166 | Legacy WSD burst, collapse and hoop stress; no exact clause supplied in strategy | `ApiRp2rdStrategy._check_burst`, `_check_collapse`, `_check_hoop`; [src/digitalmodel/codes.py:L74-L75] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L23-L48] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L69-L177] |
| API STD 2RD, code title Dynamic Risers for FPS; result label 2013 | Burst, collapse and Method 1 combined moment/tension/pressure interaction | `ApiStd2rdStrategy._check_burst`, `_check_collapse`, `_check_combined`; [src/digitalmodel/codes.py:L74-L75] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L83-L226] |
| ASME B31.4, Pipeline Transportation Systems for Liquids and Slurries; code label 2019 | Internal pressure containment, external pressure collapse and propagation buckling | `AsmeB314Strategy._check_burst`, `_check_collapse`, `_check_propagation`; [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L23-L53] [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L70-L200]; local manifest names clauses in [src/digitalmodel/structural/analysis/wall_thickness_codes/manifest.yaml:L1-L30] |
| ASME B31.8, Gas Transmission Piping; result label 2020 | Barlow burst; design, joint and temperature factors; API-like collapse and propagation | `AsmeB318Strategy._check_burst`, `_check_collapse`, `_check_propagation`; [src/digitalmodel/codes.py:L81-L83] [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py:L60-L187] |
| ISO 13623, Pipeline transportation systems; result label 2017 | Pressure containment and safety factor; cubic collapse; propagation | `Iso13623Strategy._check_pressure_containment`, `_check_collapse`, `_solve_collapse_pressure`, `_check_propagation`; [src/digitalmodel/codes.py:L80-L80] [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L57-L211] |
| PD 8010-2, Pipeline systems - Subsea pipelines; result label 2015 | Hoop stress, elastic-plastic collapse, propagation | `Pd80102Strategy._check_hoop_stress`, `_check_collapse`, `_solve_collapse_pressure`, `_check_propagation`; [src/digitalmodel/codes.py:L78-L79] [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L59-L209] |
| DNV-ST-F201, Dynamic Risers; code label 2021 unresolved under #2166 | Bursting/pressure containment | `DnvStF201Strategy._check_pressure_containment`; [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L23-L65] [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L155-L205] |

Additional calculation definitions and independent paths:

| Method | Definitions and implementation boundary |
| --- | --- |
| Legacy direct DNV/API checks | `WallThicknessAnalyzer.check_pressure_containment`, `check_collapse`, `_solve_collapse_pressure`, `check_propagation_buckling`, `check_combined_loading`, `check_api_burst`, `check_api_collapse`, `check_api_propagation` retain their own arithmetic; `perform_analysis` instead uses registered strategies. [src/digitalmodel/structural/analysis/wall_thickness.py:L251-L631] [src/digitalmodel/structural/analysis/wall_thickness.py:L637-L678] |
| Plastic section capacities | Each strategy defines `compute_plastic_moment` and `compute_plastic_tension`; API STD 2RD also defines `compute_yield_moment`; DNV uses corroded thickness while several others use nominal thickness. [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L107-L115] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L87-L98] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L50-L67] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L53-L81] [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L55-L68] [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py:L47-L58] [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L47-L55] [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L46-L57] [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L143-L153] |
| Hydrostatics and phase construction | `compute_hydrostatic_pressure` returns density times gravity times depth; `create_standard_phases` constructs empty installation, water-filled installation, hydrotest, operation and shutdown loads. [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L56-L73] [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L156-L220] |
| Vectorised equivalent stress | `_von_mises_utilisation_grid` and `plot_tension_moment_interaction` compute both fibres and the larger utilisation; the plotted unity contour is an equivalent-stress criterion, separate from strategy pressure checks. [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L271-L373] |
| Phase combined-load surface | `_dnv_combined_utilisation_grid` computes DNV capacities and interaction terms, calling the direct analyzer collapse routine; `plot_phase_tm_interaction` has a separate API equivalent-stress route and dispatches every other code to that DNV surface. [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L360-L422] [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L498-L575] |
| Scalar report equivalent stress and limits | `_von_mises_util_single`, `_find_limit_bisection`, `_compute_capacity_limits`, `_trace_envelope` compute the equivalent-stress response and allowable limits; the stopping intervals are documented in N and N m. [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L280-L442] |
| Report Method 1 and Method 2 envelopes | `_trace_envelope_method1` computes a linear interaction; `_trace_envelope_method2` computes a cosine interaction; both correct for pressure. These are separate arithmetic implementations from the API STD 2RD strategy. [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L445-L509] |
| Report contour and pressure sensitivity | `generate_mt_report` computes section properties and hoop stress; `_build_contour_chart` recomputes equivalent stress on a grid; `_compute_pressure_sensitivity` recomputes hoop stress and calls the envelope routines. [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L130-L164] [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L512-L556] [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L654-L699] |
| Grade data | `API_5L_GRADES` calls `legacy_pipe_db_grades_pa`, retaining a legacy material view rather than defining another canonical grade matrix. [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L21-L40] [docs/domains/grade-table-migration-2026-06-28.md:L40-L57] |

The DNV edition tables include edition-specific safety-class entries, but the checks read `DesignFactors.gamma_sc` instead; the result also stamps the common code reference rather than the selected edition. This is recorded as issue pending (1). [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L23-L55] [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L132-L136] [src/digitalmodel/structural/analysis/wall_thickness.py:L184-L200] [src/digitalmodel/structural/analysis/wall_thickness.py:L663-L671]

UNVERIFIED: exact-edition compliance of the standards-derived equations and factors, including the API RP 2RD and DNV-ST-F201 label discrepancies; #2166 and issue pending (3). [docs/domains/codes-register.md:L62-L105]

UNVERIFIED: the core results provide a `code_reference` string but no per-equation `Citation` sidecar; completeness against the calculation-output citation contract remains open, issue pending (3). [src/digitalmodel/structural/analysis/wall_thickness.py:L203-L212] [src/digitalmodel/structural/analysis/wall_thickness.py:L663-L671]

## 4. Validated worked examples

Offline result record: 2026-10-15, Python 3.12.3, pytest 8.4.2, code pin `1cacb74d17952505f453d7eda54f09dce1acc045`; test selection and complete counts are in section 10. All four cases passed at BASE and on the uncommitted documentation tree.

| ID and classification | Independent source or regression basis | Input and expected value | Tolerance and basis | Existing test node ID | BASE / after |
| --- | --- | --- | --- | --- | --- |
| V1 validated, closed form | Hydrostatic pressure from fluid weight divided by area; constant-density closed form | Inline inputs: depth 100 m, density 1000 kg/m³, gravity 10 m/s²; expected pressure 1e6 Pa | Relative 1e-9 (1), equivalent to 1e-3 Pa at this pressure; numerical assertion tolerance | `tests/test_wall_thickness_phases.py::TestComputeHydrostaticPressure::test_custom_density_and_gravity` | PASS / PASS; [tests/test_wall_thickness_phases.py:L75-L77] |
| R1 regression, worked example | Inline DNV-ST-F201 formula repeated in the test; no independent annex or benchmark identified | `make_10inch`, `make_x65`, inline loads; expected `exp_pb` and `exp_pbd` in Pa from the test expressions | Relative 1e-9 (1) for both pressure outputs, from assertions; standards factors omitted here | `tests/test_wall_thickness_codes/test_dnv_st_f201.py::TestF201WorkedExample::test_burst_resistance_hand_calc` | PASS / PASS; [tests/test_wall_thickness_codes/test_dnv_st_f201.py:L78-L96] |
| R2 regression, cached workflow | Committed quick-check cache and module-produced sweep | Input: `examples/structural/wall_thickness_quickcheck/data/quickcheck_cache.json`; selected wall and governing collapse label; expected wall remains in the cited assertion rather than copying a tabulated dimension | Absolute 1e-2 mm for selected wall; asserted regression tolerance | `tests/test_wall_thickness_quickcheck_example.py::test_headline_rule_selects_smallest_passing_standard_wall` | PASS / PASS; [tests/test_wall_thickness_quickcheck_example.py:L57-L83] [tests/test_wall_thickness_quickcheck_example.py:L23-L26] |
| R3 regression, comparison workflow | Module-produced comparison result and serialized files | Inline `_cfg`; 3 code result rows and HTML/Markdown/JSON artifacts; saved governing code equals returned code | 0 missing files (count), 0 differing governing labels (count) | `tests/test_wall_thickness_compare_workflow.py::test_run_compare_emits_artifacts` | PASS / PASS; [tests/test_wall_thickness_compare_workflow.py:L22-L60] |

The pressure worked example repeats the implemented expression and factors, and the quick-check test checks cached module output; neither is treated as independent standards validation. [tests/test_wall_thickness_codes/test_dnv_st_f201.py:L78-L96] [tests/test_wall_thickness_quickcheck_example.py:L57-L83] [tests/test_wall_thickness_quickcheck_example.py:L23-L26]

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations, with G5 from section 9; these are proposed module checks, with detected existing defects retained as findings rather than approvals.

| Gate | Applicability | Route |
| --- | --- | --- |
| G0 | Applicable | Core constructors, SI-unit boundary and sweep cardinality checks |
| G1 | Applicable | Python import and offline pytest route for registered strategies; this package writes calculation reports rather than solver input decks |
| G2 | Applicable | Hydrostatic closed form, collapse residual/physical-root checks and selected-edition sensitivity |
| G3 | NOT_APPLICABLE | No time integration or short dynamics in the registered wall-thickness check protocol; [src/digitalmodel/structural/analysis/wall_thickness_codes/base.py:L22-L45] |
| G4 | NOT_APPLICABLE | No diffraction geometry or hydrodynamic solver in that protocol; [src/digitalmodel/structural/analysis/wall_thickness_codes/base.py:L22-L45] |
| G5 | Applicable | Traceable case results, artifact completeness and pin/citation record |

| Check | Function, output and executable route | Numeric pass criterion and unit | Evidence and observed result | Gate |
| --- | --- | --- | --- | --- |
| C01 | `PipeGeometry` construction; run `TestPipeGeometry` in `tests/test_wall_thickness.py` | 0 accepted invalid cases (count) for the inputs asserted by that class | pytest results; PASS at BASE; [tests/test_wall_thickness.py:L51-L97] | G0 |
| C02 | `compute_hydrostatic_pressure`; execute V1 | Absolute pressure error ≤ 1e-3 Pa | V1; PASS, error 0 Pa; [tests/test_wall_thickness_phases.py:L75-L77] | G0 |
| C03 | `ParametricSweep.run`; execute `TestParametricSweep::test_sweep_correct_row_count` in `tests/test_wall_thickness_parametric.py` | 0 rows difference from the product of the valid input array lengths (count) | pytest results; PASS at BASE; [tests/test_wall_thickness_parametric.py:L58-L71] | G0 |
| C11 | Import public package and run the 32-file module/consumer selection in section 10 | 0 failed and 0 skipped tests (count); 548 passed tests (count) | Offline pytest log; PASS at BASE; public exports in [src/digitalmodel/structural/analysis/__init__.py:L4-L80] | G1 |
| C21 | `compute_hydrostatic_pressure`; execute V1 using fluid weight/area as the independent reference | Absolute pressure error ≤ 1e-3 Pa | V1; PASS at BASE; [tests/test_wall_thickness_phases.py:L75-L77] | G2 |
| C22 | `DnvStF101Strategy._solve_collapse_pressure`; run `test_root_satisfies_eq_5_9` and `test_deepwater_root_below_elastic_and_plastic_limits` in `tests/test_wall_thickness_collapse_solver.py` | Normalized cubic residual < 1e-3 (1); 0 roots outside the physical interval (count) | pytest results; PASS at BASE; [tests/test_wall_thickness_collapse_solver.py:L52-L56] [tests/test_wall_thickness_collapse_solver.py:L78-L86] | G2 |
| C23 | `Iso13623Strategy._check_collapse` and `Pd80102Strategy._check_collapse`; use `PipeGeometry(0.3239, 0.018)`, `make_x65()` from `tests/test_wall_thickness_collapse_solver.py`, `DesignLoads(20e6, 10e6)`; require each returned `p_c` below both returned limiting pressures | 0 roots outside `0 < p_c < min(p_el, p_p)` (count); pressures in Pa | Material fixture: [tests/test_wall_thickness_collapse_solver.py:L37-L49]; direct offline probe: FAIL for both, existing #1920; no detection check exists in the shipped suite; [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L103-L176] [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L101-L175] | G2 |
| C24 | Run DNV-ST-F101 strategy with 2007 and 2021 editions, shared medium-class `DesignFactors`, and C23 inputs; compare pressure containment utilisation | Absolute difference > 0 (1), because the selected edition tables differ | Direct offline probe: FAIL, outputs identical; issue pending (1); [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L23-L55] [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L132-L136] | G2 |
| C51 | `run_wall_thickness_compare`; execute R3 and resolve every repository citation against the named source pin | 0 missing artifacts (count); 0 unresolved citations (count) | R3 PASS at BASE; citation resolver record; [tests/test_wall_thickness_compare_workflow.py:L45-L60] | G5 |

The applicable G1 load route is ordinary Python import followed by the offline test selection; report generation writes HTML, Markdown and JSON. [src/digitalmodel/structural/analysis/__init__.py:L4-L80] [src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py:L96-L109]

## 6. Known failure modes and pitfalls

| Symptom | Cause and detection | Incident or tracking | Evidence |
| --- | --- | --- | --- |
| Sweep rows disappear | Invalid geometry, unknown grade and caught `ValueError`/`ZeroDivisionError` combinations are omitted; C03 checks cardinality only for valid fixtures, so compare requested and retained counts before interpreting a sweep | Documented pitfall, 2026-10-15 | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L87-L94] [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L139-L147] |
| Edition request silently ignored for a strategy | `perform_analysis` catches constructor `TypeError` and retries without the edition argument; no detection check exists for arbitrary constructor failures | issue pending (1), 2026-10-15 | [src/digitalmodel/structural/analysis/wall_thickness.py:L645-L653] |
| DNV old/new edition outputs coincide | Strategy reads shared `DesignFactors`, leaving edition-specific safety-class entries unused; C24 detects the unchanged output | issue pending (1), 2026-10-15 | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L23-L55] [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L132-L136] |
| ISO/PD collapse resistance can exceed elastic and plastic limits | Broad cubic bracket retains nonphysical roots; C23 detects it; these methods are excluded as hand-check references | #1920, confirmed 2026-10-15 | [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L154-L176] [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L153-L175] |
| Direct DNV collapse previously admitted a high root | Current direct and strategy solvers bracket below both limiting pressures; C22 checks the correction | Historical regression, 2026-10-15 | [src/digitalmodel/structural/analysis/wall_thickness.py:L377-L394] [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L217-L235] [tests/test_wall_thickness_collapse_solver.py:L52-L104] |
| Code/edition label mistaken for qualification | Generic result label is independent of selected edition; RP/STD and ST-F201 edition uncertainty is retained in the register; no detection check exists for licensed-edition conformity | #2166 and issue pending (3), 2026-10-15 | [src/digitalmodel/structural/analysis/wall_thickness.py:L663-L671] [docs/domains/codes-register.md:L62-L105] |
| Phase chart uses a different code's criterion | Every non-API-RP-1111 code enters the DNV grid branch; no detection check exists for parity across all registered codes | issue pending (2), 2026-10-15 | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L548-L575] |
| Report and strategy criteria differ | M-T report recomputes equivalent stress and condition factors independently; the report envelope table uses the first condition's tension grid against all conditions' indexed moment values; no detection check exists for a common tension coordinate | issue pending (2), 2026-10-15 | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L280-L297] [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L445-L509] [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L1117-L1154] |
| F201 coverage overstated from an issue or a worked-example label | #878 describes missing implementation, while the registered strategy now returns pressure containment only; use check names to assess scope rather than the issue's status | #878 and #2166, confirmed 2026-10-15 | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L97-L141] [tests/test_wall_thickness_codes/test_dnv_st_f201.py:L36-L58] |
| Material snapshot mistaken for current canonical grade data | The Pa adapter deliberately preserves legacy values pending convergence; no detection check exists here for a newly selected product specification | #1147, 2026-10-15 | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L36-L40] [docs/domains/grade-table-migration-2026-06-28.md:L40-L57] |

## 7. Lessons log

| Date | Record ID | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-10-15 | analysis-pack-r0 | Keep the worked formula example separate from independent validation, and exclude the known ISO/PD high-root implementations as reference answers | [tests/test_wall_thickness_codes/test_dnv_st_f201.py:L78-L96] [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L154-L176] [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L153-L175] | Sections 4 to 6 | Pending; issue pending (3) |

## 8. Ownership

Proposed responsibility assignment for this draft: engineering calculations quality manager; deputy engineering calculations quality manager; producing engineering calculations maintainers. Ambiguous standards, tolerances and method classifications go to the family quality manager; unresolved cross-family scope goes to the maintainers. This is a proposed assignment, not a recorded clearance.

## 9. Dependencies and implementation inventory

`CODE_REGISTRY` supplies code dispatch; the material adapter supplies Pa-valued grade data; pandas/NumPy/Plotly supply sweep, grid and report infrastructure. [src/digitalmodel/structural/analysis/wall_thickness_codes/base.py:L48-L57] [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L17-L40]

The comparison route consumes the package's own result objects and emits a configuration result plus reports; the tests also exercise riser screening, reporting and workflow consumers. [src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py:L41-L109] [tests/reporting/test_riser_pipe_comparison.py:L43-L61] [tests/test_riser_case_screening.py:L1-L29] [tests/workflow_api/test_run_workflow_wall_thickness.py:L1-L45]

Implementation-map provenance (a separate pin from the code pin): `docs/registry/implementation-map.yaml` on `codex/implmap-work`, commit `b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407`, declared data revision `947fa50ff9a9e210de7e8f90e01b37add85c5954`. Map citations below resolve at that map commit; every other repository citation resolves at the code pin.

| Counted family (rank order) | In-package rows | All rows inventoried | Source |
| --- | --- | --- | --- |

| `effective-tension-riser-stack-up` | 9 | 264 | [docs/registry/implementation-map.yaml:L1667-L1674] |

| `api-5ct-5l-grade-tables` | 8 | 39 | [docs/registry/implementation-map.yaml:L21641-L21648] |

Both families are counted because these are the only families with implementation paths inside the package; no subpackage is excluded. The inventories below retain the map order within each family. No search signal matches the package-relative path `structural/analysis` case-insensitively, so no signal is excluded.

### 9.1. effective-tension-riser-stack-up

Exact case-sensitive Python `re` search signals:

```text
algorithm: effective_tension|effectiveTension|top_tension|tensioner|stack_up|internal_pressure.*(?:area|A_i)|external_pressure.*(?:area|A_o)
vocabulary: effective.tension|tensioner|stack.up
constants_or_keys: internal_pressure|external_pressure|fluid_density|buoyancy_module
```

[docs/registry/implementation-map.yaml:L1670-L1672]

| Map row | Path | Symbol (`<module>` means module-level) | Relation as recorded | Source |
| --- | --- | --- | --- | --- |

| 1 | `docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L1674-L1682] |

| 2 | `docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L1683-L1691] |

| 3 | `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L1692-L1700] |

| 4 | `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L1701-L1709] |

| 5 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r01_catenary` | independent | [docs/registry/implementation-map.yaml:L1710-L1718] |

| 6 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r02_lazy_wave` | independent | [docs/registry/implementation-map.yaml:L1719-L1727] |

| 7 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r05_lay_table` | independent | [docs/registry/implementation-map.yaml:L1728-L1736] |

| 8 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r06_multi_static` | independent | [docs/registry/implementation-map.yaml:L1737-L1745] |

| 9 | `Discovery file with identifying name (locator withheld)` | `Definition at this map row` | example | [docs/registry/implementation-map.yaml:L1746-L1754] |

| 10 | `Discovery file with identifying name (locator withheld)` | `Definition at this map row` | example | [docs/registry/implementation-map.yaml:L1755-L1765] |

| 11 | `Discovery file with identifying name (locator withheld)` | `Definition at this map row` | example | [docs/registry/implementation-map.yaml:L1766-L1776] |

| 12 | `Discovery file with identifying name (locator withheld)` | `Definition at this map row` | example | [docs/registry/implementation-map.yaml:L1777-L1785] |

| 13 | `Discovery file with identifying name (locator withheld)` | `Definition at this map row` | example | [docs/registry/implementation-map.yaml:L1786-L1797] |

| 14 | `Discovery file with identifying name (locator withheld)` | `Definition at this map row` | example | [docs/registry/implementation-map.yaml:L1798-L1806] |

| 15 | `Discovery file with identifying name (locator withheld)` | `Definition at this map row` | example | [docs/registry/implementation-map.yaml:L1807-L1815] |

| 16 | `Discovery file with identifying name (locator withheld)` | `Definition at this map row` | example | [docs/registry/implementation-map.yaml:L1816-L1824] |

| 17 | `Discovery file with identifying name (locator withheld)` | `Definition at this map row` | example | [docs/registry/implementation-map.yaml:L1825-L1836] |

| 18 | `examples/domains/workflows/04_pipeline_installation.py` | `analyze_installation_catenary` | example | [docs/registry/implementation-map.yaml:L1837-L1845] |

| 19 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_schedule_row` | example | [docs/registry/implementation-map.yaml:L1846-L1854] |

| 20 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_assemble_row` | example | [docs/registry/implementation-map.yaml:L1855-L1865] |

| 21 | `scripts/capabilities/build_wall_thickness_3d.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L1866-L1874] |

| 22 | `scripts/capabilities/build_wall_thickness_3d.py` | `_analyze_point` | independent | [docs/registry/implementation-map.yaml:L1875-L1883] |

| 23 | `scripts/capabilities/build_wall_thickness_3d.py` | `build_study` | wrapper | [docs/registry/implementation-map.yaml:L1884-L1894] |

| 24 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_axes` | independent | [docs/registry/implementation-map.yaml:L1895-L1903] |

| 25 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_pack_rows` | independent | [docs/registry/implementation-map.yaml:L1904-L1912] |

| 26 | `scripts/capabilities/build_wall_thickness_explorer.py` | `_run` | independent | [docs/registry/implementation-map.yaml:L1913-L1921] |

| 27 | `scripts/orcaflex/check_generic_scr_slwr.py` | `metrics` | independent | [docs/registry/implementation-map.yaml:L1922-L1930] |

| 28 | `scripts/orcaflex/check_generic_scr_slwr.py` | `calculate` | independent | [docs/registry/implementation-map.yaml:L1931-L1939] |

| 29 | `scripts/orcaflex/prepare_slay_campaign.py` | `catenary` | independent | [docs/registry/implementation-map.yaml:L1940-L1948] |

| 30 | `scripts/orcaflex/prepare_slay_campaign.py` | `case_spec` | independent | [docs/registry/implementation-map.yaml:L1949-L1957] |

| 31 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare_case` | independent | [docs/registry/implementation-map.yaml:L1958-L1969] |

| 32 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare` | independent | [docs/registry/implementation-map.yaml:L1970-L1982] |

| 33 | `scripts/parametric/build_orcaflex_library.py` | `main` | independent | [docs/registry/implementation-map.yaml:L1983-L1991] |

| 34 | `scripts/python/digitalmodel/analysis/APIST2RD_Methods.py` | `APISTD2RDCalculation` | independent | [docs/registry/implementation-map.yaml:L1992-L2000] |

| 35 | `scripts/python/digitalmodel/analysis/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | independent | [docs/registry/implementation-map.yaml:L2001-L2011] |

| 36 | `scripts/python/digitalmodel/analysis/RiserStackup.py` | `StackUpDiagram` | independent | [docs/registry/implementation-map.yaml:L2012-L2020] |

| 37 | `scripts/python/digitalmodel/analysis/StackUp_3000Ft.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2021-L2029] |

| 38 | `scripts/python/digitalmodel/analysis/StackUp_9500Ft.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2030-L2038] |

| 39 | `scripts/reporting/build_riser_pipe_comparison.py` | `evaluate` | caller | [docs/registry/implementation-map.yaml:L2039-L2049] |

| 40 | `scripts/reporting/build_riser_pipe_comparison.py` | `_validate` | independent | [docs/registry/implementation-map.yaml:L2050-L2058] |

| 41 | `scripts/reporting/build_riser_pipe_comparison.py` | `numeric_values` | independent | [docs/registry/implementation-map.yaml:L2059-L2067] |

| 42 | `scripts/reporting/build_riser_pipe_comparison.py` | `_input_table` | independent | [docs/registry/implementation-map.yaml:L2068-L2076] |

| 43 | `scripts/riser_database/build_tables.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L2077-L2085] |

| 44 | `src/digitalmodel/asset_integrity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | independent | [docs/registry/implementation-map.yaml:L2086-L2097] |

| 45 | `src/digitalmodel/code_checks/dnv_os_f101.py` | `local_buckling_lcc` | independent | [docs/registry/implementation-map.yaml:L2098-L2106] |

| 46 | `src/digitalmodel/code_checks/workflow.py` | `_run_api_rp_2rd` | independent | [docs/registry/implementation-map.yaml:L2107-L2115] |

| 47 | `src/digitalmodel/code_checks/workflow.py` | `_run_dnv_os_f201` | independent | [docs/registry/implementation-map.yaml:L2116-L2124] |

| 48 | `src/digitalmodel/drilling_riser/assembly.py` | `minimum_top_tension_16q` | independent | [docs/registry/implementation-map.yaml:L2125-L2133] |

| 49 | `src/digitalmodel/drilling_riser/assembly.py` | `_unit_count` | independent | [docs/registry/implementation-map.yaml:L2134-L2142] |

| 50 | `src/digitalmodel/drilling_riser/assembly.py` | `check_rig_capability` | independent | [docs/registry/implementation-map.yaml:L2143-L2153] |

| 51 | `src/digitalmodel/drilling_riser/campaign.py` | `case_spec` | independent | [docs/registry/implementation-map.yaml:L2154-L2164] |

| 52 | `src/digitalmodel/drilling_riser/campaign.py` | `_ring_tensioners` | independent | [docs/registry/implementation-map.yaml:L2165-L2173] |

| 53 | `src/digitalmodel/drilling_riser/campaign.py` | `set_line_tension` | independent | [docs/registry/implementation-map.yaml:L2174-L2184] |

| 54 | `src/digitalmodel/drilling_riser/campaign.py` | `calibrate_line_tension` | independent | [docs/registry/implementation-map.yaml:L2185-L2197] |

| 55 | `src/digitalmodel/drilling_riser/campaign.py` | `_statics_path` | independent | [docs/registry/implementation-map.yaml:L2198-L2207] |

| 56 | `src/digitalmodel/drilling_riser/campaign.py` | `robust_statics` | independent | [docs/registry/implementation-map.yaml:L2208-L2220] |

| 57 | `src/digitalmodel/drilling_riser/campaign.py` | `_tensioner_vertical_n` | independent | [docs/registry/implementation-map.yaml:L2221-L2229] |

| 58 | `src/digitalmodel/drilling_riser/campaign.py` | `tensioner_vertical_target_n` | independent | [docs/registry/implementation-map.yaml:L2230-L2238] |

| 59 | `src/digitalmodel/drilling_riser/campaign.py` | `physical_state_checks` | independent | [docs/registry/implementation-map.yaml:L2239-L2251] |

| 60 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.prepare` | independent | [docs/registry/implementation-map.yaml:L2252-L2263] |

| 61 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.extract` | independent | [docs/registry/implementation-map.yaml:L2264-L2272] |

| 62 | `src/digitalmodel/drilling_riser/envelope.py` | `compute_operating_envelope` | independent | [docs/registry/implementation-map.yaml:L2273-L2281] |

| 63 | `src/digitalmodel/drilling_riser/global_model/build.py` | `equivalent_string_top_tension_n` | independent | [docs/registry/implementation-map.yaml:L2282-L2290] |

| 64 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_tensioner_geometry` | independent | [docs/registry/implementation-map.yaml:L2291-L2299] |

| 65 | `src/digitalmodel/drilling_riser/global_model/build.py` | `winch_tension_n` | independent | [docs/registry/implementation-map.yaml:L2300-L2310] |

| 66 | `src/digitalmodel/drilling_riser/global_model/build.py` | `build_generic_spec` | independent | [docs/registry/implementation-map.yaml:L2311-L2322] |

| 67 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `effective_tension_chain` | independent | [docs/registry/implementation-map.yaml:L2323-L2331] |

| 68 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tension_references` | independent | [docs/registry/implementation-map.yaml:L2332-L2342] |

| 69 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tensioner_lateral_stiffness_n_m` | independent | [docs/registry/implementation-map.yaml:L2343-L2351] |

| 70 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `reference_periods` | caller | [docs/registry/implementation-map.yaml:L2352-L2362] |

| 71 | `src/digitalmodel/drilling_riser/global_model/hang_off.py` | `hung_weight_n` | independent | [docs/registry/implementation-map.yaml:L2363-L2372] |

| 72 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `OpenWaterRiserSpec._closes` | independent | [docs/registry/implementation-map.yaml:L2373-L2381] |

| 73 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `build_open_water_generic_spec` | independent | [docs/registry/implementation-map.yaml:L2382-L2390] |

| 74 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `tension_references` | independent | [docs/registry/implementation-map.yaml:L2391-L2400] |

| 75 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `reference_periods` | independent | [docs/registry/implementation-map.yaml:L2401-L2409] |

| 76 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `release_reference` | independent | [docs/registry/implementation-map.yaml:L2410-L2420] |

| 77 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `ring_vertical_residual_n` | independent | [docs/registry/implementation-map.yaml:L2421-L2429] |

| 78 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `open_water_physical_checks` | independent | [docs/registry/implementation-map.yaml:L2430-L2438] |

| 79 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `Tensioners._failed` | independent | [docs/registry/implementation-map.yaml:L2439-L2447] |

| 80 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `RiserGlobalModelSpec._geometry_closes` | independent | [docs/registry/implementation-map.yaml:L2448-L2456] |

| 81 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `_missing_open_water` | independent | [docs/registry/implementation-map.yaml:L2457-L2465] |

| 82 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `missing_channels` | independent | [docs/registry/implementation-map.yaml:L2466-L2476] |

| 83 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract` | independent | [docs/registry/implementation-map.yaml:L2477-L2485] |

| 84 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract_open_water` | independent | [docs/registry/implementation-map.yaml:L2486-L2494] |

| 85 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `t_min` | independent | [docs/registry/implementation-map.yaml:L2495-L2503] |

| 86 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tension_setting_max` | independent | [docs/registry/implementation-map.yaml:L2504-L2512] |

| 87 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tensioner_stroke` | independent | [docs/registry/implementation-map.yaml:L2513-L2521] |

| 88 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `ScheduleAssembly.minimum_top_tension_16q_kn` | independent | [docs/registry/implementation-map.yaml:L2522-L2530] |

| 89 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `_load_oc_assembly` | independent | [docs/registry/implementation-map.yaml:L2531-L2539] |

| 90 | `src/digitalmodel/drilling_riser/stackup.py` | `effective_tension` | independent | [docs/registry/implementation-map.yaml:L2540-L2548] |

| 91 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `parse_snapshots` | independent | [docs/registry/implementation-map.yaml:L2549-L2557] |

| 92 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `snapshot_to_tension_n` | independent | [docs/registry/implementation-map.yaml:L2558-L2566] |

| 93 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `router` | independent | [docs/registry/implementation-map.yaml:L2567-L2578] |

| 94 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_section_result` | independent | [docs/registry/implementation-map.yaml:L2579-L2587] |

| 95 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_effective_tension_n` | independent | [docs/registry/implementation-map.yaml:L2588-L2596] |

| 96 | `src/digitalmodel/drilling_riser/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L2597-L2608] |

| 97 | `src/digitalmodel/drilling_riser/workflow.py` | `_write_profile_csv` | independent | [docs/registry/implementation-map.yaml:L2609-L2617] |

| 98 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_to_shear7_result` | independent | [docs/registry/implementation-map.yaml:L2618-L2626] |

| 99 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | independent | [docs/registry/implementation-map.yaml:L2627-L2635] |

| 100 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | independent | [docs/registry/implementation-map.yaml:L2636-L2644] |

| 101 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_fatigue_summary_result` | independent | [docs/registry/implementation-map.yaml:L2645-L2653] |

| 102 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_fatigue_damage_results_to_df` | independent | [docs/registry/implementation-map.yaml:L2654-L2667] |

| 103 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L2668-L2676] |

| 104 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L2677-L2685] |

| 105 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | independent | [docs/registry/implementation-map.yaml:L2686-L2697] |

| 106 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | independent | [docs/registry/implementation-map.yaml:L2698-L2708] |

| 107 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | wrapper | [docs/registry/implementation-map.yaml:L2709-L2722] |

| 108 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | independent | [docs/registry/implementation-map.yaml:L2723-L2736] |

| 109 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | independent | [docs/registry/implementation-map.yaml:L2737-L2748] |

| 110 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | independent | [docs/registry/implementation-map.yaml:L2749-L2759] |

| 111 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | independent | [docs/registry/implementation-map.yaml:L2760-L2768] |

| 112 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | independent | [docs/registry/implementation-map.yaml:L2769-L2779] |

| 113 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | independent | [docs/registry/implementation-map.yaml:L2780-L2790] |

| 114 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | independent | [docs/registry/implementation-map.yaml:L2791-L2799] |

| 115 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.prepare_stack_up_properties_df` | caller | [docs/registry/implementation-map.yaml:L2800-L2814] |

| 116 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_from_endA_in_df` | independent | [docs/registry/implementation-map.yaml:L2815-L2823] |

| 117 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_shear7_x_by_L_in_df` | independent | [docs/registry/implementation-map.yaml:L2824-L2832] |

| 118 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_mudline_in_df` | independent | [docs/registry/implementation-map.yaml:L2833-L2841] |

| 119 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_MSL_in_df` | independent | [docs/registry/implementation-map.yaml:L2842-L2850] |

| 120 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_inservice_weights_and_tensions` | caller | [docs/registry/implementation-map.yaml:L2851-L2864] |

| 121 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.assign_key_properties_to_df` | independent | [docs/registry/implementation-map.yaml:L2865-L2873] |

| 122 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stack_in_service_weights` | independent | [docs/registry/implementation-map.yaml:L2874-L2882] |

| 123 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_effective_tension` | independent | [docs/registry/implementation-map.yaml:L2883-L2891] |

| 124 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stretch_in_riser` | independent | [docs/registry/implementation-map.yaml:L2892-L2900] |

| 125 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.title` | independent | [docs/registry/implementation-map.yaml:L2901-L2909] |

| 126 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.riser_joints_to_shear7_zones` | independent | [docs/registry/implementation-map.yaml:L2910-L2918] |

| 127 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.add_block_3` | independent | [docs/registry/implementation-map.yaml:L2919-L2927] |

| 128 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_to_shear7_result` | independent | [docs/registry/implementation-map.yaml:L2928-L2936] |

| 129 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | independent | [docs/registry/implementation-map.yaml:L2937-L2945] |

| 130 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | independent | [docs/registry/implementation-map.yaml:L2946-L2954] |

| 131 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | independent | [docs/registry/implementation-map.yaml:L2955-L2963] |

| 132 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_fatigue_damage_results_to_df` | copy | [docs/registry/implementation-map.yaml:L2964-L2977] |

| 133 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L2978-L2986] |

| 134 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L2987-L2995] |

| 135 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py` | `_write_tensions_csv` | independent | [docs/registry/implementation-map.yaml:L2996-L3004] |

| 136 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._line_state` | independent | [docs/registry/implementation-map.yaml:L3005-L3015] |

| 137 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._solve_line` | independent | [docs/registry/implementation-map.yaml:L3016-L3024] |

| 138 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._result` | independent | [docs/registry/implementation-map.yaml:L3025-L3033] |

| 139 | `src/digitalmodel/orcaflex/mooring_design.py` | `solve_catenary` | independent | [docs/registry/implementation-map.yaml:L3034-L3042] |

| 140 | `src/digitalmodel/orcaflex/pipelay_analysis.py` | `JLayConfig.calculate_sagbend` | independent | [docs/registry/implementation-map.yaml:L3043-L3051] |

| 141 | `src/digitalmodel/orcaflex/riser_config.py` | `SCRDesignInput.calculate_geometry` | independent | [docs/registry/implementation-map.yaml:L3052-L3060] |

| 142 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_stroke` | independent | [docs/registry/implementation-map.yaml:L3061-L3069] |

| 143 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_top_tension` | independent | [docs/registry/implementation-map.yaml:L3070-L3078] |

| 144 | `src/digitalmodel/orcaflex/riser_input_schema.py` | `_TopTensionedInput.to_legacy` | independent | [docs/registry/implementation-map.yaml:L3079-L3087] |

| 145 | `src/digitalmodel/orcaflex/viv_screening.py` | `BeamProperties.natural_frequency` | independent | [docs/registry/implementation-map.yaml:L3088-L3096] |

| 146 | `src/digitalmodel/parametric/generate.py` | `_code_check_utilisation` | independent | [docs/registry/implementation-map.yaml:L3097-L3105] |

| 147 | `src/digitalmodel/pipelay/workflow.py` | `run_jlay` | independent | [docs/registry/implementation-map.yaml:L3106-L3114] |

| 148 | `src/digitalmodel/riser_configuration_screen/static.py` | `_lazy_row` | independent | [docs/registry/implementation-map.yaml:L3115-L3123] |

| 149 | `src/digitalmodel/riser_configuration_screen/static.py` | `scr_offsets` | independent | [docs/registry/implementation-map.yaml:L3124-L3132] |

| 150 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_validate` | independent | [docs/registry/implementation-map.yaml:L3133-L3141] |

| 151 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_utilisation` | independent | [docs/registry/implementation-map.yaml:L3142-L3150] |

| 152 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_checks` | independent | [docs/registry/implementation-map.yaml:L3151-L3159] |

| 153 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/groups_builder.py` | `GroupsBuilder._build_from_spec` | independent | [docs/registry/implementation-map.yaml:L3160-L3168] |

| 154 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `LineTypeBuilder.build` | independent | [docs/registry/implementation-map.yaml:L3169-L3177] |

| 155 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.should_generate` | independent | [docs/registry/implementation-map.yaml:L3178-L3186] |

| 156 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.build` | independent | [docs/registry/implementation-map.yaml:L3187-L3195] |

| 157 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `_apply_overrides` | independent | [docs/registry/implementation-map.yaml:L3196-L3204] |

| 158 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.mooring_pre_tension_analysis` | independent | [docs/registry/implementation-map.yaml:L3205-L3215] |

| 159 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.evaluate_line_length_next_iteration` | independent | [docs/registry/implementation-map.yaml:L3216-L3224] |

| 160 | `src/digitalmodel/solvers/orcaflex/opp_range_graph.py` | `OPPRangeGraph.postProcessRange` | independent | [docs/registry/implementation-map.yaml:L3225-L3233] |

| 161 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.postProcessRange` | independent | [docs/registry/implementation-map.yaml:L3234-L3242] |

| 162 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | independent | [docs/registry/implementation-map.yaml:L3243-L3254] |

| 163 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | copy | [docs/registry/implementation-map.yaml:L3255-L3265] |

| 164 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | wrapper | [docs/registry/implementation-map.yaml:L3266-L3279] |

| 165 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | copy | [docs/registry/implementation-map.yaml:L3280-L3293] |

| 166 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | copy | [docs/registry/implementation-map.yaml:L3294-L3305] |

| 167 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | copy | [docs/registry/implementation-map.yaml:L3306-L3316] |

| 168 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | independent | [docs/registry/implementation-map.yaml:L3317-L3325] |

| 169 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | copy | [docs/registry/implementation-map.yaml:L3326-L3336] |

| 170 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | copy | [docs/registry/implementation-map.yaml:L3337-L3347] |

| 171 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | independent | [docs/registry/implementation-map.yaml:L3348-L3356] |

| 172 | `src/digitalmodel/structural/analysis/cli.py` | `check` | independent | [docs/registry/implementation-map.yaml:L3357-L3365] |

| 173 | `src/digitalmodel/structural/analysis/wall_thickness.py` | `WallThicknessAnalyzer.check_combined_loading` | independent | [docs/registry/implementation-map.yaml:L3366-L3374] |

| 174 | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py` | `ApiStd2rdStrategy._check_combined` | independent | [docs/registry/implementation-map.yaml:L3375-L3383] |

| 175 | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py` | `DnvStF101Strategy._check_combined_loading` | independent | [docs/registry/implementation-map.yaml:L3384-L3392] |

| 176 | `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py` | `run_wall_thickness_compare` | independent | [docs/registry/implementation-map.yaml:L3393-L3401] |

| 177 | `src/digitalmodel/structural/analysis/wall_thickness_lookup.py` | `MTLookupGenerator.generate` | independent | [docs/registry/implementation-map.yaml:L3402-L3410] |

| 178 | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `ParametricSweep.run` | independent | [docs/registry/implementation-map.yaml:L3411-L3419] |

| 179 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `PipelinePhase.to_design_loads` | independent | [docs/registry/implementation-map.yaml:L3420-L3428] |

| 180 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `create_standard_phases` | independent | [docs/registry/implementation-map.yaml:L3429-L3437] |

| 181 | `src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py` | `LoadScalingProcessor.process_all_conditions` | independent | [docs/registry/implementation-map.yaml:L3438-L3446] |

| 182 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L3447-L3455] |

| 183 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | independent | [docs/registry/implementation-map.yaml:L3456-L3464] |

| 184 | `src/digitalmodel/structural/fatigue_apps/load_scaling.py` | `LoadScalingProcessor.load_reference_data` | independent | [docs/registry/implementation-map.yaml:L3465-L3473] |

| 185 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L3474-L3482] |

| 186 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L3483-L3491] |

| 187 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_sea_state` | independent | [docs/registry/implementation-map.yaml:L3492-L3500] |

| 188 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `main` | independent | [docs/registry/implementation-map.yaml:L3501-L3509] |

| 189 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py` | `ProductionDataHandlerWithProgress.process_single_analysis` | independent | [docs/registry/implementation-map.yaml:L3510-L3518] |

| 190 | `src/digitalmodel/structural/parametric_coordinator.py` | `ParametricCoordinator._run_wall_thickness` | independent | [docs/registry/implementation-map.yaml:L3519-L3527] |

| 191 | `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | copy | [docs/registry/implementation-map.yaml:L3528-L3539] |

| 192 | `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | copy | [docs/registry/implementation-map.yaml:L3540-L3550] |

| 193 | `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | copy | [docs/registry/implementation-map.yaml:L3551-L3562] |

| 194 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `simple_catenary` | independent | [docs/registry/implementation-map.yaml:L3563-L3571] |

| 195 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `lazy_wave_analysis` | independent | [docs/registry/implementation-map.yaml:L3572-L3580] |

| 196 | `src/digitalmodel/subsea/catenary_riser/lazy_wave.py` | `LazyWaveAnalyzer.analyze` | independent | [docs/registry/implementation-map.yaml:L3581-L3589] |

| 197 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.analyze_riser` | caller | [docs/registry/implementation-map.yaml:L3590-L3601] |

| 198 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_catenary_from_H` | independent | [docs/registry/implementation-map.yaml:L3602-L3610] |

| 199 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_H_from_top_tension` | independent | [docs/registry/implementation-map.yaml:L3611-L3619] |

| 200 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.check_design_limits` | independent | [docs/registry/implementation-map.yaml:L3620-L3628] |

| 201 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `developed_min_radius` | independent | [docs/registry/implementation-map.yaml:L3629-L3637] |

| 202 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `required_effective_ei` | independent | [docs/registry/implementation-map.yaml:L3638-L3646] |

| 203 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `screen_bend_stiffener` | independent | [docs/registry/implementation-map.yaml:L3647-L3658] |

| 204 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `armour_tension_stress` | independent | [docs/registry/implementation-map.yaml:L3659-L3667] |

| 205 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `verify_flexible_pipe` | independent | [docs/registry/implementation-map.yaml:L3668-L3678] |

| 206 | `src/digitalmodel/subsea/pipeline/global_buckling/hobbs_lateral.py` | `effective_driving_force` | independent | [docs/registry/implementation-map.yaml:L3679-L3687] |

| 207 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `RiserLoadCase.to_loads` | independent | [docs/registry/implementation-map.yaml:L3688-L3696] |

| 208 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `run_riser_screening` | independent | [docs/registry/implementation-map.yaml:L3697-L3705] |

| 209 | `src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py` | `StackUpDiagram` | independent | [docs/registry/implementation-map.yaml:L3706-L3714] |

| 210 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_riser_model` | caller | [docs/registry/implementation-map.yaml:L3715-L3728] |

| 211 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.read_riser_input_data` | independent | [docs/registry/implementation-map.yaml:L3729-L3737] |

| 212 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.assign_key_properties_to_self` | independent | [docs/registry/implementation-map.yaml:L3738-L3746] |

| 213 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.get_fea_properties` | independent | [docs/registry/implementation-map.yaml:L3747-L3755] |

| 214 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_tensioner_data` | independent | [docs/registry/implementation-map.yaml:L3756-L3764] |

| 215 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.evaluate_stack_up_properties` | independent | [docs/registry/implementation-map.yaml:L3765-L3773] |

| 216 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.save_data` | independent | [docs/registry/implementation-map.yaml:L3774-L3782] |

| 217 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.alter_properties_for_Shear7_software` | independent | [docs/registry/implementation-map.yaml:L3783-L3791] |

| 218 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_natural_frequency` | caller | [docs/registry/implementation-map.yaml:L3792-L3802] |

| 219 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator._frequency_tension_controlled` | independent | [docs/registry/implementation-map.yaml:L3803-L3811] |

| 220 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_effective_tension` | independent | [docs/registry/implementation-map.yaml:L3812-L3820] |

| 221 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.check_stability` | caller | [docs/registry/implementation-map.yaml:L3821-L3831] |

| 222 | `src/digitalmodel/viv_screening/inputs.py` | `_beam` | independent | [docs/registry/implementation-map.yaml:L3832-L3840] |

| 223 | `src/digitalmodel/workflows/workflow_automation/cli.py` | `riser_analysis_cmd` | independent | [docs/registry/implementation-map.yaml:L3841-L3849] |

| 224 | `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_riser_analysis_template` | independent | [docs/registry/implementation-map.yaml:L3850-L3858] |

| 225 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `CompleteRiserAnalysisWorkflow.create` | independent | [docs/registry/implementation-map.yaml:L3859-L3867] |

| 226 | `tests/asset_integrity/test_calculations.py` | `_make_pipe_cfg` | test_helper | [docs/registry/implementation-map.yaml:L3868-L3876] |

| 227 | `tests/capabilities/test_wall_thickness_3d_page.py` | `_study` | test_helper | [docs/registry/implementation-map.yaml:L3877-L3885] |

| 228 | `tests/contracts/test_force_units_boundary_contract.py` | `<module>` | test_helper | [docs/registry/implementation-map.yaml:L3886-L3894] |

| 229 | `tests/contracts/test_force_units_boundary_contract.py` | `test_envelope_von_mises_n_to_kn_seam` | test_helper | [docs/registry/implementation-map.yaml:L3895-L3905] |

| 230 | `tests/drilling_riser/global_model/conftest.py` | `synthetic_spec` | test_helper | [docs/registry/implementation-map.yaml:L3906-L3914] |

| 231 | `tests/drilling_riser/global_model/test_batch_params.py` | `test_physical_checks_use_the_remaining_tensioners` | test_helper | [docs/registry/implementation-map.yaml:L3915-L3924] |

| 232 | `tests/drilling_riser/global_model/test_campaign.py` | `_fake` | test_helper | [docs/registry/implementation-map.yaml:L3925-L3934] |

| 233 | `tests/drilling_riser/global_model/test_open_water.py` | `open_water_dict` | test_helper | [docs/registry/implementation-map.yaml:L3935-L3943] |

| 234 | `tests/drilling_riser/global_model/test_open_water_campaign.py` | `_doc` | test_helper | [docs/registry/implementation-map.yaml:L3944-L3952] |

| 235 | `tests/drilling_riser/global_model/test_orcaflex_solver.py` | `test_statics_tension_and_modes_agree_with_hand_checks` | test_helper | [docs/registry/implementation-map.yaml:L3953-L3965] |

| 236 | `tests/drilling_riser/global_model/test_statics_path.py` | `test_branch_tolerance_and_ring_buoyancy` | test_helper | [docs/registry/implementation-map.yaml:L3966-L3975] |

| 237 | `tests/drilling_riser/global_model/test_tensioner_representation.py` | `_vertical` | test_helper | [docs/registry/implementation-map.yaml:L3976-L3984] |

| 238 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_mc` | test_helper | [docs/registry/implementation-map.yaml:L3985-L3993] |

| 239 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_case` | test_helper | [docs/registry/implementation-map.yaml:L3994-L4004] |

| 240 | `tests/drilling_riser/test_drive_off_worked_example.py` | `_recorded_arguments` | test_helper | [docs/registry/implementation-map.yaml:L4005-L4013] |

| 241 | `tests/drilling_riser/test_schedule_assembly.py` | `_deltas` | test_helper | [docs/registry/implementation-map.yaml:L4014-L4022] |

| 242 | `tests/infrastructure/unit/test_pipe_capacity_codes.py` | `_base_cfg` | test_helper | [docs/registry/implementation-map.yaml:L4023-L4031] |

| 243 | `tests/orcaflex/test_generic_scr_slwr.py` | `test_analytical_reference_reproduces_from_basis` | test_helper | [docs/registry/implementation-map.yaml:L4032-L4040] |

| 244 | `tests/parametric/test_atlas.py` | `_code_check_atlas` | test_helper | [docs/registry/implementation-map.yaml:L4041-L4049] |

| 245 | `tests/reporting/test_riser_pipe_comparison.py` | `reference_input` | test_helper | [docs/registry/implementation-map.yaml:L4050-L4058] |

| 246 | `tests/riser_configuration_screen/test_workflow.py` | `settings` | test_helper | [docs/registry/implementation-map.yaml:L4059-L4067] |

| 247 | `tests/solvers/orcaflex/analysis/scripts/verify_configs.py` | `verify_configuration` | test_helper | [docs/registry/implementation-map.yaml:L4068-L4076] |

| 248 | `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py` | `_make_slay_spec` | test_helper | [docs/registry/implementation-map.yaml:L4077-L4085] |

| 249 | `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` | `_make_pipeline_spec` | test_helper | [docs/registry/implementation-map.yaml:L4086-L4094] |

| 250 | `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` | `_make_pipeline_spec` | test_helper | [docs/registry/implementation-map.yaml:L4095-L4103] |

| 251 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_slay_campaign` | test_helper | [docs/registry/implementation-map.yaml:L4104-L4112] |

| 252 | `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py` | `_make_base_spec_data` | test_helper | [docs/registry/implementation-map.yaml:L4113-L4121] |

| 253 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` | `extract_static_range_results` | test_helper | [docs/registry/implementation-map.yaml:L4122-L4130] |

| 254 | `tests/solvers/orcaflex/modular_generator/test_post_validator.py` | `_make_valid_model` | test_helper | [docs/registry/implementation-map.yaml:L4131-L4139] |

| 255 | `tests/solvers/orcaflex/test_slay_layout.py` | `test_support_types_are_defined_before_vessel_references` | test_helper | [docs/registry/implementation-map.yaml:L4140-L4148] |

| 256 | `tests/specialized/umbilical_analysis/conftest.py` | `installation_parameters` | test_helper | [docs/registry/implementation-map.yaml:L4149-L4157] |

| 257 | `tests/subsea/catenary_riser/test_catenary.py` | `run_catenary_riser` | test_helper | [docs/registry/implementation-map.yaml:L4158-L4166] |

| 258 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_catenary_analysis_mock` | test_helper | [docs/registry/implementation-map.yaml:L4167-L4175] |

| 259 | `tests/test_automation/scripts/batch_fix_tests.py` | `<module>` | test_helper | [docs/registry/implementation-map.yaml:L4176-L4184] |

| 260 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | test_helper | [docs/registry/implementation-map.yaml:L4185-L4193] |

| 261 | `tests/test_rainflow_with_effective_tension.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L4194-L4204] |

| 262 | `tests/test_wall_thickness_compare_workflow.py` | `_cfg` | test_helper | [docs/registry/implementation-map.yaml:L4205-L4213] |

| 263 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper | [docs/registry/implementation-map.yaml:L4214-L4222] |

| 264 | `tests/workflows/test_viv_screening_workflow.py` | `screening_input` | test_helper | [docs/registry/implementation-map.yaml:L4223-L4231] |

### 9.2. api-5ct-5l-grade-tables

Exact case-sensitive Python `re` search signals:

```text
algorithm: (?:API.?5[CLT]|_api5l|API5L|API5CT|MATERIAL_LIBRARY|STEEL_GRADES|GRADE_REGISTRY|_GRADES|steel_material|smys|SMYS)
vocabulary: API.?5CT|API.?5L|steel.grade|casing.grade
constants_or_keys: \b(?:J55|K55|N80|L80|P110|Q125|X42|X52|X60|X65|X70|X80)\b
```

[docs/registry/implementation-map.yaml:L21644-L21646]

| Map row | Path | Symbol (`<module>` means module-level) | Relation as recorded | Source |
| --- | --- | --- | --- | --- |

| 1 | `docs/domains/orcaflex/templates/components/materials/steel.csv` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21648-L21656] |

| 2 | `examples/demos/asset_integrity/ffs_lookup_build.py` | `<module>` | example | [docs/registry/implementation-map.yaml:L21657-L21666] |

| 3 | `examples/demos/asset_integrity/ffs_lookup_build.py` | `build_records` | example | [docs/registry/implementation-map.yaml:L21667-L21675] |

| 4 | `Discovery file with identifying name (locator withheld)` | `Definition at this map row` | example | [docs/registry/implementation-map.yaml:L21676-L21685] |

| 5 | `Discovery file with identifying name (locator withheld)` | `Definition at this map row` | example | [docs/registry/implementation-map.yaml:L21686-L21694] |

| 6 | `examples/domains/structural/wall_thickness_example.py` | `run_single_design_check` | example | [docs/registry/implementation-map.yaml:L21695-L21703] |

| 7 | `examples/domains/structural/wall_thickness_example.py` | `run_phase_analysis` | example | [docs/registry/implementation-map.yaml:L21704-L21712] |

| 8 | `scripts/capabilities/build_casing_design_explorer.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21713-L21721] |

| 9 | `src/digitalmodel/base_configs/modules/pipe_capacity/pipe_capacity.yml` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21722-L21730] |

| 10 | `src/digitalmodel/base_configs/modules/vertical_riser/vertical_riser.yml` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21731-L21739] |

| 11 | `src/digitalmodel/data_systems/data/steel_material.yml` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21740-L21748] |

| 12 | `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` | `PipeSpecificationClient` | independent | [docs/registry/implementation-map.yaml:L21749-L21757] |

| 13 | `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` | `PipeSpecificationClient._initialize_pipe_database` | caller | [docs/registry/implementation-map.yaml:L21758-L21768] |

| 14 | `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` | `PipeSpecificationClient._generate_pipe_spec` | independent | [docs/registry/implementation-map.yaml:L21769-L21777] |

| 15 | `src/digitalmodel/infrastructure/templates/api_std_2rd_template.yml` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21778-L21786] |

| 16 | `src/digitalmodel/materials/grades.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21787-L21796] |

| 17 | `src/digitalmodel/orcaflex/mooring_design.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21797-L21806] |

| 18 | `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineSegment` | independent | [docs/registry/implementation-map.yaml:L21807-L21815] |

| 19 | `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineDesign.estimate_catenary` | independent | [docs/registry/implementation-map.yaml:L21816-L21824] |

| 20 | `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineDesign.check_mbl` | independent | [docs/registry/implementation-map.yaml:L21825-L21833] |

| 21 | `src/digitalmodel/orcaflex/mooring_design.py` | `MooringLineDesign.check_mbl_with_safety_factor` | independent | [docs/registry/implementation-map.yaml:L21834-L21842] |

| 22 | `src/digitalmodel/orcaflex/riser_config.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21843-L21852] |

| 23 | `src/digitalmodel/orcaflex/riser_config.py` | `RiserPipeProperties.steel_mass_per_m` | independent | [docs/registry/implementation-map.yaml:L21853-L21861] |

| 24 | `src/digitalmodel/orcaflex/riser_config.py` | `RiserPipeProperties.bending_stiffness` | independent | [docs/registry/implementation-map.yaml:L21862-L21870] |

| 25 | `src/digitalmodel/orcaflex/riser_config.py` | `RiserPipeProperties.axial_stiffness` | independent | [docs/registry/implementation-map.yaml:L21871-L21879] |

| 26 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21880-L21889] |

| 27 | `src/digitalmodel/structural/analysis/__init__.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21890-L21898] |

| 28 | `src/digitalmodel/structural/analysis/cli.py` | `check` | independent | [docs/registry/implementation-map.yaml:L21899-L21907] |

| 29 | `src/digitalmodel/structural/analysis/cli.py` | `sweep` | independent | [docs/registry/implementation-map.yaml:L21908-L21916] |

| 30 | `src/digitalmodel/structural/analysis/cli.py` | `phases` | independent | [docs/registry/implementation-map.yaml:L21917-L21925] |

| 31 | `src/digitalmodel/structural/analysis/cli.py` | `list_grades` | independent | [docs/registry/implementation-map.yaml:L21926-L21934] |

| 32 | `src/digitalmodel/structural/analysis/cli.py` | `lookup` | independent | [docs/registry/implementation-map.yaml:L21935-L21943] |

| 33 | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21944-L21953] |

| 34 | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `ParametricSweep.run` | independent | [docs/registry/implementation-map.yaml:L21954-L21962] |

| 35 | `src/digitalmodel/subsea/catenary_riser/models.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21963-L21972] |

| 36 | `src/digitalmodel/subsea/pipeline/pipeline_pressure.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21973-L21982] |

| 37 | `src/digitalmodel/subsea/pipeline/pipeline_pressure_workflow.py` | `WallThicknessSizingWorkflow.run` | independent | [docs/registry/implementation-map.yaml:L21983-L21991] |

| 38 | `src/digitalmodel/well/tubulars/casing_design.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L21992-L22000] |

| 39 | `tests/fixtures/test_vectors/mooring_risers/riser_material_properties.yaml` | `<module>` | test_helper | [docs/registry/implementation-map.yaml:L22001-L22009] |

### 9.3. Definition audit and map follow-up

The package grep assigns each matching line to its innermost function or class; uncovered top-level statements share one module definition. The resulting 55 effective-tension definitions and 89 grade-table definitions are classified below; repeated hits inside one definition count once per family.

| Family | File and definition | Classification | Evidence |
| --- | --- | --- | --- |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/cli.py::check` | Already in inventory | [src/digitalmodel/structural/analysis/cli.py:L83-L161] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/cli.py::sweep` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/cli.py:L199-L249] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/cli.py::lookup` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/cli.py:L366-L411] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/cli.py::report` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/cli.py:L435-L492] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness.py::DesignLoads` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/wall_thickness.py:L164-L174] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness.py::DesignLoads.net_internal_pressure` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/wall_thickness.py:L173-L174] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_pressure_containment` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness.py:L251-L300] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_collapse` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness.py:L302-L358] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_propagation_buckling` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness.py:L396-L433] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_combined_loading` | Already in inventory | [src/digitalmodel/structural/analysis/wall_thickness.py:L435-L497] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_api_burst` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness.py:L503-L547] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_api_collapse` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness.py:L549-L595] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_api_propagation` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness.py:L597-L631] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py::ApiRp1111Strategy._check_burst` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L100-L133] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py::ApiRp1111Strategy._check_collapse` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L135-L171] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py::ApiRp1111Strategy._check_propagation` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L173-L199] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py::ApiRp2rdStrategy._check_burst` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L69-L103] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py::ApiRp2rdStrategy._check_collapse` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L105-L145] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py::ApiRp2rdStrategy._check_hoop` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L147-L177] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py::ApiStd2rdStrategy._check_burst` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L83-L117] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py::ApiStd2rdStrategy._check_collapse` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L119-L159] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py::ApiStd2rdStrategy._check_combined` | Already in inventory | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L161-L226] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py::AsmeB314Strategy._check_burst` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L70-L118] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py::AsmeB314Strategy._check_collapse` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L120-L164] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py::AsmeB314Strategy._check_propagation` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L166-L200] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py::AsmeB318Strategy._check_burst` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py:L60-L109] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py::AsmeB318Strategy._check_collapse` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py:L111-L153] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py::AsmeB318Strategy._check_propagation` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py:L155-L187] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py::DnvStF101Strategy._check_pressure_containment` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L117-L161] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py::DnvStF101Strategy._check_collapse` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L163-L205] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py::DnvStF101Strategy._check_propagation_buckling` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L237-L266] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py::DnvStF101Strategy._check_combined_loading` | Already in inventory | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L268-L315] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py::DnvStF201Strategy._check_pressure_containment` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L155-L205] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py::Iso13623Strategy._check_pressure_containment` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L57-L101] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py::Iso13623Strategy._check_collapse` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L103-L151] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py::Iso13623Strategy._check_propagation` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L178-L211] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py::Pd80102Strategy._check_hoop_stress` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L59-L99] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py::Pd80102Strategy._check_collapse` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L101-L150] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py::Pd80102Strategy._check_propagation` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L177-L209] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py::run_wall_thickness_compare` | Already in inventory | [src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py:L41-L109] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py::build_compare_note` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py:L112-L185] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_comparison.py::_build_comparison_html` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/wall_thickness_comparison.py:L127-L287] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_interactive_report.py::InteractiveReportBuilder.add_utilisation_vs_wt_chart` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/wall_thickness_interactive_report.py:L78-L161] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_lookup.py::LookupConfig` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/wall_thickness_lookup.py:L35-L45] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_lookup.py::MTLookupGenerator.generate` | Already in inventory | [src/digitalmodel/structural/analysis/wall_thickness_lookup.py:L60-L130] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_mt_report.py::generate_mt_report` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L78-L233] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py::SweepConfig` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L44-L57] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py::ParametricSweep.run` | Already in inventory | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L66-L147] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py::plot_tension_moment_interaction` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L303-L487] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py::generate_report` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L490-L549] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_phases.py::PipelinePhase` | Load/data helper, delegated analysis or report formatting | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L128-L148] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_phases.py::PipelinePhase.to_design_loads` | Already in inventory | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L141-L148] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_phases.py::create_standard_phases` | Already in inventory | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L156-L220] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_phases.py::_dnv_combined_utilisation_grid` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L360-L422] |

| effective-tension-riser-stack-up | `src/digitalmodel/structural/analysis/wall_thickness_phases.py::plot_phase_tm_interaction` | Arithmetic listed in section 3 | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L498-L665] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/__init__.py::<module>` | Already in inventory | [src/digitalmodel/structural/analysis/__init__.py:L1-L80] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/cli.py::<module>` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/cli.py:L1-L501] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/cli.py::check` | Already in inventory | [src/digitalmodel/structural/analysis/cli.py:L83-L161] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/cli.py::sweep` | Already in inventory | [src/digitalmodel/structural/analysis/cli.py:L199-L249] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/cli.py::phases` | Already in inventory | [src/digitalmodel/structural/analysis/cli.py:L273-L325] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/cli.py::list_grades` | Already in inventory | [src/digitalmodel/structural/analysis/cli.py:L329-L342] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/cli.py::lookup` | Already in inventory | [src/digitalmodel/structural/analysis/cli.py:L366-L411] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/cli.py::report` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/cli.py:L435-L492] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness.py::PipeMaterial` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness.py:L133-L160] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness.py::PipeMaterial.__post_init__` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness.py:L143-L151] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_pressure_containment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness.py:L251-L300] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_collapse` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness.py:L302-L358] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_propagation_buckling` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness.py:L396-L433] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_combined_loading` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness.py:L435-L497] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_api_burst` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness.py:L503-L547] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_api_collapse` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness.py:L549-L595] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness.py::WallThicknessAnalyzer.check_api_propagation` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness.py:L597-L631] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py::ApiRp1111Strategy.compute_plastic_moment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L87-L91] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py::ApiRp1111Strategy.compute_plastic_tension` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L93-L98] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py::ApiRp1111Strategy._check_burst` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L100-L133] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py::ApiRp1111Strategy._check_collapse` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L135-L171] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py::ApiRp1111Strategy._check_propagation` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L173-L199] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py::<module>` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L1-L177] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py::ApiRp2rdStrategy` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L23-L177] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py::ApiRp2rdStrategy.compute_plastic_moment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L50-L57] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py::ApiRp2rdStrategy.compute_plastic_tension` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L59-L67] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py::ApiRp2rdStrategy._check_burst` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L69-L103] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py::ApiRp2rdStrategy._check_collapse` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L105-L145] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py::ApiRp2rdStrategy._check_hoop` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L147-L177] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py::ApiStd2rdStrategy.compute_yield_moment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L53-L60] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py::ApiStd2rdStrategy.compute_plastic_moment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L62-L71] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py::ApiStd2rdStrategy.compute_plastic_tension` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L73-L81] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py::ApiStd2rdStrategy._check_burst` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L83-L117] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py::ApiStd2rdStrategy._check_collapse` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L119-L159] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py::ApiStd2rdStrategy._check_combined` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L161-L226] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py::AsmeB314Strategy.compute_plastic_moment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L55-L60] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py::AsmeB314Strategy.compute_plastic_tension` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L62-L68] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py::AsmeB314Strategy._check_burst` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L70-L118] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py::AsmeB314Strategy._check_collapse` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L120-L164] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py::AsmeB314Strategy._check_propagation` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L166-L200] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py::AsmeB318Strategy.compute_plastic_moment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py:L47-L51] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py::AsmeB318Strategy.compute_plastic_tension` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py:L53-L58] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py::AsmeB318Strategy._check_burst` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py:L60-L109] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py::AsmeB318Strategy._check_collapse` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py:L111-L153] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py::AsmeB318Strategy._check_propagation` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py:L155-L187] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py::<module>` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L1-L315] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py::DnvStF101Strategy.compute_plastic_moment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L107-L110] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py::DnvStF101Strategy.compute_plastic_tension` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L112-L115] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py::DnvStF101Strategy._check_pressure_containment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L117-L161] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py::DnvStF101Strategy._check_collapse` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L163-L205] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py::DnvStF101Strategy._check_propagation_buckling` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L237-L266] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py::DnvStF101Strategy._check_combined_loading` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L268-L315] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py::<module>` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L1-L205] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py::DnvStF201Strategy` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L68-L205] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py::DnvStF201Strategy.compute_plastic_moment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L143-L147] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py::DnvStF201Strategy.compute_plastic_tension` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L149-L153] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py::DnvStF201Strategy._check_pressure_containment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L155-L205] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py::Iso13623Strategy.compute_plastic_moment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L47-L50] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py::Iso13623Strategy.compute_plastic_tension` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L52-L55] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py::Iso13623Strategy._check_pressure_containment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L57-L101] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py::Iso13623Strategy._check_collapse` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L103-L151] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py::Iso13623Strategy._check_propagation` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L178-L211] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py::Pd80102Strategy.compute_plastic_moment` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L46-L50] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py::Pd80102Strategy.compute_plastic_tension` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L52-L57] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py::Pd80102Strategy._check_hoop_stress` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L59-L99] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py::Pd80102Strategy._check_collapse` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L101-L150] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py::Pd80102Strategy._check_propagation` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L177-L209] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py::run_wall_thickness_compare` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py:L41-L109] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py::build_compare_note` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py:L112-L185] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_comparison.py::_build_comparison_html` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_comparison.py:L127-L287] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_interactive_report.py::<module>` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_interactive_report.py:L1-L336] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_mt_report.py::generate_mt_report` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L78-L233] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_mt_report.py::_find_limit_bisection` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L321-L374] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_mt_report.py::_compute_capacity_limits` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L377-L412] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_mt_report.py::_trace_envelope` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L415-L442] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_mt_report.py::_trace_envelope_method1` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L445-L473] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_mt_report.py::_trace_envelope_method2` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L476-L509] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_mt_report.py::_compute_pressure_sensitivity` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L512-L556] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_mt_report.py::_build_contour_chart` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L654-L818] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_mt_report.py::_build_html` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L1034-L1522] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py::<module>` | Already in inventory | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L1-L575] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py::SweepConfig` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L44-L57] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py::ParametricSweep.run` | Already in inventory | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L66-L147] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py::plot_tension_moment_interaction` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L303-L487] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_phases.py::PipeDefinition` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L82-L124] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_phases.py::PipeDefinition.to_material` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L117-L124] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_phases.py::_dnv_combined_utilisation_grid` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L360-L422] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_phases.py::plot_phase_tm_interaction` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L498-L665] |

| api-5ct-5l-grade-tables | `src/digitalmodel/structural/analysis/wall_thickness_phases.py::generate_phase_report` | Grade input consumer, data holder or export; no independent grade-table result | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L678-L770] |

Audit result: 0 further second implementations summed over both counted families after the section-3 inventory; A8 passes the producer's classification. Formula-bearing pressure/interaction routines are named in section 3; grade consumers do not create a second grade matrix. The retained map rows themselves include helpers and tests and are not silently removed.

The whole-repository search included `scripts/` and other tracked text files up to the map's one-MiB text limit: 647 files matched effective-tension signals and 926 matched grade-table signals. These are discovery counts, not additional independent implementations. The full map inventory above includes script definitions as well as package definitions.

Map follow-up, issue pending (4): add wall-thickness burst, collapse, propagation and combined-load families, hydrostatic phase pressure, section-capacity formulas and equivalent-stress/report-envelope methods from section 3 before counting them in a later map revision. None has an in-package family row in this map revision. Reconcile map helper/test classifications and audit uncovered external implementations; external gaps do not change this A8 result.

Public inventory locators are withheld for 5 discovery files with identifying names; map-row ordinals and map citations retain an unambiguous inventory. Codenaming and any necessary private provenance entry remain issue pending (4); sidecar entry: to be created. No client-derived numeric result is reproduced.

## 10. Coverage and open items

Coverage record: 21 Python files, 7130 lines, plus the strategy manifest were inspected; the following source inventory anchors that scope at the code pin.

| Source file | Read extent |
| --- | --- |

| `src/digitalmodel/structural/analysis/__init__.py` | [src/digitalmodel/structural/analysis/__init__.py:L1-L80] |

| `src/digitalmodel/structural/analysis/cli.py` | [src/digitalmodel/structural/analysis/cli.py:L1-L501] |

| `src/digitalmodel/structural/analysis/wall_thickness.py` | [src/digitalmodel/structural/analysis/wall_thickness.py:L1-L678] |

| `src/digitalmodel/structural/analysis/wall_thickness_codes/__init__.py` | [src/digitalmodel/structural/analysis/wall_thickness_codes/__init__.py:L1-L13] |

| `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py` | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py:L1-L199] |

| `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py` | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:L1-L177] |

| `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py` | [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L1-L226] |

| `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py` | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py:L1-L200] |

| `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py` | [src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_8.py:L1-L187] |

| `src/digitalmodel/structural/analysis/wall_thickness_codes/base.py` | [src/digitalmodel/structural/analysis/wall_thickness_codes/base.py:L1-L57] |

| `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py` | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L1-L315] |

| `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py` | [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L1-L205] |

| `src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py` | [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L1-L211] |

| `src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py` | [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L1-L209] |

| `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py` | [src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py:L1-L185] |

| `src/digitalmodel/structural/analysis/wall_thickness_comparison.py` | [src/digitalmodel/structural/analysis/wall_thickness_comparison.py:L1-L338] |

| `src/digitalmodel/structural/analysis/wall_thickness_interactive_report.py` | [src/digitalmodel/structural/analysis/wall_thickness_interactive_report.py:L1-L336] |

| `src/digitalmodel/structural/analysis/wall_thickness_lookup.py` | [src/digitalmodel/structural/analysis/wall_thickness_lookup.py:L1-L146] |

| `src/digitalmodel/structural/analysis/wall_thickness_mt_report.py` | [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L1-L1522] |

| `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L1-L575] |

| `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L1-L770] |

Documentation found by exact package/module-name searches and inspected for the relevant content:

| Document | Evidence |
| --- | --- |

| `docs/domains/structural/analysis/README.md` | [docs/domains/structural/analysis/README.md:L1-L17] |

| `docs/domains/codes-register.md` | [docs/domains/codes-register.md:L30-L105] |

| `docs/domains/grade-table-migration-2026-06-28.md` | [docs/domains/grade-table-migration-2026-06-28.md:L1-L57] |

| `docs/domains/materials/grade-matrix-validation-2026-06-28.md` | [docs/domains/materials/grade-matrix-validation-2026-06-28.md:L1-L34] |

| `docs/migration/legacy-engineering-flow-catalog.md` | [docs/migration/legacy-engineering-flow-catalog.md:L24-L36] |

| `docs/domains/risers/scr-slwr-coverage-2026-10.md` | [docs/domains/risers/scr-slwr-coverage-2026-10.md:L184-L185] |

Tests listed and executed at BASE and AFTER: 428 package-focused tests plus 120 consumer/benchmark tests, 548 total across 32 files; these collected counts replace the inventory estimate.

| Test file | BASE passed tests | Evidence |
| --- | --- | --- |

| `tests/asset_integrity/test_code_provenance.py` | 11 | [tests/asset_integrity/test_code_provenance.py:L1-L155] |

| `tests/benchmarks/test_wall_thickness_benchmarks.py` | 1 | [tests/benchmarks/test_wall_thickness_benchmarks.py:L1-L48] |

| `tests/capabilities/test_wall_thickness_3d.py` | 2 | [tests/capabilities/test_wall_thickness_3d.py:L1-L73] |

| `tests/capabilities/test_wall_thickness_3d_page.py` | 6 | [tests/capabilities/test_wall_thickness_3d_page.py:L1-L195] |

| `tests/reporting/test_riser_pipe_comparison.py` | 12 | [tests/reporting/test_riser_pipe_comparison.py:L1-L253] |

| `tests/structural/analysis/wall_thickness_codes/test_api_rp_1111.py` | 14 | [tests/structural/analysis/wall_thickness_codes/test_api_rp_1111.py:L1-L157] |

| `tests/structural/analysis/wall_thickness_codes/test_asme_b31_4.py` | 7 | [tests/structural/analysis/wall_thickness_codes/test_asme_b31_4.py:L1-L197] |

| `tests/structural/analysis/wall_thickness_codes/test_dnv_st_f101.py` | 20 | [tests/structural/analysis/wall_thickness_codes/test_dnv_st_f101.py:L1-L258] |

| `tests/structural/analysis/wall_thickness_codes/test_wall_thickness_core.py` | 28 | [tests/structural/analysis/wall_thickness_codes/test_wall_thickness_core.py:L1-L204] |

| `tests/structural/structural_analysis/test_structural_analysis_unit.py` | 26 | [tests/structural/structural_analysis/test_structural_analysis_unit.py:L1-L351] |

| `tests/test_parametric_coordinator.py` | 46 | [tests/test_parametric_coordinator.py:L1-L415] |

| `tests/test_riser_case_screening.py` | 14 | [tests/test_riser_case_screening.py:L1-L255] |

| `tests/test_wall_thickness.py` | 58 | [tests/test_wall_thickness.py:L1-L745] |

| `tests/test_wall_thickness_api_rp_2rd.py` | 8 | [tests/test_wall_thickness_api_rp_2rd.py:L1-L127] |

| `tests/test_wall_thickness_api_std_2rd.py` | 9 | [tests/test_wall_thickness_api_std_2rd.py:L1-L210] |

| `tests/test_wall_thickness_codes/test_asme_b31_8.py` | 16 | [tests/test_wall_thickness_codes/test_asme_b31_8.py:L1-L223] |

| `tests/test_wall_thickness_codes/test_code_registry.py` | 6 | [tests/test_wall_thickness_codes/test_code_registry.py:L1-L40] |

| `tests/test_wall_thickness_codes/test_dnv_st_f201.py` | 14 | [tests/test_wall_thickness_codes/test_dnv_st_f201.py:L1-L184] |

| `tests/test_wall_thickness_codes/test_iso_13623.py` | 16 | [tests/test_wall_thickness_codes/test_iso_13623.py:L1-L218] |

| `tests/test_wall_thickness_codes/test_pd_8010_2.py` | 17 | [tests/test_wall_thickness_codes/test_pd_8010_2.py:L1-L227] |

| `tests/test_wall_thickness_collapse_solver.py` | 6 | [tests/test_wall_thickness_collapse_solver.py:L1-L104] |

| `tests/test_wall_thickness_compare_workflow.py` | 3 | [tests/test_wall_thickness_compare_workflow.py:L1-L80] |

| `tests/test_wall_thickness_comparison.py` | 17 | [tests/test_wall_thickness_comparison.py:L1-L247] |

| `tests/test_wall_thickness_dnv_editions.py` | 39 | [tests/test_wall_thickness_dnv_editions.py:L1-L392] |

| `tests/test_wall_thickness_editions.py` | 19 | [tests/test_wall_thickness_editions.py:L1-L269] |

| `tests/test_wall_thickness_interactive_report.py` | 16 | [tests/test_wall_thickness_interactive_report.py:L1-L199] |

| `tests/test_wall_thickness_lookup.py` | 18 | [tests/test_wall_thickness_lookup.py:L1-L189] |

| `tests/test_wall_thickness_mt_report.py` | 21 | [tests/test_wall_thickness_mt_report.py:L1-L437] |

| `tests/test_wall_thickness_parametric.py` | 23 | [tests/test_wall_thickness_parametric.py:L1-L291] |

| `tests/test_wall_thickness_phases.py` | 45 | [tests/test_wall_thickness_phases.py:L1-L534] |

| `tests/test_wall_thickness_quickcheck_example.py` | 8 | [tests/test_wall_thickness_quickcheck_example.py:L1-L186] |

| `tests/workflow_api/test_run_workflow_wall_thickness.py` | 2 | [tests/workflow_api/test_run_workflow_wall_thickness.py:L1-L52] |

Test commands, with destinations outside the repository, executed serially:

```text
PYTHONPATH=src uv run --no-sources --extra test python -m pytest <the 32 test files above> -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-base
PYTHONPATH=src uv run --no-sources --extra test python -m pytest <contracts-domain roots> -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-contract-base
```

The contracts-domain selection is the following set of existing roots: [tests/DOMAINS.md:L13-L13]

```text
tests/contracts/
tests/compat/
tests/cross_repo/
tests/docs/
tests/repo_structure/
tests/test_apistd2rd_migration.py
tests/test_module_reorganization.py
tests/test_restructure_compat.py
tests/test_units.py
tests/test_validation_utils.py
tests/test_no_corrupted_py_files.py
tests/test_run_contract.py
tests/validation/
```

Execution form: the configured full-path uv executable, `--no-sources --extra test`, with existing pytest addopts retained; no test workers. BASE is `1cacb74d17952505f453d7eda54f09dce1acc045`. AFTER uses the same selections and options on the uncommitted documentation tree, with separate temporary directories.

| Selection | BASE run / passed / failed / skipped / NOT_RUN | AFTER run / passed / failed / skipped / NOT_RUN |
| --- | --- | --- |
| Module and consumers | 548 / 548 / 0 / 0 / 0 | 548 / 548 / 0 / 0 / 0 |
| Contracts domain | 920 / 918 / 2 / 61 / 0 | 920 / 918 / 2 / 61 / 0 |

For the contracts selection, run means executed tests, excluding 60 skipped collected nodes and one collection-level skip; 980 nodes were collected. Both failed nodes are baseline: `tests/test_no_corrupted_py_files.py::test_all_src_py_files_parse` reports invalid escape sequences in unchanged Python files, and `tests/contracts/test_string_addressed_dependencies.py::test_the_three_known_defects_are_still_derived` cannot derive the unchanged spreadsheet-engine call site when source parsing rejects it. Both test definitions and the affected source were read from `origin/main`. [tests/test_no_corrupted_py_files.py:L14-L24] [tests/contracts/test_string_addressed_dependencies.py:L619-L636] [src/digitalmodel/asset_integrity/common/data.py:L213-L216]

The contracts skips are retained as shipped: removed compatibility paths (#949), incomplete migration paths, and an unavailable legacy module; none of the four section-4 cases skipped. [tests/test_apistd2rd_migration.py:L16-L25] [tests/test_restructure_compat.py:L55-L68] [tests/test_module_reorganization.py:L1-L30] [tests/compat/test_group_compat.py:L112-L123]

Open items (proposed responsible party: engineering calculations maintainers):

| Item | Coverage gap or defect and next check | Tracking |
| --- | --- | --- |
| O1 | Selected editions must control resistance factors and returned provenance; add unequal-edition-result assertions and reject unsupported editions | issue pending (1); [src/digitalmodel/structural/analysis/wall_thickness.py:L645-L671] [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L23-L55] [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py:L132-L136] |
| O2 | Report criterion/code dispatch and condition tension coordinates need parity checks against each strategy at common loads | issue pending (2); [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L548-L575] [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L1117-L1154] |
| O3 | UNVERIFIED claims in section 3: exact-edition compliance and Citation-sidecar completeness; independent pressure/combined-load benchmarks and publication metadata remain to be qualified; a worked formula is insufficient | #2166 and issue pending (3); [docs/domains/codes-register.md:L62-L105] [tests/test_wall_thickness_codes/test_dnv_st_f201.py:L78-L96] |
| O4 | Add missing map families before counting them; reconcile map rows and code pins, external gaps, helper classifications and withheld identifying locators; private provenance entry and documented codenames remain to be created where needed | issue pending (4); section 9 inventory and section 3 method locators |
| O5 | Replace ISO/PD nonphysical-root selection and add physical-interval regression cases | #1920; [src/digitalmodel/structural/analysis/wall_thickness_codes/iso_13623.py:L154-L176] [src/digitalmodel/structural/analysis/wall_thickness_codes/pd_8010_2.py:L153-L175] |
| O6 | Decide legacy material-view convergence before treating grade values as canonical | #1147; [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L36-L40] [docs/domains/grade-table-migration-2026-06-28.md:L40-L57] |
| O7 | Complete deep report/CLI validation and the full contextual documentation review beyond this main-path pack; create the shared lesson-log reference and confirm the proposed deputy responsibility | issue pending (3); [src/digitalmodel/structural/analysis/cli.py:L435-L492] [src/digitalmodel/structural/analysis/wall_thickness_mt_report.py:L78-L233] |

The draft does not supply a private-data sidecar ID or external benchmark result; section 4 identifies the available closed-form case and the regression evidence. No licensed standard text was retrieved for this draft.
