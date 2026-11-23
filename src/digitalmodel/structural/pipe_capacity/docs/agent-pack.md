# Pipe capacity knowledge pack

| Header | Value |
| --- | --- |
| Module | structural/pipe_capacity |
| Repository | digitalmodel |
| Path | src/digitalmodel/structural/pipe_capacity |
| Pack revision | 0 |
| Code revision | bb18c1a346b31b4ce975befa41fc84d9a5ff29e5 |
| Analysis family | Engineering calculations (EC); AQG r0, Python calculations |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`PipeCapacity.router` delegates to `PipeComponents.evaluate_pipe_capacity`, which constructs custom sizing, material and capacity objects and mutates the configuration; internal pressure, external pressure and collapse propagation are dispatched per design row. [src/digitalmodel/structural/pipe_capacity/pipe_capacity.py:L4-L17] [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L149-L156] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L280-L313]

The structural domain is registered as stable; this is a registry classification rather than demonstrated qualification of every legacy method. [docs/registry/module-routing.yaml:L337-L343]

## 2. Inputs and outputs

| Input | Unit | Range/default and route | Evidence |
| --- | --- | --- | --- |
| Outer_Pipe / Inner_Pipe geometry | inch in the durable workflow | OD, ID and wall thickness; one missing dimension is derived algebraically; no explicit physical-range validation | [examples/workflows/pipe-capacity/input.yml:L5-L15] [src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py:L20-L40] |
| Material strengths, modulus, density | psi, lb/inch3 in the durable workflow | material and grade keys; routed defaults need explicit acceptance before engineering use | [examples/workflows/pipe-capacity/input.yml:L5-L15] [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L44-L89] |
| Design pressure and code | psi; code string | scalar pressures or DNV fluid density and column mapping; missing DNV pressure becomes zero | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L18-L25] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L135-L154] |

| Output | Unit | Destination | Evidence |
| --- | --- | --- | --- |
| thickness / minimum_thickness | input length unit | configuration basename, pipe, condition, code and corrosion tag | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L444-L494] |
| Design_WT_Max_Pressure | input pressure unit | same result hierarchy | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L467-L480] |
| A, I, J and EI, EA, GJ | consistent powers of input length and stress | pipe section properties and equivalent pipe | [src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py:L42-L112] |

The routed result is scalar per pipe/condition/code and stores no coordinate origin or phase; DNV pressure differences are clipped to nonnegative internal-minus-external or external-minus-minimum-internal values. [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L135-L154] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L444-L494]

Standalone F201 accepts consistent units without internal conversion; its moment resistance is stress times length cubed, its combined criterion uses absolute bending moment and squared tension, and its output is dimensionless utilisation, limit and boolean passes. [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L37-L40] [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L82-L89] [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L113-L121] [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L195-L201]

Legacy INI numeric parsing, CSV exports and plot labels do not establish a common unit metadata contract; unit qualification is issue pending (1). [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/ConfigurationManager.py:L34-L52] [src/digitalmodel/structural/pipe_capacity/custom/saveData.py:L9-L25] [src/digitalmodel/structural/pipe_capacity/custom/plotDefault.py:L13-L16]

## 3. Method and governing standards

The following records describe code attribution; “clause text not checked against the licensed copy” applies to every standards row. [docs/domains/pipecapacity/WALL_THICKNESS_IMPLEMENTED_CALCS.md:L33-L36]

| Standard and edition as named | Method / implementation | Clause evidence and qualification |
| --- | --- | --- |
| API STD 2RD, 2013 | `PipeCapacity.evaluate_burst_pressure_API_STD_2RD`, inverse burst thickness; elastic/plastic collapse and propagation | Section 5 in workflow selector; collapse thickness is a placeholder, excluded as a hand reference. [examples/workflows/pipe-capacity/input.yml:L60-L70] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L550-L570] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L678-L707] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L841-L945] |
| API RP 1111, 2009 | `PipeCapacity.evaluate_burst_pressure_API_RP_1111`, inverse burst thickness and propagation | Code names edition; clause not supplied at the dispatch. [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L517-L523] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L572-L676] |
| DNV-OS-F101 | `DNVWallThickness` and independent legacy `DNVOSF101Pipe` | Legacy collapse comment names Section 13 D700, Local Buckling - Collapse Check. [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L38-L273] [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L215-L246] |
| DNV-OS-F201, 2010 | standalone `yield_strength`, `tensile_strength`, plastic resistances, `bursting_resistance`, `flow_stress_coefficient`, both combined-loading criteria, `factored_load_effect` | Section 5, equations 5.25–5.26 and combined-loading clause D are docstring claims. [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L1-L45] |
| ASME B31.8; edition unspecified | modified Barlow, `OtherMethodsTobeIncorporated.ASMEB318InternalPressure`, standalone `ASMEMethods.ASMEB318InternalPressure` | Clause not supplied; edition remains unverified. [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L709-L814] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1062-L1097] [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L124-L157] |
| API RP 16Q, second edition, 2017 | `API_RP_16Q` delegates to `VonMises_Pipe` stress and interpolation | Clause not supplied; mixed force/pressure treatment needs qualification. [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1182-L1200] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1266-L1418] |


Additional independently calculating definitions are retained below rather than treated as wrappers. [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/APISTD2RDMethods.py:L12-L272] [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L11-L236]

| Additional implementation | Method and relationship | Evidence |
| --- | --- | --- |
| `common.PipeCapacity.PipeCapacity`, `OtherMethodsTobeIncorporated`, `API_TR_5C3`, `CFR_30_Part_250`, `VonMises_Pipe` | Parallel formulas and a different result root from custom; the routed path uses custom | [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L10-L47] [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L178-L228] [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L916-L1192] [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L3-L7] |
| `custom.API_STD_2RD.APISTD2RDMethods` | `APISTD2RDBurst`, `APISTD2RDCollapse`, `APISTD2RDMethod1`, `APISTD2RDMethod1Utilization` and `sectionProperties` calculate burst, collapse, moment/tension interaction and geometry | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/APISTD2RDMethods.py:L12-L272] |
| `custom.API_STD_2RD.BurstPressure.APISTD2RDMethods` | Independent `APISTD2RDBurst`; `variableWTBurst` prepares rows and delegates burst | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/APISTD2RDMethods.py:L9-L76] |
| `custom.ASMEMethods` | B31.4 and B31.8 internal-pressure, longitudinal-stress and equivalent-stress functions, with loads fixed in several branches | [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L11-L236] |
| `custom.DNV_OS_F101.RigidPipe.DNVOSF101Pipe` | `DNVThickness`, `temperatureDeratingValues`, `updateStrengthValues`, containment, collapse, propagation and both combined-loading calculations; pressure containment overwrites its method name with a numeric attribute | [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L75-L213] [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L215-L587] |
| `PipeSizing.PipeSizing`, `common.PipeSizing.PipeSizing`, `custom.PipeSizing.PipeSizing` | Independent annular section, stiffness, mass and equivalent submerged-weight calculations | [src/digitalmodel/structural/pipe_capacity/PipeSizing.py:L21-L194] [src/digitalmodel/structural/pipe_capacity/common/PipeSizing.py:L20-L290] [src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py:L20-L290] |
| `common.MaterialProperties`, `custom.MaterialProperties`, both standalone `temperatureDerating` modules and the burst-pressure temperature module | Derating and mass calculations; the routed material selection assigns unity for recognized codes | [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L51-L137] [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L51-L137] [src/digitalmodel/structural/pipe_capacity/common/temperatureDerating.py:L6-L33] [src/digitalmodel/structural/pipe_capacity/custom/temperatureDerating.py:L6-L33] [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/temperatureDerating.py:L6-L25] |

The module tests use an F101-2013 selector while infrastructure tests default to F101-2010; both construct `DNVWallThickness`, whose built-in factors have no verified per-edition basis here, so equivalence is tracked in issue pending (4). [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L194-L205] [tests/infrastructure/unit/test_pipe_capacity_dnv.py:L4-L10] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L46-L90]

UNVERIFIED: authoritative editions, clause titles and emitted `Citation` sidecars for these legacy formulas are not established; issue pending (4).

## 4. Validated worked examples

No independently qualified worked example is established by the following regression assertions; the workflow pins output values, the DNV inverse reuses the implementation's forward formula, and the F201 tests describe a comparison spreadsheet without an accessible published revision. [tests/workflows/test_durable_workflows.py:L98-L108] [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L370-L378] [tests/structural/pipe_capacity/test_dnv_os_f201_lrfd_wsd.py:L1-L13]

| Case and classification | Source/input | Expected value and tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| E01 durable internal-pressure workflow; regression | Repository YAML example; no independent benchmark revision | Pressure 3149.2 psi, required wall rounds to 0.359 inch, corroded wall 0.375 inch; execute the unrounded `pytest.approx` literals with its default relative tolerance 1.0e-6 (dimensionless) | `tests/workflows/test_durable_workflows.py::test_workflow_registry[pipe-capacity]` | PASS (pinned-code run); [docs/registry/workflows.yaml:L20-L27] [tests/workflows/test_durable_workflows.py:L98-L108] |
| E02 DNV burst inverse; regression | `_make_dnv_cfg` embedded in test; scalar pressures in psi | Forward allowable at inverse thickness equals 7000.0 psi within 1.0e-6 relative error (dimensionless); tolerance is a test criterion, not a code limit | `tests/structural/pipe_capacity/test_pipe_capacity_custom.py::TestDNVWallThicknessBurst::test_get_burst_minimum_thickness_positive_design_pressure` | PASS (pinned-code run); [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L370-L378] |
| E03 DNV collapse inverse; regression | `_make_dnv_cfg` embedded in test | Recomputed collapse pressure at inverse thickness is at least 99.0 % of the factored 5000.0 psi external-demand target; lower-bound check only | `tests/structural/pipe_capacity/test_pipe_capacity_custom.py::TestDNVWallThicknessCollapse::test_collapse_minimum_thickness_satisfies_target` | PASS (pinned-code run); [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L441-L451] |

UNVERIFIED: independent published reference provenance and tolerance qualification remain open; issue pending (7).


### Offline execution record

Code pin for all calculation sources: `bb18c1a346b31b4ce975befa41fc84d9a5ff29e5`; documentation/registry edits do not change the tested calculation sources. Input YAML SHA-256: `ac076edc50cefe6321b36fe6f7fff27e9cdefe5d81a6618382faba4663d98cf4`. [examples/workflows/pipe-capacity/input.yml:L1-L15]

| Command scope | Run | Passed | Failed | Skipped | NOT_RUN | Result / limitation |
| --- | --- | --- | --- | --- | --- | --- |
| Five pipe-capacity test files, original pre-edit launch (aborted) | 0 | 0 | 0 | 0 | — | Process exited 141 before pytest output; no node outcomes were produced, rather than 232 NOT_RUN |
| Five pipe-capacity test files, fix-up BASE at the code pin | 232 | 232 | 0 | 0 | 0 | PASS; src and tests match bb18c1a3 byte-for-byte except this pack; pre-edit fix-up run |
| Same five files, historical after documentation edits | 232 | 232 | 0 | 0 | 0 | PASS; package tests: 103 common, 108 custom, 9 F201; adjacent infrastructure tests: 7 code and 5 DNV |
| Five pipe-capacity test files, matched fix-up AFTER | 232 | 232 | 0 | 0 | 0 | PASS; exact JUnit node comparison with fix-up BASE: 0 changed outcomes; PC-14 PASS for the 232-node scope |
| E01, first isolated pinned-code execution | 1 | 1 | 0 | 0 | 0 | PASS; performed after documentation editing began |
| E01, second isolated pinned-code execution | 1 | 1 | 0 | 0 | 0 | PASS; identical node outcome |
| Contracts domain, 13 registered test paths | 1115 | 1054 | 0 | 61 | 0 | Reported pytest outcomes; pre-existing migration/compatibility skips remain unchanged |

Reproduction uses `PYTHONPATH=src uv run --no-sources --extra test python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`, with the full configured executable path and external scratch directory used for actual execution; no addopts override, test deselection or solver call is used. Fix-up execution uses `PYTHONPATH=src:<dependency-src>` and `uv run --no-project` with pytest, pytest-cov, pytest-timeout, pytest-mock, pytest-randomly, numpy, pandas, scipy, pint, plotly and jsonschema; `--randomly-seed=0` and `--junitxml=<scratch>` are added, preserving repository addopts and warning policy. An initial dependency-incomplete attempt produced 3 collection errors (missing plotly); the matched runs use the completed environment. The five paths are the three `tests/structural/pipe_capacity/test_*.py` files and the two infrastructure pipe-capacity test files in the coverage table. The workflow node is run from a temporary copy containing its test, registry and example files, so generated YAML/logs remain outside the checkout. [docs/registry/workflows.yaml:L20-L27] [tests/workflows/test_durable_workflows.py:L52-L108]

The fix-up BASE run supplies the missing 232-node baseline at the code pin; no source or test difference from that pin is present apart from this pack. PC-14 compares these 232 nodes with the matched fix-up AFTER run. E01 retains its two historical pinned-code passes and is not repeated: its shared test module imports solver integration routes, outside this fix-up execution scope. No independent engineering validation is inferred.


The contracts domain completed with 1054 passed and 61 skipped; the reported skips cover an incomplete API STD 2RD migration, retired module-layout assertions and an unavailable legacy catenary alias. These skip paths already exist at the code pin; no skip was introduced. [tests/test_apistd2rd_migration.py:L18-L26] [tests/test_restructure_compat.py:L16-L21] [tests/test_module_reorganization.py:L11-L16] [tests/compat/test_group_compat.py:L109-L119]

## 5. QA gates

The gate allocation follows AQG r0, section 10, Python calculations, with G5 from section 9; the checks below distinguish code regression from engineering qualification. (AQG r0, “Family mapping” and “G5 QA appendix”.)

| Gate | Applicability and execution route | Module-specific checks |
| --- | --- | --- |
| G0 | Applicable: inspect configuration dimensions, units, code provenance and fallback behavior | PC-01–PC-07, PC-16 and PC-17; [examples/workflows/pipe-capacity/input.yml:L5-L15] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L11-L154] |
| G1 | Applicable: Python imports and offline pytest for common/custom/F201 and the engine's pipe-capacity workflow; no solver program load is part of this calculation route | PC-11, PC-13, PC-14; [tests/structural/pipe_capacity/test_pipe_capacity_common.py:L41-L49] [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L23-L32] [docs/registry/workflows.yaml:L20-L27] |
| G2 | Applicable: algebraic checks of pressure/thickness, collapse-target residual and propagation inverse; these are consistency checks pending independent source qualification | PC-08–PC-10 and PC-12; [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L370-L378] [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L441-L451] [tests/structural/pipe_capacity/test_pipe_capacity_common.py:L526-L541] |
| G3 | NOT_APPLICABLE: no time-domain dynamics route in the pressure routing | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L280-L313] |
| G4 | NOT_APPLICABLE: no diffraction mesh or hydrodynamic-coefficient output in the result writer | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L444-L494] |
| G5 | Applicable: check completeness of the routed E01 package result before reporting; a generated QA appendix is not established | PC-15; [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L444-L494] |

Criteria below are regression criteria unless explicitly described as a documentary completeness check; a passing default-preservation test does not qualify that default for a design. [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L39-L91] [tests/structural/pipe_capacity/test_pipe_capacity_common.py:L843-L850]

| Check | Gate | Executable check / output | Numeric pass criterion and unit | Evidence |
| --- | --- | --- | --- | --- |
| PC-01 | G0 | Parse workflow YAML; require explicit scalar pressures/fluids, grade strengths, modulus/density, derating and code factors; match declared units; require positive OD/wall and remaining wall; recompute SHA-256 input fingerprint | 0 missing fields; 0 inconsistent geometry cases; 0 fingerprint mismatches (count) | [examples/workflows/pipe-capacity/input.yml:L5-L15] [examples/workflows/pipe-capacity/input.yml:L29-L43] |
| PC-02 | G0 | Existing `TestExtractYearFromCode` nodes expose year-selection behavior, including malformed literal input | 0 unexpected test outcomes (count); this preserves a documented defect | [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L39-L67] |
| PC-03 | G0 | Existing `TestResolvePressureValue` nodes expose missing pressure and density/column substitution | 0 unexpected outcomes (count), absent mapping returns 0.0 psi | [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L71-L93] |
| PC-04 | G0 | Existing `TestDNVWallThicknessDesignFactors` nodes compare defaults, exact and generic override selection | 0 differences from asserted mappings (count); accepted design factors still require a source | [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L194-L240] |
| PC-05 | G0 | Existing `TestDNVWallThicknessTemperatureDerating` tests configured and missing-key factor behavior | 0 differences from the asserted dimensionless factors (count) | [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L243-L255] |
| PC-06 | G0 | Existing `TestDNVWallThicknessMaterialModulus` and `TestDNVWallThicknessCollapse` invalid-geometry/modulus nodes | 0 unexpected outcomes (count); the raw method returns None for the listed invalid cases | [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L277-L293] [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L395-L409] |
| PC-07 | G0 | Existing `TestDNVWallThicknessPressures` compares numeric, fluid-column, minimum internal and differential values | 0 unexpected outcomes (count); nonpositive internal differential clamps to 0.0 psi | [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L296-L338] |
| PC-08 | G2 | Existing `TestDNVWallThicknessBurst` forward/inverse and zero-thickness tests | Relative pressure residual ≤ 1.0e-6 (dimensionless); nonpositive input sentinel exactly 0.0 psi or 0.000 inch, per node | [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L341-L383] |
| PC-09 | G2 | E03 and zero-external-demand node in `TestDNVWallThicknessCollapse` | Recomputed pressure ≥ 99.0 % of factored target; no external demand gives 0.000 inch | [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L435-L451] |
| PC-10 | G2 | Existing `TestDNVWallThicknessPropagation` forward/inverse and zero-strength/demand nodes | Relative inverse residual ≤ 1.0e-6 (dimensionless); zero-strength pressure exactly 0.0 psi; zero-demand thickness exactly 0.000 inch | [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L454-L493] |
| PC-11 | G1 | All tests in `tests/structural/pipe_capacity/test_dnv_os_f201_lrfd_wsd.py`; comparison-spreadsheet regressions | 0 unexpected test outcomes (count); asserted numerical comparisons use 0.1 % relative tolerance | [tests/structural/pipe_capacity/test_dnv_os_f201_lrfd_wsd.py:L33-L35] [tests/structural/pipe_capacity/test_dnv_os_f201_lrfd_wsd.py:L48-L174] |
| PC-12 | G2 | `tests/structural/pipe_capacity/test_pipe_capacity_common.py::TestASMEB31BurstPressure::test_thin_wall_formula_d_over_t_above_transition`; separate arithmetic of test's pressure formula | Absolute pressure error < 1.0e-2 psi against the closed-form expression in the test | [tests/structural/pipe_capacity/test_pipe_capacity_common.py:L526-L541] |
| PC-13 | G1 | `tests/structural/pipe_capacity/test_pipe_capacity_common.py::TestAPISTD2RDCollapseMinThickness::test_returns_placeholder` exposes the incomplete inverse | 0 unexpected test outcomes (count); the asserted placeholder is never accepted as a calculated wall thickness | [tests/structural/pipe_capacity/test_pipe_capacity_common.py:L843-L850] |
| PC-14 | G1 | Run all three package test files and both infrastructure pipe-capacity files before/after documentation edits (232 nodes); retain E01 historical regression separately | 0 newly failed nodes; 0 changed node outcomes (count) | [tests/infrastructure/unit/test_pipe_capacity_codes.py:L1-L43] [tests/infrastructure/unit/test_pipe_capacity_dnv.py:L1-L43] [docs/registry/workflows.yaml:L20-L27] |
| PC-15 | G5 | Load the E01 input YAML and call `pipe_capacity.PipeCapacity.router`; inspect the `pipe_capacity/Outer_Pipe/internal_pressure/API STD 2RD-2013 Section 5` result written by `custom.PipeCapacity.update_pressure_results` for `thickness`, `minimum_thickness` and `Design_WT_Max_Pressure`, each under `Zero Corrosion Allowance` and `With Corrosion Allowance` | 0 missing keys (count) across 6 required result entries; completeness only, not independent numerical qualification | [examples/workflows/pipe-capacity/input.yml:L1-L15] [src/digitalmodel/structural/pipe_capacity/pipe_capacity.py:L12-L17] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L444-L494] |
| PC-16 | G0 | Resolve every bracketed repository citation at the code pin, with map citations also checked against the map pin | 0 missing files; 0 out-of-range citations; 0 support mismatches in ten manual samples (count) | [docs/registry/implementation-map.yaml:L1-L4] |
| PC-17 | G0 | Inspect the structural routing entry for this pack and the EC family; documentary completeness only | 0 missing or mismatched fields (count) | `docs/registry/module-routing.yaml`, structural submodules, `pipe_capacity` entry |

## 6. Known failure modes and pitfalls

sweep: 156 found

The count enumerates fallback, conversion, unit-assumption and empty-output sites, including intentional branches; it is not a count of independent defects. Each row below is tied to its source location, detection coverage and disposition. [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L18-L25] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L189-L249]

Sweep commands (package Python files only; output and classification are retained with the check evidence):

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|\.get\(|\b(int|float)\(' src/digitalmodel/structural/pipe_capacity --glob '*.py'
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|\.get\(|\b(int|float)\(|interp|[Uu]nit|[Dd]efault|[Dd]ensity|0\.0?5|0\.0254|\* 12' src/digitalmodel/structural/pipe_capacity --glob '*.py'
```

| Site | Symptom / cause | Detection check | Tracking |
| --- | --- | --- | --- |
| S001 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L11-L15] | Year regex returns no year for ordinary edition strings; literal backslash text can raise ValueError | PC-02 | issue pending (2) |
| S002 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L19-L20] | Absent scalar pressure becomes zero psi | PC-03 | issue pending (1) |
| S003 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L22-L22] | Missing fluid_density becomes zero; float conversion has no locale normalization | PC-03 | issue pending (1) |
| S004 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L23-L24] | Missing fluid_column becomes zero; column is multiplied by feet-to-inch conversion | PC-03 | issue pending (1) |
| S005 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L25-L25] | Scalar float conversion has no thousands/decimal-separator normalization | no detection check exists | issue pending (1) |
| S006 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L46-L67] | Hard-coded resistance/material/ovality defaults are selected without edition verification | PC-04 | issue pending (4) |
| S007 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L69-L69] | Absent DesignFactors becomes an empty mapping and selects built-in factors | PC-04 | issue pending (1) |
| S008 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L94-L99] | Nested section entries are skipped after merging; malformed non-dictionary entries are not rejected | no detection check exists | issue pending (1) |
| S009 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L101-L103] | Unknown scalar override keys are ignored; first matching code ends the search | no detection check exists | issue pending (1) |
| S010 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L107-L115] | Missing derating key is swallowed and replaced with unity; float conversion is locale-unaware | PC-05 | issue pending (1) |
| S011 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L120-L120] | SMYS float conversion assumes unseparated numeric text | no detection check exists | issue pending (1) |
| S012 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L121-L121] | SMUS float conversion assumes unseparated numeric text | no detection check exists | issue pending (1) |
| S013 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L128-L128] | Missing elastic modulus becomes zero; float conversion is locale-unaware | PC-06 | issue pending (1) |
| S014 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L129-L133] | Poisson aliases fall back to a fixed value; float conversion is locale-unaware | PC-06 | issue pending (1) |
| S015 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L139-L143] | Absent minimum internal pressure or absent pipe key becomes zero psi | PC-07 | issue pending (1) |
| S016 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L145-L146] | OD float conversion assumes unseparated numeric text | no detection check exists | issue pending (1) |
| S017 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L148-L150] | Negative internal pressure differential is clamped to zero psi | PC-07 | issue pending (1) |
| S018 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L152-L154] | Negative external differential is clamped to zero psi | PC-07 | issue pending (1) |
| S019 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L157-L158] | Nonpositive containment thickness returns zero psi | PC-08 | issue pending (1) |
| S020 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L177-L178] | Nonpositive burst differential returns zero thickness | PC-08 | issue pending (1) |
| S021 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L185-L186] | Nonpositive factored strength returns zero thickness | no detection check exists | issue pending (1) |
| S022 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L190-L191] | Nonpositive collapse thickness returns None | PC-06 | issue pending (1) |
| S023 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L193-L194] | Thickness at least OD returns None | PC-06 | issue pending (1) |
| S024 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L197-L198] | Nonpositive elastic modulus or strength returns None | PC-06 | issue pending (1) |
| S025 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L211-L212] | Cubic intermediate outside expected branch returns None | no detection check exists | issue pending (3) |
| S026 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L213-L214] | Inverse-cosine argument is clamped instead of exposing root-conditioning error | no detection check exists | issue pending (3) |
| S027 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L224-L225] | Invalid raw collapse becomes zero psi at public output | no detection check exists | issue pending (1) |
| S028 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L233-L234] | Nonpositive collapse target returns zero thickness | PC-09 | issue pending (1) |
| S029 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L237-L249] | Fixed bisection bracket and iteration count return upper bound without reachability check; invalid roots continue silently | PC-09 | issue pending (3) |
| S030 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L258-L259] | Nonpositive propagation strength/fabrication/OD returns zero psi | PC-10 | issue pending (1) |
| S031 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L271-L272] | Nonpositive propagation target or strength/fabrication returns zero thickness | PC-10 | issue pending (1) |
| S032 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L989-L989] | longitudinal moment stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S033 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L990-L990] | longitudinal axial stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S034 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1024-L1024] | equivalent moment stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S035 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1025-L1025] | equivalent axial stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S036 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1026-L1026] | equivalent torsion stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S037 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1103-L1103] | B31.8 longitudinal moment stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S038 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1104-L1104] | B31.8 longitudinal axial stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S039 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1138-L1138] | B31.8 equivalent moment stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S040 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1139-L1139] | B31.8 equivalent axial stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S041 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1140-L1140] | B31.8 equivalent torsion stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S042 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1253-L1258] | Collapse interpolation clamps out-of-range demand; float conversion is of a computed result, not external text | no detection check exists | issue pending (3) |
| S043 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1283-L1290] | VonMises mixes a moment conversion and force conversion with pressure times diameter; sign/unit qualification is incomplete | no detection check exists | issue pending (3) |
| S044 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1364-L1371] | Thickness interpolation clamps stress demand outside sampled values; float conversion is of a computed result | no detection check exists | issue pending (3) |
| S045 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1387-L1411] | Burst pressure sweep uses a fixed fallback range, zero lower bound and interpolation endpoint clamping | no detection check exists | issue pending (3) |
| S046 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L702-L702] | longitudinal moment stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S047 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L703-L703] | longitudinal axial stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S048 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L737-L737] | equivalent moment stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S049 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L738-L738] | equivalent axial stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S050 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L739-L739] | equivalent torsion stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S051 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L816-L816] | B31.8 longitudinal moment stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S052 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L817-L817] | B31.8 longitudinal axial stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S053 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L851-L851] | B31.8 equivalent moment stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S054 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L852-L852] | B31.8 equivalent axial stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S055 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L853-L853] | B31.8 equivalent torsion stress is fixed at zero, so supplied loads do not enter that term | no detection check exists | issue pending (5) |
| S056 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L966-L971] | Collapse interpolation clamps out-of-range demand; float conversion is of a computed result, not external text | no detection check exists | issue pending (3) |
| S057 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L996-L1003] | VonMises mixes a moment conversion and force conversion with pressure times diameter; sign/unit qualification is incomplete | no detection check exists | issue pending (3) |
| S058 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L1077-L1084] | Thickness interpolation clamps stress demand outside sampled values; float conversion is of a computed result | no detection check exists | issue pending (3) |
| S059 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L1100-L1124] | Burst pressure sweep uses a fixed fallback range, zero lower bound and interpolation endpoint clamping | no detection check exists | issue pending (3) |
| S060 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L47-L47] | longitudinal moment stress is fixed at zero | no detection check exists | issue pending (5) |
| S061 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L48-L48] | longitudinal axial stress is fixed at zero | no detection check exists | issue pending (5) |
| S062 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L83-L83] | equivalent moment stress is fixed at zero | no detection check exists | issue pending (5) |
| S063 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L84-L84] | equivalent axial stress is fixed at zero | no detection check exists | issue pending (5) |
| S064 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L85-L85] | equivalent torsion stress is fixed at zero | no detection check exists | issue pending (5) |
| S065 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L164-L164] | B31.8 longitudinal moment stress is fixed at zero | no detection check exists | issue pending (5) |
| S066 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L165-L165] | B31.8 longitudinal axial stress is fixed at zero | no detection check exists | issue pending (5) |
| S067 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L200-L200] | B31.8 equivalent moment stress is fixed at zero | no detection check exists | issue pending (5) |
| S068 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L201-L201] | B31.8 equivalent axial stress is fixed at zero | no detection check exists | issue pending (5) |
| S069 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L202-L202] | B31.8 equivalent torsion stress is fixed at zero | no detection check exists | issue pending (5) |
| S070 [src/digitalmodel/structural/pipe_capacity/PipeSizing.py:L64-L68] | Bare except swallows any G lookup error and derives shear modulus | no detection check exists | issue pending (1) |
| S071 [src/digitalmodel/structural/pipe_capacity/common/PipeSizing.py:L74-L79] | Bare except swallows any G lookup error and derives shear modulus | no detection check exists | issue pending (1) |
| S072 [src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py:L74-L79] | Bare except swallows any G lookup error and derives shear modulus | no detection check exists | issue pending (1) |
| S073 [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L51-L74] | Other-temperature branch assumes temperature scale and MPa-to-psi strength reduction | no detection check exists | issue pending (4) |
| S074 [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L75-L80] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S075 [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L83-L85] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S076 [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L87-L89] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S077 [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L91-L93] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S078 [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L95-L97] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S079 [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L99-L101] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S080 [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L115-L133] | Mass and buoyancy assume inch geometry, density per cubic inch and conversion to length per foot | no detection check exists | issue pending (1) |
| S081 [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L51-L74] | Other-temperature branch assumes temperature scale and MPa-to-psi strength reduction | no detection check exists | issue pending (4) |
| S082 [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L75-L80] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S083 [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L83-L85] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S084 [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L87-L89] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S085 [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L91-L93] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S086 [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L95-L97] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S087 [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L99-L101] | Recognized code receives unity derating without reading its temperature | no detection check exists | issue pending (1) |
| S088 [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L115-L133] | Mass and buoyancy assume inch geometry, density per cubic inch and conversion to length per foot | no detection check exists | issue pending (1) |
| S089 [src/digitalmodel/structural/pipe_capacity/common/temperatureDerating.py:L6-L26] | Other-temperature branch assumes temperature scale and MPa-to-psi strength reduction | no detection check exists | issue pending (4) |
| S090 [src/digitalmodel/structural/pipe_capacity/common/temperatureDerating.py:L27-L29] | B31.4 temperature derating defaults to unity | no detection check exists | issue pending (1) |
| S091 [src/digitalmodel/structural/pipe_capacity/common/temperatureDerating.py:L31-L33] | B31.8 temperature derating defaults to unity | no detection check exists | issue pending (1) |
| S092 [src/digitalmodel/structural/pipe_capacity/custom/temperatureDerating.py:L6-L26] | Other-temperature branch assumes temperature scale and MPa-to-psi strength reduction | no detection check exists | issue pending (4) |
| S093 [src/digitalmodel/structural/pipe_capacity/custom/temperatureDerating.py:L27-L29] | B31.4 temperature derating defaults to unity | no detection check exists | issue pending (1) |
| S094 [src/digitalmodel/structural/pipe_capacity/custom/temperatureDerating.py:L31-L33] | B31.8 temperature derating defaults to unity | no detection check exists | issue pending (1) |
| S095 [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L50-L59] | Missing outer material is synthesized from fixed steel properties | no detection check exists | issue pending (1) |
| S096 [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L70-L78] | Missing outer grade is synthesized from fixed strengths | no detection check exists | issue pending (1) |
| S097 [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L85-L89] | Missing outer coupling mass ratio receives a fixed default | no detection check exists | issue pending (1) |
| S098 [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L100-L109] | Missing inner material is synthesized from fixed steel properties | no detection check exists | issue pending (1) |
| S099 [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L115-L123] | Missing inner grade is synthesized from fixed strengths | no detection check exists | issue pending (1) |
| S100 [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L130-L134] | Missing inner coupling mass ratio receives a fixed default | no detection check exists | issue pending (1) |
| S101 [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L140-L147] | Missing API riser factors receive built-in defaults with a scalar shape unlike the capacity lookup | no detection check exists | issue pending (1) |
| S102 [src/digitalmodel/structural/pipe_capacity/common/PipeSizing.py:L203-L281] | Legacy equivalentPipe assumes inch-to-metre area conversion and density/weight conventions | no detection check exists | issue pending (1) |
| S103 [src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py:L203-L281] | Legacy equivalentPipe assumes inch-to-metre area conversion and density/weight conventions | no detection check exists | issue pending (1) |
| S104 [src/digitalmodel/structural/pipe_capacity/PipeSizing.py:L157-L182] | Legacy equivalentPipe assumes inch-to-metre area conversion and density/weight conventions | no detection check exists | issue pending (1) |
| S105 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/ConfigurationManager.py:L20-L25] | Integer recognizer catches ValueError and silently classifies non-integer text | no detection check exists | issue pending (5) |
| S106 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/ConfigurationManager.py:L27-L32] | Float recognizer catches ValueError and silently classifies non-float text | no detection check exists | issue pending (5) |
| S107 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/ConfigurationManager.py:L45-L48] | Configuration conversion leaves locale-separated numbers as strings after recognizer fallback | no detection check exists | issue pending (5) |
| S108 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/ConfigurationManager.py:L13-L18] | ConfigParser.read can return no files without raising; missing configuration becomes empty output | no detection check exists | issue pending (5) |
| S109 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L75-L82] | Missing ovality inputs use configured default; lower ovality is clamped | no detection check exists | issue pending (5) |
| S110 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L96-L98] | Below-threshold configured temperature sets strength reduction to zero | no detection check exists | issue pending (5) |
| S111 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L109-L111] | Below-threshold explicit temperature sets strength reduction to zero | no detection check exists | issue pending (5) |
| S112 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L311-L338] | Outside propagation range sets None and returns a text flag rather than a boolean failure | no detection check exists | issue pending (5) |
| S113 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L163-L164] | External-pressure flow-stress branch sets qh to zero by design | no detection check exists | issue pending (4) |
| S114 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L170-L171] | Large-diameter/thickness branch sets beta to zero by design | no detection check exists | issue pending (4) |
| S115 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L183-L185] | Combined internal check defaults resistance multipliers to unity and LRFD limit when usage_factor is absent | PC-11 | issue pending (4) |
| S116 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L212-L214] | Combined external check defaults resistance multipliers to unity and LRFD limit when usage_factor is absent | PC-11 | issue pending (4) |
| S117 [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/ymlInput.py:L11-L21] | Bare except swallows update-file parse/read error and continues with defaults | no detection check exists | issue pending (5) |
| S118 [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/APISTD2RD.py:L27-L37] | Bare except swallows argument lookup errors and selects default input | no detection check exists | issue pending (5) |
| S119 [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/Summary.py:L33-L43] | Bare except swallows argument lookup errors and selects default input | no detection check exists | issue pending (5) |
| S120 [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/APISTD2RDMethods.py:L28-L76] | Neither diameter branch populated can yield an empty table; chained assignments can leave result columns empty | no detection check exists | issue pending (5) |
| S121 [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/APISTD2RDMethods.py:L28-L76] | Neither diameter branch populated can yield an empty table; chained assignments can leave result columns empty | no detection check exists | issue pending (5) |
| S122 [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/fileList.py:L7-L15] | No glob matches returns an empty file list without an error | no detection check exists | issue pending (5) |
| S123 [src/digitalmodel/structural/pipe_capacity/custom/ASMEB31/fileList.py:L7-L15] | No glob matches returns an empty file list without an error | no detection check exists | issue pending (5) |
| S124 [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/extractData.py:L7-L28] | Empty input file list returns an empty result table without an error | no detection check exists | issue pending (5) |
| S125 [src/digitalmodel/structural/pipe_capacity/custom/ASMEB31/extractData.py:L7-L35] | Empty input file list returns an empty result table without an error | no detection check exists | issue pending (5) |
| S126 [src/digitalmodel/structural/pipe_capacity/custom/ASMEB31/extractData.py:L38-L64] | Empty input file list returns empty plot table without an error | no detection check exists | issue pending (5) |
| S127 [src/digitalmodel/structural/pipe_capacity/custom/saveData.py:L9-L25] | Export can persist an empty mapping/table without an emptiness or qualification check | no detection check exists | issue pending (5) |
| S128 [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/results/saveData.py:L6-L10] | Export can persist an empty mapping/table without an emptiness or qualification check | no detection check exists | issue pending (5) |
| S129 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L698-L731] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S130 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L733-L773] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S131 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L812-L845] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S132 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L847-L887] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S133 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L581-L593] | Collapse minimum thickness returns a fixed placeholder rather than calculating a result | PC-13 | issue pending (3) |
| S134 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L985-L1018] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S135 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1020-L1060] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S136 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1099-L1132] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S137 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1134-L1174] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S138 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L861-L873] | Collapse minimum thickness returns a fixed placeholder rather than calculating a result | PC-13 | issue pending (3) |
| S139 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L44-L78] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S140 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L81-L121] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S141 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L160-L195] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S142 [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L198-L236] | Unrestrained condition falls through with implicit None | no detection check exists | issue pending (5) |
| S143 [src/digitalmodel/structural/pipe_capacity/common/temperatureDerating.py:L6-L33] | Unrecognized code falls through with implicit None | no detection check exists | issue pending (1) |
| S144 [src/digitalmodel/structural/pipe_capacity/custom/temperatureDerating.py:L6-L33] | Unrecognized code falls through with implicit None | no detection check exists | issue pending (1) |
| S145 [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L51-L101] | Unrecognized code including DNV falls through with implicit None | no detection check exists | issue pending (1) |
| S146 [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L103-L108] | get_material_properties is a stub and returns None | no detection check exists | issue pending (5) |
| S147 [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L51-L101] | Unrecognized code including DNV falls through with implicit None | no detection check exists | issue pending (1) |
| S148 [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L103-L108] | get_material_properties is a stub and returns None | no detection check exists | issue pending (5) |
| S149 [src/digitalmodel/structural/pipe_capacity/PipeSizing.py:L193-L194] | buoyancyFactorAndDiameter is a stub and returns None | no detection check exists | issue pending (5) |
| S150 [src/digitalmodel/structural/pipe_capacity/common/PipeSizing.py:L289-L290] | buoyancyFactorAndDiameter is a stub and returns None | no detection check exists | issue pending (5) |
| S151 [src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py:L289-L290] | buoyancyFactorAndDiameter is a stub and returns None | no detection check exists | issue pending (5) |
| S152 [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L20-L47] | Unrecognized load-condition selector computes no result and raises no explicit error | no detection check exists | issue pending (1) |
| S153 [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L286-L313] | Unrecognized load-condition selector computes no result and raises no explicit error | no detection check exists | issue pending (1) |
| S154 [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L55-L73] | Built-in load/safety-class/WSD factor tables include zero accidental-load weighting in some formats; source editions and applicability require qualification | no detection check exists | issue pending (4) |
| S155 [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/APISTD2RDMethods.py:L114-L136] | Method-1 tension and moment convert between inch/force units and kN/kN.m using mixed conversion routes | no detection check exists | issue pending (4) |
| S156 [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/APISTD2RDMethods.py:L208-L228] | Utilization tension and moment conversions assume the same mixed input conventions | no detection check exists | issue pending (4) |

| Other confirmed pitfall | Detection / restriction | Tracking and evidence |
| --- | --- | --- |
| API RP 1111 external-pressure selector has no matching dispatch branch | no detection check exists; the historical issue asks whether external-pressure design is working, while current dispatch only recognizes API STD 2RD, API TR 5C3 and DNV | #59; [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L816-L839] |
| Thin/thick-wall selection uses original Design_WT rather than the supplied corroded thickness in API RP 1111 and modified Barlow pressure methods | no detection check exists for a corrosion-induced transition crossing | issue pending (3); [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L572-L586] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L723-L730] |
| API TR 5C3 optional external-pressure override is assigned to P_o, but interpolation reads the original load mapping | no detection check exists for a changed override result | issue pending (3); [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1230-L1256] |
| Several legacy modules import bare sibling names | no detection check exists in section 5; module imports shall be qualified before these standalone paths are promoted | #1900; [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L7-L8] [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L10-L13] |
| Legacy API interaction takes tension/moment from load row zero in every iteration and labels the external branch as internal | no detection check exists; exclude from hand-check references | issue pending (5); [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/APISTD2RDMethods.py:L249-L270] |
| Legacy F101 containment method is overwritten by its own scalar result; supplying Parameters bypasses assignment to self.Parameters; external displacement uses undefined names | no detection check exists; exclude from hand-check references | issue pending (5); [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L28-L55] [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L153-L182] [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L568-L587] |
| Pressure-area conversion in VonMises uses diameters and mixed force units | no detection check exists; exclude from hand-check references | issue pending (3); [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1283-L1290] |
| Two-pipe sizing updates an uninitialized nested equivalent-pipe mapping | no detection check exists; the constructor initializes only the outer mapping | issue pending (5); [src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py:L6-L9] [src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py:L114-L139] |
| Coating/buoyancy helpers call an unbound sectionProperties name | no detection check exists; keep them outside the routed pressure benchmark | issue pending (5); [src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py:L170-L188] |

No 1970 sentinel, explicit empty-list/dictionary error return, or cache-specific empty-result writer was found in the package Python sweep; empty collection construction for accumulation was distinguished from a returned empty result, and ordinary loops were distinguished from skipped records. [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/extractData.py:L7-L28] [src/digitalmodel/structural/pipe_capacity/custom/saveData.py:L9-L25]

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change / shared entry |
| --- | --- | --- | --- | --- |
| 2026-10-16 | Initial pack revision 0 | A passing regression can preserve an incomplete method; separate regression preservation from qualification | [tests/structural/pipe_capacity/test_pipe_capacity_common.py:L843-L850] | Sections 4–6 distinguish the placeholder; shared entry not established, issue pending (8) |
| 2026-10-16 | Initial pack revision 0 | A year in a selector is insufficient evidence that year-specific factors were resolved | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L11-L15] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L69-L90] | Sections 3 and 6 record the parser defect; shared entry not established, issue pending (8) |

## 8. Ownership

- Family quality manager role: engineering calculations quality manager; producing role: module documentation producer; escalation for classifications, tolerances, ambiguous clauses and unresolved input basis: family quality manager. (AQG r0, “Sign-off and independence”.)
- UNVERIFIED: deputy quality manager appointment, shared-log identifier and clearance evidence are not established; issue pending (8).
- Status remains DRAFT until a different-provider review resolves A1–A8 and the open dispositions. (AQG r0, “Sign-off and independence”.)

## 9. Dependencies

The routed input depends on `assetutilities` dictionary merging and unit quantities; sizing and material preparation precede capacity evaluation; the durable workflow writes the result YAML. [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L7-L8] [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L149-L156] [docs/registry/workflows.yaml:L20-L27]

### Implementation-map inventory and counted family

Map read at the code pin `bb18c1a3` (`bb18c1a346b31b4ce975befa41fc84d9a5ff29e5`); map data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. [docs/registry/implementation-map.yaml:L1-L4]

Only `effective-tension-riser-stack-up` has in-package implementation rows: 3 in-package rows, 264 family rows overall; it is therefore the only counted family. No subpackage is excluded. [docs/registry/implementation-map.yaml:L4150-L4179] [docs/registry/implementation-map.md:L18-L18]

The inventory below lists every row of that family in map order, including out-of-package and test rows; it records the map's classification without claiming all rows are independently qualified calculations. [docs/registry/implementation-map.md:L18-L18]

| Map row | Implementation path | Symbol | Map evidence |
| --- | --- | --- | --- |
| 1 | `docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py` | `main` | [docs/registry/implementation-map.yaml:L2146-L2155] |
| 2 | `docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py` | `main` | [docs/registry/implementation-map.yaml:L2156-L2165] |
| 3 | `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` | `main` | [docs/registry/implementation-map.yaml:L2166-L2175] |
| 4 | `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` | `main` | [docs/registry/implementation-map.yaml:L2176-L2185] |
| 5 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r01_catenary` | [docs/registry/implementation-map.yaml:L2186-L2195] |
| 6 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r02_lazy_wave` | [docs/registry/implementation-map.yaml:L2196-L2205] |
| 7 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r05_lay_table` | [docs/registry/implementation-map.yaml:L2206-L2215] |
| 8 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r06_multi_static` | [docs/registry/implementation-map.yaml:L2216-L2225] |
| 9 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `extract_vessel_params` | [docs/registry/implementation-map.yaml:L2226-L2235] |
| 10 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `vessel_band_specs` | [docs/registry/implementation-map.yaml:L2236-L2247] |
| 11 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `find_required_tension` | [docs/registry/implementation-map.yaml:L2248-L2259] |
| 12 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `_build_infeasible_result` | [docs/registry/implementation-map.yaml:L2260-L2269] |
| 13 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `run_single_case` | [docs/registry/implementation-map.yaml:L2270-L2282] |
| 14 | `examples/demos/gtm/results_store_demo04.py` | `(module-level statements)` | [docs/registry/implementation-map.yaml:L2283-L2291] |
| 15 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_good_case` | [docs/registry/implementation-map.yaml:L2292-L2301] |
| 16 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_infeasible_case` | [docs/registry/implementation-map.yaml:L2302-L2311] |
| 17 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `test_999_sentinel_and_none_null_round_trip` | [docs/registry/implementation-map.yaml:L2312-L2324] |
| 18 | `examples/domains/workflows/04_pipeline_installation.py` | `analyze_installation_catenary` | [docs/registry/implementation-map.yaml:L2325-L2334] |
| 19 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_schedule_row` | [docs/registry/implementation-map.yaml:L2335-L2344] |
| 20 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_assemble_row` | [docs/registry/implementation-map.yaml:L2345-L2356] |
| 21 | `scripts/capabilities/build_wall_thickness_3d.py` | `(module-level statements)` | [docs/registry/implementation-map.yaml:L2357-L2365] |
| 22 | `scripts/capabilities/build_wall_thickness_3d.py` | `_analyze_point` | [docs/registry/implementation-map.yaml:L2366-L2375] |
| 23 | `scripts/capabilities/build_wall_thickness_3d.py` | `build_study` | [docs/registry/implementation-map.yaml:L2376-L2387] |
| 24 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_axes` | [docs/registry/implementation-map.yaml:L2388-L2397] |
| 25 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_pack_rows` | [docs/registry/implementation-map.yaml:L2398-L2407] |
| 26 | `scripts/capabilities/build_wall_thickness_explorer.py` | `_run` | [docs/registry/implementation-map.yaml:L2408-L2417] |
| 27 | `scripts/orcaflex/check_generic_scr_slwr.py` | `metrics` | [docs/registry/implementation-map.yaml:L2418-L2427] |
| 28 | `scripts/orcaflex/check_generic_scr_slwr.py` | `calculate` | [docs/registry/implementation-map.yaml:L2428-L2437] |
| 29 | `scripts/orcaflex/prepare_slay_campaign.py` | `catenary` | [docs/registry/implementation-map.yaml:L2438-L2447] |
| 30 | `scripts/orcaflex/prepare_slay_campaign.py` | `case_spec` | [docs/registry/implementation-map.yaml:L2448-L2457] |
| 31 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare_case` | [docs/registry/implementation-map.yaml:L2458-L2470] |
| 32 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare` | [docs/registry/implementation-map.yaml:L2471-L2484] |
| 33 | `scripts/parametric/build_orcaflex_library.py` | `main` | [docs/registry/implementation-map.yaml:L2485-L2494] |
| 34 | `scripts/python/digitalmodel/analysis/APIST2RD_Methods.py` | `APISTD2RDCalculation` | [docs/registry/implementation-map.yaml:L2495-L2504] |
| 35 | `scripts/python/digitalmodel/analysis/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | [docs/registry/implementation-map.yaml:L2505-L2514] |
| 36 | `scripts/python/digitalmodel/analysis/RiserStackup.py` | `StackUpDiagram` | [docs/registry/implementation-map.yaml:L2515-L2524] |
| 37 | `scripts/python/digitalmodel/analysis/StackUp_3000Ft.py` | `main` | [docs/registry/implementation-map.yaml:L2525-L2534] |
| 38 | `scripts/python/digitalmodel/analysis/StackUp_9500Ft.py` | `main` | [docs/registry/implementation-map.yaml:L2535-L2544] |
| 39 | `scripts/reporting/build_riser_pipe_comparison.py` | `evaluate` | [docs/registry/implementation-map.yaml:L2545-L2556] |
| 40 | `scripts/reporting/build_riser_pipe_comparison.py` | `_validate` | [docs/registry/implementation-map.yaml:L2557-L2566] |
| 41 | `scripts/reporting/build_riser_pipe_comparison.py` | `numeric_values` | [docs/registry/implementation-map.yaml:L2567-L2576] |
| 42 | `scripts/reporting/build_riser_pipe_comparison.py` | `_input_table` | [docs/registry/implementation-map.yaml:L2577-L2586] |
| 43 | `scripts/riser_database/build_tables.py` | `(module-level statements)` | [docs/registry/implementation-map.yaml:L2587-L2595] |
| 44 | `src/digitalmodel/asset_integrity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | [docs/registry/implementation-map.yaml:L2596-L2605] |
| 45 | `src/digitalmodel/code_checks/dnv_os_f101.py` | `local_buckling_lcc` | [docs/registry/implementation-map.yaml:L2606-L2615] |
| 46 | `src/digitalmodel/code_checks/workflow.py` | `_run_api_rp_2rd` | [docs/registry/implementation-map.yaml:L2616-L2625] |
| 47 | `src/digitalmodel/code_checks/workflow.py` | `_run_dnv_os_f201` | [docs/registry/implementation-map.yaml:L2626-L2635] |
| 48 | `src/digitalmodel/drilling_riser/assembly.py` | `minimum_top_tension_16q` | [docs/registry/implementation-map.yaml:L2636-L2645] |
| 49 | `src/digitalmodel/drilling_riser/assembly.py` | `_unit_count` | [docs/registry/implementation-map.yaml:L2646-L2655] |
| 50 | `src/digitalmodel/drilling_riser/assembly.py` | `check_rig_capability` | [docs/registry/implementation-map.yaml:L2656-L2667] |
| 51 | `src/digitalmodel/drilling_riser/campaign.py` | `case_spec` | [docs/registry/implementation-map.yaml:L2668-L2679] |
| 52 | `src/digitalmodel/drilling_riser/campaign.py` | `_ring_tensioners` | [docs/registry/implementation-map.yaml:L2680-L2689] |
| 53 | `src/digitalmodel/drilling_riser/campaign.py` | `set_line_tension` | [docs/registry/implementation-map.yaml:L2690-L2701] |
| 54 | `src/digitalmodel/drilling_riser/campaign.py` | `calibrate_line_tension` | [docs/registry/implementation-map.yaml:L2702-L2715] |
| 55 | `src/digitalmodel/drilling_riser/campaign.py` | `_statics_path` | [docs/registry/implementation-map.yaml:L2716-L2726] |
| 56 | `src/digitalmodel/drilling_riser/campaign.py` | `robust_statics` | [docs/registry/implementation-map.yaml:L2727-L2740] |
| 57 | `src/digitalmodel/drilling_riser/campaign.py` | `_tensioner_vertical_n` | [docs/registry/implementation-map.yaml:L2741-L2750] |
| 58 | `src/digitalmodel/drilling_riser/campaign.py` | `tensioner_vertical_target_n` | [docs/registry/implementation-map.yaml:L2751-L2760] |
| 59 | `src/digitalmodel/drilling_riser/campaign.py` | `physical_state_checks` | [docs/registry/implementation-map.yaml:L2761-L2774] |
| 60 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.prepare` | [docs/registry/implementation-map.yaml:L2775-L2787] |
| 61 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.extract` | [docs/registry/implementation-map.yaml:L2788-L2797] |
| 62 | `src/digitalmodel/drilling_riser/envelope.py` | `compute_operating_envelope` | [docs/registry/implementation-map.yaml:L2798-L2807] |
| 63 | `src/digitalmodel/drilling_riser/global_model/build.py` | `equivalent_string_top_tension_n` | [docs/registry/implementation-map.yaml:L2808-L2817] |
| 64 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_tensioner_geometry` | [docs/registry/implementation-map.yaml:L2818-L2827] |
| 65 | `src/digitalmodel/drilling_riser/global_model/build.py` | `winch_tension_n` | [docs/registry/implementation-map.yaml:L2828-L2839] |
| 66 | `src/digitalmodel/drilling_riser/global_model/build.py` | `build_generic_spec` | [docs/registry/implementation-map.yaml:L2840-L2852] |
| 67 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `effective_tension_chain` | [docs/registry/implementation-map.yaml:L2853-L2862] |
| 68 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tension_references` | [docs/registry/implementation-map.yaml:L2863-L2874] |
| 69 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tensioner_lateral_stiffness_n_m` | [docs/registry/implementation-map.yaml:L2875-L2884] |
| 70 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `reference_periods` | [docs/registry/implementation-map.yaml:L2885-L2896] |
| 71 | `src/digitalmodel/drilling_riser/global_model/hang_off.py` | `hung_weight_n` | [docs/registry/implementation-map.yaml:L2897-L2907] |
| 72 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `OpenWaterRiserSpec._closes` | [docs/registry/implementation-map.yaml:L2908-L2917] |
| 73 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `build_open_water_generic_spec` | [docs/registry/implementation-map.yaml:L2918-L2927] |
| 74 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `tension_references` | [docs/registry/implementation-map.yaml:L2928-L2938] |
| 75 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `reference_periods` | [docs/registry/implementation-map.yaml:L2939-L2948] |
| 76 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `release_reference` | [docs/registry/implementation-map.yaml:L2949-L2960] |
| 77 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `ring_vertical_residual_n` | [docs/registry/implementation-map.yaml:L2961-L2970] |
| 78 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `open_water_physical_checks` | [docs/registry/implementation-map.yaml:L2971-L2980] |
| 79 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `Tensioners._failed` | [docs/registry/implementation-map.yaml:L2981-L2990] |
| 80 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `RiserGlobalModelSpec._geometry_closes` | [docs/registry/implementation-map.yaml:L2991-L3000] |
| 81 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `_missing_open_water` | [docs/registry/implementation-map.yaml:L3001-L3010] |
| 82 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `missing_channels` | [docs/registry/implementation-map.yaml:L3011-L3022] |
| 83 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract` | [docs/registry/implementation-map.yaml:L3023-L3032] |
| 84 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract_open_water` | [docs/registry/implementation-map.yaml:L3033-L3042] |
| 85 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `t_min` | [docs/registry/implementation-map.yaml:L3043-L3052] |
| 86 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tension_setting_max` | [docs/registry/implementation-map.yaml:L3053-L3062] |
| 87 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tensioner_stroke` | [docs/registry/implementation-map.yaml:L3063-L3072] |
| 88 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `ScheduleAssembly.minimum_top_tension_16q_kn` | [docs/registry/implementation-map.yaml:L3073-L3082] |
| 89 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `_load_oc_assembly` | [docs/registry/implementation-map.yaml:L3083-L3092] |
| 90 | `src/digitalmodel/drilling_riser/stackup.py` | `effective_tension` | [docs/registry/implementation-map.yaml:L3093-L3102] |
| 91 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `parse_snapshots` | [docs/registry/implementation-map.yaml:L3103-L3112] |
| 92 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `snapshot_to_tension_n` | [docs/registry/implementation-map.yaml:L3113-L3122] |
| 93 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `router` | [docs/registry/implementation-map.yaml:L3123-L3135] |
| 94 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_section_result` | [docs/registry/implementation-map.yaml:L3136-L3145] |
| 95 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_effective_tension_n` | [docs/registry/implementation-map.yaml:L3146-L3155] |
| 96 | `src/digitalmodel/drilling_riser/workflow.py` | `router` | [docs/registry/implementation-map.yaml:L3156-L3168] |
| 97 | `src/digitalmodel/drilling_riser/workflow.py` | `_write_profile_csv` | [docs/registry/implementation-map.yaml:L3169-L3178] |
| 98 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_to_shear7_result` | [docs/registry/implementation-map.yaml:L3179-L3188] |
| 99 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | [docs/registry/implementation-map.yaml:L3189-L3198] |
| 100 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | [docs/registry/implementation-map.yaml:L3199-L3208] |
| 101 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L3209-L3218] |
| 102 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_fatigue_damage_results_to_df` | [docs/registry/implementation-map.yaml:L3219-L3231] |
| 103 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3232-L3241] |
| 104 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3242-L3251] |
| 105 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L3252-L3264] |
| 106 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | [docs/registry/implementation-map.yaml:L3265-L3274] |
| 107 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | [docs/registry/implementation-map.yaml:L3275-L3287] |
| 108 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | [docs/registry/implementation-map.yaml:L3288-L3300] |
| 109 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | [docs/registry/implementation-map.yaml:L3301-L3311] |
| 110 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | [docs/registry/implementation-map.yaml:L3312-L3321] |
| 111 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | [docs/registry/implementation-map.yaml:L3322-L3331] |
| 112 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | [docs/registry/implementation-map.yaml:L3332-L3341] |
| 113 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | [docs/registry/implementation-map.yaml:L3342-L3351] |
| 114 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | [docs/registry/implementation-map.yaml:L3352-L3361] |
| 115 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.prepare_stack_up_properties_df` | [docs/registry/implementation-map.yaml:L3362-L3377] |
| 116 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_from_endA_in_df` | [docs/registry/implementation-map.yaml:L3378-L3387] |
| 117 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_shear7_x_by_L_in_df` | [docs/registry/implementation-map.yaml:L3388-L3397] |
| 118 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_mudline_in_df` | [docs/registry/implementation-map.yaml:L3398-L3407] |
| 119 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_MSL_in_df` | [docs/registry/implementation-map.yaml:L3408-L3417] |
| 120 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_inservice_weights_and_tensions` | [docs/registry/implementation-map.yaml:L3418-L3432] |
| 121 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.assign_key_properties_to_df` | [docs/registry/implementation-map.yaml:L3433-L3442] |
| 122 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stack_in_service_weights` | [docs/registry/implementation-map.yaml:L3443-L3452] |
| 123 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_effective_tension` | [docs/registry/implementation-map.yaml:L3453-L3462] |
| 124 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stretch_in_riser` | [docs/registry/implementation-map.yaml:L3463-L3472] |
| 125 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.title` | [docs/registry/implementation-map.yaml:L3473-L3482] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.riser_joints_to_shear7_zones` | [docs/registry/implementation-map.yaml:L3483-L3492] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.add_block_3` | [docs/registry/implementation-map.yaml:L3493-L3502] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_to_shear7_result` | [docs/registry/implementation-map.yaml:L3503-L3512] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | [docs/registry/implementation-map.yaml:L3513-L3522] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | [docs/registry/implementation-map.yaml:L3523-L3532] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L3533-L3542] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_fatigue_damage_results_to_df` | [docs/registry/implementation-map.yaml:L3543-L3555] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3556-L3565] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3566-L3575] |
| 135 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py` | `_write_tensions_csv` | [docs/registry/implementation-map.yaml:L3576-L3585] |
| 136 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._line_state` | [docs/registry/implementation-map.yaml:L3586-L3597] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._solve_line` | [docs/registry/implementation-map.yaml:L3598-L3607] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._result` | [docs/registry/implementation-map.yaml:L3608-L3617] |
| 139 | `src/digitalmodel/orcaflex/mooring_design.py` | `solve_catenary` | [docs/registry/implementation-map.yaml:L3618-L3627] |
| 140 | `src/digitalmodel/orcaflex/pipelay_analysis.py` | `JLayConfig.calculate_sagbend` | [docs/registry/implementation-map.yaml:L3628-L3637] |
| 141 | `src/digitalmodel/orcaflex/riser_config.py` | `SCRDesignInput.calculate_geometry` | [docs/registry/implementation-map.yaml:L3638-L3647] |
| 142 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_stroke` | [docs/registry/implementation-map.yaml:L3648-L3657] |
| 143 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_top_tension` | [docs/registry/implementation-map.yaml:L3658-L3667] |
| 144 | `src/digitalmodel/orcaflex/riser_input_schema.py` | `_TopTensionedInput.to_legacy` | [docs/registry/implementation-map.yaml:L3668-L3677] |
| 145 | `src/digitalmodel/orcaflex/viv_screening.py` | `BeamProperties.natural_frequency` | [docs/registry/implementation-map.yaml:L3678-L3687] |
| 146 | `src/digitalmodel/parametric/generate.py` | `_code_check_utilisation` | [docs/registry/implementation-map.yaml:L3688-L3697] |
| 147 | `src/digitalmodel/pipelay/workflow.py` | `run_jlay` | [docs/registry/implementation-map.yaml:L3698-L3707] |
| 148 | `src/digitalmodel/riser_configuration_screen/static.py` | `_lazy_row` | [docs/registry/implementation-map.yaml:L3708-L3717] |
| 149 | `src/digitalmodel/riser_configuration_screen/static.py` | `scr_offsets` | [docs/registry/implementation-map.yaml:L3718-L3727] |
| 150 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_validate` | [docs/registry/implementation-map.yaml:L3728-L3737] |
| 151 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_utilisation` | [docs/registry/implementation-map.yaml:L3738-L3747] |
| 152 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_checks` | [docs/registry/implementation-map.yaml:L3748-L3757] |
| 153 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/groups_builder.py` | `GroupsBuilder._build_from_spec` | [docs/registry/implementation-map.yaml:L3758-L3767] |
| 154 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `LineTypeBuilder.build` | [docs/registry/implementation-map.yaml:L3768-L3777] |
| 155 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.should_generate` | [docs/registry/implementation-map.yaml:L3778-L3787] |
| 156 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.build` | [docs/registry/implementation-map.yaml:L3788-L3797] |
| 157 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `_apply_overrides` | [docs/registry/implementation-map.yaml:L3798-L3807] |
| 158 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.mooring_pre_tension_analysis` | [docs/registry/implementation-map.yaml:L3808-L3819] |
| 159 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.evaluate_line_length_next_iteration` | [docs/registry/implementation-map.yaml:L3820-L3829] |
| 160 | `src/digitalmodel/solvers/orcaflex/opp_range_graph.py` | `OPPRangeGraph.postProcessRange` | [docs/registry/implementation-map.yaml:L3830-L3839] |
| 161 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.postProcessRange` | [docs/registry/implementation-map.yaml:L3840-L3849] |
| 162 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L3850-L3862] |
| 163 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | [docs/registry/implementation-map.yaml:L3863-L3872] |
| 164 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | [docs/registry/implementation-map.yaml:L3873-L3885] |
| 165 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | [docs/registry/implementation-map.yaml:L3886-L3898] |
| 166 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | [docs/registry/implementation-map.yaml:L3899-L3909] |
| 167 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | [docs/registry/implementation-map.yaml:L3910-L3919] |
| 168 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | [docs/registry/implementation-map.yaml:L3920-L3929] |
| 169 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | [docs/registry/implementation-map.yaml:L3930-L3939] |
| 170 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | [docs/registry/implementation-map.yaml:L3940-L3949] |
| 171 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | [docs/registry/implementation-map.yaml:L3950-L3959] |
| 172 | `src/digitalmodel/structural/analysis/cli.py` | `check` | [docs/registry/implementation-map.yaml:L3960-L3969] |
| 173 | `src/digitalmodel/structural/analysis/wall_thickness.py` | `WallThicknessAnalyzer.check_combined_loading` | [docs/registry/implementation-map.yaml:L3970-L3979] |
| 174 | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py` | `ApiStd2rdStrategy._check_combined` | [docs/registry/implementation-map.yaml:L3980-L3989] |
| 175 | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py` | `DnvStF101Strategy._check_combined_loading` | [docs/registry/implementation-map.yaml:L3990-L3999] |
| 176 | `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py` | `run_wall_thickness_compare` | [docs/registry/implementation-map.yaml:L4000-L4009] |
| 177 | `src/digitalmodel/structural/analysis/wall_thickness_lookup.py` | `MTLookupGenerator.generate` | [docs/registry/implementation-map.yaml:L4010-L4019] |
| 178 | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `ParametricSweep.run` | [docs/registry/implementation-map.yaml:L4020-L4029] |
| 179 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `PipelinePhase.to_design_loads` | [docs/registry/implementation-map.yaml:L4030-L4039] |
| 180 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `create_standard_phases` | [docs/registry/implementation-map.yaml:L4040-L4049] |
| 181 | `src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py` | `LoadScalingProcessor.process_all_conditions` | [docs/registry/implementation-map.yaml:L4050-L4059] |
| 182 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L4060-L4069] |
| 183 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | [docs/registry/implementation-map.yaml:L4070-L4079] |
| 184 | `src/digitalmodel/structural/fatigue_apps/load_scaling.py` | `LoadScalingProcessor.load_reference_data` | [docs/registry/implementation-map.yaml:L4080-L4089] |
| 185 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L4090-L4099] |
| 186 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L4100-L4109] |
| 187 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_sea_state` | [docs/registry/implementation-map.yaml:L4110-L4119] |
| 188 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `main` | [docs/registry/implementation-map.yaml:L4120-L4129] |
| 189 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py` | `ProductionDataHandlerWithProgress.process_single_analysis` | [docs/registry/implementation-map.yaml:L4130-L4139] |
| 190 | `src/digitalmodel/structural/parametric_coordinator.py` | `ParametricCoordinator._run_wall_thickness` | [docs/registry/implementation-map.yaml:L4140-L4149] |
| 191 | `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | [docs/registry/implementation-map.yaml:L4150-L4159] |
| 192 | `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | [docs/registry/implementation-map.yaml:L4160-L4169] |
| 193 | `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | [docs/registry/implementation-map.yaml:L4170-L4179] |
| 194 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `simple_catenary` | [docs/registry/implementation-map.yaml:L4180-L4189] |
| 195 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `lazy_wave_analysis` | [docs/registry/implementation-map.yaml:L4190-L4199] |
| 196 | `src/digitalmodel/subsea/catenary_riser/lazy_wave.py` | `LazyWaveAnalyzer.analyze` | [docs/registry/implementation-map.yaml:L4200-L4209] |
| 197 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.analyze_riser` | [docs/registry/implementation-map.yaml:L4210-L4222] |
| 198 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_catenary_from_H` | [docs/registry/implementation-map.yaml:L4223-L4232] |
| 199 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_H_from_top_tension` | [docs/registry/implementation-map.yaml:L4233-L4242] |
| 200 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.check_design_limits` | [docs/registry/implementation-map.yaml:L4243-L4252] |
| 201 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `developed_min_radius` | [docs/registry/implementation-map.yaml:L4253-L4262] |
| 202 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `required_effective_ei` | [docs/registry/implementation-map.yaml:L4263-L4272] |
| 203 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `screen_bend_stiffener` | [docs/registry/implementation-map.yaml:L4273-L4285] |
| 204 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `armour_tension_stress` | [docs/registry/implementation-map.yaml:L4286-L4295] |
| 205 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `verify_flexible_pipe` | [docs/registry/implementation-map.yaml:L4296-L4307] |
| 206 | `src/digitalmodel/subsea/pipeline/global_buckling/hobbs_lateral.py` | `effective_driving_force` | [docs/registry/implementation-map.yaml:L4308-L4317] |
| 207 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `RiserLoadCase.to_loads` | [docs/registry/implementation-map.yaml:L4318-L4327] |
| 208 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `run_riser_screening` | [docs/registry/implementation-map.yaml:L4328-L4337] |
| 209 | `src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py` | `StackUpDiagram` | [docs/registry/implementation-map.yaml:L4338-L4347] |
| 210 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_riser_model` | [docs/registry/implementation-map.yaml:L4348-L4362] |
| 211 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.read_riser_input_data` | [docs/registry/implementation-map.yaml:L4363-L4372] |
| 212 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.assign_key_properties_to_self` | [docs/registry/implementation-map.yaml:L4373-L4382] |
| 213 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.get_fea_properties` | [docs/registry/implementation-map.yaml:L4383-L4392] |
| 214 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_tensioner_data` | [docs/registry/implementation-map.yaml:L4393-L4402] |
| 215 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.evaluate_stack_up_properties` | [docs/registry/implementation-map.yaml:L4403-L4412] |
| 216 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.save_data` | [docs/registry/implementation-map.yaml:L4413-L4422] |
| 217 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.alter_properties_for_Shear7_software` | [docs/registry/implementation-map.yaml:L4423-L4432] |
| 218 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_natural_frequency` | [docs/registry/implementation-map.yaml:L4433-L4444] |
| 219 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator._frequency_tension_controlled` | [docs/registry/implementation-map.yaml:L4445-L4454] |
| 220 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_effective_tension` | [docs/registry/implementation-map.yaml:L4455-L4464] |
| 221 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.check_stability` | [docs/registry/implementation-map.yaml:L4465-L4476] |
| 222 | `src/digitalmodel/viv_screening/inputs.py` | `_beam` | [docs/registry/implementation-map.yaml:L4477-L4486] |
| 223 | `src/digitalmodel/workflows/workflow_automation/cli.py` | `riser_analysis_cmd` | [docs/registry/implementation-map.yaml:L4487-L4496] |
| 224 | `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_riser_analysis_template` | [docs/registry/implementation-map.yaml:L4497-L4506] |
| 225 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `CompleteRiserAnalysisWorkflow.create` | [docs/registry/implementation-map.yaml:L4507-L4516] |
| 226 | `tests/asset_integrity/test_calculations.py` | `_make_pipe_cfg` | [docs/registry/implementation-map.yaml:L4517-L4526] |
| 227 | `tests/capabilities/test_wall_thickness_3d_page.py` | `_study` | [docs/registry/implementation-map.yaml:L4527-L4536] |
| 228 | `tests/contracts/test_force_units_boundary_contract.py` | `(module-level statements)` | [docs/registry/implementation-map.yaml:L4537-L4545] |
| 229 | `tests/contracts/test_force_units_boundary_contract.py` | `test_envelope_von_mises_n_to_kn_seam` | [docs/registry/implementation-map.yaml:L4546-L4557] |
| 230 | `tests/drilling_riser/global_model/conftest.py` | `synthetic_spec` | [docs/registry/implementation-map.yaml:L4558-L4567] |
| 231 | `tests/drilling_riser/global_model/test_batch_params.py` | `test_physical_checks_use_the_remaining_tensioners` | [docs/registry/implementation-map.yaml:L4568-L4578] |
| 232 | `tests/drilling_riser/global_model/test_campaign.py` | `_fake` | [docs/registry/implementation-map.yaml:L4579-L4589] |
| 233 | `tests/drilling_riser/global_model/test_open_water.py` | `open_water_dict` | [docs/registry/implementation-map.yaml:L4590-L4599] |
| 234 | `tests/drilling_riser/global_model/test_open_water_campaign.py` | `_doc` | [docs/registry/implementation-map.yaml:L4600-L4609] |
| 235 | `tests/drilling_riser/global_model/test_orcaflex_solver.py` | `test_statics_tension_and_modes_agree_with_hand_checks` | [docs/registry/implementation-map.yaml:L4610-L4623] |
| 236 | `tests/drilling_riser/global_model/test_statics_path.py` | `test_branch_tolerance_and_ring_buoyancy` | [docs/registry/implementation-map.yaml:L4624-L4634] |
| 237 | `tests/drilling_riser/global_model/test_tensioner_representation.py` | `_vertical` | [docs/registry/implementation-map.yaml:L4635-L4644] |
| 238 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_mc` | [docs/registry/implementation-map.yaml:L4645-L4654] |
| 239 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_case` | [docs/registry/implementation-map.yaml:L4655-L4666] |
| 240 | `tests/drilling_riser/test_drive_off_worked_example.py` | `_recorded_arguments` | [docs/registry/implementation-map.yaml:L4667-L4676] |
| 241 | `tests/drilling_riser/test_schedule_assembly.py` | `_deltas` | [docs/registry/implementation-map.yaml:L4677-L4686] |
| 242 | `tests/infrastructure/unit/test_pipe_capacity_codes.py` | `_base_cfg` | [docs/registry/implementation-map.yaml:L4687-L4696] |
| 243 | `tests/orcaflex/test_generic_scr_slwr.py` | `test_analytical_reference_reproduces_from_basis` | [docs/registry/implementation-map.yaml:L4697-L4706] |
| 244 | `tests/parametric/test_atlas.py` | `_code_check_atlas` | [docs/registry/implementation-map.yaml:L4707-L4716] |
| 245 | `tests/reporting/test_riser_pipe_comparison.py` | `reference_input` | [docs/registry/implementation-map.yaml:L4717-L4726] |
| 246 | `tests/riser_configuration_screen/test_workflow.py` | `settings` | [docs/registry/implementation-map.yaml:L4727-L4736] |
| 247 | `tests/solvers/orcaflex/analysis/scripts/verify_configs.py` | `verify_configuration` | [docs/registry/implementation-map.yaml:L4737-L4746] |
| 248 | `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py` | `_make_slay_spec` | [docs/registry/implementation-map.yaml:L4747-L4756] |
| 249 | `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` | `_make_pipeline_spec` | [docs/registry/implementation-map.yaml:L4757-L4766] |
| 250 | `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` | `_make_pipeline_spec` | [docs/registry/implementation-map.yaml:L4767-L4776] |
| 251 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_slay_campaign` | [docs/registry/implementation-map.yaml:L4777-L4786] |
| 252 | `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py` | `_make_base_spec_data` | [docs/registry/implementation-map.yaml:L4787-L4796] |
| 253 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` | `extract_static_range_results` | [docs/registry/implementation-map.yaml:L4797-L4806] |
| 254 | `tests/solvers/orcaflex/modular_generator/test_post_validator.py` | `_make_valid_model` | [docs/registry/implementation-map.yaml:L4807-L4816] |
| 255 | `tests/solvers/orcaflex/test_slay_layout.py` | `test_support_types_are_defined_before_vessel_references` | [docs/registry/implementation-map.yaml:L4817-L4826] |
| 256 | `tests/specialized/umbilical_analysis/conftest.py` | `installation_parameters` | [docs/registry/implementation-map.yaml:L4827-L4836] |
| 257 | `tests/subsea/catenary_riser/test_catenary.py` | `run_catenary_riser` | [docs/registry/implementation-map.yaml:L4837-L4846] |
| 258 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_catenary_analysis_mock` | [docs/registry/implementation-map.yaml:L4847-L4856] |
| 259 | `tests/test_automation/scripts/batch_fix_tests.py` | `(module-level statements)` | [docs/registry/implementation-map.yaml:L4857-L4865] |
| 260 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | [docs/registry/implementation-map.yaml:L4866-L4875] |
| 261 | `tests/test_rainflow_with_effective_tension.py` | `main` | [docs/registry/implementation-map.yaml:L4876-L4887] |
| 262 | `tests/test_wall_thickness_compare_workflow.py` | `_cfg` | [docs/registry/implementation-map.yaml:L4888-L4897] |
| 263 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | [docs/registry/implementation-map.yaml:L4898-L4907] |
| 264 | `tests/workflows/test_viv_screening_workflow.py` | `screening_input` | [docs/registry/implementation-map.yaml:L4908-L4917] |

### Counted-family search and classification

All three signals are retained: none matches `structural/pipe_capacity` under case-insensitive path comparison. The search uses case-sensitive Python `re` against every pinned package `.py` file, with each hit assigned to its innermost function/class or module-level statements; class and module docstrings are discarded as implementations. [docs/registry/implementation-map.yaml:L2139-L2145]

```python
signals = [
    r"effective_tension|effectiveTension|top_tension|tensioner|stack_up|internal_pressure.*(?:area|A_i)|external_pressure.*(?:area|A_o)",
    r"effective.tension|tensioner|stack.up",
    r"internal_pressure|external_pressure|fluid_density|buoyancy_module",
]
```

The 28 matching functions are classified below; the two `VonMises_Pipe.init_calculate_required_properties` entries remain in the inventory even though their capitalized `EffectiveTension` spelling does not match these case-sensitive signals. [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L996-L1003] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1283-L1290]

| Definition | Classification | Evidence |
| --- | --- | --- |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` / `PipeCapacity.evaluate_load_conditions` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L20-L47] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` / `PipeCapacity.internal_pressure` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L49-L90] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` / `PipeCapacity.external_pressure` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L135-L176] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` / `PipeCapacity.internal_pressure_specification_code_based_evaluation` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L230-L275] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` / `PipeCapacity.external_pressure_specification_code_based_evaluation` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L543-L559] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` / `PipeCapacity.evaluate_collapse_propagation_minimum_thickness_API_RP_1111` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L631-L658] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` / `API_TR_5C3.get_collapse_minimum_thickness` | Pressure conversion, configuration or interpolation helper for section-3 definitions; no miss | [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L943-L976] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` / `VonMises_Pipe.get_burst_minimum_thickness` | Pressure conversion, configuration or interpolation helper for section-3 definitions; no miss | [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L1057-L1090] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` / `CFR_30_Part_250.get_burst_minimum_thickness` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L1140-L1167] |
| `src/digitalmodel/structural/pipe_capacity/common/pipe_components.py` / `PipeComponents.evaluate_pipe_capacity` | Pressure conversion, configuration or interpolation helper for section-3 definitions; no miss | [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L44-L156] |
| `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py` / `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | Listed implementation; no miss | [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L342-L493] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `_resolve_pressure_value` | Pressure conversion, configuration or interpolation helper for section-3 definitions; no miss | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L18-L25] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `DNVWallThickness._resolve_design_factors` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L46-L105] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `DNVWallThickness._get_material_strengths` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L117-L124] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `DNVWallThickness._pressure_containment_allowable` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L156-L169] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `DNVWallThickness.get_burst_minimum_thickness` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L175-L187] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `DNVWallThickness._collapse_pressure` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L189-L217] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `DNVWallThickness.get_collapse_pressure` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L219-L226] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `DNVWallThickness.get_collapse_minimum_thickness` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L228-L249] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `PipeCapacity.evaluate_load_conditions` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L286-L313] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `PipeCapacity.internal_pressure` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L315-L356] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `PipeCapacity.external_pressure` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L401-L442] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `PipeCapacity.internal_pressure_specification_code_based_evaluation` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L496-L548] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `PipeCapacity.external_pressure_specification_code_based_evaluation` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L816-L839] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `PipeCapacity.evaluate_collapse_propagation_minimum_thickness_API_RP_1111` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L918-L945] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `API_TR_5C3.get_collapse_minimum_thickness` | Pressure conversion, configuration or interpolation helper for section-3 definitions; no miss | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1230-L1263] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `VonMises_Pipe.get_burst_minimum_thickness` | Pressure conversion, configuration or interpolation helper for section-3 definitions; no miss | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1344-L1377] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` / `CFR_30_Part_250.get_burst_minimum_thickness` | Listed in section 3; pressure/capacity calculation or delegated dispatch, not a further tension implementation | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1427-L1454] |

A8 producer result: PASS, full inventory present and 0 further second implementations against the criterion of at most 1; module-level matches in the F201 overview describe effective tension rather than calculate it. [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L1-L45]

### Map follow-up and repository-wide discovery

Burst, collapse, propagation, annular section properties, temperature derating and F201 interaction do not have their own map families at this map revision; their section-3 definitions are coverage follow-up, not additional counted families; issue pending (6). [docs/registry/implementation-map.md:L9-L25]

Repository-wide discovery includes `scripts/`, using:

```bash
rg -n 'APISTD2RDBurst|ASMEB318InternalPressure|class DNVWallThickness|class VonMises_Pipe|def bursting_resistance|def factored_load_effect|localBucklingCombinedLoadingLoadControlled' . --glob '*.py'
```

Out-of-package candidates include legacy API STD 2RD and F101 scripts, an asset-integrity copy and separate wall-thickness strategies; map reconciliation is tracked in issue pending (6), with no effect on this pack's counted-family result. [scripts/python/digitalmodel/analysis/APISTD2RDMethods.py:L1-L40] [scripts/python/digitalmodel/analysis/RigidPipe.py:L1-L40] [src/digitalmodel/asset_integrity/custom/PipeCapacity.py:L1-L40] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L1-L40]

## 10. Coverage and open items

The following inventory records source inspection rather than runtime qualification; every Python file under the package is included, including legacy utilities with module-level effects. [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/APISTD2RD.py:L16-L49]

| Python file read | Lines at code pin |
| --- | --- |
| `src/digitalmodel/structural/pipe_capacity/PipeSizing.py` | [src/digitalmodel/structural/pipe_capacity/PipeSizing.py:L1-L195] |
| `src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py` | [src/digitalmodel/structural/pipe_capacity/common/MaterialProperties.py:L1-L137] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` | [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L1-L1192] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeSizing.py` | [src/digitalmodel/structural/pipe_capacity/common/PipeSizing.py:L1-L290] |
| `src/digitalmodel/structural/pipe_capacity/common/pipe_components.py` | [src/digitalmodel/structural/pipe_capacity/common/pipe_components.py:L1-L156] |
| `src/digitalmodel/structural/pipe_capacity/common/temperatureDerating.py` | [src/digitalmodel/structural/pipe_capacity/common/temperatureDerating.py:L1-L33] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_RP_2RD/APIRP2RD_Methods.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_RP_2RD/APIRP2RD_Methods.py:L1-L33] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/APISTD2RDMethods.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/APISTD2RDMethods.py:L1-L272] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/APISTD2RD.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/APISTD2RD.py:L1-L49] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/APISTD2RDMethods.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/APISTD2RDMethods.py:L1-L76] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/__init__.py` | UNVERIFIED: no line citation can be attached to an empty marker; issue pending (8) |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/checkPythonEnvironment.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/checkPythonEnvironment.py:L1-L27] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/customUpdate.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/customUpdate.py:L1-L47] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/loadConfiguration.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/loadConfiguration.py:L1-L9] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/ymlInput.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/ymlInput.py:L1-L23] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/results/__init__.py` | UNVERIFIED: no line citation can be attached to an empty marker; issue pending (8) |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/results/plotDefault.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/results/plotDefault.py:L1-L42] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/results/saveData.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/results/saveData.py:L1-L10] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/temperatureDerating.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/temperatureDerating.py:L1-L25] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/Summary.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/Summary.py:L1-L63] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/extractData.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/extractData.py:L1-L28] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/fileList.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/fileList.py:L1-L15] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/temperatureDerating.py` | [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/temperatureDerating.py:L1-L11] |
| `src/digitalmodel/structural/pipe_capacity/custom/ASMEB31/extractData.py` | [src/digitalmodel/structural/pipe_capacity/custom/ASMEB31/extractData.py:L1-L64] |
| `src/digitalmodel/structural/pipe_capacity/custom/ASMEB31/fileList.py` | [src/digitalmodel/structural/pipe_capacity/custom/ASMEB31/fileList.py:L1-L15] |
| `src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py` | [src/digitalmodel/structural/pipe_capacity/custom/ASMEMethods.py:L1-L236] |
| `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/AttributeDict.py` | [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/AttributeDict.py:L1-L6] |
| `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/ConfigurationManager.py` | [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/ConfigurationManager.py:L1-L67] |
| `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py` | [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L1-L587] |
| `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/solvePolynomialEquation.py` | [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/solvePolynomialEquation.py:L1-L23] |
| `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/sympyPolynomial.py` | [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/sympyPolynomial.py:L1-L24] |
| `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/user_setup/__init__.py` | UNVERIFIED: no line citation can be attached to an empty marker; issue pending (8) |
| `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py` | [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L1-L246] |
| `src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py` | [src/digitalmodel/structural/pipe_capacity/custom/MaterialProperties.py:L1-L137] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` | [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1-L1479] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py` | [src/digitalmodel/structural/pipe_capacity/custom/PipeSizing.py:L1-L290] |
| `src/digitalmodel/structural/pipe_capacity/custom/plotCustom.py` | [src/digitalmodel/structural/pipe_capacity/custom/plotCustom.py:L1-L40] |
| `src/digitalmodel/structural/pipe_capacity/custom/plotDefault.py` | [src/digitalmodel/structural/pipe_capacity/custom/plotDefault.py:L1-L42] |
| `src/digitalmodel/structural/pipe_capacity/custom/saveData.py` | [src/digitalmodel/structural/pipe_capacity/custom/saveData.py:L1-L33] |
| `src/digitalmodel/structural/pipe_capacity/custom/temperatureDerating.py` | [src/digitalmodel/structural/pipe_capacity/custom/temperatureDerating.py:L1-L33] |
| `src/digitalmodel/structural/pipe_capacity/pipe_capacity.py` | [src/digitalmodel/structural/pipe_capacity/pipe_capacity.py:L1-L17] |

| Tests inspected | Scope |
| --- | --- |
| `tests/structural/pipe_capacity/test_pipe_capacity_common.py` | common pressure/collapse/propagation, dispatch, regression and edge cases; [tests/structural/pipe_capacity/test_pipe_capacity_common.py:L1-L20] |
| `tests/structural/pipe_capacity/test_pipe_capacity_custom.py` | helpers, DNV, custom dispatch and ASME/API formulas; [tests/structural/pipe_capacity/test_pipe_capacity_custom.py:L1-L17] |
| `tests/structural/pipe_capacity/test_dnv_os_f201_lrfd_wsd.py` | F201 comparison-spreadsheet regression provenance, not independent validation; [tests/structural/pipe_capacity/test_dnv_os_f201_lrfd_wsd.py:L1-L13] |
| `tests/infrastructure/unit/test_pipe_capacity_codes.py` | monotonicity and dispatch; [tests/infrastructure/unit/test_pipe_capacity_codes.py:L44-L80] |
| `tests/infrastructure/unit/test_pipe_capacity_dnv.py` | DNV pressure/thickness, factors and fluid-column input; [tests/infrastructure/unit/test_pipe_capacity_dnv.py:L45-L114] |
| `tests/workflows/test_durable_workflows.py` | only registered pipe-capacity node; input/output in isolated temporary copy; [tests/workflows/test_durable_workflows.py:L52-L108] |

The apparent sizing-test matches under asset_integrity and specialized exercise separate packages; they are repository-wide dependency leads rather than module-test counts. [tests/asset_integrity/test_pipe_sizing.py:L56-L59] [tests/specialized/custom/test_pipesizing.py:L35-L35]

| Documentation discovery | Disposition |
| --- | --- |
| `docs/maps/digitalmodel-operator-map.md` | Discovery reference; [docs/maps/digitalmodel-operator-map.md:L68-L68] |
| `docs/field-development/subsea-production-systems-mapping.md` | Discovery reference; [docs/field-development/subsea-production-systems-mapping.md:L95-L95] |
| `docs/migration/legacy-batch-workflows.md` | Discovery reference; [docs/migration/legacy-batch-workflows.md:L98-L151] |
| `docs/registry/implementation-map.md` | Discovery reference; [docs/registry/implementation-map.md:L18-L18] |
| `docs/vision/CALCULATIONS-VISION.md` | Discovery reference; [docs/vision/CALCULATIONS-VISION.md:L142-L181] |
| `docs/domains/pipecapacity/_index.md` | Discovery reference; [docs/domains/pipecapacity/_index.md:L9-L9] |
| `docs/domains/testing/coverage_analysis_report.md` | Discovery reference; [docs/domains/testing/coverage_analysis_report.md:L35-L193] |
| `docs/domains/pyintegrity/TODO.md` | Discovery reference; [docs/domains/pyintegrity/TODO.md:L13-L13] |
| `docs/domains/pipecapacity/WALL_THICKNESS_IMPLEMENTED_CALCS.md` | Discovery reference; [docs/domains/pipecapacity/WALL_THICKNESS_IMPLEMENTED_CALCS.md:L29-L333] |
| `docs/domains/testing/engine_test_implementation_plan.md` | Discovery reference; [docs/domains/testing/engine_test_implementation_plan.md:L381-L645] |
| `docs/domains/test_automation/README.md` | Discovery reference; [docs/domains/test_automation/README.md:L86-L86] |
| `docs/domains/structural/pipe_capacity/README.md` | Discovery reference; [docs/domains/structural/pipe_capacity/README.md:L1-L12] |
| `docs/domains/project-docs/FINAL_TEST_BASELINE.md` | Discovery reference; [docs/domains/project-docs/FINAL_TEST_BASELINE.md:L72-L101] |
| `docs/domains/project-docs/TEST_BASELINE_SUMMARY.md` | Discovery reference; [docs/domains/project-docs/TEST_BASELINE_SUMMARY.md:L60-L194] |
| `docs/domains/pipecapacity/WALL_THICKNESS_LATEST_ONLY_LATEX.md` | Background method/prose inspected, not adopted as a verified independent reference; [docs/domains/pipecapacity/WALL_THICKNESS_LATEST_ONLY_LATEX.md:L1-L5] |
| `docs/domains/pipecapacity/wallthickness.md` | Background method/prose inspected, not adopted as a verified independent reference; [docs/domains/pipecapacity/wallthickness.md:L1-L5] |
| `docs/domains/pipecapacity/capacity_load_condition.md` | Background method/prose inspected, not adopted as a verified independent reference; [docs/domains/pipecapacity/capacity_load_condition.md:L1-L5] |

2 discovery files with identifying names are omitted from this public path list; codenaming follow-up is issue pending (8).

| Open item | Scope, disposition and responsible role | Tracking |
| --- | --- | --- |
| Input/default qualification | Maintainer: distinguish missing/invalid pressure, material, factors and temperature from intentional zero; locale-aware parsing and unit checks | issue pending (1); section-6 source rows |
| Year parsing | Maintainer: repair ordinary-edition recognition without silently changing design-factor selection | issue pending (2); [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L11-L15] |
| Inverse and force basis | Maintainer: replace collapse placeholder, qualify interpolation/bisection boundaries and pressure-area force conversion | issue pending (3); [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L861-L873] [src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py:L1283-L1290] |
| UNVERIFIED: governing standard provenance | Quality manager: resolve clause titles, applicable editions, code/docs agreement and Citation emission; licensed text remains unchecked | issue pending (4); [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py:L42-L45] |
| Legacy independent paths | Maintainer: qualify or retire legacy interaction, configuration, sizing, default-file and export paths, including implicit None and empty results | issue pending (5); section-6 source rows |
| Map coverage | Maintainer: add burst/collapse/propagation/section/derating/F201 families before counting them; reconcile outside-package candidates | issue pending (6); [docs/registry/implementation-map.md:L9-L25] |
| UNVERIFIED: independent worked-example basis | Quality manager: published benchmarks, source revision and tolerance basis for all main-path codes and F201 spreadsheet provenance | issue pending (7); [tests/structural/pipe_capacity/test_dnv_os_f201_lrfd_wsd.py:L1-L13] |
| UNVERIFIED: lifecycle records | Maintainer: appoint deputy, assign shared-log entry, complete different-provider review and replace draft references with issue numbers; codenaming of identifying discovery paths | issue pending (8) |
| Execution evidence gap | Maintainer: the 232-node baseline comparison is complete; E01 remains outside the matched BASE/AFTER comparison, with its two historical passes retained separately | issue pending (9) |
| Existing documentation gap | Maintainer: placeholder README/basic-usage page and stale pre-reorganization paths remain unmodified | #59; [docs/domains/structural/pipe_capacity/README.md:L1-L17] [examples/structural/pipe_capacity/basic_usage.py:L1-L2] [docs/domains/pipecapacity/WALL_THICKNESS_IMPLEMENTED_CALCS.md:L29-L36] |
| Existing import debt | Maintainer: resolve bare sibling imports before treating legacy paths as supported entry points | #1900; [src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py:L7-L8] |

UNVERIFIED: broader geometry, corrosion, high-temperature, combined-load, edition-conformance and client-data applicability are not established by the offline regression suite; independent qualification is issue pending (7). No client-derived values or private sidecar payload are included; sidecar entry: to be created if such evidence is later proposed, issue pending (8).
