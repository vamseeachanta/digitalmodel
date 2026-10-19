# Base solvers knowledge pack

| Header | Value |
| --- | --- |
| Module | infrastructure/base_solvers |
| Repository | digitalmodel |
| Package | src/digitalmodel/infrastructure/base_solvers |
| Pack revision | 0 |
| Code revision | 1dba289c5dc1bc8dc82d72e4bfe02f8dcfc22c23 |
| Analysis family | Engineering calculations (EC) |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The public framework exports solver base classes, configuration-aware and analysis-aware classes, and structural protocols. [src/digitalmodel/infrastructure/base_solvers/__init__.py:L6-L32]

The principal legacy calculation routes are S-N/time-trace fatigue damage and grouped plate-buckling utilization; the engine calls these routes for their corresponding basenames. [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L23-L50] [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L9-L31] [src/digitalmodel/engine.py:L318-L320] [src/digitalmodel/engine.py:L543-L545]

Detailed input and method guidance below covers those routes; framework, finite-element utilities, cathodic protection, marine/VIV exporters, pipeline mathematics and well-path tools remain in the package inventory and failure sweep. [src/digitalmodel/infrastructure/base_solvers/base.py:L26-L49] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/__init__.py:L6-L26] [src/digitalmodel/infrastructure/base_solvers/structural/__init__.py:L6-L38] [src/digitalmodel/infrastructure/base_solvers/viv/__init__.py:L6-L14] [src/digitalmodel/infrastructure/base_solvers/pipeline_solvers/math_solvers.py:L1-L20] [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1-L20]

The infrastructure routing entry declares beta maturity; the legacy README contains a description/API placeholder, so neither maturity nor documentation establishes numerical qualification. [docs/registry/module-routing.yaml:L156-L161] [docs/domains/infrastructure/base_solvers/README.md:L1-L17]

Fatigue software selections for OrcaFlex and Abaqus raise `NotImplementedError`; this route is a Python calculation rather than a solver launch. [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L33-L48]

## 2. Inputs and outputs

| Input / route | Unit and accepted basis | Range / default | Source |
| --- | --- | --- | --- |
| `inputs.calculation_type`, `inputs.stress_input` / fatigue | enum | `damage`; `sn` or `timetrace`; other calculation types return the configuration unchanged | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L37-L50] |
| `inputs.SN[].s`, `n_cycles` | stress unit must match the selected curve; cycles are counts | Direct indexing; no range validation in accumulation | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L62-L70] |
| `inputs.timetraces[].s_trace`, `n_traces` | curve-consistent stress; trace multiplier in counts | Rainflow delegation and trace-weighted damage | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L107-L139] |
| `inputs.fatigue_curve`, `fatigue_data.csv/io` | curve label and CSV locator | First matching `Curve Label`; defaults reference a 2005-labelled curve and a test-data CSV | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L154-L210] |
| `calculation_type`, `groups[]` / plate | enum and array | `DNV_rp_C201`; unsupported method raises | [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L9-L31] |
| plate `length`, `breadth`, `thickness`, `water_depth` | Code-implied metre input basis, converted to feet internally | Direct indexing; conversion rounds before ratios and resistance calculations | [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L132-L165] |
| plate `yield_strength`, `young_modulus`, `poisson_ratio` | mutually consistent stress units; Poisson ratio dimensionless | No conversion of strengths in property extraction | [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L137-L160] |
| plate `longtudinal_stress`, `transverse_stress`, `shear_stress` | same stress basis as resistance | Exact misspelled longitudinal key is required | [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L167-L187] |

| Output | Unit / convention | Source |
| --- | --- | --- |
| `fatigue_analysis.damage` | dimensionless sum of applied cycles / allowable cycles | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L62-L72] |
| `fatigue_analysis.total_damage`, per-trace damage | dimensionless, including `n_traces` multiplier | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L107-L139] |
| `cfg[basename]` / plate | list of ultimate/serviceability and DNV utilization dictionaries, dimensionless usage factors | [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L29-L31] [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L122-L130] |
| `get_results()` / framework | deep copy of cached result mapping | [src/digitalmodel/infrastructure/base_solvers/base.py:L108-L135] |

UNVERIFIED: The legacy fatigue stress/CSV unit agreement, an explicit plate input-unit declaration, the plate stress sign convention and a complete coordinate/phase convention for each exported solver format are not established by this pack; these are prerequisites for qualified reuse, issue pending (4).

## 3. Method and governing standards

`FatigueAnalysis.get_cycles_to_failure` evaluates the curve power law; direct S-N and rainflow accumulation sum cycle count divided by allowable cycles, and time traces use the delegated counter before trace weighting. [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L52-L93] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L107-L151] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L249-L261]

`PlateBuckling.run_plate_DNV_rp_C201` calculates characteristic and elastic resistances, slenderness, serviceability and ultimate checks, plus directional and combined DNV usage factors; `get_plate_properties` rounds geometry before these calculations. [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L33-L130] [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L132-L165]

| Standard label in the package | Edition / clause evidence and implementing path | Citation-output status |
| --- | --- | --- |
| DNV-RP-F103, Cathodic Protection of Submarine Pipelines by Galvanic Anodes | October 2010, with an annex reference but no clause title, as documented by `DNV_RP_F103_2010`; attenuation helper separately names a 2016 approach, an unresolved basis inconsistency, issue pending (4) | Serialized citations accompany results. Clause text not checked against the licensed copy. [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L76-L88] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L131-L150] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L576-L598] |
| DNV-RP-B401, Cathodic Protection Design | Default edition 2021; Sec. 6.3, Sec. 7 and Tables 10-1/10-2/10-4/10-6/10-7/10-8 as named in the facade; edition resolver supports explicit edition selection; facade documents the default edition and design sections | Edition, provenance and citations are returned. Clause text not checked against the licensed copy. [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L174-L183] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L711-L746] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L749-L763] |
| ABS Guidance Notes on Cathodic Protection of Offshore Structures | December 2018; anode geometry/mass/current checks under the documented design section | Legacy `ABS_gn_offshore_2018` implementation; clause text not checked against the licensed copy. [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L811-L841] |
| ABS Guidance Notes on Cathodic Protection of Ships | December 2017; legacy route key retains a different year | Legacy key is documented as misnamed; delegation does not establish a different edition. Clause text not checked against the licensed copy. [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L17-L29] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1309-L1327] [docs/domains/cathodic_protection/examples/example-02-ship-hull-abs-gn-ships-2017.md:L8-L15] |
| DNV-RP-C203 | Configuration schema names this label, without edition/clause; the fatigue defaults separately select a 2005-labelled curve | No inference of equivalence is made between the schema label and the curve label, issue pending (4). Clause text not checked against the licensed copy. [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L60-L73] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L192-L210] |
| DNV-RP-C201 | Plate router and method docstring name the code without edition/clause | No citation sidecar is assembled in the result mapping, issue pending (4). Clause text not checked against the licensed copy. [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L9-L36] [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L122-L130] |
| ASTM G80, Standard Test Method for Specific Cathodic Disbonding of Pipeline Coatings | 1998; Sections 10.1 and 10.2 as named by the code | Licensed-source applicability UNVERIFIED. [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g80.py:L1-L20] |
| ASTM G42, Standard Test Method for Cathodic Disbonding of Pipeline Coatings Subjected to Elevated Temperatures | 1996; Sections 10 and 11 as named by the code | Licensed-source applicability UNVERIFIED. [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g42.py:L1-L24] |
| DNV-RP-H103, modelling label | Edition, title and clause not established here; 6D-buoy save_model writers are mapped | Licensed-source reconciliation remains issue pending (4); no edition is inferred. [docs/registry/implementation-map.yaml:L6384-L6423] |

UNVERIFIED: The C201 and C203 title, edition, clause and formula applicability cannot be established from the labels alone; licensed-source reconciliation and output provenance remain open, issue pending (4).

## 4. Validated worked examples and regression examples

Validation below applies only to each named function and synthetic/closed-form case; it does not qualify the legacy C201 plate route or a licensed fatigue curve. [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L37-L80] [tests/infrastructure/unit/test_structural_solvers_week2.py:L243-L302]

| Kind / case | Inputs and independent expected result | Tolerance and basis | Existing test node / offline result |
| --- | --- | --- | --- |
| Validated: fatigue arithmetic | Synthetic power-law coefficients and two stress/count bins; closed-form Miner sum `1.25e-6` dimensionless, not a standard S-N qualification | Relative `1e-12`, asserted by the test | `tests/specialized/ship_design/test_ship_fatigue_analysis_import.py::test_damage_from_rainflow_cycles_is_real_miners_sum`; PASS at BASE and AFTER [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L64-L80] |
| Validated: pinned column | Inline finite-element fixture; independent Euler expression `pi² E I / L²` in N | Relative `0.05` (5.0 %), asserted against first critical load | `tests/infrastructure/unit/test_structural_solvers_week2.py::TestElasticBucklingSolver::test_solve_column_buckling`; PASS at BASE and AFTER [tests/infrastructure/unit/test_structural_solvers_week2.py:L243-L247] [tests/infrastructure/unit/test_structural_solvers_week2.py:L290-L302] |
| Validated: pure shear | Inline stress input; expected `sqrt(3) * tau` Pa, from the closed-form stress invariant | Absolute error `<1e-6` Pa against the unrounded expression | `tests/infrastructure/unit/test_structural_solvers_week2.py::TestPostProcessingUtilities::test_compute_von_mises_shear`; PASS at BASE and AFTER [tests/infrastructure/unit/test_structural_solvers_week2.py:L433-L440] |
| Validated: annular area | Inline radii; independent circle-area subtraction, expected `pi * (15² - 5²)` mm² | Absolute error `<1e-6` mm² against the unrounded geometric expression | `tests/specialized/cathodic_protection/test_astm_g80.py::TestDisbondingArea::test_nominal_annular_area`; PASS at BASE and AFTER [tests/specialized/cathodic_protection/test_astm_g80.py:L23-L29] |
| Regression: cached-result isolation | Inline test solver result; mutating retrieved results must preserve the stored output | Exactly 0 unintended cache changes (count), checked by equality | `tests/infrastructure/unit/test_base_solvers_base.py::TestBaseSolver::test_results_isolation`; PASS at BASE and AFTER [tests/infrastructure/unit/test_base_solvers_base.py:L133-L141] |
| Regression: fatigue default schema | Default config exposes the expected dispatch keys; no numerical damage criterion | 0 missing required keys (count) | `tests/specialized/ship_design/test_ship_fatigue_analysis_import.py::test_default_sn_cfg_shape`; PASS at BASE and AFTER [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L91-L97] |
| Regression: durable fatigue route | Workflow input `examples/workflows/fatigue-analysis/input.yml`; the test pins damage to a stored numerical expectation | `pytest.approx` of the stored expectation, plus positive damage bound; no independent source established | `tests/workflows/test_durable_workflows.py::test_workflow_registry[fatigue-analysis]`; PASS at BASE and AFTER [docs/registry/workflows.yaml:L28-L35] [tests/workflows/test_durable_workflows.py:L109-L112] |
| Regression: durable plate route | Workflow input `examples/workflows/plate-buckling/input.yml`; stored directional/combined utilization expectations; this regression pins rounded values and does not validate unrounded resistances | `pytest.approx` of stored expectations, plus equivalent utilization `<1` (dimensionless) | `tests/workflows/test_durable_workflows.py::test_workflow_registry[plate-buckling]`; PASS at BASE and AFTER [docs/registry/workflows.yaml:L468-L475] [tests/workflows/test_durable_workflows.py:L784-L792] |

UNVERIFIED: The plate route has a known rounding defect: geometry ratios are rounded before resistance calculations; correction remains issue pending (2). Independent end-to-end qualification of the legacy plate utilization and default fatigue-curve lookup is not established by the worked examples; issue pending (6), with missing ship fixtures tracked in #785. [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L19-L25]

## 5. QA gates and executable checks

The applicable AQG r0 Python-calculations set is G0 input/units/citations, G1 imports/tests, G2 independent calculation; G5 QA-appendix generation is applicable through the package-output metadata count C8; G3 dynamics and G4 diffraction are `NOT_APPLICABLE` for these Python calculation routes (AQG r0, section 10, Python calculations; section 9, QA appendix).

| Gate / check | Module-specific execution and numeric acceptance criterion | Evidence / status |
| --- | --- | --- |
| G0 / C0 | `SolverConfigManager.validate_config`: invalid structural type must produce `False` and at least 1 error (count); unknown schema has exactly 0 keys (count), explicitly a sentinel rather than a qualified configuration | `tests/infrastructure/unit/test_solver_config.py::TestSolverConfigManager::test_validate_structural_invalid_type` and `tests/infrastructure/unit/test_solver_config.py::TestSolverConfigManager::test_get_schema_unknown_type`; PASS at BASE and AFTER [tests/infrastructure/unit/test_solver_config.py:L71-L103] |
| G0 / C1 | `disbonding_area`: zero holiday radius must raise exactly 1 `ValueError` (exception count), rather than compute an area | `tests/specialized/cathodic_protection/test_astm_g80.py::TestDisbondingArea::test_raises_on_zero_holiday_radius`; PASS at BASE and AFTER [tests/specialized/cathodic_protection/test_astm_g80.py:L42-L44] |
| G0 / C9 | `_b401_current_densities`: explicitly disabled splash-zone result equals 0 A/m²; this is a branch/sentinel check, not a reproduction of a licensed table | `tests/specialized/cathodic_protection/test_cathodic_protection_b401.py::TestB401CurrentDensities::test_current_density_splash_is_zero`; PASS at BASE and AFTER [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L376-L382] |
| G1 / C2 | Collect and execute the package-related offline suites; compare BASE/AFTER by node; require 0 newly failed nodes (count), and report existing failures individually | Execution records below; source tests exercise the framework and arithmetic. [tests/infrastructure/unit/test_base_solvers_base.py:L80-L141] [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L41-L80] |
| G1 / C3 | Ship module must expose all 3 required fatigue methods (method count), with 0 wrong-class bindings (count) | `tests/specialized/ship_design/test_ship_fatigue_analysis_import.py::test_ship_module_wires_the_correct_fatigue_class`; PASS at BASE and AFTER [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L41-L57] |
| G2 / C4 | `damage_from_rainflow_cycles` versus independent synthetic Miner sum; relative error `<=1e-12` (dimensionless) | Section-4 fatigue arithmetic node; PASS at BASE and AFTER [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L64-L80] |
| G2 / C5 | `ElasticBucklingSolver.solve`: first critical load versus Euler; relative error `<=0.05` (dimensionless), reported load in N | Section-4 column node; PASS at BASE and AFTER [tests/infrastructure/unit/test_structural_solvers_week2.py:L290-L302] |
| G2 / C6 | `compute_von_mises` pure shear versus `sqrt(3) * tau`; absolute error `<1e-6` Pa | Section-4 pure-shear node; PASS at BASE and AFTER [tests/infrastructure/unit/test_structural_solvers_week2.py:L433-L440] |
| G2 / C7 | `disbonding_area` versus independent annular geometry; absolute error `<1e-6` mm² | Section-4 annular-area node; PASS at BASE and AFTER [tests/specialized/cathodic_protection/test_astm_g80.py:L23-L29] |
| G3 | `NOT_APPLICABLE`: these routes produce fatigue damage and plate utilization, not time-domain dynamics | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L52-L139] [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L122-L130] |
| G4 | `NOT_APPLICABLE`: these routes do not solve diffraction | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L52-L139] [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L122-L130] |
| G5 / C8 | Execute the B401 facade and count results missing any of `standard`, `edition`, `provenance` and `citations`; require exactly 0 incomplete results (count). This checks package-output metadata, not independent numerical qualification. | Executable check below: PASS, 1 result and 0 incomplete results; independent sign-off remains issue pending (6). |

C8 package-output check (run with `PYTHONPATH=src` and pytest plus the declared calculation dependencies, from the repository root):

```python
from runpy import run_path

from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_B401_2021 import (
    _b401_offshore_platform,
)

fixture = run_path(
    "tests/specialized/cathodic_protection/test_cathodic_protection_b401.py"
)["b401_base_config"]
results = [_b401_offshore_platform(fixture.__wrapped__())["results"]]
required = {"standard", "edition", "provenance", "citations"}
missing = sum(not required.issubset(result) for result in results)
assert missing == 0, f"{missing} results lack required metadata"
print(f"C8: {len(results)} result, {missing} incomplete results")
```

The marine exporter and the DNV-RP-H103 6D-buoy `save_model` writers write OrcaFlex text; its program-specific G1 route would use the free demo at the target version where the trial permits the model, otherwise the licensed `LoadData` route, with 0 warnings/errors (count), and SHEAR7 input would require its target-version parser with 0 load errors (count). Both are `NOT_RUN`: the dependency inventory is read as text and neither interface is executed; version-qualified export acceptance remains issue pending (6). [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L791-L857] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L230-L238]

The benchmark suite carries an existing whole-module skip for stale API expectations (#949); this is not numerical validation and no section-4 case relies on it. [tests/test_solver_benchmarks.py:L11-L18]

### Offline execution record

The code pin for every run is `1dba289c5dc1bc8dc82d72e4bfe02f8dcfc22c23`; package code is unchanged. Counts below are per invocation, including skipped nodes, rather than distinct engineering qualification cases.

Generic matched form: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<isolated scratch>`. The full installed executable is used; no pytest addopts override is used. Isolated-copy commands additionally select the owning project with `--project <owning checkout>`; shared-harness/snippet commands use `--randomly-seed=0` in both phases.

| Suite | BASE: passed / failed / skipped / errors | AFTER: passed / failed / skipped / errors |
| --- | --- | --- |
| Framework / structural / coating geometry | 286 / 0 / 25 / 0 | 286 / 0 / 25 / 0 |
| CP / fatigue arithmetic | 327 / 0 / 0 / 0 | 327 / 0 / 0 / 0 |
| Isolated durable workflows | 2 / 0 / 0 / 0 | 2 / 0 / 0 / 0 |
| Legacy engine smoke consumers | 19 / 0 / 0 / 0 | 19 / 0 / 0 / 0 |
| Existing mocked engine harness | 1 / 60 / 1 / 0 | 1 / 60 / 1 / 0 |
| Isolated engine mocks | 4 / 0 / 0 / 0 | 4 / 0 / 0 / 0 |
| Fatigue smoke | 5 / 0 / 0 / 0 | 5 / 0 / 0 / 0 |
| Report provenance | 18 / 0 / 0 / 0 | 18 / 0 / 0 / 0 |
| Isolated documentation snippets | 20 / 0 / 1 / 0 | 20 / 0 / 1 / 0 |

The contract/routing command selects `tests/contracts` and `tests/docs/test_digitalmodel_routing_contract.py` with the same extra and isolation flags: 494 passed, 0 failed, 0 skipped, 0 errors. [tests/docs/test_digitalmodel_routing_contract.py:L97-L119]

The initial primary command without the test extra returned 286 passed and 25 existing skips. The initial CP command returned 323 passed and 4 missing-`benchmark`-fixture setup errors; the declared test extra resolved those errors and the matched CP run passed all 327 nodes. [pyproject.toml:L173-L186]

The initial combined shared-harness collection aborted on four files with `ModuleNotFoundError: No module named scipy.optimize; scipy is not a package`; global SciPy replacement is present at the code pin. Separate-process runs are recorded above. The mock harness still has 60 baseline failures and 1 existing disabled-feature skip; these are held by #1277, while cross-file interference is covered by #2312. No module code or test is changed. [tests/test_engine.py:L34-L88] [tests/test_engine.py:L258-L281]

NOT_RUN: target-version OrcaFlex and SHEAR7 load/solver checks are excluded because they require their program/interface; no licensed program is imported or run. Numerical test nodes in section 4 are executed offline. [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L791-L857] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L230-L238]

### Every non-passing native test node

Baseline failed nodes belong to the existing mocked engine harness (#1277); their function bodies and global replacement setup remain at the code pin.

| Native node | Result / disposition | Pinned source |
| --- | --- | --- |
| `tests/test_engine.py::TestAllModuleBasenames::test_module_routing_parametrized[cathodic_protection-run_cathodic_protection]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L576-L616] |
| `tests/test_engine.py::TestAllModuleBasenames::test_module_routing_parametrized[copy_and_paste-CopyAndPasteFiles]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L576-L616] |
| `tests/test_engine.py::TestAllModuleBasenames::test_module_routing_parametrized[pipe_capacity-PipeCapacity]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L576-L616] |
| `tests/test_engine.py::TestAllModuleBasenames::test_module_routing_parametrized[plate_buckling-PlateBuckling]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L576-L616] |
| `tests/test_engine.py::TestAllModuleBasenames::test_module_routing_parametrized[rao_analysis-RAOAnalysis]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L576-L616] |
| `tests/test_engine.py::TestAllModuleBasenames::test_module_routing_parametrized[ship_design-ShipDesign]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L576-L616] |
| `tests/test_engine.py::TestAllModuleBasenames::test_module_routing_parametrized[ship_design_aqwa-ShipDesign]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L576-L616] |
| `tests/test_engine.py::TestAllModuleBasenames::test_module_routing_parametrized[time_series-TimeSeriesAnalysis]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L576-L616] |
| `tests/test_engine.py::TestAllModuleBasenames::test_module_routing_parametrized[transformation-Transformation]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L576-L616] |
| `tests/test_engine.py::TestAllModuleBasenames::test_module_routing_parametrized[viv_analysis-VIVAnalysis]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L576-L616] |
| `tests/test_engine.py::TestComplexModuleRouting::test_code_dnvrph103_circular` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L426-L444] |
| `tests/test_engine.py::TestComplexModuleRouting::test_code_dnvrph103_rectangular` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L406-L424] |
| `tests/test_engine.py::TestComplexModuleRouting::test_installation_with_structure_flag` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L446-L464] |
| `tests/test_engine.py::TestComplexModuleRouting::test_installation_without_structure_flag` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L466-L484] |
| `tests/test_engine.py::TestComplexModuleRouting::test_orcaflex_file_management_basenames` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L486-L504] |
| `tests/test_engine.py::TestConfigurationManagement::test_config_dir_path_preservation` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L727-L761] |
| `tests/test_engine.py::TestConfigurationManagement::test_library_name_and_basename_usage` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L763-L788] |
| `tests/test_engine.py::TestDynamicImports::test_catenary_dynamic_import` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L538-L554] |
| `tests/test_engine.py::TestDynamicImports::test_rigging_dynamic_import` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L510-L536] |
| `tests/test_engine.py::TestEngineComprehensiveIntegration::test_engine_error_recovery` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1319-L1334] |
| `tests/test_engine.py::TestEngineComprehensiveIntegration::test_engine_full_workflow_with_file_config` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1268-L1316] |
| `tests/test_engine.py::TestEngineComprehensiveIntegration::test_engine_multiple_module_workflow` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1337-L1369] |
| `tests/test_engine.py::TestEngineConfiguration::test_basename_extraction_from_config` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L164-L192] |
| `tests/test_engine.py::TestEngineConfiguration::test_engine_config_file_path_tracking` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L129-L162] |
| `tests/test_engine.py::TestEngineConfiguration::test_engine_with_none_config_requires_inputfile` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L106-L109] |
| `tests/test_engine.py::TestEngineConfiguration::test_engine_with_provided_config` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L111-L127] |
| `tests/test_engine.py::TestEngineConfiguration::test_missing_basename_raises_error` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L194-L199] |
| `tests/test_engine.py::TestEngineConfiguration::test_output_control_settings` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L201-L242] |
| `tests/test_engine.py::TestEngineEdgeCases::test_engine_code_dnvrph103_without_shape` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1076-L1088] |
| `tests/test_engine.py::TestEngineEdgeCases::test_engine_file_not_found` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1102-L1111] |
| `tests/test_engine.py::TestEngineEdgeCases::test_engine_installation_without_structure_flag` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1056-L1074] |
| `tests/test_engine.py::TestEngineEdgeCases::test_engine_with_empty_config` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L988-L991] |
| `tests/test_engine.py::TestEngineEdgeCases::test_engine_with_extremely_large_config` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1113-L1133] |
| `tests/test_engine.py::TestEngineEdgeCases::test_engine_with_malformed_yaml_input` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1090-L1100] |
| `tests/test_engine.py::TestEngineEdgeCases::test_engine_with_nested_meta_basename` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1016-L1040] |
| `tests/test_engine.py::TestEngineEdgeCases::test_engine_with_none_values` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L993-L1014] |
| `tests/test_engine.py::TestEngineEdgeCases::test_engine_with_special_characters_in_basename` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1042-L1054] |
| `tests/test_engine.py::TestEnginePerformance::test_engine_memory_usage` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L963-L982] |
| `tests/test_engine.py::TestEnginePerformance::test_engine_performance_with_large_configs[1000]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L939-L960] |
| `tests/test_engine.py::TestEnginePerformance::test_engine_performance_with_large_configs[100]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L939-L960] |
| `tests/test_engine.py::TestEnginePerformance::test_engine_performance_with_large_configs[10]` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L939-L960] |
| `tests/test_engine.py::TestEnginePropertyBased::test_engine_batch_processing_property` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L903-L931] |
| `tests/test_engine.py::TestEnginePropertyBased::test_engine_handles_various_basename_strings` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L837-L857] |
| `tests/test_engine.py::TestEnginePropertyBased::test_engine_handles_various_config_structures` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L872-L890] |
| `tests/test_engine.py::TestEngineSecurityAspects::test_engine_handles_malicious_input_safely` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1140-L1156] |
| `tests/test_engine.py::TestEngineSecurityAspects::test_engine_path_traversal_protection` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1159-L1179] |
| `tests/test_engine.py::TestEngineSecurityAspects::test_engine_prevents_code_injection` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L1182-L1202] |
| `tests/test_engine.py::TestEngineStatefulCase::runTest` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L34-L88] |
| `tests/test_engine.py::TestErrorHandling::test_config_application_manager_integration` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L697-L721] |
| `tests/test_engine.py::TestErrorHandling::test_none_config_validation` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L682-L695] |
| `tests/test_engine.py::TestErrorHandling::test_unsupported_basename_raises_exception` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L674-L680] |
| `tests/test_engine.py::TestModuleRouting::test_aqwa_routing` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L306-L321] |
| `tests/test_engine.py::TestModuleRouting::test_catenary_routing` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L248-L264] |
| `tests/test_engine.py::TestModuleRouting::test_fatigue_analysis_routing` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L357-L372] |
| `tests/test_engine.py::TestModuleRouting::test_modal_analysis_routing` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L323-L338] |
| `tests/test_engine.py::TestModuleRouting::test_mooring_routing` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L391-L400] |
| `tests/test_engine.py::TestModuleRouting::test_orcaflex_routing` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L286-L304] |
| `tests/test_engine.py::TestModuleRouting::test_pipeline_routing` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L374-L389] |
| `tests/test_engine.py::TestModuleRouting::test_umbilical_analysis_routing` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L340-L355] |
| `tests/test_engine.py::TestRegistryRoutingRegression::test_gis_basename_routes_to_gis_module_not_time_series` | FAIL at BASE and AFTER; #1277 | [tests/test_engine.py:L630-L652] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_solver_initialization_benchmark` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L167-L175] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_solver_validation_benchmark` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L177-L184] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_solver_execution_benchmark` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L186-L193] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_complex_solver_execution` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L195-L201] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_solver_results_caching` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L203-L210] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_solver_metadata_benchmark` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L212-L218] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_input_data_handling_benchmark` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L220-L227] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_validation_error_handling` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L229-L235] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_solver_scaling_benchmark` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L237-L250] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_concurrent_solvers_benchmark` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L252-L263] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_benchmark_result_threshold_validation` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L265-L291] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_benchmark_baseline_comparison` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L293-L324] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_benchmark_aggregator` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L326-L354] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_benchmark_export_json` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L356-L381] |
| `tests/test_solver_benchmarks.py::TestSolverBenchmarks::test_benchmark_export_csv` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L383-L410] |
| `tests/test_solver_benchmarks.py::TestConfigurationBenchmarks::test_config_benchmarks_initialization` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L421-L425] |
| `tests/test_solver_benchmarks.py::TestConfigurationBenchmarks::test_yaml_loading_benchmarks` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L427-L436] |
| `tests/test_solver_benchmarks.py::TestConfigurationBenchmarks::test_config_loader_benchmark` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L438-L444] |
| `tests/test_solver_benchmarks.py::TestConfigurationBenchmarks::test_config_manager_operations` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L446-L454] |
| `tests/test_solver_benchmarks.py::TestConfigurationBenchmarks::test_nested_config_access` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L456-L462] |
| `tests/test_solver_benchmarks.py::TestConfigurationBenchmarks::test_config_benchmarks_summary` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L464-L474] |
| `tests/test_solver_benchmarks.py::TestConfigurationBenchmarks::test_config_benchmarks_cleanup` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L476-L479] |
| `tests/test_solver_benchmarks.py::TestBenchmarkReportGenerator::test_report_generator_initialization` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L490-L494] |
| `tests/test_solver_benchmarks.py::TestBenchmarkReportGenerator::test_html_report_generation` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L496-L536] |
| `tests/test_solver_benchmarks.py::TestBenchmarkReportGenerator::test_report_with_baseline_data` | SKIP: existing stale benchmark API marker; #949 | [tests/test_solver_benchmarks.py:L538-L576] |
| `tests/test_engine.py::TestModuleRouting::test_vertical_riser_routing` | SKIP: vertical-riser import disabled in the existing harness; #1277 | [tests/test_engine.py:L258-L281] |
| `tests/cathodic_protection/test_worked_examples.py::test_worked_example_block_executes[calc-002-dnv-f103-2010-anode-depth-salinity.md#1]` | SKIP: existing not-runnable marker, resistivity helper pending; issue pending (6) | [tests/cathodic_protection/test_worked_examples.py:L83-L104] |


## 6. Known failure modes and silent-failure sweep

sweep: 504 found (candidate source lines: 262 non-failures, 238 requiring review, 4 explicit failure outcomes; not a count of proven defects).

The package screen and AST pass cover 54 Python files and 18,439 lines; source-line candidates, defaults, conversions, unit assumptions and cache operations are individually indexed below. All source references refer to the header code pin.

Exact lexical commands used:

```sh
rg -n --glob '*.py' 'except|\bcontinue\b|return None|return \[\]|return \{\}' src/digitalmodel/infrastructure/base_solvers
rg -n --glob '*.py' 'except|\bcontinue\b|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5 ?%|\b(int|float)\(|units?|cache' src/digitalmodel/infrastructure/base_solvers
```

The narrow screen returns 55 matching lines and the broad screen 404; the AST extension additionally catches multiline/default/conditional sites. The AST inventory includes 44 exception handlers, 4 continues, 7 empty/None returns and 124 numeric conversions; overlapping categories are not summed as distinct defects.

| Priority finding / pitfall | Detection and disposition | Source |
| --- | --- | --- |
| Fatigue/VIV frequency lookup failures can substitute an artificial frequency; missing/partial records can survive | no detection check exists; issue pending (1) | Per-occurrence rows below |
| F103 invalid geometry can become zero resistance; attenuation/reach fallbacks can become zero | no detection check exists; issue pending (2) | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L208-L215] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L631-L728] |
| Enhanced attenuation subtracts unity from an upstream final coating factor clamped at unity | no detection check exists; issue pending (2) | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L293-L295] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L685-L698] |
| Initial potential check is unconditional PASS; final potential check follows current margin rather than a calculated potential | no detection check exists; issue pending (2) | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1728-L1737] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1812-L1824] |
| Zero throughput bypasses the truthiness-guarded minimum; absent report metrics become zero | no detection check exists; issue pending (3) | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L85-L95] [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L121-L133] |
| Legacy plate geometry and ratios are rounded before resistance calculations | Known result defect: the registered example has unrounded `t/s = 0.0150`, stored as `0.01`; the squared-ratio multiplier is 44.4 % of the unrounded value, understating the ratio-squared elastic resistances by approximately 55.6 %. No detection check exists for an unrounded resistance bound; code correction remains issue pending (2) | [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L132-L165] |
| Rectangular buoy inertia reference is unresolved before exported-model reuse (#2135) | no detection check exists; the inertia origin shift and separately exported centre require reconciliation | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L401-L430] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L122-L138] |
| Unsupported fatigue calculation types return the configuration unchanged; default thickness is not consumed by the S-N accumulation shown here | no detection check exists for the silent dispatch; issue pending (1); thickness-basis qualification is issue pending (4) | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L37-L70] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L192-L210] |
| Historical wrong fatigue-class import (#776) is covered by binding and real arithmetic checks; end-to-end missing fixtures remain #785 | C3 catches binding regression; C4 checks synthetic damage, not missing fixtures; no detection check exists for full fixture readiness | [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L1-L25] [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L41-L80] |
| Historical F103 calibration (#573) is not evidence of current licensed-source qualification | no detection check exists for independent licensed-source agreement in this pack; issue pending (4) | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L76-L88] |
| Historical stale namespace (#756) and general audits (#1817, #940) are discovery leads rather than new incidents | no detection check exists for whole-package import portability; issue pending (6) | [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L1-L20] [src/digitalmodel/infrastructure/base_solvers/interfaces.py:L1-L20] |

Shared engine harnesses replace modules globally before collection, so combined invocation can fail on `scipy.optimize` even when the separately invoked calculation routes pass; this is a test-harness limitation, not an independent numerical check. No detection check exists for cross-file mock isolation; #1277 and #2312. [tests/test_engine.py:L34-L81]

### Per-occurrence classification

| Row | Category | Symptom or classification | Detection | Evidence |
| --- | --- | --- | --- | --- |
| 1 | cache API | Cache contract or copy operation; no empty-on-error cache write established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/base.py:L107-L113] |
| 2 | cache API | Cache contract or copy operation; no empty-on-error cache write established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/base.py:L124-L130] |
| 3 | cache API | Cache contract or copy operation; no empty-on-error cache write established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/base.py:L126-L132] |
| 4 | cache API | Cache contract or copy operation; no empty-on-error cache write established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/base.py:L129-L135] |
| 5 | cache API | Cache contract or copy operation; no empty-on-error cache write established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/base.py:L132-L138] |
| 6 | except | Error is translated and raised; no swallowed error. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/base.py:L164-L171] |
| 7 | throughput truthiness | A zero throughput value bypasses the minimum-throughput comparison. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L90-L98] |
| 8 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L97-L103] |
| 9 | empty return | Absent or invalid input returns an empty mapping or None; caller must inspect the sentinel. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L107-L113] |
| 10 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L164-L170] |
| 11 | ignored warmup | Warmup failure is logged but measured iterations continue. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L221-L227] |
| 12 | explicit failure | Measured-iteration error is logged and an error result is returned. | explicit failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L232-L238] |
| 13 | empty summary | No benchmark results yields a zero-count summary; consumers must distinguish missing runs from measured results. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L382-L388] |
| 14 | conditional zero | An unavailable or nonpositive branch yields zero; missing/invalid and genuine zero results may be indistinguishable. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L396-L402] |
| 15 | conditional zero | An unavailable or nonpositive branch yields zero; missing/invalid and genuine zero results may be indistinguishable. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L397-L403] |
| 16 | conditional zero | An unavailable or nonpositive branch yields zero; missing/invalid and genuine zero results may be indistinguishable. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L399-L405] |
| 17 | conditional zero | An unavailable or nonpositive branch yields zero; missing/invalid and genuine zero results may be indistinguishable. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L401-L407] |
| 18 | conditional zero | An unavailable or nonpositive branch yields zero; missing/invalid and genuine zero results may be indistinguishable. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L402-L408] |
| 19 | placeholder catch | Placeholder orchestration catches and logs errors; the current try body performs no calculation. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L573-L579] |
| 20 | except | Exception is consumed; execution returns or continues without propagating it. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L14-L20] |
| 21 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L121-L127] |
| 22 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L122-L128] |
| 23 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L123-L129] |
| 24 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L124-L130] |
| 25 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L127-L133] |
| 26 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L128-L134] |
| 27 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L129-L135] |
| 28 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L130-L136] |
| 29 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L161-L167] |
| 30 | conditional zero | An unavailable or nonpositive branch yields zero; missing/invalid and genuine zero results may be indistinguishable. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L162-L168] |
| 31 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L188-L194] |
| 32 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L221-L227] |
| 33 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L222-L228] |
| 34 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L247-L253] |
| 35 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L248-L254] |
| 36 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L283-L289] |
| 37 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L287-L293] |
| 38 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L437-L444] |
| 39 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L441-L448] |
| 40 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L445-L452] |
| 41 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L449-L460] |
| 42 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L517-L523] |
| 43 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L518-L524] |
| 44 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L544-L550] |
| 45 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L545-L551] |
| 46 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L546-L552] |
| 47 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L547-L553] |
| 48 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L548-L554] |
| 49 | zero metric | Missing benchmark counts, metric or comparison values are displayed or plotted as zero. | candidate; issue pending (3); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L549-L557] |
| 50 | cache API | Cache contract or copy operation; no empty-on-error cache write established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/solver_benchmarks.py:L211-L217] |
| 51 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/benchmarks/solver_benchmarks.py:L220-L226] |
| 52 | zero schema default | Schema supplies zero for an omitted environmental input; measured zero and missing input are not distinguished. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L35-L41] |
| 53 | zero schema default | Schema supplies zero for an omitted environmental input; measured zero and missing input are not distinguished. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L36-L42] |
| 54 | zero schema default | Schema supplies zero for an omitted environmental input; measured zero and missing input are not distinguished. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L37-L43] |
| 55 | continue | Iteration continues after recording a type error. | non-failure; C0 checks failure and diagnostics | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L117-L123] |
| 56 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L133-L139] |
| 57 | empty return | Absent or invalid input returns an empty mapping or None; caller must inspect the sentinel. | candidate; issue pending (1); C0 checks the sentinel | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L147-L153] |
| 58 | except | Exception is consumed; execution returns or continues without propagating it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L211-L217] |
| 59 | except | Exception is consumed; execution returns or continues without propagating it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L220-L226] |
| 60 | except | Exception is consumed; execution returns or continues without propagating it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L236-L242] |
| 61 | empty return | Absent or invalid input returns an empty mapping or None; caller must inspect the sentinel. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L238-L244] |
| 62 | except | Exception is consumed; execution returns or continues without propagating it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L245-L250] |
| 63 | empty return | Absent or invalid input returns an empty mapping or None; caller must inspect the sentinel. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L247-L250] |
| 64 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L60-L66] |
| 65 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L82-L88] |
| 66 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L104-L110] |
| 67 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L119-L125] |
| 68 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L120-L126] |
| 69 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L130-L136] |
| 70 | swallowed parse | Mode parsing or dominant-mode lookup failure is printed, then an empty or partial frame is retained. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L61-L67] |
| 71 | empty result | Empty solver-output file creates an empty frame that later aggregation can skip. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L89-L96] |
| 72 | frequency fallback | Any frequency lookup error is consumed and a small artificial frequency is substituted. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L382-L388] |
| 73 | frequency fallback | Artificial frequency substitutes for an unavailable modal result. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L383-L389] |
| 74 | unit assumption | Cycle conversion assumes an annual duration and frequency/time convention. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L404-L410] |
| 75 | unit assumption | Stress interpolation is rescaled without a quantity-aware unit check. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L415-L421] |
| 76 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L523-L529] |
| 77 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L49-L55] |
| 78 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L50-L56] |
| 79 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L96-L102] |
| 80 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L162-L168] |
| 81 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L174-L180] |
| 82 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L175-L181] |
| 83 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L176-L182] |
| 84 | zero default | Missing coating thickness is assumed absent. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L177-L183] |
| 85 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L181-L187] |
| 86 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L204-L210] |
| 87 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L206-L212] |
| 88 | zero fallback | Nonpositive geometry denominator yields zero metallic resistance. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L212-L218] |
| 89 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L277-L283] |
| 90 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L278-L284] |
| 91 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L316-L322] |
| 92 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L356-L362] |
| 93 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L357-L363] |
| 94 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L404-L410] |
| 95 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L431-L437] |
| 96 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L471-L477] |
| 97 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L472-L478] |
| 98 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L473-L479] |
| 99 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L498-L504] |
| 100 | zero fallback | One or fewer anodes yields zero spacing; no missing-data diagnosis. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L546-L552] |
| 101 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L558-L564] |
| 102 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L559-L565] |
| 103 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L610-L616] |
| 104 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L613-L619] |
| 105 | zero fallback | Nonpositive inner diameter yields zero logarithmic term. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L633-L639] |
| 106 | zero fallback | Nonpositive attenuation denominator yields zero attenuation length. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L643-L649] |
| 107 | zero fallback | Nonpositive current density yields zero polarization resistance. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L664-L670] |
| 108 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L673-L679] |
| 109 | derived-factor mismatch | Final coating fraction is reduced by one despite upstream factor clamping; positive enhanced coefficient is not established. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L682-L688] |
| 110 | zero fallback | Nonpositive enhanced numerator yields zero enhanced attenuation coefficient. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L695-L701] |
| 111 | zero fallback | Nonpositive attenuation length yields zero potential decay factor. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L711-L717] |
| 112 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L716-L722] |
| 113 | zero fallback | Nonpositive attenuation length yields zero protection reach. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L725-L731] |
| 114 | external numeric; engineering default | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L842-L848] |
| 115 | zero default | Missing structure surface area yields zero area and zero mass requirement. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L845-L851] |
| 116 | external numeric; engineering default | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L846-L852] |
| 117 | external numeric; engineering default | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L882-L888] |
| 118 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L953-L959] |
| 119 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L954-L960] |
| 120 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L955-L961] |
| 121 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L971-L977] |
| 122 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L973-L979] |
| 123 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L974-L980] |
| 124 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L977-L983] |
| 125 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L978-L984] |
| 126 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1036-L1042] |
| 127 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1037-L1043] |
| 128 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1038-L1044] |
| 129 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1039-L1045] |
| 130 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1060-L1066] |
| 131 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1061-L1067] |
| 132 | zero default | Missing protected-area keys yield zero area and zero demand. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1077-L1083] |
| 133 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1078-L1084] |
| 134 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1118-L1124] |
| 135 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1121-L1127] |
| 136 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1154-L1160] |
| 137 | empty return | Absent or invalid input returns an empty mapping or None; caller must inspect the sentinel. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1207-L1213] |
| 138 | empty return | Absent or invalid input returns an empty mapping or None; caller must inspect the sentinel. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1223-L1229] |
| 139 | empty return | Absent or invalid input returns an empty mapping or None; caller must inspect the sentinel. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1227-L1233] |
| 140 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1228-L1234] |
| 141 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1252-L1258] |
| 142 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1293-L1299] |
| 143 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1393-L1399] |
| 144 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1414-L1420] |
| 145 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1440-L1446] |
| 146 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1443-L1449] |
| 147 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1450-L1456] |
| 148 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1452-L1458] |
| 149 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1453-L1459] |
| 150 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1468-L1474] |
| 151 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1508-L1514] |
| 152 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1509-L1515] |
| 153 | zero default | First protected-area input defaults to zero; subsequent fallback or error exists. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1531-L1537] |
| 154 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1533-L1539] |
| 155 | zero default | Alternative protected-area input defaults to zero, then raises if zero. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1536-L1542] |
| 156 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1538-L1545] |
| 157 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1545-L1551] |
| 158 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1553-L1559] |
| 159 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1554-L1560] |
| 160 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1556-L1562] |
| 161 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1558-L1564] |
| 162 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1561-L1567] |
| 163 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1631-L1637] |
| 164 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1642-L1648] |
| 165 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1650-L1656] |
| 166 | zero fallback | Nonpositive available current yields zero utilization percentage. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1725-L1731] |
| 167 | assumed pass | Protection-potential check is set to PASS without calculating potential. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1734-L1740] |
| 168 | percentage fallback | Nonpositive final available current yields a saturated utilization percentage. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1808-L1814] |
| 169 | proxy check | Potential status is determined by current margin without calculating potential. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1819-L1825] |
| 170 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L116-L122] |
| 171 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L117-L123] |
| 172 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L118-L124] |
| 173 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L166-L172] |
| 174 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L167-L173] |
| 175 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L168-L174] |
| 176 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L258-L264] |
| 177 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L415-L425] |
| 178 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L418-L424] |
| 179 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L419-L425] |
| 180 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L562-L566] |
| 181 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L113-L119] |
| 182 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L114-L120] |
| 183 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L115-L121] |
| 184 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L163-L169] |
| 185 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L164-L170] |
| 186 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L165-L171] |
| 187 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L244-L250] |
| 188 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L385-L395] |
| 189 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L388-L394] |
| 190 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L389-L395] |
| 191 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L542-L546] |
| 192 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L74-L80] |
| 193 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L75-L81] |
| 194 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L76-L82] |
| 195 | except | Error is translated and raised; no swallowed error. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L142-L149] |
| 196 | except | Error is translated and raised; no swallowed error. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L153-L160] |
| 197 | external numeric; zero default | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. Absent input or metric is replaced by zero; missing and measured-zero values may be indistinguishable. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L185-L191] |
| 198 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L187-L193] |
| 199 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L218-L224] |
| 200 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L223-L229] |
| 201 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L225-L231] |
| 202 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L228-L234] |
| 203 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L229-L235] |
| 204 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L263-L269] |
| 205 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L264-L270] |
| 206 | external numeric; engineering default | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L331-L337] |
| 207 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; C9 checks the intentional zone-zero result | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L376-L382] |
| 208 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L418-L424] |
| 209 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L419-L425] |
| 210 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L420-L426] |
| 211 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L440-L446] |
| 212 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L441-L447] |
| 213 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L442-L448] |
| 214 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L443-L449] |
| 215 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L444-L450] |
| 216 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L445-L451] |
| 217 | external numeric; engineering default | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L488-L494] |
| 218 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L490-L496] |
| 219 | external numeric; engineering default | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L493-L499] |
| 220 | external numeric; engineering default | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L494-L500] |
| 221 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L497-L503] |
| 222 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L499-L505] |
| 223 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L507-L513] |
| 224 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L514-L520] |
| 225 | external numeric; engineering default | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L543-L549] |
| 226 | external numeric; engineering default | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L547-L553] |
| 227 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L563-L569] |
| 228 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L566-L572] |
| 229 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L568-L574] |
| 230 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L570-L576] |
| 231 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L573-L579] |
| 232 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L627-L633] |
| 233 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L632-L638] |
| 234 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L633-L640] |
| 235 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L638-L644] |
| 236 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L639-L646] |
| 237 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L651-L659] |
| 238 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L659-L665] |
| 239 | engineering default | An omitted engineering or reporting parameter receives a numeric default; source applicability requires explicit review. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L718-L724] |
| 240 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g42.py:L92-L98] |
| 241 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g42.py:L136-L142] |
| 242 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g42.py:L145-L151] |
| 243 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g80.py:L59-L65] |
| 244 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g80.py:L89-L95] |
| 245 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g80.py:L121-L127] |
| 246 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L13-L19] |
| 247 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L27-L33] |
| 248 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L33-L39] |
| 249 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L35-L41] |
| 250 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L71-L77] |
| 251 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (2); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L95-L101] |
| 252 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L163-L169] |
| 253 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L178-L184] |
| 254 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L213-L221] |
| 255 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L245-L251] |
| 256 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L247-L253] |
| 257 | cache API | Cache contract or copy operation; no empty-on-error cache write established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/interfaces.py:L52-L58] |
| 258 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L1-L6] |
| 259 | unit convention | Input is explicitly interpreted in a configured source unit before conversion; correctness still depends on the source-unit assumption. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L35-L41] |
| 260 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L137-L143] |
| 261 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L138-L144] |
| 262 | unit convention | Input is explicitly interpreted in a configured source unit before conversion; correctness still depends on the source-unit assumption. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L498-L504] |
| 263 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L534-L540] |
| 264 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L556-L562] |
| 265 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L559-L565] |
| 266 | unit convention | Input is explicitly interpreted in a configured source unit before conversion; correctness still depends on the source-unit assumption. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L587-L593] |
| 267 | unit convention | Input is explicitly interpreted in a configured source unit before conversion; correctness still depends on the source-unit assumption. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L588-L594] |
| 268 | unit convention | Input is explicitly interpreted in a configured source unit before conversion; correctness still depends on the source-unit assumption. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L589-L595] |
| 269 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L591-L597] |
| 270 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L614-L620] |
| 271 | unit convention | Input is explicitly interpreted in a configured source unit before conversion; correctness still depends on the source-unit assumption. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L621-L627] |
| 272 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L630-L636] |
| 273 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L637-L643] |
| 274 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L679-L685] |
| 275 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L696-L702] |
| 276 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L712-L718] |
| 277 | unit convention | Input is explicitly interpreted in a configured source unit before conversion; correctness still depends on the source-unit assumption. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L879-L885] |
| 278 | unit convention | Input is explicitly interpreted in a configured source unit before conversion; correctness still depends on the source-unit assumption. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L903-L909] |
| 279 | zero default | Absent input or metric is replaced by zero; missing and measured-zero values may be indistinguishable. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/pipe_properties.py:L274-L280] |
| 280 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/pipe_properties.py:L281-L287] |
| 281 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L83-L89] |
| 282 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L119-L125] |
| 283 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L167-L173] |
| 284 | explicit failure | Stress extraction error is logged and an explicit failure-status mapping is returned. | explicit failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L508-L517] |
| 285 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L657-L663] |
| 286 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L658-L664] |
| 287 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L659-L665] |
| 288 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L672-L678] |
| 289 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L691-L697] |
| 290 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L692-L698] |
| 291 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L693-L699] |
| 292 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L698-L704] |
| 293 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L699-L705] |
| 294 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L700-L706] |
| 295 | swallowed conversion | Failed float conversion leaves the original column values in the frame. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L738-L744] |
| 296 | swallowed conversion | Failed float conversion leaves the original column values in the frame. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L784-L790] |
| 297 | partial transform | Failed transformation is logged and the partially transformed frame is returned. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L887-L893] |
| 298 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L1-L4] |
| 299 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L27-L33] |
| 300 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L28-L34] |
| 301 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L29-L35] |
| 302 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L30-L36] |
| 303 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L54-L60] |
| 304 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L55-L61] |
| 305 | unit convention | Input is explicitly interpreted in a configured source unit before conversion; correctness still depends on the source-unit assumption. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L69-L77] |
| 306 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L90-L96] |
| 307 | unit convention | Input is explicitly interpreted in a configured source unit before conversion; correctness still depends on the source-unit assumption. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L111-L117] |
| 308 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L143-L149] |
| 309 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L145-L151] |
| 310 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L190-L196] |
| 311 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L199-L205] |
| 312 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L203-L209] |
| 313 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L307-L313] |
| 314 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L323-L329] |
| 315 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L353-L359] |
| 316 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L356-L362] |
| 317 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L373-L379] |
| 318 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L376-L382] |
| 319 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L388-L394] |
| 320 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L391-L397] |
| 321 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L400-L406] |
| 322 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L403-L409] |
| 323 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L464-L470] |
| 324 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L466-L472] |
| 325 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L467-L474] |
| 326 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L497-L503] |
| 327 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L501-L507] |
| 328 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L531-L537] |
| 329 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L532-L538] |
| 330 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/pipeline_solvers/math_solvers.py:L22-L28] |
| 331 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/pipeline_solvers/math_solvers.py:L31-L37] |
| 332 | except | Exception is consumed; execution returns or continues without propagating it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/__init__.py:L16-L22] |
| 333 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L56-L62] |
| 334 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L71-L77] |
| 335 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L147-L153] |
| 336 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L149-L155] |
| 337 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L191-L197] |
| 338 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L198-L204] |
| 339 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L205-L211] |
| 340 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L206-L212] |
| 341 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L208-L214] |
| 342 | except | Error is translated and raised; no swallowed error. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L229-L235] |
| 343 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L283-L289] |
| 344 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L23-L29] |
| 345 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L71-L77] |
| 346 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L75-L81] |
| 347 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L79-L85] |
| 348 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L83-L89] |
| 349 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L87-L93] |
| 350 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L95-L101] |
| 351 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L130-L136] |
| 352 | conditional zero | An unavailable or nonpositive branch yields zero; missing/invalid and genuine zero results may be indistinguishable. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L141-L147] |
| 353 | conditional zero | An unavailable or nonpositive branch yields zero; missing/invalid and genuine zero results may be indistinguishable. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L149-L155] |
| 354 | except | Error is logged and returned with an explicit failure status. | explicit failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L155-L163] |
| 355 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L216-L222] |
| 356 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L286-L292] |
| 357 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L297-L303] |
| 358 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L302-L308] |
| 359 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L336-L342] |
| 360 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L377-L383] |
| 361 | except | Error is translated and raised; no swallowed error. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L385-L391] |
| 362 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L403-L409] |
| 363 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L421-L427] |
| 364 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L433-L439] |
| 365 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/config.py:L36-L42] |
| 366 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/config.py:L51-L57] |
| 367 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/config.py:L59-L65] |
| 368 | continue | Missing configuration entry is skipped. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/config.py:L177-L183] |
| 369 | continue | Iteration continues after recording a type error. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/config.py:L188-L194] |
| 370 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/config.py:L209-L215] |
| 371 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/elements/beam_element.py:L56-L62] |
| 372 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/elements/beam_element.py:L57-L63] |
| 373 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/elements/beam_element.py:L58-L64] |
| 374 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/elements/beam_element.py:L244-L250] |
| 375 | except | Exception is consumed; execution returns or continues without propagating it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/fea/__init__.py:L6-L10] |
| 376 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L720-L726] |
| 377 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L40-L46] |
| 378 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L41-L47] |
| 379 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L53-L59] |
| 380 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L57-L63] |
| 381 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L61-L67] |
| 382 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L65-L71] |
| 383 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L69-L75] |
| 384 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L72-L78] |
| 385 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L75-L81] |
| 386 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L79-L85] |
| 387 | except | Error is logged and returned with an explicit failure status. | explicit failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L161-L169] |
| 388 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L199-L205] |
| 389 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L214-L220] |
| 390 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L215-L221] |
| 391 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L216-L222] |
| 392 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L217-L223] |
| 393 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L218-L224] |
| 394 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L236-L242] |
| 395 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L248-L254] |
| 396 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L316-L322] |
| 397 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/matrix_operations.py:L76-L82] |
| 398 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/matrix_operations.py:L92-L98] |
| 399 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/matrix_operations.py:L93-L99] |
| 400 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/matrix_operations.py:L99-L105] |
| 401 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/matrix_operations.py:L121-L127] |
| 402 | except | Exception is consumed; execution returns or continues without propagating it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/matrix_operations.py:L126-L131] |
| 403 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/matrix_operations.py:L128-L131] |
| 404 | zero argument default | An omitted optional argument receives zero; the caller must confirm that origin/depth convention. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L13-L19] |
| 405 | zero argument default | An omitted optional argument receives zero; the caller must confirm that origin/depth convention. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L14-L20] |
| 406 | zero argument default | An omitted optional argument receives zero; the caller must confirm that origin/depth convention. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L15-L21] |
| 407 | zero argument default | An omitted optional argument receives zero; the caller must confirm that origin/depth convention. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L16-L22] |
| 408 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L23-L29] |
| 409 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L48-L54] |
| 410 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L73-L79] |
| 411 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L75-L81] |
| 412 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L104-L110] |
| 413 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L105-L111] |
| 414 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L164-L170] |
| 415 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L165-L171] |
| 416 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L187-L193] |
| 417 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L53-L59] |
| 418 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L57-L63] |
| 419 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L63-L69] |
| 420 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L101-L107] |
| 421 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L104-L110] |
| 422 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L109-L115] |
| 423 | fixed engineering convention | Fixed coefficient, threshold or coating assumption is used; applicability requires source and input review. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L200-L206] |
| 424 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L17-L23] |
| 425 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L18-L24] |
| 426 | zero argument default | An omitted optional argument receives zero; the caller must confirm that origin/depth convention. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L150-L156] |
| 427 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L163-L169] |
| 428 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L170-L176] |
| 429 | zero argument default | An omitted optional argument receives zero; the caller must confirm that origin/depth convention. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L191-L197] |
| 430 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L204-L210] |
| 431 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L211-L217] |
| 432 | zero assignment | Zero initializes an accumulator, state or boundary value; no failure established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L303-L309] |
| 433 | swallowed parse | Mode parsing or dominant-mode lookup failure is printed, then an empty or partial frame is retained. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L117-L123] |
| 434 | empty result | Empty solver-output file creates an empty frame that later aggregation can skip. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L162-L173] |
| 435 | frequency fallback | Any frequency lookup error is consumed and a small artificial frequency is substituted. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L639-L645] |
| 436 | frequency fallback | Artificial frequency substitutes for an unavailable modal result. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L640-L646] |
| 437 | unit assumption | Cycle conversion assumes an annual duration and frequency/time convention. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L679-L685] |
| 438 | unit assumption | Stress interpolation is rescaled without a quantity-aware unit check. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L696-L702] |
| 439 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L872-L882] |
| 440 | except | Exception is consumed; execution returns or continues without propagating it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/__init__.py:L7-L11] |
| 441 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L431-L437] |
| 442 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L432-L438] |
| 443 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L433-L439] |
| 444 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L495-L501] |
| 445 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L514-L520] |
| 446 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L584-L593] |
| 447 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L685-L691] |
| 448 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L689-L708] |
| 449 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L717-L726] |
| 450 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L749-L755] |
| 451 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L750-L756] |
| 452 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L751-L757] |
| 453 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L754-L760] |
| 454 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L787-L793] |
| 455 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L852-L858] |
| 456 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L872-L878] |
| 457 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L878-L925] |
| 458 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L899-L905] |
| 459 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L921-L927] |
| 460 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1030-L1044] |
| 461 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1083-L1089] |
| 462 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1123-L1137] |
| 463 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1163-L1169] |
| 464 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1164-L1170] |
| 465 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1165-L1171] |
| 466 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1166-L1172] |
| 467 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1191-L1217] |
| 468 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1235-L1241] |
| 469 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1262-L1268] |
| 470 | continue | Loop continues when the legend is disabled; no record loss established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1389-L1395] |
| 471 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1444-L1450] |
| 472 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1459-L1465] |
| 473 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1488-L1494] |
| 474 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1591-L1597] |
| 475 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1622-L1628] |
| 476 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1623-L1629] |
| 477 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1626-L1632] |
| 478 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1631-L1637] |
| 479 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1640-L1646] |
| 480 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1642-L1648] |
| 481 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1643-L1649] |
| 482 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1645-L1651] |
| 483 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1659-L1665] |
| 484 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1664-L1686] |
| 485 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1682-L1688] |
| 486 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1687-L1693] |
| 487 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1696-L1705] |
| 488 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1734-L1740] |
| 489 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1755-L1761] |
| 490 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1756-L1762] |
| 491 | external numeric | External input is parsed by Python numeric conversion without locale normalization; malformed text raises unless an enclosing handler consumes it. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1796-L1802] |
| 492 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1798-L1804] |
| 493 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1810-L1816] |
| 494 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1813-L1819] |
| 495 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1815-L1821] |
| 496 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1816-L1822] |
| 497 | numeric conversion | Numeric scalar/output conversion; no external-text or error-suppression behaviour established. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1818-L1824] |
| 498 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1857-L1863] |
| 499 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1858-L1864] |
| 500 | GUI error sentinel | Broad error handler replaces the original exception with a GUI status message; partial state or follow-on work may remain. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1878-L1884] |
| 501 | lexical candidate | Screen matched comparison, initialization, output or prose; no suppressed error established from this line. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1883-L1901] |
| 502 | unit assumption | Quantity or model-output convention assumes compatible units; no automatic input-unit validation established here. | candidate; issue pending (1); no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1959-L1965] |
| 503 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1961-L1967] |
| 504 | documentation/unit mention | Documentation or a unit-labelled quantity is present; no suppressed error established from this occurrence. | non-failure; no detection check exists | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1962-L1968] |

## 7. Lessons log

| Date | Change reference | Lesson and pack change | Evidence / shared record |
| --- | --- | --- | --- |
| 2026-10-16 | Initial pack | Separate real synthetic-curve arithmetic from mocked engine smoke tests; state the qualification scope beside every worked example | [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L1-L25] [tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py:L13-L38] |
| 2026-10-16 | Initial pack | Record rounding and swallowed/defaulted results before relying on downstream utilization or fatigue summaries | [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L132-L165] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1728-L1737] |

UNVERIFIED: A separate shared-log entry identifier and private sidecar entry are not established; no client-derived values are included, and any future private lesson requires its own entry before use, issue pending (6).

## 8. Ownership

The reviewing role is the engineering calculations quality manager, on a provider independent of the producer; numerical criteria, standards ambiguities and unmet checks are referred to that role before clearance (AQG r0, section 11, Sign-off and independence).

UNVERIFIED: A named deputy, a permanent producing desk and a maintained escalation roster are not established in repository evidence; the maintainers will assign these before clearance, issue pending (6).

## 9. Dependencies and implementation inventory

The implementation-map ref is `1dba289c5dc1bc8dc82d72e4bfe02f8dcfc22c23`; its data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`, and its bytes match the map at the code pin. The family inventory below includes every mapped implementation for every family with an in-package row; external source locators are retained in the cited map records. [docs/registry/implementation-map.yaml:L1-L20]

| Family | In-package rows | All mapped rows | Counted grep |
| --- | --- | --- | --- |
| effective-tension-riser-stack-up | 37 | 264 | yes; [docs/registry/implementation-map.yaml:L2139-L4932] |
| orcaflex-text-model-writers | 31 | 440 | yes; [docs/registry/implementation-map.yaml:L4933-L9508] |
| cathodic-protection-calculations | 27 | 283 | yes; [docs/registry/implementation-map.yaml:L20964-L24098] |
| sn-curves-and-miner-damage | 11 | 392 | no; [docs/registry/implementation-map.yaml:L16807-L20963] |
| wind-and-current-force-routes | 4 | 135 | no; [docs/registry/implementation-map.yaml:L15393-L16806] |
| wave-spectra | 2 | 141 | no; [docs/registry/implementation-map.yaml:L13593-L15392] |
| catenary-mooring-line-statics | 1 | 140 | no; [docs/registry/implementation-map.yaml:L346-L2138] |
| irregular-frequency-treatment | 1 | 31 | no; [docs/registry/implementation-map.yaml:L13256-L13592] |

### Mapped definitions

Each numbered record below identifies one map implementation by its symbol and the exact map record containing its source locator, relation and evidence. In-package records also display the source path. [docs/registry/implementation-map.yaml:L1-L20]

#### catenary-mooring-line-statics

| Record | Definition / source locator | Map record |
| --- | --- | --- |
| 1 | `solve_static` | [docs/registry/implementation-map.yaml:L353-L364] |
| 2 | `compute_record` | [docs/registry/implementation-map.yaml:L365-L376] |
| 3 | `calc_catenary` | [docs/registry/implementation-map.yaml:L377-L388] |
| 4 | `solve_catenary_equations` | [docs/registry/implementation-map.yaml:L389-L400] |
| 5 | `main` | [docs/registry/implementation-map.yaml:L401-L414] |
| 6 | `main` | [docs/registry/implementation-map.yaml:L415-L426] |
| 7 | `analyze_mooring_line` | [docs/registry/implementation-map.yaml:L427-L438] |
| 8 | `analyze_turret_at_heading` | [docs/registry/implementation-map.yaml:L439-L450] |
| 9 | `analyze_calm_buoy` | [docs/registry/implementation-map.yaml:L451-L462] |
| 10 | `demo_spline_pipe` | [docs/registry/implementation-map.yaml:L463-L474] |
| 11 | `general_arc` | [docs/registry/implementation-map.yaml:L475-L488] |
| 12 | `scr_reference` | [docs/registry/implementation-map.yaml:L489-L504] |
| 13 | `slwr_reference` | [docs/registry/implementation-map.yaml:L505-L519] |
| 14 | `catenary` | [docs/registry/implementation-map.yaml:L520-L531] |
| 15 | `catenaryEquation` | [docs/registry/implementation-map.yaml:L532-L543] |
| 16 | `catenary_curve` | [docs/registry/implementation-map.yaml:L544-L555] |
| 17 | `catenary_derivative` | [docs/registry/implementation-map.yaml:L556-L567] |
| 18 | `arc_length.integrand` | [docs/registry/implementation-map.yaml:L568-L579] |
| 19 | `<module>` | [docs/registry/implementation-map.yaml:L580-L590] |
| 20 | `catenary` | [docs/registry/implementation-map.yaml:L591-L602] |
| 21 | `catenary` | [docs/registry/implementation-map.yaml:L603-L614] |
| 22 | `solve_a` | [docs/registry/implementation-map.yaml:L615-L626] |
| 23 | `<module>` | [docs/registry/implementation-map.yaml:L627-L637] |
| 24 | `PerformanceProfiler.profile_catenary_solver` | [docs/registry/implementation-map.yaml:L638-L649] |
| 25 | `demonstrate_usage` | [docs/registry/implementation-map.yaml:L650-L661] |
| 26 | `calculate_catenary_anchor_position` | [docs/registry/implementation-map.yaml:L662-L673] |
| 27 | `_catenary_riser_polyline` | [docs/registry/implementation-map.yaml:L674-L685] |
| 28 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.spring_properties` | [docs/registry/implementation-map.yaml:L686-L697] |
| 29 | `<module>` | [docs/registry/implementation-map.yaml:L698-L708] |
| 30 | `catenaryEquation` | [docs/registry/implementation-map.yaml:L709-L722] |
| 31 | `_angle_based_method` | [docs/registry/implementation-map.yaml:L723-L734] |
| 32 | `catenaryForces` | [docs/registry/implementation-map.yaml:L735-L746] |
| 33 | `LazyWaveSolver._solve_hangoff_section` | [docs/registry/implementation-map.yaml:L747-L758] |
| 34 | `LazyWaveSolver._solve_sag_hog_sections` | [docs/registry/implementation-map.yaml:L759-L770] |
| 35 | `SimplifiedCatenarySolver.solve_from_angle` | [docs/registry/implementation-map.yaml:L771-L782] |
| 36 | `SimplifiedCatenarySolver.solve_from_force` | [docs/registry/implementation-map.yaml:L783-L794] |
| 37 | `solve_catenary_dict` | [docs/registry/implementation-map.yaml:L795-L806] |
| 38 | `calculate_forces_dict` | [docs/registry/implementation-map.yaml:L807-L820] |
| 39 | `CatenarySolver.solve` | [docs/registry/implementation-map.yaml:L821-L834] |
| 40 | `CatenarySolver._geometry_for_a` | [docs/registry/implementation-map.yaml:L835-L846] |
| 41 | `CatenarySolver._solve_simplified.length_error` | [docs/registry/implementation-map.yaml:L847-L858] |
| 42 | `CatenarySolver._compute_results` | [docs/registry/implementation-map.yaml:L859-L870] |
| 43 | `safe_sinh` | [docs/registry/implementation-map.yaml:L871-L882] |
| 44 | `safe_cosh` | [docs/registry/implementation-map.yaml:L883-L894] |
| 45 | `calculate_elongation` | [docs/registry/implementation-map.yaml:L895-L906] |
| 46 | `touchdown_point` | [docs/registry/implementation-map.yaml:L907-L918] |
| 47 | `PerformanceProfiler.profile_catenary_solver` | [docs/registry/implementation-map.yaml:L919-L930] |
| 48 | `<module>` | [docs/registry/implementation-map.yaml:L931-L941] |
| 49 | `demonstrate_usage` | [docs/registry/implementation-map.yaml:L942-L953] |
| 50 | `<module>` | [docs/registry/implementation-map.yaml:L954-L964] |
| 51 | `FPSOMooringFullWorkflow._baseline_span` | [docs/registry/implementation-map.yaml:L965-L978] |
| 52 | `FPSOMooringFullWorkflow._line_tensions` | [docs/registry/implementation-map.yaml:L979-L992] |
| 53 | `solve_catenary` | [docs/registry/implementation-map.yaml:L989-L1002] |
| 54 | `MooringLineDesign.estimate_catenary` | [docs/registry/implementation-map.yaml:L1003-L1018] |
| 55 | `JLayConfig.calculate_sagbend` | [docs/registry/implementation-map.yaml:L1019-L1030] |
| 56 | `SCRDesignInput.calculate_geometry` | [docs/registry/implementation-map.yaml:L1031-L1042] |
| 57 | `_lazy` | [docs/registry/implementation-map.yaml:L1043-L1054] |
| 58 | `_lazy_shape` | [docs/registry/implementation-map.yaml:L1055-L1066] |
| 59 | `scr_offsets` | [docs/registry/implementation-map.yaml:L1067-L1078] |
| 60 | `scr_offsets.residual` | [docs/registry/implementation-map.yaml:L1079-L1090] |
| 61 | `OrcaflexModelComponents.spring_properties` | [docs/registry/implementation-map.yaml:L1091-L1102] |
| 62 | `LazyWaveAnalyzer.analyze` | [docs/registry/implementation-map.yaml:L1103-L1114] |
| 63 | `catenaryEquation` | [docs/registry/implementation-map.yaml:L1115-L1126] |
| 64 | `catenaryForces` | [docs/registry/implementation-map.yaml:L1127-L1138] |
| 65 | `sagHogEquation` | [docs/registry/implementation-map.yaml:L1139-L1150] |
| 66 | `lazyWaveCatenaryEquation` | [docs/registry/implementation-map.yaml:L1151-L1165] |
| 67 | `gethorizontalDistanceHangofftoTDPrange` | [docs/registry/implementation-map.yaml:L1166-L1179] |
| 68 | `CatenaryCalculator._calculate_from_angle` | [docs/registry/implementation-map.yaml:L1180-L1191] |
| 69 | `catenary_riser` | [docs/registry/implementation-map.yaml:L1192-L1208] |
| 70 | `orcaflexModel` | [docs/registry/implementation-map.yaml:L1209-L1222] |
| 71 | `lineTypes` | [docs/registry/implementation-map.yaml:L1223-L1234] |
| 72 | `SimpleCatenaryAnalyzer.analyze_riser` | [docs/registry/implementation-map.yaml:L1235-L1249] |
| 73 | `SimpleCatenaryAnalyzer._solve_catenary_from_H` | [docs/registry/implementation-map.yaml:L1250-L1261] |
| 74 | `SimpleCatenaryAnalyzer._solve_H_from_top_tension.calc_top_tension` | [docs/registry/implementation-map.yaml:L1262-L1273] |
| 75 | `SimpleCatenaryAnalyzer._solve_H_from_geometry` | [docs/registry/implementation-map.yaml:L1274-L1285] |
| 76 | `CatenaryAnalyzer.solve_catenary` | [docs/registry/implementation-map.yaml:L1286-L1297] |
| 77 | `CatenaryAnalyzer.solve_for_horizontal_tension` | [docs/registry/implementation-map.yaml:L1298-L1311] |
| 78 | `CatenaryAnalyzer.calculate_stiffness` | [docs/registry/implementation-map.yaml:L1312-L1323] |
| 79 | `CatenaryAnalyzer.check_touchdown` | [docs/registry/implementation-map.yaml:L1324-L1335] |
| 80 | `CatenaryAnalyzer.check_touchdown.s_from_a` | [docs/registry/implementation-map.yaml:L1336-L1347] |
| 81 | `catenary_analysis` | [docs/registry/implementation-map.yaml:L1348-L1361] |
| 82 | `MooringDesigner.analyze_line_tension` | [docs/registry/implementation-map.yaml:L1362-L1373] |
| 83 | `_catinb` | [docs/registry/implementation-map.yaml:L1374-L1387] |
| 84 | `_stretc` | [docs/registry/implementation-map.yaml:L1388-L1399] |
| 85 | `_fulcat` | [docs/registry/implementation-map.yaml:L1400-L1411] |
| 86 | `_catnry` | [docs/registry/implementation-map.yaml:L1412-L1425] |
| 87 | `_sub` | [docs/registry/implementation-map.yaml:L1426-L1437] |
| 88 | `build_line_table` | [docs/registry/implementation-map.yaml:L1438-L1453] |
| 89 | `interp_table` | [docs/registry/implementation-map.yaml:L1454-L1465] |
| 90 | `anchor_pull` | [docs/registry/implementation-map.yaml:L1466-L1477] |
| 91 | `solve_pretension` | [docs/registry/implementation-map.yaml:L1478-L1489] |
| 92 | `restoring_force` | [docs/registry/implementation-map.yaml:L1490-L1503] |
| 93 | `_geometry` | [docs/registry/implementation-map.yaml:L1504-L1515] |
| 94 | `_derive.primitive` | [docs/registry/implementation-map.yaml:L1516-L1527] |
| 95 | `test_elastic_fixture_geometry_and_force_identities` | [docs/registry/implementation-map.yaml:L1528-L1541] |
| 96 | `test_inextensible_chord_arc_and_end_forces` | [docs/registry/implementation-map.yaml:L1542-L1553] |
| 97 | `test_superseded_answers_fail_geometry_and_touchdown_diagnosis` | [docs/registry/implementation-map.yaml:L1554-L1565] |
| 98 | `test_solver_against_reconciled_fixture` | [docs/registry/implementation-map.yaml:L1566-L1579] |
| 99 | `<module>` | [docs/registry/implementation-map.yaml:L1580-L1590] |
| 100 | `expected_from_angle` | [docs/registry/implementation-map.yaml:L1591-L1602] |
| 101 | `expected_from_force` | [docs/registry/implementation-map.yaml:L1603-L1614] |
| 102 | `CatenarySolver.solve.length_error` | [docs/registry/implementation-map.yaml:L1615-L1626] |
| 103 | `CatenarySolver.solve.length_error_derivative` | [docs/registry/implementation-map.yaml:L1627-L1638] |
| 104 | `CatenarySolver._compute_results` | [docs/registry/implementation-map.yaml:L1639-L1652] |
| 105 | `CatenarySolver._calculate_touchdown` | [docs/registry/implementation-map.yaml:L1653-L1664] |
| 106 | `_signed_anchor_coordinate` | [docs/registry/implementation-map.yaml:L1665-L1676] |
| 107 | `TestCatenarySolver.test_shape_coordinates` | [docs/registry/implementation-map.yaml:L1677-L1690] |
| 108 | `TestCatenaryPerformance.test_batch_processing` | [docs/registry/implementation-map.yaml:L1691-L1702] |
| 109 | `test_angle_method` | [docs/registry/implementation-map.yaml:L1703-L1716] |
| 110 | `test_forces` | [docs/registry/implementation-map.yaml:L1717-L1730] |
| 111 | `TestCatenaryEquationAngleMethod.test_angle_method_exact_legacy_match` | [docs/registry/implementation-map.yaml:L1731-L1743] |
| 112 | `TestCatenaryForces.test_basic_forces_calculation` | [docs/registry/implementation-map.yaml:L1744-L1756] |
| 113 | `TestCatenaryForces.test_forces_exact_legacy_match` | [docs/registry/implementation-map.yaml:L1757-L1769] |
| 114 | `TestCatenaryForces.test_forces_various_scenarios` | [docs/registry/implementation-map.yaml:L1770-L1782] |
| 115 | `CatenarySolver.solve.length_error` | [docs/registry/implementation-map.yaml:L1783-L1794] |
| 116 | `CatenarySolver.solve.length_error_derivative` | [docs/registry/implementation-map.yaml:L1795-L1806] |
| 117 | `CatenarySolver._compute_results` | [docs/registry/implementation-map.yaml:L1807-L1820] |
| 118 | `CatenarySolver._calculate_touchdown` | [docs/registry/implementation-map.yaml:L1821-L1832] |
| 119 | `_signed_anchor_coordinate` | [docs/registry/implementation-map.yaml:L1833-L1844] |
| 120 | `TestCatenarySolver.test_shape_coordinates` | [docs/registry/implementation-map.yaml:L1845-L1858] |
| 121 | `TestCatenaryPerformance.test_batch_processing` | [docs/registry/implementation-map.yaml:L1859-L1870] |
| 122 | `test_scr_matches_independent_inextensible_catenary` | [docs/registry/implementation-map.yaml:L1871-L1882] |
| 123 | `catenary_system` | [docs/registry/implementation-map.yaml:L1883-L1894] |
| 124 | `catenary_system` | [docs/registry/implementation-map.yaml:L1895-L1906] |
| 125 | `test_marine_engineering_imports` | [docs/registry/implementation-map.yaml:L1907-L1918] |
| 126 | `lazy_results` | [docs/registry/implementation-map.yaml:L1919-L1932] |
| 127 | `test_scr_design_and_general_solver_in_inextensible_limit` | [docs/registry/implementation-map.yaml:L1933-L1946] |
| 128 | `TestLibraryVsOracle.test_catinb_sloped_bottom_matches_oracle` | [docs/registry/implementation-map.yaml:L1947-L1960] |
| 129 | `test_excel_validation_case` | [docs/registry/implementation-map.yaml:L1961-L1972] |
| 130 | `TestLazyWaveSolver.test_hangoff_section_calculation` | [docs/registry/implementation-map.yaml:L1973-L1984] |
| 131 | `TestLazyWaveSolver.test_sag_to_buoyancy_segment` | [docs/registry/implementation-map.yaml:L1985-L1996] |
| 132 | `TestLazyWaveSolver.test_buoyancy_to_hog_segment` | [docs/registry/implementation-map.yaml:L1997-L2008] |
| 133 | `TestLazyWaveSolver.test_hog_to_buoyancy_segment` | [docs/registry/implementation-map.yaml:L2009-L2020] |
| 134 | `TestLazyWaveSolver.test_buoyancy_to_touchdown_segment` | [docs/registry/implementation-map.yaml:L2021-L2032] |
| 135 | `test_hangoff_vertical_force_balances_suspended_weight` | [docs/registry/implementation-map.yaml:L2033-L2046] |
| 136 | `test_hangoff_total_tension_matches_catenary_and_force_triangle` | [docs/registry/implementation-map.yaml:L2047-L2060] |
| 137 | `solver` | [docs/registry/implementation-map.yaml:L2061-L2074] |
| 138 | `SimpleCatenaryAnalyzer.calculate_tension_along_riser` | [docs/registry/implementation-map.yaml:L2075-L2090] |
| 139 | `SimpleCatenaryAnalyzer.calculate_angle_along_riser` | [docs/registry/implementation-map.yaml:L2091-L2106] |
| 140 | `SimplifiedCatenarySolver.calculate_forces` | [docs/registry/implementation-map.yaml:L2107-L2123] |

#### effective-tension-riser-stack-up

| Record | Definition / source locator | Map record |
| --- | --- | --- |
| 1 | `main` | [docs/registry/implementation-map.yaml:L2146-L2155] |
| 2 | `main` | [docs/registry/implementation-map.yaml:L2156-L2165] |
| 3 | `main` | [docs/registry/implementation-map.yaml:L2166-L2175] |
| 4 | `main` | [docs/registry/implementation-map.yaml:L2176-L2185] |
| 5 | `check_r01_catenary` | [docs/registry/implementation-map.yaml:L2186-L2195] |
| 6 | `check_r02_lazy_wave` | [docs/registry/implementation-map.yaml:L2196-L2205] |
| 7 | `check_r05_lay_table` | [docs/registry/implementation-map.yaml:L2206-L2215] |
| 8 | `check_r06_multi_static` | [docs/registry/implementation-map.yaml:L2216-L2225] |
| 9 | `extract_vessel_params` | [docs/registry/implementation-map.yaml:L2226-L2235] |
| 10 | `vessel_band_specs` | [docs/registry/implementation-map.yaml:L2236-L2247] |
| 11 | `find_required_tension` | [docs/registry/implementation-map.yaml:L2248-L2259] |
| 12 | `_build_infeasible_result` | [docs/registry/implementation-map.yaml:L2260-L2269] |
| 13 | `run_single_case` | [docs/registry/implementation-map.yaml:L2270-L2282] |
| 14 | `<module>` | [docs/registry/implementation-map.yaml:L2283-L2291] |
| 15 | `_good_case` | [docs/registry/implementation-map.yaml:L2292-L2301] |
| 16 | `_infeasible_case` | [docs/registry/implementation-map.yaml:L2302-L2311] |
| 17 | `test_999_sentinel_and_none_null_round_trip` | [docs/registry/implementation-map.yaml:L2312-L2324] |
| 18 | `analyze_installation_catenary` | [docs/registry/implementation-map.yaml:L2325-L2334] |
| 19 | `_schedule_row` | [docs/registry/implementation-map.yaml:L2335-L2344] |
| 20 | `_assemble_row` | [docs/registry/implementation-map.yaml:L2345-L2356] |
| 21 | `<module>` | [docs/registry/implementation-map.yaml:L2357-L2365] |
| 22 | `_analyze_point` | [docs/registry/implementation-map.yaml:L2366-L2375] |
| 23 | `build_study` | [docs/registry/implementation-map.yaml:L2376-L2387] |
| 24 | `_axes` | [docs/registry/implementation-map.yaml:L2388-L2397] |
| 25 | `_pack_rows` | [docs/registry/implementation-map.yaml:L2398-L2407] |
| 26 | `_run` | [docs/registry/implementation-map.yaml:L2408-L2417] |
| 27 | `metrics` | [docs/registry/implementation-map.yaml:L2418-L2427] |
| 28 | `calculate` | [docs/registry/implementation-map.yaml:L2428-L2437] |
| 29 | `catenary` | [docs/registry/implementation-map.yaml:L2438-L2447] |
| 30 | `case_spec` | [docs/registry/implementation-map.yaml:L2448-L2457] |
| 31 | `prepare_case` | [docs/registry/implementation-map.yaml:L2458-L2470] |
| 32 | `prepare` | [docs/registry/implementation-map.yaml:L2471-L2484] |
| 33 | `main` | [docs/registry/implementation-map.yaml:L2485-L2494] |
| 34 | `APISTD2RDCalculation` | [docs/registry/implementation-map.yaml:L2495-L2504] |
| 35 | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | [docs/registry/implementation-map.yaml:L2505-L2514] |
| 36 | `StackUpDiagram` | [docs/registry/implementation-map.yaml:L2515-L2524] |
| 37 | `main` | [docs/registry/implementation-map.yaml:L2525-L2534] |
| 38 | `main` | [docs/registry/implementation-map.yaml:L2535-L2544] |
| 39 | `evaluate` | [docs/registry/implementation-map.yaml:L2545-L2556] |
| 40 | `_validate` | [docs/registry/implementation-map.yaml:L2557-L2566] |
| 41 | `numeric_values` | [docs/registry/implementation-map.yaml:L2567-L2576] |
| 42 | `_input_table` | [docs/registry/implementation-map.yaml:L2577-L2586] |
| 43 | `<module>` | [docs/registry/implementation-map.yaml:L2587-L2595] |
| 44 | `VonMises_Pipe.init_calculate_required_properties` | [docs/registry/implementation-map.yaml:L2596-L2605] |
| 45 | `local_buckling_lcc` | [docs/registry/implementation-map.yaml:L2606-L2615] |
| 46 | `_run_api_rp_2rd` | [docs/registry/implementation-map.yaml:L2616-L2625] |
| 47 | `_run_dnv_os_f201` | [docs/registry/implementation-map.yaml:L2626-L2635] |
| 48 | `minimum_top_tension_16q` | [docs/registry/implementation-map.yaml:L2636-L2645] |
| 49 | `_unit_count` | [docs/registry/implementation-map.yaml:L2646-L2655] |
| 50 | `check_rig_capability` | [docs/registry/implementation-map.yaml:L2656-L2667] |
| 51 | `case_spec` | [docs/registry/implementation-map.yaml:L2668-L2679] |
| 52 | `_ring_tensioners` | [docs/registry/implementation-map.yaml:L2680-L2689] |
| 53 | `set_line_tension` | [docs/registry/implementation-map.yaml:L2690-L2701] |
| 54 | `calibrate_line_tension` | [docs/registry/implementation-map.yaml:L2702-L2715] |
| 55 | `_statics_path` | [docs/registry/implementation-map.yaml:L2716-L2726] |
| 56 | `robust_statics` | [docs/registry/implementation-map.yaml:L2727-L2740] |
| 57 | `_tensioner_vertical_n` | [docs/registry/implementation-map.yaml:L2741-L2750] |
| 58 | `tensioner_vertical_target_n` | [docs/registry/implementation-map.yaml:L2751-L2760] |
| 59 | `physical_state_checks` | [docs/registry/implementation-map.yaml:L2761-L2774] |
| 60 | `RiserCampaignAdapter.prepare` | [docs/registry/implementation-map.yaml:L2775-L2787] |
| 61 | `RiserCampaignAdapter.extract` | [docs/registry/implementation-map.yaml:L2788-L2797] |
| 62 | `compute_operating_envelope` | [docs/registry/implementation-map.yaml:L2798-L2807] |
| 63 | `equivalent_string_top_tension_n` | [docs/registry/implementation-map.yaml:L2808-L2817] |
| 64 | `_tensioner_geometry` | [docs/registry/implementation-map.yaml:L2818-L2827] |
| 65 | `winch_tension_n` | [docs/registry/implementation-map.yaml:L2828-L2839] |
| 66 | `build_generic_spec` | [docs/registry/implementation-map.yaml:L2840-L2852] |
| 67 | `effective_tension_chain` | [docs/registry/implementation-map.yaml:L2853-L2862] |
| 68 | `tension_references` | [docs/registry/implementation-map.yaml:L2863-L2874] |
| 69 | `tensioner_lateral_stiffness_n_m` | [docs/registry/implementation-map.yaml:L2875-L2884] |
| 70 | `reference_periods` | [docs/registry/implementation-map.yaml:L2885-L2896] |
| 71 | `hung_weight_n` | [docs/registry/implementation-map.yaml:L2897-L2907] |
| 72 | `OpenWaterRiserSpec._closes` | [docs/registry/implementation-map.yaml:L2908-L2917] |
| 73 | `build_open_water_generic_spec` | [docs/registry/implementation-map.yaml:L2918-L2927] |
| 74 | `tension_references` | [docs/registry/implementation-map.yaml:L2928-L2938] |
| 75 | `reference_periods` | [docs/registry/implementation-map.yaml:L2939-L2948] |
| 76 | `release_reference` | [docs/registry/implementation-map.yaml:L2949-L2960] |
| 77 | `ring_vertical_residual_n` | [docs/registry/implementation-map.yaml:L2961-L2970] |
| 78 | `open_water_physical_checks` | [docs/registry/implementation-map.yaml:L2971-L2980] |
| 79 | `Tensioners._failed` | [docs/registry/implementation-map.yaml:L2981-L2990] |
| 80 | `RiserGlobalModelSpec._geometry_closes` | [docs/registry/implementation-map.yaml:L2991-L3000] |
| 81 | `_missing_open_water` | [docs/registry/implementation-map.yaml:L3001-L3010] |
| 82 | `missing_channels` | [docs/registry/implementation-map.yaml:L3011-L3022] |
| 83 | `extract` | [docs/registry/implementation-map.yaml:L3023-L3032] |
| 84 | `extract_open_water` | [docs/registry/implementation-map.yaml:L3033-L3042] |
| 85 | `t_min` | [docs/registry/implementation-map.yaml:L3043-L3052] |
| 86 | `tension_setting_max` | [docs/registry/implementation-map.yaml:L3053-L3062] |
| 87 | `tensioner_stroke` | [docs/registry/implementation-map.yaml:L3063-L3072] |
| 88 | `ScheduleAssembly.minimum_top_tension_16q_kn` | [docs/registry/implementation-map.yaml:L3073-L3082] |
| 89 | `_load_oc_assembly` | [docs/registry/implementation-map.yaml:L3083-L3092] |
| 90 | `effective_tension` | [docs/registry/implementation-map.yaml:L3093-L3102] |
| 91 | `parse_snapshots` | [docs/registry/implementation-map.yaml:L3103-L3112] |
| 92 | `snapshot_to_tension_n` | [docs/registry/implementation-map.yaml:L3113-L3122] |
| 93 | `router` | [docs/registry/implementation-map.yaml:L3123-L3135] |
| 94 | `_section_result` | [docs/registry/implementation-map.yaml:L3136-L3145] |
| 95 | `_effective_tension_n` | [docs/registry/implementation-map.yaml:L3146-L3155] |
| 96 | `router` | [docs/registry/implementation-map.yaml:L3156-L3168] |
| 97 | `_write_profile_csv` | [docs/registry/implementation-map.yaml:L3169-L3178] |
| 98 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.add_riser_length_to_shear7_result` | [docs/registry/implementation-map.yaml:L3179-L3188] |
| 99 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | [docs/registry/implementation-map.yaml:L3189-L3198] |
| 100 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | [docs/registry/implementation-map.yaml:L3199-L3208] |
| 101 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L3209-L3218] |
| 102 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.add_fatigue_damage_results_to_df` | [docs/registry/implementation-map.yaml:L3219-L3231] |
| 103 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3232-L3241] |
| 104 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3242-L3251] |
| 105 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L3252-L3264] |
| 106 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.customize_fj_types` | [docs/registry/implementation-map.yaml:L3265-L3274] |
| 107 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.links` | [docs/registry/implementation-map.yaml:L3275-L3287] |
| 108 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.add_support_spring` | [docs/registry/implementation-map.yaml:L3288-L3300] |
| 109 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.add_tensioners` | [docs/registry/implementation-map.yaml:L3301-L3311] |
| 110 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.tensioner_vessel_locations` | [docs/registry/implementation-map.yaml:L3312-L3321] |
| 111 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.spring_properties` | [docs/registry/implementation-map.yaml:L3322-L3331] |
| 112 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.get_connection_definitions` | [docs/registry/implementation-map.yaml:L3332-L3341] |
| 113 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.get_fj_line_definitions` | [docs/registry/implementation-map.yaml:L3342-L3351] |
| 114 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.get_line_items_vertical` | [docs/registry/implementation-map.yaml:L3352-L3361] |
| 115 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` / `TypicalRiserStackUpCalculations.prepare_stack_up_properties_df` | [docs/registry/implementation-map.yaml:L3362-L3377] |
| 116 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` / `TypicalRiserStackUpCalculations.populate_elevation_from_endA_in_df` | [docs/registry/implementation-map.yaml:L3378-L3387] |
| 117 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` / `TypicalRiserStackUpCalculations.populate_shear7_x_by_L_in_df` | [docs/registry/implementation-map.yaml:L3388-L3397] |
| 118 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` / `TypicalRiserStackUpCalculations.populate_elevation_relative_to_mudline_in_df` | [docs/registry/implementation-map.yaml:L3398-L3407] |
| 119 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` / `TypicalRiserStackUpCalculations.populate_elevation_relative_to_MSL_in_df` | [docs/registry/implementation-map.yaml:L3408-L3417] |
| 120 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` / `TypicalRiserStackUpCalculations.evaluate_inservice_weights_and_tensions` | [docs/registry/implementation-map.yaml:L3418-L3432] |
| 121 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` / `TypicalRiserStackUpCalculations.assign_key_properties_to_df` | [docs/registry/implementation-map.yaml:L3433-L3442] |
| 122 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` / `TypicalRiserStackUpCalculations.evaluate_stack_in_service_weights` | [docs/registry/implementation-map.yaml:L3443-L3452] |
| 123 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` / `TypicalRiserStackUpCalculations.evaluate_effective_tension` | [docs/registry/implementation-map.yaml:L3453-L3462] |
| 124 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` / `TypicalRiserStackUpCalculations.evaluate_stretch_in_riser` | [docs/registry/implementation-map.yaml:L3463-L3472] |
| 125 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` / `Shear7ModelComponents.title` | [docs/registry/implementation-map.yaml:L3473-L3482] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` / `Shear7ModelComponents.riser_joints_to_shear7_zones` | [docs/registry/implementation-map.yaml:L3483-L3492] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` / `Shear7ModelComponents.add_block_3` | [docs/registry/implementation-map.yaml:L3493-L3502] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.add_riser_length_to_shear7_result` | [docs/registry/implementation-map.yaml:L3503-L3512] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | [docs/registry/implementation-map.yaml:L3513-L3522] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | [docs/registry/implementation-map.yaml:L3523-L3532] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L3533-L3542] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.add_fatigue_damage_results_to_df` | [docs/registry/implementation-map.yaml:L3543-L3555] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3556-L3565] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3566-L3575] |
| 135 | `_write_tensions_csv` | [docs/registry/implementation-map.yaml:L3576-L3585] |
| 136 | `FPSOMooringFullWorkflow._line_state` | [docs/registry/implementation-map.yaml:L3578-L3589] |
| 137 | `FPSOMooringFullWorkflow._solve_line` | [docs/registry/implementation-map.yaml:L3590-L3599] |
| 138 | `FPSOMooringFullWorkflow._result` | [docs/registry/implementation-map.yaml:L3600-L3609] |
| 139 | `solve_catenary` | [docs/registry/implementation-map.yaml:L3618-L3627] |
| 140 | `JLayConfig.calculate_sagbend` | [docs/registry/implementation-map.yaml:L3628-L3637] |
| 141 | `SCRDesignInput.calculate_geometry` | [docs/registry/implementation-map.yaml:L3638-L3647] |
| 142 | `TTRDesignInput.calculate_stroke` | [docs/registry/implementation-map.yaml:L3648-L3657] |
| 143 | `TTRDesignInput.calculate_top_tension` | [docs/registry/implementation-map.yaml:L3658-L3667] |
| 144 | `_TopTensionedInput.to_legacy` | [docs/registry/implementation-map.yaml:L3668-L3677] |
| 145 | `BeamProperties.natural_frequency` | [docs/registry/implementation-map.yaml:L3678-L3687] |
| 146 | `_code_check_utilisation` | [docs/registry/implementation-map.yaml:L3688-L3697] |
| 147 | `run_jlay` | [docs/registry/implementation-map.yaml:L3698-L3707] |
| 148 | `_lazy_row` | [docs/registry/implementation-map.yaml:L3708-L3717] |
| 149 | `scr_offsets` | [docs/registry/implementation-map.yaml:L3718-L3727] |
| 150 | `_validate` | [docs/registry/implementation-map.yaml:L3728-L3737] |
| 151 | `_utilisation` | [docs/registry/implementation-map.yaml:L3738-L3747] |
| 152 | `_checks` | [docs/registry/implementation-map.yaml:L3748-L3757] |
| 153 | `GroupsBuilder._build_from_spec` | [docs/registry/implementation-map.yaml:L3758-L3767] |
| 154 | `LineTypeBuilder.build` | [docs/registry/implementation-map.yaml:L3768-L3777] |
| 155 | `WinchBuilder.should_generate` | [docs/registry/implementation-map.yaml:L3778-L3787] |
| 156 | `WinchBuilder.build` | [docs/registry/implementation-map.yaml:L3788-L3797] |
| 157 | `_apply_overrides` | [docs/registry/implementation-map.yaml:L3798-L3807] |
| 158 | `Mooring.mooring_pre_tension_analysis` | [docs/registry/implementation-map.yaml:L3808-L3819] |
| 159 | `Mooring.evaluate_line_length_next_iteration` | [docs/registry/implementation-map.yaml:L3820-L3829] |
| 160 | `OPPRangeGraph.postProcessRange` | [docs/registry/implementation-map.yaml:L3830-L3839] |
| 161 | `OrcaFlexAnalysis.postProcessRange` | [docs/registry/implementation-map.yaml:L3840-L3849] |
| 162 | `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L3850-L3862] |
| 163 | `OrcaflexModelComponents.customize_fj_types` | [docs/registry/implementation-map.yaml:L3863-L3872] |
| 164 | `OrcaflexModelComponents.links` | [docs/registry/implementation-map.yaml:L3873-L3885] |
| 165 | `OrcaflexModelComponents.add_support_spring` | [docs/registry/implementation-map.yaml:L3886-L3898] |
| 166 | `OrcaflexModelComponents.add_tensioners` | [docs/registry/implementation-map.yaml:L3899-L3909] |
| 167 | `OrcaflexModelComponents.tensioner_vessel_locations` | [docs/registry/implementation-map.yaml:L3910-L3919] |
| 168 | `OrcaflexModelComponents.spring_properties` | [docs/registry/implementation-map.yaml:L3920-L3929] |
| 169 | `OrcaflexModelComponents.get_connection_definitions` | [docs/registry/implementation-map.yaml:L3930-L3939] |
| 170 | `OrcaflexModelComponents.get_fj_line_definitions` | [docs/registry/implementation-map.yaml:L3940-L3949] |
| 171 | `OrcaflexModelComponents.get_line_items_vertical` | [docs/registry/implementation-map.yaml:L3950-L3959] |
| 172 | `check` | [docs/registry/implementation-map.yaml:L3960-L3969] |
| 173 | `WallThicknessAnalyzer.check_combined_loading` | [docs/registry/implementation-map.yaml:L3970-L3979] |
| 174 | `ApiStd2rdStrategy._check_combined` | [docs/registry/implementation-map.yaml:L3980-L3989] |
| 175 | `DnvStF101Strategy._check_combined_loading` | [docs/registry/implementation-map.yaml:L3990-L3999] |
| 176 | `run_wall_thickness_compare` | [docs/registry/implementation-map.yaml:L4000-L4009] |
| 177 | `MTLookupGenerator.generate` | [docs/registry/implementation-map.yaml:L4010-L4019] |
| 178 | `ParametricSweep.run` | [docs/registry/implementation-map.yaml:L4020-L4029] |
| 179 | `PipelinePhase.to_design_loads` | [docs/registry/implementation-map.yaml:L4030-L4039] |
| 180 | `create_standard_phases` | [docs/registry/implementation-map.yaml:L4040-L4049] |
| 181 | `LoadScalingProcessor.process_all_conditions` | [docs/registry/implementation-map.yaml:L4050-L4059] |
| 182 | `IntegratedFatigueProcessor.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L4060-L4069] |
| 183 | `IntegratedFatigueProcessorWithNaming._process_condition_base` | [docs/registry/implementation-map.yaml:L4070-L4079] |
| 184 | `LoadScalingProcessor.load_reference_data` | [docs/registry/implementation-map.yaml:L4080-L4089] |
| 185 | `ReferenceSeaStateProcessor.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L4090-L4099] |
| 186 | `LoadScaler.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L4100-L4109] |
| 187 | `LoadScaler.process_sea_state` | [docs/registry/implementation-map.yaml:L4110-L4119] |
| 188 | `main` | [docs/registry/implementation-map.yaml:L4120-L4129] |
| 189 | `ProductionDataHandlerWithProgress.process_single_analysis` | [docs/registry/implementation-map.yaml:L4130-L4139] |
| 190 | `ParametricCoordinator._run_wall_thickness` | [docs/registry/implementation-map.yaml:L4140-L4149] |
| 191 | `VonMises_Pipe.init_calculate_required_properties` | [docs/registry/implementation-map.yaml:L4150-L4159] |
| 192 | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | [docs/registry/implementation-map.yaml:L4160-L4169] |
| 193 | `VonMises_Pipe.init_calculate_required_properties` | [docs/registry/implementation-map.yaml:L4170-L4179] |
| 194 | `simple_catenary` | [docs/registry/implementation-map.yaml:L4180-L4189] |
| 195 | `lazy_wave_analysis` | [docs/registry/implementation-map.yaml:L4190-L4199] |
| 196 | `LazyWaveAnalyzer.analyze` | [docs/registry/implementation-map.yaml:L4200-L4209] |
| 197 | `SimpleCatenaryAnalyzer.analyze_riser` | [docs/registry/implementation-map.yaml:L4210-L4222] |
| 198 | `SimpleCatenaryAnalyzer._solve_catenary_from_H` | [docs/registry/implementation-map.yaml:L4223-L4232] |
| 199 | `SimpleCatenaryAnalyzer._solve_H_from_top_tension` | [docs/registry/implementation-map.yaml:L4233-L4242] |
| 200 | `SimpleCatenaryAnalyzer.check_design_limits` | [docs/registry/implementation-map.yaml:L4243-L4252] |
| 201 | `developed_min_radius` | [docs/registry/implementation-map.yaml:L4253-L4262] |
| 202 | `required_effective_ei` | [docs/registry/implementation-map.yaml:L4263-L4272] |
| 203 | `screen_bend_stiffener` | [docs/registry/implementation-map.yaml:L4273-L4285] |
| 204 | `armour_tension_stress` | [docs/registry/implementation-map.yaml:L4286-L4295] |
| 205 | `verify_flexible_pipe` | [docs/registry/implementation-map.yaml:L4296-L4307] |
| 206 | `effective_driving_force` | [docs/registry/implementation-map.yaml:L4308-L4317] |
| 207 | `RiserLoadCase.to_loads` | [docs/registry/implementation-map.yaml:L4318-L4327] |
| 208 | `run_riser_screening` | [docs/registry/implementation-map.yaml:L4328-L4337] |
| 209 | `StackUpDiagram` | [docs/registry/implementation-map.yaml:L4338-L4347] |
| 210 | `VerticalRiser.prepare_riser_model` | [docs/registry/implementation-map.yaml:L4348-L4362] |
| 211 | `VerticalRiser.read_riser_input_data` | [docs/registry/implementation-map.yaml:L4363-L4372] |
| 212 | `VerticalRiser.assign_key_properties_to_self` | [docs/registry/implementation-map.yaml:L4373-L4382] |
| 213 | `VerticalRiser.get_fea_properties` | [docs/registry/implementation-map.yaml:L4383-L4392] |
| 214 | `VerticalRiser.prepare_tensioner_data` | [docs/registry/implementation-map.yaml:L4393-L4402] |
| 215 | `VerticalRiser.evaluate_stack_up_properties` | [docs/registry/implementation-map.yaml:L4403-L4412] |
| 216 | `VerticalRiser.save_data` | [docs/registry/implementation-map.yaml:L4413-L4422] |
| 217 | `VerticalRiser.alter_properties_for_Shear7_software` | [docs/registry/implementation-map.yaml:L4423-L4432] |
| 218 | `FrequencyCalculator.calculate_natural_frequency` | [docs/registry/implementation-map.yaml:L4433-L4444] |
| 219 | `FrequencyCalculator._frequency_tension_controlled` | [docs/registry/implementation-map.yaml:L4445-L4454] |
| 220 | `FrequencyCalculator.calculate_effective_tension` | [docs/registry/implementation-map.yaml:L4455-L4464] |
| 221 | `FrequencyCalculator.check_stability` | [docs/registry/implementation-map.yaml:L4465-L4476] |
| 222 | `_beam` | [docs/registry/implementation-map.yaml:L4477-L4486] |
| 223 | `riser_analysis_cmd` | [docs/registry/implementation-map.yaml:L4487-L4496] |
| 224 | `create_riser_analysis_template` | [docs/registry/implementation-map.yaml:L4497-L4506] |
| 225 | `CompleteRiserAnalysisWorkflow.create` | [docs/registry/implementation-map.yaml:L4507-L4516] |
| 226 | `_make_pipe_cfg` | [docs/registry/implementation-map.yaml:L4517-L4526] |
| 227 | `_study` | [docs/registry/implementation-map.yaml:L4527-L4536] |
| 228 | `<module>` | [docs/registry/implementation-map.yaml:L4537-L4545] |
| 229 | `test_envelope_von_mises_n_to_kn_seam` | [docs/registry/implementation-map.yaml:L4546-L4557] |
| 230 | `synthetic_spec` | [docs/registry/implementation-map.yaml:L4558-L4567] |
| 231 | `test_physical_checks_use_the_remaining_tensioners` | [docs/registry/implementation-map.yaml:L4568-L4578] |
| 232 | `_fake` | [docs/registry/implementation-map.yaml:L4579-L4589] |
| 233 | `open_water_dict` | [docs/registry/implementation-map.yaml:L4590-L4599] |
| 234 | `_doc` | [docs/registry/implementation-map.yaml:L4600-L4609] |
| 235 | `test_statics_tension_and_modes_agree_with_hand_checks` | [docs/registry/implementation-map.yaml:L4610-L4623] |
| 236 | `test_branch_tolerance_and_ring_buoyancy` | [docs/registry/implementation-map.yaml:L4624-L4634] |
| 237 | `_vertical` | [docs/registry/implementation-map.yaml:L4635-L4644] |
| 238 | `_mc` | [docs/registry/implementation-map.yaml:L4645-L4654] |
| 239 | `_case` | [docs/registry/implementation-map.yaml:L4655-L4666] |
| 240 | `_recorded_arguments` | [docs/registry/implementation-map.yaml:L4667-L4676] |
| 241 | `_deltas` | [docs/registry/implementation-map.yaml:L4677-L4686] |
| 242 | `_base_cfg` | [docs/registry/implementation-map.yaml:L4687-L4696] |
| 243 | `test_analytical_reference_reproduces_from_basis` | [docs/registry/implementation-map.yaml:L4697-L4706] |
| 244 | `_code_check_atlas` | [docs/registry/implementation-map.yaml:L4707-L4716] |
| 245 | `reference_input` | [docs/registry/implementation-map.yaml:L4717-L4726] |
| 246 | `settings` | [docs/registry/implementation-map.yaml:L4727-L4736] |
| 247 | `verify_configuration` | [docs/registry/implementation-map.yaml:L4737-L4746] |
| 248 | `_make_slay_spec` | [docs/registry/implementation-map.yaml:L4747-L4756] |
| 249 | `_make_pipeline_spec` | [docs/registry/implementation-map.yaml:L4757-L4766] |
| 250 | `_make_pipeline_spec` | [docs/registry/implementation-map.yaml:L4767-L4776] |
| 251 | `_write_slay_campaign` | [docs/registry/implementation-map.yaml:L4777-L4786] |
| 252 | `_make_base_spec_data` | [docs/registry/implementation-map.yaml:L4787-L4796] |
| 253 | `extract_static_range_results` | [docs/registry/implementation-map.yaml:L4797-L4806] |
| 254 | `_make_valid_model` | [docs/registry/implementation-map.yaml:L4807-L4816] |
| 255 | `test_support_types_are_defined_before_vessel_references` | [docs/registry/implementation-map.yaml:L4817-L4826] |
| 256 | `installation_parameters` | [docs/registry/implementation-map.yaml:L4827-L4836] |
| 257 | `run_catenary_riser` | [docs/registry/implementation-map.yaml:L4837-L4846] |
| 258 | `get_catenary_analysis_mock` | [docs/registry/implementation-map.yaml:L4847-L4856] |
| 259 | `<module>` | [docs/registry/implementation-map.yaml:L4857-L4865] |
| 260 | `process_effective_tension_file` | [docs/registry/implementation-map.yaml:L4866-L4875] |
| 261 | `main` | [docs/registry/implementation-map.yaml:L4876-L4887] |
| 262 | `_cfg` | [docs/registry/implementation-map.yaml:L4888-L4897] |
| 263 | `test_workflow_registry` | [docs/registry/implementation-map.yaml:L4898-L4907] |
| 264 | `screening_input` | [docs/registry/implementation-map.yaml:L4908-L4917] |

#### orcaflex-text-model-writers

| Record | Definition / source locator | Map record |
| --- | --- | --- |
| 1 | `main` | [docs/registry/implementation-map.yaml:L4940-L4949] |
| 2 | `<module>` | [docs/registry/implementation-map.yaml:L4950-L4958] |
| 3 | `main` | [docs/registry/implementation-map.yaml:L4959-L4968] |
| 4 | `main` | [docs/registry/implementation-map.yaml:L4969-L4978] |
| 5 | `strengthInputFilePreperation` | [docs/registry/implementation-map.yaml:L4979-L4988] |
| 6 | `run_orcawave_diffraction` | [docs/registry/implementation-map.yaml:L4989-L4998] |
| 7 | `run_diffraction_with_monitoring` | [docs/registry/implementation-map.yaml:L4999-L5008] |
| 8 | `run_batch_execution` | [docs/registry/implementation-map.yaml:L5009-L5020] |
| 9 | `_load_data_compat` | [docs/registry/implementation-map.yaml:L5021-L5030] |
| 10 | `_load_data_compat` | [docs/registry/implementation-map.yaml:L5031-L5040] |
| 11 | `_load_data_compat` | [docs/registry/implementation-map.yaml:L5041-L5050] |
| 12 | `_load_data_compat` | [docs/registry/implementation-map.yaml:L5051-L5060] |
| 13 | `OrcaFlexExporter.create_mooring_yml` | [docs/registry/implementation-map.yaml:L5061-L5070] |
| 14 | `write_audit_yaml` | [docs/registry/implementation-map.yaml:L5071-L5080] |
| 15 | `generate_yml` | [docs/registry/implementation-map.yaml:L5081-L5090] |
| 16 | `main` | [docs/registry/implementation-map.yaml:L5091-L5100] |
| 17 | `solve_and_extract` | [docs/registry/implementation-map.yaml:L5101-L5110] |
| 18 | `solve_owd` | [docs/registry/implementation-map.yaml:L5111-L5120] |
| 19 | `solve_spec` | [docs/registry/implementation-map.yaml:L5121-L5132] |
| 20 | `_solve_spec_via_orcfxapi` | [docs/registry/implementation-map.yaml:L5133-L5142] |
| 21 | `<module>` | [docs/registry/implementation-map.yaml:L5143-L5151] |
| 22 | `load_and_run` | [docs/registry/implementation-map.yaml:L5152-L5161] |
| 23 | `run_spec_driven` | [docs/registry/implementation-map.yaml:L5162-L5171] |
| 24 | `load_json` | [docs/registry/implementation-map.yaml:L5172-L5181] |
| 25 | `_trace` | [docs/registry/implementation-map.yaml:L5182-L5191] |
| 26 | `apply_mesh_preset` | [docs/registry/implementation-map.yaml:L5192-L5201] |
| 27 | `run_mesh_sensitivity` | [docs/registry/implementation-map.yaml:L5202-L5211] |
| 28 | `benchmark_model` | [docs/registry/implementation-map.yaml:L5212-L5223] |
| 29 | `make_chart_js` | [docs/registry/implementation-map.yaml:L5224-L5233] |
| 30 | `make_profile_js` | [docs/registry/implementation-map.yaml:L5234-L5243] |
| 31 | `extract_references` | [docs/registry/implementation-map.yaml:L5244-L5253] |
| 32 | `_build_manual_entries` | [docs/registry/implementation-map.yaml:L5254-L5263] |
| 33 | `OrcaFlexConverter.convert_file` | [docs/registry/implementation-map.yaml:L5264-L5273] |
| 34 | `create_flattened_model_dir` | [docs/registry/implementation-map.yaml:L5274-L5283] |
| 35 | `process_model_dir` | [docs/registry/implementation-map.yaml:L5284-L5293] |
| 36 | `_field_to_section_name` | [docs/registry/implementation-map.yaml:L5294-L5303] |
| 37 | `write_yaml` | [docs/registry/implementation-map.yaml:L5304-L5313] |
| 38 | `main` | [docs/registry/implementation-map.yaml:L5314-L5323] |
| 39 | `save_spec` | [docs/registry/implementation-map.yaml:L5324-L5333] |
| 40 | `create_env_section_wrappers` | [docs/registry/implementation-map.yaml:L5334-L5343] |
| 41 | `create_fixed_analysis_model` | [docs/registry/implementation-map.yaml:L5344-L5353] |
| 42 | `fix_sections_for_discretised` | [docs/registry/implementation-map.yaml:L5354-L5363] |
| 43 | `<module>` | [docs/registry/implementation-map.yaml:L5364-L5372] |
| 44 | `extract_spec` | [docs/registry/implementation-map.yaml:L5373-L5382] |
| 45 | `save_spec_yml` | [docs/registry/implementation-map.yaml:L5383-L5392] |
| 46 | `create_analysis_file` | [docs/registry/implementation-map.yaml:L5393-L5402] |
| 47 | `CALMBuoyProjectGenerator.generate_orcaflex_modules` | [docs/registry/implementation-map.yaml:L5403-L5415] |
| 48 | `CALMBuoyProjectGenerator._populate_environment_module` | [docs/registry/implementation-map.yaml:L5416-L5425] |
| 49 | `CALMBuoyProjectGenerator._apply_module_customizations` | [docs/registry/implementation-map.yaml:L5426-L5437] |
| 50 | `CALMBuoyProjectGenerator._update_base_file_paths` | [docs/registry/implementation-map.yaml:L5438-L5447] |
| 51 | `CALMBuoyProjectGenerator.save_updated_config` | [docs/registry/implementation-map.yaml:L5448-L5457] |
| 52 | `create_wave_file` | [docs/registry/implementation-map.yaml:L5458-L5467] |
| 53 | `create_current_file` | [docs/registry/implementation-map.yaml:L5468-L5477] |
| 54 | `create_wind_file` | [docs/registry/implementation-map.yaml:L5478-L5487] |
| 55 | `create_version_metadata_block` | [docs/registry/implementation-map.yaml:L5488-L5497] |
| 56 | `_report_type` | [docs/registry/implementation-map.yaml:L5498-L5507] |
| 57 | `classify` | [docs/registry/implementation-map.yaml:L5508-L5519] |
| 58 | `main` | [docs/registry/implementation-map.yaml:L5520-L5531] |
| 59 | `<module>` | [docs/registry/implementation-map.yaml:L5532-L5540] |
| 60 | `main` | [docs/registry/implementation-map.yaml:L5541-L5550] |
| 61 | `apply_mesh_preset` | [docs/registry/implementation-map.yaml:L5551-L5560] |
| 62 | `scr_reference` | [docs/registry/implementation-map.yaml:L5561-L5570] |
| 63 | `slwr_reference` | [docs/registry/implementation-map.yaml:L5571-L5580] |
| 64 | `calculate` | [docs/registry/implementation-map.yaml:L5581-L5593] |
| 65 | `case_spec` | [docs/registry/implementation-map.yaml:L5594-L5603] |
| 66 | `prepare_case` | [docs/registry/implementation-map.yaml:L5604-L5615] |
| 67 | `prepare` | [docs/registry/implementation-map.yaml:L5616-L5628] |
| 68 | `_generated_sections` | [docs/registry/implementation-map.yaml:L5629-L5638] |
| 69 | `ContentAnalyzer._categorize_folder` | [docs/registry/implementation-map.yaml:L5639-L5648] |
| 70 | `FrontmatterGenerator.frontmatter_to_yaml` | [docs/registry/implementation-map.yaml:L5649-L5658] |
| 71 | `<module>` | [docs/registry/implementation-map.yaml:L5659-L5667] |
| 72 | `make_comparison_loaded_vs_ballast_cxc` | [docs/registry/implementation-map.yaml:L5668-L5677] |
| 73 | `load_config` | [docs/registry/implementation-map.yaml:L5678-L5687] |
| 74 | `generate_optimized_config` | [docs/registry/implementation-map.yaml:L5688-L5697] |
| 75 | `create_config_with_gdf` | [docs/registry/implementation-map.yaml:L5698-L5707] |
| 76 | `create_simple_vessel_config` | [docs/registry/implementation-map.yaml:L5708-L5717] |
| 77 | `create_oc4_semisub_config` | [docs/registry/implementation-map.yaml:L5718-L5727] |
| 78 | `create_minimal_test_config` | [docs/registry/implementation-map.yaml:L5728-L5737] |
| 79 | `create_final_working_config` | [docs/registry/implementation-map.yaml:L5738-L5749] |
| 80 | `create_extended_version` | [docs/registry/implementation-map.yaml:L5750-L5759] |
| 81 | `create_tug_30m_1_working` | [docs/registry/implementation-map.yaml:L5760-L5769] |
| 82 | `fix_configuration` | [docs/registry/implementation-map.yaml:L5770-L5781] |
| 83 | `create_minimal_test_config` | [docs/registry/implementation-map.yaml:L5782-L5791] |
| 84 | `fix_working_model` | [docs/registry/implementation-map.yaml:L5792-L5801] |
| 85 | `create_minimal_with_test_box` | [docs/registry/implementation-map.yaml:L5802-L5811] |
| 86 | `OrcaWaveValidator.run_validation` | [docs/registry/implementation-map.yaml:L5812-L5823] |
| 87 | `create_batch_config` | [docs/registry/implementation-map.yaml:L5824-L5833] |
| 88 | `OrcaFlexTestStandardizer.create_standard_config` | [docs/registry/implementation-map.yaml:L5834-L5843] |
| 89 | `run_library` | [docs/registry/implementation-map.yaml:L5844-L5853] |
| 90 | `main` | [docs/registry/implementation-map.yaml:L5854-L5863] |
| 91 | `<module>` | [docs/registry/implementation-map.yaml:L5864-L5872] |
| 92 | `process_dat_file` | [docs/registry/implementation-map.yaml:L5873-L5882] |
| 93 | `<module>` | [docs/registry/implementation-map.yaml:L5883-L5891] |
| 94 | `convert_dat_to_yml` | [docs/registry/implementation-map.yaml:L5892-L5901] |
| 95 | `update_mooring_lines` | [docs/registry/implementation-map.yaml:L5902-L5911] |
| 96 | `update_mooring_lines` | [docs/registry/implementation-map.yaml:L5912-L5921] |
| 97 | `update_skill_file` | [docs/registry/implementation-map.yaml:L5922-L5933] |
| 98 | `ModelValidator.extract_model_info` | [docs/registry/implementation-map.yaml:L5934-L5943] |
| 99 | `_save_body` | [docs/registry/implementation-map.yaml:L5944-L5953] |
| 100 | `main` | [docs/registry/implementation-map.yaml:L5954-L5965] |
| 101 | `SaveData.saveDataYaml` | [docs/registry/implementation-map.yaml:L5966-L5975] |
| 102 | `SaveData.WriteOrcaFlexYMLFile` | [docs/registry/implementation-map.yaml:L5976-L5985] |
| 103 | `<module>` | [docs/registry/implementation-map.yaml:L5986-L5994] |
| 104 | `StreamHandler._format_orcaflex_yml` | [docs/registry/implementation-map.yaml:L5995-L6004] |
| 105 | `MooringClient.to_orcaflex_mooring` | [docs/registry/implementation-map.yaml:L6005-L6014] |
| 106 | `ChainDatabaseClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6015-L6024] |
| 107 | `SyntheticRopeDatabaseClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6025-L6034] |
| 108 | `WireRopeDatabaseClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6035-L6044] |
| 109 | `PipeSpecificationClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6045-L6054] |
| 110 | `VesselClient.to_orcaflex_vessel` | [docs/registry/implementation-map.yaml:L6055-L6064] |
| 111 | `case_spec` | [docs/registry/implementation-map.yaml:L6065-L6074] |
| 112 | `_probe` | [docs/registry/implementation-map.yaml:L6075-L6084] |
| 113 | `_capped_probe` | [docs/registry/implementation-map.yaml:L6085-L6096] |
| 114 | `_statics_path` | [docs/registry/implementation-map.yaml:L6097-L6109] |
| 115 | `physical_state_checks` | [docs/registry/implementation-map.yaml:L6110-L6119] |
| 116 | `_line_type` | [docs/registry/implementation-map.yaml:L6120-L6129] |
| 117 | `build_generic_spec` | [docs/registry/implementation-map.yaml:L6130-L6141] |
| 118 | `write_model` | [docs/registry/implementation-map.yaml:L6142-L6153] |
| 119 | `reference_periods` | [docs/registry/implementation-map.yaml:L6154-L6163] |
| 120 | `build_open_water_generic_spec` | [docs/registry/implementation-map.yaml:L6164-L6173] |
| 121 | `extract` | [docs/registry/implementation-map.yaml:L6174-L6183] |
| 122 | `merge_stress_range` | [docs/registry/implementation-map.yaml:L6184-L6193] |
| 123 | `extract_significant` | [docs/registry/implementation-map.yaml:L6194-L6203] |
| 124 | `prepare_variant` | [docs/registry/implementation-map.yaml:L6204-L6213] |
| 125 | `OrcaFlexConverter.convert_to_vessel_yaml` | [docs/registry/implementation-map.yaml:L6214-L6223] |
| 126 | `_add_fdf_surface_traces` | [docs/registry/implementation-map.yaml:L6224-L6233] |
| 127 | `run_aqwa` | [docs/registry/implementation-map.yaml:L6234-L6243] |
| 128 | `DiffractionSpec.to_yaml` | [docs/registry/implementation-map.yaml:L6244-L6253] |
| 129 | `OrcaFlexExporter.export_vessel_type` | [docs/registry/implementation-map.yaml:L6254-L6263] |
| 130 | `_write_yml` | [docs/registry/implementation-map.yaml:L6264-L6273] |
| 131 | `OrcaWaveRunner._execute_via_api` | [docs/registry/implementation-map.yaml:L6274-L6283] |
| 132 | `PanelCatalog.to_yaml` | [docs/registry/implementation-map.yaml:L6284-L6293] |
| 133 | `RAOAnalysis.write_filtered_orcaflex_seastate_rao_file` | [docs/registry/implementation-map.yaml:L6294-L6303] |
| 134 | `RAOAnalysis.get_shape_data` | [docs/registry/implementation-map.yaml:L6304-L6313] |
| 135 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.get_viv_modes_excited` | [docs/registry/implementation-map.yaml:L6314-L6323] |
| 136 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L6324-L6333] |
| 137 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.tabulate_overall_viv_results` | [docs/registry/implementation-map.yaml:L6334-L6343] |
| 138 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.save_viv_histograms` | [docs/registry/implementation-map.yaml:L6344-L6353] |
| 139 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.save_combined_histograms` | [docs/registry/implementation-map.yaml:L6354-L6363] |
| 140 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L6364-L6373] |
| 141 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | [docs/registry/implementation-map.yaml:L6374-L6383] |
| 142 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` / `DNVRPH103_hydrodynamics_circular.save_model` | [docs/registry/implementation-map.yaml:L6384-L6393] |
| 143 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` / `DNVRPH103_hydrodynamics_circular.update_6d_buoy` | [docs/registry/implementation-map.yaml:L6394-L6403] |
| 144 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` / `DNVRPH103_hydrodynamics_rectangular.save_model` | [docs/registry/implementation-map.yaml:L6404-L6413] |
| 145 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` / `DNVRPH103_hydrodynamics_rectangular.update_6d_buoy` | [docs/registry/implementation-map.yaml:L6414-L6423] |
| 146 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L6424-L6436] |
| 147 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.riser_joints_to_line_types` | [docs/registry/implementation-map.yaml:L6437-L6448] |
| 148 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.GetGeneral` | [docs/registry/implementation-map.yaml:L6449-L6458] |
| 149 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.lineTypes` | [docs/registry/implementation-map.yaml:L6459-L6468] |
| 150 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.WriteOrcaflexModel` | [docs/registry/implementation-map.yaml:L6469-L6478] |
| 151 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.build_model` | [docs/registry/implementation-map.yaml:L6479-L6490] |
| 152 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` / `FEAComponents.AssignGeneral` | [docs/registry/implementation-map.yaml:L6491-L6500] |
| 153 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` / `FEAComponents.AssignBuoyProperties` | [docs/registry/implementation-map.yaml:L6501-L6510] |
| 154 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` / `FEAComponents.AssignLineProperties` | [docs/registry/implementation-map.yaml:L6511-L6520] |
| 155 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` / `FEAComponents.AssignLineTypeProperties` | [docs/registry/implementation-map.yaml:L6521-L6530] |
| 156 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` / `FEAComponents.AssignVesselProperties` | [docs/registry/implementation-map.yaml:L6531-L6540] |
| 157 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` / `FEAComponents.save_output` | [docs/registry/implementation-map.yaml:L6541-L6550] |
| 158 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` / `FEAComponents.load_variations` | [docs/registry/implementation-map.yaml:L6551-L6560] |
| 159 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.get_viv_modes_excited` | [docs/registry/implementation-map.yaml:L6561-L6570] |
| 160 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L6571-L6580] |
| 161 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.tabulate_overall_viv_results` | [docs/registry/implementation-map.yaml:L6581-L6590] |
| 162 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.save_viv_histograms` | [docs/registry/implementation-map.yaml:L6591-L6600] |
| 163 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.save_combined_histograms` | [docs/registry/implementation-map.yaml:L6601-L6610] |
| 164 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L6611-L6620] |
| 165 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | [docs/registry/implementation-map.yaml:L6621-L6630] |
| 166 | `SaveData.saveDataYaml` | [docs/registry/implementation-map.yaml:L6631-L6640] |
| 167 | `SaveData.WriteOrcaFlexYMLFile` | [docs/registry/implementation-map.yaml:L6641-L6650] |
| 168 | `<module>` | [docs/registry/implementation-map.yaml:L6651-L6659] |
| 169 | `generate_input` | [docs/registry/implementation-map.yaml:L6660-L6672] |
| 170 | `JumperInstallationSpec.to_yaml` | [docs/registry/implementation-map.yaml:L6673-L6682] |
| 171 | `build_line_types` | [docs/registry/implementation-map.yaml:L6683-L6692] |
| 172 | `build_modular_spec` | [docs/registry/implementation-map.yaml:L6693-L6704] |
| 173 | `UnifiedRAOReader._detect_format` | [docs/registry/implementation-map.yaml:L6705-L6714] |
| 174 | `VesselConfig.to_orcaflex_dict` | [docs/registry/implementation-map.yaml:L6715-L6724] |
| 175 | `Buoy6DConfig.to_orcaflex_dict` | [docs/registry/implementation-map.yaml:L6725-L6734] |
| 176 | `LineConfig.to_orcaflex_dict` | [docs/registry/implementation-map.yaml:L6735-L6744] |
| 177 | `build_scr_model` | [docs/registry/implementation-map.yaml:L6745-L6754] |
| 178 | `build_lazy_wave_model` | [docs/registry/implementation-map.yaml:L6755-L6764] |
| 179 | `build_mooring_line` | [docs/registry/implementation-map.yaml:L6765-L6774] |
| 180 | `build_model_summary` | [docs/registry/implementation-map.yaml:L6775-L6784] |
| 181 | `build_range_graphs` | [docs/registry/implementation-map.yaml:L6785-L6794] |
| 182 | `build_rao_plots` | [docs/registry/implementation-map.yaml:L6795-L6804] |
| 183 | `_write_result` | [docs/registry/implementation-map.yaml:L6805-L6814] |
| 184 | `orcawave_depth_sweep.runner` | [docs/registry/implementation-map.yaml:L6815-L6824] |
| 185 | `_write_diffraction_inputs` | [docs/registry/implementation-map.yaml:L6825-L6834] |
| 186 | `LineType.get_general` | [docs/registry/implementation-map.yaml:L6835-L6844] |
| 187 | `General.getGeneralOrcaFlexSetUp` | [docs/registry/implementation-map.yaml:L6845-L6854] |
| 188 | `load_csv` | [docs/registry/implementation-map.yaml:L6855-L6864] |
| 189 | `get_data` | [docs/registry/implementation-map.yaml:L6865-L6874] |
| 190 | `OrcaFlexConfig.to_yaml` | [docs/registry/implementation-map.yaml:L6875-L6884] |
| 191 | `OrcaFlexModelWrapper.save_simulation` | [docs/registry/implementation-map.yaml:L6885-L6894] |
| 192 | `FeatureAnalyzer.analyze_file` | [docs/registry/implementation-map.yaml:L6895-L6904] |
| 193 | `FeatureAnalyzer.generate_summary` | [docs/registry/implementation-map.yaml:L6905-L6914] |
| 194 | `OrcaFlexBatchConverter._real_convert` | [docs/registry/implementation-map.yaml:L6915-L6927] |
| 195 | `OrcaFlexBatchConverter._mock_convert` | [docs/registry/implementation-map.yaml:L6928-L6937] |
| 196 | `YamlConverter.convert_batch` | [docs/registry/implementation-map.yaml:L6938-L6947] |
| 197 | `DownloadedExamplesIntegrator._infer_components` | [docs/registry/implementation-map.yaml:L6948-L6957] |
| 198 | `DownloadedExamplesIntegrator._extract_common_configs` | [docs/registry/implementation-map.yaml:L6958-L6967] |
| 199 | `KnowledgeIntegrator._extract_patterns` | [docs/registry/implementation-map.yaml:L6968-L6977] |
| 200 | `KnowledgeIntegrator.build_searchable_index` | [docs/registry/implementation-map.yaml:L6978-L6987] |
| 201 | `OrcFxAPIConverter._initialize_api` | [docs/registry/implementation-map.yaml:L6988-L6997] |
| 202 | `OrcFxAPIConverter.convert_file` | [docs/registry/implementation-map.yaml:L6998-L7007] |
| 203 | `detect_format` | [docs/registry/implementation-map.yaml:L7008-L7017] |
| 204 | `ModularToSingleConverter.convert` | [docs/registry/implementation-map.yaml:L7018-L7031] |
| 205 | `ModularToSingleConverter._merge_master` | [docs/registry/implementation-map.yaml:L7032-L7041] |
| 206 | `ModularToSingleConverter._extract_includefiles.traverse` | [docs/registry/implementation-map.yaml:L7042-L7051] |
| 207 | `ModularToSpecConverter.convert` | [docs/registry/implementation-map.yaml:L7052-L7061] |
| 208 | `SingleToModularConverter._write_master_file` | [docs/registry/implementation-map.yaml:L7062-L7071] |
| 209 | `SingleToModularConverter._write_parameters_file` | [docs/registry/implementation-map.yaml:L7072-L7081] |
| 210 | `_resolve_mooring` | [docs/registry/implementation-map.yaml:L7082-L7091] |
| 211 | `_resolve_riser` | [docs/registry/implementation-map.yaml:L7092-L7101] |
| 212 | `LibraryGenerator.generate_component` | [docs/registry/implementation-map.yaml:L7102-L7111] |
| 213 | `LibraryGenerator.write_index` | [docs/registry/implementation-map.yaml:L7112-L7121] |
| 214 | `object_counts` | [docs/registry/implementation-map.yaml:L7122-L7131] |
| 215 | `geometry` | [docs/registry/implementation-map.yaml:L7132-L7141] |
| 216 | `OrcaFlexModelGenerator.generate_from_template` | [docs/registry/implementation-map.yaml:L7142-L7153] |
| 217 | `OrcaFlexModelGenerator._assemble_model` | [docs/registry/implementation-map.yaml:L7154-L7163] |
| 218 | `ModularModelGenerator.generate` | [docs/registry/implementation-map.yaml:L7164-L7176] |
| 219 | `ModularModelGenerator._write_parameters` | [docs/registry/implementation-map.yaml:L7177-L7186] |
| 220 | `ModularModelGenerator._write_master` | [docs/registry/implementation-map.yaml:L7187-L7196] |
| 221 | `ModularModelGenerator.generate_with_overrides` | [docs/registry/implementation-map.yaml:L7197-L7207] |
| 222 | `GeneralBuilder.build` | [docs/registry/implementation-map.yaml:L7208-L7217] |
| 223 | `GenericModelBuilder.build` | [docs/registry/implementation-map.yaml:L7218-L7227] |
| 224 | `LinesBuilder.build` | [docs/registry/implementation-map.yaml:L7228-L7237] |
| 225 | `LineTypeBuilder.build` | [docs/registry/implementation-map.yaml:L7238-L7247] |
| 226 | `RiserLinesBuilder.build` | [docs/registry/implementation-map.yaml:L7248-L7257] |
| 227 | `RiserLineTypeBuilder.build` | [docs/registry/implementation-map.yaml:L7258-L7269] |
| 228 | `RiserLineTypeBuilder._build_riser_line_type` | [docs/registry/implementation-map.yaml:L7270-L7279] |
| 229 | `RiserVesselBuilder.build` | [docs/registry/implementation-map.yaml:L7280-L7289] |
| 230 | `VesselBuilder.build` | [docs/registry/implementation-map.yaml:L7290-L7299] |
| 231 | `WinchBuilder.build` | [docs/registry/implementation-map.yaml:L7300-L7309] |
| 232 | `cmd_generate` | [docs/registry/implementation-map.yaml:L7310-L7319] |
| 233 | `MonolithicExtractor._extract_simulation` | [docs/registry/implementation-map.yaml:L7320-L7329] |
| 234 | `MonolithicExtractor._extract_general_properties` | [docs/registry/implementation-map.yaml:L7330-L7339] |
| 235 | `PostGenerationValidator._check_line_type_references` | [docs/registry/implementation-map.yaml:L7340-L7349] |
| 236 | `PostGenerationValidator._check_vessel_references` | [docs/registry/implementation-map.yaml:L7350-L7359] |
| 237 | `PostGenerationValidator._check_winch_references` | [docs/registry/implementation-map.yaml:L7360-L7369] |
| 238 | `_iter_includefile_refs.traverse` | [docs/registry/implementation-map.yaml:L7370-L7379] |
| 239 | `MooringRouter.route` | [docs/registry/implementation-map.yaml:L7380-L7389] |
| 240 | `MooringRouter._chain_linetype` | [docs/registry/implementation-map.yaml:L7390-L7399] |
| 241 | `MooringRouter._wire_linetype` | [docs/registry/implementation-map.yaml:L7400-L7409] |
| 242 | `MooringRouter._polyester_linetype` | [docs/registry/implementation-map.yaml:L7410-L7419] |
| 243 | `VesselRouter.route` | [docs/registry/implementation-map.yaml:L7420-L7429] |
| 244 | `VesselRouter.from_hull_catalog` | [docs/registry/implementation-map.yaml:L7430-L7439] |
| 245 | `CampaignGenerator.generate` | [docs/registry/implementation-map.yaml:L7440-L7449] |
| 246 | `write_variation_model` | [docs/registry/implementation-map.yaml:L7450-L7459] |
| 247 | `extract_includefiles.traverse` | [docs/registry/implementation-map.yaml:L7460-L7469] |
| 248 | `ModularInputValidator._collect_issues_from_level_1` | [docs/registry/implementation-map.yaml:L7470-L7479] |
| 249 | `Mooring.prepare_includefile_for_all_lines` | [docs/registry/implementation-map.yaml:L7480-L7489] |
| 250 | `Mooring.prepare_includefile_for_fender_compression` | [docs/registry/implementation-map.yaml:L7490-L7499] |
| 251 | `MockOrcaFlexAPI.load_model` | [docs/registry/implementation-map.yaml:L7500-L7509] |
| 252 | `OrcaFlexAPI.save_model` | [docs/registry/implementation-map.yaml:L7510-L7519] |
| 253 | `OperabilityAnalyzer.generate_operability_envelope` | [docs/registry/implementation-map.yaml:L7520-L7529] |
| 254 | `OPPVisualization.combine_models` | [docs/registry/implementation-map.yaml:L7530-L7539] |
| 255 | `OrcaFlexAnalysis.save_data` | [docs/registry/implementation-map.yaml:L7540-L7551] |
| 256 | `OrcaFlexAnalysis.saveSummaryToExcel` | [docs/registry/implementation-map.yaml:L7552-L7561] |
| 257 | `OrcaFlexAnalysis.save_histograms` | [docs/registry/implementation-map.yaml:L7562-L7571] |
| 258 | `OrcaFlexAnalysis.save_cfg_files_from_multiple_files` | [docs/registry/implementation-map.yaml:L7572-L7581] |
| 259 | `OrcaFlexAnalysis.save_RAOs` | [docs/registry/implementation-map.yaml:L7582-L7591] |
| 260 | `OrcaFlexConverterEnhanced._real_convert` | [docs/registry/implementation-map.yaml:L7592-L7603] |
| 261 | `OrcaFlexConverterEnhanced._mock_convert` | [docs/registry/implementation-map.yaml:L7604-L7613] |
| 262 | `OrcaFlexCustomAnalysis.process_fea` | [docs/registry/implementation-map.yaml:L7614-L7626] |
| 263 | `OrcaFlexCustomAnalysis.clean_model` | [docs/registry/implementation-map.yaml:L7627-L7636] |
| 264 | `OrcaFlexCustomAnalysis.save_model_with_calculated_positions` | [docs/registry/implementation-map.yaml:L7637-L7646] |
| 265 | `OrcaFlexCustomAnalysis.iterate_to_target_value` | [docs/registry/implementation-map.yaml:L7647-L7656] |
| 266 | `OrcaFlexCustomAnalysis.save_data` | [docs/registry/implementation-map.yaml:L7657-L7666] |
| 267 | `OrcInstallation.create_model_for_water_depth` | [docs/registry/implementation-map.yaml:L7667-L7676] |
| 268 | `OrcInstallation.create_installation_depth_model` | [docs/registry/implementation-map.yaml:L7677-L7686] |
| 269 | `OrcInstallation.create_installation_depth_model2` | [docs/registry/implementation-map.yaml:L7687-L7696] |
| 270 | `OrcaflexIterativeRuns.save_iterative_file` | [docs/registry/implementation-map.yaml:L7697-L7706] |
| 271 | `OrcaflexIterativeRuns.run_iterative_simulations` | [docs/registry/implementation-map.yaml:L7707-L7716] |
| 272 | `OrcaflexLineTypes.get_umbilical_properties` | [docs/registry/implementation-map.yaml:L7717-L7729] |
| 273 | `OrcaflexLineTypes.assign_key_umbilical_properties` | [docs/registry/implementation-map.yaml:L7730-L7739] |
| 274 | `OrcaflexLineTypes.save_model` | [docs/registry/implementation-map.yaml:L7740-L7749] |
| 275 | `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L7750-L7762] |
| 276 | `OrcaflexModelComponents.riser_joints_to_line_types` | [docs/registry/implementation-map.yaml:L7763-L7774] |
| 277 | `OrcaflexModelComponents.GetGeneral` | [docs/registry/implementation-map.yaml:L7775-L7784] |
| 278 | `OrcaflexModelComponents.lineTypes` | [docs/registry/implementation-map.yaml:L7785-L7794] |
| 279 | `OrcaflexModelComponents.WriteOrcaflexModel` | [docs/registry/implementation-map.yaml:L7795-L7804] |
| 280 | `OrcaflexModelComponents.build_model` | [docs/registry/implementation-map.yaml:L7805-L7816] |
| 281 | `OrcaFlexOptimizedParallelAnalysis.process_single_file` | [docs/registry/implementation-map.yaml:L7817-L7826] |
| 282 | `process_single_file_worker` | [docs/registry/implementation-map.yaml:L7827-L7836] |
| 283 | `OrcaFlexParallelAnalysis.process_single_file` | [docs/registry/implementation-map.yaml:L7837-L7846] |
| 284 | `OrcaflexUtilities.update_model` | [docs/registry/implementation-map.yaml:L7847-L7856] |
| 285 | `OrcaflexUtilities.update_input_file` | [docs/registry/implementation-map.yaml:L7857-L7866] |
| 286 | `OrcaflexUtilities.get_full_yaml_file_and_save` | [docs/registry/implementation-map.yaml:L7867-L7878] |
| 287 | `OrcaflexUtilities.get_general_yml` | [docs/registry/implementation-map.yaml:L7879-L7888] |
| 288 | `OrcaflexUtilities.save_cfg_files_from_multiple_files` | [docs/registry/implementation-map.yaml:L7889-L7898] |
| 289 | `convert_to_yml` | [docs/registry/implementation-map.yaml:L7899-L7908] |
| 290 | `PipelineSchematicGenerator._merge_yaml` | [docs/registry/implementation-map.yaml:L7909-L7921] |
| 291 | `PipelineSchematicGenerator._merge_lines` | [docs/registry/implementation-map.yaml:L7922-L7931] |
| 292 | `PipelineSchematicGenerator._merge_6dbuoys` | [docs/registry/implementation-map.yaml:L7932-L7941] |
| 293 | `PipelineSchematicGenerator._parse_pipeline_path` | [docs/registry/implementation-map.yaml:L7942-L7951] |
| 294 | `PipelineSchematicGenerator._parse_boundary_conditions` | [docs/registry/implementation-map.yaml:L7952-L7961] |
| 295 | `PipelineSchematicGenerator.create_plan_view` | [docs/registry/implementation-map.yaml:L7962-L7971] |
| 296 | `PipelineSchematicGenerator.create_elevation_view` | [docs/registry/implementation-map.yaml:L7972-L7981] |
| 297 | `LoadVessel.aqwa_group_item` | [docs/registry/implementation-map.yaml:L7982-L7991] |
| 298 | `_extract_dynamic_results_all` | [docs/registry/implementation-map.yaml:L7992-L8001] |
| 299 | `RiserRenderer._build_riser_dynamic_results` | [docs/registry/implementation-map.yaml:L8002-L8011] |
| 300 | `find_model_files` | [docs/registry/implementation-map.yaml:L8012-L8021] |
| 301 | `AuthoredSpec.to_spec_yaml` | [docs/registry/implementation-map.yaml:L8022-L8031] |
| 302 | `TemplateGenerator.generate` | [docs/registry/implementation-map.yaml:L8032-L8043] |
| 303 | `TemplateGenerator._generate_reference` | [docs/registry/implementation-map.yaml:L8044-L8053] |
| 304 | `InstallationVtoHost.installation_step` | [docs/registry/implementation-map.yaml:L8054-L8063] |
| 305 | `InstallationVtoHost.get_target_line_length` | [docs/registry/implementation-map.yaml:L8064-L8073] |
| 306 | `InstallationVtoHost.get_target_reference_distance` | [docs/registry/implementation-map.yaml:L8074-L8083] |
| 307 | `Lower2ndEnd.installation_step` | [docs/registry/implementation-map.yaml:L8084-L8093] |
| 308 | `Lower2ndEnd.get_target_line_length` | [docs/registry/implementation-map.yaml:L8094-L8103] |
| 309 | `Lower2ndEnd.get_target_reference_distance` | [docs/registry/implementation-map.yaml:L8104-L8113] |
| 310 | `UniversalOrcaFlexRunner._process_single_model` | [docs/registry/implementation-map.yaml:L8114-L8123] |
| 311 | `create_config` | [docs/registry/implementation-map.yaml:L8124-L8133] |
| 312 | `_mapping_issues` | [docs/registry/implementation-map.yaml:L8134-L8143] |
| 313 | `inspect_source` | [docs/registry/implementation-map.yaml:L8144-L8155] |
| 314 | `orcaflex_dump` | [docs/registry/implementation-map.yaml:L8156-L8165] |
| 315 | `orcaflex_load` | [docs/registry/implementation-map.yaml:L8166-L8175] |
| 316 | `OrcaFlexYAMLValidator._detect_format` | [docs/registry/implementation-map.yaml:L8176-L8185] |
| 317 | `OrcaFlexYAMLValidator._check_singular_section_keys` | [docs/registry/implementation-map.yaml:L8186-L8195] |
| 318 | `OrcaWaveOrchestrator._create_vessel_config` | [docs/registry/implementation-map.yaml:L8196-L8205] |
| 319 | `OrcaWaveOrchestrator.list_available_vessels` | [docs/registry/implementation-map.yaml:L8206-L8215] |
| 320 | `OrcaFlexConverter.save_orcaflex_file` | [docs/registry/implementation-map.yaml:L8216-L8227] |
| 321 | `OrcaFlexConverter._save_compact_format` | [docs/registry/implementation-map.yaml:L8228-L8237] |
| 322 | `_tree` | [docs/registry/implementation-map.yaml:L8238-L8247] |
| 323 | `check_settings` | [docs/registry/implementation-map.yaml:L8248-L8257] |
| 324 | `_saved_proof` | [docs/registry/implementation-map.yaml:L8258-L8267] |
| 325 | `prepareFatiguePostProcessing` | [docs/registry/implementation-map.yaml:L8268-L8279] |
| 326 | `orcaflexModel` | [docs/registry/implementation-map.yaml:L8280-L8290] |
| 327 | `lineTypes` | [docs/registry/implementation-map.yaml:L8291-L8300] |
| 328 | `WriteOrcaflexModel` | [docs/registry/implementation-map.yaml:L8301-L8310] |
| 329 | `build_model` | [docs/registry/implementation-map.yaml:L8311-L8323] |
| 330 | `OrcaFlexModelGenerator.generate_model_yml` | [docs/registry/implementation-map.yaml:L8324-L8333] |
| 331 | `OrcaFlexModelGenerator.generate_model_dict` | [docs/registry/implementation-map.yaml:L8334-L8343] |
| 332 | `VerticalRiser.get_fea_properties` | [docs/registry/implementation-map.yaml:L8344-L8353] |
| 333 | `save_echo` | [docs/registry/implementation-map.yaml:L8354-L8363] |
| 334 | `OrcaFlex.get` | [docs/registry/implementation-map.yaml:L8364-L8373] |
| 335 | `OrcaFlex.get` | [docs/registry/implementation-map.yaml:L8374-L8383] |
| 336 | `OrcaFlex.get` | [docs/registry/implementation-map.yaml:L8384-L8393] |
| 337 | `AnalysisMetadataGenerator._detect_workflow_type` | [docs/registry/implementation-map.yaml:L8394-L8403] |
| 338 | `integration_basis` | [docs/registry/implementation-map.yaml:L8404-L8413] |
| 339 | `named_body` | [docs/registry/implementation-map.yaml:L8414-L8423] |
| 340 | `expected_change` | [docs/registry/implementation-map.yaml:L8424-L8435] |
| 341 | `read_cases` | [docs/registry/implementation-map.yaml:L8436-L8445] |
| 342 | `_write_request` | [docs/registry/implementation-map.yaml:L8446-L8457] |
| 343 | `materialize_bracket_case` | [docs/registry/implementation-map.yaml:L8458-L8470] |
| 344 | `_source` | [docs/registry/implementation-map.yaml:L8471-L8480] |
| 345 | `_case_files` | [docs/registry/implementation-map.yaml:L8481-L8492] |
| 346 | `_apply_change` | [docs/registry/implementation-map.yaml:L8493-L8505] |
| 347 | `_canonical_tables` | [docs/registry/implementation-map.yaml:L8506-L8517] |
| 348 | `_runtime` | [docs/registry/implementation-map.yaml:L8518-L8527] |
| 349 | `_field` | [docs/registry/implementation-map.yaml:L8528-L8537] |
| 350 | `_line` | [docs/registry/implementation-map.yaml:L8538-L8547] |
| 351 | `_value` | [docs/registry/implementation-map.yaml:L8548-L8560] |
| 352 | `_anchors` | [docs/registry/implementation-map.yaml:L8561-L8572] |
| 353 | `_timing` | [docs/registry/implementation-map.yaml:L8573-L8582] |
| 354 | `_ordered` | [docs/registry/implementation-map.yaml:L8583-L8596] |
| 355 | `_apply_native` | [docs/registry/implementation-map.yaml:L8597-L8608] |
| 356 | `_counts` | [docs/registry/implementation-map.yaml:L8609-L8619] |
| 357 | `_variation` | [docs/registry/implementation-map.yaml:L8620-L8632] |
| 358 | `_native_case` | [docs/registry/implementation-map.yaml:L8633-L8647] |
| 359 | `_request` | [docs/registry/implementation-map.yaml:L8648-L8657] |
| 360 | `_snapshot` | [docs/registry/implementation-map.yaml:L8658-L8667] |
| 361 | `_structure` | [docs/registry/implementation-map.yaml:L8668-L8677] |
| 362 | `_request` | [docs/registry/implementation-map.yaml:L8678-L8687] |
| 363 | `generate_case` | [docs/registry/implementation-map.yaml:L8688-L8701] |
| 364 | `prepare_matrix` | [docs/registry/implementation-map.yaml:L8702-L8714] |
| 365 | `_change_payload` | [docs/registry/implementation-map.yaml:L8715-L8724] |
| 366 | `materialize_case` | [docs/registry/implementation-map.yaml:L8725-L8739] |
| 367 | `_verify_master` | [docs/registry/implementation-map.yaml:L8740-L8749] |
| 368 | `_verify_settings` | [docs/registry/implementation-map.yaml:L8750-L8759] |
| 369 | `prepare_variant_study` | [docs/registry/implementation-map.yaml:L8760-L8773] |
| 370 | `OrcaWaveMCPServer._register_tools.export_to_orcaflex` | [docs/registry/implementation-map.yaml:L8774-L8783] |
| 371 | `_prepare` | [docs/registry/implementation-map.yaml:L8784-L8793] |
| 372 | `extract_results` | [docs/registry/implementation-map.yaml:L8794-L8803] |
| 373 | `_write_yaml` | [docs/registry/implementation-map.yaml:L8804-L8813] |
| 374 | `SkillResolver._parse_skill_file` | [docs/registry/implementation-map.yaml:L8814-L8823] |
| 375 | `_model` | [docs/registry/implementation-map.yaml:L8824-L8833] |
| 376 | `_apply` | [docs/registry/implementation-map.yaml:L8834-L8843] |
| 377 | `build_candidate` | [docs/registry/implementation-map.yaml:L8844-L8856] |
| 378 | `_dependencies` | [docs/registry/implementation-map.yaml:L8857-L8866] |
| 379 | `base_spec` | [docs/registry/implementation-map.yaml:L8867-L8876] |
| 380 | `base_spec` | [docs/registry/implementation-map.yaml:L8877-L8886] |
| 381 | `_case` | [docs/registry/implementation-map.yaml:L8887-L8896] |
| 382 | `_generic` | [docs/registry/implementation-map.yaml:L8897-L8908] |
| 383 | `_case` | [docs/registry/implementation-map.yaml:L8909-L8918] |
| 384 | `_ow_case` | [docs/registry/implementation-map.yaml:L8919-L8928] |
| 385 | `base_spec` | [docs/registry/implementation-map.yaml:L8929-L8938] |
| 386 | `_line` | [docs/registry/implementation-map.yaml:L8939-L8948] |
| 387 | `base_spec` | [docs/registry/implementation-map.yaml:L8949-L8958] |
| 388 | `_doc` | [docs/registry/implementation-map.yaml:L8959-L8970] |
| 389 | `setup` | [docs/registry/implementation-map.yaml:L8971-L8980] |
| 390 | `test_deep_reference_and_registry_paths_are_excluded` | [docs/registry/implementation-map.yaml:L8981-L8990] |
| 391 | `test_generator_partitions_move_keep_hold_and_excluded` | [docs/registry/implementation-map.yaml:L8991-L9000] |
| 392 | `test_specs_match_basis_and_static_geometry` | [docs/registry/implementation-map.yaml:L9001-L9010] |
| 393 | `test_analytical_reference_reproduces_from_basis` | [docs/registry/implementation-map.yaml:L9011-L9020] |
| 394 | `TestModularToSpecBoundaryReporting.test_expected_gaps_classified` | [docs/registry/implementation-map.yaml:L9021-L9030] |
| 395 | `TestModularToSpecBoundaryReporting.test_actionable_gaps_classified` | [docs/registry/implementation-map.yaml:L9031-L9040] |
| 396 | `TestModularToSpecBoundaryReporting.test_all_unmapped_are_categorized` | [docs/registry/implementation-map.yaml:L9041-L9050] |
| 397 | `_build` | [docs/registry/implementation-map.yaml:L9051-L9060] |
| 398 | `_build` | [docs/registry/implementation-map.yaml:L9061-L9070] |
| 399 | `_build` | [docs/registry/implementation-map.yaml:L9071-L9080] |
| 400 | `main` | [docs/registry/implementation-map.yaml:L9081-L9090] |
| 401 | `_write_floating_campaign` | [docs/registry/implementation-map.yaml:L9091-L9100] |
| 402 | `_write_slay_campaign` | [docs/registry/implementation-map.yaml:L9101-L9110] |
| 403 | `_write_campaign_yaml` | [docs/registry/implementation-map.yaml:L9111-L9120] |
| 404 | `_write_model` | [docs/registry/implementation-map.yaml:L9121-L9130] |
| 405 | `_sections` | [docs/registry/implementation-map.yaml:L9131-L9140] |
| 406 | `test_generation_writes_master_and_includes` | [docs/registry/implementation-map.yaml:L9141-L9152] |
| 407 | `TestJumperLineTypes.test_three_jumper_variants_emitted` | [docs/registry/implementation-map.yaml:L9153-L9162] |
| 408 | `TestJumperLineReferences.test_jumper_line_uses_all_three_variants` | [docs/registry/implementation-map.yaml:L9163-L9172] |
| 409 | `_assert_line_type_references_resolve` | [docs/registry/implementation-map.yaml:L9173-L9182] |
| 410 | `_write_mono_yaml` | [docs/registry/implementation-map.yaml:L9183-L9192] |
| 411 | `find_model_files` | [docs/registry/implementation-map.yaml:L9193-L9202] |
| 412 | `_build_boundary_conditions` | [docs/registry/implementation-map.yaml:L9203-L9212] |
| 413 | `SingleFileParallelStrategy._get_line_results` | [docs/registry/implementation-map.yaml:L9213-L9222] |
| 414 | `SingleFileParallelStrategy._get_vessel_results` | [docs/registry/implementation-map.yaml:L9223-L9232] |
| 415 | `test_structural_owners_and_path_references` | [docs/registry/implementation-map.yaml:L9233-L9242] |
| 416 | `test_same_named_unrelated_folders_are_not_references` | [docs/registry/implementation-map.yaml:L9243-L9252] |
| 417 | `header_repository_findings` | [docs/registry/implementation-map.yaml:L9253-L9262] |
| 418 | `master_with_missing_include` | [docs/registry/implementation-map.yaml:L9263-L9272] |
| 419 | `test_support_types_are_defined_before_vessel_references` | [docs/registry/implementation-map.yaml:L9273-L9282] |
| 420 | `_resolve_case.read` | [docs/registry/implementation-map.yaml:L9283-L9292] |
| 421 | `_pipe_seed_points` | [docs/registry/implementation-map.yaml:L9293-L9302] |
| 422 | `sample_base_template` | [docs/registry/implementation-map.yaml:L9303-L9312] |
| 423 | `sample_variation` | [docs/registry/implementation-map.yaml:L9313-L9322] |
| 424 | `sample_case_template` | [docs/registry/implementation-map.yaml:L9323-L9332] |
| 425 | `TestTemplateGenerator.test_generate_with_basefile_reference` | [docs/registry/implementation-map.yaml:L9333-L9342] |
| 426 | `TestTemplateGenerator.test_reference_paths_resolve_from_arbitrary_output_dir` | [docs/registry/implementation-map.yaml:L9343-L9352] |
| 427 | `TestTemplateGenerator.test_reference_paths_in_cases_dir_stay_relative` | [docs/registry/implementation-map.yaml:L9353-L9362] |
| 428 | `manifest` | [docs/registry/implementation-map.yaml:L9363-L9372] |
| 429 | `temp_config_file` | [docs/registry/implementation-map.yaml:L9373-L9382] |
| 430 | `_solve_input` | [docs/registry/implementation-map.yaml:L9383-L9392] |
| 431 | `_write_skill` | [docs/registry/implementation-map.yaml:L9393-L9402] |
| 432 | `test_workflow_registry` | [docs/registry/implementation-map.yaml:L9403-L9412] |
| 433 | `test_synthetic_rope_tn_curve_matches_dnv_os_e301_table_f3` | [docs/registry/implementation-map.yaml:L9413-L9422] |
| 434 | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | [docs/registry/implementation-map.yaml:L9423-L9432] |
| 435 | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | [docs/registry/implementation-map.yaml:L9433-L9442] |
| 436 | `test_vessel_seakeeping_significant_amplitude_matches_library` | [docs/registry/implementation-map.yaml:L9443-L9452] |
| 437 | `fake` | [docs/registry/implementation-map.yaml:L9453-L9462] |
| 438 | `test_duration_render_preserves_native_default_spelling_and_source` | [docs/registry/implementation-map.yaml:L9463-L9472] |
| 439 | `resume_case` | [docs/registry/implementation-map.yaml:L9473-L9482] |
| 440 | `case` | [docs/registry/implementation-map.yaml:L9483-L9493] |

#### irregular-frequency-treatment

| Record | Definition / source locator | Map record |
| --- | --- | --- |
| 1 | `compute_reference` | [docs/registry/implementation-map.yaml:L13263-L13272] |
| 2 | `solve` | [docs/registry/implementation-map.yaml:L13273-L13282] |
| 3 | `build_case_27` | [docs/registry/implementation-map.yaml:L13283-L13292] |
| 4 | `build_case_32` | [docs/registry/implementation-map.yaml:L13293-L13302] |
| 5 | `_extract_from_spec` | [docs/registry/implementation-map.yaml:L13303-L13312] |
| 6 | `_build_common` | [docs/registry/implementation-map.yaml:L13313-L13322] |
| 7 | `_build_aqwa_meta` | [docs/registry/implementation-map.yaml:L13323-L13332] |
| 8 | `build_orcawave_spec` | [docs/registry/implementation-map.yaml:L13333-L13342] |
| 9 | `GDFHandler.write` | [docs/registry/implementation-map.yaml:L13343-L13352] |
| 10 | `create_floating_body` | [docs/registry/implementation-map.yaml:L13353-L13362] |
| 11 | `AQWABackend._build_options` | [docs/registry/implementation-map.yaml:L13363-L13372] |
| 12 | `AQWABackend.build_deck2` | [docs/registry/implementation-map.yaml:L13373-L13382] |
| 13 | `SolverOptions._migrate_irregular_frequency_fields` | [docs/registry/implementation-map.yaml:L13383-L13392] |
| 14 | `ControlSurfaceSpec.check_control_surface_defined` | [docs/registry/implementation-map.yaml:L13393-L13402] |
| 15 | `BodySpec.resolve_control_surface` | [docs/registry/implementation-map.yaml:L13403-L13412] |
| 16 | `_refuse_if_unreliable` | [docs/registry/implementation-map.yaml:L13413-L13422] |
| 17 | `repair_gdf_text` | [docs/registry/implementation-map.yaml:L13423-L13434] |
| 18 | `_slot_passthrough_extensions` | [docs/registry/implementation-map.yaml:L13435-L13444] |
| 19 | `_iter_mesh_slots` | [docs/registry/implementation-map.yaml:L13445-L13454] |
| 20 | `_check_supported_formats` | [docs/registry/implementation-map.yaml:L13455-L13467] |
| 21 | `_build_body_dict` | [docs/registry/implementation-map.yaml:L13468-L13477] |
| 22 | `_build_general_section` | [docs/registry/implementation-map.yaml:L13478-L13487] |
| 23 | `AQWAInputParser._parse_solver_options` | [docs/registry/implementation-map.yaml:L13488-L13497] |
| 24 | `OrcaWaveInputParser._parse_solver_options` | [docs/registry/implementation-map.yaml:L13498-L13507] |
| 25 | `export_gdf` | [docs/registry/implementation-map.yaml:L13508-L13517] |
| 26 | `export_orcawave` | [docs/registry/implementation-map.yaml:L13518-L13527] |
| 27 | `_subdivide_once` | [docs/registry/implementation-map.yaml:L13528-L13537] |
| 28 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection.ABS_gn_offshore_2018` | [docs/registry/implementation-map.yaml:L13538-L13547] |
| 29 | `diffraction_spec` | [docs/registry/implementation-map.yaml:L13548-L13557] |
| 30 | `_spec` | [docs/registry/implementation-map.yaml:L13558-L13567] |
| 31 | `_spec_with_aux` | [docs/registry/implementation-map.yaml:L13568-L13577] |

#### wave-spectra

| Record | Definition / source locator | Map record |
| --- | --- | --- |
| 1 | `Actuator.output` | [docs/registry/implementation-map.yaml:L13600-L13611] |
| 2 | `Actuator.output` | [docs/registry/implementation-map.yaml:L13612-L13623] |
| 3 | `define_environmental_conditions` | [docs/registry/implementation-map.yaml:L13624-L13635] |
| 4 | `generate_visualizations` | [docs/registry/implementation-map.yaml:L13636-L13647] |
| 5 | `main` | [docs/registry/implementation-map.yaml:L13648-L13662] |
| 6 | `main` | [docs/registry/implementation-map.yaml:L13663-L13674] |
| 7 | `mock_time_series` | [docs/registry/implementation-map.yaml:L13675-L13686] |
| 8 | `<module>` | [docs/registry/implementation-map.yaml:L13687-L13697] |
| 9 | `<module>` | [docs/registry/implementation-map.yaml:L13698-L13708] |
| 10 | `build_dormancy_rules` | [docs/registry/implementation-map.yaml:L13709-L13720] |
| 11 | `create_wave_file` | [docs/registry/implementation-map.yaml:L13721-L13732] |
| 12 | `main` | [docs/registry/implementation-map.yaml:L13733-L13746] |
| 13 | `<module>` | [docs/registry/implementation-map.yaml:L13747-L13757] |
| 14 | `<module>` | [docs/registry/implementation-map.yaml:L13758-L13768] |
| 15 | `<module>` | [docs/registry/implementation-map.yaml:L13769-L13779] |
| 16 | `<module>` | [docs/registry/implementation-map.yaml:L13780-L13790] |
| 17 | `<module>` | [docs/registry/implementation-map.yaml:L13791-L13801] |
| 18 | `<module>` | [docs/registry/implementation-map.yaml:L13802-L13812] |
| 19 | `<module>` | [docs/registry/implementation-map.yaml:L13813-L13823] |
| 20 | `<module>` | [docs/registry/implementation-map.yaml:L13824-L13834] |
| 21 | `<module>` | [docs/registry/implementation-map.yaml:L13835-L13845] |
| 22 | `<module>` | [docs/registry/implementation-map.yaml:L13846-L13856] |
| 23 | `<module>` | [docs/registry/implementation-map.yaml:L13857-L13867] |
| 24 | `<module>` | [docs/registry/implementation-map.yaml:L13868-L13878] |
| 25 | `analyze_excel_file` | [docs/registry/implementation-map.yaml:L13879-L13890] |
| 26 | `PerformanceProfiler.profile_wave_spectrum` | [docs/registry/implementation-map.yaml:L13891-L13902] |
| 27 | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | [docs/registry/implementation-map.yaml:L13903-L13914] |
| 28 | `freq_analyze` | [docs/registry/implementation-map.yaml:L13915-L13926] |
| 29 | `_environment_and_simulation` | [docs/registry/implementation-map.yaml:L13927-L13938] |
| 30 | `short_chord_factor_f1` | [docs/registry/implementation-map.yaml:L13939-L13950] |
| 31 | `spectrum_cmd` | [docs/registry/implementation-map.yaml:L13951-L13964] |
| 32 | `operability_analysis` | [docs/registry/implementation-map.yaml:L13965-L13976] |
| 33 | `WaveSpectra._normalize_to_significant_height` | [docs/registry/implementation-map.yaml:L13977-L13988] |
| 34 | `WaveSpectra.jonswap` | [docs/registry/implementation-map.yaml:L13989-L14004] |
| 35 | `WaveSpectra.pierson_moskowitz` | [docs/registry/implementation-map.yaml:L14005-L14017] |
| 36 | `WaveSpectra.bretschneider` | [docs/registry/implementation-map.yaml:L14018-L14030] |
| 37 | `WaveSpectra.issc` | [docs/registry/implementation-map.yaml:L14031-L14043] |
| 38 | `WaveSpectra.generate_spectrum` | [docs/registry/implementation-map.yaml:L14044-L14060] |
| 39 | `WaveSpectra.significant_height_from_spectrum` | [docs/registry/implementation-map.yaml:L14061-L14072] |
| 40 | `WaveSpectra.zero_crossing_period_from_spectrum` | [docs/registry/implementation-map.yaml:L14073-L14084] |
| 41 | `WaveSpectra.spectrum_statistics` | [docs/registry/implementation-map.yaml:L14085-L14096] |
| 42 | `router` | [docs/registry/implementation-map.yaml:L14097-L14110] |
| 43 | `_spectrum_name` | [docs/registry/implementation-map.yaml:L14111-L14122] |
| 44 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.wave` | [docs/registry/implementation-map.yaml:L14123-L14134] |
| 45 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` / `OrcaflexModelComponents.RayleighDampingCoefficients` | [docs/registry/implementation-map.yaml:L14135-L14146] |
| 46 | `_complex_rao` | [docs/registry/implementation-map.yaml:L14147-L14158] |
| 47 | `crane_tip_raos` | [docs/registry/implementation-map.yaml:L14159-L14172] |
| 48 | `crane_tip_significant_motion` | [docs/registry/implementation-map.yaml:L14173-L14186] |
| 49 | `crane_tip_significant_velocity` | [docs/registry/implementation-map.yaml:L14187-L14199] |
| 50 | `_jonswap_spectrum` | [docs/registry/implementation-map.yaml:L14200-L14213] |
| 51 | `analyze_excel_file` | [docs/registry/implementation-map.yaml:L14214-L14225] |
| 52 | `PerformanceProfiler.profile_wave_spectrum` | [docs/registry/implementation-map.yaml:L14226-L14237] |
| 53 | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | [docs/registry/implementation-map.yaml:L14238-L14249] |
| 54 | `FPSOMooringFullWorkflow._wave_spectrum` | [docs/registry/implementation-map.yaml:L14218-L14231] |
| 55 | `operation_envelope` | [docs/registry/implementation-map.yaml:L14262-L14275] |
| 56 | `_significant_response` | [docs/registry/implementation-map.yaml:L14276-L14287] |
| 57 | `synthesize_directional_forecast` | [docs/registry/implementation-map.yaml:L14288-L14301] |
| 58 | `jonswap_spectrum` | [docs/registry/implementation-map.yaml:L14302-L14315] |
| 59 | `synthesize_forecast` | [docs/registry/implementation-map.yaml:L14316-L14328] |
| 60 | `WaveSpectrumParams.tz` | [docs/registry/implementation-map.yaml:L14329-L14342] |
| 61 | `WaveSpectrumParams.spectral_density` | [docs/registry/implementation-map.yaml:L14343-L14356] |
| 62 | `WaveSpectrumParams.to_orcaflex_dict` | [docs/registry/implementation-map.yaml:L14357-L14368] |
| 63 | `compute_response_spectrum` | [docs/registry/implementation-map.yaml:L14369-L14380] |
| 64 | `pierson_moskowitz` | [docs/registry/implementation-map.yaml:L14381-L14392] |
| 65 | `jonswap` | [docs/registry/implementation-map.yaml:L14393-L14408] |
| 66 | `bretschneider` | [docs/registry/implementation-map.yaml:L14409-L14421] |
| 67 | `issc_spectrum` | [docs/registry/implementation-map.yaml:L14422-L14433] |
| 68 | `ochi_hubble._component` | [docs/registry/implementation-map.yaml:L14434-L14445] |
| 69 | `torsethaugen` | [docs/registry/implementation-map.yaml:L14446-L14459] |
| 70 | `generate_spectrum` | [docs/registry/implementation-map.yaml:L14460-L14477] |
| 71 | `_spectral_fatigue_annual_damage` | [docs/registry/implementation-map.yaml:L14478-L14489] |
| 72 | `_esp_pump_utilisation` | [docs/registry/implementation-map.yaml:L14490-L14501] |
| 73 | `_stress_psd_from_rao` | [docs/registry/implementation-map.yaml:L14502-L14513] |
| 74 | `run_orcaflex` | [docs/registry/implementation-map.yaml:L14514-L14525] |
| 75 | `Environment.get_wave_waveTrains` | [docs/registry/implementation-map.yaml:L14526-L14539] |
| 76 | `Environment.getJONSWAPGamma` | [docs/registry/implementation-map.yaml:L14540-L14551] |
| 77 | `Environment.RayleighDampingCoefficients` | [docs/registry/implementation-map.yaml:L14552-L14563] |
| 78 | `EnvironmentBuilder._build_wave_trains` | [docs/registry/implementation-map.yaml:L14564-L14577] |
| 79 | `EnvironmentBuilder._order_wave_train` | [docs/registry/implementation-map.yaml:L14578-L14591] |
| 80 | `CampaignSpec.from_legacy_config` | [docs/registry/implementation-map.yaml:L14592-L14605] |
| 81 | `OrcaflexModelComponents.wave` | [docs/registry/implementation-map.yaml:L14606-L14617] |
| 82 | `OrcaflexModelComponents.RayleighDampingCoefficients` | [docs/registry/implementation-map.yaml:L14618-L14629] |
| 83 | `_evaluate_location` | [docs/registry/implementation-map.yaml:L14630-L14641] |
| 84 | `FatigueAnalysisEngine.analyze_psd` | [docs/registry/implementation-map.yaml:L14642-L14655] |
| 85 | `FatigueAnalysisEngine._perform_frequency_validation` | [docs/registry/implementation-map.yaml:L14656-L14667] |
| 86 | `NarrowBandMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L14668-L14679] |
| 87 | `DirlikMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L14680-L14691] |
| 88 | `TovoBenasciuttiMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L14692-L14703] |
| 89 | `SingleMomentMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L14704-L14715] |
| 90 | `_resolve_wave_spectrum_func` | [docs/registry/implementation-map.yaml:L14716-L14727] |
| 91 | `_resolve_wave_spectrum_func._jonswap_wrapper` | [docs/registry/implementation-map.yaml:L14728-L14739] |
| 92 | `orcaflexModel` | [docs/registry/implementation-map.yaml:L14740-L14751] |
| 93 | `wave` | [docs/registry/implementation-map.yaml:L14752-L14763] |
| 94 | `jonswap_gamma` | [docs/registry/implementation-map.yaml:L14764-L14775] |
| 95 | `jonswap_spectrum` | [docs/registry/implementation-map.yaml:L14776-L14791] |
| 96 | `compute_wave_velocity` | [docs/registry/implementation-map.yaml:L14792-L14806] |
| 97 | `_spectrum_type` | [docs/registry/implementation-map.yaml:L14807-L14818] |
| 98 | `_design` | [docs/registry/implementation-map.yaml:L14819-L14830] |
| 99 | `design_data` | [docs/registry/implementation-map.yaml:L14831-L14842] |
| 100 | `_configure` | [docs/registry/implementation-map.yaml:L14843-L14854] |
| 101 | `_verify` | [docs/registry/implementation-map.yaml:L14855-L14866] |
| 102 | `_change_payload` | [docs/registry/implementation-map.yaml:L14867-L14878] |
| 103 | `VesselResponseWorkflow.create` | [docs/registry/implementation-map.yaml:L14879-L14892] |
| 104 | `wave_spectrum` | [docs/registry/implementation-map.yaml:L14893-L14906] |
| 105 | `jonswap_spectrum` | [docs/registry/implementation-map.yaml:L14907-L14920] |
| 106 | `main` | [docs/registry/implementation-map.yaml:L14921-L14932] |
| 107 | `TestWaveDynamicsIntegration.test_jonswap_generates_valid_spectrum` | [docs/registry/implementation-map.yaml:L14933-L14944] |
| 108 | `TestWaveDynamicsIntegration.test_zero_crossing_period_relationship` | [docs/registry/implementation-map.yaml:L14945-L14956] |
| 109 | `TestWaveDynamicsIntegration.test_wave_elevation_time_series_generation` | [docs/registry/implementation-map.yaml:L14957-L14968] |
| 110 | `TestWaveDynamicsIntegration.test_wave_energy_distribution` | [docs/registry/implementation-map.yaml:L14969-L14980] |
| 111 | `test_jonswap_hs_recovery` | [docs/registry/implementation-map.yaml:L14981-L14992] |
| 112 | `test_pierson_moskowitz_hs_recovery` | [docs/registry/implementation-map.yaml:L14993-L15004] |
| 113 | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | [docs/registry/implementation-map.yaml:L15005-L15016] |
| 114 | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | [docs/registry/implementation-map.yaml:L15017-L15028] |
| 115 | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | [docs/registry/implementation-map.yaml:L15029-L15040] |
| 116 | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | [docs/registry/implementation-map.yaml:L15041-L15052] |
| 117 | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | [docs/registry/implementation-map.yaml:L15053-L15064] |
| 118 | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | [docs/registry/implementation-map.yaml:L15065-L15076] |
| 119 | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | [docs/registry/implementation-map.yaml:L15077-L15088] |
| 120 | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | [docs/registry/implementation-map.yaml:L15089-L15100] |
| 121 | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | [docs/registry/implementation-map.yaml:L15101-L15112] |
| 122 | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | [docs/registry/implementation-map.yaml:L15113-L15124] |
| 123 | `wave_order_findings` | [docs/registry/implementation-map.yaml:L15125-L15136] |
| 124 | `_write_floating_campaign` | [docs/registry/implementation-map.yaml:L15137-L15148] |
| 125 | `_make_campaign_data` | [docs/registry/implementation-map.yaml:L15149-L15160] |
| 126 | `TestSpectralMomentsCalculation.test_white_noise_m0_equals_level_times_bandwidth` | [docs/registry/implementation-map.yaml:L15161-L15172] |
| 127 | `_jonswap_hz` | [docs/registry/implementation-map.yaml:L15173-L15184] |
| 128 | `simple_wave_params` | [docs/registry/implementation-map.yaml:L15185-L15198] |
| 129 | `TestWaveSpectra.test_jonswap_peak_near_tp` | [docs/registry/implementation-map.yaml:L15199-L15210] |
| 130 | `TestPiersonMoskowitzExtended.test_pm_peak_near_tp` | [docs/registry/implementation-map.yaml:L15211-L15222] |
| 131 | `TestBretschneiderExtended.test_bretschneider_peak_near_tp` | [docs/registry/implementation-map.yaml:L15223-L15234] |
| 132 | `TestZeroCrossingPeriodFromSpectrum.test_tz_pm_approximately_0711_tp` | [docs/registry/implementation-map.yaml:L15235-L15246] |
| 133 | `TestPeakFrequencyFromSpectrum.test_peak_frequency_near_2pi_over_tp` | [docs/registry/implementation-map.yaml:L15247-L15258] |
| 134 | `_assert_wave_spectra` | [docs/registry/implementation-map.yaml:L15259-L15270] |
| 135 | `test_workflow_registry` | [docs/registry/implementation-map.yaml:L15271-L15284] |
| 136 | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | [docs/registry/implementation-map.yaml:L15285-L15298] |
| 137 | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | [docs/registry/implementation-map.yaml:L15299-L15314] |
| 138 | `test_vessel_seakeeping_significant_amplitude_matches_library` | [docs/registry/implementation-map.yaml:L15315-L15328] |
| 139 | `test_esp_pump_hydraulics_tdh_and_sizing_closed_form` | [docs/registry/implementation-map.yaml:L15329-L15342] |
| 140 | `fatg_significant_stress_range` | [docs/registry/implementation-map.yaml:L15343-L15361] |
| 141 | `fatg_significant_stress_range.ordinate` | [docs/registry/implementation-map.yaml:L15362-L15377] |

#### wind-and-current-force-routes

| Record | Definition / source locator | Map record |
| --- | --- | --- |
| 1 | `calc_splash_zone` | [docs/registry/implementation-map.yaml:L15400-L15409] |
| 2 | `calc_phase_3_splash` | [docs/registry/implementation-map.yaml:L15410-L15419] |
| 3 | `<module>` | [docs/registry/implementation-map.yaml:L15420-L15428] |
| 4 | `_good_case` | [docs/registry/implementation-map.yaml:L15429-L15438] |
| 5 | `calculate_environmental_forces` | [docs/registry/implementation-map.yaml:L15439-L15448] |
| 6 | `main` | [docs/registry/implementation-map.yaml:L15449-L15460] |
| 7 | `main` | [docs/registry/implementation-map.yaml:L15461-L15470] |
| 8 | `calculate_environmental_forces` | [docs/registry/implementation-map.yaml:L15471-L15480] |
| 9 | `analyze_calm_buoy` | [docs/registry/implementation-map.yaml:L15481-L15490] |
| 10 | `analyze_jackup_mooring` | [docs/registry/implementation-map.yaml:L15491-L15500] |
| 11 | `analyze_floating_wind_mooring` | [docs/registry/implementation-map.yaml:L15501-L15510] |
| 12 | `analyze_wec_mooring` | [docs/registry/implementation-map.yaml:L15511-L15520] |
| 13 | `analyze_aquaculture_mooring` | [docs/registry/implementation-map.yaml:L15521-L15530] |
| 14 | `analyze_spm_terminal` | [docs/registry/implementation-map.yaml:L15531-L15540] |
| 15 | `analyze_dp_backup_mooring` | [docs/registry/implementation-map.yaml:L15541-L15550] |
| 16 | `<module>` | [docs/registry/implementation-map.yaml:L15551-L15559] |
| 17 | `<module>` | [docs/registry/implementation-map.yaml:L15560-L15568] |
| 18 | `extract_ocimf_database` | [docs/registry/implementation-map.yaml:L15569-L15578] |
| 19 | `<module>` | [docs/registry/implementation-map.yaml:L15579-L15587] |
| 20 | `main` | [docs/registry/implementation-map.yaml:L15588-L15597] |
| 21 | `PerformanceProfiler._setup_test_data` | [docs/registry/implementation-map.yaml:L15598-L15607] |
| 22 | `PerformanceProfiler.profile_ocimf_lookup` | [docs/registry/implementation-map.yaml:L15608-L15617] |
| 23 | `PerformanceProfiler.run_all` | [docs/registry/implementation-map.yaml:L15618-L15629] |
| 24 | `main` | [docs/registry/implementation-map.yaml:L15630-L15639] |
| 25 | `Phase2Validator.generate_excel_reference_data` | [docs/registry/implementation-map.yaml:L15640-L15649] |
| 26 | `Phase2Validator.export_statistics_to_csv` | [docs/registry/implementation-map.yaml:L15650-L15659] |
| 27 | `Phase2Validator.export_json_summary` | [docs/registry/implementation-map.yaml:L15660-L15669] |
| 28 | `Phase2Validator.run_complete_validation` | [docs/registry/implementation-map.yaml:L15670-L15684] |
| 29 | `<module>` | [docs/registry/implementation-map.yaml:L15685-L15693] |
| 30 | `main` | [docs/registry/implementation-map.yaml:L15694-L15703] |
| 31 | `make_polar_overlay` | [docs/registry/implementation-map.yaml:L15704-L15713] |
| 32 | `parse_args` | [docs/registry/implementation-map.yaml:L15714-L15723] |
| 33 | `PressureVesselGenerator.generate_wind_load` | [docs/registry/implementation-map.yaml:L15724-L15733] |
| 34 | `SyntheticRopeDatabaseClient._generate_rope_spec` | [docs/registry/implementation-map.yaml:L15734-L15743] |
| 35 | `main` | [docs/registry/implementation-map.yaml:L15744-L15753] |
| 36 | `environmental_force_n` | [docs/registry/implementation-map.yaml:L15754-L15763] |
| 37 | `CurrentProfile.drag_load_n_per_m` | [docs/registry/implementation-map.yaml:L15764-L15773] |
| 38 | `as_analysed_sections` | [docs/registry/implementation-map.yaml:L15774-L15783] |
| 39 | `router` | [docs/registry/implementation-map.yaml:L15784-L15793] |
| 40 | `engine` | [docs/registry/implementation-map.yaml:L15794-L15806] |
| 41 | `ocimf_wind_cmd` | [docs/registry/implementation-map.yaml:L15807-L15816] |
| 42 | `ocimf_current_cmd` | [docs/registry/implementation-map.yaml:L15817-L15826] |
| 43 | `combined_env_cmd` | [docs/registry/implementation-map.yaml:L15827-L15836] |
| 44 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.get_total_viv_fatigue` | [docs/registry/implementation-map.yaml:L15837-L15846] |
| 45 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L15847-L15856] |
| 46 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.get_total_viv_fatigue` | [docs/registry/implementation-map.yaml:L15857-L15866] |
| 47 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L15867-L15876] |
| 48 | `ETL_components.transform_viv_current_reformat_1` | [docs/registry/implementation-map.yaml:L15877-L15886] |
| 49 | `compute_splashzone_envelope` | [docs/registry/implementation-map.yaml:L15887-L15896] |
| 50 | `calc_phase_3_splash` | [docs/registry/implementation-map.yaml:L15897-L15906] |
| 51 | `main` | [docs/registry/implementation-map.yaml:L15907-L15916] |
| 52 | `OCIMFDatabase.get_coefficients` | [docs/registry/implementation-map.yaml:L15917-L15926] |
| 53 | `EnvironmentalForces.calculate_total_forces` | [docs/registry/implementation-map.yaml:L15927-L15936] |
| 54 | `create_sample_database` | [docs/registry/implementation-map.yaml:L15937-L15946] |
| 55 | `OCIMFWorkflow.router` | [docs/registry/implementation-map.yaml:L15947-L15956] |
| 56 | `<module>` | [docs/registry/implementation-map.yaml:L15957-L15965] |
| 57 | `extract_ocimf_database` | [docs/registry/implementation-map.yaml:L15966-L15975] |
| 58 | `PerformanceProfiler._setup_test_data` | [docs/registry/implementation-map.yaml:L15976-L15987] |
| 59 | `PerformanceProfiler.profile_ocimf_lookup` | [docs/registry/implementation-map.yaml:L15988-L15997] |
| 60 | `PerformanceProfiler.run_all` | [docs/registry/implementation-map.yaml:L15998-L16009] |
| 61 | `main` | [docs/registry/implementation-map.yaml:L16010-L16019] |
| 62 | `main` | [docs/registry/implementation-map.yaml:L16020-L16029] |
| 63 | `Phase2Validator.generate_excel_reference_data` | [docs/registry/implementation-map.yaml:L16030-L16039] |
| 64 | `Phase2Validator.export_statistics_to_csv` | [docs/registry/implementation-map.yaml:L16040-L16049] |
| 65 | `Phase2Validator.export_json_summary` | [docs/registry/implementation-map.yaml:L16050-L16059] |
| 66 | `Phase2Validator.run_complete_validation` | [docs/registry/implementation-map.yaml:L16060-L16074] |
| 67 | `<module>` | [docs/registry/implementation-map.yaml:L16075-L16083] |
| 68 | `_output_dir` | [docs/registry/implementation-map.yaml:L16084-L16093] |
| 69 | `_cli_output_dir` | [docs/registry/implementation-map.yaml:L16094-L16103] |
| 70 | `load_data` | [docs/registry/implementation-map.yaml:L16104-L16113] |
| 71 | `main` | [docs/registry/implementation-map.yaml:L16114-L16126] |
| 72 | `OCIMFDatabase.get_coefficients` | [docs/registry/implementation-map.yaml:L16127-L16136] |
| 73 | `EnvironmentalForces.calculate_total_forces` | [docs/registry/implementation-map.yaml:L16137-L16146] |
| 74 | `create_sample_database` | [docs/registry/implementation-map.yaml:L16147-L16156] |
| 75 | `FPSOMooringFullWorkflow._environmental_forces` | [docs/registry/implementation-map.yaml:L16127-L16139] |
| 76 | `FPSOMooringFullWorkflow._ocimf_database` | [docs/registry/implementation-map.yaml:L16140-L16149] |
| 77 | `_resolve_arrow_direction_in_body_frame` | [docs/registry/implementation-map.yaml:L16179-L16188] |
| 78 | `polar_force_overlay` | [docs/registry/implementation-map.yaml:L16189-L16198] |
| 79 | `threshold_speed_for_steerage` | [docs/registry/implementation-map.yaml:L16199-L16208] |
| 80 | `run_maneuvering_envelope_sweep` | [docs/registry/implementation-map.yaml:L16209-L16218] |
| 81 | `SplashZoneInput.calculate_splash_zone_loads` | [docs/registry/implementation-map.yaml:L16219-L16228] |
| 82 | `compute_wind_current_drift` | [docs/registry/implementation-map.yaml:L16229-L16238] |
| 83 | `KnowledgeIntegrator._extract_configurations` | [docs/registry/implementation-map.yaml:L16239-L16248] |
| 84 | `EnvironmentBuilder.build` | [docs/registry/implementation-map.yaml:L16249-L16258] |
| 85 | `extract_loads` | [docs/registry/implementation-map.yaml:L16259-L16268] |
| 86 | `calculate_dual_chute_drag` | [docs/registry/implementation-map.yaml:L16269-L16280] |
| 87 | `assess_all_load_cases` | [docs/registry/implementation-map.yaml:L16281-L16293] |
| 88 | `simulate_deployment_transient` | [docs/registry/implementation-map.yaml:L16294-L16304] |
| 89 | `export_results_yaml` | [docs/registry/implementation-map.yaml:L16305-L16314] |
| 90 | `calculate_drag_force` | [docs/registry/implementation-map.yaml:L16315-L16324] |
| 91 | `design_verification` | [docs/registry/implementation-map.yaml:L16325-L16334] |
| 92 | `MooringDesigner.calculate_environmental_loads` | [docs/registry/implementation-map.yaml:L16335-L16344] |
| 93 | `build_wind_model.force` | [docs/registry/implementation-map.yaml:L16345-L16354] |
| 94 | `_phase_demand` | [docs/registry/implementation-map.yaml:L16355-L16364] |
| 95 | `hydrodynamic_loads` | [docs/registry/implementation-map.yaml:L16365-L16374] |
| 96 | `drag_force_per_meter` | [docs/registry/implementation-map.yaml:L16375-L16384] |
| 97 | `lateral_stability_check` | [docs/registry/implementation-map.yaml:L16385-L16396] |
| 98 | `check_lateral_stability` | [docs/registry/implementation-map.yaml:L16397-L16408] |
| 99 | `run_on_bottom_stability` | [docs/registry/implementation-map.yaml:L16409-L16418] |
| 100 | `VIVScreening.screen_with_current_profile` | [docs/registry/implementation-map.yaml:L16419-L16428] |
| 101 | `viv_analysis` | [docs/registry/implementation-map.yaml:L16429-L16438] |
| 102 | `VortexSheddingAnalyzer.analyze_with_profile` | [docs/registry/implementation-map.yaml:L16439-L16448] |
| 103 | `create_mooring_design_template` | [docs/registry/implementation-map.yaml:L16449-L16458] |
| 104 | `MooringSystemDesignWorkflow.create` | [docs/registry/implementation-map.yaml:L16459-L16468] |
| 105 | `<module>` | [docs/registry/implementation-map.yaml:L16469-L16477] |
| 106 | `_load_producer` | [docs/registry/implementation-map.yaml:L16478-L16487] |
| 107 | `sample_database_path` | [docs/registry/implementation-map.yaml:L16488-L16497] |
| 108 | `ocimf_db` | [docs/registry/implementation-map.yaml:L16498-L16507] |
| 109 | `env_forces` | [docs/registry/implementation-map.yaml:L16508-L16517] |
| 110 | `main` | [docs/registry/implementation-map.yaml:L16518-L16528] |
| 111 | `TestOCIMFMooringIntegration.test_combined_environmental_forces` | [docs/registry/implementation-map.yaml:L16529-L16538] |
| 112 | `ocimf_database` | [docs/registry/implementation-map.yaml:L16539-L16549] |
| 113 | `pytest_benchmark_update_json` | [docs/registry/implementation-map.yaml:L16550-L16559] |
| 114 | `test_refactored_polar_signatures_match_fixture` | [docs/registry/implementation-map.yaml:L16560-L16569] |
| 115 | `_build_loads` | [docs/registry/implementation-map.yaml:L16570-L16579] |
| 116 | `TestAssessAllLoadCases.test_governing_force_within_1_percent_of_hand_calc` | [docs/registry/implementation-map.yaml:L16580-L16589] |
| 117 | `TestDragForce.test_drag_200_mph` | [docs/registry/implementation-map.yaml:L16590-L16600] |
| 118 | `TestDragForce.test_drag_250_mph` | [docs/registry/implementation-map.yaml:L16601-L16611] |
| 119 | `TestDragForce.test_force_scales_with_v_squared` | [docs/registry/implementation-map.yaml:L16612-L16622] |
| 120 | `TestDragForce.test_result_has_newtons` | [docs/registry/implementation-map.yaml:L16623-L16633] |
| 121 | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_newtons` | [docs/registry/implementation-map.yaml:L16634-L16644] |
| 122 | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_lbs` | [docs/registry/implementation-map.yaml:L16645-L16655] |
| 123 | `catenary_environment` | [docs/registry/implementation-map.yaml:L16656-L16665] |
| 124 | `TestLiftForce.test_known_lift_force` | [docs/registry/implementation-map.yaml:L16666-L16675] |
| 125 | `TestDragForce.test_scenario_1` | [docs/registry/implementation-map.yaml:L16676-L16685] |
| 126 | `TestLiftForce.test_scenario_1` | [docs/registry/implementation-map.yaml:L16686-L16695] |
| 127 | `get_dnv_circular_mock` | [docs/registry/implementation-map.yaml:L16696-L16705] |
| 128 | `get_dnv_rectangular_mock` | [docs/registry/implementation-map.yaml:L16706-L16715] |
| 129 | `TestHydrodynamicLoads.test_drag_force_per_meter` | [docs/registry/implementation-map.yaml:L16716-L16725] |
| 130 | `TestHydrodynamicLoads.test_lift_force_per_meter` | [docs/registry/implementation-map.yaml:L16726-L16735] |
| 131 | `test_workflow_registry` | [docs/registry/implementation-map.yaml:L16736-L16747] |
| 132 | `hydrodynamic_force_per_meter` | [docs/registry/implementation-map.yaml:L16748-L16758] |
| 133 | `lift_force_per_meter` | [docs/registry/implementation-map.yaml:L16759-L16769] |
| 134 | `lift_force_per_meter` | [docs/registry/implementation-map.yaml:L16770-L16781] |
| 135 | `inertia_force_per_meter` | [docs/registry/implementation-map.yaml:L16782-L16791] |

#### sn-curves-and-miner-damage

| Record | Definition / source locator | Map record |
| --- | --- | --- |
| 1 | `example_6_custom_workflow` | [docs/registry/implementation-map.yaml:L16814-L16823] |
| 2 | `example_1_basic_rainflow_counting` | [docs/registry/implementation-map.yaml:L16824-L16833] |
| 3 | `example_4_fatigue_analysis` | [docs/registry/implementation-map.yaml:L16834-L16843] |
| 4 | `example_6_backward_compatibility` | [docs/registry/implementation-map.yaml:L16844-L16853] |
| 5 | `main` | [docs/registry/implementation-map.yaml:L16854-L16867] |
| 6 | `main` | [docs/registry/implementation-map.yaml:L16868-L16879] |
| 7 | `demonstrate_sn_curves` | [docs/registry/implementation-map.yaml:L16880-L16889] |
| 8 | `demonstrate_rainflow_counting` | [docs/registry/implementation-map.yaml:L16890-L16899] |
| 9 | `demonstrate_damage_accumulation` | [docs/registry/implementation-map.yaml:L16900-L16909] |
| 10 | `demonstrate_frequency_domain_analysis` | [docs/registry/implementation-map.yaml:L16910-L16919] |
| 11 | `demonstrate_quick_analysis` | [docs/registry/implementation-map.yaml:L16920-L16929] |
| 12 | `main` | [docs/registry/implementation-map.yaml:L16930-L16945] |
| 13 | `list_curves` | [docs/registry/implementation-map.yaml:L16946-L16955] |
| 14 | `compare_curves` | [docs/registry/implementation-map.yaml:L16956-L16965] |
| 15 | `example_2_linear_log` | [docs/registry/implementation-map.yaml:L16966-L16975] |
| 16 | `example_3_with_scf` | [docs/registry/implementation-map.yaml:L16976-L16985] |
| 17 | `example_4_without_fatigue_limit` | [docs/registry/implementation-map.yaml:L16986-L16995] |
| 18 | `example_5_dnv_comparison` | [docs/registry/implementation-map.yaml:L16996-L17005] |
| 19 | `example_6_abs_comparison` | [docs/registry/implementation-map.yaml:L17006-L17015] |
| 20 | `example_7_reference_comparison` | [docs/registry/implementation-map.yaml:L17016-L17025] |
| 21 | `example_8_joint_type_comparison` | [docs/registry/implementation-map.yaml:L17026-L17035] |
| 22 | `example_1_basic_sn_curves` | [docs/registry/implementation-map.yaml:L17036-L17045] |
| 23 | `example_2_multislope_curves` | [docs/registry/implementation-map.yaml:L17046-L17055] |
| 24 | `example_3_time_domain_analysis` | [docs/registry/implementation-map.yaml:L17056-L17065] |
| 25 | `example_4_frequency_domain_analysis` | [docs/registry/implementation-map.yaml:L17066-L17075] |
| 26 | `example_5_high_stress_range_analysis` | [docs/registry/implementation-map.yaml:L17076-L17085] |
| 27 | `example_6_shear_data_analysis` | [docs/registry/implementation-map.yaml:L17086-L17095] |
| 28 | `run_all_examples` | [docs/registry/implementation-map.yaml:L17096-L17112] |
| 29 | `example_well_log_analysis` | [docs/registry/implementation-map.yaml:L17113-L17122] |
| 30 | `example_stratigraphic_correlation` | [docs/registry/implementation-map.yaml:L17123-L17132] |
| 31 | `main` | [docs/registry/implementation-map.yaml:L17133-L17145] |
| 32 | `<module>` | [docs/registry/implementation-map.yaml:L17146-L17154] |
| 33 | `<module>` | [docs/registry/implementation-map.yaml:L17155-L17163] |
| 34 | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17164-L17173] |
| 35 | `YAMLConfigMigrator._migrate_analysis` | [docs/registry/implementation-map.yaml:L17174-L17183] |
| 36 | `YAMLConfigMigrator._migrate_time_series_components` | [docs/registry/implementation-map.yaml:L17184-L17193] |
| 37 | `process_fatigue_files` | [docs/registry/implementation-map.yaml:L17194-L17203] |
| 38 | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17204-L17213] |
| 39 | `process_fatigue_file` | [docs/registry/implementation-map.yaml:L17214-L17223] |
| 40 | `process_sample_file` | [docs/registry/implementation-map.yaml:L17224-L17235] |
| 41 | `batch_process_all_files` | [docs/registry/implementation-map.yaml:L17236-L17246] |
| 42 | `rainflow_counting` | [docs/registry/implementation-map.yaml:L17247-L17256] |
| 43 | `process_fatigue_file` | [docs/registry/implementation-map.yaml:L17257-L17268] |
| 44 | `batch_process` | [docs/registry/implementation-map.yaml:L17269-L17280] |
| 45 | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17281-L17290] |
| 46 | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17291-L17300] |
| 47 | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17301-L17310] |
| 48 | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17311-L17320] |
| 49 | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17321-L17330] |
| 50 | `<module>` | [docs/registry/implementation-map.yaml:L17331-L17339] |
| 51 | `SNcurve.from_dnv` | [docs/registry/implementation-map.yaml:L17340-L17349] |
| 52 | `FatiguePostprocessor.calculate_cycles_to_failure` | [docs/registry/implementation-map.yaml:L17350-L17359] |
| 53 | `FatiguePostprocessor.calculate_fatigue_damage` | [docs/registry/implementation-map.yaml:L17360-L17371] |
| 54 | `FatiguePostprocessor.generate_sn_curve_apdl` | [docs/registry/implementation-map.yaml:L17372-L17381] |
| 55 | `FatiguePostprocessor.create_damage_summary_df` | [docs/registry/implementation-map.yaml:L17382-L17391] |
| 56 | `build_model_inventory` | [docs/registry/implementation-map.yaml:L17392-L17401] |
| 57 | `half_cycles` | [docs/registry/implementation-map.yaml:L17402-L17411] |
| 58 | `extract` | [docs/registry/implementation-map.yaml:L17412-L17423] |
| 59 | `engine` | [docs/registry/implementation-map.yaml:L17424-L17441] |
| 60 | `router` | [docs/registry/implementation-map.yaml:L17442-L17454] |
| 61 | `_sn_curve` | [docs/registry/implementation-map.yaml:L17455-L17464] |
| 62 | `_case_histogram` | [docs/registry/implementation-map.yaml:L17465-L17474] |
| 63 | `assess_fatigue` | [docs/registry/implementation-map.yaml:L17475-L17488] |
| 64 | `<module>` | [docs/registry/implementation-map.yaml:L17489-L17497] |
| 65 | `_adapt_pylife_4pt` | [docs/registry/implementation-map.yaml:L17498-L17507] |
| 66 | `_adapt_pypi_rainflow` | [docs/registry/implementation-map.yaml:L17508-L17517] |
| 67 | `_adapt_sigproc_astm` | [docs/registry/implementation-map.yaml:L17518-L17527] |
| 68 | `_adapt_calm_buoy` | [docs/registry/implementation-map.yaml:L17528-L17537] |
| 69 | `_adapt_fapps_counting` | [docs/registry/implementation-map.yaml:L17538-L17547] |
| 70 | `_adapt_fapps_counter` | [docs/registry/implementation-map.yaml:L17548-L17557] |
| 71 | `_adapt_struct_fatigue` | [docs/registry/implementation-map.yaml:L17558-L17567] |
| 72 | `_is_counting_name` | [docs/registry/implementation-map.yaml:L17568-L17577] |
| 73 | `miner_damage` | [docs/registry/implementation-map.yaml:L17578-L17587] |
| 74 | `design_life_check` | [docs/registry/implementation-map.yaml:L17588-L17599] |
| 75 | `environment_correction` | [docs/registry/implementation-map.yaml:L17600-L17609] |
| 76 | `apply_environment_to_sn` | [docs/registry/implementation-map.yaml:L17610-L17621] |
| 77 | `fatg_sea_state_damage` | [docs/registry/implementation-map.yaml:L17622-L17631] |
| 78 | `fatg_annual_damage` | [docs/registry/implementation-map.yaml:L17632-L17643] |
| 79 | `_sec_fatigue_locations` | [docs/registry/implementation-map.yaml:L17644-L17653] |
| 80 | `fatigue_life` | [docs/registry/implementation-map.yaml:L17654-L17666] |
| 81 | `_compute_sd` | [docs/registry/implementation-map.yaml:L17667-L17676] |
| 82 | `_build_curve_dict` | [docs/registry/implementation-map.yaml:L17677-L17688] |
| 83 | `get_sn_curve` | [docs/registry/implementation-map.yaml:L17689-L17699] |
| 84 | `SNCurveRecord.knee_stress` | [docs/registry/implementation-map.yaml:L17700-L17711] |
| 85 | `SNCurveRecord.switch_stress` | [docs/registry/implementation-map.yaml:L17712-L17723] |
| 86 | `SNCurveRecord.cycles` | [docs/registry/implementation-map.yaml:L17724-L17735] |
| 87 | `_knee_stress` | [docs/registry/implementation-map.yaml:L17736-L17745] |
| 88 | `_switch_stress` | [docs/registry/implementation-map.yaml:L17746-L17756] |
| 89 | `_bilinear_cycles` | [docs/registry/implementation-map.yaml:L17757-L17766] |
| 90 | `_sd` | [docs/registry/implementation-map.yaml:L17767-L17776] |
| 91 | `_build_dnv_curves` | [docs/registry/implementation-map.yaml:L17777-L17786] |
| 92 | `_build_api_curves` | [docs/registry/implementation-map.yaml:L17787-L17796] |
| 93 | `_build_bs7608_curves` | [docs/registry/implementation-map.yaml:L17797-L17808] |
| 94 | `_build_iiw_curves` | [docs/registry/implementation-map.yaml:L17809-L17819] |
| 95 | `_build_eurocode3_curves` | [docs/registry/implementation-map.yaml:L17820-L17830] |
| 96 | `_build_norsok_curves` | [docs/registry/implementation-map.yaml:L17831-L17841] |
| 97 | `_build_aws_curves` | [docs/registry/implementation-map.yaml:L17842-L17851] |
| 98 | `_build_asme_curves` | [docs/registry/implementation-map.yaml:L17852-L17861] |
| 99 | `_build_en13445_curves` | [docs/registry/implementation-map.yaml:L17862-L17872] |
| 100 | `_build_pd5500_curves` | [docs/registry/implementation-map.yaml:L17873-L17883] |
| 101 | `_build_abs_curves` | [docs/registry/implementation-map.yaml:L17884-L17893] |
| 102 | `_build_iso19902_curves` | [docs/registry/implementation-map.yaml:L17894-L17904] |
| 103 | `_build_csa_curves` | [docs/registry/implementation-map.yaml:L17905-L17914] |
| 104 | `_build_hse_curves` | [docs/registry/implementation-map.yaml:L17915-L17925] |
| 105 | `_build_doe_curves` | [docs/registry/implementation-map.yaml:L17926-L17936] |
| 106 | `_build_gl_curves` | [docs/registry/implementation-map.yaml:L17937-L17946] |
| 107 | `_build_jis_curves` | [docs/registry/implementation-map.yaml:L17947-L17956] |
| 108 | `_ensure_loaded` | [docs/registry/implementation-map.yaml:L17957-L17966] |
| 109 | `summary_table` | [docs/registry/implementation-map.yaml:L17967-L17978] |
| 110 | `_record_to_curve_info` | [docs/registry/implementation-map.yaml:L17979-L17988] |
| 111 | `_record_to_sncurve` | [docs/registry/implementation-map.yaml:L17989-L17998] |
| 112 | `get_curve` | [docs/registry/implementation-map.yaml:L17999-L18010] |
| 113 | `calculate_endurance` | [docs/registry/implementation-map.yaml:L18011-L18023] |
| 114 | `wirsching_light_damage` | [docs/registry/implementation-map.yaml:L18024-L18033] |
| 115 | `benasciutti_tovo_damage` | [docs/registry/implementation-map.yaml:L18034-L18043] |
| 116 | `weld_fatigue_quickcheck` | [docs/registry/implementation-map.yaml:L18044-L18054] |
| 117 | `main` | [docs/registry/implementation-map.yaml:L18055-L18064] |
| 118 | `generate_test_data` | [docs/registry/implementation-map.yaml:L18065-L18074] |
| 119 | `_fallback_log_regression` | [docs/registry/implementation-map.yaml:L18075-L18084] |
| 120 | `router` | [docs/registry/implementation-map.yaml:L18085-L18102] |
| 121 | `_curve_ids` | [docs/registry/implementation-map.yaml:L18103-L18112] |
| 122 | `_stress_ranges` | [docs/registry/implementation-map.yaml:L18113-L18122] |
| 123 | `_curve_summary` | [docs/registry/implementation-map.yaml:L18123-L18132] |
| 124 | `_curve_csv_path` | [docs/registry/implementation-map.yaml:L18133-L18144] |
| 125 | `_input_stem` | [docs/registry/implementation-map.yaml:L18145-L18154] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` / `FatigueAnalysis.router` | [docs/registry/implementation-map.yaml:L18155-L18167] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` / `FatigueAnalysis.damage_from_sn_data` | [docs/registry/implementation-map.yaml:L18168-L18177] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` / `FatigueAnalysis.damage_from_rainflow_cycles` | [docs/registry/implementation-map.yaml:L18178-L18187] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` / `FatigueAnalysis.damage_from_timetrace` | [docs/registry/implementation-map.yaml:L18188-L18200] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` / `FatigueAnalysis.get_rainflow_from_timetrace` | [docs/registry/implementation-map.yaml:L18201-L18210] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` / `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | [docs/registry/implementation-map.yaml:L18211-L18220] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` / `ShipFatigueAnalysis.router` | [docs/registry/implementation-map.yaml:L18221-L18232] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` / `ShipFatigueAnalysis.get_lcf_timetrace_rainflow_and_fatigue_damage` | [docs/registry/implementation-map.yaml:L18233-L18244] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` / `ShipFatigueAnalysis.get_fatigue_damage_output_df_for_timetrace` | [docs/registry/implementation-map.yaml:L18245-L18256] |
| 135 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` / `ShipFatigueAnalysis.get_fatigue_df_definition_for_timetrace` | [docs/registry/implementation-map.yaml:L18257-L18266] |
| 136 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` / `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | [docs/registry/implementation-map.yaml:L18267-L18276] |
| 137 | `SNcurve.cycles_to_failure` | [docs/registry/implementation-map.yaml:L18277-L18286] |
| 138 | `RainflowFatigue.count_cycles` | [docs/registry/implementation-map.yaml:L18287-L18296] |
| 139 | `MinersRuleDamage.compute` | [docs/registry/implementation-map.yaml:L18297-L18306] |
| 140 | `ScatterDiagramFatigue.compute` | [docs/registry/implementation-map.yaml:L18307-L18316] |
| 141 | `compute_fatigue_life` | [docs/registry/implementation-map.yaml:L18317-L18326] |
| 142 | `StratigraphicAnalysis.add_well_analysis` | [docs/registry/implementation-map.yaml:L18327-L18336] |
| 143 | `StratigraphicAnalysis.set_well_order` | [docs/registry/implementation-map.yaml:L18337-L18346] |
| 144 | `StratigraphicAnalysis.create_flattened_section` | [docs/registry/implementation-map.yaml:L18347-L18356] |
| 145 | `WellCorrelation.add_well_data` | [docs/registry/implementation-map.yaml:L18357-L18366] |
| 146 | `CoreAnalysis.correlate_core_to_logs` | [docs/registry/implementation-map.yaml:L18367-L18376] |
| 147 | `ReservoirAnalysis.add_well_analysis` | [docs/registry/implementation-map.yaml:L18377-L18386] |
| 148 | `ReservoirAnalysis.run_comprehensive_analysis` | [docs/registry/implementation-map.yaml:L18387-L18396] |
| 149 | `router` | [docs/registry/implementation-map.yaml:L18397-L18410] |
| 150 | `_evaluate_line` | [docs/registry/implementation-map.yaml:L18411-L18421] |
| 151 | `_history_bins` | [docs/registry/implementation-map.yaml:L18422-L18431] |
| 152 | `_curve_settings` | [docs/registry/implementation-map.yaml:L18432-L18441] |
| 153 | `_mooring_fatigue_damage` | [docs/registry/implementation-map.yaml:L18442-L18452] |
| 154 | `_free_span_utilisation` | [docs/registry/implementation-map.yaml:L18453-L18462] |
| 155 | `_span_rectification_utilisation` | [docs/registry/implementation-map.yaml:L18463-L18472] |
| 156 | `<module>` | [docs/registry/implementation-map.yaml:L18473-L18481] |
| 157 | `router` | [docs/registry/implementation-map.yaml:L18482-L18493] |
| 158 | `assess_touchdown_fatigue` | [docs/registry/implementation-map.yaml:L18494-L18503] |
| 159 | `router` | [docs/registry/implementation-map.yaml:L18504-L18515] |
| 160 | `_viv_damage` | [docs/registry/implementation-map.yaml:L18516-L18526] |
| 161 | `_curve_settings` | [docs/registry/implementation-map.yaml:L18527-L18536] |
| 162 | `TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series` | [docs/registry/implementation-map.yaml:L18537-L18546] |
| 163 | `FatigueAnalysisAdapter.get_rainflow_from_timetrace` | [docs/registry/implementation-map.yaml:L18547-L18556] |
| 164 | `FatigueAnalysisAdapter.damage_from_rainflow_cycles` | [docs/registry/implementation-map.yaml:L18557-L18566] |
| 165 | `OrcaFlexAdapter.RainflowHalfCycles` | [docs/registry/implementation-map.yaml:L18567-L18576] |
| 166 | `rainflow_cmd` | [docs/registry/implementation-map.yaml:L18577-L18586] |
| 167 | `RainflowCounter.count_cycles` | [docs/registry/implementation-map.yaml:L18587-L18596] |
| 168 | `SNCurve._bilinear` | [docs/registry/implementation-map.yaml:L18597-L18606] |
| 169 | `SNCurve.from_test_data` | [docs/registry/implementation-map.yaml:L18607-L18616] |
| 170 | `FatigueDamageCalculator.calculate_damage` | [docs/registry/implementation-map.yaml:L18617-L18632] |
| 171 | `FatigueDamageCalculator._miners_rule` | [docs/registry/implementation-map.yaml:L18633-L18644] |
| 172 | `FatigueDamageCalculator._modified_miners_rule` | [docs/registry/implementation-map.yaml:L18645-L18655] |
| 173 | `FatigueDamageCalculator._nonlinear_damage` | [docs/registry/implementation-map.yaml:L18656-L18666] |
| 174 | `FatigueDamageCalculator._calculate_allowable_cycles` | [docs/registry/implementation-map.yaml:L18667-L18676] |
| 175 | `FatigueDamageCalculator._apply_mean_stress_correction` | [docs/registry/implementation-map.yaml:L18677-L18686] |
| 176 | `FatigueDamageCalculator._estimate_cycles_to_failure` | [docs/registry/implementation-map.yaml:L18687-L18696] |
| 177 | `FatigueDamageCalculator.calculate_equivalent_stress` | [docs/registry/implementation-map.yaml:L18697-L18706] |
| 178 | `create_parser` | [docs/registry/implementation-map.yaml:L18707-L18716] |
| 179 | `build_config_from_args` | [docs/registry/implementation-map.yaml:L18717-L18726] |
| 180 | `main` | [docs/registry/implementation-map.yaml:L18727-L18739] |
| 181 | `TimeSeriesAnalyzer._analyze_column` | [docs/registry/implementation-map.yaml:L18740-L18749] |
| 182 | `TimeSeriesAnalyzer._generate_outputs` | [docs/registry/implementation-map.yaml:L18750-L18759] |
| 183 | `TimeSeriesAnalyzer._generate_summary` | [docs/registry/implementation-map.yaml:L18760-L18769] |
| 184 | `SafeAnalyzer.safe_rainflow_analysis` | [docs/registry/implementation-map.yaml:L18770-L18779] |
| 185 | `TimeSeriesAnalysis.router` | [docs/registry/implementation-map.yaml:L18780-L18789] |
| 186 | `TimeSeriesComponents.count_cycles` | [docs/registry/implementation-map.yaml:L18790-L18799] |
| 187 | `OPPTimeSeries.get_histogram_object_from_orcaflex_run` | [docs/registry/implementation-map.yaml:L18800-L18809] |
| 188 | `OPPTimeSeries.get_detailed_histogram_df_columns` | [docs/registry/implementation-map.yaml:L18810-L18819] |
| 189 | `OPPTimeSeriesV2.process_histograms` | [docs/registry/implementation-map.yaml:L18820-L18829] |
| 190 | `OrcaFlexAnalysis.get_histogram_object_from_orcaflex_run` | [docs/registry/implementation-map.yaml:L18830-L18839] |
| 191 | `OrcaFlexAnalysis.get_detailed_histogram_df_columns` | [docs/registry/implementation-map.yaml:L18840-L18849] |
| 192 | `OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` | [docs/registry/implementation-map.yaml:L18850-L18861] |
| 193 | `OrcaFlexTimeTraceProcessor._parallel_fatigue_analysis` | [docs/registry/implementation-map.yaml:L18862-L18871] |
| 194 | `OrcaFlexTimeTraceProcessor._analyze_single_trace` | [docs/registry/implementation-map.yaml:L18872-L18881] |
| 195 | `router` | [docs/registry/implementation-map.yaml:L18882-L18893] |
| 196 | `_sn_curve` | [docs/registry/implementation-map.yaml:L18894-L18903] |
| 197 | `quick_fatigue_analysis` | [docs/registry/implementation-map.yaml:L18904-L18917] |
| 198 | `get_module_info` | [docs/registry/implementation-map.yaml:L18918-L18927] |
| 199 | `<module>` | [docs/registry/implementation-map.yaml:L18928-L18936] |
| 200 | `FatigueAnalysisEngine._setup_sn_curve` | [docs/registry/implementation-map.yaml:L18937-L18947] |
| 201 | `FatigueAnalysisEngine._setup_damage_calculator` | [docs/registry/implementation-map.yaml:L18948-L18957] |
| 202 | `FatigueAnalysisEngine._setup_rainflow_counter` | [docs/registry/implementation-map.yaml:L18958-L18967] |
| 203 | `FatigueAnalysisEngine.analyze_time_series` | [docs/registry/implementation-map.yaml:L18968-L18979] |
| 204 | `FatigueAnalysisEngine.analyze_psd` | [docs/registry/implementation-map.yaml:L18980-L18989] |
| 205 | `FatigueAnalysisEngine.calculate_high_stress_range_life` | [docs/registry/implementation-map.yaml:L18990-L18999] |
| 206 | `FatigueAnalysisEngine.analyze_shear_data` | [docs/registry/implementation-map.yaml:L19000-L19009] |
| 207 | `FatigueAnalysisEngine._perform_engineering_validation` | [docs/registry/implementation-map.yaml:L19010-L19019] |
| 208 | `quick_time_domain_analysis` | [docs/registry/implementation-map.yaml:L19020-L19029] |
| 209 | `quick_frequency_domain_analysis` | [docs/registry/implementation-map.yaml:L19030-L19039] |
| 210 | `<module>` | [docs/registry/implementation-map.yaml:L19040-L19048] |
| 211 | `LinearDamageAccumulation.calculate_damage` | [docs/registry/implementation-map.yaml:L19049-L19060] |
| 212 | `LinearDamageAccumulation._apply_mean_stress_correction` | [docs/registry/implementation-map.yaml:L19061-L19070] |
| 213 | `ModifiedMinersRule.calculate_damage` | [docs/registry/implementation-map.yaml:L19071-L19080] |
| 214 | `NonlinearDamageAccumulation.calculate_damage` | [docs/registry/implementation-map.yaml:L19081-L19094] |
| 215 | `NonlinearDamageAccumulation._marco_starkey_damage` | [docs/registry/implementation-map.yaml:L19095-L19104] |
| 216 | `NonlinearDamageAccumulation._corten_dolan_damage` | [docs/registry/implementation-map.yaml:L19105-L19114] |
| 217 | `NonlinearDamageAccumulation._double_linear_damage` | [docs/registry/implementation-map.yaml:L19115-L19124] |
| 218 | `compare_damage_methods` | [docs/registry/implementation-map.yaml:L19125-L19134] |
| 219 | `_build_curve` | [docs/registry/implementation-map.yaml:L19135-L19144] |
| 220 | `failure_probability` | [docs/registry/implementation-map.yaml:L19145-L19154] |
| 221 | `sensitivity_analysis` | [docs/registry/implementation-map.yaml:L19155-L19164] |
| 222 | `monte_carlo_fatigue` | [docs/registry/implementation-map.yaml:L19165-L19174] |
| 223 | `<module>` | [docs/registry/implementation-map.yaml:L19175-L19183] |
| 224 | `NarrowBandMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L19184-L19193] |
| 225 | `DirlikMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L19194-L19203] |
| 226 | `TovoBenasciuttiMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L19204-L19213] |
| 227 | `SingleMomentMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L19214-L19223] |
| 228 | `compare_frequency_methods` | [docs/registry/implementation-map.yaml:L19224-L19233] |
| 229 | `run_sweep` | [docs/registry/implementation-map.yaml:L19234-L19243] |
| 230 | `compute_sensitivity._single_damage` | [docs/registry/implementation-map.yaml:L19244-L19253] |
| 231 | `_build_scatter_json` | [docs/registry/implementation-map.yaml:L19254-L19263] |
| 232 | `<module>` | [docs/registry/implementation-map.yaml:L19264-L19272] |
| 233 | `RainflowCounter.count_cycles` | [docs/registry/implementation-map.yaml:L19273-L19284] |
| 234 | `RainflowCounter._rainflow_count` | [docs/registry/implementation-map.yaml:L19285-L19294] |
| 235 | `RainflowCounter.extract_cycles_with_means` | [docs/registry/implementation-map.yaml:L19295-L19306] |
| 236 | `rainflow_count` | [docs/registry/implementation-map.yaml:L19307-L19316] |
| 237 | `rainflow_with_means` | [docs/registry/implementation-map.yaml:L19317-L19326] |
| 238 | `scatter_fatigue_damage` | [docs/registry/implementation-map.yaml:L19327-L19336] |
| 239 | `fatigue_assessment` | [docs/registry/implementation-map.yaml:L19337-L19348] |
| 240 | `main` | [docs/registry/implementation-map.yaml:L19349-L19358] |
| 241 | `<module>` | [docs/registry/implementation-map.yaml:L19359-L19367] |
| 242 | `StandardSNCurves._ensure_loaded` | [docs/registry/implementation-map.yaml:L19368-L19377] |
| 243 | `StandardSNCurves.register_custom_curve` | [docs/registry/implementation-map.yaml:L19378-L19387] |
| 244 | `StandardSNCurves.get_dnv_c203_curve` | [docs/registry/implementation-map.yaml:L19388-L19397] |
| 245 | `StandardSNCurves.get_curve` | [docs/registry/implementation-map.yaml:L19398-L19407] |
| 246 | `StandardSNCurves.get_multislope_curve` | [docs/registry/implementation-map.yaml:L19408-L19417] |
| 247 | `ThicknessCorrection.apply_thickness_effect` | [docs/registry/implementation-map.yaml:L19418-L19427] |
| 248 | `SNDataFitting.fit_power_law` | [docs/registry/implementation-map.yaml:L19428-L19437] |
| 249 | `get_dnv_curve` | [docs/registry/implementation-map.yaml:L19438-L19447] |
| 250 | `get_api_curve` | [docs/registry/implementation-map.yaml:L19448-L19457] |
| 251 | `get_bs_curve` | [docs/registry/implementation-map.yaml:L19458-L19467] |
| 252 | `_dnv_curves_from_c203` | [docs/registry/implementation-map.yaml:L19468-L19477] |
| 253 | `_dnv_seawater_cp` | [docs/registry/implementation-map.yaml:L19478-L19487] |
| 254 | `_dnv_seawater_free` | [docs/registry/implementation-map.yaml:L19488-L19497] |
| 255 | `_chain_seawater_cp` | [docs/registry/implementation-map.yaml:L19498-L19507] |
| 256 | `pipeline_girth_weld` | [docs/registry/implementation-map.yaml:L19508-L19520] |
| 257 | `scr_touchdown` | [docs/registry/implementation-map.yaml:L19521-L19533] |
| 258 | `mooring_chain` | [docs/registry/implementation-map.yaml:L19534-L19546] |
| 259 | `_compute_example` | [docs/registry/implementation-map.yaml:L19547-L19556] |
| 260 | `run_analysis` | [docs/registry/implementation-map.yaml:L19557-L19568] |
| 261 | `SNCurve._load_sn_parameters` | [docs/registry/implementation-map.yaml:L19569-L19578] |
| 262 | `SNCurve.calculate_cycles_to_failure` | [docs/registry/implementation-map.yaml:L19579-L19588] |
| 263 | `FatigueDamageCalculator.calculate_damage` | [docs/registry/implementation-map.yaml:L19589-L19598] |
| 264 | `main` | [docs/registry/implementation-map.yaml:L19599-L19608] |
| 265 | `<module>` | [docs/registry/implementation-map.yaml:L19609-L19617] |
| 266 | `SNCurveParameters.a1` | [docs/registry/implementation-map.yaml:L19618-L19627] |
| 267 | `SNCurveParameters.a2` | [docs/registry/implementation-map.yaml:L19628-L19637] |
| 268 | `FatigueDamageCalculator.cycles_to_failure` | [docs/registry/implementation-map.yaml:L19638-L19647] |
| 269 | `FatigueDamageCalculator.calculate_damage` | [docs/registry/implementation-map.yaml:L19648-L19659] |
| 270 | `FatigueDamageCalculator.export_results` | [docs/registry/implementation-map.yaml:L19660-L19669] |
| 271 | `FatigueFileNamer.get_rainflow_filename` | [docs/registry/implementation-map.yaml:L19670-L19679] |
| 272 | `<module>` | [docs/registry/implementation-map.yaml:L19680-L19688] |
| 273 | `IntegratedFatigueProcessor.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L19689-L19698] |
| 274 | `main` | [docs/registry/implementation-map.yaml:L19699-L19708] |
| 275 | `IntegratedFatigueProcessorWithNaming._process_condition_base` | [docs/registry/implementation-map.yaml:L19709-L19718] |
| 276 | `<module>` | [docs/registry/implementation-map.yaml:L19719-L19727] |
| 277 | `RainflowCounter.count_cycles` | [docs/registry/implementation-map.yaml:L19728-L19739] |
| 278 | `RainflowCounter._rainflow_algorithm` | [docs/registry/implementation-map.yaml:L19740-L19749] |
| 279 | `RainflowCounter.export_results` | [docs/registry/implementation-map.yaml:L19750-L19759] |
| 280 | `rainflow_count` | [docs/registry/implementation-map.yaml:L19760-L19769] |
| 281 | `RainflowCounter.process_time_series` | [docs/registry/implementation-map.yaml:L19770-L19779] |
| 282 | `RainflowCounter.process_batch` | [docs/registry/implementation-map.yaml:L19780-L19791] |
| 283 | `RainflowCounter.save_cycle_data` | [docs/registry/implementation-map.yaml:L19792-L19801] |
| 284 | `main` | [docs/registry/implementation-map.yaml:L19802-L19811] |
| 285 | `ReferenceSeaStateProcessor.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L19812-L19821] |
| 286 | `TensionStressConverter.process_batch` | [docs/registry/implementation-map.yaml:L19822-L19831] |
| 287 | `main` | [docs/registry/implementation-map.yaml:L19832-L19841] |
| 288 | `generate_all_visualizations` | [docs/registry/implementation-map.yaml:L19842-L19851] |
| 289 | `narrow_band_damage` | [docs/registry/implementation-map.yaml:L19852-L19861] |
| 290 | `assess_jumper_fatigue` | [docs/registry/implementation-map.yaml:L19862-L19873] |
| 291 | `_dnv_params` | [docs/registry/implementation-map.yaml:L19874-L19883] |
| 292 | `get_sn_curve` | [docs/registry/implementation-map.yaml:L19884-L19895] |
| 293 | `SpanFatigueDamage._load_sn_curve` | [docs/registry/implementation-map.yaml:L19896-L19907] |
| 294 | `VIVFatigueCalculator.calculate_damage` | [docs/registry/implementation-map.yaml:L19908-L19917] |
| 295 | `VIVFatigueCalculator.calculate_fatigue_life` | [docs/registry/implementation-map.yaml:L19918-L19927] |
| 296 | `VIVFatigueCalculator.analyze_viv_fatigue` | [docs/registry/implementation-map.yaml:L19928-L19940] |
| 297 | `VIVFatigueCalculator.cumulative_damage` | [docs/registry/implementation-map.yaml:L19941-L19952] |
| 298 | `parse_inputs` | [docs/registry/implementation-map.yaml:L19953-L19962] |
| 299 | `router` | [docs/registry/implementation-map.yaml:L19963-L19976] |
| 300 | `_damage_rate` | [docs/registry/implementation-map.yaml:L19977-L19987] |
| 301 | `_provenance` | [docs/registry/implementation-map.yaml:L19988-L19997] |
| 302 | `page` | [docs/registry/implementation-map.yaml:L19998-L20007] |
| 303 | `_check_receipt` | [docs/registry/implementation-map.yaml:L20008-L20017] |
| 304 | `verify_pressure_bundle` | [docs/registry/implementation-map.yaml:L20018-L20029] |
| 305 | `TestThicknessCorrection.test_known_correction` | [docs/registry/implementation-map.yaml:L20030-L20039] |
| 306 | `bundle` | [docs/registry/implementation-map.yaml:L20040-L20049] |
| 307 | `histogram_input` | [docs/registry/implementation-map.yaml:L20050-L20061] |
| 308 | `_span_input` | [docs/registry/implementation-map.yaml:L20062-L20071] |
| 309 | `_non_dnv_bilinear` | [docs/registry/implementation-map.yaml:L20072-L20081] |
| 310 | `test_non_dnv_bilinear_switch_stays_at_endurance_limit` | [docs/registry/implementation-map.yaml:L20082-L20095] |
| 311 | `_signal_e` | [docs/registry/implementation-map.yaml:L20096-L20105] |
| 312 | `_user_curve` | [docs/registry/implementation-map.yaml:L20106-L20115] |
| 313 | `_span_input` | [docs/registry/implementation-map.yaml:L20116-L20125] |
| 314 | `_fresh_standard_curves` | [docs/registry/implementation-map.yaml:L20126-L20135] |
| 315 | `_data_dir` | [docs/registry/implementation-map.yaml:L20136-L20145] |
| 316 | `f_curve` | [docs/registry/implementation-map.yaml:L20146-L20156] |
| 317 | `TestThicknessCorrection.test_correction_above_ref` | [docs/registry/implementation-map.yaml:L20157-L20166] |
| 318 | `TestThicknessCorrection.test_correction_formula` | [docs/registry/implementation-map.yaml:L20167-L20176] |
| 319 | `TestAgainstTransliterationSweep.test_all_presets_and_curves` | [docs/registry/implementation-map.yaml:L20177-L20188] |
| 320 | `_cfg` | [docs/registry/implementation-map.yaml:L20189-L20198] |
| 321 | `_library_reference` | [docs/registry/implementation-map.yaml:L20199-L20209] |
| 322 | `_sample_locations` | [docs/registry/implementation-map.yaml:L20210-L20219] |
| 323 | `_damage_calls` | [docs/registry/implementation-map.yaml:L20220-L20232] |
| 324 | `_damage_calls.calm_scatter` | [docs/registry/implementation-map.yaml:L20233-L20242] |
| 325 | `_damage_calls.fapps_batch` | [docs/registry/implementation-map.yaml:L20243-L20252] |
| 326 | `TestGetSNCurve.test_f_curve_at_100mpa` | [docs/registry/implementation-map.yaml:L20253-L20263] |
| 327 | `TestGetSNCurve.test_d_curve_at_100mpa` | [docs/registry/implementation-map.yaml:L20264-L20274] |
| 328 | `_hand_N` | [docs/registry/implementation-map.yaml:L20275-L20284] |
| 329 | `TestSNcurve.test_cycles_to_failure_chain_known_value` | [docs/registry/implementation-map.yaml:L20285-L20294] |
| 330 | `TestSNcurve.test_cycles_to_failure_wire_rope_known_value` | [docs/registry/implementation-map.yaml:L20295-L20304] |
| 331 | `TestSNcurve.test_custom_sn_curve` | [docs/registry/implementation-map.yaml:L20305-L20314] |
| 332 | `TestMinersRuleDamage.test_single_range_known_damage` | [docs/registry/implementation-map.yaml:L20315-L20324] |
| 333 | `TestMinersRuleDamage.test_damage_additivity` | [docs/registry/implementation-map.yaml:L20325-L20334] |
| 334 | `_small_atlas` | [docs/registry/implementation-map.yaml:L20335-L20344] |
| 335 | `process_single_test_file` | [docs/registry/implementation-map.yaml:L20345-L20354] |
| 336 | `process_single_test_file` | [docs/registry/implementation-map.yaml:L20355-L20364] |
| 337 | `<module>` | [docs/registry/implementation-map.yaml:L20365-L20373] |
| 338 | `analyze_signal` | [docs/registry/implementation-map.yaml:L20374-L20383] |
| 339 | `main` | [docs/registry/implementation-map.yaml:L20384-L20395] |
| 340 | `OrcaFlexTensionAnalyzer.analyze_tension` | [docs/registry/implementation-map.yaml:L20396-L20405] |
| 341 | `OrcaFlexTensionAnalyzer.export_results` | [docs/registry/implementation-map.yaml:L20406-L20415] |
| 342 | `run_rainflow_for_timeseries` | [docs/registry/implementation-map.yaml:L20416-L20425] |
| 343 | `test_rainflow_for_timeseries` | [docs/registry/implementation-map.yaml:L20426-L20437] |
| 344 | `main` | [docs/registry/implementation-map.yaml:L20438-L20452] |
| 345 | `mock_solver_factory.MockSolverFactory.create_fatigue_solver` | [docs/registry/implementation-map.yaml:L20453-L20462] |
| 346 | `simple_sn_curve` | [docs/registry/implementation-map.yaml:L20463-L20472] |
| 347 | `dnv_d_curve` | [docs/registry/implementation-map.yaml:L20473-L20482] |
| 348 | `simple_curve` | [docs/registry/implementation-map.yaml:L20483-L20492] |
| 349 | `curve_with_material` | [docs/registry/implementation-map.yaml:L20493-L20502] |
| 350 | `TestSNCurveMigration.test_multislope_curve_implementation` | [docs/registry/implementation-map.yaml:L20503-L20512] |
| 351 | `TestSNCurveMigration.test_curve_continuity` | [docs/registry/implementation-map.yaml:L20513-L20522] |
| 352 | `TestFrequencyDomainMigration.test_basic_frequency_analysis` | [docs/registry/implementation-map.yaml:L20523-L20532] |
| 353 | `simple_sn` | [docs/registry/implementation-map.yaml:L20533-L20542] |
| 354 | `mild_sn` | [docs/registry/implementation-map.yaml:L20543-L20552] |
| 355 | `_simple_input` | [docs/registry/implementation-map.yaml:L20553-L20562] |
| 356 | `TestOffshoreScenario.test_high_cycle_low_stress_result` | [docs/registry/implementation-map.yaml:L20563-L20574] |
| 357 | `simple_sn_curve` | [docs/registry/implementation-map.yaml:L20575-L20584] |
| 358 | `default_counter` | [docs/registry/implementation-map.yaml:L20585-L20594] |
| 359 | `simple_sn` | [docs/registry/implementation-map.yaml:L20595-L20604] |
| 360 | `TestFatigueLife.test_fatigue_life_inverse_of_damage` | [docs/registry/implementation-map.yaml:L20605-L20616] |
| 361 | `simple_power_law` | [docs/registry/implementation-map.yaml:L20617-L20626] |
| 362 | `power_law_with_limit` | [docs/registry/implementation-map.yaml:L20627-L20636] |
| 363 | `bilinear_curve` | [docs/registry/implementation-map.yaml:L20637-L20646] |
| 364 | `dnv_d_curve` | [docs/registry/implementation-map.yaml:L20647-L20656] |
| 365 | `TestBS7608RevisedCurveParameters.test_bs7608_revised_d_known_n_at_50mpa` | [docs/registry/implementation-map.yaml:L20657-L20666] |
| 366 | `TestISO19902CurveParameters.test_iso19902_f_air_known_n_at_100mpa` | [docs/registry/implementation-map.yaml:L20667-L20676] |
| 367 | `TestISO19902CurveParameters.test_iso19902_f2_sw_cp_known_n_at_50mpa` | [docs/registry/implementation-map.yaml:L20677-L20686] |
| 368 | `_all_single_slope_cases` | [docs/registry/implementation-map.yaml:L20687-L20696] |
| 369 | `_all_multislope_cases` | [docs/registry/implementation-map.yaml:L20697-L20706] |
| 370 | `sn_curve_data` | [docs/registry/implementation-map.yaml:L20707-L20716] |
| 371 | `run_fatigue_analysis` | [docs/registry/implementation-map.yaml:L20717-L20727] |
| 372 | `run_fatigue_analysis` | [docs/registry/implementation-map.yaml:L20728-L20738] |
| 373 | `ref_input` | [docs/registry/implementation-map.yaml:L20739-L20748] |
| 374 | `TestVIVFatigueCalculator.test_cumulative_damage` | [docs/registry/implementation-map.yaml:L20749-L20758] |
| 375 | `get_orcaflex_post_process_mock` | [docs/registry/implementation-map.yaml:L20759-L20768] |
| 376 | `ComprehensiveTestFixCampaign.generate_mock_data` | [docs/registry/implementation-map.yaml:L20769-L20778] |
| 377 | `process_configuration` | [docs/registry/implementation-map.yaml:L20779-L20788] |
| 378 | `main` | [docs/registry/implementation-map.yaml:L20789-L20800] |
| 379 | `run_all_tests` | [docs/registry/implementation-map.yaml:L20801-L20810] |
| 380 | `process_effective_tension_file` | [docs/registry/implementation-map.yaml:L20811-L20820] |
| 381 | `main` | [docs/registry/implementation-map.yaml:L20821-L20831] |
| 382 | `run_performance_tests` | [docs/registry/implementation-map.yaml:L20832-L20841] |
| 383 | `TestSNCurveDNVReference.test_dnv_d_curve_parameters` | [docs/registry/implementation-map.yaml:L20842-L20853] |
| 384 | `test_workflow_registry` | [docs/registry/implementation-map.yaml:L20854-L20864] |
| 385 | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | [docs/registry/implementation-map.yaml:L20865-L20875] |
| 386 | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | [docs/registry/implementation-map.yaml:L20876-L20886] |
| 387 | `PowerLawSNCurve.get_allowable_cycles` | [docs/registry/implementation-map.yaml:L20887-L20897] |
| 388 | `PowerLawSNCurve.get_stress_range` | [docs/registry/implementation-map.yaml:L20898-L20907] |
| 389 | `BilinearSNCurve.get_allowable_cycles` | [docs/registry/implementation-map.yaml:L20908-L20917] |
| 390 | `BilinearSNCurve.get_stress_range` | [docs/registry/implementation-map.yaml:L20918-L20928] |
| 391 | `BilinearSNCurve.get_allowable_cycles` | [docs/registry/implementation-map.yaml:L20929-L20938] |
| 392 | `BilinearSNCurve.get_stress_range` | [docs/registry/implementation-map.yaml:L20939-L20948] |

#### cathodic-protection-calculations

| Record | Definition / source locator | Map record |
| --- | --- | --- |
| 1 | `main` | [docs/registry/implementation-map.yaml:L20971-L20987] |
| 2 | `<module>` | [docs/registry/implementation-map.yaml:L20988-L20996] |
| 3 | `run_single_case` | [docs/registry/implementation-map.yaml:L20997-L21009] |
| 4 | `run_parametric_sweep` | [docs/registry/implementation-map.yaml:L21010-L21021] |
| 5 | `build_summary_table` | [docs/registry/implementation-map.yaml:L21022-L21031] |
| 6 | `run_single_case` | [docs/registry/implementation-map.yaml:L21032-L21047] |
| 7 | `run_parametric_sweep` | [docs/registry/implementation-map.yaml:L21048-L21059] |
| 8 | `<module>` | [docs/registry/implementation-map.yaml:L21060-L21068] |
| 9 | `run_single_case` | [docs/registry/implementation-map.yaml:L21069-L21081] |
| 10 | `<module>` | [docs/registry/implementation-map.yaml:L21082-L21090] |
| 11 | `run_single_case` | [docs/registry/implementation-map.yaml:L21091-L21105] |
| 12 | `run_parametric_sweep` | [docs/registry/implementation-map.yaml:L21106-L21117] |
| 13 | `test_pipeline_end_to_end_writes_artifacts` | [docs/registry/implementation-map.yaml:L21118-L21127] |
| 14 | `test_pipeline_end_to_end_writes_artifacts` | [docs/registry/implementation-map.yaml:L21128-L21137] |
| 15 | `<module>` | [docs/registry/implementation-map.yaml:L21138-L21146] |
| 16 | `_zones` | [docs/registry/implementation-map.yaml:L21147-L21156] |
| 17 | `build_study` | [docs/registry/implementation-map.yaml:L21157-L21169] |
| 18 | `generate_comparison_dashboard` | [docs/registry/implementation-map.yaml:L21170-L21179] |
| 19 | `generate_summary_csv` | [docs/registry/implementation-map.yaml:L21180-L21189] |
| 20 | `cathodic_protection_rows` | [docs/registry/implementation-map.yaml:L21190-L21199] |
| 21 | `checks` | [docs/registry/implementation-map.yaml:L21200-L21211] |
| 22 | `sensitivity` | [docs/registry/implementation-map.yaml:L21212-L21223] |
| 23 | `design_data` | [docs/registry/implementation-map.yaml:L21224-L21233] |
| 24 | `interactive_figures` | [docs/registry/implementation-map.yaml:L21234-L21243] |
| 25 | `build_document` | [docs/registry/implementation-map.yaml:L21244-L21258] |
| 26 | `section_blocks` | [docs/registry/implementation-map.yaml:L21259-L21271] |
| 27 | `_correct_page` | [docs/registry/implementation-map.yaml:L21272-L21281] |
| 28 | `current_demand` | [docs/registry/implementation-map.yaml:L21282-L21291] |
| 29 | `anode_mass` | [docs/registry/implementation-map.yaml:L21292-L21303] |
| 30 | `anode_mass_from_current_years` | [docs/registry/implementation-map.yaml:L21304-L21313] |
| 31 | `coating_breakdown_mean` | [docs/registry/implementation-map.yaml:L21314-L21323] |
| 32 | `coating_breakdown_final` | [docs/registry/implementation-map.yaml:L21324-L21333] |
| 33 | `anodes_for_current` | [docs/registry/implementation-map.yaml:L21334-L21343] |
| 34 | `longitudinal_resistance_per_m` | [docs/registry/implementation-map.yaml:L21344-L21353] |
| 35 | `depleted_long_flush` | [docs/registry/implementation-map.yaml:L21354-L21363] |
| 36 | `_coating_factors` | [docs/registry/implementation-map.yaml:L21364-L21373] |
| 37 | `_demand` | [docs/registry/implementation-map.yaml:L21374-L21385] |
| 38 | `_resistances` | [docs/registry/implementation-map.yaml:L21386-L21397] |
| 39 | `_requirements` | [docs/registry/implementation-map.yaml:L21398-L21407] |
| 40 | `design_abs_ships` | [docs/registry/implementation-map.yaml:L21408-L21423] |
| 41 | `_flags` | [docs/registry/implementation-map.yaml:L21424-L21433] |
| 42 | `evaluate_layout` | [docs/registry/implementation-map.yaml:L21434-L21446] |
| 43 | `build_result` | [docs/registry/implementation-map.yaml:L21447-L21456] |
| 44 | `calculate_remaining_life` | [docs/registry/implementation-map.yaml:L21457-L21466] |
| 45 | `generate_depletion_profile` | [docs/registry/implementation-map.yaml:L21467-L21476] |
| 46 | `calculate_current_demand` | [docs/registry/implementation-map.yaml:L21477-L21486] |
| 47 | `calculate_anode_mass` | [docs/registry/implementation-map.yaml:L21487-L21496] |
| 48 | `calculate_anode_resistance` | [docs/registry/implementation-map.yaml:L21497-L21506] |
| 49 | `depleted_equivalent_radius` | [docs/registry/implementation-map.yaml:L21507-L21516] |
| 50 | `design_cp_system` | [docs/registry/implementation-map.yaml:L21517-L21531] |
| 51 | `current_demand` | [docs/registry/implementation-map.yaml:L21532-L21541] |
| 52 | `current_per_anode` | [docs/registry/implementation-map.yaml:L21542-L21551] |
| 53 | `number_of_anodes` | [docs/registry/implementation-map.yaml:L21552-L21564] |
| 54 | `anode_life_years` | [docs/registry/implementation-map.yaml:L21565-L21574] |
| 55 | `_utilization` | [docs/registry/implementation-map.yaml:L21575-L21586] |
| 56 | `_radii` | [docs/registry/implementation-map.yaml:L21587-L21598] |
| 57 | `_resistances` | [docs/registry/implementation-map.yaml:L21599-L21610] |
| 58 | `_verification` | [docs/registry/implementation-map.yaml:L21611-L21623] |
| 59 | `design_anode_family` | [docs/registry/implementation-map.yaml:L21624-L21636] |
| 60 | `parse_families` | [docs/registry/implementation-map.yaml:L21637-L21648] |
| 61 | `allocation_check` | [docs/registry/implementation-map.yaml:L21649-L21658] |
| 62 | `reconcile_allocations` | [docs/registry/implementation-map.yaml:L21659-L21668] |
| 63 | `components` | [docs/registry/implementation-map.yaml:L21669-L21678] |
| 64 | `_component_rows` | [docs/registry/implementation-map.yaml:L21679-L21688] |
| 65 | `_allocation_rows` | [docs/registry/implementation-map.yaml:L21689-L21698] |
| 66 | `_host_rows` | [docs/registry/implementation-map.yaml:L21699-L21708] |
| 67 | `_references` | [docs/registry/implementation-map.yaml:L21709-L21718] |
| 68 | `_adequacy_rows` | [docs/registry/implementation-map.yaml:L21719-L21728] |
| 69 | `_design_family` | [docs/registry/implementation-map.yaml:L21729-L21741] |
| 70 | `_component_candidate` | [docs/registry/implementation-map.yaml:L21742-L21751] |
| 71 | `_results_payload` | [docs/registry/implementation-map.yaml:L21752-L21761] |
| 72 | `f103_breakdown` | [docs/registry/implementation-map.yaml:L21762-L21771] |
| 73 | `_zone_basis` | [docs/registry/implementation-map.yaml:L21772-L21785] |
| 74 | `_designed_zone` | [docs/registry/implementation-map.yaml:L21786-L21797] |
| 75 | `_component_result` | [docs/registry/implementation-map.yaml:L21798-L21807] |
| 76 | `_design_basis` | [docs/registry/implementation-map.yaml:L21808-L21817] |
| 77 | `_demand_series` | [docs/registry/implementation-map.yaml:L21818-L21827] |
| 78 | `_demand_rows` | [docs/registry/implementation-map.yaml:L21828-L21837] |
| 79 | `_current_demand` | [docs/registry/implementation-map.yaml:L21838-L21850] |
| 80 | `_family_rows` | [docs/registry/implementation-map.yaml:L21851-L21860] |
| 81 | `_mass_table` | [docs/registry/implementation-map.yaml:L21861-L21870] |
| 82 | `_citation_usage` | [docs/registry/implementation-map.yaml:L21871-L21880] |
| 83 | `build_b401_family_sections` | [docs/registry/implementation-map.yaml:L21881-L21894] |
| 84 | `_family` | [docs/registry/implementation-map.yaml:L21895-L21904] |
| 85 | `_zone_rows` | [docs/registry/implementation-map.yaml:L21905-L21917] |
| 86 | `_result_payload` | [docs/registry/implementation-map.yaml:L21918-L21927] |
| 87 | `_component_results` | [docs/registry/implementation-map.yaml:L21928-L21937] |
| 88 | `_phase_family_row` | [docs/registry/implementation-map.yaml:L21938-L21947] |
| 89 | `_one_phase` | [docs/registry/implementation-map.yaml:L21948-L21959] |
| 90 | `_phases_table` | [docs/registry/implementation-map.yaml:L21960-L21969] |
| 91 | `retrofit_result` | [docs/registry/implementation-map.yaml:L21970-L21983] |
| 92 | `_remaining_anode_count` | [docs/registry/implementation-map.yaml:L21984-L21993] |
| 93 | `_balances` | [docs/registry/implementation-map.yaml:L21994-L22003] |
| 94 | `_counts` | [docs/registry/implementation-map.yaml:L22004-L22013] |
| 95 | `_ambient_anode_row` | [docs/registry/implementation-map.yaml:L22014-L22023] |
| 96 | `design_current_density` | [docs/registry/implementation-map.yaml:L22024-L22033] |
| 97 | `reinforcement_current_density` | [docs/registry/implementation-map.yaml:L22034-L22043] |
| 98 | `utilisation_factor` | [docs/registry/implementation-map.yaml:L22044-L22053] |
| 99 | `buried_current_density` | [docs/registry/implementation-map.yaml:L22054-L22063] |
| 100 | `coating_constants` | [docs/registry/implementation-map.yaml:L22064-L22073] |
| 101 | `coating_breakdown_factors` | [docs/registry/implementation-map.yaml:L22074-L22085] |
| 102 | `effective_bare_area_coated` | [docs/registry/implementation-map.yaml:L22086-L22095] |
| 103 | `_o2_reduction_density` | [docs/registry/implementation-map.yaml:L22096-L22105] |
| 104 | `_anode_potential` | [docs/registry/implementation-map.yaml:L22106-L22115] |
| 105 | `_cathode_potential` | [docs/registry/implementation-map.yaml:L22116-L22127] |
| 106 | `galvanic_corrosion` | [docs/registry/implementation-map.yaml:L22128-L22141] |
| 107 | `attenuation_analysis` | [docs/registry/implementation-map.yaml:L22142-L22151] |
| 108 | `current_demand` | [docs/registry/implementation-map.yaml:L22152-L22161] |
| 109 | `anode_mass_requirement` | [docs/registry/implementation-map.yaml:L22162-L22171] |
| 110 | `coating_breakdown_factor` | [docs/registry/implementation-map.yaml:L22172-L22181] |
| 111 | `anode_current_output` | [docs/registry/implementation-map.yaml:L22182-L22191] |
| 112 | `number_of_anodes` | [docs/registry/implementation-map.yaml:L22192-L22201] |
| 113 | `flush_anode_resistance` | [docs/registry/implementation-map.yaml:L22202-L22211] |
| 114 | `design_bracelet_cp` | [docs/registry/implementation-map.yaml:L22212-L22225] |
| 115 | `_b401_breakdown` | [docs/registry/implementation-map.yaml:L22226-L22235] |
| 116 | `_b401_densities` | [docs/registry/implementation-map.yaml:L22236-L22247] |
| 117 | `_published_densities` | [docs/registry/implementation-map.yaml:L22248-L22257] |
| 118 | `_b401_anode_geometry` | [docs/registry/implementation-map.yaml:L22258-L22268] |
| 119 | `_run_b401` | [docs/registry/implementation-map.yaml:L22269-L22285] |
| 120 | `_f103_input` | [docs/registry/implementation-map.yaml:L22286-L22295] |
| 121 | `_f103_results` | [docs/registry/implementation-map.yaml:L22296-L22305] |
| 122 | `_run_f103` | [docs/registry/implementation-map.yaml:L22306-L22319] |
| 123 | `_run_abs_ships_legacy` | [docs/registry/implementation-map.yaml:L22320-L22329] |
| 124 | `_run_abs_offshore` | [docs/registry/implementation-map.yaml:L22330-L22339] |
| 125 | `mean_current_density` | [docs/registry/implementation-map.yaml:L22340-L22349] |
| 126 | `bracelet_utilisation_factor` | [docs/registry/implementation-map.yaml:L22350-L22360] |
| 127 | `anode_capacity` | [docs/registry/implementation-map.yaml:L22361-L22370] |
| 128 | `effective_bare_area` | [docs/registry/implementation-map.yaml:L22371-L22380] |
| 129 | `current_demand_segment` | [docs/registry/implementation-map.yaml:L22381-L22392] |
| 130 | `total_current_demand` | [docs/registry/implementation-map.yaml:L22393-L22404] |
| 131 | `design_ground_bed` | [docs/registry/implementation-map.yaml:L22405-L22414] |
| 132 | `single_vertical_anode_resistance` | [docs/registry/implementation-map.yaml:L22415-L22424] |
| 133 | `multiple_vertical_anode_resistance` | [docs/registry/implementation-map.yaml:L22425-L22436] |
| 134 | `deep_well_resistance` | [docs/registry/implementation-map.yaml:L22437-L22447] |
| 135 | `horizontal_column_resistance` | [docs/registry/implementation-map.yaml:L22448-L22457] |
| 136 | `iccp_anode_life` | [docs/registry/implementation-map.yaml:L22458-L22467] |
| 137 | `anode_bed_design` | [docs/registry/implementation-map.yaml:L22468-L22482] |
| 138 | `cable_sizing` | [docs/registry/implementation-map.yaml:L22483-L22492] |
| 139 | `coating_breakdown_factor` | [docs/registry/implementation-map.yaml:L22493-L22502] |
| 140 | `pipeline_current_demand` | [docs/registry/implementation-map.yaml:L22503-L22512] |
| 141 | `anode_output_current` | [docs/registry/implementation-map.yaml:L22513-L22522] |
| 142 | `anode_mass_requirement` | [docs/registry/implementation-map.yaml:L22523-L22532] |
| 143 | `seawater_current_density` | [docs/registry/implementation-map.yaml:L22533-L22544] |
| 144 | `get_seawater_current_density` | [docs/registry/implementation-map.yaml:L22545-L22556] |
| 145 | `zone_current_density` | [docs/registry/implementation-map.yaml:L22557-L22567] |
| 146 | `calculate_zone_demand` | [docs/registry/implementation-map.yaml:L22568-L22579] |
| 147 | `_to_structural_zone` | [docs/registry/implementation-map.yaml:L22580-L22589] |
| 148 | `_design_marine_cp` | [docs/registry/implementation-map.yaml:L22590-L22603] |
| 149 | `__getattr__` | [docs/registry/implementation-map.yaml:L22604-L22613] |
| 150 | `standoff_anode_design_loop` | [docs/registry/implementation-map.yaml:L22614-L22624] |
| 151 | `zone_current_density` | [docs/registry/implementation-map.yaml:L22625-L22638] |
| 152 | `_zone_densities` | [docs/registry/implementation-map.yaml:L22639-L22649] |
| 153 | `marine_structure_current_demand` | [docs/registry/implementation-map.yaml:L22650-L22662] |
| 154 | `anode_distribution` | [docs/registry/implementation-map.yaml:L22663-L22673] |
| 155 | `retrofit_assessment` | [docs/registry/implementation-map.yaml:L22674-L22683] |
| 156 | `_factors` | [docs/registry/implementation-map.yaml:L22684-L22693] |
| 157 | `_side_basis` | [docs/registry/implementation-map.yaml:L22694-L22706] |
| 158 | `_structure` | [docs/registry/implementation-map.yaml:L22707-L22716] |
| 159 | `_resistance` | [docs/registry/implementation-map.yaml:L22717-L22727] |
| 160 | `_evaluate` | [docs/registry/implementation-map.yaml:L22728-L22739] |
| 161 | `_cable_limit_possible` | [docs/registry/implementation-map.yaml:L22740-L22749] |
| 162 | `_search_requirements` | [docs/registry/implementation-map.yaml:L22750-L22762] |
| 163 | `_bank` | [docs/registry/implementation-map.yaml:L22763-L22777] |
| 164 | `design_anode_bank_cp` | [docs/registry/implementation-map.yaml:L22778-L22790] |
| 165 | `pipeline_current_demand` | [docs/registry/implementation-map.yaml:L22791-L22800] |
| 166 | `anode_spacing` | [docs/registry/implementation-map.yaml:L22801-L22810] |
| 167 | `calculate_pipeline_current_demand` | [docs/registry/implementation-map.yaml:L22811-L22822] |
| 168 | `calculate_anode_spacing` | [docs/registry/implementation-map.yaml:L22823-L22832] |
| 169 | `soil_resistivity_correction` | [docs/registry/implementation-map.yaml:L22833-L22842] |
| 170 | `design_pipeline_cp` | [docs/registry/implementation-map.yaml:L22843-L22856] |
| 171 | `_check_arrangement` | [docs/registry/implementation-map.yaml:L22857-L22866] |
| 172 | `_arrangement_rows` | [docs/registry/implementation-map.yaml:L22867-L22876] |
| 173 | `_quantities` | [docs/registry/implementation-map.yaml:L22877-L22886] |
| 174 | `_initial_design` | [docs/registry/implementation-map.yaml:L22887-L22898] |
| 175 | `reference_arrangements` | [docs/registry/implementation-map.yaml:L22899-L22908] |
| 176 | `reference_checks` | [docs/registry/implementation-map.yaml:L22909-L22919] |
| 177 | `reference_sections` | [docs/registry/implementation-map.yaml:L22920-L22934] |
| 178 | `reference_basis` | [docs/registry/implementation-map.yaml:L22935-L22944] |
| 179 | `_model_outputs` | [docs/registry/implementation-map.yaml:L22945-L22957] |
| 180 | `aggregate_table` | [docs/registry/implementation-map.yaml:L22958-L22968] |
| 181 | `_quantity_rows` | [docs/registry/implementation-map.yaml:L22969-L22978] |
| 182 | `measured_options` | [docs/registry/implementation-map.yaml:L22979-L22991] |
| 183 | `verification_summary` | [docs/registry/implementation-map.yaml:L22992-L23001] |
| 184 | `dc_shift_limit_mV` | [docs/registry/implementation-map.yaml:L23002-L23011] |
| 185 | `pipe_attenuation_constant` | [docs/registry/implementation-map.yaml:L23012-L23023] |
| 186 | `_alpha` | [docs/registry/implementation-map.yaml:L23024-L23033] |
| 187 | `point_source_shift_mV` | [docs/registry/implementation-map.yaml:L23034-L23043] |
| 188 | `ac_current_density_A_m2` | [docs/registry/implementation-map.yaml:L23044-L23053] |
| 189 | `_assess_dc` | [docs/registry/implementation-map.yaml:L23054-L23066] |
| 190 | `_assess_ac` | [docs/registry/implementation-map.yaml:L23067-L23078] |
| 191 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection.ABS_gn_ships_2018` | [docs/registry/implementation-map.yaml:L23079-L23093] |
| 192 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection.DNV_RP_F103_2010` | [docs/registry/implementation-map.yaml:L23094-L23112] |
| 193 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._dnv_pipeline_geometry` | [docs/registry/implementation-map.yaml:L23113-L23122] |
| 194 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._dnv_coating_breakdown` | [docs/registry/implementation-map.yaml:L23123-L23132] |
| 195 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._dnv_current_densities` | [docs/registry/implementation-map.yaml:L23133-L23142] |
| 196 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._dnv_current_demand` | [docs/registry/implementation-map.yaml:L23143-L23152] |
| 197 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._dnv_anode_requirements` | [docs/registry/implementation-map.yaml:L23153-L23162] |
| 198 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._dnv_anode_spacing` | [docs/registry/implementation-map.yaml:L23163-L23172] |
| 199 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._dnv_attenuation` | [docs/registry/implementation-map.yaml:L23173-L23182] |
| 200 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection.ABS_gn_offshore_2018` | [docs/registry/implementation-map.yaml:L23183-L23195] |
| 201 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._abs_offshore_2018_current_demand` | [docs/registry/implementation-map.yaml:L23196-L23205] |
| 202 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._abs_breakdown_factors` | [docs/registry/implementation-map.yaml:L23206-L23215] |
| 203 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._abs_anode_mass_requirements` | [docs/registry/implementation-map.yaml:L23216-L23225] |
| 204 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._abs_anode_performance_checks` | [docs/registry/implementation-map.yaml:L23226-L23237] |
| 205 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection._abs_anode_resistance` | [docs/registry/implementation-map.yaml:L23238-L23247] |
| 206 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection.get_anode_current_capacity` | [docs/registry/implementation-map.yaml:L23248-L23257] |
| 207 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection.get_seawater_resistivity` | [docs/registry/implementation-map.yaml:L23258-L23267] |
| 208 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection.calculate_anode_resistance` | [docs/registry/implementation-map.yaml:L23268-L23279] |
| 209 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection.get_design_current_density` | [docs/registry/implementation-map.yaml:L23280-L23289] |
| 210 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection.get_required_current_density` | [docs/registry/implementation-map.yaml:L23290-L23301] |
| 211 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` / `CathodicProtection.get_anodes_required` | [docs/registry/implementation-map.yaml:L23302-L23313] |
| 212 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py` / `_b401_coating_breakdown` | [docs/registry/implementation-map.yaml:L23314-L23323] |
| 213 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py` / `_b401_current_densities` | [docs/registry/implementation-map.yaml:L23324-L23333] |
| 214 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py` / `_b401_anode_resistance` | [docs/registry/implementation-map.yaml:L23334-L23343] |
| 215 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py` / `_b401_anode_requirements` | [docs/registry/implementation-map.yaml:L23344-L23353] |
| 216 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py` / `_b401_verify_current_output` | [docs/registry/implementation-map.yaml:L23354-L23363] |
| 217 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py` / `_b401_offshore_platform` | [docs/registry/implementation-map.yaml:L23364-L23379] |
| 218 | `_cfg` | [docs/registry/implementation-map.yaml:L23380-L23389] |
| 219 | `expected_demand` | [docs/registry/implementation-map.yaml:L23390-L23399] |
| 220 | `expected_design` | [docs/registry/implementation-map.yaml:L23400-L23411] |
| 221 | `assert_design` | [docs/registry/implementation-map.yaml:L23412-L23423] |
| 222 | `ship_comparison_values` | [docs/registry/implementation-map.yaml:L23424-L23433] |
| 223 | `TestAnodeResistanceVerticalRod.test_dwight_equation_benchmark` | [docs/registry/implementation-map.yaml:L23434-L23443] |
| 224 | `test_shared_family_reconciles_pool_allocations_and_hosted_count_once` | [docs/registry/implementation-map.yaml:L23444-L23453] |
| 225 | `_all_results` | [docs/registry/implementation-map.yaml:L23454-L23463] |
| 226 | `_mixed_family_cfg` | [docs/registry/implementation-map.yaml:L23464-L23473] |
| 227 | `test_pipeline_cp_demand_uses_kernel` | [docs/registry/implementation-map.yaml:L23474-L23485] |
| 228 | `TestFieldJoints.test_bare_field_joints_raise_demand_and_shorten_protected_length` | [docs/registry/implementation-map.yaml:L23486-L23497] |
| 229 | `_sample_anode_sizing_input` | [docs/registry/implementation-map.yaml:L23498-L23507] |
| 230 | `_sample_marine_cp_input` | [docs/registry/implementation-map.yaml:L23508-L23517] |
| 231 | `jacket_results` | [docs/registry/implementation-map.yaml:L23518-L23529] |
| 232 | `test_pipeline_f103_bracelet_design` | [docs/registry/implementation-map.yaml:L23530-L23539] |
| 233 | `_assert_f103_2010_pipeline` | [docs/registry/implementation-map.yaml:L23540-L23549] |
| 234 | `_all_f103_results` | [docs/registry/implementation-map.yaml:L23550-L23559] |
| 235 | `TestCurrentDemand.test_single_segment_current` | [docs/registry/implementation-map.yaml:L23560-L23572] |
| 236 | `TestCurrentDemand.test_total_current_multiple_segments` | [docs/registry/implementation-map.yaml:L23573-L23585] |
| 237 | `test_multiple_vertical_anode_resistance_tp16_table` | [docs/registry/implementation-map.yaml:L23586-L23597] |
| 238 | `TestPipelineCurrentDemand.test_benchmark_case` | [docs/registry/implementation-map.yaml:L23598-L23609] |
| 239 | `TestAnodeResistance.test_benchmark_case` | [docs/registry/implementation-map.yaml:L23610-L23619] |
| 240 | `_hull_expected` | [docs/registry/implementation-map.yaml:L23620-L23629] |
| 241 | `_hull_actual` | [docs/registry/implementation-map.yaml:L23630-L23641] |
| 242 | `_riser_actual` | [docs/registry/implementation-map.yaml:L23642-L23652] |
| 243 | `_input` | [docs/registry/implementation-map.yaml:L23653-L23662] |
| 244 | `test_bank_demand_resistance_and_far_potential_are_hand_derived` | [docs/registry/implementation-map.yaml:L23663-L23675] |
| 245 | `test_buried_soil_pipeline_current_demand` | [docs/registry/implementation-map.yaml:L23676-L23686] |
| 246 | `test_reference_inputs_anode_mass_and_mass_only_arrangement` | [docs/registry/implementation-map.yaml:L23687-L23698] |
| 247 | `test_reference_checks_show_every_demand_capacity_margin_and_verdict` | [docs/registry/implementation-map.yaml:L23699-L23711] |
| 248 | `test_reference_reduced_arrangement_rechecks_without_mutating_inputs` | [docs/registry/implementation-map.yaml:L23712-L23722] |
| 249 | `test_published_jacket_reproduction_uses_one_unchanged_assumption_set` | [docs/registry/implementation-map.yaml:L23723-L23732] |
| 250 | `test_reference_summary_recommends_rechecked_count_after_stated_failure` | [docs/registry/implementation-map.yaml:L23733-L23742] |
| 251 | `test_reference_unavailable_recheck_does_not_claim_a_minimum` | [docs/registry/implementation-map.yaml:L23743-L23752] |
| 252 | `_b401_current_demand` | [docs/registry/implementation-map.yaml:L23753-L23763] |
| 253 | `dnv_base_config` | [docs/registry/implementation-map.yaml:L23764-L23773] |
| 254 | `TestDNVCurrentDemand.test_current_demand_with_design_factors` | [docs/registry/implementation-map.yaml:L23774-L23783] |
| 255 | `TestDNVAnodeSpacing.test_anode_spacing_basic` | [docs/registry/implementation-map.yaml:L23784-L23793] |
| 256 | `_historical_candidate` | [docs/registry/implementation-map.yaml:L23794-L23803] |
| 257 | `cp_design_parameters` | [docs/registry/implementation-map.yaml:L23804-L23813] |
| 258 | `test_abs_offshore_2018_tropical_deep_submerged` | [docs/registry/implementation-map.yaml:L23814-L23823] |
| 259 | `test_abs_offshore_2018_subtropical_shallow_submerged` | [docs/registry/implementation-map.yaml:L23824-L23833] |
| 260 | `_make_abs_ship_cfg` | [docs/registry/implementation-map.yaml:L23834-L23843] |
| 261 | `test_abs_ship_variant_anode_mass_formula` | [docs/registry/implementation-map.yaml:L23844-L23855] |
| 262 | `_base_cfg` | [docs/registry/implementation-map.yaml:L23856-L23865] |
| 263 | `test_abs_zero_coating_breakdown` | [docs/registry/implementation-map.yaml:L23866-L23877] |
| 264 | `test_abs_short_flush_anode_geometry` | [docs/registry/implementation-map.yaml:L23878-L23887] |
| 265 | `test_abs_slender_offset_anode_geometry` | [docs/registry/implementation-map.yaml:L23888-L23897] |
| 266 | `b401_base_config` | [docs/registry/implementation-map.yaml:L23898-L23907] |
| 267 | `_pipeline` | [docs/registry/implementation-map.yaml:L23908-L23917] |
| 268 | `TestB401AnodeResistance.test_anode_resistance_dwight_formula` | [docs/registry/implementation-map.yaml:L23918-L23927] |
| 269 | `run_cathodic_protection` | [docs/registry/implementation-map.yaml:L23928-L23937] |
| 270 | `_minimal_dnv_cfg` | [docs/registry/implementation-map.yaml:L23938-L23947] |
| 271 | `test_dnv_f103_2010_full_route_buried_cold` | [docs/registry/implementation-map.yaml:L23948-L23959] |
| 272 | `_minimal_dnv_cfg` | [docs/registry/implementation-map.yaml:L23960-L23969] |
| 273 | `test_dnv_pipeline_variant_router_24in_buried_FBE` | [docs/registry/implementation-map.yaml:L23970-L23979] |
| 274 | `_dnv_cfg` | [docs/registry/implementation-map.yaml:L23980-L23989] |
| 275 | `test_dnv_short_pipeline_1km` | [docs/registry/implementation-map.yaml:L23990-L24001] |
| 276 | `test_dnv_zinc_anode_material` | [docs/registry/implementation-map.yaml:L24002-L24012] |
| 277 | `test_dnv_bracelet_vs_elongated_utilization` | [docs/registry/implementation-map.yaml:L24013-L24023] |
| 278 | `TestNetAnodeMass.test_net_mass_aluminium_typical` | [docs/registry/implementation-map.yaml:L24024-L24033] |
| 279 | `TestNetAnodeMass.test_net_mass_aluminium_2000_capacity` | [docs/registry/implementation-map.yaml:L24034-L24043] |
| 280 | `TestAnodeResistanceFlush.test_flush_resistance_known_values` | [docs/registry/implementation-map.yaml:L24044-L24053] |
| 281 | `TestAnodeResistanceBracelet.test_bracelet_resistance_known_values` | [docs/registry/implementation-map.yaml:L24054-L24063] |
| 282 | `TestSacrificialAnodeWorkflowIntegration.test_integration_net_mass` | [docs/registry/implementation-map.yaml:L24064-L24073] |
| 283 | `TestSacrificialAnodeWorkflowIntegration.test_integration_bracelet_resistance` | [docs/registry/implementation-map.yaml:L24074-L24083] |

### Counted-family grep and additional definitions

The case-sensitive Python `re` patterns are used exactly as stored in `search_signals`; a pattern matching `infrastructure/base_solvers/` case-insensitively is omitted before searching package Python files. Hits are assigned to the innermost function/class, with module-level statements grouped once. The patterns and mapped rows are available in the family records above. [docs/registry/implementation-map.yaml:L1-L20]

| Counted family | Hit definitions | Already mapped | Additional definitions inventoried below |
| --- | --- | --- | --- |
| effective-tension-riser-stack-up | 42 | 37 | 5 |
| orcaflex-text-model-writers | 73 | 29 | 44 |
| cathodic-protection-calculations | 55 | 27 | 28 |

Additional grep matches are inventoried explicitly; wrappers, serialization, data declarations and docstrings are not treated as independently computing the family result. Formula/algorithm definitions below are map follow-ups, issue pending (5).

| Family | Additional definition | Classification / evidence |
| --- | --- | --- |
| effective-tension-riser-stack-up | `<module>` | data/export helper; [src/digitalmodel/infrastructure/base_solvers/marine/__init__.py:L1-L12] |
| effective-tension-riser-stack-up | `PipeProperties.get_internal_fluid_properties` | pipe physical-property calculation; [src/digitalmodel/infrastructure/base_solvers/marine/pipe_properties.py:L290-L306] |
| effective-tension-riser-stack-up | `PipeProperties.get_buoyancy_properties` | pipe physical-property calculation; [src/digitalmodel/infrastructure/base_solvers/marine/pipe_properties.py:L308-L318] |
| effective-tension-riser-stack-up | `Shear7ModelComponents.add_block_2` | data/export helper; [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L134-L150] |
| effective-tension-riser-stack-up | `Shear7ModelComponents.add_block_5` | data/export helper; [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L191-L225] |
| orcaflex-text-model-writers | `BenchmarkAggregator.export_to_json` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L429-L447] |
| orcaflex-text-model-writers | `BenchmarkAggregator.export_to_csv` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L449-L490] |
| orcaflex-text-model-writers | `ConfigurationBenchmarks._create_test_configs` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/benchmarks/configuration_benchmarks.py:L106-L159] |
| orcaflex-text-model-writers | `ConfigurationBenchmarks.benchmark_yaml_loading.load_yaml` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/benchmarks/configuration_benchmarks.py:L186-L188] |
| orcaflex-text-model-writers | `BenchmarkReportGenerator.generate_report` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L41-L72] |
| orcaflex-text-model-writers | `BenchmarkReportGenerator._generate_basic_html_report` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L500-L567] |
| orcaflex-text-model-writers | `SolverConfigManager.save_config` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L201-L225] |
| orcaflex-text-model-writers | `SolverConfigManager.load_config` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L227-L250] |
| orcaflex-text-model-writers | `<module>` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L1-L261] |
| orcaflex-text-model-writers | `FatigueAnalysis.router` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L23-L50] |
| orcaflex-text-model-writers | `FatigueAnalysisComponents.get_wave_fatigue_result` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L297-L299] |
| orcaflex-text-model-writers | `FatigueAnalysisComponents.read_orcaflex_wave_fatigue` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L302-L306] |
| orcaflex-text-model-writers | `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L308-L314] |
| orcaflex-text-model-writers | `<module>` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L1-L566] |
| orcaflex-text-model-writers | `DNVRPH103_hydrodynamics_circular.get_orcaflex_6dbuoy` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L20-L25] |
| orcaflex-text-model-writers | `<module>` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L1-L546] |
| orcaflex-text-model-writers | `DNVRPH103_hydrodynamics_rectangular.get_orcaflex_6dbuoy` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L18-L23] |
| orcaflex-text-model-writers | `OrcaflexModelComponents.wave` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L207-L232] |
| orcaflex-text-model-writers | `OrcaflexModelComponents.current` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L234-L269] |
| orcaflex-text-model-writers | `OrcaflexModelComponents.wind` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L271-L283] |
| orcaflex-text-model-writers | `OrcaflexModelComponents.RayleighDampingCoefficients` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L307-L377] |
| orcaflex-text-model-writers | `<module>` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/marine/pipe_properties.py:L1-L318] |
| orcaflex-text-model-writers | `PipeProperties.get_material_properties` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/marine/pipe_properties.py:L185-L212] |
| orcaflex-text-model-writers | `<module>` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L1-L316] |
| orcaflex-text-model-writers | `FEAComponents.prepare_FEAModel` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L41-L49] |
| orcaflex-text-model-writers | `FEAComponents.AssignEnvironment` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L56-L59] |
| orcaflex-text-model-writers | `FEAComponents.GetBuoyProperties` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L142-L147] |
| orcaflex-text-model-writers | `FEAComponents.GetLineTypeProperties` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L149-L154] |
| orcaflex-text-model-writers | `FEAComponents.GetLineProperties` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L156-L161] |
| orcaflex-text-model-writers | `FEAComponents.GetVesselTypeProperties` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L163-L168] |
| orcaflex-text-model-writers | `FEAComponents.GetVesselProperties` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L170-L175] |
| orcaflex-text-model-writers | `FEAComponents.GetConstraintProperties` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L177-L182] |
| orcaflex-text-model-writers | `FEAComponents.GetVariableData` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L184-L189] |
| orcaflex-text-model-writers | `FEAComponents.GetShapeProperties` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L191-L196] |
| orcaflex-text-model-writers | `FEAComponents.GetGroupProperties` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L198-L203] |
| orcaflex-text-model-writers | `FEAComponents.get_fe_filename` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L253-L286] |
| orcaflex-text-model-writers | `FEAComponents.overwrite_fe_files` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L288-L295] |
| orcaflex-text-model-writers | `<module>` | SHEAR7 serialization, outside this family; [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L1-L238] |
| orcaflex-text-model-writers | `Shear7ModelComponents.save_model` | SHEAR7 serialization, outside this family; [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L230-L238] |
| orcaflex-text-model-writers | `VIVAnalysisComponents.readFirstLine` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L76-L80] |
| orcaflex-text-model-writers | `<module>` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L1-L1164] |
| orcaflex-text-model-writers | `VIVFatigueAnalysisComponents.get_wave_fatigue_result` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L524-L526] |
| orcaflex-text-model-writers | `VIVFatigueAnalysisComponents.read_orcaflex_wave_fatigue` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L528-L531] |
| orcaflex-text-model-writers | `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | delegation, generic serialization, model-component data or documentation; [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L533-L542] |
| cathodic-protection-calculations | `<module>` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/__init__.py:L1-L26] |
| cathodic-protection-calculations | `<module>` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1-L1852] |
| cathodic-protection-calculations | `CathodicProtection` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L13-L1852] |
| cathodic-protection-calculations | `CathodicProtection.router` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L17-L41] |
| cathodic-protection-calculations | `CathodicProtection.DNV_RP_B401_offshore_platform` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L749-L763] |
| cathodic-protection-calculations | `CathodicProtection._abs_offshore_2018_coating_breakdown` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L951-L1003] |
| cathodic-protection-calculations | `CathodicProtection._abs_current_demand` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1078-L1117] |
| cathodic-protection-calculations | `CathodicProtection.anode_initial_check` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1693-L1757] |
| cathodic-protection-calculations | `CathodicProtection.anode_final_check` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1759-L1852] |
| cathodic-protection-calculations | `<module>` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L1-L746] |
| cathodic-protection-calculations | `_require_valid_material` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L141-L149] |
| cathodic-protection-calculations | `_require_valid_anode_type` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L152-L160] |
| cathodic-protection-calculations | `_require_valid_coating` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L163-L171] |
| cathodic-protection-calculations | `_b401_edition` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L174-L183] |
| cathodic-protection-calculations | `_b401_current_demand` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L400-L471] |
| cathodic-protection-calculations | `G42TestResult` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g42.py:L40-L51] |
| cathodic-protection-calculations | `evaluate_g42` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g42.py:L105-L175] |
| cathodic-protection-calculations | `<module>` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g80.py:L1-L141] |
| cathodic-protection-calculations | `G80TestResult` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g80.py:L29-L37] |
| cathodic-protection-calculations | `evaluate_g80` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g80.py:L104-L141] |
| cathodic-protection-calculations | `<module>` | delegation, validation, data or coating-test evaluation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L1-L253] |
| cathodic-protection-calculations | `net_anode_mass` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L48-L76] |
| cathodic-protection-calculations | `gross_anode_mass` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L79-L99] |
| cathodic-protection-calculations | `anode_count` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L102-L121] |
| cathodic-protection-calculations | `anode_resistance_flush` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L124-L159] |
| cathodic-protection-calculations | `anode_resistance_bracelet` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L162-L183] |
| cathodic-protection-calculations | `driving_voltage` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L186-L222] |
| cathodic-protection-calculations | `anode_current_output` | independent CP calculation; [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L225-L253] |

A8 result: PASS for inventory and counted-family coverage against the criterion of at most 1 omitted computing definition over the three counted families; 0 further computing definitions remain unlisted after the additional inventory above. Map completeness remains separate from pack coverage, issue pending (5).

Families covered by the pack but absent as in-package map families include plate buckling, Euler buckling, von Mises stress, beam stiffness, well-path interpolation and pipeline root-finding; these require map follow-up before they become counted families, issue pending (5). [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L33-L130] [tests/infrastructure/unit/test_structural_solvers_week2.py:L243-L247] [tests/infrastructure/unit/test_structural_solvers_week2.py:L418-L440] [tests/unit/structural/test_structural_analysis.py:L225-L239] [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1-L20] [src/digitalmodel/infrastructure/base_solvers/pipeline_solvers/math_solvers.py:L1-L20]

### Solver-name dependency sweep

Every package occurrence of `OrcaFlex`/`orcaflex` or `SHEAR7`/`shear7` is indexed below by source line; these are text-only dependencies, not program-load or solver-version validation.

| Source | Matching lines / evidence |
| --- | --- |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L34-L34] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L38-L38] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L19-L19] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L20-L20] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L45-L45] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L47-L47] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L78-L78] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L86-L86] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L87-L87] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L96-L96] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L106-L106] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L112-L112] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L128-L128] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L129-L129] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L130-L130] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L131-L131] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L251-L251] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L257-L257] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L258-L258] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L262-L262] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L286-L286] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L287-L287] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L289-L289] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L298-L298] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L299-L299] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L302-L302] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L308-L308] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L334-L334] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L335-L335] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L336-L336] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L337-L337] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L340-L340] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L343-L343] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L346-L346] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L379-L379] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L380-L380] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L381-L381] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L382-L382] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L392-L392] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L393-L393] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L406-L406] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L412-L412] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L414-L414] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L422-L422] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L423-L423] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L429-L429] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L437-L437] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L438-L438] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L440-L440] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L546-L546] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L551-L551] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L554-L554] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L558-L558] [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L586-L586] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L8-L8] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L9-L9] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L13-L13] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L20-L20] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L6-L6] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L7-L7] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L11-L11] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L18-L18] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L6-L6] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L11-L11] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L791-L791] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L802-L802] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L815-L815] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L817-L817] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L820-L820] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L831-L831] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L833-L833] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L836-L836] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L842-L842] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L844-L844] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L851-L851] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L43-L43] [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L95-L95] [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L99-L99] [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L103-L103] |
| `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L42-L42] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L53-L53] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L58-L58] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L145-L145] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L152-L152] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L159-L159] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L166-L166] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L173-L173] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L180-L180] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L187-L187] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L194-L194] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L201-L201] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L209-L209] [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L249-L249] |
| `src/digitalmodel/infrastructure/base_solvers/viv/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/viv/__init__.py:L8-L8] [src/digitalmodel/infrastructure/base_solvers/viv/__init__.py:L13-L13] |
| `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L4-L4] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L6-L6] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L8-L8] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L12-L12] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L32-L32] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L35-L35] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L38-L38] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L42-L42] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L46-L46] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L47-L47] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L48-L48] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L55-L55] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L56-L56] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L58-L58] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L73-L73] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L76-L76] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L135-L135] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L136-L136] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L137-L137] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L140-L140] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L141-L141] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L142-L142] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L143-L143] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L144-L144] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L145-L145] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L150-L150] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L153-L153] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L154-L154] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L157-L157] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L158-L158] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L162-L162] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L165-L165] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L168-L168] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L169-L169] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L172-L172] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L175-L175] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L178-L178] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L181-L181] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L184-L184] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L187-L187] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L192-L192] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L193-L193] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L196-L196] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L199-L199] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L202-L202] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L205-L205] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L208-L208] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L211-L211] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L214-L214] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L217-L217] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L220-L220] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L223-L223] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L228-L228] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L235-L235] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L237-L237] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L32-L32] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L33-L33] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L63-L63] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L66-L66] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L67-L67] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L144-L144] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L147-L147] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L148-L148] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L173-L173] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L183-L183] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L196-L196] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L222-L222] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L224-L224] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L226-L226] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L227-L227] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L454-L454] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L458-L458] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L460-L460] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L467-L467] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L505-L505] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L507-L507] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L513-L513] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L525-L525] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L526-L526] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L528-L528] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L533-L533] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L569-L569] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L571-L571] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L572-L572] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L573-L573] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L577-L577] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L582-L582] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L586-L586] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L590-L590] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L632-L632] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L634-L634] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L635-L635] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L636-L636] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L639-L639] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L651-L651] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L652-L652] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L676-L676] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L677-L677] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L692-L692] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L693-L693] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L695-L695] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L703-L703] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L704-L704] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L705-L705] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L712-L712] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L719-L719] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L723-L723] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L724-L724] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L729-L729] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L921-L921] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L922-L922] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L936-L936] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L942-L942] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L947-L947] [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L982-L982] |

Upstream fatigue rainflow counting comes from the time-series analysis component; marine exports serialize YAML, and the durable workflow registry identifies the two primary offline calculation routes. [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L249-L261] [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L791-L857] [docs/registry/workflows.yaml:L28-L35] [docs/registry/workflows.yaml:L468-L475]

A repository-wide Python search for `PlateBuckling|DNV_rp_C201|damage_from_sn_data|cycles_to_failure` returns 138 matching lines in 43 files, including 1 script file. The calculation library implements separate plate formulas; the study script delegates to structural plate/panel analyzers. These external paths are map follow-ups, issue pending (5), not counted-family package misses. [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L91-L120] [scripts/studies/ship_plate_buckling.py:L15-L28] [scripts/studies/ship_plate_buckling.py:L61-L76]

## 10. Coverage and open items

| Open item | Responsibility and disposition | Evidence |
| --- | --- | --- |
| Stress units, sign/frame conventions and standards provenance remain unverified | Maintainers and calculations quality role; issue pending (4) | Section 2 and section 3 limitations |
| Independent end-to-end plate/default-curve qualification remains unverified | Maintainers and calculations quality role; issue pending (6); ship fixtures: #785 | [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L19-L25] |
| Silent-failure candidates and unchecked physical potential | Maintainers; issue pending (1), issue pending (2), issue pending (3) | Every section-6 candidate retains its own source range |
| Method-map follow-up, including additional CP helpers and absent structural/math families | Maintainers; issue pending (5) | Section-9 map and grep inventories |
| Remaining package input/output contracts, full source qualification, solver versions and generated QA appendix | Maintainers and calculations quality role; issue pending (6) | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L791-L857] [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L230-L238] |
| Deputy/roster, shared-log entry and any future private sidecar entry | Maintainers; issue pending (6); sidecar entry: to be created only if a future private lesson requires one | Sections 7 and 8 identify the unverified administrative coverage |
| Rectangular buoy inertia origin convention | Maintainers; #2135 | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L401-L422] |
| Test execution limitations and shared-harness drift | Maintainers; #1277 and #2312; stale benchmark API: #949 | Section-5 execution record; [tests/test_engine.py:L34-L81] [tests/test_solver_benchmarks.py:L11-L18] |

### Package source files read

All package Python files are listed below, including initializer-only subpackages; no existing child-pack exclusion is applied.

| Source | Read evidence at pin |
| --- | --- |
| `src/digitalmodel/infrastructure/base_solvers/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/base.py` | [src/digitalmodel/infrastructure/base_solvers/base.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/benchmarks/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/benchmarks/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py` | [src/digitalmodel/infrastructure/base_solvers/benchmarks/benchmark_suite.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/benchmarks/configuration_benchmarks.py` | [src/digitalmodel/infrastructure/base_solvers/benchmarks/configuration_benchmarks.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py` | [src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/benchmarks/solver_benchmarks.py` | [src/digitalmodel/infrastructure/base_solvers/benchmarks/solver_benchmarks.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/config/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/config/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/config/solver_config.py` | [src/digitalmodel/infrastructure/base_solvers/config/solver_config.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/fatigue/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | [src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py` | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py` | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g42.py` | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g42.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g80.py` | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_astm_g80.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py` | [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_sacrificial_anode_b401.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/interfaces.py` | [src/digitalmodel/infrastructure/base_solvers/interfaces.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/marine/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/marine/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | [src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/marine/pipe_properties.py` | [src/digitalmodel/infrastructure/base_solvers/marine/pipe_properties.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/marine/ship/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/marine/ship/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_design.py` | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_design.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | [src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/pipeline_solvers/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/pipeline_solvers/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/pipeline_solvers/math_solvers.py` | [src/digitalmodel/infrastructure/base_solvers/pipeline_solvers/math_solvers.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/signal/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/signal/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/specialized/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/specialized/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/structural/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/base.py` | [src/digitalmodel/infrastructure/base_solvers/structural/base.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/buckling/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py` | [src/digitalmodel/infrastructure/base_solvers/structural/buckling/elastic_buckling.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/config.py` | [src/digitalmodel/infrastructure/base_solvers/structural/config.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/elements/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/structural/elements/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/elements/beam_element.py` | [src/digitalmodel/infrastructure/base_solvers/structural/elements/beam_element.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | [src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/fea/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/structural/fea/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py` | [src/digitalmodel/infrastructure/base_solvers/structural/plate_buckling.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/stress/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/structural/stress/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py` | [src/digitalmodel/infrastructure/base_solvers/structural/stress/von_mises.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/utils/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/structural/utils/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/utils/matrix_operations.py` | [src/digitalmodel/infrastructure/base_solvers/structural/utils/matrix_operations.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py` | [src/digitalmodel/infrastructure/base_solvers/structural/utils/post_processing.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/utils/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/utils/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/viv/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/viv/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py` | [src/digitalmodel/infrastructure/base_solvers/viv/viv_analysis_components.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | [src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/well/__init__.py` | [src/digitalmodel/infrastructure/base_solvers/well/__init__.py:L1-L5] |
| `src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py` | [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L1-L5] |

### Documentation discovery

The documentation search reads text under `docs/` for `base_solvers|base.solvers|fatigue_analysis|plate_buckling`; the neutral source locators below record matching files rather than treating README maturity or validation claims as established results.

| Documentation locator | Matching evidence |
| --- | --- |
| `docs/capability-map/capabilities-added.yml` | [docs/capability-map/capabilities-added.yml:L62-L62] |
| `docs/capability-map/capabilities-sections.yml` | [docs/capability-map/capabilities-sections.yml:L52-L52] |
| `docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md` | [docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md:L10-L10] |
| `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md` | [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L21-L21] |
| `docs/plans/2026-09-25-issue-2210-cp-engine-adapter-demo-retirement.md` | [docs/plans/2026-09-25-issue-2210-cp-engine-adapter-demo-retirement.md:L21-L21] |
| `docs/domains/girder-web-frame-validation-2026-06-28.md` | [docs/domains/girder-web-frame-validation-2026-06-28.md:L41-L41] |
| `docs/domains/grillage-buckling-validation-2026-06-28.md` | [docs/domains/grillage-buckling-validation-2026-06-28.md:L167-L167] |
| `docs/domains/corrugated-bulkhead-validation-2026-06-28.md` | [docs/domains/corrugated-bulkhead-validation-2026-06-28.md:L96-L96] |
| `docs/registry/implementation-map.md` | [docs/registry/implementation-map.md:L17-L17] |
| `docs/registry/workflows.yaml` | [docs/registry/workflows.yaml:L29-L29] |
| `docs/registry/implementation-map.yaml` | [docs/registry/implementation-map.yaml:L686-L686] |
| `docs/migration/legacy-engineering-flow-catalog.md` | [docs/migration/legacy-engineering-flow-catalog.md:L30-L30] |
| `docs/plans/evidence/issue-2082-ci-discovery.md` | [docs/plans/evidence/issue-2082-ci-discovery.md:L10-L10] |
| `docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md` | [docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md:L3-L3] |
| `docs/domains/cathodic_protection/_index.md` | [docs/domains/cathodic_protection/_index.md:L48-L48] |
| `docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md` | [docs/domains/viv/VIV_ANALYSIS_IMPLEMENTATION.md:L490-L490] |
| `docs/domains/infrastructure/cache_layer_implementation.md` | [docs/domains/infrastructure/cache_layer_implementation.md:L219-L219] |
| `docs/domains/mooring/MOORING_ANALYSIS_IMPLEMENTATION.md` | [docs/domains/mooring/MOORING_ANALYSIS_IMPLEMENTATION.md:L245-L245] |
| `docs/domains/aqwa/AQWA_TESTING_SUMMARY.md` | [docs/domains/aqwa/AQWA_TESTING_SUMMARY.md:L243-L243] |
| `docs/domains/test_automation/fatigue_analysis_mock_patterns.md` | [docs/domains/test_automation/fatigue_analysis_mock_patterns.md:L27-L27] |
| `docs/domains/test_automation/README.md` | [docs/domains/test_automation/README.md:L87-L87] |
| `docs/domains/structural/STRUCTURAL_ANALYSIS_IMPLEMENTATION.md` | [docs/domains/structural/STRUCTURAL_ANALYSIS_IMPLEMENTATION.md:L14-L14] |
| `docs/domains/structural/plate_capacity_migration.md` | [docs/domains/structural/plate_capacity_migration.md:L31-L31] |
| `docs/domains/structural/PHASE_2_3_STRUCTURAL_SOLVERS_IMPLEMENTATION.md` | [docs/domains/structural/PHASE_2_3_STRUCTURAL_SOLVERS_IMPLEMENTATION.md:L6-L6] |
| `docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md` | [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L211-L211] |
| `docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md` | [docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md:L157-L157] |
| `docs/domains/fatigue/fatigue_migration_summary.md` | [docs/domains/fatigue/fatigue_migration_summary.md:L220-L220] |
| `docs/domains/core/PROVENANCE_SYSTEM.md` | [docs/domains/core/PROVENANCE_SYSTEM.md:L542-L542] |
| `docs/domains/plate-buckling/ship-structure-study.md` | [docs/domains/plate-buckling/ship-structure-study.md:L9-L9] |
| `docs/domains/plate-buckling/buckling-code-review-2026-06-26.md` | [docs/domains/plate-buckling/buckling-code-review-2026-06-26.md:L17-L17] |
| `docs/domains/project-docs/PHASE_2_2_BASE_SOLVERS_IMPLEMENTATION.md` | [docs/domains/project-docs/PHASE_2_2_BASE_SOLVERS_IMPLEMENTATION.md:L9-L9] |
| `docs/domains/project-docs/MODULE_IMPLEMENTATION_RECOMMENDATIONS.md` | [docs/domains/project-docs/MODULE_IMPLEMENTATION_RECOMMENDATIONS.md:L30-L30] |
| `docs/domains/project-docs/MODULE_PORTFOLIO_2026.md` | [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L49-L49] |
| `docs/domains/project-docs/phase1-implementation-report.md` | [docs/domains/project-docs/phase1-implementation-report.md:L835-L835] |
| `docs/domains/project-docs/MODULE_SURVEY.md` | [docs/domains/project-docs/MODULE_SURVEY.md:L18-L18] |
| `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md` | [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md:L31-L31] |
| `docs/domains/project-docs/NAMING_CONVENTION_UPDATE_SUMMARY.md` | [docs/domains/project-docs/NAMING_CONVENTION_UPDATE_SUMMARY.md:L8-L8] |
| `docs/domains/project-docs/PHASE_3_IMPLEMENTATION_GUIDE.md` | [docs/domains/project-docs/PHASE_3_IMPLEMENTATION_GUIDE.md:L447-L447] |
| `docs/domains/project-docs/PHASE_3_SUMMARY.md` | [docs/domains/project-docs/PHASE_3_SUMMARY.md:L257-L257] |
| `docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md` | [docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md:L81-L81] |
| `docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md` | [docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md:L34-L34] |
| `docs/domains/project-docs/test_performance_dashboard.html` | [docs/domains/project-docs/test_performance_dashboard.html:L344-L344] |
| `docs/domains/project-docs/TEMPLATE_GUIDE.md` | [docs/domains/project-docs/TEMPLATE_GUIDE.md:L43-L43] |
| `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md` | [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md:L43-L43] |
| `docs/domains/project-docs/PHASE_3_TEST_ARCHITECTURE.md` | [docs/domains/project-docs/PHASE_3_TEST_ARCHITECTURE.md:L90-L90] |
| `docs/domains/project-docs/TEST_BASELINE_SUMMARY.md` | [docs/domains/project-docs/TEST_BASELINE_SUMMARY.md:L71-L71] |
| `docs/domains/testing/engine_test_implementation_plan.md` | [docs/domains/testing/engine_test_implementation_plan.md:L377-L377] |
| `docs/domains/testing/CLI_TESTING_NEXT_STEPS.md` | [docs/domains/testing/CLI_TESTING_NEXT_STEPS.md:L31-L31] |
| `docs/domains/testing/CLI_TESTING_FRAMEWORK_COMPLETE.md` | [docs/domains/testing/CLI_TESTING_FRAMEWORK_COMPLETE.md:L558-L558] |
| `docs/domains/migration/orcaflex_migration_guide.md` | [docs/domains/migration/orcaflex_migration_guide.md:L46-L46] |
| `docs/domains/migration/signal_analysis_migration_plan.md` | [docs/domains/migration/signal_analysis_migration_plan.md:L28-L28] |
| `docs/domains/cathodic_protection/examples/example-03-platform-dnv-b401-offshore.md` | [docs/domains/cathodic_protection/examples/example-03-platform-dnv-b401-offshore.md:L133-L133] |
| `docs/domains/cathodic_protection/examples/example-03-platform-abs-offshore-2018.md` | [docs/domains/cathodic_protection/examples/example-03-platform-abs-offshore-2018.md:L130-L130] |
| `docs/domains/cathodic_protection/examples/example-01-pipeline-dnv-f103-2010.md` | [docs/domains/cathodic_protection/examples/example-01-pipeline-dnv-f103-2010.md:L205-L205] |
| `docs/domains/cathodic_protection/examples/example-02-ship-hull-abs-gn-ships-2017.md` | [docs/domains/cathodic_protection/examples/example-02-ship-hull-abs-gn-ships-2017.md:L187-L187] |
| `docs/domains/infrastructure/base_solvers/README.md` | [docs/domains/infrastructure/base_solvers/README.md:L1-L1] |
| `docs/domains/structural/fatigue_analysis/README.md` | [docs/domains/structural/fatigue_analysis/README.md:L1-L1] |
| `docs/api/buckling/ship-plate-buckling.html` | [docs/api/buckling/ship-plate-buckling.html:L261-L261] |

9 discovery files with identifying names are omitted from the displayed locators; neutral discovery aliases and source-rights review are issue pending (6).

The calculation-output citation contract was readable as a governing reference; F103/B401 citation output is inspected, while C201/C203 qualification remains open in section 3. [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py:L131-L150] [src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_B401_2021.py:L711-L746]

### Tests discovered

| Group | Exact selected test path |
| --- | --- |
| Primary | `tests/infrastructure/unit/test_base_solvers_base.py`; [tests/infrastructure/unit/test_base_solvers_base.py:L1-L5] |
| Primary | `tests/infrastructure/unit/test_solver_config.py`; [tests/infrastructure/unit/test_solver_config.py:L1-L5] |
| Primary | `tests/infrastructure/unit/test_structural_solvers.py`; [tests/infrastructure/unit/test_structural_solvers.py:L1-L5] |
| Primary | `tests/infrastructure/unit/test_structural_solvers_week2.py`; [tests/infrastructure/unit/test_structural_solvers_week2.py:L1-L5] |
| Primary | `tests/solvers/test_base_solvers.py`; [tests/solvers/test_base_solvers.py:L1-L5] |
| Primary | `tests/test_solver_benchmarks.py`; [tests/test_solver_benchmarks.py:L1-L5] |
| Primary | `tests/unit/structural/test_structural_analysis.py`; [tests/unit/structural/test_structural_analysis.py:L1-L5] |
| Primary | `tests/specialized/cathodic_protection/test_astm_g42.py`; [tests/specialized/cathodic_protection/test_astm_g42.py:L1-L5] |
| Primary | `tests/specialized/cathodic_protection/test_astm_g80.py`; [tests/specialized/cathodic_protection/test_astm_g80.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_abs_cathodic_protection_calcs.py`; [tests/specialized/cathodic_protection/test_abs_cathodic_protection_calcs.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_abs_offshore_2018.py`; [tests/specialized/cathodic_protection/test_abs_offshore_2018.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_abs_ship_variants.py`; [tests/specialized/cathodic_protection/test_abs_ship_variants.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_abs_ship_variants_wrk271.py`; [tests/specialized/cathodic_protection/test_abs_ship_variants_wrk271.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_astm_g42.py`; [tests/specialized/cathodic_protection/test_astm_g42.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_astm_g80.py`; [tests/specialized/cathodic_protection/test_astm_g80.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_cathodic_protection_b401.py`; [tests/specialized/cathodic_protection/test_cathodic_protection_b401.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_cathodic_protection_basic.py`; [tests/specialized/cathodic_protection/test_cathodic_protection_basic.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_dnv_f103_2010_calcs.py`; [tests/specialized/cathodic_protection/test_dnv_f103_2010_calcs.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_dnv_pipeline_variants.py`; [tests/specialized/cathodic_protection/test_dnv_pipeline_variants.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_dnv_pipeline_variants_wrk271.py`; [tests/specialized/cathodic_protection/test_dnv_pipeline_variants_wrk271.py:L1-L5] |
| CP / fatigue (entire directory) | `tests/specialized/cathodic_protection/test_sacrificial_anode_b401.py`; [tests/specialized/cathodic_protection/test_sacrificial_anode_b401.py:L1-L5] |
| CP / fatigue | `tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py`; [tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py:L1-L5] |
| CP / fatigue | `tests/specialized/ship_design/test_ship_fatigue_analysis_import.py`; [tests/specialized/ship_design/test_ship_fatigue_analysis_import.py:L1-L5] |
| CP / fatigue | `tests/cathodic_protection/test_abs_ships_rebuild.py`; [tests/cathodic_protection/test_abs_ships_rebuild.py:L1-L5] |
| CP / fatigue | `tests/cathodic_protection/test_edition_divergence_baseline.py`; [tests/cathodic_protection/test_edition_divergence_baseline.py:L1-L5] |
| CP / fatigue | `tests/benchmarks/test_cp_benchmarks.py`; [tests/benchmarks/test_cp_benchmarks.py:L1-L5] |
| Shared consumers | `tests/additional_engine_tests.py`; [tests/additional_engine_tests.py:L1-L5] |
| Shared consumers | `tests/simple_engine_test.py`; [tests/simple_engine_test.py:L1-L5] |
| Shared consumers | `tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py`; [tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py:L1-L5] |
| Shared consumers | `tests/structural/fatigue_analysis/test_fatigue_analysis_timetrace.py`; [tests/structural/fatigue_analysis/test_fatigue_analysis_timetrace.py:L1-L5] |
| Separate shared harnesses | `tests/test_engine.py`; [tests/test_engine.py:L1-L5] |
| Separate shared harnesses | `tests/test_engine_isolated.py`; [tests/test_engine_isolated.py:L1-L5] |
| Separate shared harnesses | `tests/test_fatigue_basic.py`; [tests/test_fatigue_basic.py:L1-L5] |
| Separate shared harnesses | `tests/reporting/test_cp_portfolio_provenance.py`; [tests/reporting/test_cp_portfolio_provenance.py:L1-L5] |
| Isolated snippets | `tests/cathodic_protection/test_worked_examples.py`; [tests/cathodic_protection/test_worked_examples.py:L1-L5] |

The two workflow node arguments are the complete node IDs in section 4; their fixture inputs and generated outputs are isolated outside the worktree. [tests/workflows/test_durable_workflows.py:L54-L95]

`tests/contracts` and `tests/docs/test_digitalmodel_routing_contract.py` are the additional contract/routing selection; the latter checks domain coverage and existing registry fields. [tests/docs/test_digitalmodel_routing_contract.py:L97-L119]
