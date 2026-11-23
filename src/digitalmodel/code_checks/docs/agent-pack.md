# Code checks knowledge pack

- Module: `code_checks`
- Repository: `digitalmodel`
- Package: `src/digitalmodel/code_checks`
- Pack revision: 0
- Code revision: `c099a5e1317e6bed63d876252d87cbb12c774372`
- Analysis family: engineering calculations (`EC`)
- Family quality manager: engineering calculations quality manager
- Status: `DRAFT`
- Evidence date: 2026-10-18
- Implementation-map revision: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; map data revision: `270c8759c374b67a7f032953e7fcdb14e0224461`.

## 1. Purpose and scope

The package provides standalone pipeline local-buckling, external-collapse and propagation checks, plus a router for offline riser and mooring load-array checks; its registered maturity is `development`. [src/digitalmodel/code_checks/dnv_os_f101.py:L1-L24] [src/digitalmodel/code_checks/workflow.py:L26-L55] [docs/registry/module-routing.yaml:L481-L486]

The router delegates API STD 2RD, DNV-OS-F201 and API RP 2SK calculations to `digitalmodel.orcaflex.code_check_engine`; that engine explicitly operates on load arrays without the licensed interface. [src/digitalmodel/code_checks/workflow.py:L12-L20] [src/digitalmodel/code_checks/workflow.py:L58-L127] [src/digitalmodel/orcaflex/code_check_engine.py:L1-L18]

Applicability is screening of caller-supplied loads: the local-buckling function uses the supplied pressure capacity for either net-pressure regime, and the caller supplies factored design moment and tension. [src/digitalmodel/code_checks/dnv_os_f101.py:L91-L147]

UNVERIFIED: suitability for a particular geometry envelope, water depth, operational load combination or production design requires an independently checked design basis; issue pending (3).

## 2. Inputs and outputs

Table 1. Router inputs; defaults are implementation defaults, not design recommendations.

| Input | Unit and range | Default and source | Evidence |
|---|---|---|---|
| `code_check.code`, `inputs` | String selector; mapping | Both required; aliases normalized to three enums | [src/digitalmodel/code_checks/workflow.py:L130-L156] |
| Pipe `outer_diameter`, `wall_thickness` | m, individually positive; no coupled wall/diameter guard | Model defaults when omitted | [src/digitalmodel/orcaflex/code_check_engine.py:L59-L75] [src/digitalmodel/orcaflex/code_check_engine.py:L163-L181] |
| `smys`, `smts` | Pa, positive | Model defaults when omitted | [src/digitalmodel/orcaflex/code_check_engine.py:L65-L70] [src/digitalmodel/orcaflex/code_check_engine.py:L168-L175] |
| API design and temperature factors; DNV safety class and resistance/fabrication factors | Dimensionless; model field bounds apply | Defaults at the cited definitions; explicit selection requires a checked design basis | [src/digitalmodel/orcaflex/code_check_engine.py:L65-L70] [src/digitalmodel/orcaflex/code_check_engine.py:L168-L175] |
| `arc_lengths` / `arc_length_m` | m; nonempty numeric array; order and sign unchecked | Required | [src/digitalmodel/code_checks/workflow.py:L58-L79] [src/digitalmodel/code_checks/workflow.py:L164-L171] [src/digitalmodel/orcaflex/code_check_engine.py:L93-L101] |
| Effective tension and bending arrays | kN and kN.m; length equals arc array or one value broadcast | Required; all accepted aliases occur in the cited dispatchers | [src/digitalmodel/code_checks/workflow.py:L58-L110] [src/digitalmodel/code_checks/workflow.py:L197-L215] |
| API pressure difference; DNV external/internal pressure arrays | MPa; optional, scalar broadcast allowed | `None`; API substitutes zero pressure, DNV does not consume the arguments in its calculation | [src/digitalmodel/code_checks/workflow.py:L73-L110] [src/digitalmodel/code_checks/workflow.py:L186-L205] [src/digitalmodel/orcaflex/code_check_engine.py:L103-L155] [src/digitalmodel/orcaflex/code_check_engine.py:L219-L269] |
| Mooring `mbl_kN`, `max_tension_kN`, `condition`, `line_name` | kN; breaking load positive; maximum tension has no sign guard | Breaking load/condition use model defaults; maximum tension required; identifier defaults to `Line-1` | [src/digitalmodel/code_checks/workflow.py:L113-L127] [src/digitalmodel/orcaflex/code_check_engine.py:L275-L282] [src/digitalmodel/orcaflex/code_check_engine.py:L316-L349] |
| `output_dir`, config file location, basename | Filesystem paths / string | `results`; config directory, then current directory; file stem, then basename | [src/digitalmodel/code_checks/workflow.py:L257-L276] |

Table 2. Standalone inputs; unit consistency belongs to the caller.

| Input | Unit and range | Default and source | Evidence |
|---|---|---|---|
| Diameter, thickness, yield/ultimate strength, Young modulus | Compatible length and stress units; physical bounds generally unchecked | Required scalars; tests use mm, MPa, N and N.mm | [src/digitalmodel/code_checks/dnv_os_f101.py:L15-L17] [src/digitalmodel/code_checks/dnv_os_f101.py:L40-L58] [src/digitalmodel/code_checks/dnv_os_f101.py:L164-L189] [tests/code_checks/test_dnv_os_f101.py:L1-L9] |
| Design moment, effective tension, internal/external pressure and three capacities | Moment, force, pressure; capacities must be positive | Required; absolute moment and squared tension/net pressure remove load signs | [src/digitalmodel/code_checks/dnv_os_f101.py:L91-L147] |
| Resistance factors | Dimensionless | Function defaults at cited lines; no positivity guard on each factor | [src/digitalmodel/code_checks/dnv_os_f101.py:L99-L100] [src/digitalmodel/code_checks/dnv_os_f101.py:L138-L147] [src/digitalmodel/code_checks/dnv_os_f101.py:L322-L355] |
| Ovality, Poisson ratio and fabrication factor | Dimensionless | Function defaults; ovality below the implementation minimum is clamped | [src/digitalmodel/code_checks/dnv_os_f101.py:L182-L225] |
| Net external overpressure | Pressure; sign unchecked | Required `pe - pmin` | [src/digitalmodel/code_checks/dnv_os_f101.py:L322-L361] |

No global coordinate origin or phase convention is imposed by these scalar/arc-array interfaces; the arc-length position is carried through to each output, and standalone pressure difference is internal minus external. [src/digitalmodel/code_checks/dnv_os_f101.py:L138-L147] [src/digitalmodel/orcaflex/code_check_engine.py:L147-L155] [src/digitalmodel/orcaflex/code_check_engine.py:L260-L267]

Table 3. Outputs.

| Output | Unit / meaning | Evidence |
|---|---|---|
| Standalone capacities, collapse and propagation pressures | Force, moment or pressure in the caller's consistent system | [src/digitalmodel/code_checks/dnv_os_f101.py:L40-L58] [src/digitalmodel/code_checks/dnv_os_f101.py:L216-L237] [src/digitalmodel/code_checks/dnv_os_f101.py:L288-L309] |
| `LocalBucklingResult` | Dimensionless utilisation and Boolean `passes`; equality at unity passes | [src/digitalmodel/code_checks/dnv_os_f101.py:L83-L88] [src/digitalmodel/code_checks/dnv_os_f101.py:L138-L147] |
| `PropagationCheck` | Net pressure, factored resistance, dimensionless utilisation and Boolean propagation flag | [src/digitalmodel/code_checks/dnv_os_f101.py:L312-L361] |
| Router summary replacing `cfg.code_check` | Code, point count, maximum utilisation, all-row pass, governing row, all results and CSV location | [src/digitalmodel/code_checks/workflow.py:L26-L45] |
| CSV rows | Engine result fields plus code; no independent unit metadata or citation sidecar | [src/digitalmodel/code_checks/workflow.py:L236-L254] [src/digitalmodel/code_checks/workflow.py:L279-L289] [src/digitalmodel/orcaflex/code_check_engine.py:L93-L101] [src/digitalmodel/orcaflex/code_check_engine.py:L210-L216] [src/digitalmodel/orcaflex/code_check_engine.py:L305-L313] |

## 3. Method and governing standards

Standards below are the references named in source; clause text not checked against the licensed copy for any entry. No tabulated standard value is reproduced here. [src/digitalmodel/code_checks/dnv_os_f101.py:L19-L24] [src/digitalmodel/orcaflex/code_check_engine.py:L12-L18] [src/digitalmodel/orcaflex/code_check_engine.py:L163-L166] [src/digitalmodel/orcaflex/code_check_engine.py:L275-L278]

Table 4. Implemented methods and reference limits.

| Method and implementation | Named reference / edition / clause | Algorithm and limit | Evidence |
|---|---|---|---|
| `plastic_axial_capacity`, `plastic_moment_capacity`, `pressure_containment_resistance` | DNV-OS-F101, Submarine Pipeline Systems, 2013; Local buckling | Analytical section capacities and characteristic-strength containment calculation | [src/digitalmodel/code_checks/dnv_os_f101.py:L19-L24] [src/digitalmodel/code_checks/dnv_os_f101.py:L40-L58] |
| `local_buckling_lcc` | DNV-OS-F101, 2013; Local buckling, load-controlled condition, Section 5 D404/D405 | Squared moment/tension group plus squared net-pressure ratio; explicit positive-capacity guard | [src/digitalmodel/code_checks/dnv_os_f101.py:L19-L24] [src/digitalmodel/code_checks/dnv_os_f101.py:L135-L147] |
| `elastic_collapse_pressure`, `plastic_collapse_pressure`, `collapse_pressure`, `_real_cubic_roots` | DNV-OS-F101, 2013; External-pressure collapse, Section 5 D401 | Elastic/plastic estimates enter a cubic; depressed-cubic/trigonometric solution selects the smallest positive real root | [src/digitalmodel/code_checks/dnv_os_f101.py:L19-L24] [src/digitalmodel/code_checks/dnv_os_f101.py:L164-L179] [src/digitalmodel/code_checks/dnv_os_f101.py:L221-L275] |
| `propagation_pressure`, `propagation_check` | DNV-OS-F101, 2013; Propagation buckling, Section 5 D501 | Geometry power law; compare net external pressure with factored propagation resistance | [src/digitalmodel/code_checks/dnv_os_f101.py:L19-L24] [src/digitalmodel/code_checks/dnv_os_f101.py:L288-L309] [src/digitalmodel/code_checks/dnv_os_f101.py:L352-L361] |
| `_run_api_rp_2rd` -> `check_api_rp_2rd` | API STD 2RD, Dynamic Risers for Floating Production Systems, second edition, 2013; specific clause not recorded | Historical RP symbol retained; tension/bending ellipse, rounded utilisation; pressure only echoed | [src/digitalmodel/code_checks/workflow.py:L58-L79] [src/digitalmodel/orcaflex/code_check_engine.py:L1-L18] [src/digitalmodel/orcaflex/code_check_engine.py:L128-L155] |
| `_run_dnv_os_f201` -> `check_dnv_os_f201` | DNV-OS-F201, Dynamic Risers, 2010; Section 5, combined loading | Simplified squared moment/tension criterion; pressure arguments and condition factor unused | [src/digitalmodel/code_checks/workflow.py:L82-L110] [src/digitalmodel/orcaflex/code_check_engine.py:L163-L175] [src/digitalmodel/orcaflex/code_check_engine.py:L219-L269] |
| `_run_api_rp_2sk` -> `check_mooring_api_2sk` | API RP 2SK, Design and Analysis of Stationkeeping Systems, 2005; Section 6.2, Table 1 | Breaking-load/tension safety factor against selected condition; no solver launch | [src/digitalmodel/code_checks/workflow.py:L113-L127] [src/digitalmodel/orcaflex/code_check_engine.py:L18-L18] [src/digitalmodel/orcaflex/code_check_engine.py:L275-L349] |

The workflow registry uses historical API RP naming while its delegated engine identifies API STD 2RD; F101 and F201 are separate pipeline and dynamic-riser methods, not conflicting editions of one calculation. The engine explicitly distinguishes DNV-OS-F201 (2010) from its ST successor. [docs/registry/workflows.yaml:L281-L287] [src/digitalmodel/orcaflex/code_check_engine.py:L1-L18] [src/digitalmodel/code_checks/dnv_os_f101.py:L19-L24]

Citation-sidecar gap: standalone results contain numeric fields and flags only, while workflow serialization forwards the engine result models without `Citation` metadata; issue pending (3). [src/digitalmodel/code_checks/dnv_os_f101.py:L83-L88] [src/digitalmodel/code_checks/dnv_os_f101.py:L312-L319] [src/digitalmodel/code_checks/workflow.py:L236-L254] [src/digitalmodel/orcaflex/code_check_engine.py:L93-L101] [src/digitalmodel/orcaflex/code_check_engine.py:L210-L216] [src/digitalmodel/orcaflex/code_check_engine.py:L305-L313]
## 4. Validated worked examples

The three validated cases below validate closed-form arithmetic using generic inputs and independently written expressions in tests; they do not establish licensed-standard compliance. The workflow cases assert the module's own output relationships and are regression examples. [tests/code_checks/test_dnv_os_f101.py:L33-L44] [tests/code_checks/test_dnv_os_f101.py:L94-L112] [tests/code_checks/test_code_check_workflow.py:L11-L52] [tests/workflows/test_durable_workflows.py:L515-L541]

Table 5. Case results at the code pin, before and after documentation changes; `PASS/PASS` means BASE/AFTER.

| Case / kind | Source and input | Expected quantity / tolerance basis | Existing test node ID | Offline result |
|---|---|---|---|---|
| V1 axial / validated | Closed-form annular thin-wall capacity, inline generic inputs | `450*pi*480*20` N; test `pytest.approx`; C2 also checks relative error <= 0.000001 | `tests/code_checks/test_dnv_os_f101.py::test_plastic_axial_capacity` | PASS/PASS [tests/code_checks/test_dnv_os_f101.py:L33-L37] |
| V2 moment / validated | Closed-form section capacity, inline generic inputs | `450*480**2*20` N.mm; test `pytest.approx`; C2 also checks relative error <= 0.000001 | `tests/code_checks/test_dnv_os_f101.py::test_plastic_moment_capacity` | PASS/PASS [tests/code_checks/test_dnv_os_f101.py:L40-L44] |
| V3 combined load / validated | Closed-form arithmetic with unit factors and synthetic capacities | Utilisation 0.5981, dimensionless; test `pytest.approx`; C2 also checks absolute error <= 0.000001 | `tests/code_checks/test_dnv_os_f101.py::test_local_buckling_combined_handcalc` | PASS/PASS [tests/code_checks/test_dnv_os_f101.py:L94-L112] |
| R1 API router / regression | Inline config; test-generated YAML | 3 CSV rows; governing position 100 m; CSV maximum equals summary by `pytest.approx` | `tests/code_checks/test_code_check_workflow.py::test_api_rp_2rd_code_check_workflow_writes_results_and_summary` | PASS/PASS [tests/code_checks/test_code_check_workflow.py:L11-L52] |
| R2 selector / regression | Inline unknown-code config | 1 `ValueError` matching unsupported-code message | `tests/code_checks/test_code_check_workflow.py::test_code_check_workflow_unknown_code_fails_closed` | PASS/PASS [tests/code_checks/test_code_check_workflow.py:L55-L69] |
| R3 API riser / regression | `examples/workflows/api-rp-2rd-riser/input.yml` | Correct code, positive finite utilisation, CSV maximum equals summary | `tests/workflows/test_durable_workflows.py::test_workflow_registry[api-rp-2rd-riser]` | PASS/PASS [docs/registry/workflows.yaml:L281-L288] [tests/workflows/test_durable_workflows.py:L515-L541] |
| R4 DNV riser / regression | `examples/workflows/dnv-os-f201-riser/input.yml` | Correct code, positive finite utilisation, CSV maximum equals summary | `tests/workflows/test_durable_workflows.py::test_workflow_registry[dnv-os-f201-riser]` | PASS/PASS [docs/registry/workflows.yaml:L309-L316] [tests/workflows/test_durable_workflows.py:L515-L541] |
| R5 mooring / regression | `examples/workflows/api-2sk-mooring/input.yml` | Correct code, positive finite utilisation, CSV maximum equals summary | `tests/workflows/test_durable_workflows.py::test_workflow_registry[api-2sk-mooring]` | PASS/PASS [docs/registry/workflows.yaml:L317-L324] [tests/workflows/test_durable_workflows.py:L515-L541] |

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations, with G5 from section 9; checks below concern the offline calculation and export interfaces. (AQG r0, sections 9–10, Python calculations.)

Table 6. Gate set and module-specific checks.

| Gate | Applicability | Check | Numeric pass criterion and unit | Executable evidence |
|---|---|---|---|---|
| G0 | Applicable: inputs, units, citations | C0 selector rejection | 1 expected `ValueError`; 0 accepted unknown selectors, count | R2 test [tests/code_checks/test_code_check_workflow.py:L55-L69] |
| G0 | Applicable | C0U input-basis audit | 0 omitted required load fields; 0 unit mismatches; 0 unresolved standard-reference choices, count, before design use | Audit Tables 1–4 against intended input; currently open for physical bounds, provenance and standard qualification, issue pending (1), issue pending (2), issue pending (3) [src/digitalmodel/code_checks/workflow.py:L58-L127] [src/digitalmodel/code_checks/dnv_os_f101.py:L15-L24] |
| G1 | Applicable: imports and tests | C1 offline execution | 19/19 package nodes and 3/3 named durable-workflow nodes pass; 0 failures, count | `python -m pytest tests/code_checks/` and the three full workflow node IDs in section 4; no target solver file is produced [tests/code_checks/test_code_check_workflow.py:L11-L69] [tests/code_checks/test_dnv_os_f101.py:L33-L228] [tests/workflows/test_durable_workflows.py:L515-L541] |
| G2 | Applicable: hand check | C2 closed-form arithmetic | V1/V2 relative error <= 0.000001 dimensionless; V3 absolute error <= 0.000001 utilisation | Recompute the three expressions in Table 5 separately and compare with the cited functions; PASS in the recorded offline audit [tests/code_checks/test_dnv_os_f101.py:L33-L44] [tests/code_checks/test_dnv_os_f101.py:L94-L112] |
| G2 | Applicable | C2O ovality clamp | 0 pressure difference, MPa, to `pytest.approx`, for below-minimum and minimum inputs | `tests/code_checks/test_dnv_os_f101.py::test_collapse_pressure_clamps_minimum_ovality` [tests/code_checks/test_dnv_os_f101.py:L197-L201] |
| G2 | Applicable | C2C collapse equation | Absolute cubic residual <= 0.000001 MPa cubed; collapse pressure below both elastic and plastic estimates, MPa | `tests/code_checks/test_dnv_os_f101.py::test_collapse_pressure_satisfies_cubic`; verifies equation residual, not an external benchmark [tests/code_checks/test_dnv_os_f101.py:L177-L188] |
| G3 | NOT_APPLICABLE | No time integration in these offline methods | No dynamic solver launch | Scalar calculations and array loops [src/digitalmodel/code_checks/dnv_os_f101.py:L138-L147] [src/digitalmodel/orcaflex/code_check_engine.py:L128-L155] |
| G4 | NOT_APPLICABLE | No diffraction mesh or hydrodynamic solution in these interfaces | No panel mesh produced | CSV/result export [src/digitalmodel/code_checks/workflow.py:L26-L45] [src/digitalmodel/code_checks/workflow.py:L279-L289] |
| G5 | Applicable: calculation evidence | C5 export consistency | 3 output rows; 0 missing governing rows; CSV maximum equals summary to `pytest.approx`, dimensionless | R1; durable regressions also compare output and summary; complete signed/fingerprinted QA appendix remains open, issue pending (3) [tests/code_checks/test_code_check_workflow.py:L37-L52] [tests/workflows/test_durable_workflows.py:L515-L541] |

G1's program route is Python imports and tests: this router writes CSV, not an OrcaFlex or AQWA model, so program-specific model loading is outside its output contract. [src/digitalmodel/code_checks/workflow.py:L26-L45] [src/digitalmodel/code_checks/workflow.py:L279-L289]

## 6. Known failure modes and pitfalls

sweep: 32 found

Sweep scope: all three package Python files, including defaults and numerical guards that are intentional; rows S01–S32 identify distinct sites rather than asserting that every site is a defect. [src/digitalmodel/code_checks/__init__.py:L1-L1] [src/digitalmodel/code_checks/dnv_os_f101.py:L1-L362] [src/digitalmodel/code_checks/workflow.py:L1-L297]

Exact discovery commands, followed by reading all package source:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|float\(|int\(|unit|default|cache' src/digitalmodel/code_checks
rg -n 'get\(|astype|dtype|np.repeat|gamma_|alpha_fab|poisson|ovality|1e-300|cos_arg' src/digitalmodel/code_checks --glob '*.py'
```

Table 7. Sweep register; reviewed on the evidence date in the header.

| Site | Symptom / cause | Existing detection or gap / tracking | Evidence |
|---|---|---|---|
| S01 standalone unit assumption | No unit conversion; incompatible inputs can produce plausible numbers | no detection check exists; issue pending (2) | [src/digitalmodel/code_checks/dnv_os_f101.py:L15-L17] |
| S02 local material factor default | Omitted factor selects implementation default | no detection check exists for design-basis selection; issue pending (3) | [src/digitalmodel/code_checks/dnv_os_f101.py:L99-L99] |
| S03 local safety factor default | Same omission risk for safety class | no detection check exists for design-basis selection; issue pending (3) | [src/digitalmodel/code_checks/dnv_os_f101.py:L100-L100] |
| S04 elastic Poisson default | Unstated material assumption | no detection check exists; issue pending (2) | [src/digitalmodel/code_checks/dnv_os_f101.py:L164-L168] |
| S05 plastic fabrication default | Manufacturing assumption if omitted | no detection check exists; issue pending (3) | [src/digitalmodel/code_checks/dnv_os_f101.py:L171-L179] |
| S06 collapse ovality default | Omitted imperfection selects minimum | no detection check exists for omitted input; issue pending (2) | [src/digitalmodel/code_checks/dnv_os_f101.py:L187-L187] |
| S07 collapse Poisson default | Unstated material assumption | no detection check exists; issue pending (2) | [src/digitalmodel/code_checks/dnv_os_f101.py:L188-L188] |
| S08 collapse fabrication default | Manufacturing assumption if omitted | no detection check exists; issue pending (3) | [src/digitalmodel/code_checks/dnv_os_f101.py:L189-L189] |
| S09 ovality clamp | Below-minimum input replaced without a diagnostic | C2O detects replacement; issue pending (2) covers warning/validation policy | [src/digitalmodel/code_checks/dnv_os_f101.py:L221-L222] |
| S10 root cosine clamp | Out-of-domain cosine argument forced inside bounds | no detection check exists for clamp branch; issue pending (2) | [src/digitalmodel/code_checks/dnv_os_f101.py:L269-L274] |
| S11 propagation fabrication default | Manufacturing assumption if omitted | no detection check exists; issue pending (3) | [src/digitalmodel/code_checks/dnv_os_f101.py:L288-L309] |
| S12 propagation-check fabrication default | Same assumption propagated to pressure helper | no detection check exists; issue pending (3) | [src/digitalmodel/code_checks/dnv_os_f101.py:L327-L327] |
| S13 propagation material factor default | Omitted resistance factor | no detection check exists for basis; issue pending (3) | [src/digitalmodel/code_checks/dnv_os_f101.py:L328-L328] |
| S14 propagation safety factor default | Omitted safety-class choice | no detection check exists for basis; issue pending (3) | [src/digitalmodel/code_checks/dnv_os_f101.py:L329-L329] |
| S15 settings fallback | Missing/false settings become empty mapping; `_inputs` then raises | no detection check exists for this fallback; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L26-L29] [src/digitalmodel/code_checks/workflow.py:L130-L134] |
| S16 governing `float` conversion | Numeric coercion of result; non-finite ordering unchecked | C1 catches only nominal cases; no detection check exists for non-finite inputs; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L34-L35] |
| S17 summary `float` conversion | Coercion repeats without finite guard | no detection check exists for non-finite inputs; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L39-L39] |
| S18 line-name fallback | Missing identifier becomes generic line name | no detection check exists; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L113-L115] |
| S19 model-field fallback | Unknown model metadata becomes empty field dictionary | no detection check exists; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L159-L160] |
| S20 field filtering | Extra/misspelled model input keys discarded; omitted fields use engine defaults | no detection check exists; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L159-L161] |
| S21 optional `None` | Missing pressure returns `None`; upstream API substitutes zeros, DNV ignores pressures | no detection check exists for pressure sensitivity; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L186-L194] [src/digitalmodel/orcaflex/code_check_engine.py:L128-L155] [src/digitalmodel/orcaflex/code_check_engine.py:L219-L269] |
| S22 scalar broadcast | Single response repeated at every arc position | no detection check exists for accidental scalar input; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L197-L205] |
| S23 list/tuple conversion | External text converted through NumPy; decimal/thousands separators unsupported | no detection check exists for locale-formatted numbers; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L211-L212] |
| S24 scalar conversion | Same conversion risk for scalar pressure/response | no detection check exists; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L213-L214] |
| S25 array conversion | `astype(float)` accepts non-finite values, no finite/rank validation | no detection check exists; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L208-L215] |
| S26 tension conversion | `float(value)` accepts non-finite or negative tension | no detection check exists; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L218-L226] |
| S27 alias sentinel | No populated alias returns `None`; conflicting aliases not compared | no detection check exists; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L229-L233] |
| S28 output directory default | Results location inferred when omitted | C5 checks explicit relative output only; no detection check exists for omitted directory; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L257-L262] |
| S29 config-directory fallback | No config path uses current directory | no detection check exists; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L265-L270] |
| S30 file-stem fallback | No config filename uses basename or generic stem | no detection check exists; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L273-L276] |
| S31 display-path exception | Outside repository, `ValueError` becomes absolute display path; I/O errors are not swallowed | no detection check exists; issue pending (1) | [src/digitalmodel/code_checks/workflow.py:L292-L297] |
| S32 degenerate cubic guard | Very small depressed-cubic coefficient switches root branch | no detection check exists for this branch; issue pending (2) | [src/digitalmodel/code_checks/dnv_os_f101.py:L264-L268] |

The other `except`, in `_code_standard`, re-raises unsupported selectors; C0 checks this behavior. Empty result lists are rejected before CSV writing; no `continue`, epoch default or cache writer occurs in the package bodies inspected. [src/digitalmodel/code_checks/workflow.py:L153-L156] [src/digitalmodel/code_checks/workflow.py:L236-L244] [src/digitalmodel/code_checks/workflow.py:L279-L297] [src/digitalmodel/code_checks/dnv_os_f101.py:L1-L362]

Table 8. Additional confirmed limitations, including delegated code affecting the router.

| Symptom / cause | Detection or gap / tracking | Evidence |
|---|---|---|
| API pressure only echoed; DNV pressure inputs unused; DNV `condition_factor` unused | no detection check exists for physical sensitivity; issue pending (1) | [src/digitalmodel/orcaflex/code_check_engine.py:L128-L155] [src/digitalmodel/orcaflex/code_check_engine.py:L175-L175] [src/digitalmodel/orcaflex/code_check_engine.py:L219-L269] |
| Invalid geometry can reach capacity calculations; some nonpositive upstream capacities become zero utilisation | no detection check exists; issue pending (1), issue pending (2) | [src/digitalmodel/code_checks/dnv_os_f101.py:L40-L58] [src/digitalmodel/orcaflex/code_check_engine.py:L135-L145] [src/digitalmodel/orcaflex/code_check_engine.py:L247-L258] |
| Nonpositive mooring tension produces infinite achieved safety factor and can pass | no detection check exists; issue pending (1) | [src/digitalmodel/orcaflex/code_check_engine.py:L335-L349] |
| Upstream utilisation rounded before the riser pass flag is computed | no detection check exists for near-unity misclassification; issue pending (1) | [src/digitalmodel/orcaflex/code_check_engine.py:L146-L155] [src/digitalmodel/orcaflex/code_check_engine.py:L259-L267] |
| Header docstring says strict local utilisation inequality; implementation includes equality | no detection check exists at exactly unity; issue pending (2) | [src/digitalmodel/code_checks/dnv_os_f101.py:L6-L8] [src/digitalmodel/code_checks/dnv_os_f101.py:L146-L147] |
| Missing calculation citation sidecars and complete QA appendix | C0U records the missing provenance; existing #1247 covers the delegated riser citation surface; standalone/output coverage is issue pending (3) | [src/digitalmodel/code_checks/dnv_os_f101.py:L83-L88] [src/digitalmodel/code_checks/dnv_os_f101.py:L312-L319] [src/digitalmodel/code_checks/workflow.py:L236-L254] |

No incident issue was specified for this package; the read-only issue search located #1247 for the delegated citation gap and #2161 for historical edition-label corrections. Current engine references distinguish API STD 2RD and DNV-OS-F201 editions; no conflicting edition for the same method was confirmed in the scoped files. [src/digitalmodel/orcaflex/code_check_engine.py:L1-L18] [src/digitalmodel/code_checks/dnv_os_f101.py:L19-L24]
## 7. Lessons log

Table 9. Initial evidence entry; subsequent entries append to this table.

| Date | Entry | Lesson | Evidence | Pack change | Shared-log entry |
|---|---|---|---|---|---|
| 2026-10-18 | EC-CHECKS-001 | Separate algebra checks from standard qualification; workflow CSV agreement is regression evidence | [tests/code_checks/test_dnv_os_f101.py:L94-L112] [tests/workflows/test_durable_workflows.py:L515-L541] | Sections 3–6 distinguish result provenance and missing sensitivity checks | Proposed; issue pending (3) |

## 8. Ownership

Proposed review assignments: engineering calculations quality manager; deputy engineering calculations quality manager; producing group: calculation maintainers. Standard interpretation, missing reference evidence and tolerance changes require the family quality manager's disposition before clearance; assignment confirmation is issue pending (3). (AQG r0, section 11, Sign-off and independence.)

## 9. Dependencies

The only package reference to the OrcaFlex namespace is the import from `digitalmodel.orcaflex.code_check_engine` at workflow lines 12–20; it supplies three input models, the code enum and the three delegated check functions. This is a textual dependency inventory; no solver or licensed interface was used. [src/digitalmodel/code_checks/workflow.py:L12-L20] [src/digitalmodel/orcaflex/code_check_engine.py:L1-L18]

Upstream runtime dependencies used by this package are Python `math`/`dataclasses` for standalone methods and `csv`, `re`, `pathlib`, typing and NumPy for routing; delegated input/results use Pydantic. Downstream contracts are the three registered offline workflows and the engine's code-check dispatch. No solver version is an input to these interfaces. [src/digitalmodel/code_checks/dnv_os_f101.py:L27-L32] [src/digitalmodel/code_checks/workflow.py:L3-L20] [src/digitalmodel/orcaflex/code_check_engine.py:L21-L26] [docs/registry/workflows.yaml:L281-L288] [docs/registry/workflows.yaml:L309-L324] [src/digitalmodel/engine.py:L481-L484]

### Implementation inventory and counted-family search

At the map revision and data revision named in the header, `effective-tension-riser-stack-up` is the only family with in-package rows: three, therefore rank 1 and the sole counted family. Its complete inventory contains 264 records, including outside-package definitions and `scripts/`; the cited family segment is identical at the code pin. [docs/registry/implementation-map.yaml:L2139-L2144] [docs/registry/implementation-map.yaml:L2146-L4932] [docs/registry/implementation-map.yaml:L2606-L2635]

Table 10. All in-package implementation-map rows, retaining the map's classifications.

| Family | Path / definition | Map relation | Source interpretation | Evidence |
|---|---|---|---|---|
| effective-tension-riser-stack-up | `dnv_os_f101.py` / `local_buckling_lcc` | independent | Computes combined utilisation, already listed in section 3 | [docs/registry/implementation-map.yaml:L2606-L2615] [src/digitalmodel/code_checks/dnv_os_f101.py:L135-L147] |
| effective-tension-riser-stack-up | `workflow.py` / `_run_api_rp_2rd` | independent | Delegates to listed upstream checker | [docs/registry/implementation-map.yaml:L2616-L2625] [src/digitalmodel/code_checks/workflow.py:L58-L79] |
| effective-tension-riser-stack-up | `workflow.py` / `_run_dnv_os_f201` | independent | Delegates to listed upstream checker | [docs/registry/implementation-map.yaml:L2626-L2635] [src/digitalmodel/code_checks/workflow.py:L82-L110] |

The exact case-sensitive Python `re` patterns, applied to every package `.py` file at the code pin, are below; none matches the relative package path `code_checks` case-insensitively, so none was removed. [docs/registry/implementation-map.yaml:L2139-L2144]

```text
effective_tension|effectiveTension|top_tension|tensioner|stack_up|internal_pressure.*(?:area|A_i)|external_pressure.*(?:area|A_o)
effective.tension|tensioner|stack.up
internal_pressure|external_pressure|fluid_density|buoyancy_module
```

Table 11. All matching definitions; 18 distinct matching lines, assigned to six enclosing definitions including module-level text.

| Definition | Hit lines | Classification | Evidence |
|---|---|---|---|
| `dnv_os_f101.py` module level | 7 | Docstring, no additional implementation | [src/digitalmodel/code_checks/dnv_os_f101.py:L6-L8] |
| `local_buckling_lcc` | 93, 108, 140 | Listed implementation | [src/digitalmodel/code_checks/dnv_os_f101.py:L91-L147] |
| `PropagationCheck` | 316 | Data holder | [src/digitalmodel/code_checks/dnv_os_f101.py:L312-L319] |
| `propagation_check` | 323, 338, 356, 358 | Propagation result already listed in section 3; not an unlisted tension method | [src/digitalmodel/code_checks/dnv_os_f101.py:L322-L361] |
| `_run_api_rp_2rd` | 63, 65 | Listed delegate | [src/digitalmodel/code_checks/workflow.py:L58-L79] |
| `_run_dnv_os_f201` | 87, 89, 97, 99, 102, 104, 108 | Listed delegate | [src/digitalmodel/code_checks/workflow.py:L82-L110] |

Further second implementations found inside the package: 0, meeting the at-most-1 acceptance threshold for this counted family. Propagation, collapse and section-capacity methods are listed in section 3 even where their method family has no in-package map row. [src/digitalmodel/code_checks/dnv_os_f101.py:L40-L58] [src/digitalmodel/code_checks/dnv_os_f101.py:L164-L275] [src/digitalmodel/code_checks/dnv_os_f101.py:L288-L361] [docs/registry/implementation-map.yaml:L2606-L2635]

The whole-repository method-name search includes `scripts/`, source, tests and examples; alternate local-buckling/collapse/propagation implementations include `DNVOSF101Pipe` in the analysis script and API external-collapse functions in the pipeline pressure module. These outside-package hits are map follow-up, issue pending (4). [scripts/python/digitalmodel/analysis/RigidPipe.py:L138-L205] [src/digitalmodel/subsea/pipeline/pipeline_pressure.py:L381-L445]

```sh
git grep -n -E 'local_buckling_lcc|collapse_pressure|propagation_pressure|propagation_check|check_api_rp_2rd|check_dnv_os_f201|check_mooring_api_2sk|localBucklingCombinedLoadingLoadControlled' c099a5e1317e6bed63d876252d87cbb12c774372 -- '*.py' '*.sh'
```

Map follow-up: classify local-buckling/collapse, propagation, section capacity and mooring-safety methods before counting newly mapped package families; distinguish router delegates from independent algorithms; issue pending (4). [src/digitalmodel/code_checks/dnv_os_f101.py:L40-L58] [src/digitalmodel/code_checks/dnv_os_f101.py:L164-L275] [src/digitalmodel/code_checks/dnv_os_f101.py:L288-L361] [src/digitalmodel/code_checks/workflow.py:L58-L127]

Table 12. Complete family inventory; original ordinal and map-record citation identify every external definition without duplicating discovery names.

| Map ordinal | Definition locator | Map relation | Evidence |
|---|---|---|---|
| 1 | External definition, map record 1 | independent | [docs/registry/implementation-map.yaml:L2146-L2155] |
| 2 | External definition, map record 2 | independent | [docs/registry/implementation-map.yaml:L2156-L2165] |
| 3 | External definition, map record 3 | independent | [docs/registry/implementation-map.yaml:L2166-L2175] |
| 4 | External definition, map record 4 | independent | [docs/registry/implementation-map.yaml:L2176-L2185] |
| 5 | External definition, map record 5 | independent | [docs/registry/implementation-map.yaml:L2186-L2195] |
| 6 | External definition, map record 6 | independent | [docs/registry/implementation-map.yaml:L2196-L2205] |
| 7 | External definition, map record 7 | independent | [docs/registry/implementation-map.yaml:L2206-L2215] |
| 8 | External definition, map record 8 | independent | [docs/registry/implementation-map.yaml:L2216-L2225] |
| 9 | External definition, map record 9 | example | [docs/registry/implementation-map.yaml:L2226-L2235] |
| 10 | External definition, map record 10 | example | [docs/registry/implementation-map.yaml:L2236-L2247] |
| 11 | External definition, map record 11 | example | [docs/registry/implementation-map.yaml:L2248-L2259] |
| 12 | External definition, map record 12 | example | [docs/registry/implementation-map.yaml:L2260-L2269] |
| 13 | External definition, map record 13 | example | [docs/registry/implementation-map.yaml:L2270-L2282] |
| 14 | External definition, map record 14 | example | [docs/registry/implementation-map.yaml:L2283-L2291] |
| 15 | External definition, map record 15 | example | [docs/registry/implementation-map.yaml:L2292-L2301] |
| 16 | External definition, map record 16 | example | [docs/registry/implementation-map.yaml:L2302-L2311] |
| 17 | External definition, map record 17 | example | [docs/registry/implementation-map.yaml:L2312-L2324] |
| 18 | External definition, map record 18 | example | [docs/registry/implementation-map.yaml:L2325-L2334] |
| 19 | External definition, map record 19 | example | [docs/registry/implementation-map.yaml:L2335-L2344] |
| 20 | External definition, map record 20 | example | [docs/registry/implementation-map.yaml:L2345-L2356] |
| 21 | External definition, map record 21 | independent | [docs/registry/implementation-map.yaml:L2357-L2365] |
| 22 | External definition, map record 22 | independent | [docs/registry/implementation-map.yaml:L2366-L2375] |
| 23 | External definition, map record 23 | wrapper | [docs/registry/implementation-map.yaml:L2376-L2387] |
| 24 | External definition, map record 24 | independent | [docs/registry/implementation-map.yaml:L2388-L2397] |
| 25 | External definition, map record 25 | independent | [docs/registry/implementation-map.yaml:L2398-L2407] |
| 26 | External definition, map record 26 | independent | [docs/registry/implementation-map.yaml:L2408-L2417] |
| 27 | External definition, map record 27 | independent | [docs/registry/implementation-map.yaml:L2418-L2427] |
| 28 | External definition, map record 28 | independent | [docs/registry/implementation-map.yaml:L2428-L2437] |
| 29 | External definition, map record 29 | independent | [docs/registry/implementation-map.yaml:L2438-L2447] |
| 30 | External definition, map record 30 | independent | [docs/registry/implementation-map.yaml:L2448-L2457] |
| 31 | External definition, map record 31 | independent | [docs/registry/implementation-map.yaml:L2458-L2470] |
| 32 | External definition, map record 32 | independent | [docs/registry/implementation-map.yaml:L2471-L2484] |
| 33 | External definition, map record 33 | independent | [docs/registry/implementation-map.yaml:L2485-L2494] |
| 34 | External definition, map record 34 | independent | [docs/registry/implementation-map.yaml:L2495-L2504] |
| 35 | External definition, map record 35 | independent | [docs/registry/implementation-map.yaml:L2505-L2514] |
| 36 | External definition, map record 36 | independent | [docs/registry/implementation-map.yaml:L2515-L2524] |
| 37 | External definition, map record 37 | independent | [docs/registry/implementation-map.yaml:L2525-L2534] |
| 38 | External definition, map record 38 | independent | [docs/registry/implementation-map.yaml:L2535-L2544] |
| 39 | External definition, map record 39 | caller | [docs/registry/implementation-map.yaml:L2545-L2556] |
| 40 | External definition, map record 40 | independent | [docs/registry/implementation-map.yaml:L2557-L2566] |
| 41 | External definition, map record 41 | independent | [docs/registry/implementation-map.yaml:L2567-L2576] |
| 42 | External definition, map record 42 | independent | [docs/registry/implementation-map.yaml:L2577-L2586] |
| 43 | External definition, map record 43 | independent | [docs/registry/implementation-map.yaml:L2587-L2595] |
| 44 | External definition, map record 44 | independent | [docs/registry/implementation-map.yaml:L2596-L2605] |
| 45 | `src/digitalmodel/code_checks/dnv_os_f101.py` / `local_buckling_lcc` | independent | [docs/registry/implementation-map.yaml:L2606-L2615] |
| 46 | `src/digitalmodel/code_checks/workflow.py` / `_run_api_rp_2rd` | independent | [docs/registry/implementation-map.yaml:L2616-L2625] |
| 47 | `src/digitalmodel/code_checks/workflow.py` / `_run_dnv_os_f201` | independent | [docs/registry/implementation-map.yaml:L2626-L2635] |
| 48 | External definition, map record 48 | independent | [docs/registry/implementation-map.yaml:L2636-L2645] |
| 49 | External definition, map record 49 | independent | [docs/registry/implementation-map.yaml:L2646-L2655] |
| 50 | External definition, map record 50 | independent | [docs/registry/implementation-map.yaml:L2656-L2667] |
| 51 | External definition, map record 51 | independent | [docs/registry/implementation-map.yaml:L2668-L2679] |
| 52 | External definition, map record 52 | independent | [docs/registry/implementation-map.yaml:L2680-L2689] |
| 53 | External definition, map record 53 | independent | [docs/registry/implementation-map.yaml:L2690-L2701] |
| 54 | External definition, map record 54 | independent | [docs/registry/implementation-map.yaml:L2702-L2715] |
| 55 | External definition, map record 55 | independent | [docs/registry/implementation-map.yaml:L2716-L2726] |
| 56 | External definition, map record 56 | independent | [docs/registry/implementation-map.yaml:L2727-L2740] |
| 57 | External definition, map record 57 | independent | [docs/registry/implementation-map.yaml:L2741-L2750] |
| 58 | External definition, map record 58 | independent | [docs/registry/implementation-map.yaml:L2751-L2760] |
| 59 | External definition, map record 59 | independent | [docs/registry/implementation-map.yaml:L2761-L2774] |
| 60 | External definition, map record 60 | independent | [docs/registry/implementation-map.yaml:L2775-L2787] |
| 61 | External definition, map record 61 | independent | [docs/registry/implementation-map.yaml:L2788-L2797] |
| 62 | External definition, map record 62 | independent | [docs/registry/implementation-map.yaml:L2798-L2807] |
| 63 | External definition, map record 63 | independent | [docs/registry/implementation-map.yaml:L2808-L2817] |
| 64 | External definition, map record 64 | independent | [docs/registry/implementation-map.yaml:L2818-L2827] |
| 65 | External definition, map record 65 | independent | [docs/registry/implementation-map.yaml:L2828-L2839] |
| 66 | External definition, map record 66 | independent | [docs/registry/implementation-map.yaml:L2840-L2852] |
| 67 | External definition, map record 67 | independent | [docs/registry/implementation-map.yaml:L2853-L2862] |
| 68 | External definition, map record 68 | independent | [docs/registry/implementation-map.yaml:L2863-L2874] |
| 69 | External definition, map record 69 | independent | [docs/registry/implementation-map.yaml:L2875-L2884] |
| 70 | External definition, map record 70 | caller | [docs/registry/implementation-map.yaml:L2885-L2896] |
| 71 | External definition, map record 71 | independent | [docs/registry/implementation-map.yaml:L2897-L2907] |
| 72 | External definition, map record 72 | independent | [docs/registry/implementation-map.yaml:L2908-L2917] |
| 73 | External definition, map record 73 | independent | [docs/registry/implementation-map.yaml:L2918-L2927] |
| 74 | External definition, map record 74 | independent | [docs/registry/implementation-map.yaml:L2928-L2938] |
| 75 | External definition, map record 75 | independent | [docs/registry/implementation-map.yaml:L2939-L2948] |
| 76 | External definition, map record 76 | independent | [docs/registry/implementation-map.yaml:L2949-L2960] |
| 77 | External definition, map record 77 | independent | [docs/registry/implementation-map.yaml:L2961-L2970] |
| 78 | External definition, map record 78 | independent | [docs/registry/implementation-map.yaml:L2971-L2980] |
| 79 | External definition, map record 79 | independent | [docs/registry/implementation-map.yaml:L2981-L2990] |
| 80 | External definition, map record 80 | independent | [docs/registry/implementation-map.yaml:L2991-L3000] |
| 81 | External definition, map record 81 | independent | [docs/registry/implementation-map.yaml:L3001-L3010] |
| 82 | External definition, map record 82 | independent | [docs/registry/implementation-map.yaml:L3011-L3022] |
| 83 | External definition, map record 83 | independent | [docs/registry/implementation-map.yaml:L3023-L3032] |
| 84 | External definition, map record 84 | independent | [docs/registry/implementation-map.yaml:L3033-L3042] |
| 85 | External definition, map record 85 | independent | [docs/registry/implementation-map.yaml:L3043-L3052] |
| 86 | External definition, map record 86 | independent | [docs/registry/implementation-map.yaml:L3053-L3062] |
| 87 | External definition, map record 87 | independent | [docs/registry/implementation-map.yaml:L3063-L3072] |
| 88 | External definition, map record 88 | independent | [docs/registry/implementation-map.yaml:L3073-L3082] |
| 89 | External definition, map record 89 | independent | [docs/registry/implementation-map.yaml:L3083-L3092] |
| 90 | External definition, map record 90 | independent | [docs/registry/implementation-map.yaml:L3093-L3102] |
| 91 | External definition, map record 91 | independent | [docs/registry/implementation-map.yaml:L3103-L3112] |
| 92 | External definition, map record 92 | independent | [docs/registry/implementation-map.yaml:L3113-L3122] |
| 93 | External definition, map record 93 | independent | [docs/registry/implementation-map.yaml:L3123-L3135] |
| 94 | External definition, map record 94 | independent | [docs/registry/implementation-map.yaml:L3136-L3145] |
| 95 | External definition, map record 95 | independent | [docs/registry/implementation-map.yaml:L3146-L3155] |
| 96 | External definition, map record 96 | caller | [docs/registry/implementation-map.yaml:L3156-L3168] |
| 97 | External definition, map record 97 | independent | [docs/registry/implementation-map.yaml:L3169-L3178] |
| 98 | External definition, map record 98 | independent | [docs/registry/implementation-map.yaml:L3179-L3188] |
| 99 | External definition, map record 99 | independent | [docs/registry/implementation-map.yaml:L3189-L3198] |
| 100 | External definition, map record 100 | independent | [docs/registry/implementation-map.yaml:L3199-L3208] |
| 101 | External definition, map record 101 | independent | [docs/registry/implementation-map.yaml:L3209-L3218] |
| 102 | External definition, map record 102 | independent | [docs/registry/implementation-map.yaml:L3219-L3231] |
| 103 | External definition, map record 103 | independent | [docs/registry/implementation-map.yaml:L3232-L3241] |
| 104 | External definition, map record 104 | independent | [docs/registry/implementation-map.yaml:L3242-L3251] |
| 105 | External definition, map record 105 | independent | [docs/registry/implementation-map.yaml:L3252-L3264] |
| 106 | External definition, map record 106 | independent | [docs/registry/implementation-map.yaml:L3265-L3274] |
| 107 | External definition, map record 107 | wrapper | [docs/registry/implementation-map.yaml:L3275-L3287] |
| 108 | External definition, map record 108 | independent | [docs/registry/implementation-map.yaml:L3288-L3300] |
| 109 | External definition, map record 109 | independent | [docs/registry/implementation-map.yaml:L3301-L3311] |
| 110 | External definition, map record 110 | independent | [docs/registry/implementation-map.yaml:L3312-L3321] |
| 111 | External definition, map record 111 | independent | [docs/registry/implementation-map.yaml:L3322-L3331] |
| 112 | External definition, map record 112 | independent | [docs/registry/implementation-map.yaml:L3332-L3341] |
| 113 | External definition, map record 113 | independent | [docs/registry/implementation-map.yaml:L3342-L3351] |
| 114 | External definition, map record 114 | independent | [docs/registry/implementation-map.yaml:L3352-L3361] |
| 115 | External definition, map record 115 | caller | [docs/registry/implementation-map.yaml:L3362-L3377] |
| 116 | External definition, map record 116 | independent | [docs/registry/implementation-map.yaml:L3378-L3387] |
| 117 | External definition, map record 117 | independent | [docs/registry/implementation-map.yaml:L3388-L3397] |
| 118 | External definition, map record 118 | independent | [docs/registry/implementation-map.yaml:L3398-L3407] |
| 119 | External definition, map record 119 | independent | [docs/registry/implementation-map.yaml:L3408-L3417] |
| 120 | External definition, map record 120 | caller | [docs/registry/implementation-map.yaml:L3418-L3432] |
| 121 | External definition, map record 121 | independent | [docs/registry/implementation-map.yaml:L3433-L3442] |
| 122 | External definition, map record 122 | independent | [docs/registry/implementation-map.yaml:L3443-L3452] |
| 123 | External definition, map record 123 | independent | [docs/registry/implementation-map.yaml:L3453-L3462] |
| 124 | External definition, map record 124 | independent | [docs/registry/implementation-map.yaml:L3463-L3472] |
| 125 | External definition, map record 125 | independent | [docs/registry/implementation-map.yaml:L3473-L3482] |
| 126 | External definition, map record 126 | independent | [docs/registry/implementation-map.yaml:L3483-L3492] |
| 127 | External definition, map record 127 | independent | [docs/registry/implementation-map.yaml:L3493-L3502] |
| 128 | External definition, map record 128 | independent | [docs/registry/implementation-map.yaml:L3503-L3512] |
| 129 | External definition, map record 129 | independent | [docs/registry/implementation-map.yaml:L3513-L3522] |
| 130 | External definition, map record 130 | independent | [docs/registry/implementation-map.yaml:L3523-L3532] |
| 131 | External definition, map record 131 | independent | [docs/registry/implementation-map.yaml:L3533-L3542] |
| 132 | External definition, map record 132 | copy | [docs/registry/implementation-map.yaml:L3543-L3555] |
| 133 | External definition, map record 133 | independent | [docs/registry/implementation-map.yaml:L3556-L3565] |
| 134 | External definition, map record 134 | independent | [docs/registry/implementation-map.yaml:L3566-L3575] |
| 135 | External definition, map record 135 | independent | [docs/registry/implementation-map.yaml:L3576-L3585] |
| 136 | External definition, map record 136 | independent | [docs/registry/implementation-map.yaml:L3586-L3597] |
| 137 | External definition, map record 137 | independent | [docs/registry/implementation-map.yaml:L3598-L3607] |
| 138 | External definition, map record 138 | independent | [docs/registry/implementation-map.yaml:L3608-L3617] |
| 139 | External definition, map record 139 | independent | [docs/registry/implementation-map.yaml:L3618-L3627] |
| 140 | External definition, map record 140 | independent | [docs/registry/implementation-map.yaml:L3628-L3637] |
| 141 | External definition, map record 141 | independent | [docs/registry/implementation-map.yaml:L3638-L3647] |
| 142 | External definition, map record 142 | independent | [docs/registry/implementation-map.yaml:L3648-L3657] |
| 143 | External definition, map record 143 | independent | [docs/registry/implementation-map.yaml:L3658-L3667] |
| 144 | External definition, map record 144 | independent | [docs/registry/implementation-map.yaml:L3668-L3677] |
| 145 | External definition, map record 145 | independent | [docs/registry/implementation-map.yaml:L3678-L3687] |
| 146 | External definition, map record 146 | independent | [docs/registry/implementation-map.yaml:L3688-L3697] |
| 147 | External definition, map record 147 | independent | [docs/registry/implementation-map.yaml:L3698-L3707] |
| 148 | External definition, map record 148 | independent | [docs/registry/implementation-map.yaml:L3708-L3717] |
| 149 | External definition, map record 149 | independent | [docs/registry/implementation-map.yaml:L3718-L3727] |
| 150 | External definition, map record 150 | independent | [docs/registry/implementation-map.yaml:L3728-L3737] |
| 151 | External definition, map record 151 | independent | [docs/registry/implementation-map.yaml:L3738-L3747] |
| 152 | External definition, map record 152 | independent | [docs/registry/implementation-map.yaml:L3748-L3757] |
| 153 | External definition, map record 153 | independent | [docs/registry/implementation-map.yaml:L3758-L3767] |
| 154 | External definition, map record 154 | independent | [docs/registry/implementation-map.yaml:L3768-L3777] |
| 155 | External definition, map record 155 | independent | [docs/registry/implementation-map.yaml:L3778-L3787] |
| 156 | External definition, map record 156 | independent | [docs/registry/implementation-map.yaml:L3788-L3797] |
| 157 | External definition, map record 157 | independent | [docs/registry/implementation-map.yaml:L3798-L3807] |
| 158 | External definition, map record 158 | independent | [docs/registry/implementation-map.yaml:L3808-L3819] |
| 159 | External definition, map record 159 | independent | [docs/registry/implementation-map.yaml:L3820-L3829] |
| 160 | External definition, map record 160 | independent | [docs/registry/implementation-map.yaml:L3830-L3839] |
| 161 | External definition, map record 161 | independent | [docs/registry/implementation-map.yaml:L3840-L3849] |
| 162 | External definition, map record 162 | independent | [docs/registry/implementation-map.yaml:L3850-L3862] |
| 163 | External definition, map record 163 | copy | [docs/registry/implementation-map.yaml:L3863-L3872] |
| 164 | External definition, map record 164 | wrapper | [docs/registry/implementation-map.yaml:L3873-L3885] |
| 165 | External definition, map record 165 | copy | [docs/registry/implementation-map.yaml:L3886-L3898] |
| 166 | External definition, map record 166 | copy | [docs/registry/implementation-map.yaml:L3899-L3909] |
| 167 | External definition, map record 167 | copy | [docs/registry/implementation-map.yaml:L3910-L3919] |
| 168 | External definition, map record 168 | independent | [docs/registry/implementation-map.yaml:L3920-L3929] |
| 169 | External definition, map record 169 | copy | [docs/registry/implementation-map.yaml:L3930-L3939] |
| 170 | External definition, map record 170 | copy | [docs/registry/implementation-map.yaml:L3940-L3949] |
| 171 | External definition, map record 171 | independent | [docs/registry/implementation-map.yaml:L3950-L3959] |
| 172 | External definition, map record 172 | independent | [docs/registry/implementation-map.yaml:L3960-L3969] |
| 173 | External definition, map record 173 | independent | [docs/registry/implementation-map.yaml:L3970-L3979] |
| 174 | External definition, map record 174 | independent | [docs/registry/implementation-map.yaml:L3980-L3989] |
| 175 | External definition, map record 175 | independent | [docs/registry/implementation-map.yaml:L3990-L3999] |
| 176 | External definition, map record 176 | independent | [docs/registry/implementation-map.yaml:L4000-L4009] |
| 177 | External definition, map record 177 | independent | [docs/registry/implementation-map.yaml:L4010-L4019] |
| 178 | External definition, map record 178 | independent | [docs/registry/implementation-map.yaml:L4020-L4029] |
| 179 | External definition, map record 179 | independent | [docs/registry/implementation-map.yaml:L4030-L4039] |
| 180 | External definition, map record 180 | independent | [docs/registry/implementation-map.yaml:L4040-L4049] |
| 181 | External definition, map record 181 | independent | [docs/registry/implementation-map.yaml:L4050-L4059] |
| 182 | External definition, map record 182 | independent | [docs/registry/implementation-map.yaml:L4060-L4069] |
| 183 | External definition, map record 183 | independent | [docs/registry/implementation-map.yaml:L4070-L4079] |
| 184 | External definition, map record 184 | independent | [docs/registry/implementation-map.yaml:L4080-L4089] |
| 185 | External definition, map record 185 | independent | [docs/registry/implementation-map.yaml:L4090-L4099] |
| 186 | External definition, map record 186 | independent | [docs/registry/implementation-map.yaml:L4100-L4109] |
| 187 | External definition, map record 187 | independent | [docs/registry/implementation-map.yaml:L4110-L4119] |
| 188 | External definition, map record 188 | independent | [docs/registry/implementation-map.yaml:L4120-L4129] |
| 189 | External definition, map record 189 | independent | [docs/registry/implementation-map.yaml:L4130-L4139] |
| 190 | External definition, map record 190 | independent | [docs/registry/implementation-map.yaml:L4140-L4149] |
| 191 | External definition, map record 191 | copy | [docs/registry/implementation-map.yaml:L4150-L4159] |
| 192 | External definition, map record 192 | copy | [docs/registry/implementation-map.yaml:L4160-L4169] |
| 193 | External definition, map record 193 | copy | [docs/registry/implementation-map.yaml:L4170-L4179] |
| 194 | External definition, map record 194 | independent | [docs/registry/implementation-map.yaml:L4180-L4189] |
| 195 | External definition, map record 195 | independent | [docs/registry/implementation-map.yaml:L4190-L4199] |
| 196 | External definition, map record 196 | independent | [docs/registry/implementation-map.yaml:L4200-L4209] |
| 197 | External definition, map record 197 | caller | [docs/registry/implementation-map.yaml:L4210-L4222] |
| 198 | External definition, map record 198 | independent | [docs/registry/implementation-map.yaml:L4223-L4232] |
| 199 | External definition, map record 199 | independent | [docs/registry/implementation-map.yaml:L4233-L4242] |
| 200 | External definition, map record 200 | independent | [docs/registry/implementation-map.yaml:L4243-L4252] |
| 201 | External definition, map record 201 | independent | [docs/registry/implementation-map.yaml:L4253-L4262] |
| 202 | External definition, map record 202 | independent | [docs/registry/implementation-map.yaml:L4263-L4272] |
| 203 | External definition, map record 203 | independent | [docs/registry/implementation-map.yaml:L4273-L4285] |
| 204 | External definition, map record 204 | independent | [docs/registry/implementation-map.yaml:L4286-L4295] |
| 205 | External definition, map record 205 | independent | [docs/registry/implementation-map.yaml:L4296-L4307] |
| 206 | External definition, map record 206 | independent | [docs/registry/implementation-map.yaml:L4308-L4317] |
| 207 | External definition, map record 207 | independent | [docs/registry/implementation-map.yaml:L4318-L4327] |
| 208 | External definition, map record 208 | independent | [docs/registry/implementation-map.yaml:L4328-L4337] |
| 209 | External definition, map record 209 | independent | [docs/registry/implementation-map.yaml:L4338-L4347] |
| 210 | External definition, map record 210 | caller | [docs/registry/implementation-map.yaml:L4348-L4362] |
| 211 | External definition, map record 211 | independent | [docs/registry/implementation-map.yaml:L4363-L4372] |
| 212 | External definition, map record 212 | independent | [docs/registry/implementation-map.yaml:L4373-L4382] |
| 213 | External definition, map record 213 | independent | [docs/registry/implementation-map.yaml:L4383-L4392] |
| 214 | External definition, map record 214 | independent | [docs/registry/implementation-map.yaml:L4393-L4402] |
| 215 | External definition, map record 215 | independent | [docs/registry/implementation-map.yaml:L4403-L4412] |
| 216 | External definition, map record 216 | independent | [docs/registry/implementation-map.yaml:L4413-L4422] |
| 217 | External definition, map record 217 | independent | [docs/registry/implementation-map.yaml:L4423-L4432] |
| 218 | External definition, map record 218 | caller | [docs/registry/implementation-map.yaml:L4433-L4444] |
| 219 | External definition, map record 219 | independent | [docs/registry/implementation-map.yaml:L4445-L4454] |
| 220 | External definition, map record 220 | independent | [docs/registry/implementation-map.yaml:L4455-L4464] |
| 221 | External definition, map record 221 | caller | [docs/registry/implementation-map.yaml:L4465-L4476] |
| 222 | External definition, map record 222 | independent | [docs/registry/implementation-map.yaml:L4477-L4486] |
| 223 | External definition, map record 223 | independent | [docs/registry/implementation-map.yaml:L4487-L4496] |
| 224 | External definition, map record 224 | independent | [docs/registry/implementation-map.yaml:L4497-L4506] |
| 225 | External definition, map record 225 | independent | [docs/registry/implementation-map.yaml:L4507-L4516] |
| 226 | External definition, map record 226 | test_helper | [docs/registry/implementation-map.yaml:L4517-L4526] |
| 227 | External definition, map record 227 | test_helper | [docs/registry/implementation-map.yaml:L4527-L4536] |
| 228 | External definition, map record 228 | test_helper | [docs/registry/implementation-map.yaml:L4537-L4545] |
| 229 | External definition, map record 229 | test_helper | [docs/registry/implementation-map.yaml:L4546-L4557] |
| 230 | External definition, map record 230 | test_helper | [docs/registry/implementation-map.yaml:L4558-L4567] |
| 231 | External definition, map record 231 | test_helper | [docs/registry/implementation-map.yaml:L4568-L4578] |
| 232 | External definition, map record 232 | test_helper | [docs/registry/implementation-map.yaml:L4579-L4589] |
| 233 | External definition, map record 233 | test_helper | [docs/registry/implementation-map.yaml:L4590-L4599] |
| 234 | External definition, map record 234 | test_helper | [docs/registry/implementation-map.yaml:L4600-L4609] |
| 235 | External definition, map record 235 | test_helper | [docs/registry/implementation-map.yaml:L4610-L4623] |
| 236 | External definition, map record 236 | test_helper | [docs/registry/implementation-map.yaml:L4624-L4634] |
| 237 | External definition, map record 237 | test_helper | [docs/registry/implementation-map.yaml:L4635-L4644] |
| 238 | External definition, map record 238 | test_helper | [docs/registry/implementation-map.yaml:L4645-L4654] |
| 239 | External definition, map record 239 | test_helper | [docs/registry/implementation-map.yaml:L4655-L4666] |
| 240 | External definition, map record 240 | test_helper | [docs/registry/implementation-map.yaml:L4667-L4676] |
| 241 | External definition, map record 241 | test_helper | [docs/registry/implementation-map.yaml:L4677-L4686] |
| 242 | External definition, map record 242 | test_helper | [docs/registry/implementation-map.yaml:L4687-L4696] |
| 243 | External definition, map record 243 | test_helper | [docs/registry/implementation-map.yaml:L4697-L4706] |
| 244 | External definition, map record 244 | test_helper | [docs/registry/implementation-map.yaml:L4707-L4716] |
| 245 | External definition, map record 245 | test_helper | [docs/registry/implementation-map.yaml:L4717-L4726] |
| 246 | External definition, map record 246 | test_helper | [docs/registry/implementation-map.yaml:L4727-L4736] |
| 247 | External definition, map record 247 | test_helper | [docs/registry/implementation-map.yaml:L4737-L4746] |
| 248 | External definition, map record 248 | test_helper | [docs/registry/implementation-map.yaml:L4747-L4756] |
| 249 | External definition, map record 249 | test_helper | [docs/registry/implementation-map.yaml:L4757-L4766] |
| 250 | External definition, map record 250 | test_helper | [docs/registry/implementation-map.yaml:L4767-L4776] |
| 251 | External definition, map record 251 | test_helper | [docs/registry/implementation-map.yaml:L4777-L4786] |
| 252 | External definition, map record 252 | test_helper | [docs/registry/implementation-map.yaml:L4787-L4796] |
| 253 | External definition, map record 253 | test_helper | [docs/registry/implementation-map.yaml:L4797-L4806] |
| 254 | External definition, map record 254 | test_helper | [docs/registry/implementation-map.yaml:L4807-L4816] |
| 255 | External definition, map record 255 | test_helper | [docs/registry/implementation-map.yaml:L4817-L4826] |
| 256 | External definition, map record 256 | test_helper | [docs/registry/implementation-map.yaml:L4827-L4836] |
| 257 | External definition, map record 257 | test_helper | [docs/registry/implementation-map.yaml:L4837-L4846] |
| 258 | External definition, map record 258 | test_helper | [docs/registry/implementation-map.yaml:L4847-L4856] |
| 259 | External definition, map record 259 | test_helper | [docs/registry/implementation-map.yaml:L4857-L4865] |
| 260 | External definition, map record 260 | test_helper | [docs/registry/implementation-map.yaml:L4866-L4875] |
| 261 | External definition, map record 261 | test_helper | [docs/registry/implementation-map.yaml:L4876-L4887] |
| 262 | External definition, map record 262 | test_helper | [docs/registry/implementation-map.yaml:L4888-L4897] |
| 263 | External definition, map record 263 | test_helper | [docs/registry/implementation-map.yaml:L4898-L4907] |
| 264 | External definition, map record 264 | test_helper | [docs/registry/implementation-map.yaml:L4908-L4917] |

## 10. Coverage and open items

Source coverage comprises `__init__.py`, `dnv_os_f101.py` and `workflow.py`, all package Python files at the pin; package initialization supplies only its docstring. Both `tests/code_checks/` files were read and run: 17 standalone tests and two router tests, plus the three named durable-workflow cases. [src/digitalmodel/code_checks/__init__.py:L1-L1] [src/digitalmodel/code_checks/dnv_os_f101.py:L1-L362] [src/digitalmodel/code_checks/workflow.py:L1-L297] [tests/code_checks/test_dnv_os_f101.py:L33-L228] [tests/code_checks/test_code_check_workflow.py:L11-L69] [docs/registry/workflows.yaml:L281-L288] [docs/registry/workflows.yaml:L309-L324]

Documentation discovery used `rg -l 'code_checks|code-checks|dnv_os_f101' docs`: six named index/registry files were inspected (`docs/maps/digitalmodel-operator-map.md`, `docs/registry/module-routing.yaml`, `docs/registry/workflows.yaml`, `docs/registry/implementation-map.yaml`, `docs/registry/implementation-map.md`, `docs/archive/model-yaml-feature-inventory-2026-10-08.json`), with workflows found separately by durable-workflow IDs; the generic object-property inventory was also inspected. The object-property and archived YAML inventories describe model serialization fields, not this module's analytical validation. [docs/maps/digitalmodel-operator-map.md:L75-L75] [docs/registry/module-routing.yaml:L481-L486] [docs/registry/workflows.yaml:L281-L324] [docs/registry/implementation-map.md:L18-L18] [docs/archive/model-yaml-feature-inventory-2026-10-08.json:L2721-L2729] [docs/domains/orcaflex/property_routing/object_inventory.yaml:L1018-L1020]

Three discovery files with identifying names are excluded from engineering evidence; codenaming and deciding whether they supply any relevant independent evidence remain issue pending (3). The test docstrings and generic workflow inputs provide the worked-example evidence here. [tests/code_checks/test_dnv_os_f101.py:L1-L9] [examples/workflows/api-rp-2rd-riser/input.yml:L1-L22] [examples/workflows/dnv-os-f201-riser/input.yml:L1-L25] [examples/workflows/api-2sk-mooring/input.yml:L1-L16]

Table 13. Open work; responsible group for every entry is the calculation maintainers, with family quality review required for reference interpretation.

| Item | Scope / unresolved evidence | Tracking |
|---|---|---|
| O1 | Router and delegated-input limitations, every affected sweep site in section 6, unsupported pressure effects, non-finite and boundary behavior | issue pending (1) |
| O2 | Standalone physical guards, implicit units/material assumptions, ovality/degenerate-root behavior and exact-unity documentation | issue pending (2) |
| O3 | UNVERIFIED applicability claim from section 1: geometry, depth and design load envelope have no established independent qualification | issue pending (3) |
| O4 | Licensed clause text not checked; no independent published benchmark for collapse/propagation or delegated checks; no complete signed QA appendix; standalone sidecars absent; proposed review assignment and shared-log entry unconfirmed | issue pending (3); delegated riser citation work #1247 |
| O5 | Three identifying discovery files need codenaming and relevance review; no client-derived values or private sidecar entry is used in this pack | issue pending (3) |
| O6 | Method-family coverage and router classification in the implementation map | issue pending (4) |

The calculation-output citation contract was readable; code still lacks the sidecar fields described in section 3, so output provenance remains open rather than implied by the pack. [src/digitalmodel/code_checks/dnv_os_f101.py:L83-L88] [src/digitalmodel/code_checks/dnv_os_f101.py:L312-L319] [src/digitalmodel/code_checks/workflow.py:L236-L254]
