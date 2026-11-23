# Well workflow adapters — knowledge pack

| Header | Value |
|---|---|
| Module | well, top-level |
| Repository | digitalmodel |
| Package | `src/digitalmodel/well/` — directly contained Python files only |
| Pack revision | 0, 2026-10-16 |
| Code revision | `1dba289c5dc1bc8dc82d72e4bfe02f8dcfc22c23` |
| Analysis family | Engineering calculations; EC; AQG r0 Python calculations |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The adapter runs wellbore design, annular hydraulics, swab/surge, ROP, tubular-envelope and production-casing screens and returns the updated configuration with output locations. [src/digitalmodel/well/workflow.py:L46-L89] [src/digitalmodel/well/workflow.py:L92-L183] [src/digitalmodel/well/workflow.py:L186-L303] [src/digitalmodel/well/workflow.py:L306-L432]

The public package exports drilling calculators and result types; its workflow functions call the drilling and tubular implementations. [src/digitalmodel/well/__init__.py:L14-L42] [src/digitalmodel/well/workflow.py:L13-L20] [src/digitalmodel/well/workflow.py:L318-L328]

Scope excludes implementation internals of `drilling/`, `tubulars/` and `wellpath/`; the engine routes wellpath separately, while routing the six screens through this adapter. [src/digitalmodel/engine.py:L754-L799]

The routing registry records maturity `stable`; this is registry metadata, rather than a claim of independent engineering validation. [docs/registry/module-routing.yaml:L376-L381]

## 2. Inputs and outputs

The adapter takes a mutable configuration mapping; required numeric keys are converted with Python `float` or `int`, without a unit-conversion layer in the adapter. [src/digitalmodel/well/workflow.py:L46-L60] [src/digitalmodel/well/workflow.py:L94-L105] [src/digitalmodel/well/workflow.py:L186-L203] [src/digitalmodel/well/workflow.py:L233-L245]

| Input block | Quantities and units | Range / default | Source |
|---|---|---|---|
| `well_bore_design` | `hole_type`, `objective` categorical; `water_depth_m` metres | Required type and depth; objective defaults to `development`; casing setting depths in the delegate are below mudline/seabed | [src/digitalmodel/well/workflow.py:L48-L60] [src/digitalmodel/well/drilling/well_bore_design.py:L30-L46] |
| `well_hydraulics.section` | Pipe OD/ID and hole diameter in inches; mud weight ppg; flow gal/min; TVD ft; PV cP; YP lbf/100 ft² | All required; delegate requires hole greater than pipe OD, positive mud weight and TVD, nonnegative flow/PV/YP | [src/digitalmodel/well/workflow.py:L94-L105] [src/digitalmodel/well/drilling/hydraulics.py:L31-L48] |
| `swab_surge.section`, `trip`, optional `window` | Geometry inches; TVD ft; mud and window EMW ppg; PV cP; YP lbf/100 ft²; trip speed ft/min | Clinging constant defaults to 0.45 dimensionless; pipe configuration `closed`; trip speed 60 ft/min; absent window omits limit and kick/loss results | [src/digitalmodel/well/workflow.py:L133-L165] [examples/workflows/swab-surge/input.yml:L13-L30] |
| `rop_analysis` | Two model parameter/input mappings; depth ft, WOB klbf, speed rpm, bit diameter inches, overbalance kpsi, wear fraction | Both models required; delegate rejects negative WOB/rpm, nonpositive bit diameter and wear outside [0,1] | [src/digitalmodel/well/workflow.py:L190-L203] [src/digitalmodel/well/drilling/rop_models.py:L68-L100] |
| `tubular_design` | OD/ID inches; yield/burst/collapse stress or pressure psi; design factor and yield-hoop ratio dimensionless; sample count points | Geometry required; sample count 100; design factor 1.0; optional hoop ratio | [src/digitalmodel/well/workflow.py:L233-L252] [src/digitalmodel/well/tubulars/design_envelope.py:L46-L88] |
| `casing_design` | `well` and design-factor mappings; product grade, OD inches, nominal weight lb/ft; optional sour-service H2S ppm, total pressure psia, temperature °F | Well and products required; design factors delegated; optional sour screen defaults well type to `gas` | [src/digitalmodel/well/workflow.py:L330-L353] [src/digitalmodel/well/workflow.py:L396-L403] [src/digitalmodel/well/tubulars/casing.py:L287-L295] |
| Each `outputs` mapping | Directory and summary filename; tubular/casing CSV filename | Relative directory resolves under `_config_dir_path`, otherwise current working directory; absolute paths accepted | [src/digitalmodel/well/workflow.py:L23-L43] [src/digitalmodel/well/workflow.py:L254-L261] [src/digitalmodel/well/workflow.py:L411-L429] |

| Output | Units / representation | Source |
|---|---|---|
| Wellbore `casing_program`, `cost`, `risk`, `summary` | Dataclass mappings; cost USD, potential bbl/day; risk scores dimensionless | [src/digitalmodel/well/workflow.py:L53-L88] [src/digitalmodel/well/drilling/well_bore_design.py:L12-L17] |
| Hydraulics `result` | Velocity ft/min, annular pressure loss psi, ECD ppg, transport ratio dimensionless | [src/digitalmodel/well/workflow.py:L106-L127] |
| Swab/surge `result` | Evaluation mapping plus optional speed limits and Boolean kick/loss flags | [src/digitalmodel/well/workflow.py:L149-L182] [tests/well/test_swab_surge.py:L113-L122] |
| ROP `result` | Both predictions ft/hr; sensitivities ft/hr per klbf and ft/hr per rpm | [src/digitalmodel/well/workflow.py:L205-L227] |
| Tubular CSV and summary | UTF-8 CSV: envelope, index, axial force lbf, differential pressure psi; summary includes geometry and extrema | [src/digitalmodel/well/workflow.py:L254-L303] |
| Casing CSV and summary | UTF-8 CSV: product, ratings psi/lbf, design factors dimensionless, governing depth ft, verdicts; optional sour assessment | [src/digitalmodel/well/workflow.py:L354-L432] |
| All summary files | JSON written as UTF-8 before the returned summary gains its own `summary_json` key; configuration receives directory/file paths | [src/digitalmodel/well/workflow.py:L30-L43] [src/digitalmodel/well/workflow.py:L73-L81] |

Axial force is positive in tension and negative in compression; differential pressure is positive for burst and negative for collapse in the delegated envelope coordinates. [src/digitalmodel/well/tubulars/design_envelope.py:L17-L24]

These adapters do not create spatial mesh coordinates or phase data; their output schemas are scalar/mapping summaries and envelope coordinates. [src/digitalmodel/well/workflow.py:L62-L72] [src/digitalmodel/well/workflow.py:L107-L117] [src/digitalmodel/well/workflow.py:L205-L217] [src/digitalmodel/well/workflow.py:L260-L289]

## 3. Method and governing standards

| Adapter | Implemented route | Reference boundary |
|---|---|---|
| `run_well_bore_design` | `WellDesign.casing_program`, `estimate_cost`, `risk_assessment`, `recommend_hole_type`, `production_potential` | Lookup/decision screen; the delegate attributes its casing data to issue research, so this pack supplies no independent cost or production benchmark. [src/digitalmodel/well/workflow.py:L49-L71] [src/digitalmodel/well/drilling/well_bore_design.py:L8-L22] |
| `run_well_hydraulics` | `WellboreHydraulics` velocity, Bingham pressure loss, ECD and cleaning ratio | Delegate names API RP 13D and a drilling-textbook chapter; no edition or standard clause is supplied here; clause text not checked against the licensed copy. [src/digitalmodel/well/workflow.py:L96-L112] [src/digitalmodel/well/drilling/hydraulics.py:L1-L24] |
| `run_swab_surge` | `SwabSurge.evaluate` and optional `max_safe_trip_speed`; adapter itself compares EMW against the supplied window | Example identifies a clinging-constant/Bingham screening model and names API RP 13D; clause text not checked against the licensed copy. [src/digitalmodel/well/workflow.py:L137-L165] [examples/workflows/swab-surge/input.yml:L3-L7] |
| `run_rop_analysis` | `BourgoineYoungROP.predict`, finite-difference sensitivities and `WarrenROP.predict` | Numeric conversion and serialization surround delegated predictions. [src/digitalmodel/well/workflow.py:L186-L228] |
| `run_tubular_design` | `TubularGeometry`, `design_envelope_points`; CSV serialization and extrema | Delegate names API TR 5C3 / ISO 10400:2011 and a textbook chapter; clause text not checked against the licensed copy. [src/digitalmodel/well/workflow.py:L231-L303] [src/digitalmodel/well/tubulars/design_envelope.py:L26-L30] |
| `run_casing_design` | Build catalog product, external profile, production checks, frac-pressure limit, optional sour-service screen | Adapter names API 5C3, API 5CT and NACE MR0175; delegate identifies NACE MR0175 Table A.3; clause text not checked against the licensed copy. [src/digitalmodel/well/workflow.py:L306-L328] [src/digitalmodel/well/workflow.py:L330-L409] [src/digitalmodel/well/tubulars/casing_design.py:L30-L34] |

UNVERIFIED: API 5CT, API 5C3, API RP 13D and NACE MR0175 editions, clause/table provenance and cross-document edition consistency remain incomplete in the cited code references; no numerical table values are reproduced here; issue pending (1). [src/digitalmodel/well/workflow.py:L315-L316] [src/digitalmodel/well/tubulars/casing.py:L24-L35]

The casing summary emits a reference string rather than a structured `Citation` sidecar; closing the provenance gap is tracked in section 10, issue pending (1). [src/digitalmodel/well/workflow.py:L378-L409]

## 4. Validated worked examples

Validated examples: 0. The six cases below are regression examples; no independent standard annex or published dataset revision is established by these adapter-level assertions. [tests/workflows/test_durable_workflows.py:L1282-L1363] [tests/well/test_swab_surge.py:L113-L122]

Each node ran offline at the code pin: PASS at BASE; AFTER: PASS for the same six nodes. Expected engineering ratings derived from standard tables are referenced by assertion location rather than copied. [tests/workflows/test_durable_workflows.py:L1282-L1363]

| Regression case | Input file | Expected result, unit and tolerance basis | Existing test node | Offline result |
|---|---|---|---|---|
| Casing | `examples/workflows/casing-design/input.yml` | 8 products exactly, 3 passing labels exactly; numerical rating assertions stay in the cited test | `tests/workflows/test_durable_workflows.py::test_workflow_registry[casing-design]` | PASS; [tests/workflows/test_durable_workflows.py:L1282-L1304] |
| Wellbore | `examples/workflows/well-bore-design/input.yml` | 5 casing strings exactly, standard-hole recommendation; cost/potential are regression assertions without an independent source | `tests/workflows/test_durable_workflows.py::test_workflow_registry[well-bore-design]` | PASS; [tests/workflows/test_durable_workflows.py:L1305-L1313] |
| Hydraulics | `examples/workflows/well-hydraulics/input.yml` | Velocity 188.538461538 ft/min, pressure loss 127.087912088 psi, ECD 10.805499789 ppg, transport ratio 1; default pytest.approx tolerance | `tests/workflows/test_durable_workflows.py::test_workflow_registry[well-hydraulics]` | PASS; [tests/workflows/test_durable_workflows.py:L1314-L1319] |
| ROP | `examples/workflows/rop-analysis/input.yml` | Predictions 5.636893157 and 167.096226327 ft/hr; sensitivity assertions in cited test; default pytest.approx tolerance | `tests/workflows/test_durable_workflows.py::test_workflow_registry[rop-analysis]` | PASS; [tests/workflows/test_durable_workflows.py:L1334-L1341] |
| Envelope | `examples/workflows/tubular-design-envelope/input.yml` | 160 rows exactly, 2 envelope names exactly; pressure/force extrema use default pytest.approx tolerance | `tests/workflows/test_durable_workflows.py::test_workflow_registry[tubular-design-envelope]` | PASS; [tests/workflows/test_durable_workflows.py:L1342-L1362] |
| Swab/surge | `examples/workflows/swab-surge/input.yml` | Pressure >0 psi, governing speed >0 ft/min, 0 kick/loss flags; strict inequalities and Boolean equality | `tests/well/test_swab_surge.py::test_swab_surge_workflow_offline` | PASS; [tests/well/test_swab_surge.py:L113-L122] |

Test form: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest tests/well <five registry nodes above> -q -rA -p no:cacheprovider --basetemp=<scratch directory>`, one process, temporary repository copy because the workflow tests write example outputs. BASE and AFTER, each: 357 run, 357 passed, 0 failed, 0 skipped, 0 NOT_RUN; code pin as in header. The complete well directory contributes 352 tests and the registry cases contribute 5 tests. An earlier command naming a nonexistent swab/surge registry node exited during selection with 0 tests run; the actual swab test is in `tests/well/`. [tests/workflows/test_durable_workflows.py:L57-L105] [tests/well/test_swab_surge.py:L113-L122] [docs/registry/workflows.yaml:L941-L997]

## 5. QA gates

AQG r0, section 10, Python calculations supplies G0 inputs/units/citations, G1 imports/tests, G2 hand or published check, G3 and G4 NOT_APPLICABLE; section 9 supplies G5 evidence. The criteria below are proposed repeatable pack checks, rather than a claim of standards certification. (AQG r0, sections 9–10.)

| Gate | Applicability | Check ID and execution | Numeric pass criterion, unit | Evidence |
|---|---|---|---|---|
| G0 | Applicable | Q0: parse each of the six example YAML inputs; check basename/block correspondence, required numeric types and output declarations against the adapter | 6 input mappings accepted; 0 missing required blocks, count | Input audit and adapter signatures; [src/digitalmodel/well/workflow.py:L46-L60] [src/digitalmodel/well/workflow.py:L92-L105] [src/digitalmodel/well/workflow.py:L131-L158] [src/digitalmodel/well/workflow.py:L186-L203] [src/digitalmodel/well/workflow.py:L231-L252] [src/digitalmodel/well/workflow.py:L330-L353] |
| G1 | Applicable | Q1: import Python adapters and run the complete well suite plus the five registry nodes of section 4; target is Python, no external-program load route | 357 passes, 0 failures and 0 skips, tests | pytest output; [tests/workflows/test_durable_workflows.py:L57-L105] [tests/well/test_swab_surge.py:L113-L122] |
| G2 | Applicable | Q2: run `run_well_hydraulics` on its example with output redirected; compare returned annular velocity with volumetric flow divided by annular area, converting gal to in³ and inches to feet | Absolute velocity difference ≤0.1 ft/min; pressure loss >0 psi and ECD > mud weight ppg | Hand continuity calculation using the delegate's documented conversion derivation; this checks velocity independently of the rounded multiplier, not the Bingham constitutive model; [src/digitalmodel/well/drilling/hydraulics.py:L82-L101] [src/digitalmodel/well/workflow.py:L106-L112] |
| G3 | NOT_APPLICABLE | No dynamic time integration in these adapter bodies | 0 dynamic integrations, count | [src/digitalmodel/well/workflow.py:L46-L303] [src/digitalmodel/well/workflow.py:L306-L432] |
| G4 | NOT_APPLICABLE | No diffraction mesh or hydrodynamic coefficient solve in these adapter bodies | 0 diffraction solves, count | [src/digitalmodel/well/workflow.py:L46-L303] [src/digitalmodel/well/workflow.py:L306-L432] |
| G5 | Applicable | Q5: invoke all six adapters in scratch output directories; parse all declared JSON/CSV artifacts and compare persisted calculation identifiers with returned summaries | 6 JSON summaries parse, 2 CSV files parse, 0 missing declared files, count | Artifact audit; self-path and provenance omissions remain findings rather than completeness claims; [src/digitalmodel/well/workflow.py:L30-L43] [src/digitalmodel/well/workflow.py:L254-L267] [src/digitalmodel/well/workflow.py:L411-L432] |

Defined checks: G0=1, G1=1, G2=1, G5=1; total 4. Q0 and Q5 use nominal examples and do not detect malformed input, absent optional assessments or nonfinite results; the failure-mode inventory explicitly marks those missing checks. [src/digitalmodel/well/workflow.py:L133-L165] [src/digitalmodel/well/workflow.py:L396-L409]

## 6. Known failure modes and pitfalls

The sweep covers only the two directly contained Python files. It uses the following commands, then classifies every conversion, default, omission, unit assumption and artifact-write risk by reading the complete function bodies. [src/digitalmodel/well/__init__.py:L1-L42] [src/digitalmodel/well/workflow.py:L1-L432]

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|float\(|int\(|\.get\(' src/digitalmodel/well/*.py
rg -n 'write|open|json|csv|ppg|psi|ft|_m|ratio|constant|default' src/digitalmodel/well/*.py
```

sweep: 94 found

Occurrences below include nominal defaults and internal casts; they are not all confirmed engineering defects. Missing mandatory blocks default to empty mappings and then fail on required-key access; those failures are visible rather than swallowed. No swallowed exception, record-skipping `continue`, error-path empty return, epoch fallback or result cache exists in these two files. [src/digitalmodel/well/__init__.py:L1-L42] [src/digitalmodel/well/workflow.py:L1-L432]

| Occurrence | Kind | Symptom / interpretation | Detection and tracking |
|---|---|---|---|
| S01 | path / write | Absolute output locations are accepted; missing config root uses the current directory; summary writes replace the final file directly. [src/digitalmodel/well/workflow.py:L23-L43] | no detection check exists; issue pending (3) |
| S02 | default / omission | Absent `_config_dir_path` silently selects `Path.cwd()`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L27-L27] | no detection check exists; issue pending (3) |
| S03 | default / omission | Absent `directory` silently selects `default_directory`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L37-L37] | no detection check exists; issue pending (3) |
| S04 | default / omission | Absent `summary_json` silently selects `default_filename`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L39-L39] | no detection check exists; issue pending (3) |
| S05 | JSON | The default JSON encoder accepts nonfinite floats; no finite-result audit is performed before writing. [src/digitalmodel/well/workflow.py:L40-L40] | no detection check exists; issue pending (3) |
| S06 | default / omission | Absent `well_bore_design` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L48-L48] | no detection check exists; issue pending (2) |
| S07 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L51-L51] | no detection check exists; issue pending (2) |
| S08 | default / omission | Absent `objective` silently selects `'development'`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L56-L56] | no detection check exists; issue pending (2) |
| S09 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L58-L58] | no detection check exists; issue pending (2) |
| S10 | serialization | The persisted summary lacks its own summary_json key because the path is attached after the write; this applies to all six adapters. [src/digitalmodel/well/workflow.py:L73-L81] | no detection check exists; issue pending (3) |
| S11 | default / omission | Absent `outputs` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L76-L76] | no detection check exists; issue pending (2) |
| S12 | default / omission | Absent `well_hydraulics` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L94-L94] | no detection check exists; issue pending (2) |
| S13 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L97-L97] | no detection check exists; issue pending (2) |
| S14 | units | Hydraulics fields are passed in field units without conversion or unit metadata validation. [src/digitalmodel/well/workflow.py:L97-L104] | no detection check exists; issue pending (2) |
| S15 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L98-L98] | no detection check exists; issue pending (2) |
| S16 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L99-L99] | no detection check exists; issue pending (2) |
| S17 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L100-L100] | no detection check exists; issue pending (2) |
| S18 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L101-L101] | no detection check exists; issue pending (2) |
| S19 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L102-L102] | no detection check exists; issue pending (2) |
| S20 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L103-L103] | no detection check exists; issue pending (2) |
| S21 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L104-L104] | no detection check exists; issue pending (2) |
| S22 | default / omission | Absent `outputs` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L121-L121] | no detection check exists; issue pending (2) |
| S23 | default / omission | Absent `swab_surge` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L133-L133] | no detection check exists; issue pending (2) |
| S24 | default / omission | Absent `trip` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L135-L135] | no detection check exists; issue pending (2) |
| S25 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L138-L138] | no detection check exists; issue pending (2) |
| S26 | units | Swab/surge uses inch/foot/ppg inputs and compares window EMW directly; alternate units are not detected. [src/digitalmodel/well/workflow.py:L138-L164] | no detection check exists; issue pending (2) |
| S27 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L139-L139] | no detection check exists; issue pending (2) |
| S28 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L140-L140] | no detection check exists; issue pending (2) |
| S29 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L141-L141] | no detection check exists; issue pending (2) |
| S30 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L142-L142] | no detection check exists; issue pending (2) |
| S31 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L143-L143] | no detection check exists; issue pending (2) |
| S32 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L144-L144] | no detection check exists; issue pending (2) |
| S33 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L145-L145] | no detection check exists; issue pending (2) |
| S34 | default / omission | Absent `clinging_constant` silently selects `0.45`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L145-L145] | no detection check exists; issue pending (2) |
| S35 | default / omission | Absent `pipe_config` silently selects `'closed'`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L146-L146] | no detection check exists; issue pending (2) |
| S36 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L149-L149] | no detection check exists; issue pending (2) |
| S37 | default / omission | Absent `trip_speed_ft_min` silently selects `60.0`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L149-L149] | no detection check exists; issue pending (2) |
| S38 | default / omission | Absent `window` selects None; the guarded optional result is omitted or delegated without that input. [src/digitalmodel/well/workflow.py:L153-L153] | no detection check exists; issue pending (2) |
| S39 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L156-L156] | no detection check exists; issue pending (2) |
| S40 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L157-L157] | no detection check exists; issue pending (2) |
| S41 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L161-L161] | no detection check exists; issue pending (2) |
| S42 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L164-L164] | no detection check exists; issue pending (2) |
| S43 | default / omission | Absent `outputs` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L176-L176] | no detection check exists; issue pending (2) |
| S44 | default / omission | Absent `rop_analysis` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L188-L188] | no detection check exists; issue pending (2) |
| S45 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L191-L191] | no detection check exists; issue pending (2) |
| S46 | units | ROP coefficient/input dictionaries are cast without checking dimensional conventions. [src/digitalmodel/well/workflow.py:L191-L203] | no detection check exists; issue pending (2) |
| S47 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L193-L193] | no detection check exists; issue pending (2) |
| S48 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L198-L198] | no detection check exists; issue pending (2) |
| S49 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L199-L199] | no detection check exists; issue pending (2) |
| S50 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L200-L200] | no detection check exists; issue pending (2) |
| S51 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L202-L202] | no detection check exists; issue pending (2) |
| S52 | default / omission | Absent `outputs` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L221-L221] | no detection check exists; issue pending (2) |
| S53 | default / omission | Absent `tubular_design` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L233-L233] | no detection check exists; issue pending (2) |
| S54 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L235-L235] | no detection check exists; issue pending (2) |
| S55 | units | Tubular geometry uses inches and psi; design factors and hoop ratios are untagged dimensionless inputs. [src/digitalmodel/well/workflow.py:L235-L245] | no detection check exists; issue pending (2) |
| S56 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L236-L236] | no detection check exists; issue pending (2) |
| S57 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L237-L237] | no detection check exists; issue pending (2) |
| S58 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L238-L238] | no detection check exists; issue pending (2) |
| S59 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L239-L239] | no detection check exists; issue pending (2) |
| S60 | conversion | External sample-count conversion truncates fractions; separator-formatted strings fail; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L241-L241] | no detection check exists; issue pending (2) |
| S61 | default / omission | Absent `n_points` silently selects `100`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L241-L241] | no detection check exists; issue pending (2) |
| S62 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L242-L242] | no detection check exists; issue pending (2) |
| S63 | default / omission | Absent `design_factor` silently selects `1.0`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L242-L242] | no detection check exists; issue pending (2) |
| S64 | default / omission | Absent `yield_hoop_ratio` selects None; the guarded optional result is omitted or delegated without that input. [src/digitalmodel/well/workflow.py:L243-L243] | no detection check exists; issue pending (2) |
| S65 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L245-L245] | no detection check exists; issue pending (2) |
| S66 | default / omission | Absent `outputs` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L254-L254] | no detection check exists; issue pending (2) |
| S67 | default / omission | Absent `directory` silently selects `'results/tubular_design'`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L255-L255] | no detection check exists; issue pending (3) |
| S68 | default / omission | Absent `envelope_csv` silently selects `'tubular_design_envelope.csv'`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L257-L257] | no detection check exists; issue pending (3) |
| S69 | conversion | Internal result cast for CSV or extrema; not an external-text parser or an error fallback. [src/digitalmodel/well/workflow.py:L266-L266] | Q5 checks artifact parsing; internal representation only |
| S70 | conversion | Internal result cast for CSV or extrema; not an external-text parser or an error fallback. [src/digitalmodel/well/workflow.py:L266-L266] | Q5 checks artifact parsing; internal representation only |
| S71 | conversion | Internal result cast for CSV or extrema; not an external-text parser or an error fallback. [src/digitalmodel/well/workflow.py:L281-L281] | Q5 checks artifact parsing; internal representation only |
| S72 | conversion | Internal result cast for CSV or extrema; not an external-text parser or an error fallback. [src/digitalmodel/well/workflow.py:L282-L282] | Q5 checks artifact parsing; internal representation only |
| S73 | conversion | Internal result cast for CSV or extrema; not an external-text parser or an error fallback. [src/digitalmodel/well/workflow.py:L283-L283] | Q5 checks artifact parsing; internal representation only |
| S74 | conversion | Internal result cast for CSV or extrema; not an external-text parser or an error fallback. [src/digitalmodel/well/workflow.py:L284-L284] | Q5 checks artifact parsing; internal representation only |
| S75 | conversion | Internal result cast for CSV or extrema; not an external-text parser or an error fallback. [src/digitalmodel/well/workflow.py:L285-L285] | Q5 checks artifact parsing; internal representation only |
| S76 | default / omission | Absent `casing_design` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L330-L330] | no detection check exists; issue pending (2) |
| S77 | units | Casing well depths/pressures and catalog product dimensions are delegated without conversion. [src/digitalmodel/well/workflow.py:L331-L353] | no detection check exists; issue pending (2) |
| S78 | default / omission | Absent `design_factors` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L332-L332] | no detection check exists; issue pending (2) |
| S79 | empty input | An empty products iterable reaches a summary write and then the rows[0] CSV header access at lines 425–428; partial output precedes IndexError. [src/digitalmodel/well/workflow.py:L343-L344] | no detection check exists; issue pending (3) |
| S80 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L347-L347] | no detection check exists; issue pending (2) |
| S81 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L348-L348] | no detection check exists; issue pending (2) |
| S82 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L351-L351] | no detection check exists; issue pending (2) |
| S83 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L359-L359] | no detection check exists; issue pending (2) |
| S84 | empty checks | all() gives a passing flag for an empty checks mapping; no explicit nonempty-mode assertion is present here. [src/digitalmodel/well/workflow.py:L367-L367] | no detection check exists; issue pending (2) |
| S85 | null output | An infinite design factor is exported as null rather than an explicit unloaded-mode status. [src/digitalmodel/well/workflow.py:L369-L372] | no detection check exists; issue pending (3) |
| S86 | conversion | Infinite design-factor sentinel becomes JSON null; null does not distinguish an unloaded mode from unavailable data. [src/digitalmodel/well/workflow.py:L371-L371] | no detection check exists; issue pending (3) |
| S87 | default / omission | Absent `sour_service` selects None; the guarded optional result is omitted or delegated without that input. [src/digitalmodel/well/workflow.py:L396-L396] | no detection check exists; issue pending (2) |
| S88 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L399-L399] | no detection check exists; issue pending (2) |
| S89 | units | Sour-service concentration and pressure are cast, while temperature is passed through; ppm, psia and Fahrenheit are assumed. [src/digitalmodel/well/workflow.py:L399-L402] | no detection check exists; issue pending (2) |
| S90 | conversion | External numeric conversion accepts Python numeric syntax, not locale-aware separators; finite-value rejection is absent at this boundary. [src/digitalmodel/well/workflow.py:L400-L400] | no detection check exists; issue pending (2) |
| S91 | default / omission | Absent `well_type` silently selects `'gas'`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L401-L401] | no detection check exists; issue pending (2) |
| S92 | default / omission | Absent `service_temp_f` selects None; the guarded optional result is omitted or delegated without that input. [src/digitalmodel/well/workflow.py:L402-L402] | no detection check exists; issue pending (2) |
| S93 | default / omission | Absent `outputs` silently selects `{}`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L411-L411] | no detection check exists; issue pending (2) |
| S94 | default / omission | Absent `products_csv` silently selects `'casing_design_products.csv'`; supplied-value provenance is not carried into the output. [src/digitalmodel/well/workflow.py:L423-L424] | no detection check exists; issue pending (3) |

Issue references #1290 and #1305 occur in the casing adapter's source note, and #1036 occurs in the swab/surge example; they describe feature origins, not established failure incidents. No incident issue is named for this top-level pack; the new boundary findings use the drafts above. [src/digitalmodel/well/workflow.py:L315-L316] [examples/workflows/swab-surge/input.yml:L3-L7]

## 7. Lessons log

| Date | Lesson | Evidence | Pack change | Shared-log entry |
|---|---|---|---|---|
| 2026-10-16 | Treat adapter snapshots as regression evidence; reserve independent validation for a separately sourced reference | [tests/workflows/test_durable_workflows.py:L1282-L1362] | Sections 3–5 distinguish provenance from regression agreement | Not assigned |
| 2026-10-16 | Audit persisted and returned summaries separately because the self-path is added after serialization | [src/digitalmodel/well/workflow.py:L30-L43] [src/digitalmodel/well/workflow.py:L73-L81] | Section 6 includes the artifact boundary | Not assigned |

## 8. Ownership

Proposed review routing: engineering calculations quality manager; deputy: engineering calculations deputy quality manager; producer: module documentation contributor; escalation: family quality manager for engineering classification and the maintainer for scope or publication decisions. Clearance requires a second provider. (AQG r0, section 11.)

## 9. Dependencies and implementation inventory

The engine supplies the configuration and dispatches the six top-level adapters; the package re-exports drilling result types, while the adapter imports delegated drilling and tubular calculators. [src/digitalmodel/engine.py:L754-L799] [src/digitalmodel/well/__init__.py:L14-L42] [src/digitalmodel/well/workflow.py:L13-L20] [src/digitalmodel/well/workflow.py:L318-L328]

| Subpackage boundary | Relationship | Scope |
|---|---|---|
| `well/drilling/` | Hydraulics, ROP, wellbore and swab/surge implementations | Excluded implementation internals; upstream dependency of top-level adapter; [src/digitalmodel/well/workflow.py:L13-L16] |
| `well/tubulars/` | Geometry/envelopes, casing catalog and design checks | Excluded implementation internals; upstream dependency of top-level adapter; [src/digitalmodel/well/workflow.py:L17-L20] [src/digitalmodel/well/workflow.py:L318-L328] |
| `well/wellpath/` | Separate engine route | Excluded; [src/digitalmodel/engine.py:L788-L791] |
| Python standard library | CSV/JSON, dataclass conversion, paths and typing | Used directly by adapter; [src/digitalmodel/well/workflow.py:L5-L11] |

Implementation-map source: `docs/registry/implementation-map.yaml` on `codex/implmap-work`, ref pin `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. [docs/registry/implementation-map.yaml:L1-L4] The inventory below is computed from that map, with subpackage rows excluded by the scope in the header. [docs/registry/implementation-map.yaml:L345-L26502]

| Map inventory result | Count / disposition |
|---|---|
| Families with an implementation row in a directly contained package Python file | 0 |
| In-scope implementation inventory | Empty |
| Counted families, ranked by in-scope row count and map order | 0 |
| Counted-family grep | NOT_APPLICABLE: no map family has an implementation row in scope |
| Further second implementations in counted families | 0; no counted family |
| A8 | Inventory holds; counted-family search NOT_APPLICABLE |

Map follow-up: add family coverage for the section-3 wellbore lookup/decision screen, annular hydraulics, swab/surge and EMW-window criteria, ROP, tubular envelopes and casing design checks; distinguish delegates from wrappers; issue pending (4). The top-level swab/surge function itself computes kick/loss comparisons, so a future criterion family must account for those definitions. [src/digitalmodel/well/workflow.py:L160-L165]

The map's API grade-table family has a row inside the excluded tubular subpackage; it is not counted here. [docs/registry/implementation-map.yaml:L24471-L24483] The map also retains `_resolve_dir` as an unresolved solver-family name match; its body only resolves a filesystem path, so it is a helper rather than a solver launch. [docs/registry/implementation-map.yaml:L50855-L50868] [src/digitalmodel/well/workflow.py:L23-L27]

The following pinned whole-repository search includes `scripts/` and retains discovery leads for the map follow-up, issue pending (4).

```sh
git grep -n -E 'BourgoineYoungROP|WarrenROP|WellboreHydraulics|SwabSurge|design_envelope_points|check_production_casing|class WellDesign' 1dba289c5dc1bc8dc82d72e4bfe02f8dcfc22c23 -- '*.py'
```

The casing explorer delegates its product checks to the tubular calculation layer, and the durable tests call the engine. [scripts/capabilities/build_casing_design_explorer.py:L150-L162] [tests/workflows/test_durable_workflows.py:L57-L105]

## 10. Coverage and open items

Coverage record at the header pin: the two in-scope Python files were read completely; subordinate folders were enumerated and delegate contracts were read where needed for units, sign conventions and standards routing. [src/digitalmodel/well/__init__.py:L1-L42] [src/digitalmodel/well/workflow.py:L1-L432]

| Coverage / open item | Evidence and tracking | Responsible role |
|---|---|---|
| UNVERIFIED claim from section 3: API 5CT, API 5C3, API RP 13D and NACE MR0175 editions, clause/table provenance and edition consistency; structured citation sidecars and independently sourced adapter benchmarks remain incomplete | issue pending (1); [src/digitalmodel/well/workflow.py:L315-L316] [src/digitalmodel/well/workflow.py:L378-L409] | Engineering calculations maintainer |
| Conversion, default, optional-assessment and unit findings in section 6; malformed-input detection checks remain absent | issue pending (2); [src/digitalmodel/well/workflow.py:L133-L165] [src/digitalmodel/well/workflow.py:L233-L245] | Well module maintainer |
| Output-path policy, direct writes, nonfinite JSON, missing self-path, empty-product and null-status findings | issue pending (3); [src/digitalmodel/well/workflow.py:L23-L43] [src/digitalmodel/well/workflow.py:L369-L372] [src/digitalmodel/well/workflow.py:L411-L432] | Well module maintainer |
| Method-map coverage and classification of wrappers versus criteria | issue pending (4); [src/digitalmodel/well/workflow.py:L13-L20] [src/digitalmodel/well/workflow.py:L160-L165] | Engineering calculations maintainer |

Documentation discovery/read record: `docs/README.md`, `docs/domains/README.md`, `docs/maps/digitalmodel-operator-map.md`, `docs/registry/module-routing.yaml`, `docs/registry/workflows.yaml`, `docs/capability-map/capabilities-ia-spec-1444.md`, `docs/research/ffs-worn-corroded-casing-collapse-burst-screen.md`, `docs/domains/tubulars-casing-rod-validation-2026-06-28.md`, `docs/domains/drilling/well_design.md`, and `docs/domains/drilling/casing_design/casing_design.md`. These are discovery records; numerical validation claims in the historical validation note are not adopted as independent evidence. [docs/domains/tubulars-casing-rod-validation-2026-06-28.md:L55-L68] [docs/maps/digitalmodel-operator-map.md:L72-L72]

The workflow registry supplies casing-design, well-bore-design, well-hydraulics, rop-analysis and tubular-design-envelope examples; wellpath is separately routed and outside scope. The additional swab/surge example has a dedicated test rather than a registry node. [docs/registry/workflows.yaml:L941-L997] [tests/well/test_swab_surge.py:L113-L122] [src/digitalmodel/engine.py:L788-L791]

Test coverage record: `tests/well/` plus the five named registry nodes in section 4; the full well suite is broader than the top-level scope and includes delegate tests. The pack's method claims stay at the adapter boundary; the delegate internals and their standards compliance require separate coverage, issue pending (1). [docs/registry/module-routing.yaml:L376-L381] [tests/workflows/test_durable_workflows.py:L1282-L1362]
