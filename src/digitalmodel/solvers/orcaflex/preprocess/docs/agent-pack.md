# OrcaFlex vessel preprocessing knowledge pack

| Header | Value |
| --- | --- |
| Module | `solvers/orcaflex/preprocess` |
| Repository | `digitalmodel` |
| Package | `src/digitalmodel/solvers/orcaflex/preprocess` |
| Pack revision | 0 |
| Code pin | `b47e42fbe7208c69b28dd436aa91fb129bc385f7` |
| Analysis family | OrcaFlex riser and mooring |
| Quality role | OrcaFlex riser/mooring quality manager |
| Status | DRAFT; independent clearance outstanding |

Repository citations resolve at the code pin, except citations to `docs/registry/implementation-map.yaml`, which resolve at map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; the map declares data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. [docs/registry/implementation-map.yaml:L1-L4]

## 1. Purpose and scope

`LoadVessel.router` selects the literal `aqwa` input route, `aqwa_groups` iterates configured groups, and `aqwa_group_item` imports vessel-type data into an existing OrcaFlex template before saving the model. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L22] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L56-L100]

The method delegates diffraction-data import to `model.ImportVesselTypeData`; it does not implement a hydrodynamic coefficient calculation, statics calculation or dynamics calculation in this package. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L56-L100]

The upstream `OrcaflexPreProcess.router` invokes this helper only when the nested `load_vessel.flag` is true; the helper itself reads a different group location, so a nested-only configuration is insufficient. [src/digitalmodel/solvers/orcaflex/orcaflex_preprocess.py:L12-L25] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L22]

The existing routing registry marks the enclosing `solvers` domain beta; this is domain maturity, not evidence of numerical validation of this helper. [docs/registry/module-routing.yaml:L477-L482] [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L19-L50]

## 2. Inputs and outputs

These tables describe direct dictionary access, rather than a validated schema; missing keys have no local defaults or range checks. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L38] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L58-L89]

| Input | Unit | Allowed range / default | Source and evidence |
| --- | --- | --- | --- |
| `cfg.orcaflex.preprocess.load_vessel.input_prgram` | text | Only literal `aqwa` triggers work; spelling is as implemented; no default | Caller dictionary. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L16] |
| `cfg.preprocess.load_vessel.groups` | groups, count | Iterable; no minimum enforced; no default | Top-level dictionary, unlike the selector location. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L18-L22] |
| CLI `filePath` | path text | One required positional argument, parsed but not used by active import/save statements | Process arguments. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L29-L35] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L81-L97] |
| Group `input_file` | path text | File checked through `is_file_valid_func`; no extension check here | Group and analysis root. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L33-L41] |
| Group `output_template` | path text | File checked through `is_file_valid_func`; loaded by `OrcFxAPI.Model` | Group and analysis root. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L34-L56] |
| Group `output_file` | path text | Joined to analysis root; no extension or containment check here | Group dictionary. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L35-L35] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L94-L97] |
| `cfg.Analysis.analysis_root_folder` | path text | No local default | Caller dictionary. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L37-L44] |
| Group `vt_names` | vessel-type names, count | Iterable; body map for each name; no minimum or uniqueness check here | Group dictionary. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L58-L65] |
| Group `multibodyGroupName` | text | Passed through; no local default | Group dictionary. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L67-L89] |

| Output | Unit | Behaviour / default | Evidence |
| --- | --- | --- | --- |
| Router return | configuration mapping | Returns `cfg`, including when the selector does not match | [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L16] |
| Saved model | program-native model data | `SaveData` at the joined path only if import `success` is true; extension is caller supplied | [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L81-L100] |
| Import messages | text | Logged at warning level regardless of success; false success raises an exception | [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L92-L100] |

UNVERIFIED: Input-file and template unit compatibility, coordinate frame, origin, sign and phase conventions are not established by this helper; no conversion or reconciliation is visible, and no external format specification was checked; issue pending (4). [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L56-L90]

## 3. Method and governing standards

| Definition | Implemented method | Reference / governing basis |
| --- | --- | --- |
| `LoadVessel.__init__` | Empty initializer | No numerical method. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L8-L10] |
| `LoadVessel.router` | Exact string dispatch; otherwise returns configuration | Application control flow. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L16] |
| `LoadVessel.aqwa_groups` | Shallow copy of the top-level preprocessing mapping and sequential group iteration | Application control flow. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L18-L22] |
| `LoadVessel.aqwa_group_item` | Parse CLI, resolve two files, load template with one thread, select AQWA import, build body maps with fixed `Draught1`, request data and save on success | Vendor API delegation, rather than a locally derived formula. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L24-L100] |

The requested categories are structure, load RAOs, stiffness/added mass/damping, displacement RAOs, Newman QTFs and full QTFs; other damping, symmetry and drawing are commented out, and the method token is the literal `lmHaskind`. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L68-L79]

The importer passes `clearExistingData=False`; the separate standalone script passes `True` and requests fewer active data categories, so it is not an equivalent reference output. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L68-L90] [scripts/python/digitalmodel/analysis/AQWAVesselTypeImport.py:L14-L34]

No governing engineering standard, edition, clause, standards-derived constant or `Citation` sidecar is named in this file; no licensed standard text or tabulated value is used in this pack. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L1-L100]

UNVERIFIED: The compatible OrcaFlex/AQWA versions and the vendor meaning and physical applicability of `lmHaskind` are not established by the code; issue pending (4). [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L49-L89]

## 4. Validated worked examples

Validated examples: 0; regression examples with an asserted module output: 0; the only related discovered test replaces the engine with a fixed mapping and comments out its comparison assertions. [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L19-L50]

| Classification / case | Source and input | Expected value, unit and tolerance | Existing test node ID | Offline result at code pin |
| --- | --- | --- | --- | --- |
| Mock smoke case; neither validated nor a numerical regression | Synthetic engine return; input filename supplied by the test, but no input file is opened by the mocked engine. [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L19-L26] [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L44-L50] | No active result assertion, independent value or numerical tolerance; cannot qualify as an engineering example. [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L28-L50] | `tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py::test_process` | NOT_RUN: collection imports the engine before the patch; its import chain reaches module-level `import OrcFxAPI`, prohibited for this offline review. [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L9-L23] [src/digitalmodel/engine.py:L30-L30] [src/digitalmodel/solvers/orcaflex/orcaflex.py:L6-L10] [src/digitalmodel/solvers/orcaflex/all_vars.py:L1-L5] |

The package has no direct helper test in the discovered related file: the active call is to the patched engine rather than `LoadVessel`; direct tests and an independent generic fixture are tracked as issue pending (5). [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L19-L50]

UNVERIFIED: Imported numerical values, units and physical equivalence to independent diffraction data have not been demonstrated by this test; issue pending (5). [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L19-L50]

## 5. QA gates and executable checks

Required family gates are G0, G1, G2, G3 and G5; G4 requires a cited record when vessel data originate in a new diffraction run. These are acceptance requirements, not completed solver results. (AQG r0, sections 3, 8, 9 and 10, “OrcaFlex riser and mooring”.)

The following checks are reproducible source inspections or specified downstream acceptance procedures; a source-inspection FAIL identifies existing code, while NOT_RUN identifies absent execution evidence. No FAIL is represented as numerical clearance. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L100]

| Gate | Applicability | Module-specific route | Check IDs / current evidence |
| --- | --- | --- | --- |
| G0 | Applicable | Inspect `LoadVessel` dictionary access, CLI coupling, file guards and import/save policy | C01-C09; source audit below. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L100] |
| G1 | Applicable | Load the `aqwa_group_item` saved model in the target OrcaFlex version: demo only if its version and load capability qualify, otherwise licensed `Model.LoadData`; AQWA is an input-results format here, not a deck this package writes | C10; NOT_RUN: licensed interface prohibited. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L56-L97] (AQG r0, section 5.) |
| G2 | Applicable to the downstream model using the imported vessel type | Run statics of that complete model and compare independent checks and configuration sweep | C11; NOT_RUN: solver and complete reference configuration required. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L56-L100] (AQG r0, section 6.) |
| G3 | Applicable to the downstream model | Run short dynamics after C11, preserving production settings | C12; NOT_RUN: licensed solver required. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L56-L100] (AQG r0, section 7.) |
| G4 | Conditional; not waived by import success | Cite the new AQWA diffraction run's physical and mesh evidence; use NOT_APPLICABLE only with documented provenance showing the condition does not apply | C13; NOT_RUN: no diffraction provenance record supplied by this helper. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L67-L97] (AQG r0, sections 8 and 10.) |
| G5 | Applicable | Assemble evidence for the saved model and its imported source; bind it to input/template/output fingerprints | C14; NOT_RUN: this helper writes a model and log messages, not the required evidence appendix. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L81-L100] (AQG r0, section 9.) |

All source checks below use the single Python file at the code pin; counts are counts of source sites, not failing test counts. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L1-L100]

| ID / gate | Reproducible check and numeric pass criterion | Evidence / observed result |
| --- | --- | --- |
| C01 / G0 | Compare selector and groups dictionary prefixes in `router` and `aqwa_groups`; require 0 mismatched roots, count | FAIL: 1 mismatch, nested `orcaflex.preprocess` versus top-level `preprocess`. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L22] |
| C02 / G0 | Parse the AST and compare loaded `file_name` names with arguments, imports and assignments; require 0 unresolved references, count | FAIL: 2 unresolved references in the missing-file branches. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L38-L47] |
| C03 / G0 | Count `parse_args` calls in `aqwa_group_item` that read process arguments rather than a passed list; require 0 such calls, count | FAIL: 1 call; the resulting `args` is used only by a commented save statement. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L29-L35] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L94-L97] |
| C04 / G0 | Inspect selector dispatch and both group/name iterations; require 0 unsupported-selector or empty-iterable paths without explicit refusal, count | FAIL: 3 paths: unmatched selector, empty groups, empty `vt_names`. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L22] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L58-L65] |
| C05 / G0 | Inspect API import exception handlers; require 0 broad handlers that replace distinct import failures with a licence-only diagnosis, count | FAIL: 1 broad `except Exception`; it raises, rather than silently continuing. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L49-L54] |
| C06 / G0 | Before import, reconcile 4 convention fields between source and template: unit system, reference frame/origin, DOF/sign convention and phase convention; require 0 missing or mismatched fields, count | NOT_RUN: no convention reconciliation or field values supplied in this method; this is a required input-evidence check. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L56-L90] |
| C07 / G0 | Compare 4 import policy choices with approved input metadata: `Draught1`, `lmHaskind`, `clearExistingData=False`, and the complete requested-category set; require 0 undocumented or mismatched choices, count | NOT_RUN: four fixed choices exist, but no approved policy metadata accompanies this method. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L58-L90] |
| C08 / G0 | Trace success and message branches before `SaveData`; require 0 paths that save with unresolved diagnostic messages, count | FAIL: 1 success-to-save path has no message-disposition predicate; this is a missing guard, not proof that a particular message is erroneous. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L81-L100] |
| C09 / G0 | Trace `output_file_name` construction; require 0 save paths without a containment check against the approved output directory, count | FAIL: 1 joined path is saved without a containment check. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L94-L97] |
| C10 / G1 | Load the saved model in the recorded target OrcaFlex version; require 0 errors, 0 warnings, 0 missing expected vessel types and 0 object-count discrepancies against the template-plus-import manifest, count | NOT_RUN; record program version, route, messages, elapsed time in s and vessel-type names/count. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L56-L65] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L94-L97] (AQG r0, section 5.) |
| C11 / G2 | For the downstream model using each imported type: require 0 unconverged statics, 0 unchecked components, at least 6 offset steps; independent line top tension within 5 %, angle within 1 degree and touchdown distance within 5 % | NOT_RUN; record converged model, component table, views, independent reference and sweep. The importer supplies none of those records. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L81-L100] (AQG r0, section 6, checks G2.1-G2.6.) |
| C12 / G3 | Simulate 1 s after C11; require 0 warnings/errors/non-numeric responses and line tensions within 25 % of static values, with no newly slack/compressed line | NOT_RUN; record end image, time series, warning list and timing in s. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L81-L100] (AQG r0, section 7.) |
| C13 / G4 | For new diffraction input, reconcile imported source metadata and its G4 record: weight/buoyancy residual at most 1 % of weight, volume discrepancy at most 0.5 %, 0 absent mesh/centre/normal checks; document preloads where applicable | NOT_RUN; record diffraction-source fingerprint, hydrostatics/mesh evidence and convention reconciliation; import success alone is insufficient. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L68-L90] (AQG r0, section 8.) |
| C14 / G5 | Recompute source/template/output SHA-256 values and compare appendix records; require 0 fingerprint mismatches and 0 missing applicable gate records, images, dispositions or independent signatures, count | NOT_RUN; retain an appendix linked to the actual saved model, not just log messages. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L81-L100] (AQG r0, sections 9 and 13.) |

The full downstream gates remain open as issue pending (5); missing unit/version/policy evidence remains issue pending (4). (AQG r0, sections 5-10.)

## 6. Known failure modes and pitfalls

sweep: 12 found

The sweep uses the following commands, followed by a complete read of the file and AST control-flow inspection; the twelve rows are reviewed sites, including the exception that rethrows and the normal empty accumulator, rather than twelve demonstrated numerical errors. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L1-L100]

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5\s*%|\b(int|float)\(' src/digitalmodel/solvers/orcaflex/preprocess --glob '*.py'
rg -n 'unit|Units|Draught|clearExistingData|calculationMethods|messages|input_prgram|preprocess|parse_args|bodyMaps|output_file_name' src/digitalmodel/solvers/orcaflex/preprocess --glob '*.py'
```

| Site | Symptom / cause at code pin | Detection check | Tracking / date |
| --- | --- | --- | --- |
| S01: unmatched selector | Returns `cfg` without indicating that import was skipped. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L16] | C04 | issue pending (1); 2026-11-08 |
| S02: group default by empty iterable | An empty `groups` iterable completes without importing a group. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L18-L22] | C04 | issue pending (1); 2026-11-08 |
| S03: broad import exception | All API import exceptions become a licence-unavailable RuntimeError; it does not swallow and continue. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L49-L54] | C05 | issue pending (4); 2026-11-08 |
| S04: empty body-map accumulator | `bodyMaps=[]` is an ordinary accumulator, not an error fallback; empty `vt_names` leaves it empty and still passes it to import without a local guard. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L58-L65] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L81-L89] | C04 | issue pending (1); 2026-11-08 |
| S05: unit assumption | Source and template convention compatibility is delegated with no local reconciliation. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L56-L90] | C06 | issue pending (4); 2026-11-08 |
| S06: fixed draught | Every body map uses `Draught1`, irrespective of source metadata. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L60-L65] | C07 | issue pending (4); 2026-11-08 |
| S07: fixed calculation method | The method token is fixed to `lmHaskind` rather than reconciled with input policy. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L79-L85] | C07 | issue pending (4); 2026-11-08 |
| S08: retaining old data | Existing template data are retained; completeness of replacement is not checked locally. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L81-L90] | C07, C10 | issue pending (4); 2026-11-08 |
| S09: diagnostic disposition | Messages are logged, but a successful import is saved without a message-based acceptance check. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L92-L100] | C08 | issue pending (4); 2026-11-08 |
| S10: omitted other damping | Other-damping import is commented out; source/category completeness needs an explicit policy. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L68-L78] | C07 | issue pending (4); 2026-11-08 |
| S11: omitted symmetry and drawing | Both categories are commented out; downstream assumptions cannot be established by import success. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L72-L78] | C07, C10 | issue pending (4); 2026-11-08 |
| S12: output location assumption | A caller-supplied output name is joined to the root and saved without containment validation. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L94-L97] | C09 | issue pending (4); 2026-11-08 |

No `continue`, explicit `return None`, empty-list/dict error return, numeric 0/0.0/1970/percentage fallback, `int`/`float` conversion of external text or cache write occurs in this file; the empty body-map list is covered by S04. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L1-L100]

| Other confirmed failure mode | Detection check | Tracking / evidence |
| --- | --- | --- |
| Nested selector and top-level groups require inconsistent configuration roots | C01 | issue pending (1). [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L22] |
| Missing input or template references undefined `file_name`, obscuring the intended diagnostic | C02 | issue pending (2). [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L38-L47] |
| Library call parses unrelated process arguments before using group paths | C03 | issue pending (3). [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L29-L38] |
| Related test fabricates the engine result and has no active comparison; it is not evidence that import works | no detection check exists in the existing test; C01-C09 are source checks, not a replacement test suite | issue pending (5). [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L19-L50] |

The map associates #2107 with this helper as `affected: not_checked`; the helper invokes vendor import rather than the separate fixed-window parser, so #2107 is not asserted to be a confirmed defect of this package. [docs/registry/implementation-map.yaml:L12620-L12635] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L81-L90]

## 7. Lessons log

| Date | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- |
| 2026-11-08 | A dispatch selector and its consumed groups need one explicit configuration contract | Nested selector versus top-level groups. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L22] | Input table, C01 and failure row | Not assigned; issue pending (1) |
| 2026-11-08 | A mocked engine result without assertions cannot establish vessel-data conversion correctness | Patch and commented comparison. [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L19-L50] | Smoke case separated from validated/regression examples | Not assigned; issue pending (5) |

## 8. Ownership and escalation

Proposed review responsibilities: OrcaFlex riser/mooring quality manager for this pack; deputy from the same family; diffraction quality manager for G4. Clearance requires a different provider from the producer, and unresolved tolerance, input or interpretation questions go to the family quality manager; cross-family conflicts go to the maintainers. (AQG r0, sections 10 and 11.)

This DRAFT assigns no individual and records no completed independent review; the required evidence remains tracked in section 10.

## 9. Dependencies and implementation inventory

### 9.1 Upstream, downstream and solver references

| Dependency | Role and every source occurrence naming OrcaFlex/AQWA | Version / evidence |
| --- | --- | --- |
| `os`, `argparse`, `loguru.logger` | Path joining, process-argument parsing and logging | Imports and calls; no versions pinned here. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L1-L5] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L29-L31] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L92-L100] |
| `assetutilities.common.utilities.is_file_valid_func` | Resolves and checks input and template paths | External helper; implementation not read as part of this package. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L5-L5] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L37-L47] |
| Upstream `OrcaflexPreProcess` | Flag-controlled caller of `LoadVessel.router` | Outside this package, with its own parent scope. [src/digitalmodel/solvers/orcaflex/orcaflex_preprocess.py:L3-L5] [src/digitalmodel/solvers/orcaflex/orcaflex_preprocess.py:L19-L25] |
| OrcaFlex configuration / AQWA route | Nested `orcaflex` key and `aqwa` selector; `aqwa_groups` / `aqwa_group_item` definitions and calls | Text-only review. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L24] |
| AQWA input documentation / CLI | AQWA import docstring and positional argument help | Text-only review. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L25-L31] |
| OrcFxAPI import / OrcaFlex diagnostic | Local import, broad exception and licence-unavailable text | No interface import or licence query executed. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L49-L54] |
| OrcFxAPI model / AQWA format selector | Template model with `threadCount=1` and `iftAQWA` | Version not recorded in this method. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L56-L57] |
| OrcFxAPI body map | `VesselTypeDataDiffractionImportBodyMap` with `Draught1` | Vendor call read as text only. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L60-L65] |
| OrcFxAPI categories | Six active requested categories; three commented vendor categories | Active and commented names both inventoried. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L68-L78] |
| AQWA import log / failure text | Warning message and two failure messages | Text-only review, no import outcome inferred. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L92-L100] |
| Saved-model consumer | The loaded template is saved after a successful import; upstream router returns configuration | No model execution in the helper. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L16] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L94-L100] |

### 9.2 Map scope and counted-family search

Inventory entries below reproduce map identities and relations, rather than asserting that every discovered definition computes the same result. Both families have one in-package row, so both count, in map-file order: `orcaflex-text-model-writers` first, then `rao-readers-and-orcaflex-converters`; their inventories contain 440 and 295 rows respectively. [docs/registry/implementation-map.yaml:L4933-L4940] [docs/registry/implementation-map.yaml:L9509-L9516]

The in-package row in each family is `LoadVessel.aqwa_group_item`; section 3 also lists every other definition in this package. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L8-L100]

Search procedure: read the package's Python text at the code pin; apply each map `search_signals[].pattern` unchanged with Python `re`, case-sensitive; omit a pattern if it matches `solvers/orcaflex/preprocess` case-insensitively; assign matching lines to their innermost enclosing function/class, with module statements treated as one definition. The exact patterns and outcomes follow.

| Family / signal | Exact Python regular expression | Result |
| --- | --- | --- |
| `orcaflex-text-model-writers` / algorithm | <code>(?:yaml\.(?:dump&#124;safe_dump)&#124;write_text&#124;open\(&#124;\.write\(&#124;save\(&#124;SaveData&#124;dump\().*&#124;to_orcaflex_dict&#124;ObjectType&#124;EndAConnection&#124;LineConfig&#124;VesselConfig&#124;WaveTrains&#124;CurrentRamp&#124;WindSpeed&#124;\.write\(&#124;\.ya?ml&#124;yaml\.safe_dump&#124;shutil\.copy&#124;\.write\(&#124;write_model&#124;ModularModelGenerator&#124;ProjectInputSpec&#124;yaml\.safe_dump&#124;\.write_text</code> | Lines 96,97; `aqwa_group_item` only |
| `orcaflex-text-model-writers` / vocabulary | <code>orcaflex&#124;OrcaFlex&#124;includefile&#124;modular_generator</code> | Omitted: matches package path case-insensitively |
| `orcaflex-text-model-writers` / constants_or_keys | <code>[\"\'](?:Lines&#124;LineTypes&#124;Vessels&#124;6DBuoys&#124;General&#124;IncludeFile&#124;includefile)[\"\']&#124;%YAML</code> | Lines none; 0 definitions |
| `rao-readers-and-orcaflex-converters` / algorithm | <code>\.lis\b&#124;\.hyd\b&#124;\.owr\b&#124;read_rao&#124;parse_rao&#124;read_hyd&#124;read_wamit&#124;read_nemoh&#124;parse_lis&#124;rao_data&#124;rao_table&#124;RAOAmplitude&#124;RAOPhase&#124;DisplacementRAOs&#124;read_csv&#124;read_excel&#124;loadtxt&#124;open_dataset&#124;netCDF4&#124;read_hydrodynamic&#124;parse_aqwa&#124;parse_wamit&#124;parse_nemoh&#124;convert_rao&#124;RAOInterpolator&#124;self\.content&#124;_table_span&#124;parse_aqwa_lis_file&#124;AQWALISParser</code> | Lines 73,96; `aqwa_group_item` only |
| `rao-readers-and-orcaflex-converters` / vocabulary | <code>\bRAO\b&#124;\braos?\b&#124;AQWA&#124;WAMIT&#124;NEMOH&#124;HydroD&#124;BEMRosetta&#124;Capytaine</code> | Lines 26,30,57,92,99,100; `aqwa_group_item` only |
| `rao-readers-and-orcaflex-converters` / constants_or_keys | <code>ADDED MASS&#124;RADIATION DAMPING&#124;displacement_rao&#124;displacementRAO&#124;DisplacementRAOs&#124;RAOPeriodOrFrequency&#124;RAOAmplitude&#124;RAOPhase&#124;RAODirection</code> | Lines 73; `aqwa_group_item` only |

Exact stored patterns (JSON escaping; decode the strings before using Python `re`):

```json
{
  "orcaflex-text-model-writers": [
    "(?:yaml\\.(?:dump|safe_dump)|write_text|open\\(|\\.write\\(|save\\(|SaveData|dump\\().*|to_orcaflex_dict|ObjectType|EndAConnection|LineConfig|VesselConfig|WaveTrains|CurrentRamp|WindSpeed|\\.write\\(|\\.ya?ml|yaml\\.safe_dump|shutil\\.copy|\\.write\\(|write_model|ModularModelGenerator|ProjectInputSpec|yaml\\.safe_dump|\\.write_text",
    "orcaflex|OrcaFlex|includefile|modular_generator",
    "[\\\"\\'](?:Lines|LineTypes|Vessels|6DBuoys|General|IncludeFile|includefile)[\\\"\\']|%YAML"
  ],
  "rao-readers-and-orcaflex-converters": [
    "\\.lis\\b|\\.hyd\\b|\\.owr\\b|read_rao|parse_rao|read_hyd|read_wamit|read_nemoh|parse_lis|rao_data|rao_table|RAOAmplitude|RAOPhase|DisplacementRAOs|read_csv|read_excel|loadtxt|open_dataset|netCDF4|read_hydrodynamic|parse_aqwa|parse_wamit|parse_nemoh|convert_rao|RAOInterpolator|self\\.content|_table_span|parse_aqwa_lis_file|AQWALISParser",
    "\\bRAO\\b|\\braos?\\b|AQWA|WAMIT|NEMOH|HydroD|BEMRosetta|Capytaine",
    "ADDED MASS|RADIATION DAMPING|displacement_rao|displacementRAO|DisplacementRAOs|RAOPeriodOrFrequency|RAOAmplitude|RAOPhase|RAODirection"
  ]
}
```

All retained hits belong to the already listed `LoadVessel.aqwa_group_item`; the commented `SaveData` line is not another implementation. Further unlisted in-package implementations: 0, summed over both counted families. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L24-L100]

Whole-repository search, including `scripts/`, used `rg -n 'ImportVesselTypeData|iftAQWA|VesselTypeDataDiffractionImportBodyMap' --glob '*.py'`; the standalone script is a module-level alternative that creates a new model/type and saves only on import success, whereas the package loads an existing template. [scripts/python/digitalmodel/analysis/AQWAVesselTypeImport.py:L1-L39] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L56-L97]

Map follow-up: reconcile that standalone script's omission from the two family inventories, classify vendor delegation consistently, and codename discovery files with identifying names; issue pending (6). Other pack method families are dispatch, file validation and CLI adaptation in section 3; they have no separate in-package map rows and are not additional counted families. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L54]

### 9.3 Complete map inventory

11 discovery files with identifying names have their path/symbol text withheld; every implementation still has a distinct family-relative row number and exact map citation; issue pending (6).
#### orcaflex-text-model-writers

| Row | Implementation identity | Map relation | Evidence at map pin |
| --- | --- | --- | --- |
| 1 | `docs/domains/examples/orcaflex/run_universal_example.py` — `main` | independent | [docs/registry/implementation-map.yaml:L4940-L4949] |
| 2 | Discovery file 1 with identifying name withheld; implementation 2 | independent | [docs/registry/implementation-map.yaml:L4950-L4958] |
| 3 | `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` — `main` | independent | [docs/registry/implementation-map.yaml:L4959-L4968] |
| 4 | `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` — `main` | independent | [docs/registry/implementation-map.yaml:L4969-L4978] |
| 5 | `docs/domains/orcaflex/risers/drilling/wd9500ft/Preprocessing code.py` — `strengthInputFilePreperation` | independent | [docs/registry/implementation-map.yaml:L4979-L4988] |
| 6 | Discovery file 2 with identifying name withheld; implementation 6 | independent | [docs/registry/implementation-map.yaml:L4989-L4998] |
| 7 | Discovery file 3 with identifying name withheld; implementation 7 | independent | [docs/registry/implementation-map.yaml:L4999-L5008] |
| 8 | Discovery file 3 with identifying name withheld; implementation 8 | independent | [docs/registry/implementation-map.yaml:L5009-L5020] |
| 9 | `docs/domains/orcawave/examples/L02 OC4 Semi-sub/run_orcawave.py` — `_load_data_compat` | independent | [docs/registry/implementation-map.yaml:L5021-L5030] |
| 10 | `docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/run_orcawave.py` — `_load_data_compat` | copy | [docs/registry/implementation-map.yaml:L5031-L5040] |
| 11 | `docs/domains/orcawave/examples/L04 Sectional bodies/run_orcawave.py` — `_load_data_compat` | copy | [docs/registry/implementation-map.yaml:L5041-L5050] |
| 12 | `docs/domains/orcawave/examples/L05 Panel pressures/run_orcawave.py` — `_load_data_compat` | copy | [docs/registry/implementation-map.yaml:L5051-L5060] |
| 13 | `examples/domains/workflows/utils.py` — `OrcaFlexExporter.create_mooring_yml` | example | [docs/registry/implementation-map.yaml:L5061-L5070] |
| 14 | `scripts/audit_spec_library.py` — `write_audit_yaml` | independent | [docs/registry/implementation-map.yaml:L5071-L5080] |
| 15 | `scripts/benchmark/generate_owd_ymls.py` — `generate_yml` | independent | [docs/registry/implementation-map.yaml:L5081-L5090] |
| 16 | `scripts/benchmark/run_spar_benchmark.py` — `main` | independent | [docs/registry/implementation-map.yaml:L5091-L5100] |
| 17 | `scripts/benchmark/solve_owd_extract.py` — `solve_and_extract` | independent | [docs/registry/implementation-map.yaml:L5101-L5110] |
| 18 | `scripts/benchmark/validate_owd_vs_spec.py` — `solve_owd` | independent | [docs/registry/implementation-map.yaml:L5111-L5120] |
| 19 | `scripts/benchmark/validate_owd_vs_spec.py` — `solve_spec` | independent | [docs/registry/implementation-map.yaml:L5121-L5132] |
| 20 | `scripts/benchmark/validate_owd_vs_spec.py` — `_solve_spec_via_orcfxapi` | independent | [docs/registry/implementation-map.yaml:L5133-L5142] |
| 21 | `scripts/benchmark_model_library.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L5143-L5151] |
| 22 | `scripts/benchmark_model_library.py` — `load_and_run` | independent | [docs/registry/implementation-map.yaml:L5152-L5161] |
| 23 | `scripts/benchmark_model_library.py` — `run_spec_driven` | independent | [docs/registry/implementation-map.yaml:L5162-L5171] |
| 24 | `scripts/benchmark_model_library.py` — `load_json` | independent | [docs/registry/implementation-map.yaml:L5172-L5181] |
| 25 | `scripts/benchmark_model_library.py` — `_trace` | independent | [docs/registry/implementation-map.yaml:L5182-L5191] |
| 26 | `scripts/benchmark_riser_library.py` — `apply_mesh_preset` | independent | [docs/registry/implementation-map.yaml:L5192-L5201] |
| 27 | `scripts/benchmark_riser_library.py` — `run_mesh_sensitivity` | independent | [docs/registry/implementation-map.yaml:L5202-L5211] |
| 28 | `scripts/benchmark_riser_library.py` — `benchmark_model` | independent | [docs/registry/implementation-map.yaml:L5212-L5223] |
| 29 | `scripts/build_sme_report.py` — `make_chart_js` | independent | [docs/registry/implementation-map.yaml:L5224-L5233] |
| 30 | `scripts/build_sme_report.py` — `make_profile_js` | independent | [docs/registry/implementation-map.yaml:L5234-L5243] |
| 31 | `scripts/ci/check_include_targets.py` — `extract_references` | independent | [docs/registry/implementation-map.yaml:L5244-L5253] |
| 32 | `scripts/consolidate_panels.py` — `_build_manual_entries` | independent | [docs/registry/implementation-map.yaml:L5254-L5263] |
| 33 | `scripts/convert_orcaflex_to_yml.py` — `OrcaFlexConverter.convert_file` | independent | [docs/registry/implementation-map.yaml:L5264-L5273] |
| 34 | `scripts/create_flattened_models.py` — `create_flattened_model_dir` | independent | [docs/registry/implementation-map.yaml:L5274-L5283] |
| 35 | `scripts/extract_library_validation.py` — `process_model_dir` | independent | [docs/registry/implementation-map.yaml:L5284-L5293] |
| 36 | `scripts/extract_property_inventory.py` — `_field_to_section_name` | independent | [docs/registry/implementation-map.yaml:L5294-L5303] |
| 37 | `scripts/extract_property_inventory.py` — `write_yaml` | independent | [docs/registry/implementation-map.yaml:L5304-L5313] |
| 38 | `scripts/extract_riser_validation.py` — `main` | independent | [docs/registry/implementation-map.yaml:L5314-L5323] |
| 39 | `scripts/extract_s7_specs.py` — `save_spec` | independent | [docs/registry/implementation-map.yaml:L5324-L5333] |
| 40 | `scripts/fix_orcaflex_structure.py` — `create_env_section_wrappers` | independent | [docs/registry/implementation-map.yaml:L5334-L5343] |
| 41 | `scripts/fix_orcaflex_structure.py` — `create_fixed_analysis_model` | independent | [docs/registry/implementation-map.yaml:L5344-L5353] |
| 42 | `scripts/fix_orcaflex_structure.py` — `fix_sections_for_discretised` | independent | [docs/registry/implementation-map.yaml:L5354-L5363] |
| 43 | `scripts/generate_all_specs.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L5364-L5372] |
| 44 | `scripts/generate_all_specs.py` — `extract_spec` | independent | [docs/registry/implementation-map.yaml:L5373-L5382] |
| 45 | `scripts/generate_all_specs.py` — `save_spec_yml` | independent | [docs/registry/implementation-map.yaml:L5383-L5392] |
| 46 | `scripts/generate_analysis_files.py` — `create_analysis_file` | independent | [docs/registry/implementation-map.yaml:L5393-L5402] |
| 47 | `scripts/generate_calm_buoy_project.py` — `CALMBuoyProjectGenerator.generate_orcaflex_modules` | independent | [docs/registry/implementation-map.yaml:L5403-L5415] |
| 48 | `scripts/generate_calm_buoy_project.py` — `CALMBuoyProjectGenerator._populate_environment_module` | independent | [docs/registry/implementation-map.yaml:L5416-L5425] |
| 49 | `scripts/generate_calm_buoy_project.py` — `CALMBuoyProjectGenerator._apply_module_customizations` | independent | [docs/registry/implementation-map.yaml:L5426-L5437] |
| 50 | `scripts/generate_calm_buoy_project.py` — `CALMBuoyProjectGenerator._update_base_file_paths` | independent | [docs/registry/implementation-map.yaml:L5438-L5447] |
| 51 | `scripts/generate_calm_buoy_project.py` — `CALMBuoyProjectGenerator.save_updated_config` | independent | [docs/registry/implementation-map.yaml:L5448-L5457] |
| 52 | `scripts/generate_env_files.py` — `create_wave_file` | independent | [docs/registry/implementation-map.yaml:L5458-L5467] |
| 53 | `scripts/generate_env_files.py` — `create_current_file` | independent | [docs/registry/implementation-map.yaml:L5468-L5477] |
| 54 | `scripts/generate_env_files.py` — `create_wind_file` | independent | [docs/registry/implementation-map.yaml:L5478-L5487] |
| 55 | `scripts/initialize_skill_metadata.py` — `create_version_metadata_block` | independent | [docs/registry/implementation-map.yaml:L5488-L5497] |
| 56 | `scripts/inventory/orcaflex_legacy_inventory.py` — `_report_type` | independent | [docs/registry/implementation-map.yaml:L5498-L5507] |
| 57 | `scripts/inventory/orcaflex_legacy_inventory.py` — `classify` | independent | [docs/registry/implementation-map.yaml:L5508-L5519] |
| 58 | `scripts/inventory/orcaflex_legacy_inventory.py` — `main` | independent | [docs/registry/implementation-map.yaml:L5520-L5531] |
| 59 | `scripts/maintenance/archive_candidates.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L5532-L5540] |
| 60 | `scripts/maintenance/archive_candidates.py` — `main` | independent | [docs/registry/implementation-map.yaml:L5541-L5550] |
| 61 | `scripts/mesh_sensitivity_riser.py` — `apply_mesh_preset` | independent | [docs/registry/implementation-map.yaml:L5551-L5560] |
| 62 | `scripts/orcaflex/check_generic_scr_slwr.py` — `scr_reference` | independent | [docs/registry/implementation-map.yaml:L5561-L5570] |
| 63 | `scripts/orcaflex/check_generic_scr_slwr.py` — `slwr_reference` | independent | [docs/registry/implementation-map.yaml:L5571-L5580] |
| 64 | `scripts/orcaflex/check_generic_scr_slwr.py` — `calculate` | independent | [docs/registry/implementation-map.yaml:L5581-L5593] |
| 65 | `scripts/orcaflex/prepare_slay_campaign.py` — `case_spec` | independent | [docs/registry/implementation-map.yaml:L5594-L5603] |
| 66 | `scripts/orcaflex/prepare_slay_campaign.py` — `prepare_case` | independent | [docs/registry/implementation-map.yaml:L5604-L5615] |
| 67 | `scripts/orcaflex/prepare_slay_campaign.py` — `prepare` | independent | [docs/registry/implementation-map.yaml:L5616-L5628] |
| 68 | `scripts/prepare_mooring_qualification.py` — `_generated_sections` | independent | [docs/registry/implementation-map.yaml:L5629-L5638] |
| 69 | `scripts/python/digitalmodel/doc_tools/check_structure_creation.py` — `ContentAnalyzer._categorize_folder` | independent | [docs/registry/implementation-map.yaml:L5639-L5648] |
| 70 | `scripts/python/digitalmodel/doc_tools/frontmatter_generator.py` — `FrontmatterGenerator.frontmatter_to_yaml` | independent | [docs/registry/implementation-map.yaml:L5649-L5658] |
| 71 | `scripts/python/digitalmodel/legacy/51_-_SerializationYAMLPart1.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L5659-L5667] |
| 72 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` — `make_comparison_loaded_vs_ballast_cxc` | independent | [docs/registry/implementation-map.yaml:L5668-L5677] |
| 73 | Discovery file 4 with identifying name withheld; implementation 73 | independent | [docs/registry/implementation-map.yaml:L5678-L5687] |
| 74 | Discovery file 4 with identifying name withheld; implementation 74 | independent | [docs/registry/implementation-map.yaml:L5688-L5697] |
| 75 | `scripts/python/digitalmodel/orcawave/create_gdf_consistent.py` — `create_config_with_gdf` | independent | [docs/registry/implementation-map.yaml:L5698-L5707] |
| 76 | `scripts/python/digitalmodel/orcawave/create_working_examples.py` — `create_simple_vessel_config` | independent | [docs/registry/implementation-map.yaml:L5708-L5717] |
| 77 | `scripts/python/digitalmodel/orcawave/create_working_examples.py` — `create_oc4_semisub_config` | independent | [docs/registry/implementation-map.yaml:L5718-L5727] |
| 78 | `scripts/python/digitalmodel/orcawave/create_working_examples.py` — `create_minimal_test_config` | independent | [docs/registry/implementation-map.yaml:L5728-L5737] |
| 79 | `scripts/python/digitalmodel/orcawave/final_working_config.py` — `create_final_working_config` | independent | [docs/registry/implementation-map.yaml:L5738-L5749] |
| 80 | `scripts/python/digitalmodel/orcawave/final_working_config.py` — `create_extended_version` | independent | [docs/registry/implementation-map.yaml:L5750-L5759] |
| 81 | Discovery file 5 with identifying name withheld; implementation 81 | independent | [docs/registry/implementation-map.yaml:L5760-L5769] |
| 82 | Discovery file 6 with identifying name withheld; implementation 82 | independent | [docs/registry/implementation-map.yaml:L5770-L5781] |
| 83 | Discovery file 6 with identifying name withheld; implementation 83 | independent | [docs/registry/implementation-map.yaml:L5782-L5791] |
| 84 | `scripts/python/digitalmodel/orcawave/fix_working_model.py` — `fix_working_model` | independent | [docs/registry/implementation-map.yaml:L5792-L5801] |
| 85 | `scripts/python/digitalmodel/orcawave/fix_working_model.py` — `create_minimal_with_test_box` | independent | [docs/registry/implementation-map.yaml:L5802-L5811] |
| 86 | `scripts/python/digitalmodel/orcawave/validate_orcawave_configs.py` — `OrcaWaveValidator.run_validation` | independent | [docs/registry/implementation-map.yaml:L5812-L5823] |
| 87 | `scripts/python/digitalmodel/tools/add_orcaflex_test_configs.py` — `create_batch_config` | independent | [docs/registry/implementation-map.yaml:L5824-L5833] |
| 88 | `scripts/python/digitalmodel/tools/standardize_orcaflex_tests.py` — `OrcaFlexTestStandardizer.create_standard_config` | independent | [docs/registry/implementation-map.yaml:L5834-L5843] |
| 89 | `scripts/run_riser_analysis.py` — `run_library` | independent | [docs/registry/implementation-map.yaml:L5844-L5853] |
| 90 | `scripts/run_semisub_quality_check.py` — `main` | independent | [docs/registry/implementation-map.yaml:L5854-L5863] |
| 91 | `scripts/sanitize_s7_models.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L5864-L5872] |
| 92 | `scripts/sanitize_s7_models.py` — `process_dat_file` | independent | [docs/registry/implementation-map.yaml:L5873-L5882] |
| 93 | `scripts/semantic_validate.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L5883-L5891] |
| 94 | `scripts/sync_monolithic_yml.py` — `convert_dat_to_yml` | independent | [docs/registry/implementation-map.yaml:L5892-L5901] |
| 95 | Discovery file 7 with identifying name withheld; implementation 95 | independent | [docs/registry/implementation-map.yaml:L5902-L5911] |
| 96 | Discovery file 8 with identifying name withheld; implementation 96 | independent | [docs/registry/implementation-map.yaml:L5912-L5921] |
| 97 | `scripts/update_skill_versions.py` — `update_skill_file` | independent | [docs/registry/implementation-map.yaml:L5922-L5933] |
| 98 | `scripts/validate_orcaflex_model.py` — `ModelValidator.extract_model_info` | independent | [docs/registry/implementation-map.yaml:L5934-L5943] |
| 99 | `scripts/verification/verify_orcaflex_campaign.py` — `_save_body` | independent | [docs/registry/implementation-map.yaml:L5944-L5953] |
| 100 | `scripts/verification/verify_orcaflex_campaign.py` — `main` | independent | [docs/registry/implementation-map.yaml:L5954-L5965] |
| 101 | `src/digitalmodel/asset_integrity/common/data.py` — `SaveData.saveDataYaml` | independent | [docs/registry/implementation-map.yaml:L5966-L5975] |
| 102 | `src/digitalmodel/asset_integrity/common/data.py` — `SaveData.WriteOrcaFlexYMLFile` | independent | [docs/registry/implementation-map.yaml:L5976-L5985] |
| 103 | `src/digitalmodel/asset_integrity/common/data.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L5986-L5994] |
| 104 | `src/digitalmodel/data_systems/data_procurement/common/stream_handler.py` — `StreamHandler._format_orcaflex_yml` | independent | [docs/registry/implementation-map.yaml:L5995-L6004] |
| 105 | `src/digitalmodel/data_systems/data_procurement/mooring/client.py` — `MooringClient.to_orcaflex_mooring` | independent | [docs/registry/implementation-map.yaml:L6005-L6014] |
| 106 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/chain_db_client.py` — `ChainDatabaseClient.to_orcaflex_line_type` | independent | [docs/registry/implementation-map.yaml:L6015-L6024] |
| 107 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` — `SyntheticRopeDatabaseClient.to_orcaflex_line_type` | independent | [docs/registry/implementation-map.yaml:L6025-L6034] |
| 108 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/wire_rope_db_client.py` — `WireRopeDatabaseClient.to_orcaflex_line_type` | independent | [docs/registry/implementation-map.yaml:L6035-L6044] |
| 109 | `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` — `PipeSpecificationClient.to_orcaflex_line_type` | independent | [docs/registry/implementation-map.yaml:L6045-L6054] |
| 110 | `src/digitalmodel/data_systems/data_procurement/vessel/client.py` — `VesselClient.to_orcaflex_vessel` | independent | [docs/registry/implementation-map.yaml:L6055-L6064] |
| 111 | `src/digitalmodel/drilling_riser/campaign.py` — `case_spec` | independent | [docs/registry/implementation-map.yaml:L6065-L6074] |
| 112 | `src/digitalmodel/drilling_riser/campaign.py` — `_probe` | independent | [docs/registry/implementation-map.yaml:L6075-L6084] |
| 113 | `src/digitalmodel/drilling_riser/campaign.py` — `_capped_probe` | caller | [docs/registry/implementation-map.yaml:L6085-L6096] |
| 114 | `src/digitalmodel/drilling_riser/campaign.py` — `_statics_path` | independent | [docs/registry/implementation-map.yaml:L6097-L6109] |
| 115 | `src/digitalmodel/drilling_riser/campaign.py` — `physical_state_checks` | independent | [docs/registry/implementation-map.yaml:L6110-L6119] |
| 116 | `src/digitalmodel/drilling_riser/global_model/build.py` — `_line_type` | independent | [docs/registry/implementation-map.yaml:L6120-L6129] |
| 117 | `src/digitalmodel/drilling_riser/global_model/build.py` — `build_generic_spec` | independent | [docs/registry/implementation-map.yaml:L6130-L6141] |
| 118 | `src/digitalmodel/drilling_riser/global_model/build.py` — `write_model` | caller | [docs/registry/implementation-map.yaml:L6142-L6153] |
| 119 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` — `reference_periods` | independent | [docs/registry/implementation-map.yaml:L6154-L6163] |
| 120 | `src/digitalmodel/drilling_riser/global_model/open_water.py` — `build_open_water_generic_spec` | independent | [docs/registry/implementation-map.yaml:L6164-L6173] |
| 121 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py` — `extract` | independent | [docs/registry/implementation-map.yaml:L6174-L6183] |
| 122 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py` — `merge_stress_range` | independent | [docs/registry/implementation-map.yaml:L6184-L6193] |
| 123 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py` — `extract_significant` | independent | [docs/registry/implementation-map.yaml:L6194-L6203] |
| 124 | `src/digitalmodel/floating_wind/solver_tier.py` — `prepare_variant` | independent | [docs/registry/implementation-map.yaml:L6204-L6213] |
| 125 | `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` — `OrcaFlexConverter.convert_to_vessel_yaml` | independent | [docs/registry/implementation-map.yaml:L6214-L6223] |
| 126 | `src/digitalmodel/hydrodynamics/diffraction/benchmark_mesh_schematic.py` — `_add_fdf_surface_traces` | independent | [docs/registry/implementation-map.yaml:L6224-L6233] |
| 127 | `src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py` — `run_aqwa` | independent | [docs/registry/implementation-map.yaml:L6234-L6243] |
| 128 | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` — `DiffractionSpec.to_yaml` | independent | [docs/registry/implementation-map.yaml:L6244-L6253] |
| 129 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` — `OrcaFlexExporter.export_vessel_type` | independent | [docs/registry/implementation-map.yaml:L6254-L6263] |
| 130 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` — `_write_yml` | independent | [docs/registry/implementation-map.yaml:L6264-L6273] |
| 131 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` — `OrcaWaveRunner._execute_via_api` | independent | [docs/registry/implementation-map.yaml:L6274-L6283] |
| 132 | `src/digitalmodel/hydrodynamics/hull_library/panel_catalog.py` — `PanelCatalog.to_yaml` | independent | [docs/registry/implementation-map.yaml:L6284-L6293] |
| 133 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` — `RAOAnalysis.write_filtered_orcaflex_seastate_rao_file` | independent | [docs/registry/implementation-map.yaml:L6294-L6303] |
| 134 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` — `RAOAnalysis.get_shape_data` | independent | [docs/registry/implementation-map.yaml:L6304-L6313] |
| 135 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.get_viv_modes_excited` | independent | [docs/registry/implementation-map.yaml:L6314-L6323] |
| 136 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.tabulate_viv_results` | independent | [docs/registry/implementation-map.yaml:L6324-L6333] |
| 137 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.tabulate_overall_viv_results` | independent | [docs/registry/implementation-map.yaml:L6334-L6343] |
| 138 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.save_viv_histograms` | independent | [docs/registry/implementation-map.yaml:L6344-L6353] |
| 139 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.save_combined_histograms` | independent | [docs/registry/implementation-map.yaml:L6354-L6363] |
| 140 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.save_fatigue_summary_result` | independent | [docs/registry/implementation-map.yaml:L6364-L6373] |
| 141 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L6374-L6383] |
| 142 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` — `DNVRPH103_hydrodynamics_circular.save_model` | independent | [docs/registry/implementation-map.yaml:L6384-L6393] |
| 143 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` — `DNVRPH103_hydrodynamics_circular.update_6d_buoy` | independent | [docs/registry/implementation-map.yaml:L6394-L6403] |
| 144 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` — `DNVRPH103_hydrodynamics_rectangular.save_model` | copy | [docs/registry/implementation-map.yaml:L6404-L6413] |
| 145 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` — `DNVRPH103_hydrodynamics_rectangular.update_6d_buoy` | independent | [docs/registry/implementation-map.yaml:L6414-L6423] |
| 146 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.orcaflex_model` | independent | [docs/registry/implementation-map.yaml:L6424-L6436] |
| 147 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.riser_joints_to_line_types` | independent | [docs/registry/implementation-map.yaml:L6437-L6448] |
| 148 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.GetGeneral` | independent | [docs/registry/implementation-map.yaml:L6449-L6458] |
| 149 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.lineTypes` | independent | [docs/registry/implementation-map.yaml:L6459-L6468] |
| 150 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.WriteOrcaflexModel` | independent | [docs/registry/implementation-map.yaml:L6469-L6478] |
| 151 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.build_model` | independent | [docs/registry/implementation-map.yaml:L6479-L6490] |
| 152 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.AssignGeneral` | independent | [docs/registry/implementation-map.yaml:L6491-L6500] |
| 153 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.AssignBuoyProperties` | independent | [docs/registry/implementation-map.yaml:L6501-L6510] |
| 154 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.AssignLineProperties` | independent | [docs/registry/implementation-map.yaml:L6511-L6520] |
| 155 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.AssignLineTypeProperties` | independent | [docs/registry/implementation-map.yaml:L6521-L6530] |
| 156 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.AssignVesselProperties` | independent | [docs/registry/implementation-map.yaml:L6531-L6540] |
| 157 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.save_output` | independent | [docs/registry/implementation-map.yaml:L6541-L6550] |
| 158 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.load_variations` | independent | [docs/registry/implementation-map.yaml:L6551-L6560] |
| 159 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.get_viv_modes_excited` | independent | [docs/registry/implementation-map.yaml:L6561-L6570] |
| 160 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.tabulate_viv_results` | independent | [docs/registry/implementation-map.yaml:L6571-L6580] |
| 161 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.tabulate_overall_viv_results` | independent | [docs/registry/implementation-map.yaml:L6581-L6590] |
| 162 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.save_viv_histograms` | copy | [docs/registry/implementation-map.yaml:L6591-L6600] |
| 163 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.save_combined_histograms` | copy | [docs/registry/implementation-map.yaml:L6601-L6610] |
| 164 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | independent | [docs/registry/implementation-map.yaml:L6611-L6620] |
| 165 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L6621-L6630] |
| 166 | `src/digitalmodel/infrastructure/utils/data.py` — `SaveData.saveDataYaml` | copy | [docs/registry/implementation-map.yaml:L6631-L6640] |
| 167 | `src/digitalmodel/infrastructure/utils/data.py` — `SaveData.WriteOrcaFlexYMLFile` | copy | [docs/registry/implementation-map.yaml:L6641-L6650] |
| 168 | `src/digitalmodel/infrastructure/utils/data.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L6651-L6659] |
| 169 | `src/digitalmodel/marine_ops/installation/jumper_input.py` — `generate_input` | independent | [docs/registry/implementation-map.yaml:L6660-L6672] |
| 170 | `src/digitalmodel/marine_ops/installation/jumper_installation_schema.py` — `JumperInstallationSpec.to_yaml` | independent | [docs/registry/implementation-map.yaml:L6673-L6682] |
| 171 | `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py` — `build_line_types` | independent | [docs/registry/implementation-map.yaml:L6683-L6692] |
| 172 | `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py` — `build_modular_spec` | independent | [docs/registry/implementation-map.yaml:L6693-L6704] |
| 173 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` — `UnifiedRAOReader._detect_format` | independent | [docs/registry/implementation-map.yaml:L6705-L6714] |
| 174 | `src/digitalmodel/orcaflex/model_builder.py` — `VesselConfig.to_orcaflex_dict` | independent | [docs/registry/implementation-map.yaml:L6715-L6724] |
| 175 | `src/digitalmodel/orcaflex/model_builder.py` — `Buoy6DConfig.to_orcaflex_dict` | independent | [docs/registry/implementation-map.yaml:L6725-L6734] |
| 176 | `src/digitalmodel/orcaflex/model_builder.py` — `LineConfig.to_orcaflex_dict` | independent | [docs/registry/implementation-map.yaml:L6735-L6744] |
| 177 | `src/digitalmodel/orcaflex/model_builder.py` — `build_scr_model` | independent | [docs/registry/implementation-map.yaml:L6745-L6754] |
| 178 | `src/digitalmodel/orcaflex/model_builder.py` — `build_lazy_wave_model` | independent | [docs/registry/implementation-map.yaml:L6755-L6764] |
| 179 | `src/digitalmodel/orcaflex/model_builder.py` — `build_mooring_line` | independent | [docs/registry/implementation-map.yaml:L6765-L6774] |
| 180 | `src/digitalmodel/orcaflex/reporting/sections/model_summary.py` — `build_model_summary` | independent | [docs/registry/implementation-map.yaml:L6775-L6784] |
| 181 | `src/digitalmodel/orcaflex/reporting/sections/range_graphs.py` — `build_range_graphs` | independent | [docs/registry/implementation-map.yaml:L6785-L6794] |
| 182 | `src/digitalmodel/orcawave/reporting/sections/rao_plots.py` — `build_rao_plots` | independent | [docs/registry/implementation-map.yaml:L6795-L6804] |
| 183 | `src/digitalmodel/parametric/query.py` — `_write_result` | independent | [docs/registry/implementation-map.yaml:L6805-L6814] |
| 184 | `src/digitalmodel/parametrics/pilots.py` — `orcawave_depth_sweep.runner` | independent | [docs/registry/implementation-map.yaml:L6815-L6824] |
| 185 | `src/digitalmodel/solvers/benchmark/solvers.py` — `_write_diffraction_inputs` | independent | [docs/registry/implementation-map.yaml:L6825-L6834] |
| 186 | `src/digitalmodel/solvers/fea_model/LineType_components.py` — `LineType.get_general` | independent | [docs/registry/implementation-map.yaml:L6835-L6844] |
| 187 | `src/digitalmodel/solvers/fea_model/general_components.py` — `General.getGeneralOrcaFlexSetUp` | independent | [docs/registry/implementation-map.yaml:L6845-L6854] |
| 188 | `src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py` — `load_csv` | independent | [docs/registry/implementation-map.yaml:L6855-L6864] |
| 189 | `src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py` — `get_data` | independent | [docs/registry/implementation-map.yaml:L6865-L6874] |
| 190 | `src/digitalmodel/solvers/orcaflex/core/configuration.py` — `OrcaFlexConfig.to_yaml` | independent | [docs/registry/implementation-map.yaml:L6875-L6884] |
| 191 | `src/digitalmodel/solvers/orcaflex/core/model_interface.py` — `OrcaFlexModelWrapper.save_simulation` | independent | [docs/registry/implementation-map.yaml:L6885-L6894] |
| 192 | `src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py` — `FeatureAnalyzer.analyze_file` | independent | [docs/registry/implementation-map.yaml:L6895-L6904] |
| 193 | `src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py` — `FeatureAnalyzer.generate_summary` | independent | [docs/registry/implementation-map.yaml:L6905-L6914] |
| 194 | `src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py` — `OrcaFlexBatchConverter._real_convert` | wrapper | [docs/registry/implementation-map.yaml:L6915-L6927] |
| 195 | `src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py` — `OrcaFlexBatchConverter._mock_convert` | independent | [docs/registry/implementation-map.yaml:L6928-L6937] |
| 196 | `src/digitalmodel/solvers/orcaflex/examples_integration/converter.py` — `YamlConverter.convert_batch` | independent | [docs/registry/implementation-map.yaml:L6938-L6947] |
| 197 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py` — `DownloadedExamplesIntegrator._infer_components` | independent | [docs/registry/implementation-map.yaml:L6948-L6957] |
| 198 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py` — `DownloadedExamplesIntegrator._extract_common_configs` | independent | [docs/registry/implementation-map.yaml:L6958-L6967] |
| 199 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` — `KnowledgeIntegrator._extract_patterns` | independent | [docs/registry/implementation-map.yaml:L6968-L6977] |
| 200 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` — `KnowledgeIntegrator.build_searchable_index` | independent | [docs/registry/implementation-map.yaml:L6978-L6987] |
| 201 | `src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py` — `OrcFxAPIConverter._initialize_api` | independent | [docs/registry/implementation-map.yaml:L6988-L6997] |
| 202 | `src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py` — `OrcFxAPIConverter.convert_file` | independent | [docs/registry/implementation-map.yaml:L6998-L7007] |
| 203 | `src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py` — `detect_format` | independent | [docs/registry/implementation-map.yaml:L7008-L7017] |
| 204 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter.convert` | independent | [docs/registry/implementation-map.yaml:L7018-L7031] |
| 205 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._merge_master` | independent | [docs/registry/implementation-map.yaml:L7032-L7041] |
| 206 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._extract_includefiles.traverse` | independent | [docs/registry/implementation-map.yaml:L7042-L7051] |
| 207 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` — `ModularToSpecConverter.convert` | independent | [docs/registry/implementation-map.yaml:L7052-L7061] |
| 208 | `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._write_master_file` | independent | [docs/registry/implementation-map.yaml:L7062-L7071] |
| 209 | `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._write_parameters_file` | independent | [docs/registry/implementation-map.yaml:L7072-L7081] |
| 210 | `src/digitalmodel/solvers/orcaflex/inverse_resolver.py` — `_resolve_mooring` | independent | [docs/registry/implementation-map.yaml:L7082-L7091] |
| 211 | `src/digitalmodel/solvers/orcaflex/inverse_resolver.py` — `_resolve_riser` | independent | [docs/registry/implementation-map.yaml:L7092-L7101] |
| 212 | `src/digitalmodel/solvers/orcaflex/library_generator.py` — `LibraryGenerator.generate_component` | independent | [docs/registry/implementation-map.yaml:L7102-L7111] |
| 213 | `src/digitalmodel/solvers/orcaflex/library_generator.py` — `LibraryGenerator.write_index` | independent | [docs/registry/implementation-map.yaml:L7112-L7121] |
| 214 | `src/digitalmodel/solvers/orcaflex/model_catalogue_render.py` — `object_counts` | independent | [docs/registry/implementation-map.yaml:L7122-L7131] |
| 215 | `src/digitalmodel/solvers/orcaflex/model_catalogue_render.py` — `geometry` | independent | [docs/registry/implementation-map.yaml:L7132-L7141] |
| 216 | `src/digitalmodel/solvers/orcaflex/model_generator/__init__.py` — `OrcaFlexModelGenerator.generate_from_template` | independent | [docs/registry/implementation-map.yaml:L7142-L7153] |
| 217 | `src/digitalmodel/solvers/orcaflex/model_generator/__init__.py` — `OrcaFlexModelGenerator._assemble_model` | independent | [docs/registry/implementation-map.yaml:L7154-L7163] |
| 218 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` — `ModularModelGenerator.generate` | independent | [docs/registry/implementation-map.yaml:L7164-L7176] |
| 219 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` — `ModularModelGenerator._write_parameters` | independent | [docs/registry/implementation-map.yaml:L7177-L7186] |
| 220 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` — `ModularModelGenerator._write_master` | independent | [docs/registry/implementation-map.yaml:L7187-L7196] |
| 221 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` — `ModularModelGenerator.generate_with_overrides` | independent | [docs/registry/implementation-map.yaml:L7197-L7207] |
| 222 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/general_builder.py` — `GeneralBuilder.build` | independent | [docs/registry/implementation-map.yaml:L7208-L7217] |
| 223 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py` — `GenericModelBuilder.build` | independent | [docs/registry/implementation-map.yaml:L7218-L7227] |
| 224 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py` — `LinesBuilder.build` | independent | [docs/registry/implementation-map.yaml:L7228-L7237] |
| 225 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` — `LineTypeBuilder.build` | independent | [docs/registry/implementation-map.yaml:L7238-L7247] |
| 226 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_lines_builder.py` — `RiserLinesBuilder.build` | independent | [docs/registry/implementation-map.yaml:L7248-L7257] |
| 227 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py` — `RiserLineTypeBuilder.build` | caller | [docs/registry/implementation-map.yaml:L7258-L7269] |
| 228 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py` — `RiserLineTypeBuilder._build_riser_line_type` | independent | [docs/registry/implementation-map.yaml:L7270-L7279] |
| 229 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_vessel_builder.py` — `RiserVesselBuilder.build` | independent | [docs/registry/implementation-map.yaml:L7280-L7289] |
| 230 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py` — `VesselBuilder.build` | independent | [docs/registry/implementation-map.yaml:L7290-L7299] |
| 231 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` — `WinchBuilder.build` | independent | [docs/registry/implementation-map.yaml:L7300-L7309] |
| 232 | `src/digitalmodel/solvers/orcaflex/modular_generator/cli.py` — `cmd_generate` | independent | [docs/registry/implementation-map.yaml:L7310-L7319] |
| 233 | `src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py` — `MonolithicExtractor._extract_simulation` | independent | [docs/registry/implementation-map.yaml:L7320-L7329] |
| 234 | `src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py` — `MonolithicExtractor._extract_general_properties` | independent | [docs/registry/implementation-map.yaml:L7330-L7339] |
| 235 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` — `PostGenerationValidator._check_line_type_references` | independent | [docs/registry/implementation-map.yaml:L7340-L7349] |
| 236 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` — `PostGenerationValidator._check_vessel_references` | independent | [docs/registry/implementation-map.yaml:L7350-L7359] |
| 237 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` — `PostGenerationValidator._check_winch_references` | independent | [docs/registry/implementation-map.yaml:L7360-L7369] |
| 238 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` — `_iter_includefile_refs.traverse` | independent | [docs/registry/implementation-map.yaml:L7370-L7379] |
| 239 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` — `MooringRouter.route` | independent | [docs/registry/implementation-map.yaml:L7380-L7389] |
| 240 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` — `MooringRouter._chain_linetype` | independent | [docs/registry/implementation-map.yaml:L7390-L7399] |
| 241 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` — `MooringRouter._wire_linetype` | independent | [docs/registry/implementation-map.yaml:L7400-L7409] |
| 242 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` — `MooringRouter._polyester_linetype` | independent | [docs/registry/implementation-map.yaml:L7410-L7419] |
| 243 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py` — `VesselRouter.route` | independent | [docs/registry/implementation-map.yaml:L7420-L7429] |
| 244 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py` — `VesselRouter.from_hull_catalog` | independent | [docs/registry/implementation-map.yaml:L7430-L7439] |
| 245 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` — `CampaignGenerator.generate` | independent | [docs/registry/implementation-map.yaml:L7440-L7449] |
| 246 | `src/digitalmodel/solvers/orcaflex/modular_generator/writers/basefile.py` — `write_variation_model` | independent | [docs/registry/implementation-map.yaml:L7450-L7459] |
| 247 | `src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py` — `extract_includefiles.traverse` | independent | [docs/registry/implementation-map.yaml:L7460-L7469] |
| 248 | `src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py` — `ModularInputValidator._collect_issues_from_level_1` | independent | [docs/registry/implementation-map.yaml:L7470-L7479] |
| 249 | `src/digitalmodel/solvers/orcaflex/mooring.py` — `Mooring.prepare_includefile_for_all_lines` | independent | [docs/registry/implementation-map.yaml:L7480-L7489] |
| 250 | `src/digitalmodel/solvers/orcaflex/mooring.py` — `Mooring.prepare_includefile_for_fender_compression` | independent | [docs/registry/implementation-map.yaml:L7490-L7499] |
| 251 | `src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py` — `MockOrcaFlexAPI.load_model` | independent | [docs/registry/implementation-map.yaml:L7500-L7509] |
| 252 | `src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py` — `OrcaFlexAPI.save_model` | independent | [docs/registry/implementation-map.yaml:L7510-L7519] |
| 253 | `src/digitalmodel/solvers/orcaflex/operability_analysis.py` — `OperabilityAnalyzer.generate_operability_envelope` | independent | [docs/registry/implementation-map.yaml:L7520-L7529] |
| 254 | `src/digitalmodel/solvers/orcaflex/opp_visualization.py` — `OPPVisualization.combine_models` | independent | [docs/registry/implementation-map.yaml:L7530-L7539] |
| 255 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` — `OrcaFlexAnalysis.save_data` | independent | [docs/registry/implementation-map.yaml:L7540-L7551] |
| 256 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` — `OrcaFlexAnalysis.saveSummaryToExcel` | independent | [docs/registry/implementation-map.yaml:L7552-L7561] |
| 257 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` — `OrcaFlexAnalysis.save_histograms` | independent | [docs/registry/implementation-map.yaml:L7562-L7571] |
| 258 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` — `OrcaFlexAnalysis.save_cfg_files_from_multiple_files` | independent | [docs/registry/implementation-map.yaml:L7572-L7581] |
| 259 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` — `OrcaFlexAnalysis.save_RAOs` | independent | [docs/registry/implementation-map.yaml:L7582-L7591] |
| 260 | `src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py` — `OrcaFlexConverterEnhanced._real_convert` | wrapper | [docs/registry/implementation-map.yaml:L7592-L7603] |
| 261 | `src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py` — `OrcaFlexConverterEnhanced._mock_convert` | independent | [docs/registry/implementation-map.yaml:L7604-L7613] |
| 262 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` — `OrcaFlexCustomAnalysis.process_fea` | independent | [docs/registry/implementation-map.yaml:L7614-L7626] |
| 263 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` — `OrcaFlexCustomAnalysis.clean_model` | independent | [docs/registry/implementation-map.yaml:L7627-L7636] |
| 264 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` — `OrcaFlexCustomAnalysis.save_model_with_calculated_positions` | independent | [docs/registry/implementation-map.yaml:L7637-L7646] |
| 265 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` — `OrcaFlexCustomAnalysis.iterate_to_target_value` | independent | [docs/registry/implementation-map.yaml:L7647-L7656] |
| 266 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` — `OrcaFlexCustomAnalysis.save_data` | independent | [docs/registry/implementation-map.yaml:L7657-L7666] |
| 267 | `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` — `OrcInstallation.create_model_for_water_depth` | independent | [docs/registry/implementation-map.yaml:L7667-L7676] |
| 268 | `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` — `OrcInstallation.create_installation_depth_model` | independent | [docs/registry/implementation-map.yaml:L7677-L7686] |
| 269 | `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` — `OrcInstallation.create_installation_depth_model2` | independent | [docs/registry/implementation-map.yaml:L7687-L7696] |
| 270 | `src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py` — `OrcaflexIterativeRuns.save_iterative_file` | independent | [docs/registry/implementation-map.yaml:L7697-L7706] |
| 271 | `src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py` — `OrcaflexIterativeRuns.run_iterative_simulations` | independent | [docs/registry/implementation-map.yaml:L7707-L7716] |
| 272 | `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` — `OrcaflexLineTypes.get_umbilical_properties` | wrapper | [docs/registry/implementation-map.yaml:L7717-L7729] |
| 273 | `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` — `OrcaflexLineTypes.assign_key_umbilical_properties` | independent | [docs/registry/implementation-map.yaml:L7730-L7739] |
| 274 | `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` — `OrcaflexLineTypes.save_model` | independent | [docs/registry/implementation-map.yaml:L7740-L7749] |
| 275 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.orcaflex_model` | independent | [docs/registry/implementation-map.yaml:L7750-L7762] |
| 276 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.riser_joints_to_line_types` | copy | [docs/registry/implementation-map.yaml:L7763-L7774] |
| 277 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.GetGeneral` | copy | [docs/registry/implementation-map.yaml:L7775-L7784] |
| 278 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.lineTypes` | copy | [docs/registry/implementation-map.yaml:L7785-L7794] |
| 279 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.WriteOrcaflexModel` | independent | [docs/registry/implementation-map.yaml:L7795-L7804] |
| 280 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.build_model` | independent | [docs/registry/implementation-map.yaml:L7805-L7816] |
| 281 | `src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel.py` — `OrcaFlexOptimizedParallelAnalysis.process_single_file` | independent | [docs/registry/implementation-map.yaml:L7817-L7826] |
| 282 | `src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py` — `process_single_file_worker` | independent | [docs/registry/implementation-map.yaml:L7827-L7836] |
| 283 | `src/digitalmodel/solvers/orcaflex/orcaflex_parallel_analysis.py` — `OrcaFlexParallelAnalysis.process_single_file` | independent | [docs/registry/implementation-map.yaml:L7837-L7846] |
| 284 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` — `OrcaflexUtilities.update_model` | independent | [docs/registry/implementation-map.yaml:L7847-L7856] |
| 285 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` — `OrcaflexUtilities.update_input_file` | independent | [docs/registry/implementation-map.yaml:L7857-L7866] |
| 286 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` — `OrcaflexUtilities.get_full_yaml_file_and_save` | caller | [docs/registry/implementation-map.yaml:L7867-L7878] |
| 287 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` — `OrcaflexUtilities.get_general_yml` | independent | [docs/registry/implementation-map.yaml:L7879-L7888] |
| 288 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` — `OrcaflexUtilities.save_cfg_files_from_multiple_files` | independent | [docs/registry/implementation-map.yaml:L7889-L7898] |
| 289 | `src/digitalmodel/solvers/orcaflex/orcaflex_yml_converter.py` — `convert_to_yml` | independent | [docs/registry/implementation-map.yaml:L7899-L7908] |
| 290 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator._merge_yaml` | wrapper | [docs/registry/implementation-map.yaml:L7909-L7921] |
| 291 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator._merge_lines` | independent | [docs/registry/implementation-map.yaml:L7922-L7931] |
| 292 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator._merge_6dbuoys` | independent | [docs/registry/implementation-map.yaml:L7932-L7941] |
| 293 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator._parse_pipeline_path` | independent | [docs/registry/implementation-map.yaml:L7942-L7951] |
| 294 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator._parse_boundary_conditions` | independent | [docs/registry/implementation-map.yaml:L7952-L7961] |
| 295 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator.create_plan_view` | independent | [docs/registry/implementation-map.yaml:L7962-L7971] |
| 296 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator.create_elevation_view` | independent | [docs/registry/implementation-map.yaml:L7972-L7981] |
| 297 | `src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py` — `LoadVessel.aqwa_group_item` | independent | [docs/registry/implementation-map.yaml:L7982-L7991] |
| 298 | `src/digitalmodel/solvers/orcaflex/reporting/extractors/aggregator.py` — `_extract_dynamic_results_all` | independent | [docs/registry/implementation-map.yaml:L7992-L8001] |
| 299 | `src/digitalmodel/solvers/orcaflex/reporting/renderers/riser.py` — `RiserRenderer._build_riser_dynamic_results` | independent | [docs/registry/implementation-map.yaml:L8002-L8011] |
| 300 | `src/digitalmodel/solvers/orcaflex/run_to_sim.py` — `find_model_files` | independent | [docs/registry/implementation-map.yaml:L8012-L8021] |
| 301 | `src/digitalmodel/solvers/orcaflex/spec_author.py` — `AuthoredSpec.to_spec_yaml` | independent | [docs/registry/implementation-map.yaml:L8022-L8031] |
| 302 | `src/digitalmodel/solvers/orcaflex/template_generator.py` — `TemplateGenerator.generate` | caller | [docs/registry/implementation-map.yaml:L8032-L8043] |
| 303 | `src/digitalmodel/solvers/orcaflex/template_generator.py` — `TemplateGenerator._generate_reference` | independent | [docs/registry/implementation-map.yaml:L8044-L8053] |
| 304 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` — `InstallationVtoHost.installation_step` | independent | [docs/registry/implementation-map.yaml:L8054-L8063] |
| 305 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` — `InstallationVtoHost.get_target_line_length` | independent | [docs/registry/implementation-map.yaml:L8064-L8073] |
| 306 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` — `InstallationVtoHost.get_target_reference_distance` | independent | [docs/registry/implementation-map.yaml:L8074-L8083] |
| 307 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` — `Lower2ndEnd.installation_step` | independent | [docs/registry/implementation-map.yaml:L8084-L8093] |
| 308 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` — `Lower2ndEnd.get_target_line_length` | copy | [docs/registry/implementation-map.yaml:L8094-L8103] |
| 309 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` — `Lower2ndEnd.get_target_reference_distance` | independent | [docs/registry/implementation-map.yaml:L8104-L8113] |
| 310 | `src/digitalmodel/solvers/orcaflex/universal/universal_runner.py` — `UniversalOrcaFlexRunner._process_single_model` | independent | [docs/registry/implementation-map.yaml:L8114-L8123] |
| 311 | `src/digitalmodel/solvers/orcaflex/universal_cli.py` — `create_config` | independent | [docs/registry/implementation-map.yaml:L8124-L8133] |
| 312 | `src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py` — `_mapping_issues` | independent | [docs/registry/implementation-map.yaml:L8134-L8143] |
| 313 | `src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py` — `inspect_source` | independent | [docs/registry/implementation-map.yaml:L8144-L8155] |
| 314 | `src/digitalmodel/solvers/orcaflex/yaml_utils.py` — `orcaflex_dump` | independent | [docs/registry/implementation-map.yaml:L8156-L8165] |
| 315 | `src/digitalmodel/solvers/orcaflex/yaml_utils.py` — `orcaflex_load` | independent | [docs/registry/implementation-map.yaml:L8166-L8175] |
| 316 | `src/digitalmodel/solvers/orcaflex/yaml_validator.py` — `OrcaFlexYAMLValidator._detect_format` | independent | [docs/registry/implementation-map.yaml:L8176-L8185] |
| 317 | `src/digitalmodel/solvers/orcaflex/yaml_validator.py` — `OrcaFlexYAMLValidator._check_singular_section_keys` | independent | [docs/registry/implementation-map.yaml:L8186-L8195] |
| 318 | `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` — `OrcaWaveOrchestrator._create_vessel_config` | independent | [docs/registry/implementation-map.yaml:L8196-L8205] |
| 319 | `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` — `OrcaWaveOrchestrator.list_available_vessels` | independent | [docs/registry/implementation-map.yaml:L8206-L8215] |
| 320 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` — `OrcaFlexConverter.save_orcaflex_file` | independent | [docs/registry/implementation-map.yaml:L8216-L8227] |
| 321 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` — `OrcaFlexConverter._save_compact_format` | independent | [docs/registry/implementation-map.yaml:L8228-L8237] |
| 322 | `src/digitalmodel/solvers/smoke/model_manifest.py` — `_tree` | independent | [docs/registry/implementation-map.yaml:L8238-L8247] |
| 323 | `src/digitalmodel/solvers/smoke/model_probe.py` — `check_settings` | independent | [docs/registry/implementation-map.yaml:L8248-L8257] |
| 324 | `src/digitalmodel/solvers/smoke/probes.py` — `_saved_proof` | independent | [docs/registry/implementation-map.yaml:L8258-L8267] |
| 325 | `src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py` — `prepareFatiguePostProcessing` | independent | [docs/registry/implementation-map.yaml:L8268-L8279] |
| 326 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `orcaflexModel` | independent | [docs/registry/implementation-map.yaml:L8280-L8290] |
| 327 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `lineTypes` | independent | [docs/registry/implementation-map.yaml:L8291-L8300] |
| 328 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `WriteOrcaflexModel` | independent | [docs/registry/implementation-map.yaml:L8301-L8310] |
| 329 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `build_model` | independent | [docs/registry/implementation-map.yaml:L8311-L8323] |
| 330 | `src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py` — `OrcaFlexModelGenerator.generate_model_yml` | independent | [docs/registry/implementation-map.yaml:L8324-L8333] |
| 331 | `src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py` — `OrcaFlexModelGenerator.generate_model_dict` | independent | [docs/registry/implementation-map.yaml:L8334-L8343] |
| 332 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` — `VerticalRiser.get_fea_properties` | independent | [docs/registry/implementation-map.yaml:L8344-L8353] |
| 333 | `src/digitalmodel/viv_screening/workflow.py` — `save_echo` | independent | [docs/registry/implementation-map.yaml:L8354-L8363] |
| 334 | `src/digitalmodel/web/digitaltwinfeed/BuoySALM/BuoySALM.py` — `OrcaFlex.get` | independent | [docs/registry/implementation-map.yaml:L8364-L8373] |
| 335 | `src/digitalmodel/web/digitaltwinfeed/FFS/FFS.py` — `OrcaFlex.get` | copy | [docs/registry/implementation-map.yaml:L8374-L8383] |
| 336 | `src/digitalmodel/web/digitaltwinfeed/OffshoreRiser/OffshoreRiser.py` — `OrcaFlex.get` | copy | [docs/registry/implementation-map.yaml:L8384-L8393] |
| 337 | `src/digitalmodel/workflows/automation/go_by_folder/metadata/analysis.py` — `AnalysisMetadataGenerator._detect_workflow_type` | independent | [docs/registry/implementation-map.yaml:L8394-L8403] |
| 338 | `src/digitalmodel/workflows/installation_added_mass_bracket.py` — `integration_basis` | independent | [docs/registry/implementation-map.yaml:L8404-L8413] |
| 339 | `src/digitalmodel/workflows/installation_added_mass_bracket.py` — `named_body` | independent | [docs/registry/implementation-map.yaml:L8414-L8423] |
| 340 | `src/digitalmodel/workflows/installation_added_mass_bracket.py` — `expected_change` | independent | [docs/registry/implementation-map.yaml:L8424-L8435] |
| 341 | `src/digitalmodel/workflows/installation_added_mass_evidence.py` — `read_cases` | independent | [docs/registry/implementation-map.yaml:L8436-L8445] |
| 342 | `src/digitalmodel/workflows/installation_added_mass_materialize.py` — `_write_request` | independent | [docs/registry/implementation-map.yaml:L8446-L8457] |
| 343 | `src/digitalmodel/workflows/installation_added_mass_materialize.py` — `materialize_bracket_case` | independent | [docs/registry/implementation-map.yaml:L8458-L8470] |
| 344 | `src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py` — `_source` | independent | [docs/registry/implementation-map.yaml:L8471-L8480] |
| 345 | `src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py` — `_case_files` | independent | [docs/registry/implementation-map.yaml:L8481-L8492] |
| 346 | `src/digitalmodel/workflows/installation_numerical_compare.py` — `_apply_change` | independent | [docs/registry/implementation-map.yaml:L8493-L8505] |
| 347 | `src/digitalmodel/workflows/installation_numerical_compare.py` — `_canonical_tables` | wrapper | [docs/registry/implementation-map.yaml:L8506-L8517] |
| 348 | `src/digitalmodel/workflows/installation_numerical_compare.py` — `_runtime` | independent | [docs/registry/implementation-map.yaml:L8518-L8527] |
| 349 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_field` | independent | [docs/registry/implementation-map.yaml:L8528-L8537] |
| 350 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_line` | independent | [docs/registry/implementation-map.yaml:L8538-L8547] |
| 351 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_value` | caller | [docs/registry/implementation-map.yaml:L8548-L8560] |
| 352 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_anchors` | independent | [docs/registry/implementation-map.yaml:L8561-L8572] |
| 353 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_timing` | independent | [docs/registry/implementation-map.yaml:L8573-L8582] |
| 354 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_ordered` | independent | [docs/registry/implementation-map.yaml:L8583-L8596] |
| 355 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_apply_native` | wrapper | [docs/registry/implementation-map.yaml:L8597-L8608] |
| 356 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_counts` | wrapper | [docs/registry/implementation-map.yaml:L8609-L8619] |
| 357 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_variation` | caller | [docs/registry/implementation-map.yaml:L8620-L8632] |
| 358 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_native_case` | independent | [docs/registry/implementation-map.yaml:L8633-L8647] |
| 359 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_request` | independent | [docs/registry/implementation-map.yaml:L8648-L8657] |
| 360 | `src/digitalmodel/workflows/installation_seastates.py` — `_snapshot` | independent | [docs/registry/implementation-map.yaml:L8658-L8667] |
| 361 | `src/digitalmodel/workflows/installation_seastates.py` — `_structure` | independent | [docs/registry/implementation-map.yaml:L8668-L8677] |
| 362 | `src/digitalmodel/workflows/installation_seastates.py` — `_request` | independent | [docs/registry/implementation-map.yaml:L8678-L8687] |
| 363 | `src/digitalmodel/workflows/installation_seastates.py` — `generate_case` | independent | [docs/registry/implementation-map.yaml:L8688-L8701] |
| 364 | `src/digitalmodel/workflows/installation_seastates.py` — `prepare_matrix` | independent | [docs/registry/implementation-map.yaml:L8702-L8714] |
| 365 | `src/digitalmodel/workflows/installation_seastates.py` — `_change_payload` | independent | [docs/registry/implementation-map.yaml:L8715-L8724] |
| 366 | `src/digitalmodel/workflows/installation_seastates.py` — `materialize_case` | independent | [docs/registry/implementation-map.yaml:L8725-L8739] |
| 367 | `src/digitalmodel/workflows/installation_variant_study.py` — `_verify_master` | independent | [docs/registry/implementation-map.yaml:L8740-L8749] |
| 368 | `src/digitalmodel/workflows/installation_variant_study.py` — `_verify_settings` | independent | [docs/registry/implementation-map.yaml:L8750-L8759] |
| 369 | `src/digitalmodel/workflows/installation_variant_study.py` — `prepare_variant_study` | independent | [docs/registry/implementation-map.yaml:L8760-L8773] |
| 370 | `src/digitalmodel/workflows/mcp_server/orcawave/core/mcp_server.py` — `OrcaWaveMCPServer._register_tools.export_to_orcaflex` | independent | [docs/registry/implementation-map.yaml:L8774-L8783] |
| 371 | `src/digitalmodel/workflows/orcaflex_reproduce.py` — `_prepare` | independent | [docs/registry/implementation-map.yaml:L8784-L8793] |
| 372 | `src/digitalmodel/workflows/orcaflex_reproduce_results.py` — `extract_results` | independent | [docs/registry/implementation-map.yaml:L8794-L8803] |
| 373 | `src/digitalmodel/workflows/parametric_run.py` — `_write_yaml` | independent | [docs/registry/implementation-map.yaml:L8804-L8813] |
| 374 | `src/digitalmodel/workflows/skills/skill_resolver.py` — `SkillResolver._parse_skill_file` | independent | [docs/registry/implementation-map.yaml:L8814-L8823] |
| 375 | `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` — `_model` | independent | [docs/registry/implementation-map.yaml:L8824-L8833] |
| 376 | `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` — `_apply` | independent | [docs/registry/implementation-map.yaml:L8834-L8843] |
| 377 | `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` — `build_candidate` | independent | [docs/registry/implementation-map.yaml:L8844-L8856] |
| 378 | `src/digitalmodel/workflows/structure_installation_prepare.py` — `_dependencies` | independent | [docs/registry/implementation-map.yaml:L8857-L8866] |
| 379 | `tests/drilling_riser/global_model/test_batch_params.py` — `base_spec` | test_helper | [docs/registry/implementation-map.yaml:L8867-L8876] |
| 380 | `tests/drilling_riser/global_model/test_campaign.py` — `base_spec` | test_helper | [docs/registry/implementation-map.yaml:L8877-L8886] |
| 381 | `tests/drilling_riser/global_model/test_event_models.py` — `_case` | test_helper | [docs/registry/implementation-map.yaml:L8887-L8896] |
| 382 | `tests/drilling_riser/global_model/test_hang_off.py` — `_generic` | test_helper | [docs/registry/implementation-map.yaml:L8897-L8908] |
| 383 | `tests/drilling_riser/global_model/test_hang_off.py` — `_case` | test_helper | [docs/registry/implementation-map.yaml:L8909-L8918] |
| 384 | `tests/drilling_riser/global_model/test_open_water.py` — `_ow_case` | test_helper | [docs/registry/implementation-map.yaml:L8919-L8928] |
| 385 | `tests/drilling_riser/global_model/test_statics_path.py` — `base_spec` | test_helper | [docs/registry/implementation-map.yaml:L8929-L8938] |
| 386 | `tests/drilling_riser/global_model/test_tensioner_representation.py` — `_line` | test_helper | [docs/registry/implementation-map.yaml:L8939-L8948] |
| 387 | `tests/drilling_riser/global_model/test_w5_channels.py` — `base_spec` | test_helper | [docs/registry/implementation-map.yaml:L8949-L8958] |
| 388 | `tests/drilling_riser/postprocess/test_w510_significant_range.py` — `_doc` | test_helper | [docs/registry/implementation-map.yaml:L8959-L8970] |
| 389 | `tests/legal/test_sanitize_s7_audit_private.py` — `setup` | test_helper | [docs/registry/implementation-map.yaml:L8971-L8980] |
| 390 | `tests/maintenance/test_archive_safety.py` — `test_deep_reference_and_registry_paths_are_excluded` | test_helper | [docs/registry/implementation-map.yaml:L8981-L8990] |
| 391 | `tests/maintenance/test_archive_safety.py` — `test_generator_partitions_move_keep_hold_and_excluded` | test_helper | [docs/registry/implementation-map.yaml:L8991-L9000] |
| 392 | `tests/orcaflex/test_generic_scr_slwr.py` — `test_specs_match_basis_and_static_geometry` | test_helper | [docs/registry/implementation-map.yaml:L9001-L9010] |
| 393 | `tests/orcaflex/test_generic_scr_slwr.py` — `test_analytical_reference_reproduces_from_basis` | test_helper | [docs/registry/implementation-map.yaml:L9011-L9020] |
| 394 | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` — `TestModularToSpecBoundaryReporting.test_expected_gaps_classified` | test_helper | [docs/registry/implementation-map.yaml:L9021-L9030] |
| 395 | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` — `TestModularToSpecBoundaryReporting.test_actionable_gaps_classified` | test_helper | [docs/registry/implementation-map.yaml:L9031-L9040] |
| 396 | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` — `TestModularToSpecBoundaryReporting.test_all_unmapped_are_categorized` | test_helper | [docs/registry/implementation-map.yaml:L9041-L9050] |
| 397 | `tests/solvers/orcaflex/modular_generator/builders/test_general_builder.py` — `_build` | test_helper | [docs/registry/implementation-map.yaml:L9051-L9060] |
| 398 | `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` — `_build` | test_helper | [docs/registry/implementation-map.yaml:L9061-L9070] |
| 399 | `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` — `_build` | test_helper | [docs/registry/implementation-map.yaml:L9071-L9080] |
| 400 | `tests/solvers/orcaflex/modular_generator/goldens/basefile/capture.py` — `main` | test_helper | [docs/registry/implementation-map.yaml:L9081-L9090] |
| 401 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` — `_write_floating_campaign` | test_helper | [docs/registry/implementation-map.yaml:L9091-L9100] |
| 402 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` — `_write_slay_campaign` | test_helper | [docs/registry/implementation-map.yaml:L9101-L9110] |
| 403 | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py` — `_write_campaign_yaml` | test_helper | [docs/registry/implementation-map.yaml:L9111-L9120] |
| 404 | `tests/solvers/orcaflex/modular_generator/test_collection_collision.py` — `_write_model` | test_helper | [docs/registry/implementation-map.yaml:L9121-L9130] |
| 405 | Discovery file 9 with identifying name withheld; implementation 405 | test_helper | [docs/registry/implementation-map.yaml:L9131-L9140] |
| 406 | Discovery file 9 with identifying name withheld; implementation 406 | test_helper | [docs/registry/implementation-map.yaml:L9141-L9152] |
| 407 | Discovery file 10 with identifying name withheld; implementation 407 | test_helper | [docs/registry/implementation-map.yaml:L9153-L9162] |
| 408 | Discovery file 10 with identifying name withheld; implementation 408 | test_helper | [docs/registry/implementation-map.yaml:L9163-L9172] |
| 409 | `tests/solvers/orcaflex/modular_generator/test_riser_variant_semantic_proof.py` — `_assert_line_type_references_resolve` | test_helper | [docs/registry/implementation-map.yaml:L9173-L9182] |
| 410 | `tests/solvers/orcaflex/modular_generator/test_semantic_roundtrip.py` — `_write_mono_yaml` | test_helper | [docs/registry/implementation-map.yaml:L9183-L9192] |
| 411 | Discovery file 11 with identifying name withheld; implementation 411 | test_helper | [docs/registry/implementation-map.yaml:L9193-L9202] |
| 412 | `tests/solvers/orcaflex/reporting/fixture_helpers.py` — `_build_boundary_conditions` | test_helper | [docs/registry/implementation-map.yaml:L9203-L9212] |
| 413 | `tests/solvers/orcaflex/single_file_parallel_test.py` — `SingleFileParallelStrategy._get_line_results` | test_helper | [docs/registry/implementation-map.yaml:L9213-L9222] |
| 414 | `tests/solvers/orcaflex/single_file_parallel_test.py` — `SingleFileParallelStrategy._get_vessel_results` | test_helper | [docs/registry/implementation-map.yaml:L9223-L9232] |
| 415 | `tests/solvers/orcaflex/test_model_catalogue.py` — `test_structural_owners_and_path_references` | test_helper | [docs/registry/implementation-map.yaml:L9233-L9242] |
| 416 | `tests/solvers/orcaflex/test_model_catalogue_references.py` — `test_same_named_unrelated_folders_are_not_references` | test_helper | [docs/registry/implementation-map.yaml:L9243-L9252] |
| 417 | `tests/solvers/orcaflex/test_model_header_guard.py` — `header_repository_findings` | test_helper | [docs/registry/implementation-map.yaml:L9253-L9262] |
| 418 | `tests/solvers/orcaflex/test_modular_input_validation_fixes.py` — `master_with_missing_include` | test_helper | [docs/registry/implementation-map.yaml:L9263-L9272] |
| 419 | `tests/solvers/orcaflex/test_slay_layout.py` — `test_support_types_are_defined_before_vessel_references` | test_helper | [docs/registry/implementation-map.yaml:L9273-L9282] |
| 420 | `tests/solvers/orcaflex/test_slay_load_form.py` — `_resolve_case.read` | test_helper | [docs/registry/implementation-map.yaml:L9283-L9292] |
| 421 | `tests/solvers/orcaflex/test_slay_load_form.py` — `_pipe_seed_points` | test_helper | [docs/registry/implementation-map.yaml:L9293-L9302] |
| 422 | `tests/solvers/orcaflex/test_template_generator.py` — `sample_base_template` | test_helper | [docs/registry/implementation-map.yaml:L9303-L9312] |
| 423 | `tests/solvers/orcaflex/test_template_generator.py` — `sample_variation` | test_helper | [docs/registry/implementation-map.yaml:L9313-L9322] |
| 424 | `tests/solvers/orcaflex/test_template_generator.py` — `sample_case_template` | test_helper | [docs/registry/implementation-map.yaml:L9323-L9332] |
| 425 | `tests/solvers/orcaflex/test_template_generator.py` — `TestTemplateGenerator.test_generate_with_basefile_reference` | test_helper | [docs/registry/implementation-map.yaml:L9333-L9342] |
| 426 | `tests/solvers/orcaflex/test_template_generator.py` — `TestTemplateGenerator.test_reference_paths_resolve_from_arbitrary_output_dir` | test_helper | [docs/registry/implementation-map.yaml:L9343-L9352] |
| 427 | `tests/solvers/orcaflex/test_template_generator.py` — `TestTemplateGenerator.test_reference_paths_in_cases_dir_stay_relative` | test_helper | [docs/registry/implementation-map.yaml:L9353-L9362] |
| 428 | `tests/solvers/smoke/test_model_manifest.py` — `manifest` | test_helper | [docs/registry/implementation-map.yaml:L9363-L9372] |
| 429 | `tests/test_engine.py` — `temp_config_file` | test_helper | [docs/registry/implementation-map.yaml:L9373-L9382] |
| 430 | `tests/test_main_exit_status.py` — `_solve_input` | test_helper | [docs/registry/implementation-map.yaml:L9383-L9392] |
| 431 | `tests/test_skill_resolver.py` — `_write_skill` | test_helper | [docs/registry/implementation-map.yaml:L9393-L9402] |
| 432 | `tests/workflows/test_durable_workflows.py` — `test_workflow_registry` | test_helper | [docs/registry/implementation-map.yaml:L9403-L9412] |
| 433 | `tests/workflows/test_durable_workflows.py` — `test_synthetic_rope_tn_curve_matches_dnv_os_e301_table_f3` | test_helper | [docs/registry/implementation-map.yaml:L9413-L9422] |
| 434 | `tests/workflows/test_durable_workflows.py` — `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | test_helper | [docs/registry/implementation-map.yaml:L9423-L9432] |
| 435 | `tests/workflows/test_durable_workflows.py` — `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | test_helper | [docs/registry/implementation-map.yaml:L9433-L9442] |
| 436 | `tests/workflows/test_durable_workflows.py` — `test_vessel_seakeeping_significant_amplitude_matches_library` | test_helper | [docs/registry/implementation-map.yaml:L9443-L9452] |
| 437 | `tests/workflows/test_installation_instability_diagnostic.py` — `fake` | test_helper | [docs/registry/implementation-map.yaml:L9453-L9462] |
| 438 | `tests/workflows/test_orcaflex_batch_contract.py` — `test_duration_render_preserves_native_default_spelling_and_source` | test_helper | [docs/registry/implementation-map.yaml:L9463-L9472] |
| 439 | `tests/workflows/test_orcaflex_reproduce.py` — `resume_case` | test_helper | [docs/registry/implementation-map.yaml:L9473-L9482] |
| 440 | `tests/workflows/test_structure_hydrodynamic_candidate.py` — `case` | test_helper | [docs/registry/implementation-map.yaml:L9483-L9493] |

#### rao-readers-and-orcaflex-converters

| Row | Implementation identity | Map relation | Evidence at map pin |
| --- | --- | --- | --- |
| 1 | `docs/domains/diffraction/examples/orcawave_conversion_example.py` — `main` | independent | [docs/registry/implementation-map.yaml:L9516-L9527] |
| 2 | `docs/domains/orcawave/L01_aqwa_benchmark/compare_model_inputs.py` — `main` | independent | [docs/registry/implementation-map.yaml:L9528-L9539] |
| 3 | `docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py` — `extract_aqwa_raos` | independent | [docs/registry/implementation-map.yaml:L9540-L9551] |
| 4 | `docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py` — `extract_orcawave_raos` | independent | [docs/registry/implementation-map.yaml:L9552-L9563] |
| 5 | `docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py` — `main` | independent | [docs/registry/implementation-map.yaml:L9564-L9578] |
| 6 | `docs/domains/orcawave/L01_aqwa_benchmark/extract_orcawave_raos.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L9579-L9589] |
| 7 | `docs/domains/orcawave/L01_aqwa_benchmark/extract_orcawave_raos.py` — `extract_orcawave_raos` | independent | [docs/registry/implementation-map.yaml:L9590-L9601] |
| 8 | `docs/domains/orcawave/L01_aqwa_benchmark/monitor_orcawave_run.py` — `monitor_progress` | independent | [docs/registry/implementation-map.yaml:L9602-L9613] |
| 9 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py` — `extract_aqwa_data` | independent | [docs/registry/implementation-map.yaml:L9614-L9625] |
| 10 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py` — `extract_orcawave_data` | independent | [docs/registry/implementation-map.yaml:L9626-L9637] |
| 11 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py` — `main` | independent | [docs/registry/implementation-map.yaml:L9638-L9652] |
| 12 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison_with_interpolation.py` — `extract_orcawave_raos` | independent | [docs/registry/implementation-map.yaml:L9653-L9664] |
| 13 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison_with_interpolation.py` — `main` | independent | [docs/registry/implementation-map.yaml:L9665-L9678] |
| 14 | `docs/domains/orcawave/L01_aqwa_benchmark/run_heading_specific_comparison.py` — `extract_orcawave_raos` | independent | [docs/registry/implementation-map.yaml:L9679-L9690] |
| 15 | `docs/domains/orcawave/L01_aqwa_benchmark/run_heading_specific_comparison.py` — `main` | independent | [docs/registry/implementation-map.yaml:L9691-L9704] |
| 16 | `docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_api.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L9705-L9715] |
| 17 | `docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_matched_api.py` — `main` | independent | [docs/registry/implementation-map.yaml:L9716-L9727] |
| 18 | `docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py` — `extract_orcawave_motion_raos` | independent | [docs/registry/implementation-map.yaml:L9728-L9739] |
| 19 | `docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py` — `main` | independent | [docs/registry/implementation-map.yaml:L9740-L9753] |
| 20 | `docs/domains/orcawave/L01_aqwa_benchmark/validate_matched_model.py` — `main` | independent | [docs/registry/implementation-map.yaml:L9754-L9765] |
| 21 | Discovery file 2 with identifying name withheld; implementation 21 | independent | [docs/registry/implementation-map.yaml:L9766-L9777] |
| 22 | `docs/domains/orcawave/examples/qa/orcawave_example_qa.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L9778-L9788] |
| 23 | `docs/domains/orcawave/examples/qa/orcawave_example_qa.py` — `_check_raos_finite` | independent | [docs/registry/implementation-map.yaml:L9789-L9800] |
| 24 | `docs/domains/orcawave/examples/qa/orcawave_example_qa.py` — `_check_heave_rao_long_period` | independent | [docs/registry/implementation-map.yaml:L9801-L9812] |
| 25 | `examples/domains/core/cache_example.py` — `example_realistic_usage.load_hydrodynamic_database` | example | [docs/registry/implementation-map.yaml:L9813-L9826] |
| 26 | `examples/domains/validation/validation_pipeline_example.py` — `example_rao_polar_data_validation` | example | [docs/registry/implementation-map.yaml:L9827-L9838] |
| 27 | `scripts/benchmark/compare_hydro_properties.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L9839-L9849] |
| 28 | `scripts/benchmark/compare_hydro_properties.py` — `extract_aqwa_damping` | independent | [docs/registry/implementation-map.yaml:L9850-L9861] |
| 29 | `scripts/benchmark/compare_hydro_properties.py` — `extract_aqwa_added_mass` | independent | [docs/registry/implementation-map.yaml:L9862-L9873] |
| 30 | `scripts/benchmark/compare_hydro_properties.py` — `parse_args` | independent | [docs/registry/implementation-map.yaml:L9874-L9885] |
| 31 | `scripts/benchmark/digitize_validation_plots.py` — `parse_args` | independent | [docs/registry/implementation-map.yaml:L9886-L9897] |
| 32 | `scripts/benchmark/probe_owd_content.py` — `_extract_core_data` | independent | [docs/registry/implementation-map.yaml:L9898-L9909] |
| 33 | `scripts/benchmark/probe_owd_content.py` — `probe_body_details` | independent | [docs/registry/implementation-map.yaml:L9910-L9921] |
| 34 | `scripts/benchmark/qtf_postprocessing.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L9922-L9932] |
| 35 | `scripts/benchmark/regenerate_barge_benchmark.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L9933-L9943] |
| 36 | `scripts/benchmark/regenerate_ship_benchmark.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L9944-L9954] |
| 37 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L9955-L9965] |
| 38 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` — `_extract_correlations_from_json` | independent | [docs/registry/implementation-map.yaml:L9966-L9977] |
| 39 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` — `_extract_correlations_from_runner` | independent | [docs/registry/implementation-map.yaml:L9978-L9989] |
| 40 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` — `_run_ship_benchmark` | independent | [docs/registry/implementation-map.yaml:L9990-L10004] |
| 41 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` — `_run_spar_benchmark` | independent | [docs/registry/implementation-map.yaml:L10005-L10017] |
| 42 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10018-L10032] |
| 43 | `scripts/benchmark/run_3way_benchmark.py` — `run_orcawave` | independent | [docs/registry/implementation-map.yaml:L10033-L10046] |
| 44 | `scripts/benchmark/run_3way_benchmark.py` — `run_aqwa` | independent | [docs/registry/implementation-map.yaml:L10047-L10060] |
| 45 | `scripts/benchmark/run_3way_benchmark.py` — `_extract_from_owr` | independent | [docs/registry/implementation-map.yaml:L10061-L10072] |
| 46 | `scripts/benchmark/run_3way_benchmark.py` — `_extract_from_aqwa_lis` | independent | [docs/registry/implementation-map.yaml:L10073-L10087] |
| 47 | `scripts/benchmark/run_3way_benchmark.py` — `_parse_aqwa_raos` | independent | [docs/registry/implementation-map.yaml:L10088-L10099] |
| 48 | `scripts/benchmark/run_3way_benchmark.py` — `_parse_aqwa_matrix_section` | independent | [docs/registry/implementation-map.yaml:L10100-L10111] |
| 49 | `scripts/benchmark/run_spar_benchmark.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L10112-L10122] |
| 50 | `scripts/benchmark/run_spar_benchmark.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10123-L10134] |
| 51 | `scripts/benchmark/run_validation_benchmark.py` — `_run_orcawave_for_case` | independent | [docs/registry/implementation-map.yaml:L10135-L10146] |
| 52 | `scripts/benchmark/run_validation_benchmark.py` — `_load_wamit_reference` | independent | [docs/registry/implementation-map.yaml:L10147-L10158] |
| 53 | `scripts/benchmark/solve_owd_extract.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L10159-L10169] |
| 54 | `scripts/benchmark/solve_owd_extract.py` — `solve_and_extract` | independent | [docs/registry/implementation-map.yaml:L10170-L10181] |
| 55 | `scripts/benchmark/validate_owd_vs_spec.py` — `_extract_from_diffraction` | independent | [docs/registry/implementation-map.yaml:L10182-L10193] |
| 56 | `scripts/benchmark/validate_owd_vs_spec.py` — `solve_owd` | independent | [docs/registry/implementation-map.yaml:L10194-L10207] |
| 57 | `scripts/benchmark/validate_owd_vs_spec.py` — `solve_spec` | independent | [docs/registry/implementation-map.yaml:L10208-L10222] |
| 58 | `scripts/benchmark/validate_owd_vs_spec.py` — `_solve_spec_via_orcfxapi` | independent | [docs/registry/implementation-map.yaml:L10223-L10235] |
| 59 | `scripts/build_results_workbook.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L10236-L10246] |
| 60 | `scripts/populate_rao_database.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10247-L10260] |
| 61 | `scripts/python/digitalmodel/legacy/aqwa_data_helper.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10261-L10272] |
| 62 | `scripts/python/digitalmodel/legacy/check_aqwa_heading_fix.py` — `test_actual_file_counts` | independent | [docs/registry/implementation-map.yaml:L10273-L10284] |
| 63 | `scripts/python/digitalmodel/legacy/check_aqwa_parser_fix.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10285-L10296] |
| 64 | `scripts/python/digitalmodel/legacy/diagnose_rao_parsing.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L10297-L10307] |
| 65 | `scripts/python/digitalmodel/legacy/direct_rao_plot.py` — `parse_rao_section_simple` | independent | [docs/registry/implementation-map.yaml:L10308-L10319] |
| 66 | `scripts/python/digitalmodel/legacy/direct_rao_plot.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10320-L10333] |
| 67 | `scripts/python/digitalmodel/legacy/fix_rao_plots.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10334-L10345] |
| 68 | `scripts/python/digitalmodel/legacy/fix_rao_plots_standalone.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L10346-L10356] |
| 69 | `scripts/python/digitalmodel/legacy/generate_rao_qa_report.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10357-L10368] |
| 70 | `scripts/python/digitalmodel/legacy/regenerate_rao_plots.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10369-L10380] |
| 71 | `scripts/python/digitalmodel/legacy/simple_rao_plot.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L10381-L10391] |
| 72 | `scripts/python/digitalmodel/legacy/verify_parser_extraction.py` — `test_rao_section` | independent | [docs/registry/implementation-map.yaml:L10392-L10403] |
| 73 | `scripts/python/digitalmodel/legacy/verify_parser_extraction.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10404-L10415] |
| 74 | `scripts/rao_quality_check.py` — `parse_args` | independent | [docs/registry/implementation-map.yaml:L10416-L10427] |
| 75 | `scripts/rao_quality_check.py` — `process_file` | independent | [docs/registry/implementation-map.yaml:L10428-L10439] |
| 76 | `scripts/run_benchmark_ship_raos.py` — `create_diffraction_results` | independent | [docs/registry/implementation-map.yaml:L10440-L10451] |
| 77 | `scripts/run_benchmark_ship_raos.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10452-L10465] |
| 78 | `scripts/run_semisub_quality_check.py` — `main` | independent | [docs/registry/implementation-map.yaml:L10466-L10477] |
| 79 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` — `GenericRAOClient._generate_generic_raos` | independent | [docs/registry/implementation-map.yaml:L10478-L10489] |
| 80 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` — `GenericRAOClient.interpolate_raos` | independent | [docs/registry/implementation-map.yaml:L10490-L10501] |
| 81 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` — `GenericRAOClient.stream_raos_orcaflex` | independent | [docs/registry/implementation-map.yaml:L10502-L10513] |
| 82 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` — `GenericRAOClient.query_by_date` | caller | [docs/registry/implementation-map.yaml:L10514-L10527] |
| 83 | `src/digitalmodel/data_systems/data_procurement/vessel/client.py` — `VesselClient._initialize_clients` | independent | [docs/registry/implementation-map.yaml:L10528-L10539] |
| 84 | `src/digitalmodel/drilling_riser/global_model/build.py` — `_vessel_type` | independent | [docs/registry/implementation-map.yaml:L10540-L10551] |
| 85 | `src/digitalmodel/drilling_riser/global_model/raos.py` — `parse_direction_grouped_rao_text` | independent | [docs/registry/implementation-map.yaml:L10552-L10563] |
| 86 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` — `AqwaRAOs.get_natural_period_from_lis_file` | independent | [docs/registry/implementation-map.yaml:L10564-L10575] |
| 87 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` — `AqwaRAOs.get_percent_critical_damping` | independent | [docs/registry/implementation-map.yaml:L10576-L10587] |
| 88 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` — `AqwaRAOs.get_absolute_damping` | independent | [docs/registry/implementation-map.yaml:L10588-L10599] |
| 89 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` — `AqwaRAOs.get_raos_by_cfg` | independent | [docs/registry/implementation-map.yaml:L10600-L10611] |
| 90 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py` — `create_parser` | independent | [docs/registry/implementation-map.yaml:L10612-L10623] |
| 91 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py` — `list_methods` | independent | [docs/registry/implementation-map.yaml:L10624-L10635] |
| 92 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py` — `AqwaReader.get_change_of_data_result_group` | independent | [docs/registry/implementation-map.yaml:L10636-L10647] |
| 93 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py` — `AqwaReader.get_property_args_array` | independent | [docs/registry/implementation-map.yaml:L10648-L10659] |
| 94 | `src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py` — `ViscousDampingDetermination._process_case` | independent | [docs/registry/implementation-map.yaml:L10660-L10671] |
| 95 | `src/digitalmodel/hydrodynamics/bemrosetta/__init__.py` — `get_module_info` | independent | [docs/registry/implementation-map.yaml:L10672-L10683] |
| 96 | `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` — `convert` | independent | [docs/registry/implementation-map.yaml:L10684-L10699] |
| 97 | `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` — `_export_raos_csv` | independent | [docs/registry/implementation-map.yaml:L10700-L10711] |
| 98 | `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` — `status` | independent | [docs/registry/implementation-map.yaml:L10712-L10725] |
| 99 | `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` — `OrcaFlexConverter._call_orcaflex_exporter` | independent | [docs/registry/implementation-map.yaml:L10726-L10737] |
| 100 | `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` — `OrcaFlexConverter._build_vessel_yaml` | independent | [docs/registry/implementation-map.yaml:L10738-L10749] |
| 101 | `src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py` — `DATHandler._parse_aqwa_deck` | independent | [docs/registry/implementation-map.yaml:L10750-L10761] |
| 102 | `src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py` — `AQWAParser.supported_extensions` | independent | [docs/registry/implementation-map.yaml:L10762-L10775] |
| 103 | `src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py` — `AQWAParser.parse` | independent | [docs/registry/implementation-map.yaml:L10776-L10789] |
| 104 | `src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py` — `AQWAParser._extract_raos` | independent | [docs/registry/implementation-map.yaml:L10790-L10801] |
| 105 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` — `AQWAConverter._extract_rao_data` | caller | [docs/registry/implementation-map.yaml:L10802-L10815] |
| 106 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` — `AQWAConverter._extract_added_mass_data` | caller | [docs/registry/implementation-map.yaml:L10816-L10828] |
| 107 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` — `AQWAConverter._extract_damping_data` | caller | [docs/registry/implementation-map.yaml:L10829-L10841] |
| 108 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` — `AQWAConverter._build_rao_set` | independent | [docs/registry/implementation-map.yaml:L10842-L10853] |
| 109 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` — `AQWAConverter._build_added_mass_set` | independent | [docs/registry/implementation-map.yaml:L10854-L10865] |
| 110 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` — `AQWAConverter._build_damping_set` | independent | [docs/registry/implementation-map.yaml:L10866-L10877] |
| 111 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `AQWALISParser.extract_frequencies_and_periods` | independent | [docs/registry/implementation-map.yaml:L10878-L10891] |
| 112 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `AQWALISParser.extract_headings` | independent | [docs/registry/implementation-map.yaml:L10892-L10905] |
| 113 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `AQWALISParser._table_span` | independent | [docs/registry/implementation-map.yaml:L10906-L10917] |
| 114 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `AQWALISParser.parse_added_mass_table` | caller | [docs/registry/implementation-map.yaml:L10918-L10933] |
| 115 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `AQWALISParser.parse_damping_table` | caller | [docs/registry/implementation-map.yaml:L10934-L10948] |
| 116 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `AQWALISParser.parse_rao_table` | independent | [docs/registry/implementation-map.yaml:L10949-L10962] |
| 117 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `AQWALISParser.parse_all` | wrapper | [docs/registry/implementation-map.yaml:L10963-L10979] |
| 118 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `parse_aqwa_lis_file` | independent | [docs/registry/implementation-map.yaml:L10980-L10991] |
| 119 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `extract_aqwa_frequencies` | independent | [docs/registry/implementation-map.yaml:L10992-L11003] |
| 120 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `extract_aqwa_headings` | independent | [docs/registry/implementation-map.yaml:L11004-L11015] |
| 121 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `extract_aqwa_added_mass` | independent | [docs/registry/implementation-map.yaml:L11016-L11027] |
| 122 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `extract_aqwa_damping` | independent | [docs/registry/implementation-map.yaml:L11028-L11039] |
| 123 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` — `extract_aqwa_raos` | independent | [docs/registry/implementation-map.yaml:L11040-L11051] |
| 124 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_result_extractor.py` — `AQWAResultExtractor.extract` | independent | [docs/registry/implementation-map.yaml:L11052-L11063] |
| 125 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_runner.py` — `AQWARunner._capture_lis_file` | independent | [docs/registry/implementation-map.yaml:L11064-L11075] |
| 126 | `src/digitalmodel/hydrodynamics/diffraction/cli.py` — `convert_aqwa` | independent | [docs/registry/implementation-map.yaml:L11076-L11089] |
| 127 | `src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py` — `create_parser` | independent | [docs/registry/implementation-map.yaml:L11090-L11101] |
| 128 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` — `OrcaFlexExporter.export_raos_csv` | independent | [docs/registry/implementation-map.yaml:L11102-L11113] |
| 129 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` — `OrcaFlexExporter._write_rao_sheet` | independent | [docs/registry/implementation-map.yaml:L11114-L11125] |
| 130 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` — `OrcaFlexExporter._write_discretization_sheet` | independent | [docs/registry/implementation-map.yaml:L11126-L11137] |
| 131 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` — `OrcaWaveRunner._execute_via_api` | independent | [docs/registry/implementation-map.yaml:L11138-L11149] |
| 132 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` — `OrcaWaveRunner._read_live_attrs` | independent | [docs/registry/implementation-map.yaml:L11150-L11163] |
| 133 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` — `OrcaWaveRunner._build_results_from_object` | independent | [docs/registry/implementation-map.yaml:L11164-L11175] |
| 134 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_test_utilities.py` — `MockDataGenerator.create_mock_vessel` | independent | [docs/registry/implementation-map.yaml:L11176-L11187] |
| 135 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py` — `convert_orcawave_xlsx_to_orcaflex` | independent | [docs/registry/implementation-map.yaml:L11188-L11202] |
| 136 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L11203-L11213] |
| 137 | `src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py` — `PolarsExporter.export_all_csv` | independent | [docs/registry/implementation-map.yaml:L11214-L11225] |
| 138 | `src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py` — `PolarsExporter._build_rao_records` | independent | [docs/registry/implementation-map.yaml:L11226-L11237] |
| 139 | `src/digitalmodel/hydrodynamics/diffraction/report_generator.py` — `<module>` | independent | [docs/registry/implementation-map.yaml:L11238-L11248] |
| 140 | `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py` — `OrcaWaveInputParser.parse` | independent | [docs/registry/implementation-map.yaml:L11249-L11260] |
| 141 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` — `OrcaWaveConverter._extract_rao_data` | caller | [docs/registry/implementation-map.yaml:L11261-L11274] |
| 142 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` — `OrcaWaveConverter._build_rao_set` | independent | [docs/registry/implementation-map.yaml:L11275-L11286] |
| 143 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` — `OrcaWaveConverter._build_added_mass_set` | independent | [docs/registry/implementation-map.yaml:L11287-L11298] |
| 144 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` — `OrcaWaveConverter._build_damping_set` | independent | [docs/registry/implementation-map.yaml:L11299-L11310] |
| 145 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py` — `OrcaWaveDataExtractor.extract_rao_frequencies` | independent | [docs/registry/implementation-map.yaml:L11311-L11322] |
| 146 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py` — `OrcaWaveDataExtractor.extract_rao_headings` | independent | [docs/registry/implementation-map.yaml:L11323-L11334] |
| 147 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py` — `extract_all_rao_data` | independent | [docs/registry/implementation-map.yaml:L11335-L11346] |
| 148 | `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` — `WamitReferenceLoader.from_owd` | independent | [docs/registry/implementation-map.yaml:L11347-L11360] |
| 149 | `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` — `WamitReferenceLoader.from_yaml` | independent | [docs/registry/implementation-map.yaml:L11361-L11374] |
| 150 | `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` — `WamitReferenceLoader._build_results` | independent | [docs/registry/implementation-map.yaml:L11375-L11388] |
| 151 | `src/digitalmodel/hydrodynamics/hull_library/analysis_setup.py` — `_attempt_rao_lookup` | independent | [docs/registry/implementation-map.yaml:L11389-L11400] |
| 152 | `src/digitalmodel/hydrodynamics/hull_library/analysis_setup.py` — `setup_hull_analysis` | caller | [docs/registry/implementation-map.yaml:L11401-L11414] |
| 153 | `src/digitalmodel/hydrodynamics/hull_library/catalog.py` — `HullCatalog.compute_motions` | independent | [docs/registry/implementation-map.yaml:L11415-L11426] |
| 154 | `src/digitalmodel/hydrodynamics/hull_library/catalog.py` — `HullCatalog.compute_accelerations` | independent | [docs/registry/implementation-map.yaml:L11427-L11438] |
| 155 | `src/digitalmodel/hydrodynamics/hull_library/rao_database.py` — `RAODatabase.save_to_disk` | independent | [docs/registry/implementation-map.yaml:L11439-L11450] |
| 156 | `src/digitalmodel/hydrodynamics/hull_library/rao_database.py` — `RAODatabase.load_from_disk` | independent | [docs/registry/implementation-map.yaml:L11451-L11462] |
| 157 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` — `PipelineXlsxReader.read_rao_data` | independent | [docs/registry/implementation-map.yaml:L11463-L11474] |
| 158 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` — `NativeXlsxReader.read_rao_data` | independent | [docs/registry/implementation-map.yaml:L11475-L11486] |
| 159 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` — `xlsx_to_rao_data` | independent | [docs/registry/implementation-map.yaml:L11487-L11498] |
| 160 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` — `xlsx_to_hydro_coefficients` | independent | [docs/registry/implementation-map.yaml:L11499-L11510] |
| 161 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` — `populate_database_from_xlsx` | independent | [docs/registry/implementation-map.yaml:L11511-L11524] |
| 162 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_comparison.py` — `compare_box_exports` | independent | [docs/registry/implementation-map.yaml:L11525-L11536] |
| 163 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` — `_read_units` | independent | [docs/registry/implementation-map.yaml:L11537-L11548] |
| 164 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` — `_same_content` | independent | [docs/registry/implementation-map.yaml:L11549-L11560] |
| 165 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` — `store_fixture_entry` | caller | [docs/registry/implementation-map.yaml:L11561-L11574] |
| 166 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` — `populate_fixture_database` | independent | [docs/registry/implementation-map.yaml:L11575-L11590] |
| 167 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` — `_verify_readback` | caller | [docs/registry/implementation-map.yaml:L11591-L11603] |
| 168 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` — `regenerate_fixture_database` | independent | [docs/registry/implementation-map.yaml:L11604-L11619] |
| 169 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` — `RaoRegistry.register_rao` | caller | [docs/registry/implementation-map.yaml:L11620-L11633] |
| 170 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` — `RaoRegistry._store_content` | independent | [docs/registry/implementation-map.yaml:L11634-L11648] |
| 171 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` — `RaoRegistry.load_rao_data` | caller | [docs/registry/implementation-map.yaml:L11649-L11662] |
| 172 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` — `_read_payload` | independent | [docs/registry/implementation-map.yaml:L11663-L11674] |
| 173 | `src/digitalmodel/hydrodynamics/interpolator.py` — `CoefficientsInterpolator.load_raos` | independent | [docs/registry/implementation-map.yaml:L11675-L11686] |
| 174 | `src/digitalmodel/hydrodynamics/interpolator.py` — `CoefficientsInterpolator.interpolate_rao_1d` | independent | [docs/registry/implementation-map.yaml:L11687-L11698] |
| 175 | `src/digitalmodel/hydrodynamics/interpolator.py` — `CoefficientsInterpolator.interpolate_rao_2d` | independent | [docs/registry/implementation-map.yaml:L11699-L11710] |
| 176 | `src/digitalmodel/hydrodynamics/interpolator.py` — `CoefficientsInterpolator.interpolate_all_dofs` | caller | [docs/registry/implementation-map.yaml:L11711-L11724] |
| 177 | `src/digitalmodel/hydrodynamics/interpolator.py` — `CoefficientsInterpolator.extract_rao_at_frequency` | independent | [docs/registry/implementation-map.yaml:L11725-L11736] |
| 178 | `src/digitalmodel/hydrodynamics/interpolator.py` — `CoefficientsInterpolator.extract_rao_at_direction` | independent | [docs/registry/implementation-map.yaml:L11737-L11748] |
| 179 | `src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py` — `run_parametric_sweep` | independent | [docs/registry/implementation-map.yaml:L11749-L11762] |
| 180 | `src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py` — `_to_rao_db_entry` | independent | [docs/registry/implementation-map.yaml:L11763-L11774] |
| 181 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` — `RAOAnalysis.read_orcaflex_displacement_raos` | independent | [docs/registry/implementation-map.yaml:L11775-L11786] |
| 182 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` — `RAOAnalysis.get_rao_data_for_vessel` | independent | [docs/registry/implementation-map.yaml:L11787-L11798] |
| 183 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` — `router` | caller | [docs/registry/implementation-map.yaml:L11799-L11812] |
| 184 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` — `_database` | independent | [docs/registry/implementation-map.yaml:L11813-L11824] |
| 185 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` — `_rao_array` | independent | [docs/registry/implementation-map.yaml:L11825-L11836] |
| 186 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` — `_interpolated_rows` | caller | [docs/registry/implementation-map.yaml:L11837-L11850] |
| 187 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` — `_check_query_bounds` | independent | [docs/registry/implementation-map.yaml:L11851-L11862] |
| 188 | `src/digitalmodel/installation/installation_pamphlet/calculations.py` — `_extract_rao_dict` | independent | [docs/registry/implementation-map.yaml:L11863-L11874] |
| 189 | `src/digitalmodel/installation/installation_pamphlet/calculations.py` — `load_rao_set_from_artifact` | independent | [docs/registry/implementation-map.yaml:L11875-L11888] |
| 190 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` — `crane_tip_raos` | independent | [docs/registry/implementation-map.yaml:L11889-L11900] |
| 191 | `src/digitalmodel/marine_ops/installation/models.py` — `Vessel.has_complete_raos` | independent | [docs/registry/implementation-map.yaml:L11901-L11912] |
| 192 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` — `AQWAEnhancedParser.parse_lis_file` | caller | [docs/registry/implementation-map.yaml:L11913-L11926] |
| 193 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` — `AQWAEnhancedParser._parse_rao_section_enhanced` | independent | [docs/registry/implementation-map.yaml:L11927-L11938] |
| 194 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` — `AQWAEnhancedParser._extract_section_lines` | independent | [docs/registry/implementation-map.yaml:L11939-L11950] |
| 195 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` — `AQWAEnhancedParser.extract_drag_table` | caller | [docs/registry/implementation-map.yaml:L11951-L11964] |
| 196 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` — `AQWAEnhancedParser.export_to_csv` | independent | [docs/registry/implementation-map.yaml:L11965-L11976] |
| 197 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` — `AQWAReader.parse_lis_file` | caller | [docs/registry/implementation-map.yaml:L11977-L11990] |
| 198 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` — `AQWAReader._parse_rao_section` | independent | [docs/registry/implementation-map.yaml:L11991-L12002] |
| 199 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` — `AQWAReader._extract_last_rao_section` | independent | [docs/registry/implementation-map.yaml:L12003-L12014] |
| 200 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` — `AQWAReader._parse_frequencies` | independent | [docs/registry/implementation-map.yaml:L12015-L12026] |
| 201 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` — `AQWAReader._parse_headings` | independent | [docs/registry/implementation-map.yaml:L12027-L12038] |
| 202 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader_fixed.py` — `AQWAReaderFixed.parse_lis_file` | caller | [docs/registry/implementation-map.yaml:L12039-L12052] |
| 203 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader_fixed.py` — `AQWAReaderFixed._parse_rao_section_fixed` | independent | [docs/registry/implementation-map.yaml:L12053-L12064] |
| 204 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` — `OrcaFlexReader.parse_yml_file` | caller | [docs/registry/implementation-map.yaml:L12065-L12078] |
| 205 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` — `OrcaFlexReader._parse_orcaflex_rao_data` | wrapper | [docs/registry/implementation-map.yaml:L12079-L12095] |
| 206 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` — `OrcaFlexReader._extract_headings` | independent | [docs/registry/implementation-map.yaml:L12096-L12107] |
| 207 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` — `OrcaFlexReader._extract_frequencies` | independent | [docs/registry/implementation-map.yaml:L12108-L12119] |
| 208 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` — `OrcaFlexReader._fill_rao_values` | independent | [docs/registry/implementation-map.yaml:L12120-L12131] |
| 209 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` — `OrcaFlexReader._parse_table_format` | independent | [docs/registry/implementation-map.yaml:L12132-L12143] |
| 210 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` — `AQWALISParser.parse_lis_file` | caller | [docs/registry/implementation-map.yaml:L12144-L12159] |
| 211 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` — `AQWALISParser._extract_displacement_raos` | caller | [docs/registry/implementation-map.yaml:L12160-L12173] |
| 212 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` — `AQWALISParser._extract_velocity_raos` | caller | [docs/registry/implementation-map.yaml:L12174-L12186] |
| 213 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` — `AQWALISParser._extract_acceleration_raos` | caller | [docs/registry/implementation-map.yaml:L12187-L12199] |
| 214 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` — `AQWALISParser._parse_rao_section` | independent | [docs/registry/implementation-map.yaml:L12200-L12211] |
| 215 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` — `OrcaFlexYMLParser.parse_yml_file` | caller | [docs/registry/implementation-map.yaml:L12212-L12226] |
| 216 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` — `OrcaFlexYMLParser._navigate_to_draught` | independent | [docs/registry/implementation-map.yaml:L12227-L12238] |
| 217 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` — `OrcaFlexYMLParser._parse_orcaflex_rao_structure` | caller | [docs/registry/implementation-map.yaml:L12239-L12254] |
| 218 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` — `OrcaFlexYMLParser._extract_headings` | copy | [docs/registry/implementation-map.yaml:L12255-L12266] |
| 219 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` — `OrcaFlexYMLParser._extract_frequencies` | independent | [docs/registry/implementation-map.yaml:L12267-L12278] |
| 220 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` — `OrcaFlexYMLParser._fill_rao_values` | independent | [docs/registry/implementation-map.yaml:L12279-L12290] |
| 221 | `src/digitalmodel/marine_ops/marine_analysis/rao_interpolator.py` — `RAOInterpolator.interpolate_2d` | independent | [docs/registry/implementation-map.yaml:L12291-L12302] |
| 222 | `src/digitalmodel/marine_ops/marine_analysis/rao_interpolator.py` — `RAOInterpolator.interpolate_1d_frequency` | independent | [docs/registry/implementation-map.yaml:L12303-L12314] |
| 223 | `src/digitalmodel/marine_ops/marine_analysis/rao_processor.py` — `RAODataProcessor.import_aqwa_lis_file` | independent | [docs/registry/implementation-map.yaml:L12315-L12326] |
| 224 | `src/digitalmodel/marine_ops/marine_analysis/rao_processor.py` — `RAODataProcessor.import_orcaflex_yml_file` | independent | [docs/registry/implementation-map.yaml:L12327-L12338] |
| 225 | `src/digitalmodel/marine_ops/marine_analysis/rao_processor.py` — `RAODataProcessor.get_rao_dataframes` | independent | [docs/registry/implementation-map.yaml:L12339-L12350] |
| 226 | `src/digitalmodel/marine_ops/marine_analysis/rao_processor.py` — `RAODataProcessor.interpolate_rao_data` | independent | [docs/registry/implementation-map.yaml:L12351-L12362] |
| 227 | `src/digitalmodel/marine_ops/marine_analysis/rao_validators.py` — `RAODataValidators._detect_phase_convention` | caller | [docs/registry/implementation-map.yaml:L12363-L12376] |
| 228 | `src/digitalmodel/marine_ops/marine_analysis/rao_validators.py` — `RAODataValidators._find_phase_convention_in_data` | independent | [docs/registry/implementation-map.yaml:L12377-L12388] |
| 229 | `src/digitalmodel/marine_ops/marine_analysis/rao_validators.py` — `RAODataValidators._extract_orcaflex_raos` | independent | [docs/registry/implementation-map.yaml:L12389-L12400] |
| 230 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` — `RAOReaderError.get_user_message` | independent | [docs/registry/implementation-map.yaml:L12401-L12412] |
| 231 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` — `UnifiedRAOReader.read` | caller | [docs/registry/implementation-map.yaml:L12413-L12427] |
| 232 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` — `UnifiedRAOReader.read_aqwa_lis` | wrapper | [docs/registry/implementation-map.yaml:L12428-L12441] |
| 233 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` — `UnifiedRAOReader._detect_format` | independent | [docs/registry/implementation-map.yaml:L12442-L12453] |
| 234 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` — `UnifiedRAOReader._read_aqwa_lis` | independent | [docs/registry/implementation-map.yaml:L12454-L12465] |
| 235 | `src/digitalmodel/marine_ops/marine_analysis/visualization/rao_plotter.py` — `main` | independent | [docs/registry/implementation-map.yaml:L12466-L12477] |
| 236 | `src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py` — `read_rao_workbook` | independent | [docs/registry/implementation-map.yaml:L12478-L12489] |
| 237 | `src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py` — `real_rao_datasets` | independent | [docs/registry/implementation-map.yaml:L12490-L12503] |
| 238 | `src/digitalmodel/marine_ops/vessel_db/promote.py` — `promote` | independent | [docs/registry/implementation-map.yaml:L12504-L12515] |
| 239 | `src/digitalmodel/motion_forecast/rao_adapter.py` — `GridRAO.from_file` | independent | [docs/registry/implementation-map.yaml:L12516-L12529] |
| 240 | `src/digitalmodel/orcawave/reporting/sections/qa_summary.py` — `_run_checks` | independent | [docs/registry/implementation-map.yaml:L12530-L12541] |
| 241 | `src/digitalmodel/parametric/generate.py` — `_rao_interpolator` | independent | [docs/registry/implementation-map.yaml:L12542-L12555] |
| 242 | `src/digitalmodel/solvers/benchmark/solvers.py` — `run_orcawave` | independent | [docs/registry/implementation-map.yaml:L12556-L12567] |
| 243 | `src/digitalmodel/solvers/benchmark/solvers.py` — `run_aqwa` | independent | [docs/registry/implementation-map.yaml:L12568-L12582] |
| 244 | `src/digitalmodel/solvers/orcaflex/library_generator.py` — `csv_row_to_vessel_type` | independent | [docs/registry/implementation-map.yaml:L12583-L12594] |
| 245 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_type_builder.py` — `VesselTypeBuilder.build` | independent | [docs/registry/implementation-map.yaml:L12595-L12606] |
| 246 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py` — `VesselRouter.from_hull_catalog` | independent | [docs/registry/implementation-map.yaml:L12607-L12620] |
| 247 | `src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py` — `LoadVessel.aqwa_group_item` | independent | [docs/registry/implementation-map.yaml:L12621-L12632] |
| 248 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` — `OrcaFlexConverter.convert_to_orcaflex` | independent | [docs/registry/implementation-map.yaml:L12633-L12646] |
| 249 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` — `OrcaFlexConverter._format_load_raos` | independent | [docs/registry/implementation-map.yaml:L12647-L12658] |
| 250 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` — `OrcaFlexConverter._save_compact_format` | independent | [docs/registry/implementation-map.yaml:L12659-L12670] |
| 251 | `src/digitalmodel/solvers/smoke/probes.py` — `check_aqwa` | independent | [docs/registry/implementation-map.yaml:L12671-L12682] |
| 252 | `src/digitalmodel/workflows/mcp_server/orcawave/core/mcp_server.py` — `OrcaWaveMCPServer._register_tools.export_to_orcaflex` | independent | [docs/registry/implementation-map.yaml:L12683-L12696] |
| 253 | `tests/drilling_riser/global_model/test_nonlinear_vessel_foundation.py` — `_vessel_motion` | test_helper | [docs/registry/implementation-map.yaml:L12697-L12710] |
| 254 | `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py` — `<module>` | test_helper | [docs/registry/implementation-map.yaml:L12711-L12721] |
| 255 | `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py` — `_FakeLISParser.parse_lis_file` | test_helper | [docs/registry/implementation-map.yaml:L12722-L12733] |
| 256 | `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py` — `_run_case` | test_helper | [docs/registry/implementation-map.yaml:L12734-L12745] |
| 257 | `tests/hydrodynamics/bemrosetta/conftest.py` — `sample_aqwa_file` | test_helper | [docs/registry/implementation-map.yaml:L12746-L12759] |
| 258 | `tests/hydrodynamics/bemrosetta/test_cli.py` — `temp_lis_file` | test_helper | [docs/registry/implementation-map.yaml:L12760-L12773] |
| 259 | `tests/hydrodynamics/bemrosetta/test_parsers.py` — `temp_lis_file` | test_helper | [docs/registry/implementation-map.yaml:L12774-L12787] |
| 260 | `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py` — `_load_reference_data` | test_helper | [docs/registry/implementation-map.yaml:L12788-L12799] |
| 261 | `tests/hydrodynamics/diffraction/conftest.py` — `l00_owr_path` | test_helper | [docs/registry/implementation-map.yaml:L12800-L12813] |
| 262 | `tests/hydrodynamics/diffraction/conftest.py` — `l01_owr_path` | test_helper | [docs/registry/implementation-map.yaml:L12814-L12827] |
| 263 | `tests/hydrodynamics/diffraction/test_aqwa_lis_coefficient_grid.py` — `_synthetic` | test_helper | [docs/registry/implementation-map.yaml:L12828-L12839] |
| 264 | `tests/hydrodynamics/diffraction/test_aqwa_parser.py` — `<module>` | test_helper | [docs/registry/implementation-map.yaml:L12840-L12850] |
| 265 | `tests/hydrodynamics/diffraction/test_aqwa_runner.py` — `_patch_extract` | test_helper | [docs/registry/implementation-map.yaml:L12851-L12862] |
| 266 | `tests/hydrodynamics/diffraction/test_matrix_unit_completeness.py` — `_fake_diffraction` | test_helper | [docs/registry/implementation-map.yaml:L12863-L12874] |
| 267 | `tests/hydrodynamics/diffraction/test_orcawave_runner.py` — `_make_fake_diffraction` | test_helper | [docs/registry/implementation-map.yaml:L12875-L12886] |
| 268 | `tests/hydrodynamics/diffraction/test_orcawave_runner.py` — `_make_fake_diffraction._install` | test_helper | [docs/registry/implementation-map.yaml:L12887-L12898] |
| 269 | `tests/hydrodynamics/diffraction/test_orcawave_runner_units_boundary.py` — `_fake_diffraction` | test_helper | [docs/registry/implementation-map.yaml:L12899-L12910] |
| 270 | `tests/hydrodynamics/diffraction/test_report_generator_golden.py` — `_full_basic` | test_helper | [docs/registry/implementation-map.yaml:L12911-L12922] |
| 271 | `tests/hydrodynamics/diffraction/test_report_generator_golden.py` — `_benchmark` | test_helper | [docs/registry/implementation-map.yaml:L12923-L12934] |
| 272 | `tests/hydrodynamics/diffraction/test_solver_orcawave_data_extraction.py` — `TestValidateDataConsistency.test_mismatched_frequencies_logged` | test_helper | [docs/registry/implementation-map.yaml:L12935-L12946] |
| 273 | `tests/hydrodynamics/diffraction/test_solver_report_extractors.py` — `_mock_diffraction_object` | test_helper | [docs/registry/implementation-map.yaml:L12947-L12958] |
| 274 | `tests/hydrodynamics/hull_library/test_rao_fixture_metadata.py` — `repository_pair` | test_helper | [docs/registry/implementation-map.yaml:L12959-L12973] |
| 275 | `tests/hydrodynamics/hull_library/test_rao_fixture_population.py` — `test_ship_periods_convert_and_responses_stay_aligned` | test_helper | [docs/registry/implementation-map.yaml:L12974-L12986] |
| 276 | `tests/hydrodynamics/hull_library/test_rao_lookup_plots.py` — `_make_entry` | test_helper | [docs/registry/implementation-map.yaml:L12987-L12998] |
| 277 | `tests/hydrodynamics/hull_library/test_rao_registry.py` — `TestPanelCatalogRaoRoundTrip.test_csv_export_with_raos` | test_helper | [docs/registry/implementation-map.yaml:L12999-L13010] |
| 278 | `tests/hydrodynamics/rao_analysis/test_rao_analysis.py` — `run_rao_analysis` | test_helper | [docs/registry/implementation-map.yaml:L13011-L13022] |
| 279 | `tests/hydrodynamics/test_rao_tabulation_workflow.py` — `_base_cfg` | test_helper | [docs/registry/implementation-map.yaml:L13023-L13034] |
| 280 | `tests/marine_ops/installation/conftest.py` — `vessel` | test_helper | [docs/registry/implementation-map.yaml:L13035-L13048] |
| 281 | `tests/marine_ops/installation/test_crane_tip_motion.py` — `TestCraneTipRAOs.test_heave_transfer_formula` | test_helper | [docs/registry/implementation-map.yaml:L13049-L13062] |
| 282 | `tests/marine_ops/marine_analysis/test_rao_quality_checks.py` — `create_synthetic_orcaflex_rao_data` | test_helper | [docs/registry/implementation-map.yaml:L13063-L13074] |
| 283 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` — `RAOVerification._find_test_files` | test_helper | [docs/registry/implementation-map.yaml:L13075-L13086] |
| 284 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` — `RAOVerification.verify_file` | test_helper | [docs/registry/implementation-map.yaml:L13087-L13101] |
| 285 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` — `RAOVerification._extract_unified_data` | test_helper | [docs/registry/implementation-map.yaml:L13102-L13113] |
| 286 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` — `RAOVerification._extract_legacy_data` | test_helper | [docs/registry/implementation-map.yaml:L13114-L13125] |
| 287 | `tests/orcawave/conftest.py` — `mock_diff_single_body` | test_helper | [docs/registry/implementation-map.yaml:L13126-L13139] |
| 288 | `tests/orcawave/conftest.py` — `mock_diff_two_body` | test_helper | [docs/registry/implementation-map.yaml:L13140-L13153] |
| 289 | `tests/orcawave/test_builder_orchestration.py` — `owr_path` | test_helper | [docs/registry/implementation-map.yaml:L13154-L13167] |
| 290 | `tests/orcawave/test_report_builder.py` — `builder` | test_helper | [docs/registry/implementation-map.yaml:L13168-L13181] |
| 291 | `tests/postprocess/test_build_results_workbook.py` — `_orcawave_dir` | test_helper | [docs/registry/implementation-map.yaml:L13182-L13193] |
| 292 | `tests/solver/smoke_test.py` — `<module>` | test_helper | [docs/registry/implementation-map.yaml:L13194-L13204] |
| 293 | `tests/solver/smoke_test.py` — `promote_committed_artifacts` | test_helper | [docs/registry/implementation-map.yaml:L13205-L13216] |
| 294 | `tests/solver/smoke_test.py` — `run_l00_smoke_test` | test_helper | [docs/registry/implementation-map.yaml:L13217-L13228] |
| 295 | `tests/solver/smoke_test.py` — `run_l01_smoke_test` | test_helper | [docs/registry/implementation-map.yaml:L13229-L13240] |


## 10. Coverage and open items

Completed source coverage: the package's single Python file, including all definitions, imports, argument handling, validation branches, import policy and save path; no subpackage is excluded from this package scope. [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L1-L100]

Related test coverage read: the complete mock smoke file and its commented comparisons; the existing routing-contract tests cover registry parsing, domain coverage, duplicate-key refusal and the existing exact solver-submodule list. [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L1-L53] [tests/docs/test_digitalmodel_routing_contract.py:L17-L40] [tests/docs/test_digitalmodel_routing_contract.py:L124-L140] [tests/docs/test_digitalmodel_routing_contract.py:L188-L254]

Documentation discovery searched `docs/` for `load_vessel` and the exact package path; the following files were read at their matching contexts. Broader `preprocess` matches describe other packages and are not evidence for this helper.

| Discovered document | What the matching context supports |
| --- | --- |
| `docs/domains/test_automation/aqwa_mock_patterns.md` | Mock engine pattern and the related test basename; its success-count statements are historical documentation claims, not numerical validation. [docs/domains/test_automation/aqwa_mock_patterns.md:L30-L45] [docs/domains/test_automation/aqwa_mock_patterns.md:L75-L93] |
| `docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md` | Historical test-path mention, not a current execution result. [docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md:L80-L87] |
| `docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md` | Mentions this file among excluded follow-ups; the local helper actually raises on an import exception. [docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md:L510-L515] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L49-L54] |
| `docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md` | A different `load_vessel` method, not this importer. [docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md:L38-L38] |
| `docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md` | A different `orcaflex_api.load_vessel` usage example. [docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md:L323-L330] |
| `docs/domains/pipeline-installation/coverage-2026-10.md` | A different `load_vessels` catalogue helper. [docs/domains/pipeline-installation/coverage-2026-10.md:L469-L469] |

| Open item / responsible review role | Evidence and tracking |
| --- | --- |
| Configuration shape, unsupported selector and empty iterables / family quality manager | C01, C04; issue pending (1). [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L12-L22] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L58-L65] |
| Missing-file diagnostic failures / family quality manager | C02; issue pending (2). [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L38-L47] |
| Process-argument coupling / family quality manager | C03; issue pending (3). [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L29-L35] |
| Unverified source/template units, origin, signs and phase; unverified program versions and method applicability; import policy, messages and output containment / family and diffraction quality managers | Both section-2/3 UNVERIFIED claims and C05-C10; issue pending (4). [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L49-L100] |
| Unverified numerical equivalence, missing independent worked examples, direct helper tests and downstream G1-G5 evidence / family quality manager | Section-4 UNVERIFIED claim, smoke case and C10-C14; issue pending (5). [tests/solvers/orcaflex/file_preparation/test_load_vessel_aqwa.py:L19-L50] [src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py:L81-L100] |
| External implementation-map gap, vendor-helper classification and discovery codenaming / family quality manager | Map follow-up in section 9; issue pending (6). [scripts/python/digitalmodel/analysis/AQWAVesselTypeImport.py:L1-L39] |
| Registry growth requires an explicit contract-inventory update / maintainer | The routing test compares the complete solver-submodule mapping with sixteen enumerated entries, which omit this package; issue pending (7). [tests/docs/test_digitalmodel_routing_contract.py:L227-L254] |

Future reference examples must use generic synthetic inputs with independently established expectations; do not use operational values from discovery files.
