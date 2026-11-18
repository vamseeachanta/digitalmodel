# Blender automation knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/blender_automation |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/solvers/blender_automation` |
| Pack revision | 1 |
| Code revision | `f1d58a2ac0db65e68b5b690f0a609ef4746d114e` |
| Analysis family | CFD; registry code CF |
| Quality role | CFD family quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The public API exports a subprocess wrapper, scene manager, CAD importer, mesh exporter, batch processor and installation helpers. [src/digitalmodel/solvers/blender_automation/__init__.py:L6-L25]
The wrapper constructs Python expressions for Blender; `bpy` occurs inside generated scripts rather than as a direct wrapper import. [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L85-L104] [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L170-L184]
The importer routes six mesh/interchange extensions directly and four STEP/IGES extensions through optional FreeCAD conversion. [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L24-L35] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L90-L105]
Scene operations create objects, cameras, lighting, materials and rendered images. [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L28-L52] [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L54-L109] [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L111-L155] [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L157-L251] [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L253-L296]
The containing solvers domain has maturity `beta`; there is no separate Blender maturity field at this pin. [docs/registry/module-routing.yaml:L374-L416]
The marine example describes STL as an OpenFOAM input surface; its route is `CADImporter.import_file` followed by `MeshExporter.export_file`, without an OpenFOAM launch in that workflow. [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L90-L144]
This pack covers the complete package, including both example scripts; example success messages are not evidence of an engineering mesh validation. [src/digitalmodel/solvers/blender_automation/examples/basic_usage.py:L298-L317] [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L377-L400]

## 2. Inputs and outputs

### Inputs

| Name | Unit and range | Default | Source and evidence |
| --- | --- | --- | --- |
| `blender_path`, script, background, input scene | Executable/text/path; no timeout argument or version constraint enforced | `blender`, background true, no scene | [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L22-L50] [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L57-L104] |
| CAD path and suffix | Path must exist; suffix in supported table | No path default | [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L59-L105] |
| CAD `scale`, `cleanup` | Dimensionless scale; no range check; scale is passed but unused by both import routes | 1.0, true | [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L59-L65] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L107-L124] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L126-L179] |
| Object pose | `(x,y,z)` location; Euler rotation in radians; dimensionless scale; no origin/sign or length unit declaration | Location and rotation `(0,0,0)`, scale `(1,1,1)` | [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L54-L100] |
| Camera | Position/target triples; lens mm | Target `(0,0,0)`, lens 50.0 mm | [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L111-L149] |
| Lighting/material | RGB and RGBA documented 0–1; metallic/roughness 0–1; no range enforcement | SUN, energy 1.0; material `(0.8,0.8,0.8,1.0)`, metallic 0.0, roughness 0.5 | [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L157-L189] [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L197-L245] |
| Render settings | Width/height pixels; samples count | 1920 × 1080 px, 128 samples, PNG | [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L253-L294] |
| Export options | Format-specific keys forwarded to Blender; no physical-unit verification | STL scene-unit flag true; OBJ forward `-Z`, up `Y`; FBX `FBX_SCALE_ALL`; GLTF `GLB` | [src/digitalmodel/solvers/blender_automation/converters/mesh_exporter.py:L24-L59] [src/digitalmodel/solvers/blender_automation/converters/mesh_exporter.py:L93-L119] |
| Batch execution | File list, callable; workers count | 4 workers; parallel and continue-on-error true | [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L26-L47] |

### Outputs

| Name | Unit, allowed result and default | Consumer and evidence |
| --- | --- | --- |
| `run_script` result | Boolean `success`, stdout/stderr strings, integer return code; unavailable gives code -1 | Caller must inspect result; subprocess exit success alone supplies no artifact readback | [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L76-L118] |
| Saved scene and mesh | `.blend`; exports STL, OBJ, FBX, GLTF, DAE, PLY, X3D; file units are not established by this package | Blender and downstream geometry tools; format table and raw option forwarding | [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L179-L184] [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L205-L234] |
| Render | Image format and pixel dimensions configured in script | Image consumer; script calls `render(write_still=True)` | [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L279-L296] |
| Batch report | Total/success/failure counts; duration seconds; per-file result list | Caller; success currently counts absence of a raised exception, including returned failure dictionaries | [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L60-L85] [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L99-L117] [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L141-L162] |
| Metadata | Bytes; `size_mb` actually bytes divided by 1024²; modification timestamp and absolute path | `get_file_info` caller | [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L170-L195] |

UNVERIFIED: physical length units, origin, handedness, positive axes and phase conventions across each imported/exported format are not established by an independent readback; caller-specified unit and axis provenance is required before analysis use (issue pending (1)).

## 3. Method and governing standards

| Method | Implementation and limits | Reference basis |
| --- | --- | --- |
| Subprocess launch | `BlenderWrapper.run_script`: optional `--background`, scene path, then `--python-expr`; `output_file` appends `--render-output`, not a save-mainfile statement | Native command construction; no engineering formula [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L85-L118] |
| Import/export dispatch | `BlenderWrapper.import_file` resets the scene and uses a suffix-to-operator table; `export_file` interpolates keyword arguments | Native operator mappings [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L157-L184] [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L205-L234] |
| CAD tessellation and cleanup | `CADImporter._import_via_freecad` selects objects with Shape, tessellates with 0.1, writes temporary STL and imports; `_cleanup_mesh` requests duplicate removal and outward normals in a separate process | Code parameter, not a cited standards tolerance; cleanup result discarded [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L149-L189] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L191-L223] |
| Scene/camera/material | `SceneManager.add_object` assigns rotation/scale; camera uses `to_track_quat('-Z','Y')`; material writes Principled BSDF inputs only when that node exists | Blender operations [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L79-L109] [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L132-L155] [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L222-L251] |
| Export and size check | `MeshExporter.export_file` merges defaults and forwards options; `optimize_for_web` makes one compressed GLB attempt and compares bytes/1024² with the target | Code-derived size criterion, not iterative optimization [src/digitalmodel/solvers/blender_automation/converters/mesh_exporter.py:L93-L119] [src/digitalmodel/solvers/blender_automation/converters/mesh_exporter.py:L299-L329] |
| Batch scheduling/discovery | `BatchProcessor` uses sequential iteration or `ThreadPoolExecutor`; discovery uses suffix globbing; repeated stems produce the same flat output name | Implementation algorithms [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L87-L162] [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L187-L238] [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L110-L167] |

AQG r0, sections 10 and 9, supplies the OpenFOAM gate vocabulary and evidence requirement; section 5 limits its application to this geometry-preparation boundary. (AQG, *Analysis QA gates*, revision 0, sections 9–10.)
UNVERIFIED: no governing engineering-standard edition, clause or standards-derived `Citation` sidecar was identified for the package's geometry operations; no licensed clause text was checked (issue pending (5)).

## 4. Validated worked examples

Validated engineering examples: **0**; the existing scale test asserts only `success`, and scene tests use mocked subprocess results without geometry readback. [tests/solvers/blender_automation/test_cad_importer.py:L117-L135] [tests/solvers/blender_automation/test_scene_manager.py:L126-L155]
Regression examples: **6**, with exact discrete assertions (tolerance 0 count units); none supplies an independently validated mesh volume. [tests/solvers/blender_automation/test_blender_wrapper.py:L99-L106] [tests/solvers/blender_automation/test_batch_processor.py:L43-L57] [tests/solvers/blender_automation/test_batch_processor.py:L76-L92]

| Case | Source and synthetic input | Expected result and tolerance | Existing test node ID | Offline result at code pin |
| --- | --- | --- | --- | --- |
| R1 unavailable wrapper | Mock executable missing; wrapper error contract | `success=False`, error contains “not available”; 0 mismatches | `tests/solvers/blender_automation/test_blender_wrapper.py::TestBlenderWrapperOperations::test_run_script_on_unavailable_wrapper` | PASS [tests/solvers/blender_automation/test_blender_wrapper.py:L168-L176] |
| R2 failed script | Mock-only subprocess-error contract; fixture succeeds for `--version` and raises for other commands | False success and 1 error key; 0 mismatches; native script exceptions are UNVERIFIED | `tests/solvers/blender_automation/test_blender_wrapper.py::TestBlenderWrapperOperations::test_script_error_handling` | PASS [tests/solvers/blender_automation/test_blender_wrapper.py:L99-L106] |
| R3 scale request | Test-created triangle OBJ; scale 2.0 | Boolean success true; 0 mismatches; **does not assert scale** | `tests/solvers/blender_automation/test_cad_importer.py::TestCADImporter::test_import_with_scale` | PASS [tests/solvers/blender_automation/test_cad_importer.py:L117-L125] |
| R4 sequential list | 5 synthetic OBJ paths; callback returns metadata | Total 5, successful 5, failed 0, records 5; exact counts | `tests/solvers/blender_automation/test_batch_processor.py::TestBatchProcessor::test_process_files_sequential` | PASS [tests/solvers/blender_automation/test_batch_processor.py:L29-L57] |
| R5 exception accounting | Same 5 paths; one callback raises | Total 5, successful 4, failed 1; exact counts | `tests/solvers/blender_automation/test_batch_processor.py::TestBatchProcessor::test_process_files_with_error` | PASS [tests/solvers/blender_automation/test_batch_processor.py:L76-L92] |
| R6 recursive discovery | 3 files across nested directories | Recursive 3 files, flat 1 file; exact counts | `tests/solvers/blender_automation/test_file_utils.py::TestFileUtils::test_find_cad_files_recursive` | PASS [tests/solvers/blender_automation/test_file_utils.py:L182-L197] |

Historical execution record, 2026-10-18: BASE module invocation collected 60 nodes, 58 PASS, 0 FAIL, 2 SKIP, 2 native cases NOT_RUN (the NOT_RUN classification describes those same skipped nodes, not additional tests); the suite includes mocked and availability-tolerant tests. [tests/solvers/blender_automation/conftest.py:L51-L66] [tests/solvers/blender_automation/test_blender_wrapper.py:L201-L220]
AFTER module invocation at the same code pin: 60 collected, 58 PASS, 0 FAIL, 2 SKIP; the same 2 native nodes remain NOT_RUN because Blender is absent. Supplementary checks before and after the documentation edit: 499 PASS, 0 FAIL, 0 SKIP, 0 NOT_RUN per run, comprising the two temporary-file parameter cases, routing contract and the tests-contracts directory. [tests/security/test_review_20260523_remaining.py:L86-L106] [tests/docs/test_digitalmodel_routing_contract.py:L1-L108]
Command form: `PYTHONPATH=src uv run --no-sources python -m pytest tests/solvers/blender_automation -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-base`; the executable was invoked by its full configured path; no addopts override was used. `--basetemp` directs pytest fixtures only; these tests also use `tempfile.TemporaryDirectory()`, which follows `TMPDIR` independently. The supplementary command substitutes `tests/security/test_review_20260523_remaining.py::test_blender_temp_files_no_mktemp`, `tests/docs/test_digitalmodel_routing_contract.py` and `tests/contracts` for the module path with a separate scratch basetemp. The historical run did not record its OS or Python version, so its cross-platform applicability is not established.

Current execution record, 2026-10-19: Linux, Python 3.11.14, pytest 8.4.2 and Blender 4.0.2. BASE collected 560 tests: 558 PASS, 1 FAIL (the combined test-path span), 1 SKIP (installed-package metadata unavailable). All 60 module cases passed, including the 2 native wrapper cases; supplementary checks had 498 PASS and the metadata SKIP. AFTER: all 60 module cases passed; supplementary checks again had 498 PASS and the same metadata SKIP. The focused node-ID check passed after splitting the span into separate citations. The broader run collected 638 tests: 606 PASS, 31 FAIL in untouched packs, and the metadata SKIP; its node-ID subset had 48 PASS and 31 FAIL, including collection errors from missing declared dependencies. After declared runtime dependencies were supplied, the complete node-ID file had 78 PASS and 1 FAIL because the declared pytest-timeout plugin was missing. Installing that plugin and retrying only the affected case gave 1 PASS; all 79 node-ID cases therefore have passing evidence across these final runs. The edited pack passed in every AFTER invocation. `TMPDIR` was exported for standard-library temporary directories, separately from pytest basetemp. No Windows execution or native geometry readback was performed.

UNVERIFIED: native exit status when a Blender script raises is not established; R2 uses a fixture that raises CalledProcessError after a successful version probe, while the native command has no `--python-exit-code` (S51; issue pending (4)). [tests/solvers/blender_automation/conftest.py:L37-L45] [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L85-L118]

## 5. QA gates

Gate applicability is for geometry preparation, not CFD-run clearance: G0 OpenFOAM dictionaries/boundaries/dimensions and G1 solver mesh validation remain downstream responsibilities. (AQG, *Analysis QA gates*, revision 0, section 10, OpenFOAM row.)

| Gate | Applicability and route | Module checks and disposition |
| --- | --- | --- |
| G0 | Applicable to source geometry, suffix, metadata and unit provenance; this module writes Blender scripts rather than OpenFOAM dictionaries | C0.1–C0.3 PASS; physical-unit assurance remains open, issue pending (1) [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L78-L105] [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L170-L213] |
| G1 | Applicable to Blender loading and exported geometry checking; native route is `BlenderWrapper.run_script` → Blender `--background` / `--python-expr`, import operator then save only when `output_blend` is supplied; export route opens the `.blend` then dispatches export operator; downstream OpenFOAM mesh check is outside this package | C1.1–C1.2 PASS only for error handling; C1.3 PASS in the current Linux run (historically NOT_RUN); no native mesh readback check exists, #2670 [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L85-L104] [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L157-L184] [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L205-L234] |
| G2 | NOT_APPLICABLE: this package's CFD example exports geometry; it sets no hydrostatic initial field | Downstream hydrostatic initialisation requires its own evidence [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L120-L144] |
| G3 | NOT_APPLICABLE: the same geometry export workflow performs no solver time step and computes no Courant number | Downstream short-run and Courant checks require their own evidence [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L120-L144] |
| G4 | NOT_APPLICABLE for the OpenFOAM family | AQG, *Analysis QA gates*, revision 0, section 10, OpenFOAM row |
| G5 | Applicable to evidence and batch-accounting records; no report-appendix generator in the illustrated conversion workflow | C5.1–C5.3 PASS within their stated limits; a production appendix and fingerprints remain open, issue pending (5) [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L79-L85] [src/digitalmodel/solvers/blender_automation/examples/basic_usage.py:L195-L211] |

Checks below specify discrete pass criteria; a PASS of an error-contract test does not establish native geometry quality. [tests/solvers/blender_automation/conftest.py:L18-L38]

| Check | Executable procedure | Numeric pass criterion and unit | Evidence / result | Gate |
| --- | --- | --- | --- | --- |
| C0.1 | `tests/solvers/blender_automation/test_cad_importer.py::TestCADImporter::test_unsupported_format_raises_error` | 1 ValueError for unknown suffix; 0 missed exceptions | pytest PASS [tests/solvers/blender_automation/test_cad_importer.py:L69-L75] | G0 |
| C0.2 | `tests/solvers/blender_automation/test_file_utils.py::TestFileUtils::test_get_file_info` | Absolute byte-count error = 0 bytes versus encoded fixture | pytest PASS on Linux only; Windows text-mode CRLF gives 34 bytes versus the asserted 31 bytes [tests/solvers/blender_automation/test_file_utils.py:L106-L120] | G0 |
| C0.3 | R6 | Recursive count 3 files; flat count 1 file | pytest PASS [tests/solvers/blender_automation/test_file_utils.py:L182-L197] | G0 |
| C1.1 | R1 | Unavailable wrapper produces 1 failure result and 0 successes | pytest PASS [tests/solvers/blender_automation/test_blender_wrapper.py:L168-L176] | G1 |
| C1.2 | R2 | Mock-only subprocess-error result contains 1 error key and 0 successes; native script-exception status is UNVERIFIED | pytest PASS with mocked Blender [tests/solvers/blender_automation/test_blender_wrapper.py:L99-L106] | G1 |
| C1.3 | `tests/solvers/blender_automation/test_blender_wrapper.py::TestBlenderReal::test_real_version_detection` and `tests/solvers/blender_automation/test_blender_wrapper.py::TestBlenderReal::test_real_simple_script` | 2 passing native cases, 0 missing “Real test” stdout markers | Current Linux run: 2 PASS with Blender 4.0.2; historical run: SKIP / NOT_RUN because Blender was absent [tests/solvers/blender_automation/test_blender_wrapper.py:L201-L220] | G1 |
| C5.1 | R4 | 5 input paths = 5 records = 5 successful callbacks; 0 failures | pytest PASS; returned failure dictionaries are outside this assertion [tests/solvers/blender_automation/test_batch_processor.py:L43-L57] | G5 |
| C5.2 | R5 | 4 successes + 1 failure = 5 input paths | pytest PASS for raised exceptions [tests/solvers/blender_automation/test_batch_processor.py:L76-L92] | G5 |
| C5.3 | Audit section-4 rows against pytest node results and code pin | 6 of 6 rows resolve; 0 missing results; 0 pin mismatches | Scratch citation/node audit; recorded result table above [tests/solvers/blender_automation/test_blender_wrapper.py:L99-L106] [tests/solvers/blender_automation/test_cad_importer.py:L117-L125] [tests/solvers/blender_automation/test_batch_processor.py:L43-L92] [tests/solvers/blender_automation/test_file_utils.py:L182-L197] | G5 |

## 6. Known failure modes and pitfalls

sweep: 53 found

The sweep includes generated Blender script strings and both examples; rows distinguish explicit error contracts, silent omission risks and convention defaults rather than treating every match as a defect. [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L48-L50] [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L222-L245] [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L171-L194]
Exact package sweep commands, evaluated on the pinned `.py` files:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|\b(int|float)\(|= 0(\.0)?\b|1970|0\.05|units?|scale|cache' src/digitalmodel/solvers/blender_automation -g '*.py'
rg -n 'pass|break|if |get\(|\[:|success|default|tessellate|return' src/digitalmodel/solvers/blender_automation -g '*.py'
```

| ID | Symptom / cause at pin | Detection | Tracking / date |
| --- | --- | --- | --- |
| S01 | Installation probe suppresses CalledProcessError/FileNotFoundError/OSError, clearing version and availability [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L39-L50] | C1.1 covers missing executable; no detection check exists for every caught error | issue pending (4); 2026-10-18 |
| S02 | CalledProcessError becomes a failure dictionary; caller must inspect it; mock-only evidence does not establish native script-exception status [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L98-L118] | C1.2 (mock only); no detection check exists for native script-exception status | issue pending (4); 2026-10-18 |
| S03 | Missing FreeCAD becomes false availability and a warning; STEP request then raises [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L47-L57] | no detection check exists for all availability failures; existing test checks STEP refusal | issue pending (4); 2026-10-18 |
| S04 | FreeCAD conversion catches all exceptions as failure dictionaries [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L181-L189] | no detection check exists | issue pending (2); 2026-10-18 |
| S05 | Cleanup return is discarded; caller can retain original import success despite cleanup failure [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L121-L124] | no detection check exists | issue pending (2); 2026-10-18 |
| S06 | FreeCAD route likewise discards cleanup failure [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L176-L179] | no detection check exists | issue pending (2); 2026-10-18 |
| S07 | Cleanup returns None implicitly and neither saves edited geometry nor returns process status [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L191-L223] | no detection check exists | issue pending (2); 2026-10-18 |
| S08 | Batch import catches exceptions per file and continues, retaining aggregate counts [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L292-L311] | no detection check exists for exception aggregation | issue pending (2); 2026-10-18 |
| S09 | Batch export catches exceptions per file, writing output None on error [src/digitalmodel/solvers/blender_automation/converters/mesh_exporter.py:L283-L297] | no detection check exists | issue pending (4); 2026-10-18 |
| S10 | Sequential callback exceptions become error rows; continue-on-error defaults true [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L99-L117] | C5.2; stop-on-error test checks exactly 2 calls | issue pending (3); 2026-10-18 |
| S11 | Parallel callback exceptions become error rows; cancellation breaks collection even if some tasks already ran [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L141-L162] | no detection check exists for cancellation completeness | issue pending (3); 2026-10-18 |
| S12 | Missing which command is swallowed before trying where [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L40-L50] | no detection check exists | issue pending (4); 2026-10-18 |
| S13 | Missing where command is swallowed [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L52-L62] | no detection check exists | issue pending (4); 2026-10-18 |
| S14 | Executable discovery returns None after all candidates fail [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L23-L64] | C1.1 covers wrapper unavailability, not discovery fallback | issue pending (4); 2026-10-18 |
| S15 | Installation verification with missing executable returns installed false and null path/version [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L74-L82] | no detection check exists for exact missing result fields | issue pending (4); 2026-10-18 |
| S16 | Installation subprocess failure becomes installed false with error string [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L84-L107] | no detection check exists for failure branches | issue pending (4); 2026-10-18 |
| S17 | CAD direct import accepts scale default 1.0 but never applies any requested scale [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L107-L124] | no detection check exists; R3 asserts success only | issue pending (1); 2026-10-18 |
| S18 | FreeCAD route ignores requested scale and tessellates at fixed 0.1 without declared length unit [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L126-L179] | no detection check exists | issue pending (1); 2026-10-18 |
| S19 | FreeCAD Shape selection silently omits objects lacking Shape; all-empty shapes raises RuntimeError internally, caught and returned as a failure dictionary [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L153-L165] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L181-L186] | no detection check exists | issue pending (2); 2026-10-18 |
| S20 | Default object position/rotation zeros and scale ones have no declared physical coordinate frame [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L54-L100] | no detection check exists for physical units | issue pending (1); 2026-10-18 |
| S21 | Default camera target origin and 50.0 mm lens are visualization conventions, not checked geometry [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L111-L149] | no detection check exists for physical frame/optics readback | issue pending (1); 2026-10-18 |
| S22 | Lighting defaults energy 1.0, white and location (0,0,10); intensity physical unit is unspecified [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L157-L189] | no detection check exists for physical light readback | issue pending (1); 2026-10-18 |
| S23 | Material defaults metallic 0.0 and roughness 0.5; missing BSDF node silently skips assignments [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L197-L245] | no detection check exists | issue pending (1); 2026-10-18 |
| S24 | STL defaults binary and ascii true in generic table; scene-unit flag assumes a meaningful scene scale [src/digitalmodel/solvers/blender_automation/converters/mesh_exporter.py:L24-L29] | no detection check exists for operator compatibility or units | issue pending (1); 2026-10-18 |
| S25 | OBJ defaults forward -Z and up Y without physical frame provenance [src/digitalmodel/solvers/blender_automation/converters/mesh_exporter.py:L30-L35] | no detection check exists | issue pending (1); 2026-10-18 |
| S26 | FBX default FBX_SCALE_ALL is forwarded without dimension readback [src/digitalmodel/solvers/blender_automation/converters/mesh_exporter.py:L36-L40] | no detection check exists | issue pending (1); 2026-10-18 |
| S27 | Basic examples catch all exceptions, print traceback and return normally [src/digitalmodel/solvers/blender_automation/examples/basic_usage.py:L303-L317] | no detection check exists | issue pending (6); 2026-10-18 |
| S28 | Marine examples catch all exceptions, print traceback and return normally [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L382-L400] | no detection check exists | issue pending (6); 2026-10-18 |
| S29 | Mooring example filters absent component paths, so partial assemblies can proceed; repeated imports also replace earlier components (S50) [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L171-L194] | no detection check exists | issue pending (6); 2026-10-18 |
| S30 | Catalog processes only the first 5 discovered equipment paths [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L292-L299] | no detection check exists | issue pending (6); 2026-10-18 |
| S31 | Discovery globs return empty lists on no matches; matching is case-sensitive on Linux and case-insensitive on Windows; no missing-input-directory test exists for conversion [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L187-L197] | no detection check exists for accidental zero-work conversion | issue pending (3); 2026-10-18 |
| S32 | Callback returned failure dictionary is counted successful in sequential execution [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L99-L105] | no detection check exists; C5.2 covers exceptions only | issue pending (3); 2026-10-18 |
| S33 | Callback returned failure dictionary is counted successful in parallel execution [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L141-L147] | no detection check exists | issue pending (3); 2026-10-18 |
| S34 | Render settings assign samples only for CYCLES or BLENDER_EEVEE, leaving other engines unchanged [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L286-L296] | no detection check exists for native render settings | issue pending (4); 2026-10-18 |
| S35 | Batch renderer repeats the same engine-specific sample-setting fallback [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L268-L289] | no detection check exists | issue pending (4); 2026-10-18 |
| S36 | Default empty scene puts camera and sun at fixed unlabelled length coordinates [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L38-L47] | no detection check exists for coordinate-unit readback | issue pending (1); 2026-10-18 |
| S37 | Basic scene example ignores intermediate operation results and prints scene-created regardless [src/digitalmodel/solvers/blender_automation/examples/basic_usage.py:L70-L118] | no detection check exists | issue pending (6); 2026-10-18 |
| S38 | Platform visualization ignores lighting/camera/render results before printing saved [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L48-L83] | no detection check exists | issue pending (6); 2026-10-18 |
| S39 | Equipment callback ignores import/setup/render failures and returns metadata as a successful operation [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L251-L290] | no detection check exists | issue pending (6); 2026-10-18 |
| S40 | Mooring example ignores scene/setup results before reporting assembly-created; each component import resets the scene, retaining only the last component (S50) [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L180-L211] | no detection check exists | issue pending (6); 2026-10-18 |
| S41 | Empty BatchProcessor input returns zero success/failure; valid empty case cannot distinguish unintended no work [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L60-L85] | Existing empty-list test asserts 0/0/0 counts; no detection check exists for unintended omission | issue pending (3); 2026-10-18 |
| S42 | find_blend_files returns [] for an existing directory without matching suffixes [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L124-L132] | no detection check exists for unintended no-match | issue pending (3); 2026-10-18 |
| S43 | find_cad_files returns [] for no matches, excludes DAE/PLY/X3D by default and uses suffix globbing that is case-sensitive on Linux and case-insensitive on Windows [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L151-L167] | C0.3 covers positive discovery; no detection check exists for omissions | issue pending (3); 2026-10-18 |
| S44 | Web-size default 5.0 is compared with bytes/1024², labelled MB; there is one attempt without size reduction retry [src/digitalmodel/solvers/blender_automation/converters/mesh_exporter.py:L299-L329] | no detection check exists | issue pending (4); 2026-10-18 |
| S45 | Completed counter starts at 0 for parallel accounting, rather than filling missing physical input [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L128-L139] | no detection check exists for missing completion records | issue pending (3); 2026-10-18 |
| S46 | Basic generated surface and camera coordinates carry no declared length unit; success output does not check dimensions [src/digitalmodel/solvers/blender_automation/examples/basic_usage.py:L223-L257] | no detection check exists | issue pending (1); 2026-10-18 |
| S47 | Marine examples supply camera/object coordinates and scale 1.0 without a physical scene-unit declaration [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L40-L69] [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L197-L209] | no detection check exists | issue pending (1); 2026-10-18 |
| S48 | File metadata labels bytes divided by 1024² as size_mb without declaring the binary size convention [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L185-L195] | C0.2 checks bytes only; no detection check exists for the size label | issue pending (1); 2026-10-18 |
| S49 | Without `output_blend`, cleanup opens the source mesh or intermediate STL as a scene; the import saves a scene only when an output is supplied [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L121-L122] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L176-L177] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L223-L223] [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L179-L182] | no detection check exists | issue pending (2); 2026-10-19 |
| S50 | Repeated imports into one output reset the scene each time; the example retains only the last imported component; the mock test asserts success without object readback [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L170-L182] [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L181-L191] [tests/solvers/blender_automation/test_blender_wrapper.py:L153-L166] | no detection check exists | issue pending (6); 2026-10-19 |
| S51 | Command omits `--python-exit-code`; a native script exception need not produce the nonzero exit status required for CalledProcessError [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L85-L118] [tests/solvers/blender_automation/conftest.py:L37-L45] | C1.2 is mock-only; no detection check exists for native script-exception status | issue pending (4); 2026-10-19 |
| S52 | `run_script` catches only CalledProcessError; OSError propagates rather than returning a failure dictionary [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L98-L118] | no detection check exists | issue pending (4); 2026-10-19 |
| S53 | Neither wrapper subprocess call has a timeout; version probing or script execution can hang indefinitely [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L39-L44] [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L98-L104] | no detection check exists | issue pending (4); 2026-10-19 |

The zero completed-counter initialisation is accounting state rather than substituted input data; no explicit record-skipping `continue`, external-text `int()`/`float()` conversion, 1970 default, 0.05 fallback or empty-result cache write occurs in the reviewed implementation. [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L128-L160] [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L110-L249]

| Other confirmed pitfall / incident | Detection | Tracking |
| --- | --- | --- |
| `CADImporter.batch_import` appends objects without a subsequent `save_as_mainfile`, so later merged objects need not persist [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L265-L288] | no detection check exists | issue pending (2) |
| Recursive conversion flattens to basename; same stems overwrite the same target [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L220-L238] | no detection check exists | issue pending (3) |
| `run_script(output_file=...)` documents a saved scene but constructs `--render-output`; subprocess status is not artifact readback [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L68-L71] [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L93-L109] | no detection check exists | issue pending (4) |
| String interpolation embeds paths as non-raw Python literals and does not escape quotes or backslashes; Windows paths containing `\U` can cause SyntaxError [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L170-L182] [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L219-L234] | no detection check exists | issue pending (4) |
| Flat and grouped submodule imports can create different class identities and `patch()` targets; `isinstance` can differ by route. DeprecationWarning is hidden by pytest filters [src/digitalmodel/_compat.py:L230-L234] [src/digitalmodel/_compat.py:L237-L320] [pytest.ini:L69-L72] | no detection check exists | issue pending (4) |
| `BlenderContext` accepts a scene argument but only yields a new wrapper; finally is a no-op [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L237-L260] | no detection check exists for scene binding or cleanup | issue pending (4) |
| Temporary-file race fixed by commit `a2b01eaf8d00c3b8f4e13d379a5ff630b0b031d5`, not #620; the regression header explicitly covers findings not already closed by #620/#618. Current routes use NamedTemporaryFile and finally cleanup [tests/security/test_review_20260523_remaining.py:L1-L5] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L142-L148] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L187-L189] [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L201-L232] | `tests/security/test_review_20260523_remaining.py::test_blender_temp_files_no_mktemp` (2 parameter cases), PASS; syntax guard only [tests/security/test_review_20260523_remaining.py:L86-L106] | Fixed in `a2b01eaf`; syntax regression retained |
| Native exported-volume assertion remains absent from module tests; scale and cleanup tests assert success only [tests/solvers/blender_automation/test_cad_importer.py:L117-L135] | no detection check exists for exported volume | #2670 |

## 7. Lessons log

| Date | Entry | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-10-18 | BA-L01 | A success-only scale regression cannot establish scaled geometry | [tests/solvers/blender_automation/test_cad_importer.py:L117-L125] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L107-L124] | Sections 2, 4 and 6 separate status from physical readback | Not recorded; issue pending (5) |
| 2026-10-18 | BA-L02 | Returned failure dictionaries and raised exceptions follow different batch success paths | [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L99-L115] [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L141-L160] | Sections 5–6 bound the exception tests' evidence | Not recorded; issue pending (3) |

## 8. Ownership

Review routing: CFD family quality manager; deputy role: CFD deputy quality manager; producing role: module documentation maintainer; unresolved method classifications go to the family quality role. These are review responsibilities for this draft, not a record of completed review.
UNVERIFIED: appointments for the quality and deputy roles and a shared-log entry are not established in the package evidence (issue pending (5)).
Escalate unsupported physical units, missing native evidence and changed criteria to the family quality role; cross-module changes require a maintainer decision. Must not merge before the second-provider review.

## 9. Dependencies and other implementations

| Dependency / interface | Evidence and limit |
| --- | --- |
| Blender executable and version string | Constructor probes `--version`; command success sets availability but does not constrain version [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L22-L50] |
| FreeCAD and Mesh, optional | FreeCAD import detection; STEP/IGES path imports FreeCAD and Mesh, writes intermediate STL and deletes it in finally [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L47-L57] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L138-L189] |
| Python standard library | Subprocess, pathlib, concurrent futures and datetime supply execution, paths, scheduling and timing [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L6-L11] [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L6-L10] |
| Compatibility namespace | Tests import `digitalmodel.blender_automation`; `_GroupRedirectFinder` redirects the flat package into solvers and is installed at package import. The loader swaps the top-level package; flat and grouped submodules can load separately, creating distinct module/class objects [src/digitalmodel/_compat.py:L83-L83] [src/digitalmodel/_compat.py:L230-L234] [src/digitalmodel/_compat.py:L237-L320] [src/digitalmodel/__init__.py:L52-L55] [tests/solvers/blender_automation/test_blender_wrapper.py:L14-L19] |
| OpenFOAM downstream surface consumer | **Every source occurrence:** one, in `example_ship_hull_analysis`, STL purpose string “Mesh-based CFD (OpenFOAM, Fluent)”; this is descriptive export metadata, not a solver dependency import, launch or version query [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L123-L138] |

UNVERIFIED: README version claims (Blender 4.0+, recommendation 5.0+) do not establish native operator compatibility at this code pin; the runtime was absent in the historical run; the current Linux run uses Blender 4.0.2 but does not check import/export operator compatibility (issue pending (4)). [src/digitalmodel/solvers/blender_automation/README.md:L394-L398]

### Implementation-map inventory

Map source: `docs/registry/implementation-map.yaml` at map ref commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; embedded data revision `270c8759c374b67a7f032953e7fcdb14e0224461`, schema 1; the default-branch copy records the same data revision. [docs/registry/implementation-map.yaml:L1-L4]
This map pin is separate from the pack's code pin; the inventory computation reads the map-ref bytes, not the default-branch copy.
Reproduction: parse `families` in file order, retain each `implementations` row whose `path` begins `src/digitalmodel/solvers/blender_automation/`, then rank families by retained-row count with file-order tie breaking.
Inventory result: **0 module map families; 0 in-package rows; empty implementation inventory**. Counted-family grep: **NOT_APPLICABLE**, because there is no family with an implementation row inside this package. No search signal is selected or path-suppressed in this case; no additional implementation is counted. As a separate cross-check, applying the same filter to the code-pin family inventory also yields zero rows. [docs/registry/implementation-map.yaml:L346-L26466]
Map follow-up, issue pending (5): add the section-3 method families—Blender subprocess launch, CAD import/tessellation, mesh cleanup, scene/camera/material preparation, mesh export/size checks and batch/discovery—before they can become counted families. Rejected candidates and symbol-name matches do not establish implementation membership.

### Repository-wide discovery

The following supplementary search covers the entire tracked repository, including `scripts/`, at the code pin; it does not change the empty-map result:

```sh
git grep -n -E 'bpy\.ops|FreeCAD\.open|find_blender_executable|class (BlenderWrapper|CADImporter|MeshExporter)' <code-pin> -- '*.py'
git grep -n -E 'blender_automation|CADImporter|MeshExporter|BlenderWrapper|class SceneManager|class BatchProcessor|def find_blender_executable' <code-pin> -- '*.py'
```

| Discovery | Classification and evidence |
| --- | --- |
| `src/digitalmodel/visualization/design_tools/blender_integration.py` | Independent scene/import/cleanup and GLTF/FBX export script generation; map gap outside the package, issue pending (5) [src/digitalmodel/visualization/design_tools/blender_integration.py:L80-L139] [src/digitalmodel/visualization/design_tools/blender_integration.py:L410-L468] |
| `scripts/field_development/field_flythrough_bpy.py` | Independent scene clearing, asset geometry and rendering, not delegation to this wrapper; map gap, issue pending (5) [scripts/field_development/field_flythrough_bpy.py:L133-L146] [scripts/field_development/field_flythrough_bpy.py:L402-L426] |
| `scripts/field_development/onshore_flythrough_bpy.py` | Independent scene clearing and terrain construction; map gap, issue pending (5) [scripts/field_development/onshore_flythrough_bpy.py:L64-L85] |
| `src/digitalmodel/gis/integrations/blender_script_generator.py` | Independent Blender script generation for point meshes; map gap, issue pending (5) [src/digitalmodel/gis/integrations/blender_script_generator.py:L16-L48] |
| `src/digitalmodel/gis/integrations/blender_export.py` | Independent scene-origin and mesh-object script generation; map gap, issue pending (5) [src/digitalmodel/gis/integrations/blender_export.py:L285-L325] |
| `src/digitalmodel/visualization/design_tools/freecad_integration.py` | Separate FreeCAD document-loading interface; outside-package CAD map follow-up, issue pending (5) [src/digitalmodel/visualization/design_tools/freecad_integration.py:L35-L65] |
| Other `BatchProcessor` symbols | Separate fatigue, time-series, native-solver and diffraction scheduling rather than Blender geometry implementations [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py:L122-L143] [src/digitalmodel/signal_processing/signal_analysis/orcaflex/batch.py:L25-L42] [src/digitalmodel/solvers/orcaflex/universal/batch_processor.py:L24-L50] [src/digitalmodel/hydrodynamics/diffraction/batch_processor.py:L69-L85] |
| Tests and wrapper examples | Test assertions and calls to listed package classes; helpers, not independent engineering implementations [tests/solvers/blender_automation/test_blender_wrapper.py:L113-L166] [src/digitalmodel/solvers/blender_automation/examples/basic_usage.py:L38-L52] |

## 10. Coverage and open items

### Files read

All 12 Python files and their subfolders were read as text; generated model, image and scene files are not independent reference values. [src/digitalmodel/solvers/blender_automation/__init__.py:L6-L25] [src/digitalmodel/solvers/blender_automation/examples/basic_usage.py:L17-L57] [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L90-L144]

| Package file | Coverage evidence at code pin |
| --- | --- |
| `__init__.py` | [src/digitalmodel/solvers/blender_automation/__init__.py:L1-L25] |
| `converters/__init__.py` | [src/digitalmodel/solvers/blender_automation/converters/__init__.py:L1-L9] |
| `converters/cad_importer.py` | [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L1-L311] |
| `converters/mesh_exporter.py` | [src/digitalmodel/solvers/blender_automation/converters/mesh_exporter.py:L1-L329] |
| `core/__init__.py` | [src/digitalmodel/solvers/blender_automation/core/__init__.py:L1-L9] |
| `core/blender_wrapper.py` | [src/digitalmodel/solvers/blender_automation/core/blender_wrapper.py:L1-L260] |
| `core/scene_manager.py` | [src/digitalmodel/solvers/blender_automation/core/scene_manager.py:L1-L296] |
| `examples/basic_usage.py` | [src/digitalmodel/solvers/blender_automation/examples/basic_usage.py:L1-L321] |
| `examples/marine_engineering_integration.py` | [src/digitalmodel/solvers/blender_automation/examples/marine_engineering_integration.py:L1-L404] |
| `utils/__init__.py` | [src/digitalmodel/solvers/blender_automation/utils/__init__.py:L1-L26] |
| `utils/batch_processor.py` | [src/digitalmodel/solvers/blender_automation/utils/batch_processor.py:L1-L333] |
| `utils/file_utils.py` | [src/digitalmodel/solvers/blender_automation/utils/file_utils.py:L1-L249] |

| Module documentation found | Reading and disposition |
| --- | --- |
| `src/digitalmodel/solvers/blender_automation/README.md` | API overview; version and test-path claims require native rechecking [src/digitalmodel/solvers/blender_automation/README.md:L1-L39] |
| `docs/domains/solvers/blender_automation/README.md` | Placeholder overview and API [docs/domains/solvers/blender_automation/README.md:L1-L17] |
| `docs/domains/blender/BLENDER_AUTOMATION_MODULE.md` | Test-suite summary explicitly leaves mesh-exporter tests to be created [docs/domains/blender/BLENDER_AUTOMATION_MODULE.md:L131-L166] |
| `docs/plans/2026-06-29-cfd-capability-assessment.md` | Adjacent CAD/CFD planning lead; not execution evidence [docs/plans/2026-06-29-cfd-capability-assessment.md:L28-L44] |
| `docs/plans/evidence/issue-2082-ci-discovery.md` | Historical discovery/CI evidence, not this pin or this module-only run [docs/plans/evidence/issue-2082-ci-discovery.md:L107-L123] |
| `docs/domains/marine-engineering/marine_3d_models_catalog.md` | Catalog compatibility lead; no catalog model values adopted [docs/domains/marine-engineering/marine_3d_models_catalog.md:L198-L215] |
| `docs/registry/implementation-map.yaml` | Default-branch map metadata also read; counted-family inventory uses the separate map pin above [docs/registry/implementation-map.yaml:L1-L4] |

Documentation follow-up: reconcile old import/test paths, the incomplete mesh-exporter test summary and catalog add-on claims with current FreeCAD dispatch; issue pending (5). [src/digitalmodel/solvers/blender_automation/README.md:L365-L383] [docs/domains/blender/BLENDER_AUTOMATION_MODULE.md:L131-L166] [docs/domains/marine-engineering/marine_3d_models_catalog.md:L198-L215] [src/digitalmodel/solvers/blender_automation/converters/cad_importer.py:L90-L105]

| Tests read | Scope and result basis |
| --- | --- |
| `tests/solvers/blender_automation/__init__.py` | [tests/solvers/blender_automation/__init__.py:L1-L4] |
| `tests/solvers/blender_automation/conftest.py` | [tests/solvers/blender_automation/conftest.py:L1-L75] |
| `tests/solvers/blender_automation/test_batch_processor.py` | [tests/solvers/blender_automation/test_batch_processor.py:L1-L214] |
| `tests/solvers/blender_automation/test_blender_wrapper.py` | [tests/solvers/blender_automation/test_blender_wrapper.py:L1-L220] |
| `tests/solvers/blender_automation/test_cad_importer.py` | [tests/solvers/blender_automation/test_cad_importer.py:L1-L135] |
| `tests/solvers/blender_automation/test_file_utils.py` | [tests/solvers/blender_automation/test_file_utils.py:L1-L197] |
| `tests/solvers/blender_automation/test_scene_manager.py` | [tests/solvers/blender_automation/test_scene_manager.py:L1-L176] |
| `tests/security/test_review_20260523_remaining.py` | Two parameter cases guarding package temporary-file syntax [tests/security/test_review_20260523_remaining.py:L86-L106] |
| `tests/docs/test_digitalmodel_routing_contract.py` | Routing surfaces, registry schema and domain equality [tests/docs/test_digitalmodel_routing_contract.py:L1-L108] |

The planning calculation-citation contract was readable; no standards-derived calculation was added. Private data and licensed programs were not accessed. Native program results and cross-format physical units remain unverified; no private-sidecar value or entry is introduced.

| Open item | Responsible role | Tracking and closure evidence |
| --- | --- | --- |
| Physical units, coordinates and non-unit scale readback; includes the section-2 UNVERIFIED item | Module maintainer | issue pending (1); dimension checks on independently reloaded exports |
| Cleanup failure propagation and persisted merged objects | Module maintainer | issue pending (2); reload scene after cleanup and merge, verify object count and process status |
| Returned failures, cancellation completeness, zero-work handling and output collisions | Module maintainer | issue pending (3); per-input terminal records and duplicate-stem assertions |
| Native version/operator/path and artifact contracts; includes the section-9 UNVERIFIED item | Module maintainer | issue pending (4); 2 native wrapper cases PASS on Linux with Blender 4.0.2, without native geometry or script-error readback; historically NOT_RUN because Blender was absent |
| Documentation, named quality/deputy appointments, shared log, standards applicability, method-map coverage and production evidence appendix; includes section-3 and section-8 UNVERIFIED items | Module maintainer and CFD quality role | issue pending (5); map families with in-package rows, corrected docs and recorded evidence; cited planning contract was readable |
| Example omissions, truncated catalog and discarded errors | Module maintainer | issue pending (6); explicit partial-work and error-result assertions |
| Independent engineering geometry value assertion | Module maintainer | #2670; independently sourced exported-volume expectation; not the current success-only tests |

A1–A8 are self-checks of this draft; issue numbers for pending drafts and independent review remain required before clearance. Native Blender/FreeCAD geometry execution, downstream OpenFOAM mesh/initialisation/run gates and standards-clause verification were not performed.
