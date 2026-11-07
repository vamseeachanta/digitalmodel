# pyintegrity compatibility knowledge pack

| Header | Value |
| --- | --- |
| Module | data_systems/pyintegrity |
| Repository | digitalmodel |
| Path | src/digitalmodel/data_systems/pyintegrity |
| Pack revision | 0 |
| Code revision | b47e42fbe7208c69b28dd436aa91fb129bc385f7 |
| Analysis family | EC; compatibility import, reviewed with Python calculations gates |
| Quality role | Calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package is a compatibility shim for the rename to `digitalmodel.asset_integrity`; its complete implementation emits a warning and executes a star import from that package. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L1-L14]

The scope is this package initializer, not the replacement package's assessment engines; no calculation, file reader, file writer, solver launch or submodule alias is implemented here. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14]

The routing registry records `data_systems` as beta, with `tests/data_systems/` as its domain test root; that maturity is a domain field, not evidence of shim-specific validation. [docs/registry/module-routing.yaml:L113-L125]

## 2. Inputs and outputs

| Input | Unit | Allowed range | Default | Source |
| --- | --- | --- | --- | --- |
| Import request | dimensionless module name | `digitalmodel.data_systems.pyintegrity` | No call arguments | Python import statement; the initializer declares no callable API. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L1-L14] |
| Replacement namespace | dimensionless attributes | Names selected by `from digitalmodel.asset_integrity import *` | No explicit shim export list | Replacement package initializer and star import. [src/digitalmodel/asset_integrity/__init__.py:L1-L8] [src/digitalmodel/data_systems/pyintegrity/__init__.py:L14-L14] |

| Output | Unit | Range or default | Destination |
| --- | --- | --- | --- |
| Rename notice | dimensionless warning count | One `warnings.warn` call per initializer execution, `DeprecationWarning`, `stacklevel=2`; visibility depends on warning filters | Python warning machinery. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L8-L13] |
| Imported namespace | dimensionless attributes | No engine or numerical result is created by the shim | Importing module; the replacement initializer contains a docstring and `__version__`, with no public function definitions. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L14-L14] [src/digitalmodel/asset_integrity/__init__.py:L1-L8] |

Coordinate frame, origin, sign, phase, physical units and file-format conventions are inapplicable to this initializer because it has no engineering inputs or file operations. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14]

## 3. Method and governing standards

| Operation | Implementation | Reference and limit |
| --- | --- | --- |
| Deprecation notice | Module-level `warnings.warn` with the replacement import path | This is migration behavior, not a standards-derived equation. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L8-L13] |
| Namespace forwarding | Module-level star import | This copies selected attributes; it does not register old submodules or call the assessment engine. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L14-L14] [src/digitalmodel/asset_integrity/__init__.py:L1-L8] |

No governing engineering standard, equation, coefficient or standards-derived numeric output appears in this package; a calculation Citation sidecar is therefore inapplicable to its two import-time operations. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L1-L14]

## 4. Validated worked examples

Validated examples: 0; this initializer supplies no engineering result or independent expected value. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14]

Regression examples: 3 upstream snapshot comparisons, not shim tests; each calls the replacement engine and compares its result with stored YAML using `DeepDiff(ignore_order=True, significant_digits=4)`. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L6-L25]

| Case | Source and input | Expected value and tolerance basis | Existing test node ID | Last offline result |
| --- | --- | --- | --- | --- |
| R1 | Stored output YAML and test-local input selected by the node; values not reproduced. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L36-L44] | 0 differences, dimensionless; four significant digits, a regression setting rather than an independent engineering tolerance. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L19-L25] | `tests/asset_integrity/test_pyintegrity_gml_lml_api579.py::test_api579_design_apib318` | NOT_RUN at BASE and after: pytest skips for missing `asset_integrity/tests/test_data/API579.yml`; the skip branch is explicit. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L14-L17] |
| R2 | Stored output YAML and test-local input selected by the node; values not reproduced. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L47-L58] | 0 differences at four significant digits, or the exact handled failure string; neither establishes an independent benchmark. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L19-L25] [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L55-L58] | `tests/asset_integrity/test_pyintegrity_gml_lml_api579.py::test_api579_design_apib314_1` | NOT_RUN at BASE and after: same missing base YAML; pytest skip. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L14-L17] |
| R3 | Stored output YAML and test-local input selected by the node; values not reproduced. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L61-L72] | 0 differences at four significant digits, or the exact handled failure string; neither establishes an independent benchmark. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L19-L25] [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L69-L72] | `tests/asset_integrity/test_pyintegrity_gml_lml_api579.py::test_api579_design_apib314_2` | NOT_RUN at BASE and after: same missing base YAML; pytest skip. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L14-L17] |

The discovered worked script is excluded as a validated or regression case: it has no test node or asserted expected result and imports the nonexistent old `engine` submodule before entering its function. [examples/data_systems/pyintegrity/basic_usage.py:L15-L18] [examples/data_systems/pyintegrity/basic_usage.py:L34-L61]

## 5. QA gates

Review basis: AQG r0, section 10, Python calculations; G5 applies through section 9 of that standard.

The following are reproducible pack checks, not newly added repository tests; diagnostic checks document the current import boundary and do not certify assessment results. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14]

| Gate | Applicability | Check and numeric pass criterion | Evidence and current result |
| --- | --- | --- | --- |
| G0 | Applicable: import input, namespace and citation boundary | C0: parse the package with Python `ast`; exactly 1 `.py` file, 0 function/class definitions and 1 `ImportFrom` targeting `digitalmodel.asset_integrity` with name `*`; unit: counts. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L1-L14] | PASS by source inspection and AST assertions; no physical units enter this initializer. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14] |
| G1 | Applicable: Python import and offline tests; target is the Python interpreter | C1: in a fresh process under `warnings.catch_warnings(record=True)` and `simplefilter("always", DeprecationWarning)`, import the old package; exactly 1 notice with the exact category and rename message. Also try the old `.engine` submodule: exactly 1 `ModuleNotFoundError` naming that submodule; unit: counts. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L8-L14] | PASS for package import and documented boundary diagnostic; old `.engine` remains unavailable, issue pending (1). [src/digitalmodel/data_systems/pyintegrity/__init__.py:L14-L14] [examples/data_systems/pyintegrity/basic_usage.py:L15-L15] |
| G1 | Applicable: replacement-package regression discovery | C3: run R1–R3 using pytest on a temporary copy; required assessment coverage is 3 executed nodes and 0 skipped or failed nodes, unit: nodes. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L36-L72] | NOT_RUN: 3 skipped nodes at both runs; missing base YAML, issue pending (2). [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L14-L17] |
| G2 | Applicable: independent source trace for import semantics, not an engineering hand calculation | C2: manually trace the two initializers, then compare runtime namespaces; exactly 0 copied public assessment exports, 0 `engine` attributes and 0 `__version__` attributes on the shim; unit: attributes. The shim's own public `warnings` binding is excluded from the copied set. [src/digitalmodel/asset_integrity/__init__.py:L1-L8] [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14] | PASS for namespace trace; no numerical assessment benchmark is claimed. [src/digitalmodel/asset_integrity/__init__.py:L1-L8] |
| G3 | NOT_APPLICABLE | No dynamics state, time stepping or solver call in this package. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14] | NOT_APPLICABLE, import-only implementation. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14] |
| G4 | NOT_APPLICABLE | No mesh, diffraction result or hydrostatic calculation in this package. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14] | NOT_APPLICABLE, import-only implementation. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14] |
| G5 | Applicable: evidence record | C5: record pin, commands, per-node status, citation audit and source digest; require 0 missing fields among those 5 field groups, unit: groups; retain explicit NOT_RUN results. This records import evidence, not an assessment report. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14] | PASS for evidence completeness; no engineering clearance inferred. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14] |

## 6. Known failure modes and pitfalls

sweep: 0 found

The complete package has no swallowing `except`, record-skipping `continue`, error-path return, numeric fallback, external-text conversion, unit assumption or cache write; the only executable statements import `warnings`, issue the notice and perform the star import. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14]

Exact sweep command, run on package Python files:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5\s*%|int\(|float\(|unit|cache' src/digitalmodel/data_systems/pyintegrity --glob '*.py'
```

| Symptom | Cause and scope | Detection | Tracking |
| --- | --- | --- | --- |
| Old `.engine` import fails before example execution | The shim has no submodule registration; the example imports `.engine` at module level. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L14-L14] [examples/data_systems/pyintegrity/basic_usage.py:L15-L15] | C1 boundary diagnostic; no committed shim-specific regression test exists | issue pending (1) |
| Example reports a missing fixture or catches an exception and returns `None` | These two return paths are outside the shim, in the discovered example, and the configured old package test-data path is absent from the shim's implementation. [examples/data_systems/pyintegrity/basic_usage.py:L21-L26] [examples/data_systems/pyintegrity/basic_usage.py:L57-L61] [src/digitalmodel/data_systems/pyintegrity/__init__.py:L1-L14] | no detection check exists for these example branches | issue pending (1) |
| Three snapshot nodes produce no assessment result | `run_api579` converts `FileNotFoundError` into a pytest skip; replacement configuration lookup falls back to package test data. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L14-L17] [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L165-L177] | C3 reports 3 NOT_RUN nodes, not 3 passed examples | issue pending (2) |
| A version or engine export is assumed from the rename notice | The replacement initializer defines only a private-by-star-import version name; the shim provides no explicit engine export. [src/digitalmodel/asset_integrity/__init__.py:L1-L8] [src/digitalmodel/data_systems/pyintegrity/__init__.py:L14-L14] | C2 namespace assertions | issue pending (1) |
| Historical tests asserted empty results against empty results | Upstream legacy fracture-mechanics regression weakness, outside the shim | no shim detection check exists; inspect replacement-package tests rather than claiming shim validation | #2160, closed defect record, legacy-test item |

The rename history is recorded in #446; it is background rather than a confirmed shim incident, and neither historical issue establishes current numerical correctness. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L1-L5] [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L6-L25]

## 7. Lessons log

| Date | Entry | Lesson | Evidence | Pack change | Shared entry |
| --- | --- | --- | --- | --- | --- |
| 2026-11-07 | Initial source audit | A star import of an initializer is not a submodule redirect; migration callers must use the replacement path explicitly. | [src/digitalmodel/data_systems/pyintegrity/__init__.py:L14-L14] [examples/data_systems/pyintegrity/basic_usage.py:L15-L15] | Sections 1–6 distinguish package import from engine import | Not assigned; issue pending (1) |

## 8. Ownership

Required review roles for this draft: calculations quality manager, with a calculations deputy quality manager; the maintainer assigns the deputy before clearance. Review independence follows AQG r0, section 11, Sign-off and independence.

Producing responsibility: module maintainer; technical classifications and check criteria go to the calculations quality manager, and cross-family routing gaps go to the maintainers. These are review requirements, not evidence of completed review; basis: AQG r0, section 11, Sign-off and independence.

## 9. Dependencies and implementation inventory

The direct dependency is the standard-library warning facility plus the `digitalmodel.asset_integrity` initializer; no dataset or solver version is selected in the shim. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14]

The replacement engine owns YAML configuration and dispatch to assessment routines, outside this package's boundary; the discovered script is a downstream caller of a currently unresolved old engine path. [src/digitalmodel/asset_integrity/engine.py:L16-L43] [examples/data_systems/pyintegrity/basic_usage.py:L15-L22]

Implementation-map snapshot: `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; data revision `270c8759c374b67a7f032953e7fcdb14e0224461`, as named by the map header. The map citations in this section use that snapshot, separately from the code pin. [docs/registry/implementation-map.yaml:L1-L4]

A8 inventory: 0 implementation rows have a `path` inside `src/digitalmodel/data_systems/pyintegrity/`, across all 13 map families; therefore the module's map-family inventory and counted-family set are empty, and the search-signals step is `NOT_APPLICABLE` because there is no in-package map family. No signal was excluded or applied, and no counted-family miss is asserted. [docs/registry/implementation-map.yaml:L346-L26502]

| Map family with in-package rows | Implementation inventory | Counted search |
| --- | --- | --- |
| None | Empty; no definition inventory to reproduce | NOT_APPLICABLE; no in-package rows. [docs/registry/implementation-map.yaml:L346-L26502] |

Whole-repository discovery, including `scripts/`, used the following command at the code pin; its 11 matching lines identify the shim, replacement-package history and example, with no additional Python consumer in `scripts/`. This is supplementary discovery, not a counted-family grep. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L1-L14] [src/digitalmodel/_compat.py:L127-L134] [src/digitalmodel/asset_integrity/__init__.py:L1-L8] [src/digitalmodel/asset_integrity/__main__.py:L9-L19] [examples/data_systems/pyintegrity/basic_usage.py:L1-L22]

```sh
git grep -n -E 'pyintegrity|data_systems[./]pyintegrity' b47e42fbe7208c69b28dd436aa91fb129bc385f7 -- '*.py'
```

Map follow-up: section 3's rename-warning and namespace-forwarding operations are absent as method families; request classification before adding any map rows, issue pending (4). They are import helpers rather than engineering algorithms in this package. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14]

## 10. Coverage and open items

Read coverage: the complete package consists of its one initializer; the replacement initializer and engine, configuration lookup, compatibility routing comment and worked script were also inspected. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L1-L14] [src/digitalmodel/asset_integrity/__init__.py:L1-L8] [src/digitalmodel/asset_integrity/engine.py:L1-L81] [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L153-L177] [src/digitalmodel/_compat.py:L127-L134] [examples/data_systems/pyintegrity/basic_usage.py:L1-L65]

| Documentation found and read | Use and limit |
| --- | --- |
| `docs/domains/pyintegrity/TODO.md` | Historical documentation and configuration tasks; not validation evidence. [docs/domains/pyintegrity/TODO.md:L1-L19] |
| `docs/domains/pyintegrity/ffs_scope.puml` and `ffs_scope.svg` | Historical scope diagram, not a shim computation or a checked standard clause. [docs/domains/pyintegrity/ffs_scope.puml:L9-L25] [docs/domains/pyintegrity/ffs_scope.svg:L28-L44] |
| `docs/domains/pyintegrity/ffs_methodology_general_metal.puml` and corresponding `.svg` | Related historical workflow diagram, not shim functionality. [docs/domains/pyintegrity/ffs_methodology_general_metal.puml:L8-L47] [docs/domains/pyintegrity/ffs_methodology_general_metal.svg:L1-L1] |
| `docs/domains/pyintegrity/ffs_methodology_local_metal.puml` and corresponding `.svg` | Related historical workflow diagram, not shim functionality. [docs/domains/pyintegrity/ffs_methodology_local_metal.puml:L8-L51] [docs/domains/pyintegrity/ffs_methodology_local_metal.svg:L1-L1] |
| `docs/domains/project-docs/MODULE_SURVEY.md` | Historical rename mention; maturity and test figures are not adopted. [docs/domains/project-docs/MODULE_SURVEY.md:L24-L32] |
| `docs/archive/archive-blob-map-2026-10-08.csv` and `archive-candidates-2026-10-08.csv` | Diagram index records only. [docs/archive/archive-blob-map-2026-10-08.csv:L1061-L1063] [docs/archive/archive-candidates-2026-10-08.csv:L1061-L1063] |
| `docs/audits/knowledge-pack-acceptance.json` | Existing audit references to this shim and legacy tests; these do not replace a fresh test result. [docs/audits/knowledge-pack-acceptance.json:L3748-L3748] [docs/audits/knowledge-pack-acceptance.json:L3809-L3813] |

Repository tests read and run: R1–R3 plus 1 discovery test with an identifying filename, whose filename and fixture values are withheld; the latter checks fixture structure, not the shim (codenaming tracked below). The R1–R3 implementation imports the replacement engine directly. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L6-L7] See issue pending (3).

Offline record at the header code pin: BASE and after each collected 4 nodes, with 1 passed, 0 failed and 3 skipped; the 3 skipped nodes are the 3 NOT_RUN regression cases in section 4. Tests used an unchanged temporary copy because the engine contains a configuration-save path. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L14-L17] [src/digitalmodel/asset_integrity/engine.py:L40-L43] [src/digitalmodel/asset_integrity/engine.py:L73-L81]

Command form: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest <temporary-copy module test paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`; no addopts override, solver import or solver execution was used. Contract and routing checks used `tests/contracts/ tests/docs/test_digitalmodel_routing_contract.py` in the same form; 590 passed before and after the registry edit. The test framework and runtime dependencies are declared separately. [pyproject.toml:L195-L213] [pytest.ini:L48-L52]

The full contracts domain additionally includes generic import tests that reach the diffraction solver converter, so it was NOT_RUN under the solver-interface restriction; the solver-free contract and routing subset above was run without deselection or altered tests. [tests/compat/test_group_compat.py:L85-L115] [src/digitalmodel/hydrodynamics/diffraction/__init__.py:L57-L72]

UNVERIFIED: public source provenance for the anonymous discovery fixture test is withheld until codenaming, issue pending (3). The anonymous test result is retained in the local execution record; its exact source node cannot be cited publicly here.

UNVERIFIED claims: 1 (the public provenance item above); numerical validation of assessment routines is outside the shim scope, and no maturity, standard edition or benchmark figure is inferred from the historical diagrams. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L1-L14]

| Open item | Responsible role | Tracking and completion evidence |
| --- | --- | --- |
| O1: Repair or retire the old engine example, specify namespace/version compatibility and add direct shim tests | Maintainer | issue pending (1); replace the unresolved submodule import and cover the warning and import boundary. [examples/data_systems/pyintegrity/basic_usage.py:L15-L26] [src/digitalmodel/data_systems/pyintegrity/__init__.py:L8-L14] |
| O2: Establish distributable base configuration or explicit reference-data provenance for R1–R3 | Maintainer | issue pending (2); C3 must execute 3 nodes with 0 fixture skips before numerical regression coverage is claimed. [tests/asset_integrity/test_pyintegrity_gml_lml_api579.py:L14-L25] [src/digitalmodel/asset_integrity/common/ApplicationManager.py:L165-L177] |
| O3: Codename 1 discovery test file with an identifying name before publishing its node ID or fixture details | Maintainer | issue pending (3); retain the passing structural assertions without publishing identifying values; historical test context is #2160. |
| O4: Classify rename-warning and namespace-forwarding operations in the implementation map | Maintainer | issue pending (4); no counted family exists at the named map revision; map additions precede future counted searches. [docs/registry/implementation-map.yaml:L346-L26502] [src/digitalmodel/data_systems/pyintegrity/__init__.py:L6-L14] |
