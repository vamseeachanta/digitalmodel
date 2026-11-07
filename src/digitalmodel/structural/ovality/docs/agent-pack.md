# Pipe ovality knowledge pack

| Header | Value |
| --- | --- |
| Module | structural/ovality |
| Repository | digitalmodel |
| Package | src/digitalmodel/structural/ovality |
| Pack revision | 0 |
| Code revision | aebd7e4bc5c3dda5f7433e20e021dd58ac17d0b3 |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`router` computes pipe out-of-roundness from diameters or accepts a supplied fraction, compares the result with a configurable allowable, writes one CSV row and replaces the `pipe_ovality` configuration mapping with its summary. [src/digitalmodel/structural/ovality/workflow.py:L18-L50]

The diameter route computes `(max_OD - min_OD) / nominal_OD`; the fraction route constructs symmetric extreme diameters around the nominal diameter. [src/digitalmodel/structural/ovality/workflow.py:L25-L32] [src/digitalmodel/structural/ovality/workflow.py:L53-L60]

The workflow performs a geometry comparison; its result has no pressure, material strength, buckling capacity or load-history field. [src/digitalmodel/structural/ovality/workflow.py:L34-L42]

The registry calls the parent structural domain `stable`; it does not assign separate ovality maturity, and that label is not evidence of standards validation. [docs/registry/module-routing.yaml:L525-L530]

## 2. Inputs and outputs

| Input | Unit and range implemented | Default and route | Evidence |
| --- | --- | --- | --- |
| `nominal_OD` | Diameter; positive comparison only | Required | [src/digitalmodel/structural/ovality/workflow.py:L103-L110] |
| `max_OD`, `min_OD` | Diameter; both positive, maximum at least minimum | Both required if either is supplied | [src/digitalmodel/structural/ovality/workflow.py:L22-L28] [src/digitalmodel/structural/ovality/workflow.py:L53-L60] |
| `ovality_fraction`, `out_of_roundness`, `ovality` | Dimensionless; negative values rejected | First non-None alias wins when neither extreme diameter is supplied | [src/digitalmodel/structural/ovality/workflow.py:L29-L32] [src/digitalmodel/structural/ovality/workflow.py:L63-L70] |
| `allowable` | Dimensionless; positive comparison only | Code default `0.03`; no standards provenance asserted | [src/digitalmodel/structural/ovality/workflow.py:L21-L21] [src/digitalmodel/structural/ovality/workflow.py:L113-L124] |
| `output_dir` | Filesystem location | `results`; relative to configuration directory, configuration file parent, then working directory | [src/digitalmodel/structural/ovality/workflow.py:L73-L86] |
| Configuration file / `basename` | Filename stem / string | File stem takes priority; basename fallback is `pipe_ovality` | [src/digitalmodel/structural/ovality/workflow.py:L89-L92] |

UNVERIFIED: U1 — the physical diameter unit of the shipped example is unspecified; use one consistent length unit for all diameters, because no conversion or unit metadata accompanies the calculation. [examples/workflows/pipe-ovality/input.yml:L3-L8] [src/digitalmodel/structural/ovality/workflow.py:L53-L60]

Coordinate frame, origin, sign and phase are not inputs to the scalar geometry calculation; diameter ordering is explicitly checked. [src/digitalmodel/structural/ovality/workflow.py:L53-L60]

| Output | Unit / format | Behavior | Evidence |
| --- | --- | --- | --- |
| `nominal_OD`, `max_OD`, `min_OD` | Same numerical diameter unit as input | Measured or constructed extreme diameters | [src/digitalmodel/structural/ovality/workflow.py:L25-L37] |
| `out_of_roundness`, `allowable` | Dimensionless | Geometry ratio and configured limit | [src/digitalmodel/structural/ovality/workflow.py:L34-L41] |
| `ovality_percent` | Percent | Fraction multiplied by 100 | [src/digitalmodel/structural/ovality/workflow.py:L38-L39] |
| `passes` | Boolean | Inclusive comparison `out_of_roundness <= allowable` | [src/digitalmodel/structural/ovality/workflow.py:L41-L41] |
| `results_csv` | Path string in returned configuration | Resolved path; relative to the repository-root constant when possible, otherwise absolute | [src/digitalmodel/structural/ovality/workflow.py:L15-L15] [src/digitalmodel/structural/ovality/workflow.py:L43-L49] [src/digitalmodel/structural/ovality/workflow.py:L134-L139] |
| CSV | Header plus one data row | Parent directories created; existing destination overwritten; filename ends `_ovality.csv` | [src/digitalmodel/structural/ovality/workflow.py:L73-L78] [src/digitalmodel/structural/ovality/workflow.py:L95-L100] |

## 3. Method and governing standards

| Method | Implementation and limits | Evidence |
| --- | --- | --- |
| Measured geometry | `_out_of_roundness` divides the extreme-diameter difference by nominal diameter after positivity and ordering comparisons | [src/digitalmodel/structural/ovality/workflow.py:L53-L60] |
| Supplied fraction | `_required_ovality_fraction` converts the first present alias to float and rejects negative values | [src/digitalmodel/structural/ovality/workflow.py:L63-L70] |
| Reconstructed diameters | `router` uses nominal diameter times `1 + fraction / 2` and `1 - fraction / 2` | [src/digitalmodel/structural/ovality/workflow.py:L29-L32] |
| Acceptance comparison | `router` applies the user/code allowable inclusively | [src/digitalmodel/structural/ovality/workflow.py:L21-L21] [src/digitalmodel/structural/ovality/workflow.py:L41-L41] |

UNVERIFIED: U2 — DNV-OS-F101 is named without edition, clause, table or equation identifier in the module docstring and durable-workflow title; no edition conflict can be resolved from those references, and clause text not checked against the licensed copy. [src/digitalmodel/structural/ovality/workflow.py:L1-L6] [docs/registry/workflows.yaml:L338-L345]

The emitted row and returned mapping contain no `Citation` sidecar; standards attribution and the allowable's engineering basis need resolution before treating `passes` as a code-compliance conclusion (issue pending (2)). [src/digitalmodel/structural/ovality/workflow.py:L34-L50]

## 4. Worked examples and offline results

These are two regression cases: their expected ratio repeats the geometry expression without an independently identified standard annex or benchmark revision; neither establishes standards acceptance. [tests/structural/ovality/test_ovality_workflow.py:L10-L41] [tests/workflows/test_durable_workflows.py:L548-L563]

| Case / classification | Inputs and expected result | Tolerance basis | Existing test node ID | Offline record |
| --- | --- | --- | --- | --- |
| Configuration-file geometry / regression | Inline YAML; expected fraction `(325.0 - 322.7) / 323.85` dimensionless, percent equal to that ratio times 100, Boolean true and one CSV data row | Existing `pytest.approx` assertions; no engineering tolerance is declared | `tests/structural/ovality/test_ovality_workflow.py::test_pipe_ovality_workflow_writes_geometry_check_next_to_input` | BASE PASS; AFTER PASS; [tests/structural/ovality/test_ovality_workflow.py:L10-L41] |
| Durable workflow / regression | `examples/workflows/pipe-ovality/input.yml`; same ratio and Boolean, checked in configuration and CSV | Existing `pytest.approx` assertions; no engineering tolerance is declared | `tests/workflows/test_durable_workflows.py::test_workflow_registry[pipe-ovality]` | BASE PASS in temporary copy; AFTER PASS; [examples/workflows/pipe-ovality/input.yml:L1-L14] [tests/workflows/test_durable_workflows.py:L548-L563] |

The durable registry specifies the example, output and offline test route. [docs/registry/workflows.yaml:L338-L345]

UNVERIFIED: U3 — an independent worked example and its engineering tolerance remain unavailable in the two identified tests; validated cases: 0, regression cases: 2 (issue pending (2)). [tests/structural/ovality/test_ovality_workflow.py:L31-L41] [tests/workflows/test_durable_workflows.py:L554-L563]

## 5. QA gates and executable checks

Apply the AQG r0 Python-calculations gate set: G0 inputs, units and citations; G1 imports and tests; G2 hand or published check; G3 and G4 not applicable; G5 evidence completeness.

The following checks specify review procedures, not new acceptance limits from a standard; use isolated output directories for every direct `router` invocation.

| Gate | Applicability | Checks and numeric pass criteria | Evidence / limitation |
| --- | --- | --- | --- |
| G0 | Applicable | C0: `_required_positive_float` rejects zero and negative nominal diameter, `_out_of_roundness` rejects non-positive extremes and reversed extremes: 5 exceptions / 5 invalid cases, count. C6: reject non-finite values through each float-conversion helper: 4 rejected routes / 4 routes, count. C8: diameter-unit declaration, governing edition/clause and citation sidecar present: 3 / 3 provenance items, count. | C0 uses the explicit validation branches; C6 and C8 expose open defects, issue pending (1) and issue pending (2). [src/digitalmodel/structural/ovality/workflow.py:L53-L70] [src/digitalmodel/structural/ovality/workflow.py:L103-L131] [src/digitalmodel/structural/ovality/workflow.py:L34-L50] |
| G1 | Applicable; Python import and pytest route | C1: run both section-4 nodes: 2 passed / 2 cases and 0 failures, count. C3: reload CSV and require 1 data row and 7 numeric/Boolean fields matching the returned row, count. C5: call `_required_ovality_fraction` for each of its 3 aliases, then exercise the fraction route; 3 / 3 aliases selected and diameter residual at most `1e-12` input-length units, with 0 import errors. | The dedicated test already asserts the output row count and ratio; fraction and full-row probes supplement the committed test. [tests/structural/ovality/test_ovality_workflow.py:L25-L41] [src/digitalmodel/structural/ovality/workflow.py:L29-L42] [src/digitalmodel/structural/ovality/workflow.py:L63-L70] |
| G2 | Applicable; geometry arithmetic only | C2: compare `_out_of_roundness` using the section-4 inputs with an exact decimal/rational evaluation of the same diameter difference; absolute residual at most `1e-12` dimensionless. C4: set allowable equal to the computed ratio; require `passes == True` for 1 / 1 equality case, count. | Arithmetic agreement does not verify the DNV attribution or allowable; a published reference remains issue pending (2). [tests/structural/ovality/test_ovality_workflow.py:L14-L17] [tests/structural/ovality/test_ovality_workflow.py:L31-L39] [src/digitalmodel/structural/ovality/workflow.py:L41-L41] |
| G3 | NOT_APPLICABLE | No time-stepping or dynamics quantity in the scalar row. | [src/digitalmodel/structural/ovality/workflow.py:L34-L42] |
| G4 | NOT_APPLICABLE | No diffraction body, mesh or hydrodynamic quantity in the scalar row. | [src/digitalmodel/structural/ovality/workflow.py:L34-L42] |
| G5 | Applicable | C9: require 2 / 2 section-4 cases to carry node ID, code pin, input, tolerance basis and before/after outcome; unresolved provenance items require 1 disposition each, count. | This pack's example table is the evidence index; the runtime CSV has no QA appendix or citation fields. [src/digitalmodel/structural/ovality/workflow.py:L34-L50] |

Check inventory: G0 has 3 checks; G1 has 3; G2 has 2; G3 and G4 have 0; G5 has 1; total 9.

Offline record at the header code pin on 2026-11-07: each section-4 node passed once before and once after the documentation change; each invocation ran 1 case with 1 passed, 0 failed, 0 skipped and 0 NOT_RUN, using the command form below without overriding addopts.

Direct arithmetic and output probes passed C0 (5 exceptions / 5 invalid cases), C2 (residual below `1e-12` dimensionless), C3 (1 row, 7 fields), C4 (1 inclusive-boundary case) and C5 (3 aliases and reconstruction residual below `1e-12` input-length units); C1 and C9 cover both recorded example nodes. [src/digitalmodel/structural/ovality/workflow.py:L25-L41] [src/digitalmodel/structural/ovality/workflow.py:L53-L70] [src/digitalmodel/structural/ovality/workflow.py:L95-L131]

C6 failed: all 4 float-conversion routes accepted text `nan` rather than rejecting it (0 / 4 required rejections); the excessive-fraction probe also produced a negative synthetic minimum diameter, confirming issue pending (1). [src/digitalmodel/structural/ovality/workflow.py:L29-L32] [src/digitalmodel/structural/ovality/workflow.py:L63-L70] [src/digitalmodel/structural/ovality/workflow.py:L103-L131]

C8 failed: 0 / 3 required provenance items (declared example unit, identified standard edition/clause, emitted sidecar) were present; G0 remains incomplete, with issue pending (2), even though the arithmetic checks pass. [examples/workflows/pipe-ovality/input.yml:L3-L8] [src/digitalmodel/structural/ovality/workflow.py:L1-L6] [src/digitalmodel/structural/ovality/workflow.py:L34-L50]

For repeatability, run `PYTHONPATH=src uv run --no-sources python -m pytest <node-id> -q -rA -p no:cacheprovider --basetemp=<scratch-directory>` for C1, preserving repository addopts; use the unchanged durable test in a temporary tree because its default root is the repository. [tests/workflows/test_durable_workflows.py:L21-L27] [tests/workflows/test_durable_workflows.py:L56-L64]

## 6. Known failure modes and silent-failure sweep

sweep: 19 found

The following inventory classifies candidate sites, including intentional defaults and returns; counts refer to rows, not regex match lines.

Exact discovery commands:

```sh
rg -n -i 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|float\(|int\(|unit|cache|default|0\.03' src/digitalmodel/structural/ovality
rg -n 'get\(|open\(|return|if |100\.0|1\.0|2\.0' src/digitalmodel/structural/ovality --glob '*.py'
```

| Site | Symptom and classification | Detection and disposition |
| --- | --- | --- |
| S01 settings fallback | False-valued `pipe_ovality` becomes an empty mapping; missing nominal diameter subsequently raises. [src/digitalmodel/structural/ovality/workflow.py:L19-L20] [src/digitalmodel/structural/ovality/workflow.py:L103-L110] | No detection check exists for false-valued settings; issue pending (3). |
| S02 allowable fallback | Omitted or None allowable selects a code constant without recorded standards provenance. [src/digitalmodel/structural/ovality/workflow.py:L21-L21] [src/digitalmodel/structural/ovality/workflow.py:L118-L120] | C8; issue pending (2). |
| S03 diameter precedence | When both extremes are present the fraction route is ignored. [src/digitalmodel/structural/ovality/workflow.py:L25-L30] | No detection check exists for conflicting routes; issue pending (1). |
| S04 alias precedence | First present fraction alias wins, with no consistency comparison between aliases. [src/digitalmodel/structural/ovality/workflow.py:L64-L69] | C5 covers individual aliases; no detection check exists for conflicts; issue pending (1). |
| S05 fraction text conversion | `float(settings[key])` has no locale normalization or finite-number check; malformed text raises, but non-finite text can escape the negative check. [src/digitalmodel/structural/ovality/workflow.py:L66-L69] | C6 finite-fraction probe; locale-specific coverage absent; issue pending (1). |
| S06 nominal text conversion | `float(value)` plus a non-positive comparison does not establish finiteness. [src/digitalmodel/structural/ovality/workflow.py:L103-L110] | C6 nominal probe; issue pending (1). |
| S07 allowable text conversion | Same finite-number gap in `_optional_positive_float`; external text is passed directly to `float`. [src/digitalmodel/structural/ovality/workflow.py:L118-L124] | C6 allowable probe; issue pending (1). |
| S08 extreme-diameter conversion | `_optional_float` passes supplied text directly to `float`, with no finiteness or locale check. [src/digitalmodel/structural/ovality/workflow.py:L127-L131] | C6 extreme-diameter probe; issue pending (1). |
| S09 None return | Missing optional diameter returns None intentionally; a single missing extreme raises, while two missing extremes select the fraction route. [src/digitalmodel/structural/ovality/workflow.py:L22-L30] [src/digitalmodel/structural/ovality/workflow.py:L127-L131] | No detection check exists for the missing-pair branches; issue pending (4). |
| S10 output-directory default | Omitted `output_dir` selects `results`. [src/digitalmodel/structural/ovality/workflow.py:L73-L78] | No detection check exists for omitted output directory; issue pending (3). |
| S11 configuration-location fallback | Missing configuration-directory metadata selects file parent, then current directory. [src/digitalmodel/structural/ovality/workflow.py:L81-L86] | No detection check exists for current-directory fallback; issue pending (3). |
| S12 filename fallback | Missing input-file metadata selects basename, then `pipe_ovality`. [src/digitalmodel/structural/ovality/workflow.py:L89-L92] | No detection check exists for both fallback branches; issue pending (3). |
| S13 swallowed path exception | `_display_path` catches `ValueError` and returns an absolute path; its root constant resolves to the repository directory. [src/digitalmodel/structural/ovality/workflow.py:L15-L15] [src/digitalmodel/structural/ovality/workflow.py:L134-L139] | No detection check exists for repository-relative display or external-root fallback; issue pending (3). |
| S14 implicit unit consistency | Mixed length units can produce an unqualified numerical ratio because no unit conversion or declaration is checked. [src/digitalmodel/structural/ovality/workflow.py:L53-L60] | C8; issue pending (2). |
| S15 reconstructed geometry | An unrestricted supplied fraction can construct a non-positive minimum diameter without invoking `_out_of_roundness`. [src/digitalmodel/structural/ovality/workflow.py:L29-L32] [src/digitalmodel/structural/ovality/workflow.py:L63-L70] | No committed detection check exists; issue pending (1). |
| S16 overwrite | CSV is opened in write mode, replacing any existing file; this is result output, not an empty-result cache. [src/digitalmodel/structural/ovality/workflow.py:L95-L100] | C3 checks a new output; no detection check exists for overwrite policy; issue pending (3). |
| S17 zero thresholds and arithmetic constants | Zero is a rejection threshold rather than an error fallback; 1 and 2 construct symmetric diameters and 100 converts fraction to percent. [src/digitalmodel/structural/ovality/workflow.py:L31-L39] [src/digitalmodel/structural/ovality/workflow.py:L53-L68] [src/digitalmodel/structural/ovality/workflow.py:L103-L124] | C0, C2, C5; intentional conventions. |
| S18 text encoding | CSV writing does not specify an encoding. [src/digitalmodel/structural/ovality/workflow.py:L95-L100] | No detection check exists for encoding portability; issue pending (3). |
| S19 settings replacement | Returned `pipe_ovality` replaces the original settings, retaining the computed row and display path only. [src/digitalmodel/structural/ovality/workflow.py:L46-L50] | C3 covers the row; no detection check exists for preserving input-only keys; issue pending (3). |

The complete module has no `continue`, empty-list/dict error return, integer conversion, epoch fallback or cache write; the None return and caught path exception are the explicit branches above. [src/digitalmodel/structural/ovality/workflow.py:L18-L139] [src/digitalmodel/structural/ovality/__init__.py:L1-L2]

## 7. Lessons log

This table is append-only; identifiers for an external shared log remain unassigned in this initial draft (issue pending (4)).

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-11-07 | Initial pack revision 0 | Keep geometry regression evidence separate from standards qualification | [tests/structural/ovality/test_ovality_workflow.py:L31-L41] [src/digitalmodel/structural/ovality/workflow.py:L1-L6] | Sections 3–5 distinguish the arithmetic check from a governing acceptance basis | Unassigned; issue pending (4) |

## 8. Review responsibilities

Proposed routing for this draft: engineering calculations quality manager; deputy role to be assigned by the maintainer (issue pending (4)).

The producing function is calculation documentation; ambiguous standard editions, units, tolerances and duplicate-method classifications require the family quality manager's disposition before clearance.

Escalate cross-family scope and scheduling to the maintainers; keep the pack status DRAFT until independent clearance.

## 9. Dependencies and implementation inventory

The engine dispatches `basename: pipe_ovality` to this package's router; application configuration is supplied through `assetutilities`. [src/digitalmodel/engine.py:L10-L14] [src/digitalmodel/engine.py:L489-L492]

The package itself imports only standard-library CSV, path and typing facilities; its output is a CSV file and the mutated configuration, with no solver invocation in the complete router. [src/digitalmodel/structural/ovality/workflow.py:L8-L12] [src/digitalmodel/structural/ovality/workflow.py:L18-L50]

The workflow registry links the public YAML example, CSV output and offline regression node; the base configuration supplies the same measured-diameter inputs. [docs/registry/workflows.yaml:L338-L345] [src/digitalmodel/base_configs/modules/pipe_ovality/pipe_ovality.yml:L1-L14]

Implementation-map revision: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; map-declared source revision: `270c8759c374b67a7f032953e7fcdb14e0224461`; map citations below use that explicit revision rather than the code pin. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L3]

Inventory: 0 map families with an implementation row under the header package, 0 inventory rows, 0 counted families; counted-family grep `NOT_APPLICABLE` because no family qualifies; there are no counted further implementations or excluded signals to classify. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502]

Reproduce the empty inventory with this read-only calculation:

```python
import subprocess
import yaml
revision = "3256bab95f84da3d9fadce8d5dcf8458d66d1eb4"
text = subprocess.check_output([
    "git", "show", revision + ":docs/registry/implementation-map.yaml"
], text=True)
package = "src/digitalmodel/structural/ovality/"
families = yaml.safe_load(text)["families"]
ranked = []
for order, family in enumerate(families):
    rows = family.get("implementations", [])
    inside = [row for row in rows if row["path"].startswith(package)]
    if inside:
        ranked.append((len(inside), order, family["id"], rows))
ranked.sort(key=lambda row: (-row[0], row[1]))
print("families", len(ranked), "counted", len(ranked[:3]))
for count, order, family_id, rows in ranked:
    print(family_id, count, rows)
```

Map follow-up: add measured-diameter out-of-roundness, supplied-fraction diameter reconstruction and allowable comparison from section 3 before counting those families in a later map revision (issue pending (4)). [src/digitalmodel/structural/ovality/workflow.py:L25-L41] [src/digitalmodel/structural/ovality/workflow.py:L53-L70]

Repository-wide discovery includes the following additional implementation sites; these are outside the package and are map gaps, not counted misses. [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L75-L82] [scripts/python/digitalmodel/analysis/RigidPipe.py:L60-L67] [scripts/python/digitalmodel/analysis/collapse.py:L26-L30]

| Site | Classification / relationship | Follow-up |
| --- | --- | --- |
| `RigidPipe.updateOvality` in the pipe-capacity package | Computes the diameter-difference ratio itself and applies its own lower bound; do not use it as an independent standard check | [src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py:L74-L82]; issue pending (4) |
| `RigidPipe.updateOvality` in the analysis script | Computes the same ratio itself with a lower bound | [scripts/python/digitalmodel/analysis/RigidPipe.py:L59-L67]; issue pending (4) |
| Module-level collapse script expression | Uses the sum of extreme diameters as denominator, so its parameter cannot be substituted directly for this module's nominal-diameter ratio | [scripts/python/digitalmodel/analysis/collapse.py:L26-L30]; issue pending (4) |

## 10. Coverage and open items

Read coverage includes the complete two-file Python package, the dedicated test, durable-test loading and ovality assertion branch, public example, base configuration, engine dispatch and routing/workflow entries. [src/digitalmodel/structural/ovality/__init__.py:L1-L2] [src/digitalmodel/structural/ovality/workflow.py:L1-L139] [tests/structural/ovality/test_ovality_workflow.py:L1-L41] [tests/workflows/test_durable_workflows.py:L21-L64] [tests/workflows/test_durable_workflows.py:L548-L564] [examples/workflows/pipe-ovality/input.yml:L1-L14] [src/digitalmodel/base_configs/modules/pipe_ovality/pipe_ovality.yml:L1-L14] [src/digitalmodel/engine.py:L489-L492] [docs/registry/module-routing.yaml:L525-L530] [docs/registry/workflows.yaml:L338-L345]

Documentation discovery uses the following commands; no package README or pre-existing package documentation is present in the pinned two-file tree, and no Markdown file under `docs/` directly names the package or durable workflow. [src/digitalmodel/structural/ovality/__init__.py:L1-L2] [src/digitalmodel/structural/ovality/workflow.py:L1-L139]

```sh
git ls-tree -r --name-only aebd7e4bc5c3dda5f7433e20e021dd58ac17d0b3 -- src/digitalmodel/structural/ovality
rg -l 'pipe_ovality|pipe-ovality|structural/ovality|structural\.ovality' docs
rg -l 'pipe_ovality|pipe-ovality|structural/ovality|structural\.ovality' docs --glob '*.md'
git grep -n -E 'Dmax|Dmin|out_of_roundness|updateOvality' aebd7e4bc5c3dda5f7433e20e021dd58ac17d0b3 -- '*.py'
```

The pinned documentation discoveries are `docs/registry/workflows.yaml` and `docs/audits/workflow-failure-evidence-coverage.html`; the latter labels pipe-ovality coverage as implicit and offline and bounds its evidence to synthetic failure injection rather than engineering qualification. [docs/registry/workflows.yaml:L338-L345] [docs/audits/workflow-failure-evidence-coverage.html:L7-L10] [docs/audits/workflow-failure-evidence-coverage.html:L85-L85]

The audit's coverage statement does not establish this router's standard edition. [docs/audits/workflow-failure-evidence-coverage.html:L7-L10] [src/digitalmodel/structural/ovality/workflow.py:L1-L6]

| Item | Disposition | Responsible role / issue |
| --- | --- | --- |
| U1 diameter units; U2 standard edition, clause and allowable provenance; citation-sidecar gap | Establish an explicit unit contract and authoritative standard reference without reproducing licensed text or tabulated values | Maintainer; issue pending (2) |
| U3 independently sourced worked example and tolerance | Current two cases remain regression examples; add independent expected values with declared units | Maintainer; issue pending (2) |
| Non-finite inputs, incompatible routes/aliases and excessive supplied fraction | Add rejection checks and tests before treating arbitrary external inputs as finite numerical evidence | Maintainer; issue pending (1) |
| Output path, overwrite, encoding and configuration fallback branches | Define expected behavior and add detection coverage for section-6 sites | Maintainer; issue pending (3) |
| Missing boundary, invalid-input and supplied-fraction regression coverage | Extend solver-free tests; keep the present two-node evidence distinct | Maintainer; issue pending (4) |
| Map families and repository-wide alternate formulas | Add the section-3 methods to the map, then review the alternate implementations listed in section 9 | Maintainer; issue pending (4) |
| Deputy assignment and shared lesson-log reference | Assign the deputy role and external log entry before clearance | Maintainer; issue pending (4) |

The calculation-output citation contract was readable; it introduces no additional source document to quote here, and no licensed standard was opened for this pack.

Issue numbers replace the numbered draft markers after the maintainers create the drafts; independent clearance remains pending.
