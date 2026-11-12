# Wellpath knowledge pack

| Header | Value |
| --- | --- |
| Module | well/wellpath |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/well/wellpath` |
| Pack revision | 0 |
| Code revision | `b47e42fbe7208c69b28dd436aa91fb129bc385f7` |
| Implementation-map reference revision | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Analysis family | Engineering calculations (EC) |
| Family quality manager role | Engineering calculations quality manager |
| Deputy role | Engineering calculations deputy quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`router` computes stationwise minimum-curvature positions and dogleg severity, writes a CSV, and replaces `cfg["wellpath"]` with a summary. [src/digitalmodel/well/wellpath/workflow.py:L14-L33]

The existing `well` domain has registry maturity `stable`; this is routing metadata, not an independent accuracy qualification of this subpackage. [docs/registry/module-routing.yaml:L646-L661]

Scope is the router and its private helpers; the engine dispatches basename `wellpath` to this router, while the legacy graphical survey implementation is separate. [src/digitalmodel/engine.py:L801-L804] [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L783-L803]

Positions start at the first supplied station with TVD, north and east equal to zero; positive TVD follows `cos(inclination)`, north follows `sin(inclination)*cos(azimuth)`, and east follows `sin(inclination)*sin(azimuth)`. [src/digitalmodel/well/wellpath/workflow.py:L40-L85]

The calculation accepts inclination through 180 degrees and nonnegative increasing MD, but does not require the initial MD to equal zero. [src/digitalmodel/well/wellpath/workflow.py:L143-L156]

## 2. Inputs and outputs

Input contract; numeric values pass through Python `float` and finite checks rather than a unit-conversion library. [src/digitalmodel/well/wellpath/workflow.py:L191-L206]

| Input | Unit | Allowed range / default | Source and evidence |
| --- | --- | --- | --- |
| `wellpath.stations` | records | Nonempty list of mappings; one station is permitted | `_stations`; [src/digitalmodel/well/wellpath/workflow.py:L120-L141] |
| `measured_depth_m` | m | Required, finite, >= 0, strictly increasing | `_stations`, `_required_float`; [src/digitalmodel/well/wellpath/workflow.py:L131-L133] [src/digitalmodel/well/wellpath/workflow.py:L143-L150] [src/digitalmodel/well/wellpath/workflow.py:L191-L198] |
| `inclination_deg` | degrees from positive TVD direction | Required, finite, 0 to 180 | `_stations` and displacement equations; [src/digitalmodel/well/wellpath/workflow.py:L62-L67] [src/digitalmodel/well/wellpath/workflow.py:L134-L136] [src/digitalmodel/well/wellpath/workflow.py:L151-L154] |
| `azimuth_deg` | degrees, north toward east | Required and finite; no bounded interval or explicit normalization | `_required_float`, displacement equations; [src/digitalmodel/well/wellpath/workflow.py:L68-L85] [src/digitalmodel/well/wellpath/workflow.py:L137-L139] [src/digitalmodel/well/wellpath/workflow.py:L143-L156] [src/digitalmodel/well/wellpath/workflow.py:L191-L198] |
| `dls_normalization` | m | Positive finite; default 30.0 | `_positive_float`; [src/digitalmodel/well/wellpath/workflow.py:L17-L17] [src/digitalmodel/well/wellpath/workflow.py:L95-L95] [src/digitalmodel/well/wellpath/workflow.py:L201-L206] |
| `output_dir` | filesystem path | Default `results`; relative to configuration directory | `_results_csv_path`; [src/digitalmodel/well/wellpath/workflow.py:L159-L164] |
| `_config_dir_path`, `_config_file_path`, `basename` | path / text | Directory preference: explicit directory, file parent, current directory; stem preference: file stem, basename, `wellpath` | `_config_dir`, `_input_stem`; [src/digitalmodel/well/wellpath/workflow.py:L167-L178] |

Output contract; the writer emits the following seven columns without unit metadata. [src/digitalmodel/well/wellpath/workflow.py:L181-L188]

| Output | Unit | Meaning / range | Evidence |
| --- | --- | --- | --- |
| CSV `md`, `inc`, `azi` | m, degrees, degrees | Supplied station values | [src/digitalmodel/well/wellpath/workflow.py:L40-L49] [src/digitalmodel/well/wellpath/workflow.py:L87-L97] |
| CSV `tvd`, `north`, `east` | m | Cumulative displacement relative to first station; positive signs defined in section 1 | [src/digitalmodel/well/wellpath/workflow.py:L52-L85] |
| CSV `dls` | degrees per `dls_normalization` m | First row 0; following interval dogleg scaled by normalization / MD increment | [src/digitalmodel/well/wellpath/workflow.py:L48-L48] [src/digitalmodel/well/wellpath/workflow.py:L95-L95] |
| Summary `points` | count | Number of rows | [src/digitalmodel/well/wellpath/workflow.py:L24-L25] |
| Summary `total_depth`, `total_tvd` | m | Last MD and last relative TVD, respectively | [src/digitalmodel/well/wellpath/workflow.py:L26-L27] |
| Summary `max_dls`, `max_dls_depth` | degrees per normalization m; m | Maximum DLS and its ending-station MD; `max` returns the first tied row | [src/digitalmodel/well/wellpath/workflow.py:L23-L30] |
| Summary `dls_normalization`, `wellpath_csv` | m; path text | Normalization retained only in summary; CSV path relative to repository when possible, otherwise absolute | [src/digitalmodel/well/wellpath/workflow.py:L30-L31] [src/digitalmodel/well/wellpath/workflow.py:L209-L214] |

Callable inventory, including private helpers to make the numerical and output boundaries explicit. [src/digitalmodel/well/wellpath/workflow.py:L14-L214]

| Definition | Role | Evidence |
| --- | --- | --- |
| `workflow.router` | Validates, calculates, writes and summarizes | [src/digitalmodel/well/wellpath/workflow.py:L14-L33] |
| `workflow._minimum_curvature_survey` | Integrates the station positions and DLS | [src/digitalmodel/well/wellpath/workflow.py:L36-L98] |
| `workflow._dogleg_angle` | Computes bounded-acos tangent separation | [src/digitalmodel/well/wellpath/workflow.py:L101-L111] |
| `workflow._ratio_factor` | Computes tangent ratio with straight-segment limit | [src/digitalmodel/well/wellpath/workflow.py:L114-L117] |
| `workflow._stations` | Validates station structure and ranges | [src/digitalmodel/well/wellpath/workflow.py:L120-L156] |
| `workflow._results_csv_path` | Resolves output directory and filename | [src/digitalmodel/well/wellpath/workflow.py:L159-L164] |
| `workflow._config_dir` | Resolves configuration directory | [src/digitalmodel/well/wellpath/workflow.py:L167-L172] |
| `workflow._input_stem` | Chooses output stem | [src/digitalmodel/well/wellpath/workflow.py:L175-L178] |
| `workflow._write_results_csv` | Creates parents and overwrites CSV | [src/digitalmodel/well/wellpath/workflow.py:L181-L188] |
| `workflow._required_float` | Requires and validates a finite numeric field | [src/digitalmodel/well/wellpath/workflow.py:L191-L198] |
| `workflow._positive_float` | Requires positive finite normalization | [src/digitalmodel/well/wellpath/workflow.py:L201-L206] |
| `workflow._display_path` | Formats repository-relative or absolute path | [src/digitalmodel/well/wellpath/workflow.py:L209-L214] |

## 3. Method and governing standards

With inclinations `i1,i2` and azimuths `a1,a2` converted to radians, `_dogleg_angle` computes `beta = acos(clamp(sin(i1)*sin(i2)*cos(a2-a1) + cos(i1)*cos(i2), -1, 1))`. [src/digitalmodel/well/wellpath/workflow.py:L55-L60] [src/digitalmodel/well/wellpath/workflow.py:L101-L111]

`_ratio_factor` returns `RF=1` for `abs(beta)<=1e-12` radians and otherwise `RF=(2/beta)*tan(beta/2)`. [src/digitalmodel/well/wellpath/workflow.py:L114-L117]

For each MD increment `ds`, `_minimum_curvature_survey` adds `ds*RF/2` times the sum of the two endpoint tangent components; DLS is `degrees(beta)*dls_normalization/ds`. [src/digitalmodel/well/wellpath/workflow.py:L54-L95]

The implementation and example provide no named governing standard, edition, clause or emitted `Citation` sidecar; the test named textbook case gives fixed expected coordinates without a published attribution. [src/digitalmodel/well/wellpath/workflow.py:L1-L214] [examples/workflows/wellpath/input.yml:L1-L27] [tests/workflows/test_durable_workflows.py:L1805-L1843]

UNVERIFIED: independent published provenance and accuracy envelope for the general minimum-curvature algorithm have not been established here; issue pending (3).

## 4. Validated worked examples

Validated examples: 0; both existing workflow cases are classified as regression examples because their expected outputs have no independent published attribution in their test definitions. [tests/workflows/test_durable_workflows.py:L1345-L1358] [tests/workflows/test_durable_workflows.py:L1805-L1843]

| Case / classification | Source and input | Expected output and tolerance | Existing test node ID | Offline result at code pin |
| --- | --- | --- | --- | --- |
| R1 durable survey / regression | Five stations, MD 0 to 400 m, inclination 0 to 30 degrees, zero azimuth; [examples/workflows/wellpath/input.yml:L1-L21] | MD 400.0 m and maximum-DLS depth 200.0 m with pytest default approximation; 5 rows, positive DLS and TVD below MD; no independent positional tolerance claimed. [tests/workflows/test_durable_workflows.py:L1345-L1358] | `tests/workflows/test_durable_workflows.py::test_workflow_registry[wellpath]` | PASS at BASE and AFTER |
| R2 single build / regression | Temporary YAML with MD 0 and 100 m, inclination 0 and 30 degrees, azimuth 0; [tests/workflows/test_durable_workflows.py:L1805-L1837] | MD 100.0 m; TVD 95.4929658551 m; north 25.5872630837 m; east 0 m with absolute tolerance 1e-12 m; DLS 9 degrees per 30 m. Other comparisons use pytest default approximation, not an engineering allowance. [tests/workflows/test_durable_workflows.py:L1839-L1843] | `tests/workflows/test_durable_workflows.py::test_wellpath_minimum_curvature_textbook_case` | PASS at BASE and AFTER |

The durable registry identifies R1 as offline; its test executes the engine and checks the registered output file. Runs for this pack use a temporary repository copy so the checked-in example directory receives no outputs. [docs/registry/workflows.yaml:L996-L1003] [tests/workflows/test_durable_workflows.py:L55-L94]

Independent method qualification, including general three-dimensional survey benchmarks, remains issue pending (3). [tests/workflows/test_durable_workflows.py:L1805-L1843]

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations, with G5 from section 9; the following criteria are pack checks rather than claimed licensed-standard acceptance limits.

| Gate | Applicability | Module-specific check and criterion | Evidence / current disposition |
| --- | --- | --- | --- |
| G0 | Applicable: inputs, units, citations | C01, C02 and C03 below | Input and artifact diagnostics; open limitations in section 6 |
| G1 | Applicable: imports and tests | C04 below; 0 failed tests (count) | Python router through the engine; no program-specific load route because this router writes CSV, not solver input. [src/digitalmodel/engine.py:L801-L804] [src/digitalmodel/well/wellpath/workflow.py:L181-L188] |
| G2 | Applicable: hand check | C05 below; coordinate residual <= 1e-9 m and DLS residual <= 1e-12 degrees per 30 m | Circular-arc check of R2; general published benchmark still issue pending (3). [tests/workflows/test_durable_workflows.py:L1812-L1825] |
| G3 | NOT_APPLICABLE | No dynamics time integration in the station survey | [src/digitalmodel/well/wellpath/workflow.py:L36-L98] |
| G4 | NOT_APPLICABLE | No diffraction mesh or hydrodynamic response calculation in the station survey | [src/digitalmodel/well/wellpath/workflow.py:L36-L98] |
| G5 | Applicable: evidence appendix | C06 below; 0 missing evidence fields (count) | Retain external evidence; the router itself emits CSV and summary only. [src/digitalmodel/well/wellpath/workflow.py:L19-L33] [src/digitalmodel/well/wellpath/workflow.py:L181-L188] |

Reproduce C01–C03 and C05 with a direct import of `digitalmodel.well.wellpath.workflow`; use temporary output directories and retain the JSON diagnostic record, inputs and CSV digest as C06 evidence. The checks target these callable implementations. [src/digitalmodel/well/wellpath/workflow.py:L14-L214]

| Check | Executable procedure and numeric criterion with unit | Evidence and gate |
| --- | --- | --- |
| C01 | Call `_stations` on empty, missing, nonlist, nonmapping, missing-field, negative-MD, duplicate-MD, decreasing-MD, out-of-range inclination and nonfinite inputs; `_positive_float` on zero/nonfinite normalization. All 14 invalid cases raise (14 errors; 0 accepted records). | JSON rejection count; G0. [src/digitalmodel/well/wellpath/workflow.py:L120-L156] [src/digitalmodel/well/wellpath/workflow.py:L191-L206] |
| C02 | Diagnose 5 edge cases through `_stations` / `_minimum_curvature_survey`: nonzero starting MD, boolean MD, antiparallel tangents, one station, and finite inputs with an extremely short MD interval. Require 5 recorded observations (count), including the 100 m TVD / 200 m last-MD mismatch for the nonzero-start vertical pair; this documents behavior without qualifying it. | JSON observations; G0; issue pending (1). [src/digitalmodel/well/wellpath/workflow.py:L40-L52] [src/digitalmodel/well/wellpath/workflow.py:L114-L117] [src/digitalmodel/well/wellpath/workflow.py:L143-L156] |
| C03 | Exercise `router` and its output/path helpers on a 100 m vertical pair: exactly 7 CSV columns, 2 rows, final TVD 100 m and north/east/DLS 0 within 1e-12 in their respective units; RF at beta=0 equals 1 (dimensionless). Check directory/stem precedence, external path fallback, normalization retention, tied maximum at first MD, replacement of input settings and existing CSV; 0 mismatched assertions (count). | CSV plus JSON diagnostic; G0; output retention/provenance limitations remain issue pending (2). [src/digitalmodel/well/wellpath/workflow.py:L14-L33] [src/digitalmodel/well/wellpath/workflow.py:L114-L117] [src/digitalmodel/well/wellpath/workflow.py:L159-L214] |
| C04 | Run R1 and R2 test node IDs plus `tests/well/completion/test_pump_down.py`; 40 passed, 0 failed, 0 skipped tests (count). Consumer results qualify compatibility, not independent positional accuracy. | Pytest node results; G1. [tests/workflows/test_durable_workflows.py:L1345-L1358] [tests/workflows/test_durable_workflows.py:L1805-L1843] [tests/well/completion/test_pump_down.py:L194-L267] |
| C05 | Independently integrate a circular arc for R2: `theta=radians(30)`, `R=100/theta`, TVD=`R*sin(theta)`, north=`R*(1-cos(theta))`, east=0, DLS=`30*30/100`. Compare against survey row 1 with maximum coordinate residual <= 1e-9 m and DLS residual <= 1e-12 degrees per 30 m; criteria are arithmetic checks, not field accuracy claims. | Hand-check JSON residuals; G2; supplied geometry from [tests/workflows/test_durable_workflows.py:L1812-L1825]; implementation being checked at [src/digitalmodel/well/wellpath/workflow.py:L62-L95] |
| C06 | Retain code revision, exact input, input SHA-256, output SHA-256, unit labels, DLS normalization, check results and test node outcomes: 8 fields present, 0 missing fields (count). This is an external evidence record, not a built-in module appendix. | JSON manifest; G5; summary/writer boundary at [src/digitalmodel/well/wellpath/workflow.py:L24-L32] [src/digitalmodel/well/wellpath/workflow.py:L181-L188] |

Recorded direct results: C01 rejected 14 invalid cases; C02 retained 5 edge observations; C03/C05/C06 completed with 0 assertion mismatches, and C04 passed 40 tests at BASE and AFTER. The hand-check residuals met the stated criteria. [src/digitalmodel/well/wellpath/workflow.py:L14-L214] [tests/workflows/test_durable_workflows.py:L1805-L1843] [tests/well/completion/test_pump_down.py:L194-L267]

## 6. Known failure modes and pitfalls

sweep: 28 found

The sweep classifies defaults, numeric conversions, unit assumptions and output/error handling, including intentional safeguards; each location has a row below, and rows with unresolved engineering or evidence limits carry a draft reference. [src/digitalmodel/well/wellpath/workflow.py:L14-L214]

Exact command used, followed by full-file inspection of both Python files:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|0\.0|1970|0\.05|5 ?%|\b(int|float)\(|radians|normalization|cache|\.get\(|acos|1\.0e-12|open\("w"|cfg\["wellpath"\]' src/digitalmodel/well/wellpath --glob '*.py'
```

| ID | Location and symptom / cause | Detection check and disposition |
| --- | --- | --- |
| S01 | Missing/falsy wellpath settings become an empty mapping, then fail station validation; not a silent empty success. [src/digitalmodel/well/wellpath/workflow.py:L15-L16] [src/digitalmodel/well/wellpath/workflow.py:L120-L123] | C01; explicit error |
| S02 | Omitted DLS normalization silently uses 30.0 m. [src/digitalmodel/well/wellpath/workflow.py:L17-L17] | C03; retain normalization, issue pending (2) |
| S03 | `float` in maximum selection converts internal DLS values; tied maxima select the first row. [src/digitalmodel/well/wellpath/workflow.py:L23-L23] | C03; first-row zero-DLS tie documented |
| S04 | `float` casts last internal MD into summary total depth. [src/digitalmodel/well/wellpath/workflow.py:L26-L26] | C03; dimensional output check |
| S05 | `float` casts last internal TVD into summary. [src/digitalmodel/well/wellpath/workflow.py:L27-L27] | C03; dimensional output check |
| S06 | `float` casts the selected internal maximum DLS. [src/digitalmodel/well/wellpath/workflow.py:L28-L28] | C03; zero-DLS limit |
| S07 | `float` casts selected internal maximum-DLS depth. [src/digitalmodel/well/wellpath/workflow.py:L29-L29] | C03; tied-maximum depth check |
| S08 | First TVD is forced to zero even for nonzero starting MD. [src/digitalmodel/well/wellpath/workflow.py:L42-L45] | C02; issue pending (1) |
| S09 | First north coordinate defaults to zero; no supplied positional tie-in is used. [src/digitalmodel/well/wellpath/workflow.py:L40-L49] | C03; relative origin documented |
| S10 | First east coordinate defaults to zero. [src/digitalmodel/well/wellpath/workflow.py:L47-L47] | C03; relative origin documented |
| S11 | First DLS defaults to zero without a preceding interval. [src/digitalmodel/well/wellpath/workflow.py:L48-L48] | C02/C03; singleton and tie behavior, issue pending (1) |
| S12 | TVD/north/east accumulators start at zero, independently of supplied MD. [src/digitalmodel/well/wellpath/workflow.py:L52-L54] | C02; issue pending (1) |
| S13 | Angles assume degrees on input; MD and normalization share metres; trigonometric accumulation uses radians. [src/digitalmodel/well/wellpath/workflow.py:L54-L58] [src/digitalmodel/well/wellpath/workflow.py:L95-L95] | C03/C05; retain unit labels externally, issue pending (2) |
| S14 | Acos input is silently clamped to [-1,1], masking roundoff outside the interval. [src/digitalmodel/well/wellpath/workflow.py:L107-L111] | C03; aligned/antialigned tangent angle checks |
| S15 | RF returns exactly 1 for dogleg magnitude <= 1e-12 radians. [src/digitalmodel/well/wellpath/workflow.py:L114-L117] | C03; zero and threshold checks |
| S16 | `previous_md=None` bypasses the increasing check only for the first station; first MD need not be zero. [src/digitalmodel/well/wellpath/workflow.py:L143-L155] | C01/C02; issue pending (1) |
| S17 | Output directory defaults to `results`. [src/digitalmodel/well/wellpath/workflow.py:L159-L164] | C03; path precedence |
| S18 | Missing configuration paths fall back to current working directory. [src/digitalmodel/well/wellpath/workflow.py:L167-L172] | C03; path precedence, issue pending (2) |
| S19 | Missing file path and basename silently use output stem `wellpath`. [src/digitalmodel/well/wellpath/workflow.py:L175-L178] | C03; stem precedence |
| S20 | External station text passes to `float`, with finite enforcement; comma separators raise, but booleans are accepted numerically. [src/digitalmodel/well/wellpath/workflow.py:L191-L198] | C01/C02; boolean typing limit, issue pending (1) |
| S21 | External normalization text passes to `float`, then positive/finite validation; comma separators raise. [src/digitalmodel/well/wellpath/workflow.py:L201-L206] | C01/C03; normalized-string and rejection checks |
| S22 | The only caught exception is `ValueError` from repository-relative path conversion; fallback returns an absolute path. [src/digitalmodel/well/wellpath/workflow.py:L209-L214] | C03; path disclosure/portability, issue pending (2) |
| S23 | CSV field names omit unit and normalization metadata. [src/digitalmodel/well/wellpath/workflow.py:L181-L188] | C03/C06; issue pending (2) |
| S24 | Summary assignment replaces input settings, including stations and output directory. [src/digitalmodel/well/wellpath/workflow.py:L24-L32] | C03/C06; issue pending (2) |
| S25 | CSV `open("w")` overwrites an existing destination without a retention check. [src/digitalmodel/well/wellpath/workflow.py:L181-L188] | C03; issue pending (2) |
| S26 | The router accepts a one-station list and reports zero displacement and DLS. [src/digitalmodel/well/wellpath/workflow.py:L40-L52] [src/digitalmodel/well/wellpath/workflow.py:L120-L129] | C02; no interval accuracy can be checked, issue pending (1) |
| S27 | Exactly antiparallel tangents reach the tangent ratio without a unique-arc guard; the consumer separately rejects this geometry. [src/digitalmodel/well/wellpath/workflow.py:L101-L117] [src/digitalmodel/well/completion/pump_down/force_balance.py:L168-L169] | C02; issue pending (1) |
| S28 | Positive finite MD spacing can be too small for finite DLS: angle times normalization divided by spacing overflows, with no output finite check before CSV serialization. [src/digitalmodel/well/wellpath/workflow.py:L54-L95] [src/digitalmodel/well/wellpath/workflow.py:L181-L188] | C02; issue pending (1) |

No swallowing `continue`, error-path `return None`, empty-result return, epoch default, percentage fallback or cache write is present in the two-file package; empty local lists are populated or rejected before use. [src/digitalmodel/well/wellpath/__init__.py:L1-L2] [src/digitalmodel/well/wellpath/workflow.py:L1-L214]

Unresolved findings use the draft references above.

## 7. Lessons log

| Date | Change identifier | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-11-08 | Initial pack revision 0 | Relative first-station coordinates must not be interpreted as an absolute surface tie-in; retain the starting MD with the result | [src/digitalmodel/well/wellpath/workflow.py:L40-L54] | Inputs and S08/S12/S16 document the limitation | issue pending (1) |
| 2026-11-08 | Initial pack revision 0 | Preserve input and normalization beside a CSV because the returned configuration replaces the input settings | [src/digitalmodel/well/wellpath/workflow.py:L24-L32] [src/digitalmodel/well/wellpath/workflow.py:L181-L188] | C06 defines an external evidence manifest | issue pending (2) |

## 8. Ownership

Review assignments for this draft: engineering calculations quality manager; deputy engineering calculations quality manager; producing discipline: engineering calculations. A separate provider reviews the evidence before the maintainer merges; unresolved method classification goes to the family quality manager. Review scope is AQG r0, sections 10–11, Python calculations; these are assignments, not a claim of clearance.

## 9. Dependencies and implementation inventory

The package imports Python standard-library CSV, math, path and typing facilities; the engine provides its workflow dispatch, and the pump-down consumer imports the private station validator and survey calculation. [src/digitalmodel/well/wellpath/workflow.py:L3-L8] [src/digitalmodel/engine.py:L801-L804] [src/digitalmodel/well/completion/pump_down/force_balance.py:L17-L17] [src/digitalmodel/well/completion/pump_down/force_balance.py:L152-L169]

The durable registry's input is `examples/workflows/wellpath/input.yml`, with CSV output and an offline test node; no solver version is selected by the router. [docs/registry/workflows.yaml:L996-L1003] [src/digitalmodel/well/wellpath/workflow.py:L14-L33]

Implementation-map reference: `origin/codex/implmap-work`, reference commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; the inventory is evaluated at that snapshot, separately from the header's code revision. Citations to the implementation map use the separate map pin in the header. [docs/registry/implementation-map.yaml:L1-L4]

| Map inventory | Result |
| --- | --- |
| Families having a row inside the package | 0; complete inventory at map pin: [docs/registry/implementation-map.yaml:L345-L53690] |
| Implementations to list for those families | 0; empty inventory |
| Counted families | 0 |
| Counted-family signal grep | NOT_APPLICABLE: no map family has an implementation row inside this package |
| Further in-package second implementations | NOT_APPLICABLE: no counted family |

Map follow-up: minimum-curvature positional integration, spherical-tangent dogleg angle and the ratio-factor/DLS calculations in section 3 need an in-package family entry before they become counted; issue pending (4). [src/digitalmodel/well/wellpath/workflow.py:L36-L117]

The additional whole-repository search was `rg -n -i 'wellpath|minimum.curvature' scripts src docs tests --glob '*.py' --glob '*.md' --glob '*.yaml'`; it is discovery evidence, not a counted-family signal grep.

| Related implementation / helper | Classification and reason | Follow-up |
| --- | --- | --- |
| `_minimum_curvature_survey`, `_dogleg_angle`, `_ratio_factor` | In-package positional integration, dogleg and ratio factor; fully listed in section 3. [src/digitalmodel/well/wellpath/workflow.py:L36-L117] | Map gap; issue pending (4) |
| Nested `myApplication.surveyWin.writesurvey` | Outside-package independent implementation; computes dogleg, normalized DLS, RF and displacements directly; RF uses normalized DLS rather than raw interval dogleg. It is not an independent accuracy reference for this pack. [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L713-L715] [src/digitalmodel/infrastructure/base_solvers/well/wellpath3D.py:L783-L803] | Map gap and qualification question; issue pending (4) |
| Compatibility `wellpath3D` module | Re-exports legacy definitions; no second calculation at the re-export. [src/digitalmodel/infrastructure/common/wellpath3D.py:L16-L22] | Record helper relation; issue pending (4) |
| Pump-down `tension_profile` | Delegates station validation and survey to this package, then computes its own cable-force integration; reuse is not a second survey implementation. [src/digitalmodel/well/completion/pump_down/force_balance.py:L152-L169] | Downstream compatibility covered by C04 |
| Engine and workflow registry | Dispatch and data routing helpers, not independent survey algorithms. [src/digitalmodel/engine.py:L801-L804] [docs/registry/workflows.yaml:L996-L1003] | None |

## 10. Coverage and open items

Read coverage: both package Python files, their full callable inventory in section 2, the registered input, the wellpath branches and explicit example test, and the downstream survey integration and its offline tests. No package-local test folder or README is part of this package at the code pin. [src/digitalmodel/well/wellpath/__init__.py:L1-L2] [src/digitalmodel/well/wellpath/workflow.py:L1-L214] [examples/workflows/wellpath/input.yml:L1-L27] [tests/workflows/test_durable_workflows.py:L1345-L1358] [tests/workflows/test_durable_workflows.py:L1805-L1843] [src/digitalmodel/well/completion/pump_down/force_balance.py:L120-L174] [tests/well/completion/test_pump_down.py:L187-L267]

Documentation discovery found the durable registry, legacy flow catalogue and downstream worked-case page; the audit page repeats the wellpath offline registry entry. These are routing/method context rather than independent survey qualification. [docs/registry/workflows.yaml:L996-L1003] [docs/migration/legacy-engineering-flow-catalog.md:L64-L65] [docs/domains/well/pump-down/worked-base-case.md:L164-L173] [docs/audits/workflow-failure-evidence-coverage.html:L149-L149]

Generated function-audit JSON also mentions legacy survey and parent-package discovery rows; these inventory snapshots are discovery metadata, not method references. The parent pack explicitly excludes this package's internals. [docs/registry/agent-pack-audit/production-nde-functions-1.json:L9547-L9572] [docs/registry/agent-pack-audit/production-nde-functions-3.json:L7348-L7360] [src/digitalmodel/well/docs/agent-pack.md:L18-L20]

| Open item | Assigned role | Tracking / required resolution |
| --- | --- | --- |
| Initial-MD reference, single-station applicability, boolean conversion, antiparallel tangent policy and finite-output limits; S08/S11/S12/S16/S20/S26/S27/S28 | Engineering calculations maintainer | issue pending (1); define policy and direct negative tests |
| Retained inputs, normalized DLS units, overwrite protection, portable paths and direct output-helper tests; S02/S13/S18/S22/S23/S24/S25 | Engineering calculations maintainer | issue pending (2); retain evidence and define replacement behavior |
| Independent worked benchmark, general three-dimensional accuracy envelope, method provenance and citation-sidecar applicability; the one UNVERIFIED claim in section 3 | Engineering calculations quality manager | issue pending (3); establish a public reference and expected-value basis before validated classification |
| Missing implementation-map family and legacy RF/DLS qualification; generated discovery inventories are not qualification evidence | Engineering calculations maintainer | issue pending (4); add in-package rows and classify related implementations |

All section-10 items use numbered issue drafts for conversion to repository issues; no private values or private sidecar entries are introduced by this pack.
