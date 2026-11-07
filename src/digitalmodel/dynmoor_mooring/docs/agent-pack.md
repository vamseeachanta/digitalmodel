# DYNMOOR mooring workflow knowledge pack

| Header | Value |
|---|---|
| Module / repository | `dynmoor_mooring` / `digitalmodel` |
| Package / scope | `src/digitalmodel/dynmoor_mooring/`; the physics package is a dependency with separate scope |
| Pack revision / status | 0 / DRAFT |
| Code revision | `21942e8cfc7dfb985c5bfa693245df7d197cf76c` |
| Analysis family | Engineering calculations; EC; AQG r0, Python calculations |
| Quality roles | Engineering calculations quality manager; deputy engineering calculations quality manager |

## 1. Purpose and scope

`router` builds line tables and pretension summaries, evaluates supplied static offsets, and optionally writes fender, wind and passing-ship results to CSV. [src/digitalmodel/dynmoor_mooring/workflow.py:L113-L121] [src/digitalmodel/dynmoor_mooring/workflow.py:L157-L254]
The workflow delegates its physics to `digitalmodel.subsea.mooring_analysis.dynmoor`; its public package exports `router`. [src/digitalmodel/dynmoor_mooring/workflow.py:L79-L94] [src/digitalmodel/dynmoor_mooring/__init__.py:L1-L8]
The registry records development maturity and points to two test files; development is routing metadata rather than an engineering qualification. [docs/registry/module-routing.yaml:L267-L272]
A non-null `time_domain` setting raises `NotImplementedError`, including an empty mapping or `False`; a null setting passes that guard. [src/digitalmodel/dynmoor_mooring/workflow.py:L102-L110]
The workflow does not write a solver deck: its output branches write CSV files and annotate the returned configuration. [src/digitalmodel/dynmoor_mooring/workflow.py:L240-L272]

## 2. Inputs and outputs

| Public symbol | Inputs | Output | Evidence |
|---|---|---|---|
| `router` | Configuration dictionary | Mutated configuration and CSV exports | [src/digitalmodel/dynmoor_mooring/workflow.py:L101-L272] |

Input dictionary entries are parsed by the following functions; ranges below describe implemented guards, not approved physical operating limits. [src/digitalmodel/dynmoor_mooring/workflow.py:L278-L382]

| Input | Unit | Implemented range / default | Source and parser |
|---|---|---|---|
| `line_types`, `lines` | entries | Non-empty lists, required | Configuration; `_req_list`. [src/digitalmodel/dynmoor_mooring/workflow.py:L113-L114] [src/digitalmodel/dynmoor_mooring/workflow.py:L278-L282] |
| `segments` | entries | 1–3; padded with empty segments to 3 | `_line_type`; anchor-end segment first. [src/digitalmodel/dynmoor_mooring/workflow.py:L304-L332] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L228-L236] |
| Segment material; length; air weight | label; ft; lb/ft | Defaults `none`; 0; 0; no parser range guard | `_segment`. [src/digitalmodel/dynmoor_mooring/workflow.py:L292-L301] |
| Segment area; modulus; breaking strength; nominal size | in²; ksi; kips; in | Each defaults 0; no parser range guard | `_segment`. [src/digitalmodel/dynmoor_mooring/workflow.py:L292-L301] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L189-L195] |
| Anchor-to-fairlead vertical distance | ft | Required; converted to float; no positive-range guard here | `_line_type`. [src/digitalmodel/dynmoor_mooring/workflow.py:L324-L334] |
| Pile height; bottom slope; friction | ft; deg; % | Each defaults 0 | `_line_type`. [src/digitalmodel/dynmoor_mooring/workflow.py:L335-L337] |
| Anchor weight; mud/sand efficiencies | kips; dimensionless | Each defaults 0 | `_line_type`. [src/digitalmodel/dynmoor_mooring/workflow.py:L323-L340] |
| Buoy buoyancy; clump weight | kips | Each defaults 0 | `_line_type`. [src/digitalmodel/dynmoor_mooring/workflow.py:L341-L342] |
| Nonlinear spring length; strength; load/stretch points | ft; kips; % | Optional block; all 6 fields required when non-empty | `_line_type`. [src/digitalmodel/dynmoor_mooring/workflow.py:L312-L322] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L212-L224] |
| Line type; fairlead x/y; angle; pretension | index; ft; deg; % of breaking strength | Type defaults 1, constrained to 1..number of types; other fields required | `_line`. [src/digitalmodel/dynmoor_mooring/workflow.py:L346-L359] |
| Vessel draft; beam; displacement | ft; ft; kips | Each defaults 0 | `router`; draft feeds wind model. [src/digitalmodel/dynmoor_mooring/workflow.py:L111-L112] [src/digitalmodel/dynmoor_mooring/workflow.py:L163-L168] [src/digitalmodel/dynmoor_mooring/workflow.py:L214-L215] |
| Vessel length and water depth | ft | Required only for non-empty passing block | `router` / `_req_float`. [src/digitalmodel/dynmoor_mooring/workflow.py:L207-L221] |
| Offsets surge/sway/yaw | ft; ft; deg | Absent/null list gives no evaluations; each coordinate defaults 0 | `router`. [src/digitalmodel/dynmoor_mooring/workflow.py:L169-L177] |
| Fender station/gap; curve arrays; side | ft; ft; ft/kips; dimensionless | Station/gap 0; exactly 4 points each; side 1 | `_fender`, `router`. [src/digitalmodel/dynmoor_mooring/workflow.py:L158-L160] [src/digitalmodel/dynmoor_mooring/workflow.py:L362-L372] |
| Wind speed/direction; frontal/lateral areas | knots; deg; lists | Speed/direction required for evaluated wind; absent area lists empty | `router`. [src/digitalmodel/dynmoor_mooring/workflow.py:L161-L168] [src/digitalmodel/dynmoor_mooring/workflow.py:L192-L198] |
| Wind area width/height/centre; coefficient/lever | ft; ft; ft above keel; dimensionless/ft | First 3 required; coefficient 1; lever 0 | `_wind_area`. [src/digitalmodel/dynmoor_mooring/workflow.py:L375-L382] |
| Passing mode; steps; start; direction | label; count; count; dimensionless | Defaults parallel; 300; 1; 1 | `router`. [src/digitalmodel/dynmoor_mooring/workflow.py:L204-L211] |
| Passing speed, length, separation | knots; ft; ft | Required; upstream sweep applies separation guards | `router`. [src/digitalmodel/dynmoor_mooring/workflow.py:L212-L221] [tests/subsea/mooring_analysis/test_dynmoor.py:L781-L789] |
| Passing beam/draft/displacement; midship areas; current speed/direction | ft/ft/kips; ft²; knots/deg | Each defaults 0 | `router`. [src/digitalmodel/dynmoor_mooring/workflow.py:L219-L227] |
| Output directory; input stem | path; label | `results`, relative to config directory then current directory; stem from input path then basename | Output helpers. [src/digitalmodel/dynmoor_mooring/workflow.py:L388-L406] |

Coordinates use fairlead x forward of midship and y to starboard of centreline; directions are clockwise from bow, while restoring-force yaw is in radians after workflow conversion from degrees. [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L273-L280] [src/digitalmodel/dynmoor_mooring/workflow.py:L169-L173]
Returned restoring forces use body-frame positive forward surge and positive starboard sway; yaw moment follows the implemented cross product and frame rotation. [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L901-L910] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L951-L961]
The workflow has no separate phase convention or harmonic output: it evaluates static offsets and passes through force-history samples. [src/digitalmodel/dynmoor_mooring/workflow.py:L169-L202] [src/digitalmodel/dynmoor_mooring/workflow.py:L229-L238]

| Output | Unit / convention | Contents and destination |
|---|---|---|
| Line tables CSV | ft; kips; counts | One row per table sample; tension/pull divide lb by 1000; suspended length is total minus grounded length. [src/digitalmodel/dynmoor_mooring/workflow.py:L123-L137] |
| Lines CSV | kips; ft; deg; % | Pretension, anchor coordinates, fairlead angle and holding power per input line. [src/digitalmodel/dynmoor_mooring/workflow.py:L139-L155] |
| Offsets CSV | ft; deg; kips; kip-ft | Conditional; restoring forces, per-line tension, optional fender/wind columns; these columns remain separate. [src/digitalmodel/dynmoor_mooring/workflow.py:L169-L202] [src/digitalmodel/dynmoor_mooring/workflow.py:L247-L250] |
| Passing CSV | step; x/L dimensionless; kips; kip-ft | Conditional sweep samples with lb-to-kips conversion. [src/digitalmodel/dynmoor_mooring/workflow.py:L229-L238] [src/digitalmodel/dynmoor_mooring/workflow.py:L251-L254] |
| Returned dictionary | mixed metadata | Mutates input: replaces `lines` with summaries; adds method, evaluated offsets, absolute peaks or null, paths, and false time-domain flag. [src/digitalmodel/dynmoor_mooring/workflow.py:L256-L272] |

`_write_csv` refuses empty row sets, creates parent directories and uses the first row's keys as the header; it does not specify an encoding on `open`. [src/digitalmodel/dynmoor_mooring/workflow.py:L409-L416]
`_display_path` returns a repository-relative path when possible and otherwise an absolute path; do not copy the latter into public evidence. [src/digitalmodel/dynmoor_mooring/workflow.py:L419-L424]

## 3. Method and governing standards

| Method / operation | Implementing definition and reference basis |
|---|---|
| Line tables and pretension | `router` delegates to `build_line_table` and `solve_pretension`; the former selects CATNRY for second-segment length ≥5 ft, FULCAT for positive pile height, else CATINB. [src/digitalmodel/dynmoor_mooring/workflow.py:L116-L120] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L689-L700] |
| Pretension and anchor placement | Upstream `solve_pretension` uses pretension percent × governing breaking strength in kips ×10 to obtain lb, interpolates distance, then uses line angle to locate anchors. [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L833-L884] |
| Restoring force and yaw | `router` converts yaw degrees to radians and calls `restoring_force`, which sums line components and moments before rotating force components into the body frame. [src/digitalmodel/dynmoor_mooring/workflow.py:L169-L180] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L935-L961] |
| Fender and wind | Calls `fender_forces`, `build_wind_model`, `static_wind_force`; the workflow serialises their separate results. [src/digitalmodel/dynmoor_mooring/workflow.py:L163-L168] [src/digitalmodel/dynmoor_mooring/workflow.py:L188-L201] |
| Passing sweep and peaks | Calls `passing_ship_sweep`, serialises samples, then takes the maximum absolute value of each component. [src/digitalmodel/dynmoor_mooring/workflow.py:L204-L238] [src/digitalmodel/dynmoor_mooring/workflow.py:L256-L262] |
| Numerical output conversion | lb→kips and lb-ft→kip-ft divide by 1000; yaw input degrees→radians multiplies by π/180. [src/digitalmodel/dynmoor_mooring/workflow.py:L131-L135] [src/digitalmodel/dynmoor_mooring/workflow.py:L170-L180] |

The package contains configuration, delegation and CSV logic rather than a standards-derived design criterion; neither package file names a governing standard or emits a `Citation` sidecar. [src/digitalmodel/dynmoor_mooring/workflow.py:L1-L71] [src/digitalmodel/dynmoor_mooring/workflow.py:L101-L424] [src/digitalmodel/dynmoor_mooring/__init__.py:L1-L8]
The upstream physics documentation names legacy empirical and numerical methods; that documentation is an implementation reference, not an independently checked publication or a licensed-standard qualification. [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L14-L62]
UNVERIFIED: Publication editions, validity envelopes and independent benchmark provenance for the delegated empirical methods have not been established; no published-method validation is claimed (issue pending (5)).

## 4. Validated worked examples and regression examples

The independent check below is limited to pairwise force and moment cancellation for the symmetric, equal-line, zero-offset fixture; it does not validate the general catenary or empirical force models. Opposite line vectors cancel and mirrored fairlead moments cancel. [tests/subsea/mooring_analysis/test_dynmoor.py:L82-L95] [tests/subsea/mooring_analysis/test_dynmoor.py:L625-L638]
Legacy-binary goldens and transliterations are retained as regression evidence because their benchmark provenance is described in test comments without a separately qualified dataset revision here. [tests/subsea/mooring_analysis/test_dynmoor.py:L1-L38]

| ID / kind | Source / input | Expected result and tolerance | Existing test node ID | Offline result at code pin |
|---|---|---|---|---|
| V1 validated, limited closed form | Pairwise vector cancellation; `CASE_A_TYPE` and `CASE_A_LINES` | Surge and sway 0 lb, absolute tolerance 1e-6 lb; yaw 0 lb-ft, absolute tolerance 1e-4 lb-ft; assertion tolerances from test | `tests/subsea/mooring_analysis/test_dynmoor.py::TestTablesAndStatics::test_symmetric_pattern_equilibrium` | PASS before/after. [tests/subsea/mooring_analysis/test_dynmoor.py:L625-L641] |
| R1 regression | `_base_config(tmp_path)`, synthetic fixture; legacy-output expectations | Pretension 75.0 kips ±0.05 kips; anchor distance 989.6 ft ±0.06 ft; angle 21.5° ±0.01°; 50 table rows and 2 offset rows | `tests/subsea/mooring_analysis/test_dynmoor_workflow.py::test_router_static_outputs` | PASS before/after. [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L69-L92] |
| R2 regression | `_base_config(tmp_path)` plus parallel passing block | 237 samples; peak sway 37.4 kips ±0.5 kips; peak yaw 3400.0 kip-ft ±5.0%; legacy-output rounding basis in test comments | `tests/subsea/mooring_analysis/test_dynmoor_workflow.py::test_router_passing_ship` | PASS before/after. [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L95-L125] |

Run record: both test files, 40 collected, 36 passed, 0 failed, 4 skipped existing time-domain placeholders, 0 NOT_RUN; before and after use `PYTHONPATH=src uv run --no-sources python -m pytest <both files> -q -rA -p no:cacheprovider --basetemp=<scratch>`, with the repository's unchanged addopts, against the header's code pin and uncommitted documentation changes. Test definitions and the four skip reasons are pinned here. [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L69-L151] [tests/subsea/mooring_analysis/test_dynmoor.py:L221-L299] [tests/subsea/mooring_analysis/test_dynmoor.py:L531-L820]

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations, and section 9 for G5; these are calculation gates, so no target-program load or solver run is required. (AQG r0, sections 9–10, gate titles G0–G5.)

| Gate | Applicability | Module-specific executable check / numeric criterion | Evidence |
|---|---|---|---|
| G0 | Applicable | C0: `test_router_validation_errors` rejects missing line-type list and invalid type index: 2 exceptions, 0 accepted invalid cases (count); R1 checks the kips conversion and row counts | Pytest result plus line-table CSV checks. [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L77-L92] [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L135-L144] |
| G1 | Applicable | C1: run both module test files; require 0 failed tests (count), all 36 implemented cases passed (count), exactly 4 documented placeholders skipped (count); import `router` via its existing tests | Before/after pytest summaries; source-only engine routing assertion. [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L8-L8] [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L147-L151] [tests/subsea/mooring_analysis/test_dynmoor.py:L803-L820] |
| G2 | Applicable | C2: V1 / `test_symmetric_pattern_equilibrium`; absolute surge and sway ≤1e-6 lb, yaw ≤1e-4 lb-ft at zero offset; positive 10 ft surge gives surge <−1000 lb | Existing symmetry assertions; closed-form cancellation, limited configuration. [tests/subsea/mooring_analysis/test_dynmoor.py:L625-L641] |
| G3 | NOT_APPLICABLE | Python-calculation family has no short-dynamics gate; this wrapper rejects time-domain requests, 0 supported transient workflows (count) | Rejection test, not a dynamic validation. [src/digitalmodel/dynmoor_mooring/workflow.py:L102-L110] [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L128-L132] |
| G4 | NOT_APPLICABLE | No diffraction mesh or body model is emitted; 0 mesh outputs (count) | CSV output branches. [src/digitalmodel/dynmoor_mooring/workflow.py:L240-L254] |
| G5 | Applicable | C5: recompute SHA-256 of both test input source files at the code pin and preserve their test statuses beside the pin; require 40 recorded cases, 0 missing statuses (count), 0 changed implementation files (count) | Reproducible evidence record; do not treat CSV files alone as a generated QA appendix. [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L11-L61] [tests/subsea/mooring_analysis/test_dynmoor.py:L82-L108] [src/digitalmodel/dynmoor_mooring/workflow.py:L240-L272] |

Checks defined: G0 1, G1 1, G2 1, G3 0, G4 0, G5 1; R1/R2 are the regression examples reused by C0/C1. C5 is a pack-level evidence check, not an assertion that the workflow generates fingerprints or signed gate records; adding those remains issue pending (3). [src/digitalmodel/dynmoor_mooring/workflow.py:L263-L272]
Execute C5 by reading both pinned files with `git show`, hashing their bytes, parsing the two pytest logs, comparing sorted node/status pairs, and asserting 36 PASS plus 4 SKIP with zero differences; no solver package is required. [tests/subsea/mooring_analysis/test_dynmoor.py:L803-L820] [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L1-L151]

## 6. Known failure modes and pitfalls

Silent-failure sweep scope is every Python file below the header package; physics dependency internals remain outside this package sweep. [src/digitalmodel/dynmoor_mooring/__init__.py:L1-L8] [src/digitalmodel/dynmoor_mooring/workflow.py:L79-L94]
The sweep uses the following exact commands, followed by AST enumeration of conversions, defaults and unit conversions; each distinct source line is recorded below, including benign branches so that exclusions are explicit. [src/digitalmodel/dynmoor_mooring/workflow.py:L101-L424]

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|1970|0\.05|= 0(\.0)?\b|float\(|int\(|\.get\(' src/digitalmodel/dynmoor_mooring --glob '*.py'
rg -n 'except|continue|return|1000|180|empty|cache|or \[\]|or \{\}' src/digitalmodel/dynmoor_mooring --glob '*.py'
```

Inventory rows are potential silent defaults, assumptions or fallback sites, not a claim that every default is defective; the parser and serializer statements below are the supporting evidence. [src/digitalmodel/dynmoor_mooring/workflow.py:L101-L424]

sweep: 108 found

| Site | Operation / symptom | Detection and disposition | Evidence at pin |
|---|---|---|---|
| S001 | `cfg.get('dynmoor_mooring')` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L102-L102] |
| S002 | `settings.get('time_domain')` — Null time-domain setting bypasses rejection; non-null is rejected | C1 rejection test for non-null; no detection check exists for null semantics; issue pending (4) | [src/digitalmodel/dynmoor_mooring/workflow.py:L103-L103] |
| S003 | `settings.get('vessel')` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L111-L111] |
| S004 | `float(vessel.get('draft_ft', 0.0)); vessel.get('draft_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L112-L112] |
| S005 | `table_rows = []` — Benign result accumulator, optional absence or padding; not an error return | C1; intentional control flow | [src/digitalmodel/dynmoor_mooring/workflow.py:L123-L123] |
| S006 | `table.h[row] / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L132-L132] |
| S007 | `table.t[row] / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L133-L133] |
| S008 | `table.p[row] / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L135-L135] |
| S009 | `line_rows = []` — Benign result accumulator, optional absence or padding; not an error return | C1; intentional control flow | [src/digitalmodel/dynmoor_mooring/workflow.py:L139-L139] |
| S010 | `sol.pretension_lb / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L146-L146] |
| S011 | `offset_rows = []` — Benign result accumulator, optional absence or padding; not an error return | C1; intentional control flow | [src/digitalmodel/dynmoor_mooring/workflow.py:L157-L157] |
| S012 | `settings.get('fenders')` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L158-L158] |
| S013 | `fender_cfg.get('curves', [])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L159-L159] |
| S014 | `int(fender_cfg.get('side', 1)); fender_cfg.get('side', 1)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L160-L160] |
| S015 | `settings.get('wind')` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L161-L161] |
| S016 | `wind_model = None` — Benign result accumulator, optional absence or padding; not an error return | C1; intentional control flow | [src/digitalmodel/dynmoor_mooring/workflow.py:L162-L162] |
| S017 | `wind_cfg.get('frontal_areas', [])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L165-L165] |
| S018 | `wind_cfg.get('lateral_areas', [])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L166-L166] |
| S019 | `settings.get('offsets', [])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L169-L169] |
| S020 | `float(item.get('surge_ft', 0.0)); item.get('surge_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L170-L170] |
| S021 | `float(item.get('sway_ft', 0.0)); item.get('sway_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L171-L171] |
| S022 | `float(item.get('yaw_deg', 0.0)) * math.pi / 180.0; float(item.get('yaw_deg', 0.0)); item.get('yaw_deg', 0.0)` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L172-L172] |
| S023 | `float(item.get('yaw_deg', 0.0)); item.get('yaw_deg', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L177-L177] |
| S024 | `result.surge_lb / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L178-L178] |
| S025 | `result.sway_lb / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L179-L179] |
| S026 | `result.yaw_lb_ft / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L180-L180] |
| S027 | `tension / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L186-L186] |
| S028 | `horizontal / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L187-L187] |
| S029 | `fl / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L190-L190] |
| S030 | `fmz / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L191-L191] |
| S031 | `float(wind_cfg['speed_knots'])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L195-L195] |
| S032 | `float(wind_cfg['direction_deg'])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L196-L196] |
| S033 | `fwd[0] / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L199-L199] |
| S034 | `fwd[1] / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L200-L200] |
| S035 | `fwd[2] / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L201-L201] |
| S036 | `passing_rows = []` — Benign result accumulator, optional absence or padding; not an error return | C1; intentional control flow | [src/digitalmodel/dynmoor_mooring/workflow.py:L204-L204] |
| S037 | `settings.get('passing_ship')` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L205-L205] |
| S038 | `passing_cfg.get('mode', 'parallel')` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L208-L208] |
| S039 | `int(passing_cfg.get('nsteps', 300)); passing_cfg.get('nsteps', 300)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L209-L209] |
| S040 | `int(passing_cfg.get('start_step', 1)); passing_cfg.get('start_step', 1)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L210-L210] |
| S041 | `float(passing_cfg.get('direction', 1.0)); passing_cfg.get('direction', 1.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L211-L211] |
| S042 | `float(vessel.get('beam_ft', 0.0)); vessel.get('beam_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L214-L214] |
| S043 | `float(vessel.get('displacement_kips', 0.0)); vessel.get('displacement_kips', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L215-L215] |
| S044 | `float(passing_cfg.get('passing_beam_ft', 0.0)); passing_cfg.get('passing_beam_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L219-L219] |
| S045 | `float(passing_cfg.get('current_knots', 0.0)); passing_cfg.get('current_knots', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L222-L222] |
| S046 | `float(passing_cfg.get('current_direction_deg', 0.0)); passing_cfg.get('current_direction_deg', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L223-L223] |
| S047 | `float(passing_cfg.get('moored_midship_area_ft2', 0.0)); passing_cfg.get('moored_midship_area_ft2', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L224-L224] |
| S048 | `float(passing_cfg.get('passing_midship_area_ft2', 0.0)); passing_cfg.get('passing_midship_area_ft2', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L225-L225] |
| S049 | `float(passing_cfg.get('passing_draft_ft', 0.0)); passing_cfg.get('passing_draft_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L226-L226] |
| S050 | `float(passing_cfg.get('passing_displacement_kips', 0.0)); passing_cfg.get('passing_displacement_kips', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L227-L227] |
| S051 | `s.surge_lb / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L233-L233] |
| S052 | `s.sway_lb / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L234-L234] |
| S053 | `s.yaw_lb_ft / 1000.0` — Unit conversion assumes legacy English input and lb-valued dependency result | C0/R1 for table/static route; no detection check exists for optional wind/fender conversion; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L235-L235] |
| S054 | `outputs: dict[str, str] = {}` — Benign result accumulator, optional absence or padding; not an error return | C1; intentional control flow | [src/digitalmodel/dynmoor_mooring/workflow.py:L240-L240] |
| S055 | `peak = None` — Benign result accumulator, optional absence or padding; not an error return | C1; intentional control flow | [src/digitalmodel/dynmoor_mooring/workflow.py:L256-L256] |
| S056 | `settings.get(name)` — List lookup or type coercion; guards cover missing list or out-of-range type | C0; no detection check exists for fractional type truncation; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L279-L279] |
| S057 | `source.get(name)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L286-L286] |
| S058 | `float(value)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L289-L289] |
| S059 | `item.get('material', 'none')` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L294-L294] |
| S060 | `float(item.get('length_ft', 0.0)); item.get('length_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L295-L295] |
| S061 | `float(item.get('weight_air_lb_ft', 0.0)); item.get('weight_air_lb_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L296-L296] |
| S062 | `float(item.get('area_in2', 0.0)); item.get('area_in2', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L297-L297] |
| S063 | `float(item.get('modulus_ksi', 0.0)); item.get('modulus_ksi', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L298-L298] |
| S064 | `float(item.get('breaking_strength_kips', 0.0)); item.get('breaking_strength_kips', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L299-L299] |
| S065 | `float(item.get('size_in', 0.0)); item.get('size_in', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L300-L300] |
| S066 | `item.get('segments')` — List lookup or type coercion; guards cover missing list or out-of-range type | C0; no detection check exists for fractional type truncation; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L306-L306] |
| S067 | `while len(segs) < 3:` — Benign result accumulator, optional absence or padding; not an error return | C1; intentional control flow | [src/digitalmodel/dynmoor_mooring/workflow.py:L310-L310] |
| S068 | `segs.append(DynmoorSegment())` — Benign result accumulator, optional absence or padding; not an error return | C1; intentional control flow | [src/digitalmodel/dynmoor_mooring/workflow.py:L311-L311] |
| S069 | `spring = None` — Benign result accumulator, optional absence or padding; not an error return | C1; intentional control flow | [src/digitalmodel/dynmoor_mooring/workflow.py:L312-L312] |
| S070 | `item.get('nonlinear_spring')` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L313-L313] |
| S071 | `float(sp['length_ft'])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L316-L316] |
| S072 | `float(sp['breaking_strength_kips'])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L317-L317] |
| S073 | `float(sp['ep1'])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L318-L318] |
| S074 | `float(sp['p1'])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L319-L319] |
| S075 | `float(sp['ep2'])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L320-L320] |
| S076 | `float(sp['p2'])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L321-L321] |
| S077 | `item.get('anchor')` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L323-L323] |
| S078 | `item.get('vertical_distance_anchor_to_fairlead_ft')` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L324-L324] |
| S079 | `float(depth)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L334-L334] |
| S080 | `float(item.get('pile_height_ft', 0.0)); item.get('pile_height_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L335-L335] |
| S081 | `float(item.get('bottom_slope_deg', 0.0)); item.get('bottom_slope_deg', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L336-L336] |
| S082 | `float(item.get('bottom_friction_percent', 0.0)); item.get('bottom_friction_percent', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L337-L337] |
| S083 | `float(anchor.get('weight_kips', 0.0)); anchor.get('weight_kips', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L338-L338] |
| S084 | `float(anchor.get('mud_efficiency', 0.0)); anchor.get('mud_efficiency', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L339-L339] |
| S085 | `float(anchor.get('sand_efficiency', 0.0)); anchor.get('sand_efficiency', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L340-L340] |
| S086 | `float(item.get('buoy_buoyancy_kips', 0.0)); item.get('buoy_buoyancy_kips', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L341-L341] |
| S087 | `float(item.get('clump_weight_kips', 0.0)); item.get('clump_weight_kips', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L342-L342] |
| S088 | `int(item.get('type', 1)); item.get('type', 1)` — List lookup or type coercion; guards cover missing list or out-of-range type | C0; no detection check exists for fractional type truncation; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L348-L348] |
| S089 | `float(v)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L363-L363] |
| S090 | `float(v)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L364-L364] |
| S091 | `float(item.get('station_x_ft', 0.0)); item.get('station_x_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L368-L368] |
| S092 | `float(item.get('initial_gap_ft', 0.0)); item.get('initial_gap_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L369-L369] |
| S093 | `float(item['width_ft'])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L377-L377] |
| S094 | `float(item['height_ft'])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L378-L378] |
| S095 | `float(item['vca_above_keel_ft'])` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L379-L379] |
| S096 | `float(item.get('shape_coefficient', 1.0)); item.get('shape_coefficient', 1.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L380-L380] |
| S097 | `float(item.get('lever_arm_ft', 0.0)); item.get('lever_arm_ft', 0.0)` — Missing value/default, optional-block omission or numeric conversion can suppress an intended load or accept nonfinite/truncated input | no detection check exists for this site; issue pending (1) | [src/digitalmodel/dynmoor_mooring/workflow.py:L381-L381] |
| S098 | `settings.get('output_dir', 'results')` — Location fallback or external-path display can depend on execution context | no detection check exists; issue pending (2) | [src/digitalmodel/dynmoor_mooring/workflow.py:L389-L389] |
| S099 | `def _config_dir(cfg: dict) -> Path:` — Location fallback or external-path display can depend on execution context | no detection check exists; issue pending (2) | [src/digitalmodel/dynmoor_mooring/workflow.py:L395-L395] |
| S100 | `cfg.get('_config_dir_path')` — Location fallback or external-path display can depend on execution context | no detection check exists; issue pending (2) | [src/digitalmodel/dynmoor_mooring/workflow.py:L396-L396] |
| S101 | `cfg.get('_config_file_path')` — Location fallback or external-path display can depend on execution context | no detection check exists; issue pending (2) | [src/digitalmodel/dynmoor_mooring/workflow.py:L398-L398] |
| S102 | `return Path.cwd()` — Location fallback or external-path display can depend on execution context | no detection check exists; issue pending (2) | [src/digitalmodel/dynmoor_mooring/workflow.py:L400-L400] |
| S103 | `def _input_stem(cfg: dict) -> str:` — Location fallback or external-path display can depend on execution context | no detection check exists; issue pending (2) | [src/digitalmodel/dynmoor_mooring/workflow.py:L403-L403] |
| S104 | `cfg.get('_config_file_path')` — Location fallback or external-path display can depend on execution context | no detection check exists; issue pending (2) | [src/digitalmodel/dynmoor_mooring/workflow.py:L404-L404] |
| S105 | `cfg.get('basename', 'dynmoor_mooring')` — Location fallback or external-path display can depend on execution context | no detection check exists; issue pending (2) | [src/digitalmodel/dynmoor_mooring/workflow.py:L406-L406] |
| S106 | `if not rows:` — Empty export is rejected explicitly | no detection check exists for direct helper rejection; issue pending (3) | [src/digitalmodel/dynmoor_mooring/workflow.py:L410-L410] |
| S107 | `except ValueError:` — Location fallback or external-path display can depend on execution context | no detection check exists; issue pending (2) | [src/digitalmodel/dynmoor_mooring/workflow.py:L423-L423] |
| S108 | `return str(resolved)` — Location fallback or external-path display can depend on execution context | no detection check exists; issue pending (2) | [src/digitalmodel/dynmoor_mooring/workflow.py:L424-L424] |

The sole caught exception is `_display_path`'s `ValueError`, which returns the resolved absolute path; it is not a calculation fallback. No `continue`, `return None`, empty-list/dict error return, 1970 constant, 5%-style fallback or cache write occurs in these two source files. [src/digitalmodel/dynmoor_mooring/workflow.py:L101-L424] [src/digitalmodel/dynmoor_mooring/__init__.py:L1-L8]

| Other symptom | Cause / limit | Detection | Tracking |
|---|---|---|---|
| Repeating `router` on its returned dictionary can lose required input fields | Input `lines` is overwritten by summary rows | no detection check exists; issue pending (2) | [src/digitalmodel/dynmoor_mooring/workflow.py:L263-L270] [src/digitalmodel/dynmoor_mooring/workflow.py:L346-L358] |
| Unsupported transient response | Time-domain implementation absent; non-null config rejected | C1 rejection test; 4 existing placeholders skipped; issue pending (4) | [src/digitalmodel/dynmoor_mooring/workflow.py:L102-L110] [tests/subsea/mooring_analysis/test_dynmoor.py:L803-L820] |
| Broader empirical validation can be overstated | Binary/transliteration agreement shares legacy assumptions | C2 detects symmetry only; no detection check exists for general physical validity; issue pending (5) | [tests/subsea/mooring_analysis/test_dynmoor.py:L1-L38] |
| Optional outputs and provenance lack workflow assertions | Existing five wrapper tests omit fender/wind branches and generated gate manifests | no detection check exists; issue pending (3) | [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L69-L151] [src/digitalmodel/dynmoor_mooring/workflow.py:L188-L201] [src/digitalmodel/dynmoor_mooring/workflow.py:L263-L272] |

No incident issue is named for this module in the acceptance scope. The issue search found a historical unit-name contract fix, #1638, rather than a confirmed defect of this wrapper; it is not evidence of a current numerical defect. The unit-bearing field names are visible in the output construction. [src/digitalmodel/dynmoor_mooring/workflow.py:L131-L135] [src/digitalmodel/dynmoor_mooring/workflow.py:L178-L201]

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change / shared record |
|---|---|---|---|---|
| 2026-11-06 | Initial pack | Preserve the distinction between wrapper regression, legacy arithmetic agreement and independently derived symmetry | [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L75-L92] [tests/subsea/mooring_analysis/test_dynmoor.py:L1-L38] [tests/subsea/mooring_analysis/test_dynmoor.py:L625-L638] | Sections 3–6; shared record not assigned |

## 8. Ownership

Review assignment: engineering calculations quality manager; deputy: deputy engineering calculations quality manager; producing role: module documentation maintainer. Proposed tolerance changes and ambiguous applicability go to the family quality manager; cross-family decisions go to the maintainers. Clearance requires an independent second-provider review; this pack remains DRAFT. (Module agent lifecycle, revision 0, sections 3–4; AQG r0, section 11.)

## 9. Dependencies and other implementations

The engine dispatches `basename: dynmoor_mooring` to this router; the router delegates calculations to `subsea.mooring_analysis.dynmoor`, uses standard-library CSV/path/math facilities, and returns filenames and summaries for downstream consumers. [src/digitalmodel/engine.py:L556-L561] [src/digitalmodel/dynmoor_mooring/workflow.py:L75-L96] [src/digitalmodel/dynmoor_mooring/workflow.py:L240-L272]
No solver version or external dataset is loaded by the wrapper; the `dynmoor_legacy_static_v610` string is an output method identifier rather than a solver execution record. [src/digitalmodel/dynmoor_mooring/workflow.py:L101-L272]

### Implementation-map inventory and counted-family search

Map ref: `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, embedded data/code revision `270c8759c374b67a7f032953e7fcdb14e0224461`. Map-file citations in this section resolve at that map commit; all other file citations resolve at the header code pin. The package has 1 map family with 1 in-package row, so that family is counted. [docs/registry/implementation-map.yaml:L1-L3]

Counted family `wind-and-current-force-routes`: 1 in-package row; 135 total inventory rows, including wrappers and tests as the map records them. All are inventoried below without endorsing their map classifications. [docs/registry/implementation-map.yaml:L15393-L15400]

Search uses Python `re` case-sensitively, exactly the following patterns; none matches `dynmoor_mooring` under case-insensitive comparison, so none is excluded. [docs/registry/implementation-map.yaml:L15396-L15398]

- `(?:0\.5|1\s*/\s*2).*(?:rho|density).*(?:cd|drag|area)|(?:wind|current|drag|lift)_forces?|calculate_(?:wind|current)|OCIMF|current_profile|wind_height_coefficient`
- `OCIMF|wind.load|current.load|wind.force|current.force|drag.coefficient|current.profile`
- `Cxw|Cyw|CXw|CYw|CXc|CYc|drag_coefficient|lift_coefficient|wind_speed|current_velocity`

| Inventory row | Path | Definition as mapped | Map evidence |
|---|---|---|---|
| 1 | `examples/demos/gtm/demo_03_deepwater_mudmat_installation.py` | `calc_splash_zone` | [docs/registry/implementation-map.yaml:L15400-L15409] |
| 2 | `examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py` | `calc_phase_3_splash` | [docs/registry/implementation-map.yaml:L15410-L15419] |
| 3 | `examples/demos/gtm/results_store_demo05.py` | `(module-level)` | [docs/registry/implementation-map.yaml:L15420-L15428] |
| 4 | `examples/demos/gtm/tests/test_demo_05_results_store.py` | `_good_case` | [docs/registry/implementation-map.yaml:L15429-L15438] |
| 5 | `examples/domains/fpso/fpso_mooring_analysis.py` | `calculate_environmental_forces` | [docs/registry/implementation-map.yaml:L15439-L15448] |
| 6 | `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | [docs/registry/implementation-map.yaml:L15449-L15460] |
| 7 | `examples/domains/ocimf/ocimf_demo.py` | `main` | [docs/registry/implementation-map.yaml:L15461-L15470] |
| 8 | `examples/domains/workflows/01_spread_mooring_8point.py` | `calculate_environmental_forces` | [docs/registry/implementation-map.yaml:L15471-L15480] |
| 9 | `examples/domains/workflows/03_calm_buoy.py` | `analyze_calm_buoy` | [docs/registry/implementation-map.yaml:L15481-L15490] |
| 10 | `examples/domains/workflows/05_jackup_installation.py` | `analyze_jackup_mooring` | [docs/registry/implementation-map.yaml:L15491-L15500] |
| 11 | `examples/domains/workflows/06_floating_wind_turbine.py` | `analyze_floating_wind_mooring` | [docs/registry/implementation-map.yaml:L15501-L15510] |
| 12 | `examples/domains/workflows/07_wave_energy_converter.py` | `analyze_wec_mooring` | [docs/registry/implementation-map.yaml:L15511-L15520] |
| 13 | `examples/domains/workflows/08_aquaculture_mooring.py` | `analyze_aquaculture_mooring` | [docs/registry/implementation-map.yaml:L15521-L15530] |
| 14 | `examples/domains/workflows/09_spm_terminal.py` | `analyze_spm_terminal` | [docs/registry/implementation-map.yaml:L15531-L15540] |
| 15 | `examples/domains/workflows/10_dp_backup_mooring.py` | `analyze_dp_backup_mooring` | [docs/registry/implementation-map.yaml:L15541-L15550] |
| 16 | `scripts/capabilities/build_onepagers.py` | `(module-level)` | [docs/registry/implementation-map.yaml:L15551-L15559] |
| 17 | `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `(module-level)` | [docs/registry/implementation-map.yaml:L15560-L15568] |
| 18 | `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `extract_ocimf_database` | [docs/registry/implementation-map.yaml:L15569-L15578] |
| 19 | `scripts/python/digitalmodel/legacy/generate_integration_charts.py` | `(module-level)` | [docs/registry/implementation-map.yaml:L15579-L15587] |
| 20 | `scripts/python/digitalmodel/legacy/generate_ocimf_report.py` | `main` | [docs/registry/implementation-map.yaml:L15588-L15597] |
| 21 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler._setup_test_data` | [docs/registry/implementation-map.yaml:L15598-L15607] |
| 22 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | [docs/registry/implementation-map.yaml:L15608-L15617] |
| 23 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.run_all` | [docs/registry/implementation-map.yaml:L15618-L15629] |
| 24 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `main` | [docs/registry/implementation-map.yaml:L15630-L15639] |
| 25 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | [docs/registry/implementation-map.yaml:L15640-L15649] |
| 26 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | [docs/registry/implementation-map.yaml:L15650-L15659] |
| 27 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_json_summary` | [docs/registry/implementation-map.yaml:L15660-L15669] |
| 28 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.run_complete_validation` | [docs/registry/implementation-map.yaml:L15670-L15684] |
| 29 | `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `(module-level)` | [docs/registry/implementation-map.yaml:L15685-L15693] |
| 30 | `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `main` | [docs/registry/implementation-map.yaml:L15694-L15703] |
| 31 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `make_polar_overlay` | [docs/registry/implementation-map.yaml:L15704-L15713] |
| 32 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `parse_args` | [docs/registry/implementation-map.yaml:L15714-L15723] |
| 33 | `src/digitalmodel/ansys/pressure_vessel.py` | `PressureVesselGenerator.generate_wind_load` | [docs/registry/implementation-map.yaml:L15724-L15733] |
| 34 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` | `SyntheticRopeDatabaseClient._generate_rope_spec` | [docs/registry/implementation-map.yaml:L15734-L15743] |
| 35 | `src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py` | `main` | [docs/registry/implementation-map.yaml:L15744-L15753] |
| 36 | `src/digitalmodel/drilling_riser/drift_off.py` | `environmental_force_n` | [docs/registry/implementation-map.yaml:L15754-L15763] |
| 37 | `src/digitalmodel/drilling_riser/envelope.py` | `CurrentProfile.drag_load_n_per_m` | [docs/registry/implementation-map.yaml:L15764-L15773] |
| 38 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `as_analysed_sections` | [docs/registry/implementation-map.yaml:L15774-L15783] |
| 39 | `src/digitalmodel/dynmoor_mooring/workflow.py` | `router` | [docs/registry/implementation-map.yaml:L15784-L15793] |
| 40 | `src/digitalmodel/engine.py` | `engine` | [docs/registry/implementation-map.yaml:L15794-L15806] |
| 41 | `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_wind_cmd` | [docs/registry/implementation-map.yaml:L15807-L15816] |
| 42 | `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_current_cmd` | [docs/registry/implementation-map.yaml:L15817-L15826] |
| 43 | `src/digitalmodel/hydrodynamics/cli.py` | `combined_env_cmd` | [docs/registry/implementation-map.yaml:L15827-L15836] |
| 44 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_total_viv_fatigue` | [docs/registry/implementation-map.yaml:L15837-L15846] |
| 45 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L15847-L15856] |
| 46 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_total_viv_fatigue` | [docs/registry/implementation-map.yaml:L15857-L15866] |
| 47 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L15867-L15876] |
| 48 | `src/digitalmodel/infrastructure/utils/ETL_components.py` | `ETL_components.transform_viv_current_reformat_1` | [docs/registry/implementation-map.yaml:L15877-L15886] |
| 49 | `src/digitalmodel/installation/installation_pamphlet/calculations.py` | `compute_splashzone_envelope` | [docs/registry/implementation-map.yaml:L15887-L15896] |
| 50 | `src/digitalmodel/installation/jumper_installation/calculations.py` | `calc_phase_3_splash` | [docs/registry/implementation-map.yaml:L15897-L15906] |
| 51 | `src/digitalmodel/marine_ops/marine_analysis/__main__.py` | `main` | [docs/registry/implementation-map.yaml:L15907-L15916] |
| 52 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | [docs/registry/implementation-map.yaml:L15917-L15926] |
| 53 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | [docs/registry/implementation-map.yaml:L15927-L15936] |
| 54 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `create_sample_database` | [docs/registry/implementation-map.yaml:L15937-L15946] |
| 55 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py` | `OCIMFWorkflow.router` | [docs/registry/implementation-map.yaml:L15947-L15956] |
| 56 | `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `(module-level)` | [docs/registry/implementation-map.yaml:L15957-L15965] |
| 57 | `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `extract_ocimf_database` | [docs/registry/implementation-map.yaml:L15966-L15975] |
| 58 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler._setup_test_data` | [docs/registry/implementation-map.yaml:L15976-L15987] |
| 59 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | [docs/registry/implementation-map.yaml:L15988-L15997] |
| 60 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.run_all` | [docs/registry/implementation-map.yaml:L15998-L16009] |
| 61 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `main` | [docs/registry/implementation-map.yaml:L16010-L16019] |
| 62 | `src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py` | `main` | [docs/registry/implementation-map.yaml:L16020-L16029] |
| 63 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | [docs/registry/implementation-map.yaml:L16030-L16039] |
| 64 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | [docs/registry/implementation-map.yaml:L16040-L16049] |
| 65 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_json_summary` | [docs/registry/implementation-map.yaml:L16050-L16059] |
| 66 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.run_complete_validation` | [docs/registry/implementation-map.yaml:L16060-L16074] |
| 67 | `src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py` | `(module-level)` | [docs/registry/implementation-map.yaml:L16075-L16083] |
| 68 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_output_dir` | [docs/registry/implementation-map.yaml:L16084-L16093] |
| 69 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_cli_output_dir` | [docs/registry/implementation-map.yaml:L16094-L16103] |
| 70 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `load_data` | [docs/registry/implementation-map.yaml:L16104-L16113] |
| 71 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `main` | [docs/registry/implementation-map.yaml:L16114-L16126] |
| 72 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | [docs/registry/implementation-map.yaml:L16127-L16136] |
| 73 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | [docs/registry/implementation-map.yaml:L16137-L16146] |
| 74 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `create_sample_database` | [docs/registry/implementation-map.yaml:L16147-L16156] |
| 75 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._environmental_forces` | [docs/registry/implementation-map.yaml:L16157-L16168] |
| 76 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._ocimf_database_path` | [docs/registry/implementation-map.yaml:L16169-L16178] |
| 77 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `_resolve_arrow_direction_in_body_frame` | [docs/registry/implementation-map.yaml:L16179-L16188] |
| 78 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `polar_force_overlay` | [docs/registry/implementation-map.yaml:L16189-L16198] |
| 79 | `src/digitalmodel/naval_architecture/maneuvering_envelope.py` | `threshold_speed_for_steerage` | [docs/registry/implementation-map.yaml:L16199-L16208] |
| 80 | `src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py` | `run_maneuvering_envelope_sweep` | [docs/registry/implementation-map.yaml:L16209-L16218] |
| 81 | `src/digitalmodel/orcaflex/installation_analysis.py` | `SplashZoneInput.calculate_splash_zone_loads` | [docs/registry/implementation-map.yaml:L16219-L16228] |
| 82 | `src/digitalmodel/orcawave/drift_forces.py` | `compute_wind_current_drift` | [docs/registry/implementation-map.yaml:L16229-L16238] |
| 83 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` | `KnowledgeIntegrator._extract_configurations` | [docs/registry/implementation-map.yaml:L16239-L16248] |
| 84 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder.build` | [docs/registry/implementation-map.yaml:L16249-L16258] |
| 85 | `src/digitalmodel/solvers/orcaflex/reporting/extractors/loads_extractor.py` | `extract_loads` | [docs/registry/implementation-map.yaml:L16259-L16268] |
| 86 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `calculate_dual_chute_drag` | [docs/registry/implementation-map.yaml:L16269-L16280] |
| 87 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `assess_all_load_cases` | [docs/registry/implementation-map.yaml:L16281-L16293] |
| 88 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `simulate_deployment_transient` | [docs/registry/implementation-map.yaml:L16294-L16304] |
| 89 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `export_results_yaml` | [docs/registry/implementation-map.yaml:L16305-L16314] |
| 90 | `src/digitalmodel/structural/parachute/parachute_drag.py` | `calculate_drag_force` | [docs/registry/implementation-map.yaml:L16315-L16324] |
| 91 | `src/digitalmodel/subsea/mooring_analysis/cli.py` | `design_verification` | [docs/registry/implementation-map.yaml:L16325-L16334] |
| 92 | `src/digitalmodel/subsea/mooring_analysis/designer.py` | `MooringDesigner.calculate_environmental_loads` | [docs/registry/implementation-map.yaml:L16335-L16344] |
| 93 | `src/digitalmodel/subsea/mooring_analysis/dynmoor.py` | `build_wind_model.force` | [docs/registry/implementation-map.yaml:L16345-L16354] |
| 94 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `_phase_demand` | [docs/registry/implementation-map.yaml:L16355-L16364] |
| 95 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `hydrodynamic_loads` | [docs/registry/implementation-map.yaml:L16365-L16374] |
| 96 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `drag_force_per_meter` | [docs/registry/implementation-map.yaml:L16375-L16384] |
| 97 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lateral_stability_check` | [docs/registry/implementation-map.yaml:L16385-L16396] |
| 98 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `check_lateral_stability` | [docs/registry/implementation-map.yaml:L16397-L16408] |
| 99 | `src/digitalmodel/subsea/pipeline/integrity_workflows.py` | `run_on_bottom_stability` | [docs/registry/implementation-map.yaml:L16409-L16418] |
| 100 | `src/digitalmodel/subsea/viv_analysis/screening.py` | `VIVScreening.screen_with_current_profile` | [docs/registry/implementation-map.yaml:L16419-L16428] |
| 101 | `src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py` | `viv_analysis` | [docs/registry/implementation-map.yaml:L16429-L16438] |
| 102 | `src/digitalmodel/subsea/viv_analysis/vortex_shedding.py` | `VortexSheddingAnalyzer.analyze_with_profile` | [docs/registry/implementation-map.yaml:L16439-L16448] |
| 103 | `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_mooring_design_template` | [docs/registry/implementation-map.yaml:L16449-L16458] |
| 104 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `MooringSystemDesignWorkflow.create` | [docs/registry/implementation-map.yaml:L16459-L16468] |
| 105 | `tests/contracts/test_force_units_boundary_contract.py` | `(module-level)` | [docs/registry/implementation-map.yaml:L16469-L16477] |
| 106 | `tests/legal/test_published_pages_have_no_internal_paths.py` | `_load_producer` | [docs/registry/implementation-map.yaml:L16478-L16487] |
| 107 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `sample_database_path` | [docs/registry/implementation-map.yaml:L16488-L16497] |
| 108 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `ocimf_db` | [docs/registry/implementation-map.yaml:L16498-L16507] |
| 109 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `env_forces` | [docs/registry/implementation-map.yaml:L16508-L16517] |
| 110 | `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | [docs/registry/implementation-map.yaml:L16518-L16528] |
| 111 | `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py` | `TestOCIMFMooringIntegration.test_combined_environmental_forces` | [docs/registry/implementation-map.yaml:L16529-L16538] |
| 112 | `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `ocimf_database` | [docs/registry/implementation-map.yaml:L16539-L16549] |
| 113 | `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `pytest_benchmark_update_json` | [docs/registry/implementation-map.yaml:L16550-L16559] |
| 114 | `tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py` | `test_refactored_polar_signatures_match_fixture` | [docs/registry/implementation-map.yaml:L16560-L16569] |
| 115 | `tests/solvers/orcaflex/reporting/fixture_helpers.py` | `_build_loads` | [docs/registry/implementation-map.yaml:L16570-L16579] |
| 116 | `tests/structural/parachute/test_chute_assessment.py` | `TestAssessAllLoadCases.test_governing_force_within_1_percent_of_hand_calc` | [docs/registry/implementation-map.yaml:L16580-L16589] |
| 117 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_200_mph` | [docs/registry/implementation-map.yaml:L16590-L16600] |
| 118 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_250_mph` | [docs/registry/implementation-map.yaml:L16601-L16611] |
| 119 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_force_scales_with_v_squared` | [docs/registry/implementation-map.yaml:L16612-L16622] |
| 120 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_result_has_newtons` | [docs/registry/implementation-map.yaml:L16623-L16633] |
| 121 | `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_newtons` | [docs/registry/implementation-map.yaml:L16634-L16644] |
| 122 | `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_lbs` | [docs/registry/implementation-map.yaml:L16645-L16655] |
| 123 | `tests/subsea/catenary_riser/conftest.py` | `catenary_environment` | [docs/registry/implementation-map.yaml:L16656-L16665] |
| 124 | `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py` | `TestLiftForce.test_known_lift_force` | [docs/registry/implementation-map.yaml:L16666-L16675] |
| 125 | `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestDragForce.test_scenario_1` | [docs/registry/implementation-map.yaml:L16676-L16685] |
| 126 | `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestLiftForce.test_scenario_1` | [docs/registry/implementation-map.yaml:L16686-L16695] |
| 127 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_circular_mock` | [docs/registry/implementation-map.yaml:L16696-L16705] |
| 128 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_rectangular_mock` | [docs/registry/implementation-map.yaml:L16706-L16715] |
| 129 | `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_drag_force_per_meter` | [docs/registry/implementation-map.yaml:L16716-L16725] |
| 130 | `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_lift_force_per_meter` | [docs/registry/implementation-map.yaml:L16726-L16735] |
| 131 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | [docs/registry/implementation-map.yaml:L16736-L16747] |
| 132 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `hydrodynamic_force_per_meter` | [docs/registry/implementation-map.yaml:L16748-L16758] |
| 133 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `lift_force_per_meter` | [docs/registry/implementation-map.yaml:L16759-L16769] |
| 134 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lift_force_per_meter` | [docs/registry/implementation-map.yaml:L16770-L16781] |
| 135 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `inertia_force_per_meter` | [docs/registry/implementation-map.yaml:L16782-L16791] |

| Signal hit definition | Hit lines | Classification / second implementation count |
|---|---|---|
| `(module-level)` | 46, 93 | Import references / docstring vocabulary; helper/module documentation; 0 misses [src/digitalmodel/dynmoor_mooring/workflow.py:L46-L93] |
| `router` | 193 | Listed router; delegates to wind model/force functions, then serialises; 0 misses [src/digitalmodel/dynmoor_mooring/workflow.py:L193-L193] |

Counted-family result: 0 further second implementations; A8 inventory includes all 135 map rows and passes the ≤1-miss criterion. The map labels `router` independent, but the code calls upstream wind functions; this classification discrepancy is a map follow-up, not an additional implementation. [src/digitalmodel/dynmoor_mooring/workflow.py:L163-L168] [src/digitalmodel/dynmoor_mooring/workflow.py:L192-L201]

Repository-wide discovery also searches tracked Python text, including `scripts/`, for the package route and the delegated method names; the upstream implementations are outside this package's A8 boundary. The inventory above preserves map rows from scripts, examples and tests. [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L689-L700] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L833-L884] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L913-L961] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L1086-L1127]
Map follow-up: upstream `static_wind_force` computes its own result and is absent from this family's inventory; add/reconcile that outside-package map gap. Add package rows for line tables/pretension/restoring, fenders and passing-force routes before those families count in a later map revision; they currently have no rows in this package. Record the router's delegation classification too (issue pending (6)). [src/digitalmodel/dynmoor_mooring/workflow.py:L79-L94] [src/digitalmodel/dynmoor_mooring/workflow.py:L116-L120] [src/digitalmodel/dynmoor_mooring/workflow.py:L173-L173] [src/digitalmodel/dynmoor_mooring/workflow.py:L188-L207] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L1104-L1118]

## 10. Coverage and open items

Read scope: both package Python files in full; both module test files in full; the DYNMOOR domain documentation and the routing entries below; dependency definitions cited in sections 2–3 and 9. The physics package's other files are excluded from this pack. [src/digitalmodel/dynmoor_mooring/__init__.py:L1-L8] [src/digitalmodel/dynmoor_mooring/workflow.py:L1-L424] [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L1-L151] [tests/subsea/mooring_analysis/test_dynmoor.py:L1-L820]

| Documentation discovery read | Relevance / coverage |
|---|---|
| `docs/domains/mooring/dynmoor-legacy-mooring.md` | Method claims, regression provenance and time-domain gaps; including the usage example, all read; numerical claims restricted to implemented/tested scope. [docs/domains/mooring/dynmoor-legacy-mooring.md:L1-L116] |
| `docs/maps/digitalmodel-operator-map.md` | Module routing row. [docs/maps/digitalmodel-operator-map.md:L86-L86] |
| `docs/registry/module-routing.yaml` | Existing development entry, both test paths. [docs/registry/module-routing.yaml:L267-L272] |
| `docs/registry/agent-pack-audit/production-nde-functions-3.json` | Ancillary discovery: structural diagnostics for the separate physics package; not numerical qualification evidence. [docs/registry/agent-pack-audit/production-nde-functions-3.json:L1-L18] [docs/registry/agent-pack-audit/production-nde-functions-3.json:L6482-L6518] |
| `docs/registry/implementation-map.md` | Repository-wide wind/current family discovery summary. [docs/registry/implementation-map.md:L24-L24] |
| `docs/registry/implementation-map.yaml` | Full parsed map family inventory in section 9 at its separately stated ref. [docs/registry/implementation-map.yaml:L1-L3] |
| `docs/plans/2026-07-13-issue-1571-aqwa-triangles-frequency-v261.md` | Incidental historical mention of DYNMOOR field-name contracts; no additional wrapper requirements inferred. [docs/plans/2026-07-13-issue-1571-aqwa-triangles-frequency-v261.md:L171-L171] |

The calculation-citation contract was read from the planning repository; no unavailable referenced contract is claimed, and this change introduces no standards-derived constants or module code. Existing serializer outputs carry no citation sidecar. [src/digitalmodel/dynmoor_mooring/workflow.py:L240-L272]

| Open item | Maintainer action / tracking |
|---|---|
| Silent defaults, finite values, integer coercion and units | Add schema and malformed-input coverage; issue pending (1). [src/digitalmodel/dynmoor_mooring/workflow.py:L285-L301] [src/digitalmodel/dynmoor_mooring/workflow.py:L348-L350] |
| Repeated invocation and output location / encoding / disclosure | Test returned-config reuse and portable output handling; issue pending (2). [src/digitalmodel/dynmoor_mooring/workflow.py:L263-L272] [src/digitalmodel/dynmoor_mooring/workflow.py:L388-L424] |
| Optional-branch and generated-evidence coverage | Add wrapper fender/wind/perpendicular/export checks and fingerprinted evidence; issue pending (3). [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L69-L151] [src/digitalmodel/dynmoor_mooring/workflow.py:L188-L228] |
| Time-domain scope | Four placeholders remain SKIP, not NOT_RUN; scope or implement separately; issue pending (4). [tests/subsea/mooring_analysis/test_dynmoor.py:L803-L820] |
| UNVERIFIED claim in section 3 | Qualify publication editions, validity envelopes and independent benchmark provenance; issue pending (5). [tests/subsea/mooring_analysis/test_dynmoor.py:L1-L38] |
| Map gaps and router classification | Add missing package families and upstream wind definition, reconcile delegation; issue pending (6). [src/digitalmodel/dynmoor_mooring/workflow.py:L79-L94] [src/digitalmodel/subsea/mooring_analysis/dynmoor.py:L1104-L1118] |

### Full offline test inventory

| Existing node ID | Before / after result | Source at code pin |
|---|---|---|
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestCatinbVsLegacyBinary::test_catenary_table` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L222-L233] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestCatinbVsLegacyBinary::test_pretension_summary` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L235-L243] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestCatinbVsLegacyBinary::test_anchor_holding_power` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L245-L247] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestCatnryVsLegacyBinary::test_catenary_table` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L251-L264] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestCatnryVsLegacyBinary::test_pretension_summary` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L266-L274] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestPassingShipVsLegacyBinary::test_parallel_sweep` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L278-L287] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestPassingShipVsLegacyBinary::test_perpendicular_sweep` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L289-L299] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestLibraryVsOracle::test_catinb_matches_oracle` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L532-L550] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestLibraryVsOracle::test_catinb_sloped_bottom_matches_oracle` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L552-L572] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestLibraryVsOracle::test_parship_matches_oracle` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L574-L580] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestLibraryVsOracle::test_pership_matches_oracle` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L582-L592] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTablesAndStatics::test_table_monotonic` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L601-L605] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTablesAndStatics::test_interp_recovers_pretension` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L607-L612] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTablesAndStatics::test_interp_slack_and_off_table` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L614-L617] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTablesAndStatics::test_anchor_pull_zero_when_grounded` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L619-L623] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTablesAndStatics::test_symmetric_pattern_equilibrium` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L625-L641] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTablesAndStatics::test_broken_line_carries_no_load` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L643-L656] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTablesAndStatics::test_pretension_guards` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L658-L667] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTablesAndStatics::test_fulcat_dispatch_and_shape` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L669-L681] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestFendersAndWind::test_not_engaged` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L692-L694] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestFendersAndWind::test_linear_first_segment` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L696-L702] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestFendersAndWind::test_breakpoint_continuity` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L704-L709] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestFendersAndWind::test_side_convention` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L711-L715] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestFendersAndWind::test_wcoef_steps` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L717-L722] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestFendersAndWind::test_wind_model_and_force` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L724-L743] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestPassingShipBehaviour::test_parship_zero_stagger_symmetry` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L747-L758] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestPassingShipBehaviour::test_parship_speed_squared_scaling` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L760-L764] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestPassingShipBehaviour::test_parship_integer_length_quirk` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L766-L770] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestPassingShipBehaviour::test_pership_outside_window_is_zero` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L772-L779] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestPassingShipBehaviour::test_separation_guards` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L781-L789] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestPassingShipBehaviour::test_sweep_window` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor.py:L791-L794] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTimeDomainFollowUp::test_oscill_regular_wave_response` | SKIP / SKIP — unimplemented time-domain placeholder | [tests/subsea/mooring_analysis/test_dynmoor.py:L809-L810] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTimeDomainFollowUp::test_random_sea_statistics` | SKIP / SKIP — unimplemented time-domain placeholder | [tests/subsea/mooring_analysis/test_dynmoor.py:L812-L814] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTimeDomainFollowUp::test_wave_drift_options` | SKIP / SKIP — unimplemented time-domain placeholder | [tests/subsea/mooring_analysis/test_dynmoor.py:L816-L817] |
| `tests/subsea/mooring_analysis/test_dynmoor.py::TestTimeDomainFollowUp::test_broken_line_transient` | SKIP / SKIP — unimplemented time-domain placeholder | [tests/subsea/mooring_analysis/test_dynmoor.py:L819-L820] |
| `tests/subsea/mooring_analysis/test_dynmoor_workflow.py::test_router_static_outputs` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L69-L92] |
| `tests/subsea/mooring_analysis/test_dynmoor_workflow.py::test_router_passing_ship` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L95-L125] |
| `tests/subsea/mooring_analysis/test_dynmoor_workflow.py::test_router_time_domain_not_ported` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L128-L132] |
| `tests/subsea/mooring_analysis/test_dynmoor_workflow.py::test_router_validation_errors` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L135-L144] |
| `tests/subsea/mooring_analysis/test_dynmoor_workflow.py::test_engine_routes_basename` | PASS / PASS | [tests/subsea/mooring_analysis/test_dynmoor_workflow.py:L147-L151] |
