# Pipelay module knowledge pack

| Header | Value |
|---|---|
| Module / repository | `pipelay` / `digitalmodel` |
| Package | `src/digitalmodel/pipelay` |
| Pack revision | 0, 2026-10-16 |
| Code revision | `7aad603be1c6136106dbb2381ce5096b050654f8` |
| Analysis family / registry code | OrcaFlex installation / IN |
| Quality role | OrcaFlex installation quality manager |
| Status | DRAFT; independent clearance pending |

All repository citations below resolve at the code revision; implementation-map rows are also byte-identical at map revision `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, whose recorded data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`. [docs/registry/implementation-map.yaml:L1-L3]

## 1. Purpose and scope

`router` dispatches analytical J-lay or reel-lay screening, writes a JSON summary, and merges the result into the supplied configuration; the registry records development maturity. [src/digitalmodel/pipelay/workflow.py:L164-L203] [docs/registry/module-routing.yaml:L460-L465]

J-lay uses `JLayConfig.calculate_sagbend`; reel-lay computes core bending strain, residual curvature and out-of-straightness directly; optional operability delegates to `LayOperability`. [src/digitalmodel/pipelay/workflow.py:L49-L93] [src/digitalmodel/pipelay/workflow.py:L96-L161]

The scope is the two package Python files, without sub-packages; the workflow emits screening JSON rather than a native solver model, lowering-stage solution, cyclic-plasticity solution or ovalisation result, despite the introductory ovalisation wording. [src/digitalmodel/pipelay/__init__.py:L1-L10] [src/digitalmodel/pipelay/workflow.py:L15-L19] [src/digitalmodel/pipelay/workflow.py:L79-L93] [src/digitalmodel/pipelay/workflow.py:L143-L203]

## 2. Inputs and outputs

Input quantities are scalar screening inputs; the tower angle is measured from vertical, and no spatial origin, coordinate transform or phase convention is defined by this router. [src/digitalmodel/orcaflex/pipelay_analysis.py:L211-L220] [src/digitalmodel/pipelay/workflow.py:L62-L193]

| Input | Unit | Allowed range / handling | Default | Source |
|---|---|---|---|---|
| `basename`, `pipelay.method` | text | J-lay aliases or reel-lay aliases; other methods raise | method J-Lay | [src/digitalmodel/pipelay/workflow.py:L164-L177] |
| `config.water_depth_m` | m | greater than zero in delegated model | 1500.0 | [src/digitalmodel/pipelay/workflow.py:L67-L72] [src/digitalmodel/orcaflex/pipelay_analysis.py:L216-L220] |
| `config.tower_angle_deg` | degree from vertical | 0 to 15 inclusive | 0.0 | [src/digitalmodel/pipelay/workflow.py:L67-L72] [src/digitalmodel/orcaflex/pipelay_analysis.py:L218-L220] |
| `config.top_tension_kN` | kN | greater than zero | 800.0 | [src/digitalmodel/pipelay/workflow.py:L67-L72] [src/digitalmodel/orcaflex/pipelay_analysis.py:L220-L220] |
| `config.allowable_sagbend_strain` | strain, dimensionless | positive required for finite utilisation; otherwise infinity | 0.002 | [src/digitalmodel/pipelay/workflow.py:L75-L77] |
| `config.reel_core_radius_m` | m | positive; otherwise ValueError | 8.0 | [src/digitalmodel/pipelay/workflow.py:L111-L113] |
| `config.allowable_reeling_strain` | strain, dimensionless | positive required for finite utilisation; otherwise infinity | 0.02 | [src/digitalmodel/pipelay/workflow.py:L114-L127] |
| `config.straightener_efficiency` | fraction, dimensionless | 0 to 1 inclusive | 0.95 | [src/digitalmodel/pipelay/workflow.py:L115-L117] |
| `config.residual_out_of_straightness_span_m` | m | float conversion only, no sign check | 12.0 | [src/digitalmodel/pipelay/workflow.py:L118-L141] |
| `config.allowable_overbend_strain` | strain, dimensionless | float conversion, then division in helper; no positivity guard | 0.002 | [src/digitalmodel/pipelay/workflow.py:L179-L186] [src/digitalmodel/orcaflex/pipelay_analysis.py:L307-L324] |
| `pipe.outer_diameter`, `pipe.wall_thickness` | m | positive individually; no thickness-to-diameter constraint | 0.3238, 0.0206 | [src/digitalmodel/orcaflex/pipelay_analysis.py:L49-L61] |
| `pipe.steel_density`, `pipe.seawater_density` | kg/m³ | positive | 7850.0, 1025.0 | [src/digitalmodel/orcaflex/pipelay_analysis.py:L51-L57] |
| `pipe.youngs_modulus`, `pipe.smys` | Pa | positive | 207e9, 448e6 | [src/digitalmodel/orcaflex/pipelay_analysis.py:L52-L53] |
| `pipe.poissons_ratio` | dimensionless | positive; no upper bound | 0.3 | [src/digitalmodel/orcaflex/pipelay_analysis.py:L54-L54] |
| `pipe.coating_thickness`, `pipe.coating_density` | m, kg/m³ | non-negative | 0.06, 2400.0 | [src/digitalmodel/orcaflex/pipelay_analysis.py:L55-L56] |
| `operability.max_hs_lay`, `max_hs_welding`, `max_hs_aband_recovery` | m | positive; supplied only when operability requested | model defaults at cited lines; no standard allowable asserted | [src/digitalmodel/pipelay/workflow.py:L49-L59] [src/digitalmodel/orcaflex/pipelay_analysis.py:L272-L275] |
| `operability.lay_rate_calm`, `lay_rate_degradation` | km/day, km/day/m | positive rate, non-negative degradation | 4.0, 0.5 | [src/digitalmodel/orcaflex/pipelay_analysis.py:L276-L278] |
| `outputs.directory`, `outputs.summary_json` | path text | relative directory uses configuration directory, else current directory | `results/pipelay`, `pipelay_summary.json` | [src/digitalmodel/pipelay/workflow.py:L196-L203] |

Table 1. Workflow and delegated-model inputs; defaults describe code behaviour, without qualification as design-basis values.

| Output | Unit / representation | Meaning and precision | Source |
|---|---|---|---|
| `sagbend` | m, strain, MPa | parameter, radius, offset and suspended length rounded to 0.1 m; strain to six decimals; stress to 0.1 MPa | [src/digitalmodel/orcaflex/pipelay_analysis.py:L253-L260] |
| `sagbend_strain_utilisation`, `screening_status` | dimensionless, text | utilisation rounded to four decimals; status uses unrounded utilisation at most 1.0 | [src/digitalmodel/pipelay/workflow.py:L75-L90] |
| reel strain / utilisation | dimensionless | six / four decimal places; status uses unrounded utilisation | [src/digitalmodel/pipelay/workflow.py:L148-L157] |
| residual curvature / radius / offset | 1/m, m, m | eight decimals / 0.1 m / five decimals | [src/digitalmodel/pipelay/workflow.py:L151-L156] |
| pipe submerged weight | N/m | rounded to three decimals | [src/digitalmodel/pipelay/workflow.py:L84-L85] [src/digitalmodel/pipelay/workflow.py:L146-L147] |
| `min_stinger_radius_m` | m | helper output rounded to 0.1 m | [src/digitalmodel/pipelay/workflow.py:L179-L186] |
| operability | m, km/day | maximum wave heights plus half-metre-step rate table; zero above lay limit | [src/digitalmodel/pipelay/workflow.py:L49-L59] [src/digitalmodel/orcaflex/pipelay_analysis.py:L280-L304] |
| `summary_json`, returned configuration | UTF-8 JSON, dictionary | summary written before adding its path; `cfg.pipelay` and root `screening_status` updated | [src/digitalmodel/pipelay/workflow.py:L188-L203] |

Table 2. Output units and precision.

## 3. Method and governing standards

| Method | Implementation and basis | Qualification |
|---|---|---|
| J-lay | `run_jlay` delegates sagbend geometry; the delegate forms horizontal tension from tower angle, uses a small horizontal component at zero angle, divides by submerged weight, and uses hyperbolic geometry for span | Code reference: Bai and Bai (2014), *Subsea Pipeline Design, Analysis and Installation*, Chapter 5; the external text is not checked. [src/digitalmodel/pipelay/workflow.py:L62-L93] [src/digitalmodel/orcaflex/pipelay_analysis.py:L211-L260] |
| Reel-lay | `run_reel_lay`: strain `D/(2R)`; residual curvature `(1-efficiency)/R`; sagitta `curvature*span²/8` | Simplified screening, without cyclic material history or ovalisation output; code reference: Bai and Bai (2014), Chapter 6; the external text is not checked. [src/digitalmodel/pipelay/workflow.py:L96-L161] |
| Stinger radius | `calculate_stinger_radius`: `D/(2*allowable strain)` | Code names DNV-OS-F101, *Submarine Pipeline Systems*, Section 11; edition absent; clause text not checked against the licensed copy. [src/digitalmodel/orcaflex/pipelay_analysis.py:L307-L324] |
| Pipe section and submerged weight | `PipelayPipeProperties` uses circular-section area and displaced coated area, with steel and coating masses | Code names DNV-OS-F101, *Submarine Pipeline Systems*, Section 5; edition absent; clause text not checked against the licensed copy. [src/digitalmodel/orcaflex/pipelay_analysis.py:L44-L91] |
| Operability | `LayOperability.lay_rate_at_hs` subtracts linear degradation, clamps non-negative and returns zero above the lay wave-height limit | Code names DNV-OS-F101, *Submarine Pipeline Systems*, Section 11; edition absent; clause text not checked against the licensed copy. [src/digitalmodel/orcaflex/pipelay_analysis.py:L267-L304] |

Table 3. Algorithms and source attribution; no standard-tabulated allowable is reproduced.

The workflow docstring calls Section 11 “installation”; the coverage document also records missing edition and clause title, so a complete edition-to-clause mapping is not established. [src/digitalmodel/pipelay/workflow.py:L21-L24] [docs/domains/pipeline-installation/coverage-2026-10.md:L171-L179]

The result dictionaries contain no `Citation` sidecar; provenance qualification is an open item rather than an assertion of standard compliance. [src/digitalmodel/pipelay/workflow.py:L79-L93] [src/digitalmodel/pipelay/workflow.py:L143-L161] [src/digitalmodel/pipelay/workflow.py:L188-L203]

## 4. Validated worked examples

No independently sourced installation benchmark is established by the five workflow tests: they check self-consistency, ordering, dispatch and status, rather than an external expected solution; the validated-example count is zero. [tests/pipelay/test_pipelay_workflow.py:L29-L122]

The following five cases are regression examples, including the algebraic reel-strain check; a repeated expression from the implementation is not an independent benchmark. [tests/pipelay/test_pipelay_workflow.py:L67-L83] [src/digitalmodel/pipelay/workflow.py:L122-L127]

| Regression case / input | Existing test node ID | Expected result, tolerance and basis | Offline result |
|---|---|---|---|
| J-lay template; `examples/workflows/jlay-pipelay/input.yml` | `tests/pipelay/test_pipelay_workflow.py::test_jlay_template_runs_offline` | positive finite radius, suspended length above depth in m; utilisation at most 1.0 and consistent to relative 0.1 %; zero final rate in km/day, non-increasing in-band rate; JSON method/status match; test-defined criteria | PASS at BASE and AFTER [tests/pipelay/test_pipelay_workflow.py:L29-L64] |
| Reel-lay template; `examples/workflows/reel-lay-pipelay/input.yml` | `tests/pipelay/test_pipelay_workflow.py::test_reel_lay_template_runs_offline` | strain equals `D/(2R)` to relative 0.01 %; utilisation consistent to relative 0.1 % and at most 1.0; positive residual strain below reeling strain; positive offset in m; JSON method/status match | PASS at BASE and AFTER [tests/pipelay/test_pipelay_workflow.py:L67-L92] |
| Tight-core inline configuration | `tests/pipelay/test_pipelay_workflow.py::test_reeling_strain_exceeding_limit_fails` | utilisation greater than 1.0, dimensionless; status fail | PASS at BASE and AFTER [tests/pipelay/test_pipelay_workflow.py:L95-L107] |
| Unsupported-method inline configuration | `tests/pipelay/test_pipelay_workflow.py::test_router_rejects_unknown_method` | exactly one ValueError with unknown-method message | PASS at BASE and AFTER [tests/pipelay/test_pipelay_workflow.py:L110-L112] |
| Increased-tension inline configurations | `tests/pipelay/test_pipelay_workflow.py::test_jlay_higher_tension_reduces_sagbend_strain` | strain at the larger specified tension strictly lower, dimensionless | PASS at BASE and AFTER [tests/pipelay/test_pipelay_workflow.py:L115-L122] |

Table 4. Existing regression examples; tolerances reproduce the test assertions, without a claim of engineering validation.

Execution at the code pin, both BASE and AFTER: 22 collected, 21 passed, 0 failed, 0 ordinary skips, 1 existing expected failure, 0 NOT_RUN; all five workflow cases passed in each run. Command form: `PYTHONPATH=src uv run --no-sources python -m pytest <temporary-copy workflow test> tests/orcaflex/test_pipelay_analysis.py -q -rA -p no:cacheprovider --basetemp=<external scratch>`. The host-required absolute uv executable was used, without an addopts override. The two file-writing template tests used byte-identical tests and example inputs in an external temporary copy; calculation sources remained those at the code pin. No native solver was executed. [tests/pipelay/test_pipelay_workflow.py:L14-L19] [tests/orcaflex/test_pipelay_analysis.py:L69-L78]

## 5. QA gates

AQG r0, Sections 3–10, assigns G0–G5 to the installation family; the scope dispositions below distinguish an offline JSON screen from a future native installation model. The proposed check criteria are requirements for this pack, rather than a record of native clearance.

| Gate | Applicability to this package | Module-specific executable check and numeric criterion | Evidence / current disposition |
|---|---|---|---|
| G0 inputs | Applicable | C01: run unsupported-method regression; 1 ValueError, 0 silently accepted unknown methods. C02: for each supplied input, compare its key against the fields in section 2 and require 0 unknown keys, 0 omitted required design-basis quantities, 0 non-finite scalars and 0 unit mismatches and 0 destination mismatches; compare pipe wall thickness to radius, requiring `0 < wall_thickness < outer_diameter/2`, m | C01 existing test; C02 hand audit against section 2, not a shipped validator. Filtering and defaults are detailed in section 6. [tests/pipelay/test_pipelay_workflow.py:L110-L112] [src/digitalmodel/pipelay/workflow.py:L42-L53] [src/digitalmodel/orcaflex/pipelay_analysis.py:L49-L61] |
| G1 load | NOT_APPLICABLE to screening JSON | 0 native models are written by `_write_summary`. If this output feeds a native OrcaFlex model, G1 will require that model to open in the recorded target version with 0 load errors and 0 warnings, using the matching-version demo route if supported, otherwise licensed `LoadData`; that route is outside this offline package | No native input or solver version emitted; future native load remains NOT_RUN, rather than inferred from JSON parsing. [src/digitalmodel/pipelay/workflow.py:L196-L203] AQG r0, Section 5. |
| G2 statics / hand checks | Applicable to analytical screening; lowering-stage hook/sling checks NOT_APPLICABLE because no lowering-stage, hook or sling input/output is present | C03: J-lay template radius finite and greater than 0 m, suspended length above depth in m, utilisation at most 1.0 and self-consistency within relative 0.1 %. C04: reel template strain equals `D/(2R)` within relative 0.01 %, utilisation at most 1.0; tight-core utilisation greater than 1.0. C05: larger J-lay tension gives strictly lower sagbend strain, dimensionless | Existing regression tests only; these are not independent native statics hand references. Every future lowering-stage hook/sling balance will need separate evidence before native clearance. [tests/pipelay/test_pipelay_workflow.py:L29-L51] [tests/pipelay/test_pipelay_workflow.py:L67-L107] [tests/pipelay/test_pipelay_workflow.py:L115-L122] [src/digitalmodel/pipelay/workflow.py:L62-L161] |
| G3 short dynamics | NOT_APPLICABLE to this analytical screen | 0 time-history / splash-zone / deepest-stage simulations are produced; a future native workflow will need stage-specific dynamics evidence before using this screen for installation clearance | No stage-dependent simulation result in the returned payload. [src/digitalmodel/pipelay/workflow.py:L79-L93] [src/digitalmodel/pipelay/workflow.py:L143-L161] AQG r0, Sections 7 and 10. |
| G4 diffraction | NOT_APPLICABLE to this package | 0 meshes, vessel types, RAOs or diffraction result inputs appear in `run_jlay` or `run_reel_lay`; a downstream native model importing diffraction data will require the separate G4 evidence | This disposition is limited to the current package interface. [src/digitalmodel/pipelay/workflow.py:L62-L161] AQG r0, Sections 8 and 10. |
| G5 evidence appendix | Applicable to screening evidence; a complete issued-report appendix is not implemented here | C06: each template summary parses, with 0 mismatches in method/status versus the expected values. C07: for each generated summary, parse with non-finite constants rejected and require 0 rejected tokens; record input and output SHA-256, with 0 digest mismatches on readback | C06 exists in the two template tests. C07 is an executable strict-JSON/readback procedure; full signed gate appendix remains an open item. [tests/pipelay/test_pipelay_workflow.py:L22-L26] [tests/pipelay/test_pipelay_workflow.py:L62-L64] [tests/pipelay/test_pipelay_workflow.py:L90-L92] [src/digitalmodel/pipelay/workflow.py:L196-L203] |

Table 5. Seven checks: G0 two, G2 three, G5 two; the remaining gates have scope-specific reasons.

C02 is performed by listing supplied dictionary keys, checking finite scalar values and confirming units and the geometry inequality against the source fields; C07 is performed with `json.loads(text, parse_constant=reject)` where `reject` raises ValueError, then SHA-256 readback. These are explicit manual/script procedures; their definition does not imply an automated production guard.

Check record at the code pin: C01 and C03–C06 passed in the cited BASE and AFTER tests; C07 passed for both templates in both runs, with zero rejected constants and zero digest mismatches. C02 completeness fails against the zero-omission criterion: each template omits two pipe scalars, steel density and Poisson ratio, which use model defaults; this is existing input behaviour, tracked in issue pending (1), rather than a change to the calculations. [tests/pipelay/test_pipelay_workflow.py:L29-L122] [examples/workflows/jlay-pipelay/input.yml:L21-L29] [examples/workflows/reel-lay-pipelay/input.yml:L23-L31] [src/digitalmodel/orcaflex/pipelay_analysis.py:L49-L57]

## 6. Known failure modes and pitfalls

sweep: 27 found

The sweep scope is every Python file under `src/digitalmodel/pipelay`; the following commands will reproduce the textual candidates, followed by manual review of defaults, units, external conversions and output writes:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|float\(|int\(|\.get\(' src/digitalmodel/pipelay --glob '*.py'
rg -n 'filtered|or \{\}|else 0|inf|round|write|json|unit|_m|_kN|_deg' src/digitalmodel/pipelay --glob '*.py'
```

No swallowing `except`, skipped-record `continue`, error `return None`, error empty-list/dictionary return or empty-result cache write is present in the package; the rows count 27 distinct reviewed fallback, conversion, unit-assumption or serialization sites, not 27 demonstrated wrong numerical results. [src/digitalmodel/pipelay/__init__.py:L1-L10] [src/digitalmodel/pipelay/workflow.py:L27-L203]

| Site at pin | Symptom / cause | Detection in section 5 | Tracking / date |
|---|---|---|---|
| S01 `workflow.py:45` | Unknown pipe fields discarded; falsey pipe mapping becomes empty and model defaults are used | C02; no shipped rejection test | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L42-L46] |
| S02 `workflow.py:52` | Unknown operability fields discarded; falsey mapping uses defaults | C02; no shipped rejection test | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L49-L58] |
| S03 `workflow.py:64` | Missing J-lay pipe block uses default pipe | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L64-L64] |
| S04 `workflow.py:65` | Missing J-lay config becomes empty, enabling defaults below | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L65-L65] |
| S05 `workflow.py:68` | Missing water depth silently defaults; bare float conversion assumes m and decimal-dot text | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L67-L72] |
| S06 `workflow.py:70` | Missing tower angle becomes zero degrees from vertical; bare float text conversion | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L67-L72] |
| S07 `workflow.py:71` | Missing top tension defaults; bare float conversion assumes kN, not N | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L67-L72] |
| S08 `workflow.py:75` | Default strain criterion and bare float conversion; no finite-positive guard | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L75-L77] |
| S09 `workflow.py:77` | Non-positive limit becomes infinite utilisation rather than input error | C02, C07 | issue pending (2); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L75-L89] |
| S10 `workflow.py:108` | Missing reel-lay pipe block uses default pipe | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L108-L109] |
| S11 `workflow.py:109` | Missing reel-lay config enables defaults | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L108-L111] |
| S12 `workflow.py:111` | Default core radius and float conversion assume m; positivity comparison does not establish finiteness | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L111-L113] |
| S13 `workflow.py:114` | Default reeling limit; float conversion, no finite-positive guard | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L114-L127] |
| S14 `workflow.py:115` | Default straightener fraction is assumed if omitted; float conversion | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L115-L117] |
| S15 `workflow.py:118` | Span defaults; float conversion assumes m, no sign or finiteness guard | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L118-L141] |
| S16 `workflow.py:127` | Non-positive reeling limit becomes infinite utilisation | C02, C07 | issue pending (2); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L124-L128] |
| S17 `workflow.py:135` | Perfect straightening generates infinite radius; strict JSON compatibility is not ensured | C07 | issue pending (2); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L130-L137] |
| S18 `workflow.py:137` | Conditional zero residual strain is a fallback branch, not an independent zero-curvature verification | C02, C07 | issue pending (2); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L130-L137] |
| S19 `workflow.py:166` | Missing or falsey pipelay block silently becomes an empty dictionary | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L164-L167] |
| S20 `workflow.py:167` | Omitted method defaults to J-lay; coercion to text can hide wrong input type | C01 catches unsupported value; C02 catches omission/type | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L166-L177] |
| S21 `workflow.py:183` | Overbend strain defaults; float conversion; zero can divide by zero, negative can return negative radius | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L179-L186] [src/digitalmodel/orcaflex/pipelay_analysis.py:L307-L324] |
| S22 `workflow.py:188` | Missing outputs block falls back to default destination | C02 manual destination audit | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L188-L189] |
| S23 `workflow.py:197` | Missing directory uses `results/pipelay` | C02 manual destination audit | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L196-L199] |
| S24 `workflow.py:199` | Missing configuration-directory context uses process working directory | C02 manual destination audit | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L197-L200] |
| S25 `workflow.py:201` | Missing summary filename uses the same default filename; existing output can be overwritten | C02 manual destination audit | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L201-L202] |
| S26 `workflow.py:202` | Default JSON encoding does not reject infinity or NaN; file can be readable by Python yet invalid for strict consumers | C07 | issue pending (2); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L202-L202] |
| S27 `workflow.py:123-141` | Diameter, radius and span are assumed in metres; the curvature/offset formula has no separate unit metadata | C02 | issue pending (1); 2026-10-16 [src/digitalmodel/pipelay/workflow.py:L120-L141] |

Table 6. Exhaustive package sweep ledger; the counts include intended defaults so that omission cannot silently become a qualified design basis.

| Additional pitfall | Detection | Tracking / evidence |
|---|---|---|
| Introductory docstring promises residual ovalisation but reel payload does not contain it | no detection check exists | issue pending (3). [src/digitalmodel/pipelay/workflow.py:L15-L19] [src/digitalmodel/pipelay/workflow.py:L143-L161] |
| Standard edition, clause title and calculation-output provenance unresolved | no detection check exists | #1817; source-side evidence is the incomplete reference and result dictionaries. [src/digitalmodel/pipelay/workflow.py:L21-L24] [src/digitalmodel/pipelay/workflow.py:L79-L93] [src/digitalmodel/pipelay/workflow.py:L143-L161] |
| J-lay zero-angle horizontal component is an imposed fallback, while non-positive submerged weight becomes infinite catenary parameter | no detection check exists for physical force/geometry closure; C03 is only positivity/self-consistency | issue pending (3); upstream method is not designated an independent hand reference. [src/digitalmodel/orcaflex/pipelay_analysis.py:L231-L259] |
| Upstream S-lay depth table repeats its angle without using loop depth in the angle calculation | existing strict expected-failure test, outside counted package scope | #1817. [src/digitalmodel/orcaflex/pipelay_analysis.py:L180-L202] [tests/orcaflex/test_pipelay_analysis.py:L69-L78] |

Table 7. Confirmed source limitations; no incident issue was specified for this package, and the closed template issue is provenance rather than evidence of numerical validation. [tests/pipelay/test_pipelay_workflow.py:L1-L5]

## 7. Lessons log

| Date | Change reference | Lesson / pack change | Evidence | Shared-log entry |
|---|---|---|---|---|
| 2026-10-16 | pack revision 0 | Separate screening regression from independent installation validation; inventory defaults and non-finite summaries | [tests/pipelay/test_pipelay_workflow.py:L29-L122] [src/digitalmodel/pipelay/workflow.py:L124-L157] | Not assigned; issue pending (5) |

Table 8. Initial lesson entry; subsequent lessons will append rows.

## 8. Ownership

Review assignment for this draft: OrcaFlex installation quality manager, using a different provider from the producer; escalation for ambiguous method criteria or classification will remain with that family quality manager, and the maintainer will resolve cross-family scope.

UNVERIFIED: a named deputy and a durable shared-log/index entry are not established in the inspected module; assignment is required before clearance, issue pending (5). [src/digitalmodel/pipelay/__init__.py:L1-L10] [docs/registry/module-routing.yaml:L460-L465]

## 9. Dependencies

| Dependency / consumer | Role / version boundary | Evidence |
|---|---|---|
| `digitalmodel.orcaflex.pipelay_analysis` | Analytical upstream models: `JLayConfig`, `LayMethod`, `LayOperability`, `PipelayPipeProperties`, stinger helper; no solver version selected | [src/digitalmodel/pipelay/workflow.py:L33-L39] |
| `digitalmodel.engine` | Engine-facing router and input examples exercised by workflow tests | [tests/pipelay/test_pipelay_workflow.py:L14-L19] [src/digitalmodel/pipelay/workflow.py:L164-L193] |
| JSON and filesystem | UTF-8 summary serialization and output-directory creation | [src/digitalmodel/pipelay/workflow.py:L196-L203] |

Table 9. Dependencies at the package boundary; companion inventories are discovery metadata, not qualified solver evidence.

Every source mention of OrcaFlex or its interface, including case-insensitive package paths, is listed below; these are textual references or analytical imports, not native calls. [src/digitalmodel/pipelay/__init__.py:L1-L10] [src/digitalmodel/pipelay/workflow.py:L1-L39]

| Source location | Dependency interpretation | Evidence |
|---|---|---|
| Initializer line 4 | Analytical upstream path and explicit interface/license wording | [src/digitalmodel/pipelay/__init__.py:L3-L5] |
| Workflow line 4 | Analytical upstream module in docstring | [src/digitalmodel/pipelay/workflow.py:L3-L6] |
| Workflow line 6 | Docstring explicitly states no interface call or license requirement | [src/digitalmodel/pipelay/workflow.py:L5-L6] |
| Workflow line 11 | J-lay analytical configuration class reference | [src/digitalmodel/pipelay/workflow.py:L10-L14] |
| Workflow line 33 | Python import of analytical configuration classes and stinger helper | [src/digitalmodel/pipelay/workflow.py:L33-L39] |

Table 10. Complete source-level solver-name inventory for the package.

### Implementation-map inventory and counted-family search

The map family with an in-package row is `effective-tension-riser-stack-up`: one row, `run_jlay`; this is therefore the sole counted family and ranks first, with no tie. The map calls it independent, while source inspection shows delegation for sagbend geometry plus a local utilisation criterion. [docs/registry/implementation-map.yaml:L3698-L3708] [src/digitalmodel/pipelay/workflow.py:L62-L93]

The inventory below preserves every one of that family's 264 rows, including examples, tests, wrappers, callers and implementations outside the package; it is a transcription of the map classifications, not a new physical qualification of them. [docs/registry/implementation-map.md:L15-L20] [docs/registry/implementation-map.yaml:L2139-L4932]

Exact case-sensitive Python `re` signals from the named map revision:

```text
effective_tension|effectiveTension|top_tension|tensioner|stack_up|internal_pressure.*(?:area|A_i)|external_pressure.*(?:area|A_o)
effective.tension|tensioner|stack.up
internal_pressure|external_pressure|fluid_density|buoyancy_module
```

None of these signals matches `pipelay` when tested against the package-relative path case-insensitively, so none is removed; the exact package search yields two line hits, both inside listed `run_jlay`, and zero further second implementations, satisfying the at-most-one criterion. [docs/registry/implementation-map.yaml:L2139-L2144] [src/digitalmodel/pipelay/workflow.py:L71-L83]

A whole-repository search of tracked Python and shell sources, including `scripts/`, also covers `calculate_sagbend`, `run_jlay`, `run_reel_lay`, `residual_curvature`, `reeling_strain` and `calculate_stinger_radius`; upstream implementations are map follow-up rather than additional in-package misses. In particular the actual J-lay hyperbolic geometry and stinger formula are outside the package. [src/digitalmodel/orcaflex/pipelay_analysis.py:L222-L260] [src/digitalmodel/orcaflex/pipelay_analysis.py:L307-L324]

Map follow-up: catenary geometry has map rows upstream but no row inside this package; reel bending/residual-curvature screening, pipe-section/stinger bending and lay-rate operability need package-aware family classification before counting. These non-counted families do not change the result above; issue pending (4). [docs/registry/implementation-map.yaml:L1019-L1029] [src/digitalmodel/pipelay/workflow.py:L49-L59] [src/digitalmodel/pipelay/workflow.py:L96-L161] [src/digitalmodel/pipelay/workflow.py:L179-L186]

| Map row | Implementation path | Symbol (module-level if empty) | Recorded relation | Map evidence |
|---|---|---|---|---|
| 1 | `docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2146-L2155] |
| 2 | `docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2156-L2165] |
| 3 | `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2166-L2175] |
| 4 | `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2176-L2185] |
| 5 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r01_catenary` | independent | [docs/registry/implementation-map.yaml:L2186-L2195] |
| 6 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r02_lazy_wave` | independent | [docs/registry/implementation-map.yaml:L2196-L2205] |
| 7 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r05_lay_table` | independent | [docs/registry/implementation-map.yaml:L2206-L2215] |
| 8 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r06_multi_static` | independent | [docs/registry/implementation-map.yaml:L2216-L2225] |
| 9 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `extract_vessel_params` | example | [docs/registry/implementation-map.yaml:L2226-L2235] |
| 10 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `vessel_band_specs` | example | [docs/registry/implementation-map.yaml:L2236-L2247] |
| 11 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `find_required_tension` | example | [docs/registry/implementation-map.yaml:L2248-L2259] |
| 12 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `_build_infeasible_result` | example | [docs/registry/implementation-map.yaml:L2260-L2269] |
| 13 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `run_single_case` | example | [docs/registry/implementation-map.yaml:L2270-L2282] |
| 14 | `examples/demos/gtm/results_store_demo04.py` | `(module-level)` | example | [docs/registry/implementation-map.yaml:L2283-L2291] |
| 15 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_good_case` | example | [docs/registry/implementation-map.yaml:L2292-L2301] |
| 16 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_infeasible_case` | example | [docs/registry/implementation-map.yaml:L2302-L2311] |
| 17 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `test_999_sentinel_and_none_null_round_trip` | example | [docs/registry/implementation-map.yaml:L2312-L2324] |
| 18 | `examples/domains/workflows/04_pipeline_installation.py` | `analyze_installation_catenary` | example | [docs/registry/implementation-map.yaml:L2325-L2334] |
| 19 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_schedule_row` | example | [docs/registry/implementation-map.yaml:L2335-L2344] |
| 20 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_assemble_row` | example | [docs/registry/implementation-map.yaml:L2345-L2356] |
| 21 | `scripts/capabilities/build_wall_thickness_3d.py` | `(module-level)` | independent | [docs/registry/implementation-map.yaml:L2357-L2365] |
| 22 | `scripts/capabilities/build_wall_thickness_3d.py` | `_analyze_point` | independent | [docs/registry/implementation-map.yaml:L2366-L2375] |
| 23 | `scripts/capabilities/build_wall_thickness_3d.py` | `build_study` | wrapper | [docs/registry/implementation-map.yaml:L2376-L2387] |
| 24 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_axes` | independent | [docs/registry/implementation-map.yaml:L2388-L2397] |
| 25 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_pack_rows` | independent | [docs/registry/implementation-map.yaml:L2398-L2407] |
| 26 | `scripts/capabilities/build_wall_thickness_explorer.py` | `_run` | independent | [docs/registry/implementation-map.yaml:L2408-L2417] |
| 27 | `scripts/orcaflex/check_generic_scr_slwr.py` | `metrics` | independent | [docs/registry/implementation-map.yaml:L2418-L2427] |
| 28 | `scripts/orcaflex/check_generic_scr_slwr.py` | `calculate` | independent | [docs/registry/implementation-map.yaml:L2428-L2437] |
| 29 | `scripts/orcaflex/prepare_slay_campaign.py` | `catenary` | independent | [docs/registry/implementation-map.yaml:L2438-L2447] |
| 30 | `scripts/orcaflex/prepare_slay_campaign.py` | `case_spec` | independent | [docs/registry/implementation-map.yaml:L2448-L2457] |
| 31 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare_case` | independent | [docs/registry/implementation-map.yaml:L2458-L2470] |
| 32 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare` | independent | [docs/registry/implementation-map.yaml:L2471-L2484] |
| 33 | `scripts/parametric/build_orcaflex_library.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2485-L2494] |
| 34 | `scripts/python/digitalmodel/analysis/APIST2RD_Methods.py` | `APISTD2RDCalculation` | independent | [docs/registry/implementation-map.yaml:L2495-L2504] |
| 35 | `scripts/python/digitalmodel/analysis/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | independent | [docs/registry/implementation-map.yaml:L2505-L2514] |
| 36 | `scripts/python/digitalmodel/analysis/RiserStackup.py` | `StackUpDiagram` | independent | [docs/registry/implementation-map.yaml:L2515-L2524] |
| 37 | `scripts/python/digitalmodel/analysis/StackUp_3000Ft.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2525-L2534] |
| 38 | `scripts/python/digitalmodel/analysis/StackUp_9500Ft.py` | `main` | independent | [docs/registry/implementation-map.yaml:L2535-L2544] |
| 39 | `scripts/reporting/build_riser_pipe_comparison.py` | `evaluate` | caller | [docs/registry/implementation-map.yaml:L2545-L2556] |
| 40 | `scripts/reporting/build_riser_pipe_comparison.py` | `_validate` | independent | [docs/registry/implementation-map.yaml:L2557-L2566] |
| 41 | `scripts/reporting/build_riser_pipe_comparison.py` | `numeric_values` | independent | [docs/registry/implementation-map.yaml:L2567-L2576] |
| 42 | `scripts/reporting/build_riser_pipe_comparison.py` | `_input_table` | independent | [docs/registry/implementation-map.yaml:L2577-L2586] |
| 43 | `scripts/riser_database/build_tables.py` | `(module-level)` | independent | [docs/registry/implementation-map.yaml:L2587-L2595] |
| 44 | `src/digitalmodel/asset_integrity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | independent | [docs/registry/implementation-map.yaml:L2596-L2605] |
| 45 | `src/digitalmodel/code_checks/dnv_os_f101.py` | `local_buckling_lcc` | independent | [docs/registry/implementation-map.yaml:L2606-L2615] |
| 46 | `src/digitalmodel/code_checks/workflow.py` | `_run_api_rp_2rd` | independent | [docs/registry/implementation-map.yaml:L2616-L2625] |
| 47 | `src/digitalmodel/code_checks/workflow.py` | `_run_dnv_os_f201` | independent | [docs/registry/implementation-map.yaml:L2626-L2635] |
| 48 | `src/digitalmodel/drilling_riser/assembly.py` | `minimum_top_tension_16q` | independent | [docs/registry/implementation-map.yaml:L2636-L2645] |
| 49 | `src/digitalmodel/drilling_riser/assembly.py` | `_unit_count` | independent | [docs/registry/implementation-map.yaml:L2646-L2655] |
| 50 | `src/digitalmodel/drilling_riser/assembly.py` | `check_rig_capability` | independent | [docs/registry/implementation-map.yaml:L2656-L2667] |
| 51 | `src/digitalmodel/drilling_riser/campaign.py` | `case_spec` | independent | [docs/registry/implementation-map.yaml:L2668-L2679] |
| 52 | `src/digitalmodel/drilling_riser/campaign.py` | `_ring_tensioners` | independent | [docs/registry/implementation-map.yaml:L2680-L2689] |
| 53 | `src/digitalmodel/drilling_riser/campaign.py` | `set_line_tension` | independent | [docs/registry/implementation-map.yaml:L2690-L2701] |
| 54 | `src/digitalmodel/drilling_riser/campaign.py` | `calibrate_line_tension` | independent | [docs/registry/implementation-map.yaml:L2702-L2715] |
| 55 | `src/digitalmodel/drilling_riser/campaign.py` | `_statics_path` | independent | [docs/registry/implementation-map.yaml:L2716-L2726] |
| 56 | `src/digitalmodel/drilling_riser/campaign.py` | `robust_statics` | independent | [docs/registry/implementation-map.yaml:L2727-L2740] |
| 57 | `src/digitalmodel/drilling_riser/campaign.py` | `_tensioner_vertical_n` | independent | [docs/registry/implementation-map.yaml:L2741-L2750] |
| 58 | `src/digitalmodel/drilling_riser/campaign.py` | `tensioner_vertical_target_n` | independent | [docs/registry/implementation-map.yaml:L2751-L2760] |
| 59 | `src/digitalmodel/drilling_riser/campaign.py` | `physical_state_checks` | independent | [docs/registry/implementation-map.yaml:L2761-L2774] |
| 60 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.prepare` | independent | [docs/registry/implementation-map.yaml:L2775-L2787] |
| 61 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.extract` | independent | [docs/registry/implementation-map.yaml:L2788-L2797] |
| 62 | `src/digitalmodel/drilling_riser/envelope.py` | `compute_operating_envelope` | independent | [docs/registry/implementation-map.yaml:L2798-L2807] |
| 63 | `src/digitalmodel/drilling_riser/global_model/build.py` | `equivalent_string_top_tension_n` | independent | [docs/registry/implementation-map.yaml:L2808-L2817] |
| 64 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_tensioner_geometry` | independent | [docs/registry/implementation-map.yaml:L2818-L2827] |
| 65 | `src/digitalmodel/drilling_riser/global_model/build.py` | `winch_tension_n` | independent | [docs/registry/implementation-map.yaml:L2828-L2839] |
| 66 | `src/digitalmodel/drilling_riser/global_model/build.py` | `build_generic_spec` | independent | [docs/registry/implementation-map.yaml:L2840-L2852] |
| 67 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `effective_tension_chain` | independent | [docs/registry/implementation-map.yaml:L2853-L2862] |
| 68 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tension_references` | independent | [docs/registry/implementation-map.yaml:L2863-L2874] |
| 69 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tensioner_lateral_stiffness_n_m` | independent | [docs/registry/implementation-map.yaml:L2875-L2884] |
| 70 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `reference_periods` | caller | [docs/registry/implementation-map.yaml:L2885-L2896] |
| 71 | `src/digitalmodel/drilling_riser/global_model/hang_off.py` | `hung_weight_n` | independent | [docs/registry/implementation-map.yaml:L2897-L2907] |
| 72 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `OpenWaterRiserSpec._closes` | independent | [docs/registry/implementation-map.yaml:L2908-L2917] |
| 73 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `build_open_water_generic_spec` | independent | [docs/registry/implementation-map.yaml:L2918-L2927] |
| 74 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `tension_references` | independent | [docs/registry/implementation-map.yaml:L2928-L2938] |
| 75 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `reference_periods` | independent | [docs/registry/implementation-map.yaml:L2939-L2948] |
| 76 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `release_reference` | independent | [docs/registry/implementation-map.yaml:L2949-L2960] |
| 77 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `ring_vertical_residual_n` | independent | [docs/registry/implementation-map.yaml:L2961-L2970] |
| 78 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `open_water_physical_checks` | independent | [docs/registry/implementation-map.yaml:L2971-L2980] |
| 79 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `Tensioners._failed` | independent | [docs/registry/implementation-map.yaml:L2981-L2990] |
| 80 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `RiserGlobalModelSpec._geometry_closes` | independent | [docs/registry/implementation-map.yaml:L2991-L3000] |
| 81 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `_missing_open_water` | independent | [docs/registry/implementation-map.yaml:L3001-L3010] |
| 82 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `missing_channels` | independent | [docs/registry/implementation-map.yaml:L3011-L3022] |
| 83 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract` | independent | [docs/registry/implementation-map.yaml:L3023-L3032] |
| 84 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract_open_water` | independent | [docs/registry/implementation-map.yaml:L3033-L3042] |
| 85 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `t_min` | independent | [docs/registry/implementation-map.yaml:L3043-L3052] |
| 86 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tension_setting_max` | independent | [docs/registry/implementation-map.yaml:L3053-L3062] |
| 87 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tensioner_stroke` | independent | [docs/registry/implementation-map.yaml:L3063-L3072] |
| 88 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `ScheduleAssembly.minimum_top_tension_16q_kn` | independent | [docs/registry/implementation-map.yaml:L3073-L3082] |
| 89 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `_load_oc_assembly` | independent | [docs/registry/implementation-map.yaml:L3083-L3092] |
| 90 | `src/digitalmodel/drilling_riser/stackup.py` | `effective_tension` | independent | [docs/registry/implementation-map.yaml:L3093-L3102] |
| 91 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `parse_snapshots` | independent | [docs/registry/implementation-map.yaml:L3103-L3112] |
| 92 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `snapshot_to_tension_n` | independent | [docs/registry/implementation-map.yaml:L3113-L3122] |
| 93 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `router` | independent | [docs/registry/implementation-map.yaml:L3123-L3135] |
| 94 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_section_result` | independent | [docs/registry/implementation-map.yaml:L3136-L3145] |
| 95 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_effective_tension_n` | independent | [docs/registry/implementation-map.yaml:L3146-L3155] |
| 96 | `src/digitalmodel/drilling_riser/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L3156-L3168] |
| 97 | `src/digitalmodel/drilling_riser/workflow.py` | `_write_profile_csv` | independent | [docs/registry/implementation-map.yaml:L3169-L3178] |
| 98 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_to_shear7_result` | independent | [docs/registry/implementation-map.yaml:L3179-L3188] |
| 99 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | independent | [docs/registry/implementation-map.yaml:L3189-L3198] |
| 100 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | independent | [docs/registry/implementation-map.yaml:L3199-L3208] |
| 101 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_fatigue_summary_result` | independent | [docs/registry/implementation-map.yaml:L3209-L3218] |
| 102 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_fatigue_damage_results_to_df` | independent | [docs/registry/implementation-map.yaml:L3219-L3231] |
| 103 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L3232-L3241] |
| 104 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L3242-L3251] |
| 105 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | independent | [docs/registry/implementation-map.yaml:L3252-L3264] |
| 106 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | independent | [docs/registry/implementation-map.yaml:L3265-L3274] |
| 107 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | wrapper | [docs/registry/implementation-map.yaml:L3275-L3287] |
| 108 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | independent | [docs/registry/implementation-map.yaml:L3288-L3300] |
| 109 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | independent | [docs/registry/implementation-map.yaml:L3301-L3311] |
| 110 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | independent | [docs/registry/implementation-map.yaml:L3312-L3321] |
| 111 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | independent | [docs/registry/implementation-map.yaml:L3322-L3331] |
| 112 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | independent | [docs/registry/implementation-map.yaml:L3332-L3341] |
| 113 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | independent | [docs/registry/implementation-map.yaml:L3342-L3351] |
| 114 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | independent | [docs/registry/implementation-map.yaml:L3352-L3361] |
| 115 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.prepare_stack_up_properties_df` | caller | [docs/registry/implementation-map.yaml:L3362-L3377] |
| 116 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_from_endA_in_df` | independent | [docs/registry/implementation-map.yaml:L3378-L3387] |
| 117 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_shear7_x_by_L_in_df` | independent | [docs/registry/implementation-map.yaml:L3388-L3397] |
| 118 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_mudline_in_df` | independent | [docs/registry/implementation-map.yaml:L3398-L3407] |
| 119 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_MSL_in_df` | independent | [docs/registry/implementation-map.yaml:L3408-L3417] |
| 120 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_inservice_weights_and_tensions` | caller | [docs/registry/implementation-map.yaml:L3418-L3432] |
| 121 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.assign_key_properties_to_df` | independent | [docs/registry/implementation-map.yaml:L3433-L3442] |
| 122 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stack_in_service_weights` | independent | [docs/registry/implementation-map.yaml:L3443-L3452] |
| 123 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_effective_tension` | independent | [docs/registry/implementation-map.yaml:L3453-L3462] |
| 124 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stretch_in_riser` | independent | [docs/registry/implementation-map.yaml:L3463-L3472] |
| 125 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.title` | independent | [docs/registry/implementation-map.yaml:L3473-L3482] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.riser_joints_to_shear7_zones` | independent | [docs/registry/implementation-map.yaml:L3483-L3492] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.add_block_3` | independent | [docs/registry/implementation-map.yaml:L3493-L3502] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_to_shear7_result` | independent | [docs/registry/implementation-map.yaml:L3503-L3512] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | independent | [docs/registry/implementation-map.yaml:L3513-L3522] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | independent | [docs/registry/implementation-map.yaml:L3523-L3532] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | independent | [docs/registry/implementation-map.yaml:L3533-L3542] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_fatigue_damage_results_to_df` | copy | [docs/registry/implementation-map.yaml:L3543-L3555] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L3556-L3565] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | independent | [docs/registry/implementation-map.yaml:L3566-L3575] |
| 135 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py` | `_write_tensions_csv` | independent | [docs/registry/implementation-map.yaml:L3576-L3585] |
| 136 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._line_state` | independent | [docs/registry/implementation-map.yaml:L3586-L3597] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._solve_line` | independent | [docs/registry/implementation-map.yaml:L3598-L3607] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._result` | independent | [docs/registry/implementation-map.yaml:L3608-L3617] |
| 139 | `src/digitalmodel/orcaflex/mooring_design.py` | `solve_catenary` | independent | [docs/registry/implementation-map.yaml:L3618-L3627] |
| 140 | `src/digitalmodel/orcaflex/pipelay_analysis.py` | `JLayConfig.calculate_sagbend` | independent | [docs/registry/implementation-map.yaml:L3628-L3637] |
| 141 | `src/digitalmodel/orcaflex/riser_config.py` | `SCRDesignInput.calculate_geometry` | independent | [docs/registry/implementation-map.yaml:L3638-L3647] |
| 142 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_stroke` | independent | [docs/registry/implementation-map.yaml:L3648-L3657] |
| 143 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_top_tension` | independent | [docs/registry/implementation-map.yaml:L3658-L3667] |
| 144 | `src/digitalmodel/orcaflex/riser_input_schema.py` | `_TopTensionedInput.to_legacy` | independent | [docs/registry/implementation-map.yaml:L3668-L3677] |
| 145 | `src/digitalmodel/orcaflex/viv_screening.py` | `BeamProperties.natural_frequency` | independent | [docs/registry/implementation-map.yaml:L3678-L3687] |
| 146 | `src/digitalmodel/parametric/generate.py` | `_code_check_utilisation` | independent | [docs/registry/implementation-map.yaml:L3688-L3697] |
| 147 | `src/digitalmodel/pipelay/workflow.py` | `run_jlay` | independent | [docs/registry/implementation-map.yaml:L3698-L3707] |
| 148 | `src/digitalmodel/riser_configuration_screen/static.py` | `_lazy_row` | independent | [docs/registry/implementation-map.yaml:L3708-L3717] |
| 149 | `src/digitalmodel/riser_configuration_screen/static.py` | `scr_offsets` | independent | [docs/registry/implementation-map.yaml:L3718-L3727] |
| 150 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_validate` | independent | [docs/registry/implementation-map.yaml:L3728-L3737] |
| 151 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_utilisation` | independent | [docs/registry/implementation-map.yaml:L3738-L3747] |
| 152 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_checks` | independent | [docs/registry/implementation-map.yaml:L3748-L3757] |
| 153 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/groups_builder.py` | `GroupsBuilder._build_from_spec` | independent | [docs/registry/implementation-map.yaml:L3758-L3767] |
| 154 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `LineTypeBuilder.build` | independent | [docs/registry/implementation-map.yaml:L3768-L3777] |
| 155 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.should_generate` | independent | [docs/registry/implementation-map.yaml:L3778-L3787] |
| 156 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.build` | independent | [docs/registry/implementation-map.yaml:L3788-L3797] |
| 157 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `_apply_overrides` | independent | [docs/registry/implementation-map.yaml:L3798-L3807] |
| 158 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.mooring_pre_tension_analysis` | independent | [docs/registry/implementation-map.yaml:L3808-L3819] |
| 159 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.evaluate_line_length_next_iteration` | independent | [docs/registry/implementation-map.yaml:L3820-L3829] |
| 160 | `src/digitalmodel/solvers/orcaflex/opp_range_graph.py` | `OPPRangeGraph.postProcessRange` | independent | [docs/registry/implementation-map.yaml:L3830-L3839] |
| 161 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.postProcessRange` | independent | [docs/registry/implementation-map.yaml:L3840-L3849] |
| 162 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | independent | [docs/registry/implementation-map.yaml:L3850-L3862] |
| 163 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | copy | [docs/registry/implementation-map.yaml:L3863-L3872] |
| 164 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | wrapper | [docs/registry/implementation-map.yaml:L3873-L3885] |
| 165 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | copy | [docs/registry/implementation-map.yaml:L3886-L3898] |
| 166 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | copy | [docs/registry/implementation-map.yaml:L3899-L3909] |
| 167 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | copy | [docs/registry/implementation-map.yaml:L3910-L3919] |
| 168 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | independent | [docs/registry/implementation-map.yaml:L3920-L3929] |
| 169 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | copy | [docs/registry/implementation-map.yaml:L3930-L3939] |
| 170 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | copy | [docs/registry/implementation-map.yaml:L3940-L3949] |
| 171 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | independent | [docs/registry/implementation-map.yaml:L3950-L3959] |
| 172 | `src/digitalmodel/structural/analysis/cli.py` | `check` | independent | [docs/registry/implementation-map.yaml:L3960-L3969] |
| 173 | `src/digitalmodel/structural/analysis/wall_thickness.py` | `WallThicknessAnalyzer.check_combined_loading` | independent | [docs/registry/implementation-map.yaml:L3970-L3979] |
| 174 | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py` | `ApiStd2rdStrategy._check_combined` | independent | [docs/registry/implementation-map.yaml:L3980-L3989] |
| 175 | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py` | `DnvStF101Strategy._check_combined_loading` | independent | [docs/registry/implementation-map.yaml:L3990-L3999] |
| 176 | `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py` | `run_wall_thickness_compare` | independent | [docs/registry/implementation-map.yaml:L4000-L4009] |
| 177 | `src/digitalmodel/structural/analysis/wall_thickness_lookup.py` | `MTLookupGenerator.generate` | independent | [docs/registry/implementation-map.yaml:L4010-L4019] |
| 178 | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `ParametricSweep.run` | independent | [docs/registry/implementation-map.yaml:L4020-L4029] |
| 179 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `PipelinePhase.to_design_loads` | independent | [docs/registry/implementation-map.yaml:L4030-L4039] |
| 180 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `create_standard_phases` | independent | [docs/registry/implementation-map.yaml:L4040-L4049] |
| 181 | `src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py` | `LoadScalingProcessor.process_all_conditions` | independent | [docs/registry/implementation-map.yaml:L4050-L4059] |
| 182 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L4060-L4069] |
| 183 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | independent | [docs/registry/implementation-map.yaml:L4070-L4079] |
| 184 | `src/digitalmodel/structural/fatigue_apps/load_scaling.py` | `LoadScalingProcessor.load_reference_data` | independent | [docs/registry/implementation-map.yaml:L4080-L4089] |
| 185 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L4090-L4099] |
| 186 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L4100-L4109] |
| 187 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_sea_state` | independent | [docs/registry/implementation-map.yaml:L4110-L4119] |
| 188 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `main` | independent | [docs/registry/implementation-map.yaml:L4120-L4129] |
| 189 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py` | `ProductionDataHandlerWithProgress.process_single_analysis` | independent | [docs/registry/implementation-map.yaml:L4130-L4139] |
| 190 | `src/digitalmodel/structural/parametric_coordinator.py` | `ParametricCoordinator._run_wall_thickness` | independent | [docs/registry/implementation-map.yaml:L4140-L4149] |
| 191 | `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | copy | [docs/registry/implementation-map.yaml:L4150-L4159] |
| 192 | `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | copy | [docs/registry/implementation-map.yaml:L4160-L4169] |
| 193 | `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | copy | [docs/registry/implementation-map.yaml:L4170-L4179] |
| 194 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `simple_catenary` | independent | [docs/registry/implementation-map.yaml:L4180-L4189] |
| 195 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `lazy_wave_analysis` | independent | [docs/registry/implementation-map.yaml:L4190-L4199] |
| 196 | `src/digitalmodel/subsea/catenary_riser/lazy_wave.py` | `LazyWaveAnalyzer.analyze` | independent | [docs/registry/implementation-map.yaml:L4200-L4209] |
| 197 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.analyze_riser` | caller | [docs/registry/implementation-map.yaml:L4210-L4222] |
| 198 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_catenary_from_H` | independent | [docs/registry/implementation-map.yaml:L4223-L4232] |
| 199 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_H_from_top_tension` | independent | [docs/registry/implementation-map.yaml:L4233-L4242] |
| 200 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.check_design_limits` | independent | [docs/registry/implementation-map.yaml:L4243-L4252] |
| 201 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `developed_min_radius` | independent | [docs/registry/implementation-map.yaml:L4253-L4262] |
| 202 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `required_effective_ei` | independent | [docs/registry/implementation-map.yaml:L4263-L4272] |
| 203 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `screen_bend_stiffener` | independent | [docs/registry/implementation-map.yaml:L4273-L4285] |
| 204 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `armour_tension_stress` | independent | [docs/registry/implementation-map.yaml:L4286-L4295] |
| 205 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `verify_flexible_pipe` | independent | [docs/registry/implementation-map.yaml:L4296-L4307] |
| 206 | `src/digitalmodel/subsea/pipeline/global_buckling/hobbs_lateral.py` | `effective_driving_force` | independent | [docs/registry/implementation-map.yaml:L4308-L4317] |
| 207 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `RiserLoadCase.to_loads` | independent | [docs/registry/implementation-map.yaml:L4318-L4327] |
| 208 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `run_riser_screening` | independent | [docs/registry/implementation-map.yaml:L4328-L4337] |
| 209 | `src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py` | `StackUpDiagram` | independent | [docs/registry/implementation-map.yaml:L4338-L4347] |
| 210 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_riser_model` | caller | [docs/registry/implementation-map.yaml:L4348-L4362] |
| 211 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.read_riser_input_data` | independent | [docs/registry/implementation-map.yaml:L4363-L4372] |
| 212 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.assign_key_properties_to_self` | independent | [docs/registry/implementation-map.yaml:L4373-L4382] |
| 213 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.get_fea_properties` | independent | [docs/registry/implementation-map.yaml:L4383-L4392] |
| 214 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_tensioner_data` | independent | [docs/registry/implementation-map.yaml:L4393-L4402] |
| 215 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.evaluate_stack_up_properties` | independent | [docs/registry/implementation-map.yaml:L4403-L4412] |
| 216 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.save_data` | independent | [docs/registry/implementation-map.yaml:L4413-L4422] |
| 217 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.alter_properties_for_Shear7_software` | independent | [docs/registry/implementation-map.yaml:L4423-L4432] |
| 218 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_natural_frequency` | caller | [docs/registry/implementation-map.yaml:L4433-L4444] |
| 219 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator._frequency_tension_controlled` | independent | [docs/registry/implementation-map.yaml:L4445-L4454] |
| 220 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_effective_tension` | independent | [docs/registry/implementation-map.yaml:L4455-L4464] |
| 221 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.check_stability` | caller | [docs/registry/implementation-map.yaml:L4465-L4476] |
| 222 | `src/digitalmodel/viv_screening/inputs.py` | `_beam` | independent | [docs/registry/implementation-map.yaml:L4477-L4486] |
| 223 | `src/digitalmodel/workflows/workflow_automation/cli.py` | `riser_analysis_cmd` | independent | [docs/registry/implementation-map.yaml:L4487-L4496] |
| 224 | `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_riser_analysis_template` | independent | [docs/registry/implementation-map.yaml:L4497-L4506] |
| 225 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `CompleteRiserAnalysisWorkflow.create` | independent | [docs/registry/implementation-map.yaml:L4507-L4516] |
| 226 | `tests/asset_integrity/test_calculations.py` | `_make_pipe_cfg` | test_helper | [docs/registry/implementation-map.yaml:L4517-L4526] |
| 227 | `tests/capabilities/test_wall_thickness_3d_page.py` | `_study` | test_helper | [docs/registry/implementation-map.yaml:L4527-L4536] |
| 228 | `tests/contracts/test_force_units_boundary_contract.py` | `(module-level)` | test_helper | [docs/registry/implementation-map.yaml:L4537-L4545] |
| 229 | `tests/contracts/test_force_units_boundary_contract.py` | `test_envelope_von_mises_n_to_kn_seam` | test_helper | [docs/registry/implementation-map.yaml:L4546-L4557] |
| 230 | `tests/drilling_riser/global_model/conftest.py` | `synthetic_spec` | test_helper | [docs/registry/implementation-map.yaml:L4558-L4567] |
| 231 | `tests/drilling_riser/global_model/test_batch_params.py` | `test_physical_checks_use_the_remaining_tensioners` | test_helper | [docs/registry/implementation-map.yaml:L4568-L4578] |
| 232 | `tests/drilling_riser/global_model/test_campaign.py` | `_fake` | test_helper | [docs/registry/implementation-map.yaml:L4579-L4589] |
| 233 | `tests/drilling_riser/global_model/test_open_water.py` | `open_water_dict` | test_helper | [docs/registry/implementation-map.yaml:L4590-L4599] |
| 234 | `tests/drilling_riser/global_model/test_open_water_campaign.py` | `_doc` | test_helper | [docs/registry/implementation-map.yaml:L4600-L4609] |
| 235 | `tests/drilling_riser/global_model/test_orcaflex_solver.py` | `test_statics_tension_and_modes_agree_with_hand_checks` | test_helper | [docs/registry/implementation-map.yaml:L4610-L4623] |
| 236 | `tests/drilling_riser/global_model/test_statics_path.py` | `test_branch_tolerance_and_ring_buoyancy` | test_helper | [docs/registry/implementation-map.yaml:L4624-L4634] |
| 237 | `tests/drilling_riser/global_model/test_tensioner_representation.py` | `_vertical` | test_helper | [docs/registry/implementation-map.yaml:L4635-L4644] |
| 238 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_mc` | test_helper | [docs/registry/implementation-map.yaml:L4645-L4654] |
| 239 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_case` | test_helper | [docs/registry/implementation-map.yaml:L4655-L4666] |
| 240 | `tests/drilling_riser/test_drive_off_worked_example.py` | `_recorded_arguments` | test_helper | [docs/registry/implementation-map.yaml:L4667-L4676] |
| 241 | `tests/drilling_riser/test_schedule_assembly.py` | `_deltas` | test_helper | [docs/registry/implementation-map.yaml:L4677-L4686] |
| 242 | `tests/infrastructure/unit/test_pipe_capacity_codes.py` | `_base_cfg` | test_helper | [docs/registry/implementation-map.yaml:L4687-L4696] |
| 243 | `tests/orcaflex/test_generic_scr_slwr.py` | `test_analytical_reference_reproduces_from_basis` | test_helper | [docs/registry/implementation-map.yaml:L4697-L4706] |
| 244 | `tests/parametric/test_atlas.py` | `_code_check_atlas` | test_helper | [docs/registry/implementation-map.yaml:L4707-L4716] |
| 245 | `tests/reporting/test_riser_pipe_comparison.py` | `reference_input` | test_helper | [docs/registry/implementation-map.yaml:L4717-L4726] |
| 246 | `tests/riser_configuration_screen/test_workflow.py` | `settings` | test_helper | [docs/registry/implementation-map.yaml:L4727-L4736] |
| 247 | `tests/solvers/orcaflex/analysis/scripts/verify_configs.py` | `verify_configuration` | test_helper | [docs/registry/implementation-map.yaml:L4737-L4746] |
| 248 | `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py` | `_make_slay_spec` | test_helper | [docs/registry/implementation-map.yaml:L4747-L4756] |
| 249 | `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` | `_make_pipeline_spec` | test_helper | [docs/registry/implementation-map.yaml:L4757-L4766] |
| 250 | `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` | `_make_pipeline_spec` | test_helper | [docs/registry/implementation-map.yaml:L4767-L4776] |
| 251 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_slay_campaign` | test_helper | [docs/registry/implementation-map.yaml:L4777-L4786] |
| 252 | `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py` | `_make_base_spec_data` | test_helper | [docs/registry/implementation-map.yaml:L4787-L4796] |
| 253 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` | `extract_static_range_results` | test_helper | [docs/registry/implementation-map.yaml:L4797-L4806] |
| 254 | `tests/solvers/orcaflex/modular_generator/test_post_validator.py` | `_make_valid_model` | test_helper | [docs/registry/implementation-map.yaml:L4807-L4816] |
| 255 | `tests/solvers/orcaflex/test_slay_layout.py` | `test_support_types_are_defined_before_vessel_references` | test_helper | [docs/registry/implementation-map.yaml:L4817-L4826] |
| 256 | `tests/specialized/umbilical_analysis/conftest.py` | `installation_parameters` | test_helper | [docs/registry/implementation-map.yaml:L4827-L4836] |
| 257 | `tests/subsea/catenary_riser/test_catenary.py` | `run_catenary_riser` | test_helper | [docs/registry/implementation-map.yaml:L4837-L4846] |
| 258 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_catenary_analysis_mock` | test_helper | [docs/registry/implementation-map.yaml:L4847-L4856] |
| 259 | `tests/test_automation/scripts/batch_fix_tests.py` | `(module-level)` | test_helper | [docs/registry/implementation-map.yaml:L4857-L4865] |
| 260 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | test_helper | [docs/registry/implementation-map.yaml:L4866-L4875] |
| 261 | `tests/test_rainflow_with_effective_tension.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L4876-L4887] |
| 262 | `tests/test_wall_thickness_compare_workflow.py` | `_cfg` | test_helper | [docs/registry/implementation-map.yaml:L4888-L4897] |
| 263 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper | [docs/registry/implementation-map.yaml:L4898-L4907] |
| 264 | `tests/workflows/test_viv_screening_workflow.py` | `screening_input` | test_helper | [docs/registry/implementation-map.yaml:L4908-L4917] |

Table 11. All implementation-map rows of the sole counted family, in map order; a map row is not a verified solver result.

## 10. Coverage and open items

Read scope: both package Python files in full; upstream `orcaflex/pipelay_analysis.py`; both J-lay/reel-lay example inputs; the complete five-test workflow file and upstream test file; routing metadata; and the module-specific coverage section. [src/digitalmodel/pipelay/__init__.py:L1-L10] [src/digitalmodel/pipelay/workflow.py:L1-L203] [src/digitalmodel/orcaflex/pipelay_analysis.py:L1-L324] [tests/pipelay/test_pipelay_workflow.py:L1-L122] [tests/orcaflex/test_pipelay_analysis.py:L1-L148] [examples/workflows/jlay-pipelay/input.yml:L1-L38] [examples/workflows/reel-lay-pipelay/input.yml:L1-L40]

Documentation discovery read includes `docs/maps/digitalmodel-operator-map.md`, `docs/registry/module-routing.yaml`, `docs/registry/implementation-map.md`, `docs/registry/implementation-map.yaml`, and `docs/domains/pipeline-installation/coverage-2026-10.md`; generic pipelay discussion also occurs in the migration catalog and research overview, without establishing independent results for this workflow. [docs/maps/digitalmodel-operator-map.md:L94-L94] [docs/registry/module-routing.yaml:L460-L465] [docs/registry/implementation-map.md:L1-L49] [docs/domains/pipeline-installation/coverage-2026-10.md:L171-L181] [docs/migration/legacy-engineering-flow-catalog.md:L27-L27]

| Open item / assigned role | Evidence / limitation | Tracking |
|---|---|---|
| Input validation and design-basis completeness / package maintainer | Silent key filtering, defaults, bare text conversion and absent finite/unit/geometry checks; S01–S08, S10–S15, S19–S25, S27 | issue pending (1) [src/digitalmodel/pipelay/workflow.py:L42-L77] [src/digitalmodel/pipelay/workflow.py:L108-L118] [src/digitalmodel/pipelay/workflow.py:L164-L203] |
| Strict finite JSON / package maintainer | Infinite utilisation/radius and permissive serializer; S09, S16–S18, S26 | issue pending (2) [src/digitalmodel/pipelay/workflow.py:L124-L157] [src/digitalmodel/pipelay/workflow.py:L202-L202] |
| UNVERIFIED: independent J-lay/reel-lay physical accuracy / installation quality manager | Zero independent workflow benchmarks; unsupported ovalisation wording; upstream near-vertical fallback and force/geometry closure need qualification | issue pending (3) [tests/pipelay/test_pipelay_workflow.py:L29-L122] [src/digitalmodel/pipelay/workflow.py:L15-L19] [src/digitalmodel/orcaflex/pipelay_analysis.py:L231-L259] |
| UNVERIFIED: exact governing standard edition/clause and output provenance / installation quality manager | DNV-OS-F101 edition absent; clause text not checked against the licensed copy; no emitted Citation sidecar | #1817 [src/digitalmodel/pipelay/workflow.py:L21-L24] [src/digitalmodel/pipelay/workflow.py:L143-L161] |
| Native stages, hook/sling loads, dynamics and signed report appendix / installation quality manager | JSON screen does not supply native installation qualification; future model/load route and gate evidence remain outside this package | #2431 [src/digitalmodel/pipelay/workflow.py:L164-L203] |
| Family-map reconciliation / map maintainer | One in-package counted row; local reel/residual and operability coverage remains a follow-up | issue pending (4) [docs/registry/implementation-map.yaml:L3698-L3708] [src/digitalmodel/pipelay/workflow.py:L49-L59] [src/digitalmodel/pipelay/workflow.py:L96-L161] |
| UNVERIFIED: deputy, shared-log and clearance index / maintainer | No module-local assignment or lesson index is established; section 8 records the required disposition | issue pending (5) [src/digitalmodel/pipelay/__init__.py:L1-L10] [docs/registry/module-routing.yaml:L460-L465] |
| Upstream expected failure / upstream maintainer | S-lay depth sweep expected failure, not a regression of this package | #1817 [tests/orcaflex/test_pipelay_analysis.py:L69-L78] |

Table 12. Open items; numbered draft references require issue creation and substitution before independent acceptance of issue coverage.

The private sidecar is outside this pack's read/write scope; no private dataset or client-derived value is included. The calculation-citation contract was read for the provenance assessment; no formula, source module, standard value or package-data rule is changed by this documentation.
