# Hull-girder screening knowledge pack

| Header | Value |
| --- | --- |
| Module / repository | `hull_girder_screening` / `digitalmodel` |
| Package | `src/digitalmodel/hull_girder_screening` |
| Pack revision / date | 0 / 2026-10-18 |
| Code pin | `c099a5e1317e6bed63d876252d87cbb12c774372` |
| Analysis family | Engineering calculations (`EC`); AQG r0, section 10, Python calculations |
| Quality role / status | Engineering calculations quality manager / `DRAFT` |

## 1. Purpose and scope

The routed workflow builds still-water shear force and bending moment, compares frame demands with supplied allowable curves, and optionally adds wave envelopes and deck/keel section-modulus screening. [src/digitalmodel/hull_girder_screening/workflow.py:L131-L189]

This is a screening calculation; class-compliance conclusions require an approved loading instrument or endorsed calculation, and allowable curves remain supplied inputs. [src/digitalmodel/hull_girder_screening/workflow.py:L10-L13]

The registry records maturity `development`; the public package exports `router`, and the engine dispatches the matching basename to it. [docs/registry/module-routing.yaml:L511-L516] [src/digitalmodel/hull_girder_screening/__init__.py:L1-L5] [src/digitalmodel/engine.py:L513-L518]

Scope includes both Python files in the package and the delegated calculation path; the surrounding naval-architecture package retains its separate scope, including ultimate strength outside this router's section-modulus call. [src/digitalmodel/hull_girder_screening/__init__.py:L1-L5] [src/digitalmodel/hull_girder_screening/workflow.py:L390-L440] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L303-L347]

## 2. Inputs and outputs

Coordinates run from the aft end forward; bending moment is positive for hogging and negative for sagging, and the load curve is weight minus buoyancy. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L22-L34]

Inputs are a dictionary under `hull_girder_screening`; numerical parsing uses Python conversions without locale normalization. [src/digitalmodel/hull_girder_screening/workflow.py:L131-L151] [src/digitalmodel/hull_girder_screening/workflow.py:L272-L353]

| Input | Unit | Range / default / source |
| --- | --- | --- |
| `length_m`, `n_stations` | m, count | Length required and compared with zero; station count converted to integer, minimum 11, default 401; finite/integer validation gap in section 6. [src/digitalmodel/hull_girder_screening/workflow.py:L131-L137] [src/digitalmodel/hull_girder_screening/workflow.py:L536-L544] |
| `weights.lightship`, `tanks` | t, m | Required weights mapping; distributed weights nonnegative, increasing extent, LCG in middle third; LCG defaults to extent midpoint. [src/digitalmodel/hull_girder_screening/workflow.py:L272-L299] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L91-L119] |
| `weights.point_weights` | t, m | Optional; nonnegative weight, position and optional extent; extent defaults to one grid interval and is clipped at hull ends. [src/digitalmodel/hull_girder_screening/workflow.py:L288-L295] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L136-L149] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L423-L430] |
| `buoyancy.method` | enum | Required mapping: `box`, `hydrostatics_table`, or `direct`; table needs at least 2 stations; direct curve needs matching position/value lengths of at least 2. [src/digitalmodel/hull_girder_screening/workflow.py:L302-L336] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L568-L573] |
| Table stations: `x_m`, `drafts_m`, `areas_m2`; density | m, m, m², t/m³ | Draft/area lists matching and at least 2 points, increasing drafts; density defaults to 1.025; unit metadata is not parsed. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L153-L181] [src/digitalmodel/hull_girder_screening/workflow.py:L317-L326] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L80-L85] |
| Direct `x_m`, `buoyancy_per_m_t` | m, t/m | Interpolated onto the integration grid; no monotonicity check in the direct builder. [src/digitalmodel/hull_girder_screening/workflow.py:L327-L332] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L568-L573] |
| Closure correction / tolerance | boolean, fraction | Default true / 0.005; raw shear divided by displacement and raw moment divided by displacement times length determine closure status. [src/digitalmodel/hull_girder_screening/workflow.py:L143-L153] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L579-L615] |
| Still-water and `total_*` allowables | t, t·m | Optional positive magnitudes at increasing knots; missing negative curve copies the positive curve; total curves require wave inputs. [src/digitalmodel/hull_girder_screening/workflow.py:L155-L181] [src/digitalmodel/hull_girder_screening/workflow.py:L339-L353] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L184-L214] |
| `frames`, `utilization_limit` | m, dimensionless | Frames supplied or union of still-water allowable knots, otherwise 21 positions; limit defaults to 1.0 and is only converted to float. [src/digitalmodel/hull_girder_screening/workflow.py:L163-L167] [src/digitalmodel/hull_girder_screening/workflow.py:L356-L371] |
| `wave_loads` | m, dimensionless | Optional beam and block coefficient passed through the positive-value helper; rule length defaults to integration length; applicability and rule-adjusted coefficient are returned. [src/digitalmodel/hull_girder_screening/workflow.py:L374-L387] [src/digitalmodel/hull_girder_screening/workflow.py:L242-L250] |
| `section_modulus` | MPa, m³, m², m⁴, m | Optional; yield and exactly one of approved moduli or scantlings; explicit area defaults own inertia to zero; an explicit section wave mapping takes precedence over top-level wave inputs. [src/digitalmodel/hull_girder_screening/workflow.py:L390-L458] |
| Output directory | path | Default `results`, relative to configuration directory, configuration file directory, or working directory; filenames use the input stem. [src/digitalmodel/hull_girder_screening/workflow.py:L547-L565] |

| Output | Unit / representation | Contents and conditions |
| --- | --- | --- |
| Station CSV / returned `stations` | m, t/m, t, t·m | Position, weight, buoyancy, shear, moment; wave and total columns added when wave inputs exist. [src/digitalmodel/hull_girder_screening/workflow.py:L191-L217] |
| Utilisation CSVs / returned rows | m, t, t·m, dimensionless, status | Still-water frame rows always written; total CSV only when total rows are nonempty. [src/digitalmodel/hull_girder_screening/workflow.py:L218-L241] |
| Summary CSV / returned summary | t, m, t·m, kN, kN·m, dimensionless | Extremes, locations, closure diagnostics and optional maximum utilisations; force conversion uses gravity 9.81 m/s². [src/digitalmodel/hull_girder_screening/workflow.py:L479-L530] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L80-L85] |
| Returned configuration | dictionary | Replaces the settings block with results, paths, optional wave/section data and governance text; top-level `screening_status` is assigned separately. [src/digitalmodel/hull_girder_screening/workflow.py:L234-L266] |
| CSV serialization | text | Header follows first row key order; empty rows raise; file encoding is not explicitly specified in the writer. [src/digitalmodel/hull_girder_screening/workflow.py:L568-L575] |

## 3. Method and governing standards

| Method | Implementing definition and evidence | Reference / qualification |
| --- | --- | --- |
| Weight builder | `build_weight_curve`: cell-averaged distributed density; point weights spread over an extent. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L390-L431] | Code references longitudinal-strength textbooks without precise editions/pages. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L49-L58] |
| Buoyancy | `buoyancy_box` balances discrete force and moment; `solve_waterline` uses bisection followed by damped Newton iteration; `buoyancy_direct` interpolates. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L454-L573] | Box and table routes compute equilibrium; direct data are supplied by the caller. [src/digitalmodel/hull_girder_screening/workflow.py:L302-L336] |
| Still-water integration | `still_water_sf_bm`: cumulative trapezoidal integrations of q = w − b, then SF; raw closure tested before optional linear buoyancy correction. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L361-L384] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L579-L639] | Closed-form synthetic comparator derived in tests, rather than an approved loading-manual reference. [tests/naval_architecture/test_hull_girder_screening.py:L5-L27] |
| Frame checks | `utilization_at_frames` interpolates demand and sign-matched allowables; `total_utilization_at_frames` checks both wave-sign combinations against their resulting signs. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L645-L680] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L753-L857] | Supplied allowable provenance is required for engineering use. [docs/domains/ship-design/hull-girder-strength-screening.md:L14-L20] |
| Rule waves | `_wave_loads` delegates to `rule_wave_loads`, which reuses coefficient and distribution functions from `hull_girder_strength`. [src/digitalmodel/hull_girder_screening/workflow.py:L374-L387] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L686-L750] | IACS UR S11, *Longitudinal Strength Standard*, Rev.7 (2010), 2.2.1 “Wave bending moment” and 2.2.2 “Wave shear force”; clause text not checked against the licensed copy. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L692-L706] [src/digitalmodel/hull_girder_screening/workflow.py:L242-L250] |
| Section geometry and yield screen | `section_properties` sums area, centroid and parallel-axis inertia; `section_modulus_screen` combines extreme moments and delegates to `section_modulus_check`. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L878-L953] | IACS UR S11 permissible-stress framework; the called yield functions give no edition or clause identifier and carry a string code reference. [src/digitalmodel/naval_architecture/hull_girder_strength.py:L250-L296] |

UNVERIFIED: conformity with the licensed S11 clauses, exact permissible-stress clause, and consistent edition provenance; implementation uses Rev.7 (2010), while the code register lists 2020, and the router returns a string reference rather than a `Citation` sidecar; issue pending (3). [src/digitalmodel/hull_girder_screening/workflow.py:L234-L264] [src/digitalmodel/naval_architecture/hull_girder_strength.py:L250-L296] [docs/domains/codes-register.md:L127-L128]

## 4. Validated worked examples and regression examples

Execution record at the code pin, before and after this documentation change: core/workflow/demo suite 43 passed, strength suite 26 passed; failures 0, skips 0, NOT_RUN 0 in both runs; test commands and node results are recorded below against their source definitions. [tests/naval_architecture/test_hull_girder_screening.py:L72-L109] [tests/naval_architecture/test_hull_girder_screening_workflow.py:L59-L77] [tests/report_pack/test_hull_girder_report_pack_demo.py:L33-L113]

Use `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch-directory>`; paths are the two screening test files, the report-demo test file, and, in a separate command, the strength test file; no addopts override or extra installation was needed in these runs. [docs/registry/module-routing.yaml:L515-L515] [tests/naval_architecture/test_hull_girder_screening.py:L33-L52] [tests/naval_architecture/test_hull_girder_strength.py:L1-L30]

Validated means an independent closed-form expected value, not confirmation of a specific textbook edition or licensed standard; the synthetic continuous-load derivation supplies the independent comparator for the first two rows. [tests/naval_architecture/test_hull_girder_screening.py:L16-L27]

| Case / classification | Source, input and expected result | Criterion / node / BASE and after |
| --- | --- | --- |
| V1 sagging / validated | Synthetic central-half loading: L = 100 m, W = 10,000 t; integrate constant q independently to obtain SF quarter-point magnitude W/4 = 2,500 t and midship BM −WL/16 = −62,500 t·m; input `_sagging_case`. [tests/naval_architecture/test_hull_girder_screening.py:L16-L24] [tests/naval_architecture/test_hull_girder_screening.py:L56-L66] | SF relative 1.0%, BM relative 0.1%; grid-smearing criteria encoded in test; `tests/naval_architecture/test_hull_girder_screening.py::test_sagging_barge_matches_hand_calc`; PASS / PASS. [tests/naval_architecture/test_hull_girder_screening.py:L72-L95] |
| V2 hogging / validated | Mirrored end-quarter weights; BM +WL/16 = +62,500 t·m. [tests/naval_architecture/test_hull_girder_screening.py:L26-L27] | BM relative 0.1%, opposite extreme 0 t·m; `tests/naval_architecture/test_hull_girder_screening.py::test_hogging_barge_sign_convention`; PASS / PASS. [tests/naval_architecture/test_hull_girder_screening.py:L98-L109] |
| V3 trimmed waterline / validated | Closed-form rectangular-waterplane equilibrium; fixture end drafts 3.8 m and 4.2 m, trim 0.4 m. [tests/naval_architecture/test_hull_girder_screening.py:L167-L180] | End-draft relative 0.1%, trim relative 1.0%; `tests/naval_architecture/test_hull_girder_screening.py::test_waterline_solve_recovers_box_drafts`; PASS / PASS. [tests/naval_architecture/test_hull_girder_screening.py:L167-L186] |
| V4 section geometry / validated | Parallel-axis hand calculation for plate elements: area 0.6 m², centroid 25/12 m, inertia about 3.2 m⁴; exact comparison expression retained in test. [tests/naval_architecture/test_hull_girder_screening.py:L383-L407] | Centroid/area relative 1e-9; inertia and moduli relative 1e-4; `tests/naval_architecture/test_hull_girder_screening.py::test_section_properties_box_girder_hand_calc`; PASS / PASS. [tests/naval_architecture/test_hull_girder_screening.py:L395-L407] |
| R1 routed fixture / regression | Router output checked against the synthetic comparator, including converted moment, closure and status; no new independent source beyond V1. [tests/naval_architecture/test_hull_girder_screening_workflow.py:L59-L77] | SF relative 1.0%, BM relative 0.1%; `tests/naval_architecture/test_hull_girder_screening_workflow.py::test_router_matches_golden_fixture`; PASS / PASS. [tests/naval_architecture/test_hull_girder_screening_workflow.py:L59-L77] |
| R2 wave equations / regression | Test repeats the implementation's rule equations; licensed reference and coefficients are not independently checked or reproduced here. [tests/naval_architecture/test_hull_girder_screening.py:L269-L299] | Relative 1e-9 for moment/shear reproductions; `tests/naval_architecture/test_hull_girder_screening.py::test_rule_wave_loads_hand_calc`; PASS / PASS. [tests/naval_architecture/test_hull_girder_screening.py:L269-L299] |
| R3 report data / regression | `screening_input.yml` through `generate(tmp_path)`, compared with committed demo files; workflow-derived golden output, not an independent strength benchmark. [examples/workflows/hull-girder-report-pack/make_demo_data.py:L44-L69] [tests/report_pack/test_hull_girder_report_pack_demo.py:L33-L50] | 0 differing normalized bytes / 0 differing filenames; `tests/report_pack/test_hull_girder_report_pack_demo.py::test_committed_demo_data_matches_regeneration`; PASS / PASS. [tests/report_pack/test_hull_girder_report_pack_demo.py:L33-L50] |

## 5. QA gates

Gate applicability follows AQG r0, sections 9–10: G0 inputs, G1 imports/tests, G2 independent hand checks, G5 evidence; G3 and G4 are NOT_APPLICABLE because this route computes Python screening tables without dynamics or diffraction solves. [src/digitalmodel/hull_girder_screening/workflow.py:L131-L266]

The criteria below are reproducible checks for this pack, not permission to use unqualified allowable curves or an unsigned report for a compliance decision. [src/digitalmodel/hull_girder_screening/workflow.py:L124-L128]

| Gate / check | Function, quantity and numeric pass criterion with unit | Executable evidence / observed result |
| --- | --- | --- |
| G0 / I1 | Router rejects missing weights, unknown buoyancy method and two simultaneous section-modulus sources: 3 expected `ValueError` outcomes (count). | Three rejection tests from `test_router_requires_weights` through `test_router_rejects_double_section_modulus_source`; PASS. [tests/naval_architecture/test_hull_girder_screening_workflow.py:L365-L388] |
| G0 / I2 | Total allowable without wave inputs: 1 `ValueError` outcome (count), message contains `wave_loads`. | `tests/naval_architecture/test_hull_girder_screening_workflow.py::test_router_total_allowables_require_wave_loads`; PASS. [tests/naval_architecture/test_hull_girder_screening_workflow.py:L322-L333] |
| G0 / I3 | Diagnostic reproduction: 9 observations match the values in the reproducible probe below (count); this detects existing limitations rather than approving the inputs. | Probe run at the code pin: 9/9; missing-limit and numeric risks remain in section 6. [src/digitalmodel/hull_girder_screening/workflow.py:L134-L164] [src/digitalmodel/hull_girder_screening/workflow.py:L443-L450] [src/digitalmodel/hull_girder_screening/workflow.py:L467-L476] [src/digitalmodel/hull_girder_screening/workflow.py:L536-L544] [src/digitalmodel/hull_girder_screening/workflow.py:L568-L575] |
| G1 / T1 | Import route is `digitalmodel.hull_girder_screening.workflow.router` under Python; screening/demo plus strength tests: 69 passed, 0 failed (count). No solver target or program-load route is generated by this package. | Section 4 command, with whole screening/demo and strength files; PASS. [tests/naval_architecture/test_hull_girder_screening_workflow.py:L12-L14] [src/digitalmodel/hull_girder_screening/workflow.py:L131-L266] |
| G2 / H1 | `still_water_sf_bm`: sagging BM error ≤0.1% of 62,500 t·m and quarter-point SF error ≤1.0% of 2,500 t; end residuals <1e-6 t and <1e-6 t·m. | V1 node; PASS. [tests/naval_architecture/test_hull_girder_screening.py:L72-L95] |
| G2 / H2 | `buoyancy_hydrostatics_table`: end drafts relative error ≤0.1% in m; trim relative error ≤1.0% in m; normalized shear closure <1e-6 (dimensionless). | V3 node; PASS. [tests/naval_architecture/test_hull_girder_screening.py:L167-L186] |
| G2 / H3 | `section_properties`: centroid/area relative error ≤1e-9 (m / m²); inertia/modulus relative error ≤1e-4 (m⁴ / m³). | V4 node; PASS. [tests/naval_architecture/test_hull_girder_screening.py:L383-L407] |
| G3 | NOT_APPLICABLE: no time-domain solver run in router. | AQG r0, section 10, Python calculations; route evidence. [src/digitalmodel/hull_girder_screening/workflow.py:L131-L266] |
| G4 | NOT_APPLICABLE: no diffraction deck, mesh or solver run in router. | AQG r0, section 10, Python calculations; route evidence. [src/digitalmodel/hull_girder_screening/workflow.py:L131-L266] |
| G5 / E1 | Router writes 3 still-water CSV files; station row count 401; summary BM relative error ≤1e-9 (t·m). | `tests/naval_architecture/test_hull_girder_screening_workflow.py::test_router_writes_report_pack_ready_csvs`; PASS. [tests/naval_architecture/test_hull_girder_screening_workflow.py:L106-L132] |
| G5 / E2 | Report-demo output includes 1 calculation-record appendix and 1 citation code match (count), HTML and manifest evidence; missing optional PDF must give 1 explicit status (count). | `tests/report_pack/test_hull_girder_report_pack_demo.py::test_demo_report_pack_end_to_end`; PASS. A signed AQG record and fingerprint-based release check are not established by this test; issue pending (3). [tests/report_pack/test_hull_girder_report_pack_demo.py:L53-L113] |

I3 reproduction recipe: construct the same synthetic dictionary as the workflow fixture, remove `allowables` and `frames`, direct `output_dir` to scratch, and call `router`; expect top-level status `pass` and frame statuses `{no-allowable}` (two observations); change `n_stations` to 401.9 and expect 401 (one observation); call `_positive_float({'x': 'nan'}, 'x')` and observe NaN, and evaluate `bool('false')` as true (two observations); `_element({'z_m': 1, 'area_m2': 1})` returns own inertia 0 m⁴ (one observation); `_positive_float` rejects `'1,000'` and `'1,5'` with `ValueError` (two observations); `_write_csv(scratch_path, [])` raises `ValueError` (one observation). [tests/naval_architecture/test_hull_girder_screening_workflow.py:L21-L56] [src/digitalmodel/hull_girder_screening/workflow.py:L131-L167] [src/digitalmodel/hull_girder_screening/workflow.py:L443-L450] [src/digitalmodel/hull_girder_screening/workflow.py:L467-L476] [src/digitalmodel/hull_girder_screening/workflow.py:L536-L544] [src/digitalmodel/hull_girder_screening/workflow.py:L568-L575]

## 6. Known failure modes and pitfalls

sweep: 46 found

The sweep inventory below counts distinct input/default/omission sites, including intentional optional results; it does not equate every site with a defect. No record-skipping `continue`, empty-result cache write, or 1970 default occurs in the package; the sole exception handler changes path presentation. [src/digitalmodel/hull_girder_screening/__init__.py:L1-L5] [src/digitalmodel/hull_girder_screening/workflow.py:L131-L583]

Exact searches, followed by reading both package files at the code pin:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|\b(int|float)\(|0\.0|1970|0\.05|cache' src/digitalmodel/hull_girder_screening -g '*.py'
rg -n 'get\(|else \[\]|else None|units|t/m|t·m|kN|density|closure' src/digitalmodel/hull_girder_screening -g '*.py'
```

Rows refer to observations at the code pin on 2026-10-18; “no detection check exists” means no rejection or qualification check was identified, even where a successful-path regression exists. [src/digitalmodel/hull_girder_screening/workflow.py:L131-L583]

| Site | Symptom / cause | Detection / follow-up | Evidence |
| --- | --- | --- | --- |
| S01 | Missing strength checks can yield overall `pass`. | I3 reproduces; issue pending (2). | [src/digitalmodel/hull_girder_screening/workflow.py:L467-L476] |
| S02 | Fractional station count truncates; default 401 may hide omitted resolution. | I3; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L134-L137] |
| S03 | Quoted false text becomes true; correction defaults true. | I3; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L148-L148] |
| S04 | Tolerance converted without range/finiteness gate; default inherited. | no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L149-L151] |
| S05 | Utilisation limit converted without range/finiteness gate; default unity. | no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L164-L167] |
| S06 | Missing lightship/tank groups become empty; only all-empty case rejected. | I1 covers missing mapping; partial omission: no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L273-L298] |
| S07 | Distributed weight text converted as tonnes. | I3 shows locale rejection; complete finite/unit check: no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L282-L282] |
| S08 | Distributed start text converted as metres. | no detection check exists for non-finite/unit input; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L283-L283] |
| S09 | Distributed end text converted as metres. | no detection check exists for non-finite/unit input; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L284-L284] |
| S10 | LCG text converted as metres, or None delegates midpoint. | no detection check exists for unintended default; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L285-L285] |
| S11 | Point mass text converted as tonnes. | no detection check exists for non-finite/unit input; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L291-L291] |
| S12 | Point position text converted as metres. | no detection check exists for non-finite/unit input; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L292-L292] |
| S13 | Point extent text converted, or None delegates grid spacing. | no detection check exists for unintended omission; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L293-L293] |
| S14 | Missing point-weight list becomes empty. | no detection check exists for unintended omission; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L295-L295] |
| S15 | Hydrostatic position text converted as metres. | H2 checks valid equilibrium; invalid values: no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L319-L319] |
| S16 | Hydrostatic draft text converted as metres. | H2 checks valid equilibrium; unit validation: no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L320-L320] |
| S17 | Hydrostatic area text converted as square metres. | H2 checks valid equilibrium; finite/unit gate: no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L321-L321] |
| S18 | Density text converted; seawater default applies without unit declaration. | no detection check exists for finite/positive density or units; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L325-L326] |
| S19 | Direct positions converted; absent list becomes empty and direct builder rejects insufficient lengths. | no detection check exists for unsorted positions; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L330-L330] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L568-L573] |
| S20 | Direct buoyancy text converted as t/m; absent list becomes empty. | no detection check exists for finite/unit input; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L331-L331] |
| S21 | Absent allowable returns None, intentionally omitting its check. | I3; issue pending (2). | [src/digitalmodel/hull_girder_screening/workflow.py:L342-L344] |
| S22 | Allowable positions converted; missing positions become empty. | no detection check exists for non-finite/unit input; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L347-L347] |
| S23 | Positive limits converted; missing values become empty. | no detection check exists for non-finite/unit input; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L348-L348] |
| S24 | Negative limits converted or silently copied from positive limits. | no detection check exists for unintended symmetry; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L349-L353] |
| S25 | Explicit frame text converted as metres. | no detection check exists for non-finite/unit input; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L362-L364] |
| S26 | Absent frames use still-water knots or 21 points; total-only knots are excluded. | no detection check exists for missed total-curve detail; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L365-L371] |
| S27 | Absent wave block returns None. | I2 checks required wave for total limits; intentional optional result. | [src/digitalmodel/hull_girder_screening/workflow.py:L374-L379] |
| S28 | Missing rule length uses integration length. | no detection check exists for wrong rule-length basis; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L382-L387] |
| S29 | Missing section block returns empty list/dictionary. | I3 exercises omission; issue pending (2). | [src/digitalmodel/hull_girder_screening/workflow.py:L393-L395] |
| S30 | Optional keel modulus converted without positive helper; omission removes keel check. | no detection check exists for invalid keel input; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L409-L413] |
| S31 | Missing approved-modulus source labelled unspecified. | no detection check exists for unqualified provenance; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L414-L414] |
| S32 | Optional depth converted, or omitted to infer top from element centroids. | H3 checks explicit depth; omitted-depth qualification: no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L419-L423] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L891-L895] |
| S33 | Section wave block overrides shared inputs; None reuses them. | no detection check exists for inconsistent duplicate bases; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L428-L439] |
| S34 | Missing section-element name becomes generic label. | no detection check exists for lost element identity; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L443-L444] |
| S35 | Element elevation text converted as metres above keel. | H3 checks valid geometry; invalid units: no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L445-L445] |
| S36 | Lumped area text converted as square metres. | no detection check exists for invalid area; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L446-L448] |
| S37 | Own inertia defaults to zero m⁴. | I3 reproduces default; qualification gap: issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L449-L449] |
| S38 | Breadth and thickness text converted as metres. | H3 checks valid section; invalid-unit rejection: no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L451-L454] |
| S39 | Height and thickness text converted as metres. | H3 checks valid section; invalid-unit rejection: no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L455-L458] |
| S40 | Positive helper float conversion accepts NaN because comparison with zero is false. | I3; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L536-L544] |
| S41 | Summary removes None values and reports None for empty maxima. | I3 detects no-limit case; missing coverage qualification: issue pending (2). | [src/digitalmodel/hull_girder_screening/workflow.py:L482-L492] [src/digitalmodel/hull_girder_screening/workflow.py:L519-L527] |
| S42 | Empty CSV output raises instead of writing an empty cache/result. | I3 reproduces 1 rejection; intentional guard. | [src/digitalmodel/hull_girder_screening/workflow.py:L568-L570] |
| S43 | Path relativization catches ValueError and returns an absolute path. | no detection check exists for path disclosure in published output; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L578-L583] |
| S44 | Fixed force conversion and unit conventions are assumed, not parsed. | H1 verifies moments; external unit mismatch: no detection check exists; issue pending (1). | [src/digitalmodel/hull_girder_screening/workflow.py:L87-L88] [src/digitalmodel/hull_girder_screening/workflow.py:L508-L514] |
| S45 | Empty settings, buoyancy, or allowable mappings fall back to dictionaries; required settings/weights/buoyancy are subsequently checked, allowables remain optional. | I1 / I3; allowable qualification gap: issue pending (2). | [src/digitalmodel/hull_girder_screening/workflow.py:L132-L153] [src/digitalmodel/hull_girder_screening/workflow.py:L273-L275] [src/digitalmodel/hull_girder_screening/workflow.py:L305-L307] [src/digitalmodel/hull_girder_screening/workflow.py:L342-L344] |
| S46 | No wave means empty total rows; empty rows suppress total CSV and return None path. | I3 exercises omission; intentional optional output. | [src/digitalmodel/hull_girder_screening/workflow.py:L175-L182] [src/digitalmodel/hull_girder_screening/workflow.py:L227-L232] [src/digitalmodel/hull_girder_screening/workflow.py:L258-L261] |

Additional pitfall: wave-equation agreement and a report citation chip do not establish an independently checked rule edition or a signed QA appendix; E2 detects report structure only, and no detection check exists for complete standards qualification; issue pending (3). [tests/naval_architecture/test_hull_girder_screening.py:L269-L299] [tests/report_pack/test_hull_girder_report_pack_demo.py:L75-L113] [docs/domains/codes-register.md:L127-L128]

## 7. Lessons log

| Date | Lesson | Evidence | Pack change / shared-log entry |
| --- | --- | --- | --- |
| 2026-10-18 | Distinguish equilibrium closure from an evaluated strength limit; default pass omits that distinction. | I3 and status implementation. [src/digitalmodel/hull_girder_screening/workflow.py:L467-L476] | Sections 5–6; shared-log entry to be assigned with issue pending (2). |
| 2026-10-18 | Keep synthetic closed-form validation separate from rule-equation reproduction and regenerated report goldens. | Continuous comparator and demo comparison. [tests/naval_architecture/test_hull_girder_screening.py:L16-L27] [tests/report_pack/test_hull_girder_report_pack_demo.py:L33-L50] | Section 4; shared-log entry to be assigned with issue pending (3). |

## 8. Ownership

Review assignment for this draft: engineering calculations quality manager; deputy to be designated by the maintainer; producing discipline engineering calculations; ambiguous standards clauses, criteria and missing reference data go to the family quality manager, with cross-family matters directed to the maintainers (AQG r0, section 11, sign-off and independence; Module agent lifecycle, revision 0, section 4, escalation).

No clearance is asserted: status remains `DRAFT` pending independent review (AQG r0, section 11, sign-off and independence).

## 9. Dependencies and implementation inventory

| Dependency / consumer | Contract and inventory location |
| --- | --- |
| NumPy, Python standard library | Arrays, CSV serialization, dictionaries and paths used by router; no solver dependency in this route. [src/digitalmodel/hull_girder_screening/workflow.py:L93-L120] [src/digitalmodel/hull_girder_screening/workflow.py:L131-L266] |
| `naval_architecture` | Upstream weight/buoyancy construction, still-water integration, allowable comparisons, wave envelopes and section properties; registered package domain, separate from this package. [src/digitalmodel/hull_girder_screening/workflow.py:L100-L120] [docs/maps/digitalmodel-operator-map.md:L88-L88] |
| Engine | Downstream basename dispatch to router. [src/digitalmodel/engine.py:L513-L518] |
| Report demo / report workflow | Consumer of generated CSVs; synthetic dataset fixture `screening_input.yml`, with regenerated report tables and figure; optional PDF route does not require a solver. [examples/workflows/hull-girder-report-pack/make_demo_data.py:L44-L69] [tests/report_pack/test_hull_girder_report_pack_demo.py:L53-L113] |

A8 audit basis: implementation map read from `origin/codex/implmap-work`, map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; the map header identifies this data revision. [docs/registry/implementation-map.yaml:L1-L5]

Computed A8 inventory: 0 in-package implementation rows across the map's 13 families; module map families 0, counted families 0, section-9 implementation inventory empty; signal grep `NOT_APPLICABLE` because no map family has an implementation row inside the header package. Audit operation: parse that map, select each family's `implementations` whose `path.startswith('src/digitalmodel/hull_girder_screening/')`, preserve map order, count and rank; result 0. The map and package boundaries are the audit inputs. [docs/registry/implementation-map.yaml:L1-L5] [src/digitalmodel/hull_girder_screening/__init__.py:L1-L5] [src/digitalmodel/hull_girder_screening/workflow.py:L100-L120]

Map follow-up, issue pending (4): add the section-3 method families (weight distribution; buoyancy equilibrium/interpolation; still-water integration/closure; allowable utilisation; rule wave envelopes; section properties/yield screening), classify delegated definitions and router helpers, and give each implementation an appropriate package row. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L390-L953] [src/digitalmodel/hull_girder_screening/workflow.py:L131-L266]

Wider repository search record: a case-sensitive Python `re` scan over all 5,702 tracked `.py` files, including `scripts/`, using the following pattern returned 88 matching lines in 9 files, including 0 script hits; it supplies map follow-up findings without changing the counted-family result. The corresponding substantive definitions and helpers are classified below. [src/digitalmodel/naval_architecture/loading_computer.py:L370-L487] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L579-L680] [src/digitalmodel/hull_girder_screening/workflow.py:L131-L266]

```text
still_water_sf_bm|longitudinal_strength|def (?:section_properties|rule_wave_loads|wave_bending_moment)|_cumulative_trapz|shear_force_t|bending_moment_t_m
```

| Definition / match | Classification and difference |
| --- | --- |
| `naval_architecture.hull_girder_screening` methods | Delegated computations; section 3 inventory covers them. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L390-L953] |
| `naval_architecture.hull_girder_strength` wave and yield methods | Underlying calculations reused by the delegated screen, already named in section 3. [src/digitalmodel/naval_architecture/hull_girder_screening.py:L686-L750] [src/digitalmodel/naval_architecture/hull_girder_screening.py:L905-L953] |
| `naval_architecture.loading_computer.longitudinal_strength`, `_cumulative_trapz` | Separate box-hull still-water computation outside the package; uses buoyancy minus weight, opposite load sign from this screen; map gap, issue pending (4). [src/digitalmodel/naval_architecture/loading_computer.py:L370-L399] [src/digitalmodel/naval_architecture/loading_computer.py:L459-L487] |
| `drilling_riser.section.section_properties` | Tubular section computation rather than the midship plate-assembly method; different geometry. [src/digitalmodel/drilling_riser/section.py:L75-L110] |
| Workflow router / `_summary`, report builder | Delegating, combining or exporting helpers; section 3 names the actual computations; not further second implementations. [src/digitalmodel/hull_girder_screening/workflow.py:L139-L189] [src/digitalmodel/hull_girder_screening/workflow.py:L479-L530] [examples/workflows/hull-girder-report-pack/build_report.py:L123-L130] |
| Screening, workflow and loading-computer tests | Comparator or regression evidence rather than production implementations. [tests/naval_architecture/test_hull_girder_screening.py:L72-L109] [tests/naval_architecture/test_hull_girder_screening_workflow.py:L58-L75] [tests/naval_architecture/test_loading_computer.py:L1-L30] |

## 10. Coverage and open items

Read coverage: package `__init__.py` and all of `workflow.py`; delegated screening/strength source; module configuration; all screening/workflow/demo tests and supporting strength tests; report generator and example configuration; routing entry and operator map. [src/digitalmodel/hull_girder_screening/__init__.py:L1-L5] [src/digitalmodel/hull_girder_screening/workflow.py:L1-L583] [src/digitalmodel/base_configs/modules/hull_girder_screening/hull_girder_screening.yml:L1-L8] [docs/registry/module-routing.yaml:L511-L516]

Documentation discovery under `docs/` with `rg -l 'hull_girder_screening' docs -g '*.md' -g '*.rst'` found the operator map, the screening guide and the offering program document; all were read. The strength validation note and code register were also read for standards context. [docs/maps/digitalmodel-operator-map.md:L88-L88] [docs/domains/ship-design/hull-girder-strength-screening.md:L1-L183] [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L21-L21] [docs/domains/hull-girder-strength-validation-2026-06-28.md:L1-L40] [docs/domains/codes-register.md:L127-L128]

Read-only issue search for hull-girder/module, numeric, allowable, citation and implementation-map terms identified feature records #1585, #1592, #1593 and #1082; no incident issue was specified for this package. Those records describe feature scope rather than proving that the section-6 risks are resolved. The cited regression tests cover the wave and report features. [tests/report_pack/test_hull_girder_report_pack_demo.py:L3-L9] [tests/naval_architecture/test_hull_girder_screening_workflow.py:L227-L333]

Test inventory recomputed from offline results: core 23, workflow 18, report-demo 2, supporting strength 26; total 69 module/dependency cases. Contract and routing command additionally passed 497 cases before and after the registry edit; no skipped, failed or NOT_RUN case in either command set. [tests/naval_architecture/test_hull_girder_screening.py:L72-L444] [tests/naval_architecture/test_hull_girder_screening_workflow.py:L58-L399] [tests/report_pack/test_hull_girder_report_pack_demo.py:L33-L113] [tests/naval_architecture/test_hull_girder_strength.py:L1-L30]

| Open item | Responsible role / reference |
| --- | --- |
| Numeric, unit, provenance and resolution assumptions, including locale parsing, symmetry, optional defaults, path presentation and omitted section geometry detail. [src/digitalmodel/hull_girder_screening/workflow.py:L272-L458] [src/digitalmodel/hull_girder_screening/workflow.py:L536-L583] | Maintainer; issue pending (1). |
| Missing-limit status qualification and partial checking; closure pass cannot establish strength acceptance. [src/digitalmodel/hull_girder_screening/workflow.py:L467-L476] | Maintainer; issue pending (2). |
| UNVERIFIED claim from section 3: licensed clause conformity, exact permissible-stress clause, edition consistency and citation sidecars; signed/fingerprinted QA evidence and an independently qualified rule example remain unestablished. [docs/domains/codes-register.md:L127-L128] [src/digitalmodel/hull_girder_screening/workflow.py:L234-L264] [tests/report_pack/test_hull_girder_report_pack_demo.py:L75-L113] | Engineering calculations quality manager and maintainer; issue pending (3). |
| Missing map families and the outside-package box-hull implementation; counted grep remains NOT_APPLICABLE until the map has in-package rows. [src/digitalmodel/naval_architecture/loading_computer.py:L370-L487] [src/digitalmodel/hull_girder_screening/workflow.py:L100-L120] | Maintainer; issue pending (4). |

No client-derived values are needed for the synthetic examples; the guide records that example allowables are placeholders and a loading-instrument reference fixture remains planned. No private reference fixture or licensed clause copy was used for this pack. [docs/domains/ship-design/hull-girder-strength-screening.md:L84-L110]
