# VIV screening knowledge pack

| Header | Value |
| --- | --- |
| Module | `viv_screening` |
| Repository | `digitalmodel` |
| Package | `src/digitalmodel/viv_screening` |
| Pack revision | 1 |
| Code revision | `f1d58a2ac0db65e68b5b690f0a609ef4746d114e` |
| Analysis family | Engineering calculations; EC |
| Review role | Engineering calculations quality manager |
| Status | DRAFT |

Repository citations resolve at the code revision above; implementation-map citations in section 9 resolve separately at map revision `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, whose recorded data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`. [docs/registry/implementation-map.yaml:L1-L4]

## 1. Purpose and scope

`router` parses one structure and a scalar current or current-speed envelope, builds modal cross-flow and in-line lock-in flags, estimates annual cross-flow Miner damage, compares screening life with design life times DFF, and writes result tables. [src/digitalmodel/viv_screening/workflow.py:L25-L57]

The engine dispatches `basename: viv_screening` to this router and calls its echo writer afterward; the registered durable workflow is `riser-viv-screening`. [src/digitalmodel/engine.py:L330-L333] [src/digitalmodel/engine.py:L890-L893] [docs/registry/workflows.yaml:L454-L463]

The registry records development maturity; the numerical scope is a uniform beam, envelope current and supplied modal stress, with cross-flow damage only and in-line flags retained as unassessed. [docs/registry/module-routing.yaml:L686-L691] [src/digitalmodel/viv_screening/workflow.py:L173-L191] [src/digitalmodel/viv_screening/workflow.py:L43-L54]

Missing modal stress with lock-in, excessive amplitude, three or more unique modes (CF and IL combined), insufficient frequency coverage and an out-of-window Reynolds number produce escalation reasons and suppress calculated damage and life. [src/digitalmodel/viv_screening/workflow.py:L110-L125] [src/digitalmodel/viv_screening/workflow.py:L34-L54]

## 2. Inputs and outputs

Inputs are mapping fields under `viv_screening`; numeric parsing rejects booleans, nonnumeric text, nonfinite values and violations of the stated lower bounds. [src/digitalmodel/viv_screening/inputs.py:L8-L32] [src/digitalmodel/viv_screening/inputs.py:L88-L125]

| Input | Unit | Allowed range / default | Source and citation |
| --- | --- | --- | --- |
| `design_life_years`, `dff` | year; 1 | Both required and positive | Configuration; [src/digitalmodel/viv_screening/inputs.py:L88-L95] |
| `n_modes` | count | Integer at least 1; default 10 | Configuration; [src/digitalmodel/viv_screening/inputs.py:L92-L95] |
| `structure.length_m`, `outer_diameter_m` | m | Required, positive | Configuration; [src/digitalmodel/viv_screening/inputs.py:L63-L79] |
| `structure.inner_diameter_m` | m | Required, nonnegative and smaller than outer diameter | Configuration; [src/digitalmodel/viv_screening/inputs.py:L63-L79] |
| `structure.youngs_modulus_Pa`, `mass_per_length_kg_m` | Pa; kg/m | Required, positive; mass includes added mass in the core model; independent of `mass_ratio`, without a consistency check (S30) | Configuration; [src/digitalmodel/viv_screening/inputs.py:L63-L85] [src/digitalmodel/orcaflex/viv_screening.py:L153-L158] |
| `structure.effective_tension_N` | N | Required, nonnegative; positive means tensile | Configuration; [src/digitalmodel/viv_screening/inputs.py:L71-L73] [src/digitalmodel/orcaflex/viv_screening.py:L158-L159] |
| `structure.boundary` | enum | Required; `pinned_pinned`, `fixed_fixed`, `fixed_free` or `fixed_pinned`; higher-mode scaling is exact only for `pinned_pinned`; alternative boundaries and tension factors remain unqualified (S29) | Configuration; [src/digitalmodel/viv_screening/inputs.py:L74-L85] [src/digitalmodel/orcaflex/viv_screening.py:L33-L38] |
| `environment.current.speed_m_s` or `profile` | m/s; depth in m | Exactly one; nonnegative speeds, positive maximum; nonempty profile with nonnegative depths | Configuration; [src/digitalmodel/viv_screening/inputs.py:L39-L60] |
| `environment.kinematic_viscosity_m2_s`, `mass_ratio` | m²/s; 1 | Required, positive; `mass_ratio` is independent of beam mass, without a consistency check (S30) | Configuration; [src/digitalmodel/viv_screening/inputs.py:L118-L124] |
| `environment.damping_ratio`, `exposure_fraction` | 1 | Required; damping nonnegative; exposure positive and at most 1 | Configuration; [src/digitalmodel/viv_screening/inputs.py:L102-L105] [src/digitalmodel/viv_screening/inputs.py:L118-L124] |
| `unit_modal_stress_MPa` | MPa | Positive when supplied; absent stress triggers escalation if any mode locks | Configuration; [src/digitalmodel/viv_screening/inputs.py:L106-L110] [src/digitalmodel/viv_screening/workflow.py:L110-L118] |
| `sn_curve.curve`, `sn_curve.environment` | identifiers | Defaults F1 and `seawater_cp`; unsupported selections raise | Configuration / fatigue library; [src/digitalmodel/viv_screening/inputs.py:L111-L117] [src/digitalmodel/viv_screening/workflow.py:L29-L33] |
| `output_dir` | path | Nonempty string; default `results`, relative to configuration directory | Configuration; [src/digitalmodel/viv_screening/workflow.py:L206-L221] |

Depth is checked but only the minimum and maximum profile speeds enter screening; no spatial coordinate frame, depth origin, directional-current sign or oscillation phase enters the calculation. [src/digitalmodel/viv_screening/inputs.py:L39-L60] [src/digitalmodel/viv_screening/workflow.py:L69-L107]

The supplied modal stress is an amplitude at unit displacement ratio; the workflow doubles it when constructing stress range in MPa, with annual cycles independent of design life. [examples/workflows/riser-viv-screening/README.md:L30-L34] [src/digitalmodel/viv_screening/workflow.py:L128-L147]

| Output | Unit / format | Meaning and default / missing behavior | Citation |
| --- | --- | --- | --- |
| `<stem>_viv_screening.csv` | UTF-8 CSV | One row per requested mode; frequency Hz, reduced velocity and amplitude ratio dimensionless, stress range MPa, cycles/year, damage/year | [src/digitalmodel/viv_screening/workflow.py:L69-L107] [src/digitalmodel/viv_screening/workflow.py:L128-L147] [src/digitalmodel/viv_screening/workflow.py:L194-L203] [src/digitalmodel/viv_screening/workflow.py:L230-L243] |
| `<stem>_viv_screening_summary.csv` | UTF-8 CSV | Status, counts, in-line unassessed flag, damage/year, life/year, required life/year and dimensionless DFF margin | [src/digitalmodel/viv_screening/workflow.py:L34-L54] [src/digitalmodel/viv_screening/workflow.py:L194-L203] |
| `<stem>.yml` | UTF-8 YAML | Input/result echo, provenance and output paths; no runtime timestamp; nonrelative echo paths fall back to basename | [src/digitalmodel/viv_screening/workflow.py:L254-L273] |
| Returned `cfg.viv_screening` | Mapping | Lowest-numbered cross-flow mode is critical; absent critical mode is null; escalation leaves damage/life null; zero damage gives infinite life | [src/digitalmodel/viv_screening/workflow.py:L34-L54] [src/digitalmodel/viv_screening/workflow.py:L150-L170] |

## 3. Method and governing standards

The method associations below are source claims; clause text not checked against the licensed copy. [src/digitalmodel/viv_screening/workflow.py:L18-L22] [examples/workflows/riser-viv-screening/README.md:L51-L55]

| Method | Implementation and reference association | Qualification |
| --- | --- | --- |
| Current envelope and reduced velocity | `_current_envelope` supplies speed extrema; `_mode_rows` divides each speed by modal frequency times diameter and uses interval intersections | Workflow criterion; DNV-RP-C205, *Environmental Conditions and Environmental Loads*, 2019, reduced-velocity screening association; [src/digitalmodel/viv_screening/inputs.py:L39-L60] [src/digitalmodel/viv_screening/workflow.py:L60-L107] [src/digitalmodel/viv_screening/workflow.py:L18-L22] |
| Modal frequency | Delegates to `BeamProperties.natural_frequency`; the core calculates tube inertia, bending stiffness, beam frequency and tension correction | DNV-RP-F105, *Free Spanning Pipelines*, 2017, natural-frequency association; the core names equation/section references but supplies no checked clause title; [src/digitalmodel/orcaflex/viv_screening.py:L148-L218] [src/digitalmodel/viv_screening/workflow.py:L69-L77] |
| Reynolds, Strouhal, shedding frequency, stability parameter | Delegates to `VIVScreeningInput` properties and `strouhal_number` | DNV-RP-C205, 2019; source names the Strouhal table and stability subsection; table coefficients are intentionally not reproduced; [src/digitalmodel/orcaflex/viv_screening.py:L45-L109] |
| Cross-flow response amplitude | Delegates to `estimate_response_amplitude`; source describes a simplified empirical amplitude curve | DNV-RP-C205, 2019, response-amplitude figure association; source values are not independently qualified here; [src/digitalmodel/orcaflex/viv_screening.py:L294-L332] [src/digitalmodel/viv_screening/workflow.py:L80-L88] |
| Annual fatigue | `_damage_rate` constructs stress range and annual cycles, calls `miner_damage` and sums modal damage; `router` uses reciprocal rate for life and compares against design life times DFF | DNV-RP-C203, *Fatigue Design of Offshore Steel Structures*, edition unspecified in workflow, S-N/Miner association; [src/digitalmodel/viv_screening/workflow.py:L128-L147] [src/digitalmodel/viv_screening/workflow.py:L29-L54] [docs/domains/risers/scr-slwr-coverage-2026-10.md:L50-L55] |
| S-N dependency | `get_sn_curve` constructs a pyLife curve; `miner_damage` obtains allowable cycles and divides applied cycles by allowable cycles | Library heading names 2021 while its data provenance says verified against 2011; the workflow does not resolve this edition distinction; [src/digitalmodel/fatigue/sn_curves.py:L1-L20] [src/digitalmodel/fatigue/sn_curves.py:L80-L99] [src/digitalmodel/fatigue/damage.py:L13-L39] |

The following are implemented screening criteria, not independently qualified standard requirements. [src/digitalmodel/viv_screening/workflow.py:L60-L125]

| Criterion | Implemented condition / calculation | Pinned evidence |
| --- | --- | --- |
| CF lock-in | Reduced-velocity envelope intersects inclusive [4, 8]; governing velocity is clamped to the intersection nearest peak 6.0 | [src/digitalmodel/viv_screening/workflow.py:L60-L81] |
| IL lock-in | Reduced-velocity envelope intersects inclusive [1, 3.5]; governing velocity is clamped to the intersection nearest peak 2.25 | [src/digitalmodel/viv_screening/workflow.py:L60-L81] |
| Stability and CF amplitude | Ks = 2·mass_ratio·damping_ratio; A/D = exp(-0.15·Ks)·max(0, 1 - ((Vr - 6.0)/2.0)²), rounded to 4 decimals; zero outside [4, 8] | [src/digitalmodel/orcaflex/viv_screening.py:L106-L109] [src/digitalmodel/orcaflex/viv_screening.py:L315-L332] |
| T1 | Any locked mode with absent `unit_modal_stress_MPa` escalates | [src/digitalmodel/viv_screening/workflow.py:L110-L114] |
| T2 | Any CF-locked mode with rounded A/D > 0.8 escalates | [src/digitalmodel/viv_screening/workflow.py:L115-L116] |
| T3 | Three or more unique locked modes, counting CF and IL together, escalate; a mode locked in both directions counts once | [src/digitalmodel/viv_screening/workflow.py:L111-L118] |
| T4 | Last checked modal frequency below shedding frequency escalates | [src/digitalmodel/viv_screening/workflow.py:L119-L120] |
| T5 | Reynolds number at maximum envelope speed outside inclusive [300, 1e7] escalates | [src/digitalmodel/viv_screening/inputs.py:L118-L124] [src/digitalmodel/orcaflex/viv_screening.py:L87-L90] [src/digitalmodel/viv_screening/workflow.py:L121-L124] |

UNVERIFIED: exact licensed clause titles, edition equivalence and independent physical accuracy of these standard associations remain unchecked; workflow provenance is a string/mapping rather than a checked `Citation` sidecar, and its C203 edition is absent; issue pending (1). [src/digitalmodel/viv_screening/workflow.py:L18-L22] [src/digitalmodel/viv_screening/workflow.py:L173-L191] [src/digitalmodel/fatigue/sn_curves.py:L1-L20]

## 4. Validated worked examples and regression examples

V1 validates only the zero-tension pinned-beam frequency against a separately evaluated closed form; the synthetic damage and deterministic outputs below are regression evidence, with corpus validation deferred. [tests/workflows/test_durable_workflows.py:L193-L227] [examples/workflows/riser-viv-screening/README.md:L76-L81]

Execution requires UTF-8 mode (`PYTHONUTF8=1`). The focused paths are `tests/workflows/test_viv_screening_workflow.py`, `tests/orcaflex/test_viv_screening.py` and `tests/workflows/test_durable_workflows.py::test_workflow_registry[riser-viv-screening]` (50 + 17 + 1 tests). Contract/routing coverage is `tests/contracts` plus `tests/docs/test_digitalmodel_routing_contract.py` (497 collected tests). Run the workflow tests in a disposable repository copy because the durable node writes example outputs. These counts define the selected checks, rather than guaranteeing that unrelated contract checks pass. [tests/workflows/test_viv_screening_workflow.py:L49-L52] [tests/workflows/test_durable_workflows.py:L54-L86]

| Case / classification | Input and independent or regression basis | Expected quantity and tolerance | Existing test node ID | Offline result at code pin |
| --- | --- | --- | --- | --- |
| V1 / validated component | Synthetic `examples/workflows/riser-viv-screening/input.yml`; independently evaluated closed form `f₁ = π/(2L²) sqrt(EI/m)`, `I = π(OD⁴-ID⁴)/64` | First modal frequency equals the closed form in Hz within `pytest.approx` default relative tolerance 1e-6 or absolute tolerance 1e-12 Hz | `tests/workflows/test_durable_workflows.py::test_workflow_registry[riser-viv-screening]` | PASS; [examples/workflows/riser-viv-screening/input.yml:L10-L17] [tests/workflows/test_durable_workflows.py:L222-L224] [src/digitalmodel/viv_screening/workflow.py:L238-L243] |
| R1 / regression | Same synthetic input; aggregate rate is a golden module output, not independent fatigue validation | `fail`, locked modes `[1, 2]`, positive annual damage matching the stored test literal within default approximate tolerance; reciprocal life in years | `tests/workflows/test_durable_workflows.py::test_workflow_registry[riser-viv-screening]` | PASS; [tests/workflows/test_durable_workflows.py:L203-L221] |
| R2 / regression | Same input, two engine runs | 3 unchanged artifact hashes; 0 temporary-directory strings and 0 `start_time` strings in echo | `tests/workflows/test_viv_screening_workflow.py::test_committed_example_runs_through_engine_and_is_stable` | PASS; [tests/workflows/test_viv_screening_workflow.py:L223-L238] |
| R3 / regression | Test fixture with in-line-only scalar current; supplied modal stress retained | 0 cross-flow flags, 1 in-line flag, 0.0/year cross-flow damage, infinite life in years, `pass` plus unassessed flag | `tests/workflows/test_viv_screening_workflow.py::test_il_only_lock_in_is_unassessed` | PASS; [tests/workflows/test_viv_screening_workflow.py:L241-L251] |

The component check does not qualify tension corrections, alternative boundaries, amplitude response or fatigue life against measurements; the source explicitly labels corpus validation deferred. [tests/workflows/test_durable_workflows.py:L222-L224] [src/digitalmodel/viv_screening/workflow.py:L179-L190]

## 5. QA gates

Review criteria use the Python-calculations family of AQG r0, section 10; G5 uses its evidence requirement in section 9. (AQG r0, *Analysis QA gates*, revision 0, sections 9–10.)

| Gate | Applicability and module-specific checks | Numeric pass criterion and unit | Executable evidence |
| --- | --- | --- | --- |
| G0 | Applicable: C0 required-input and finite-number checks in `parse_inputs` | 0 accepted invalid configurations, count | Run all required-input, invalid-settings, invalid-current and invalid-physics parameterizations; [tests/workflows/test_viv_screening_workflow.py:L99-L180] |
| G1 | Applicable: C1 imports and workflow/core execution | 68 passed tests, 0 failures and 0 skips, count | Full workflow and core files plus the selected durable node from section 4; imports are Python calculations, not a solver load; [tests/workflows/test_viv_screening_workflow.py:L7-L14] [tests/orcaflex/test_viv_screening.py:L7-L15] [docs/registry/workflows.yaml:L454-L463] |
| G1 | Applicable: C2 `_write_outputs` and `save_echo` location contract | 3 files in the requested directory, 0 in the default directory when overridden, count | `tests/workflows/test_viv_screening_workflow.py::test_custom_output_dir_contains_all_workflow_files[relative]` and `[absolute]`; [tests/workflows/test_viv_screening_workflow.py:L281-L306] |
| G2 | Applicable: C3 `BeamProperties.natural_frequency` against independent zero-tension closed form | Frequency discrepancy within relative tolerance 1e-6 or absolute tolerance 1e-12 Hz (`pytest.approx` defaults) | V1 durable node evaluates the closed form separately from production calculations and compares the first CSV modal frequency; [tests/workflows/test_durable_workflows.py:L222-L224] [src/digitalmodel/viv_screening/workflow.py:L238-L243] |
| G3 | NOT_APPLICABLE: no time-marching dynamics in this screening route | NOT_APPLICABLE | Modal calculations and annual-rate assembly only; [src/digitalmodel/viv_screening/workflow.py:L25-L57] [src/digitalmodel/viv_screening/workflow.py:L69-L147] |
| G4 | NOT_APPLICABLE: no diffraction mesh/body calculation in this route | NOT_APPLICABLE | Beam/current and fatigue inputs only; [src/digitalmodel/viv_screening/inputs.py:L63-L125] |
| G5 | Applicable: C4 reproducible result evidence and echo hygiene | 3 identical artifact hashes across 2 runs; 0 timestamp or scratch-path occurrences, count | R2 test; retain test outcomes, code pin and input digest with the evidence bundle; [tests/workflows/test_viv_screening_workflow.py:L223-L238] |

These checks address screening evidence; a complete report appendix and independent corpus qualification remain follow-ups rather than inferred results of passing import or golden tests. [examples/workflows/riser-viv-screening/README.md:L76-L81]

## 6. Known failure modes and pitfalls

sweep: 30 found

The sweep counts 30 reviewed package and analytical-dependency behavior sites below, including intentional sentinels and fail-closed handlers; the count is not a count of confirmed defects. [src/digitalmodel/viv_screening/inputs.py:L8-L125] [src/digitalmodel/viv_screening/workflow.py:L15-L273]

Exact commands used, followed by reading every Python file and its branches:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|default|units|depth_m|SECONDS_PER_YEAR|\.get\(' src/digitalmodel/viv_screening --glob '*.py'
rg -n 'None|0\.0|\[\]|\{\}|write|cache|except|continue|year|MPa|Hz|m_s|_N|_Pa' src/digitalmodel/viv_screening --glob '*.py'
rg -n 'boundary_constants|mode\*\*2|C_t|mass_per_length|mass_ratio|stability_parameter' src/digitalmodel/orcaflex/viv_screening.py
```

| Site | Symptom / cause and disposition | Detection check / tracking | Pinned evidence |
| --- | --- | --- | --- |
| S01 `_mapping` missing-key lookup | Missing or nonmapping input raises rather than silently returning an empty mapping | C0 | [src/digitalmodel/viv_screening/inputs.py:L8-L12] |
| S02 `_number` zero lower bound | Default lower bound is validation policy, not a replacement input value | C0 | [src/digitalmodel/viv_screening/inputs.py:L15-L32] |
| S03 `_number` float conversion and exception | External text uses Python float syntax; booleans, separators not supported by float, and nonfinite values raise; locale normalization is absent | C0 covers nonfinite/bool cases; separator-specific cases: no detection check exists; issue pending (2) | [src/digitalmodel/viv_screening/inputs.py:L18-L31] |
| S04 `_current_envelope` empty profile | Empty profile is rejected, not cached or returned as an empty result | C0 | [src/digitalmodel/viv_screening/inputs.py:L47-L59] |
| S05 profile depth and units | Depth must be nonnegative metres but is otherwise ignored; speed extrema replace spatial shear | C1 exercises profile intersection; depth-origin/shear equivalence: no detection check exists; #809 | [src/digitalmodel/viv_screening/inputs.py:L50-L60] |
| S06 zero current maximum | An all-zero envelope raises; zero entries within a positive envelope are allowed | C0 | [src/digitalmodel/viv_screening/inputs.py:L45-L60] |
| S07 `_beam` boundary lookup / handler | Missing boundary raises; model-validation ValueError is contextualized and re-raised | C0 | [src/digitalmodel/viv_screening/inputs.py:L74-L85] |
| S08 implicit SI / added mass | Geometry, elastic modulus, added-mass-inclusive mass and tension are passed without unit conversion | C3 covers one consistent SI beam; mixed-unit rejection: no detection check exists; #809 | [src/digitalmodel/viv_screening/inputs.py:L63-L85] [src/digitalmodel/orcaflex/viv_screening.py:L153-L158] |
| S09 default mode count | Missing `n_modes` selects 10 and can omit higher-frequency coverage; T4 escalates insufficient coverage | C1 includes `test_unchecked_modes_escalate` | [src/digitalmodel/viv_screening/inputs.py:L92-L95] [src/digitalmodel/viv_screening/workflow.py:L119-L120] |
| S10 absent modal stress | Null stress is retained intentionally; any locked mode then escalates | C1 includes missing-stress and in-line-only missing-stress cases | [src/digitalmodel/viv_screening/inputs.py:L106-L110] [tests/workflows/test_viv_screening_workflow.py:L71-L76] [tests/workflows/test_viv_screening_workflow.py:L195-L208] |
| S11 default S-N mapping / selectors | Missing mapping chooses F1 with cathodic protection; string conversion precedes curve lookup | C1 exercises the defaults; all selector-type/default combinations: no detection check exists; issue pending (2) | [src/digitalmodel/viv_screening/inputs.py:L111-L117] |
| S12 calendar conversion | Annual cycles use the module's fixed seconds-per-year conversion and supplied exposure, without a design-life multiplier | C1 includes annual-rate independence check | [src/digitalmodel/viv_screening/workflow.py:L16-L16] [src/digitalmodel/viv_screening/workflow.py:L134-L135] [tests/workflows/test_viv_screening_workflow.py:L183-L192] |
| S13 curve lookup exception | Unsupported curve selections re-raise with workflow context | no detection check exists for this handler; issue pending (2) | [src/digitalmodel/viv_screening/workflow.py:L29-L33] |
| S14 escalation null rate/life | Out-of-applicability leaves damage/life/margin null instead of guessed results | C1 checks null result and CSV damage fields | [src/digitalmodel/viv_screening/workflow.py:L34-L54] [tests/workflows/test_viv_screening_workflow.py:L55-L68] |
| S15 zero rate / infinite life | Reciprocal life becomes infinity at zero damage; in-line-only response can still return `pass` with unassessed flag | C1 includes R3 and no-lock-in cases; #812 | [src/digitalmodel/viv_screening/workflow.py:L34-L54] [tests/workflows/test_viv_screening_workflow.py:L211-L220] [tests/workflows/test_viv_screening_workflow.py:L241-L251] [docs/domains/risers/scr-slwr-coverage-2026-10.md:L229-L229] |
| S16 `_intersection` null | Disjoint intervals return None; this is a no-lock-in sentinel | C1 exercises no-lock-in and profile cases | [src/digitalmodel/viv_screening/workflow.py:L60-L66] |
| S17 absent cross-flow amplitude | No cross-flow intersection sets amplitude to zero; in-line damage is not calculated | C1 / R3; #812 | [src/digitalmodel/viv_screening/workflow.py:L80-L88] [docs/domains/risers/scr-slwr-coverage-2026-10.md:L229-L229] |
| S18 initial per-mode null values | Stress, cycles and damage start null; escalation preserves them | C1 checks damage columns on escalation; all three fields: no detection check exists; issue pending (2) | [src/digitalmodel/viv_screening/workflow.py:L101-L105] |
| S19 empty escalation list | Empty reasons means applicable under these workflow criteria, not physical validation | C1 checks applicable and T1–T5 cases; #808, #816 | [src/digitalmodel/viv_screening/workflow.py:L110-L125] |
| S20 `_damage_rate` zero accumulator | Total starts at zero before modal accumulation | C1 compares mixed and no-lock-in totals | [src/digitalmodel/viv_screening/workflow.py:L128-L131] |
| S21 per-row zero damage defaults | Applicable rows start with zero stress, cycles and damage before cross-flow assessment | C1 checks totals; per-row zero completeness: no detection check exists; issue pending (2) | [src/digitalmodel/viv_screening/workflow.py:L130-L131] |
| S22 non-cross-flow `continue` | In-line-only rows skip fatigue accumulation; unassessed flag is essential | C1 / R3; #812 | [src/digitalmodel/viv_screening/workflow.py:L132-L133] [docs/domains/risers/scr-slwr-coverage-2026-10.md:L229-L229] |
| S23 zero-stress `continue` | Zero amplitude leaves zero damage without calling Miner calculation | no detection check exists for an exact cross-flow interval endpoint; issue pending (2) | [src/digitalmodel/viv_screening/workflow.py:L134-L140] |
| S24 critical-mode fallback | No cross-flow row gives null critical mode/velocity; first cross-flow row is selected rather than ranked by damage | C1 checks null and mixed-mode cases | [src/digitalmodel/viv_screening/workflow.py:L150-L157] |
| S25 empty reason fallback | Non-escalating result has a null reason, with required=false | C1 checks applicability/status; exact null-reason assertion: no detection check exists; issue pending (2) | [src/digitalmodel/viv_screening/workflow.py:L163-L170] |
| S26 directory/stem fallbacks | Missing directory selects `results`; missing configuration paths select current directory; missing file stem selects basename | C2 tests configured relative/absolute/default locations; current-directory/stem fallbacks: no detection check exists; issue pending (2) | [src/digitalmodel/viv_screening/workflow.py:L206-L227] |
| S27 `_display_path` caught ValueError | Outside-repository result paths become absolute strings in returned mapping/CSV path metadata | C2 exercises external outputs; explicit display policy: no detection check exists; issue pending (2) | [src/digitalmodel/viv_screening/workflow.py:L246-L251] |
| S28 `save_echo` caught ValueError | An output outside the config directory is reduced to its basename in echo; location context can be lost | C4 tests ordinary echo hygiene; external-directory basename fallback: no detection check exists; issue pending (2) | [src/digitalmodel/viv_screening/workflow.py:L254-L265] |
| S29 non-pinned higher-mode scaling / tension factors | Core scales every boundary as n²·C1; zero-tension higher-mode scaling is exact only for `pinned_pinned`; other boundaries can misplace lock-in windows; tension factors 0.9 and 0.95 have no documented derivation | C3 qualifies only the zero-tension pinned first mode; no detection check exists for alternative-boundary higher modes or tension-factor accuracy; issue pending (5) | [src/digitalmodel/orcaflex/viv_screening.py:L172-L218] |
| S30 independent mass inputs | `mass_ratio` controls Ks/amplitude while added-mass-inclusive beam mass controls frequency; inconsistent inputs can silently change screening results because no consistency check exists | C0 validates each positive input separately; no detection check exists for consistency between the two inputs; issue pending (6) | [src/digitalmodel/viv_screening/inputs.py:L63-L85] [src/digitalmodel/viv_screening/inputs.py:L118-L124] [src/digitalmodel/orcaflex/viv_screening.py:L106-L109] [src/digitalmodel/orcaflex/viv_screening.py:L153-L158] |

The package writes CSV and YAML results rather than a result cache; an empty CSV row set raises, and the three validation handlers above re-raise errors instead of swallowing them. [src/digitalmodel/viv_screening/workflow.py:L230-L273] [src/digitalmodel/viv_screening/inputs.py:L25-L26] [src/digitalmodel/viv_screening/inputs.py:L82-L85] [src/digitalmodel/viv_screening/workflow.py:L29-L33]

No incident issue was prescribed; #812 supplies the VIV integration boundary, while missing corpus qualification, schema alignment and report evidence are recorded by the example as #808, #816, #809 and #814; no independent corpus detection check exists in this package. [docs/domains/risers/scr-slwr-coverage-2026-10.md:L229-L229] [examples/workflows/riser-viv-screening/README.md:L76-L81]

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack | In-line-only lock-in may return a screening pass with zero cross-flow damage; preserve the unassessed flag | [tests/workflows/test_viv_screening_workflow.py:L241-L251] | Sections 1, 4 and 6 distinguish the two directions | Index entry pending; issue pending (4) |

## 8. Ownership

Proposed responsibilities for this draft: engineering calculations quality manager reviews the calculation evidence; a deputy from the same family supplies independent review when needed; maintainers resolve publication and indexing. These are review assignments, not a claim of completed review.

UNVERIFIED: a named deputy and a durable shared-log/index entry have not been established from the package or routing entry; issue pending (4). [src/digitalmodel/viv_screening/__init__.py:L1-L1] [docs/registry/module-routing.yaml:L686-L691]

## 9. Dependencies and implementation inventory

Map citations throughout this section refer to `docs/registry/implementation-map.yaml` at revision `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, recorded data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; source citations retain the header code pin. [docs/registry/implementation-map.yaml:L1-L4]

| Dependency / source occurrence | Use and boundary | Source citation |
| --- | --- | --- |
| `inputs.py`, line 5, `digitalmodel.orcaflex.viv_screening` | Imports analytical `BeamProperties` and `VIVScreeningInput`; this is a Python module dependency, with no solver launch | [src/digitalmodel/viv_screening/inputs.py:L5-L5] [src/digitalmodel/orcaflex/viv_screening.py:L7-L20] |
| `workflow.py`, line 12, `digitalmodel.orcaflex.viv_screening` | Imports analytical amplitude function; OrcaFlex-qualified pathname does not select a licensed runtime | [src/digitalmodel/viv_screening/workflow.py:L12-L12] [src/digitalmodel/orcaflex/viv_screening.py:L294-L332] |
| `workflow.py`, line 17, SHEAR7 | Detailed-tool recommendation string only, exported under escalation; no SHEAR7 invocation or version is provided | [src/digitalmodel/viv_screening/workflow.py:L17-L17] [src/digitalmodel/viv_screening/workflow.py:L163-L169] |
| Fatigue core | `get_sn_curve` and `miner_damage` supply pyLife curve/damage results | [src/digitalmodel/viv_screening/workflow.py:L10-L11] [src/digitalmodel/viv_screening/workflow.py:L128-L147] |
| pandas / PyYAML / CSV / filesystem | Build fatigue histogram, write two CSV files and YAML echo | [src/digitalmodel/viv_screening/workflow.py:L128-L147] [src/digitalmodel/viv_screening/workflow.py:L194-L203] [src/digitalmodel/viv_screening/workflow.py:L230-L273] |
| Engine and durable workflow | Engine dispatch and postprocessing consume the package; registered input and expected outputs are synthetic | [src/digitalmodel/engine.py:L330-L333] [src/digitalmodel/engine.py:L890-L893] [docs/registry/workflows.yaml:L454-L463] |

These three import/recommendation locations exhaust case-insensitive `orcaflex|shear7` occurrences in the package's pinned Python source; all were read as text, and no licensed solver or interface was used. [src/digitalmodel/viv_screening/inputs.py:L1-L125] [src/digitalmodel/viv_screening/workflow.py:L1-L273] [src/digitalmodel/viv_screening/__init__.py:L1-L1]

### Counted-family search and classification

The literal search-signal definitions come from the same pinned map for fatigue, effective tension and text writers respectively. [docs/registry/implementation-map.yaml:L16807-L16812] [docs/registry/implementation-map.yaml:L2139-L2144] [docs/registry/implementation-map.yaml:L4933-L4938]

| Rank | Family | In-package rows | All map rows | Package line/signal hits | Enclosing definitions and classification |
| --- | --- | --- | --- | --- | --- |
| 1 | `sn-curves-and-miner-damage` | 4 | 392 | 19 | `parse_inputs`, `router`, `_damage_rate`, `_provenance` are inventoried; module-level hits are imports/metadata; [src/digitalmodel/viv_screening/inputs.py:L88-L125] [src/digitalmodel/viv_screening/workflow.py:L10-L54] [src/digitalmodel/viv_screening/workflow.py:L128-L147] [src/digitalmodel/viv_screening/workflow.py:L173-L191] |
| 2 | `effective-tension-riser-stack-up` | 1 | 264 | 4 | `_beam` is inventoried and parses supplied tension, without recomputing effective tension; [src/digitalmodel/viv_screening/inputs.py:L63-L85] |
| 3 | `orcaflex-text-model-writers` | 1 | 440 | 5 | `save_echo` is inventoried; module-level hits import analytical helpers; `_write_csv` serializes listed workflow results and is a helper; [src/digitalmodel/viv_screening/inputs.py:L5-L5] [src/digitalmodel/viv_screening/workflow.py:L12-L12] [src/digitalmodel/viv_screening/workflow.py:L230-L273] |

Ranking uses descending in-package row count, retaining map order for the tied families. Each `search_signals[].pattern` was used literally with Python `re`, case-sensitive; 0 signals matched package-relative `viv_screening` case-insensitively, so 0 were omitted. Hits were assigned to the smallest enclosing function/class, with module-level statements grouped as one definition. The exhaustive map row blocks below supply the counted-family inventory and signal definitions.

Self-check result: 0 further in-package second implementations across the three counted families; the additional `_write_csv` hit serializes data and does not build a solver model. [src/digitalmodel/viv_screening/workflow.py:L230-L243]

The whole-repository search included 5,727 tracked Python files, including `scripts/`; line/signal hit counts in `scripts/` were 237 for fatigue, 135 for effective tension and 3,103 for text writers. These broad discovery counts include helpers, tests and documentation and are not counts of independent implementations. External discoveries never change the package miss count.

Reproduction outline for the counted search (use the map revision above and the header code pin; no signal normalization):

```python
import ast, re, subprocess, yaml
map_text = subprocess.check_output([
    "git", "show",
    "3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:docs/registry/implementation-map.yaml"
], text=True)
families = yaml.safe_load(map_text)["families"]
package = "src/digitalmodel/viv_screening/"
counted = [f for f in families if any(
    row["path"].startswith(package) for row in f["implementations"])]
counted.sort(key=lambda f: -sum(
    row["path"].startswith(package) for row in f["implementations"]))
# For each of counted[:3], compile each unchanged pattern with re.compile.
# Omit only re.search(pattern, "viv_screening", re.I) matches.
# Search pinned .py lines, then use AST lineno/end_lineno for enclosing definitions.
```

### Map follow-up

Reduced-velocity envelope screening, modal-frequency assessment and response-amplitude estimation lack a dedicated map family with rows in this package; section 3 records their functions and dependencies. Parser/provenance rows and the echo-writer description need relation corrections; issue pending (3). [src/digitalmodel/viv_screening/workflow.py:L60-L107] [src/digitalmodel/viv_screening/workflow.py:L173-L191] [src/digitalmodel/viv_screening/inputs.py:L63-L125]

The full inventory below lists every map row for the three families, including rows outside the package. Family heading plus one-based row number and exact pinned map block uniquely identify each path and symbol. External rows use these reference identities to avoid copying identifying discovery names into this pack; in-package identities are written explicitly. This is an inventory of the map's assertions, not an endorsement of its independence classifications.

### sn-curves-and-miner-damage

| Map row | Implementation identity | Pinned map citation |
| --- | --- | --- |
| 1 | External implementation, row 1 | [docs/registry/implementation-map.yaml:L16814-L16823] |
| 2 | External implementation, row 2 | [docs/registry/implementation-map.yaml:L16824-L16833] |
| 3 | External implementation, row 3 | [docs/registry/implementation-map.yaml:L16834-L16843] |
| 4 | External implementation, row 4 | [docs/registry/implementation-map.yaml:L16844-L16853] |
| 5 | External implementation, row 5 | [docs/registry/implementation-map.yaml:L16854-L16867] |
| 6 | External implementation, row 6 | [docs/registry/implementation-map.yaml:L16868-L16879] |
| 7 | External implementation, row 7 | [docs/registry/implementation-map.yaml:L16880-L16889] |
| 8 | External implementation, row 8 | [docs/registry/implementation-map.yaml:L16890-L16899] |
| 9 | External implementation, row 9 | [docs/registry/implementation-map.yaml:L16900-L16909] |
| 10 | External implementation, row 10 | [docs/registry/implementation-map.yaml:L16910-L16919] |
| 11 | External implementation, row 11 | [docs/registry/implementation-map.yaml:L16920-L16929] |
| 12 | External implementation, row 12 | [docs/registry/implementation-map.yaml:L16930-L16945] |
| 13 | External implementation, row 13 | [docs/registry/implementation-map.yaml:L16946-L16955] |
| 14 | External implementation, row 14 | [docs/registry/implementation-map.yaml:L16956-L16965] |
| 15 | External implementation, row 15 | [docs/registry/implementation-map.yaml:L16966-L16975] |
| 16 | External implementation, row 16 | [docs/registry/implementation-map.yaml:L16976-L16985] |
| 17 | External implementation, row 17 | [docs/registry/implementation-map.yaml:L16986-L16995] |
| 18 | External implementation, row 18 | [docs/registry/implementation-map.yaml:L16996-L17005] |
| 19 | External implementation, row 19 | [docs/registry/implementation-map.yaml:L17006-L17015] |
| 20 | External implementation, row 20 | [docs/registry/implementation-map.yaml:L17016-L17025] |
| 21 | External implementation, row 21 | [docs/registry/implementation-map.yaml:L17026-L17035] |
| 22 | External implementation, row 22 | [docs/registry/implementation-map.yaml:L17036-L17045] |
| 23 | External implementation, row 23 | [docs/registry/implementation-map.yaml:L17046-L17055] |
| 24 | External implementation, row 24 | [docs/registry/implementation-map.yaml:L17056-L17065] |
| 25 | External implementation, row 25 | [docs/registry/implementation-map.yaml:L17066-L17075] |
| 26 | External implementation, row 26 | [docs/registry/implementation-map.yaml:L17076-L17085] |
| 27 | External implementation, row 27 | [docs/registry/implementation-map.yaml:L17086-L17095] |
| 28 | External implementation, row 28 | [docs/registry/implementation-map.yaml:L17096-L17112] |
| 29 | External implementation, row 29 | [docs/registry/implementation-map.yaml:L17113-L17122] |
| 30 | External implementation, row 30 | [docs/registry/implementation-map.yaml:L17123-L17132] |
| 31 | External implementation, row 31 | [docs/registry/implementation-map.yaml:L17133-L17145] |
| 32 | External implementation, row 32 | [docs/registry/implementation-map.yaml:L17146-L17154] |
| 33 | External implementation, row 33 | [docs/registry/implementation-map.yaml:L17155-L17163] |
| 34 | External implementation, row 34 | [docs/registry/implementation-map.yaml:L17164-L17173] |
| 35 | External implementation, row 35 | [docs/registry/implementation-map.yaml:L17174-L17183] |
| 36 | External implementation, row 36 | [docs/registry/implementation-map.yaml:L17184-L17193] |
| 37 | External implementation, row 37 | [docs/registry/implementation-map.yaml:L17194-L17203] |
| 38 | External implementation, row 38 | [docs/registry/implementation-map.yaml:L17204-L17213] |
| 39 | External implementation, row 39 | [docs/registry/implementation-map.yaml:L17214-L17223] |
| 40 | External implementation, row 40 | [docs/registry/implementation-map.yaml:L17224-L17235] |
| 41 | External implementation, row 41 | [docs/registry/implementation-map.yaml:L17236-L17246] |
| 42 | External implementation, row 42 | [docs/registry/implementation-map.yaml:L17247-L17256] |
| 43 | External implementation, row 43 | [docs/registry/implementation-map.yaml:L17257-L17268] |
| 44 | External implementation, row 44 | [docs/registry/implementation-map.yaml:L17269-L17280] |
| 45 | External implementation, row 45 | [docs/registry/implementation-map.yaml:L17281-L17290] |
| 46 | External implementation, row 46 | [docs/registry/implementation-map.yaml:L17291-L17300] |
| 47 | External implementation, row 47 | [docs/registry/implementation-map.yaml:L17301-L17310] |
| 48 | External implementation, row 48 | [docs/registry/implementation-map.yaml:L17311-L17320] |
| 49 | External implementation, row 49 | [docs/registry/implementation-map.yaml:L17321-L17330] |
| 50 | External implementation, row 50 | [docs/registry/implementation-map.yaml:L17331-L17339] |
| 51 | External implementation, row 51 | [docs/registry/implementation-map.yaml:L17340-L17349] |
| 52 | External implementation, row 52 | [docs/registry/implementation-map.yaml:L17350-L17359] |
| 53 | External implementation, row 53 | [docs/registry/implementation-map.yaml:L17360-L17371] |
| 54 | External implementation, row 54 | [docs/registry/implementation-map.yaml:L17372-L17381] |
| 55 | External implementation, row 55 | [docs/registry/implementation-map.yaml:L17382-L17391] |
| 56 | External implementation, row 56 | [docs/registry/implementation-map.yaml:L17392-L17401] |
| 57 | External implementation, row 57 | [docs/registry/implementation-map.yaml:L17402-L17411] |
| 58 | External implementation, row 58 | [docs/registry/implementation-map.yaml:L17412-L17423] |
| 59 | External implementation, row 59 | [docs/registry/implementation-map.yaml:L17424-L17441] |
| 60 | External implementation, row 60 | [docs/registry/implementation-map.yaml:L17442-L17454] |
| 61 | External implementation, row 61 | [docs/registry/implementation-map.yaml:L17455-L17464] |
| 62 | External implementation, row 62 | [docs/registry/implementation-map.yaml:L17465-L17474] |
| 63 | External implementation, row 63 | [docs/registry/implementation-map.yaml:L17475-L17488] |
| 64 | External implementation, row 64 | [docs/registry/implementation-map.yaml:L17489-L17497] |
| 65 | External implementation, row 65 | [docs/registry/implementation-map.yaml:L17498-L17507] |
| 66 | External implementation, row 66 | [docs/registry/implementation-map.yaml:L17508-L17517] |
| 67 | External implementation, row 67 | [docs/registry/implementation-map.yaml:L17518-L17527] |
| 68 | External implementation, row 68 | [docs/registry/implementation-map.yaml:L17528-L17537] |
| 69 | External implementation, row 69 | [docs/registry/implementation-map.yaml:L17538-L17547] |
| 70 | External implementation, row 70 | [docs/registry/implementation-map.yaml:L17548-L17557] |
| 71 | External implementation, row 71 | [docs/registry/implementation-map.yaml:L17558-L17567] |
| 72 | External implementation, row 72 | [docs/registry/implementation-map.yaml:L17568-L17577] |
| 73 | External implementation, row 73 | [docs/registry/implementation-map.yaml:L17578-L17587] |
| 74 | External implementation, row 74 | [docs/registry/implementation-map.yaml:L17588-L17599] |
| 75 | External implementation, row 75 | [docs/registry/implementation-map.yaml:L17600-L17609] |
| 76 | External implementation, row 76 | [docs/registry/implementation-map.yaml:L17610-L17621] |
| 77 | External implementation, row 77 | [docs/registry/implementation-map.yaml:L17622-L17631] |
| 78 | External implementation, row 78 | [docs/registry/implementation-map.yaml:L17632-L17643] |
| 79 | External implementation, row 79 | [docs/registry/implementation-map.yaml:L17644-L17653] |
| 80 | External implementation, row 80 | [docs/registry/implementation-map.yaml:L17654-L17666] |
| 81 | External implementation, row 81 | [docs/registry/implementation-map.yaml:L17667-L17676] |
| 82 | External implementation, row 82 | [docs/registry/implementation-map.yaml:L17677-L17688] |
| 83 | External implementation, row 83 | [docs/registry/implementation-map.yaml:L17689-L17699] |
| 84 | External implementation, row 84 | [docs/registry/implementation-map.yaml:L17700-L17711] |
| 85 | External implementation, row 85 | [docs/registry/implementation-map.yaml:L17712-L17723] |
| 86 | External implementation, row 86 | [docs/registry/implementation-map.yaml:L17724-L17735] |
| 87 | External implementation, row 87 | [docs/registry/implementation-map.yaml:L17736-L17745] |
| 88 | External implementation, row 88 | [docs/registry/implementation-map.yaml:L17746-L17756] |
| 89 | External implementation, row 89 | [docs/registry/implementation-map.yaml:L17757-L17766] |
| 90 | External implementation, row 90 | [docs/registry/implementation-map.yaml:L17767-L17776] |
| 91 | External implementation, row 91 | [docs/registry/implementation-map.yaml:L17777-L17786] |
| 92 | External implementation, row 92 | [docs/registry/implementation-map.yaml:L17787-L17796] |
| 93 | External implementation, row 93 | [docs/registry/implementation-map.yaml:L17797-L17808] |
| 94 | External implementation, row 94 | [docs/registry/implementation-map.yaml:L17809-L17819] |
| 95 | External implementation, row 95 | [docs/registry/implementation-map.yaml:L17820-L17830] |
| 96 | External implementation, row 96 | [docs/registry/implementation-map.yaml:L17831-L17841] |
| 97 | External implementation, row 97 | [docs/registry/implementation-map.yaml:L17842-L17851] |
| 98 | External implementation, row 98 | [docs/registry/implementation-map.yaml:L17852-L17861] |
| 99 | External implementation, row 99 | [docs/registry/implementation-map.yaml:L17862-L17872] |
| 100 | External implementation, row 100 | [docs/registry/implementation-map.yaml:L17873-L17883] |
| 101 | External implementation, row 101 | [docs/registry/implementation-map.yaml:L17884-L17893] |
| 102 | External implementation, row 102 | [docs/registry/implementation-map.yaml:L17894-L17904] |
| 103 | External implementation, row 103 | [docs/registry/implementation-map.yaml:L17905-L17914] |
| 104 | External implementation, row 104 | [docs/registry/implementation-map.yaml:L17915-L17925] |
| 105 | External implementation, row 105 | [docs/registry/implementation-map.yaml:L17926-L17936] |
| 106 | External implementation, row 106 | [docs/registry/implementation-map.yaml:L17937-L17946] |
| 107 | External implementation, row 107 | [docs/registry/implementation-map.yaml:L17947-L17956] |
| 108 | External implementation, row 108 | [docs/registry/implementation-map.yaml:L17957-L17966] |
| 109 | External implementation, row 109 | [docs/registry/implementation-map.yaml:L17967-L17978] |
| 110 | External implementation, row 110 | [docs/registry/implementation-map.yaml:L17979-L17988] |
| 111 | External implementation, row 111 | [docs/registry/implementation-map.yaml:L17989-L17998] |
| 112 | External implementation, row 112 | [docs/registry/implementation-map.yaml:L17999-L18010] |
| 113 | External implementation, row 113 | [docs/registry/implementation-map.yaml:L18011-L18023] |
| 114 | External implementation, row 114 | [docs/registry/implementation-map.yaml:L18024-L18033] |
| 115 | External implementation, row 115 | [docs/registry/implementation-map.yaml:L18034-L18043] |
| 116 | External implementation, row 116 | [docs/registry/implementation-map.yaml:L18044-L18054] |
| 117 | External implementation, row 117 | [docs/registry/implementation-map.yaml:L18055-L18064] |
| 118 | External implementation, row 118 | [docs/registry/implementation-map.yaml:L18065-L18074] |
| 119 | External implementation, row 119 | [docs/registry/implementation-map.yaml:L18075-L18084] |
| 120 | External implementation, row 120 | [docs/registry/implementation-map.yaml:L18085-L18102] |
| 121 | External implementation, row 121 | [docs/registry/implementation-map.yaml:L18103-L18112] |
| 122 | External implementation, row 122 | [docs/registry/implementation-map.yaml:L18113-L18122] |
| 123 | External implementation, row 123 | [docs/registry/implementation-map.yaml:L18123-L18132] |
| 124 | External implementation, row 124 | [docs/registry/implementation-map.yaml:L18133-L18144] |
| 125 | External implementation, row 125 | [docs/registry/implementation-map.yaml:L18145-L18154] |
| 126 | External implementation, row 126 | [docs/registry/implementation-map.yaml:L18155-L18167] |
| 127 | External implementation, row 127 | [docs/registry/implementation-map.yaml:L18168-L18177] |
| 128 | External implementation, row 128 | [docs/registry/implementation-map.yaml:L18178-L18187] |
| 129 | External implementation, row 129 | [docs/registry/implementation-map.yaml:L18188-L18200] |
| 130 | External implementation, row 130 | [docs/registry/implementation-map.yaml:L18201-L18210] |
| 131 | External implementation, row 131 | [docs/registry/implementation-map.yaml:L18211-L18220] |
| 132 | External implementation, row 132 | [docs/registry/implementation-map.yaml:L18221-L18232] |
| 133 | External implementation, row 133 | [docs/registry/implementation-map.yaml:L18233-L18244] |
| 134 | External implementation, row 134 | [docs/registry/implementation-map.yaml:L18245-L18256] |
| 135 | External implementation, row 135 | [docs/registry/implementation-map.yaml:L18257-L18266] |
| 136 | External implementation, row 136 | [docs/registry/implementation-map.yaml:L18267-L18276] |
| 137 | External implementation, row 137 | [docs/registry/implementation-map.yaml:L18277-L18286] |
| 138 | External implementation, row 138 | [docs/registry/implementation-map.yaml:L18287-L18296] |
| 139 | External implementation, row 139 | [docs/registry/implementation-map.yaml:L18297-L18306] |
| 140 | External implementation, row 140 | [docs/registry/implementation-map.yaml:L18307-L18316] |
| 141 | External implementation, row 141 | [docs/registry/implementation-map.yaml:L18317-L18326] |
| 142 | External implementation, row 142 | [docs/registry/implementation-map.yaml:L18327-L18336] |
| 143 | External implementation, row 143 | [docs/registry/implementation-map.yaml:L18337-L18346] |
| 144 | External implementation, row 144 | [docs/registry/implementation-map.yaml:L18347-L18356] |
| 145 | External implementation, row 145 | [docs/registry/implementation-map.yaml:L18357-L18366] |
| 146 | External implementation, row 146 | [docs/registry/implementation-map.yaml:L18367-L18376] |
| 147 | External implementation, row 147 | [docs/registry/implementation-map.yaml:L18377-L18386] |
| 148 | External implementation, row 148 | [docs/registry/implementation-map.yaml:L18387-L18396] |
| 149 | External implementation, row 149 | [docs/registry/implementation-map.yaml:L18397-L18410] |
| 150 | External implementation, row 150 | [docs/registry/implementation-map.yaml:L18411-L18421] |
| 151 | External implementation, row 151 | [docs/registry/implementation-map.yaml:L18422-L18431] |
| 152 | External implementation, row 152 | [docs/registry/implementation-map.yaml:L18432-L18441] |
| 153 | External implementation, row 153 | [docs/registry/implementation-map.yaml:L18442-L18452] |
| 154 | External implementation, row 154 | [docs/registry/implementation-map.yaml:L18453-L18462] |
| 155 | External implementation, row 155 | [docs/registry/implementation-map.yaml:L18463-L18472] |
| 156 | External implementation, row 156 | [docs/registry/implementation-map.yaml:L18473-L18481] |
| 157 | External implementation, row 157 | [docs/registry/implementation-map.yaml:L18482-L18493] |
| 158 | External implementation, row 158 | [docs/registry/implementation-map.yaml:L18494-L18503] |
| 159 | External implementation, row 159 | [docs/registry/implementation-map.yaml:L18504-L18515] |
| 160 | External implementation, row 160 | [docs/registry/implementation-map.yaml:L18516-L18526] |
| 161 | External implementation, row 161 | [docs/registry/implementation-map.yaml:L18527-L18536] |
| 162 | External implementation, row 162 | [docs/registry/implementation-map.yaml:L18537-L18546] |
| 163 | External implementation, row 163 | [docs/registry/implementation-map.yaml:L18547-L18556] |
| 164 | External implementation, row 164 | [docs/registry/implementation-map.yaml:L18557-L18566] |
| 165 | External implementation, row 165 | [docs/registry/implementation-map.yaml:L18567-L18576] |
| 166 | External implementation, row 166 | [docs/registry/implementation-map.yaml:L18577-L18586] |
| 167 | External implementation, row 167 | [docs/registry/implementation-map.yaml:L18587-L18596] |
| 168 | External implementation, row 168 | [docs/registry/implementation-map.yaml:L18597-L18606] |
| 169 | External implementation, row 169 | [docs/registry/implementation-map.yaml:L18607-L18616] |
| 170 | External implementation, row 170 | [docs/registry/implementation-map.yaml:L18617-L18632] |
| 171 | External implementation, row 171 | [docs/registry/implementation-map.yaml:L18633-L18644] |
| 172 | External implementation, row 172 | [docs/registry/implementation-map.yaml:L18645-L18655] |
| 173 | External implementation, row 173 | [docs/registry/implementation-map.yaml:L18656-L18666] |
| 174 | External implementation, row 174 | [docs/registry/implementation-map.yaml:L18667-L18676] |
| 175 | External implementation, row 175 | [docs/registry/implementation-map.yaml:L18677-L18686] |
| 176 | External implementation, row 176 | [docs/registry/implementation-map.yaml:L18687-L18696] |
| 177 | External implementation, row 177 | [docs/registry/implementation-map.yaml:L18697-L18706] |
| 178 | External implementation, row 178 | [docs/registry/implementation-map.yaml:L18707-L18716] |
| 179 | External implementation, row 179 | [docs/registry/implementation-map.yaml:L18717-L18726] |
| 180 | External implementation, row 180 | [docs/registry/implementation-map.yaml:L18727-L18739] |
| 181 | External implementation, row 181 | [docs/registry/implementation-map.yaml:L18740-L18749] |
| 182 | External implementation, row 182 | [docs/registry/implementation-map.yaml:L18750-L18759] |
| 183 | External implementation, row 183 | [docs/registry/implementation-map.yaml:L18760-L18769] |
| 184 | External implementation, row 184 | [docs/registry/implementation-map.yaml:L18770-L18779] |
| 185 | External implementation, row 185 | [docs/registry/implementation-map.yaml:L18780-L18789] |
| 186 | External implementation, row 186 | [docs/registry/implementation-map.yaml:L18790-L18799] |
| 187 | External implementation, row 187 | [docs/registry/implementation-map.yaml:L18800-L18809] |
| 188 | External implementation, row 188 | [docs/registry/implementation-map.yaml:L18810-L18819] |
| 189 | External implementation, row 189 | [docs/registry/implementation-map.yaml:L18820-L18829] |
| 190 | External implementation, row 190 | [docs/registry/implementation-map.yaml:L18830-L18839] |
| 191 | External implementation, row 191 | [docs/registry/implementation-map.yaml:L18840-L18849] |
| 192 | External implementation, row 192 | [docs/registry/implementation-map.yaml:L18850-L18861] |
| 193 | External implementation, row 193 | [docs/registry/implementation-map.yaml:L18862-L18871] |
| 194 | External implementation, row 194 | [docs/registry/implementation-map.yaml:L18872-L18881] |
| 195 | External implementation, row 195 | [docs/registry/implementation-map.yaml:L18882-L18893] |
| 196 | External implementation, row 196 | [docs/registry/implementation-map.yaml:L18894-L18903] |
| 197 | External implementation, row 197 | [docs/registry/implementation-map.yaml:L18904-L18917] |
| 198 | External implementation, row 198 | [docs/registry/implementation-map.yaml:L18918-L18927] |
| 199 | External implementation, row 199 | [docs/registry/implementation-map.yaml:L18928-L18936] |
| 200 | External implementation, row 200 | [docs/registry/implementation-map.yaml:L18937-L18947] |
| 201 | External implementation, row 201 | [docs/registry/implementation-map.yaml:L18948-L18957] |
| 202 | External implementation, row 202 | [docs/registry/implementation-map.yaml:L18958-L18967] |
| 203 | External implementation, row 203 | [docs/registry/implementation-map.yaml:L18968-L18979] |
| 204 | External implementation, row 204 | [docs/registry/implementation-map.yaml:L18980-L18989] |
| 205 | External implementation, row 205 | [docs/registry/implementation-map.yaml:L18990-L18999] |
| 206 | External implementation, row 206 | [docs/registry/implementation-map.yaml:L19000-L19009] |
| 207 | External implementation, row 207 | [docs/registry/implementation-map.yaml:L19010-L19019] |
| 208 | External implementation, row 208 | [docs/registry/implementation-map.yaml:L19020-L19029] |
| 209 | External implementation, row 209 | [docs/registry/implementation-map.yaml:L19030-L19039] |
| 210 | External implementation, row 210 | [docs/registry/implementation-map.yaml:L19040-L19048] |
| 211 | External implementation, row 211 | [docs/registry/implementation-map.yaml:L19049-L19060] |
| 212 | External implementation, row 212 | [docs/registry/implementation-map.yaml:L19061-L19070] |
| 213 | External implementation, row 213 | [docs/registry/implementation-map.yaml:L19071-L19080] |
| 214 | External implementation, row 214 | [docs/registry/implementation-map.yaml:L19081-L19094] |
| 215 | External implementation, row 215 | [docs/registry/implementation-map.yaml:L19095-L19104] |
| 216 | External implementation, row 216 | [docs/registry/implementation-map.yaml:L19105-L19114] |
| 217 | External implementation, row 217 | [docs/registry/implementation-map.yaml:L19115-L19124] |
| 218 | External implementation, row 218 | [docs/registry/implementation-map.yaml:L19125-L19134] |
| 219 | External implementation, row 219 | [docs/registry/implementation-map.yaml:L19135-L19144] |
| 220 | External implementation, row 220 | [docs/registry/implementation-map.yaml:L19145-L19154] |
| 221 | External implementation, row 221 | [docs/registry/implementation-map.yaml:L19155-L19164] |
| 222 | External implementation, row 222 | [docs/registry/implementation-map.yaml:L19165-L19174] |
| 223 | External implementation, row 223 | [docs/registry/implementation-map.yaml:L19175-L19183] |
| 224 | External implementation, row 224 | [docs/registry/implementation-map.yaml:L19184-L19193] |
| 225 | External implementation, row 225 | [docs/registry/implementation-map.yaml:L19194-L19203] |
| 226 | External implementation, row 226 | [docs/registry/implementation-map.yaml:L19204-L19213] |
| 227 | External implementation, row 227 | [docs/registry/implementation-map.yaml:L19214-L19223] |
| 228 | External implementation, row 228 | [docs/registry/implementation-map.yaml:L19224-L19233] |
| 229 | External implementation, row 229 | [docs/registry/implementation-map.yaml:L19234-L19243] |
| 230 | External implementation, row 230 | [docs/registry/implementation-map.yaml:L19244-L19253] |
| 231 | External implementation, row 231 | [docs/registry/implementation-map.yaml:L19254-L19263] |
| 232 | External implementation, row 232 | [docs/registry/implementation-map.yaml:L19264-L19272] |
| 233 | External implementation, row 233 | [docs/registry/implementation-map.yaml:L19273-L19284] |
| 234 | External implementation, row 234 | [docs/registry/implementation-map.yaml:L19285-L19294] |
| 235 | External implementation, row 235 | [docs/registry/implementation-map.yaml:L19295-L19306] |
| 236 | External implementation, row 236 | [docs/registry/implementation-map.yaml:L19307-L19316] |
| 237 | External implementation, row 237 | [docs/registry/implementation-map.yaml:L19317-L19326] |
| 238 | External implementation, row 238 | [docs/registry/implementation-map.yaml:L19327-L19336] |
| 239 | External implementation, row 239 | [docs/registry/implementation-map.yaml:L19337-L19348] |
| 240 | External implementation, row 240 | [docs/registry/implementation-map.yaml:L19349-L19358] |
| 241 | External implementation, row 241 | [docs/registry/implementation-map.yaml:L19359-L19367] |
| 242 | External implementation, row 242 | [docs/registry/implementation-map.yaml:L19368-L19377] |
| 243 | External implementation, row 243 | [docs/registry/implementation-map.yaml:L19378-L19387] |
| 244 | External implementation, row 244 | [docs/registry/implementation-map.yaml:L19388-L19397] |
| 245 | External implementation, row 245 | [docs/registry/implementation-map.yaml:L19398-L19407] |
| 246 | External implementation, row 246 | [docs/registry/implementation-map.yaml:L19408-L19417] |
| 247 | External implementation, row 247 | [docs/registry/implementation-map.yaml:L19418-L19427] |
| 248 | External implementation, row 248 | [docs/registry/implementation-map.yaml:L19428-L19437] |
| 249 | External implementation, row 249 | [docs/registry/implementation-map.yaml:L19438-L19447] |
| 250 | External implementation, row 250 | [docs/registry/implementation-map.yaml:L19448-L19457] |
| 251 | External implementation, row 251 | [docs/registry/implementation-map.yaml:L19458-L19467] |
| 252 | External implementation, row 252 | [docs/registry/implementation-map.yaml:L19468-L19477] |
| 253 | External implementation, row 253 | [docs/registry/implementation-map.yaml:L19478-L19487] |
| 254 | External implementation, row 254 | [docs/registry/implementation-map.yaml:L19488-L19497] |
| 255 | External implementation, row 255 | [docs/registry/implementation-map.yaml:L19498-L19507] |
| 256 | External implementation, row 256 | [docs/registry/implementation-map.yaml:L19508-L19520] |
| 257 | External implementation, row 257 | [docs/registry/implementation-map.yaml:L19521-L19533] |
| 258 | External implementation, row 258 | [docs/registry/implementation-map.yaml:L19534-L19546] |
| 259 | External implementation, row 259 | [docs/registry/implementation-map.yaml:L19547-L19556] |
| 260 | External implementation, row 260 | [docs/registry/implementation-map.yaml:L19557-L19568] |
| 261 | External implementation, row 261 | [docs/registry/implementation-map.yaml:L19569-L19578] |
| 262 | External implementation, row 262 | [docs/registry/implementation-map.yaml:L19579-L19588] |
| 263 | External implementation, row 263 | [docs/registry/implementation-map.yaml:L19589-L19598] |
| 264 | External implementation, row 264 | [docs/registry/implementation-map.yaml:L19599-L19608] |
| 265 | External implementation, row 265 | [docs/registry/implementation-map.yaml:L19609-L19617] |
| 266 | External implementation, row 266 | [docs/registry/implementation-map.yaml:L19618-L19627] |
| 267 | External implementation, row 267 | [docs/registry/implementation-map.yaml:L19628-L19637] |
| 268 | External implementation, row 268 | [docs/registry/implementation-map.yaml:L19638-L19647] |
| 269 | External implementation, row 269 | [docs/registry/implementation-map.yaml:L19648-L19659] |
| 270 | External implementation, row 270 | [docs/registry/implementation-map.yaml:L19660-L19669] |
| 271 | External implementation, row 271 | [docs/registry/implementation-map.yaml:L19670-L19679] |
| 272 | External implementation, row 272 | [docs/registry/implementation-map.yaml:L19680-L19688] |
| 273 | External implementation, row 273 | [docs/registry/implementation-map.yaml:L19689-L19698] |
| 274 | External implementation, row 274 | [docs/registry/implementation-map.yaml:L19699-L19708] |
| 275 | External implementation, row 275 | [docs/registry/implementation-map.yaml:L19709-L19718] |
| 276 | External implementation, row 276 | [docs/registry/implementation-map.yaml:L19719-L19727] |
| 277 | External implementation, row 277 | [docs/registry/implementation-map.yaml:L19728-L19739] |
| 278 | External implementation, row 278 | [docs/registry/implementation-map.yaml:L19740-L19749] |
| 279 | External implementation, row 279 | [docs/registry/implementation-map.yaml:L19750-L19759] |
| 280 | External implementation, row 280 | [docs/registry/implementation-map.yaml:L19760-L19769] |
| 281 | External implementation, row 281 | [docs/registry/implementation-map.yaml:L19770-L19779] |
| 282 | External implementation, row 282 | [docs/registry/implementation-map.yaml:L19780-L19791] |
| 283 | External implementation, row 283 | [docs/registry/implementation-map.yaml:L19792-L19801] |
| 284 | External implementation, row 284 | [docs/registry/implementation-map.yaml:L19802-L19811] |
| 285 | External implementation, row 285 | [docs/registry/implementation-map.yaml:L19812-L19821] |
| 286 | External implementation, row 286 | [docs/registry/implementation-map.yaml:L19822-L19831] |
| 287 | External implementation, row 287 | [docs/registry/implementation-map.yaml:L19832-L19841] |
| 288 | External implementation, row 288 | [docs/registry/implementation-map.yaml:L19842-L19851] |
| 289 | External implementation, row 289 | [docs/registry/implementation-map.yaml:L19852-L19861] |
| 290 | External implementation, row 290 | [docs/registry/implementation-map.yaml:L19862-L19873] |
| 291 | External implementation, row 291 | [docs/registry/implementation-map.yaml:L19874-L19883] |
| 292 | External implementation, row 292 | [docs/registry/implementation-map.yaml:L19884-L19895] |
| 293 | External implementation, row 293 | [docs/registry/implementation-map.yaml:L19896-L19907] |
| 294 | External implementation, row 294 | [docs/registry/implementation-map.yaml:L19908-L19917] |
| 295 | External implementation, row 295 | [docs/registry/implementation-map.yaml:L19918-L19927] |
| 296 | External implementation, row 296 | [docs/registry/implementation-map.yaml:L19928-L19940] |
| 297 | External implementation, row 297 | [docs/registry/implementation-map.yaml:L19941-L19952] |
| 298 | `src/digitalmodel/viv_screening/inputs.py::parse_inputs` | [docs/registry/implementation-map.yaml:L19953-L19962] |
| 299 | `src/digitalmodel/viv_screening/workflow.py::router` | [docs/registry/implementation-map.yaml:L19963-L19976] |
| 300 | `src/digitalmodel/viv_screening/workflow.py::_damage_rate` | [docs/registry/implementation-map.yaml:L19977-L19987] |
| 301 | `src/digitalmodel/viv_screening/workflow.py::_provenance` | [docs/registry/implementation-map.yaml:L19988-L19997] |
| 302 | External implementation, row 302 | [docs/registry/implementation-map.yaml:L19998-L20007] |
| 303 | External implementation, row 303 | [docs/registry/implementation-map.yaml:L20008-L20017] |
| 304 | External implementation, row 304 | [docs/registry/implementation-map.yaml:L20018-L20029] |
| 305 | External implementation, row 305 | [docs/registry/implementation-map.yaml:L20030-L20039] |
| 306 | External implementation, row 306 | [docs/registry/implementation-map.yaml:L20040-L20049] |
| 307 | External implementation, row 307 | [docs/registry/implementation-map.yaml:L20050-L20061] |
| 308 | External implementation, row 308 | [docs/registry/implementation-map.yaml:L20062-L20071] |
| 309 | External implementation, row 309 | [docs/registry/implementation-map.yaml:L20072-L20081] |
| 310 | External implementation, row 310 | [docs/registry/implementation-map.yaml:L20082-L20095] |
| 311 | External implementation, row 311 | [docs/registry/implementation-map.yaml:L20096-L20105] |
| 312 | External implementation, row 312 | [docs/registry/implementation-map.yaml:L20106-L20115] |
| 313 | External implementation, row 313 | [docs/registry/implementation-map.yaml:L20116-L20125] |
| 314 | External implementation, row 314 | [docs/registry/implementation-map.yaml:L20126-L20135] |
| 315 | External implementation, row 315 | [docs/registry/implementation-map.yaml:L20136-L20145] |
| 316 | External implementation, row 316 | [docs/registry/implementation-map.yaml:L20146-L20156] |
| 317 | External implementation, row 317 | [docs/registry/implementation-map.yaml:L20157-L20166] |
| 318 | External implementation, row 318 | [docs/registry/implementation-map.yaml:L20167-L20176] |
| 319 | External implementation, row 319 | [docs/registry/implementation-map.yaml:L20177-L20188] |
| 320 | External implementation, row 320 | [docs/registry/implementation-map.yaml:L20189-L20198] |
| 321 | External implementation, row 321 | [docs/registry/implementation-map.yaml:L20199-L20209] |
| 322 | External implementation, row 322 | [docs/registry/implementation-map.yaml:L20210-L20219] |
| 323 | External implementation, row 323 | [docs/registry/implementation-map.yaml:L20220-L20232] |
| 324 | External implementation, row 324 | [docs/registry/implementation-map.yaml:L20233-L20242] |
| 325 | External implementation, row 325 | [docs/registry/implementation-map.yaml:L20243-L20252] |
| 326 | External implementation, row 326 | [docs/registry/implementation-map.yaml:L20253-L20263] |
| 327 | External implementation, row 327 | [docs/registry/implementation-map.yaml:L20264-L20274] |
| 328 | External implementation, row 328 | [docs/registry/implementation-map.yaml:L20275-L20284] |
| 329 | External implementation, row 329 | [docs/registry/implementation-map.yaml:L20285-L20294] |
| 330 | External implementation, row 330 | [docs/registry/implementation-map.yaml:L20295-L20304] |
| 331 | External implementation, row 331 | [docs/registry/implementation-map.yaml:L20305-L20314] |
| 332 | External implementation, row 332 | [docs/registry/implementation-map.yaml:L20315-L20324] |
| 333 | External implementation, row 333 | [docs/registry/implementation-map.yaml:L20325-L20334] |
| 334 | External implementation, row 334 | [docs/registry/implementation-map.yaml:L20335-L20344] |
| 335 | External implementation, row 335 | [docs/registry/implementation-map.yaml:L20345-L20354] |
| 336 | External implementation, row 336 | [docs/registry/implementation-map.yaml:L20355-L20364] |
| 337 | External implementation, row 337 | [docs/registry/implementation-map.yaml:L20365-L20373] |
| 338 | External implementation, row 338 | [docs/registry/implementation-map.yaml:L20374-L20383] |
| 339 | External implementation, row 339 | [docs/registry/implementation-map.yaml:L20384-L20395] |
| 340 | External implementation, row 340 | [docs/registry/implementation-map.yaml:L20396-L20405] |
| 341 | External implementation, row 341 | [docs/registry/implementation-map.yaml:L20406-L20415] |
| 342 | External implementation, row 342 | [docs/registry/implementation-map.yaml:L20416-L20425] |
| 343 | External implementation, row 343 | [docs/registry/implementation-map.yaml:L20426-L20437] |
| 344 | External implementation, row 344 | [docs/registry/implementation-map.yaml:L20438-L20452] |
| 345 | External implementation, row 345 | [docs/registry/implementation-map.yaml:L20453-L20462] |
| 346 | External implementation, row 346 | [docs/registry/implementation-map.yaml:L20463-L20472] |
| 347 | External implementation, row 347 | [docs/registry/implementation-map.yaml:L20473-L20482] |
| 348 | External implementation, row 348 | [docs/registry/implementation-map.yaml:L20483-L20492] |
| 349 | External implementation, row 349 | [docs/registry/implementation-map.yaml:L20493-L20502] |
| 350 | External implementation, row 350 | [docs/registry/implementation-map.yaml:L20503-L20512] |
| 351 | External implementation, row 351 | [docs/registry/implementation-map.yaml:L20513-L20522] |
| 352 | External implementation, row 352 | [docs/registry/implementation-map.yaml:L20523-L20532] |
| 353 | External implementation, row 353 | [docs/registry/implementation-map.yaml:L20533-L20542] |
| 354 | External implementation, row 354 | [docs/registry/implementation-map.yaml:L20543-L20552] |
| 355 | External implementation, row 355 | [docs/registry/implementation-map.yaml:L20553-L20562] |
| 356 | External implementation, row 356 | [docs/registry/implementation-map.yaml:L20563-L20574] |
| 357 | External implementation, row 357 | [docs/registry/implementation-map.yaml:L20575-L20584] |
| 358 | External implementation, row 358 | [docs/registry/implementation-map.yaml:L20585-L20594] |
| 359 | External implementation, row 359 | [docs/registry/implementation-map.yaml:L20595-L20604] |
| 360 | External implementation, row 360 | [docs/registry/implementation-map.yaml:L20605-L20616] |
| 361 | External implementation, row 361 | [docs/registry/implementation-map.yaml:L20617-L20626] |
| 362 | External implementation, row 362 | [docs/registry/implementation-map.yaml:L20627-L20636] |
| 363 | External implementation, row 363 | [docs/registry/implementation-map.yaml:L20637-L20646] |
| 364 | External implementation, row 364 | [docs/registry/implementation-map.yaml:L20647-L20656] |
| 365 | External implementation, row 365 | [docs/registry/implementation-map.yaml:L20657-L20666] |
| 366 | External implementation, row 366 | [docs/registry/implementation-map.yaml:L20667-L20676] |
| 367 | External implementation, row 367 | [docs/registry/implementation-map.yaml:L20677-L20686] |
| 368 | External implementation, row 368 | [docs/registry/implementation-map.yaml:L20687-L20696] |
| 369 | External implementation, row 369 | [docs/registry/implementation-map.yaml:L20697-L20706] |
| 370 | External implementation, row 370 | [docs/registry/implementation-map.yaml:L20707-L20716] |
| 371 | External implementation, row 371 | [docs/registry/implementation-map.yaml:L20717-L20727] |
| 372 | External implementation, row 372 | [docs/registry/implementation-map.yaml:L20728-L20738] |
| 373 | External implementation, row 373 | [docs/registry/implementation-map.yaml:L20739-L20748] |
| 374 | External implementation, row 374 | [docs/registry/implementation-map.yaml:L20749-L20758] |
| 375 | External implementation, row 375 | [docs/registry/implementation-map.yaml:L20759-L20768] |
| 376 | External implementation, row 376 | [docs/registry/implementation-map.yaml:L20769-L20778] |
| 377 | External implementation, row 377 | [docs/registry/implementation-map.yaml:L20779-L20788] |
| 378 | External implementation, row 378 | [docs/registry/implementation-map.yaml:L20789-L20800] |
| 379 | External implementation, row 379 | [docs/registry/implementation-map.yaml:L20801-L20810] |
| 380 | External implementation, row 380 | [docs/registry/implementation-map.yaml:L20811-L20820] |
| 381 | External implementation, row 381 | [docs/registry/implementation-map.yaml:L20821-L20831] |
| 382 | External implementation, row 382 | [docs/registry/implementation-map.yaml:L20832-L20841] |
| 383 | External implementation, row 383 | [docs/registry/implementation-map.yaml:L20842-L20853] |
| 384 | External implementation, row 384 | [docs/registry/implementation-map.yaml:L20854-L20864] |
| 385 | External implementation, row 385 | [docs/registry/implementation-map.yaml:L20865-L20875] |
| 386 | External implementation, row 386 | [docs/registry/implementation-map.yaml:L20876-L20886] |
| 387 | External implementation, row 387 | [docs/registry/implementation-map.yaml:L20887-L20897] |
| 388 | External implementation, row 388 | [docs/registry/implementation-map.yaml:L20898-L20907] |
| 389 | External implementation, row 389 | [docs/registry/implementation-map.yaml:L20908-L20917] |
| 390 | External implementation, row 390 | [docs/registry/implementation-map.yaml:L20918-L20928] |
| 391 | External implementation, row 391 | [docs/registry/implementation-map.yaml:L20929-L20938] |
| 392 | External implementation, row 392 | [docs/registry/implementation-map.yaml:L20939-L20948] |

### effective-tension-riser-stack-up

| Map row | Implementation identity | Pinned map citation |
| --- | --- | --- |
| 1 | External implementation, row 1 | [docs/registry/implementation-map.yaml:L2146-L2155] |
| 2 | External implementation, row 2 | [docs/registry/implementation-map.yaml:L2156-L2165] |
| 3 | External implementation, row 3 | [docs/registry/implementation-map.yaml:L2166-L2175] |
| 4 | External implementation, row 4 | [docs/registry/implementation-map.yaml:L2176-L2185] |
| 5 | External implementation, row 5 | [docs/registry/implementation-map.yaml:L2186-L2195] |
| 6 | External implementation, row 6 | [docs/registry/implementation-map.yaml:L2196-L2205] |
| 7 | External implementation, row 7 | [docs/registry/implementation-map.yaml:L2206-L2215] |
| 8 | External implementation, row 8 | [docs/registry/implementation-map.yaml:L2216-L2225] |
| 9 | External implementation, row 9 | [docs/registry/implementation-map.yaml:L2226-L2235] |
| 10 | External implementation, row 10 | [docs/registry/implementation-map.yaml:L2236-L2247] |
| 11 | External implementation, row 11 | [docs/registry/implementation-map.yaml:L2248-L2259] |
| 12 | External implementation, row 12 | [docs/registry/implementation-map.yaml:L2260-L2269] |
| 13 | External implementation, row 13 | [docs/registry/implementation-map.yaml:L2270-L2282] |
| 14 | External implementation, row 14 | [docs/registry/implementation-map.yaml:L2283-L2291] |
| 15 | External implementation, row 15 | [docs/registry/implementation-map.yaml:L2292-L2301] |
| 16 | External implementation, row 16 | [docs/registry/implementation-map.yaml:L2302-L2311] |
| 17 | External implementation, row 17 | [docs/registry/implementation-map.yaml:L2312-L2324] |
| 18 | External implementation, row 18 | [docs/registry/implementation-map.yaml:L2325-L2334] |
| 19 | External implementation, row 19 | [docs/registry/implementation-map.yaml:L2335-L2344] |
| 20 | External implementation, row 20 | [docs/registry/implementation-map.yaml:L2345-L2356] |
| 21 | External implementation, row 21 | [docs/registry/implementation-map.yaml:L2357-L2365] |
| 22 | External implementation, row 22 | [docs/registry/implementation-map.yaml:L2366-L2375] |
| 23 | External implementation, row 23 | [docs/registry/implementation-map.yaml:L2376-L2387] |
| 24 | External implementation, row 24 | [docs/registry/implementation-map.yaml:L2388-L2397] |
| 25 | External implementation, row 25 | [docs/registry/implementation-map.yaml:L2398-L2407] |
| 26 | External implementation, row 26 | [docs/registry/implementation-map.yaml:L2408-L2417] |
| 27 | External implementation, row 27 | [docs/registry/implementation-map.yaml:L2418-L2427] |
| 28 | External implementation, row 28 | [docs/registry/implementation-map.yaml:L2428-L2437] |
| 29 | External implementation, row 29 | [docs/registry/implementation-map.yaml:L2438-L2447] |
| 30 | External implementation, row 30 | [docs/registry/implementation-map.yaml:L2448-L2457] |
| 31 | External implementation, row 31 | [docs/registry/implementation-map.yaml:L2458-L2470] |
| 32 | External implementation, row 32 | [docs/registry/implementation-map.yaml:L2471-L2484] |
| 33 | External implementation, row 33 | [docs/registry/implementation-map.yaml:L2485-L2494] |
| 34 | External implementation, row 34 | [docs/registry/implementation-map.yaml:L2495-L2504] |
| 35 | External implementation, row 35 | [docs/registry/implementation-map.yaml:L2505-L2514] |
| 36 | External implementation, row 36 | [docs/registry/implementation-map.yaml:L2515-L2524] |
| 37 | External implementation, row 37 | [docs/registry/implementation-map.yaml:L2525-L2534] |
| 38 | External implementation, row 38 | [docs/registry/implementation-map.yaml:L2535-L2544] |
| 39 | External implementation, row 39 | [docs/registry/implementation-map.yaml:L2545-L2556] |
| 40 | External implementation, row 40 | [docs/registry/implementation-map.yaml:L2557-L2566] |
| 41 | External implementation, row 41 | [docs/registry/implementation-map.yaml:L2567-L2576] |
| 42 | External implementation, row 42 | [docs/registry/implementation-map.yaml:L2577-L2586] |
| 43 | External implementation, row 43 | [docs/registry/implementation-map.yaml:L2587-L2595] |
| 44 | External implementation, row 44 | [docs/registry/implementation-map.yaml:L2596-L2605] |
| 45 | External implementation, row 45 | [docs/registry/implementation-map.yaml:L2606-L2615] |
| 46 | External implementation, row 46 | [docs/registry/implementation-map.yaml:L2616-L2625] |
| 47 | External implementation, row 47 | [docs/registry/implementation-map.yaml:L2626-L2635] |
| 48 | External implementation, row 48 | [docs/registry/implementation-map.yaml:L2636-L2645] |
| 49 | External implementation, row 49 | [docs/registry/implementation-map.yaml:L2646-L2655] |
| 50 | External implementation, row 50 | [docs/registry/implementation-map.yaml:L2656-L2667] |
| 51 | External implementation, row 51 | [docs/registry/implementation-map.yaml:L2668-L2679] |
| 52 | External implementation, row 52 | [docs/registry/implementation-map.yaml:L2680-L2689] |
| 53 | External implementation, row 53 | [docs/registry/implementation-map.yaml:L2690-L2701] |
| 54 | External implementation, row 54 | [docs/registry/implementation-map.yaml:L2702-L2715] |
| 55 | External implementation, row 55 | [docs/registry/implementation-map.yaml:L2716-L2726] |
| 56 | External implementation, row 56 | [docs/registry/implementation-map.yaml:L2727-L2740] |
| 57 | External implementation, row 57 | [docs/registry/implementation-map.yaml:L2741-L2750] |
| 58 | External implementation, row 58 | [docs/registry/implementation-map.yaml:L2751-L2760] |
| 59 | External implementation, row 59 | [docs/registry/implementation-map.yaml:L2761-L2774] |
| 60 | External implementation, row 60 | [docs/registry/implementation-map.yaml:L2775-L2787] |
| 61 | External implementation, row 61 | [docs/registry/implementation-map.yaml:L2788-L2797] |
| 62 | External implementation, row 62 | [docs/registry/implementation-map.yaml:L2798-L2807] |
| 63 | External implementation, row 63 | [docs/registry/implementation-map.yaml:L2808-L2817] |
| 64 | External implementation, row 64 | [docs/registry/implementation-map.yaml:L2818-L2827] |
| 65 | External implementation, row 65 | [docs/registry/implementation-map.yaml:L2828-L2839] |
| 66 | External implementation, row 66 | [docs/registry/implementation-map.yaml:L2840-L2852] |
| 67 | External implementation, row 67 | [docs/registry/implementation-map.yaml:L2853-L2862] |
| 68 | External implementation, row 68 | [docs/registry/implementation-map.yaml:L2863-L2874] |
| 69 | External implementation, row 69 | [docs/registry/implementation-map.yaml:L2875-L2884] |
| 70 | External implementation, row 70 | [docs/registry/implementation-map.yaml:L2885-L2896] |
| 71 | External implementation, row 71 | [docs/registry/implementation-map.yaml:L2897-L2907] |
| 72 | External implementation, row 72 | [docs/registry/implementation-map.yaml:L2908-L2917] |
| 73 | External implementation, row 73 | [docs/registry/implementation-map.yaml:L2918-L2927] |
| 74 | External implementation, row 74 | [docs/registry/implementation-map.yaml:L2928-L2938] |
| 75 | External implementation, row 75 | [docs/registry/implementation-map.yaml:L2939-L2948] |
| 76 | External implementation, row 76 | [docs/registry/implementation-map.yaml:L2949-L2960] |
| 77 | External implementation, row 77 | [docs/registry/implementation-map.yaml:L2961-L2970] |
| 78 | External implementation, row 78 | [docs/registry/implementation-map.yaml:L2971-L2980] |
| 79 | External implementation, row 79 | [docs/registry/implementation-map.yaml:L2981-L2990] |
| 80 | External implementation, row 80 | [docs/registry/implementation-map.yaml:L2991-L3000] |
| 81 | External implementation, row 81 | [docs/registry/implementation-map.yaml:L3001-L3010] |
| 82 | External implementation, row 82 | [docs/registry/implementation-map.yaml:L3011-L3022] |
| 83 | External implementation, row 83 | [docs/registry/implementation-map.yaml:L3023-L3032] |
| 84 | External implementation, row 84 | [docs/registry/implementation-map.yaml:L3033-L3042] |
| 85 | External implementation, row 85 | [docs/registry/implementation-map.yaml:L3043-L3052] |
| 86 | External implementation, row 86 | [docs/registry/implementation-map.yaml:L3053-L3062] |
| 87 | External implementation, row 87 | [docs/registry/implementation-map.yaml:L3063-L3072] |
| 88 | External implementation, row 88 | [docs/registry/implementation-map.yaml:L3073-L3082] |
| 89 | External implementation, row 89 | [docs/registry/implementation-map.yaml:L3083-L3092] |
| 90 | External implementation, row 90 | [docs/registry/implementation-map.yaml:L3093-L3102] |
| 91 | External implementation, row 91 | [docs/registry/implementation-map.yaml:L3103-L3112] |
| 92 | External implementation, row 92 | [docs/registry/implementation-map.yaml:L3113-L3122] |
| 93 | External implementation, row 93 | [docs/registry/implementation-map.yaml:L3123-L3135] |
| 94 | External implementation, row 94 | [docs/registry/implementation-map.yaml:L3136-L3145] |
| 95 | External implementation, row 95 | [docs/registry/implementation-map.yaml:L3146-L3155] |
| 96 | External implementation, row 96 | [docs/registry/implementation-map.yaml:L3156-L3168] |
| 97 | External implementation, row 97 | [docs/registry/implementation-map.yaml:L3169-L3178] |
| 98 | External implementation, row 98 | [docs/registry/implementation-map.yaml:L3179-L3188] |
| 99 | External implementation, row 99 | [docs/registry/implementation-map.yaml:L3189-L3198] |
| 100 | External implementation, row 100 | [docs/registry/implementation-map.yaml:L3199-L3208] |
| 101 | External implementation, row 101 | [docs/registry/implementation-map.yaml:L3209-L3218] |
| 102 | External implementation, row 102 | [docs/registry/implementation-map.yaml:L3219-L3231] |
| 103 | External implementation, row 103 | [docs/registry/implementation-map.yaml:L3232-L3241] |
| 104 | External implementation, row 104 | [docs/registry/implementation-map.yaml:L3242-L3251] |
| 105 | External implementation, row 105 | [docs/registry/implementation-map.yaml:L3252-L3264] |
| 106 | External implementation, row 106 | [docs/registry/implementation-map.yaml:L3265-L3274] |
| 107 | External implementation, row 107 | [docs/registry/implementation-map.yaml:L3275-L3287] |
| 108 | External implementation, row 108 | [docs/registry/implementation-map.yaml:L3288-L3300] |
| 109 | External implementation, row 109 | [docs/registry/implementation-map.yaml:L3301-L3311] |
| 110 | External implementation, row 110 | [docs/registry/implementation-map.yaml:L3312-L3321] |
| 111 | External implementation, row 111 | [docs/registry/implementation-map.yaml:L3322-L3331] |
| 112 | External implementation, row 112 | [docs/registry/implementation-map.yaml:L3332-L3341] |
| 113 | External implementation, row 113 | [docs/registry/implementation-map.yaml:L3342-L3351] |
| 114 | External implementation, row 114 | [docs/registry/implementation-map.yaml:L3352-L3361] |
| 115 | External implementation, row 115 | [docs/registry/implementation-map.yaml:L3362-L3377] |
| 116 | External implementation, row 116 | [docs/registry/implementation-map.yaml:L3378-L3387] |
| 117 | External implementation, row 117 | [docs/registry/implementation-map.yaml:L3388-L3397] |
| 118 | External implementation, row 118 | [docs/registry/implementation-map.yaml:L3398-L3407] |
| 119 | External implementation, row 119 | [docs/registry/implementation-map.yaml:L3408-L3417] |
| 120 | External implementation, row 120 | [docs/registry/implementation-map.yaml:L3418-L3432] |
| 121 | External implementation, row 121 | [docs/registry/implementation-map.yaml:L3433-L3442] |
| 122 | External implementation, row 122 | [docs/registry/implementation-map.yaml:L3443-L3452] |
| 123 | External implementation, row 123 | [docs/registry/implementation-map.yaml:L3453-L3462] |
| 124 | External implementation, row 124 | [docs/registry/implementation-map.yaml:L3463-L3472] |
| 125 | External implementation, row 125 | [docs/registry/implementation-map.yaml:L3473-L3482] |
| 126 | External implementation, row 126 | [docs/registry/implementation-map.yaml:L3483-L3492] |
| 127 | External implementation, row 127 | [docs/registry/implementation-map.yaml:L3493-L3502] |
| 128 | External implementation, row 128 | [docs/registry/implementation-map.yaml:L3503-L3512] |
| 129 | External implementation, row 129 | [docs/registry/implementation-map.yaml:L3513-L3522] |
| 130 | External implementation, row 130 | [docs/registry/implementation-map.yaml:L3523-L3532] |
| 131 | External implementation, row 131 | [docs/registry/implementation-map.yaml:L3533-L3542] |
| 132 | External implementation, row 132 | [docs/registry/implementation-map.yaml:L3543-L3555] |
| 133 | External implementation, row 133 | [docs/registry/implementation-map.yaml:L3556-L3565] |
| 134 | External implementation, row 134 | [docs/registry/implementation-map.yaml:L3566-L3575] |
| 135 | External implementation, row 135 | [docs/registry/implementation-map.yaml:L3576-L3585] |
| 136 | External implementation, row 136 | [docs/registry/implementation-map.yaml:L3586-L3597] |
| 137 | External implementation, row 137 | [docs/registry/implementation-map.yaml:L3598-L3607] |
| 138 | External implementation, row 138 | [docs/registry/implementation-map.yaml:L3608-L3617] |
| 139 | External implementation, row 139 | [docs/registry/implementation-map.yaml:L3618-L3627] |
| 140 | External implementation, row 140 | [docs/registry/implementation-map.yaml:L3628-L3637] |
| 141 | External implementation, row 141 | [docs/registry/implementation-map.yaml:L3638-L3647] |
| 142 | External implementation, row 142 | [docs/registry/implementation-map.yaml:L3648-L3657] |
| 143 | External implementation, row 143 | [docs/registry/implementation-map.yaml:L3658-L3667] |
| 144 | External implementation, row 144 | [docs/registry/implementation-map.yaml:L3668-L3677] |
| 145 | External implementation, row 145 | [docs/registry/implementation-map.yaml:L3678-L3687] |
| 146 | External implementation, row 146 | [docs/registry/implementation-map.yaml:L3688-L3697] |
| 147 | External implementation, row 147 | [docs/registry/implementation-map.yaml:L3698-L3707] |
| 148 | External implementation, row 148 | [docs/registry/implementation-map.yaml:L3708-L3717] |
| 149 | External implementation, row 149 | [docs/registry/implementation-map.yaml:L3718-L3727] |
| 150 | External implementation, row 150 | [docs/registry/implementation-map.yaml:L3728-L3737] |
| 151 | External implementation, row 151 | [docs/registry/implementation-map.yaml:L3738-L3747] |
| 152 | External implementation, row 152 | [docs/registry/implementation-map.yaml:L3748-L3757] |
| 153 | External implementation, row 153 | [docs/registry/implementation-map.yaml:L3758-L3767] |
| 154 | External implementation, row 154 | [docs/registry/implementation-map.yaml:L3768-L3777] |
| 155 | External implementation, row 155 | [docs/registry/implementation-map.yaml:L3778-L3787] |
| 156 | External implementation, row 156 | [docs/registry/implementation-map.yaml:L3788-L3797] |
| 157 | External implementation, row 157 | [docs/registry/implementation-map.yaml:L3798-L3807] |
| 158 | External implementation, row 158 | [docs/registry/implementation-map.yaml:L3808-L3819] |
| 159 | External implementation, row 159 | [docs/registry/implementation-map.yaml:L3820-L3829] |
| 160 | External implementation, row 160 | [docs/registry/implementation-map.yaml:L3830-L3839] |
| 161 | External implementation, row 161 | [docs/registry/implementation-map.yaml:L3840-L3849] |
| 162 | External implementation, row 162 | [docs/registry/implementation-map.yaml:L3850-L3862] |
| 163 | External implementation, row 163 | [docs/registry/implementation-map.yaml:L3863-L3872] |
| 164 | External implementation, row 164 | [docs/registry/implementation-map.yaml:L3873-L3885] |
| 165 | External implementation, row 165 | [docs/registry/implementation-map.yaml:L3886-L3898] |
| 166 | External implementation, row 166 | [docs/registry/implementation-map.yaml:L3899-L3909] |
| 167 | External implementation, row 167 | [docs/registry/implementation-map.yaml:L3910-L3919] |
| 168 | External implementation, row 168 | [docs/registry/implementation-map.yaml:L3920-L3929] |
| 169 | External implementation, row 169 | [docs/registry/implementation-map.yaml:L3930-L3939] |
| 170 | External implementation, row 170 | [docs/registry/implementation-map.yaml:L3940-L3949] |
| 171 | External implementation, row 171 | [docs/registry/implementation-map.yaml:L3950-L3959] |
| 172 | External implementation, row 172 | [docs/registry/implementation-map.yaml:L3960-L3969] |
| 173 | External implementation, row 173 | [docs/registry/implementation-map.yaml:L3970-L3979] |
| 174 | External implementation, row 174 | [docs/registry/implementation-map.yaml:L3980-L3989] |
| 175 | External implementation, row 175 | [docs/registry/implementation-map.yaml:L3990-L3999] |
| 176 | External implementation, row 176 | [docs/registry/implementation-map.yaml:L4000-L4009] |
| 177 | External implementation, row 177 | [docs/registry/implementation-map.yaml:L4010-L4019] |
| 178 | External implementation, row 178 | [docs/registry/implementation-map.yaml:L4020-L4029] |
| 179 | External implementation, row 179 | [docs/registry/implementation-map.yaml:L4030-L4039] |
| 180 | External implementation, row 180 | [docs/registry/implementation-map.yaml:L4040-L4049] |
| 181 | External implementation, row 181 | [docs/registry/implementation-map.yaml:L4050-L4059] |
| 182 | External implementation, row 182 | [docs/registry/implementation-map.yaml:L4060-L4069] |
| 183 | External implementation, row 183 | [docs/registry/implementation-map.yaml:L4070-L4079] |
| 184 | External implementation, row 184 | [docs/registry/implementation-map.yaml:L4080-L4089] |
| 185 | External implementation, row 185 | [docs/registry/implementation-map.yaml:L4090-L4099] |
| 186 | External implementation, row 186 | [docs/registry/implementation-map.yaml:L4100-L4109] |
| 187 | External implementation, row 187 | [docs/registry/implementation-map.yaml:L4110-L4119] |
| 188 | External implementation, row 188 | [docs/registry/implementation-map.yaml:L4120-L4129] |
| 189 | External implementation, row 189 | [docs/registry/implementation-map.yaml:L4130-L4139] |
| 190 | External implementation, row 190 | [docs/registry/implementation-map.yaml:L4140-L4149] |
| 191 | External implementation, row 191 | [docs/registry/implementation-map.yaml:L4150-L4159] |
| 192 | External implementation, row 192 | [docs/registry/implementation-map.yaml:L4160-L4169] |
| 193 | External implementation, row 193 | [docs/registry/implementation-map.yaml:L4170-L4179] |
| 194 | External implementation, row 194 | [docs/registry/implementation-map.yaml:L4180-L4189] |
| 195 | External implementation, row 195 | [docs/registry/implementation-map.yaml:L4190-L4199] |
| 196 | External implementation, row 196 | [docs/registry/implementation-map.yaml:L4200-L4209] |
| 197 | External implementation, row 197 | [docs/registry/implementation-map.yaml:L4210-L4222] |
| 198 | External implementation, row 198 | [docs/registry/implementation-map.yaml:L4223-L4232] |
| 199 | External implementation, row 199 | [docs/registry/implementation-map.yaml:L4233-L4242] |
| 200 | External implementation, row 200 | [docs/registry/implementation-map.yaml:L4243-L4252] |
| 201 | External implementation, row 201 | [docs/registry/implementation-map.yaml:L4253-L4262] |
| 202 | External implementation, row 202 | [docs/registry/implementation-map.yaml:L4263-L4272] |
| 203 | External implementation, row 203 | [docs/registry/implementation-map.yaml:L4273-L4285] |
| 204 | External implementation, row 204 | [docs/registry/implementation-map.yaml:L4286-L4295] |
| 205 | External implementation, row 205 | [docs/registry/implementation-map.yaml:L4296-L4307] |
| 206 | External implementation, row 206 | [docs/registry/implementation-map.yaml:L4308-L4317] |
| 207 | External implementation, row 207 | [docs/registry/implementation-map.yaml:L4318-L4327] |
| 208 | External implementation, row 208 | [docs/registry/implementation-map.yaml:L4328-L4337] |
| 209 | External implementation, row 209 | [docs/registry/implementation-map.yaml:L4338-L4347] |
| 210 | External implementation, row 210 | [docs/registry/implementation-map.yaml:L4348-L4362] |
| 211 | External implementation, row 211 | [docs/registry/implementation-map.yaml:L4363-L4372] |
| 212 | External implementation, row 212 | [docs/registry/implementation-map.yaml:L4373-L4382] |
| 213 | External implementation, row 213 | [docs/registry/implementation-map.yaml:L4383-L4392] |
| 214 | External implementation, row 214 | [docs/registry/implementation-map.yaml:L4393-L4402] |
| 215 | External implementation, row 215 | [docs/registry/implementation-map.yaml:L4403-L4412] |
| 216 | External implementation, row 216 | [docs/registry/implementation-map.yaml:L4413-L4422] |
| 217 | External implementation, row 217 | [docs/registry/implementation-map.yaml:L4423-L4432] |
| 218 | External implementation, row 218 | [docs/registry/implementation-map.yaml:L4433-L4444] |
| 219 | External implementation, row 219 | [docs/registry/implementation-map.yaml:L4445-L4454] |
| 220 | External implementation, row 220 | [docs/registry/implementation-map.yaml:L4455-L4464] |
| 221 | External implementation, row 221 | [docs/registry/implementation-map.yaml:L4465-L4476] |
| 222 | `src/digitalmodel/viv_screening/inputs.py::_beam` | [docs/registry/implementation-map.yaml:L4477-L4486] |
| 223 | External implementation, row 223 | [docs/registry/implementation-map.yaml:L4487-L4496] |
| 224 | External implementation, row 224 | [docs/registry/implementation-map.yaml:L4497-L4506] |
| 225 | External implementation, row 225 | [docs/registry/implementation-map.yaml:L4507-L4516] |
| 226 | External implementation, row 226 | [docs/registry/implementation-map.yaml:L4517-L4526] |
| 227 | External implementation, row 227 | [docs/registry/implementation-map.yaml:L4527-L4536] |
| 228 | External implementation, row 228 | [docs/registry/implementation-map.yaml:L4537-L4545] |
| 229 | External implementation, row 229 | [docs/registry/implementation-map.yaml:L4546-L4557] |
| 230 | External implementation, row 230 | [docs/registry/implementation-map.yaml:L4558-L4567] |
| 231 | External implementation, row 231 | [docs/registry/implementation-map.yaml:L4568-L4578] |
| 232 | External implementation, row 232 | [docs/registry/implementation-map.yaml:L4579-L4589] |
| 233 | External implementation, row 233 | [docs/registry/implementation-map.yaml:L4590-L4599] |
| 234 | External implementation, row 234 | [docs/registry/implementation-map.yaml:L4600-L4609] |
| 235 | External implementation, row 235 | [docs/registry/implementation-map.yaml:L4610-L4623] |
| 236 | External implementation, row 236 | [docs/registry/implementation-map.yaml:L4624-L4634] |
| 237 | External implementation, row 237 | [docs/registry/implementation-map.yaml:L4635-L4644] |
| 238 | External implementation, row 238 | [docs/registry/implementation-map.yaml:L4645-L4654] |
| 239 | External implementation, row 239 | [docs/registry/implementation-map.yaml:L4655-L4666] |
| 240 | External implementation, row 240 | [docs/registry/implementation-map.yaml:L4667-L4676] |
| 241 | External implementation, row 241 | [docs/registry/implementation-map.yaml:L4677-L4686] |
| 242 | External implementation, row 242 | [docs/registry/implementation-map.yaml:L4687-L4696] |
| 243 | External implementation, row 243 | [docs/registry/implementation-map.yaml:L4697-L4706] |
| 244 | External implementation, row 244 | [docs/registry/implementation-map.yaml:L4707-L4716] |
| 245 | External implementation, row 245 | [docs/registry/implementation-map.yaml:L4717-L4726] |
| 246 | External implementation, row 246 | [docs/registry/implementation-map.yaml:L4727-L4736] |
| 247 | External implementation, row 247 | [docs/registry/implementation-map.yaml:L4737-L4746] |
| 248 | External implementation, row 248 | [docs/registry/implementation-map.yaml:L4747-L4756] |
| 249 | External implementation, row 249 | [docs/registry/implementation-map.yaml:L4757-L4766] |
| 250 | External implementation, row 250 | [docs/registry/implementation-map.yaml:L4767-L4776] |
| 251 | External implementation, row 251 | [docs/registry/implementation-map.yaml:L4777-L4786] |
| 252 | External implementation, row 252 | [docs/registry/implementation-map.yaml:L4787-L4796] |
| 253 | External implementation, row 253 | [docs/registry/implementation-map.yaml:L4797-L4806] |
| 254 | External implementation, row 254 | [docs/registry/implementation-map.yaml:L4807-L4816] |
| 255 | External implementation, row 255 | [docs/registry/implementation-map.yaml:L4817-L4826] |
| 256 | External implementation, row 256 | [docs/registry/implementation-map.yaml:L4827-L4836] |
| 257 | External implementation, row 257 | [docs/registry/implementation-map.yaml:L4837-L4846] |
| 258 | External implementation, row 258 | [docs/registry/implementation-map.yaml:L4847-L4856] |
| 259 | External implementation, row 259 | [docs/registry/implementation-map.yaml:L4857-L4865] |
| 260 | External implementation, row 260 | [docs/registry/implementation-map.yaml:L4866-L4875] |
| 261 | External implementation, row 261 | [docs/registry/implementation-map.yaml:L4876-L4887] |
| 262 | External implementation, row 262 | [docs/registry/implementation-map.yaml:L4888-L4897] |
| 263 | External implementation, row 263 | [docs/registry/implementation-map.yaml:L4898-L4907] |
| 264 | External implementation, row 264 | [docs/registry/implementation-map.yaml:L4908-L4917] |

### orcaflex-text-model-writers

| Map row | Implementation identity | Pinned map citation |
| --- | --- | --- |
| 1 | External implementation, row 1 | [docs/registry/implementation-map.yaml:L4940-L4949] |
| 2 | External implementation, row 2 | [docs/registry/implementation-map.yaml:L4950-L4958] |
| 3 | External implementation, row 3 | [docs/registry/implementation-map.yaml:L4959-L4968] |
| 4 | External implementation, row 4 | [docs/registry/implementation-map.yaml:L4969-L4978] |
| 5 | External implementation, row 5 | [docs/registry/implementation-map.yaml:L4979-L4988] |
| 6 | External implementation, row 6 | [docs/registry/implementation-map.yaml:L4989-L4998] |
| 7 | External implementation, row 7 | [docs/registry/implementation-map.yaml:L4999-L5008] |
| 8 | External implementation, row 8 | [docs/registry/implementation-map.yaml:L5009-L5020] |
| 9 | External implementation, row 9 | [docs/registry/implementation-map.yaml:L5021-L5030] |
| 10 | External implementation, row 10 | [docs/registry/implementation-map.yaml:L5031-L5040] |
| 11 | External implementation, row 11 | [docs/registry/implementation-map.yaml:L5041-L5050] |
| 12 | External implementation, row 12 | [docs/registry/implementation-map.yaml:L5051-L5060] |
| 13 | External implementation, row 13 | [docs/registry/implementation-map.yaml:L5061-L5070] |
| 14 | External implementation, row 14 | [docs/registry/implementation-map.yaml:L5071-L5080] |
| 15 | External implementation, row 15 | [docs/registry/implementation-map.yaml:L5081-L5090] |
| 16 | External implementation, row 16 | [docs/registry/implementation-map.yaml:L5091-L5100] |
| 17 | External implementation, row 17 | [docs/registry/implementation-map.yaml:L5101-L5110] |
| 18 | External implementation, row 18 | [docs/registry/implementation-map.yaml:L5111-L5120] |
| 19 | External implementation, row 19 | [docs/registry/implementation-map.yaml:L5121-L5132] |
| 20 | External implementation, row 20 | [docs/registry/implementation-map.yaml:L5133-L5142] |
| 21 | External implementation, row 21 | [docs/registry/implementation-map.yaml:L5143-L5151] |
| 22 | External implementation, row 22 | [docs/registry/implementation-map.yaml:L5152-L5161] |
| 23 | External implementation, row 23 | [docs/registry/implementation-map.yaml:L5162-L5171] |
| 24 | External implementation, row 24 | [docs/registry/implementation-map.yaml:L5172-L5181] |
| 25 | External implementation, row 25 | [docs/registry/implementation-map.yaml:L5182-L5191] |
| 26 | External implementation, row 26 | [docs/registry/implementation-map.yaml:L5192-L5201] |
| 27 | External implementation, row 27 | [docs/registry/implementation-map.yaml:L5202-L5211] |
| 28 | External implementation, row 28 | [docs/registry/implementation-map.yaml:L5212-L5223] |
| 29 | External implementation, row 29 | [docs/registry/implementation-map.yaml:L5224-L5233] |
| 30 | External implementation, row 30 | [docs/registry/implementation-map.yaml:L5234-L5243] |
| 31 | External implementation, row 31 | [docs/registry/implementation-map.yaml:L5244-L5253] |
| 32 | External implementation, row 32 | [docs/registry/implementation-map.yaml:L5254-L5263] |
| 33 | External implementation, row 33 | [docs/registry/implementation-map.yaml:L5264-L5273] |
| 34 | External implementation, row 34 | [docs/registry/implementation-map.yaml:L5274-L5283] |
| 35 | External implementation, row 35 | [docs/registry/implementation-map.yaml:L5284-L5293] |
| 36 | External implementation, row 36 | [docs/registry/implementation-map.yaml:L5294-L5303] |
| 37 | External implementation, row 37 | [docs/registry/implementation-map.yaml:L5304-L5313] |
| 38 | External implementation, row 38 | [docs/registry/implementation-map.yaml:L5314-L5323] |
| 39 | External implementation, row 39 | [docs/registry/implementation-map.yaml:L5324-L5333] |
| 40 | External implementation, row 40 | [docs/registry/implementation-map.yaml:L5334-L5343] |
| 41 | External implementation, row 41 | [docs/registry/implementation-map.yaml:L5344-L5353] |
| 42 | External implementation, row 42 | [docs/registry/implementation-map.yaml:L5354-L5363] |
| 43 | External implementation, row 43 | [docs/registry/implementation-map.yaml:L5364-L5372] |
| 44 | External implementation, row 44 | [docs/registry/implementation-map.yaml:L5373-L5382] |
| 45 | External implementation, row 45 | [docs/registry/implementation-map.yaml:L5383-L5392] |
| 46 | External implementation, row 46 | [docs/registry/implementation-map.yaml:L5393-L5402] |
| 47 | External implementation, row 47 | [docs/registry/implementation-map.yaml:L5403-L5415] |
| 48 | External implementation, row 48 | [docs/registry/implementation-map.yaml:L5416-L5425] |
| 49 | External implementation, row 49 | [docs/registry/implementation-map.yaml:L5426-L5437] |
| 50 | External implementation, row 50 | [docs/registry/implementation-map.yaml:L5438-L5447] |
| 51 | External implementation, row 51 | [docs/registry/implementation-map.yaml:L5448-L5457] |
| 52 | External implementation, row 52 | [docs/registry/implementation-map.yaml:L5458-L5467] |
| 53 | External implementation, row 53 | [docs/registry/implementation-map.yaml:L5468-L5477] |
| 54 | External implementation, row 54 | [docs/registry/implementation-map.yaml:L5478-L5487] |
| 55 | External implementation, row 55 | [docs/registry/implementation-map.yaml:L5488-L5497] |
| 56 | External implementation, row 56 | [docs/registry/implementation-map.yaml:L5498-L5507] |
| 57 | External implementation, row 57 | [docs/registry/implementation-map.yaml:L5508-L5519] |
| 58 | External implementation, row 58 | [docs/registry/implementation-map.yaml:L5520-L5531] |
| 59 | External implementation, row 59 | [docs/registry/implementation-map.yaml:L5532-L5540] |
| 60 | External implementation, row 60 | [docs/registry/implementation-map.yaml:L5541-L5550] |
| 61 | External implementation, row 61 | [docs/registry/implementation-map.yaml:L5551-L5560] |
| 62 | External implementation, row 62 | [docs/registry/implementation-map.yaml:L5561-L5570] |
| 63 | External implementation, row 63 | [docs/registry/implementation-map.yaml:L5571-L5580] |
| 64 | External implementation, row 64 | [docs/registry/implementation-map.yaml:L5581-L5593] |
| 65 | External implementation, row 65 | [docs/registry/implementation-map.yaml:L5594-L5603] |
| 66 | External implementation, row 66 | [docs/registry/implementation-map.yaml:L5604-L5615] |
| 67 | External implementation, row 67 | [docs/registry/implementation-map.yaml:L5616-L5628] |
| 68 | External implementation, row 68 | [docs/registry/implementation-map.yaml:L5629-L5638] |
| 69 | External implementation, row 69 | [docs/registry/implementation-map.yaml:L5639-L5648] |
| 70 | External implementation, row 70 | [docs/registry/implementation-map.yaml:L5649-L5658] |
| 71 | External implementation, row 71 | [docs/registry/implementation-map.yaml:L5659-L5667] |
| 72 | External implementation, row 72 | [docs/registry/implementation-map.yaml:L5668-L5677] |
| 73 | External implementation, row 73 | [docs/registry/implementation-map.yaml:L5678-L5687] |
| 74 | External implementation, row 74 | [docs/registry/implementation-map.yaml:L5688-L5697] |
| 75 | External implementation, row 75 | [docs/registry/implementation-map.yaml:L5698-L5707] |
| 76 | External implementation, row 76 | [docs/registry/implementation-map.yaml:L5708-L5717] |
| 77 | External implementation, row 77 | [docs/registry/implementation-map.yaml:L5718-L5727] |
| 78 | External implementation, row 78 | [docs/registry/implementation-map.yaml:L5728-L5737] |
| 79 | External implementation, row 79 | [docs/registry/implementation-map.yaml:L5738-L5749] |
| 80 | External implementation, row 80 | [docs/registry/implementation-map.yaml:L5750-L5759] |
| 81 | External implementation, row 81 | [docs/registry/implementation-map.yaml:L5760-L5769] |
| 82 | External implementation, row 82 | [docs/registry/implementation-map.yaml:L5770-L5781] |
| 83 | External implementation, row 83 | [docs/registry/implementation-map.yaml:L5782-L5791] |
| 84 | External implementation, row 84 | [docs/registry/implementation-map.yaml:L5792-L5801] |
| 85 | External implementation, row 85 | [docs/registry/implementation-map.yaml:L5802-L5811] |
| 86 | External implementation, row 86 | [docs/registry/implementation-map.yaml:L5812-L5823] |
| 87 | External implementation, row 87 | [docs/registry/implementation-map.yaml:L5824-L5833] |
| 88 | External implementation, row 88 | [docs/registry/implementation-map.yaml:L5834-L5843] |
| 89 | External implementation, row 89 | [docs/registry/implementation-map.yaml:L5844-L5853] |
| 90 | External implementation, row 90 | [docs/registry/implementation-map.yaml:L5854-L5863] |
| 91 | External implementation, row 91 | [docs/registry/implementation-map.yaml:L5864-L5872] |
| 92 | External implementation, row 92 | [docs/registry/implementation-map.yaml:L5873-L5882] |
| 93 | External implementation, row 93 | [docs/registry/implementation-map.yaml:L5883-L5891] |
| 94 | External implementation, row 94 | [docs/registry/implementation-map.yaml:L5892-L5901] |
| 95 | External implementation, row 95 | [docs/registry/implementation-map.yaml:L5902-L5911] |
| 96 | External implementation, row 96 | [docs/registry/implementation-map.yaml:L5912-L5921] |
| 97 | External implementation, row 97 | [docs/registry/implementation-map.yaml:L5922-L5933] |
| 98 | External implementation, row 98 | [docs/registry/implementation-map.yaml:L5934-L5943] |
| 99 | External implementation, row 99 | [docs/registry/implementation-map.yaml:L5944-L5953] |
| 100 | External implementation, row 100 | [docs/registry/implementation-map.yaml:L5954-L5965] |
| 101 | External implementation, row 101 | [docs/registry/implementation-map.yaml:L5966-L5975] |
| 102 | External implementation, row 102 | [docs/registry/implementation-map.yaml:L5976-L5985] |
| 103 | External implementation, row 103 | [docs/registry/implementation-map.yaml:L5986-L5994] |
| 104 | External implementation, row 104 | [docs/registry/implementation-map.yaml:L5995-L6004] |
| 105 | External implementation, row 105 | [docs/registry/implementation-map.yaml:L6005-L6014] |
| 106 | External implementation, row 106 | [docs/registry/implementation-map.yaml:L6015-L6024] |
| 107 | External implementation, row 107 | [docs/registry/implementation-map.yaml:L6025-L6034] |
| 108 | External implementation, row 108 | [docs/registry/implementation-map.yaml:L6035-L6044] |
| 109 | External implementation, row 109 | [docs/registry/implementation-map.yaml:L6045-L6054] |
| 110 | External implementation, row 110 | [docs/registry/implementation-map.yaml:L6055-L6064] |
| 111 | External implementation, row 111 | [docs/registry/implementation-map.yaml:L6065-L6074] |
| 112 | External implementation, row 112 | [docs/registry/implementation-map.yaml:L6075-L6084] |
| 113 | External implementation, row 113 | [docs/registry/implementation-map.yaml:L6085-L6096] |
| 114 | External implementation, row 114 | [docs/registry/implementation-map.yaml:L6097-L6109] |
| 115 | External implementation, row 115 | [docs/registry/implementation-map.yaml:L6110-L6119] |
| 116 | External implementation, row 116 | [docs/registry/implementation-map.yaml:L6120-L6129] |
| 117 | External implementation, row 117 | [docs/registry/implementation-map.yaml:L6130-L6141] |
| 118 | External implementation, row 118 | [docs/registry/implementation-map.yaml:L6142-L6153] |
| 119 | External implementation, row 119 | [docs/registry/implementation-map.yaml:L6154-L6163] |
| 120 | External implementation, row 120 | [docs/registry/implementation-map.yaml:L6164-L6173] |
| 121 | External implementation, row 121 | [docs/registry/implementation-map.yaml:L6174-L6183] |
| 122 | External implementation, row 122 | [docs/registry/implementation-map.yaml:L6184-L6193] |
| 123 | External implementation, row 123 | [docs/registry/implementation-map.yaml:L6194-L6203] |
| 124 | External implementation, row 124 | [docs/registry/implementation-map.yaml:L6204-L6213] |
| 125 | External implementation, row 125 | [docs/registry/implementation-map.yaml:L6214-L6223] |
| 126 | External implementation, row 126 | [docs/registry/implementation-map.yaml:L6224-L6233] |
| 127 | External implementation, row 127 | [docs/registry/implementation-map.yaml:L6234-L6243] |
| 128 | External implementation, row 128 | [docs/registry/implementation-map.yaml:L6244-L6253] |
| 129 | External implementation, row 129 | [docs/registry/implementation-map.yaml:L6254-L6263] |
| 130 | External implementation, row 130 | [docs/registry/implementation-map.yaml:L6264-L6273] |
| 131 | External implementation, row 131 | [docs/registry/implementation-map.yaml:L6274-L6283] |
| 132 | External implementation, row 132 | [docs/registry/implementation-map.yaml:L6284-L6293] |
| 133 | External implementation, row 133 | [docs/registry/implementation-map.yaml:L6294-L6303] |
| 134 | External implementation, row 134 | [docs/registry/implementation-map.yaml:L6304-L6313] |
| 135 | External implementation, row 135 | [docs/registry/implementation-map.yaml:L6314-L6323] |
| 136 | External implementation, row 136 | [docs/registry/implementation-map.yaml:L6324-L6333] |
| 137 | External implementation, row 137 | [docs/registry/implementation-map.yaml:L6334-L6343] |
| 138 | External implementation, row 138 | [docs/registry/implementation-map.yaml:L6344-L6353] |
| 139 | External implementation, row 139 | [docs/registry/implementation-map.yaml:L6354-L6363] |
| 140 | External implementation, row 140 | [docs/registry/implementation-map.yaml:L6364-L6373] |
| 141 | External implementation, row 141 | [docs/registry/implementation-map.yaml:L6374-L6383] |
| 142 | External implementation, row 142 | [docs/registry/implementation-map.yaml:L6384-L6393] |
| 143 | External implementation, row 143 | [docs/registry/implementation-map.yaml:L6394-L6403] |
| 144 | External implementation, row 144 | [docs/registry/implementation-map.yaml:L6404-L6413] |
| 145 | External implementation, row 145 | [docs/registry/implementation-map.yaml:L6414-L6423] |
| 146 | External implementation, row 146 | [docs/registry/implementation-map.yaml:L6424-L6436] |
| 147 | External implementation, row 147 | [docs/registry/implementation-map.yaml:L6437-L6448] |
| 148 | External implementation, row 148 | [docs/registry/implementation-map.yaml:L6449-L6458] |
| 149 | External implementation, row 149 | [docs/registry/implementation-map.yaml:L6459-L6468] |
| 150 | External implementation, row 150 | [docs/registry/implementation-map.yaml:L6469-L6478] |
| 151 | External implementation, row 151 | [docs/registry/implementation-map.yaml:L6479-L6490] |
| 152 | External implementation, row 152 | [docs/registry/implementation-map.yaml:L6491-L6500] |
| 153 | External implementation, row 153 | [docs/registry/implementation-map.yaml:L6501-L6510] |
| 154 | External implementation, row 154 | [docs/registry/implementation-map.yaml:L6511-L6520] |
| 155 | External implementation, row 155 | [docs/registry/implementation-map.yaml:L6521-L6530] |
| 156 | External implementation, row 156 | [docs/registry/implementation-map.yaml:L6531-L6540] |
| 157 | External implementation, row 157 | [docs/registry/implementation-map.yaml:L6541-L6550] |
| 158 | External implementation, row 158 | [docs/registry/implementation-map.yaml:L6551-L6560] |
| 159 | External implementation, row 159 | [docs/registry/implementation-map.yaml:L6561-L6570] |
| 160 | External implementation, row 160 | [docs/registry/implementation-map.yaml:L6571-L6580] |
| 161 | External implementation, row 161 | [docs/registry/implementation-map.yaml:L6581-L6590] |
| 162 | External implementation, row 162 | [docs/registry/implementation-map.yaml:L6591-L6600] |
| 163 | External implementation, row 163 | [docs/registry/implementation-map.yaml:L6601-L6610] |
| 164 | External implementation, row 164 | [docs/registry/implementation-map.yaml:L6611-L6620] |
| 165 | External implementation, row 165 | [docs/registry/implementation-map.yaml:L6621-L6630] |
| 166 | External implementation, row 166 | [docs/registry/implementation-map.yaml:L6631-L6640] |
| 167 | External implementation, row 167 | [docs/registry/implementation-map.yaml:L6641-L6650] |
| 168 | External implementation, row 168 | [docs/registry/implementation-map.yaml:L6651-L6659] |
| 169 | External implementation, row 169 | [docs/registry/implementation-map.yaml:L6660-L6672] |
| 170 | External implementation, row 170 | [docs/registry/implementation-map.yaml:L6673-L6682] |
| 171 | External implementation, row 171 | [docs/registry/implementation-map.yaml:L6683-L6692] |
| 172 | External implementation, row 172 | [docs/registry/implementation-map.yaml:L6693-L6704] |
| 173 | External implementation, row 173 | [docs/registry/implementation-map.yaml:L6705-L6714] |
| 174 | External implementation, row 174 | [docs/registry/implementation-map.yaml:L6715-L6724] |
| 175 | External implementation, row 175 | [docs/registry/implementation-map.yaml:L6725-L6734] |
| 176 | External implementation, row 176 | [docs/registry/implementation-map.yaml:L6735-L6744] |
| 177 | External implementation, row 177 | [docs/registry/implementation-map.yaml:L6745-L6754] |
| 178 | External implementation, row 178 | [docs/registry/implementation-map.yaml:L6755-L6764] |
| 179 | External implementation, row 179 | [docs/registry/implementation-map.yaml:L6765-L6774] |
| 180 | External implementation, row 180 | [docs/registry/implementation-map.yaml:L6775-L6784] |
| 181 | External implementation, row 181 | [docs/registry/implementation-map.yaml:L6785-L6794] |
| 182 | External implementation, row 182 | [docs/registry/implementation-map.yaml:L6795-L6804] |
| 183 | External implementation, row 183 | [docs/registry/implementation-map.yaml:L6805-L6814] |
| 184 | External implementation, row 184 | [docs/registry/implementation-map.yaml:L6815-L6824] |
| 185 | External implementation, row 185 | [docs/registry/implementation-map.yaml:L6825-L6834] |
| 186 | External implementation, row 186 | [docs/registry/implementation-map.yaml:L6835-L6844] |
| 187 | External implementation, row 187 | [docs/registry/implementation-map.yaml:L6845-L6854] |
| 188 | External implementation, row 188 | [docs/registry/implementation-map.yaml:L6855-L6864] |
| 189 | External implementation, row 189 | [docs/registry/implementation-map.yaml:L6865-L6874] |
| 190 | External implementation, row 190 | [docs/registry/implementation-map.yaml:L6875-L6884] |
| 191 | External implementation, row 191 | [docs/registry/implementation-map.yaml:L6885-L6894] |
| 192 | External implementation, row 192 | [docs/registry/implementation-map.yaml:L6895-L6904] |
| 193 | External implementation, row 193 | [docs/registry/implementation-map.yaml:L6905-L6914] |
| 194 | External implementation, row 194 | [docs/registry/implementation-map.yaml:L6915-L6927] |
| 195 | External implementation, row 195 | [docs/registry/implementation-map.yaml:L6928-L6937] |
| 196 | External implementation, row 196 | [docs/registry/implementation-map.yaml:L6938-L6947] |
| 197 | External implementation, row 197 | [docs/registry/implementation-map.yaml:L6948-L6957] |
| 198 | External implementation, row 198 | [docs/registry/implementation-map.yaml:L6958-L6967] |
| 199 | External implementation, row 199 | [docs/registry/implementation-map.yaml:L6968-L6977] |
| 200 | External implementation, row 200 | [docs/registry/implementation-map.yaml:L6978-L6987] |
| 201 | External implementation, row 201 | [docs/registry/implementation-map.yaml:L6988-L6997] |
| 202 | External implementation, row 202 | [docs/registry/implementation-map.yaml:L6998-L7007] |
| 203 | External implementation, row 203 | [docs/registry/implementation-map.yaml:L7008-L7017] |
| 204 | External implementation, row 204 | [docs/registry/implementation-map.yaml:L7018-L7031] |
| 205 | External implementation, row 205 | [docs/registry/implementation-map.yaml:L7032-L7041] |
| 206 | External implementation, row 206 | [docs/registry/implementation-map.yaml:L7042-L7051] |
| 207 | External implementation, row 207 | [docs/registry/implementation-map.yaml:L7052-L7061] |
| 208 | External implementation, row 208 | [docs/registry/implementation-map.yaml:L7062-L7071] |
| 209 | External implementation, row 209 | [docs/registry/implementation-map.yaml:L7072-L7081] |
| 210 | External implementation, row 210 | [docs/registry/implementation-map.yaml:L7082-L7091] |
| 211 | External implementation, row 211 | [docs/registry/implementation-map.yaml:L7092-L7101] |
| 212 | External implementation, row 212 | [docs/registry/implementation-map.yaml:L7102-L7111] |
| 213 | External implementation, row 213 | [docs/registry/implementation-map.yaml:L7112-L7121] |
| 214 | External implementation, row 214 | [docs/registry/implementation-map.yaml:L7122-L7131] |
| 215 | External implementation, row 215 | [docs/registry/implementation-map.yaml:L7132-L7141] |
| 216 | External implementation, row 216 | [docs/registry/implementation-map.yaml:L7142-L7153] |
| 217 | External implementation, row 217 | [docs/registry/implementation-map.yaml:L7154-L7163] |
| 218 | External implementation, row 218 | [docs/registry/implementation-map.yaml:L7164-L7176] |
| 219 | External implementation, row 219 | [docs/registry/implementation-map.yaml:L7177-L7186] |
| 220 | External implementation, row 220 | [docs/registry/implementation-map.yaml:L7187-L7196] |
| 221 | External implementation, row 221 | [docs/registry/implementation-map.yaml:L7197-L7207] |
| 222 | External implementation, row 222 | [docs/registry/implementation-map.yaml:L7208-L7217] |
| 223 | External implementation, row 223 | [docs/registry/implementation-map.yaml:L7218-L7227] |
| 224 | External implementation, row 224 | [docs/registry/implementation-map.yaml:L7228-L7237] |
| 225 | External implementation, row 225 | [docs/registry/implementation-map.yaml:L7238-L7247] |
| 226 | External implementation, row 226 | [docs/registry/implementation-map.yaml:L7248-L7257] |
| 227 | External implementation, row 227 | [docs/registry/implementation-map.yaml:L7258-L7269] |
| 228 | External implementation, row 228 | [docs/registry/implementation-map.yaml:L7270-L7279] |
| 229 | External implementation, row 229 | [docs/registry/implementation-map.yaml:L7280-L7289] |
| 230 | External implementation, row 230 | [docs/registry/implementation-map.yaml:L7290-L7299] |
| 231 | External implementation, row 231 | [docs/registry/implementation-map.yaml:L7300-L7309] |
| 232 | External implementation, row 232 | [docs/registry/implementation-map.yaml:L7310-L7319] |
| 233 | External implementation, row 233 | [docs/registry/implementation-map.yaml:L7320-L7329] |
| 234 | External implementation, row 234 | [docs/registry/implementation-map.yaml:L7330-L7339] |
| 235 | External implementation, row 235 | [docs/registry/implementation-map.yaml:L7340-L7349] |
| 236 | External implementation, row 236 | [docs/registry/implementation-map.yaml:L7350-L7359] |
| 237 | External implementation, row 237 | [docs/registry/implementation-map.yaml:L7360-L7369] |
| 238 | External implementation, row 238 | [docs/registry/implementation-map.yaml:L7370-L7379] |
| 239 | External implementation, row 239 | [docs/registry/implementation-map.yaml:L7380-L7389] |
| 240 | External implementation, row 240 | [docs/registry/implementation-map.yaml:L7390-L7399] |
| 241 | External implementation, row 241 | [docs/registry/implementation-map.yaml:L7400-L7409] |
| 242 | External implementation, row 242 | [docs/registry/implementation-map.yaml:L7410-L7419] |
| 243 | External implementation, row 243 | [docs/registry/implementation-map.yaml:L7420-L7429] |
| 244 | External implementation, row 244 | [docs/registry/implementation-map.yaml:L7430-L7439] |
| 245 | External implementation, row 245 | [docs/registry/implementation-map.yaml:L7440-L7449] |
| 246 | External implementation, row 246 | [docs/registry/implementation-map.yaml:L7450-L7459] |
| 247 | External implementation, row 247 | [docs/registry/implementation-map.yaml:L7460-L7469] |
| 248 | External implementation, row 248 | [docs/registry/implementation-map.yaml:L7470-L7479] |
| 249 | External implementation, row 249 | [docs/registry/implementation-map.yaml:L7480-L7489] |
| 250 | External implementation, row 250 | [docs/registry/implementation-map.yaml:L7490-L7499] |
| 251 | External implementation, row 251 | [docs/registry/implementation-map.yaml:L7500-L7509] |
| 252 | External implementation, row 252 | [docs/registry/implementation-map.yaml:L7510-L7519] |
| 253 | External implementation, row 253 | [docs/registry/implementation-map.yaml:L7520-L7529] |
| 254 | External implementation, row 254 | [docs/registry/implementation-map.yaml:L7530-L7539] |
| 255 | External implementation, row 255 | [docs/registry/implementation-map.yaml:L7540-L7551] |
| 256 | External implementation, row 256 | [docs/registry/implementation-map.yaml:L7552-L7561] |
| 257 | External implementation, row 257 | [docs/registry/implementation-map.yaml:L7562-L7571] |
| 258 | External implementation, row 258 | [docs/registry/implementation-map.yaml:L7572-L7581] |
| 259 | External implementation, row 259 | [docs/registry/implementation-map.yaml:L7582-L7591] |
| 260 | External implementation, row 260 | [docs/registry/implementation-map.yaml:L7592-L7603] |
| 261 | External implementation, row 261 | [docs/registry/implementation-map.yaml:L7604-L7613] |
| 262 | External implementation, row 262 | [docs/registry/implementation-map.yaml:L7614-L7626] |
| 263 | External implementation, row 263 | [docs/registry/implementation-map.yaml:L7627-L7636] |
| 264 | External implementation, row 264 | [docs/registry/implementation-map.yaml:L7637-L7646] |
| 265 | External implementation, row 265 | [docs/registry/implementation-map.yaml:L7647-L7656] |
| 266 | External implementation, row 266 | [docs/registry/implementation-map.yaml:L7657-L7666] |
| 267 | External implementation, row 267 | [docs/registry/implementation-map.yaml:L7667-L7676] |
| 268 | External implementation, row 268 | [docs/registry/implementation-map.yaml:L7677-L7686] |
| 269 | External implementation, row 269 | [docs/registry/implementation-map.yaml:L7687-L7696] |
| 270 | External implementation, row 270 | [docs/registry/implementation-map.yaml:L7697-L7706] |
| 271 | External implementation, row 271 | [docs/registry/implementation-map.yaml:L7707-L7716] |
| 272 | External implementation, row 272 | [docs/registry/implementation-map.yaml:L7717-L7729] |
| 273 | External implementation, row 273 | [docs/registry/implementation-map.yaml:L7730-L7739] |
| 274 | External implementation, row 274 | [docs/registry/implementation-map.yaml:L7740-L7749] |
| 275 | External implementation, row 275 | [docs/registry/implementation-map.yaml:L7750-L7762] |
| 276 | External implementation, row 276 | [docs/registry/implementation-map.yaml:L7763-L7774] |
| 277 | External implementation, row 277 | [docs/registry/implementation-map.yaml:L7775-L7784] |
| 278 | External implementation, row 278 | [docs/registry/implementation-map.yaml:L7785-L7794] |
| 279 | External implementation, row 279 | [docs/registry/implementation-map.yaml:L7795-L7804] |
| 280 | External implementation, row 280 | [docs/registry/implementation-map.yaml:L7805-L7816] |
| 281 | External implementation, row 281 | [docs/registry/implementation-map.yaml:L7817-L7826] |
| 282 | External implementation, row 282 | [docs/registry/implementation-map.yaml:L7827-L7836] |
| 283 | External implementation, row 283 | [docs/registry/implementation-map.yaml:L7837-L7846] |
| 284 | External implementation, row 284 | [docs/registry/implementation-map.yaml:L7847-L7856] |
| 285 | External implementation, row 285 | [docs/registry/implementation-map.yaml:L7857-L7866] |
| 286 | External implementation, row 286 | [docs/registry/implementation-map.yaml:L7867-L7878] |
| 287 | External implementation, row 287 | [docs/registry/implementation-map.yaml:L7879-L7888] |
| 288 | External implementation, row 288 | [docs/registry/implementation-map.yaml:L7889-L7898] |
| 289 | External implementation, row 289 | [docs/registry/implementation-map.yaml:L7899-L7908] |
| 290 | External implementation, row 290 | [docs/registry/implementation-map.yaml:L7909-L7921] |
| 291 | External implementation, row 291 | [docs/registry/implementation-map.yaml:L7922-L7931] |
| 292 | External implementation, row 292 | [docs/registry/implementation-map.yaml:L7932-L7941] |
| 293 | External implementation, row 293 | [docs/registry/implementation-map.yaml:L7942-L7951] |
| 294 | External implementation, row 294 | [docs/registry/implementation-map.yaml:L7952-L7961] |
| 295 | External implementation, row 295 | [docs/registry/implementation-map.yaml:L7962-L7971] |
| 296 | External implementation, row 296 | [docs/registry/implementation-map.yaml:L7972-L7981] |
| 297 | External implementation, row 297 | [docs/registry/implementation-map.yaml:L7982-L7991] |
| 298 | External implementation, row 298 | [docs/registry/implementation-map.yaml:L7992-L8001] |
| 299 | External implementation, row 299 | [docs/registry/implementation-map.yaml:L8002-L8011] |
| 300 | External implementation, row 300 | [docs/registry/implementation-map.yaml:L8012-L8021] |
| 301 | External implementation, row 301 | [docs/registry/implementation-map.yaml:L8022-L8031] |
| 302 | External implementation, row 302 | [docs/registry/implementation-map.yaml:L8032-L8043] |
| 303 | External implementation, row 303 | [docs/registry/implementation-map.yaml:L8044-L8053] |
| 304 | External implementation, row 304 | [docs/registry/implementation-map.yaml:L8054-L8063] |
| 305 | External implementation, row 305 | [docs/registry/implementation-map.yaml:L8064-L8073] |
| 306 | External implementation, row 306 | [docs/registry/implementation-map.yaml:L8074-L8083] |
| 307 | External implementation, row 307 | [docs/registry/implementation-map.yaml:L8084-L8093] |
| 308 | External implementation, row 308 | [docs/registry/implementation-map.yaml:L8094-L8103] |
| 309 | External implementation, row 309 | [docs/registry/implementation-map.yaml:L8104-L8113] |
| 310 | External implementation, row 310 | [docs/registry/implementation-map.yaml:L8114-L8123] |
| 311 | External implementation, row 311 | [docs/registry/implementation-map.yaml:L8124-L8133] |
| 312 | External implementation, row 312 | [docs/registry/implementation-map.yaml:L8134-L8143] |
| 313 | External implementation, row 313 | [docs/registry/implementation-map.yaml:L8144-L8155] |
| 314 | External implementation, row 314 | [docs/registry/implementation-map.yaml:L8156-L8165] |
| 315 | External implementation, row 315 | [docs/registry/implementation-map.yaml:L8166-L8175] |
| 316 | External implementation, row 316 | [docs/registry/implementation-map.yaml:L8176-L8185] |
| 317 | External implementation, row 317 | [docs/registry/implementation-map.yaml:L8186-L8195] |
| 318 | External implementation, row 318 | [docs/registry/implementation-map.yaml:L8196-L8205] |
| 319 | External implementation, row 319 | [docs/registry/implementation-map.yaml:L8206-L8215] |
| 320 | External implementation, row 320 | [docs/registry/implementation-map.yaml:L8216-L8227] |
| 321 | External implementation, row 321 | [docs/registry/implementation-map.yaml:L8228-L8237] |
| 322 | External implementation, row 322 | [docs/registry/implementation-map.yaml:L8238-L8247] |
| 323 | External implementation, row 323 | [docs/registry/implementation-map.yaml:L8248-L8257] |
| 324 | External implementation, row 324 | [docs/registry/implementation-map.yaml:L8258-L8267] |
| 325 | External implementation, row 325 | [docs/registry/implementation-map.yaml:L8268-L8279] |
| 326 | External implementation, row 326 | [docs/registry/implementation-map.yaml:L8280-L8290] |
| 327 | External implementation, row 327 | [docs/registry/implementation-map.yaml:L8291-L8300] |
| 328 | External implementation, row 328 | [docs/registry/implementation-map.yaml:L8301-L8310] |
| 329 | External implementation, row 329 | [docs/registry/implementation-map.yaml:L8311-L8323] |
| 330 | External implementation, row 330 | [docs/registry/implementation-map.yaml:L8324-L8333] |
| 331 | External implementation, row 331 | [docs/registry/implementation-map.yaml:L8334-L8343] |
| 332 | External implementation, row 332 | [docs/registry/implementation-map.yaml:L8344-L8353] |
| 333 | `src/digitalmodel/viv_screening/workflow.py::save_echo` | [docs/registry/implementation-map.yaml:L8354-L8363] |
| 334 | External implementation, row 334 | [docs/registry/implementation-map.yaml:L8364-L8373] |
| 335 | External implementation, row 335 | [docs/registry/implementation-map.yaml:L8374-L8383] |
| 336 | External implementation, row 336 | [docs/registry/implementation-map.yaml:L8384-L8393] |
| 337 | External implementation, row 337 | [docs/registry/implementation-map.yaml:L8394-L8403] |
| 338 | External implementation, row 338 | [docs/registry/implementation-map.yaml:L8404-L8413] |
| 339 | External implementation, row 339 | [docs/registry/implementation-map.yaml:L8414-L8423] |
| 340 | External implementation, row 340 | [docs/registry/implementation-map.yaml:L8424-L8435] |
| 341 | External implementation, row 341 | [docs/registry/implementation-map.yaml:L8436-L8445] |
| 342 | External implementation, row 342 | [docs/registry/implementation-map.yaml:L8446-L8457] |
| 343 | External implementation, row 343 | [docs/registry/implementation-map.yaml:L8458-L8470] |
| 344 | External implementation, row 344 | [docs/registry/implementation-map.yaml:L8471-L8480] |
| 345 | External implementation, row 345 | [docs/registry/implementation-map.yaml:L8481-L8492] |
| 346 | External implementation, row 346 | [docs/registry/implementation-map.yaml:L8493-L8505] |
| 347 | External implementation, row 347 | [docs/registry/implementation-map.yaml:L8506-L8517] |
| 348 | External implementation, row 348 | [docs/registry/implementation-map.yaml:L8518-L8527] |
| 349 | External implementation, row 349 | [docs/registry/implementation-map.yaml:L8528-L8537] |
| 350 | External implementation, row 350 | [docs/registry/implementation-map.yaml:L8538-L8547] |
| 351 | External implementation, row 351 | [docs/registry/implementation-map.yaml:L8548-L8560] |
| 352 | External implementation, row 352 | [docs/registry/implementation-map.yaml:L8561-L8572] |
| 353 | External implementation, row 353 | [docs/registry/implementation-map.yaml:L8573-L8582] |
| 354 | External implementation, row 354 | [docs/registry/implementation-map.yaml:L8583-L8596] |
| 355 | External implementation, row 355 | [docs/registry/implementation-map.yaml:L8597-L8608] |
| 356 | External implementation, row 356 | [docs/registry/implementation-map.yaml:L8609-L8619] |
| 357 | External implementation, row 357 | [docs/registry/implementation-map.yaml:L8620-L8632] |
| 358 | External implementation, row 358 | [docs/registry/implementation-map.yaml:L8633-L8647] |
| 359 | External implementation, row 359 | [docs/registry/implementation-map.yaml:L8648-L8657] |
| 360 | External implementation, row 360 | [docs/registry/implementation-map.yaml:L8658-L8667] |
| 361 | External implementation, row 361 | [docs/registry/implementation-map.yaml:L8668-L8677] |
| 362 | External implementation, row 362 | [docs/registry/implementation-map.yaml:L8678-L8687] |
| 363 | External implementation, row 363 | [docs/registry/implementation-map.yaml:L8688-L8701] |
| 364 | External implementation, row 364 | [docs/registry/implementation-map.yaml:L8702-L8714] |
| 365 | External implementation, row 365 | [docs/registry/implementation-map.yaml:L8715-L8724] |
| 366 | External implementation, row 366 | [docs/registry/implementation-map.yaml:L8725-L8739] |
| 367 | External implementation, row 367 | [docs/registry/implementation-map.yaml:L8740-L8749] |
| 368 | External implementation, row 368 | [docs/registry/implementation-map.yaml:L8750-L8759] |
| 369 | External implementation, row 369 | [docs/registry/implementation-map.yaml:L8760-L8773] |
| 370 | External implementation, row 370 | [docs/registry/implementation-map.yaml:L8774-L8783] |
| 371 | External implementation, row 371 | [docs/registry/implementation-map.yaml:L8784-L8793] |
| 372 | External implementation, row 372 | [docs/registry/implementation-map.yaml:L8794-L8803] |
| 373 | External implementation, row 373 | [docs/registry/implementation-map.yaml:L8804-L8813] |
| 374 | External implementation, row 374 | [docs/registry/implementation-map.yaml:L8814-L8823] |
| 375 | External implementation, row 375 | [docs/registry/implementation-map.yaml:L8824-L8833] |
| 376 | External implementation, row 376 | [docs/registry/implementation-map.yaml:L8834-L8843] |
| 377 | External implementation, row 377 | [docs/registry/implementation-map.yaml:L8844-L8856] |
| 378 | External implementation, row 378 | [docs/registry/implementation-map.yaml:L8857-L8866] |
| 379 | External implementation, row 379 | [docs/registry/implementation-map.yaml:L8867-L8876] |
| 380 | External implementation, row 380 | [docs/registry/implementation-map.yaml:L8877-L8886] |
| 381 | External implementation, row 381 | [docs/registry/implementation-map.yaml:L8887-L8896] |
| 382 | External implementation, row 382 | [docs/registry/implementation-map.yaml:L8897-L8908] |
| 383 | External implementation, row 383 | [docs/registry/implementation-map.yaml:L8909-L8918] |
| 384 | External implementation, row 384 | [docs/registry/implementation-map.yaml:L8919-L8928] |
| 385 | External implementation, row 385 | [docs/registry/implementation-map.yaml:L8929-L8938] |
| 386 | External implementation, row 386 | [docs/registry/implementation-map.yaml:L8939-L8948] |
| 387 | External implementation, row 387 | [docs/registry/implementation-map.yaml:L8949-L8958] |
| 388 | External implementation, row 388 | [docs/registry/implementation-map.yaml:L8959-L8970] |
| 389 | External implementation, row 389 | [docs/registry/implementation-map.yaml:L8971-L8980] |
| 390 | External implementation, row 390 | [docs/registry/implementation-map.yaml:L8981-L8990] |
| 391 | External implementation, row 391 | [docs/registry/implementation-map.yaml:L8991-L9000] |
| 392 | External implementation, row 392 | [docs/registry/implementation-map.yaml:L9001-L9010] |
| 393 | External implementation, row 393 | [docs/registry/implementation-map.yaml:L9011-L9020] |
| 394 | External implementation, row 394 | [docs/registry/implementation-map.yaml:L9021-L9030] |
| 395 | External implementation, row 395 | [docs/registry/implementation-map.yaml:L9031-L9040] |
| 396 | External implementation, row 396 | [docs/registry/implementation-map.yaml:L9041-L9050] |
| 397 | External implementation, row 397 | [docs/registry/implementation-map.yaml:L9051-L9060] |
| 398 | External implementation, row 398 | [docs/registry/implementation-map.yaml:L9061-L9070] |
| 399 | External implementation, row 399 | [docs/registry/implementation-map.yaml:L9071-L9080] |
| 400 | External implementation, row 400 | [docs/registry/implementation-map.yaml:L9081-L9090] |
| 401 | External implementation, row 401 | [docs/registry/implementation-map.yaml:L9091-L9100] |
| 402 | External implementation, row 402 | [docs/registry/implementation-map.yaml:L9101-L9110] |
| 403 | External implementation, row 403 | [docs/registry/implementation-map.yaml:L9111-L9120] |
| 404 | External implementation, row 404 | [docs/registry/implementation-map.yaml:L9121-L9130] |
| 405 | External implementation, row 405 | [docs/registry/implementation-map.yaml:L9131-L9140] |
| 406 | External implementation, row 406 | [docs/registry/implementation-map.yaml:L9141-L9152] |
| 407 | External implementation, row 407 | [docs/registry/implementation-map.yaml:L9153-L9162] |
| 408 | External implementation, row 408 | [docs/registry/implementation-map.yaml:L9163-L9172] |
| 409 | External implementation, row 409 | [docs/registry/implementation-map.yaml:L9173-L9182] |
| 410 | External implementation, row 410 | [docs/registry/implementation-map.yaml:L9183-L9192] |
| 411 | External implementation, row 411 | [docs/registry/implementation-map.yaml:L9193-L9202] |
| 412 | External implementation, row 412 | [docs/registry/implementation-map.yaml:L9203-L9212] |
| 413 | External implementation, row 413 | [docs/registry/implementation-map.yaml:L9213-L9222] |
| 414 | External implementation, row 414 | [docs/registry/implementation-map.yaml:L9223-L9232] |
| 415 | External implementation, row 415 | [docs/registry/implementation-map.yaml:L9233-L9242] |
| 416 | External implementation, row 416 | [docs/registry/implementation-map.yaml:L9243-L9252] |
| 417 | External implementation, row 417 | [docs/registry/implementation-map.yaml:L9253-L9262] |
| 418 | External implementation, row 418 | [docs/registry/implementation-map.yaml:L9263-L9272] |
| 419 | External implementation, row 419 | [docs/registry/implementation-map.yaml:L9273-L9282] |
| 420 | External implementation, row 420 | [docs/registry/implementation-map.yaml:L9283-L9292] |
| 421 | External implementation, row 421 | [docs/registry/implementation-map.yaml:L9293-L9302] |
| 422 | External implementation, row 422 | [docs/registry/implementation-map.yaml:L9303-L9312] |
| 423 | External implementation, row 423 | [docs/registry/implementation-map.yaml:L9313-L9322] |
| 424 | External implementation, row 424 | [docs/registry/implementation-map.yaml:L9323-L9332] |
| 425 | External implementation, row 425 | [docs/registry/implementation-map.yaml:L9333-L9342] |
| 426 | External implementation, row 426 | [docs/registry/implementation-map.yaml:L9343-L9352] |
| 427 | External implementation, row 427 | [docs/registry/implementation-map.yaml:L9353-L9362] |
| 428 | External implementation, row 428 | [docs/registry/implementation-map.yaml:L9363-L9372] |
| 429 | External implementation, row 429 | [docs/registry/implementation-map.yaml:L9373-L9382] |
| 430 | External implementation, row 430 | [docs/registry/implementation-map.yaml:L9383-L9392] |
| 431 | External implementation, row 431 | [docs/registry/implementation-map.yaml:L9393-L9402] |
| 432 | External implementation, row 432 | [docs/registry/implementation-map.yaml:L9403-L9412] |
| 433 | External implementation, row 433 | [docs/registry/implementation-map.yaml:L9413-L9422] |
| 434 | External implementation, row 434 | [docs/registry/implementation-map.yaml:L9423-L9432] |
| 435 | External implementation, row 435 | [docs/registry/implementation-map.yaml:L9433-L9442] |
| 436 | External implementation, row 436 | [docs/registry/implementation-map.yaml:L9443-L9452] |
| 437 | External implementation, row 437 | [docs/registry/implementation-map.yaml:L9453-L9462] |
| 438 | External implementation, row 438 | [docs/registry/implementation-map.yaml:L9463-L9472] |
| 439 | External implementation, row 439 | [docs/registry/implementation-map.yaml:L9473-L9482] |
| 440 | External implementation, row 440 | [docs/registry/implementation-map.yaml:L9483-L9493] |


## 10. Coverage and open items

Read coverage: all package Python files (`__init__.py`, `inputs.py`, `workflow.py`), the example README/input, engine routing/echo call sites, VIV analytical core, fatigue curve/damage dependencies, workflow/routing registries, and the two workflow/core test files plus the selected durable test. [src/digitalmodel/viv_screening/__init__.py:L1-L1] [src/digitalmodel/viv_screening/inputs.py:L1-L125] [src/digitalmodel/viv_screening/workflow.py:L1-L273] [tests/workflows/test_viv_screening_workflow.py:L1-L306] [tests/orcaflex/test_viv_screening.py:L1-L161] [tests/workflows/test_durable_workflows.py:L193-L227]

Documentation discovery under `docs/` found the operator map, the riser coverage analysis, a separate synthetic VIV parametric plan, and the implementation-map narrative; the workflow registry and module-routing entries were also read. [docs/maps/digitalmodel-operator-map.md:L107-L107] [docs/domains/risers/scr-slwr-coverage-2026-10.md:L181-L183] [docs/plans/2026-07-11-issue-1505-synthetic-viv-parametric-hf-pilot.md:L138-L157] [docs/registry/implementation-map.md:L18-L24] [docs/registry/workflows.yaml:L454-L463] [docs/registry/module-routing.yaml:L686-L691]

The separate parametric workflow/API publication tests exercise another router and do not supply damage validation for this package; the older plan's no-route statement is historical, since the pinned engine now has a `viv_screening` arm. [tests/workflow_api/test_run_workflow_viv_screening.py:L1-L9] [docs/plans/2026-07-11-issue-1505-synthetic-viv-parametric-hf-pilot.md:L70-L75] [src/digitalmodel/engine.py:L330-L333]

| Open item | Responsible role | Tracking | Evidence and limit |
| --- | --- | --- | --- |
| Licensed-source qualification, complete clause titles/C203 edition and checked citation metadata; section 3 UNVERIFIED claim | Engineering calculations quality manager | issue pending (1) | No licensed text or coefficient table is reproduced; [src/digitalmodel/viv_screening/workflow.py:L18-L22] [src/digitalmodel/viv_screening/workflow.py:L173-L191] [src/digitalmodel/fatigue/sn_curves.py:L1-L20] |
| Missing targeted sweep-site assertions in section 6 | Maintainers | issue pending (2) | Existing checks do not cover every fallback; no detection check exists at the explicitly marked sites; [tests/workflows/test_viv_screening_workflow.py:L71-L306] |
| Non-pinned higher-mode frequencies and undocumented tension factors | Maintainers | issue pending (5) | No detection check exists beyond C3 zero-tension pinned first-mode validation; [src/digitalmodel/orcaflex/viv_screening.py:L172-L218] |
| Mass-ratio and added-mass-inclusive beam-mass consistency | Maintainers | issue pending (6) | No consistency check or corresponding detection check exists; [src/digitalmodel/viv_screening/inputs.py:L63-L85] [src/digitalmodel/viv_screening/inputs.py:L118-L124] |
| Implementation-map relation corrections, external discovery identities and VIV family coverage | Maintainers | issue pending (3) | Six in-package map rows retained in section 9; `_beam` parses, and `save_echo` serializes; [src/digitalmodel/viv_screening/inputs.py:L63-L85] [src/digitalmodel/viv_screening/workflow.py:L254-L273] |
| Deputy assignment, shared-log/index entry and replacement of draft references by issue numbers; section 8 UNVERIFIED claim | Maintainers | issue pending (4) | Routing entry provides module references but no deputy or shared-log record; [docs/registry/module-routing.yaml:L686-L691] |
| Independent corpus and nonuniform/detailed response validation | Engineering calculations quality manager | #808, #816 | Synthetic analytic goldens do not establish corpus validity; [examples/workflows/riser-viv-screening/README.md:L76-L81] [src/digitalmodel/viv_screening/workflow.py:L179-L190] |
| Unified riser schema, unit/coordinate contract and shear representation | Maintainers | #809 | Workflow-local parser uses a speed envelope; [src/digitalmodel/viv_screening/inputs.py:L39-L60] [examples/workflows/riser-viv-screening/README.md:L36-L49] [examples/workflows/riser-viv-screening/README.md:L79-L80] |
| Report appendix and detailed-response handoff | Maintainers | #814, #812 | This route writes tables/echo and a detailed-tool recommendation; [src/digitalmodel/viv_screening/workflow.py:L163-L169] [src/digitalmodel/viv_screening/workflow.py:L194-L203] [examples/workflows/riser-viv-screening/README.md:L79-L81] [docs/domains/risers/scr-slwr-coverage-2026-10.md:L229-L229] |

Only synthetic example inputs and public source behavior are recorded; the example explicitly identifies its numbers as synthetic. [examples/workflows/riser-viv-screening/README.md:L15-L19]
