# RAO spectral fatigue knowledge pack

| Header | Value |
|---|---|
| Module | `rao_spectral_fatigue` |
| Repository | `digitalmodel` |
| Package path | `src/digitalmodel/rao_spectral_fatigue` |
| Pack revision | 0 |
| Code revision | `29a87f1aba14aa22067794846aa5c2ec8bf407c1` |
| Analysis family | Engineering calculations (EC) |
| Quality role | Engineering calculations family quality manager |
| Status | DRAFT |

Repository citations resolve at the code revision above; implementation-map citations in section 9 resolve at the separately named map revision.

## 1. Purpose and scope

`router` converts a stress RAO and parametric wave spectra into stress PSDs, delegates damage evaluation to `spectral_fatigue.router`, and copies the result and screening status back into the input configuration. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L34-L81]

The transfer uses a real non-negative scalar RAO, linear interpolation and squared amplitude; it contains no complex phase, coordinate transformation or directional combination. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L116-L143]

The routing registry assigns development maturity; the engine selects this workflow using `basename: rao_spectral_fatigue`. [docs/registry/module-routing.yaml:L608-L613] [src/digitalmodel/engine.py:L353-L358]

Scope covers both package Python files and the routed example; the downstream damage library remains a dependency rather than a second implementation in this package. [src/digitalmodel/rao_spectral_fatigue/__init__.py:L1-L8] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L29-L81] [docs/registry/workflows.yaml:L198-L207]

## 2. Inputs and outputs

Inputs are entries of `cfg["rao_spectral_fatigue"]`; the router forwards the life, design factor, method and S-N mapping to the downstream workflow. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L34-L75]

| Input | Unit and implemented restriction | Default and source |
|---|---|---|
| `locations`, `sea_states` | Non-empty lists; individual mappings are assumed | Required; `_locations`, `_sea_states`. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L146-L157] |
| `rao_frequency_Hz` | Hz; list, at least 2 entries, strictly increasing after float conversion; no finite or non-negative check | Required at each location. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L124-L143] |
| `rao_stress_MPa_per_m` | MPa/m of wave amplitude; same list length, non-negative after float conversion; no finite check | Required at each location. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L124-L143] |
| `wave_spectrum` | Mapping; `type` in `jonswap`, `pierson_moskowitz`, `bretschneider` | `jonswap` when type absent; `_wave_spectrum` requires mapping. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L99-L114] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L160-L164] |
| `Hs`, `Tp` | m and s; positive, NaN rejected, positive infinity not rejected | Required; units from generator, validation by `_positive`. [src/digitalmodel/hydrodynamics/wave_spectra.py:L60-L69] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L104-L105] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L174-L181] |
| `gamma` | Dimensionless float; no domain check here | Generator-specific default is encoded at the cited line; no standard coefficient reproduced here. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L108-L110] |
| `occurrence_fraction` | Dimensionless positive weight; neither upper bound nor sum checked here | 1.0; `_occurrence`. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L167-L171] |
| `design_life_years`, `dff` | Years and dimensionless; downstream positive-float checks | No local default; base configuration supplies values. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L62-L68] [src/digitalmodel/spectral_fatigue/workflow.py:L37-L42] [src/digitalmodel/spectral_fatigue/workflow.py:L182-L190] [src/digitalmodel/base_configs/modules/rao_spectral_fatigue/rao_spectral_fatigue.yml:L3-L14] |
| `method`, `sn_curve` | Method selector and one-slope S-N mapping; downstream interprets slope and log intercept | `dirlik`; base configuration supplies the S-N mapping. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L62-L68] [src/digitalmodel/spectral_fatigue/workflow.py:L130-L143] |
| `id`, `name`, `output_dir` | Text identifier and filesystem destination | Identifier falls through `id`, `name`, `location`; destination `results`. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L42-L43] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L62-L68] |

| Output | Unit / format | Source |
|---|---|---|
| Intermediate `frequency_Hz`, `stress_psd_MPa2_Hz` | Hz and MPa²/Hz; lists passed in memory | `_stress_psd_from_rao` converts angular-frequency density with the matching Jacobian. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L116-L121] |
| `locations`, governing location, life and margin | Annual damage in year⁻¹, life in years, dimensionless margin and Boolean passes | Downstream summary and governing minimum margin. [src/digitalmodel/spectral_fatigue/workflow.py:L54-L75] [src/digitalmodel/spectral_fatigue/workflow.py:L113-L127] |
| `results_csv`, `summary_csv` | CSV detail and location-summary paths; relative output directory uses configuration directory | Downstream path resolution and writer. [src/digitalmodel/spectral_fatigue/workflow.py:L193-L229] |
| `wave_to_stress_transfer`, `screening_status` | Formula string and `pass`/`fail` status | RAO result copied from downstream result and labelled with the transfer formula. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L77-L81] [src/digitalmodel/spectral_fatigue/workflow.py:L68-L75] |

There is no file-format coordinate frame or phase input: only scalar frequency and stress-amplitude arrays enter the calculation; interpolation outside the RAO grid holds the endpoint amplitude. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L119-L143]

## 3. Method and governing standards

| Method / use | Implementing definition and reference as recorded |
|---|---|
| Wave-to-stress transfer | `_stress_psd_from_rao`: `S_stress(f) = |H(f)|**2 * S_wave(f)`, including angular-frequency to Hz conversion; module docstring cites DNV-RP-F204 and DNV-RP-C203 section 4 without editions or a F204 clause. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L9-L21] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L84-L121] |
| Wave spectrum generation | `WaveSpectra.jonswap` cites DNV-RP-C205 section 3.5.2; `pierson_moskowitz` cites section 3.5.1; no edition is recorded there. [src/digitalmodel/hydrodynamics/wave_spectra.py:L60-L73] [src/digitalmodel/hydrodynamics/wave_spectra.py:L111-L123] |
| Bretschneider | Selected by `_stress_psd_from_rao`; generator docstring provides units and formula but no edition or clause. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L111-L114] [src/digitalmodel/hydrodynamics/wave_spectra.py:L139-L161] |
| Damage chain | `router` delegates to the downstream method table: Dirlik, narrow-band, Wirsching-Light and Benasciutti-Tovo; the library names DNV-RP-C203 (2021), section 4.6, “Frequency domain fatigue analysis”. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L62-L75] [src/digitalmodel/spectral_fatigue/workflow.py:L16-L34] [src/digitalmodel/fatigue/spectral_fatigue.py:L23-L28] |
| Accumulation and acceptance | `_evaluate_location` weights annual damage by occurrence, uses reciprocal damage for life and compares life with design life times design factor. [src/digitalmodel/spectral_fatigue/workflow.py:L91-L125] |

Licensed clause text not checked against the licensed copy; no tabulated standard values are reproduced. The module's generic C203 section 4 reference and the dependency's specific 2021 section 4.6 reference do not establish contradictory editions, but leave the module edition unspecified. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L20-L21] [src/digitalmodel/fatigue/spectral_fatigue.py:L23-L28]

The local result adds a formula string rather than a `Citation` sidecar; the downstream result dictionary also has no citation field. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L77-L81] [src/digitalmodel/spectral_fatigue/workflow.py:L60-L75]

## 4. Validated worked examples

Validated examples: 0; regression examples: 2. The constant-gain comparison uses another repository implementation, while the registry case checks the routed example and its output invariants; neither supplies an independent published engineering benchmark. [tests/workflows/test_durable_workflows.py:L402-L420] [tests/workflows/test_durable_workflows.py:L1952-L2019]

| Case | Source / input | Expected result and tolerance | Existing test node ID | Offline result |
|---|---|---|---|---|
| R1 registry regression | `examples/workflows/rao-spectral-fatigue/input.yml`; illustrative RAOs and weighted sea states | Recorded transfer string, consistent screening status, Dirlik selection, two location identities and output files; exact categorical equality. [tests/workflows/test_durable_workflows.py:L55-L92] [tests/workflows/test_durable_workflows.py:L402-L420] | `tests/workflows/test_durable_workflows.py::test_workflow_registry[rao-spectral-fatigue]` | BASE: PASS; AFTER: PASS |
| R2 constant-gain regression | Synthetic configuration written by the test; expected annual damage from `_spectral_fatigue_annual_damage` | Annual damage (year⁻¹) and reciprocal life (years), relative error at most 1e-9; tolerance is the test's regression criterion. [tests/workflows/test_durable_workflows.py:L1952-L2019] | `tests/workflows/test_durable_workflows.py::test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | BASE: PASS; AFTER: PASS |

Execution record: both tests ran without a solver on the pinned code revision, using `PYTHONPATH=src uv run --no-sources python -m pytest <two node IDs> -q -rA -p no:cacheprovider --basetemp=<scratch>`, with the repository's existing addopts. The registry test was read from a temporary repository export so its result files stayed in scratch; no tests were rewritten. [tests/workflows/test_durable_workflows.py:L55-L92] [tests/workflows/test_durable_workflows.py:L1952-L2019]

## 5. QA gates

Gate allocation follows AQG r0, section 10, “Python calculations”, with G5 from section 9; criteria below are pack checks rather than licensed-standard limits.

| Gate | Applicability | Module-specific checks |
|---|---|---|
| G0 | Applicable: configuration, units and provenance | C01, C02, C03 |
| G1 | Applicable: Python import and offline tests; no external program file is written by this router | C04. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L29-L81] |
| G2 | Applicable: hand transfer checks and an internal regression comparison | C05, C06, C07 |
| G3 | NOT_APPLICABLE: no time-domain integration in the RAO transfer | Scalar PSD transfer only. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L84-L121] |
| G4 | NOT_APPLICABLE: no mesh or diffraction solution is generated | Inputs are arrays and spectrum mappings. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L124-L164] |
| G5 | Applicable: input-to-output evidence for the routed calculation | C08; a signed report appendix remains outside the current workflow output contract. [src/digitalmodel/spectral_fatigue/workflow.py:L60-L75] |

| Check | Executable procedure | Numeric criterion and unit | Recorded evidence / result |
|---|---|---|---|
| C01 / G0 | Call `_stress_rao` with duplicate frequencies, a negative amplitude, a one-point list, and unequal lengths | 0 accepted malformed arrays out of 4 cases (count) | PASS: 0 accepted; shape checks are implemented. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L124-L143] |
| C02 / G0 | Probe `_stress_rao` with NaN frequency and infinite amplitude, `_occurrence` with NaN and a weight above unity, `_positive` with positive infinity | 0 accepted invalid configurations out of 5 cases (count); finite numeric quantities and weights in (0,1] | FAIL at the unchanged code pin: 5 accepted; issue pending (1). [src/digitalmodel/rao_spectral_fatigue/workflow.py:L137-L142] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L167-L181] |
| C03 / G0 | Audit each section-6 default, conversion and unit site against section 2 before using the router; require explicit RAO amplitude convention, spectrum type, gamma, occurrence, identifier and output destination in a design basis | 0 unreviewed sites among 19 sweep entries (count) | PASS for documentation coverage: 19 recorded; this is an audit procedure, not runtime enforcement. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L34-L79] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L99-L181] |
| C04 / G1 | Import `digitalmodel.rao_spectral_fatigue.workflow`; run both section-4 node IDs in the recorded offline form | 0 import errors and 0 failing tests out of 2 cases (count) | PASS at BASE and AFTER. [tests/workflows/test_durable_workflows.py:L402-L420] [tests/workflows/test_durable_workflows.py:L1952-L2019] |
| C05 / G2 | Run R2; compare `annual_damage` and `fatigue_life_years` with the repository fixed-gain route | Relative error ≤1e-9 (dimensionless), annual damage year⁻¹ and life years | PASS at BASE; internal regression only. [tests/workflows/test_durable_workflows.py:L2006-L2019] |
| C06 / G2 | For each supported spectrum, call `_stress_psd_from_rao([0,1],[2,2],{"type":kind,"Hs":2,"Tp":8},"synthetic")`; trapezoid-integrate PSD in Hz | Absolute error ≤1e-10 MPa² against 1 MPa², from constant gain squared times the generator's target wave variance | PASS: error <1e-10 MPa²; a code-derived identity, not a licensed tabulated value. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L116-L121] [src/digitalmodel/hydrodynamics/wave_spectra.py:L29-L44] |
| C07 / G2 | Repeat C06 with amplitudes `[1,3]`; evaluate the hand interpolant `1+2*f` on the returned frequency grid and multiply its square by the same generated wave density in Hz | Maximum absolute PSD error ≤1e-10 MPa²/Hz over all 3 spectrum types | PASS: error <1e-10 MPa²/Hz; isolates interpolation and unit conversion from damage estimation. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L99-L121] |
| C08 / G5 | Read the registry case's detail and summary CSVs; for each location sum weighted annual damage and compare reciprocal life; hash the input and both CSVs into an evidence manifest and recompute hashes | 0 absent artefacts, 0 hash mismatches (count); relative damage and life error ≤1e-9 (dimensionless) | PASS: both CSVs and the input present, 0 hash mismatches, damage/life errors below criterion; evidence manifest retained with the check records. [docs/registry/workflows.yaml:L198-L207] [src/digitalmodel/spectral_fatigue/workflow.py:L91-L127] |

C02 is a confirmed pre-existing input-validation defect, not a failure introduced by this documentation; it remains an engineering-use limit until corrected, rather than a claim that the calculation passes every gate. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L137-L142] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L167-L181]

## 6. Known failure modes and pitfalls

sweep: 19 found

The sweep examined both Python files; the commands were `rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|float\(|int\(|\.get\(|unit|Hz|MPa|interp|cache' src/digitalmodel/rao_spectral_fatigue --glob '*.py'` followed by a line-by-line review of all 181 workflow lines and all 8 initializer lines. No swallowing `except`, record-skipping `continue`, error-path empty return or empty-result cache write appears in those files; the empty lists are normal accumulators. [src/digitalmodel/rao_spectral_fatigue/__init__.py:L1-L8] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L1-L181]

Each row denotes one reviewed behavioral site, including defaults that are intentional but can hide an omitted design choice; counts are sites, not a count of distinct defects.

| Site | Symptom / cause at the pin | Detection | Tracking / observation date |
|---|---|---|---|
| S01 | Missing or falsy settings become an empty mapping before the non-empty location check. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L34-L36] | C03; `_locations` then rejects the missing list | issue pending (2); 2026-10-18 |
| S02 | Missing identifiers fall back to `name` or the same generic label, allowing collisions. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L42-L43] | C03; no runtime uniqueness detection check exists | issue pending (2); 2026-10-18 |
| S03 | Missing method selects Dirlik without an explicit design choice. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L62-L66] | C03 | issue pending (2); 2026-10-18 |
| S04 | Missing output destination selects `results`. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L67-L68] | C03 | issue pending (2); 2026-10-18 |
| S05 | Absent spectrum type selects JONSWAP. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L99-L103] | C03 | issue pending (2); 2026-10-18 |
| S06 | Hs and Tp are assumed to use the generator's m and s convention, with no input unit tag. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L104-L110] [src/digitalmodel/hydrodynamics/wave_spectra.py:L60-L69] | C03 | issue pending (3); 2026-10-18 |
| S07 | Gamma uses `float` on external input and a fallback peak parameter, without finite/domain or locale validation. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L108-L110] | C03; no runtime gamma detection check exists | issue pending (1); 2026-10-18 |
| S08 | Returned angular frequencies are assumed rad/s and converted to Hz. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L116-L116] | C06, C07 | issue pending (3); 2026-10-18 |
| S09 | Density is assumed per rad/s and converted to per Hz with the Jacobian. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L117-L117] | C06, C07 | issue pending (3); 2026-10-18 |
| S10 | Out-of-band frequencies silently hold endpoint RAO values. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L119-L119] | C03; no runtime support-coverage detection check exists | issue pending (2); 2026-10-18 |
| S11 | Squaring a scalar MPa/m gain assumes consistent stress amplitude semantics and discards sign; example comment instead says stress range. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L119-L120] [examples/workflows/rao-spectral-fatigue/input.yml:L12-L15] | C03; no physical amplitude-versus-range detection check exists | issue pending (3); 2026-10-18 |
| S12 | Frequency `float` conversion has no thousands/decimal separator policy. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L137-L137] | No detection check exists | issue pending (1); 2026-10-18 |
| S13 | Stress `float` conversion has no thousands/decimal separator policy. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L138-L138] | No detection check exists | issue pending (1); 2026-10-18 |
| S14 | Increasing-order comparisons do not reject NaN or enforce finite, non-negative frequency values. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L139-L140] | C02 | issue pending (1); 2026-10-18 |
| S15 | The stress lower-bound comparison does not reject NaN or infinity. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L141-L142] | C02 | issue pending (1); 2026-10-18 |
| S16 | Occurrence defaults to full exposure and uses `float` without locale policy. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L167-L168] | C03; no locale detection check exists | issue pending (1); 2026-10-18 |
| S17 | Positive-only occurrence check allows non-finite values and weights above unity; no total-weight check exists. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L167-L171] | C02 for individual weights; no sum detection check exists | issue pending (1); 2026-10-18 |
| S18 | `_positive` uses `float` without a locale policy for Hs and Tp. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L174-L178] | No detection check exists | issue pending (1); 2026-10-18 |
| S19 | `_positive` excludes NaN but accepts positive infinity. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L178-L181] | C02 | issue pending (1); 2026-10-18 |

Additional pitfalls: the initializer describes the damage-chain location differently from the workflow's actual import; follow the actual import rather than the prose path. [src/digitalmodel/rao_spectral_fatigue/__init__.py:L3-L7] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L29-L29]

No incident was supplied for this module; #904 records the feature and constant-gain acceptance intent, rather than an incident. Map issue #2543 concerns other spectrum implementations; the module uses `WaveSpectra`, whose generator normalizes the discrete spectrum to target height, so no direct impact is established by that issue alone. C06 detects a changed energy normalization on this route. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L97-L114] [src/digitalmodel/hydrodynamics/wave_spectra.py:L29-L44] [src/digitalmodel/hydrodynamics/wave_spectra.py:L93-L96]

## 7. Lessons log

Append-only pack observations; the record identifier is local to this pack rather than a shared-log registration.

| Date | Record | Lesson / evidence | Pack change | Shared-log entry |
|---|---|---|---|---|
| 2026-10-18 | 0-1 | A constant-gain comparison uses the same repository damage and spectrum chain; it establishes consistency, not independent physical validation. [tests/workflows/test_durable_workflows.py:L1952-L2019] | Classify both examples as regression in section 4 | Not registered; issue pending (4) |
| 2026-10-18 | 0-2 | Ordered/non-negative comparisons alone leave non-finite values admissible. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L137-L142] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L167-L181] | Add C02 and explicit numerical pitfalls | Not registered; issue pending (1) |

## 8. Ownership

Required review roles: engineering calculations family quality manager and engineering calculations deputy quality manager; the producing role maintains calculation documentation, and unresolved standards, tolerance or interpretation choices go to the family quality manager, then to the maintainer for cross-family decisions. These are review requirements, not a claim of completed review. (AQG r0, section 11, “Sign-off and independence”.)

## 9. Dependencies and implementation inventory

`router` depends on `spectral_fatigue.workflow.router`; `_stress_psd_from_rao` imports NumPy and `hydrodynamics.wave_spectra.WaveSpectra`, and the downstream workflow imports `fatigue.spectral_fatigue` estimators. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L29-L29] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L95-L114] [src/digitalmodel/spectral_fatigue/workflow.py:L16-L22]

The engine is the routed entry point; the durable registry names the example input, three expected outputs and the registry test. No solver version is selected by this package's computation. [src/digitalmodel/engine.py:L353-L358] [docs/registry/workflows.yaml:L198-L207] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L34-L121]

Inventory references use `docs/registry/implementation-map.yaml` from map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, read through `origin/codex/implmap-work`; its declared data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`. Map relations below are recorded classifications, not new equivalence or solver validations. [docs/registry/implementation-map.yaml:L1-L3]

Audit record: `wave-spectra` has 1 in-package row and 141 total rows; `sn-curves-and-miner-damage` has 1 in-package row and 392 total rows. Both families are counted, in map order because the in-package counts tie. All 533 rows are inventoried below, including tests and module-level definitions.

The search procedure uses each `search_signals[].pattern` unchanged with Python `re`, case-sensitive, on all package `.py` files at the code pin; no signal matches the package-relative path `rao_spectral_fatigue` case-insensitively, so none is omitted. Each matching line is assigned to the smallest enclosing function/class, or to one module-level definition. The same patterns were also searched across all tracked repository `.py` files, including `scripts/`; broad-search counts were 496 files / 3232 matching lines for wave spectra and 321 files / 4452 matching lines for S-N/Miner. These are discovery counts, not implementation counts.

| Counted family | Package hit definition | Classification and evidence |
|---|---|---|
| Wave spectra | `__init__.py` module-level | Docstring only; helper/non-computing. [src/digitalmodel/rao_spectral_fatigue/__init__.py:L1-L8] |
| Wave spectra | `workflow.py` module-level | Docstring and allowed-type set; no second calculation. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L1-L31] |
| Wave spectra | `workflow.py` router | Listed in section 3; wraps the listed transfer and delegates damage. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L34-L81] |
| Wave spectra | `_stress_psd_from_rao` | Listed map implementation and section-3 transfer. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L84-L121] |
| Wave spectra | `_wave_spectrum` | Input mapping accessor/validator; no calculation. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L160-L164] |
| S-N/Miner | `workflow.py` module-level | Reference docstring only. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L14-L21] |
| S-N/Miner | `router` | Listed map caller, delegates to the downstream chain; no local damage formula. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L62-L81] |

Audit result: 0 further independent package definitions, summed over both counted families; inventory and grep meet the acceptance criterion of at most 1. The package's other functions validate or return input data. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L124-L181]

Map follow-up: the implemented scalar RAO transfer is represented by the wave-spectra row and damage delegation by the S-N/Miner row; no additional in-package method family was established. Repository-wide candidates outside the package still need equivalence classification and map reconciliation, issue pending (5); they do not change the package result. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L34-L181]

### wave-spectra inventory

| Row | Path | Definition | Map relation | Map evidence |
|---|---|---|---|---|
| 1 | `docs/domains/orcaflex/examples/raw/K01/PythonController.py` | `Actuator.output` | `independent` | [docs/registry/implementation-map.yaml:L13600-L13611] |
| 2 | `docs/domains/orcaflex/examples/raw/K02/BladedControllerWrapper.py` | `Actuator.output` | `independent` | [docs/registry/implementation-map.yaml:L13612-L13623] |
| 3 | `examples/domains/fpso/fpso_mooring_analysis.py` | `define_environmental_conditions` | `example` | [docs/registry/implementation-map.yaml:L13624-L13635] |
| 4 | `examples/domains/fpso/fpso_mooring_analysis.py` | `generate_visualizations` | `example` | [docs/registry/implementation-map.yaml:L13636-L13647] |
| 5 | `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | `example` | [docs/registry/implementation-map.yaml:L13648-L13662] |
| 6 | `examples/prototypes/orcaflex-agent/generate_test.py` | `main` | `example` | [docs/registry/implementation-map.yaml:L13663-L13674] |
| 7 | `scripts/_seed_eq_mock.py` | `mock_time_series` | `independent` | [docs/registry/implementation-map.yaml:L13675-L13686] |
| 8 | `scripts/benchmark/extract_pdf_plots.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13687-L13697] |
| 9 | `scripts/benchmark/validate_owd_vs_spec.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13698-L13708] |
| 10 | `scripts/extract_property_inventory.py` | `build_dormancy_rules` | `independent` | [docs/registry/implementation-map.yaml:L13709-L13720] |
| 11 | `scripts/generate_env_files.py` | `create_wave_file` | `independent` | [docs/registry/implementation-map.yaml:L13721-L13732] |
| 12 | `scripts/generate_env_files.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L13733-L13746] |
| 13 | `scripts/ingest_orcina_help.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13747-L13757] |
| 14 | `scripts/python/digitalmodel/legacy/19_-_DistanceBetweenTwoVessels.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13758-L13768] |
| 15 | `scripts/python/digitalmodel/legacy/26_-_LinkedStatistics.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13769-L13779] |
| 16 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13780-L13790] |
| 17 | `scripts/python/digitalmodel/legacy/29_-_ExtremeStatisticsRayleigh.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13791-L13801] |
| 18 | `scripts/python/digitalmodel/legacy/30_-_ExtremeStatisticsLikelihood.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13802-L13812] |
| 19 | `scripts/python/digitalmodel/legacy/31_-_ExtremeStatisticsAggregation.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13813-L13823] |
| 20 | `scripts/python/digitalmodel/legacy/32_-_FrequencyDomainResults.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13824-L13834] |
| 21 | `scripts/python/digitalmodel/legacy/33_-_FrequencyDomainMPM.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13835-L13845] |
| 22 | `scripts/python/digitalmodel/legacy/34_-_FrequencyDomainRangeGraph.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13846-L13856] |
| 23 | `scripts/python/digitalmodel/legacy/35_-_FrequencyDomainSpectralDensitySpectralResponse.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13857-L13867] |
| 24 | `scripts/python/digitalmodel/legacy/36_-_FrequencyDomainTimeHistorySynthesis.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L13868-L13878] |
| 25 | `scripts/python/digitalmodel/legacy/analyze_marine_excel.py` | `analyze_excel_file` | `independent` | [docs/registry/implementation-map.yaml:L13879-L13890] |
| 26 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L13891-L13902] |
| 27 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L13903-L13914] |
| 28 | `scripts/python/digitalmodel/orcawave/mcp_orcawave.py` | `freq_analyze` | `independent` | [docs/registry/implementation-map.yaml:L13915-L13926] |
| 29 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_environment_and_simulation` | `independent` | [docs/registry/implementation-map.yaml:L13927-L13938] |
| 30 | `src/digitalmodel/fatigue/scf_library.py` | `short_chord_factor_f1` | `independent` | [docs/registry/implementation-map.yaml:L13939-L13950] |
| 31 | `src/digitalmodel/hydrodynamics/cli.py` | `spectrum_cmd` | `independent` | [docs/registry/implementation-map.yaml:L13951-L13964] |
| 32 | `src/digitalmodel/hydrodynamics/seakeeping.py` | `operability_analysis` | `independent` | [docs/registry/implementation-map.yaml:L13965-L13976] |
| 33 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra._normalize_to_significant_height` | `independent` | [docs/registry/implementation-map.yaml:L13977-L13988] |
| 34 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.jonswap` | `independent` | [docs/registry/implementation-map.yaml:L13989-L14004] |
| 35 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.pierson_moskowitz` | `independent` | [docs/registry/implementation-map.yaml:L14005-L14017] |
| 36 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.bretschneider` | `independent` | [docs/registry/implementation-map.yaml:L14018-L14030] |
| 37 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.issc` | `independent` | [docs/registry/implementation-map.yaml:L14031-L14043] |
| 38 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.generate_spectrum` | `wrapper` | [docs/registry/implementation-map.yaml:L14044-L14060] |
| 39 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.significant_height_from_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L14061-L14072] |
| 40 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.zero_crossing_period_from_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L14073-L14084] |
| 41 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.spectrum_statistics` | `independent` | [docs/registry/implementation-map.yaml:L14085-L14096] |
| 42 | `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `router` | `independent` | [docs/registry/implementation-map.yaml:L14097-L14110] |
| 43 | `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `_spectrum_name` | `independent` | [docs/registry/implementation-map.yaml:L14111-L14122] |
| 44 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | `independent` | [docs/registry/implementation-map.yaml:L14123-L14134] |
| 45 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | `independent` | [docs/registry/implementation-map.yaml:L14135-L14146] |
| 46 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_complex_rao` | `independent` | [docs/registry/implementation-map.yaml:L14147-L14158] |
| 47 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_raos` | `independent` | [docs/registry/implementation-map.yaml:L14159-L14172] |
| 48 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_motion` | `independent` | [docs/registry/implementation-map.yaml:L14173-L14186] |
| 49 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_velocity` | `independent` | [docs/registry/implementation-map.yaml:L14187-L14199] |
| 50 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_jonswap_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L14200-L14213] |
| 51 | `src/digitalmodel/marine_ops/marine_analysis/analysis/excel_analyzer.py` | `analyze_excel_file` | `copy` | [docs/registry/implementation-map.yaml:L14214-L14225] |
| 52 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L14226-L14237] |
| 53 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L14238-L14249] |
| 54 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._wave_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L14250-L14261] |
| 55 | `src/digitalmodel/marine_ops/operation_envelope.py` | `operation_envelope` | `independent` | [docs/registry/implementation-map.yaml:L14262-L14275] |
| 56 | `src/digitalmodel/marine_ops/operation_envelope.py` | `_significant_response` | `independent` | [docs/registry/implementation-map.yaml:L14276-L14287] |
| 57 | `src/digitalmodel/motion_forecast/wave_forecast.py` | `synthesize_directional_forecast` | `independent` | [docs/registry/implementation-map.yaml:L14288-L14301] |
| 58 | `src/digitalmodel/motion_forecast/wave_source.py` | `jonswap_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L14302-L14315] |
| 59 | `src/digitalmodel/motion_forecast/wave_source.py` | `synthesize_forecast` | `independent` | [docs/registry/implementation-map.yaml:L14316-L14328] |
| 60 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.tz` | `independent` | [docs/registry/implementation-map.yaml:L14329-L14342] |
| 61 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.spectral_density` | `independent` | [docs/registry/implementation-map.yaml:L14343-L14356] |
| 62 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.to_orcaflex_dict` | `independent` | [docs/registry/implementation-map.yaml:L14357-L14368] |
| 63 | `src/digitalmodel/orcawave/motion_statistics.py` | `compute_response_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L14369-L14380] |
| 64 | `src/digitalmodel/orcawave/wave_spectrum.py` | `pierson_moskowitz` | `independent` | [docs/registry/implementation-map.yaml:L14381-L14392] |
| 65 | `src/digitalmodel/orcawave/wave_spectrum.py` | `jonswap` | `independent` | [docs/registry/implementation-map.yaml:L14393-L14408] |
| 66 | `src/digitalmodel/orcawave/wave_spectrum.py` | `bretschneider` | `wrapper` | [docs/registry/implementation-map.yaml:L14409-L14421] |
| 67 | `src/digitalmodel/orcawave/wave_spectrum.py` | `issc_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L14422-L14433] |
| 68 | `src/digitalmodel/orcawave/wave_spectrum.py` | `ochi_hubble._component` | `independent` | [docs/registry/implementation-map.yaml:L14434-L14445] |
| 69 | `src/digitalmodel/orcawave/wave_spectrum.py` | `torsethaugen` | `independent` | [docs/registry/implementation-map.yaml:L14446-L14459] |
| 70 | `src/digitalmodel/orcawave/wave_spectrum.py` | `generate_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L14460-L14477] |
| 71 | `src/digitalmodel/parametric/generate.py` | `_spectral_fatigue_annual_damage` | `independent` | [docs/registry/implementation-map.yaml:L14478-L14489] |
| 72 | `src/digitalmodel/parametric/generate.py` | `_esp_pump_utilisation` | `independent` | [docs/registry/implementation-map.yaml:L14490-L14501] |
| 73 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `_stress_psd_from_rao` | `independent` | [docs/registry/implementation-map.yaml:L14502-L14513] |
| 74 | `src/digitalmodel/solvers/benchmark/solvers.py` | `run_orcaflex` | `independent` | [docs/registry/implementation-map.yaml:L14514-L14525] |
| 75 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.get_wave_waveTrains` | `wrapper` | [docs/registry/implementation-map.yaml:L14526-L14539] |
| 76 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.getJONSWAPGamma` | `independent` | [docs/registry/implementation-map.yaml:L14540-L14551] |
| 77 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.RayleighDampingCoefficients` | `copy` | [docs/registry/implementation-map.yaml:L14552-L14563] |
| 78 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._build_wave_trains` | `independent` | [docs/registry/implementation-map.yaml:L14564-L14577] |
| 79 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._order_wave_train` | `independent` | [docs/registry/implementation-map.yaml:L14578-L14591] |
| 80 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `CampaignSpec.from_legacy_config` | `independent` | [docs/registry/implementation-map.yaml:L14592-L14605] |
| 81 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | `copy` | [docs/registry/implementation-map.yaml:L14606-L14617] |
| 82 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | `copy` | [docs/registry/implementation-map.yaml:L14618-L14629] |
| 83 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_evaluate_location` | `independent` | [docs/registry/implementation-map.yaml:L14630-L14641] |
| 84 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | `independent` | [docs/registry/implementation-map.yaml:L14642-L14655] |
| 85 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_frequency_validation` | `independent` | [docs/registry/implementation-map.yaml:L14656-L14667] |
| 86 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L14668-L14679] |
| 87 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L14680-L14691] |
| 88 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L14692-L14703] |
| 89 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L14704-L14715] |
| 90 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func` | `independent` | [docs/registry/implementation-map.yaml:L14716-L14727] |
| 91 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func._jonswap_wrapper` | `independent` | [docs/registry/implementation-map.yaml:L14728-L14739] |
| 92 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `orcaflexModel` | `independent` | [docs/registry/implementation-map.yaml:L14740-L14751] |
| 93 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `wave` | `independent` | [docs/registry/implementation-map.yaml:L14752-L14763] |
| 94 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_gamma` | `independent` | [docs/registry/implementation-map.yaml:L14764-L14775] |
| 95 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_spectrum` | `independent` | [docs/registry/implementation-map.yaml:L14776-L14791] |
| 96 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `compute_wave_velocity` | `independent` | [docs/registry/implementation-map.yaml:L14792-L14806] |
| 97 | `src/digitalmodel/vessel_seakeeping/workflow.py` | `_spectrum_type` | `independent` | [docs/registry/implementation-map.yaml:L14807-L14818] |
| 98 | `src/digitalmodel/workflows/installation_full_report.py` | `_design` | `independent` | [docs/registry/implementation-map.yaml:L14819-L14830] |
| 99 | `src/digitalmodel/workflows/installation_report_sections.py` | `design_data` | `independent` | [docs/registry/implementation-map.yaml:L14831-L14842] |
| 100 | `src/digitalmodel/workflows/installation_seastates.py` | `_configure` | `independent` | [docs/registry/implementation-map.yaml:L14843-L14854] |
| 101 | `src/digitalmodel/workflows/installation_seastates.py` | `_verify` | `independent` | [docs/registry/implementation-map.yaml:L14855-L14866] |
| 102 | `src/digitalmodel/workflows/installation_seastates.py` | `_change_payload` | `independent` | [docs/registry/implementation-map.yaml:L14867-L14878] |
| 103 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `VesselResponseWorkflow.create` | `independent` | [docs/registry/implementation-map.yaml:L14879-L14892] |
| 104 | `tests/hydrodynamics/rao_analysis/conftest.py` | `wave_spectrum` | `test_helper` | [docs/registry/implementation-map.yaml:L14893-L14906] |
| 105 | `tests/hydrodynamics/test_seakeeping.py` | `jonswap_spectrum` | `test_helper` | [docs/registry/implementation-map.yaml:L14907-L14920] |
| 106 | `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | `test_helper` | [docs/registry/implementation-map.yaml:L14921-L14932] |
| 107 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_jonswap_generates_valid_spectrum` | `test_helper` | [docs/registry/implementation-map.yaml:L14933-L14944] |
| 108 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_zero_crossing_period_relationship` | `test_helper` | [docs/registry/implementation-map.yaml:L14945-L14956] |
| 109 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_elevation_time_series_generation` | `test_helper` | [docs/registry/implementation-map.yaml:L14957-L14968] |
| 110 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_energy_distribution` | `test_helper` | [docs/registry/implementation-map.yaml:L14969-L14980] |
| 111 | `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_jonswap_hs_recovery` | `test_helper` | [docs/registry/implementation-map.yaml:L14981-L14992] |
| 112 | `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_pierson_moskowitz_hs_recovery` | `test_helper` | [docs/registry/implementation-map.yaml:L14993-L15004] |
| 113 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | `test_helper` | [docs/registry/implementation-map.yaml:L15005-L15016] |
| 114 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | `test_helper` | [docs/registry/implementation-map.yaml:L15017-L15028] |
| 115 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | `test_helper` | [docs/registry/implementation-map.yaml:L15029-L15040] |
| 116 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | `test_helper` | [docs/registry/implementation-map.yaml:L15041-L15052] |
| 117 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | `test_helper` | [docs/registry/implementation-map.yaml:L15053-L15064] |
| 118 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | `test_helper` | [docs/registry/implementation-map.yaml:L15065-L15076] |
| 119 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | `test_helper` | [docs/registry/implementation-map.yaml:L15077-L15088] |
| 120 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | `test_helper` | [docs/registry/implementation-map.yaml:L15089-L15100] |
| 121 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | `test_helper` | [docs/registry/implementation-map.yaml:L15101-L15112] |
| 122 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | `test_helper` | [docs/registry/implementation-map.yaml:L15113-L15124] |
| 123 | `tests/orcaflex/test_model_default_tokens.py` | `wave_order_findings` | `test_helper` | [docs/registry/implementation-map.yaml:L15125-L15136] |
| 124 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_floating_campaign` | `test_helper` | [docs/registry/implementation-map.yaml:L15137-L15148] |
| 125 | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py` | `_make_campaign_data` | `test_helper` | [docs/registry/implementation-map.yaml:L15149-L15160] |
| 126 | `tests/structural/fatigue/test_frequency_domain.py` | `TestSpectralMomentsCalculation.test_white_noise_m0_equals_level_times_bandwidth` | `test_helper` | [docs/registry/implementation-map.yaml:L15161-L15172] |
| 127 | `tests/structural/fatigue/test_scatter_fatigue.py` | `_jonswap_hz` | `test_helper` | [docs/registry/implementation-map.yaml:L15173-L15184] |
| 128 | `tests/unit/hydrodynamics/test_rao_analysis.py` | `simple_wave_params` | `test_helper` | [docs/registry/implementation-map.yaml:L15185-L15198] |
| 129 | `tests/unit/hydrodynamics/test_rao_analysis.py` | `TestWaveSpectra.test_jonswap_peak_near_tp` | `test_helper` | [docs/registry/implementation-map.yaml:L15199-L15210] |
| 130 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPiersonMoskowitzExtended.test_pm_peak_near_tp` | `test_helper` | [docs/registry/implementation-map.yaml:L15211-L15222] |
| 131 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestBretschneiderExtended.test_bretschneider_peak_near_tp` | `test_helper` | [docs/registry/implementation-map.yaml:L15223-L15234] |
| 132 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestZeroCrossingPeriodFromSpectrum.test_tz_pm_approximately_0711_tp` | `test_helper` | [docs/registry/implementation-map.yaml:L15235-L15246] |
| 133 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPeakFrequencyFromSpectrum.test_peak_frequency_near_2pi_over_tp` | `test_helper` | [docs/registry/implementation-map.yaml:L15247-L15258] |
| 134 | `tests/workflows/hydrodynamics_assertions.py` | `_assert_wave_spectra` | `test_helper` | [docs/registry/implementation-map.yaml:L15259-L15270] |
| 135 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | `test_helper` | [docs/registry/implementation-map.yaml:L15271-L15284] |
| 136 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | `test_helper` | [docs/registry/implementation-map.yaml:L15285-L15298] |
| 137 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | `test_helper` | [docs/registry/implementation-map.yaml:L15299-L15314] |
| 138 | `tests/workflows/test_durable_workflows.py` | `test_vessel_seakeeping_significant_amplitude_matches_library` | `test_helper` | [docs/registry/implementation-map.yaml:L15315-L15328] |
| 139 | `tests/workflows/test_durable_workflows.py` | `test_esp_pump_hydraulics_tdh_and_sizing_closed_form` | `test_helper` | [docs/registry/implementation-map.yaml:L15329-L15342] |
| 140 | `src/digitalmodel/fatigue/fatg.py` | `fatg_significant_stress_range` | `caller` | [docs/registry/implementation-map.yaml:L15343-L15361] |
| 141 | `src/digitalmodel/fatigue/fatg.py` | `fatg_significant_stress_range.ordinate` | `independent` | [docs/registry/implementation-map.yaml:L15362-L15377] |

### sn-curves-and-miner-damage inventory

| Row | Path | Definition | Map relation | Map evidence |
|---|---|---|---|---|
| 1 | `docs/domains/examples/orcaflex_signal_analysis_example.py` | `example_6_custom_workflow` | `independent` | [docs/registry/implementation-map.yaml:L16814-L16823] |
| 2 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_1_basic_rainflow_counting` | `independent` | [docs/registry/implementation-map.yaml:L16824-L16833] |
| 3 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_4_fatigue_analysis` | `independent` | [docs/registry/implementation-map.yaml:L16834-L16843] |
| 4 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_6_backward_compatibility` | `independent` | [docs/registry/implementation-map.yaml:L16844-L16853] |
| 5 | `docs/domains/examples/signal_analysis_usage_example.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L16854-L16867] |
| 6 | `examples/demos/demo_sn_library.py` | `main` | `example` | [docs/registry/implementation-map.yaml:L16868-L16879] |
| 7 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_sn_curves` | `example` | [docs/registry/implementation-map.yaml:L16880-L16889] |
| 8 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_rainflow_counting` | `example` | [docs/registry/implementation-map.yaml:L16890-L16899] |
| 9 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_damage_accumulation` | `example` | [docs/registry/implementation-map.yaml:L16900-L16909] |
| 10 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_frequency_domain_analysis` | `example` | [docs/registry/implementation-map.yaml:L16910-L16919] |
| 11 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_quick_analysis` | `example` | [docs/registry/implementation-map.yaml:L16920-L16929] |
| 12 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `main` | `example` | [docs/registry/implementation-map.yaml:L16930-L16945] |
| 13 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `list_curves` | `example` | [docs/registry/implementation-map.yaml:L16946-L16955] |
| 14 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `compare_curves` | `example` | [docs/registry/implementation-map.yaml:L16956-L16965] |
| 15 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_2_linear_log` | `example` | [docs/registry/implementation-map.yaml:L16966-L16975] |
| 16 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_3_with_scf` | `example` | [docs/registry/implementation-map.yaml:L16976-L16985] |
| 17 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_4_without_fatigue_limit` | `example` | [docs/registry/implementation-map.yaml:L16986-L16995] |
| 18 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_5_dnv_comparison` | `example` | [docs/registry/implementation-map.yaml:L16996-L17005] |
| 19 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_6_abs_comparison` | `example` | [docs/registry/implementation-map.yaml:L17006-L17015] |
| 20 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_7_reference_comparison` | `example` | [docs/registry/implementation-map.yaml:L17016-L17025] |
| 21 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_8_joint_type_comparison` | `example` | [docs/registry/implementation-map.yaml:L17026-L17035] |
| 22 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_1_basic_sn_curves` | `example` | [docs/registry/implementation-map.yaml:L17036-L17045] |
| 23 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_2_multislope_curves` | `example` | [docs/registry/implementation-map.yaml:L17046-L17055] |
| 24 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_3_time_domain_analysis` | `example` | [docs/registry/implementation-map.yaml:L17056-L17065] |
| 25 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_4_frequency_domain_analysis` | `example` | [docs/registry/implementation-map.yaml:L17066-L17075] |
| 26 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_5_high_stress_range_analysis` | `example` | [docs/registry/implementation-map.yaml:L17076-L17085] |
| 27 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_6_shear_data_analysis` | `example` | [docs/registry/implementation-map.yaml:L17086-L17095] |
| 28 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `run_all_examples` | `example` | [docs/registry/implementation-map.yaml:L17096-L17112] |
| 29 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_well_log_analysis` | `example` | [docs/registry/implementation-map.yaml:L17113-L17122] |
| 30 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_stratigraphic_correlation` | `example` | [docs/registry/implementation-map.yaml:L17123-L17132] |
| 31 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `main` | `example` | [docs/registry/implementation-map.yaml:L17133-L17145] |
| 32 | `scripts/capabilities/build_onepagers.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L17146-L17154] |
| 33 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L17155-L17163] |
| 34 | `scripts/python/digitalmodel/tools/check_process_fatigue_single.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17164-L17173] |
| 35 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_analysis` | `independent` | [docs/registry/implementation-map.yaml:L17174-L17183] |
| 36 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_time_series_components` | `independent` | [docs/registry/implementation-map.yaml:L17184-L17193] |
| 37 | `scripts/python/digitalmodel/tools/process_fatigue_data.py` | `process_fatigue_files` | `independent` | [docs/registry/implementation-map.yaml:L17194-L17203] |
| 38 | `scripts/python/digitalmodel/tools/process_fatigue_final.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17204-L17213] |
| 39 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_fatigue_file` | `independent` | [docs/registry/implementation-map.yaml:L17214-L17223] |
| 40 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_sample_file` | `independent` | [docs/registry/implementation-map.yaml:L17224-L17235] |
| 41 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `batch_process_all_files` | `independent` | [docs/registry/implementation-map.yaml:L17236-L17246] |
| 42 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `rainflow_counting` | `independent` | [docs/registry/implementation-map.yaml:L17247-L17256] |
| 43 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `process_fatigue_file` | `independent` | [docs/registry/implementation-map.yaml:L17257-L17268] |
| 44 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `batch_process` | `independent` | [docs/registry/implementation-map.yaml:L17269-L17280] |
| 45 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v2.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17281-L17290] |
| 46 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v3.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17291-L17300] |
| 47 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v4.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17301-L17310] |
| 48 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v5.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17311-L17320] |
| 49 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v6.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17321-L17330] |
| 50 | `scripts/riser_database/build_tables.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L17331-L17339] |
| 51 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `SNcurve.from_dnv` | `independent` | [docs/registry/implementation-map.yaml:L17340-L17349] |
| 52 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_cycles_to_failure` | `independent` | [docs/registry/implementation-map.yaml:L17350-L17359] |
| 53 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_fatigue_damage` | `independent` | [docs/registry/implementation-map.yaml:L17360-L17371] |
| 54 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.generate_sn_curve_apdl` | `independent` | [docs/registry/implementation-map.yaml:L17372-L17381] |
| 55 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.create_damage_summary_df` | `independent` | [docs/registry/implementation-map.yaml:L17382-L17391] |
| 56 | `src/digitalmodel/benchmarks/inventory.py` | `build_model_inventory` | `independent` | [docs/registry/implementation-map.yaml:L17392-L17401] |
| 57 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `half_cycles` | `independent` | [docs/registry/implementation-map.yaml:L17402-L17411] |
| 58 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `extract` | `independent` | [docs/registry/implementation-map.yaml:L17412-L17423] |
| 59 | `src/digitalmodel/engine.py` | `engine` | `caller` | [docs/registry/implementation-map.yaml:L17424-L17441] |
| 60 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `router` | `independent` | [docs/registry/implementation-map.yaml:L17442-L17454] |
| 61 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `_sn_curve` | `independent` | [docs/registry/implementation-map.yaml:L17455-L17464] |
| 62 | `src/digitalmodel/fatigue/assessment.py` | `_case_histogram` | `independent` | [docs/registry/implementation-map.yaml:L17465-L17474] |
| 63 | `src/digitalmodel/fatigue/assessment.py` | `assess_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L17475-L17488] |
| 64 | `src/digitalmodel/fatigue/counting_contract.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L17489-L17497] |
| 65 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pylife_4pt` | `independent` | [docs/registry/implementation-map.yaml:L17498-L17507] |
| 66 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pypi_rainflow` | `independent` | [docs/registry/implementation-map.yaml:L17508-L17517] |
| 67 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_sigproc_astm` | `independent` | [docs/registry/implementation-map.yaml:L17518-L17527] |
| 68 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_calm_buoy` | `independent` | [docs/registry/implementation-map.yaml:L17528-L17537] |
| 69 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counting` | `independent` | [docs/registry/implementation-map.yaml:L17538-L17547] |
| 70 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counter` | `independent` | [docs/registry/implementation-map.yaml:L17548-L17557] |
| 71 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_struct_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L17558-L17567] |
| 72 | `src/digitalmodel/fatigue/counting_contract.py` | `_is_counting_name` | `independent` | [docs/registry/implementation-map.yaml:L17568-L17577] |
| 73 | `src/digitalmodel/fatigue/damage.py` | `miner_damage` | `independent` | [docs/registry/implementation-map.yaml:L17578-L17587] |
| 74 | `src/digitalmodel/fatigue/damage.py` | `design_life_check` | `independent` | [docs/registry/implementation-map.yaml:L17588-L17599] |
| 75 | `src/digitalmodel/fatigue/environmental_correction.py` | `environment_correction` | `independent` | [docs/registry/implementation-map.yaml:L17600-L17609] |
| 76 | `src/digitalmodel/fatigue/environmental_correction.py` | `apply_environment_to_sn` | `independent` | [docs/registry/implementation-map.yaml:L17610-L17621] |
| 77 | `src/digitalmodel/fatigue/fatg.py` | `fatg_sea_state_damage` | `independent` | [docs/registry/implementation-map.yaml:L17622-L17631] |
| 78 | `src/digitalmodel/fatigue/fatg.py` | `fatg_annual_damage` | `independent` | [docs/registry/implementation-map.yaml:L17632-L17643] |
| 79 | `src/digitalmodel/fatigue/fatigue_reporting.py` | `_sec_fatigue_locations` | `independent` | [docs/registry/implementation-map.yaml:L17644-L17653] |
| 80 | `src/digitalmodel/fatigue/rainflow.py` | `fatigue_life` | `independent` | [docs/registry/implementation-map.yaml:L17654-L17666] |
| 81 | `src/digitalmodel/fatigue/sn_curves.py` | `_compute_sd` | `independent` | [docs/registry/implementation-map.yaml:L17667-L17676] |
| 82 | `src/digitalmodel/fatigue/sn_curves.py` | `_build_curve_dict` | `caller` | [docs/registry/implementation-map.yaml:L17677-L17688] |
| 83 | `src/digitalmodel/fatigue/sn_curves.py` | `get_sn_curve` | `caller` | [docs/registry/implementation-map.yaml:L17689-L17699] |
| 84 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.knee_stress` | `wrapper` | [docs/registry/implementation-map.yaml:L17700-L17711] |
| 85 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.switch_stress` | `wrapper` | [docs/registry/implementation-map.yaml:L17712-L17723] |
| 86 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.cycles` | `wrapper` | [docs/registry/implementation-map.yaml:L17724-L17735] |
| 87 | `src/digitalmodel/fatigue/sn_library.py` | `_knee_stress` | `independent` | [docs/registry/implementation-map.yaml:L17736-L17745] |
| 88 | `src/digitalmodel/fatigue/sn_library.py` | `_switch_stress` | `caller` | [docs/registry/implementation-map.yaml:L17746-L17756] |
| 89 | `src/digitalmodel/fatigue/sn_library.py` | `_bilinear_cycles` | `independent` | [docs/registry/implementation-map.yaml:L17757-L17766] |
| 90 | `src/digitalmodel/fatigue/sn_library.py` | `_sd` | `independent` | [docs/registry/implementation-map.yaml:L17767-L17776] |
| 91 | `src/digitalmodel/fatigue/sn_library.py` | `_build_dnv_curves` | `independent` | [docs/registry/implementation-map.yaml:L17777-L17786] |
| 92 | `src/digitalmodel/fatigue/sn_library.py` | `_build_api_curves` | `independent` | [docs/registry/implementation-map.yaml:L17787-L17796] |
| 93 | `src/digitalmodel/fatigue/sn_library.py` | `_build_bs7608_curves` | `independent` | [docs/registry/implementation-map.yaml:L17797-L17808] |
| 94 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iiw_curves` | `independent` | [docs/registry/implementation-map.yaml:L17809-L17819] |
| 95 | `src/digitalmodel/fatigue/sn_library.py` | `_build_eurocode3_curves` | `independent` | [docs/registry/implementation-map.yaml:L17820-L17830] |
| 96 | `src/digitalmodel/fatigue/sn_library.py` | `_build_norsok_curves` | `caller` | [docs/registry/implementation-map.yaml:L17831-L17841] |
| 97 | `src/digitalmodel/fatigue/sn_library.py` | `_build_aws_curves` | `independent` | [docs/registry/implementation-map.yaml:L17842-L17851] |
| 98 | `src/digitalmodel/fatigue/sn_library.py` | `_build_asme_curves` | `independent` | [docs/registry/implementation-map.yaml:L17852-L17861] |
| 99 | `src/digitalmodel/fatigue/sn_library.py` | `_build_en13445_curves` | `independent` | [docs/registry/implementation-map.yaml:L17862-L17872] |
| 100 | `src/digitalmodel/fatigue/sn_library.py` | `_build_pd5500_curves` | `caller` | [docs/registry/implementation-map.yaml:L17873-L17883] |
| 101 | `src/digitalmodel/fatigue/sn_library.py` | `_build_abs_curves` | `independent` | [docs/registry/implementation-map.yaml:L17884-L17893] |
| 102 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iso19902_curves` | `independent` | [docs/registry/implementation-map.yaml:L17894-L17904] |
| 103 | `src/digitalmodel/fatigue/sn_library.py` | `_build_csa_curves` | `independent` | [docs/registry/implementation-map.yaml:L17905-L17914] |
| 104 | `src/digitalmodel/fatigue/sn_library.py` | `_build_hse_curves` | `independent` | [docs/registry/implementation-map.yaml:L17915-L17925] |
| 105 | `src/digitalmodel/fatigue/sn_library.py` | `_build_doe_curves` | `caller` | [docs/registry/implementation-map.yaml:L17926-L17936] |
| 106 | `src/digitalmodel/fatigue/sn_library.py` | `_build_gl_curves` | `independent` | [docs/registry/implementation-map.yaml:L17937-L17946] |
| 107 | `src/digitalmodel/fatigue/sn_library.py` | `_build_jis_curves` | `independent` | [docs/registry/implementation-map.yaml:L17947-L17956] |
| 108 | `src/digitalmodel/fatigue/sn_library.py` | `_ensure_loaded` | `independent` | [docs/registry/implementation-map.yaml:L17957-L17966] |
| 109 | `src/digitalmodel/fatigue/sn_library.py` | `summary_table` | `caller` | [docs/registry/implementation-map.yaml:L17967-L17978] |
| 110 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_curve_info` | `independent` | [docs/registry/implementation-map.yaml:L17979-L17988] |
| 111 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_sncurve` | `independent` | [docs/registry/implementation-map.yaml:L17989-L17998] |
| 112 | `src/digitalmodel/fatigue/sn_library_api.py` | `get_curve` | `wrapper` | [docs/registry/implementation-map.yaml:L17999-L18010] |
| 113 | `src/digitalmodel/fatigue/sn_library_api.py` | `calculate_endurance` | `wrapper` | [docs/registry/implementation-map.yaml:L18011-L18023] |
| 114 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `wirsching_light_damage` | `independent` | [docs/registry/implementation-map.yaml:L18024-L18033] |
| 115 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `benasciutti_tovo_damage` | `independent` | [docs/registry/implementation-map.yaml:L18034-L18043] |
| 116 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `weld_fatigue_quickcheck` | `independent` | [docs/registry/implementation-map.yaml:L18044-L18054] |
| 117 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L18055-L18064] |
| 118 | `src/digitalmodel/fatigue/woehler_fitting.py` | `generate_test_data` | `independent` | [docs/registry/implementation-map.yaml:L18065-L18074] |
| 119 | `src/digitalmodel/fatigue/woehler_fitting.py` | `_fallback_log_regression` | `independent` | [docs/registry/implementation-map.yaml:L18075-L18084] |
| 120 | `src/digitalmodel/fatigue/workflow.py` | `router` | `caller` | [docs/registry/implementation-map.yaml:L18085-L18102] |
| 121 | `src/digitalmodel/fatigue/workflow.py` | `_curve_ids` | `independent` | [docs/registry/implementation-map.yaml:L18103-L18112] |
| 122 | `src/digitalmodel/fatigue/workflow.py` | `_stress_ranges` | `independent` | [docs/registry/implementation-map.yaml:L18113-L18122] |
| 123 | `src/digitalmodel/fatigue/workflow.py` | `_curve_summary` | `independent` | [docs/registry/implementation-map.yaml:L18123-L18132] |
| 124 | `src/digitalmodel/fatigue/workflow.py` | `_curve_csv_path` | `independent` | [docs/registry/implementation-map.yaml:L18133-L18144] |
| 125 | `src/digitalmodel/fatigue/workflow.py` | `_input_stem` | `independent` | [docs/registry/implementation-map.yaml:L18145-L18154] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.router` | `caller` | [docs/registry/implementation-map.yaml:L18155-L18167] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_sn_data` | `independent` | [docs/registry/implementation-map.yaml:L18168-L18177] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_rainflow_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18178-L18187] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_timetrace` | `independent` | [docs/registry/implementation-map.yaml:L18188-L18200] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.get_rainflow_from_timetrace` | `independent` | [docs/registry/implementation-map.yaml:L18201-L18210] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18211-L18220] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.router` | `wrapper` | [docs/registry/implementation-map.yaml:L18221-L18232] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_lcf_timetrace_rainflow_and_fatigue_damage` | `independent` | [docs/registry/implementation-map.yaml:L18233-L18244] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_damage_output_df_for_timetrace` | `wrapper` | [docs/registry/implementation-map.yaml:L18245-L18256] |
| 135 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_df_definition_for_timetrace` | `independent` | [docs/registry/implementation-map.yaml:L18257-L18266] |
| 136 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18267-L18276] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `SNcurve.cycles_to_failure` | `independent` | [docs/registry/implementation-map.yaml:L18277-L18286] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `RainflowFatigue.count_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18287-L18296] |
| 139 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `MinersRuleDamage.compute` | `independent` | [docs/registry/implementation-map.yaml:L18297-L18306] |
| 140 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `ScatterDiagramFatigue.compute` | `independent` | [docs/registry/implementation-map.yaml:L18307-L18316] |
| 141 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `compute_fatigue_life` | `independent` | [docs/registry/implementation-map.yaml:L18317-L18326] |
| 142 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.add_well_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18327-L18336] |
| 143 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.set_well_order` | `independent` | [docs/registry/implementation-map.yaml:L18337-L18346] |
| 144 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.create_flattened_section` | `independent` | [docs/registry/implementation-map.yaml:L18347-L18356] |
| 145 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `WellCorrelation.add_well_data` | `independent` | [docs/registry/implementation-map.yaml:L18357-L18366] |
| 146 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `CoreAnalysis.correlate_core_to_logs` | `independent` | [docs/registry/implementation-map.yaml:L18367-L18376] |
| 147 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.add_well_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18377-L18386] |
| 148 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.run_comprehensive_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18387-L18396] |
| 149 | `src/digitalmodel/mooring_fatigue/workflow.py` | `router` | `caller` | [docs/registry/implementation-map.yaml:L18397-L18410] |
| 150 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_evaluate_line` | `independent` | [docs/registry/implementation-map.yaml:L18411-L18421] |
| 151 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_history_bins` | `independent` | [docs/registry/implementation-map.yaml:L18422-L18431] |
| 152 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_curve_settings` | `independent` | [docs/registry/implementation-map.yaml:L18432-L18441] |
| 153 | `src/digitalmodel/parametric/generate.py` | `_mooring_fatigue_damage` | `independent` | [docs/registry/implementation-map.yaml:L18442-L18452] |
| 154 | `src/digitalmodel/parametric/generate.py` | `_free_span_utilisation` | `independent` | [docs/registry/implementation-map.yaml:L18453-L18462] |
| 155 | `src/digitalmodel/parametric/generate.py` | `_span_rectification_utilisation` | `independent` | [docs/registry/implementation-map.yaml:L18463-L18472] |
| 156 | `src/digitalmodel/parametric/refresh.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L18473-L18481] |
| 157 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `router` | `caller` | [docs/registry/implementation-map.yaml:L18482-L18493] |
| 158 | `src/digitalmodel/riser_fatigue/touchdown.py` | `assess_touchdown_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L18494-L18503] |
| 159 | `src/digitalmodel/riser_fatigue/workflow.py` | `router` | `caller` | [docs/registry/implementation-map.yaml:L18504-L18515] |
| 160 | `src/digitalmodel/riser_fatigue/workflow.py` | `_viv_damage` | `independent` | [docs/registry/implementation-map.yaml:L18516-L18526] |
| 161 | `src/digitalmodel/riser_fatigue/workflow.py` | `_curve_settings` | `independent` | [docs/registry/implementation-map.yaml:L18527-L18536] |
| 162 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series` | `independent` | [docs/registry/implementation-map.yaml:L18537-L18546] |
| 163 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.get_rainflow_from_timetrace` | `independent` | [docs/registry/implementation-map.yaml:L18547-L18556] |
| 164 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.damage_from_rainflow_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18557-L18566] |
| 165 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `OrcaFlexAdapter.RainflowHalfCycles` | `independent` | [docs/registry/implementation-map.yaml:L18567-L18576] |
| 166 | `src/digitalmodel/signal_processing/signal_analysis/cli.py` | `rainflow_cmd` | `independent` | [docs/registry/implementation-map.yaml:L18577-L18586] |
| 167 | `src/digitalmodel/signal_processing/signal_analysis/core/rainflow.py` | `RainflowCounter.count_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18587-L18596] |
| 168 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve._bilinear` | `independent` | [docs/registry/implementation-map.yaml:L18597-L18606] |
| 169 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve.from_test_data` | `independent` | [docs/registry/implementation-map.yaml:L18607-L18616] |
| 170 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L18617-L18632] |
| 171 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._miners_rule` | `independent` | [docs/registry/implementation-map.yaml:L18633-L18644] |
| 172 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._modified_miners_rule` | `independent` | [docs/registry/implementation-map.yaml:L18645-L18655] |
| 173 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._nonlinear_damage` | `independent` | [docs/registry/implementation-map.yaml:L18656-L18666] |
| 174 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._calculate_allowable_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18667-L18676] |
| 175 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._apply_mean_stress_correction` | `independent` | [docs/registry/implementation-map.yaml:L18677-L18686] |
| 176 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._estimate_cycles_to_failure` | `independent` | [docs/registry/implementation-map.yaml:L18687-L18696] |
| 177 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_equivalent_stress` | `independent` | [docs/registry/implementation-map.yaml:L18697-L18706] |
| 178 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `create_parser` | `independent` | [docs/registry/implementation-map.yaml:L18707-L18716] |
| 179 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `build_config_from_args` | `independent` | [docs/registry/implementation-map.yaml:L18717-L18726] |
| 180 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L18727-L18739] |
| 181 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._analyze_column` | `independent` | [docs/registry/implementation-map.yaml:L18740-L18749] |
| 182 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_outputs` | `independent` | [docs/registry/implementation-map.yaml:L18750-L18759] |
| 183 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_summary` | `independent` | [docs/registry/implementation-map.yaml:L18760-L18769] |
| 184 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/error_handling.py` | `SafeAnalyzer.safe_rainflow_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18770-L18779] |
| 185 | `src/digitalmodel/signal_processing/time_series/time_series_analysis.py` | `TimeSeriesAnalysis.router` | `independent` | [docs/registry/implementation-map.yaml:L18780-L18789] |
| 186 | `src/digitalmodel/signal_processing/time_series/time_series_components.py` | `TimeSeriesComponents.count_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18790-L18799] |
| 187 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_histogram_object_from_orcaflex_run` | `independent` | [docs/registry/implementation-map.yaml:L18800-L18809] |
| 188 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_detailed_histogram_df_columns` | `independent` | [docs/registry/implementation-map.yaml:L18810-L18819] |
| 189 | `src/digitalmodel/solvers/orcaflex/opp_time_series_v2.py` | `OPPTimeSeriesV2.process_histograms` | `independent` | [docs/registry/implementation-map.yaml:L18820-L18829] |
| 190 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_histogram_object_from_orcaflex_run` | `independent` | [docs/registry/implementation-map.yaml:L18830-L18839] |
| 191 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_detailed_histogram_df_columns` | `copy` | [docs/registry/implementation-map.yaml:L18840-L18849] |
| 192 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18850-L18861] |
| 193 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._parallel_fatigue_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18862-L18871] |
| 194 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._analyze_single_trace` | `independent` | [docs/registry/implementation-map.yaml:L18872-L18881] |
| 195 | `src/digitalmodel/spectral_fatigue/workflow.py` | `router` | `caller` | [docs/registry/implementation-map.yaml:L18882-L18893] |
| 196 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_sn_curve` | `independent` | [docs/registry/implementation-map.yaml:L18894-L18903] |
| 197 | `src/digitalmodel/structural/fatigue/__init__.py` | `quick_fatigue_analysis` | `caller` | [docs/registry/implementation-map.yaml:L18904-L18917] |
| 198 | `src/digitalmodel/structural/fatigue/__init__.py` | `get_module_info` | `independent` | [docs/registry/implementation-map.yaml:L18918-L18927] |
| 199 | `src/digitalmodel/structural/fatigue/analysis.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L18928-L18936] |
| 200 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_sn_curve` | `wrapper` | [docs/registry/implementation-map.yaml:L18937-L18947] |
| 201 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_damage_calculator` | `independent` | [docs/registry/implementation-map.yaml:L18948-L18957] |
| 202 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_rainflow_counter` | `independent` | [docs/registry/implementation-map.yaml:L18958-L18967] |
| 203 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_time_series` | `independent` | [docs/registry/implementation-map.yaml:L18968-L18979] |
| 204 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | `independent` | [docs/registry/implementation-map.yaml:L18980-L18989] |
| 205 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.calculate_high_stress_range_life` | `independent` | [docs/registry/implementation-map.yaml:L18990-L18999] |
| 206 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_shear_data` | `independent` | [docs/registry/implementation-map.yaml:L19000-L19009] |
| 207 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_engineering_validation` | `independent` | [docs/registry/implementation-map.yaml:L19010-L19019] |
| 208 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_time_domain_analysis` | `independent` | [docs/registry/implementation-map.yaml:L19020-L19029] |
| 209 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_frequency_domain_analysis` | `independent` | [docs/registry/implementation-map.yaml:L19030-L19039] |
| 210 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L19040-L19048] |
| 211 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L19049-L19060] |
| 212 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation._apply_mean_stress_correction` | `independent` | [docs/registry/implementation-map.yaml:L19061-L19070] |
| 213 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `ModifiedMinersRule.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L19071-L19080] |
| 214 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation.calculate_damage` | `wrapper` | [docs/registry/implementation-map.yaml:L19081-L19094] |
| 215 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._marco_starkey_damage` | `independent` | [docs/registry/implementation-map.yaml:L19095-L19104] |
| 216 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._corten_dolan_damage` | `independent` | [docs/registry/implementation-map.yaml:L19105-L19114] |
| 217 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._double_linear_damage` | `independent` | [docs/registry/implementation-map.yaml:L19115-L19124] |
| 218 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `compare_damage_methods` | `independent` | [docs/registry/implementation-map.yaml:L19125-L19134] |
| 219 | `src/digitalmodel/structural/fatigue/design_code_report.py` | `_build_curve` | `independent` | [docs/registry/implementation-map.yaml:L19135-L19144] |
| 220 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `failure_probability` | `independent` | [docs/registry/implementation-map.yaml:L19145-L19154] |
| 221 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `sensitivity_analysis` | `independent` | [docs/registry/implementation-map.yaml:L19155-L19164] |
| 222 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `monte_carlo_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L19165-L19174] |
| 223 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L19175-L19183] |
| 224 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L19184-L19193] |
| 225 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L19194-L19203] |
| 226 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L19204-L19213] |
| 227 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L19214-L19223] |
| 228 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `compare_frequency_methods` | `independent` | [docs/registry/implementation-map.yaml:L19224-L19233] |
| 229 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `run_sweep` | `independent` | [docs/registry/implementation-map.yaml:L19234-L19243] |
| 230 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `compute_sensitivity._single_damage` | `independent` | [docs/registry/implementation-map.yaml:L19244-L19253] |
| 231 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `_build_scatter_json` | `independent` | [docs/registry/implementation-map.yaml:L19254-L19263] |
| 232 | `src/digitalmodel/structural/fatigue/rainflow.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L19264-L19272] |
| 233 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.count_cycles` | `caller` | [docs/registry/implementation-map.yaml:L19273-L19284] |
| 234 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter._rainflow_count` | `independent` | [docs/registry/implementation-map.yaml:L19285-L19294] |
| 235 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.extract_cycles_with_means` | `caller` | [docs/registry/implementation-map.yaml:L19295-L19306] |
| 236 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_count` | `independent` | [docs/registry/implementation-map.yaml:L19307-L19316] |
| 237 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_with_means` | `independent` | [docs/registry/implementation-map.yaml:L19317-L19326] |
| 238 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `scatter_fatigue_damage` | `independent` | [docs/registry/implementation-map.yaml:L19327-L19336] |
| 239 | `src/digitalmodel/structural/fatigue/skill.py` | `fatigue_assessment` | `independent` | [docs/registry/implementation-map.yaml:L19337-L19348] |
| 240 | `src/digitalmodel/structural/fatigue/sn_curve_plotter.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L19349-L19358] |
| 241 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L19359-L19367] |
| 242 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves._ensure_loaded` | `independent` | [docs/registry/implementation-map.yaml:L19368-L19377] |
| 243 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.register_custom_curve` | `independent` | [docs/registry/implementation-map.yaml:L19378-L19387] |
| 244 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_dnv_c203_curve` | `independent` | [docs/registry/implementation-map.yaml:L19388-L19397] |
| 245 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_curve` | `independent` | [docs/registry/implementation-map.yaml:L19398-L19407] |
| 246 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_multislope_curve` | `independent` | [docs/registry/implementation-map.yaml:L19408-L19417] |
| 247 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `ThicknessCorrection.apply_thickness_effect` | `independent` | [docs/registry/implementation-map.yaml:L19418-L19427] |
| 248 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `SNDataFitting.fit_power_law` | `independent` | [docs/registry/implementation-map.yaml:L19428-L19437] |
| 249 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_dnv_curve` | `independent` | [docs/registry/implementation-map.yaml:L19438-L19447] |
| 250 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_api_curve` | `independent` | [docs/registry/implementation-map.yaml:L19448-L19457] |
| 251 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_bs_curve` | `independent` | [docs/registry/implementation-map.yaml:L19458-L19467] |
| 252 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `_dnv_curves_from_c203` | `independent` | [docs/registry/implementation-map.yaml:L19468-L19477] |
| 253 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_cp` | `independent` | [docs/registry/implementation-map.yaml:L19478-L19487] |
| 254 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_free` | `independent` | [docs/registry/implementation-map.yaml:L19488-L19497] |
| 255 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_chain_seawater_cp` | `independent` | [docs/registry/implementation-map.yaml:L19498-L19507] |
| 256 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `pipeline_girth_weld` | `caller` | [docs/registry/implementation-map.yaml:L19508-L19520] |
| 257 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `scr_touchdown` | `caller` | [docs/registry/implementation-map.yaml:L19521-L19533] |
| 258 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `mooring_chain` | `caller` | [docs/registry/implementation-map.yaml:L19534-L19546] |
| 259 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_compute_example` | `independent` | [docs/registry/implementation-map.yaml:L19547-L19556] |
| 260 | `src/digitalmodel/structural/fatigue_apps/__main__.py` | `run_analysis` | `independent` | [docs/registry/implementation-map.yaml:L19557-L19568] |
| 261 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve._load_sn_parameters` | `independent` | [docs/registry/implementation-map.yaml:L19569-L19578] |
| 262 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve.calculate_cycles_to_failure` | `independent` | [docs/registry/implementation-map.yaml:L19579-L19588] |
| 263 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `FatigueDamageCalculator.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L19589-L19598] |
| 264 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L19599-L19608] |
| 265 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L19609-L19617] |
| 266 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a1` | `independent` | [docs/registry/implementation-map.yaml:L19618-L19627] |
| 267 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a2` | `independent` | [docs/registry/implementation-map.yaml:L19628-L19637] |
| 268 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.cycles_to_failure` | `independent` | [docs/registry/implementation-map.yaml:L19638-L19647] |
| 269 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L19648-L19659] |
| 270 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.export_results` | `independent` | [docs/registry/implementation-map.yaml:L19660-L19669] |
| 271 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `FatigueFileNamer.get_rainflow_filename` | `independent` | [docs/registry/implementation-map.yaml:L19670-L19679] |
| 272 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L19680-L19688] |
| 273 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | `independent` | [docs/registry/implementation-map.yaml:L19689-L19698] |
| 274 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L19699-L19708] |
| 275 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | `independent` | [docs/registry/implementation-map.yaml:L19709-L19718] |
| 276 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `<module>` | `independent` | [docs/registry/implementation-map.yaml:L19719-L19727] |
| 277 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.count_cycles` | `caller` | [docs/registry/implementation-map.yaml:L19728-L19739] |
| 278 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter._rainflow_algorithm` | `independent` | [docs/registry/implementation-map.yaml:L19740-L19749] |
| 279 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.export_results` | `independent` | [docs/registry/implementation-map.yaml:L19750-L19759] |
| 280 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `rainflow_count` | `independent` | [docs/registry/implementation-map.yaml:L19760-L19769] |
| 281 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_time_series` | `independent` | [docs/registry/implementation-map.yaml:L19770-L19779] |
| 282 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_batch` | `independent` | [docs/registry/implementation-map.yaml:L19780-L19791] |
| 283 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.save_cycle_data` | `independent` | [docs/registry/implementation-map.yaml:L19792-L19801] |
| 284 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L19802-L19811] |
| 285 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | `independent` | [docs/registry/implementation-map.yaml:L19812-L19821] |
| 286 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `TensionStressConverter.process_batch` | `independent` | [docs/registry/implementation-map.yaml:L19822-L19831] |
| 287 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L19832-L19841] |
| 288 | `src/digitalmodel/structural/fatigue_apps/visualizer.py` | `generate_all_visualizations` | `independent` | [docs/registry/implementation-map.yaml:L19842-L19851] |
| 289 | `src/digitalmodel/structural/spectral_fatigue.py` | `narrow_band_damage` | `independent` | [docs/registry/implementation-map.yaml:L19852-L19861] |
| 290 | `src/digitalmodel/subsea/connectors/fatigue.py` | `assess_jumper_fatigue` | `caller` | [docs/registry/implementation-map.yaml:L19862-L19873] |
| 291 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `_dnv_params` | `independent` | [docs/registry/implementation-map.yaml:L19874-L19883] |
| 292 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `get_sn_curve` | `independent` | [docs/registry/implementation-map.yaml:L19884-L19895] |
| 293 | `src/digitalmodel/subsea/pipeline/free_span/span_fatigue_damage.py` | `SpanFatigueDamage._load_sn_curve` | `caller` | [docs/registry/implementation-map.yaml:L19896-L19907] |
| 294 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L19908-L19917] |
| 295 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_fatigue_life` | `independent` | [docs/registry/implementation-map.yaml:L19918-L19927] |
| 296 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.analyze_viv_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L19928-L19940] |
| 297 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.cumulative_damage` | `caller` | [docs/registry/implementation-map.yaml:L19941-L19952] |
| 298 | `src/digitalmodel/viv_screening/inputs.py` | `parse_inputs` | `independent` | [docs/registry/implementation-map.yaml:L19953-L19962] |
| 299 | `src/digitalmodel/viv_screening/workflow.py` | `router` | `independent` | [docs/registry/implementation-map.yaml:L19963-L19976] |
| 300 | `src/digitalmodel/viv_screening/workflow.py` | `_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L19977-L19987] |
| 301 | `src/digitalmodel/viv_screening/workflow.py` | `_provenance` | `independent` | [docs/registry/implementation-map.yaml:L19988-L19997] |
| 302 | `src/digitalmodel/web/digitaltwinfeed/app.py` | `page` | `independent` | [docs/registry/implementation-map.yaml:L19998-L20007] |
| 303 | `tests/ansys/padeye_pressure_bundle.py` | `_check_receipt` | `test_helper` | [docs/registry/implementation-map.yaml:L20008-L20017] |
| 304 | `tests/ansys/padeye_pressure_bundle.py` | `verify_pressure_bundle` | `test_helper` | [docs/registry/implementation-map.yaml:L20018-L20029] |
| 305 | `tests/ansys/test_fatigue_postprocessor.py` | `TestThicknessCorrection.test_known_correction` | `test_helper` | [docs/registry/implementation-map.yaml:L20030-L20039] |
| 306 | `tests/ansys/test_padeye_pressure_bundle.py` | `bundle` | `test_helper` | [docs/registry/implementation-map.yaml:L20040-L20049] |
| 307 | `tests/fatigue/test_assessment.py` | `histogram_input` | `test_helper` | [docs/registry/implementation-map.yaml:L20050-L20061] |
| 308 | `tests/fatigue/test_c203_consumers.py` | `_span_input` | `test_helper` | [docs/registry/implementation-map.yaml:L20062-L20071] |
| 309 | `tests/fatigue/test_c203_sn_values.py` | `_non_dnv_bilinear` | `test_helper` | [docs/registry/implementation-map.yaml:L20072-L20081] |
| 310 | `tests/fatigue/test_c203_sn_values.py` | `test_non_dnv_bilinear_switch_stays_at_endurance_limit` | `test_helper` | [docs/registry/implementation-map.yaml:L20082-L20095] |
| 311 | `tests/fatigue/test_c203_variable_amplitude.py` | `_signal_e` | `test_helper` | [docs/registry/implementation-map.yaml:L20096-L20105] |
| 312 | `tests/fatigue/test_c203_variable_amplitude.py` | `_user_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20106-L20115] |
| 313 | `tests/fatigue/test_c203_variable_amplitude.py` | `_span_input` | `test_helper` | [docs/registry/implementation-map.yaml:L20116-L20125] |
| 314 | `tests/fatigue/test_c203_variable_amplitude.py` | `_fresh_standard_curves` | `test_helper` | [docs/registry/implementation-map.yaml:L20126-L20135] |
| 315 | `tests/fatigue/test_c203_variable_amplitude.py` | `_data_dir` | `test_helper` | [docs/registry/implementation-map.yaml:L20136-L20145] |
| 316 | `tests/fatigue/test_damage.py` | `f_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20146-L20156] |
| 317 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_above_ref` | `test_helper` | [docs/registry/implementation-map.yaml:L20157-L20166] |
| 318 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_formula` | `test_helper` | [docs/registry/implementation-map.yaml:L20167-L20176] |
| 319 | `tests/fatigue/test_fatg.py` | `TestAgainstTransliterationSweep.test_all_presets_and_curves` | `test_helper` | [docs/registry/implementation-map.yaml:L20177-L20188] |
| 320 | `tests/fatigue/test_fatg_workflow.py` | `_cfg` | `test_helper` | [docs/registry/implementation-map.yaml:L20189-L20198] |
| 321 | `tests/fatigue/test_fatg_workflow.py` | `_library_reference` | `test_helper` | [docs/registry/implementation-map.yaml:L20199-L20209] |
| 322 | `tests/fatigue/test_fatigue_reporting.py` | `_sample_locations` | `test_helper` | [docs/registry/implementation-map.yaml:L20210-L20219] |
| 323 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls` | `test_helper` | [docs/registry/implementation-map.yaml:L20220-L20232] |
| 324 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.calm_scatter` | `test_helper` | [docs/registry/implementation-map.yaml:L20233-L20242] |
| 325 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.fapps_batch` | `test_helper` | [docs/registry/implementation-map.yaml:L20243-L20252] |
| 326 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_f_curve_at_100mpa` | `test_helper` | [docs/registry/implementation-map.yaml:L20253-L20263] |
| 327 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_d_curve_at_100mpa` | `test_helper` | [docs/registry/implementation-map.yaml:L20264-L20274] |
| 328 | `tests/fatigue/test_weld_fatigue_quickcheck.py` | `_hand_N` | `test_helper` | [docs/registry/implementation-map.yaml:L20275-L20284] |
| 329 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_chain_known_value` | `test_helper` | [docs/registry/implementation-map.yaml:L20285-L20294] |
| 330 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_wire_rope_known_value` | `test_helper` | [docs/registry/implementation-map.yaml:L20295-L20304] |
| 331 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_custom_sn_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20305-L20314] |
| 332 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_single_range_known_damage` | `test_helper` | [docs/registry/implementation-map.yaml:L20315-L20324] |
| 333 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_damage_additivity` | `test_helper` | [docs/registry/implementation-map.yaml:L20325-L20334] |
| 334 | `tests/parametric/test_atlas.py` | `_small_atlas` | `test_helper` | [docs/registry/implementation-map.yaml:L20335-L20344] |
| 335 | `tests/signal_processing/signal_analysis/fatigue_analysis_reference/process_fatigue_final.py` | `process_single_test_file` | `test_helper` | [docs/registry/implementation-map.yaml:L20345-L20354] |
| 336 | `tests/signal_processing/signal_analysis/reference_fatigue_processor.py` | `process_single_test_file` | `test_helper` | [docs/registry/implementation-map.yaml:L20355-L20364] |
| 337 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `<module>` | `test_helper` | [docs/registry/implementation-map.yaml:L20365-L20373] |
| 338 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `analyze_signal` | `test_helper` | [docs/registry/implementation-map.yaml:L20374-L20383] |
| 339 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `main` | `test_helper` | [docs/registry/implementation-map.yaml:L20384-L20395] |
| 340 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.analyze_tension` | `test_helper` | [docs/registry/implementation-map.yaml:L20396-L20405] |
| 341 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.export_results` | `test_helper` | [docs/registry/implementation-map.yaml:L20406-L20415] |
| 342 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `run_rainflow_for_timeseries` | `test_helper` | [docs/registry/implementation-map.yaml:L20416-L20425] |
| 343 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `test_rainflow_for_timeseries` | `test_helper` | [docs/registry/implementation-map.yaml:L20426-L20437] |
| 344 | `tests/signal_processing/signal_analysis/validate_module.py` | `main` | `test_helper` | [docs/registry/implementation-map.yaml:L20438-L20452] |
| 345 | `tests/solvers/test_integration_solvers.py` | `mock_solver_factory.MockSolverFactory.create_fatigue_solver` | `test_helper` | [docs/registry/implementation-map.yaml:L20453-L20462] |
| 346 | `tests/structural/fatigue/conftest.py` | `simple_sn_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20463-L20472] |
| 347 | `tests/structural/fatigue/test_damage_accumulation.py` | `dnv_d_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20473-L20482] |
| 348 | `tests/structural/fatigue/test_damage_accumulation.py` | `simple_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20483-L20492] |
| 349 | `tests/structural/fatigue/test_damage_accumulation.py` | `curve_with_material` | `test_helper` | [docs/registry/implementation-map.yaml:L20493-L20502] |
| 350 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_multislope_curve_implementation` | `test_helper` | [docs/registry/implementation-map.yaml:L20503-L20512] |
| 351 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_curve_continuity` | `test_helper` | [docs/registry/implementation-map.yaml:L20513-L20522] |
| 352 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestFrequencyDomainMigration.test_basic_frequency_analysis` | `test_helper` | [docs/registry/implementation-map.yaml:L20523-L20532] |
| 353 | `tests/structural/fatigue/test_fatigue_reliability.py` | `simple_sn` | `test_helper` | [docs/registry/implementation-map.yaml:L20533-L20542] |
| 354 | `tests/structural/fatigue/test_fatigue_reliability.py` | `mild_sn` | `test_helper` | [docs/registry/implementation-map.yaml:L20543-L20552] |
| 355 | `tests/structural/fatigue/test_fatigue_skill.py` | `_simple_input` | `test_helper` | [docs/registry/implementation-map.yaml:L20553-L20562] |
| 356 | `tests/structural/fatigue/test_fatigue_skill.py` | `TestOffshoreScenario.test_high_cycle_low_stress_result` | `test_helper` | [docs/registry/implementation-map.yaml:L20563-L20574] |
| 357 | `tests/structural/fatigue/test_frequency_domain.py` | `simple_sn_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20575-L20584] |
| 358 | `tests/structural/fatigue/test_rainflow.py` | `default_counter` | `test_helper` | [docs/registry/implementation-map.yaml:L20585-L20594] |
| 359 | `tests/structural/fatigue/test_scatter_fatigue.py` | `simple_sn` | `test_helper` | [docs/registry/implementation-map.yaml:L20595-L20604] |
| 360 | `tests/structural/fatigue/test_scatter_fatigue.py` | `TestFatigueLife.test_fatigue_life_inverse_of_damage` | `test_helper` | [docs/registry/implementation-map.yaml:L20605-L20616] |
| 361 | `tests/structural/fatigue/test_sn_curves.py` | `simple_power_law` | `test_helper` | [docs/registry/implementation-map.yaml:L20617-L20626] |
| 362 | `tests/structural/fatigue/test_sn_curves.py` | `power_law_with_limit` | `test_helper` | [docs/registry/implementation-map.yaml:L20627-L20636] |
| 363 | `tests/structural/fatigue/test_sn_curves.py` | `bilinear_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20637-L20646] |
| 364 | `tests/structural/fatigue/test_sn_curves.py` | `dnv_d_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20647-L20656] |
| 365 | `tests/structural/fatigue/test_sn_curves.py` | `TestBS7608RevisedCurveParameters.test_bs7608_revised_d_known_n_at_50mpa` | `test_helper` | [docs/registry/implementation-map.yaml:L20657-L20666] |
| 366 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f_air_known_n_at_100mpa` | `test_helper` | [docs/registry/implementation-map.yaml:L20667-L20676] |
| 367 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f2_sw_cp_known_n_at_50mpa` | `test_helper` | [docs/registry/implementation-map.yaml:L20677-L20686] |
| 368 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_single_slope_cases` | `test_helper` | [docs/registry/implementation-map.yaml:L20687-L20696] |
| 369 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_multislope_cases` | `test_helper` | [docs/registry/implementation-map.yaml:L20697-L20706] |
| 370 | `tests/structural/fatigue_analysis/conftest.py` | `sn_curve_data` | `test_helper` | [docs/registry/implementation-map.yaml:L20707-L20716] |
| 371 | `tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py` | `run_fatigue_analysis` | `test_helper` | [docs/registry/implementation-map.yaml:L20717-L20727] |
| 372 | `tests/structural/fatigue_analysis/test_fatigue_analysis_timetrace.py` | `run_fatigue_analysis` | `test_helper` | [docs/registry/implementation-map.yaml:L20728-L20738] |
| 373 | `tests/subsea/pipeline/test_free_span_f105.py` | `ref_input` | `test_helper` | [docs/registry/implementation-map.yaml:L20739-L20748] |
| 374 | `tests/subsea/viv_analysis/test_viv_analysis_unit.py` | `TestVIVFatigueCalculator.test_cumulative_damage` | `test_helper` | [docs/registry/implementation-map.yaml:L20749-L20758] |
| 375 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_orcaflex_post_process_mock` | `test_helper` | [docs/registry/implementation-map.yaml:L20759-L20768] |
| 376 | `tests/test_automation/scripts/comprehensive_fix_campaign.py` | `ComprehensiveTestFixCampaign.generate_mock_data` | `test_helper` | [docs/registry/implementation-map.yaml:L20769-L20778] |
| 377 | `tests/test_complete_fatigue_pipeline.py` | `process_configuration` | `test_helper` | [docs/registry/implementation-map.yaml:L20779-L20788] |
| 378 | `tests/test_complete_fatigue_pipeline.py` | `main` | `test_helper` | [docs/registry/implementation-map.yaml:L20789-L20800] |
| 379 | `tests/test_fatigue_basic.py` | `run_all_tests` | `test_helper` | [docs/registry/implementation-map.yaml:L20801-L20810] |
| 380 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | `test_helper` | [docs/registry/implementation-map.yaml:L20811-L20820] |
| 381 | `tests/test_rainflow_with_effective_tension.py` | `main` | `test_helper` | [docs/registry/implementation-map.yaml:L20821-L20831] |
| 382 | `tests/test_reservoir_framework.py` | `run_performance_tests` | `test_helper` | [docs/registry/implementation-map.yaml:L20832-L20841] |
| 383 | `tests/test_spectral_fatigue_validation.py` | `TestSNCurveDNVReference.test_dnv_d_curve_parameters` | `test_helper` | [docs/registry/implementation-map.yaml:L20842-L20853] |
| 384 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | `test_helper` | [docs/registry/implementation-map.yaml:L20854-L20864] |
| 385 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | `test_helper` | [docs/registry/implementation-map.yaml:L20865-L20875] |
| 386 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | `test_helper` | [docs/registry/implementation-map.yaml:L20876-L20886] |
| 387 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_allowable_cycles` | `independent` | [docs/registry/implementation-map.yaml:L20887-L20897] |
| 388 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_stress_range` | `independent` | [docs/registry/implementation-map.yaml:L20898-L20907] |
| 389 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_allowable_cycles` | `independent` | [docs/registry/implementation-map.yaml:L20908-L20917] |
| 390 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_stress_range` | `independent` | [docs/registry/implementation-map.yaml:L20918-L20928] |
| 391 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_allowable_cycles` | `independent` | [docs/registry/implementation-map.yaml:L20929-L20938] |
| 392 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_stress_range` | `independent` | [docs/registry/implementation-map.yaml:L20939-L20948] |

## 10. Coverage and open items

Files read within scope: both Python files in the package, the example input and base configuration, the relevant workflow and routing entries, the registry test and the constant-gain test. No package-local tests or README are present in the pinned package tree; the two discovered module-specific nodes are listed in section 4. [src/digitalmodel/rao_spectral_fatigue/__init__.py:L1-L8] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L1-L181] [examples/workflows/rao-spectral-fatigue/input.yml:L1-L35] [src/digitalmodel/base_configs/modules/rao_spectral_fatigue/rao_spectral_fatigue.yml:L1-L23] [docs/registry/workflows.yaml:L198-L207] [docs/registry/module-routing.yaml:L608-L613] [tests/workflows/test_durable_workflows.py:L402-L420] [tests/workflows/test_durable_workflows.py:L1952-L2019]

Additional documentation found by a module-name search under `docs/`: `docs/registry/implementation-map.md`, `docs/maps/digitalmodel-operator-map.md`, `docs/domains/fatigue/fatg-legacy-spectral-fatigue.md`, and `docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md`; their workflow-related statements were read as context rather than independent validation. [docs/registry/implementation-map.md:L22-L24] [docs/maps/digitalmodel-operator-map.md:L95-L95] [docs/domains/fatigue/fatg-legacy-spectral-fatigue.md:L54-L63] [docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md:L10-L12]

| Open item | Evidence / required resolution | Responsible role and tracking |
|---|---|---|
| Numeric-domain and numeric-text validation | C02 accepts invalid values; conversions and checks are incomplete. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L137-L142] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L167-L181] | Maintainer; issue pending (1) |
| Explicit defaults, unique labels and RAO support coverage | Document or enforce identifiers, design choices and endpoint behavior. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L42-L43] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L62-L68] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L99-L119] | Maintainer; issue pending (2) |
| UNVERIFIED: physical calibration and stress amplitude/range convention of supplied RAOs | Example says stress range; transfer docstring says stress per wave amplitude; resolve before accepting engineering values. [examples/workflows/rao-spectral-fatigue/input.yml:L12-L15] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L4-L9] | Family quality manager; issue pending (3) |
| UNVERIFIED: applicability and edition alignment of licensed C205, C203 and F204 references | Clause text not checked against the licensed copy; do not infer edition agreement from absent editions. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L20-L21] [src/digitalmodel/hydrodynamics/wave_spectra.py:L71-L72] [src/digitalmodel/fatigue/spectral_fatigue.py:L23-L28] | Family quality manager; issue pending (3) |
| Citation sidecar and initializer import documentation | Formula string is not a structured standards citation; initializer and actual import differ. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L29-L29] [src/digitalmodel/rao_spectral_fatigue/workflow.py:L77-L81] [src/digitalmodel/rao_spectral_fatigue/__init__.py:L3-L7] | Maintainer; issue pending (3) |
| UNVERIFIED: engineering accuracy beyond internal consistency checks | Add an independent public expected result and offline test node; neither current regression supplies one. [tests/workflows/test_durable_workflows.py:L402-L420] [tests/workflows/test_durable_workflows.py:L1952-L2019] | Family quality manager; issue pending (4) |
| Report appendix, independent example and shared lessons registration | Current result records calculation fields, CSV paths and screening status, with no signed gate record; section-7 observations require durable registration. [src/digitalmodel/spectral_fatigue/workflow.py:L60-L75] | Maintainer; issue pending (4) |
| Map follow-up and dependency qualification | Broad-search candidates outside the package are discovery evidence; relation and defect annotations in the map are not validated comparisons. [docs/registry/implementation-map.yaml:L1-L3] | Maintainer; issue pending (5) |

The calculation-output citation contract was read; this pack records the sidecar gap without adding a standards-derived coefficient or changing calculation code. [src/digitalmodel/rao_spectral_fatigue/workflow.py:L77-L81] [src/digitalmodel/spectral_fatigue/workflow.py:L60-L75]
