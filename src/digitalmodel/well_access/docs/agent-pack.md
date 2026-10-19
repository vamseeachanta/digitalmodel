# Well-access module knowledge pack

| Header | Value |
|---|---|
| Module / repository | `well_access` / `digitalmodel` |
| Package scope | `src/digitalmodel/well_access` (all eight Python files; no excluded subpackages) |
| Pack revision / date | 1 / 2026-10-19 |
| Code revision | `f1d58a2ac0db65e68b5b690f0a609ef4746d114e` |
| Analysis family | Engineering calculations (`EC`); AQG r0 Python calculations |
| Quality role | Engineering calculations quality manager; independent provider required |
| Status | DRAFT; no gate clearance asserted |
| Implementation-map revision | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, ref `origin/codex/implmap-work`; data revision `270c8759c374b67a7f032953e7fcdb14e0224461` |

Citation coordinates refer to the code revision above; map inventory refers separately to the map revision above. Test outcomes below are execution records, not claims of source-code behaviour.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-18 | Initial package inventory, examples, gates and failure-mode sweep |
| 1 | 2026-10-19 | Distinguish historical floors from assumed central values; correct validation, payload and rounding descriptions; record local test execution and broaden identifier cleanup |

## 1. Purpose and scope

The package computes expected intervention demand, access-permitted events, deferred backlog, resource loading and lifecycle time availability; it does not simulate reservoir, flow or downhole behaviour. [src/digitalmodel/well_access/__init__.py:L3-L9] [src/digitalmodel/well_access/lifecycle_summary.py:L49-L88]

Uptime is a time-availability fraction rather than a production rate; the package describes calibration as an external empirical input and contains no cost engine. [src/digitalmodel/well_access/models.py:L9-L15]

Resource waiting is a comparative screening measure rather than a commitment scheduling model; the implementation reports excess demand-days above available days. [src/digitalmodel/well_access/resource_queue.py:L7-L14] [src/digitalmodel/well_access/resource_queue.py:L74-L78]

The routing registry records maturity `beta` and routes module tests to `tests/well_access/`. [docs/registry/module-routing.yaml:L698-L703]

UNVERIFIED: empirical accuracy, representativeness and current applicability of the supplied calibration have not been established independently; the adapter preserves caveats but does not establish those properties (section 10, issue pending (2)). The snapshot records source commit `adbd44c0` as an unmerged feature-branch revision; its current upstream merge status has not been verified. Only `bands.*.empirical` supplies the historical floor; central demand, supply, gaps and utilisation ratios depend on assumed intervention frequency and resource utilisation. [src/digitalmodel/base_configs/modules/well_access/wed_intervention_snapshot.yml:L12-L18] [src/digitalmodel/base_configs/modules/well_access/wed_intervention_snapshot.yml:L44-L64]

## 2. Inputs and outputs

Input constraints below distinguish implemented validation from preflight requirements; unrestricted dataclass fields must not be mistaken for validated inputs. [src/digitalmodel/well_access/models.py:L34-L90]

| Input | Unit | Implemented range / default | Source and citation |
|---|---|---|---|
| `n_wells`, `field_life_years` | wells; yr | Required; direct dataclass construction rejects values <= 0 but accepts NaN and positive infinity, without enforcing integer types; router applies `int()`, rejecting nonfinite values | [src/digitalmodel/well_access/models.py:L69-L85] [src/digitalmodel/well_access/router.py:L168-L174] |
| `access_class` | enum | Required; four architecture values | [src/digitalmodel/well_access/models.py:L25-L31] [src/digitalmodel/well_access/router.py:L171-L171] |
| `first_production_year` | yr offset | 0; stored, not used in the age integral | [src/digitalmodel/well_access/models.py:L73-L77] [src/digitalmodel/well_access/intervention_rates.py:L38-L56] |
| `facility_availability` | fraction (1) | (0, 1]; default 0.95 | [src/digitalmodel/well_access/models.py:L77-L90] |
| `base_rates`, `age_curve` | mapping; list | Missing mapping or curve becomes empty | [src/digitalmodel/well_access/intervention_rates.py:L142-L150] |
| `age_min_yr`, `age_max_yr`, `rate_per_well_year` | yr; yr; events/(well yr) | Required per supplied band; `float()` conversion, no band-range validation here | [src/digitalmodel/well_access/intervention_rates.py:L143-L150] [src/digitalmodel/well_access/models.py:L35-L45] |
| `duration_days`, `resource_class`, `low_confidence` | days/event; string; boolean | 0.0; `modu`; False | [src/digitalmodel/well_access/intervention_rates.py:L151-L158] |
| `access_multiplier` | fraction (1) | Missing architecture gives empty mapping; missing type gives 1.0; clipped into [0, 1] | [src/digitalmodel/well_access/intervention_rates.py:L81-L88] [src/digitalmodel/well_access/intervention_rates.py:L110-L124] |
| `resources` | list | Router default empty; each supplied pool needs class and available days/yr | [src/digitalmodel/well_access/router.py:L176-L179] [src/digitalmodel/well_access/lifecycle_summary.py:L35-L46] |
| Pool lead / batching | days/campaign; boolean | 0.0 / False in pool construction; adapter construction instead defaults to 45.0 / True | [src/digitalmodel/well_access/lifecycle_summary.py:L40-L44] [src/digitalmodel/well_access/wed_calibration.py:L306-L328] |
| `deferred_loss_per_event_days` | well-days/event | Router default 30.0; core requires argument | [src/digitalmodel/well_access/router.py:L179-L179] [src/digitalmodel/well_access/uptime_rollup.py:L29-L47] |
| Calibration location | YAML mapping | `calibration_path` takes precedence over inline `calibration`, otherwise empty mapping | [src/digitalmodel/well_access/router.py:L55-L61] |
| Optional `wed_calibration` | YAML snapshot / mapping | Off unless truthy; `use_wed_supply` supplies pools only when no pools are already supplied | [src/digitalmodel/well_access/router.py:L187-L202] |
| Adapter band quantities | counts; rig-days/yr; days; USD/yr; USD; dimensionless utilisation ratios (can exceed 1) | Missing optional historical-floor values remain None; missing supply values can become zero; central values depend on frequency and utilisation assumptions | [src/digitalmodel/well_access/wed_calibration.py:L74-L110] [src/digitalmodel/well_access/wed_calibration.py:L136-L234] [src/digitalmodel/base_configs/modules/well_access/wed_intervention_snapshot.yml:L44-L64] |
| `outputs` | file paths | Directory `results`; JSON `well_access_summary.json`; Markdown `well_access_lifecycle_note.md` | [src/digitalmodel/well_access/router.py:L238-L244] |

No coordinate frame, spatial origin or phase is consumed by the scalar event/day/fraction calculations; age overlap starts at 0 yr and uptime uses 365.0 days/yr. [src/digitalmodel/well_access/intervention_rates.py:L38-L42] [src/digitalmodel/well_access/uptime_rollup.py:L53-L76]

| Output | Unit / convention | Source and citation |
|---|---|---|
| `EventExpectation` | Demand, realized and deferred expected event counts, not integer random draws; sorted by type | [src/digitalmodel/well_access/intervention_rates.py:L81-L98] |
| `ResourceDemand` | Demand, availability, wait and mobilisation in days; `rho` dimensionless; sorted by resource class | [src/digitalmodel/well_access/resource_queue.py:L60-L90] |
| `UptimeResult` | Availability and losses as fractions; output rounded to four or six decimal places | [src/digitalmodel/well_access/uptime_rollup.py:L78-L82] |
| `LifecycleSummary` | Architecture, basis, component records, totals and confidence notes | [src/digitalmodel/well_access/lifecycle_summary.py:L71-L88] |
| Architecture deltas | Uptime: dry minus subsea; backlog and mobilisation: subsea minus dry | [src/digitalmodel/well_access/lifecycle_summary.py:L121-L140] |
| Router payload / files | UTF-8 JSON and Markdown; payload gains file paths after writing, then merges as `{**params, **payload}` into `cfg["well_access"]`; payload keys overwrite matching input keys, including `wed_calibration` when engaged | [src/digitalmodel/well_access/router.py:L208-L249] |
| Snapshot provenance | Source identifiers, revision/date and confidence copied to provenance mapping | [src/digitalmodel/well_access/wed_calibration.py:L249-L261] |

## 3. Method and governing standards

| Method | Implemented equation / algorithm | Basis and implementation |
|---|---|---|
| Age-banded expected demand | Band overlap = max(0, min(life, upper) - max(0, lower)); expected demand/well = sum(rate × overlap), rounded to six decimals | Piecewise-constant rate integral, explicitly described as NHPP expectation; no stochastic sampling. [src/digitalmodel/well_access/intervention_rates.py:L7-L14] [src/digitalmodel/well_access/intervention_rates.py:L38-L56] |
| Access | Aggregate demand = per-well demand × wells; realized = demand × clipped multiplier; deferred = demand - realized; four-decimal output rounding | Demand remains access-neutral; the documented calibration basis calls for access-capable analogs rather than access-suppressed history. [src/digitalmodel/well_access/intervention_rates.py:L16-L22] [src/digitalmodel/well_access/intervention_rates.py:L81-L98] |
| Resource loading | Realized events × duration, grouped by class; campaigns = min(events, life) with batching, otherwise events; add campaigns × lead | Available days = available days/yr × life; rho = demand/available or infinity; wait = max(0, demand - available), not a probabilistic queue wait. [src/digitalmodel/well_access/resource_queue.py:L46-L90] |
| Uptime | Well-days = wells × life × 365; losses = realized duration and deferred loss divided by well-days; uptime = availability × max(0, 1 - losses) | Resource waiting and mobilisation are not passed into this calculation. [src/digitalmodel/well_access/uptime_rollup.py:L29-L82] [src/digitalmodel/well_access/lifecycle_summary.py:L63-L69] |
| Snapshot access indicator | m = min(1, 1/ratio); absent or nonpositive ratio gives 1.0 | Forward-looking risk indicator, not a measured crossover; water-depth band does not resolve pressure class. [src/digitalmodel/well_access/wed_calibration.py:L332-L364] |
| Comparison | Same program/calibration with replaced access classes; differences of rounded summaries | Outputs are time availability, deferred-event and mobilisation differences, not recovery barrels. [src/digitalmodel/well_access/lifecycle_summary.py:L91-L140] |

No governing standard number, edition or clause is named in these method implementations; the operator map's generic “well engineering standards” entry does not identify one. No standards-derived constant or `Citation` sidecar is established for these methods. [src/digitalmodel/well_access/intervention_rates.py:L38-L98] [src/digitalmodel/well_access/resource_queue.py:L46-L90] [src/digitalmodel/well_access/uptime_rollup.py:L53-L82] [src/digitalmodel/well_access/wed_calibration.py:L332-L364] [docs/maps/digitalmodel-operator-map.md:L109-L109]

The elementary closed-form checks are documented in the tests: 10 × 0.1 + 15 × 0.2 = 4.0 events/well, and 5 × 0.1 = 0.5 events/well; empirical snapshot fixtures are not independent reference data. [tests/well_access/test_well_access.py:L108-L122] [tests/well_access/test_wed_calibration.py:L40-L48]

## 4. Validated worked examples and regression examples

Execution record: the initial pack recorded a setup failure installing `h5netcdf==1.8.1` before collection. This revision used the isolated C04 command with pytest and the declared PyYAML dependency: BASE and after each gave 27 passed, 0 failed and 0 skipped. The six cited nodes collect. C03's two elementary references were replayed independently; C05 remains a defined procedure, not an executed qualification. No validated example is claimed pending independent qualification.

| Case / classification | Input / expected value and tolerance basis | Existing test node ID | BASE / after |
|---|---|---|---|
| E1 band integral; regression pending qualification | In-code fixture, 25 yr; 10 × 0.1 + 15 × 0.2 = 4.0 events/well; `pytest.approx` default comparison; independent replay uses C03 | `tests/well_access/test_well_access.py::TestNHPP::test_demand_integrates_over_bands` | PASS / PASS. [tests/well_access/test_well_access.py:L108-L116] |
| E2 life cap; regression pending qualification | In-code fixture, 5 yr at 0.1 events/(well yr); 0.5 events/well; same approximate comparison; replay uses C03 | `tests/well_access/test_well_access.py::TestNHPP::test_life_caps_band_integration` | PASS / PASS. [tests/well_access/test_well_access.py:L118-L122] |
| E3 uptime bounds; regression | Fixture program and rates; uptime between 0 and facility availability (fraction), zero bound violations | `tests/well_access/test_well_access.py::TestUptime::test_uptime_bracket_bounded` | PASS / PASS. [tests/well_access/test_well_access.py:L173-L181] |
| E4 determinism; regression | Same fixture inputs twice; exactly equal demand, realized events and uptime, zero mismatches | `tests/well_access/test_well_access.py::TestComparisonAndDeterminism::test_run_lifecycle_deterministic` | PASS / PASS. [tests/well_access/test_well_access.py:L201-L208] |
| E5 optional adapter; regression | Inline synthetic config, no adapter block; adapter payload key count = 0 | `tests/well_access/test_wed_calibration.py::TestRouterIntegrationAdditive::test_router_wed_block_optional_and_off_by_default` | PASS / PASS. [tests/well_access/test_wed_calibration.py:L128-L164] |
| E6 supply adapter; regression | Inline synthetic config plus shipped snapshot; assumed-utilisation capacity = 2 × 0.7 × 365 = 511 rig-days/yr, then × 20 yr for available pool-days; test uses default approximate comparison; snapshot is not independent validation | `tests/well_access/test_wed_calibration.py::TestRouterIntegrationAdditive::test_router_wed_block_engages_when_requested` | PASS / PASS. [tests/well_access/test_wed_calibration.py:L166-L215] [src/digitalmodel/base_configs/modules/well_access/wed_intervention_snapshot.yml:L57-L64] |

Use the explicit absolute criterion C03 for independent replay; the existing tests do not specify their approximate-comparison tolerance arguments. [tests/well_access/test_well_access.py:L115-L122]

## 5. QA gates

The gate selection is AQG r0 section 10, Python calculations, with G5 from section 9; check criteria below are pack requirements, not additional runtime guards.

| Gate | Applicability | Module-specific check / numeric criterion / unit | Evidence / current result |
|---|---|---|---|
| G0 inputs, units, citations | Applicable | C01 and C02 | C01 PASS (2 rejection fixtures); C02 remains a manual audit for each submitted input |
| G1 imports and tests | Applicable; Python route, no solver model format | C04 | PASS; full `tests/well_access/` record: 27 passed at BASE and after |
| G2 hand or published check | Applicable | C03 and C05 | C03 PASS for the two elementary references; C05 NOT_RUN; G2 qualification remains open |
| G3 short dynamics | NOT_APPLICABLE | No time-domain dynamics integration in this chain | [src/digitalmodel/well_access/lifecycle_summary.py:L57-L69] |
| G4 diffraction | NOT_APPLICABLE | No diffraction calculation in this chain | [src/digitalmodel/well_access/lifecycle_summary.py:L57-L69] |
| G5 evidence appendix | Applicable | C06 | Retained input/provenance/gate record; no generated QA appendix in current router; issue pending (2). [src/digitalmodel/well_access/router.py:L208-L249] |

| Check | Executable procedure and numeric pass criterion | Gate / unit | Implementation or test evidence |
|---|---|---|---|
| C01 | Run both `TestProgramValidation` nodes: exactly 2 rejected invalid fixtures, 0 unexpected exceptions | G0 / cases | [tests/well_access/test_well_access.py:L97-L104] |
| C02 | Before calling `run_lifecycle` or `router`, audit the submitted mapping against section 2: missing required fields = 0; numeric nonfinite values = 0; noninteger well/life inputs = 0; bands with lower < 0 yr, upper <= lower, negative rates or overlaps = 0; negative durations/leads/losses = 0 days; capacity <= 0 = 0 pools; multipliers outside [0,1] = 0; missing event-type/pool joins = 0; duplicate event/type/pool names = 0; nonboolean flags = 0; unacknowledged defaults = 0; unit/provenance omissions = 0. Repeat for custom snapshots, requiring explicit None reasons and valid supply-scope enum. This is a manual mapping audit with a counted exception list, not implemented runtime validation. | G0 / fields, values, bands, days, pools, fraction | Input and conversion sites requiring audit. [src/digitalmodel/well_access/models.py:L34-L90] [src/digitalmodel/well_access/intervention_rates.py:L81-L88] [src/digitalmodel/well_access/intervention_rates.py:L142-L158] [src/digitalmodel/well_access/wed_calibration.py:L136-L234] |
| C03 | For E1 and E2, compute reference counts on paper without calling module functions: 4.0 and 0.5 events/well; compare `expected_demand_per_well` outputs with absolute error <= 1e-6 events/well (one output rounding increment). Run both `TestNHPP` nodes as an additional regression record. | G2 / events/well | [src/digitalmodel/well_access/intervention_rates.py:L45-L56] [tests/well_access/test_well_access.py:L108-L122] |
| C04 | Run every node in `tests/well_access/` using `PYTHONPATH=src uv run --no-project --with pytest --with pyyaml python -m pytest tests/well_access/ -q -rA -p no:cacheprovider --basetemp=<scratch>`; collect 27 nodes, require 27 passed, 0 failed, 0 skipped, 0 NOT_RUN before reporting G1 PASS. | G1 / test cases | The 11 core and 16 adapter test definitions. [tests/well_access/test_well_access.py:L97-L208] [tests/well_access/test_wed_calibration.py:L22-L215] |
| C05 | Use the module's rounded `EventExpectation` records as inputs to the independent worksheet, rather than unrounded rate integrals. Sum realized events × durations and campaigns × lead; round mobilisation to two decimals before adding it to base demand, then round demand and annual capacity × life to two decimals. From those rounded totals compute wait and rho with the source's rounding order; compare pool demand/available/wait/mobilisation within 0.01 days, rho within 0.0001 (1). Independently divide losses from those same events by wells × life × 365 and compare loss fractions within 0.000001 (1), uptime within 0.0001 (1). These criteria apply to the staged, rounded worksheet, not cumulative error against unrounded inputs. Check 0 <= uptime <= facility availability, 0 violations; explicitly exclude waiting from uptime. | G2 / days; fractions (1) | [src/digitalmodel/well_access/resource_queue.py:L46-L90] [src/digitalmodel/well_access/uptime_rollup.py:L53-L82] |
| C06 | Retain code pin, input bytes/digests, calibration revision/units/confidence, reference arithmetic, every required gate result and reviewer disposition together; require 0 missing evidence fields and 0 result rows lacking an input digest before issuing a report. Count omissions manually against this list; do not treat router JSON as a signed QA record. | G5 / fields, result rows | Existing output lacks such an appendix; provenance block is conditional. [src/digitalmodel/well_access/router.py:L187-L215] [src/digitalmodel/well_access/router.py:L236-L249] |

## 6. Known failure modes and pitfalls

sweep: 76 found

Sweep scope: all eight package Python files; the count is the number of reviewed behaviour rows below, including intentional defaults and unit conventions, not a count of confirmed defects. No swallowed `except`, record-skipping `continue`, or empty-result cache write is present in the reviewed source. Findings with no runtime guard are routed to issue pending (1); reference/retention qualification is routed to issue pending (2).

Exact discovery commands, with each hit reviewed in its enclosing definition:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.95|int\(|float\(|get\(' src/digitalmodel/well_access --glob '*.py'
rg -n '365|days|year|rate|unit|cache|write|default|or \[\]|or \{\}' src/digitalmodel/well_access --glob '*.py'
```

Rows with C02 refer to the explicit preflight mapping audit, not to a claim that the package rejects the input; runtime regression coverage for these malformed-input paths is absent unless specified. The source inspection was repeated on 2026-10-19 at the same code pin.

| ID | Symptom / cause / exact source location | Detection / disposition |
|---|---|---|
| S01 | Negative or out-of-life age spans are silently clipped to zero; life is converted with float(). [src/digitalmodel/well_access/intervention_rates.py:L38-L42] | C02; issue pending (1) |
| S02 | Empty age curves sum to zero demand; per-well output rounds to six decimals. [src/digitalmodel/well_access/intervention_rates.py:L50-L56] | C02, C03; issue pending (1) |
| S03 | Missing per-type multiplier becomes 1.0, numeric text is converted, out-of-range values are clipped; totals round to four decimals. [src/digitalmodel/well_access/intervention_rates.py:L84-L88] | C02; issue pending (1) |
| S04 | Missing architecture mapping becomes empty, indirectly allowing all demand. [src/digitalmodel/well_access/intervention_rates.py:L110-L124] | C02; issue pending (1) |
| S05 | Absent/false base_rates becomes empty mapping and produces an empty type list. [src/digitalmodel/well_access/intervention_rates.py:L142-L142] | C02; issue pending (1) |
| S06 | Age minimum uses float() with no decimal/thousands separator normalization. [src/digitalmodel/well_access/intervention_rates.py:L145-L145] | C02; issue pending (1) |
| S07 | Age maximum uses float() with no separator normalization. [src/digitalmodel/well_access/intervention_rates.py:L146-L146] | C02; issue pending (1) |
| S08 | Demand rate uses float() with no finite/range validation. [src/digitalmodel/well_access/intervention_rates.py:L147-L147] | C02; issue pending (1) |
| S09 | Missing age_curve becomes empty list and removes type demand. [src/digitalmodel/well_access/intervention_rates.py:L149-L149] | C02; issue pending (1) |
| S10 | Missing duration becomes 0.0 days; numeric text converts directly. [src/digitalmodel/well_access/intervention_rates.py:L155-L155] | C02; issue pending (1) |
| S11 | Missing class becomes modu and may misassign demand. [src/digitalmodel/well_access/intervention_rates.py:L156-L156] | C02; issue pending (1) |
| S12 | Missing low_confidence becomes False; bool("false") is truthy. [src/digitalmodel/well_access/intervention_rates.py:L157-L157] | C02; issue pending (1) |
| S13 | Absent resources becomes empty list; no pool rows are returned. [src/digitalmodel/well_access/lifecycle_summary.py:L35-L46] | C02; issue pending (1) |
| S14 | Available days/yr converts directly with float(), no capacity range guard here. [src/digitalmodel/well_access/lifecycle_summary.py:L41-L41] | C02; issue pending (1) |
| S15 | Omitted mobilisation lead becomes 0.0 days and converts directly. [src/digitalmodel/well_access/lifecycle_summary.py:L42-L42] | C02; issue pending (1) |
| S16 | Omitted batching is False; a nonempty text flag is truthy. [src/digitalmodel/well_access/lifecycle_summary.py:L43-L43] | C02; issue pending (1) |
| S17 | Resource wait and mobilisation do not feed uptime; reporting an availability response to queue pressure would misstate this chain. [src/digitalmodel/well_access/lifecycle_summary.py:L63-L69] | C05; issue pending (2) |
| S18 | Duration is resource-days/event; confidence defaults False without independent qualification. [src/digitalmodel/well_access/models.py:L54-L56] | C02; issue pending (1) |
| S19 | Capacity assumes days/yr; lead defaults 0.0 days and batching defaults False. [src/digitalmodel/well_access/models.py:L64-L66] | C02; issue pending (1) |
| S20 | First-production offset defaults 0; integral uses age rather than calendar offset. [src/digitalmodel/well_access/models.py:L76-L76] | C02; issue pending (1) |
| S21 | Facility availability defaults 0.95 and is range-checked; wells/life are checked only with <= 0, so direct construction accepts NaN and positive infinity. Router int() conversions reject those nonfinite values. [src/digitalmodel/well_access/models.py:L77-L90] [src/digitalmodel/well_access/router.py:L169-L170] | C01 covers two existing invalid fixtures only; C02 audits nonfinite inputs; no nonfinite runtime regression check exists; issue pending (1) |
| S22 | Result container defaults empty event/resource/notes lists, uptime None and three zero totals; direct construction can look complete without a run. [src/digitalmodel/well_access/models.py:L132-L138] | C06; issue pending (2) |
| S23 | Duplicate type/event names collapse in dictionaries before aggregation. [src/digitalmodel/well_access/resource_queue.py:L46-L48] | C02; issue pending (1) |
| S24 | Unknown event type defaults to modu and duration 0.0; accumulator zeros are intentional but unknown joins hide service days. [src/digitalmodel/well_access/resource_queue.py:L54-L58] | C02; issue pending (1) |
| S25 | Only supplied pools are emitted; absent demand/count within a pool defaults to zero; demand with no pool has no output row. [src/digitalmodel/well_access/resource_queue.py:L61-L64] | C02; issue pending (1) |
| S26 | Batching assumes at most one campaign per life-year; fractional expected campaigns are possible. [src/digitalmodel/well_access/resource_queue.py:L68-L72] | C05; issue pending (2) |
| S27 | Capacity rounds before division; zero/negative capacity returns infinity; wait is excess days clipped at zero. [src/digitalmodel/well_access/resource_queue.py:L74-L78] | C02, C05; issue pending (1) |
| S28 | Unknown event duration is 0.0 days; duplicate type names collapse. [src/digitalmodel/well_access/uptime_rollup.py:L53-L62] | C02; issue pending (1) |
| S29 | Well-time assumes exactly 365.0 days/yr and identical whole-life exposure per well. [src/digitalmodel/well_access/uptime_rollup.py:L55-L58] | C05; issue pending (2) |
| S30 | Deferred losses assume days/event; direct construction with NaN wells or life makes total_well_days > 0 false, silently returning zero loss fractions and facility availability as uptime. Ordinary nonpositive wells/life are rejected earlier; positive infinity is also accepted by direct construction. [src/digitalmodel/well_access/uptime_rollup.py:L55-L82] [src/digitalmodel/well_access/models.py:L79-L90] | C02; no nonfinite runtime regression check exists; issue pending (1) |
| S31 | Negative uptime bracket clamps to zero; negative losses can instead exceed availability because the upper bound is not clamped. [src/digitalmodel/well_access/uptime_rollup.py:L74-L82] | C02, C05; issue pending (1) |
| S32 | Missing config base defaults to current directory, permitting outputs at an unintended relative location. [src/digitalmodel/well_access/router.py:L48-L52] | C02; issue pending (1) |
| S33 | Missing inline calibration becomes empty mapping; YAML load does not validate schema. [src/digitalmodel/well_access/router.py:L55-L61] | C02; issue pending (1) |
| S34 | Missing notes silently renders no note rows. [src/digitalmodel/well_access/router.py:L150-L152] | C06; issue pending (2) |
| S35 | Direct int() conversion of wells and life can truncate supplied numeric fractions; formatted numeric strings are not normalized. [src/digitalmodel/well_access/router.py:L169-L170] | C02; issue pending (1) |
| S36 | Calendar offset defaults 0 via int(); availability defaults 0.95 via float(). [src/digitalmodel/well_access/router.py:L172-L173] | C02; issue pending (1) |
| S37 | Missing access table becomes empty mapping and ultimately unity multipliers. [src/digitalmodel/well_access/router.py:L177-L177] | C02; issue pending (1) |
| S38 | Missing resources becomes empty list. [src/digitalmodel/well_access/router.py:L178-L178] | C02; issue pending (1) |
| S39 | Missing deferred-loss assumption becomes 30.0 days via float(). [src/digitalmodel/well_access/router.py:L179-L179] | C02; issue pending (1) |
| S40 | False adapter setting disables it; a truthy nonmapping setting is silently replaced with an empty mapping then loads the default snapshot. [src/digitalmodel/well_access/router.py:L187-L192] | C02; issue pending (1) |
| S41 | Requested snapshot supply is ignored when resources already exist; class/scope have defaults. [src/digitalmodel/well_access/router.py:L193-L197] | C02; issue pending (1) |
| S42 | Default output names and directory overwrite matching files; JSON permits nonfinite numbers and no empty-result rejection occurs. [src/digitalmodel/well_access/router.py:L238-L244] | C02, C06; issue pending (1) |
| S43 | _triple returns None for absent mapping or missing aliases; present numeric text uses float() without locale normalization. [src/digitalmodel/well_access/wed_calibration.py:L55-L62] | C02; issue pending (1) |
| S44 | Missing/false bands yields no bands; missing empirical mapping yields empty mapping. [src/digitalmodel/well_access/wed_calibration.py:L139-L140] | C02; issue pending (1) |
| S45 | Band/label default to key, service flag False via bool(), well count zero via int(). [src/digitalmodel/well_access/wed_calibration.py:L143-L146] | C02; issue pending (1) |
| S46 | Absent interventions/well stays None, supplied text uses float(). [src/digitalmodel/well_access/wed_calibration.py:L147-L151] | C02; issue pending (1) |
| S47 | Absent intervention count stays None; supplied count uses int(). [src/digitalmodel/well_access/wed_calibration.py:L152-L156] | C02; issue pending (1) |
| S48 | Absent distinct-well count stays None; supplied count uses int(). [src/digitalmodel/well_access/wed_calibration.py:L157-L161] | C02; issue pending (1) |
| S49 | Absent floor rig-days stays None; supplied value uses int(), potentially truncating a numeric fraction. [src/digitalmodel/well_access/wed_calibration.py:L162-L166] | C02; issue pending (1) |
| S50 | Duration takes median before central alias, otherwise None. [src/digitalmodel/well_access/wed_calibration.py:L167-L169] | C02; issue pending (1) |
| S51 | Annual rig-day demand takes central alias, otherwise None. [src/digitalmodel/well_access/wed_calibration.py:L170-L172] | C02; issue pending (1) |
| S52 | Annual intervention count takes central alias, otherwise None. [src/digitalmodel/well_access/wed_calibration.py:L173-L175] | C02; issue pending (1) |
| S53 | Global rig-day gap takes central alias, otherwise None. [src/digitalmodel/well_access/wed_calibration.py:L176-L178] | C02; issue pending (1) |
| S54 | Resident rig-day gap takes central alias, otherwise None. [src/digitalmodel/well_access/wed_calibration.py:L179-L181] | C02; issue pending (1) |
| S55 | Global utilisation takes central alias, otherwise None. [src/digitalmodel/well_access/wed_calibration.py:L182-L184] | C02; issue pending (1) |
| S56 | Resident utilisation takes central alias, otherwise None. [src/digitalmodel/well_access/wed_calibration.py:L185-L187] | C02; issue pending (1) |
| S57 | Annual currency exposure takes central alias, otherwise None; currency unit is encoded in the field name. [src/digitalmodel/well_access/wed_calibration.py:L188-L190] | C02; issue pending (1) |
| S58 | Cost takes median before central alias, otherwise None; confidence defaults to forward_looking_risk. [src/digitalmodel/well_access/wed_calibration.py:L191-L194] | C02; issue pending (1) |
| S59 | Missing supply or parameters becomes empty mapping. [src/digitalmodel/well_access/wed_calibration.py:L198-L199] | C02; issue pending (1) |
| S60 | Missing global count becomes 0 via int(); missing eligible classes becomes empty mapping. [src/digitalmodel/well_access/wed_calibration.py:L201-L202] | C02; issue pending (1) |
| S61 | Missing/zero global central rig-day capacity becomes 0.0 via float(). [src/digitalmodel/well_access/wed_calibration.py:L203-L205] | C02; issue pending (1) |
| S62 | Missing resident total and eligible-heavy counts become 0 via int(). [src/digitalmodel/well_access/wed_calibration.py:L206-L209] | C02; issue pending (1) |
| S63 | Missing eligible-unit list becomes empty list. [src/digitalmodel/well_access/wed_calibration.py:L210-L212] | C02; issue pending (1) |
| S64 | Missing/zero resident rig-day capacity becomes 0.0 via float(). [src/digitalmodel/well_access/wed_calibration.py:L213-L215] | C02; issue pending (1) |
| S65 | Missing resident light count becomes 0 via int(). [src/digitalmodel/well_access/wed_calibration.py:L216-L218] | C02; issue pending (1) |
| S66 | Missing/zero supply utilisation becomes 0.0; supply confidence defaults soft. [src/digitalmodel/well_access/wed_calibration.py:L219-L222] | C02; issue pending (1) |
| S67 | Missing metadata/dayrates becomes empty mapping, caveats empty list, method empty string; these do not block object construction. [src/digitalmodel/well_access/wed_calibration.py:L225-L234] | C02, C06; issue pending (1) |
| S68 | Missing date becomes empty string; provenance identifiers can be None; confidence can default rather than prove source readiness. [src/digitalmodel/well_access/wed_calibration.py:L245-L260] | C06; issue pending (2) |
| S69 | Pool constructor assumes modu, resident scope, 45.0-day lead and batching True unless overridden. [src/digitalmodel/well_access/wed_calibration.py:L306-L328] | C02; issue pending (1) |
| S70 | Access-ratio method treats every scope other than gom_resident as global; unlike the capacity selector it does not reject invalid scope. [src/digitalmodel/well_access/wed_calibration.py:L343-L348] | C02; issue pending (1) |
| S71 | Missing or nonpositive ratio gives unity access; unknown demand is conflated with zero demand. [src/digitalmodel/well_access/wed_calibration.py:L349-L352] | C02; issue pending (1) |
| S72 | Multiplier is rounded to four decimals and always receives forward-looking confidence; it is not a measured recovery or pressure-capability result. [src/digitalmodel/well_access/wed_calibration.py:L353-L364] | C06; issue pending (2) |
| S73 | Event result confidence defaults False when constructed directly. [src/digitalmodel/well_access/models.py:L93-L101] | C02; issue pending (1) |
| S74 | Band confidence defaults forward_looking_risk when constructed directly. [src/digitalmodel/well_access/wed_calibration.py:L74-L91] | C02; issue pending (1) |
| S75 | supply confidence defaults soft when constructed directly. [src/digitalmodel/well_access/wed_calibration.py:L101-L110] | C02; issue pending (1) |
| S76 | Snapshot source_path defaults empty in the container and from_dict call; provenance does not require a source path. [src/digitalmodel/well_access/wed_calibration.py:L113-L137] | C06; issue pending (2) |

Discovery record: the incident search returned a broad calibration/data-basis epic (#890), not an incident-specific malformed-input guard; no claim of a closed incident or existing negative-path test is made. The existing tests cover valid fixtures, two program-validation failures and selected adapter selectors, rather than the full C02 audit. [tests/well_access/test_well_access.py:L97-L208] [tests/well_access/test_wed_calibration.py:L22-L215]

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared-log entry |
|---|---|---|---|---|---|
| 2026-10-18 | Initial source inventory | Keep an undefined access ratio distinct from evidence of unrestricted access | [src/digitalmodel/well_access/wed_calibration.py:L343-L352] | Section 6 documents the fallback and C02 requires explicit missing-value disposition | Not assigned; issue pending (1) |
| 2026-10-18 | Initial source inventory | Resource overload and time availability are separate outputs; no queue feedback is implemented | [src/digitalmodel/well_access/lifecycle_summary.py:L63-L69] | Sections 1, 3 and 5 state the comparison limit | Not assigned; issue pending (2) |

## 8. Ownership

| Responsibility | Required role / route |
|---|---|
| Family review | Engineering calculations quality manager, on a provider independent of production |
| Deputy | Engineering calculations deputy quality manager; named assignment remains open under issue pending (2) |
| Production | Calculation implementation and calibration-data maintainers |
| Numerical ambiguity or missing input | Refer to the family quality manager; do not widen tolerances or substitute a standard edition |
| Cross-family or outward-use decision | Refer to the maintainers after family disposition |

These are review requirements for this DRAFT, not evidence of an assigned deputy or completed review.

## 9. Dependencies and implementation inventory

The code exports models, intervention functions, resource calculation, uptime calculation, lifecycle orchestration, adapter classes and the router through the package namespace. [src/digitalmodel/well_access/__init__.py:L12-L62]

The router uses YAML loading, dataclass/enum serialization, filesystem output and the adapter; the engine dispatches `basename == "well_access"` to that router. [src/digitalmodel/well_access/router.py:L23-L45] [src/digitalmodel/engine.py:L748-L751]

The adapter's default snapshot is in `src/digitalmodel/base_configs/modules/well_access/wed_intervention_snapshot.yml`; the adapter loads it as YAML without importing the upstream extraction package. Only `bands.*.empirical` is the historical floor; central demand, supply, gaps and utilisation ratios depend on assumed frequency and utilisation. A supplied snapshot remains a data dependency, not a verified engineering reference. [src/digitalmodel/well_access/wed_calibration.py:L7-L17] [src/digitalmodel/well_access/wed_calibration.py:L45-L52] [src/digitalmodel/well_access/wed_calibration.py:L128-L134] [src/digitalmodel/base_configs/modules/well_access/wed_intervention_snapshot.yml:L44-L64]

The model describes economics as a downstream consumer; the lifecycle function itself returns a summary and does not invoke an economics module. [src/digitalmodel/well_access/models.py:L11-L15] [src/digitalmodel/well_access/lifecycle_summary.py:L78-L88]

### Map inventory record

| Item | Recorded inventory / result |
|---|---|
| Map | `docs/registry/implementation-map.yaml`, ref and revisions in header |
| Scope | Every family implementation row whose path starts `src/digitalmodel/well_access/`; all package files included |
| In-package family rows | 0; the implementation inventory is empty |
| Counted families | 0; ranking by in-package row count and map order produces an empty set |
| Required family-signal grep | NOT_APPLICABLE because no map family has an implementation row inside this package |
| Additional implementations counted as misses | NOT_APPLICABLE; no counted family |
| Rejected candidates | Package paths occur in `rejected_candidates`, not family implementation rows; those entries do not enroll a family |
| Map follow-up | Age-banded intervention demand, access suppression/deferred backlog, resource-day loading and campaign batching, uptime rollup, snapshot supply-ratio calibration, architecture comparison; issue pending (3) |

Reproduce the inventory against the named map revision, without substituting the current default-branch map:

```python
import yaml
# map_text is git show <map-ref>:docs/registry/implementation-map.yaml
mapping = yaml.safe_load(map_text)
ranked = []
for order, family in enumerate(mapping["families"]):
    rows = [row for row in family["implementations"]
            if row["path"].startswith("src/digitalmodel/well_access/")]
    if rows:
        ranked.append((len(rows), order, family["id"], rows))
ranked.sort(key=lambda row: (-row[0], row[1]))
counted = ranked[:3]
assert len(ranked) == 0 and len(counted) == 0
```

Repository-wide supplemental discovery included `scripts/` and used the following command; matching production Python files were inside this package; remaining matches were tests, with no external production implementation located by these signals. This bounded search is not proof that differently named algorithms are absent. Discovery is an execution record; additional families require map enrollment before they affect the counted result.

```sh
rg -n 'NHPP|expected_demand_per_well|access_multiplier_from_supply_ratio|deferred_loss_per_event_days|field_uptime' . --glob '*.py'
```

## 10. Coverage and open items

Read inventory: `__init__.py`, `models.py`, `intervention_rates.py`, `resource_queue.py`, `uptime_rollup.py`, `lifecycle_summary.py`, `router.py` and `wed_calibration.py` under the package; both files under `tests/well_access/`; routing entry, the operator-map row, engine dispatch, and the base configuration. No package README or prior package docs were found at the pin; the docs search found the operator map and routing/map registries. This is a discovery record, not an assertion about unsearched repositories.

The test definitions provide 11 core and 16 adapter nodes, 27 total; none is licensed-solver dependent in these two files. [tests/well_access/test_well_access.py:L97-L208] [tests/well_access/test_wed_calibration.py:L22-L215]

| Open item | Required disposition / responsible role | Issue |
|---|---|---|
| Input/default sweep | Add negative-path coverage and strict preflight for the section-6 sites; retain intended zero/default semantics separately; calculation maintainer | issue pending (1) |
| Empirical qualification | UNVERIFIED claim from section 1: qualify source revision (recorded as unmerged), coverage, units, intended use and freshness; central demand/supply/gap/ratio depend on frequency/utilisation assumptions; revise the adapter's "REAL" / "rather than an assumed capacity" wording and matching router comment; do not promote snapshot regressions to validation; calibration-data maintainer. [src/digitalmodel/well_access/wed_calibration.py:L9-L14] [src/digitalmodel/well_access/wed_calibration.py:L313-L318] [src/digitalmodel/well_access/router.py:L181-L186] | issue pending (2); related #890 |
| Execution qualification | Local BASE and after each pass all 27 module tests, the pack node-ID check (6 IDs), and 10 routing-contract tests. C01 and C03 executed; C05 still needs an independent resource/uptime worksheet; calculation maintainer | issue pending (2) |
| Independent worked examples | Six regression examples and zero validated examples recorded; qualify the two elementary closed-form references and a resource/uptime independent example after execution; family quality manager | issue pending (2) |
| QA retention / review | C06 is a pack requirement, not an automatic output appendix; provide input digests, result records, independent review, named deputy and shared-log IDs before qualified issue; family quality manager | issue pending (2) |
| Identifying snapshot and fixture text | Two discovery files contain identifying names: one adapter test node and another assertion identify units, and the shipped snapshot also contains unit/operator identifiers exposed through `FleetSupply`. No identifying values are reproduced here; replace them with codenames before reusing snapshot or fixture text; calibration-data maintainer. [src/digitalmodel/well_access/wed_calibration.py:L101-L110] [src/digitalmodel/well_access/wed_calibration.py:L206-L212] | issue pending (2) |
| Method-map coverage | Enroll all method families listed in section 9; package rejected candidates alone do not count as mapped implementations; map maintainer | issue pending (3) |

The standards-citation contract was readable; no licensed clause or licensed document was opened. No restricted sidecar values or independently verified empirical values are supplied by this pack. Passing regressions establish execution at the code pin; independent calibration and C05 qualification remain open.
