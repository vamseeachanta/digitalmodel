# Mooring resilience knowledge pack

| Header | Value |
| --- | --- |
| Module | `mooring_resilience` |
| Repository | `digitalmodel` |
| Package path | `src/digitalmodel/mooring_resilience` |
| Pack revision | 0, 2026-10-18 |
| Code revision | `c099a5e1317e6bed63d876252d87cbb12c774372` |
| Analysis family | Engineering calculations; EC |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`assess` combines interpolated intact tension, scaled damaged tension, anchor holding capacity and a constant-amplitude fatigue proxy into utilizations, fatigue margin and a traffic light. [src/digitalmodel/mooring_resilience/screening.py:L146-L184]

The package is a screening calculation for spread moorings; its documentation reserves the analysis of record for a full mooring analysis, mentioning OrcaFlex as an example rather than launching it. [src/digitalmodel/mooring_resilience/screening.py:L3-L28]

Maturity is `beta` in the routing registry; this is a registry classification, not independent engineering validation. [docs/registry/module-routing.yaml:L235-L240]

The tension query uses only wave height, period and water depth; changing `n_lines`, line area, anchor geometry or MBL does not change its atlas query, and damaged loading is a multiplier rather than a simulated line removal. [src/digitalmodel/mooring_resilience/screening.py:L95-L104] [src/digitalmodel/mooring_resilience/screening.py:L162-L166]

## 2. Inputs and outputs

Input ranges below describe the pinned default atlas grids, not a general design envelope; the dataclasses contain no range validation. [src/digitalmodel/mooring_resilience/screening.py:L43-L72] [atlases/fpso_mooring_full/879f4f041243/manifest.yaml:L5-L24] [atlases/anchor_capacity/c8f67ad7b69c/manifest.yaml:L5-L29]

| Input | Unit | Allowed range / behavior | Default | Source |
| --- | --- | --- | --- | --- |
| `MooringConfig.n_lines` | count | Stored; not consumed by `assess` or either query | Required | [src/digitalmodel/mooring_resilience/screening.py:L43-L51] [src/digitalmodel/mooring_resilience/screening.py:L95-L116] [src/digitalmodel/mooring_resilience/screening.py:L146-L184] |
| `mbl_kN` | kN | No guard on divisor; require positive finite input before use | Required | [src/digitalmodel/mooring_resilience/screening.py:L164-L165] |
| `line_area_mm2` | mm² | No guard on divisor; require positive finite input before use | Required | [src/digitalmodel/mooring_resilience/screening.py:L126-L132] |
| `anchor_diameter_m`, `anchor_length_m`, `soil_su_kpa` | m, m, kPa | Atlas: 2–8 m, 10–30 m, 20–100 kPa | Required | [atlases/anchor_capacity/c8f67ad7b69c/manifest.yaml:L5-L29] [src/digitalmodel/mooring_resilience/screening.py:L112-L116] |
| `water_depth_m` | m | Atlas: 500–1500 m | Required | [atlases/fpso_mooring_full/879f4f041243/manifest.yaml:L19-L24] |
| `Metocean.Hs`, `Tp` | m, s | Atlas: 2–5 m, 9–13 s | Required | [src/digitalmodel/mooring_resilience/screening.py:L54-L57] [atlases/fpso_mooring_full/879f4f041243/manifest.yaml:L5-L18] |
| `fos_intact`, `fos_damaged`, `fos_foundation` | dimensionless | Overridable; no positivity check | Code defaults referenced, numeric standard factors not reproduced | [src/digitalmodel/mooring_resilience/screening.py:L60-L65] |
| `redistribution` | dimensionless | Overridable; no topology dependence | 1.3 | [src/digitalmodel/mooring_resilience/screening.py:L65-L65] [src/digitalmodel/mooring_resilience/screening.py:L162-L162] |
| `design_life_years`, `annual_cycles` | years, cycles/year | No positivity or finiteness checks | 25 years; 3.15e6 cycles/year | [src/digitalmodel/mooring_resilience/screening.py:L66-L67] [src/digitalmodel/mooring_resilience/screening.py:L130-L132] [src/digitalmodel/mooring_resilience/screening.py:L168-L169] |
| `dyn_tension_per_Hs_kN` | kN/m | Screening assumption: tension range proportional to Hs | 45 kN/m | [src/digitalmodel/mooring_resilience/screening.py:L68-L69] [src/digitalmodel/mooring_resilience/screening.py:L126-L127] |
| `sn_a_D`, `sn_m` | coefficient for MPa stress and cycles; exponent dimensionless | Constant-amplitude T-N parameters; no validity checks | Code defaults referenced, numeric standard parameters not reproduced | [src/digitalmodel/mooring_resilience/screening.py:L70-L72] [src/digitalmodel/mooring_resilience/screening.py:L130-L132] |
| `atlas_root`, `factors` | path, dataclass | `None` selects default root or default factors | `None` | [src/digitalmodel/mooring_resilience/screening.py:L90-L92] [src/digitalmodel/mooring_resilience/screening.py:L146-L150] [src/digitalmodel/parametric/query.py:L19-L20] |

| Output | Unit | Meaning / nonfinite behavior | Source |
| --- | --- | --- | --- |
| `intact_tension_kN`, `foundation_capacity_kN` function tuples | kN, boolean, text | Value, in-range status, reason; out of range gives NaN | [src/digitalmodel/mooring_resilience/screening.py:L95-L116] |
| `t_intact_kN`, `t_damaged_kN`, `foundation_capacity_kN` result fields | kN | Tension from N divided by 1000; anchor response already kN; damaged tension scaled from intact | [src/digitalmodel/mooring_resilience/screening.py:L101-L116] [src/digitalmodel/mooring_resilience/screening.py:L162-L166] |
| `util_intact`, `util_damaged`, `util_foundation` | dimensionless | Factored tension / MBL or capacity; unavailable query produces NaN | [src/digitalmodel/mooring_resilience/screening.py:L164-L166] |
| `fatigue_life_years`, `fatigue_margin` | years, dimensionless | Life and life/design life; NaN without intact query; nonpositive stress or annual damage returns infinity | [src/digitalmodel/mooring_resilience/screening.py:L126-L132] [src/digitalmodel/mooring_resilience/screening.py:L168-L169] |
| `governing_util`, `light`, `notes` | dimensionless, enum text, text list | Maximum non-NaN utilization; GREEN/AMBER/RED/ESCALATE; out-of-range reasons | [src/digitalmodel/mooring_resilience/screening.py:L135-L143] [src/digitalmodel/mooring_resilience/screening.py:L156-L184] |

No coordinate, origin, sign or phase convention is encoded: the API consumes scalar magnitudes and emits scalar checks; there is no model-file reader or writer in this package. [src/digitalmodel/mooring_resilience/screening.py:L43-L87] [src/digitalmodel/mooring_resilience/screening.py:L90-L184] [src/digitalmodel/mooring_resilience/__init__.py:L3-L23]

Atlas persistence consists of `default.txt`, `surrogate.json`, `manifest.yaml`, `validation.json` and `grid.parquet`; the loader reads them directly, while the wrapper assumes the response units described above. [src/digitalmodel/parametric/atlas.py:L270-L288] [src/digitalmodel/mooring_resilience/screening.py:L101-L116]

## 3. Method and governing standards

| Method / reference | Implementation and provenance limits |
| --- | --- |
| Intact and damaged strength | `intact_tension_kN` delegates to `Atlas.predict`; `assess` computes damaged tension = intact tension × redistribution, then each strength utilization = its factor × tension / MBL. [src/digitalmodel/mooring_resilience/screening.py:L95-L104] [src/digitalmodel/mooring_resilience/screening.py:L162-L165] |
| Foundation | `foundation_capacity_kN` queries anchor diameter, length and undrained strength; utilization = foundation factor × damaged tension / capacity. [src/digitalmodel/mooring_resilience/screening.py:L107-L116] [src/digitalmodel/mooring_resilience/screening.py:L166-L166] |
| Chain fatigue proxy | `_fatigue_life_years`: ΔT = dynamic coefficient × Hs; Δσ = 1000 ΔT / area; allowable cycles = `sn_a_D` × Δσ raised to negative `sn_m`; life = allowable cycles / annual cycles for positive damage. This is the implemented proxy, not an independently checked fatigue reference. [src/digitalmodel/mooring_resilience/screening.py:L119-L132] |
| Decision criterion | `_traffic_light` gives ESCALATE first; otherwise RED if governing utilization > 1 or fatigue margin < 1, AMBER if utilization > 0.8 or margin < 2, GREEN otherwise; equality follows those strict comparisons. [src/digitalmodel/mooring_resilience/screening.py:L135-L143] |
| API RP 2SK | Package comments identify ASD intact/damaged conditions without edition or clause; the citation registry also mentions Table C-1, but does not supply an API edition or an API sidecar. Clause text not checked against the licensed copy. [src/digitalmodel/mooring_resilience/screening.py:L20-L22] [src/digitalmodel/mooring_resilience/screening.py:L62-L64] [src/digitalmodel/citations/registry.py:L47-L52] |
| DNV-OS-E301 | `safety_factor_citations` returns DNV sidecars for intact, damaged and foundation conditions; registry edition 2021-07, Section 2.2.3 (intact/damaged quasi-static) and Section 2 (anchor resistance, geotechnical safety factor). Clause text not checked against the licensed copy. [src/digitalmodel/mooring_resilience/screening.py:L190-L224] [src/digitalmodel/citations/registry.py:L26-L31] [src/digitalmodel/citations/registry.py:L47-L52] [src/digitalmodel/citations/registry.py:L116-L135] |
| DNV-RP-C203 | The T-N comment names this standard alongside DNV-OS-E301 without an edition, clause or equation identifier; `assess` emits neither fatigue citation nor citation sidecar. Clause text not checked against the licensed copy. [src/digitalmodel/mooring_resilience/screening.py:L70-L72] [src/digitalmodel/mooring_resilience/screening.py:L178-L184] |
| Edition discrepancy | The tension atlas declares DNV-OS-E301 edition 2018-07, while the registry getter declares 2021-07; this is an unresolved basis mismatch, not a selected common edition: issue pending (2). [atlases/fpso_mooring_full/879f4f041243/manifest.yaml:L30-L39] [src/digitalmodel/citations/registry.py:L26-L31] |
| Upstream anchor reference | The anchor atlas declares DNV-RP-E303, edition 2005, without a clause; it does not establish the basis of the package's separate foundation factor. Clause text not checked against the licensed copy. [atlases/anchor_capacity/c8f67ad7b69c/manifest.yaml:L35-L44] [src/digitalmodel/mooring_resilience/screening.py:L64-L64] |

The example README repeats API RP 2SK and DNV-OS-E301 without editions or clauses, and the report template emits the same references; neither adds a DNV-RP-C203 clause reference. [examples/marine_ops/mooring_resilience/README.md:L13-L18] [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L151-L156]

The numeric factors remain in `ResilienceFactors`; citation getters are separate, and the equality test compares only the defaults against a synthetic standards-page fixture. [src/digitalmodel/mooring_resilience/screening.py:L60-L72] [src/digitalmodel/mooring_resilience/screening.py:L150-L150] [tests/unit/marine_ops/test_mooring_resilience.py:L130-L147]

UNVERIFIED: U1 — applicability of the named standards, exact fatigue basis, API/C203 editions and missing clause titles are not established by the implementation or the synthetic citation fixture; issue pending (2). [src/digitalmodel/mooring_resilience/screening.py:L62-L72] [tests/unit/marine_ops/test_mooring_resilience.py:L130-L147]

## 4. Validated worked examples

Classification: 0 validated examples and 11 regression examples; the tests check implementation invariants, atlas trends and synthetic citation metadata, without an independently sourced absolute result. [tests/unit/marine_ops/test_mooring_resilience.py:L23-L157]

The worked example is `run_resilience_curves.py`, which produces four charts and a README from `assess`; its reported baseline is generated by the same implementation, so it is represented below by R04, not promoted to independent validation. [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L47-L133] [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L136-L199]

Verification observation: every node below passed at the header pin before and after the documentation change; 11 run, 11 passed, 0 failed, 0 skipped, 0 NOT_RUN per module run, using `PYTHONPATH=src uv run --no-sources python -m pytest tests/unit/marine_ops/test_mooring_resilience.py -q -rA -p no:cacheprovider --basetemp=<scratch>`, without an `addopts` override. [tests/unit/marine_ops/test_mooring_resilience.py:L49-L157]

| ID / source | Input / expected result / tolerance | Existing test node ID | Offline result |
| --- | --- | --- | --- |
| R01, authored classifier assertions | Six classifier outcomes match exactly; 0 text mismatches | `tests/unit/marine_ops/test_mooring_resilience.py::test_traffic_light_thresholds` | PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L49-L55] |
| R02, algebraic invariant of implemented proxy | Hs 2, 4 and 5 m; increasing Hs reduces life, doubling Hs divides life by 8.0, relative tolerance 1e-6 dimensionless | `tests/unit/marine_ops/test_mooring_resilience.py::test_fatigue_life_decreases_with_sea_state` | PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L58-L65] |
| R03, area monotonicity | Area 15000 → 30000 mm² at Hs 4 m: life increase > 0 years | `tests/unit/marine_ops/test_mooring_resilience.py::test_larger_chain_area_extends_fatigue_life` | PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L68-L74] |
| R04, example baseline / default atlases | Test `BASE`, Hs 3 m / Tp 11 s: 2 finite utilizations and light among GREEN/AMBER/RED; no numeric README snapshot tolerance is asserted | `tests/unit/marine_ops/test_mooring_resilience.py::test_in_range_baseline_produces_finite_result` | PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L23-L25] [tests/unit/marine_ops/test_mooring_resilience.py:L79-L84] |
| R05, damaged-load invariant | Test `BASE`, Hs 3 m / Tp 11 s: damaged minus intact peak > 0 kN | `tests/unit/marine_ops/test_mooring_resilience.py::test_damaged_peak_exceeds_intact_peak` | PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L87-L90] |
| R06, atlas range guard | Hs 9 m: ESCALATE, NaN intact utilization and ≥ 1 out-of-range note; 0 extrapolated results | `tests/unit/marine_ops/test_mooring_resilience.py::test_out_of_range_escalates_not_extrapolates` | PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L93-L98] |
| R07, depth sensitivity | Depth 500 → 1500 m: tension-utilization increase > 0 dimensionless | `tests/unit/marine_ops/test_mooring_resilience.py::test_deeper_water_raises_tension_utilization` | PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L101-L107] |
| R08, soil sensitivity | Strength 25 → 100 kPa: foundation-utilization reduction > 0 dimensionless | `tests/unit/marine_ops/test_mooring_resilience.py::test_stronger_soil_reduces_foundation_utilization` | PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L110-L116] |
| R09, MBL sensitivity | MBL 12000 → 18000 kN: intact-utilization reduction > 0 dimensionless | `tests/unit/marine_ops/test_mooring_resilience.py::test_higher_mbl_reduces_tension_utilization` | PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L119-L125] |
| R10, synthetic metadata fixture | 3 returned cited values equal defaults exactly; intact/foundation identify DNV-OS-E301; no licensed numerical validation | `tests/unit/marine_ops/test_mooring_resilience.py::test_safety_factor_citations_resolve_and_match_factors` | PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L130-L147] |
| R11, explicit empty reference clone | Warning captured and dictionary length = 0 entries | `tests/unit/marine_ops/test_mooring_resilience.py::test_safety_factor_citations_degrade_gracefully` | PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L150-L157] |

UNVERIFIED: U2 — the atlas trend tests and generated README do not establish independent absolute accuracy or an eligible design configuration; issue pending (4). [tests/unit/marine_ops/test_mooring_resilience.py:L79-L125] [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L136-L170]

## 5. QA gates

Gate allocation follows AQG r0, section 10, “Python calculations”; G5 follows AQG r0, section 9, “QA appendix.”

| Gate | Applicability / route | Module-specific checks |
| --- | --- | --- |
| G0 | Applicable: scalar units, atlas response units and reference metadata | C01, C02, C13 |
| G1 | Applicable: import `digitalmodel.mooring_resilience` and execute offline Python tests; no target-program file is written by this package | C03; [src/digitalmodel/mooring_resilience/__init__.py:L3-L23] [src/digitalmodel/mooring_resilience/screening.py:L146-L184] |
| G2 | Applicable: hand algebra and implementation invariants; absolute engineering validation remains open | C04–C12 |
| G3 | NOT_APPLICABLE: scalar screening has no time-stepping solver | [src/digitalmodel/mooring_resilience/screening.py:L95-L184] |
| G4 | NOT_APPLICABLE: no diffraction body or mesh calculation | [src/digitalmodel/mooring_resilience/screening.py:L95-L184] |
| G5 | Applicable: evidence and provenance audit of the screen | C14; appendix completeness is a recorded gap |

These are executable check specifications, not a statement that every engineering gate is cleared; the test nodes identify existing checks and the hand recipes can be rerun without a solver. [tests/unit/marine_ops/test_mooring_resilience.py:L49-L157] (AQG r0, sections 10–11.)

| Check | Gate | Function / quantity and numeric pass criterion | Evidence / observed result |
| --- | --- | --- | --- |
| C01 | G0 | `safety_factor_citations`: 3 values equal default factors; 0 value or reference-ID mismatches, dimensionless | R10 PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L139-L147] |
| C02 | G0 | Query test `BASE` at Hs 3 m / Tp 11 s; compare `intact_tension_kN` to tension `Atlas.predict` / 1000 and `foundation_capacity_kN` to anchor `Atlas.predict`; each error ≤ 1e-9 kN | Hand recipe run: both errors 0 kN, PASS; tests wrapper units only, not atlas accuracy. [tests/unit/marine_ops/test_mooring_resilience.py:L23-L25] [src/digitalmodel/mooring_resilience/screening.py:L95-L116] |
| C03 | G1 | Import and run the 11 section-4 test nodes: failures = 0 tests, skips = 0 tests, NOT_RUN = 0 tests | BASE and AFTER: 11/11 PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L49-L157] |
| C04 | G2 | `_fatigue_life_years`: increasing Hs decreases life by > 0 years; doubling Hs divides life by 8.0 within relative 1e-6; doubling area increases life by > 0 years | R02/R03 PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L58-L74] |
| C05 | G2 | Synthetic hand fixture, not standard parameters: Hs = 2 m, area = 1000 mm², dynamic coefficient = 10 kN/m, `sn_a_D` = 100000 for MPa/cycles, `sn_m` = 2, annual cycles = 100/year; stress = 20 MPa, allowable cycles = 250, life = 2.5 years. Compare `_fatigue_life_years` with that arithmetic within 1e-12 years | Hand recipe run: error 0 years, PASS; verifies algebra/units only. [src/digitalmodel/mooring_resilience/screening.py:L126-L132] |
| C06 | G2 | `_traffic_light`: all 6 authored input/output pairs match; 0 string mismatches | R01 PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L49-L55] |
| C07 | G2 | `assess`: 2 tested utilization values are finite, dimensionless; light has 0 invalid labels | R04 PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L79-L84] |
| C08 | G2 | `assess`: damaged tension minus intact tension > 0 kN for the baseline | R05 PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L87-L90] |
| C09 | G2 | `assess`: depth increase gives utilization increase > 0 dimensionless | R07 PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L101-L107] |
| C10 | G2 | `assess`: soil-strength increase gives foundation-utilization reduction > 0 dimensionless | R08 PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L110-L116] |
| C11 | G2 | `assess`: MBL increase gives intact-utilization reduction > 0 dimensionless | R09 PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L119-L125] |
| C12 | G2 | `assess`: at Hs 9 m, light = ESCALATE, intact utilization = NaN, notes ≥ 1 entry; 0 extrapolated results | R06 PASS; [tests/unit/marine_ops/test_mooring_resilience.py:L93-L98] |
| C13 | G0 | `safety_factor_citations` with an empty explicit reference clone: ≥ 1 captured RuntimeWarning and 0 returned citation entries | R11 PASS; fallback behavior, not fail-closed assurance. [tests/unit/marine_ops/test_mooring_resilience.py:L150-L157] |
| C14 | G5 | Inspect `dataclasses.fields(ResilienceResult)` for code revision, input fingerprint, atlas IDs, atlas hashes, gate records, standard editions and quality signature; missing count must be 0 fields for self-contained evidence | Observed 7 missing fields: FAIL (baseline gap); issue pending (4). The example report also lacks these records. [src/digitalmodel/mooring_resilience/screening.py:L75-L87] [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L136-L186] |

## 6. Known failure modes and pitfalls

sweep: 32 found

Sweep scope: both package Python files at the header pin; each fallback, numeric assumption or error-substitution site is classified below, including intentional policies rather than treating every match as a defect. [src/digitalmodel/mooring_resilience/screening.py:L43-L224] [src/digitalmodel/mooring_resilience/__init__.py:L1-L23]

Exact searches used before adding this Markdown file:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|math.inf|math.nan|default|kN|MPa|mm2|kpa' src/digitalmodel/mooring_resilience
rg -n --glob '*.py' 'except|continue|return|=|int\(|float\(|cache|unit' src/digitalmodel/mooring_resilience
```

Manual review of all definitions supplemented the searches; no `continue`, `return None`, `return []`, external-text `int()`/`float()` conversion, epoch default or cache write occurs in these two files. [src/digitalmodel/mooring_resilience/screening.py:L1-L224] [src/digitalmodel/mooring_resilience/__init__.py:L1-L23]

| ID / site | Symptom or assumption | Existing detection / limitation | Tracking |
| --- | --- | --- | --- |
| S01, `fos_intact` default | Fixed intact factor can be mistaken for a project-specific basis | C01 checks default equality only | #996; applicability issue pending (2); [src/digitalmodel/mooring_resilience/screening.py:L62-L62] |
| S02, `fos_damaged` default | Fixed damaged factor has no edition in the dataclass | C01 checks default equality only | #996; issue pending (2); [src/digitalmodel/mooring_resilience/screening.py:L63-L63] |
| S03, `fos_foundation` default | Representative geotechnical factor is not selected by anchor/consequence class | C01 checks default equality only | #1000; issue pending (2); [src/digitalmodel/mooring_resilience/screening.py:L64-L64] [src/digitalmodel/citations/registry.py:L121-L126] |
| S04, redistribution default | Fixed scaling does not resolve damaged equilibrium | C08 detects only damaged > intact, not applicability | issue pending (2); [src/digitalmodel/mooring_resilience/screening.py:L65-L65] [src/digitalmodel/mooring_resilience/screening.py:L162-L162] |
| S05, design-life default | Omitted factors silently select a 25-year basis | no detection check exists for design-basis omission | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L66-L66] [src/digitalmodel/mooring_resilience/screening.py:L150-L150] |
| S06, annual-cycle default | Fixed cycle rate bypasses a site-specific spectrum | C05 checks arithmetic with explicit synthetic rate only | issue pending (2); [src/digitalmodel/mooring_resilience/screening.py:L67-L67] |
| S07, dynamic coefficient default | Wave-height-only stress proxy ignores an actual tension history | C04 checks scaling, not calibration | issue pending (2); [src/digitalmodel/mooring_resilience/screening.py:L68-L69] [src/digitalmodel/mooring_resilience/screening.py:L126-L127] |
| S08, T-N intercept default | Unverified fatigue coefficient basis | no detection check exists for standards applicability | issue pending (2); [src/digitalmodel/mooring_resilience/screening.py:L70-L71] |
| S09, T-N exponent default | Unverified fatigue exponent basis | C04 checks implemented scaling only | issue pending (2); [src/digitalmodel/mooring_resilience/screening.py:L72-L72] |
| S10, result notes factory | Empty list is a normal result default; absence of notes does not certify valid inputs | C12 checks populated notes on tension out-of-range only | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L87-L87] |
| S11, default atlas root | `None` selects repository atlases and mutable default pointers | no detection check exists for selection provenance in result | issue pending (4); [src/digitalmodel/mooring_resilience/screening.py:L90-L92] [src/digitalmodel/parametric/atlas.py:L270-L278] |
| S12, tension unit assumption | N response is assumed and divided by 1000 without response-schema validation | C02 detects wrapper conversion mismatch for selected fixture | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L101-L104] |
| S13, tension out-of-range | NaN replaces a rejected prediction, accompanied by status/reason | C12 checks escalation and NaN | Covered policy; [src/digitalmodel/mooring_resilience/screening.py:L103-L104] |
| S14, capacity unit assumption | Prediction is accepted as kN, inputs as m/kPa | C02 checks wrapper identity, not substituted-atlas units | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L112-L116] |
| S15, capacity out-of-range | NaN capacity on rejected anchor prediction | no detection check exists for anchor-only range rejection | issue pending (4); [src/digitalmodel/mooring_resilience/screening.py:L116-L116] |
| S16, stress unit assumption | kN × 1000 / mm² is interpreted as MPa | C05 checks explicit dimensional arithmetic | Covered arithmetic; [src/digitalmodel/mooring_resilience/screening.py:L126-L127] |
| S17, nonpositive stress | Negative stress as well as zero gives infinite life | no detection check exists for negative stress rejection | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L128-L129] |
| S18, nonpositive annual damage | Invalid cycle/coefficient choices may produce infinite life | no detection check exists for nonpositive damage rejection | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L130-L132] |
| S19, classification constants | Fixed traffic-light thresholds are screening policy | C06 checks interior points; no equality check exists | issue pending (6); [src/digitalmodel/mooring_resilience/screening.py:L135-L143] |
| S20, factors fallback | `factors or ResilienceFactors()` silently selects defaults when absent | no detection check exists for omitted design basis | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L150-L150] |
| S21, local empty notes | Notes remain empty unless atlas status is out of range | C12 covers tension rejection only | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L151-L160] |
| S22, damaged NaN | Invalid intact query suppresses damaged tension | no detection check exists for damaged NaN assertion | issue pending (4); [src/digitalmodel/mooring_resilience/screening.py:L162-L162] |
| S23, intact-utilization NaN | Invalid intact query suppresses intact utilization | C12 asserts NaN | Covered policy; [src/digitalmodel/mooring_resilience/screening.py:L164-L164] |
| S24, damaged-utilization NaN | Invalid intact query suppresses damaged utilization | no detection check exists for damaged-utilization NaN | issue pending (4); [src/digitalmodel/mooring_resilience/screening.py:L165-L165] |
| S25, foundation-utilization NaN | Either invalid query suppresses foundation utilization | no detection check exists for both failure combinations | issue pending (4); [src/digitalmodel/mooring_resilience/screening.py:L166-L166] |
| S26, fatigue-life NaN | Invalid intact query suppresses fatigue despite fatigue being algebraic | no detection check exists for fatigue-life NaN | issue pending (4); [src/digitalmodel/mooring_resilience/screening.py:L168-L168] |
| S27, margin NaN | Invalid intact query suppresses margin | no detection check exists for margin NaN | issue pending (4); [src/digitalmodel/mooring_resilience/screening.py:L169-L169] |
| S28, filtered utilization / empty maximum | NaNs are dropped, and an empty set gives NaN governing utilization; infinity is retained | no detection check exists for all-NaN or mixed-NaN inputs with valid atlas flags | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L171-L172] |
| S29, governing fallback | Missing governing utilization is replaced by 0.0 for classification | C12 covers rejected atlas; no detection check exists for NaN inputs with valid atlas flags | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L174-L174] |
| S30, margin fallback | NaN margin is replaced by infinity for classification | C12 covers rejected atlas; no detection check exists for invalid margin with valid atlas flags | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L175-L176] |
| S31, caught citation failure / empty dictionary | Any CitationResolutionError returns `{}`, including explicit missing-page failures, despite docstring fail-closed wording | C13 demonstrates fallback; no detection check exists for mismatched metadata rejection | #996, #1000; issue pending (3); [src/digitalmodel/mooring_resilience/screening.py:L194-L198] [src/digitalmodel/mooring_resilience/screening.py:L216-L224] |
| S32, one-shot warning | Subsequent citation failures can have no warning after the module flag is set | no detection check exists for repeated-call warning semantics | issue pending (3); [src/digitalmodel/mooring_resilience/screening.py:L187-L187] [src/digitalmodel/mooring_resilience/screening.py:L217-L224] |

| Additional pitfall | Symptom / existing detection | Tracking |
| --- | --- | --- |
| Unguarded scalar inputs | Zero divisors can raise; negative or nonfinite inputs are not checked in constructors or arithmetic; no detection check exists for rejection | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L43-L72] [src/digitalmodel/mooring_resilience/screening.py:L126-L132] [src/digitalmodel/mooring_resilience/screening.py:L164-L176] |
| Fixed tension configuration | `n_lines` is stored but never queried; MBL changes a denominator rather than atlas physics; no detection check exists for eligibility | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L43-L51] [src/digitalmodel/mooring_resilience/screening.py:L101-L104] [src/digitalmodel/mooring_resilience/screening.py:L164-L165] |
| Provenance and stale atlas | Direct `Atlas.load`/`predict` does not call the query router's freshness check; C14 detects missing evidence fields, not freshness | issue pending (4); [src/digitalmodel/mooring_resilience/screening.py:L90-L116] [src/digitalmodel/parametric/query.py:L51-L59] |
| Optional citation call | `assess` does not attach citations or validate custom factors against registry values; C01 covers only defaults | issue pending (3); [src/digitalmodel/mooring_resilience/screening.py:L146-L184] [tests/unit/marine_ops/test_mooring_resilience.py:L139-L147] |
| Atlas test availability guard | Test helper catches every exception and reports a parquet-engine skip even for missing or malformed atlases; C03 requires 0 skipped tests but does not identify the root cause | issue pending (4); [tests/unit/marine_ops/test_mooring_resilience.py:L28-L44] |
| README equality | README excludes utilization = 0.8 or margin = 2 from its GREEN wording, while code admits equality; no detection check exists for equality | issue pending (6); [examples/marine_ops/mooring_resilience/README.md:L20-L22] [src/digitalmodel/mooring_resilience/screening.py:L139-L143] |

Issue evidence: #974 records the screening capability request, #996 the tension citation addition, and #1000 the foundation citation addition; the latter two are provenance follow-ups rather than numerical incidents, with C01/C13 detecting their implemented default-resolution/fallback behavior. No separate incident is inferred from their closed state. [src/digitalmodel/mooring_resilience/screening.py:L3-L6] [src/digitalmodel/mooring_resilience/screening.py:L190-L224] [tests/unit/marine_ops/test_mooring_resilience.py:L139-L157]

## 7. Lessons log

| Date | Entry | Lesson / evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack | Separate screening invariants from validation; the existing tests check trends and synthetic metadata, not an independent absolute engineering reference. [tests/unit/marine_ops/test_mooring_resilience.py:L49-L157] | Regression classification and numeric gate recipes | Not assigned; issue pending (4) |
| 2026-10-18 | Citation behavior | Explicit missing-page errors become an empty citation dictionary; successful numerical calculation does not imply citation availability. [src/digitalmodel/mooring_resilience/screening.py:L146-L184] [src/digitalmodel/mooring_resilience/screening.py:L216-L224] | S31–S32 and reference limitations | Not assigned; issue pending (3) |

## 8. Ownership

Review role: engineering calculations quality manager; producing role: calculation documentation contributor; technical ambiguity and tolerance changes go to the family quality manager, with unresolved cross-family questions referred to the maintainers. The reviewer must be independent of the producer. (AQG r0, section 11, “Sign-off and independence.”)

UNVERIFIED: U3 — an appointed deputy for the engineering calculations quality manager is not established by the available module evidence; the intended deputy role is engineering calculations deputy quality manager, with appointment confirmation assigned to the maintainers, issue pending (4).

This pack remains DRAFT until independent review; the maintainer handles integration after that review. (AQG r0, section 11, “Sign-off and independence.”)

## 9. Dependencies

| Dependency / inventory locator | Role and limit |
| --- | --- |
| `parametric`, `Atlas.load` / `Atlas.predict` | Upstream file loading and interpolation; `DEFAULT_ATLAS_ROOT` is repository `atlases`. [src/digitalmodel/mooring_resilience/screening.py:L90-L116] [src/digitalmodel/parametric/query.py:L19-L20] |
| `fpso_mooring_full`, atlas `879f4f041243` | Upstream tension response `max_line_tension_N`, default pointer and manifest at the code pin. [atlases/fpso_mooring_full/default.txt:L1-L1] [atlases/fpso_mooring_full/879f4f041243/manifest.yaml:L1-L24] |
| `anchor_capacity`, atlas `c8f67ad7b69c` | Upstream holding response `capacity_kN`. [atlases/anchor_capacity/default.txt:L1-L1] [atlases/anchor_capacity/c8f67ad7b69c/manifest.yaml:L1-L29] |
| `citations`, registry getters | Optional provenance dependency; numerical `assess` is independent of the separate helper. [src/digitalmodel/mooring_resilience/screening.py:L146-L184] [src/digitalmodel/mooring_resilience/screening.py:L190-L224] |
| Example curve generator | Confirmed downstream caller; writes four plots and generated README, rather than a solver deck. [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L27-L37] [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L189-L199] |
| `mooring_fatigue` | Named higher-fidelity path in the screen documentation; not called by `assess`. Its workflow computes stress bins and Miner damage separately. [src/digitalmodel/mooring_resilience/screening.py:L17-L18] [src/digitalmodel/mooring_resilience/screening.py:L146-L184] [src/digitalmodel/mooring_fatigue/workflow.py:L58-L86] |

OrcaFlex occurrences, read as text only: the package's sole Python occurrence is the screening disclaimer at `screening.py:27`; related example occurrences are the README disclaimer and both the script docstring and generated report disclaimer. No solver version is declared at those occurrences. [src/digitalmodel/mooring_resilience/screening.py:L27-L28] [examples/marine_ops/mooring_resilience/README.md:L8-L9] [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L12-L13] [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L146-L147]

### Implementation-map inventory and follow-up

Map read pin: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; declared data revision: `270c8759c374b67a7f032953e7fcdb14e0224461`. The header-pin map declares the same data revision; the two map files differ, so their file revisions are kept distinct. [docs/registry/implementation-map.yaml:L1-L3]

Inventory observation from parsing every family's `implementations`: 0 rows have a path inside this package at either inspected map revision; module map families = 0, counted families = 0, inventory = empty. The header-pin complete family block is the reproducible comparison inventory. [docs/registry/implementation-map.yaml:L345-L26478]

Counted-family grep: `NOT_APPLICABLE` because no map family has an in-package row; no hit or miss count is substituted for an unperformed grep. Family ranking and path-matching signal removal therefore have no counted input. Map follow-up, issue pending (5): add the section-3 methods—atlas tension/anchor queries, damaged-load scaling, strength/foundation utilization, chain T-N proxy and traffic-light classification—before counting any of them. [docs/registry/implementation-map.yaml:L345-L26478] [src/digitalmodel/mooring_resilience/screening.py:L95-L184]

Additional repository search, including `scripts/`, used the exact expression below to locate related definitions; it is exploratory and does not change the counted-family result. [src/digitalmodel/mooring_resilience/screening.py:L119-L143] [src/digitalmodel/mooring_fatigue/workflow.py:L230-L239] [src/digitalmodel/riser_fatigue/workflow.py:L311-L314]

```sh
rg -n '_fatigue_life_years|dyn_tension_per_Hs_kN|def _traffic_light|fos_damaged.*redistribution|sn_a_D' --glob '*.py'
```

| Related definition | Classification / reason |
| --- | --- |
| `mooring_resilience.screening._fatigue_life_years` | Own implementation of stress-range-to-life proxy, already described in section 3; map gap. [src/digitalmodel/mooring_resilience/screening.py:L119-L132] |
| `mooring_resilience.screening._traffic_light` / `assess` | Own criteria and factored-utilization formulas, already described in section 3; map gaps. [src/digitalmodel/mooring_resilience/screening.py:L135-L184] |
| `mooring_fatigue.workflow._evaluate_line`, `_fatigue_life_years` | Related implementation outside this package: Miner-damage route and life/design conversion; not the same wave-height proxy and not a counted miss. [src/digitalmodel/mooring_fatigue/workflow.py:L58-L86] [src/digitalmodel/mooring_fatigue/workflow.py:L230-L239] |
| `fatigue.damage.miner_damage`, `design_life_check` | Related cumulative-damage implementation outside this package; not delegated to by the screen. [src/digitalmodel/fatigue/damage.py:L13-L39] [src/digitalmodel/fatigue/damage.py:L71-L87] [src/digitalmodel/mooring_resilience/screening.py:L119-L132] |
| `structural.fatigue.analysis.MultislopeSNCurve.get_allowable_cycles` | Separate multi-slope allowable-cycle algorithm outside this package; no equivalence or benchmark accuracy inferred. [src/digitalmodel/structural/fatigue/analysis.py:L171-L208] |
| `run_resilience_curves` functions | Downstream plotting/report wrappers around `assess`, not further numerical implementations. [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L47-L133] [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L136-L186] |

## 10. Coverage and open items

Source coverage: both package Python files, their complete definitions, and the single discovered module test file were read; there are 11 test functions in that file. [src/digitalmodel/mooring_resilience/__init__.py:L1-L23] [src/digitalmodel/mooring_resilience/screening.py:L1-L224] [tests/unit/marine_ops/test_mooring_resilience.py:L1-L157]

Example coverage: `examples/marine_ops/mooring_resilience/README.md` and `run_resilience_curves.py` were read; chart images are generated outputs, not independent reference data, and no values were digitized from them. [examples/marine_ops/mooring_resilience/README.md:L34-L50] [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L189-L199]

Documentation discovery used `rg -n 'mooring_resilience|mooring.resilience' docs`; the matching documents read were the following, including metadata rather than treating it as engineering validation. [docs/maps/digitalmodel-operator-map.md:L50-L50] [docs/capability-map/capability-cards.json:L272-L276]

| Discovered document | Relevant evidence |
| --- | --- |
| `docs/maps/digitalmodel-operator-map.md` | Routing row; [docs/maps/digitalmodel-operator-map.md:L50-L50] |
| `docs/registry/module-routing.yaml` | Existing beta module entry; [docs/registry/module-routing.yaml:L235-L240] |
| `docs/capability-map/capability-cards.json` | Screen and related geotechnical summaries; [docs/capability-map/capability-cards.json:L272-L276] [docs/capability-map/capability-cards.json:L453-L455] |
| `docs/capability-map/capabilities-added.yml` | Engine registration; [docs/capability-map/capabilities-added.yml:L64-L64] |
| `docs/capability-map/capabilities-sections.yml` | Example link discovery; [docs/capability-map/capabilities-sections.yml:L160-L160] |
| `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md` | Reuse listing; its wider capability counts are not adopted; [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L21-L21] |
| `docs/archive/archive-candidates-2026-10-08.summary.json` | Four example plot paths, discovery metadata only; [docs/archive/archive-candidates-2026-10-08.summary.json:L1513-L1544] |
| `docs/api/structural/anchor-holding-explorer.html` | Related anchor-screen description, not a module validation case; [docs/api/structural/anchor-holding-explorer.html:L36-L40] |

Licensed standard text was not inspected; clause text not checked against the licensed copy. API and C203 editions and missing clause titles remain U1 rather than invented references. [src/digitalmodel/mooring_resilience/screening.py:L20-L22] [src/digitalmodel/mooring_resilience/screening.py:L62-L72]

No private sidecar or client-derived value is included; no sidecar entry was required for the public code/test facts documented here. [src/digitalmodel/mooring_resilience/screening.py:L43-L72] [tests/unit/marine_ops/test_mooring_resilience.py:L23-L25]

| Open item / acceptance limit | Responsibility | Tracking |
| --- | --- | --- |
| Positive/finite scalar and factor guards; fixed-configuration eligibility; error substitutions and unvalidated unit schema (S05, S10, S12, S14, S17–S18, S20–S21, S28–S30) | Maintainers | issue pending (1); [src/digitalmodel/mooring_resilience/screening.py:L43-L72] [src/digitalmodel/mooring_resilience/screening.py:L95-L176] |
| U1: standards applicability, API/C203 editions and clauses, fatigue/redistribution calibration and tension-atlas edition mismatch (S01–S04, S06–S09) | Maintainers | issue pending (2); [src/digitalmodel/mooring_resilience/screening.py:L62-L72] [src/digitalmodel/citations/registry.py:L26-L31] [atlases/fpso_mooring_full/879f4f041243/manifest.yaml:L34-L35] |
| Citation suppression, repeated-warning coverage and custom-factor/result sidecar parity (S31–S32) | Maintainers | issue pending (3); previous additions #996 and #1000; [src/digitalmodel/mooring_resilience/screening.py:L190-L224] |
| U2: independent absolute references and eligibility; U3: deputy appointment; missing evidence/freshness records, shared-log identifiers, anchor-only rejection and NaN branch assertions (S11, S15, S22, S24–S27) | Maintainers / engineering calculations quality manager | issue pending (4); [src/digitalmodel/mooring_resilience/screening.py:L75-L116] [tests/unit/marine_ops/test_mooring_resilience.py:L28-L44] [tests/unit/marine_ops/test_mooring_resilience.py:L79-L157] |
| Implementation-map coverage of every section-3 family; exploratory outside-package findings need classification in the map | Maintainers | issue pending (5); [docs/registry/implementation-map.yaml:L345-L26478] [src/digitalmodel/mooring_resilience/screening.py:L95-L184] |
| Threshold equality in generated README; boundary assertions missing (S19) | Maintainers | issue pending (6); [examples/marine_ops/mooring_resilience/README.md:L20-L22] [examples/marine_ops/mooring_resilience/run_resilience_curves.py:L158-L160] [tests/unit/marine_ops/test_mooring_resilience.py:L49-L55] |

Draft issue references require replacement with issued numbers before acceptance item A7 can be finally cleared; no numerical or solver approval is asserted by this DRAFT pack. (AQG r0, section 11, “Sign-off and independence.”)
