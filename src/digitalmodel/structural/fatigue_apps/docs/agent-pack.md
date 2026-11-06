# Fatigue applications knowledge pack

| Header | Value |
|---|---|
| Module | structural/fatigue_apps |
| Repository | digitalmodel |
| Package | src/digitalmodel/structural/fatigue_apps |
| Pack revision | 1, 2026-10-17 |
| Code revision | 7aad603be1c6136106dbb2381ce5096b050654f8 |
| Analysis family | Engineering calculations (EC) |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT; independent clearance outstanding |
| Map data revision | 3256bab95f84da3d9fadce8d5dcf8458d66d1eb4 |

Code citations resolve at the code revision; citations to `docs/registry/implementation-map.yaml` resolve at the separate map data revision.

## 1. Purpose and scope

The public API exports rainflow counting, S-N damage calculation, reference-seastate processing, tension-to-stress conversion and visualisation; its main processing path combines scaled wind and wave tension histories before cycle counting and damage calculation. [src/digitalmodel/structural/fatigue_apps/__init__.py:L26-L56] [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L364-L428]

The enclosing structural domain is marked stable; that domain-level label does not establish validation of every application in this package. [docs/registry/module-routing.yaml:L338-L343]

The package scope includes its `fatigue_validation` and `examples` subfolders; `FatigueValidator.validate_all` is a placeholder returning `NOT_IMPLEMENTED`, rather than an implemented compliance assessment. [src/digitalmodel/structural/fatigue_apps/fatigue_validation/validator.py:L32-L50]

Results from the default damage path require qualification because its branch selection compares stress with a threshold documented in cycles, and missing cases can yield infinite configuration life; neither path is an independent engineering reference. [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L28-L33] [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L111-L129] [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L457-L475]

## 2. Inputs and outputs

| Input | Unit | Range and default handling | Source |
|---|---|---|---|
| Environmental condition | wind speed m/s; wave height m; period s; headings degrees; occurrence % | Dataclass declares fields without validation; CSV missing columns receive defaults. | [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L39-L48] [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L192-L211] |
| Reference histories | time s; tension kN by column convention | First CSV column is time; tension column is selected by text; missing file/column/read error produces empty arrays. | [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L252-L282] [tests/structural/fatigue_analysis/test_reference_seastate_processor.py:L333-L341] |
| Conversion table | tension kN; stress MPa | Optional CSV; absent table selects an assumed linear relation; `np.interp` clamps beyond endpoints. | [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L70-L102] |
| S-N parameters, SCF, design life | stress MPa; SCF dimensionless; life years | Optional curve selects a built-in curve; no physical range check is made in the constructor. | [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L25-L43] [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L89-L106] |
| Sample truncation | samples | `None` retains the full input; a truthy sample limit slices both arrays. | [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L133-L153] [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L267-L273] |

| Output | Unit / representation | Source |
|---|---|---|
| `FatigueResult` | condition/configuration identifiers, range count, tension and stress statistics, dimensionless damage, life years, occurrence % | [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L50-L65] [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L412-L428] |
| Detailed CSV, summary JSON, Markdown report | tabular results; configuration and overall life | [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L588-L635] |
| Cycle CSV | Range, Counts; optional statistics header | [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L293-L325] |

Directions are compared modulo a full turn with circular distance; malformed direction tokens are skipped, and wave selection uses a fixed period candidate list. [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L309-L352]

UNVERIFIED: a geographic heading origin, handedness, phase convention and coordinate frame are not established for these input files; a design-basis declaration is required before physical use (issue pending (2)).

The reference processor discards input timestamps during superposition, truncates to the shorter history, and annualises using a fixed sample interval rather than the loaded time vector. [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L379-L409]

## 3. Method and governing standards

| Method / implementation | Basis and limits | Evidence |
|---|---|---|
| `ReferenceSeaStateProcessor.process_fatigue_condition` | Quadratic wind scaling, linear wave scaling and addition of the two histories; source histories are shortened to equal length. | [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L364-L391] |
| `RainflowCounter.count_cycles`, `_extract_reversals`, `_rainflow_algorithm`, `_aggregate_cycles` in `rainflow_counter.py` | Reversal extraction, stack cycle counting, post-count absolute gate and range aggregation; ASTM E1049-85, *Standard Practices for Cycle Counting in Fatigue Analysis*, and Downing and Socie, *Simple rainflow counting algorithms*, are named in the docstring; published source: 1982, *International Journal of Fatigue*, 4(1), 31–40 ([publisher record](https://www.sciencedirect.com/science/article/pii/0142112382900184)). Clause text not checked against the licensed copy. | [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L3-L11] [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L46-L119] [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L163-L259] |
| `FatigueDamageCalculator.cycles_to_failure`, `calculate_damage`, `calculate_fatigue_life`, `calculate_combined_damage` | Piecewise S-N evaluation, sum of cycle count / cycles to failure, occurrence weighting, annualisation and reciprocal life; the stress/cycle threshold defect prevents use as a hand-check reference. | [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L108-L231] [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L323-L361] |
| `FatigueDamageCalculator.mean_stress_correction` | Implements three named mean-stress corrections with assumed strengths and an unknown-method identity fallback. | [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L233-L268] |
| Alternate `fatigue_damage.SNCurve` and `FatigueDamageCalculator` | Separate curve library, stress threshold derived from cycle threshold, damage accumulation, annual damage and reciprocal life; it is a separate implementation, not the exported calculator. | [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L40-L147] [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L170-L254] |
| `rainflow_counting.RainflowCounter.rainflow_counting_astm` | Separate cycle algorithm; batch damage use is explicitly refused through the counting contract. | [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L127-L196] [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L343-L359] |
| `FatigueVisualizer.plot_sn_curve` | Independently generates S-N curve ordinates for plotting; this is not a published validation source. | [src/digitalmodel/structural/fatigue_apps/visualizer.py:L42-L115] |
| Example `LoadScalingProcessor.process_fatigue_condition` | Independently scales and combines reference histories. | [src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py:L143-L184] |
| `TensionStressConverter` | Alternative lookup converter extrapolates through SciPy; this differs from the exported converter's endpoint clamping. | [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L134-L177] [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L88-L102] |

The alternate curve library includes a Eurocode 3 preset without an edition or clause; clause text not checked against the licensed copy. [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L103-L110]

The exported damage calculator names the ABS *Guide for Fatigue Assessment of Offshore Structures* and DNV-RP-C203, *Fatigue Design of Offshore Steel Structures*, without editions or clauses; clause text not checked against the licensed copy. [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L9-L12]

UNVERIFIED: ABS and DNV edition, clause and built-in parameter provenance, Eurocode 3 edition/clause qualification, and the absence of a matching calculation Citation sidecar remain unresolved; no standards-derived tabulated value is reproduced here (issue pending (1)).

## 4. Validated worked examples

The following synthetic cases assess bounded numerical behavior; they do not qualify a physical operating profile or the built-in S-N tables. [tests/structural/fatigue_analysis/test_reference_seastate_processor.py:L228-L248] [tests/structural/fatigue_analysis/test_rainflow_counter.py:L26-L41]

| Case | Classification and independent basis | Inputs / expected value / criterion | Existing test node ID | Offline result |
|---|---|---|---|---|
| V1 harmonic weighted life | Validated arithmetic: reciprocal weighted reciprocal lives, hand expression in test | Two equal weights and synthetic lives; 24.0 years; absolute error <0.1 years | `tests/structural/fatigue_analysis/test_reference_seastate_processor.py::TestReferenceSeaStateProcessor::test_calculate_overall_fatigue_life` [tests/structural/fatigue_analysis/test_reference_seastate_processor.py:L228-L248] | PASS at code pin, BASE and AFTER |
| V2 sine range | Validated range only: closed-form peak-to-peak sine amplitude | Synthetic sine; maximum range strictly between 195 and 205 load units, representing the test's discretisation allowance | `tests/structural/fatigue_analysis/test_rainflow_counter.py::TestRainflowCounter::test_simple_sine_wave` [tests/structural/fatigue_analysis/test_rainflow_counter.py:L26-L41] | PASS at code pin, BASE and AFTER |
| R1 lookup knots | Regression: synthetic lookup table, no independent structural calibration | Exact agreement at all four tested table knots, MPa | `tests/structural/fatigue_analysis/test_reference_seastate_processor.py::TestTensionToStressConverter::test_custom_lookup_table` [tests/structural/fatigue_analysis/test_reference_seastate_processor.py:L59-L75] | PASS at code pin, BASE and AFTER |

The source-attributed ASTM example is not counted as a validated worked example because its underlying published table has not been checked; its local assertions do not establish a checked standards benchmark and it is not listed as a worked example. [tests/structural/fatigue_analysis/test_rainflow_counter.py:L196-L214]

## 5. QA gates

The gate assignment follows AQG r0, section 10, *Python calculations*, and section 9, *QA appendix*: G0 inputs, G1 imports/tests, G2 independent checks and G5 evidence apply; G3 dynamics and G4 diffraction are NOT_APPLICABLE because this calculation route consumes histories and performs offline fatigue processing. [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L364-L428]

These are executable acceptance checks, not a claim that all production controls exist; missing controls are tracked in section 10.

| Gate | Check | Function / execution and evidence | Numeric pass criterion and unit |
|---|---|---|---|
| G0 applicable | C0 input rejection | Existing CLI missing-input test exercises `validate_inputs`; records return status and error messages. | Exactly 1 false return, at least 1 error message; counts. [tests/structural/fatigue_analysis/test_fatigue_cli.py:L140-L154] |
| G1 applicable | C1 Python load and offline assertions | Run the discovered rainflow, reference-processor and CLI suites; import occurs during collection, no solver program is targeted. | 0 collection errors and 0 failed assertions; counts. [tests/structural/fatigue_analysis/test_rainflow_counter.py:L7-L11] [tests/structural/fatigue_analysis/test_reference_seastate_processor.py:L17-L24] [tests/structural/fatigue_analysis/test_fatigue_cli.py:L21-L26] |
| G2 applicable | C2 harmonic life | Run V1; compare `calculate_overall_fatigue_life` with independently written reciprocal-weighted expression. | Absolute error <0.1 years at 24.0 years. [tests/structural/fatigue_analysis/test_reference_seastate_processor.py:L228-L248] |
| G2 applicable | C3 sine range | Run V2 through exported `RainflowCounter.count_cycles`. | 195 < maximum range <205 load units. [tests/structural/fatigue_analysis/test_rainflow_counter.py:L26-L41] |
| G3 NOT_APPLICABLE | No dynamics solver | Offline fatigue calculations do not integrate a dynamic model. | No dynamics run criterion; NOT_APPLICABLE. [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L364-L428] |
| G4 NOT_APPLICABLE | No diffraction body | No diffraction model is produced by this route. | No mesh criterion; NOT_APPLICABLE. [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L364-L428] |
| G5 applicable | C5 output readback | Existing `test_save_results` reads generated CSV/JSON from `save_results` and checks output artifacts. | 3 named artifacts present; CSV rows =1; summary life =25.0 years. This checks serialization, not a generated AQG appendix. [tests/structural/fatigue_analysis/test_reference_seastate_processor.py:L250-L300] |
| Defect characterisation (outside G0 acceptance) | C6 empty-input behavior | `test_missing_data_path`, `test_invalid_csv_format`, `test_empty_reference_selection` exercise the loader/selector fallback. | 0 samples in each returned array; 0 characters in each missing reference string. This documents fallback and does not reject incomplete assessments. [tests/structural/fatigue_analysis/test_reference_seastate_processor.py:L432-L480] |
| G1 applicable | C7 empty cycle behavior | `RainflowCounter.count_cycles` with an empty history and its statistics. | 0 ranges, 0 counts, total cycles =0 cycles. [tests/structural/fatigue_analysis/test_rainflow_counter.py:L87-L98] |
| G1 applicable | C8 lookup regression | Run R1 through `TensionToStressConverter`. | Error =0 MPa at tested synthetic knots; no extrapolation qualification. [tests/structural/fatigue_analysis/test_reference_seastate_processor.py:L59-L75] |
| G1 applicable | C9 explicit stale-test collection | Run `python -m pytest tests/structural/fatigue_apps/test_load_scaling.py --collect-only -q` explicitly; repository discovery ignores this file, so C1 cannot detect its stale import. | 0 collection errors; currently 1, exposing #1923. [tests/structural/fatigue_apps/test_load_scaling.py:L25-L25] [tests/conftest.py:L53-L57] |

G1 has a Python import/test route only: no external program deck is written or loaded by this reference processor. [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L588-L635]

## 6. Known failure modes and pitfalls

sweep: 182 found

This count records reviewed candidate locations, including intended empty-input behavior and explicit failure returns; it is not a count of confirmed defects. Each location below has a disposition and either a section-5 detection check or an explicit detection gap.

The sweep includes all package Python sources, each `except`, record-skipping `continue`, error-to-empty return, numeric fallback, text conversion, unit assumption and empty cache/output path; the broad search is followed by enclosing-definition inspection.

```bash
rg -n --glob '*.py' 'except|continue|return None|return \[\]|return \{\}|np\.array\(\[\]\)|= 0(\.0)?\b|1970|int\(|float\(|\.get\(|cache|unit|timestep' src/digitalmodel/structural/fatigue_apps
```

| Reference / defect | Symptom and existing detection | Evidence |
|---|---|---|
| #1923 | Stale `LoadScaler` import is suppressed by repository collection policy; explicit collection exposes the absent name. Detection: C9 (collection failure); no implementation correction is made by this pack. | [tests/structural/fatigue_apps/test_load_scaling.py:L25-L25] [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L54-L64] [tests/conftest.py:L53-L57] |
| issue pending (4) | Alternate rainflow path is documented as failing the counting contract; its batch damage handoff raises while diagnostic APIs remain callable. No section-5 distribution detection check exists for this path; no detection check exists for its discarded filter. | [docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md:L227-L261] [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L222-L245] [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L343-L361] |

### Sweep dispositions

Inspection date: 2026-10-17; source ranges resolve at the code pin.

| Location at code pin | Symptom / disposition | Detection and tracking |
|---|---|---|
| S001: `reference_seastate_processor.py:77-98` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L72-L103] | Missing or absent lookup silently selects assumed linear conversion; interpolation clips outside table. | C8 documents lookup behavior only; no detection check exists for missing-table rejection or clipping; issue pending (2). |
| S002: `reference_seastate_processor.py:109-130` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L104-L135] | Built-in configuration weights are selected without a supplied operating profile. | no detection check exists; issue pending (2). |
| S003: `reference_seastate_processor.py:155-161` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L150-L166] | Reference scaling and absolute rainflow gate assume engineering units; gate comment calls it a percentage though counter uses absolute range. | no detection check exists; issue pending (2). |
| S004: `reference_seastate_processor.py:192-205` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L187-L210] | External CSV int/float conversions lack locale normalization; each absent physical column silently receives a numeric default including zero headings. | no detection check exists; issue pending (2). |
| S005: `reference_seastate_processor.py:210-211` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L205-L216] | Missing named conditions file silently substitutes sample conditions. | no detection check exists; issue pending (2). |
| S006: `reference_seastate_processor.py:251-253` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L246-L258] | Missing strut input becomes two empty arrays. | C6 documents fallback only; no detection check exists for assessment completeness; issue pending (2). |
| S007: `reference_seastate_processor.py:276-278` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L271-L283] | Missing tension column becomes two empty arrays. | C6 documents fallback only; no detection check exists for assessment completeness; issue pending (2). |
| S008: `reference_seastate_processor.py:280-282` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L275-L287] | Every read exception is logged and becomes empty arrays; caller cannot distinguish corrupt input from no data. | no detection check exists; issue pending (2). |
| S009: `reference_seastate_processor.py:295-297` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L290-L302] | Missing configuration directory becomes empty reference strings. | C6 documents fallback only; no detection check exists for assessment completeness; issue pending (2). |
| S010: `reference_seastate_processor.py:312-314` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L307-L319] | Empty reference list becomes empty reference string. | no detection check exists; issue pending (2). |
| S011: `reference_seastate_processor.py:319-333` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L314-L338] | External direction text conversion failure silently skips that reference; all invalid entries become empty selection. | no detection check exists; issue pending (2). |
| S012: `reference_seastate_processor.py:339-340` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L334-L345] | Empty wave list becomes empty reference string. | no detection check exists; issue pending (2). |
| S013: `reference_seastate_processor.py:343-353` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L338-L358] | Wave selection assumes fixed available periods and falls back to all periods if period-specific reference is absent. | no detection check exists; issue pending (2). |
| S014: `reference_seastate_processor.py:377-379` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L372-L384] | Missing selected wind or wave reference drops condition with None. | no detection check exists; issue pending (2). |
| S015: `reference_seastate_processor.py:385-386` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L380-L391] | Empty wind or wave arrays drop condition with None. | no detection check exists; issue pending (2). |
| S016: `reference_seastate_processor.py:388-391` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L383-L396] | Reference lengths are silently truncated; time arrays are discarded so time alignment is not checked. | no detection check exists; issue pending (2). |
| S017: `reference_seastate_processor.py:401-402` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L396-L407] | No rainflow cycles drops condition with None. | no detection check exists; issue pending (2). |
| S018: `reference_seastate_processor.py:407-412` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L402-L417] | Annualization assumes fixed timestep instead of reading supplied time vector. | no detection check exists; issue pending (2). |
| S019: `reference_seastate_processor.py:423-423` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L418-L428] | Empty stress extrema default to zero. | no detection check exists; issue pending (2). |
| S020: `reference_seastate_processor.py:451-453` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L446-L458] | Missing configuration drops whole configuration as empty dict. | no detection check exists; issue pending (2). |
| S021: `reference_seastate_processor.py:457-475` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L452-L480] | Zero-initialized damage receives only successful conditions; unprocessed struts therefore become infinite fatigue life. | no detection check exists; issue pending (2). |
| S022: `reference_seastate_processor.py:478-483` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L473-L488] | Empty strut set becomes infinite minimum life and no critical strut. | no detection check exists; issue pending (2). |
| S023: `reference_seastate_processor.py:505-515` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L500-L520] | Infinite-life configuration contributions are omitted; empty or zero total damage becomes infinite overall life. | no detection check exists; issue pending (2). |
| S024: `reference_seastate_processor.py:553-555` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L548-L560] | Unknown requested configuration is skipped. | no detection check exists; issue pending (2). |
| S025: `reference_seastate_processor.py:557-587` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L552-L592] | Empty configuration results are omitted but analysis still saves summary and returns True. | no detection check exists; issue pending (2). |
| S026: `reference_seastate_processor.py:633-633` [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L628-L635] | Missing overall-life summary key becomes zero in report. | no detection check exists; issue pending (3). |
| S027: `integrated_processor.py:57-68` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L52-L73] | Missing lookup silently selects assumed linear conversion; interpolation clips outside table. | no detection check exists; issue pending (2). |
| S028: `integrated_processor.py:73-112` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L68-L117] | Default data-source fallback and built-in configuration weights can replace explicit production basis. | no detection check exists; issue pending (2). |
| S029: `integrated_processor.py:118-120` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L113-L125] | Missing configuration path becomes empty list. | no detection check exists; issue pending (2). |
| S030: `integrated_processor.py:125-140` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L120-L145] | Reference names are parsed with raw int/float; unknown names become empty dict and missing period gets assumed value. | no detection check exists; issue pending (2). |
| S031: `integrated_processor.py:147-149` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L142-L154] | Missing input becomes empty arrays. | no detection check exists; issue pending (2). |
| S032: `integrated_processor.py:161-163` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L156-L168] | Missing tension column becomes empty arrays. | no detection check exists; issue pending (2). |
| S033: `integrated_processor.py:168-170` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L163-L175] | Default sample mode truncates input to configured sample length. | no detection check exists; issue pending (2). |
| S034: `integrated_processor.py:174-176` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L169-L181] | All read exceptions become empty arrays. | no detection check exists; issue pending (2). |
| S035: `integrated_processor.py:183-187` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L178-L192] | Reference scaling assumes fixed engineering units; absolute rainflow gate is labelled as a percentage. | no detection check exists; issue pending (2). |
| S036: `integrated_processor.py:212-213` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L207-L218] | Absent fatigue CSV silently substitutes samples. | no detection check exists; issue pending (2). |
| S037: `integrated_processor.py:219-226` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L214-L231] | External CSV int/float lacks locale normalization; missing columns silently receive physical defaults including zero headings. | no detection check exists; issue pending (2). |
| S038: `integrated_processor.py:258-259` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L253-L264] | No reference directories becomes empty selection string. | no detection check exists; issue pending (2). |
| S039: `integrated_processor.py:267-268` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L262-L273] | No matching reference type becomes empty selection string. | no detection check exists; issue pending (2). |
| S040: `integrated_processor.py:302-304` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L297-L309] | Missing references drop condition as empty dict. | no detection check exists; issue pending (2). |
| S041: `integrated_processor.py:310-311` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L305-L316] | Empty reference data drop condition as empty dict. | no detection check exists; issue pending (2). |
| S042: `integrated_processor.py:314-316` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L309-L321] | Wind/wave arrays silently truncated without timestamp comparison. | no detection check exists; issue pending (2). |
| S043: `integrated_processor.py:330-348` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L325-L353] | Nonpositive ranges are ignored and zero damage becomes infinite life; annualization assumes fixed timestep. | no detection check exists; issue pending (2). |
| S044: `integrated_processor.py:358-358` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L353-L363] | Empty stress result reports zero maximum stress. | no detection check exists; issue pending (3). |
| S045: `integrated_processor.py:389-392` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L384-L397] | Missing configuration path skips configuration. | no detection check exists; issue pending (2). |
| S046: `integrated_processor.py:400-428` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L395-L433] | All struts start at zero damage while demo processes only selected struts and conditions; omitted struts become infinite life. | no detection check exists; issue pending (2). |
| S047: `integrated_processor.py:449-456` [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L444-L461] | Infinite-life summaries are omitted from weighted damage; no positive total becomes infinite overall life. | no detection check exists; issue pending (2). |
| S048: `strut_foundation_processor.py:61-112` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L56-L117] | Default sample limit and built-in operating configuration weights can substitute for explicit production basis. | no detection check exists; issue pending (2). |
| S049: `strut_foundation_processor.py:155-176` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L150-L181] | Reference numeric suffix raw int conversion; missing suffix defaults to first reference; new references assume zero heading and fixed physical conditions. | no detection check exists; issue pending (2). |
| S050: `strut_foundation_processor.py:180-205` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L175-L210] | Legacy raw int suffix conversion; out-of-range index maps to zero heading, unknown name becomes empty dict. | no detection check exists; issue pending (2). |
| S051: `strut_foundation_processor.py:221-223` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L216-L228] | Missing reference file becomes empty arrays. | no detection check exists; issue pending (2). |
| S052: `strut_foundation_processor.py:238-240` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L233-L245] | Missing tension column becomes empty arrays. | no detection check exists; issue pending (2). |
| S053: `strut_foundation_processor.py:247-249` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L242-L254] | Default sample mode truncates time history. | no detection check exists; issue pending (2). |
| S054: `strut_foundation_processor.py:253-255` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L248-L260] | Every input read exception becomes empty arrays. | no detection check exists; issue pending (2). |
| S055: `strut_foundation_processor.py:261-263` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L256-L268] | Scaling assumes fixed reference physical conditions and units. | no detection check exists; issue pending (2). |
| S056: `strut_foundation_processor.py:273-275` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L268-L280] | Missing fatigue CSV substitutes generated sample conditions. | no detection check exists; issue pending (2). |
| S057: `strut_foundation_processor.py:281-288` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L276-L293] | External fatigue CSV int/float conversion lacks locale normalization; missing columns receive defaults including zero headings. | no detection check exists; issue pending (2). |
| S058: `strut_foundation_processor.py:298-300` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L293-L305] | Missing sea-state CSV substitutes generated sample states. | no detection check exists; issue pending (2). |
| S059: `strut_foundation_processor.py:305-313` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L300-L318] | External sea-state CSV float conversion lacks locale normalization; missing columns receive defaults including zero headings. | no detection check exists; issue pending (2). |
| S060: `strut_foundation_processor.py:404-405` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L399-L410] | No available reference type becomes empty string. | no detection check exists; issue pending (2). |
| S061: `strut_foundation_processor.py:423-424` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L418-L429] | No parseable reference type becomes empty string. | no detection check exists; issue pending (2). |
| S062: `strut_foundation_processor.py:426-433` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L421-L438] | New naming format selects baseline or first available reference independently of requested direction and period. | no detection check exists; issue pending (2). |
| S063: `strut_foundation_processor.py:470-472` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L465-L477] | Missing condition reference returns empty array and metadata dict. | no detection check exists; issue pending (2). |
| S064: `strut_foundation_processor.py:478-479` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L473-L484] | Empty condition reference input returns empty array and metadata dict. | no detection check exists; issue pending (2). |
| S065: `strut_foundation_processor.py:481-484` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L476-L489] | Condition histories truncated to common length without timestamp alignment. | no detection check exists; issue pending (2). |
| S066: `strut_foundation_processor.py:528-530` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L523-L535] | Missing sea-state reference returns empty array and metadata dict. | no detection check exists; issue pending (2). |
| S067: `strut_foundation_processor.py:536-537` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L531-L542] | Empty sea-state reference input returns empty array and metadata dict. | no detection check exists; issue pending (2). |
| S068: `strut_foundation_processor.py:539-542` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L534-L547] | Sea-state histories truncated to common length without timestamp alignment. | no detection check exists; issue pending (2). |
| S069: `strut_foundation_processor.py:590-593` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L585-L598] | No configuration files skips entire configuration. | no detection check exists; issue pending (2). |
| S070: `strut_foundation_processor.py:624-624` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L619-L629] | Exported time assumes fixed timestep rather than loaded timestamps. | no detection check exists; issue pending (2). |
| S071: `integrated_processor_with_naming.py:38-42` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L33-L47] | Missing explicit stress lookup falls back to assumed linear converter. | no detection check exists; issue pending (2). |
| S072: `integrated_processor_with_naming.py:104-106` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L99-L111] | Empty scaled history drops case as None. | no detection check exists; issue pending (2). |
| S073: `integrated_processor_with_naming.py:110-112` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L105-L117] | Exported time assumes fixed timestep. | no detection check exists; issue pending (2). |
| S074: `integrated_processor_with_naming.py:138-141` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L133-L146] | Annualization assumes fixed timestep; zero damage becomes infinite life. | no detection check exists; issue pending (2). |
| S075: `integrated_processor_with_naming.py:149-149` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L144-L154] | Zero total cycles defaults damage-per-cycle to zero. | no detection check exists; issue pending (2). |
| S076: `integrated_processor_with_naming.py:166-166` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L161-L171] | Empty stress array defaults maximum stress to zero. | no detection check exists; issue pending (2). |
| S077: `integrated_processor_with_naming.py:185-186` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L180-L191] | Omitted condition argument silently limits processing to first three conditions. | no detection check exists; issue pending (2). |
| S078: `integrated_processor_with_naming.py:237-237` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L232-L242] | Configuration with no successful conditions returns empty list and dict. | no detection check exists; issue pending (2). |
| S079: `integrated_processor_with_naming.py:251-253` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L246-L258] | Omitted sea-state argument silently limits processing to first four states. | no detection check exists; issue pending (2). |
| S080: `integrated_processor_with_naming.py:302-302` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L297-L307] | Configuration with no successful sea states returns empty list and dict. | no detection check exists; issue pending (2). |
| S081: `integrated_processor_with_naming.py:335-338` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L330-L343] | Missing configuration path skips configuration. | no detection check exists; issue pending (2). |
| S082: `integrated_processor_with_naming.py:361-369` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L356-L374] | Infinite minimum-life configurations omitted; zero weighted damage becomes infinite overall life. | no detection check exists; issue pending (2). |
| S083: `strut_foundation_processor_with_progress.py:57-60` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py:L52-L65] | Configuration with no files is skipped while progress advances. | no detection check exists; issue pending (2). |
| S084: `strut_foundation_processor_with_progress.py:97-98` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py:L92-L103] | Empty scaled history drops case as None. | no detection check exists; issue pending (2). |
| S085: `strut_foundation_processor_with_progress.py:104-104` [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py:L99-L109] | Exported time assumes fixed timestep. | no detection check exists; issue pending (2). |
| S086: `load_scaling.py:97-99` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L92-L104] | Invalid total occurrence logs warning and continues. | no detection check exists; issue pending (2). |
| S087: `load_scaling.py:148-150` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L143-L155] | Header-only reference CSV caches and returns an empty tension array without readiness validation. | no detection check exists; issue pending (2). |
| S088: `load_scaling.py:201-202` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L196-L207] | Empty wind candidates become None. | no detection check exists; issue pending (2). |
| S089: `load_scaling.py:206-213` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L201-L218] | External direction strings accepted by a narrow decimal predicate; invalid strings get infinity and all-invalid selection still chooses a row. | no detection check exists; issue pending (2). |
| S090: `load_scaling.py:219-220` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L214-L225] | Empty wave candidates become None. | no detection check exists; issue pending (2). |
| S091: `load_scaling.py:226-246` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L221-L251] | External direction and period text uses narrow decimal predicate; all-invalid scores still choose a row. | no detection check exists; issue pending (2). |
| S092: `load_scaling.py:272-274` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L267-L279] | External wind text converted directly; absent or nonpositive reference speed produces zero scale before clipping. | no detection check exists; issue pending (2). |
| S093: `load_scaling.py:277-279` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L272-L284] | External wave text converted directly; absent or nonpositive reference height produces zero scale before clipping. | no detection check exists; issue pending (2). |
| S094: `load_scaling.py:281-289` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L276-L294] | Out-of-range scaling is clipped to configured limits without recording pre-clipping exceedance in result. | no detection check exists; issue pending (2). |
| S095: `load_scaling.py:335-335` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L330-L340] | Missing processing timestep silently takes fixed default. | no detection check exists; issue pending (2). |
| S096: `load_scaling.py:374-377` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L369-L382] | Case exceptions are dropped when continue_on_error is enabled; no failed-case manifest stored here. | no detection check exists; issue pending (2). |
| S097: `load_scaling.py:394-403` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L389-L408] | Efficient-writer import errors select fallback format. | no detection check exists; issue pending (2). |
| S098: `load_scaling.py:408-411` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L403-L416] | Writer NameError selects legacy writer, concealing the cause. | no detection check exists; issue pending (2). |
| S099: `fatigue_damage_calculator.py:33-33` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L28-L38] | Stress units default to MPa and are metadata only; inputs are not converted or validated. | no detection check exists; issue pending (2). |
| S100: `fatigue_damage_calculator.py:119-129` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L114-L134] | Bilinear branch compares stress with parameter documented as cycle threshold; stress and cycle units differ. | no detection check exists; issue pending (1). |
| S101: `fatigue_damage_calculator.py:148-149` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L143-L154] | Empty stress input reports zero damage. | no detection check exists; issue pending (3). |
| S102: `fatigue_damage_calculator.py:154-156` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L149-L161] | zip truncates unmatched arrays and nonpositive stress ranges are omitted. | no detection check exists; issue pending (2). |
| S103: `fatigue_damage_calculator.py:175-180` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L170-L185] | Invalid nonpositive duration uses unannualized value for log-only annual calculation. | no detection check exists; issue pending (2). |
| S104: `fatigue_damage_calculator.py:209-216` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L204-L221] | Annualization assumes fixed year conversion; nonpositive weighted damage becomes infinite life. | no detection check exists; issue pending (2). |
| S105: `fatigue_damage_calculator.py:244-269` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L239-L274] | Fixed steel strengths are assumed; unknown mean-stress method silently leaves range unchanged and zip truncates unmatched arrays. | no detection check exists; issue pending (2). |
| S106: `fatigue_damage_calculator.py:280-281` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L280-L281] | No detailed damage results returns empty DataFrame. | no detection check exists; issue pending (2). |
| S107: `fatigue_damage_calculator.py:304-306` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L304-L306] | No detailed damage results skips export. | no detection check exists; issue pending (3). |
| S108: `fatigue_damage_calculator.py:335-336` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L330-L341] | No configurations returns empty dict. | no detection check exists; issue pending (2). |
| S109: `fatigue_damage_calculator.py:338-347` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L333-L352] | Absent weight map means equal weighting; missing key in supplied map silently receives full weight. | no detection check exists; issue pending (2). |
| S110: `fatigue_damage_calculator.py:351-354` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L346-L359] | Nonpositive combined damage becomes infinite life. | no detection check exists; issue pending (2). |
| S111: `fatigue_damage.py:114-118` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L109-L123] | Unknown curve type logs warning and silently selects default standard curve. | no detection check exists; issue pending (1). |
| S112: `fatigue_damage.py:130-135` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L125-L140] | Nonpositive stress and missing endurance limit default lead to infinite cycles; unit input validation absent. | no detection check exists; issue pending (2). |
| S113: `fatigue_damage.py:174-176` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L169-L181] | Annualization assumes fixed year conversion. | no detection check exists; issue pending (2). |
| S114: `fatigue_damage.py:197-207` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L192-L212] | Empty ranges return zero; zip truncates lengths and nonfinite cycles-to-failure are omitted. | no detection check exists; issue pending (2). |
| S115: `fatigue_damage.py:245-246` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L240-L251] | Nonpositive annual damage becomes infinite life. | no detection check exists; issue pending (2). |
| S116: `fatigue_damage.py:284-292` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L279-L297] | Empty stress CSV reports zero damage, zero annual damage and infinite life. | no detection check exists; issue pending (3). |
| S117: `fatigue_damage.py:295-297` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L290-L302] | Missing expected stress/count column names silently substitutes positional columns with assumed MPa/count units. | no detection check exists; issue pending (2). |
| S118: `fatigue_damage.py:310-313` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L305-L318] | Missing stress/cycle statistics become zero including damage-per-cycle. | no detection check exists; issue pending (2). |
| S119: `fatigue_damage.py:351-352` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L346-L357] | External condition identifier raw int conversion; missing occurrence defaults to equal condition share. | no detection check exists; issue pending (2). |
| S120: `fatigue_damage.py:365-380` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L360-L385] | No matching stress files omit requested condition/strut; multiple matches silently take first file. | no detection check exists; issue pending (2). |
| S121: `fatigue_damage.py:467-477` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L462-L482] | No aggregated configurations leaves all_pass true and report overall PASS. | no detection check exists; issue pending (3). |
| S122: `fatigue_damage.py:529-549` [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L524-L553] | Batch CLI writes sample report instead of computing supplied stress-directory inputs. | no detection check exists; issue pending (3). |
| S123: `rainflow_counter.py:95-96` [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L90-L101] | Empty history returns empty reversal arrays. | C7 documents empty behavior; no detection check exists for downstream completeness; issue pending (2). |
| S124: `rainflow_counter.py:123-125` [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L118-L130] | Fixed absolute duplicate tolerance assumes load units and removes near-duplicate reversals. | no detection check exists; issue pending (2). |
| S125: `rainflow_counter.py:210-211` [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L205-L216] | Exactly zero residual range is skipped. | no detection check exists for explicit duplicate residual; issue pending (2). |
| S126: `rainflow_counter.py:232-233` [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L227-L238] | No cycles returns empty range/count arrays. | C7 documents empty behavior; no detection check exists for downstream completeness; issue pending (2). |
| S127: `rainflow_counter.py:240-241` [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L235-L246] | Cycle aggregation rounds ranges to fixed decimal precision with assumed load units. | no detection check exists; issue pending (2). |
| S128: `rainflow_counter.py:261-272` [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L256-L277] | No cycles yields zero-valued statistics rather than unavailable status. | C7 documents empty behavior; no detection check exists for downstream completeness; issue pending (2). |
| S129: `rainflow_counting.py:43-55` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L38-L60] | Default zero gate disables filtering; subunit gates use relative amplitude while other counter uses absolute units. | no detection check exists; issue pending (4). |
| S130: `rainflow_counting.py:137-138` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L132-L143] | Fewer than two turning points becomes empty arrays. | no detection check exists; issue pending (4). |
| S131: `rainflow_counting.py:195-196` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L190-L201] | No extracted cycles becomes empty arrays. | no detection check exists; issue pending (4). |
| S132: `rainflow_counting.py:222-229` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L217-L234] | No cycles returns empty arrays and empty statistics. | no detection check exists; issue pending (4). |
| S133: `rainflow_counting.py:232-233` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L227-L238] | Zero maximum fallback is unreachable after early return; computed filtered ranges are never used in returned result. | no detection check exists; issue pending (4). |
| S134: `rainflow_counting.py:269-270` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L264-L275] | Empty ranges returns empty histogram arrays. | no detection check exists; issue pending (4). |
| S135: `rainflow_counting.py:315-322` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L310-L327] | CSV with no named load/tension silently takes second column with assumed load units. | no detection check exists; issue pending (4). |
| S136: `rainflow_counting.py:428-431` [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L423-L436] | Absent statistics are serialized as zeros. | no detection check exists; issue pending (4). |
| S137: `parallel_processor.py:142-144` [src/digitalmodel/structural/fatigue_apps/parallel_processor.py:L137-L149] | Any task exception becomes None plus duration. | no detection check exists; issue pending (3). |
| S138: `parallel_processor.py:194-196` [src/digitalmodel/structural/fatigue_apps/parallel_processor.py:L189-L201] | Future exception excludes task from result list and records failed task separately. | no detection check exists; issue pending (3). |
| S139: `parallel_processor.py:298-299` [src/digitalmodel/structural/fatigue_apps/parallel_processor.py:L293-L304] | No successful task times returns empty performance dict. | no detection check exists; issue pending (3). |
| S140: `parallel_processor.py:329-330` [src/digitalmodel/structural/fatigue_apps/parallel_processor.py:L324-L335] | No timing samples returns zero efficiency. | no detection check exists; issue pending (3). |
| S141: `parallel_processor.py:388-390` [src/digitalmodel/structural/fatigue_apps/parallel_processor.py:L383-L395] | Absent resource monitor returns fabricated moderate system load. | no detection check exists; issue pending (3). |
| S142: `cli_load_scaling.py:114-116` [src/digitalmodel/structural/fatigue_apps/cli_load_scaling.py:L109-L121] | Any configuration error becomes error dict instead of propagated failure. | no detection check exists; issue pending (3). |
| S143: `cli_load_scaling.py:146-148` [src/digitalmodel/structural/fatigue_apps/cli_load_scaling.py:L141-L153] | Any future error becomes error dict instead of propagated failure. | no detection check exists; issue pending (3). |
| S144: `cli_load_scaling.py:178-180` [src/digitalmodel/structural/fatigue_apps/cli_load_scaling.py:L173-L185] | Missing occurrence column becomes zero and invalid total only warns; validation proceeds. | no detection check exists; issue pending (3). |
| S145: `cli_load_scaling.py:182-184` [src/digitalmodel/structural/fatigue_apps/cli_load_scaling.py:L177-L189] | Validation exception is converted to False (explicit failure, retained in sweep). | no detection check exists; issue pending (3). |
| S146: `cli_load_scaling.py:235-248` [src/digitalmodel/structural/fatigue_apps/cli_load_scaling.py:L230-L253] | Single/batch error results still end with completion text and success exit code. | no detection check exists; issue pending (3). |
| S147: `cli_load_scaling.py:270-272` [src/digitalmodel/structural/fatigue_apps/cli_load_scaling.py:L265-L277] | Missing test-mode metrics default to zero, including error result dictionaries. | no detection check exists; issue pending (3). |
| S148: `__main__.py:230-235` [src/digitalmodel/structural/fatigue_apps/__main__.py:L225-L240] | Raw external strut integer text conversion catches ValueError and appends explicit validation failure; no locale support. | no detection check exists; issue pending (3). |
| S149: `__main__.py:304-311` [src/digitalmodel/structural/fatigue_apps/__main__.py:L299-L316] | External CSV int/float lacks locale normalization; missing columns receive physical defaults including zero headings. | no detection check exists; issue pending (3). |
| S150: `__main__.py:449-451` [src/digitalmodel/structural/fatigue_apps/__main__.py:L444-L456] | Absent summary distributions become zero extrema and mean. | no detection check exists; issue pending (3). |
| S151: `__main__.py:499-501` [src/digitalmodel/structural/fatigue_apps/__main__.py:L494-L506] | All analysis exceptions become False with logged failure (explicit failure, retained in sweep). | no detection check exists; issue pending (3). |
| S152: `visualizer.py:139-140` [src/digitalmodel/structural/fatigue_apps/visualizer.py:L134-L145] | Empty damage input returns blank figure without populated data. | no detection check exists; issue pending (3). |
| S153: `visualizer.py:244-244` [src/digitalmodel/structural/fatigue_apps/visualizer.py:L239-L249] | Missing strut life is replaced by infinity in heatmap. | no detection check exists; issue pending (3). |
| S154: `visualizer.py:281-281` [src/digitalmodel/structural/fatigue_apps/visualizer.py:L276-L286] | Missing minimum fatigue life sorts as infinity when choosing critical configuration. | no detection check exists; issue pending (3). |
| S155: `visualizer.py:414-416` [src/digitalmodel/structural/fatigue_apps/visualizer.py:L409-L421] | Input read errors return None from dashboard creation. | no detection check exists; issue pending (3). |
| S156: `visualizer.py:569-570` [src/digitalmodel/structural/fatigue_apps/visualizer.py:L564-L575] | Any visualization-generation error is logged and swallowed by wrapper. | no detection check exists; issue pending (3). |
| S157: `tension_stress_conversion.py:74-97` [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L69-L102] | Missing or unrecognized lookup columns substitute synthetic linear table. | no detection check exists; issue pending (2). |
| S158: `tension_stress_conversion.py:113-114` [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L108-L119] | Synthetic fallback is written into requested lookup path as though supplied data. | no detection check exists; issue pending (2). |
| S159: `tension_stress_conversion.py:119-135` [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L114-L140] | Missing or unrecognized SCF data defaults to empty map and unity factor. | no detection check exists; issue pending (2). |
| S160: `tension_stress_conversion.py:139-156` [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L134-L161] | Insufficient lookup rows silently substitute linear conversion; extrapolation permits values outside reference support. | no detection check exists; issue pending (2). |
| S161: `tension_stress_conversion.py:175-178` [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L170-L183] | Unknown requested SCF location silently gets unity correction. | no detection check exists; issue pending (2). |
| S162: `tension_stress_conversion.py:206-214` [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L201-L219] | Empty cycle CSV returns empty ranges, counts and statistics. | no detection check exists; issue pending (2). |
| S163: `tension_stress_conversion.py:225-229` [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L220-L234] | Missing extrema and cycle statistics default to zero. | no detection check exists; issue pending (2). |
| S164: `tension_stress_conversion.py:266-268` [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L261-L273] | Missing exported statistics default to zero. | no detection check exists; issue pending (3). |
| S165: `tension_stress_conversion.py:345-347` [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L340-L352] | Missing plot dependency logs and returns without artifact. | no detection check exists; issue pending (2). |
| S166: `load_scaling_efficient_v2.py:81-82` [src/digitalmodel/structural/fatigue_apps/load_scaling_efficient_v2.py:L76-L87] | Rounded metadata cache key can merge distinct scaling factors differing below stored decimal precision. | no detection check exists; issue pending (3). |
| S167: `load_scaling_efficient_v2.py:110-118` [src/digitalmodel/structural/fatigue_apps/load_scaling_efficient_v2.py:L105-L123] | Empty tension/time inputs can be written as header-only CSV with metadata and mapping; no readiness guard. | no detection check exists; issue pending (3). |
| S168: `load_scaling_efficient_v2.py:139-141` [src/digitalmodel/structural/fatigue_apps/load_scaling_efficient_v2.py:L134-L146] | Empty metadata cache finalization returns without required lookup outputs. | no detection check exists; issue pending (3). |
| S169: `load_scaling_efficient_v2.py:195-195` [src/digitalmodel/structural/fatigue_apps/load_scaling_efficient_v2.py:L190-L200] | Missing metadata cache reports zero deduplication ratio. | no detection check exists; issue pending (3). |
| S170: `examples/load_scaling_runner.py:94-102` [src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py:L89-L107] | Missing source time series silently replaced with stochastic synthetic data. | no detection check exists; issue pending (2). |
| S171: `examples/load_scaling_runner.py:105-108` [src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py:L100-L113] | CSV selects first time and second tension column with assumed engineering units. | no detection check exists; issue pending (2). |
| S172: `examples/load_scaling_runner.py:51-60` [src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py:L46-L65] | Fixed reference physical conditions and built-in configuration weights replace explicit basis in example workflow. | no detection check exists; issue pending (2). |
| S173: `validate_load_scaling_interactive.py:342-357` [src/digitalmodel/structural/fatigue_apps/validate_load_scaling_interactive.py:L337-L362] | Missing sample input creates synthetic history; even present sample uses synthetic wave history and positional columns. | no detection check exists; issue pending (2). |
| S174: `validate_load_scaling_interactive.py:544-599` [src/digitalmodel/structural/fatigue_apps/validate_load_scaling_interactive.py:L539-L604] | Complete-chain validation prints fixed example loads and success checks without executing full chain. | no detection check exists; issue pending (2). |
| S175: `fatigue_validation/models.py:108-109` [src/digitalmodel/structural/fatigue_apps/fatigue_validation/models.py:L103-L114] | Default zero validation duration is paired with UNKNOWN status; not a completed measurement. | no detection check exists; issue pending (3). |
| S176: `fatigue_validation/models.py:81-83` [src/digitalmodel/structural/fatigue_apps/fatigue_validation/models.py:L76-L88] | Per-curve validity flags default True before any validation is performed. | no detection check exists; issue pending (3). |
| S177: `file_namer.py:56-56` [src/digitalmodel/structural/fatigue_apps/file_namer.py:L51-L61] | Scale factor is truncated to integer hundredths in filename; distinct scaling factors can collide. | no detection check exists; issue pending (2). |
| S178: `integrated_processor_with_naming.py:399-399` [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L394-L404] | Complete analysis with no successful records returns empty DataFrame and dict. | no detection check exists; issue pending (2). |
| S179: `fatigue_damage_calculator.py:376-383` [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L371-L388] | Demonstration supplies fixed occurrence weight and sample duration; these are demonstration assumptions, not production defaults. | no detection check exists; issue pending (2). |
| S180: `load_scaling.py:329-336` [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L324-L341] | Loads combine by array index and regenerated time vector rather than checked input timestamps. | no detection check exists; issue pending (2). |
| S181: `visualizer.py:269-271` [src/digitalmodel/structural/fatigue_apps/visualizer.py:L264-L276] | Missing overall life, condition count and strut count become zero in the plotted summary, concealing absent evidence. | no detection check exists; issue pending (3). |
| S182: `visualizer.py:465-466` [src/digitalmodel/structural/fatigue_apps/visualizer.py:L460-L471] | Missing overall life and result count become zero in the interactive summary, concealing absent evidence. | no detection check exists; issue pending (3). |

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log entry |
|---|---|---|---|---|---|
| 2026-10-17 | Initial pack revision | Distinguish exported counting from the guarded diagnostic implementation. | [src/digitalmodel/structural/fatigue_apps/__init__.py:L26-L29] [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L343-L361] | Separate method rows and detection gaps. | Not assigned; issue pending (4). |
| 2026-10-17 | Initial pack revision | Empty/missing histories are not evidence of zero physical damage. | [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L377-L402] [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L457-L475] | Explicit sweep and completeness limitation. | Not assigned; issue pending (2). |

## 8. Ownership

The engineering calculations quality manager will assess the pack; the deputy engineering calculations quality manager will provide cover. The producing role will maintain code evidence and test records. Ambiguous standards clauses, changed tolerances and missing design inputs will be referred to the family quality manager; publication and merge will remain with the maintainer. These are proposed responsibilities for this draft, not completed approvals (AQG r0, section 11, *Sign-off and independence*).

## 9. Dependencies and implementation inventory

The reference processor consumes NumPy/Pandas arrays and CSV data and calls its local rainflow counter and damage calculator; the public API also eagerly imports the visualizer, making plotting dependencies relevant to package import. [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L17-L28] [src/digitalmodel/structural/fatigue_apps/__init__.py:L26-L39] [src/digitalmodel/structural/fatigue_apps/visualizer.py:L17-L26]

The separate stress converter uses SciPy interpolation, while the standalone load-scaling route reads YAML and CSV. [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L23-L31] [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L140-L147] [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L71-L94]

UNVERIFIED: inventory-row identifiers for upstream/downstream data sets and qualified physical reference histories are not established; sidecar entry: to be created before restricted defaults or calibration data are reused (issue pending (2)).

### Counted-family search

The map data revision is pinned separately from the code pin. Families are selected by at least one row inside the package, ranked by in-package row count with map-order tie breaking; both discovered families are counted. The inventory below preserves every map implementation row, including helper, test and vocabulary hits; parenthesized symbol line numbers are stored map coordinates, not refreshed code-pin coordinates; an inventory entry is not an assertion that its code is independently qualified.

| Counted family | In-package map rows | Complete family rows | Matched enclosing definitions | Remaining further implementations after section 3 disclosure |
|---|---|---|---|---|
| sn-curves-and-miner-damage [docs/registry/implementation-map.yaml:L16807-L16813] | 29 | 392 | 80: 28 listed, 26 containers/descriptive text, 21 helpers, 5 computing definitions disclosed in section 3 | 0 |
| effective-tension-riser-stack-up [docs/registry/implementation-map.yaml:L2139-L2145] | 9 | 264 | 17: 9 listed, 2 descriptive/module containers, 5 helpers, 1 computing definition disclosed in section 3 | 0 |

The search uses each family's exact `search_signals` as case-sensitive Python `re` against every package `.py` file at the code pin, assigning each matching line to the innermost enclosing function or class, with module statements grouped as one definition. No signal matches `structural/fatigue_apps` case-insensitively, so no signal is excluded. The A8 further-implementation sum is 0, meeting the maximum of 1; additions are disclosed rather than silently omitted.

The six additional computing definitions and their source ranges are retained here as map follow-up candidates, even though section 3 already lists them.


| Family | Further definition disclosed | Classification | Evidence |
|---|---|---|---|
| sn-curves-and-miner-damage | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` — `FatigueDamageCalculator.calculate_fatigue_life` | Annualises delegated damage, weights occurrence and computes reciprocal life. | [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L186-L231] |
| sn-curves-and-miner-damage | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` — `FatigueDamageCalculator.mean_stress_correction` | Computes Goodman/Gerber/Soderberg corrected stresses; related preprocessing, not direct S-N damage result. | [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L233-L268] |
| sn-curves-and-miner-damage | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` — `calculate_combined_damage` | Computes weighted annual damage and reciprocal life. | [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L323-L361] |
| sn-curves-and-miner-damage | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` — `RainflowCounter.rainflow_counting_astm` | Independent stack/range algorithm and cycle aggregation. | [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L127-L196] |
| sn-curves-and-miner-damage | `src/digitalmodel/structural/fatigue_apps/visualizer.py` — `FatigueVisualizer.plot_sn_curve` | Recomputes single/bilinear S-N ordinates for plot rather than calling listed curve evaluator. | [src/digitalmodel/structural/fatigue_apps/visualizer.py:L42-L115] |
| effective-tension-riser-stack-up | `src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py` — `LoadScalingProcessor.process_fatigue_condition` | Computes scaled wind/wave superposition of tension time histories. | [src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py:L143-L184] |

### Complete map inventory

#### sn-curves-and-miner-damage: 392 map rows

| Path | Symbols (map rows preserved) | Map citation |
|---|---|---|
| `docs/domains/examples/orcaflex_signal_analysis_example.py` | `example_6_custom_workflow` (L295) | [docs/registry/implementation-map.yaml:L16814-L16823] |
| `docs/domains/examples/signal_analysis_usage_example.py` | `example_1_basic_rainflow_counting` (L29); `example_4_fatigue_analysis` (L175); `example_6_backward_compatibility` (L315); `main` (L380) | [docs/registry/implementation-map.yaml:L16824-L16867] |
| `examples/demos/demo_sn_library.py` | `main` (L43) | [docs/registry/implementation-map.yaml:L16868-L16879] |
| `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_sn_curves` (L120); `demonstrate_rainflow_counting` (L163); `demonstrate_damage_accumulation` (L246); `demonstrate_frequency_domain_analysis` (L329); `demonstrate_quick_analysis` (L411); `main` (L461) | [docs/registry/implementation-map.yaml:L16880-L16945] |
| `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `list_curves` (L24); `compare_curves` (L66) | [docs/registry/implementation-map.yaml:L16946-L16965] |
| `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_2_linear_log` (L46); `example_3_with_scf` (L67); `example_4_without_fatigue_limit` (L88); `example_5_dnv_comparison` (L109); `example_6_abs_comparison` (L136); `example_7_reference_comparison` (L177); `example_8_joint_type_comparison` (L200) | [docs/registry/implementation-map.yaml:L16966-L17035] |
| `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_1_basic_sn_curves` (L55); `example_2_multislope_curves` (L98); `example_3_time_domain_analysis` (L142); `example_4_frequency_domain_analysis` (L215); `example_5_high_stress_range_analysis` (L282); `example_6_shear_data_analysis` (L327); `run_all_examples` (L454) | [docs/registry/implementation-map.yaml:L17036-L17112] |
| `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_well_log_analysis` (L138); `example_stratigraphic_correlation` (L376); `main` (L578) | [docs/registry/implementation-map.yaml:L17113-L17145] |
| `scripts/capabilities/build_onepagers.py` | `<module>` (L36) | [docs/registry/implementation-map.yaml:L17146-L17154] |
| `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `<module>` (L3) | [docs/registry/implementation-map.yaml:L17155-L17163] |
| `scripts/python/digitalmodel/tools/check_process_fatigue_single.py` | `process_single_test_file` (L25) | [docs/registry/implementation-map.yaml:L17164-L17173] |
| `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_analysis` (L134); `YAMLConfigMigrator._migrate_time_series_components` (L225) | [docs/registry/implementation-map.yaml:L17174-L17193] |
| `scripts/python/digitalmodel/tools/process_fatigue_data.py` | `process_fatigue_files` (L24) | [docs/registry/implementation-map.yaml:L17194-L17203] |
| `scripts/python/digitalmodel/tools/process_fatigue_final.py` | `process_single_test_file` (L25) | [docs/registry/implementation-map.yaml:L17204-L17213] |
| `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_fatigue_file` (L23); `process_sample_file` (L275); `batch_process_all_files` (L302) | [docs/registry/implementation-map.yaml:L17214-L17246] |
| `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `rainflow_counting` (L19); `process_fatigue_file` (L61); `batch_process` (L275) | [docs/registry/implementation-map.yaml:L17247-L17280] |
| `scripts/python/digitalmodel/tools/process_fatigue_single_test_v2.py` | `process_single_test_file` (L25) | [docs/registry/implementation-map.yaml:L17281-L17290] |
| `scripts/python/digitalmodel/tools/process_fatigue_single_test_v3.py` | `process_single_test_file` (L26) | [docs/registry/implementation-map.yaml:L17291-L17300] |
| `scripts/python/digitalmodel/tools/process_fatigue_single_test_v4.py` | `process_single_test_file` (L25) | [docs/registry/implementation-map.yaml:L17301-L17310] |
| `scripts/python/digitalmodel/tools/process_fatigue_single_test_v5.py` | `process_single_test_file` (L25) | [docs/registry/implementation-map.yaml:L17311-L17320] |
| `scripts/python/digitalmodel/tools/process_fatigue_single_test_v6.py` | `process_single_test_file` (L25) | [docs/registry/implementation-map.yaml:L17321-L17330] |
| `scripts/riser_database/build_tables.py` | `<module>` (L24) | [docs/registry/implementation-map.yaml:L17331-L17339] |
| `src/digitalmodel/ansys/fatigue_postprocessor.py` | `SNcurve.from_dnv` (L100); `FatiguePostprocessor.calculate_cycles_to_failure` (L158); `FatiguePostprocessor.calculate_fatigue_damage` (L222); `FatiguePostprocessor.generate_sn_curve_apdl` (L347); `FatiguePostprocessor.create_damage_summary_df` (L371) | [docs/registry/implementation-map.yaml:L17340-L17391] |
| `src/digitalmodel/benchmarks/inventory.py` | `build_model_inventory` (L77) | [docs/registry/implementation-map.yaml:L17392-L17401] |
| `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `half_cycles` (L51); `extract` (L104) | [docs/registry/implementation-map.yaml:L17402-L17423] |
| `src/digitalmodel/engine.py` | `engine` (L90) | [docs/registry/implementation-map.yaml:L17424-L17441] |
| `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `router` (L52); `_sn_curve` (L120) | [docs/registry/implementation-map.yaml:L17442-L17464] |
| `src/digitalmodel/fatigue/assessment.py` | `_case_histogram` (L116); `assess_fatigue` (L136) | [docs/registry/implementation-map.yaml:L17465-L17488] |
| `src/digitalmodel/fatigue/counting_contract.py` | `<module>` (L47); `_adapt_pylife_4pt` (L432); `_adapt_pypi_rainflow` (L443); `_adapt_sigproc_astm` (L457); `_adapt_calm_buoy` (L472); `_adapt_fapps_counting` (L481); `_adapt_fapps_counter` (L490); `_adapt_struct_fatigue` (L497); `_is_counting_name` (L866) | [docs/registry/implementation-map.yaml:L17489-L17577] |
| `src/digitalmodel/fatigue/damage.py` | `miner_damage` (L13); `design_life_check` (L42) | [docs/registry/implementation-map.yaml:L17578-L17599] |
| `src/digitalmodel/fatigue/environmental_correction.py` | `environment_correction` (L165); `apply_environment_to_sn` (L251) | [docs/registry/implementation-map.yaml:L17600-L17621] |
| `src/digitalmodel/fatigue/fatg.py` | `fatg_sea_state_damage` (L211); `fatg_annual_damage` (L241) | [docs/registry/implementation-map.yaml:L17622-L17643] |
| `src/digitalmodel/fatigue/fatigue_reporting.py` | `_sec_fatigue_locations` (L376) | [docs/registry/implementation-map.yaml:L17644-L17653] |
| `src/digitalmodel/fatigue/rainflow.py` | `fatigue_life` (L130) | [docs/registry/implementation-map.yaml:L17654-L17666] |
| `src/digitalmodel/fatigue/sn_curves.py` | `_compute_sd` (L56); `_build_curve_dict` (L61); `get_sn_curve` (L80) | [docs/registry/implementation-map.yaml:L17667-L17699] |
| `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.knee_stress` (L140); `SNCurveRecord.switch_stress` (L148); `SNCurveRecord.cycles` (L162); `_knee_stress` (L180); `_switch_stress` (L200); `_bilinear_cycles` (L225); `_sd` (L288); `_build_dnv_curves` (L293); `_build_api_curves` (L384); `_build_bs7608_curves` (L416); `_build_iiw_curves` (L466); `_build_eurocode3_curves` (L500); `_build_norsok_curves` (L529); `_build_aws_curves` (L569); `_build_asme_curves` (L600); `_build_en13445_curves` (L630); `_build_pd5500_curves` (L657); `_build_abs_curves` (L690); `_build_iso19902_curves` (L733); `_build_csa_curves` (L805); `_build_hse_curves` (L835); `_build_doe_curves` (L880); `_build_gl_curves` (L914); `_build_jis_curves` (L944); `_ensure_loaded` (L1002); `summary_table` (L1129) | [docs/registry/implementation-map.yaml:L17700-L17978] |
| `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_curve_info` (L146); `_record_to_sncurve` (L158); `get_curve` (L210); `calculate_endurance` (L238) | [docs/registry/implementation-map.yaml:L17979-L18023] |
| `src/digitalmodel/fatigue/spectral_fatigue.py` | `wirsching_light_damage` (L207); `benasciutti_tovo_damage` (L339) | [docs/registry/implementation-map.yaml:L18024-L18043] |
| `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `weld_fatigue_quickcheck` (L105); `main` (L340) | [docs/registry/implementation-map.yaml:L18044-L18064] |
| `src/digitalmodel/fatigue/woehler_fitting.py` | `generate_test_data` (L18); `_fallback_log_regression` (L137) | [docs/registry/implementation-map.yaml:L18065-L18084] |
| `src/digitalmodel/fatigue/workflow.py` | `router` (L16); `_curve_ids` (L43); `_stress_ranges` (L58); `_curve_summary` (L99); `_curve_csv_path` (L114); `_input_stem` (L131) | [docs/registry/implementation-map.yaml:L18085-L18154] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.router` (L23); `FatigueAnalysis.damage_from_sn_data` (L52); `FatigueAnalysis.damage_from_rainflow_cycles` (L74); `FatigueAnalysis.damage_from_timetrace` (L95); `FatigueAnalysis.get_rainflow_from_timetrace` (L249) | [docs/registry/implementation-map.yaml:L18155-L18210] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` (L586) | [docs/registry/implementation-map.yaml:L18211-L18220] |
| `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.router` (L22); `ShipFatigueAnalysis.get_lcf_timetrace_rainflow_and_fatigue_damage` (L113); `ShipFatigueAnalysis.get_fatigue_damage_output_df_for_timetrace` (L298); `ShipFatigueAnalysis.get_fatigue_df_definition_for_timetrace` (L476) | [docs/registry/implementation-map.yaml:L18221-L18266] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` (L982) | [docs/registry/implementation-map.yaml:L18267-L18276] |
| `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `SNcurve.cycles_to_failure` (L53); `RainflowFatigue.count_cycles` (L203); `MinersRuleDamage.compute` (L323); `ScatterDiagramFatigue.compute` (L373); `compute_fatigue_life` (L522) | [docs/registry/implementation-map.yaml:L18277-L18326] |
| `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.add_well_analysis` (L414); `StratigraphicAnalysis.set_well_order` (L425); `StratigraphicAnalysis.create_flattened_section` (L451); `WellCorrelation.add_well_data` (L803); `CoreAnalysis.correlate_core_to_logs` (L1046); `ReservoirAnalysis.add_well_analysis` (L1119); `ReservoirAnalysis.run_comprehensive_analysis` (L1144) | [docs/registry/implementation-map.yaml:L18327-L18396] |
| `src/digitalmodel/mooring_fatigue/workflow.py` | `router` (L23); `_evaluate_line` (L61); `_history_bins` (L137); `_curve_settings` (L184) | [docs/registry/implementation-map.yaml:L18397-L18441] |
| `src/digitalmodel/parametric/generate.py` | `_mooring_fatigue_damage` (L28); `_free_span_utilisation` (L141); `_span_rectification_utilisation` (L327) | [docs/registry/implementation-map.yaml:L18442-L18472] |
| `src/digitalmodel/parametric/refresh.py` | `<module>` (L27) | [docs/registry/implementation-map.yaml:L18473-L18481] |
| `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `router` (L34) | [docs/registry/implementation-map.yaml:L18482-L18493] |
| `src/digitalmodel/riser_fatigue/touchdown.py` | `assess_touchdown_fatigue` (L157) | [docs/registry/implementation-map.yaml:L18494-L18503] |
| `src/digitalmodel/riser_fatigue/workflow.py` | `router` (L65); `_viv_damage` (L205); `_curve_settings` (L255) | [docs/registry/implementation-map.yaml:L18504-L18536] |
| `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series` (L50); `FatigueAnalysisAdapter.get_rainflow_from_timetrace` (L293); `FatigueAnalysisAdapter.damage_from_rainflow_cycles` (L317); `OrcaFlexAdapter.RainflowHalfCycles` (L376) | [docs/registry/implementation-map.yaml:L18537-L18576] |
| `src/digitalmodel/signal_processing/signal_analysis/cli.py` | `rainflow_cmd` (L24) | [docs/registry/implementation-map.yaml:L18577-L18586] |
| `src/digitalmodel/signal_processing/signal_analysis/core/rainflow.py` | `RainflowCounter.count_cycles` (L39) | [docs/registry/implementation-map.yaml:L18587-L18596] |
| `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve._bilinear` (L189); `SNCurve.from_test_data` (L375) | [docs/registry/implementation-map.yaml:L18597-L18616] |
| `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_damage` (L34); `FatigueDamageCalculator._miners_rule` (L86); `FatigueDamageCalculator._modified_miners_rule` (L118); `FatigueDamageCalculator._nonlinear_damage` (L160); `FatigueDamageCalculator._calculate_allowable_cycles` (L201); `FatigueDamageCalculator._apply_mean_stress_correction` (L257); `FatigueDamageCalculator._estimate_cycles_to_failure` (L316); `FatigueDamageCalculator.calculate_equivalent_stress` (L353) | [docs/registry/implementation-map.yaml:L18617-L18706] |
| `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `create_parser` (L55); `build_config_from_args` (L251); `main` (L327) | [docs/registry/implementation-map.yaml:L18707-L18739] |
| `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._analyze_column` (L147); `TimeSeriesAnalyzer._generate_outputs` (L327); `TimeSeriesAnalyzer._generate_summary` (L438) | [docs/registry/implementation-map.yaml:L18740-L18769] |
| `src/digitalmodel/signal_processing/signal_analysis/orcaflex/error_handling.py` | `SafeAnalyzer.safe_rainflow_analysis` (L162) | [docs/registry/implementation-map.yaml:L18770-L18779] |
| `src/digitalmodel/signal_processing/time_series/time_series_analysis.py` | `TimeSeriesAnalysis.router` (L20) | [docs/registry/implementation-map.yaml:L18780-L18789] |
| `src/digitalmodel/signal_processing/time_series/time_series_components.py` | `TimeSeriesComponents.count_cycles` (L644) | [docs/registry/implementation-map.yaml:L18790-L18799] |
| `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_histogram_object_from_orcaflex_run` (L441); `OPPTimeSeries.get_detailed_histogram_df_columns` (L473) | [docs/registry/implementation-map.yaml:L18800-L18819] |
| `src/digitalmodel/solvers/orcaflex/opp_time_series_v2.py` | `OPPTimeSeriesV2.process_histograms` (L241) | [docs/registry/implementation-map.yaml:L18820-L18829] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_histogram_object_from_orcaflex_run` (L470); `OrcaFlexAnalysis.get_detailed_histogram_df_columns` (L755) | [docs/registry/implementation-map.yaml:L18830-L18849] |
| `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` (L248); `OrcaFlexTimeTraceProcessor._parallel_fatigue_analysis` (L420); `OrcaFlexTimeTraceProcessor._analyze_single_trace` (L450) | [docs/registry/implementation-map.yaml:L18850-L18881] |
| `src/digitalmodel/spectral_fatigue/workflow.py` | `router` (L37); `_sn_curve` (L139) | [docs/registry/implementation-map.yaml:L18882-L18903] |
| `src/digitalmodel/structural/fatigue/__init__.py` | `quick_fatigue_analysis` (L226); `get_module_info` (L431) | [docs/registry/implementation-map.yaml:L18904-L18927] |
| `src/digitalmodel/structural/fatigue/analysis.py` | `<module>` (L39); `FatigueAnalysisEngine._setup_sn_curve` (L266); `FatigueAnalysisEngine._setup_damage_calculator` (L287); `FatigueAnalysisEngine._setup_rainflow_counter` (L302); `FatigueAnalysisEngine.analyze_time_series` (L312); `FatigueAnalysisEngine.analyze_psd` (L421); `FatigueAnalysisEngine.calculate_high_stress_range_life` (L528); `FatigueAnalysisEngine.analyze_shear_data` (L571); `FatigueAnalysisEngine._perform_engineering_validation` (L733); `quick_time_domain_analysis` (L979); `quick_frequency_domain_analysis` (L1025) | [docs/registry/implementation-map.yaml:L18928-L19039] |
| `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `<module>` (L32); `LinearDamageAccumulation.calculate_damage` (L109); `LinearDamageAccumulation._apply_mean_stress_correction` (L219); `ModifiedMinersRule.calculate_damage` (L292); `NonlinearDamageAccumulation.calculate_damage` (L383); `NonlinearDamageAccumulation._marco_starkey_damage` (L402); `NonlinearDamageAccumulation._corten_dolan_damage` (L451); `NonlinearDamageAccumulation._double_linear_damage` (L497); `compare_damage_methods` (L652) | [docs/registry/implementation-map.yaml:L19040-L19134] |
| `src/digitalmodel/structural/fatigue/design_code_report.py` | `_build_curve` (L101) | [docs/registry/implementation-map.yaml:L19135-L19144] |
| `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `failure_probability` (L84); `sensitivity_analysis` (L171); `monte_carlo_fatigue` (L276) | [docs/registry/implementation-map.yaml:L19145-L19174] |
| `src/digitalmodel/structural/fatigue/frequency_domain.py` | `<module>` (L30); `NarrowBandMethod.calculate_damage_rate` (L191); `DirlikMethod.calculate_damage_rate` (L240); `TovoBenasciuttiMethod.calculate_damage_rate` (L382); `SingleMomentMethod.calculate_damage_rate` (L481); `compare_frequency_methods` (L683) | [docs/registry/implementation-map.yaml:L19175-L19233] |
| `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `run_sweep` (L74); `compute_sensitivity._single_damage` (L206); `_build_scatter_json` (L448) | [docs/registry/implementation-map.yaml:L19234-L19263] |
| `src/digitalmodel/structural/fatigue/rainflow.py` | `<module>` (L32); `RainflowCounter.count_cycles` (L230); `RainflowCounter._rainflow_count` (L316); `RainflowCounter.extract_cycles_with_means` (L502); `rainflow_count` (L729); `rainflow_with_means` (L755) | [docs/registry/implementation-map.yaml:L19264-L19326] |
| `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `scatter_fatigue_damage` (L102) | [docs/registry/implementation-map.yaml:L19327-L19336] |
| `src/digitalmodel/structural/fatigue/skill.py` | `fatigue_assessment` (L188) | [docs/registry/implementation-map.yaml:L19337-L19348] |
| `src/digitalmodel/structural/fatigue/sn_curve_plotter.py` | `main` (L397) | [docs/registry/implementation-map.yaml:L19349-L19358] |
| `src/digitalmodel/structural/fatigue/sn_curves.py` | `<module>` (L32); `StandardSNCurves._ensure_loaded` (L455); `StandardSNCurves.register_custom_curve` (L521); `StandardSNCurves.get_dnv_c203_curve` (L565); `StandardSNCurves.get_curve` (L618); `StandardSNCurves.get_multislope_curve` (L670); `ThicknessCorrection.apply_thickness_effect` (L862); `SNDataFitting.fit_power_law` (L926); `get_dnv_curve` (L1047); `get_api_curve` (L1069); `get_bs_curve` (L1074); `_dnv_curves_from_c203` (L1085); `PowerLawSNCurve.get_allowable_cycles` (L152); `PowerLawSNCurve.get_stress_range` (L186); `BilinearSNCurve.get_allowable_cycles` (L272); `BilinearSNCurve.get_stress_range` (L290) | [docs/registry/implementation-map.yaml:L19359-L20928] |
| `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_cp` (L58); `_dnv_seawater_free` (L80); `_chain_seawater_cp` (L100); `pipeline_girth_weld` (L199); `scr_touchdown` (L263); `mooring_chain` (L326); `_compute_example` (L395) | [docs/registry/implementation-map.yaml:L19478-L19556] |
| `src/digitalmodel/structural/fatigue_apps/__main__.py` | `run_analysis` (L244) | [docs/registry/implementation-map.yaml:L19557-L19568] |
| `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve._load_sn_parameters` (L55); `SNCurve.calculate_cycles_to_failure` (L120); `FatigueDamageCalculator.calculate_damage` (L182); `main` (L489) | [docs/registry/implementation-map.yaml:L19569-L19608] |
| `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `<module>` (L21); `SNCurveParameters.a1` (L35); `SNCurveParameters.a2` (L40); `FatigueDamageCalculator.cycles_to_failure` (L106); `FatigueDamageCalculator.calculate_damage` (L134); `FatigueDamageCalculator.export_results` (L297) | [docs/registry/implementation-map.yaml:L19609-L19669] |
| `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `FatigueFileNamer.get_rainflow_filename` (L87); `<module>` (L201) | [docs/registry/implementation-map.yaml:L19670-L19688] |
| `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` (L290); `main` (L366) | [docs/registry/implementation-map.yaml:L19689-L19708] |
| `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` (L81) | [docs/registry/implementation-map.yaml:L19709-L19718] |
| `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `<module>` (L20); `RainflowCounter.count_cycles` (L48); `RainflowCounter._rainflow_algorithm` (L164); `RainflowCounter.export_results` (L292); `rainflow_count` (L325) | [docs/registry/implementation-map.yaml:L19719-L19769] |
| `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_time_series` (L198); `RainflowCounter.process_batch` (L332); `RainflowCounter.save_cycle_data` (L405); `main` (L463) | [docs/registry/implementation-map.yaml:L19770-L19811] |
| `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` (L355) | [docs/registry/implementation-map.yaml:L19812-L19821] |
| `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `TensionStressConverter.process_batch` (L271); `main` (L428) | [docs/registry/implementation-map.yaml:L19822-L19841] |
| `src/digitalmodel/structural/fatigue_apps/visualizer.py` | `generate_all_visualizations` (L507) | [docs/registry/implementation-map.yaml:L19842-L19851] |
| `src/digitalmodel/structural/spectral_fatigue.py` | `narrow_band_damage` (L122) | [docs/registry/implementation-map.yaml:L19852-L19861] |
| `src/digitalmodel/subsea/connectors/fatigue.py` | `assess_jumper_fatigue` (L62) | [docs/registry/implementation-map.yaml:L19862-L19873] |
| `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `_dnv_params` (L133); `get_sn_curve` (L170); `BilinearSNCurve.get_allowable_cycles` (L104); `BilinearSNCurve.get_stress_range` (L117) | [docs/registry/implementation-map.yaml:L19874-L20948] |
| `src/digitalmodel/subsea/pipeline/free_span/span_fatigue_damage.py` | `SpanFatigueDamage._load_sn_curve` (L79) | [docs/registry/implementation-map.yaml:L19896-L19907] |
| `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_damage` (L108); `VIVFatigueCalculator.calculate_fatigue_life` (L167); `VIVFatigueCalculator.analyze_viv_fatigue` (L209); `VIVFatigueCalculator.cumulative_damage` (L270) | [docs/registry/implementation-map.yaml:L19908-L19952] |
| `src/digitalmodel/viv_screening/inputs.py` | `parse_inputs` (L88) | [docs/registry/implementation-map.yaml:L19953-L19962] |
| `src/digitalmodel/viv_screening/workflow.py` | `router` (L25); `_damage_rate` (L128); `_provenance` (L173) | [docs/registry/implementation-map.yaml:L19963-L19997] |
| `src/digitalmodel/web/digitaltwinfeed/app.py` | `page` (L70) | [docs/registry/implementation-map.yaml:L19998-L20007] |
| `tests/ansys/padeye_pressure_bundle.py` | `_check_receipt` (L23); `verify_pressure_bundle` (L41) | [docs/registry/implementation-map.yaml:L20008-L20029] |
| `tests/ansys/test_fatigue_postprocessor.py` | `TestThicknessCorrection.test_known_correction` (L92) | [docs/registry/implementation-map.yaml:L20030-L20039] |
| `tests/ansys/test_padeye_pressure_bundle.py` | `bundle` (L20) | [docs/registry/implementation-map.yaml:L20040-L20049] |
| `tests/fatigue/test_assessment.py` | `histogram_input` (L22) | [docs/registry/implementation-map.yaml:L20050-L20061] |
| `tests/fatigue/test_c203_consumers.py` | `_span_input` (L303) | [docs/registry/implementation-map.yaml:L20062-L20071] |
| `tests/fatigue/test_c203_sn_values.py` | `_non_dnv_bilinear` (L298); `test_non_dnv_bilinear_switch_stays_at_endurance_limit` (L325) | [docs/registry/implementation-map.yaml:L20072-L20095] |
| `tests/fatigue/test_c203_variable_amplitude.py` | `_signal_e` (L179); `_user_curve` (L230); `_span_input` (L304); `_fresh_standard_curves` (L377); `_data_dir` (L394) | [docs/registry/implementation-map.yaml:L20096-L20145] |
| `tests/fatigue/test_damage.py` | `f_curve` (L15); `TestThicknessCorrection.test_correction_above_ref` (L90); `TestThicknessCorrection.test_correction_formula` (L97) | [docs/registry/implementation-map.yaml:L20146-L20176] |
| `tests/fatigue/test_fatg.py` | `TestAgainstTransliterationSweep.test_all_presets_and_curves` (L265) | [docs/registry/implementation-map.yaml:L20177-L20188] |
| `tests/fatigue/test_fatg_workflow.py` | `_cfg` (L23); `_library_reference` (L52) | [docs/registry/implementation-map.yaml:L20189-L20209] |
| `tests/fatigue/test_fatigue_reporting.py` | `_sample_locations` (L15) | [docs/registry/implementation-map.yaml:L20210-L20219] |
| `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls` (L408); `_damage_calls.calm_scatter` (L443); `_damage_calls.fapps_batch` (L456) | [docs/registry/implementation-map.yaml:L20220-L20252] |
| `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_f_curve_at_100mpa` (L40); `TestGetSNCurve.test_d_curve_at_100mpa` (L52) | [docs/registry/implementation-map.yaml:L20253-L20274] |
| `tests/fatigue/test_weld_fatigue_quickcheck.py` | `_hand_N` (L29) | [docs/registry/implementation-map.yaml:L20275-L20284] |
| `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_chain_known_value` (L117); `TestSNcurve.test_cycles_to_failure_wire_rope_known_value` (L125); `TestSNcurve.test_custom_sn_curve` (L138); `TestMinersRuleDamage.test_single_range_known_damage` (L294); `TestMinersRuleDamage.test_damage_additivity` (L319) | [docs/registry/implementation-map.yaml:L20285-L20334] |
| `tests/parametric/test_atlas.py` | `_small_atlas` (L19) | [docs/registry/implementation-map.yaml:L20335-L20344] |
| `tests/signal_processing/signal_analysis/fatigue_analysis_reference/process_fatigue_final.py` | `process_single_test_file` (L25) | [docs/registry/implementation-map.yaml:L20345-L20354] |
| `tests/signal_processing/signal_analysis/reference_fatigue_processor.py` | `process_single_test_file` (L25) | [docs/registry/implementation-map.yaml:L20355-L20364] |
| `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `<module>` (L20); `analyze_signal` (L97); `main` (L377) | [docs/registry/implementation-map.yaml:L20365-L20395] |
| `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.analyze_tension` (L108); `OrcaFlexTensionAnalyzer.export_results` (L301) | [docs/registry/implementation-map.yaml:L20396-L20415] |
| `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `run_rainflow_for_timeseries` (L19); `test_rainflow_for_timeseries` (L74) | [docs/registry/implementation-map.yaml:L20416-L20437] |
| `tests/signal_processing/signal_analysis/validate_module.py` | `main` (L183) | [docs/registry/implementation-map.yaml:L20438-L20452] |
| `tests/solvers/test_integration_solvers.py` | `mock_solver_factory.MockSolverFactory.create_fatigue_solver` (L153) | [docs/registry/implementation-map.yaml:L20453-L20462] |
| `tests/structural/fatigue/conftest.py` | `simple_sn_curve` (L13) | [docs/registry/implementation-map.yaml:L20463-L20472] |
| `tests/structural/fatigue/test_damage_accumulation.py` | `dnv_d_curve` (L39); `simple_curve` (L50); `curve_with_material` (L61) | [docs/registry/implementation-map.yaml:L20473-L20502] |
| `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_multislope_curve_implementation` (L90); `TestSNCurveMigration.test_curve_continuity` (L130); `TestFrequencyDomainMigration.test_basic_frequency_analysis` (L231) | [docs/registry/implementation-map.yaml:L20503-L20532] |
| `tests/structural/fatigue/test_fatigue_reliability.py` | `simple_sn` (L31); `mild_sn` (L37) | [docs/registry/implementation-map.yaml:L20533-L20552] |
| `tests/structural/fatigue/test_fatigue_skill.py` | `_simple_input` (L39); `TestOffshoreScenario.test_high_cycle_low_stress_result` (L494) | [docs/registry/implementation-map.yaml:L20553-L20574] |
| `tests/structural/fatigue/test_frequency_domain.py` | `simple_sn_curve` (L83) | [docs/registry/implementation-map.yaml:L20575-L20584] |
| `tests/structural/fatigue/test_rainflow.py` | `default_counter` (L40) | [docs/registry/implementation-map.yaml:L20585-L20594] |
| `tests/structural/fatigue/test_scatter_fatigue.py` | `simple_sn` (L24); `TestFatigueLife.test_fatigue_life_inverse_of_damage` (L233) | [docs/registry/implementation-map.yaml:L20595-L20616] |
| `tests/structural/fatigue/test_sn_curves.py` | `simple_power_law` (L56); `power_law_with_limit` (L62); `bilinear_curve` (L70); `dnv_d_curve` (L89); `TestBS7608RevisedCurveParameters.test_bs7608_revised_d_known_n_at_50mpa` (L886); `TestISO19902CurveParameters.test_iso19902_f_air_known_n_at_100mpa` (L964); `TestISO19902CurveParameters.test_iso19902_f2_sw_cp_known_n_at_50mpa` (L994) | [docs/registry/implementation-map.yaml:L20617-L20686] |
| `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_single_slope_cases` (L55); `_all_multislope_cases` (L63) | [docs/registry/implementation-map.yaml:L20687-L20706] |
| `tests/structural/fatigue_analysis/conftest.py` | `sn_curve_data` (L5) | [docs/registry/implementation-map.yaml:L20707-L20716] |
| `tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py` | `run_fatigue_analysis` (L13) | [docs/registry/implementation-map.yaml:L20717-L20727] |
| `tests/structural/fatigue_analysis/test_fatigue_analysis_timetrace.py` | `run_fatigue_analysis` (L15) | [docs/registry/implementation-map.yaml:L20728-L20738] |
| `tests/subsea/pipeline/test_free_span_f105.py` | `ref_input` (L42) | [docs/registry/implementation-map.yaml:L20739-L20748] |
| `tests/subsea/viv_analysis/test_viv_analysis_unit.py` | `TestVIVFatigueCalculator.test_cumulative_damage` (L502) | [docs/registry/implementation-map.yaml:L20749-L20758] |
| `tests/test_automation/fixtures/engineering_fixtures.py` | `get_orcaflex_post_process_mock` (L72) | [docs/registry/implementation-map.yaml:L20759-L20768] |
| `tests/test_automation/scripts/comprehensive_fix_campaign.py` | `ComprehensiveTestFixCampaign.generate_mock_data` (L103) | [docs/registry/implementation-map.yaml:L20769-L20778] |
| `tests/test_complete_fatigue_pipeline.py` | `process_configuration` (L65); `main` (L178) | [docs/registry/implementation-map.yaml:L20779-L20800] |
| `tests/test_fatigue_basic.py` | `run_all_tests` (L223) | [docs/registry/implementation-map.yaml:L20801-L20810] |
| `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` (L25); `main` (L117) | [docs/registry/implementation-map.yaml:L20811-L20831] |
| `tests/test_reservoir_framework.py` | `run_performance_tests` (L800) | [docs/registry/implementation-map.yaml:L20832-L20841] |
| `tests/test_spectral_fatigue_validation.py` | `TestSNCurveDNVReference.test_dnv_d_curve_parameters` (L50) | [docs/registry/implementation-map.yaml:L20842-L20853] |
| `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` (L56); `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` (L1876); `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` (L1935) | [docs/registry/implementation-map.yaml:L20854-L20886] |

#### effective-tension-riser-stack-up: 264 map rows

| Path | Symbols (map rows preserved) | Map citation |
|---|---|---|
| `docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py` | `main` (L45) | [docs/registry/implementation-map.yaml:L2146-L2155] |
| `docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py` | `main` (L44) | [docs/registry/implementation-map.yaml:L2156-L2165] |
| `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` | `main` (L47) | [docs/registry/implementation-map.yaml:L2166-L2175] |
| `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` | `main` (L48) | [docs/registry/implementation-map.yaml:L2176-L2185] |
| `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r01_catenary` (L140); `check_r02_lazy_wave` (L187); `check_r05_lay_table` (L317); `check_r06_multi_static` (L353) | [docs/registry/implementation-map.yaml:L2186-L2225] |
| `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `extract_vessel_params` (L273); `vessel_band_specs` (L293); `find_required_tension` (L563); `_build_infeasible_result` (L777); `run_single_case` (L826) | [docs/registry/implementation-map.yaml:L2226-L2282] |
| `examples/demos/gtm/results_store_demo04.py` | `<module>` (L52) | [docs/registry/implementation-map.yaml:L2283-L2291] |
| `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_good_case` (L79); `_infeasible_case` (L127); `test_999_sentinel_and_none_null_round_trip` (L342) | [docs/registry/implementation-map.yaml:L2292-L2324] |
| `examples/domains/workflows/04_pipeline_installation.py` | `analyze_installation_catenary` (L51) | [docs/registry/implementation-map.yaml:L2325-L2334] |
| `examples/workflows/riser-stackup-registry-batch/run.py` | `_schedule_row` (L115); `_assemble_row` (L219) | [docs/registry/implementation-map.yaml:L2335-L2356] |
| `scripts/capabilities/build_wall_thickness_3d.py` | `<module>` (L36); `_analyze_point` (L171); `build_study` (L222) | [docs/registry/implementation-map.yaml:L2357-L2387] |
| `scripts/capabilities/build_wall_thickness_3d_page.py` | `_axes` (L34); `_pack_rows` (L75) | [docs/registry/implementation-map.yaml:L2388-L2407] |
| `scripts/capabilities/build_wall_thickness_explorer.py` | `_run` (L48) | [docs/registry/implementation-map.yaml:L2408-L2417] |
| `scripts/orcaflex/check_generic_scr_slwr.py` | `metrics` (L38); `calculate` (L418) | [docs/registry/implementation-map.yaml:L2418-L2437] |
| `scripts/orcaflex/prepare_slay_campaign.py` | `catenary` (L27); `case_spec` (L69); `prepare_case` (L90); `prepare` (L116) | [docs/registry/implementation-map.yaml:L2438-L2484] |
| `scripts/parametric/build_orcaflex_library.py` | `main` (L39) | [docs/registry/implementation-map.yaml:L2485-L2494] |
| `scripts/python/digitalmodel/analysis/APIST2RD_Methods.py` | `APISTD2RDCalculation` (L6) | [docs/registry/implementation-map.yaml:L2495-L2504] |
| `scripts/python/digitalmodel/analysis/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` (L205) | [docs/registry/implementation-map.yaml:L2505-L2514] |
| `scripts/python/digitalmodel/analysis/RiserStackup.py` | `StackUpDiagram` (L26) | [docs/registry/implementation-map.yaml:L2515-L2524] |
| `scripts/python/digitalmodel/analysis/StackUp_3000Ft.py` | `main` (L10) | [docs/registry/implementation-map.yaml:L2525-L2534] |
| `scripts/python/digitalmodel/analysis/StackUp_9500Ft.py` | `main` (L10) | [docs/registry/implementation-map.yaml:L2535-L2544] |
| `scripts/reporting/build_riser_pipe_comparison.py` | `evaluate` (L50); `_validate` (L84); `numeric_values` (L151); `_input_table` (L210) | [docs/registry/implementation-map.yaml:L2545-L2586] |
| `scripts/riser_database/build_tables.py` | `<module>` (L24) | [docs/registry/implementation-map.yaml:L2587-L2595] |
| `src/digitalmodel/asset_integrity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` (L696) | [docs/registry/implementation-map.yaml:L2596-L2605] |
| `src/digitalmodel/code_checks/dnv_os_f101.py` | `local_buckling_lcc` (L91) | [docs/registry/implementation-map.yaml:L2606-L2615] |
| `src/digitalmodel/code_checks/workflow.py` | `_run_api_rp_2rd` (L58); `_run_dnv_os_f201` (L82) | [docs/registry/implementation-map.yaml:L2616-L2635] |
| `src/digitalmodel/drilling_riser/assembly.py` | `minimum_top_tension_16q` (L208); `_unit_count` (L254); `check_rig_capability` (L266) | [docs/registry/implementation-map.yaml:L2636-L2667] |
| `src/digitalmodel/drilling_riser/campaign.py` | `case_spec` (L134); `_ring_tensioners` (L358); `set_line_tension` (L362); `calibrate_line_tension` (L371); `_statics_path` (L400); `robust_statics` (L568); `_tensioner_vertical_n` (L734); `tensioner_vertical_target_n` (L752); `physical_state_checks` (L758); `RiserCampaignAdapter.prepare` (L855); `RiserCampaignAdapter.extract` (L912) | [docs/registry/implementation-map.yaml:L2668-L2797] |
| `src/digitalmodel/drilling_riser/envelope.py` | `compute_operating_envelope` (L278) | [docs/registry/implementation-map.yaml:L2798-L2807] |
| `src/digitalmodel/drilling_riser/global_model/build.py` | `equivalent_string_top_tension_n` (L35); `_tensioner_geometry` (L51); `winch_tension_n` (L58); `build_generic_spec` (L180) | [docs/registry/implementation-map.yaml:L2808-L2852] |
| `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `effective_tension_chain` (L45); `tension_references` (L64); `tensioner_lateral_stiffness_n_m` (L164); `reference_periods` (L216) | [docs/registry/implementation-map.yaml:L2853-L2896] |
| `src/digitalmodel/drilling_riser/global_model/hang_off.py` | `hung_weight_n` (L48) | [docs/registry/implementation-map.yaml:L2897-L2907] |
| `src/digitalmodel/drilling_riser/global_model/open_water.py` | `OpenWaterRiserSpec._closes` (L113); `build_open_water_generic_spec` (L143); `tension_references` (L218); `reference_periods` (L272); `release_reference` (L293) | [docs/registry/implementation-map.yaml:L2908-L2960] |
| `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `ring_vertical_residual_n` (L181); `open_water_physical_checks` (L309) | [docs/registry/implementation-map.yaml:L2961-L2980] |
| `src/digitalmodel/drilling_riser/global_model/spec.py` | `Tensioners._failed` (L145); `RiserGlobalModelSpec._geometry_closes` (L404) | [docs/registry/implementation-map.yaml:L2981-L3000] |
| `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `_missing_open_water` (L140); `missing_channels` (L188); `extract` (L317); `extract_open_water` (L409) | [docs/registry/implementation-map.yaml:L3001-L3042] |
| `src/digitalmodel/drilling_riser/postprocess/checks.py` | `t_min` (L172); `tension_setting_max` (L177); `tensioner_stroke` (L198) | [docs/registry/implementation-map.yaml:L3043-L3072] |
| `src/digitalmodel/drilling_riser/schedule_assembly.py` | `ScheduleAssembly.minimum_top_tension_16q_kn` (L196); `_load_oc_assembly` (L532) | [docs/registry/implementation-map.yaml:L3073-L3092] |
| `src/digitalmodel/drilling_riser/stackup.py` | `effective_tension` (L85) | [docs/registry/implementation-map.yaml:L3093-L3102] |
| `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `parse_snapshots` (L135); `snapshot_to_tension_n` (L178) | [docs/registry/implementation-map.yaml:L3103-L3122] |
| `src/digitalmodel/drilling_riser/tsj_workflow.py` | `router` (L19); `_section_result` (L74); `_effective_tension_n` (L146) | [docs/registry/implementation-map.yaml:L3123-L3155] |
| `src/digitalmodel/drilling_riser/workflow.py` | `router` (L32); `_write_profile_csv` (L154) | [docs/registry/implementation-map.yaml:L3156-L3178] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_to_shear7_result` (L251); `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` (L308); `FatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` (L374); `FatigueAnalysisComponents.save_fatigue_summary_result` (L493); `FatigueAnalysisComponents.add_fatigue_damage_results_to_df` (L521); `FatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` (L554); `FatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` (L564) | [docs/registry/implementation-map.yaml:L3179-L3251] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` (L11); `OrcaflexModelComponents.customize_fj_types` (L494); `OrcaflexModelComponents.links` (L530); `OrcaflexModelComponents.add_support_spring` (L535); `OrcaflexModelComponents.add_tensioners` (L557); `OrcaflexModelComponents.tensioner_vessel_locations` (L587); `OrcaflexModelComponents.spring_properties` (L619); `OrcaflexModelComponents.get_connection_definitions` (L677); `OrcaflexModelComponents.get_fj_line_definitions` (L691); `OrcaflexModelComponents.get_line_items_vertical` (L704) | [docs/registry/implementation-map.yaml:L3252-L3361] |
| `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.prepare_stack_up_properties_df` (L8); `TypicalRiserStackUpCalculations.populate_elevation_from_endA_in_df` (L56); `TypicalRiserStackUpCalculations.populate_shear7_x_by_L_in_df` (L95); `TypicalRiserStackUpCalculations.populate_elevation_relative_to_mudline_in_df` (L107); `TypicalRiserStackUpCalculations.populate_elevation_relative_to_MSL_in_df` (L164); `TypicalRiserStackUpCalculations.evaluate_inservice_weights_and_tensions` (L173); `TypicalRiserStackUpCalculations.assign_key_properties_to_df` (L179); `TypicalRiserStackUpCalculations.evaluate_stack_in_service_weights` (L344); `TypicalRiserStackUpCalculations.evaluate_effective_tension` (L415); `TypicalRiserStackUpCalculations.evaluate_stretch_in_riser` (L529) | [docs/registry/implementation-map.yaml:L3362-L3472] |
| `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.title` (L21); `Shear7ModelComponents.riser_joints_to_shear7_zones` (L58); `Shear7ModelComponents.add_block_3` (L152) | [docs/registry/implementation-map.yaml:L3473-L3502] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_to_shear7_result` (L454); `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` (L533); `VIVFatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` (L631); `VIVFatigueAnalysisComponents.save_fatigue_summary_result` (L799); `VIVFatigueAnalysisComponents.add_fatigue_damage_results_to_df` (L866); `VIVFatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` (L942); `VIVFatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` (L953) | [docs/registry/implementation-map.yaml:L3503-L3575] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py` | `_write_tensions_csv` (L95) | [docs/registry/implementation-map.yaml:L3576-L3585] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._line_state` (L291); `FPSOMooringFullWorkflow._solve_line` (L323); `FPSOMooringFullWorkflow._result` (L369) | [docs/registry/implementation-map.yaml:L3586-L3617] |
| `src/digitalmodel/orcaflex/mooring_design.py` | `solve_catenary` (L256) | [docs/registry/implementation-map.yaml:L3618-L3627] |
| `src/digitalmodel/orcaflex/pipelay_analysis.py` | `JLayConfig.calculate_sagbend` (L222) | [docs/registry/implementation-map.yaml:L3628-L3637] |
| `src/digitalmodel/orcaflex/riser_config.py` | `SCRDesignInput.calculate_geometry` (L170); `TTRDesignInput.calculate_stroke` (L339); `TTRDesignInput.calculate_top_tension` (L377) | [docs/registry/implementation-map.yaml:L3638-L3667] |
| `src/digitalmodel/orcaflex/riser_input_schema.py` | `_TopTensionedInput.to_legacy` (L244) | [docs/registry/implementation-map.yaml:L3668-L3677] |
| `src/digitalmodel/orcaflex/viv_screening.py` | `BeamProperties.natural_frequency` (L185) | [docs/registry/implementation-map.yaml:L3678-L3687] |
| `src/digitalmodel/parametric/generate.py` | `_code_check_utilisation` (L65) | [docs/registry/implementation-map.yaml:L3688-L3697] |
| `src/digitalmodel/pipelay/workflow.py` | `run_jlay` (L62) | [docs/registry/implementation-map.yaml:L3698-L3707] |
| `src/digitalmodel/riser_configuration_screen/static.py` | `_lazy_row` (L92); `scr_offsets` (L166) | [docs/registry/implementation-map.yaml:L3708-L3727] |
| `src/digitalmodel/riser_configuration_screen/workflow.py` | `_validate` (L28); `_utilisation` (L63); `_checks` (L97) | [docs/registry/implementation-map.yaml:L3728-L3757] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/groups_builder.py` | `GroupsBuilder._build_from_spec` (L93) | [docs/registry/implementation-map.yaml:L3758-L3767] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `LineTypeBuilder.build` (L85) | [docs/registry/implementation-map.yaml:L3768-L3777] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.should_generate` (L33); `WinchBuilder.build` (L40) | [docs/registry/implementation-map.yaml:L3778-L3797] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `_apply_overrides` (L411) | [docs/registry/implementation-map.yaml:L3798-L3807] |
| `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.mooring_pre_tension_analysis` (L127); `Mooring.evaluate_line_length_next_iteration` (L289) | [docs/registry/implementation-map.yaml:L3808-L3829] |
| `src/digitalmodel/solvers/orcaflex/opp_range_graph.py` | `OPPRangeGraph.postProcessRange` (L34) | [docs/registry/implementation-map.yaml:L3830-L3839] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.postProcessRange` (L401) | [docs/registry/implementation-map.yaml:L3840-L3849] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` (L14); `OrcaflexModelComponents.customize_fj_types` (L673); `OrcaflexModelComponents.links` (L743); `OrcaflexModelComponents.add_support_spring` (L748); `OrcaflexModelComponents.add_tensioners` (L777); `OrcaflexModelComponents.tensioner_vessel_locations` (L837); `OrcaflexModelComponents.spring_properties` (L917); `OrcaflexModelComponents.get_connection_definitions` (L1048); `OrcaflexModelComponents.get_fj_line_definitions` (L1071); `OrcaflexModelComponents.get_line_items_vertical` (L1110) | [docs/registry/implementation-map.yaml:L3850-L3959] |
| `src/digitalmodel/structural/analysis/cli.py` | `check` (L59) | [docs/registry/implementation-map.yaml:L3960-L3969] |
| `src/digitalmodel/structural/analysis/wall_thickness.py` | `WallThicknessAnalyzer.check_combined_loading` (L435) | [docs/registry/implementation-map.yaml:L3970-L3979] |
| `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py` | `ApiStd2rdStrategy._check_combined` (L161) | [docs/registry/implementation-map.yaml:L3980-L3989] |
| `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py` | `DnvStF101Strategy._check_combined_loading` (L268) | [docs/registry/implementation-map.yaml:L3990-L3999] |
| `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py` | `run_wall_thickness_compare` (L41) | [docs/registry/implementation-map.yaml:L4000-L4009] |
| `src/digitalmodel/structural/analysis/wall_thickness_lookup.py` | `MTLookupGenerator.generate` (L60) | [docs/registry/implementation-map.yaml:L4010-L4019] |
| `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `ParametricSweep.run` (L66) | [docs/registry/implementation-map.yaml:L4020-L4029] |
| `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `PipelinePhase.to_design_loads` (L141); `create_standard_phases` (L156) | [docs/registry/implementation-map.yaml:L4030-L4049] |
| `src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py` | `LoadScalingProcessor.process_all_conditions` (L186) | [docs/registry/implementation-map.yaml:L4050-L4059] |
| `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` (L290) | [docs/registry/implementation-map.yaml:L4060-L4069] |
| `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` (L81) | [docs/registry/implementation-map.yaml:L4070-L4079] |
| `src/digitalmodel/structural/fatigue_apps/load_scaling.py` | `LoadScalingProcessor.load_reference_data` (L101) | [docs/registry/implementation-map.yaml:L4080-L4089] |
| `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` (L355) | [docs/registry/implementation-map.yaml:L4090-L4099] |
| `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_fatigue_condition` (L458); `LoadScaler.process_sea_state` (L506); `main` (L566) | [docs/registry/implementation-map.yaml:L4100-L4129] |
| `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py` | `ProductionDataHandlerWithProgress.process_single_analysis` (L83) | [docs/registry/implementation-map.yaml:L4130-L4139] |
| `src/digitalmodel/structural/parametric_coordinator.py` | `ParametricCoordinator._run_wall_thickness` (L255) | [docs/registry/implementation-map.yaml:L4140-L4149] |
| `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` (L985) | [docs/registry/implementation-map.yaml:L4150-L4159] |
| `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` (L342) | [docs/registry/implementation-map.yaml:L4160-L4169] |
| `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` (L1272) | [docs/registry/implementation-map.yaml:L4170-L4179] |
| `src/digitalmodel/subsea/catenary_riser/cli.py` | `simple_catenary` (L31); `lazy_wave_analysis` (L170) | [docs/registry/implementation-map.yaml:L4180-L4199] |
| `src/digitalmodel/subsea/catenary_riser/lazy_wave.py` | `LazyWaveAnalyzer.analyze` (L34) | [docs/registry/implementation-map.yaml:L4200-L4209] |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.analyze_riser` (L42); `SimpleCatenaryAnalyzer._solve_catenary_from_H` (L103); `SimpleCatenaryAnalyzer._solve_H_from_top_tension` (L174); `SimpleCatenaryAnalyzer.check_design_limits` (L396) | [docs/registry/implementation-map.yaml:L4210-L4252] |
| `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `developed_min_radius` (L64); `required_effective_ei` (L91); `screen_bend_stiffener` (L109) | [docs/registry/implementation-map.yaml:L4253-L4285] |
| `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `armour_tension_stress` (L160); `verify_flexible_pipe` (L178) | [docs/registry/implementation-map.yaml:L4286-L4307] |
| `src/digitalmodel/subsea/pipeline/global_buckling/hobbs_lateral.py` | `effective_driving_force` (L294) | [docs/registry/implementation-map.yaml:L4308-L4317] |
| `src/digitalmodel/subsea/riser/riser_case_screening.py` | `RiserLoadCase.to_loads` (L44); `run_riser_screening` (L125) | [docs/registry/implementation-map.yaml:L4318-L4337] |
| `src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py` | `StackUpDiagram` (L27) | [docs/registry/implementation-map.yaml:L4338-L4347] |
| `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_riser_model` (L28); `VerticalRiser.read_riser_input_data` (L41); `VerticalRiser.assign_key_properties_to_self` (L50); `VerticalRiser.get_fea_properties` (L120); `VerticalRiser.prepare_tensioner_data` (L297); `VerticalRiser.evaluate_stack_up_properties` (L325); `VerticalRiser.save_data` (L374); `VerticalRiser.alter_properties_for_Shear7_software` (L403) | [docs/registry/implementation-map.yaml:L4348-L4432] |
| `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_natural_frequency` (L59); `FrequencyCalculator._frequency_tension_controlled` (L164); `FrequencyCalculator.calculate_effective_tension` (L248); `FrequencyCalculator.check_stability` (L288) | [docs/registry/implementation-map.yaml:L4433-L4476] |
| `src/digitalmodel/viv_screening/inputs.py` | `_beam` (L63) | [docs/registry/implementation-map.yaml:L4477-L4486] |
| `src/digitalmodel/workflows/workflow_automation/cli.py` | `riser_analysis_cmd` (L74) | [docs/registry/implementation-map.yaml:L4487-L4496] |
| `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_riser_analysis_template` (L257) | [docs/registry/implementation-map.yaml:L4497-L4506] |
| `src/digitalmodel/workflows/workflow_automation/workflows.py` | `CompleteRiserAnalysisWorkflow.create` (L34) | [docs/registry/implementation-map.yaml:L4507-L4516] |
| `tests/asset_integrity/test_calculations.py` | `_make_pipe_cfg` (L29) | [docs/registry/implementation-map.yaml:L4517-L4526] |
| `tests/capabilities/test_wall_thickness_3d_page.py` | `_study` (L32) | [docs/registry/implementation-map.yaml:L4527-L4536] |
| `tests/contracts/test_force_units_boundary_contract.py` | `<module>` (L52); `test_envelope_von_mises_n_to_kn_seam` (L454) | [docs/registry/implementation-map.yaml:L4537-L4557] |
| `tests/drilling_riser/global_model/conftest.py` | `synthetic_spec` (L42) | [docs/registry/implementation-map.yaml:L4558-L4567] |
| `tests/drilling_riser/global_model/test_batch_params.py` | `test_physical_checks_use_the_remaining_tensioners` (L77) | [docs/registry/implementation-map.yaml:L4568-L4578] |
| `tests/drilling_riser/global_model/test_campaign.py` | `_fake` (L65) | [docs/registry/implementation-map.yaml:L4579-L4589] |
| `tests/drilling_riser/global_model/test_open_water.py` | `open_water_dict` (L43) | [docs/registry/implementation-map.yaml:L4590-L4599] |
| `tests/drilling_riser/global_model/test_open_water_campaign.py` | `_doc` (L17) | [docs/registry/implementation-map.yaml:L4600-L4609] |
| `tests/drilling_riser/global_model/test_orcaflex_solver.py` | `test_statics_tension_and_modes_agree_with_hand_checks` (L17) | [docs/registry/implementation-map.yaml:L4610-L4623] |
| `tests/drilling_riser/global_model/test_statics_path.py` | `test_branch_tolerance_and_ring_buoyancy` (L281) | [docs/registry/implementation-map.yaml:L4624-L4634] |
| `tests/drilling_riser/global_model/test_tensioner_representation.py` | `_vertical` (L24) | [docs/registry/implementation-map.yaml:L4635-L4644] |
| `tests/drilling_riser/postprocess/test_aggregate.py` | `_mc` (L20); `_case` (L44) | [docs/registry/implementation-map.yaml:L4645-L4666] |
| `tests/drilling_riser/test_drive_off_worked_example.py` | `_recorded_arguments` (L48) | [docs/registry/implementation-map.yaml:L4667-L4676] |
| `tests/drilling_riser/test_schedule_assembly.py` | `_deltas` (L57) | [docs/registry/implementation-map.yaml:L4677-L4686] |
| `tests/infrastructure/unit/test_pipe_capacity_codes.py` | `_base_cfg` (L6) | [docs/registry/implementation-map.yaml:L4687-L4696] |
| `tests/orcaflex/test_generic_scr_slwr.py` | `test_analytical_reference_reproduces_from_basis` (L215) | [docs/registry/implementation-map.yaml:L4697-L4706] |
| `tests/parametric/test_atlas.py` | `_code_check_atlas` (L84) | [docs/registry/implementation-map.yaml:L4707-L4716] |
| `tests/reporting/test_riser_pipe_comparison.py` | `reference_input` (L24) | [docs/registry/implementation-map.yaml:L4717-L4726] |
| `tests/riser_configuration_screen/test_workflow.py` | `settings` (L17) | [docs/registry/implementation-map.yaml:L4727-L4736] |
| `tests/solvers/orcaflex/analysis/scripts/verify_configs.py` | `verify_configuration` (L13) | [docs/registry/implementation-map.yaml:L4737-L4746] |
| `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py` | `_make_slay_spec` (L39) | [docs/registry/implementation-map.yaml:L4747-L4756] |
| `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` | `_make_pipeline_spec` (L22) | [docs/registry/implementation-map.yaml:L4757-L4766] |
| `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` | `_make_pipeline_spec` (L21) | [docs/registry/implementation-map.yaml:L4767-L4776] |
| `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_slay_campaign` (L126) | [docs/registry/implementation-map.yaml:L4777-L4786] |
| `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py` | `_make_base_spec_data` (L8) | [docs/registry/implementation-map.yaml:L4787-L4796] |
| `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` | `extract_static_range_results` (L165) | [docs/registry/implementation-map.yaml:L4797-L4806] |
| `tests/solvers/orcaflex/modular_generator/test_post_validator.py` | `_make_valid_model` (L26) | [docs/registry/implementation-map.yaml:L4807-L4816] |
| `tests/solvers/orcaflex/test_slay_layout.py` | `test_support_types_are_defined_before_vessel_references` (L89) | [docs/registry/implementation-map.yaml:L4817-L4826] |
| `tests/specialized/umbilical_analysis/conftest.py` | `installation_parameters` (L23) | [docs/registry/implementation-map.yaml:L4827-L4836] |
| `tests/subsea/catenary_riser/test_catenary.py` | `run_catenary_riser` (L13) | [docs/registry/implementation-map.yaml:L4837-L4846] |
| `tests/test_automation/fixtures/engineering_fixtures.py` | `get_catenary_analysis_mock` (L97) | [docs/registry/implementation-map.yaml:L4847-L4856] |
| `tests/test_automation/scripts/batch_fix_tests.py` | `<module>` (L12) | [docs/registry/implementation-map.yaml:L4857-L4865] |
| `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` (L25); `main` (L117) | [docs/registry/implementation-map.yaml:L4866-L4887] |
| `tests/test_wall_thickness_compare_workflow.py` | `_cfg` (L22) | [docs/registry/implementation-map.yaml:L4888-L4897] |
| `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` (L56) | [docs/registry/implementation-map.yaml:L4898-L4907] |
| `tests/workflows/test_viv_screening_workflow.py` | `screening_input` (L17) | [docs/registry/implementation-map.yaml:L4908-L4917] |

### Map follow-up

Load scaling, tension-to-stress conversion and independent cycle-distribution assessment are covered by this pack but do not have dedicated in-package map-family rows under those method names; the map follow-up will add reviewed rows before treating any new family as counted (issue pending (5)). The effective-tension vocabulary currently includes processors that scale or hold a tension history; its family classification will be refined without changing this inventory. [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L364-L428] [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L155-L177]

Repository-wide supplementary discovery included `scripts/`, using `rg -n --glob '*.py' 'class (RainflowCounter|FatigueDamageCalculator|SNCurve)|def (rainflow_counting_astm|calculate_combined_damage|calculate_annual_damage|calculate_fatigue_life|cycles_to_failure|calculate_cycles_to_failure)' src scripts`; the map inventory includes the sibling fatigue and signal-analysis implementations. This is supplementary evidence, not a claim of complete global map coverage.

## 10. Coverage and open items

The package read inventory includes all 23 Python files, including the example and validation subfolders; no solver is required to inspect these sources. The following read inventory uses source-range citations for the pinned files; blank or export-only modules are included.

| Python source | Read range |
|---|---|
| `__init__.py` | [src/digitalmodel/structural/fatigue_apps/__init__.py:L1-L60] |
| `__main__.py` | [src/digitalmodel/structural/fatigue_apps/__main__.py:L1-L520] |
| `cli_load_scaling.py` | [src/digitalmodel/structural/fatigue_apps/cli_load_scaling.py:L1-L352] |
| `examples/load_scaling_runner.py` | [src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py:L1-L352] |
| `fatigue_damage.py` | [src/digitalmodel/structural/fatigue_apps/fatigue_damage.py:L1-L553] |
| `fatigue_damage_calculator.py` | [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L1-L389] |
| `fatigue_validation/__init__.py` | [src/digitalmodel/structural/fatigue_apps/fatigue_validation/__init__.py:L1-L26] |
| `fatigue_validation/models.py` | [src/digitalmodel/structural/fatigue_apps/fatigue_validation/models.py:L1-L132] |
| `fatigue_validation/validator.py` | [src/digitalmodel/structural/fatigue_apps/fatigue_validation/validator.py:L1-L50] |
| `file_namer.py` | [src/digitalmodel/structural/fatigue_apps/file_namer.py:L1-L230] |
| `integrated_processor.py` | [src/digitalmodel/structural/fatigue_apps/integrated_processor.py:L1-L489] |
| `integrated_processor_with_naming.py` | [src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py:L1-L461] |
| `load_scaling.py` | [src/digitalmodel/structural/fatigue_apps/load_scaling.py:L1-L569] |
| `load_scaling_efficient_v2.py` | [src/digitalmodel/structural/fatigue_apps/load_scaling_efficient_v2.py:L1-L452] |
| `parallel_processor.py` | [src/digitalmodel/structural/fatigue_apps/parallel_processor.py:L1-L546] |
| `rainflow_counter.py` | [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L1-L359] |
| `rainflow_counting.py` | [src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:L1-L506] |
| `reference_seastate_processor.py` | [src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py:L1-L635] |
| `strut_foundation_processor.py` | [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py:L1-L665] |
| `strut_foundation_processor_with_progress.py` | [src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py:L1-L228] |
| `tension_stress_conversion.py` | [src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py:L1-L478] |
| `validate_load_scaling_interactive.py` | [src/digitalmodel/structural/fatigue_apps/validate_load_scaling_interactive.py:L1-L704] |
| `visualizer.py` | [src/digitalmodel/structural/fatigue_apps/visualizer.py:L1-L582] |

The package documentation read comprises `README.md`, `INPUT_FILE_STRUCTURE.md`, `LOAD_SCALING_DOCUMENTATION.md` and `LOAD_SCALING_VALIDATION.md`; additional matching documentation is `docs/registry/implementation-map.md` and `docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md`. [src/digitalmodel/structural/fatigue_apps/README.md:L1-L19] [src/digitalmodel/structural/fatigue_apps/INPUT_FILE_STRUCTURE.md:L1-L20] [src/digitalmodel/structural/fatigue_apps/LOAD_SCALING_DOCUMENTATION.md:L1-L20] [src/digitalmodel/structural/fatigue_apps/LOAD_SCALING_VALIDATION.md:L1-L20] [docs/registry/implementation-map.md:L1-L20] [docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md:L1-L20]

### Test inventory and execution

Execution uses `PYTHONPATH=src uv run --no-sources --extra test python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`, with the uv executable and a dedicated environment. No `addopts` override is used. AFTER means the uncommitted documentation tree at the same code revision, not a post-commit execution.

| Run / selected paths | BASE result | AFTER result |
|---|---|---|
| T1 three core suites plus `tests/structural/fatigue_apps/test_load_scaling.py` | 58 collected; 0 executed; 1 collection error: `ImportError: cannot import name 'LoadScaler'`; 16 definitions in the stale file could not collect | Same baseline collection error; 58 collected, 0 executed |
| T2 `tests/structural/fatigue_analysis/test_rainflow_counter.py`, `test_reference_seastate_processor.py`, `test_fatigue_cli.py` | 58 run / 58 passed / 0 failed / 0 skipped / 0 NOT_RUN | 58 run / 58 passed / 0 failed / 0 skipped / 0 NOT_RUN |
| T3 both `tests/naming_convention_validation/` files plus `tests/test_complete_fatigue_pipeline.py` and `tests/test_rainflow_with_effective_tension.py` | 7 run / 7 passed / 0 failed / 0 skipped / 0 NOT_RUN; the two standalone demonstrations collect 0 test nodes | 7 run / 7 passed / 0 failed / 0 skipped / 0 NOT_RUN |
| T4 selected package counting-contract and compatibility nodes listed below | 8 run / 7 passed / 0 failed / 0 skipped / 0 NOT_RUN / 1 existing XFAIL | 8 run / 7 passed / 0 failed / 0 skipped / 0 NOT_RUN / 1 existing XFAIL |

Additional routing/domain validation on the uncommitted tree: `uv run --no-sources --extra test python -m pytest tests/contracts/ tests/docs/test_digitalmodel_routing_contract.py -n 2 -q -rA -p no:cacheprovider --basetemp=<scratch>`; 494 run / 494 passed / 0 failed / 0 skipped / 0 NOT_RUN, at the same code pin. The complete repository test matrix is not claimed.

T3 runs unchanged source/test copies in a disposable directory with `--project <owning-checkout>`; generated output stays there. Its missing-data warnings are not a successful physical fatigue campaign. [tests/naming_convention_validation/test_integrated_processor_naming.py:L29-L56] [tests/naming_convention_validation/test_updated_naming_convention.py:L114-L158]

T4 node selection: `tests/fatigue/test_rainflow_invariants.py::test_4_contract_fapps_counter` (four signals), `tests/fatigue/test_rainflow_invariants.py::test_7a_contract_fapps_counting_broadband`, `tests/compat/test_group_compat.py::TestFlatToGroupMapping::test_get_group_for_known_module`, `tests/compat/test_group_compat.py::TestFlatImports::test_flat_import[fatigue_apps]`, and `tests/compat/test_group_compat.py::TestGroupedImports::test_grouped_import[structural-fatigue_apps]`; unrelated program-import cases are outside this package selection. [tests/fatigue/test_rainflow_invariants.py:L125-L127] [tests/fatigue/test_rainflow_invariants.py:L164-L169] [tests/compat/test_group_compat.py:L53-L63] [tests/compat/test_group_compat.py:L112-L125] [tests/compat/test_group_compat.py:L168-L180]

The eight directly discovered module test files contain 81 syntactic test definitions: 58 core, 7 naming, 16 stale load-scaling, and none in the two standalone demonstrations; parametrized cross-package checks are counted separately, and this scope-specific count does not adopt an inventory estimate. [tests/structural/fatigue_apps/test_load_scaling.py:L28-L295] [tests/structural/fatigue_analysis/test_rainflow_counter.py:L14-L281] [tests/structural/fatigue_analysis/test_reference_seastate_processor.py:L28-L480] [tests/structural/fatigue_analysis/test_fatigue_cli.py:L29-L421] [tests/naming_convention_validation/test_integrated_processor_naming.py:L26-L127] [tests/naming_convention_validation/test_updated_naming_convention.py:L25-L190] [tests/test_complete_fatigue_pipeline.py:L29-L271] [tests/test_rainflow_with_effective_tension.py:L25-L186]


| Open item / qualification | Responsible role | Tracking |
|---|---|---|
| UNVERIFIED: ABS/DNV/Eurocode edition, clause, preset provenance and Citation sidecar; resolve the dimensional branch defect and supply an independent damage benchmark before engineering use. | Engineering calculations quality manager | issue pending (1) |
| UNVERIFIED: coordinate/heading/phase conventions, physical data lineage, dataset inventory IDs and calibrated stress conversion; sidecar entry: to be created. Defaults and reference histories are not a qualified design basis. | Maintainer and engineering calculations quality manager | issue pending (2) |
| No G0 units/citation detection check exists: add checks that reject incompatible units and missing or unqualified Citation sidecars; C0 checks missing paths and C6 only characterises fallback. | Maintainer and engineering calculations quality manager | issue pending (1); issue pending (2) |
| Missing-case completeness, fixed timestep, truncation, numeric text parsing, unit validation and synthetic fallback controls listed in section 6. | Maintainer | issue pending (2) |
| Empty-success status, partial output, generated AQG appendix/fingerprint evidence and unimplemented validation status checks. C5 checks serialization only. Trace `overall_fatigue_life` to its input set in a generated record, including source histories, units, scaling, occurrence weights, timestep and calculation revision; no detection check exists. | Maintainer | issue pending (3) |
| Diagnostic rainflow limits, ignored filter, stale documentation imports/test commands and shared-log assignment. README/document validation claims remain unverified until independently checked. | Maintainer | issue pending (4) |
| Stale load-scaling test import and any test-run baseline failures recorded above; no module code is changed here. | Maintainer | #1923; issue pending (4) |
| Additional computing definitions, dedicated method families and effective-tension classification refinement in the map. | Maintainer | issue pending (5) |
| Independent provider clearance, shared-log entry assignment and replacement of issue-draft markers with opened issue numbers before clearance. | Engineering calculations quality manager | issue pending (4) |

The package's documentation still names an obsolete `fatigue_analysis` namespace and test commands outside the discovered test tree; these are documentation defects rather than proof of current behavior. [src/digitalmodel/structural/fatigue_apps/README.md:L29-L49] [src/digitalmodel/structural/fatigue_apps/README.md:L257-L265] [src/digitalmodel/structural/fatigue_apps/LOAD_SCALING_DOCUMENTATION.md:L288-L304]
