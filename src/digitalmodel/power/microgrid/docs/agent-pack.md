# Microgrid knowledge pack

| Header | Value |
| --- | --- |
| Module | power/microgrid |
| Repository | digitalmodel |
| Path | src/digitalmodel/power/microgrid |
| Pack revision | 0 |
| Code revision | c099a5e1317e6bed63d876252d87cbb12c774372 |
| Analysis family | Engineering calculations (EC) |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package provides distributed energy resource models, battery state-of-charge control, frequency/angle islanding detection and rule-based energy dispatch. [src/digitalmodel/power/microgrid/__init__.py:L5-L23]
The separate droop module computes active/reactive droop curves, parallel generator load sharing and recommended droop settings. [src/digitalmodel/power/microgrid/droop_control.py:L88-L114] [src/digitalmodel/power/microgrid/droop_control.py:L117-L143] [src/digitalmodel/power/microgrid/droop_control.py:L164-L206] [src/digitalmodel/power/microgrid/droop_control.py:L309-L354]
The enclosing power domain is beta; that registry entry supplies no separate microgrid maturity. [docs/registry/module-routing.yaml:L295-L307]
Scope is this package, excluding the neighbouring analysis, protection, datacenter, commissioning and protocols packages; its control cycle is detection followed by dispatch and a state snapshot, without a battery time-integration call or physical breaker operation. [src/digitalmodel/power/microgrid/microgrid_ems.py:L125-L152]
Black start and reconnect change mode flags; reconnect sets islanded mode rather than utility-connected mode. [src/digitalmodel/power/microgrid/microgrid_ems.py:L154-L176]

## 2. Inputs and outputs

### Inputs

| Input / upstream object | Unit | Enforced range / limitation | Default | Evidence |
| --- | --- | --- | --- | --- |
| DERAsset: asset_id, der_type, capacity_kw, marginal_cost, dispatch_priority, available | identifier, enum, kW, currency/kWh, integer, boolean | capacity > 0; priority >= 1; no uniqueness or finite-value guard | available=True; others required | [src/digitalmodel/power/microgrid/models.py:L62-L97] |
| BESSController: capacity_kwh, max_power_kw | kWh, kW | stored without positivity/finite-value checks | required | [src/digitalmodel/power/microgrid/bess_controller.py:L43-L63] |
| soc, soc_min, soc_max | fraction | min < max and min <= initial SOC <= max; no absolute [0,1] guard | 0.5, 0.10, 0.90 | [src/digitalmodel/power/microgrid/bess_controller.py:L43-L63] |
| requested_kw; power_kw; dt_hours | kW; kW; h | positive power discharges; negative charges; elapsed time not validated | required; island_mode=False | [src/digitalmodel/power/microgrid/bess_controller.py:L108-L148] |
| IslandDetector thresholds | Hz/s, degrees | no validation; inclusive magnitude comparison | code defaults at cited constructor; not established standard settings | [src/digitalmodel/power/microgrid/island_detector.py:L41-L58] [src/digitalmodel/power/microgrid/island_detector.py:L151-L160] |
| timestamps, frequencies, voltage angles | s, Hz, degrees | at least 2 timestamps; length equality, order and finiteness unchecked | required | [src/digitalmodel/power/microgrid/island_detector.py:L60-L97] [src/digitalmodel/power/microgrid/island_detector.py:L99-L124] |
| GovernorDroop: rated_power_kw, droop_percent, nominal_freq_hz | kW, %, Hz | droop in [0,10]; rating and frequency unchecked | nominal frequency 60.0 Hz | [src/digitalmodel/power/microgrid/droop_control.py:L16-L38] |
| AVRDroop: rated_reactive_kvar, droop_percent, nominal_voltage_v | kVAR, %, V | droop in [0,10]; rating and voltage unchecked | nominal voltage 480.0 V | [src/digitalmodel/power/microgrid/droop_control.py:L41-L63] |
| Generator; total_load_kw; nominal_freq_hz | object; kW; Hz | isochronous flag selects solver; nonpositive load returns zero output | is_isochronous=False; frequency 60.0 Hz | [src/digitalmodel/power/microgrid/droop_control.py:L66-L85] [src/digitalmodel/power/microgrid/droop_control.py:L164-L206] |
| target_splits | fraction per generator | equal list lengths and positive splits enforced; sum-to-one not enforced | required | [src/digitalmodel/power/microgrid/droop_control.py:L309-L354] |
| EMS assets, BESS controller, detector; load_kw | objects; kW | stores references; dispatch sorts priority before cost; one shared battery controller | starts grid-connected | [src/digitalmodel/power/microgrid/microgrid_ems.py:L40-L95] |

### Outputs and conventions

| Output | Unit / convention | Evidence |
| --- | --- | --- |
| max_discharge_kw, max_charge_kw, compute_power_setpoint | kW; positive setpoint discharges, negative charges | [src/digitalmodel/power/microgrid/bess_controller.py:L65-L133] |
| update_soc | mutates fractional SOC, clipped to configured bounds | [src/digitalmodel/power/microgrid/bess_controller.py:L135-L148] |
| DetectionResult | trip booleans; signed regression slope in Hz/s; minimum angle displacement in degrees | [src/digitalmodel/power/microgrid/island_detector.py:L16-L38] [src/digitalmodel/power/microgrid/island_detector.py:L151-L161] |
| power_at_frequency; reactive_at_voltage | nonnegative kW; nonnegative kVAR, clipped to rating | [src/digitalmodel/power/microgrid/droop_control.py:L108-L143] |
| parallel_load_sharing | frequency_hz, gen_outputs containing ID/power_kw/pct_rated, unserved_kw | [src/digitalmodel/power/microgrid/droop_control.py:L293-L306] |
| recommend_droop_settings | list of percentages, clamped after normalization | [src/digitalmodel/power/microgrid/droop_control.py:L342-L354] |
| merit_order_dispatch | mapping of asset ID to kW; repeated IDs overwrite previous values | [src/digitalmodel/power/microgrid/microgrid_ems.py:L73-L95] |
| MicrogridState | mode, load/generation/import/unserved/curtailment in kW, frequency in Hz; net load = load minus generation | [src/digitalmodel/power/microgrid/models.py:L100-L139] |

The state model documents negative grid import as export, but EMS update emits only nonnegative imports; curtailment retains its zero default. [src/digitalmodel/power/microgrid/models.py:L112-L128] [src/digitalmodel/power/microgrid/microgrid_ems.py:L138-L152]
These in-memory APIs use time in seconds for detection and hours for battery integration; voltage angles are degrees, with modulo wrapping and no geographical frame or file-format conversion in these calculations. [src/digitalmodel/power/microgrid/island_detector.py:L60-L124] [src/digitalmodel/power/microgrid/bess_controller.py:L135-L148]

## 3. Method and governing standards

| Method | Implemented calculation / reference |
| --- | --- |
| Battery limits | max_discharge_kw and max_charge_kw linearly derate near configured SOC bounds; compute_power_setpoint clips the request; update_soc subtracts power times hours divided by capacity and clips SOC. [src/digitalmodel/power/microgrid/bess_controller.py:L65-L148] |
| Island detection | compute_rocof uses least-squares slope; compute_vector_shift wraps to the minimum displacement; detect trips when either magnitude is greater than or equal to its threshold. [src/digitalmodel/power/microgrid/island_detector.py:L86-L97] [src/digitalmodel/power/microgrid/island_detector.py:L121-L124] [src/digitalmodel/power/microgrid/island_detector.py:L151-L161] |
| Droop curves | power_at_frequency computes rated_power*(nominal-frequency)/(nominal*droop/100); reactive_at_voltage substitutes voltage and reactive rating, with zero-droop special cases. [src/digitalmodel/power/microgrid/droop_control.py:L88-L143] |
| Parallel sharing | With isochronous generators, hold nominal frequency and allocate remaining power by rating; otherwise bisect frequency with a bounded loop and power-residual break condition. [src/digitalmodel/power/microgrid/droop_control.py:L209-L306] |
| Recommended settings | Rating divided by target share is normalized to a hard-coded average, then each recommendation is clamped; clipping can alter the requested split. [src/digitalmodel/power/microgrid/droop_control.py:L331-L354] |
| EMS | Sort by (priority,cost), skip unavailable resources, constrain BESS using the shared controller, subtract each dispatch from remaining demand; update switches modes on detection. [src/digitalmodel/power/microgrid/microgrid_ems.py:L73-L152] |

The code names IEEE 1547.4-2011, *Guide for Design, Operation, and Integration of Distributed Resource Island Systems with Electric Power Systems*, in both domain-model and EMS docstrings. [src/digitalmodel/power/microgrid/models.py:L8-L12] [src/digitalmodel/power/microgrid/microgrid_ems.py:L6-L10]
The BESS reference names section 6 as “Energy storage integration requirements”, droop names section 5 as “Frequency and voltage regulation for island systems”, and the detector names section 6.4 as “Islanding detection requirements”; these are code-attributed clause titles, clause text not checked against the licensed copy. [src/digitalmodel/power/microgrid/bess_controller.py:L7-L10] [src/digitalmodel/power/microgrid/droop_control.py:L6-L9] [src/digitalmodel/power/microgrid/island_detector.py:L1-L9]
UNVERIFIED: the clause attribution, compliance of the implemented methods and provenance of the default protection settings have not been independently established; issue pending (1). [src/digitalmodel/power/microgrid/island_detector.py:L41-L58]
No standards-derived numeric setting is endorsed here; the battery policy constants remain code choices without an emitted Citation sidecar. [src/digitalmodel/power/microgrid/bess_controller.py:L14-L18] [src/digitalmodel/power/microgrid/bess_controller.py:L81-L88]

## 4. Validated worked examples

The following authored audit record uses elementary closed forms independent of the implementation; inputs and expected assertions are embedded in the cited test functions, with no external input file. [tests/power/microgrid/test_island_detector.py:L10-L17] [tests/power/microgrid/test_island_detector.py:L49-L53] [tests/power/microgrid/test_bess_controller.py:L105-L115]

| Case | Independent basis, input and expected result | Tolerance and basis | Existing test node ID | Offline record at code pin |
| --- | --- | --- | --- | --- |
| V1 | Constant slope: (59-60)/(1-0) = -1.0 Hz/s for the linear sample series. [tests/power/microgrid/test_island_detector.py:L10-L17] | 0.05 Hz/s, explicit test assertion | tests/power/microgrid/test_island_detector.py::TestROCOF::test_declining_frequency | PASS at BASE and AFTER |
| V2 | Circular distance: min(340,360-340) = 20.0 degrees for 350 to 10 degrees. [tests/power/microgrid/test_island_detector.py:L49-L53] | Existing pytest.approx assertion; independent arithmetic audit <= 1e-9 degrees | tests/power/microgrid/test_island_detector.py::TestVectorShift::test_wraps_at_180 | PASS at BASE and AFTER |
| V3 | Energy conservation before clipping: 0.5-(50 kW*0.5 h)/(100 kWh) = 0.25 SOC. [tests/power/microgrid/test_bess_controller.py:L105-L115] | Existing pytest.approx assertion; independent arithmetic audit <= 1e-12 fraction | tests/power/microgrid/test_bess_controller.py::TestUpdateSOC::test_discharge_decreases_soc | PASS at BASE and AFTER |

These closed-form checks establish arithmetic properties, rather than standards compliance; the other representative cases below are regression examples because their expectations encode implementation choices. [tests/power/microgrid/test_droop_control.py:L103-L109] [tests/power/microgrid/test_microgrid_ems.py:L38-L44]

| Case | Expected behaviour / input source | Existing test node ID | Offline record at code pin |
| --- | --- | --- | --- |
| R1 | Half-droop active output 250.0 kW, using the test's code-policy curve and pytest.approx. [tests/power/microgrid/test_droop_control.py:L103-L109] | tests/power/microgrid/test_droop_control.py::TestPowerAtFrequency::test_at_half_droop_returns_half_rated | PASS at BASE and AFTER |
| R2 | Full voltage-droop reactive output 300.0 kVAR using the test's settings and pytest.approx. [tests/power/microgrid/test_droop_control.py:L128-L134] | tests/power/microgrid/test_droop_control.py::TestReactiveAtVoltage::test_at_full_droop_voltage_returns_rated | PASS at BASE and AFTER |
| R3 | An 800.0 kW request dispatches 500.0/200.0/100.0 kW to the fixture's three resource IDs, each with pytest.approx. [tests/power/microgrid/test_microgrid_ems.py:L17-L26] [tests/power/microgrid/test_microgrid_ems.py:L38-L44] | tests/power/microgrid/test_microgrid_ems.py::TestMeritOrderDispatch::test_dispatch_spills_to_next_der | PASS at BASE and AFTER |
| R4 | Black start followed by reconnect ends in islanded mode, with exact enum equality. [tests/power/microgrid/test_microgrid_ems.py:L121-L127] | tests/power/microgrid/test_microgrid_ems.py::TestBlackStart::test_black_start_to_island_transition | PASS at BASE and AFTER |

Audit execution record: `PYTHONPATH=src uv run --no-sources python -m pytest tests/power/microgrid -q -rA -p no:cacheprovider --basetemp=<scratch>`; no addopts override, serial execution, code pin in the header, BASE 71/71 passed, AFTER 71/71 passed, zero failed/skipped/NOT_RUN in each run; per-file counts are battery 14, droop 29, detector 10, EMS 10 and models 8.

## 5. QA gates

Gate applicability follows AQG r0, section 10, “Python calculations”, and section 9, “G5 QA appendix”; the entries below define the checks for this pack, without claiming independent clearance.

| Gate | Applicability / route | Checks and numeric pass criterion | Evidence |
| --- | --- | --- | --- |
| G0 | Applicable: in-memory input units, validation and citation inventory | C01: the existing negative-capacity, zero-priority and negative-frequency model cases each raise ValueError, 3/3 cases; C02: battery invalid SOC ordering/range cases reject, 2/2 cases; C03: section-6 source-site and unit inventory has 0 omitted sites (count). | Model and battery tests plus sweep inventory. [tests/power/microgrid/test_models.py:L46-L66] [tests/power/microgrid/test_models.py:L81-L90] [tests/power/microgrid/test_bess_controller.py:L19-L34] |
| G1 | Applicable: Python imports and offline tests; no solver program/file writer route | C04: 71/71 module test cases pass and 0 collection errors (counts); public imports resolve through the package exports. | BASE and AFTER pytest records; imports. [src/digitalmodel/power/microgrid/__init__.py:L5-L23] |
| G2 | Applicable: closed forms and control regression | C05: V1 slope error <= 0.05 Hz/s; C06: V2 angular error <= 1e-9 degrees in the arithmetic audit; C07: V3 SOC error <= 1e-12 fraction in the arithmetic audit; C08: R1 active error <= 1e-6 relative fraction; C09: R2 reactive error <= 1e-6 relative fraction; C10: R3 each dispatch error <= 1e-6 relative fraction. | Tests named in section 4 and independent arithmetic record. [tests/power/microgrid/test_island_detector.py:L10-L17] [tests/power/microgrid/test_island_detector.py:L49-L53] [tests/power/microgrid/test_bess_controller.py:L105-L115] [tests/power/microgrid/test_droop_control.py:L103-L109] [tests/power/microgrid/test_droop_control.py:L128-L134] [tests/power/microgrid/test_microgrid_ems.py:L38-L44] |
| G3 | NOT_APPLICABLE: this family evaluates Python calculations; no short solver-dynamics run | 0 solver runs required (count) | AQG r0, section 10, “Python calculations”. |
| G4 | NOT_APPLICABLE: no diffraction mesh/body calculation in this package | 0 diffraction cases required (count) | AQG r0, section 10, “Python calculations”. |
| G5 | Applicable: retain a reproducible evidence inventory before reporting | C11: 7/7 section-4 cases carry a node, pin and result; every citation range resolves, 0 invalid ranges (count); retain input/source fingerprints with the evidence record. | This pack's audit record and citation resolver; AQG r0, section 9, “G5 QA appendix”. |

Additional diagnostic checks define reproducible detection for the known gaps; a detected gap is not a conformance pass or a hand-check reference.

| Check | Executable recipe and numeric detection criterion | Observed result / scope |
| --- | --- | --- |
| D01 (G0) | compute_rocof([0,0],[60,59]) returns 0.0 Hz/s; compute_rocof([0,1],[60]) returns -60.0 Hz/s; detect([0,1],[],0,0) yields false; a detector with both thresholds -1 trips on stable input: reproduce 4/4 signatures (count). | DETECTED; input-quality failures, issue pending (2). [src/digitalmodel/power/microgrid/island_detector.py:L52-L58] [src/digitalmodel/power/microgrid/island_detector.py:L86-L97] [src/digitalmodel/power/microgrid/island_detector.py:L151-L160] |
| D02 (G0) | BESSController(0,-1,soc=2,soc_min=1,soc_max=3) is accepted and produces -1.0 kW discharge capability; update_soc then raises ZeroDivisionError; normal 100 kWh/50 kW battery with dt=-0.5 h raises SOC to 0.75; an oversized discharge clips to 0.1: reproduce 5/5 signatures (count). | DETECTED; issue pending (3). [src/digitalmodel/power/microgrid/bess_controller.py:L43-L63] [src/digitalmodel/power/microgrid/bess_controller.py:L81-L88] [src/digitalmodel/power/microgrid/bess_controller.py:L145-L148] |
| D03 (G2) | One 500 kW/zero-droop generator serving 250 kW returns 0 generation and 0 unserved; a 5 percent-droop counterpart's returned balance error exceeds the loop's 0.01 kW criterion; recommendations accept [1,1] shares: reproduce 3/3 signatures (count). | DETECTED; normal-case residual magnitude rounded upward is within 0.2 kW; issue pending (4). [src/digitalmodel/power/microgrid/droop_control.py:L108-L114] [src/digitalmodel/power/microgrid/droop_control.py:L269-L305] [src/digitalmodel/power/microgrid/droop_control.py:L331-L354] |
| D04 (G0) | Two 10 kW BESS assets with one 50 kW controller dispatch 100.0 kW; two 10 kW nonbattery assets with the same ID return a mapping totaling 10.0 kW for a 20.0 kW request: reproduce 2/2 signatures (count). | DETECTED; issue pending (5). [src/digitalmodel/power/microgrid/microgrid_ems.py:L73-L95] |

## 6. Known failure modes and pitfalls

sweep: 49 found

Sweep inventory: 49 distinct source sites or assumption groups below, including intentional zero branches, initializers and unit conventions; this is a source-site count, not a claim of 49 defects.
The complete package was searched with the following commands, followed by line-by-line reading of all six Python files:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|float\(|int\(|cache|kwh|kvar|_kw|_hz|hours|degrees' src/digitalmodel/power/microgrid --glob '*.py'
rg -n 'except|continue|return|0\.0|0\.05|0\.20|0\.30|60\.0|480\.0|zip|sum|clamp|unit|percent|fraction|currency' src/digitalmodel/power/microgrid --glob '*.py'
```

No exception handlers, external-text int/float conversions, epoch defaults or cache writes were found in the inspected six-file source inventory; collection initializers are retained in the rows where their output can be empty. [src/digitalmodel/power/microgrid/__init__.py:L1-L23] [src/digitalmodel/power/microgrid/bess_controller.py:L1-L148] [src/digitalmodel/power/microgrid/droop_control.py:L1-L354] [src/digitalmodel/power/microgrid/island_detector.py:L1-L161] [src/digitalmodel/power/microgrid/microgrid_ems.py:L1-L176] [src/digitalmodel/power/microgrid/models.py:L1-L139]

| Site | Symptom / cause | Existing detection or gap | Follow-up |
| --- | --- | --- | --- |
| S01 | Duplicate timestamps return a zero slope rather than invalid input. [src/digitalmodel/power/microgrid/island_detector.py:L94-L97] | D01 | issue pending (2) |
| S02 | Unequal series lengths silently truncate zip, while sums and sample count use different populations. [src/digitalmodel/power/microgrid/island_detector.py:L86-L97] | D01 | issue pending (2) |
| S03 | Timestamp ordering and non-finite measurements are not checked. [src/digitalmodel/power/microgrid/island_detector.py:L86-L97] | no detection check exists | issue pending (2) |
| S04 | Default thresholds are accepted without range checks; negative thresholds can trip stable input. [src/digitalmodel/power/microgrid/island_detector.py:L52-L58] [src/digitalmodel/power/microgrid/island_detector.py:L153-L154] | D01 | issue pending (2) |
| S05 | Angle inputs assume degrees and wrap modulo one revolution; radians are not detected. [src/digitalmodel/power/microgrid/island_detector.py:L99-L124] | C06 checks degree wrapping only; no detection check exists for unit misuse | issue pending (2) |
| S06 | Discharge capability returns zero at/below the effective minimum, intentionally. [src/digitalmodel/power/microgrid/bess_controller.py:L81-L88] | C04 includes battery zero-at-min case | none: guarded policy branch |
| S07 | Charge capability returns zero at/above maximum, intentionally. [src/digitalmodel/power/microgrid/bess_controller.py:L100-L106] | C04 includes battery zero-at-max case | none: guarded policy branch |
| S08 | Fixed derating band lacks independently established standards provenance. [src/digitalmodel/power/microgrid/bess_controller.py:L14-L15] | C03 source inventory; no conformance check exists | issue pending (1) |
| S09 | Fixed island reserve can exhaust the configured operating band. [src/digitalmodel/power/microgrid/bess_controller.py:L17-L18] [src/digitalmodel/power/microgrid/bess_controller.py:L81-L84] | C04 covers ordinary reserve boundary; no detection check exists for incompatible bounds | issue pending (3) |
| S10 | Initial SOC defaults to a configured policy value. [src/digitalmodel/power/microgrid/bess_controller.py:L47-L47] | C04 includes default-parameter case | none: explicit default |
| S11 | Minimum SOC default is configured rather than capacity-derived. [src/digitalmodel/power/microgrid/bess_controller.py:L48-L48] | C04 includes default-parameter case | none: explicit default |
| S12 | Maximum SOC default is configured rather than capacity-derived. [src/digitalmodel/power/microgrid/bess_controller.py:L49-L49] | C04 includes default-parameter case | none: explicit default |
| S13 | Capacity and power lack positive/finite guards; zero capacity divides by zero and negative power violates capability sign. [src/digitalmodel/power/microgrid/bess_controller.py:L51-L63] [src/digitalmodel/power/microgrid/bess_controller.py:L145-L146] | D02 | issue pending (3) |
| S14 | SOC bounds need only ordering, so values outside fractional physical limits pass. [src/digitalmodel/power/microgrid/bess_controller.py:L51-L63] | D02 | issue pending (3) |
| S15 | Hour-based duration can be negative and reverse energy flow. [src/digitalmodel/power/microgrid/bess_controller.py:L135-L148] | D02 | issue pending (3) |
| S16 | SOC clipping loses excess-step energy without a delivered-energy report. [src/digitalmodel/power/microgrid/bess_controller.py:L145-L148] | D02 | issue pending (3) |
| S17 | Zero governor droop returns zero active power although the rating still counts as capacity. [src/digitalmodel/power/microgrid/droop_control.py:L108-L114] [src/digitalmodel/power/microgrid/droop_control.py:L269-L275] | D03 | issue pending (4) |
| S18 | Zero AVR droop returns zero reactive power. [src/digitalmodel/power/microgrid/droop_control.py:L137-L143] | no detection check exists | issue pending (4) |
| S19 | Active output clamps to zero/rating, hiding out-of-curve demand. [src/digitalmodel/power/microgrid/droop_control.py:L110-L114] | C04 includes above/below-curve cases | none: explicit clamp |
| S20 | Reactive output clamps to zero/rating. [src/digitalmodel/power/microgrid/droop_control.py:L139-L143] | C09 and C04 reactive curve cases | none: explicit clamp |
| S21 | Governor nominal frequency defaults; rating/frequency finite and positive checks are absent. [src/digitalmodel/power/microgrid/droop_control.py:L30-L38] | C04 covers nominal defaults; no detection check exists for invalid rating/frequency | issue pending (4) |
| S22 | AVR nominal voltage defaults; rating/voltage guards are absent. [src/digitalmodel/power/microgrid/droop_control.py:L55-L63] | C04 covers default voltage; no detection check exists for invalid rating/voltage | issue pending (4) |
| S23 | Parallel sharing has a separate nominal-frequency default that can disagree with generators. [src/digitalmodel/power/microgrid/droop_control.py:L164-L183] | no detection check exists | issue pending (4) |
| S24 | _total_power_at_freq initializes zero and skips every isochronous generator, ignoring iso_load_kw despite its docstring. [src/digitalmodel/power/microgrid/droop_control.py:L146-L161] | no detection check exists | issue pending (4) |
| S25 | Nonpositive loads return zero power, percentages and shortage without distinguishing negative input. [src/digitalmodel/power/microgrid/droop_control.py:L192-L200] | C04 includes zero load; no detection check exists for negative input | issue pending (4) |
| S26 | Isochronous path initializes droop_total to zero and sums only non-isochronous generators. [src/digitalmodel/power/microgrid/droop_control.py:L220-L224] | C04 mixed-mode case | none: arithmetic initializer |
| S27 | Isochronous shortage starts at zero, changing only above its capacity. [src/digitalmodel/power/microgrid/droop_control.py:L226-L235] | C04 covers mixed mode; no detection check exists for mixed overload | issue pending (4) |
| S28 | Zero isochronous capacity falls back to zero share but then percentage divides by generator rating. [src/digitalmodel/power/microgrid/droop_control.py:L237-L253] | no detection check exists | issue pending (4) |
| S29 | Droop shortage starts at zero and depends on summed ratings, not feasible curve output. [src/digitalmodel/power/microgrid/droop_control.py:L269-L275] | D03 | issue pending (4) |
| S30 | Bisection has a fixed iteration cap, no convergence flag, and reports a different midpoint after checking residual. [src/digitalmodel/power/microgrid/droop_control.py:L277-L305] | D03 | issue pending (4) |
| S31 | Recommendation uses a fixed average and clamps settings without reporting split distortion. [src/digitalmodel/power/microgrid/droop_control.py:L348-L354] | C04 covers ordinary recommendations; no detection check exists for clamp distortion | issue pending (4) |
| S32 | Split length/positivity checks omit the documented sum-to-one requirement; empty lists divide by zero. [src/digitalmodel/power/microgrid/droop_control.py:L323-L354] | D03 detects nonunit sum; no detection check exists for empty list | issue pending (4) |
| S33 | DER availability defaults to true; capacity/priority validation does not enforce identity uniqueness or finite values. [src/digitalmodel/power/microgrid/models.py:L82-L97] | C01 for basic guards, D04 for duplicate IDs; no detection check exists for non-finite inputs | issue pending (5) |
| S34 | Snapshot unserved load defaults to zero regardless of the other powers. [src/digitalmodel/power/microgrid/models.py:L122-L128] | no detection check exists | issue pending (5) |
| S35 | Snapshot curtailment defaults to zero and is not computed by EMS. [src/digitalmodel/power/microgrid/models.py:L127-L128] [src/digitalmodel/power/microgrid/microgrid_ems.py:L145-L152] | no detection check exists | issue pending (5) |
| S36 | Snapshot only rejects nonpositive frequency; finite values and power consistency are unchecked. [src/digitalmodel/power/microgrid/models.py:L122-L139] | C01 rejects negative frequency; no detection check exists for remaining domains | issue pending (5) |
| S37 | EMS initially assumes grid-connected mode. [src/digitalmodel/power/microgrid/microgrid_ems.py:L46-L49] | C04 mode-transition case | none: explicit default |
| S38 | Standalone dispatch defaults to grid-mode battery policy independently of stored EMS mode. [src/digitalmodel/power/microgrid/microgrid_ems.py:L51-L55] [src/digitalmodel/power/microgrid/microgrid_ems.py:L86-L89] | no detection check exists | issue pending (5) |
| S39 | Empty asset collection produces an empty mapping without a shortage field. [src/digitalmodel/power/microgrid/microgrid_ems.py:L77-L95] | no detection check exists | issue pending (5) |
| S40 | Demand already met causes zero dispatch and continue, rather than charging excess generation. [src/digitalmodel/power/microgrid/microgrid_ems.py:L80-L82] | C04 cheapest-first case | none: intentional demand-limited policy |
| S41 | Unavailable resource causes zero dispatch and continue. [src/digitalmodel/power/microgrid/microgrid_ems.py:L83-L85] | C04 unavailable-resource case | none: intentional availability policy |
| S42 | Every BESS gets the same controller capability without an individual asset-rating cap. [src/digitalmodel/power/microgrid/microgrid_ems.py:L86-L94] | D04 | issue pending (5) |
| S43 | Duplicate IDs overwrite a previous dispatch entry after remaining demand has been reduced. [src/digitalmodel/power/microgrid/microgrid_ems.py:L92-L95] | D04 | issue pending (5) |
| S44 | Unserved power is clipped at zero, so excess generation is not represented here. [src/digitalmodel/power/microgrid/microgrid_ems.py:L138-L139] | no detection check exists | issue pending (5) |
| S45 | Islanded import is forced to zero; connected import assumes all positive shortfall is supplied. [src/digitalmodel/power/microgrid/microgrid_ems.py:L140-L140] | no detection check exists | issue pending (5) |
| S46 | Connected unserved load is overwritten with zero without a utility-capacity input. [src/digitalmodel/power/microgrid/microgrid_ems.py:L141-L142] | no detection check exists | issue pending (5) |
| S47 | Empty frequency measurements fall back to nominal frequency after malformed detection can pass. [src/digitalmodel/power/microgrid/microgrid_ems.py:L144-L150] [src/digitalmodel/power/microgrid/island_detector.py:L86-L97] | D01 detects the empty-series precursor; no detection check exists for the complete EMS fallback | issue pending (5) |
| S48 | Black start uses a fixed SOC threshold and only sets a mode flag. [src/digitalmodel/power/microgrid/microgrid_ems.py:L23-L24] [src/digitalmodel/power/microgrid/microgrid_ems.py:L154-L169] | C04 black-start threshold cases | issue pending (1) for provenance; issue pending (5) for sequencing |
| S49 | A control cycle has no elapsed-time input and does not integrate SOC; reconnect only assigns islanded mode. [src/digitalmodel/power/microgrid/microgrid_ems.py:L97-L104] [src/digitalmodel/power/microgrid/microgrid_ems.py:L125-L176] | C11 records R4 mode semantics; no detection check exists for sustained energy balance | issue pending (5) |

The battery discharge test docstring describes a different duration from its executable body; V3 follows the executable 0.5 h step and 0.25 SOC assertion, not the docstring; issue pending (3). [tests/power/microgrid/test_bess_controller.py:L105-L115]

No incident issue was supplied for this module; read-only issue searches for microgrid, droop, islanding and BESS returned no matching incident during this audit. Existing tests therefore supply the cited lessons, while the six issue drafts retain the newly recorded gaps.

## 7. Lessons log

This is the initial authored log; no previous shared-log record is asserted.

| Date | Entry | Lesson / pack change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack | Separate source-code arithmetic checks from claims of standards conformance; mark clause provenance open. | [src/digitalmodel/power/microgrid/island_detector.py:L41-L58] | Pending, issue pending (1) |
| 2026-10-18 | Initial pack | Record malformed-sample fallbacks before treating stable output as a healthy grid. | [src/digitalmodel/power/microgrid/island_detector.py:L86-L97] | Pending, issue pending (2) |
| 2026-10-18 | Initial pack | Check the returned generator balance, because the loop checks a different midpoint. | [src/digitalmodel/power/microgrid/droop_control.py:L281-L305] | Pending, issue pending (4) |
| 2026-10-18 | Initial pack | Check resource identities and per-asset/controller limits before summing dispatch. | [src/digitalmodel/power/microgrid/microgrid_ems.py:L73-L95] | Pending, issue pending (5) |

## 8. Ownership

Proposed clearance roles for this draft: engineering calculations quality manager; deputy engineering calculations quality manager, designation pending under issue pending (6).
Proposed producing discipline: electrical engineering calculations; ambiguities in methods, tolerances and standard clauses go to the family quality manager, and cross-family coverage questions go to the maintainers (AQG r0, section 11, “Sign-off and independence”).
No clearance or completed second-provider review is asserted by DRAFT status.

## 9. Dependencies and implementation inventory

Runtime dependencies are Python dataclasses/enums and this package's own models, battery controller and detector; the inspected module imports do not invoke a solver or reference an external dataset. [src/digitalmodel/power/microgrid/models.py:L14-L17] [src/digitalmodel/power/microgrid/droop_control.py:L11-L13] [src/digitalmodel/power/microgrid/island_detector.py:L11-L13] [src/digitalmodel/power/microgrid/microgrid_ems.py:L12-L21] [src/digitalmodel/power/microgrid/__init__.py:L5-L13]
MicrogridEMS calls IslandDetector and BESSController and returns MicrogridState; tests import the same package APIs, while droop tests import the separate droop module. [src/digitalmodel/power/microgrid/microgrid_ems.py:L125-L152] [tests/power/microgrid/test_microgrid_ems.py:L7-L14] [tests/power/microgrid/test_droop_control.py:L7-L15]

### Map census and counted-family audit

Authored census record: map ref `origin/codex/implmap-work`, map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, map data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; package prefix `src/digitalmodel/power/microgrid/` at this pack's code pin.
The census traversed all 13 families and their 2348 implementation rows, selecting rows whose path starts with that prefix: 0 rows and 0 module map families.
The implementation inventory is therefore empty; A8 inventory PASS and counted-family grep NOT_APPLICABLE because no family has an in-package implementation row.
No family is ranked or counted, and no search signal is omitted on a path-vocabulary match, because the counted-family set is empty.
The map's rejected-candidate list contains _solve_with_isochronous and _solve_droop_only in external-solver-batch-run-stacks; these are expressly excluded candidates, not implementation rows, and section 3 already describes their electrical algorithms. [src/digitalmodel/power/microgrid/droop_control.py:L209-L306]
Reproduction recipe for the census, using the saved map from the named ref:

```python
import yaml
m = yaml.safe_load(map_text)
prefix = "src/digitalmodel/power/microgrid/"
module_families = [(f, [r for r in f["implementations"]
                       if r["path"].startswith(prefix)])
                   for f in m["families"]]
module_families = [(f, rows) for f, rows in module_families if rows]
counted = sorted(module_families, key=lambda fr: -len(fr[1]))[:3]
```

### Whole-repository search and map follow-up

The supplementary search covered all tracked Python files, including scripts, with case-sensitive patterns and definition-level inspection:

```sh
git grep -n -E 'microgrid|rocof|parallel_load_sharing|recommend_droop_settings|DERATING_BAND|ISLAND_RESERVE|BLACK_START_MIN_SOC' -- '*.py'
git grep -n -E 'def (power_at_frequency|reactive_at_voltage|compute_rocof|compute_vector_shift|max_discharge_kw|max_charge_kw|update_soc|merit_order_dispatch)|rated_power.*delta_f|sum_tf.*sum_f' -- '*.py'
```

Audit result: hits outside this package are its tests and the power-domain description; 0 further computing definitions were found outside the package by these searches.
Map follow-up: battery energy/SOC limits, frequency/angle islanding detection, active/reactive droop, parallel sharing, droop recommendations and merit-order dispatch need appropriate map families before they become counted; issue pending (6).
This supplemental finding does not turn rejected external-solver candidates into a counted family.

## 10. Coverage and open items

Read inventory: the complete six-file package (__init__.py, models.py, bess_controller.py, island_detector.py, microgrid_ems.py and droop_control.py), all five test modules plus the test package initializer, and the power registry entry. [src/digitalmodel/power/microgrid/__init__.py:L1-L23] [src/digitalmodel/power/microgrid/models.py:L1-L139] [src/digitalmodel/power/microgrid/bess_controller.py:L1-L148] [src/digitalmodel/power/microgrid/island_detector.py:L1-L161] [src/digitalmodel/power/microgrid/microgrid_ems.py:L1-L176] [src/digitalmodel/power/microgrid/droop_control.py:L1-L354] [tests/power/microgrid/test_models.py:L1-L90] [tests/power/microgrid/test_bess_controller.py:L1-L122] [tests/power/microgrid/test_island_detector.py:L1-L102] [tests/power/microgrid/test_microgrid_ems.py:L1-L127] [tests/power/microgrid/test_droop_control.py:L1-L263] [docs/registry/module-routing.yaml:L295-L307]
No package README or docs existed at the code pin; documentation discovery under docs found the neutral operator map and 1 discovery document with an identifying path, omitted here for codenaming under issue pending (6); the operator map describes microgrid EMS under power. [docs/maps/digitalmodel-operator-map.md:L57-L57]
All five module test files were run; there were no licensed-binary, private-fixture or optional-package exclusions in the 71-case result recorded in section 4.
The calculation-citation contract was readable and calls for Citation evidence for standards-derived inputs; no such sidecar is emitted on the inspected battery/detector calculation paths. [src/digitalmodel/power/microgrid/bess_controller.py:L65-L148] [src/digitalmodel/power/microgrid/island_detector.py:L126-L161]

| Open item | Coverage / required closure | Proposed responsible role | Tracking |
| --- | --- | --- | --- |
| Standards evidence | Resolve the single UNVERIFIED claim in section 3: IEEE clause attribution, method compliance and default-setting provenance; establish Citation sidecars and independently sourced protection/dispatch benchmarks. | Engineering calculations quality manager | issue pending (1) |
| Detector input quality | Cover non-finite/order/length/degenerate-time and threshold boundaries, including inclusive comparison versus docstring wording. [src/digitalmodel/power/microgrid/island_detector.py:L86-L97] [src/digitalmodel/power/microgrid/island_detector.py:L151-L160] | Electrical calculations maintainer | issue pending (2) |
| Battery physical domains | Add positive finite capacity/rating, physical SOC, duration and clipped-energy evidence. [src/digitalmodel/power/microgrid/bess_controller.py:L43-L63] [src/digitalmodel/power/microgrid/bess_controller.py:L145-L148] | Electrical calculations maintainer | issue pending (3) |
| Droop feasibility | Resolve zero-droop semantics, returned residual, empty/invalid configurations, unused helper argument and recommendation constraints. [src/digitalmodel/power/microgrid/droop_control.py:L146-L161] [src/digitalmodel/power/microgrid/droop_control.py:L269-L354] | Electrical calculations maintainer | issue pending (4) |
| EMS accounting | Resolve shared-controller/asset identity, time/energy progression, grid-supply assumptions, snapshot consistency and state-only sequencing limits. [src/digitalmodel/power/microgrid/microgrid_ems.py:L73-L176] [src/digitalmodel/power/microgrid/models.py:L122-L139] | Electrical calculations maintainer | issue pending (5) |
| Knowledge infrastructure | Add the section-3 method families to the map, provide a neutral discovery-document reference, designate the deputy role, retain a durable input-fingerprint evidence record and register the initial lessons. | Maintainers and engineering calculations quality manager | issue pending (6) |

No client-derived value or private dataset was needed by the inspected test fixtures; no private sidecar entry is asserted. [tests/power/microgrid/test_microgrid_ems.py:L17-L26] [tests/power/microgrid/test_droop_control.py:L144-L152]
