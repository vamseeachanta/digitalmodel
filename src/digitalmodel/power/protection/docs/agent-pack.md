# Protection relay coordination knowledge pack

| Header | Value |
| --- | --- |
| Module | power/protection |
| Repository | digitalmodel |
| Path | `src/digitalmodel/power/protection` |
| Pack revision | 1, 2026-10-27 |
| Code revision | `2aa19e88306d9b4294e49a2b7849b07ae036b205` |
| Analysis family | Engineering calculations (EC); AQG r0, section 10, Python calculations |
| Quality role | Engineering calculations family quality manager |
| Status | DRAFT; independent review pending |

## 1. Purpose and scope

The package provides overcurrent settings, differential settings, an inverse-time operating-time calculation, a two-relay grading-margin check and a time-current intersection search. [src/digitalmodel/power/protection/relay_coordination.py:L76-L194] [src/digitalmodel/power/protection/relay_coordination.py:L200-L327]

`DifferentialRelay` stores settings and validates slope and minimum operate current; it does not calculate a differential operating criterion. `coordination_check` and `time_current_intersection` use pickup, time dial and curve type, without using `relay_type` or `instantaneous_pickup_A`; do not treat their outputs as instantaneous-element or differential trip predictions. [src/digitalmodel/power/protection/relay_coordination.py:L114-L144] [src/digitalmodel/power/protection/relay_coordination.py:L228-L246] [src/digitalmodel/power/protection/relay_coordination.py:L283-L327]

The containing power domain is registered as beta; that label is a routing maturity designation, not independent numerical qualification of this package. [docs/registry/module-routing.yaml:L333-L338]

### Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-26 | Document calculation boundaries, regression evidence and qualification gaps. |
| 1 | 2026-10-27 | Retain offline H1 and D1–D3 probes with source and script digests so the diagnostic record can be repeated. |

## 2. Inputs and outputs

### Inputs

| Interface / field | Unit / convention | Allowed range actually checked | Default | Source |
| --- | --- | --- | --- | --- |
| `OvercurrentRelay.relay_id`, `relay_type`, `curve_type` | Identifier; `RelayType` and `CurveType` enum selections | No runtime type or enum-membership validation | Required | [src/digitalmodel/power/protection/relay_coordination.py:L24-L60] [src/digitalmodel/power/protection/relay_coordination.py:L96-L111] |
| `OvercurrentRelay.pickup_current_A` | A | Rejects values <= 0; no explicit finiteness check | Required | [src/digitalmodel/power/protection/relay_coordination.py:L88-L111] |
| `OvercurrentRelay.time_dial_setting` | Raw time dial; definite-time output uses the same number in s | Rejects values <= 0; no explicit finiteness check | Required | [src/digitalmodel/power/protection/relay_coordination.py:L90-L111] [src/digitalmodel/power/protection/relay_coordination.py:L173-L194] |
| `OvercurrentRelay.instantaneous_pickup_A` | A | Stored without range validation | `None` (element unspecified) | [src/digitalmodel/power/protection/relay_coordination.py:L92-L111] |
| `DifferentialRelay.relay_id` | Identifier | No runtime validation | Required | [src/digitalmodel/power/protection/relay_coordination.py:L130-L144] |
| `DifferentialRelay.slope_percent` | % | `0 < slope_percent <= 100` | Required | [src/digitalmodel/power/protection/relay_coordination.py:L122-L144] |
| `DifferentialRelay.minimum_operate_current_A` | A | Rejects values <= 0; no explicit finiteness check | Required | [src/digitalmodel/power/protection/relay_coordination.py:L124-L144] |
| `DifferentialRelay.restraint_current_A` | A | No range or finiteness validation | Required | [src/digitalmodel/power/protection/relay_coordination.py:L126-L144] |
| `inverse_time_curve(fault_current_A, pickup_current_A, time_dial_setting, curve_type)` | Fault-current magnitude and pickup in A; output s | Only `fault_current_A > pickup_current_A` explicitly checked, including definite time; direct calls do not enforce positive pickup or time dial | All required | [src/digitalmodel/power/protection/relay_coordination.py:L150-L194] |
| `coordination_check(upstream, downstream, fault_current_A, min_margin_s)` | Backup upstream minus primary downstream time, s; same fault magnitude in A for both | Curve function enforces fault above each pickup; margin sign/range not separately checked | `min_margin_s = 0.3` s; relays and current required | [src/digitalmodel/power/protection/relay_coordination.py:L200-L246] |
| `time_current_intersection(relay1, relay2, current_range)` | `(low, high)` in A | Lower bound silently raised to maximum pickup multiplied by the factor at line 286; non-increasing effective range returns `None` | Relays and range required | [src/digitalmodel/power/protection/relay_coordination.py:L252-L289] |
| `tolerance`, `max_iterations` | A; iterations (count) | Neither positivity nor finiteness checked; additional time-residual termination exists | `1e-3` A; 100 iterations | [src/digitalmodel/power/protection/relay_coordination.py:L252-L282] [src/digitalmodel/power/protection/relay_coordination.py:L309-L327] |

These interfaces consume scalar magnitudes and relay settings; they define no spatial coordinate frame, origin, phase convention or file format, and perform no unit conversion. [src/digitalmodel/power/protection/relay_coordination.py:L150-L194] [src/digitalmodel/power/protection/relay_coordination.py:L200-L246] [src/digitalmodel/power/protection/relay_coordination.py:L252-L327]

### Outputs

| Interface | Output / unit | Failure or absence route | Source |
| --- | --- | --- | --- |
| Relay constructors | Settings instances | `ValueError` for the explicitly guarded ranges | [src/digitalmodel/power/protection/relay_coordination.py:L96-L144] |
| `inverse_time_curve` | Operating time, float, s | `ValueError` at or below pickup; no structured status | [src/digitalmodel/power/protection/relay_coordination.py:L173-L194] |
| `coordination_check` | `pass` boolean; `margin_s`, `upstream_time_s`, `downstream_time_s` floats in s | Propagates curve errors; passes when upstream minus downstream is at least requested margin | [src/digitalmodel/power/protection/relay_coordination.py:L228-L246] |
| `time_current_intersection` | `(intersection_current_A, operating_time_s)` or `None` | `None` for invalid effective interval, same-sign endpoint differences or exhausted iterations; no reason code | [src/digitalmodel/power/protection/relay_coordination.py:L283-L327] |

## 3. Method and governing standards

| Method | Implementation and reference boundary |
| --- | --- |
| Curve selection | `CurveType` includes moderately, very and extremely inverse curves, plus definite time; the coefficient tuples remain in code and are not reproduced here. [src/digitalmodel/power/protection/relay_coordination.py:L42-L70] |
| Inverse-time calculation | `inverse_time_curve` evaluates the symbolic equation documented in its docstring using `_CURVE_CONSTANTS`; definite time returns the time dial after the pickup guard. [src/digitalmodel/power/protection/relay_coordination.py:L150-L194] |
| Governing reference | The module names IEEE C37.112, *IEEE Standard Inverse-Time Characteristic Equations for Overcurrent Relays*, 2018; its reference block supplies no clause or table locator. Clause text not checked against the licensed copy. [src/digitalmodel/power/protection/relay_coordination.py:L1-L10] |
| Coordination | `coordination_check` delegates both operating-time calculations to `inverse_time_curve`, subtracts downstream from upstream, and compares the difference inclusively with `min_margin_s`; no separate standard is cited for the default margin. [src/digitalmodel/power/protection/relay_coordination.py:L200-L246] |
| Intersection | `time_current_intersection` bisects the difference of two delegated curve evaluations; success occurs on time residual or interval half-width, and returns relay1's time. [src/digitalmodel/power/protection/relay_coordination.py:L283-L327] |
| Differential settings | `DifferentialRelay.__post_init__` performs range checks only; no slope operating equation is implemented. [src/digitalmodel/power/protection/relay_coordination.py:L114-L144] |
| Calculation citations | The implementation returns a float, a dictionary or a tuple/`None`; no `Citation` sidecar is emitted by these return routes. [src/digitalmodel/power/protection/relay_coordination.py:L189-L194] [src/digitalmodel/power/protection/relay_coordination.py:L240-L246] [src/digitalmodel/power/protection/relay_coordination.py:L313-L327] |

UNVERIFIED: U1 — conformance of the coefficient provenance and equation to a specific IEEE C37.112-2018 clause/table, including an independently sourced numerical benchmark; issue pending (4).

## 4. Validated worked examples

Validated examples: **0**; the three numerical inverse-time tests repeat the code's coefficient/equation basis, the definite-time test checks the implemented branch, and the coordination and crossing tests check internal consistency rather than an independently cited benchmark. [tests/power/protection/test_relay_coordination.py:L148-L194] [tests/power/protection/test_relay_coordination.py:L218-L248] [tests/power/protection/test_relay_coordination.py:L289-L313]

Regression examples: **6**, listed below; input settings are inline in the cited tests, with no external input file. Expected inverse-time coefficient values are intentionally omitted from this pack. [tests/power/protection/test_relay_coordination.py:L151-L194] [tests/power/protection/test_relay_coordination.py:L218-L248] [tests/power/protection/test_relay_coordination.py:L290-L313]

Offline execution record, 2026-10-26: BASE `2aa19e88306d9b4294e49a2b7849b07ae036b205`, Python 3.12.15, pytest 8.4.2; the PASS column is the producing check record, not independent engineering clearance.

| Case | Expected result, unit and tolerance basis | Existing test node ID | Offline result BASE / after | Source |
| --- | --- | --- | --- | --- |
| E1 moderately inverse | Test-local repeated formula, s; relative tolerance `1e-6`, chosen by the test | `tests/power/protection/test_relay_coordination.py::TestInverseTimeCurve::test_moderately_inverse_known_value` | PASS / PASS | [tests/power/protection/test_relay_coordination.py:L151-L161] |
| E2 very inverse | Test-local repeated formula, s; relative tolerance `1e-6`, chosen by the test | `tests/power/protection/test_relay_coordination.py::TestInverseTimeCurve::test_very_inverse_known_value` | PASS / PASS | [tests/power/protection/test_relay_coordination.py:L163-L173] |
| E3 extremely inverse | Test-local repeated formula, s; relative tolerance `1e-6`, chosen by the test | `tests/power/protection/test_relay_coordination.py::TestInverseTimeCurve::test_extremely_inverse_known_value` | PASS / PASS | [tests/power/protection/test_relay_coordination.py:L175-L185] |
| E4 definite time | Time equals input time dial, 0.3 s; `pytest.approx` default tolerance, not a published tolerance | `tests/power/protection/test_relay_coordination.py::TestInverseTimeCurve::test_definite_time_returns_tds` | PASS / PASS | [tests/power/protection/test_relay_coordination.py:L187-L194] |
| E5 coordination | Boolean true; margin >= 0.3 s and upstream time greater than downstream; no separate numerical accuracy assertion | `tests/power/protection/test_relay_coordination.py::TestCoordinationCheck::test_coordination_pass` | PASS / PASS | [tests/power/protection/test_relay_coordination.py:L218-L248] |
| E6 crossing | Non-`None` pair, A and s; both delegated curve times agree at relative tolerance `1e-2`, as does returned time; no independent current reference | `tests/power/protection/test_relay_coordination.py::TestTimecurrentIntersection::test_intersection_found` | PASS / PASS | [tests/power/protection/test_relay_coordination.py:L290-L313] |

Reproduction form: `PYTHONPATH=src uv run --no-sources python -m pytest tests/power/protection/ tests/docs/test_digitalmodel_routing_contract.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/bt-run"`; the executable is resolved explicitly in the execution record, no addopts override or test deselection is used. BASE and after: each 33 run, 33 passed (23 relay, 10 routing), 0 failed, 0 skipped, 0 NOT_RUN in this command. Both runs use the header code pin; the after run includes only documentation/registry edits. No test file or calculation code changed.

## 5. QA gates

Required gate allocation: AQG r0, *Analysis QA gates*, revision 0, section 10, Python calculations; G5 applies under sections 9–10. The criteria below are pack check prescriptions; passing a regression or reproducing a defect does not establish IEEE conformance.

| Gate | Applicability / module route | Checks |
| --- | --- | --- |
| G0 | Applicable: scalar relay inputs, units, reference traceability | C01, C02, D1 |
| G1 | Applicable: Python imports and pytest; no target-program deck is emitted by these functions. [src/digitalmodel/power/protection/relay_coordination.py:L150-L194] [src/digitalmodel/power/protection/relay_coordination.py:L200-L327] | C03, C04, D2 |
| G2 | Applicable: hand subtraction plus regression and boundary diagnostics | C05, H1, C06, D3 |
| G3 | NOT_APPLICABLE: functions evaluate operating time and margins or bisect a scalar difference; no dynamics integration is implemented. [src/digitalmodel/power/protection/relay_coordination.py:L150-L194] [src/digitalmodel/power/protection/relay_coordination.py:L200-L327] | 0 dynamics checks |
| G4 | NOT_APPLICABLE: the scalar calculation interfaces have no mesh, diffraction body or hydrodynamic coefficients. [src/digitalmodel/power/protection/relay_coordination.py:L150-L194] [src/digitalmodel/power/protection/relay_coordination.py:L200-L327] | 0 diffraction checks |
| G5 | Applicable: retain the input pin, test-node results, citations and limitation dispositions with any use of these calculations; independent clearance remains required (AQG r0, sections 9–11) | C07 |

| Check | Gate | Executable check / numeric criterion and unit | Evidence and present disposition |
| --- | --- | --- | --- |
| C01 | G0 | Run the five invalid-setting tests: negative pickup, zero pickup, negative time dial, slope out of range and negative minimum operate current; 5 expected `ValueError` outcomes, 0 unexpected outcomes (test cases). | PASS at BASE and after; range guards only, not finite-input qualification. [tests/power/protection/test_relay_coordination.py:L79-L107] [tests/power/protection/test_relay_coordination.py:L124-L142] |
| C02 | G0 | Read every section-2 field against signatures/docstrings; require 0 undocumented unit assumptions (fields), specifically A for current and s for returned times/margins; raw time-dial semantics must remain explicit. | Manual unit audit; unit conversion is absent and time-dial convention requires follow-up (4). [src/digitalmodel/power/protection/relay_coordination.py:L80-L101] [src/digitalmodel/power/protection/relay_coordination.py:L118-L133] [src/digitalmodel/power/protection/relay_coordination.py:L162-L176] [src/digitalmodel/power/protection/relay_coordination.py:L270-L280] |
| C03 | G1 | Run `tests/power/protection/` by the section-4 command; 23 passed, 0 failed/skipped/NOT_RUN (test cases). Collection exercises the explicit Python import route. | PASS at BASE and after. [tests/power/protection/test_relay_coordination.py:L14-L22] [tests/power/protection/test_relay_coordination.py:L28-L332] |
| C04 | G1 | Run `tests/docs/test_digitalmodel_routing_contract.py`; require 10 passes and 0 failures (test cases); compare parsed registry before/after with exactly 1 added submodule and 0 changed existing entries (mappings). | PASS at BASE and after for tests; parsed mapping comparison confirms only the new protection entry. [tests/docs/test_digitalmodel_routing_contract.py:L46-L150] |
| C05 | G2 | Run E1–E3; require 3 repeated-formula comparisons within `1e-6` relative error (dimensionless), returned times in s. | PASS at BASE and after; regression only; do not use as independent standard validation. [tests/power/protection/test_relay_coordination.py:L151-L185] |
| H1 | G2 | Hand check with definite-time relays: pickup 100.0 A, fault 500.0 A, time dials 2.0 s and 0.5 s; calculate `2.0 - 0.5` outside the module, compare `coordination_check()['margin_s']` with 1.5 s within `1e-12` s; require `pass` true at the default margin. | PASS in the retained H1 probe below; these synthetic settings use the existing fixture values and definite-time branch; this is not a shipped worked-example node. [tests/power/protection/test_relay_coordination.py:L187-L194] [tests/power/protection/test_relay_coordination.py:L218-L248] [src/digitalmodel/power/protection/relay_coordination.py:L189-L194] [src/digitalmodel/power/protection/relay_coordination.py:L240-L246] |
| C06 | G2 | Run E6 and `tests/power/protection/test_relay_coordination.py::TestTimecurrentIntersection::test_no_intersection_returns_none`; require 2 passes (test cases), crossing time agreement within `1e-2` relative error, same-curve/different-time-dial case returns `None` (1 absent result). | PASS at BASE and after; boundary accuracy is not established by these assertions. [tests/power/protection/test_relay_coordination.py:L290-L332] |
| D1 | G0 | Diagnostic reproduction, not an input-acceptance gate: demonstrate 5 unguarded cases (NaN overcurrent pickup, NaN differential minimum, negative direct definite time, negative restraint and negative requested margin); require 5 observed reproductions (cases) and link each limitation to draft 1. | Reproduced; correct physical input rejection remains open. [src/digitalmodel/power/protection/relay_coordination.py:L103-L111] [src/digitalmodel/power/protection/relay_coordination.py:L130-L144] [src/digitalmodel/power/protection/relay_coordination.py:L183-L194] [src/digitalmodel/power/protection/relay_coordination.py:L228-L246] |
| D2 | G1 | Attribute-read audit: check the 2 relay calculation functions for 0 reads of `relay_type` or `instantaneous_pickup_A`; inspect `DifferentialRelay` for 0 operating-result methods (methods); changing instantaneous setting must leave the coordination dictionary equal (0 differing keys). | Reproduced/documented capability boundary; draft 2 holds missing behavior. [src/digitalmodel/power/protection/relay_coordination.py:L114-L144] [src/digitalmodel/power/protection/relay_coordination.py:L228-L246] [src/digitalmodel/power/protection/relay_coordination.py:L283-L327] |
| D3 | G2 | Diagnostic audit/reproduction: 6 conditions (lower-bound clipping, empty effective interval, no sign change, 0 iterations, coincident curves, exact lower-endpoint root), 0 undocumented outcomes (conditions). Reproduce endpoint loss with a very-inverse relay and a definite-time relay set to its value at 150.0 A over 150.0–5000.0 A; flag returned current > 4999.9 A as a defect, not convergence. | Reproduced; inspect termination at the time-residual or current-width branch separately; no independent current-error guarantee. [src/digitalmodel/power/protection/relay_coordination.py:L283-L327] [tests/power/protection/test_relay_coordination.py:L290-L332] |
| C07 | G5 | Match E1–E6 node IDs against retained pytest output at the code pin; require 6/6 paired BASE/after results, 0 missing records (cases), and 0 unresolved ranged citations (citations). Preserve diagnostics and issue dispositions with the evidence. | PASS: BASE and after have 6/6 records; every ranged citation resolves at its stated pin. [tests/power/protection/test_relay_coordination.py:L151-L194] [tests/power/protection/test_relay_coordination.py:L239-L248] [tests/power/protection/test_relay_coordination.py:L290-L313] |

### Retained diagnostic evidence

The offline [probe script](../../../../../scripts/validation/protection_relay_probe.py) retains H1 and D1–D3 assertions. The [evidence record](evidence/protection-relay-probes.json) pins the source revision and SHA-256 digests of both the calculation file and the script. The script rejects calculation bytes that differ from the pin and compares its deterministic output byte for byte with the retained record. Displayed results use one decimal place; comparisons use unrounded values. These diagnostics reproduce the documented limitations and do not add independently validated examples.

```sh
PYTHONPATH=src python scripts/validation/protection_relay_probe.py --check src/digitalmodel/power/protection/docs/evidence/protection-relay-probes.json
```

H1 retains the literal hand-calculated expectation **1.5 s**, independent of the subtraction inside `coordination_check`. D3 records every relay field, current range and search control. Its exact lower-endpoint case uses a very-inverse relay with 100.0 A pickup and time dial 1.0, and a definite-time relay with 100.0 A pickup whose time dial is exactly `inverse_time_curve(150.0, 100.0, 1.0, CurveType.VERY_INVERSE)`. Both use `OVERCURRENT_51` with no instantaneous setting. The range is 150.0–5000.0 A, tolerance is `1e-3` A and the iteration limit is 100. The probe explicitly identifies the returned current above 4999.9 A as endpoint loss, not successful root convergence.

## 6. Known failure modes and pitfalls

sweep: 14 found

Sweep record, 2026-10-26: 14 distinct findings/dispositions below, including deliberate defaults and absence sentinels; this is not a count of confirmed engineering defects. The raw grep has 20 matching lines, including guard comparisons and repeated unit docstrings. Inspection additionally includes all signatures, return routes and external-input assumptions. [src/digitalmodel/power/protection/relay_coordination.py:L76-L327]

Exact discovery command (followed by a complete read of both package Python files):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|\b(?:int|float)\(|=\s*0(?:\.0)?\b|1970|0\.05|5\s*%|1\.01|\[A\]|\[s\]|\[%\]|cache|write' src/digitalmodel/power/protection --glob '*.py'
```

| ID | Symptom / cause, with source at the code pin | Detection / disposition | Tracking |
| --- | --- | --- | --- |
| F01 | Non-finite overcurrent pickup/time dial bypass a simple `<= 0` rejection; no finite check. [src/digitalmodel/power/protection/relay_coordination.py:L103-L111] | D1 reproduces NaN pickup; C01 covers ordinary zero/negative values only; no comprehensive finite-value rejection test exists. | issue pending (1) |
| F02 | Differential minimum can accept NaN or positive infinity; restraint has no validation. Slope has the bounded comparison, which rejects NaN. [src/digitalmodel/power/protection/relay_coordination.py:L130-L144] | D1 reproduces NaN minimum and negative restraint; C01 tests a negative minimum and out-of-range slope only. | issue pending (1) |
| F03 | Direct `inverse_time_curve` calls need not pass the constructor guards; negative definite time can be returned, and zero pickup can reach division for inverse curves. [src/digitalmodel/power/protection/relay_coordination.py:L183-L194] | D1 reproduces negative time; no comprehensive direct-call input rejection test exists. | issue pending (1) |
| F04 | Instantaneous pickup defaults to `None` and is only stored; `relay_type` and the instantaneous threshold do not alter either calculation route. [src/digitalmodel/power/protection/relay_coordination.py:L96-L111] [src/digitalmodel/power/protection/relay_coordination.py:L228-L246] [src/digitalmodel/power/protection/relay_coordination.py:L283-L327] | D2 attribute audit and equality probe; the instantaneous-pickup test checks storage, not tripping. [tests/power/protection/test_relay_coordination.py:L68-L77] | issue pending (2) |
| F05 | Differential settings have no operating criterion or current-comparison method. [src/digitalmodel/power/protection/relay_coordination.py:L114-L144] | D2 method audit; constructor tests do not demonstrate differential protection. [tests/power/protection/test_relay_coordination.py:L113-L142] | issue pending (2) |
| F06 | Coordination margin defaults to 0.3 s without a cited engineering basis; negative requested margins are accepted. [src/digitalmodel/power/protection/relay_coordination.py:L200-L246] | D1 reproduces acceptance; E5 tests the configured margin only. | issue pending (1) |
| F07 | Intersection search silently increases the low current boundary; roots between pickup and the adjusted boundary are excluded. [src/digitalmodel/power/protection/relay_coordination.py:L283-L286] | D3 reads the clamp; no shipped near-pickup clipping test exists. | issue pending (3) |
| F08 | `return None` at line 289 conflates an empty effective range with lack of crossing. [src/digitalmodel/power/protection/relay_coordination.py:L283-L289] | D3 empty-interval reproduction; no status/reason-code test exists. | issue pending (3) |
| F09 | `return None` at line 307 uses endpoint signs only; it is a bracketing decision, not proof that all possible crossings were searched. [src/digitalmodel/power/protection/relay_coordination.py:L302-L307] | C06 covers the same-curve non-crossing case; D3 records the endpoint-sign route; no general completeness test exists. | issue pending (3) |
| F10 | `return None` at line 327 also means iteration exhaustion; a zero or negative iteration budget enters no loop. [src/digitalmodel/power/protection/relay_coordination.py:L309-L327] | D3 zero-iteration reproduction; no iteration-status test exists. | issue pending (3) |
| F11 | A zero endpoint difference is not returned before bisection; the strict sign-update branch can discard an exact endpoint root. Identical definite curves instead return a midpoint without identifying non-uniqueness. [src/digitalmodel/power/protection/relay_coordination.py:L302-L325] | D3 endpoint and coincident-curve probes; E6 does not exercise either case. | issue pending (3) |
| F12 | Current tolerance defaults to `1e-3` A and the budget to 100 iterations, but the alternative `1e-6` s residual can terminate first; neither argument is validated. [src/digitalmodel/power/protection/relay_coordination.py:L252-L282] [src/digitalmodel/power/protection/relay_coordination.py:L309-L327] | D3 termination audit; C06 checks relative time agreement, not current-error accuracy. | issue pending (3) |
| F13 | Plain floats are assumed to mean A, s or %, with no conversions, reference frame or phase handling; definite time treats raw time dial as seconds. [src/digitalmodel/power/protection/relay_coordination.py:L80-L101] [src/digitalmodel/power/protection/relay_coordination.py:L118-L133] [src/digitalmodel/power/protection/relay_coordination.py:L162-L194] | C02 unit audit; no detection check exists for mislabeled caller units. | issue pending (1) |
| F14 | Curve constants feed numeric output without a clause-level citation sidecar; repeated-formula tests can preserve the same provenance error. [src/digitalmodel/power/protection/relay_coordination.py:L63-L70] [src/digitalmodel/power/protection/relay_coordination.py:L189-L194] [tests/power/protection/test_relay_coordination.py:L151-L185] | C05 is regression only; no detection check exists for a wrong standard coefficient or edition. | issue pending (4) |

The complete source read finds 0 exception handlers, 0 `continue` statements, 0 empty-list/dictionary error returns, 0 external-text `int()`/`float()` conversions, 0 epoch defaults and 0 cache writes; the three `None` returns and the intentional `None` field default are accounted for above. The ordinary zero comparisons are rejection/branch criteria, not fallback data. [src/digitalmodel/power/protection/__init__.py:L1-L5] [src/digitalmodel/power/protection/relay_coordination.py:L1-L327]

No incident issue is supplied for this package; read-only issue searches for the module, relay intersections and IEEE reference found no matching incident to reuse. The draft references above record source-confirmed limitations rather than attributing an unobserved incident.

## 7. Lessons log

This initial record is append-only; subsequent changes should retain the evidence and append a new dated row.

| Date | Reference | Lesson / pack change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-26 | Initial pack | Keep settings storage separate from implemented trip behavior; sections 1, 3 and 6 document the boundary. | [src/digitalmodel/power/protection/relay_coordination.py:L114-L144] [src/digitalmodel/power/protection/relay_coordination.py:L228-L246] | Not assigned; draft 2 |
| 2026-10-26 | Initial pack | Treat repeated-formula and self-crossing tests as regression evidence; section 4 reserves independent validation. | [tests/power/protection/test_relay_coordination.py:L151-L185] [tests/power/protection/test_relay_coordination.py:L290-L313] | Not assigned; draft 4 |
| 2026-10-26 | Initial pack | Distinguish absence from exhausted/invalid intersection searches; section 6 records each return route. | [src/digitalmodel/power/protection/relay_coordination.py:L283-L327] | Not assigned; draft 3 |

## 8. Ownership

Review assignment for this draft: engineering calculations family quality manager; proposed deputy: a second engineering calculations reviewer. The producing role maintains the calculation evidence; the maintainer confirms the role holders and resolves repository scope. Ambiguous standard clauses, changed tolerances and unresolved numerical checks go to the family quality manager before clearance (AQG r0, *Analysis QA gates*, revision 0, section 11). No independent clearance is recorded by this draft.

## 9. Dependencies and implementation inventory

| Direction | Dependency / boundary | Evidence |
| --- | --- | --- |
| Runtime | `dataclasses`, `enum`, `typing` and `math` imports; no third-party or solver import in the package; repository Python requirement is >= 3.11. | [src/digitalmodel/power/protection/relay_coordination.py:L13-L18] [src/digitalmodel/power/protection/__init__.py:L1-L5] [pyproject.toml:L5-L10] |
| Internal | `coordination_check` and the intersection residual/return calculation call `inverse_time_curve`; settings are passed explicitly, with no upstream model or external dataset read. | [src/digitalmodel/power/protection/relay_coordination.py:L228-L246] [src/digitalmodel/power/protection/relay_coordination.py:L283-L327] |
| Demonstrated consumer | `tests/power/protection/test_relay_coordination.py` imports the enums, dataclasses and three functions; it constructs synthetic inputs inline. | [tests/power/protection/test_relay_coordination.py:L14-L22] [tests/power/protection/test_relay_coordination.py:L56-L332] |
| Domain routing | The existing power entry routes source and tests at domain level and has a `submodules` mapping. | [docs/registry/module-routing.yaml:L333-L351] |

### Map inventory and counted-family audit

Map ref: `origin/codex/implmap-work` at `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; declared data revision: `270c8759c374b67a7f032953e7fcdb14e0224461`. Only citations with an explicit `@revision` below use that map ref; all other ranged citations use the code revision in the header. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L3]

Audit record: parsing every family's `implementations` list and selecting paths inside `src/digitalmodel/power/protection/` yields **0 module map families and 0 implementation rows** across the map's 13 families. There are no subpackages to exclude. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502] [src/digitalmodel/power/protection/__init__.py:L1-L5] [src/digitalmodel/power/protection/relay_coordination.py:L1-L327]

| Inventory | Rows in package | Counted families | Counted grep | Further second implementations |
| --- | --- | --- | --- | --- |
| Empty at the stated map ref | 0 | 0 | NOT_APPLICABLE: no family has an in-package row | Not counted; no counted search to perform |

Self-check A8: the inventory condition passes with an empty inventory; the counted search is `NOT_APPLICABLE` for the reason above. No signal was selected or suppressed, because there is no counted family. Map follow-up: add inverse-time relay evaluation, relay grading-margin calculation and time-current intersection search, with the implementations already listed in section 3; issue pending (5). [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502] [src/digitalmodel/power/protection/relay_coordination.py:L150-L327]

Supplementary whole-repository search at the code pin, including `scripts/` and tracked hidden directories:

```sh
git grep -n -E 'relay_coordination|inverse_time_curve|time_current_intersection|coordination_check|C37[.]112|OvercurrentRelay|DifferentialRelay' 2aa19e88306d9b4294e49a2b7849b07ae036b205 -- '*.py' '*.md' '*.yaml'
```

Search record: 4 matching files, the two package Python files, the module test file and the repository README. Definition classification: the three engineering calculations are in section 3; relay enums/dataclasses hold settings, test formulas are test oracles and README text is documentation. No further production implementation was found by this supplementary search; this result does not broaden the counted-family audit. [src/digitalmodel/power/protection/__init__.py:L1-L5] [src/digitalmodel/power/protection/relay_coordination.py:L24-L327] [tests/power/protection/test_relay_coordination.py:L1-L332] [README.md:L124-L133]

## 10. Coverage and open items

### Coverage record

Both package Python files were read completely: `src/digitalmodel/power/protection/__init__.py` and `src/digitalmodel/power/protection/relay_coordination.py`; the test package marker `tests/power/protection/__init__.py` is empty, and the module test file contains 23 test definitions. [src/digitalmodel/power/protection/__init__.py:L1-L5] [src/digitalmodel/power/protection/relay_coordination.py:L1-L327] [tests/power/protection/test_relay_coordination.py:L28-L332]

Documentation discovery record: no pre-existing README or docs subfolder exists within the package at the code pin. A search under `docs/` for the package path, dotted module path, module name, IEEE reference and protection-relay phrase finds the power row of `docs/maps/digitalmodel-operator-map.md`; the routing entry is in `docs/registry/module-routing.yaml`. The repository README also names protection relays at domain level; its domain-wide test count is not used as a package count. [docs/maps/digitalmodel-operator-map.md:L57-L57] [docs/registry/module-routing.yaml:L333-L351] [README.md:L133-L133]

All non-example module nodes are listed below; the six example nodes are in section 4, giving 23 distinct nodes in total. [tests/power/protection/test_relay_coordination.py:L28-L332]

| Other existing module test node ID | Coverage / BASE and after result | Source |
| --- | --- | --- |
| `tests/power/protection/test_relay_coordination.py::TestRelayType::test_overcurrent_50_value` | Enum value / PASS | [tests/power/protection/test_relay_coordination.py:L28-L30] |
| `tests/power/protection/test_relay_coordination.py::TestRelayType::test_overcurrent_51_value` | Enum value / PASS | [tests/power/protection/test_relay_coordination.py:L32-L33] |
| `tests/power/protection/test_relay_coordination.py::TestRelayType::test_differential_87_value` | Enum value / PASS | [tests/power/protection/test_relay_coordination.py:L35-L36] |
| `tests/power/protection/test_relay_coordination.py::TestCurveType::test_all_curve_types_exist` | Enum set / PASS | [tests/power/protection/test_relay_coordination.py:L42-L50] |
| `tests/power/protection/test_relay_coordination.py::TestOvercurrentRelay::test_valid_construction` | Storage / PASS | [tests/power/protection/test_relay_coordination.py:L56-L66] |
| `tests/power/protection/test_relay_coordination.py::TestOvercurrentRelay::test_with_instantaneous_pickup` | Storage only / PASS | [tests/power/protection/test_relay_coordination.py:L68-L77] |
| `tests/power/protection/test_relay_coordination.py::TestOvercurrentRelay::test_negative_pickup_current_raises` | Guard / PASS | [tests/power/protection/test_relay_coordination.py:L79-L87] |
| `tests/power/protection/test_relay_coordination.py::TestOvercurrentRelay::test_zero_pickup_current_raises` | Guard / PASS | [tests/power/protection/test_relay_coordination.py:L89-L97] |
| `tests/power/protection/test_relay_coordination.py::TestOvercurrentRelay::test_negative_time_dial_raises` | Guard / PASS | [tests/power/protection/test_relay_coordination.py:L99-L107] |
| `tests/power/protection/test_relay_coordination.py::TestDifferentialRelay::test_valid_construction` | Storage / PASS | [tests/power/protection/test_relay_coordination.py:L113-L122] |
| `tests/power/protection/test_relay_coordination.py::TestDifferentialRelay::test_slope_out_of_range_raises` | Guard / PASS | [tests/power/protection/test_relay_coordination.py:L124-L131] |
| `tests/power/protection/test_relay_coordination.py::TestDifferentialRelay::test_negative_minimum_operate_current_raises` | Guard / PASS | [tests/power/protection/test_relay_coordination.py:L133-L142] |
| `tests/power/protection/test_relay_coordination.py::TestInverseTimeCurve::test_current_at_pickup_raises` | Pickup equality / PASS | [tests/power/protection/test_relay_coordination.py:L196-L203] |
| `tests/power/protection/test_relay_coordination.py::TestInverseTimeCurve::test_current_below_pickup_raises` | Below pickup / PASS | [tests/power/protection/test_relay_coordination.py:L205-L212] |
| `tests/power/protection/test_relay_coordination.py::TestCoordinationCheck::test_coordination_fail_insufficient_margin` | Margin failure / PASS | [tests/power/protection/test_relay_coordination.py:L250-L270] |
| `tests/power/protection/test_relay_coordination.py::TestCoordinationCheck::test_coordination_result_keys` | Four-key result shape / PASS | [tests/power/protection/test_relay_coordination.py:L272-L283] |
| `tests/power/protection/test_relay_coordination.py::TestTimecurrentIntersection::test_no_intersection_returns_none` | Same-curve no crossing / PASS | [tests/power/protection/test_relay_coordination.py:L315-L332] |

The full contracts-domain command is NOT_RUN under the no-solver-interface restriction: compatibility tests import the diffraction package, whose initializer imports the solver subpackage and attempts `OrcFxAPI` at module scope. The focused routing contract file is run separately without disabling tests or altering repository configuration. [tests/compat/test_group_compat.py:L90-L122] [src/digitalmodel/hydrodynamics/diffraction/__init__.py:L61-L72] [src/digitalmodel/hydrodynamics/diffraction/solver/__init__.py:L22-L34]

### Open items

| Item | Action / responsible role | Tracking and evidence |
| --- | --- | --- |
| Numeric input and unit boundary | Maintainer: qualify finite, positive direct inputs, restraint, optional instantaneous threshold and requested margin; retain the raw time-dial distinction. | issue pending (1); F01–F03, F06, F13. [src/digitalmodel/power/protection/relay_coordination.py:L96-L144] [src/digitalmodel/power/protection/relay_coordination.py:L183-L246] |
| Missing trip behavior | Maintainer: constrain the advertised scope or independently implement instantaneous and differential operating criteria. | issue pending (2); F04–F05. [src/digitalmodel/power/protection/relay_coordination.py:L114-L144] [src/digitalmodel/power/protection/relay_coordination.py:L228-L246] |
| Intersection robustness | Maintainer: distinguish search outcomes, validate controls and cover endpoint, clipping, coincident-curve and exhausted-iteration cases. | issue pending (3); F07–F12. [src/digitalmodel/power/protection/relay_coordination.py:L252-L327] |
| U1: unverified standard conformance and missing independent worked example | Family quality manager: establish IEEE C37.112-2018 clause/table provenance, decide citation sidecar adoption and review an independent reference test; no licensed tabulated values are reproduced here. | issue pending (4); F14. [src/digitalmodel/power/protection/relay_coordination.py:L7-L10] [src/digitalmodel/power/protection/relay_coordination.py:L63-L70] [tests/power/protection/test_relay_coordination.py:L151-L185] |
| Map and clearance follow-up | Maintainer: add the three section-3 calculation families to the map, replace draft references with issue numbers, and record independent review and role assignments before clearance. | issue pending (5). [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502] [src/digitalmodel/power/protection/relay_coordination.py:L150-L327] |
