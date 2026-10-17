# Power analysis knowledge pack

| Header | Value |
| --- | --- |
| Module | power/analysis |
| Repository | digitalmodel |
| Path | src/digitalmodel/power/analysis |
| Pack revision | 0 |
| Code revision | 0f7510691e577765c3cd754970b398a4531f367c |
| Analysis family | Engineering calculations (EC) |
| QM role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The implemented main path solves AC load flow with Newton–Raphson iterations and returns bus voltages, angles, injections and branch flows. [src/digitalmodel/power/analysis/load_flow.py:L219-L238] [src/digitalmodel/power/analysis/load_flow.py:L391-L440]
The domain maturity is beta. [docs/registry/module-routing.yaml:L289-L294]
The package description also names contingency and fault studies; the inspected package consists of that description and the load-flow implementation, so those capabilities are not established by the implemented main path. [src/digitalmodel/power/analysis/__init__.py:L1-L3] [src/digitalmodel/power/analysis/load_flow.py:L219-L238]
Scope excludes neighbouring power subpackages; no licensed program interface is required by the imports in this module. [src/digitalmodel/power/analysis/load_flow.py:L13-L20]

## 2. Inputs and outputs

| Input | Unit | Range or validation | Default | Source |
| --- | --- | --- | --- | --- |
| Bus ID and type | identifier, enum | unique IDs; exactly one slack in network | required | [src/digitalmodel/power/analysis/load_flow.py:L69-L70] [src/digitalmodel/power/analysis/load_flow.py:L148-L158] |
| voltage_pu, angle_rad | pu, rad | supplied starting values; no finite-value check | 1.0, 0.0 | [src/digitalmodel/power/analysis/load_flow.py:L69-L89] |
| p_gen_pu, q_gen_pu, p_load_pu, q_load_pu | pu | no numerical range check | 0.0 each | [src/digitalmodel/power/analysis/load_flow.py:L69-L89] |
| v_min_pu, v_max_pu | pu | minimum positive and no greater than maximum | 0.9, 1.1 | [src/digitalmodel/power/analysis/load_flow.py:L77-L89] |
| Branch endpoints | bus IDs | looked up in network dictionary | required | [src/digitalmodel/power/analysis/load_flow.py:L112-L117] [src/digitalmodel/power/analysis/load_flow.py:L198-L205] |
| r_pu, x_pu | pu | not both zero | required | [src/digitalmodel/power/analysis/load_flow.py:L102-L126] |
| b_pu, tap_ratio | pu, dimensionless ratio | tap positive | 0.0, 1.0 | [src/digitalmodel/power/analysis/load_flow.py:L106-L126] |
| base_mva | MVA | stored, not used in numerical solve | required | [src/digitalmodel/power/analysis/load_flow.py:L140-L158] [src/digitalmodel/power/analysis/load_flow.py:L240-L256] |
| tol, max_iter | pu mismatch, iterations | no explicit argument validation | 1e-6, 50 | [src/digitalmodel/power/analysis/load_flow.py:L219-L239] [src/digitalmodel/power/analysis/load_flow.py:L275-L303] |

| Output | Unit and convention | Source |
| --- | --- | --- |
| build_ybus | complex pu admittance, input bus-list ordering | [src/digitalmodel/power/analysis/load_flow.py:L185-L216] |
| converged, iterations | boolean, iteration count | [src/digitalmodel/power/analysis/load_flow.py:L275-L303] [src/digitalmodel/power/analysis/load_flow.py:L435-L440] |
| bus_results | ID, voltage pu, angle degrees, active/reactive net injections pu | [src/digitalmodel/power/analysis/load_flow.py:L391-L400] |
| branch_results | endpoint IDs, sending-end active/reactive flow and total active/reactive losses pu | [src/digitalmodel/power/analysis/load_flow.py:L415-L433] |

Positive scheduled injection is generation minus load; initial angles are radians and output angles are converted to degrees. [src/digitalmodel/power/analysis/load_flow.py:L246-L256] [src/digitalmodel/power/analysis/load_flow.py:L394-L400]
No geographic coordinate frame or file-format unit conversion is implemented in this in-memory API. [src/digitalmodel/power/analysis/load_flow.py:L219-L242] [src/digitalmodel/power/analysis/load_flow.py:L435-L440]

## 3. Method and governing standards

| Method | Implementation and reference basis |
| --- | --- |
| Pi-model admittance | Reciprocal complex series impedance, half charging at each end and real off-nominal tap terms in build_ybus. [src/digitalmodel/power/analysis/load_flow.py:L202-L214] |
| AC injections | Double sum using voltage products, admittance conductance/susceptance and angle differences. [src/digitalmodel/power/analysis/load_flow.py:L279-L299] |
| Newton update | Four Jacobian blocks; NumPy linear solve; angles updated at PV/PQ buses and magnitudes at PQ buses. [src/digitalmodel/power/analysis/load_flow.py:L305-L374] |
| Convergence | Maximum absolute scheduled-power mismatch strictly less than tol. [src/digitalmodel/power/analysis/load_flow.py:L294-L303] |
| Branch flow | Complex end voltages, end currents, conjugate-power products and summed end losses. [src/digitalmodel/power/analysis/load_flow.py:L404-L433] |

The docstring names Glover, Sarma and Overbye, *Power Systems Analysis and Design*, sixth edition, and IEEE Std 399, *Recommended Practice for Industrial and Commercial Power Systems Analysis*, without a clause or standard edition. [src/digitalmodel/power/analysis/load_flow.py:L6-L10]
UNVERIFIED: correspondence to either publication is not established; clause text not checked against the licensed copy, and no publication-specific equation or value is adopted here; issue pending (1). [src/digitalmodel/power/analysis/load_flow.py:L6-L10]
No Citation sidecar is returned with PowerFlowResult; the output fields hold numerical results and convergence metadata only. [src/digitalmodel/power/analysis/load_flow.py:L161-L182] [src/digitalmodel/power/analysis/load_flow.py:L435-L440]

## 4. Validated worked examples

Offline results below were recorded at the header code pin: 32 passed, 0 failed, 0 skipped, 0 NOT_RUN, both before and after documentation changes; the existing test definitions provide the criteria. [tests/power/analysis/test_load_flow.py:L32-L386]
The closed-form matrix cases are local algebra checks, not a published network benchmark. [tests/power/analysis/test_load_flow.py:L178-L212]

| Kind / case | Independent basis or regression basis | Inputs | Expected / tolerance | Test node ID | Offline result |
| --- | --- | --- | --- | --- | --- |
| Validated: zero-shunt row sum | Closed form: admittance row sum is zero for unit taps without shunts | _make_3bus_network | 0 pu admittance, decimal=10 comparison | tests/power/analysis/test_load_flow.py::TestYBus::test_ybus_diagonal_sum | PASS [tests/power/analysis/test_load_flow.py:L148-L184] |
| Validated: off-diagonal | Closed form negative reciprocal impedance | branch 1–2 and absent 1–3 edge | -1/(0.02+j0.06) pu admittance, default assert_almost_equal; absent edge <1e-12 pu | tests/power/analysis/test_load_flow.py::TestYBus::test_ybus_off_diagonal_values | PASS [tests/power/analysis/test_load_flow.py:L186-L195] |
| Validated: shunt diagonal | Closed form reciprocal impedance plus half shunt | two buses, unit tap | 1/(0.01+j0.1)+j0.04/2 pu admittance, default assert_almost_equal | tests/power/analysis/test_load_flow.py::TestYBus::test_ybus_with_shunt | PASS [tests/power/analysis/test_load_flow.py:L197-L212] |
| Regression: three-bus convergence | Fixture and solver convergence behaviour | _make_3bus_network | converged=True, <20 iterations | tests/power/analysis/test_load_flow.py::TestSolverConvergence::test_3bus_converges | PASS [tests/power/analysis/test_load_flow.py:L219-L223] |
| Regression: five-bus envelope | Fixture and broad voltage bounds | _make_5bus_network | every voltage strictly between 0.8 and 1.2 pu | tests/power/analysis/test_load_flow.py::TestFiveBusSystem::test_5bus_voltages_in_range | PASS [tests/power/analysis/test_load_flow.py:L270-L302] |

## 5. QA gates

AQG r0, section 10, Python calculations supplies G0 inputs/units/citations, G1 imports/tests and G2 hand or published check; G3 and G4 are NOT_APPLICABLE, and G5 applies to evidence packaging (AQG r0, sections 9–10).

| Gate | Check ID | Executable check and numerical pass criterion | Evidence |
| --- | --- | --- | --- |
| G0 | C0 | TestBus.test_bus_defaults: voltage=1.0 pu, angle=0.0 rad, each power default=0.0 pu and limits=0.9/1.1 pu, 0 failed assertions | tests/power/analysis/test_load_flow.py::TestBus::test_bus_defaults [tests/power/analysis/test_load_flow.py:L45-L55] |
| G0 | C1 | TestBranch.test_branch_defaults: b=0.0 pu, tap=1.0 ratio, 0 failed assertions | tests/power/analysis/test_load_flow.py::TestBranch::test_branch_defaults [tests/power/analysis/test_load_flow.py:L83-L87] |
| G0 | C2 | Invalid voltage minimum/range, zero impedance, negative tap, missing/multiple slack and duplicate IDs each raise ValueError; 7 cases, 0 missing exceptions (count) | TestBus, TestBranch, TestNetwork validation tests [tests/power/analysis/test_load_flow.py:L71-L77] [tests/power/analysis/test_load_flow.py:L93-L142] |
| G1 | C3 | Import digitalmodel.power.analysis.load_flow through tests/power/analysis/test_load_flow.py; 32 passed, 0 failed, 0 skipped (tests); no target-program load route is needed for this in-memory Python API | [tests/power/analysis/test_load_flow.py:L18-L26] [tests/power/analysis/test_load_flow.py:L32-L386] |
| G2 | C4 | build_ybus row sums=0 pu admittance to decimal=10; off-diagonal reciprocal and shunt checks use NumPy default decimal=7, with missing edge <1e-12 pu | Three validated test nodes in section 4 [tests/power/analysis/test_load_flow.py:L178-L212] |
| G2 | C5 | solve_power_flow three-bus active balance absolute error <=1e-4 pu; slack voltage/angle absolute tolerance 1e-8 pu/degree; PV voltage tolerance 1e-6 pu | tests/power/analysis/test_load_flow.py::TestSolverConvergence::test_3bus_power_balance and fixed-voltage tests [tests/power/analysis/test_load_flow.py:L225-L255] |
| G2 | C6 | max_iter=1 yields converged=False and iterations=1 (count); three/five-bus convergence <20 iterations; five-bus losses >=-1e-10 pu | tests/power/analysis/test_load_flow.py::TestSolverNonConvergence::test_max_iterations_exceeded and convergence/loss tests [tests/power/analysis/test_load_flow.py:L219-L223] [tests/power/analysis/test_load_flow.py:L258-L264] [tests/power/analysis/test_load_flow.py:L291-L315] |
| G3 | NOT_APPLICABLE | No time-domain integration in the solve; 0 dynamic states are advanced by the Newton updates | [src/digitalmodel/power/analysis/load_flow.py:L367-L374] |
| G4 | NOT_APPLICABLE | Electrical bus/branch API has no diffraction mesh; 0 mesh inputs | [src/digitalmodel/power/analysis/load_flow.py:L130-L146] |
| G5 | C7 | Record code pin, all 5 section-4 results and all 8 applicable check IDs C0–C7; 0 missing records (count); second-provider clearance required before use as cleared guidance | Section 4 and this table; AQG r0 sections 9–11 |

## 6. Known failure modes and pitfalls

sweep: 21 found

The sweep covers both package Python files; the rows count distinct defaults, unit assumptions, conversion sites and return-path risks, rather than raw grep lines. [src/digitalmodel/power/analysis/load_flow.py:L69-L89] [src/digitalmodel/power/analysis/load_flow.py:L112-L126] [src/digitalmodel/power/analysis/load_flow.py:L219-L440]

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|cache|unit' src/digitalmodel/power/analysis
rg -n 'default|zeros|base_mva|tol|max_iter|v_min|v_max|tap' src/digitalmodel/power/analysis
```

| Site at code pin | Symptom / interpretation | Detection check or gap | Tracking |
| --- | --- | --- | --- |
| angle_rad default | Omitted angle starts at 0.0 rad. [src/digitalmodel/power/analysis/load_flow.py:L72-L72] | C0 | Documented default |
| p_gen_pu default | Omitted active generation becomes 0.0 pu. [src/digitalmodel/power/analysis/load_flow.py:L73-L73] | C0 | Documented default |
| q_gen_pu default | Omitted reactive generation becomes 0.0 pu. [src/digitalmodel/power/analysis/load_flow.py:L74-L74] | C0 | Documented default |
| p_load_pu default | Omitted active load becomes 0.0 pu. [src/digitalmodel/power/analysis/load_flow.py:L75-L75] | C0 | Documented default |
| q_load_pu default | Omitted reactive load becomes 0.0 pu. [src/digitalmodel/power/analysis/load_flow.py:L76-L76] | C0 | Documented default |
| voltage_pu default | Omitted magnitude starts at 1.0 pu. [src/digitalmodel/power/analysis/load_flow.py:L71-L71] | C0 | Documented default |
| v_min_pu default | 0.9 pu limit is metadata, not a solve constraint. [src/digitalmodel/power/analysis/load_flow.py:L77-L89] [src/digitalmodel/power/analysis/load_flow.py:L370-L374] | C0 checks default; no detection check exists for enforcement | issue pending (2) |
| v_max_pu default | 1.1 pu limit is metadata, not a solve constraint. [src/digitalmodel/power/analysis/load_flow.py:L77-L89] [src/digitalmodel/power/analysis/load_flow.py:L370-L374] | C0 checks default; no detection check exists for enforcement | issue pending (2) |
| b_pu default | Missing charging becomes 0.0 pu. [src/digitalmodel/power/analysis/load_flow.py:L116-L116] | C1 | Documented default |
| tap_ratio default | Missing tap becomes 1.0 ratio; non-unity tap path lacks an independent test. [src/digitalmodel/power/analysis/load_flow.py:L117-L117] [src/digitalmodel/power/analysis/load_flow.py:L210-L214] [src/digitalmodel/power/analysis/load_flow.py:L415-L421] | C1 checks default; no detection check exists for non-unity tap | issue pending (3) |
| tol default | 1e-6 pu mismatch governs termination. [src/digitalmodel/power/analysis/load_flow.py:L219-L222] [src/digitalmodel/power/analysis/load_flow.py:L301-L303] | C6 checks nominal convergence; no detection check exists for invalid tolerance | issue pending (2) |
| max_iter default | 50 iterations; nonpositive input skips iteration and returns unconverged numerical outputs. [src/digitalmodel/power/analysis/load_flow.py:L219-L222] [src/digitalmodel/power/analysis/load_flow.py:L275-L278] [src/digitalmodel/power/analysis/load_flow.py:L435-L440] | C6 checks one-iteration failure; no detection check exists for nonpositive input | issue pending (2) |
| iteration initialization | Zero count remains when loop is skipped. [src/digitalmodel/power/analysis/load_flow.py:L275-L278] | no detection check exists | issue pending (2) |
| per-unit input assumptions | Numerical powers and impedances assume compatible bases; no base conversion occurs. [src/digitalmodel/power/analysis/load_flow.py:L202-L214] [src/digitalmodel/power/analysis/load_flow.py:L250-L256] | no detection check exists for incompatible bases | issue pending (2) |
| base_mva | Changing stored MVA does not scale per-unit results. [src/digitalmodel/power/analysis/load_flow.py:L140-L158] | tests/power/analysis/test_load_flow.py::TestPerUnit::test_base_mva_scaling checks invariance, not base validity [tests/power/analysis/test_load_flow.py:L321-L355] | issue pending (2) |
| angle output conversion | float conversion operates on computed arrays, not external text; radians become degrees. [src/digitalmodel/power/analysis/load_flow.py:L394-L400] | C5 | Documented conversion |
| bus voltage/power float conversions | Numerical values only; non-finite inputs can propagate without a validation error. [src/digitalmodel/power/analysis/load_flow.py:L247-L255] [src/digitalmodel/power/analysis/load_flow.py:L396-L399] | no detection check exists | issue pending (2) |
| loss float conversions | Summed complex-power components are converted to float. [src/digitalmodel/power/analysis/load_flow.py:L420-L424] | C5, C6 | Documented conversion |
| branch flow float conversions | Sending-end complex-power components become floats. [src/digitalmodel/power/analysis/load_flow.py:L426-L433] | C5 checks only aggregate balance for unit taps | issue pending (3) |
| zero accumulators and lists | Y-bus, injection arrays, Jacobian and result lists start empty/zero and are populated explicitly; these are not exception fallbacks. [src/digitalmodel/power/analysis/load_flow.py:L198-L214] [src/digitalmodel/power/analysis/load_flow.py:L279-L292] [src/digitalmodel/power/analysis/load_flow.py:L305-L309] [src/digitalmodel/power/analysis/load_flow.py:L376-L403] | C4, C5; result-count tests [tests/power/analysis/test_load_flow.py:L304-L307] [tests/power/analysis/test_load_flow.py:L383-L386] | Documented construction |
| unconverged result return | Recomputed bus/branch outputs are returned even when converged=False; consumers must check that flag. [src/digitalmodel/power/analysis/load_flow.py:L275-L303] [src/digitalmodel/power/analysis/load_flow.py:L376-L440] | C6 | Documented return contract |

No incident issue is named in the task; repository issue searches for load-flow and power-flow defects did not supply an incident reference, so confirmed gaps use the drafts below. [src/digitalmodel/power/analysis/load_flow.py:L71-L89] [src/digitalmodel/power/analysis/load_flow.py:L148-L158] [src/digitalmodel/power/analysis/load_flow.py:L301-L303]
No exception handler, record-skipping continue, return None, error-path empty return, external-text numeric parser, epoch fallback or cache writer was found in the inspected implementation. [src/digitalmodel/power/analysis/load_flow.py:L13-L440]
Additional risk: Y-bus sending shunt and branch sending-power tap factors are not expressed consistently for non-unity tap; the unit-tap checks do not establish transformer balance, issue pending (3). [src/digitalmodel/power/analysis/load_flow.py:L210-L214] [src/digitalmodel/power/analysis/load_flow.py:L415-L421] [tests/power/analysis/test_load_flow.py:L197-L212]

## 7. Lessons log

| Date | Lesson | Evidence | Pack change | Shared entry |
| --- | --- | --- | --- | --- |
| 2026-10-17 | Keep analytical matrix checks distinct from fixture-only convergence checks | [tests/power/analysis/test_load_flow.py:L178-L223] | Section 4 separates validated algebra and regression behaviour | Not assigned; issue pending (1) |

## 8. Ownership

Assigned review role: engineering calculations quality manager; proposed deputy: engineering calculations deputy quality manager; producing discipline: engineering calculations; unresolved engineering classifications go to the family quality manager (AQG r0, section 11; module agent lifecycle revision 0, sections 3–4).
Status remains DRAFT pending independent-provider review (AQG r0, section 11).

## 9. Dependencies and implementation inventory

Runtime dependencies are Python standard-library math/dataclasses/enum/typing and NumPy; module tests additionally import pytest. [src/digitalmodel/power/analysis/load_flow.py:L13-L20] [tests/power/analysis/test_load_flow.py:L13-L16]
The tested caller route imports Bus, Branch, Network, build_ybus and solve_power_flow directly; no upstream dataset or licensed solver version is specified by this API. [tests/power/analysis/test_load_flow.py:L18-L26] [src/digitalmodel/power/analysis/load_flow.py:L130-L146] [src/digitalmodel/power/analysis/load_flow.py:L219-L242]

Implementation-map revision: 3256bab95f84da3d9fadce8d5dcf8458d66d1eb4; map data revision: 270c8759c374b67a7f032953e7fcdb14e0224461; source: docs/registry/implementation-map.yaml on codex/implmap-work.
Inventory: 0 confirmed implementation rows inside this package; no map family qualifies for ranking; counted-family grep: NOT_APPLICABLE because there are no in-package implementation rows.
The map instead places solve_power_flow under rejected candidates with disposition needs_review for external-solver-batch-run-stacks; that candidate is not an implementation row.
Map follow-up: add explicit admittance assembly, AC power injection, Newton load-flow and branch-flow families with in-package rows; issue pending (4).
A whole-repository Python search, including scripts/, used `solve_power_flow|build_ybus|Newton.Raphson|load.flow`; only this package and its tests implement the electrical load-flow API; other hits concern mechanical/root-finding calculations rather than electrical networks. [src/digitalmodel/power/analysis/load_flow.py:L185-L238] [src/digitalmodel/power/analysis/load_flow.py:L279-L299]

## 10. Coverage and open items

Files read: both package Python files, tests/power/analysis/__init__.py, tests/power/analysis/test_load_flow.py, and the power routing entry. [src/digitalmodel/power/analysis/load_flow.py:L13-L20] [src/digitalmodel/power/analysis/__init__.py:L1-L3] [tests/power/analysis/test_load_flow.py:L1-L26] [docs/registry/module-routing.yaml:L289-L294]
Documentation discovery found 1 file with identifying names; its path and text are withheld pending neutral codenaming, issue pending (5).
The 32 test functions cover enum/dataclass construction, validation, Y-bus assembly, convergence, a five-bus fixture, base invariance and result schemas. [tests/power/analysis/test_load_flow.py:L32-L386]

| Item | Disposition / maintainer follow-up |
| --- | --- |
| UNVERIFIED publication correspondence; no clause, edition for IEEE reference, benchmark network or Citation sidecar basis | issue pending (1); obtain precise public references and a separately sourced solved network before claiming published validation [src/digitalmodel/power/analysis/load_flow.py:L6-L10] [src/digitalmodel/power/analysis/load_flow.py:L161-L182] |
| Input validation and output qualification: finite inputs, positive base/tolerance/iteration count, connectivity, endpoint validity, voltage-limit and reactive-limit treatment | issue pending (2); current validation is limited to voltage bounds, branch impedance/tap and bus-ID/slack checks [src/digitalmodel/power/analysis/load_flow.py:L80-L89] [src/digitalmodel/power/analysis/load_flow.py:L119-L126] [src/digitalmodel/power/analysis/load_flow.py:L148-L158] |
| Transformer tap and charging balance reference case | issue pending (3); compare terminal powers with Y-bus injections for a non-unity tap [src/digitalmodel/power/analysis/load_flow.py:L210-L214] [src/digitalmodel/power/analysis/load_flow.py:L415-L424] |
| Map family coverage and rejected-candidate classification | issue pending (4); add the section-3 method families to the map before applying counted-family grep |
| Discovery document with identifying names | issue pending (5); neutralize names before linking publicly |
| Independent-provider clearance and shared lesson entry | issue pending (1); pack remains DRAFT |

The calc-citation contract was readable; no new standards-derived coefficient or formula is introduced by this documentation change. [src/digitalmodel/power/analysis/load_flow.py:L6-L10]
