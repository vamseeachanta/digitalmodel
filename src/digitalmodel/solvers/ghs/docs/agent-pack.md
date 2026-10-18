# GHS module knowledge pack

| Header | Value |
|---|---|
| Module | solvers/ghs |
| Repository | digitalmodel |
| Package | src/digitalmodel/solvers/ghs |
| Pack revision | 0 |
| Code revision | c099a5e1317e6bed63d876252d87cbb12c774372 |
| Analysis family | Engineering calculations (EC) |
| Family quality manager | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The module prepares a closed synthetic hydrostatics packet, previews receipt consistency, stages trusted synthetic events and compares normalized assertions with an exact rectangular-box oracle. [src/digitalmodel/solvers/ghs/contracts.py:L90-L126] [src/digitalmodel/solvers/ghs/runner.py:L119-L131] [src/digitalmodel/solvers/ghs/comparison.py:L68-L98]

`run_approved_capture` always raises; synthetic capture cannot establish native execution, diagnostic closure or process containment. [src/digitalmodel/solvers/ghs/runner.py:L15-L16] [src/digitalmodel/solvers/ghs/runner.py:L43-L51]

The separate qualification path observes fixed harmless Windows processes and keeps both launch authority and licensed-execution flags false; its native API requires 64-bit Windows. [src/digitalmodel/solvers/ghs/qualification.py:L47-L60] [src/digitalmodel/solvers/ghs/_windows_job.py:L76-L82]

The parent solvers routing entry is beta; this is domain maturity rather than evidence of native GHS validation. [docs/registry/module-routing.yaml:L354-L359] [src/digitalmodel/solvers/ghs/comparison.py:L97-L98]

## 2. Inputs and outputs

### Inputs

| Input | Unit / allowed range | Default | Origin and evidence |
|---|---|---|---|
| Request depths and density | Exactly depths 1, 2, 3 m and density 1000 kg/m3; decimal strings | Required | `normalized_request`; no alternative fluid/case accepted. [src/digitalmodel/solvers/ghs/contracts.py:L69-L87] |
| Units and frame | m, tonne, m3, m2; x forward from aft end, y port from centreplane, z up from baseline; depth from origin | Exact fixed mapping | `UNITS_FRAME`; native transformation is outside this contract. [src/digitalmodel/solvers/ghs/contracts.py:L7-L8] [docs/domains/ghs/neutral-hydrostatics-canary.md:L11-L13] |
| Geometry | Nonempty bytes, at most 8388608 bytes, hash equal to request | Required | Supplied bytes are identity-bound, not parsed as native geometry. [src/digitalmodel/solvers/ghs/_canonical.py:L58-L61] [src/digitalmodel/solvers/ghs/contracts.py:L90-L100] |
| Runtime profile | Closed schema; Windows, absolute drive paths, executable suffix `.exe`, distinct work/temp paths | Required; provisional command and unqualified containment only | `runtime_record` checks lexical paths and SHA-256 syntax. [src/digitalmodel/solvers/ghs/contracts.py:L19-L45] |
| Resource policy | 120 s declared wall timeout, 1 instance, 8388608 bytes/artifact, 33554432 bytes total | Fixed values | The synthetic seam does not implement a real wall-clock timeout. [src/digitalmodel/solvers/ghs/contracts.py:L10-L11] [src/digitalmodel/solvers/ghs/runner.py:L43-L51] |
| Normalized comparison | Exactly 3 rows, 10 named quantities per row; string decimals and artifact references | Required | `evidence_record`, `row_record`, `compare_normalized`. [src/digitalmodel/solvers/ghs/comparison.py:L31-L87] |
| Receipt preview | Packet/runtime digests, reference, timezone-aware expiry, nonce; consumed list at most 10000 entries | Required | Consistency check only; no nonce reservation or provenance verification. [src/digitalmodel/solvers/ghs/contracts.py:L129-L150] |
| Qualification timing | Watchdog 30 s; readiness 5 s; trigger budget 10 s; cleanup 2 s; margin 5 s; timeout observation 1 s | Fixed profile when `None` supplied | `validate_profile` rejects changed values. [src/digitalmodel/solvers/ghs/_qualification_contract.py:L1-L4] [src/digitalmodel/solvers/ghs/qualification.py:L19-L25] |

### Outputs

| Output | Units / format | Meaning and evidence |
|---|---|---|
| Prepared packet and preview argv/runfile | Canonical JSON identity; SHA-256; bytes and string argv | Hashes bind geometry, provisional runfile, runtime, command, output policy and oracle; preview runfile is explicitly not runnable. [src/digitalmodel/solvers/ghs/contracts.py:L58-L66] [src/digitalmodel/solvers/ghs/contracts.py:L90-L100] |
| Synthetic capture | States, SHA-256 strings, byte counts | `capture_failed` or `capture_unqualified`, one attempt, no native-execution claim. [src/digitalmodel/solvers/ghs/runner.py:L43-L76] |
| Comparison | Volume m3, mass tonne, area m2, centres/KB/BM m; exact rational expectation and tolerance strings | 30 comparisons; passed remains unreviewed and cannot establish equilibrium, GM/GZ or damage suitability. [src/digitalmodel/solvers/ghs/comparison.py:L6-L18] [src/digitalmodel/solvers/ghs/comparison.py:L68-L98] |
| Qualification records | Bounded JSON, at most 65536 bytes per encoded record | Private observations and allowlisted public summary; attempt state is persistent, independently of output directory. [src/digitalmodel/solvers/ghs/_qualification_state.py:L23-L38] [src/digitalmodel/solvers/ghs/_qualification_state.py:L66-L98] [src/digitalmodel/solvers/ghs/qualification.py:L74-L112] |

## 3. Method and governing standards

The ideal box has length 20 m, beam 10 m and declared height 4 m; at immersion T, V=LBT, mass=rho V/1000 tonnes, waterplane A=LB, KB=T/2, BMT=(LB cubed/12)/V and BML=(BL cubed/12)/V. The closed-form derivation is repository documentation, independent of a native GHS result; the implementation uses `Fraction`. [docs/domains/ghs/neutral-hydrostatics-canary.md:L9-L23] [src/digitalmodel/solvers/ghs/comparison.py:L9-L23]

`comparison.tolerance` takes the larger of 0.1 % of the absolute expectation and the quantity floor: coordinate/BM 0.005 m, area 0.01 m2, volume 0.1 m3, mass 0.1 tonne. Output resolution exceeding that tolerance rejects the assertion; the test threshold is a canary policy rather than a regulatory acceptance criterion. [src/digitalmodel/solvers/ghs/contracts.py:L9-L9] [src/digitalmodel/solvers/ghs/comparison.py:L26-L28] [src/digitalmodel/solvers/ghs/comparison.py:L78-L87] [docs/domains/ghs/neutral-hydrostatics-canary.md:L25-L25]

Canonical decimal parsing rejects locale separators, exponents, floats, booleans and negative zero; canonical JSON sorts keys and forbids nonfinite values. No conversion of external text through binary `float()` occurs on this calculation path. [src/digitalmodel/solvers/ghs/_canonical.py:L31-L55]

Receipt consistency and byte identity do not authenticate approval or prove truth of asserted source rows. Native line references currently use the coarse bound artifact byte count plus one. [src/digitalmodel/solvers/ghs/contracts.py:L137-L150] [src/digitalmodel/solvers/ghs/comparison.py:L49-L65] [docs/domains/ghs/neutral-hydrostatics-canary.md:L65-L65]

No licensed standard is claimed as the basis of these geometric integrals; this pack uses the documented closed form and implementation policy. The staged plan reserves vendor command/format qualification for a separate step. [docs/domains/ghs/neutral-hydrostatics-canary.md:L15-L25] [docs/plans/2026-09-11-issue-2089-ghs-canary.md:L59-L75]

UNVERIFIED: native command compatibility, geometry closure, density interpretation, native frame conversion and parser accuracy have not been established by the offline tests; tracked by #2089. [docs/domains/ghs/neutral-hydrostatics-canary.md:L11-L13] [docs/domains/ghs/neutral-hydrostatics-canary.md:L35-L39] [src/digitalmodel/solvers/ghs/comparison.py:L97-L98]

## 4. Validated worked examples and regression examples

Execution record: the offline GHS suite at the code revision in the header collected 146 cases: 145 PASS, 0 FAIL, 1 SKIP, 0 NOT_RUN; the skipped case is the explicitly enabled Windows sentinel acceptance, not a solver calculation. The test nodes below are the executed assertions, not native GHS observations. [tests/solvers/ghs/test_windows_qualification.py:L7-L15]

| ID / class | Source and input | Expected value and tolerance | Existing test node / result |
|---|---|---|---|
| V1 / validated closed form | Ideal box integrals, depths 1, 2, 3 m, density 1000 kg/m3; second check at 500 kg/m3. [docs/domains/ghs/neutral-hydrostatics-canary.md:L9-L23] | V=mass=200T in m3/tonne, A=200 m2, LCB=LCF=10 m, TCB=TCF=0 m, KB=T/2 m, BMT=25/(3T) m, BML=100/(3T) m; exact rational equality, zero numerical error; at half density first mass=100 tonnes. [tests/solvers/ghs/test_comparison.py:L27-L38] | `tests/solvers/ghs/test_comparison.py::test_independent_exact_oracle` — PASS. [tests/solvers/ghs/test_comparison.py:L27-L38] |
| R1 / regression | Synthetic normalized rows generated from production oracle; no independent native reference. [tests/solvers/ghs/test_comparison.py:L11-L24] | 30 comparisons, unreviewed state, licensed flag false. [tests/solvers/ghs/test_comparison.py:L41-L47] | `tests/solvers/ghs/test_comparison.py::test_synthetic_comparison_never_claims_native_execution` — PASS. [tests/solvers/ghs/test_comparison.py:L41-L47] |
| R2 / regression | Prepared synthetic packet and trusted injected backend. [tests/solvers/ghs/test_boundaries.py:L24-L32] | 1 backend call, identical argv/limits/digests, capture unqualified. [tests/solvers/ghs/test_boundaries.py:L33-L40] | `tests/solvers/ghs/test_boundaries.py::test_synthetic_pipeline_binds_prepared_packet` — PASS. [tests/solvers/ghs/test_boundaries.py:L24-L40] |
| R3 / regression | Synthetic transverse centre mutation. [tests/solvers/ghs/test_comparison.py:L69-L75] | 0.005 m passes; 0.005001 m fails with failure reasons. [tests/solvers/ghs/test_comparison.py:L69-L76] | `tests/solvers/ghs/test_comparison.py::test_tolerance_boundary_and_failed_conclusion` — PASS. [tests/solvers/ghs/test_comparison.py:L69-L76] |
| R4 / regression | Event iterator yields partial stdout, then raises. [tests/solvers/ghs/test_boundaries.py:L87-L91] | Partial-content SHA-256 retained, `stream_error`, failed state. [tests/solvers/ghs/test_boundaries.py:L92-L95] | `tests/solvers/ghs/test_boundaries.py::test_iterator_failure_preserves_partial_evidence` — PASS. [tests/solvers/ghs/test_boundaries.py:L87-L95] |
| R5 / regression | Injected marker-finalization error. [tests/solvers/ghs/test_qualification_finalization.py:L9-L23] | Private record exists; 0 public success files. [tests/solvers/ghs/test_qualification_finalization.py:L24-L25] | `tests/solvers/ghs/test_qualification_finalization.py::test_failed_marker_finalization_never_publishes_success` — PASS. [tests/solvers/ghs/test_qualification_finalization.py:L9-L25] |

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations, and section 9, QA appendix; checks below define the module evidence rather than certify a native run.

| Gate | Applicability | Checks / executable evidence |
|---|---|---|
| G0 | Applicable: inputs, units, citations | C01, C02, C03 |
| G1 | Applicable: imports and offline tests | C04, C07, C08, C09, C10, C11; the native GHS load route remains blocked by `run_approved_capture`. [src/digitalmodel/solvers/ghs/runner.py:L15-L16] |
| G2 | Applicable: independent hand calculation | C05, C06 |
| G3 | NOT_APPLICABLE: no dynamics in this bounded box comparison | AQG r0 section 10, Python calculations |
| G4 | NOT_APPLICABLE: no diffraction calculation in this family | AQG r0 section 10, Python calculations |
| G5 | Applicable: traceable evidence and bounded conclusions | C12, C13, C14 |

Run `python -m pytest tests/solvers/ghs -q -rA -p no:cacheprovider` with `PYTHONPATH=src` in the project environment; preserve per-node results and the code pin. The checks below all have passing offline evidence except the explicitly separate native acceptance. [tests/solvers/ghs/test_windows_qualification.py:L7-L15]

| Check / gate | Module check and numeric pass criterion with unit | Existing executable evidence |
|---|---|---|
| C01 / G0 | `prepare_canary` rejects all 11 invalid depth inputs, 7 closed-profile mutations and widened tolerance: 0 accepted invalid records (count). | `tests/solvers/ghs/test_contracts.py::test_invalid_depth`, `tests/solvers/ghs/test_contracts.py::test_closed_profile_and_fixed_case`, `tests/solvers/ghs/test_contracts.py::test_tolerance_cannot_widen`. [tests/solvers/ghs/test_contracts.py:L60-L76] |
| C02 / G0 | `run_synthetic_capture` rejects 5 stage substitutions and an oversized file; 0 substituted stages accepted (count). | `tests/solvers/ghs/test_boundaries.py::test_stage_substitution_rejects`, `tests/solvers/ghs/test_boundaries.py::test_oversize_substitution_rejected_before_read`. [tests/solvers/ghs/test_boundaries.py:L43-L57] [tests/solvers/ghs/test_boundaries.py:L98-L106] |
| C03 / G0 | `validate_packet` rejects 4 swapped components; `preview_approval` rejects 6 receipt mutations; 0 unauthorized records accepted (count). | `tests/solvers/ghs/test_contracts.py::test_swapped_components_rejected`, `tests/solvers/ghs/test_contracts.py::test_receipt_boundary`. [tests/solvers/ghs/test_contracts.py:L79-L86] [tests/solvers/ghs/test_contracts.py:L98-L108] |
| C04 / G1 | `run_approved_capture` makes 0 process calls; `qualify` on non-Windows creates 0 output directories (count). | `tests/solvers/ghs/test_runner.py::test_live_never_calls_process`, `tests/solvers/ghs/test_qualification.py::test_non_windows_public_call_never_creates_output`. [tests/solvers/ghs/test_runner.py:L51-L55] [tests/solvers/ghs/test_qualification.py:L37-L41] |
| C05 / G2 | `box_oracle`: error exactly 0 m3, 0 tonnes, 0 m2 and 0 m against V1 closed forms, over all 3 depths (count). | `tests/solvers/ghs/test_comparison.py::test_independent_exact_oracle`. [tests/solvers/ghs/test_comparison.py:L27-L38] |
| C06 / G2 | `compare_normalized`: transverse-centre error 0.005 m accepted, 0.005001 m rejected; 0 false pass states (count). | `tests/solvers/ghs/test_comparison.py::test_tolerance_boundary_and_failed_conclusion`. [tests/solvers/ghs/test_comparison.py:L69-L76] |
| C07 / G1 | `inspect_synthetic_events` stops an infinite empty stream at its 4096-event bound and rejects a chunk of 8388609 bytes; 0 overflow captures accepted (count). | `tests/solvers/ghs/test_boundaries.py::test_bad_terminal_evidence`, `tests/solvers/ghs/test_boundaries.py::test_event_cap_stops_infinite_empty_chunks`, `tests/solvers/ghs/test_runner.py::test_incremental_overflow_stops_consumption`. [src/digitalmodel/solvers/ghs/runner.py:L10-L12] [tests/solvers/ghs/test_boundaries.py:L67-L78] [tests/solvers/ghs/test_runner.py:L36-L42] |
| C08 / G1 | `_safe_events` preserves the partial stdout SHA-256 and records 1 stream-error reason (count), with 0 successful captures. | `tests/solvers/ghs/test_boundaries.py::test_iterator_failure_preserves_partial_evidence`. [tests/solvers/ghs/test_boundaries.py:L87-L95] |
| C09 / G1 | `Attempt.reserve/finish` leaves exactly 1 active marker after incomplete cleanup or finalization failure, blocking a second reservation (count). | `tests/solvers/ghs/test_qualification.py::test_persistent_reservation_and_no_cross_output_bypass`, `tests/solvers/ghs/test_qualification.py::test_failed_finalization_preserves_active_marker`. [tests/solvers/ghs/test_qualification.py:L50-L57] [tests/solvers/ghs/test_qualification.py:L115-L122] |
| C10 / G1 | `check_timing` rejects 5 invalid times; cleanup wait <=2000 ms; observed timeout deadline=1 s; no effective kill after trigger budget >10 s. | `tests/solvers/ghs/test_qualification.py::test_profile_closed_and_fixed`, `tests/solvers/ghs/test_qualification.py::test_timing_margin_rejects_natural_exit_confounds`, `tests/solvers/ghs/test_qualification.py::test_ineffective_kill_cannot_pass_after_natural_watchdog_exit`, `tests/solvers/ghs/test_qualification.py::test_timeout_has_observed_deadline_and_rejects_early_exit`, `tests/solvers/ghs/test_qualification_finalization.py::test_second_handle_rechecks_budget_before_effective_kill`. [tests/solvers/ghs/test_qualification.py:L20-L23] [tests/solvers/ghs/test_qualification.py:L44-L47] [tests/solvers/ghs/test_qualification.py:L145-L176] [tests/solvers/ghs/test_qualification_finalization.py:L28-L43] |
| C11 / G1 | `Owned.cleanup` attempts 3 handle releases even under close/wait failures; launch failure injection accepts 0 unchecked resumes across 8 cases (count). | `tests/solvers/ghs/test_qualification.py::test_retained_identity_mismatch_closes_handle`, `tests/solvers/ghs/test_windows_job.py::test_cleanup_attempts_all_handle_releases`, `tests/solvers/ghs/test_windows_launch.py::test_failure_never_resumes_unchecked_child`, `tests/solvers/ghs/test_windows_launch.py::test_failed_launch_cleanup_attempts_every_handle`. [tests/solvers/ghs/test_qualification.py:L156-L162] [tests/solvers/ghs/test_windows_job.py:L24-L38] [tests/solvers/ghs/test_windows_launch.py:L55-L77] |
| C12 / G5 | `public_summary` exposes 0 fixture identifiers and retains a 64-character evidence hash; 0 incomplete scenario sets yield success (counts). | `tests/solvers/ghs/test_qualification.py::test_public_summary_deidentifies_recursively`, `tests/solvers/ghs/test_qualification.py::test_incomplete_scenarios_cannot_be_public_success`, `tests/solvers/ghs/test_qualification.py::test_recovery_state_same_in_public_serialization`. [tests/solvers/ghs/test_qualification.py:L69-L91] [tests/solvers/ghs/test_qualification.py:L125-L126] |
| C13 / G5 | `qualify` finalization error leaves 0 public success files (count), preserving private evidence. | `tests/solvers/ghs/test_qualification_finalization.py::test_failed_marker_finalization_never_publishes_success`. [tests/solvers/ghs/test_qualification_finalization.py:L9-L25] |
| C14 / G5 | `compare_normalized` yields exactly 30 comparison records for 3 depths and 10 quantities; native-execution flag remains false (counts). | `tests/solvers/ghs/test_comparison.py::test_synthetic_comparison_never_claims_native_execution`, `tests/solvers/ghs/test_boundaries.py::test_synthetic_pipeline_binds_prepared_packet`. [tests/solvers/ghs/test_comparison.py:L41-L47] [tests/solvers/ghs/test_boundaries.py:L24-L40] |

## 6. Known failure modes and pitfalls

### Silent-failure sweep

The sweep inventories candidate sites, including deliberate fail-closed handling and initialization, rather than asserting that each site is a defect. Every candidate below has a detection check or an explicitly recorded coverage gap. The supplementary unit/default rows cover assumptions beyond lexical error handlers. [src/digitalmodel/solvers/ghs/runner.py:L54-L87] [src/digitalmodel/solvers/ghs/qualification.py:L47-L60]

sweep: 50 found

Commands used (package-wide, read as text; AST inspection supplements this lexical search):

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|\b(int|float)\(|1970|0\.05|=0\b|=0\.0\b|= 0\b|= 0\.0\b|\.get\(' src/digitalmodel/solvers/ghs
rg -n 'getattr|Fraction|UNITS_FRAME|TOLERANCES|RESOURCE_LIMITS|PROFILE|publish|write|cache|None' src/digitalmodel/solvers/ghs
```

| Site | Symptom / classification | Detection / tracking |
|---|---|---|
| S01 | Failed launch attempts cleanup, then rethrows; a cleanup exception can replace the initial exception. [src/digitalmodel/solvers/ghs/_owned_process.py:L62-L64] | C11; #2096 |
| S02 | Cleanup exception is accumulated; aggregate failure raises rather than reporting successful release. [src/digitalmodel/solvers/ghs/_owned_process.py:L71-L71] | C11; #2096 |
| S03 | Cleanup exception is accumulated; aggregate failure raises rather than reporting successful release. [src/digitalmodel/solvers/ghs/_owned_process.py:L73-L73] | C11; #2096 |
| S04 | Cleanup exception is accumulated; aggregate failure raises rather than reporting successful release. [src/digitalmodel/solvers/ghs/_owned_process.py:L77-L77] | C11; #2096 |
| S05 | Cleanup exception is accumulated; aggregate failure raises rather than reporting successful release. [src/digitalmodel/solvers/ghs/_owned_process.py:L80-L80] | C11; #2096 |
| S06 | Cleanup exception is accumulated; aggregate failure raises rather than reporting successful release. [src/digitalmodel/solvers/ghs/_owned_process.py:L103-L103] | C11; #2096 |
| S07 | Cleanup exception is accumulated; aggregate failure raises rather than reporting successful release. [src/digitalmodel/solvers/ghs/_owned_process.py:L106-L106] | C11; #2096 |
| S08 | Cleanup exception is accumulated; aggregate failure raises rather than reporting successful release. [src/digitalmodel/solvers/ghs/_owned_process.py:L109-L109] | C11; #2096 |
| S09 | Identity mismatch closes the retained handle then rethrows. [src/digitalmodel/solvers/ghs/_owned_process.py:L123-L124] | C11; #2096 |
| S10 | Monotonic seconds become truncated integer milliseconds, clamped at 0; this is not external numeric text. [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L44-L44] | C10; #2089 |
| S11 | Cleanup exception is accumulated; incomplete cleanup returns false rather than a successful proof. [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L73-L73] | no detection check exists for this Resources cleanup-error branch; issue pending (3) |
| S12 | Cleanup exception is accumulated; incomplete cleanup returns false rather than a successful proof. [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L77-L77] | no detection check exists for this Resources cleanup-error branch; issue pending (3) |
| S13 | Cleanup exception is accumulated; incomplete cleanup returns false rather than a successful proof. [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L81-L81] | no detection check exists for this Resources cleanup-error branch; issue pending (3) |
| S14 | Cleanup exception is accumulated; incomplete cleanup returns false rather than a successful proof. [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L84-L84] | no detection check exists for this Resources cleanup-error branch; issue pending (3) |
| S15 | Poll counter starts at 0; deadline uses the fixed 1 s timeout. [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L155-L155] | C10; #2089 |
| S16 | Expected failure of the retained-handle termination proof is swallowed; a later liveness check and final-close proof remain mandatory. [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L169-L169] | C10; #2089 |
| S17 | Scenario exception text becomes the exception class name, with cleanup status checked before outcome return. [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L215-L215] | no detection check exists for loss of exception detail; issue pending (3) |
| S18 | Absent platform file attributes become 0; symlink mode remains separately checked. [src/digitalmodel/solvers/ghs/_qualification_state.py:L18-L18] | no detection check exists for this platform-attribute fallback; issue pending (3) |
| S19 | JSON or Unicode error becomes a generic invalid-readiness error; original detail is discarded. [src/digitalmodel/solvers/ghs/_qualification_state.py:L46-L46] | no detection check exists for malformed JSON/encoding branches; issue pending (3) |
| S20 | Barrier existence returns None as successful completion, without opening its content. [src/digitalmodel/solvers/ghs/_sentinel.py:L17-L17] | no offline detection check exists; #2089 |
| S21 | Breakaway launch error becomes a parent record with no child; other launch errors rethrow. [src/digitalmodel/solvers/ghs/_sentinel.py:L42-L46] | no offline detection check exists for this sentinel branch; #2089 |
| S22 | Blocked-breakaway parent waits for finish, then returns None as normal completion. [src/digitalmodel/solvers/ghs/_sentinel.py:L46-L46] | no offline detection check exists; #2089 |
| S23 | Job configuration failure closes the job and rethrows. [src/digitalmodel/solvers/ghs/_windows_job.py:L98-L99] | C11; #2096 |
| S24 | Attribute update failure deletes initialized attribute storage and rethrows. [src/digitalmodel/solvers/ghs/_windows_job.py:L121-L122] | C11; #2096 |
| S25 | Native process ID is converted with int; this is a typed API integer, not external numeric text. [src/digitalmodel/solvers/ghs/_windows_job.py:L133-L133] | no detection check exists specifically for numeric conversion; issue pending (3) |
| S26 | Default wait is 0 ms: a poll, not a cleanup wait. [src/digitalmodel/solvers/ghs/_windows_job.py:L142-L145] | C10; #2089 |
| S27 | Artifact total starts at 0 and is bounded before comparison. [src/digitalmodel/solvers/ghs/comparison.py:L39-L39] | C14; #2089 |
| S28 | Invalid timestamp text becomes the generic canonical-evidence error. [src/digitalmodel/solvers/ghs/contracts.py:L132-L132] | no detection check exists for malformed or timezone-free timestamp text; issue pending (3) |
| S29 | Reservation failure removes only the just-created output root and rethrows. [src/digitalmodel/solvers/ghs/qualification.py:L86-L87] | no detection check exists for output-root reservation cleanup; issue pending (3) |
| S30 | ObservationFailure keeps a failure classification and cleanup flag, then preserves failed records when cleanup is confirmed. [src/digitalmodel/solvers/ghs/qualification.py:L96-L101] | no detection check exists for this complete orchestration branch; issue pending (3) |
| S31 | Failure preserving failed records silently changes clean to false; later state becomes recovery_required. [src/digitalmodel/solvers/ghs/qualification.py:L101-L101] | no detection check exists; issue pending (3) |
| S32 | Other known errors become recovery_required; detailed original error is not retained. [src/digitalmodel/solvers/ghs/qualification.py:L102-L103] | no detection check exists for this full orchestration branch; issue pending (3) |
| S33 | Absent platform file attributes become 0; symlink detection remains active. [src/digitalmodel/solvers/ghs/runner.py:L21-L21] | C02 covers symlink substitution, not this fallback; no detection check exists for the fallback; issue pending (3) |
| S34 | Staging OSError becomes a ValueError with retained cause and owned residue. [src/digitalmodel/solvers/ghs/runner.py:L37-L38] | no detection check exists for the I/O-failure branch; issue pending (3) |
| S35 | Total byte count starts at 0; sizes and hashes accumulate only accepted chunks. [src/digitalmodel/solvers/ghs/runner.py:L56-L56] | C07; #2096 |
| S36 | Iterator exhaustion returns None normally; missing terminal evidence is then rejected. [src/digitalmodel/solvers/ghs/runner.py:L84-L84] | C07; #2096 |
| S37 | Iterator error becomes a stream_error event; prior bytes remain hashed. [src/digitalmodel/solvers/ghs/runner.py:L86-L87] | C08; #2096 |
| S38 | Missing O_BINARY or O_NOFOLLOW becomes 0; observational checks do not establish atomic race containment. [src/digitalmodel/solvers/ghs/runner.py:L93-L93] | C02 covers substitution rejection, not the missing-flag branch; no detection check exists for that branch; #2089 |
| S39 | Backend exception becomes launch_error; original exception details are discarded. [src/digitalmodel/solvers/ghs/runner.py:L125-L126] | no detection check exists for a throwing injected backend; issue pending (3) |
| S40 | Decimal strings use a dot and forbid exponents, separators and negative zero; trailing zeroes normalize. [src/digitalmodel/solvers/ghs/_canonical.py:L37-L44] | C01; #2089 |
| S41 | Fixed SI/tonne frame assumes aft origin, port y and upward z; this is not a qualified native transform. [src/digitalmodel/solvers/ghs/contracts.py:L7-L8] | C01; #2089 |
| S42 | Fixed relative tolerance and absolute floors are canary policy, not regulatory limits. [src/digitalmodel/solvers/ghs/contracts.py:L9-L9] | C01, C06; #2089 |
| S43 | Resource defaults declare 120 s and one instance; synthetic capture does not enforce a real process timeout. [src/digitalmodel/solvers/ghs/contracts.py:L10-L11] | C04, C07; #2089 |
| S44 | Box dimensions, density default 1000 kg/m3, zero transverse centres and tonne conversion are analytical assumptions. [src/digitalmodel/solvers/ghs/comparison.py:L9-L18] | C05; #2089 |
| S45 | Fixed timing profile supplies watchdog/readiness/cleanup/margin values and 1 s timeout. [src/digitalmodel/solvers/ghs/_qualification_contract.py:L3-L4] | C10; #2089 |
| S46 | None timing profile selects fixed defaults instead of rejecting absence. [src/digitalmodel/solvers/ghs/qualification.py:L19-L25] | C10; #2089 |
| S47 | Missing scenarios default to an empty list, and unknown/non-dict entries are omitted from public serialization; noniterable scenarios raise TypeError despite the validity guard. [src/digitalmodel/solvers/ghs/qualification.py:L48-L58] | C12 detects incomplete sets; no detection check exists for noniterable input; issue pending (2) |
| S48 | Missing readiness files are skipped; an empty failed-record dictionary can be published without a completeness assertion. [src/digitalmodel/solvers/ghs/qualification.py:L63-L71] | no detection check exists for empty failed-record preservation; issue pending (3) |
| S49 | None or false sentinel return is mapped to exit code 0; readiness/watchdog controls determine whether this is expected. [src/digitalmodel/solvers/ghs/_sentinel.py:L90-L90] | no offline detection check exists; #2089 |
| S50 | Empty streams start with 0 bytes and the digest of empty content; capture state still depends on terminal evidence. [src/digitalmodel/solvers/ghs/runner.py:L55-L55] | C07, C08; #2096 |

The textual sweep found no `continue` statements, no literal empty-list/dict return on an error path, no 1970 date or 5 % fallback, and no cache write implementation; record publication and initialized accumulators are inventoried above. [src/digitalmodel/solvers/ghs/_qualification_state.py:L23-L49] [src/digitalmodel/solvers/ghs/runner.py:L54-L87]

### Confirmed incidents and applicability limits

| Symptom | Cause / recorded incident | Detection and disposition |
|---|---|---|
| A historical sentinel attempt could not establish complete process identity and cleanup. | A virtual-environment executable redirected to another process; the historical census remained incomplete, not repaired by later source changes. [docs/plans/2026-09-11-issue-2089-first-attempt-recovery.md:L6-L12] | C09, C10, C11; current-image regression `tests/solvers/ghs/test_windows_launch.py::test_native_image_comes_from_current_process_not_venv_redirector`; #2089. [tests/solvers/ghs/test_windows_launch.py:L116-L127] |
| Synthetic agreement can be misread as native GHS validation. | Native parsing, source truth, equilibrium and suitability remain excluded by result limitations. [src/digitalmodel/solvers/ghs/comparison.py:L88-L98] | C04, C12, C14; #2089 and cross-adapter control audit #2096. |
| A source locator can be within a byte bound without identifying an actual source line. | `row_record` checks line <= byte_count+1, not source contents. [src/digitalmodel/solvers/ghs/comparison.py:L57-L65] | No detection check exists for actual line contents; #2089. |
| A malformed scenario container raises TypeError rather than a failed public summary. | The iteration after the list validity guard remains unconditional. Offline probes using null and integer containers both raised TypeError. [src/digitalmodel/solvers/ghs/qualification.py:L47-L60] | No detection check exists; issue pending (2). |

## 7. Lessons log

| Date | Entry | Lesson | Evidence | Pack change | Shared-log entry |
|---|---|---|---|---|---|
| 2026-10-18 | Initial public pack | Keep closed-form validation separate from synthetic row agreement and native execution. | [tests/solvers/ghs/test_comparison.py:L11-L47] [src/digitalmodel/solvers/ghs/comparison.py:L88-L98] | Sections 3 to 5 distinguish the three claims. | Not assigned; #2089 |
| 2026-10-18 | Error-path inventory | Aggregated errors, fixed defaults and missing native proof need explicit detection coverage. | [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L69-L85] [src/digitalmodel/solvers/ghs/qualification.py:L96-L112] | Section 6 inventories individual sites. | Not assigned; #2096 |

## 8. Ownership

Review assignment: engineering calculations quality manager; deputy assignment: engineering calculations deputy quality manager. The producing role is module documentation maintainer; the reviewing provider must differ from the producing provider (AQG r0, section 11, Sign-off and independence).

Escalate missing evidence, tolerance changes and ambiguous applicability to the family quality manager; unresolved cross-family coverage goes to the maintainer. This is the review route for this draft, not a claim of completed approval (AQG r0, sections 6.4 and 11).

## 9. Dependencies and implementation inventory

### Revision and counted map families

Implementation-map reference: `codex/implmap-work`, file revision `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. Map citations in this subsection use that file revision; all other source citations use the header code revision. [docs/registry/implementation-map.yaml:L1-L3]

Inventory result: 0 map families have implementation rows inside `src/digitalmodel/solvers/ghs`; the counted inventory is empty and the counted-family grep is `NOT_APPLICABLE` because no family has an in-package implementation row. Three lexical matches for `validate_prepared`, `validate_packet` and `validate_profile` are excluded under `needs_review`, rather than counted as irregular-frequency implementations. [docs/registry/implementation-map.yaml:L42826-L42846]

Map follow-up: ideal-box hydrostatics, canonical packet/receipt identity, synthetic capture/comparison and harmless-process containment need method-family classification and in-package rows before they become counted families; issue pending (1). [src/digitalmodel/solvers/ghs/comparison.py:L9-L28] [src/digitalmodel/solvers/ghs/contracts.py:L90-L150] [src/digitalmodel/solvers/ghs/runner.py:L119-L131] [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L106-L218]

The repository-wide supplementary searches, including `scripts/`, were:

```bash
rg -n -i 'box_oracle|\bbmt_m\b|\bbml_m\b|canary\.rf|sentinel_containment' src scripts --glob '*.py'
rg -n 'length\s*\*\s*beam\s*\*\s*(depth|draft)|beam\*\*3/12|beam \*\* 3 / 12' src scripts --glob '*.py'
```

The synthetic replay supplies independent fixed assertions and delegates to the listed packet, capture and comparison definitions; it is not native output parsing or another solver launcher. A separate out-of-package hydrostatic estimator uses a rectangular-waterplane inertia expression; this is map follow-up, not a counted-package miss; issue pending (1). [scripts/review/results/issue-2089/replay-synthetic-example.py:L8-L20] [scripts/review/results/issue-2089/replay-synthetic-example.py:L77-L103] [src/digitalmodel/naval_architecture/ship_data.py:L320-L343]

### GHS references in package source (read as text)

The following inventory covers every case-insensitive `ghs` occurrence in the package source; private qualification-state names and the isolated helper namespace are dependencies, not vendor interfaces. [src/digitalmodel/solvers/ghs/_qualification_state.py:L52-L63] [src/digitalmodel/solvers/ghs/_sentinel.py:L80-L90]

| Source location | Dependency / boundary |
|---|---|
| Package docstring | Neutral preparation; licensed execution/native parsing gated. [src/digitalmodel/solvers/ghs/__init__.py:L1-L1] |
| `run_approved_capture` | Unconditional live GHS refusal. [src/digitalmodel/solvers/ghs/runner.py:L15-L16] |
| Comparison limitations | Explicitly excludes native execution and parsing. [src/digitalmodel/solvers/ghs/comparison.py:L97-L98] |
| State namespace | Local application-data namespace for synthetic qualification interlock. [src/digitalmodel/solvers/ghs/_qualification_state.py:L52-L63] |
| Sentinel identity import | Isolated helper package, native identity lookup. [src/digitalmodel/solvers/ghs/_sentinel.py:L22-L25] |
| Sentinel child import | Fixed helper interpreter selection. [src/digitalmodel/solvers/ghs/_sentinel.py:L28-L34] |
| Sentinel parent import | Readiness record publication/read. [src/digitalmodel/solvers/ghs/_sentinel.py:L37-L38] |
| Sentinel controller imports | Owned harmless-process launch and readiness publication. [src/digitalmodel/solvers/ghs/_sentinel.py:L55-L58] |
| Sentinel main import | Role, record and reparse validation. [src/digitalmodel/solvers/ghs/_sentinel.py:L66-L74] |
| Sentinel package loading and profile import | Original local helper package and finite watchdog. [src/digitalmodel/solvers/ghs/_sentinel.py:L80-L90] |
| Public qualification fields and limitation | Launch false; licensed execution false; no vendor or entitlement qualification. [src/digitalmodel/solvers/ghs/qualification.py:L54-L60] |
| Private qualification fields | Launch and licensed-execution flags false from initialization. [src/digitalmodel/solvers/ghs/qualification.py:L80-L82] |

No installed GHS version is established: the accepted runtime record remains `provisional_not_runnable`/`unqualified`, and qualification results never authorize vendor execution. The upstream inputs are caller-supplied dictionaries and bytes; downstream consumers receive asserted synthetic records, not approved hydrostatic tables. [src/digitalmodel/solvers/ghs/contracts.py:L30-L45] [src/digitalmodel/solvers/ghs/contracts.py:L90-L100] [src/digitalmodel/solvers/ghs/qualification.py:L47-L60] [docs/domains/ghs/neutral-hydrostatics-canary.md:L29-L37]

## 10. Coverage and open items

### Files and tests read

Coverage manifest at the code pin (source, tests and documentation; entries identify reading scope, not acceptance of historical claims):

**Source and tests**

- [src/digitalmodel/solvers/ghs/__init__.py:L1-L1]
- [src/digitalmodel/solvers/ghs/_canonical.py:L1-L66]
- [src/digitalmodel/solvers/ghs/_owned_process.py:L1-L131]
- [src/digitalmodel/solvers/ghs/_qualification_contract.py:L1-L4]
- [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L1-L218]
- [src/digitalmodel/solvers/ghs/_qualification_state.py:L1-L98]
- [src/digitalmodel/solvers/ghs/_sentinel.py:L1-L90]
- [src/digitalmodel/solvers/ghs/_windows_job.py:L1-L154]
- [src/digitalmodel/solvers/ghs/comparison.py:L1-L98]
- [src/digitalmodel/solvers/ghs/contracts.py:L1-L150]
- [src/digitalmodel/solvers/ghs/qualification.py:L1-L112]
- [src/digitalmodel/solvers/ghs/runner.py:L1-L131]
- [tests/solvers/ghs/__init__.py:L1-L1]
- [tests/solvers/ghs/test_boundaries.py:L1-L132]
- [tests/solvers/ghs/test_comparison.py:L1-L76]
- [tests/solvers/ghs/test_contracts.py:L1-L122]
- [tests/solvers/ghs/test_qualification.py:L1-L199]
- [tests/solvers/ghs/test_qualification_finalization.py:L1-L43]
- [tests/solvers/ghs/test_runner.py:L1-L55]
- [tests/solvers/ghs/test_windows_job.py:L1-L38]
- [tests/solvers/ghs/test_windows_launch.py:L1-L127]
- [tests/solvers/ghs/test_windows_qualification.py:L1-L15]

**Documentation discovery**

- [docs/archive/archive-blob-map-2026-10-08.csv:L1-L1284]
- [docs/archive/archive-candidates-2026-10-08.csv:L1-L1284]
- [docs/archive/archive-candidates-2026-10-08.summary.json:L1-L2659]
- [docs/session-handoffs/2026-09-11-ghs-containment-next.md:L1-L38]
- [docs/registry/implementation-map.yaml:L1-L53690]
- [docs/plans/2026-05-05-issue-483-curves-py-decomposition.md:L1-L46]
- [docs/plans/README.md:L1-L40]
- [docs/plans/2026-09-11-issue-2089-first-attempt-recovery.md:L1-L27]
- [docs/plans/2026-09-11-issue-2089-windows-containment.md:L1-L147]
- [docs/plans/evidence/2026-09-12-solver-continuation.html:L1-L22]
- [docs/plans/2026-09-11-issue-2089-ghs-canary.html:L1-L207]
- [docs/plans/2026-09-11-issue-2089-ghs-canary.md:L1-L206]
- [docs/domains/ship-design/software_options.puml:L1-L44]
- [docs/domains/ship-design/software_footprint.puml:L1-L53]
- [docs/domains/ship-design/hull-girder-strength-screening.md:L1-L183]
- [docs/plans/2026-09-11-issue-2089-windows-containment.html:L1-L213]
- [docs/domains/ship-design/vessel-stability-screening.md:L1-L184]
- [docs/domains/software-tools-index.md:L1-L120]
- [docs/domains/ghs/neutral-hydrostatics-canary.md:L1-L77]
- [docs/domains/ghs/2026-09-11-m1-synthetic.html:L1-L1]
- [docs/domains/ghs/api/api.md:L1-L19]
- [docs/domains/aqwa/workflows/model-checks.md:L1-L76]

The documentation discovery includes historical inventories, generated previews and mentions in other domains; their historical test totals and capability claims are not current evidence. The current scope uses the executable tests in section 4 and source boundaries in sections 1 to 3. [docs/domains/ghs/neutral-hydrostatics-canary.md:L55-L77] [tests/solvers/ghs/test_windows_qualification.py:L7-L15]

### Open-item register

| Item | Remaining work / responsible role | Tracking |
|---|---|---|
| U1 | UNVERIFIED: native command/version, geometry, density, frame and parser qualification remain unestablished; module maintainer and family quality manager to obtain reviewed native evidence. [docs/domains/ghs/neutral-hydrostatics-canary.md:L35-L39] [docs/domains/ghs/neutral-hydrostatics-canary.md:L65-L77] | #2089 |
| O2 | Native sentinel acceptance is SKIP on this platform; current offline results do not clear the historical incomplete census or establish native containment; module maintainer. [tests/solvers/ghs/test_windows_qualification.py:L7-L15] [docs/plans/2026-09-11-issue-2089-first-attempt-recovery.md:L8-L23] | #2089 |
| O3 | Add absent method families and in-package implementations to the map; include the out-of-package waterplane estimator in classification; registry maintainer. [src/digitalmodel/solvers/ghs/comparison.py:L9-L28] [src/digitalmodel/naval_architecture/ship_data.py:L320-L343] | issue pending (1) |
| O4 | Reject malformed scenario containers with a defined failure contract and focused offline tests; module maintainer. [src/digitalmodel/solvers/ghs/qualification.py:L47-L60] | issue pending (2) |
| O5 | Add focused error/default detection checks for section-6 coverage gaps; module maintainer. [src/digitalmodel/solvers/ghs/_qualification_scenarios.py:L69-L85] [src/digitalmodel/solvers/ghs/qualification.py:L96-L104] | issue pending (3) |
| O6 | Audit transferable approval/recovery/native-fixture controls; adapter maintainers; this pack makes no allegation about other adapters. [docs/plans/2026-09-11-issue-2089-ghs-canary.md:L118-L142] | #2096 |

No client-derived value or private sidecar content is used; the worked data are the neutral synthetic box and placeholder geometry already defined in the test inputs. [tests/solvers/ghs/test_contracts.py:L12-L23] [docs/domains/ghs/neutral-hydrostatics-canary.md:L9-L15]
