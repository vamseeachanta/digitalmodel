# Plan for #1633: OrcaWave-vs-AQWA — establish an abscissa contract and a defensible ship verdict

> **Status:** draft (r2 — redrafted after r1 MAJOR)
> **Complexity:** T3
> **Date:** 2026-08-03
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1633
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** `scripts/review/results/2026-08-03-plan-1633-claude.md` (r1: **MAJOR**, 17 findings, 6 blockers) | codex r1 UNAVAILABLE (rc=124 timeout) | agy r1 UNAVAILABLE (auth timeout)

---

## What changed since r1

The r1 review returned **MAJOR** and its blocker 1 was correct: the plan's
causal story could not explain the artifact it was chartered to fix. Investigating
that objection surfaced a defect that neither the issue, the r1 plan, nor the r1
review had named — **there is no abscissa contract between solvers**. This
redraft leads with it.

r1 proposed promoting `CORR_MIN` and setting it from measurement. This redraft
does **not** do that, because the correlations are computed without an abscissa
contract and are therefore inputs of unknown validity. A threshold fitted to them
would be fitted to noise. Thresholds are set in Phase 3, after the contract exists
and the numbers have been re-measured.

---

## Resource Intelligence Summary

### The abscissa defect (new — the lead finding)

The two solvers tabulate on **different grids running in opposite directions**,
and nothing in the comparator reconciles them.

| | grid | direction | points |
|---|---|---|---|
| AQWA `001_SHIP_RAOS_REV2.LIS` | 22.00, 19.00, 17.00, 15.42, 8.79, 6.15, 4.73, 3.84, 3.23, 2.79 s | period **descending** (⇒ freq ascending) | **10** |
| OrcaWave `orcawave_001_ship_raos_rev2.yml` | 2, 3, 4, 5, 6, 7, 7.5, 8, 8.5, 9, 9.5, 10, 11, 13, 15, 16, 17, 19, 20, 22 s | period **ascending** (⇒ OrcFxAPI freq descending) | **20** |

Only three abscissa values coincide (22, 19, 17 s).

`multi_solver_comparator.py:264-268` passes `rao_a.frequencies.values` to
`_calculate_deviation_stats` as **metadata only**; the arithmetic at line 209 is
`errors = values2 - values1` — raw elementwise subtraction. There is no equality
check on the two frequency arrays, no interpolation, and no ordering
normalisation anywhere in the module.

Downstream of that, the defect is **script-dependent**, which is why it has
survived:

- `scripts/benchmark/run_3way_benchmark.py:433-436` handles it **correctly** —
  `sort_idx = np.argsort(frequencies)` is applied to *both* `frequencies` and
  `raw_raos`, under the comment *"Sort by ascending frequency (OrcFxAPI returns
  descending)"*. The codebase already knows the ordering fact.
- `docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py:149` handles
  it **wrongly** — `np.interp(aqwa_freq, ow_freq, ow_mag_heading)` where
  `ow_freq` (line 74, derived from `diffraction.frequencies`) is descending.
  `np.interp` requires an increasing `xp`, does not validate it, and returns
  wrong values silently.

Demonstrated on the real grids with identical physics on both sides:

```
ow_freq increasing?   False
aqwa_freq increasing? True

  period     AQWA  as-coded  corrected
   22.00   0.9828    0.0039     0.9828
   19.00   0.9695    0.9828     0.9695
    8.79   0.5931    0.9828     0.5932
    2.79   0.0146    0.9828     0.0171

correlation as-coded  : -0.3961
correlation corrected : +1.0000
```

**Claim discipline:** this proves the mechanism is present and destructive — a
perfect correlation becomes −0.40. It does **not** prove it accounts for the
recorded −0.8551; that figure depends on the real RAO curves. Attribution is a
Phase 0 deliverable, not an assumption of this plan.

### Provenance is unknown (why Phase 0 exists)

`benchmark_report.json` (vessel `Ship_001_RAOs`, 2026-02-05 05:36:21) has the
shape of `MultiSolverComparator.generate_report()`. But its co-located inputs —
`benchmark_results/aqwa/` and `benchmark_results/orcawave/` — contain
`unit_box_clean.gdf`, `UnitBox_Benchmark.yml`, `WRK-031_3WAY_BENCHMARK.LIS`
(307 bytes): **unit-box artifacts, not ship**. The sibling
`benchmark_summary.json` (same directory, 2026-02-05T20:21) targets
`unit_box_spec.yml`. A later unit-box run overwrote the directory around the ship
report.

Consequently **we do not know which script produced the artifact this issue is
about**, and the two candidate scripts differ precisely on the defect in
question. Choosing a fix before resolving this would be guessing.

### Existing repo code

- Found: `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py`
  (737 lines) — no abscissa contract (above); absolute agreement threshold
  (126/135/469-471); solver-count-blind ladder (483-490); `allclose → 1.0`
  shortcut (217-218); a **deliberate** near-zero-magnitude override returning
  `correlation=1.0` (275-288); an **independent linear** `max_phase_diff` path
  (297, 316).
- Found: `tests/hydrodynamics/diffraction/test_multi_solver_comparator.py` —
  **22** tests (`grep -c "def test_"`). Consensus cases use identical solvers or
  assert membership in a 3-of-4 outcome set, so none can fail on the defects above.
- Found: `tests/hydrodynamics/diffraction/test_multi_solver_units_guard.py` —
  established guard convention is `pytest.raises(ValueError, match="[Uu]nit")`.
- Found (ship inputs, named per r1 blocker 5):
  `docs/domains/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2.xlsx`
  (2,340,355 B) and `orcawave_001_ship_raos_rev2.yml` (4,127 B, OrcaWave 11.6b);
  AQWA side `001_SHIP_RAOS_REV2.LIS` (3,391,338 B, Aqwa Workbench 2022 R2).
- Gap: no rotational-RAO unit handling in the comparator (grep for
  `57.29|np.degrees|np.radians|deg2rad|rad2deg` → zero matches).

### Standards

Not applicable — solver-comparison methodology, no standards-derived constant.
No `Citation` sidecar required per `.claude/rules/calc-citation-contract.md`.

### LLM Wiki pages consulted

No relevant wiki pages; defects are code-local. `Client: N/A`, so
`.claude/rules/wiki-sibling-routing.md` does not bind.

### Documents consulted

- Issue #1633 body — the original finding.
- Issue #1550 closing comment (PR #1636, `4d465406`) — records that `np.corrcoef`
  is scale-invariant, so the te/kg defect cannot explain the heave result.
- Issue #1631 — licensed lane reports rc 0 on validator FAIL; `priority:critical`.
- `scripts/review/results/2026-08-03-plan-1633-claude.md` — r1 MAJOR, 17 findings.
- `.claude/rules/licensed-solver-dispatch.md` (workspace-hub, branch
  `docs/licensed-solver-dispatch-rule`, unmerged) — Scheduled-Task dispatch
  contract. **Note:** this file does *not* contain the `ace-win-2` default or any
  heartbeat date (r1 finding 16 was correct); those come from
  `deckhand/config/deckhand/policy.yml` and
  `deckhand-licensed-runs-queue/queue/heartbeat/ace-win-2.json` respectively.
- `docs/domains/orcawave/L01_aqwa_benchmark/INTERPOLATION_COMPARISON_SUMMARY.md`
  (2026-01-05) — an **earlier** comparison that used *load* RAOs against AQWA
  *displacement* RAOs, giving ratios to 6.2×10⁷. Superseded:
  `run_proper_comparison.py:76` switched to `motion_raos`. Recorded so a future
  reader does not re-derive it as a live defect.

### Gaps identified

- No abscissa contract between solvers (the lead defect).
- No provenance record tying a benchmark report to the script that produced it.
- No relative agreement criterion; no solver-count-aware ladder; no
  degenerate-array guard; no circular phase statistic; no rotational unit guard.
- No test asserts any solver-produced value against an external reference.

### Evidence (embedded verification)

**Issue statuses** (verified 2026-08-03 via `gh issue view`):
`#1633` OPEN · `#1550` CLOSED/COMPLETED 2026-07-26 · `#1631` OPEN · `#1640` OPEN · `#1825` OPEN.

**File existence** (`ls -la`, 2026-08-03):
- EXISTS: `multi_solver_comparator.py` (737 lines), `001_SHIP_RAOS_REV2.LIS`
  (3,391,338 B), `orcawave_001_ship_raos_rev2.xlsx` (2,340,355 B),
  `orcawave_001_ship_raos_rev2.yml` (4,127 B),
  `scripts/benchmark/run_3way_benchmark.py`, `scripts/benchmark/validate_owd_vs_spec.py`
- MISSING (new — this plan creates): `benchmark_abscissa.py`,
  `test_abscissa_contract.py`, `test_consensus_criteria.py`,
  `test_l01_ship_external_oracle.py`

**Corrections carried from r1** (findings 6, 8, 16 — all verified):
- `test_multi_solver_comparator.py` has **22** tests, not 30.
- `L00_validation_wamit/validation_config.yaml` is **11 `pass` + 1 `blocked`**
  (case 2.2, line 51), and only **4** `reference_data.yaml` files exist across
  12 cases — 8 cases have no reference file at all.
- The `ace-win-2` / heartbeat claims are re-attributed above.

**Reproduction proof** — the recorded L01 table (`benchmark_report.json`):

| DOF | mag corr | mag rms | phase corr | max Δphase |
|---|---|---|---|---|
| surge | 0.958 | 0.383 | 0.433 | 270.1° |
| sway | 0.981 | 0.504 | 0.035 | 302.6° |
| heave | **−0.855** | 0.698 | −0.925 | 67.5° |
| roll | 0.489 | 1.791 | 0.388 | 248.7° |
| pitch | 0.968 | **26.153** | −0.060 | 355.7° |
| yaw | 0.967 | 8.504 | 0.392 | 179.8° |

All 36 `added_mass_correlations` and 36 `damping_correlations` are exactly `1.0`.

- Reproduced at: 2026-08-03. Failure mode matches the issue's *claim* (verdict
  untrustworthy) but **not its stated mechanism**; see *Correction to the issue record*.

**Dispatch precondition — verified live 2026-08-03**, `ace-win-1`, via
`dispatch-run.ps1 -Action submit` (Scheduled-Task path):

```
state: finished   exit_code: 0   stderr_bytes: 0
stdout: LICENCE_OK dll=11.6c
        DIFFRACTION_OK Diffraction
```

Repo root on that host is `D:\ws` (not `D:\workspace-hub`, a stale container);
workspace-hub there is on `main` at `7701d4e78`. Job and probe cleaned up.

<!-- Distinct sources: 12. Minimum 3 required. -->

---

## Correction to the issue record

1. **#1633 item 3** ("fix unit normalisation before re-running", blocked on #1550)
   is **done and did not move the verdict** — `np.corrcoef` is scale-invariant.
2. **`NO_CONSENSUS` on a 2-solver run carries no information.** With two solvers
   there is one pair, so `MAJORITY` (needs ≥2) is unreachable and `SPLIT` (needs
   ≥1) is identical to `FULL`. The ladder collapses.
3. **r1's proposed remedy is also declined.** Blocker 1 correctly showed the
   binding gate is `CORR_MIN = 0.99` (max observed correlation 0.981), but
   promoting `CORR_MIN` and fitting it to these numbers fits it to values computed
   without an abscissa contract. Order matters: contract → re-measure → threshold.

Belonging to #1631, not this plan: `benchmark_summary.json` records
`"AQWA": {"status": "failed", … ELM1 MISSING}` under a top-level `"success": true`.

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md` |
| Impl — abscissa contract | `src/digitalmodel/hydrodynamics/diffraction/benchmark_abscissa.py` (new) |
| Impl — comparator | `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py` |
| Impl — verdict derivation | `src/digitalmodel/hydrodynamics/diffraction/benchmark_verdict.py` (new) |
| Tests — abscissa | `tests/hydrodynamics/diffraction/test_abscissa_contract.py` (new) |
| Tests — criteria | `tests/hydrodynamics/diffraction/test_consensus_criteria.py` (new) |
| Tests — oracle | `tests/hydrodynamics/diffraction/test_l01_ship_external_oracle.py` (new) |
| Tests — existing | `tests/hydrodynamics/diffraction/test_multi_solver_comparator.py` |
| Regenerated report | `docs/domains/orcawave/L01_aqwa_benchmark/benchmark_results/benchmark_report.json` |
| Provenance record | `docs/domains/orcawave/L01_aqwa_benchmark/benchmark_results/provenance.json` (new) |
| Human-facing report | `docs/reports/2026-08-03-issue-1633-ship-benchmark.html` |
| Plan review r2 | `scripts/review/results/2026-08-03-plan-1633-{claude,codex,agy}-r2.md` |

---

## Deliverable

An **abscissa contract** that refuses to compare two solvers until their RAOs sit
on a common, ascending, verified-overlapping grid — plus the re-derived L01 ship
verdict computed under it, asserted against the AQWA `.LIS` external oracle, with
agreement thresholds set from the re-measured data and a recorded provenance
linking every report to the script that produced it.

---

## Pseudocode

```
function align_to_common_abscissa(rao_a, freq_a, rao_b, freq_b):
    # D0 — the contract. Refuse, never guess.
    assert_strictly_increasing(freq_a)          # raise, do not silently sort
    assert_strictly_increasing(freq_b)
    lo, hi = max(min(freq_a), min(freq_b)), min(max(freq_a), max(freq_b))
    if (hi - lo) / (max(hi, ...) ) < MIN_OVERLAP_FRACTION:
        raise AbscissaOverlapError(freq_a, freq_b)   # 3-of-20 overlap must fail loudly
    grid = union_within(freq_a, freq_b, lo, hi)
    return interp_on(grid, freq_a, rao_a), interp_on(grid, freq_b, rao_b), grid

function load_solver_result(source):
    freqs, raos = read(source)
    order = argsort(freqs)                       # sort BOTH, as run_3way_benchmark does
    return freqs[order], raos[order, ...]

function pair_agrees(comparison, tolerance, corr_min):
    scale = max(rms(series_a), rms(series_b), FLOOR_FOR_DOF[dof])   # symmetric, per-DOF
    return comparison.correlation > corr_min and comparison.rms_error / scale < tolerance

function classify_consensus(pairs, n_solvers):
    if n_solvers < 3: return FULL if all(pairs) else DISAGREE
    else:             ... existing 3-solver ladder ...

function deviation_stats(a, b):
    if empty(a) or empty(b):        return quality = INSUFFICIENT_DATA
    if zero_variance(a) and zero_variance(b):
        if peak_magnitude < NULL_RESPONSE_EPS:   return quality = NULL_RESPONSE   # preserves 275-288
        else:                                     return quality = INSUFFICIENT_DATA
    if allclose(a, b):              return quality = IDENTICAL, correlation = 1.0
    return quality = COMPARED, correlation = corrcoef(a, b)

function phase_stats(pa, pb):                     # D4 — both paths
    d = wrap_to_180(pb - pa)
    return circular_mean(d), circular_rms(d), max(abs(d))   # replaces line 316 too

function derive_status(report):
    if any dof.quality == INSUFFICIENT_DATA:  return "incomplete"
    if any dof.quality == IDENTICAL:          return "suspect"   # never "pass"
    if consensus == DISAGREE:                 return "fail"
    return "pass"
```

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/.../diffraction/benchmark_abscissa.py` | D0 contract: ordering, overlap, common-grid resampling |
| Modify | `src/.../diffraction/multi_solver_comparator.py` | consume the contract; D1 relative tolerance; D2 ladder; D3 quality flags preserving 275-288; D4 circular phase at **both** 293 and 316 |
| Create | `src/.../diffraction/benchmark_verdict.py` | derived status incl. the `suspect` rung |
| Fix | `docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py:149` | sort `xp` ascending before `np.interp`, or delete the script if superseded (Phase 0 decides) |
| Create | `tests/.../test_abscissa_contract.py` | TDD for D0 |
| Create | `tests/.../test_consensus_criteria.py` | TDD for D1–D4 |
| Create | `tests/.../test_l01_ship_external_oracle.py` | external-oracle assertion |
| Modify | `tests/.../test_multi_solver_comparator.py` | rewrite `test_consensus_majority_with_outlier`; re-baseline the other 21 (r1 finding 10) |
| Create | `docs/domains/orcawave/L01_aqwa_benchmark/benchmark_results/provenance.json` | Phase 0 output |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/benchmark_results/benchmark_report.json` | the one regenerated file, named explicitly (r1 finding 17 — no globs) |
| Update | `docs/plans/README.md` | index this plan |

---

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_descending_abscissa_raises` | D0 — descending `xp` is refused, not sorted silently | OrcaWave freqs from ascending periods | raises `ValueError` matching `"increasing"` |
| `test_np_interp_descending_xp_is_wrong` | D0 — pins the mechanism as a regression | the 10/20-point real grids | as-coded corr ≈ −0.40; corrected ≈ +1.00 |
| `test_insufficient_overlap_raises` | D0 — 3-of-20 shared points must fail loudly | AQWA 10-pt vs OrcaWave 20-pt grids | raises `AbscissaOverlapError` |
| `test_common_grid_is_ascending_and_within_both` | D0 — resampled grid is valid | the two real grids | strictly increasing, no extrapolation |
| `test_loader_sorts_freqs_and_raos_together` | D0 — the `run_3way_benchmark` invariant, now enforced | descending freqs + RAOs | both reordered by one index |
| `test_agreement_tolerance_is_relative` | D1 | two ~30 deg/m series 1% apart | `agrees is True` |
| `test_agreement_scale_is_symmetric` | D1 — no order dependence (r1 finding 14) | swap A and B | identical verdict |
| `test_two_solver_ladder_has_no_majority_rung` | D2 | 2 disagreeing solvers | level in `{FULL, DISAGREE}` |
| `test_three_solver_ladder_unchanged` | D2 — no regression | 3 solvers, 2 agreeing | `MAJORITY` |
| `test_empty_arrays_report_insufficient_data` | D3 | two empty arrays | `INSUFFICIENT_DATA`, `correlation is None` |
| `test_null_response_dof_is_not_insufficient` | D3 — preserves 275-288 (r1 blocker 3) | `peak_mag < 1e-10` | `NULL_RESPONSE`; status still reachable as `pass` |
| `test_identical_arrays_never_yield_pass` | D3/D4 — closes `IDENTICAL → FULL → pass` (r1 blocker 4) | two equal arrays | `quality=IDENTICAL`, derived status `suspect` |
| `test_phase_wrap_360_and_0_are_close` | D4 | 359° vs 1° | Δ = 2° |
| `test_max_phase_diff_is_circular` | D4 — covers line 316 (r1 finding 13) | phases straddling 0° | ≤ 180° |
| `test_rotational_rao_unit_mismatch_raises` | D5 | pitch deg/m vs rad/m | raises `ValueError` matching `"[Uu]nit"` (repo convention, r1 finding 11) |
| `test_l01_ship_heave_matches_aqwa_lis` | external oracle — **single falsifiable assertion** (r1 blocker 2) | aligned ship heave, both solvers | `corr > 0.95` **and** `rel_rms < 0.10`; no disjunction |
| `test_aqwa_lis_parser_matches_hand_checked_block` | the oracle's own oracle (r1 finding 15) | `.LIS` line 33311, heave 0.9224 @ 22 s | parser returns 0.9224 ± 1e-4 |
| `test_status_is_derived_not_authored` | derived status | report with `DISAGREE` | `fail`; hand-authored `pass` overridden |

---

## Phasing

**Phase 0 — provenance and attribution (no licence).** Determine which script
produced `benchmark_report.json`; determine empirically whether the 72 identical
matrix entries are same-input or zero-filled (r1 blocker 4); attribute the
−0.8551 to the abscissa defect or record what else contributes. Emits
`provenance.json`. **Gate: no fix is chosen before this phase reports.**

**Phase 1 — the abscissa contract (no licence).** D0, then D1–D5, then re-baseline
the 22 existing tests.

**Phase 2 — re-derive the ship verdict (no licence).** Inputs, named: AQWA
`001_SHIP_RAOS_REV2.LIS`; OrcaWave `orcawave_001_ship_raos_rev2.xlsx` (reader
selected in Phase 0 — `.xlsx` via `hull_library/rao_extractor.py`, or re-export
from the `.yml` if Phase 3 runs). Produces the regenerated report and the
measured correlations from which thresholds are set.

**Phase 3 — licensed re-run (GATED, `ace-win-1`).** Only if a genuine
disagreement survives Phase 2.

- **Dispatch path: VERIFIED 2026-08-03** — `dispatch-run.ps1 -Action submit`
  returned `LICENCE_OK dll=11.6c` / `DIFFRACTION_OK` at rc 0. Use
  `D:\ws\workspace-hub\scripts\windows\dispatch-run.ps1`; never direct SSH.
- **#1631 must be fixed first** — a lane reporting rc 0 on validator FAIL cannot
  confirm the re-run succeeded.
- Pin `host: "ace-win-1"`; the deckhand default is `ace-win-2`
  (`config/deckhand/policy.yml`), whose heartbeat last polled 2026-07-13
  (`queue/heartbeat/ace-win-2.json`).
- One floating Orcina seat fleet-wide; AQWA runs on a separate lane and does not
  contend.

---

## Acceptance Criteria

- [ ] Phase 0 `provenance.json` names the producing script for the committed report, and states whether the 72 identical entries are same-input or zero-filled
- [ ] All new tests pass: `uv run pytest tests/hydrodynamics/diffraction/ -v`
- [ ] No regression, measured as **both** counts: pass count ≥ 2121 **and** failure count ≤ 12 (the pre-existing `test_cli_integration.py` set) — r1 finding 9
- [ ] Comparing two solvers on non-overlapping or non-increasing grids **raises**; no code path silently sorts or truncates
- [ ] `test_np_interp_descending_xp_is_wrong` pins the mechanism so it cannot regress
- [ ] The regenerated L01 report states a per-DOF verdict with an explicit quality flag per DOF; no bare `1.0` and no `IDENTICAL` reaching `pass`
- [ ] A null-response DOF (`peak_mag < 1e-10`) does **not** force `incomplete`
- [ ] `CORR_MIN` and `tolerance` are set from Phase 2's measured values, with the chosen numbers and their justification recorded in the report
- [ ] The AQWA `.LIS` parser is validated against a hand-checked block before being used as an oracle
- [ ] The `.owd`-vs-`spec.yml` comparison is relabelled an input-fidelity check (#1633 item 1)
- [ ] No physical hostname in the diff — logical alias `ace-win-1` only
- [ ] `scripts/legal/legal-sanity-scan.sh` passes
- [ ] r2 review artifacts posted for all three providers, or a documented UNAVAILABLE per provider

---

## Deferred scope

- **#1633 item 2 and all of `L00_validation_wamit/`.** Requires acquiring WAMIT
  reference data. Note the tree is worse than #1633 states: 11 `pass` + 1
  `blocked`, and only 4 of 12 cases have a `reference_data.yaml` at all.
  **The `validation_config.yaml` acceptance criterion from r1 is withdrawn** —
  r1 finding 7 correctly showed it forced the deferred work. Status derivation in
  this plan is scoped to L01's own report. Follow-on under epic #1825.
- **L02 / L03 / L04.** The contract applies unchanged; each needs its own oracle.
- **#1631.** Gates Phase 3; planned separately.
- **Merging `.claude/rules/licensed-solver-dispatch.md`** (workspace-hub).

---

## Adversarial Review Summary

**r1 (2026-08-03):** Claude **MAJOR** (17 findings, 6 blockers); Codex UNAVAILABLE
(rc=124, skills-scan traversal limit); Agy UNAVAILABLE (auth timeout). Effective
coverage 1 of 3 — below T3 requirement.

Disposition of r1 blockers in this redraft:

| # | r1 blocker | Disposition |
|---|---|---|
| 1 | causal attribution falsified; `CORR_MIN` is binding | **Accepted, remedy declined.** Root cause is the abscissa contract; thresholds set in Phase 2 from re-measured data. Deliverable restated. |
| 2 | oracle test unfalsifiable | **Fixed** — single assertion, `corr > 0.95 and rel_rms < 0.10`. |
| 3 | D3 breaks the 275-288 null-response path | **Fixed** — `NULL_RESPONSE` quality + dedicated test. |
| 4 | 72 identical entries undiagnosed; `IDENTICAL → pass` | **Fixed** — Phase 0 diagnoses; `IDENTICAL` derives `suspect`. |
| 5 | Phase 2 input unnamed | **Fixed** — `.xlsx`/`.yml` named with reader selection in Phase 0. |
| 7 | L00 deferred yet required | **Fixed** — criterion withdrawn; L00 deferred wholesale. |

Findings 6, 8, 16 corrected in place. 9, 10, 13, 14, 17 folded into criteria and
the test list. 11 adopted (`ValueError`/`[Uu]nit`). 12 resolved (D-numbering now
D0–D5, one defect each).

**r2 result:** pending.

---

## Risks and Open Questions

- **Risk — the disagreement may be real.** The mechanism proof gives −0.40 on
  synthetic data, not −0.8551. If alignment does not close the gap, the models
  genuinely differ (mesh, draft, heading convention) and that is the finding.
  Phase 0 must attribute rather than assume.
- **Risk — `.xlsx` may not carry what is needed.** If the workbook lacks phase or
  full headings, Phase 2 depends on Phase 3 re-export, and the "no licence
  needed" property of Phase 2 is lost. Phase 0 resolves this.
- **Risk — the AQWA `.LIS` parser is unverified** and applies no unit
  normalisation. Now carries its own test rather than a prose "must".
- **Risk — regenerating reports rewrites committed artifacts.** Exactly one file
  is named; it lands in its own commit.
- **Open:** if a genuine disagreement survives, does it block epic #1825's
  three-way contract or land as a documented known-difference? Flagged for approval.
- **Open:** should `run_proper_comparison.py` be fixed or deleted? It is an ad-hoc
  docs-tree script superseded by `run_3way_benchmark.py`. Phase 0 decides;
  leaving a script with a known-silent-wrong `np.interp` in the tree is its own hazard.

---

## Complexity: T3

Spans a new contract module, a new verdict module, three new test modules, a
re-baseline of 22 existing tests, regenerated artifacts, and a gated licensed
re-run. Requires 3-provider adversarial review; r1 achieved 1 of 3, so **r2 must
restore coverage** before this plan goes to approval.
