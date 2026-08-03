# Plan for #1633: OrcaWave-vs-AQWA — make the ship benchmark verdict derived and defensible

> **Status:** draft
> **Complexity:** T3
> **Date:** 2026-08-03
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1633
> **Client:** N/A
> **Lane:** lane:codex  <!-- matches the issue's existing lane: label -->
> **Review artifacts:** scripts/review/results/2026-08-03-plan-1633-claude.md | ...-codex.md | ...-agy.md

---

## Scope note (read first)

#1633 lists six resolution directions. This plan will deliver a **defensible
OrcaWave-vs-AQWA verdict for the L01 ship case** and the comparator correctness
required to reach one. Two of the six directions will be **deferred to follow-on
issues**, named explicitly in *Deferred scope* below, because they require
acquiring external data rather than fixing logic.

Item 3 of #1633 ("fix unit normalisation before re-running") is **already
closed** — see *Correction to the issue record*. It will not be re-done here.

---

## Resource Intelligence Summary

### Existing repo code

- Found: `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py`
  (737 lines on `origin/main`) — owns `compare_raos()`, `compute_consensus()`,
  `_calculate_deviation_stats()`. This is where all four verdict defects live.
- Found: `tests/hydrodynamics/diffraction/test_multi_solver_comparator.py` —
  30 tests, but the consensus tests either use *identical* solvers (both gates
  pass trivially) or assert membership in a three-outcome set. **No existing
  test can fail on any of the defects below.**
- Found: `tests/hydrodynamics/diffraction/test_multi_solver_units_guard.py` —
  landed with PR #1636 for #1550; covers translational te↔kg only.
- Found: `docs/domains/orcawave/L01_aqwa_benchmark/001_SHIP_RAOS_REV2.LIS` —
  3,391,338 bytes, Aqwa Workbench 2022 R2, 15 added-mass blocks, no input-data
  error. **A real external oracle for a ship already exists on disk.**
- Gap: no rotational-RAO unit handling anywhere in the comparator — a grep for
  `57.29|np.degrees|np.radians|deg2rad|rad2deg` against the module returns zero.
- Gap: nothing derives `validation_config.yaml` `status:` from a benchmark
  report; the field is hand-authored.

### Standards

Not applicable — this issue concerns solver-comparison methodology, not a
standards-derived constant. No `Citation` sidecar is required per
`.claude/rules/calc-citation-contract.md` (the values compared are
solver-produced, not standard-derived).

### LLM Wiki pages consulted

No relevant wiki pages — the defects are code-local. No wiki update will be
required, so `.claude/rules/wiki-sibling-routing.md` does not bind this plan.

### Documents consulted

- Issue #1633 body — the original finding (12 reports name one solver twice;
  L01 `NO_CONSENSUS` recorded as `status: pass`).
- Issue #1550 closing comment (PR #1636, merged 2026-07-26 as `4d465406`) —
  establishes on the record that the te/kg defect **cannot** explain the heave
  result, because `compute_consensus` derives from `np.corrcoef`, which is
  invariant under uniform scale.
- Issue #1631 — `bug(licensed-lane): runs report returncode 0 / finished despite
  validator Overall Status: FAIL`, `priority:critical`, `status:needs-plan`.
  Gates Phase 3 of this plan.
- `.claude/rules/licensed-solver-dispatch.md` (workspace-hub, branch
  `docs/licensed-solver-dispatch-rule`, **unmerged**) — measured dispatch
  contract for the licensed hosts; Phase 3 depends on it.
- `docs/domains/orcawave/L00_validation_wamit/validation_config.yaml` — 12 cases
  carrying hand-authored `status: pass` with `wamit_version: "v7.3"`.

### Gaps identified

- No relative-tolerance agreement criterion exists — the criterion is absolute.
- No valid consensus ladder exists for a 2-solver comparison.
- No guard exists against empty/degenerate arrays reporting perfect correlation.
- No circular-statistics phase comparison exists.
- No declared unit for rotational RAOs, and therefore no deg/rad guard.
- No test asserts any solver-produced value against an external reference.

### Evidence (embedded verification)

**Issue statuses** (verified 2026-08-03 via `gh issue view`):

- `#1633` — OPEN — bug(validation): OrcaWave benchmark suite compares OrcaWave to itself; the one AQWA cross-check is NO_CONSENSUS but marked status: pass
- `#1550` — CLOSED/COMPLETED (2026-07-26T23:49:39Z) — Added-mass/damping units inconsistency in DiffractionResults
- `#1631` — OPEN — bug(licensed-lane): runs report returncode 0 / finished despite validator Overall Status: FAIL
- `#1640` — OPEN — INITIATIVE: Trustworthy OrcaFlex results
- `#1825` — OPEN — [Epic] Diffraction results agree across solvers — one strict contract, three-way benchmarks

**File existence** (`ls -la`, 2026-08-03):

- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py` (737 lines)
- EXISTS: `docs/domains/orcawave/L01_aqwa_benchmark/001_SHIP_RAOS_REV2.LIS` (3,391,338 bytes)
- EXISTS: `docs/domains/orcawave/L01_aqwa_benchmark/aqwa_001_ship_raos_rev2.dat` (848,971 bytes)
- EXISTS: `tests/hydrodynamics/diffraction/test_multi_solver_comparator.py`
- MISSING (new — this plan creates): `tests/hydrodynamics/diffraction/test_consensus_criteria.py`
- MISSING (new — this plan creates): `tests/hydrodynamics/diffraction/test_l01_ship_external_oracle.py`

**Line excerpts** (`sed -n` against `git show origin/main:...multi_solver_comparator.py`):

D1 — the docstring promises *relative*, the code applies *absolute*:

```
126:        tolerance: Relative tolerance for agreement assessment (default 5%).
135:        tolerance: float = 0.05,
...
467:                # Agreement requires high correlation AND low rms
468:                # rms threshold is based on tolerance (default 0.05)
469:                pair_agrees[pk] = (
470:                    corr > 0.99 and rms < self.tolerance
471:                )
```

D2 — the ladder needs two agreeing pairs, but two solvers yield exactly one:

```
483:            if all(pair_agrees.values()):
484:                level = "FULL"
485:            elif len(high_pairs) >= 2:
486:                level = "MAJORITY"
487:            elif len(high_pairs) >= 1:
488:                level = "SPLIT"
489:            else:
490:                level = "NO_CONSENSUS"
```

D3 — absent data reports as perfect agreement:

```
215:        flat1 = values1.flatten()
216:        flat2 = values2.flatten()
217:        if np.allclose(flat1, flat2):
218:            correlation = 1.0
219:        else:
220:            correlation = float(np.corrcoef(flat1, flat2)[0, 1])
```

**Gap proofs:**

- `git grep -c "57.29\|np.degrees\|np.radians\|deg2rad\|rad2deg" origin/main -- src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py` → no match → confirms no rotational-unit handling exists.

**Reproduction proofs** (verify-against-repo-state):

The issue alleges a wrong *recorded verdict*, not a runtime crash, so the
reproduction is the committed artifact itself.

From `docs/domains/orcawave/L01_aqwa_benchmark/benchmark_results/benchmark_report.json`
(vessel `Ship_001_RAOs`, solvers `["AQWA", "OrcaWave"]`, dated 2026-02-05 05:36:21):

| DOF | mag corr | mag rms | phase corr | max Δphase |
|---|---|---|---|---|
| surge | 0.958 | 0.383 | 0.433 | 270.1° |
| sway | 0.981 | 0.504 | 0.035 | 302.6° |
| heave | **−0.855** | 0.698 | −0.925 | 67.5° |
| roll | 0.489 | 1.791 | 0.388 | 248.7° |
| pitch | 0.968 | **26.153** | −0.060 | 355.7° |
| yaw | 0.967 | 8.504 | 0.392 | 179.8° |

All 36 `added_mass_correlations` and all 36 `damping_correlations` in that same
file are **exactly `1.0`** — impossible for two BEM solvers on different meshes,
and explained by D3.

Four of six DOF exceed 0.95 magnitude correlation and every one is recorded
`NO_CONSENSUS` with `agreement_pairs: []`, which is D1 and D2 acting together.

- Reproduced at: 2026-08-03
- Failure mode observed matches issue claim: **PARTIALLY** — the recorded verdict
  is indeed untrustworthy, but the mechanism is the comparator's criteria, not
  (as #1633 item 4 proposes) a unit defect. The plan addresses the actual
  mechanism. See *Correction to the issue record*.

<!-- Distinct sources consulted: 9 (issue body, #1550, #1631, comparator source,
     comparator tests, L01 report JSON, L01 summary JSON, L00 validation_config,
     licensed-solver-dispatch rule). Minimum 3 required. -->

---

## Correction to the issue record

Two claims in #1633 will not survive contact with the current tree, and the plan
must not inherit them:

1. **#1633 item 3 says to fix unit normalisation "before re-running any
   cross-solver benchmark", blocked on #1550.** #1550 closed 2026-07-26 and its
   closing comment states the correction directly: `compute_consensus` derives
   from `compare_raos()` and the metric is `np.corrcoef`, which is invariant
   under uniform scale, so the te/kg defect *cannot* produce the −0.855 heave
   correlation. That work is done and it did not move the verdict.

2. **#1633 treats `NO_CONSENSUS` as the finding.** With two solvers there is
   exactly one pair, so `MAJORITY` (needs ≥2) and `SPLIT` (needs ≥1, which is
   already `FULL`) are both unreachable. The ladder collapses to FULL-or-
   NO_CONSENSUS, and given D1's absolute threshold, `FULL` is unreachable for
   any dimensional DOF. **`NO_CONSENSUS` on a 2-solver run carries no
   information about the solvers.** The informative content is per-DOF.

A separate observation, recorded here because it belongs to #1631 rather than
this plan: `benchmark_summary.json` in the same L01 directory describes a
*different* run (it targets `unit_box_spec.yml`, ran 3 solvers in 6.09 s) whose
AQWA leg failed —
`"AQWA LIS: **** INPUT DATA ERROR :LINE 42 END IN DECK HEADER ELM1 MISSING."` —
while the top level records `"success": true`. That is a live instance of #1631
and will be cited there, not fixed here.

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md` |
| Implementation — comparator | `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py` |
| Implementation — status derivation | `src/digitalmodel/hydrodynamics/diffraction/benchmark_verdict.py` (new) |
| Tests — consensus criteria | `tests/hydrodynamics/diffraction/test_consensus_criteria.py` (new) |
| Tests — external oracle | `tests/hydrodynamics/diffraction/test_l01_ship_external_oracle.py` (new) |
| Tests — existing, to extend | `tests/hydrodynamics/diffraction/test_multi_solver_comparator.py` |
| Re-derived L01 verdict | `docs/domains/orcawave/L01_aqwa_benchmark/benchmark_results/benchmark_report.json` |
| Human-facing report | `docs/reports/2026-08-03-issue-1633-ship-benchmark.html` |
| Plan review — Claude | `scripts/review/results/2026-08-03-plan-1633-claude.md` |
| Plan review — Codex | `scripts/review/results/2026-08-03-plan-1633-codex.md` |
| Plan review — Agy | `scripts/review/results/2026-08-03-plan-1633-agy.md` |

---

## Deliverable

A `benchmark_verdict` module plus a corrected `multi_solver_comparator` that
together produce a **derived, dimensionally sound OrcaWave-vs-AQWA verdict for
the L01 ship case**, asserted in CI against the existing AQWA `.LIS` external
oracle, with the hand-authored `status:` field replaced by a computed one.

---

## Pseudocode

```
function pair_agrees(comparison, tolerance):
    # D1 — relative, not absolute
    scale = max(rms_magnitude(reference_series), FLOOR)   # FLOOR guards near-zero DOF
    normalised_rms = comparison.rms_error / scale
    return comparison.correlation > CORR_MIN and normalised_rms < tolerance

function classify_consensus(pairs, n_solvers):
    # D2 — ladder must be valid for the actual solver count
    if n_solvers < 3:
        return FULL if all pairs agree else DISAGREE      # no MAJORITY/SPLIT rung exists
    else:
        ... existing 3-solver ladder ...

function deviation_stats(values_a, values_b):
    # D3 — absent data must not read as agreement
    if either series is empty or has zero variance:
        return stats with correlation = None, quality = INSUFFICIENT_DATA
    if allclose(values_a, values_b):
        return stats with correlation = 1.0, quality = IDENTICAL   # flagged, not silent
    return stats with correlation = corrcoef(...), quality = COMPARED

function phase_deviation(phase_a_deg, phase_b_deg):
    # D4 — circular, not linear
    delta = wrap_to_180(phase_a_deg - phase_b_deg)
    return circular_mean(delta), circular_rms(delta)

function derive_status(benchmark_report):
    # D5 of #1633 item 5 — status is computed, never hand-authored
    if any DOF quality is INSUFFICIENT_DATA:  return "incomplete"
    if overall consensus is DISAGREE:          return "fail"
    return "pass"
```

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py` | D1–D4: relative tolerance, solver-count-aware ladder, degenerate-array guard, circular phase |
| Create | `src/digitalmodel/hydrodynamics/diffraction/benchmark_verdict.py` | derive `status:` from the report instead of hand-authoring it |
| Create | `tests/hydrodynamics/diffraction/test_consensus_criteria.py` | TDD for D1–D4 |
| Create | `tests/hydrodynamics/diffraction/test_l01_ship_external_oracle.py` | first external-oracle assertion (#1633 item 6) |
| Modify | `tests/hydrodynamics/diffraction/test_multi_solver_comparator.py` | tighten `test_consensus_majority_with_outlier` (currently accepts any of three outcomes) |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/**/benchmark_report.json` | regenerated with corrected criteria |
| Rename | `scripts/benchmark/validate_owd_vs_spec.py` label strings | #1633 item 1 — call it an input-fidelity check, not validation |
| Update | `docs/plans/README.md` | index this plan |

---

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_agreement_tolerance_is_relative` | D1 — a 1% disagreement on a pitch-scale series agrees | two series ~30 deg/m differing 1% | `agrees == True` (today: False, rms 0.3 > 0.05) |
| `test_absolute_rms_does_not_gate_dimensional_dof` | D1 — threshold scales with magnitude | series ~30 vs series ~0.3, both 1% apart | both agree |
| `test_near_zero_dof_uses_floor_not_ratio` | D1 — no divide-by-zero blowup | reference series ≈ 0 | finite result, no `ZeroDivisionError` |
| `test_two_solver_ladder_has_no_majority_rung` | D2 — 2 solvers cannot return MAJORITY/SPLIT | 2 disagreeing solvers | level in `{FULL, DISAGREE}`, never MAJORITY |
| `test_two_solver_agreeing_pair_returns_full` | D2 — the reachable positive outcome works | 2 agreeing solvers | `FULL` |
| `test_three_solver_ladder_unchanged` | D2 — no regression for the 3-solver path | 3 solvers, 2 agreeing | `MAJORITY` |
| `test_empty_arrays_report_insufficient_data` | D3 — absent data is not agreement | two empty arrays | `quality == INSUFFICIENT_DATA`, `correlation is None` |
| `test_zero_variance_arrays_report_insufficient_data` | D3 — constant series has undefined correlation | two all-zero arrays | `INSUFFICIENT_DATA`, **not** `1.0` |
| `test_identical_arrays_flagged_as_identical` | D3 — genuine identity is labelled, not silent | two equal non-constant arrays | `quality == IDENTICAL` |
| `test_phase_wrap_360_and_0_are_close` | D4 — circular comparison | 359° vs 1° | Δ = 2°, not 358° |
| `test_phase_rms_is_circular` | D4 — RMS over wrapped residuals | phases straddling 0° | finite, < 180° |
| `test_rotational_rao_unit_mismatch_raises` | D5 — deg/rad guard | pitch in deg/m vs rad/m | raises `UnitMismatchError` |
| `test_l01_ship_heave_matches_aqwa_lis` | external oracle | parsed `001_SHIP_RAOS_REV2.LIS` heave | OrcaWave within stated tolerance, or an explicit recorded disagreement |
| `test_status_is_derived_not_authored` | #1633 item 5 | report with `DISAGREE` | derived status `fail`; a hand-authored `pass` is overridden |
| `test_status_incomplete_when_any_dof_lacks_data` | #1633 item 5 | report with one `INSUFFICIENT_DATA` DOF | `incomplete` |

---

## Phasing

**Phase 1 — comparator correctness (no licence, dev-primary).** D1–D4 plus the
tightened existing tests. Independent of any solver run.

**Phase 2 — re-derive the ship verdict (no licence, dev-primary).** Re-parse the
existing `001_SHIP_RAOS_REV2.LIS` and the existing OrcaWave results, run the
corrected comparator, measure D5 (the deg/rad hypothesis), and record the actual
per-DOF verdict. This phase produces the deliverable the issue asks for and
consumes **no licensed seat**.

**Phase 3 — licensed re-run (GATED, `ace-win-1`).** Only if a genuine
disagreement survives Phase 2. Two hard preconditions:

- **#1631 must be fixed first.** A lane that reports `returncode 0` on a
  validator FAIL cannot be trusted to tell us whether the re-run succeeded.
  Dispatching into it would produce another artifact of unknown validity.
- **Dispatch must use the Scheduled-Task path**, per
  `.claude/rules/licensed-solver-dispatch.md`: `dispatch-run.ps1 -Action submit`,
  never direct SSH (an SSH public-key logon token cannot complete a FlexNet
  checkout — measured `FlexNet Error 21`). Requests must pin
  `host: "ace-win-1"`, because the deckhand default `licensed_host` is
  `ace-win-2`, whose heartbeat last polled 2026-07-13.

---

## Acceptance Criteria

- [ ] All new tests pass: `uv run pytest tests/hydrodynamics/diffraction/ -v`
- [ ] No regression: full suite matches or exceeds the 2121-passed baseline recorded in #1550's closing comment
- [ ] Every `added_mass`/`damping` correlation of exactly `1.0` in a regenerated report is accompanied by an explicit `IDENTICAL` or `INSUFFICIENT_DATA` quality flag — no bare `1.0`
- [ ] The regenerated L01 report states a per-DOF verdict, and no DOF whose quality is `INSUFFICIENT_DATA` contributes to a `pass`
- [ ] At least one test asserts an OrcaWave-produced value against the AQWA `.LIS` external oracle (closes #1633 item 6)
- [ ] `validation_config.yaml` `status:` is computed; a hand-authored `pass` on a `DISAGREE` report fails the suite (closes #1633 item 5)
- [ ] The `.owd`-vs-`spec.yml` comparison is labelled an input-fidelity check wherever it is reported (closes #1633 item 1)
- [ ] The deg/rad hypothesis for pitch/yaw is either confirmed with a measured scale factor or explicitly refuted in the report
- [ ] No physical hostname appears anywhere in the diff — routing is by logical alias (`ace-win-1`) only
- [ ] `scripts/legal/legal-sanity-scan.sh` passes
- [ ] Review artifacts posted to `scripts/review/results/`

---

## Deferred scope

Named explicitly so the deferral is a decision, not an omission:

- **#1633 item 2 — populate or remove `L00_validation_wamit/`.** All 12 cases
  carry hand-authored `status: pass` while their `reference_data.yaml` files are
  `status: template` with no arrays. Resolving this requires *acquiring WAMIT
  reference data*, which is an external-data problem, not a logic fix. Will file
  as a follow-on under epic #1825.
- **L02 barge / L03 spar / L04 re-derivation.** The corrected comparator will
  apply to them unchanged, but each needs its own oracle check. This plan proves
  the method on the ship (L01), which is what was asked for.
- **#1631 itself.** Gates Phase 3; planned separately, `priority:critical`.
- **Merging `.claude/rules/licensed-solver-dispatch.md`** (workspace-hub). Phase 3
  depends on it being canonical rather than sitting on an unmerged branch.

---

## Adversarial Review Summary

<!-- To be filled after review fan-out. Not to be posted to GitHub until populated. -->

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | pending | |
| Codex | pending | |
| Agy | pending | |

**Overall result:** pending

---

## Risks and Open Questions

- **Risk — the heave result may be real.** If −0.855 survives Phases 1 and 2,
  the ship models genuinely disagree and the investigation moves to mesh, draft,
  or heading-convention differences between the `.dat` and the `.yml`. The plan
  must not assume the defects explain everything; the acceptance criteria
  require recording the surviving disagreement rather than suppressing it.
- **Risk — changing the criteria will change historical verdicts.** Regenerating
  reports will alter committed artifacts. Every regenerated file will be
  committed in its own commit, separate from the logic change, so the before/
  after is reviewable.
- **Risk — `CORR_MIN = 0.99` may still be too strict** for two different BEM
  codes on different meshes even after the RMS fix. Phase 2 will report the
  achieved correlations so the threshold is set from measurement, not taste.
- **Risk — the AQWA `.LIS` parser is itself unverified.** `aqwa_lis_parser.py`
  applies no unit normalisation (noted in #1633). Phase 2 must validate the
  parser against a hand-checked block of the `.LIS` before trusting it as an
  oracle, or the "external oracle" is just another untested path.
- **Open:** should a surviving genuine disagreement block the epic #1825
  three-way contract, or be recorded as a known-difference with a documented
  tolerance? Flagging for the approval decision.

---

## Complexity: T3

**T3** — spans the comparator, a new verdict module, two new test modules,
regenerated committed artifacts, and a gated licensed re-run on another host;
and it invalidates recorded verdicts across a validation tree. Per AGENTS.md
this requires **3-provider adversarial review** (Claude + Codex + Agy).
