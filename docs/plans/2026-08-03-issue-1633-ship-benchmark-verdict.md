# Plan for #1633: comparison-code correctness — make a wrong comparison impossible to record as a verdict

> **Status:** draft (r3 — rescoped after r1 MAJOR + r2 MAJOR×3)
> **Complexity:** T2 (reduced from T3 — the licensed run and the ship verdict moved to #714)
> **Date:** 2026-08-03
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1633
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** `docs/plans/evidence/2026-08-03-plan-1633-r1-claude.md` (MAJOR) · `...-r2-codex-{physics,testdesign,evidence}.md` (MAJOR ×3) · agy UNAVAILABLE both rounds (auth timeout); owner elected to proceed without a third provider

---

## What changed since r2, and why this plan is now smaller

r2 refuted this plan's own root cause. Two reviewers independently replayed
`scripts/run_benchmark_ship_raos.py` against the committed CSVs and reproduced
`benchmark_report.json` exactly (heave `-0.8551433929687695`, pitch rms
`26.153448148151465`). The recorded L01 numbers came from:

1. **two physically different models** — OrcaWave at 30 m depth / 9,017.95 mass
   vs AQWA at 500 m / 44,082, a 4.9× mass ratio (a *matched* OrcaWave model
   exists in the same directory and was never run);
2. **three coincident frequencies** (22, 19, 17 s) after 1% nearest-matching at
   `run_benchmark_ship_raos.py:278-284`; and
3. **hardcoded placeholder matrices** — `np.eye(6)*1000` and `np.eye(6)*100`,
   identical on both legs (`:176-196`), which is the entire explanation for the
   72 exactly-`1.0` correlations.

**The ship verdict therefore cannot be recovered by fixing code, and has moved to
[#714](https://github.com/vamseeachanta/digitalmodel/issues/714)** — which
already owned the 180-case licensed run and whose Scope anticipated this split
("if the comparison code is at fault, spin a fix issue").

What remains here is the fix issue: **the comparison code accepted all three of
those conditions and emitted a verdict anyway.** That is worth fixing on its own
merits, and it needs no licensed seat and no ship data.

Two prior root causes are also dead and must not be re-investigated: te/kg units
(#1550 — `np.corrcoef` is scale-invariant) and abscissa misalignment (real, but
in `run_proper_comparison.py:149`, which did not produce the artifact).

---

## Deliverable

A comparison layer that **refuses to produce a verdict** when its inputs cannot
support one — mismatched abscissae, insufficient sampling, fabricated matrices,
or absent data — instead of emitting a number that reads as a measurement.

---

## Resource Intelligence Summary

### Existing repo code

- `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py` (737 lines):
  - `_calculate_deviation_stats:203-230` — `errors = values2 - values1`, raw
    elementwise; `np.allclose → correlation = 1.0` at `217-218`.
  - `compare_raos:240-321` — passes `rao_a.frequencies.values` as metadata only
    (`264-268`); **no** equality check between the two solvers' grids.
  - Deliberate near-zero-magnitude override at `275-288` returning
    `correlation=1.0` (this is correct behaviour and must be preserved).
  - Independent **linear** phase path: `phase_diff` at `297`, consumed as
    `max_phase_diff` at `316`.
  - `compute_consensus:434-527` — agreement gate `corr > 0.99 and rms < self.tolerance`
    at `469-471`; `tolerance` documented "Relative … 5%" at `126` but applied as
    an absolute bound; ladder at `483-490` requires `len(high_pairs) >= 2` for
    `MAJORITY`, unreachable with two solvers.
  - JSON export calls `float(correlation)` at `677-684` — rejects `None`.
- `src/digitalmodel/hydrodynamics/diffraction/comparison_framework.py:32-41` —
  `DeviationStatistics` has **no** `quality` field. This file must change and was
  missing from r2's Files to Change (r2-evidence finding 7).
- `scripts/benchmark/run_3way_benchmark.py:436-437` — the correct ordering
  precedent: `frequencies = frequencies[sort_idx]` then
  `raw_raos = raw_raos[sort_idx, :, :]`, under the comment *"OrcFxAPI returns
  descending"*. (r2-evidence finding 5: 436-437, not 433-436.)
- `tests/hydrodynamics/diffraction/test_multi_solver_comparator.py` — **22**
  tests; `conftest.py:357-375` fixtures all share one increasing grid, so the
  abscissa contract should **not** require re-baselining them (r2-testdesign
  finding 7 corrected r2's assumption of a wide blast radius).
- `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py:251-343` — already
  reads native OrcaWave displacement-RAO sheets and sorts frequencies with RAOs.

### Gaps identified

- No abscissa contract; no sampling-adequacy floor; no provenance on inputs; no
  way for a comparison to decline. `DeviationStatistics` cannot express
  "unavailable".

### Evidence

**Issue states** (2026-08-03): `#1633` OPEN · `#714` OPEN · `#1631` OPEN · `#1550` CLOSED · `#1825` OPEN.

**Verified line citations** (r2-evidence finding 12 confirmed these):
`multi_solver_comparator.py` is 737 lines; lines 209, 217-218, 264-268, 275-288,
297, 316, 469-471, 483-490 support the descriptions above; the test file has 22
tests.

**Corrections carried in** (r2-evidence findings 5, 6, 8):
- `run_3way_benchmark.py` sorts at **436-437**, not 433-436.
- `L00_validation_wamit/validation_config.yaml:51` blocks case **2.4**, not 2.2
  (r1 said 2.2 and r2 of this plan repeated it).
- The AQWA file reports **two** versions: "generated by Aqwa in Workbench 2022 R2"
  (`:4`) and `Aqwa-Line 2024 R2` (`:36`). Cite both.

**Cross-repo citations** (r2-evidence finding 7): paths outside this repo are
named with their repo. `.claude/rules/licensed-solver-dispatch.md` and
`config/deckhand/policy.yml` live in **workspace-hub** and **deckhand**
respectively, not here. The `legal-sanity-scan.sh` acceptance criterion is
**withdrawn** — it is not executable from a digitalmodel checkout.

---

## Design decisions (each resolves a specific r2 blocker)

**D1 — Ordering: normalize at ingestion, fail closed at comparison.**
r2-evidence finding 3 caught r2 requiring both "no code path silently sorts" and
a sorting loader. Resolved: the **loader** sorts frequencies and RAOs together
(the `run_3way_benchmark.py:436-437` precedent). The **comparator** asserts
strictly-increasing and raises `AbscissaOrderError` if violated. Normalization
has exactly one home; the comparator never guesses.

**D2 — Overlap is interval coverage, not node coincidence.**
All three r2 lenses flagged this. AQWA spans 0.285599–2.252038 rad/s; OrcaWave
spans 0.285599–3.141593 rad/s; the AQWA interval is **wholly inside** OrcaWave's
(100% of the smaller domain, 68.853% of the union). Three coincident knots is not
three-point overlap — it is different sampling, which is exactly what
interpolation is for. The contract accepts this case and interpolates. It rejects
on: interval coverage below `MIN_COVERAGE`, fewer than `MIN_SAMPLES` source points
inside the shared interval, or a source gap wider than `MAX_GAP`.

**D3 — Evaluation grid is declared, not a union.**
r2-physics finding 3: a union grid weights the verdict toward the denser solver
and manufactures synthetic points. The evaluation grid is **the coarser solver's
grid restricted to the shared interval**, so every evaluation point is supported
by real data on at least one side and no extrapolation occurs.

**D4 — Interpolate the complex transfer function, not magnitude and phase.**
r2-physics finding 3 and r2-testdesign finding 6: independent magnitude/phase
interpolation crosses branch cuts (179°/−179°) and mishandles response zeros.
Interpolate real and imaginary parts, then derive magnitude and phase.

**D5 — Sampling adequacy is a first-class gate.**
The 3-point correlation is the defect that made the L01 artifact look like a
measurement. A comparison over fewer than `MIN_SAMPLES` points must return
`INSUFFICIENT_SAMPLING`, never a correlation.

**D6 — Inputs carry provenance and a synthetic flag.**
`np.eye(6)*1000` placeholders must be *unable* to reach a verdict. Matrix inputs
declare `source` (`solver` | `placeholder` | `unknown`); anything not `solver`
forces the derived status to `suspect` and can never yield `pass`.

**D7 — Thresholds are configuration with declared provenance, never fitted.**
r2-physics finding 6 and r2-testdesign finding 8: r2 proposed setting `CORR_MIN`
from the same observations being judged. Withdrawn. Thresholds live in a config
object with a documented justification field; this plan does not choose numeric
values for the ship — that belongs to #714 with matched inputs.

---

## Pseudocode

```
# --- ingestion (the only place ordering is normalized) ---
function load_solver_result(source):
    freqs, raos, matrices = read(source)
    order = argsort(freqs)
    return SolverResult(freqs[order], raos[order, ...], matrices,
                        provenance = declared_source_of(source))

# --- contract (fail closed; never guess) ---
function abscissa_contract(a, b, cfg):
    for s in (a, b):
        if not strictly_increasing(s.freqs): raise AbscissaOrderError(s.name)
    lo, hi = max(a.freqs[0], b.freqs[0]), min(a.freqs[-1], b.freqs[-1])
    if hi <= lo:                                    raise AbscissaOverlapError("disjoint")
    coverage = (hi - lo) / (min(a.span, b.span))
    if coverage < cfg.MIN_COVERAGE:                 raise AbscissaOverlapError(coverage)
    grid = coarser_of(a, b).freqs restricted to [lo, hi]      # D3
    if len(grid) < cfg.MIN_SAMPLES:                 return InsufficientSampling(len(grid))
    if max_gap(source_points_in(lo, hi)) > cfg.MAX_GAP: raise AbscissaGapError(...)
    return grid

function resample(rao, freqs, grid):                # D4 — complex, no extrapolation
    z = rao.magnitude * exp(1j * radians(rao.phase))
    zi = interp(grid, freqs, real(z)) + 1j * interp(grid, freqs, imag(z))
    return abs(zi), degrees(angle(zi))

# --- statistics ---
function deviation_stats(a, b, peak_magnitude):
    if empty(a) or empty(b):              return unavailable(INSUFFICIENT_DATA)
    if peak_magnitude < cfg.NULL_EPS:     return null_response()      # preserves 275-288
    if zero_variance(a) or zero_variance(b): return unavailable(INSUFFICIENT_DATA)
    if allclose(a, b):                    return stats(corr=1.0, quality=IDENTICAL)
    return stats(corr=corrcoef(a, b), quality=COMPARED)

function phase_stats(pa, pb):                       # replaces BOTH 297 and 316
    d = wrap_to_180(pb - pa)
    return circular_mean(d), circular_rms(d), max(abs(d))

# --- verdict ---
function derive_status(report):
    if any dof.quality in (INSUFFICIENT_DATA, INSUFFICIENT_SAMPLING): return "incomplete"
    if any input.provenance != "solver":                              return "suspect"   # D6
    if any dof.quality == IDENTICAL:                                  return "suspect"
    if consensus == DISAGREE:                                         return "fail"
    return "pass"
```

`unavailable(...)` carries `correlation=None`; the JSON exporter must emit `null`
rather than calling `float()` (`multi_solver_comparator.py:677-684`).

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/.../diffraction/benchmark_abscissa.py` | D1–D5: contract, evaluation grid, complex resampling |
| Modify | `src/.../diffraction/comparison_framework.py` | add `quality` + `correlation: Optional[float]` to `DeviationStatistics` (r2-evidence 7) |
| Modify | `src/.../diffraction/multi_solver_comparator.py` | consume contract; relative tolerance; 2-solver ladder; quality flags preserving 275-288; circular phase at 297 **and** 316; `None`-safe export at 677-684 |
| Create | `src/.../diffraction/benchmark_verdict.py` | `derive_status` incl. `suspect` and `incomplete` |
| Modify | `src/.../diffraction/output_schemas.py` | matrix `source` provenance field (D6) |
| Fix | `docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py:149` | sort `xp` ascending, or delete the script as superseded |
| Fix | `scripts/run_benchmark_ship_raos.py:176-196` | placeholder matrices must declare `source="placeholder"`, not masquerade as data |
| Create | `tests/.../test_abscissa_contract.py` | D1–D5 |
| Create | `tests/.../test_verdict_provenance.py` | D6 |
| Create | `tests/.../fixtures/abscissa_l01_grids.json` | committed deterministic fixture (r2-evidence 4) |
| Modify | `tests/.../test_multi_solver_comparator.py` | rewrite `test_consensus_majority_with_outlier:296-313`; expect `test_init_default_tolerance:66-84` to change if tolerance semantics change |
| Create | `docs/reports/2026-08-03-issue-1633-comparison-hardening.html` | declared artifact, now with a creation step (r2-evidence 11) |
| Update | `docs/plans/README.md` | index |

---

## TDD Test List

Every row states the exact expected value **and** why it is red on `origin/main`.
r2-testdesign finding 4 rejected eight rows of the previous list as vacuous; those
are removed or rewritten here.

| Test | Input | Expected | Red today because |
|---|---|---|---|
| `test_descending_freqs_raise` | freqs `[3.14 … 0.29]` descending | raises `AbscissaOrderError` | no such check exists |
| `test_loader_reorders_raos_with_freqs` | descending freqs + RAOs whose values encode their index | `raos[0]` corresponds to `min(freq)`; **value identity checked, not just sortedness** | loader does not exist; catches the permutation hole r2 flagged |
| `test_l01_grids_are_accepted_not_rejected` | committed fixture (AQWA 10-pt, OrcaWave 20-pt) | returns a grid; does **not** raise | reverses r2's wrong rejection; no contract exists |
| `test_disjoint_grids_raise` | `[0.1,0.2]` vs `[5.0,6.0]` | raises `AbscissaOverlapError("disjoint")` | no check |
| `test_coverage_below_min_raises` | 20% interval coverage, `MIN_COVERAGE=0.5` | raises with the coverage value in the message | no check |
| `test_three_samples_is_insufficient` | 3 points, `MIN_SAMPLES=5` | `INSUFFICIENT_SAMPLING`; correlation is `None` | **this is the L01 defect**; today it returns a correlation |
| `test_evaluation_grid_is_coarser_solver_restricted` | 10-pt and 20-pt grids | grid == AQWA points within shared interval; `len==10`, not 20 or 30 | no grid policy |
| `test_no_extrapolation_beyond_shared_interval` | grid endpoints | all points within both sources' ranges | no policy |
| `test_complex_interp_across_branch_cut` | phase `179°` → `-179°` | interpolated phase magnitude `180.0 ± 0.5`, **not** `0.0` | magnitude/phase interpolated independently today |
| `test_identical_input_yields_unit_correlation_and_identical_flag` | same array twice | `corr == 1.0` **and** `quality == IDENTICAL` | `quality` field does not exist |
| `test_placeholder_matrices_cannot_yield_pass` | `np.eye(6)*1000` both legs, `source="placeholder"` | derived status `"suspect"` | **the L01 defect**; today yields `1.0` → `pass` |
| `test_unknown_provenance_cannot_yield_pass` | `source="unknown"` | `"suspect"` | no provenance field |
| `test_null_response_dof_still_permits_pass` | `peak_mag < NULL_EPS` | `NULL_RESPONSE`; overall status may be `pass` | preserves `275-288`; guards against over-correction |
| `test_empty_arrays_export_null_not_crash` | empty arrays → full `export_report_json` | JSON contains `"correlation": null`; no `TypeError` | `float(None)` raises at `677-684` |
| `test_max_phase_diff_is_circular_and_nonzero` | phases straddling `0°` with a real 30° offset | `max_phase_diff == 30.0 ± 0.5` | today line 316 gives ~330; **pins a value, not `≤180`** |
| `test_two_solver_disagreement_is_not_full` | 2 solvers, corr 0.5 | `DISAGREE` exactly | today `NO_CONSENSUS`; pins one outcome, no set membership |
| `test_two_solver_agreement_is_full` | 2 solvers, corr 0.999, rel_rms 0.001 | `FULL` exactly | absolute tolerance blocks it |
| `test_relative_tolerance_admits_one_percent_on_pitch_scale` | two ~30 deg/m series 1% apart | `agrees is True` | `rms 0.3 > 0.05` absolute |

**Not included, deliberately:** no test asserting NumPy's own `np.interp`
behaviour (r2-testdesign 4 — passes before and after any fix), and no ship-oracle
test (moved to #714, where matched inputs make it meaningful).

---

## Acceptance Criteria

- [ ] **Every test above fails on `origin/main` and passes after.** Verified by running the new test files against a clean `origin/main` worktree and recording the failure list in the PR body. A test that is green before the change is removed or rewritten.
- [ ] Full suite: `uv run pytest tests/ -q` — compared against a **baseline captured on `origin/main` in the same environment**, node-ID by node-ID. No new failure node IDs. (r2-testdesign 8: raw counts are meaningless against 20,241 tests.)
- [ ] A comparison over 3 sampling points returns `INSUFFICIENT_SAMPLING` and no correlation
- [ ] Matrices with `source != "solver"` cannot produce `pass`
- [ ] The real L01 grids are **accepted** and interpolated; disjoint and low-coverage grids raise
- [ ] Interpolation is complex-valued; the branch-cut test pins `180.0 ± 0.5`
- [ ] `export_report_json` emits `null` for unavailable correlations
- [ ] `run_proper_comparison.py:149` is fixed or the script is deleted
- [ ] `scripts/run_benchmark_ship_raos.py` placeholder matrices declare their provenance
- [ ] Thresholds ship as configuration with a `justification` string; **no numeric threshold in this PR is derived from L01 data**
- [ ] HTML report rendered at the declared path
- [ ] r3 review artifacts recorded per provider

---

## Out of scope

- **The ship verdict, the matched-model run, and the 180-case investigation** — [#714](https://github.com/vamseeachanta/digitalmodel/issues/714), updated 2026-08-03 with the model-mismatch finding. Needs the licensed seat; dispatch path verified working.
- **#1631** — gates any licensed re-run; planned separately.
- **`L00_validation_wamit/`** and #1633 item 2 — needs WAMIT reference data. Tree state: 11 `pass` + 1 `blocked` (case **2.4**), only 4 of 12 cases have a `reference_data.yaml`.
- **L02 / L03 / L04** — the contract applies unchanged; each needs its own oracle.

---

## Adversarial Review Summary

| Round | Provider | Verdict |
|---|---|---|
| r1 | Claude | **MAJOR** — 17 findings, 6 blockers |
| r1 | Codex | UNAVAILABLE (rc=124; skills-scan traversal limit on workspace-hub's 8,502 skill files) |
| r1 | Agy | UNAVAILABLE (auth timeout) |
| r2 | Codex — physics lens | **MAJOR** — 6 findings |
| r2 | Codex — test-design lens | **MAJOR** — 8 findings |
| r2 | Codex — evidence lens | **MAJOR** — 12 findings |
| r2 | Agy | UNAVAILABLE; owner elected to proceed |

r2 was run from the digitalmodel worktree (56 skill files) to avoid the r1 timeout.
**Three codex lenses are one provider, not three** — lens diversity is not
cross-provider consensus, and a systematic codex blind spot would hit all three.
Recorded as a known limitation of this plan's review coverage.

**r3 result:** pending.

---

## Risks and Open Questions

- **Risk — `MIN_SAMPLES`, `MIN_COVERAGE`, `MAX_GAP` have no independent justification yet.** They are the gate that would have caught L01. Setting them from L01 is exactly the circular calibration r2 rejected. Proposal: derive `MIN_SAMPLES` from the sampling needed to resolve a resonant peak (a hydrodynamic argument, not a data-fitting one) and state that argument in the config's `justification`. Flagged for approval.
- **Risk — `test_complex_interp_across_branch_cut` encodes an assumption** that linear interpolation of the complex transfer function is the right physics near a response zero. It is standard practice and better than independent phase interpolation, but it is not exact. Recorded rather than hidden.
- **Risk — `output_schemas.py` provenance is a schema change** that may ripple into the four producers touched by PR #1636. Blast radius must be enumerated before implementation.
- **Open:** delete `run_proper_comparison.py` or fix it? It is a superseded ad-hoc docs-tree script carrying a silent-wrong `np.interp`. Deleting removes a hazard; fixing preserves history. Recommend delete, with the abscissa test in the suite as the durable record.

---

## Complexity: T2

Reduced from T3: the licensed run, the ship verdict, and the cross-machine
dependency all moved to #714. What remains is one new module, three modified
modules, three new test files, and one rewritten test — no licensed seat, no
cross-repo coordination.
