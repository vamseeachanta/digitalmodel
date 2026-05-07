# Plan for digitalmodel #559: `test_full_matrix_interpolation` mis-asserts diagonal dominance on a fixture that physics does not require to be diagonally dominant

> **Status:** draft
> **Complexity:** T2
> **Date:** 2026-05-06
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/559
> **Review artifacts:** scripts/review/results/2026-05-06-plan-559-claude.md | ...-codex.md | ...-gemini.md

---

## Resource Intelligence Summary

### Existing repo code
- Found: `digitalmodel/tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py:308-356` — `test_full_matrix_interpolation` body containing the failing dominance loop at lines 327-331.
- Found: `digitalmodel/tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py:42-93` — fixture `sample_database` constructs the 6×6 added-mass matrix with diagonals [1.0, 1.1, 1.3, 0.05, 0.05, 0.03] × `added_mass_diag` and couplings A[1,3]=A[3,1]=0.05× (Sway-Roll) and A[2,4]=A[4,2]=0.1× (Heave-Pitch).
- Found: `digitalmodel/src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/coefficients.py` — `CoefficientDatabase.get_added_mass_matrix(freq)` is production code, but the failure is **not** in production: the test fixture violates the test's own assertion. No production code change required.
- Gap: nothing missing — this is a test-correctness defect, not a coverage gap.

### Standards
| Standard | Status | Source |
|---|---|---|
| n/a | n/a | Test asserts a generic linear-algebra property (diagonal dominance), not a standards-derived numeric. |

Diagonal dominance is **not** a physics property of hydrodynamic added-mass matrices. Standard references (Faltinsen 1990 §3.2; DNV-RP-C205 §6.2) state added-mass matrices are symmetric and positive-semidefinite, but couplings between translational and rotational DOFs (off-diagonal entries) are dimensionally moments and have no physical bound relative to the rotational-row diagonal entries (which are moments of inertia, dimensionally different from translational mass). The original assertion conflates "diagonally dominant" (a specific row-sum inequality from numerical analysis) with the looser intuition that "couplings are smaller than direct terms" — true for translational rows in this fixture, false for rotational rows.

### LLM Wiki pages consulted
- No relevant wiki pages located under `knowledge/wikis/marine-engineering/` for the term "added mass diagonal dominance" (as expected — not a standard physics property).

### Documents consulted
- Prior plan at this exact path (the one this revision overwrites) — claimed root cause was strict-greater-on-equal-floats, recommending `>` → `>=`. That diagnosis was incomplete: it would shift the failure from row 3 (where off-diag equals diag) to row 4 (where off-diag is 2× diag).
- Issue #559 body — captures only the (3,1) failure site visible at fail time.
- `digitalmodel/CLAUDE.md` — confirms repo override rules (Required Gates, Plan Locality); plan locality for this Route lives in `digitalmodel/docs/plans/`.

### Gaps identified
- The original test asserts an invariant the fixture is not designed to satisfy. The fix must either (a) weaken the assertion to match what the fixture actually establishes, (b) reshape the fixture, or (c) replace the assertion with a defensible physics invariant.

### Evidence (embedded verification)

**File existence** (`ls` 2026-05-07T03:09:00Z, from `/mnt/local-analysis/workspace-hub/digitalmodel`):
- EXISTS: `tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py`
- EXISTS: `docs/plans/2026-05-05-issue-559-rao-full-matrix-interpolation.md` (this plan, being revised)
- EXISTS: `src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/coefficients.py`

**Line excerpts** — fixture coupling assignment, `tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py:78-82`:
```
            A_matrix[2, 4] = A_matrix[4, 2] = added_mass_diag * 0.1   # Heave-Pitch
            A_matrix[1, 3] = A_matrix[3, 1] = added_mass_diag * 0.05  # Sway-Roll

            B_matrix[2, 4] = B_matrix[4, 2] = damping_diag * 0.08
            B_matrix[1, 3] = B_matrix[3, 1] = damping_diag * 0.04
```

Rotational-row diagonal scales, lines 59-66:
```
            A_matrix = np.diag([
                added_mass_diag * 1.0,   # Surge
                added_mass_diag * 1.1,   # Sway
                added_mass_diag * 1.3,   # Heave (largest)
                added_mass_diag * 0.05,  # Roll
                added_mass_diag * 0.05,  # Pitch
                added_mass_diag * 0.03   # Yaw
            ])
```

So `A[3,3] = 0.05·scale = A[3,1] = 0.05·scale` (equal, fails `>`) and `A[4,4] = 0.05·scale < A[4,2] = 0.1·scale` (fixture violates dominance, fails `>=` too).

**Reproduction proofs** (verify-against-repo-state, per Step 1.5 of `issue-planning-mode`):

```
$ uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_full_matrix_interpolation -xvs
...
tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_full_matrix_interpolation FAILED

=================================== FAILURES ===================================
____________ TestHydroRAOIntegration.test_full_matrix_interpolation ____________
tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py:330: in test_full_matrix_interpolation
    assert abs(A_matrix[i, i]) > abs(A_matrix[i, j]), \
E   AssertionError: Added mass should be diagonally dominant at (3,1)
E   assert 624146.2952202364 > 624146.2952202364
E    +  where 624146.2952202364 = abs(624146.2952202364)
E    +  and   624146.2952202364 = abs(624146.2952202364)
=========================== short test summary info ============================
FAILED tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_full_matrix_interpolation - AssertionError: Added mass should be diagonally dominant at (3,1)
```

- Reproduced at: 2026-05-07T03:09:17Z (cwd `/mnt/local-analysis/workspace-hub/digitalmodel`)
- Failure mode observed matches issue claim: YES at (3,1). Numerical confirmation that swapping `>` for `>=` would shift the failure to (4,2): the fixture has `A[4,2]=0.1·scale ≈ 1,249,535` versus `A[4,4]=0.05·scale ≈ 624,768`, off-diag exceeds diag by 2×. So the prior plan's `>` → `>=` is **incorrect** in isolation.

<!-- Source count: prior plan (1) + test fixture file (2) + issue body (3) + production module location (4) + reproduction (5) = 5 ≥ 3 minimum. -->

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `digitalmodel/docs/plans/2026-05-05-issue-559-rao-full-matrix-interpolation.md` |
| Test under repair | `digitalmodel/tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py` |
| Production module (read-only) | `digitalmodel/src/digitalmodel/marine_ops/marine_analysis/hydrodynamic_coefficients/coefficients.py` |
| Plan review — Claude | `scripts/review/results/2026-05-06-plan-559-claude.md` |
| Plan review — Codex | `scripts/review/results/2026-05-06-plan-559-codex.md` |
| Plan review — Gemini | `scripts/review/results/2026-05-06-plan-559-gemini.md` |

---

## Deliverable

A repaired `test_full_matrix_interpolation` that asserts only the invariants the fixture genuinely establishes (symmetry + positive-semidefiniteness + translational-row dominance), and a new dedicated test that documents the rotational-row coupling pattern explicitly so future readers cannot mistake the original "diagonally dominant" mis-assertion for a physics property.

---

## Decision: which option

**Choosing Option 1 — weaken the assertion to translational-row dominance only — and add structural assertions that capture what the fixture actually proves.**

Rationale:

- (a) **Marine engineering correctness.** The 6×6 added-mass matrix mixes translational mass (rows 0-2) with rotational moments of inertia (rows 3-5), and off-diagonal couplings are dimensional moments. The intuition "diagonal > off-diagonal entry-wise" holds for the translational rows of this fixture (S/Sw/H scales 1.0/1.1/1.3 vs couplings 0.05/0.1) but is not a physics property — Faltinsen, DNV-RP-C205 specify only symmetry and positive-semidefiniteness as universal properties of A. The fixture deliberately codes rotational diagonals as 0.03–0.05× scale (because rotational moments-of-inertia for this notional vessel are small relative to mass) while keeping couplings at 0.05–0.1× scale; that asymmetry was the author's choice and is physically defensible.
- (b) **Blast radius.** Option 1 touches **only the test file**. Option 2 (reshape fixture) ripples into 4 sibling tests (`test_database_loading`, `test_coefficient_interpolation_accuracy`, `test_added_mass_affects_natural_frequency`, `test_coupling_terms_affect_response`) all of which read this fixture and assert numeric expectations against the current scales. Option 3 (replace dominance with positive-semidefinite + symmetry) is the strongest physics claim but loses the translational-row sanity check we currently get for free.
- (c) **False-negative risk.** Option 1 keeps a tight check on translational rows (where dominance IS a fixture invariant) and substitutes the rotational-row check with the universally-true invariants (symmetry already asserted at line 321; add PSD via eigenvalue check). False-negative risk: lower than current state (which asserts a property that does not hold). Option 2 false-negative risk: medium (changes coupling magnitudes, may mask interpolation defects in `test_coupling_terms_affect_response`). Option 3 alone false-negative risk: medium (PSD only, drops the translational row sanity).

**Hybrid landed:** Option 1 for the rotational rows + retained strict dominance for translational rows + add explicit PSD eigenvalue check for full-matrix sanity. This is the smallest defensible change that puts the test on solid physics ground.

---

## Pseudocode

Replace the `for i in range(6) / for j in range(6)` dominance loop (lines 327-331) with:

```
# Translational rows (0-2): diagonals are dominant by fixture construction
for i in [0, 1, 2]:
    for j in range(6):
        if i != j:
            assert abs(A_matrix[i, i]) > abs(A_matrix[i, j]), (
                f"Translational-row dominance violated at ({i},{j}): "
                f"|A[{i},{i}]|={abs(A_matrix[i,i]):.3e} not > |A[{i},{j}]|={abs(A_matrix[i,j]):.3e}"
            )

# Rotational rows (3-5): no diagonal-dominance physics requirement; assert PSD globally instead.
eigenvalues = np.linalg.eigvalsh(A_matrix)
assert np.all(eigenvalues > -1e-6), (
    f"Added-mass matrix must be positive semi-definite; "
    f"min eigenvalue={eigenvalues.min():.3e}"
)
```

Add a new sibling test `test_added_mass_matrix_psd` that verifies the PSD invariant holds across the full interpolation frequency range (0.2 → 1.4 rad/s, 50 samples) and a new `test_translational_dominance_holds_across_frequencies` covering rows 0-2 across the same sweep.

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `digitalmodel/tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py` | Replace mis-asserted dominance loop in `test_full_matrix_interpolation`; add `test_added_mass_matrix_psd` and `test_translational_dominance_holds_across_frequencies` |

No production-code change. No fixture-data change.

---

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_full_matrix_interpolation` (repaired) | Symmetry + translational-row dominance + PSD at `freq=0.7` | sample_database | passes; chart still emits |
| `test_added_mass_matrix_psd` (new) | PSD holds for `A(ω)` across 50 frequencies in [0.2, 1.4] | sample_database | min eigenvalue ≥ -1e-6 at every sample frequency |
| `test_translational_dominance_holds_across_frequencies` (new) | Strict diagonal dominance for rows 0,1,2 holds across 50 frequencies | sample_database | for each ω, `|A[i,i]| > |A[i,j]|` for `i ∈ {0,1,2}`, `j ≠ i` |
| (regression check, not new) | Sibling tests in same file still pass | sample_database | all 6 sibling tests pass |

---

## Acceptance Criteria

- [ ] `uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_full_matrix_interpolation -xvs` passes.
- [ ] `uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_added_mass_matrix_psd -xvs` passes (new test).
- [ ] `uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py::TestHydroRAOIntegration::test_translational_dominance_holds_across_frequencies -xvs` passes (new test).
- [ ] `uv run pytest tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py -x` runs all 8 tests in the class with zero failures (no regression).
- [ ] Comment in `test_full_matrix_interpolation` documents *why* rotational rows do not satisfy strict dominance, citing the fixture's choice of rotational-diagonal scale (0.05) versus heave-pitch coupling scale (0.1).
- [ ] Review artifacts posted to `scripts/review/results/2026-05-06-plan-559-{claude,codex,gemini}.md`.

---

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | APPROVE | 1× P2 (dimensional inconsistency in retained translational-row check — call out as fixture invariant, not physics invariant), 4× P3 (relative PSD tolerance, inline symmetry assertion before `eigvalsh`, `B_matrix` coverage gap, sibling-test enumeration), 5× author questions. APPROVE because root cause correctly identified, reproduction embedded with numerical witness, Option 1 well-justified on blast-radius and false-negative grounds. |
| Codex | UNAVAILABLE — INCOMPATIBLE_VERSION | CLI 0.128.0 in known-bad range (>= 0.124.0), upstream `openai/codex#19945`, see workspace-hub#2479. Remediation: `scripts/install/pin-codex.sh` to downgrade. |
| Gemini | NO_OUTPUT | Wrapper returned no review content (likely related to `feedback_gemini_sandbox_overlay_blindness.md`). |

**Overall result:** Single-provider Claude APPROVE with documented Codex/Gemini unavailability. Per `feedback_permission_gate_blocks_cross_review.md`, treat as r1-complete with transparent provenance; user-approval surface accepts the documented provider gaps.

**Review artifacts (2026-05-07T03:21:51Z):**
- Claude: `scripts/review/results/20260507T032151Z-2026-05-05-issue-559-rao-full-matrix-interpolation.md-plan-claude.md`
- Codex: `scripts/review/results/20260507T032151Z-2026-05-05-issue-559-rao-full-matrix-interpolation.md-plan-codex.md`
- Gemini: `scripts/review/results/20260507T032151Z-2026-05-05-issue-559-rao-full-matrix-interpolation.md-plan-gemini.md`

**P2/P3 polish deferred to implementation review:** none of Claude's findings are blocking. The P2 (mark retained translational-row check as *fixture invariant*, not physics invariant) and 4 P3s should be folded into the implementation PR as test-code comments and assertion refinements, not into the plan text.

Revisions made based on review:
- r1 returned APPROVE — no plan revisions required to clear blockers. P2/P3 findings tracked as implementation-PR checklist items.

---

## Risks and Open Questions

- **Risk:** Translational-row dominance check may mask a future production-code defect in `CoefficientDatabase.get_added_mass_matrix` that introduces large couplings into translational rows. Mitigation: the new `test_added_mass_matrix_psd` runs across 50 frequencies, providing broader sanity.
- **Risk:** PSD via `np.linalg.eigvalsh` is sensitive to floating-point noise near zero; tolerance `-1e-6` may need calibration if the fixture is later scaled. Mitigation: tolerance is documented inline; a future plan can switch to a relative-tolerance form if scaling changes.
- **Risk:** The fixture's `A[4,2]=A[2,4]=0.1·scale` coupling is intentionally large to make `test_coupling_terms_affect_response` meaningful. Reshaping it (Option 2) would weaken that sibling test. Option 1 sidesteps this entirely.
- **Open:** Should the comment in the assertion cite a specific reference (Faltinsen 1990 §3.2 or DNV-RP-C205 §6.2)? Default: no — citation contract (`/.claude/rules/calc-citation-contract.md`) applies to standards-derived constants in production calc paths, not to test-only commentary. Flag for reviewer if they disagree.
- **Open:** The original test was authored as integration coverage for the coefficient database; should the dominance check be re-homed to a unit test on the database itself rather than living inside the integration suite? Default: no — keep the in-place fix; the broader re-organization is a separate refactor.

---

## Complexity: T2

**T2** — requires understanding the marine-engineering invariant (added-mass matrices are PSD + symmetric, NOT diagonally dominant in general) and producing two new physics-grounded tests beyond the surgical assertion fix. Bigger than a one-line tier-1 patch; smaller than an architecture change.
