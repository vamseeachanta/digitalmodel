# Plan: digitalmodel #518 — Model-library regression tests for strict-vs-generated OrcaFlex semantic diffs

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/518
**Status:** plan-review
**Tier:** T2

## Context

Part of #515 (`status:plan-approved`). Middle issue of the semantic-diff trio: **#517 → #518 (this) → #519**. #517 defines the taxonomy and the `semantic_diff/taxonomy.py` module; this issue builds a regression-test harness against representative model-library cases that consumes that module; #519 then uses the harness to detect and close specific gaps.

Today, claims about "strict vs generated equivalence" rest on qualitative review of one model (`a01_catenary_riser`). Observed baseline: exact match for `VariableData/VesselTypes/LineTypes/Vessels/Lines`; differences in `General/Environment/Groups`; reverse-extraction confidence `0.88` with unmapped sections `VesselTypes/LineTypes/Vessels/Lines/Groups`. The harness must turn these qualitative observations into asserted, repeatable evidence.

**Implementation order: #518 must wait on #517's taxonomy module landing.** This plan assumes that module exists; if it does not, mark task 1 as a blocker.

## Plan

1. **Verify dependency.** Confirm `src/digitalmodel/solvers/orcaflex/semantic_diff/taxonomy.py` exists and exports `classify_diff` / `is_semantically_equivalent` (from #517). If not yet landed, this plan is blocked on #517 — document and pause.

2. **Build harness scaffolding.** New file `tests/solvers/orcaflex/semantic_diff/test_model_library_regression.py`:
   - `MODEL_LIBRARY_CASES = [...]` — list of representative cases starting with `a01_catenary_riser`. Each case names the strict YAML path, the spec.yml path, and an expected-diffs registry.
   - Helper `compare_strict_vs_generated(strict_path, spec_path) -> ComparisonResult` that runs the existing strict→spec→generated round-trip and feeds diffs through `classify_diff`.

3. **Author per-case expected-diffs registry.** For `a01_catenary_riser`, encode the observed baseline as expected diffs:
   - **Equal sections (assert exact equality):** `VariableData`, `VesselTypes`, `LineTypes`, `Vessels`, `Lines`
   - **Allowed normalization:** `Environment` `Yes/No` ↔ `true/false`, int → float (cited as `normalization-only`)
   - **Intentional omissions:** `General` view-state keys (the `_GENERAL_VIEW_KEYS` skip-set from #519 / current code)
   - **Unresolved (must surface, not silenced):** `Groups` omission, `Environment.VerticalWindVariationFactor`, `ImplicitVariableMaxTimeStep` — flag these as "investigate per #519"
   Store as YAML at `tests/solvers/orcaflex/semantic_diff/fixtures/a01_catenary_riser_expected.yml` so additions in #519 are data-only edits.

4. **Add reverse-extraction assertions.** Same harness file: parametrized test that runs `a01_catenary_riser` strict YAML through reverse extraction and asserts:
   - `confidence == 0.88` (or whatever the current value is — assert exact, not "≥ threshold")
   - `unmapped_sections == {"VesselTypes", "LineTypes", "Vessels", "Lines", "Groups"}` (exact set)
   The point is to lock current behavior so #519's fixes change these numbers visibly.

5. **Smoke check.** `uv run pytest tests/solvers/orcaflex/semantic_diff/ -v`. The new test should report PASS for all asserted-baseline diffs and emit clear FAIL with diff category for any unexpected change. Confirm test output makes "allowed vs unexpected diffs" explicit (not just a generic assertion error).

## Acceptance Criteria

- [ ] `tests/solvers/orcaflex/semantic_diff/test_model_library_regression.py` exists and consumes `taxonomy.py` from #517
- [ ] At least `a01_catenary_riser` is covered end-to-end (strict + spec + reverse-extraction)
- [ ] Per-case expected-diffs registry stored as YAML data file (extensible without code edits)
- [ ] Reverse-extraction confidence + unmapped-sections asserted exactly (not loosely)
- [ ] Test failure messages classify each unexpected diff via `classify_diff` (no opaque assertion errors)
- [ ] Plan explicitly identifies #519 as the consumer that will narrow expected-diffs registry as gaps close
