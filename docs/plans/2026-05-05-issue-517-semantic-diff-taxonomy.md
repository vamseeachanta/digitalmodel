# Plan: digitalmodel #517 — Define OrcaFlex YAML semantic-diff taxonomy and comparison policy

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/517
**Status:** plan-review
**Tier:** T2

## Context

Part of #515 (`status:plan-approved`). The semantic-diff trio is **#517 (this) → #518 → #519**: this issue defines the taxonomy, #518 builds the regression-test harness against it, #519 closes specific fidelity gaps using the classification.

Today the repo can detect differences between strict and generated OrcaFlex YAML, but it has no consistent way to classify whether a diff is physics-significant, loadability-significant, reference-resolution-significant, normalization-only, UI/cosmetic-only, or an intentional omission. Without that taxonomy, comparison code is ad hoc and the project risks over-claiming semantic equivalence.

**Recommended implementation order: #517 first (this) → #518 → #519.** #518 and #519 both depend on the registry/module this plan produces.

## Plan

1. **Survey existing comparison code.** `git grep -n "semantic\\|equivalence\\|YamlDiff\\|strict_vs_generated" src/digitalmodel/solvers/orcaflex/ tests/solvers/orcaflex/` to enumerate every place a diff/equivalence decision is currently made. Capture findings in `docs/sessions/2026-05-05-issue-517-survey.md`. This avoids inventing a taxonomy that contradicts existing tacit policy.

2. **Author taxonomy doc.** New file `docs/domains/orcaflex/SEMANTIC_DIFF_TAXONOMY.md` with these sections:
   - **Categories** — six labels with one-line definitions: `physics-significant`, `loadability-significant`, `reference-resolution-significant`, `normalization-only`, `ui-cosmetic-only`, `intentional-omission`
   - **Decision rules** — flowchart-style ordering for classifying a given diff (e.g., "if key affects mooring tension result → physics; if `Yes/No` ↔ `true/false` → normalization-only; if listed in `_GENERAL_VIEW_KEYS` skip-set → intentional-omission")
   - **Examples** — at least one observed diff per category drawn from the issue's evidence (`General` view-state keys, `Environment` `Yes/No` and int→float, `Groups`, dormant `ImplicitVariableMaxTimeStep`)
   - **Equivalence policy** — one-paragraph statement of when two YAMLs may be claimed "semantically equivalent" (no diffs in physics/loadability/reference categories)

3. **Implement the taxonomy as code.** New module `src/digitalmodel/solvers/orcaflex/semantic_diff/taxonomy.py` (create directory if absent) exposing:
   - `class DiffCategory(StrEnum)` with the six labels
   - `classify_diff(section: str, key: str, strict_value, generated_value) -> DiffCategory` — uses a structured registry (per-section mapping), not ad hoc sets
   - `is_semantically_equivalent(strict_yaml: dict, generated_yaml: dict) -> tuple[bool, list[Diff]]` — returns equivalence boolean plus categorized diffs
   Keep the registry tables small and data-driven so #519 can extend them without code changes.

4. **Reference from existing code/tests.** Add `from digitalmodel.solvers.orcaflex.semantic_diff.taxonomy import classify_diff` import to whichever comparison utility was found in step 1. If no central comparison utility exists yet, leave the module standalone with a TODO note linking to #518 (which will be the primary consumer).

5. **Smoke check.** `uv run pytest tests/solvers/orcaflex/ -q --collect-only` to confirm zero collection errors. Add unit tests in `tests/solvers/orcaflex/semantic_diff/test_taxonomy.py` covering each category with one fixture diff per category.

## Acceptance Criteria

- [ ] `docs/domains/orcaflex/SEMANTIC_DIFF_TAXONOMY.md` exists with six categories, decision rules, examples, equivalence policy
- [ ] `src/digitalmodel/solvers/orcaflex/semantic_diff/taxonomy.py` exposes `DiffCategory`, `classify_diff`, `is_semantically_equivalent`
- [ ] Per-section registry is data-driven (extensible without modifying `classify_diff` body)
- [ ] Unit tests cover at least one fixture diff per category, all pass
- [ ] At least one existing comparison call site or test imports from the new module
- [ ] Document explicitly cross-references #518 (consumer) and #519 (gap-closer)
