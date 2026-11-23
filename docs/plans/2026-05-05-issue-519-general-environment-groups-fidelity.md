# Plan: digitalmodel #519 — Classify and fix General/Environment/Groups fidelity gaps in OrcaFlex generation

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/519
**Status:** plan-review
**Tier:** T2

## Context

Part of #515 (`status:plan-approved`). Final issue of the semantic-diff trio: **#517 → #518 → #519 (this)**. #517 defines the taxonomy and code module; #518 builds the regression-test harness; this issue uses both to triage and close specific observed gaps in `General`, `Environment`, and `Groups` sections of generated strict YAML.

Observed gaps from baseline review of `a01_catenary_riser`:
- **General** drops 7 view/UI keys: `DefaultViewMode`, `DefaultShadedFillMode`, `DefaultShadedProjectionMode`, `DefaultViewSize`, `DefaultViewCentre`, `DefaultViewAzimuth`, `DefaultViewElevation`
- **Environment** changes `true/false` ↔ `Yes/No`, int → float, drops `VerticalWindVariationFactor` from generated sample
- **Groups** present in original strict YAML but missing from generated — current policy unstated

**Implementation order: #519 depends on both #517 (taxonomy) and #518 (regression harness) landing.** This plan assumes both exist; without them, gap-classification is uncalibrated.

## Plan

1. **Verify dependencies.** Confirm `semantic_diff/taxonomy.py` (#517) and `tests/solvers/orcaflex/semantic_diff/test_model_library_regression.py` (#518) exist. Without them this plan cannot classify gaps consistently — document and pause if either is missing.

2. **Classify each observed gap using the #517 taxonomy.** For every key in the issue's observed-gaps list, decide its `DiffCategory` and record in `docs/domains/orcaflex/GENERAL_ENVIRONMENT_GROUPS_GAPS.md`:
   - **General view keys** — likely `ui-cosmetic-only` → `intentional-omission`. Document the skip-set explicitly.
   - **Environment `Yes/No` ↔ `true/false`** — `normalization-only` (OrcaFlex accepts both; no physics impact). Lock as expected.
   - **Environment int → float** — `normalization-only` (numeric equivalence). Lock as expected.
   - **`VerticalWindVariationFactor` missing** — likely `physics-significant` (affects wind loading). Treat as a real gap to fix.
   - **`Groups` missing** — needs decision: `ui-cosmetic-only` (UI grouping, no physics) vs. `loadability-significant` (some workflows reference groups). Investigate before deciding.
   - **`ImplicitVariableMaxTimeStep`** — confirm dormant/no-op key per existing strip behavior; classify as `intentional-omission`.

3. **Fix physics/loadability gaps in builders.** For each gap classified as physics- or loadability-significant in step 2, edit the responsible builder under `src/digitalmodel/solvers/orcaflex/modular_generator/builders/`:
   - `environment_builder.py` — emit `VerticalWindVariationFactor` when present in spec
   - `groups_builder.py` — implement preservation if step 2 decides Groups must round-trip (file already exists at `src/digitalmodel/solvers/orcaflex/modular_generator/builders/groups_builder.py`)
   - `general_builder.py` — codify the view-key skip-list as a named constant `_GENERAL_VIEW_SKIP_KEYS` with a docstring linking to the taxonomy doc
   Each builder edit must be accompanied by an update to the #518 expected-diffs YAML moving the formerly-unresolved key into either `intentional-omission` or `equal`.

4. **Re-run #518 regression harness.** `uv run pytest tests/solvers/orcaflex/semantic_diff/test_model_library_regression.py -v`. Every gap from step 2 should now appear in the expected-diffs registry with its final category — no unexplained diffs remain in `General/Environment/Groups` for `a01_catenary_riser`.

5. **Smoke check.** Full `uv run pytest tests/solvers/orcaflex/modular_generator/ -q` to confirm builder edits don't break sibling tests. Confirm `groups_builder.py` (already present) still has its existing tests passing if any.

## Acceptance Criteria

- [ ] `docs/domains/orcaflex/GENERAL_ENVIRONMENT_GROUPS_GAPS.md` exists with per-key classification using #517 taxonomy
- [ ] `general_builder.py` exposes named `_GENERAL_VIEW_SKIP_KEYS` constant with taxonomy-doc link
- [ ] `Environment.VerticalWindVariationFactor` is preserved end-to-end (spec → strict)
- [ ] `Groups` policy is decided (preserve or skip), implemented, and documented
- [ ] #518 regression harness reports zero unexplained `General/Environment/Groups` diffs for `a01_catenary_riser`
- [ ] Each builder edit has a paired update to the #518 expected-diffs YAML
