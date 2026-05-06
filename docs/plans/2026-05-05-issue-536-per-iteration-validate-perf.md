# Plan: digitalmodel #536 — per-iteration model_validate perf for large sweep matrices

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/536
**Status:** plan-review
**Tier:** T2

## Context

`_apply_overrides` sweep loop at `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py:437-439` calls `apply_dotted_override` once per sweep parameter per combo. Each call does a full `model_dump()` + `model_validate()` round-trip on `ProjectInputSpec` (a deeply nested Pydantic model). For an N-sweep × M-combo campaign, that's **N×M full re-validations**. A real 680-combo / 3-sweep campaign would be ~2,040 round-trips ≈ 10-30s of pure validation overhead.

This is a **performance investigation**, not a known correctness bug. The plan is to **measure first, optimize only if measurement justifies it**. Code-review follow-up from #511 / PR #533, defect ID **m5**, Phase-2 blueprint risk R6, DEFERRED in commit `1d96aa63`. Sibling defects: #534, #535, #537.

## Plan

1. **Add benchmark harness.** New file `tests/solvers/orcaflex/modular_generator/test_campaign_perf.py` using `pytest-benchmark` (already a dev dependency — verify with `grep pytest-benchmark pyproject.toml`). Benchmark `CampaignGenerator.generate(spec_only=True)` at four scales: 1 combo, 10 combos, 100 combos, 680 combos (3-sweep). Use synthetic specs constructed in-memory; do not write to disk. Mark with `@pytest.mark.benchmark(group="campaign-sweep")` and `@pytest.mark.slow` so it stays out of the default suite.

2. **Profile to confirm validation is the dominant cost.** Run `uv run python -m cProfile -o /tmp/campaign-680.prof -s cumulative -m pytest tests/solvers/orcaflex/modular_generator/test_campaign_perf.py::test_perf_680_combos -v` and inspect with `pstats` or `snakeviz`. Capture top 20 cumulative-time entries to `docs/sessions/2026-05-05-issue-536-profile.md`. Confirm `model_validate` / `model_dump` dominate before any optimization.

3. **Decide based on data.** Document measurement results in the issue. Three branches:
   - **A** — measured cost <10s for 680 combos: close as "acceptable, benchmark retained as regression guard". No `src/` change.
   - **B** — measured cost 10-30s: prototype Option A from issue (batch all sweep mutations into one dict, single `model_validate` per combo) on a feature branch, re-benchmark. Promote if ≥2× speedup.
   - **C** — measured cost >30s: prototype Option B (`model_construct` to skip per-sweep validation, single `model_validate` at end). Stricter testing required since `model_construct` bypasses field validators.

4. **If branch B or C selected: add correctness regression test.** Before any optimization PR, add a test that builds a 5-combo × 3-sweep campaign and asserts the optimized path produces specs **byte-identical** (post-`model_dump()`) to the current per-iteration path. Without this guard, validation-skipping optimizations risk silent semantic drift.

5. **Smoke check.** Run benchmark twice, confirm <10% variance. If optimization landed, full `tests/solvers/orcaflex/modular_generator/` suite must pass with zero regressions in `TestApplyOverridesWithSweeps`.

## Acceptance Criteria

- [ ] Benchmark file `test_campaign_perf.py` exists with 4 scales (1, 10, 100, 680 combos)
- [ ] Profile output captured in `docs/sessions/2026-05-05-issue-536-profile.md`
- [ ] Per-combo cost reported in #536 issue comment with measurement methodology
- [ ] Optimization PR opened ONLY if measurement shows ≥2× speedup needed (else close as acceptable)
- [ ] If optimized: byte-identical correctness regression test passes
- [ ] No changes to `apply_dotted_override` public signature
