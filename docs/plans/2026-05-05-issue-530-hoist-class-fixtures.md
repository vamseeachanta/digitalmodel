# Plan: digitalmodel #530 — hoist class-scoped fixtures in test_orcaflex_converter_enhanced.py

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/530
**Status:** plan-review
**Tier:** T3

## Context

`tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py` defines 4 pytest fixtures (`temp_output_dir`, `sample_dat_files`, etc.) inside `class TestOrcaFlexConverterEnhanced` (L35-62). Sibling classes `TestPerformance` (L415) and `TestCLI` (L448) consume the fixtures by parameter name but are not subclasses — class-scoped fixtures don't propagate sideways. Result: 3 ERRORs at setup (`fixture 'temp_output_dir' not found`, `fixture 'sample_dat_files' not found` x2).

Spillover from #510. Cluster: #529 (parallel-stats bug, same file), #531 (pre-existing failures across 5 other files). Test-only fix; no `src/` impact.

## Plan

1. **Confirm fixture inventory.** Read `tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py` L1-70 and list the 4 fixtures with their scopes and any internal `self`-binding. Pytest module-level fixtures cannot bind `self`, so any `self.X` reference in fixture bodies will need rewriting before the hoist.

3. **Hoist to module level (Option 1, recommended).** De-indent the 4 `@pytest.fixture` defs out of `class TestOrcaFlexConverterEnhanced` to module level (immediately after imports). If any fixture currently uses `self`, refactor to a plain function — fixtures are usually pure factories, so this should be a no-op. Preserve fixture scopes exactly (`function` is the pytest default).

4. **Verify all three classes pick them up.** Run `uv run pytest tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py -v` and confirm:
   - `TestCLI::test_cli_batch` passes
   - `TestCLI::test_cli_single_file` passes
   - `TestPerformance::test_large_batch_performance` passes (or skips if it has a separate skip marker)
   - All `TestOrcaFlexConverterEnhanced` tests still pass

5. **Smoke check.** `uv run pytest tests/solvers/orcaflex/test_orcaflex_converter_enhanced.py --collect-only` first to verify zero collection errors, then full run. `git diff src/` must be empty.

## Acceptance Criteria

- [ ] `TestCLI::test_cli_batch` passes
- [ ] `TestCLI::test_cli_single_file` passes
- [ ] `TestPerformance::test_large_batch_performance` collects without ERROR (passes or skips per its own logic)
- [ ] All `TestOrcaFlexConverterEnhanced` tests continue to pass — zero regressions
- [ ] `git diff src/` is empty (test-only fix)
- [ ] No new conftest.py file introduced (Option 2 rejected for blast radius)
