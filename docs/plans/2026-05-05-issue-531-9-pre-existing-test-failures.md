# Plan: digitalmodel #531 — 9 pre-existing test failures across 5 files

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/531
**Status:** plan-review
**Tier:** T2

## Context

Umbrella tracking issue covering 9 test failures across 5 files in `tests/solvers/orcaflex/`. These were inside `#510`'s file-scope footprint but excluded from the plan's diagnosed root-cause set (#510 only addressed cwd-drift at lines 142/174). Adversarial review of plan-510 flagged the gap as defect D1.

The 9 failures (enumerated in the issue body) span: `test_config.py` (1), `test_builder_registry.py` (2), `test_orcaflex_cli.py` (2), `test_schema_compat.py` (1), `mooring_tension_iteration_test.py` (3 — likely shared root cause). Cluster siblings: #529 (parallel-stats), #530 (fixture hoist).

Each failure is potentially test-side drift OR a genuine source regression — the plan must triage before fixing. Source-side fixes require a separate plan with adversarial review per the #510 Hard Constraint pattern.

## Plan

1. **Capture per-file tracebacks.** For each of the 5 files run with `-v --tb=short`:
   - `uv run pytest tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_config.py::TestAnalysisConfig::test_config_validation -v --tb=short`
   - `uv run pytest tests/solvers/orcaflex/modular_generator/test_builder_registry.py -v --tb=short`
   - `uv run pytest tests/solvers/orcaflex/test_orcaflex_cli.py -v --tb=short -k "test_universal_defaults_shown_in_help or test_cli_commands_from_module"`
   - `uv run pytest tests/solvers/orcaflex/modular_generator/test_schema_compat.py::TestSchemaValidation::test_invalid_water_depth_too_deep -v --tb=short`
   - `uv run pytest tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py -v --tb=short`
   Save all output to `docs/sessions/2026-05-05-issue-531-tracebacks.md` for traceability.

2. **Triage each failure.** For each traceback classify as:
   - **Test-side drift** (assertion outdated, fixture stale, helper renamed) → fixable in this plan, test-only edit
   - **Source regression** (genuine `src/` bug surfaced by the test) → file a child issue, do NOT fix in this plan
   - **Environment/config drift** (missing data file, env var, plugin) → fix or document
   For the 3-file `mooring_tension_iteration_test.py` failures, treat as one root cause unless tracebacks diverge.

3. **Fix test-side drift inline.** For each test-side classification, edit only the test file. Keep edits surgical (one assertion or one fixture call at a time) per the workspace `coding-style.md` Edit Safety rule. Run the affected test after each edit before moving on.

4. **File child issues for source regressions.** Anything classified as a `src/` regression gets a new GitHub issue with: traceback, suspected source file, the test that surfaced it, and a link back to #531. Add `discovered-via:531` reference. Do NOT touch `src/` in this plan.

5. **Smoke check.** Run `uv run pytest tests/solvers/orcaflex/ -q --tb=no` and confirm baseline `10 failed → 1 failed (or 0)` plus any deferred-to-child failures. Update issue body with final disposition.

## Acceptance Criteria

- [ ] All 9 failures triaged with classification recorded in `docs/sessions/2026-05-05-issue-531-tracebacks.md`
- [ ] All test-side-drift failures fixed (test-only edits)
- [ ] Each source-regression failure has a child issue filed with traceback + cross-link
- [ ] `git diff src/` is empty (zero `src/` modifications in this plan)
- [ ] Full `tests/solvers/orcaflex/` suite shows no NEW failures vs. baseline
- [ ] Issue #531 body updated with final disposition table
