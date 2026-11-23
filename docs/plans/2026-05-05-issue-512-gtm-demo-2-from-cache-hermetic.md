# Plan: digitalmodel #512 — Fix GTM Demo 2 --from-cache path and make GTM tests hermetic

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/512
**Status:** plan-review
**Tier:** T2

## Context

Two real defects in `examples/demos/gtm/`:

1. **Functional regression:** `demo_02_wall_thickness_multicode.py` `--from-cache` path raises `NameError: name 'PipeDefinition' is not defined`. The chart-building code on the cache path still references engineering symbols that are only loaded on the non-cache path. Surfaces via `examples/demos/gtm/tests/test_gtm_demos.py::test_from_cache_smoke[wall_thickness]`.

2. **Test-harness flaw:** The same test file asserts that HTML reports already exist on disk. Only one of those reports is actually git-tracked; the others are local-only build artifacts. Tests pass on a dirty workspace and fail in CI / on a clean clone. The validation suite does not actually validate the user-facing workflow.

Both are repeatable failures with concrete reproduction commands. No source/test files outside `examples/demos/gtm/` need touching.

## Plan

1. **Reproduce both failures.**
   ```bash
   PYTHONPATH=examples/demos/gtm:src uv run pytest \
     examples/demos/gtm/tests/test_gtm_demos.py::test_from_cache_smoke -v
   ```
   Confirm `NameError: name 'PipeDefinition' is not defined`. Then enumerate which assertions in `test_gtm_demos.py` reference pre-existing HTML outputs — `git grep -n 'exists\\|is_file\\|html' examples/demos/gtm/tests/test_gtm_demos.py`. Record findings in `docs/sessions/2026-05-05-issue-512-repro.md`.

2. **Fix demo 2 cache path.** Read `examples/demos/gtm/demo_02_wall_thickness_multicode.py`. Locate the `--from-cache` branch and the `PipeDefinition` reference in chart-building. Two acceptable approaches:
   - **(a)** Move chart-building code into a function that takes pre-resolved values (no `PipeDefinition` reference)
   - **(b)** Eagerly load the engineering symbols module on the cache path before chart code runs
   Prefer (a) for cleaner separation. Make sure cache-path output is byte-identical to non-cache-path output for the same inputs (this is the "fully cache-driven" criterion in the issue).

3. **Make GTM tests hermetic.** Edit `examples/demos/gtm/tests/test_gtm_demos.py`:
   - Replace any "asserts file at fixed path exists" with "test generates output into `tmp_path` (pytest fixture) and asserts the generation succeeded".
   - For tests that need a cached input, the test itself runs the cache-population step in its own `tmp_path` rather than reading from a workspace-relative location.
   - Remove or rewrite the test that asserts pre-existing HTML reports — those are build artifacts, not test inputs.

4. **Extend cache-path coverage.** The issue explicitly requires "validate the whole cached report-generation flow". Add a test that:
   - calls the demo's non-cache path once into `tmp_path`
   - calls the `--from-cache` path against the same `tmp_path`
   - asserts the cache-path produced an HTML report and its key engineering values match the non-cache run (parse the HTML for known field names, or check a JSON sidecar if one exists)

5. **Smoke check (on a clean tree simulation).**
   ```bash
   PYTHONPATH=examples/demos/gtm:src uv run pytest examples/demos/gtm/tests/test_gtm_demos.py -v
   ```
   Then to simulate clean-clone hermeticity:
   ```bash
   git stash -u  # hide all untracked artifacts
   PYTHONPATH=examples/demos/gtm:src uv run pytest examples/demos/gtm/tests/test_gtm_demos.py -v
   git stash pop
   ```
   Both invocations must pass.

## Acceptance Criteria

- [ ] `test_from_cache_smoke[wall_thickness]` passes (no `NameError`)
- [ ] All assertions reference outputs generated within the test's `tmp_path`, not workspace-tracked files
- [ ] Test suite passes on a clean clone (verified via `git stash -u` simulation)
- [ ] New end-to-end cache-flow test exists and passes (non-cache run → cache run → equivalence assertion)
- [ ] `git diff src/` is empty — fix is contained to `examples/demos/gtm/`
- [ ] `docs/sessions/2026-05-05-issue-512-repro.md` records the pre-fix failure state for traceability
