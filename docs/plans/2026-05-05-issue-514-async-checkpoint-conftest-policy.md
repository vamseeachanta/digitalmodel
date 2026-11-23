# Plan: digitalmodel #514 — Resolve or document the conftest ignore policy around async checkpoint tests

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/514
**Status:** plan-review
**Tier:** T3

## Context

`tests/conftest.py:9-36` contains:
```python
collect_ignore = [
    ...
    # Requires pytest-asyncio plugin (disabled due to hypothesis conflict)
    str(_tests_dir / "test_workflow_checkpoints.py"),
]
```

Per #513, `pytest-asyncio` packaging is now consistent across extras and async collection works. The ignore comment cites a "hypothesis conflict" — that justification may now be obsolete or may still apply for a different reason. Either:
- the ignore is **stale**: re-enable the test file
- the ignore is **still needed**: update the comment to name the current blocker and link a tracking issue

Without resolution, the repo carries silent test coverage loss with an outdated rationale. `tests/test_workflow_checkpoints.py` exists and is git-tracked.

## Plan

1. **Probe whether the ignore is stale.** Comment out the `str(_tests_dir / "test_workflow_checkpoints.py")` line in `tests/conftest.py`, then run:
   ```bash
   uv run pytest tests/test_workflow_checkpoints.py -v --tb=short
   ```
   Capture full output to `docs/sessions/2026-05-05-issue-514-collection-probe.md`. Three outcomes possible:
   - **(a)** All tests pass → ignore is stale, proceed to step 2
   - **(b)** Tests collect but some fail → mixed; classify failures (real bug vs. environment) and proceed
   - **(c)** Collection error or hypothesis-related crash → ignore still needed, proceed to step 4

2. **If outcome (a) or partial-(b): re-enable.** Restore the conftest with the ignore line removed. For any failing tests in (b), file follow-up issues with tracebacks (do NOT fix in this plan — out of scope). Leave the obsolete `pytest-asyncio` / hypothesis comment removed.

3. **If outcome (a): verify no broader regression.** Run `uv run pytest tests/ -q --tb=no` (full top-level test tree, not deep subdirs that have their own conftests). Confirm the previously-ignored file's tests are now collected and the suite shape changed by exactly the expected count.

4. **If outcome (c): document the current blocker.** Reproduce and capture the actual error. Update the conftest comment from the obsolete "hypothesis conflict" rationale to the **current** root cause (e.g., specific plugin interaction, specific test that crashes, specific Python/pytest version mismatch). File a new tracking issue for the blocker and reference it in the conftest comment:
   ```python
   # tests/test_workflow_checkpoints.py disabled — see #NNNN
   # Root cause: <one-line current blocker>
   str(_tests_dir / "test_workflow_checkpoints.py"),
   ```

5. **Smoke check.** Whichever path was taken, run `uv run pytest tests/conftest.py -h 2>&1 | head -5` (sanity that conftest loads without syntax error) followed by `uv run pytest tests/ --collect-only -q 2>&1 | tail -10` to confirm collection completes cleanly.

## Acceptance Criteria

- [ ] `docs/sessions/2026-05-05-issue-514-collection-probe.md` records the probe result with full pytest output
- [ ] Conftest is in one of two end states:
  - (a) ignore line removed AND `test_workflow_checkpoints.py` collected normally, OR
  - (b) ignore retained with comment naming the current blocker AND a tracking issue linked
- [ ] If state (a): any new test failures filed as separate child issues with tracebacks
- [ ] If state (b): conftest comment links to the new tracking issue by number
- [ ] No silent removal of test files; intent is preserved either as "now active" or "explicitly deferred"
- [ ] `pytest tests/ --collect-only` completes without error
