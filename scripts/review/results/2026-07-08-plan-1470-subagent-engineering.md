## Verdict
MAJOR

## Retrieval
Verified locally: plan file, issue body, registry seed rows, batch logic, schedule/calc tests, wiki source pages for X-Drill and RSU-0077, review artifacts, legal-scan script. Ran focused tests: `tests/drilling_riser/test_schedule_assembly.py tests/drilling_riser/test_assembly_golden.py` -> `22 passed`.

## Findings
1. `RSU-0077` is under-specified despite being the obvious source-backed lift candidate. The plan leaves prioritization open at `docs/plans/2026-07-08-issue-1470-registry-batch-coverage.md:210`, but the wiki source already documents RSU-0077 as a shallow-water tension workbook with four cases and operating tensions (`ri-sbop-weight-data-family.md:223-246`). The TDD list has no RSU-0077-specific contract/schema/golden test.

2. The runnable-count acceptance is too weak for an issue titled/specified to lift `8/21`. Plan lines `112` and `168` allow "above 8/21, or explicit reviewed no-lift source-gap result," which can pass without closing any runnable gap. Given RSU-0077 source evidence exists, this is not approval-ready.

3. Legal scan command is invalid for this worktree. Plan line `184` requires `workspace-hub/scripts/legal/legal-sanity-scan.sh --repo=digitalmodel --diff-only`; local verification returned `ERROR: Repository not found` because this checkout is a worktree outside the path that `--repo=digitalmodel` resolves.

4. Plan artifact status is inconsistent. Header cites `scripts/review/results/2026-07-08-plan-1470-codex.md` at line `10`, but that file does not exist locally. Existing 2026-07-07 provider artifacts are all `UNAVAILABLE`, and the 2026-07-08 Claude artifact is also `UNAVAILABLE`.

## Blockers
- Need explicit RSU-0077 implementation/test contract or a source-backed reason it cannot be lifted.
- Need a real runnable-count acceptance criterion, not a no-lift escape hatch.
- Need corrected legal-scan invocation for the actual worktree.
- Need valid no-MAJOR review artifacts before presenting as approval-ready.
