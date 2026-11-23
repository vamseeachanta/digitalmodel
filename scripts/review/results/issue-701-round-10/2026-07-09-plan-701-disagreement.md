# Disagreement report — plan #701 (2026-07-09)

## Verdicts

| Provider | Verdict |
|---|---|
| claude | **MINOR** |

## Findings unique to each provider

A finding is 'unique to X' if its text appears in X's artifact but not
verbatim in any other provider's artifact.

### claude

- **Stale round-10 artifacts violate the plan's own precondition.** `scripts/review/results/issue-701-round-10/` already contains a zero-byte `2026-07-09-plan-701-claude.md` and a zero-byte `.err` sibling, while the Files-to-Change row (plan line 364) requires the "directory must be absent/empty before review" and the Approval Gate rejects any retained `.err` sibling. If the fanout overwrites the `.md` but leaves the stale `.err`, the plan's own promotion validation will (correctly) refuse the claude artifact and the round wastes a full fanout. The directory must be deleted before dispatching round 10.
- **The "hiredis is inactive" claim has no embedded proof.** Plan lines 33–36 assert `redis.utils.HIREDIS_AVAILABLE` is false because redis-py 7.2 rejects hiredis 2.2.3, and the removal therefore "preserves current correctness and performance behavior." The Embedded Verification Evidence section contains no command demonstrating this — the reproduction proofs only show hiredis 2.2.3 fails to *build* on 3.12, and it cannot be checked locally (the failed sync left `.venv` without redis installed). On Python 3.11, where 2.2.3 wheels install fine, a wrong version-gating claim would make the removal a (minor, performance-only) behavior change. One `python -c "import redis.utils; print(redis.utils.HIREDIS_AVAILABLE)"` proof on a 3.11 environment would close this; correctness is unaffected either way since the cache falls back per `cache.py:27`.
- **Superseded review rounds have no disposition.** `git status` shows ~14 untracked artifacts (`round-2` through `round-4` files, `issue-701-round-{5..9}/` directories) that the plan header calls "superseded evidence only" (line 13), but neither Files to Change nor the Approval Gate ("Commit/push the digitalmodel plan evidence") says whether they are committed to the PR branch, deleted, or left untracked. As written, the evidence commit could either drag nine rounds of noise into the PR or silently drop prior-round evidence.
- **The baseline cannot attribute failures caused by the removals themselves.** §Verification Commands step 2 builds the baseline worktree as base SHA *plus* "the two metadata removals and regenerated lock", so any full-suite failure caused by removing cx-Oracle/hiredis appears identically in baseline and final and passes the subset rule unflagged. This is largely defused by the no-consumer greps (reproduced: zero tracked imports) and the mandatory focused cache suites, and a true base-SHA baseline is genuinely uninstallable on this host — but the plan nowhere states this masking limitation of its own comparison design.
- **Live-GitHub and fetched-ref evidence is asserted, not reproducible in this environment.** The plan's issue states/labels (lines 187–192) and the `REMOTE_PYPROJECT_REFS_CHECKED=93` audit could not be re-verified (`gh` absent on this host, consistent with the plan's own note; the audit script was not re-run). The plan was reviewed against these as unverified assertions; no contradiction found, but they were not independently confirmed.
- The plan was additionally checked for: line-number drift in `pyproject.toml`/`README.md` (none), workflow-command misquotes (none), phantom file paths including all four sibling-repo references (none — all exist), transitive lock edges that would make the TDD lock tests un-greenable (none), a hidden next Windows-wheel blocker among the old C-extension pins (none found in the lock), missing pytest plugins for the baseline (`pytest-json-report`, `pytest-randomly` both present), an isolated-RED-runner collection failure via `tests/contracts/conftest.py`'s module-level assetutilities import (mechanism verified sound), and YAML `on`-key coercion in the workflow contract (BaseLoader choice is correct). None of those produced findings.

