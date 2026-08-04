# Issue Plans

This is the active issue-plan index for `digitalmodel`. Historical plans in
this directory predate this index and are not backfilled here.

| Issue # | Title / Slug | Plan File | Date | Status | Complexity | Notes |
|---|---|---|---|---|---|---|
| [#1965](https://github.com/vamseeachanta/digitalmodel/issues/1965) | published-provenance-paths | `docs/plans/2026-08-04-issue-1965-published-provenance-paths.md` | 2026-08-04 | plan-review | T2 | Premise verification refuted the generated-page remedy (the page is a PAGE_EXCLUSION the drift gate never regenerates) and the issue's own claim that no in-repo producer exists. Leak is 3.25x larger than filed: a second published page, covered by no gate, carries the same 8 occurrences. The intended legal-scan verification is fail-open per workspace-hub #3804 and was observed PASSing over the live leak, so a repo-local detector replaces it. r1 Claude MAJOR (6 findings) folded in. Owner approval pending. |
| [#1898](https://github.com/vamseeachanta/digitalmodel/issues/1898) | equipment-catalog-reference-data | `docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md` | 2026-07-29 | adversarial-reviewed | T3 | Catalog preflight complete; tubing source unavailable; Codex lanes APPROVE, Claude/Agy UNAVAILABLE; user approval pending. |
| [#1602](https://github.com/vamseeachanta/digitalmodel/issues/1602) | solver-neutral-riser-host-diffraction-orcaflex-hf-program | `docs/plans/2026-07-18-issue-1602-riser-host-diffraction-plan.html` | 2026-07-18 | plan-approved | T3 | User approved the reviewed parent plan on 2026-07-18. Parent-scope implementation may proceed with TDD; every child retains its independent plan/review/approval gate. |
| [#1470](https://github.com/vamseeachanta/digitalmodel/issues/1470) | registry-batch-coverage-wave-6 | `docs/plans/2026-07-08-issue-1470-registry-batch-coverage.md` | 2026-07-08 | plan-approved | T3 | Provider CLIs unavailable; two subagent reviews returned MAJOR, plan was revised, focused re-reviews returned APPROVE. User approved; implementation authorized with TDD. |
