# Issue Plans

This is the active issue-plan index for `digitalmodel`. Historical plans in
this directory predate this index and are not backfilled here.

| Issue # | Title / Slug | Plan File | Date | Status | Complexity | Notes |
|---|---|---|---|---|---|---|
| [#1969](https://github.com/vamseeachanta/digitalmodel/issues/1969) | prebuilt-case-schema-arm | `docs/plans/2026-08-05-issue-1969-prebuilt-case-schema-arm.md` | 2026-08-05 | plan-review | T2 | Premise refuted: prebuilt is fully reachable via OpenFOAMRunner and covered by 14 tests; only the YAML schema refuses it, with a message that is false as written. Option B (honest refusal) is a floor regardless; option A (wire the arm) is additive. r1 Claude inline; user approval pending. |
| [#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968) | mpi-resume-decomposition-validation | `docs/plans/2026-08-05-issue-1968-mpi-resume-decomposition-validation.md` | 2026-08-05 | plan-review | T2 | Premise 5 refuted: no serial/pool resume exists, so rejecting MPI resume deletes the only resume capability. Both plan and merged test are wrong; validated resume proposed with reject as a ranked alternative. r1 Claude inline; user approval pending. |
| [#1898](https://github.com/vamseeachanta/digitalmodel/issues/1898) | equipment-catalog-reference-data | `docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md` | 2026-07-29 | adversarial-reviewed | T3 | Catalog preflight complete; tubing source unavailable; Codex lanes APPROVE, Claude/Agy UNAVAILABLE; user approval pending. |
| [#1602](https://github.com/vamseeachanta/digitalmodel/issues/1602) | solver-neutral-riser-host-diffraction-orcaflex-hf-program | `docs/plans/2026-07-18-issue-1602-riser-host-diffraction-plan.html` | 2026-07-18 | plan-approved | T3 | User approved the reviewed parent plan on 2026-07-18. Parent-scope implementation may proceed with TDD; every child retains its independent plan/review/approval gate. |
| [#1470](https://github.com/vamseeachanta/digitalmodel/issues/1470) | registry-batch-coverage-wave-6 | `docs/plans/2026-07-08-issue-1470-registry-batch-coverage.md` | 2026-07-08 | plan-approved | T3 | Provider CLIs unavailable; two subagent reviews returned MAJOR, plan was revised, focused re-reviews returned APPROVE. User approved; implementation authorized with TDD. |
