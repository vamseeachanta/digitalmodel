# Issue 2082 — plan review disposition

Owner: [digitalmodel 2082](https://github.com/vamseeachanta/digitalmodel/issues/2082). This is planning evidence, not implementation acceptance or owner approval.

## Actual provider results

- [Codex r1](issue-2082-codex-plan-r1.md): advisory APPROVE on the initial plan.
- [Claude initial route](issue-2082-claude-plan-r1.md): UNAVAILABLE, usage limit.
- [Claude Haiku r1](issue-2082-claude-plan-haiku.md): MAJOR, completed exit zero. Findings remain preserved verbatim.
- [Gemini r1](issue-2082-gemini-plan-r1.md): UNAVAILABLE; installed CLI rejected plan mode without experimental configuration. No settings were changed.

## Main-session disposition of Claude r1

| Findings | Disposition and evidence |
|---|---|
| 1, 4: baseline does not demonstrate repaired reload/complete regression coverage | Not an implementation blocker at planning stage. The harness deliberately executes unchanged merged source to reproduce false success. Replacing its output with post-repair success before implementation would destroy the reproduction. The amended plan explicitly separates historical evidence from future RED-first contract tests, retaining every proposed reload failure case. |
| 2: constructor thread argument unsupported | Official Model documentation, linked in the plan and checked by main, documents the constructor argument and observed property. The amended plan names the signature and limits native acceptance to the verified 11.6c environment. |
| 3: reload call unspecified | Accepted clarification: explicit fresh Model, LoadSimulation(sim), SmokeTestLine lookup and identical TimeHistory arguments now appear in the plan. Exceptions and failed postconditions both fail. |
| 5, 6: proof schema and unavailable thread observation | Accepted: typed additive keys are specified; a failed import/constructor cannot fabricate an observed count. |
| 7: finiteness threshold | Clarified: math.isfinite after float conversion; no unsupported engineering plausibility threshold. |
| 8, 9: diagnostic mode and hostname | Existing workflow behavior will be preserved. Host opt-in exists in run_probes, even though the single OrcaFlex probe does not add a hostname. Plan now names that entry point explicitly. |
| 10: cleanup | Accepted: initialize references and clear them in finally, lines first; no invented API close method or hard in-process timeout promise. |
| 11, 12: discovery and native harness ownership | Accepted: explicit pytest collection, operator/implementer ownership, proposed tracked PowerShell evidence harness, owned output directory, fake-child timeout check and bounded local native invocation. |

## Independent final discovery and inline correction

The final source-verification subagent found a MAJOR CI coverage gap: the proposed new test root was absent from `.claude/quality-gates.yaml`'s `tests-solver-smoke` explicit roots and pytest command. Main verified the live configuration and amended the artifact map and acceptance to register both, update `tests/DOMAINS.md`, verify touched-domain routing and prove actual configured-shard collection/execution. This correction is after the focused Claude r2 snapshot; it is not attributed to that provider's review. The general omission class is promoted to [follow-on 2083](https://github.com/vamseeachanta/digitalmodel/issues/2083), pending its own planning and approval.

## Focused Claude r2 and final inline correction

[Claude r2](issue-2082-claude-plan-r2.md) completed with MAJOR, stating all r1 findings were addressed and identifying only the missing explicit Windows platform statement as blocking. The reviewer imported its Linux working-directory and private host-memory assumptions; these do not describe the main session's verified Windows execution context. Main nevertheless accepted the useful platform clarification: native acceptance now explicitly requires Windows PowerShell 5.1+ on the licensed Windows 11.6c host. Offline checks remain portable. No new provider review round was dispatched.

Main also made observed threadCount read-points and `json.dumps(report, allow_nan=False)` explicit. The review's suggestion that ordinary json.dumps rejects nonfinite values is not adopted. The stopped/incomplete regression remains mandatory and will not be skipped merely because a fake separates the two properties.

Final disposition: known plan findings addressed inline; owner approval pending. Actual provider verdicts remain Codex advisory APPROVE, Claude r2 MAJOR before the final platform clarification, Gemini UNAVAILABLE. This is not unanimous approval. Plan and code review are separate gates.

Review transcript whitespace was normalized for repository diff checks without changing findings. Private host and scratch-path references introduced by the r2 reviewer were redacted; raw output is retained privately. No production implementation, deployment, queue submission or plan-approved label is part of this checkpoint.
