# Issue #1575 plan review — r1 consolidated

> **Reviewed plan SHA:** `a2b22603be1118b6d5c5b64c874126377c59e569`
> **Reviewed plan:** `docs/plans/2026-07-13-issue-1575-openfoam-case-definition-contract.md`
> **Disposition:** MAJOR consensus; revised r2 draft requires fresh review

## Provider verdicts

| Provider | Verdict | Consolidated blocking themes |
|---|---|---|
| Claude | MAJOR | case-source ambiguity, incomplete schema/ownership, dependency and validation gaps |
| Codex | MAJOR | prebuilt locator/semantic loss, legacy allowlist, shared-workflow dependency conflicts |
| Gemini | MAJOR | rank/path portability, unit/frame, function-object and size-limit enforcement gaps |

Provider-specific raw review text is not reproduced here. This artifact records
the orchestrator-confirmed three-provider consensus themes applied to r2.

## Consolidated blockers and r2 resolution map

1. **Authored/prebuilt ambiguity.** V1 allowed semantic fields beside a relative
   manifest without a safe case locator, so prebuilt execution could drop every
   authored field. R2 will define a discriminated source type, support only
   `kind: authored` in v1, and reserve prebuilt for a future opaque
   Deckhand-locator plus canonical-definition-digest contract.
2. **Unknown/unconsumed fields.** Generic and batch roots, execution controls,
   legacy compatibility, and variant targets were not exhaustively closed. R2
   will freeze exact root/nested allowlists and an accepted-leaf-to-consumer
   ledger tested for zero or duplicate consumers.
3. **Execution and rank authority.** Timeout/setFields controls and MPI
   `n_subdomains` had competing owners. R2 will create one typed execution
   mapping and derive MPI subdomains only from batch workers, rejecting drift.
4. **Frame, units, and portable paths.** Motion amplitude units, axes, vertical
   convention, portable case names, and symlink confinement were incomplete. R2
   will freeze SI/global axes, DOF-specific units, a portable component grammar,
   and #1565-rooted pre-mutation checks.
5. **Function-object/model ownership.** `write_control` was accepted without a
   complete typed/rendered path and model ownership was conditional. R2 will make
   one immutable function-object config the owner and pass every leaf to the
   neutral #1574 renderer.
6. **Portfolio order.** #1565/#1575/#1576 shared workflow, identity, and tests
   without an order; #1574 privacy API was not pinned. R2 will require
   #1565 → #1574 API gate → #1575 → #1576 and record exact upstream merge SHAs
   before implementation.
7. **Existing size violations.** The intended edits touched 680-, 420-, and
   445-line files plus 87-, 81-, and 71-line functions. R2 will require named
   behavior-preserving splits and literal 400/50 AST enforcement before feature
   edits.
8. **Non-executable validation.** The legal command referenced a nonexistent
   local script and test/lint/compile commands were incomplete. R2 will pin the
   cross-repository scanner invocation and literal focused/full commands.

## Gate

No item in this artifact is an approval. The revised plan will remain `draft`
until fresh r2 adversarial review has no MAJOR and the user explicitly approves.
