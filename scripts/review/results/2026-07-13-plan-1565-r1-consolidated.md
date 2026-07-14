# Issue #1565 plan review — r1 consolidated MAJOR findings

## Review target

- Plan:
  docs/plans/2026-07-13-issue-1565-openfoam-external-work-root.md
- Reviewed pushed head:
  c69ca6e5afb2ae326e53d526f84b5fe57e5e2fd5
- Review posture: adversarial; default non-approval
- R1 outcome: MAJOR

This artifact consolidates the r1 findings supplied by the orchestrator with the
locally verified Claude review and Gemini theme summary. The standalone Codex
#1565 artifact was not present in this worktree; the Codex verdict/themes below
are therefore recorded from the orchestrator-supplied consensus rather than
represented as a verbatim provider transcript. A Codex downstream review of
#1575 independently confirmed that #1575 must consume the merged #1565
RunIdentity/layout and pin its SHA.

## Provider verdicts and themes

| Provider | Verdict | MAJOR themes |
|---|---|---|
| Claude | MAJOR | Untrusted YAML could select a host root; deterministic namespaces lacked locks; checkpoint identity omitted source/tool identity; no Deckhand activation/readback/canary oracle; the 680-line workflow and 445-line test violated 400/50 without a split; exact legal/full-suite commands were absent. |
| Codex | MAJOR, per orchestrator-supplied r1 consensus | Shared source/package/config/input/tool identity and landing order had to be frozen upstream; downstream #1575/#1576 could not redefine layout/checkpoint identity; hosted filesystem authority and exact verification remained blocking. |
| Gemini | MAJOR | The 680-line workflow/89-line router required explicit decomposition; the fingerprint omitted clean source/package/tool identity; a two-file frozen result set conflicted with #1576 artifact_index.json; Deckhand systemd root provisioning/readback required separate owner authority. |

## Consolidated blocking findings

### R1-1 — Hosted filesystem authority was granted to request YAML

The r1 plan allowed run_batch.work_root to select an arbitrary absolute writable
directory whenever DIGITALMODEL_WORK_ROOT was unset. A Deckhand request could
therefore acquire host placement authority that belongs to the operator.

Required disposition:

- hosted mode will require the operator-provisioned environment root;
- hosted YAML will provide only a validated relative namespace;
- an absolute config root will require explicit trusted-local mode;
- hosted mode will fail before side effects when operator policy is missing.

R2 location: Normative Design Contract §1.

### R1-2 — Host activation lacked an owner gate and end-to-end oracle

Unit tests alone could not prove that the running Deckhand agent inherited the
root, that heavy work stayed outside the scope checkout, or that returned
stdout/stderr and bounded files did not disclose the path.

Required disposition:

- code merge and host mutation will be separate transactions;
- an owner will approve the exact environment/service/directory preview;
- privacy-safe readback will precede activation;
- a synthetic dispatch canary will prove external heavy work, two-file bounded
  return, retry/isolation behavior, and path redaction;
- the canary will not run a real solver.

R2 location: Normative Design Contract §6 and Acceptance Criteria.

### R1-3 — Deterministic namespace reuse was race-unsafe

Identical inputs selected the same directory, while cleanup/checkpoint operations
had no intrinsic run/case ownership lock. Deckhand's current host-level
serialization was an external implementation detail and did not protect local or
future execution surfaces.

Required disposition:

- atomically create and fsync an identity-bound ownership marker;
- acquire exclusive run and case locks before checkpoint reads or mutations;
- use bounded contention and token-matched release;
- reclaim only dead, expired locks through an atomic tombstone claim;
- never guess liveness or adopt an unowned directory;
- add real concurrency/stale-lock tests.

R2 location: Normative Design Contract §3 and TDD Test List.

### R1-4 — The fingerprint was not a complete execution identity

The r1 fingerprint bound selected config but omitted source/package/tool identity
and referenced-input byte closure. A deployment or executable change could reuse
a stale completed checkpoint.

Required disposition:

- create canonical digitalmodel-run-identity-v1;
- bind clean source commit, package content, effective config, all referenced
  input bytes, selected executable hashes, result-policy version, and layout
  version;
- use the full identity digest for namespace/checkpoint reuse;
- require #1575 and #1576 to consume the shared schema rather than duplicate it.

R2 location: Normative Design Contract §2 and §4.

### R1-5 — Landing order and result extension ownership were undefined

#1565 froze exactly two returned files while #1576 required
artifact_index.json. All three issues also modify the same batch workflow.

Required disposition:

- land #1565, then #1575, then #1576 with exact predecessor SHA pins;
- keep cases.csv and batch_summary.json as the mandatory base;
- expose a code-owned, exact-name, schema-validated result extension registry;
- reserve openfoam-artifact-index-v1 for #1576;
- prohibit YAML/glob/suffix-only extension registration.

R2 location: Related issues and landing order; Normative Design Contract §5.

### R1-6 — The implementation plan violated the 400/50 limits

The r1 plan proposed adding behavior to a 680-line module containing an 89-line
router and a 445-line test without an explicit decomposition.

Required disposition:

- split configuration/identity, layout/locks, execution, and results into named
  modules before behavior changes;
- reduce the public facade to at most 200 lines and router to at most 50;
- split tests by responsibility;
- make a touched-path manifest plus AST structural tests fail on file/function
  excess or an omitted touched Python path.

R2 location: File Decomposition and Limits.

### R1-7 — Acceptance was not executable

The r1 plan used an unnamed canonical suite, a non-local legal scan, and no
machine-readable same-base failure oracle.

Required disposition:

- freeze literal focused, canonical full, paired base/candidate JSON-report,
  comparator, size, Ruff, compileall, diff, and cross-repository legal commands;
- pin base SHA 2ff0f72c9c5ce9022bfca763a6bb24ae4fb768d4;
- require candidate failure signatures to be a subset of base signatures;
- fail on touched-scope failures, collection drift, missing reports, or malformed
  evidence.

R2 location: Exact Verification Commands.

### R1-8 — Planning metadata overstated or referenced false artifacts

The r1 draft listed a local docs/plans/README.md artifact even though the branch
removed that false index, and its execution-mode text described transient past
agent-slot state rather than proposed work.

Required disposition:

- remove the false plan-index artifact;
- express proposed execution in future tense;
- record r1 as resolved-in-r2 draft only;
- require a fresh r2 review and user approval before implementation.

R2 location: plan header, Artifact Map, and Adversarial Review Summary.

## R1 disposition

All listed MAJOR themes are incorporated into the revised plan contract. This is
not a no-MAJOR verdict: the revised plan requires a fresh adversarial r2 review.
No provider or agent has approved implementation, host mutation, dispatch,
labels, issue comments, merge, or close.
