# Issue #1576 Plan Review — R1 Consolidated

> **Reviewed commit:** `b97aa4f42546c75781687dad67c0d420527accd5`
> **Plan:** `docs/plans/2026-07-13-issue-1576-openfoam-mpi-postprocessing-artifacts.md`
> **Review stance:** adversarial; approval withheld unless every correctness-critical claim is verified
> **Verdict:** MAJOR

## Blocking Findings

1. **Dependency and landing order were absent.** The plan modified the same oversized batch surfaces as #1565/#1575 without consuming their identity/layout and parsed execution-plan contracts. Required resolution: hard order `#1565 -> #1575 -> #1576`, exact merged SHA/interface gates, and no duplicate identity or case-path inference.
2. **MPI resource semantics were unsafe.** The draft retained `--oversubscribe` and did not bound workers to visible and dispatcher-authorized ranks. Required resolution: remove the flag, fail before mutation on rank overflow, and require an exact eight-rank canary.
3. **Completed locators could reference deleted processors.** The draft scanned before pruning while allowing `processor_field_tree`. Required resolution: prune before the final successful snapshot, omit processors from completed indexes, and keep failed diagnostics separate and generation-specific.
4. **Artifact identity was ambiguous and race-prone.** Artifact roots overlapped, delimiter-free canonical encoding was unspecified, and path/stat checks did not close replacement/addition/removal races. Required resolution: disjoint root selections, a versioned length-framed codec with golden vectors, descriptor-relative `O_NOFOLLOW` traversal, before/after identity checks, and exact path-set rewalk.
5. **Source/tool identity was not revalidated at commit.** Required resolution: consume clean pinned #1565 identity and recheck source commit/tree plus selected executable descriptors/content immediately before completion.
6. **Attempt publication was not crash-consistent.** A generic temporary-directory replacement did not define one attempt generation, same-device constraints, or the sole completion point. Required resolution: per-attempt same-filesystem staging, immutable generation commit manifest, fsync/rename, and one atomic current-generation pointer.
7. **Queue and remote retrieval claims exceeded repository authority.** Digitalmodel cannot promise full-set queue atomicity, host routing, retention, authorization, or locator retrieval under Deckhand's fixed execution/return boundary. Required resolution: create a public Deckhand dependency with exact 100-file/2,000,000-byte, full-set/error-state/host-lease semantics; keep #1576 host-local until it merges.
8. **Universal size limits were violated.** The draft added behavior to a 680-line source file and 445-line test file. Required resolution: characterization-first splits and mechanical 400-line/50-line gates before feature edits.
9. **Verification commands were incomplete.** Required resolution: literal reproduction evidence and exact focused/regression/lint/compile/legal/size commands.

## Required R2 Checks

R2 reviewers must verify the revised pushed plan against each finding above, Deckhand issue #564, current #1565/#1575 dependency state, the actual 680/445-line source/test sizes, and the existing MPI argv. Claims in the revised plan remain assertions until verified.

## Disposition

R1 blocks approval and implementation. The revised plan may describe findings as patched for r2, but it must remain `draft` until independent adversarial r2 review returns no MAJOR and the user separately approves it.
