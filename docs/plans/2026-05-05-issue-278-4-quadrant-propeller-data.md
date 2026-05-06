# Plan: digitalmodel #278 — WRK-1280 4-quadrant propeller data (close-as-stale recommended)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/278
**Status:** plan-review
**Tier:** T1 (verification + close, no source changes)

## Context

Issue #278 (WRK-1280) extends WRK-1148's propeller-rudder literature survey with 4-quadrant propeller performance data covering crash-stop and full maneuvering envelope modelling. The issue's `Status` field reads `done` and **every acceptance bullet is checked**: ≥4 references gathered (11 actually delivered), Fourier coefficient representation of K_T(β)/K_Q(β) documented in van Lammeren format, Wageningen B-series (B4-70) 4-quadrant coefficients tabulated, applicability ranges documented, new file `propeller-4-quadrant-data.md` plus cross-ref in `propeller-rudder-literature.md`, and the FW-1 future-work item updated.

Verified live: `docs/domains/hydrodynamics/propeller-rudder-literature.md` exists in the tree (confirmed via `find`). The 4-quadrant cross-ref file is expected at `docs/domains/hydrodynamics/propeller-4-quadrant-data.md` per the acceptance list — the executor will verify in Task 1.

**Stale-flag:** Yes. This issue is functionally complete; the residue is only the WRK lifecycle close-out (the GitHub issue is still in OPEN state with `priority:medium` and `wrk-item` labels). Recommend close-as-done with a short verification comment, **not** re-implementation.

## Plan

### Task 1 — Verify completed deliverables in HEAD
Run `ls docs/domains/hydrodynamics/propeller-4-quadrant-data.md` and `grep -n "propeller-4-quadrant-data" docs/domains/hydrodynamics/propeller-rudder-literature.md` to confirm both the new file and the cross-reference exist. Open both files and confirm the issue's acceptance bullets (11 references, van Lammeren Fourier-coefficient block, B4-70 table, applicability/limitations section).

### Task 2 — Verify the FW-1 update
Locate WRK-1148's evidence/artifact (likely `docs/domains/hydrodynamics/` or a `specs/wrk/` subtree) and confirm FW-1 is annotated `wrk_ref: WRK-1280`. If the WRK-1148 evidence file is not findable, document the gap in the close comment rather than silently dropping it.

### Task 3 — Close the issue
Post a close comment on #278 listing: (a) the verified file paths, (b) line counts or section headings demonstrating each acceptance bullet, (c) the FW-1 cross-ref status from Task 2. Close as completed.

## Acceptance Criteria

- [ ] `docs/domains/hydrodynamics/propeller-4-quadrant-data.md` exists with sections covering Fourier coefficient representation, B-series 4-quadrant table, and applicability ranges.
- [ ] `propeller-rudder-literature.md` contains a working cross-reference link to the new file.
- [ ] WRK-1148's FW-1 entry is annotated `wrk_ref: WRK-1280` (or the gap is explicitly documented in the close comment).
- [ ] #278 closed with a verification comment; no source changes pushed.

## Open questions

- Where does WRK-1148's evidence/future-work ledger live in this repo? The plan assumes a `specs/wrk/WRK-1148/` or `docs/domains/hydrodynamics/wrk-1148-evidence.md` location — confirm in Task 2 before claiming the FW-1 cross-ref.
