# Plan: digitalmodel #276 — WRK-1149 Propeller-rudder method assessment (close-as-stale recommended)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/276
**Status:** plan-review
**Tier:** T1 (verification + close, no source changes)

## Context

Issue #276 (WRK-1149) is a method-assessment doc deliverable: rank candidate propeller-rudder interaction models (actuator-disk + Rankine-Froude, Holtrop-Mennen corrections, Söding/Brix slipstream-in-rudder, Molland & Turnock, hybrid momentum + empirical) on five criteria (accuracy, input requirements, RPM/J range, computational cost, implementation complexity), pick a primary, identify a fallback for low-J / engine-off cases, document validity gaps, and write the result to `docs/hydrodynamics/propeller-rudder-method-selection.md`.

The issue's `Status` field reads `done`. The deliverable file exists in HEAD: `docs/domains/hydrodynamics/propeller-rudder-method-selection.md` (verified via `find`). Note the path divergence — the issue specifies `docs/hydrodynamics/...` but the actual location is under `docs/domains/hydrodynamics/...`, which is the correct layout for the digitalmodel docs structure.

**Stale-flag:** Yes. The work appears complete; only the GitHub issue close-out remains. The acceptance bullets in the issue body are unchecked but that is a stage-tracking artifact, not a real-work signal — the file exists and `Status: done` was hand-set. Recommend close-as-done with a verification comment, **not** re-implementation.

## Plan

### Task 1 — Verify deliverable file content
Open `docs/domains/hydrodynamics/propeller-rudder-method-selection.md` and confirm it contains:
- A 5-criterion comparison table covering all five candidate methods.
- A primary method selection with written rationale.
- A fallback method called out for low-J / free-wheeling regimes.
- A validity-gap section.

### Task 2 — Cross-reference WRK-1148 + WRK-1280
Confirm the method-selection doc cites `propeller-rudder-literature.md` (WRK-1148) and `propeller-4-quadrant-data.md` (WRK-1280, see plan for #278). If either cross-ref is missing, propose adding it as a one-line edit in the close comment rather than leaving the doc isolated.

### Task 3 — Reconcile the path discrepancy in the issue
Either rename the deliverable to match the issue (move to `docs/hydrodynamics/`) or update the issue acceptance line to point at the correct `docs/domains/hydrodynamics/` location. The latter is far less disruptive and matches the rest of the docs tree — recommend that path.

### Task 4 — Close the issue
Post a close comment listing: (a) the verified file path, (b) the section headings demonstrating each acceptance bullet, (c) cross-reference status from Task 2. Close as completed.

## Acceptance Criteria

- [ ] `docs/domains/hydrodynamics/propeller-rudder-method-selection.md` contains the comparison table, primary selection, fallback, and gap section.
- [ ] Document cross-references to literature and 4-quadrant-data files are present (or added in Task 2).
- [ ] Issue body's acceptance line updated to match the actual file path (or file moved — recommend update-in-place).
- [ ] #276 closed with verification comment; no implementation work pushed under this plan.

## Open questions

- Should the deliverable be promoted from `docs/domains/hydrodynamics/` to a more discoverable location (e.g., `docs/standards/methodology/`) given that method-selection docs are decision artifacts useful across modules? Defer to owner — out of scope for this close-out.
