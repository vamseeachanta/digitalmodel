# Issue 2259 code review — round 2

- Date: 2026-09-29
- Reviewers: Codex architecture and ABS engineering lanes
- Verdict: MAJOR (both reviewers)

The reviews confirmed fractional coating conversion, explicit core plumbing, selected-
count checks, and legacy warning coverage. Remaining findings covered masked dynamic/
static weighting, unused annual deterioration, an unsupported universal two-anode layout
rule, missing report decision surfaces, presence-only CSV tests, implicit out-of-table
current-density inference, unresolved citations, and stale examples. The next revision
requires explicit project current densities, removes the universal layout rule, exercises
unequal dynamic/static densities and CSV parity, expands reports/limitations, and records
the long-flush resistance interpretation needed to reproduce the private benchmark.
