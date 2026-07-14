# Issue 1578 Plan Review — Round 1

Reviewed exact draft: `47bbd4eba58d1371356a65914b0d696ef8a2f028`.

| Provider | Verdict | Major findings |
|---|---|---|
| Claude | MAJOR | #1577 provenance was incompatible; amplitude-dependent K/B were not self-consistent; 3-D interpolation, units, excitation phase, and singular tolerance were incomplete. |
| Codex | MAJOR | no coefficient-set envelope bound reductions to fill/amplitude; arbitrary forced-motion amplitude could be reused; topology and support distances were undefined. |
| Gemini | MAJOR | callers could bypass interpolation by supplying K/B and bounds; mixed-unit support distance and acceptance commands were not closed. |

Round 2 will require a hash-bound coefficient-set envelope, typed quantities,
closed rectilinear interpolation, per-axis normalized support evidence, complex
excitation reference, scale-aware singularity, and an exact self-consistent
amplitude root contract. This is review evidence, not approval.
