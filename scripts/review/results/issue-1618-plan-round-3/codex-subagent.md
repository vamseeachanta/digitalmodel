# Issue 1618 plan review — Codex subagent — round 3

Verdict: **MAJOR**

- `ReviewedHostResolutionEvidence` lacked an independent durable selection artifact and review authority, creating a circular input/output boundary.
- The selection predicate did not require equality to all upstream physical/loading/particulars/mass evidence commitments.
- Raw-hash exclusion wording could be read to exclude parent-required semantic evidence hashes.

Disposition: the next revision adds a separately reviewed immutable resolution-selection artifact, exact producer-evidence equalities and tamper tests, and precise transport-versus-semantic hash language.
