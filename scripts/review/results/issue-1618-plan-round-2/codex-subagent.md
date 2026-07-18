# Issue 1618 plan review — Codex subagent — round 2

Verdict: **MAJOR**

- Whole-catalog inputs left the join predicate and selected physical vessel/hull/loading underdetermined.
- The local vessel frame was not bound to the parent-owned global ENU frame.
- Canonical bytes lacked a complete cross-language scalar/Unicode/escaping algorithm and golden vector.
- Child YAML, runtime model, CLI, and preimage declarations could drift.
- Output-file symlink, alias, overwrite, concurrency, fsync/rename, and residue cases were missing.

Disposition: the next revision replaces discovery with reviewed resolution evidence and exact hash lookups, adds local-to-ENU transform fixtures, freezes typed normalization plus RFC 8785 and a golden vector, requires bidirectional artifact equivalence, and expands output-security tests.
