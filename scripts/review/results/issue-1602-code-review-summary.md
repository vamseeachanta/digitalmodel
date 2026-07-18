# Issue #1602 Task 1 — adversarial code-review summary

Final implementation commit: `214fb59e` on `feature/1602-riser-hf-design`.

## Review trail

| Round | Claude | Codex | Agy | Disposition |
|---|---|---|---|---|
| 1 | UNAVAILABLE (wrapper repo mismatch) | In-session MAJOR | MAJOR | Composition, semantic and security bypasses fixed via RED fixtures. |
| 2 | MAJOR | In-session targeted PASS before external round | MAJOR | Owner, consumer, DAG, closeout, commitment and typed-error gaps fixed. |
| 3 | MAJOR | Targeted PASS; one raw-hash objection rejected | UNAVAILABLE (timeout/no artifact) | Canonical output, typed validation errors, string-only YAML keys and Windows-safe fsync added. |
| 4 | APPROVE | PASS | UNAVAILABLE (invalid non-review output) | Final T2 consensus; no unresolved finding. |

The final T3 gate degraded to T2 because Agy did not return a valid round-4 review. This is recorded as UNAVAILABLE, not approval. Agy was materially useful in rounds 1 and 2 and its findings were regression-tested.

## Final evidence

- Focused suite: 55 tests passed.
- Ruff, Black, mypy and Bandit: passed.
- Absolute-path enforcement: passed.
- Maximum file length: 400 lines; maximum function length: 37 lines.
- Deterministic canonical composed output: 43 top-level keys; SHA-256 `eb31e8c2eed4924daaa76a0d2aff0918ef0ddeabe78fa950e38d3de1ed847c9b`.
- Targeted legal scan of Task 1 paths: zero deny-list matches.
- Full-tree legal scan remains blocked by 40 pre-existing findings outside Task 1 (33 Perdido, 3 Caesar Tonga, 4 Yellowtail).

## Scope boundary

This review clears only the approved parent Task 1 validator/composer. It does not approve any child issue, solver adapter, HF publication, OrcaFlex execution, operating envelope, or monitoring implementation.
