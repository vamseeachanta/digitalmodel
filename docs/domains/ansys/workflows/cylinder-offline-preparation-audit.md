# Retained cylinder preparation audit

## Summary

The read-only `audit_inputs` function checks a retained offline preparation
against the frozen four-case proposal and the current deterministic generator.
It returns `VERIFIED_INPUTS_ONLY` after every required input and receipt matches.
Reference agreement and execution readiness remain `NOT_EVALUATED`.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-19 | Added retained-input auditing to detect changes after preparation. |

## Scope and basis

Refs #2121. The audit extends the existing
[`prepare_inputs`](../../../../src/digitalmodel/ansys/cylinder_preparation.py)
writer without altering its format or any historical evidence. The required
proposal is checked by its existing frozen-contract validator.

The source is the repository's
[four-case proposal](../../../plans/evidence/2026-09-13-issue-2121-canary-proposal.json)
(2026, `basis`, `cases`, `criteria` and `budget`). Its demonstration inputs and
fixed candidate limits remain unchanged; this audit introduces no engineering
equation, coefficient or acceptance tolerance.

For each member, acceptance requires byte equality with its regenerated input.
The returned SHA-256 digest is computed from those same read bytes. Artifact
names, order, canonical receipt and provenance follow `prepare_inputs` and
`_artifacts` in the linked writer. The inventory consists of proposal and
basis/criteria files, a deck and metadata pair per declared case, and the
preparation receipt. Extra members, including a partial-preparation failure
receipt, are refused.

## Use

```python
import json
from pathlib import Path

from digitalmodel.ansys.cylinder_preparation_audit import audit_inputs

proposal = json.loads(Path(
    "docs/plans/evidence/2026-09-13-issue-2121-canary-proposal.json"
).read_bytes())
result = audit_inputs(Path("retained-inputs"), proposal)
```

Keep the retained directory quiescent during the audit. Missing, altered or
unavailable evidence raises `ValueError`; no file is created, repaired or
removed. Receipt-supplied paths are never opened. Symlinked directories and
members, reparse points, shared hardlinks and nonregular members are refused.
Reads are bounded by the regenerated member length. A changed deck cannot pass
by updating its receipt digest. Receipt equality also refuses duplicate JSON
keys, byte-order marks, changed ordering and coerced readiness booleans.

## Evidence and limits

The tests independently enumerate the proposal's case identities and hand-count
the expected inventory, then exercise missing members, extra members, malformed
receipts, rehashed mutations, changed loads, prohibited radial restraints,
changed stations and path redirection. A successful audit preserves file bytes.

The audit reuses the production generator, so it does not independently establish
the physical correctness of its decks or analytical solution. It does not check
native results, establish source rights, authenticate engineering review or
grant execution approval. Its file reads do not form an atomic snapshot and
cannot establish a durable guarantee against concurrent mutation. The audit is
for a preparation directory, not a native-result or final execution bundle.

Independent numerical reference checks, competent review and the separate
execution gate remain later work. Existing approvals bound to previous runtime
inventories are not renewed by this audit.
