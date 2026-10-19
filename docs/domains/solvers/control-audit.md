# Solver control audit

## Summary

This read-only audit inventories solver source and test ownership for #2096.
The catalog records bounded static observations about approval, process
containment, recovery and native-fixture controls. It grants no execution or
publication permission. Native qualification remains `NOT_EVALUATED`.

The inspected OrcaWave batch invocation has no explicit timeout argument.
The regression reproduces that source-level absence without executing the
adapter. Remediation is routed to #2096; no solver failure is alleged.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| A | 2026-10-19 | Add a reproducible inventory and source-bound observations to separate implemented controls from unevaluated runtime behavior. |

## Scope and reproduction

Run from the repository root:

```sh
python scripts/audit_solver_controls.py
```

The command emits JSON to standard output and writes no files. A report may
be redirected to external scratch storage. It parses Python source under
`src/digitalmodel/solvers/` using the standard-library AST parser. It never
imports adapters, detects installed solvers, queries entitlement or starts
processes. The only dynamically loaded helper is the repository's existing
test-domain metadata parser.

The inventory recognizes directly imported subprocess APIs, selected OS and
solver APIs, import aliases, and Windows process-creation method names.
Lexical matching can overcount shadowed names and miss dynamic dispatch,
facade assignments, factories and wrappers. A group with no detected calls
is not evidence that it cannot execute a solver. Source outside this tree,
shell and PowerShell entrypoints, and downstream fixture-promotion systems
are outside the automatic inventory. The catalog separately cites the shared
credential scanner and evidence verifier.

At the audited source revision, the inventory contains:

| Source group | Python files | Lexical execution candidates |
| --- | ---: | ---: |
| benchmark | 7 | 6 |
| blender_automation | 12 | 5 |
| calculix | 4 | 1 |
| fea_model | 12 | 0 |
| ghs | 12 | 2 |
| gmsh_meshing | 9 | 0 |
| mystran | 5 | 1 |
| openfoam | 92 | 15 |
| orcaflex | 297 | 38 |
| orcawave | 4 | 3 |
| package-root | 2 | 0 |
| smoke | 8 | 0 |

The counts come from the command above, not runtime observations. Each
candidate includes a repository-relative file, line and recognized API name;
arguments, source excerpts and environment identifiers are excluded.

All 269 discovered test modules under `tests/solvers/` match an existing
ownership root in `tests/DOMAINS.md`. Matching reuses
`scripts/ci/detect_touched_domains.py`, including its directory boundary
semantics. This checks metadata only; it does not prove pytest collection,
CI execution, or test adequacy. New audit tests live under the already owned
`tests/scripts/` root. The previously reported GHS ownership gap is not
present at this revision. The audit test module is also added to the existing
unconditional CI harness, so script-only or catalog-only changes run these
checks without depending on touched-domain routing.

## Slices delivered

Slice 1 provides the deterministic source inventory and test-ownership audit.
Synthetic tests independently enumerate import-alias call sites and directory
prefix ownership, including an unowned sibling test directory. Candidate
source includes a deliberate import-time exception to demonstrate that the
audit parses it without execution.

Slice 2 adds the [control catalog](control-audit.json). Each observation names
the inspected family and function, pins the complete source-file SHA-256,
and routes follow-up to an existing issue. Changed bytes, missing or
ambiguous symbols, duplicate observation identifiers and unsupported status
claims are refused. Digest identity does not authenticate approval or prove
the observation's interpretation. Re-review the observation before updating
its digest; do not mechanically refresh evidence after a semantic change.
Pins use exact checkout bytes. Line-ending conversion can invalidate an
observation and is not silently normalized by the audit.

Slice 3 records native-format and public-fixture boundaries using the same
contract. Missing runtime, permission and format evidence remains explicit.
No fixture is copied, redacted or promoted by this audit.

## Control observations

`STATIC_ONLY` means source is present for the named observation.
`NOT_EVALUATED` means the requested guarantee has not been established.
`GAP` requires a cited test reproducer and an existing follow-up issue.
These labels are audit metadata, not launch eligibility. A control row's
aggregate label describes its cited observations only; it does not rate every
adapter. Exit status zero means the audit completed, including any reported
gaps; invalid scope or evidence returns a nonzero status.

| Control | Bounded finding | Route |
| --- | --- | --- |
| Staged input binding | GHS synthetic packet reconstruction and stage read-back are present. A live chain is unevaluated. | #2089 |
| Runtime image and argv | GHS sentinel image hashing and synthetic packet argv binding exist in separate contexts. | #2089 |
| Windows aliases | GHS rejects dot segments and trailing dot or space components before normalized path comparison. Other adapters are unevaluated. | #2096 |
| Atomic assignment | GHS sentinel supplies a job attribute during suspended creation and verifies membership before resume. Native qualification is unevaluated. | #2089 |
| Job-handle inheritance | GHS sentinel creation uses null job security attributes and disables inherited process handles. Native observation is unevaluated. | #2089 |
| Owned timeout | GHS has a sentinel timeout observer. OpenFOAM attached invocation supplies a timeout; detached enforcement requires polling. The inspected OrcaWave batch call lacks an explicit timeout. | #2089, #2096 |
| Independent cleanup | GHS cleanup attempts independent resources and accumulates errors. Native completion is unevaluated. | #2089 |
| Historical census | GHS checks observed child identities and job membership and journals creation stages. Present absence does not reconstruct missing history. | #2089 |
| Recovery consistency | GHS public success publication follows interlock finalization. The existing regression covers failed finalization. | #2089 |
| Positive trigger evidence | GHS sentinel checks child identity, census, liveness and trigger timing. A fresh native observation is unevaluated. | #2089 |
| License recovery | GHS live execution remains disabled. OrcaFlex explicit mock mode and statics checks do not establish license recovery. | #2089, #2096 |
| Observed native format | GHS normalized synthetic comparison is not a native-report parser. Wider parser profiles are unevaluated. | #2089 |
| Public fixture clearance | Credential screening and byte verification do not establish native-header clearance, publication rights or binary semantics. | #2096 |

The machine-readable catalog supplies exact symbol references and digests for
each row. The confirmed gap is limited to
`OrcaWaveOrchestrator._phase2_orcawave_execution`: its direct
`subprocess.run` call has neither a `timeout` keyword nor keyword unpacking.
The audit test parses that method and checks the actual call. It makes no
claim about other entrypoints, detached execution or unseen external controls.

## Sources and test basis

Policy sources are #2096, its linked #2089, the repository's
[GHS canary guide](../ghs/neutral-hydrostatics-canary.md),
[canary plan](../../plans/2026-09-11-issue-2089-ghs-canary.md),
[containment addendum](../../plans/2026-09-11-issue-2089-windows-containment.md)
and [recovery proposal](../../plans/2026-09-11-issue-2089-first-attempt-recovery.md).
The recovery proposal is historical context, not authorization or evidence
that recovery occurred. Current implementation claims use the pinned source,
not historical test counts or plan status.

No physical equations, coefficients, engineering constants or acceptance
limits are introduced. Source and line counts are measured inventory results.
SHA-256 byte comparison follows the existing
`scripts/verify_solver_evidence.py::verify` contract. Test expectations come
from direct hand enumeration of synthetic Python calls and ownership paths,
or independent inspection of the named OrcaWave method, as described in the
test docstrings. Tests also reject changed digests, stale symbols, symlinks,
malformed source, unsupported qualification claims and unsubstantiated gaps.

## Later slices

- Review each remaining candidate entrypoint before assigning a control finding.
- Address the bounded OrcaWave timeout gap under #2096 with a reviewed
  process-ownership, timeout, cleanup and recovery contract and offline tests.
- Qualify native containment and license recovery separately under #2089.
- Bind parser profiles to observed native formats and versioned source locators.
- Require permission and human header review before native-fixture publication;
  preserve original evidence privately and record any permitted redaction.

Trusted local operation remains the assumed context. This audit introduces
no remote authentication requirement or runtime policy change.
