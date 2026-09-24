# ANSYS Stage A implementation review

Current source/data verdicts: **Claude MINOR; independent Codex APPROVE**.
Claude reviewed exact tree `1a9800e14afbd52643cc4e3d09c8871301b189cf`.
No blocking code finding remains. Final documentation corrections are listed
below; their separate exact-file verification receipt accompanies publication.
The commit containing this record and the issue publication comment identify
the published revision. Native solves and engineering qualification remain gated.

Scope: [issue 2119](https://github.com/vamseeachanta/digitalmodel/issues/2119),
approved offline evidence packaging and exact lookup. No native execution or
engineering promotion was authorized or performed.

## Plan and authority

The reviewed plan is `docs/plans/2026-09-13-issue-2119-ansys-analysis-lifecycle.html`,
SHA256 `3ad0710a146a464703a71c906c5f93a0e28e396d05e487cb6c00848fc2dd31e5`.
Plan-stage verdicts were independent Codex APPROVE and Claude MINOR. User approval
is recorded in [the approval comment](https://github.com/vamseeachanta/digitalmodel/issues/2119#issuecomment-5657519998).
No plan-approved label was self-applied. Stage B remains separately gated.

## Implementation review coverage

Historical coverage: Codex APPROVE and Claude MAJOR applied to tree
`43ee0ab92426671d0f1ed5bba8532e4cc43a20d2`. Both providers returned MAJOR on
tree `076b5ee49c94e545eea69a8b7903873591a4b1f7`: Claude identified missing
mandatory provenance fields and incomplete review bookkeeping; Codex identified
checkout-dependent source hashes. Those verdicts cover their exact historical
bytes only, not the subsequently changed adapter, tests, dataset or documentation.
Main-session corrections and final exact-revision verification are recorded below. Earlier pending checkpoints do not extend the historical verdicts to changed bytes.
One Codex worker attempt was unavailable because of model capacity; another
existing Codex worker performed the independent review. No provider pass was
inferred from that unavailable attempt.

The defect-hunting reviews defaulted to non-APPROVE. Supported defects were
reproduced with failing tests before correction:

- Package validation now rechecks membership and schema, beyond digest equality.
- Engineering lookup checks every evidence reference, including optional-tagged
  evidence used by responses.
- Decimal identity preserves exact numeric value under declared normalization; comparison uses a bounded exact
  arithmetic context, including nonzero-tolerance edge cases.
- Duplicate verification keys, null review-clear flags and string-valued
  intended-use lists refuse qualification.
- Import parses the same bytes whose digest was verified.
- Current criteria require a matching live digest reference, beyond a version label.

## Verification evidence

The selected offline suite completed with **175 passed, 2 skipped**. It comprises
the five new analysis test modules, existing results extractor/design-point/golden
tests and documentation routing. Native solvers were not invoked. The two skipped checks are licensed native re-solves, intentionally disabled by the existing ANSYS_NATIVE_TESTS gate; all selected offline golden-data checks passed.

A separate Codex source comparison used line splitting and Decimal conversion,
without importing the implementation parser. It compared 94/94 observations,
94/94 units, 92/92 parameter entries, eight family/input-hash mappings and all
46 required references (36 native, 10 repository). No mismatch was found. Six
invalid original padeye reaction/residual channels preserve observations with
null usable values. This comparison establishes import fidelity only.

The final-data independent comparison verified explicit unknown authorship and
execution status, six native-source classifications, two original-to-corrected
padeye supersession links and all 13 finding IDs on every response.
Case-data SHA256 is
`8b5218be92e225bda0f2f08549aee3682943d95675f49f44e26019f35e5375a2` at the earlier comparison checkpoint:
cases in manifest order, excluding row_hash and generated_at, sorted-key compact
UTF-8 JSON with ensure_ascii=False. The final metadata adds explicit author_status=unknown and source-encoding notes; numerical observations are unchanged. Final package hash is `ff1e1525d47463581fc95f50681b01b73d02f93947e32a6db788a6fabfce24e5`; intake SHA256 is `3e641f8e3e82db665f42d7f793eeeb769cc712935f6c47f0c713818a5bb0e0d8`. The production CLI reproduced and read back this exact package.

The result is eight diagnostic records, six native captures, 94 response fields
and **zero engineering-qualified responses**. Inherited reviews and intended-use
rights are unresolved; no independent engineering checker was assigned because
no response was proposed for promotion. Physical parametric coverage is not
established. Catalog discovery remains incomplete; sibling catalogs are unchanged.

## Publication checks

The first exact-staged legal scan passed; both HTML documents rendered without overflow or page errors and all local links resolved. The final source/data reciprocal review and scoped cleanup results are recorded below; the refreshed publication scan covers the appended receipts. Native originals and private resolver/cache records remain
outside the public repository at their authorized residence. This record will
be updated with the final evidence before publication.

## Claude R1 finding dispositions

- F1: criteria-ready review-change fixture corrected; full suite rerun.
- F2: intake requires explicit source/execution classifications; all retained
  execution statuses remain unknown. Identifier prefixes confer no trust.
- F3: capture roles and supersession pointers are retained; all 13 finding IDs
  conservatively affect every response. Each needs its own response-specific
  disposition, justification and live evidence. Superseded captures refuse use.
- F4: explicit per-candidate retention approval is required; actual author is
  unknown and blocks qualification. Existing rights rationale remains in intake.
- F5: size test checks actual JSON filenames; cumulative failure preserves the
  prior revision and leaves neither lock nor pending publication file.
- F6: two ledger entries now exercise reordering and truncation refusal.
- F7: criteria, intake and all 12 source-record digests are resolved and verified
  at import. Current criteria, intake and review bytes are checked for engineering lookup.
- F8: report and review record are present and linked; staged HTML links verified.
- F9: workflow adapter/intake descriptions now reflect delivered state.
- F10: COM1–9 and LPT1–9 are rejected with regression tests.
- F11: CLI reloads the published JSON; go-by includes typed CSV export/readback.
- F12: negative CSV branches and wrong authority revision hash are covered.
- F13: discarded float-parser invocation removed. Inspection established that
  the existing parser loses exact decimal identity; this adapter retains strings.
- F14: undeclared response units produce a boundary ValueError.
- F15: normal lock cleanup tolerates absence; interrupted-lock recovery is documented.
- F16: optional fixture relocation deferred. Existing tracked tests/ansys/__init__.py
  provides package importability; randomized-order full runs passed. No collection
  defect was reproduced. No new conftest scope is needed for this bounded adapter.

Claude review output was independently assessed by Codex; no review statement
was accepted solely because a provider emitted it. The public review JSON
preserves the exact first-round findings and file hashes.

## Second-round dispositions and portability correction

- Claude R2-1: finding ledger, criteria/intake/review references, supersession,
  capture role, author status and per-response finding lists are mandatory.
  Missing-field regressions refuse; a positive protocol test resolves an actual
  finding with response-specific evidence, then refuses missing evidence.
- Claude R2-2: historical and current review coverage are separated above.
  Test evidence is 175 passed/2 native skips for the source/test blobs submitted
  for the final verification; exact hashes are preserved in the provider result.
- Claude R2-3: no broader ignore-policy change is adopted. The pre-existing
  reports/ rule already excluded future reports; this patch only admits the
  named Stage A report. Broadly admitting unrelated reports would expand scope.
- Claude R2-4: declared physical input digests are compared with retained input
  or repository model evidence; a missing/mismatching input refuses import.
  The redundant model_basis_input_matches_capture field was removed.
- Claude R2-5: numeric grammar rejects underscores and malformed literals.
  Signs, scientific notation and APDL delimiter padding remain intentional
  accepted syntax, now documented; they are not malformed numeric identity.
- Claude R2-6: explicit author_status=recorded is required for qualification;
  checker/author identities are stripped and case-folded before comparison.
- Claude R2-7: CSV example is self-contained, reads the reproduced revision and
  compares its hash to the committed manifest; source PYTHONPATH is explicit.
- Claude R2-8: padeye headers are re-read from retained UTF-8 input bytes; the
  intake states that any mojibake belongs to the source quotation.
- Claude R2-9: optional uniform exception wrapping is deferred. The listed
  failures already refuse output; the adapter does not promise one exception
  type for filesystem, subprocess, decoding and malformed-object failures.
- Codex portability MAJOR: code fingerprints now use versioned UTF-8/LF
  canonicalization. LF/CRLF regression passed; raw native evidence stays hashed.
- Codex trailing-dot MINOR: portable identifiers now reject trailing dots.

Generalizable defect classes are promoted to
[issue 2120](https://github.com/vamseeachanta/digitalmodel/issues/2120) for a
separately planned ecosystem audit; no sibling implementation is authorized.

## Final current source/data verification

Claude final verdict MINOR at tree `1a9800e14afbd52643cc4e3d09c8871301b189cf`
is preserved in [the exact provider result](2026-09-13-issue-2119-claude-final.json).
Independent Codex APPROVE covers the exact source, tests, intake, manifest and
CSV hashes listed by that result. The final main-session test run is reproduced
in [the test receipt](2026-09-13-issue-2119-tests.json), including the exact
command, source tree and all nine selected test-file/four implementation hashes.
Result: 175 passed, two intentionally disabled licensed native re-solve cases.

Final independent source comparison: 94/94 values, 94/94 units, 92/92 parameters
and 46/46 evidence references match. All eight author_status values are unknown.
Final case-data SHA256 (serialization defined above) is
`52b77c24e30533bb04e551b15d577092a15eae04287afa75df9dbf4519c47454`.
Manifest SHA256: `e6a2b9e78d5ffab5ee70b3162934f4b75a35b97750a0a324bba71404042aa247`.
CSV SHA256: `39cf726b6bc177c9256a4cb12d2e3946d26fa25399ea6346825c4215ecf6adf6`.
Both Python go-by blocks executed successfully in the isolated environment; the
reproduced CSV was byte-identical to the committed example. Production CLI
package hash remains `ff1e1525d47463581fc95f50681b01b73d02f93947e32a6db788a6fabfce24e5`.

Claude final MINOR dispositions:

- R3-1: documentation declares canonical numeric observation, trailing-zero
  removal and negative-zero normalization; original tokens remain in source CSV.
- R3-2: exact CSV digest is recorded above; independent equality and executed
  go-by checks establish agreement for these bytes. Source-backed tests remain
  offline; no licensed solve was substituted for artifact verification.
- R3-3: exact-query canonical keys and the 2.5e-05 to 0.000025 example are documented.
- R3-4: intake resolver obligation and canonical cache-byte residence are documented.
- R3-5: exact final provider result, test command, tree and test digests are retained.
- R3-6: legacy bare hashes are documented as descriptive mirrors; reference
  objects remain authoritative. Equality was verified for this publication;
  a future schema simplification may remove the mirrors under its own review.

## Cleanup and publication boundary

CLEAN: no stashes, no task publication locks or pending files, no unexpected
changes in the isolated task worktree. Only named Stage A paths are staged.
EXPECTED: retained native originals, private source cache, versioned diagnostic
publication/readback copies, provider bundles, raw test/JUnit receipts and HTML
screenshots remain under the SOLVERS coordination/retained-results residence.
Existing sibling worktrees and previously archived cleanup directories remain
outside this pane's implementation scope. No all-machine cleanup claim is made.
UNEXPECTED: none found by the scoped audit. No native process or persistent
reviewer session was launched. Shared claim release follows publication.

Legal validation uses the existing hub scanner/global deny list in a retained
scoped snapshot because its live-root resolver has a previously recorded defect.
Every new staged text blob is copied and SHA-verified before scanning; no owner
deny list is present. HTML rendering checks passed without overflow or script
errors and local links resolve. This is text disclosure validation, not rights
qualification of native binaries or engineering acceptance.
