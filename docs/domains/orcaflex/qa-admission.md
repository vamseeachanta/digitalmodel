# OrcaFlex batch QA admission

## Summary

Native parallel batches require a separately supplied evidence record for every
case before output creation or worker submission. Admission validates the
evidence contract. It does not execute engineering checks or establish native
qualification. A worker's `status=ok` remains an execution result.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-19 | Define evidence admission and worker binding so incomplete or changed evidence cannot silently enter a batch. |
| 2 | 2026-10-19 | Migrate native test callers, retain validated evidence digests, and preserve adapter failure classification and test dependency coverage. |
| 3 | 2026-10-25 | Bind the selected relative master path to the approved fingerprint so another root model cannot execute under the same approval. |

## Basis and scope

The [module knowledge pack](../../../src/digitalmodel/solvers/orcaflex/docs/agent-pack.md),
section 5, maps AQG r0 section 12 to refusals B1–B9 (checks C15–C23).
That repository-resident table is the implementation basis. The underlying AQG
document was not inspected; this implementation does not reconstruct its full
schema or engineering criteria.
No licensed standard values or new engineering acceptance limits are introduced.

This change implements evidence admission and worker fingerprint binding.
Native evidence capture remains a later slice: complete load routes/messages,
dynamics duration and wall-time ratio, end images, independent statics criteria
and dispositions, and generated report appendices. Gate producers must retain
that evidence and an independent reviewer must approve the exact record before
it is eligible here. Merely assigning `PASS` does not perform those checks.

## Caller contract

`parallel_runner.run_cases(..., qa=context)` and `check_cases(..., qa=context)`
require an `AdmissionContext` for every nonempty batch. Legacy requests without
it are refused. An empty batch has no evidence records to admit. Existing
callers must supply the context; there is no compatibility bypass.
`check_cases` returns `(cases, bindings)`, where bindings retain the evidence
digests returned by validation; `run_cases` passes those identities to workers.

Native test callers requiring synthetic controls are
`tests/drilling_riser/global_model/test_parallel_runner.py`,
`tests/drilling_riser/global_model/test_campaign.py`, and
`tests/drilling_riser/global_model/test_w5_channels.py`.
The campaign adapter documentation requires the same context for application
callers. Synthetic controls test mechanics and do not qualify engineering QA.
The domain ownership table selects both consumer shards when shared QA controls
change.

The context contains the current `criteria_revision`, a nonempty
`required_images` role register, a `verify_signature` callable, and `requests`
indexed by case identifier. These are trusted policy inputs, resolved outside
the submitted evidence. No current revision or signature scheme is assumed.
Choose required image roles for the analysis being admitted; an incomplete
policy cannot establish complete image coverage.

Each `AdmissionRequest` contains:

- `record`: the candidate evidence object described below.
- `input_sha256`: an independently recomputed, closed-input fingerprint using
  `parallel_runner.input_digest(master, case)`. Every referenced dependency must
  be materialised below the model folder before hashing. The existing folder
  hash does not discover external references; unresolved input closure must
  remain unqualified.
- `configuration` and `sample`: the qualification identities being requested.
- `images`: required roles mapped to the actual local files for digest readback.

The request fingerprint is checked against the evidence at admission and
against newly built worker inputs before any native binding access. The selected
master path relative to its folder, the case JSON and all files below that folder
participate in this fingerprint. The digest starts with a compact JSON object
containing `master` and its relative path, followed by a newline, sorted file
digest lines and canonical case JSON. Absolute preparation and worker paths do
not participate. Selecting another master changes the fingerprint even when
both files remain present and have identical contents. Evidence produced with
the previous directory-only convention requires recomputation and approval.
Keep the evidence outside that folder to avoid a circular fingerprint.
The worker primitive `run_one` consumes admission identities from the parent;
it is not an independent signature-authenticating admission API.

## Evidence fields and refusals

| Refusal | Required evidence |
| --- | --- |
| B1 | A mapping with `case_id` matching the requested case. |
| B2 | `input_sha256` exactly matching the independently supplied fingerprint. |
| B3 | `gates` entries for G0, G1, G2, G3, G4 and G5. Each has `status=PASS`, or `status=NOT_APPLICABLE` with a nonblank `reason`. FAIL, NOT_RUN, NOT_EVALUATED, missing and unknown statuses refuse admission. |
| B4 | `program=OrcaFlex` and `program_version` equal to the requested DLL version. The worker retains its independent actual DLL-version check. |
| B5 | `images` maps every required role to its SHA-256 digest; each actual file is readable and has the same digest. |
| B6 | Nonblank `producer` and `signature.reviewer`, distinct from each other; the trusted verifier approves the exact payload and signature. |
| B7 | A `deviations` list, possibly empty; every listed deviation has a nonblank `disposition`. |
| B8 | `criteria_revision` equal to the independently supplied current revision. |
| B9 | Matching `configuration` and `sample`, and a `representatives` entry with matching identities, program version and criteria revision, `status=PASS`, a nonblank `evidence_ref` and an `evidence_sha256` digest. |

The signature verifier receives UTF-8 JSON bytes with sorted keys, compact
separators, nonfinite values prohibited, and only the top-level `signature`
field excluded. It must authenticate those exact bytes and the reviewer against
an approved trust store. Its exception, false return or nonboolean return
refuses admission. Neither an embedded approval flag nor a record checksum
authenticates a reviewer. The verifier and trust store are integration inputs;
no permissive default is supplied.

The approved payload includes gate rationales, image digests, deviation
dispositions and representative references. Referenced representative evidence
must be authenticated by the review process; this validator does not retrieve
that evidence or independently repeat the underlying calculation.

Validation returns one deterministic `AdmissionRefused` condition per invalid
request. Structural conditions are checked before exact-content authentication
so isolated malformed controls retain their specific refusal code. No worker
is created until every case passes. Input drift in a worker is a nonretryable
`admission_failed` result. Existing native state and saved-result readback checks
remain required. Ledger and extracted result records retain
`qa_evidence_sha256`, the approved payload identity; re-extraction preserves it.

## Test basis and limits

The refusal expectations come directly from the cited B1–B9 table. Synthetic
controls use a separate exact-payload approval set as a signature test double.
They are not native qualification records or cryptographic implementation tests.
Worker tests independently prepare the candidate bytes, then require matching
fingerprints or refusal before binding access. Existing scheduling, retry and
saved-state checks remain covered by solver-free tests.

No native solver was started, imported or queried. Licensed round-trip tests
require a separately authorised environment. Their synthetic approvals exercise
execution mechanics and do not provide engineering qualification.
