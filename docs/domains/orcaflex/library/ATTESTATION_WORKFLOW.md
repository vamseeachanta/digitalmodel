# Specification library attestation workflow

`python scripts/attestation_manifest.py` prepares offline input manifests and
reconciles stored results for [the library attestation issue](https://github.com/vamseeachanta/digitalmodel/issues/2428).
It never runs a native solver. A generation PASS means that the existing
modular generator accepted the specification and its generated bundle was read
back; it does not establish loadability, static convergence or equivalence.

## Inventory and generation

The roots are those named in the issue: `library/model_library`,
`library/templates`, `library/tier2_fast`, `installation`, `jumper`,
`subsea/jumper/installation`, `pipeline/installation`, `reference`, `regional`,
`mooring/reference`, `passing_ship` and `training`, relative to
`docs/domains/orcaflex`. Enumeration is sorted and deduplicated by relative path.
Campaign specifications elsewhere are outside this inventory. Counts are
calculated from the current checkout, rather than copied from a historical audit.

Supply a target solver version selected for the later native batch. This is a
requested version, not a claim that the version is installed or has been used.
The `ATTESTATION_SOLVER_VERSION` variable below must contain that selected version.
The output parent must already exist. Use a fresh directory and output filenames
for each invocation; existing output is never reused or overwritten.

```bash
PYTHONPATH=src uv run python scripts/attestation_manifest.py \
  --solver-version "${ATTESTATION_SOLVER_VERSION:?}" \
  --manifest "$TMPDIR/attestation-manifest.json" \
  --generate "$TMPDIR/attestation-generated" \
  --summary "$TMPDIR/attestation-summary.html" \
  --report-date 2026-10-20
```

Omit `--generate` to inventory without importing the generator. Each case contains
its `case_id` (relative specification path), source SHA-256, intended `master`
path, generated-file SHA-256 map, and generation/load/statics states. Master and
bundle paths are relative to the chosen generated root, so they can be transferred
without embedding filesystem locations. Identical content at different paths
remains separate cases. All files in a successful generated case are hashed,
including parameter files; nested YAML `includefile` and `BaseFile` references
must resolve inside that case's directory. Absolute paths, parent traversal and
linked files are rejected.

The existing `ModularModelGenerator.generate` method is used without diagnostic
validation overrides. Per-case schema, validation and file errors are retained
as `FAIL`, with the exception type and message (root locations are replaced with
role markers). Remaining independent cases continue. Alternate schemas detected
by the discriminators in `scripts/audit_all_specs.py::_is_different_schema` are
retained as `NOT_EVALUATED`, pending explicit adapters. Changed source digests
stop the batch. Dependency/import failures also stop the batch rather than being
misrepresented as individual model failures. Failed output is diagnostic only;
only cases with generation PASS may be supplied to a later native batch.

## Stored native-result contract

A later, separately authorized native runner will consume only generation-PASS
cases and use their `master` paths. No native launcher or automatic queueing is
included here. It must retain original evidence and return a JSON list of case
records with these fields:

| Field | Meaning and validation |
| --- | --- |
| `case_id` | Exact inventory identity; unknown and duplicate cases are rejected. |
| `source_sha256`, `files` | Exact copies of the manifest source digest and full generated-file digest map. |
| `solver_version` | Exact requested manifest version; mixed versions are rejected. |
| `api_version` | Nonempty version actually recorded by the native runner. |
| `started_at`, `finished_at` | Timezone-aware ISO timestamps, ordered by time. |
| `load`, `statics` | Objects described below; these remain separate outcomes. |
| `evidence` | Nonempty map from relative native evidence paths to SHA-256 values, checked against retained files. |

Table 1: stored-result fields and acceptance checks.

Each stage object contains `status`, `error`, `failing_object` and `warnings`.
`status` is `PASS`, `FAIL`, `NOT_ATTEMPTED` or `NOT_EVALUATED`; `warnings` is a
list of strings. Statics PASS additionally requires `converged: true` (a JSON
boolean, recorded by the native runner). A failure requires the exact error and failing object, or
`failing_object: null` with a nonempty `object_unavailable_reason`. Other statuses
require `error: null`. Statics cannot be attempted unless load passed: after a
load failure, statics remains `NOT_ATTEMPTED`. Missing case records stay
`NOT_EVALUATED`; they never become passes. Evidence hashes bind supplied bytes,
not authenticity, convergence interpretation or scientific correctness.

After the original generated bundle and native evidence have been retained,
reconcile without regenerating inputs:

```bash
PYTHONPATH=src uv run python scripts/attestation_manifest.py \
  --solver-version "${ATTESTATION_SOLVER_VERSION:?}" \
  --input-manifest "$TMPDIR/attestation-manifest.json" \
  --generated-root "$TMPDIR/attestation-generated" \
  --results "$TMPDIR/native-results.json" \
  --evidence-root "$TMPDIR/native-evidence" \
  --manifest "$TMPDIR/attestation-reconciled.json" \
  --summary "$TMPDIR/attestation-reconciled.html" \
  --report-date 2026-10-20
```

Readback checks cover current source bytes, complete generated bundle bytes and
native evidence digests. A digest mismatch stops ingestion. The manifest is read
back after saving. Reports escape supplied text, distinguish recorded outcomes
from claims and place revision history directly after the summary. Each report
is an initial snapshot; an issued revision must preserve its previous revision
history under the document issuance workflow.

## Worked example: identity and digest calculation

Inputs: two explicitly synthetic files, `library/templates/a/spec.yml` and
`library/model_library/b/spec.yml`, each containing exactly the three UTF-8 bytes
`abc`, without a newline. These are inventory fixtures, not valid model inputs.

Method: sort the relative paths and compute SHA-256 over the raw bytes. The
independent reference is the NIST **Secure Hash Algorithm, Message Digest Length
= 256**, *One Block Message Sample*, input `abc`, output H[0] through H[7]
([published example](https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/SHA256.pdf)).
The hashing algorithm is specified in FIPS 180-4 (2015), section 6.2. No
engineering coefficient or numerical acceptance tolerance is used by this tool.

Answer: enumeration gives two distinct cases, with `b` before `a` because
`model_library` sorts before `templates`. Both source digests equal
`ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad`.
Their intended masters are respectively `library/model_library/b/master.yml`
and `library/templates/a/master.yml`. Generation is `NOT_ATTEMPTED` and load and
statics are `NOT_EVALUATED` for both. This expected answer is asserted literally
in `tests/scripts/test_attestation_manifest.py`, independently of the tool's
hashing function. Other tests construct explicit finite file sets to establish
expected bundle counts and use synthetic stored records to exercise rejection
and joins. Synthetic records do not attest any repository model.

## Claim boundary and remaining work

`MODEL_CLAIM_REGISTRY.yaml` remains the sole claim authority. No registry is
written by this workflow. `claim_update` is always `NOT_EVALUATED` because
[the claim boundary](../SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md), sections 1.1 and
4.2–4.3, requires baseline result comparisons for L2 and difference classification
for L3; generic-track L3 is excluded. Native load/statics records alone cannot
establish those claims. No numerical benchmark tolerance is reproduced here.

The next execution checkpoint is a separately reviewed native batch with a pinned
installed solver/API version, exact failure/object reporting and retained native
output. Explicit alternate-schema adapters, reviewed baseline comparisons,
claim-gated registry edits and individual failure issue filing remain later
work. Input model redistribution requires the origin-class checks in
[the parent epic](https://github.com/vamseeachanta/digitalmodel/issues/2421).
Generated models and native evidence are execution artifacts and are not
published by this change.
