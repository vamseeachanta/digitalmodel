# ANSYS retained-evidence go-by

[Issue 2119](https://github.com/vamseeachanta/digitalmodel/issues/2119) historical Stage A baseline
retains eight diagnostic case records and 94 response fields from pressure-vessel,
padeye and mudmat examples. Six records represent native captures; two are older
repository response references with incomplete native provenance. **Zero responses
are engineering-qualified.** The captures do not establish a physical parametric
range or an engineering lookup table.

`intake.json` owns the frozen 17-candidate intake and inherited finding ledger.
`manifest.json` is the authoritative typed dataset; `responses.csv` is its derived
response export. The JSON record distinguishes decimal strings, null, absent
fields and true zero. Six invalid original padeye reaction/residual channels have
null usable values and preserve their observed values separately. `generated_at`
is the intake observation time, not a claim about native execution time.


## Current diagnostic result revision

Revision r6 retains 12 cases and 350 responses, with zero engineering-qualified
responses. The bounded canary has two recorded native attempts/completions: the
zero control and pressure N4. N8 and N16 remain unattempted. All 64 N4 matrix
values remain null/not_evaluated; numerical response counts remain 152 computed,
six failed and 192 not_evaluated.

The [retained N4 offline recovery report](https://github.com/vamseeachanta/digitalmodel-data/blob/main/reports/ansys-n4-recovery-2026-09-16.html)
records 54 checked exported quantities, nine derived von Mises values and the
support sum. Frozen N4 criteria give 21 passing checks and one failed check:
outer_y120 radial stress 0.01059709996930703 MPa exceeds the 0.010 MPa zero limit.
The coarse-mesh outcome is CONTINUE, not PASS. The r6 package-level diagnostic supplement records this assessment and its
qualification-blocking finding. Numerical adoption remains NOT_ADOPTED: all case
rows and numerical values are unchanged from r5. The original capture record
remains historical. Only 21 primary y120 checks plus axial equilibrium were
evaluated; auxiliary and refinement criteria were not evaluated. The two
auxiliary outer radial values also exceed the same numeric threshold, as
disclosed in the supplement, without widening the governing N4 criteria.
The private result owner is digitalmodel-data; this repository maintains the
existing computational lookup view. No additional native run is represented.
The r6 supplement fingerprints four transformation entry files using
`raw-sha256-v1`, distinct from the historical import fingerprint. It does not
claim transitive dependency closure or cross-checkout line-ending equivalence.
Replaying this bounded migration requires those exact source bytes; loading a
historical package verifies its recorded hashes without re-running the migration.

## Historical r4 diagnostic result revision

Revision r4 contains 12 cases and 350 responses: 88 historical computed responses,
64 newly computed zero-control diagnostic responses, six historical failures,
and 192 not-evaluated responses. Qualified responses remain zero. The bounded
cylinder canary has one native attempt/completion and three unattempted pressure
cases; this intake adds no native run. Revisions r1-r4 remain in the existing
retained-evidence owner. `manifest.json` is authoritative; `responses.csv` and the
[diagnostic report](../../../docs/reports/2026-09-14-ansys-cylinder-zero-native-checkpoint.html)
are views of it.

The new 64 responses comprise 54 extracted station components, nine derived von
Mises values and one summed reaction. All fixed zero-control checks pass; pressure
accuracy and mesh convergence are not established. Current receipt field names
label derived von Mises values with the other measured values; separate derived
provenance remains a tracked refinement, not native validation. All 23 original
files remain retained unchanged. Nine consumed numerical/diagnostic files are
within the 13 execution-bound artifacts; other retained files do not acquire
numerical authority merely by retention. Private raw-repository backup was unestablished at initial r4 publication;
see Original evidence storage below for the later verified backup. The pending-coverage description below records the r2 builder basis.

## Pending cylinder coverage

The `analysis_pending` builder adds four explicitly unattempted cylinder cases to
the eight historical cases: 12 cases and 350 response records, including 256 new
null values. This is coverage registration, with no new native result or
engineering qualification. The frozen 17-candidate intake remains the authority
for the eight historical cases; each cylinder case binds its prepared benchmark
manifest, input deck, metadata, criteria and analytical reference separately.

The backward-compatible `ansys-evidence-1` extension uses paired
`attempt_consumed` (boolean) and `native_attempt_count` (nonnegative integer)
fields. Both absent means not recorded; it never means zero. A `pending_native`
case requires false/zero, unverified capture author and source, unknown execution,
and null/not-evaluated responses with the `native-not-attempted` limitation.
`matrix_preparer` identifies descriptor preparation only. Shared loading and
construction validation enforce these rules. Native ingestion requires a separate
reviewed evidence contract; changing these fields does not qualify a result.

`analysis_pending_inputs.pending_inputs` reads retained benchmark descriptors
against caller-supplied frozen manifest and reference digests, without executing
a solver or the frozen checker. `build_pending_package` binds
all source bytes and preserves the historical case payloads. The caller supplies
the current code fingerprint, explicit revision and observation time.
`analysis_matrix_publish.publish_matrix` publishes into the existing owner root,
retaining r1 and refusing a different payload at an existing revision. The
canonical manifest is atomically replaced; the CSV refresh is separate. A crash
after manifest replacement can leave a stale CSV: regenerate it from the manifest,
or retry the same explicit revision to repair it without creating another one.

The owner-recorded `.integration.lock` serializes participating publishers and
contains host/PID/time/baseline identity. It is never removed automatically as
stale. Existing revision publication uses its nested `.publish.lock`. Both owner
termination and absence of an active publisher must be established before stale
lock removal. Nonparticipating writers and concurrent source mutation are outside
this cooperative protocol; source evidence must remain quiescent during capture.
The original frozen r1 importer below remains unchanged.

## Reproduce the offline import

Run from the repository with its Python environment and source package available:

```powershell
$env:PYTHONPATH = Join-Path (Get-Location) 'src'
python -m digitalmodel.ansys.analysis_intake `
  --intake examples/ansys/analysis-evidence/intake.json `
  --repo-root . --native-root $env:ANSYS_RETAINED_ROOT `
  --cache-root $env:ANSYS_SOURCE_CACHE --output-root $env:ANSYS_DATASET_ROOT
```

The owner supplies these local directories. `retained-native/` resolves beneath
the retained-results root; repository evidence is read as raw Git blobs at the
frozen source commit. Native files are required for reproduction and remain at
their authorized residence. References are not backups. No solver is launched.
The importer verifies captured bytes before parsing and verifies every retained
reference before publication. Missing or changed evidence stops import. The
destination is `ansys-retained-evidence/r1.json`; an existing revision is refused.
Limits are 5 MB per revision and 25 MB per dataset directory. A new package
revision is required after code, inputs, criteria or verification changes; this
frozen importer reproduces r1 only and does not overwrite published revisions.
The cooperative `.publish.lock` is removed on normal exit. After interruption,
the owner first confirms that no publisher is active, then removes only that
dataset's stale lock before retrying; active locks must not be cleared.

The CLI reloads its published JSON before reporting success. The committed
`manifest.json` tracks the current reviewed matrix revision, which can be later than r1. This self-contained check
compares its package hash and reproduces the derived CSV:

```python
from pathlib import Path
from os import environ
from digitalmodel.ansys.analysis_evidence import load_package, response_csv, read_response_csv
root = Path(environ["ANSYS_DATASET_ROOT"])
expected = load_package(Path("examples/ansys/analysis-evidence/manifest.json"))
package = load_package(root / "ansys-retained-evidence" / (expected["revision"] + ".json"))
assert package["package_hash"] == expected["package_hash"]
text = response_csv(package)
output = Path("examples/ansys/analysis-evidence/responses.csv")
assert output.read_text(encoding="utf-8") == text
assert read_response_csv(text)
```

The code fingerprint uses declared `utf8-lf-v1` canonical source bytes so LF and
CRLF checkouts reproduce the same identity. Native input/output digests continue
to cover raw bytes. Numeric digest tokens permit signs, scientific notation and
APDL field padding; embedded separators such as underscores are rejected.
`observed_value` preserves the exact numeric value as a canonical decimal string,
not the original token or significant-figure formatting. Trailing fractional zeros
are removed, scientific notation is expanded, and negative zero becomes `0`.
Original token formatting remains available in the digest-bound source CSV.

## Exact diagnostic query

```python
from pathlib import Path
from digitalmodel.ansys.analysis_evidence import load_package
from digitalmodel.ansys.analysis_lookup import lookup

package = load_package(Path("examples/ansys/analysis-evidence/manifest.json"))
case = package["cases"][0]
response = case["responses"][0]
query = {
    **{key: package[key] for key in ("dataset_id", "revision", "analysis_id")},
    **{key: case[key] for key in ("component_id", "model_revision", "parameters")},
    "response_name": response["name"],
    **{key: response[key] for key in ("definition", "location", "unit")},
    "intended_use": "screening",
}
answer = lookup(package, query, {}, diagnostic=True)
assert answer["qualified"] is False
```

Diagnostic retrieval reports the retained snapshot, including its limitations;
it does not verify current file availability or permit engineering use. The
calling owner controls access and current retention rights. Engineering queries
omit `diagnostic=True` and refuse these examples. Exact identity is required:
there is no interpolation, extrapolation, rounding or nearest-case selection.
The capture ID included in `model_revision` separates original and corrected
extractions; it does not assert different physical geometry.
Parameter keys use the same canonical decimal strings: for example,
`applied_pressure_x_gradient_mpa_per_mm` is `0.000025`, not intake literal
`2.5e-05`. Use the manifest key or `decimal_text` to construct it; lookup itself
does not normalize query strings or select a nearby case.

## Engineering promotion contract

The owner supplies a current authority record and logical-ID resolver outside
the dataset. Authority fields are `dataset_id`, `head`, `criteria_revision`,
`criteria_reference`, `review_sources`, `verification_reference`,
`blocking_findings` (explicit false), and `revisions` mapping revision to package
hash. Criteria, intake, review and verification references are `{id, sha256}` records
that match the package and resolve to current evidence bytes. Unreachable,
changed or unmapped evidence refuses engineering lookup.
The intake reference resolves to `canonical_bytes(intake)` in the source cache;
the committed intake is byte-identical at this revision. A reformatted intake
must be canonicalized before it can satisfy that reference. Bare `criteria_sha256`
and `intake_digest` fields are descriptive mirrors; the reference objects are
the authoritative bindings. Their equality was checked for this publication.

The verification JSON register keys entries by `case_id:response_name` and binds
the row hash, independent checker, competence, delegated authority, date,
intended uses, criterion ID, exact expected value, absolute tolerance, reference
evidence, required roles, allowed limitations and completed source-rights,
equilibrium, numerical-quality and independent-method checks. The checker must
have a different normalized recorded author identity; `author_status=recorded`
is required and unknown authorship refuses qualification.
The eight historical retained cases declare unknown individual authorship and
unverified execution completion; the later diagnostic case records its own
operational author and observed execution, without engineering qualification. Superseded captures cannot qualify. Each inherited finding
requires its own response-specific authority disposition, justification and live
evidence references under `finding_dispositions["case_id:response_name"][finding_id]`.
The disposition is `resolved` only after the owner accepts its supporting evidence.
All inherited findings conservatively affect every response until a new reviewed
revision justifies narrower applicability. These records are owner-controlled audit claims,
not authenticated identities or automated proof of engineering judgment.
Protocol tests simulate these claims only in temporary test state; they are not
published evidence and cannot qualify the retained cases.

`create_ledger` and `append_withdrawal` return ledger/head records. The caller
persists each new ledger state and its separately trusted current head under the
owner's serialized publication process. The adapter validates sequence, chained
digests and the current head; it does not provide a database or concurrent
authority-store service. Rollback, truncation and post-publication withdrawal
refuse engineering lookup. Compromise of both authority and dataset is outside
this local trust boundary.

The remaining engineering work is source/use-rights qualification, disposition
of inherited findings, response definitions and an independently checked
verification register. A new native campaign requires a reviewed case manifest, technical prerequisites
and a bounded solve budget under the standing analysis authorization. See the [technical report](../../../docs/reports/2026-09-13-ansys-evidence-stage-a.html).


## Original evidence storage

The private [digitalmodel-data evidence owner](https://github.com/vamseeachanta/digitalmodel-data)
retains the non-client cylinder originals under
[`data/ansys-cylinder-benchmark/manifest.json`](https://github.com/vamseeachanta/digitalmodel-data/blob/fb25f39f285127fe63cf54579df4e53260f16c7d/data/ansys-cylinder-benchmark/manifest.json).
The existing run and case IDs are preserved. Fresh remote verification on
2026-09-15 checked 133 payloads; the private owner records the verification receipt
in the [pinned publication receipt](https://github.com/vamseeachanta/digitalmodel-data/blob/68a2b9e955bec4b2753a60751454b34a3dc3a619/reports/ansys-cylinder-benchmark-r1-publication.json).

This `ansys-retained-evidence` matrix remains the sole computational lookup
authority. Private retention does not qualify diagnostic results. The completed
zero-load diagnostic and three pressure cases not attempted retain their existing
states. Future non-client evidence and authorized generalized/de-identified client-derived
solver inputs/results follow the private owner README and existing llm-wiki
catalogs. Identifiable client originals remain canonical in their private project
owners; licensed-standard originals remain at licensed locations. Generalization
requires recorded transformations, restricted provenance and physical-meaning
validation under the charter. This scope amendment adds no client-derived dataset
or qualified result.
