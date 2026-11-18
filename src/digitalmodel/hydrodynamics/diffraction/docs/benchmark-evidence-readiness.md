# Diffraction benchmark evidence readiness

Dataset `diffraction-matched-benchmark`, revision `r2`, remains **UNVERIFIED**.
The private evidence has not been inspected by this change. No digest agreement,
independent results review or intended-use eligibility is established here.
The [public disposition record](benchmark-evidence-readiness.json) records the
identity and outstanding check dispositions without private evidence values.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-19 | Open an acceptance record because the historical issue closure does not establish evidence readiness. |

## Basis and evidence responsibility

The requirements below come from digitalmodel issue #2539, Scope and Acceptance,
and the [diffraction knowledge pack](agent-pack.md), sections 3, 8 and 10, U5.
Issue #2140 is the historical reference; its closure is not an acceptance record
for the evidence requirements of #2539. No equations, numerical coefficients or
engineering acceptance limits are introduced by this record.

The evidence-owning repository is private `digitalmodel-data`, as referenced in
issue #2140. Its maintainers are the evidence custodians at repository level;
assignment of a responsible verifier remains to be confirmed privately. The pack
requires a diffraction quality manager on a different provider from the producer.
This record does not assert that such a review has occurred.

## Acceptance record

`NOT_EVALUATED` means the required private evidence was not inspected here. It is
neither a passing result nor an observed mismatch. All checks below remain open;
the referenced revision label alone does not establish the manifest revision.

| Check key | Disposition | Evidence required in the owning repository |
| --- | --- | --- |
| `source_identity` | NOT_EVALUATED | Resolve the authoritative source identity and existing dataset identifier without substituting a local copy or inventing an identity. |
| `manifest_identity` | NOT_EVALUATED | Resolve the authoritative manifest and its pinned repository revision, with its relationship to the source recorded. |
| `revision_identity` | NOT_EVALUATED | Confirm that the source, manifest and review all refer to dataset revision r2; a schema revision or earlier dataset revision is insufficient. |
| `sha256_digest_agreement` | NOT_EVALUATED | Compare the actual evidence bytes with the SHA-256 digests in that pinned manifest; record scope, agreement or mismatch privately. A digest string or file presence alone is insufficient. |
| `independent_review` | NOT_EVALUATED | Retain a reviewer disposition independent of the producer, bound to the same revision and evidence, including outstanding findings and their dispositions. |
| `intended_use_eligibility` | NOT_EVALUATED | Record the proposed use, governing criteria, units and conventions, applicability, convergence limitations and the evidence custodian's eligibility disposition. Program agreement alone is insufficient. |

## Readiness and publication boundary

Keep U5 and the public disposition **UNVERIFIED** while any required evidence is
missing, inaccessible, inconsistent or not evaluated. A source or digest mismatch
must be resolved in the owning repository before readiness can be established.
No benchmark reuse is authorized by this record or by a passing documentation
test. There is no automatic promotion from check statuses to readiness.

Retain the manifest locator, pinned private commit, actual digests, review evidence
and detailed intended-use limitations privately. Public updates identify only the
dataset, revision and readiness/check dispositions. Do not copy private geometry,
coefficient or response values, acceptance counts or raw result files into the
pack or this record. Any eventual readiness disposition remains bounded to the
reviewed intended use; it does not qualify other uses or later revisions.

## Checks and later slices

`tests/docs/test_diffraction_benchmark_readiness.py` checks the public field
allowlist, dataset/revision identity, the unresolved check dispositions, acceptance
table consistency and the link from U5. Expected identities and required checks
are taken from #2539 and U5. These tests do not execute a solver or validate the
private results.

The next slice is private evidence inspection and independent review against the
acceptance record above, followed by an evidence custodian's intended-use
disposition. Only then can a bounded public disposition update be considered,
with the documentation tests updated in the same change. No private evidence was
imported, and the missing checks remain NOT_EVALUATED.
