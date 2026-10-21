# Response benchmark reference intake

Slice 1 of issue #2771 adds a read-only intake check for significant-motion
reference evidence.
`inspect_response_reference()` reports missing declarations, incompatible units
and byte-identity failures. Physical response accuracy remains `NOT_EVALUATED`
even when `intake_complete` is true. Existing envelope results are unchanged.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| Initial | 2026-10-19 | Add explicit reference intake gaps so metadata completeness cannot be reported as physical response qualification. |

## Scope and sources

The existing [envelope basis](envelope-basis.md), B1 and B5, defines significant
single amplitude, angular frequency in rad/s, translations in m and rotations
in deg. Intake accepts those exact units. RAO gain units, Hz, radians and other
conventions require a later explicit adapter; units are neither inferred nor
converted. No physical equation, coefficient or engineering acceptance limit
is introduced by this slice.

Byte identity uses SHA-256 from
[RFC 6234 (2011), section 6.2](https://www.rfc-editor.org/rfc/rfc6234#section-6.2).
The expected test digest is the published SHA256 test for the literal message
`abc` in section 8.5. It establishes hashing behavior only. It supplies no
physical response data. The source was retrieved successfully from the public
publisher on 2026-10-19.

## Intake contract

`ResponseReference` records the following caller-supplied evidence. Empty,
whitespace-only and non-text declarations are reported as missing. No default
acceptance tolerance or source claim is supplied.

| Field | Required evidence |
|---|---|
| `citation` | Source document, edition or year, and table/example location |
| `source_locator` | Durable locator for the cited source |
| `reuse_basis` | Evidence that the reference data may be used for this comparison |
| `independence_basis` | Evidence that results were obtained independently of the package implementation |
| `frequency_dependence_basis` | Location of the frequency-dependent response data and its coverage |
| `dof` | Exact motion name: surge, sway, heave, roll, pitch or yaw |
| `frequency_unit` | rad/s |
| `significant_amplitude_unit` | m for translations; deg for rotations |
| `acceptance_criteria` | Intended-use comparison limits, units and supporting source |
| `declared_sha256` | Lowercase hexadecimal digest of the exact reference bytes |

Supply the reference bytes through the `artifact` argument. The API downloads
nothing, executes nothing and writes nothing. It hashes bytes without decoding
or newline normalization, records `observed_sha256`, and compares it with the
declared digest. Missing or empty bytes, malformed digests and mismatches prevent
complete intake. A digest mismatch requires reconciling the source artifact
before proceeding to comparison.

`intake_complete` means that declarations, canonical unit labels and matching
non-empty bytes are present. Text is not authenticated; the artifact is not
parsed to establish that it contains the declared data. The synthetic `abc`
fixture deliberately demonstrates that complete metadata alone cannot establish
physical accuracy. The returned `benchmark_status` is always `NOT_EVALUATED`.
Callers shall use that status when reporting physical qualification.

## Evidence and review register

| Requirement | Current status | Evidence needed next |
|---|---|---|
| Independent frequency-dependent physical reference | NOT_EVALUATED | Authorized source data, units, coverage and matching digest |
| Numerical response comparison | NOT_EVALUATED | Reviewed common inputs, response convention and sourced acceptance criteria |
| Independent engineering review | NOT_EVALUATED | Durable review record against the reference, comparison and implementation revision |
| Deputy review responsibility | NOT_EVALUATED | Maintainer-recorded assignment and scope; no individual is assigned here |
| Shared lesson registration | NOT_EVALUATED | Maintainer-confirmed entry in the shared lessons register |

The local lesson is recorded here for later shared registration: constant-gain
RAOs can check spectrum normalization, and the analytical low-pass case can
check quadrature, while neither establishes independent physical accuracy.
Keep numerical test evidence and physical qualification status separate. This
local entry does not claim registration in a shared log or independent review.

## Validation and later slices

`tests/marine_ops/test_response_benchmark.py` checks each required declaration,
unit compatibility, exact byte identity and unchanged physical qualification
status. Its expected digest comes from the published RFC vector; other cases
assert the intake contract rather than invented response values. The fixture's
independence and frequency-dependence text is explicitly synthetic.

Later slices will obtain authorized physical data, parse the source with its
scenario and response conventions, compare package output under sourced limits,
and register independent review, deputy responsibility and the shared lesson.
A commercial reference solve requires separate execution authority. This slice
does not execute, import or query a licensed solver and does not close the issue.
