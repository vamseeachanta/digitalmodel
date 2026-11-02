# Operability atlas text fingerprint method

The analytical operability atlas identifies its declared config and source text
after UTF-8 decoding and CRLF-to-LF replacement. Other text remains significant.
The accepted replacement cache preserves every grid value and node classification.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-28 | Define newline normalization and record cache acceptance for #2761. |

## Text basis

`content_fingerprint` in
[`operability_atlas.py`](../../../src/digitalmodel/drilling_riser/operability_atlas.py)
reads the config and every member of `SOURCE_FILES`, including its own module.
For each file the canonical bytes are exactly:

```python
raw.decode("utf-8").replace("\r\n", "\n").encode("utf-8")
```

Source digests use these bytes. The existing sorted-JSON basis retains the
canonical config text, parsed standard-edition metadata, source digests and atlas
identity fields. The config is not numerically reserialized. Spaces, blank lines,
lone CR, Unicode composition and numerical spelling remain significant. Missing,
unreadable or invalid UTF-8 declared text raises; an earlier fingerprint is never
substituted.

## Numerical acceptance

The generic engineering inputs, source membership, axes, formulae, screening
bands and tolerances are unchanged. The acceptance comparison uses the retained
`bcc370f46b9c` cache as its numerical reference, without a tolerance allowance:
all 330 rows, all axes and all node classifications are equal. Classifications
use the existing governing-utilisation boundary at unity.

The unchanged held-out interpolation gate in `build_atlas` passes: maximum
relative error is 8.8% against the existing 10.0% threshold. These percentages
are rounded for display; acceptance uses the unrounded values. The existing
serializer writes the accepted `7c23a1e69c2e` cache and updates `default.txt`.
Both earlier cache directories remain present.

The generated files under `atlases/drilling_riser_operability/` are:

| Path | Git filter attribute |
| --- | --- |
| `7c23a1e69c2e/grid.parquet` | unspecified |
| `7c23a1e69c2e/manifest.yaml` | unspecified |
| `7c23a1e69c2e/surrogate.json` | unspecified |
| `7c23a1e69c2e/validation.json` | unspecified |
| `default.txt` | unspecified |

No LFS-tracked file changes. The build input digest, serialized output digests
and completion receipt are retained outside the repository so a completed
candidate can be recognized without rebuilding it.

## Regression coverage and limits

The text tests copy only the declared basis into independent temporary roots,
retain deep config snapshots, and exercise every assignment of LF and CRLF to
the config and declared sources with ASCII and non-ASCII additions. An independent
canonical-byte oracle checks the fingerprint. Semantic input changes, source
tokens and standard editions invalidate identity; whitespace and equivalent
numeric spelling remain distinguishable. Missing and unreadable inputs refuse
verification. LF and CRLF rebuilds have exactly equal numerical arrays and
validation records, and agree exactly with the retained cache.

Before the change, the two existing atlas test files collected 12 tests:
11 passed, one failed, none skipped. The failure was the unchanged Parquet byte
identity assertion; the numerical arrays were equal. No node or interpolation
failure occurred, and no tolerance was adjusted.

After the accepted cache update, the same two files and the new text-fingerprint
file collect 73 tests: 73 pass, none fail, none skip. The original exact-node and
byte-identity assertions pass unchanged. The new file contributes 61 tests.

Source-checkout tests do not establish installed-package source discovery.
That lookup policy remains unresolved and fail-closed; #2761 remains open for it.
Any subsequent change to declared source text, including this fingerprint module,
requires a fresh identity and numerical acceptance before changing the default.

Lesson: provenance normalization requires both text-equivalence controls and an
independent numerical acceptance check before publishing a cache identity.
