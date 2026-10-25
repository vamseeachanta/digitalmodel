# Open-source finite-element benchmark analysis

The 32×96 CQUAD4 mesh returns 91.4 MPa at point D against the published LE1
reference of 92.7 MPa: a −1.5% difference, within the 5.0% band. All 15 patch
stress components pass the 0.1% band. The coarse 8×24 mesh fails the LE1 band;
its result remains recorded without modifying the deck or criterion.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-10-20 | Measured four open-source solves and retained offline replay evidence. |
| 2 | 2026-10-25 | Rejected duplicate result IDs, documented the supplied deck limits and replaced the copied implementation with a numerical citation record. |

Table 1. Initial measured record.

## Results

| Mesh | Elements | Stress at D (MPa) | Reference (MPa) | Difference (%) | Band (%) | Verdict | Solve time (s) |
|---|---:|---:|---:|---:|---:|---|---:|
| LE1 8×24 | 192 | 82.1 | 92.7 | −11.4 | ±5.0 | FAIL | 0.264 |
| LE1 16×48 | 768 | 88.7 | 92.7 | −4.4 | ±5.0 | PASS | 0.840 |
| LE1 32×96 | 3,072 | 91.4 | 92.7 | −1.5 | ±5.0 | PASS | 3.141 |
| Patch | 5 | See Table 3 | See Table 3 | 0.0 maximum, rounded | ±0.1 | PASS, 15/15 | 0.091 |

Table 2. Measured global stress comparisons; timing is elapsed solve time at execution.

| Patch component | Expected stress (MPa) | Recovered stress range (MPa) | Verdict |
|---|---:|---:|---|
| sigma_xx | 300.000 | 299.9997–300.0003 | PASS, 5/5 |
| sigma_yy | 300.000 | 299.9997–300.0003 | PASS, 5/5 |
| tau_xy | 80.769 | 80.7692–80.7693 | PASS, 5/5 |

Table 3. Every patch element is compared with the analytic plane-stress solution.

Differences and acceptance are calculated from unrounded values. The patch
comparator reports all components within 0.1% after upward rounding. LE1 uses
the supplied linear extrapolation to x = 2000 mm from centroids of elements
1 and nt+1; the centroids lie near the symmetry line rather than on it.

## FE0, FE1 and versions

pyNastran 1.4.1 cross-references every deck with validation enabled. All four
have zero read errors, unresolved references, unsupported cards and rejected
cards. [FE0 console](console/fe0.txt) records the element and grid counts.

MYSTRAN 19.0.0, banner dated JUN 28 2026, completes all four native runs.
FE1 is read back from each run's ERR and F06 outputs: zero fatal/error messages,
zero warnings, and the native end marker present. ERR informational messages
include inferred shear modulus, SPLITD axes, sparse storage and internal memory
accounting notices; none is classified as a solver warning. The `--help` call
prints the banner but treats the option as a deck name and then exits on EOF;
the successful deck runs independently confirm the same version.

The comparison and extraction interpreter is Python 3.12.3. pyNastran and
pytest run with Python 3.11.15; pytest is 9.1.1. The uv version is recorded in
[versions](console/versions.txt). No licensed software was used.

## Recovery, provenance and checks

The native result frame is local SPLITD, the diagonal-bisector basis. The
arithmetic mean of the two fibre tensors is rotated into global XY before
comparison. [The extractor](tools/extract_stresses.py) records the inspected
open-source routine and revision. Tests include unequal fibre stresses,
nonzero shear, a rotated element, incomplete data and negative controls.

The received manifest's 19 file digests match. All four deck digests remain
unchanged. [manifest.json](manifest.json) records committed digests and the
publication transformations. Header lines containing identifying paths,
contacts or hosts are replaced by a script; result values are unchanged.
Native F06 files retain only the lexical centroid fibre and in-plane stress
fields. Original native-output digests are retained in [record.json](record.json).
Numerical, source and console evidence is bound by raw-byte hashes. Markdown
documentation is enumerated separately to permit formatting hooks; its content
is pinned independently in the review packet.

Two source excerpts were omitted because reuse rights were not asserted:
`sources/feenox-nafems-le1-excerpt.txt` and
`sources/featool-ex_planestress5-excerpt.txt`. No file exceeded 1 MB. The
retained numerical definition, generator, references and decks suffice for
replay. Historical preparation records are preserved with neutralized metadata.

Offline regression: 22 benchmark tests pass, including native replay and two
perturbed-CSV numerical failure controls. The 54 repository structure tests also
pass: 76 tests total. Comparator selftest passes. File-level checks pass for
whitespace, EOF, JSON/YAML, conflict markers, case collisions, symlinks, script
modes, private keys, file sizes, encoding, line endings, mypy and pydocstyle.
The four deck-generator outputs reproduce the received bytes; the expected-value
generator reproduces the committed JSON. Console evidence is retained in `console/`.

## Limits and way forward

The 8×24 result differs by −11.4%, outside the fixed ±5.0% criterion. The
successive errors reduce to −4.4% and −1.5% with refinement, consistent with
the supplied warning about coarse centroid recovery. The coarse mesh shall
not be accepted against that band. The 32×96 result passes the stated band;
formal acceptance of the fine-mesh choice and extrapolation method remains
with the maintainer. This record qualifies only the measured version, element
type, decks and recovery method; it does not establish other solver modes.

The original NAFEMS mesh and licensed publication were not opened. The source
basis is the supplied public definition and cited reference value; no licensed
publication is copied. Catalog metadata was unavailable in this checkout, so
this package does not establish catalog registration or wider input readiness.

## Baseline

None identified during benchmark execution.

## Hooks that could not run

The hook framework is not invoked by this task. Applicable file-level checks
are run directly; the publishing runner executes hooks when committing.
The commands `gitleaks`, `markdown-link-check`, `prettier`, `radon` and `vulture`
are absent. Their entries are confirmed on the base revision in
`.pre-commit-config.yaml`; no hook configuration is changed. The private-key
check and the task-specific public-text scan run directly.
Scratch creation at the preferred location returned `Read-only file system`;
the explicitly permitted alternative scratch location is used. No denied
command is retried through another access route.

## Follow-up

Second-provider review and the maintainer's acceptance of the fine mesh and
stress-recovery method remain required before merging.
