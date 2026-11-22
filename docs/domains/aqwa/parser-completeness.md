# AQWA listing parser completeness

Frequency extraction and displacement RAO extraction use structural section
boundaries or end of input. Blank lines and repeated displacement page titles
retain continuation rows. Velocity, acceleration, wave-direction tables and
the existing major-section markers end a displacement span. Heading extraction
uses the parsed displacement records across all displacement sections.
An optional independent frequency count also detects a frequency omitted from
every observed table; observation-only calls retain their existing behavior.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-28 | Remove remaining extent heuristics and reject inconsistent returned grids to prevent silent truncation; refs #2107. |
| Independent count | 2026-11-16 | Reject mismatches against a caller-supplied deck count while preserving observation-only calls; refs #2107. |

`parse_all` requires each coefficient table's frequency keys to equal the
extracted frequency keys. Displacement keys must equal the Cartesian product
of those frequencies and the observed displacement headings. A mismatch raises
`ValueError` naming the table/grid, missing keys and surplus keys before success
is reported. Membership is compared directly; no rows are padded. Existing
frequency units, numeric row formats, DOF mapping, phases and normalization are
preserved.

The independent hand calculation specified in #2107 uses
`omega_j = (100+j)/1000 rad/s`, `j = 0..255`, headings `[-90, 0, 90]` degrees,
`period = 2*pi/omega_j`, and six amplitude/phase pairs per row. For zero-based
DOF `k`, amplitude is `10*j+k+1` and phase is zero. Decimal frequency strings and
the expected Cartesian key set are constructed before parsing. These ordinal
probes have no physical-result meaning. They check every value at all 768 keys,
including the final yaw amplitude of 2556, beyond the former 50000-character
table cap and with later headings beyond the former 10000-character window.
Those former limits are defect reproduction thresholds from #2107, not limits
on accepted input.

## Verification

| Focused test file | Before: collected / passed / failed / skipped | After: collected / passed / failed / skipped |
| --- | --- | --- |
| `test_aqwa_lis_coefficient_grid.py` | 9 / 9 / 0 / 0 | 9 / 9 / 0 / 0 |
| `test_aqwa_lis_frequency_extraction.py` | 7 / 7 / 0 / 0 | 7 / 7 / 0 / 0 |
| `test_aqwa_rao_table_extent.py` | Initial probes: 17 / 0 / 17 / 0 | 22 / 22 / 0 / 0 |

Counts are from the focused parser tests. Optional shipped-listing skips:
none; both listings were available. The initial probes failed before parser
changes. Additional boundary and membership probes were then added. Missing RAO
rows, surplus RAO frequencies, missing/surplus coefficient frequencies and an
equal-size substituted coefficient grid all raise table/grid diagnostics;
complete controls pass. End-of-input, repeated page-title, multiple
displacement-section, explicit next-section and adjacent velocity/acceleration
variants pass.

For the initial structural-boundary revision, normal collection could not start
because the test configuration imports dependencies outside this parser. A temporary isolated
harness loads the parser directly, rejects licensed API imports and runs every
test in the three focused files without unrelated package initializers or test
configuration. This establishes parser behavior, not full-package integration.

### Independent declaration checks

`AQWALISParser.parse_all(*, expected_frequency_count=None)` accepts an optional
positive integer from an independent input deck declaration. Booleans,
non-integers, zero and negative counts raise `ValueError` identifying the
declaration before table extraction. A count mismatch raises `ValueError` with
expected and observed counts before a result is returned. Existing cross-table
membership checks still apply, and no partial result or padding is returned.

For a synthetic input deck declaring 256 frequencies:

```python
from pathlib import Path
from digitalmodel.hydrodynamics.diffraction.aqwa_lis_parser import AQWALISParser

parser = AQWALISParser(Path("ordinal.LIS"))
result = parser.parse_all(expected_frequency_count=256)
```

The caller must obtain 256 independently from the input deck. Never derive
`expected_frequency_count` from this listing or its parsed rows: doing so cannot
detect a shared omission. No automatic deck interpretation is added here.

Normal repository collection passed for all four focused parser test files,
with pytest caching disabled and temporary output outside the repository.
The three existing files collected 46 cases at the unchanged parser baseline:
44 passed and two skipped because a private listing was unavailable. Before
implementation, the new declaration file collected 15 cases: 14 failed and one
passed. After implementation, the combined suite collected 61 cases: 59 passed
and the same two private-listing cases skipped. The new controls return all
256 frequencies and both 256-entry coefficient tables. Consistent 255-row and
surplus listings fail against independent declarations. Invalid declarations,
the one-frequency boundary, explicit `None`, keyword-only use and the unchanged
no-argument return contract are covered. The existing continuation and absent
heading assertions remain unchanged.

## Open completeness contract

An absent entire heading group cannot be inferred from observed headings.
Likewise, a frequency absent from all returned tables cannot be inferred from
their agreement. The optional count detects a count mismatch only when a caller
supplies an independent declaration; omitted or `None` declarations check
internal consistency only. Equal-sized substituted frequency grids cannot be
detected by a count alone. Automatic deck-aware caller integration and declared
heading membership remain open under #2107; this change does not close that
issue or add a deck parser or authority registry. Duplicate-key semantics and
unsupported listing layouts are not qualified by these probes. No licensed
solver was imported or invoked, no solver accuracy checks were performed, and no hydrodynamic accuracy
qualification is claimed.
