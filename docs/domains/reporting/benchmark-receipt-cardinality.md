# Benchmark receipt cardinality inventory

## Summary

The inspected boundary is benchmark receipt comparison, receipt ingestion and
the existing HTML report's agreement readback. Comparison now requires a
nonempty baseline, unique case/variant identities and matching entry membership
before overall agreement. Conflicting identities within a report receipt are
rejected. Synthetic fixtures qualify these consumer behaviours only; they
represent no solver result. Other reports remain uninspected.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-28 | Recorded the bounded inspection and synthetic regressions to distinguish complete evidence from truncated or overwritten entries. |
| Correction | 2026-10-28 | Distinguished booleans from numbers in nested duplicate entries so receipt eligibility cannot depend on entry order. |

The date records this inspection, not the date of any numerical result.

## Inspected consumers and findings

| File and symbol evidence | Existing behaviour | Current qualification |
| --- | --- | --- |
| [compare.py](../../../src/digitalmodel/solvers/benchmark/compare.py), `_matches` | Fingerprint key sets and list lengths were checked before positional `zip` comparison. List truncation was already guarded. Scalar and singleton-list shapes were treated alike; equal infinities could match. | Finite numerical scalars or nonempty flat lists are required. Scalar/list shape, key sets, list lengths and supplied list order must match for `MATCH`. Booleans, nested lists and nonfinite values do not qualify. |
| [compare.py](../../../src/digitalmodel/solvers/benchmark/compare.py), `compare` | Indexing the new receipt by identity silently overwrote duplicates. Iteration visited baseline entries only, ignoring extra new entries. An empty baseline reached vacuous `all(rows)` success. | Empty baselines and duplicate identities on either side raise `ValueError`, including equal duplicates. Surplus new identities raise a membership error. Missing new identities retain `MISSING` rows and make overall `ok` false. Membership is relative to the supplied baseline. |
| [compare.py](../../../src/digitalmodel/solvers/benchmark/compare.py), `fingerprint_status` | Completion, input digest, tolerance and version rules determined row status; a version change could excuse a numerical mismatch containing nonfinite values. | Nonfinite fingerprints on either side produce `FAILED` before the version rule. Existing timing semantics, baseline-eligibility flags, tolerance selection and finite `VERSION_CHANGED` classification are retained. That classification is reported separately from numerical agreement. |
| [compare.py](../../../src/digitalmodel/solvers/benchmark/compare.py), `differing_keys` | The union of fingerprint keys delegated each value comparison to `_matches`. | Missing keys, shortened or reordered lists, shape differences and nonfinite values are identified through the same matching predicate. No input is rewritten. |
| [report.py](../../../src/digitalmodel/solvers/benchmark/report.py), `load_receipts`, `_entries_equal`, `_check`, `build_report` | Receipt shape was checked, but contradictory entries with the same identity could enter candidate selection. | Both file ingestion and direct dictionary ingestion reject conflicting duplicate identities within one receipt. Equality covers the whole entry, including fingerprint and timing metadata, and distinguishes booleans from numbers at every dictionary/list depth. Dictionary key order is immaterial; list lengths and order must agree. Exact duplicate entries remain permitted. Validation uses the same string case identity as report grouping. |
| [report.py](../../../src/digitalmodel/solvers/benchmark/report.py), `_pack`, `_row`, `_agreement` | The newest receipt per label and case/variant was selected by `started_utc`, with supplied order breaking ties. Only baseline-eligible, comparable rows entered agreement. | Selection across distinct receipts is unchanged. A newer conflicting fingerprint is valid receipt history; it is distinct from contradictory entries in a single receipt. Finite eligibility follows `fingerprint_status`. |
| [report.py](../../../src/digitalmodel/solvers/benchmark/report.py), `render_html`, `_summary` | Agreement counts and per-case statements were derived from the report model. | HTML text readback confirms that missing, extra and empty result sets leave combinations marked as one-label coverage, rather than complete agreement. Valid overlapping combinations can still agree; the report does not claim a complete campaign. |

## Synthetic evidence and independent anchors

[test_receipt_cardinality.py](../../../tests/solvers/benchmark/test_receipt_cardinality.py)
defines the expected identity set independently as
`{(case_a, 1), (case_b, 1)}` before consumers receive deep copies. Variant `1`
denotes the single synthetic variant. Ordered samples `[2, 3]` have the
independently hand-calculated sum `5` because `2 + 3 = 5`; no production
comparison function generates the expected values. Timing, repeat count,
version strings and timestamps are explicitly synthetic bookkeeping.

The tests remove and add entries, repeat identities with equal and conflicting
fingerprints, empty receipts, shorten and reorder samples, distinguish scalar
and list shapes, and inject positive infinity, negative infinity and NaN into
scalar and list values. Nonfinite checks include either side and both sides,
with different version tokens. Positive controls retain the original fixtures
and the exact dictionaries passed to comparison and HTML rendering. Receipt
history tests supply distinct receipts in both orders. HTML readback parses
rendered text through the standard library HTML parser without a browser.

## Check record

Counts below come from pytest collection and result summaries in this
inspection. The baseline source revision is `09089a121a16`.

| Test file | Before: collected / passed / failed / skipped | After: collected / passed / failed / skipped |
| --- | --- | --- |
| `tests/solvers/benchmark/test_runner.py` | 54 / 54 / 0 / 0 | 54 / 54 / 0 / 0 |
| `tests/solvers/benchmark/test_report.py` | 21 / 21 / 0 / 0 | 21 / 21 / 0 / 0 |
| `tests/solvers/benchmark/test_receipt_cardinality.py` | 42 / 12 / 30 / 0 before production corrections | 42 / 42 / 0 / 0 |

The initial pytest-only invocations stopped at collection because `loguru` was
absent. Supplying import dependencies through uv allowed the baseline to run.
The new file was added and run before changing production validation. The
combined final run collected and passed `117` tests, with no failures or skips.
The two existing test files were not edited.

Reproduction uses `PYTHONPATH=src uv run --no-project --with pytest --with
loguru --with numpy --with pydantic --with scipy python -m pytest`, followed by
the test paths and `-q -rA -p no:cacheprovider`, with `--basetemp` directed to
external scratch storage. Initial baseline commands named each existing file
separately; the final command named all three files. All checks ran on the
uncommitted tree.

## Limits and work not evaluated

- Native solver accuracy and convergence: **NOT_EVALUATED**. No solver or
  benchmark campaign was invoked; the existing runner tests use fake outcomes.
- Physical coordinate provenance, units and independently authenticated sample
  coordinates: **NOT_EVALUATED**. Ordered fingerprint lists are compared as
  supplied; receipt schemas do not establish those physical facts.
- Completeness against an external case manifest: **NOT_EVALUATED**. Comparison
  can establish membership against the supplied baseline, not detect a case
  omitted identically from both receipts. The report groups the supplied union
  of cases and variants and has no separate complete-campaign verdict.
- Other reporting packages, solver-output parsers and shared gates:
  **NOT_EVALUATED** and uninspected. No package-wide qualification is claimed.
- An exact repeated entry remains permitted by report ingestion, preserving its
  existing selection and counting behaviour. Comparison rejects any repeated
  identity because each baseline and new entry must be unambiguous.

## Baseline

No baseline test failures remained after supplying the import dependencies.

## Hooks that could not run

Hook framework execution is reserved for the publishing runner. Direct
file-level checks are recorded in the change description.

## Duplicate type correction

Ordinary dictionary equality treats boolean `true` as numerical `1` and
boolean `false` as numerical `0`. Those pairs are now distinct throughout
nested duplicate entries, including scalar fingerprints, sample lists and
metadata. This prevents candidate selection from accepting an ambiguous receipt
whose eligibility changes when the duplicate entries are reordered. Equality
between equivalent integer and floating-point values is retained.

At correction baseline `f266cffa`, the three targeted test files collected and
passed `117` tests, with no failures or skips. Before the correction, all `36`
new boolean/numerical conflict regressions failed because no exception was
raised. Both entry orders and both ingestion APIs were exercised. After the
correction, the same three files collected and passed `164` tests, with no
failures or skips: runner `54`, report `21`, receipt cardinality `89`.
The run used the reproduction command above with `PYTHONUTF8` unset.

Positive controls retain equal duplicates through both APIs, including nested
booleans, equivalent integer/floating-point values and reordered dictionary keys,
and confirm agreement with an
independent reference receipt. Additional negative controls reject changed
dictionary keys and list lengths. Text writes in the corrected source and test
file specify UTF-8 and LF. Native solver accuracy, convergence and external
completeness remain **NOT_EVALUATED**.

## Follow-up

Native accuracy, convergence and external completeness evidence require separate
qualification before using these synthetic tests to support engineering claims.
