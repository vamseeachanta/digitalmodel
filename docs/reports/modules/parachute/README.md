# Parachute module analysis

[index.html](index.html) inventories existing methods and presents synthetic drag
and elastic cantilever arithmetic. Physical and operational qualification are
`NOT_EVALUATED`. The report does not select equipment or an acceptance basis.

## Rebuild

With NumPy and PyYAML available, run from the repository root with source imports:

```sh
PYTHONPATH=src python scripts/reports/build_parachute_module_report.py --scratch <external-scratch> --output-dir <output-directory>
```

Scratch must be outside the checkout; every intermediate file stays there.
Both arguments are required. Output directories contain only the small report
artifacts. Fixed revision metadata and exact-byte digests exclude execution
timestamps, absolute paths and environment identities.

[results.json](results.json) retains computed public API outputs at 13 significant
decimal digits; input values retain their original precision. Raw API values
remain in external scratch as `calculation.json`. This encoding removes backend
roundoff for the bounded examples while remaining within the independent
numerical tolerances. Non-finite computed values are rejected.
[changes.json](changes.json) matches the report revision history.
[manifest.json](manifest.json) binds the report, results, history and this guide,
the builder and its source evidence using SHA-256. The manifest does not hash
itself. Digests prove byte identity, not physical accuracy or authenticity.

Independent numerical tests use the exact SI definitions in the
[existing worked example](../../../../src/digitalmodel/structural/parachute/docs/worked-synthetic-canopy-drag.md)
and an integrated elastic curve with force/moment balance. Replaying the builder
alone is a freshness check using numerical tolerances. Exact delivered bytes and
digests are checked separately, including a one-step float perturbation test for
these examples. Test assertions also reject sign/value mutations
even when the regenerated page, result data and digests agree.
