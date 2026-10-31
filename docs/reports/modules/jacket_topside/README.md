# Jacket and topside module analysis

## Summary

[Read the report](index.html) for independently derived annulus properties,
single-brace joint ratios and zero-demand arithmetic. Nonzero member and joint
capacities, damage acceptance and standards compliance remain `NOT_EVALUATED`.
No design compliance is certified.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| A | 2026-10-28 | Initial module review and independent geometry calculation; capacity qualification gaps are retained to bound engineering use. |
| B | 2026-10-28 | Expose escaped calculation JSON and format wall thicknesses to three decimals while retaining finer input precision. |

## Rebuild

From the repository root, use a writable scratch directory outside the checkout:

```sh
PYTHONPATH=src python scripts/reports/build_jacket_topside_module_report.py --scratch "$TMPDIR/calculation" --output-dir docs/reports/modules/jacket_topside
```

The builder uses the Python standard library and public module functions. All
synthetic inputs are committed in the builder and reproduced with units and
source numbers in the report. Temporary calculation JSON remains in scratch.
The [manifest](manifest.json) records source and artifact SHA-256 digests, excluding
its own hash. [Change records](changes.json) use the report revision history.

## Evidence and limits

Independent expectations are the radial integrals in the existing
[worked geometry example](../../../../src/digitalmodel/structural/jacket_topside/docs/worked-tubular-section-geometry.md)
and direct divisions of synthetic joint dimensions. Zero-demand results check
arithmetic at finite positive returned capacity; numeric standard allowables
are withheld. Existing implementation tests are distinguished from independent
source qualification in the method inventory. Simplified bending, interaction
and joint assumptions require independently sourced nonzero cases before use.

Rebuild tests compare every deliverable byte and source/artifact digest across
independent directories. No external solver or standard-capacity qualification
is part of the rebuild. The broader issue remains open for unresolved methods.
