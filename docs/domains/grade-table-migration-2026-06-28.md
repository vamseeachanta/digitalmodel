# Material adapter migration analysis

The former canonical and legacy numerical tables have been removed. All material
adapters now query validated caller-supplied rows. Historical strength snapshots
are not a permitted numerical basis and are no longer reproduced in this report.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-10-18 | Added the #2726 supersession note so historical conversion values are not treated as current product-level minima. |

| 2 | 2026-10-26 | Removed standards-derived numerical comparisons and documented the caller-supplied replacement. |

## Current input contract

The [material input analysis](materials/caller-supplied-data.md) lists every
removed row identifier, its former source and the replacement schema. No
engineering row has been re-sourced. Each caller must provide qualified data and
an adjacent per-row `sources.yml` before running an engineering calculation.

The compatibility function names and their unit/key conventions remain. They
return read-only views of the active caller catalogue, with no embedded
snapshots, strength inferred from a grade name, or grade substitution. Yield
and tensile strengths come from the same supplied material record across callers.

## Verification and limits

Synthetic fixtures exercise the adapters and downstream callers. These tests
establish software behavior; they do not validate material requirements or
qualify a study. Missing catalogue data raises an explicit input error.
