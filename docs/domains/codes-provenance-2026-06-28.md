# Code-Reference Provenance — Validation Record (2026-06-28)

Backing for issue #1092 (EPIC #1080 workstream C). Every strength / capacity /
FFS result now carries a `code_reference` string stating its governing code and
edition in the returned object — not only in the docstring.

## Single source of truth

`src/digitalmodel/codes.py` defines `CodeReference(standard, edition, title)`
with a uniform `label` (e.g. `"ASME B31G (2012)"`) and named constants. The
`REGISTER` dict enumerates them for the codes register (#1093), so the
documentation cannot drift from what the code stamps on results.

## Result class → governing code

| Result dataclass | Module | `code_reference` |
|---|---|---|
| `CorrodedPipeResult` | `asset_integrity/corroded_pipe.py` | ASME B31G (2012) |
| `DNVF101Result` | `asset_integrity/dnv_rp_f101.py` | DNV-RP-F101 (2021) |
| `BucklingResult` (plate) | `structural_analysis/buckling.py` | DNV-RP-C201 (2010) |
| `BucklingResult` (column) | `structural_analysis/buckling.py` | EN 1993-1-1 (2005) |
| `PanelBucklingResult` | `structural_analysis/panel_buckling.py` | DNV-RP-C201 (2010) |
| `CapacityResult` | `structural_analysis/capacity.py` | EN 1993-1-1 (2005) |
| `MetalLossFFSResult` | `structural_analysis/plate_metal_loss_ffs.py` | API 579-1/ASME FFS-1 (2021); DNV-RP-C201 (2010) (capacity basis) |
| `FFSAssessmentResult` | `assessment/ffs_coordinator.py` | API 579-1/ASME FFS-1 (2021) |
| `WallThicknessResult` | `analysis/wall_thickness.py` | governing `DesignCode` (e.g. DNV-ST-F101 (2021)) |

`BucklingResult` is set per construction site because the module hosts two codes
(plate buckling per DNV-RP-C201, column buckling per Eurocode 3). The others use
a single governing code (field default or a single `_finalise` choke point).
`WallThicknessResult` maps its selected `DesignCode` enum to the reference.

## Surfaced in lookup / dashboards

The FFS lookup record (`ffs_results.json`, consumed by the field dashboard /
Deckhand API) now carries `code_reference`:
- `ffs_lookup._record(...)` adds it as a first-class field.
- `evaluate_pipe_corroded`, `evaluate_plate_metal_loss`, and
  `record_from_assessment` thread the result's `code_reference` through.
- `FFSAssessmentResult.to_dict()` includes it.

(The published dashboard HTML regenerates from this data; rendering the field
visually is a publish-step follow-up.)

## Tests

`tests/asset_integrity/test_code_provenance.py` — 11 tests asserting each result
type's `code_reference` (presence + exact value vs the `codes` constants), the
two BucklingResult codes, the metal-loss combined reference, the
`FFSAssessmentResult.to_dict()` key, and the lookup-record surfacing. All
passing; the broader asset-integrity + structural + materials suites stay green
(2365 passed; the 16 `test_structural_analysis_cli` failures are a pre-existing
environment artifact — they subprocess an installed `structural-analysis`
console script absent under a PYTHONPATH-only run).

## Relates to

Per-strategy / module-level docstring citations and the consolidated
`docs/domains/codes-register.md` are tracked by the sibling issue #1093, which
reuses the `codes.REGISTER` defined here.
