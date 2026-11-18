# Diffraction run export contract

`RunResult` and `AQWARunResult` retain their optional `xlsx_path` and
`report_path` fields. Their computed `export_dispositions` property distinguishes
an absent path from a recorded, unverified path. A completed solver status alone
does not establish spreadsheet or report production.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-11-16 | Export dispositions distinguish absent paths from unverified recorded paths. |

## Dispositions

Each access returns a new mapping with `xlsx` and `report` keys. Each value is an
immutable `ExportDisposition` containing `state`, `path` and `reason`.

| Recorded path | State | Meaning |
| --- | --- | --- |
| `None` | `NOT_PRODUCED` | No export path is recorded; automatic export production is not integrated. |
| Any supplied path | `PATH_RECORDED_UNVERIFIED` | The path is recorded; file existence and contents are unverified. |

The state is derived independently for each export from the current path field,
including after reassignment or reset to `None`. It is independent of pending,
dry-run, failed or completed solver status. Paths are never inferred, resolved,
read or created by this contract. Even an existing empty file remains unverified.
`NOT_PRODUCED` describes the recorded contract; it does not establish whether a
separate manual export exists elsewhere.

The shared CLI output prints each disposition and reason, plus its recorded path
when present. Validation output remains separate: stored `WARNING` is displayed
as `WARN` without changing the stored verdict.

The property is computed metadata, not an additional dataclass field; existing
constructors and dataclass serialization retain their original field shapes.
Callers needing the dispositions in a serialized payload must explicitly include
the property. No new serialized payload format is introduced here.

## Downstream consumers

- `convert_orcawave_xlsx_to_orcaflex(xlsx_path, output_dir, ...)` in
  [orcawave_to_orcaflex.py](../../../src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py)
  requires an explicit spreadsheet path. Spreadsheet export remains manual.
- `extract_report_data_from_owr(owr_path)` in
  [solver/report_extractors.py](../../../src/digitalmodel/hydrodynamics/diffraction/solver/report_extractors.py)
  requires an explicit results path and native API access when called. OrcaWave
  `owr_path` remains separate from its log path; AQWA gains no OWR field.

A recorded path alone is insufficient evidence for either downstream operation.
Existence, format, usable content and applicable engineering checks remain the
responsibility of the producer and consumer.

## Remaining acceptance

Issue #611 remains open.
Automatic spreadsheet/report production, verified populated export paths,
automatic downstream handoff and native execution acceptance remain outstanding.
Construction-only tests establish metadata semantics and CLI formatting; they do
not establish a successful native run or an export.
