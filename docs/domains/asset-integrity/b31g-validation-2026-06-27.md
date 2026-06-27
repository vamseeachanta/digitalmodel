# Corroded-Pipe Remaining Strength — Validation Record

**Date:** 2026-06-27
**Module:** `digitalmodel.asset_integrity.corroded_pipe`
**Standards:** ASME B31G-2012 (original & Modified B31G), RSTRENG effective-area
**Issues:** #1060 (implementation), #1061 (validation), epic #1057

## Methods implemented
`b31g_original`, `modified_b31g`, `rstreng_effective_area`, `allowable_flaw_length`
(pressure-based, Modified B31G), and `b31g_original_allowable_length` (the
canonical geometric B31G acceptance chart).

## Validation 1 — failure pressure (hand calculation)
Reference defect: 30 in OD × 0.375 in WT, d = 0.15 in (40 % WT), L = 8 in, X52
(SMYS = 52 000 psi).

| Method | Folias M | A/A0 | Failure pressure | Source |
|---|---|---|---|---|
| Modified B31G | 2.112 | 0.340 | **1219 psi** | hand calc |
| Original B31G | 2.356 | 0.267 | **1183 psi** | hand calc |

Original is more conservative, as expected. Limit cases verified: zero defect →
intact pressure; `L²/(Dt) > 20` → original infinite-length regime.

## Validation 2 — ASME B31G-2012 allowable-defect-length table (page 26)
Source table located at
`/mnt/ace/aceengineercode/data_manager/data/codes/asmeb31g/asme31gfile_pg_26.csv`
(rows = depth d, columns = wall thickness t, values = max allowable longitudinal
length, **OD = 20 in**, reverse-derived and confirmed). The table is reproduced
by `b31g_original_allowable_length` via `L = 1.12·B·√(Dt)`,
`B = min(4.0, √((d/t / (1.1·d/t − 0.15))² − 1))`:

| d (in) | t (in) | ASME table L (in) | Computed L (in) | Δ |
|---|---|---|---|---|
| 0.10 | 0.500 | 9.48 | 9.48 | 0.0 |
| 0.15 | 0.500 | 4.72 | 4.72 | 0.0 |
| 0.25 | 0.625 | 3.76 | 3.76 | 0.0 |
| 0.10 | 0.219 | 1.93 | 1.93 | 0.0 |
| 0.05 | 0.500 | 14.17 | 14.17 | 0.0 (B capped at 4) |
| 0.04 | 0.344 | 11.75 | 11.75 | 0.0 (B capped at 4) |
| 0.50 | 0.625 | 1.78 | 1.78 | 0.0 (d/t = 0.80 boundary) |

Defects deeper than 80 % of the wall return 0 (reject), matching the table's
zero entries. Golden tests: `test_b31g_allowable_length_matches_asme_table`.

## Note
The ASME table is an **output** of the public B31G formula, so the formula (not
the lost CSV) is the source of truth; the table merely confirms the
implementation. The CSV lives outside this repo (`aceengineercode`) and is not
required at runtime or in tests.
