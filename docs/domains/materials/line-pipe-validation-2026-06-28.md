# API 5L Line-Pipe Catalog — Validation Record (2026-06-28)

Backing for `src/digitalmodel/materials/line_pipe.py` (issue #1087, EPIC #1080
workstream B). Adds API 5L line pipe as a usable **product** catalog —
NPS × schedule × grade → a `LinePipe` carrying geometry, the `MaterialGrade`
(#1088), and the API 5L reference — not just a grade table.

## Scope

- **Dimensions:** ASME B36.10M. Outside diameters for NPS 1/8 .. 80 in; wall
  thicknesses for schedules 10/20/30/40/60/80/100/120/140/160 + STD/XS/XXS over
  the standard NPS range. Combinations not tabulated are reachable via the
  explicit wall-thickness path.
- **Product standard:** API 5L / ISO 3183 (line pipe); grades from the canonical
  matrix (A25..X120).
- **Per size:** ID, D/t, metal cross-section area, plain-end weight (lb/ft and
  kg/m), bore area, internal volume (m³/m and bbl/ft).
- **Query API:** `line_pipe(grade, nps=, schedule=)` or
  `line_pipe(grade, od_in=, wt_in=)`; schedule labels normalise
  (`"sch80"`/`"80"`/`"SCH 80"`).

## Plain-end weight validation

Weight is computed as metal area × density (7850 kg/m³, the matrix default),
which reproduces ASME B36.10M's weight column. The closed form is
`w = 10.68 · t · (OD − t)` lb/ft. Validated against published ASME B36.10M
plain-end weights (tolerance 0.3 %, since the formula is exact and the published
column is rounded):

| NPS | Schedule | OD × t (in) | Computed | Published (lb/ft) |
|---|---|---|---|---|
| 1 | STD | 1.315 × 0.133 | 1.68 | 1.679 |
| 2 | STD | 2.375 × 0.154 | 3.65 | 3.653 |
| 8 | STD | 8.625 × 0.322 | 28.58 | 28.55 |
| 8 | XS | 8.625 × 0.500 | 43.4 | 43.39 |
| 12 | STD | 12.750 × 0.375 | 49.6 | 49.56 |
| 16 | SCH 40 | 16.0 × 0.500 | 82.8 | 82.77 |
| 24 | XS | 24.0 × 0.500 | 125.6 | 125.49 |

All within 0.15 %. Weight is also asserted to equal area × density internally.

## Tests

`tests/materials/test_line_pipe.py` — 39 tests: OD table spot-checks (1/8 .. 80),
the published-weight goldens above, area↔weight consistency, geometry
derivations (ID, D/t, metal area, mm), bore-volume sanity, grade integration
(`LinePipe.grade` is the #1088 `MaterialGrade`), schedule normalisation, the
explicit OD/WT path, error handling, and catalog coverage. All passing
(black + flake8 clean).

## Builds on / relates to

- `materials/grades.py` (#1088) — the grade carried by each `LinePipe`.
- `subsea/pipeline/pipeline_pressure.py` `PipeGeometry` (SI/m) and
  `data_systems/.../pipe_db_client.py` (the prior partial NPS 2–36 schedule
  table this supersedes for the catalog). Migration of those consumers onto the
  catalog is tracked with the broader table consolidation (#1089).

## Deferred

API 5CT casing (#1090) and API 11B sucker rod (#1091) follow the same
product-object pattern (geometry + grade + code) on this backbone.
