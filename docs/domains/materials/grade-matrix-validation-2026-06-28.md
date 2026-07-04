# Material Grade Matrix — Validation Record (2026-06-28)

Backing for `src/digitalmodel/materials/` (issue #1088, EPIC #1080 workstream B).
The matrix is the single source of truth that the line-pipe (#1087), casing
(#1090), rod (#1091) and structural modules consume, replacing 8+ scattered
strength dicts that disagreed on values and units.

## Scope

| Standard | Grades | Count |
|---|---|---|
| API 5L / ISO 3183 | A25, A, B, X42, X46, X52, X56, X60, X65, X70, X80, X90, X100, X120 | 14 |
| IACS UR W11 (hull) | Grade A/B/D/E (235) + {A,D,E,F}H 32 (315) / 36 (355) / 40 (390) | 16 |
| EN 10025-2 | S275, S355, S420 | 3 |

Stored in **MPa** (SI); psi properties and back-compat adapters provided.

## Reconciliation of the prior disagreement

The scattered tables disagreed on X52 in two distinct ways, both now resolved
and documented per record:

1. **SMYS 358 / 359 / 360 MPa** — 358–359 are the US-customary `52 ksi`
   conversion (52,000 psi = 358.5 MPa); 360 MPa is the ISO 3183 metric grade
   `L360`. The matrix stores the ISO metric value (`smys_mpa`) and preserves the
   US designation via `grade_ksi` for psi back-compat.
2. **SMTS 455 vs 460 MPa** — this is the **PSL1 vs PSL2** split, *not* an error.
   PSL1 X52 minimum tensile = 455 MPa; PSL2 = 460 MPa. The matrix stores PSL2 in
   `smts_mpa` and PSL1 in `smts_psl1_mpa`; `smts_mpa(name, psl=...)` selects.

The same PSL1/PSL2 pattern explains the X60 (517/520), X65 (530/535), X70
(565/570) and X80 (620/625) differences seen across the legacy dicts.

## Golden values (published references)

Spot-checked against API 5L (46th ed.) / ISO 3183, IACS UR W11, EN 10025-2:

| Grade | SMYS (MPa) | SMTS (MPa) | Note |
|---|---|---|---|
| X52 | 360 | 460 (PSL2) / 455 (PSL1) | ISO L360 |
| X65 | 450 | 535 (PSL2) / 530 (PSL1) | ISO L450 |
| X80 | 555 | 625 (PSL2) / 620 (PSL1) | ISO L555 |
| X120 | 830 | 915 | PSL2 only |
| Grade A (hull) | 235 | 400 | NS, Charpy +20 C |
| AH36 / EH36 / FH36 | 355 | 490 | letter = toughness only |
| EH40 | 390 | 510 | HS-40 |
| S355 | 355 | 510 | EN, t ≤ 16 mm |

Young's modulus by family: line pipe 207 GPa, hull 206 GPa, EN structural
210 GPa; ν = 0.30, ρ = 7850 kg/m³.

## Back-compat verification

The legacy FFS dicts are **regenerated from the matrix** (not copied) and the
test-suite asserts equality against the live imports, which both documents the
convention and guards against drift:

* `legacy_smys_psi_dict()` == `asset_integrity.corroded_pipe.SMYS_PSI`
  — convention: SMYS psi = grade number (ksi) × 1000 (X52 → 52,000 psi).
* `legacy_smts_psi_dict()` == `asset_integrity.dnv_rp_f101.SMTS_PSI`
  — convention: PSL2 SMTS (MPa) × 145.0377, rounded to nearest 100 psi
  (460 MPa → 66,700 psi).

## Tests

`tests/materials/test_grade_matrix.py` — 29 tests: published-value golden
parametrizations (API 5L / IACS / EN), the legacy back-compat equalities, the
toughness-letter invariance, ISO-alias and name-collision lookup behavior
(API `A` vs hull `Grade A`), psi conversions, and registry completeness (33
grades). All passing.

## Deferred

Migrating the remaining scattered consumers (`pipe_db_client`, `riser_config`,
`pipeline_pressure`, `wall_thickness_parametric`, structural `models.py`, …)
onto this matrix is tracked by issue #1089. Some of those carry US-customary or
PSL1 roundings; #1089 will surface and resolve each value delta on migration.
