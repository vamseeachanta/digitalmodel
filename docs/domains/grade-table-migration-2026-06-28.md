<!-- ABOUTME: Convergence-delta report for the API 5L grade-table migration (#1089). -->
<!-- ABOUTME: Legacy-per-consumer values vs canonical materials.grades; sign-off gate. -->
# API 5L Grade-Table Migration — Convergence-Delta Report

**Issue:** [#1089](https://github.com/vamseeachanta/digitalmodel/issues/1089) (EPIC #1080, Workstream B, Phase 2)
**Date:** 2026-06-28
**Status:** PRESERVE migration shipped (zero value change). Convergence is a *future* step gated on this report.

## What this PR did

Routed the eight scattered hardcoded API 5L line-pipe grade dicts through the
single canonical matrix `digitalmodel.materials.grades`, via thin adapter
"views". **No numeric value changed** — every consumer keeps its exact legacy
value, so all existing golden tests stay green. The win is *consolidation into
one module* + this documented reconciliation, **not** forcing every value to
canonical.

Where the legacy value already equalled a canonical-derived value, the view
derives it (canonical-derived). Where it diverges, the view returns a
**legacy-preserved** snapshot stored in `grades.py`. This table is the artifact
to sign off **before** any value is converged.

## Canonical reference (`materials.grades`, MPa)

API 5L / ISO 3183. `smys_mpa` = ISO L-grade; `smts_mpa` = PSL2 min;
`smts_psl1_mpa` = PSL1 min (US-customary) where it differs.

| Grade | grade_ksi | smys_mpa (ISO) | smts PSL2 | smts PSL1 |
|-------|-----------|----------------|-----------|-----------|
| X42   | 42  | 290 | 415 | 414 |
| X46   | 46  | 320 | 435 | 434 |
| X52   | 52  | 360 | 460 | 455 |
| X56   | 56  | 390 | 490 | 489 |
| X60   | 60  | 415 | 520 | 517 |
| X65   | 65  | 450 | 535 | 530 |
| X70   | 70  | 485 | 570 | 565 |
| X80   | 80  | 555 | 625 | 620 |
| X100  | 100 | 690 | 760 | —   |

## Migration map (consumer -> view)

| # | Consumer (constant) | Unit | View added | Kind |
|---|---------------------|------|------------|------|
| 1 | `asset_integrity/corroded_pipe.py` `SMYS_PSI` | psi | `legacy_smys_psi_dict()` | canonical-derived (grade-ksi×1000) |
| 2 | `asset_integrity/dnv_rp_f101.py` `SMTS_PSI` | psi | `legacy_smts_psi_dict()` | canonical-derived (PSL2→psi, round 100) |
| 3 | `asset_integrity/pipeline_skill.py` `SMYS_TABLE` | MPa | `legacy_smys_table_mpa()` | legacy-preserved (US-customary SMYS) |
| 4 | `data_systems/.../pipe_db_client.py` `API_5L_GRADES` | MPa | `legacy_pipe_db_grades_mpa()` | legacy-preserved |
| 5 | `structural/analysis/wall_thickness_parametric.py` `API_5L_GRADES` | Pa | `legacy_pipe_db_grades_pa()` | legacy-preserved (= #4 ×1e6) |
| 6 | `orcaflex/riser_config.py` `PIPE_GRADES` | MPa | `legacy_riser_pipe_grades_mpa()` | legacy-preserved (E/ρ canonical) |
| 7 | `subsea/pipeline/pipeline_pressure.py` `MATERIAL_LIBRARY` | MPa | `legacy_pipeline_material_library_mpa()` | legacy-preserved |
| 8 | `solvers/orcaflex/.../linetype_builder.py` `MATERIAL_PROPERTIES` (`DNVSTF101Fy`) | kN/m² | `smys_kn_m2()` | canonical-derived (ISO SMYS ×1000) |

**Out of scope (untouched):** `structural_analysis/models.py` marine IACS + EN
grades (different grade systems), general steels (carbon/stainless/titanium in
catenary/viv models), and the deprecated `steel_material.yml`.
`pipe_db_client.STEEL_PROPERTIES` (generic E/ν/ρ/shear, not grade-specific, and
`shear_modulus` is not in the matrix) was also left as-is.

## Convergence deltas — SMYS (specified minimum yield)

Legacy SMYS values are **US-customary** (grade ksi → MPa, historically rounded)
and sit 1–3 MPa **below** canonical ISO SMYS. There is also small per-module
drift on the low grades (358 vs 359; 413 vs 414; 482 vs 483).

| Grade | Canonical ISO | pipeline_skill (#3) | pipe_db/wall_thk (#4,#5) | riser_config (#6) | pipeline_pressure (#7) | linetype (#8) | Δ vs canonical |
|-------|---------------|--------------------|--------------------------|-------------------|------------------------|---------------|----------------|
| X42   | 290 | —   | **289** | —   | —   | —   | −1 MPa (−0.34%) |
| X52   | 360 | **359** | **359** | **358** | **358** | 360 ✓ | −1…−2 MPa (−0.28…−0.56%) |
| X60   | 415 | **414** | **414** | **414** | **413** | 415 ✓ | −1…−2 MPa (−0.24…−0.48%) |
| X65   | 450 | **448** | **448** | **448** | **448** | 450 ✓ | −2 MPa (−0.44%) |
| X70   | 485 | **483** | **483** | **483** | **482** | 485 ✓ | −2…−3 MPa (−0.41…−0.62%) |
| X80   | 555 | **552** | **552** | **552** | —   | —   | −3 MPa (−0.54%) |
| X100  | 690 | 690 ✓ | —   | —   | —   | —   | 0 |

Bold = diverges from canonical and is preserved as legacy. ✓ = already canonical.

## Convergence deltas — SMTS (specified minimum tensile)

dnv_rp_f101 (#2) uses **PSL2** (canonical, exact). The MPa consumers (#4–#7) use
a **PSL1-ish** UTS, with X65 and X80 sitting +1 MPa above the canonical PSL1.

| Grade | Canonical PSL2 | Canonical PSL1 | Legacy MPa (#4–#7) | Δ vs PSL1 | Δ vs PSL2 |
|-------|----------------|----------------|--------------------|-----------|-----------|
| X42   | 415 | 414 | **414** | 0 | −1 (−0.24%) |
| X52   | 460 | 455 | **455** | 0 | −5 (−1.09%) |
| X60   | 520 | 517 | **517** | 0 | −3 (−0.58%) |
| X65   | 535 | 530 | **531** | +1 (+0.19%) | −4 (−0.75%) |
| X70   | 570 | 565 | **565** | 0 | −5 (−0.88%) |
| X80   | 625 | 620 | **621** | +1 (+0.16%) | −4 (−0.64%) |

dnv_rp_f101 `SMTS_PSI` (psi) = canonical PSL2 → psi rounded to nearest 100 psi
(exact, no delta).

## Future-convergence candidates (require sign-off)

All deltas are small (≤ ~1.1%) and engineering-conservative (legacy values are
≤ canonical for SMYS, and ≤ PSL2 for SMTS). Recommended convergence order, each
to be done only after confirming no golden-baseline regression:

1. **SMYS → canonical ISO** for the MPa/Pa consumers (#3–#7): unify X52 360,
   X60 415, X65 450, X70 485, X80 555, X42 290. Removes the 358/359 and
   413/414/482/483 per-module drift. *Slightly less conservative* (raises
   allowable hoop stress / wall-thickness sizing) — review DNV/API design-factor
   margins before adopting.
2. **SMTS X65 531→530 and X80 621→620** (#4–#7): align to canonical PSL1.
3. **Decide PSL1 vs PSL2 policy** for the MPa SMTS consumers (currently PSL1-ish;
   dnv_rp_f101 is PSL2). Pick one convention per assessment code.

Until sign-off, the legacy-preserved views are the source of truth and **must
not** be "fixed" toward canonical.
