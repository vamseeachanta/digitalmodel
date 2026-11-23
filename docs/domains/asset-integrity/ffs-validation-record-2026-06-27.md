# FFS Decision Layer — Consolidated Validation Record

**Date:** 2026-06-27 · **Issue:** #1061 (FFS epic #1057)

This is the canonical validation reference for the FFS decision layer. Each
method is validated against a published reference or a hand calculation, and
pinned by a golden test. The cross-cutting checks live in
`tests/asset_integrity/test_ffs_validation.py`; per-module golden tests are
cited per row.

## Validation summary

**Part 5 qualification correction (2026-10-06, issue 2287):** the Level 1/2
rows below establish existing implementation checks, not edition-matched 2021
Part 5 qualification for an axial-length/circumferential-width envelope. The
canonical simplified Folias helper, circumferential polynomial helper and
legacy fixture anchors disagree; L1 omits damage extent and L2 omits width.
See [the source audit](uniform-loss-source-audit-2026-10-06.md) for exact record
coverage, unresolved clause requests and conditional fixes/tests. Other method
rows retain their existing independent validation scopes.

| # | Method / module | Reference | Check | Result | Test |
|---|---|---|---|---|---|
| 1 | **Folias bulging factor** (`level2_engine._folias_factor`) | API 579-1/ASME FFS-1 Table 4.4 (M_t = √(1+0.48λ²)) | λ=1 / 2 / 5 | M_t = 1.217 / 1.709 / 3.606 ✓ | `test_ffs_validation::test_folias_factor_matches_api579_table_4_4` |
| 2 | **Level 1 screening** (`level1_screener`) | B31.4 / B31.8 / ASME VIII t_min formulas | t_mm vs t_min | pass/fail logic | `test_ffs_assessment` (Level 1) |
| 3 | **Level 2 RSF — GML/LML** (`level2_engine`) | API 579-1 Part 4 area-average / Part 5 Folias | RSF + verdict | within applicability | `test_ffs_assessment` (Level 2) |
| 4 | **Original B31G failure pressure** (`corroded_pipe.b31g_original`) | ASME B31G-2012 (1.1·SMYS, 2/3 area) | 30"×0.375", d=0.15", L=8", X52 | 1183 psi ✓ | `test_corroded_pipe`, `test_ffs_validation` |
| 5 | **Modified B31G failure pressure** (`modified_b31g`) | ASME B31G-2012 (SMYS+10 ksi, 0.85 area) | same defect | 1219 psi ✓ | `test_corroded_pipe`, `test_ffs_validation` |
| 6 | **B31G allowable length chart** (`b31g_original_allowable_length`) | ASME B31G-2012 page-26 table, OD 20" | 7 cells | ≤0.02" all ✓ | `test_corroded_pipe`, `test_ffs_validation` |
| 7 | **RSTRENG effective area** (`rstreng_effective_area`) | Modified-B31G effective-area method | rectangular profile ⇒ A/A0 = d/t (more conservative than 0.85 d/t) | ordering ✓ | `test_corroded_pipe` |
| 8 | **DNV-RP-F101** (`dnv_rp_f101.dnv_f101_single_defect`) | DNV-RP-F101 capacity equation | 30"×0.375", d=0.15", L=8", X52 (SMTS 66.7 ksi) | Q=1.662, P_cap=1334 psi ✓ | `test_dnv_rp_f101`, `test_ffs_validation` |
| 9 | **Plate buckling** (`structural_analysis.buckling`) | DNV-RP-C201 | golden | utilisation | `test_buckling_capacity` |
| 10 | **Stiffened-panel buckling** (`panel_buckling`) | 0119-015 worked example (DNV-RP-C201 §7.5.2) | A, NA, I, f_Epx, f_ET, λ_T, f_T | ≤0.2% ✓ | `test_panel_buckling`; doc `panel-buckling-validation-2026-06-26.md` |
| 11 | **Plate metal-loss FFS** (`plate_metal_loss_ffs`) | re-run validated buckling at reduced t | monotonicity, util=1 envelope | ✓ | `test_plate_metal_loss_ffs` |
| 12 | **Measurement sufficiency** (`measurement_sufficiency`) | API 579 grid-adequacy / COV practice | branch logic | ✓ | `test_measurement_sufficiency` |
| 13 | **Coordinator** (`ffs_coordinator`) | end-to-end chain | unified record | ✓ | `test_ffs_coordinator` |

## DNV-RP-F101 Part B factor correction (2026-10-10)

Scope: [issue 2181](https://github.com/vamseeachanta/digitalmodel/issues/2181),
[PR 2338](https://github.com/vamseeachanta/digitalmodel/pull/2338), fix-up fz2338.

The single-defect Part B path is validated separately from the unchanged row 8
capacity anchor. For the row 8 geometry, the regenerated capacity is
1334.194009224692 psi. With modelling factor F1 = 0.9 and caller operational
factor F2 = 0.72, total F = 0.648 gives safe working pressure 864.557717977600 psi
(864.56 psi rounded). This is the Part B pressure criterion used by the screen.
The caller shall establish F2 against the original design basis; 0.72 is a
synthetic example input, not a universal DNV operational factor.

`test_dnv_rp_f101::test_part_b_explicit_factors_and_legacy_validation_record`
pins the explicit factors and preserves legacy total-F pressure
960.619686641778 psi, capacity and Q. The original row 8 capacity numbers remain
unchanged. `test_pipeline_defect_screen_review::test_confirmed_boundary_preserves_reference_pressures`
pins the corrected pressure, while
`test_dnv_governs_derate_between_old_and_corrected_limits` checks F2 = 0.5:
600.387304151111 psi is below the 630 psi demand and requires DERATE.
F2 <= 0, F2 > 1 and nonfinite F2 are rejected by the explicit engine path.

Reference: DNV-RP-F101 Part B, total usage factor and single-defect pressure
criterion. All values above are regenerated from the reference geometry;
no licensed table is reproduced. Catalog `dnv-f101` remains live for this bounded
single-defect Part B workflow, not Part A or interacting-defect qualification.

## Cross-method consistency
For the reference defect (30"×0.375", d=0.15", L=8"), failure pressures order
**original B31G (1183) < Modified B31G (1219) < DNV-RP-F101 (1334)** — original
is most conservative; DNV runs highest because it uses SMTS rather than SMYS.
Pinned by `test_ffs_validation::test_method_ordering_for_reference_defect`.

## Accuracy envelope & limitations
- B31G / Modified B31G / RSTRENG / DNV-RP-F101: single blunt metal-loss defect,
  longitudinally oriented; no crack-like flaws (→ Level 3 / BS 7910).
- Folias factor uses the API 579 cylindrical-shell form; toroidal/spherical not
  covered.
- Plate/panel: simply-supported field, effective width = stiffener spacing;
  stiffener tripping included, lateral pressure not.
- The lookup MAOP datum is a Barlow design pressure at the B31.8 class-1 factor
  (0.72), used only as a common acceptance basis across methods.

## Supporting validation documents
- `docs/domains/asset-integrity/b31g-validation-2026-06-27.md` (B31G vs ASME table)
- `docs/domains/plate-buckling/panel-buckling-validation-2026-06-26.md` (panel vs 0119-015)
- `docs/domains/asset-integrity/ffs-architecture.md` (canonical-vs-legacy)
