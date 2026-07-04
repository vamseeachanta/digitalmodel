<!-- ABOUTME: Validation record for the Efthymiou short-chord correction factor F1 (alpha<12, axial saddle terms). -->
<!-- ABOUTME: Documents the F1 formula + DNV-RP-C203 source and every saddle golden that changed from uncorrected to F1-corrected. -->

# Efthymiou Short-Chord Correction Factor F1 (2026-06-29)

## Summary

Adds the published **short-chord correction factor F1** (brace-axial load, fixed
chord ends) to the Efthymiou T/Y SCF set. F1 reduces the **saddle** SCFs (chord
saddle + brace saddle) for short chords (`alpha < 12`). This stacks on the
exponent fix in [`efthymiou-scf-correction-2026-06-28.md`](efthymiou-scf-correction-2026-06-28.md)
(PR #1133), which had **explicitly omitted** the short-chord factors so the
chord-saddle golden was the uncorrected `6.21`.

The repository's default geometry uses `L = 2.5*D` -> `alpha = 2L/D = 5`, which is
`< 12`, so F1 **should** apply but previously did not. This change applies it in
both:

- `src/digitalmodel/fatigue/scf_library.py::efthymiou_ty_axial` (new helper
  `short_chord_factor_f1`), and
- `src/digitalmodel/structural/structural_analysis/connection_scf.py::efthymiou_chord_saddle_axial_scf`
  (new `alpha` parameter, default 5), kept in agreement with the library.

## Source of truth

- DNV-RP-C203 (April 2010 / 2021), **Appendix B, Table B-1** — Stress
  Concentration Factors for Simple Tubular T/Y Joints, "Axial load — Chord ends
  fixed". The "Short chord correction" column lists **F1** against the chord
  saddle (Eqn. 1) and brace saddle (Eqn. 3); crown terms (Eqns. 2, 4) are marked
  "None".
- Efthymiou, M. (1988), "Development of SCF formulae and generalised influence
  functions for use in fatigue analysis", OTC 4829.

### F1 formula (confirmed verbatim against DNV-RP-C203 App. B)

```
F1 = 1 - (0.83*beta - 0.56*beta^2 - 0.02) * gamma^0.23 * exp(-0.21 * gamma^(-1.16) * alpha^2.5)   for alpha < 12
F1 = 1.0                                                                                            for alpha >= 12
```

with `beta = d/D`, `gamma = D/(2T)`, `alpha = 2L/D`.

Companion factors (out of scope for this axial, fixed-end function, noted only):
- **F2** — axial saddle, *pinned* chord ends: `1 - (1.43*beta - 0.97*beta^2 - 0.03) * gamma^0.04 * exp(-0.71 * gamma^(-1.38) * alpha^2.5)`.
- **F3** — out-of-plane-bending saddle terms: `1 - (0.55*beta^1.8) * gamma^0.16 * exp(-0.49 * gamma^(-0.89) * alpha^1.8)`.

F2/F3 are not applied here (F2 would belong to a pinned-end axial variant; F3 to
`efthymiou_ty_opb`, which is out of scope).

## Where F1 is applied

| Term (axial, fixed ends) | DNV Eqn. | Short-chord factor |
|--------------------------|----------|--------------------|
| Chord saddle | (1) | **F1** |
| Chord crown  | (2) | None |
| Brace saddle | (3) | **F1** |
| Brace crown  | (4) | None |

## Computed F1 values and SCF changes (round to 3 dp)

`efthymiou_ty_axial` `SCFResult` at the test geometries (all `alpha = 5`,
`theta = 90` unless noted). "old" = pre-F1 (PR #1133 value); "new" = F1-applied.

| Geometry (D,T,d,t,theta) | beta,gamma,alpha | F1 | chord old -> new | brace old -> new |
|--------------------------|------------------|------|------------------|------------------|
| 1000,50,500,25,90 (typical) | 0.5,10,5 | 0.808 | 5.173 -> 4.178 | 5.029 -> 4.062 |
| 1000,50,500,25,45           | 0.5,10,5 | 0.808 | 2.971 -> 2.400 | 2.788 -> 2.413* |
| 1000,50,300,15,90 (low beta)| 0.3,10,5 | 0.865 | 2.566 -> 2.221 | 3.853 -> 3.334 |
| 1000,50,800,40,90 (high beta)| 0.8,10,5 | 0.785 | 6.844 -> 5.370 | 4.892 -> 3.839 |
| 1200,50,600,25,90 (golden g=12)| 0.5,12,5 | 0.766 | 6.207 -> 4.755 | 5.775 -> 4.423 |

\* theta=45: after F1 the brace saddle (2.252) drops below the (unreduced) brace
crown (2.413), so the brace crown now governs `scf_brace` at 2.413.

The headline change: the chord-saddle golden **6.21 -> 4.755** (`6.207 * F1=0.766`).
Long chords are unaffected — at `alpha >= 12`, `F1 = 1.0` and the uncorrected
`6.21` is recovered (regression-tested).

## Test changes

`tests/fatigue/test_scf_library.py` (`TestEfthymiouTYAxialGolden`):
- `test_chord_saddle_golden_g12` — `6.21 -> 4.755`.
- `test_typical_axial_golden_values` — chord `5.173 -> 4.178`, brace `5.029 -> 4.062`, governing `5.173 -> 4.178`.
- `test_chord_saddle_theta45_golden` — chord `2.971 -> 2.400`.
- `test_chord_saddle_matches_connection_scf` — now passes `alpha=5`; both sides apply F1 (still agree, at 4.755).
- **Added** `test_long_chord_no_f1_recovers_uncorrected` — `alpha=12 -> 6.207`.
- **Added** `test_short_chord_factor_f1_value` — F1 published form at beta=0.5, gamma=12, alpha=5 -> 0.766; `alpha>=12 -> 1.0`.

`tests/structural/structural_analysis/test_connection_scf.py`:
- `test_golden_efthymiou_chord_saddle_axial` — `6.21 -> 4.755` (full-precision hand value now multiplies by F1).
- **Added** `test_long_chord_recovers_uncorrected_saddle` — `alpha=12 -> 6.207`.
- `test_efthymiou_angle_reduces_saddle_scf` / `test_efthymiou_rejects_nonpositive` — unchanged (F1 is angle-independent; the s45<s90 relation holds).

## Functions reviewed but NOT changed

- `efthymiou_ty_ipb` — in-plane bending has no saddle short-chord factor (DNV
  marks Eqns. 8/9 "None").
- `efthymiou_ty_opb` — its saddle terms take **F3**, not F1; out of scope here
  (this lane is brace-axial only). Noted for a future change.
- `efthymiou_k_axial` — simplified K-joint formulation, unchanged.
