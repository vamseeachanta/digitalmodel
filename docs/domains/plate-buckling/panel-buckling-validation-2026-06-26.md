# Stiffened-Panel Buckling — Validation Record

**Date:** 2026-06-26
**Module:** `digitalmodel.structural.structural_analysis.panel_buckling.StiffenedPanelBucklingAnalyzer`
**Standard:** DNV-RP-C201 ("Buckling Strength of Plated Structures")
**Issue:** #1045 · supersedes the PRELIMINARY status from #1043 / PR #1044

## Purpose

Promote the stiffened-panel buckling solver from PRELIMINARY to a production
("main") calculation by validating it against a worked example with known
intermediate quantities, and documenting the accuracy envelope.

## Reference case — 0119-015 worked example

Inputs (from `structural/plate_capacity/StiffnerBuckling_Cal/DataProvision/parameters_stiffnerbuckling.py`):

| Quantity | Value |
|---|---|
| Plate spacing s | 700 mm |
| Plate thickness t | 12 mm |
| Stiffener web | 600 × 10 mm (tee) |
| Stiffener flange | 200 × 20 mm |
| Stiffener span L | 2230 mm |
| Torsional restraint spacing L_T | 3500 mm |
| Yield strength f_y | 234.6 MPa (34 ksi) |
| Young's modulus E | 210 105 MPa (30 450 ksi) |
| Poisson ratio ν | 0.30 |
| Applied σ_x / σ_y / τ | 15.73 / 48.58 / 0.95 MPa |

The legacy spreadsheet's **final** resistances/utilisations are not used as
targets — they contain coding errors (equal NRd values, negative utilisations,
an exponent typo). Validation therefore targets the **trustworthy intermediate
quantities** (section properties, plate elastic stresses, torsional buckling),
each independently re-derived by hand.

## Validation results

| Quantity | Solver | Reference | Δ | Source of reference |
|---|---|---|---|---|
| Effective area A | 18 400 mm² | 18 400 mm² | 0.0% | hand calc = legacy |
| Neutral axis (from outer face) | 239.70 mm | 239.70 mm | 0.0% | hand calc = legacy |
| Second moment I | 1.255×10⁹ mm⁴ | 1.255×10⁹ mm⁴ | 0.0% | hand calc¹ |
| Plate elastic f_Epx (=3.62·E·(t/s)²) | 223.52 MPa | 223.52 MPa | 0.0% | analytic = legacy |
| Plate elastic f_Epy (=0.9·E·(t/s)²) | 55.57 MPa | 55.57 MPa | 0.0% | analytic = legacy |
| Plate elastic f_Ept (=5.0·E·(t/s)²) | 308.73 MPa | 308.73 MPa | 0.0% | analytic = legacy |
| Column slenderness λ̄ | 0.090 | 0.0902 | 0.2% | legacy |
| Torsional elastic f_ET | 442.0 MPa | 441.66 MPa | 0.1% | legacy |
| Torsional slenderness λ_T | 0.7285 | 0.729 | 0.1% | legacy |
| Torsional resistance f_T | 215.66 MPa | 215.61 MPa | 0.0% | legacy |

¹ The legacy spreadsheet reports I = 1.273×10⁹ mm⁴ because its flange
parallel-axis term references `Z_Gp` (measured from plate mid-thickness)
instead of the neutral axis `Y_GA` — a 6 mm inconsistency that inflates I by
1.45%. The solver returns the corrected value 1.255×10⁹ mm⁴. The legacy radius
of gyration (263.08 mm) inherits this error; the solver's 261.16 mm is correct.

These checks are encoded as golden tests `test_validation_0119_015_*` in
`tests/structural/structural_analysis/test_panel_buckling.py`.

## Method summary (what the solver computes)

Three modes are screened; the governing (max-utilisation) mode is reported:

1. **plate_induced** — plate-field buckling between stiffeners (`PlateBucklingAnalyzer`, k=4 simply-supported, Johnson-Ostenfeld).
2. **column** — Euler/flexural buckling of the plate+stiffener built-up column over the frame span (`ColumnBucklingAnalyzer`, EC3 curve "c").
3. **torsional** — stiffener tripping per DNV-RP-C201 Sec. 7.5.2:
   f_ET = β·[(A_w+(t_f/t_w)²A_f)/(A_w+3A_f)]·G·(t_w/h_w)² + π²·E·I_z/[(A_w/3+A_f)·L_T²],
   with the plate rotational-restraint coefficient β = (3C+0.2)/(C+0.2),
   C = (h_w/s)(t_p/t_w)³√(1−σ_j/f_e), and the DNV reduction giving f_T.

## Accuracy envelope / limitations

Validated for: tee / flat-bar longitudinal stiffeners, in-plane σ_x (+ σ_y, τ
for the β interaction), single stiffener with its plate flange, ship-steel
grades. **Not** included: effective-width reduction for very slender plate
fields (full spacing is used), lateral-pressure beam bending of the stiffener,
girder-level interaction, angle-stiffener lateral offset. Within these bounds
the solver reproduces the reference intermediates to ≤0.2%.
