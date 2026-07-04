# Corrugated bulkhead buckling — validation (2026-06-28)

Issue: digitalmodel #1085 (EPIC #1080, Workstream A, Phase 3).
Module: `src/digitalmodel/structural/structural_analysis/corrugated_bulkhead.py`
Tests: `tests/structural/structural_analysis/test_corrugated_bulkhead.py` (11 tests).
Code reference carried on results: `DNV-RP-C201 / IACS CSR`.

## What this models

A corrugated bulkhead is a folded plate forming a trapezoidal wave. The fold
geometry gives a large bending stiffness in the corrugation direction (it acts
as a deep beam) with failure modes distinct from a flat or stiffened panel. We
provide:

1. **Trapezoidal-corrugation section properties** (rigorously validated closed
   forms — the core deliverable).
2. **Buckling screens** for the governing failure mode: local flange plate
   buckling, local web plate buckling, overall column buckling, web shear
   buckling. These **reuse** the validated DNV-RP-C201 solvers — no plate or
   column physics is reimplemented.

## Geometry of the repeating half-pitch unit

Corrugation angle `phi` is measured from the bulkhead plane. Inputs are the
flange (face) width `a`, the web **slant** width `c`, plate thickness `t`
(separate `t_f`/`t_w` supported), and `phi`. Derived:

- corrugation depth `d = c·sin(phi)`  (perpendicular distance between flanges)
- web horizontal projection `c·cos(phi)`
- half pitch `s = a + c·cos(phi)`  (one flange + one web)
- full pitch `2s` (two flanges + two webs)

Important: `c` is the **slant** web width, so web material area is `t_w·c`. The
horizontal projection `c·cos(phi)` is used only for the pitch `s`, never for the
web area. Depth is derived from `c` and `phi` so the inputs cannot be made
mutually inconsistent.

## Section-property derivation (per half-pitch / per corrugation)

Neutral axis lies at mid-depth `d/2`. (Full-pitch centroid:
`[a·t_f·d + a·t_f·0 + 2·c·t_w·(d/2)] / [2(a·t_f + c·t_w)] = d/2` when both
flanges share thickness `t_f`.)

- **Area**: `A = a·t_f + c·t_w`
- **Moment of inertia** about the NA:
  - flange at `|z| = d/2`: `a·t_f·(d/2)²`
  - inclined web spanning `z = 0…d` (centroid at NA, own second moment of a
    uniform strip `= A_web·d²/12`): `c·t_w·d²/12`
  - `I = a·t_f·(d/2)² + c·t_w·d²/12`
- **Section modulus** (extreme fibre = flange at `d/2`):
  `Z = I/(d/2) = d·(3·a·t_f + c·t_w)/6`  ← the IACS CSR corrugated-bulkhead form.

With equal thickness `t` these collapse to the clean closed forms:

- `I = t·d²·(3a + c)/12`
- `Z = t·d·(3a + c)/6`
- `A = t·(a + c)`

Per-unit-width values divide by the half pitch `s`.

## Golden values (hand-derived)

### Golden 1 — equal thickness: a=300, c=200, t=10 mm, phi=30°
`d = 200·sin30 = 100 mm` (exact), `s = 300 + 200·cos30 = 473.2050808 mm`.

| Quantity | Formula | Value |
|----------|---------|-------|
| A (half-pitch) | `t(a+c) = 10·500` | 5 000 mm² |
| I (half-pitch) | `t·d²(3a+c)/12 = 10·10⁴·1100/12` | 9 166 666.67 mm⁴ |
| Z (half-pitch) | `t·d(3a+c)/6 = 10·100·1100/6` | 183 333.33 mm³ |
| Z per unit width | `Z/s` | 387.43 mm³/mm |
| I per unit width | `I/s` | 19 371.6 mm⁴/mm |

### Golden 2 — unequal thickness (general IACS form): t_f=12, t_w=8, a=300, c=200, phi=30°
`d = 100 mm`.

| Quantity | Formula | Value |
|----------|---------|-------|
| Z (half-pitch) | `d(3·a·t_f + c·t_w)/6 = 100·(10800+1600)/6` | 206 666.67 mm³ |
| I (half-pitch) | `a·t_f·(d/2)² + c·t_w·d²/12 = 9e6 + 1.333e6` | 10 333 333.33 mm⁴ |

This case validates the general (unequal-thickness) form and the NA at `d/2`.

### Golden 3 — symmetric 45° corrugation (independent cross-check): a=c=800, t=15 mm, phi=45°
`d = 800·sin45 = 565.685 mm`, `d² = 320000`.

| Quantity | Formula | Value |
|----------|---------|-------|
| A (half-pitch) | `t(a+c) = 15·1600` | 24 000 mm² |
| I (half-pitch) | `t·d²(3a+c)/12 = 15·320000·3200/12` | 1.280×10⁹ mm⁴ |
| Z (half-pitch) | `t·d(3a+c)/6` | 4.525×10⁶ mm³ |
| flange elastic plate buckling | `σ_E = 4π²E/[12(1−ν²)]·(t/a)²`, E=206000, ν=0.3 | 261.82 MPa |

## Buckling screens (what is reused, simplification level)

- **Local flange plate buckling** — `PlateBucklingAnalyzer.check_plate_buckling`
  on a plate element of width = flange width `a` (short edge), length = bulkhead
  span, thickness `t_f`. Test `test_flange_buckling_matches_plate_solver`
  confirms the returned utilisation equals a direct plate-solver call.
- **Local web plate buckling** — same solver on a plate element of width =
  web slant width `c`, carrying axial + shear.
- **Overall column buckling** — `ColumnBucklingAnalyzer.check_column_buckling`
  with the half-pitch `A` and `I`, effective length = span. The partial factor
  `gamma_m` is passed **explicitly and consistently** (same value as the plate
  checks; default 1.15) rather than relying on the column solver's `1.0`
  default.
- **Web shear buckling** — classical `k_tau` coefficient with the **same**
  Johnson-Ostenfeld inelastic knockdown as the plate solver.

`check_corrugated_bulkhead` returns the governing (highest-utilisation) mode.

## Simplification level — PRELIMINARY

`PRELIMINARY = True` (carried on `CorrugatedBulkheadResult.preliminary`).

The **section properties (Z, I, A)** are rigorously validated closed forms
(three golden cases, two thickness regimes, NA verified, IACS CSR section
modulus reproduced). The **buckling screens** reuse the validated DNV-RP-C201
plate/column solvers, but no in-repo CSR worked example anchors the combined
overall-buckling path, so the combined check is flagged preliminary. It is a
screening tool: section-property accuracy is production-grade; the
mode-governing utilisation is a conservative screen pending a CSR worked-example
anchor.
