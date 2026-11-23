# Grillage / Cross-Stiffened Panel Buckling — Validation

**Module:** `src/digitalmodel/structural/structural_analysis/grillage_buckling.py`
**Tests:** `tests/structural/structural_analysis/test_grillage_buckling.py`
**Code basis:** DNV-RP-C201 *Buckling Strength of Plated Structures*, Sec. 7.3–7.4
**Issue:** digitalmodel #1081 (EPIC #1080, Workstream A, Phase 1)
**Date:** 2026-06-28

## 1. Scope

A **grillage** is a plate field stiffened by *both* longitudinal stiffeners
and transverse frames (the real hull-plating layout). The pre-existing
`StiffenedPanelBucklingAnalyzer` only models a single longitudinal stiffener +
attached plate. This module adds the cross-stiffener (orthogonally stiffened)
interaction and reports the governing mode among four:

| Mode | Solver | Length scale |
|------|--------|--------------|
| `plate_field` | `PlateBucklingAnalyzer` (reused) | bay `s_T × s_L` |
| `longitudinal_stiffener` | `StiffenedPanelBucklingAnalyzer.check_panel` (reused, unchanged) | column `L_eff = s_T` |
| `transverse_frame` | `column_buckling` (+ in-domain `torsional_buckling`, reused) | column `L_eff = width_y` |
| `overall_grillage` | **new** orthotropic-plate mode | gross `a = length_x`, `b = width_y` |

Only the **overall orthotropic mode + the governing-mode aggregation** are new
physics. Everything else delegates to the validated single-stiffener solvers
(`effective_section`, `plate_induced` via `check_panel`, `torsional_buckling`,
`column_buckling`, `johnson_ostenfeld`).

## 2. Two length scales (kept distinct)

- **Longitudinal-stiffener column** spans *between transverse frames*:
  `L_eff = s_T` (the transverse spacing).
- **Overall grillage** spans the *gross* length: `a = length_x = L_g`.

Conflating these is the classic grillage error; the geometry object stores
`length_x` separately from `transverse.spacing`.

## 3. Overall orthotropic mode (the new physics)

For a simply supported orthotropic plate `a × b` under uniaxial compression
`σ_x`, the critical axial force per unit width is

```
N_x(m, n) = π² [ Dx (m/a)² + 2 H (n/b)² + Dy (n/b)⁴ (a/m)² ]
```

minimised over integer half-wave counts `m ≥ 1` (along `x`) and `n ≥ 1`
(along `y`; `n = 1` governs for pure `σ_x`). Smeared flexural rigidities:

```
Dx = E · I_L / s_L         longitudinal stiffener + full-spacing attached plate
Dy = E · I_T / s_T         transverse frame + C_xs effective-width attached plate
H  = √(Dx · Dy)            equivalent-isotropic torsional rigidity
```

`I_L`, `I_T` come from the validated `effective_section`. The critical stress
is `σ_e = N_x,cr / t_x` with smeared axial thickness `t_x = t_p + A_L / s_L`
(A_L = longitudinal stiffener area). The inelastic **Johnson-Ostenfeld**
correction (reused) yields the characteristic capacity `σ_cr`.

`H = √(Dx·Dy)` is the standard simplification: it makes an equally-stiffened
panel collapse to the isotropic plate result and vanishes correctly as either
stiffener direction softens (so the limit behaviour below is exact).

### Effective width (risk guard)
The transverse spacing `s_T` is wide, so the full-spacing attached plate would
overstate `I_T`. `Dy` therefore uses the DNV-RP-C201 `C_xs` effective-width
reduction:

```
λ_p  = 0.525 (s_T / t_p) √(f_y / E)
C_xs = 1.0                       if λ_p ≤ 0.673
C_xs = (λ_p − 0.22) / λ_p²       otherwise
b_eff,T = C_xs · s_T
```

### Multi-bay guard (risk guard)
`Dy = E·I_T/s_T → 0` as `s_T → ∞`, which would let the overall mode spuriously
dominate a single-bay panel. The overall mode is therefore **suppressed**
(`overall_mode_applicable = False`, utilisation 0) unless the gross length spans
**≥ 2 frame bays** (`num_frame_bays = round(length_x / s_T) ≥ 2`). For a single
bay the longitudinal-stiffener column is the correct governing mode.

## 4. Golden values

### G1 — Isotropic collapse to DNV `k = 4` (independent closed form)
With `Dx = Dy = H = Dp = E t³ / [12(1−ν²)]`, a long panel (`a = 3b`) gives
`N_x,cr = 4π²·Dp/b²`, so `σ_e = 4π²·Dp/(b²·t)`.

For `t = 12 mm, b = 700 mm, E = 210000 MPa, ν = 0.3`:
`Dp = 210000·12³/(12·0.91) = 3.3231×10⁷ N·mm`
`σ_e = 4π²·3.3231e7 / (700²·12) = 223.11 MPa`

This equals the DNV plate-buckling stress `σ = k·π²E/[12(1−ν²)]·(t/b)²` with
`k = 4` exactly. **→ `test_orthotropic_collapses_to_isotropic_k4_plate`** (and
a direct equality to the `k=4` formula at `rel=1e-9`).

### G2 — Euler limit (independent closed form)
With `Dy = H = 0` the formula reduces to `N_x,cr = π²·Dx·m²/a²`, minimised at
`m = 1` → `N_x,cr = π²·Dx/a²` (Euler buckling of the longitudinal direction
over the gross length). **→ `test_orthotropic_euler_limit_single_longitudinal`**.

### G3 — Defined grillage, overall mode
AH36 (`E = 206000, f_y = 355, ν = 0.3`), plate `t = 10`, longitudinal flat-bar
`150×10 @ s_L = 600`, transverse flat-bar `250×12 @ s_T = 2400`,
`length_x = 9600` (4 bays), `width_y = 3600`:

| Quantity | Value |
|----------|-------|
| `I_L` (full-width attached plate) | 1.054×10⁷ mm⁴ |
| `C_xs` (transverse) | 0.183 |
| `I_T` (effective-width attached plate) | 4.580×10⁷ mm⁴ |
| `Dx` | 3.620×10⁹ N·mm |
| `Dy` | 3.931×10⁹ N·mm |
| `H = √(Dx·Dy)` | 3.772×10⁹ N·mm |
| `t_x` | 12.5 mm |
| min at | `m = 3, n = 1` |
| `σ_e` (elastic) | **927.9 MPa** |
| `σ_cr` (Johnson-Ostenfeld, < f_y) | **321.0 MPa** |

**→ `test_overall_mode_golden_values`**. (σ_e well above f_y confirms that for
a normally-stiffened grillage the overall mode rarely governs — it is the
lightly-stiffened, wide-bay panels where it bites; see §5.)

### G4 — Closer transverse frames raise the overall capacity (monotone)
Same grillage, `s_T ∈ {4800, 3200, 2400, 1600, 1200}` → `σ_e =
{664, 820, 928, 1122, 1299}` MPa: strictly increasing as `s_T` decreases.
**→ `test_closer_transverse_frames_raise_overall_capacity`**.

### G5 — Reduces to the single stiffener (acceptance criterion)
One-bay grillage (`s_T = length_x = 9600`): overall mode suppressed, and the
grillage `longitudinal_utilization` equals the standalone
`StiffenedPanelBucklingAnalyzer.check_panel` result to `rel=1e-12`; governing
mode = `longitudinal_stiffener`.
**→ `test_single_bay_reduces_to_single_stiffener`**.

### G6 — Governing-mode selection switches correctly
`length_x = 14400`, `σ_x = 150`: at `s_T = 4800` slender longitudinal columns
govern (util > 1); at `s_T = 1800` the plate field governs. Under biaxial load
(`σ_x = σ_y = 120`) the transverse frame governs.
**→ `test_governing_mode_switches_with_layout`,
`test_transverse_frame_can_govern_under_biaxial_load`**.

## 5. Simplification level vs a full FE grillage analysis

This is a **screening** model, not an FE plate/beam grillage. Relative to FE:

- **Smeared orthotropic idealisation** — discrete stiffener positions, local
  stiffener flexibility and the true coupled mode shapes are homogenised into
  `Dx, Dy, H`; `H = √(Dx·Dy)` neglects the stiffeners' own (small, open-section)
  St-Venant torsional contribution.
- **Boundary / loading** — simply supported on all four edges; no rotational
  edge restraint, no lateral pressure / continuous-beam bending, no residual
  stresses. Only the leading buckle mode (integer `m, n` sweep), not the full
  eigenvalue spectrum.
- **Transverse-frame mode** — its tripping (torsional) check is only evaluated
  inside the panel solver's validated domain (`spacing ≤ span`, non-zero axial);
  a widely spaced frame uses column buckling only.

Use it to **rank modes and flag a grillage for detailed FE**, not as a final
capacity statement for a critical structure.

## 6. What is reused vs new

| Reused (validated, unchanged) | New |
|-------------------------------|-----|
| `PlateBucklingAnalyzer.check_plate_buckling`, `johnson_ostenfeld` | `orthotropic_critical_force` (m,n sweep) |
| `StiffenedPanelBucklingAnalyzer.effective_section` | `flexural_rigidities` (Dx, Dy, H, t_x) |
| `.check_panel`, `.column_buckling`, `.torsional_buckling` | `overall_buckling` (σ_e, J-O, multi-bay guard) |
| `MaterialProperties`, `MARINE_GRADES`, `StiffenerGeometry` | `GrillageGeometry`, `GrillageBucklingAnalyzer`, `check_grillage` |
| | `C_xs` effective-width for the transverse plate |
