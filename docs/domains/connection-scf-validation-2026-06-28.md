# Connection & Bracket Stress-Concentration Validation (DNV-RP-C203)

Issue: digitalmodel #1086 (Ship-struct A6, EPIC #1080, Workstream A, Phase 3).
Module: `src/digitalmodel/structural/structural_analysis/connection_scf.py`
Tests:  `tests/structural/structural_analysis/test_connection_scf.py` (14 tests)
Code basis: **DNV-RP-C203** (Fatigue design of offshore steel structures, 2021).

## Scope

Local detail checks at loaded ship/offshore connections:

- **Frame-to-girder / girder-to-shell junctions** — plate butt / cruciform
  junctions with a fit-up eccentricity that bends an axially-loaded plate
  (axial-misalignment SCF).
- **Soft-toe brackets** — bracket toes welded to plating; a ground/tapered
  ("soft") toe lowers the weld-toe SCF vs a square ("hard") toe.
- **Attachment lugs / tubular brace stubs** — loaded tubular attachment on a
  chord, governed by the Efthymiou parametric chord-saddle SCF.

## What is reused (no physics reimplemented)

This module is a thin **ship-detail orchestration** layer over the existing
fatigue library:

| Reused from | Function | Used for |
|---|---|---|
| `fatigue/scf_library.py` | `scf_butt_weld_misalignment` (1 + 3 e/t) | pinned junction misalignment |
| `fatigue/scf_library.py` | `scf_cruciform_joint` (km = 1 + 6 e/t) | restrained junction misalignment |
| `fatigue/scf_library.py` | `scf_fillet_weld_toe` | bracket-toe (soft/hard) SCF |
| `fatigue/hotspot_stress.py` | `extrapolate_hotspot_linear` (1.67/−0.67) | weld-toe hot-spot extrapolation |
| `fatigue/weld_classification.py` | `classify_weld_detail` | DNV S-N detail class (E/F/F1/W3...) |

The only new closed-form physics added is the **published Efthymiou
chord-saddle** equation (see reconciliation note below).

## Approach: detail class AND SCF

DNV-RP-C203 handles welded ship attachments/brackets mainly by **S-N detail
classification** (E / F / F1 / F3 by attachment length & load path), *not* by a
single multiplicative SCF. `assess_connection()` therefore returns **both**:

1. the DNV S-N detail class (from the weld classifier), and
2. the discontinuity SCF (misalignment / bracket toe / tubular), used to form
   the weld-toe hot-spot stress `sigma_hs = SCF * sigma_nominal`
   (DNV-RP-C203 Sec. 2.3).

The misalignment restraint condition is an **explicit argument**
(`restrained=False` → pinned 3 e/t; `restrained=True` → restrained 6 e/t).

The plate **thickness-effect (size) correction** is deliberately kept OUT of the
SCF — per DNV-RP-C203 it belongs in the S-N (fatigue-strength) layer, not the
stress side.

## Golden values (independently reproduced)

| Quantity | Inputs | Formula | Expected | Source |
|---|---|---|---|---|
| Axial misalignment SCF (pinned) | e=3 mm, t=20 mm | 1 + 3 e/t | **1.45** | DNV-RP-C203 App. D (single-sided / pinned plate) |
| Axial misalignment SCF (restrained) | e=3 mm, t=20 mm | 1 + 6 e/t | **1.90** | DNV-RP-C203 (restrained / cruciform / fixed ends) |
| Hot-spot stress (linear extrap.) | σ(0.4t)=200, σ(1.0t)=150 MPa | 1.67·σ₀.₄ₜ − 0.67·σ₁.₀ₜ | **233.5 MPa** | DNV-RP-C203 Eq. 4.1 |
| Efthymiou chord-saddle SCF (axial T) | β=0.5, γ=12, τ=0.5, θ=90° | γ·τ^1.1·(1.11 − 3(β−0.52)²)·sin θ^1.6 | **6.21** | DNV-RP-C203 App. B / Efthymiou (1988) |
| Hot-spot stress = SCF·nominal | SCF=1.45, σ=100 MPa | SCF·σ | **145.0 MPa** | DNV-RP-C203 Sec. 2.3 |

Efthymiou hand check (full precision):
`12 · 0.5^1.1 · (1.11 − 3·(0.5−0.52)²) · sin(90°)^1.6`
`= 12 · 0.466516 · 1.1088 · 1 = 6.2072` → rounds to **6.21**.

## Reconciliation note — existing library τ^1.0 vs published τ^1.1

The repository's `scf_library.efthymiou_ty_axial` chord-saddle term uses
`tau^1.0`, so for the same geometry (β=0.5, γ=12, τ=0.5, θ=90°) it returns
**6.65** — about **7% higher** than the published **6.21** (`tau^1.1`). This
module's `efthymiou_chord_saddle_axial_scf` reproduces the **published τ^1.1**
form (DNV-RP-C203 App. B / Efthymiou 1988). The discrepancy in the existing
library is documented here and intentionally **not** silently inherited; a
follow-up could correct `scf_library.efthymiou_ty_axial` to `tau^1.1`.

## Simplification level / caveats

- **Misalignment SCF** is the standard membrane-bending amplification (3 e/t
  pinned, 6 e/t restrained); the thickness-step term is available via `t2_mm`
  for the pinned case.
- **Bracket-toe SCF** uses representative soft/hard toe geometry presets
  (soft: flank 20°, r=4 mm; hard: flank 45°, r=1 mm) fed to the validated
  IIW/DNV fillet-toe SCF. Presets are overridable; for detailed design, replace
  with measured toe geometry or an FE hot-spot SCF. The model guarantees
  soft-toe SCF < hard-toe SCF.
- **Efthymiou** here covers the axial chord-saddle hot spot only (the governing
  term for a loaded brace stub); IPB/OPB/brace-side terms remain available in
  `scf_library` (with the τ^1.0 caveat above).
- This is a screening / detail-design-input layer; it does not perform the S-N
  damage summation (that is the `fatigue/` damage layer).
