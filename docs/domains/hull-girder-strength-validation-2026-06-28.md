# Hull-Girder Longitudinal Strength — Validation Record (2026-06-28)

Backing for `src/digitalmodel/naval_architecture/hull_girder_strength.py`
(issue #1082, EPIC #1080 workstream A). Combines the still-water bending moment
(from `loading_computer`) with the IACS UR S11 wave bending moment and checks the
midship section against yield and a simplified hull-girder ultimate capacity.

## What it computes

| Function | Rule | Output |
|---|---|---|
| `wave_coefficient(L)` | IACS UR S11 wave coefficient C | dimensionless |
| `wave_bending_moment(L, B, Cb)` | S11 hog `+0.19·C·L²·B·Cb`, sag `−0.11·C·L²·B·(Cb+0.7)` | kN·m |
| `combined_bending_moment(M_sw, wave)` | still-water + wave per condition | kN·m envelope |
| `section_modulus_check(M, Z, fy)` | yield: σ = M/Z ≤ 175/k | utilisation |
| `hull_girder_ultimate_moment(Z, σ_U)` | simplified M_U = σ_U·Z | kN·m |
| `ultimate_strength_check(M_sw, M_wv, M_U)` | S11A: γ_S·M_sw + γ_W·M_wv ≤ M_U/γ_R | utilisation |

Material factor k: 1.00 (235) / 0.78 (315) / 0.72 (355) / 0.68 (390) / 0.62
(460). S11A partial factors: γ_S = 1.0, γ_W = 1.2, γ_R = 1.1 (overridable).
Units: moments kN·m (matching `loading_computer.max_bending_kn_m`), Z in m³,
stress MPa.

## Validation vs the IACS UR S11 rule formula

Wave coefficient at the rule breakpoints: L=90 → 7.7068, L=200 → 9.75, L=300 →
**10.75** (plateau start), L=350 → **10.75** (plateau end), L=400 → 10.5575.

Worked example — L=200 m, B=32 m, Cb=0.8 (C=9.75, base = C·L²·B = 12,480,000):
- M_wv,hog = 0.19 · 12,480,000 · 0.8 = **+1,896,960 kN·m**
- M_wv,sag = −0.11 · 12,480,000 · 1.5 = **−2,059,200 kN·m**

Yield: M = 2.0×10⁶ kN·m over Z = 50 m³ → σ = 40 MPa; AH36 permissible = 175/0.72
= 243 MPa → utilisation 0.165. All reproduced by the tests.

## Simplification level of the ultimate-strength method (required disclosure)

`hull_girder_ultimate_moment` is a **single-step ("first-collapse")**
estimate: `M_U = σ_U · Z_compression`, where σ_U is the buckling-reduced
ultimate compressive stress of the compression flange and the tension flange is
taken at yield. σ_U is **not** reimplemented here — it is taken from the
validated DNV-RP-C201 stiffened-panel solver via
`compression_flange_ultimate_stress(panel, material)` →
`StiffenedPanelBucklingAnalyzer.check_panel(...).critical_stress`.

This deliberately **omits** what the full IACS UR S11A / CSR **Smith
incremental-iterative** method captures:
- progressive collapse of individual stiffener-plate elements along their
  load-shortening curves;
- migration of the instantaneous neutral axis as the compression side softens;
- post-buckling load redistribution to the still-effective elements.

Consequently this M_U is a **preliminary-design figure**, generally a mild
*over*-estimate of the true ultimate moment; it is not a CSR-compliant capacity.
A full Smith march over a discretised cross-section is the follow-on (needs the
section element list, not yet modelled).

## Builds on (no physics reimplemented)

- `naval_architecture/loading_computer.py` — still-water BM (`max_bending_kn_m`).
- `naval_architecture/compliance.py` — ABS-SVR minimum section-modulus floor
  (`check_section_modulus`), complementary to the stress-based yield check here.
- `structural/structural_analysis/panel_buckling.py` — the compression-flange
  σ_U (DNV-RP-C201, validated).
- `structural/structural_analysis/models.py` — `MARINE_GRADES` (Grade A / AH36 /
  EH40) for yields and the material factor.

## Tests

`tests/naval_architecture/test_hull_girder_strength.py` — 14 tests: the S11 wave
coefficient breakpoints, the wave-BM golden, the combined envelope, material
factor / permissible stress, yield check (pass + overstress), the simplified
ultimate moment + S11A partial-factor check (incl. sagging magnitudes), the
panel-solver integration, and an end-to-end worked example. black + flake8 clean;
runs under the `naval-architecture` CI domain.
