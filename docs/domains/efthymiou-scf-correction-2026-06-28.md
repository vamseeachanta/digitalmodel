<!-- ABOUTME: Validation record for the Efthymiou T/Y axial SCF exponent fix in scf_library.py. -->
<!-- ABOUTME: Documents wrong-vs-correct equations, the published source, and every test value that changed. -->

# Efthymiou T/Y Axial SCF Correction (2026-06-28)

## Summary

`src/digitalmodel/fatigue/scf_library.py::efthymiou_ty_axial` implemented the
Efthymiou T/Y tubular-joint stress-concentration-factor (SCF) equations for
brace **axial** load with several wrong exponents/terms versus the published
set. All four terms (chord saddle, chord crown, brace saddle, brace crown) were
corrected to the published forms.

Surfaced during #1086 (connection & bracket SCF, `structural_analysis/connection_scf.py`),
whose `efthymiou_chord_saddle_axial_scf` already uses the correct published
`tau^1.1` chord-saddle term (6.21 for beta=0.5, gamma=12, tau=0.5, theta=90).
This change makes `scf_library` consistent with it.

This is a **behavior-changing** fix: chord SCFs were previously inflated (the
chord crown term in particular was grossly over-stated by using `gamma^1.0`
instead of `gamma^0.2`).

## Source of truth

- Efthymiou, M. (1988), "Development of SCF formulae and generalised influence
  functions for use in fatigue analysis", OTC 4829.
- DNV-RP-C203, Appendix B (tubular joints), T/Y joint axial-load set.

Non-dimensional geometry: `beta = d/D`, `gamma = D/(2T)`, `tau = t/T`,
`alpha = 2L/D` (defaults to `alpha = 5` when chord length `L` is unset, via
`L = 2.5*D`), `theta` = brace-to-chord angle.

## Wrong vs. correct equations

| Term | Previous (WRONG) | Corrected (published) |
|------|------------------|------------------------|
| Chord saddle | `gamma * tau^1.0 * (1.11 - 3(beta-0.52)^2) * sin(theta)^1.6` | `gamma * tau^1.1 * (1.11 - 3(beta-0.52)^2) * sin(theta)^1.6` |
| Chord crown | `gamma^1.0 * tau * (2.65 + 5(beta-0.65)^2) + tau*beta*(0.25*alpha-3)*sin(theta)^0.2` | `gamma^0.2 * tau * (2.65 + 5(beta-0.65)^2) + tau*beta*(0.25*alpha-3)*sin(theta)` |
| Brace saddle | `1.3 + gamma*tau*0.187*beta*(1.25*beta^0.5 - beta^2.5)*sin(theta)^1.1` | `1.3 + gamma*tau^0.52*alpha^0.1*(0.187 - 1.25*beta^1.1*(beta-0.96))*sin(theta)^(2.7 - 0.01*alpha)` |
| Brace crown | `3 + gamma*tau*0.12*exp(-4*beta)*(0.011*beta^2 - 0.045) + beta*tau*sin(theta)^0.2` | `3 + gamma^1.2*(0.12*exp(-4*beta) + 0.011*beta^2 - 0.045) + beta*tau*(0.1*alpha - 1.2)` |

Key defects:
- **Chord saddle**: `tau` exponent was `1.0`, published is `1.1` (≈7% low on tau,
  but combined with the chord-crown error the *governing* chord SCF was far off).
- **Chord crown**: `gamma` exponent was `1.0`, published is `0.2` — this hugely
  over-stated the term (it was the governing chord SCF in the old code), and the
  trailing `sin(theta)` was raised to `0.2` instead of `1.0`.
- **Brace saddle**: the bracket polynomial, the `alpha^0.1` factor, the `tau^0.52`
  exponent, and the `sin(theta)^(2.7-0.01*alpha)` angle exponent were all wrong.
- **Brace crown**: the `gamma^1.2` factor was missing (was `gamma^1.0`), the
  `exp` and polynomial terms were multiplied instead of added, and the final
  `beta*tau*(0.1*alpha-1.2)` term was replaced by `beta*tau*sin(theta)^0.2`.

Scope note: short-chord correction factors (F1/F2/F3 for `alpha < 12`) are **not**
applied — consistent with `connection_scf.efthymiou_chord_saddle_axial_scf` and
with the 6.21 golden value, which is the uncorrected published form.

## Computed value changes (round to 3 dp)

Returned `SCFResult.scf_chord` / `scf_brace` for the test geometries:

| Geometry (D,T,d,t,theta) | beta,gamma,tau,alpha | chord old -> new | brace old -> new |
|--------------------------|----------------------|------------------|------------------|
| 1000,50,500,25,90 (typical) | 0.5,10,0.5,5 | 13.375 -> 5.173 | 3.247 -> 5.029 |
| 1000,50,500,25,45 | 0.5,10,0.5,5 | 13.404 -> 2.971 | 3.230 -> 2.788 |
| 1000,50,300,15,90 (low beta) | 0.3,10,0.3,5 | 9.630 -> 2.566 | 3.085 -> 3.853 |
| 1000,50,800,40,90 (high beta) | 0.8,10,0.8,5 | 20.980 -> 6.844 | 3.639 -> 4.892 |
| 1200,50,600,25,90 (golden g=12) | 0.5,12,0.5,5 | 16.137 -> 6.207 | 3.246 -> 5.775 |

The golden chord-saddle case (beta=0.5, gamma=12, tau=0.5, theta=90) now returns
**6.21** (6.207 to 3 dp), matching `connection_scf.efthymiou_chord_saddle_axial_scf`
and the published Efthymiou/DNV-RP-C203 value.

## Test changes

The pre-existing `efthymiou_ty_axial` assertions were all **inequality/relational**
(`>= 1.0`, governing-is-max, `scf_90 != scf_45`, `scf_low != scf_high`) — none
encoded an exact numeric SCF, so none asserted a wrong value, and all still hold
under the corrected equations. No existing assertion required editing.

Added golden tests (`TestEfthymiouTYAxialGolden`):
1. `test_chord_saddle_golden_g12` — chord saddle = 6.21 (beta=0.5, gamma=12, tau=0.5, theta=90).
2. `test_typical_axial_golden_values` — chord 5.173, brace 5.029, governing 5.173 (typical T-joint).
3. `test_chord_saddle_theta45_golden` — chord saddle 2.971 (theta=45, sin^1.6 scaling).
4. `test_chord_saddle_matches_connection_scf` — equals the published `connection_scf` form.

## Functions reviewed but NOT changed

- `efthymiou_ty_ipb`, `efthymiou_ty_opb` — out of scope for this defect (brace
  axial only). Left unchanged; not re-validated here.
- `efthymiou_k_axial` — documented as a *simplified* K-joint formulation; not a
  clear line-by-line transcription of the published parametric set, so it is not
  unambiguously "wrong" and is left as-is. No KT function exists in this module.
