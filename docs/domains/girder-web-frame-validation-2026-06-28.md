# Girder & Web-Frame Buckling — Validation Record (2026-06-28)

Backing for
`src/digitalmodel/structural/structural_analysis/girder_web_frame.py`
(issue #1083, EPIC #1080 workstream A). Primary supporting members — girders and
transverse web frames — built on the validated plate / stiffened-panel solvers
(no buckling physics reimplemented).

## Web-panel shear buckling (transverse frames)

`tau_cr = JO(k_tau · π²E / (12(1−ν²)) · (t_w/d)²)`, with the classical shear
coefficient:
- `a/d ≥ 1` (long panel): `k_tau = 5.34 + 4·(d/a)²`
- `a/d < 1` (short panel): `k_tau = 4.0 + 5.34·(d/a)²`

and **the same Johnson-Ostenfeld inelastic knockdown** as the plate solver
(`PlateBucklingAnalyzer.johnson_ostenfeld`, reused directly).

Validation:
- k_tau: a/d=2 → **6.34**; a/d=0.5 → **25.36**; square → **9.34**.
- Web 1000×12 mm, panel 2000 mm, AH36 → `tau_e` < 0.5·f_y → `tau_cr = tau_e`
  (elastic, no knockdown).
- Web 1000×**25** mm → `tau_e` > 0.5·f_y → `tau_cr < tau_e` (Johnson-Ostenfeld
  applied) and `< f_y`.

## Girder tripping — bracket-spacing effect

Reuses the validated DNV-RP-C201 Sec. 7.5.2 tripping solver
(`StiffenedPanelBucklingAnalyzer.torsional_buckling`, validated against the
0119-015 worked example: fET=441.66, λ_T=0.729, fT=215.61 MPa) with the
**tripping-bracket spacing** set as the torsional restraint length `L_T`.

Validation: for a tee girder, the tripping capacity `fT` at `L_T = 800 mm` is
**greater** than at `L_T = 2400 mm` — i.e. closer brackets raise the capacity, as
expected.

## Builds on (no physics reimplemented)

- `structural_analysis/buckling.py` — `PlateBucklingAnalyzer.johnson_ostenfeld`
  (the inelastic shear knockdown); the elastic shear form mirrors the in-package
  `k_tau·π²E/(12(1−ν²))·(t/b)²` used in `check_plate_buckling`.
- `structural_analysis/panel_buckling.py` — the DNV-RP-C201 tripping solver and
  `StiffenedPanelGeometry` (`torsional_restraint_spacing`).
- `structural_analysis/models.py` — `MaterialProperties` / `MARINE_GRADES`.

Complements the stiffened-panel buckling (single longitudinal stiffener) and the
hull-girder longitudinal strength (#1082), where girders and web frames are the
primary supporting members.

## Tests

`tests/structural/structural_analysis/test_girder_web_frame.py` — 7 tests: the
k_tau branches, the web shear stress (elastic + inelastic-knockdown cases), the
web-frame shear check (pass + overload), the bracket-spacing effect on tripping,
the girder-tripping check, and input validation. black + flake8 clean; runs under
the `structural` CI domain.

## Deferred

Web-frame combined shear+bending interaction and the full grillage (orthogonally
stiffened, longitudinal × transverse) buckling are #1081.
