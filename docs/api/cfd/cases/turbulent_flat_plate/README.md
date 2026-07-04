# Turbulent flat-plate — k-ω SST verification case

Reference OpenFOAM case backing the verification report
[`../../turbulent-flat-plate-verification.html`](../../turbulent-flat-plate-verification.html)
(digitalmodel issue #1168, epic #1161).

This is a **known-answer turbulence verification**: a zero-pressure-gradient turbulent boundary
layer whose near-wall velocity profile obeys the **universal law of the wall** and whose skin
friction matches the classical Schlichting/Prandtl turbulent correlations. It is the **first
turbulent case** in the suite (the prior two — flat plate Blasius and cylinder Re = 100 — are
laminar).

## Result

| Metric | Result |
|--------|--------|
| Velocity profile vs law of the wall | captured: viscous sublayer `u+ = y+`, log law `u+ = (1/0.41) ln y+ + 5.0`, wake |
| Skin friction `Cf(Re_x)` vs turbulent correlations | mean ~9%, ≤9.9% max (developed region `Re_x = 5e5–1e6`) |
| `Cf` level | ~0.0034, ≈ 5× the laminar Blasius value (unambiguously turbulent) |
| Near-wall resolution | `y+ = 0.12–0.31` (wall-resolved, no wall functions) |
| Mesh | two-block, 50,400 cells |
| Solver | OpenFOAM ESI v2312 `simpleFoam` + `kOmegaSST` (RANS, steady) |

## Conditions

`U∞ = 1 m/s`, `ν = 1e-6 m²/s`, plate length `L = 1 m` → `Re_L = 1e6` (turbulent).
Two-block geometry: 0.2 m shear-free (`symmetryPlane`) run-in ahead of a 1.0 m no-slip plate,
domain height `H = 0.3 m`.

## Key modelling decision

The case is **wall-resolved** (`y+ ≈ 0.13`, into the viscous sublayer) with a **low-Re wall
treatment — no wall functions bridging the near-wall gap**. This is the stronger verification: the
k-ω SST profile collapses onto the universal law of the wall (sublayer + log law + wake) directly,
rather than relying on a wall-function boundary condition to impose the log law. The residual ~9%
`Cf` offset versus the empirical correlations sits within their own 5–10% mutual scatter and the
resolved near-leading-edge transition; the **law-of-the-wall collapse is the primary check**, the
`Cf` correlation match a secondary one.

## Reproduce

Requires a solver-capable host (run `uv run openfoam doctor`; source the OpenFOAM environment
first, e.g. `source /usr/lib/openfoam/openfoamXXXX/etc/bashrc`). Note: `kOmegaSST` needs
`wallDist { method meshWave; }` in `system/fvSchemes` (already present).

```bash
cd <a writable copy of this case>
blockMesh
simpleFoam                 # steady RANS; ~converges by ~1200 iterations

# post-process + compare to law of the wall + turbulent Cf correlations
# (writes results.json + PNGs). Run from the digitalmodel repo dir so numpy/pyvista resolve:
uv run python analyze_tflat.py . ./tflat_results
```

The `wallShearStress` and `yPlus` function objects (in `system/controlDict`) write the wall
traction and near-wall spacing; the analysis script builds `u+ (y+)` profiles and compares `Cf(Re_x)`
against the Prandtl/Schlichting turbulent correlations.
