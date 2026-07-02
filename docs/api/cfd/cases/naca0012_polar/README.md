# NACA0012 airfoil — lift-curve verification case

Reference OpenFOAM case backing the verification report
[`../../naca0012-airfoil-verification.html`](../../naca0012-airfoil-verification.html)
(digitalmodel issue #1174, epic #1161).

This is a **known-answer lift verification**: an angle-of-attack polar of the NACA0012 airfoil whose
**lift-curve slope** is compared against the experimental NACA0012 value and the ideal thin-airfoil
theory. It is the **first lift-generating case** in the suite (the prior three — flat plate Blasius,
cylinder Re = 100, turbulent flat plate — measure friction, drag or boundary-layer profiles, not lift).

## Result

| Metric | Result |
|--------|--------|
| Lift-curve slope | **0.1037 /deg** |
| vs experiment (NACA0012, ~0.106/deg) | **&minus;2.2%** |
| vs thin-airfoil theory 2&pi;/rad (0.1097/deg) | **&minus;5.5%** |
| Lift coefficient C<sub>l</sub> over 0&ndash;8&deg; | rises linearly **0.14 &rarr; 0.92** |
| Drag polar C<sub>d</sub> | physical, 0.010 &rarr; 0.031 |
| Zero-lift angle (mesh built-in incidence) | &minus;1.41&deg; |
| Reynolds number | Re &asymp; 9.1&times;10<sup>7</sup> |
| Mesh | OpenFOAM `airFoil2D` C-grid, 10,720 cells, far field ~7 chords |
| Solver | `simpleFoam` + Spalart-Allmaras (RANS, steady) |

Falling slightly below the ideal 2&pi;/rad is the **physically correct thickness/viscous deficit** for
a real airfoil — the verification target is the slope, not the absolute intercept.

## Key points

- **The polar sweeps AoA by rotating the freestream**, not by re-meshing: for each angle the
  inlet/outlet `freestreamVelocity` is rotated, and the `forceCoeffs` lift/drag directions are
  rotated to **match wind axes** (drag along the freestream, lift normal to it).
- **The reference area uses the measured chord.** The tutorial airfoil is **not** unit-chord — its
  chord is ~35 (measured from the `walls` patch) — so `Aref` = chord &times; span
  (`1.7525 = 35.05 &times; 0.05` in `run_polar.sh`). Using a unit chord would mis-scale every
  coefficient.
- **The mesh has a built-in geometric incidence**, which appears as a **&minus;1.4&deg; zero-lift
  angle**. This offset does **not** affect the verification: the **slope is incidence-independent**,
  so a fit through the linear range recovers dC<sub>l</sub>/d&alpha; regardless of the offset.
- **Steady RANS is used only in the linear range (&le;8&deg;).** It does not converge near stall, so
  the polar is deliberately kept below stall onset; the slope fit uses &le;6&deg;.

## Reproduce

The mesh and base fields are **not committed** — they come from the OpenFOAM `airFoil2D` tutorial.
Requires a solver-capable host (check with `uv run openfoam doctor`; source the OpenFOAM environment
first, e.g. `source /usr/lib/openfoam/openfoamXXXX/etc/bashrc`).

```bash
cp -r $FOAM_TUTORIALS/incompressible/simpleFoam/airFoil2D naca && cd naca
cp -r constant/polyMesh.orig constant/polyMesh && cp -r 0.orig 0
cp <this dir>/{run_polar.sh,analyze_naca.py,render_naca.py,system/controlDict} .  # controlDict into system/
bash run_polar.sh                 # sweeps alpha 0..8, writes polar.csv (~a few min)
uv run python analyze_naca.py .   # lift-curve slope + plots (run from the digitalmodel repo dir)
```

`run_polar.sh` writes the rotated `0/U` and `system/forceCoeffs` per angle and appends
`alpha,Cl,Cd` to `polar.csv`; note the `Aref` in it (`1.7525 = chord 35.05 &times; span 0.05`).
`analyze_naca.py` fits the lift-curve slope over the linear range, compares it against the
experimental and thin-airfoil values, and writes `results.json` plus the lift-curve and drag-polar
plots.
