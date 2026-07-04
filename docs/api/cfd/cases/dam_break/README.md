# Dam break — Martin & Moyce (1952) verification case

Reference OpenFOAM case backing the verification report
[`../../dam-break-verification.html`](../../dam-break-verification.html)
(digitalmodel issue #1165, epic #1161).

This is the suite's **first free-surface (VOF) case** and the keystone multiphase
validation: a water column `a × 2a` (n² = 2) collapses in a plain rectangular tank and the
dimensionless surge front `Z = x/a` vs `T = t·√(2g/a)` is compared to the Martin & Moyce
(1952) experiments — the canonical benchmark gating every marine free-surface workflow
(wave loading, green water, sloshing).

## Result

| Metric | Result |
|--------|--------|
| Surge front Z(T) vs Martin & Moyce, with the standard +0.175 gate-release shift | mean 3.0%, max 8.1% |
| Surge front Z(T), uncorrected | mean 9.8% (early-time offset = finite experimental gate removal) |
| Mass conservation (total `alpha.water` volume drift) | ~1×10⁻⁶ (gate < 1%) |
| Mesh | 288×144×1 uniform (41,472 cells), `checkMesh` OK, non-orthogonality 0 |
| Solver | OpenFOAM ESI v2312 `interFoam` (VOF, laminar) |

A 144×72 mesh (the in-repo test builder default, `DamBreakConfig(cells_per_width=18)`)
reproduces the same deviations (mean 2.9%, max 8.1%) — the comparison is mesh-independent
at these resolutions.

## Geometry / conditions

Column base `a = 0.1461 m`, height `2a` (the tutorial-scale n²=2 configuration); plain tank
`8a × 4a` (no obstacle, unlike the stock `damBreak` tutorial); uniform mesh with `dx = a/36`
so the column edges fall exactly on cell faces (`setFields` initialises the exact column;
initial volume matches `a·2a·depth` to 5 digits). Water/air standard properties, `σ = 0.07`.

## Key modelling decision

The simulation releases the column **instantaneously**; in the experiment the diaphragm
release takes finite time, so the raw CFD front leads the measured one at early `T` (up to
~18% at `T ≈ 1`) and converges to ~5–7% later. The literature-standard correction — applied
identically by the Lethe benchmark — is a **+0.175 dimensionless time shift**, after which
every digitized point agrees within 8.1%. Both raw and corrected comparisons are reported
and gated in the regression test.

## Reproduce

Requires a solver-capable host (run `uv run openfoam doctor`; source the OpenFOAM
environment first, e.g. `source /usr/lib/openfoam/openfoamXXXX/etc/bashrc`).

```bash
# Regenerate this case from the in-repo builder (this archive = cells_per_width=36):
uv run python -c "
from digitalmodel.solvers.openfoam.validation import DamBreakConfig, build_dam_break_case
build_dam_break_case(DamBreakConfig(cells_per_width=36, end_time=0.5, name='dam_break'), '.')"

cd dam_break
blockMesh
setFields                 # initialise the water column (or: openfoam run . --set-fields)
interFoam                 # ~3 min (288x144); ~10 s at the test resolution

# post-process + compare to Martin & Moyce (writes results.json + 4 PNGs):
uv run python analyze_dam_break.py . ./validation_results
```

The `waterVolume` function object (in `system/controlDict`) records the total water volume
every time step for the mass-conservation gate.

## Citations

- Martin, J.C. & Moyce, W.J. (1952). "Part IV. An experimental study of the collapse of
  liquid columns on a rigid horizontal plane." *Phil. Trans. R. Soc. Lond. A*, 244(882),
  312–324. doi:10.1098/rsta.1952.0006.
- Digitized surge-front data + gate-release shift: Lethe project,
  `chaos-polymtl/lethe` `examples/multiphysics/dam-break/dam-break-2d.py`.
- Dictionaries derive from `$FOAM_TUTORIALS/multiphase/interFoam/laminar/damBreak/damBreak`
  (ESI v2312), with the downstream obstacle removed to match the Martin & Moyce tank.
