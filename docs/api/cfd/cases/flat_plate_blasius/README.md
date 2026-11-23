# Laminar flat-plate — Blasius verification case

Reference OpenFOAM case backing the verification report
[`../../flat-plate-blasius-verification.html`](../../flat-plate-blasius-verification.html)
(digitalmodel issue #1192, epic #1161).

This is a **known-answer verification**: a sharp-leading-edge laminar boundary layer whose
skin friction and velocity profiles have an exact analytical answer (Blasius, 1908).

## Result

| Metric | Result |
|--------|--------|
| Skin friction `Cf(Re_x)` vs `0.664/√Re_x` | mean 4.6%, max 5.2% (developed region) |
| Velocity profile collapse onto `f'(η)` | RMS(u/U∞) = 0.025 over 0 ≤ η ≤ 6 |
| Mesh | 28,600 cells, `checkMesh` OK |
| Solver | OpenFOAM ESI v2312 `simpleFoam`, laminar |

## Conditions

`U∞ = 1 m/s`, `ν = 1e-5 m²/s`, plate length `L = 0.1 m` → `Re_L = 1e4` (laminar; `Re_x < 5e5`).

## Key modelling decision

A naive setup (plate starting at the velocity inlet) over-predicts `Cf` by ~30% because the
sharp no-slip leading edge sits in the inlet, creating a spurious singularity and a too-thin
boundary layer. The fix — and the dominant error source found in the verification — is a **20 mm
shear-free (`symmetryPlane`) run-in** ahead of the plate, so the stream arrives uniformly and the
boundary layer develops from a true `x = 0` origin. See the report's solution-verification table.

## Reproduce

Requires a solver-capable host (run `uv run openfoam doctor`; source the OpenFOAM environment
first, e.g. `source /usr/lib/openfoam/openfoamXXXX/etc/bashrc`).

```bash
cd <a writable copy of this case>
blockMesh
simpleFoam                 # ~30 s; velocity residual → ~4e-6 (steady)

# post-process + compare to Blasius (writes results.json + 2 PNGs):
uv run python analyze_flat_plate.py . ./validation_results
```

The `wallShearStress` function object (in `system/controlDict`) writes the wall traction used
for `Cf`; the analysis script reconstructs the Blasius solution by shooting the Blasius ODE.
