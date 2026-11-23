# Floating-body heave free decay — verification case

Reference OpenFOAM case backing the verification report
[`../../floating-body-decay-verification.html`](../../floating-body-decay-verification.html)
(digitalmodel issue #1169, epic #1161).

First **moving-body** case of the suite: a half-density cuboid (0.3×0.2×0.5 m, 15 kg)
released ~0.054 m below its hydrostatic equilibrium heaves freely under
`sixDoFRigidBodyMotion` (heave-only constraints) on an interFoam VOF free surface with
mesh morphing.

## Result (14 s)

| Metric | Result | Gate |
|--------|--------|------|
| Equilibrium draft vs Archimedes (0.2505 m) | **+1.1%** | ≤ 5% |
| Decay envelope | **11 cycles**, monotone, δ = 0.34/cycle | damped |
| Heave period vs hydrostatic 0.973 s | 1.149 s, ratio **1.18** | ∈ [1.0, 1.6] |
| Implied added-mass coefficient | **Ca ≈ 0.39** | ∈ [0.1, 1.5] (physical) |

The period excess over the waterplane-stiffness value **is** the added mass — a solver
that missed radiation/added-mass physics would land on the hydrostatic period. The fast
regression variant (3.5 s, ~3 min) reproduces the period to 0.3% and draft to 1.3%.

## Reproduce

```bash
uv run python -c "
from digitalmodel.solvers.openfoam.validation import FloatingBodyConfig, build_floating_body_case
build_floating_body_case(FloatingBodyConfig(end_time=14.0, name='floating_body_decay'), '.')"
cd floating_body_decay
blockMesh && topoSet && subsetMesh -overwrite c0 -patch floatingObject && setFields && interFoam
# or via the runner: OpenFOAMRunConfig(run_topo_set=True, subset_mesh_set='c0',
#                    subset_mesh_patch='floatingObject', run_set_fields=True)
uv run python analyze_floating_body.py . ./validation_results
```

## Citations
- `$FOAM_TUTORIALS/multiphase/interFoam/RAS/floatingObject` (ESI v2312), base case; motion
  solver switched to sixDoFRigidBodyMotion + heave-only constraints, water lump removed.
- Newman (1977), *Marine Hydrodynamics* — hydrostatics, natural periods, added mass.
