# PIMPLE loop anatomy and per-cell cost (OpenFOAM v2312, interFoam)

Source-verified notes for anyone tuning `fvSolution` on the free-surface resistance chain
(`scripts/cfd/`). Every fact below was read in the installed openfoam.com v2312 source
(`/usr/lib/openfoam/openfoam2312`) on 2026-09-05; line numbers refer to that tree.
Community documentation (openfoamwiki.net PIMPLE/PISO/SIMPLE guide pages) is reconciled
with these facts in the analysis wiki; where the two disagree the source wins.

## Loop nesting (`applications/solvers/multiphase/interFoam/interFoam.C`)

```
while (pimple.loop())                 // nOuterCorrectors                      (line 110)
    alphaControls / alphaEqnSubCycle  // MULES alpha transport, INSIDE the outer loop (151-152)
    UEqn                              // momentum predictor                    (161)
    while (pimple.correct())          // nCorrectors                           (164)
        pEqn                          // (nNonOrthogonalCorrectors + 1) p_rgh solves
    if (pimple.turbCorr()) turbulence->correct()   // final outer iteration only by default (169)
```

- p_rgh solves per pseudo-step = nOuter x nCorr x (nNonOrth + 1): 2/3/2 -> 18, 1/2/0 -> 2.
- The alpha (MULES) transport repeats on every outer iteration.
- Measured on the chain: 2/3/2 with limited schemes costs ~3.4x more per cell-iteration
  than 1/2/0 on the same host (8.05 vs 2.3 us). Never compare us/cell-iteration across
  different PIMPLE settings.

## Keyword defaults (`src/finiteVolume/cfdTools/general/solutionControl/`)

| Keyword | Default | Read at |
|---|---|---|
| nOuterCorrectors | 1 | pimpleControl.C:48 |
| nCorrectors | 1 | pimpleControl.C:49 |
| nNonOrthogonalCorrectors | 0 | solutionControl.C:47 |
| momentumPredictor | true | solutionControl.C:49 |
| turbOnFinalIterOnly | true | pimpleControl.C:51-52 |
| finalOnLastPimpleIterOnly | false | pimpleControl.C:54 |
| ddtCorr | true | pimpleControl.C:55 |
| solveFlow | true | pimpleControl.C:47 |
| maxCo / maxAlphaCo (localEuler) | 0.9 / 0.2 | applications/solvers/multiphase/VoF/setRDeltaT.H:6-13 |
| rDeltaTSmoothingCoeff / nAlphaSpreadIter | 0.1 / 1 | setRDeltaT.H:16-23 |

## residualControl cannot shorten a two-iteration outer loop

`pimpleControl.C:61-64`: `criteriaSatisfied()` returns false when `corr_ == 1`, when no
residualControl is set, or on the final iteration; `pimpleControl.C:227` exits only on
`converged_ || criteriaSatisfied()`. So the earliest early exit is after outer iteration 2
of a loop with `nOuterCorrectors >= 3`. With `nOuterCorrectors 2` a residualControl entry
changes nothing but the log line `PIMPLE: not converged within 2 iterations`
(pimpleControl.C:213-218). The outer-loop cost lever is `nOuterCorrectors` itself.

## Final-iteration settings

`fvMatrix.C:1546` selects the solver dictionary with `psi.select(isFinalIteration())`, and
`GeometricField.C:1167/1187` appends `Final` to the field name; relaxation uses the same
switch (`fvMatrix.C:1260`). On the last outer iteration the `p_rghFinal`, `UFinal`,
`"(U|k|omega)Final"` solver entries and the `.*Final` relaxation factors apply. With
`nOuterCorrectors 1` every iteration is final, so only the `*Final` entries are ever used.

## How to check what a case really runs

`grep -m1 -E 'PIMPLE: (Operating solver in PISO mode|Calculations will employ)' log.interFoam`
prints the mode at start-up (pimpleControl.C:167-177); `foamDictionary -entry PIMPLE
system/fvSolution` prints the effective dictionary.
