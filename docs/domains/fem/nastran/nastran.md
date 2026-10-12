## History

<https://forum.freecad.org/viewtopic.php?f=18&t=12753>

<https://en.wikipedia.org/wiki/Nastran>

## Key References

<https://github.com/nasa/NASTRAN-95>

<https://opensource.gsfc.nasa.gov/projects/mystran/index.php>
<https://github.com/dr-bill-c/MYSTRAN>
<https://www.mystran.com/>
<https://github.com/MYSTRANsolver/MYSTRAN>

## pyNastran

pyNastran is an interface library to the various Nastran file formats (BDF, OP2, OP4).

What is pyNastran?
<https://github.com/SteveDoyle2/pyNastran>

<https://www.mystran.com/forums/showthread.php?tid=53>

## FreeCAD vs. Nastran

The solver debate
<https://forum.freecad.org/viewtopic.php?f=9&t=48695&sid=3156dd12a4c274de232923d726d1e5b0>
<https://forum.freecad.org/viewtopic.php?f=18&t=39234>

## OpenFOAM vs. FreeCAD vs. Nastran

<https://www.openfoam.com/documentation/tutorial-guide/5-stress-analysis/5.1-stress-analysis-of-a-plate-with-a-hole>

## MYSTRAN in digitalmodel (2026-09-24)

MYSTRAN (MIT, [MYSTRANsolver/MYSTRAN](https://github.com/MYSTRANsolver/MYSTRAN))
is the open-source Nastran-format solver adopted for linear static, modal and
buckling work. NASA's NASTRAN-95 (NOSA 1.3, frozen, Fortran 77) was evaluated
and rejected. Full evaluation: `workspace-hub/docs/research/mystran-eval.md`.

- Solver chain: `src/digitalmodel/solvers/mystran/` (`BDFWriter`,
  `MystranResultParser`, `MystranChain`, `MeshConvergenceStudy`), mirroring
  the CalculiX chain in `solvers/calculix/`.
- Tests: `tests/solvers/mystran/` — writer/parser/convergence tests run
  anywhere; integration tests (CBAR cantilever vs `PL^3/3EI`, CHEXA8
  mesh-convergence sweep) skip unless `mystran` is on PATH or `MYSTRAN_EXE`
  is set.
- OP2 output is readable with pyNastran: `uv sync --extra nastran`.
- Windows: prebuilt `mystran-19.0.0-windows-x86_64.exe` from GitHub Releases.
  Linux: `workspace-hub/scripts/setup/mystran-build-linux.sh` (gfortran + CMake).
- Gotchas found: MYSTRAN exits 0 on FATAL errors (check stdout/F06);
  `PSOLID` needs the IN field for HEXA8; output extension is upper-case `.F06`.
