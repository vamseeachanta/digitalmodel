# MacCamy-Fuchs cylinder wave-loading verification case

Reference OpenFOAM case backing the verification report
[`../../maccamy-fuchs-cylinder-verification.html`](../../maccamy-fuchs-cylinder-verification.html)
(digitalmodel issue #1171, epic #1161).

The suite's first true wave-**loading** validation (vs the propagation-only #1170
numerical wave tank and the body-motion #1302 float): a rigid, bottom-mounted,
surface-piercing vertical circular cylinder in a regular wave, with the inline
horizontal force measured by an OpenFOAM `forces` function object and compared to
the **exact MacCamy-Fuchs (1954) linear diffraction force** — and to its small-ka
Morison inertia limit.

## Reference (frozen, pure-Python)

MacCamy, R.C. & Fuchs, R.A. (1954), *Wave Forces on Piles: A Diffraction Theory*,
U.S. Army Corps of Engineers, Beach Erosion Board, Technical Memorandum No. 69.
The total inline force amplitude on a cylinder of radius `a` in a linear wave of
height `H`, wavenumber `k` (from `ω² = g k tanh kh`), depth `h` is

```
F0 = (2 ρ g H / k²) · tanh(kh) · A(ka),   A(ka) = 1 / |H1'(ka)| = 1 / √(J1'(ka)² + Y1'(ka)²)
```

with the force leading the incident crest / free-surface velocity by
`γ = 90° − δ`, `δ = atan2(J1'(ka), Y1'(ka))`. As `ka → 0` the force collapses onto
the Morison inertia limit (`Cm = 2`, added mass `Ca = 1` for a circle) and
`γ → 90°`. Frozen with `scipy.special` derivative Bessel functions in
`digitalmodel.solvers.openfoam.validation.maccamy_fuchs`
(`maccamy_fuchs_force`, `maccamy_fuchs_A`, `morison_inertia_force`).

## Geometry & regime (`CylinderWaveLoadingConfig` defaults)

| Parameter | Value | Note |
|-----------|-------|------|
| Wave height H | 0.030 m | verified #1170 StokesII wave |
| Wave period T | 0.80 s | |
| Depth h | 0.40 m | on a background cell face (`nz = 60` over 0.60 m ⇒ 40 cells) |
| Cylinder diameter D | 0.30 m | |
| Cylinder x | 6.0 m | > 2λ from the inlet, established region |
| Tank | 10.0 × 1.6 (half) × 0.60 m | far y-wall slip, > 5 D from the axis |
| Background mesh | 400 × 64 × 60 | ~40 cells / wavelength |
| Wavenumber k | 6.366 m⁻¹ | λ = 0.987 m |
| **ka** | **0.955** | diffraction (ka > 0.5), D/L = 0.30 |

**Headline diffraction point:** MacCamy-Fuchs F₀ = **14.74 N**, Morison inertia =
**20.55 N** ⇒ MF / Morison = **0.717** (a **28% diffraction reduction**), force
lead over velocity γ = **69.58°**, effective inertia Cm_eff = **1.435**. The
reduction is comfortably outside the 15% force gate, so a Morison prediction
genuinely **fails** the gate — the case discriminates diffraction physics.

## Gate (`test_maccamy_fuchs.py`)

| Quantity | Target | Tolerance |
|----------|--------|-----------|
| Peak inline force | MacCamy-Fuchs F₀ at the **measured** incident H | ±15% (`MACCAMY_FUCHS_FORCE_TOLERANCE`) |
| Force lead over velocity γ | 69.58° | ±15° (`PHASE_TOLERANCE_DEG`) |
| Loading period | wave period T | ±5% (`LOADING_PERIOD_TOLERANCE`) |

Grading against MacCamy-Fuchs re-evaluated at the *measured* incident wave height
(from a Goda–Suzuki gauge split) keeps a small wave-generation error out of the
force verdict. The phase gate checks γ = 90° − δ: matching the diffraction phase
departure δ confirms the CFD resolves the scattered-wave phase, not just the
amplitude.

The frozen closed form + config + provenance always run (no OpenFOAM needed); the
end-to-end solve is opt-in on a solver-capable host with
`DIGITALMODEL_RUN_LONG_CFD=1`.

## Reproduce

```bash
# 1. build the case from the in-repo builder (solver host, OpenFOAM ESI v2312):
uv run python -c "
from digitalmodel.solvers.openfoam.validation.maccamy_fuchs import (
    CylinderWaveLoadingConfig, build_maccamy_fuchs_case)
build_maccamy_fuchs_case(CylinderWaveLoadingConfig(), '.')"

# 2. mesh + solve (interFoam + forces FO; keep the cylinder clear of slab cuts):
cd validation_maccamy_fuchs
blockMesh && snappyHexMesh -overwrite && setFields
decomposePar && mpirun -np 10 interFoam -parallel && reconstructPar -newTimes

# 3. analyze the solved case → results.json (incident split + force fit + gate):
cd ../..
uv run python docs/api/cfd/cases/maccamy_fuchs/analyze_maccamy_fuchs.py \
    validation_maccamy_fuchs ./results

# 4. render the interactive report from results.json:
uv run python docs/api/cfd/report_build/build_maccamy_fuchs.py \
    ./results/results.json .
# → docs/api/cfd/maccamy-fuchs-cylinder-verification.html
```

The long solve is opt-in: `DIGITALMODEL_RUN_LONG_CFD=1` on a solver host drives
the regression test `tests/solvers/openfoam/validation/test_maccamy_fuchs.py`
end-to-end (`build_maccamy_fuchs_case` → solve → `analyze_cylinder_loading` →
assert within the force + phase gates).

## Report

`build_maccamy_fuchs.py` reads `results.json` (schema in the driver docstring) and
emits `../../maccamy-fuchs-cylinder-verification.html` with:

- **Inline force F_x(t)** over the settled window, its single-harmonic fit, and the
  MacCamy-Fuchs amplitude band at the measured wave height.
- **CFD vs MacCamy-Fuchs vs Morison** peak force — showing the ~28% diffraction
  reduction a Morison estimate misses.
- **ka-sweep** (MF/Morison and Cm_eff vs ka = πD/L, from the frozen closed form)
  documenting the Morison → diffraction transition, with the headline case marked.
- **Known-answer gate table** (force ±15%, phase lead vs 69.58°, periodicity) and an
  honest verdict badge.

All reference curves are recomputed live from the frozen closed form at build time;
the only measured inputs are the CFD fields in `results.json`.

## Citations

- MacCamy, R.C. & Fuchs, R.A. (1954). *Wave Forces on Piles: A Diffraction Theory*,
  U.S. Army Corps of Engineers, Beach Erosion Board, TM-69.
- Morison, J.R. et al. (1950). The force exerted by surface waves on piles.
  *Petroleum Transactions AIME* 189 — the inertia + drag small-body limit.
- Sarpkaya, T. & Isaacson, M. (1981); Chakrabarti, S.K. (1987); Faltinsen, O.M.
  (1990) — diffraction regime and inertia coefficient.
- `$FOAM_TUTORIALS/multiphase/interFoam/laminar/waves/stokesII` (ESI v2312) — wave
  generation/absorption, verified in #1170.
- Goda, Y. & Suzuki, Y. (1976) — incident/reflected wave resolution from gauge arrays.
