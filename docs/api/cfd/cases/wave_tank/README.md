# Numerical wave tank — StokesII verification case

Reference OpenFOAM case backing the verification report
[`../../wave-tank-verification.html`](../../wave-tank-verification.html)
(digitalmodel issue #1170, epic #1161).

The suite's **wave-generation gate**: a StokesII regular wave (H = 0.05 m, T = 3.0 s,
d = 0.4 m, λ = 5.77 m) is generated with active absorption, propagates over a 20 m
(3.5λ) 2D tank, and exits through a `shallowWaterAbsorption` outlet. Ten
`interfaceHeight` gauges record the surface elevation.

## Result (500×55, 30 s)

| Metric | Result | Gate |
|--------|--------|------|
| Wavenumber vs dispersion ω² = gk·tanh(kd) | **+0.2%** | ≤ 5% |
| Wave height, established region (x = 8–18 m) | max err **4.7%** | ≤ 5% |
| Height decay over 1.7λ | **4.6%** | < 5% |
| Reflection coefficient (7-gauge split) | **Kr = 0.010** | < 0.10 |

The x = 2 m gauge (+8%) sits in the wavemaker near-field (evanescent modes) and is
excluded per standard NWT practice (reported, not hidden). The fast regression variant
(250×28, 16 s, ~2.5 min) reproduces: H 2.8%, k +0.7%, Kr 0.004.

## Reproduce

```bash
# regenerate from the in-repo builder (this archive = reference mesh):
uv run python -c "
from digitalmodel.solvers.openfoam.validation import WaveTankConfig, build_wave_tank_case
build_wave_tank_case(WaveTankConfig(nx=500, nz=55, end_time=30.0, name='wave_tank'), '.')"
cd wave_tank
blockMesh && setFields && interFoam    # ~11 min serial; or decomposePar + mpirun
# or: openfoam run . --set-fields
uv run python analyze_wave_tank.py . ./validation_results
```

## Citations
- `$FOAM_TUTORIALS/multiphase/interFoam/laminar/waves/stokesII` (ESI v2312), base case.
- Larsen, Fuhrman & Roenby (2019), arXiv:1804.01158 — interFoam progressive-wave performance.
- Goda & Suzuki (1976) — incident/reflected split from gauge arrays.
