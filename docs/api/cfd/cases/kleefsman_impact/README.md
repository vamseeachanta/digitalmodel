# Green-water impact — Kleefsman/MARIN verification case

Reference OpenFOAM case backing the verification report
[`../../kleefsman-impact-verification.html`](../../kleefsman-impact-verification.html)
(digitalmodel issue #1172, epic #1161).

The canonical **violent free-surface impact** benchmark: MARIN's dam-break-with-obstacle
experiment (a scale model of green water striking a deck container). A 1.228×1×0.55 m
water reservoir collapses down a 3.22×1×1 m tank onto an instrumented box (P1–P4 on the
impact face, P5–P8 on top; H1–H4 water-height gauges). **Geometry and experimental traces
are the official SPHERIC/ERCOFTAC Test 2 distribution** (`kleefsman_exp_data.csv` in this
directory) — nothing digitized from figures.

## Result (161×50×50, 6 s — half the Kleefsman reference grid)

| Metric | Result | Gate |
|--------|--------|------|
| P2 peak impact pressure | within **20%** band | ≤ 20% |
| P2 impact arrival (25%-of-peak crossing) | within a few % | ≤ 20% |
| H2 / H4 water heights (normalized MAE, 0–6 s) | **~2–4%** | ≤ 10% |
| Water volume drift through impact | < 1% | < 1% |

Fast regression variant (81×25×25, 1.5 s): P2 peak −15.5%, arrival +2.4%, H2 3.0%,
H4 1.8% — used by the solve-gated test (opt-in via `DIGITALMODEL_RUN_LONG_CFD=1`;
~25 min serial).

## Key modelling points
- **Probe placement is density-aware**: the builder offsets each face sensor by half a
  cell into the fluid — a fixed offset valid at one resolution lands *inside* the carved
  obstacle at another (found the hard way; regression-tested).
- Impact peaks are single-cell, single-millisecond events: P1's initial spike is under-
  predicted at any practical resolution (the original 1.2M-cell Kleefsman VOF smoothed it
  too), which is why impact validation uses a 20% band on P2.

## Reproduce

```bash
uv run python -c "
from digitalmodel.solvers.openfoam.validation import KleefsmanConfig, build_kleefsman_case
build_kleefsman_case(KleefsmanConfig(nx=161, ny=50, nz=50, end_time=6.0, name='kleefsman'), '.')"
cd kleefsman
blockMesh && topoSet && subsetMesh -overwrite c0 -patch obstacle && setFields
decomposePar && mpirun -np 16 interFoam -parallel   # ~2.5 h; serial not advised
# metrics vs the official experiment:
uv run python -c "
from digitalmodel.solvers.openfoam.validation import extract_impact_metrics
import json; print(json.dumps(extract_impact_metrics('.'), indent=2))"
```

## Citations
- Kleefsman et al. (2005), *J. Comput. Phys.* 206(1) 363–393, doi:10.1016/j.jcp.2004.12.007.
- SPHERIC/ERCOFTAC Test-case 2 (Issa & Violeau, EDF-LNHE 2006) — official geometry + data,
  https://www.spheric-sph.org/tests/test-02.
