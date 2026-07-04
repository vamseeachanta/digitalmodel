# Demo 10: CAD Geometry-Verification — Plate-Buckling Capacity + Render

**Audience: CAD / engineering-design firms.** The value proposition is
*geometry-for-analysis*: the geometry you already model in CAD can be pushed
straight through a structural verification and come back as a render + a
pass/fail report — no hand re-modelling for analysis.

## The story: geometry → check → report

1. **Geometry.** A parametric set of rectangular plate panels (the midsurface
   panels a CAD package exports as STEP/IGES) — four footprints, four gauges.
2. **Check.** Each panel is verified for **elastic plate buckling** plus a
   cheap **geometry-hygiene** screen (watertight footprint / aspect ratio /
   plate slenderness) for analysis readiness.
3. **Report.** A branded, self-contained HTML report with three interactive
   charts, a summary table, and a 3D geometry render of the imported plates.

This packages digitalmodel's existing plate-buckling capability — it does
**not** reimplement the buckling math.

## Buckling capacity

Critical elastic buckling stress for a rectangular plate under uniaxial
longitudinal compression (classical plate theory; the DNV-RP-C201 elastic
reference stress):

```
sigma_cr = k · π² · E / (12 (1 − ν²)) · (t / b)²
usage    = sigma_applied / sigma_cr        (PASS if usage ≤ 1.0)
```

- `k` — boundary-condition buckling coefficient: **4.0** simply supported,
  **7.0** clamped.
- `E`, `ν` — Young's modulus and Poisson's ratio.
- `t`, `b` — plate thickness and loaded (short) edge breadth.

The base factor `π² E / (12(1−ν²)) · (t/b)²` and the coefficient are computed
by `ElasticBucklingCalculator` in
`digitalmodel.infrastructure.calculations.plate_buckling`
(`calculate_base_factor` / `calculate_longitudinal_buckling_stress`).

## Parametric matrix (96 cases)

| Axis | Values |
| --- | --- |
| Geometry (a × b) | small 800×400, deck 2400×1200, bulkhead 3000×1000, wide 2000×1600 (mm) |
| Thickness | 8, 12, 18, 25 mm |
| Boundary | simply-supported (k=4.0), clamped (k=7.0) |
| Material | Steel S235, Steel S355, Al 5083-H116 |

`4 × 4 × 2 × 3 = 96` verification cases.

The applied longitudinal stress is a per-material design-utilisation target
(0.60·fy for steel, 0.50·fy for aluminium) — a representative demand that
exercises the screening independent of the buckling capacity.

## Run

```bash
cd digitalmodel
PYTHONPATH=examples/demos/gtm:src PYTHONUNBUFFERED=1 \
    .venv/bin/python examples/demos/gtm/demo_10_cad_plate_buckling_verification.py

# Reuse cached results (rebuild charts + report only):
PYTHONPATH=examples/demos/gtm:src \
    .venv/bin/python examples/demos/gtm/demo_10_cad_plate_buckling_verification.py --from-cache
```

### Outputs

- `output/demo_10_cad_plate_buckling_verification_report.html` — branded report
- `output/demo_10_geometry_render.png` — 3D plate-geometry render
- `results/demo_10_cad_plate_buckling_verification_results.json` — full results

A representative run: **52 PASS / 44 FAIL** of 96 — thin, slender,
simply-supported panels in higher-strength steel are the buckling-critical
ones (higher yield ⇒ higher applied demand, same elastic capacity).

## Example input

`inputs/demo_10_plate_geometries.yml` documents the geometry set / parametric
matrix in human-readable form (the demo defines the same axes in-script).

## Scope & limitations

- **Elastic buckling only** — no post-buckling reserve, plasticity reduction,
  residual stress, imperfection knock-down, or biaxial/shear interaction (the
  full DNV-RP-C201 usage adds these and lowers the allowable stress).
- Geometry hygiene is a cheap analysis-readiness screen, **not** a CAD-kernel
  STEP/IGES topology validation.
- Footprints, gauges, and materials are representative screening values, not
  project takeoffs. Outputs are preliminary; review by a qualified structural
  engineer is required.

## Smoke test

```bash
PYTHONPATH=examples/demos/gtm:src \
    .venv/bin/python -m pytest examples/demos/gtm/tests/test_demo_10_smoke.py -q
```
