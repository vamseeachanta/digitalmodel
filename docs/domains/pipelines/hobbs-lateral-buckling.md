# Hobbs closed-form lateral buckling

`digitalmodel.subsea.pipeline.global_buckling`

## Why this exists

`digitalmodel.subsea.pipeline.lateral_buckling` already builds the
effective-axial-force profile along a route from a YAML config, finds the
anchor lengths, and applies a single susceptibility screen
(`0.65 x 2.26 (EA)^0.25 (EI)^0.25 phi_L^0.5`, plus a route-curve criterion).
It answers **"can this line buckle?"**

It does not answer the question that follows: **"and then what?"** — how long
is the buckle, how far does it sweep, what bending moment and combined stress
does it carry, and how much hotter can the line get before the buckle snaps
onto the long branch.

This package supplies that half, from the closed-form solution in
Hobbs (1984). It is deliberately standalone: SI units, plain dataclasses, no
config plumbing, no pandas, so it can be called from a notebook, from a study
script, or from the existing runner.

## Reference

Hobbs, R.E. (1984). *In-Service Buckling of Heated Pipelines.*
ASCE Journal of Transportation Engineering **110**(2), 175–189.
DOI: [10.1061/(ASCE)0733-947X(1984)110:2(175)](https://doi.org/10.1061/(ASCE)0733-947X(1984)110:2(175))

Lateral modes 1–4 come from Table 1 with Eqs. 26–29; the periodic
("infinite") mode from Eqs. 20–25.

## Equations

With `q_A = phi_A w` and `q_L = phi_L w` the fully mobilised axial and lateral
soil resistance per unit length, and `L` the characteristic lobe length:

| Quantity | Modes 1–4 | Periodic mode |
|---|---|---|
| Buckle force | `P = k1 EI / L^2` | `P = 4 pi^2 EI / L^2` |
| Force release | `P0 - P = k3 q_A L [sqrt(1+z) - 1]`, `z = k2 EA q_L^2 L^5 / (q_A (EI)^2)` | `P0 - P = k2 EA q_L^2 L^6 / (EI)^2` |
| Amplitude | `y = k4 q_L L^4 / EI` | same form |
| Peak moment | `M = k5 q_L L^2` | same form |
| Peak slope | not given | `k6 q_L L^3 / EI` |

| Mode | k1 | k2 | k3 | k4 | k5 | k6 |
|---|---|---|---|---|---|---|
| 1 | 80.76 | 6.391e-5 | 0.500 | 2.407e-3 | 6.938e-2 | — |
| 2 | 4 pi^2 | 1.743e-4 | 1.000 | 5.532e-3 | 1.088e-1 | — |
| 3 | 34.06 | 1.668e-4 | 1.294 | 1.032e-2 | 1.434e-1 | — |
| 4 | 28.20 | 2.144e-4 | 1.608 | 1.047e-2 | 1.483e-1 | — |
| infinite | 4 pi^2 | 4.7050e-5 | 0 | 4.4495e-3 | 5.066e-2 | 1.267e-2 |

Most textbook presentations assume one friction coefficient for both
directions. That is a real simplification — axial breakout is typically
0.3–0.6 while lateral breakout is 0.5–1.2 on the same soil — so this
implementation keeps `phi_A` and `phi_L` separate. Setting them equal
recovers the textbook form exactly, because `z` collapses to
`k2 EA q L^5 / (EI)^2`; there is a regression test that asserts it.

## The shape of the solution

`P0(L)` is **not** monotonic. It falls as the buckle lengthens (the
`k1 EI / L^2` term) and then rises as the feed-in term grows, so it has a
minimum. That minimum is the classical Hobbs snap-through force:

* below it, no equilibrium buckle of that mode exists;
* at it, the two roots merge;
* above it there are two — a short, tightly curved branch and the long
  post-snap branch, which is the damaging one.

`critical_state()` returns the minimum, `equilibria_at_temperature()` returns
the roots.

## Usage

```python
from digitalmodel.subsea.pipeline.global_buckling import (
    PipeSection, SoilResistance, critical_state, effective_driving_force,
    equilibria_at_temperature, screen_modes,
)

pipe = PipeSection.from_dimensions(
    e_modulus_pa=207e9,
    od_m=0.3239,          # 12.75 in
    wt_m=0.0159,
    submerged_weight_N_m=900.0,
    thermal_expansion_per_K=1.17e-5,
)
soil = SoilResistance(axial_friction=0.5, lateral_friction=0.7)

# What force does the line actually offer?
s_eff = effective_driving_force(
    pipe,
    temperature_rise_K=60.0,
    internal_pressure_pa=15e6,
    internal_area_m2=0.06700,
)                                      # 2 638 kN

# Which mode goes first, and by how much is it exceeded?
for result in screen_modes(pipe, soil, s_eff):
    state = result.critical_state
    print(result.mode.value, f"{state.temperature_rise_K:.1f} C",
          f"util {result.utilisation:.2f}")
```

For the section above this gives:

| Mode | L [m] | P0 [kN] | dT [°C] | amplitude [m] | M [kN·m] | combined stress [MPa] |
|---|---|---|---|---|---|---|
| 3 | 50.3 | 782 | 21.0 | 1.10 | 229 | 236 |
| 4 | 45.7 | 783 | 21.0 | 0.76 | 195 | 206 |
| 2 | 53.9 | 794 | 21.3 | 0.78 | 199 | 210 |
| 1 | 76.1 | 818 | 21.9 | 1.35 | 253 | 259 |
| infinite | 43.2 | 1071 | 28.7 | 0.26 | 59 | 105 |

Mode 3 governs, as it usually does for a surface-laid line on this class of
soil. At 1.5× the mode-3 critical temperature the two branches are 34.0 m /
0.23 m and 69.9 m / 4.09 m — the second one carries 408 MPa of combined
stress, which is the point of running the calculation at all.

## What this is not

The Hobbs solution is small-slope, elastic, and assumes an **initially
straight** line with idealised fully mobilised Coulomb friction. It gives
equilibrium paths, not the initiation temperature of a real line, which
buckles earlier at its out-of-straightness. For initiation use an
imperfection method (Taylor & Gan 1986) or FE per DNV-RP-F110.

Not included: coating bending stiffness, cyclic soil memory / ratcheting,
walking, upheaval (vertical) buckling, and any design-code acceptance check.

## Related

* `docs/domains/pipelines/lateral_buckling.md` — route-based screening scope
* `docs/domains/pipelines/uplheaval_buckling.md` — buried-line vertical case
* `docs/domains/orcaflex/library/model_library/m01_pipeline_lateral_buckling/` —
  the OrcaFlex FE model to cross-check against
* Tests: `tests/subsea/pipeline/test_hobbs_lateral_buckling.py`
