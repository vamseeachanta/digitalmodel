# Parametric monohull forms

Phase 1 of [issue #2191](https://github.com/vamseeachanta/digitalmodel/issues/2191)
generates synthetic underwater station offsets as a normal `HullProfile`.
Dimensions are metres; x runs forward from the aft perpendicular and z runs upward
from the keel. Positive LCB is forward of geometric midships. No client geometry
or free metadata is embedded. Depth is retained as a principal dimension; offsets
end at the design waterline, not the deck.

## Parameters

| Parameter | Default / permitted values |
|---|---|
| `length_bp`, `beam`, `draft`, `depth` | Required, finite and positive; depth >= draft |
| `cb` | 0.70; 0.35–0.95, and no greater than analytic Cm |
| `lcb_fraction` | 0; strictly between -0.5 and 0.5, subject to SAC feasibility |
| `parallel_midbody_fraction` | 0.4; 0–0.8 |
| `bilge_radius_fraction` | 0.2; 0–1 times beam/2 |
| `deadrise_deg` | 0; 0–30 |
| `flare_deg` | 0; -15–30; negative means tumblehome |
| `bow_fullness`, `stern_fullness` | 2; 1–4, beta end-shape controls described below |
| `transom_fraction` | 0; 0–0.95, stern waterline half-breadth / midship half-breadth |
| `n_stations`, `n_waterlines` | 41, 21; odd integers, at least 9 and 5 |
| `box`, `wigley` | False; mutually exclusive analytic comparator modes |

Models are immutable; construct a new validated model for each variation. Unknown
parameters, nonfinite values, impossible bilge/side tangencies and unreachable
Cb/LCB pairs raise `ValueError`. The parallel segment must contain geometric
midships, so its waterline breadth remains exactly beam/2. In particular, zero
parallel length fixes the fullest station at midships and restricts attainable LCB.
Tumblehome may decrease breadth with increasing z, but coordinates remain
nonnegative and must respect HullProfile's existing 5% breadth tolerance.

## Section and SAC construction

The bottom is the deadrise line z = y tan(deadrise). A circle of radius
`bilge_radius_fraction * beam/2` joins it tangentially to the straight side
`y = beam/2 + (z - draft) tan(flare)`. `midship_section(params, z)` evaluates
that closed form; `midship_area(params)` integrates its line/arc pieces
analytically. A zero-angle bottom includes its flat breadth at z=0.

**Necessary departure from the original plan:** fixed power-law exponents and a
fixed parallel length leave one run/entrance split for two independent constraints.
For equal exponents and pointed ends, changing that split does not change volume.
The implementation therefore uses beta-CDF ends with an algebraically calibrated
second shape parameter. This adds the missing degree of freedom without adding an
optimization loop. Requested fullness values are shape controls, not literal
power-law exponents. The verbatim original plan is retained separately.

Let P be parallel length / L, R be run length / L, E=1-P-R, f the transom
fraction, t=f², Am the analytic midship area, and C=Cb*B*T/Am. The normalized
mean of each end curve is

`c = (C - P - R*t) / (1 - P - R*t)`.

For each end, set b=fullness+1 and a=b(1-c)/c. The normalized curve is the
regularized incomplete beta function F(s)=I_s(a,b), from the end (s=0) to the
parallel segment (s=1). It has integral c. The stern is t+(1-t)F(x/RL);
the bow is F((L-x)/EL). The middle equals one. Their dimensional areas are
multiplied by Am. The first moment of F is

`j = [1 - a(a+1)/((a+b)(a+b+1))] / 2`.

These exact moments give the SAC centroid. One cached `scipy.optimize.brentq`
solve adjusts R to meet LCB; c is recalculated algebraically for each candidate.
Only c in (0,1) and a split whose parallel region contains midships are accepted.
Because b >= 2, the end curves meet the parallel region with zero first derivative.
Extreme calibrations can still have sharp end behavior; dense offsets do not prove
curvature convergence.

Stations and waterlines use cosine spacing to resolve the ends and bilges. Bow
sections blend toward a V shape with a smoothstep over the entrance; pointed stern
sections retain their U shape. This is a section-shape blend rather than the
original plan's interpolated section exponent. Breadths are normalized using
sampled sectional areas. For a nonzero transom, width is sqrt(local area / Am),
and a vertical shape blend provides the remaining area reduction: stern width is
f times midship width and stern area is approximately f² Am at sampling precision.

Pointed ends use continuous regularization
`sqrt((0.005*y_mid)^2 + (1-0.005^2)*y_raw^2)`. It retains the requested 0.5%
end breadth, preserves midship offsets, and avoids pinched interior stations that
an endpoint-only replacement produces. It slightly increases achieved volume;
reports always integrate the emitted offsets, including this effect.

Requested sampling can be inadequate even when the analytic SAC is feasible.
Generation rejects sampled Cb errors above 0.5%, LCB errors above 0.002 L, or a
Simpson/trapezoidal volume mismatch above 1%, with grid-refinement guidance. Very
small positive transoms that cannot fit even the last waterline triangle are
rejected with a `transom_fraction` / `n_waterlines` error. Increase counts
explicitly; the generator does not silently refine the requested discretization.

## Reports and consumers

`generate_profile(params, name="parametric_monohull")` returns a ship profile
with source `parametric_form` and its achieved block coefficient.
`form_report(params)` returns a frozen `FormReport`: Cb, Cp, Cm, Cwp,
LCB fraction, Simpson displaced volume, the independent
`HullHydrostatics.compute_displaced_volume()` trapezoidal result, and all targets.
`form_report(profile)` derives the same achieved values from offsets, with
`targets=None`; targets cannot be recovered from schema-free metadata.
Cm in the report is sampled at geometric midships, while `midship_area` is analytic.

`sweep_forms(base, ranges, screen=False, mesh_config=None)` reuses
`ParametricRange` and returns rows with `parameters`, `report`, and
`signature`. The signature is None when screening is off or HullProd is absent.
When present, it is the mesh `CurvatureSignature` from `screen_profile`.
`parametric_hull.form_space_profiles(base, ranges)` yields
`(variation_id, profile)` for existing sweep consumers.

## Closed-form checks

- Rectangular prism: `box=True, cb=1, bilge_radius_fraction=0` gives
  Cb=Cp=Cm=Cwp=1 to 1e-6. The box flag explicitly permits Cb=1.
- Wigley comparator: `wigley=True, cb=4/9, bilge_radius_fraction=0` uses
  `y=(B/2)[1-(2x/L-1)^2][1-(1-z/T)^2]` before the small pointed-end
  regularization. Analytic Cm=Cwp=2/3 and Cb=4/9; sampled Cb is checked within 1%.
- Semicircular section: radius=B/2=T, zero angles, gives analytic Cm=pi/4
  to 1e-6. Numerical section quadrature is checked independently.

Comparator flags require zero bilge, angles, LCB and transom; their longitudinal
forms override parallel length and fullness controls. These are explicit synthetic
controls, not validation against a measured vessel.

The 3 × 3 acceptance grid uses L=100, B=20, T=10, D=14 and other defaults,
with Cb in {0.55,0.70,0.85} and LCB in {-0.02,0,0.02}. All nine meet
0.5% relative Cb and 0.002 absolute LCB tolerances. The maximum measured
hydrostatics cross-check difference is 0.236%.

## Worked drillship-like example

This synthetic example has a wide transom; it has no moonpool or appendages.
The requested wide stern makes default LCB=0 unreachable for this centered
parallel segment, so an explicit attainable aft LCB target is used.

```python
from digitalmodel.hydrodynamics.hull_library import (
    MonohullFormParameters, MeshGeneratorConfig, generate_profile,
    form_report, screen_profile,
)

params = MonohullFormParameters(
    length_bp=100, beam=20, draft=8, depth=12,
    cb=0.72, transom_fraction=0.9, lcb_fraction=-0.10,
)
profile = generate_profile(params, name="example_drillship_like")
report = form_report(params)
config = MeshGeneratorConfig(target_panels=7225, waterline_refinement=2.125)
signature = screen_profile(profile, config).signature
```

Achieved Cb=0.72060327, Cp=0.72833981, Cm=0.98937785,
Cwp=0.74994590, LCB=-0.09960465. Simpson volume=11529.65237 m³;
hydrostatics volume=11520.10023 m³. Stern half-breadth is exactly 9 m.
HullProd 1.0.1 at 7,225 half-hull quad panels (28,900 expanded triangles), Lref=100:

| I_D | I_D_plus | I_D_minus | a_flat | a_single | a_elliptic | a_saddle |
|---:|---:|---:|---:|---:|---:|---:|
| 18.87238784 | 9.76817589 | 9.10421195 | 0.12016369 | 0.34359069 | 0.23946998 | 0.29677564 |

Mesh reliability is `caution`, native status `mesh_representation_sensitive`,
valid area fraction 0.98055578. Its default BRep fit exports successfully but has
`quadrature_unconverged` status and no finite I_D; no delta is inferred from NaN.
The ordinary Cb=0.7 generated rounded form likewise produced native
`geometric_singularity_nonintegrable` status at the default fit.

For a finite representation comparison, the generated Wigley control with
L=100, B=20, T=8, D=12, 41 stations and 21 waterlines uses
`MeshGeneratorConfig(target_panels=8000)`, which produces exactly 7,225
half-hull side panels (no nonzero bottom). Mesh I_D=28.94569329, BRep
I_D=7.06772470 (native `valid`), absolute delta=21.87796859,
relative delta=309.54754934%. No threshold is asserted. This large gap and the
rounded-form validity failures show that dense stations alone do not establish a
canonical curvature signature; use fit and mesh convergence checks before
interpreting curvature magnitudes.

The regression comparing Cb=0.85/P=0.5 against Cb=0.55/P=0.1 verifies the
larger developable-area fraction at equal mesh settings. The bilge-radius
regression uses Cb=0.85/P=0.6 to emphasize the parallel circular bilge:
increasing radius fraction from 0.1 to 0.6 raises singly curved area there.
That trend is not universal when changed Cm forces changed end shapes at fixed Cb.

## Phase 2

Semi-submersible and spar primitives, moonpools and holes, bulbs, appendages,
optimization, line-plan and DXF export remain out of scope. See
[curvature screening](curvature-screening.md) for representation caveats.
