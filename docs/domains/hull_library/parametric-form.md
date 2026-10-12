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
| `entrance_angle_deg`, `run_angle_deg` | Derived when omitted or None; finite half-angles, 5–60 degrees |
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

The SAC retains three segments: run, parallel body, and entrance. Let P be
parallel length / L, R be run length / L, E=1-P-R, f the transom fraction,
t=f², Am the analytic midship area, and C=Cb*B*T/Am. The normalized mean
of each end curve is still calibrated algebraically:

`c = (C - P - R*t) / (1 - P - R*t)`.

The end helper mixes a power curve and a beta CDF:
`F(s) = w[1-(1-s)^q] + (1-w) I_s(a,b)`, for distance s from the end
normalized by its run or entrance length. Set d to the required normalized
area slope, q=max(b,4d/c), w=d/q, b=max(fullness+1,2c/(1-c)), and solve
`a=b(1-c_beta)/c_beta`, where `c_beta=(c-w*q/(q+1))/(1-w)`.
Both terms are monotone, a>1 and b>=2, so F(0)=0, F'(0)=d, F(1)=1,
and F'(1)=0. Its first moment is

`j = w[1/2-1/((q+1)(q+2))] + (1-w)[1-a(a+1)/((a+b)(a+b+1))]/2`.

These exact moments feed a cached scalar Brent solve for R to meet LCB.
The stern SAC is t+(1-t)F(x/RL), the bow is F((L-x)/EL), and the
parallel segment is one, all multiplied by Am. The parallel segment must
contain midships. Fullness remains a shape control, not a literal exponent.

Pointed sections scale the midship shape by local area/Am. This removes the
old bow V-shape blend, whose area renormalization changed waterline tangency.
For a transom, width is sqrt(local area/Am); the existing vertical shape blend
supplies the remaining area reduction. At the stern its width is f times
midship width and sampled area is approximately f² Am. For the transom curve,
d includes the factor 2f/(1-f²), so the waterline slope is still tan(run angle).

## End closure

The former 0.5% breadth floor and square-root regularization displaced the
pointed tips from the centreline and distorted the Wigley control. Raw cosine
station spacing also put the first interior sample only 0.154 m from the end
on a 100 m, 41-station form. Both treatments have been removed.

Pointed ends now have exactly zero breadth at every waterline. Design-waterline
slope is +tan(run_angle_deg) at the stern and -tan(entrance_angle_deg) at the
bow, joining the parallel body with zero first derivative. Interior waterlines
inherit the midship section's scaled tangent. Transoms retain finite breadth.
Both parameters accept 5–60 degrees. When omitted or None, ordinary forms use
`degrees(atan(fullness * B / ((1-P)*L)))`, clipped to that range: bow_fullness
for entrance and stern_fullness for run. Both defaults are 33.69006753 degrees
for L=100, B=20, P=0.4 and fullness=2. Sweeps recompute implicit defaults while
preserving explicit numeric overrides.

Wigley fixes both angles to `degrees(atan(2B/L))`: 21.80140949 degrees for
L=100/B=20, or 11.30993247 degrees for L=100/B=10. Conflicting explicit angles,
or Wigley dimensions that require an angle outside 5–60 degrees, are rejected.
Box geometry overrides longitudinal shape controls; its end angles are unused.

Station coordinates use `x/L=0.25*t+0.75*(1-cos(pi*t))/2`, with evenly
spaced t in [0,1]. The first interior station is at least L/(4*n_stations)
from either end; adjacent spacing ratios stay below 2. End spacing remains
smaller than midship spacing (0.74060 m versus about 3.57 m at L=100, n=41).
Waterlines retain cosine spacing to resolve bilges. Existing schema and mesh
consumers accept all-zero end stations; neither consumer needed modification.

**Acceptance limitation:** exact offsets and BRep now agree with the matching
closed-form Wigley, but production mesh signature thresholds remain blocked.
At 7,225 half-hull panels, L=100/B=20/T=8, the consumer's fixed triangle
diagonals produce mesh I_D=38.39900 with 85.603% end share, versus I_D=6.59966
for direct analytic sampling with alternating diagonals. Even exact analytic
vertices with fixed diagonals give I_D=38.31570 and 85.594% end share. These
measurements isolate a triangulation-sensitive consumer issue outside this
lane's permitted changes. The <10% signature difference and <20% end share
remain strict expected failures, not satisfied acceptance criteria.

The BRep is valid with I_D=7.12577, within 5% of the matching closed-form
7.119738. The plan's 4.31 target belongs to L=100/B=10/T=6.25, a separate
canonical Wigley also covered by the BRep regression. The verbatim plan is
preserved in [the issue plan](../../plans/2026-09-27-issue-2241-end-closure.md).
The geometry and triangulation used for comparisons must both be recorded;
a dimensionless curvature index still depends on hull aspect ratios.

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
  `y=(B/2)[1-(2x/L-1)^2][1-(1-z/T)^2]` exactly at every emitted offset. Analytic Cm=Cwp=2/3 and Cb=4/9; sampled Cb is checked within 1%.
- Semicircular section: radius=B/2=T, zero angles, gives analytic Cm=pi/4
  to 1e-6. Numerical section quadrature is checked independently.

Comparator flags require zero bilge, angles, LCB and transom; their longitudinal
forms override parallel length and fullness controls. These are explicit synthetic
controls, not validation against a measured vessel.

The 3 × 3 acceptance grid uses L=100, B=20, T=10, D=14 and other defaults,
with Cb in {0.55,0.70,0.85} and LCB in {-0.02,0,0.02}. All nine meet
0.5% relative Cb and 0.002 absolute LCB tolerances. The maximum measured hydrostatics cross-check difference is 0.1978%.

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

Updated example measurements and the before/after Wigley comparison are recorded
below. Signature statuses remain essential: successful STEP export alone does
not imply converged curvature.

The transom example now achieves Cb=0.71999941, Cp=0.72772946,
Cm=0.98937785, Cwp=0.74542715 and LCB=-0.10006745. Simpson volume is
11519.99053 m³ and hydrostatics volume is 11510.68376 m³; stern half-breadth
remains exactly 9 m. Its default BRep status remains `quadrature_unconverged`
with no finite I_D. The rounded Cb=0.70 form remains
`geometric_singularity_nonintegrable`, also with no finite I_D. These results
do not close [issue #2241 item 2](https://github.com/vamseeachanta/digitalmodel/issues/2241).

The generated Wigley L=100/B=20/T=8 uses 41 stations, 21 waterlines and
`MeshGeneratorConfig(target_panels=8000)`, producing 7,225 half-hull quads.
End share sums |K| over finite, valid vertices with x/L<0.05 or x/L>0.95;
Lref² cancels from the ratio.

| Screening geometry | Before mesh I_D | After mesh I_D | Before end share | After end share | Before BRep | After BRep |
|---|---:|---:|---:|---:|---|---|
| Default expanded symmetry | 28.94569 | 44.00308 | 74.750% | 83.544% | valid / 7.06772 | valid / 7.12577 |
| Open half hull | 25.41117 | 38.39900 | 78.040% | 85.603% | same profile | same profile |

The mesh metrics regress despite exact offset closure; they are an unresolved
consumer limitation, not a successful end-cap signature acceptance. The canonical
L=100/B=10/T=6.25 generated Wigley BRep is valid / 4.31926, within 5% of 4.31.

The full hull-library and diffraction quality-gate suite reports 582 passed,
42 skipped and 3 expected failures. Two expected failures preserve the blocked
mesh thresholds above; the third is the existing BRep representation regression.
The curvature-screen and BRep suites are unchanged (21 and 26 collected tests).

The regression comparing Cb=0.85/P=0.5 against Cb=0.55/P=0.1 verifies the
larger developable-area fraction at equal mesh settings. The bilge-radius
regression uses Cb=0.85/P=0.6 to emphasize the parallel circular bilge:
increasing radius fraction from 0.1 to 0.6 raises singly curved area there.
That trend is not universal when changed Cm forces changed end shapes at fixed Cb.

## Phase 2

Semi-submersible and spar primitives, moonpools and holes, bulbs, appendages,
optimization, line-plan and DXF export remain out of scope. See
[curvature screening](curvature-screening.md) for representation caveats.
