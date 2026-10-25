# Passing ship mooring response analysis

Sampled passing ship forces can be converted to line tensions, surge, sway and
yaw with a solver-free planar quasi-static model. Explicit wind drag and
compression-only fenders can be included. This is a screening calculation with
user-supplied stiffnesses and validity bounds; dynamic amplification and design
acceptance are **NOT_EVALUATED**.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-20 | Added sampled response, wind, case execution and a reproducible spring example to consume existing force histories. |
| 2 | 2026-10-22 | Included the full pretension resultant, added a period and passage-duration comparison, corrected wind conventions and preserved small reported responses. |
| 3 | 2026-10-25 | Clarified unbalanced reference pretensions and recorded regression coverage to distinguish implementation checks from response qualification. |

## Inputs and method

`MooringLineSpec.horizontal_stiffness_n_m` is the effective horizontal tangent
stiffness of each line about the reference position. It is required when
response is requested. Neither line material nor length determines it. A
separate line calculation or measurement must supply it. Existing `pretension`
is in kN; the response boundary converts it to N as specified by
[the units convention](../../UNITS.md). Fairleads and anchors share the midship
coordinate origin. Reference pretension resultants are included in total
equilibrium, with supplied motion bounds and residual tolerances governing the
response calculation. Unstretched length does not
bound the loaded horizontal reach; stretched geometry is not reconstructed.

Let `q = [surge, sway, yaw]`, with translations in metres and yaw in radians.
For the unit vector `n` from anchor to fairlead at location `(x,y)`, define
`b = [nx, ny, x*ny-y*nx]`. Linearized extension is `b.q`, and the tension is
`T = T0 + k*b.q`. The incremental restoring matrix is `K = sum(k*b*b.T)`.
These equations follow spring force and rigid-body force/moment balance in
OpenStax, *University Physics Volume 1* (2016),
[section 15.1](https://openstax.org/books/university-physics-volume-1/pages/15-1-simple-harmonic-motion)
and [section 12.1](https://openstax.org/books/university-physics-volume-1/pages/12-1-conditions-for-static-equilibrium).
Fixed line directions and the omission of geometric stiffness are explicit
model assumptions. The user must qualify them for the intended displacement.

A `FenderSpec` supplies contact coordinates, a normal pointing toward
compression, stiffness and initial gap. The normal is normalized. Fender force
is `R = kf*max(bf.q-gap,0)`. The contact set is iterated, solving
`K*q + B.T*T0 + sum(bf*R) = [Fx,Fy,Mz]` at each sample, where the rows of `B`
are the line vectors `b`. Unsupported restraint, contact
iteration failure, negative line tension and
excess residuals raise an error. Slack lines are not silently clipped.

`ResponseSettings` requires `max_translation_m`, `max_yaw_rad`,
`force_tolerance_n` and `moment_tolerance_nm`. The translation bound applies to
the horizontal displacement norm. These are user-defined model validity and
numerical checks, not sourced design acceptance limits. No defaults are guessed.
Numerical tolerances apply to the total equilibrium residual; no tolerance
discards a reference pretension resultant. Response numeric inputs reject
booleans, numeric strings and non-finite values.

An uncoupled surge-period estimate `T=2*pi*sqrt(m/Kxx)` uses displacement mass
in kg and bare-line surge stiffness in N/m, following OpenStax, *University
Physics Volume 1* (2016), section 15.1, cited above. The passage duration
`(L1+L2)/U` uses both hull lengths and the effective case speed. The report
compares the period with that duration and gives their ratio. This estimate
excludes added mass, coupled modes and fender contact stiffness. No dynamic
acceptance criterion is supplied: applicability remains **NOT_EVALUATED**.
Cases without a passing load have no passage duration or corresponding ratio.

## Wind and coordinate conventions

`MooredVesselSpec.wind_load` supplies air density, frontal/lateral reference
areas, surge/sway coefficients and centre of pressure coordinates. Wind speed
and direction come from `EnvironmentSpec`. Direction is the meteorological
**from** direction relative to the moored bow, increasing toward starboard.
Transform global meteorological bearings into this body convention before use.
Thus 0 degrees is from the bow, 90 from starboard, 180 from the stern and 270
from port. The passing ship travels on the positive-y (starboard) side of the
moored hull; the supplied separation is the positive centreline distance.

For `vx=-V*cos(direction)` and `vy=-V*sin(direction)`, the explicit simplified
model is `Fx=rho*Ax*Cx*vx*abs(vx)/2` and
`Fy=rho*Ay*Cy*vy*abs(vy)/2`. Yaw moment is `xcp*Fy-ycp*Fx`.
The one-half factor is from NASA, [*Drag Equation*](https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/drag-equation/)
(2021, updated 2025). Componentwise projection is a model assumption; supplied
coefficients must use this convention. No directional ship coefficient table,
shielding or aerodynamic yaw coefficient is inferred.

## Case execution and output requests

```python
from digitalmodel.hydrodynamics.passing_ship import (
    PassingShipSpec, run_analysis_cases,
)

spec = PassingShipSpec.from_yaml("spec.yml")
results = run_analysis_cases(spec, time_s=sample_times, start_stagger_m=start)
```

Each declared `AnalysisCase` runs once in order with its own wind/passage flags,
speed and separation overrides. Inputs are not mutated. An empty case list runs
one `base` passage without wind. The caller supplies the event extent and sample
grid; times start at zero and strictly increase. Position is
`start_stagger_m + closest_approach_offset_m + speed_ms*time`.

`OutputSpec` flags return the corresponding histories or `None` when unrequested.
The force history contains the combined wind and passage loads. Defined fenders
participate in equilibrium even when their output is unrequested. Requesting
fender output without definitions raises an error. A force-only calculation can
omit response settings and line stiffnesses. The existing preparation workflow
continues to prepare force samples independently.

The existing passing force model supports parallel tracks only. Nonzero passing
angles and nonzero current are refused explicitly. Water depth must exceed both
drafts, and passing hulls must have positive lateral clearance. This calculation
does not check anchor clearance, line footprints or navigation safety. The
existing finite-depth force correction is an uncited approximation and remains
unqualified; response results inherit that limitation when finite depth is used.

## Worked example: inputs, method and answer

The executable example is
[passing_ship_response.py](../../../examples/domains/hydrodynamics/passing_ship_response.py).
All numerical inputs are synthetic arithmetic values selected for simple hand
calculations. They are not measured or qualified line or wind properties.

Six opposed springs have stiffness 1000.0 N/m and pretension 10.0 kN each.
One pair restrains surge at midship; two pairs restrain sway at longitudinal
coordinates +2.0 m and -2.0 m. Their anchor reaches and lengths are 20.0 m.
The initially balanced arrangement gives:

- Surge stiffness: `2*1000 = 2000.0 N/m`.
- Sway stiffness: `4*1000 = 4000.0 N/m`.
- Yaw stiffness: `4*1000*2**2 = 16000.0 N m/rad`.

For a bow wind of 10.0 m/s, assumed air density 1.0 kg/m³, frontal area
4.0 m² and coefficient 1.0, `Fx = -1*4*1*10**2/2 = -200.0 N`.
With lateral centre of pressure 1.0 m, `Mz = 200.0 N m` and `Fy=0.0 N`.
The independent hand answer is surge `-200/2000 = -0.1 m`, zero sway,
and yaw `200/16000 rad` (approximately 0.7 degrees). The opposing surge
line extends 0.1 m, giving `10000+1000*0.1 = 10100.0 N` tension.
These exact arithmetic expectations are asserted in the tests.

Reference pretensions may also have a nonzero resultant. With no external load,
a synthetic surge resultant of 50.0 N and surge stiffness of 2000.0 N/m give
`surge = (0-50)/2000 = -0.025 m`. The independent equilibrium test checks this
displacement and equal opposing tensions at both strict and looser residual
tolerances; the tolerance does not discard the physical resultant.

Run from the repository with an explicit destination outside the source tree:

```sh
PYTHONPATH=src python examples/domains/hydrodynamics/passing_ship_response.py \
  --output-dir "$TMPDIR/passing-response"
```

The example exports `spec.yml`, `response.csv` and `response.html`. It runs still,
wind-only, passage-only, combined and faster/closer cases. Its three samples
illustrate the response API; they do not cover a complete passage event.
The HTML embeds the plotting library and shows computed tension envelopes,
motion histories and applied force histories. To add computed results to the
existing force benchmark report, pass
`generate_benchmark_report(response_results=results)`.
Tension envelopes are displayed in N; yaw plots use mrad. Significant-figure
axis and hover formats preserve small motions. SI histories and CSV inputs
retain their original precision. `response_sections` accepts `revision_history`
rows with `revision`, `date` and `change`; missing records are labelled explicitly,
and no issue date or revision is inferred from the date of generation.

## Verification and remaining qualification

Tests independently assemble spring equilibrium answers, fender gap closure,
wind force/moment arithmetic and output selection. Real force-calculator tests
check increasing response with decreasing separation and quadratic speed
scaling. These verify implementation behaviour, not measured response accuracy.

The local regression on 2026-10-25 returned **370 passed, 6 skipped** across
`tests/hydrodynamics/passing_ship`,
`tests/hydrodynamics/test_passing_ship_forces_workflow.py` and
`tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py`.
The three response modules (`test_mooring_response.py`,
`test_response_analysis.py` and `test_response_report.py`) contributed **73
passing cases**, including example exports. The skips comprise one unavailable
optional validation module and five cases awaiting extracted VBA reference
values. These local results do not establish completion of hosted checks.

Later work includes nonlinear catenary/geometric stiffness, slack-line response,
inertia and damping, nonparallel tracks, directional wind tables and measured
or published response benchmarks. No licensed solver or historical response
dataset is used. Line strength, fender energy, dynamic amplification and
operability checks remain **NOT_EVALUATED** until their inputs and criteria are
provided and qualified.
