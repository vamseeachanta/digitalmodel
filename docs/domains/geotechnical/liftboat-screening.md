# Liftboat screening analysis

## Summary

The [screening module](../../../src/digitalmodel/marine_ops/liftboat_screening.py)
connects staged preload equilibrium, pad or spudcan penetration, elevated loads,
leg bending and raised-leg intact stability for a generic three-leg unit.
Results describe supplied cases and criteria. They do not authorize installation,
transit or elevated storm survival. The synthetic example meets its numerical
criteria while its complete assessment remains **unresolved**.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-27 | Added synthetic screening and interface checks to identify calculation results and missing evidence separately. |

## Inputs and result contract

Every phase requires a `Basis`: configuration identifier, input provenance,
criterion version, uncertainty bounds and evaluated cases, and method
applicability evidence. These records describe evidence supplied by the caller;
the program cannot authenticate or approve them. Synthetic evidence qualifies
arithmetic demonstrations only. It does not establish measured condition,
soil behaviour, return periods or class compliance.

Units are metres, seconds, hours for preload histories, kN, kNm, kPa and kg/m³.
Angles are degrees at the interface; stability areas use metre-radians. The
right-handed frame has z upward from seabed, with x/y shared by support positions,
weight centres and buoyancy centres. Heading is measured from +x toward +y.
Positive external moments obey `sum(x R) = My` and `sum(y R) = -Mx` after
subtracting foundation moments. Hydrostatic tables start at zero heel and GZ.

`Check` records `met`, `not_met` or `unresolved`, demand, allowable, utilization,
margin and an explanatory note. Minimum criteria use allowable/demand for
utilization; maximum criteria use demand/allowable. The full-precision values
are retained. A known failed criterion takes precedence in the aggregate;
unresolved checks remain individually visible. Missing required input returns
an unresolved screen, never an implicit zero. Invalid numerical domains raise
`ValueError`. Explicit zero kinematics, free-surface correction or guide stiffness
are valid where physically applicable.

## Calculation coverage

| Check | Inputs and calculation | Criterion and boundary |
| --- | --- | --- |
| Preload equilibrium | Stage weight, buoyancy and centres, external moments and three leg histories. Every sample needs a common stage/time and a balance case. | Computed and recorded per-leg reactions differ by no more than the supplied force tolerance. Zero-load afloat preparation is permitted. |
| Foundation and travel | Explicit `pad` or `spudcan`, `ballast`, `jacking` or `combined` method, geometry, soil sensitivity profiles, depth grid, resistance factor, available travel and condition records. | Reuses [spudcan bearing](../../../src/digitalmodel/geotechnical/spudcan.py) within its bounds. Target and later storm demand need sampled equilibria without extrapolation. Observed and predicted depth, depth deviation and jacking force have explicit limits. |
| Preload hold and instability | Four-stage per-leg readings and explicit `PreloadCriteria`. | Reuses [preload sequence checks](../../../src/digitalmodel/marine_ops/preload_sequence.py), including the existing minimum one-hour hold. Missing settlement-window evidence is unresolved; persistent movement, short holds and capacity-drop flags are reported separately. |
| Storm actions | Prescribed simultaneous strip velocities and accelerations; diameter including marine growth; length, centroid, fluid density, Cd and Cm; hull and topside wind panels; heading and environmental factor. | Morison drag plus inertia and wind drag are integrated at supplied centroids. Kinematics are inputs, with no inferred design wave or return period. A factor of at least unity applies to environmental actions; vertical load and gravity moments must already represent the intended combination. |
| Storm reactions | Weight and gravity moments, common leg layout, leg stiffness and foundation moments, hull wind sharing and combined-load foundation allowables. | Three signed vertical reactions; positive contact is the overturning criterion. Per-leg unloading, foundation moment, vertical, preload and sliding utilizations are separate. Full unloading fails the strict contact criterion. Sliding resistance is the smaller of supplied horizontal capacity and friction coefficient times compression. No tension resistance or support removal is inferred. |
| Hull clearance | Hull underside, governing water surface and minimum clearance. | Actual clearance must meet the explicit minimum. Missing crest/water-level evidence stays unresolved. |
| Bending and guide | Prismatic tubular EI, point actions, top couple, guide height and rotational stiffness; fixed-foundation restraint evidence; axial force, elastic critical load and capacity inputs. | Spring compatibility, moment/shear/deflection envelopes and linear axial/bending interaction with magnification. Member, guide, shear and deflection allowables are inputs. Trusses, foundation flexibility and uncertain condition require a different qualified model. |
| Transit intact stability | Solid-weight GM/GZ table for the raised-leg loading state, free-surface correction, downflooding angle, displacement, draft, trim and configuration evidence. | Correct GM and GZ once; integrate piecewise linear GZ in radians. Minimum areas, area ratios and positive range are explicit inputs. No extrapolation or integration past downflooding. First loss of positive GZ ends the range; later positive lobes do not repair it. |
| Resistance | Hull, appendage, propulsion/tow and environmental applicability inputs are not implemented. | **NEXT**, unresolved in every integrated result. No power or transit-speed limit is issued. |

Table 1. Implemented arithmetic and the evidence required to interpret it.

For flat circular pads in uniform sand, the existing bearing expression has
finite capacity at zero depth. The wrapper explicitly assumes zero-load origin
and linear mobilization to the first positive depth sample. This grid-dependent
segment is an assumption, based on previous experience; it is not a settlement
model. The caller supplies the grid and applicability evidence. The resistance
factor in `(0, 1]` multiplies both pressure and force; it has no standard default.
The conical layered-soil calibration is not applied to a flat pad. For spudcans,
the inherited layered curve retains its documented illustrative connections and
capacity-drop warnings. Soil cases bound supplied assumptions; they do not
establish a probability distribution or validate an omitted layer.

## Structural coupling and limits

For lateral forces `P` at heights `a`, guide height `L`, applied top couple `T`,
flexural rigidity `EI` and rotational guide stiffness `k`, first-order rotation
is `(sum(P a²/2) + T L) / (EI + k L)`. The guide spring transfers moment `k theta`.
The moment at height z is `sum(P max(a-z, 0)) + T - k theta`.
Deflection extrema include interval endpoints and zero-slope locations.
The beam-column screening factor is `1 / (1 - N/Ncr)`; `N >= Ncr` fails the
instability check and leaves amplified capacities unresolved. Euler load and
allowables must represent the same condition, section and restraint. Nominal
bending alone does not establish local buckling, connection capacity or fatigue.

`screen_liftboat` derives leg actions from the storm strips and allocated hull
wind. Wind above the guide contributes a top couple. First-order foundation
moments feed back into the global vertical reaction solve; those reactions then
supply axial force to bending and later governing load to the penetration check.
The supplied transit weight change reconciles storm weight with hydrostatic
displacement within an explicit force tolerance; the phase-link evidence records
ballast and leg-position changes.
The standalone storm screen requires foundation moments explicitly. Integrated
screening replaces a claimed preload with the independently resolved staged
history target; missing or failed evidence cannot become a proven capacity.

This is first-order static coupling with an approximate structural demand
magnifier. Magnified foundation moments are not fed into a nonlinear global
solution. It assumes a level rigid hull, fixed foundations, prismatic tubular
legs, collinear environmental actions and qualified load sharing. Torsion,
soil rotational springs, structural dynamics, imperfect geometry, cyclic soil
response, plasticity, fatigue and local details need separate assessment.
Support compatibility and those applicability decisions remain explicit evidence
gates. Preload-stage lateral stability, installation/removal, jacking/guide load
transfer, transit motions/towworthiness and phase-specific risk assessment also
remain unresolved without separate records.

The public [HydroDyn introduction](https://openfast.readthedocs.io/en/main/source/user/hydrodyn/index.html)
describes strip loading from prescribed kinematics and user-specified
coefficients for slender members. It supports the method family only; the code
here does not implement the HydroDyn solver or its flexible-body terms.
No licensed standard was accessed and no class-rule values are supplied.
A project-specific basis may record applicable standard editions and clause
numbers in the criterion-version record; criteria and tabulated values remain
user inputs, with permission to use them established separately.

## Synthetic worked example

Run `PYTHONPATH=src python examples/domains/geotechnical/liftboat_screening.py`.
The [example](../../../examples/domains/geotechnical/liftboat_screening.py)
prints JSON to stdout. Redirect it to a scratch location. All numerical inputs
are **assumed, based on previous experience**, for arithmetic demonstration.
The histories are constructed from the nominal mobilization curve, not measured
settlement observations. There is no input file from an earlier engineering job.

| Quantity | Synthetic input or result |
| --- | --- |
| Layout | Supports at (-8.0, -5.0), (8.0, -5.0), (0.0, 10.0) m. |
| Storm and transit weight | 6,000.0 kN; centred gravity and buoyancy. |
| Preload | 3,000.0 kN per leg, combined ballast/jacking; preparation, preload, hold and discharge balance cases. |
| Foundation | Circular pads, diameter 4.0 m; homogeneous sand friction 30.0 degrees; submerged unit weight 8.0 to 10.0 kN/m³. Resistance factor 1.0 is illustrative. |
| Travel | Available 2.0 m; depth-deviation limit 0.2 m; jacking allowable 6,000.0 kN. |
| Water actions | Five 2.0 m strips per leg; diameter 1.0 m, velocity 1.0 m/s, acceleration 0.3 m/s², Cd 1.0, Cm 2.0, density 1,025.0 kg/m³. |
| Wind | Hull area 200.0 m² at 21.0 m; topside area 100.0 m² at 23.0 m; speed 25.0 m/s, Cd 1.0, air density supplied explicitly. |
| Structural model | Guide at 20.0 m; EI 16,000,000.0 kNm²; guide stiffness 0.0 and 1,000,000.0 kNm/rad; axial allowable 8,000.0 kN, moment allowable 6,000.0 kNm, critical load 80,000.0 kN. |
| Coverage | Four headings at both guide stiffnesses, with both soil cases for every leg; coverage remains illustrative. |
| Integrated base shear | 144.7 kN. Free-guide reactions are 2,000.0 kN per leg; spring restraint redistributes reactions. |
| Transit | Raised and secured legs; solid GM 1.2 m and free-surface correction 0.2 m give corrected GM 1.0 m. GZ points are explicitly synthetic. |
| Transit criteria | GM at least 0.8 m; range at least 40.0 degrees; area 0–30 degrees at least 0.1 m rad; area ratio (0–30)/(30–50) at least 1.0. |
| Outcome | Selected numerical criteria met; aggregate unresolved for resistance, coverage qualification and adjacent assessments. |

Table 2. Example inputs and rounded display results; calculations retain precision.

The accompanying tests check independent Morison force arithmetic, both moment
balances, spring limits, beam deflection, preload equilibrium, uncertainty cases,
free-surface corrections, radians, range truncation and configuration coupling.
Each implemented family has passing, failing and missing-input cases. Arithmetic
agreement does not qualify the methods for an actual operating decision.

## Way forward

Supply qualified geometry, measured condition, soil data, uncertainty and loading
states; establish applicability and versioned capacities; extend foundations and
frames where the fixed-base model is unsuitable. Evaluate the complete declared
environmental and transit cases, including damage/heeling requirements where
applicable. Complete resistance, motions, installation/removal and the adjacent
assessments before independent engineering review of any proposed limit.
