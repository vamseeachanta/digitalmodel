# Generic jack-up preload holding analysis

The equivalent soft-clay calculation predicts primary settlement, consolidation
and gained undrained capacity for three loading rates. For the assumed profile,
rapid loading needs about 5.2 h of holding to reach the illustrative settlement
rate criterion; slower loading reaches that criterion at the one-hour floor.
The calculation supplies a planning sensitivity, with no installation acceptance
or qualified regional parameter distribution. The way forward is to obtain
colocated consolidation and strength tests, confirm the standard holding clauses
and assess the complete foundation and loading procedure independently.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-28 | Add an equivalent clay consolidation model, three loading rates and independent calculations to expose holding-time sensitivity. |

## Calculation basis

The public [MIT 1.361 course notes (2004), Part V-2, sections 1.1-1.2,
pages 2-3](https://ocw.mit.edu/courses/1-361-advanced-soil-mechanics-fall-2004/resources/part_v_2/)
give the constant-property one-dimensional diffusion equation, its drainage
normalization and the settlement/consolidation relationship. The public
[FHWA NHI-06-088, *Soils and Foundations*, Volume I (December 2006),
section 7.5.3, equation 7-8](https://www.fhwa.dot.gov/engineering/geotech/pubs/nhi06088.pdf)
gives the same normalization. Two selected points in its Table 7-4 are used as
independent test checks. No source table or figure is reproduced.

For a uniform initial excess pore pressure, a single-drained slab has a drained
boundary at x = 0 and a no-flow boundary at x = Hd. Separation of variables
gives sine modes with odd positive integer j and eigenvalues j² pi²/4 in
dimensionless time. Expanding uniform initial pressure gives coefficients
4/(j pi); averaging each mode through the slab multiplies these by 2/(j pi).
Thus the independently derived average excess-pressure coefficients are
8/(j² pi²), and:

```text
Tv = cv t / Hd²
U(Tv) = 1 - sum over odd j [8/(pi² j²) exp(-pi² j² Tv/4)]
delta_sigma = influence_factor * held_load / footing_area
s_final = mv * clay_thickness * delta_sigma
s(t) = s_final * U(Tv)
```

Double drainage uses symmetry about the layer centre and Hd equal to half
the layer thickness. For very early times, the semi-infinite solution
U = 2 sqrt(Tv/pi) avoids subtracting nearly equal numbers; the image correction
at the chosen crossover Tv = 0.01 is below exp(-100). Above the crossover,
64 odd modes leave an absolute omitted contribution below 1e-180.

A finite linear loading ramp of duration r uses load superposition:
Ubar(h,r) = integral of U(v) from h to h+r, divided by r, with all times
normalized by cv/Hd². Individual exponential modes are integrated analytically.
Short-time integration uses a rationalized difference of powers; a ramp
crossing the formula boundary is split into weighted means to avoid cancellation.
The ramp duration approaches zero continuously. Its zero-duration limit is an
ideal rapid undrained application: no consolidation settlement or strength gain
is credited at first attainment of full load.

FHWA section 5.5.6.1, page 5-54, describes the approximately proportional
relationship between normally consolidated clay strength and effective vertical
stress, and cautions about its variability. This model uses a supplied ratio S:

```text
su(t) = S * [initial_effective_stress + delta_sigma * Ubar(t)]
capacity(t) = area * existing_uniform_clay_resistance(su(t))
margin_ratio(t) = capacity(t) / held_load - 1
```

Applying this normalized relationship to the evolving mean stress is an
equivalent-slab approximation, with S held constant. It is not an
overconsolidation model. Capacity calls the existing uniform-clay circular
research reference in [spudcan.py](../../../src/digitalmodel/geotechnical/spudcan.py),
using zero strength gradient and the equivalent circular diameter. The research
factor is not a standard acceptance factor. Undrained capacity is used at every
time, evaluated with the strength gained so far. No drained capacity is assigned
to the slower cases. An unsupported ramp invalidates the stable held-load
prediction; a low settlement rate cannot remedy insufficient capacity.

## Assumed profile and ranges

The example represents a generic three-leg independent-leg jack-up. Each leg is
evaluated separately with the same synthetic inputs. The layered bed is an
assumed regional-style sensitivity profile: permeable sand above soft clay,
with stiffer low-permeability clay below. It is **assumed, based on previous
experience**, without a measured regional distribution or a site match. The
following ranges are illustrative assumptions, not published regional limits,
equipment ratings or statistical intervals.

| Input | Example | Assumed sensitivity range | Role |
| --- | --- | --- | --- |
| Upper sand thickness | 1.0 m | 0.5–2.0 m | Drained upper boundary only; no sand settlement or bearing contribution. |
| Compressible clay thickness | 4.0 m | 2.0–8.0 m | Only layer contributing consolidation settlement and strength gain. |
| Lower clay | Stiffer, low permeability | Rigid to compressible support remains unqualified | Treated as an impermeable rigid base; deeper settlement excluded. |
| Drainage length | 4.0 m | Half to full clay thickness | Single drainage selected; double drainage is an API sensitivity. |
| cv | 0.02 m²/h | 0.005–0.05 m²/h | Assumed constant, requiring laboratory or field qualification. |
| mv | 0.001 inverse kPa | 0.0005–0.002 inverse kPa | Assumed constant volume compressibility. |
| Initial mean effective stress | 80.0 kPa | 40.0–120.0 kPa | Equivalent representative clay stress, not derived from the layer weights. |
| S = su / effective stress | 0.25 | 0.20–0.35 | Supplied NC strength ratio; initial example strength is 20.0 kPa. |
| Footing area | 160.0 m² | 100.0–200.0 m² | Constant equivalent circular bearing area. |
| Preload per leg | 18,000.0 kN | 12,000.0–24,000.0 kN | Held vertical load; no structural capacity claim. |
| Stress influence factor | 0.25 | 0.10–0.40 | Converts footing pressure into uniform slab stress; no stress-spreading solution. |
| Initial penetration datum | 1.0 m | 0.5–2.0 m | Added to calculated settlement for sequence readings. |
| Rapid loading duration | 0.1 h | 0.05–0.2 h | Drainage during this ramp is deliberately neglected. |
| Intermediate loading duration | 24.0 h | 12.0–48.0 h | Linear ramp with partial drainage retained. |
| Slow loading duration | 240.0 h | 120.0–480.0 h | Linear ramp with partial drainage retained. |

The numerical implementation restricts ultimate vertical strain to 0.1 as an
assumed small-strain applicability bound. Independently varying the ranges
above can violate it; those combinations are rejected, not qualified by the
table. The layered description supplies boundary context for one equivalent
compressible slab, not a coupled multilayer flow or penetration calculation.

The rates follow from load divided by ramp duration: 180,000.0, 750.0 and
75.0 kN/h. Holding time starts at first attainment of full load in every case.
Settlement includes primary consolidation during ramp loading. Initially,
capacity exceeds the target even without consolidation credit, so these
illustrative ramps do not rely on waiting under an unsupported full load.

## Holding criterion and integration

The recommendation uses the existing
[PreloadCriteria](../../../src/digitalmodel/marine_ops/preload_sequence.py):
at least 1.0 h at full load, with total settlement travel over the trailing
0.25 h strictly below 0.001 m/h. These controls are **assumed, based on previous
experience**. Illustrative sensitivity ranges are 1.0–4.0 h for the hold floor,
0.25–1.0 h for the window and 0.0005–0.002 m/h for its rate limit. Their source
is the existing explicit screening assumption; no standard provenance is claimed.

For this monotonic settlement model, travel equals the positive depth change.
The recommendation searches from the larger of the hold floor and complete
window duration, using bisection on the monotonic trailing-window rate.
The example uses a numerical horizon of 2,400.0 h; a criterion not reached in
the supplied horizon returns an explicit unresolved result, rather than a
fabricated recommendation. A zero instantaneous ramp has an initially singular
settlement derivative, but complete window rates remain finite.

The example rounds the suggested schedule upward in increments of 0.1 h, then
generates preparation, preload, holding and discharge readings and calls
`screen_preload_sequence`. The existing screen owns stage validation, full-load
duration, strict settlement-rate comparisons, depth comparisons and independent
hazard flags. No second implementation of those decisions is introduced.
The illustrative loading reference rises linearly from zero to initial capacity
at 6.0 m, with an assumed reference-depth sensitivity of 4.0–8.0 m. It only
exercises the existing depth comparison; it is not a layered penetration model.
Discharge retains the last depth as a placeholder, with no rebound prediction.

| Case | Settlement at full-load attainment | Capacity margin at that instant | Suggested hold, rounded for display | Scheduled hold checked by existing screen |
| --- | --- | --- | --- | --- |
| Rapid undrained ramp | 0.0 mm | 12.7 % | 5.2 h | 5.2 h |
| 24.0 h ramp | 14.7 mm | 17.9 % | 1.0 h | 1.1 h |
| 240.0 h ramp | 46.3 mm | 29.0 % | 1.0 h | 1.1 h |

Longer loading durations allow more consolidation before holding begins; their
shorter additional hold does not imply a shorter total loading-and-holding time.
A low rate can also result from slow drainage while much settlement remains.
Rate, strength, capacity and operational constraints require separate assessment.

## Standard references and open clauses

| Reference edition | Holding topic | Status |
| --- | --- | --- |
| SNAME 5-5A, Revision 3; publication year TO CONFIRM | Minimum holding period and cessation of settlement | **clause: TO CONFIRM**; no period is attributed to this reference. |
| ISO 19905-1:2023; edition metadata requires independent confirmation | Preload holding criterion | **clause: TO CONFIRM**; no compliance claim or criterion imported. |

No standard text or numerical standard table has been imported. Public catalogue
access did not establish the holding provisions. The edition and exact clause
must be confirmed before replacing the illustrative criterion with a qualified
procedure. The research equations and public course notes are the executing
calculation basis, independently of those open standard references.

## Reproduce and embed the curves

```sh
PYTHONPATH=src python examples/domains/geotechnical/jackup_preload_holding.py "$OUTPUT_DIRECTORY"
```

Use an explicit output directory outside the checkout. The example writes
`results.json`, `settlement.figure.json` and `capacity.figure.json` together.
The two figure files are Plotly figure dictionaries with three traces each,
local data arrays, labelled units and a common holding-time origin. A report
can embed them with its own local Plotly renderer; neither generation nor the
JSON figures request a network resource. Chart labels use one decimal place;
calculation JSON retains full precision for later computation and rounding.

The tests independently check selected published time factors, early-time and
two-mode analytical solutions, dimensional stress/settlement/strength arithmetic,
linear-ramp integration, single/double drainage scaling and an analytical
late-time holding root. A regression covers tiny ramps across the numerical
formula boundary. An end-to-end example check verifies the existing sequence
screen and adjacent JSON outputs while refusing network connections.

## Known limits and way forward

The model assumes saturated normally consolidated clay, constant material
properties, uniform stress increment and unchanged geometry. Radial flow,
creep, remoulding, cyclic degradation, scour, large deformation, changing bearing
area, deeper-layer deformation and coupled penetration are excluded. The
normalized-strength relationship is a preliminary correlation, not a calibrated
spudcan strength-gain law. Capacity is the existing homogeneous circular
research reference rather than a layered or combined-load resistance envelope.

Obtain site-specific oedometer and strength data, resolve drainage boundaries and
effective stresses, qualify stress influence and capacity, confirm the standard
edition and holding clauses, and review the full load history and operational
controls before applying a holding schedule. Independent engineering review is
still required; the arithmetic and offline-output checks establish reproducibility
within the stated model, not field applicability.
