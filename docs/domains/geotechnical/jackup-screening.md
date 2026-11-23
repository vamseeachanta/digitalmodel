# Generic jack-up screening demonstrator

This example calculates a spudcan resistance curve and illustrative three-leg
static checks using selected public facts and explicit assumptions. It is a
**generic screening demonstrator, not a verified site-specific assessment**.
Published-case disagreement and unqualified environmental inputs prevent an
installation acceptance conclusion. The way forward is to qualify those inputs
and improve the penetration method before using it for a particular installation.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-27 | Add public research calculations, benchmark limitations and an offline example to make the screening boundaries reproducible. |
| 1 | 2026-10-27 | Replace the square sand reference with published circular factors and add traceable environmental extracts and load sensitivities to close the missing calculation paths. |
| 2 | 2026-10-27 | Separate constants and bounds by their evidence basis, replace unsupported collection statistics with synthetic particulars, preserve frozen bytes across checkouts and add independent layered-curve and recovery expectations. |
| 3 | 2026-10-27 | Add per-leg preload histories, explicit hold controls and penetration comparisons to expose settlement evidence and independent punch-through warnings. |
| 4 | 2026-10-27 | Separate arithmetic equality from hold criteria to accept complete decimal-time windows and reject equality at the strict settlement-rate limit. |
| 5 | 2026-10-27 | Confirm layered resistance warnings, include preload air-gap mitigation and map implemented checks to clause references with uncovered requirements OPEN. |
| 6 | 2026-10-27 | Add a synthetic weak-pocket sweep, penetration changes and an assumed-rate drop indication to expose sensitivity to unsurveyed local strength loss. |

## Run the example

Use an environment containing the repository dependencies. Direct all outputs and
plotting caches outside the checkout.

```sh
PYTHONPATH=src python examples/domains/geotechnical/jackup_screening.py "$OUTPUT_DIRECTORY"
```

The example verifies the frozen input and environmental extract digests before calculating. It writes
`results.json` to the supplied directory. It performs no network request and
uses no licensed solver. The [implementation plan](../../plans/jackup-screening-demonstrator.md)
records scope, rights and verification.

## Inputs and reuse

The numerical snapshot is [inputs.json](../../../examples/data/jackup_screening/inputs.json).
Its [manifest](../../../examples/data/jackup_screening/manifest.json) hashes the
exact committed bytes and records digests of external research manuscripts
and observation archives. Narrow LF checkout attributes preserve all four
frozen files under Windows text-conversion settings; digest verification remains strict. Digests prove byte identity,
not engineering applicability or publication rights.

| Input | Basis | Use and limitation |
| --- | --- | --- |
| Generic particulars | Synthetic inputs assumed, based on previous experience | Hull length 72.0 m, breadth 66.0 m, depth 8.0 m and leg length 150.0 m are illustrative context only, unused in load transfer. No recovered specifications, sample medians or collection statistics are claimed. |
| Layered soil | [Hossain et al. (2019), Table 1, Site 3 and equation 1](https://api.research-repository.uwa.edu.au/ws/portalfiles/portal/45701479/hossaintxt.pdf) | Selected numerical facts for a published anonymized Gulf profile: sand to 4.9 m, clay below it. Strength is defined at the interface and increases below it. It is not colocated with the buoy. |
| Observations | [NOAA NDBC station 42019, 2024 archive](https://www.ndbc.noaa.gov/data/historical/stdmet/42019h2024.txt.gz) | Replayed through the existing NOAA client parser. 52,698 timestamped records; 16,693 usable wave-height and 50,597 usable wind-speed values. Sample maxima are 6.8 m and 24.3 m/s. Missing values are counted, not replaced with zero. |
| ERA5 | Existing ERA5 client interface and [public API access instructions](https://cds.climate.copernicus.eu/how-to-api) | Authenticated retrieval remains unavailable. A separate, labelled illustrative scenario reuses the original NDBC sample maxima of 6.8 m and 24.3 m/s; these are not ERA5 estimates. Interpreting that wind at 10 m is an assumption, without a sensor-height adjustment. |
| Bathymetry | Existing GEBCO client and [current public service description](https://www.gebco.net/data-products/gebco-web-services/web-map-service) | Correct the obsolete endpoint, select a queryable layer and use latitude–longitude axis order for WMS 1.3.0 EPSG:4326. The current query at the rounded buoy coordinates returns no numerical result. A separately labelled 60.0 m shelf-depth sensitivity is used; no measured depth is inferred. |
| Settlement context | [Li et al. (2018)](https://pmc.ncbi.nlm.nih.gov/articles/PMC6218051/) | Public collection identifies a post-preload settlement study. No settlement equation, numerical input or copied source content is used here. Settlement is not equated with punch-through. |
| Shelf-strength context | [BSEE research report 552](https://www.bsee.gov/sites/bsee.gov/files/research-reports/552ab.pdf) | Collection notes identify public strength curves and proprietary extensions. Neither is digitized or used here. The selected layered profile comes from the cited research case instead. |

Only synthetic particulars, selected cited facts and original calculations are included;
source tables, text, documents and figures are excluded. The Hossain accepted
manuscript is identified as CC BY-NC-ND in the supplied source register. Numerical
facts are cited; no adapted source figure is supplied. NOAA observation facts are
United States government data. The supplied specification and design collections
have no retrievable anonymous provenance/reuse record suitable for this example.
Their aggregate tables and collection hashes are therefore excluded; synthetic
particulars replace those unsupported source claims. ERA5 and GEBCO reuse
qualification remains open because neither supplies an observed calculation input here.
Li and BSEE are bibliographic context from the supplied register, not claims that
their full documents were re-fetched or numerically qualified in this work.

## Illustrative assumptions

| Quantity | Value | Basis |
| --- | --- | --- |
| Longitudinal and transverse spacing | 40.0 m and 44.0 m | Assumed, based on previous experience; synthetic isosceles triangle centred at the origin, chosen to exercise biaxial equilibrium. |
| Lightship | 8,000.0 t | Assumed, based on previous experience; synthetic mass chosen to exercise three-support equilibrium. |
| Variable load | 3,470.0 t | Assumed, based on previous experience; synthetic mass retained for numerical continuity, with no collection median claimed. |
| Preload per leg | 45.0 MN | Assumed, based on previous experience; synthetic load comparator, not an equipment rating or structural capacity. |
| Spudcan area | 160.0 m² | Assumed, based on previous experience; synthetic circular footing area, with equivalent diameter 14.3 m; actual geometry is unqualified. |
| Base cone angle | 13.0 degrees | Illustrative assumption satisfying the research model's conical-base domain. Actual geometry is unqualified. |
| Critical-state friction and log crushing strength | 31.0 degrees and 10.0 | Illustrative assumptions for the research model's silica-sand parameters; mineralogy is unqualified. Crushing strength uses kPa inside the logarithm. |
| Curve connections | Straight segments | Illustrative assumptions connecting zero resistance, the analytical peak and the clay branch. No published post-peak solution is claimed. |
| Structural load sensitivity | F = C Hs²; C = 1,000.0 kN/m²; application height 50.0 m | Illustrative assumptions exercising equilibrium. The height does not assert a water depth. No metocean transfer function is established. |
| Sliding resistance | 10.0 MN | Illustrative assumption supplied independently to exercise the inequality; not derived from the selected soil profile. |

The particulars remain explicitly labelled in the input snapshot. Additional
environmental assumptions are labelled in
[environment.json](../../../examples/data/jackup_screening/environment.json):
depth 60.0 m, period 12.0 s, aligned current 0.5 m/s, air gap 10.0 m,
effective wind/water areas 1,000.0/300.0 m² and drag coefficients 1.2/0.9.
They exercise the load calculation; they are not measurements or unit ratings.
No qualified current return level or site-specific joint extreme is supplied.

## Environmental extract and extremes

[freeze_jackup_environment.py](../../../examples/domains/geotechnical/freeze_jackup_environment.py)
retrieves the 20 annual NDBC station 42019 archives for 2005–2024, one request
at a time. Responses remain external, with URL, UTC retrieval date, HTTP status
and SHA-256. Completed responses are reused only after digest verification.
The supplied extract was retrieved on 2026-10-27. NDBC units and UTC timestamps
follow its [measurement descriptions](https://www.ndbc.noaa.gov/faq/measdes.shtml).
Selected government observation facts are used under the
[NWS public-data terms](https://www.weather.gov/disclaimer); no endorsement is implied.

The [small extract](../../../examples/data/jackup_screening/ndbc_annual_maxima.txt)
retains the first row attaining each annual wind or wave maximum, deduplicated
by timestamp. Only timestamps, wind and wave values are retained; unused stdmet
columns contain missing markers. The original record counts, valid counts and
valid-day counts remain in the environmental metadata. Both extraction and
offline replay use the existing NOAA date-query parser. Values outside the
finite interval from zero inclusive to 99 exclusive are rejected as missing.
The extract and metadata have independent committed-byte digests in the manifest.

An **illustrative assumption** selects years with both parameters observed on
at least 330 distinct UTC days. This retains 2005–2008, 2013, 2016–2018,
2021–2022 and 2024: 11 annual blocks. Nine other years are excluded. This day
screen does not qualify within-day completeness, sensor changes, storm-peak
censoring, independence or stationarity. The original 2024 sample maximum
remains a sample maximum and is not relabelled as a return level.

For each parameter independently, fit the Gumbel maximum distribution by
the [NIST/SEMATECH e-Handbook method of moments](https://www.itl.nist.gov/div898/handbook/eda/section3/eda366g.htm).
With annual maxima x, sample standard deviation s using n−1, and Euler's
constant γE, use β = s√6/π and μ = mean(x) − γEβ. Invert the published maximum
CDF at annual non-exceedance probability 1−1/T:
xT = μ − β ln[−ln(1−1/T)]. Reject fewer than three blocks or zero variation.

| Marginal level | Wave height | Wind speed |
| --- | ---: | ---: |
| Illustrative 50-year fit | 8.3 m | 25.6 m/s |
| Illustrative 100-year fit | 8.9 m | 27.0 m/s |

These are **illustrative extrapolations**, without distribution-fit or uncertainty
qualification. The wind remains at the observation's unqualified sensor height.
Missing storms may bias the fit downward; a day-count threshold does not resolve
that bias. Pairing marginal wind and wave levels does not establish a joint
return period. No hurricane-specific model, directional analysis or validated
design extreme is claimed.

Reproduce the environmental inputs after preparing the original input snapshot:

```sh
PYTHONPATH=src python examples/domains/geotechnical/freeze_jackup_environment.py fetch "$SOURCE_DIRECTORY"
PYTHONPATH=src python examples/domains/geotechnical/freeze_jackup_environment.py freeze "$SOURCE_DIRECTORY" "$SNAPSHOT_DIRECTORY"
```

Keep both directories external when reproducing the calculation. The example
also accepts configured NOAA, ERA5 and GEBCO clients through `public_clients`.
It consumes their date/location queries and depth lookup, rejecting empty
observations and nonpositive depth. Those queries supply 2024 sample maxima,
without assigned return periods. Transport fixtures verify the interfaces;
authenticated ERA5 retrieval remains unqualified.

## Geotechnical methods

[spudcan.py](../../../src/digitalmodel/geotechnical/spudcan.py) implements
[Hu et al. (2015), equations 4–12](https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-11.pdf)
for peak pressure using an implicit, stress-dependent sand strength. Bisection
stays inside the stated dilatancy interval. Equations 13–14 provide the underlying
clay branch, including a sand-plug contribution. Conical-base calculations require
sand thickness divided by equivalent diameter between 0.16 and 1.0, and a base
angle greater than 7.0 degrees. Inputs outside these bounds are rejected.

Clay strength is evaluated as interface strength plus gradient times depth below
the interface, following equation 1's definition in Hossain. The collection warns
that depth origins must be reconciled rather than inferred from column labels.
This interface interpretation reproduces the selected published peak comparison;
it does not independently qualify the original boring interpretation.

Pressure in kPa multiplied by area in m² gives load in kN. Depth follows the
lowest level of the widest spudcan cross-section, as defined in the research
methods, rather than the cone tip. The curve includes the exact peak and layer
interface, even when the requested sampling grid misses them.

A decreasing resistance branch flags potential capacity loss. A separate flag
states whether the supplied preload reaches the peak. A preload below the peak
is **not an installation acceptance criterion**, particularly given the field
comparison below. Recovery depth solves static force equilibrium; it does not
predict rapid penetration, inertia, buoyancy arrest or leg interaction. The
caller must restrict the calculation to the known extent of its soil profile.

The soil-to-preload regression covers strong-over-weak resistance, uniform sand
and a rising two-layer connection representing stronger lower support. The first
case flags; the other two do not. `PunchThroughScreen` reports `peak_depth_m`,
`peak_load_kn` and the reduced `interface_load_kn`; the interface depth is the
input `sand_thickness_m`. The warning follows the descending load-penetration
branch below the current penetration within the supplied curve. It is independent
of whether the preload reaches the peak. A rising connection is an illustrative
limiting case within the existing sand-over-clay model, without a general method
for arbitrary soil layers or an independently qualified reach calculation.

Uniform-clay reference pressure follows equation 5 at zero sand thickness:
q = 6.34 su + 0.56 k D. At zero gradient this reduces to q = 6.34 su.
The uniform-sand branch uses
[The Bearing Capacity of Sand, tested by Loading Circular Plates (1961), equations 3 and 5, printed page 662](https://www.issmge.org/uploads/publications/1/40/1961_01_0110.pdf):
q = 0.3 γ′B Nγ + 1.2 γ′d (1 + 0.1 d/B) Nq, where B is the circular diameter.
The published circular factors are sγ = 0.6, sq = 1.2, dγ = 1 and
dq = 1 + 0.1 d/B. Extra surface loading, apparent cohesion and the settlement
correction are zero in this reference. Bearing factors follow
[A Revised and Extended Formula for Bearing Capacity (1970), equations 2 and 4](https://www.geo.dk/media/1431/geodgibulletinno28.pdf):
Nq = exp(π tan φ) tan²(π/4 + φ/2), Nγ = 1.5 (Nq−1) tan φ.
The calculation deliberately retains the 1961 circular shape/depth factors;
it does not substitute the later rectangular factors or an equal-area square.

`penetration_curve(UniformSand(...), depths)` evaluates that homogeneous branch
and converts pressure to force with πB²/4. Independent closed forms check
surface, quarter-diameter and one-diameter embedment. The implementation restricts
d/B to at most 1.0 and friction to between 0.0 and 50.0 degrees exclusive.
This is an explicit uniform-soil limiting case, **not** an infinite-thickness
extrapolation of Hu's bounded H/D calibration. It does not add a conical-base
correction. The 1961 paper reports measured capacities exceeding the theoretical
factors; arithmetic agreement does not validate those measurements or sand
penetration generally.
No industry standard document was accessed and no standard-derived criterion was
extracted for the new calculation.

## Constants, bounds and numerical controls

The following categories are distinct; passing a restriction does not qualify an
engineering input or establish a research calibration beyond its cited scope.

| Category | Value and basis |
| --- | --- |
| Research calibration | Conical base angle greater than 7.0 degrees and H/D from 0.16 to 1.0 follow equation 7 of the cited 2015 paper. The dilatancy interval 0.0 < IR < 4.0 follows equation 10. The 0.12 H peak depth follows the discussion after equation 12. |
| Illustrative layered-sand restriction | Critical-state friction from 20.0 to 40.0 degrees is assumed, based on previous experience, for the silica-sand example. It is not a published calibration bound. The upper cone-angle bound excludes a degenerate 90.0-degree geometry. |
| Illustrative circular-sand restrictions | Depth/diameter at most 1.0 and friction strictly between 0.0 and 50.0 degrees are assumed, based on previous experience, to restrict shallow reference calculations and high-friction extrapolation. Neither is attributed to the cited papers as a calibration limit. |
| Input reconciliation tolerance | Area may differ by at most 2.0% from the equivalent-diameter area to accommodate independently rounded inputs. This is a numerical consistency tolerance, not a research or physical bound. |
| Numerical solution | The peak solver excludes IR endpoints by 1e-5, stops at relative pressure residual 1e-8 (absolute 1e-8 kPa below 1.0 kPa), and permits 100 bisections. Dispersion uses 80 bisections. Support collinearity and contact margins are numerical tolerances of 1e-12 times squared coordinate differences and 1e-10 times total vertical load respectively. |
| Reference physical constants | Air density 1.225 kg/m³ and gravity rounded from 9.80665 to 9.81 m/s² follow Appendices B.1 and A.1 of [Definition of a 5-MW Reference Wind Turbine for Offshore System Development (2009)](https://www.osti.gov/servlets/purl/947422). Water density 1,025.0 kg/m³ follows Table 4-2 of [Definition of the Floating System for Phase IV of OC3 (2010)](https://www.osti.gov/servlets/purl/979456). These are adopted reference values, not measured site fluid properties. |
| Illustrative wave restriction | H/depth at most 0.3 is assumed, based on previous experience, to reject large relative wave heights in the sensitivity model. It is not a published breaking criterion or a linear-theory validity test; the retained range remains unqualified. |

## Structural methods and operating envelope

[jackup_screening.py](../../../src/digitalmodel/marine_ops/jackup_screening.py)
solves the three signed vertical reactions from vertical force and two moment
balances. External moments and support coordinates share an origin. The checks
report positive contact, maximum reaction relative to supplied preload, and
horizontal force relative to separately supplied sliding resistance.
These are illustrative inequalities without load factors or acceptance rules.

An optional drag calculator supplies a separate sensitivity model using assumed
effective areas, co-directional current and wave velocity, and linear-wave
kinematics. It uses H = Hs as an explicit regular-wave proxy, not an extreme-wave
conversion. Inertia, shielding, actual leg members and dynamic amplification are
absent. The example now propagates the explicitly assumed depth and load geometry
through this calculator for two NDBC marginal-fit scenarios and the separately
assumed ERA5 scenario, then solves the corresponding support reactions. All
three cases retain `illustrative_only` qualification and no joint return period.

The new `jackup_operating_envelope` entry point in
[operation_envelope.py](../../../src/digitalmodel/marine_ops/operation_envelope.py)
evaluates these static checks. It does not apply floating-vessel RAOs to an
elevated unit. It reports the last admissible sampled height and the first failed
sample, or an explicit grid cap. A failed sample followed by a passing sample
raises an error. No interpolation or forecast contingency is inferred.

The example's synthetic force is independent of period, so equal bounds across
periods are expected. This is a load-sensitivity illustration, not an environmental
operating envelope for a Gulf site. The added environmental scenarios are also
sensitivities, pending input qualification. The client adapter rejects
missing ERA5 data, empty observation records and zero or unparseable offshore depth.

## Weak-pocket sensitivity

The example also runs a separate, fully synthetic cohesive-layer case through
[weak_pocket.py](../../../src/digitalmodel/geotechnical/weak_pocket.py).
It represents a leg encountering weaker soil between survey points. All case
inputs are **assumed, based on previous experience**: footing area 160.0 m²,
an upper interval from 0.0 to 2.0 m with strength increasing from 0.0 to 40.0 kPa,
and a lower layer from 2.0 to 14.0 m with strength increasing from 60.0 to
120.0 kPa. Zero surface strength represents illustrative mobilization. The pocket
reduces the lower layer between 2.0 and 12.0 m. Nearest-survey distance is 75.0 m;
it records the reason for sensitivity screening, without a spatial correlation,
probability or geostatistical interpretation. Preload and storm leg reactions
are independently assumed as 45.0 and 50.0 MN; the storm reaction does not come
from the environmental cases above.

`WeakPocket` identifies the layer, retained-strength factors, top and bottom
depths and survey distance. The factor multiplies the interpolated strength
inside the pocket only: 0.6 retains 60.0% of strength, a 40.0% reduction.
The illustrative pressure model is q = 6.34 su at each depth, using the
uniform-clay factor described above, with no gradient or embedment correction.
Abrupt layer/pocket boundaries retain both resistance values at the same depth;
an upward jump crosses the leg load at that boundary depth. This is a sensitivity
reference, without a layered bearing mechanism, and does not modify the public
sand-over-clay profile or extend its calibration.

The sweep uses retained factors 1.0, 0.9, 0.8, 0.7, 0.6 and 0.5, from mild to
severe. It reports both reaction intersections, changes from the unreduced
profile, capacity-drop warnings and the first sampled factor with a warning.
The first flagged factor is 0.6; the onset lies between the tested factors
0.7 and 0.6, rather than being a solved critical factor. A warning remains
independent of whether either reaction reaches the descending peak.

| Retained factor | Capacity-drop warning | Preload depth / change | Storm depth / change | Preload / storm drop distance | Preload / storm assumed duration |
| --- | --- | --- | --- | --- | --- |
| 1.0 | No | 2.0 / 0.0 m | 2.0 / 0.0 m | Not applicable | Not applicable |
| 0.8 | No | 2.0 / 0.0 m | 2.3 / +0.3 m | Not applicable | Not applicable |
| 0.6 | Yes | 4.8 / +2.8 m | 6.4 / +4.4 m | 2.8 / 4.4 m | 27.9 / 44.3 s |
| 0.5 | Yes | 7.7 / +5.7 m | 9.7 / +7.7 m | 5.7 / 7.7 m | 57.4 / 77.2 s |

When a load reaches a descending peak, the drop indication is the distance
from that peak to the next upward intersection at the same leg load. No recovery
beyond the supplied profile is extrapolated. The explicitly assumed
**constant settlement rate of 0.1 m/s** gives duration = drop distance / rate;
these cases indicate tens of seconds, with the containing decade also reported.
This duration is a planning assumption, without a physical upper or lower bound
on speed or time. It supports monitoring and preload planning and is **not a
dynamic analysis**: hull inertia, load redistribution, buoyancy, damping and
rate-dependent soil response are absent. Missing case inputs or recovery outside
the supplied depths produce `unresolved`, with absent values rather than zero
penetration or zero duration. The sensitivity does not clear the
[preload air-gap mitigation](#preload-air-gap-mitigation) or establish installation
acceptance; qualified investigation and assessment remain required.

## Preload sequence screening

[preload_sequence.py](../../../src/digitalmodel/marine_ops/preload_sequence.py)
walks four explicit `PreloadStage` records in order: **preparation, preload,
holding, discharge**. Each contains chronological `PreloadSample` readings of
elapsed hours, applied load in kN and measured or computed penetration in metres.
Supply the target preload for that leg, the existing planned penetration curve
and `PreloadCriteria`; screen each leg separately. Times must increase across all
stages. Depth must share the curve's datum at the widest spudcan cross-section;
it does not use the cone apex. Computed readings need a separate settlement model.

At the last holding reading, the screen requires uninterrupted load at or above
target for at least **1.0 h**, and settlement rate strictly below the supplied
limit over a full trailing window. A load dip restarts the clock. Preparation,
preloading and discharge time earn no holding credit. Results report hold start,
hold end, duration, rate, whether the criterion was met and separate failure
reasons. A settled final window suffices; settlement need not have stopped for
the entire hour. No time after the last reading is inferred.

The default window is **0.25 h** and the default rate limit is **0.001 m/h**.
Rate is total absolute penetration movement divided by window duration, with
linear interpolation at the window boundary. This prevents rebound from
cancelling settlement. A missing full window fails the criterion; sparse
readings do not qualify motion between observations. The minimum hold can be
increased through `minimum_hold_h`, but cannot be reduced below one hour.
All defaults are **illustrative assumptions, based on previous experience**,
exposed as inputs for sensitivity screening, without a standard-derived basis.

Arithmetic boundary comparisons interpret the decimal string of each input
exactly and retain rational arithmetic for interpolation and rate division.
No comparison tolerance is used: equal durations pass, a short window fails
without extrapolation, and equality at the settlement-rate limit fails the
strict less-than criterion. Only reported results become floating-point values.
This arithmetic does not represent measurement uncertainty or recover precision
already lost in the supplied time or depth inputs.

Each reading reports planned penetration at its applied load and signed
actual-minus-planned deviation. The planned depth is the shallowest intersection
of the piecewise linear loading curve; deeper alternative roots are not chosen
silently. Loads outside its range return unavailable depth and deviation, with
no extrapolation. A separate indicator reports whether the actual depth lies
within the curve's extent. Discharge comparisons remain loading-curve references,
without an unloading or rebound model.

During preloading, a positive penetration increment raises a punch-through flag
when the load increment is at most `small_load_increment_fraction` times target
and penetration per load increment exceeds `punch_through_rate_m_kn`. Defaults
are **0.05** of target and **0.01 m/kN**, respectively, also illustrative inputs
assumed, based on previous experience. Positive penetration at constant load in
the preload stage flags separately, without division by zero; holding settlement
uses the time-based criterion instead. The first preload reading is compared
with the final preparation reading. At every stage, any decreasing capacity
segment extending below the current tip raises an independent warning, even
below peak load. Neither a settled hold nor an absent flag establishes acceptance.

### Preload air-gap mitigation

Every `PreloadSequenceScreen` includes `mitigation_note`: during preload, keep
the hull at a small air gap, close to the water, so that buoyancy can be regained
if a leg punches through. The note is returned with both flagged and unflagged
results and is serialized in the example's per-leg `preload_sequences` output.
It supplies no numerical clearance, hull immersion calculation or prediction of
buoyancy arrest; installation-specific limits and procedures remain OPEN.

The offline example adds three identical synthetic per-leg histories under
`preload_sequences` in `results.json`, labelled `synthetic_histories_only`.
Loads, times and added settlement are assumptions; loading depths follow the
existing illustrative mobilization branch. Each history holds from 0.25 h to
1.25 h and satisfies the final window criterion, while retaining the deeper
capacity-drop warning. No actual leg interaction or settlement response is
predicted. Synthetic tests cover a passing hold, persistent settlement, a short
hold, load interruption, missing evidence, rebound, explicit threshold changes,
sharp penetration and a deeper capacity drop. No standard document was accessed
or transcribed for this addition.

## Clause references and coverage

G identifies *Guideline for Site Specific Assessment of Mobile Jack-Up Units*,
first edition. RP identifies *Recommended Practice for Site Specific Assessment
of Mobile Jack-Up Units*, first edition; C identifies its commentary clauses.
The references below follow the supplied implementation checklist. They locate
assessment topics, without claiming that the research equations or illustrative
criteria implement the standards. No standard text, table payload or figure
values are reproduced. Standard tabulated values must remain explicit user
inputs; the demonstrator supplies none. Exact table/figure identifiers not
provided in the checklist remain OPEN rather than being inferred.

| Clause references | Demonstrated check and boundary | Uncovered checklist items |
| --- | --- | --- |
| G 5.3.1-5.3.3; RP 3.1.2, 8.3.1.1-8.3.1.4 | Raw per-leg reaction versus supplied preload; four-stage history, full-load duration and trailing settlement window with explicit illustrative controls. | **OPEN:** factored Vd versus 0.90 P; horizontal-demand applicability limits; unspecified standard preload hold rules and a qualified installation procedure. |
| RP 6.2.1-6.2.3; C6.2.1-C6.2.3 | SI input contract, finite/domain checks, circular area reconciliation and kPa-times-area conversion to kN. | **OPEN:** independent unit/provenance qualification, partially engaged area and actual spudcan geometry. |
| RP 6.2.1-6.2.3; C6.2.1-C6.2.3 | Common mudline datum; research depth refers to the lowest level of the widest cross-section, rather than the cone tip. | **OPEN:** actual tip-to-bearing-plane conversion and paired standard backflow corrections. |
| RP 6.2.1-6.2.3; C6.2.1-C6.2.3 | Each branch retains its declared research factor family; the layered peak evaluates its stress-dependent sand angle once per trial pressure. | **OPEN:** standard sand-angle adjustment, standard factor families, exact standard table/figure identifiers and their user-supplied values. Research factors are not substituted for standard factors. |
| RP 6.2.6.1-6.2.6.4, 8.3.5; C6.2.6.3, C8.3 | Descending branches warn independently of peak attainment. Peak depth, peak load, reduced interface load and static recovery depths are reported. No finite recovery is explicit. | **OPEN:** enumerating and flagging multiple roots, qualified lower support within the investigated extent, a supplied margin/plunge criterion and dynamic penetration. Selecting the shallowest root does not resolve alternative equilibria. |
| RP 6.2.6.1-6.2.6.4; C6.2.6.3 | Only named research equations execute; unresolved OCR expressions have no executable route or expression-input interface. | **OPEN:** resolving and qualifying those expressions before any future implementation; they must remain blocked until resolved. |
| RP 5.2-5.7, 7.1-7.3, 8.1.3-8.1.6, 8.5 | Common-origin force and biaxial moment equilibrium; example reactions derive from labelled synthetic inputs and illustrative environmental actions. | **OPEN:** qualified reaction provenance, complete concurrent load vectors and joint environmental cases, dynamics, second-order treatment, member capacity and holding-system scope. Synthetic reaction inputs must remain labelled synthetic. |
| RP 8.2.1-8.2.3 | Signed contact reactions are reported for the supplied geometry. No soil moment credit is taken. | **OPEN:** testing every candidate overturning axis, displaced restoring geometry, the 0.95 resistance treatment and supporting evidence before any soil moment credit. Contact alone does not perform these checks. |
| RP 6.3.3-6.3.5, 8.3.1.5-8.3.4 | Concurrent scalar horizontal force is compared with independently supplied sliding resistance; the static Hs/Tp grid reports failed checks and rejects re-entry. | **OPEN:** drained/undrained resistance factors, combined-load envelope intersections from the still-water point and qualified fixity/displacement assessment. The Hs/Tp grid is not a foundation interaction envelope. |

Fixity or displacement demands require escalation to a qualified foundation and
structural assessment before installation decisions. The air-gap note is a
qualitative mitigation only. This partial screen does not establish complete
site approval, even when every implemented inequality and hold check passes.

## Worked result and verification

The generic footing has a calculated peak of 60.9 MN and interface resistance of
55.7 MN. Its illustrative preload is 45.0 MN, below both. The zero-environment
reaction is 37.5 MN per leg. The synthetic height grid remains admissible through
2.25 m and first fails the preload comparison at 2.5 m. These are
illustrative numerical results, not site limits.

| Check | Evidence | Interpretation |
| --- | --- | --- |
| Uniform clay | Independent q = 6.34 su test; gradient term tested separately | Reference half-space arithmetic, without deep installation corrections. |
| Uniform sand | Independent circular-plate equations at three embedments, including pressure-to-force conversion through the homogeneous penetration branch | Checks published circular factors and arithmetic; does not extrapolate the layered calibration or qualify conical penetration. |
| Layered curve and recovery | Independent decimal expectations for pressure and load at seven depths, sub-peak equilibrium, falling/rising connections, peak recovery, preload recovery and constant-strength clay | Checks equations 4–14 and assumed connections without deriving expectations from production functions; static depth is not a dynamic run. |
| Frozen checkout | Git Windows-style text filters preserve all four frozen files; deliberate CRLF tampering still raises a digest error | Linux filter probe covers checkout conversion; native Windows execution remains untested. |
| Environmental inputs | Frozen-source digests, annual NOAA replay, independent Gumbel moments/quantiles and injected existing client transports | Checks reproducibility and propagation into loads; qualified ERA5, depth and joint extremes remain open. |
| Gulf Site 3 | Research peak 46.6 MN; calculated 46.5 MN, a 0.3% difference. Published preload 35.6 MN does not reach the calculated peak. | Numerical comparison agrees within the 1.0% test tolerance; no installation acceptance is inferred. |
| Gulf Site 2 | Published punch-through and final depth 15.8–18.5 m at 53.4 MN preload; calculated peak 60.0 MN and static peak-recovery depth 11.1 m | **Disagreement:** the earlier formulation does not trigger at the published preload and underestimates recovery depth. This is a documented false negative, not validation success. |
| Equilibrium | Independent vertical force and biaxial moment sums | Checks sign conventions, unequal reactions and the centroid load case. |
| Boundaries | Invalid numbers, geometry, layer ratios, preload, missing input, digest mismatch and grid re-entry | Rejects incomplete or out-of-domain evaluation. |
| Existing operation envelope | Existing motion-envelope regression suite | Retains the existing floating-vessel calculation while adding a separate static entry point. |

The Site 2 test preserves the disagreement rather than calibrating the method to
an observation. Hossain's later paper evaluates subsequent formulations and also
records method limitations. Agreement at one case and arithmetic tests cannot
qualify a penetration model across layered Gulf soils.

## Remaining work

Supply qualified ERA5 snapshots and numerical GEBCO depth under suitable reuse
terms, qualify the illustrative extreme-value fit and supply a joint-loading analysis. Audit observation
coverage, station changes, units and time conventions; the annual sample maxima
alone are insufficient. Provide a colocated geotechnical investigation, actual
unit geometry, mass distribution, preload procedure, hydrodynamic members and
combined foundation resistance. Extend the circular arithmetic reference with
field comparisons, and replace the illustrative post-peak curve
where engineering use is intended. Independent engineering review remains required.
