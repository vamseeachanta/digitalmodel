# Generic jack-up preload weather-window analysis

The calculation screens persistent weather windows and regular-swell actions
for an assumed three-leg independent-leg jack-up. It supplies neither an
installation limit nor a site-specific climate probability. The default hourly
sequence is constructed: its monthly probabilities describe that scenario only.
The way forward is a qualified hourly series, operational limits and geometry,
followed by nonlinear hydrodynamic and foundation assessment where required.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-28 | Add persistent-window counts, explicit missing-data bounds and tide-dependent load screening to support preload planning. |

Table 1. Document issue history.

## Public environmental basis

[Assessment of Wave Data in West Africa for the Estimation of Wave Climate
(2026)](https://www.mdpi.com/2673-964x/6/1/8), sections 3 and 5, compares ERA5
and WAVERYS. The published summary supports periods near 25 s, a June–September
swell maximum and December–February minimum. These northern Gulf of Guinea
results provide regional context, rather than a colocated offshore Gabon record.
The indexed published text was inspected; direct full-page access returned HTTP
429. No hourly observations were recovered from that article.

[Seasonal Variability, Long-Term Trends, and Extreme-Value Statistics of ERA5
Wave Climate Along the West African Coast, 1940–2024
(2026)](https://www.mdpi.com/2073-4433/17/10/918), sections 3.5–3.6, supplies
additional offshore ERA5 regional context. A published summary cannot establish
weather-window persistence. The catalogue identity is `copernicus-cmems`; the
example does not claim that a reanalysis dataset was downloaded or calibrated.
Network access was available, but CDS authentication was not configured at the
time of the check. The [official access instructions](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=analysis_ready_data)
require a CDS API key for the wave-variable stores. An inspected hourly source
series with site and period coverage remains missing.

[Physical Forcing Induced Coastal Vulnerability along the Gulf of Guinea
(2019)](https://doi.org/10.4236/jep.2019.109071), section 1, describes energetic
waves reaching about 3–4 m during June–September. It does not establish a 4.0 m
significant-height return level at the study location. Accordingly **Hs = 4.0 m
is an assumed stress input**, rather than a measured design condition.

[Methodology for assessment and control of coastal erosion in West and Central
Africa (1989)](https://digitallibrary.un.org/record/777751), section 3.1.5 and
Table 3.1, lists historical spring tidal ranges of 1.6 m at Libreville and 2.0 m
at Cap Lopez. Only those selected facts are used. The example assumes a 2.0 m
range centred on mean sea level, giving tide offsets of −1.0, 0.0 and +1.0 m.
The centring and offshore transfer are assumptions, based on previous experience;
the historical port ranges are not a present offshore prediction. A local tide
datum, surge allowance and time history remain required for operational use.

Air gap, draft, sea state, tide, ballast and single-leg/all-leg preload are not
established by the dispatch-described source, referred to here only as **a 2026
public industry article on preload draft**. The example retains each as an
input range. No actual unit condition is inferred.

## Persistence definition and controls

Each timestamp is the start of a constant sea-state block of length `cadence_h`.
For a requested duration D, `ceil(D / cadence_h)` consecutive blocks must satisfy
both inclusive Hs and Tp limits. A duration just above one cadence therefore
requires two blocks. This convention does not resolve sub-cadence excursions.
Timestamps must be aware, chronological, unique and cadence-aligned; they are
converted to UTC. Windows cross month and year boundaries and are assigned to
their starting month. Calendar-month statistics pool all supplied years.

For each month let C denote starts with complete evidence, G wholly workable
starts, and U starts containing missing data. The reported probability is G/C,
conditional on complete evidence. Missing-data bounds are G/(C+U) and
(G+U)/(C+U). Missing rows, `None` and NaN cannot earn working credit. A zero
denominator returns `None`, distinct from a measured zero. Windows extending
past the record end are censored and counted separately; no window is extrapolated.
There is no inference outside the first-to-last timestamp span. These are
descriptive frequencies for a sampled start, not independent trials, confidence
intervals, or the chance of finding at least one window in a month.

The inherited `PreloadCriteria.minimum_hold_h` supplies a 1.0 h floor. No
published holding-time range is established by that module. Holds of 1.0, 3.0
and 6.0 h and a further 2.0 h operational allowance are assumed, based on previous
experience. The allowance represents unspecified preparation/loading/discharge
time; actual sequence duration must replace it. The example limits are Hs at
most 1.0 m for hull-in-water and 1.5 m for hull-elevated, with Tp between 5.0 and
25.0 s. They are assumed operational inputs, not qualified equipment limits.

All assumed input ranges and selected load inputs are emitted in the results.
Ballast and preload mode are recorded but are not converted to leg reactions.
The synthetic weather uses an hourly leap year, an assumed five-day repeating
pulse, an August seasonal maximum and Tp from 12.0 to 25.0 s. The deterministic
pattern exercises persistence without claiming an observed storm distribution.

## Load method and limits

All elevations use mean sea level; the seabed and supported hull remain fixed.
Instantaneous depth is mean depth plus tide. Draft is the positive part of tide
minus hull-bottom elevation, capped at hull depth. Box buoyancy is density ×
gravity × length × breadth × draft. An immersed hull following the tide freely
requires a different boundary condition; that motion is not modelled.

The caller supplies **individual regular-wave height H**, distinct from Hs. The
example assumes H/Hs = 1.8 within an assumed range 1.5–2.0. Periods of 12.0 and
25.0 s exercise the state-limit and stress cases; the latter does not assert that
the largest height and longest period are an observed joint event.

The linear dispersion equation is solved for k:
`omega² = g k tanh(k h)`. For amplitude a = H/2 and y measured above the seabed,
horizontal velocity amplitude is `a omega cosh(k y) / sinh(k h)`.
Quadratic drag and inertia are integrated over the still-water leg column:

```text
J = (a omega)² [h / (2 sinh²(kh)) + coth(kh) / (2k)]
D = rho Cd diameter J / 2
I = rho Cm area a omega² / k
peak = I                         when I >= 2D
peak = D + I²/(4D)               otherwise
```

These expressions retain the phase difference between velocity and acceleration.
Three times the per-leg peak is labelled the sum of peaks; without leg positions
and wave phase it is not a simultaneous total action.

Hull forces integrate undisturbed incident pressure over opposite box end faces
and the bottom, including their phase separation along hull length. They exclude
diffraction, radiation and body motion. Static buoyancy and oscillatory vertical
force are separate outputs and are not added as concurrent maxima. A dry hull
returns zero only above the regular-wave crest. Intermittent bottom contact or
deck wetting returns unavailable hull forces, retaining the contact flag and leg
screen. Such cases require an impact/nonlinear model rather than a zero load.

For the assumed stress case Hs = 4.0 m and H/Hs = 1.8, the regular-wave crest
is 3.6 m above still water. At the assumed +1.0 m tide, its elevation is +4.6 m
relative to mean sea level, above the assumed elevated hull bottom at +1.0 m.
The dry-hull clearance criterion therefore fails in this case and hull forces
are withheld. Revise the operational wave limit or hull elevation/draft, then
repeat the contact and load assessment; weather persistence alone does not
establish a workable installation sequence.

SNAME 5-5A Rev 3 (2008), clauses 4.3 *Hydrodynamic Forces*, 4.4 *Wave Theories
and Analysis Methods*, 4.6 *Leg Hydrodynamic model* and 4.7 *Hydrodynamic
Coefficients for Leg Members* locate the assessment topics. ISO 19905-1:2023,
clauses 6.4 *Metocean data*, 7.3.2 *Hydrodynamic model* and 7.3.3 *Wave and current
actions* provide corresponding context. The ISO clause titles are visible in
the [public contents preview](https://cdn.standards.iteh.ai/samples/iso/iso-19905-1-2023/4ddd0017f5904dd5b0d6f63d08f8857e/iso-19905-1-2023.pdf).
No standard-derived coefficient or installation criterion is implemented, and no
compliance conclusion follows. Source tables and sections are not reproduced.

Equivalent cylinders are not a lattice-leg model. Current, wind, wave stretching,
soil response, structural dynamics, slamming and green water are excluded. Linear
theory is unqualified for the assumed long-period/shallow-water cases; a trough
below the seabed is rejected, but that guard is not a validity criterion. Even
fully immersed incident-pressure results require diffraction and nonlinear-wave
review before engineering use. Tests establish mathematical agreement with hand
integrals, not field validation.

## Offline example and retained evidence

```bash
PYTHONPATH=src python examples/domains/geotechnical/jackup_preload_window.py OUTPUT
```

The output directory receives `preload_window_results.json`,
`monthly_preload_windows.plotly.json` and `preload_window_inputs.json`. No network
or licensed solver is used. The Plotly specification carries its assumed or
supplied-unqualified status in its title; percentages are displayed to one
decimal place. Counts and missing-data bounds remain in the results. The retained
input bytes have a SHA-256 digest recorded in the results; verify it before reuse.
The files are local calculation outputs, not a backup or publication.

For a supplied series, pass `--observations SERIES.json --sha256 EXPECTED`
and optionally `--cadence-h HOURS`. A mismatched digest fails before calculation.
The input object requires nonempty `source`, `version`, `dataset_id` and a
`samples` array. Each row requires ISO 8601 `timestamp` with UTC offset, `hs_m`
and `tp_s`; missing readings use JSON `null`. Units are metres and seconds.
Source metadata is retained; a matching digest does not establish scientific
qualification, source rights or site applicability. The outputs retain
`qualified_for_installation: false` for both assumed and supplied data.
Supplied source bytes are also retained unchanged as `preload_window_source.json`,
with their digest in `basis.sha256`; the normalized input copy uses UTC timestamps.
