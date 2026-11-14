# Dynacard browser application

This free static application converts a surface dynamometer card to a pump card
and reproduces the default `DynacardWorkflow.run_full_analysis()` arithmetic in
`digitalmodel.marine_ops.artificial_lift.dynacard`. Both classifier outputs are
**HYPOTHESIS**, not established field diagnoses. Field accuracy is
**NOT_EVALUATED**: the classifiers were trained on synthetic generators. The
shape-only model's diagnostic-plate score is **5 of 8 literal**.

## Open and calculate

Open `index.html` directly from the filesystem, or serve this folder unchanged
from a static host. No build, server calculation, account, package installation,
or paid service is required. All script and stylesheet files must remain beside
the page. A current browser with JavaScript enabled is required.

The models are stored in six part scripts, `models-part-1.js` to
`models-part-6.js` (4,257,106 bytes together; the largest is 819,204 bytes),
joined by the 966-byte loader `models.js`. Each file is below the repository's
1,000 KB file-size limit.
The page needs no network after its files are on disk.

Example 1 is loaded and analysed automatically on page load. Its surface card,
computed pump card, results and diagnostic hypotheses are displayed immediately.
**Run analysis** recalculates the results after edits.

1. Load a CSV file, or choose one of ten bundled examples and select **Load example**.
2. Enter the pumping speed, pump geometry, fluid density, tubing pressure and
   steel rod tapers in surface-first order.
3. Select **Run analysis**. Editing an input hides the prior results until the
   analysis is run again. A rejected CSV clears the previously loaded card.

Results are formatted by unit: in 2 dp; lbf 0 dp with thousands separators;
% and psi 1 dp; lb/ft³ 3 dp; psi/ft 4 dp; bbl/day 1 dp; ft·lbf and h/day 0 dp.
**Download results (JSON)** retains full calculation precision and both model
outputs. **Download pump card (CSV)** exports `position_in,load_lbf` at full
precision, retaining the original position datum. Both downloads are generated
in page memory with Blob URLs and require no network.

Loading an example also restores its example well. The initial example well has
6.42 strokes/min, a 1.5 in pump at 4844 ft, fluid density 63.237 lb/ft³ and tubing
pressure 100 psi. The surface-first rod sections are 0.875 in × 1400 ft at
2.224 lbf/ft, then 0.750 in × 3350 ft at 1.634 lbf/ft; both use 30.5 million psi
modulus. The 4750 ft rod length differs from the 4844 ft pump depth by the
specified example basis; the application does not reconcile that mismatch.

## CSV format

The downloadable [template.csv](template.csv) contains the first example surface
card and a comment header describing units and sampling. The required header is
`position_in,load_lbf`. The worked-example column names
`surface_position_in,surface_load_lbf` are also accepted. Columns are
comma-separated; lines starting with `#` are comments.

```csv
# One full stroke; uniformly timed samples; start anywhere.
# Position in inches, surface load in lbf.
position_in,load_lbf
```

Supply at least four paired finite numerical samples and nonzero surface
stroke. Empty or nonnumeric cells, nonfinite values, inconsistent row lengths,
missing required columns and zero stroke are rejected. Sampling timestamps are
not inferred from position spacing: uniform time intervals remain an input
assumption. The periodic solver reads the last sample as phase zero repeated.
A CSV whose last row repeats the first row exactly is used as supplied; any
other CSV is read as unique phases and its first row is appended as the closing
row before solving, as the pump-card study does. Keeping or omitting the
closing row therefore gives the same pump card, and the loaded sample count
includes the closing row. Cards passed directly to the core as JSON, such as
the field fixtures below, are used as supplied, matching the Python workflow.

## Calculation basis

The JavaScript ports the Python default path, including the 200-node
Everitt–Jennings spatial march, coupling-inclusive density derived from rod
weight, separate upstroke/downstroke damping, strain-derived load, and
Savitzky–Golay filtering with polynomial endpoint interpolation (`mode='interp'`).
The filter uses a 21-sample window and polynomial order 5, with the reference's
short-card window adjustment. The default smoothed card supplies both the
calculations and the classifiers.

The outputs include surface stroke and polished-rod load extrema; pump stroke,
load extrema and card samples; four corner indices; fluid, upstroke and
downstroke loads; fillage; CPIP intake and discharge pressure; gross and net
displacement; runtime-adjusted theoretical displacement; two work quantities;
the reference buckling flag; and solver guard annotations.
**Polished-rod work per stroke (ft·lbf)** is the surface card's shoelace area
divided by 12 (`polished_rod_work_per_stroke`). **Pump-card work per stroke
(ft·lbf)** is the computed pump card's shoelace area divided by 12
(`pump_work_per_stroke`). Both are compared independently with the module's
card-area calculation. The full probabilities and
top three alternatives are returned for both models. The shipped model uses 19
features; the shape-only model uses 16 normalized projections. Tree evaluation,
feature scaling and softmax follow the shipped JSON documents.

The surface card is evaluated at the polished rod. The computed pump card is
evaluated at the plunger after the wave equation removes rod weight, stretch and
dynamics. Fillage is effective travel divided by gross plunger travel. Fluid
load is the upstroke mean load minus the downstroke mean load. The pump plot
subtracts its minimum position for display only; all calculations and classifier
inputs retain the original positions. Dashed lines show the two mean loads;
annotations identify gross stroke, effective travel, fillage and fluid load.

Results precede the limitations panel. The shape-only model's top three calls
and percentages and the shipped model's call and confidence are labelled
**HYPOTHESIS**. A loaded example also identifies its plate condition, source DOI
and crosswalk status: `literal` or `ASSUMED`, as recorded by the plate mapping.
Uploaded CSV cards do not inherit example provenance.

CPIP uses the reference's separate mixture calculation: oil 35° API, water
specific gravity 1.03, and water cut 50% when no well test is supplied. The page's
fluid-density input affects the wave equation, **not** this CPIP mixture basis.
The page assumes runtime of 24 h/day and reference pump efficiency of 100%.
The API retains supplied well-test and runtime information where the default
workflow uses it. These outputs are displacement, not measured production.

The buckling flag is triggered when minimum pump-card load is below −500 lbf,
exactly as in the reference adapter. Critical buckling capacity, neutral-point
depth and a depth-resolved compression assessment are not established.

References:

- Everitt, T. A., and Jennings, J. W. (1992), *An Improved Finite-Difference
  Calculation of Downhole Dynamometer Cards for Sucker-Rod Pumps*,
  SPE-18189-PA. This is the default solver's numerical basis.
- Gibbs, S. G. (1963), *Predicting the Behavior of Sucker-Rod Pumping Systems*,
  SPE-588-PA. This is historical wave-equation context; the legacy Gibbs
  implementation is not executed by this application.
- CC0 diagnostic plate, [DOI 10.17632/hnfwm9mjs6.1](https://doi.org/10.17632/hnfwm9mjs6.1).
  The ten examples retain sheet order and English glosses. Plate pump curves are
  scaled to the specified 40 in stroke and fluid-load basis, then transformed to
  surface cards with the reference forward model. They are synthetic surface
  examples, not measured field cards. `build_assets.build_examples()` reads the
  committed [results document](../../domains/artificial_lift/dynacard-examples/results.json)
  and preserves its surface samples, well basis and source metadata without
  reconstructing cards. Each loaded example links to its worked-example section.

## What this does not check

- Tubing anchoring: anchored tubing is assumed; unanchored tubing is not modelled.
- Deviation and side load: the application assumes a vertical well.
- Load-cell calibration: a calibration record is required to establish load accuracy.
- Timing: uniform samples are assumed; the timing source and sampling rate are unverified.
- Damping and friction calibration: 10 cP viscosity, 2.441 in tubing ID and the
  default damping calculation are used; field calibration is missing.
- Fluid properties: the entered density and the separate CPIP mixture are not
  validated against fluid samples.
- Rod materials other than steel: these are outside the port's supported basis.
- Pump depth versus rod length mismatch: the wave equation uses rod length;
  the pressure calculation uses pump depth. Their agreement is not established.
- Valve and plunger slip: leakage and slippage are not calculated.
- Production: displacement is not production. Runtime, well-test rate, leakage
  and volume-factor evidence are required for a production estimate.
- Classifier field accuracy: **NOT_EVALUATED**. Synthetic-generator training
  and a shape-only plate score of **5 of 8 literal** do not establish field accuracy.

Nonvertical deviation surveys, non-steel rods and unanchored tubing are rejected with an
explanation rather than sent through an unported solver branch. Legacy Gibbs
and time-domain finite-difference solvers, the missing-model threshold fallback,
alternative corner fallbacks and optional unsmoothed diagnostic-card mode are
not ported. The page does not compute gearbox torque, motor power, rod stress
capacity or the optional P2–P7 analyses, which the default workflow does not run.

The static follow-up panel lists the evidence an analyst would
request: fluid level or measured intake pressure; well-test rate and runtime;
anchor status; deviation survey; rod tally and pump depth; load-cell calibration
date; sampling rate and timing source; recent workovers; and gas/oil/water ratios.
An interactive follow-up is a later step.

## Reproduction evidence

The tests compare the ten examples and supported field fixtures with the Python
workflow. Per-sample position/load and scalar comparisons target a maximum
relative difference of `1e-6`; full probabilities target `1e-6` absolute, with
identical labels and corner indices. Differences below are measured against the
reference, not independent field validation. The generator updates this table
and `reproduction.json`; tests check numerical limits, not byte-identical measured
differences.

<!-- reproduction:start -->

| Example | Max pump-load difference (lbf) | Max pump-position difference (in) | Max scalar relative difference | Labels equal | Max probability difference | Polished-rod work difference (ft·lbf) | Pump-card work difference (ft·lbf) |
| --- | ---: | ---: | ---: | :---: | ---: | ---: | ---: |
| Normal operation | 2.260549081e-09 | 3.548272787e-13 | 4.211619095e-11 | yes | 2.220446049e-16 | 0.000000000e+00 | 6.488335202e-09 |
| Insufficient supply | 2.220986062e-09 | 3.552713679e-14 | 2.650754944e-12 | yes | 2.220446049e-16 | 0.000000000e+00 | 2.862179826e-09 |
| Gas interference | 2.279193723e-09 | 0.000000000e+00 | 8.382061185e-12 | yes | 1.734723476e-18 | 0.000000000e+00 | 4.239154805e-09 |
| Rod parting | 2.266915544e-09 | 5.373479439e-14 | 2.360233160e-12 | yes | 2.220446049e-16 | 0.000000000e+00 | 5.624315236e-09 |
| Tagging up | 2.205069904e-09 | 3.019806627e-14 | 9.797272129e-12 | yes | 6.776263578e-21 | 0.000000000e+00 | 5.502442946e-09 |
| Tagging down | 2.284195944e-09 | 2.273736754e-13 | 5.683533897e-12 | yes | 8.271806126e-25 | 0.000000000e+00 | 4.996763892e-09 |
| Traveling valve leakage | 2.280103217e-09 | 0.000000000e+00 | 7.649144582e-12 | yes | 3.231174268e-27 | 0.000000000e+00 | 6.510163075e-09 |
| Standing valve leakage | 2.281012712e-09 | 2.620126338e-14 | 2.164386796e-11 | yes | 2.220446049e-16 | 0.000000000e+00 | 6.481059245e-09 |
| Plunger out of barrel | 2.287833922e-09 | 1.127986593e-13 | 1.343616193e-12 | yes | 1.615587134e-27 | 0.000000000e+00 | 4.985849955e-09 |
| Sand production | 2.244178177e-09 | 0.000000000e+00 | 4.502234000e-12 | yes | 4.135903063e-25 | 0.000000000e+00 | 5.058609531e-09 |

Table 1. Measured Python/JavaScript reproduction differences for the ten examples.

For the ten saved surface cards replayed through live Python and JavaScript, the tolerances are `1e-6` of reference range for pump samples, `1e-6` of `max(abs(reference), 1)` for scalars and `1e-6` absolute for probabilities, and the measured maxima are 2.287833922e-09 lbf, 3.548272787e-13 in, 4.211619095e-11 scalar relative, 2.220446049e-16 probability absolute, 0.000000000e+00 ft·lbf polished-rod work and 6.510163075e-09 ft·lbf pump-card work, with both classification labels identical in all ten cases.

<!-- reproduction:end -->

Stored-result comparisons use relative tolerance `1e-8` because `results.json`
rounds to nine significant digits, together with the existing `1e-6` range
envelope for pump samples and `1e-6` absolute for fillage and load scalars;
both classifications require exact agreement. These checks run independently
of the live Python comparison above.

All ten stored examples satisfy the comparison. Example 4, Rod parting, now
stores the fluid load of its saved (rounded) surface card, 2935.33391 lbf; the
earlier stored value of 2950.083910 lbf came from the unrounded card, whose
top-left corner falls one sample later.

The field fixtures `field_well_alpha.json`, `field_well_bravo.json`,
`field_well_charlie.json` and `field_well_delta.json` are supported and compared
with the Python workflow. `field_well_echo.json` is rejected: its deviation
survey reaches 39.98° inclination, outside the application's vertical-well
basis. Explicit vertical surveys remain supported. Reproduction measurements
and the rejection reason are retained in `reproduction.json`.

The CSV can start anywhere in the cycle, as in the reference workflow. The
shipped feature extractor nevertheless splits projections at the first maximum
position, with its original endpoint fallback. Classification is therefore not
established as invariant to the sample origin; the application preserves that
algorithm instead of rephasing the input.

## Privacy and runtime dependencies

No data leaves the browser. The page has no third-party scripts, fonts, CDN,
analytics, cookies, remote API calls or persistent storage of card data. CSV
contents remain in page memory until the page is closed or reloaded. The
Content-Security-Policy blocks connection requests and form submission. The
bundled model and example assets are loaded as classic local scripts, so no
JSON request or ES-module server is needed. The documentation DOI link above
opens a separate source page only when deliberately followed.

## Rebuild and verify

From the repository root, select an existing Python environment containing the
repository's dependencies and Node on `PATH`. No dependency installation is
part of this application. The commands below use PowerShell syntax:

```powershell
$env:PYTHONPATH = 'src'
$env:PYTHONIOENCODING = 'utf-8'
python docs/apps/dynacard/build_assets.py
python docs/apps/dynacard/build_assets.py --check
python docs/apps/dynacard/build_reproduction.py
python -m pytest tests/apps/test_dynacard_app.py -q
python scripts/enforcement/check-engineering-register.py docs/apps/dynacard/README.md
node docs/apps/dynacard/test/page_smoke.cjs
```

`build_assets.py --check` rejects generated asset drift, including a stale part
file. The part scripts hold the two reference JSON documents as compact JSON,
with no indentation, cut into string pieces of at most 900,000 bytes; `index.html`
loads them in order before `models.js`, which joins and parses them and stops
with an error naming any missing part. Parsed equality with both module
documents, the page load order and the file-size limit are tested; no model
values are pruned.
`examples.js` and `template.csv` derive from the
committed worked-example `results.json`. `test/run_core.cjs` accepts a JSON file containing
`{card:{position,load},well:{...}}` or an array of requests and emits results JSON
to stdout. The Python suite skips only Node-dependent tests if Node is absent.

The optional browser smoke test uses an installed Chromium browser through a
debugging pipe, without packages or a web server. `BROWSER_BINARY` can select an
existing executable. It checks initial auto-run, work-value/card bindings,
example crosswalks, plot annotations, two plots, all ten example
analyses, valid and rejected CSV uploads, stale file-read races, input
invalidation, taper add/remove, console errors and page network requests.
`DYNACARD_SCREENSHOT` specifies an optional full-page PNG path; capture occurs
after the initial auto-run and before interaction tests.
