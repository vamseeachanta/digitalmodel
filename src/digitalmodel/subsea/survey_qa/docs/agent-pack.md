# Survey QA: synthetic multibeam patch-test analysis

Purpose: exercise signed roll, pitch, yaw and latency estimation with prescribed
synthetic footprints. No real survey data or raw-format reader is included.

## Entry points, inputs and units

Import from `digitalmodel.subsea.survey_qa`. Core geometry needs only the Python
standard library. Reporting also needs the existing house report dependencies.

| Object or function | Required inputs | Output |
| --- | --- | --- |
| `Acquisition` | Sensor offsets `(forward, starboard, down)` m; constant SVP `(depth_m, speed_m_s)` rows; paired sonar/navigation timestamps s; sync tolerance s | Validated metadata |
| `Line` | Ground start X and track Y m; direction `+1` or `-1`; positive speed m/s | Navigation line |
| `Seabed` | Reference depth m; X/Y slopes m/m; optional point `(X_m, Y_m, height_m)` | Plane and one shoaling point |
| `Offsets` | Signed roll/pitch/yaw deg and latency s; omitted components are zero by identity | Biases to inject or remove |
| `generate_swath` | Seabed, line, acquisition, unique beam offsets m, offsets, noise standard deviation m, seed | `Swath` with measured/truth XYZ and sensor origins m |
| `estimate_roll`, `estimate_pitch`, `estimate_yaw` | Two selected swaths with earlier biases removed | Signed injected angle deg |
| `estimate_latency` | Same-direction slope lines at different speeds | Signed injected latency s |
| `PatchTestLines` | Four pairs: `latency`, `pitch`, `roll`, `yaw` | Explicit test geometry |
| `estimate_patch` | `PatchTestLines`, common constant biases | `Offsets`, estimated in latency/pitch/roll/yaw order |
| `apply_corrections` | Swath and estimated offsets | Corrected swath, default inverse order |
| `DepthUncertainty` | Order, edition, `a_m`, dimensionless `b`, table reference | Caller-supplied TVU ceiling |
| `screen_overlap` | Two swaths, budget or explicit `None`, XY matching tolerance m | Matched/unmatched counts, maximum depth difference m, minimum ceiling m, worst ratio, source, status |

Ground X follows the forward line, Y is starboard, Z is down. Reciprocal lines
reverse both horizontal axes. Positive roll moves starboard soundings deeper;
positive pitch moves nadir soundings forward; positive yaw moves starboard
soundings aft. Positive latency advances position along travel. These are the
module's declared right-handed active rotations, not an instrument convention.

## Method and source basis

- [NOAA Field Procedures Manual (2020), §1.5.6.2, pp. 26–27](https://nauticalcharts.noaa.gov/publications/docs/standards-and-requirements/fpm/field_procedures_manual_2020.pdf):
  flat reciprocal roll lines, slope reciprocal pitch lines, same-direction
  two-speed timing lines and adjacent parallel target lines. The manual's
  angular sequence is pitch, roll, heading. Timing is a separate gross
  diagnostic; this synthetic workflow evaluates it first.
- [OpenStax, Precalculus 2e (2021), §5.4](https://openstax.org/books/precalculus-2e/pages/5-4-right-triangle-trigonometry)
  supplies the right-triangle sine, cosine and tangent ratios.
- [OpenStax, Introductory Statistics 2e (2023), §12.3](https://openstax.org/books/introductory-statistics-2e/pages/12-3-the-regression-equation)
  supplies the least-squares criterion; the normal-equation rearrangement is
  derived in the estimator docstring.
- `geometry.py` derives the right-handed matrices and their inverse. Injection
  is `T_latency Ry Rx Rz`; correction is latency, pitch, roll, yaw. Alternative
  complete orders are accepted only for the noncommutation demonstration.
- `estimators.py` derives OLS from the normal equations, reciprocal roll from
  `delta_m = 2 tan(roll)`, pitch from `dx = 2 D_observed tan(pitch)`, timing
  from `dx = direction delta_v latency`, and yaw from `tan(yaw) = -dx/dy`.
  The yaw denominator is the difference in observed sensor-relative transverse
  offsets. Track spacing would instead require an arcsine.
- [IHO S-44, edition 6.1.0 (October 2022), §3.2.3 and Table 1, p. 18](https://iho.int/uploads/user/pubs/standards/s-44/S-44_Edition_6.1.0.pdf):
  `TVU_max(d) = sqrt(a² + (b d)²)`. The user supplies the applicable order,
  edition, coefficients and evidence. There is no built-in order table.
  The report explicitly supplies Special Order values read from Table 1.

All four sources were opened from their publishers on 2026-10-22. The attempted
6.2.0 URL returned HTML and supplies no numerical basis. Public sources are
recorded by URL in report provenance; wiki citation authority is not invented.

## Preconditions and failures

Sensor offsets and a constant sound-velocity profile must be present. Both
clocks must increase strictly, have equal lengths and agree within the required
user tolerance. Variable sound velocity is refused. The SVP is validated
metadata, not an input to acoustic ray tracing.

Roll requires flat feature-free terrain and reciprocal equal-speed tracks.
Pitch requires an along-track slope and reciprocal equal-speed tracks; latency
requires a slope and coincident tracks with the same direction and different
speeds. Pitch and latency require measured nadir profiles with overlapping depth
ranges. A cross-track slope alone does not supply along-track information.
Yaw requires same-direction equal-speed adjacent tracks with the same sampled
feature in their overlap. The target is explicitly associated; it is not found
by a peak picker. Unsupported geometry and nonfinite inputs raise `ValueError`.

## Output interpretation and limits

`WITHIN_SCREEN` and `EXCEEDS_SCREEN` compare matched depth differences to an
individual-sounding TVU ceiling. This is a consistency screen,
not an IHO crossline rule or a full 95 percent uncertainty assessment.
`NOT_EVALUATED` means the caller supplied no table evidence; differences and
coverage counts remain available. No overlap is an error. Nearest-point matches
are one-to-one, have no interpolation, and report unmatched counts. The matcher
is quadratic and intended for small synthetic sets.

Only two parallel headings, constant speed/biases, known planar suitability and
prescribed beam footprints are supported. The point target must fall exactly
on the chosen ping/beam grid. No motion history, refraction, footprint model,
confidence interval, datum reduction or operational qualification is supplied.

## Worked example and checks

`example.make_example(noise_std_m=0.002, seed=7)` builds eight lines with the
issue-prescribed biases: roll 0.5 deg, pitch 1.0 deg, yaw 1.5 deg, latency 0.08 s.
`estimate_patch` recovers them within chosen synthetic regression tolerances of
0.1 deg and 0.01 s; these are not field acceptance criteria. Independent hand
calculations and truth-blind tests live in `tests/subsea/survey_qa/`.

`report.write_example_report(output_dir)` uses `digitalmodel.report_pack` and
its reporting engine. The wrapper is
`examples/workflows/survey-qa-report-pack/build_report.py --output-dir <directory>`.
The rendered page is `docs/domains/subsea/survey-qa-patch-test.html`, with the
Markdown companion beside it. PDF rendering is explicitly off. All temporary
outputs belong in a caller-selected directory outside the source checkout.
