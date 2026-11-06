# Rod-pump surface-card undulation analysis

## Summary

The surface-card humps are compatible with elastic rod-string ringing. Equal
intervals in time occupy less position near reversal because rod velocity
approaches zero. This explains the appearance without establishing pump health.
The new screenshot digitizer and timing bounds extend the existing `rod_pump`
package; the existing downhole solvers and design correlations are unchanged.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-10-19 | Added calibrated digitization, bounded timing and a worked surface-card example to make screenshot assumptions reviewable. |

## Scope and sources

Three slices cover the remaining surface-card work in #1855:

1. `rod_pump.digitization`: explicit pixel calibration, trace segmentation,
   branch CSV export and conversion to a closed polygon.
2. `rod_pump.surface_undulations`: harmonic position, velocity, acceleration,
   bounded position-to-time inversion and ideal string ringing.
3. `examples/domains/artificial_lift/surface_undulations_example.py`: the
   existing digitized card, corrected issue inputs and an image-to-CSV example.

The method sources are:

- *University Physics Volume 1*, 2016, [section 15.1, Simple Harmonic Motion](https://openstax.org/books/university-physics-volume-1/pages/15-1-simple-harmonic-motion):
  harmonic displacement and its time derivatives.
- The same textbook, [section 17.4, Normal Modes of a Standing Sound Wave](https://openstax.org/books/university-physics-volume-1/pages/17-4-normal-modes-of-a-standing-sound-wave):
  the quarter-wave node/antinode eigencondition. Applying the same boundary
  eigencondition to an ideal uniform longitudinal bar is a model assumption.
- The repository's existing `rod_pump/kinematics.py`,
  [rod-pump analysis guide](rod-pump-rp11l.md) and screenshot `digitize.py`
  method (2026): taper-factor definition, ringing interpretation and calibrated
  trace extraction. No new licensed-standard values or limits are introduced.
- Issue #1855, corrected data and uncertainty discussion (2026), and #1860:
  example inputs and remaining data gaps. Earlier claims about surface-stroke
  efficiency, gearbox rating and unequal peak intervals were withdrawn.

The two textbook pages were opened successfully during implementation. Their
content supplies analytical relations, not a calibrated rod damping model.

## Equations and assumptions

For surface stroke `S`, speed `N` in cycles/minute and time `t` in seconds:

```text
omega = 2*pi*N/60
x = S/2 * (1 - cos(omega*t))
v = S/2 * omega * sin(omega*t)
a = S/2 * omega² * cos(omega*t)
t_up(x) = acos(1 - 2*x/S)/omega
t_down(x) = 60/N - t_up(x)
```

These are the translated harmonic relations from section 15.1, not an exact
crank-pitman-beam geometry. A real unit requires measured timing or geometry.
For measured position `x ± e`, invert `[max(0,x-e), min(S,x+e)]` at both ends.
This produces finite deterministic bounds even at stroke reversal. It avoids
using the local `e/v` approximation where velocity vanishes. Adjacent interval
bounds are `[max(0,t_next_low-t_prev_high), t_next_high-t_prev_low]`.
They are worst-case bounds, not confidence intervals; correlated calibration
errors and crank-geometry errors are not estimated by this calculation.

For a uniform elastic bar, the ideal longitudinal wave equation is
`u_tt = c²*u_zz`. A fixed displacement at one end and a free-force condition
at the other give `u(0)=0`, `u_z(L)=0`, so the fundamental wavenumber is
`pi/(2L)`. Section 17.4 supplies the same node/antinode boundary eigencondition:
`lambda=4L`, `T=4L/c`. With externally supplied frequency taper factor `Fc`:

```text
T = 4*L/(c*Fc)
No = 60/T
speed ratio = N/No
oscillations per half stroke = (30/N)/T
```

`L`, `c` and `Fc` are required inputs. No material speed, damping, acceptance
coefficient, phase trigger or resonance cutoff is inferred. A finite real rod
string has coupled boundary conditions, fluid loading, friction and damping;
this ideal calculation screens timing only. A `COMPATIBLE` result means the
predicted period lies in every supplied interval bound. `INCOMPATIBLE` calls
for checking inputs and assumptions, not a fault label. Fewer than two selected
peaks returns `NOT_EVALUATED`.

## Screenshot workflow

`AxisCalibration(pixel0, value0, pixel1, value1)` defines the affine map
`value = value0 + (pixel-pixel0)*(value1-value0)/(pixel1-pixel0)`.
Two tick locations and values are required for each axis. The crop uses the
original image coordinate frame; row coordinates normally increase downward.
The image API requires inclusive RGB bounds and a maximum within-run pixel
gap. These settings must be measured from the image; there are no tuned defaults.

Each column is split into contiguous trace runs. The mean row of each run
maps to load; higher/lower load becomes the nominal upstroke/downstroke branch.
This interpretation requires operator confirmation. It does not identify
stroke direction from pixels and is unsuitable for self-crossing cards without
additional interpretation. Single-run reversal columns, missing columns and
ambiguous multiple traces are reported as excluded. No interpolation is done.
`allow_envelope=True` explicitly admits columns with more than two traces;
`run_counts` retains that multiplicity. An outer envelope is not a single stroke.

`closed_card()` joins retained samples with straight lines and closes the two
branches at their retained endpoints. Inspect `excluded_columns`, trace
multiplicity and endpoint coverage before treating its area as full-cycle work.
The method does not remove grids or labels automatically, stitch screenshots,
propagate load-calibration uncertainty or implement automatic peak selection.

## Worked example: inputs, method and answer

The corrected issue supplies a reported uniform rod length of 4200 ft,
surface stroke 41 in and speed 6.4 cycles/minute. The steel wave speed
16300 ft/s is the issue's stated assumption, not a measured property of this
string. `Fc=1` represents the reported uniform string. Selected upstroke
peak positions are 7.5, 23.5 and 35.0 in, with the issue's revised ±1.5 in
peak-location error. Pump depth is not substituted for rod length.

Run using the existing CSV and an explicit output directory for the synthetic
image demonstration. Set `OUTPUT_DIR` to a writable scratch folder first:

```bash
CARD=$(git ls-files '*dynacard_digitized.csv')
PYTHONPATH=src python examples/domains/artificial_lift/surface_undulations_example.py \
  "$CARD" --image-output "$OUTPUT_DIR"
```

The calculation gives a ringing period of 1.03 s and frequency of 58.2
cycles/minute. Pumping speed is 11.0 % of the natural frequency, giving 4.5
oscillations per half stroke. These are ideal oscillations, not a guaranteed
integer count of visible humps. The issue describes four clear humps; visibility
and endpoint phase affect that integer count. The selected peak intervals are compatible
with that period under the stated position-error bounds. Slowing the unit
increases the time available for ringing without changing its ideal period.
The existing design-constant route uses a rounded frequency coefficient;
this example consistently uses the supplied wave speed instead.

The CSV covers 0.5 to 40.0 in, short of both ends of the stated stroke. Its
sampled polygon area is 69950.0 in-lb, corresponding to sampled-work power
of 1.13 hp at the stated speed. These use the existing card metrics and
horsepower conversion. Straight endpoint closure and the screenshot envelope
limit their interpretation as full-cycle work. Absolute load calibration
remains unresolved; no load-datum conclusion is inferred from pixels alone.

The optional image example creates an invented raster with `x=2*column` and
`load=100-10*row`. Thick trace rows 1–2 and 7–8 give centroid loads 85 and 25.
Its width is 8, so its polygon area is `8*(85-25)=480`. These synthetic values
are arithmetic fixtures, not operating settings or measured field data.

## Validation and way forward

Tests use independent hand calculations written in their docstrings: affine
pixel mapping and rectangle area, harmonic quarter-cycle values, exact inverse
bounds, quarter-wave periods and a composite-trapezoid sum of the existing CSV.
They also cover excluded columns, explicit envelope consent, reversed axes,
crop coordinates, nonfinite inputs and unresolved engineering checks.

`pump_fillage`, `gearbox_rating` and `downhole_diagnosis` remain explicitly
`NOT_EVALUATED`. Fillage requires plunger travel, runtime and fluid-volume
basis. Gearbox loading requires unit geometry, counterbalance and rated torque;
a prime mover designation does not supply them. Downhole diagnosis requires a
validated inversion and confirmed string, load calibration, tubing and damping
inputs. Raw timestamped load/position data is preferable to screenshot timing.
These are the later data-dependent slices; no operating change is prescribed.
