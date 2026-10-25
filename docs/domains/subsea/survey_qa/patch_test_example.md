# Synthetic MBES patch-test analysis

The `digitalmodel.subsea.survey_qa` package estimates a roll bias from matched
reciprocal profiles and pitch, yaw and latency biases from measured feature
shifts. The solution is a planar geometry model for synthetic data. It provides
a starting point for implementing feature matching and a complete calibration
workflow; it does not qualify a real survey.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-19 | Added synthetic profiles, independent displacement equations and a worked example to make the geometry testable. |

## Basis and geometry

[NOAA Field Procedures Manual (2020), section 1.5.6.2, pages 26–28](https://nauticalcharts.noaa.gov/publications/docs/standards-and-requirements/fpm/field_procedures_manual_2020.pdf)
describes reciprocal flat-bottom lines for roll, reciprocal slope lines for
pitch, same-direction lines at different speeds for timing, and parallel offset
lines with overlapping outer swaths for yaw. The equations here are derived
from those geometries under the stated planar assumptions; they are not formulas
quoted from the manual. No recommended depth, slope, speed or acceptance value
is embedded in the package.

```text
Roll: common across-track frame, positive depth down
                     sensor
                       |
 y < 0 ----------------+---------------- y > 0
 apparent forward: z_f = d + y tan(r)
 apparent reverse: z_r = d - y tan(r)

Pitch: reciprocal nadir profiles across the same slope/feature
 forward displacement: +d tan(p)
 reverse displacement: -d tan(p)

Yaw: same heading on adjacent lines; feature in the overlap
 reported cross-track coordinates: y_first, y_second
 along-track feature error on each line: -y tan(h)

Latency: same heading over the same slope/feature
 along-track reporting error on each line: speed * latency
```

These definitions specify the signs of estimated biases. Instrument correction
signs need a separate reference-frame transformation. The roll coordinate `y`
is in one shared frame: do not supply starboard coordinates from each reciprocal
run without transforming them first. All shifts below are signed
`x_first - x_second` in the common along-track direction.

| Parameter | Derived relation | Inverse |
| --- | --- | --- |
| Roll `r` | `dz = a + 2*y*tan(r)` | `r = atan(b/2)`, where `b` is fitted slope |
| Pitch `p` | `dx = d*tan(p) - (-d*tan(p))` | `p = atan(dx/(2*d))` |
| Yaw `h` | `dx = -y_first*tan(h) - (-y_second*tan(h))` | `h = atan(dx/(y_second-y_first))` |
| Latency `t` | `dx = v_first*t - v_second*t` | `t = dx/(v_first-v_second)` |

Angles are returned in degrees; lengths and depths are metres, speeds metres per
second, latency seconds. The angular inverses use the principal tangent branch.
The yaw relation uses **reported** cross-track coordinates: an ideal rotated
feature has `dx_local = -y_true*sin(h)` and `y_reported = y_true*cos(h)`, giving
`dx_local = -y_reported*tan(h)` when its true along-track lever is zero.
The pitch relation assumes one common local depth at the feature. Neither
inverse replaces a full sonar observation model on a varying seabed.

The roll fit includes a nuisance vertical offset `a`. The straight-line least
squares solution is given in the
[NIST/SEMATECH e-Handbook, section 4.4.3.1, Least Squares (online edition accessed 2026-10-19)](https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd431.htm).
Minimising
`sum((dz_i-a-b*y_i)**2)` gives
`b = sum((y_i-mean(y))*(dz_i-mean(dz))) / sum((y_i-mean(y))**2)` and
`a = mean(dz)-b*mean(y)`. The factor two follows from opposing slopes, rather
than an empirical coefficient. The fit RMS is the square root of the mean
squared residual about that line. It measures model fit, not survey accuracy;
two distinct points determine a line exactly without providing redundancy.
`correct_roll` removes the opposing angular terms and preserves datum offsets.

## Reproducible example

The prescribed offsets follow #2955: roll 0.5 degrees, pitch 1.0 degrees,
yaw 1.5 degrees and latency 0.08 seconds. Depth, line coordinates and speeds
below are deliberately chosen synthetic inputs, not field recommendations.
The pitch and yaw shifts are analytical feature observations; only roll has a
swath generator in this increment. Noise, when supplied, is an explicit additive
array in metres; omitted noise is exactly zero.

```python
from math import isclose, radians, tan

from digitalmodel.subsea.survey_qa import (
    correct_roll,
    estimate_latency,
    estimate_pitch,
    estimate_roll,
    estimate_yaw,
    synthetic_roll_swaths,
)

# Synthetic design inputs; equations derived above from NOAA FPM (2020), 1.5.6.2.
depth = 20.0
first_speed, second_speed = 6.0, 2.0
pair = synthetic_roll_swaths(
    (-10.0, -5.0, 0.0, 5.0, 10.0), depth_m=depth, roll_deg=0.5,
)
latency = estimate_latency(
    (first_speed - second_speed) * 0.08,
    first_speed_m_s=first_speed, second_speed_m_s=second_speed,
)
pitch = estimate_pitch(2 * depth * tan(radians(1.0)), depth_m=depth)
roll = estimate_roll(pair)
yaw = estimate_yaw(
    20.0 * tan(radians(1.5)),
    first_across_track_m=-10.0, second_across_track_m=10.0,
)
corrected = correct_roll(pair, roll.roll_deg)
# Numerical demonstration precision only; no engineering acceptance limit.
for actual, prescribed in ((latency, 0.08), (pitch, 1.0),
                           (roll.roll_deg, 0.5), (yaw, 1.5)):
    assert isclose(actual, prescribed, rel_tol=1e-12, abs_tol=1e-12)
assert all(isclose(a, b, abs_tol=1e-12) for a, b in zip(
    corrected.forward_depth_m, corrected.reverse_depth_m, strict=True,
))
```

The example recovers the prescribed inputs at floating-point precision. This
round trip demonstrates API usage; the test suite separately checks literal
profile depths, offsets, fit residuals and signed shifts using hand calculations
written out in its docstrings. A wrong-sign roll correction doubles the mismatch
in the hand-calculated case.

## Limits and way forward

The package rejects nonfinite values, nonpositive depths/speeds, unequal profile
lengths and unidentifiable geometries. It does not validate acquisition metadata,
heading, sound velocity, time synchronisation or seabed suitability. The caller
must establish the flat-bottom, slope, matched-feature and line-direction
conditions described above. Near-identical speeds or lever arms can amplify
noise; no conditioning or uncertainty acceptance threshold is assumed.

IHO S-44 acceptance is **NOT_EVALUATED**: neither an order nor qualified
uncertainty inputs are present, and overlap RMS alone is not a total propagated
uncertainty assessment. No S-44 coefficients are stored. A later increment needs
an explicitly selected order, a cited edition and appropriate uncertainty inputs.

Later slices need sloping/feature swath generation with jointly injected offsets,
automatic feature matching, metadata pre-checks and a coupled correction test.
The example calls latency, pitch, roll, then yaw, following the requested workflow.
It does not demonstrate order dependence: the current inputs isolate the biases.
NOAA's cited section excludes timing from routine annual calibration and gives
pitch, roll, heading as its angular sequence. The extra timing step here is a
synthetic study of a supplied latency, not a recommendation for routine practice.
