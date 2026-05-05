# B1528 SIROCCO Time-Trace Benchmark Report

This page documents the [workspace-hub #2571](https://github.com/vamseeachanta/workspace-hub/issues/2571)
B1528 SIROCCO preliminary time-trace workflow in `digitalmodel`.

## Traceability links

| Item | GitHub link |
|---|---|
| Source pack issue | [workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569) |
| Static yaw report issue | [workspace-hub #2570](https://github.com/vamseeachanta/workspace-hub/issues/2570) |
| Time-trace report issue | [workspace-hub #2571](https://github.com/vamseeachanta/workspace-hub/issues/2571) |
| Time-trace generator | [`b1528_sirocco_time_trace.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_time_trace.py) |
| Packaged input YAML | [`b1528_sirocco_time_trace.yml`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/b1528_sirocco_time_trace.yml) |
| Generated Markdown report | [`b1528_sirocco_time_trace_report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/time_trace/b1528_sirocco_time_trace_report.md) |
| Generated HTML report | [`b1528_sirocco_time_trace_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/time_trace/b1528_sirocco_time_trace_report.html) |
| Master calculation review | [`rudder-and-ship-force-calculation-review.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/rudder-and-ship-force-calculation-review.md) |

## Scope

The workflow runs a first-order Nomoto yaw-rate/heading integration with rudder-local inflow feedback:

```text
v_R = x_R * r
beta_R = atan2(-x_R * r, U)
alpha_R = delta_cmd - beta_R
U_R = sqrt(U^2 + v_R^2)
r_dot = (K * alpha_R - r) / T
psi_dot = r
x_dot = U * cos(psi)
y_dot = U * sin(psi)
```

Rudder force and yaw moment are computed as diagnostics only from `U_R` and `alpha_R`. They are **not** fed back into `r_dot`; this avoids double-counting a direct moment balance and Nomoto `K/T` response in the same preliminary model.

## Propeller rotation factor `Cr`

`Cr` is not applied in this Nomoto time-trace workflow. It belongs to the
B1528 static workbook-regression formula `F = beta * AR * V^2 * Cr`, where the
legacy workbook uses `Cr=1.065` for the port rotation case and `Cr=0.935` for
the starboard rotation case. The time-trace diagnostics instead use the reusable
`digitalmodel` static-yaw rudder-force model.

If the workbook-regression formula is run for a non-rotating propeller, or for
a case where no propeller-rotation correction is intended, the applicable value
is `Cr=1.0`. That neutral multiplier means no rotation-induced amplification or
reduction; it does not model locked or freewheeling propeller drag/wake effects.

## Packaged input

Packaged YAML: `src/digitalmodel/naval_architecture/data/b1528_sirocco_time_trace.yml`

Key inputs:

| Field | Value |
|---|---:|
| Speed | `2.5 kn` |
| Default rudder command | `+1 deg` |
| LBP | `225.5 m` |
| Rudder x from CG | `-135.3 m` |
| Rudder area | `44.93956319369854 m²` |
| Nomoto K | `0.018 1/s` |
| Nomoto T | `55 s` |
| Duration / dt | `600 s / 1 s` |

The Nomoto coefficients are marked as assumptions for source-gap sensitivity reporting; they are not B1528 telemetry calibration evidence.

## Generated outputs

Default output directory: `../acma-projects/B1528/yaw-and-time-trace/time_trace/` (project-specific outputs are owned by `acma-projects`, not `digitalmodel`).

- `b1528_sirocco_time_trace_results.csv`
- `b1528_sirocco_time_trace_results.json`
- `b1528_sirocco_time_trace_provenance.json`
- `b1528_sirocco_time_trace_report.md`
- `b1528_sirocco_time_trace_report.html`
- `b1528_sirocco_time_trace_manifest.json`

The HTML report includes interactive Plotly charts for:

- trajectory
- heading vs time
- yaw rate vs time
- effective rudder angle vs time
- diagnostic yaw moment vs time
- benchmark source-gap panel
- sample verification point

## Sample working example

Sample point for visual verification:

```text
Scenario: positive_rudder
Time step: first step, 0 s to 1 s
Initial local speed: U_R = 1.28610 m/s
Initial effective rudder angle: alpha_R = 1.000000 deg = 0.01745329 rad
Nomoto K/T: K = 0.018 1/s, T = 55 s
r_dot = (K * alpha_R - 0) / T = 0.0000057120 rad/s^2
yaw rate after 1 s = 0.000327273 deg/s
initial diagnostic yaw moment = -339.513315 kN-m
```

The generated HTML report highlights this first-step yaw-rate point in the
sample verification chart, so the plotted marker can be checked against the
worked value above.

## Benchmark boundary

The [#2569 benchmark source pack](https://github.com/vamseeachanta/workspace-hub/issues/2569)
contains narrative VDR/Rosepoint heading and speed anchors but no instrumented
x/y trajectory, and the notes include tug, current, anchor, and bank effects.
Therefore [#2571](https://github.com/vamseeachanta/workspace-hub/issues/2571)
reports a benchmark **source-gap** panel rather than fabricating a quantitative
overlay.

## Limitations

This workflow is:

- a preliminary first-order Nomoto time trace only;
- a source-gap/sensitivity report unless calibrated B1528 `K/T` data is later supplied;
- not a full MMG simulation;
- not an incident reconstruction;
- not an IMO compliance assessment;
- no class compliance conclusion.
