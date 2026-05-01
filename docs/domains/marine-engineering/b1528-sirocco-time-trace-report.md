# B1528 SIROCCO Time-Trace Benchmark Report

This page documents the #2571 B1528 SIROCCO preliminary time-trace workflow in `digitalmodel`.

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

## Benchmark boundary

The #2569 benchmark source pack contains narrative VDR/Rosepoint heading and speed anchors but no instrumented x/y trajectory, and the notes include tug, current, anchor, and bank effects. Therefore #2571 reports a benchmark **source-gap** panel rather than fabricating a quantitative overlay.

## Limitations

This workflow is:

- a preliminary first-order Nomoto time trace only;
- a source-gap/sensitivity report unless calibrated B1528 `K/T` data is later supplied;
- not a full MMG simulation;
- not an incident reconstruction;
- not an IMO compliance assessment;
- no class compliance conclusion.
