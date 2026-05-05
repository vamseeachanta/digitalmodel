# B1528 SIROCCO Time-Trace Benchmark Report

This report supports [workspace-hub #2571](https://github.com/vamseeachanta/workspace-hub/issues/2571) with a preliminary Nomoto time trace and rudder-local inflow feedback.

## Traceability links

- Source pack issue: [workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569)
- Static yaw report issue: [workspace-hub #2570](https://github.com/vamseeachanta/workspace-hub/issues/2570)
- Time-trace report issue: [workspace-hub #2571](https://github.com/vamseeachanta/workspace-hub/issues/2571)
- Durable report page: [b1528-sirocco-time-trace-report.md](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-time-trace-report.md)
- Generated Markdown report: [b1528_sirocco_time_trace_report.md](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/time_trace/b1528_sirocco_time_trace_report.md)
- Generated HTML report: [b1528_sirocco_time_trace_report.html](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/time_trace/b1528_sirocco_time_trace_report.html)

## Method boundary

preliminary first-order Nomoto time trace with rudder-local inflow diagnostics; rudder force and yaw moment are diagnostic only; source-gap sensitivity mode; not a full MMG simulation; not an incident reconstruction; not an IMO compliance assessment; no class compliance conclusion

The yaw-rate equation is Nomoto-driven. Rudder force and yaw moment are diagnostic only and are not fed back into `r_dot`, avoiding double-counting of Nomoto `K/T` and direct moment balance.

## Propeller rotation factor Cr

Cr is the legacy workbook propeller-rotation correction multiplier in F = beta * AR * V^2 * Cr. In the B1528 workbook-regression rows, port uses Cr=1.065 and starboard uses Cr=0.935 because the workbook applies a side-dependent empirical allowance for propeller/rudder inflow asymmetry. If the propeller is not rotating, or if the calculation intentionally excludes propeller-rotation correction, use Cr=1.0 so the base force is not amplified or reduced. This neutral Cr value does not model locked or freewheeling propeller drag/wake effects. This time-trace report does not apply Cr because its rudder diagnostics use the reusable digitalmodel static-yaw force model, not the legacy workbook regression.

## Sample working example

Data point: `positive_rudder first time step`.

- Initial local speed: `U_R = 1.28610 m/s`.
- Initial effective rudder angle: `alpha_R = 1.000000 deg = 0.01745329 rad`.
- Initial Nomoto acceleration: `r_dot = (0.018000 * 0.01745329 - 0) / 55.000 = 0.0000057120 rad/s^2`.
- After `1.0 s`, calculated yaw rate is `0.000327273 deg/s`; the generated row reports `0.000327273 deg/s`.
- Initial diagnostic yaw moment is `-339.513315 kN-m`.

The HTML report includes a sample-verification chart that highlights this early yaw-rate data point.

## Scenario metrics

| Scenario | Final heading (deg) | Final yaw rate (deg/s) | Advance (m) | Tactical diameter proxy (m) |
|---|---:|---:|---:|---:|
| positive_rudder | 3.620416 | 0.006221 | 771.161 | 23.705 |
| negative_rudder | -3.620416 | -0.006221 | 771.161 | 23.705 |
| zero_rudder | 0.000000 | 0.000000 | 771.660 | 0.000 |

## Benchmark source-gap panel

`benchmark-source-gap`: Extracted heading/SOG narrative anchors have no x/y coordinates and include tug/current/anchor effects; this report therefore shows source-gap/sensitivity context rather than fabricated trajectory overlay.

## Interactive charts

Open `b1528_sirocco_time_trace_report.html` for trajectory, heading, yaw-rate, effective rudder angle, yaw moment, benchmark-source-gap, and sample-verification panels.