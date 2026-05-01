# B1528 SIROCCO Time-Trace Benchmark Report

This report supports #2571 with a preliminary Nomoto time trace and rudder-local inflow feedback.

## Method boundary

preliminary first-order Nomoto time trace with rudder-local inflow diagnostics; rudder force and yaw moment are diagnostic only; source-gap sensitivity mode; not a full MMG simulation; not an incident reconstruction; not an IMO compliance assessment; no class compliance conclusion

The yaw-rate equation is Nomoto-driven. Rudder force and yaw moment are diagnostic only and are not fed back into `r_dot`, avoiding double-counting of Nomoto `K/T` and direct moment balance.

## Scenario metrics

| Scenario | Final heading (deg) | Final yaw rate (deg/s) | Advance (m) | Tactical diameter proxy (m) |
|---|---:|---:|---:|---:|
| positive_rudder | 3.620416 | 0.006221 | 771.161 | 23.705 |
| negative_rudder | -3.620416 | -0.006221 | 771.161 | 23.705 |
| zero_rudder | 0.000000 | 0.000000 | 771.660 | 0.000 |

## Benchmark source-gap panel

`benchmark-source-gap`: Extracted heading/SOG narrative anchors have no x/y coordinates and include tug/current/anchor effects; this report therefore shows source-gap/sensitivity context rather than fabricated trajectory overlay.

## Interactive charts

Open `b1528_sirocco_time_trace_report.html` for trajectory, heading, yaw-rate, effective rudder angle, yaw moment, and benchmark-source-gap panels.