# B1528 SIROCCO Static Yaw-Moment Report

This report supports [workspace-hub #2570](https://github.com/vamseeachanta/workspace-hub/issues/2570) using the completed [#2569 source pack](https://github.com/vamseeachanta/workspace-hub/issues/2569).

## Traceability links

- Source pack issue: [workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569)
- Static yaw report issue: [workspace-hub #2570](https://github.com/vamseeachanta/workspace-hub/issues/2570)
- Reusable yaw-moment issue: [workspace-hub #2564](https://github.com/vamseeachanta/workspace-hub/issues/2564)
- Durable report page: [b1528-sirocco-yaw-moment-report.md](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-yaw-moment-report.md)
- Generated Markdown report: [b1528_sirocco_yaw_moment_report.md](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/b1528_sirocco_yaw_moment_report.md)
- Generated HTML report: [b1528_sirocco_yaw_moment_report.html](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/b1528_sirocco_yaw_moment_report.html)

## 2.5 kn ±1° operating-point table

| Mode | Speed (kn) | Rudder (deg) | Rotation | Yaw moment (kN-m) | Notes |
|---|---:|---:|---|---:|---|
| workbook_regression | 2.5 | -1.0 | stbd | -98.467815 | legacy_barrass_0_6_lbp |
| digitalmodel_static_yaw | 2.5 | -1.0 | not_applicable | 339.513315 | legacy_0_6_lbp_mapped_for_comparison_only |
| workbook_regression | 2.5 | 1.0 | port | 112.158527 | legacy_barrass_0_6_lbp |
| digitalmodel_static_yaw | 2.5 | 1.0 | not_applicable | -339.513315 | legacy_0_6_lbp_mapped_for_comparison_only |

## Method boundary

Workbook-regression rows reproduce the evaluated workbook family: the workbook text mentions Ft, but the evaluated yaw-moment cell uses Fn via C23. Digitalmodel rows are separately labeled and map the legacy 0.6*LBP lever to x_rudder_from_cg_m for comparison only.

## Propeller rotation factor Cr

Cr is the legacy workbook propeller-rotation correction multiplier in F = beta * AR * V^2 * Cr. In the B1528 workbook-regression rows, port uses Cr=1.065 and starboard uses Cr=0.935 because the workbook applies a side-dependent empirical allowance for propeller/rudder inflow asymmetry. If the propeller is not rotating, or if the calculation intentionally excludes propeller-rotation correction, use Cr=1.0 so the base force is not amplified or reduced. This neutral Cr value does not model locked or freewheeling propeller drag/wake effects.

## Sample working example

Data point: `workbook_regression, 2.5 kn, +1 deg rudder, port rotation case`.

- Speed conversion: `V = 2.5 kn * 0.51444 = 1.28610 m/s`.
- Base force: `F = 600.0 * 44.939563 * 1.28610^2 * 1.065 = 47498.422 N`.
- Normal force: `Fn = F * sin(1.0 deg) = 828.962 N`.
- Yaw moment: `Mz = Fn / 1000 * 135.300 = 112.158527 kN-m`.

The HTML report charts this sample as a highlighted marker so the numeric point can be checked visually against the plotted workbook-regression curve.

This is not a full MMG simulation, not an incident reconstruction, not an IMO compliance assessment, and no class compliance conclusion is made.

## Interactive charts

Open `b1528_sirocco_yaw_moment_report.html` for interactive Plotly charts of yaw moment versus rudder angle, yaw moment versus speed, and the sample verification point.