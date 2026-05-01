# B1528 SIROCCO Static Yaw-Moment Report

This report supports issue #2570 using the completed #2569 source pack.

## 2.5 kn ±1° operating-point table

| Mode | Speed (kn) | Rudder (deg) | Rotation | Yaw moment (kN-m) | Notes |
|---|---:|---:|---|---:|---|
| workbook_regression | 2.5 | -1.0 | stbd | -98.467815 | legacy_barrass_0_6_lbp |
| digitalmodel_static_yaw | 2.5 | -1.0 | not_applicable | 339.513315 | legacy_0_6_lbp_mapped_for_comparison_only |
| workbook_regression | 2.5 | 1.0 | port | 112.158527 | legacy_barrass_0_6_lbp |
| digitalmodel_static_yaw | 2.5 | 1.0 | not_applicable | -339.513315 | legacy_0_6_lbp_mapped_for_comparison_only |

## Method boundary

Workbook-regression rows reproduce the evaluated workbook family: the workbook text mentions Ft, but the evaluated yaw-moment cell uses Fn via C23. Digitalmodel rows are separately labeled and map the legacy 0.6*LBP lever to x_rudder_from_cg_m for comparison only.

This is not a full MMG simulation, not an incident reconstruction, not an IMO compliance assessment, and no class compliance conclusion is made.

## Interactive charts

Open `b1528_sirocco_yaw_moment_report.html` for interactive Plotly charts of yaw moment versus rudder angle and speed.