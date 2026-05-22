# B1528 SIROCCO Current/Rudder Force Review — Issue #2760 Report

Prepared for engineer review on 2026-05-09.

## Scope

SIROCCO issue #2760 current/rudder force-component review at COG; rudder-induced ship-fixed X/Y/N components are derived by rotating local current-frame loads by heading offset, and generic/reference OCIMF tanker-current review loads are estimated separately from the licensed off-repo workbook route; bank effect, tug loads, mooring-line stiffness, current-profile variation, propeller race, and class compliance conclusion are excluded

This includes first-cut hull-current + rudder component sums for comparison only; it is not a validated whole-vessel current-load, oblique-current hull/rudder interaction, mooring, tug, bank-effect, or compliance model. The current terms use a generic/reference OCIMF tanker-current basis resolved through the approved licensed off-repo workbook route, not vessel-specific SIROCCO current-coefficient curves.

## Traceability links

- GitHub issue: [workspace-hub #2760](https://github.com/vamseeachanta/workspace-hub/issues/2760)
- Approved plan: [issue #2760 plan](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/plans/2026-05-20-issue-2760-b1528-sirocco-force-review-revision.md)
- Source pack issue: [workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569)
- Packaged input YAML: [b1528_sirocco_current_heading_rudder.yml](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml)
- Report generator: [b1528_sirocco_current_heading_rudder_report.py](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py)

## Design data

| Field | Value |
|---|---:|
| `lbp_m` | `225.5` |
| `beam_m` | `32.26` |
| `draft_m` | `12.2` |
| `water_depth_m` | `100.0` |
| `water_depth_to_draft_ratio` | `8.196721311475411` |
| `yaw_lever_m` | `135.3` |
| `rudder_area_m2` | `44.93956319369854` |
| `rudder_span_m` | `9.0` |
| `ship_sog_kn` | `0.0` |
| `rho_kg_m3` | `1025.0` |
| `beta` | `600.0` |
| `prop_rotation_factor` | `1.0` |
| `ocimf_current_cx_basis` | `interpolated OCIMF loaded-tanker A9 longitudinal-current curve from approved workbook route` |
| `ocimf_current_cm_basis` | `interpolated OCIMF loaded-tanker A11 yaw-moment curve from approved workbook route` |

## Analysis methodology and assumptions

```text
V=kn*0.51444; F=beta*A_R*V^2*Cr; alpha=delta-psi; X_local=F*sin(alpha)^2; Y_local=F*sin(alpha)*cos(alpha); X_ship=X_local*cos(psi)-Y_local*sin(psi); Y_ship=X_local*sin(psi)+Y_local*cos(psi); N=Y_ship*yaw_lever
```

- Heading/rudder effective-angle convention: `α = rudder_angle_deg - heading_offset_deg`.
- Local-to-ship transform: `X_ship=X_local*cos(psi)-Y_local*sin(psi)`, `Y_ship=X_local*sin(psi)+Y_local*cos(psi)`.
- First-cut generic/reference OCIMF tanker-current review loads: `q=0.5*rho*V^2`; `Cxc`, `Cyc`, and `Cxyc` are interpolated from the approved off-repo OCIMF workbook/provenance route; `Xc=q*(beam*draft)*Cxc`, `Yc=q*(LBP*draft)*Cyc`, `Nc=q*(LBP*draft)*LBP*Cxyc`. These report-specific coefficient lookups are not a reusable coefficient corpus.
- Component sums at COG in ship-fixed axes: `X_total=X_current+X_rudder`, `Y_total=Y_current+Y_rudder`, `N_total=N_current+N_rudder`.
- 3.08 kn is the issue #2760 default current speed; practical plots/tables are bounded to 0..4 kn.
- When rudder_angle_deg equals heading_offset_deg, alpha is zero and this rudder-induced component is zero; that is not total hull current load.

## Sweep coverage

- Requested engineering rows: `990`.
- Extra chart-default rows: `0`.
- Total generated rows: `990`.

## Heading/rudder schematic

Use the report schematic to read the geometry convention before reviewing components: ship-fixed +X is forward, +Y is port, heading offset `psi` is positive bow-to-port, rudder command `delta` is positive to port, and the rudder inflow angle is `α = delta - psi`.

## Sample working example

Data point: `issue #2760 default 3.08 kn, heading +5 deg, rudder +28 deg, Cr=1.0`.

- Speed conversion: `V = 3.08 kn * 0.51444 = 1.58448 m/s`.
- Base force: `F = 600.0 * 44.939563 * 1.58448^2 * 1.0 = 67694.127 N`.
- Rudder-induced COG components: `X_rudder = 8173.562 N`, `Y_rudder = 25155.637 N`, `N_rudder = 3403.557744 kN-m`.
- Generic/reference OCIMF current-review components at this same point: `X_current = -16548.971 N`, `Y_current = 120548.359 N`, `N_current = 6724.854215 kN-m`.
- Combined COG components: `X_total = -8375.409 N`, `Y_total = 145703.996 N`, `N_total = 10128.411959 kN-m`.

## OCIMF current vs rudder component sums

The report exposes individual generic/reference OCIMF current-review, rudder-induced, and total rows using `ocimf_current_*`, rudder `force_*`/`moment_*`, and `total_*` fields. Yaw moment is about the ship-fixed COG with positive bow-to-port convention.

## Interpretation charts
Chart 1 plots rudder-induced ship-fixed `X_ship` and `Y_ship` versus heading for the selected current speed and rudder angle. Chart 2 overlays OCIMF reference current-review, rudder, and total yaw moment about COG. The HTML report also includes selected-speed and selected-case summary panels that update with the controls.

## Limitations

- rudder-induced moored-current loads plus generic/reference OCIMF tanker-current review loads are reported for comparison
- hull-current terms use report-specific coefficients resolved through the approved off-repo workbook route, not ship-specific SIROCCO current-coefficient curves or certified coefficients
- bank effect, tug loads, mooring-line stiffness, current-profile variation, and propeller race are excluded
- oblique-current extension is a first-cut visualization using alpha = rudder angle - heading offset
- not a validated oblique-current hull/rudder interaction model
- Cr=1.0 is a neutral no-propeller-rotation correction value
- no class compliance conclusion
