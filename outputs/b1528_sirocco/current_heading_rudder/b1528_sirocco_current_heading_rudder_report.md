# B1528 SIROCCO Rudder-Induced Current-Heading Force Component Report

Prepared for engineer review on 2026-05-09.

## Scope

rudder-induced SIROCCO current-heading/rudder force-component sweep at COG; ship-fixed X/Y/N components are derived by rotating local current-frame loads by heading offset; hull current loads, bank effect, tug loads, mooring-line stiffness, current-profile variation, and propeller race are excluded; no class compliance conclusion

This is not a validated oblique-current hull/rudder interaction model; hull current loads are excluded.

## Traceability links

- GitHub issue: [digitalmodel #598](https://github.com/vamseeachanta/digitalmodel/issues/598)
- Approved plan: [issue #598 plan](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/plans/2026-05-08-issue-598-sirocco-current-heading-rudder-force-components.md)
- Source pack issue: [workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569)
- Packaged input YAML: [b1528_sirocco_current_heading_rudder.yml](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml)
- Report generator: [b1528_sirocco_current_heading_rudder_report.py](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py)

## Design data

| Field | Value |
|---|---:|
| `lbp_m` | `225.5` |
| `yaw_lever_m` | `135.3` |
| `rudder_area_m2` | `44.93956319369854` |
| `rudder_span_m` | `9.0` |
| `ship_sog_kn` | `0.0` |
| `rho_kg_m3` | `1025.0` |
| `beta` | `600.0` |
| `prop_rotation_factor` | `1.0` |

## Analysis methodology and assumptions

```text
V=kn*0.51444; F=beta*A_R*V^2*Cr; alpha=delta-psi; X_local=F*sin(alpha)^2; Y_local=F*sin(alpha)*cos(alpha); X_ship=X_local*cos(psi)-Y_local*sin(psi); Y_ship=X_local*sin(psi)+Y_local*cos(psi); N=Y_ship*yaw_lever
```

- Heading/rudder effective-angle convention: `alpha = rudder_angle_deg - heading_offset_deg`.
- Local-to-ship transform: `X_ship=X_local*cos(psi)-Y_local*sin(psi)`, `Y_ship=X_local*sin(psi)+Y_local*cos(psi)`.
- 4.56 kn is an extra chart-default case outside the requested engineering sweep list.
- When rudder_angle_deg equals heading_offset_deg, alpha is zero and this rudder-induced component is zero; that is not total hull current load.

## Sweep coverage

- Requested engineering rows: `3528`.
- Extra 4.56 kn chart-default rows: `441`.
- Total generated rows: `3969`.

## Sample working example

Data point: `chart-default 4.56 kn, heading 0 deg, rudder +1 deg, Cr=1.0`.

- Speed conversion: `V = 4.56 kn * 0.51444 = 2.34585 m/s`.
- Base force: `F = 600.0 * 44.939563 * 2.34585^2 * 1.0 = 148381.324 N`.
- Ship-fixed COG components: `X_ship = 45.195 N`, `Y_ship = 2589.217 N`, `N_ship = 350.321028 kN-m`.

## Interpretation charts

Chart 1 plots rudder-induced ship-fixed `X_ship`, `Y_ship`, and resultant horizontal force versus heading for the selected current speed and rudder angle. Chart 2 is a signed rudder-induced yaw-moment heatmap over heading offset and rudder angle for the selected current speed. The HTML report also includes a selected-speed envelope summary panel that updates with the current-speed selector.

## Limitations

- rudder-induced moored-current loads only
- hull current loads, bank effect, tug loads, mooring-line stiffness, current-profile variation, and propeller race are excluded
- oblique-current extension is a first-cut visualization using alpha = rudder angle - heading offset
- not a validated oblique-current hull/rudder interaction model
- Cr=1.0 is a neutral no-propeller-rotation correction value
- no class compliance conclusion