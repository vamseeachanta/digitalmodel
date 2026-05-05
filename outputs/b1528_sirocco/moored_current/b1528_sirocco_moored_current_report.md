# B1528 SIROCCO Moored-Current Rudder Force Component Report

Prepared for engineer review on 2026-05-06.

## Scope

rudder-induced moored-current force-component sweep at COG; current aligned with vessel centerline; hull current force, bank effect, tug loads, mooring-line stiffness, current-profile variation, and propeller race are excluded; no class compliance conclusion

## Traceability links

- Source pack issue: [workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569)
- Moored-current report issue: [workspace-hub #2642](https://github.com/vamseeachanta/workspace-hub/issues/2642)
- Static yaw report issue: [workspace-hub #2570](https://github.com/vamseeachanta/workspace-hub/issues/2570)
- Time-trace report issue: [workspace-hub #2571](https://github.com/vamseeachanta/workspace-hub/issues/2571)
- Durable report: [b1528-sirocco-moored-current-report.md](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-moored-current-report.md)
- Packaged input YAML: [b1528_sirocco_moored_current.yml](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/b1528_sirocco_moored_current.yml)
- Report generator: [b1528_sirocco_moored_current_report.py](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_moored_current_report.py)
- Master calculation review: [rudder-and-ship-force-calculation-review.md](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/rudder-and-ship-force-calculation-review.md)

## Design data

| Item | Value |
|---|---:|
| LBP | `225.500 m` |
| Yaw lever | `135.300 m` |
| Rudder area | `44.939563 m^2` |
| Rudder span | `9.000 m` |
| Ship speed over ground | `0.000 kn` |
| Current speed | `3.500 kn / 1.80054 m/s` |
| Rudder angle sweep | `+/-1 deg, +/-2 deg, +/-3 deg, +/-4 deg, +/-5 deg` |
| Beta | `600.0` |
| Cr | `1.0` |
| X convention | `downstream/current drag direction` |
| Y convention | `port` |
| N convention | `bow_to_port` |

## Analysis methodology and assumptions

```text
V=3.5 kn*0.51444; F=beta*A_R*V^2*Cr; Fn=F*sin(delta); X=F*sin(delta)^2 downstream; Y=F*sin(delta)*cos(delta); N=Y*(0.6*LBP)
```

- Vessel is moored with ship speed over ground equal to 0 kn.
- Current is aligned with the vessel centerline and passes the rudder at 3.5 kn.
- Rudder-induced loads are calculated with the B1528/Barrass workbook force family.
- Cr=1.0 is used because the propeller is not rotating or no propeller-rotation correction is applied.
- Only rudder-induced COG force components are reported; hull current loads require vessel current coefficients.
- Mooring reactions are shown as equal and opposite loads for static equilibrium context only.

## Force-component table at COG

| Side | Rudder (deg) | X downstream (N) | Y port (N) | N bow-port (kN-m) | Resultant H (N) |
|---|---:|---:|---:|---:|---:|
| port | 1.0 | 26.625 | 1525.369 | 206.382377 | 1525.601 |
| port | 2.0 | 106.469 | 3048.879 | 412.513309 | 3050.737 |
| port | 3.0 | 239.434 | 4568.674 | 618.141656 | 4574.944 |
| port | 4.0 | 425.358 | 6082.904 | 823.016894 | 6097.758 |
| port | 5.0 | 664.015 | 7589.722 | 1026.889412 | 7618.714 |
| starboard | -1.0 | 26.625 | -1525.369 | -206.382377 | 1525.601 |
| starboard | -2.0 | 106.469 | -3048.879 | -412.513309 | 3050.737 |
| starboard | -3.0 | 239.434 | -4568.674 | -618.141656 | 4574.944 |
| starboard | -4.0 | 425.358 | -6082.904 | -823.016894 | 6097.758 |
| starboard | -5.0 | 664.015 | -7589.722 | -1026.889412 | 7618.714 |

Unsupported components in this bounded rudder-only calculation are reported as zero: Z, K, and M. Hull current force components are not included without current coefficients.

## Sample working example

Data point: `moored current, 3.5 kn, port rudder 1 deg, Cr=1.0`.

- Speed conversion: `V = 3.5 kn * 0.51444 = 1.80054 m/s`.
- Base force: `F = 600.0 * 44.939563 * 1.80054^2 * 1.0 = 87414.936 N`.
- Normal force: `Fn = F * sin(1.0 deg) = 1525.601 N`.
- COG components: `X = 26.625 N`, `Y = 1525.369 N`, `N = 206.382377 kN-m`.

## Limitations

- rudder-induced moored-current loads only
- hull current force, bank effect, tug loads, mooring-line stiffness, current-profile variation, and propeller race are excluded
- current is assumed aligned with vessel centerline and passes the rudder at 3.5 kn
- Cr=1.0 is a neutral no-propeller-rotation correction value
- no class compliance conclusion

## References

- B1528 source pack issue: [workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569)
- B1528 moored-current generated HTML report: [generated HTML](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/moored_current/b1528_sirocco_moored_current_report.html)
- B1528 static yaw report: [durable report](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-yaw-moment-report.md)
- B1528 time-trace report: [durable report](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-time-trace-report.md)
- Propeller rotation factor note source: [b1528_sirocco_yaw_report.py](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_yaw_report.py)

## Interactive charts

Open `b1528_sirocco_moored_current_report.html` for force and moment charts.