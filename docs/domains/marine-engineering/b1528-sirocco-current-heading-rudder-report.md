# B1528 SIROCCO Current-Heading/Rudder Force Component Report

Prepared for engineer review on 2026-05-09.

This document records the durable report contract for
[digitalmodel #598](https://github.com/vamseeachanta/digitalmodel/issues/598).
The generated interactive report is produced by:

```bash
PYTHONPATH=src uv run python - <<'PY'
from digitalmodel.naval_architecture.b1528_sirocco_current_heading_rudder_report import load_packaged_b1528_current_heading_rudder_config, run_b1528_current_heading_rudder_report, write_b1528_current_heading_rudder_report
result = run_b1528_current_heading_rudder_report(load_packaged_b1528_current_heading_rudder_config())
print(write_b1528_current_heading_rudder_report(result, 'outputs/b1528_sirocco/current_heading_rudder'))
PY
```

## Traceability links

| Item | GitHub link |
|---|---|
| Current-heading/rudder report issue | [digitalmodel #598](https://github.com/vamseeachanta/digitalmodel/issues/598) |
| Approved plan | [`2026-05-08-issue-598-sirocco-current-heading-rudder-force-components.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/plans/2026-05-08-issue-598-sirocco-current-heading-rudder-force-components.md) |
| Source pack issue | [workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569) |
| Report generator | [`b1528_sirocco_current_heading_rudder_report.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py) |
| Packaged input YAML | [`b1528_sirocco_current_heading_rudder.yml`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml) |
| Generated Markdown report | [`b1528_sirocco_current_heading_rudder_report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/current_heading_rudder/b1528_sirocco_current_heading_rudder_report.md) |
| Generated HTML report | [`b1528_sirocco_current_heading_rudder_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/current_heading_rudder/b1528_sirocco_current_heading_rudder_report.html) |
| Related moored-current report | [`b1528-sirocco-moored-current-report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-moored-current-report.md) |

## Scope

The workflow evaluates rudder-induced force components at the COG for SIROCCO
in a moored current condition with:

- requested engineering current-speed planes: `1, 1.5, 2.0, 2.5, 3, 3.5, 4, 4.5 kn`
- chart-default current speed: `4.56 kn` as an extra, flagged plane
- heading offsets: `-10..+10 deg` in `1 deg` steps
- rudder angles: `-10..+10 deg` in `1 deg` steps

This is a bounded rudder-induced force-component chart set. Hull current loads,
bank effects, tug loads, mooring-line stiffness, current-profile variation,
propeller race, locked/freewheeling propeller drag, and class/IMO compliance
conclusions are not included.

## Design data

| Field | Value |
|---|---:|
| Vessel condition | `moored` |
| Ship speed over ground | `0.0 kn` |
| LBP | `225.5 m` |
| COG yaw lever | `0.6 * LBP = 135.3 m` |
| Rudder area | `44.93956319369854 m^2` |
| Rudder span | `9.0 m` |
| Current-speed sweep | `1..4.5 kn` plus chart-default `4.56 kn` |
| Heading sweep | `-10..+10 deg`, `1 deg` step |
| Rudder sweep | `-10..+10 deg`, `1 deg` step |
| Barrass workbook beta | `600.0` |
| Propeller rotation factor | `Cr=1.0` |

## Methodology and assumptions

The report reuses the B1528/Barrass workbook force family for the rudder-induced
current-load component and extends it with an explicit first-cut heading/rudder
visualization convention:

```text
V = current_speed_kn * 0.51444
F = beta * A_R * V^2 * Cr
alpha = rudder_angle_deg - heading_offset_deg
Fn = F * sin(alpha)
X_local = F * sin(alpha)^2
Y_local = F * sin(alpha) * cos(alpha)
X_ship = X_local * cos(psi) - Y_local * sin(psi)
Y_ship = X_local * sin(psi) + Y_local * cos(psi)
N_ship = Y_ship * (0.6 * LBP)
```

COG component convention:

- `+X_ship`: forward/downstream reference for zero heading
- `+Y_ship`: port
- `+Z`: upward; zero in this planar rudder-only model
- `+N_ship`: bow-to-port yaw moment
- `K` and `M`: zero in this planar rudder-only model
- Mooring reactions are equal-and-opposite static-equilibrium context values,
  not line-stiffness or line-tension analysis.

When `rudder_angle_deg == heading_offset_deg`, the effective rudder inflow angle
`alpha` is zero and the rudder-induced component is zero. That is not total hull
current load.

## Chart set

The generated HTML report uses two Plotly chart areas:

1. **Ship-fixed force component chart** — line traces of `X_ship`, `Y_ship`, and
   resultant horizontal force over heading offset for the selected current speed
   and selected rudder angle.
2. **Yaw-moment heatmap** — signed `N_ship` yaw moment over heading offset and
   rudder angle for the selected current speed.

The current-speed dropdown defaults to `4.56 kn`. The rudder-angle dropdown for
Chart 1 defaults to `+10 deg` so the first view has visible non-zero response.

## Generated outputs

Default output directory: `outputs/b1528_sirocco/current_heading_rudder/`.

- `b1528_sirocco_current_heading_rudder_results.csv`
- `b1528_sirocco_current_heading_rudder_results.json`
- `b1528_sirocco_current_heading_rudder_provenance.json`
- `b1528_sirocco_current_heading_rudder_report.md`
- `b1528_sirocco_current_heading_rudder_report.html`
- `b1528_sirocco_current_heading_rudder_manifest.json`

## Review caveats

- This is not a validated oblique-current hull/rudder interaction model.
- The heading extension is an interpretation chart set built from local
  rudder-induced forces rotated into ship axes.
- Hull current coefficients or an MMG-style maneuvering model are required before
  treating these values as total ship current loads.
