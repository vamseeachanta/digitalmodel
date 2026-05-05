# B1528 SIROCCO Static Yaw-Moment Report

This document records the durable report contract for
[workspace-hub #2570](https://github.com/vamseeachanta/workspace-hub/issues/2570).
The generated interactive report is produced by:

```bash
# Project-specific outputs land in acma-projects/B1528/yaw-and-time-trace/.
# Adjust the path to your local acma-projects checkout.
UV_NO_SYNC=1 PYTHONPATH=src uv run --with pyyaml --with plotly python - <<'PY'
from digitalmodel.naval_architecture.b1528_sirocco_yaw_report import load_packaged_b1528_yaw_config, run_b1528_static_yaw_report, write_b1528_static_yaw_report
result = run_b1528_static_yaw_report(load_packaged_b1528_yaw_config())
print(write_b1528_static_yaw_report(result, '../acma-projects/B1528/yaw-and-time-trace'))
PY
```

## Traceability links

| Item | GitHub link |
|---|---|
| Source pack issue | [workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569) |
| Static yaw report issue | [workspace-hub #2570](https://github.com/vamseeachanta/workspace-hub/issues/2570) |
| Reusable yaw-moment issue | [workspace-hub #2564](https://github.com/vamseeachanta/workspace-hub/issues/2564) |
| Report generator | [`b1528_sirocco_yaw_report.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_yaw_report.py) |
| Packaged input YAML | [`b1528_sirocco_yaw_moment.yml`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/b1528_sirocco_yaw_moment.yml) |
| Generated Markdown report | [`b1528_sirocco_yaw_moment_report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/b1528_sirocco_yaw_moment_report.md) |
| Generated HTML report | [`b1528_sirocco_yaw_moment_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/b1528_sirocco_yaw_moment_report.html) |
| Master calculation review | [`rudder-and-ship-force-calculation-review.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/rudder-and-ship-force-calculation-review.md) |

## Source pack

The B1528 source pack is completed in
[workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569)
and documents:

- LBP: `225.5 m`
- Rudder area: `44.939563193699 m²`
- Legacy yaw lever: `0.6 * LBP = 135.3 m`
- Workbook distinction: workbook text mentions `Ft`, but the evaluated yaw-moment cell uses `Fn` via `C23`.

## Calculation modes

1. `workbook_regression` reproduces the evaluated workbook family:
   - `F = beta * AR * V^2 * Cr`
   - `Ft = F * sin(alpha) * cos(alpha)`
   - `Fn = F * sin(alpha)`
   - `Mz = (Fn / 1000 / g) * (0.6 * LBP)`
2. `digitalmodel_static_yaw` uses the reusable [#2564 yaw-moment function](https://github.com/vamseeachanta/workspace-hub/issues/2564) and maps the legacy `0.6 * LBP` lever to `x_rudder_from_cg_m` for comparison only.

## Propeller rotation factor `Cr`

`Cr` is the legacy workbook propeller-rotation correction multiplier in
`F = beta * AR * V^2 * Cr`. It is selected from the workbook rotation case
before the force is split into `Ft` and `Fn`.

- Port workbook rotation case: `Cr=1.065`, increasing the base force by 6.5%.
- Starboard workbook rotation case: `Cr=0.935`, reducing the base force by 6.5%.
- Non-rotating propeller, or no rotation correction: `Cr=1.0`.

The logic is to use the side-dependent workbook value only when that rotation
case is part of the regression being reproduced. When the propeller is not
rotating, there is no rotation-induced amplification or reduction to apply, so
the neutral multiplier is `Cr=1.0`. This neutral value does not model locked or
freewheeling propeller drag/wake effects; those require a separate
propeller/wake model.

## 2.5 kn operating-point regression targets

- `+1 deg`, port `Cr=1.065`: approximately `+112.159 kN-m`.
- `-1 deg`, starboard `Cr=0.935`: approximately `-98.468 kN-m`.

## Charts and sample visual check

The generated HTML report includes these Plotly charts:

- workbook-regression yaw moment versus rudder angle;
- digitalmodel static-yaw moment versus speed;
- highlighted sample verification point.

Sample working point for visual verification:

```text
Mode: workbook_regression
Speed: 2.5 kn = 1.28610 m/s
Rudder angle: +1 deg
Rotation case: port
Cr: 1.065
F = 600 * 44.939563 * 1.28610^2 * 1.065 = 47498.422 N
Fn = F * sin(1 deg) = 828.962 N
Mz = Fn / 1000 * 135.3 = 112.158527 kN-m
```

The sample marker in the HTML chart should sit at `rudder angle = +1 deg` and
`yaw moment = +112.159 kN-m`.

## Boundary

This is a preliminary static rudder-induced yaw-moment workflow. It is not a full MMG simulation, not an incident reconstruction, not an IMO compliance assessment, and no class compliance conclusion is made.
