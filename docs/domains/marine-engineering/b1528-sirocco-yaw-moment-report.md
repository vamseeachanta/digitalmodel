# B1528 SIROCCO Static Yaw-Moment Report

This document records the durable report contract for issue #2570. The generated interactive report is produced by:

```bash
UV_NO_SYNC=1 PYTHONPATH=src uv run --with pyyaml --with plotly python - <<'PY'
from digitalmodel.naval_architecture.b1528_sirocco_yaw_report import load_packaged_b1528_yaw_config, run_b1528_static_yaw_report, write_b1528_static_yaw_report
result = run_b1528_static_yaw_report(load_packaged_b1528_yaw_config())
print(write_b1528_static_yaw_report(result, 'outputs/b1528_sirocco'))
PY
```

## Source pack

The B1528 source pack is completed in workspace-hub issue #2569 and documents:

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
2. `digitalmodel_static_yaw` uses the reusable #2564 yaw-moment function and maps the legacy `0.6 * LBP` lever to `x_rudder_from_cg_m` for comparison only.

## 2.5 kn operating-point regression targets

- `+1 deg`, port `Cr=1.065`: approximately `+112.159 kN-m`.
- `-1 deg`, starboard `Cr=0.935`: approximately `-98.468 kN-m`.

## Boundary

This is a preliminary static rudder-induced yaw-moment workflow. It is not a full MMG simulation, not an incident reconstruction, not an IMO compliance assessment, and no class compliance conclusion is made.
