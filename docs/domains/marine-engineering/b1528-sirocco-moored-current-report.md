# B1528 SIROCCO Moored-Current Rudder Force Component Report

Prepared for engineer review on 2026-05-06.

This document records the durable report contract for
[workspace-hub #2642](https://github.com/vamseeachanta/workspace-hub/issues/2642).
The generated interactive report is produced by:

```bash
UV_NO_SYNC=1 PYTHONPATH=src uv run python - <<'PY'
from digitalmodel.naval_architecture.b1528_sirocco_moored_current_report import load_packaged_b1528_moored_current_config, run_b1528_moored_current_report, write_b1528_moored_current_report
result = run_b1528_moored_current_report(load_packaged_b1528_moored_current_config())
print(write_b1528_moored_current_report(result, 'outputs/b1528_sirocco/moored_current'))
PY
```

## Traceability links

| Item | GitHub link |
|---|---|
| Source pack issue | [workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569) |
| Moored-current report issue | [workspace-hub #2642](https://github.com/vamseeachanta/workspace-hub/issues/2642) |
| Static yaw report issue | [workspace-hub #2570](https://github.com/vamseeachanta/workspace-hub/issues/2570) |
| Time-trace report issue | [workspace-hub #2571](https://github.com/vamseeachanta/workspace-hub/issues/2571) |
| Report generator | [`b1528_sirocco_moored_current_report.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_moored_current_report.py) |
| Packaged input YAML | [`b1528_sirocco_moored_current.yml`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/b1528_sirocco_moored_current.yml) |
| Generated Markdown report | [`b1528_sirocco_moored_current_report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/moored_current/b1528_sirocco_moored_current_report.md) |
| Generated HTML report | [`b1528_sirocco_moored_current_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/moored_current/b1528_sirocco_moored_current_report.html) |
| Master calculation review | [`rudder-and-ship-force-calculation-review.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/rudder-and-ship-force-calculation-review.md) |

## Scope

The workflow evaluates rudder-induced force components at the COG for SIROCCO
moored in a 3.5 kn centerline current with rudder angles of 1, 2, 3, 4, and
5 deg to port and starboard.

This is a bounded rudder-only calculation. Hull current loads, bank effects,
tug loads, mooring-line stiffness, current-profile variation, propeller race,
locked/freewheeling propeller drag, and class/IMO compliance conclusions are
not included.

## Design data

| Field | Value |
|---|---:|
| Vessel condition | `moored` |
| Ship speed over ground | `0.0 kn` |
| Current passing rudder | `3.5 kn = 1.80054 m/s` |
| Current direction | `aligned_with_centerline` |
| LBP | `225.5 m` |
| COG yaw lever | `0.6 * LBP = 135.3 m` |
| Rudder area | `44.93956319369854 m^2` |
| Rudder span | `9.0 m` |
| Rudder angle sweep | `+/-1, +/-2, +/-3, +/-4, +/-5 deg` |
| Barrass workbook beta | `600.0` |
| Propeller rotation factor | `Cr=1.0` |

## Methodology and assumptions

The report uses the B1528/Barrass workbook force family for the rudder-induced
current-load component:

```text
V = 3.5 kn * 0.51444
F = beta * A_R * V^2 * Cr
Fn = F * sin(delta)
X = F * sin(delta)^2
Y = F * sin(delta) * cos(delta)
N = Y * (0.6 * LBP)
```

COG component convention:

- `+X`: downstream/current-drag direction
- `+Y`: port
- `+Z`: upward
- `+N`: bow-to-port yaw moment
- `Z`, `K`, and `M`: zero in this planar rudder-only model

Mooring reactions in the generated table are equal and opposite reactions for
static-equilibrium context. They are not a mooring-line stiffness or line-tension
analysis.

## Propeller rotation factor `Cr`

`Cr` is the legacy workbook propeller-rotation correction multiplier in
`F = beta * A_R * V^2 * Cr`.

For this moored-current case, `Cr=1.0` is used because the requested scenario
does not include propeller rotation correction. The neutral multiplier applies
no rotation-induced amplification or reduction. It does not model locked or
freewheeling propeller drag/wake effects; those require a separate propeller
wake/slipstream model.

## Sample working example

Sample point for visual verification:

```text
Scenario: moored current, port rudder 1 deg
V = 3.5 kn * 0.51444 = 1.80054 m/s
F = 600 * 44.939563 * 1.80054^2 * 1.0 = 87414.936 N
Fn = F * sin(1 deg) = 1525.601 N
X = F * sin(1 deg)^2 = 26.625 N
Y = F * sin(1 deg) * cos(1 deg) = 1525.369 N
N = Y * 135.3 / 1000 = 206.382377 kN-m
```

The generated HTML report highlights this `1 deg` sample point so the plotted
marker can be checked against the worked value above.

## Generated outputs

Default output directory: `outputs/b1528_sirocco/moored_current/`.

- `b1528_sirocco_moored_current_results.csv`
- `b1528_sirocco_moored_current_results.json`
- `b1528_sirocco_moored_current_provenance.json`
- `b1528_sirocco_moored_current_report.md`
- `b1528_sirocco_moored_current_report.html`
- `b1528_sirocco_moored_current_manifest.json`

The HTML report includes interactive Plotly charts for:

- sway force at COG versus rudder angle
- yaw moment at COG versus rudder angle
- surge drag component versus rudder angle
- horizontal resultant force versus rudder angle
- sample verification point

## Limitations

This workflow is:

- a rudder-induced moored-current load component report only;
- not a full hull current-load analysis;
- not a mooring analysis;
- not a tug-load or bank-effect analysis;
- not a propeller race/slipstream analysis;
- not an incident reconstruction;
- not an IMO compliance assessment;
- no class compliance conclusion.
