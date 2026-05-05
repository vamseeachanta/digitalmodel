# Rudder stock torque sweep

Issue: <https://github.com/vamseeachanta/workspace-hub/issues/2565>

This workflow evaluates a **preliminary** rudder-stock torque and equal/opposite steering-gear holding torque over a typical-ship grid of forward speeds and rudder angles.

## Traceability links

| Item | GitHub link |
|---|---|
| Rudder-stock torque issue | [workspace-hub #2565](https://github.com/vamseeachanta/workspace-hub/issues/2565) |
| Calculation source | [`rudder_stock_torque.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/rudder_stock_torque.py) |
| Rudder force source | [`maneuverability.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/maneuverability.py) |
| Packaged input YAML | [`rudder_stock_torque_typical_ship.yml`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/rudder_stock_torque_typical_ship.yml) |
| Validation tests | [`test_rudder_stock_torque_sweep.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/tests/naval_architecture/test_rudder_stock_torque_sweep.py) |
| Master calculation review | [`rudder-and-ship-force-calculation-review.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/rudder-and-ship-force-calculation-review.md) |

## Scope boundary

The calculation is intentionally bounded:

- It reuses `digitalmodel.naval_architecture.maneuverability.rudder_normal_force` for signed scalar rudder normal force.
- It applies a user-supplied constant perpendicular arm from rudder stock axis to the resultant normal-force line of action.
- It reports hydrodynamic torque and the required equal/opposite holding torque.
- It is **not** steering gear machinery sizing, actuator sizing, rudder stock scantling, or class/SOLAS compliance proof.

## Formula and sign convention

For each speed/rudder-angle grid point:

```text
hydrodynamic_rudder_stock_torque_Nm = scalar_normal_force_N * stock_to_center_of_pressure_arm_m
required_steering_gear_holding_torque_Nm = -hydrodynamic_rudder_stock_torque_Nm
rudder_stock_torque_abs_Nm = abs(hydrodynamic_rudder_stock_torque_Nm)
rudder_stock_torque_abs_kNm = rudder_stock_torque_abs_Nm / 1000
```

The stock arm is defined as the **perpendicular force-line moment arm** from the rudder stock axis to the resultant normal-force line of action. It is a user input, not a standards-derived coefficient.

Positive hydrodynamic torque follows the right-hand rule about the positive rudder-stock axis (`+z` up): counterclockwise viewed from above for positive signed scalar normal force and positive stock arm. The required steering-gear holding torque is reported with the equal/opposite sign.

## Propeller rotation factor `Cr`

The rudder-stock torque sweep does not use the B1528 workbook propeller rotation
factor `Cr`. It uses the reusable rudder normal-force helper and a user-supplied
stock-to-center-of-pressure arm. If the separate legacy workbook force formula
is run for a non-rotating propeller, the neutral propeller-rotation multiplier is
`Cr=1.0`.

## Sample working example

For a visual and arithmetic check, use:

```text
V = 5.0 m/s
rho = 1025 kg/m^3
rudder area = 20.0 m^2
rudder span = 5.0 m
rudder angle = +10 deg
behind_hull = false
stock-to-center-of-pressure arm = 0.75 m

scalar normal force = 97417.403 N
T_stock = 97417.403 * 0.75 = 73063.052 N*m = 73.063 kN*m
T_holding = -73063.052 N*m = -73.063 kN*m
```

The generated torque charts should show the `+10 deg`, `5 m/s` equivalent point
at positive hydrodynamic stock torque and equal/opposite negative holding torque.

## Packaged input

The packaged typical-ship input is:

```text
src/digitalmodel/naval_architecture/data/rudder_stock_torque_typical_ship.yml
```

Key fields:

- `rudder.area_m2`
- `rudder.span_m`
- `stock.stock_to_center_of_pressure_arm_m`
- `environment.rho_kg_m3`
- `sweep.speeds.units` and `sweep.speeds.values`
- `sweep.rudder_angles_deg`
- `outputs.tables`
- `outputs.charts.required`

## Python API

```python
from digitalmodel.naval_architecture import (
    load_packaged_rudder_stock_torque_yaml,
    run_rudder_stock_torque_sweep,
    write_rudder_stock_torque_results,
)

config = load_packaged_rudder_stock_torque_yaml()
result = run_rudder_stock_torque_sweep(config)
manifest = write_rudder_stock_torque_results(
    result,
    "results/rudder_stock_torque",
    table_formats=("csv", "json"),
    chart_formats=("png", "html"),
)
```

For a single case:

```python
from digitalmodel.naval_architecture.rudder_stock_torque import rudder_stock_torque

result = rudder_stock_torque(
    velocity_m_s=5.0,
    rho_kg_m3=1025.0,
    rudder_area_m2=20.0,
    rudder_span_m=5.0,
    rudder_angle_deg=10.0,
    stock_to_center_of_pressure_arm_m=0.75,
    behind_hull=False,
)
```

## Outputs

`write_rudder_stock_torque_results(...)` writes:

- `rudder_stock_torque_sweep.csv`
- `rudder_stock_torque_sweep.json`
- `rudder_stock_torque_provenance.json`
- `artifact_manifest.json`
- chart files in each requested chart format.

Required charts:

1. `rudder_stock_torque_vs_rudder_angle_by_speed`
2. `rudder_stock_torque_vs_speed_by_rudder_angle`
3. `scalar_normal_force_vs_rudder_angle_by_speed`
4. `rudder_stock_torque_speed_angle_heatmap`

## Provenance and citations

The provenance sidecar records:

- force source module,
- torque relation,
- equal/opposite holding torque relation,
- user-supplied arm definition,
- scope limitations.

No new standards-derived coefficient is introduced by this workflow. The underlying force helper already documents its empirical rudder-force basis.

## Validation

Targeted validation for [#2565](https://github.com/vamseeachanta/workspace-hub/issues/2565) is in:

```text
tests/naval_architecture/test_rudder_stock_torque_sweep.py
```

The tests cover:

- TDD red/green module creation,
- direct torque identity,
- speed-squared behavior inherited from rudder normal force,
- arm linearity,
- sign semantics for hydrodynamic vs holding torque,
- packaged YAML loading via `importlib.resources`,
- built-wheel package data preservation,
- output table/sidecar/manifest/chart generation,
- public import smoke.
