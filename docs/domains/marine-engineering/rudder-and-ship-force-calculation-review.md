# Rudder and Ship-Force Calculation Review

Date: 2026-05-05

This report reviews the GitHub issue chain and existing `digitalmodel` calculations
related to rudder force, rudder-induced yaw moment, rudder-stock torque, B1528
SIROCCO yaw/time-trace reporting, and the adjacent passing-ship force calculator.

The current rudder calculation family is suitable for preliminary parametric
engineering reports. It is not a full MMG maneuvering model, not an incident
reconstruction, not an IMO/class compliance assessment, and not a rudder-stock
scantling or steering-gear machinery sizing calculation.

## Issue Trace

| Issue | State | Calculation role | Primary artifacts |
|---|---|---|---|
| [workspace-hub #1317](https://github.com/vamseeachanta/workspace-hub/issues/1317) | Open | Older maneuverability umbrella for rudder forces and turning circle. | [`maneuverability.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/maneuverability.py) |
| [workspace-hub #2564](https://github.com/vamseeachanta/workspace-hub/issues/2564) | Closed, done | Reusable typical-ship yaw-moment sweep from rudder normal force. | [`yaw_moment.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/yaw_moment.py), [`yaw_moment_typical_ship.yml`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/yaw_moment_typical_ship.yml), [`yaw-moment-sweep.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/yaw-moment-sweep.md) |
| [workspace-hub #2565](https://github.com/vamseeachanta/workspace-hub/issues/2565) | Closed, done | Rudder-stock torque and equal/opposite holding torque sweep. | [`rudder_stock_torque.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/rudder_stock_torque.py), [`rudder_stock_torque_typical_ship.yml`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/rudder_stock_torque_typical_ship.yml), [`rudder-stock-torque-sweep.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/rudder-stock-torque-sweep.md) |
| [workspace-hub #2567](https://github.com/vamseeachanta/workspace-hub/issues/2567) | Closed | Standards/source crosswalk for future steering gear and rudder-stock design checks. No formulas implemented. | naval-architecture wiki source map, concept page, standards crosswalk |
| [workspace-hub #2568](https://github.com/vamseeachanta/workspace-hub/issues/2568) | Closed but left at plan-review | Preliminary turning-circle/tactical-diameter estimator planning stream. Later review found blockers. | plan/review artifacts; no tracked `turning_circle.py` implementation in this checkout |
| [workspace-hub #2570](https://github.com/vamseeachanta/workspace-hub/issues/2570) | Closed, done | B1528 SIROCCO static yaw-moment workbook-regression and digitalmodel comparison report. | [`b1528_sirocco_yaw_report.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_yaw_report.py), [`b1528_sirocco_yaw_moment.yml`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/b1528_sirocco_yaw_moment.yml), [`b1528-sirocco-yaw-moment-report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-yaw-moment-report.md), [`generated output`](https://github.com/vamseeachanta/digitalmodel/tree/main/outputs/b1528_sirocco) |
| [workspace-hub #2571](https://github.com/vamseeachanta/workspace-hub/issues/2571) | Closed, done | B1528 SIROCCO first-order Nomoto time trace with rudder-local inflow diagnostics. | [`b1528_sirocco_time_trace.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_time_trace.py), [`b1528_sirocco_time_trace.yml`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/b1528_sirocco_time_trace.yml), [`b1528-sirocco-time-trace-report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-time-trace-report.md), [`generated output`](https://github.com/vamseeachanta/digitalmodel/tree/main/outputs/b1528_sirocco/time_trace) |
| [workspace-hub #2642](https://github.com/vamseeachanta/workspace-hub/issues/2642) | Closed, done | B1528 SIROCCO moored-current rudder force components at COG for 3.5 kn current and +/-1 to +/-5 deg rudder. | [`b1528_sirocco_moored_current_report.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_moored_current_report.py), [`b1528_sirocco_moored_current.yml`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/b1528_sirocco_moored_current.yml), [`b1528-sirocco-moored-current-report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-moored-current-report.md), [`generated output`](https://github.com/vamseeachanta/digitalmodel/tree/main/outputs/b1528_sirocco/moored_current) |
| [workspace-hub #2603](https://github.com/vamseeachanta/workspace-hub/issues/2603) | Closed | Public API fix for rudder-stock torque loader and runner exports. | [`__init__.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/__init__.py) |
| [digitalmodel #283](https://github.com/vamseeachanta/digitalmodel/issues/283) | Open | Separate passing-ship hydrodynamic force calculation for moored vessels. | [`passing_ship/`](https://github.com/vamseeachanta/digitalmodel/tree/main/src/digitalmodel/hydrodynamics/passing_ship), [`benchmark_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/passing_ship/wang_benchmark/benchmark_report.html) |

## Report Links

| Report | GitHub link |
|---|---|
| Master rudder and ship-force calculation review | [`rudder-and-ship-force-calculation-review.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/rudder-and-ship-force-calculation-review.md) |
| Reusable yaw-moment sweep report | [`yaw-moment-sweep.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/yaw-moment-sweep.md) |
| Rudder-stock torque sweep report | [`rudder-stock-torque-sweep.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/rudder-stock-torque-sweep.md) |
| B1528 static yaw durable report | [`b1528-sirocco-yaw-moment-report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-yaw-moment-report.md) |
| B1528 generated static Markdown report | [`b1528_sirocco_yaw_moment_report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/b1528_sirocco_yaw_moment_report.md) |
| B1528 generated static HTML report | [`b1528_sirocco_yaw_moment_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/b1528_sirocco_yaw_moment_report.html) |
| B1528 time-trace durable report | [`b1528-sirocco-time-trace-report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-time-trace-report.md) |
| B1528 generated time-trace Markdown report | [`b1528_sirocco_time_trace_report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/time_trace/b1528_sirocco_time_trace_report.md) |
| B1528 generated time-trace HTML report | [`b1528_sirocco_time_trace_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/time_trace/b1528_sirocco_time_trace_report.html) |
| B1528 moored-current durable report | [`b1528-sirocco-moored-current-report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/b1528-sirocco-moored-current-report.md) |
| B1528 generated moored-current Markdown report | [`b1528_sirocco_moored_current_report.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/moored_current/b1528_sirocco_moored_current_report.md) |
| B1528 generated moored-current HTML report | [`b1528_sirocco_moored_current_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/moored_current/b1528_sirocco_moored_current_report.html) |
| Passing-ship benchmark HTML report | [`benchmark_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/passing_ship/wang_benchmark/benchmark_report.html) |

## Associated Calculations

### 1. Rudder Lift Coefficient

Implemented in `src/digitalmodel/naval_architecture/maneuverability.py`.

```text
AR_geo = rudder_span_m^2 / rudder_area_m2
AR_eff = 2 * AR_geo     when behind_hull is true
AR_eff = AR_geo         when behind_hull is false
C_N = (6.13 * AR_eff) / (AR_eff + 2.25) * sin(delta)
```

Inputs:

- `rudder_area_m2`
- `rudder_span_m`
- `rudder_angle_deg`
- `behind_hull`

The code validates positive area and span. It does not enforce a stall limit in
the helper itself; the current wiki/source guidance treats roughly `+/-35 deg`
as the bounded pre-stall working range and warns that larger angles need
additional treatment.

### 2. Rudder Normal Force

Implemented in `maneuverability.rudder_normal_force(...)`.

```text
F_N = 0.5 * rho_kg_m3 * velocity_m_s^2 * rudder_area_m2 * C_N
```

Inputs:

- `velocity_m_s`
- `rho_kg_m3`
- `rudder_area_m2`
- `rudder_span_m`
- `rudder_angle_deg`
- `behind_hull`

Important behavior:

- Force scales with `V^2`.
- Force changes sign with rudder angle through `sin(delta)`.
- Zero speed or zero angle produces zero force.
- The higher-level yaw and torque wrappers add stronger finite/positive input
  validation around density, speed, area, span, and lever arms.

### 3. Rudder-Induced Yaw Moment

Implemented in `src/digitalmodel/naval_architecture/yaw_moment.py` for #2564.

```text
transverse_force_N = scalar_normal_force_N
    if positive_force_direction == "port"
    else -scalar_normal_force_N

M_z = x_rudder_from_cg_m * transverse_force_N
```

Coordinate convention in this workflow:

- `+x` forward
- `+y` port
- `+z` up
- positive yaw moment is bow-to-port

Packaged typical-ship input:

```text
src/digitalmodel/naval_architecture/data/yaw_moment_typical_ship.yml
```

Packaged grid:

- speeds: `0, 2, 5, 10, 15 kn`
- rudder angles: `-35, -20, -10, 0, 10, 20, 35 deg`
- rudder area: `20.0 m^2`
- rudder span: `5.0 m`
- rudder x from CG: `-45.0 m`
- density: `1025.0 kg/m^3`
- `behind_hull: false`

Outputs:

- `yaw_moment_sweep.csv`
- `yaw_moment_sweep.json`
- yaw moment vs rudder angle by speed
- yaw moment vs speed by rudder angle
- transverse force vs rudder angle by speed
- speed/angle yaw-moment heatmap

### 4. Rudder-Stock Torque and Holding Torque

Implemented in `src/digitalmodel/naval_architecture/rudder_stock_torque.py` for
#2565.

```text
T_stock = scalar_normal_force_N * stock_to_center_of_pressure_arm_m
T_holding = -T_stock
T_abs_Nm = abs(T_stock)
T_abs_kNm = T_abs_Nm / 1000
```

Packaged typical-ship input:

```text
src/digitalmodel/naval_architecture/data/rudder_stock_torque_typical_ship.yml
```

Packaged grid:

- speeds: `0.0, 2.5, 5.0, 7.5, 10.0 kn`
- rudder angles: `-30, -20, -10, 0, 10, 20, 30 deg`
- rudder area: `20.0 m^2`
- rudder span: `5.0 m`
- stock-to-center-of-pressure arm: `0.75 m`
- density: `1025.0 kg/m^3`
- `behind_hull: true`

Outputs:

- `rudder_stock_torque_sweep.csv`
- `rudder_stock_torque_sweep.json`
- `rudder_stock_torque_provenance.json`
- `artifact_manifest.json`
- torque vs rudder angle by speed
- torque vs speed by rudder angle
- scalar normal force vs rudder angle by speed
- speed/angle torque heatmap

This is a constant-arm torque envelope only. It does not calculate stock
diameter, section modulus, combined bending/torsion stress, bearing reactions,
actuator sizing, hydraulic power, tiller/rotor/key capacity, or class-rule
acceptance.

### 5. B1528 SIROCCO Static Yaw Report

Implemented in `src/digitalmodel/naval_architecture/b1528_sirocco_yaw_report.py`
for #2570.

Packaged input:

```text
src/digitalmodel/naval_architecture/data/b1528_sirocco_yaw_moment.yml
```

Source values preserved in YAML:

- LBP: `225.5 m`
- rudder area: `44.93956319369854 m^2`
- rudder span: `9.0 m`
- legacy yaw lever: `0.6 * LBP = 135.3 m`
- workbook beta: `600.0`
- propeller rotation factors: port `1.065`, stbd `0.935`
- speeds: `0.0, 1.0, 2.5, 5.0 kn`
- rudder angles: `-5, -2, -1, 0, 1, 2, 5 deg`

The report intentionally keeps two calculation modes separate:

```text
workbook_regression:
F = beta * AR * V^2 * Cr
Ft = F * sin(alpha) * cos(alpha)
Fn = F * sin(alpha)
Mz = (Fn / 1000 / g) * (0.6 * LBP)

digitalmodel_static_yaw:
M_z = x_rudder_from_cg_m * transverse_force_N
```

`Cr` is the legacy workbook propeller-rotation correction multiplier. It is
selected by the workbook rotation case before the force is decomposed into
`Ft` and `Fn`:

- port rotation case: `Cr=1.065`, which increases the workbook base force by
  6.5%;
- starboard rotation case: `Cr=0.935`, which reduces the workbook base force by
  6.5%;
- non-rotating propeller, or no propeller-rotation correction: `Cr=1.0`.

The logic is that the side-dependent values reproduce the workbook's empirical
allowance for propeller/rudder inflow asymmetry. With no propeller rotation,
there is no rotation correction to apply, so `Cr=1.0` is the neutral multiplier.
This neutral value does not model locked or freewheeling propeller drag/wake
effects; those require a separate propeller/wake/slipstream model.

The workbook text mentions `Ft`, but the evaluated yaw-moment cell uses `Fn`.
The code records that distinction as provenance rather than silently blending
the two methods.

Generated B1528 operating-point results already present in
`outputs/b1528_sirocco/b1528_sirocco_yaw_moment_results.json`:

| Mode | Speed | Rudder | Rotation | Yaw moment |
|---|---:|---:|---|---:|
| workbook_regression | 2.5 kn | -1 deg | stbd | `-98.467815 kN*m` |
| digitalmodel_static_yaw | 2.5 kn | -1 deg | n/a | `+339.513315 kN*m` |
| workbook_regression | 2.5 kn | +1 deg | port | `+112.158527 kN*m` |
| digitalmodel_static_yaw | 2.5 kn | +1 deg | n/a | `-339.513315 kN*m` |

The digitalmodel comparison rows use the #2564 Whicker/Fehlner-style rudder
force basis and map the legacy `0.6 * LBP` lever to `x_rudder_from_cg_m` for
comparison only. They are not a workbook-regression target.

### 6. B1528 SIROCCO Time Trace

Implemented in `src/digitalmodel/naval_architecture/b1528_sirocco_time_trace.py`
for #2571.

Packaged input:

```text
src/digitalmodel/naval_architecture/data/b1528_sirocco_time_trace.yml
```

Governing equations:

```text
v_R = x_R * r
beta_R = atan2(-x_R * r, U)
alpha_R = delta_cmd - beta_R
U_R = hypot(U, v_R)
r_dot = (K * alpha_R - r) / T
psi_dot = r
x_dot = U * cos(psi)
y_dot = U * sin(psi)
```

The rudder force and yaw moment are diagnostic only in this time-trace mode.
They are not fed back into `r_dot`, which avoids double-counting direct moment
balance and Nomoto `K/T` response in a preliminary model.

The time-trace workflow does not apply the workbook side-dependent `Cr=1.065`
or `Cr=0.935` values. It uses the reusable digitalmodel static-yaw rudder-force
diagnostic rather than the legacy workbook regression formula, and its rows
carry `Cr=1.0` as the neutral non-rotating/no-rotation-correction multiplier.

Current packaged defaults:

- speed: `2.5 kn`
- rudder command: `+1 deg`
- duration: `600 s`
- time step: `1 s`
- Nomoto `K`: `0.018 1/s`
- Nomoto `T`: `55 s`
- rudder x from CG: `-135.3 m`

Generated metrics from
`outputs/b1528_sirocco/time_trace/b1528_sirocco_time_trace_results.json`:

| Scenario | Final heading | Final yaw rate | Advance | Tactical-diameter proxy |
|---|---:|---:|---:|---:|
| positive_rudder | `3.620416 deg` | `0.006221 deg/s` | `771.161 m` | `23.705 m` |
| negative_rudder | `-3.620416 deg` | `-0.006221 deg/s` | `771.161 m` | `23.705 m` |
| zero_rudder | `0.0 deg` | `0.0 deg/s` | `771.660 m` | `0.0 m` |

These values are source-gap/sensitivity outputs because B1528-specific Nomoto
coefficients and instrumented x/y track data are not available in the current
source pack.

### 7. B1528 SIROCCO Moored-Current Rudder Force Components

Implemented in
`src/digitalmodel/naval_architecture/b1528_sirocco_moored_current_report.py`
for #2642.

Packaged input:

```text
src/digitalmodel/naval_architecture/data/b1528_sirocco_moored_current.yml
```

Current packaged defaults:

- vessel condition: `moored`
- ship speed over ground: `0.0 kn`
- current passing rudder: `3.5 kn`
- current direction: aligned with vessel centerline
- rudder angle sweep: `+/-1, +/-2, +/-3, +/-4, +/-5 deg`
- LBP: `225.5 m`
- COG yaw lever: `0.6 * LBP = 135.3 m`
- rudder area: `44.93956319369854 m^2`
- beta: `600.0`
- propeller rotation factor: `Cr=1.0`

The report uses the B1528/Barrass workbook force family and resolves the
rudder-induced load to COG components:

```text
V = 3.5 kn * 0.51444
F = beta * A_R * V^2 * Cr
Fn = F * sin(delta)
X = F * sin(delta)^2
Y = F * sin(delta) * cos(delta)
N = Y * (0.6 * LBP)
```

Component convention:

- `+X`: downstream/current-drag direction
- `+Y`: port
- `+Z`: upward
- `+N`: bow-to-port yaw moment
- `Z`, `K`, and `M`: zero in this planar rudder-only model

The selected `Cr=1.0` is the neutral multiplier because the requested moored
case excludes propeller-rotation correction. It does not model locked or
freewheeling propeller drag/wake effects.

Sample generated results from
`outputs/b1528_sirocco/moored_current/b1528_sirocco_moored_current_results.json`:

| Side | Rudder | X downstream | Y port | N bow-port |
|---|---:|---:|---:|---:|
| port | `+1 deg` | `26.625 N` | `1525.369 N` | `206.382377 kN*m` |
| port | `+5 deg` | `664.015 N` | `7589.722 N` | `1026.889412 kN*m` |
| starboard | `-1 deg` | `26.625 N` | `-1525.369 N` | `-206.382377 kN*m` |
| starboard | `-5 deg` | `664.015 N` | `-7589.722 N` | `-1026.889412 kN*m` |

This is not a full hull current-load analysis. Hull current force, bank effect,
tug loads, mooring-line stiffness, current-profile variation, and propeller race
remain excluded unless additional coefficients or project requirements are
supplied.

### 8. Passing-Ship Force Calculation

The phrase "ship force calculation" also matches the separate live
`digitalmodel` issue #283: passing-ship hydrodynamic interaction forces for a
moored vessel.

Implemented module:

```text
src/digitalmodel/hydrodynamics/passing_ship/
```

Main outputs:

- surge force on the moored vessel
- sway force on the moored vessel
- yaw moment on the moored vessel
- optional force time history as the passing ship moves along its track

The current formulation uses Wang-style slender-body interaction kernels:

```text
S1(x) = 1 - (2x/L)^2
S2(x) = S1(x) * (2x/L)
surge = (2/3) * rho * U^2 * A1 * A2 * integral(G)
sway  = (2/3) * rho * U^2 * A1 * A2 * integral(F)
yaw   = (2/3) * rho * U^2 * A1 * A2 * integral(eta * F)
```

Workbook `Cr` is not used in this passing-ship calculation. `Cr` is only a
propeller-rotation multiplier in the B1528 static workbook-regression rudder
force formula.

There is a coordinate convention difference that matters if the passing-ship
loads are compared against the rudder-yaw family:

- rudder/yaw-moment workflow: `+y` port, positive yaw bow-to-port
- passing-ship workflow: `+y` starboard, positive yaw clockwise/bow-to-starboard
- AQWA mapping in the passing-ship convention negates `Fy` and `Mz`

The passing-ship calculation is adjacent hydrodynamics work, but it is not part
of the rudder-force/yaw-moment/stock-torque chain.

## Charts and Sample Verification Points

The detailed report pages linked above include the chart inventory for each
workflow. Where generated output exists in this repository, the HTML reports add
interactive Plotly charts and a highlighted sample point for visual checking.

| Calculation | Chart/report reference | Sample point |
|---|---|---|
| Rudder normal force | Used by [`yaw-moment-sweep.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/yaw-moment-sweep.md) and [`rudder-stock-torque-sweep.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/rudder-stock-torque-sweep.md). | `V=5.0 m/s`, `rho=1025 kg/m^3`, `area=20.0 m^2`, `span=5.0 m`, `delta=+10 deg`, `behind_hull=false` gives `F_N=97417.403 N`. |
| Reusable yaw moment | [`yaw-moment-sweep.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/yaw-moment-sweep.md) lists yaw moment vs angle, yaw moment vs speed, transverse force, and heatmap charts. | With the normal-force sample and `x_R=-45.0 m`, port force mapping gives `M_z=-4383.783 kN*m`. |
| Rudder-stock torque | [`rudder-stock-torque-sweep.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/rudder-stock-torque-sweep.md) lists torque vs angle, torque vs speed, scalar normal force, and heatmap charts. | With the normal-force sample and stock arm `0.75 m`, `T_stock=73.063 kN*m` and `T_holding=-73.063 kN*m`. |
| B1528 static yaw | Generated HTML: [`b1528_sirocco_yaw_moment_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/b1528_sirocco_yaw_moment_report.html). | `workbook_regression`, `2.5 kn`, `+1 deg`, port `Cr=1.065`: `F=47498.422 N`, `Fn=828.962 N`, `M_z=112.158527 kN*m`. |
| B1528 time trace | Generated HTML: [`b1528_sirocco_time_trace_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/time_trace/b1528_sirocco_time_trace_report.html). Includes heading angle vs time and yaw moment vs time with neutral `Cr=1.0`. | Positive-rudder first step: `alpha_R=1.000000 deg`, `r_dot=0.0000057120 rad/s^2`, yaw rate after `1 s` is `0.000327273 deg/s`. |
| B1528 moored-current rudder COG loads | Generated HTML: [`b1528_sirocco_moored_current_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/moored_current/b1528_sirocco_moored_current_report.html). Includes sway force, yaw moment, surge drag, resultant force, and sample verification charts with neutral `Cr=1.0`. | `3.5 kn` current, port `+1 deg`, `Cr=1.0`: `F=87414.936 N`, `X=26.625 N`, `Y=1525.369 N`, `N=206.382377 kN*m`. |
| Passing-ship force | Benchmark HTML: [`benchmark_report.html`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/passing_ship/wang_benchmark/benchmark_report.html). | Basic template at separation `40 m`, stagger `75 m`, velocity `5 m/s` gives surge `98219.321 N`, sway `-70399.784 N`, yaw `-4289.950 kN*m`. |

## Review Findings

1. The reusable rudder chain is internally traceable: normal force -> transverse
   force -> yaw moment -> stock torque. Output metadata records units and
   limitations.
2. [B1528 #2570](https://github.com/vamseeachanta/workspace-hub/issues/2570)
   correctly separates workbook-regression outputs from reusable
   digitalmodel yaw outputs. This separation is necessary because the workbook
   regression uses `beta`, prop-rotation factor, `Fn`, and `0.6*LBP`, while the
   reusable digitalmodel calculation uses the Whicker/Fehlner-style rudder force
   helper and an explicit CG lever arm.
3. [B1528 #2571](https://github.com/vamseeachanta/workspace-hub/issues/2571)
   correctly treats rudder force/yaw moment as diagnostics in the
   Nomoto time trace. Feeding those moments back into the yaw-rate equation would
   double-count the steering response unless the model were rebuilt as a direct
   moment-balance/MMG model.
4. [B1528 #2642](https://github.com/vamseeachanta/workspace-hub/issues/2642)
   correctly keeps the moored-current request bounded to rudder-induced COG
   components. It does not fabricate hull current loads without B1528 hull
   current coefficients or mooring-line stiffness data.
5. [#2567](https://github.com/vamseeachanta/workspace-hub/issues/2567)
   correctly holds standards-backed rudder-stock design checks out of the
   preliminary [#2565](https://github.com/vamseeachanta/workspace-hub/issues/2565)
   torque envelope. The current torque result is an input to
   later design checks, not a compliance answer.
6. [#2568](https://github.com/vamseeachanta/workspace-hub/issues/2568)
   should not be treated as an implemented reusable turning-circle module
   in this checkout. The issue history records unresolved plan-review blockers,
   and the local worktree contains an untracked `tests/naval_architecture/test_turning_circle_estimator.py`
   but no tracked `src/digitalmodel/naval_architecture/turning_circle.py`.
7. The largest integration risk is sign convention mixing. Rudder/yaw uses `+y`
   port; passing-ship uses `+y` starboard. Any combined report must include an
   explicit force/moment mapping table.
8. The second largest technical gap is hydrodynamic interaction. The
   [#2564](https://github.com/vamseeachanta/workspace-hub/issues/2564)/[#2565](https://github.com/vamseeachanta/workspace-hub/issues/2565)
   chain excludes propeller slipstream, hull interaction amplification, drift,
   yaw inertia, MMG derivatives, and environmental loads.

## Recommended Detailed Report Structure

For a client-facing or calculation-record package, use this structure:

1. Executive summary
2. Issue and artifact traceability
3. Input data table
4. Coordinate and sign convention statement
5. Rudder normal force calculation
6. Yaw moment calculation
7. Rudder-stock torque and holding torque calculation
8. B1528 workbook-regression comparison, if project-specific
9. Time-trace sensitivity analysis, if dynamic response is required
10. Passing-ship force appendix only if the case includes external ship-ship
    interaction loads
11. Validation evidence and test commands
12. Limitations and future standards/compliance upgrade path

## Next Calculation Options

| Option | Calculation | Status | Recommendation |
|---|---|---|---|
| A | Standards-backed rudder-stock scantling and steering-gear checks | Source crosswalk done in #2567; no implementation | Create child issues only after exact class clauses are promoted |
| B | Resolve #2568 turning-circle estimator | Blocked by plan-review/sign/API issues | Reconcile plan and tracked API before coding |
| C | Propeller-rudder interaction / slipstream correction | Existing digitalmodel issue family #143/#144/#276 | Good next hydrodynamic upgrade for low-speed/drive-off cases |
| D | Integrated rudder + passing-ship load report | Requires convention mapping and load-combination rules | Do only if project case needs both internal rudder loads and external ship-passing loads |
