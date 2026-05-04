# Rudder and Ship-Force Calculation Review

Date: 2026-05-04

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
| workspace-hub #1317 | Open | Older maneuverability umbrella for rudder forces and turning circle. | `src/digitalmodel/naval_architecture/maneuverability.py` |
| workspace-hub #2564 | Closed, done | Reusable typical-ship yaw-moment sweep from rudder normal force. | `yaw_moment.py`, `yaw_moment_typical_ship.yml`, `test_yaw_moment_sweep.py`, `yaw-moment-sweep.md` |
| workspace-hub #2565 | Closed, done | Rudder-stock torque and equal/opposite holding torque sweep. | `rudder_stock_torque.py`, `rudder_stock_torque_typical_ship.yml`, `test_rudder_stock_torque_sweep.py`, `rudder-stock-torque-sweep.md` |
| workspace-hub #2567 | Closed | Standards/source crosswalk for future steering gear and rudder-stock design checks. No formulas implemented. | naval-architecture wiki source map, concept page, standards crosswalk |
| workspace-hub #2568 | Closed but left at plan-review | Preliminary turning-circle/tactical-diameter estimator planning stream. Later review found blockers. | plan/review artifacts; no tracked `turning_circle.py` implementation in this checkout |
| workspace-hub #2570 | Closed, done | B1528 SIROCCO static yaw-moment workbook-regression and digitalmodel comparison report. | `b1528_sirocco_yaw_report.py`, B1528 YAML, outputs under `outputs/b1528_sirocco/` |
| workspace-hub #2571 | Closed, done | B1528 SIROCCO first-order Nomoto time trace with rudder-local inflow diagnostics. | `b1528_sirocco_time_trace.py`, B1528 time-trace YAML, outputs under `outputs/b1528_sirocco/time_trace/` |
| workspace-hub #2603 | Closed | Public API fix for rudder-stock torque loader and runner exports. | `src/digitalmodel/naval_architecture/__init__.py` |
| digitalmodel #283 | Open | Separate passing-ship hydrodynamic force calculation for moored vessels. | `src/digitalmodel/hydrodynamics/passing_ship/` |

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

### 7. Passing-Ship Force Calculation

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

There is a coordinate convention difference that matters if the passing-ship
loads are compared against the rudder-yaw family:

- rudder/yaw-moment workflow: `+y` port, positive yaw bow-to-port
- passing-ship workflow: `+y` starboard, positive yaw clockwise/bow-to-starboard
- AQWA mapping in the passing-ship convention negates `Fy` and `Mz`

The passing-ship calculation is adjacent hydrodynamics work, but it is not part
of the rudder-force/yaw-moment/stock-torque chain.

## Review Findings

1. The reusable rudder chain is internally traceable: normal force -> transverse
   force -> yaw moment -> stock torque. Output metadata records units and
   limitations.
2. B1528 #2570 correctly separates workbook-regression outputs from reusable
   digitalmodel yaw outputs. This separation is necessary because the workbook
   regression uses `beta`, prop-rotation factor, `Fn`, and `0.6*LBP`, while the
   reusable digitalmodel calculation uses the Whicker/Fehlner-style rudder force
   helper and an explicit CG lever arm.
3. B1528 #2571 correctly treats rudder force/yaw moment as diagnostics in the
   Nomoto time trace. Feeding those moments back into the yaw-rate equation would
   double-count the steering response unless the model were rebuilt as a direct
   moment-balance/MMG model.
4. #2567 correctly holds standards-backed rudder-stock design checks out of the
   preliminary #2565 torque envelope. The current torque result is an input to
   later design checks, not a compliance answer.
5. #2568 should not be treated as an implemented reusable turning-circle module
   in this checkout. The issue history records unresolved plan-review blockers,
   and the local worktree contains an untracked `tests/naval_architecture/test_turning_circle_estimator.py`
   but no tracked `src/digitalmodel/naval_architecture/turning_circle.py`.
6. The largest integration risk is sign convention mixing. Rudder/yaw uses `+y`
   port; passing-ship uses `+y` starboard. Any combined report must include an
   explicit force/moment mapping table.
7. The second largest technical gap is hydrodynamic interaction. The #2564/#2565
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
