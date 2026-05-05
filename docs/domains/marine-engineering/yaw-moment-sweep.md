# Yaw moment sweep for a typical ship

Issue: [workspace-hub #2564](https://github.com/vamseeachanta/workspace-hub/issues/2564)

This workflow evaluates preliminary rudder-induced yaw moment about the vessel
CG over a grid of forward speeds and rudder angles. It is a first-cut lever-arm
calculation, not a full MMG model, IMO maneuverability prediction, class-rule
calculation, or dynamic yaw-response simulation.

## Traceability links

| Item | GitHub link |
|---|---|
| Yaw-moment issue | [workspace-hub #2564](https://github.com/vamseeachanta/workspace-hub/issues/2564) |
| Calculation source | [`yaw_moment.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/yaw_moment.py) |
| Rudder force source | [`maneuverability.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/maneuverability.py) |
| Packaged input YAML | [`yaw_moment_typical_ship.yml`](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/yaw_moment_typical_ship.yml) |
| Validation tests | [`test_yaw_moment_sweep.py`](https://github.com/vamseeachanta/digitalmodel/blob/main/tests/naval_architecture/test_yaw_moment_sweep.py) |
| Master calculation review | [`rudder-and-ship-force-calculation-review.md`](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/domains/marine-engineering/rudder-and-ship-force-calculation-review.md) |

## Packaged input

The packaged sample input is available through `importlib.resources` at:

```text
digitalmodel.naval_architecture.data/yaw_moment_typical_ship.yml
```

Key input sections:

- `case`: case identifier and description.
- `vessel`: descriptive typical-ship dimensions.
- `rudder`: rudder area, span, aft/forward lever arm from CG, and `behind_hull` flag.
- `sign_convention`: `+x` forward, `+y` port, `+z` up; positive yaw moment is bow-to-port.
- `environment`: seawater density.
- `sweep`: forward speeds in `kn` or `m/s`, plus rudder angles in degrees.
- `outputs`: CSV/JSON table formats and required chart set.

## Methodology

The calculation reuses the existing maneuverability helper:

```python
rudder_normal_force(
    velocity_m_s=...,
    rho_kg_m3=...,
    rudder_area_m2=...,
    rudder_span_m=...,
    rudder_angle_deg=...,
    behind_hull=...,
)
```

The yaw moment is then computed as:

```text
M_z = x_rudder_from_cg_m * transverse_force_N
```

The existing helper returns a signed scalar normal force, so the yaw workflow
makes the force-direction mapping explicit with `positive_force_direction`:

- `port`: positive scalar normal force is reported as positive transverse force.
- `starboard`: positive scalar normal force is mapped to negative transverse force.

## Propeller rotation factor `Cr`

The reusable yaw-moment sweep does not use the B1528 workbook propeller rotation
factor `Cr`. `Cr` belongs to the legacy workbook-regression formula
`F = beta * AR * V^2 * Cr`, not to the Whicker/Fehlner-style rudder normal-force
helper used here. If that legacy workbook formula is run for a non-rotating
propeller, the neutral value is `Cr=1.0`.

## Sample working example

For a visual and arithmetic check, use:

```text
V = 5.0 m/s
rho = 1025 kg/m^3
rudder area = 20.0 m^2
rudder span = 5.0 m
rudder angle = +10 deg
behind_hull = false
x_rudder_from_cg = -45.0 m
positive_force_direction = port

scalar normal force = 97417.403 N
transverse force = 97417.403 N
Mz = -45.0 * 97417.403 = -4383783.130 N*m = -4383.783 kN*m
```

The generated yaw-moment charts should show the `+10 deg`, `5 m/s` equivalent
point on the negative yaw-moment side for a stern rudder with the stated sign
convention.

## Outputs

`write_yaw_moment_results(...)` writes:

- `yaw_moment_sweep.csv` with unit-suffixed, stable headers.
- `yaw_moment_sweep.json` with `metadata`, `provenance`, `rows`, and `artifacts`.
- Required PNG/HTML charts:
  - yaw moment vs rudder angle by speed;
  - yaw moment vs speed by rudder angle;
  - transverse force vs rudder angle by speed;
  - speed/angle yaw-moment heatmap.

PNG charts use Matplotlib. HTML charts use self-contained Plotly output.

## Provenance and limitations

Output provenance states that this workflow uses the existing Whicker & Fehlner
rudder-force basis documented in `maneuverability.py`, but it does not introduce
a new standards-derived numeric constant. Therefore it does not fabricate a
strict class-rule `Citation` object. Future standards-derived coefficients or
limits must add the strict citation schema required by workspace-hub policy.
