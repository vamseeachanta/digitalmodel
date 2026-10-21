# Tank gauging

Purpose: convert tank-axis sounding or ullage into volume, mass and a
small-angle rectangular free-surface GM loss. All examples are synthetic;
no real calibration, liquid specification or stability approval is supplied.

## Entry points and units

Import from `digitalmodel.naval_architecture.tank_gauging`.

| Function | Inputs | Output |
| --- | --- | --- |
| `CalibrationTable(points, reading_kind, reference_height_m)` | `(depth_m, volume_m3)` pairs; `sounding` or `ullage`; height in m | Immutable calibration |
| `volume_at(depth_m)` | Declared reading kind, m | Volume, m³ |
| `depth_at_volume(volume_m3)` | Volume, m³ | Declared reading kind, m |
| `sounding_to_ullage`, `ullage_to_sounding` | Depth and common reference height, m | Complementary depth, m |
| `centre_sounding` | Gauge sounding and height, m; signed offsets, m; independent surface slopes, degrees | Centre sounding, m |
| `density_at_temperature` | Reference density, kg/m³; reference and measured temperature, °C; volumetric expansion coefficient, 1/°C | Density, kg/m³ |
| `liquid_mass_kg` | Volume, m³; density at the same condition, kg/m³ | Mass, kg |
| `rectangular_free_surface_moment_kg_m` | Longitudinal length, transverse breadth, m; density, kg/m³; required fill state | Mass moment, kg m |
| `gm_loss_m` | Free-surface mass moment, kg m; displacement mass, kg | GM loss, m |

Table 1. Public functions use SI mass units throughout; the free-surface moment is not torque.

## Calibration input

CSV has exactly `depth_m,volume_m3` columns. Supply `reading_kind` and
`reference_height_m` to `CalibrationTable.from_csv`. YAML is read by
`CalibrationTable.from_yaml` and has exactly this structure:

```yaml
reading_kind: sounding
reference_height_m: 3
rows:
  - {depth_m: 0, volume_m3: 0}
  - {depth_m: 1, volume_m3: 40}
  - {depth_m: 3, volume_m3: 120}
```

These synthetic rows describe a box with 40 m² plan area. Depths must
increase strictly; volumes must increase for sounding and decrease for
ullage. At least two rows are required. Partial tables are permitted.
The table endpoints bound both lookup directions. Extrapolation, repeated
depths, repeated volumes, duplicate YAML keys, nonfinite numbers and unsupported columns raise
`ValueError`. Source-file access errors retain their native exception.
Calibration importers must reject duplicate mapping keys before schema
validation; silent replacement can change a reading convention or volume.

## Method and sources

- Linear interpolation uses a two-point line: [OpenStax, Precalculus 2e
  (2021), §2.1](https://openstax.org/books/precalculus-2e/pages/2-1-linear-functions).
  Sounding and ullage are complementary depths from a common datum: `h + u = H`.
- The surface convention is `h(x,y) = h_c + x tan(trim) + y tan(list)`.
  Positive slopes raise the surface towards positive offsets, so the
  gauge-to-centre correction is subtracted. Basis: [OpenStax, Precalculus
  2e (2021), §5.4](https://openstax.org/books/precalculus-2e/pages/5-4-right-triangle-trigonometry).
- `m = rho V`: [OpenStax, University Physics vol. 1 (2016),
  §14.1](https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure).
- `rho(T) = rho_ref / (1 + beta (T - T_ref))` follows constant mass and
  linear volume expansion, [OpenStax, University Physics vol. 2 (2016),
  §1.3, eq. 1.4](https://openstax.org/books/university-physics-volume-2/pages/1-3-thermal-expansion).
  Density, coefficient and their applicable temperature range are caller responsibilities.
- `i_t = L B³/12`, `FSM = rho i_t`, `FSC = FSM/displacement_mass`:
  [USNA, EN400 Principles of Ship Performance (Summer 2020), §4.8.3–4.8.3.1,
  pp. 4-29–4-30](https://www.usna.edu/NAOE/_files/documents/Courses/EN400/EN400_Course_Notes_Summer_2020.pdf).
  The divisor 12 follows integration of `y² dA` over the rectangle. No gravity
  factor is required because displacement is supplied as mass.

## Limits and deferred work

`centre_sounding` converts tank-axis depths, rather than an inclined tape
length or an earth-vertical measurement. The two angles describe surface
slopes, not Euler rotations. Angles at or beyond vertical are refused as
outside this geometric representation; no operational angle limit is implied.
The function checks gauge and centre depth but cannot detect roof or bottom
clipping elsewhere without geometry. A corrected depth determines volume
only for an unclipped prismatic tank or a separately qualified calibration.

The free-surface helper requires the state `slack`, `empty` or `pressed_full`.
Only the latter two return zero. A slack tank close to full still has a free
surface. The rectangular expression assumes the whole rectangular surface
is exposed and a small-angle stability analysis. Baffles, subdivisions,
large angles, dynamic sloshing and overall stability criteria are not evaluated.

Attitude-indexed calibration/correction columns are deferred and are refused
explicitly. No correction is silently ignored or applied twice. Temperature
correction is a caller-qualified linear approximation, without tank thermal
expansion, phase change or custody-transfer procedures.

The hand calculation and three-significant-figure results are in
[the box-tank analysis](../../../../../docs/domains/naval-architecture/tank-gauging-box-example.html).
Tests are `tests/naval_architecture/test_tank_calibration.py`,
`test_tank_contents.py` and `test_tank_free_surface.py`.
The existing naval-architecture quality gate recursively runs that folder.
