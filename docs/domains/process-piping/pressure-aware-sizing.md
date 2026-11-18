# Pressure-aware line sizing

`size_line` can apply the existing straight-pipe pressure-thickness screen
while selecting a hydraulic NPS and schedule. Supply `pressure_design` to
recalculate the required nominal wall at each candidate outside diameter.
This is a screening calculation; it does not establish a complete piping design.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-11-05 | Add candidate-specific pressure selection to prevent reuse of a wall requirement calculated for another diameter. |

Table 1. Guide revision history.

## API and assumptions

```python
from digitalmodel.process_piping import Flow, PressureDesign, size_line

water = Flow("liquid", 50 / 3600, 998.2, 0.001002, 100, 0.000045)
design = PressureDesign(
    pressure_pa=8e6,
    outside_diameter_m=0.060325,
    allowable_stress_pa=138e6,
    quality_factor=1,
    weld_strength_factor=1,
    y=0.4,
    corrosion_allowance_m=0.003,
    mechanical_allowance_m=0,
    mill_tolerance_fraction=0.125,
)
selected = size_line(
    water,
    candidate_nps=(4, 6),
    schedules=("40", "80"),
    velocity_limit_m_s=4,
    pressure_design=design,
)
assert (selected.nps, selected.schedule) == (4, "80")
assert selected.pressure_requirement is not None
```

These inputs are synthetic assumptions, not qualified material properties.
The supplied design is a template: its outside diameter is replaced by each
catalog candidate's outside diameter, and the original object is unchanged.
All factors and allowances remain explicit. The caller must qualify their
applicability across the candidate materials, sizes and schedules.

The optional `minimum_wall_m` constraint still applies independently. A fixed
wall requirement calculated for one diameter does not establish pressure
suitability at another diameter. Hydraulic velocity and pressure-drop limits
also remain in force. The smallest eligible NPS is selected, with thinner
eligible schedules tried first. Rejection of every candidate raises `ValueError`
with reasons, including insufficient pressure wall or pressure-method limits.

## Hand check

Using the existing pressure helper with the stated factors, required nominal
wall is `[8 D / (2 (138 + 8 × 0.4)) + 0.003] / 0.875` in metres, with pressure
and stress expressed in MPa. The independent hand calculations below are
checked by `tests/process_piping/test_pressure_sizing.py`.

| Candidate | Outside diameter (mm) | Required nominal wall (mm) | Sch 40 wall (mm) | Selected schedule |
| --- | --- | --- | --- | --- |
| NPS 4 | 114.300 | 7.129 | 6.020 | 80 |
| NPS 6 | 168.275 | 8.877 | 7.112 | 80 |

Table 2. Each diameter evaluated separately with the synthetic pressure inputs.

Sch 40 meets the velocity limit in this example but fails the pressure-wall
constraint at both diameters. With both sizes available, NPS 4 Sch 80 is selected.
Adding a pressure-drop budget of 10 kPa selects NPS 6 Sch 80 instead.

## Basis and limits

The pressure equation, factors, allowances and validity checks reuse
`pressure_thickness`; the change introduces no new pressure formula or
material tables. Method locator: ASME B31.3-2024, 304.1.1, *General*, and
304.1.2, *Straight Pipe Under Internal Pressure*.
The existing [public equation reference](https://engstandards.lanl.gov/esm/pressure_safety/var_2010_001.pdf)
returned HTTP 403 on 2026-11-05. Independent confirmation of the current
edition remains open. No full current-edition compliance claim is made.

Dimensions and friction reuse the existing catalog and Darcy helper.
The [manufacturer's public dimensional sheet](https://www.wheatland.com/wp-content/uploads/2017/12/ASTM-A53-Schedule-40-Submittal-Sheet.pdf)
was retrieved on 2026-11-05 and corroborates the NPS 4 and NPS 6 Sch 40
dimensions used here. Sch 80 dimensions are taken from the existing catalog;
their independent public-source confirmation remains open.

The result exposes `pressure_requirement` and includes the pressure citations
alongside the hydraulic citations. Without `pressure_design`, this field is
`None` and selection uses the existing hydraulic and fixed-wall criteria.
Hydraulics use nominal catalog bore, without future corrosion enlargement.
The pressure constraint applies to liquid, gas and two-phase velocity screens;
two-phase pressure-drop sizing remains unsupported. Hydrotest yield screening,
bolted joints, external pressure, mechanical loads, fatigue and product
qualification require separate assessment. The existing hydrotest-edition
assumption is unresolved by this change.
