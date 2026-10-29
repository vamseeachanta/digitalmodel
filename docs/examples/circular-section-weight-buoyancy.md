# Circular-section weight and buoyancy example

This worked example calculates the geometry, dry mass and submerged mass
equivalent of a synthetic coated circular pipe using the existing
`PipeCrossSection` calculator. All dimensions and densities are teaching inputs;
they are not material specifications or design recommendations.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-27 | Add an executable hand calculation to document units and displacement assumptions. |

## Input and calculation basis

**S1 — synthetic inputs.** Each value below is independently selected for this
example. The API's steel, LPP and concrete labels identify concentric layers;
their densities do not come from an external material catalogue.

| Input | Synthetic value | API argument |
| --- | --- | --- |
| Carrier outside diameter | 200 mm | `steel_od_mm` |
| Carrier radial wall thickness | 10 mm | `steel_wt_mm` |
| Carrier density | 8000 kg/m³ | `steel_density` |
| First coating radial thickness | 5 mm | `lpp_thickness_mm` |
| First coating density | 1000 kg/m³ | `lpp_density` |
| Outer coating radial thickness | 20 mm | `concrete_thickness_mm` |
| Outer coating density | 2400 kg/m³ | `concrete_density` |
| Surrounding water density | 1000 kg/m³ | `seawater_density` |
| Bore contents | Air, with contents mass excluded | `internal_contents="air"` |

**U1 — unit definitions.** The SI prefix milli gives 1000 mm per metre and
kilo gives 1000 N per kN. A unit length is 1 m. Consequently, area in m²
times that length gives volume in m³, and area times density gives kg/m.

**G1 — force convention.** For this example, use the calculator's fixed gravity
of 9.81 m/s², as implemented in
[`total_weight_in_air_kN_m`, `buoyancy_force_kN_m` and `submerged_weight_kN_m`](../../src/digitalmodel/structural/pipe_cross_section/calculator.py).
This is an explicit calculation convention, not a site gravity measurement.

## Independent hand calculation (H1)

For a circular annulus, subtract the inner circle area from the outer circle
area: `A = pi * (OD² - ID²) / 4`. The divisor follows from radius being half
the diameter. Thickness is radial, so each coating increases diameter by twice
its thickness.

Convert S1 dimensions to metres using U1 **before squaring**:

- Carrier bore: `(200 - 2 * 10) / 1000 = 0.180 m`.
- Carrier outside: `200 / 1000 = 0.200 m`.
- First coating outside: `(200 + 2 * 5) / 1000 = 0.210 m`.
- Outer coating outside: `(210 + 2 * 20) / 1000 = 0.250 m`.

The following exact coefficients retain pi until the final display rounding.

| Layer | Area in m², from annulus geometry | Mass per length in kg/m, area times S1 density |
| --- | --- | --- |
| Carrier | `pi/4 * (0.200² - 0.180²) = 0.0019 pi` | `0.0019 pi * 8000 = 15.2 pi` |
| First coating | `pi/4 * (0.210² - 0.200²) = 0.001025 pi` | `0.001025 pi * 1000 = 1.025 pi` |
| Outer coating | `pi/4 * (0.250² - 0.210²) = 0.0046 pi` | `0.0046 pi * 2400 = 11.04 pi` |

Table H1. Independent layer geometry and mass arithmetic from S1 and U1.

Dry mass per length is `(15.2 + 1.025 + 11.04) pi = 27.265 pi kg/m`.
With full immersion and a sealed, empty bore, water is excluded from the entire
exterior circle. Displaced volume per length is therefore
`pi/4 * 0.250² = 0.015625 pi m³/m`, including the bore volume.
The displaced water mass is `1000 * 0.015625 pi = 15.625 pi kg/m`.
Dry mass minus displaced water mass gives a submerged mass equivalent of
`(27.265 - 15.625) pi = 11.64 pi kg/m`, positive downward.

The API retains legacy `weight` and `force` names for some kg/m properties.
Those values are **mass per length** or a mass equivalent, not force per length.
Using G1 and U1, submerged force is `11.64 pi * 9.81 / 1000 kN/m`.
The same conversion applies to dry weight and buoyancy force.

## Runnable example

From the repository root, with the package dependencies installed, run
`PYTHONPATH=src uv run python`, then paste the following snippet. It writes
only to standard output and uses the grouped public import path.

```python
from digitalmodel.structural.pipe_cross_section import PipeCrossSection

# All dimensions (mm) and densities (kg/m^3) are synthetic inputs from S1.
pipe = PipeCrossSection(
    steel_od_mm=200,
    steel_wt_mm=10,
    steel_density=8000,
    lpp_thickness_mm=5,
    lpp_density=1000,
    concrete_thickness_mm=20,
    concrete_density=2400,
    seawater_density=1000,
    internal_contents="air",
)

# H1 results, rounded only for display; kg/m and kN/m have different meanings.
print(f"Dry mass: {pipe.total_weight_in_air_kg_m:.1f} kg/m")
print(f"Displaced volume: {pipe.displaced_volume_m3_m:.6f} m^3/m")
print(f"Displaced water mass: {pipe.buoyancy_force_kg_m:.1f} kg/m")
print(f"Submerged mass equivalent: {pipe.submerged_weight_kg_m:.1f} kg/m")
print(f"Submerged force: {pipe.submerged_weight_kN_m:.3f} kN/m")
```

Expected output, obtained from H1 with G1 and U1:

```text
Dry mass: 85.7 kg/m
Displaced volume: 0.049087 m^3/m
Displaced water mass: 49.1 kg/m
Submerged mass equivalent: 36.6 kg/m
Submerged force: 0.359 kN/m
```

The tests in
[`test_pipe_calculator.py`](../../tests/structural/pipe_cross_section/test_pipe_calculator.py)
execute this exact Python block and compare the layer areas and masses,
displacement, mass balance and force conversions with H1. Full precision is used
for assertions; the output above is rounded for readability.

## Geometry and weight limits

The calculation assumes concentric, uniform circular layers, constant densities,
full immersion, and a sealed bore containing negligible-mass air. It excludes
end fittings, end effects, attachments and air buoyancy. It does not model
partial immersion, flooding, or the mass of internal contents: setting
`internal_contents` or `internal_fluid_density` does not add contents mass to
the current calculator's layer sum. Do not apply this example's displacement
assumption to an open or flooded pipe.

This example checks geometry and weight arithmetic only. It establishes no
strength, buckling, lifting or code capacity rating. The parent issue #1847
remains open: a general section-properties interface, a standard-shape catalogue,
bolted-flange rating, padeye checks, and API RP 2A WSD member and tubular-joint
capacity remain outside this example.
