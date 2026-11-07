# Layered pipe buoyancy worked example

## Summary

This example evaluates a bare pipe and the same pipe with a single concentric
coating using explicit synthetic inputs. Circle integrals give each material
area and mass per length, and the outer displaced area gives the displaced
water mass. Subtraction gives submerged mass equivalents of **15.0 kg/m** for
the bare pipe and **15.6 kg/m** for the coated pipe, rounded for display.
These quantities are mass equivalents, not forces or engineering acceptance
decisions. The calculation below and its executable test establish these values.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-25 | Added synthetic bare and coated cases with an executable example and independent integral checks. |

## Inputs

All dimensions and densities in this table are **assumed synthetic inputs**,
chosen for this example; they represent no material grade, measured water
condition or existing design. No default material density is used for an
active layer. The coating uses the calculator's `lpp_*` input fields solely as
the interface for a concentric coating.

| Input | Bare pipe | Coated pipe | Basis |
| --- | ---: | ---: | --- |
| Steel outside diameter, mm | 200.25 | 200.25 | Synthetic assumption |
| Steel radial wall thickness, mm | 10.125 | 10.125 | Synthetic assumption |
| Steel density, kg/m³ | 7800.25 | 7800.25 | Synthetic assumption |
| Coating radial thickness, mm | 0 | 5.250 | Synthetic assumption; zero omits the layer |
| Coating density, kg/m³ | 1200.75 | 1200.75 | Synthetic assumption; unused for bare pipe |
| Concrete radial thickness, mm | 0 | 0 | Synthetic assumption; no concrete layer |
| Water density, kg/m³ | 1020.5 | 1020.5 | Synthetic assumption |
| Internal fluid density, kg/m³ | 0 | 0 | Assumed negligible internal air mass |

The idealized pipe is fully immersed with a sealed, dry bore. End effects and
closure mass are neglected. Flooding would change the displaced volume basis;
the full outer disk applies here because water cannot enter the bore.
`internal_contents="air"` is a label, not an instruction to add air or fluid
mass. The example explicitly sets internal fluid density to zero and sums only
the material layers.

The millimetre-to-metre conversion is exact: the SI prefix milli means
one thousandth, so `1000 mm = 1 m`
([NIST SI prefixes](https://physics.nist.gov/cuu/Units/prefixes.html)).
The only mathematical constant needed is π, evaluated with `math.pi`
([Python mathematical constants](https://docs.python.org/3/library/math.html#math.pi)).
The factors of two below come from diameter being twice radius; they are
geometric identities, not fitted or physical constants. No gravity value is
chosen or needed for a mass-equivalent calculation.

## Derivation

For a concentric annulus with inner radius r, outer radius R and constant
density ρ, the polar area element is s ds dθ. Integrating around the circle
and through its radius gives

\[
A(r,R)=\int_0^{2\pi}\int_r^R s\,ds\,d\theta
=\pi(R^2-r^2),\qquad
m'=\int_A\rho\,dA=\rho A.
\]

Here m′ is mass per axial length in kg/m: volume per length has the same
numerical value as cross-sectional area in m². The input dimensions give a
steel bore diameter of 180.0 mm. Exact radii in metres are

\[
r_s=9/100,\qquad R_s=801/8000,\qquad R_c=843/8000.
\]

The steel annulus spans r_s to R_s; the coating spans R_s to R_c. The outer
disk displaces water, including the sealed bore. Its integral starts at the
axis rather than at the steel inner radius:

\[
A_d(R)=\int_0^{2\pi}\int_0^R s\,ds\,d\theta=\pi R^2.
\]

| Quantity | Exact area, m² | Independent calculation |
| --- | --- | --- |
| Steel annulus, both cases | π × 123201 / 64000000 | π × [(801/8000)² − (9/100)²] |
| Coating annulus, coated case | π × 8631 / 8000000 | π × [(843/8000)² − (801/8000)²] |
| Displaced disk, bare case | π × 641601 / 64000000 | π × (801/8000)² |
| Displaced disk, coated case | π × 710649 / 64000000 | π × (843/8000)² |

All coefficients in this table follow directly from the synthetic inputs and
the circle integrals above; they are not calculator outputs. For the bare case
use the steel annulus alone. For the coated case add both annuli:

\[
m'_{\rm air}=\sum_j \rho_j A_j,\qquad
m'_{\rm water}=\rho_w A_d,\qquad
m'_{\rm sub}=m'_{\rm air}-m'_{\rm water}.
\]

The displaced-water subtraction follows the
[OpenStax buoyancy principle](https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy):
buoyant force equals displaced-fluid weight. Dividing both gravitational
weight and buoyant force by the same local gravitational acceleration leaves
the mass-equivalent expression above. A positive result means the material
mass exceeds displaced water mass under these assumptions; it does not
establish a stability or installation criterion.

| Result, kg/m | Bare pipe | Coated pipe | Basis |
| --- | ---: | ---: | --- |
| Steel mass per length | 47.2 | 47.2 | Steel area × assumed steel density |
| Coating mass per length | 0.0 | 4.1 | Coating area × assumed coating density |
| Total material mass per length | 47.2 | 51.2 | Sum before rounding |
| Displaced water mass per length | 32.1 | 35.6 | Outer disk area × assumed water density |
| Submerged mass equivalent per length | 15.0 | 15.6 | Material mass minus displaced water mass before rounding |

Results in this table use one decimal place. Calculations retain full
precision until the displayed result is rounded; adding or subtracting the
displayed entries can therefore differ from the rounded result.

## Executable example

With the package and its declared Pint dependency available, save the single
Python block below as a script and pass an existing temporary directory as
its sole argument: `python example.py TEMP_DIRECTORY`. The block runs offline
and writes only `layered-pipe-buoyancy.json` inside the supplied directory.
The automated test extracts this exact block and executes it outside the
checkout; it does not repeat the calculator calls.

```python
import json
from pathlib import Path
import sys

from digitalmodel.structural.pipe_cross_section import PipeCrossSection


output_directory = Path(sys.argv[1])
output_directory.mkdir(parents=True, exist_ok=True)
results = {}
for name, coating_mm in (("bare", 0.0), ("coated", 5.25)):
    pipe = PipeCrossSection(
        steel_od_mm=200.25,
        steel_wt_mm=10.125,
        steel_density=7800.25,
        lpp_thickness_mm=coating_mm,
        lpp_density=1200.75,
        concrete_thickness_mm=0.0,
        seawater_density=1020.5,
        internal_contents="air",
        internal_fluid_density=0.0,
    )
    results[name] = {
        "live": {
            "layers": [
                {
                    "inner_diameter_mm": layer.inner_diameter_mm,
                    "outer_diameter_mm": layer.outer_diameter_mm,
                    "thickness_mm": layer.thickness_mm,
                    "density_kg_m3": layer.density_kg_m3,
                    "area_m2": layer.cross_sectional_area_m2,
                    "mass_kg_m": layer.weight_per_meter_kg,
                }
                for layer in pipe.layers
            ],
            "mass_in_air_kg_m": pipe.total_weight_in_air_kg_m,
            "displaced_area_m2": pipe.displaced_volume_m3_m,
            "displaced_water_mass_kg_m": pipe.buoyancy_force_kg_m,
            "submerged_mass_kg_m": pipe.submerged_weight_kg_m,
        },
        "serialized": pipe.to_dict(),
    }

(output_directory / "layered-pipe-buoyancy.json").write_text(
    json.dumps(results, indent=2, allow_nan=False) + "\n", encoding="utf-8"
)
```

### Numeric retention and summary rounding

`live` records the unrounded geometry and mass-equivalent properties. In
`serialized`, `to_dict()` retains numeric configuration inputs such as
`steel_od_mm=200.25`, `steel_wt_mm=10.125` and `steel_density=7800.25`.
It does **not** retain unrounded derived masses or areas. Specifically:

- Layer `Area_m2_m` is rounded to five decimal places and `Weight_kg_m` to
  one; layer diameters are rounded to one and thickness to two.
- Both `summary` and `buoyancy` round `Displaced_Volume_m3_m` to four decimal
  places: **0.0315** for bare and **0.0349** for coated, in m³/m.
- `summary.Total_Weight_Air_kg_m` is **47.2** or **51.2**;
  `Buoyancy_Force_kg_m` is **32.1** or **35.6** and
  `Submerged_Weight_kg_m` is **15.0** or **15.6**, respectively, in both
  `summary` and `buoyancy` where those keys exist.

Despite the existing names containing `Weight` or `Force`, the `kg_m` fields
are mass equivalents. The displaced-volume field is volume per length,
numerically equal to displaced area; the layer area field's name also carries
an extra `_m` but represents m². Tests compare unrounded live values with the
exact integral coefficients, then check configuration retention and rounded
layer, summary and buoyancy fields separately. Relative tolerance is 1e-12,
with absolute floors of 1e-15 m² and 1e-10 kg/m. These are numerical roundoff
allowances for a few binary floating-point operations, many orders below the
serialization increments; they are not engineering acceptance tolerances.

## Limits

The geometry is uniform, circular and concentric with homogeneous layers and
constant assumed water density. Internal contents mass, flooding, end caps,
porosity, water absorption, corrosion and dimensional tolerances are excluded.
Changing the internal contents label alone does not add an internal-fluid mass
term to the current layer sum. Real inputs require their own data basis.

The exported dictionary also contains force and inch fields generated by the
existing serializer. This example does not use or validate those fields, adopt
a fixed gravity conversion, generate plots, select sizes or assess engineering
acceptance. The numerical methods remain unchanged. This is the worked-example
slice; the full module report, builder and index remain separate work.
