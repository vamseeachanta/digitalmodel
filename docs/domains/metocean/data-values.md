# Engineering value checks for data and shear flow

The three slices exercise stored pressure values, serialized pipe geometry,
registered engineering tables and loaded velocity fields. All examples use
synthetic inputs and independent hand calculations. They establish numerical
and data-interface behavior; they do not qualify a measured dataset or a design.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-21 | Added synthetic value checks and a worked example to cover missing engineering assertions. |
| 1 | 2026-10-21 | Rejected boolean and non-real calculation inputs, moved the example test to the dependency-bearing suite, and made the full-circle citation explicit. |

## Slice 1: pressure CSV retrieval

`digitalmodel.data.hydrostatic.write_hydrostatic_csv` writes `depth_m` and
`pressure_pa`. `read_pressure_csv` retrieves the persisted numbers without
recomputing them. The interface requires explicit density and gravity, rejects
boolean, non-real, nonfinite or negative calculation inputs, and validates all
rows before opening the output file. Numeric CSV cells are parsed explicitly on
retrieval. Pressure is gauge pressure relative to the fluid surface. Depth
increases downward; density and gravity are constant throughout the profile.

The basis is `p = rho*g*h`, obtained by subtracting surface pressure in
[OpenStax, University Physics Volume 1 (2016), section 14.1, equation 14.9](https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure).

The worked example chooses `rho = 1000.0 kg/m³`, `g = 10.0 m/s²` and depths
`0.0`, `2.0`, `5.0 m`. Multiplication gives `0.0`, `20000.0`, `50000.0 Pa`.
These are assumed arithmetic inputs, not physical defaults or site data.
The CSV schema states SI units; it does not retain fluid-property provenance.

## Slice 2: serialized geometry and panel force

`digitalmodel.data_models.pipe_section.PipeSection` requires both diameters in
metres and supports JSON round trips. Material area is the difference of circle
areas, `A = pi*(Do - Di)*(Do + Di)/4`. The factor of four follows from diameter
being twice radius. The sector-area equation `A = θ*r²/2`, with θ = 2π radians
for a full circle, gives `A = pi*r²`, using
[OpenStax, Precalculus 2e (2021), section 5.1, Area of a Sector](https://openstax.org/books/precalculus-2e/pages/5-1-angles).
For chosen diameters `4.0 m` and `2.0 m`, the radii are `2.0 m` and `1.0 m`:
`A = (4 - 1)*pi = 3*pi m²`, displayed as `9.4 m²`.

The existing `DataCatalog` saves and reloads catalogue metadata and retrieves
CSV or JSON table payloads. For constant-pressure parallel panels, the normal
force is `sum(p_i*A_i)`, from `p = F/A` in
[OpenStax, University Physics Volume 1 (2016), section 14.1, equation 14.3](https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure).
The chosen panels have pressures `20000.0`, `50000.0 Pa` and areas `3.0`, `2.0 m²`.
The hand sum is `20000*3 + 50000*2 = 160000.0 N`. This is exact integration of
piecewise uniform pressure over the stated panels. It does not infer a spatial
interpolant or add force vectors with differing normals.

## Slice 3: vorticity from a loaded linear field

`vorticity_from_velocity` accepts one snapshot with shape `(2, H, W)`:
components `(u, v)`, rows increasing along `y`, columns increasing along `x`.
Grid spacings are required positive finite real inputs in metres; boolean and
non-real spacings are rejected. Velocities are in metres per second. The output
has shape `(1, H, W)` in inverse seconds.

The z component is `omega_z = dv/dx - du/dy`, the Cartesian curl in
[OpenStax, Calculus Volume 3 (2016), section 6.5, equation 6.17](https://openstax.org/books/calculus-volume-3/pages/6-5-divergence-and-curl).
The chosen field is `u = 3*y + 2*x + 7`, `v = 5*x - 4*y - 1`, giving
`omega_z = 5 - 3 = 2.0 s^-1`. Tests stream this analytic field through the
loader with a local synthetic dataset substitute and then compute its curl.
Existing positive, negative and zero curl retrieval tests remain in place.

Interior derivatives use centred difference quotients; boundary derivatives
use one-sided quotients, without periodic wrapping. For an affine field,
`[f(x+h)-f(x-h)]/(2*h)` and `[f(x+h)-f(x)]/h` both reduce to its slope.
This hand derivation supplies the numerical coefficients. The tests use
exactly representable coordinates and coefficients, so exact equality applies;
there is no empirical tolerance or design acceptance limit.

## Run the worked example

With the package's runtime dependencies available, choose an output directory:

```bash
PYTHONPATH=src python examples/data/engineering_value_checks.py "$OUTPUT_DIR"
```

The script creates the pressure CSV, section JSON, panel CSV and catalogue in
that directory, checks the unrounded answers, and prints:

```text
deepest_pressure_pa: 50000.0
pipe_area_m2: 9.4
panel_force_n: 160000.0
vorticity_per_s: 2.0
```

## Scope and limits

The checks require no external dataset, download, tensor framework or solver.
The synthetic loader substitute tests the documented local snapshot contract;
it does not establish compatibility with a live upstream dataset release.
Density variation, absolute pressure, nonuniform grids, periodic boundary
stencils and turbulence convergence remain outside these slices. No engineering
acceptance criterion is inferred from these arithmetic fixtures.
