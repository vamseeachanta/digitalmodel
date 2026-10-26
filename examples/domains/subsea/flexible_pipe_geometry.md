# Flexible-pipe geometry example

This example demonstrates contiguous synthetic full-wall layers using the
existing `Layer` and `FlexiblePipeSection` classes. It calculates geometry and
mass per unit length only. Every input below is chosen for this arithmetic
example; none represents a qualified material or a reference pipe design.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-26 | Added synthetic inputs and independent hand calculations for a runnable geometry example. |

## Run offline

From the repository root, with Python and uv already available:

```bash
PYTHONPATH=src uv run --no-project --offline python examples/domains/subsea/flexible_pipe_geometry.py
```

The script prints diameters in m, annular areas in m^2 and masses per unit
length in kg/m. Areas display nine decimal places and masses six to make
rounding reproducible; this display precision does not express input accuracy.

## Synthetic inputs

The table is the source of all layer inputs in the script and tests. Layer
types label the API objects; each layer occupies its entire annular wall.

| Layer type | Inner diameter (m) | Radial thickness (m) | Density (kg/m^3) |
| --- | --- | --- | --- |
| `pressure_sheath` | 0.100 | 0.010 | 1000 |
| `anti_wear` | 0.120 | 0.020 | 2000 |
| `outer_sheath` | 0.160 | 0.020 | 500 |

The external-fluid density is explicitly chosen as 800 kg/m^3 and the bore-fluid
density as 600 kg/m^3. Neither calculation uses a default fluid density.

## Independent hand calculation

Source: elementary circle geometry, A = pi r^2. A diameter spans two radii,
so r = D/2 and an annulus has A = pi (r_outer^2 - r_inner^2). For a unit
length of 1 m, its volume is A times 1 m, so mass per unit length is rho A
in kg/m. The following calculations use those radii directly, independently
of the implementation's mean-diameter mass formula.

Each outer diameter is D_inner + 2t. The layer outer diameters are therefore
0.120, 0.160 and 0.200 m. Adjacent diameters coincide, giving a bore diameter
of 0.100 m and an overall outer diameter of 0.200 m.

| Layer | Inner radius (m) | Outer radius (m) | Annular area (m^2) | Dry mass per length (kg/m) |
| --- | --- | --- | --- | --- |
| `pressure_sheath` | 0.050 | 0.060 | pi (0.060^2 - 0.050^2) = 0.0011 pi | 1000 (0.0011 pi) = 1.1 pi |
| `anti_wear` | 0.060 | 0.080 | pi (0.080^2 - 0.060^2) = 0.0028 pi | 2000 (0.0028 pi) = 5.6 pi |
| `outer_sheath` | 0.080 | 0.100 | pi (0.100^2 - 0.080^2) = 0.0036 pi | 500 (0.0036 pi) = 1.8 pi |

The three annuli sum to 0.0075 pi m^2, also obtained as
pi (0.100^2 - 0.050^2). Total dry mass is
(1.1 + 5.6 + 1.8) pi = 8.5 pi kg/m.

The displaced-fluid calculation uses the entire outer-envelope circle,
including the bore: A_envelope = pi (0.100^2) = 0.0100 pi m^2.
Displaced mass per length is 800 (0.0100 pi) = 8 pi kg/m.
The bore circle has A_bore = pi (0.050^2) = 0.0025 pi m^2.
Content mass per length is 600 (0.0025 pi) = 1.5 pi kg/m.

The subprocess test stores the decimal expansions of these exact expressions,
rounded to the script's display precision, as literal expected output.

## Layer-gap check

For the rejection case, change only the second layer's inner diameter from
0.120 m to the deliberately chosen 0.122 m. Its gap from the first layer's
outer diameter is 0.122 - 0.120 = 0.002 m. This exceeds the existing
`FlexiblePipeSection.tol` default of 0.000001 m (defined in
`src/digitalmodel/subsea/flexible_pipes/cross_section.py`), so construction
raises `ValueError` with the message `layers are not contiguous`.

## Limits and way forward

These full walls do not represent wound wires, interlocked armour, voids or a
complete manufactured pipe. The example supplies no armour stiffness, standard
design limits or licensed reference values and claims no API 17B/17J design
qualification. Fatigue, structural capacity, layer interaction, bend-stiffener
sizing and catenary integration require separate qualified inputs and analysis.
The parent issue remains open for that work.
