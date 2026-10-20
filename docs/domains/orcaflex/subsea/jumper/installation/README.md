# Jumper installation input

The domain specification is converted to `ProjectInputSpec`, emitted as a
modular YAML model, and checked by the repository's solver-free validators.
The retained inputs in `generated/jumper-a` and `generated/jumper-b` each contain
27 line sections. No solver run was made; solver loadability and installation
performance are not established by these checks.

The optional `installation` block now adds supplied vessel definitions and
explicit suspended-stage attachments. Licensed `.dat` proof remains in #604.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-14 | Retain generated inputs and document generation and validation so the specification-to-input path can be reproduced. |
| 1 | 2026-10-20 | Document conversion assumptions and explicit vessel attachments so emitted installation geometry can be reviewed. |

Table 1. Revision history.

## Generation

Select an existing domain specification as `SPEC`, and a destination outside the
checkout as `OUTPUT`. The existing source specifications remain unchanged.

```bash
PYTHONPATH=src uv run --no-sources python -m \
  digitalmodel.marine_ops.installation.jumper_input \
  generate "$SPEC" "$OUTPUT" --name jumper-a
```

`build_modular_spec` uses the existing jumper calculations to construct line
types, section rows, an assumed M-profile, water and metocean properties, and
simulation controls. `generate_input` replaces descriptive source identifiers
with the supplied neutral name, validates `ProjectInputSpec`, then calls
`ModularModelGenerator.from_spec`. Numerical settings are preserved.

The generated files are `master.yml`, `inputs/parameters.yml`, and
`includes/01_general.yml`, `includes/03_environment.yml`, and
`includes/20_generic_objects.yml`. The two retained models correspond to the
first and second source specifications in lexicographic path order. Their
current section geometry is identical; environmental inputs differ. The
integration test compares regenerated and retained YAML values for both models.

## Validation

```bash
PYTHONPATH=src uv run --no-sources python -m \
  digitalmodel.marine_ops.installation.jumper_input \
  validate docs/domains/orcaflex/subsea/jumper/installation/generated/jumper-a

PYTHONPATH=src uv run --no-sources python -m \
  digitalmodel.marine_ops.installation.jumper_input \
  validate docs/domains/orcaflex/subsea/jumper/installation/generated/jumper-b
```

Each command reports `PASS: repository solver-free input validation; no solver
run` when the checks pass. Generation runs the same checks automatically.
Validation requires parseable master and include YAML, existing includes within
the model directory, a parameters file, no collection collisions, and no
cross-object reference errors. Warnings from the existing format validator do
not establish solver acceptance.

```bash
PYTHONPATH=src uv run --no-sources python -m pytest \
  tests/solvers/orcaflex/modular_generator/test_jumper_input.py \
  -q -rA -p no:cacheprovider --basetemp="$TEST_TEMP"
```

Set `TEST_TEMP` to a dedicated test output directory. The integration test
requires 27 positive-length sections with resolved line types, complete include
references, and matching retained inputs. Fresh subprocesses fail if the solver
binding is imported or looked up, even if an optional import catches the error.
Negative tests reject missing includes, invalid format and escaping include
paths.

## Basis and limits

Existing repository data is used as **assumed, based on previous experience**.
The M-profile orientation, hub clearance, equivalent coating and mesh settings
retain the existing converter's assumptions. Source provenance requires
maintainer confirmation; these inputs are not qualified engineering results.
Vessel and crane dynamics, lift, splash-zone, lowering and landing results
require later analysis. No licensed solver or licence check is invoked by these
commands.

## Canonical inputs and method

`docs/domains/orcaflex/subsea/jumper/installation/` is the canonical location
for domain `spec.yml` inputs. The older floating-pipeline pattern is not the
jumper input contract. Domain specs are not directly valid `ProjectInputSpec`
inputs: conversion supplies the `generic` model envelope.

```python
from digitalmodel.marine_ops.installation.jumper_to_modular_spec import build_modular_spec
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

data = build_modular_spec("domain.yml")
model = ProjectInputSpec(**data)
ModularModelGenerator.from_spec(model).generate("output")
```

The method loads the domain YAML, runs the jumper calculations, builds line
types and line sections, and copies environment settings into the generator
schema. Vessel definitions, when supplied, join the line objects in
`includes/20_generic_objects.yml`. No solver API is imported or invoked.

## Explicit vessel and lift-stage inputs

The optional `installation` block contains:

| Field | Meaning and source |
| --- | --- |
| `stage` | Required `in_place` or `suspended`; a conversion choice, not a timed stage schedule. |
| `vessel_types` | List of `name`, positive `length` in metres, and optional native `properties`; matches `GenericVesselType`. Supply draughts, RAOs, units and phase conventions in `properties` when required. |
| `vessels` | List of `name`, `vessel_type`, `connection` (`Free` or `Fixed`), global `initial_position` XYZ in metres, optional positive `length`, and `properties`; matches `GenericVessel`. |
| Vessel `properties` | Must explicitly supply `Orientation` (heading, trim, heel in degrees), `PrimaryMotion` and `SuperimposedMotion`. Native draught and motion data pass through unchanged. |
| `end_a`, `end_b` | For `suspended`, both are required. Each supplies `connection`, XYZ `position` in metres, and `azimuth`, `declination`, `gamma` in degrees. Connection is `Fixed` or the name of a supplied vessel. |
| `spline_control_points` | For `suspended`, required global XYZ starting-shape points. The converter retains its cubic spline order, so this interface requires at least four points. This is input validation, not a convergence criterion. |

Vessel references must resolve and names must be unique within each list.
The suspended stage requires at least one vessel-attached end. Either end may
remain fixed. Nonfinite attachment, spline and vessel-position coordinates
are rejected. Unknown installation fields are rejected to expose misspellings.

Vessel-attached end positions and end angles use **vessel axes**; fixed end
positions use **global axes**. The spline uses global coordinates. The
converter does not calculate a coordinate transform or check that the supplied
spline agrees with the end coordinates. The modeller supplies consistent
positions, rotations and connection angles. These conventions follow Orcina's
[Line data: End connections](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Linedata,Endconnections.htm)
and [Vessel data](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseldata.htm)
(online help, accessed 2026-10-20).

An absent `installation` block preserves the existing fixed-end, in-place
conversion. `stage: in_place` may include vessels but rejects end and spline
overrides; its jumper geometry remains the same. Existing domain examples have
no vessel motion inputs, so none are invented for them. Missing RAOs, crane
geometry and a rigging load path must be supplied before an operational lift
can be assessed.

Direct endpoint attachments are a kinematic representation. They do not add
crane wires, slings, clamps, constraints, load sharing, AHC or release events.
The inherited rigid end bending connections remain in use. An interior-clamp
lift requires an explicitly modelled rigging system beyond this conversion.

## Inherited conversion parameters and assumptions

The following records the current converter, rather than endorsing its values
for another design. Values without an independently available source are
**assumed, based on previous experience** and require confirmation before use.
They are not acceptance limits. No engineering Go/No-Go check is performed.

| Parameter | Current mapping | Source or assumption status |
| --- | --- | --- |
| A–G orientation | A up, B across, C down, D across, E up, F across, G down; connectors vertical. Each bend advances by the bend radius in both adjoining directions. | `walk_m_profile`; assumed orientation pending drawing confirmation in #480/#481. Applies to the default in-place starting shape. |
| Segment and bend lengths | Domain segment dimensions and pipe bend radius feed the existing jumper calculation; the resulting line sections are preserved. | Domain `jumper` and `pipe` blocks; `jumper_lift.compute_pipe_geometry` and `compute_orcaflex_sections`. The two configurations still share baseline geometry pending #480/#481. |
| Hub elevation | `_HUB_CLEARANCE_M = 5.0` m; default End A elevation is `-water_depth + clearance`. | Not present in the domain spec: assumed, based on previous experience. Suspended-stage coordinates and spline replace this starting geometry. |
| Smeared coating | A uniform coating combines insulation and module mass and spans the steel OD to module hydrodynamic OD. | `_smeared_coating`, mass conservation below. Replaces the baseline's arc-length `VariableData`; does not preserve local module shape or local response. |
| Drag | Normal `Cdn = 2.4` for coated pipe and `3.6` for module sections; axial `Cdz = 0.008`; connector normal drag `1.2`. | Repository-extracted baseline `generic.line_types` properties, linked below. Carried over, not independently calibrated. |
| Added mass and Poisson ratio | Normal `Can = 1.0`, axial `Caz = 0`, Poisson ratio `0.293`. | Same baseline line-type properties. |
| Material constants | Steel density `7.85` te/m³ and elastic modulus `212e6` kPa. | Same baseline homogeneous-pipe properties. These remain converter constants even when domain pipe density is overridden. |
| Connector | OD `1.8` m, ID `1.5` m, bending stiffness `100e3` kN·m², axial stiffness `1e6` kN, torsional stiffness `80.0` kN·m². Mass/length uses domain connector weight divided by length, converted from kg to te. | Shape and stiffness from the same baseline connector entry; mass from jumper inputs, not baseline connector mass. |
| Contents | Uniform contents density `0.0`, pressure `0`, flow `0`; axial contents inertia enabled. | Assumed empty/air-filled installation state with air mass neglected, not a flooding or trapped-gas assessment. |
| Discretisation and statics | Target segment length `0.5` m, spline order `3`, spline step followed by full statics, fixed bending connections, no torsion, VIV or seabed friction. | Existing converter choices: assumed, based on previous experience. No mesh or equilibrium proof is supplied. |
| Simulation | Time step `0.1` s, durations `[10.0, 60.0]` s. | Existing converter choices: assumed, based on previous experience. No time-step sensitivity proof is supplied. These durations do not schedule a lift or release. |
| Water | Domain depth/density; fallbacks `1400` m / `1.025` te/m³. | Existing defaults: assumed, based on previous experience when absent. |
| Seabed | Flat slope; domain normal/shear stiffness; normal fallback `100`, shear falls back to normal (kN/m/m²). | Existing simplified mapping: assumed, based on previous experience when absent. Domain soil type is not mapped. |
| Waves | Airy wave with domain significant height, period and direction; absent values become `0` m, `8` s and `0` degrees. | Existing mapping: assumed regular-wave representation of the supplied height, not a spectral sea or motion analysis. |
| Current and wind | Current direction `0`, no domain current profile mapped; wind design speed or `0`, direction `0`. | Existing simplified mapping: assumed, based on previous experience when absent. Gust factors are not mapped. |

The baseline property source is the repository's
[extracted line-type specification](../../../jumper/plet_to_plem/spec.yml)
(`generic.line_types`), not a licensed standard. The formula implementation is
in [the converter](../../../../../../src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py).
The section geometry and module properties come from
[jumper calculations](../../../../../../src/digitalmodel/marine_ops/installation/jumper_lift.py).

For steel outside diameter `Ds`, insulated diameter `Di`, and module
hydrodynamic diameter `Dh`, the coating calculation uses:

```text
Ai = pi * (Di² - Ds²) / 4
Ah = pi * (Dh² - Ds²) / 4
t = (Dh - Ds) / 2
rho_equivalent = (Ai * rho_insulation + module_mass_per_length) / Ah
```

This is the annular-area identity and equality of mass per unit length used in
`_smeared_coating`; all density and mass units must be consistent. The source
is the repository implementation above; no empirical coefficient is introduced.
The result preserves combined coating mass and hydro cross-section, rather
than the original spatial distribution. A starting spline is not an equilibrium
solution, as described by Orcina's
[Line statics: Spline](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Linestatics,Spline.htm)
(online help, accessed 2026-10-20).

## Worked example: inputs, method and answer

The [installation overlay](../../../../../../examples/workflows/jumper-vessel/installation.yml)
contains declared illustrative inputs, not measured vessel data. It sets a
stationary vessel origin at `(100, 200, 0)` m with zero rotation, and local end
positions `(-10, 0, -20)` and `(10, 0, -20)` m. The independent hand calculation
is vector addition: global endpoints are `(90, 200, -20)` and
`(110, 200, -20)` m. Those are the first and last supplied global spline points.
This demonstrates coordinate preservation only; the spline is a starting guess.

With the source package on `PYTHONPATH`, run the
[example command](../../../../../../examples/workflows/jumper-vessel/convert.py):

```bash
python examples/workflows/jumper-vessel/convert.py \
  DOMAIN_SPEC examples/workflows/jumper-vessel/installation.yml OUTPUT_DIR
```

`DOMAIN_SPEC` is either domain specification in the canonical directory above;
`OUTPUT_DIR` is a caller-selected output directory. The command merges the
overlay through a temporary file, validates `ProjectInputSpec`, and emits YAML.
The answer in `includes/20_generic_objects.yml` contains a vessel type and
vessel, and both line-end rows reference `support` with their original local
coordinates. The global spline is copied without translation or rescaling.
The example disables primary and superimposed motion explicitly. Vessel motion,
rigging capacity, statics convergence and licensed load/export are
**NOT_EVALUATED**. Supply sourced vessel and rigging data and complete #604
before using such a model for an operational decision.

The regression tests exercise both domain configurations with and without the
overlay, preserve nested supplied RAOs, check invalid inputs, and run the
example command. Expected coordinate values come from the hand calculation in
[test_jumper_vessel_conversion.py](../../../../../../tests/solvers/orcaflex/modular_generator/test_jumper_vessel_conversion.py).
