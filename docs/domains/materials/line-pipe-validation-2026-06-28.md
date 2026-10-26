# Line-pipe input analysis

The bundled diameter, schedule and reference-weight tables have been removed.
A pipe now combines caller-supplied geometry and material data. No engineering
catalogue is shipped. Missing catalogue inputs raise an error.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-10-26 | Removed standards-derived dimensions and weight fixtures; documented geometry formulas and the required caller data. |

## Geometry basis

For outside diameter D and wall thickness t, the inside diameter is D - 2t,
metal area is pi * (D squared - ID squared) / 4, and bore area is pi * ID
squared / 4. Mass per length is metal area in square metres times the supplied
material density. Bore volume per length is the bore area in square metres.
These are geometric calculations; no dimensional table or assumed density is
used. Values must be finite, D and t positive, and t less than D/2.

## Input and test contract

See [caller-supplied data](caller-supplied-data.md) for the source schema and
removed-row inventory. Schedule aliases remain supported, but a schedule label
provides no wall thickness without a supplied row. Explicit OD and wall inputs
remain supported with a validated material record.

The tests use synthetic dimensions and material properties, check the formulas,
schedule normalization, explicit geometry, and invalid inputs. They establish
software behavior, not conformance with a product specification.
