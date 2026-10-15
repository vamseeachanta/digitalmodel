---
name: orcaflex-jumper-analysis
description: Prepare inspected jumper-library inputs with the existing format converter; native execution requires separate qualification.
---
# Jumper library preparation

Prepare solver-free inputs from the recorded specifications and retain validator failures and absent comparators. The specifications do not establish an M-shape, lazy-S or steep-S classification or a verified operational envelope.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-15 | Initial source-bound library preparation document and execution limits. |

Declared revision: 1.

## Specification groups

Values below are declared in each specification, not demonstrated response limits or ranges across every override. Units follow the repository environment schema. Unrecorded values remain unrecorded.

### manifold_to_plet

Installation rig with 11 line objects and 10 line types; the source includes crane wires, slings, connectors and a finite element jumper.

Source: `docs/domains/orcaflex/jumper/manifold_to_plet/spec.yml`; digest in `jumper-groups.json`.

Water depth 1996.0 m; wave height 2.0 m, period 8.0 s, direction 180.0 degrees. Current 0.0 m/s; wind 0.0 m/s. Simulation stages 10.0, 10.0, 1.0 s; time step not recorded s.

### plet_to_plem

Installation rig with 11 line objects and 10 line types; the source includes crane wires, slings, connectors and a finite element jumper.

Source: `docs/domains/orcaflex/jumper/plet_to_plem/spec.yml`; digest in `jumper-groups.json`.

Water depth 1996.0 m; wave height 2.0 m, period 8.0 s, direction 180.0 degrees. Current 0.0 m/s; wind 0.0 m/s. Simulation stages 10.0, 10.0, 1.0 s; time step not recorded s.

### sut_mm

Parameter override with no object geometry; a standalone jumper configuration is not established.

Source: `docs/domains/orcaflex/jumper/sut_mm/spec.yml`; digest in `jumper-groups.json`.

Water depth 100.0 m; wave Hs 2.2 m, period 9.0 s, direction 165.0 degrees. Current not recorded m/s; wind not recorded m/s. Simulation stages not recorded s; time step not recorded s.

### sut_mm/resonance

Vessel and buoy specification with 2 line types and no line objects; jumper geometry is not established by this specification.

Source: `docs/domains/orcaflex/jumper/sut_mm/resonance/spec.yml`; digest in `jumper-groups.json`.

Water depth 100.0 m; wave height 7.0 m, period 8.0 s, direction 180.0 degrees. Current 0.0 m/s; wind 0.0 m/s. Simulation stages 8.0, 16.0 s; time step 0.1 s.

## Worked preparation

Set `OUTPUT_DIR` to a writable directory outside the repository. This converts one named model without a solver:

```sh
PYTHONPATH=src uv run --no-sources python -m digitalmodel.solvers.orcaflex.format_converter single2modular docs/domains/orcaflex/jumper/manifold_to_plet/monolithic/DZ_AHCoff.yml -o "$OUTPUT_DIR/modular"
PYTHONPATH=src uv run --no-sources python -m digitalmodel.solvers.orcaflex.format_converter modular2single "$OUTPUT_DIR/modular/master.yml" -o "$OUTPUT_DIR/roundtrip.yml"
```

For parametric preparation, edit a copy of a group's `spec.yml`, retain its source digest, validate it against `ProjectInputSpec`, then use the same CLI's `spec2modular` command. The two specifications without jumper line geometry require that missing basis before a jumper model can be generated.

Use `validators.json` to identify structural failures and `comparators.json` for stored-result availability. A conversion exit code does not establish semantic fidelity: the existing semantic-diff taxonomy and converter remain the basis for the outstanding conversion campaign (#2014).

## Resonance and execution limits

The `sut_mm/resonance` group records a separate simulation specification. The group name alone does not establish a resonant frequency, dynamic amplification or response criterion. Those results require qualified native dynamics and read-back against an explicit comparator (#2014). No native result is claimed here. The native manifest template keeps load, statics and comparison refused until a qualified execution adapter and an approved manifest digest exist.

The document is retained here as a skill draft; installation at the requested skill location and a complete group-wide operating envelope remain open (#2015).
