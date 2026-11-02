# Diffraction mesh symmetry and buoyancy preflight

Both native-input backends check GDF header symmetry before writing a deck.
The declaration describes the stored mesh sector. A physical body can be
symmetric while its full mesh still requires `symmetry: none`.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-21 | Define matching stored-sector declarations and explicit buoyancy screening to prevent incomplete-body conversion. |

## Inputs and method

[WAMIT User Manual v7.4, section 6.1](https://www.wamit.com/manualv7.4/wamit_v74manualch6.html)
defines the binary symmetry indices and the stored half or quarter surface:

| Stored panels | ISX | ISY | Spec symmetry |
| --- | --- | --- | --- |
| Full body | 0 | 0 | `none` |
| One side of y=0 | 0 | 1 | `xz` |
| One side of x=0 | 1 | 0 | `yz` |
| One quadrant | 1 | 1 | `xz+yz` |

A mismatch names both flags and the declared symmetry. Malformed flags refuse.
A declared cut with coordinates on both sides also refuses; cut coordinates
must be exactly on the plane or on one side. Input files remain unchanged.

[OrcaWave help, Bodies: Body mesh](https://www.orcina.com/webhelp/OrcaWave/Content/html/Data,Bodies.htm#BodyMesh)
requires an explicit symmetry setting even when the format contains flags.
The backend emits the matching `BodyMeshSymmetry`. AQWA reflects sectors with
outward winding, then emits an xz half with `SYMX`, or a full body for the other
cuts. Full-body AQWA node and panel ordering is retained.

The optional `vessel.geometry.buoyancy_relative_tolerance` enables the DF2
free-flotation screen. Supply a traced criterion appropriate to the body;
there is no implicit acceptance limit. Without it, the backend's
`preflight_reports` explicitly records `NOT_EVALUATED` with the missing input.
Generation alone therefore does not establish buoyancy qualification.

The screen reuses `mesh_orientation.orientation_report` and
`equilibrium_checks.check_mass_displacement`. The former documents the
divergence-theorem identity `V = integral(z n_z dA)` for an outward wetted
surface closing at z=0. The source is
[WAMIT v7.4, section 3.1: Hydrostatic data](https://www.wamit.com/manualv7.4/wamit_v74manualch3.html),
with its fluid-normal sign reversed for outward panel vector areas.
Reflected sectors are welded before the topology and
orientation check. Archimedes' displaced mass is `rho * V`; gravity cancels
in the mass/weight comparison. The inclusive residual criterion is
`abs(mass - rho * V) <= tolerance * rho * V`.

With a supplied criterion, unreliable geometry or failed mass balance refuses
before any native file is written. This check supports upright, metre-unit
wetted GDF geometry at z=0. It does not clip dry panels, infer units, resolve
external loads, establish stability, or qualify a native solver result.

## Worked example

Run with an empty caller-selected output directory:

```sh
PYTHONPATH=src python examples/hydrodynamics/diffraction_symmetry.py OUTPUT
```

The example writes a GDF positive-y half, its specification and both native
input formats. All physical and sampling inputs are manufactured controls.
The box length is 4.0 m, full beam 2.0 m and draft 1.0 m, so the stored half
volume is 4.0 cubic metres and reconstructed volume is 8.0 cubic metres.
At the supplied density 1000.0 kg per cubic metre, displaced mass is 8000.0 kg.
The supplied mass is 8000.0 kg: the residual is 0.0 kg and both screens pass.
Zero tolerance requests exact equality for this manufactured case; it is
not a recommended engineering tolerance.

Changing only `symmetry` to `none` refuses with header `ISX=0, ISY=1` and spec
symmetry `none`. Changing only mass to 4000.0 kg refuses DF2: the discrepancy
is 4000.0 kg, or 50.0 % of displaced mass. The source mesh is retained.

## Migration and limits

Previously a full mesh could declare geometric `xz` symmetry and AQWA would
extract its half. Such a mesh must now declare `none`, or be explicitly cut
and given matching header flags. Matching partial GDFs are accepted instead
of being rejected indiscriminately by OrcaWave.

PR #3066 remains open and its inspected changes contain no reusable DF2
implementation. These checks reuse the existing solver-neutral geometry and
equilibrium routines instead. Native execution, reverse parsing of partial
OrcaWave meshes, other mesh formats and external-load equilibrium remain
outside this screen. No licensed solver execution is part of the example.
