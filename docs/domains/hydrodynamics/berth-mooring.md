# Berth mooring with fenders and passing loads

## Summary

Prepare a generic berth mooring analysis with explicit wind/current inputs,
passing-load histories, tension-only mooring links and compression-only fenders.
The capability produces a twelve-case review package and a standard worked
report without invoking a licensed solver. Coupled response is `NOT_EVALUATED`.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 00 | 2026-10-20 | Initial preparation capability and worked example to expose the method and pending response. |

## Capability

- Validate explicit geometry, input bases, passage clearance and fender tables.
- Superpose steady wind/current resultants with the existing finite-depth
  passing-force calculation in `hydrodynamics/wang_passing_ship.py`.
- Generate native review YAML, SI load histories, a case table and input/artifact
  digests. Three assumed passages cross calm, wind, current and combined loading.
- Check exported line tensile/fender compression magnitudes against sourced
  allowables, retaining the input digest and one solver version. Missing criteria
  or results remain `NOT_EVALUATED`.
- Build the repository's standard HTML report with inputs, equations, sampled
  applied-load peaks, revision history and a way forward.

## Worked example

[Inputs, method, source records and reproduction command](../../../examples/domains/mooring/berth_mooring/README.md)
accompany the [worked report](../../../examples/domains/mooring/berth_mooring/report.html) and
[case table](../../../examples/domains/mooring/berth_mooring/case-table.csv).
All physical parameters are **assumed, based on previous experience**; none comes
from the provenance-unknown mooring/fender group. The example gives **160.0 kN**
steady combined sway and **1600.0 kN m** yaw. These are applied loads, not line
or fender responses. The source records identify the public NASA drag relation,
Orcina link laws and the repository's existing 1975 passing-force method.

## Qualification boundary and way forward

Structural YAML validation passes; solver load attestation remains pending.
Review models need applied-load wiring, qualified hydrodynamic inertia/damping
and initial equilibrium before an approved solve. Supply sourced tension,
fender reaction, compression and displacement criteria and retain results on one
recorded solver version. Establish sampling, time-step and image-sum convergence
before treating sampled peaks as governing. Bank/channel effects, passage-current
interaction, friction, line sag and hull-motion feedback are excluded.
