# Issue 1602 excitation amendment — semantics review — round 2

Reviewed commit: `e0bf030c4440490027af8ad00193551d285ba6d4`

Verdict: **MAJOR**

- Payload preimages did not structurally share every DOF/grid/basis/unit/convention/reference commitment or forbid interpolation in the coherence oracle.
- Pressure components omitted outward-normal orientation and signed force/moment integrals.
- Nullability used undeclared aliases instead of exact response-kind keys.

Verified: impedance sign, phase origin, forward-speed scope, heading transform, units, component availability, confidentiality, and indirect boundary invalidation.

Disposition: patch and re-review.
