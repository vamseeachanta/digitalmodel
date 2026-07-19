# Issue 1602 excitation amendment — semantics review — round 1

Reviewed commit: `6e68f58cc09ba4b54d4851290b56d277bceec1a2`

Verdict: **MAJOR**

- The `Z·RAO = excitation` oracle did not define canonical impedance terms/signs/reference transforms under `Re(X exp(-iωt))`.
- Body-local CCW heading conflicted with the inherited ENU clockwise-from-true-north convention without an exact mapping.
- The elevation equation did not locate its spatial phase origin relative to the bound reference point.
- The unavailable-component disposition was not response-envelope committed.
- Froude-Krylov and diffraction component labels lacked solver-neutral pressure-integral definitions.

Verified: immutable commit and manifest hashes; excitation minimum binding, projection, release derivation, first-order scope, unit-wave basis, and indirect motion-boundary invalidation.

Disposition: draft remains blocked; patch every finding and re-review.
