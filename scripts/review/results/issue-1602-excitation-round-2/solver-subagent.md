# Issue 1602 excitation amendment — solver-neutrality review — round 2

Reviewed commit: `e0bf030c4440490027af8ad00193551d285ba6d4`

Verdict: **MAJOR**

- Release membership contradicted the derivation for approved-but-unreleased responses.
- Component definitions omitted surface-normal orientation and signed generalized force/moment equations.

Verified: hashes, validator RED state, heading conversion, impedance convention/units, and downstream #1619 executability after prerequisites.

Disposition: patch and re-review.
