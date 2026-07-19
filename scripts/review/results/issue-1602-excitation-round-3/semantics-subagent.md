# Issue 1602 excitation amendment — semantics review — round 3

Reviewed commit: `994fbcdb3f1e7879e2cef0fe3828664e961604bb`

Verdict: **MAJOR**

- Shared payload commitments were not explicitly equal to the same-named enclosing response-envelope commitments, allowing a coherent payload triplet from another physical/run context.
- The approval prose overloaded `M` for excitation moment and rigid-body mass matrix.

Verified all prior pressure, nullability, coordinate, impedance, release-universe, and hash findings as resolved.

Disposition: patch inline; retain conservative draft state pending focused final verification.
