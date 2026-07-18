# Issue 1619 plan review — Agy — round 1

Verdict: **MAJOR**

- The normalized response omitted wave-excitation force vectors.
- The generic response did not freeze a canonical time-harmonic convention.
- The Capytaine adapter was placed in the generic diffraction package rather than the solver-owned package.
- Byte-equivalence did not constrain floating serialization/execution fingerprint.
- The TDD dependency on the not-yet-landed #1618 type was not explicit.

Disposition: the plan remains draft. The revision adds a non-overloaded excitation commitment and parent-amendment gate, uses `Re(X exp(-i omega t))`, moves the adapter into the Capytaine package, defines exact float serialization, and requires the reviewed #1618 type/fixture before TDD begins.
