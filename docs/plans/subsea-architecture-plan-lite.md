# Bounded planar manifold placement

## Plan-lite

Evaluate every location in a finite, caller-supplied candidate sequence.
Minimize the sum of all well-to-manifold lengths and exactly one
manifold-to-host length, using straight Euclidean legs in one supplied
Cartesian frame. Equal floating-point totals select the smallest identifier
in case-sensitive Unicode order; no tolerance is applied.

Return the selected candidate, coordinates, provenance, ordered well legs,
one host leg, total length, explicit units and an identifier-ordered audit
of every candidate and its objective. Reject ambiguous or nonfinite
coordinates, duplicate identifiers, missing provenance, mixed or unsupported
units, empty sequences and numeric overflow.

Acceptance uses the independent synthetic geometry in the
[runnable example](../../examples/field_development/subsea_architecture.py)
and the 55 [optimizer tests](../../tests/field_development/test_subsea_architecture.py).
The expected selection is centre, with two well legs of 5.0 m and one host
leg of 4.0 m, totaling 14.0 m. Tests also cover host-leg accounting, ties,
input ordering, units, provenance and invalid inputs. The 17 existing
[architecture-pattern tests](../../tests/field_development/test_architecture_patterns.py)
provide regression coverage for the adjacent statistics module.

Exclude continuous optimization, terrain routing, installation feasibility,
pressure and thermal design, equipment catalogues, multiple-manifold
allocation and engine integration. Unit conversion and reference-frame
qualification remain the caller's responsibility. The result is a planar
length objective; physical feasibility requires further design.
