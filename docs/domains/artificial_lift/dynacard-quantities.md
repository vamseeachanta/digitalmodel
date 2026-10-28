# Dynacard result quantities

Card calculations distinguish a computed zero from an unavailable quantity.
The remaining quantity-contract corrections for #1871 return null for a physical
perimeter, unsupported neutral depth, and power results when validation fails.
Shape comparison reports a signed, position-aware score.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-19 | Document result units, unavailable outputs and a synthetic example so callers can interpret the corrected quantities. |

## Inputs, method and answer

Run the worked example from the repository root with the package importable:

```sh
PYTHONPATH=src python examples/domains/artificial_lift/dynacard_quantities.py
```

All inputs are synthetic and chosen for hand calculation. The card has position
samples `[0, 0, 12, 12]` inches and load samples `[0, 100, 100, 0]` pounds, in time
order. Rod and pump dimensions in the example satisfy the context schema; they
are not needed to compute the geometric quantities.

For a polygon with successive vertices `(x_i, y_i)` and `(x_j, y_j)`, define
`q_i = x_i*y_j - x_j*y_i`. The signed area is `A = sum(q_i)/2`, and the centroid
coordinates are `Cx = sum((x_i+x_j)*q_i)/(6*A)` and
`Cy = sum((y_i+y_j)*q_i)/(6*A)`. These are the area and centroid equations in
[Calculating the area and centroid of a polygon (1988)](https://paulbourke.net/geometry/polygonmesh/).
The rectangle therefore encloses `12*100 = 1200.0 lb.in`, with centroid at
`(12/2, 100/2) = (6.0 in, 50.0 lb)`.

Axis travel is the sum of absolute changes in that axis around the closed loop:
`sum(abs(x_j-x_i)) = 24.0 in` and `sum(abs(y_j-y_i)) = 200.0 lb`.
The dimensionless perimeter is the polygon edge-length sum after each coordinate
is divided by its own range. For this rectangle it is
`2*(12/12 + 100/100) = 4.0`. This is a coordinate normalization convention,
not a physical length. A physical perimeter combining pounds and inches is
undefined; the compatibility field and convenience function both return null.

Shape comparison linearly interpolates the two stroke branches at common
normalized positions. Increasing position identifies upstroke; decreasing
position identifies downstroke. Vertical transfers have no position interval
and are excluded. The default computational grid retains the existing sample
count; it is not an engineering acceptance limit. Each concatenated load
profile is centered and normalized to unit Euclidean norm. The score is their
dot product, the signed cosine defined by
[NIST Dataplot, Cosine Similarity (2018 revision)](https://www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/cosdist.htm).
For reflected loads `100-load`, the centered vectors are `v` and `-v`:
`dot(v,-v)/(norm(v)*norm(-v)) = -1.0`. Matching profiles score `1.0`;
orthogonal profiles score `0.0`. These are mathematical outcomes, not diagnostic
acceptance limits. Positive load scaling and constant load offsets cancel, so
this score describes normalized shape rather than absolute load agreement.

A neutral depth needs axial load versus depth. Cycle extrema at one depth
cannot supply that profile, so the example returns null for neutral depth.
The same limitation holds for a measured downhole card.

## Result contract and compatibility

| Quantity | Behavior |
| --- | --- |
| Area and centroid | Polygon area and area-weighted centroid; centroid is null for zero enclosed area. Geometry area is in lb.in. |
| Zone areas | Enclosed sub-areas from quadrant clipping, not counts of samples. |
| Perimeter | Always null; use axis travel or dimensionless normalized perimeter. Non-null perimeter input is rejected by the result schema. |
| Compressive load | Maximum compressive effective-load magnitude; evaluated zero is distinct from null. |
| Depth and compression length | Null without a depth-resolved axial load profile. The legacy neutral-point convenience function also returns null. |
| Buckling thresholds | Require inclination and tubing clearance. Verdicts use those thresholds; missing inputs yield null. Each calculator call starts a fresh result. |
| Analysis method | Explicitly records the method actually used. |
| Shape similarity and deviation | Signed centered cosine and branch-wise RMS load difference; degenerate or ambiguous branches yield null. |
| Non-electric power | Existing behavior retains an explicit driver type and omits the electric cyclic-load factor, with a warning. |
| Power card area | Work per stroke in ft.lb; failed validation returns null work and power, with an error warning. |

The existing classifier still receives its original distance score and raw
mixed-axis edge feature through explicitly named compatibility fields. Those
fields are excluded from JSON/model serialization and are not engineering
results. Feature extraction from deserialized results without these fields
leaves the corresponding feature slots at zero; recompute the analysis before
classification. Retraining against corrected features remains separate work
under #1864. No classifier accuracy claim follows from these quantity tests.

Input samples must retain cycle order. Reversing time swaps stroke roles; without
phase metadata, reversed traversal and swapped stroke loads cannot always be
distinguished. This implementation does not infer that metadata. A cycle with
multiple position reversals cannot be represented by two single-valued stroke
profiles and returns null for comparison. Zero position or load range also
makes the shape score unavailable.

The geometry and buckling formula corrections already present in the base are
retained. This change introduces no new material property, empirical buckling
coefficient, motor factor or diagnostic acceptance threshold. The tests derive
new expected values independently in their docstrings. A physical depth model,
vertical buckling criterion and classifier retraining remain later slices.
