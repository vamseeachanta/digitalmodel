# Geometry validation evidence

- Separate topological validity, geometric fidelity, numerical convergence and
  physical validation. A valid CAD face or successful export proves none of the
  other three by itself.
- Preserve an explicit between-sample reference when changing parameterization;
  matching only the original offsets cannot establish surface equivalence.
- Match comparators to the represented surface and units. Same-estimator mesh
  comparisons are representation checks, not independent cross-solver validation.
- Check native per-metric statuses and valid-area coverage. Fractions normalized
  over a valid subset must not silently hide excluded geometry.
- Do not use closed-solid volume properties on an open wetted surface; state the
  integration or measurement-only closure method.
- Compare fitted curvature against the actual source interpolant separately from
  an analytic shape. Sparse interpolation error is not a fitting regression.
- Bound density/refinement experiments and preserve invalid outcomes. A diagnostic
  report or strict xfail is not evidence that a geometry defect is repaired.
- Preserve executable counterexamples alongside published geometry evidence;
  summary JSON alone is insufficient if the committed diagnostic cannot reproduce
  the claimed sampling method. Distinguish boundary from interior violations.

Evidence: [issue 2241](https://github.com/vamseeachanta/digitalmodel/issues/2241),
`scripts/review/results/2026-10-01-2241-brep-plan-main-r3.md`.
