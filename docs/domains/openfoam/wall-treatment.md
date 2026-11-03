# Smooth-wall treatment from y+ evidence

The arbitrary-hull free-surface and double-body builders accept
`achieved_y_plus`, `predicted_y_plus` and `nut_wall_function` on
`HullCaseConfig` (inherited by `DoubleBodyCaseConfig`). The selected boundary
condition is written to `0.orig/nut`, and `case_provenance.json` carries a
`wall_treatment` record with the selection, evidence statistics, sources and
range-check warnings.

Supply a `YPlusEvidence` object from
`digitalmodel.solvers.openfoam.wall_treatment`. Its `values` are the
wall-adjacent cell-centre y+ samples from every face sharing the treatment;
`source` identifies the mesh, condition, patch set and snapshot. If appendages
are present, include their faces: the builders copy the hull treatment to
appendages. Optional `face_areas` must match the samples in order and mesh.

The record reports minimum, maximum, mean and fraction below the log-law
lower bound, with weighting explicitly identified as `face_count` or
`face_area`. The weighted mean is `sum(w*y+)/sum(w)` and the fraction is
`sum(w for y+ below the bound)/sum(w)`, by definition. Without face areas,
the fraction describes faces, not wetted area. A log with only minimum,
maximum and mean cannot determine that fraction and must not be converted
into invented samples.

With `nut_wall_function="auto"`, `nutkWallFunction` is selected only if every
achieved sample lies strictly inside the typical log-law range
`30 < y+ < 500` [1, section 7.5]. Otherwise the selection is
`nutUSpaldingWallFunction`, whose continuous profile extends to the wall [2].
Requiring every face to satisfy the range is a conservative selection policy,
not an experimentally calibrated acceptance criterion.

An explicit supported function is retained. A conflicting achieved or
predicted distribution produces a configuration warning and a `FAIL` range
check in provenance. Missing evidence produces `NOT_EVALUATED`; predictions
and `y_plus_target` never stand in for achieved samples. A `PASS` only means
the range check did not reject the selected function.

These inputs can inform a rebuild from a prior solve. The builders do not
extract solver fields, verify the source label, establish patch coverage or
change a running case. New achieved evidence is needed after a rebuild.
Neither a Spalding selection nor a passing range check establishes
wall-resolved flow, qualifies a form factor, or verifies the other turbulence
boundary conditions. Per-patch extraction and attachment of wall-treatment
evidence to every derived coefficient remain later work.

## Sources

1. [Notes on Computational Fluid Dynamics: General Principles (2022),
   section 7.5, Wall functions](https://doc.cfd.direct/notes/cfd-general-principles/wall-functions),
   current online text accessed 2026-10-19. The stated range is typical,
   rather than a universal guarantee of accuracy.
2. [OpenFOAM v2312, nutUSpaldingWallFunction,
   Description](https://doc.openfoam.com/2312/tools/processing/boundary-conditions/rtm/derived/wall/nutUSpaldingWallFunction/).
