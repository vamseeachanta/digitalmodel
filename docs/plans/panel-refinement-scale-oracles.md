# Panel refinement scale-oracle plan

Independent planar rectangle oracles characterize midpoint subdivision across
coordinate planes, windings, translations, binary scales and refinement levels.
Production behavior is unchanged.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-27 | Record the test scope, documentation, verification and exclusions. |

## Plan-lite

- **Proposed test scope:** retain the 92 independent scale-oracle cases in
  [test_refinement_scale_oracles.py](../../tests/hydrodynamics/hull_library/test_refinement_scale_oracles.py)
  and the 32 existing cases in
  [test_mesh_refiner.py](../../tests/hydrodynamics/hull_library/test_mesh_refiner.py).
  Derive child counts, areas, bounds, grid coverage and cyclic normals from
  rectangle geometry. Characterize the advisory area threshold, coordinate
  welding and small-panel normal fallback without changing their behavior.
- **Documentation changes:** explain coordinate resolution, ten-decimal-place
  welding and the advisory area count in the
  [refinement docstrings](../../src/digitalmodel/hydrodynamics/hull_library/mesh_refiner.py).
  Keep the numerical derivation in the oracle test module and record this plan.
- **Verification:** run both test files together with pytest in an isolated
  environment, keeping temporary output outside the repository; compare
  production syntax trees with the merge base after excluding docstrings.
  Run repository structure, engineering-register, identifier, absolute-path,
  whitespace and publication checks, and inspect hosted check failures.
- **Exclusions:** no production algorithm, tolerance, welding or normal-fallback
  changes; no solver execution, general mesh acceptance or self-intersection
  qualification; no unrelated generated-report, workflow or hook changes.

The tests characterize the resolved synthetic geometries and the disclosed
boundary cases. They establish no engineering acceptance limit.
