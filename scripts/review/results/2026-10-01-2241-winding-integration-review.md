# Issue 2241 item 6 — integration review

Issue: https://github.com/vamseeachanta/digitalmodel/issues/2241

Original reviewed range: `b0213e57..f3c68c79`. Review mode: parallel-readonly;
main session owns integration. The implementation propagates edge-orientation
parity by BFS and selects one area-weighted sign per connected component.

## Adversarial review

- Codex: APPROVE after verifying shared-edge parity, non-manifold and contradictory
  orientation rejection before mutation, per-component reference vertices,
  triangle encodings, stable quad diagonals, diagnostics and regression coverage.
- Claude: MINOR. Core algorithm verified by source inspection; numerical outputs
  were not rerun by that reviewer. Actionable gaps concerned half-hull outward
  assertions, a documented public-generator non-manifold case and comparator labels.
- Gemini: UNAVAILABLE in this session because CLI authentication is not configured;
  not counted as a passing review.

## Disposition

- A synthetic profile with consecutive zero-breadth stations produces overlapping
  mirrored centerplane panels. An explicit public-generator regression now asserts
  rejection. Invalid topology is not returned as a valid mesh.
- The Wigley half-hull regression now requires zero flips and strictly positive y
  normals, rather than accepting a globally reversed component.
- Guidance labels archived numerical results and same-estimator representation
  comparisons; it states the remaining approximate 7% mesh/BRep gap.
- `flipped_panels` counts net reversals, not triangle-encoding normalization.
- Existing strict box outward checks remain intact. General open-component outward
  sign selection remains a heuristic; zero-score components retain BFS orientation.
- Malformed all-empty private-helper inputs remain outside the generated-mesh
  contract, which removes panels with fewer than three unique vertices.
- Historical fold-count and end-share values remain archived-run evidence, not
  claims of new independently reproduced physical accuracy.

Codex focused review of the hardening diff: APPROVE. No numerical implementation
changed during this review; changes add assertions and clarify documentation.

## Verification

- Hardening stage: **42 mesh-generator tests passed**, including half-hull outward
  orientation and public rejection of overlapping mirrored panels.
- Ruff reports the same three pre-existing diagnostics in the existing test file
  as at its prior HEAD; no new diagnostic introduced. Existing style retained.
- `git diff --check` passes.
- Final rebased integration suite and legal scan: pending before publication.

Comparator classes: fold-edge/orientation checks are conservation checks;
historical signature deltas are archived-run representation comparisons. They do
not establish physical validation of generated hulls.
