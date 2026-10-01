# Issue 2253 integration review

Issue: https://github.com/vamseeachanta/digitalmodel/issues/2253

Reviewed implementation: `6018d1bfbbbd289d37c28e690ff568e13a7a96c7`, based on
`81e0fd893fad002fef0a247d1e3f3fae6cec6f7f`. Range-diff confirms the two rebased
implementation commits preserve their original patches. Integration mode:
parallel-readonly review and validation; main session owns Git mutations.

## Provider results

- Codex: APPROVE after source/test inspection of split selection, grid parity,
  winding preservation, symmetry expansion, signature forwarding, legacy loading,
  and compatibility with the merged DXF reader. Numerical checks were assigned
  to the main session, not claimed by this reviewer.
- Claude: MINOR, no blocking split-logic defect. Verified the same core paths,
  fixed-policy reproduction, inventory table/catalog consistency, and spike
  conversion tests. Read-only review did not reproduce numerical values.
- Gemini: UNAVAILABLE; CLI exited 41 because authentication was not configured.
  This is not a passing review. Review continued with two available providers.

## Finding disposition

1. Tight tie tolerance permits rounded coordinates to choose diagonals on an
   analytically equal-diagonal surface. Documented; tolerance remains unchanged.
2. Periodic/incomplete grids fall back to panel-index parity and may form stripes.
   Guidance now distinguishes panel vertex-order rotations from periodic grids,
   states the fallback and its provenance limitation, and corrects the docstring.
3. Large inflation requires inconsistent winding, not just a fixed diagonal.
   Dated clarifications now accompany both historical correction surfaces.
   The original plan remains verbatim; its fixed-above-25 premise is superseded
   by the controlled comparisons in `docs/domains/hull_library/curvature-screening.md`.
4. The parametric xfail reason now names generator winding and issue 2241 item 6.
5. The semi-sub regression qualifies the cylinder primitive only, not whole-form
   developability. This matches the planned primitive-screening scope.
6. The broken-winding regression intentionally reproduces the current generator
   defect; the subsequent winding branch replaces that assertion.
7. The unused spike `mirror()` helper remains for now; no correctness impact.

No numerical implementation changed during this integration review; the follow-up
edits only clarify documentation, a docstring and an xfail explanation.

## Orchestrator verification

- Python 3.11: hull-library suite plus diffraction quality gates, with random
  ordering disabled: **642 passed, 42 skipped, 4 xfailed** in 83.34 seconds.
  This includes the newly merged DXF reader tests.
- Black 24.10.0 and Ruff pass on affected Python files; `git diff --check` passes.
- Source search found no callers of `_triangles_from_panels` outside its owning
  adapter. Public import paths and the DXF CLI were exercised by the suite.
- Legal scanner: verified exact-checkout `--all` route, with no initialized hub
  submodules and the exact checkout registered: PASS, zero blocking violations,
  11 advisory matches. Named-repository route was rejected as evidence because
  it supplied an empty scan target despite exit zero; existing follow-up:
  https://github.com/vamseeachanta/workspace-hub/issues/3804.

Comparator classes: analytic sphere/cylinder and Wigley-integral checks are
closed-form; repeated inventory and triangulation tables are archived-run
comparisons. Alternate triangle meshes use the same estimator, so they are
representation comparisons, not independent cross-solver validation.

Known limitations: the generator winding correction is a subsequent branch;
the four strict xfails remain visible; no private geometry was processed.
