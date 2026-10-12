# Plan: curvature_screen quad triangulation fix and signature re-baseline

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/2253 (parent #2170; found by #2241 item 1)
**Date:** 2026-09-27 · **Status:** plan drafted by the orchestrator; dispatched to the Codex lane on the owner's instruction ("delegate to codex as much as possible; continue with recommended order"). `status:plan-approved` is the owner's label.
**Tier:** T2 (small code change, wide re-baseline) · **Lane:** lane:codex · **Client:** N/A
**Base:** branch `codex/2241-end-closure` (stacked on `codex/2191-parametric-form`, PR #2242); the orchestrator rebases onto main before opening the PR.

## Problem

`hull_library/curvature_screen.py::_triangles_from_panels` splits every quad on the same
diagonal (`[0,1,2]`, `[0,2,3]`). On exact analytic Wigley vertices (L 100, B 20, T 8,
425 x 17 quads) the mesh signature reads `I_D = 38.3` with that split, `6.60` with alternating
diagonals (the pattern HullProd's own Wigley control uses) and `7.13` on the BRep of the same
surface (closed-form integral 7.1197). The fixed split folds each quad the same way, which the
discrete curvature estimator reads as real curvature. Every quad-panel input screened so far
(GDF/DAT: the inventory table from #2223, the diffraction quality gate, the 2026-09-25
evaluation note whose spike converter used the same split) carries this bias. Triangle inputs
are unaffected.

## Scope

1. **Adapter.** `panel_mesh_to_trimesh(mesh, *, expand_symmetry=True, quad_split="shortest")`
   with `quad_split in {"shortest", "alternate", "fixed"}`: `shortest` splits each quad on its
   shorter 3-D diagonal (ties: `alternate` rule), `alternate` uses the (i+j) parity pattern
   when the panel index carries structure and otherwise alternates by panel index, `fixed` is
   the old behaviour for reproduction. Thread the option through `screen_panel_mesh` and
   `screen_profile`; record `quad_split` in `provenance` and add it as a field
   `CurvatureSignature.quad_split: str | None = None` (None for triangle inputs and BRep).
2. **Regression tests** in `test_curvature_screen.py` (hullprod required, skip cleanly):
   build the analytic Wigley as a structured quad `PanelMesh` (same L, B, T and density as the
   `_wigley` triangle helper); `shortest` and `alternate` must each be within 10 % of the
   triangle helper's `I_D` and within 15 % of the BRep value 7.1197 (compute the triangle
   reference in the test); `fixed` must reproduce the inflated value (above 25) so the bug
   stays documented; sphere and cylinder controls unchanged; the OC4-style semi-sub fixture
   (box + cylinder primitives, build synthetically) keeps `a_single = 1` on the cylinder part.
3. **Un-xfail** the two mesh acceptance tests in `test_parametric_form.py` if they pass with
   the default split; if one still fails, keep it as xfail with the new measured number in the
   reason and explain in the PR body.
4. **Re-baseline the inventory.** Run `scripts/hull_library/screen_inventory.py` to regenerate
   `docs/domains/hull_library/curvature-signature-catalog.yaml` and
   `curvature-signature-table.md`; add a dated correction paragraph at the top of the table with
   a before/after mini-table for three repo-local hulls (L01 vessel, L02 OC4 semi-sub, L03
   outer column) and the reason. Update `test_screen_inventory.py` only if its expectations
   change.
5. **Correct the published numbers in place** (mechanism-before-publication rule: name the
   correction, do not silently amend):
   - `docs/domains/hull_library/hullprod-curvature-screening-evaluation.md`: add a
     "Correction 2026-09-27" section after the summary table stating that all GDF-derived rows
     used the fixed diagonal split, that repo-local hulls are re-baselined in the inventory
     table, and that client-hull rows are superseded and will be recomputed outside the public
     repo; do not change the original table values.
   - `docs/spikes/2026-09-25-hullprod-curvature-screening/gdf_to_stl.py`: switch to the
     shortest-diagonal split and say so in its docstring; `README.md`: add the same correction
     note; leave `results/signatures.csv` as the historical record with a "superseded" line in
     the README.
   - `docs/domains/hull_library/curvature-screening.md`: document `quad_split` and the finding.
6. **Docs:** commit this plan verbatim as
   `docs/plans/2026-09-27-issue-2253-quad-triangulation.md` (replace `2253` with the issue
   number given in the prompt header).

## Out of scope

Recomputing client-hull signatures (private data, orchestrator side); any change to the
Rusinkiewicz estimator inside HullProd; the BRep route; #2241 items 2 to 5.

## Acceptance

- Item 2 passes; full `tests/hydrodynamics/hull_library` + `diffraction/test_quality_gates.py`
  green; BRep suite unchanged.
- PR body: the analytic-Wigley table (fixed / alternate / shortest / triangle reference /
  BRep), the three-hull before/after inventory rows, the un-xfail outcome, and any changed
  expectation with its reason. No client, host or private-path identifiers.
