# Plan: digitalmodel #2241 item 1 — end-closure treatment for parametric monohull forms

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/2241 (item 1; items 2 to 5 stay open)
**Parent:** #2191 Phase 1 (PR #2242), #2170 (decision D8)
**Date:** 2026-09-27 · **Status:** plan drafted by the orchestrator; dispatched to the Codex lane on the owner's instruction ("delegate to codex as much as possible; continue with recommended order"). `status:plan-approved` is the owner's label.
**Tier:** T2 (small) · **Lane:** lane:codex · **Client:** N/A
**Base:** branch `codex/2191-parametric-form` (PR #2242) until it merges; the orchestrator rebases onto main before opening the PR.

## Problem (verified from HullProd per-vertex fields, 2026-09-27)

Generated forms from `parametric_form.py` carry a pinched end closure: end stations keep a
0.5 % half-breadth floor through `y = sqrt((0.005 b)^2 + y^2)` (line 228), the cosine station
grid (`_grid`, line 192) places the first interior station at x = 0.15 m on a 100 m hull, and the
beta-CDF sectional-area ends give near-vertical area slopes at the extremities. On a generated
Wigley (L 100, B 20, T 8, 41 x 21, 7 225 panels) the mesh signature reads `I_D = 28.9` while the
BRep of the same profile reads 7.07 and a directly sampled analytic Wigley reads 4.2 to 4.3.
41.6 % of the `|K| L_ref^2` sum lies in x/L < 0.05 and 33.2 % in x/L > 0.95 (maximum density
16 000 to 22 000 against 41 in the midbody). The midbody is correct; the ends are not.

## Scope

1. **Closed-form end shape.** Add `entrance_angle_deg` and `run_angle_deg` to
   `MonohullFormParameters` (half-angles of the design waterline at the ends, 5 to 60 deg,
   default derived from the fullness exponents so existing defaults stay valid) and make the
   waterline half-breadth near each end tangent-continuous with a **finite** slope
   `tan(angle)` at the end point, half-breadth exactly 0 at x = 0 and x = L for pointed ends
   (transom ends keep their finite transom breadth). Remove the 0.5 % floor and the sqrt
   regularisation; replace by a proper closure. Sectional areas must still integrate to the
   Cb and LCB targets (re-solve; the beta ends may keep their role for the area curve, but the
   breadth distribution at each waterline must obey the end tangency).
2. **Station distribution.** Replace the raw cosine grid by a clustered grid whose first
   interior station is at least `L / (4 * n_stations)` from each end and whose spacing ratio
   between neighbours never exceeds 2.0; keep more stations near the ends than midships.
3. **Zero-breadth end stations through the consumers.** Verify `HullProfile` validators and
   `HullMeshGenerator` accept end stations with all half-breadths 0 (the generator already
   removes degenerate panels at zero-breadth tips). If a change in `mesh_generator.py` is
   strictly required, it must be minimal (degenerate-panel handling only), covered by a test
   with a zero-breadth fixture, and listed as a deviation in the PR body; no other change to
   that file.
4. **Wigley equivalence** (closed-form comparator): with `wigley=True`, every station
   half-breadth must equal `B/2 (1 - (2x/L)^2) (1 - (z/T)^2)` within 1 % of `B/2` at all
   stations and waterlines, including the end stations (0) and the entrance angle
   `atan(2B/L)`. This is the test that forces the end closure to be right.
5. **Signature acceptance** (hullprod required, skip cleanly without):
   - generated Wigley mesh `I_D` at 7 225 half-hull panels within 10 % of the directly
     sampled analytic Wigley mesh `I_D` at the same panel density (build the analytic sample
     in the test the way `test_curvature_screen._wigley` does, same L, B, T);
   - share of the `|K| L_ref^2` sum in x/L < 0.05 plus x/L > 0.95 below 20 % on the generated
     Wigley (use `screen_panel_mesh(..., keep_fields=True)`);
   - generated Wigley BRep (`screen_profile(..., representation="brep")`) status `valid` and
     `I_D` within 5 % of 4.31 (the closed-form value established in #2190);
   - report (no threshold) the BRep status and `I_D` for the rounded-Cb form (`cb=0.70`
     defaults) and the drillship-like transom form from the #2242 PR body; if they become
     `valid`, say so, and that closes #2241 item 2 as a side effect.
6. **Regression of Phase 1 guarantees:** the 3 x 3 Cb/LCB grid, the box, semicircle and
   consumer tests in `test_parametric_form.py` must still pass; update expectations only where
   the end closure legitimately changes a value, and state each change in the PR body.
7. **Docs:** add an "End closure" section to `docs/domains/hull_library/parametric-form.md`
   (what was wrong, the tangency rule, the new parameters, the acceptance numbers) and commit
   this plan verbatim as `docs/plans/2026-09-27-issue-2241-end-closure.md`.

## Out of scope

Semi-sub/spar generators, moonpools, optimisation loops (#2241 items 3 to 5); any change to
`curvature_screen.py`, `hull_surface_brep.py`, `quality_gates.py`; client geometry.

## Acceptance

- Items 4 and 5 pass; full `tests/hydrodynamics/hull_library` + `diffraction/test_quality_gates.py`
  green with the existing curvature and BRep suites unchanged.
- PR body: before/after table for the generated Wigley (mesh `I_D`, end-cap share, BRep status
  and `I_D`), the entrance-angle defaults chosen, any changed Phase 1 expectation with its reason,
  and the reported BRep statuses of the two extra forms.
- Module stays under 400 lines (split a helper module `parametric_form_ends.py` if needed),
  functions under 50 lines, black/ruff clean, no client, host or private-path identifiers.
