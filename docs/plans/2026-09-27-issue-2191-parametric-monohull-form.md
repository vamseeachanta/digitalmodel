# Plan: digitalmodel #2191 — parametric monohull form generator (Phase 1 of true hull parametrization)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/2191
**Parent:** #2170 (decision D8; decision page workspace-hub PR #3889). Relates #136 (WRK-043), #1700, #1705.
**Date:** 2026-09-27 · **Status:** plan drafted by the orchestrator; dispatched to the Codex lane on the owner's instruction of 2026-09-27 ("plan #2191 and dispatch to codex"). The `status:plan-approved` label is the owner's to apply.
**Tier:** T2 (bounded Phase 1 of an epic) · **Lane:** lane:codex · **Client:** N/A

## Context

`HullParametricSpace` only scales a catalog hull in L, B, T and D; it cannot change form. The
FreeCAD Ship Workbench was ruled out as a generator (desk check on #2191, 2026-09-25). The BRep
route (#2190, merged 2026-09-26 via #2227) showed that the HullProd signature is canonical only when
the surface behind it is smooth and densely defined: a 5-station profile gives a fit-sensitive BRep.
A generator that emits dense, faired stations from a handful of naval-architecture parameters
removes that limitation and gives the parametric sweeps a real form axis.

Phase 1 (this lane) is a **monohull** generator producing a `HullProfile`, so it plugs into every
existing consumer unchanged: `HullMeshGenerator`, `hull_surface_brep.profile_to_step`,
`curvature_screen.screen_profile`, `HullCatalog`, `HullHydrostatics`. Semi-sub and spar
(pontoon-column topology) and moonpools (a hole, which the station-offset representation cannot
express) are Phase 2 and are only recorded here.

## Scope

1. **`src/digitalmodel/hydrodynamics/hull_library/parametric_form.py`** (new, under 400 lines):
   - `MonohullFormParameters` (pydantic): `length_bp`, `beam`, `draft`, `depth`; targets
     `cb` (block coefficient, 0.35 to 0.95), `lcb_fraction` (LCB from midships as a fraction of
     `length_bp`, positive forward, default 0.0), `parallel_midbody_fraction` (0 to 0.8);
     midship section shape `bilge_radius_fraction` (of `beam/2`, 0 to 1), `deadrise_deg`
     (0 to 30), `flare_deg` at the waterline (negative is tumblehome, -15 to 30); end shapes
     `bow_fullness` and `stern_fullness` (power-law exponents, 1.0 to 4.0), `transom_fraction`
     (transom half-beam over midship half-beam, 0 gives a pointed stern, up to 0.95 for a
     drillship-like transom); discretisation `n_stations` (default 41) and `n_waterlines`
     (default 21). Validators reject impossible combinations (for example `cb` above the midship
     coefficient implied by the section shape).
   - `midship_section(params, z) -> half-breadth` closed form: flat bottom with deadrise,
     circular bilge of radius `bilge_radius_fraction * beam/2`, straight side with flare; its
     area gives `cm` analytically.
   - `sectional_area_curve(params, x) -> area`: three-segment curve (entrance power law with
     exponent `bow_fullness`, parallel midbody, run power law with exponent `stern_fullness` and
     transom end area `transom_fraction^2 * midship area`) whose integral equals
     `cb * length_bp * beam * draft`; the entrance/run split is solved so the centroid matches
     `lcb_fraction`. Use `scipy.optimize.brentq` on the one free split parameter; raise a clear
     `ValueError` when the targets are unreachable with the given fullness exponents.
   - `station_offsets(params, x) -> list[(z_keel_up, y)]`: the midship section scaled to the
     local sectional area with a shape blend toward the ends (a section exponent interpolated so
     bow sections become V-shaped and stern sections stay U-shaped), evaluated on the
     `n_waterlines` z grid; offsets are monotone in z, non-negative, and exactly `beam/2` at
     midships. Bow and stern end stations are given a small non-zero half-breadth (0.5 % of
     `beam/2`) so the existing profile validators and mesh generator degenerate-panel handling
     behave as with the fixtures.
   - `generate_profile(params, name="parametric_monohull") -> HullProfile` with
     `hull_type=HullType.SHIP`, `source="parametric_form"`, `block_coefficient` set to the
     achieved value, and `metadata`-free (the schema has no free metadata; put achieved
     coefficients in the returned `FormReport`).
   - `form_report(profile_or_params) -> FormReport` (dataclass): achieved `cb`, `cp`, `cm`,
     `cwp`, `lcb_fraction`, displaced volume from the generator's own Simpson integration and
     from `HullHydrostatics(profile).compute_displaced_volume()` (cross-check), plus the targets.
   - `sweep_forms(base: MonohullFormParameters, ranges: dict[str, ParametricRange], *,
     screen: bool = False, mesh_config=None) -> list[dict]`: reuse `ParametricRange` from
     `parametric_hull.py`; each row carries the parameter combination, the `FormReport`, and,
     when `screen=True` and hullprod is installed, the mesh `CurvatureSignature` from
     `screen_profile(profile, mesh_config)`.
2. **`parametric_hull.py`**: no change to existing classes; add one function
   `form_space_profiles(base, ranges) -> Iterator[(variation_id, HullProfile)]` that mirrors
   `HullParametricSpace.generate_profiles` for the new form parameters so downstream sweep code
   can consume either.
3. **`__init__.py`**: export `MonohullFormParameters`, `FormReport`, `generate_profile`,
   `form_report`, `sweep_forms`.
4. **Tests** `tests/hydrodynamics/hull_library/test_parametric_form.py` (hullprod-dependent tests
   skip cleanly; everything else must run without it):
   - closed-form comparators (per the reproducibility rule, comparator class stated in the
     docstring): box barge (`cb=1`, zero bilge, zero deadrise, zero flare, `bow_fullness` and
     `stern_fullness` at their box limit or a dedicated `box=True` path) achieves `cb`, `cp`,
     `cm`, `cwp` = 1.000 within 1e-6; a Wigley-like target (parabolic sections and waterlines)
     reproduces `cb = 4/9` within 1 %; the midship coefficient of a section with a full
     semicircular bilge (`bilge_radius_fraction=1`, zero deadrise, zero flare, `draft = beam/2`)
     equals `pi/4` within 1e-6.
   - target attainment: for a 3 x 3 grid of `cb` in {0.55, 0.70, 0.85} and `lcb_fraction` in
     {-0.02, 0, 0.02}, achieved `cb` within 0.5 % and `lcb_fraction` within 0.002 of targets;
     unreachable combinations raise `ValueError` with the parameter named.
   - consumers: the generated profile validates, `HullMeshGenerator` produces a mesh whose
     bounding box matches L, B/2, T within 1 %; `HullHydrostatics.compute_displaced_volume()`
     agrees with the generator's volume within 1 %; `profile_to_step` (from #2190) exports without
     error (skip without hullprod).
   - curvature regression (skip without hullprod): a `cb=0.85`, `parallel_midbody_fraction=0.5`
     form has `a_flat + a_single` larger than a `cb=0.55`, `parallel_midbody_fraction=0.1` form
     at equal panel density; increasing `bilge_radius_fraction` from 0.1 to 0.6 raises `a_single`;
     the fixture-style 5-station coarse profile is not used here, so the mesh-vs-BRep `I_D` delta
     of a `n_stations=41` generated form at 7 225 panels is reported in the PR body (no threshold
     asserted; the number is the deliverable).
   - `sweep_forms` on a 2 x 2 grid with `screen=False` returns four rows with reports in under
     five seconds.
5. **Docs:** `docs/domains/hull_library/parametric-form.md` (parameters, the SAC construction,
   the closed-form checks, a worked drillship-like example with `transom_fraction=0.9`,
   `cb=0.72`, and its curvature signature) and this plan committed verbatim as
   `docs/plans/2026-09-27-issue-2191-parametric-monohull-form.md`. Add a short pointer to
   `curvature-screening.md` under a "Parametric forms" heading.

## Out of scope (Phase 2, to be planned separately)

Semi-submersible and spar generators (column and pontoon primitives with the crease-dominated
caveat), moonpools and other cutouts (need a surface-with-holes representation or a mesh-level
boolean), bulbous bows, appendages, optimisation loops (HullProd or resistance as an objective),
line-plan or DXF export, any client geometry.

## Acceptance

- All closed-form and target-attainment tests pass without hullprod; hullprod-dependent tests
  pass with the extra (0 skips there).
- Full `tests/hydrodynamics/hull_library` + `diffraction/test_quality_gates.py` green; existing
  curvature and BRep suites unchanged.
- New module under 400 lines, functions under 50 lines, black/ruff clean; no client, host or
  private-path identifiers.
- PR body: a table of the 3 x 3 target-attainment grid (target vs achieved `cb`, `lcb`), the
  drillship-like example's signature, and the mesh-vs-BRep `I_D` delta of one generated form.

## Risks

- Reaching low `cb` with a full midship section requires very fine ends; validators must say so
  rather than produce negative half-breadths (clamp and report, never silently).
- Section blending toward V-shaped bows can create a kink at the parallel-midbody junction;
  blend the shape exponent smoothly (smoothstep over at least three stations) and confirm with
  the `a_saddle` trend in the curvature regression.
- Keep runtime small: closed-form sections and a single 1-D root solve per profile; no
  optimisation loops in Phase 1.
