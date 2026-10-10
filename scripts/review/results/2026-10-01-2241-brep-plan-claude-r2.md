# Claude BRep plan review

**Verdict: not approved.** I found 4 MAJOR and 7 MINOR defects. I checked the plan against `hull_surface_brep.py`, `parametric_form.py`, `curvature_screen.py`, the test module, and the installed `hullprod` (`brep_validity.py`, `validity.py`, `types.py`). I didn't run the fixtures, so the baseline statuses and hotspot coordinates are still unverified.

## MAJOR

**M1. Acceptance row "Extruded rounded-bilge control": the bilge-curvature control can't pass against the fixed reference.**
- The candidate is sampled only from the fixed-z/PCHIP reference built on 21 cosine-spaced waterlines (`parametric_form.py:173-183`). With T=8 and r=2, the nodes near the bilge are z ≈ 0, 0.049, 0.196, 0.436, 0.764, 1.17, 1.649, 2.184.
- **At 10% of the arc** (z ≈ 0.025 m), the true section has an infinite dy/dz at the keel tangency. PCHIP's finite end slope can't reproduce that curvature.
- **At about 89–90% of the arc** (z ≈ 1.65–2.0), the PCHIP interval [1.649, 2.184] spans the bilge-to-side tangency at z=2, where curvature drops from 1/r to 0. A single cubic can't match 1/r there.
- The 5% criterion therefore tests PCHIP interpolation error, not the fit. It will fail for any candidate, and its two possible outcomes are a forced stop or pressure to widen the threshold.
- The midbody check |K|L² ≤ 1e−4 means |K| ≤ 1e−8 m⁻². At a bilge curvature of 0.5 m⁻¹, that leaves only about 2e−8 m⁻¹ of longitudinal curvature. The fitter's own 1e−6 m tolerance (`hull_surface_brep.py:67,83`) at 2.5 m column spacing already allows about 1.6e−7 m⁻¹, so this check is not achievable either.

**M2. Acceptance row "Area/topology": the bottom-side seam limit of ≤1e−4 m conflicts with the frozen bottom construction.**
- `flat_bottom_face` joins the sampled keel points with straight chords (`hull_surface_brep.py:136-143`), while the side's keel edge is a smooth spline between those points.
- In the aft run the keel half-breadth (yc·ratio) changes by about 8 m over roughly 25 m, so chord sag is about y″h²/8. That is roughly 2e−2 m at 41 columns and still about 1e−3 m at 161.
- Sewing at 1e−4 (`hull_surface_brep.py:95`) will leave free edges. The bottom policy can't change ("Risks" section), and the plan doesn't say whether the gap is measured at the nodes (trivially passes) or along the edge (fails). Either definition makes this criterion defective.

**M3. Step 4: "smooth reference joins" is undefined, and every allowed patch boundary is non-smooth in the reference.**
- Patches must split at source stations. The reference is longitudinal PCHIP (`hull_surface_brep.py:52-54`), which is only C1 at exactly those stations, so the reference's curvature jumps at every allowed boundary (except inside the identical-section midbody).
- That makes the ≤5% principal-curvature-mismatch test either vacuous or in conflict with fidelity, depending on how the implementer labels each join.
- The plan needs a numerical rule (for example, agreement of the reference's one-sided second derivatives within a stated tolerance). Boundary selection "from derivative diagnostics" also has no stated rule.

**M4. First acceptance row: area excluded by the native classifier isn't guarded.**
- Native `curvature_classes` fractions are integrated over `curvature_valid_area` (`brep_validity.py:299-302`), and `developability_deviation` can be `valid` through the `k_stable` path even when cells are unconverged (`:196-201`).
- So "class-area sum within 1e−6 of one" is close to a tautology. A candidate could hide a degenerate strip and still show `valid`, which contradicts the aside's ban on excluding strips.
- Fix: require `curvature_valid_area_fraction` and the per-record `valid_area_fraction` to be ≥ 1−1e−6, and require `surface_area` status `valid`.

## MINOR

- **m1 (Step 6, convergence row):** `screen_step` hardcodes `ProducibilityConfig(brep_cache=False, brep_display_mesh=False)` (`curvature_screen.py:404`), and the plan freezes `curvature_screen.py`. The order-7 / depth-6 diagnostics therefore have to call `hullprod.assess` directly and reimplement the `lref×1000` scaling and the `_step_settings` wrapper. The plan should say so and require parity with `screen_step` at production settings.
- **m2 (Step 1):** `screen_profile(representation="brep")` has no `sampling` argument (`curvature_screen.py:489-546`). The RED tests therefore either target the default route, where they can never pass because defaults are frozen, or call `profile_to_step(sampling=...)`, where they fail with a `TypeError` rather than a meaningful status. The entry point and the expected RED failure should be stated.
- **m3 (convergence row):** "I" isn't defined. The plan should say whether the contraction test covers `I_D` only or all seven components, including the a_C fractions.
- **m4 (runtime bound):** The 32-assessment budget isn't allocated. One way to count: baseline 6 + production 12 + order/depth diagnostics at every level 24 = 42, before the fit-density sweep, the extrusion control and the metre/millimetre check. The plan should allocate the budget per step.
- **m5 (area row):** V and M are defined on a 501×201 grid in (x, z), which requires inverting the candidate surface. Near the keel the side surface is almost horizontal (∂z/∂v ≈ 0), so the inversion is ill-conditioned, and spline overshoot below z = −T would be clipped. Use the parametric form ∫∫ y (x_u z_v − x_v z_u) du dv, or state an inversion tolerance.
- **m6 (Step 4):** Boundary columns duplicated across patches would make `flat_bottom_face` reject the keel edge, because it requires strictly increasing x (`hull_surface_brep.py:129`). The plan should state that the bottom is built from the unique columns. A shared-edge gap of ≤1e−6 m between separately fitted surfaces also needs a shared boundary construction (common knot vector or constraints), and none is specified.
- **m7 (source table, Step 5):** The existing xfail is `test_fixture_ship_fine_mesh_delta_expectation`, a mesh-vs-BRep I_D delta at 7225 panels (test module lines 251-261). It isn't a "coarse-profile fit-sensitive" xfail, so the name should be cited exactly.

## APPROVE (checked against the code)

- **Status strings:** All exact strings exist in `VALIDITY_VOCABULARY`. The `developability_deviation` and `curvature_classes` records exist, and `I_D±` take `k_validity` when `I_D` is valid.
- **Native defaults:** order=5, tolerance=1e−4, max depth=5, base subdivisions=1 match `types.py:32-35`.
- **Fixtures:** The defaults (41 stations, 21 waterlines, bilge .20, cb .70) give a default BRep grid of 41×21, which matches level 1.
- **Existing code:** Fixed-z/PCHIP sampling, the 1e−4 sewing tolerance, the square-root bilge, the grid/mesh agreement test and the analytic controls all exist as described.
- **Scope:** `curvature_screen.py` is 585 lines, and `hull_surface_brep.py` (217 lines) has room under the 400-line limit.
