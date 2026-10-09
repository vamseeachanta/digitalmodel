# Claude BRep plan review

**Verdict: MAJOR**

You told me the main session reproduced both native statuses, so I accepted that and did not re-run them. I checked everything else by reading the source. I could not check the claim about commit `5d5c144d` because I only had file-read access, not git.

## Major findings

**M1. The regular parameterization is not defined well enough to build or test (§Implementation step 3, §TDD convergence row).**
- **The x-grid doesn't match the plan.** Today `profile_point_grid` puts columns on a uniform `np.linspace(0, L, n_x)` (`hull_surface_brep.py:33`). It never puts columns at the source stations, which are cosine-clustered (`parametric_form.py:173-176,279`).
- **The grid sizes conflict.** Step 3 says it will "resample each source section", but the convergence row requires 81 and 161 columns. Those columns can only come from stations made up between the source stations. The plan never says how.
- **The geometry between stations will change without being declared.** Today each fixed-z waterline is PCHIP-interpolated along x (`:51-54`). If samples are spaced by arc length, z at a given parameter varies from station to station. Interpolating along x then produces non-planar parameter lines, which is a different surface between stations.
- **An existing test conflicts with this.** `test_profile_grid_matches_mesh_interpolation` (`test_hull_surface_brep.py:114-123`) requires the grid to equal the mesh generator's fixed-z interpolation. The plan doesn't say whether that test is kept, split or changed.
- **Profiles with partial offsets will now break.** "Retaining exact source endpoints" only works if every station's offsets run from 0 to the draft. Today, a station that stops short is extended with its endpoint values (`:43-44`). Under the new rule the keel edge stops being planar, and `flat_bottom_face` raises at `:127-128`. That changes which profiles are accepted, which step 5 says needs owner approval, but the plan doesn't flag it.

**M2. The fidelity checks can't detect curvature errors (§TDD fidelity row).**
- **The position bound is too loose for curvature.** 1e-4 × max(L,B,T) = 10 mm. Ripples of that height at about station spacing (~2.5 m) create curvature far larger than the hull's real curvature. §Risks admits second derivatives may change, yet no test checks curvature.
- **"Between-station checks" have nothing to compare against.** The source data only exists at the stations.
- **Two exact references are available but unused:**
  - For these synthetic fixtures, `station_offsets(p, x)` and `midship_section` can serve as test-only references at any x. That doesn't conflict with the rule against inferring generator geometry from metadata, which is about production code.
  - The parallel midbody is exactly prismatic (`station_offsets` sets `y = base` when `ratio ≥ 1−1e-12`, `:201-202`). So K should be 0 there and the bilge curvature should be 1/r = 0.5 m⁻¹. The plan uses a separate extrusion control instead of checking these zones on the real fixtures.
- **Volume, Cb and LCB have no stated method.** The surface is open (no waterplane, open transom). Something like a divergence integral of z·n_z over the side and bottom would work, but it has to be specified. "Cb change" also doesn't say whether it is measured against the target or `profile.block_coefficient`. The 0.5% / 0.002L limits simply copy the generator's own tolerances (`:254-255`), so differences between calculation methods could use up the whole margin.

**M3. "Native valid" is not defined precisely (§TDD row 1, §Deliverable).**
- **Two different statuses count as valid.** `brep_validity.py:187-192` can return `valid_improper_integral_convergent` for I_D even when a singularity is detected. `_metric_status` (`curvature_screen.py:246-255`) passes that string through unchanged. The plan doesn't say whether it counts as a pass.
- **Class warnings are invisible.** `curvature_classes` can be `caution_singular_measure_zero` (`brep_validity.py:287-291`), but `_brep_signature` reads the a_C fractions without checking that (`curvature_screen.py:430-433`). So "finite seven-component signatures" can pass while the classes carry a singularity warning.
- **Needed:** an exact list of accepted strings for I_D and for the classes, plus a test that asserts the class status.

**M4. The convergence requirement is too weak (§TDD convergence row).**
- **It only compares the last two levels.** A difference of ≤5% between 81×41 and 161×81 doesn't show the trend is settling. Steady growth under refinement is exactly what HullProd treats as a singularity (`_persistent_growth`, `:40-43`).
- **Needed:** require the successive differences to shrink, and set an absolute floor for small fixture I_D values. Absolute tolerances are currently given only for "near-zero controls".
- **The patch escape hatch is undefined.** "Explicitly equivalent per-patch totals" has no definition, so the convergence ladder can be bypassed.
- **"Area" is not identified.** It could mean HullProd `surface_area`, OCC `BRepGProp`, or a dense-grid reference. No reference area is given for detecting "missing wetted strips".

**M5. The approval gates have gaps (§Implementation steps 5–6, §Authority).**
- **Changing defaults has no owner gate.** "Selected production settings" and the public default switch need only passing tests, not owner approval. Changing them alters every stored `curvature_signature_brep` (catalog test `:153-170`). They also invalidate the measured 33.7086% in the strict xfail (`:251-261`): it may XPASS, which fails the run, or need a new number. The plan says the xfail "will not be silently removed" but gives no rule for re-baselining it.
- **The fallback path leaves tests failing.** The RED tests are plain assertions (§TDD row 1). If the lane takes the "diagnostic report" route in step 5, they stay red. The plan should say they become `xfail(strict=True)` with the measured status, or the fallback breaks CI.
- **The extrusion control can be gamed.** Its "declared patch junctions" are chosen by the implementer. They should be declared before measuring, with a fixed maximum width.

**M6. The integration requirement contradicts itself (§TDD integration row, §Artifact map).**
- **The file it edits is already too big.** "Modules below 400 lines, functions below 50" can't hold: `curvature_screen.py`, which the plan edits, is already 586 lines.
- **A function is too long as well.** `screen_trimesh` runs about 77 lines (`:278-354`).
- **Needed:** either limit the rule to new or touched functions, or approve the module split as scope.

## Minor findings

- **m1. The "transom" label is misleading (§Baseline).** The transom diagnostic point is at x = 76.48 m, measured forward from the AP (`parametric_form.py:3`). That is forward of midship and far from the transom at x = 0. `transom_fraction` only affects the run (`u < run`, `:190`). Both hotspots are about 0.06–0.07 m above the keel, which fits the keel/bilge hypothesis rather than the label. The plan should name this and add a separate check of the real x = 0 transom edge.
- **m2. Patch joins can hide curvature jumps (§Step 4).** Requiring matching tangents (G1) still allows jumps in curvature. Bound the curvature jump at joins.
- **m3. Bottom sewing isn't covered.** The bottom polygon is straight segments joined to a fitted B-spline keel edge, sewn at 1e-4 m (`hull_surface_brep.py:95,136-142`). The gap depends on n_x, but the shared-edge coincidence check has no tolerance and doesn't mention the bottom seam.
- **m4. No runtime budget or slow marker** is set for 2 fixtures × 3 levels of native quadrature.
- **m5. Document format is inconsistent.** The new doc is `.html`, while the sibling domain docs it cites are `.md`.

**Needed before approval:**
1. Define the grid: where the x columns go and how points are interpolated between stations.
2. Name the fidelity references: generator-based references at stations and between them, plus K≈0 on the parallel midbody and bilge curvature 1/r.
3. List the exact accepted status strings, including the class status.
4. Require monotone convergence with absolute floors, and define the area and volume measures.
5. Put default switches and xfail/catalog re-baselining behind owner approval.
6. Resolve the 400-line rule against `curvature_screen.py`.
