# Source and generated triangle validation audit

Source mesh quality and generated triangle resolution serve different purposes.
The hydrostatics adapter already separates them. The analytic cases below
characterize that separation and several additional clipping consumers; they
establish no general mesh validity or solver suitability. No production acceptance
rule changes. Refs #2342; context: #2239.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-19 | Record source and generated geometry contracts, governing scales and independent counterexamples to prevent reuse of a source quality floor after clipping. |

## Scope and evidence

Slice 1 covers the source `TriMesh` contract, the generated physical-face guard,
and signed artificial caps. Slice 2 covers additional triangle clippers and their
area/division assumptions. Slice 3 covers translated near-plane cases after
midpoint subdivision, station sections and clipped extents. The executable record
is `tests/naval_architecture/test_generated_geometry_audit.py`: 122 passing cases.
The tests characterize existing behavior; they do not introduce a shared tolerance
or transfer one consumer's acceptance criterion to another.

Repository policy references below describe the code at revision `6476218e`.
They are evidence of current implementation, not externally qualified engineering
limits. No published basis was established for their chosen coefficients.
Qualification of those coefficients is **NOT_EVALUATED**. No coefficient in this
audit is proposed as a new acceptance limit.

## Equations and sources

- [Polyhedral Mass Properties (Revisited)](https://www.geometrictools.com/Documentation/PolyhedralMassProperties.pdf),
  revised 2009, section 3, equations (7)–(11): with edges `u=b-a`, `v=c-a`,
  the vector area is `u×v/2`, unsigned area is `|u×v|/2`, and projected area
  is the corresponding component of the vector area. Integrating a constant
  over the reference triangle supplies the factor `1/2`.
- The same source, section 2, equations (2)–(5): flux of a field with unit
  divergence gives volume on a closed boundary. For a triangle, the linear
  coordinate mean is `(a_k+b_k+c_k)/3`, hence coordinate flux is that mean
  times the projected area. Multiplying unit normal by area cancels the normal's
  denominator; zero-area integration pieces need no normalized normal.
- [Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric Predicates](https://doi.org/10.1007/PL00009321),
  1997, public abstract: robust predicates require accuracy that depends on their
  inputs. A local area heuristic does not provide that adaptive guarantee;
  this is a limitation, not a coefficient supplied by the paper for the guard.
- Midpoint subdivision: replacing two edges by their halves multiplies their
  cross product by `1/4`, directly from equations (7)–(9) of the first source.
  Four children therefore conserve area in exact arithmetic. For an edge cut by
  a linear depth function, solving `d(a)+t*(d(b)-d(a))=0` gives
  `t=d(a)/(d(a)-d(b))`; a strict sign crossing makes the denominator nonzero.

Both public sources were opened successfully on 2026-10-19. They are mathematical
references, not physical validation evidence. Analytic dimensions and binary
exponents in the tests are manufactured inputs; each expected value is derived
in its test docstring, without obtaining the answer from the implementation.

## Audited contracts and falsifying cases

Paths in this table are relative to `src/digitalmodel/naval_architecture/`.
`D` is the source bounding-box diagonal, `A` is computed triangle area, and
`Lmax` is the longest generated triangle edge. All comparisons refer to the
current binary floating-point implementation.

| Consumer and repository policy source (2026) | Governing scale and rule | Division/normalization | Falsifying test in the executable record |
| --- | --- | --- | --- |
| `mesh_validation._validate_geometry`; `mesh_geometry._DEGENERATE_AREA_REL` | Source quality refusal: repeated indices or `A <= 1e-12*D²`. Global source scale, not a generated-face resolution criterion. | Area is computed by cross product, without normal normalization. | `test_source_floor_and_generated_guard_have_distinct_contracts`: identical thin tetrahedron refused as source and accepted by generated guard. |
| `mesh_clipping._check_physical_faces` | Physical generated-face refusal: repeated indices, nonfinite `A`, or `A <= eps_float*Lmax²`; `eps_float` is `np.finfo(float).eps`. Local edge scale, not `D`. | No division by physical area. The coefficient is the runtime floating-point spacing at unity, used as an existing heuristic. | `test_generated_guard_boundary_uses_local_edge_scale`: equality refused, resolved area accepted at three binary scales; `test_generated_guard_refuses_unusable_physical_faces`: repeated, collinear and nonfinite faces refused. |
| `mesh_clipping.clip_at_waterline`; `mesh_geometry._SNAP_REL`, `_CLOSURE_VOLUME_REL` | Plane distances within `1e-12*D` are snapped before clipping. Generated physical guard precedes signed cap construction. Closure requires reverse edges and volume-integral agreement within `1e-9` relative to the larger absolute volume. | Crossing denominator is a signed-distance difference. Artificial fans are signed integration pieces; collinearity is allowed. | `test_near_keel_trimmed_box_keeps_resolved_faces_below_source_floor`: both trim signs, zero trim and horizontal translation retain volume; `test_signed_cap_can_have_zero_area`: zero-area artificial fan accepted. |
| `mesh_sections._section` and `SectionIndex` | Reuses the caller's snap epsilon and the waterline clipper; there is no source area floor or physical-face guard on the temporary station triangles. Boundary fan supplies signed section area. | No division by triangle or section area in this consumer. | `test_station_near_end_retains_analytic_section`: translated box section close to end has analytic area. |
| `kcs_geometry._clipped_area_below` and `_clip_polygon_below` | Inside is `z <= waterline`; no positive area floor on generated pieces. Exact computed-zero pieces may remain. | Crossing height difference is nonzero by the crossing branch. Area and volume-flux consumers multiply by vector area; neither normalizes generated normals. | `test_clipping_area_survives_translation_and_subdivision`, consumers `surface_area` and `volume_clip`: tangency, near vertex and larger cuts conserve area. |
| `symmetry_cut._clip`, `_projected`, `_flux` | Signed-distance classification; no positive area floor. Zero-area fan pieces are expressly permitted. | Crossing denominator is nonzero by branch; projected area and coordinate flux use unnormalized cross products. | Same test, consumer `symmetry`: signed projected area and flux checked independently. |
| `appendage_submergence._area_submerged` | Arbitrary linear depth, kept when nonnegative; no positive area floor. | Depth difference is nonzero at crossing. Area uses original coordinates, not a shear to flatten the plane. | Same test, consumer `appendage`, plus `test_tilted_depth_uses_original_triangle_area`: analytic edge fractions under tilted depth. |
| `solid_occlusion._subdivide` | Arithmetic edge midpoints, four children; no source or generated area threshold in subdivision itself. | No normal or area division in subdivision. | Same test at zero, one and three levels checks clipped area conservation for all four consumers. Containment classification is outside this test. |
| `trim_hydrostatics.waterplane_extent` | Retained vertices and interpolated crossings; no triangle area threshold. | Crossing height denominator is nonzero by branch. No area division. | `test_near_vertex_extent_uses_intersections`: translated small cut gives clipped length, not full source span. |

The hydrostatic moment code does divide by total signed volume when finding a
centroid (`mesh_geometry._tetra_moments`). The main calculation refuses volume
and waterline extents using its existing global geometry contract
(`mesh_hydrostatics._hydro_integrals`); this does not constitute a triangle-area
floor. Auditing total-volume cancellation and denominator evaluation order is
**NOT_EVALUATED** here. Likewise, the statement about generated normals applies
to the listed integration paths, not mesh export or downstream solver adapters.

## Independent expectations

The local guard fixture has perpendicular legs `s` and `s*k*eps_float`. Its
area is `s²*k*eps_float/2`; longest edge squared rounds to `s²` for the tested
values. Thus `k=2` is the refusal boundary, `k=1` is refused, and `k=4` passes.
The thin tetrahedron with height `8*eps_float` separates this rule from the
existing source floor without needing clipping to manufacture the input.

For the box, length `8`, beam `4`, height `2`, mean draft `d=2^-20` and trim
`-d`, `0` or `d` give `V=4*integral(d+trim*x/8, x=-4..4)=32*d`.
The test also requires an actual generated physical face below the source floor;
volume agreement alone would not demonstrate that distinction.

The planar triangle `(o,3,0),(o+1,3,1),(o-1,3,1)` has area `1`. A cut at `h`
has base `2h` and height `h`, so retained area is `h²`, projected y area is
`-h²`, and y flux is `-3h²`. Tangency gives zero. Translation and midpoint
subdivision preserve these expected values. The tilted depth
`h-z-(x-o)/2` gives edge fractions `h/(3/2)` and `h/(1/2)`, so area is
`4*h²/3`. Box and tilted cases use a declared binary regression comparison
budget; it is not a physical quality acceptance limit or a general error bound.

## Inventory requiring later qualification

These paths were inspected, but are **NOT_EVALUATED** by the new falsifying tests.
The values below cite existing repository implementations (2026); no published
basis was established for their applicability. They must not be promoted to a
universal generated triangle guard.

| Consumer | Observed policy and scale | Required next evidence |
| --- | --- | --- |
| `hydrodynamics/hull_library/mesh_refiner._subdivide_once` | Quad midpoints and center; rounds coordinates to ten decimal places and retains children with at least three unique indices. `compute_quality_metrics` counts `area < 1e-6`; this is a diagnostic count, not an acceptance gate. `_compute_aspect_ratios` excludes edges no longer than `1e-12` before dividing longest by shortest retained edge. | Translated/scaled quad cases around coordinate quantization; distinguish retained index count, normal reliability and area diagnostics. |
| `hydrodynamics/bemrosetta/models/mesh_models.PanelMesh` | Normalizes a cross product only when its magnitude exceeds `1e-10`, otherwise supplies a fixed normal. This is an absolute cross-product scale, distinct from area acceptance. | Separate computed area, fallback normal and downstream solver requirements for small resolved panels. |
| `hydrodynamics/hull_library/mesh_cutouts._bottom_cut`, `_assemble`; `_cutout_geometry.triangulate_remnant` | Intersection-area selection uses `_TOL=1e-9`; assembly rounds coordinate keys to nine places. Remnant inclusion uses relative area `1e-12`, area recovery uses relative `1e-9`, and triangle aspect `Lmax²/(2A)` is bounded by `MAX_TRIANGLE_ASPECT=50`. Zero denominator produces infinite aspect. | Scaled/translated near-boundary remnants, units and separate length/area tolerance qualification. Aspect is an intentional quality check on new triangles, not necessarily an erroneous source-floor reuse. |
| `hydrodynamics/diffraction/geometry_quality.GeometryQualityChecker.check_watertightness` | Reports triangle area below absolute `1e-10` as degenerate; role differs from hydrostatic integration. | Establish intended source/generated contract and dimensions before proposing any change. |
| `naval_architecture/solid_occlusion._ray_x_hit` | Projected determinant magnitude below absolute `1e-18` returns no hit before barycentric division; `_EDGE_TOLERANCE=1e-9` marks near-edge hits. | Independent containment cases across units, translation and grazing rays; the subdivision-area tests do not qualify these predicates or their retry policy. |
| `naval_architecture/symmetry_cut.HalfBody.consistency`, `PlaneSection` | Total-volume diagnostics use an absolute denominator floor `1e-12` and relative consistency threshold `1e-6`. | Near-zero volumes and off-origin cut-plane accounting; triangle-area conservation is insufficient. |

The quad/cutout runtime paths were not loaded for this audit: their package import
chain reaches an optional licensed-solver integration. Static inspection is not
runtime validation. Other domains, mesh coarsening, export normalization, exact
pre-rounding collinearity, arbitrary self-intersections, nonfinite inputs to the
scalar clippers, and overflow/underflow extremes remain **NOT_EVALUATED**.

The way forward is a separate bounded qualification for each listed consumer,
with explicit units, governing scale, required caller inputs where a published
limit is unavailable, and falsifying cases before changing behavior. This audit
establishes no defect in those consumers and authorizes no acceptance-rule change.
