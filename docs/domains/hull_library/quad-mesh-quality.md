# Quadrilateral hull mesh quality checks

The shared `digitalmodel.hydrodynamics.quad_mesh_quality` module measures edge
ratio, projected corner angles, strict projected convexity and the minimum
signed projected bilinear mapping Jacobian. It reports each check separately.
All acceptance bounds are declared geometric inputs. Solver qualification remains
`NOT_EVALUATED`; neither a volume comparison nor an edge ratio establishes it.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-19 | Define reusable quad diagnostics and independent fixtures to expose angular skew, concavity and invalid maps. |
| 2 | 2026-10-20 | Preserve corner determinant signs at collinearity so normalization cannot qualify a degenerate map. |
| 3 | 2026-10-22 | Add per-check mesh summaries, complete JSON snapshots and an executable example so saved reports retain qualification gaps and affected panel indices. |

## Sources

- **S1:** [Coreform Cubit 2026.8, Quadrilateral Metrics](https://coreform.com/cubit_help/mesh_generation/mesh_quality_assessment/quadrilateral_metrics.htm).
  The minimum/maximum angle definitions and the Jacobian evaluated at corners
  and centre motivate the distinct metrics. Its recommended bounds are not
  adopted. This implementation is not an implementation of Cubit qualification.
- **S2:** [Introduction to Solid Mechanics, Quadrilateral Elements](https://engcourses-uofa.ca/books/introduction-to-solid-mechanics/finite-element-analysis/two-dimensional-solid-elements/quadrilateral-elements/),
  the shape-function expressions immediately before Figure 8. This institutional
  online text is undated; accessed 2026-10-19. Affinely rescaling its rectangular
  reference coordinates gives the unit-square functions below.

Both public pages were opened successfully. The following derivation defines
this module's geometry convention. It is not a solver acceptance specification.

## Mapping and equations

Let `p0,p1,p2,p3` be perimeter-ordered vertices, and `u,v` lie in `[0,1]`.
The shape functions derived from S2 are

```text
N0 = (1-u)(1-v), N1 = u(1-v), N2 = uv, N3 = (1-u)v
x(u,v) = sum(Ni*pi) = a + b*u + c*v + d*u*v
 a = p0, b = p1-p0, c = p3-p0, d = p0-p1+p2-p3
x_u = b + d*v, x_v = c + d*u
```

Use the unit centre normal `n`, parallel to
`(p2-p0) cross (p3-p1)`. This diagonal cross product equals twice
`x_u cross x_v` at the centre, by substitution. Reversing the winding reverses
`n`, so these checks do not certify outward orientation.

The signed projected Jacobian on this reference domain is

```text
J(u,v) = n dot (x_u cross x_v)
       = n dot (b cross c + u*(b cross d) + v*(d cross c))
```

The `uv` term vanishes because `d cross d = 0`. Thus `J` is affine and its
minimum over the closed square is the minimum at its corners. There is no
sampling claim for a general higher-order map. At corner `i` its value is
`n dot (e_previous cross e_i)`, where `e_i = p_next-p_i`.
A strictly positive corner minimum establishes a nonsingular, consistently signed
planar bilinear map and strict convexity of the projected perimeter polygon.
A zero corner value rejects a collinear corner; a singular centre rejects folded
or collapsed maps even when a usable reference normal cannot be constructed.
The strict zero boundary follows this declared geometric definition, not a
solver tolerance. `min_jacobian`, when supplied, is an additional inclusive bound.

`J` has coordinate units squared. Switching to a `[-1,1]` reference square
would divide its values by four, by the chain rule applied to S2. This metric is
neither a scaled Jacobian nor the minimum magnitude of a warped surface's
three-dimensional derivative cross product.

The required absolute `planarity_tolerance` is compared with
`max(abs(n dot (pi-p0)))`, in coordinate units. Within this tolerance the corner
angles use edges projected onto the plane through `p0` with normal `n`:

```text
alpha_i = degrees(atan2(|(-e_previous) cross e_i|, (-e_previous) dot e_i))
theta_i = alpha_i for a nonnegative turn; 360-alpha_i for a negative turn
edge_ratio = max(length(e_i)) / min(length(e_i))
```

The angle follows the included-angle definition in S1, with the reflex complement
selected by the signed turn. A full turn is 360 degrees by the definition of
degrees. Edge ratio uses the original spatial edges. Normalization by the longest
edge improves numerical scale handling; the Jacobian is restored with its square.

Floating-point normalization can alter the sign of a nearly cancelling corner
determinant. Each sign is therefore certified from the original binary64 input
coordinates using the scalar
`D_i = (e_previous cross e_i) dot ((p2-p0) cross (p3-p1))`.
Its sign matches the projected Jacobian because the centre-normal length is
positive. Coordinate differences, cross products and the dot product are enclosed
by interval arithmetic; every operation is rounded outward with `nextafter`.
An interval that cannot certify a sign, or disagreement with the normalized
sign, triggers exact rational evaluation of the original input values.
No geometric epsilon or additional acceptance bound is introduced.

Ordinary magnitudes still use normalized arithmetic. On the exact path, `D_i`
is divided by the fourth power of the normalization scale exactly, then by the
normalized centre-normal length. Restoring the square of the scale gives the
dimensional Jacobian. An exactly zero determinant remains zero for convexity,
mapping and aggregate status; a nonzero magnitude that cannot be represented
raises the numeric-domain error. This predicate qualifies the supplied binary64
coordinates; it cannot recover precision lost before input conversion.

## API and statuses

```python
from digitalmodel.hydrodynamics.quad_mesh_quality import assess_quad_mesh

report = assess_quad_mesh(nodes, panels, planarity_tolerance=declared_tolerance)
for panel in report.panels:
    print(panel.panel_index, panel.checks, panel.min_projected_jacobian)
```

Inputs are finite `(n,3)` coordinates and integer `(m,4)` connectivity, with four
distinct indices per panel. Repeated-index triangle encodings are rejected at the
API boundary; geometrically coincident vertices with distinct indices are
reported as degenerate quads. Mixed triangle/quad support is deferred.

`QuadQualityCriteria` accepts a nonempty `basis` and optional `max_edge_ratio`,
`min_angle_deg`, `max_angle_deg` and `min_jacobian`. These are caller inputs;
there are no undocumented default limits. Bounds are inclusive, in addition to
the strict positivity requirement for the mapping. `basis` should identify the
declared geometric criterion or an accessible solver specification and edition.
Naming a solver in this field does not establish solver qualification.

Each check records `PASS`, `FAIL` or `NOT_EVALUATED` and a reason. Any failure
controls the aggregate status. With no failures, any unevaluated check prevents
an aggregate pass. An empty mesh is unevaluated. Unknown diagnostics are `None`,
so reports can be serialized with `dataclasses.asdict` and strict JSON without
nonfinite sentinels. The aggregate `status` properties are computed at access;
`asdict` contains the individual check statuses, not the computed properties.

`report.check_summaries` groups panel indices for each geometric check into
`passed_panels`, `failed_panels` and `not_evaluated_panels`. Each summary has a
computed `status` with the same failure precedence. An empty mesh produces an
unevaluated summary for every check. These summaries preserve the difference
between a rejected map and a warped map whose surface validity is unknown.

Use `report.to_dict()` when saving a complete report. In addition to all fields
available through `asdict`, this detached snapshot includes the mesh `status`,
each panel's `status`, and `check_summaries` including their statuses. It retains
the separate `solver_qualification` result even when the geometric status is
`PASS`. `json.dumps(report.to_dict(), allow_nan=False)` saves a strict JSON
representation; unknown diagnostics remain `None` and become JSON `null`.
Diagnostic precision is retained; display rounding is separate from evaluation.

Panels outside the declared tolerance fail the planarity criterion. Their edge ratio, plane
distance and minimum projected Jacobian remain diagnostics; their corner-angle,
convexity and surface mapping checks remain `NOT_EVALUATED`. If the centre is
singular, convexity and mapping fail and the normal-dependent measurements are
undefined. Permitting a nonzero plane distance explicitly qualifies the projected
geometry within that tolerance; it does not qualify the actual surface map.
Coordinates exceeding the supported floating-point range, including a nonzero
Jacobian that underflows to zero, raise a numeric-domain error.

## Independent test basis

All fixture coordinates and bounds are synthetic declared geometry. They are
not values extracted from a solver manual. Expected values are derived in test
docstrings from the equations above, independently of the implementation.

| Fixture | Independent result or rejection reason |
| --- | --- |
| Rectangle, sides 2 and 1 | Edge ratio 2; right angles; `J=2` everywhere. |
| Trapezoid, `x=3u-uv,y=v` | `J=3-v`, corner minimum 2, distinct from centre value `5/2`. |
| Unit rhombus with edge vector `(3/5,4/5)` | Edge ratio 1; acute angle `acos(3/5)`; `J=4/5`. |
| Concave quad with interior vertex `(1/2,1/2)` | `J=4-3u-3v`, minimum -2; a reflex corner. |
| Bow tie | `J=2u-1`; singular centre and changing sign. |
| Fold with nonsingular centre | `J=2-3v`; centre `1/2` but corner minimum -1. |
| Four collinear or coincident vertices | All derivative cross products vanish. |
| Quad with three consecutive collinear vertices | `J=2-2u+2v`, minimum zero despite nonzero enclosed area. |
| Oblique corner with `p1=(1,3,0)`, `p2=(3,h,0)` | Corner determinant `h-9`: exact zero at `h=9`, with distinct positive and negative signs at adjacent binary64 values; cyclic starts, reversed winding and coordinate planes preserve the result. |
| Warped quad, `x=u,y=v,z=uv` | Centre normal `(-1,-1,2)/sqrt(6)`; projected minimum `2/sqrt(6)` and plane distance `1/sqrt(6)` do not qualify the surface. |
| Rotated, translated, reversed rectangle at scale 3 | Ratios and angles invariant; `J=18` from quadratic scale dependence. |

The tests also separate caller bounds, preserve panel indices in mixed-quality
meshes, check strict JSON serialization, and reject malformed coordinates,
connectivity and criteria. Numerical fixtures test quadratic scaling at
representable extremes and explicit rejection when the dimensional Jacobian
cannot be represented. Hydrostatic volume is not used as a quality oracle.

## Executable worked example

Run the example from the repository root with its numerical dependencies installed:

```bash
PYTHONPATH=src python examples/domains/hydrodynamics/quad_mesh_quality.py
```

The script emits a complete JSON report to standard output. `case_names` follows
the same order as `panels`, and each panel retains its zero-based `panel_index`.
No solver is needed. The following coordinates are synthetic inputs in a
consistent arbitrary length unit; connectivity follows each row's perimeter.

| Case | Ordered vertices `(x,y,z)` |
| --- | --- |
| rectangle | `(0,0,0), (2,0,0), (2,1,0), (0,1,0)` |
| skew_rhombus | `(0,0,0), (1,0,0), (1.6,0.8,0), (0.6,0.8,0)` |
| flat_triangle | `(0,0,0), (2,0,0), (4,0,0), (0,1,0)` |
| folded_quad | `(0,0,0), (2,0,0), (0,1,0), (1,1,0)` |
| warped_quad | `(0,0,0), (1,0,0), (1,1,1), (0,1,0)` |

The demonstration declares exact planarity, edge ratio at most 5, angles from
60 to 120 degrees inclusive and strictly positive mapping Jacobian. These are
synthetic caller inputs chosen to separate failure modes; no solver recommends
these limits. The zero boundary for the Jacobian follows the nonsingularity
criterion derived from S2 above. All five edge ratios pass the declared bound.

Substitution into the S2-derived map gives independent answers:

- Rectangle: `x=2u,y=v`, hence `J=2` and every angle is a right angle.
- Rhombus: `x=u+3v/5,y=4v/5`, hence `J=4/5`; unit edges have dot product
  `3/5`, so the angles alternate `acos(3/5)` and its supplement. Edge ratio
  passes while the angular bound fails.
- Flat triangle: `x=2u+2uv,y=v-uv`, hence `J=2-2u+2v`. Its minimum is zero
  at `(1,0)`, and the collinear corner has an angle of 180 degrees.
- Fold: `x=2u+v-3uv,y=v`, hence `J=2-3v`. The centre value is positive,
  but the corner minimum is negative. Two corner turns are negative. The
  edge lengths are `2,sqrt(5),1,sqrt(2)`. The unsigned angles are 45 degrees
  and `atan(1/2)`; the negative turns select their reflex complements.
- Warp: `x=u,y=v,z=uv`, hence the centre normal is `(-1,-1,2)/sqrt(6)`.
  The projected Jacobian is `(2+u+v)/sqrt(6)`, and the maximum vertex distance
  from the reference plane through the origin is `1/sqrt(6)`. Positive
  projection does not qualify the surface mapping.

Displayed results are rounded to one decimal place; assertions use the exact
expressions above with floating-point comparison where needed.

| Panel | Edge ratio | Angle range (degrees) | Minimum projected J | Geometry status |
| --- | ---: | --- | ---: | --- |
| rectangle | 2.0 | 90.0 to 90.0 | 2.0 | PASS |
| skew_rhombus | 1.0 | 53.1 to 126.9 | 0.8 | FAIL |
| flat_triangle | 4.1 | 14.0 to 180.0 | 0.0 | FAIL |
| folded_quad | 2.2 | 26.6 to 333.4 | -1.0 | FAIL |
| warped_quad | 1.4 | unevaluated | 0.8 | FAIL |

The mesh status is `FAIL`. Mapping failures are panels `(2,3)`; panel `(4)` has
an unevaluated mapping and fails exact planarity. Its plane distance is about
0.4 length units. The edge-ratio summary passes every panel; angular failures
are `(1,2,3)`. Solver qualification remains `NOT_EVALUATED`, including for the
passing rectangle. The way forward is to repair the degenerate and folded
connectivity, improve the skewed panel's angles, and subdivide or explicitly
qualify the warped surface before applying accessible solver criteria.

## Scope and later slices

Slice 1 supplies shared geometric diagnostics and independent rejection fixtures.
Slice 2 adds explicit bounds and mesh aggregation. Slice 3 supplies warped-panel
diagnostics and records the unresolved three-dimensional mapping criterion.
These slices add an opt-in API. Existing mesh generators, hydrostatic reports and
pipeline acceptance gates are unchanged.

Later work must establish solver-specific limits from accessible documentation,
qualify warped surface maps, support mixed triangle/quad encodings, and integrate
the shared report into generation and diffraction preflight. Existing shape
heuristics do not supply that missing qualification.
