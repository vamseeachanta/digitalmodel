# OC4 semi-submersible diffraction mesh

An offline GDF generator constructs the wetted external geometry from the public
OC4 definition [P]. The exported panels pass the numerical criteria [M]. No
licensed solver is used, and solver agreement or frequency convergence is not
established. The requested system centre of mass remains NOT_EVALUATED; the
published platform centre of mass is a distinct quantity.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| A | 2026-10-20 | Public column and member geometry, independent mesh checks and dimension-based tests are added to provide a reproducible diffraction input. |

## Public basis

[P]: https://www.nlr.gov/docs/fy14osti/60601.pdf
[F]: https://www.wamit.com/manual7.x/html/wamit_v75manualch6.html
[M]: https://github.com/vamseeachanta/digitalmodel/issues/3086
[G]: https://gmsh.info/doc/texinfo/

[P] is *Definition of the Semisubmersible Floating System for Phase II of OC4*,
NREL/TP-5000-60601, September 2014. Printed page numbers are used below.
The source PDF was read directly; its SHA-256 is
`ccb0f7cdc001cfc41f3a32abc858c5ab0fca2564152b0ed81a0f56781913a1a0`.
The PDF and generated meshes are not bundled. Source catalog identity:
`nrel-iea-reference-turbines`. This example owns the derived geometry and code;
it contains no vendor mesh or solver output.

The origin is at the main-column centreline at still water level (SWL), with
positive z upward [P, section 3.3]. All dimensions below are external, at the
undisplaced position. Wall thickness and ballast do not change this wetted mesh.

| Quantity | Value | Citation |
| --- | --- | --- |
| Draft (m) | 20 | [P], Table 3-1, p. 7 |
| Main column diameter (m) | 6.5 | [P], Table 3-1, p. 7 |
| Main column top z (m) | 10 | [P], Table 3-1, p. 7 |
| Offset upper column diameter (m) | 12 | [P], Table 3-1, p. 7 |
| Offset upper column length (m) | 26 | [P], Table 3-1, p. 7 |
| Offset upper column top z (m) | 12 | [P], Table 3-1, p. 7 |
| Offset column centre spacing (m) | 50 | [P], Table 3-1, p. 7 |
| Base column diameter (m) | 24 | [P], Table 3-1, p. 7 |
| Base column height (m) | 6 | [P], Table 3-1, p. 7 |
| Base column top z (m) | −14 | [P], Table 3-1, p. 7 |
| Pontoon and brace diameter (m) | 1.6 | [P], Table 3-1, p. 7 |
| Main column centre (x, y), m | (0, 0) | [P], Table 3-2, p. 7 |
| Offset column centres (x, y), m | (14.43, 25), (−28.87, 0), (14.43, −25) | [P], Table 3-2, p. 7 |
| Displaced volume (m³) | 13,917 | [P], Table 4-6, p. 25 |
| Centre of buoyancy z (m) | −13.15 | [P], Table 4-6, p. 25 |
| Platform centre of mass z (m) | −13.46 | [P], Table 3-3, p. 9; excludes tower, rotor-nacelle assembly and moorings |
| Requested system centre of mass z (m) | −9.89, NOT_EVALUATED | [M]; no supporting value is located in [P]; a public mass-property source is required |

Table A. Geometry and mass-property basis. The mesh cannot determine a structural centre of mass.

| Member | Start (x, y, z), m | End (x, y, z), m | Citation |
| --- | --- | --- | --- |
| DL1 | (4, 19, −17) | (−18.47, 6, −17) | [P], Table 3-2, p. 7 |
| DL2 | (−18.47, −6, −17) | (4, −19, −17) | [P], Table 3-2, p. 7 |
| DL3 | (14.43, −13, −17) | (14.43, 13, −17) | [P], Table 3-2, p. 7 |
| YL1 | (1.625, 2.815, −17) | (8.4, 14.6, −17) | [P], Table 3-2, p. 8 |
| YL2 | (−3.25, 0, −17) | (−16.87, 0, −17) | [P], Table 3-2, p. 8 |
| YL3 | (1.625, −2.815, −17) | (8.4, −14.6, −17) | [P], Table 3-2, p. 8 |
| CB1 | (1.625, 2.815, −16.2) | (11.43, 19.81, 9.13) | [P], Table 3-2, p. 8 |
| CB2 | (−3.25, 0, −16.2) | (−22.87, 0, 9.13) | [P], Table 3-2, p. 8 |
| CB3 | (1.625, −2.815, −16.2) | (11.43, −19.81, 9.13) | [P], Table 3-2, p. 8 |

Table B. Centreline endpoints, retaining published rounding.

The upper delta and Y pontoons lie above SWL and are excluded [P, Table 3-2].
The lower members and braces are included. Each member is extended into its
adjoining columns before a boolean union removes the hidden caps and internal
surfaces. These overlap extensions are numerical construction devices; they do
not change the exposed member centreline or diameter. The union is intersected
with the submerged half-space, and the artificial SWL cap is removed for export.

The approximate waterplane criterion of 380 m² is specified in [M]. Independently,
the column contribution is `pi*(6.5**2 + 3*12**2)/4`; the inclined brace
contribution is `3*pi*(1.6/2)**2/cos(theta)`, where `cos(theta)` is the vertical
component of a brace's unit axis. These expressions use only Tables A and B [P]
and give about 380.1 m², rather than the columns alone at 372.5 m².

## Generate and check

Run from the repository root with the configured cache and scratch environment.
The scripts require Python, NumPy and open-source Gmsh [G]. No repository package
or solver API is imported. The GDF header gravity below is the metre-unit value
specified by the public file-format manual [F, section 6.1]; it remains a required
generator argument rather than an inferred physical input.

```bash
uv run --no-project --with numpy --with gmsh==4.15.2 python \
  examples/hydrodynamics/oc4-semisub/generate_mesh.py \
  "$TMPDIR/oc4-public.gdf" --gravity 9.80665
uv run --no-project --with numpy python \
  examples/hydrodynamics/oc4-semisub/check_mesh.py "$TMPDIR/oc4-public.gdf"
```

The default target edge size is 1.25 m, with curvature refinement at 16 elements
per full circle [G, `Mesh.MeshSizeFromCurvature`]. These are numerical choices,
not publication dimensions. The reference output below uses Gmsh 4.15.2 and
contains 35,928 triangular panels. Triangles repeat the last vertex, normals
point into the fluid and symmetry flags are disabled for full-body export
[F, section 6.1]. No lid or irregular-frequency treatment is included.

The checker reads the exported coordinates, integrates signed tetrahedral
volumes and first moments, and closes only the SWL boundary implicitly for
hydrostatics. It calculates waterplane area from the oriented open boundary.
Every non-SWL edge must have exactly one occurrence in each direction. A hole,
duplicate face or reversed winding therefore cannot silently pass the topology
check. Aspect ratio means longest boundary edge divided by shortest boundary
edge, excluding the repeated triangle vertex; span means the maximum distance
between any panel vertices. Verdicts use unrounded values. Coordinates use
numerical tolerances stated in the code; the criteria in [M] are unchanged.

```text
Check | Limit | Value | Result
Waterline crossings | = 0 | 0.0 | PASS
Above-water vertices | = 0 | 0.0 | PASS
Degenerate panels | = 0 | 0.0 | PASS
Bad non-SWL edges | = 0 | 0.0 | PASS
Maximum aspect | <= 3.0 | 2.1 | PASS
Maximum span (m) | <= 3.66 | 1.7 | PASS
Displacement (m3) [T4-6] | 13917 +/- 1% | 13872.5 | PASS
Waterplane (m2) [criteria] | 380 +/- 1% | 376.9 | PASS
COB z (m) [T4-6] | -13.15 +/- 1% | -13.2 | PASS
COB x,y (m) [computed] | reported | -0.0, 0.0 | INFO
Published platform CM z (m) [T3-3] | metadata | -13.46 | INFO
Requested system CM z | required public source | NOT_EVALUATED | NOT_EVALUATED
```

Table C. Checker output from the default discretization. Values are computed;
reference quantities cite [P] and numerical acceptance limits cite [M].
The volume and waterplane discrepancies from [M] are 0.3% and 0.8%, respectively.

The checker returns a nonzero status for failed numerical checks or malformed
GDF. NOT_EVALUATED mass metadata is printed separately and does not imply mass
property acceptance. Before dynamic analysis, the system mass basis must be
resolved and solver-specific input compatibility and convergence assessed.

## Tests

```bash
uv run --no-project --with numpy --with gmsh==4.15.2 --with 'pytest<9' python -m pytest \
  tests/hydrodynamics/test_oc4_public_mesh.py \
  --basetemp="$TMPDIR/oc4-pytest" -p no:cacheprovider
uv run --no-project --with ruff ruff check \
  examples/hydrodynamics/oc4-semisub/*.py tests/hydrodynamics/test_oc4_public_mesh.py
```

The test module writes out the public dimension basis in its docstring. It
checks hydrostatics, the dimension-derived waterplane, draft and base extent,
mesh limits, CLI readback, malformed files and damaged topology. A synthetic unit
cube independently checks volume and buoyancy integration. All generated files
are temporary test outputs; none is a stored solver result.
