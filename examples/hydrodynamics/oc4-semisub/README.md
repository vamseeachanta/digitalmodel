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
| B | 2026-10-20 | Hosted test coverage is added, the dependency pin and reference platform are clarified, and every adjacent triangle repeat is supported. |
| C | 2026-10-20 | Buoyancy depth is reported as information because no acceptance tolerance is specified, and a translated cube checks the integration origin. |
| D | 2026-10-26 | Extended-band hull and lid generation, versioned inputs and complex exports are added for offline preparation; native acceptance remains open. |

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
uv run --no-project --with 'numpy<2' --with gmsh==4.15.1 python \
  examples/hydrodynamics/oc4-semisub/generate_mesh.py \
  "$TMPDIR/oc4-public.gdf" --gravity 9.80665
uv run --no-project --with 'numpy<2' python \
  examples/hydrodynamics/oc4-semisub/check_mesh.py "$TMPDIR/oc4-public.gdf"
```

The historical target edge size was 1.25 m, with curvature refinement at 16 elements
per full circle [G, `Mesh.MeshSizeFromCurvature`]. These are numerical choices,
not publication dimensions. The reference output below was generated on Linux
x86-64 with Python 3.11.14, NumPy 1.26.4 and Gmsh 4.15.1, matching the repository's
`cfd` extra. It contains 35,928 triangular panels. Panel counts and rounded
hydrostatics can vary across platforms and Gmsh releases; acceptance depends on
the numerical criteria, rather than exact reproduction of these counts or values.
The generator repeats the last vertex of each triangle, normals
point into the fluid and symmetry flags are disabled for full-body export
[F, section 6.1]. That historical output contains no interior lid.

The checker reads the exported coordinates, integrates signed tetrahedral
volumes and first moments, and closes only the SWL boundary implicitly for
hydrostatics. It calculates waterplane area from the oriented open boundary.
Every non-SWL edge must have exactly one occurrence in each direction. A hole,
duplicate face or reversed winding therefore cannot silently pass the topology
check. Aspect ratio means longest boundary edge divided by shortest boundary
edge, excluding any adjacent repeated triangle vertex, including the closing
fourth/first pair permitted by [F, section 6.1]; span means the maximum distance
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
COB z (m) [T4-6] | reference -13.15 | -13.2 | INFO
COB x,y (m) [computed] | reported | -0.0, 0.0 | INFO
Published platform CM z (m) [T3-3] | metadata | -13.46 | INFO
Requested system CM z | required public source | NOT_EVALUATED | NOT_EVALUATED
```

Table C. Illustrative checker output from the reference platform and default discretization. Values are computed;
reference quantities cite [P] and numerical acceptance limits cite [M].
The buoyancy depth is reported against [P, Table 4-6] as information because [M]
specifies no tolerance for it; the eight PASS/FAIL checks retain their limits.
The volume and waterplane discrepancies from [M] are 0.3% and 0.8%, respectively.

The checker returns a nonzero status for failed numerical checks or malformed
GDF. NOT_EVALUATED mass metadata is printed separately and does not imply mass
property acceptance. Before dynamic analysis, the system mass basis must be
resolved and solver-specific input compatibility and convergence assessed.

## Tests

```bash
uv run --no-project --with 'numpy<2' --with gmsh==4.15.1 --with 'pytest<9' python -m pytest \
  tests/hydrodynamics/test_oc4_public_mesh.py \
  --basetemp="$TMPDIR/oc4-pytest" -p no:cacheprovider
uv run --no-project --with ruff ruff check \
  examples/hydrodynamics/oc4-semisub/*.py tests/hydrodynamics/test_oc4_public_mesh.py
```

The test module writes out the public dimension basis in its docstring. It
checks hydrostatics, the dimension-derived waterplane, draft and base extent,
mesh limits, CLI readback, malformed files and damaged topology. A synthetic unit
cube, both unshifted and translated horizontally, independently checks volume,
waterplane area and buoyancy integration. All generated files
are temporary test outputs; none is a stored solver result.

Hosted execution belongs to `.github/workflows/gmsh-meshing-tests.yml`, which
installs the Gmsh runtime library and the locked `cfd` and `test` extras. Its push
and pull-request filters include this example and its tests. Registration
regressions check those filters and the executable pytest arguments, so metadata
alone cannot substitute for running the tests.

## Extended band, version 2

The default target is now 0.55 m, followed by a maximum pairwise vertex-span
check on the exported coordinates. `--pair` produces independently meshed coarse
and fine hulls and separate SWL interior lids; all four meshes are checked at
1500 m and 3.50 rad/s. The limit follows `omega² = g k tanh(k h)`,
`lambda = 2 pi/k` and `span <= lambda/6`, with `g = 9.80665 m/s²` and six panels
per wavelength **ASSUMED** from the supplied study criterion. The resulting
unrounded limit is 0.8383265196279277 m. This is an input-resolution gate;
response convergence remains unestablished.

```bash
uv run --no-project --with 'numpy<2' --with gmsh==4.15.1 python \
  examples/hydrodynamics/oc4-semisub/generate_mesh.py \
  "$TMPDIR/extended-packet" --gravity 9.80665 --pair
uv run --no-project --with pyyaml python \
  examples/hydrodynamics/oc4-semisub/build_datafile_v2.py \
  "$TMPDIR/extended-packet/coarse-model-v2.yml" coarse-hull-v2.gdf
uv run --no-project --with 'numpy<2' --with pyyaml python \
  examples/hydrodynamics/oc4-semisub/input_checks_v2.py \
  "$TMPDIR/extended-packet" coarse
```

The builder is also run for `fine-model-v2.yml` and `fine-hull-v2.gdf`. DF0
checks the exact versioned datafile, platform mass and inertia, headings,
350-frequency list, GDF gravity, generation sources and mesh digests. DF3
checks the hull and reference lid against the upper frequency. Completed mesh
cases are reused only after their recorded inputs and payload digests match.
A partial case is retained and reported for inspection, rather than overwritten.
The mesh payloads exceed the publication size limit and are retained outside
Git; the checked SHA-256 manifests are included as generation receipts. Regeneration
on a different Gmsh platform can change the bytes; both the receipt comparison
and the physical checks are required before those bytes become solver inputs.

The separate lid is a checked reference covering every column and brace
waterline. The datafile requests automatic native interior panels with
`BodyAddInteriorSurfacePanels: Yes`; the native panel footprint and span still
require readback against that reference. `HasResonanceDampingLid: No` controls
a different damping mechanism and remains unchanged. No assertion is made that
the native automatic panels reproduce the reference triangulation.

`input-specification-v2.json` and `expected-v2.json` preserve the supplied study
basis, including the historical DF5.2 and DF5.3 FAILs. The old builder and
expected record are not present in this checkout and are not replaced.
The platform mass basis is unchanged; its suitability for a complete system
is unverified. The additional 1.983 rad/s diagnostic is separate from the exact
350-frequency production list.

`complex_export_v2.py` requires a `ResultAdapter` for snapshot, native save
and native reload. Every motion RAO and both excitation formulations retain
real and imaginary parts; axes, coordinate and heading conventions, origins,
SI units and the harmonic time sign are explicit. The adapter converts native
units before export. Reload checks cover frequencies, headings, full added-mass
and damping matrices, all three complex response arrays and metadata exactly.
Only a fake adapter is tested here; native integration remains open.

The [probe and acceptance plan](extended-band-plan.html) defines the smoke,
lid, high-band, mesh and frequency-refinement checks. It also records the
retention basis and the missing historical acceptance criteria. No licensed
solver is imported or executed by this example's offline preparation.

Generation receipts bind source bytes as well as mesh bytes. Applying the
standard licence header changes source digests; a header-only equality proof
and renewed DF0 checks are required before the normalised receipt is used.

## Staged preparation v3

The [staged acceptance plan](staged-band-plan-v3.html) separates strength from
fatigue acceptance. Version 2 and its historical high-band failures are retained.
`mesh_band_v3.py` measures serialized panel spans and inverts finite-depth
dispersion at 1500 m. The six-panel wavelength rule is ASSUMED from the study
criterion. A declared-band DF3 pass does not clear the 3.50 rad/s failure.

`input-specification-v3.json` is a recipe requiring the exact retained L1 mesh.
Stage S has 27 frequencies: 0.30 to 0.55 rad/s at 0.01 rad/s, plus the specified
0.418879 rad/s. Its upper frequency is below the supplied 1.983 rad/s
irregular-frequency estimate, so no interior lid is requested. Stage F reaches
the measured L1 band, includes the three unrounded spectral peaks and requests
automatic native interior panels when the band reaches that estimate. The
separate reference lid matches the retained hull waterline and panel scale.
Native lid geometry still requires readback; reference panels are not imported
as a separate physical body.

Both decks, a four-frequency L2 probe deck and conditional resource estimates
are written by the offline packet builder. Inputs are local GDF paths supplied
by the caller; output shall be an empty directory outside the checkout.

```bash
uv run --no-project --with 'numpy<2' --with gmsh==4.15.1 --with pyyaml python \
  examples/hydrodynamics/oc4-semisub/prepare_staged_v3.py \
  "$OUTPUT_DIRECTORY" "$L1_MESH" --l2-mesh "$L2_MESH"
uv run --no-project --with 'numpy<2' --with pyyaml python \
  examples/hydrodynamics/oc4-semisub/input_checks_v3.py "$OUTPUT_DIRECTORY"
```

DF0 checks both stages, the optional L2 probe and the retained-byte/source
digests. Both v2 and v3 independently check serialized hull topology, panel
aspect, displacement and waterplane against the existing platform criteria.
Closure permits only the implicit SWL cap. A consistent hash does not excuse
damaged geometry. Lid acceptance remains separate. Completed packets are reused
only after all checks pass; partial packets are retained and rejected.

The retained L1 and L2 inputs have been located and their six supplied digests
verified. The [measurement receipt](retained-v3/retained-measurements-v3.json)
binds the exact spans, supported endpoints and generated decks. L1 has 14,064
panels and L2 has 35,776; both independent hull checks pass. Stage S has 27
frequencies and Stage F has 244. The four L2 convergence probes remain separate
from the indicative above-band envelope probes. Displayed spans and frequencies
are rounded; generation uses the unrounded receipt values.

The [reference lid](retained-v3/l1-lid-v3.gdf) has 2,446 panels, maximum span
1.2 m and SHA-256
`5c2dc8313de480d32a989af2ffc301c279e327045e10413685fcf88229e41626`.
Waterline coverage, separation and Stage F declared-band DF3 pass with zero
oversized panels. The executing Gmsh package is recorded as 4.15.1; unsupported
versions are rejected before generation. The receipt includes the generator
command. Conditional estimates with this lid are 1.2 h for Stage S and 10.4 h
for Stage F, both at 49.1 GB, using `native_estimate_v3.py`. Native automatic-lid
equivalence, native capacity and engineering acceptance remain OPEN.

`tail_response_v3.response_bound` accepts the reload-verified JSON export from
`complex_export_v2.py`, the selected fatigue seas and the exact declared band
endpoint. Each required heading is evaluated separately; absent headings fail
closed. The return value includes all six motions and three hang-off motion
components at (0,0,-10) m, in-band spectral moments m0 and m2, omitted wave
fractions, conditional upper moment bounds and bound/in-band diagnostic ratios.
Optional real stress-transfer coefficients produce a stress-range proxy. The
ASSUMED default threshold is 1.0%, applied independently to the variance tail
fraction and the narrow-band fatigue-damage tail fraction. Their denominators
are the in-band-plus-bound variance and damage respectively. Damage is
`1 - D(in-band)/D(in-band plus tail bound)`, with
`D proportional to m0^((m-1)/2) * sqrt(m2)` and default S-N slope 3, ASSUMED for
review. No wide-band correction is implemented. Gamma values other than the
supplied 1.0 are rejected. Occurrences and spectra are never renormalised.

The last 0.5 rad/s envelope is applied only as a bound. Its validity beyond the
band requires quantitative review for unresolved resonances; numerical passes
retain `release_eligible=false`. The lower omitted tail uses the full-band
maximum. Point envelopes use component bounds and the triangle inequality,
without credit for translation/rotation cancellation outside the measured band.
No physical response is extrapolated. Strength release requires its applicable
native and convergence gates; fatigue additionally requires the reviewed tail
condition, irregular-frequency checks and whole-band convergence evidence.

The optional `probe_envelope` argument supplies a reload-verified complex export
under `record`, plus `mesh_max_span_m` and `mesh_depth_m`. Above-band probes
must share the in-band units, origin, coordinate frame, phase convention and
selected heading. The receipt records INDICATIVE status, a computed supported
upper frequency and each probe's panels-per-wavelength shortfall. A suffix-maximum
staircase bounds the complex linear chords between the band limit and probes;
a rising probe raises preceding steps. The final probe magnitude is held to
infinity. Exact wave-tail moment differences integrate the steps. Component
motion envelopes precede the hang-off and stress triangle-inequality transfers.
This remains a conditional bound: unresolved resonances and insufficient probe
resolution require independent review. Existing flat last-window envelopes remain
available. The [acceptance plan](staged-band-plan-v3.html) specifies the first
batch at 2.6, 3.0 and 3.5 rad/s without extending the production response band.

`scan_irregular_spikes` flags each motion magnitude exceeding three times both
immediate neighbours inside the default 1.9 to 2.45 rad/s window. The factor is
configurable and greater than one. Interpolation uses real and imaginary parts
between unflagged neighbours; raw arrays remain unchanged. Every `response_bound`
result records window coverage, flagged counts and points, and the changes in
variance and both tail fractions using identical envelopes. The full default
window exceeds L1 support and is recorded as PARTIAL. Interpolation is applied
only with `interpolate_spikes=True`; a substantive `verified_lid_statement` may
instead record the caller's native-lid evidence. Neither disposition grants
release. Endpoints and broad resonances are outside this isolated-spike rule.

The focused baseline passed 126 tests. The final focused suite passed 146 tests,
with seven additional CI-registration tests passing, for 153 total. All checks
were run against the uncommitted tree. Synthetic tests establish software
behaviour only; native response and engineering release remain OPEN.

Lesson: an approximate span supports an illustrative estimate; exact retained
coordinates are required for production bands and source-bound decks. Hull
acceptance also requires independent geometry checks, and fatigue tail acceptance
requires both spectral moments rather than variance alone.
