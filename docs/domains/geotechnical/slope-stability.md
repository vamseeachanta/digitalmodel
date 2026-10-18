# Slope stability and seepage screening

The slope_stability workflow uses xslope 1.0.3 for two-dimensional plane-strain
Bishop and Spencer circular searches and optional steady finite-element seepage.
Results are screening calculations; no design acceptance threshold is assigned.

## Revision history

| Revision | Date | Change and purpose |
| --- | --- | --- |
| 0 | 2026-10-14 | Add the SI profile interface and public benchmark reproductions for slope screening. |
| 2 | 2026-10-18 | Require an explicit positive-gradient datum; use and report multi-seed grid search. |
| 1 | 2026-10-15 | Add the Bishop search benchmark, seepage handoff and example tests, and hosted solver execution; clarify validation limits. |
| 3 | 2026-10-18 | Isolate seed refinements to prevent cache sharing; preserve thin user-circle mechanisms; disclose group minima, the coarse filter and per-search unsolved counts; require explicit search mode in result records. |

## Basis and boundaries

xslope uses soil properties: cohesion, friction angle, unit weight, undrained
strength and permeability. It does not derive properties from CPT, SPT,
laboratory or other test data. Property sourcing remains with the engineer and
<code>data/geotechnical/geotech_database.yaml</code>. Mechanical finite-element runs
would also require stiffness and Poisson ratio; those runs are outside this adapter.
This workflow does not replace mudmat bearing capacity, pile or suction-caisson
capacity, p-y springs or liquefaction calculators.

Potential applications include seabed and shore-approach slopes, pipeline
landfall cuts, trenches and sheetpile seepage, pits, dams, levees and bunds.
Rapid drawdown is an upstream use case; this adapter supports steady seepage
followed by stability only. Transient and staged drawdown, finite-element
strength reduction, reliability studies, test-data interpretation, xslope Studio
and the ai extra are unsupported.

## Dependencies

Imports are lazy. The tested combination is xslope 1.0.3, gmsh 4.15.2 and NumPy
1.26.4. The required range is gmsh>=4.15 and numpy>=1.24.0,<2.0.0.
The optional slope extra and regenerated lock are not delivered: lock regeneration
cannot resolve the declared sibling path dependency in an isolated checkout.
Base dependency declarations and lock are preserved. An independently managed
environment can install the dependencies explicitly:

~~~sh
pip install "xslope==1.0.3" "gmsh>=4.15" "numpy>=1.24.0,<2.0.0"
~~~

Install and import were exercised on Linux x86-64 with Python 3.11 and 3.12.
Windows x64 and Linux Arm are not tested; the adoption issue records no gmsh
Linux aarch64 wheel. Python 3.13 is not established by this work. Linux gmsh
requires the system OpenGL libraries noted in the
[xslope README](https://github.com/njones61/xslope/blob/main/README.md).

## SI profile schema

The [example input](../../../examples/workflows/slope-stability/input.yml)
converts the public README workbook using Pint; it contains no site data.
The adapter accepts SI only and performs no implicit conversion. Positive y is
elevation, not depth. Coordinates are metres, weights kN/m³, strengths kPa,
angles degrees, conductivity m/s and seismic coefficient dimensionless.

| Field | Meaning |
| --- | --- |
| units | Literal SI |
| ground_surface_m | Increasing-x polyline of [x, elevation] |
| bottom_elevation_m | Horizontal model base elevation |
| water_unit_weight_kn_m3 | Explicit positive water unit weight |
| layers | One to fifteen layers, ordered top to bottom |
| unit_weight_kn_m3 | Positive bulk weight; gamma_kn_m3 is an alias |
| saturated_unit_weight_kn_m3 | Optional saturated weight, at least bulk weight |
| c_kpa, phi_deg | Drained effective cohesion and friction angle |
| su_kpa | Alternative undrained strength; existing Su_kpa spelling accepted |
| su_gradient_kpa_per_m | Non-negative strength gradient; default zero |
| reference_elevation_m | Required explicit strength datum when the gradient is positive; immaterial at zero gradient |
| permeability_m_s | Optional positive isotropic conductivity |
| top_boundary_m | Lower layers only: full-width increasing-x polyline |
| water_table_m | Optional full-width piezometric line |
| seismic_coefficient | Non-negative horizontal pseudo-static coefficient; default zero |
| circles | One to ten starting circles with x_m, y_m, positive radius_m |
| seepage.element_size_m | Positive target linear-triangle mesh size |
| seepage.heads_m | Two to five head blocks, each with head_m and boundary_m |

Undrained strength maps to upstream cp behavior:
Su(y) = su_kpa + su_gradient_kpa_per_m × max(0, reference_elevation_m − y).
This is the governing cp equation in xslope 1.0.3, `xslope/slice.py`
(strength at the slice base). A positive gradient without an explicit
reference_elevation_m is rejected before importing the solver. Constant strength
(gradient zero) may omit the datum because the equation is independent of it.
For example, su_kpa=5, gradient=1.5 and reference=-100 m give Su=20 kPa at
z=-110 m; the same relative geometry at a +100 m datum gives the same strength.
Undrained layers use total stress; drained layers use the piezometric line when
supplied. Seepage supplies its nodal pore pressures to drained layers on the
same model before stability runs. Every seepage layer requires conductivity.

Overlapping existing fields are accepted only where units and meaning match
(Su_kpa, phi_deg). SPT counts, stress samples and pile geometry are not slope
properties and are rejected. Unknown fields, conflicting aliases, Booleans,
non-finite numbers, negative strengths, crossing boundaries and a units label
other than SI are rejected. Numeric values are not checked for unit plausibility;
the caller shall supply values in the declared units. Layer tops span the same
horizontal extent. Vertical or
overhanging polylines are unsupported. Head boundary polylines currently require
increasing x; other boundaries remain no-flow by default. Upstream preflight
checks whether specified heads bind mesh edges.

## Running and reading results

~~~python
from digitalmodel.engine import engine
result = engine("examples/workflows/slope-stability/input.yml")
~~~

Set slope_stability.outputs.directory to the output directory. Temporary workbooks
are populated from the installed upstream template there and removed after
loading; source workbooks are untouched. The JSON summary records xslope_version,
units, requested slices, each method's FS, critical circle, search_mode and search convergence.
Seepage adds discharge per unit out-of-plane width, length/time units,
convergence and relative mass-balance error. No design report is generated.
A failed seepage convergence stops calculation. A non-converged search is
labelled not_converged; a converged search is screening_only. Geometry bounds
and soil properties require engineering review. Each method searches independently.
The adapter records search_mode="grid" for its combined search and
"fixed_circle" for a direct evaluation. The search uses xslope 1.0.3
`_grid_seed_circles` to retain coarse tangent families, then calls
`circular_search(seed="circles")` separately for every retained seed and every
user circle. Each refinement has its own cache, including when two seeds share a
centre but have different depths. The lowest solved FS is reported. The upstream
coarse sweep retains at most four families within 25% of its coarse minimum;
it can discard a family whose refined minimum would govern.

The coarse sweep excludes surfaces thinner than 5% of the slope height
(maximum minus minimum ground elevation in `_grid_seed_circles`). Refinement applies no thin-surface filter, preserving thin mechanisms
from user circles. JSON records `grid_thin_surface_filter="coarse_only_5_percent"`
and `search_factors_of_safety` with separate `grid` and `user_circles` minima;
a null minimum means that group produced no solved surface. Unsolved counts
include the sweep and all refinements, with their individual records retained.
Convergence describes the selected refinement and does not certify a global
minimum. Geometry bounds, coarse family retention and unsolved trials can omit
mechanisms. Screening requires sensitivity checks and engineering review.
The governing upstream implementation is `xslope/search.py` in version 1.0.3;
its seed helper defaults to `keep=4`. Profile construction and searches of
workbook-loaded models reject other xslope versions. Direct workbook
fixed-circle evaluation records the installed version without this guard.
A rejected solver result in a nonempty seed cache stops the method rather than
reporting another seed as if that rejection had not occurred.

## Benchmarks and limits

Tests reproduce README Bishop FS = 1.279 at 20 slices and pin the adapter search
re-baseline FS = 1.214733 at 40 slices to 0.01% relative tolerance (still rounding
to the published 1.215). Tests also reproduce
[SEEP-1](https://xslope.org/en/latest/tutorials/seep01_sheetpile/) discharge
40.111 m³/yr per m on a 0.5 m linear-triangle mesh. A 1% relative tolerance
allows mesh/platform variation and printed-value rounding. The equivalent SI
profile and workbook each reproduce the search minimum. The SI single-circle
result is also compared directly with the README workbook at 1e-8 relative
tolerance. Spencer searches are exercised for convergence and serialization;
no published Spencer benchmark is established here. Fixture provenance,
digests and upstream licence notices accompany the workbooks.

The synthetic two-family regression uses a shallow fill above a stiff crust
and a weak undrained foundation. The search is bounded above by a directly
re-evaluated deep circle (FS=0.689, with 1% search tolerance) obtained from an
earlier xslope 1.0.3 grid search. This is a repeatability check, not an
independent minimum: the circle is tangent to the model floor
and exits near the domain edge. Domain expansion remains necessary to assess
boundary sensitivity. These synthetic values are test bounds only.
Stability tests run without Gmsh; only seepage solves skip when it is absent.

The SI profile-driven seepage test checks metre/second units and the drained
material pore-pressure handoff before both stability searches solve. Failed
seepage convergence raises RuntimeError before its outputs are consumed,
including when those outputs contain NaN. The YAML example is compared with
the public workbook conversion and run through the engine with temporary output.

At the previously reviewed revision, hosted tests skipped the solver module
because xslope was absent; solver benchmarks were local only. The specialized
hosted shard now includes an additional geotechnical test step with xslope 1.0.3
and gmsh>=4.15, plus the Gmsh runtime libraries. Hosted success at a new revision
shall be confirmed from that step's result; configuration alone is not evidence
of a completed hosted solve.

The upstream [validation overview](https://xslope.org/en/latest/verification/)
and [groundwater corpus](https://xslope.org/en/latest/verification/rocscience_groundwater/)
are author-reported benchmarks, not independent qualification. The adoption issue
records two mismatches among 21 groundwater problems, a single maintainer and a
1.0 release date of 2026-10-11. The release is new and changes rapidly; the exact
pin and both reproductions need reassessment on every version change. These two
examples do not establish suitability for a particular design or all seepage
boundary conditions. See the upstream
[input template](https://xslope.org/en/latest/usage/input_template/) and
[seepage-to-stability interface](https://xslope.org/en/latest/seep/seep_slope/).

## Third-party licences

xslope is Apache-2.0; gmsh is GPL and is not vendored. Workbooks retain the
upstream Apache-2.0 licence and notice. See
[third-party notices](THIRD_PARTY_NOTICES.md).
