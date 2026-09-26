# Curvature screening with HullProd

How to obtain a curvature / fairness signature for a hull mesh through the `hull_library`
API, what the values mean, and where the signature is stored. Background, evidence and
limits: [`hullprod-curvature-screening-evaluation.md`](hullprod-curvature-screening-evaluation.md)
(issue [#2170](https://github.com/vamseeachanta/digitalmodel/issues/2170)).

## Install

```bash
uv pip install -e '.[curvature]'      # adds hullprod (BSD-3, bundles OpenCascade, ~150 MB)
```

Python 3.10 to 3.12 only. Without the extra, every function below raises an actionable
`ImportError`; `hullprod_available()` reports the state and the diffraction quality gate
becomes a no-op.

## API

```python
from digitalmodel.hydrodynamics.hull_library import (
    HullCatalog, HullParametricSpace, MeshGeneratorConfig,
    screen_panel_mesh, screen_profile,
)

# 1. A PanelMesh (from HullMeshGenerator, a GDF loader, ...)
result = screen_panel_mesh(mesh, lref=profile.length_bp, hull_type=profile.hull_type)
sig = result.signature            # CurvatureSignature (pydantic)
sig.as_vector()                   # [I_D, I_D_plus, I_D_minus, a_flat, a_single, a_elliptic, a_saddle]
result.fields.K, result.fields.H  # per-vertex Gaussian / mean curvature on the expanded mesh

# 2. A HullProfile (mesh generated for you, lref = length_bp)
sig = screen_profile(profile, MeshGeneratorConfig(target_panels=2000)).signature

# 3. Catalog and parametric sweeps
catalog.screen_hull("kvlcc2")                       # stores entry.curvature_signature
for vid, prof, sig in space.generate_signatures(catalog):
    ...
```

`CurvatureSignature` carries the seven values plus `lref`, `lref_mode`, `reliability`
(`good` / `caution` / `poor`), `status` (HullProd validity), `valid_area_fraction`, panel and
vertex counts, `hullprod_version`, `hull_type`, `crease_dominated` and `notes`.
`PanelCatalogEntry.curvature_signature` serialises to YAML with the catalog.

## Reading the values

| Symbol | Meaning |
|---|---|
| `I_D` | area-averaged `|K| L_ref^2`; 0 for a developable surface |
| `I_D_plus` / `I_D_minus` | elliptic (synclastic) and saddle (anticlastic) parts of `I_D` |
| `a_flat`, `a_single`, `a_elliptic`, `a_saddle` | area fractions by curvature class, sum to 1 |

Reference values from the 2026-09-25 evaluation (coarse BEM meshes, mesh backend):
FPSO `a_single` 0.75; drillship `a_flat` 0.52, `a_elliptic` 0.23, `a_saddle` 0.21;
unit sphere `I_D` 4.00 exactly; cylinder `a_single` 1.00 exactly.

## Rules of use

1. Always pass `lref` (`length_bp`). The automatic reference length is a principal-axis span
   and differs between hull types.
2. Compare hulls at matched panel density. Mesh values converge first order; a coarse BEM mesh
   under-reads `I_D` by 10 to 15 percent.
3. Semi-sub, spar, TLP, cylinder and lid meshes are `crease_dominated`: the signature measures
   column/pontoon junctions and facet edges, not plate curvature. No saddle threshold applies.
4. The descriptors are geometry screening values, not fabrication-cost or forming predictors.

## Quality gate (diffraction)

`diffraction.quality_gates.run_mesh_quality_gate(path, curvature=True, hull_type=..., lref=...)`
adds the HullProd gate when the extra is installed: `reliability == poor` (sliver triangles,
non-manifold edges, disconnected components, extreme valence) **blocks**; a monohull
`a_saddle` above 0.35 **warns**. The signature is written to `mesh_quality_report.json`
under `curvature`.

## Station lofting (D3)

`HullMeshGenerator` lofts stations with shape-preserving PCHIP interpolation in both x and z
(`_shape_preserving_interp`). The previous linear lofting produced ruled surfaces, which have
`K <= 0` everywhere, so every generated monohull read as 60 percent saddle with no elliptic
bow or stern. On the fixture ship the change moves `a_elliptic` from 0.16 to 0.60 and
`a_saddle` from 0.60 to 0.40 at 7 225 panels. Regression baselines that depend on generated
vertex positions shift slightly; record before/after signatures when re-baselining.

## Citation

Serani, A. and Maki, K. J. (2026). Geometry-Based Metrics for Early-Stage Hull-Form
Producibility Screening. arXiv:2609.27544. Software: https://github.com/cnr-inm-mao/hullprod

## BRep route

For a signature independent of panel discretization, fit the profile offsets to a
C2 B-spline surface using the OpenCascade bindings bundled with HullProd. FreeCAD is
not required. The mesh route remains the default.

```python
from digitalmodel.hydrodynamics.hull_library import profile_to_step, screen_step

path = profile_to_step(profile, "hull.step")
brep = screen_step(path, lref=profile.length_bp)
both = screen_profile(profile, representation="both")
both.provenance["brep_signature"]
both.provenance["representation_delta"]
catalog.screen_hull(profile.name, representation="both")
# entry.curvature_signature: mesh; entry.curvature_signature_brep: BRep
```

`representation="brep"` returns only the BRep result; `"both"` returns the mesh
result with the BRep signature and seven comparison values in provenance. Existing
YAML signatures without a representation load as `"mesh"`. BRep results carry
`reliability="not_applicable"`, native metric validity, no per-vertex fields, and
zero mesh panel/vertex counts. Native face counts remain in provenance. Source
identifiers contain only the file name.

All public geometry coordinates, fitting tolerances and reference lengths are in
metres. STEP export declares metres and preserves physical dimensions. HullProd
1.0.1 imports STEP and IGES into millimetres: `screen_step` converts the reference
length by 1,000 for assessment and returns the original metre reference in the
signature. Raw HullProd provenance bounds/reference lengths remain in millimetres.
Direct `hullprod.assess` calls must supply a reference length in its import units.
Persistent HullProd caching and display tessellation are disabled for this scalar
route. The accepted `workdir` controls temporary profile exports; direct
`screen_step` writes no work files.

The orchestrator's Wigley spike (100 × 10 × 6.25, 41 × 11 samples) reported
`I_D=4.3094`, `I_D_plus=2.1545`, `I_D_minus=2.1549`,
`a_elliptic=0.6263`, `a_saddle=0.3737`, with native status `valid`.
The test comparator independently integrates the analytic Gaussian curvature
and area element; sphere and cylinder controls check exact values.

Comparison requires the same reference length and represented surface. Deltas are
`abs(mesh - brep)`, with relative delta divided by `abs(brep)`: zero when both
vanish, null when a nonzero mesh value is compared to zero. Unavailable native
metrics retain their validity status and are represented by NaN in the signature;
their absolute and relative deltas are null.

The default fit samples at least 41 × 11 points (or the number of input stations
and offsets, when larger), using the mesh generator's shape-preserving interpolation.
Explicit `n_x` and `n_z` override those counts. Mesh independence does **not**
remove fitting sensitivity: the fixture ship gives BRep `I_D=5.7746` and mesh
`I_D=3.8280` at 7,225 half-hull panels, a **33.71%** relative gap. The planned
15% expectation remains a strict expected-failure test. A 161 × 41 fit gives
`I_D=4.4389`; an 81 × 21 fit is `quadrature_unconverged`. The coarse profile's
PCHIP joins and subsequent C2 approximation change second derivatives; increasing
fit density alone does not guarantee stable curvature. Check native validity and
fit sensitivity before using the signature as a reference.

A planar bottom is included by default when the keel has nonzero breadth; zero-area
bottoms are omitted. It follows the sampled keel polygon. For curved keel edges,
that polygon may not sew to the fitted side within tolerance: the returned shape
can contain disconnected faces, although both surfaces are assessed. Self-touching
bottom polygons are rejected. No end caps or waterplane are added.
Use `bottom=False` to omit the bottom explicitly. Omitting nonzero bottom area
changes **all** area-normalized values, including the three `I_D` values, not only
`a_flat`. This corrects the narrower comparability claim in the original plan.
GDF/panel inventories have no BRep surface and continue to use the mesh route.
