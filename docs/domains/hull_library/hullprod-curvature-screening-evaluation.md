# HullProd curvature screening: evaluation for hull_library and parametric hull forms

| Field | Value |
|-------|-------|
| **Date** | 2026-09-25 |
| **Issue** | [#2170](https://github.com/vamseeachanta/digitalmodel/issues/2170) |
| **Tool** | HullProd 1.0.1, CNR-INM, BSD-3-Clause, `pip install hullprod`, https://github.com/cnr-inm-mao/hullprod |
| **Paper** | Serani, A. & Maki, K. J. (2026). *Geometry-Based Metrics for Early-Stage Hull-Form Producibility Screening*. arXiv:2609.27544, DOI 10.48550/arXiv.2609.27544 |
| **Source** | LinkedIn post by Andrea Serani, 2026-09 (BEAM project, ONR N00014-26-1-2164) |
| **Spike** | [`docs/spikes/2026-09-25-hullprod-curvature-screening/`](../../spikes/2026-09-25-hullprod-curvature-screening/README.md) |
| **Verdict** | **Adopt as an optional curvature/fairness screen.** Not a producibility or cost predictor for our hull types. |

---

## 1. What HullProd is

HullProd takes a hull surface, either native CAD (IGES/STEP BRep, evaluated by direct surface
derivatives and trimmed-domain quadrature) or a triangle mesh (STL/OBJ/PLY, Rusinkiewicz discrete
curvature), and reports a seven-value dimensionless signature:

| Symbol | Meaning |
|---|---|
| `I_D` | area-averaged `|K| L_ref^2` over the valid surface (0 for a developable surface) |
| `I_D_plus`, `I_D_minus` | the elliptic (synclastic) and saddle (anticlastic) parts of `I_D` |
| `a_C_flat`, `a_C_single`, `a_C_elliptic`, `a_C_saddle` | area fractions by curvature class, summing to 1 |

`K` is Gaussian curvature, `H` is mean curvature, `L_ref` is a declared or automatic
(principal-axis span) reference length. Per-vertex `H`, `K`, class id and validity are exported as
CSV and ParaView VTP, with curvature-class and density maps as PNG, a static HTML report,
`validity.json` and `provenance.json` (input hash, versions, settings).

The authors are explicit that these are geometry screening descriptors only: no forming
simulation, no panelization, no cost, no fairing. Mesh results are always flagged
`MESH-SENSITIVE`; BRep is the canonical realization.

## 2. Where our ecosystem stands today

Nothing in the ecosystem computes surface curvature as a quality measure:

- `hydrodynamics/hull_library/mesh_generator.py` uses |d²y/dx²| only to place panels (`adaptive_density`).
- `hull_library/coarsen_mesh.py` preserves high-curvature regions during coarsening.
- `line_generator/panelizer.py::MeshQuality`, `diffraction/quality_gates.py`,
  `orcawave/panel_mesh.py` check aspect ratio, skew, minimum angle, watertightness, normals and panel count.
- `naval_architecture/hull_form.py` checks Cb/Cp/Cm/Cwp ranges.

There is no Gaussian/mean curvature, developability, fairness or plate-forming check, and
`parametric_hull.py::HullParametricSpace` can only scale a catalog hull in L/B/T/D. It cannot
change the form, so it produces no measure of how a variant's surface differs from the parent.
(Confirmed against `workspace-hub/docs/assessments/hull-library-audit.md`, 2026-04-02.)

## 3. What was run

HullProd installed cleanly into a Python 3.12 venv on the dev-primary Windows 11 workstation, bundling
OpenCascade via `cadquery-ocp`. Each hull assessed in 0.4 to 1.2 s.

Our panel meshes are WAMIT GDF. HullProd does not read GDF, so the spike adds a 60-line
`gdf_to_stl.py` (quad split, ISX/ISY mirroring, vertex merge). OBJ was read directly.

### 3.1 Ecosystem hulls (auto `L_ref`, mesh backend)

| Hull | Source | Faces | `I_D` | flat | single | elliptic | saddle | Mesh reliability |
|---|---|---:|---:|---:|---:|---:|---:|---|
| client drillship DS-A | `<client-wiki>/cases/orcawave-diffraction-<DS-A>/<DS-A>-hull.gdf` | 20 473 | 67.3 | 0.52 | 0.04 | 0.23 | 0.21 | caution (open boundary) |
| OC4 semi-sub | `docs/domains/orcawave/examples/L02 OC4 Semi-sub/*.gdf` | 3 540 | 47.3 | 0.12 | 0.32 | 0.44 | 0.12 | caution (4 components) |
| client heavy-lift vessel HLV-A | `<client-wiki>/cases/orcawave-diffraction-<HLV-A>/<HLV-A>.gdf` | 2 928 | 20.1 | 0.67 | 0.00 | 0.18 | 0.15 | caution (open boundary) |
| L01 default vessel | `docs/domains/orcawave/examples/L01_default_vessel/L01 Vessel mesh.gdf` | 1 514 | 15.1 | 0.43 | 0.20 | 0.25 | 0.12 | caution |
| FPSO | `<client-wiki>/cases/orcawave-diffraction-fpso/fpso.gdf` | 1 760 | 10.2 | 0.13 | 0.75 | 0.09 | 0.03 | caution |
| floating production vessel FPV-A | `worldenergydata/.../hulls/<FPV-A>.obj` (Rhino export) | 30 k | 6.6 | 0.00 | 0.00 | 0.21 | 0.78 | **poor** (slivers, 21 non-manifold edges, 25 components, valence 112) |
| Test ship, our generator | `tests/.../conftest.py::ship_profile` via `HullMeshGenerator`, 7 225 panels | 28 900 | 3.13 | 0.13 | 0.12 | 0.16 | **0.60** | caution |
| Unit sphere (control) | HullProd example | 80 | 4.00 | 0 | 0 | 1.00 | 0 | good |

The sphere control is exact: `K = 1`, `L_ref = 2`, so `I_D = 4`.

### 3.2 Analytical convergence controls

| Control | Resolution | `I_D` | elliptic | saddle | single |
|---|---|---:|---:|---:|---:|
| Wigley L=100, B=10, T=6.25 (`L_ref`=100) | 40x10 | 3.69 | 0.665 | 0.335 | 0 |
| | 80x20 | 4.01 | 0.647 | 0.353 | 0 |
| | 160x40 | 4.16 | 0.636 | 0.364 | 0 |
| | 320x80 | 4.24 | 0.630 | 0.370 | 0 |
| Cylinder D=12, H=26 (OC4 column) | 19x13, 48x26, 120x60 | 0.00 | 0 | 0 | **1.00** |
| Our test ship, uniform x-grid | 400 / 1 600 / 7 225 panels | 2.72 / 3.01 / 3.13 | 0.47 / 0.25 / 0.16 | 0.53 / 0.64 / 0.60 | 0 / 0.07 / 0.12 |
| Our test ship, adaptive x-grid | 400 / 1 600 / 7 208 panels | 2.75 / 3.01 / 3.16 | 0.46 / 0.24 / 0.15 | 0.54 / 0.65 / 0.60 | 0 / 0.07 / 0.14 |

## 4. Findings

1. **Runs on what we already have.** GDF panel meshes from OrcaWave, AQWA and our own generator
   go through unchanged apart from the STL conversion. No CAD licence, no FreeCAD, no gmsh.

2. **The drillship map is physically right.** Parallel midbody is 74 % flat with zero
   curvature density, the bow is 66 % elliptic, the stern is 48 % saddle, and the highest
   `|K| L_ref^2` values (> 500) sit on the moonpool edges at x = -20 to +40 m, z = -7.7 m, and at
   the bilge turns fore and aft. See `results/plots/client_drillship_a_curvature_classes.png`.
   This is the first curvature-resolved picture of a client hull in the ecosystem.

3. **For pontoon-column semi-subs the signature is crease-dominated.** On the OC4 mesh, 53 % of
   vertices lie on a dihedral crease (> 20 deg) and those vertices carry 78 % of the total
   curvature density. A plain cylinder scores exactly `single = 1.00` at any resolution, so the
   OC4 "44 % elliptic" is column-to-pontoon junctions and coarse column faceting, not plate
   curvature. HullProd is not a producibility measure for semi-subs, spars or TLPs built from
   developable primitives. It is still a useful paneling-quality metric for them.

4. **Our own hull generator produces reverse curvature.** The fixture ship is 60 % saddle at
   every resolution and the value does not fall with refinement, so it is geometry, not mesh
   noise. `mesh_generator.py` interpolates between stations with `scipy.interp1d(kind="linear")`
   in x (lines 150, 229, 302). Linear lofting between two different section curves is a ruled
   surface, and a ruled surface has Gaussian curvature `K <= 0` everywhere, so the generator can
   only produce flat, single-curved or saddle plate between stations, never the elliptic bow and
   stern that a faired hull has. The elliptic fraction the table does show falls with
   refinement (0.47 to 0.25 to 0.16) while saddle stays at 0.6, which is the signature of vertex
   creases between bilinear patches, not of synclastic plate. This is the first quantitative
   fairness signal against the generator. Adaptive and uniform x-spacing agree within 1 %, so
   the adaptive grid is not the cause.

5. **Mesh sensitivity is real but first-order convergent.** Wigley `I_D` steps shrink by half
   each refinement (0.32, 0.15, 0.07), extrapolating to about 4.31. A single coarse BEM mesh
   under-reads `I_D` by 10 to 15 %. Comparisons between hulls must therefore be made at matched
   panel density, or on the STEP BRep.

6. **The mesh-quality gate is worth adopting on its own.** HullProd rejected `<FPV-A>.obj`
   for slivers, non-manifold edges and 25 disconnected components. Our `MeshQuality` and
   `quality_gates.py` would not have caught the non-manifold edges or the component count.

7. **Reference-length handling matches our needs.** `--lref` takes any unit and records the
   choice; auto mode uses principal-axis span (about Lpp for monohulls, about beam for the OC4).
   Always pass `lref=length_bp` for catalog hulls so signatures are comparable.

## 5. How it can enhance the hull models and the parametrization work

The value is not the producibility framing but the per-vertex `K`, `H` fields and the
dimensionless, hull-type-independent signature. Concretely:

| Use | Where it plugs in | What it gives |
|---|---|---|
| Fairness regression on generated meshes | `hull_library/mesh_generator.py`, `line_generator/panelizer.py` | `a_C_saddle` and `I_D_minus` trend per hull type; catches interpolation artefacts like finding 4 |
| Variant fingerprint in parametric sweeps | `parametric_hull.py::HullParametricSpace`, `panel_catalog.py`, `rao_database.py` | every L/B/T/D variant carries `[I_D, I_D+, I_D-, a_C]` next to its RAOs, so RAO differences can be attributed to form change vs. pure scaling |
| Hull-type classifier / catalog lookup | `hull_library/lookup.py`, `worldenergydata` `hull_form_mapper.py` | signature separates FPSO (75 % single) from drillship (52 % flat, 23/21 ell/sad) from semi-sub (crease-dominated) without metadata |
| Panel density gate before BEM | `diffraction/quality_gates.py`, `orcawave/panel_mesh.py` | `curvature_reliability` (good/caution/poor) plus non-manifold and component counts |
| Curvature-adaptive refinement target | `mesh_refiner.py`, `coarsen_mesh.py` | refine where `|K| L_ref^2` is high (bow, stern, moonpool, bilge) instead of |d²y/dx²| only |
| Canonical BRep signature for catalog hulls | `visualization/design_tools/freecad_hull.py` STEP export | mesh-independent reference values for the catalog and for regression tests |
| Client-facing hull comparison | `parametric_hull_analysis` charts, `llm-wiki` naval-architecture wiki | drillship vs. drillship "how much of this hull is flat plate" tables from geometry alone |

What it does not do: it will not make `HullParametricSpace` a form generator. True
parametrization (Cb, Cp, LCB, bilge radius, flare, moonpool) still needs a surface model
(offset table or NURBS). HullProd is the objective function and the regression check for that
work, not the generator.

## 6. Proposed follow-ups

Each item needs its own plan and approval per `CLAUDE.md`. Ordered by value per effort.

1. **`hull_library/curvature_screen.py` adapter** (S). `PanelMesh -> trimesh -> hullprod.assess`
   with `lref=length_bp`; returns the signature dataclass and per-vertex `K`, `H` arrays. Add
   `hullprod` as an optional extra (`pip install digitalmodel[curvature]`). Move `gdf_to_stl.py`
   logic into `bemrosetta/mesh/stl_handler.py` or reuse it there.
2. **Signature in catalog and sweeps** (S). Field `curvature_signature` on `HullCatalogEntry`,
   `PanelCatalog` rows and `HullParametricSpace` variants; persisted in the Parquet RAO database.
3. **Fix ruled-surface lofting in `mesh_generator.py`** (M). Replace the three
   `interp1d(kind="linear")` calls in x with the PCHIP/B-spline station interpolation already
   present in `line_generator/hull_surface.py`; prove with `a_C_elliptic` appearing at bow and
   stern of the fixture ship and Wigley reproducing 0.63/0.37.
4. **Fairness gate** (S). In `quality_gates.py`: fail monohulls when `curvature_reliability == poor`,
   warn when `a_C_saddle` exceeds a per-`HullType` threshold seeded from the table above.
5. **BRep route** (M). `freecad_hull.py` STEP -> HullProd native backend for catalog hulls;
   record BRep vs mesh delta in provenance. Needs FreeCAD on the runner (ace-linux-2 candidate).
6. **Semi-sub handling** (M). Split OC4-type meshes into primitives at creases (columns, pontoons,
   braces) and screen per primitive, or restrict HullProd to paneling-quality use for these types.
7. **Wiki and citation** (XS). Add HullProd to `llm-wiki/wikis/naval-architecture` sources and
   cite Serani & Maki 2026 wherever the signature is reported.

## 7. Limitations and cautions

- Mesh results are always `MESH-SENSITIVE`; report the panel density with every value.
- Open boundaries (waterline cut, symmetry plane) are excluded from the valid area; valid area
  fraction was 97.5 % for the drillship, 92.6 % for the FPSO and only 75.7 % for the coarse
  L01 vessel, so coarse meshes lose a real share of their surface to the boundary exclusion.
- The thresholds `h_f = k_f = 1e-4` are numerical, not plate-forming limits; the authors say so.
- Python 3.10 to 3.12 only; the bundled OpenCascade (`cadquery-ocp`) is about 150 MB installed,
  and a full venv with vtk is about 750 MB.
- The signature is scale-invariant only through `L_ref`; always pass it explicitly.
- No license issue: BSD-3-Clause, compatible with the ecosystem.

## 8. Reproduce

See the spike README. All numbers above come from `results/signatures.csv` and the console
output of `convergence_checks.py` on 2026-09-25 with HullProd 1.0.1, trimesh 5.1.0.
