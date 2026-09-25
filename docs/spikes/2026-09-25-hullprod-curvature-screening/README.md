# Spike: HullProd curvature screening on ecosystem hull meshes

**Date:** 2026-09-25 | **Issue:** [#2170](https://github.com/vamseeachanta/digitalmodel/issues/2170) |
**Evaluation note:** [`docs/domains/hull_library/hullprod-curvature-screening-evaluation.md`](../../domains/hull_library/hullprod-curvature-screening-evaluation.md)

HullProd (Serani & Maki, CNR-INM, BSD-3-Clause) computes a Gaussian/mean-curvature signature of a hull
surface: `I_D`, `I_D_plus`, `I_D_minus` and area fractions flat / single / elliptic / saddle.
This spike runs it on our own GDF, OBJ and generator-produced meshes and on analytical controls.

## Reproduce

```bash
uv venv .hp -p 3.12 && .hp/Scripts/activate      # HullProd needs 3.10-3.12; digitalmodel venv is 3.11 and also works
pip install hullprod                              # pulls trimesh, cadquery-ocp (OpenCascade), matplotlib, vtk

# GDF panel meshes -> STL (symmetry mirrored)
python gdf_to_stl.py "../../domains/orcawave/examples/L02 OC4 Semi-sub/L02 OC4 Semi-sub mesh.gdf" work/L02_oc4_semisub.stl

# Signature table for any set of STL/OBJ/PLY meshes
python screen_hulls.py work work/*.stl

# Analytical controls + hull_library test ship at 3 resolutions (needs digitalmodel importable)
python convergence_checks.py work
```

Or the CLI directly: `hullprod work/L02_oc4_semisub.stl --out work/out_oc4 --lref 73.9`.

## Files

| Path | Role |
|---|---|
| `gdf_to_stl.py` | WAMIT low-order GDF → triangulated STL with ISX/ISY mirroring |
| `screen_hulls.py` | batch `hullprod.assess` → `signatures.csv` |
| `convergence_checks.py` | Wigley, cylinder and `HullMeshGenerator` test ship at several resolutions |
| `results/signatures.csv` | signature table for the 7 ecosystem hulls + sphere control (HullProd 1.0.1) |
| `results/client_drillship_a_signature.csv` | HullProd's own signature file for the drillship |
| `results/plots/*_curvature_classes.png` | curvature-class maps (drillship, OC4 semi-sub, FPSO, our test ship) |
| `results/plots/client_drillship_a_developability_density.png` | `|K| L_ref^2` density map |

Input hulls (not copied here): `docs/domains/orcawave/examples/L01_default_vessel`, `.../L02 OC4 Semi-sub`,
`llm-wiki-acma/cases/orcawave-diffraction-{fpso,hlv-a,<DS-A>}/*.gdf`,
`worldenergydata/data/modules/vessel_hull_models/hulls/sea_cypress.obj`,
and `tests/hydrodynamics/hull_library/conftest.py::ship_profile` through `HullMeshGenerator`.
