# sectionproperties — Composite Section Evaluation

**Issue:** vamseeachanta/workspace-hub#1499
**Date:** 2026-03-29
**Version tested:** 3.10.2
**Prerequisite:** #1452 (homogeneous steel PoC)

## Overview

Extension of the sectionproperties evaluation to composite (multi-material) sections using `CompoundGeometry` and `Material` objects. Three offshore-relevant cases were tested: concrete-filled tubular, grouted pile-sleeve connection, and steel-concrete composite beam.

## Key API Notes

- **Material definition:** `Material(name, elastic_modulus, poissons_ratio, yield_strength, density, color)` — color is used in mesh plots to distinguish materials.
- **CompoundGeometry:** Created from a list of `Geometry` objects, each with its own `Material`. The `+` operator also works for combining two geometries.
- **Effective properties:** When materials are assigned, use `get_ea()`, `get_eic(e_ref)`, `get_ej(e_ref)` — the `e_ref` parameter sets the reference modulus for transformed-section results.
- **Mesh sizes:** `create_mesh(mesh_sizes=[...])` accepts a list matching the number of sub-geometries in the compound.

## Cases Tested

### Case A: Concrete-Filled Tubular (CFT)

Steel CHS OD=508 mm, WT=25.4 mm (20" x 1") filled with concrete (E=30,000 MPa).

| Property | Computed | Analytical | Diff |
|----------|----------|------------|------|
| A_total (mm^2) | 202,358 | 202,683 | 0.16% |
| EA (N) | 12.607e9 | 12.627e9 | 0.16% |
| EI_xx eff (mm^4) | 1.441e9 | 1.446e9 | 0.32% |
| EJ eff (mm^4) | 2.883e9 | 2.892e9 | 0.32% |

All within 0.35% of closed-form solutions. Differences due to polygon approximation of circles (n=64 segments).

### Case B: Grouted Pile-Sleeve Connection

Three concentric annuli: steel sleeve (OD=1200, WT=30), grout annulus (OD=1140, ID=1000, E=20,000 MPa), steel pile (OD=1000, WT=25).

| Property | Computed | Analytical | Diff |
|----------|----------|------------|------|
| A_total (mm^2) | 421,474 | 422,152 | 0.16% |
| EA (N) | 42.008e9 | 42.075e9 | 0.16% |
| EI_xx eff (mm^4) | 31.268e9 | 31.368e9 | 0.32% |
| EJ eff (mm^4) | 62.526e9 | 62.737e9 | 0.34% |

All within 0.35%. The three-material concentric geometry worked without issues.

### Case C: Steel-Concrete Composite Beam

W14x90 steel beam (d=356, b=368, tf=18, tw=11.2, r=25.4) with 150 mm x 1500 mm concrete slab on top flange.

| Property | Computed | Analytical (approx) | Diff |
|----------|----------|---------------------|------|
| EI_xx eff (mm^4) | 1.220e9 | 1.203e9 | 1.46% |

Analytical uses simplified W-shape without fillets; FEM includes fillet radii, accounting for the difference.

## API Observations

### Ergonomics (positive)

1. **Clean material composition** — assigning `material=` to each geometry, then combining via `CompoundGeometry([...])` is intuitive.
2. **Consistent effective methods** — `get_eic(e_ref=steel)` returns transformed-section properties directly; no manual modular-ratio arithmetic needed.
3. **Mesh visualization** — `plot_mesh(materials=True)` color-codes by material, immediately verifiable.
4. **Geometric operations** — `shift_section()` makes positioning composite components straightforward.

### Gotchas

1. **Effective vs geometric methods** — Using `get_ic()` on a composite section returns geometric (unweighted) properties. Must use `get_eic()` for meaningful composite results. Easy to mix up.
2. **e_ref parameter** — Defaults to 1.0 (returns raw E*I products in N-mm^2). Must explicitly pass the reference material to get transformed mm^4 values.
3. **Mesh size list** — Must match the number of sub-geometries in the CompoundGeometry, or pass a single value. Mismatch raises an error.
4. **Polygon approximation** — Circular sections are approximated by n-sided polygons. Use n>=64 for engineering accuracy (<0.5% error).
5. **numpy compatibility** — System matplotlib compiled against numpy 1.x will crash with numpy 2.x. Use a venv with compatible versions.

## Recommendations for Offshore Use

1. **CFT members** — sectionproperties can directly compute effective stiffness for concrete-filled tubulars per API RP 2A / DNVGL-ST-0126. The `CompoundGeometry` approach handles the transformed-section method automatically.
2. **Grouted connections** — Pile-sleeve-grout assemblies are correctly handled as three-material compound sections. Results suitable for global frame analysis stiffness input.
3. **Composite beams** — Topsides composite deck beams (steel + concrete slab) modeled correctly. The `shift_section()` method handles slab positioning.
4. **Integration path** — Wrap as a utility function accepting OD/WT/material parameters, returning EA/EI/EJ for direct use in frame analysis input generation.

## Files

- PoC script: `scripts/integrations/sectionproperties_composite_poc.py`
- Mesh plots: `scripts/integrations/output/sectionproperties_composite_meshes.png`
- Results JSON: `scripts/integrations/output/sectionproperties_composite_results.json`
