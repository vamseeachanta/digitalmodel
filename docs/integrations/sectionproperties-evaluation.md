# sectionproperties — Integration Evaluation

**Issue:** vamseeachanta/workspace-hub#1452
**Date:** 2026-03-29
**Version tested:** 3.10.2
**License:** MIT

## Overview

[sectionproperties](https://github.com/robbievanleeuwen/section-properties) computes geometric and warping properties of arbitrary cross-sections using the finite element method (FEM). It supports standard steel sections (I, CHS, RHS, angles, channels, tees) and arbitrary polygonal geometries.

## Installation

```bash
pip install sectionproperties
```

**Dependencies:** numpy, scipy, shapely, matplotlib, cytriangle, ipywidgets
**Note:** Requires numpy<2 if using system-installed matplotlib built against numpy 1.x.

## Capabilities Tested

### 1. AISC W14x90 (W360x134)

Defined via `i_section(d, b, t_f, t_w, r, n_r)` with dimensions converted from AISC manual.

| Property | Computed | AISC Ref | Diff |
|----------|----------|----------|------|
| A (mm^2) | 17,390 | 17,097 | 1.7% |
| Ix (mm^4) | 422.6e6 | 415.8e6 | 1.6% |
| Iy (mm^4) | 149.6e6 | 150.7e6 | 0.7% |
| Cw (mm^6) | 4.21e12 | 4.30e12 | 2.0% |
| Zx (mm^3) | 2.37e6 | 2.57e6 | 7.7% |
| Sx (mm^3) | 2.61e6 | 2.34e6 | 11.4% |
| J (mm^4) | 1.91e6 | 1.69e6 | 12.8% |

**Notes:**
- Area, Ix, Iy, Cw match within 2% — excellent for geometric properties.
- Zx, Sx, J deviations are due to approximate fillet radius (used r=25.4mm vs actual AISC k_det-derived ~21.7mm). The fillet region significantly affects torsion constant J and the plastic/elastic section moduli. Using exact AISC dimensions would improve agreement.

### 2. Offshore Tubular CHS (914.4 x 25.4 mm)

Defined via `circular_hollow_section(d, t, n)` — typical 36" x 1" offshore tubular.

| Property | Computed | Analytical | Diff |
|----------|----------|------------|------|
| A (mm^2) | 70,825 | 70,939 | 0.16% |
| Ix (mm^4) | 6.99e9 | 7.01e9 | 0.32% |
| J (mm^4) | 13.98e9 | 14.03e9 | 0.35% |

All CHS properties match analytical closed-form solutions within 0.4%.

## API Summary

```python
from sectionproperties.pre.library import i_section, circular_hollow_section
from sectionproperties.analysis import Section

# Define geometry
geom = i_section(d=356, b=368, t_f=18, t_w=11.2, r=25.4, n_r=16)

# Mesh and analyse
geom.create_mesh(mesh_sizes=[10])
sec = Section(geom)
sec.calculate_geometric_properties()   # A, I, centroid
sec.calculate_warping_properties()     # J, Cw
sec.calculate_plastic_properties()     # Z (plastic), S (elastic)

# Extract
sec.get_area()     # A
sec.get_ic()       # (Ixx_c, Iyy_c, Ixy_c)
sec.get_z()        # (Sxx+, Sxx-, Syy+, Syy-)  — plastic (AISC "Z")
sec.get_s()        # (Zxx+, Zxx-)               — elastic (AISC "S")
sec.get_j()        # J (torsion)
sec.get_gamma()    # Cw (warping)
```

**Key API note:** When a `Material` is assigned, use `get_eic()`, `get_ej()`, `get_egamma()` etc. (effective/composite methods). Without material, use `get_ic()`, `get_j()`, `get_gamma()`.

## Standard Section Library

Built-in parametric generators:
- **Steel:** `i_section`, `channel_section`, `angle_section`, `tee_section`, `circular_hollow_section`, `rectangular_hollow_section`, `tapered_flange_i_section`, `mono_i_section`, `zed_section`
- **Concrete:** `concrete_rectangular_section`, `concrete_circular_section`, `concrete_tee_section`, `concrete_column_section`
- **Bridge:** `super_t_girder_section`, `i_girder_section`, `box_girder_section`
- **Primitives:** `rectangular_section`, `circular_section`, `triangular_section`
- **Nastran:** Full PBEAM/PBAR section catalog

No built-in AISC database lookup (e.g., "W14x90" by name). Users must supply dimensions. Could integrate with `steeldesign` or `aisc-shapes` for name-to-dimension mapping.

## Code Compliance

- **AISC:** No direct code checks, but computes all properties needed for AISC 360 checks (A, Ix, Iy, Sx, Zx, J, Cw, r)
- **Eurocode:** Supported via analysis methods
- **AS (Australian):** Supported via analysis methods

## Assessment

| Criterion | Rating | Notes |
|-----------|--------|-------|
| Accuracy | High | <2% for geometric props, <0.4% for CHS |
| API quality | Good | Clean, well-documented, Pythonic |
| Section library | Extensive | All common structural shapes |
| Arbitrary sections | Yes | Any polygon via Shapely geometry |
| Plot/visualization | Good | Mesh, geometry, stress plots |
| Performance | Good | W14x90 analysis in ~2s with fine mesh |
| Maintenance | Active | Regular releases, 40+ contributors |
| Integration fit | Very high | Pure Python, MIT, pip-installable |

## Recommendation

**Adopt for digitalmodel.** Suitable for:
1. Cross-section property computation for structural members
2. Validation of hand calculations and spreadsheet tools
3. Parametric studies (varying wall thickness, member sizing)
4. Custom section analysis (built-up sections, openings)

## AISC Shapes Database Lookup

**Issue:** vamseeachanta/workspace-hub#1497
**Date:** 2026-03-29

Since sectionproperties has no built-in AISC shape lookup (users must supply dimensions), we added a lookup layer backed by a YAML catalog.

### Catalog

`data/aisc_shapes.yaml` — 10 W-shapes from AISC Shapes Database v15.0, including:
- Dimensions: d, bf, tf, tw, k_des, k_det
- Reference properties: A, Ix, Iy, Sx, Zx, J, Cw
- Shapes: W6x15, W8x31, W10x49, W12x65, W14x22, W14x48, W14x90, W21x44, W24x176, W36x150

### Lookup Module

`scripts/integrations/aisc_section_lookup.py` provides:

```python
from aisc_section_lookup import get_aisc_geometry, list_shapes, validate_shape

# Get a sectionproperties Geometry object by AISC designation
geom = get_aisc_geometry("W14x90", unit="imperial")  # or "metric"

# List available shapes
list_shapes()              # all shapes
list_shapes(series="W14")  # W14 series only

# Validate against AISC reference values
validate_shape("W14x90", verbose=True)
```

Key design decisions:
- Fillet radius computed from k_des (design k): `r = k_des - tf`
- sectionproperties API note: `get_z()` returns elastic (AISC S), `get_s()` returns plastic (AISC Z) — opposite to AISC naming convention

### Validation Results

All 10 shapes validated against AISC manual reference values. Every property (A, Ix, Iy, Sx, Zx, J, Cw) within 1.4%:

| Shape | A | Ix | Iy | Sx | Zx | J | Cw |
|-------|---|----|----|----|----|---|------|
| W14x90 | 0.13% | 0.29% | 0.11% | 0.49% | 0.43% | 0.32% | 0.65% |
| W14x48 | 0.35% | 0.27% | 0.04% | 0.19% | 0.12% | 0.80% | 1.03% |
| W14x22 | 0.07% | 0.63% | 0.01% | 0.46% | 0.49% | 0.17% | 1.39% |
| W12x65 | 0.14% | 0.43% | 0.19% | 0.21% | 0.16% | 0.25% | 1.24% |
| W10x49 | 0.24% | 0.62% | 0.02% | 0.26% | 0.17% | 0.57% | 0.39% |
| W8x31 | 0.04% | 0.07% | 0.11% | 0.07% | 0.05% | 0.69% | 0.66% |
| W6x15 | 0.07% | 0.03% | 0.01% | 0.01% | 0.10% | 0.92% | 0.86% |
| W24x176 | 0.04% | 0.24% | 0.27% | 0.07% | 0.21% | 0.03% | 0.66% |
| W36x150 | 0.02% | 0.48% | 0.57% | 0.41% | 0.41% | 0.74% | 0.20% |
| W21x44 | 0.03% | 0.46% | 0.06% | 0.28% | 0.26% | 0.49% | 0.45% |

The improvement from the PoC (which showed 7-13% errors on Sx, Zx, J) is due to:
1. Using k_des (design fillet) instead of approximate k_det for fillet radius
2. Correcting the Sx/Zx mapping from sectionproperties API

## Files

- PoC script: `scripts/integrations/sectionproperties_poc.py`
- Mesh plot: `scripts/integrations/output/sectionproperties_poc_meshes.png`
- Results JSON: `scripts/integrations/output/sectionproperties_poc_results.json`
- AISC catalog: `data/aisc_shapes.yaml`
- Lookup module: `scripts/integrations/aisc_section_lookup.py`
- Validation script: `scripts/integrations/aisc_validation.py`
