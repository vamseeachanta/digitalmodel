# Hull Scaling Guide

How to scale hull panel meshes for different vessel sizes using the `hull_library` API.

## Overview

The hull library stores reference hull profiles at their original (as-designed) dimensions.
When you need a mesh for a vessel that shares the same form but differs in size, you
have two options: **geometric (uniform) scaling** and **parametric (non-uniform) scaling**.
Both operate on the vertex coordinates of the generated `PanelMesh`.

## Geometric (Uniform) Scaling

Multiply every vertex coordinate by a single scale factor. This preserves the hull
shape exactly -- all dimensions grow or shrink at the same rate.

```
v_new = v_original * scale_factor
```

- Panel count stays the same.
- Panel sizes scale proportionally with geometry.
- Aspect ratios are preserved, so mesh quality is unchanged.
- Best suited when the target vessel is a proportionally larger or smaller copy
  of the reference hull (e.g. scaling a barge from 100 m to 120 m while keeping
  beam-to-length and draft-to-length ratios constant).

### Example

```python
import numpy as np
from digitalmodel.hydrodynamics.hull_library import (
    HullCatalog,
    MeshGeneratorConfig,
)

catalog = HullCatalog(profiles_dir=Path("data/hull_profiles"))
mesh = catalog.generate_mesh("generic_tanker")

scale_factor = 1.25  # 25 % larger in all dimensions
mesh.vertices = mesh.vertices * scale_factor
```

## Parametric (Non-Uniform) Scaling

Scale length (L), beam (B), and draft (T) independently by applying axis-specific
factors. This is the approach encoded in `HullVariation.scale_factors`.

```
x_new = x * (L_target / L_source)
y_new = y * (B_target / B_source)
z_new = z * (T_target / T_source)
```

- Allows you to match a target vessel whose proportions differ from the reference.
- Panel count stays the same, but panel shapes change because each axis scales
  differently.

### Example

```python
import numpy as np
from digitalmodel.hydrodynamics.hull_library import (
    HullCatalog,
    HullVariation,
    MeshGeneratorConfig,
)

catalog = HullCatalog(profiles_dir=Path("data/hull_profiles"))
entry = catalog.get_hull("generic_tanker")
profile = entry.profile

# Define a variation for a wider, shallower vessel
variation = HullVariation(
    variation_id="wide_shallow",
    scale_factors={"length": 1.0, "beam": 1.3, "draft": 0.85},
)

mesh = catalog.generate_mesh("generic_tanker")

sx = variation.scale_factors["length"]
sy = variation.scale_factors["beam"]
sz = variation.scale_factors["draft"]

mesh.vertices[:, 0] *= sx  # x -- longitudinal
mesh.vertices[:, 1] *= sy  # y -- transverse (half-breadth)
mesh.vertices[:, 2] *= sz  # z -- vertical (draft)
```

## Re-panelization Notes

Non-uniform scaling distorts panel aspect ratios. A panel that was square on the
reference hull can become elongated after a large beam increase, which reduces
accuracy in boundary-element solvers (WAMIT, NEMOH, etc.).

Guidelines:

- **Moderate factors (0.8x -- 1.5x per axis)**: the original panel layout is
  generally acceptable. Spot-check the largest aspect ratio.
- **Large factors (> 2x on any axis)**: re-mesh for solver accuracy. Generate a
  fresh mesh from an adjusted `HullProfile` rather than stretching vertices.
- **Mixed extremes** (e.g. 0.5x draft with 2x beam): always re-mesh. The waterline
  panel grading from the original mesh will be poorly distributed.

To re-mesh cleanly, create a new `HullProfile` with the target dimensions and feed
it to `HullMeshGenerator`:

```python
from digitalmodel.hydrodynamics.hull_library import (
    HullProfile,
    HullMeshGenerator,
    MeshGeneratorConfig,
)

# Build a new profile with adjusted principal dimensions and stations
scaled_profile = HullProfile(
    name="tanker_wide_shallow",
    hull_type=original_profile.hull_type,
    stations=scaled_stations,       # stations with adjusted offsets
    length_bp=target_length,
    beam=target_beam,
    draft=target_draft,
    depth=target_depth,
    source=f"Scaled from {original_profile.name}",
)

generator = HullMeshGenerator()
config = MeshGeneratorConfig(target_panels=2000, waterline_refinement=2.5)
mesh = generator.generate(scaled_profile, config)
```

Setting `adaptive_density=True` in `MeshGeneratorConfig` concentrates panels at
high-curvature regions (bow/stern), which is especially useful after non-uniform
scaling distorts those areas.

## Workflow Summary

1. **Pick a hull** from the catalog via `HullCatalog.get_hull(hull_id)`.
2. **Decide the scaling approach**:
   - Uniform: same proportions, different size.
   - Non-uniform: different L, B, T ratios (use `HullVariation.scale_factors`).
3. **Apply scaling** to the mesh vertices, or re-generate a mesh from an adjusted
   profile when scale factors are large.
4. **Validate mesh quality**: check panel aspect ratios and total panel count.
   Solvers typically want aspect ratios below 3:1 and at least 1000 panels for
   converged added-mass and damping coefficients.
5. **Export** the mesh in the required solver format via the panel catalog or
   BEMRosetta export utilities.
