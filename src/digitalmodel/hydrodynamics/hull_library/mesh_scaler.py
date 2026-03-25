"""Scale hull panel meshes to target principal dimensions.

ABOUTME: Provides uniform, parametric, and target-dimension scaling for hull
panel meshes used in diffraction analysis. Includes validation of scaled mesh
quality (degenerate panels, aspect ratios, normal consistency) and GDF export.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class ScaleDimensions:
    """Principal dimensions for scaling."""

    length_m: float
    beam_m: float
    draft_m: float

    def __post_init__(self) -> None:
        if self.length_m <= 0:
            raise ValueError(f"length_m must be positive, got {self.length_m}")
        if self.beam_m <= 0:
            raise ValueError(f"beam_m must be positive, got {self.beam_m}")
        if self.draft_m <= 0:
            raise ValueError(f"draft_m must be positive, got {self.draft_m}")


@dataclass(frozen=True)
class ScaleResult:
    """Result of a scaling operation with validation metrics."""

    mesh: PanelMesh
    scale_factors: tuple[float, float, float]
    source_dims: ScaleDimensions
    target_dims: ScaleDimensions
    validation: dict


def scale_mesh_uniform(mesh: PanelMesh, factor: float) -> PanelMesh:
    """Scale mesh uniformly by a single factor.

    Preserves aspect ratios. All XYZ coordinates multiplied by *factor*.
    Panel topology is unchanged; normals, areas, and centers are recomputed.

    Args:
        mesh: Source panel mesh.
        factor: Uniform scale factor (must be positive).

    Returns:
        New PanelMesh with scaled geometry.

    Raises:
        ValueError: If *factor* is not positive.
    """
    if factor <= 0:
        raise ValueError(f"Uniform scale factor must be positive, got {factor}")

    scaled_vertices = mesh.vertices * factor
    ref = [r * factor for r in mesh.reference_point]

    return PanelMesh(
        vertices=scaled_vertices,
        panels=mesh.panels.copy(),
        name=f"{mesh.name}_scaled_{factor:.2f}x",
        format_origin=mesh.format_origin,
        symmetry_plane=mesh.symmetry_plane,
        reference_point=ref,
        metadata={**mesh.metadata, "scale_factor": factor, "scale_type": "uniform"},
    )


def scale_mesh_parametric(
    mesh: PanelMesh, sx: float, sy: float, sz: float
) -> PanelMesh:
    """Scale mesh with independent X/Y/Z factors.

    *sx* scales length (X), *sy* scales beam (Y), *sz* scales draft (Z).
    Panel topology is preserved; normals are recomputed from the new vertex
    positions.

    Args:
        mesh: Source panel mesh.
        sx: Scale factor for the X (longitudinal) axis.
        sy: Scale factor for the Y (transverse) axis.
        sz: Scale factor for the Z (vertical) axis.

    Returns:
        New PanelMesh with parametrically scaled geometry.

    Raises:
        ValueError: If any scale factor is not positive.
    """
    for label, val in [("sx", sx), ("sy", sy), ("sz", sz)]:
        if val <= 0:
            raise ValueError(f"Scale factor {label} must be positive, got {val}")

    scale_array = np.array([sx, sy, sz], dtype=np.float64)
    scaled_vertices = mesh.vertices * scale_array
    ref = [
        mesh.reference_point[0] * sx,
        mesh.reference_point[1] * sy,
        mesh.reference_point[2] * sz,
    ]

    return PanelMesh(
        vertices=scaled_vertices,
        panels=mesh.panels.copy(),
        name=f"{mesh.name}_scaled_{sx:.2f}x{sy:.2f}x{sz:.2f}",
        format_origin=mesh.format_origin,
        symmetry_plane=mesh.symmetry_plane,
        reference_point=ref,
        metadata={
            **mesh.metadata,
            "scale_factors": (sx, sy, sz),
            "scale_type": "parametric",
        },
    )


def scale_mesh_to_target(
    mesh: PanelMesh,
    source_dims: ScaleDimensions,
    target_dims: ScaleDimensions,
) -> ScaleResult:
    """Scale mesh from source dimensions to target dimensions.

    Computes independent X/Y/Z scale factors from the ratio of target to
    source principal dimensions and applies parametric scaling.

    Args:
        mesh: Source panel mesh.
        source_dims: Current principal dimensions of the mesh.
        target_dims: Desired principal dimensions after scaling.

    Returns:
        ScaleResult containing the scaled mesh, applied factors, and
        validation metrics.
    """
    sx = target_dims.length_m / source_dims.length_m
    sy = target_dims.beam_m / source_dims.beam_m
    sz = target_dims.draft_m / source_dims.draft_m

    scaled = scale_mesh_parametric(mesh, sx, sy, sz)
    validation = validate_scaled_mesh(scaled, target_dims)

    logger.info(
        "Scaled mesh '%s': factors (%.3f, %.3f, %.3f), bbox_ok=%s",
        mesh.name,
        sx,
        sy,
        sz,
        validation["bounding_box_ok"],
    )

    return ScaleResult(
        mesh=scaled,
        scale_factors=(sx, sy, sz),
        source_dims=source_dims,
        target_dims=target_dims,
        validation=validation,
    )


def validate_scaled_mesh(mesh: PanelMesh, target_dims: ScaleDimensions) -> dict:
    """Validate a scaled mesh against target dimensions.

    Checks performed:

    * **bounding_box_ok** -- actual dimensions within 1% of target.
    * **no_degenerate_panels** -- all panel areas exceed 1e-4 m^2.
    * **normals_consistent** -- all normals point outward (positive dot
      product of normal with vector from mesh centroid to panel center).
    * **max_aspect_ratio** -- worst panel aspect ratio.
    * **high_aspect_panels** -- count of panels with aspect ratio > 10.

    Args:
        mesh: The scaled panel mesh to validate.
        target_dims: The intended principal dimensions.

    Returns:
        Dict with validation results.
    """
    bb_min, bb_max = mesh.bounding_box

    actual_length = float(bb_max[0] - bb_min[0])
    actual_draft = float(abs(bb_min[2]))

    # Beam: if symmetric about Y, actual beam = 2 * max_y
    if mesh.symmetry_plane and "y" in (mesh.symmetry_plane or "").lower():
        actual_beam = 2.0 * float(bb_max[1])
    else:
        actual_beam = float(bb_max[1] - bb_min[1])

    tol = 0.01
    length_ok = (
        abs(actual_length - target_dims.length_m) / target_dims.length_m < tol
        if target_dims.length_m > 0
        else True
    )
    beam_ok = (
        abs(actual_beam - target_dims.beam_m) / target_dims.beam_m < tol
        if target_dims.beam_m > 0
        else True
    )
    draft_ok = (
        abs(actual_draft - target_dims.draft_m) / target_dims.draft_m < tol
        if target_dims.draft_m > 0
        else True
    )

    # Degenerate panels
    min_area = 1e-4
    areas = mesh.panel_areas
    no_degenerate = bool(np.all(areas > min_area))
    degenerate_count = int(np.sum(areas <= min_area))

    # Panel aspect ratios
    aspect_ratios = _compute_aspect_ratios(mesh)
    max_aspect = float(np.max(aspect_ratios)) if len(aspect_ratios) > 0 else 0.0
    high_aspect = int(np.sum(aspect_ratios > 10.0))

    # Normal consistency: dot(normal, center - centroid) should be > 0
    centroid = np.mean(mesh.panel_centers, axis=0)
    radial = mesh.panel_centers - centroid
    dots = np.sum(mesh.normals * radial, axis=1)
    normals_ok = bool(np.all(dots > -1e-6))

    return {
        "bounding_box_ok": length_ok and beam_ok and draft_ok,
        "actual_dims": {
            "length_m": actual_length,
            "beam_m": actual_beam,
            "draft_m": actual_draft,
        },
        "no_degenerate_panels": no_degenerate,
        "degenerate_count": degenerate_count,
        "normals_consistent": normals_ok,
        "max_aspect_ratio": max_aspect,
        "high_aspect_panels": high_aspect,
    }


def _compute_aspect_ratios(mesh: PanelMesh) -> np.ndarray:
    """Compute aspect ratio for each panel (max_edge / min_edge).

    For quad panels the four edge lengths are measured; for triangles
    (indicated by a repeated vertex index) only three edges are used.
    """
    n = mesh.n_panels
    ratios = np.zeros(n, dtype=np.float64)
    n_verts_per_panel = mesh.panels.shape[1]

    for i in range(n):
        panel = mesh.panels[i]
        verts = mesh.vertices[panel]
        edges = []
        for j in range(n_verts_per_panel):
            edge_len = float(
                np.linalg.norm(verts[(j + 1) % n_verts_per_panel] - verts[j])
            )
            if edge_len > 1e-12:
                edges.append(edge_len)
        if len(edges) >= 2:
            ratios[i] = max(edges) / min(edges)
        else:
            ratios[i] = float("inf")

    return ratios


def export_scaled_gdf(mesh: PanelMesh, output_path: Path) -> Path:
    """Export a scaled mesh as a GDF file.

    Uses :class:`GDFHandler` to write WAMIT-compatible output.

    Args:
        mesh: The panel mesh to export.
        output_path: Destination file path.

    Returns:
        The path to the written file.
    """
    from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import GDFHandler

    handler = GDFHandler()
    result = handler.write(mesh, output_path)
    logger.info("Exported scaled GDF: %s (%d panels)", result, mesh.n_panels)
    return result


__all__ = [
    "ScaleDimensions",
    "ScaleResult",
    "scale_mesh_uniform",
    "scale_mesh_parametric",
    "scale_mesh_to_target",
    "validate_scaled_mesh",
    "export_scaled_gdf",
]
