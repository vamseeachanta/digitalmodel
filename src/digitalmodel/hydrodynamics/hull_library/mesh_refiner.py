"""Refine and coarsen hull panel meshes for convergence sensitivity.

ABOUTME: Provides mesh subdivision (refine_mesh), quality metrics, and mesh
family generation for convergence studies in diffraction analysis. Pairs with
coarsen_mesh.py for the coarsening direction.

Algorithm (refine_mesh):
1. For each quad panel [v0, v1, v2, v3], insert edge midpoints and a center.
2. Create 4 sub-quads preserving CCW winding order.
3. Deduplicate vertices via edge midpoint cache + np.unique.
4. Remove degenerate panels (< 3 unique vertices).
5. Repeat for requested number of levels (4^levels panel multiplier).
"""

from __future__ import annotations

import logging
import math
from dataclasses import dataclass
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class MeshQualityMetrics:
    """Quality metrics for a panel mesh."""

    panel_count: int
    vertex_count: int
    min_area: float
    max_area: float
    mean_area: float
    total_area: float
    min_aspect_ratio: float
    max_aspect_ratio: float
    mean_aspect_ratio: float
    degenerate_count: int  # panels with area < 1e-6 m^2


@dataclass
class MeshFamilyMember:
    """A single member of a mesh convergence family."""

    mesh: PanelMesh
    level_name: str  # e.g. "coarse", "medium", "fine", "very_fine"
    factor: float  # density factor relative to source (0.25, 0.5, 1.0, 2.0, 4.0)
    metrics: MeshQualityMetrics


def refine_mesh(mesh: PanelMesh, levels: int = 1) -> PanelMesh:
    """Subdivide each quad panel into 4 sub-quads by inserting midpoints.

    For each quad [v0, v1, v2, v3]:
      - Insert edge midpoints: m01, m12, m23, m30
      - Insert center point: mc (average of 4 vertices)
      - Create 4 sub-quads:
        [v0, m01, mc, m30]
        [m01, v1, m12, mc]
        [mc, m12, v2, m23]
        [m30, mc, m23, v3]

    Apply ``levels`` times for 4^levels panel multiplier.
    Deduplicates vertices after each level.

    Args:
        mesh: Source panel mesh to refine.
        levels: Number of subdivision levels (each level multiplies panels by 4).

    Returns:
        A new PanelMesh with refined geometry.

    Raises:
        ValueError: If *levels* is less than 1.
    """
    if levels < 1:
        raise ValueError(f"levels must be >= 1, got {levels}")

    result = mesh
    for _ in range(levels):
        result = _subdivide_once(result)
    return result


def _subdivide_once(mesh: PanelMesh) -> PanelMesh:
    """Single level of quad subdivision."""
    verts = mesh.vertices.copy()
    new_verts_list: list[np.ndarray] = list(verts)
    new_panels: list[list[int]] = []

    # Cache for edge midpoints to avoid duplicates.
    # Key: (min_idx, max_idx) -> new vertex index
    edge_cache: dict[tuple[int, int], int] = {}

    def get_midpoint(i: int, j: int) -> int:
        """Get or create edge midpoint vertex."""
        key = (min(i, j), max(i, j))
        if key in edge_cache:
            return edge_cache[key]
        mid = (np.asarray(new_verts_list[i]) + np.asarray(new_verts_list[j])) / 2.0
        idx = len(new_verts_list)
        new_verts_list.append(mid)
        edge_cache[key] = idx
        return idx

    for panel in mesh.panels:
        v0, v1, v2, v3 = int(panel[0]), int(panel[1]), int(panel[2]), int(panel[3])

        # Edge midpoints
        m01 = get_midpoint(v0, v1)
        m12 = get_midpoint(v1, v2)
        m23 = get_midpoint(v2, v3)
        m30 = get_midpoint(v3, v0)

        # Center point (average of 4 original vertices)
        center = (
            np.asarray(new_verts_list[v0])
            + np.asarray(new_verts_list[v1])
            + np.asarray(new_verts_list[v2])
            + np.asarray(new_verts_list[v3])
        ) / 4.0
        mc = len(new_verts_list)
        new_verts_list.append(center)

        # 4 sub-quads (preserve CCW winding order)
        new_panels.append([v0, m01, mc, m30])
        new_panels.append([m01, v1, m12, mc])
        new_panels.append([mc, m12, v2, m23])
        new_panels.append([m30, mc, m23, v3])

    vertices = np.array(new_verts_list, dtype=np.float64)
    panels_arr = np.array(new_panels, dtype=np.int32)

    # Deduplicate vertices (round to avoid floating-point duplicates)
    unique_verts, inverse = np.unique(
        np.round(vertices, decimals=10), axis=0, return_inverse=True
    )
    remapped_panels = inverse[panels_arr]

    # Remove degenerate panels (fewer than 3 unique vertices)
    valid: list[np.ndarray] = []
    for p in remapped_panels:
        if len(set(int(x) for x in p)) >= 3:
            valid.append(p)

    valid_panels = (
        np.array(valid, dtype=np.int32)
        if valid
        else np.empty((0, 4), dtype=np.int32)
    )

    return PanelMesh(
        vertices=unique_verts,
        panels=valid_panels,
        normals=None,
        panel_areas=None,
        panel_centers=None,
        name=f"{mesh.name}_refined",
        format_origin=mesh.format_origin,
        symmetry_plane=mesh.symmetry_plane,
        reference_point=list(mesh.reference_point),
        metadata={
            **mesh.metadata,
            "refinement_levels": mesh.metadata.get("refinement_levels", 0) + 1,
        },
    )


def compute_quality_metrics(mesh: PanelMesh) -> MeshQualityMetrics:
    """Compute quality metrics for a panel mesh.

    Args:
        mesh: The panel mesh to analyse.

    Returns:
        MeshQualityMetrics with panel areas, aspect ratios, and degenerate count.
    """
    areas = mesh.panel_areas
    aspect_ratios = _compute_aspect_ratios(mesh)

    return MeshQualityMetrics(
        panel_count=mesh.n_panels,
        vertex_count=mesh.n_vertices,
        min_area=float(np.min(areas)) if len(areas) > 0 else 0.0,
        max_area=float(np.max(areas)) if len(areas) > 0 else 0.0,
        mean_area=float(np.mean(areas)) if len(areas) > 0 else 0.0,
        total_area=float(np.sum(areas)),
        min_aspect_ratio=float(np.min(aspect_ratios)) if len(aspect_ratios) > 0 else 0.0,
        max_aspect_ratio=float(np.max(aspect_ratios)) if len(aspect_ratios) > 0 else 0.0,
        mean_aspect_ratio=float(np.mean(aspect_ratios)) if len(aspect_ratios) > 0 else 0.0,
        degenerate_count=int(np.sum(areas < 1e-6)),
    )


def _compute_aspect_ratios(mesh: PanelMesh) -> np.ndarray:
    """Compute max_edge / min_edge for each panel."""
    ratios = np.zeros(mesh.n_panels, dtype=np.float64)
    for i in range(mesh.n_panels):
        verts = mesh.vertices[mesh.panels[i]]
        edges: list[float] = []
        for j in range(4):
            edge_len = float(np.linalg.norm(verts[(j + 1) % 4] - verts[j]))
            if edge_len > 1e-12:
                edges.append(edge_len)
        if len(edges) >= 2:
            ratios[i] = max(edges) / min(edges)
        else:
            ratios[i] = float("inf")
    return ratios


def generate_mesh_family(
    source_mesh: PanelMesh,
    factors: list[float] | None = None,
) -> list[MeshFamilyMember]:
    """Generate a family of meshes at different density levels.

    Args:
        source_mesh: The base mesh (factor=1.0).
        factors: Density multipliers. Default [0.25, 0.5, 1.0, 2.0, 4.0].
            factor < 1.0 -> coarsen, factor > 1.0 -> refine, factor == 1.0 -> original.

    Returns:
        List of MeshFamilyMember sorted by ascending panel count.
    """
    if factors is None:
        factors = [0.25, 0.5, 1.0, 2.0, 4.0]

    from digitalmodel.hydrodynamics.hull_library.coarsen_mesh import coarsen_mesh

    level_names = {
        0.25: "very_coarse",
        0.5: "coarse",
        1.0: "medium",
        2.0: "fine",
        4.0: "very_fine",
    }

    family: list[MeshFamilyMember] = []
    base_panels = source_mesh.n_panels

    for factor in sorted(factors):
        if abs(factor - 1.0) < 1e-6:
            # Original mesh
            mesh = PanelMesh(
                vertices=source_mesh.vertices.copy(),
                panels=source_mesh.panels.copy(),
                normals=None,
                panel_areas=None,
                panel_centers=None,
                name=f"{source_mesh.name}_{base_panels}p",
                format_origin=source_mesh.format_origin,
                symmetry_plane=source_mesh.symmetry_plane,
                reference_point=list(source_mesh.reference_point),
                metadata={**source_mesh.metadata, "density_factor": 1.0},
            )
        elif factor < 1.0:
            # Coarsen
            target_panels = max(4, int(base_panels * factor))
            mesh = coarsen_mesh(
                source_mesh, target_panels=target_panels, preserve_features=True
            )
            mesh = PanelMesh(
                vertices=mesh.vertices,
                panels=mesh.panels,
                normals=None,
                panel_areas=None,
                panel_centers=None,
                name=f"{source_mesh.name}_{mesh.n_panels}p",
                format_origin=mesh.format_origin,
                symmetry_plane=mesh.symmetry_plane,
                reference_point=list(mesh.reference_point),
                metadata={**mesh.metadata, "density_factor": factor},
            )
        else:
            # Refine: levels = round(log4(factor)), minimum 1
            levels = max(1, round(math.log(factor) / math.log(4)))
            mesh = refine_mesh(source_mesh, levels=levels)
            mesh = PanelMesh(
                vertices=mesh.vertices,
                panels=mesh.panels,
                normals=None,
                panel_areas=None,
                panel_centers=None,
                name=f"{source_mesh.name}_{mesh.n_panels}p",
                format_origin=mesh.format_origin,
                symmetry_plane=mesh.symmetry_plane,
                reference_point=list(mesh.reference_point),
                metadata={**mesh.metadata, "density_factor": factor},
            )

        level_name = level_names.get(factor, f"x{factor:.1f}")
        metrics = compute_quality_metrics(mesh)

        family.append(
            MeshFamilyMember(
                mesh=mesh,
                level_name=level_name,
                factor=factor,
                metrics=metrics,
            )
        )

        logger.info(
            "Mesh family %s: %d panels, area range [%.4f, %.4f], "
            "aspect ratio max %.1f",
            level_name,
            metrics.panel_count,
            metrics.min_area,
            metrics.max_area,
            metrics.max_aspect_ratio,
        )

    return sorted(family, key=lambda m: m.metrics.panel_count)


def export_mesh_family(
    family: list[MeshFamilyMember],
    output_dir: Path,
) -> list[Path]:
    """Export all members of a mesh family as GDF files.

    Args:
        family: List of mesh family members to export.
        output_dir: Directory to write GDF files into.

    Returns:
        List of output file paths.
    """
    from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import GDFHandler

    output_dir.mkdir(parents=True, exist_ok=True)
    handler = GDFHandler()
    paths: list[Path] = []

    for member in family:
        filename = f"{member.mesh.name}.gdf"
        path = output_dir / filename
        handler.write(member.mesh, path)
        paths.append(path)
        logger.info("Exported %s: %s", member.level_name, path)

    return paths


def convergence_summary(family: list[MeshFamilyMember]) -> str:
    """Generate a text summary table of mesh family for convergence studies.

    Args:
        family: List of mesh family members (should be sorted by panel count).

    Returns:
        Markdown-formatted table string.
    """
    lines = [
        "| Level | Factor | Panels | Vertices | Min Area | Max Area "
        "| Mean Area | Total Area | Max AR | Degen |",
        "|-------|--------|--------|----------|----------|----------"
        "|-----------|------------|--------|-------|",
    ]
    for m in family:
        q = m.metrics
        lines.append(
            f"| {m.level_name} | {m.factor:.2f} | {q.panel_count} "
            f"| {q.vertex_count} | {q.min_area:.4f} | {q.max_area:.4f} "
            f"| {q.mean_area:.4f} | {q.total_area:.1f} "
            f"| {q.max_aspect_ratio:.1f} | {q.degenerate_count} |"
        )
    return "\n".join(lines)


__all__ = [
    "MeshQualityMetrics",
    "MeshFamilyMember",
    "refine_mesh",
    "compute_quality_metrics",
    "generate_mesh_family",
    "export_mesh_family",
    "convergence_summary",
]
