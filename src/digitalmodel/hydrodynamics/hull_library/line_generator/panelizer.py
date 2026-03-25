"""
ABOUTME: Hull surface paneliser — Phase 3 of the hull panel generator.

Converts a dense HullSurface grid into a PanelMesh suitable for diffraction
analysis solvers.  Responsibilities:
  - Build starboard side quad panels from the y_grid surface
  - Add flat bottom panels along the keel centreline
  - Mirror to full hull when symmetry=False
  - Remove degenerate panels (zero-area, collapsed edges)
  - Orient panel normals outward from the hull interior
  - Compute mesh quality metrics (aspect ratio, minimum angle)

Coordinate convention (marine, as used by BEMRosetta/WAMIT):
  x — longitudinal (AP=0)
  y — transverse (starboard positive)
  z — vertical, z=0 at waterline, z=-draft at keel

The paneliser converts the surface from keel-up z (0..draft) to marine z
(-draft..0) during vertex construction.

Mesh quality thresholds (WRK-106 acceptance criteria):
  max_aspect_ratio < 5
  min_angle_deg    > 15 degrees
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import TYPE_CHECKING

import numpy as np
from numpy.typing import NDArray
from pydantic import BaseModel, Field

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)

if TYPE_CHECKING:
    from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
        HullSurface,
    )
    from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
        HullLineDefinition,
    )


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------


class PanelizerConfig(BaseModel):
    """Configuration for hull panelisation.

    Attributes:
        target_panels: Approximate desired panel count (ignored when
            HullSurface grid size is used directly).
        waterline_refinement: Z-grid refinement multiplier (passed to
            HullSurfaceConfig if building the surface internally).
        symmetry: If True, use y-symmetry plane (BEMRosetta convention —
            only starboard panels are written); if False, include both sides.
        n_y_bottom: Number of transverse subdivisions for flat-bottom panels.
            Increasing this value improves the aspect ratio of bottom panels
            for wide, shallow hulls.  Defaults to 5.
    """

    target_panels: int = Field(default=500, gt=0)
    waterline_refinement: float = Field(default=1.5, gt=0)
    symmetry: bool = True
    n_y_bottom: int = Field(default=5, ge=1)


# ---------------------------------------------------------------------------
# Quality metrics
# ---------------------------------------------------------------------------


@dataclass
class MeshQuality:
    """Quality metrics for a panel mesh.

    Attributes:
        n_panels: Total panel count.
        max_aspect_ratio: Maximum edge-length ratio across all panels.
        min_angle_deg: Minimum interior angle across all panels in degrees.
        mean_area: Mean panel area.
        area_variance: Variance of panel areas.
    """

    n_panels: int
    max_aspect_ratio: float
    min_angle_deg: float
    mean_area: float
    area_variance: float


def compute_mesh_quality(mesh: PanelMesh) -> MeshQuality:
    """Compute mesh quality metrics for a PanelMesh.

    Iterates over all panels to compute per-panel aspect ratio and minimum
    interior angle, then reports the worst-case values.

    Args:
        mesh: PanelMesh with vertices and panels.

    Returns:
        MeshQuality dataclass with worst-case metric values.
    """
    max_ar = 0.0
    min_ang = 180.0

    for panel_idx in range(mesh.n_panels):
        panel = mesh.panels[panel_idx]
        unique_indices = list(dict.fromkeys(panel.tolist()))  # preserve order
        verts = mesh.vertices[unique_indices]
        n_v = len(verts)
        if n_v < 3:
            continue

        # Compute edge lengths
        edges = []
        for k in range(n_v):
            e = verts[(k + 1) % n_v] - verts[k]
            edges.append(np.linalg.norm(e))

        e_arr = np.array(edges)
        e_max = np.max(e_arr)
        e_min = np.min(e_arr[e_arr > 1e-12]) if np.any(e_arr > 1e-12) else 1e-12
        ar = e_max / max(e_min, 1e-12)
        max_ar = max(max_ar, ar)

        # Minimum interior angle via dot product of consecutive edges
        for k in range(n_v):
            v0 = verts[k]
            v1 = verts[(k + 1) % n_v]
            v2 = verts[(k + 2) % n_v]
            a = v0 - v1
            b = v2 - v1
            len_a = np.linalg.norm(a)
            len_b = np.linalg.norm(b)
            if len_a < 1e-12 or len_b < 1e-12:
                continue
            cos_ang = np.clip(np.dot(a, b) / (len_a * len_b), -1.0, 1.0)
            ang_deg = math.degrees(math.acos(cos_ang))
            min_ang = min(min_ang, ang_deg)

    areas = mesh.panel_areas if mesh.panel_areas is not None else np.array([0.0])
    return MeshQuality(
        n_panels=mesh.n_panels,
        max_aspect_ratio=float(max_ar),
        min_angle_deg=float(min_ang) if min_ang < 180.0 else 90.0,
        mean_area=float(np.mean(areas)),
        area_variance=float(np.var(areas)),
    )


# ---------------------------------------------------------------------------
# Paneliser
# ---------------------------------------------------------------------------


class Panelizer:
    """Converts a HullSurface into a PanelMesh.

    Steps:
    1. Build starboard-side hull-surface vertices from y_grid.
    2. Build flat-bottom vertices along the centreline at z=-draft (keel).
    3. Assemble quad panels for hull side and flat bottom.
    4. Optionally mirror to port side when symmetry=False.
    5. Deduplicate vertices (collapsed bow/stern tips).
    6. Remove degenerate panels.
    7. Orient all normals outward.
    """

    def __init__(self, config: PanelizerConfig | None = None) -> None:
        self._cfg = config or PanelizerConfig()

    def panelise(
        self,
        surface: "HullSurface",
        defn: "HullLineDefinition",
    ) -> PanelMesh:
        """Panelise a HullSurface into a PanelMesh.

        Args:
            surface: Interpolated hull surface grid.
            defn: Original hull line definition (provides name, draft).

        Returns:
            PanelMesh ready for diffraction analysis.
        """
        x_grid = surface.x_grid
        z_grid = surface.z_grid  # keel-up: 0..draft
        y_grid = surface.y_grid  # (n_x+1, n_z+1)

        # Convert to marine z convention: z_marine = z_keel_up - draft
        draft = surface.draft
        z_marine = z_grid - draft  # -draft..0

        # 1. Starboard hull-surface vertices
        side_verts, side_panels = self._build_side_surface(
            x_grid, z_marine, y_grid
        )

        # 2. Flat bottom vertices and panels (at z = -draft)
        bot_verts, bot_panels = self._build_bottom(
            x_grid, z_marine, y_grid, n_y=self._cfg.n_y_bottom
        )

        # 3. Combine
        offset = len(side_verts)
        vertices = np.vstack([side_verts, bot_verts])
        panels = np.vstack([side_panels, bot_panels + offset]) if len(bot_panels) > 0 else side_panels

        # 4. Mirror to port if not using symmetry
        if not self._cfg.symmetry:
            vertices, panels = self._mirror_to_port(vertices, panels)
            symmetry_plane = None
        else:
            symmetry_plane = "y"

        # 5. Deduplicate
        vertices, panels = self._deduplicate_vertices(vertices, panels)

        # 6. Remove degenerate panels
        panels = self._remove_degenerate(panels)

        # 7. Build PanelMesh
        mesh = PanelMesh(
            vertices=vertices.astype(np.float64),
            panels=panels.astype(np.int32),
            name=f"{defn.name}_line_mesh",
            format_origin=MeshFormat.UNKNOWN,
            symmetry_plane=symmetry_plane,
        )

        # 8. Orient normals outward
        self._orient_normals(mesh)

        return mesh

    # ------------------------------------------------------------------
    # Private builders
    # ------------------------------------------------------------------

    @staticmethod
    def _build_side_surface(
        x_grid: NDArray[np.float64],
        z_marine: NDArray[np.float64],
        y_grid: NDArray[np.float64],
    ) -> tuple[NDArray[np.float64], NDArray[np.int32]]:
        """Build starboard-side surface vertices and quad panels.

        Vertex index: i_x * n_z_pts + i_z
        Panel winding (CCW from outside starboard): v00->v01->v11->v10
        """
        n_x_pts = len(x_grid)
        n_z_pts = len(z_marine)

        verts = np.zeros((n_x_pts * n_z_pts, 3), dtype=np.float64)
        for i in range(n_x_pts):
            for j in range(n_z_pts):
                idx = i * n_z_pts + j
                verts[idx, 0] = x_grid[i]
                verts[idx, 1] = y_grid[i, j]
                verts[idx, 2] = z_marine[j]

        panels: list[list[int]] = []
        for i in range(n_x_pts - 1):
            for j in range(n_z_pts - 1):
                v00 = i * n_z_pts + j
                v10 = (i + 1) * n_z_pts + j
                v11 = (i + 1) * n_z_pts + j + 1
                v01 = i * n_z_pts + j + 1
                panels.append([v00, v01, v11, v10])

        panels_arr = (
            np.array(panels, dtype=np.int32) if panels
            else np.empty((0, 4), dtype=np.int32)
        )
        return verts, panels_arr

    @staticmethod
    def _build_bottom(
        x_grid: NDArray[np.float64],
        z_marine: NDArray[np.float64],
        y_grid: NDArray[np.float64],
        n_y: int = 5,
    ) -> tuple[NDArray[np.float64], NDArray[np.int32]]:
        """Build flat-bottom (keel plane) vertices and quad panels.

        At z_marine[0] (keel depth), panels span from the centreline (y=0)
        to the keel half-breadth (y=y_grid[i, 0]).  The bottom is subdivided
        into *n_y* strips in the transverse direction to keep aspect ratios
        reasonable for wide hulls.

        Vertex index: i_x * (n_y + 1) + i_y
        """
        n_x_pts = len(x_grid)
        n_y_pts = n_y + 1  # including centreline (y=0) and hull edge
        z_keel = z_marine[0]

        verts = np.zeros((n_x_pts * n_y_pts, 3), dtype=np.float64)
        for i in range(n_x_pts):
            y_edge = y_grid[i, 0]
            for k in range(n_y_pts):
                t = k / n_y
                y_val = t * y_edge
                idx = i * n_y_pts + k
                verts[idx, :] = [x_grid[i], y_val, z_keel]

        panels: list[list[int]] = []
        for i in range(n_x_pts - 1):
            for k in range(n_y_pts - 1):
                v00 = i * n_y_pts + k
                v10 = (i + 1) * n_y_pts + k
                v11 = (i + 1) * n_y_pts + k + 1
                v01 = i * n_y_pts + k + 1
                # Normal points downward (-z); winding: v00->v10->v11->v01
                # reversed to: v00->v01->v11->v10 for consistent CCW from below
                panels.append([v00, v01, v11, v10])

        panels_arr = (
            np.array(panels, dtype=np.int32) if panels
            else np.empty((0, 4), dtype=np.int32)
        )
        return verts, panels_arr

    @staticmethod
    def _mirror_to_port(
        vertices: NDArray[np.float64],
        panels: NDArray[np.int32],
    ) -> tuple[NDArray[np.float64], NDArray[np.int32]]:
        """Mirror starboard geometry to port by negating y coordinates."""
        n_verts = len(vertices)
        port_verts = vertices.copy()
        port_verts[:, 1] = -port_verts[:, 1]
        # Reverse winding order so normals point outward on port side
        port_panels = panels[:, ::-1].copy() + n_verts
        return (
            np.vstack([vertices, port_verts]),
            np.vstack([panels, port_panels]),
        )

    @staticmethod
    def _deduplicate_vertices(
        vertices: NDArray[np.float64],
        panels: NDArray[np.int32],
    ) -> tuple[NDArray[np.float64], NDArray[np.int32]]:
        """Remove duplicate vertices (e.g. bow/stern tips) and remap indices."""
        rounded = np.round(vertices, decimals=10)
        _, first_occ, inverse = np.unique(
            rounded, axis=0, return_index=True, return_inverse=True
        )
        unique_verts = vertices[first_occ]
        new_panels = inverse[panels].astype(np.int32)
        return unique_verts, new_panels

    @staticmethod
    def _remove_degenerate(panels: NDArray[np.int32]) -> NDArray[np.int32]:
        """Drop panels where deduplication left fewer than 3 unique vertices."""
        if len(panels) == 0:
            return panels
        keep = [i for i, p in enumerate(panels) if len(set(p.tolist())) >= 3]
        if len(keep) == len(panels):
            return panels
        if not keep:
            return np.empty((0, panels.shape[1]), dtype=panels.dtype)
        return panels[np.array(keep)]

    @staticmethod
    def _orient_normals(mesh: PanelMesh) -> None:
        """Flip normals inward to the hull if they point toward the interior.

        The hull centroid is inside the surface.  A correct outward normal
        should have a positive dot product with the vector from the centroid
        to the panel centre.
        """
        centroid = np.mean(mesh.vertices, axis=0)
        for i in range(mesh.n_panels):
            to_outside = mesh.panel_centers[i] - centroid
            if np.dot(mesh.normals[i], to_outside) < 0:
                mesh.normals[i] = -mesh.normals[i]
                mesh.panels[i] = mesh.panels[i, ::-1]


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "PanelizerConfig",
    "MeshQuality",
    "Panelizer",
    "compute_mesh_quality",
]
