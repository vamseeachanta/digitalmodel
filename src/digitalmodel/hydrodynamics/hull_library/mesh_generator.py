"""
ABOUTME: Panel mesh generator that converts hull line profiles to PanelMesh
instances using linear interpolation, quad panelisation, and waterline density
grading. Mesh coarsening lives in coarsen_mesh.py.

Steps: normalise waterline offsets to marine z-convention, compute (n_x, n_z)
subdivisions, interpolate half-breadth y(x,z), build starboard quads + flat
bottom, optionally mirror to port, orient normals outward, return PanelMesh.
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING

import numpy as np
from numpy.typing import NDArray
from pydantic import BaseModel, Field
from scipy.interpolate import interp1d

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import (
    MeshFormat,
    PanelMesh,
)

if TYPE_CHECKING:
    from digitalmodel.hydrodynamics.hull_library.profile_schema import (
        HullProfile,
    )


class MeshGeneratorConfig(BaseModel):
    """Configuration for hull mesh generation."""

    target_panels: int = Field(default=1000, gt=0)
    waterline_refinement: float = Field(default=2.0, gt=0)
    symmetry: bool = True
    mesh_below_waterline_only: bool = True
    adaptive_density: bool = Field(
        default=False,
        description="Use curvature-adaptive panel distribution. "
        "Concentrates panels at bow/stern and spreads them in parallel body.",
    )


class HullMeshGenerator:
    """Converts a HullProfile into a PanelMesh."""

    def generate(
        self,
        profile: HullProfile,
        config: MeshGeneratorConfig | None = None,
    ) -> PanelMesh:
        """Generate a quad panel mesh from a hull profile.

        Args:
            profile: Hull profile with stations and waterline offsets.
            config: Mesh generation configuration. Uses defaults if None.

        Returns:
            PanelMesh with vertices, quad panels, and computed normals.
        """
        if config is None:
            config = MeshGeneratorConfig()

        # 1. Determine grid resolution from target panels
        n_x, n_z = self._compute_grid_resolution(profile, config)

        # 2. Build x and z grids
        if config.adaptive_density:
            x_values = self._compute_adaptive_x_grid(profile, n_x + 1)
        else:
            x_values = np.linspace(0.0, profile.length_bp, n_x + 1)
        draft = profile.draft
        # Marine convention: z=0 at waterline, z=-draft at keel
        z_values = np.linspace(-draft, 0.0, n_z + 1)

        # 3. Interpolate half-breadth y(x, z) on the grid
        y_grid = self._interpolate_hull_surface(profile, x_values, z_values)

        # 4. Build starboard hull surface vertices and panels
        vertices, panels = self._build_hull_surface(
            x_values, z_values, y_grid
        )

        # 5. Add flat bottom panels
        bottom_verts, bottom_panels = self._build_bottom(
            x_values, z_values, y_grid
        )
        if len(bottom_panels) > 0:
            offset = len(vertices)
            vertices = np.vstack([vertices, bottom_verts])
            panels = np.vstack([panels, bottom_panels + offset])

        # 6. Handle symmetry
        if not config.symmetry:
            vertices, panels = self._mirror_to_port(vertices, panels)
            symmetry_plane = None
        else:
            symmetry_plane = "y"

        # 7. Deduplicate vertices
        vertices, panels = self._deduplicate_vertices(vertices, panels)

        # 7b. Remove degenerate panels (where vertex deduplication collapsed
        # edges, e.g. at bow/stern tips where half-breadth is zero)
        panels = self._remove_degenerate_panels(panels)

        # 8. Build PanelMesh (normals/areas/centers computed in __post_init__)
        mesh = PanelMesh(
            vertices=vertices.astype(np.float64),
            panels=panels.astype(np.int32),
            name=f"{profile.name}_mesh",
            format_origin=MeshFormat.UNKNOWN,
            symmetry_plane=symmetry_plane,
        )

        # 9. Orient normals outward
        self._orient_normals_outward(mesh)

        return mesh

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _compute_grid_resolution(
        self,
        profile: HullProfile,
        config: MeshGeneratorConfig,
    ) -> tuple[int, int]:
        """Determine n_x and n_z subdivisions from target panel count.

        For the hull surface we have n_x * n_z panels on the side.
        Plus n_x bottom panels.  We solve:
            n_x * n_z + n_x ~ target  =>  n_x * (n_z + 1) ~ target
        with aspect ratio driven by hull proportions.
        """
        target = config.target_panels
        aspect = profile.length_bp / profile.draft if profile.draft > 0 else 10
        # n_x / n_z ~ aspect * refinement_factor
        refinement = config.waterline_refinement
        # n_x * (n_z + 1) = target
        # n_x = aspect * refinement * n_z
        # => aspect * refinement * n_z * (n_z + 1) = target
        # quadratic in n_z
        a = aspect * refinement
        # a * n_z^2 + a * n_z - target = 0
        discriminant = a * a + 4 * a * target
        n_z = int((-a + math.sqrt(discriminant)) / (2 * a))
        n_z = max(n_z, 2)
        n_x = max(int(a * n_z), 2)

        return n_x, n_z

    def _compute_adaptive_x_grid(
        self,
        profile: HullProfile,
        n_points: int,
    ) -> NDArray[np.float64]:
        """Compute a non-uniform x-grid that concentrates points at high-curvature regions.

        Samples the hull half-breadth at design draft on a fine uniform grid,
        computes a curvature proxy (|d^2y/dx^2|), and uses inverse-CDF sampling
        to place more x-positions where curvature is higher (bow/stern).

        Args:
            profile: Hull profile providing station geometry.
            n_points: Number of x-grid points to produce (including endpoints).

        Returns:
            Sorted array of n_points x-positions in [0, length_bp].
        """
        length = profile.length_bp
        draft = profile.draft
        n_sample = 200

        # Fine uniform sample of x positions
        x_fine = np.linspace(0.0, length, n_sample)

        # Build half-breadth at design draft for each x position
        sorted_stations = sorted(profile.stations, key=lambda s: s.x_position)
        station_x = np.array([s.x_position for s in sorted_stations])

        # Evaluate each station's half-breadth at design draft (z_keel = draft)
        station_y_at_draft = np.zeros(len(sorted_stations), dtype=np.float64)
        for si, station in enumerate(sorted_stations):
            offsets = sorted(station.waterline_offsets, key=lambda p: p[0])
            z_keel = np.array([p[0] for p in offsets])
            y_half = np.array([p[1] for p in offsets])
            if len(z_keel) == 1:
                station_y_at_draft[si] = y_half[0]
            else:
                interp_z = interp1d(
                    z_keel,
                    y_half,
                    kind="linear",
                    bounds_error=False,
                    fill_value=(y_half[0], y_half[-1]),
                )
                station_y_at_draft[si] = float(interp_z(draft))

        # Interpolate half-breadth along x at design draft
        if len(station_x) == 1:
            y_fine = np.full(n_sample, station_y_at_draft[0])
        else:
            interp_x = interp1d(
                station_x,
                station_y_at_draft,
                kind="linear",
                bounds_error=False,
                fill_value=(station_y_at_draft[0], station_y_at_draft[-1]),
            )
            y_fine = interp_x(x_fine)

        # Compute second derivative as curvature proxy
        dy = np.gradient(y_fine, x_fine)
        d2y = np.gradient(dy, x_fine)
        abs_d2y = np.abs(d2y)

        # Build curvature weight
        curvature_gain = 3.0
        max_d2y = np.max(abs_d2y)
        eps = 1e-12
        weights = 1.0 + curvature_gain * abs_d2y / (max_d2y + eps)

        # Build cumulative density function from weights
        cdf = np.cumsum(weights)
        cdf = cdf / cdf[-1]  # normalize to [0, 1]

        # Inverse CDF: place n_points uniformly in [0, 1] and map to x
        uniform_samples = np.linspace(0.0, 1.0, n_points)
        x_adaptive = np.interp(uniform_samples, cdf, x_fine)

        # Force endpoints to be exact
        x_adaptive[0] = 0.0
        x_adaptive[-1] = length

        return x_adaptive

    def _interpolate_hull_surface(
        self,
        profile: HullProfile,
        x_values: NDArray[np.float64],
        z_values: NDArray[np.float64],
    ) -> NDArray[np.float64]:
        """Interpolate half-breadth y on a regular (x, z) grid.

        Returns:
            y_grid of shape (len(x_values), len(z_values)) with half-breadth
            values.  y_grid[i, j] = half-breadth at (x_values[i], z_values[j]).
        """
        draft = profile.draft
        # Sort stations by x_position
        sorted_stations = sorted(profile.stations, key=lambda s: s.x_position)

        station_x = np.array([s.x_position for s in sorted_stations])
        n_x = len(x_values)
        n_z = len(z_values)
        y_grid = np.zeros((n_x, n_z), dtype=np.float64)

        # For each station, build y(z) interpolator in marine convention
        station_y_at_z = np.zeros((len(sorted_stations), n_z), dtype=np.float64)
        for si, station in enumerate(sorted_stations):
            offsets = sorted(station.waterline_offsets, key=lambda p: p[0])
            # offsets are (z_keel_up, half_breadth)
            # Convert z from keel-up to marine convention: z_marine = z_keel - draft
            z_keel = np.array([p[0] for p in offsets])
            y_half = np.array([p[1] for p in offsets])
            z_marine = z_keel - draft

            if len(z_keel) == 1:
                # Single point: constant half-breadth
                station_y_at_z[si, :] = y_half[0]
            else:
                interp_z = interp1d(
                    z_marine,
                    y_half,
                    kind="linear",
                    bounds_error=False,
                    fill_value=(y_half[0], y_half[-1]),
                )
                station_y_at_z[si, :] = interp_z(z_values)

        # Interpolate along x for each z level
        for j in range(n_z):
            if len(station_x) == 1:
                y_grid[:, j] = station_y_at_z[0, j]
            else:
                interp_x = interp1d(
                    station_x,
                    station_y_at_z[:, j],
                    kind="linear",
                    bounds_error=False,
                    fill_value=(station_y_at_z[0, j], station_y_at_z[-1, j]),
                )
                y_grid[:, j] = interp_x(x_values)

        # Clamp negative half-breadths to zero
        y_grid = np.maximum(y_grid, 0.0)

        return y_grid

    def _build_hull_surface(
        self,
        x_values: NDArray[np.float64],
        z_values: NDArray[np.float64],
        y_grid: NDArray[np.float64],
    ) -> tuple[NDArray[np.float64], NDArray[np.int32]]:
        """Build starboard side hull surface vertices and quad panels.

        Vertices are placed on the hull surface at (x, y(x,z), z).
        Panels are quads connecting adjacent grid points.
        """
        n_x = len(x_values)
        n_z = len(z_values)

        # Build vertex array: index = i * n_z + j
        vertices = np.zeros((n_x * n_z, 3), dtype=np.float64)
        for i in range(n_x):
            for j in range(n_z):
                idx = i * n_z + j
                vertices[idx, 0] = x_values[i]
                vertices[idx, 1] = y_grid[i, j]
                vertices[idx, 2] = z_values[j]

        # Build quad panels on the side surface
        panels = []
        for i in range(n_x - 1):
            for j in range(n_z - 1):
                # Four corners of the quad
                v00 = i * n_z + j
                v10 = (i + 1) * n_z + j
                v11 = (i + 1) * n_z + (j + 1)
                v01 = i * n_z + (j + 1)
                # Wind counter-clockwise when viewed from outside (starboard)
                # For starboard (y > 0): normal should point in +y direction
                # CCW from outside: v00 -> v01 -> v11 -> v10
                panels.append([v00, v01, v11, v10])

        panels_arr = np.array(panels, dtype=np.int32) if panels else np.empty(
            (0, 4), dtype=np.int32
        )
        return vertices, panels_arr

    def _build_bottom(
        self,
        x_values: NDArray[np.float64],
        z_values: NDArray[np.float64],
        y_grid: NDArray[np.float64],
    ) -> tuple[NDArray[np.float64], NDArray[np.int32]]:
        """Build flat bottom panels along the keel (z = z_min).

        Bottom panels span from y=0 (centerline) to y=y_grid[i, 0] (keel
        half-breadth) at z = z_values[0] (keel depth).
        """
        n_x = len(x_values)
        z_keel = z_values[0]

        # Create vertices along centerline and at keel edge
        # For each x position: two vertices -- one at centerline, one at
        # hull edge
        vertices = np.zeros((n_x * 2, 3), dtype=np.float64)
        for i in range(n_x):
            # Centerline vertex
            vertices[i * 2, 0] = x_values[i]
            vertices[i * 2, 1] = 0.0
            vertices[i * 2, 2] = z_keel
            # Hull edge vertex at keel
            vertices[i * 2 + 1, 0] = x_values[i]
            vertices[i * 2 + 1, 1] = y_grid[i, 0]
            vertices[i * 2 + 1, 2] = z_keel

        panels = []
        for i in range(n_x - 1):
            v_cl_0 = i * 2        # centerline at x[i]
            v_ed_0 = i * 2 + 1    # edge at x[i]
            v_cl_1 = (i + 1) * 2  # centerline at x[i+1]
            v_ed_1 = (i + 1) * 2 + 1  # edge at x[i+1]
            # Normal should point downward (-z) for bottom panel
            # When viewed from below (looking up), CCW winding:
            # v_cl_0 -> v_cl_1 -> v_ed_1 -> v_ed_0
            panels.append([v_cl_0, v_cl_1, v_ed_1, v_ed_0])

        panels_arr = np.array(panels, dtype=np.int32) if panels else np.empty(
            (0, 4), dtype=np.int32
        )
        return vertices, panels_arr

    def _mirror_to_port(
        self,
        vertices: NDArray[np.float64],
        panels: NDArray[np.int32],
    ) -> tuple[NDArray[np.float64], NDArray[np.int32]]:
        """Mirror starboard geometry to create port side.

        Port-side vertices have negated y-coordinates.
        Port-side panel winding is reversed so normals point outward on that
        side as well.
        """
        n_verts = len(vertices)
        port_verts = vertices.copy()
        port_verts[:, 1] = -port_verts[:, 1]

        # Reverse winding order for port panels
        port_panels = panels.copy()
        port_panels = port_panels[:, ::-1]
        port_panels = port_panels + n_verts

        all_vertices = np.vstack([vertices, port_verts])
        all_panels = np.vstack([panels, port_panels])

        return all_vertices, all_panels

    def _deduplicate_vertices(
        self,
        vertices: NDArray[np.float64],
        panels: NDArray[np.int32],
    ) -> tuple[NDArray[np.float64], NDArray[np.int32]]:
        """Remove duplicate vertices and remap panel indices."""
        unique_verts, inverse = np.unique(
            np.round(vertices, decimals=10),
            axis=0,
            return_inverse=True,
        )
        # Remap panel indices
        new_panels = inverse[panels]
        # Restore original precision for unique vertices by picking the first
        # occurrence
        # np.unique sorts, so we need to map back properly
        # Actually, unique_verts from rounded is fine for our precision needs
        # But let's use the original vertex positions for accuracy
        first_occurrence = np.zeros(len(unique_verts), dtype=np.int64)
        seen = set()
        for old_idx in range(len(vertices)):
            new_idx = inverse[old_idx]
            if new_idx not in seen:
                first_occurrence[new_idx] = old_idx
                seen.add(new_idx)
        accurate_verts = vertices[first_occurrence]

        return accurate_verts, new_panels.astype(np.int32)

    @staticmethod
    def _remove_degenerate_panels(
        panels: NDArray[np.int32],
    ) -> NDArray[np.int32]:
        """Remove panels with fewer than 3 unique vertices.

        After vertex deduplication, some panels may have collapsed edges
        (e.g. at bow/stern tips where half-breadth is zero and multiple
        vertices merge). These produce zero-area panels and must be removed.
        """
        if len(panels) == 0:
            return panels
        keep = []
        for i in range(len(panels)):
            if len(set(panels[i].tolist())) >= 3:
                keep.append(i)
        if len(keep) == len(panels):
            return panels
        if len(keep) == 0:
            return np.empty((0, panels.shape[1]), dtype=panels.dtype)
        return panels[np.array(keep)]

    def _orient_normals_outward(self, mesh: PanelMesh) -> None:
        """Ensure all panel normals point outward (away from hull interior).

        For a closed hull, the centroid of all vertices is inside. Each
        panel's normal should point away from this centroid.
        """
        centroid = np.mean(mesh.vertices, axis=0)

        flip_mask = np.zeros(mesh.n_panels, dtype=bool)
        for i in range(mesh.n_panels):
            center = mesh.panel_centers[i]
            normal = mesh.normals[i]
            to_outside = center - centroid
            if np.dot(normal, to_outside) < 0:
                flip_mask[i] = True

        if np.any(flip_mask):
            # Flip normals
            mesh.normals[flip_mask] = -mesh.normals[flip_mask]
            # Reverse winding of those panels
            mesh.panels[flip_mask] = mesh.panels[flip_mask, ::-1]


__all__ = [
    "MeshGeneratorConfig",
    "HullMeshGenerator",
]
