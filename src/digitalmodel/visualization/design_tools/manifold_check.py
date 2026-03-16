"""
ABOUTME: Manifold validation for hull surface meshes -- checks watertightness,
self-intersection, and topological consistency of generated hull geometry.
"""

from __future__ import annotations

import numpy as np


class ManifoldChecker:
    """Validate a structured hull surface grid for geometric consistency.

    The surface is represented as a numpy array of shape
    (n_stations, n_waterlines, 3) forming a structured quad mesh.
    """

    def __init__(self, surface_points: np.ndarray) -> None:
        if surface_points.ndim != 3 or surface_points.shape[2] != 3:
            raise ValueError(
                "surface_points must have shape (n_stations, n_waterlines, 3)"
            )
        self._pts = surface_points

    # ------------------------------------------------------------------
    # Individual checks
    # ------------------------------------------------------------------

    def check_watertight(self) -> bool:
        """Verify the surface has no open boundaries (no NaN/inf gaps).

        A structured grid is watertight if every point is finite and
        boundary edges form continuous curves (no gaps in the grid).
        """
        if not np.all(np.isfinite(self._pts)):
            return False

        n_st, n_wl, _ = self._pts.shape
        if n_st < 2 or n_wl < 2:
            return False

        # Check boundary continuity: adjacent boundary points should be
        # reasonably close (no teleportation gaps).
        max_span = np.max(np.ptp(self._pts.reshape(-1, 3), axis=0))
        if max_span == 0:
            return False
        threshold = max_span * 0.5

        # Check along-length boundaries (first/last waterline rows)
        for wl_idx in [0, n_wl - 1]:
            for i in range(n_st - 1):
                dist = np.linalg.norm(
                    self._pts[i + 1, wl_idx] - self._pts[i, wl_idx]
                )
                if dist > threshold:
                    return False

        # Check across-waterline boundaries (first/last station cols)
        for st_idx in [0, n_st - 1]:
            for j in range(n_wl - 1):
                dist = np.linalg.norm(
                    self._pts[st_idx, j + 1] - self._pts[st_idx, j]
                )
                if dist > threshold:
                    return False

        return True

    def check_self_intersection(self) -> bool:
        """Verify no self-intersecting panels in the quad mesh.

        Uses a simplified check: for each quad, verify that adjacent
        face normals do not flip direction (indicating a fold-over).
        Returns True if NO self-intersection detected.
        """
        n_st, n_wl, _ = self._pts.shape
        if n_st < 2 or n_wl < 2:
            return True

        normals = self._compute_face_normals()
        if normals is None:
            return False

        n_faces_u = n_st - 1
        n_faces_v = n_wl - 1

        # Check u-adjacent faces
        for i in range(n_faces_u - 1):
            for j in range(n_faces_v):
                dot = np.dot(
                    normals[i, j], normals[i + 1, j]
                )
                if dot < -0.5:
                    return False

        # Check v-adjacent faces
        for i in range(n_faces_u):
            for j in range(n_faces_v - 1):
                dot = np.dot(
                    normals[i, j], normals[i, j + 1]
                )
                if dot < -0.5:
                    return False

        return True

    def check_normals_consistent(self) -> bool:
        """Verify all face normals point in a consistent direction.

        For a hull surface, all normals should point generally outward
        (positive y-component for starboard side).
        """
        normals = self._compute_face_normals()
        if normals is None:
            return False

        # Check that no normal has a wildly different direction from
        # the mean normal.
        flat = normals.reshape(-1, 3)
        norms = np.linalg.norm(flat, axis=1)
        valid = norms > 1e-12
        if not np.any(valid):
            return False
        flat_valid = flat[valid]
        mean_normal = np.mean(flat_valid, axis=0)
        mean_norm = np.linalg.norm(mean_normal)
        if mean_norm < 1e-12:
            return False
        mean_normal /= mean_norm

        for n in flat_valid:
            n_unit = n / np.linalg.norm(n)
            if np.dot(n_unit, mean_normal) < -0.3:
                return False
        return True

    # ------------------------------------------------------------------
    # Summary
    # ------------------------------------------------------------------

    def run_all_checks(self) -> dict:
        """Run all manifold checks and return a summary dict."""
        watertight = self.check_watertight()
        no_self = self.check_self_intersection()
        normals = self.check_normals_consistent()
        return {
            "watertight": watertight,
            "no_self_intersection": no_self,
            "normals_consistent": normals,
            "pass": watertight and no_self and normals,
        }

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _compute_face_normals(self) -> np.ndarray | None:
        """Compute face normals for the quad mesh.

        Each quad (i,j)-(i+1,j)-(i+1,j+1)-(i,j+1) has a normal
        computed from the cross product of its diagonals.

        Returns shape (n_st-1, n_wl-1, 3) or None if grid too small.
        """
        n_st, n_wl, _ = self._pts.shape
        if n_st < 2 or n_wl < 2:
            return None

        if not np.all(np.isfinite(self._pts)):
            return None

        n_u = n_st - 1
        n_v = n_wl - 1
        normals = np.zeros((n_u, n_v, 3))

        for i in range(n_u):
            for j in range(n_v):
                p00 = self._pts[i, j]
                p10 = self._pts[i + 1, j]
                p11 = self._pts[i + 1, j + 1]
                p01 = self._pts[i, j + 1]
                diag1 = p11 - p00
                diag2 = p01 - p10
                normal = np.cross(diag1, diag2)
                norm = np.linalg.norm(normal)
                if norm > 1e-12:
                    normals[i, j] = normal / norm
        return normals
