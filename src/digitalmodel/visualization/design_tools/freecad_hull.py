"""
ABOUTME: FreeCAD parametric hull generator consuming HullProfile from hull_library.
Creates NURBS solid bodies for CAD operations and STEP export.
"""

from __future__ import annotations

import logging
import warnings
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile

logger = logging.getLogger(__name__)

# Conditional FreeCAD import -- fall back to scipy NURBS when absent.
try:
    import FreeCAD
    import Part

    FREECAD_AVAILABLE = True
except ImportError:
    FREECAD_AVAILABLE = False


class FreeCADHullGenerator:
    """Generate a NURBS hull surface from a :class:`HullProfile`.

    When FreeCAD is available the generator produces a ``Part.BSplineSurface``
    and can export to STEP.  Without FreeCAD the generator builds an
    equivalent numpy point grid suitable for downstream scipy interpolation.
    """

    def __init__(self, profile: HullProfile) -> None:
        self._profile = profile
        self._stations = sorted(
            profile.stations, key=lambda s: s.x_position
        )

    # ------------------------------------------------------------------
    # NURBS surface generation
    # ------------------------------------------------------------------

    def generate_nurbs_surface(self) -> dict:
        """Build a NURBS surface representation of the hull.

        Returns
        -------
        dict
            surface_points : np.ndarray, shape (n_stations, n_wl, 3)
            stations_used  : int
            surface_type   : 'freecad' | 'scipy'
        """
        n_wl = self._uniform_waterline_count()
        pts = self._build_point_grid(n_wl)

        if FREECAD_AVAILABLE:
            return {
                "surface_points": pts,
                "stations_used": len(self._stations),
                "surface_type": "freecad",
                "fc_surface": self._build_freecad_surface(pts),
            }
        return {
            "surface_points": pts,
            "stations_used": len(self._stations),
            "surface_type": "scipy",
        }

    # ------------------------------------------------------------------
    # Solid / export
    # ------------------------------------------------------------------

    def create_solid(self, export_path: Path | None = None) -> dict:
        """Create a solid body from the hull surface.

        Parameters
        ----------
        export_path : Path, optional
            If given and FreeCAD is available, export the solid to STEP.

        Returns
        -------
        dict  Metadata about the generated solid.
        """
        surface_data = self.generate_nurbs_surface()
        meta: dict = {
            "length_bp": self._profile.length_bp,
            "beam": self._profile.beam,
            "draft": self._profile.draft,
            "stations_used": surface_data["stations_used"],
            "surface_type": surface_data["surface_type"],
        }

        if FREECAD_AVAILABLE:
            solid = self._freecad_solid(surface_data)
            meta["fc_solid"] = solid
            if export_path is not None:
                self.export_step(export_path)
        return meta

    def export_step(self, path: Path) -> Path | None:
        """Export the hull solid to a STEP file.

        Returns the path on success, *None* if FreeCAD is unavailable.
        """
        if not FREECAD_AVAILABLE:
            warnings.warn(
                "FreeCAD not available -- STEP export skipped.",
                stacklevel=2,
            )
            return None
        surface_data = self.generate_nurbs_surface()
        solid = self._freecad_solid(surface_data)
        Part.export([solid], str(path))
        logger.info("Exported hull STEP to %s", path)
        return Path(path)

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _uniform_waterline_count(self) -> int:
        """Determine a common waterline point count across stations."""
        counts = [len(s.waterline_offsets) for s in self._stations]
        return max(counts)

    def _build_point_grid(self, n_wl: int) -> np.ndarray:
        """Interpolate all stations onto a uniform (n_stations, n_wl, 3)
        grid of (x, y, z) points representing the starboard side.
        """
        n_st = len(self._stations)
        grid = np.zeros((n_st, n_wl, 3))
        draft = self._profile.draft
        z_uniform = np.linspace(0, draft, n_wl)

        for i, station in enumerate(self._stations):
            offsets = sorted(station.waterline_offsets, key=lambda o: o[0])
            z_arr = np.array([o[0] for o in offsets])
            y_arr = np.array([o[1] for o in offsets])
            y_interp = np.interp(z_uniform, z_arr, y_arr)
            grid[i, :, 0] = station.x_position
            grid[i, :, 1] = y_interp
            grid[i, :, 2] = z_uniform
        return grid

    @staticmethod
    def _build_freecad_surface(pts: np.ndarray):  # pragma: no cover
        """Build a ``Part.BSplineSurface`` from the point grid."""
        n_u, n_v, _ = pts.shape
        fc_pts = [
            [FreeCAD.Vector(float(pts[i, j, 0]),
                            float(pts[i, j, 1]),
                            float(pts[i, j, 2]))
             for j in range(n_v)]
            for i in range(n_u)
        ]
        surface = Part.BSplineSurface()
        surface.interpolate(fc_pts)
        return surface

    @staticmethod
    def _freecad_solid(surface_data: dict):  # pragma: no cover
        """Create a FreeCAD solid from the surface data."""
        fc_surface = surface_data.get("fc_surface")
        if fc_surface is None:
            raise RuntimeError("No FreeCAD surface available")
        face = fc_surface.toShape()
        # Mirror for port side
        mirror = face.mirror(FreeCAD.Vector(0, 0, 0),
                             FreeCAD.Vector(0, 1, 0))
        shell = Part.Shell([face, mirror])
        solid = Part.Solid(shell)
        return solid
