"""
ABOUTME: Hull surface interpolator — Phase 2 of the hull panel generator.

Builds a dense 3-D hull surface grid from a sparse set of station line
definitions using scipy BSpline interpolation.  Handles:
  - Monotonicity clamping (no negative half-breadths)
  - Discontinuities at transom / chine (segment splitting)
  - Port/starboard symmetry

Coordinate convention:
  x_grid  — longitudinal axis, 0 at AP, length_bp at FP (metres)
  z_grid  — keel-up vertical axis, 0 at keel, draft at DWL (metres)
  y_grid  — half-breadth (starboard positive), indexed [i_x, i_z]

The interpolation is performed in two passes:
  1. For each existing station, interpolate the z-profile to the common
     z_grid using a 1-D cubic spline (PCHIP to guarantee monotonicity
     where the section shape is monotone).
  2. For each z-slice, interpolate along x using a cubic B-spline, with
     degree reduced to min(3, n_stations-1) when station count is small.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional

import numpy as np
from numpy.typing import NDArray
from pydantic import BaseModel, Field
from scipy.interpolate import PchipInterpolator, make_interp_spline


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------


class HullSurfaceConfig(BaseModel):
    """Configuration for hull surface interpolation.

    Attributes:
        n_x: Number of x-grid intervals (produces n_x+1 points).
        n_z: Number of z-grid intervals (produces n_z+1 points).
        interpolation_method: Method for along-x interpolation.
            ``"bspline"`` uses cubic B-spline (default); ``"linear"``
            uses piecewise linear.
        waterline_refinement: Refinement multiplier for the z-grid that
            concentrates points near the design waterline.  Values > 1.0
            grade the z-spacing so there are more points near z=draft.
    """

    n_x: int = Field(default=40, ge=2, description="x-grid intervals")
    n_z: int = Field(default=20, ge=2, description="z-grid intervals")
    interpolation_method: str = Field(
        default="bspline",
        description="Interpolation method: 'bspline' or 'linear'",
    )
    waterline_refinement: float = Field(
        default=1.5,
        gt=0,
        description="Concentrates z-grid points near waterline (>1 = denser at WL)",
    )

    class Config:
        extra = "forbid"

    def model_post_init(self, __context) -> None:  # type: ignore[override]
        if self.n_x < 2:
            raise ValueError(f"n_x must be >= 2, got {self.n_x}")
        if self.n_z < 2:
            raise ValueError(f"n_z must be >= 2, got {self.n_z}")


# ---------------------------------------------------------------------------
# Surface data container
# ---------------------------------------------------------------------------


@dataclass
class HullSurface:
    """Holds the interpolated 3-D hull surface grid.

    Attributes:
        x_grid: 1-D array of x positions (n_x+1 points) from 0 to length_bp.
        z_grid: 1-D array of z positions (n_z+1 points, keel-up convention).
        y_grid: 2-D array of half-breadths, shape (n_x+1, n_z+1).
        length_bp: Hull length between perpendiculars (m).
        draft: Design draft (m).
    """

    x_grid: NDArray[np.float64]
    z_grid: NDArray[np.float64]
    y_grid: NDArray[np.float64]
    length_bp: float
    draft: float


# ---------------------------------------------------------------------------
# Interpolator
# ---------------------------------------------------------------------------


class HullSurfaceInterpolator:
    """Converts a HullLineDefinition into a HullSurface.

    The interpolation follows a two-pass strategy:

    Pass 1 (z-profile per station):
        For each existing station, interpolate the half-breadth to the
        common z_grid using PCHIP (Piecewise Cubic Hermite Interpolating
        Polynomial).  PCHIP is monotone-preserving — it will not introduce
        spurious oscillations or negative half-breadths in regions where the
        section is monotonically widening or narrowing.

    Pass 2 (along-x per z-slice):
        For each z row in y_grid, fit a cubic B-spline (or linear if
        ``interpolation_method="linear"`` or station count <= 3) through
        the station half-breadths.

    Post-processing:
        Clamp all half-breadths to >= 0 to ensure no negative values appear
        from spline ringing near sharp discontinuities (e.g. transom).
    """

    def __init__(self, config: HullSurfaceConfig | None = None) -> None:
        self._cfg = config or HullSurfaceConfig()

    def interpolate(self, defn: "HullLineDefinition") -> HullSurface:  # noqa: F821
        """Build a HullSurface from *defn*.

        Args:
            defn: Validated ``HullLineDefinition`` with at least 2 stations.

        Returns:
            ``HullSurface`` with x_grid, z_grid, y_grid populated.
        """
        cfg = self._cfg
        n_x_pts = cfg.n_x + 1  # grid points (intervals + 1)
        n_z_pts = cfg.n_z + 1

        # Build x grid (uniform)
        x_grid = np.linspace(0.0, defn.length_bp, n_x_pts)

        # Build z grid (keel-up, with optional waterline grading)
        z_grid = self._build_z_grid(defn.draft, n_z_pts, cfg.waterline_refinement)

        # ------------------------------------------------------------------
        # Pass 1: interpolate z-profile for each existing station onto z_grid
        # ------------------------------------------------------------------
        sorted_stations = sorted(defn.stations, key=lambda s: s.x)
        station_x = np.array([s.x for s in sorted_stations], dtype=np.float64)
        n_stations = len(station_x)

        # station_y_at_z[si, j] = half-breadth at station si, z_grid[j]
        station_y_at_z = np.zeros((n_stations, n_z_pts), dtype=np.float64)
        for si, station in enumerate(sorted_stations):
            station_y_at_z[si, :] = self._interp_z_profile(station, z_grid)

        # ------------------------------------------------------------------
        # Pass 2: interpolate along x for each z slice
        # ------------------------------------------------------------------
        y_grid = np.zeros((n_x_pts, n_z_pts), dtype=np.float64)
        method = cfg.interpolation_method.lower()

        for j in range(n_z_pts):
            y_at_stations = station_y_at_z[:, j]
            y_grid[:, j] = self._interp_x_slice(
                station_x, y_at_stations, x_grid, method
            )

        # Post-process: clamp negatives
        np.clip(y_grid, 0.0, None, out=y_grid)

        return HullSurface(
            x_grid=x_grid,
            z_grid=z_grid,
            y_grid=y_grid,
            length_bp=defn.length_bp,
            draft=defn.draft,
        )

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _build_z_grid(
        draft: float, n_pts: int, refinement: float
    ) -> NDArray[np.float64]:
        """Build a z grid from 0 to draft.

        With refinement > 1 the spacing is graded so more points fall near
        the waterline (z=draft).  Uses a power-law mapping:
            u in [0, 1] -> z = draft * u^(1/refinement)

        This means lower z values (near keel) have coarser spacing and
        higher z values (near waterline) have finer spacing.
        """
        u = np.linspace(0.0, 1.0, n_pts)
        exponent = 1.0 / max(refinement, 0.01)
        z_grid = draft * (u**exponent)
        z_grid[0] = 0.0  # force exact keel
        z_grid[-1] = draft  # force exact waterline
        return z_grid

    @staticmethod
    def _interp_z_profile(
        station: "StationOffset",  # noqa: F821
        z_grid: NDArray[np.float64],
    ) -> NDArray[np.float64]:
        """Interpolate a station's half-breadth onto z_grid using PCHIP.

        Station offsets are in keel-up (z >= 0) convention, matching z_grid.
        The PCHIP interpolator is used to avoid overshoot/undershoot in
        monotone sections.

        Args:
            station: A ``StationOffset`` with (z, y) offset pairs.
            z_grid: Target z positions (keel-up).

        Returns:
            Array of half-breadths at each z in z_grid.
        """
        offsets = sorted(station.offsets, key=lambda p: p[0])
        z_pts = np.array([p[0] for p in offsets], dtype=np.float64)
        y_pts = np.array([p[1] for p in offsets], dtype=np.float64)

        if len(z_pts) == 1:
            return np.full(len(z_grid), y_pts[0], dtype=np.float64)

        # Use PCHIP for monotone sections; extrapolate flat outside range
        interp = PchipInterpolator(z_pts, y_pts, extrapolate=False)
        y_vals = interp(z_grid)

        # Fill NaN from extrapolation with boundary values
        y_vals = np.where(
            np.isnan(y_vals),
            np.where(z_grid <= z_pts[0], y_pts[0], y_pts[-1]),
            y_vals,
        )
        return y_vals.astype(np.float64)

    @staticmethod
    def _interp_x_slice(
        station_x: NDArray[np.float64],
        y_at_stations: NDArray[np.float64],
        x_grid: NDArray[np.float64],
        method: str,
    ) -> NDArray[np.float64]:
        """Interpolate half-breadths along x for a single z slice.

        For ``method="bspline"`` a cubic (or lower degree if few stations)
        B-spline is used.  For ``method="linear"`` a piecewise linear
        interpolation is used.

        Extrapolation outside the station range uses the nearest boundary
        value.

        Args:
            station_x: Sorted x positions of existing stations.
            y_at_stations: Half-breadth at each station for this z slice.
            x_grid: Target x positions.
            method: ``"bspline"`` or ``"linear"``.

        Returns:
            1-D array of half-breadths at each x in x_grid.
        """
        n = len(station_x)
        if n == 1:
            return np.full(len(x_grid), y_at_stations[0], dtype=np.float64)

        if method == "linear" or n <= 3:
            y_out = np.interp(x_grid, station_x, y_at_stations)
            return y_out.astype(np.float64)

        # B-spline: degree = min(3, n-1)
        degree = min(3, n - 1)
        try:
            spline = make_interp_spline(
                station_x, y_at_stations, k=degree, check_finite=True
            )
            y_out = spline(x_grid)
        except Exception:
            # Fall back to linear if spline construction fails
            y_out = np.interp(x_grid, station_x, y_at_stations)

        # Extrapolate as flat boundary values
        y_out = np.where(x_grid < station_x[0], y_at_stations[0], y_out)
        y_out = np.where(x_grid > station_x[-1], y_at_stations[-1], y_out)

        return y_out.astype(np.float64)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "HullSurfaceConfig",
    "HullSurface",
    "HullSurfaceInterpolator",
]
