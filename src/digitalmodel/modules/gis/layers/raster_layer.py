"""Raster layer for grid-based data such as bathymetry and seabed surveys.

Provides :class:`RasterLayer`, a wrapper around a 2D numpy array that
carries CRS metadata and geographic bounds, with convenience methods for
sampling, statistics, and GeoTIFF I/O.
"""

from __future__ import annotations

import logging
from pathlib import Path

import numpy as np

from digitalmodel.modules.gis.core.crs import CRSDefinition
from digitalmodel.modules.gis.io.geotiff_handler import GeoTIFFHandler

logger = logging.getLogger(__name__)


class RasterLayer:
    """A raster layer backed by a 2D numpy array with CRS metadata.

    Parameters
    ----------
    data:
        2D numpy array of raster values (e.g. depth, elevation).
    crs:
        Coordinate reference system definition. Defaults to WGS 84.
    name:
        Human-readable layer name.
    bounds:
        Geographic extent as ``(min_x, min_y, max_x, max_y)``.
    nodata:
        Value representing missing or invalid cells.
    """

    def __init__(
        self,
        data: np.ndarray,
        crs: CRSDefinition | None = None,
        name: str = "raster",
        bounds: tuple[float, float, float, float] | None = None,
        nodata: float | None = None,
    ) -> None:
        self._data = data
        self._crs = crs if crs is not None else CRSDefinition.wgs84()
        self._name = name
        self._bounds = bounds
        self._nodata = nodata

    # ------------------------------------------------------------------
    # Properties
    # ------------------------------------------------------------------

    @property
    def data(self) -> np.ndarray:
        """The underlying raster data as a 2D numpy array."""
        return self._data

    @property
    def crs(self) -> CRSDefinition:
        """The coordinate reference system of this layer."""
        return self._crs

    @property
    def name(self) -> str:
        """Human-readable layer name."""
        return self._name

    @property
    def bounds(self) -> tuple[float, float, float, float] | None:
        """Geographic extent as ``(min_x, min_y, max_x, max_y)``."""
        return self._bounds

    @property
    def shape(self) -> tuple[int, int]:
        """Shape of the raster grid as ``(rows, cols)``."""
        return self._data.shape  # type: ignore[return-value]

    @property
    def resolution(self) -> tuple[float, float] | None:
        """Cell size as ``(x_res, y_res)`` computed from bounds and shape."""
        if self._bounds is None:
            return None
        min_x, min_y, max_x, max_y = self._bounds
        rows, cols = self._data.shape
        return ((max_x - min_x) / cols, (max_y - min_y) / rows)

    @property
    def nodata(self) -> float | None:
        """The value representing missing or invalid cells."""
        return self._nodata

    # ------------------------------------------------------------------
    # Factory class methods
    # ------------------------------------------------------------------

    @classmethod
    def from_geotiff(
        cls, filepath: str | Path, name: str = "raster"
    ) -> RasterLayer:
        """Load a RasterLayer from a GeoTIFF file.

        Parameters
        ----------
        filepath:
            Path to the GeoTIFF file.
        name:
            Human-readable layer name.

        Returns
        -------
        RasterLayer
            A new layer with data read from the file.
        """
        result = GeoTIFFHandler.read(filepath)
        crs_str = result["crs"]
        crs = None
        if crs_str and "EPSG" in crs_str.upper():
            try:
                epsg_code = int(crs_str.split(":")[-1])
                crs = CRSDefinition.from_epsg(epsg_code)
            except (ValueError, IndexError):
                crs = CRSDefinition.wgs84()
        return cls(
            data=result["data"],
            crs=crs,
            name=name,
            bounds=result["bounds"],
            nodata=result["nodata"],
        )

    # ------------------------------------------------------------------
    # Export methods
    # ------------------------------------------------------------------

    def to_geotiff(self, filepath: str | Path) -> Path:
        """Export this layer to a GeoTIFF file.

        Parameters
        ----------
        filepath:
            Destination file path.

        Returns
        -------
        Path
            The path to the written file.
        """
        crs_str = "EPSG:4326"
        if self._crs.epsg_code is not None:
            crs_str = f"EPSG:{self._crs.epsg_code}"
        return GeoTIFFHandler.write(
            self._data,
            filepath,
            crs=crs_str,
            bounds=self._bounds,
            nodata=self._nodata,
        )

    # ------------------------------------------------------------------
    # Sampling and query methods
    # ------------------------------------------------------------------

    def sample_point(self, x: float, y: float) -> float | None:
        """Sample the raster value at geographic coordinates.

        Parameters
        ----------
        x:
            X coordinate (longitude or easting).
        y:
            Y coordinate (latitude or northing).

        Returns
        -------
        float | None
            The raster value at ``(x, y)``, or ``None`` if the point
            is outside the bounds or falls on a nodata cell.
        """
        if self._bounds is None:
            return None
        min_x, min_y, max_x, max_y = self._bounds
        if not (min_x <= x <= max_x and min_y <= y <= max_y):
            return None

        rows, cols = self._data.shape
        col = int((x - min_x) / (max_x - min_x) * cols)
        row = int((max_y - y) / (max_y - min_y) * rows)
        col = min(col, cols - 1)
        row = min(row, rows - 1)

        value = float(self._data[row, col])
        if self._nodata is not None and value == self._nodata:
            return None
        return value

    # ------------------------------------------------------------------
    # Conversion methods
    # ------------------------------------------------------------------

    def to_xyz_points(self) -> list[tuple[float, float, float]]:
        """Convert raster cells to a list of ``(x, y, z)`` tuples.

        Cells matching the nodata value are skipped.

        Returns
        -------
        list[tuple[float, float, float]]
            One tuple per valid cell.
        """
        if self._bounds is None:
            return []
        min_x, min_y, max_x, max_y = self._bounds
        rows, cols = self._data.shape
        x_res = (max_x - min_x) / cols
        y_res = (max_y - min_y) / rows

        points: list[tuple[float, float, float]] = []
        for row in range(rows):
            for col in range(cols):
                value = self._data[row, col]
                if self._nodata is not None and value == self._nodata:
                    continue
                x = min_x + (col + 0.5) * x_res
                y = max_y - (row + 0.5) * y_res
                points.append((x, y, float(value)))
        return points

    # ------------------------------------------------------------------
    # Statistics
    # ------------------------------------------------------------------

    def statistics(self) -> dict:
        """Compute summary statistics for the raster data.

        Returns
        -------
        dict
            Keys: min, max, mean, std, nodata_count.
        """
        if self._nodata is not None:
            mask = self._data != self._nodata
            valid = self._data[mask]
            nodata_count = int(np.sum(~mask))
        else:
            valid = self._data.ravel()
            nodata_count = 0

        if valid.size == 0:
            return {
                "min": None,
                "max": None,
                "mean": None,
                "std": None,
                "nodata_count": nodata_count,
            }
        return {
            "min": float(np.min(valid)),
            "max": float(np.max(valid)),
            "mean": float(np.mean(valid)),
            "std": float(np.std(valid)),
            "nodata_count": nodata_count,
        }

    # ------------------------------------------------------------------
    # Dunder methods
    # ------------------------------------------------------------------

    def __repr__(self) -> str:
        return (
            f"RasterLayer(name={self._name!r}, "
            f"shape={self.shape}, "
            f"crs={self._crs.crs_type.value})"
        )
