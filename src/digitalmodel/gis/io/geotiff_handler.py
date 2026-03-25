"""GeoTIFF file handler for reading, writing, and converting raster data.

Uses rasterio (optional dependency) for GeoTIFF I/O. Primarily intended
for bathymetry and seabed survey grids.
"""

from __future__ import annotations

import logging
from pathlib import Path

import numpy as np

logger = logging.getLogger(__name__)

try:
    import rasterio
    from rasterio.transform import from_bounds

    HAS_RASTERIO = True
except ImportError:  # pragma: no cover
    HAS_RASTERIO = False


def _require_rasterio() -> None:
    """Raise ImportError if rasterio is not available."""
    if not HAS_RASTERIO:
        raise ImportError(
            "rasterio is required for GeoTIFF operations but is not installed. "
            "Install it with: pip install rasterio"
        )


class GeoTIFFHandler:
    """Handles reading, writing, and conversion of GeoTIFF raster data."""

    @classmethod
    def read(cls, filepath: str | Path) -> dict:
        """Read a GeoTIFF file and return its contents as a dict.

        Parameters
        ----------
        filepath:
            Path to the GeoTIFF file.

        Returns
        -------
        dict
            Keys: 'data' (numpy 2D array), 'crs' (EPSG string),
            'bounds' (min_x, min_y, max_x, max_y), 'resolution' (x_res, y_res),
            'nodata' (nodata value or None).
        """
        _require_rasterio()
        filepath = Path(filepath)

        with rasterio.open(filepath) as src:
            data = src.read(1)
            bounds = src.bounds
            res_x = src.res[0]
            res_y = src.res[1]
            crs_str = src.crs.to_string() if src.crs else None
            nodata = src.nodata

        logger.info("Read GeoTIFF file: %s (%s)", filepath, data.shape)
        return {
            "data": data,
            "crs": crs_str,
            "bounds": (bounds.left, bounds.bottom, bounds.right, bounds.top),
            "resolution": (res_x, res_y),
            "nodata": nodata,
        }

    @classmethod
    def write(
        cls,
        data: np.ndarray,
        filepath: str | Path,
        crs: str = "EPSG:4326",
        bounds: tuple[float, float, float, float] | None = None,
        nodata: float | None = None,
    ) -> Path:
        """Write a numpy 2D array to a GeoTIFF file.

        Parameters
        ----------
        data:
            2D numpy array of raster values.
        filepath:
            Destination file path.
        crs:
            Coordinate reference system string (default ``"EPSG:4326"``).
        bounds:
            ``(min_x, min_y, max_x, max_y)`` geographic bounds. When
            ``None`` a unit transform is used.
        nodata:
            Value representing no-data pixels.

        Returns
        -------
        Path
            The path to the written file.
        """
        _require_rasterio()
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)

        height, width = data.shape
        if bounds is not None:
            transform = from_bounds(
                bounds[0], bounds[1], bounds[2], bounds[3], width, height
            )
        else:
            transform = rasterio.transform.from_bounds(0, 0, width, height, width, height)

        with rasterio.open(
            filepath,
            "w",
            driver="GTiff",
            height=height,
            width=width,
            count=1,
            dtype=data.dtype,
            crs=crs,
            transform=transform,
            nodata=nodata,
        ) as dst:
            dst.write(data, 1)

        logger.info("Wrote GeoTIFF file: %s (%s)", filepath, data.shape)
        return filepath

    @classmethod
    def to_xyz(cls, filepath: str | Path) -> list[tuple[float, float, float]]:
        """Convert a GeoTIFF to a list of (x, y, z) coordinate tuples.

        Parameters
        ----------
        filepath:
            Path to the GeoTIFF file.

        Returns
        -------
        list[tuple[float, float, float]]
            Each tuple contains ``(x, y, z)`` for one raster cell.
        """
        result = cls.read(filepath)
        data = result["data"]
        bounds = result["bounds"]
        nodata = result["nodata"]
        height, width = data.shape
        min_x, min_y, max_x, max_y = bounds

        x_res = (max_x - min_x) / width
        y_res = (max_y - min_y) / height

        points: list[tuple[float, float, float]] = []
        for row in range(height):
            for col in range(width):
                value = data[row, col]
                if nodata is not None and value == nodata:
                    continue
                x = min_x + (col + 0.5) * x_res
                y = max_y - (row + 0.5) * y_res
                points.append((x, y, float(value)))

        return points

    @classmethod
    def get_metadata(cls, filepath: str | Path) -> dict:
        """Return metadata for a GeoTIFF file.

        Parameters
        ----------
        filepath:
            Path to the GeoTIFF file.

        Returns
        -------
        dict
            Keys: crs, bounds, shape, resolution, nodata, dtype.
        """
        _require_rasterio()
        filepath = Path(filepath)

        with rasterio.open(filepath) as src:
            return {
                "crs": src.crs.to_string() if src.crs else None,
                "bounds": (src.bounds.left, src.bounds.bottom, src.bounds.right, src.bounds.top),
                "shape": (src.height, src.width),
                "resolution": src.res,
                "nodata": src.nodata,
                "dtype": str(src.dtypes[0]),
            }
