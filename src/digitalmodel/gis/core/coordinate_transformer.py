from __future__ import annotations

import pandas as pd
import pyproj

from .crs import CRSDefinition, detect_utm_zone


class CoordinateTransformer:
    """Coordinate transformation between two CRS using pyproj.

    Parameters
    ----------
    source_crs : CRSDefinition
        The source coordinate reference system.
    target_crs : CRSDefinition
        The target coordinate reference system.
    """

    def __init__(self, source_crs: CRSDefinition, target_crs: CRSDefinition) -> None:
        self._source_crs = source_crs
        self._target_crs = target_crs
        src = source_crs.to_pyproj() if isinstance(source_crs, CRSDefinition) else source_crs
        tgt = target_crs.to_pyproj() if isinstance(target_crs, CRSDefinition) else target_crs
        self._transformer = pyproj.Transformer.from_crs(
            src, tgt, always_xy=True
        )

    @property
    def source_crs(self) -> CRSDefinition:
        """The source coordinate reference system."""
        return self._source_crs

    @property
    def target_crs(self) -> CRSDefinition:
        """The target coordinate reference system."""
        return self._target_crs

    def transform_point(
        self, x: float, y: float, z: float | None = None
    ) -> tuple:
        """Transform a single point from source CRS to target CRS.

        Parameters
        ----------
        x : float
            The x coordinate (longitude or easting).
        y : float
            The y coordinate (latitude or northing).
        z : float | None
            Optional z coordinate (elevation).

        Returns
        -------
        tuple
            Transformed coordinates as (x, y) or (x, y, z) if z was provided.
        """
        if z is not None:
            return self._transformer.transform(x, y, z)
        return self._transformer.transform(x, y)

    def transform_points(
        self,
        xs: list[float],
        ys: list[float],
        zs: list[float] | None = None,
    ) -> tuple:
        """Transform arrays of points from source CRS to target CRS.

        Parameters
        ----------
        xs : list[float]
            The x coordinates (longitudes or eastings).
        ys : list[float]
            The y coordinates (latitudes or northings).
        zs : list[float] | None
            Optional z coordinates (elevations).

        Returns
        -------
        tuple
            Transformed coordinate arrays as (xs, ys) or (xs, ys, zs).
        """
        if zs is not None:
            return self._transformer.transform(xs, ys, zs)
        return self._transformer.transform(xs, ys)

    def transform_dataframe(
        self,
        df: pd.DataFrame,
        x_col: str,
        y_col: str,
        z_col: str | None = None,
        prefix: str = "transformed_",
    ) -> pd.DataFrame:
        """Transform coordinate columns in a DataFrame.

        Adds new columns with the given prefix containing the transformed
        coordinates. The original columns are preserved.

        Parameters
        ----------
        df : pd.DataFrame
            Input DataFrame containing coordinate columns.
        x_col : str
            Name of the column with x coordinates.
        y_col : str
            Name of the column with y coordinates.
        z_col : str | None
            Optional name of the column with z coordinates.
        prefix : str
            Prefix for the new transformed coordinate columns.

        Returns
        -------
        pd.DataFrame
            DataFrame with additional transformed coordinate columns.
        """
        result = df.copy()

        if z_col is not None:
            tx, ty, tz = self._transformer.transform(
                df[x_col].values, df[y_col].values, df[z_col].values
            )
            result[f"{prefix}{x_col}"] = tx
            result[f"{prefix}{y_col}"] = ty
            result[f"{prefix}{z_col}"] = tz
        else:
            tx, ty = self._transformer.transform(
                df[x_col].values, df[y_col].values
            )
            result[f"{prefix}{x_col}"] = tx
            result[f"{prefix}{y_col}"] = ty

        return result


def _auto_detect_utm_zone(longitude: float, latitude: float) -> int:
    """Determine the UTM zone number from geographic coordinates.

    Parameters
    ----------
    longitude : float
        Longitude in decimal degrees (-180 to 180).
    latitude : float
        Latitude in decimal degrees (-90 to 90).

    Returns
    -------
    int
        UTM zone number (1-60).
    """
    zone, _ = detect_utm_zone(longitude, latitude)
    return zone


def wgs84_to_utm(
    longitude: float,
    latitude: float,
    zone: int | None = None,
) -> tuple[float, float, int, str]:
    """Convert WGS84 geographic coordinates to UTM projection.

    Parameters
    ----------
    longitude : float
        Longitude in decimal degrees.
    latitude : float
        Latitude in decimal degrees.
    zone : int | None
        UTM zone number. Auto-detected from longitude if not provided.

    Returns
    -------
    tuple[float, float, int, str]
        (easting, northing, zone, hemisphere) where hemisphere is 'N' or 'S'.
    """
    if zone is None:
        zone = _auto_detect_utm_zone(longitude, latitude)

    if not 1 <= zone <= 60:
        raise ValueError(f"UTM zone must be 1-60, got {zone}")

    hemisphere = "N" if latitude >= 0 else "S"

    source_crs = "EPSG:4326"
    epsg_code = 32600 + zone if hemisphere == "N" else 32700 + zone
    target_crs = f"EPSG:{epsg_code}"

    transformer = pyproj.Transformer.from_crs(
        source_crs, target_crs, always_xy=True
    )
    easting, northing = transformer.transform(longitude, latitude)

    return easting, northing, zone, hemisphere


def utm_to_wgs84(
    easting: float,
    northing: float,
    zone: int,
    hemisphere: str = "N",
) -> tuple[float, float]:
    """Convert UTM coordinates to WGS84 geographic coordinates.

    Parameters
    ----------
    easting : float
        UTM easting in metres.
    northing : float
        UTM northing in metres.
    zone : int
        UTM zone number (1-60).
    hemisphere : str
        Hemisphere, either 'N' (default) or 'S'.

    Returns
    -------
    tuple[float, float]
        (longitude, latitude) in decimal degrees.
    """
    hemisphere = hemisphere.upper()
    if hemisphere not in ("N", "S"):
        raise ValueError(f"Hemisphere must be 'N' or 'S', got '{hemisphere}'")
    if not 1 <= zone <= 60:
        raise ValueError(f"UTM zone must be 1-60, got {zone}")

    epsg_code = 32600 + zone if hemisphere == "N" else 32700 + zone
    source_crs = f"EPSG:{epsg_code}"
    target_crs = "EPSG:4326"

    transformer = pyproj.Transformer.from_crs(
        source_crs, target_crs, always_xy=True
    )
    longitude, latitude = transformer.transform(easting, northing)

    return longitude, latitude
