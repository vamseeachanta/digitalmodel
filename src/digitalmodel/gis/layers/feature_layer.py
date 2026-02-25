"""FeatureLayer: a pandas-backed tabular GIS feature collection.

Provides a lightweight wrapper around a :class:`pandas.DataFrame` that
adds spatial metadata (coordinate column names, bounding box, centroid)
and GeoJSON import/export without introducing heavy external dependencies.
"""

from __future__ import annotations

from typing import Optional

import pandas as pd

from digitalmodel.gis.core.geometry import GeoBoundingBox, GeoPoint


class FeatureLayer:
    """A tabular collection of geospatial features backed by a DataFrame.

    Parameters
    ----------
    data:
        DataFrame containing at minimum longitude and latitude columns.
    name:
        Human-readable name for this layer.
    lon_col:
        Name of the longitude column (default ``"longitude"``).
    lat_col:
        Name of the latitude column (default ``"latitude"``).

    Raises
    ------
    KeyError
        If *lon_col* or *lat_col* are not present in *data*.
    """

    def __init__(
        self,
        data: pd.DataFrame,
        name: str,
        lon_col: str = "longitude",
        lat_col: str = "latitude",
    ) -> None:
        if lon_col not in data.columns:
            raise KeyError(
                f"Longitude column '{lon_col}' not found in DataFrame. "
                f"Available columns: {list(data.columns)}"
            )
        if lat_col not in data.columns:
            raise KeyError(
                f"Latitude column '{lat_col}' not found in DataFrame. "
                f"Available columns: {list(data.columns)}"
            )

        self._data = data.copy().reset_index(drop=True)
        self._name = name
        self._lon_col = lon_col
        self._lat_col = lat_col

    # ------------------------------------------------------------------
    # Properties
    # ------------------------------------------------------------------

    @property
    def data(self) -> pd.DataFrame:
        """The underlying DataFrame (defensive copy)."""
        return self._data.copy()

    @property
    def name(self) -> str:
        """Human-readable layer name."""
        return self._name

    @property
    def lon_col(self) -> str:
        """Name of the longitude column."""
        return self._lon_col

    @property
    def lat_col(self) -> str:
        """Name of the latitude column."""
        return self._lat_col

    @property
    def centroid(self) -> GeoPoint:
        """Return the centroid as the mean of all point coordinates."""
        lon = float(self._data[self._lon_col].mean())
        lat = float(self._data[self._lat_col].mean())
        return GeoPoint(x=lon, y=lat)

    @property
    def bounding_box(self) -> GeoBoundingBox:
        """Return the axis-aligned bounding box that encloses all features."""
        return GeoBoundingBox(
            min_x=float(self._data[self._lon_col].min()),
            min_y=float(self._data[self._lat_col].min()),
            max_x=float(self._data[self._lon_col].max()),
            max_y=float(self._data[self._lat_col].max()),
        )

    def __len__(self) -> int:
        return len(self._data)

    def __repr__(self) -> str:
        return (
            f"{self.__class__.__name__}("
            f"name={self._name!r}, n={len(self)}, "
            f"lon_col={self._lon_col!r}, lat_col={self._lat_col!r})"
        )

    # ------------------------------------------------------------------
    # Filtering
    # ------------------------------------------------------------------

    def filter(self, mask: pd.Series) -> FeatureLayer:
        """Return a new layer containing only rows where *mask* is ``True``.

        Parameters
        ----------
        mask:
            Boolean :class:`pandas.Series` aligned to this layer's index.

        Returns
        -------
        FeatureLayer
            New layer with the same name and column configuration.
        """
        filtered_df = self._data.loc[mask].reset_index(drop=True)
        return self.__class__(
            data=filtered_df,
            name=self._name,
            lon_col=self._lon_col,
            lat_col=self._lat_col,
        )

    # ------------------------------------------------------------------
    # GeoJSON import / export
    # ------------------------------------------------------------------

    @classmethod
    def from_geojson(
        cls,
        feature_collection: dict,
        name: str,
        lon_col: str = "longitude",
        lat_col: str = "latitude",
    ) -> FeatureLayer:
        """Construct a FeatureLayer from a GeoJSON FeatureCollection dict.

        Only ``Point`` geometries are extracted; other geometry types are
        ignored.

        Parameters
        ----------
        feature_collection:
            A GeoJSON FeatureCollection dict.
        name:
            Layer name.
        lon_col:
            Column name for extracted longitude values.
        lat_col:
            Column name for extracted latitude values.

        Returns
        -------
        FeatureLayer
        """
        rows: list[dict] = []
        for feature in feature_collection.get("features", []):
            geometry = feature.get("geometry") or {}
            properties = feature.get("properties") or {}

            if geometry.get("type") != "Point":
                continue

            coords = geometry.get("coordinates", [])
            if len(coords) < 2:
                continue

            row = {
                lon_col: float(coords[0]),
                lat_col: float(coords[1]),
                **{k: v for k, v in properties.items()},
            }
            rows.append(row)

        df = pd.DataFrame(rows) if rows else pd.DataFrame(columns=[lon_col, lat_col])
        return cls(data=df, name=name, lon_col=lon_col, lat_col=lat_col)

    def to_geojson(
        self,
        properties_cols: Optional[list[str]] = None,
    ) -> dict:
        """Export this layer as a GeoJSON FeatureCollection dict.

        Parameters
        ----------
        properties_cols:
            Columns to include as feature properties.  ``None`` includes
            all columns other than the coordinate columns.

        Returns
        -------
        dict
            A GeoJSON FeatureCollection.
        """
        if properties_cols is None:
            properties_cols = [
                col
                for col in self._data.columns
                if col not in (self._lon_col, self._lat_col)
            ]

        features: list[dict] = []
        for _, row in self._data.iterrows():
            feature = {
                "type": "Feature",
                "geometry": {
                    "type": "Point",
                    "coordinates": [
                        float(row[self._lon_col]),
                        float(row[self._lat_col]),
                    ],
                },
                "properties": {col: row[col] for col in properties_cols},
            }
            features.append(feature)

        return {"type": "FeatureCollection", "features": features}
