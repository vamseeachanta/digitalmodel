"""Specialized FeatureLayer for offshore well data.

Provides :class:`WellLayer`, a domain-specific subclass of :class:`FeatureLayer`
with well-centric filtering, statistics, and proximity search capabilities
tailored for offshore engineering workflows.
"""

from __future__ import annotations

import logging
from pathlib import Path

import pandas as pd

from digitalmodel.gis.core.crs import CRSDefinition
from digitalmodel.gis.core.geometry import GeoPoint
from digitalmodel.gis.io.geojson_handler import GeoJSONHandler
from digitalmodel.gis.io.kml_handler import KMLHandler
from digitalmodel.gis.layers.feature_layer import FeatureLayer

logger = logging.getLogger(__name__)


class WellLayer(FeatureLayer):
    """A FeatureLayer subclass with well-specific functionality for offshore engineering.

    Parameters
    ----------
    data:
        DataFrame containing well records with at least longitude and latitude columns.
    crs:
        Coordinate reference system.  Defaults to WGS 84 when ``None``.
    name:
        Human-readable layer name.
    lon_col:
        Name of the column holding longitude values.
    lat_col:
        Name of the column holding latitude values.
    """

    def __init__(
        self,
        data: pd.DataFrame,
        crs: CRSDefinition | None = None,
        name: str = "wells",
        lon_col: str = "longitude",
        lat_col: str = "latitude",
    ) -> None:
        if crs is None:
            crs = CRSDefinition.wgs84()
        super().__init__(data=data, crs=crs, name=name, lon_col=lon_col, lat_col=lat_col)

    # ------------------------------------------------------------------
    # Well-specific properties
    # ------------------------------------------------------------------

    @property
    def well_count(self) -> int:
        """Number of wells in this layer."""
        return len(self._data)

    @property
    def fields(self) -> list[str]:
        """Unique field names from the ``'field'`` column.

        Returns an empty list when the column is absent.
        """
        if "field" not in self._data.columns:
            return []
        return sorted(self._data["field"].dropna().unique().tolist())

    @property
    def statuses(self) -> dict[str, int]:
        """Count of wells per status from the ``'status'`` column.

        Returns an empty dict when the column is absent.
        """
        if "status" not in self._data.columns:
            return {}
        return dict(self._data["status"].value_counts())

    # ------------------------------------------------------------------
    # Dunder overrides
    # ------------------------------------------------------------------

    def __repr__(self) -> str:
        return (
            f"WellLayer(name={self._name!r}, "
            f"wells={len(self)}, "
            f"crs={self._crs.crs_type.value})"
        )

    # ------------------------------------------------------------------
    # Factory class methods
    # ------------------------------------------------------------------

    @classmethod
    def from_geojson(cls, file_path: str | Path, name: str = "wells") -> WellLayer:
        """Load a WellLayer from a GeoJSON file.

        Parameters
        ----------
        file_path:
            Path to the GeoJSON file.
        name:
            Layer name.  Defaults to ``'wells'``.

        Returns
        -------
        WellLayer
            A new well layer with data parsed from the file.
        """
        geojson = GeoJSONHandler.read(file_path)
        features = geojson.get("features", [])
        df = GeoJSONHandler.features_to_dataframe(features)
        return cls(data=df, crs=CRSDefinition.wgs84(), name=name)

    @classmethod
    def from_kml(cls, file_path: str | Path, name: str = "wells") -> WellLayer:
        """Load a WellLayer from a KML or KMZ file.

        Parameters
        ----------
        file_path:
            Path to the KML or KMZ file.
        name:
            Layer name.  Defaults to ``'wells'``.

        Returns
        -------
        WellLayer
            A new well layer with data parsed from the file.
        """
        features = KMLHandler.read(file_path)
        df = KMLHandler.features_to_dataframe(features)
        return cls(data=df, crs=CRSDefinition.wgs84(), name=name)

    # ------------------------------------------------------------------
    # Well-specific filter methods
    # ------------------------------------------------------------------

    def filter_by_field(self, field_name: str) -> WellLayer:
        """Filter wells to a specific field name.

        Parameters
        ----------
        field_name:
            The field name to filter on (matched against the ``'field'`` column).

        Returns
        -------
        WellLayer
            A new layer containing only wells in the specified field.

        Raises
        ------
        ValueError
            If the ``'field'`` column is not present in the data.
        """
        if "field" not in self._data.columns:
            raise ValueError("Data does not contain a 'field' column")
        mask = self._data["field"] == field_name
        filtered = self._data.loc[mask].reset_index(drop=True)
        return WellLayer(
            data=filtered, crs=self._crs, name=self._name,
            lon_col=self._lon_col, lat_col=self._lat_col,
        )

    def filter_by_status(self, status: str) -> WellLayer:
        """Filter wells by status.

        Parameters
        ----------
        status:
            The status value to filter on (e.g. ``"active"``, ``"decommissioned"``).

        Returns
        -------
        WellLayer
            A new layer containing only wells with the specified status.

        Raises
        ------
        ValueError
            If the ``'status'`` column is not present in the data.
        """
        if "status" not in self._data.columns:
            raise ValueError("Data does not contain a 'status' column")
        mask = self._data["status"] == status
        filtered = self._data.loc[mask].reset_index(drop=True)
        return WellLayer(
            data=filtered, crs=self._crs, name=self._name,
            lon_col=self._lon_col, lat_col=self._lat_col,
        )

    def filter_by_water_depth(
        self,
        min_depth: float = 0,
        max_depth: float = float("inf"),
    ) -> WellLayer:
        """Filter wells by water depth range.

        Parameters
        ----------
        min_depth:
            Minimum water depth in meters (inclusive).
        max_depth:
            Maximum water depth in meters (inclusive).

        Returns
        -------
        WellLayer
            A new layer containing only wells within the depth range.

        Raises
        ------
        ValueError
            If the ``'water_depth_m'`` column is not present in the data.
        """
        if "water_depth_m" not in self._data.columns:
            raise ValueError("Data does not contain a 'water_depth_m' column")
        mask = (self._data["water_depth_m"] >= min_depth) & (
            self._data["water_depth_m"] <= max_depth
        )
        filtered = self._data.loc[mask].reset_index(drop=True)
        return WellLayer(
            data=filtered, crs=self._crs, name=self._name,
            lon_col=self._lon_col, lat_col=self._lat_col,
        )

    def wells_within_radius(
        self,
        origin_lon: float,
        origin_lat: float,
        radius_m: float,
    ) -> WellLayer:
        """Find wells within a radius of a geographic point.

        Uses :class:`~digitalmodel.gis.core.spatial_query.SpatialQuery`
        internally for distance calculations.

        Parameters
        ----------
        origin_lon:
            Longitude of the search origin.
        origin_lat:
            Latitude of the search origin.
        radius_m:
            Search radius in meters.

        Returns
        -------
        WellLayer
            A new layer containing only wells within the specified radius.
        """
        from digitalmodel.gis.core.spatial_query import SpatialQuery

        origin = GeoPoint(x=origin_lon, y=origin_lat)
        keep: list[bool] = []
        for _, row in self._data.iterrows():
            pt = GeoPoint(x=float(row[self._lon_col]), y=float(row[self._lat_col]))
            keep.append(origin.distance_to(pt) <= radius_m)

        mask = pd.Series(keep, index=self._data.index)
        filtered = self._data.loc[mask].reset_index(drop=True)
        return WellLayer(
            data=filtered, crs=self._crs, name=self._name,
            lon_col=self._lon_col, lat_col=self._lat_col,
        )

    # ------------------------------------------------------------------
    # Summary
    # ------------------------------------------------------------------

    def summary(self) -> dict:
        """Return a summary dictionary of this well layer.

        Returns
        -------
        dict
            Contains ``well_count``, ``fields``, ``statuses``, ``bounds``,
            and ``centroid``.
        """
        return {
            "well_count": self.well_count,
            "fields": self.fields,
            "statuses": self.statuses,
            "bounds": self.bounds,
            "centroid": self.centroid,
        }
