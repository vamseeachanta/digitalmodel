"""DataFrame-backed vector layer with CRS support.

Provides :class:`FeatureLayer`, a thin wrapper around a :class:`pandas.DataFrame`
that carries coordinate reference system metadata and offers convenient I/O
methods for GeoJSON and KML/KMZ files as well as coordinate reprojection.
"""

from __future__ import annotations

import logging
from pathlib import Path

import pandas as pd

from digitalmodel.modules.gis.core.coordinate_transformer import CoordinateTransformer
from digitalmodel.modules.gis.core.crs import CRSDefinition, CRSType
from digitalmodel.modules.gis.core.geometry import GeoBoundingBox, GeoPoint
from digitalmodel.modules.gis.io.geojson_handler import GeoJSONHandler
from digitalmodel.modules.gis.io.kml_handler import KMLHandler

logger = logging.getLogger(__name__)


class FeatureLayer:
    """A vector layer backed by a pandas DataFrame with CRS metadata.

    Parameters
    ----------
    data:
        DataFrame containing at least longitude and latitude columns.
    crs:
        Coordinate reference system definition for the layer.
    name:
        Human-readable layer name.
    lon_col:
        Name of the column holding longitude (or easting) values.
    lat_col:
        Name of the column holding latitude (or northing) values.
    """

    def __init__(
        self,
        data: pd.DataFrame,
        crs: CRSDefinition,
        name: str = "layer",
        lon_col: str = "longitude",
        lat_col: str = "latitude",
    ) -> None:
        self._data = data
        self._crs = crs
        self._name = name
        self._lon_col = lon_col
        self._lat_col = lat_col

    # ------------------------------------------------------------------
    # Properties
    # ------------------------------------------------------------------

    @property
    def data(self) -> pd.DataFrame:
        """The underlying feature data."""
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
    def lon_col(self) -> str:
        """Name of the longitude / easting column."""
        return self._lon_col

    @property
    def lat_col(self) -> str:
        """Name of the latitude / northing column."""
        return self._lat_col

    @property
    def bounds(self) -> GeoBoundingBox:
        """Compute the axis-aligned bounding box from the coordinate columns."""
        return GeoBoundingBox(
            min_x=float(self._data[self._lon_col].min()),
            min_y=float(self._data[self._lat_col].min()),
            max_x=float(self._data[self._lon_col].max()),
            max_y=float(self._data[self._lat_col].max()),
        )

    @property
    def centroid(self) -> GeoPoint:
        """Compute the centroid (mean position) of all features."""
        return GeoPoint(
            x=float(self._data[self._lon_col].mean()),
            y=float(self._data[self._lat_col].mean()),
        )

    # ------------------------------------------------------------------
    # Dunder methods
    # ------------------------------------------------------------------

    def __len__(self) -> int:
        """Return the number of features in this layer."""
        return len(self._data)

    def __repr__(self) -> str:
        return (
            f"FeatureLayer(name={self._name!r}, "
            f"features={len(self)}, "
            f"crs={self._crs.crs_type.value})"
        )

    # ------------------------------------------------------------------
    # Class methods (factory constructors)
    # ------------------------------------------------------------------

    @classmethod
    def from_geojson(cls, file_path: str | Path) -> FeatureLayer:
        """Load a FeatureLayer from a GeoJSON file.

        Assumes the file uses WGS 84 (EPSG:4326) coordinates.

        Parameters
        ----------
        file_path:
            Path to the GeoJSON file.

        Returns
        -------
        FeatureLayer
            A new layer with data parsed from the file.
        """
        geojson = GeoJSONHandler.read(file_path)
        features = geojson.get("features", [])
        df = GeoJSONHandler.features_to_dataframe(features)
        name = Path(file_path).stem
        return cls(data=df, crs=CRSDefinition.wgs84(), name=name)

    @classmethod
    def from_kml(cls, file_path: str | Path) -> FeatureLayer:
        """Load a FeatureLayer from a KML or KMZ file.

        Assumes the file uses WGS 84 (EPSG:4326) coordinates.

        Parameters
        ----------
        file_path:
            Path to the KML or KMZ file.

        Returns
        -------
        FeatureLayer
            A new layer with data parsed from the file.
        """
        features = KMLHandler.read(file_path)
        df = KMLHandler.features_to_dataframe(features)
        name = Path(file_path).stem
        return cls(data=df, crs=CRSDefinition.wgs84(), name=name)

    @classmethod
    def from_dataframe(
        cls,
        df: pd.DataFrame,
        crs: CRSDefinition | None = None,
        lon_col: str = "longitude",
        lat_col: str = "latitude",
    ) -> FeatureLayer:
        """Create a FeatureLayer from an existing DataFrame.

        Parameters
        ----------
        df:
            Source DataFrame with coordinate columns.
        crs:
            Coordinate reference system.  Defaults to WGS 84 if ``None``.
        lon_col:
            Name of the longitude / easting column.
        lat_col:
            Name of the latitude / northing column.

        Returns
        -------
        FeatureLayer
            A new layer wrapping the provided DataFrame.
        """
        if crs is None:
            crs = CRSDefinition.wgs84()
        return cls(data=df, crs=crs, lon_col=lon_col, lat_col=lat_col)

    # ------------------------------------------------------------------
    # Export methods
    # ------------------------------------------------------------------

    def to_geojson(self, file_path: str | Path) -> Path:
        """Export this layer to a GeoJSON file.

        Parameters
        ----------
        file_path:
            Destination file path.

        Returns
        -------
        Path
            The resolved path of the written file.
        """
        features = GeoJSONHandler.dataframe_to_features(
            self._data,
            lon_col=self._lon_col,
            lat_col=self._lat_col,
        )
        collection = GeoJSONHandler.create_feature_collection(features)
        return GeoJSONHandler.write(collection, file_path)

    def to_kml(self, file_path: str | Path) -> Path:
        """Export this layer to a KML file.

        Parameters
        ----------
        file_path:
            Destination file path.

        Returns
        -------
        Path
            The resolved path of the written file.
        """
        features = KMLHandler.dataframe_to_features(
            self._data,
            lon_col=self._lon_col,
            lat_col=self._lat_col,
        )
        return KMLHandler.write_kml(features, file_path, document_name=self._name)

    # ------------------------------------------------------------------
    # Transformation methods
    # ------------------------------------------------------------------

    def reproject(self, target_crs: CRSDefinition) -> FeatureLayer:
        """Return a new FeatureLayer with coordinates transformed to *target_crs*.

        The transformer converts the existing longitude/latitude columns and
        writes the results into ``'longitude'`` and ``'latitude'`` columns in
        the new DataFrame.

        Parameters
        ----------
        target_crs:
            The destination coordinate reference system.

        Returns
        -------
        FeatureLayer
            A new layer with reprojected coordinates.
        """
        transformer = CoordinateTransformer(self._crs, target_crs)
        new_xs, new_ys = transformer.transform_points(
            self._data[self._lon_col].tolist(),
            self._data[self._lat_col].tolist(),
        )

        new_data = self._data.copy()
        new_data["longitude"] = new_xs
        new_data["latitude"] = new_ys

        return FeatureLayer(
            data=new_data,
            crs=target_crs,
            name=self._name,
            lon_col="longitude",
            lat_col="latitude",
        )

    def filter(self, mask: pd.Series) -> FeatureLayer:
        """Return a new FeatureLayer containing only rows where *mask* is True.

        Parameters
        ----------
        mask:
            Boolean Series aligned with :attr:`data`.

        Returns
        -------
        FeatureLayer
            A filtered copy of this layer.
        """
        filtered = self._data.loc[mask].reset_index(drop=True)
        return FeatureLayer(
            data=filtered,
            crs=self._crs,
            name=self._name,
            lon_col=self._lon_col,
            lat_col=self._lat_col,
        )

    def to_geo_points(self) -> list[GeoPoint]:
        """Convert each row to a :class:`GeoPoint`.

        Returns
        -------
        list[GeoPoint]
            One ``GeoPoint`` per row, using the configured lon/lat columns.
        """
        points: list[GeoPoint] = []
        for _, row in self._data.iterrows():
            points.append(
                GeoPoint(
                    x=float(row[self._lon_col]),
                    y=float(row[self._lat_col]),
                )
            )
        return points
