"""Backward-compatible shim for the GIS shapefile handler.

This module preserves the legacy import path used by tests and downstream code
while delegating to :mod:`digitalmodel.gis.io.shapefile_handler`.
"""

from digitalmodel.gis.io import shapefile_handler as _impl

HAS_GEOPANDAS = _impl.HAS_GEOPANDAS


class ShapefileHandler:
    """Compatibility wrapper around the canonical shapefile handler."""

    @classmethod
    def read(cls, filepath):
        if not HAS_GEOPANDAS:
            raise ImportError(_impl._MISSING_MSG)
        return _impl.ShapefileHandler.read(filepath)

    @classmethod
    def write(cls, features, filepath, crs="EPSG:4326"):
        if not HAS_GEOPANDAS:
            raise ImportError(_impl._MISSING_MSG)
        return _impl.ShapefileHandler.write(features, filepath, crs=crs)

    @classmethod
    def features_to_dataframe(cls, features):
        return _impl.ShapefileHandler.features_to_dataframe(features)

    @classmethod
    def dataframe_to_features(cls, df, lon_col="longitude", lat_col="latitude"):
        return _impl.ShapefileHandler.dataframe_to_features(
            df, lon_col=lon_col, lat_col=lat_col
        )


__all__ = ["ShapefileHandler", "HAS_GEOPANDAS"]
