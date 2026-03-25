"""Deprecated shim. Use digitalmodel.gis.io.shapefile_handler instead."""
from digitalmodel.gis.io.shapefile_handler import ShapefileHandler, HAS_GEOPANDAS  # noqa: F401

__all__ = ["ShapefileHandler", "HAS_GEOPANDAS"]
