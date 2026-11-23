"""Deprecated shim. Use digitalmodel.gis.io.geotiff_handler instead."""
from digitalmodel.gis.io.geotiff_handler import GeoTIFFHandler, HAS_RASTERIO  # noqa: F401

__all__ = ['GeoTIFFHandler', 'HAS_RASTERIO']
