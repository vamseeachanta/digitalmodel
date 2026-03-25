"""GIS module for cross-application geospatial tools.

Provides CRS handling, coordinate transforms, spatial queries, and
integrations with Blender, QGIS, Google Earth, and Folium for offshore
engineering workflows.

Public API
----------
Core:
    CRSType, CRSDefinition, detect_utm_zone
    CoordinateTransformer, wgs84_to_utm, utm_to_wgs84
    GeoPoint, GeoBoundingBox
    SpatialQuery

I/O:
    GeoJSONHandler, KMLHandler, ShapefileHandler

Layers:
    FeatureLayer, WellLayer

Integrations:
    GoogleEarthExporter, FoliumMapBuilder, PlotlyMapBuilder

Module:
    GISModule  (engine router)
"""

from __future__ import annotations

import logging

from digitalmodel.gis.core.coordinate_transformer import (
    CoordinateTransformer,
    utm_to_wgs84,
    wgs84_to_utm,
)
from digitalmodel.gis.core.crs import CRSDefinition, CRSType, detect_utm_zone
from digitalmodel.gis.core.geometry import GeoBoundingBox, GeoPoint
from digitalmodel.gis.core.spatial_query import SpatialQuery
from digitalmodel.gis.io.geojson_handler import GeoJSONHandler
from digitalmodel.gis.io.kml_handler import KMLHandler
from digitalmodel.gis.io.shapefile_handler import ShapefileHandler
from digitalmodel.gis.layers.feature_layer import FeatureLayer
from digitalmodel.gis.layers.well_layer import WellLayer
from digitalmodel.gis.integrations.google_earth_export import GoogleEarthExporter
from digitalmodel.gis.integrations.plotly_maps import PlotlyMapBuilder

logger = logging.getLogger(__name__)

__all__ = [
    "CRSType",
    "CRSDefinition",
    "CoordinateTransformer",
    "GeoPoint",
    "GeoBoundingBox",
    "GeoJSONHandler",
    "KMLHandler",
    "FeatureLayer",
    "WellLayer",
    "SpatialQuery",
    "ShapefileHandler",
    "GoogleEarthExporter",
    "PlotlyMapBuilder",
    "GISModule",
    "detect_utm_zone",
    "wgs84_to_utm",
    "utm_to_wgs84",
]


class GISModule:
    """Engine-compatible router for the GIS module."""

    def router(self, cfg_base: dict) -> dict:
        """Route GIS configuration to the appropriate handler.

        Parameters
        ----------
        cfg_base:
            Configuration dictionary from the engine.

        Returns
        -------
        dict
            Updated configuration dictionary.
        """
        logger.info("GIS module router invoked")
        return cfg_base
