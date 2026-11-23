"""Deprecated shim. Use digitalmodel.gis.core.geometry instead."""
from digitalmodel.gis.core.geometry import GeoPoint, GeoBoundingBox, EARTH_RADIUS_METERS  # noqa: F401

__all__ = ["GeoPoint", "GeoBoundingBox", "EARTH_RADIUS_METERS"]
