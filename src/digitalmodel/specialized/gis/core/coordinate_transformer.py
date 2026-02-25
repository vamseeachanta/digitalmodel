"""Deprecated shim. Use digitalmodel.gis.core.coordinate_transformer instead."""
from digitalmodel.gis.core.coordinate_transformer import CoordinateTransformer, wgs84_to_utm, utm_to_wgs84  # noqa: F401

__all__ = ['CoordinateTransformer', 'wgs84_to_utm', 'utm_to_wgs84']
