"""Deprecated shim. Use digitalmodel.gis.core.crs instead."""
from digitalmodel.gis.core.crs import CRSDefinition, CRSType, detect_utm_zone  # noqa: F401

__all__ = ['CRSDefinition', 'CRSType', 'detect_utm_zone']
