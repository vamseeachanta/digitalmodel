"""Deprecated shim. Use digitalmodel.gis.integrations.folium_maps instead."""
from digitalmodel.gis.integrations.folium_maps import FoliumMapBuilder, HAS_FOLIUM  # noqa: F401

__all__ = ['FoliumMapBuilder', 'HAS_FOLIUM']
