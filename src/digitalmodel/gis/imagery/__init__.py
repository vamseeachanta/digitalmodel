"""Historical-imagery timelapse pipeline for the digitalmodel GIS package.

Generates property- and neighborhood-scale historical-imagery timelapse
artifacts (GIF/MP4/contact-sheet) for a manifest of geocoded addresses, using
public NAIP/Landsat/Sentinel-2 imagery via the Microsoft Planetary Computer
STAC API (with optional Earth Engine support).

Pipeline stages:
    1. :func:`~digitalmodel.gis.imagery.aoi.prepare_all` -- build AOI buffers.
    2. :func:`~digitalmodel.gis.imagery.stac_client.probe_imagery_access` -- probe source coverage.
    3. :func:`~digitalmodel.gis.imagery.renderer.render_all` -- render artifacts.

See ``examples/gis/imagery-timelapse-2538`` for a worked example (workspace-hub #2538).
"""

from __future__ import annotations

from digitalmodel.gis.imagery.aoi import build_aoi, prepare_all
from digitalmodel.gis.imagery.manifest import (
    AddressRecord,
    ImageryManifest,
    ManifestDefaults,
)
from digitalmodel.gis.imagery.renderer import render_address, render_all
from digitalmodel.gis.imagery.stac_client import (
    probe_from_metadata,
    probe_imagery_access,
    stac_search,
)

__all__ = [
    "AddressRecord",
    "ImageryManifest",
    "ManifestDefaults",
    "build_aoi",
    "prepare_all",
    "probe_from_metadata",
    "probe_imagery_access",
    "stac_search",
    "render_address",
    "render_all",
]
