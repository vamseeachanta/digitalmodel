# ABOUTME: FREE API clients for metocean data procurement
# ABOUTME: ERA5, NOAA NDBC, Open-Meteo, GEBCO - all with date-based streaming queries

"""
Metocean API Clients
====================

FREE API integrations for metocean data.

Clients:
- ERA5Client: Copernicus Climate Data Store (global reanalysis)
- NOAAClient: NOAA NDBC buoy data (US coastal)
- OpenMeteoClient: Marine weather forecast (global)
- GEBCOClient: Global bathymetry database

All clients implement streaming, date-based queries with zero storage.
"""

from .era5_client import ERA5Client
from .noaa_client import NOAAClient
from .openmeteo_client import OpenMeteoClient
from .gebco_client import GEBCOClient

__all__ = [
    "ERA5Client",
    "NOAAClient",
    "OpenMeteoClient",
    "GEBCOClient",
]
