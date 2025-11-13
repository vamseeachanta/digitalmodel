# ABOUTME: Metocean data procurement module (waves, wind, current, tides)
# ABOUTME: Implements streaming, date-based queries with zero storage for environmental data

"""
Metocean Data Procurement
==========================

Universal metocean data retrieval via FREE web APIs.

Data Types:
- Wave data (Hs, Tp, Tz, direction, spectrum)
- Wind data (speed, direction, gusts at multiple heights)
- Current data (speed, direction, vertical profile)
- Tide data (water level, surge)
- Bathymetry (water depth)

FREE APIs:
- NOAA NDBC: Buoy measurements (real-time and historical)
- ERA5 Copernicus: Global reanalysis (1940-present)
- Open-Meteo: Marine forecast (16-day)
- GEBCO: Global bathymetry

Architecture:
- Date-based queries (start_date, end_date, location)
- Streaming retrieval (no file storage)
- In-memory processing
- Direct consumption (OrcaFlex, AQWA, Python objects)

CRITICAL: NO data saving (data can be huge and infeasible)
"""

from .client import MetoceanClient
from .api_clients import (
    ERA5Client,
    NOAAClient,
    OpenMeteoClient,
    GEBCOClient,
)

__all__ = [
    "MetoceanClient",
    "ERA5Client",
    "NOAAClient",
    "OpenMeteoClient",
    "GEBCOClient",
]
