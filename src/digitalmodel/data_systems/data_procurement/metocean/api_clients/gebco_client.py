# ABOUTME: GEBCO global bathymetry client for water depth data
# ABOUTME: Streaming, location-based queries for seabed elevation/depth

"""
GEBCO Client
============

FREE global bathymetry (water depth) database.

Data Coverage:
- Spatial: Global, 15 arc-second grid (~450m resolution)
- Data: Seabed elevation (negative = below sea level)
- Source: GEBCO_2023 Grid (latest)

API: https://www.gebco.net/data_and_products/gebco_web_services/web_map_service/
Authentication: None (public data)
Rate Limit: Fair use

Critical: WMS tile streaming with location-based queries - NO file storage!
"""

import logging
from typing import Dict, Any, Iterator
from datetime import datetime
from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class GEBCOClient(BaseAPIClient):
    """
    GEBCO global bathymetry client.

    Retrieves water depth via WMS tiles (streaming, location-based queries).

    Example:
        client = GEBCOClient()
        depth = client.get_depth(lat=29.5, lon=-88.5)
        print(f"Water depth: {-depth} m")  # Negative elevation = depth
    """

    def __init__(self, **kwargs):
        """
        Initialize GEBCO client.

        No authentication required (public data).
        """
        auth_config = {'method': 'none'}

        super().__init__(
            base_url="https://www.gebco.net",
            auth_config=auth_config,
            rate_limit=None,  # Fair use
            **kwargs
        )

        logger.info("Initialized GEBCOClient (Global Bathymetry)")

    def get_depth(self, lat: float, lon: float) -> float:
        """
        Get water depth at location.

        Args:
            lat: Latitude
            lon: Longitude

        Returns:
            Water depth in meters (positive value)
            Returns 0 for land areas

        Note: GEBCO provides elevation (negative = below sea level)
              This method returns positive depth values for convenience
        """
        elevation = self.get_elevation(lat, lon)

        # Convert elevation to depth (positive values)
        return -elevation if elevation < 0 else 0

    def get_elevation(self, lat: float, lon: float) -> float:
        """
        Get seabed elevation at location.

        Args:
            lat: Latitude
            lon: Longitude

        Returns:
            Elevation in meters (negative = below sea level)
        """
        # GEBCO WMS GetFeatureInfo request
        params = {
            'service': 'WMS',
            'version': '1.3.0',
            'request': 'GetFeatureInfo',
            'layers': 'GEBCO_LATEST',
            'query_layers': 'GEBCO_LATEST',
            'info_format': 'text/plain',
            'i': 50,  # Pixel position
            'j': 50,
            'width': 101,
            'height': 101,
            'crs': 'EPSG:4326',
            'bbox': f"{lon-0.1},{lat-0.1},{lon+0.1},{lat+0.1}"
        }

        response = self.get("data_and_products/gebco_web_services/web_map_service/mapserv", params=params)

        # Parse text response
        text = response.text
        for line in text.split('\n'):
            if 'Elevation' in line or 'elevation' in line:
                try:
                    elevation = float(line.split(':')[-1].strip().split()[0])
                    return elevation
                except (ValueError, IndexError):
                    continue

        logger.warning(f"Could not parse GEBCO elevation at ({lat}, {lon})")
        return 0.0

    def query_by_date(self, start_date: datetime, end_date: datetime,
                      location: Dict[str, float],
                      **kwargs) -> Iterator[Dict[str, Any]]:
        """
        Query bathymetry data (single value, not time-dependent).

        Bathymetry doesn't change with time, so this returns a single record.

        Args:
            start_date: Not used (bathymetry is static)
            end_date: Not used
            location: Geographic location (lat, lon)

        Yields:
            Single bathymetry record
        """
        depth = self.get_depth(location['lat'], location['lon'])

        yield {
            'timestamp': datetime.now(),
            'latitude': location['lat'],
            'longitude': location['lon'],
            'water_depth': depth,
            'elevation': -depth
        }

    def get_depth_grid(self, bbox: Dict[str, float],
                      resolution: float = 0.01) -> Iterator[Dict[str, Any]]:
        """
        Get bathymetry grid for bounding box (streaming).

        Args:
            bbox: Bounding box (north, south, east, west in degrees)
            resolution: Grid resolution in degrees (default 0.01° ≈ 1km)

        Yields:
            Grid points with depth values (streaming, not stored)

        Warning: Large bounding boxes with fine resolution can generate
                 many requests. Use wisely to avoid overwhelming the API.
        """
        north, south = bbox['north'], bbox['south']
        east, west = bbox['east'], bbox['west']

        # Generate grid points
        lat = south
        while lat <= north:
            lon = west
            while lon <= east:
                depth = self.get_depth(lat, lon)

                yield {
                    'latitude': lat,
                    'longitude': lon,
                    'water_depth': depth,
                    'elevation': -depth
                }

                lon += resolution

            lat += resolution

            # Rate limiting (don't overwhelm the API)
            import time
            time.sleep(0.1)  # 10 requests/second
