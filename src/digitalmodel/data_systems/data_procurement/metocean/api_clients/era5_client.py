# ABOUTME: ERA5 Copernicus Climate Data Store client for global reanalysis data
# ABOUTME: Streaming, date-based queries for waves, wind, current (1940-present)

"""
ERA5 Client
===========

FREE global reanalysis data from Copernicus Climate Data Store.

Data Coverage:
- Temporal: 1940-present, hourly resolution
- Spatial: Global, 0.25° × 0.25° grid
- Parameters: Waves (Hs, Tp, direction), Wind (10m, 100m), Current (surface)

API: https://cds.climate.copernicus.eu/api/v2
Authentication: FREE API key (no credit card)
Rate Limit: Fair use (no hard limits)

Critical: Streaming retrieval with date-based queries - NO file storage!
"""

import logging
from typing import Dict, Any, Iterator
from datetime import datetime, timedelta
from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class ERA5Client(BaseAPIClient):
    """
    ERA5 Copernicus Climate Data Store client.

    Retrieves global metocean data via streaming, date-based queries.

    Example:
        client = ERA5Client(api_key="${ERA5_API_KEY}")
        for record in client.query_by_date(
            start_date=datetime(2020, 1, 1),
            end_date=datetime(2020, 12, 31),
            location={"lat": 29.5, "lon": -88.5},
            parameters=["wave_height", "wind_speed_10m"]
        ):
            # Process record in-memory (not saved)
            process_metocean(record)
    """

    def __init__(self, api_key: str, **kwargs):
        """
        Initialize ERA5 client.

        Args:
            api_key: Copernicus API key (FREE, get from https://cds.climate.copernicus.eu)
            **kwargs: Additional BaseAPIClient arguments
        """
        auth_config = {
            'method': 'api_key',
            'credentials': {'key': api_key},
            'header_name': 'X-API-Key'
        }

        super().__init__(
            base_url="https://cds.climate.copernicus.eu/api/v2",
            auth_config=auth_config,
            rate_limit=None,  # Fair use (no hard limits)
            **kwargs
        )

        logger.info("Initialized ERA5Client (Copernicus CDS)")

    def query_by_date(self, start_date: datetime, end_date: datetime,
                      location: Dict[str, float],
                      parameters: list = None,
                      **kwargs) -> Iterator[Dict[str, Any]]:
        """
        Query ERA5 data for date range (streaming, zero storage).

        Args:
            start_date: Start date/time
            end_date: End date/time
            location: Geographic location (lat, lon)
            parameters: Data parameters to retrieve:
                - "wave_height" (significant wave height, m)
                - "wave_period" (peak period, s)
                - "wave_direction" (mean direction, degrees)
                - "wind_speed_10m" (wind at 10m, m/s)
                - "wind_speed_100m" (wind at 100m, m/s)
                - "current_speed" (surface current, m/s)
                - "current_direction" (current direction, degrees)
            **kwargs: Additional parameters

        Yields:
            Metocean data records (streaming, not stored)

        Example:
            for record in client.query_by_date(...):
                print(f"Hs: {record['wave_height']} m")
                # Data discarded after use (retrieve again if needed)
        """
        parameters = parameters or ["wave_height", "wind_speed_10m"]

        # Map parameter names to ERA5 variable names
        variable_map = {
            "wave_height": "significant_height_of_combined_wind_waves_and_swell",
            "wave_period": "mean_wave_period",
            "wave_direction": "mean_wave_direction",
            "wind_speed_10m": "10m_u_component_of_wind",  # Also need v component
            "wind_speed_100m": "100m_u_component_of_wind",
            "current_speed": "surface_ocean_current_speed",
            "current_direction": "surface_ocean_current_direction"
        }

        variables = [variable_map.get(p, p) for p in parameters]

        # ERA5 uses monthly chunks for efficient retrieval
        for chunk_start, chunk_end in self._split_date_range(start_date, end_date, days=30):
            logger.info(f"Querying ERA5 for {chunk_start} to {chunk_end}")

            # Build request payload
            payload = {
                "product_type": "reanalysis",
                "format": "netcdf",
                "variable": variables,
                "year": [str(d.year) for d in self._date_range(chunk_start, chunk_end, step_days=1)],
                "month": [f"{d.month:02d}" for d in self._date_range(chunk_start, chunk_end, step_days=1)],
                "day": [f"{d.day:02d}" for d in self._date_range(chunk_start, chunk_end, step_days=1)],
                "time": [f"{h:02d}:00" for h in range(24)],  # Hourly
                "area": [
                    location['lat'] + 0.5,  # North
                    location['lon'] - 0.5,  # West
                    location['lat'] - 0.5,  # South
                    location['lon'] + 0.5   # East
                ]
            }

            # Stream data chunks (no file storage)
            yield from self._stream_era5_data(payload, parameters)

    def _stream_era5_data(self, payload: Dict[str, Any],
                          parameters: list) -> Iterator[Dict[str, Any]]:
        """
        Stream ERA5 data from API response (in-memory processing).

        Args:
            payload: ERA5 API request payload
            parameters: Original parameter names

        Yields:
            Parsed metocean records (streaming)
        """
        # Submit request
        response = self.post("resources", json_data=payload)
        request_id = response.json()['request_id']

        logger.info(f"ERA5 request submitted: {request_id}")

        # Poll for completion
        import time
        while True:
            status_response = self.get(f"tasks/{request_id}")
            status = status_response.json()

            if status['state'] == 'completed':
                download_url = status['location']
                break
            elif status['state'] == 'failed':
                raise RuntimeError(f"ERA5 request failed: {status.get('error')}")
            else:
                logger.debug(f"ERA5 request status: {status['state']}")
                time.sleep(5)  # Wait before polling again

        # Stream NetCDF data (in-memory, no file saving)
        logger.info(f"Streaming ERA5 data from {download_url}")

        netcdf_data = self.get(download_url, stream=True)

        # Parse NetCDF in-memory
        from ...common.stream_handler import FormatParser

        parsed_data = FormatParser.parse_netcdf_stream(netcdf_data.content)

        # Convert to records
        for record in self._netcdf_to_records(parsed_data, parameters):
            yield record

    def _netcdf_to_records(self, parsed_data: Dict[str, Any],
                           parameters: list) -> Iterator[Dict[str, Any]]:
        """
        Convert parsed NetCDF to metocean records.

        Args:
            parsed_data: Parsed NetCDF data dict
            parameters: Parameter names

        Yields:
            Metocean records
        """
        # Extract time, lat, lon coordinates
        times = parsed_data['coords']['time']
        lats = parsed_data['coords']['latitude']
        lons = parsed_data['coords']['longitude']

        # Extract variables
        variables = parsed_data['variables']

        # Generate records (time series at single location)
        for i, time_value in enumerate(times):
            record = {
                'timestamp': self._convert_era5_time(time_value),
                'latitude': float(lats[0]),  # Single point
                'longitude': float(lons[0])
            }

            # Add parameters
            for param in parameters:
                if param in variables:
                    record[param] = float(variables[param][i, 0, 0])  # [time, lat, lon]

            yield record

    def _convert_era5_time(self, time_value: Any) -> datetime:
        """Convert ERA5 time value to datetime."""
        # ERA5 uses hours since 1900-01-01
        import numpy as np
        reference = datetime(1900, 1, 1)
        return reference + timedelta(hours=float(time_value))

    def _split_date_range(self, start_date: datetime, end_date: datetime,
                         days: int = 30) -> Iterator[tuple]:
        """
        Split date range into chunks (for efficient API retrieval).

        Args:
            start_date: Start date
            end_date: End date
            days: Days per chunk

        Yields:
            (chunk_start, chunk_end) tuples
        """
        current = start_date

        while current < end_date:
            chunk_end = min(current + timedelta(days=days), end_date)
            yield (current, chunk_end)
            current = chunk_end

    def _date_range(self, start_date: datetime, end_date: datetime,
                   step_days: int = 1) -> Iterator[datetime]:
        """Generate date range."""
        current = start_date

        while current <= end_date:
            yield current
            current += timedelta(days=step_days)
