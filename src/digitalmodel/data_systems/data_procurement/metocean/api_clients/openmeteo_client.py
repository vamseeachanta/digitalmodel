# ABOUTME: Open-Meteo marine weather forecast client for global ocean conditions
# ABOUTME: Streaming, date-based queries for 16-day marine weather forecasts

"""
Open-Meteo Client
=================

FREE marine weather forecast API (global coverage).

Data Coverage:
- Temporal: 16-day forecast, hourly resolution
- Spatial: Global ocean, 0.05° × 0.05° grid
- Parameters: Waves (Hs, direction, period), Wind (10m, 80m, 120m), Current

API: https://api.open-meteo.com/v1/marine
Authentication: None (public FREE API)
Rate Limit: None (unlimited, fair use)

Critical: JSON streaming with date-based queries - NO file storage!
"""

import logging
from typing import Dict, Any, Iterator
from datetime import datetime, timedelta
from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class OpenMeteoClient(BaseAPIClient):
    """
    Open-Meteo marine weather forecast client.

    Retrieves 16-day marine forecasts via streaming JSON (date-based queries).

    Example:
        client = OpenMeteoClient()
        for record in client.query_by_date(
            start_date=datetime.now(),
            end_date=datetime.now() + timedelta(days=7),
            location={"lat": 29.5, "lon": -88.5}
        ):
            print(f"Forecast Hs: {record['wave_height']} m")
            # Data discarded after use
    """

    def __init__(self, **kwargs):
        """
        Initialize Open-Meteo client.

        No authentication required (FREE public API).
        """
        auth_config = {'method': 'none'}

        super().__init__(
            base_url="https://api.open-meteo.com/v1",
            auth_config=auth_config,
            rate_limit=None,  # Unlimited (fair use)
            **kwargs
        )

        logger.info("Initialized OpenMeteoClient (Marine Forecast)")

    def query_by_date(self, start_date: datetime, end_date: datetime,
                      location: Dict[str, float],
                      parameters: list = None,
                      **kwargs) -> Iterator[Dict[str, Any]]:
        """
        Query Open-Meteo marine forecast for date range (streaming, zero storage).

        Args:
            start_date: Start date/time
            end_date: End date/time (max 16 days from now)
            location: Geographic location (lat, lon)
            parameters: Marine parameters to retrieve:
                - "wave_height" (Hs, m)
                - "wave_direction" (degrees)
                - "wave_period" (s)
                - "wind_speed_10m" (m/s)
                - "wind_speed_80m" (m/s)
                - "wind_speed_120m" (m/s)
                - "current_speed" (m/s)
                - "current_direction" (degrees)
            **kwargs: Additional parameters

        Yields:
            Marine forecast records (streaming, not stored)

        Note: Open-Meteo provides up to 16-day forecasts only
        """
        parameters = parameters or ["wave_height", "wind_speed_10m"]

        # Map parameter names to Open-Meteo variable names
        variable_map = {
            "wave_height": "wave_height",
            "wave_direction": "wave_direction",
            "wave_period": "wave_period",
            "wind_speed_10m": "wind_speed_10m",
            "wind_speed_80m": "wind_speed_80m",
            "wind_speed_120m": "wind_speed_120m",
            "current_speed": "ocean_current_velocity",
            "current_direction": "ocean_current_direction"
        }

        variables = [variable_map.get(p, p) for p in parameters]

        # Check date range (max 16 days)
        max_forecast_date = datetime.now() + timedelta(days=16)
        if end_date > max_forecast_date:
            logger.warning(f"Open-Meteo supports max 16-day forecast, truncating end_date")
            end_date = max_forecast_date

        logger.info(f"Querying Open-Meteo for {location} from {start_date} to {end_date}")

        # Build query parameters
        params = {
            "latitude": location['lat'],
            "longitude": location['lon'],
            "hourly": ",".join(variables),
            "start_date": start_date.strftime("%Y-%m-%d"),
            "end_date": end_date.strftime("%Y-%m-%d"),
            "timezone": "UTC"
        }

        # Stream JSON response (no file storage)
        yield from self._stream_marine_data(params, parameters)

    def _stream_marine_data(self, params: Dict[str, Any],
                           parameters: list) -> Iterator[Dict[str, Any]]:
        """
        Stream Open-Meteo marine data from API response (in-memory processing).

        Args:
            params: Query parameters
            parameters: Original parameter names

        Yields:
            Parsed marine forecast records (streaming)
        """
        response = self.get("marine", params=params)
        data = response.json()

        # Extract hourly forecast data
        hourly = data.get('hourly', {})
        times = hourly.get('time', [])

        # Convert to records (time series)
        for i, time_str in enumerate(times):
            timestamp = datetime.fromisoformat(time_str)

            record = {
                'timestamp': timestamp,
                'latitude': data['latitude'],
                'longitude': data['longitude']
            }

            # Add forecast parameters
            for param in parameters:
                if param in hourly:
                    record[param] = hourly[param][i]

            yield record

    def get_historical(self, start_date: datetime, end_date: datetime,
                      location: Dict[str, float],
                      **kwargs) -> Iterator[Dict[str, Any]]:
        """
        Get historical marine data (last 2 months only).

        Open-Meteo provides limited historical data (last 2 months).
        For longer historical data, use ERA5Client or NOAAClient.

        Args:
            start_date: Start date
            end_date: End date (max 2 months ago)
            location: Location (lat, lon)

        Yields:
            Historical marine records
        """
        # Open-Meteo historical archive endpoint
        params = {
            "latitude": location['lat'],
            "longitude": location['lon'],
            "hourly": "wave_height,wave_direction,wave_period,wind_speed_10m",
            "start_date": start_date.strftime("%Y-%m-%d"),
            "end_date": end_date.strftime("%Y-%m-%d"),
            "timezone": "UTC"
        }

        response = self.get("marine/archive", params=params)
        data = response.json()

        hourly = data.get('hourly', {})
        times = hourly.get('time', [])

        for i, time_str in enumerate(times):
            timestamp = datetime.fromisoformat(time_str)

            record = {
                'timestamp': timestamp,
                'latitude': data['latitude'],
                'longitude': data['longitude'],
                'wave_height': hourly.get('wave_height', [])[i],
                'wave_direction': hourly.get('wave_direction', [])[i],
                'wave_period': hourly.get('wave_period', [])[i],
                'wind_speed_10m': hourly.get('wind_speed_10m', [])[i]
            }

            yield record
