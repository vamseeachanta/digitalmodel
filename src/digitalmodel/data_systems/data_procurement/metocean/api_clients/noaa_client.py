# ABOUTME: NOAA NDBC buoy data client for US coastal metocean measurements
# ABOUTME: Streaming, date-based queries for real-time and historical buoy data

"""
NOAA NDBC Client
================

FREE buoy measurement data from NOAA National Data Buoy Center.

Data Coverage:
- Temporal: Real-time and historical (1970-present)
- Spatial: US coastal and offshore buoys (~200 stations)
- Parameters: Waves (Hs, Tp, direction), Wind (speed, direction, gust), Pressure, Temp

API: https://www.ndbc.noaa.gov/data/realtime2/
Authentication: None (public data)
Rate Limit: 1000 requests/day (fair use)

Critical: Text file streaming with date-based queries - NO file storage!
"""

import logging
from typing import Dict, Any, Iterator
from datetime import datetime
from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class NOAAClient(BaseAPIClient):
    """
    NOAA NDBC buoy data client.

    Retrieves buoy measurements via streaming text files (date-based queries).

    Example:
        client = NOAAClient()
        for record in client.query_by_date(
            start_date=datetime(2023, 1, 1),
            end_date=datetime(2023, 12, 31),
            station_id="42001"  # Gulf of Mexico buoy
        ):
            print(f"Hs: {record['wave_height']} m")
            # Data discarded after use
    """

    def __init__(self, **kwargs):
        """
        Initialize NOAA NDBC client.

        No authentication required (public data).
        """
        auth_config = {'method': 'none'}

        super().__init__(
            base_url="https://www.ndbc.noaa.gov/data",
            auth_config=auth_config,
            rate_limit={'requests_per_day': 1000},  # Fair use
            **kwargs
        )

        logger.info("Initialized NOAAClient (NDBC)")

    def query_by_date(self, start_date: datetime, end_date: datetime,
                      location: Dict[str, float] = None,
                      station_id: str = None,
                      **kwargs) -> Iterator[Dict[str, Any]]:
        """
        Query NOAA buoy data for date range (streaming, zero storage).

        Args:
            start_date: Start date/time
            end_date: End date/time
            location: Geographic location (lat, lon) - finds nearest buoy
            station_id: Specific buoy station ID (e.g., "42001")
            **kwargs: Additional parameters

        Yields:
            Buoy measurement records (streaming, not stored)

        Note: Must provide either location or station_id
        """
        if not station_id and not location:
            raise ValueError("Must provide either station_id or location")

        # Find nearest buoy if location provided
        if location and not station_id:
            station_id = self._find_nearest_buoy(location)

        logger.info(f"Querying NOAA buoy {station_id} for {start_date} to {end_date}")

        # Determine which files to fetch based on date range
        for year in range(start_date.year, end_date.year + 1):
            # Historical data (yearly files)
            if year < datetime.now().year:
                yield from self._query_historical(station_id, year, start_date, end_date)
            # Real-time data (last 45 days)
            else:
                yield from self._query_realtime(station_id, start_date, end_date)

    def _query_historical(self, station_id: str, year: int,
                         start_date: datetime, end_date: datetime) -> Iterator[Dict[str, Any]]:
        """
        Query historical buoy data (streaming).

        Args:
            station_id: Buoy station ID
            year: Year to query
            start_date: Filter start date
            end_date: Filter end date

        Yields:
            Parsed buoy records
        """
        # Historical standard meteorological data
        endpoint = f"historical/stdmet/{station_id}h{year}.txt"

        try:
            # Stream text file line by line (no storage)
            for line in self.stream_lines(endpoint):
                if line.startswith('#') or line.startswith('YY'):
                    continue  # Skip headers

                record = self._parse_stdmet_line(line)

                if record and start_date <= record['timestamp'] <= end_date:
                    yield record

        except Exception as e:
            logger.warning(f"Failed to fetch {endpoint}: {e}")

    def _query_realtime(self, station_id: str,
                       start_date: datetime, end_date: datetime) -> Iterator[Dict[str, Any]]:
        """
        Query real-time buoy data (streaming).

        Args:
            station_id: Buoy station ID
            start_date: Filter start date
            end_date: Filter end date

        Yields:
            Parsed buoy records
        """
        # Real-time standard meteorological data (last 45 days)
        endpoint = f"realtime2/{station_id}.txt"

        try:
            for line in self.stream_lines(endpoint):
                if line.startswith('#') or line.startswith('YY'):
                    continue

                record = self._parse_stdmet_line(line)

                if record and start_date <= record['timestamp'] <= end_date:
                    yield record

        except Exception as e:
            logger.warning(f"Failed to fetch {endpoint}: {e}")

    def _parse_stdmet_line(self, line: str) -> Dict[str, Any]:
        """
        Parse NOAA standard meteorological data line.

        Format: YY MM DD hh mm WDIR WSPD GST WVHT DPD APD MWD PRES ATMP WTMP DEWP VIS TIDE

        Args:
            line: Data line from NOAA file

        Returns:
            Parsed record dict or None
        """
        parts = line.split()

        if len(parts) < 13:
            return None

        try:
            timestamp = datetime(
                int(parts[0]) if int(parts[0]) > 1900 else 2000 + int(parts[0]),  # YY
                int(parts[1]),  # MM
                int(parts[2]),  # DD
                int(parts[3]),  # hh
                int(parts[4])   # mm
            )

            record = {
                'timestamp': timestamp,
                'wind_direction': float(parts[5]) if parts[5] != 'MM' else None,
                'wind_speed': float(parts[6]) if parts[6] != 'MM' else None,  # m/s
                'wind_gust': float(parts[7]) if parts[7] != 'MM' else None,
                'wave_height': float(parts[8]) if parts[8] != 'MM' else None,  # m (Hs)
                'dominant_wave_period': float(parts[9]) if parts[9] != 'MM' else None,  # s
                'average_wave_period': float(parts[10]) if parts[10] != 'MM' else None,
                'mean_wave_direction': float(parts[11]) if parts[11] != 'MM' else None,
                'pressure': float(parts[12]) if parts[12] != 'MM' else None,  # hPa
            }

            # Additional fields if available
            if len(parts) > 13:
                record['air_temperature'] = float(parts[13]) if parts[13] != 'MM' else None
            if len(parts) > 14:
                record['water_temperature'] = float(parts[14]) if parts[14] != 'MM' else None

            return record

        except (ValueError, IndexError) as e:
            logger.debug(f"Failed to parse line: {line}, error: {e}")
            return None

    def _find_nearest_buoy(self, location: Dict[str, float]) -> str:
        """
        Find nearest NOAA buoy to location.

        Args:
            location: Geographic location (lat, lon)

        Returns:
            Station ID of nearest buoy

        Note: This uses a small subset of major buoys for efficiency.
              For full coverage, query NOAA's station list API.
        """
        # Major Gulf of Mexico and US coastal buoys
        major_buoys = {
            '42001': {'lat': 25.9, 'lon': -89.7, 'name': 'Mid Gulf'},
            '42002': {'lat': 25.8, 'lon': -93.6, 'name': 'West Gulf'},
            '42003': {'lat': 26.0, 'lon': -85.6, 'name': 'East Gulf'},
            '42019': {'lat': 27.9, 'lon': -95.4, 'name': 'Freeport'},
            '42020': {'lat': 26.9, 'lon': -96.7, 'name': 'Corpus Christi'},
            '42035': {'lat': 29.2, 'lon': -94.4, 'name': 'Galveston'},
            '42036': {'lat': 28.5, 'lon': -84.5, 'name': 'West Tampa'},
            '42039': {'lat': 28.8, 'lon': -86.0, 'name': 'Pensacola'},
            '42040': {'lat': 29.2, 'lon': -88.2, 'name': 'Mississippi Canyon'},
        }

        # Find nearest buoy (simple distance calculation)
        min_distance = float('inf')
        nearest_id = None

        for buoy_id, buoy_info in major_buoys.items():
            distance = ((location['lat'] - buoy_info['lat']) ** 2 +
                       (location['lon'] - buoy_info['lon']) ** 2) ** 0.5

            if distance < min_distance:
                min_distance = distance
                nearest_id = buoy_id

        logger.info(f"Nearest buoy to ({location['lat']}, {location['lon']}): "
                   f"{nearest_id} ({major_buoys[nearest_id]['name']})")

        return nearest_id
