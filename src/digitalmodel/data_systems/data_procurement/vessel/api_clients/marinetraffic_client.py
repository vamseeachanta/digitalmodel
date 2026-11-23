# ABOUTME: MarineTraffic API client for vessel database queries
# ABOUTME: Streaming, vessel-based queries (IMO/MMSI → ship particulars)

"""
MarineTraffic Client
====================

FREE vessel database API from MarineTraffic.

Data Coverage:
- Database: 800,000+ vessels worldwide
- Data: IMO, MMSI, name, type, dimensions, flag, built year
- Real-time: Current position, speed, course
- Historical: Track history, port calls

API: https://services.marinetraffic.com/api
Authentication: FREE API key (5 requests/minute)
Rate Limit: 5 req/min (FREE tier), 50 req/min (paid)

Critical: JSON streaming with vessel-based queries - NO file storage!
"""

import logging
from typing import Dict, Any, Iterator, Optional
from datetime import datetime
from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class MarineTrafficClient(BaseAPIClient):
    """
    MarineTraffic vessel database client.

    Retrieves vessel data via vessel identifiers (IMO, MMSI, name).

    Example:
        client = MarineTrafficClient(api_key="${MARINETRAFFIC_API_KEY}")

        # Query by IMO
        vessel = client.get_vessel_by_imo("9321483")
        print(f"Vessel: {vessel['name']}, LOA: {vessel['length']}m")

        # Query by name
        vessels = list(client.search_vessels_by_name("Front Altair"))
    """

    def __init__(self, api_key: str, **kwargs):
        """
        Initialize MarineTraffic client.

        Args:
            api_key: MarineTraffic API key (FREE tier: 5 req/min)
                    Get from https://www.marinetraffic.com/en/ais-api-services
            **kwargs: Additional BaseAPIClient arguments
        """
        auth_config = {
            'method': 'api_key',
            'credentials': {'key': api_key},
            'header_name': 'API-Key'
        }

        super().__init__(
            base_url="https://services.marinetraffic.com/api",
            auth_config=auth_config,
            rate_limit={'requests_per_minute': 5},  # FREE tier
            **kwargs
        )

        logger.info("Initialized MarineTrafficClient (FREE tier: 5 req/min)")

    def get_vessel_by_imo(self, imo: str) -> Dict[str, Any]:
        """
        Get vessel data by IMO number.

        Args:
            imo: IMO number (e.g., "9321483")

        Returns:
            Vessel data dict with:
                - imo: IMO number
                - mmsi: MMSI number
                - name: Vessel name
                - type: Vessel type (e.g., "Tanker", "Container Ship")
                - length: Length overall (LOA) in meters
                - beam: Beam in meters
                - draft: Maximum draft in meters
                - dwt: Deadweight tonnage
                - built_year: Year built
                - flag: Flag country

        Example:
            vessel = client.get_vessel_by_imo("9321483")
            print(f"{vessel['name']}: {vessel['length']}m × {vessel['beam']}m")
        """
        # MarineTraffic exportvessel endpoint
        params = {
            'v': 8,
            'protocol': 'json',
            'imo': imo
        }

        response = self.get('exportvessel', params=params)
        data = response.json()

        if not data or 'error' in data:
            raise ValueError(f"Vessel not found for IMO: {imo}")

        # Parse vessel data
        vessel_data = data[0] if isinstance(data, list) else data

        return self._parse_vessel_data(vessel_data)

    def get_vessel_by_mmsi(self, mmsi: str) -> Dict[str, Any]:
        """
        Get vessel data by MMSI number.

        Args:
            mmsi: MMSI number (e.g., "219018314")

        Returns:
            Vessel data dict (same format as get_vessel_by_imo)
        """
        params = {
            'v': 8,
            'protocol': 'json',
            'mmsi': mmsi
        }

        response = self.get('exportvessel', params=params)
        data = response.json()

        if not data or 'error' in data:
            raise ValueError(f"Vessel not found for MMSI: {mmsi}")

        vessel_data = data[0] if isinstance(data, list) else data

        return self._parse_vessel_data(vessel_data)

    def search_vessels_by_name(self, name: str) -> Iterator[Dict[str, Any]]:
        """
        Search vessels by name (streaming results).

        Args:
            name: Vessel name (partial match supported)

        Yields:
            Vessel data dicts (streaming, not stored)

        Example:
            for vessel in client.search_vessels_by_name("Front"):
                print(f"{vessel['name']}: IMO {vessel['imo']}")
        """
        params = {
            'v': 8,
            'protocol': 'json',
            'shipname': name
        }

        response = self.get('exportvessel', params=params)
        data = response.json()

        if not data or 'error' in data:
            logger.warning(f"No vessels found for name: {name}")
            return

        # Stream results
        vessels = data if isinstance(data, list) else [data]

        for vessel_data in vessels:
            yield self._parse_vessel_data(vessel_data)

    def get_vessel_position(self, imo: str) -> Dict[str, Any]:
        """
        Get current vessel position (real-time).

        Args:
            imo: IMO number

        Returns:
            Position data dict:
                - latitude: Current latitude
                - longitude: Current longitude
                - speed: Speed in knots
                - course: Course in degrees
                - timestamp: Position timestamp
                - status: Navigation status

        Note: FREE tier has limited real-time access
        """
        params = {
            'v': 8,
            'protocol': 'json',
            'imo': imo,
            'msgtype': 'extended'
        }

        response = self.get('exportvessel', params=params)
        data = response.json()

        if not data or 'error' in data:
            raise ValueError(f"Position not available for IMO: {imo}")

        vessel_data = data[0] if isinstance(data, list) else data

        return {
            'latitude': float(vessel_data.get('LAT', 0)),
            'longitude': float(vessel_data.get('LON', 0)),
            'speed': float(vessel_data.get('SPEED', 0)),
            'course': float(vessel_data.get('COURSE', 0)),
            'timestamp': vessel_data.get('TIMESTAMP'),
            'status': vessel_data.get('STATUS')
        }

    def _parse_vessel_data(self, vessel_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Parse MarineTraffic vessel data to standardized format.

        Args:
            vessel_data: Raw API response data

        Returns:
            Standardized vessel data dict
        """
        return {
            'imo': vessel_data.get('IMO', vessel_data.get('imo')),
            'mmsi': vessel_data.get('MMSI', vessel_data.get('mmsi')),
            'name': vessel_data.get('SHIPNAME', vessel_data.get('name')),
            'type': vessel_data.get('TYPE_NAME', vessel_data.get('type')),
            'type_code': vessel_data.get('TYPE', vessel_data.get('type_code')),
            'length': float(vessel_data.get('LENGTH', vessel_data.get('length', 0))),
            'beam': float(vessel_data.get('WIDTH', vessel_data.get('beam', 0))),
            'draft': float(vessel_data.get('DRAUGHT', vessel_data.get('draft', 0))),
            'dwt': int(vessel_data.get('DWT', vessel_data.get('dwt', 0))),
            'built_year': int(vessel_data.get('YEAR_BUILT', vessel_data.get('built_year', 0))),
            'flag': vessel_data.get('FLAG', vessel_data.get('flag')),
            'callsign': vessel_data.get('CALLSIGN', vessel_data.get('callsign')),
            'gross_tonnage': int(vessel_data.get('GT', vessel_data.get('gross_tonnage', 0))),
        }

    def query_by_date(self, start_date: datetime, end_date: datetime,
                      location: Dict[str, float] = None,
                      vessel_imo: str = None,
                      **kwargs) -> Iterator[Dict[str, Any]]:
        """
        Query vessel data (not time-dependent, returns single record).

        Vessel characteristics don't change with time, so this returns
        current vessel data.

        Args:
            start_date: Not used (vessel data is static)
            end_date: Not used
            location: Not used for vessel database queries
            vessel_imo: IMO number to query
            **kwargs: Additional parameters

        Yields:
            Single vessel data record

        Note: Implemented for compatibility with base client interface
        """
        if not vessel_imo:
            raise ValueError("vessel_imo required for vessel queries")

        vessel_data = self.get_vessel_by_imo(vessel_imo)

        # Add timestamp for consistency
        vessel_data['timestamp'] = datetime.now()

        yield vessel_data
