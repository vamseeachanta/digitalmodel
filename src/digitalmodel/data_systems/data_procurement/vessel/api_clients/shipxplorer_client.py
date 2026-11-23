# ABOUTME: ShipXplorer API client for vessel database queries (fallback/alternative)
# ABOUTME: Streaming, vessel-based queries (IMO/MMSI → ship particulars)

"""
ShipXplorer Client
==================

FREE vessel database API from ShipXplorer (alternative to MarineTraffic).

Data Coverage:
- Database: Global vessel database
- Data: IMO, MMSI, name, type, dimensions
- Real-time: Position tracking

API: https://api.shipxplorer.com/v1
Authentication: FREE API key (100 requests/hour)
Rate Limit: 100 req/hour (FREE tier)

Critical: JSON streaming with vessel-based queries - NO file storage!
"""

import logging
from typing import Dict, Any, Iterator, Optional
from datetime import datetime
from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class ShipXplorerClient(BaseAPIClient):
    """
    ShipXplorer vessel database client (alternative/fallback).

    Retrieves vessel data via vessel identifiers (IMO, MMSI, name).

    Example:
        client = ShipXplorerClient(api_key="${SHIPXPLORER_API_KEY}")

        # Query by IMO
        vessel = client.get_vessel_by_imo("9321483")
        print(f"Vessel: {vessel['name']}, LOA: {vessel['length']}m")
    """

    def __init__(self, api_key: str = None, **kwargs):
        """
        Initialize ShipXplorer client.

        Args:
            api_key: ShipXplorer API key (optional for basic queries)
                    Get from https://shipxplorer.com/api
            **kwargs: Additional BaseAPIClient arguments
        """
        if api_key:
            auth_config = {
                'method': 'api_key',
                'credentials': {'key': api_key},
                'header_name': 'Authorization'
            }
            rate_limit = {'requests_per_hour': 100}  # FREE tier with key
        else:
            auth_config = {'method': 'none'}
            rate_limit = {'requests_per_hour': 10}  # Limited without key

        super().__init__(
            base_url="https://api.shipxplorer.com/v1",
            auth_config=auth_config,
            rate_limit=rate_limit,
            **kwargs
        )

        logger.info(f"Initialized ShipXplorerClient ({'with' if api_key else 'without'} API key)")

    def get_vessel_by_imo(self, imo: str) -> Dict[str, Any]:
        """
        Get vessel data by IMO number.

        Args:
            imo: IMO number (e.g., "9321483")

        Returns:
            Vessel data dict (same format as MarineTrafficClient)
        """
        params = {'imo': imo}

        response = self.get('vessels/search', params=params)
        data = response.json()

        if not data or 'vessels' not in data or not data['vessels']:
            raise ValueError(f"Vessel not found for IMO: {imo}")

        vessel_data = data['vessels'][0]

        return self._parse_vessel_data(vessel_data)

    def get_vessel_by_mmsi(self, mmsi: str) -> Dict[str, Any]:
        """
        Get vessel data by MMSI number.

        Args:
            mmsi: MMSI number (e.g., "219018314")

        Returns:
            Vessel data dict
        """
        params = {'mmsi': mmsi}

        response = self.get('vessels/search', params=params)
        data = response.json()

        if not data or 'vessels' not in data or not data['vessels']:
            raise ValueError(f"Vessel not found for MMSI: {mmsi}")

        vessel_data = data['vessels'][0]

        return self._parse_vessel_data(vessel_data)

    def search_vessels_by_name(self, name: str) -> Iterator[Dict[str, Any]]:
        """
        Search vessels by name (streaming results).

        Args:
            name: Vessel name (partial match)

        Yields:
            Vessel data dicts (streaming)
        """
        params = {'name': name}

        response = self.get('vessels/search', params=params)
        data = response.json()

        if not data or 'vessels' not in data:
            logger.warning(f"No vessels found for name: {name}")
            return

        # Stream results
        for vessel_data in data['vessels']:
            yield self._parse_vessel_data(vessel_data)

    def _parse_vessel_data(self, vessel_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Parse ShipXplorer vessel data to standardized format.

        Args:
            vessel_data: Raw API response data

        Returns:
            Standardized vessel data dict (matches MarineTraffic format)
        """
        return {
            'imo': vessel_data.get('imo'),
            'mmsi': vessel_data.get('mmsi'),
            'name': vessel_data.get('name'),
            'type': vessel_data.get('type'),
            'type_code': vessel_data.get('type_code'),
            'length': float(vessel_data.get('length', 0)),
            'beam': float(vessel_data.get('beam', 0)),
            'draft': float(vessel_data.get('draft', 0)),
            'dwt': int(vessel_data.get('dwt', 0)),
            'built_year': int(vessel_data.get('year_built', 0)),
            'flag': vessel_data.get('flag'),
            'callsign': vessel_data.get('callsign'),
            'gross_tonnage': int(vessel_data.get('gross_tonnage', 0)),
        }

    def query_by_date(self, start_date: datetime, end_date: datetime,
                      location: Dict[str, float] = None,
                      vessel_imo: str = None,
                      **kwargs) -> Iterator[Dict[str, Any]]:
        """
        Query vessel data (compatibility method).

        Args:
            start_date: Not used
            end_date: Not used
            location: Not used
            vessel_imo: IMO number to query

        Yields:
            Single vessel data record
        """
        if not vessel_imo:
            raise ValueError("vessel_imo required for vessel queries")

        vessel_data = self.get_vessel_by_imo(vessel_imo)
        vessel_data['timestamp'] = datetime.now()

        yield vessel_data
