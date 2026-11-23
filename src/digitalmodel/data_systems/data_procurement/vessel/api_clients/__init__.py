# ABOUTME: FREE API clients for vessel data procurement
# ABOUTME: MarineTraffic, ShipXplorer, Generic RAO database - all with vessel-based queries

"""
Vessel API Clients
==================

FREE API integrations for vessel systems data.

Clients:
- MarineTrafficClient: Vessel database (IMO/MMSI → ship particulars)
- ShipXplorerClient: Alternative vessel database (fallback)
- GenericRAOClient: Generic RAO library (vessel type → RAOs)

All clients implement streaming, vessel-based queries with zero storage.
"""

from .marinetraffic_client import MarineTrafficClient
from .shipxplorer_client import ShipXplorerClient
from .generic_rao_client import GenericRAOClient

__all__ = [
    "MarineTrafficClient",
    "ShipXplorerClient",
    "GenericRAOClient",
]
