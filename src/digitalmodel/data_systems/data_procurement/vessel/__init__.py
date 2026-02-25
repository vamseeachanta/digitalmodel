# ABOUTME: Vessel systems data procurement module (RAOs, ship data, hydrostatics)
# ABOUTME: Implements streaming, vessel-based queries with zero storage for vessel characteristics

"""
Vessel Systems Data Procurement
================================

Universal vessel data retrieval via FREE web APIs and generic databases.

Data Types:
- Vessel identification (IMO, MMSI, name, dimensions)
- RAO data (Response Amplitude Operators, 6 DOF)
- Hydrostatic properties (displacement, GM, radii of gyration)
- Natural periods (roll, pitch, heave)

FREE APIs:
- MarineTraffic: Vessel database (5 req/min FREE tier)
- ShipXplorer: Alternative vessel database (100 req/hour FREE)
- Generic RAO Database: Repository-based RAO library

Architecture:
- Vessel-based queries (IMO/MMSI/name → vessel data)
- Streaming retrieval (no file storage)
- In-memory processing
- Direct consumption (OrcaFlex vessel YAML, AQWA)

CRITICAL: NO data saving (RAO files can be large)
"""

from .client import VesselClient
from .api_clients import (
    MarineTrafficClient,
    ShipXplorerClient,
    GenericRAOClient,
)

__all__ = [
    "VesselClient",
    "MarineTrafficClient",
    "ShipXplorerClient",
    "GenericRAOClient",
]
