# ABOUTME: Data procurement module for marine engineering assets via web APIs
# ABOUTME: Implements streaming, date-based queries, and zero storage architecture

"""
Data Procurement Module
=======================

Universal data procurement framework for marine engineering assets.

Key Principles:
- Date-based API queries for on-demand retrieval
- Streaming architecture with zero storage footprint
- Direct consumption by analysis software (OrcaFlex, AQWA)
- FREE APIs only (no credit card required)

Supported Asset Categories:
- Metocean data (waves, wind, current, tides, bathymetry)
- Vessel systems (RAOs, ship data, hydrostatics)
- Mooring systems (chains, ropes, anchors, connectors)

Architecture:
- No intermediate file storage (data can be huge)
- In-memory processing pipelines
- Streaming consumers for large datasets
- Multi-tier caching (L1: Memory, L2: Redis, L3: Disk) for metadata only
"""

from .common import (
    BaseAPIClient,
    StreamHandler,
    CacheManager,
    ConfigLoader,
)
from .metocean import MetoceanClient
from .vessel import VesselClient
from .mooring import MooringClient
from .riser import RiserClient

__version__ = "4.0.0"  # Phase 4: Repository Integration complete
__all__ = [
    "BaseAPIClient",
    "StreamHandler",
    "CacheManager",
    "ConfigLoader",
    "MetoceanClient",
    "VesselClient",
    "MooringClient",
    "RiserClient",
]
