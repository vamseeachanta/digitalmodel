# ABOUTME: Common components for data procurement
# ABOUTME: Universal API clients, streaming handlers, caching, and configuration

"""
Common Data Procurement Components
===================================

DRY foundation framework referenced by all asset-specific modules.

Components:
- BaseAPIClient: Universal REST/GraphQL/WebSocket client
- StreamHandler: Streaming data processor (zero storage)
- CacheManager: Multi-tier caching (L1/L2/L3)
- ConfigLoader: YAML configuration parser
"""

from .base_client import BaseAPIClient
from .stream_handler import StreamHandler
from .cache_manager import CacheManager
from .config_loader import ConfigLoader

__all__ = [
    "BaseAPIClient",
    "StreamHandler",
    "CacheManager",
    "ConfigLoader",
]
