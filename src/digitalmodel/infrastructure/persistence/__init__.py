# ABOUTME: Persistence module — database management, caching, and data provenance
# Consolidated from infrastructure/core/ in Phase 2A (WRK-415).

from .database_manager import DatabaseManager, get_db_connection_pooled
from .cache import CacheClient, CacheConfig, CacheStats, LRUCache, cache_result
from .provenance import (
    DataProvenance,
    ProvenanceTracker,
    TransformationRecord,
    compute_hash,
    track_provenance,
    find_sources_for,
    find_outputs_from,
)

__all__ = [
    # database_manager
    "DatabaseManager",
    "get_db_connection_pooled",
    # cache
    "CacheClient",
    "CacheConfig",
    "CacheStats",
    "LRUCache",
    "cache_result",
    # provenance
    "DataProvenance",
    "ProvenanceTracker",
    "TransformationRecord",
    "compute_hash",
    "track_provenance",
    "find_sources_for",
    "find_outputs_from",
]
