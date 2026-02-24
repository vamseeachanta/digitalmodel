# ABOUTME: Backward-compatibility shim for infrastructure/core/ (WRK-415 Phase 2A).
# All persistence logic has moved to infrastructure/persistence/.
# This shim re-exports everything for existing callers.

import warnings

warnings.warn(
    "digitalmodel.infrastructure.core is deprecated. "
    "Import from digitalmodel.infrastructure.persistence instead. "
    "Will be removed in a future release.",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.persistence import (
    DatabaseManager,
    get_db_connection_pooled,
    CacheClient,
    CacheConfig,
    CacheStats,
    LRUCache,
    cache_result,
    DataProvenance,
    ProvenanceTracker,
    TransformationRecord,
    compute_hash,
    track_provenance,
    find_sources_for,
    find_outputs_from,
)

__all__ = [
    "DatabaseManager",
    "get_db_connection_pooled",
    "CacheClient",
    "CacheConfig",
    "CacheStats",
    "LRUCache",
    "cache_result",
    "DataProvenance",
    "ProvenanceTracker",
    "TransformationRecord",
    "compute_hash",
    "track_provenance",
    "find_sources_for",
    "find_outputs_from",
]
