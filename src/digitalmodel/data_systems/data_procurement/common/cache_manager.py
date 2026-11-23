# ABOUTME: Multi-tier caching system (L1: Memory, L2: Redis, L3: Disk)
# ABOUTME: For metadata and small data only - NOT for large datasets (use streaming instead)

"""
Cache Manager
=============

Multi-tier caching for API metadata and small data.

Tiers:
- L1 (Memory): Hot data, fast access, limited size
- L2 (Redis): Warm data, shared across processes, TTL-based
- L3 (Disk): Cold data, persistent storage, large capacity

CRITICAL: Use only for metadata (API schemas, configurations, small lookup tables)
For large metocean/vessel data: Use streaming (stream_handler.py) - NO caching!

Why: Large datasets (GB-TB) are infeasible to cache. Always fetch on-demand via API.
"""

import logging
import time
import json
import hashlib
from typing import Any, Optional, Dict
from datetime import datetime, timedelta
from pathlib import Path
from abc import ABC, abstractmethod

logger = logging.getLogger(__name__)


class CacheTier(ABC):
    """Abstract base class for cache tiers."""

    @abstractmethod
    def get(self, key: str) -> Optional[Any]:
        """Get value from cache."""
        pass

    @abstractmethod
    def set(self, key: str, value: Any, ttl: Optional[int] = None) -> None:
        """Set value in cache."""
        pass

    @abstractmethod
    def delete(self, key: str) -> None:
        """Delete value from cache."""
        pass

    @abstractmethod
    def clear(self) -> None:
        """Clear entire cache tier."""
        pass


class L1MemoryCache(CacheTier):
    """
    L1 Memory Cache (in-process).

    Fast access, limited size, no persistence.
    Use for: API schemas, configurations, small lookup tables.
    """

    def __init__(self, max_size: int = 1000, default_ttl: int = 3600):
        """
        Initialize L1 cache.

        Args:
            max_size: Maximum number of entries
            default_ttl: Default time-to-live in seconds
        """
        self.cache: Dict[str, Dict[str, Any]] = {}
        self.max_size = max_size
        self.default_ttl = default_ttl
        logger.info(f"Initialized L1MemoryCache (max_size={max_size})")

    def get(self, key: str) -> Optional[Any]:
        """Get value from L1 cache."""
        entry = self.cache.get(key)

        if entry is None:
            return None

        # Check TTL
        if entry['expires_at'] and datetime.now() > entry['expires_at']:
            del self.cache[key]
            return None

        logger.debug(f"L1 cache HIT: {key}")
        return entry['value']

    def set(self, key: str, value: Any, ttl: Optional[int] = None) -> None:
        """Set value in L1 cache."""
        # Evict oldest entry if cache full
        if len(self.cache) >= self.max_size and key not in self.cache:
            oldest_key = min(self.cache.keys(), key=lambda k: self.cache[k]['created_at'])
            del self.cache[oldest_key]
            logger.debug(f"L1 cache evicted: {oldest_key}")

        ttl = ttl or self.default_ttl
        expires_at = datetime.now() + timedelta(seconds=ttl) if ttl else None

        self.cache[key] = {
            'value': value,
            'created_at': datetime.now(),
            'expires_at': expires_at
        }

        logger.debug(f"L1 cache SET: {key} (ttl={ttl}s)")

    def delete(self, key: str) -> None:
        """Delete value from L1 cache."""
        if key in self.cache:
            del self.cache[key]
            logger.debug(f"L1 cache DELETE: {key}")

    def clear(self) -> None:
        """Clear L1 cache."""
        self.cache.clear()
        logger.info("L1 cache CLEARED")


class L2RedisCache(CacheTier):
    """
    L2 Redis Cache (shared across processes).

    Persistent, shared, TTL-based.
    Use for: Shared metadata, API responses, session data.
    """

    def __init__(self, host: str = 'localhost', port: int = 6379,
                 db: int = 0, default_ttl: int = 86400):
        """
        Initialize L2 cache.

        Args:
            host: Redis host
            port: Redis port
            db: Redis database number
            default_ttl: Default time-to-live in seconds
        """
        self.default_ttl = default_ttl

        try:
            import redis
            self.redis = redis.Redis(host=host, port=port, db=db, decode_responses=True)
            self.redis.ping()
            logger.info(f"Initialized L2RedisCache ({host}:{port}/{db})")
        except ImportError:
            logger.warning("redis-py not installed, L2 cache disabled")
            self.redis = None
        except Exception as e:
            logger.warning(f"Failed to connect to Redis: {e}, L2 cache disabled")
            self.redis = None

    def get(self, key: str) -> Optional[Any]:
        """Get value from L2 cache."""
        if not self.redis:
            return None

        try:
            value = self.redis.get(key)
            if value:
                logger.debug(f"L2 cache HIT: {key}")
                return json.loads(value)
            return None
        except Exception as e:
            logger.error(f"L2 cache GET error: {e}")
            return None

    def set(self, key: str, value: Any, ttl: Optional[int] = None) -> None:
        """Set value in L2 cache."""
        if not self.redis:
            return

        try:
            ttl = ttl or self.default_ttl
            serialized = json.dumps(value)
            self.redis.setex(key, ttl, serialized)
            logger.debug(f"L2 cache SET: {key} (ttl={ttl}s)")
        except Exception as e:
            logger.error(f"L2 cache SET error: {e}")

    def delete(self, key: str) -> None:
        """Delete value from L2 cache."""
        if not self.redis:
            return

        try:
            self.redis.delete(key)
            logger.debug(f"L2 cache DELETE: {key}")
        except Exception as e:
            logger.error(f"L2 cache DELETE error: {e}")

    def clear(self) -> None:
        """Clear L2 cache."""
        if not self.redis:
            return

        try:
            self.redis.flushdb()
            logger.info("L2 cache CLEARED")
        except Exception as e:
            logger.error(f"L2 cache CLEAR error: {e}")


class L3DiskCache(CacheTier):
    """
    L3 Disk Cache (persistent file storage).

    Large capacity, slow access, persistent.
    Use for: Rarely-changing data, large lookup tables, backups.

    WARNING: NOT for metocean/vessel data streams (use streaming instead)!
    """

    def __init__(self, cache_dir: str = './cache', default_ttl: int = 2592000):
        """
        Initialize L3 cache.

        Args:
            cache_dir: Directory for cache files
            default_ttl: Default time-to-live in seconds (30 days)
        """
        self.cache_dir = Path(cache_dir)
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        self.default_ttl = default_ttl
        logger.info(f"Initialized L3DiskCache ({cache_dir})")

    def _get_cache_path(self, key: str) -> Path:
        """Get file path for cache key."""
        # Hash key to avoid filesystem issues
        key_hash = hashlib.sha256(key.encode()).hexdigest()
        return self.cache_dir / f"{key_hash}.json"

    def get(self, key: str) -> Optional[Any]:
        """Get value from L3 cache."""
        cache_path = self._get_cache_path(key)

        if not cache_path.exists():
            return None

        try:
            with open(cache_path, 'r') as f:
                entry = json.load(f)

            # Check TTL
            if entry['expires_at']:
                expires_at = datetime.fromisoformat(entry['expires_at'])
                if datetime.now() > expires_at:
                    cache_path.unlink()
                    return None

            logger.debug(f"L3 cache HIT: {key}")
            return entry['value']

        except Exception as e:
            logger.error(f"L3 cache GET error: {e}")
            return None

    def set(self, key: str, value: Any, ttl: Optional[int] = None) -> None:
        """Set value in L3 cache."""
        cache_path = self._get_cache_path(key)

        try:
            ttl = ttl or self.default_ttl
            expires_at = datetime.now() + timedelta(seconds=ttl) if ttl else None

            entry = {
                'value': value,
                'created_at': datetime.now().isoformat(),
                'expires_at': expires_at.isoformat() if expires_at else None
            }

            with open(cache_path, 'w') as f:
                json.dump(entry, f, indent=2)

            logger.debug(f"L3 cache SET: {key} (ttl={ttl}s)")

        except Exception as e:
            logger.error(f"L3 cache SET error: {e}")

    def delete(self, key: str) -> None:
        """Delete value from L3 cache."""
        cache_path = self._get_cache_path(key)

        try:
            if cache_path.exists():
                cache_path.unlink()
                logger.debug(f"L3 cache DELETE: {key}")
        except Exception as e:
            logger.error(f"L3 cache DELETE error: {e}")

    def clear(self) -> None:
        """Clear L3 cache."""
        try:
            for cache_file in self.cache_dir.glob('*.json'):
                cache_file.unlink()
            logger.info("L3 cache CLEARED")
        except Exception as e:
            logger.error(f"L3 cache CLEAR error: {e}")


class CacheManager:
    """
    Multi-tier cache manager.

    Cascades through L1 → L2 → L3, caching at each tier on hit.

    CRITICAL: Use ONLY for metadata (API schemas, configs, small lookups).
    For large data streams (metocean, vessel): Use StreamHandler - NO caching!
    """

    def __init__(self, enable_l1: bool = True, enable_l2: bool = False,
                 enable_l3: bool = False, **tier_configs):
        """
        Initialize cache manager.

        Args:
            enable_l1: Enable L1 (Memory) cache
            enable_l2: Enable L2 (Redis) cache
            enable_l3: Enable L3 (Disk) cache
            **tier_configs: Configuration for each tier
        """
        self.tiers: Dict[str, CacheTier] = {}

        if enable_l1:
            l1_config = tier_configs.get('l1', {})
            self.tiers['L1'] = L1MemoryCache(**l1_config)

        if enable_l2:
            l2_config = tier_configs.get('l2', {})
            self.tiers['L2'] = L2RedisCache(**l2_config)

        if enable_l3:
            l3_config = tier_configs.get('l3', {})
            self.tiers['L3'] = L3DiskCache(**l3_config)

        logger.info(f"Initialized CacheManager with tiers: {list(self.tiers.keys())}")

    def get(self, key: str) -> Optional[Any]:
        """
        Get value from cache (cascading L1 → L2 → L3).

        On hit at lower tier, promote to higher tiers.
        """
        for tier_name, tier in self.tiers.items():
            value = tier.get(key)

            if value is not None:
                logger.debug(f"Cache HIT at {tier_name}: {key}")

                # Promote to higher tiers
                self._promote(key, value, tier_name)

                return value

        logger.debug(f"Cache MISS: {key}")
        return None

    def set(self, key: str, value: Any, ttl: Optional[int] = None,
            tiers: Optional[list] = None) -> None:
        """
        Set value in cache (all tiers by default).

        Args:
            key: Cache key
            value: Value to cache
            ttl: Time-to-live in seconds (None = default)
            tiers: Specific tiers to cache in (None = all)
        """
        target_tiers = tiers or list(self.tiers.keys())

        for tier_name in target_tiers:
            if tier_name in self.tiers:
                self.tiers[tier_name].set(key, value, ttl)

    def delete(self, key: str) -> None:
        """Delete value from all cache tiers."""
        for tier in self.tiers.values():
            tier.delete(key)

    def clear(self) -> None:
        """Clear all cache tiers."""
        for tier in self.tiers.values():
            tier.clear()

    def _promote(self, key: str, value: Any, from_tier: str) -> None:
        """Promote value to higher cache tiers."""
        tier_order = ['L1', 'L2', 'L3']
        from_index = tier_order.index(from_tier)

        # Promote to all tiers above
        for tier_name in tier_order[:from_index]:
            if tier_name in self.tiers:
                self.tiers[tier_name].set(key, value)
