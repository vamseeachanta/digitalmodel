# ABOUTME: Redis-backed cache with in-memory LRU fallback, decorator support, and metrics tracking
# Provides both decorator (@cache_result) and explicit client API with cache warming

import hashlib
import json
import logging
import pickle
import time
from collections import OrderedDict
from dataclasses import dataclass, field
from functools import wraps
from typing import Any, Callable, Dict, List, Optional, Tuple

logger = logging.getLogger(__name__)


@dataclass
class CacheConfig:
    """Cache configuration with Redis and memory backend settings."""

    redis_host: str = "localhost"
    redis_port: int = 6379
    redis_db: int = 0
    redis_password: Optional[str] = None
    default_ttl: int = 3600  # 1 hour default TTL
    max_memory_items: int = 1000  # LRU max items
    enable_redis: bool = True  # Try Redis, fallback to memory


@dataclass
class CacheStats:
    """Cache statistics tracking."""

    hits: int = 0
    misses: int = 0
    sets: int = 0
    deletes: int = 0
    errors: int = 0

    @property
    def hit_rate(self) -> float:
        """Calculate hit rate (0-1)."""
        total = self.hits + self.misses
        return self.hits / total if total > 0 else 0.0

    def to_dict(self) -> Dict[str, Any]:
        """Export stats to dictionary."""
        return {
            "hits": self.hits,
            "misses": self.misses,
            "sets": self.sets,
            "deletes": self.deletes,
            "errors": self.errors,
            "hit_rate": self.hit_rate,
        }


class LRUCache:
    """In-memory LRU cache with TTL support."""

    def __init__(self, max_items: int = 1000):
        """
        Initialize LRU cache.

        Args:
            max_items: Maximum number of items to store
        """
        self.max_items = max_items
        self._cache: OrderedDict = OrderedDict()
        self._expiry: Dict[str, float] = {}

    def get(self, key: str) -> Optional[Any]:
        """
        Get item from cache.

        Args:
            key: Cache key

        Returns:
            Cached value or None if not found/expired
        """
        # Check expiry
        if key in self._expiry:
            if time.time() > self._expiry[key]:
                # Expired
                self._cache.pop(key, None)
                self._expiry.pop(key, None)
                return None

        # Get and move to end (most recently used)
        if key in self._cache:
            self._cache.move_to_end(key)
            return self._cache[key]

        return None

    def set(self, key: str, value: Any, ttl: Optional[int] = None) -> None:
        """
        Set item in cache.

        Args:
            key: Cache key
            value: Value to cache
            ttl: Time to live in seconds (optional)
        """
        # Remove if exists (to update position)
        if key in self._cache:
            self._cache.pop(key)

        # Add to cache
        self._cache[key] = value

        # Set expiry if TTL provided
        if ttl is not None:
            self._expiry[key] = time.time() + ttl
        else:
            self._expiry.pop(key, None)

        # Evict LRU if over capacity
        if len(self._cache) > self.max_items:
            # Pop from beginning (least recently used)
            oldest_key = next(iter(self._cache))
            self._cache.pop(oldest_key)
            self._expiry.pop(oldest_key, None)

    def delete(self, key: str) -> None:
        """
        Delete item from cache.

        Args:
            key: Cache key
        """
        self._cache.pop(key, None)
        self._expiry.pop(key, None)

    def clear(self) -> None:
        """Clear all cache entries."""
        self._cache.clear()
        self._expiry.clear()


class CacheClient:
    """
    Cache client with Redis backend and in-memory fallback.

    Features:
    - Automatic Redis fallback to in-memory LRU
    - Pickle serialization for complex objects
    - TTL support
    - Statistics tracking
    - Cache warming
    """

    def __init__(self, config: Optional[CacheConfig] = None):
        """
        Initialize cache client.

        Args:
            config: Cache configuration (uses defaults if None)
        """
        self.config = config or CacheConfig()
        self.stats = CacheStats()
        self.redis = None
        self.backend = "memory"

        # Try to connect to Redis if enabled
        if self.config.enable_redis:
            self._init_redis()

        # Always have memory backend as fallback
        self.memory_cache = LRUCache(max_items=self.config.max_memory_items)

    def _init_redis(self) -> None:
        """Initialize Redis connection with error handling."""
        try:
            import redis

            self.redis = redis.Redis(
                host=self.config.redis_host,
                port=self.config.redis_port,
                db=self.config.redis_db,
                password=self.config.redis_password,
                decode_responses=False,  # We use pickle
                socket_connect_timeout=2,
                socket_timeout=2,
            )

            # Test connection
            self.redis.ping()
            self.backend = "redis"
            logger.info(
                f"Connected to Redis at {self.config.redis_host}:{self.config.redis_port}"
            )

        except ImportError:
            logger.warning("redis package not available, using memory backend")
            self.redis = None

        except Exception as error:
            logger.warning(
                f"Failed to connect to Redis: {error}. Using memory backend."
            )
            self.redis = None

    def get(self, key: str) -> Optional[Any]:
        """
        Get value from cache.

        Args:
            key: Cache key

        Returns:
            Cached value or None if not found
        """
        try:
            if self.backend == "redis" and self.redis:
                # Try Redis first
                value = self.redis.get(key)
                if value is not None:
                    self.stats.hits += 1
                    return pickle.loads(value)
                else:
                    self.stats.misses += 1
                    return None
            else:
                # Use memory backend
                value = self.memory_cache.get(key)
                if value is not None:
                    self.stats.hits += 1
                else:
                    self.stats.misses += 1
                return value

        except Exception as error:
            logger.error(f"Cache get error for key {key}: {error}")
            self.stats.errors += 1
            return None

    def set(
        self, key: str, value: Any, ttl: Optional[int] = None
    ) -> None:
        """
        Set value in cache.

        Args:
            key: Cache key
            value: Value to cache
            ttl: Time to live in seconds (uses default if None)
        """
        try:
            ttl = ttl or self.config.default_ttl

            if self.backend == "redis" and self.redis:
                # Use Redis
                serialized = pickle.dumps(value)
                self.redis.setex(key, ttl, serialized)
            else:
                # Use memory backend
                self.memory_cache.set(key, value, ttl)

            self.stats.sets += 1

        except Exception as error:
            logger.error(f"Cache set error for key {key}: {error}")
            self.stats.errors += 1

    def delete(self, key: str) -> None:
        """
        Delete value from cache.

        Args:
            key: Cache key
        """
        try:
            if self.backend == "redis" and self.redis:
                self.redis.delete(key)
            else:
                self.memory_cache.delete(key)

            self.stats.deletes += 1

        except Exception as error:
            logger.error(f"Cache delete error for key {key}: {error}")
            self.stats.errors += 1

    def clear(self) -> None:
        """Clear all cache entries."""
        try:
            if self.backend == "redis" and self.redis:
                self.redis.flushdb()
            else:
                self.memory_cache.clear()

            logger.info("Cache cleared")

        except Exception as error:
            logger.error(f"Cache clear error: {error}")
            self.stats.errors += 1

    def warm(
        self,
        func: Callable,
        args_list: List[Tuple],
        kwargs_list: Optional[List[Dict]] = None,
    ) -> None:
        """
        Warm cache by pre-computing values.

        Continues on errors, logs failures.

        Args:
            func: Function to call for computing values
            args_list: List of argument tuples to call func with
            kwargs_list: Optional list of kwargs dicts (parallel to args_list)
        """
        if kwargs_list is None:
            kwargs_list = [{}] * len(args_list)

        func_name = func.__name__

        for args, kwargs in zip(args_list, kwargs_list):
            try:
                # Compute value
                result = func(*args, **kwargs)

                # Generate cache key
                key = self._generate_key(func_name, args, kwargs)

                # Store in cache
                self.set(key, result)

                logger.debug(f"Warmed cache for {func_name}{args}")

            except Exception as error:
                logger.warning(
                    f"Cache warming failed for {func_name}{args}: {error}"
                )
                continue

    def get_stats(self) -> Dict[str, Any]:
        """
        Get cache statistics.

        Returns:
            Statistics dictionary
        """
        return self.stats.to_dict()

    def reset_stats(self) -> None:
        """Reset statistics to zero."""
        self.stats = CacheStats()

    def export_stats(self, filepath: str) -> None:
        """
        Export statistics to JSON file.

        Args:
            filepath: Path to save JSON stats
        """
        stats = self.get_stats()
        with open(filepath, "w") as f:
            json.dump(stats, f, indent=2)
        logger.info(f"Stats exported to {filepath}")

    @staticmethod
    def _generate_key(func_name: str, args: Tuple, kwargs: Dict) -> str:
        """
        Generate cache key from function name and arguments.

        Args:
            func_name: Function name
            args: Positional arguments
            kwargs: Keyword arguments

        Returns:
            Cache key string
        """
        # Simple key generation: "funcname:args:kwargs"
        if kwargs:
            return f"{func_name}:{args}:{kwargs}"
        else:
            return f"{func_name}:{args}"


def cache_result(
    cache_client: CacheClient,
    ttl: Optional[int] = None,
    key_prefix: Optional[str] = None,
) -> Callable:
    """
    Decorator to cache function results.

    Usage:
        @cache_result(cache_client, ttl=3600)
        def expensive_function(x, y):
            return x + y

    Args:
        cache_client: Cache client instance
        ttl: Time to live in seconds (uses default if None)
        key_prefix: Optional key prefix (uses function name if None)

    Returns:
        Decorator function
    """

    def decorator(func: Callable) -> Callable:
        prefix = key_prefix or func.__name__

        @wraps(func)
        def wrapper(*args, **kwargs):
            # Generate cache key
            cache_key = cache_client._generate_key(prefix, args, kwargs)

            # Try to get from cache
            cached_value = cache_client.get(cache_key)
            if cached_value is not None:
                return cached_value

            # Compute value
            result = func(*args, **kwargs)

            # Store in cache
            cache_client.set(cache_key, result, ttl=ttl)

            return result

        return wrapper

    return decorator
