# ABOUTME: Test suite for cache layer with Redis fallback to in-memory LRU
# Tests decorator, client API, cache warming, metrics, and serialization

import pickle
import time
from unittest.mock import MagicMock, patch

import pytest

from digitalmodel.core.cache import (
    CacheClient,
    CacheConfig,
    CacheStats,
    cache_result,
)


class TestCacheConfig:
    """Test cache configuration."""

    def test_default_config(self):
        """Test default configuration values."""
        config = CacheConfig()
        assert config.redis_host == "localhost"
        assert config.redis_port == 6379
        assert config.redis_db == 0
        assert config.default_ttl == 3600
        assert config.max_memory_items == 1000
        assert config.enable_redis is True

    def test_custom_config(self):
        """Test custom configuration values."""
        config = CacheConfig(
            redis_host="custom-host",
            redis_port=6380,
            default_ttl=7200,
            max_memory_items=500,
            enable_redis=False,
        )
        assert config.redis_host == "custom-host"
        assert config.redis_port == 6380
        assert config.default_ttl == 7200
        assert config.max_memory_items == 500
        assert config.enable_redis is False


class TestCacheStats:
    """Test cache statistics tracking."""

    def test_stats_initialization(self):
        """Test stats are initialized to zero."""
        stats = CacheStats()
        assert stats.hits == 0
        assert stats.misses == 0
        assert stats.sets == 0
        assert stats.deletes == 0
        assert stats.errors == 0

    def test_hit_rate_calculation(self):
        """Test hit rate calculation."""
        stats = CacheStats()
        assert stats.hit_rate == 0.0

        stats.hits = 8
        stats.misses = 2
        assert stats.hit_rate == 0.8

        stats.hits = 0
        stats.misses = 0
        assert stats.hit_rate == 0.0

    def test_stats_to_dict(self):
        """Test stats export to dict."""
        stats = CacheStats()
        stats.hits = 10
        stats.misses = 5
        stats.sets = 12
        stats.deletes = 3
        stats.errors = 1

        result = stats.to_dict()
        assert result["hits"] == 10
        assert result["misses"] == 5
        assert result["sets"] == 12
        assert result["deletes"] == 3
        assert result["errors"] == 1
        assert result["hit_rate"] == pytest.approx(0.6667, rel=0.01)


class TestCacheClientMemoryBackend:
    """Test cache client with in-memory LRU backend."""

    @pytest.fixture
    def cache_client(self):
        """Create cache client with Redis disabled."""
        config = CacheConfig(enable_redis=False, max_memory_items=3)
        return CacheClient(config)

    def test_set_and_get(self, cache_client):
        """Test basic set and get operations."""
        cache_client.set("key1", {"data": "value1"})
        result = cache_client.get("key1")
        assert result == {"data": "value1"}

    def test_get_miss(self, cache_client):
        """Test get on non-existent key returns None."""
        result = cache_client.get("nonexistent")
        assert result is None

    def test_delete(self, cache_client):
        """Test delete operation."""
        cache_client.set("key1", "value1")
        assert cache_client.get("key1") == "value1"

        cache_client.delete("key1")
        assert cache_client.get("key1") is None

    def test_clear(self, cache_client):
        """Test clear all cache entries."""
        cache_client.set("key1", "value1")
        cache_client.set("key2", "value2")

        cache_client.clear()
        assert cache_client.get("key1") is None
        assert cache_client.get("key2") is None

    def test_lru_eviction(self, cache_client):
        """Test LRU eviction when max items exceeded."""
        cache_client.set("key1", "value1")
        cache_client.set("key2", "value2")
        cache_client.set("key3", "value3")

        # Access key1 to make it recently used
        cache_client.get("key1")

        # Add key4, should evict key2 (least recently used)
        cache_client.set("key4", "value4")

        assert cache_client.get("key1") == "value1"  # Still exists
        assert cache_client.get("key2") is None  # Evicted
        assert cache_client.get("key3") == "value3"  # Still exists
        assert cache_client.get("key4") == "value4"  # Newly added

    def test_ttl_expiration(self, cache_client):
        """Test TTL expiration."""
        cache_client.set("key1", "value1", ttl=1)  # 1 second TTL
        assert cache_client.get("key1") == "value1"

        time.sleep(1.1)  # Wait for expiration
        assert cache_client.get("key1") is None

    def test_stats_tracking(self, cache_client):
        """Test statistics tracking."""
        cache_client.set("key1", "value1")
        cache_client.get("key1")  # Hit
        cache_client.get("key2")  # Miss
        cache_client.delete("key1")

        stats = cache_client.get_stats()
        assert stats["hits"] == 1
        assert stats["misses"] == 1
        assert stats["sets"] == 1
        assert stats["deletes"] == 1


class TestCacheClientRedisBackend:
    """Test cache client with Redis backend (using fakeredis)."""

    @pytest.fixture
    def cache_client(self):
        """Create cache client with fakeredis."""
        import fakeredis

        config = CacheConfig(enable_redis=True)
        client = CacheClient(config)

        # Replace real redis with fakeredis
        client.redis = fakeredis.FakeRedis(decode_responses=False)
        client.backend = "redis"

        return client

    def test_redis_set_and_get(self, cache_client):
        """Test Redis set and get with pickle serialization."""
        test_data = {"array": [1, 2, 3], "value": 42.5}
        cache_client.set("key1", test_data)
        result = cache_client.get("key1")
        assert result == test_data

    def test_redis_delete(self, cache_client):
        """Test Redis delete operation."""
        cache_client.set("key1", "value1")
        assert cache_client.get("key1") == "value1"

        cache_client.delete("key1")
        assert cache_client.get("key1") is None

    def test_redis_ttl(self, cache_client):
        """Test Redis TTL functionality."""
        cache_client.set("key1", "value1", ttl=1)
        assert cache_client.get("key1") == "value1"

        time.sleep(1.1)
        assert cache_client.get("key1") is None

    def test_redis_clear(self, cache_client):
        """Test Redis clear all entries."""
        cache_client.set("key1", "value1")
        cache_client.set("key2", "value2")

        cache_client.clear()
        assert cache_client.get("key1") is None
        assert cache_client.get("key2") is None


class TestCacheClientFallback:
    """Test Redis to in-memory fallback."""

    def test_fallback_on_redis_unavailable(self):
        """Test fallback to memory when Redis unavailable."""
        config = CacheConfig(enable_redis=True)
        client = CacheClient(config)

        # Should fallback to memory backend
        assert client.backend == "memory"

        # Should still work with memory backend
        client.set("key1", "value1")
        assert client.get("key1") == "value1"

    def test_redis_error_tracking(self):
        """Test Redis errors are tracked in stats."""
        config = CacheConfig(enable_redis=True)
        client = CacheClient(config)

        # Attempt operations (will fail since Redis not available)
        client.set("key1", "value1")
        client.get("key1")

        stats = client.get_stats()
        # Memory backend should work, no errors
        assert stats["errors"] == 0


class TestCacheDecorator:
    """Test @cache_result decorator."""

    @pytest.fixture
    def cache_client(self):
        """Create cache client for decorator tests."""
        config = CacheConfig(enable_redis=False)
        return CacheClient(config)

    def test_decorator_basic_caching(self, cache_client):
        """Test decorator caches function results."""
        call_count = 0

        @cache_result(cache_client, ttl=3600)
        def expensive_function(x, y):
            nonlocal call_count
            call_count += 1
            return x + y

        # First call - should execute
        result1 = expensive_function(1, 2)
        assert result1 == 3
        assert call_count == 1

        # Second call - should use cache
        result2 = expensive_function(1, 2)
        assert result2 == 3
        assert call_count == 1  # Not called again

        # Different arguments - should execute
        result3 = expensive_function(2, 3)
        assert result3 == 5
        assert call_count == 2

    def test_decorator_key_generation(self, cache_client):
        """Test decorator generates correct cache keys."""

        @cache_result(cache_client, ttl=3600)
        def func(a, b, c=3):
            return a + b + c

        func(1, 2, c=4)
        func(1, 2)  # Different c value

        stats = cache_client.get_stats()
        assert stats["sets"] == 2  # Two different cache keys

    def test_decorator_with_kwargs(self, cache_client):
        """Test decorator handles kwargs correctly."""

        @cache_result(cache_client, ttl=3600)
        def func(a, b=2, **kwargs):
            return a + b + kwargs.get("c", 0)

        result1 = func(1, b=2, c=3)
        result2 = func(1, b=2, c=3)

        assert result1 == result2
        stats = cache_client.get_stats()
        assert stats["hits"] == 1

    def test_decorator_custom_key_prefix(self, cache_client):
        """Test decorator with custom key prefix."""

        @cache_result(cache_client, ttl=3600, key_prefix="custom")
        def func(x):
            return x * 2

        func(5)
        # Key should be "custom:func:5" or similar
        # Verify by checking cache hit on second call
        func(5)

        stats = cache_client.get_stats()
        assert stats["hits"] == 1


class TestCacheWarming:
    """Test cache warming functionality."""

    @pytest.fixture
    def cache_client(self):
        """Create cache client for warming tests."""
        config = CacheConfig(enable_redis=False)
        return CacheClient(config)

    def test_warm_cache_basic(self, cache_client):
        """Test basic cache warming."""

        def compute_func(x):
            return x * 2

        args_list = [(1,), (2,), (3,)]
        cache_client.warm(compute_func, args_list)

        # Verify all values are cached
        assert cache_client.get("compute_func:(1,)") == 2
        assert cache_client.get("compute_func:(2,)") == 4
        assert cache_client.get("compute_func:(3,)") == 6

    def test_warm_cache_with_errors(self, cache_client):
        """Test cache warming continues on errors."""
        call_log = []

        def compute_func(x):
            call_log.append(x)
            if x == 2:
                raise ValueError("Test error")
            return x * 2

        args_list = [(1,), (2,), (3,)]
        cache_client.warm(compute_func, args_list)

        # Should have called all items
        assert call_log == [1, 2, 3]

        # Item 1 and 3 should be cached
        assert cache_client.get("compute_func:(1,)") == 2
        assert cache_client.get("compute_func:(3,)") == 6

        # Item 2 should not be cached (error)
        assert cache_client.get("compute_func:(2,)") is None

    def test_warm_cache_with_kwargs(self, cache_client):
        """Test cache warming with keyword arguments."""

        def compute_func(x, multiplier=2):
            return x * multiplier

        args_list = [(1,), (2,)]
        kwargs_list = [{"multiplier": 3}, {"multiplier": 4}]

        cache_client.warm(compute_func, args_list, kwargs_list)

        # Verify cached with kwargs
        assert cache_client.get("compute_func:(1,):{'multiplier': 3}") == 3
        assert cache_client.get("compute_func:(2,):{'multiplier': 4}") == 8


class TestCacheStatsExport:
    """Test cache statistics export to JSON."""

    @pytest.fixture
    def cache_client(self):
        """Create cache client for stats tests."""
        config = CacheConfig(enable_redis=False)
        return CacheClient(config)

    def test_export_stats_to_file(self, cache_client, tmp_path):
        """Test exporting stats to JSON file."""
        import json

        # Generate some stats
        cache_client.set("key1", "value1")
        cache_client.get("key1")
        cache_client.get("key2")

        # Export to file
        stats_file = tmp_path / "cache_stats.json"
        cache_client.export_stats(str(stats_file))

        # Verify file contents
        assert stats_file.exists()
        with open(stats_file) as f:
            stats = json.load(f)

        assert stats["hits"] == 1
        assert stats["misses"] == 1
        assert stats["sets"] == 1

    def test_stats_reset(self, cache_client):
        """Test resetting statistics."""
        cache_client.set("key1", "value1")
        cache_client.get("key1")

        cache_client.reset_stats()
        stats = cache_client.get_stats()

        assert stats["hits"] == 0
        assert stats["misses"] == 0
        assert stats["sets"] == 0


class TestCacheIntegration:
    """Integration tests with realistic scenarios."""

    @pytest.fixture
    def cache_client(self):
        """Create cache client for integration tests."""
        config = CacheConfig(enable_redis=False, max_memory_items=100)
        return CacheClient(config)

    def test_hydrodynamic_coefficient_caching(self, cache_client):
        """Test caching hydrodynamic coefficients (real use case)."""

        @cache_result(cache_client, ttl=7200)
        def load_hydrodynamic_coefficients(vessel_id, wave_period):
            """Simulate loading expensive hydrodynamic data."""
            # Simulate computation
            time.sleep(0.01)
            return {
                "added_mass": [[1.0, 0.0], [0.0, 1.0]],
                "damping": [[0.5, 0.0], [0.0, 0.5]],
                "wave_period": wave_period,
            }

        # First call - should compute
        start = time.time()
        result1 = load_hydrodynamic_coefficients("vessel_1", 10.0)
        duration1 = time.time() - start

        # Second call - should use cache (much faster)
        start = time.time()
        result2 = load_hydrodynamic_coefficients("vessel_1", 10.0)
        duration2 = time.time() - start

        assert result1 == result2
        assert duration2 < duration1 / 2  # Cache should be significantly faster

        stats = cache_client.get_stats()
        assert stats["hits"] == 1
        assert stats["misses"] == 1  # First call is a miss, second is a hit

    def test_bulk_data_warming(self, cache_client):
        """Test warming cache with bulk hydrodynamic data."""

        def compute_rao(wave_period):
            """Simulate RAO computation."""
            return {"amplitude": 1.0 / wave_period, "phase": 45.0}

        # Warm cache for common wave periods
        wave_periods = [(5.0,), (10.0,), (15.0,), (20.0,)]
        cache_client.warm(compute_rao, wave_periods)

        # All should be cached now
        stats = cache_client.get_stats()
        assert stats["sets"] == 4

        # Verify fast retrieval
        result = cache_client.get("compute_rao:(10.0,)")
        assert result == {"amplitude": 0.1, "phase": 45.0}
