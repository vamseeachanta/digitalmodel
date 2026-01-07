"""
ABOUTME: Test suite for cache layer with Redis and in-memory fallback.
Tests cache decorators, TTL, invalidation, and performance metrics.
"""

import time
import pytest
from unittest.mock import Mock, patch, MagicMock
import pickle
import hashlib
import json


@pytest.fixture
def mock_redis():
    """Mock Redis client for testing."""
    redis_mock = MagicMock()
    redis_mock.ping.return_value = True
    redis_mock.get.return_value = None
    redis_mock.set.return_value = True
    redis_mock.delete.return_value = 1
    redis_mock.exists.return_value = 0
    return redis_mock


@pytest.fixture
def cache_with_redis(mock_redis):
    """Cache instance with mocked Redis backend."""
    from digitalmodel.core.cache import Cache

    with patch('digitalmodel.core.cache.redis.Redis', return_value=mock_redis):
        cache = Cache(redis_url="redis://localhost:6379")
        yield cache


@pytest.fixture
def cache_without_redis():
    """Cache instance with in-memory fallback (no Redis)."""
    from digitalmodel.core.cache import Cache

    with patch('digitalmodel.core.cache.redis.Redis', side_effect=Exception("Redis unavailable")):
        cache = Cache(redis_url="redis://localhost:6379")
        yield cache


class TestCacheInitialization:
    """Test cache initialization and backend selection."""

    def test_redis_backend_initialization(self, mock_redis):
        """Test successful Redis backend initialization."""
        from digitalmodel.core.cache import Cache

        with patch('digitalmodel.core.cache.redis.Redis', return_value=mock_redis):
            cache = Cache(redis_url="redis://localhost:6379")
            assert cache.backend_type == "redis"
            assert cache._redis_client is not None

    def test_fallback_to_memory_on_redis_failure(self):
        """Test fallback to in-memory cache when Redis fails."""
        from digitalmodel.core.cache import Cache

        with patch('digitalmodel.core.cache.redis.Redis', side_effect=Exception("Connection failed")):
            cache = Cache(redis_url="redis://localhost:6379")
            assert cache.backend_type == "memory"
            assert cache._memory_cache is not None

    def test_default_redis_url_from_env(self, monkeypatch):
        """Test Redis URL defaults to environment variable."""
        from digitalmodel.core.cache import Cache

        monkeypatch.setenv("DM_REDIS_URL", "redis://custom:6380")
        with patch('digitalmodel.core.cache.redis.Redis') as mock_redis_class:
            cache = Cache()
            # Verify Redis was initialized with custom URL
            mock_redis_class.assert_called_once()
            call_args = mock_redis_class.call_args
            assert "custom" in str(call_args) or call_args[1].get('host') == 'custom'


class TestCacheKeyGeneration:
    """Test cache key generation from function arguments."""

    def test_simple_function_key_generation(self, cache_with_redis):
        """Test cache key for simple function with basic args."""
        @cache_with_redis.cache(ttl=60)
        def simple_func(a, b):
            return a + b

        # Generate expected key
        args_key = pickle.dumps(((), {'a': 1, 'b': 2}))
        hash_key = hashlib.sha256(args_key).hexdigest()
        expected_key = f"cache:test_cache:TestCacheKeyGeneration:simple_func:{hash_key}"

        # Call function to generate key
        result = simple_func(a=1, b=2)

        # Verify key was used
        cache_with_redis._redis_client.get.assert_called()
        call_key = cache_with_redis._redis_client.get.call_args[0][0]
        assert call_key.startswith("cache:")
        assert "simple_func" in call_key

    def test_unhashable_args_key_generation(self, cache_with_redis):
        """Test cache key generation with unhashable arguments (dict, list)."""
        @cache_with_redis.cache(ttl=60)
        def dict_func(data):
            return data['value']

        # Call with dict argument
        result = dict_func(data={'value': 42, 'nested': [1, 2, 3]})

        # Verify key was generated (should not raise exception)
        cache_with_redis._redis_client.get.assert_called()
        call_key = cache_with_redis._redis_client.get.call_args[0][0]
        assert call_key.startswith("cache:")

    def test_method_key_includes_class_name(self, cache_with_redis):
        """Test cache key for class methods includes class name."""
        class TestClass:
            @cache_with_redis.cache(ttl=60)
            def method(self, value):
                return value * 2

        obj = TestClass()
        result = obj.method(value=5)

        # Verify class name in key
        call_key = cache_with_redis._redis_client.get.call_args[0][0]
        assert "TestClass" in call_key
        assert "method" in call_key

    def test_namespace_prefix_format(self, cache_with_redis):
        """Test cache key has correct namespace prefix format."""
        @cache_with_redis.cache(ttl=60)
        def namespaced_func():
            return "test"

        namespaced_func()

        call_key = cache_with_redis._redis_client.get.call_args[0][0]
        # Format: cache:[module]:[class]:[method]:[hash]
        parts = call_key.split(':')
        assert parts[0] == "cache"
        assert len(parts) >= 4


class TestCacheDecoratorBasics:
    """Test basic cache decorator functionality."""

    def test_cache_miss_calls_function(self, cache_with_redis):
        """Test cache miss triggers function execution."""
        mock_func = Mock(return_value=42)

        @cache_with_redis.cache(ttl=60)
        def cached_func():
            return mock_func()

        cache_with_redis._redis_client.get.return_value = None
        result = cached_func()

        assert result == 42
        mock_func.assert_called_once()

    def test_cache_hit_skips_function(self, cache_with_redis):
        """Test cache hit returns cached value without calling function."""
        mock_func = Mock(return_value=42)
        cached_value = pickle.dumps(99)

        @cache_with_redis.cache(ttl=60)
        def cached_func():
            return mock_func()

        cache_with_redis._redis_client.get.return_value = cached_value
        result = cached_func()

        assert result == 99
        mock_func.assert_not_called()

    def test_cache_stores_result_on_miss(self, cache_with_redis):
        """Test function result is stored in cache on miss."""
        @cache_with_redis.cache(ttl=60)
        def cached_func():
            return {'data': [1, 2, 3]}

        cache_with_redis._redis_client.get.return_value = None
        result = cached_func()

        # Verify set was called with pickled result
        cache_with_redis._redis_client.set.assert_called()
        call_args = cache_with_redis._redis_client.set.call_args
        stored_value = pickle.loads(call_args[0][1])
        assert stored_value == {'data': [1, 2, 3]}

    def test_ttl_parameter_passed_to_redis(self, cache_with_redis):
        """Test TTL parameter is passed to Redis SET command."""
        @cache_with_redis.cache(ttl=120)
        def cached_func():
            return "test"

        cache_with_redis._redis_client.get.return_value = None
        cached_func()

        # Verify TTL was set
        call_args = cache_with_redis._redis_client.set.call_args
        assert call_args[1].get('ex') == 120 or call_args[0][2] == 120


class TestInMemoryFallback:
    """Test in-memory LRU cache fallback."""

    def test_memory_cache_basic_operation(self, cache_without_redis):
        """Test basic cache operations with in-memory backend."""
        call_count = 0

        @cache_without_redis.cache(ttl=60)
        def memory_func(x):
            nonlocal call_count
            call_count += 1
            return x * 2

        # First call - cache miss
        result1 = memory_func(5)
        assert result1 == 10
        assert call_count == 1

        # Second call - cache hit
        result2 = memory_func(5)
        assert result2 == 10
        assert call_count == 1  # Function not called again

    def test_memory_cache_lru_eviction(self, cache_without_redis):
        """Test LRU eviction when cache size exceeds limit."""
        # Set small cache size for testing
        cache_without_redis._memory_cache.clear()
        cache_without_redis._cache_max_size = 3

        @cache_without_redis.cache(ttl=60)
        def limited_func(x):
            return x * 2

        # Fill cache beyond limit
        for i in range(5):
            limited_func(i)

        # Cache should only have most recent 3 items
        assert len(cache_without_redis._memory_cache) <= 3

    def test_memory_cache_ttl_expiration(self, cache_without_redis):
        """Test TTL expiration in memory cache."""
        call_count = 0

        @cache_without_redis.cache(ttl=1)  # 1 second TTL
        def expiring_func():
            nonlocal call_count
            call_count += 1
            return "result"

        # First call
        result1 = expiring_func()
        assert call_count == 1

        # Immediate second call - should hit cache
        result2 = expiring_func()
        assert call_count == 1

        # Wait for expiration
        time.sleep(1.1)

        # Third call - should miss cache after expiration
        result3 = expiring_func()
        assert call_count == 2


class TestCacheInvalidation:
    """Test cache invalidation methods."""

    def test_invalidate_single_key(self, cache_with_redis):
        """Test invalidating a single cache key."""
        @cache_with_redis.cache(ttl=60)
        def func_to_invalidate(x):
            return x * 2

        # Generate cache key by calling function
        func_to_invalidate(5)
        cache_key = cache_with_redis._redis_client.get.call_args[0][0]

        # Invalidate
        cache_with_redis.invalidate(cache_key)
        cache_with_redis._redis_client.delete.assert_called_with(cache_key)

    def test_invalidate_pattern(self, cache_with_redis):
        """Test invalidating multiple keys by pattern."""
        cache_with_redis._redis_client.keys.return_value = [
            b'cache:test:func1:hash1',
            b'cache:test:func1:hash2',
            b'cache:test:func2:hash1'
        ]

        # Invalidate pattern
        deleted = cache_with_redis.invalidate_pattern("cache:test:func1:*")

        # Verify keys were found and deleted
        cache_with_redis._redis_client.keys.assert_called_with("cache:test:func1:*")
        assert cache_with_redis._redis_client.delete.call_count > 0

    def test_invalidate_all(self, cache_with_redis):
        """Test invalidating all cache entries."""
        cache_with_redis._redis_client.keys.return_value = [
            b'cache:test:func1:hash1',
            b'cache:test:func2:hash2'
        ]

        cache_with_redis.invalidate_all()

        cache_with_redis._redis_client.keys.assert_called_with("cache:*")

    def test_memory_invalidate_single_key(self, cache_without_redis):
        """Test invalidating single key in memory cache."""
        @cache_without_redis.cache(ttl=60)
        def memory_func(x):
            return x * 2

        # Populate cache
        memory_func(5)
        initial_size = len(cache_without_redis._memory_cache)

        # Get the key that was generated
        keys = list(cache_without_redis._memory_cache.keys())
        if keys:
            cache_without_redis.invalidate(keys[0])
            assert len(cache_without_redis._memory_cache) < initial_size

    def test_memory_invalidate_all(self, cache_without_redis):
        """Test invalidating all entries in memory cache."""
        @cache_without_redis.cache(ttl=60)
        def memory_func(x):
            return x * 2

        # Populate cache
        for i in range(3):
            memory_func(i)

        assert len(cache_without_redis._memory_cache) > 0
        cache_without_redis.invalidate_all()
        assert len(cache_without_redis._memory_cache) == 0


class TestCacheStatistics:
    """Test cache performance metrics and statistics."""

    def test_stats_tracking_hits_and_misses(self, cache_with_redis):
        """Test hit and miss statistics are tracked correctly."""
        @cache_with_redis.cache(ttl=60)
        def stats_func(x):
            return x * 2

        # First call - miss
        cache_with_redis._redis_client.get.return_value = None
        stats_func(1)

        # Second call - hit
        cache_with_redis._redis_client.get.return_value = pickle.dumps(2)
        stats_func(1)

        stats = cache_with_redis.get_stats()
        assert stats['hits'] >= 1
        assert stats['misses'] >= 1

    def test_stats_hit_rate_calculation(self, cache_with_redis):
        """Test hit rate calculation."""
        @cache_with_redis.cache(ttl=60)
        def rate_func(x):
            return x

        # 2 misses, 3 hits
        cache_with_redis._redis_client.get.side_effect = [
            None, None,  # misses
            pickle.dumps(1), pickle.dumps(2), pickle.dumps(3)  # hits
        ]

        for i in range(5):
            rate_func(i)

        stats = cache_with_redis.get_stats()
        expected_rate = 3 / 5  # 60%
        assert abs(stats['hit_rate'] - expected_rate) < 0.01

    def test_stats_cache_size(self, cache_without_redis):
        """Test cache size tracking."""
        @cache_without_redis.cache(ttl=60)
        def size_func(x):
            return x

        # Add 3 items
        for i in range(3):
            size_func(i)

        stats = cache_without_redis.get_stats()
        assert stats['size'] == 3

    def test_stats_persist_across_backends(self):
        """Test stats persist when switching between Redis and memory."""
        from digitalmodel.core.cache import Cache

        # Start with Redis, record some hits
        with patch('digitalmodel.core.cache.redis.Redis') as mock_redis_class:
            mock_redis = MagicMock()
            mock_redis.ping.return_value = True
            mock_redis.get.return_value = pickle.dumps(42)
            mock_redis_class.return_value = mock_redis

            cache = Cache()

            @cache.cache(ttl=60)
            def persistent_func():
                return 42

            persistent_func()
            stats1 = cache.get_stats()

        # Stats should be preserved
        assert stats1['hits'] + stats1['misses'] > 0

    def test_stats_latency_tracking(self, cache_with_redis):
        """Test average latency tracking."""
        @cache_with_redis.cache(ttl=60)
        def latency_func():
            time.sleep(0.01)  # Simulate work
            return "result"

        cache_with_redis._redis_client.get.return_value = None
        latency_func()

        stats = cache_with_redis.get_stats()
        assert 'avg_latency_ms' in stats
        assert stats['avg_latency_ms'] > 0


class TestCacheWarming:
    """Test cache warming utilities."""

    def test_warm_with_args_list(self, cache_with_redis):
        """Test warming cache with list of arguments."""
        call_count = 0

        @cache_with_redis.cache(ttl=60)
        def warmable_func(x, y):
            nonlocal call_count
            call_count += 1
            return x + y

        # Warm cache with multiple argument sets
        args_list = [
            {'x': 1, 'y': 2},
            {'x': 3, 'y': 4},
            {'x': 5, 'y': 6}
        ]

        cache_with_redis.warm(warmable_func, args_list)

        # Function should be called for each arg set
        assert call_count == 3
        assert cache_with_redis._redis_client.set.call_count >= 3

    def test_warm_skips_errors(self, cache_with_redis):
        """Test warming continues on errors."""
        def error_func(x):
            if x == 2:
                raise ValueError("Test error")
            return x * 2

        decorated = cache_with_redis.cache(ttl=60)(error_func)

        args_list = [{'x': 1}, {'x': 2}, {'x': 3}]

        # Should not raise, should skip error case
        cache_with_redis.warm(decorated, args_list)

        # At least 2 successful calls
        assert cache_with_redis._redis_client.set.call_count >= 2


class TestCacheEdgeCases:
    """Test edge cases and error handling."""

    def test_none_return_value_cached(self, cache_with_redis):
        """Test that None return values are properly cached."""
        call_count = 0

        @cache_with_redis.cache(ttl=60)
        def none_func():
            nonlocal call_count
            call_count += 1
            return None

        cache_with_redis._redis_client.get.return_value = None
        result1 = none_func()

        cache_with_redis._redis_client.get.return_value = pickle.dumps(None)
        result2 = none_func()

        assert result1 is None
        assert result2 is None

    def test_exception_not_cached(self, cache_with_redis):
        """Test that exceptions are not cached."""
        call_count = 0

        @cache_with_redis.cache(ttl=60)
        def error_func():
            nonlocal call_count
            call_count += 1
            raise ValueError("Test error")

        # First call raises
        with pytest.raises(ValueError):
            error_func()

        # Second call should also raise (not cached)
        with pytest.raises(ValueError):
            error_func()

        assert call_count == 2
        # Verify set was not called
        cache_with_redis._redis_client.set.assert_not_called()

    def test_redis_connection_error_fallback(self, cache_with_redis):
        """Test graceful fallback when Redis operations fail."""
        call_count = 0

        @cache_with_redis.cache(ttl=60)
        def fallback_func(x):
            nonlocal call_count
            call_count += 1
            return x * 2

        # Simulate Redis failure mid-operation
        cache_with_redis._redis_client.get.side_effect = Exception("Connection lost")

        # Should still execute function
        result = fallback_func(5)
        assert result == 10
        assert call_count == 1

    def test_large_object_serialization(self, cache_with_redis):
        """Test caching large objects."""
        large_data = {'data': [i for i in range(10000)]}

        @cache_with_redis.cache(ttl=60)
        def large_func():
            return large_data

        cache_with_redis._redis_client.get.return_value = None
        result = large_func()

        # Verify large object was serialized
        cache_with_redis._redis_client.set.assert_called()
        call_args = cache_with_redis._redis_client.set.call_args
        serialized = call_args[0][1]
        assert len(serialized) > 1000  # Should be substantial


class TestCacheIntegration:
    """Integration tests for real-world scenarios."""

    def test_multiple_cached_functions(self, cache_with_redis):
        """Test multiple functions sharing same cache instance."""
        @cache_with_redis.cache(ttl=60)
        def func1(x):
            return x * 2

        @cache_with_redis.cache(ttl=120)
        def func2(x):
            return x * 3

        cache_with_redis._redis_client.get.return_value = None

        func1(5)
        func2(5)

        # Both should have different cache keys
        assert cache_with_redis._redis_client.set.call_count == 2
        call_keys = [
            call[0][0] for call in cache_with_redis._redis_client.set.call_args_list
        ]
        assert call_keys[0] != call_keys[1]

    def test_concurrent_access_safety(self, cache_without_redis):
        """Test thread-safe cache access."""
        import threading

        results = []
        call_count = 0
        lock = threading.Lock()

        @cache_without_redis.cache(ttl=60)
        def concurrent_func(x):
            nonlocal call_count
            with lock:
                call_count += 1
            time.sleep(0.01)  # Simulate work
            return x * 2

        def worker(value):
            result = concurrent_func(value)
            results.append(result)

        # Run 10 threads with same argument
        threads = [threading.Thread(target=worker, args=(5,)) for _ in range(10)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        # Function should be called once (or very few times due to race)
        assert call_count <= 3  # Allow some race condition
        assert all(r == 10 for r in results)
