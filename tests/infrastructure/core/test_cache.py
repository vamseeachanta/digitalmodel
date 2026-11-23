"""
ABOUTME: CacheClient test suite for memory and Redis-backed caching.
Exercises the production cache API, decorator, TTL, invalidation, and stats.
"""

import json
import pickle
import sys
import types
from unittest.mock import MagicMock

import pytest

from digitalmodel.infrastructure.core.cache import (
    CacheClient,
    CacheConfig,
    CacheStats,
    cache_result,
)

cache_module = sys.modules[CacheClient.__module__]


@pytest.fixture
def memory_client():
    """Create a CacheClient with the memory backend forced on."""
    config = CacheConfig(enable_redis=False, max_memory_items=3, default_ttl=60)
    return CacheClient(config)


@pytest.fixture
def redis_client():
    """Create a CacheClient backed by fakeredis."""
    fakeredis = pytest.importorskip("fakeredis")

    client = CacheClient(CacheConfig(enable_redis=False, default_ttl=60))
    client.redis = fakeredis.FakeRedis(decode_responses=False)
    client.backend = "redis"
    return client


class TestCacheConfig:
    """Test cache configuration."""

    def test_default_config_matches_production_defaults(self):
        config = CacheConfig()

        assert config.redis_host == "localhost"
        assert config.redis_port == 6379
        assert config.redis_db == 0
        assert config.redis_password is None
        assert config.default_ttl == 3600
        assert config.max_memory_items == 1000
        assert config.enable_redis is True

    def test_custom_config_is_preserved(self):
        config = CacheConfig(
            redis_host="cache.local",
            redis_port=6380,
            redis_db=2,
            redis_password="secret",
            default_ttl=120,
            max_memory_items=10,
            enable_redis=False,
        )

        assert config.redis_host == "cache.local"
        assert config.redis_port == 6380
        assert config.redis_db == 2
        assert config.redis_password == "secret"
        assert config.default_ttl == 120
        assert config.max_memory_items == 10
        assert config.enable_redis is False


class TestCacheStats:
    """Test cache statistics value object."""

    def test_stats_initialise_to_zero(self):
        stats = CacheStats()

        assert stats.hits == 0
        assert stats.misses == 0
        assert stats.sets == 0
        assert stats.deletes == 0
        assert stats.errors == 0
        assert stats.hit_rate == 0.0

    def test_hit_rate_uses_hits_and_misses(self):
        stats = CacheStats(hits=3, misses=1)

        assert stats.hit_rate == pytest.approx(0.75)

    def test_to_dict_exports_supported_metrics(self):
        stats = CacheStats(hits=2, misses=3, sets=4, deletes=5, errors=6)

        assert stats.to_dict() == {
            "hits": 2,
            "misses": 3,
            "sets": 4,
            "deletes": 5,
            "errors": 6,
            "hit_rate": pytest.approx(0.4),
        }


class TestCacheClientInitialization:
    """Test backend selection during CacheClient construction."""

    def test_memory_backend_when_redis_disabled(self):
        client = CacheClient(CacheConfig(enable_redis=False, max_memory_items=7))

        assert client.backend == "memory"
        assert client.redis is None
        assert client.memory_cache.max_items == 7

    def test_redis_backend_when_connection_succeeds(self, monkeypatch):
        redis_module = types.ModuleType("redis")
        created = {}

        class FakeRedisConnection:
            def __init__(self, **kwargs):
                created["kwargs"] = kwargs

            def ping(self):
                return True

        redis_module.Redis = FakeRedisConnection
        monkeypatch.setitem(sys.modules, "redis", redis_module)

        config = CacheConfig(
            redis_host="cache.local",
            redis_port=6380,
            redis_db=4,
            redis_password="pw",
            enable_redis=True,
        )
        client = CacheClient(config)

        assert client.backend == "redis"
        assert isinstance(client.redis, FakeRedisConnection)
        assert created["kwargs"]["host"] == "cache.local"
        assert created["kwargs"]["port"] == 6380
        assert created["kwargs"]["db"] == 4
        assert created["kwargs"]["password"] == "pw"
        assert created["kwargs"]["decode_responses"] is False

    def test_falls_back_to_memory_when_redis_connection_fails(self, monkeypatch):
        redis_module = types.ModuleType("redis")

        class FailingRedisConnection:
            def __init__(self, **_kwargs):
                raise RuntimeError("redis unavailable")

        redis_module.Redis = FailingRedisConnection
        monkeypatch.setitem(sys.modules, "redis", redis_module)

        client = CacheClient(CacheConfig(enable_redis=True, max_memory_items=5))

        assert client.backend == "memory"
        assert client.redis is None
        assert client.memory_cache.max_items == 5


class TestMemoryBackend:
    """Test CacheClient against the in-memory LRU backend."""

    def test_set_get_delete_and_clear(self, memory_client):
        memory_client.set("alpha", {"value": 1})
        memory_client.set("beta", [1, 2, 3])

        assert memory_client.get("alpha") == {"value": 1}
        assert memory_client.get("beta") == [1, 2, 3]

        memory_client.delete("alpha")
        assert memory_client.get("alpha") is None

        memory_client.clear()
        assert memory_client.get("beta") is None

    def test_missing_key_returns_none_and_records_miss(self, memory_client):
        assert memory_client.get("missing") is None

        stats = memory_client.get_stats()
        assert stats["misses"] == 1
        assert stats["hits"] == 0

    def test_default_ttl_is_applied_to_memory_entries(self, monkeypatch):
        now = [1000.0]
        monkeypatch.setattr(cache_module.time, "time", lambda: now[0])

        client = CacheClient(CacheConfig(enable_redis=False, default_ttl=10))
        client.set("short-lived", "value")

        assert client.get("short-lived") == "value"

        now[0] = 1011.0
        assert client.get("short-lived") is None

    def test_explicit_ttl_controls_memory_expiration(self, monkeypatch, memory_client):
        now = [2000.0]
        monkeypatch.setattr(cache_module.time, "time", lambda: now[0])

        memory_client.set("short-lived", "value", ttl=5)

        now[0] = 2004.0
        assert memory_client.get("short-lived") == "value"

        now[0] = 2006.0
        assert memory_client.get("short-lived") is None

    def test_lru_eviction_removes_least_recently_used_entry(self, memory_client):
        memory_client.set("one", 1)
        memory_client.set("two", 2)
        memory_client.set("three", 3)

        assert memory_client.get("one") == 1
        memory_client.set("four", 4)

        assert memory_client.get("one") == 1
        assert memory_client.get("two") is None
        assert memory_client.get("three") == 3
        assert memory_client.get("four") == 4

    def test_stats_track_supported_operations(self, memory_client):
        memory_client.set("key", "value")
        memory_client.get("key")
        memory_client.get("missing")
        memory_client.delete("key")

        assert memory_client.get_stats() == {
            "hits": 1,
            "misses": 1,
            "sets": 1,
            "deletes": 1,
            "errors": 0,
            "hit_rate": pytest.approx(0.5),
        }

    def test_reset_stats_replaces_counters(self, memory_client):
        memory_client.set("key", "value")
        memory_client.get("key")

        memory_client.reset_stats()

        assert memory_client.get_stats() == {
            "hits": 0,
            "misses": 0,
            "sets": 0,
            "deletes": 0,
            "errors": 0,
            "hit_rate": 0.0,
        }

    def test_export_stats_writes_json(self, tmp_path, memory_client):
        memory_client.set("key", "value")
        memory_client.get("key")
        memory_client.get("missing")

        stats_path = tmp_path / "cache-stats.json"
        memory_client.export_stats(str(stats_path))

        assert json.loads(stats_path.read_text()) == {
            "hits": 1,
            "misses": 1,
            "sets": 1,
            "deletes": 0,
            "errors": 0,
            "hit_rate": 0.5,
        }


class TestRedisBackend:
    """Test CacheClient against a fakeredis backend."""

    def test_redis_set_get_uses_pickle_serialization(self, redis_client):
        value = {"array": [1, 2, 3], "nested": {"ok": True}}

        redis_client.set("payload", value)

        raw_value = redis_client.redis.get("payload")
        assert isinstance(raw_value, bytes)
        assert pickle.loads(raw_value) == value
        assert redis_client.get("payload") == value

    def test_redis_delete_and_clear(self, redis_client):
        redis_client.set("one", 1)
        redis_client.set("two", 2)

        redis_client.delete("one")
        assert redis_client.get("one") is None
        assert redis_client.get("two") == 2

        redis_client.clear()
        assert redis_client.get("two") is None

    def test_redis_ttl_expiration(self, redis_client):
        redis_client.set("short-lived", "value", ttl=1)

        assert redis_client.get("short-lived") == "value"
        redis_client.redis.expire("short-lived", 0)
        assert redis_client.get("short-lived") is None

    def test_redis_errors_are_counted_and_do_not_raise(self):
        client = CacheClient(CacheConfig(enable_redis=False))
        client.backend = "redis"
        client.redis = MagicMock()
        client.redis.get.side_effect = RuntimeError("connection lost")

        assert client.get("key") is None

        stats = client.get_stats()
        assert stats["errors"] == 1
        assert stats["hits"] == 0
        assert stats["misses"] == 0


class TestCacheDecorator:
    """Test the cache_result decorator."""

    def test_decorator_caches_by_arguments(self, memory_client):
        call_count = 0

        @cache_result(memory_client, ttl=60)
        def expensive_sum(left, right):
            nonlocal call_count
            call_count += 1
            return left + right

        assert expensive_sum(1, 2) == 3
        assert expensive_sum(1, 2) == 3
        assert expensive_sum(2, 3) == 5

        assert call_count == 2
        assert memory_client.get_stats()["hits"] == 1

    def test_decorator_handles_kwargs_as_distinct_keys(self, memory_client):
        call_count = 0

        @cache_result(memory_client, ttl=60)
        def scale(value, *, factor=1):
            nonlocal call_count
            call_count += 1
            return value * factor

        assert scale(2, factor=3) == 6
        assert scale(2, factor=3) == 6
        assert scale(2, factor=4) == 8

        assert call_count == 2
        assert memory_client.get_stats()["sets"] == 2

    def test_decorator_uses_custom_key_prefix(self, memory_client):
        @cache_result(memory_client, ttl=60, key_prefix="custom-prefix")
        def double(value):
            return value * 2

        assert double(5) == 10
        assert memory_client.get("custom-prefix:(5,)") == 10

    def test_decorator_preserves_wrapped_function_metadata(self, memory_client):
        @cache_result(memory_client)
        def named_function():
            """Original docstring."""
            return "ok"

        assert named_function.__name__ == "named_function"
        assert named_function.__doc__ == "Original docstring."

    def test_exceptions_are_not_cached(self, memory_client):
        call_count = 0

        @cache_result(memory_client, ttl=60)
        def failing_function():
            nonlocal call_count
            call_count += 1
            raise ValueError("boom")

        with pytest.raises(ValueError):
            failing_function()
        with pytest.raises(ValueError):
            failing_function()

        assert call_count == 2
        assert memory_client.get_stats()["sets"] == 0


class TestCacheWarming:
    """Test CacheClient cache warming."""

    def test_warm_populates_cache_for_args(self, memory_client):
        def compute(value):
            return value * 2

        memory_client.warm(compute, [(1,), (2,), (3,)])

        assert memory_client.get("compute:(1,)") == 2
        assert memory_client.get("compute:(2,)") == 4
        assert memory_client.get("compute:(3,)") == 6

    def test_warm_populates_cache_for_kwargs(self, memory_client):
        def compute(value, factor=1):
            return value * factor

        memory_client.warm(
            compute,
            [(2,), (3,)],
            [{"factor": 5}, {"factor": 7}],
        )

        assert memory_client.get("compute:(2,):{'factor': 5}") == 10
        assert memory_client.get("compute:(3,):{'factor': 7}") == 21

    def test_warm_continues_after_function_error(self, memory_client):
        calls = []

        def compute(value):
            calls.append(value)
            if value == 2:
                raise ValueError("bad input")
            return value * 2

        memory_client.warm(compute, [(1,), (2,), (3,)])

        assert calls == [1, 2, 3]
        assert memory_client.get("compute:(1,)") == 2
        assert memory_client.get("compute:(2,)") is None
        assert memory_client.get("compute:(3,)") == 6


class TestSupportedInvalidation:
    """Test invalidation operations that CacheClient actually exposes."""

    def test_delete_invalidates_single_memory_key(self, memory_client):
        memory_client.set("target", "value")

        memory_client.delete("target")

        assert memory_client.get("target") is None

    def test_clear_invalidates_all_memory_keys(self, memory_client):
        memory_client.set("one", 1)
        memory_client.set("two", 2)

        memory_client.clear()

        assert memory_client.get("one") is None
        assert memory_client.get("two") is None

    def test_delete_invalidates_single_redis_key(self, redis_client):
        redis_client.set("target", "value")

        redis_client.delete("target")

        assert redis_client.get("target") is None


class TestIntegrationScenarios:
    """Realistic cache scenarios using the production API."""

    def test_multiple_cached_functions_share_client_with_distinct_keys(
        self, memory_client
    ):
        @cache_result(memory_client, ttl=60)
        def added_mass(period):
            return {"period": period, "added_mass": [period * 0.1]}

        @cache_result(memory_client, ttl=60)
        def damping(period):
            return {"period": period, "damping": [period * 0.01]}

        assert added_mass(10.0) == {"period": 10.0, "added_mass": [1.0]}
        assert damping(10.0) == {"period": 10.0, "damping": [0.1]}
        assert added_mass(10.0) == {"period": 10.0, "added_mass": [1.0]}
        assert damping(10.0) == {"period": 10.0, "damping": [0.1]}

        assert memory_client.get_stats()["sets"] == 2
        assert memory_client.get_stats()["hits"] == 2

    def test_large_object_round_trips_through_redis(self, redis_client):
        large_payload = {"values": list(range(1000))}

        redis_client.set("large", large_payload)

        assert redis_client.get("large") == large_payload
