"""
Tests for data_systems.data_procurement.common.cache_manager.

Focuses on the L1MemoryCache TTL logic, LRU eviction, and the multi-tier
CacheManager cascade — all pure-Python paths that do NOT require Redis.
"""

import time
import pytest


# ---------------------------------------------------------------------------
# L1MemoryCache — core behaviour
# ---------------------------------------------------------------------------


def _make_l1(max_size=100, default_ttl=3600):
    from digitalmodel.data_systems.data_procurement.common.cache_manager import (
        L1MemoryCache,
    )
    return L1MemoryCache(max_size=max_size, default_ttl=default_ttl)


class TestL1MemoryCacheBasic:
    """Tests for basic set/get/delete/clear operations."""

    def test_get_missing_key_returns_none(self):
        cache = _make_l1()
        assert cache.get("no-such-key") is None

    def test_set_and_get_string_value(self):
        cache = _make_l1()
        cache.set("k1", "hello")
        assert cache.get("k1") == "hello"

    def test_set_and_get_dict_value(self):
        cache = _make_l1()
        cache.set("cfg", {"a": 1, "b": 2})
        assert cache.get("cfg") == {"a": 1, "b": 2}

    def test_set_and_get_list_value(self):
        cache = _make_l1()
        cache.set("items", [10, 20, 30])
        assert cache.get("items") == [10, 20, 30]

    def test_set_and_get_numeric_value(self):
        cache = _make_l1()
        cache.set("pi", 3.14159)
        assert cache.get("pi") == pytest.approx(3.14159)

    def test_set_and_get_none_value(self):
        # None is a valid cache value; get() should return the stored None.
        # However, the implementation returns None for both "missing" and "stored None".
        # We verify at least that setting doesn't raise.
        cache = _make_l1()
        cache.set("null_key", None)
        # Behaviour: get() returns None (same as cache miss for None values)
        result = cache.get("null_key")
        assert result is None

    def test_overwrite_existing_key(self):
        cache = _make_l1()
        cache.set("x", "first")
        cache.set("x", "second")
        assert cache.get("x") == "second"

    def test_delete_existing_key(self):
        cache = _make_l1()
        cache.set("y", 99)
        cache.delete("y")
        assert cache.get("y") is None

    def test_delete_missing_key_is_noop(self):
        cache = _make_l1()
        cache.delete("phantom")  # Should not raise

    def test_clear_removes_all_entries(self):
        cache = _make_l1()
        cache.set("a", 1)
        cache.set("b", 2)
        cache.clear()
        assert cache.get("a") is None
        assert cache.get("b") is None

    def test_multiple_keys_are_independent(self):
        cache = _make_l1()
        cache.set("k1", "v1")
        cache.set("k2", "v2")
        assert cache.get("k1") == "v1"
        assert cache.get("k2") == "v2"


class TestL1MemoryCacheTTL:
    """Tests for TTL expiry logic."""

    def test_entry_not_expired_within_ttl(self):
        cache = _make_l1(default_ttl=10)
        cache.set("key", "value")
        assert cache.get("key") == "value"

    def test_expired_entry_returns_none(self):
        cache = _make_l1(default_ttl=3600)
        cache.set("stale", "data", ttl=1)
        # Force expiry by manipulating the stored expires_at
        from datetime import datetime, timedelta
        cache.cache["stale"]["expires_at"] = datetime.now() - timedelta(seconds=1)
        assert cache.get("stale") is None

    def test_expired_entry_is_removed_from_cache(self):
        cache = _make_l1()
        cache.set("old", "value", ttl=1)
        from datetime import datetime, timedelta
        cache.cache["old"]["expires_at"] = datetime.now() - timedelta(seconds=1)
        cache.get("old")  # Trigger expiry check
        assert "old" not in cache.cache

    def test_custom_ttl_overrides_default(self):
        # Per-key TTL is stored; verify it was applied
        cache = _make_l1(default_ttl=3600)
        cache.set("k", "v", ttl=60)
        from datetime import datetime, timedelta
        expires_at = cache.cache["k"]["expires_at"]
        remaining = (expires_at - datetime.now()).total_seconds()
        assert 50 <= remaining <= 65  # ~60 seconds remaining


class TestL1MemoryCacheEviction:
    """Tests for LRU-style eviction when max_size is reached."""

    def test_eviction_on_max_size_reached(self):
        cache = _make_l1(max_size=3)
        cache.set("a", 1)
        cache.set("b", 2)
        cache.set("c", 3)
        # Cache is full; adding "d" should evict the oldest entry
        cache.set("d", 4)
        assert len(cache.cache) == 3

    def test_new_entry_present_after_eviction(self):
        cache = _make_l1(max_size=2)
        cache.set("x", 10)
        cache.set("y", 20)
        cache.set("z", 30)
        assert cache.get("z") == 30

    def test_cache_size_stays_at_max(self):
        cache = _make_l1(max_size=5)
        for i in range(20):
            cache.set(f"key_{i}", i)
        assert len(cache.cache) == 5

    def test_update_existing_key_does_not_evict(self):
        cache = _make_l1(max_size=2)
        cache.set("a", 1)
        cache.set("b", 2)
        cache.set("a", 99)  # Update, not insert
        assert len(cache.cache) == 2
        assert cache.get("a") == 99
        assert cache.get("b") == 2


# ---------------------------------------------------------------------------
# CacheManager (L1 only) — cascade behaviour
# ---------------------------------------------------------------------------


def _make_manager(max_size=100):
    from digitalmodel.data_systems.data_procurement.common.cache_manager import (
        CacheManager,
    )
    return CacheManager(enable_l1=True, enable_l2=False, enable_l3=False,
                        l1={"max_size": max_size})


class TestCacheManagerL1Only:
    """Tests for CacheManager with only L1 enabled."""

    def test_get_miss_returns_none(self):
        mgr = _make_manager()
        assert mgr.get("missing") is None

    def test_set_and_get_value(self):
        mgr = _make_manager()
        mgr.set("key", {"result": 42})
        assert mgr.get("key") == {"result": 42}

    def test_delete_removes_entry(self):
        mgr = _make_manager()
        mgr.set("target", "data")
        mgr.delete("target")
        assert mgr.get("target") is None

    def test_clear_removes_all(self):
        mgr = _make_manager()
        mgr.set("a", 1)
        mgr.set("b", 2)
        mgr.clear()
        assert mgr.get("a") is None
        assert mgr.get("b") is None

    def test_set_with_ttl_is_respected(self):
        mgr = _make_manager()
        mgr.set("k", "v", ttl=100)
        assert mgr.get("k") == "v"

    def test_set_to_specific_l1_tier(self):
        mgr = _make_manager()
        mgr.set("k", "val", tiers=["L1"])
        assert mgr.get("k") == "val"

    def test_has_l1_tier(self):
        mgr = _make_manager()
        assert "L1" in mgr.tiers

    def test_no_l2_when_disabled(self):
        mgr = _make_manager()
        assert "L2" not in mgr.tiers

    def test_no_l3_when_disabled(self):
        mgr = _make_manager()
        assert "L3" not in mgr.tiers
