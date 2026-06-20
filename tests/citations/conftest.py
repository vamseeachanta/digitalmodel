"""Shared fixtures for the citations test tree (#618).

Finding (c): the resolver's `_RESOLUTION_CACHE` is module-global state (used to
emit the DIGITALMODEL_REPO_ROOT deprecation warning at most once per process).
Without isolation, cross-test-file ordering can leak the `deprecation_warned`
flag — a test that expects the warning runs after one that already set it, and
the warning is silently suppressed (or vice-versa). Each individual test file
previously reset the cache with its own autouse fixture; centralizing the reset
here guarantees every test in the citations tree starts and ends with a clean
cache regardless of collection order, and covers files (e.g. test_schema.py,
test_registry.py) that did not reset it themselves.
"""
from __future__ import annotations

import pytest


@pytest.fixture(autouse=True)
def reset_resolution_cache():
    """Give every citations test a fresh resolver cache (no cross-test leakage)."""
    from digitalmodel.citations import resolver

    resolver._RESOLUTION_CACHE.clear()
    yield
    resolver._RESOLUTION_CACHE.clear()
