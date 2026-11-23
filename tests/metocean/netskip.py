"""skip-on-network-error helper for live metocean tests (#1282).

CI must never FLAKE on a live metocean fetch. The metocean provider clients
retry 5xx and then, on exhaustion, raise ``requests.exceptions.RetryError`` (a
``RequestException`` subclass — NOT an ``HTTPError``); connection loss raises
``ConnectionError``/``Timeout``. All are ``RequestException`` subclasses, so we
catch the base and skip cleanly (mirroring the repo's ``importorskip`` "skip
clean, never error" convention for licensed software).

The provider clients are GENERATORS — the HTTP call fires on iteration, not when
the generator is created — so the iterator MUST be consumed inside the guard:

    with skip_on_network_error("NOAA NDBC"):
        records = list(client.query_by_date(...))   # consume INSIDE
"""
from __future__ import annotations

import contextlib

import pytest
import requests


@contextlib.contextmanager
def skip_on_network_error(what: str = "live metocean API"):
    """Skip (never fail) the test if a live fetch cannot complete."""
    try:
        yield
    except requests.exceptions.RequestException as exc:
        pytest.skip(f"{what} unreachable ({type(exc).__name__}) — skipping live test")
