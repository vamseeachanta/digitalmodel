"""#1282: the skip-on-network-error helper must SKIP (not error) on a live
5xx/connection failure, and must NOT swallow real bugs."""
from __future__ import annotations

import _pytest.outcomes
import pytest
import requests

from tests.metocean.netskip import skip_on_network_error


def test_skips_on_exhausted_5xx_retry_error():
    # base_client's Retry raises RetryError (a RequestException, not HTTPError)
    with pytest.raises(_pytest.outcomes.Skipped):
        with skip_on_network_error("test"):
            raise requests.exceptions.RetryError("simulated exhausted 5xx")


def test_skips_on_connection_error():
    with pytest.raises(_pytest.outcomes.Skipped):
        with skip_on_network_error("test"):
            raise requests.exceptions.ConnectionError("no route")


def test_does_not_swallow_non_network_bugs():
    with pytest.raises(ValueError):
        with skip_on_network_error("test"):
            raise ValueError("a real bug must propagate, not skip")
