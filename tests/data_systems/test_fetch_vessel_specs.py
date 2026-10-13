# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline retrieval contracts; no transport reaches a real site."""

import json
import runpy
from copy import deepcopy
from pathlib import Path
from tempfile import TemporaryDirectory

import pytest

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "assets/data/vessel_specs_provenance.json"


@pytest.fixture(name="tmp_path")
def cleaned_output(tmp_path):
    """Remove each test's output while using pytest's external temporary root."""
    with TemporaryDirectory(dir=tmp_path) as directory:
        yield Path(directory)


@pytest.fixture
def fetcher():
    script = ROOT / "scripts/fetch_vessel_specs.py"
    assert script.exists(), "Reproducible retrieval helper is required"
    return runpy.run_path(str(script))["retrieve"]


@pytest.fixture
def template():
    original = json.loads(MANIFEST.read_text())
    result = deepcopy(original)
    result.pop("assembly", None)
    for index, source in enumerate(result["sources"]):
        suffix = "robots.txt" if source["role"] == "access_policy" else str(index)
        source["source_url"] = f"https://example.org/{suffix}"
    # Two independent sites with their own access policies.
    for source in result["sources"][3:]:
        source["source_url"] = source["source_url"].replace(
            "example.org", "example.net"
        )
    return result


def shape(value):
    if isinstance(value, dict):
        return {key: shape(item) for key, item in value.items()}
    if isinstance(value, list):
        return [shape(item) for item in value]
    return None


def transport(url, user_agent, timeout):
    assert "(+https://github.com/vamseeachanta/digitalmodel;" in user_agent
    assert timeout == 30
    body = b"User-agent: *\nAllow: /\n" if url.endswith("robots.txt") else b"facts"
    return 200, url, body


def test_emitted_manifest_contract(fetcher, template, tmp_path):
    output = tmp_path / "raw"
    result = fetcher(template, output, ROOT, transport=transport, pause=lambda _: None)
    committed = json.loads(MANIFEST.read_text())
    committed.pop("assembly", None)
    assert shape(result) == shape(committed)
    saved = json.loads((output / MANIFEST.name).read_text())
    assert saved == result
    assert len(list(output.glob("*.document"))) == 11
    assert all(source["status"] == "retrieved" for source in result["sources"])


@pytest.mark.parametrize("status", [401, 403, 429])
def test_refusal_stops_only_affected_site(fetcher, template, tmp_path, status):
    calls = []

    def refused(url, user_agent, timeout):
        calls.append(url)
        if url == "https://example.org/1":
            return status, url, b"refused"
        return transport(url, user_agent, timeout)

    result = fetcher(template, tmp_path, ROOT, transport=refused, pause=lambda _: None)
    assert "https://example.org/2" not in calls
    assert calls.count("https://example.org/1") == 1
    assert "https://example.net/4" in calls
    assert len(result["refused_sites"]) == 1
    assert "hand-assembled" in result["assembly"]
    assert result["sources"][1] == template["sources"][1]
    assert result["sources"][0] == template["sources"][0]


def test_robots_disallow_stops_site(fetcher, template, tmp_path):
    calls = []

    def disallowed(url, user_agent, timeout):
        calls.append(url)
        return 200, url, b"User-agent: *\nDisallow: /\n"

    result = fetcher(
        template, tmp_path, ROOT, transport=disallowed, pause=lambda _: None
    )
    assert calls == ["https://example.org/robots.txt", "https://example.net/robots.txt"]
    assert len(result["refused_sites"]) == 2


def test_checkout_output_is_refused(fetcher, template, tmp_path):
    with pytest.raises(ValueError, match="outside"):
        fetcher(template, ROOT, ROOT, transport=transport)
    checkout = tmp_path / "checkout"
    checkout.mkdir()
    alias = tmp_path / "alias"
    alias.symlink_to(checkout, target_is_directory=True)
    with pytest.raises(ValueError, match="outside"):
        fetcher(template, alias / "raw", checkout, transport=transport)


def test_existing_output_is_not_overwritten(fetcher, template, tmp_path):
    existing = tmp_path / "preserved"
    existing.write_bytes(b"original")
    with pytest.raises(ValueError, match="empty"):
        fetcher(template, tmp_path, ROOT, transport=transport)
    assert existing.read_bytes() == b"original"


def test_challenge_is_not_retried(fetcher, template, tmp_path):
    calls = []

    def challenged(url, user_agent, timeout):
        calls.append(url)
        return 200, url, b"<html>Verify you are human</html>"

    result = fetcher(
        template, tmp_path, ROOT, transport=challenged, pause=lambda _: None
    )
    assert len(calls) == 2
    assert len(result["refused_sites"]) == 2


def test_single_transient_retry_obeys_site_pause(fetcher, template, tmp_path):
    calls = []
    pauses = []

    def transient(url, user_agent, timeout):
        calls.append(url)
        if len(calls) == 1:
            raise TimeoutError
        return transport(url, user_agent, timeout)

    result = fetcher(template, tmp_path, ROOT, transport=transient, pause=pauses.append)
    assert len(calls) == 12
    assert pauses == [3] * 10
    assert not result["refused_sites"]
