# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic contracts only: no provider response, credentials or network."""

import json
import socket
from copy import deepcopy
from datetime import datetime, timezone
from pathlib import Path

import pytest
import requests

from digitalmodel.drilling_riser.metocean_inputs import (
    reduce_to_scatter,
    scatter_to_seastates,
)

FIXTURE = Path(__file__).parent / "fixtures" / "cmems_synthetic.json"


def _adapt(payload):
    # Import inside each test so the red run records every missing behaviour.
    from digitalmodel.data_systems.data_procurement.metocean.api_clients.cmems_offline import (
        OfflineCMEMSFixture,
    )

    return OfflineCMEMSFixture(payload)


@pytest.fixture(autouse=True)
def forbid_network(monkeypatch):
    """Fail on HTTP or socket use, including during validation."""

    def forbidden(*args, **kwargs):
        pytest.fail("offline contract attempted network access")

    monkeypatch.setattr(requests.sessions.Session, "request", forbidden)
    monkeypatch.setattr(socket.socket, "connect", forbidden)
    monkeypatch.setattr(socket, "create_connection", forbidden)


@pytest.fixture
def payload():
    """Read the hand-authored example outside the pure adapter."""
    return json.loads(FIXTURE.read_text(encoding="utf-8"))


def test_normalized_records_are_synthetic_and_independent(payload):
    before = deepcopy(payload)
    adapter = _adapt(payload)
    records = adapter.records()
    assert records == [
        {
            "timestamp": datetime(2024, 1, 1, 0, tzinfo=timezone.utc),
            "wave_height": 0.5,
            "wave_period": 5.0,
            "synthetic": True,
            "provenance": before["provenance"],
        },
        {
            "timestamp": datetime(2024, 1, 1, 1, tzinfo=timezone.utc),
            "wave_height": 1.5,
            "wave_period": 7.0,
            "synthetic": True,
            "provenance": before["provenance"],
        },
    ]
    records[0]["provenance"]["units"]["peak_period"] = "changed"
    assert records[1]["provenance"] == before["provenance"]
    payload["records"][0]["peak_period"] = 99
    assert adapter.records()[0]["wave_period"] == 5.0
    assert adapter.records()[0]["provenance"] == before["provenance"]
    payload["records"][0]["peak_period"] = 5.0
    assert payload == before


def test_fixture_reuses_reducer_and_midpoints(payload):
    scatter = reduce_to_scatter(
        _adapt(payload).records(), hs_bins=[1.0, 2.0], tp_bins=[6.0, 8.0]
    )
    assert scatter.occurrences == [[1.0, 0.0], [0.0, 1.0]]
    assert [(s.hs_m, s.tp_s) for s in scatter_to_seastates(scatter)] == [
        (0.5, 3.0),
        (1.5, 7.0),
    ]


@pytest.mark.parametrize("field", ["significant_wave_height", "peak_period"])
def test_nulls_remain_missing_and_are_not_binned(payload, field):
    payload["records"][0][field] = None
    records = _adapt(payload).records()
    key = "wave_height" if field == "significant_wave_height" else "wave_period"
    assert records[0][key] is None
    scatter = reduce_to_scatter(records, hs_bins=[1.0, 2.0], tp_bins=[6.0, 8.0])
    assert scatter.occurrences == [[0.0, 0.0], [0.0, 1.0]]


def test_zero_height_and_explicit_timezone_are_preserved(payload):
    payload["records"][0].update(
        significant_wave_height=0, timestamp="2024-01-01T01:00:00+01:00"
    )
    record = _adapt(payload).records()[0]
    assert record["wave_height"] == 0.0
    assert record["timestamp"] == datetime(2024, 1, 1, tzinfo=timezone.utc)


@pytest.mark.parametrize("field", ["significant_wave_height", "peak_period"])
@pytest.mark.parametrize(
    "value",
    [
        True,
        False,
        float("nan"),
        float("inf"),
        -float("inf"),
        -1,
        "1,000",
        "1.5",
        [],
        {},
        10**400,
    ],
)
def test_invalid_wave_values_raise_without_echo(payload, field, value):
    payload["records"][1][field] = value
    with pytest.raises(ValueError, match="wave value"):
        _adapt(payload)


def test_zero_period_is_rejected(payload):
    payload["records"][1]["peak_period"] = 0
    with pytest.raises(ValueError, match="wave value"):
        _adapt(payload)


@pytest.mark.parametrize(
    "stamp",
    [
        None,
        0,
        True,
        "invalid",
        "2024-02-30T00:00:00Z",
        "2024-01-01",
        "2024-01-01T00:00:00",
        "2024-01-01T00:00:00+00:60",
        "2024-01-01T00:00:00+24:00",
        "0001-01-01T00:00:00+01:00",
        "https://example.invalid/data",
    ],
)
def test_timestamp_requires_a_valid_explicit_timezone(payload, stamp):
    payload["records"][1]["timestamp"] = stamp
    with pytest.raises(ValueError, match="timestamp") as error:
        _adapt(payload)
    assert str(error.value) == "invalid timestamp"


@pytest.mark.parametrize(
    "field", ["origin", "schema_version", "purpose", "third_party_data", "units"]
)
@pytest.mark.parametrize("mutation", ["missing", "unsupported"])
def test_provenance_is_required_and_exact(payload, field, mutation):
    if mutation == "missing":
        del payload["provenance"][field]
    else:
        payload["provenance"][field] = "provider-derived"
    with pytest.raises(ValueError, match="synthetic provenance"):
        _adapt(payload)


@pytest.mark.parametrize("field", ["significant_wave_height", "peak_period"])
def test_units_are_explicit_without_conversion(payload, field):
    payload["provenance"]["units"][field] = "unsupported"
    with pytest.raises(ValueError, match="synthetic provenance"):
        _adapt(payload)


@pytest.mark.parametrize("level", ["payload", "provenance", "units", "record"])
@pytest.mark.parametrize("key", ["password", "authorization", "url", "metadata"])
def test_unknown_or_credential_fields_are_refused_without_echo(payload, level, key):
    target = {
        "payload": payload,
        "provenance": payload["provenance"],
        "units": payload["provenance"]["units"],
        "record": payload["records"][1],
    }[level]
    target[key] = "synthetic-rejection-marker"
    with pytest.raises(ValueError) as error:
        _adapt(payload)
    assert "synthetic-rejection-marker" not in str(error.value)
    assert key not in str(error.value)


@pytest.mark.parametrize("value", [None, [], "https://example.invalid/data"])
def test_only_decoded_mappings_are_accepted(value):
    with pytest.raises(ValueError, match="fixture fields"):
        _adapt(value)


@pytest.mark.parametrize("value", [None, {}, "records", [None]])
def test_records_require_a_list_of_complete_mappings(payload, value):
    payload["records"] = value
    with pytest.raises(ValueError):
        _adapt(payload)


@pytest.mark.parametrize(
    "field", ["timestamp", "significant_wave_height", "peak_period"]
)
def test_missing_record_fields_are_rejected(payload, field):
    del payload["records"][1][field]
    with pytest.raises(ValueError, match="record fields"):
        _adapt(payload)


def test_empty_synthetic_fixture_is_explicit(payload):
    payload["records"] = []
    assert _adapt(payload).records() == []


def test_third_party_marker_is_boolean_false(payload):
    payload["provenance"]["third_party_data"] = 0
    with pytest.raises(ValueError, match="synthetic provenance"):
        _adapt(payload)


def test_adapter_performs_no_file_access(payload, monkeypatch):
    def forbidden(*args, **kwargs):
        pytest.fail("offline adapter attempted file access")

    monkeypatch.setattr("builtins.open", forbidden)
    monkeypatch.setattr(Path, "open", forbidden)
    assert len(_adapt(payload).records()) == 2
