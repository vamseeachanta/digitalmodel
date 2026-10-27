# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline contract for marine hourly responses.

Numerical basis: independent synthetic example, not measured marine conditions.
Two samples at the equator and prime meridian have heights zero and one metre,
with wind speeds one and zero metres per second. The current speeds are half
and zero metres per second; directions are zero and a quarter turn (90 degrees).
The timestamps are deliberately in reverse hourly order on an arbitrary date.
Expected records are transcribed from this example without using the parser.
Malformed cases remove, duplicate, or replace samples from the same example.

Minimal dependencies beyond pytest: requests and PyYAML for client imports,
numpy and scipy for eager sibling-package imports, and pytest-asyncio for the
repository's pytest configuration. No provider connection is made.
"""

from copy import deepcopy
from datetime import datetime

import pytest

from digitalmodel.data_systems.data_procurement.metocean.api_clients.openmeteo_client import (
    OpenMeteoClient,
)


# Variable names and aliases are the client's existing query_by_date contract.
PARAMETER_VARIABLES = (
    ("wave_height", "wave_height"),
    ("wave_direction", "wave_direction"),
    ("wave_period", "wave_period"),
    ("wind_speed_10m", "wind_speed_10m"),
    ("wind_speed_80m", "wind_speed_80m"),
    ("wind_speed_120m", "wind_speed_120m"),
    ("current_speed", "ocean_current_velocity"),
    ("current_direction", "ocean_current_direction"),
)


@pytest.fixture
def response_payload():
    """Return the independent two-sample example described above."""
    return {
        "latitude": 0.0,
        "longitude": 0.0,
        "hourly": {
            "time": ["2024-01-01T01:00", "2024-01-01T00:00"],
            "wave_height": [0.0, 1.0],
            "wind_speed_10m": [1.0, 0.0],
        },
    }


@pytest.fixture
def marine_query(monkeypatch):
    """Replace only the client's HTTP boundary with a synthetic response."""
    class Response:
        def __init__(self, payload):
            self.payload = payload

        def json(self):
            return self.payload

    with OpenMeteoClient() as client:
        def query(payload, parameters=None, variables="wave_height,wind_speed_10m"):
            def get(endpoint, *, params):
                assert endpoint == "marine"
                assert params["hourly"] == variables
                assert params["timezone"] == "UTC"
                return Response(deepcopy(payload))

            monkeypatch.setattr(client, "get", get)
            return client.query_by_date(
                datetime(2024, 1, 1),
                datetime(2024, 1, 1),
                {"lat": 0.0, "lon": 0.0},
                parameters=parameters,
            )

        yield query


@pytest.mark.parametrize("field", ["wave_height", "wind_speed_10m"])
@pytest.mark.parametrize(
    "defect",
    ["missing", "null", "short", "long", "empty", "scalar", "string", "mapping"],
)
def test_marine_response_rejects_missing_or_misaligned_requested_arrays_before_yield(
    marine_query, response_payload, field, defect
):
    """Even a valid leading sample must not escape a malformed response."""
    hourly = response_payload["hourly"]
    if defect == "missing":
        del hourly[field]
    else:
        hourly[field] = {
            "null": None,
            "short": [0.0],
            "long": [0.0, 1.0, 0.0],
            "empty": [],
            "scalar": 0.0,
            "string": "01",
            "mapping": {"first": 0.0, "second": 1.0},
        }[defect]

    with pytest.raises(ValueError, match=rf"hourly\.{field}"):
        next(marine_query(response_payload))


@pytest.mark.parametrize("field", ["wave_height", "wind_speed_10m"])
@pytest.mark.parametrize("sample", [float("nan"), float("inf"), -float("inf")])
def test_marine_response_rejects_nonfinite_requested_samples(
    marine_query, response_payload, field, sample
):
    """A nonfinite later sample is rejected before the valid first sample."""
    response_payload["hourly"][field] = [0.0, sample]
    with pytest.raises(ValueError, match=rf"hourly\.{field}.*finite"):
        next(marine_query(response_payload))


@pytest.mark.parametrize("sample", [None, "1.0", True, False, [], {}])
def test_marine_response_rejects_nonnumeric_requested_samples(
    marine_query, response_payload, sample
):
    """Nulls, strings, booleans and containers are not numeric weather samples."""
    response_payload["hourly"]["wind_speed_10m"] = [0.0, sample]
    with pytest.raises(ValueError, match=r"hourly\.wind_speed_10m.*numeric"):
        next(marine_query(response_payload))


def test_complete_response_preserves_zero_and_order(marine_query, response_payload):
    """Preserve the independent example, including zero and reversed times."""
    assert list(marine_query(response_payload)) == [
        {
            "timestamp": datetime(2024, 1, 1, 1),
            "latitude": 0.0,
            "longitude": 0.0,
            "wave_height": 0.0,
            "wind_speed_10m": 1.0,
        },
        {
            "timestamp": datetime(2024, 1, 1, 0),
            "latitude": 0.0,
            "longitude": 0.0,
            "wave_height": 1.0,
            "wind_speed_10m": 0.0,
        },
    ]


@pytest.mark.parametrize("parameter,variable", PARAMETER_VARIABLES)
def test_requested_parameter_names_and_aliases(
    marine_query, response_payload, parameter, variable
):
    """Read provider keys while exposing the caller's original parameter names."""
    response_payload["hourly"][variable] = [0.5, 0.0]
    records = list(marine_query(response_payload, [parameter], variable))
    assert [record[parameter] for record in records] == [0.5, 0.0]
    assert all(set(record) == {"timestamp", "latitude", "longitude", parameter}
               for record in records)


@pytest.mark.parametrize(
    "parameter,variable",
    [("current_speed", "ocean_current_velocity"),
     ("current_direction", "ocean_current_direction")],
)
def test_current_alias_requires_the_provider_array(
    marine_query, response_payload, parameter, variable
):
    """An array under the caller alias cannot replace the requested provider key."""
    response_payload["hourly"][parameter] = [0.0, 90.0]
    with pytest.raises(ValueError, match=rf"hourly\.{variable}"):
        next(marine_query(response_payload, [parameter], variable))


def test_provider_variable_passthrough(marine_query, response_payload):
    """Preserve query_by_date's existing unmapped-variable behavior."""
    variable = "ocean_current_velocity"
    response_payload["hourly"][variable] = [0.5, 0.0]
    records = list(marine_query(response_payload, [variable], variable))
    assert [record[variable] for record in records] == [0.5, 0.0]


def test_unrequested_malformed_array_is_ignored(marine_query, response_payload):
    """Only requested variables participate in the hourly array contract."""
    response_payload["hourly"]["wave_direction"] = None
    assert list(marine_query(response_payload))


@pytest.mark.parametrize("hourly", [None, [], "invalid", {}])
def test_marine_response_requires_hourly_time_array(
    marine_query, response_payload, hourly
):
    """Malformed hourly envelopes raise a descriptive parser error."""
    response_payload["hourly"] = hourly
    with pytest.raises(ValueError, match="hourly"):
        next(marine_query(response_payload))


@pytest.mark.parametrize("times", [None, "2024-01-01T00:00", ["2024-01-01T01:00", "invalid"]])
def test_marine_response_rejects_malformed_times_before_yield(
    marine_query, response_payload, times
):
    """A malformed later timestamp must not allow partial delivery either."""
    response_payload["hourly"]["time"] = times
    with pytest.raises(ValueError, match=r"hourly\.time"):
        next(marine_query(response_payload))


def test_complete_empty_response(marine_query, response_payload):
    """An explicitly empty time axis requires explicitly empty requested arrays."""
    response_payload["hourly"] = {
        "time": [], "wave_height": [], "wind_speed_10m": [],
    }
    assert list(marine_query(response_payload)) == []
