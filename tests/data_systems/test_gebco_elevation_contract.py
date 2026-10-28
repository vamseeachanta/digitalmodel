# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic response-admission examples; no bathymetry data is downloaded.

All coordinates use the arbitrary origin, not a surveyed location. Elevations
are invented metre values. Independent hand calculations for depth are:
max(-(-120.5), 0) = 120.5; max(-0, 0) = 0; max(-12.5, 0) = 0;
max(-(-0.25), 0) = 0.25. Scientific notation -1.205e2 equals -120.5.
The exponent magnitude in the overflow cases exceeds finite float range.
"""

from datetime import datetime
from http import HTTPStatus

import pytest
import requests

from digitalmodel.data_systems.data_procurement.metocean.api_clients.gebco_client import (
    GEBCOClient,
)


@pytest.fixture
def client_for_text(monkeypatch):
    """Replace only HTTP transport, retaining requests and the real parser."""
    response = requests.Response()
    response.status_code = HTTPStatus.OK
    response.encoding = "utf-8"

    def send(_session, request, **_kwargs):
        assert request.method == "GET"
        response.request = request
        return response

    monkeypatch.setattr(requests.Session, "send", send)
    with GEBCOClient() as client:
        def configure(text):
            response._content = text.encode("utf-8")
            return client

        yield configure


@pytest.mark.parametrize("method", ["get_elevation", "get_depth"])
@pytest.mark.parametrize("text", [
    "",
    "No features found",
    "<ServiceException>Unavailable</ServiceException>",
    "Elevation:",
    "Elevation: unavailable m",
    "Elevation: --1 m",
    "Elevation: 1.2.3 m",
    "elevation:   \nother: value",
])
def test_missing_elevation_is_not_land(client_for_text, method, text):
    client = client_for_text(text)
    with pytest.raises(ValueError, match="GEBCO.*elevation"):
        getattr(client, method)(lat=0, lon=0)


@pytest.mark.parametrize("method", ["get_elevation", "get_depth"])
@pytest.mark.parametrize("token", [
    "nan", "NaN", "+nan", "-nan", "inf", "+inf", "-inf",
    "Infinity", "+Infinity", "-Infinity", "1e309", "-1e309",
])
def test_nonfinite_elevation_rejected(client_for_text, method, token):
    client = client_for_text(f"Elevation: {token} m")
    with pytest.raises(ValueError, match="GEBCO.*elevation"):
        getattr(client, method)(lat=0, lon=0)


@pytest.mark.parametrize("text, elevation, depth", [
    ("Elevation: -120.5 m", -120.5, 120.5),
    ("elevation: 0 m", 0, 0),
    ("Elevation: 12.5 m", 12.5, 0),
    ("Elevation: -0.0 m", -0.0, 0),
    ("Feature information\n  elevation: -0.25 m\r\n", -0.25, 0.25),
    ("Elevation: -1.205e2 m", -120.5, 120.5),
])
def test_finite_elevation_and_depth(client_for_text, text, elevation, depth):
    client = client_for_text(text)
    assert client.get_elevation(lat=0, lon=0) == elevation
    assert client.get_depth(lat=0, lon=0) == depth


@pytest.mark.parametrize("text", [
    "", "Elevation: unavailable m", "Elevation: nan m", "Elevation: -inf m",
])
def test_query_by_date_propagates_parse_error(client_for_text, text):
    client = client_for_text(text)
    records = client.query_by_date(datetime.min, datetime.max, {"lat": 0, "lon": 0})
    with pytest.raises(ValueError, match="GEBCO.*elevation"):
        next(records)


@pytest.mark.parametrize("text, depth", [
    ("Elevation: -120.5 m", 120.5),
    ("Elevation: 0 m", 0),
    ("Elevation: 12.5 m", 0),
])
def test_query_by_date_finite_depth(client_for_text, text, depth):
    client = client_for_text(text)
    records = client.query_by_date(datetime.min, datetime.max, {"lat": 0, "lon": 0})
    record = next(records)
    assert record["water_depth"] == depth
    assert record["latitude"] == record["longitude"] == 0
    with pytest.raises(StopIteration):
        next(records)


def test_malformed_line_can_precede_finite_elevation(client_for_text):
    client = client_for_text("Elevation: unavailable\nelevation: -120.5 m")
    assert client.get_depth(lat=0, lon=0) == 120.5


def test_nonfinite_line_is_not_hidden_by_later_finite_value(client_for_text):
    client = client_for_text("Elevation: nan m\nElevation: -120.5 m")
    with pytest.raises(ValueError, match="GEBCO.*elevation"):
        client.get_elevation(lat=0, lon=0)
