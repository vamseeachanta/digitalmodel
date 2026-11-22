# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""UTF-8 authentication contracts using generated, synthetic credentials.

The independent oracle is exact text equality: UTF-8 encodes equal strings to
equal byte sequences; appending an ASCII character makes them unequal. No
normalization or trimming is requested. Python 3.11 Library Reference,
hmac.compare_digest, requires bytes for non-ASCII comparisons:
https://docs.python.org/3.11/library/hmac.html#hmac.compare_digest

Successful requests return HTTP 200 from todobasic.get_tasks; rejected requests
return HTTP 403 from todobasic.unauthorized (repository route contract).
"""

import importlib
import secrets
from base64 import b64encode
from http import HTTPStatus

import pytest
from flask import Flask

from digitalmodel.web.digitaltwinfeed import basic_auth


@pytest.fixture(params=["ascii", "unicode"])
def configured_pair(monkeypatch, request):
    user = secrets.token_hex()
    if request.param == "unicode":
        user = "\N{LATIN SMALL LETTER U WITH DIAERESIS}" + user
    value = (
        "\N{LATIN SMALL LETTER E WITH ACUTE}"
        "\N{CJK UNIFIED IDEOGRAPH-5BC6}"
        "\N{LOCK}" + secrets.token_hex() + ": "
    )
    monkeypatch.setenv("DIGITALTWINFEED_BASIC_AUTH_USER", user)
    monkeypatch.setenv("DIGITALTWINFEED_BASIC_AUTH_PASSWORD", value)
    return user, value


@pytest.fixture
def client():
    module = importlib.import_module(
        "digitalmodel.web.digitaltwinfeed.todobasic.todobasic"
    )
    app = Flask(__name__)
    app.config["TESTING"] = True
    app.register_blueprint(module.todobasic)
    return app.test_client()


def authorization(user, value):
    encoded = b64encode(f"{user}:{value}".encode("utf-8")).decode("ascii")
    return {"Authorization": "Basic " + encoded}


@pytest.mark.parametrize("wrong_part", [None, "user", "value"])
def test_unicode_password_request(client, configured_pair, wrong_part):
    """Equal input succeeds; appending x to either field must be refused."""
    user, value = configured_pair
    if wrong_part == "user":
        user += "x"
    elif wrong_part == "value":
        value += "x"
    expected = HTTPStatus.OK if wrong_part is None else HTTPStatus.FORBIDDEN
    response = client.get("/tasks", headers=authorization(user, value))
    if response.status_code != expected:
        pytest.fail("Unicode request authentication result changed")


def test_unicode_password_rotation(client, configured_pair, monkeypatch):
    """Replacing the configured value invalidates old bytes immediately."""
    user, value = configured_pair
    monkeypatch.setenv("DIGITALTWINFEED_BASIC_AUTH_PASSWORD", value + "x")
    for presented, expected in (
        (value, HTTPStatus.FORBIDDEN), (value + "x", HTTPStatus.OK)
    ):
        response = client.get("/tasks", headers=authorization(user, presented))
        if response.status_code != expected:
            pytest.fail("Password changes must apply to the next request")


def test_unicode_user_rotation(client, configured_pair, monkeypatch):
    """Replacing the configured user invalidates the old user immediately."""
    user, value = configured_pair
    monkeypatch.setenv("DIGITALTWINFEED_BASIC_AUTH_USER", user + "x")
    for presented, expected in (
        (user, HTTPStatus.FORBIDDEN), (user + "x", HTTPStatus.OK)
    ):
        response = client.get("/tasks", headers=authorization(presented, value))
        if response.status_code != expected:
            pytest.fail("User changes must apply to the next request")


@pytest.mark.parametrize("empty", [None, ""])
@pytest.mark.parametrize("part", ["user", "value"])
def test_verifier_refuses_empty_input(configured_pair, empty, part):
    """A missing presented field cannot match a nonblank configured field."""
    user, value = configured_pair
    result = basic_auth.basic_auth_verify_password(
        empty if part == "user" else user,
        empty if part == "value" else value,
    )
    if result is not False:
        pytest.fail("Empty presented credentials must be refused")


@pytest.mark.parametrize("suffix", ["USER", "PASSWORD"])
@pytest.mark.parametrize("missing", [None, "", " ", "\t\n"])
def test_unicode_credentials_fail_closed(
    client, configured_pair, monkeypatch, suffix, missing
):
    """Absent or strip-empty configuration cannot match supplied credentials."""
    user, value = configured_pair
    variable = f"DIGITALTWINFEED_BASIC_AUTH_{suffix}"
    if missing is None:
        monkeypatch.delenv(variable)
    else:
        monkeypatch.setenv(variable, missing)
    response = client.get("/tasks", headers=authorization(user, value))
    if response.status_code != HTTPStatus.FORBIDDEN:
        pytest.fail("Missing or blank configuration must refuse requests")


@pytest.mark.parametrize("wrong_part", [None, "user", "value"])
def test_verifier_compares_utf8_bytes(configured_pair, monkeypatch, wrong_part):
    """Both byte comparisons run; changing either operand makes the AND false."""
    user, value = configured_pair
    presented_user = user + "x" if wrong_part == "user" else user
    presented_value = value + "x" if wrong_part == "value" else value
    calls = []

    def compare(left, right):
        calls.append((left, right))
        return left == right

    monkeypatch.setattr(basic_auth.hmac, "compare_digest", compare)
    result = basic_auth.basic_auth_verify_password(presented_user, presented_value)
    expected_calls = [
        (presented_user.encode("utf-8"), user.encode("utf-8")),
        (presented_value.encode("utf-8"), value.encode("utf-8")),
    ]
    if calls != expected_calls:
        pytest.fail("Both credentials must receive UTF-8 digest comparisons")
    if result is not (wrong_part is None):
        pytest.fail("Verifier must accept only matching credentials")
