# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Authentication contracts using synthetic input and the installed library."""

import hmac
import importlib
import secrets
from base64 import b64encode

import pytest
from flask import Flask


@pytest.fixture(params=["application", "template"])
def provider(request):
    if request.param == "application":
        module = importlib.import_module(
            "digitalmodel.web.digitaltwinfeed.basic_auth"
        )
        return module.basic_auth_password_for, "DIGITALTWINFEED"
    module = importlib.import_module("digitalmodel.web.template_code.TemplateCode")
    return module._basic_auth_password_for, "TEMPLATE_CODE"


@pytest.fixture
def synthetic_pair():
    return secrets.token_hex(16), secrets.token_hex(24)


@pytest.mark.parametrize("unicode_input", [False, True])
def test_user_name_comparison_uses_digest(
    monkeypatch, provider, synthetic_pair, unicode_input
):
    callback, prefix = provider
    user, value = synthetic_pair
    if unicode_input:
        user = "\N{LATIN SMALL LETTER U WITH DIAERESIS}" + user
    monkeypatch.setenv(f"{prefix}_BASIC_AUTH_USER", user)
    monkeypatch.setenv(f"{prefix}_BASIC_AUTH_PASSWORD", value)
    calls = []
    compare = hmac.compare_digest

    def capture(left, right):
        calls.append((left, right))
        return compare(left, right)

    monkeypatch.setattr(hmac, "compare_digest", capture)
    for presented, expected in ((user, value), (user + "x", None)):
        if callback(presented) != expected:
            pytest.fail("User-name matching changed")
        if not calls or calls[-1] != (presented.encode("utf-8"), user.encode("utf-8")):
            pytest.fail("User-name comparison must use compare_digest on UTF-8 bytes")
    if len(calls) != 2:
        pytest.fail("Each nonblank user name must receive a digest comparison")


@pytest.fixture(params=["application_plain", "application_restful", "template"])
def protected_route(request):
    if request.param == "application_plain":
        module = importlib.import_module(
            "digitalmodel.web.digitaltwinfeed.todobasic.todobasic"
        )
        blueprint, prefix, route = module.todobasic, "DIGITALTWINFEED", "/tasks"
    elif request.param == "application_restful":
        module = importlib.import_module(
            "digitalmodel.web.digitaltwinfeed.todorestful.todorestful"
        )
        blueprint, prefix, route = module.AppBlueprint, "DIGITALTWINFEED", "/"
    else:
        module = importlib.import_module("digitalmodel.web.template_code.TemplateCode")
        blueprint, prefix, route = module.AppBlueprint, "TEMPLATE_CODE", "/"
    app = Flask(__name__)
    app.config["TESTING"] = True
    app.register_blueprint(blueprint, url_prefix="/protected")
    return app.test_client(), prefix, "/protected" + route


def authorization(user, value):
    encoded = b64encode(f"{user}:{value}".encode("utf-8")).decode("ascii")
    return {"Authorization": "Basic " + encoded}


def check_status(response, expected):
    if response.status_code != expected:
        pytest.fail(f"Protected route returned {response.status_code}, expected {expected}")


@pytest.mark.parametrize("suffix", ["USER", "PASSWORD"])
@pytest.mark.parametrize("missing", [None, "", " ", "\t\n"])
def test_protected_routes_refuse_missing_configuration(
    monkeypatch, protected_route, synthetic_pair, suffix, missing
):
    client, prefix, route = protected_route
    user, value = synthetic_pair
    monkeypatch.setenv(f"{prefix}_BASIC_AUTH_USER", user)
    monkeypatch.setenv(f"{prefix}_BASIC_AUTH_PASSWORD", value)
    variable = f"{prefix}_BASIC_AUTH_{suffix}"
    if missing is None:
        monkeypatch.delenv(variable, raising=False)
    else:
        monkeypatch.setenv(variable, missing)
    for headers in ({}, authorization(user, value), authorization(user, ""),
                    authorization("", ""), {"Authorization": "Basic !!!"}):
        check_status(client.get(route, headers=headers), 403)


@pytest.mark.parametrize("unicode_input", [False, True])
def test_protected_routes_authenticate_and_read_environment_live(
    monkeypatch, protected_route, synthetic_pair, unicode_input
):
    client, prefix, route = protected_route
    user, value = synthetic_pair
    if unicode_input:
        user = "\N{LATIN SMALL LETTER U WITH DIAERESIS}" + user
    monkeypatch.setenv(f"{prefix}_BASIC_AUTH_USER", user)
    monkeypatch.setenv(f"{prefix}_BASIC_AUTH_PASSWORD", value)
    check_status(client.get(route, headers=authorization(user, value)), 200)
    for headers in ({}, authorization(user + "x", value), authorization(user, value + "x"),
                    authorization(user, ""), authorization("", value)):
        check_status(client.get(route, headers=headers), 403)
    monkeypatch.setenv(f"{prefix}_BASIC_AUTH_PASSWORD", value + "x")
    check_status(client.get(route, headers=authorization(user, value)), 403)
    check_status(client.get(route, headers=authorization(user, value + "x")), 200)
    monkeypatch.delenv(f"{prefix}_BASIC_AUTH_USER")
    check_status(client.get(route, headers=authorization(user, value + "x")), 403)
