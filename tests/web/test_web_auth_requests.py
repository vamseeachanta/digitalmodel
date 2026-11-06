# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Authentication contracts using synthetic input and the installed library."""

import hmac
import importlib
import secrets
from base64 import b64encode
from pathlib import Path

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


APPLICATIONS = (
    "BuoyCALM", "BuoyCoupled", "BuoySALM", "FFS", "Fatigue",
    "FixedWindTurbine", "Floaters", "FloatingWindTurbine", "Foundations",
    "Metocean", "OffshorePipeline", "OffshoreRiser", "OnshorePipeline",
    "Pipings", "PortfolioAnalysis", "PressureVessels", "TemplateCode",
)


@pytest.fixture(params=["application_plain", "application_restful", "template",
                        *APPLICATIONS])
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
    elif request.param == "template":
        module = importlib.import_module("digitalmodel.web.template_code.TemplateCode")
        blueprint, prefix, route = module.AppBlueprint, "TEMPLATE_CODE", "/"
    else:
        name = request.param
        module = importlib.import_module(
            f"digitalmodel.web.digitaltwinfeed.{name}.{name}"
        )
        app = Flask(__name__)
        app.config["TESTING"] = True
        # Exercise the module's registered authentication object independently
        # of business routes, some of which render templates or are public.
        app.add_url_rule("/probe", view_func=module.auth.login_required(lambda: "ok"))
        return app.test_client(), "DIGITALTWINFEED", "/probe"
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


@pytest.mark.parametrize("unicode_value", [False, True])
@pytest.mark.parametrize("unicode_input", [False, True])
def test_protected_routes_authenticate_and_read_environment_live(
    monkeypatch, protected_route, synthetic_pair, unicode_input, unicode_value
):
    client, prefix, route = protected_route
    user, value = synthetic_pair
    if unicode_input:
        user = "\N{LATIN SMALL LETTER U WITH DIAERESIS}" + user
    if unicode_value:
        value = "\N{LATIN SMALL LETTER E WITH ACUTE}" + value
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


@pytest.mark.parametrize("correct", [False, True])
def test_non_ascii_password_request(
    monkeypatch, protected_route, synthetic_pair, correct
):
    """UTF-8 equality accepts; appending x refuses (independent oracle).

    The synthetic prefix uses U+00E9, U+5BC6 and U+1F512, as in the existing
    test_web_unicode_auth fixture. Python 3.11 hmac.compare_digest documents
    the bytes requirement; route handlers define success 200 and refusal 403.
    """
    client, prefix, route = protected_route
    user, value = synthetic_pair
    value = "\N{LATIN SMALL LETTER E WITH ACUTE}\N{CJK UNIFIED IDEOGRAPH-5BC6}\N{LOCK}" + value
    monkeypatch.setenv(f"{prefix}_BASIC_AUTH_USER", user)
    monkeypatch.setenv(f"{prefix}_BASIC_AUTH_PASSWORD", value)
    presented = value if correct else value + "x"
    check_status(client.get(route, headers=authorization(user, presented)),
                 200 if correct else 403)


@pytest.mark.parametrize("malformed", ["base64", "utf8", "empty", "long"])
def test_protected_routes_refuse_malformed_credentials(
    monkeypatch, protected_route, synthetic_pair, malformed
):
    """Bad wire encodings must not become matching text.

    RFC 4648 (2006), section 3.3 requires rejection of non-alphabet data.
    U+00FF encodes as C3 BF in UTF-8; FF alone is invalid UTF-8, even though
    the old Latin-1 fallback maps it to the configured character. Empty
    input is unequal to a nonblank setting. Repetition constructs unequal
    bytes; its length is a stress input, not an acceptance limit.
    """
    client, prefix, route = protected_route
    user, value = synthetic_pair
    value = "\N{LATIN SMALL LETTER Y WITH DIAERESIS}" + value
    monkeypatch.setenv(f"{prefix}_BASIC_AUTH_USER", user)
    monkeypatch.setenv(f"{prefix}_BASIC_AUTH_PASSWORD", value)
    if malformed == "base64":
        headers = authorization(user, value)
        headers["Authorization"] += "!"
    elif malformed == "utf8":
        encoded = b64encode(user.encode() + b":" + b"\xff" + value[1:].encode())
        headers = {"Authorization": "Basic " + encoded.decode("ascii")}
    else:
        # The probe size is derived from the generated input, not a length
        # policy: equal input repeated len(input) times is a different value.
        presented = "" if malformed == "empty" else value * len(value) ** 2
        headers = authorization(user, presented)
    check_status(client.get(route, headers=headers), 403)


def test_all_registered_callbacks_have_request_coverage():
    """Every active web auth registration belongs to the request matrix."""
    import ast

    root = Path(__file__).resolve().parents[2] / "src" / "digitalmodel" / "web"
    actual = set()
    for path in root.rglob("*.py"):
        tree = ast.parse(path.read_bytes())
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                for decorator in node.decorator_list:
                    if (isinstance(decorator, ast.Attribute)
                            and decorator.attr in {"get_password", "verify_password"}):
                        assert decorator.attr == "verify_password"
                        actual.add(path.relative_to(root).as_posix())
    expected = {f"digitaltwinfeed/{name}/{name}.py" for name in APPLICATIONS}
    expected.update({"digitaltwinfeed/todobasic/todobasic.py",
                     "digitaltwinfeed/todorestful/todorestful.py",
                     "template_code/TemplateCode.py"})
    assert actual == expected
