# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reproduce the old error and the replacement results without printing inputs.

Run with PYTHONPATH=src python scripts/web/authentication_example.py.
The input alphabet follows tests/web/test_web_unicode_auth.py: U+00E9,
U+5BC6 and U+1F512 followed by generated random ASCII text. Expected outcomes
follow exact equality of the UTF-8 sequences and the protected route contract.
"""

import importlib
import json
import secrets
from base64 import b64encode
from importlib.metadata import version

from flask import Flask
from flask_httpauth import HTTPBasicAuth


def worked_example():
    """Return measured outcomes; no generated credential leaves this function."""
    import os
    from unittest.mock import patch

    user = secrets.token_hex()
    value = "\N{LATIN SMALL LETTER E WITH ACUTE}\N{CJK UNIFIED IDEOGRAPH-5BC6}\N{LOCK}" + secrets.token_hex()

    def headers(presented):
        encoded = b64encode(f"{user}:{presented}".encode("utf-8")).decode("ascii")
        return {"Authorization": "Basic " + encoded}

    results = {
        "library": "Flask-HTTPAuth " + version("Flask-HTTPAuth"),
        "input": "random ASCII user; U+00E9 U+5BC6 U+1F512 + random ASCII password",
        "request": "GET protected route; Authorization: Basic Base64(UTF-8(user:password))",
        "old": {},
        "new": {},
    }
    for label, module_name, prefix, reader_name, blueprint_name, route in (
        ("application", "digitalmodel.web.digitaltwinfeed.todorestful.todorestful",
         "DIGITALTWINFEED", "basic_auth_password_for", "AppBlueprint", "/"),
        ("template", "digitalmodel.web.template_code.TemplateCode",
         "TEMPLATE_CODE", "_basic_auth_password_for", "AppBlueprint", "/"),
    ):
        module = importlib.import_module(module_name)
        if label == "application":
            readers = importlib.import_module("digitalmodel.web.digitaltwinfeed.basic_auth")
            reader = getattr(readers, reader_name)
        else:
            reader = getattr(module, reader_name)
        settings = {f"{prefix}_BASIC_AUTH_USER": user,
                    f"{prefix}_BASIC_AUTH_PASSWORD": value}
        with patch.dict(os.environ, settings):
            legacy = HTTPBasicAuth()
            legacy.get_password(reader)
            old_app = Flask(__name__)
            old_app.testing = True
            old_app.add_url_rule("/protected", view_func=legacy.login_required(lambda: "ok"))
            try:
                old_app.test_client().get("/protected", headers=headers(value))
            except TypeError as error:
                results["old"][label] = type(error).__name__ + ": " + str(error)
            else:
                results["old"][label] = "no exception"
            app = Flask(__name__)
            app.testing = True
            app.register_blueprint(getattr(module, blueprint_name), url_prefix="/protected")
            client = app.test_client()
            results["new"][label] = {
                "equal": client.get("/protected" + route, headers=headers(value)).status_code,
                "unequal": client.get("/protected" + route, headers=headers(value + "x")).status_code,
            }
    return results


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2))
