# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Shared UTF-8 Basic authentication for environment-backed web examples."""

import hmac
import os
from base64 import b64decode
from binascii import Error as Base64Error

from flask import request
from flask_httpauth import HTTPBasicAuth


class UTF8BasicAuth(HTTPBasicAuth):
    """Reject malformed encodings before Flask-HTTPAuth's Latin-1 fallback."""

    def get_auth(self):
        header = request.headers.get(self.header or "Authorization")
        if header is not None:
            try:
                # RFC 4648 (2006), section 3.3: reject non-alphabet data.
                # RFC 7617 (2015), section 2: user-pass has one separator;
                # section 2.1 describes UTF-8 credentials.
                _, encoded = header.split(" ", 1)
                decoded = b64decode(encoded, validate=True).decode("utf-8")
                if ":" not in decoded:
                    return None
            except (ValueError, Base64Error):
                return None
        return super().get_auth()


def verify_environment_credentials(username, password, *, prefix):
    """Return a boolean using the current, nonblank environment settings.

    Compare both fields as bytes before combining the results. Input lengths
    can affect timing; this does not promise constant time for whole requests.
    Source: Python 3.11 Library Reference, hmac.compare_digest:
    https://docs.python.org/3.11/library/hmac.html#hmac.compare_digest
    """
    configured_user = os.environ.get(f"{prefix}_BASIC_AUTH_USER")
    configured_value = os.environ.get(f"{prefix}_BASIC_AUTH_PASSWORD")
    if (configured_user is None or configured_value is None
            or not configured_user.strip() or not configured_value.strip()
            or not username or not password):
        return False
    user_matches = hmac.compare_digest(
        username.encode("utf-8"), configured_user.encode("utf-8")
    )
    value_matches = hmac.compare_digest(
        password.encode("utf-8"), configured_value.encode("utf-8")
    )
    return user_matches and value_matches
