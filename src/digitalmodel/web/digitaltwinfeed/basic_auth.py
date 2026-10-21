# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Environment credentials for HTTP basic authentication.

DIGITALTWINFEED_BASIC_AUTH_USER and DIGITALTWINFEED_BASIC_AUTH_PASSWORD have
no defaults. Missing or blank variables close authentication for every request.
"""

import hmac
import logging
import os

_startup_checked = False


def basic_auth_user():
    """Return DIGITALTWINFEED_BASIC_AUTH_USER unchanged, or None if blank."""
    value = os.environ.get('DIGITALTWINFEED_BASIC_AUTH_USER')
    return value if value is not None and value.strip() else None


def basic_auth_password():
    """Return DIGITALTWINFEED_BASIC_AUTH_PASSWORD unchanged, or None if blank."""
    value = os.environ.get('DIGITALTWINFEED_BASIC_AUTH_PASSWORD')
    return value if value is not None and value.strip() else None


def basic_auth_password_for(username):
    """Return the configured password only for the configured nonblank user."""
    configured = basic_auth_user()
    value = basic_auth_password()
    if configured is None or value is None or not username:
        return None
    matches = hmac.compare_digest(username.encode('utf-8'), configured.encode('utf-8'))
    return value if matches else None


def basic_auth_verify_password(username, password):
    """Verify both credentials against the live, nonblank environment values."""
    configured = basic_auth_user()
    value = basic_auth_password()
    if configured is None or value is None or not username or not password:
        return False
    # Python 3.11 Library Reference, hmac.compare_digest: non-ASCII text must
    # be encoded as bytes. Compare both fields before combining the results.
    # https://docs.python.org/3.11/library/hmac.html#hmac.compare_digest
    user_matches = hmac.compare_digest(
        username.encode('utf-8'), configured.encode('utf-8')
    )
    value_matches = hmac.compare_digest(
        password.encode('utf-8'), value.encode('utf-8')
    )
    return user_matches and value_matches


def warn_if_basic_auth_closed():
    """Warn once at start-up, naming missing variables without their values."""
    global _startup_checked
    if _startup_checked:
        return
    _startup_checked = True
    missing = []
    if basic_auth_user() is None:
        missing.append('DIGITALTWINFEED_BASIC_AUTH_USER')
    if basic_auth_password() is None:
        missing.append('DIGITALTWINFEED_BASIC_AUTH_PASSWORD')
    if missing:
        logging.getLogger(__name__).warning(
            '%s absent or blank; HTTP basic authentication is closed and every '
            'request to a protected route is refused until the variables are set; '
            'the repository holds no value for them.', ', '.join(missing)
        )
