# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Environment credentials for HTTP basic authentication.

DIGITALTWINFEED_BASIC_AUTH_USER and DIGITALTWINFEED_BASIC_AUTH_PASSWORD have
no defaults. Missing or blank variables close authentication for every request.
"""

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
    return value if username == configured else None


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
