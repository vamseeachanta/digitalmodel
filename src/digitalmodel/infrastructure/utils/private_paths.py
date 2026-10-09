"""Locations of private data, resolved at run time and never hard-coded.

This repository is public. Paths into a client share or a user profile were
removed from it (owner decision C11) and replaced in documentation by the
neutral text ``<private-data>``. Code must not open that text: a program that
needs a private file takes its location from the caller -- an argument or a
configuration value -- or from an environment variable, and stops with a clear
message when it has neither.

Environment variables:

``DIGITALMODEL_ACCESS_DB``
    The Microsoft Access database (``.accdb``/``.mdb``) read by the Access
    branch of ``Database`` when the configuration names none.
``DIGITALMODEL_PRIVATE_DATA``
    The folder that holds private inputs and outputs for the legacy scripts
    and ``__main__`` examples (spreadsheets, timelines, drawings).
"""

from __future__ import annotations

import os
from pathlib import Path

#: The redaction text. A value containing it is not a location.
PLACEHOLDER = "<private-data>"

ACCESS_DB_ENV = "DIGITALMODEL_ACCESS_DB"
PRIVATE_DATA_ENV = "DIGITALMODEL_PRIVATE_DATA"

ACCESS_DRIVER = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"


class PrivatePathNotConfigured(RuntimeError):
    """A private location was needed and neither the caller nor the
    environment supplied one."""


def _usable(value: object) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    if PLACEHOLDER in text:
        raise PrivatePathNotConfigured(
            f"{text!r} is the redaction placeholder, not a location. Supply the "
            f"real path through the configuration or the environment."
        )
    return text


def resolve_private_path(
    configured: object = None, env: str = PRIVATE_DATA_ENV, what: str = "a path"
) -> Path:
    """The caller's value if given, else the environment variable ``env``."""
    value = _usable(configured)
    if value is None:
        value = _usable(os.environ.get(env))
    if value is None:
        raise PrivatePathNotConfigured(
            f"{what} is not configured: pass it in the configuration or set "
            f"the environment variable {env}."
        )
    return Path(value)


def private_data_path(*parts: str, configured: object = None) -> Path:
    """``parts`` under the private data folder (``DIGITALMODEL_PRIVATE_DATA``,
    or ``configured`` when the caller has one)."""
    root = resolve_private_path(
        configured, PRIVATE_DATA_ENV, what="the private data folder"
    )
    return root.joinpath(*parts)


def access_connection_string(database: object = None) -> str:
    """ODBC connection string for an Access database.

    ``database`` is the caller's configured file; without it
    ``DIGITALMODEL_ACCESS_DB`` is used. Neither set is an error.
    """
    path = resolve_private_path(
        database, ACCESS_DB_ENV, what="the Access database file"
    )
    return f"{ACCESS_DRIVER}DBQ={path};"
