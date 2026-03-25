"""Compatibility shim -- pyintegrity has been renamed to asset_integrity.

This module redirects imports to the new location.
Import from digitalmodel.asset_integrity instead.
"""
import warnings

warnings.warn(
    "digitalmodel.data_systems.pyintegrity has been renamed to "
    "digitalmodel.asset_integrity. Update your imports.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.asset_integrity import *  # noqa: F401,F403,E402
