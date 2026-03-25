"""Backward-compat shim. Use digitalmodel.web instead."""
import warnings

warnings.warn(
    "digitalmodel.infrastructure.services is deprecated. "
    "Use digitalmodel.web instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.web import *  # noqa: F401, F403
