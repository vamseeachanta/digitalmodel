"""Backward-compat shim. Use digitalmodel.infrastructure.utils.database instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.database is deprecated. "
    "Use digitalmodel.infrastructure.utils.database instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.database import *  # noqa: F401,F403
