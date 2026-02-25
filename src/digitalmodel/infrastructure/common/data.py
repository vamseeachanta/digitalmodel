"""Backward-compat shim. Use digitalmodel.infrastructure.utils.data instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.data is deprecated. "
    "Use digitalmodel.infrastructure.utils.data instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.data import *  # noqa: F401,F403
