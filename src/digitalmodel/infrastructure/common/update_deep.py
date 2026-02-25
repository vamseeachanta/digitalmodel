"""Backward-compat shim. Use digitalmodel.infrastructure.utils.update_deep instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.update_deep is deprecated. "
    "Use digitalmodel.infrastructure.utils.update_deep instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.update_deep import *  # noqa: F401,F403
