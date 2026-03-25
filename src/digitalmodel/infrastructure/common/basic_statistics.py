"""Backward-compat shim. Use digitalmodel.infrastructure.utils.basic_statistics instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.basic_statistics is deprecated. "
    "Use digitalmodel.infrastructure.utils.basic_statistics instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.basic_statistics import *  # noqa: F401,F403
