"""Backward-compat shim. Use digitalmodel.infrastructure.utils.ymlInput instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.ymlInput is deprecated. "
    "Use digitalmodel.infrastructure.utils.ymlInput instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.ymlInput import *  # noqa: F401,F403
