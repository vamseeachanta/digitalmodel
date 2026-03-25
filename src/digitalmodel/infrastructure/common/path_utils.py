"""Backward-compat shim. Use digitalmodel.infrastructure.utils.path_utils instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.path_utils is deprecated. "
    "Use digitalmodel.infrastructure.utils.path_utils instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.path_utils import *  # noqa: F401,F403
