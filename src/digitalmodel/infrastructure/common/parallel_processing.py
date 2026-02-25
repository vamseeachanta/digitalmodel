"""Backward-compat shim. Use digitalmodel.infrastructure.utils.parallel_processing instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.parallel_processing is deprecated. "
    "Use digitalmodel.infrastructure.utils.parallel_processing instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.parallel_processing import *  # noqa: F401,F403
