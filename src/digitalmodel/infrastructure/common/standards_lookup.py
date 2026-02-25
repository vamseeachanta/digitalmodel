"""Backward-compat shim. Use digitalmodel.infrastructure.utils.standards_lookup instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.standards_lookup is deprecated. "
    "Use digitalmodel.infrastructure.utils.standards_lookup instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.standards_lookup import *  # noqa: F401,F403
