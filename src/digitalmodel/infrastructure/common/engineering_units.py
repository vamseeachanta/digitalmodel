"""Backward-compat shim. Use digitalmodel.infrastructure.utils.engineering_units instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.engineering_units is deprecated. "
    "Use digitalmodel.infrastructure.utils.engineering_units instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.engineering_units import *  # noqa: F401,F403
