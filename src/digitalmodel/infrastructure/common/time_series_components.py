"""Backward-compat shim. Use digitalmodel.infrastructure.utils.time_series_components instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.time_series_components is deprecated. "
    "Use digitalmodel.infrastructure.utils.time_series_components instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.time_series_components import *  # noqa: F401,F403
