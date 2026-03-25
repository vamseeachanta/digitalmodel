"""Backward-compat shim. Use digitalmodel.infrastructure.utils.front_end_components instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.front_end_components is deprecated. "
    "Use digitalmodel.infrastructure.utils.front_end_components instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.front_end_components import *  # noqa: F401,F403
