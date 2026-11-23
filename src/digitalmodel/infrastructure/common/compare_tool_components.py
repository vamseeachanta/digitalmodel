"""Backward-compat shim. Use digitalmodel.infrastructure.utils.compare_tool_components instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.compare_tool_components is deprecated. "
    "Use digitalmodel.infrastructure.utils.compare_tool_components instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.compare_tool_components import *  # noqa: F401,F403
