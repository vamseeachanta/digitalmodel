"""Backward-compat shim. Use digitalmodel.infrastructure.utils.visualization.visualization_components instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.visualization_components is deprecated. "
    "Use digitalmodel.infrastructure.utils.visualization.visualization_components instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.visualization.visualization_components import *  # noqa: F401,F403
