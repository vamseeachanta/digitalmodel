"""Backward-compat shim. Use digitalmodel.infrastructure.utils.visualization.visualizations instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.visualizations is deprecated. "
    "Use digitalmodel.infrastructure.utils.visualization.visualizations instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.visualization.visualizations import *  # noqa: F401,F403
