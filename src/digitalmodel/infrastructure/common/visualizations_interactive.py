"""Backward-compat shim. Use digitalmodel.infrastructure.utils.visualization.visualizations_interactive instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.visualizations_interactive is deprecated. "
    "Use digitalmodel.infrastructure.utils.visualization.visualizations_interactive instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.visualization.visualizations_interactive import *  # noqa: F401,F403
