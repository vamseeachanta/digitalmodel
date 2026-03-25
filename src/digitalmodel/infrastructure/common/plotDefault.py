"""Backward-compat shim. Use digitalmodel.infrastructure.utils.visualization.plotDefault instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.plotDefault is deprecated. "
    "Use digitalmodel.infrastructure.utils.visualization.plotDefault instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.visualization.plotDefault import *  # noqa: F401,F403
