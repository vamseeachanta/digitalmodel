"""Backward-compat shim. Use digitalmodel.infrastructure.utils.MaterialProperties instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.MaterialProperties is deprecated. "
    "Use digitalmodel.infrastructure.utils.MaterialProperties instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.MaterialProperties import *  # noqa: F401,F403
