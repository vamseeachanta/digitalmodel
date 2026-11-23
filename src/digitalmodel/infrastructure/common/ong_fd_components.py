"""Backward-compat shim. Use digitalmodel.infrastructure.utils.ong_fd_components instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.ong_fd_components is deprecated. "
    "Use digitalmodel.infrastructure.utils.ong_fd_components instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.ong_fd_components import *  # noqa: F401,F403
