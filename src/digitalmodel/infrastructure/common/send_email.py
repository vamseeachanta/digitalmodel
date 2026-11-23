"""Backward-compat shim. Use digitalmodel.infrastructure.utils.send_email instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.send_email is deprecated. "
    "Use digitalmodel.infrastructure.utils.send_email instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.send_email import *  # noqa: F401,F403
