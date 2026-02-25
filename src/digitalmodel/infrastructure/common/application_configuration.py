"""Backward-compat shim. Use digitalmodel.infrastructure.utils.application_configuration instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.application_configuration is deprecated. "
    "Use digitalmodel.infrastructure.utils.application_configuration instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.application_configuration import *  # noqa: F401,F403
