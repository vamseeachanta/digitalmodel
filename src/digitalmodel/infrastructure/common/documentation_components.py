"""Backward-compat shim. Use digitalmodel.infrastructure.utils.documentation_components instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.documentation_components is deprecated. "
    "Use digitalmodel.infrastructure.utils.documentation_components instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.documentation_components import *  # noqa: F401,F403
