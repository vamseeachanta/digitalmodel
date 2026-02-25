"""Backward-compat shim. Use digitalmodel.infrastructure.utils.data_models_components instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.data_models_components is deprecated. "
    "Use digitalmodel.infrastructure.utils.data_models_components instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.data_models_components import *  # noqa: F401,F403
