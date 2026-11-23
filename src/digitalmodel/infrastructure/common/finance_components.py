"""Backward-compat shim. Use digitalmodel.infrastructure.utils.finance_components instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.finance_components is deprecated. "
    "Use digitalmodel.infrastructure.utils.finance_components instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.finance_components import *  # noqa: F401,F403
