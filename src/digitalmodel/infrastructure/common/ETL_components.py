"""Backward-compat shim. Use digitalmodel.infrastructure.utils.ETL_components instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.ETL_components is deprecated. "
    "Use digitalmodel.infrastructure.utils.ETL_components instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.ETL_components import *  # noqa: F401,F403
