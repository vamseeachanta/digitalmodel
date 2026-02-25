"""Backward-compat shim. Use digitalmodel.infrastructure.utils.excel_utilities instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.excel_utilities is deprecated. "
    "Use digitalmodel.infrastructure.utils.excel_utilities instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.excel_utilities import *  # noqa: F401,F403
