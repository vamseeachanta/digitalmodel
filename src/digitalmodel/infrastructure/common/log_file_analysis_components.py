"""Backward-compat shim. Use digitalmodel.infrastructure.utils.log_file_analysis_components instead."""
import warnings
warnings.warn(
    "digitalmodel.infrastructure.common.log_file_analysis_components is deprecated. "
    "Use digitalmodel.infrastructure.utils.log_file_analysis_components instead.",
    DeprecationWarning,
    stacklevel=2,
)
from digitalmodel.infrastructure.utils.log_file_analysis_components import *  # noqa: F401,F403
