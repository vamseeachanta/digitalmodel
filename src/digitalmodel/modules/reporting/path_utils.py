"""Backward compatibility shim for digitalmodel.modules.reporting.path_utils.

Use digitalmodel.reporting.path_utils instead.
"""
import warnings

warnings.warn(
    "digitalmodel.modules.reporting.path_utils is deprecated. "
    "Use digitalmodel.reporting.path_utils instead.",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.reporting.path_utils import (
    get_project_root,
    get_data_path,
    get_report_path,
    ensure_report_dir,
    relative_path_from_report,
)

__all__ = [
    "get_project_root",
    "get_data_path",
    "get_report_path",
    "ensure_report_dir",
    "relative_path_from_report",
]
