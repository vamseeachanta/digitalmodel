"""Backward compatibility shim for digitalmodel.modules.reporting.

Use digitalmodel.reporting instead.
"""
import warnings

warnings.warn(
    "digitalmodel.modules.reporting is deprecated. Use digitalmodel.reporting instead.",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.reporting.report_generator import PlotlyReportGenerator
from digitalmodel.reporting.path_utils import (
    get_project_root,
    get_data_path,
    get_report_path,
    ensure_report_dir,
    relative_path_from_report,
)

__all__ = [
    "PlotlyReportGenerator",
    "get_project_root",
    "get_data_path",
    "get_report_path",
    "ensure_report_dir",
    "relative_path_from_report",
]
