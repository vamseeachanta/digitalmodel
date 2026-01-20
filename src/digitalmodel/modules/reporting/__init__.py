"""
Reporting Module

Interactive HTML report generation with Plotly visualizations.
"""

from .report_generator import PlotlyReportGenerator
from .path_utils import (
    get_project_root,
    get_data_path,
    get_report_path,
    ensure_report_dir,
    relative_path_from_report,
)

__all__ = [
    'PlotlyReportGenerator',
    'get_project_root',
    'get_data_path',
    'get_report_path',
    'ensure_report_dir',
    'relative_path_from_report',
]
