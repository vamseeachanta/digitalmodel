"""
ABOUTME: Standardized reporting framework for digitalmodel
ABOUTME: Provides consistent report structure across all analysis modules
"""

from .models import (
    StandardReport,
    ParameterSet,
    AnalysisResult,
    ValidationResult,
    ReportMetadata,
    ParametricStudy,
)

from .exporters import (
    export_to_html,
    export_to_json,
    export_to_csv,
    export_all_formats,
    export_parametric_study_html,
)

from .report_generator import PlotlyReportGenerator

from .path_utils import (
    get_project_root,
    get_data_path,
    get_report_path,
    ensure_report_dir,
    relative_path_from_report,
)

__all__ = [
    # Core models
    'StandardReport',
    'ParameterSet',
    'AnalysisResult',
    'ValidationResult',
    'ReportMetadata',
    'ParametricStudy',

    # Exporters
    'export_to_html',
    'export_to_json',
    'export_to_csv',
    'export_all_formats',
    'export_parametric_study_html',

    # Plotly report generator (merged from modules/reporting)
    'PlotlyReportGenerator',

    # Path utilities (merged from modules/reporting)
    'get_project_root',
    'get_data_path',
    'get_report_path',
    'ensure_report_dir',
    'relative_path_from_report',
]

__version__ = '1.0.0'
