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
]

__version__ = '1.0.0'
