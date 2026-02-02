"""Analysis Module for Marine Engineering Data.

This module provides analytical tools for processing marine engineering
data from various sources including Excel spreadsheets and hydro databases.

Modules:
    excel_analyzer: Excel-based marine engineering data analysis
    hydro_usage_example: Example usage patterns for hydrodynamic analysis

Example:
    >>> from digitalmodel.marine_ops.marine_analysis.analysis import excel_analyzer
    >>> analyzer = excel_analyzer.MarineExcelAnalyzer('vessel_data.xlsx')
    >>> summary = analyzer.analyze_stability()
"""

from .excel_analyzer import *
from .hydro_usage_example import *

__all__ = [
    'excel_analyzer',
    'hydro_usage_example'
]
