"""
OrcaFlex Analysis Module

This module provides analysis tools for OrcaFlex mooring systems including:
- Comparative analysis across configurations
- Statistical analysis of mooring properties
- Report generation and visualization
"""

from .comparative import MooringComparativeAnalysis
from .report_generator import ReportGenerator
from .cli import main as run_comparative_analysis

__all__ = [
    'MooringComparativeAnalysis',
    'ReportGenerator',
    'run_comparative_analysis'
]

__version__ = '1.0.0'