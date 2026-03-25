"""Profiling Module for Marine Engineering Analysis.

This module provides comprehensive performance profiling, benchmarking, and
optimization analysis tools for marine engineering computations.

Modules:
    profile_modules: Core profiling functionality for marine modules
    performance_charts: Visualization of performance metrics
    optimization_report: Detailed optimization recommendations
    run_analysis: Convenience wrapper for running performance analysis

Example:
    >>> from digitalmodel.marine_ops.marine_analysis.profiling import profile_modules
    >>> results = profile_modules.profile_module('wave_spectra')
    >>> print(results.summary())
"""

from .profile_modules import *
from .performance_charts import *
from .optimization_report import *
from .run_analysis import *

__all__ = [
    'profile_modules',
    'performance_charts',
    'optimization_report',
    'run_analysis'
]
