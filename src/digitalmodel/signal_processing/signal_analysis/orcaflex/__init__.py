"""
OrcaFlex Time Series Analysis Module

This module provides specialized functionality for analyzing time series data
from OrcaFlex simulations, including tension data, mooring loads, and structural responses.

Main Components:
- GenericTimeSeriesReader: Flexible file reader with pattern matching and auto-detection
- TimeSeriesAnalyzer: Main analysis orchestrator for rainflow and FFT processing
- BatchProcessor: Parallel processing for multiple files
- ConfigurationManager: YAML-based configuration handling
"""

from .reader import GenericTimeSeriesReader
from .analyzer import TimeSeriesAnalyzer
from .batch import BatchProcessor
from .config import ConfigurationManager

__all__ = [
    'GenericTimeSeriesReader',
    'TimeSeriesAnalyzer',
    'BatchProcessor',
    'ConfigurationManager'
]

__version__ = '1.0.0'