"""
Signal Analysis Module

A comprehensive module for signal processing, including rainflow counting
and FFT/spectral analysis. This module consolidates various signal processing
implementations across the codebase into a unified, well-tested package.

Main components:
- core: Core signal processing algorithms (rainflow, FFT, time series)
- fatigue: Fatigue analysis specific implementations
- filters: Frequency and time domain filtering
- visualization: Plotting and visualization utilities
- utils: Helper functions and utilities
"""

__version__ = "1.0.0"

# Core imports
from .core.rainflow import RainflowCounter
from .core.spectral import SpectralAnalyzer
from .core.timeseries import TimeSeriesProcessor

__all__ = [
    "RainflowCounter",
    "SpectralAnalyzer", 
    "TimeSeriesProcessor",
]