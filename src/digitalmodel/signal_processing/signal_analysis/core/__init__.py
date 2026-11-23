"""
Core signal analysis algorithms

This module contains the fundamental signal processing algorithms:
- Rainflow counting for fatigue analysis
- FFT and spectral analysis
- Time series processing utilities
"""

from .rainflow import RainflowCounter
from .spectral import SpectralAnalyzer
from .timeseries import TimeSeriesProcessor

__all__ = ["RainflowCounter", "SpectralAnalyzer", "TimeSeriesProcessor"]