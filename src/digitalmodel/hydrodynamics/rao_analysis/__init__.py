"""RAO Analysis Module

ABOUTME: Response Amplitude Operator analysis for hydrodynamic vessel data.

Provides RAO reading, plotting, and comparison utilities for
OrcaFlex displacement RAOs across multiple vessels and headings.
"""

from .rao_analysis import RAOAnalysis

__all__ = [
    "RAOAnalysis",
]
