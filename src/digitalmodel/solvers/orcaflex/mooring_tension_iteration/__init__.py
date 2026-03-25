"""
OrcaFlex Mooring Tension Iteration System

This module provides automated iteration of mooring line tensions to achieve
target values using scipy optimization and Newton-Raphson methods.

Key Components:
- IterationConfig: Configuration management for target tensions and parameters
- MooringTensionIterator: Main orchestrator for the optimization process
- TensionAnalyzer: Interface with OrcaFlex for tension extraction
- LinePropertyManager: Automated adjustment of mooring line properties
"""

from .config import IterationConfig
from .tension_iterator import MooringTensionIterator
from .tension_analyzer import TensionAnalyzer
from .line_manager import LinePropertyManager

__version__ = "1.0.0"
__all__ = [
    "IterationConfig",
    "MooringTensionIterator", 
    "TensionAnalyzer",
    "LinePropertyManager"
]