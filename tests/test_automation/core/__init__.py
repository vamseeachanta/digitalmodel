"""
Core test automation components including discovery, execution, 
analysis, and auto-fix functionality.
"""

from .discovery import TestDiscoveryEngine
from .runner import ModuleTestRunner
from .analysis import FailureAnalyzer
from .autofix import AutoFixEngine

__all__ = [
    "TestDiscoveryEngine",
    "ModuleTestRunner",
    "FailureAnalyzer", 
    "AutoFixEngine"
]