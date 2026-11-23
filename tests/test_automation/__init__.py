"""
Test Suite Automation and Maintenance System

This package provides continuous monitoring, execution, and maintenance 
of the repository's comprehensive test suite.
"""

__version__ = "0.1.0"

# Core components
from test_automation.core.discovery import TestDiscoveryEngine
from test_automation.core.runner import ModuleTestRunner
from test_automation.core.analysis import FailureAnalyzer
from test_automation.core.autofix import AutoFixEngine

# Reporting
from test_automation.reporting.generator import TestReportGenerator

# CLI interface
from test_automation.cli.main import main

__all__ = [
    "TestDiscoveryEngine",
    "ModuleTestRunner", 
    "FailureAnalyzer",
    "AutoFixEngine",
    "TestReportGenerator",
    "main"
]