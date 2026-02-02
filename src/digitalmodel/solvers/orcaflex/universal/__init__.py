"""
Universal OrcaFlex Simulation Runner
====================================

A comprehensive, library-based OrcaFlex simulation runner that can be executed 
from any directory on any computer with flexible keyword arguments.

Main Classes:
    - UniversalOrcaFlexRunner: Main runner with flexible configuration
    - PathResolver: Resolve paths for universal access
    - ModelDiscovery: Discover and filter OrcaFlex models
    - BatchProcessor: Manage batch processing with optimization
    - StatusReporter: Real-time status reporting

Usage:
    from digitalmodel.orcaflex.universal import UniversalOrcaFlexRunner
    
    runner = UniversalOrcaFlexRunner()
    results = runner.run(pattern="*.yml", input_directory="./models")
"""

from .universal_runner import UniversalOrcaFlexRunner
from .path_resolver import PathResolver
from .model_discovery import ModelDiscovery
from .batch_processor import BatchProcessor
from .status_reporter import StatusReporter

__all__ = [
    'UniversalOrcaFlexRunner',
    'PathResolver',
    'ModelDiscovery',
    'BatchProcessor',
    'StatusReporter',
]

__version__ = '1.0.0'