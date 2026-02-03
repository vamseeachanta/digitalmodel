"""
OrcaFlex Core Framework Module

This module provides the foundational components for the refactored OrcaFlex analysis system.
It includes interfaces, protocols, base classes, and the component registry system.
"""

from .interfaces import (
    AnalyzerInterface,
    ProcessorInterface,
    ExtractorInterface,
    ConfigurationInterface,
    ModelInterface
)

from .base_classes import (
    BaseAnalyzer,
    BaseProcessor,
    BaseExtractor,
    BaseWorkflow
)

from .registry import ComponentRegistry, register_component, get_component

from .exceptions import (
    OrcaFlexError,
    ConfigurationError,
    AnalysisError,
    ValidationError,
    LicenseError
)

__all__ = [
    # Interfaces
    'AnalyzerInterface',
    'ProcessorInterface',
    'ExtractorInterface',
    'ConfigurationInterface',
    'ModelInterface',
    
    # Base Classes
    'BaseAnalyzer',
    'BaseProcessor',
    'BaseExtractor',
    'BaseWorkflow',
    
    # Registry
    'ComponentRegistry',
    'register_component',
    'get_component',
    
    # Exceptions
    'OrcaFlexError',
    'ConfigurationError',
    'AnalysisError',
    'ValidationError',
    'LicenseError'
]

# Module version
__version__ = '2.0.0'