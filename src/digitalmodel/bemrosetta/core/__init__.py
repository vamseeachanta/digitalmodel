"""
BEMRosetta Core Module

Contains fundamental components for BEMRosetta integration:
- exceptions: Custom exception classes for error handling
- interfaces: Abstract base classes defining component contracts
- runner: BEMRosetta executable detection and management
"""

from .exceptions import (
    BEMRosettaError,
    ParserError,
    ConverterError,
    ValidationError,
    MeshError,
    ExecutableNotFoundError,
)

from .interfaces import (
    ParserInterface,
    ConverterInterface,
    MeshHandlerInterface,
    ValidatorInterface,
    ValidationReport,
)

from .runner import (
    is_bemrosetta_available,
    get_runner,
    BEMRosettaRunner,
)

__all__ = [
    # Exceptions
    "BEMRosettaError",
    "ParserError",
    "ConverterError",
    "ValidationError",
    "MeshError",
    "ExecutableNotFoundError",

    # Interfaces
    "ParserInterface",
    "ConverterInterface",
    "MeshHandlerInterface",
    "ValidatorInterface",
    "ValidationReport",

    # Runner
    "is_bemrosetta_available",
    "get_runner",
    "BEMRosettaRunner",
]
