"""
ABOUTME: Base solvers module for digitalmodel mathematical solver framework
ABOUTME: Unified interface for all solver implementations across all domains
"""

from .base import (
    SolverStatus,
    BaseSolver,
    ConfigurableSolver,
    AnalysisSolver,
)

from .interfaces import (
    SolverProtocol,
    ConfigurableSolverProtocol,
    AnalysisSolverProtocol,
)

__version__ = "1.0.0"

__all__ = [
    # Status enumeration
    "SolverStatus",
    # Base classes
    "BaseSolver",
    "ConfigurableSolver",
    "AnalysisSolver",
    # Protocol interfaces
    "SolverProtocol",
    "ConfigurableSolverProtocol",
    "AnalysisSolverProtocol",
]
