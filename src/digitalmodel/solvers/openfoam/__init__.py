"""
ABOUTME: OpenFOAM CFD analysis capability for marine/offshore engineering.
Provides case setup, mesh generation, solver configuration, post-processing,
and parametric study tooling.
"""

from .models import (
    BoundaryCondition,
    BoundaryType,
    CaseType,
    DomainConfig,
    OpenFOAMCase,
    SolverConfig,
    TurbulenceModel,
    TurbulenceType,
)
from .case_builder import OpenFOAMCaseBuilder
from .domain_builder import DomainBuilder

__all__ = [
    "BoundaryCondition",
    "BoundaryType",
    "CaseType",
    "DomainConfig",
    "DomainBuilder",
    "OpenFOAMCase",
    "OpenFOAMCaseBuilder",
    "SolverConfig",
    "TurbulenceModel",
    "TurbulenceType",
]
