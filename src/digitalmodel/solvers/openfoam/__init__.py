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
from .runner import (
    OpenFOAMRunConfig,
    OpenFOAMRunner,
    OpenFOAMRunResult,
    OpenFOAMRunStatus,
    StageResult,
)
from .spectral_analysis import (
    SloshingFrequencyResult,
    SpectralPeak,
    SpectrumResult,
    compute_fft_spectrum,
    compute_welch_spectrum,
    extract_natural_frequency,
    prismatic_tank_natural_frequency,
    sloshing_natural_frequency,
)

__all__ = [
    "BoundaryCondition",
    "BoundaryType",
    "CaseType",
    "DomainConfig",
    "DomainBuilder",
    "OpenFOAMCase",
    "OpenFOAMCaseBuilder",
    "OpenFOAMRunConfig",
    "OpenFOAMRunner",
    "OpenFOAMRunResult",
    "OpenFOAMRunStatus",
    "StageResult",
    "SolverConfig",
    "TurbulenceModel",
    "TurbulenceType",
    "SloshingFrequencyResult",
    "SpectralPeak",
    "SpectrumResult",
    "compute_fft_spectrum",
    "compute_welch_spectrum",
    "extract_natural_frequency",
    "prismatic_tank_natural_frequency",
    "sloshing_natural_frequency",
]
