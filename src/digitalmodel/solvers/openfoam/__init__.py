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
from .motion import (
    MotionType,
    PrescribedMotion,
    render_dynamic_mesh_dict,
    render_dynamic_mesh_dict_body,
    write_dynamic_mesh_dict,
)
from .runner import (
    OpenFOAMRunConfig,
    OpenFOAMRunner,
    OpenFOAMRunResult,
    OpenFOAMRunStatus,
    StageResult,
)
from .spectral_analysis import (
    SpectralPeak,
    SpectrumResult,
    compute_fft_spectrum,
    compute_welch_spectrum,
    extract_natural_frequency,
    prismatic_tank_natural_frequency,
)
from .sloshing_coupling import (
    CouplingStrengthReport,
    FillDampingResult,
    MomentCoefficients,
    SloshingCase,
    SloshingCouplingModel,
    TuningReport,
)

__all__ = [
    "BoundaryCondition",
    "BoundaryType",
    "CaseType",
    "DomainConfig",
    "DomainBuilder",
    "MotionType",
    "OpenFOAMCase",
    "OpenFOAMCaseBuilder",
    "PrescribedMotion",
    "render_dynamic_mesh_dict",
    "render_dynamic_mesh_dict_body",
    "write_dynamic_mesh_dict",
    "OpenFOAMRunConfig",
    "OpenFOAMRunner",
    "OpenFOAMRunResult",
    "OpenFOAMRunStatus",
    "StageResult",
    "SolverConfig",
    "TurbulenceModel",
    "TurbulenceType",
    "SpectralPeak",
    "SpectrumResult",
    "compute_fft_spectrum",
    "compute_welch_spectrum",
    "extract_natural_frequency",
    "prismatic_tank_natural_frequency",
    "CouplingStrengthReport",
    "FillDampingResult",
    "MomentCoefficients",
    "SloshingCase",
    "SloshingCouplingModel",
    "TuningReport",
]
