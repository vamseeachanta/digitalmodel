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
    SpectralPeak,
    SpectrumResult,
    compute_fft_spectrum,
    compute_welch_spectrum,
    extract_natural_frequency,
    prismatic_tank_natural_frequency,
)
from .pressure_taps import (
    PressureTap,
    PressureTapStatistics,
    b1546_default_taps,
    compute_tap_statistics,
    point_tap_names,
    read_tap_statistics,
    render_patch_probes_entry,
    render_pressure_tap_functions,
    render_probes_entry,
    render_surface_entry,
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
    "SpectralPeak",
    "SpectrumResult",
    "compute_fft_spectrum",
    "compute_welch_spectrum",
    "extract_natural_frequency",
    "prismatic_tank_natural_frequency",
    "PressureTap",
    "PressureTapStatistics",
    "b1546_default_taps",
    "compute_tap_statistics",
    "point_tap_names",
    "read_tap_statistics",
    "render_patch_probes_entry",
    "render_pressure_tap_functions",
    "render_probes_entry",
    "render_surface_entry",
]
