"""
Unified catenary analysis module for marine engineering.

Combines advanced BVP solver (Phase 1) with specialized features
from legacy catenary modules (lazy-wave, plotting, simplified methods).

Modern API (recommended):
    from digitalmodel.modules.marine_engineering.catenary import CatenarySolver, CatenaryInput
    solver = CatenarySolver()
    result = solver.solve(params)

Lazy-Wave API:
    from digitalmodel.modules.marine_engineering.catenary import LazyWaveSolver, LazyWaveConfiguration
    solver = LazyWaveSolver()
    result = solver.solve(config)

Legacy API (deprecated, for backward compatibility):
    from digitalmodel.modules.marine_engineering.catenary import catenaryEquation
    result = catenaryEquation({"F": 10000, "w": 500, "d": 100, ...})
"""

try:
    from .solver import CatenarySolver, CatenaryInput, CatenaryResults
except ImportError:
    # Fallback to mooring_analysis module
    from digitalmodel.modules.marine_engineering.mooring_analysis.catenary_solver import (
        CatenarySolver,
        CatenaryInput,
        CatenaryResults
    )

# Import lazy-wave solver
from .lazy_wave import (
    LazyWaveSolver,
    LazyWaveConfiguration,
    LazyWaveResults,
    LazyWaveSegment
)

# Import legacy compatibility adapter
from .adapter import (
    catenaryEquation,
    catenaryForces,
    validate_catenary_data,
    ADAPTER_VERSION
)

__all__ = [
    # Modern API
    'CatenarySolver',
    'CatenaryInput',
    'CatenaryResults',
    # Lazy-Wave API
    'LazyWaveSolver',
    'LazyWaveConfiguration',
    'LazyWaveResults',
    'LazyWaveSegment',
    # Legacy API (deprecated)
    'catenaryEquation',
    'catenaryForces',
    'validate_catenary_data',
    # Metadata
    'ADAPTER_VERSION',
]

__version__ = '2.0.0'  # Unified module version
