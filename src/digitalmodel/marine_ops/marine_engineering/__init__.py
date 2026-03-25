"""
Marine Engineering Module.

ABOUTME: Comprehensive marine engineering analysis tools including catenary solvers,
environmental loading calculations, hydrodynamic coefficients, mooring analysis,
and wave spectra modeling.

Submodules:
    environmental_loading: OCIMF wind and current coefficient calculations
    hydrodynamic_coefficients: Added mass, damping matrices, and AQWA parsing
    mooring_analysis: Quasi-static mooring system analysis

Note: Catenary analysis has been consolidated into
    digitalmodel.marine_ops.marine_analysis.catenary
"""

# Catenary analysis (re-exported from marine_analysis.catenary)
from digitalmodel.marine_ops.marine_analysis.catenary import (
    CatenarySolver,
    CatenaryInput,
    CatenaryResults,
    LazyWaveSolver,
    LazyWaveConfiguration,
    LazyWaveResults,
)

# Environmental loading (OCIMF)
from .environmental_loading import (
    OCIMFDatabase,
    EnvironmentalForces,
    EnvironmentalConditions,
    VesselGeometry,
    EnvironmentalForceResults,
)

# Hydrodynamic coefficients
from .hydrodynamic_coefficients import (
    CoefficientDatabase,
    FrequencyDependentMatrix,
    AQWAParser,
    HydrodynamicPlotter,
    DOF_NAMES,
    DOF_INDEX,
)

# Wave spectra - canonical location: digitalmodel.hydrodynamics.wave_spectra

__version__ = "1.0.0"
__author__ = "DigitalModel Team"

__all__ = [
    # Catenary
    "CatenarySolver",
    "CatenaryInput",
    "CatenaryResults",
    "LazyWaveSolver",
    "LazyWaveConfiguration",
    "LazyWaveResults",
    # Environmental loading
    "OCIMFDatabase",
    "EnvironmentalForces",
    "EnvironmentalConditions",
    "VesselGeometry",
    "EnvironmentalForceResults",
    # Hydrodynamic coefficients
    "CoefficientDatabase",
    "FrequencyDependentMatrix",
    "AQWAParser",
    "HydrodynamicPlotter",
    "DOF_NAMES",
    "DOF_INDEX",
    # Wave spectra - moved to digitalmodel.hydrodynamics.wave_spectra
]
